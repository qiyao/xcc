//
// Instruction level code motion
//


// Copyright (c) 2003-2007 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

/*
   Code Motion moves common part of two BBs into a new block which is the
   new successor of these two BBs (downward motion).  In order to perform
   this motion,  these two BBs must have the same set of successors.
 */

#include "defs.h"
#include <alloca.h>
#include "config.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "cg_flags.h"
#include "bb.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gra_live.h"
#include "cgexp.h"
#include "cg_dep_graph.h"
#include "cmotion.h"

#include <list>
#include <utility>

using std::list;
using std::pair;


struct cm_backpatch {
  BB *bb;
  TN *src_tn;
  TN *dst_tn;
  struct cm_backpatch *next;
};

static struct cm_backpatch *CM_backpatches;
static struct cm_backpatch *CM_committed_backpatches;
static struct cm_backpatch *CM_uncommitted_additions;
static struct cm_backpatch *CM_uncommitted_removals;
static list< pair<BB*,BB*> > CM_work_list;
static BB_MAP CM_bb_info_map;

static TN* CM_Find_Backpatch(BB *bb, TN* src_tn, TN *dst_tn)
{
  struct cm_backpatch *ptr = CM_backpatches;
  struct cm_backpatch *result = NULL;
  while (ptr != NULL) {
    if ((ptr->bb == bb) && 
	((src_tn == NULL) || (ptr->src_tn == src_tn)) && 
	((dst_tn == NULL) || (ptr->dst_tn == dst_tn))) {
      /* Could be two backpatches with the same src/dst. Return NULL in this 
	 case */
      if (result)
	return NULL;
      else
	result = ptr;
    }
    ptr = ptr->next;
  }
  if (result)
    return result->dst_tn;
  else
    return src_tn;
}

static void CM_Remove_Backpatch(BB *bb, TN* src_tn, TN *dst_tn)
{
  struct cm_backpatch *ptr = CM_backpatches;
  struct cm_backpatch *prev_ptr = NULL;
  while (ptr != NULL) {
    if ((ptr->bb == bb) && 
	((src_tn == NULL) || (ptr->src_tn == src_tn)) && 
	((dst_tn == NULL) || (ptr->dst_tn == dst_tn))) {
      if (prev_ptr)
	prev_ptr->next = ptr->next;
      else
	CM_backpatches = ptr->next;
      if (CM_committed_backpatches == ptr)
	CM_committed_backpatches = ptr->next;
      ptr->next = CM_uncommitted_removals;
      CM_uncommitted_removals = ptr;
    } else
      prev_ptr = ptr;
    ptr = ptr->next;
  }
}

static void CM_Add_Backpatch(BB *bb, TN* src_tn, TN *dst_tn)
{
  if (CM_Find_Backpatch(bb, src_tn, dst_tn) != dst_tn) {
    struct cm_backpatch *new_bp;
    new_bp = TYPE_MEM_POOL_ALLOC(struct cm_backpatch, &MEM_local_pool);
    new_bp->bb = bb;
    new_bp->src_tn = src_tn;
    new_bp->dst_tn = dst_tn;
    new_bp->next = CM_backpatches;
    CM_backpatches = new_bp;
  }
}

static void CM_Commit_Backpatches()
{
  CM_committed_backpatches = CM_backpatches;
#if 0
  struct cm_backpatch *ptr = CM_uncommitted_additions;
  struct cm_backpatch *prev_ptr = NULL;
  while (ptr != NULL) {
    prev_ptr = ptr;
    ptr = ptr->next;
  }
  if (prev_ptr != NULL) {
    prev_ptr->next = CM_backpatches;
    CM_backpatches = CM_uncommitted_additions;
  }
#endif
  CM_uncommitted_removals = NULL;
  CM_uncommitted_additions = NULL;
}

static void CM_Backout_Backpatches()
{
  struct cm_backpatch *ptr = CM_uncommitted_removals;
  struct cm_backpatch *prev_ptr = NULL;
  while (ptr != NULL) {
    prev_ptr = ptr;
    ptr = ptr->next;
  }
  if (prev_ptr != NULL) {
    prev_ptr->next = CM_committed_backpatches;
    CM_committed_backpatches = CM_uncommitted_removals;
  }
  CM_backpatches = CM_committed_backpatches;
  CM_uncommitted_removals = NULL;
  CM_uncommitted_additions = NULL;
}

static BOOL CM_Live_Outof_BB(TN *tn, BB *bb, OP* moved_op)
{
  if (CM_Find_Backpatch(bb, NULL, tn) != NULL)
    return TRUE;
  OP *op;
  BOOL defined = ((BB_bbregs(bb) == NULL) || 
		  (TN_is_global_reg(tn) && 
		   GTN_SET_MemberP(BB_defreach_in(bb), tn)));
  FOR_ALL_BB_OPs_FWD(bb, op) {
    if (OP_flag1(op) || (op == moved_op)) {
      for (INT i=0; i<OP_opnds(op); i++) {
	TN *opnd = OP_opnd(op,i);
	if ((opnd == tn) && defined)
	  return TRUE;
      }
    } else {
      for (INT i=0; i<OP_results(op); i++) {
	TN *res = OP_result(op,i);
	if (res == tn)
	  defined = TRUE;
      }
    }
  }
  return ((BB_bbregs(bb) == NULL) || 
	  (TN_is_global_reg(tn) && defined &&
	   GTN_SET_MemberP(BB_live_out(bb), tn)));
}

static BOOL CM_Determine_Backpatches(BB *bb, BB *alt_bb, OP *op, OP *alt_op)
{
  for (INT i=0; i<OP_opnds(alt_op); i++) {
    TN *alt_opnd = OP_opnd(alt_op, i);
    TN *opnd = OP_opnd(op,i);
    if (!TN_is_register(opnd)) continue;
    if (TN_is_dedicated(opnd) && TN_is_dedicated(alt_opnd)) {
      TOP top = OP_code(op);
      if (  ((top == TOP_ret) || (top == TOP_ret_n) ||
             (top == TOP_retw) || (top == TOP_retw_n)) && TN_is_ra_reg (opnd)
          ||
            (top == TOP_spadjust) )
      {
        // The dedicated TN can't be renamed ! Do it only if
        // both opnd and alt_opnd is the same (it should be).
        if (opnd != alt_opnd) {
          return TRUE;
        } else {
          continue;
        }
      }
        
      TN *new_tn = Dup_TN_Even_If_Dedicated (opnd);
      REGISTER reg = TN_register(opnd);
      ISA_REGCLASS regclass = TN_register_class(opnd);
      OP *prev_op = OP_prev(op);
      BOOL found_write1 = FALSE, found_read1 = FALSE;
      while (prev_op && !found_write1 && !found_read1) {
        if (OP_code(prev_op) != TOP_spadjust) {
          for (INT j=0; j<OP_results(prev_op); j++) {
            if (OP_result(prev_op,j) == opnd) {
              Set_OP_result(prev_op,j,new_tn);
              found_write1 = TRUE;
            }
          }
          for (INT j=0; j<OP_opnds(prev_op); j++) {
            if (OP_opnd(prev_op,j) == opnd) {
              found_read1 = TRUE;
            }
          }
        }
	prev_op = OP_prev(prev_op);
      }
      if (found_write1) {
	Set_OP_opnd(op,i,new_tn);
      }
#if 0
      BOOL found_write2 = FALSE;
      prev_op = OP_prev(alt_op);
      while (prev_op && !found_write2) {
	for (INT j=0; j<OP_results(prev_op); j++) {
	  if (OP_result(prev_op,j) == opnd) {
	    Set_OP_result(prev_op,j,new_tn);
	    found_write2 = TRUE;
	  }
	}
	prev_op = OP_prev(prev_op);
      }
      if (found_write2) {
	Set_OP_opnd(alt_op,i,new_tn);
      }
      if (!found_write1 && !found_write2 && (opnd != alt_opnd))
	return TRUE;
#else
      /* Dedicated register operands are ok if it's the stack pointer since
	 we don't care about its lifetime (it's always live). Other dedicated
         register operands must have lifetimes contained within a single basic 
	 block after code motion. */
      BOOL good_dedicated_use = FALSE;
      if ((regclass == REGISTER_CLASS_sp) && (reg == REGISTER_sp))
	good_dedicated_use = TRUE;
      if (!found_write1 && 
	  ((opnd != alt_opnd) || !good_dedicated_use))
	return TRUE;
#endif
    }  
  }
  for (INT i=0; i<OP_results(alt_op); i++) 
    CM_Remove_Backpatch(alt_bb, OP_result(alt_op, i), NULL);
  for (INT i=0; i<OP_results(op); i++) 
    CM_Remove_Backpatch(bb, OP_result(op, i), NULL);
  OP *new_op = Dup_OP(op);
  BOOL need_new_op = FALSE;
  for (INT i=0; i<OP_opnds(alt_op); i++) {
    TN *alt_opnd = OP_opnd(alt_op, i);
    TN *opnd = OP_opnd(op,i);
    if (!TN_is_register(opnd)) continue;
    if (TN_is_dedicated(opnd) && !TN_is_dedicated(alt_opnd)) {
#if 1
      CM_Backout_Backpatches();
      return TRUE;
#else
      if (CM_Live_Outof_BB(alt_opnd, bb, op)) {
	CM_Backout_Backpatches();
	return TRUE;
      }
      Set_OP_opnd(new_op, i, alt_opnd);
      need_new_op = TRUE;
      CM_Add_Backpatch(bb, opnd, alt_opnd);
#endif
    }
    else if (opnd != alt_opnd) {
      if (CM_Live_Outof_BB(opnd, alt_bb, alt_op)) {
#if 1
	CM_Backout_Backpatches();
	return TRUE;
#else
	if (CM_Live_Outof_BB(alt_opnd, bb, op)) {
	  CM_Backout_Backpatches();
	  return TRUE;
	}
	return TRUE;
	Set_OP_opnd(new_op, i, alt_opnd);
	need_new_op = TRUE;
	CM_Add_Backpatch(bb, opnd, alt_opnd);
#endif
      } else
	CM_Add_Backpatch(alt_bb, alt_opnd, opnd);
    }
  }
  if (need_new_op) {
    for (INT i=0; i<OP_opnds(op); i++)
      Set_OP_opnd(op, i, OP_opnd(new_op, i));
  }
  CM_Commit_Backpatches();
  return FALSE;
}

static void CM_Insert_Backpatches(BB *bb)
{
  struct cm_backpatch *ptr = CM_backpatches;
  struct cm_backpatch *prev_ptr = NULL;
  OPS copy_ops = OPS_EMPTY;
  while (ptr != NULL) {
    if (ptr->bb == bb) {
      Exp_COPY(ptr->dst_tn, ptr->src_tn, &copy_ops);

      if (!TN_is_dedicated(ptr->dst_tn)) {
	if (!TN_is_global_reg(ptr->dst_tn))
	  GTN_UNIVERSE_Add_TN(ptr->dst_tn);
	GRA_LIVE_Add_Live_Out_GTN(bb, ptr->dst_tn);
      }

      /* Copying from two different branches probably will kill the 
	 rematerializable property. */
      if (TN_is_global_reg(ptr->dst_tn) && TN_is_rematerializable(ptr->dst_tn)) {
        Reset_TN_is_rematerializable(ptr->dst_tn);
        Set_TN_home(ptr->dst_tn, NULL);
      }

      if (Get_Trace(TP_TEMP, 0x10000)) {
	fprintf(TFile, "<cmotion> Copy TN%d to TN%d in block %d\n", 
		TN_number(ptr->src_tn), TN_number(ptr->dst_tn), BB_id(bb));
      }
      if (prev_ptr)
	prev_ptr->next = ptr->next;
      else
	CM_backpatches = ptr->next;
    } else
      prev_ptr = ptr;
    ptr = ptr->next;
  }
  BB_Append_Ops(bb, &copy_ops);
}

static BOOL CM_Moveable_OP(BB *bb, OP *op)
{
  ARC_LIST *arcs;
  // Can only move ops with no successors left within the block
  for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    OP *succ = ARC_succ(ARC_LIST_first(arcs));
    if (op->bb->id != succ->bb->id)
      continue;
    if (!OP_flag1(succ))
      return FALSE;
  }

  for (INT i=0; i<OP_opnds(op); i++) {
    TN *opnd = OP_opnd(op, i);
    if (TN_is_register(opnd) && (CM_Find_Backpatch(bb, NULL, opnd) != NULL))
      return FALSE;
    if (!TN_is_dedicated(opnd))
      continue;
    // Can't move ops with dedicated TNs as operands if we can't copy them out.
    ISA_REGCLASS rc = TN_register_class(opnd);
    if ((rc != TI_ISA_Regclass_Integer()) && 
	!MTYPE_is_tie(TN_mtype(opnd)))
      return FALSE;
  }

  // Can't move ops with multiple backpatch dependencies
  for (INT i=0; i<OP_results(op); i++) {
    TN *res = OP_result(op,i);
    if (CM_Find_Backpatch(bb, res, NULL) == NULL)
      return FALSE;
  }
  
  return TRUE;
}

static OP* CM_Find_Common_OP(BB *bb, BB *alt_bb, OP* op)
{
  if (!CM_Moveable_OP(bb, op))
    return NULL;

  // do not commonize any of the asm, volatile, side-effect ops
  if (op && (OP_code(op)==TOP_asm || OP_volatile(op) || OP_side_effects(op)))
    return NULL;

  OP *alt_op;
  FOR_ALL_BB_OPs_REV(alt_bb, alt_op) {
    if (OP_flag1(alt_op))
      continue;
    BOOL same_results = FALSE;
    BOOL same_op = (OP_code(op) == OP_code(alt_op));
    for (INT i=0; i<OP_results(op); i++) {
      TN *res = OP_result(op,i);
      TN *res2 = CM_Find_Backpatch(bb, res, NULL);
      TN *alt_res;
      TN *alt_res2;
      for (INT j=0; j<OP_results(alt_op); j++) {
	alt_res = OP_result(alt_op, j);
	alt_res2 = CM_Find_Backpatch(alt_bb, alt_res, res2);
	if (res2 == alt_res2)
	  same_results = TRUE;
      }
      if (same_op) {
	alt_res = OP_result(alt_op, i);
	alt_res2 = CM_Find_Backpatch(alt_bb, alt_res, NULL);
	if (res2 != alt_res2)
	  same_op = FALSE;
      }
    }
    if (same_op) {
      for (INT i=0; i<OP_opnds(op); i++) {
	TN *opnd = OP_opnd(op,i);
	TN *alt_opnd = OP_opnd(alt_op, i);
	if (!TN_is_register(opnd) || !TN_is_register(alt_opnd)) {
	  if ((opnd != alt_opnd) &&
	      !(TN_has_value(opnd) && TN_has_value(alt_opnd) &&
		(TN_value(opnd) == TN_value(alt_opnd))) &&
	      !(TN_is_label(opnd) && TN_is_label(alt_opnd) &&
		(TN_label(opnd) == TN_label(alt_opnd))))
	    same_op = FALSE;
	} else {
          /*
             If opnd is of tie ctype, make sure they are the same
          */
          TYPE_ID opnd_mtype = TN_mtype(opnd);
          TYPE_ID alt_opnd_mtype = TN_mtype(alt_opnd);
          if (MTYPE_is_tie(opnd_mtype) || MTYPE_is_tie(alt_opnd_mtype)) {
            if (opnd_mtype != alt_opnd_mtype) {
              same_op = FALSE;
            }
          }
        }
      }
    }
    if (same_results || (OP_results(op) == 0)) {
      if (same_op) {
	if (!CM_Moveable_OP(alt_bb, alt_op))
	  return NULL;
	else
	  return alt_op;
      } else if (same_results)
	return NULL;
    }
  }
  return NULL;
}

static inline BOOL
Is_Empty_Goto_BB(BB *bb)
{ 
  return ((BB_kind(bb) == BBKIND_GOTO) && 
	  (((BB_length(bb) == 1) && (BB_branch_op(bb) != NULL)) ||
	   (BB_length(bb) == 0)));
}

static void CM_Fix_Argument_Registers(BB *bb1, BB *bb2, BB *tail_bb)
{
  OP *op;
  OPS bb1_ops = OPS_EMPTY, bb2_ops = OPS_EMPTY, tail_ops = OPS_EMPTY;
  FOR_ALL_BB_OPs_REV (bb1, op) {
    for (INT i=0; i<OP_results(op); i++) {
      TN *res = OP_result(op,i);
      if (TN_is_register(res) && TN_is_dedicated(res)) {
	REGISTER reg = TN_register(res);
	ISA_REGCLASS regclass = TN_register_class(res);
	if (REGISTER_SET_MemberP(REGISTER_CLASS_outgoing_argument(regclass),reg)) {
	  TN *new_tn = CM_Find_Backpatch(bb2, res, NULL);
	  if ((new_tn == res) || (new_tn == NULL)) {
	    new_tn = Dup_TN_Even_If_Dedicated (res);
	    Exp_COPY(res, new_tn, &tail_ops);
	  }
	  CM_Add_Backpatch(bb1, res, new_tn);
	  CM_Add_Backpatch(bb2, res, new_tn);
	}
      }
    }    
  }
  FOR_ALL_BB_OPs_REV (bb2, op) {
    for (INT i=0; i<OP_results(op); i++) {
      TN *res = OP_result(op,i);
      if (TN_is_register(res) && TN_is_dedicated(res)) {
	REGISTER reg = TN_register(res);
	ISA_REGCLASS regclass = TN_register_class(res);
	if (REGISTER_SET_MemberP(REGISTER_CLASS_outgoing_argument(regclass),reg)) {
	  TN *new_tn = CM_Find_Backpatch(bb1, res, NULL);
	  if ((new_tn == res) || (new_tn == NULL)) {
	    new_tn = Dup_TN_Even_If_Dedicated (res);
	    Exp_COPY(res, new_tn, &tail_ops);
	  }
	  CM_Add_Backpatch(bb1, res, new_tn);
	  CM_Add_Backpatch(bb2, res, new_tn);
	}
      }
    }    
  }
  BB_Prepend_Ops(tail_bb, &tail_ops);
}

static float CM_Compute_Benefit(BB *bb1, BB *bb2)
{
  OP *op;
  float benefit = 0;
  FOR_ALL_BB_OPs_FWD (bb1, op) {
    if (OP_flag1(op) && !OP_xfer(op) && (op != BB_exit_sp_adj_op(bb1))) {
      if (OP_copy(op))
	benefit += 0.5;
      else
	benefit += 1;
    }      
  }
  struct cm_backpatch *ptr = CM_backpatches;
  while (ptr != NULL) {
    if ((ptr->bb == bb1) || (ptr->bb == bb2))
      benefit -= 0.5;
    ptr = ptr->next;
  }
  return benefit;
}

/* For exit BBs, if defining OPs of all dedicated TNs cannot
   be moved down, then no move down at all (so it's either all
   or zero).  This is because each dedicated TNs cannot be alive
   across BBs (except return values for a call).

   CM_Verify_Dedicated_TNs() will return TRUE if both bb1 and
   bb2 will move all OPs that define dedicated TNs down; 
   FALSE othwerwise.
*/
static BOOL CM_Verify_Dedicated_TNs (BB *bb1, BB *bb2)
{
  OP *op;
  TN *res;
  int i;

  // So far, only need to check exit BBs
  if ( (!BB_exit(bb1)) && (!BB_exit(bb2)) ) {
    return TRUE;
  }

  FOR_ALL_BB_OPs (bb1, op) {
    if (OP_flag1(op))
      continue;

    for (i=0; i < OP_results(op); i++) {
      res = OP_result(op, i);
      if (TN_is_register(res) && TN_is_dedicated(res)) {
        return FALSE;
      }
    }
  }

  FOR_ALL_BB_OPs (bb2, op) {
    if (OP_flag1(op))
      continue;

    for (i=0; i < OP_results(op); i++) {
      res = OP_result(op, i);
      if (TN_is_register(res) && TN_is_dedicated(res)) {
        return FALSE;
      }
    }
  }

  return TRUE;
}

static BOOL CM_Find_Common_Tails(BB *bb1, BB *bb2)
{
  OP *op;
  INT move_count = 0;
  BOOL changed = FALSE;
  list<BB*> bb_list;
  bb_list.push_back(bb1);
  bb_list.push_back(bb2);

  /* Flag1 will be used to mark ops to move. Start by clearing flag1 for all 
     ops. */
  FOR_ALL_BB_OPs_FWD (bb1, op) {
    Reset_OP_flag1(op);
  }
  FOR_ALL_BB_OPs_FWD (bb2, op) {
    Reset_OP_flag1(op);
  }
  if ((BB_kind(bb1) == BBKIND_GOTO) && (BB_branch_op(bb1) != NULL))
    Set_OP_flag1(BB_branch_op(bb1));
  if ((BB_kind(bb2) == BBKIND_GOTO) && (BB_branch_op(bb2) != NULL))
    Set_OP_flag1(BB_branch_op(bb2));
  CG_DEP_Compute_Region_Graph(bb_list, INCLUDE_ASSIGNED_REG_DEPS, 
			      INCLUDE_MEMREAD_ARCS, INCLUDE_CONTROL_ARCS);

  OP *prev_op;
  for (op = BB_last_op(bb1); op != NULL; op = prev_op) {
    prev_op = OP_prev(op);
    if (OP_flag1(op))
      continue;

    // cannot commonize calls since that requires all dedicated registers
    // to be moved and that check is not in the current implementation
    if (OP_call(op))
      continue;
    OP* other_op = CM_Find_Common_OP(bb1, bb2, op);
    if (other_op) {
#if 0
      if (OP_copy(op) && OP_copy(other_op) && 
	  (CGTARG_Copy_Operand_TN(op) != CGTARG_Copy_Operand_TN(other_op))) {
	CM_Add_Backpatch(bb1, OP_result(op, 0), CGTARG_Copy_Operand_TN(op));
	CM_Add_Backpatch(bb2, OP_result(other_op, 0), 
			 CGTARG_Copy_Operand_TN(other_op));
	BB_Remove_Op(bb1, op);
	BB_Remove_Op(bb2, other_op);
      } else 
#endif
      {
	BOOL error = CM_Determine_Backpatches(bb1, bb2, op, other_op);
	if (!error) {
	  Set_OP_flag1(op);
	  Set_OP_flag1(other_op);
	  /* Do the optimization only if we merge more than one non-jump
	     instruction. Replacing a single branch with a jump is not 
	     beneficial. */
	  if (!OP_xfer(op) && (op != BB_exit_sp_adj_op(bb1)) && !OP_copy(op))
	    move_count++;
	}
      }
    } else {
      if (op != BB_exit_sp_adj_op(bb1))
	break;
    }
  }

  if ( CM_Verify_Dedicated_TNs(bb1, bb2) &&
       (CM_Compute_Benefit(bb1, bb2) > 0) ) {
    changed = TRUE;
    BB *new_tail_bb = Gen_BB();
    Insert_BB(new_tail_bb, bb1);
    BB_freq(new_tail_bb) = BB_freq(bb1)+BB_freq(bb2);
    if (Get_Trace(TP_TEMP, 0x10000)) {
      fprintf(TFile, "\n%s", DBar);
      fprintf(TFile, "<cmotion> Created block %d after blocks %d and %d\n", 
	      BB_id(new_tail_bb), BB_id(bb1), BB_id(bb2));
    }

    OP *next_op;
    for (op = BB_first_op(bb1); op != NULL; op = next_op) {
      next_op = OP_next(op);
      if (OP_flag1(op)) {
	BB_Remove_Op(bb1, op);
	BB_Append_Op(new_tail_bb, op);

	/* reset profile block id since its count
	   is the total of bb1 and bb2
	*/
	Set_OP_profile_bb_id(op,0);
	Reset_OP_flag1(op);
	if (OP_call(op))
	  Set_BB_call(new_tail_bb);
      }
    }
    for (op = BB_first_op(bb2); op != NULL; op = next_op) {
      next_op = OP_next(op);
      if (OP_flag1(op)) {
	BB_Remove_Op(bb2, op);
      }
    }

    if (BB_call(new_tail_bb)) {
      ANNOTATION *ant1, *ant2;
      ant1 = ANNOT_First(BB_annotations(bb1), ANNOT_CALLINFO);
      BB_annotations(bb1) = ANNOT_Unlink(BB_annotations(bb1), ant1);
      ant2 = ANNOT_First(BB_annotations(bb2), ANNOT_CALLINFO);
      BB_annotations(bb2) = ANNOT_Unlink(BB_annotations(bb2), ant2);
      if (WN_num_actuals(CALLINFO_call_wn(ANNOT_callinfo(ant1))) >= 
	  WN_num_actuals(CALLINFO_call_wn(ANNOT_callinfo(ant2)))) {
	ANNOT_next(ant1) = BB_annotations(new_tail_bb);
	BB_annotations(new_tail_bb) = ant1;
      } else {
	ANNOT_next(ant2) = BB_annotations(new_tail_bb);
	BB_annotations(new_tail_bb) = ant2;
      }
      Reset_BB_call(bb1);
      Reset_BB_call(bb2);
      CM_Fix_Argument_Registers(bb1, bb2, new_tail_bb);
    }
    BB_rid(new_tail_bb) = BB_rid(bb1);
    BB_MAP32_Set(CM_bb_info_map, new_tail_bb, 
		 BB_MAP32_Get(CM_bb_info_map, bb1));

    if (BB_exit(bb1)) {
      ANNOTATION *ant;
      ant = ANNOT_First(BB_annotations(bb1), ANNOT_EXITINFO);
      BB_annotations(bb1) = ANNOT_Unlink(BB_annotations(bb1), ant);
      ANNOT_next(ant) = BB_annotations(new_tail_bb);
      BB_annotations(new_tail_bb) = ant;
      ant = ANNOT_First(BB_annotations(bb2), ANNOT_EXITINFO);
      BB_annotations(bb2) = ANNOT_Unlink(BB_annotations(bb2), ant);
      Reset_BB_exit(bb1);
      Reset_BB_exit(bb2); 
      Set_BB_exit(new_tail_bb);
      Exit_BB_Head = BB_LIST_Delete(bb1, Exit_BB_Head);
      Exit_BB_Head = BB_LIST_Delete(bb2, Exit_BB_Head);
      Exit_BB_Head = BB_LIST_Push(new_tail_bb, Exit_BB_Head, &MEM_pu_pool);
    }
    CM_Insert_Backpatches(bb2);
    CM_Insert_Backpatches(bb1);

    LABEL_IDX lab = Gen_Label_For_BB(new_tail_bb);
    TN *lab_tn = Gen_Label_TN(lab, 0);
    OPS ops1 = OPS_EMPTY, ops2 = OPS_EMPTY;
    Exp_OP1(OPC_GOTO, NULL, lab_tn, &ops1);
    BB_Append_Ops(bb1, &ops1);
    Exp_OP1(OPC_GOTO, NULL, lab_tn, &ops2);
    BB_Append_Ops(bb2, &ops2);
    
    BBLIST *succ_list, *next_list;
    BB *succ_bb;
    for (succ_list = BB_succs(bb1); succ_list != NULL; succ_list = next_list) {
      succ_bb = BBLIST_item(succ_list);
      next_list = BBLIST_next(succ_list);
      BBLIST *succ_list2;
      for (succ_list2 = BB_succs(bb2); succ_list2 != NULL; 
	   succ_list2 = BBLIST_next(succ_list2)) {
	BB *tmp1, *tmp2, *next_bb;
	tmp1 = succ_bb;
	while (Is_Empty_Goto_BB(tmp1) && (next_bb = BB_Unique_Successor(tmp1)))
	  tmp1 = next_bb;
	tmp2 = BBLIST_item(succ_list2);
	while (Is_Empty_Goto_BB(tmp2) && (next_bb = BB_Unique_Successor(tmp2)))
	  tmp2 = next_bb;
	if (tmp1 == tmp2)
	  break;
      }
      Is_True(succ_list2 != NULL, ("Cannot find common successors"));
      float prob1 = BBLIST_prob(succ_list);
      float prob2 = BBLIST_prob(succ_list2);
      float combine_prob = 
        (prob1 == 1.0f && prob2 == 1.0f ? 1.0f :
	 (BB_freq(new_tail_bb) != 0.0f ?
          (BB_freq(bb1) * prob1 + BB_freq(bb2) * prob2) / BB_freq(new_tail_bb) :
          (prob1 + prob2) / 2.0f));
      Link_Pred_Succ_with_Prob(new_tail_bb, succ_bb, combine_prob);
      Unlink_Pred_Succ(bb1, succ_bb);
    }
    for (succ_list = BB_succs(bb2); succ_list != NULL; succ_list = next_list) {
      succ_bb = BBLIST_item(succ_list);
      next_list = BBLIST_next(succ_list);
      Unlink_Pred_Succ(bb2, succ_bb);
    }
    Link_Pred_Succ_with_Prob(bb1, new_tail_bb, 1.0);
    Link_Pred_Succ_with_Prob(bb2, new_tail_bb, 1.0);
    GRA_LIVE_Compute_Liveness_For_BB(new_tail_bb);
    if (Get_Trace(TP_TEMP, 0x10000)) {
      fprintf(TFile, "\n<cmotion> BBs after this code motion \n%s", DBar);
      Print_BB(bb1);
      Print_BB(bb2);
      Print_BB(new_tail_bb);
    }
  }

  CG_DEP_Delete_Graph(&bb_list);
  return changed;
}

/* Two BB's must have identical successor BB lists to be optimized */
static BOOL CM_Same_Succs(BB *bb1, BB* bb2)
{
  if (bb1 == bb2)
    return FALSE;
  BBLIST *succ_list1, *succ_list2;
  BB *succ1, *succ2;
  FOR_ALL_BB_SUCCS(bb1, succ_list1) {
    succ1 = BBLIST_item(succ_list1);
    // Skip succ1 if it is empty and has unique succ and pred
    while (Is_Empty_Goto_BB(succ1) &&
           (BB_Unique_Successor(succ1) != NULL) &&
           (BB_Unique_Predecessor(succ1) != NULL)) {
      succ1 = BB_Unique_Successor(succ1);
    }

    BOOL found_succ1 = FALSE;
    FOR_ALL_BB_SUCCS(bb2, succ_list2) {
      succ2 = BBLIST_item(succ_list2);
      // Skip succ2 if it is empty and has unique succ and pred
      while (Is_Empty_Goto_BB(succ2) &&
             (BB_Unique_Successor(succ2) != NULL) &&
             (BB_Unique_Predecessor(succ2) != NULL)) {
        succ2 = BB_Unique_Successor(succ2);
      }

      if (succ1 == succ2) {
	found_succ1 = TRUE;
	break;
      }
    }
    if (!found_succ1)
      return FALSE;
  }
  FOR_ALL_BB_SUCCS(bb2, succ_list2) {
    succ2 = BBLIST_item(succ_list2);
    // Skip succ2 if it is empty and has unique succ and pred
    while (Is_Empty_Goto_BB(succ2) &&
           (BB_Unique_Successor(succ2) != NULL) &&
           (BB_Unique_Predecessor(succ2) != NULL)) {
      succ2 = BB_Unique_Successor(succ2);
    }
    BOOL found_succ2 = FALSE;
    FOR_ALL_BB_SUCCS(bb1, succ_list1) {
      succ1 = BBLIST_item(succ_list1);
      // Skip succ1 if it is empty and has unique succ and pred
      while (Is_Empty_Goto_BB(succ1) &&
             (BB_Unique_Successor(succ1) != NULL) &&
             (BB_Unique_Predecessor(succ1) != NULL)) {
        succ1 = BB_Unique_Successor(succ1);
      }
      if (succ1 == succ2) {
	found_succ2 = TRUE;
	break;
      }
    }
    if (!found_succ2)
      return FALSE;
  }
  return TRUE;
}

static void CM_Init_Work_List()
{
  BB *bb1, *bb2;
  CM_work_list.clear();
  for (bb1 = REGION_First_BB; bb1 != NULL; bb1 = BB_next(bb1)) {
    if (Is_Empty_Goto_BB(bb1))
      continue;
    for (bb2 = BB_next(bb1); bb2 != NULL; bb2 = BB_next(bb2)) {
      if (Is_Empty_Goto_BB(bb2))
	continue;
      if (BB_rid(bb1) != BB_rid(bb2))
	continue;
      if (BB_MAP32_Get(CM_bb_info_map, bb1) != 
	  BB_MAP32_Get(CM_bb_info_map, bb2))
	continue;
      if (CM_Same_Succs(bb1, bb2))
	CM_work_list.push_back(std::make_pair(bb1, bb2));
    }
  }
}

static void CM_Downward_Motion()
{
  BB *bb, *cand_bb, *succ_bb;
  BOOL found_tail = FALSE;
  CM_Init_Work_List();
  while (!CM_work_list.empty()) {
    pair<BB*, BB*> list_head = CM_work_list.front();
    CM_work_list.pop_front();
    bb = list_head.first;
    cand_bb = list_head.second;
    if (CM_Find_Common_Tails(bb, cand_bb)) {
      found_tail = TRUE;
      list< pair<BB*, BB*> >::iterator list_ptr = CM_work_list.begin();
      while (list_ptr != CM_work_list.end()) {
	if ((list_ptr->first == bb) || (list_ptr->first == cand_bb) ||
	    (list_ptr->second == bb) || (list_ptr->second == cand_bb))
	  list_ptr = CM_work_list.erase(list_ptr);
	else
	  list_ptr++;
      }
#if 0
      BB *new_succ = BB_Unique_Successor(bb);
      Is_True(new_succ != NULL, ("Bad code motion"));
      for (BB *bb1 = REGION_First_BB; bb1 != NULL; bb1 = BB_next(bb1)) {
	if (Is_Empty_Goto_BB(bb1))
	  continue;
	if (CM_Same_Succs(bb1, new_succ))
	  CM_work_list.push_back(std::make_pair(bb1, new_succ));
      }

      BBLIST *list1, *list2;
      if (Is_Empty_Goto_BB(bb) && Is_Empty_Goto_BB(cand_bb)) {
	FOR_ALL_BB_PREDS(bb, list1) {
	  BB *pred1 = BBLIST_item(list1);
	  FOR_ALL_BB_PREDS(cand_bb, list2) {
	    BB *pred2 = BBLIST_item(list2);
	    if (CM_Same_Succs(pred1, pred2))
	      CM_work_list.push_back(std::make_pair(pred1, pred2));
	  }
	}
      } else if (Is_Empty_Goto_BB(bb)) {
	FOR_ALL_BB_PREDS(bb, list1) {
	  BB *pred = BBLIST_item(list1);
	  if (CM_Same_Succs(pred, cand_bb))
	    CM_work_list.push_back(std::make_pair(cand_bb, pred));
	}
      } else if (Is_Empty_Goto_BB(cand_bb)) {
	FOR_ALL_BB_PREDS(cand_bb, list2) {
	  BB *pred = BBLIST_item(list2);
	  if (CM_Same_Succs(pred, bb))
	    CM_work_list.push_back(std::make_pair(bb, pred));
	}
      }
#endif
    }
    if (CM_work_list.empty() && found_tail) {
      found_tail = FALSE;
      CM_Init_Work_List();
    }
  }
}

static void
CM_Initialize_EH_BB_Map(void)
{
  /* We maintain a stack of exception regions so that we can assign
   * a unique ID to each region. The stack is implemented with a linked
   * list of the following structures:
   */
  struct eh_ctx {
    INT32 rgn;			/* ID for this EH region */
    struct eh_ctx *prev;	/* Previous stack entry */
    struct eh_ctx *next;	/* Next stack entry */
  };

  struct eh_ctx eh_stack;	/* The bottom of the stack */
  struct eh_ctx *eh_tos;	/* The top of the stack -- eh_tos->next
				 * points to unused link list elements
				 */
  INT eh_rgn;			/* The current EH region ID */
  INT eh_id;			/* Highest EH region ID used */
  BB *bb;

  /* Init the exception region ID. We'll assign each exc region
   * a unique ID.
   */
  eh_id = 0;

  /* Create a stack that we'll use to save the current exc region ID
   * when we enter a new eh region.  The top-of-stack contains the
   * current eh region ID.
   */
  eh_tos = &eh_stack;
  eh_tos->rgn = 0;
  eh_tos->prev = NULL;
  eh_tos->next = NULL;

  CM_bb_info_map = BB_MAP32_Create();
  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    /* If the block has labels, process any eh begin/end labels.
     */
    eh_rgn = eh_tos->rgn;
    if (BB_has_label(bb)) {
      ANNOTATION *ant;

      for (ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
	   ant != NULL;
	   ant = ANNOT_Next(ant, ANNOT_LABEL)
      ) {
	LABEL_IDX lab = ANNOT_label(ant);
	switch (LABEL_kind(Label_Table[lab])) {
	case LKIND_BEGIN_EH_RANGE:
	  {
	    struct eh_ctx *eh_next = eh_tos->next;

	    /* Start of a new eh region, save the old ID and setup the new.
	     * If there are available link entries, use the first one,
	     * otherwise allocate a new one.
	     */
	    if (eh_next == NULL) {
	      eh_next = (eh_ctx *)alloca(sizeof(struct eh_ctx));
	      eh_next->next = NULL;
	      eh_next->prev = eh_tos;
	      eh_tos->next = eh_next;
	    }
	    eh_tos = eh_next;
	    eh_rgn = ++eh_id;
	    eh_tos->rgn = eh_rgn;
	  }
	  break;
	case LKIND_END_EH_RANGE:

	  /* End of an eh region; restore the outer region's ID.
	   */
	  eh_tos = eh_tos->prev;
	  Is_True(eh_tos, ("exception stack underflow"));
	  break;
	}
      }
    }
    BB_MAP32_Set(CM_bb_info_map, bb, eh_rgn);
  }
}

/* This optimization merges instructions from two halves of a branch. Note that
   global live ranges and block frequencies need to be recomputed after this
   optimization has run. */
void CM_Optimize_Region()
{
  if ((CG_opt_level > 1) && CM_Enable && OPT_Space) {
    if (Get_Trace(TP_TEMP, 0x10000)) {
      fprintf(TFile, "\n%s Code Motion\n%s", DBar, DBar);
    }
    MEM_POOL_Push(&MEM_local_pool);
    CM_backpatches = NULL;
    CM_committed_backpatches = NULL;
    CM_uncommitted_additions = NULL;
    CM_uncommitted_removals = NULL;
    CM_Initialize_EH_BB_Map();
    CM_Downward_Motion();
    BB_MAP_Delete(CM_bb_info_map);
    MEM_POOL_Pop(&MEM_local_pool);
  }
}

// Local Variables:
// mode:c++
// c-basic-offset:2
// End:

