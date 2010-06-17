
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


// =======================================================================
// =======================================================================
//
//  Module: hb_hazards.cxx
//  $Revision: 1.26 $
//  $Date: 2000/04/28 21:59:02 $
//  $Author: srinivas $
//  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/hb_hazards.cxx,v $
//
//  Description:
//  ============
//
//  Interface to the all hazard detection routines.
//
// =======================================================================
// =======================================================================

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <alloca.h>
#include <math.h>
#include "defs.h"
#include "config.h"
#include "config_targ_options.h"
#include "config_asm.h"
#include "mempool.h"
#include "bb.h"
#include "bb_set.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cg.h"
#include "cg_flags.h"
#include "cgexp.h"
#include "ercg.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "dominate.h"
#include "findloops.h"
#include "hb_hazards.h"
#include "note.h"
#include "lra.h"
#include "gcm.h"
#include "ti_res.h"
#include "ti_res_res.h"
#include "ti_latency.h"
#include "ti_errors.h"
#include "cg_region.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cxx_memory.h"
#include "ti_bundle.h"
#include "fb_whirl.h"

TI_RES_RES* HB_Hazard_resource_table;
INT HB_Hazard_current_cycle;


// ======================================================================
// Given a scheduling note <info>, print it to <file>.
// ======================================================================
static void Print_Scheduling_Note (
  NOTE_ACTION action,
  NOTE_INFO *info,
  FILE *file)
{
  switch (action) {
  case NOTE_PRINT_TO_ANL_FILE:
  case NOTE_PRINT_TO_FILE:
    {
      NOTE_SCHED *note = (NOTE_SCHED *)info;
      const char *prefix = ASM_CMNT_LINE"<sched> ";
      const char *suffix = "\n";
      INT sched_length = note->schedule_length;
      INT blk_parallelism = note->block_parallelism;
      BOOL anl_note = action == NOTE_PRINT_TO_ANL_FILE;

      // By default only print out the loop scheduling notes.
      if (sched_length >= 0 && !Get_Trace (TP_SCHED, 0x010)) return;

      if (anl_note) {
	if (sched_length > 0) return;
	prefix = "\"";
	suffix = "\"\n";
      } else {
	fprintf (file, "%s\n", prefix);
      }

      if (sched_length < 0) {
        sched_length  = -sched_length;
	if (anl_note) {
	  SRCPOS srcpos = note->loop_srcpos;
	  INT32 lineno = SRCPOS_linenum(srcpos);
	  INT32 fileno = SRCPOS_filenum(srcpos);
	  INT32 colno = SRCPOS_column(srcpos);
	  fprintf (file,
		   "\nmsg sched lines [%d %d %d] ",
		   fileno,
		   lineno,
		   colno);
	}
        fprintf (file, 
	         "%sLoop schedule length: %d cycles (ignoring nested loops)%s", 
	         prefix, 
	         sched_length,
		 suffix);
      }
      else {
        fprintf (file, "%s bb schedule length: %d cycles\n", prefix, sched_length);
        fprintf (file, "%s bb parallelism : %d \n", prefix, blk_parallelism);
      }
      if (!anl_note) fprintf (file, "%s\n", prefix);
      CGTARG_Print_PRC_INFO (file, &note->prc_info, sched_length, prefix, suffix);
      if (!anl_note) fprintf (file, "%s\n", prefix);
    }
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file, "Print_Scheduling_Note");
    break;
  }
}


// ======================================================================
// Create a scheduling note for the given <bb>.
// ======================================================================
void
Add_Scheduling_Note (BB *bb, void *bbsch)
{
  NOTE_SCHED *info = NULL;
  BOOL new_note = FALSE;

  // check if there is a note already. If yes, we only need to update it.
  if (BB_has_note(bb)) {
    info = (NOTE_SCHED *)NOTE_Retrieve_Note_For_Handler (
					  bb, Print_Scheduling_Note);
  }
  if (info == NULL) {
    info = TYPE_MEM_POOL_ALLOC (NOTE_SCHED, &MEM_pu_pool);
    new_note = TRUE;
  }

  // this should really be got from BBSCH data structure
  info->schedule_length = OP_scycle(BB_last_op(bb))+1;
#ifdef TARG_XTENSA
  /* For simulated OPs, adjust the schedule length to account for how
     many real instructions that simulated OP emits. */
  OP *op;
  FOR_ALL_BB_OPs(bb, op)
    if (OP_simulated(op))
    {
      INT real_ops = Simulated_Op_Real_Ops(op);
      info->schedule_length += (real_ops - 1);
    }
  info->schedule_length = MAX(0, info->schedule_length);
#endif
  
  info->block_parallelism = (bbsch != NULL) ? 
		BBSCH_block_parallelism ((BBSCH*)bbsch) : 0;

  CGTARG_Compute_PRC_INFO (bb, &(info->prc_info));
  FmtAssert (info->schedule_length >= 0, 
		  ("illegal schedule length, BB:%d",BB_id(bb)));
  if (new_note) 
    NOTE_Add_To_BB (bb, Print_Scheduling_Note, (NOTE_INFO *)info);
}

static INT
Weighted_Schedule_Length (float freq, INT length)
{
  /* If frequency is based on heuristics, then ignore it. */
  if (!Cur_PU_Feedback)
    return length;

  return ((INT)(freq * (float)length + (float)0.5));
}  

void
Add_Scheduling_Notes_For_Loops (void)
{
  Calculate_Dominators ();
  L_Save ();
  LOOP_DESCR *loop_list = LOOP_DESCR_Detect_Loops (&MEM_local_pool);
  BB_SET *processed_bbs = BB_SET_Create_Empty (PU_BB_Count+2, &MEM_local_pool);
  for (LOOP_DESCR *cloop = loop_list;
       cloop != NULL;
       cloop = LOOP_DESCR_next(cloop))
  {
#ifdef TARG_XTENSA
    /* Only add scheduling notes for single-bb innermost loops. The
       others can be confusing. */
    if (BB_SET_Size(LOOP_DESCR_bbset(cloop)) == 1)
#endif
    {
      BB *bb;
      NOTE_SCHED *info = TYPE_MEM_POOL_ALLOC (NOTE_SCHED, &MEM_pu_pool);
      BB *loop_head = LOOP_DESCR_loophead(cloop);
      info->loop_srcpos = BB_Loop_Srcpos(loop_head);

      FOR_ALL_BB_SET_members (LOOP_DESCR_bbset(cloop), bb) {
        // if bb has already been processed, ignore it.
        if (BB_SET_MemberP (processed_bbs, bb)) continue;
	NOTE_SCHED *bbinfo = (NOTE_SCHED *)NOTE_Retrieve_Note_For_Handler (
							 bb, Print_Scheduling_Note);
	if (bbinfo != NULL) {
	  float head_freq = BB_freq(loop_head);
	  float freq_ratio = head_freq == 0.0 ? 0.0 : BB_freq(bb)/head_freq;
	  info->schedule_length -= 
	    Weighted_Schedule_Length (freq_ratio, bbinfo->schedule_length);
	  info->block_parallelism = bbinfo->block_parallelism;
	  for (INT i = 0; i < PRC_LAST; i++) {
	    info->prc_info.refs[i] += 
	      Weighted_Schedule_Length (freq_ratio, bbinfo->prc_info.refs[i]);
	  }
	}
      }

      if (info->schedule_length < 0) {
	NOTE_Add_To_BB (loop_head, Print_Scheduling_Note, (NOTE_INFO *)info);
      }

      processed_bbs = BB_SET_Union (processed_bbs, LOOP_DESCR_bbset(cloop),
				    &MEM_local_pool);
    }
  }
  L_Free ();
  Free_Dominators_Memory ();
}

// ======================================================================
// Noop Insertion:
//
// The following set of routines handle the detection of hazards and 
// insertion of noops to eliminate the hazards. This is the last phase
// of the local scheduler invoked after register allocation.
// ======================================================================


/* Operand numbers are a positive value in the range 0..OP_opnds(op)-1.
 * Define special operand numbers we use so we can have common
 * routines to handle the various kinds of hazards.
 */
#define OPND_RESULT -1	/* The dependence is on the result */
#define OPND_NONE -2	/* There is no operand or result dependence */

// ======================================================================
// Returns TRUE if <op> is in the delay slot of the terminating branch
// in <bb>.
// ======================================================================
BOOL
Is_Delay_Slot_Op (OP *op, BB *bb)
{
  if (op != BB_last_op(bb)) return FALSE;
  OP *xfer_op = OP_prev(op);
  if (xfer_op == NULL || !OP_xfer(xfer_op)) return FALSE;
  return TRUE;
}


// ======================================================================
// Check if there is a post-hazard between 'op1' (operand 'opnd') and 
// 'op2'. Return TRUE if this is a hazard, FALSE otherwise.
// ======================================================================
static BOOL
Detect_Post_Hazard (OP *op1, INT opnd, OP *op2)
{
  TN *tn;
  ISA_REGCLASS cl;
  REGISTER reg;

  switch (opnd) {
  case OPND_NONE:
    return FALSE;

  case OPND_RESULT:
    // is there an flow- or output-dep to scan_op ?
    tn = OP_result(op1,0 /*???*/);
    cl = TN_register_class(tn);
    reg = TN_register(tn);

    // if result register is $0 there is no hazard.
    if (TN_is_zero_reg (tn)) {
      return FALSE;
    }

    return OP_Refs_Reg (op2, cl, reg) || CGTARG_OP_Refs_TN (op2, tn);

  default:
    // is there an flow- or output-dep to scan_op ?
    tn = OP_opnd(op1, opnd);
    cl = TN_register_class(tn);
    reg = TN_register(tn);

    return OP_Defs_Reg (op2, cl, reg) || CGTARG_OP_Defs_TN (op2, tn);
  }
  /*NOTREACHED*/
}


// ======================================================================
// Determine if there is a pre-hazard between 'op1' and 'op2' 
// (operand 'opnd'). Return TRUE if this is a hazard, FALSE otherwise.
// ======================================================================
static BOOL
Detect_Pre_Hazard (OP *op1, OP *op2, INT opnd)
{
  TN *operand;
  ISA_REGCLASS cl;
  REGISTER reg;

  // is there a flow-dep from op1?
  operand = OP_opnd(op2,opnd);
  cl = TN_register_class(operand);
  reg = TN_register(operand);
  return OP_Defs_Reg (op1, cl, reg) || CGTARG_OP_Defs_TN (op1, operand);
}


// ======================================================================
// Handle the pre-hazard for the 'op' (operand 'opnd'). 'ops_to_check'
// gives the size of the hazard.
// ======================================================================
static void
Handle_Pre_Hazard (OP *op, INT opnd, INT ops_to_check)
{
  OP *scan_op = op;
  BOOL add_noops = FALSE;

  while (ops_to_check > 0) {
    scan_op = OP_prev(scan_op);
    if (scan_op == NULL) {
      add_noops = TRUE;
      break;
    }
    // we don't count dummy ops because they are not emitted.
    if (OP_dummy(scan_op)) continue;

    if (Detect_Pre_Hazard (scan_op, op, opnd))
    {
      add_noops = TRUE;
      break;
    }
    ops_to_check-= OP_Real_Ops (scan_op);;
  }

  if (add_noops) {
    // add ops_to_check number of noops.
    BB_Insert_Noops (op, ops_to_check, TRUE);
  }
}


// ======================================================================
// Handle the post-hazard for the 'op' (operand 'opnd'). 'ops_to_check'
// gives the size of the hazard.
// ======================================================================
static void
Handle_Post_Hazard (OP *op, INT opnd, INT ops_to_check)
{
  OP *scan_op = op;
  BOOL add_noops = FALSE;

  while (ops_to_check > 0) {
    scan_op = OP_next(scan_op);
    if (scan_op == NULL) {
      add_noops = TRUE;
      break;
    }
    // we don't count dummy ops because they are not emitted.
    if (OP_dummy(scan_op)) continue;

    if (Detect_Post_Hazard (op, opnd, scan_op)) {
      add_noops = TRUE;
      break;
    }
    ops_to_check -= OP_Real_Ops (scan_op);
    if (OP_xfer(scan_op)) ops_to_check -= 1;  // account for the delay slot
  }
  if (add_noops) {
    // add ops_to_check number of noops.
    BB_Insert_Noops (op, ops_to_check, FALSE);
  }
}

// ======================================================================
// To deal with additive hazards, we maintain a queue of post-hazards
// and look at them while handling pre-hazards. We remember only 
// the last PQ_SIZE number of post-hazards.
// ======================================================================
#define PQ_SIZE 4
static struct {
  OP *op;
  mINT16 opnd;
  mINT16 numops;
} post_Q[PQ_SIZE+1];


// ======================================================================
// Add a hazard to the post-hazard queue. The order of elements in the
// queue is reversed (i.e. the last hazard added is at index 0).
// ======================================================================
static void
Add_Post_Hazard_To_Q (OP *op, INT opnd, INT numops)
{
  INT i;

  for (i = PQ_SIZE; i > 0; i--) {
    post_Q[i] = post_Q[i-1];
  }
  post_Q[0].op = op;
  post_Q[0].opnd = opnd;
  post_Q[0].numops = numops;
}


// ======================================================================
// Handle the case of possible additive hazards (when a post-hazard is
// added to a pre-hazard to come up with a combined hazard). An example
// of additive hazards is a ctc1,bc1t sequence for mips1-3.
// ======================================================================
static void
Handle_Additive_Hazards (OP *op, INT opnd, INT numops)
{
  INT i;

  for (i = 0; i < PQ_SIZE; i++) {
    OP *pq_op = post_Q[i].op;
    if (pq_op == NULL) break;
    if (Detect_Post_Hazard (pq_op, post_Q[i].opnd, op)) {
      OP *tmp_op;
      INT pq_noops = post_Q[i].numops;

      if (Detect_Pre_Hazard (pq_op, op, opnd)) pq_noops += numops;

      for (tmp_op = OP_next(pq_op); tmp_op != op; tmp_op = OP_next(tmp_op)) {
	pq_noops -= OP_Real_Ops (tmp_op);
      }
      if (pq_noops > 0) {
	// add pq_noops before op.
	BB_Insert_Noops (op, pq_noops, TRUE);
      }
    }
  }
}

// ======================================================================
// Placeholder routine to check if <op> has any dependence conflict with
// <prev_op>.
// ======================================================================
static BOOL
Is_There_OP_Dependence(OP *op, OP *prev_op)
{

  INT j;

  if (CGTARG_Dependence_Required(prev_op, op)) return TRUE;

  // cannot bundle two memory ops together unless they are scheduled already
  if ((OP_memory(prev_op) && OP_memory(op)) && BB_scheduled(op->bb)==false)
    return TRUE;

  for (j = 0; j < OP_results(prev_op); ++j) {
    TN *result_tn = OP_result(prev_op, j);
    ISA_REGCLASS cl = TN_register_class(result_tn);
    REGISTER reg = TN_register(result_tn);

    BOOL read_dependence = 
      OP_Refs_Reg (op, cl, reg) || CGTARG_OP_Refs_TN (op, result_tn);

    BOOL write_dependence =
      OP_Defs_Reg (op, cl, reg) || CGTARG_OP_Defs_TN (op, result_tn);

    if (read_dependence || write_dependence) {

      // Exclude specific cases here.
      // (1) Ignore dependence between an integer compare operation
      // which sets the predicate and the branch operation which uses
      // the same predicate.

      if (OP_icmp(prev_op) && OP_xfer(op)) {
	TN *tn1, *tn2;
	OP *cmp_op;
	
	CGTARG_Analyze_Compare(op, &tn1, &tn2, &cmp_op);
	if (prev_op == cmp_op) continue;
      }

      // (2) Ignore all dependences originating from p0 predicate.
      if (TN_is_true_pred(result_tn)) continue;

      // (3) Ignore dependences originating from exclusive predicates
      if (OP_has_disjoint_predicate(prev_op, op)) continue;

      // (4) If a STOP bit already exists in the bundle for that specific
      // position, then the dependence can be relaxed.
      // if (TI_BUNDLE_stop_bit(bundle, i)) continue;

      // (5) A check load and a subsequent instruction that reads the
      // target of the check load may exist in the same instruction group.
      if (read_dependence && CGTARG_Is_OP_Speculative(prev_op)) continue;

      return TRUE;
    }
  }

  return FALSE;
}

// ======================================================================
// Placeholder routine to check if <op> has any conflicts with prev_ops
// in the instruction group <bundle_vector>.
// ======================================================================
static BOOL
Is_There_Group_Dependence(OP *op, VECTOR *bundle_vector)
{
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TOP alt_topcode = TOP_UNDEFINED;
  if (Convert_TNs_To_INT(op, tn_num_array))
    alt_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY) -1,
				tn_num_array);
  if (!TI_RES_RES_Resources_Available(HB_Hazard_resource_table, OP_code(op), 
				      HB_Hazard_current_cycle, alt_topcode))
    return TRUE;

  for (INT i = 0; i < VECTOR_count(*bundle_vector); ++i) {
    OP *prev_op = (OP *) VECTOR_element(*bundle_vector, i);
    if (Is_There_OP_Dependence(op, prev_op)) return TRUE;
  }

  return FALSE;
}

// ======================================================================
// Placeholder routine to check if placement of <op> at <slot_pos> in
// a <bundle> can be delayed.
// ======================================================================
static BOOL
Delay_Scheduling_OP(OP *op, INT slot_pos, TI_BUNDLE *bundle)
{

  // If <op> is a <xfer_op>, we would like to not greedily bundle it.
  // Rather, delay the same, so that nops (if necessary) can be 
  // inserted before (instead of after). As a result, any <xfer_op>
  // will be the last_op in a legal bundle.
  INT template_bit = TI_BUNDLE_Return_Template(bundle);

  if (BB_last_op(OP_bb(op)) == op && OP_xfer(op) && 
      (slot_pos != (TI_ISA_Num_Slots(template_bit) - 1))) 
    return TRUE;

  // If <op> needs to be the last member of the group, check for slot
  // positions and any prior stop bits present.
  if (OP_l_group(op) && (slot_pos != (TI_ISA_Num_Slots(template_bit) - 1)) &&
      !TI_BUNDLE_stop_bit(bundle, slot_pos))
    return TRUE;

  return FALSE;

}

// ======================================================================
// Placeholder for all issues related to formation of legal bundles.
// Specific functionalities include: legal insts grouping within a bundle,
// Insertion of necessary nops and stop bits.
// ======================================================================
static void
Check_For_Bundle_Hazards(OP *op, TI_BUNDLE *bundle, VECTOR *bundle_vector)
{

  BOOL slot_avail = FALSE;

  // Legal values:
  // <slot_pos> : 0, 1,.. ISA_BUNDLE_MAX_SLOTS - 1
  // <stop_pos> : -1, 0, 1, ... ISA_BUNDLE_MAX_SLOTS - 1
  // the extra "-1" stop_pos indicates a group dependence outside the
  // context of the current bundle but still need to be preserved.

  INT slot_pos = -1; // not a legal value
  INT stop_pos = -2; // not a legal value

  // If <op> already bundled, ignore..
  if (OP_bundled(op)) return;

  INT i;
  ISA_EXEC_UNIT_PROPERTY prop;
  INT ti_err;
  BOOL stop_bit_reqd = FALSE;

  // No need to process black-box OPs (eg. TOP_asm, TOP_intrncall, etc)
  // For those, cases, finish, the current bundle, insert stop bits around
  // these OPs, and continue.

  BOOL bundling_reqd = (OP_code(op) != TOP_asm);
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TOP alt_topcode = TOP_UNDEFINED;
  if (Convert_TNs_To_INT(op, tn_num_array))
    alt_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY) -1,
				tn_num_array);

  if (bundling_reqd) {

    // Stop bit is required, when there exists a group dependence.
    stop_bit_reqd = Is_There_Group_Dependence(op, bundle_vector);

    FOR_ALL_SLOT_MEMBERS (bundle, i) {
 
      // STOP bit may be required, if <op> needs to be the first element
      // in an instruction group.
      stop_bit_reqd |= (i > 0) && OP_f_group(op);

      // Check for availability at slot <i> position in <bundle>.
      if (CGTARG_Bundle_Slot_Available (bundle, op, i, &prop,
					stop_bit_reqd, NULL)) {

	slot_avail = TRUE;
	slot_pos = i;
	// Assumes that stop_bit position is also available.
    	if (stop_bit_reqd) {
	  if (i > 0) stop_pos = i - 1;
	  else stop_pos = -1;
	}
	// If there is a need to delay the scheduling of <op>...
	if (!Delay_Scheduling_OP(op, i, bundle)) break;
      }
    }
  }

  // If slot available, reserve the entry in TI_BUNDLE.
  if (slot_avail) {
    Set_OP_bundled (op);
    TI_BUNDLE_Reserve_Slot (bundle, slot_pos, prop);

    // Inter-Bundle dependence, need to set the stop bit.
    if (stop_pos >= 0) {
      TI_BUNDLE_Reserve_Stop_Bit (bundle, stop_pos);
    }

    // Intra-Bundle dependence, no need to set the stop bit but need to
    // reset the bundle_vector to begin new instruction group.

    if (stop_pos >= -1) {
      HB_Hazard_current_cycle++;
      OP* prev_op = NULL;
      if (op) prev_op = OP_prev(op);
      if (op && prev_op && OP_scycle(op)>=0 && OP_scycle(prev_op)>=0) {
	int cycles = OP_scycle(op) - OP_scycle(prev_op);
	if (cycles>0)
          HB_Hazard_current_cycle += (cycles-1);
      }

      VECTOR_Reset (*bundle_vector);
    }

    TI_RES_RES_Reserve_Resources(HB_Hazard_resource_table, OP_code(op), 
				 HB_Hazard_current_cycle, alt_topcode);
    VECTOR_Add_Element (*bundle_vector, op);

    // Check for any hazards (nops) that may need to be filled.
    CGTARG_Handle_Bundle_Hazard(op, bundle, bundle_vector, slot_avail, 
				slot_pos, slot_pos, stop_bit_reqd, prop);

    // Add the <stop_bit> marker appropriately.
    // TODO: need to be refined further.
    if (stop_pos >= -1) {
      OP *last_real_op = Last_Real_OP(op);
      Set_OP_end_group(last_real_op);
    }
  }

  BOOL bundle_full = TI_BUNDLE_Is_Full(bundle, &ti_err);
  FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));

  // If slot not available or <op> is the last op in bb, do extra stuff.
  if (!slot_avail || (BB_last_real_op(OP_bb(op)) == op) || bundle_full) {

    // Pack NOPs till end of the bundle.
    CGTARG_Handle_Bundle_Hazard (op, bundle, bundle_vector, slot_avail,
				 slot_pos, ISA_BUNDLE_MAX_SLOTS, stop_bit_reqd, prop);
    // Reset the bundle
    TI_BUNDLE_Clear (bundle);
    // VECTOR_Reset (*bundle_vector);

    // Set the <end_group> flag either, if
    // <op> has to be last inst in the instruction group. or,
    // OP_prev(op) has to be first instruction in a group.

    if (OP_l_group(op)) {
      Set_OP_end_group(op);
      HB_Hazard_current_cycle++;
      VECTOR_Reset (*bundle_vector);
    }

    if (OP_f_group(op) && OP_prev(op)) {
      OP *last_real_op = Last_Real_OP (op);
      Set_OP_end_group(last_real_op);
      HB_Hazard_current_cycle++;
      VECTOR_Reset (*bundle_vector);
      TI_RES_RES_Reserve_Resources(HB_Hazard_resource_table, OP_code(op), 
				   HB_Hazard_current_cycle, alt_topcode);
      VECTOR_Add_Element (*bundle_vector, op);
    }

    // Check to see if the <next_op> has a dependence with the OPs in
    // the current <bundle>. If yes, it's always better to set the stop-bit
    // apriori and create a new instr. group.

    if (bundle_full && OP_next(op)) {
      OP *next_op = OP_next(op);
      if (Is_There_Group_Dependence(next_op, bundle_vector)) {
	Set_OP_end_group(op);
        HB_Hazard_current_cycle++;
        OP* next_op = NULL;
        if (op) next_op = OP_next(op);
        if (op && next_op && OP_scycle(op)>=0 && OP_scycle(next_op)>=0) {
	  int cycles = OP_scycle(next_op) - OP_scycle(op);
	  if (cycles>0)
            HB_Hazard_current_cycle += (cycles-1);
        }
	VECTOR_Reset (*bundle_vector);
      }
    }

    // if black_box_op, set the end_group market and quit now.
    if (!bundling_reqd) {
      Set_OP_end_group(op);
      return;
    }

    // Reattempt packing the <op> after clearing the bundle.
    if (!slot_avail) {
      FOR_ALL_SLOT_MEMBERS (bundle, i) {
	stop_bit_reqd = (i > 0) && OP_f_group(op);
	if (CGTARG_Bundle_Slot_Available (bundle, op, i, &prop,
					  stop_bit_reqd, NULL)) {
	  
	  slot_pos = i;
	  if (stop_bit_reqd) {
	    if (i > 0) stop_pos = i - 1;
	    else stop_pos = -1;
	  }
	  // If there is a need to delay the scheduling of <op>...
	  if (!Delay_Scheduling_OP(op, i, bundle)) break;
	}
      }

      FmtAssert(slot_pos != -1, ("Slot Position not a legal value"));
      Set_OP_bundled (op);
      TI_BUNDLE_Reserve_Slot (bundle, slot_pos, prop);
      if (stop_pos >= 0) TI_BUNDLE_Reserve_Stop_Bit (bundle, stop_pos);
      TI_RES_RES_Reserve_Resources(HB_Hazard_resource_table, OP_code(op), 
				   HB_Hazard_current_cycle, alt_topcode);
      VECTOR_Add_Element (*bundle_vector, op);

      // Do extra stuff.
      if ((BB_last_real_op((OP_bb(op))) == op) || (slot_pos != 0)) {
	INT max_pos = (BB_last_real_op((OP_bb(op))) == op) ?
	  ISA_BUNDLE_MAX_SLOTS : slot_pos;
	CGTARG_Handle_Bundle_Hazard (op, bundle, bundle_vector, TRUE, 
				     slot_pos, max_pos, stop_bit_reqd, prop);
	if (stop_pos >= -1) {
	  OP *last_real_op = Last_Real_OP(op);
	  Set_OP_end_group(last_real_op);
	  HB_Hazard_current_cycle++;
          if (OP_scycle(last_real_op)>=0 && OP_scycle(op)>=0) {
	    int cycles = OP_scycle(op) - OP_scycle(last_real_op);
	    if (cycles>0)
	      HB_Hazard_current_cycle += (cycles-1);
	  }
	  VECTOR_Reset (*bundle_vector);
	  TI_RES_RES_Reserve_Resources(HB_Hazard_resource_table, OP_code(op), 
				       HB_Hazard_current_cycle, alt_topcode);
	  VECTOR_Add_Element (*bundle_vector, op);
	}

	if (TI_BUNDLE_Is_Full(bundle, &ti_err)) {
	  TI_BUNDLE_Clear(bundle);
	}
      }
    }
  }
}

// ======================================================================
// Handle the case of possible additive hazards (when a post-hazard is
// added to a pre-hazard to come up with a combined hazard). An example
// of additive hazards is a ctc1,bc1t sequence for mips1-3.
// ======================================================================
static void
Check_For_Other_Hazards(OP *op)
{
  
  TOP opcode = OP_code(op);
  INT numops;
  INT opnd;
  INT result;
  INT errata_num;
  INT ti_err;

  // Check for operand hazards.
  numops = TI_LATENCY_Operand_Hazard (opcode, &opnd, &ti_err);
  FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
  if (numops < 0) {
    // Operand pre-hazard.
    // Check if there are any post-hazards in the queue that can add up
    // with this pre-hazard. If yes, add the two hazards and see if we 
    // need to insert any nops.
    Handle_Additive_Hazards (op, opnd, -numops);
    Handle_Pre_Hazard (op, opnd, -numops);
  } else if (numops > 0) {
    // Operand post-hazard.
    Handle_Post_Hazard (op, opnd, numops);
    Add_Post_Hazard_To_Q (op, opnd, numops);
  }

  // Check for result hazards.
  numops = TI_LATENCY_Result_Hazard (opcode, &result, &ti_err);
  FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
  if (numops != 0) {
    // Result post-hazard.
    FmtAssert (numops > 0, ("Handle_All_Hazards: can't handle result pre-hazard"));
    Is_True(result == 0, ("can't handle hazard for result number %d", result));
    Handle_Post_Hazard (op, OPND_RESULT, numops);
    Add_Post_Hazard_To_Q (op, OPND_RESULT, numops);
  }

  if (TI_PROC_Property_Set(PROP_has_branch_delay_slot)) {
    // Check for delay slot hazards.
    if (OP_xfer(op)) {
      Handle_Post_Hazard (op, OPND_NONE, 1);
      Add_Post_Hazard_To_Q (op, OPND_NONE, 1);
    }
  }

  // Check for errata hazards.
  numops = TI_LATENCY_Errata_Hazard (opcode, &errata_num, &ti_err);
  FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
  if (numops != 0) {
    CGTARG_Handle_Errata_Hazard (op, errata_num, numops);
  }
}

// ======================================================================
// Handle the case when an instruction with hazard is placed in a delay
// slot.
// ======================================================================
void
Check_For_Delay_Slot_Hazards (BB *bb)
{

  OP *last_op = BB_last_op(bb);

  // If there is an instruction with hazards in the delay slot, move it.
  if (Is_Delay_Slot_Op (last_op, bb) && OP_has_hazard(last_op)) {
    BB_Move_Delay_Slot_Op (bb);
  }

  // R10k chip workaround: Avoid placing integer mult/div ops in delay
  // slots of unconditional branches. (see pv516598) for more details.
  if (Is_Delay_Slot_Op (last_op, bb) && 
      (OP_imul(last_op) || OP_idiv(last_op))) {
    OP *xfer_op = OP_prev(last_op);
    if (xfer_op && OP_uncond(last_op)) {
      BB_Move_Delay_Slot_Op (bb);
      BB_Insert_Noops(xfer_op, 1, FALSE); 
    }
  }
}

static void 
Init_Resource_Table(BB *bb)
{
  HB_Hazard_resource_table = TI_RES_RES_Alloc(FALSE, &MEM_local_pool);
  OP *op;
  INT max_cycles = 0;
  FOR_ALL_BB_OPs_FWD(bb, op) {
    INT cur_resource_cycles = TI_RES_Cycle_Count(OP_code(op));
    if (cur_resource_cycles > max_cycles) {
      max_cycles = cur_resource_cycles;
    }
    INT cur_last_def_cycles = TI_TOP_Latest_Def_Stage(OP_code(op));
    if (cur_last_def_cycles > max_cycles) {
      max_cycles = cur_last_def_cycles;
    }
  }
  TI_RES_RES_Set_BB_Cycle_Count(HB_Hazard_resource_table, 
				BB_length(bb)*max_cycles);
  HB_Hazard_current_cycle = 0;
}

/*
   Has_def_use() returns TRUE if each of all OPs from bundle_ops[start] 
   to bundle_ops[end], except 'op', has use-def relation to the 'op' (ie,
   a reg that is defined by 'op' is used by the other OPs); return FALSE
   if no def-use relation.
*/
static BOOL Has_def_use (OP *op, OP **bundle_ops, int start, int end)
{
  BOOL flow_dep = FALSE;
  for (int i=start; i < end; i++) {
    OP *op1 = bundle_ops[i];
    if (op == op1) continue;
    for (int j=0; j < OP_results(op); j++) {
      TN *tn = OP_result(op,j);
      if (TN_is_register(tn) && 
          OP_Refs_Reg(op1, TN_register_class(tn), TN_register(tn))) {
        return TRUE;
      }
    }
  }
  return FALSE;
}

static TOP get_special_op(OP *op)
{
  TOP tops[256];
  TOP ret_top = TOP_UNDEFINED;

  if (op==NULL || OP_generic(op)==false)
    return ret_top;

  TOP generic_top = OP_code(op);
  int num_tops = TI_TOP_Get_Special_Tops(generic_top, tops, 256);

  for (int i=0; i < num_tops; i++) {
    TOP specialized_top = tops[i];
    if (TI_ISA_Exec_Unit_Prop(specialized_top) &
        ISA_EXEC_PROPERTY_Fetch_Unit) {     // format 0's property
      ret_top =  specialized_top;
      break;
    }
  }
  return ret_top;
}

static TOP Convert_TO_Density_OP (OP *op)
{
  TOP new_opcode = TOP_UNDEFINED;
  TOP top = TOP_UNDEFINED;
  if (OP_generic(op)) {
    top = get_special_op(op);
  }
  if (top == TOP_UNDEFINED) {
    top = OP_code(op);
  }

  INT64 val;
  if (xt_density) {

    switch (top)
    {
    case TOP_add:
      new_opcode = TOP_add_n;
      break;

    case TOP_addi:
      if ( Imm_Or_Stack_Sym_Offset(OP_opnd(op, 1), &val) &&
           TI_ISA_LC_Value_In_Class(val, LC_ai4const))
        new_opcode = TOP_addi_n;
      break;

    case TOP_l32i:
      if (Imm_Or_Stack_Sym_Offset(OP_opnd(op, 1), &val) &&
          TI_ISA_LC_Value_In_Class(val, LC_lsi4x4))
        new_opcode = TOP_l32i_n;
      break;

    case TOP_s32i:
      if ( Imm_Or_Stack_Sym_Offset(OP_opnd(op, 2), &val) &&
          TI_ISA_LC_Value_In_Class(val, LC_lsi4x4))
        new_opcode = TOP_s32i_n;
      break;

    case TOP_or:
      /* Is this TOP_or acting as a move... */
      if (OP_opnd(op, 0) != OP_opnd(op, 1))
        break;

      if (OP_result(op, 0) != OP_opnd(op, 0))
        new_opcode = TOP_mov_n;
      break;

    case TOP_movi:
      Is_True(TN_has_value(OP_opnd(op, 0)), ("expecting literal tn"));
      if (TI_ISA_LC_Value_In_Class(TN_value(OP_opnd(op, 0)), LC_simm7))
        new_opcode = TOP_movi_n;
      break;
    case TOP_ret:
      new_opcode = TOP_ret_n;
      break;

    case TOP_retw:
      new_opcode = TOP_retw_n;
      break;
    }
  }

  return new_opcode;
}

#define BUNDLE_ONLY_FULL_FLIX 0
#define       BUNDLE_FOR_SPACE_OPT    1


#define MAX_SINGLE_SLOT_FORMAT 5
static int format_single_slot[MAX_SINGLE_SLOT_FORMAT];
static int nformat_single_slot = -1;  // -1: not set up yet

static void Bundle_breaker (BB *bb, int reason)
{
  if (!TI_PROC_Property_Set(PROP_has_bundles)) {
    return;
  }

  if (nformat_single_slot == -1) {
    nformat_single_slot = 1;
    format_single_slot[0] = 0;  // format 0
    // setup all single-slot formats
    for (int f=1; f < TI_ISA_Num_Bundles(); f++) {
      if (TI_ISA_Num_Slots(f) == 1) {
        if (nformat_single_slot >= MAX_SINGLE_SLOT_FORMAT) {
          // If we don't record all single-slot formats, we
          // may end up with some flix that should be un-flixed.
          // However, this does not affect the correctness.
          break;
        }
        format_single_slot[nformat_single_slot++] = f;
      }
    }
  }

  OP *bundle_ops[ISA_BUNDLE_MAX_SLOTS];
  int bundle_ops_format[ISA_BUNDLE_MAX_SLOTS];
  OP *op;
  OP *next;   // points to the next OP to be processed
  OP *prev;   // points to the last OP right before the current bundle.
              // i.e.  prev->next is the first OP within the current bundle.
  int n=0;
  prev = NULL;
  for (op = BB_first_op(bb); op; op = next) {
    next = OP_next(op);

    // Ignore dummy and pseudo OPs and only handle bundled OPs
    if (!OP_dummy(op) && !OP_pseudo(op) && OP_bundled(op)) {
      /* Collect all OPs within the current bundle.
         Ignore nop (nop will be deleted if flix is removed).
       */
      if (OP_code(op) != TOP_nop) {
        bundle_ops[n] = op;
        n++;
      } 
      if (!OP_end_group(op)) {
        continue;
      }

      // Now, op is the end of the current bundle, which has n real OPs.
      int format_num = OP_format_num(op);

      if (reason == BUNDLE_ONLY_FULL_FLIX) {
        // Now, op is the end of the current bundle, which has n real OPs.
        if (TI_ISA_Num_Slots(format_num) == n) {
          // fully packed flix or non-flix OPs, skip this bundle
          // and start processing the next one.
          prev = op;
          n = 0;
          continue;
        }
      } else if (reason == BUNDLE_FOR_SPACE_OPT) {
        int bsizes = 0;
        if ( (format_num == 0) || (TI_ISA_Num_Slots(format_num) == 1) ) {
          prev = op;
          n = 0;
          continue;
        } else {
          for (int i=0; i < n; i++) {
            TOP top = Convert_TO_Density_OP(bundle_ops[i]);
            if (top != TOP_UNDEFINED) {
              bsizes += TI_ISA_Inst_Bytes(top);
            } else {
              bsizes += OP_Real_Inst_Bytes(bundle_ops[i]);
            }
          }
          if (bsizes >= TI_ISA_Bundle_Bytes(format_num)) {
            prev = op;
            n = 0;
            continue;
          }
        }
      } else {
        FmtAssert(FALSE, ("Should not reach this point"));
      }

        
      /* The current bundle is not fully packed. Check if any OP in this bundle
         can use a single-slot format
       */
      BOOL break_flix = TRUE;
      for (int i=0; i < n; i++) {
        ISA_EXEC_UNIT_PROPERTY op_props = 
            TI_ISA_Exec_Unit_Prop(OP_code(bundle_ops[i]));
        BOOL op_has_one = FALSE;
        for (int j=0; j < nformat_single_slot; j++) {
          if (op_props & TI_ISA_Exec_Slot_Prop(format_single_slot[j], 0) != 0) {
            bundle_ops_format[i] = format_single_slot[j];
            op_has_one = TRUE;
            break;
          } 
        }
        if (!op_has_one) {
          break_flix = FALSE;
          break;
        }
      }

      if (break_flix) {
        // xfer OP, if any,  must be the last one.
        int pos = -1;
        for (int i=0; i < n; i++) {
          if (OP_xfer(bundle_ops[i])) {
            FmtAssert ((pos == -1), ("no two branches in a single flix"));
            pos = i;
          }
        }

        int new_n = n;
        if ((pos >= 0) && (pos != (n-1))) {
          OP *xfer_op = bundle_ops[pos];
          bundle_ops[pos] = bundle_ops[n-1];
          bundle_ops[n-1] = xfer_op;

          int j = bundle_ops_format[pos];
          bundle_ops_format[pos] = bundle_ops_format[n-1];
          bundle_ops_format[n-1] = j;

          new_n = n-1;
        }

        /* Go over all OPs from bundle_ops[0 : new_n-1], sort these OPs so that
           no def-use relation among any OP and its following OPs (i.e. no
           def-use from bundle_ops[i] to bundle_ops[i+1 : new_n-1]).

           Here we use a simple bubble sorting algorithm: first, pick up an OP that
           does not have def-use relation on all the other OPs; second, among
           the rest of OPs (excluding OPs that have been picked already), repeat
           the first until all OPs have been picked.

           Because of no dependence cycle, this sorting method should succeed.
         */
        for (pos = 0; pos < new_n; pos++) {
          BOOL found = FALSE;
          OP *op1, *op2;
          for (int i=pos; i < new_n; i++) {
            op1 = bundle_ops[i];
            if (Has_def_use(op1, bundle_ops, pos, new_n)) {
              continue;
            } else {
              if (pos != i) {
                bundle_ops[i]  = bundle_ops[pos];
                bundle_ops[pos] = op1;

                int j = bundle_ops_format[i];
                bundle_ops_format[i]  = bundle_ops_format[pos];
                bundle_ops_format[pos] = j;
              }
              found = TRUE;
              break;
            }
          }
          Is_True(found, ("dependence cycle detected within a bundle, wrong!"));
        }
          
        // Now, reset bundle and relink the OPs within BB
        for (int i=0; i < n; i++) {
          OP *tmp_op = bundle_ops[i];

          tmp_op->prev = prev;
          if (prev) {
            prev->next = tmp_op;
          }
          prev = tmp_op;

          Set_OP_end_group(tmp_op);
          Set_OP_format_num(tmp_op, bundle_ops_format[i]);
          Set_OP_slot_num(tmp_op, 0);
        }
        if (prev) {
          prev->next = next;
        }
        if (next) {
          next->prev = prev;
        }
      } else {
        prev = op;
      }
      n = 0;
    }
  }
}

/*
   Bundle_Only_Full_Flix() is a post-bundling pass that only uses
   fully-packed flix or none. It breaks a partially-packed flix
   into a single non-flix instruction.  Clearly, if any OP in a
   partially-packed flix can only appear in a flix, the partial
   flix remains unchanged.
*/
void Bundle_Only_Full_Flix (BB *bb)
{
  FmtAssert((xt_flix && xt_only_full_flix),
            ("flix not enabled or xt_only_full_flix isn't set"));

  Bundle_breaker(bb, BUNDLE_ONLY_FULL_FLIX);
}

/*
  Bundle_For_Space() is a post-bundling pass that uses flix
  only if using flix does not increase the code size. But if
  an OP in a flix can only appears in a flix, the flix remains
*/
void Bundle_For_Space (BB *bb)
{
  Bundle_breaker(bb, BUNDLE_FOR_SPACE_OPT);
}



// ======================================================================
// Eliminate hazards for 'bb' by adding noops.
// ======================================================================
void
Handle_All_Hazards (BB *bb)
{
  OP *op;
  OP *last_op = BB_last_op(bb);

  if (last_op == NULL) return;

  // Check for any delay slot hazards.
  if (TI_PROC_Property_Set(PROP_has_branch_delay_slot))
    Check_For_Delay_Slot_Hazards(bb);

  memset(post_Q, 0, sizeof(post_Q));

  VECTOR bundle_vector = VECTOR_Init (TI_TIE_SLOTS_MAX, &MEM_local_pool);
  TI_BUNDLE *bundle = TYPE_MEM_POOL_ALLOC (TI_BUNDLE, &MEM_local_pool);
  memset(bundle, 0, sizeof(bundle));
  bundle->bundle_info = TYPE_MEM_POOL_ALLOC (ISA_BUNDLE_INFO, &MEM_local_pool);
  TI_BUNDLE_Clear(bundle);
  Init_Resource_Table(bb);

  FOR_ALL_BB_OPs_FWD (bb, op) {

    // Check for bundle hazards.
    if (TI_PROC_Property_Set(PROP_has_bundles) && LOCS_Enable_Bundle_Formation) {

      // Avoid processing dummy and already bundled OPs.
      if (!OP_dummy(op) && !OP_bundled(op) && !OP_pseudo(op)) 
	Check_For_Bundle_Hazards (op, bundle, &bundle_vector);
    }

    // Check for other hazards.
    Check_For_Other_Hazards(op);

  }
  // Check for any extra hazards.
  Insert_Stop_Bits(bb);
}
