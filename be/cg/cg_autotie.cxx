
/*

  Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.

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

*/

/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_autotie
 *
 *  Description:
 *  ============
 *
 *  See header.
 *
 * =======================================================================
 * ======================================================================= */

#include <stdio.h>
#include <stdlib.h>

#include "at.h"
#include "at_xcc.h"
#include "at_op_xcc.h"
#include "cg_autotie.h"
#include "cg_fusion.h"
#include "cg_spill.h"
#include "cgexp.h"
#include "config_targ_options.h"
#include "config_lno.h"
#include "cxx_hash.h"
#include "data_layout.h"
#include "dwarf_DST.h"
#include "freq.h"
#include "ir_reader.h"
#include "pu_info.h"
#include "srcpos.h"
#include "tracing.h"
#include "whirl2ops.h"
#include "dep_graph.h" /* for LNO dep graph */

extern REGISTER_SET GRA_Local_Register_Grant( BB* bb, ISA_REGCLASS rc );

#ifdef TARG_XTENSA

#define CG_AUTOTIE_ERROR_PHASE "XPRES Analysis"
#define CG_AUTOTIE_DFG_ERROR_PHASE "XPRES DFG Analysis"
#define CG_AUTOTIE_EXEC_ERROR_PHASE "XPRES Execution Count Analysis"
#define CG_AUTOTIE_SPILL_ERROR_PHASE "XPRES Spill Analysis"
#define CG_AUTOTIE_TAKEN_PROB_ERROR_PHASE "XPRES Taken Probabilities Analysis"


//
// Statics
//

static AT_PU *at_pu;
static AT_OP_TAB *op_tab;
static AT_FACTORY *at_factory;

typedef HASH_TABLE<AT_MESSAGE *, bool> MESSAGE_MAP;
typedef HASH_TABLE_ITER<AT_MESSAGE *, bool> MESSAGE_MAP_ITER;
MESSAGE_MAP *remove_pu_messages;

/* Map from BB to the autotie DFG corresponding to that BB. */
static BB_MAP bb_dfg_map;
static tf_dfg_t BB_get_autotie_dfg (BB *bb);
static void BB_set_autotie_dfg (BB *bb, tf_dfg_t dfg);

/* Map from BB to the AT_STAT corresponding to that BB. */
static BB_MAP bb_stat_map;
static AT_STAT *BB_get_at_stat (BB *bb);
static void BB_set_at_stat (BB *bb, AT_STAT *stat);


//
// Enter and exit autotie error phase
//

static const char *
enter_autotie_phase (const char *autotie_phase)
{
  const char *phase = Get_Error_Phase();
  Set_Error_Phase(autotie_phase);
  return phase;
}

static void
exit_autotie_phase (const char *phase)
{
  Set_Error_Phase(phase);
}


static void
BB_set_autotie_dfg (BB *bb, tf_dfg_t dfg)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  BB_MAP_Set(bb_dfg_map, bb, (void *)dfg);
}


static tf_dfg_t 
BB_get_autotie_dfg (BB *bb)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  return (tf_dfg_t )BB_MAP_Get(bb_dfg_map, bb);
}


static void
BB_set_at_stat (BB *bb, AT_STAT *stat)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  BB_MAP_Set(bb_stat_map, bb, (void *)stat);
}


static AT_STAT *
BB_get_at_stat (BB *bb)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  return (AT_STAT *)BB_MAP_Get(bb_stat_map, bb);
}


/* Return the AT_OP_ID that represents 'op'. */
static AT_OP_ID
Find_At_Op_Id (AT_OP_TAB *op_tab, OP *op)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  Is_True(op, ("Null op"));

  TOP top = OP_code(op);

  /* If no density, use 'or' instead of 'mov'. */
  if (top == TOP_mov_n && !xt_density)
    top = TOP_or;

  /* Find the AT_TIE_OP that represents 'op'. */
  AT_TIE_OP *at_tie_op = AT_Tie_Info()->find_op(TI_TOP_Name(top));
  if (!at_tie_op)
    return AT_OP_ID_UNKNOWN;

  /* Always request the wide version of 'op'. Autotie doesn't expect
     narrow versions to be requested. */
  if ((top != TOP_mov_n) && (at_tie_op->wide_version() != NULL))
    at_tie_op = at_tie_op->wide_version();

  AT_OP *at_op = XT_New(AT_Libauto_Pool()) AT_OP(AT_Libauto_Pool(), op_tab, AT_OP_KIND_TIE_OP);
  at_op->Set_Tie_Op(at_tie_op);
    
  /* Add an immediate parameter for each immediate operand. */
  for (INT opnd_idx = 0; opnd_idx < OP_fixed_opnds(op); opnd_idx++)
  {
    if (OP_opnd_is_literal(op, opnd_idx) && !OP_opnd_is_pcrel(op, opnd_idx))
    {
      TN *tn = OP_opnd(op, opnd_idx);
      FmtAssert(TN_is_constant(tn), ("expecting literal TN to have constant value"));
	    
      /* We recognize immediates that are explicit, or that are
	 offsets off of SP or FP. */
      INT64 tn_val;
      if (TN_has_value(tn))
      {
	tn_val = TN_value(tn);
      }
      else if (TN_is_symbol(tn))
      {
	ST *st = TN_var(tn);
	ST *base_st;
	INT64 base_ofst;
		
	Base_Symbol_And_Offset(st, &base_st, &base_ofst);
	if (base_st == SP_Sym || base_st == FP_Sym)
	{
	  tn_val = TN_value(tn) + base_ofst;
	}
	else
	{
	  continue;
	}
      }
      else
      {
	continue;
      }
	    
      AT_TY_ID ty_id = at_factory->get_at_ty_id(MTYPE_I4, 1, AT_TY::NONE);
      AT_PARAM *param = XT_New(AT_Libauto_Pool()) AT_PARAM(at_factory->ty_tab(),
							 AT_PARAM_KIND_IMM, ty_id);
      XT_BIGNUM *val = XT_New(AT_Libauto_Pool()) XT_BIGNUM(32, tn_val);
      param->Set_Imm_Value(val);
      at_op->Add_Param(param);
	    
      /* Change 'at_op's kind to a specializable TIE_OP. */
      at_op->Set_Kind(AT_OP_KIND_SPEC_TIE_OP);
    }
  }
    
  AT_OP *old_at_op = op_tab->Find_Op(at_op);
  if (old_at_op)
  {
    XT_MEM_Destroy(at_op, AT_Libauto_Pool());
    return old_at_op->Id();
  }
  
  return op_tab->Add_Op(at_op, /* copy */ false)->Id();
}


static const char *
get_filename_for_srcpos (SRCPOS srcpos)
{
  const char *fname, *dirname;
  IR_Srcpos_Filename(srcpos, &fname, &dirname);
  return((!fname) ? NULL :
         (!dirname) ? fname :
         xt_pool_printf(AT_Libauto_Pool(), "%s/%s", dirname, fname));
}


/* Return the AT_TY for 'tn' or NULL */
static AT_TY *
Find_At_Ty (TN *tn)
{
  Is_True(Run_Autotie, ("expecting only during autotie"));
  Is_True(tn, ("Null TN"));

  AT_TY_ID at_ty_id = AT_TY_ID_UNKNOWN;
    
  TYPE_ID mtype = TN_mtype(tn);
  if (mtype != MTYPE_UNKNOWN) {
    at_ty_id = at_factory->get_at_ty_id(mtype, 1, AT_TY::NONE);
  } else {
    if (!TN_is_register(tn))
      return NULL;
	
    // Check for special or state registers.
    ISA_REGSUBCLASS subclass = ISA_REGSUBCLASS_UNDEFINED;
    if (TN_register_class(tn) == TI_ISA_Regclass_Special()) {
      subclass = TI_ISA_Regsubclass_Special_Reg_Num(TN_register(tn) - REGISTER_MIN);
    } else if (TI_ISA_Regclass_Is_State(TN_register_class(tn))) {
      subclass = TI_ISA_Regsubclass_State(REGISTER_name(TN_register_class(tn),
							TN_register(tn)));
    }
	
    if (subclass == ISA_REGSUBCLASS_UNDEFINED)
      return NULL;
	
    const char *state_name = TI_ISA_Regsubclass_Name(TI_ISA_Regsubclass_Info(subclass));
    AT_TIE_STATE *tie_state = AT_Tie_Info()->find_state(state_name);
    if (!tie_state)
      return NULL;
	
    at_ty_id = at_factory->get_at_ty_id(tie_state, 1, AT_TY::NONE);
  }
    
  if (at_ty_id == AT_TY_ID_UNKNOWN)
    return NULL;
    
  return at_factory->ty_tab()->Get_Type(at_ty_id);
}


static int
find_parent_region_id (LOOP_DESCR *loop)
{
  for (LOOP_DESCR *enclosing = loop;
       enclosing;
       enclosing = LOOP_DESCR_Next_Enclosing_Loop(enclosing))
  {
    LOOPINFO *li = LOOP_DESCR_loopinfo(enclosing);
    if (li)
    {
      WN *loop_info = LOOPINFO_wn(li);
      if (loop_info && (WN_loop_at_region_id(loop_info) != AT_REGION_ID_UNKNOWN))
      {
	return WN_loop_at_region_id(loop_info);
      }
    }
  }
  
  return AT_REGION_ID_UNKNOWN;
}



static void
Autotie_Update_Exec_Count(AT_REGION *region, double bb_exec_cnt, AT_EXEC_COUNT_KIND bb_exec_kind)
{
#ifdef TARG_XTENSA
  /* If 'region' doesn't have any exec count or if it has an estimated
     count and 'bb_exec_cnt' is a real count, just set 'region's count
     to 'bb's. Otherwise, if the exec count kinds match, then max
     'region's count against 'bb'. */
  if ((region->Exec_Kind() == AT_EXEC_CNT_UNKNOWN) ||
      ((bb_exec_kind == AT_EXEC_CNT_PROFILE) &&
       (region->Exec_Kind() != AT_EXEC_CNT_PROFILE)))
    {
      region->Set_Exec_Kind(bb_exec_kind);
      region->Set_Exec_Count(bb_exec_cnt);
    }
  else if (region->Exec_Kind() == bb_exec_kind)
    {
      region->Set_Exec_Count(MAX(region->Exec_Count(), bb_exec_cnt));
    }
#endif
}


static tf_dfg_t
Convert_DFG_Operands(tf_dfg_t dfg) {

  if (!dfg)
    return NULL;

  /* We need to set the user data of all operands to the corresponding AT_TYs. */
  for (tf_node_t node = tfex->dfg_first_node(dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {

    for (INT dir = 0; dir < 2; dir++) {
      tf_opnd_vec_t opnds =
	(dir == 0) ? tfex->node_inputs(node) : tfex->node_outputs(node);
      
      INT size = tfex->opnd_vec_size(opnds);
      for (INT oidx = 0; oidx < size; oidx++) {
	tf_opnd_t opnd = tfex->opnd_vec_get(opnds, oidx);
	TN *tn = (TN *)tfex->opnd_user(opnd);
	if (tn) {
	  AT_TY *at_ty = Find_At_Ty(tn);
	  if (at_ty == NULL) {
	    return NULL;
	  }
	  tfex->opnd_set_user(opnd, at_ty);
	}
      }
    }
  }
    
  return dfg;
}

static tf_dfg_t 
Autotie_Build_DFG (XT_MEMPOOL *pool, BB *bb)
{
  tf_dfg_t dfg = CG_FUSION_Build_DFG(pool, bb);

  if (!dfg)
    return NULL;

  /* We need to set the user data of all nodes to the corresponding AT_OPs */
  for (tf_node_t node = tfex->dfg_first_node(dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    OP *op = (OP *)tfex->node_user(node);
    if (op) {
      TOP top = OP_code(op);
      AT_OP_ID at_op_id = Find_At_Op_Id(at_factory->op_tab(), op);
      Is_True(at_op_id != AT_OP_ID_UNKNOWN, 
	      ("Unable to create AT_OP for OP %s", TI_TOP_Name(top)));
      AT_OP *at_op = at_factory->op_tab()->Get_Op(at_op_id);
      tfex->node_set_user(node, at_op);
    }
  }

  // we will convert the TNs in dfg to AT_TY
  // Convert_DFG_Operands(dfg) after register allocation
  // and Autotie_Analyze_Spills ()

  return dfg;
}


static void
Autotie_Remove_Copied_PU_Messages (void)
{
  if (at_pu->Messages()->Empty())
    return;

  AT_MESSAGE *message;
  bool remove;
  MESSAGE_MAP_ITER iter(remove_pu_messages);
  while (iter.Step(&message, &remove)) {
    at_pu->Messages()->Remove(message);
  }
  
  remove_pu_messages->Clear();
}


/* If a PU-level message refers to 'srcpos', copy the message into the
   region-level messages. */
static void
Autotie_Copy_Srcpos_PU_Messages_To_Region (SRCPOS srcpos, AT_REGION *region)
{
  if (at_pu->Messages()->Empty() || srcpos == 0)
    return;
  
  AT_REGION *to_region = region;
  if (region->Parent_Id() != AT_REGION_ID_UNKNOWN)
    to_region = at_pu->Get_Region(region->Parent_Id());
  
  AT_MESSAGE *lend_message = to_region->Messages()->Find_Loop_End();
  
  INT filenum = SRCPOS_filenum(srcpos);
  INT linenum = SRCPOS_linenum(srcpos);
  
  /* Check if there're any messages with matching source position.
     If any, copy them to the region-level message list. */
  for (AT_MESSAGES::iter iter(at_pu->Messages()->Message_List());
       iter.hasCurrent(); iter.next()) {
    
    AT_MESSAGE *msg = iter.current();
    
    if (msg->File_Number() == filenum &&
        msg->Line_Number() == linenum &&
        !to_region->Messages()->Find_Equal(msg)) {
      
      if (lend_message) {
	if (lend_message->Id()==AT_MSG_SIMD_LOOP_VECTORIZABLE &&
	    msg->Id()==AT_MSG_SIMD_ARRAY_ALIAS) {
	  /* suppress array alias message for vectorizable loops
	  * see PR11562
	  */
	} else
	  to_region->Messages()->Add_Before(lend_message, msg);
      } else {
        to_region->Messages()->Append(msg);
      }
      
      /* Mark the message for deletion. We don't delete the message
         right away because we want it to appear in all regions
         that contain ops with matching source position. */
      remove_pu_messages->Enter_If_Unique(msg, true);
    }
  }
}


/* If a PU-level message refers to a source position contained within
   'bb', copy the message into the region-level messages. */
static void
Autotie_Copy_PU_Messages_To_Region (BB *bb, AT_REGION *region)
{
  if (at_pu->Messages()->Empty())
    return;
  
  SRCPOS prev_srcpos = 0;
  OP *op;
  FOR_ALL_BB_OPs(bb, op)
    {
      SRCPOS srcpos = OP_srcpos(op);
      
      /* A lot of adjacent ops have the same source position, so
         avoid some message traversal. */
      if ((prev_srcpos == srcpos) || (srcpos == 0))
        continue;
      
      prev_srcpos = srcpos;
      Autotie_Copy_Srcpos_PU_Messages_To_Region(srcpos, region);
    }
}


static AT_REGION *
Autotie_Build_Block_Region (BB *const bb, BB_MAP bb_exec_cnts,
                            UINT32 at_parent_id, bool inner_loop)
{
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid AT_PU"));

  /* Create an AT_REGION block for 'bb' and place a scalar AT_STAT in
     the region. If 'at_parent' is non-NULL, show it is the loop
     region holding this block. */
  char *name = XT_New(AT_Libauto_Pool()) char [strlen(at_pu->Name()) + strlen("_B") + 64];
  sprintf(name, "%s_B%d", at_pu->Name(), at_pu->Region_Count());
	
  AT_REGION *at_region =
    XT_New(AT_Libauto_Pool()) AT_REGION(inner_loop ? AT_REGION_KIND_BLOCK_INNER :
                                        AT_REGION_KIND_BLOCK, name);
  SRCPOS srcpos = BB_Loop_Srcpos(bb);
  UINT lineno = SRCPOS_linenum(srcpos);
  UINT filenum = SRCPOS_filenum(srcpos);

  at_region->Set_Linenum(lineno);

  const char *fname = get_filename_for_srcpos(srcpos);
  if (fname != NULL)
    at_region->Set_Filename(fname);

  at_region->Set_CG_Region(true);

  char *aux_info = NULL;
  if (BB_has_label(bb)) {
    ANNOTATION *ant;
    ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
    LABEL_IDX label = ANNOT_label(ant);
    aux_info = xt_pool_printf(AT_Libauto_Pool(), "BB:%d(%s)", BB_id(bb), LABEL_name(label));
  } else {
    aux_info = xt_pool_printf(AT_Libauto_Pool(), "BB:%d(no_label)", BB_id(bb));
  }
  
  at_region->Set_Aux_Info(aux_info);

  at_pu->Add_Region(at_region);
  if (at_parent_id != AT_REGION_ID_UNKNOWN)
    at_region->Set_Parent_Id(at_parent_id);

  /* Set the execution count for 'at_region' to be the execution
     count of the block it represents. */
  double bb_exec_cnt = BB_MAPFP_Get(bb_exec_cnts, bb);
  AT_EXEC_COUNT_KIND bb_exec_kind = ((BB_freq_fb_based(bb)) ?
                                     AT_EXEC_CNT_PROFILE :
                                     AT_EXEC_CNT_EST);

  at_region->Set_Exec_Count(bb_exec_cnt);
  at_region->Set_Exec_Kind(bb_exec_kind);

  /* Create the AT_STAT for 'bb'... */
  AT_STAT *at_stat = XT_New(AT_Libauto_Pool()) AT_STAT(1);
  at_region->Add_Stat(at_stat);
  at_stat->Set_Unroll(MAX(BB_unrollings(bb), 1));
  at_stat->Set_SWP(BB_rotating_kernel(bb)!=0);
  OP* br_op = BB_branch_op(bb);
  if (br_op && OP_code(br_op)==TOP_loop_end)
    at_stat->Set_ZCL(true);
      
  /* Associate 'bb' with its AT_STAT. */
  BB_set_at_stat(bb, at_stat);
      
  /* If we find a data-flow graph for 'bb', attach it... */
  tf_dfg_t dfg = BB_get_autotie_dfg(bb);
  if (dfg)
    at_stat->Set_DFG(dfg);
      
  /* Add all 'bb' OPs to 'at_stat'... */
  OP *op;
  FOR_ALL_BB_OPs(bb, op)
    {
      if (OP_noop(op))
        continue;

      /* If 'op' is simulated, then we want to add the actual
         operation(s) it will expand into. Ignore asms since we can't
         understand them... */
      if (OP_simulated(op))
        {
          OPS *ops = OPS_Create();
          if ((OP_code(op) != TOP_asm) && (OP_code(op) != TOP_spadjust))
	    {
	      if (!Exp_Br_Reg_Simulated_Op(op, ops))
		Exp_Simulated_Op(op, ops, 0);
	    }
          
          OP *sop;
          FOR_ALL_OPS_OPs(ops, sop)
            {
              if (OP_noop(sop))
                continue;
              
              AT_OP_ID at_op_id = Find_At_Op_Id(op_tab, sop);
              if (at_op_id == AT_OP_ID_UNKNOWN)
                DevWarn("unable to create AT_OP for simulated OP %s (%s) in BB %d of %s\n",
                        TI_TOP_Name(OP_code(op)), TI_TOP_Name(OP_code(sop)),
                        BB_id(bb), at_pu->Name());
              else
                at_stat->Inc_Loop_Op_Count(at_op_id);
            }
        }
      else
        {
          AT_OP_ID at_op_id = Find_At_Op_Id(op_tab, op);
          if (at_op_id == AT_OP_ID_UNKNOWN)
            DevWarn("unable to create AT_OP for OP %s in BB %d of %s\n",
                    TI_TOP_Name(OP_code(op)), BB_id(bb), at_pu->Name());
          else
            at_stat->Inc_Loop_Op_Count(at_op_id);
        }
      
      /* If an LNO dependence graph vertex is available for at least one
         operation in the BB, let XPRES know the LNO dependence info is
         available for this region. */
      if (!at_region->LNO_Dep_Info_Available() &&
          !CG_DEP_Ignore_LNO &&
          Current_Dep_Graph != NULL)
      {
        WN *wn = Get_WN_From_Memory_OP(op);
        if (wn)
        {
          VINDEX16 v = Current_Dep_Graph->Get_Vertex(wn);
          if (v != 0)
            at_region->Set_LNO_Dep_Info_Available(true);
        }
      }
    }
  
  Autotie_Copy_PU_Messages_To_Region(bb, at_region);

  return at_region;
}


static void
add_loop_messages (AT_REGION *at_region, const char *fname, UINT lineno, BOOL inner_loop)
{
  if (LNO_SIMD_Level == 0)
    return;

  AT_MSG_ID msg_id = inner_loop ? AT_MSG_SIMD_NOANALYSIS_LOOP : AT_MSG_SIMD_OUTER_LOOP;
  
  if (inner_loop)
    at_region->Messages()->Append(
      XT_New(AT_Libauto_Pool()) AT_MESSAGE(AT_Libauto_Pool(),
                                           AT_MSG_SIMD_LOOP_BEGIN,
                                           fname ?: "",
                                           lineno,
                                           AT_MSG_ID_short_desc_fmt(AT_MSG_SIMD_LOOP_BEGIN)));
  at_region->Messages()->Append(
    XT_New(AT_Libauto_Pool()) AT_MESSAGE(AT_Libauto_Pool(),
                                         msg_id,
                                         fname ?: "",
                                         lineno,
                                         AT_MSG_ID_short_desc_fmt(msg_id)));
  
  if (inner_loop)
    at_region->Messages()->Append(
      XT_New(AT_Libauto_Pool()) AT_MESSAGE(AT_Libauto_Pool(),
                                           AT_MSG_SIMD_LOOP_NON_VECTORIZABLE,
                                           fname ?: "",
                                           lineno,
                                           AT_MSG_ID_short_desc_fmt(AT_MSG_SIMD_LOOP_NON_VECTORIZABLE)));
}


static void
Autotie_Analyze_Loop (LOOP_DESCR *const loop, BB_MAP bb_exec_cnts)
{
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid AT_PU"));

  BOOL inner_loop = (BB_innermost(LOOP_DESCR_loophead(loop)) != 0);
				 
  /* See if 'loop' or an outer loop of 'loop' has an AT_REGION
     attached. If so, then that region will serve as the parent region
     for all the block regions we create for the blocks of 'loop'. */
  UINT32 at_parent_id = find_parent_region_id(loop);

  /* If we don't have an AT_REGION, create an AT_REGION to represent it. */
  AT_REGION *new_parent_region = NULL;
  if (at_parent_id == AT_REGION_ID_UNKNOWN)
  {
    char *name = XT_New(AT_Libauto_Pool()) char[strlen(at_pu->Name()) + strlen("_L") + 64];
    sprintf(name, "%s_L%d", at_pu->Name(), at_pu->Region_Count());
	
    AT_REGION *at_region = XT_New(AT_Libauto_Pool()) AT_REGION(AT_REGION_KIND_LOOP, name);
	
    /* Set the source position from loopinfo if it is available, otherwise
       from the head block. */
    SRCPOS srcpos = 0;
    if (LOOP_DESCR_loopinfo(loop))
    {
      srcpos = LOOPINFO_srcpos(LOOP_DESCR_loopinfo(loop));
    }
    else
    {
      BB *head = LOOP_DESCR_loophead(loop);
      if (BB_loop_head_bb(head))
      {
	srcpos = BB_Loop_Srcpos(head);
      }
    }
    
    UINT lineno = SRCPOS_linenum(srcpos);
    at_region->Set_Linenum(lineno);

    const char *fname = get_filename_for_srcpos(srcpos);
    if (fname != NULL)
      at_region->Set_Filename(fname);

    at_region->Set_CG_Region(true);

    at_pu->Add_Region(at_region);
    
    /* Add messages to the analysis data for the XX XPRES analysis view. */
    add_loop_messages(at_region, fname, lineno, inner_loop);
    
    /* Copy PU-level messages associated with this region to
       the region-level message list. */
    Autotie_Copy_Srcpos_PU_Messages_To_Region(srcpos, at_region);

    at_parent_id = at_region->Id();
    new_parent_region = at_region;
  }

  Is_True(at_parent_id != AT_REGION_ID_UNKNOWN,
          ("Expected an AT_REGION parent ID"));
  
  /* At this point, we have a loop AT_REGION for which we create a set
     of child block AT_REGIONs representing all the blocks in
     'loop'. */

  /* Visit every BB that is in 'loop' and not in an enclosed loop. */
  BB *bb;
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
    {
      if (LOOP_DESCR_Find_Loop(bb) != loop)
	continue;

      /* Don't build regions for empty basic blocks. */
      if (BB_length(bb) == 0)
        continue;
      
      AT_REGION *at_region = Autotie_Build_Block_Region(bb, bb_exec_cnts, at_parent_id, inner_loop);

      /* If we created a new parent (loop) region, then set it's
         execution count based on 'bb's execution count. */
      if (new_parent_region)
        Autotie_Update_Exec_Count(new_parent_region, at_region->Exec_Count(), at_region->Exec_Kind());

      if (Get_Trace(TP_TEMP, 0x4000))
        {
          fprintf(TFile, "\nCG XPRES Analysis for BB %d, in loop with head BB %d\n",
                  BB_id(bb), BB_id(LOOP_DESCR_loophead(loop)));
          at_region->Print(TFile, 2, true);
        }
    }
}


static void
Autotie_Analyze_Block (BB *const bb, BB_MAP bb_exec_cnts)
{
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid AT_PU"));

  /* Ignore blocks that have a trivial number of ops... */
  if (BB_length(bb) <= 1)
    return;

  AT_REGION *at_region = Autotie_Build_Block_Region(bb, bb_exec_cnts, AT_REGION_ID_UNKNOWN, FALSE);

  /* Try to find a linenum and file for the block. */
  OP *op = BB_first_op(bb);
  while (op && (SRCPOS_linenum(OP_srcpos(op)) == 0))
    op = OP_next(op);

  if (op != NULL)
    {
      UINT lineno = SRCPOS_linenum(OP_srcpos(op));
      at_region->Set_Linenum(lineno);

      const char *fname = get_filename_for_srcpos(OP_srcpos(op));
      if (fname != NULL)
        at_region->Set_Filename(fname);
    }
    
  if (Get_Trace(TP_TEMP, 0x4000))
    {
      fprintf(TFile, "\nCG XPRES Analysis for BB %d\n", BB_id(bb));
      at_region->Print(TFile, 2, true);
    }
}


static void
Autotie_Analyze_Spills (BB *bb, AT_STAT *stat)
{

  int real_op_count = 0;
  int dfg_user_op_count = 0;

  OP *op;
  FOR_ALL_BB_OPs(bb, op)
    {
      if (!OP_simulated(op))
	real_op_count++;

      if (CGSPILL_Is_Spill_Op(op) && !OP_noop(op))
      {
	AT_OP_ID at_op_id = Find_At_Op_Id(op_tab, op);
	if (at_op_id == AT_OP_ID_UNKNOWN)
	  DevWarn("unable to create AT_OP for spill OP %s in BB %d of %s\n",
                  TI_TOP_Name(OP_code(op)), BB_id(bb), at_pu->Name());
	else
	  stat->Inc_Loop_Op_Count(at_op_id);
      }
    }

  tf_dfg_t dfg = BB_get_autotie_dfg(bb);

  if (!dfg)
    return;

  // SWP region usually has no register allocation problem
  // and the GTNs for SWP region is renamed
  // so invalidate the libauto register allocation info
  if (stat->Is_SWP())
    return;

  stat->Set_Has_LRA_Info(true);

  /* scan all operand TNs and store the LRA and GRA info */
  /* We need to set the user data of all nodes to the corresponding AT_OPs and
     the data of all operands to the corresponding AT_TYs. */
  for (tf_node_t node = tfex->dfg_first_node(dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {

    if (tfex->node_kind(node)==TFN_USER)
      dfg_user_op_count++;

    for (INT dir = 0; dir < 2; dir++) {
      tf_opnd_vec_t opnds =
	(dir == 0) ? tfex->node_inputs(node) : tfex->node_outputs(node);
      
      INT size = tfex->opnd_vec_size(opnds);
      for (INT oidx = 0; oidx < size; oidx++) {
	tf_opnd_t opnd = tfex->opnd_vec_get(opnds, oidx);
	TN *tn = (TN *)tfex->opnd_user(opnd);
	if (tn && TN_is_register(tn)) {

	  ISA_REGCLASS cl = TN_register_class(tn);
	  if (cl == TI_ISA_Regclass_Special() ||
	      TI_ISA_Regclass_Is_State(cl))
	    continue;

	  if (TN_is_global_reg(tn) || TN_is_dedicated(tn)) {
	    int tn_number = TN_number(tn);
	    if (!stat->Is_GRA_TN(tn_number))
	      stat->Set_GRA_TN(tn_number);
	  } else {
	    // a local TN
	    AT_TY *at_ty = Find_At_Ty(tn);
	    if (at_ty != NULL) {
	      AT_TY_ID at_ty_id = at_ty->Id();
	      int count;
	      if (!stat->Get_LRA_Reg_Count(at_ty_id,count)) {
	        count = REGISTER_SET_Size(GRA_Local_Register_Grant(bb, cl));
	        stat->Set_LRA_Reg_Count(at_ty_id, count);
	      }
	    }
	  }

	}
      }
    }
  }

  // check if the graph has changed so much that the LRA info is not reliable anymore
  if (((float)real_op_count/(float)dfg_user_op_count)<=0.5f)
    stat->Set_Has_LRA_Info(false);
}


static void
Autotie_Analyze_Taken_Prob (BB *bb, AT_STAT *stat)
{
  if (!FREQ_Frequencies_Computed())
    return;

  OP *br_op = BB_branch_op(bb);
  if (!br_op)
    return;

  /* Find the fall-through BB. */
  BBLIST *fts_list = BBlist_Fall_Thru_Succ(bb);
  if (!fts_list)
    return;

  /* Find the taken BB. */
  BBLIST *tlist = (fts_list == BB_succs(bb)) ?
    BBLIST_next(BB_succs(bb)) : BB_succs(bb);
  
  /* Set the taken probability based on the taken BB edge probability. */
  if (tlist)
    stat->Region()->Set_Taken_Prob(BBLIST_prob(tlist));
}


#endif /* TARG_XTENSA */


//
// Externs
//


void
AUTOTIE_Copy_BB_DFG (BB *bb_from, BB *bb_to)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  tf_dfg_t dfg = BB_get_autotie_dfg(bb_from);
  if (dfg)
    BB_set_autotie_dfg(bb_to, dfg);
#endif
}


void
AUTOTIE_Move_BB_DFG (BB *bb_from, BB *bb_to)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  tf_dfg_t dfg = BB_get_autotie_dfg(bb_from);
  if (dfg)
  {
    BB_set_autotie_dfg(bb_from, NULL);
    BB_set_autotie_dfg(bb_to, dfg);
  }
#endif
}


void
AUTOTIE_Convert_DFGs (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  const char *phase = enter_autotie_phase(CG_AUTOTIE_DFG_ERROR_PHASE);

  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid AT_PU"));

  /* We want to convert every fusion dfg to an autotie dfg
     We cannot do this by traversing all BBs as the dfgs were associated
     with BBs which may be merged by CG after the dfg creation
   */

  int region_count = at_pu->Region_Count();
  for (int i=0; i<region_count; i++) {
    AT_REGION* region = at_pu->Get_Region(i);

    if (region->Is_CG_Region()==false)
      continue;

    // convert operands for DFGs created in CG phase
    int stat_count = region->Stat_Count();
    for (int j=0; j<stat_count; j++) {
      AT_STAT* stat = region->Get_Stat(j);
      tf_dfg_t dfg = stat->DFG();
      dfg = Convert_DFG_Operands(dfg);
      stat->Set_DFG(dfg);
    }
  }
  
  exit_autotie_phase(phase);
#endif
}

void
AUTOTIE_Build_DFGs (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  const char *phase = enter_autotie_phase(CG_AUTOTIE_DFG_ERROR_PHASE);
  
  /* We want a dfg for every loop with a non-trivial number of ops. */
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
    {
      if (BB_length(bb) <= 1)
        continue;

      tf_dfg_t dfg = Autotie_Build_DFG(AT_Libauto_Pool(), bb);
      BB_set_autotie_dfg(bb, dfg);
    }

  exit_autotie_phase(phase);
#endif
}


void
AUTOTIE_Set_Region_Exec_Counts (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;
  
  const char *phase = enter_autotie_phase(CG_AUTOTIE_EXEC_ERROR_PHASE);
  
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid current AT_PU"));
  
  /* Get the actual execution count for each BB (BB_freq is normalized
     to per-function-invocation, so we don't want that). */
  BB_MAP bb_exec_cnts = FREQ_Denormalized_Counts();

  /* For each BB that belongs to a loop, find the AT_REGION that
     corresponds to that loop, or to an outer loop, and set the
     AT_REGION's execution count to be no less than BB's execution
     count. */
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    LOOP_DESCR *ld = LOOP_DESCR_Find_Loop(bb);
    while (ld)
    {
      LOOPINFO *li = LOOP_DESCR_loopinfo(ld);
      if (li)
      {
	WN *loop_info = LOOPINFO_wn(li);
	if (loop_info && (WN_loop_at_region_id(loop_info) != AT_REGION_ID_UNKNOWN))
	{
	  UINT32 at_region_id = WN_loop_at_region_id(loop_info);
	  AT_REGION *at_region = at_pu->Get_Region(at_region_id);
	  double bb_exec_cnt = BB_MAPFP_Get(bb_exec_cnts, bb);
	  AT_EXEC_COUNT_KIND bb_exec_kind = ((BB_freq_fb_based(bb)) ?
					     AT_EXEC_CNT_PROFILE :
					     AT_EXEC_CNT_EST);

          Autotie_Update_Exec_Count(at_region, bb_exec_cnt, bb_exec_kind);
	  break;
	}
      }

      ld = LOOP_DESCR_Next_Enclosing_Loop(ld);
    }
  }

  BB_MAP_Delete(bb_exec_cnts);

  exit_autotie_phase(phase);
#endif
}


void
AUTOTIE_Analyze (LOOP_DESCR *loops)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  const char *phase = enter_autotie_phase(CG_AUTOTIE_ERROR_PHASE);
  
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid current AT_PU"));
  
  /* Get the actual execution count for each BB (BB_freq is normalized
     to per-function-invocation, so we don't want that). */
  BB_MAP bb_exec_cnts = FREQ_Denormalized_Counts();

  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid AT_PU."));
  
  /* Initialize the total dynamic instruction count of 'at_pu'. */
  AT_CYCLES tcnt = 0;
  BOOL tcnt_fb = FALSE;
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    tcnt += (BB_MAPFP_Get(bb_exec_cnts, bb) * BB_length(bb));
    if (BB_freq_fb_based(bb))
      tcnt_fb = TRUE;
  }

  at_pu->Set_Dynamic_Inst_Kind(tcnt_fb ? AT_EXEC_CNT_PROFILE : AT_EXEC_CNT_EST);
  at_pu->Set_Dynamic_Inst_Count(tcnt);

  /* Collect information from each loop, and attach that information
     to the corresponding AT_REGION (which may need to be
     created). Also collect the bitset of all BBs that are contained
     in any loop. */
  BB_SET *loop_bbset = BB_SET_Create_Empty(100, &MEM_phase_nz_pool); 
  for (LOOP_DESCR *loop = loops; loop; loop = LOOP_DESCR_next(loop))
  {
    loop_bbset = BB_SET_UnionD(loop_bbset, LOOP_DESCR_bbset(loop), &MEM_phase_nz_pool);
    Autotie_Analyze_Loop(loop, bb_exec_cnts);
  }

  /* For each BB that is not in a loop, generate analysis
     information. */
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    if (BB_SET_MemberP(loop_bbset, bb))
      continue;
    
    /* Don't build regions for empty basic blocks. */
    if (BB_length(bb) == 0)
      continue;
    
    Autotie_Analyze_Block(bb, bb_exec_cnts);
  }
  
  BB_MAP_Delete(bb_exec_cnts);
  
  /* Remove all PU messages copied to a region from the PU-level
     list of messages. */
  Autotie_Remove_Copied_PU_Messages();

  exit_autotie_phase(phase);
#endif
}


void
AUTOTIE_Analyze_Spills (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;
  
  const char *phase = enter_autotie_phase(CG_AUTOTIE_SPILL_ERROR_PHASE);
  
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid current AT_PU"));
  
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    AT_STAT *stat = BB_get_at_stat(bb);
    if (stat)
      Autotie_Analyze_Spills(bb, stat);
  }

  exit_autotie_phase(phase);
#endif
}


void
AUTOTIE_Analyze_Taken_Probs (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;
  
  const char *phase = enter_autotie_phase(CG_AUTOTIE_TAKEN_PROB_ERROR_PHASE);
  
  FmtAssert(at_pu == (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info),
	    ("Invalid current AT_PU"));
  
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    AT_STAT *stat = BB_get_at_stat(bb);
    if (stat)
      Autotie_Analyze_Taken_Prob(bb, stat);
  }
  
  exit_autotie_phase(phase);
#endif
}


BOOL
AUTOTIE_Suppress_Unroll_Fully (LOOPINFO *info)
{
  /* pr8308. Suppress full unroll when loop has vector stats. */
  WN *loop_info = LOOPINFO_wn(info);
  if (loop_info && (WN_loop_at_region_id(loop_info) != AT_REGION_ID_UNKNOWN)) {
    UINT32 at_region_id = WN_loop_at_region_id(loop_info);
    AT_REGION *at_region = at_pu->Get_Region(at_region_id);
    if (at_region) {
      for (INT32 i = at_region->Stat_Count() - 1; i >= 0; i--) {
        if (at_region->Get_Stat(i)->SIMD_Width() > 1)
          return true;
      }
    }
  }

  return false;
}


void
AUTOTIE_Initialize (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  if (CG_opt_level < 2)
    fprintf(stderr, "\nwarning: -xpres analysis works better at -O2 or above\n");

  const char *phase = enter_autotie_phase(CG_AUTOTIE_ERROR_PHASE);
  
  at_pu = (AT_PU *)PU_Info_autotie_ptr(Current_PU_Info);
  op_tab = &(at_pu->Op_Tab());
  at_factory = XT_New(AT_Libauto_Pool()) AT_FACTORY(AT_Libauto_Pool(), op_tab);

  /* pr11095. Attach a filename to this PU because the containing
   * AT_FILE is the compilation file, not necessarily the file from
   * which the PU is defined (for example, the PU could be defined in
   * a header file, ugh). */
  INT64 srcpos = WN_Get_Linenum(PU_Info_tree_ptr(Current_PU_Info));
  const char *fname = get_filename_for_srcpos(srcpos);
  if (fname != NULL)
    at_pu->Set_Source_Filename(fname);
  
  remove_pu_messages = CXX_NEW(MESSAGE_MAP(101, &MEM_pu_nz_pool), &MEM_pu_nz_pool);

  /* Create a map from BB's to the autotie DFG corresponding to that BB. */
  bb_dfg_map = BB_MAP_Create();

  /* Create a map from BB's to the AT_STAT corresponding to that BB. */
  bb_stat_map = BB_MAP_Create();

  exit_autotie_phase(phase);
#endif
}


void
AUTOTIE_Finalize (void)
{
#ifdef TARG_XTENSA
  if (!Run_Autotie)
    return;

  const char *phase = enter_autotie_phase(CG_AUTOTIE_ERROR_PHASE);
  
  BB_MAP_Delete(bb_stat_map);
  BB_MAP_Delete(bb_dfg_map);

  exit_autotie_phase(phase);
#endif
}




// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
