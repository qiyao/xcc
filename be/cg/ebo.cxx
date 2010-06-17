
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo.cxx
 *  $Revision: 1.121 $
 *  $Date: 2000/10/17 16:09:12 $
 *  $Author: dew $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/ebo.cxx,v $
 *
 *  Revision comments:
 *
 *  29-May-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  EBO implementation.  See "ebo.h" for interface.
 *
 * =======================================================================
 * =======================================================================
 */

/* =======================================================================
 * =======================================================================
 *
 * This is the Extended Block Optimizer (EBO).
 *
 * It is a peep hole optimizer that works on a sequence of blocks that may
 * contain branches out, but can only be execxuted from the start of the
 * first block in the sequence.  These sequences are recognized during
 * processing.
 *
 * Instructions are processed in the forward direction through each block
 * and in the forward direction through the block's successor list.  New
 * blocks are processed until a "branched to" label is encountered, at
 * which time processing backs up and attempts to take a different path
 * down another successor list.
 *
 * The optimizations performed include forward propagation, common expression
 * elimination, constant folding, dead code elimination and a host of special
 * case transformations that are unique to the arcitecture of a particular
 * machine.
 *
 * In order to perform the optimizations it is nececssary to recognize the
 * definition and use connections between values.  Since this routine may be
 * called several times during compilation, and may have different information
 * available each time it is called, it is necessary to abstract the code
 * representation of the original program so that data values can be easily
 * tracked by common routines.
 *
 * The data structure used to abstract the values is the EBO_TN_INFO.  One is
 * created the first time a value is encoutered.  The EBO_TN_INFO entry is
 * associated with the register, if one has been assigned, or the TN.  This
 * allows us to support special, hard coded registers that are assigned early
 * in the code generation process, provided that references to the value
 * always use the register name to refer to that particular value.
 *
 * The data structure used to abstract each OP is the EBO_OP_INFO.  A unique
 * one is created for each OP that is encountered and contain pointers to
 * the unique EBO_TN_INFO entries that represent the values used and defined
 * by the original OP.
 *
 * These data structures make it simple to track predicated values and allow
 * for the redefintion of TNs and regeisters in a block, while providing
 * access to the previous definition.  It also simplifies the task of tracking
 * predicated code that reuses TN names and registers but define unique values.
 *
 * The construction of this abstraction is done as early s possible, allowing
 * the rest of EBO to work only with these *_INFO entries.  The design supports
 * tracking of constants, even when the original instructions can not directly
 * reference constants in their operand fields.
 *
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include <alloca.h>
#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "glob.h"    // for Cur_PU_Name
#include "tn_map.h"
#include "cg_loop.h"
#include "cg.h"
#include "cgexp.h"
#include "register.h"
#include "cg_region.h"
#include "wn.h"
#include "region_util.h"
#include "op_list.h"
#include "cgprep.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_db_op.h"
#include "whirl2ops.h"
#include "cgtarget.h"
#include "gra_live.h"
#include "reg_live.h"
#include "cflow.h"
#include "cg_spill.h"
#include "data_layout.h"
#include "stblock.h"

#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"


/* ===================================================================== */
/* Global Data:								 */
/* ===================================================================== */

INT32 EBO_Opt_Level_Default = 5;
INT32 EBO_Opt_Level = 5;
INT32 EBO_Copy_Prop = TRUE;
INT32 EBO_Prop_BB_Loop = TRUE;
INT32 EBO_Prop_Backward = TRUE;
INT32 EBO_Copy_Prop_Intra = FALSE;
INT32 EBO_Reg_Copy_Prop = TRUE;
INT32 EBO_Reg_Prop_SwpRegion = TRUE;
INT32 EBO_Reg_Prop_Backward = TRUE;
INT32 EBO_Select_Br = TRUE;
INT32 EBO_Remove_Store = TRUE;

INT EBO_tninfo_number = 0;
EBO_TN_INFO *EBO_free_tninfo = NULL;
EBO_TN_INFO *EBO_first_tninfo = NULL;
EBO_TN_INFO *EBO_last_tninfo = NULL;
EBO_OP_INFO *EBO_free_opinfo = NULL;
EBO_OP_INFO *EBO_first_opinfo = NULL;
EBO_OP_INFO *EBO_last_opinfo = NULL;
EBO_OP_INFO *EBO_opinfo_table[EBO_MAX_OP_HASH];

/* Entry point indicators. */
BOOL EBO_in_pre  = FALSE;
BOOL EBO_in_before_unrolling = FALSE;
BOOL EBO_in_after_unrolling = FALSE;
BOOL EBO_in_peep = FALSE;
BOOL EBO_in_acgprep = FALSE;

/* Are OMEGA entries present? */
BOOL EBO_in_loop = FALSE;

TN_MAP EBO_tninfo_table;
MEM_POOL EBO_pool;

INT EBO_num_tninfo_entries = 0;
INT EBO_tninfo_entries_reused = 0;
INT EBO_num_opinfo_entries = 0;
INT EBO_opinfo_entries_reused = 0;

char *EBO_trace_pfx;
BOOL EBO_Trace_Execution    = FALSE;
BOOL EBO_Trace_Optimization = FALSE;
BOOL EBO_Trace_Block_Flow   = FALSE;
BOOL EBO_Trace_Data_Flow    = FALSE;
BOOL EBO_Trace_Hash_Search  = FALSE;


/* ===================================================================== */
/* Local Data:								 */
/* ===================================================================== */

#define MAX_EBO_PASSES 3

static BOOL in_delay_slot = FALSE;
static BOOL rerun_cflow = FALSE;
static BOOL rerun_ebo = FALSE;
static UINT ebo_pass;

/* ===================================================================== */

/* The BB flag: <local_flag1> is overloaded temporarily in this routine
 * as <visited> to keep track of the fact that we have seen this block
 * during processing. The bit will NOT be cleared by the time we exit.
 */
#define BB_visited          BB_local_flag1
#define Set_BB_visited      Set_BB_local_flag1
#define Reset_BB_visited    Reset_BB_local_flag1

inline void clear_bb_flag(BB *first_bb)
{
  BB *bb;
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    BBLIST *succ_list;

    Reset_BB_visited(bb);

    FOR_ALL_BB_SUCCS(bb, succ_list) { 
      BB *succ = BBLIST_item(succ_list);
      Reset_BB_visited(succ);
    }

  }
}

/* ===================================================================== */

static
BOOL EBO_Fix_Same_Res_Op (OP *op,
                          TN **opnd_tn,
                          EBO_TN_INFO **opnd_tninfo)
{
  if (EBO_in_loop) return FALSE;;

#ifndef TARG_XTENSA
  if (OP_unalign_ld(op)) {
    TN *res = OP_result(op, 0);
    TN *tnl = OP_opnd(op, OP_opnds(op)-1);

    if (!TN_is_zero_reg(tnl) && !tn_registers_identical(res, tnl)) {
     /* Allocate a new TN for the result. */
      OPS ops = OPS_EMPTY;
      TN *new_res = Dup_TN (res);
      OP *new_op = Dup_OP (op);
      Exp_COPY(new_res, tnl, &ops);
      Set_OP_result(new_op, 0, new_res);
      Set_OP_opnd(new_op, OP_opnds(op)-1, new_res);
      OPS_Append_Op(&ops, new_op);
      Exp_COPY(res, new_res, &ops);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }

  } else if (OP_select(op)) {
    TN *res = OP_result(op, 0);
    TN *tn0 = OP_opnd(op, 0);
    TN *tn1;
    TN *tn2;

   /* For special case optimizations, check the OPTIMAL operands. */
    tn1 = opnd_tn[1];
    tn2 = opnd_tn[2];

    if (tn_registers_identical(tn1, tn2)) {
     /* We can optimize this! But return the ACTUAL operand. */
      OPS ops = OPS_EMPTY;
      Exp_COPY(res, OP_opnd(op, 1), &ops);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"Optimize select - operands are the same\n");
      }
      return TRUE;
    }

    if (TN_is_global_reg(tn1) &&
        (opnd_tninfo[1] != NULL) &&
        (opnd_tninfo[1]->in_op == NULL) &&
        (opnd_tninfo[1]->in_bb != NULL) &&
	(opnd_tninfo[1]->in_bb != OP_bb(op)) &&
        !tn_has_live_def_into_BB(tn1, opnd_tninfo[1]->in_bb)) {
     /* Assume that this value will not be used -
        turn this instruction into a copy of the other operand.
        But return the ACTUAL operand. */
      OPS ops = OPS_EMPTY;
      Exp_COPY(res, OP_opnd(op, 2), &ops);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"Optimize select - op1 %s can't be used: ",
                EBO_in_peep ? "REG" : "TN");
        Print_TN(OP_opnd(op, 2), FALSE);
        fprintf(TFile,"\n");
      }
      return TRUE;
    }

    if (TN_is_global_reg(tn2) &&
        (opnd_tninfo[2] != NULL) &&
        (opnd_tninfo[2]->in_op == NULL) &&
        (opnd_tninfo[2]->in_bb != NULL) &&
        (opnd_tninfo[2]->in_bb != OP_bb(op)) &&
        !tn_has_live_def_into_BB(tn2, opnd_tninfo[2]->in_bb)) {
    /* Assume that this value will not be used -
        turn this instruction into a copy of the other operand.
        But return the ACTUAL operand. */
      OPS ops = OPS_EMPTY;
      Exp_COPY(res, OP_opnd(op, 1), &ops);
      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"Optimize select - op2 %s can't be used: ",
                EBO_in_peep ? "REG" : "TN");
        Print_TN(OP_opnd(op, 1), FALSE);
        fprintf(TFile,"\n");
      }
      return TRUE;
    }

   /* For ensuring that the result operand matches one of the input operands,
      test the ACTUAL operands used in the expression. */
    tn0 = OP_opnd(op, 0);
    tn1 = OP_opnd(op, 1);
    tn2 = OP_opnd(op, 2);
    if ((TN_Is_Constant(tn1) || !tn_registers_identical(res, tn1)) &&
        (TN_Is_Constant(tn2) || !tn_registers_identical(res, tn2))) {
      OPS ops = OPS_EMPTY;
      OP *new_op = Dup_OP (op);

      if (has_assigned_reg(res)) {
       /* Use the existing result TN as the duplicate input. */
        TN *new_res = OP_result(op,0);
        FmtAssert((TN_Is_Constant(tn0) || !tn_registers_identical(res, tn0)),
                  ("Condition code also used as result of select"));
        if (TN_Is_Constant(tn2)) {
          Exp_COPY(new_res, tn2, &ops);
          Set_OP_opnd(new_op, 2, new_res);
        } else {
          Exp_COPY(new_res, tn1, &ops);
          Set_OP_opnd(new_op, 1, new_res);
        }
        OPS_Append_Op(&ops, new_op);
      } else {
       /* Allocate a new TN for the result. */
        TN *new_res = Dup_TN (res);
        Exp_COPY(new_res, tn1, &ops);
        Set_OP_result(new_op, 0, new_res);
        Set_OP_opnd(new_op, 1, new_res);
        OPS_Append_Op(&ops, new_op);
        Exp_COPY(res, new_res, &ops);
      }

      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      if (EBO_Trace_Optimization) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"Rewrite select\n");
      }
      return TRUE;
    }
  }
#endif /* TARG_XTENSA */
  
  return FALSE;
}

inline BOOL TN_live_out_of(TN *tn, BB *bb)
/* -----------------------------------------------------------------------
 * Requires: global liveness info up-to-date
 * Return TRUE iff <tn> is live out of <bb>.
 * -----------------------------------------------------------------------
 */
{
  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter TN_live_out_of BB:%d ",EBO_trace_pfx,BB_id(bb));
    Print_TN(tn, FALSE);
    fprintf(TFile,"\n");
  }

  if (EBO_in_peep) {
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"REG_LIVE_Outof_BB %s\n",
         REG_LIVE_Outof_BB (TN_register_class(tn), TN_register(tn), bb)?"TRUE":"FALSE");
    }
    return (TN_register(tn) != REGISTER_UNDEFINED &&
            REG_LIVE_Outof_BB (TN_register_class(tn), TN_register(tn), bb));
  }
  else {
    if (CG_localize_tns) return (TN_is_dedicated(tn) || TN_is_global_reg(tn));
    return GRA_LIVE_TN_Live_Outof_BB (tn, bb);
  }
}



inline BOOL op_is_needed_globally(OP *op)
/* -----------------------------------------------------------------------
 * Requires: global liveness info
 * Return TRUE iff the result of <op> is necessary past the end of its BB.
 * ----------------------------------------------------------------------- */
{
  BB *bb = OP_bb(op);

  if (OP_copy(op)) {
   /* Copies don't have side effects unless a save_reg is involved. */
    if (OP_glue(op) && !EBO_in_peep)
      return TRUE;

    INT32 copy_opnd = TI_ISA_Copy_Operand(OP_code(op), TRUE);
    bool is_save_result = TN_is_save_reg(OP_result(op,0));
    bool is_save_opnd = TN_is_save_reg(OP_opnd(op,copy_opnd));
    bool regs_identical = tn_registers_identical(OP_result(op,0), OP_opnd(op,copy_opnd));
    if ((is_save_result || is_save_opnd) && !regs_identical) {
      return TRUE;
    }
    return FALSE;
  }
  if (TN_is_save_reg(OP_result(op,0)))
    return TRUE;
  if (OP_glue(op) && !has_assigned_reg(OP_result(op,0)))
    return TRUE;
  if (CGTARG_Is_OP_Intrinsic(op))
   /* Intrinsic ops may have side effects we don't understand */
    return TRUE;
  if (OP_call(op)) 
   /* Calls may have side effects we don't understand */
    return TRUE;
  if (op == BB_exit_sp_adj_op(bb) || op == BB_entry_sp_adj_op(bb))
    return TRUE;
  return FALSE;
}


/* ===================================================================== */


void
tn_info_entry_dump (EBO_TN_INFO *tninfo)
{
  fprintf(TFile,"entry %d\tBB:%d, use count = %d, redefined = %s, same as %d, predicate %d:  ",
          tninfo->sequence_num,tninfo->in_bb?BB_id(tninfo->in_bb):0,
          tninfo->reference_count,
          tninfo->redefined_before_block_end?"TRUE":"FALSE",
          tninfo->same?tninfo->same->sequence_num:0,
          tninfo->predicate_tninfo?tninfo->predicate_tninfo->sequence_num:0);
  Print_TN (tninfo->local_tn, TRUE);
  fprintf(TFile,"[%d]",tninfo->omega);
  if (tninfo->replacement_tn != NULL) {
    fprintf(TFile,"\n\treplace TN with: ");
    Print_TN (tninfo->replacement_tn, TRUE);
    if (tninfo->replacement_tninfo != NULL) {
      fprintf(TFile," (Entry Number %d)",
              tninfo->replacement_tninfo->sequence_num);
    }
  }
  fprintf(TFile,"\n");
  if (tninfo->in_op) {
    fprintf(TFile,"\t");
    Print_OP_No_SrcLine(tninfo->in_op);
  }

  FmtAssert((TN_number(tninfo->local_tn) <= Last_TN),
                  ("TN number exceeds allowed range"));

}


void
tn_info_table_dump ()
{
  EBO_TN_INFO *tninfo = EBO_first_tninfo;

  fprintf(TFile,"\n>>>>>> EBO INFO DUMP <<<<<\n");

  while (tninfo != NULL) {
    tn_info_entry_dump(tninfo);
    tninfo = tninfo->next;
  }

  fprintf(TFile,">>>>>> EBO INFO DUMP COMPLETE <<<<<\n\n");

}



void EBO_Init(void)
/* -----------------------------------------------------------------------
 * See "ebo.h" for interface.
 * -----------------------------------------------------------------------
 */
{

  EBO_Trace_Execution    = FALSE;
  EBO_Trace_Optimization = FALSE;
  EBO_Trace_Block_Flow   = FALSE;
  EBO_Trace_Data_Flow    = FALSE;
  EBO_Trace_Hash_Search  = FALSE;

  MEM_POOL_Initialize(&EBO_pool, "ebo", FALSE);
  MEM_POOL_Push(&EBO_pool);
  EBO_tninfo_table = NULL;

  EBO_tninfo_number = 0;
  EBO_free_tninfo = NULL;
  EBO_first_tninfo = NULL;
  EBO_last_tninfo = NULL;

  EBO_free_opinfo = NULL;
  EBO_first_opinfo = NULL;
  EBO_last_opinfo = NULL;

  memset(EBO_opinfo_table, 0,sizeof(EBO_opinfo_table));

  EBO_num_tninfo_entries = 0;
  EBO_tninfo_entries_reused = 0;
  EBO_num_opinfo_entries = 0;
  EBO_opinfo_entries_reused = 0;
  EBO_trace_pfx = "<ebo> ";
}




static void EBO_Start()
/* -----------------------------------------------------------------------
 * -----------------------------------------------------------------------
 */
{
 /* Initialize data structures.  */
  MEM_POOL_Push(&MEM_local_pool);
  EBO_tninfo_table = TN_MAP_Create();
  EBO_Special_Start(&MEM_local_pool);
}



static void EBO_Finish(void)
/* -----------------------------------------------------------------------
 * -----------------------------------------------------------------------
 */
{
  EBO_Special_Finish();
  TN_MAP_Delete (EBO_tninfo_table);
  EBO_tninfo_table = NULL;
  MEM_POOL_Pop(&MEM_local_pool);
}




void EBO_Finalize(void)
/* -----------------------------------------------------------------------
 * See "ebo.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  MEM_POOL_Pop(&EBO_pool);
  MEM_POOL_Delete(&EBO_pool);
}


/* ===================================================================== */

  
static void
find_index_and_offset (EBO_TN_INFO *original_tninfo,
                       TN **base_tn, EBO_TN_INFO **base_tninfo,
                       TN **offset_tn, EBO_TN_INFO **offset_tninfo)
{
 /* Look for an offset descriptor in the index. */
  EBO_OP_INFO *indx_opinfo = locate_opinfo_entry(original_tninfo);
  if ((indx_opinfo != NULL) &&
      (indx_opinfo->in_op != NULL) &&
      (OP_iadd(indx_opinfo->in_op) || 
       EBO_Can_Merge_Into_Offset(indx_opinfo->in_op))) {
    INT op1_idx = TI_TOP_Find_Operand_Use(OP_code(indx_opinfo->in_op),OU_opnd1);
    INT op2_idx = TI_TOP_Find_Operand_Use(OP_code(indx_opinfo->in_op),OU_opnd2);

    if ((op1_idx >= 0) && (op2_idx >= 0)) {
      EBO_TN_INFO *op1_tninfo = indx_opinfo->actual_opnd[op1_idx];
      EBO_TN_INFO *op2_tninfo = indx_opinfo->actual_opnd[op2_idx];
      TN *op1_tn;
      TN *op2_tn;
      if (op1_tninfo != NULL) {
        if ((op1_tninfo->replacement_tn) &&
            (TN_is_symbol(op1_tninfo->replacement_tn) || TN_Is_Constant(op1_tninfo->replacement_tn))) {
          op1_tn = op1_tninfo->replacement_tn;
          op1_tninfo = op1_tninfo->replacement_tninfo;
        } else {
          op1_tn = op1_tninfo->local_tn;
        }
      } else {
        op1_tn = OP_opnd(indx_opinfo->in_op,op1_idx);
      }
      if (op2_tninfo != NULL) {
        if ((op2_tninfo->replacement_tn) &&
            (TN_is_symbol(op2_tninfo->replacement_tn) || TN_Is_Constant(op2_tninfo->replacement_tn))) {
          op2_tn = op2_tninfo->replacement_tn;
          op2_tninfo = op2_tninfo->replacement_tninfo;
        } else {
          op2_tn = op2_tninfo->local_tn;
        }
      } else {
        op2_tn = OP_opnd(indx_opinfo->in_op,op2_idx);
      }

      if (TN_is_symbol(op1_tn) || TN_Is_Constant(op1_tn)) {
        TN *save = op1_tn;
        op1_tn = op2_tn;
        op2_tn = save;
        op1_tninfo = op2_tninfo;
        op2_tninfo = NULL;
      }

      *base_tn = op1_tn;
      *base_tninfo = op1_tninfo;
      *offset_tn = op2_tn;
      *offset_tninfo = op2_tninfo;

      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sUse inputs to index: ",EBO_trace_pfx);
        Print_TN(*base_tn,FALSE);
        fprintf(TFile," ");
        Print_TN(*offset_tn,FALSE);
        fprintf(TFile,"\n");
      }
    }
  }
}



static void
merge_memory_offsets( OP *op,
                      INT index_opnd,
                      INT offset_opnd,
                      TN **opnd_tn,
                      EBO_TN_INFO **opnd_tninfo,
                      EBO_TN_INFO **actual_tninfo)
{
  EBO_TN_INFO *index_tninfo = opnd_tninfo[index_opnd];
  OP *index_op = (index_tninfo != NULL) ? index_tninfo->in_op : NULL;

  TN *mem_tn = opnd_tn[offset_opnd];
  ST *mem_sym = TN_is_symbol(mem_tn) ? TN_var(mem_tn) : NULL;
  INT64 mem_offset = TN_is_symbol(mem_tn) ? TN_offset(mem_tn) : TN_Value(mem_tn);

  UINT index_ind, immed_ind;

  if ((index_tninfo == NULL) ||
      (index_op == NULL) ||
      (!EBO_Can_Merge_Into_Offset (index_op, &index_ind, &immed_ind)))
    return;

  EBO_OP_INFO *index_opinfo = locate_opinfo_entry (index_tninfo);
  if (index_opinfo == NULL)
    return;

  TN *additive_index_tn = OP_opnd(index_op, index_ind);
  EBO_TN_INFO *additive_index_tninfo = index_opinfo->actual_opnd[index_ind];

  TN *additive_immed_tn = OP_opnd(index_op, immed_ind);
  if (!TN_Is_Constant(additive_immed_tn)) {

    EBO_TN_INFO *additive_offset_tninfo = index_opinfo->actual_opnd[immed_ind];
    if (additive_offset_tninfo==NULL)
      return;

    if (EBO_in_peep) {
      OP* ld_const_op = additive_offset_tninfo->in_op;
      if (ld_const_op==NULL ||
          (OP_code(ld_const_op)!=TOP_load_const &&
           OP_code(ld_const_op)!=TOP_const16lo &&
           OP_code(ld_const_op)!=TOP_movi))
        return;
      if (OP_code(ld_const_op) == TOP_const16lo)
	additive_immed_tn = OP_opnd(ld_const_op, 1);
      else
	additive_immed_tn = OP_opnd(ld_const_op, 0);
    } else
      return;
  }

  BOOL updating = false;
#ifdef TARG_XTENSA
  /* if the base_updating then make sure the add/sub is self-updating and there is no other
   * use in between
   */
  if (OP_same_res(op)) {
    if (OP_base_update(op) && TI_TOP_Find_Operand_Use(OP_code(op),OU_base)==index_opnd) {
      TN *base_tn = opnd_tn[index_opnd];
      if (base_tn != additive_index_tn)
        return;

      /* for base_updating case, we allow only constant immediate */
      if (!TN_Is_Constant(additive_immed_tn))
	return;

      OP* scan = OP_prev(op);
      while (scan && scan!=index_op) {
	if (OP_Refs_TN(scan, additive_index_tn))
	  return;
	scan = OP_prev(scan);
      }
      if (scan==NULL)
	return;

      updating = true;
    }
  }
#endif
  
  /* Would the new index value be available for use? */
  if (!TN_Is_Constant(additive_index_tn) && !updating &&
      !EBO_tn_available(OP_bb(op), additive_index_tninfo))
    return;

  TN *adjust_tn = NULL;
  ST *adjust_sym = NULL;
  INT64 adjust_offset = 0;

  if (TN_is_symbol(additive_immed_tn))
  {
    if ((mem_sym != NULL) || OP_isub(index_op))
      return;
    
    adjust_sym = TN_var(additive_immed_tn);
    adjust_tn = additive_immed_tn;
    adjust_offset = mem_offset + TN_offset(additive_immed_tn);

    /* handles stack symbols */
    if (adjust_sym!=SP_Sym || additive_index_tn!=SP_TN)
      return;
  }
  else
  {
    adjust_sym = mem_sym;
    adjust_tn = mem_tn;
    adjust_offset = TN_Value(additive_immed_tn);
    if (OP_isub(index_op))
      adjust_offset = -adjust_offset;
    adjust_offset += mem_offset;
  }

  /* Make sure 'adjust_offset' can fit in the immediate operand. */

  INT64 base_ofst = adjust_offset;
  if (adjust_sym)
  {
    ST *base_st;
    Base_Symbol_And_Offset_For_Addressing(adjust_sym, adjust_offset, &base_st, &base_ofst);
  }
  
  if (!TI_TOP_Can_Have_Immediate( base_ofst, OP_code(op)))
  {
    if (EBO_Trace_Optimization)
    {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "%sin BB:%d combined index expressions do not fit in the offset field\n",
              EBO_trace_pfx, BB_id(OP_bb(op)));
      Print_OP_No_SrcLine(op);
    }
    
    return;
  }

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "%sin BB:%d merge memory index expression (from BB:%d) with offset (in BB:%d)\n",
            EBO_trace_pfx, BB_id(OP_bb(op)),BB_id(OP_bb(index_op)),BB_id(OP_bb(op)));
    Print_OP_No_SrcLine(index_op);
    Print_OP_No_SrcLine(op);
  }

 /* Create a combined TN and update the op and other data structures. */
  TN *new_tn;
  if (adjust_sym != NULL) {
    return;
    new_tn = Gen_Symbol_TN(adjust_sym, adjust_offset, TN_relocs(adjust_tn));
  } else {
    new_tn = Gen_Literal_TN (adjust_offset, TN_size(adjust_tn));
  }

  Set_OP_opnd(op, offset_opnd, new_tn);
  if (EBO_in_loop && _CG_LOOP_info(op)) {
    Set_OP_omega (op, offset_opnd, 0);
  }
  opnd_tn[offset_opnd] = new_tn;
  opnd_tninfo[offset_opnd] = NULL;
  actual_tninfo[offset_opnd] = NULL;

  if (updating) {
    /* for updating load/store, we simply set the immediate for index op to be 0
     */
    TN* immed_tn = Gen_Literal_TN (0, TN_size(adjust_tn));

    /* the additive immediate is guaranteed to be constant for base_updating case */
    Set_OP_opnd(index_op, immed_ind, immed_tn);
    if (EBO_in_loop && _CG_LOOP_info(index_op)) {
      Set_OP_omega (index_op, immed_ind, 0);
    }
  } else {

    if (actual_tninfo[index_opnd] != NULL) {
      dec_ref_count(actual_tninfo[index_opnd]);
    }
    if (additive_index_tninfo != NULL) {
      inc_ref_count(additive_index_tninfo);
    }
    Set_OP_opnd(op, index_opnd, additive_index_tn);
    if (EBO_in_loop && _CG_LOOP_info(op)) {
      Set_OP_omega (op, index_opnd, (additive_index_tninfo != NULL) ? additive_index_tninfo->omega : 0);
    }
    opnd_tn[index_opnd] = additive_index_tn;
    opnd_tninfo[index_opnd] = additive_index_tninfo;
    actual_tninfo[index_opnd] = additive_index_tninfo;
  }

  if (EBO_Trace_Optimization) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile, "  to produce the new expression:\n");
    Print_OP_No_SrcLine(op);
  }

}




/* 
 * find_duplicate_mem_op
 *
 * For a given memory op, look for a preceeding memory op to 
 * the same location and attempt to replace one of them.
 * Return TRUE if this memory op is no longer needed.
 */
static BOOL
find_duplicate_mem_op (BB *bb,
                       OP *op,
                       TN **opnd_tn,
                       EBO_TN_INFO **opnd_tninfo,
                       EBO_TN_INFO **actual_tninfo)
/* -----------------------------------------------------------------------
 * Requires: 
 * Returns TRUE if the operands of each OP are identical.
 * -----------------------------------------------------------------------
 */
{
  INT hash_value = 0;
  INT hash_search_length = 0;
  EBO_OP_INFO *opinfo;
  EBO_OP_INFO *adjacent_location = NULL;
  INT64 adjacent_offset_pred;
  INT64 adjacent_offset_succ;

  if (op == NULL) return FALSE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter find_duplicate_mem_op\n",EBO_trace_pfx);
  }

 /* Determine the indexes of the address components of this memory op. */
  INT succ_base_idx = TI_TOP_Find_Operand_Use(OP_code(op),OU_base);
  INT succ_offset_idx = TI_TOP_Find_Operand_Use(OP_code(op),OU_offset);

  if ((succ_base_idx >= 0) && (succ_offset_idx >= 0) &&
      TN_Is_Constant(opnd_tn[succ_offset_idx])) {
   /* Look for merge-able expressions. */
    merge_memory_offsets (op, succ_base_idx, succ_offset_idx, opnd_tn, opnd_tninfo, actual_tninfo);
  }

  if (!(OP_load(op) && OP_results(op)==1) &&
      !(OP_store(op) && OP_results(op)==0))
    return FALSE;

 /* Determine the address components of this memory op. */
  TN *succ_base_tn = (succ_base_idx >= 0) ? opnd_tn[succ_base_idx] : NULL;
  EBO_TN_INFO *succ_base_tninfo = (succ_base_idx >= 0) ? opnd_tninfo[succ_base_idx] : NULL;
  TN *succ_offset_tn = (succ_offset_idx >= 0) ? opnd_tn[succ_offset_idx] : NULL;
  EBO_TN_INFO *succ_offset_tninfo = (succ_offset_idx >= 0) ? opnd_tninfo[succ_offset_idx] : NULL;

  if ((succ_offset_tn == NULL) && (succ_base_tn != NULL)) {
    find_index_and_offset(succ_base_tninfo,
                          &succ_base_tn, &succ_base_tninfo,
                          &succ_offset_tn, &succ_offset_tninfo);
  }

 /* Determine the proper hash value. */
  hash_value = EBO_hash_op( op, opnd_tninfo);

  if (EBO_Trace_Hash_Search) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sLook for redundant memory ops in hash chain %d for\n\t",
                  EBO_trace_pfx,hash_value);
    Print_OP_No_SrcLine(op);
  }

  opinfo = (OP_has_implicit_interactions(op) ||
	    OP_unalign_mem(op) ||
	    OP_unknown_addr(op)) ? NULL : EBO_opinfo_table[hash_value];

  while (opinfo) {
    OP *pred_op = opinfo->in_op;
    INT64 offset_pred = 0;
    INT64 offset_succ = 0;

   /* Identify the address components of the predecessor memory op. */
    INT pred_base_idx = 0;
    INT pred_offset_idx = 0;
    TN *pred_base_tn = NULL;
    EBO_TN_INFO *pred_base_tninfo = NULL;
    TN *pred_offset_tn = NULL;
    EBO_TN_INFO *pred_offset_tninfo = NULL;

    if ((pred_op != NULL) &&
        (OP_load(pred_op) || OP_store(pred_op))) {
     /* Determine the address components of the predecessor memory op. */
      pred_base_idx = TI_TOP_Find_Operand_Use(OP_code(pred_op),OU_base);
      pred_offset_idx = TI_TOP_Find_Operand_Use(OP_code(pred_op),OU_offset);
      pred_base_tn = (pred_base_idx >= 0) ? OP_opnd(pred_op,pred_base_idx) : NULL;
      pred_base_tninfo = (pred_base_idx >= 0) ? opinfo->optimal_opnd[pred_base_idx] : NULL;
      pred_offset_tn = (pred_offset_idx >= 0) ? OP_opnd(pred_op,pred_offset_idx) : NULL;
      pred_offset_tninfo = (pred_offset_idx >= 0) ? opinfo->optimal_opnd[pred_offset_idx] : NULL;
    }

    if ((pred_offset_tn == NULL) && (pred_base_tn != NULL)) {
      find_index_and_offset(pred_base_tninfo,
                            &pred_base_tn, &pred_base_tninfo,
                            &pred_offset_tn, &pred_offset_tninfo);
    }

    BOOL hash_op_matches = ((pred_op != NULL) &&
                            OP_memory(pred_op) &&
#ifdef TARG_XTENSA
			    /* We want to make sure there is a base
                               and offset TN in each OP. Otherwise we
                               can mistakenly match incorrectly
                               (e.g. see pr2611, where index loads
                               match because neither has an offset
                               even-though they have different index
                               registers). We could match OPs that
                               have the same base and index register,
                               but currently this code only handles
                               immediate offsets. We are also
                               conservative because we don't match OPs
                               which have only a base TN (no offset or
                               index), because we currently can
                               distinquish those (we just need to add
                               another OP property). */
			    pred_base_tn &&
			    (pred_offset_tn == NULL || 
			     TN_is_constant(pred_offset_tn)) &&
			    (succ_offset_tn == NULL ||
			     TN_is_constant(succ_offset_tn)) &&
#endif
                            (pred_base_tn == succ_base_tn) &&           /* The base  index must match */
                            (pred_base_tninfo == succ_base_tninfo) &&   /* The base   info must match */
                            (pred_offset_tninfo == succ_offset_tninfo)) /* The offset info must match */
                            ? TRUE : FALSE;
    BOOL op_is_subset = FALSE;
    BOOL offsets_may_overlap = TRUE;

    hash_search_length++;

    if (hash_op_matches &&
        (pred_offset_tn != succ_offset_tn)) {
     /* The offset tn's need to be looked at in more detail. */

      ST *symbol_pred = ((pred_offset_tn != NULL) && TN_is_symbol(pred_offset_tn)) ?TN_var(pred_offset_tn) : NULL;
      ST *symbol_succ = ((succ_offset_tn != NULL) && TN_is_symbol(succ_offset_tn)) ?TN_var(succ_offset_tn) : NULL;
      mUINT8 relocs_pred = (pred_offset_tn != NULL) ? TN_relocs(pred_offset_tn) : 0;
      mUINT8 relocs_succ = (succ_offset_tn != NULL) ? TN_relocs(succ_offset_tn) : 0;
      offset_pred = (pred_offset_tn != NULL) ? TN_offset(pred_offset_tn) : 0;
      offset_succ = (succ_offset_tn != NULL) ? TN_offset(succ_offset_tn) : 0;

     /* This time, the relocations must be the same. */
      hash_op_matches = (symbol_pred == symbol_succ) && (relocs_pred == relocs_succ);

      if ((OP_prefetch(op) || OP_prefetch(pred_op)) &&
          (offset_pred != offset_succ)) {
        hash_op_matches = FALSE;
      } else if (hash_op_matches) {
       /* If the relocations are the same, we need to examine the offsets and sizes. */
        INT size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
        INT size_succ = CGTARG_Mem_Ref_Bytes(op);

        if ((offset_pred == offset_succ) &&
            (size_pred == size_succ)) {
         /* The perfect match: location and size. */
        }  else if ((offset_pred <= offset_succ) &&
                    ((offset_pred + size_pred) >= (offset_succ + size_succ))) {
         /* The current reference is a subset of the preceeding one. */
          op_is_subset = TRUE;
        } else if (OP_load(op) && OP_load(pred_op) && (size_pred == size_succ) &&
                   (((offset_pred + size_pred) == offset_succ) ||
                    ((offset_succ + size_succ) == offset_pred))) {
          offsets_may_overlap = FALSE;
          if (adjacent_location == NULL) {
            adjacent_location = opinfo;
            adjacent_offset_pred = offset_pred;
            adjacent_offset_succ = offset_succ;
          }
        } else if (((offset_pred + size_pred) <= offset_succ) ||
                   ((offset_succ + size_succ) <= offset_pred)) {
         /* There is no potential overlap. */
          offsets_may_overlap = FALSE;
        } else {
         /* Any other case may be a potential conflict. */
          hash_op_matches = FALSE;
        }
      }
    }

    if (hash_op_matches && !offsets_may_overlap ) {
      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sMemory overlap not possible with\n\t",
                      EBO_trace_pfx);
        Print_OP_No_SrcLine(pred_op);
      }
      opinfo = opinfo->same;
      continue;
    }

    if (hash_op_matches && OP_has_predicate(op)) {
     /* Check predicates for safety. */

      if (OP_store(op) && OP_store(pred_op)) {
        if (!EBO_predicate_dominates(OP_opnd(op,OP_PREDICATE_OPND),
                                     actual_tninfo[OP_PREDICATE_OPND],
                                     OP_opnd(pred_op,OP_PREDICATE_OPND),
                                     opinfo->optimal_opnd[OP_PREDICATE_OPND])) {
          hash_op_matches = FALSE;

          if (EBO_Trace_Hash_Search) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sStore predicates do not match\n\t", EBO_trace_pfx);
            Print_OP_No_SrcLine(pred_op);
          }

         /* We need to be extra safe with stores.  Assume a conflict. */
          break;
        }
      } else if (!OP_store(op)) {
        if (!EBO_predicate_dominates(OP_opnd(pred_op,OP_PREDICATE_OPND),
                                     opinfo->optimal_opnd[OP_PREDICATE_OPND],
                                     OP_opnd(op,OP_PREDICATE_OPND),
                                     actual_tninfo[OP_PREDICATE_OPND])) {
          hash_op_matches = FALSE;

          if (EBO_Trace_Hash_Search) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sLoad predicates do not match\n\t", EBO_trace_pfx);
            Print_OP_No_SrcLine(pred_op);
          }

          if (OP_store(pred_op)) {
           /* We need to be extra safe with stores.  Assume a conflict. */
            break;
          }
        }
      }
    }

    if ((pred_op != NULL) &&
        (hash_value == EBO_DEFAULT_MEM_HASH) &&
        (OP_store(pred_op) != OP_store(op)) &&
        (!OP_prefetch(op))) {
     /* Need to be careful about alias issues. */
      WN *pred_wn;
      WN *succ_wn;
      ALIAS_RESULT result;

      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sPotential Alias problem with\n\t",EBO_trace_pfx);
        Print_OP_No_SrcLine(pred_op);
      }

      result = POSSIBLY_ALIASED;
      if (Alias_Manager != NULL) {
        pred_wn = OP_hoisted(pred_op) ? NULL : Get_WN_From_Memory_OP(pred_op);
        succ_wn = OP_hoisted(op) ? NULL : Get_WN_From_Memory_OP(op);
        if ((pred_wn != NULL) && (succ_wn != NULL)) {
          result = Aliased(Alias_Manager, pred_wn, succ_wn);
          if ((!hash_op_matches) && (result == SAME_LOCATION)) {
           /* This also implies that the size of the items is the same. */
            hash_op_matches = TRUE;
            if (EBO_Trace_Hash_Search) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,"%sAlias_Manager identifies SAME_LOCATION\n",
                             EBO_trace_pfx);    
            }
           /* The Alias_Manager may think that the locations are the same,
              but we know that they may not be.  This is because if-conversion
              may create something called a "black hole".  It is hard to believe
              that the optimizations we do will be OK in this situation. */
            if (OP_store(op)) opinfo->op_must_not_be_moved = TRUE;
            break;
          }
        }
      }

      if ((result == POSSIBLY_ALIASED) && (!hash_op_matches)) {
        if (OP_store(pred_op)) opinfo->op_must_not_be_removed = TRUE;
        if (EBO_Trace_Hash_Search) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sAlias information(%d) prevents us from continuing.\n",
                        EBO_trace_pfx,result);
        }
        break;
      }
      if (hash_op_matches && op_is_subset &&
          OP_store(op) && OP_store(pred_op)) {
        opinfo->op_must_not_be_removed = TRUE;
        if (EBO_Trace_Hash_Search) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sOverlapping store prevents us from continuing.\n",
                        EBO_trace_pfx);
        }
        break;
      }

      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sAlias information allows us to continue.\n",EBO_trace_pfx);
      }
    }

    if (in_delay_slot && !OP_store(op)) {
     /* In general, we can't remove an OP from a delay slot, but
        duplicate stores are removed by noop'ing the pred_op. */
      break;
    }

    if (hash_op_matches && 
        (pred_op != NULL)
#ifndef TARG_XTENSA
	/* What do these test mean??? How can we have a match unless
           both 'pred_op' and 'op' are both loads or both stores. With
           these tests, when both are stores we don't check to make
           sure the stored value is available at the second store. */
        && (OP_load(pred_op) ||
         (OP_store(pred_op) && !OP_store(op)))
#endif
       ) {

      if (OP_store(pred_op)) {
        INT pred_stored_idx = TI_TOP_Find_Operand_Use(OP_code(pred_op),OU_storeval);
        TN *pred_tn = NULL;
	if (pred_stored_idx >=0)
          pred_tn = OP_opnd(pred_op,pred_stored_idx);
        if (pred_tn && !TN_Is_Constant(pred_tn)) {
         /* The stored register needs to be available at this point. */
          if (!EBO_tn_available(bb,opinfo->actual_opnd[pred_stored_idx])) {
            opinfo->op_must_not_be_removed = TRUE;
            if (EBO_Trace_Hash_Search) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,"%sMemory match found, but stored value is not available\n\t",
                            EBO_trace_pfx);
              Print_OP_No_SrcLine(pred_op);
            }
            break;
          }
        }
      } else {
        TN *pred_tn = OP_result(pred_op,0);
        if (!TN_Is_Constant(pred_tn)) {
         /* The previous result needs to be available at this point. */
          if (!EBO_tn_available(bb,opinfo->actual_rslt[0])) {
            if (EBO_Trace_Hash_Search) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,"%sMemory match found, but loaded value is not available\n\t",
                            EBO_trace_pfx);
              Print_OP_No_SrcLine(pred_op);
            }
            break;
          }
        }
      }
    }

    if (pred_op != NULL &&
	((OP_load(pred_op) && OP_results(pred_op)!=1) ||
	 (OP_store(pred_op) && OP_results(pred_op)!=0))) {

      // avoid matching updating mem ops as duplicate
      opinfo = opinfo->same;
      continue;
    }

    if (hash_op_matches) {
      BOOL op_replaced = FALSE;

      if (OP_volatile(pred_op)) {
       /* If we match a volatile memory op, this
          one should have been volatile, too. */
        break;
      }

      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sMatch after searching %d items on hash chain %d\n",
                      EBO_trace_pfx,hash_search_length,hash_value);
      }

      if (op_is_subset) {
        op_replaced = delete_subset_mem_op (op, opnd_tninfo, opinfo, offset_pred, offset_succ);
      } else {
        op_replaced = delete_duplicate_op (op, opnd_tninfo, opinfo);
      }

      if (op_replaced) {
        return TRUE;
      } else {
       /* If we matched once and failed to eliminate it,
          we may need to keep both around. */
          if (OP_store(op)) opinfo->op_must_not_be_moved = TRUE;
        if (op_is_subset || (hash_value == EBO_DEFAULT_MEM_HASH)) {
          if (OP_store(pred_op)) opinfo->op_must_not_be_removed = TRUE;
          break;
        }
        if (OP_store(pred_op) || OP_store(op)) {
          break;
        }
      }
    }

  opinfo = opinfo->same;
  }

  if (adjacent_location != NULL) {
    BOOL op_replaced = combine_adjacent_loads (op, opnd_tninfo, adjacent_location,
                                               adjacent_offset_pred, adjacent_offset_succ);
    if (op_replaced) {
      return TRUE;
    }
  }

  if (EBO_Trace_Hash_Search) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sFail after searching %d items on memory hash chain %d\n",
                  EBO_trace_pfx,hash_search_length,hash_value);
  }

  return FALSE;
}




/* 
 * find_duplicate_op
 *
 * For a given expression op, look for a preceeding indentical
 * expressionn and attempt to replace the new one.
 * Return TRUE if this expression is no longer needed.
 */
static BOOL
find_duplicate_op (BB *bb,
                   OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo,
                   EBO_TN_INFO **actual_tninfo)
/* -----------------------------------------------------------------------
 * Requires: 
 * Returns TRUE if the operands of each OP are identical.
 * -----------------------------------------------------------------------
 */
{
  INT opcount;
  INT opndnum;
  INT hash_value = 0;
  INT hash_search_length = 0;
  EBO_OP_INFO *opinfo;
  BOOL hash_op_matches = FALSE;

  if (op == NULL) return FALSE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter find_duplicate_op\n",EBO_trace_pfx);
  }
  
  opcount = OP_opnds(op);

  if (OP_memory(op)) return FALSE;

  if (OP_results(op)!=1) return FALSE;

#ifdef TARG_XTENSA
  /* Two asms or two spadjusts potentially expand to different
     sequences, so don't try to match duplicates. */
  if ((OP_code(op) == TOP_asm) || (OP_code(op) == TOP_spadjust))
    return FALSE;
#endif
   
 /* Compute a hash value for the OP. */
  hash_value = EBO_hash_op( op, opnd_tninfo);

  if (EBO_Trace_Hash_Search) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sLook for duplicates at hash value %d of\n\t",EBO_trace_pfx,hash_value);
    Print_OP_No_SrcLine(op);
  }

  opinfo = (opcount == 0) ? NULL : EBO_opinfo_table[hash_value];

  while (opinfo) {
    OP *pred_op = opinfo->in_op;

    hash_search_length++;
    hash_op_matches =    OP_results(op) == OP_results(pred_op)
		      && OP_opnds(op) == OP_opnds(pred_op)
		      && OP_code(op) == OP_code(pred_op);

    if (hash_op_matches) {

      for (opndnum = 0; opndnum < opcount; opndnum++) {
        if (OP_has_predicate(op) && (opndnum == OP_PREDICATE_OPND)) {
         /* Check predicates later. */
          continue;
        }
        if (opinfo->optimal_opnd[opndnum] == opnd_tninfo[opndnum]) {
          if (!TN_Is_Constant(opnd_tn[opndnum])) {
           /* If operands are not constants and the tninfo_entries match,
              then the values represented must also be identical. */
            continue;
          }

         /* Items that are constant (i.e. have a NULL tninfo_entry pointer)
            must be checked to verify that the constants are the same.
            Note that there are several "reasonable" combinations that
            can come up:
              1. The constants have identical TNs.
              2. The OPs have identical TNs and the predecessor hasn't changed.
              3. The TNs have been resolved to the same constant, but the
                 operand of the OP (for some reason) could not be changed to
                 reference a constant.
         */
          if (/* case 1 */(opnd_tn[opndnum] == OP_opnd(pred_op, opndnum)) ||
              /* case 2 */((OP_opnd(op, opndnum) == OP_opnd(pred_op, opndnum)) &&
                           EBO_tn_available(bb, opinfo->actual_opnd[opndnum])) ||
              /* case 3 */((opinfo->actual_opnd[opndnum] != NULL) && 
                           (opnd_tn[opndnum] == opinfo->actual_opnd[opndnum]->replacement_tn))) {
            continue;
          }
        }

       /* Operands don't match - get out of inner loop and try next OP. */
        hash_op_matches = FALSE;
        break;
      }

    }

    if (hash_op_matches && 
        (pred_op != NULL)) {
      int resnum;

      for (resnum = 0; resnum < OP_results(op); resnum++) {
       /* All of the results need to be available at this point. */
        if (!EBO_tn_available(bb,opinfo->actual_rslt[resnum])) {

          if (EBO_Trace_Hash_Search) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sExpression match found, but the result[%d] value is not available\n\t",
                           EBO_trace_pfx,resnum);
            Print_OP_No_SrcLine(pred_op);
          }

          hash_op_matches = FALSE;
          break;
        }
      }

    }

    if (in_delay_slot) {
     /* We can't insert or remove items from the delay slot. */
      break;
    }

    if (hash_op_matches && OP_has_predicate(op)) {
     /* Check predicates for safety. */

        if (!EBO_predicate_dominates(OP_opnd(pred_op,OP_PREDICATE_OPND),
                                     opinfo->optimal_opnd[OP_PREDICATE_OPND],
                                     OP_opnd(op,OP_PREDICATE_OPND),
                                     actual_tninfo[OP_PREDICATE_OPND])) {
          hash_op_matches = FALSE;

          if (EBO_Trace_Hash_Search) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sExpression match found, but the predicates do not match\n\t",
                           EBO_trace_pfx);
            Print_OP_No_SrcLine(pred_op);
          }

        }
    }

    if (hash_op_matches) {

      if (EBO_Trace_Hash_Search) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sMatch after searching %d items on expression hash chain %d\n",
                      EBO_trace_pfx,hash_search_length,hash_value);
      }

      if (delete_duplicate_op (op, opnd_tninfo, opinfo)) {
        return TRUE;
      } else {
       /* If we matched once and failed to eliminate it,
          we need to keep both around. */
        break;
      }
    }

  opinfo = opinfo->same;
  }

  if (EBO_Trace_Hash_Search) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sFail after searching %d items on hash chain %d\n",
                  EBO_trace_pfx,hash_search_length,hash_value);
  }

  return FALSE;
}




/* 
 * find_previous_constant
 *
 * For a given expression op, look for a preceeding indentical
 * expressionn and attempt to replace the new one.
 * Return TRUE if this expression is no longer needed.
 */
static BOOL
find_previous_constant (OP *op,
                        EBO_TN_INFO **actual_tninfo)
{
  TN *const_tn = OP_opnd(op,EBO_Copy_Operand(op));
  EBO_TN_INFO *predicate_tninfo = (OP_has_predicate(op)?actual_tninfo[OP_PREDICATE_OPND]:NULL);
  EBO_TN_INFO *check_tninfo;

  if (!TN_is_constant(const_tn)) return FALSE;
  if (const_tn == Zero_TN) return FALSE;
  if (OP_cond_def(op)) return FALSE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter find_previous_constant %d\n",
                  EBO_trace_pfx,(INT32)TN_value(const_tn));
  }

  for (check_tninfo = EBO_last_tninfo;
       check_tninfo != NULL;
       check_tninfo = check_tninfo->prior) {
    if (check_tninfo->replacement_tn == const_tn) {
       /* The asigned register needs to be available at this point. */
      TN *pred_tn = check_tninfo->local_tn;
      OP *pred_op = check_tninfo->in_op;

      if (TN_register_class(OP_result(op, 0)) != TN_register_class(pred_tn)) {
        continue;
      }
      if ((pred_op != NULL) && OP_has_predicate(op) && OP_has_predicate(pred_op)) {
       /* Check predicates for safety. */
        EBO_OP_INFO *opinfo = locate_opinfo_entry(check_tninfo);
        if ((opinfo == NULL) ||
            !EBO_predicate_dominates(OP_opnd(pred_op,OP_PREDICATE_OPND),
                                     opinfo->optimal_opnd[OP_PREDICATE_OPND],
                                     OP_opnd(op,OP_PREDICATE_OPND),
                                     predicate_tninfo)) {
         /* This previous definition is not always available, keep looking. */
          continue;
        }
      }
      if (EBO_tn_available(OP_bb(op),check_tninfo) &&
          (TN_is_rematerializable(pred_tn))) {
          OPS ops = OPS_EMPTY;

          EBO_Exp_COPY((OP_has_predicate(op)?OP_opnd(op,OP_PREDICATE_OPND):NULL),
                       OP_result(op, 0), pred_tn, &ops);
	  if (OPS_first(&ops))
	  {
	    if (EBO_in_loop) {
	      CG_LOOP_Init_Op(OPS_first(&ops));
	      if (OP_has_predicate(op))
	      {
		Set_OP_omega (OPS_first(&ops),
			      OP_PREDICATE_OPND,
			      (predicate_tninfo != NULL)?predicate_tninfo->omega:0);
	      }
	    }
	    
	    OP_srcpos(OPS_first(&ops)) = OP_srcpos(op);
	    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
	  }

          if (EBO_Trace_Optimization) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sAvoid reloading constant ",EBO_trace_pfx);
            Print_TN(const_tn,FALSE);
            fprintf(TFile," into ");
            Print_TN(OP_result(op, 0),FALSE);
            fprintf(TFile,"\n");
          }
        return TRUE;
      }
      return FALSE;
    }
  }

  return FALSE;
}


static BOOL
EBO_opnd_is_continuation (OP *op, INT opndnum)
{
  if (opndnum >= OP_opnds(op))
    return FALSE;
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(OP_code(op));
  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, opndnum);
  return TI_ISA_Valtyp_Is_Continuation(vtype);
}


/* 
 * Iterate through a Basic Block and build EBO_TN_INFO entries.
 */
static BOOL
Find_BB_TNs (BB *bb)
{
  OP *op;
  BOOL no_barriers_encountered = TRUE;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter Find_BB_TNs BB:%d%s\n",
            EBO_trace_pfx,BB_id(bb),EBO_in_peep?" - peep ":" ");
    Print_BB(bb);
  }

 /* Allocate the dynamic arrays for various operand info. The minimum
    size we allocate is large enough for all OPs with a fixed number
    of operands. If we just allocated the size based on the BB OP with the
    greatest number of operands, then a transformation could produce
    an OP with more operands. It is expected that these new OPs will 
    always have fixed operands, which is why we use TI_ISA_Operand_Max()
    (we of course verify this assumption). */
  INT max_opnds = TI_ISA_Operand_Max();
  FOR_ALL_BB_OPs (bb, op) {
    INT nopnds = OP_opnds(op);
    if (nopnds > max_opnds) max_opnds = nopnds;
  }
  TN **opnd_tn = TYPE_ALLOCA_N(TN *, max_opnds);
  EBO_TN_INFO **opnd_tninfo = TYPE_ALLOCA_N(EBO_TN_INFO *, max_opnds);
  EBO_TN_INFO **orig_tninfo = TYPE_ALLOCA_N(EBO_TN_INFO *, max_opnds);

  in_delay_slot = FALSE;

  FOR_ALL_BB_OPs (bb, op) {
    TN *tn;
    INT opndnum;
    INT resnum;
    EBO_TN_INFO *tninfo;
    TN *tn_replace;
    INT num_opnds = OP_opnds(op);
    TN *rslt_tn[TI_ISA_Operand_Max()];
    INT rslt_num[TI_ISA_Operand_Max()];
    BOOL rslt_needs_replacement = FALSE;
    BOOL opnds_constant = TRUE;
    BOOL op_replaced = FALSE;
    BOOL op_is_predicated = OP_has_predicate(op)?TRUE:FALSE;
    TN *op_predicate_tn = NULL;
    EBO_TN_INFO *op_predicate_tninfo = NULL;
    BOOL check_omegas = (EBO_in_loop && _CG_LOOP_info(op))?TRUE:FALSE;

    memset(rslt_tn, 0, sizeof(rslt_tn));
    
    for (opndnum = 0; opndnum < num_opnds; opndnum++) {
      opnd_tn[opndnum] = NULL;
      opnd_tninfo[opndnum] = NULL;
      orig_tninfo[opndnum] = NULL;
    }

   /* The assumption is that this can never occur, but make sure it doesn't! */
    FmtAssert(num_opnds <= max_opnds, ("dynamic array allocation was too small!"));

    if (CGTARG_Is_OP_Barrier(op) || OP_access_reg_bank(op)) {
      if (OP_code(op)==TOP_entry && BB_first_op(bb)==op) {
       /* entry at the beginning of a block is ok */
        if (EBO_Trace_Execution) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sFound entry and allow ebo to proceed in BB:%d\t",
                  EBO_trace_pfx,BB_id(OP_bb(op)));
          Print_OP_No_SrcLine(op);
        }  
      } else if (Special_Sequence(op, opnd_tn, opnd_tninfo, orig_tninfo)) {
       /* We were able to restrict propagation of the specific registers. */
        if (EBO_Trace_Execution) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sWe were able to restrict propagation of the specific registers in BB:%d\t",
                  EBO_trace_pfx,BB_id(OP_bb(op)));
          Print_OP_No_SrcLine(op);
        }  
      } else {
       /* We could not identify the specific registers involved. */
        if (EBO_Trace_Execution) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%sFind_BB_TNs Barrier OP encountered\t",EBO_trace_pfx);
          Print_OP_No_SrcLine(op);
        }
        no_barriers_encountered = FALSE;
      }
    }

    if ((num_opnds == 0) && (OP_results(op) == 0)) {
      // Need to handle TOP_asm's clobber list
      if (OP_code(op) == TOP_asm) {
        ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op);

        ISA_REGCLASS rc;
        FOR_ALL_ISA_REGCLASS( rc ) {
          REGISTER_SET asm_regs = ASM_OP_clobber_set(asm_info)[rc];
          for (REGISTER reg = REGISTER_SET_Choose(asm_regs);
               reg != REGISTER_UNDEFINED;
               reg = REGISTER_SET_Choose_Next(asm_regs, reg)) {
            regtn_info_def(bb, op, rc, reg);
          }
        }
      }
      continue;
    }

    if (EBO_Trace_Data_Flow) {
      fprintf(TFile,"%sProcess OP\n\t",EBO_trace_pfx); Print_OP_No_SrcLine(op);
    }

    /* Process all the operand TNs. */
    for (opndnum = 0; opndnum < num_opnds; opndnum++) {
      BOOL replace_result = FALSE;
      mUINT8 operand_omega = 0;

      tn = OP_opnd(op, opndnum);
      tninfo = NULL;
      tn_replace = NULL;
      opnd_tn[opndnum] = tn;
      opnd_tninfo[opndnum] = NULL;
      orig_tninfo[opndnum] = NULL;
      operand_omega = check_omegas ? OP_omega(op,opndnum) : 0;

      if (tn == NULL || TN_is_constant(tn) || TN_is_label(tn)) {
        continue;
      }

      if (tn != True_TN) {
        tninfo = tn_info_use (bb, op, tn, op_predicate_tn, op_predicate_tninfo, operand_omega);
        orig_tninfo[opndnum] = tninfo;
        tn_replace = tninfo->replacement_tn;
      }

      BOOL Will_def_tn_twice = FALSE;
      if (OP_same_res(op)) {
        INT same_res_idx = Op_Sameres_Result(op, opndnum);
        replace_result = (same_res_idx != -1);
        if (replace_result) {
          rslt_num[opndnum] = same_res_idx;
        }
        if (replace_result && tn_replace) {
          INT i;
          for (i=0; i < OP_results(op); i++) {
            if ( (i != same_res_idx) && 
                 tn_registers_identical(tn_replace, OP_result(op,i)) ) {
              Will_def_tn_twice = TRUE;
              break;
            }
          }
        }
      }

      if ( Will_def_tn_twice ) {
        continue;
      }

      if ((tn_replace != NULL) &&
          (!TN_is_asm_reg(tn)) &&
          (TN_Is_Constant(tn_replace) ||
           EBO_tn_available(bb,tninfo->replacement_tninfo) ||
           ((tn_registers_identical(tn, tn_replace)) && !check_omegas)) &&
          (TN_Is_Constant(tn_replace) ||
           ((tninfo->replacement_tninfo != NULL) &&
            (tninfo->replacement_tninfo->in_bb == bb)) ||
           ((has_assigned_reg(tn) == has_assigned_reg(tn_replace)) &&
            (EBO_in_peep || (!BB_reg_alloc(bb) && !TN_is_dedicated(tn_replace))))) &&
          (!(TN_is_xtbool(tn) && TN_is_dedicated(tn) && EBO_in_peep))) {
       /* The original TN can be "logically" replaced with another TN. */

        if (EBO_Trace_Data_Flow) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile, "%sin BB:%d propagate value for opnd[%d] ",
                  EBO_trace_pfx, BB_id(OP_bb(op)),opndnum);
          Print_TN(tn, FALSE);
          fprintf(TFile," with ");
          Print_TN(tn_replace, FALSE);
          fprintf(TFile,"\n");
        }

        TN *old_tn = tn;
        tn = tninfo->replacement_tn;
        tninfo = tninfo->replacement_tninfo;
	BOOL can_replace = TRUE;
	if (OP_code(op) == TOP_asm) {
	  ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op);
	  for (INT i=0; i<OP_results(op); i++) {
	    if (ASM_OP_result_clobber(asm_info)[i] &&
		tn_registers_identical(tn_replace, OP_result(op,i))) {
	      can_replace = FALSE;
	      break;
	    }
	  }
	}

#ifdef TARG_XTENSA
	/* Gack! What's all that gook? We don't want to physically
	   replace TNs since that can lead to cases like this:

	   We want to avoid this:

	   t1 = t2
	   t3 = t1 op t4
	   end of block, t1, t2, t3 live

	   being changed by ebo into this:

	   t1 = t2
	   t3 = t2 op t4
	   end of block, t1, t2, t3 live

	   because now t1 and t2 interfere (maybe GRA handles this and
	   still preferences t1 and t2, but I doubt it).

	   If 'EBO_Copy_Prop' is TRUE, we use the sandalone copy
	   propagater (ebo_cprop.cxx) for all passes except
	   EBO_in_peep. */
#endif
        if ((!EBO_Copy_Prop || EBO_in_peep) &&
	    !TN_is_constant(tn) &&
	    can_replace &&
            (!OP_store(op) ||
             (TI_TOP_Find_Operand_Use(OP_code(op),OU_storeval)>=0 &&
              opndnum != TI_TOP_Find_Operand_Use(OP_code(op),OU_storeval)) ||
             !TN_has_spill(old_tn)) &&
            (!TN_save_reg(tn)) &&
            (!replace_result || tn_registers_identical(old_tn, tn_replace)) &&
            (EBO_in_peep ||
             (has_assigned_reg(old_tn) == has_assigned_reg(tn_replace)) ||
             (!OP_copy(op))) &&
            (EBO_in_peep ||
             !TN_is_gra_homeable(tn_replace) ||
             (tninfo->in_bb == bb)) &&
            (TN_register_class(old_tn) == TN_register_class(tn_replace)) &&
            (!has_assigned_reg(old_tn) || OP_code(op)==TOP_asm ||
             (TI_ISA_Valtyp_Regsubclass(TI_ISA_Op_Operand(TI_ISA_Operand_Info(OP_code(op)),opndnum)) == ISA_REGSUBCLASS_UNDEFINED) ||
 	     (has_assigned_reg(tn_replace) &&
 		(REGISTER_SET_MemberP(REGISTER_SUBCLASS_members(TI_ISA_Valtyp_Regsubclass(TI_ISA_Op_Operand(TI_ISA_Operand_Info(OP_code(op)),opndnum))), TN_register(tn_replace))))) &&
            (TN_size(old_tn) <= TN_size(tn_replace)) &&
            (TN_is_float(old_tn) == TN_is_float(tn_replace)) &&
            (TN_is_fpu_int(old_tn) == TN_is_fpu_int(tn_replace)) &&
            ((OP_results(op) == 0) ||
             !OP_uniq_res(op) ||
             !tn_registers_identical(tn, OP_result(op,0))) &&
	    !(has_assigned_reg(tn_replace) && 
	      (EBO_opnd_is_continuation(op, opndnum) ||
	       EBO_opnd_is_continuation(op, opndnum+1)))) {
	  /* The original TN can be "physically" replaced with another TN. */
         /* Put the new TN in the expression,           */
         /* decrement the use count of the previous TN, */
         /* increment the use count of the new TN.      */

          if (EBO_Trace_Optimization) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, "%sin BB:%d replace opnd[%d] ",
                    EBO_trace_pfx, BB_id(OP_bb(op)),opndnum);
            Print_TN(OP_opnd(op, opndnum), FALSE);
            fprintf(TFile," with ");
            Print_TN(tn_replace, FALSE);
            if (!TN_Is_Constant(tn_replace) &&
                (tninfo != NULL) &&
                (tninfo->in_bb != NULL)) {
              fprintf(TFile," from BB:%d",BB_id(tninfo->in_bb));
            }
            fprintf(TFile,"\n");
          }

          dec_ref_count(orig_tninfo[opndnum]);
          Set_OP_opnd(op, opndnum, tn);
          if (check_omegas) {
            Set_OP_omega (op, opndnum, (tninfo != NULL) ? tninfo->omega : 0);
          }

          if (tninfo != NULL) {
            inc_ref_count(tninfo);
          }

          if (replace_result) {
           /* This use is also the new result. */
            rslt_tn[opndnum] = tn;
	    rslt_needs_replacement = TRUE;
          }

         /* Update information about the actual expression. */
          orig_tninfo[opndnum] = tninfo;
	  rerun_ebo = TRUE;
        } /* replace the operand with another TN. */
      }

      opnd_tn[opndnum] = tn;
      opnd_tninfo[opndnum] = tninfo;
      if (!TN_Is_Constant(tn) &&
          (!op_is_predicated || (opndnum != OP_PREDICATE_OPND))) {
        opnds_constant = FALSE;
      }
      if (op_is_predicated && (opndnum == OP_PREDICATE_OPND)) {
        if ((tn == Zero_TN) && !OP_xfer(op)) {
         /* The instruction will not be executed - it can be deleted!
            However, Branch instructions should go through
            Resolve_Conditional_Branch so that links between blocks can be updated. */
          op_replaced = Fold_Constant_Expression (op, opnd_tn, opnd_tninfo);
          num_opnds = opndnum + 1;

          if (EBO_Trace_Optimization) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile, "%sin BB:%d Op can not execute with 0 predicate\n",
                    EBO_trace_pfx, BB_id(OP_bb(op)));
          }
          if (op_replaced) break;
        }

        op_predicate_tn = tn;
        op_predicate_tninfo = tninfo;
      }

    } /* End: Process all the operand TNs. */

    if (OP_memory(op)) {
      if (!op_replaced &&
          OP_same_res(op)) {
        op_replaced = EBO_Fix_Same_Res_Op (op, opnd_tn, opnd_tninfo);
      }
      if (!op_replaced) {
        op_replaced = Special_Sequence ( op, opnd_tn, opnd_tninfo, orig_tninfo);
      }
      if (!op_replaced &&
          no_barriers_encountered) {
        op_replaced = find_duplicate_mem_op (bb, op, opnd_tn, opnd_tninfo, orig_tninfo);
      }
    } else if (OP_effectively_copy(op)) {
      if (!op_replaced &&
          opnds_constant && !in_delay_slot) {
#ifdef TARG_XTENSA
	/* We don't want to change immediate moves into reg-reg copies
           during the first pass of EBO performed in the
           compilation. There are some local immediates that get
           cleaned up during this pass, and we want to avoid extending
           their liveranges. Also, after GRA (i.e. EBO_in_peep) we
           don't want to ever change immediate moves to reg-reg
           copies. */
	if (!EBO_in_peep && (!EBO_in_pre || (ebo_pass > 1)))
	  op_replaced = find_previous_constant(op, opnd_tninfo);
#endif
      }
      if (!op_replaced) {
        op_replaced = Special_Sequence (op, opnd_tn, opnd_tninfo, orig_tninfo);
      }
    } else if (!op_replaced &&
               !OP_effectively_copy(op) &&
               (!OP_glue(op) || EBO_in_peep) &&
               !OP_side_effects(op) &&
               !OP_access_reg_bank(op)) {
      if (!in_delay_slot) {
	/* We can attempt to evaluate 'op' at compile time if all
           operands are constant. TENSILICA: This was "num_opnds > 1"
           but we have 1 operand instructions we would like to
           evaluate here (e.g. beqz). */
        if (opnds_constant && (num_opnds > 0)) {
          if (OP_xfer(op)) {
           /* If we remove a conditional branch and alter the flow, we
              may have created dead code that could cause later processing
              to get into trouble. This needs to be looked into. */
            op_replaced = Resolve_Conditional_Branch (op, opnd_tn);
            rerun_cflow |= op_replaced;
          } else if (OP_results(op) >= 1) {
            op_replaced = Fold_Constant_Expression (op, opnd_tn, opnd_tninfo);
          }
        }
	/* Ops with all constant operands are handled above. Here we
           take care of ops with more than 1 operand where at least
           one of those operands is constant. */
	else if (num_opnds > 1) {
	  if (OP_xfer(op)) {
	    op_replaced = Resolve_Static_Branch(op, opnd_tn);
	    rerun_cflow |= op_replaced;
	  }
	  /* TENSILICA: Why only for op's with results? e.g. we want
	     to create immediate versions of branches. */
          if (!op_replaced /* OP_results(op) > 0 */) {
            INT o2_idx = TI_TOP_Find_Operand_Use(OP_code(op),OU_opnd2);
            INT o1_idx = TI_TOP_Find_Operand_Use(OP_code(op),OU_opnd1);

            if (OP_same_res(op)) {
              op_replaced = EBO_Fix_Same_Res_Op (op, opnd_tn, opnd_tninfo);
            }
            if (o2_idx >= 0) {
              tn = opnd_tn[o2_idx];
              if (!op_replaced &&
                  (tn != NULL) &&
                  TN_Is_Constant(tn) && TN_Has_Value(tn)) {
                op_replaced = Constant_Operand1 (op, opnd_tn, opnd_tninfo, o1_idx, o2_idx);
              }
            }
            if (o1_idx >= 0) {
              tn = opnd_tn[o1_idx];
              if (!op_replaced &&
                  (tn != NULL) &&
                  TN_Is_Constant(tn) && TN_Has_Value(tn)) {
                  op_replaced = Constant_Operand0 (op, opnd_tn, opnd_tninfo, o1_idx, o2_idx);
              }
            }
          }
        }
        if (!op_replaced) {
          op_replaced = Special_Sequence (op,opnd_tn,opnd_tninfo,orig_tninfo);
        }
      }

      if (no_barriers_encountered && !op_replaced && !OP_copy(op)) {
       /* Look for redundant OPs. */
        op_replaced = find_duplicate_op(bb, op, opnd_tn, opnd_tninfo, orig_tninfo);
      }

    }

    if (op_replaced) {
      if (EBO_Trace_Optimization) {
        fprintf(TFile,"%sin BB:%d change simplified op to noop - ",EBO_trace_pfx,BB_id(bb));
        Print_OP_No_SrcLine(op);
      }
      remove_uses (num_opnds, orig_tninfo);
      rerun_ebo = TRUE;
      OP_Change_To_Noop(op);
    } else {
     /* Add this OP to the hash table and define all the result TN's. */
      add_to_hash_table (in_delay_slot, op, orig_tninfo, opnd_tninfo);

      FmtAssert(((EBO_last_opinfo != NULL) && (EBO_last_opinfo->in_op == op)),
                  ("OP wasn't added to hash table"));

      if (OP_code(op) == TOP_asm) {
        ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op);

        ISA_REGCLASS rc;
        FOR_ALL_ISA_REGCLASS( rc ) {
          REGISTER_SET asm_regs = ASM_OP_clobber_set(asm_info)[rc];
          for (REGISTER reg = REGISTER_SET_Choose(asm_regs);
               reg != REGISTER_UNDEFINED;
               reg = REGISTER_SET_Choose_Next(asm_regs, reg)) {
            regtn_info_def(bb, op, rc, reg);
          }
        }
      }

     /* Special processing for the result TNs */
      resnum = OP_results(op);
      if (OP_effectively_copy(op) || (resnum && OP_glue(op) && !OP_memory(op))) {
       /* Propagate copy assignements. */
        INT cix = EBO_Copy_Operand(op);
        TN *tnr = OP_result(op, 0);

        if ((tnr != NULL) && (tnr != True_TN) && (tnr != Zero_TN)) {
          tninfo = EBO_last_opinfo->actual_rslt[0];

          if ( (!OP_glue(op) || EBO_in_peep) && (cix >= 0)) {
#ifdef TARG_XTENSA
	    /* Unless GRA is completed, in an entry BB, don't
               propagate copies sourcing a dedicated register, and in
               an exit block don't propagate a copy targeting a
               dedicated register. We don't want to propagate these
               dedicated registers since that will interfere with
               GRA's ability to separate these copies into temporary
               entry/exit blocks to improve preferencing. */
	    if (EBO_in_peep ||
		((!BB_entry(bb) || !TN_is_dedicated(opnd_tn[cix])) &&
		 (!BB_exit(bb) || !TN_is_dedicated(tnr))))
		/* Don't copy propogate asm registers */
		if (!TN_is_asm_reg(tnr) && !TN_is_asm_reg(opnd_tn[cix])) 
#endif
	    {
	      tninfo->replacement_tn = opnd_tn[cix];
	      tninfo->replacement_tninfo = opnd_tninfo[cix];

	      if (EBO_Trace_Data_Flow) {
                #pragma mips_frequency_hint NEVER
		fprintf(TFile,"%sPropagate Copy of ",EBO_trace_pfx);
		Print_TN(tninfo->replacement_tn,FALSE);
		fprintf(TFile,"[%d] into ",(tninfo->replacement_tninfo != NULL)?tninfo->replacement_tninfo->omega:0);
		Print_TN(tnr,FALSE); fprintf(TFile,"\n");
	      }
	    }
          }
        }

#ifndef TARG_XTENSA
        if ((resnum == 2) && ((tnr=OP_result(op,1)) != NULL) && (tnr != True_TN)  && (tnr != Zero_TN)) {
         /* This logic must be in sync with what ebo_special calls a "copy".       
            This instruction must actually be placing a "FALSE" condition in a predicate. */
          tninfo = EBO_last_opinfo->actual_rslt[1];
          tninfo->replacement_tn = Zero_TN;

          if (EBO_Trace_Data_Flow) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%sPropagate Copy of ",EBO_trace_pfx);
            Print_TN(tninfo->replacement_tn,FALSE); fprintf(TFile," into ");
            Print_TN(tnr,FALSE); fprintf(TFile,"\n");
          }
        }
#endif

      }
      else if (rslt_needs_replacement)
      {
	for (UINT oidx = 0; oidx < TI_ISA_Operand_Max(); oidx++)
	{
	  if (rslt_tn[oidx] != NULL)
	  {
	    /* result tn needs to be replaced. */
	    TN *tnr = OP_result(op, rslt_num[oidx]);
	    tninfo = EBO_last_opinfo->actual_rslt[rslt_num[oidx]];

	    /* This is subtle - yes we do want the replacement_tninfo
	       entry to point to the tninfo entry we just
	       created. Yes, it does create a circular link in the
	       chain. Code that searches the chain will need to be
	       aware of this. */
	    tninfo->replacement_tn = rslt_tn[oidx];
	    tninfo->replacement_tninfo = tninfo;
	    Set_OP_result (op, rslt_num[oidx], rslt_tn[oidx]);

	    if (EBO_Trace_Data_Flow) {
              #pragma mips_frequency_hint NEVER
	      fprintf(TFile,"%sReplace result[%d] tn ",EBO_trace_pfx,rslt_num[oidx]);
	      Print_TN(tnr,FALSE); fprintf(TFile," with ");
	      Print_TN(rslt_tn[oidx], FALSE); fprintf(TFile,"\n");
	    }
	  }
	}
      }
    }

    if (TI_PROC_Property_Set(PROP_has_branch_delay_slot))
      in_delay_slot = OP_xfer(op);
  }

  return no_barriers_encountered;
}
  


// =======================================================================
// Is_BB_Empty
// 
// Check if a basic block has any executable instructions other than
// possibly an unconditional jump. Return TRUE if the block is empty.
// =======================================================================
static BOOL
Is_BB_Empty (BB *bb)
{
  for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op))
  {
    if (OP_jump(op) && !OP_ijump(op))
      break;
    
    if (OP_Real_Ops(op) != 0)
      return FALSE;
  }
  return TRUE;
}



static
void EBO_Remove_Unused_Ops (BB *bb, BOOL BB_has_barrier)
/* -----------------------------------------------------------------------
 * -----------------------------------------------------------------------
 */
{
  EBO_OP_INFO *opinfo;
  EBO_TN_INFO *tninfo;
  TN *tn;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter EBO_Remove_Unused_Ops in BB:%d\n",EBO_trace_pfx,BB_id(bb));
    tn_info_table_dump();
  }

  if (EBO_first_opinfo == NULL) goto scan;

  for (opinfo = EBO_last_opinfo; opinfo != NULL; opinfo = opinfo->prior) {
    INT rslt_count = 0;
    INT idx;
    OP *op = opinfo->in_op;

    if (op == NULL) continue;

    if (OP_bb(op) != bb) {
      if (EBO_Trace_Block_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"Stop looking for unused ops, next op is in BB:%d\n",
                OP_bb(op) ? BB_id(OP_bb(op)) : -1);
        Print_OP_No_SrcLine(op);
      }
      break;  /* get out of  loop over opinfo entries. */
    }

    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,"%sConsider removing OP:\n\t",
              EBO_trace_pfx);
      Print_OP_No_SrcLine(op);
    }

    rslt_count = OP_results(op);
    if (rslt_count == 0) goto op_is_needed;
    if (op_is_needed_globally(op)) goto op_is_needed;
    if ( BB_has_barrier && 
         ( OP_memory(op) || OP_extern_effects(op)) ) {
      goto op_is_needed;
    }

   /* Check that all the result operands can be safely removed. */
    for (idx = 0; idx < rslt_count; idx++) {
      tninfo = opinfo->actual_rslt[idx];

     /* A couple of safety checks. */
      if (tninfo == NULL) continue;
      if (tninfo->in_bb != bb) goto op_is_needed;
      if (tninfo->in_op == NULL) goto op_is_needed;
      tn = tninfo->local_tn;

      if (EBO_Trace_Data_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sTry to remove definition of entry[%d] ",
                EBO_trace_pfx,tninfo->sequence_num);
        Print_TN(tn,FALSE);
        fprintf(TFile,"\n");
      }

     /* Zero_TN or True_TN for a result is a no-op. */
      if (tn == Zero_TN) continue;
      if (tn == True_TN) continue;

     /* Copies to and from the same register are not needed. */
      if (EBO_in_peep &&
          OP_effectively_copy(op) &&
          has_assigned_reg(tn) &&
          (EBO_Copy_Operand(op) >= 0) &&
          has_assigned_reg(OP_opnd(op,EBO_Copy_Operand(op))) &&
          (tn_registers_identical(tn, OP_opnd(op,EBO_Copy_Operand(op))))) {
        INT cpo = EBO_Copy_Operand(op);

       /* We may be able to get rid of the copy, but be
          sure that the TN is marked live into this block. */
        if ((opinfo->actual_opnd[cpo] != NULL) &&
            (bb != opinfo->actual_opnd[cpo]->in_bb)) {
          mark_tn_live_into_BB (tn, bb, opinfo->actual_opnd[cpo]->in_bb);
        }

       /* Propagate use count for this TN to it's input TN. */
        if (tninfo->same != NULL) {
          tninfo->same->reference_count += tninfo->reference_count;
        }

        if (!tninfo->redefined_before_block_end &&
            (tninfo->same != NULL) &&
            (tninfo->same->in_bb == bb)) {
         /* Removing the copy causes the previous definition
            of the TN (or reg) to reach the end of the block. */
          tninfo->same->redefined_before_block_end = FALSE;
        }
        goto can_be_removed;
      }

     /* There must be no direct references to the TN. */
      if (tninfo->reference_count != 0) goto op_is_needed;
      if (OP_has_implicit_interactions(tninfo->in_op))
	goto op_is_needed;

     /* Check for indirect and global references.   */
      if (!tninfo->redefined_before_block_end &&
          TN_live_out_of(tn, tninfo->in_bb)) goto op_is_needed;

      if (TN_is_pfs_reg(tn)) goto op_is_needed;
      if (TN_is_lc_reg(tn)) goto op_is_needed;
      if (TN_is_ec_reg(tn)) goto op_is_needed;
    }

   /* None of the results are needed. */
    if (opinfo->op_must_not_be_removed) goto op_is_needed;
    if (OP_store(op)) goto op_is_needed;

can_be_removed:

    remove_op (opinfo);

    if (EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "%sin BB:%d removing    ",
              EBO_trace_pfx, BB_id(bb));
      Print_OP_No_SrcLine(op);
    }

    if (opinfo->in_delay_slot) {
      OP_Change_To_Noop(op);
    } else {
      BB_Remove_Op(bb, op);
      rerun_ebo = TRUE;
      if (Is_BB_Empty(bb))
	rerun_cflow = TRUE;
    }
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;

   /* Propagate "reaches block end" information. */
    for (idx = 0; idx < rslt_count; idx++) {
      tninfo = opinfo->actual_rslt[idx];
      if ((tninfo != NULL) &&
          tninfo->redefined_before_block_end &&
          (tninfo->same != NULL) &&
          (tninfo->same->in_bb == bb)) {
        EBO_TN_INFO *next_tninfo = tninfo->same;
        next_tninfo->redefined_before_block_end = TRUE;
      }
    }

    continue;

op_is_needed:

    if (OP_has_predicate(op))
    {
      /* Predicate resolution may not have been possible, and */
      /* preceeding definitions of the same result TN's may   */
      /* need to be saved.                                    */
      for (idx = 0; idx < rslt_count; idx++) {
	tninfo = opinfo->actual_rslt[idx];
	if ((tninfo != NULL) &&
	    (tninfo->local_tn != NULL) &&
	    (tninfo->same != NULL)) {
	  EBO_TN_INFO *next_tninfo = tninfo->same;
	  
	  while (next_tninfo != NULL) {
	    if ((next_tninfo->in_op != NULL) &&
		(!EBO_predicate_dominates((tninfo->predicate_tninfo != NULL)?tninfo->predicate_tninfo->local_tn:True_TN,
					  tninfo->predicate_tninfo,
					  (next_tninfo->predicate_tninfo != NULL)?
					  next_tninfo->predicate_tninfo->local_tn:True_TN,
					  next_tninfo->predicate_tninfo))) {
	      
	      /* A store into an unresolved predicate is a potential problem     */
	      /* because the last store might not completely redefine the first. */
	      /* The predicates could be completely independant.  But we      */
	      /* don't know how to check for that, currently.                 */
	      
	      /* Stop searching and preserve the preceeding definition. */
	      EBO_OP_INFO *opinfo = locate_opinfo_entry(next_tninfo);
	      if (opinfo != NULL) {
		opinfo->op_must_not_be_removed = TRUE;
	      } else {
		/* Couldn't find the opinfo entry.  Make sure that the TN has
		   a use count so that the defining OP entry will not be deleted. */
		next_tninfo->reference_count += tninfo->reference_count;
	      }
	      
	      if (EBO_Trace_Data_Flow) {
                #pragma mips_frequency_hint NEVER
		fprintf(TFile,"%sMark(1) same_tn as needed - original [%d]: ",
			EBO_trace_pfx,tninfo->sequence_num);
		Print_TN(tninfo->local_tn,FALSE);
		fprintf(TFile," same as [%d]: ",next_tninfo->sequence_num);
		Print_TN(next_tninfo->local_tn,FALSE);
		fprintf(TFile,"\n");
	      }
	      
	      if (EBO_predicate_dominates((next_tninfo->predicate_tninfo != NULL)?
					  next_tninfo->predicate_tninfo->local_tn:True_TN,
					  next_tninfo->predicate_tninfo,
					  (tninfo->predicate_tninfo != NULL)?tninfo->predicate_tninfo->local_tn:True_TN,
					  tninfo->predicate_tninfo)) {
		/* If the first store dominates the last, there can be no       */
		/* preceeding definitions that are partially redefined by       */
		/* the last store.  We can stop searching for other dominators. */
		break;
	      }
	    }
	    next_tninfo = next_tninfo->same;
	  }
	  
	}
      }

      /* Predicate resolution may not have been possible, and */
      /* preceeding inputs to this instruction may be defined */
      /* within the current extended block, and must be saved.*/
      for (idx = 0; idx < OP_opnds(op); idx++) {
	tninfo = opinfo->actual_opnd[idx];
	if ((tninfo != NULL) &&
	    (tninfo->local_tn != NULL) &&
	    (tninfo->same != NULL)) {
	  EBO_TN_INFO *next_tninfo = tninfo->same;
	  
	  while (next_tninfo != NULL) {
	    if ((next_tninfo->in_op != NULL) &&
		(next_tninfo->omega == tninfo->omega)) {
	      if (EBO_predicate_dominates((next_tninfo->predicate_tninfo != NULL)?
					  next_tninfo->predicate_tninfo->local_tn:True_TN,
					  next_tninfo->predicate_tninfo,
					  (tninfo->predicate_tninfo != NULL)?tninfo->predicate_tninfo->local_tn:True_TN,
					  tninfo->predicate_tninfo)) {
		/* This predicate dominates the OP we need to save. It's   */
		/* use count should be sufficiant to cause it to be saved. */
		/* We can stop searching for other dominators.             */
		break;
	      } else {
		/* A store into an unresolved predicate is a potential problem. */
		/* The predicates could be completely independant.  But we      */
		/* don't know how to check for that, currently.                 */
		
		/* Stop searching and preserve the preceeding definition. */
		EBO_OP_INFO *opinfo = locate_opinfo_entry(next_tninfo);
		if (opinfo != NULL) {
		  opinfo->op_must_not_be_removed = TRUE;
		} else {
		  /* Couldn't find the opinfo entry.  Make sure that the TN has
		     a use count so that the defining OP entry will not be deleted. */
		  next_tninfo->reference_count += tninfo->reference_count;
		}

		if (EBO_Trace_Data_Flow) {
                  #pragma mips_frequency_hint NEVER
		  fprintf(TFile,"%sMark(2) same_tn as needed - original [%d]: ",
			  EBO_trace_pfx,tninfo->sequence_num);
		  Print_TN(tninfo->local_tn,FALSE);
		  fprintf(TFile," same as [%d]: ",next_tninfo->sequence_num);
		  Print_TN(next_tninfo->local_tn,FALSE);
		  fprintf(TFile,"\n");
		}
		
	      }
	    }
	    next_tninfo = next_tninfo->same;
	  }
	  
	}
      }
    }


   /* Check for newly created references that cross a block
      boundary.  If one is found, mark it as global and carry
      registers in/out of blocks. */
    for (idx = 0; idx < OP_opnds(op); idx++) {
      tninfo = opinfo->actual_opnd[idx];
      if ((tninfo != NULL) &&
          (bb != tninfo->in_bb)) {
        mark_tn_live_into_BB (tninfo->local_tn, bb, tninfo->in_bb);
      }
    }

  } /* end: for each opinfo entry */

scan:
  /* Make a quick scan of the OPS in a BB and remove noops. Also
     convert instructions that are equivalent to a move into actual
     move instructions. */
  {
    OP *op;
    OP * next_op = NULL;
    in_delay_slot = FALSE;
    TOP noop_top = CGTARG_Noop_Top();

    for (op = BB_first_op(bb); op != NULL; op = next_op) {
      next_op = OP_next(op);
      if (   (OP_code(op) == noop_top || OP_code(op) == TOP_noop) 
	  && !in_delay_slot)
      {
        if (EBO_Trace_Optimization) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile, "%sin BB:%d removing noop    ",
                  EBO_trace_pfx, BB_id(bb));
          Print_OP_No_SrcLine(op);
        }

        BB_Remove_Op(bb, op);
	rerun_ebo = TRUE;
	if (Is_BB_Empty(bb))
	  rerun_cflow = TRUE;
      }
      else if (TI_PROC_Property_Set(PROP_has_branch_delay_slot))
      {
	if (in_delay_slot && OP_code(op) == TOP_noop) {
	   // ugly hack for mips
	   OP_Change_Opcode(op, noop_top);
	}
        in_delay_slot = OP_xfer(op);
      }
      else if (OP_effectively_copy(op) && OP_code(op)<TOP_count)
      {
	TOP actual_mov = CGTARG_Copy_Op(4, FALSE);
	TN *copy_src = OP_opnd(op, EBO_Copy_Operand(op));

	if ((OP_code(op) != actual_mov) && TN_is_register(copy_src))
	{
	  if (TN_register_class(copy_src)==TI_ISA_Regclass_Integer()) {
	    if (EBO_Trace_Optimization)
	    {
              #pragma mips_frequency_hint NEVER
	      fprintf(TFile, "%sin BB:%d converting to actual move    ",
		    EBO_trace_pfx, BB_id(bb));
	      Print_OP_No_SrcLine(op);
	    }
	  
	    OP *new_op = Mk_OP(actual_mov, OP_result(op, 0), copy_src);
	    bool is_identity = false;
	    if (OP_result(new_op,0)==OP_opnd(new_op,0))
	      is_identity = true;
	    INT omega=0;
	    if (Is_CG_LOOP_Op(op)) {

	      INT opnd;

	      CG_LOOP_Init_Op(new_op);

	      opnd = EBO_Copy_Operand(op);
	      omega = OP_omega(op,opnd);

	      opnd = EBO_Copy_Operand(new_op);
	      Set_OP_omega(new_op, opnd, omega);
	    }
	    if (is_identity==false || omega!=0)
	      BB_Insert_Op_After(bb, op, new_op);
	    else {
	      if (EBO_Trace_Optimization)
	      {
                #pragma mips_frequency_hint NEVER
	        fprintf(TFile, "%sin BB:%d removing identity move    ",
		    EBO_trace_pfx, BB_id(bb));
	        Print_OP_No_SrcLine(new_op);
	      }
	    }
	    BB_Remove_Op(bb, op);
	  } else {
	    if (EBO_Trace_Optimization)
	    {
              #pragma mips_frequency_hint NEVER
	      fprintf(TFile, "%sin BB:%d non integer register copy not converting to actual move    ",
		    EBO_trace_pfx, BB_id(bb));
	      Print_OP_No_SrcLine(op);
	    }
	  }
	}
      }
    }

  }

  return;
}
  
/*
 * Consider each pair of constants in order within a BB 'bb', say (c1,c2),
 * if c2 can be converted to addi/addmi based on c1, do so.
 * For instance:
 *        const16hi r1 100000             // c1 = 100000
 *        const16lo r1, r1, 100000
 *        
 *        const16hi r2 100001             // c2 = 100001
 *        const16lo r2, r2, 100001
 *
 *  ===>
 *        const16hi r1 100000
 *        const16lo r1, r1, 100000
 *
 *        addi r2, r1, 1
 *        (of course, r1 is still available at this point)
 *
 *  This code is invoked if EBO_in_peep is true, that is, after
 *  register allocation and before the final IGLS.
 *
 *  Return:
 *      true:  optimization happened (some redundant const16 reduced)
 *      false: nothing happend.
 */
static bool EBO_reduce_redundant_const16(BB *bb)
{
  bool ret_val = false;

  /* only do it when expanding load_const into const16 */
  if (!xt_prefer_const16) return ret_val;

  /* only consider two load_consts that are in the same BB
   * and basically are next to each other. We can have a general
   * approach if it is worth it
   */
  OP *op;
  OP *const16lo_first=NULL;
  TN *const16lo_first_res;
  TN *const16lo_first_tn;

  /**
     const16hi_second is used only if both 1st and 2nd consts share 
     the same result register tn as follows:

        1  const16hi r1 const_1   
           const16lo r1 r1 const_1

        2  const16hi r1 const_2
           const16lo r1 r1 const_2

      Normally, the second const16hi will kill the first one, so no conversion
      for second const is performed. To get around this, const16hi_second is
      used so the above pair <1,2> is till considered as a candidate for the conversion.
   */
  OP *const16hi_second=NULL;

  FOR_ALL_BB_OPs(bb, op)
  {

    if ( !const16lo_first && (OP_code(op) == TOP_const16lo) ) {
      // found the first const16lo
      const16lo_first_tn = OP_opnd(op, 1);
      if ( TN_has_value(const16lo_first_tn) || 
           TN_is_symbol(const16lo_first_tn) ) {
        const16lo_first = op;
        const16lo_first_res = OP_result(op, 0);
      }
    } else if (const16lo_first) {
      // We have found the first const16, now let's find the second!
      for (OP *next_op, *op1=op; op1; op1 = next_op) {
        next_op = OP_next(op1);
        if ((OP_code(op1) == TOP_const16lo) &&
            ((!const16hi_second) || const16hi_second &&
                 (OP_result(const16hi_second, 0) == OP_result(op1, 0)))) {
          /* either a normal const16lo  OR const16lo whose result reg tn is 
             the same as the first one. 

             Let's try to do the convertion.
           */
          TN *tn = OP_opnd(op1, 1);
          if ( TN_has_value(tn) && TN_has_value(const16lo_first_tn) ||
               TN_is_symbol(tn) && TN_is_symbol(const16lo_first_tn) &&
                (TN_var(tn) == TN_var(const16lo_first_tn)) ) {
            ISA_LITCLASS add_lc;
            TOP top=TOP_UNDEFINED;

            INT64 imm_val;
            if (TN_has_value(tn)) {
              Is_True(TN_has_value(const16lo_first_tn), ("TN must have value"));
              imm_val = TN_value(tn) - TN_value(const16lo_first_tn);
            } else {
              Is_True(TN_is_symbol(tn), ("must be symbol TN"));
              Is_True(TN_is_symbol(const16lo_first_tn), ("must be symbol TN"));
              Is_True(TN_var(tn) == TN_var(const16lo_first_tn), 
                      ("must be the same symbol TN"));
              imm_val = TN_offset(tn) - TN_offset(const16lo_first_tn);
            }

            /* TI_ISA_LC_Value_In_Class() expects imm_val to be INT32,
             * so need to make sure it is the case. 
             */
            if ( (imm_val <= INT32_MAX) && (imm_val >= INT32_MIN)) {
              if ((TI_TOP_Immediate_Operand(TOP_addi, &add_lc) == 1) &&
                  TI_ISA_LC_Value_In_Class(imm_val, add_lc)) {
                top = TOP_addi;
              } else if ((TI_TOP_Immediate_Operand(TOP_addmi, &add_lc) == 1) &&
                         TI_ISA_LC_Value_In_Class(imm_val, add_lc)) {
                top = TOP_addmi;
              }

              if ( top != TOP_UNDEFINED) {
                // The second can be replaced, now do so.
                DEF_KIND kind;
                OP *const16hi = TN_Reaching_Value_At_Op (
                         OP_result(op1, 0), op1, &kind, TRUE);
                if (const16hi) {
                  Is_True ((OP_code(const16hi) == TOP_const16hi),
                           ("const16hi not found for a const16lo"));
                  BB_Remove_Op(OP_bb(const16hi), const16hi);
                }

                OPS *ops = OPS_Create();
                TN  *tnr = OP_result(op1, 0);
                if (imm_val == 0) {
                  Build_OP( TOP_mov_n, tnr, const16lo_first_res, ops);
                } else {
                  Build_OP( top, tnr, const16lo_first_res,
                          Gen_Literal_TN(imm_val, TN_size(tnr)), ops);
                }
                BB_Insert_Ops_After(bb, op1, ops);
                BB_Remove_Op(bb,op1);

                ret_val = true;
              } 
            }
          }
        }

        /* At this time, check if the first const16 will be killed 
           by the current OP 'op1', if so, the first const16 cannot
           be reused and we go on checking the next OP ! (except 
           that when the second const16 uses the same result reg tn,
           as showed in the above case of pair <1,2>).
         */

        // Check if this ASM clobbers the reg of const16lo_first_res
        if ((OP_code(op1) == TOP_asm) &&
            TN_is_register(const16lo_first_res)) {
          ISA_REGCLASS rc = TN_register_class(const16lo_first_res);

          ASM_OP_ANNOT *asm_info = (ASM_OP_ANNOT *) OP_MAP_Get(OP_Asm_Map, op1);
          REGISTER_SET clobbered_regs = ASM_OP_clobber_set(asm_info)[rc];
          if (REGISTER_SET_MemberP(clobbered_regs, TN_register(const16lo_first_res))) {
            // result tn of the first const16 is not available, stop!
            const16lo_first = const16hi_second = NULL;
            break;
          }
        }

        /*
           Note: using OP_Defs_Reg() alone is probably enough because 
                 const16lo_first_res must be a register TN.
         */
        if (OP_Defs_TN(op1, const16lo_first_res) ||
            (TN_is_register(const16lo_first_res) &&
             OP_Defs_Reg(op1, TN_register_class(const16lo_first_res),
                              TN_register(const16lo_first_res)))) {
          if (OP_code(op1) == TOP_const16hi) {
            // for case of pair <1,2>
            if (!const16hi_second) {
              const16hi_second = op1;
            } else {
              // found the third const16hi that shares the same
              // result register tn, quit finding!
              const16lo_first = const16hi_second = NULL;
              break;
            }
          } else {
            // result tn of the first const16 is not available, stop!
            const16lo_first = const16hi_second = NULL;
            break;
          }
        }
      }
    }
  }

  return ret_val;
}
  
  
/* ===================================================================== */

  
/* 
 */
static
void
EBO_Add_BB_to_EB (BB * bb)
{
  EBO_TN_INFO *save_last_tninfo = EBO_last_tninfo;
  EBO_OP_INFO *save_last_opinfo = EBO_last_opinfo;
  BBLIST *succ_list;
  BOOL no_barrier;

  if (EBO_Trace_Execution) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEnter EBO_Add_BB_to_EB BB:%d. It has %d successors\n",
            EBO_trace_pfx,BB_id(bb),BB_succs(bb)?BB_succs_len(bb):0);
  }

  Set_BB_visited(bb);

  /* Invoke it after reg allocation and before final IGLS */
  if (EBO_in_peep) {
    EBO_reduce_redundant_const16(bb);
  }

 /* Add this block to the current Extended Block (EB). */
  no_barrier = Find_BB_TNs (bb);
  if (no_barrier &&
      !CG_localize_tns) {
   /* Walk through the successors, trying to grow the EB. */
    FOR_ALL_BB_SUCCS(bb, succ_list) { 
      BB *succ = BBLIST_item(succ_list);

      if (EBO_Trace_Block_Flow) {
        #pragma mips_frequency_hint NEVER
        fprintf(TFile,"%sCheck successor BB:%d. It has %d preds and has%s been visited.\n",
               EBO_trace_pfx,BB_id(succ),BB_preds_len(succ),BB_visited(succ)?" ":" not");
      }

      if (!BB_call(bb) &&
          (BB_preds_len(succ) == 1) &&
          !BB_visited(succ) &&
          (BB_rid(bb) == BB_rid(succ))) {
        EBO_Add_BB_to_EB (succ);
      }
    }
  }

 /* When we are unable to grow the EB any more, optimize what we have. */
  if (EBO_Trace_Block_Flow) {
    #pragma mips_frequency_hint NEVER
    fprintf(TFile,"%sEBO optimization at BB:%d\n",EBO_trace_pfx,BB_id(bb));
  }

  EBO_Remove_Unused_Ops(bb, !no_barrier);

 /* Remove information about TN's and OP's in this block. */
  backup_tninfo_list(save_last_tninfo);
  backup_opinfo_list(save_last_opinfo);

  return;
}

/* Increase the absolute value of sz by inc. Inc > 0 */
static INT32 AbsInc(INT32 sz, INT32 inc)
{ 
  Is_True(inc > 0, ("inc must be greater than zero"));
  return (sz > 0) ? sz + inc : sz - inc;
}

/* Decrease the absolute value of sz by inc. Inc > 0 */
static INT32 AbsDec(INT32 sz, INT32 inc)
{ 
  Is_True(inc > 0, ("inc must be greater than zero"));
  INT32 t = (sz > 0) ? MAX(0, sz - inc) : MIN(0, sz + inc);
  return t;
  
}

static TOP Get_Converted_Top(OP *op, bool rri8_to_rri12)
{
  TN *imm_tn;

  if (rri8_to_rri12) {
    switch (OP_code(op)) {
    case TOP_beq:
        return TOP_beqz;
    case TOP_bne:
        return TOP_bnez;
    case TOP_bge:
        // condtion is to prevent overflow, see EBO_Select_Branch
        if ( (TN_size(OP_opnd(op, 0)) < 4) &&
             (TN_size(OP_opnd(op, 1)) < 4)) {
          return TOP_bgez;
        } else {
          return TOP_UNDEFINED;
        }
    case TOP_blt:
        // condtion is to prevent overflow, see EBO_Select_Branch
        if ( (TN_size(OP_opnd(op, 0)) < 4) &&
             (TN_size(OP_opnd(op, 1)) < 4)) {
          return TOP_bltz;
        } else {
          return TOP_UNDEFINED;
        }
    case TOP_beqi:
    case TOP_bnei:
        imm_tn = OP_opnd(op, 1);
        Is_True(TN_is_constant(imm_tn) && TN_has_value(imm_tn),
                ("TOP_beqi/TOP_bnei does not have constant immediate"));
        // used addi-beqz/bnez to replace beqi/bnei
        if (TI_ISA_LC_Value_In_Class(-TN_value(imm_tn), LC_simm8) ||
            TI_ISA_LC_Value_In_Class(-TN_value(imm_tn), LC_simm8x256)) {
          if (OP_code(op) == TOP_beqi) {
            return TOP_beqz;
          } else {
            return TOP_bnez;
          }
        } else {
          return TOP_UNDEFINED;
        }
    case TOP_bbci:
        return TOP_beqz;
    case TOP_bbsi:
        return TOP_bnez; 
    default:
      return TOP_UNDEFINED;
    }
  } else {
    switch (OP_code(op)) {
    case TOP_beqz:
      return TOP_beq;
    case TOP_bnez:
      return TOP_bne;
    case TOP_bgez:
        // condtion is to prevent overflow, see EBO_Select_Branch
        if ( (TN_size(OP_opnd(op, 0)) < 4) &&
             (TN_size(OP_opnd(op, 1)) < 4)) {
          return TOP_bge;
        } else {
          return TOP_UNDEFINED;
        }
    case TOP_bltz:
        // condtion is to prevent overflow, see EBO_Select_Branch
        if ( (TN_size(OP_opnd(op, 0)) < 4) &&
             (TN_size(OP_opnd(op, 1)) < 4)) {
          return TOP_blt;
        } else {
          return TOP_UNDEFINED;
        }
    default:
      return TOP_UNDEFINED;
    }
  }
}

static TOP Get_Branch_Density_Top(OP *op)
{
  if (xt_density) {
    switch (OP_code(op)) {
    case TOP_beqz:
      return TOP_beqz_n;
    case TOP_bnez:
      return TOP_bnez_n;
    }
  }
  return TOP_UNDEFINED;
}

/*
 * Since the code layout is not final yet, the exact distance
 * b/w the branch and the target is not known.  So, we use an 
 * estimate. Because of this estimate, the following buffer size
 * are used to make the range of each immediate bigger or smaller,
 * for achieving a conservative decision. (Note both smaller and
 * bigger range can mean 'conservative', it's up to how the range
 * is used.)
 */
#define IMM6_BUF_SIZE  6
#define IMM8_BUF_SIZE  12
#define IMM12_BUF_SIZE 120


/* This function selects a better branch based on branch's target range.
 * For example,  
 *
 *      beq  tn1, tn2, L
 *      ...
 *   L:
 *
 *  =====>
 *      sub tn3, tn1, tn2
 *      beqz tn3, L
 *      ...
 *   L:
 * 
 *  If the distance b/w beq and L is over the range of 8-bit immediate of
 *  beq and is within the range of 12-bit immediate for beqz; then we do
 *  the above optimization. Notice that we cannot do this for blt and bge,
 *  because (tn1 - tn2) may overflow and thus the original code is not
 *  equivalent to the transformed one (if both tn1 and tn2's size is smaller
 *  than 4 bytes, we know there is definitely no overflow. So, only under
 *  the condition (both sizes < 4 bytes) do we perform this transformation).
 *
 *  Since the distance calculated is an estimate of the real distance,
 *  we will also check if this optimization is still valid after GRA,
 *  if not, we undo the optimization (when EBO_in_peep is true).
 *
 *  This function also converts branches to their density form if there
 *  are ones, this might be useful when branches can be bundled.
 *
 *  As requested (Oct, 2005), we add convertion for beqi/bnei here too.
 *     beqi  tn1, c, L
 *     ...
 *   L:
 *
 *   ========>
 *     addi tn2, tn1, -c
 *     beqz tn2, L
 *     ...
 *   L:
 *
 *   if -c is an imm8 of (-128, 127), we will use addi; if -c is
 *   an imm8x256, we use addmi.
 */
static void EBO_Select_Branch(BB *first_bb)
{
  Is_True(EBO_Select_Br, ("EBO_Select_Br must be true"));

  
  BOOL EBO_Trace_Select_Br = Get_Trace(TP_EBO, 0x040);

  BB *bb;
  bool rri8_to_rri12 = EBO_in_acgprep;
  UINT32 nBBs = BB_MAP_idx_max + 2; // possible max number of BBs
  mUINT32 sizeOfBB[nBBs]; 

  if (!rri8_to_rri12) {
    Is_True(EBO_in_peep, ("Must be in EBO_in_peep phase"));
  }

  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    Is_True ((BB_id(bb) < nBBs), ("BB.id out of bound"));
    sizeOfBB[BB_id(bb)] = BB_size(bb);
  }

  clear_bb_flag(first_bb);
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    Set_BB_visited(bb);

    // Skip swp-scheduled BBs
    if (BB_rotating_kernel(bb)) continue;

    OP *xfer_op = BB_xfer_op(bb);
    if (xfer_op == NULL) continue;

    BBLIST *target_edge = BB_succs(bb);
    if (target_edge == NULL) continue;

    BB *target_bb = BBLIST_item(target_edge);
    if (target_bb == BB_next(bb)) {
      // the first succ isn't a target, so try the second
      target_edge = BBLIST_next(target_edge);
      if (target_edge != NULL) {
        target_bb = BBLIST_item(target_edge);
      } else {
        target_bb = NULL;
      }
    }
    if (target_bb == NULL) continue;

    TOP new_xfer_top = Get_Converted_Top(xfer_op, rri8_to_rri12);
    TOP new_density_xfer_top = Get_Branch_Density_Top(xfer_op);
    bool do_density_opt = EBO_in_peep && (new_density_xfer_top != TOP_UNDEFINED);
    bool do_rri8_rri12_opt = EBO_in_acgprep && (new_xfer_top != TOP_UNDEFINED);

    if ( do_rri8_rri12_opt || do_density_opt ) { 
      /* candidate branch */

      // Calculate the distance 
      INT32 dist=0;
      if (target_bb == bb) {
        dist = -sizeOfBB[BB_id(bb)];
      } else if (BB_visited(target_bb)) {
        dist = -sizeOfBB[BB_id(target_bb)];
        for (BB *b=bb; b != target_bb; b = BB_prev(b)) {
          Is_True( (b != NULL), ("BB_prev isn't correct"));
          dist -= sizeOfBB[BB_id(b)];
        }
      } else {
        dist = 0;
        for (BB *b=BB_next(bb); b != target_bb; b = BB_next(b)) {
          Is_True( (b != NULL), ("BB_next isn't correct"));
          dist += sizeOfBB[BB_id(b)];
        }
      }

      // Do convertion optimization between rri8 and rri12
      if (do_rri8_rri12_opt) {
        if (rri8_to_rri12 && 
            (!TI_ISA_LC_Value_In_Class (AbsDec(dist, IMM8_BUF_SIZE), LC_simm8)) &&
            TI_ISA_LC_Value_In_Class (AbsInc(dist, IMM12_BUF_SIZE), LC_simm12)) {

          // From rri8 and rri12
          // If making dist a little smaller, it still cannot fit into simm8;
          // and making dist a little bigger, it still can fit into simm12;
          // it is more likely that dist won't fit into simm8 but fit into
          // simm12.

          if (EBO_Trace_Select_Br) {
            #pragma mips_frequency_hint NEVER
            fprintf(TFile,"%s select_br[acgprep]: xfer OP  ==> br w/ imme12 at BB %d\n",
                    EBO_trace_pfx, BB_id(bb));
          }

          
          OPS *ops = OPS_Create();
          TN *opnd0, *opnd1, *target_tn, *new_tn;
          if (OP_code(xfer_op) == TOP_beqi || OP_code(xfer_op) == TOP_bnei) {
            opnd0 = OP_opnd(xfer_op, 0);
            opnd1 = OP_opnd(xfer_op, 1);
            target_tn = OP_opnd(xfer_op, 2);
            new_tn = Build_TN_Of_Mtype(TN_mtype(opnd0));
            TOP new_add_top = TOP_addi;
            if (!TI_ISA_LC_Value_In_Class(-TN_value(opnd1), LC_simm8) &&
                TI_ISA_LC_Value_In_Class(-TN_value(opnd1), LC_simm8x256)) {
              new_add_top = TOP_addmi;
            } else {
              Is_True(TN_is_constant(opnd1) && 
                    (TI_ISA_LC_Value_In_Class(-TN_value(opnd1), LC_simm8)),
                    ("beqi/bnei's immediate: cannot be used in addi's imm8!"));
            }
            Build_OP (new_add_top, new_tn, opnd0, 
                      Gen_Literal_TN(-TN_value(opnd1), TN_size(opnd1)), ops);

            // Build_OP(new_xfer_top, new_tn, target_tn, ops); 
            OP *new_op = Mk_OP(new_xfer_top, new_tn, target_tn);
            OPS_Append_Op(ops, new_op);
            Set_OP_no_generics(new_op);

            BB_Insert_Ops_After(bb, xfer_op, ops);
            BB_Remove_Op(bb,xfer_op);

          } else if (OP_code(xfer_op) == TOP_bbci || OP_code(xfer_op) == TOP_bbsi) {

            opnd0 = OP_opnd(xfer_op, 0);
            opnd1 = OP_opnd(xfer_op, 1);
            target_tn = OP_opnd(xfer_op, 2);

            TN *tn_const_one = Build_TN_Of_Mtype(MTYPE_I4);
            Set_TN_is_constant(tn_const_one);
            Set_TN_value(tn_const_one, 1);
            Set_TN_has_value(tn_const_one);

            // For big-endian, adjust shift amount
            if (Target_Byte_Sex == BIG_ENDIAN) {
              Is_True( TN_is_constant(opnd1) && TN_has_value(opnd1), 
                       ("bbci/bbsi/s 2nd operand isn't int constant"));
              INT64 val = 31 - TN_value(opnd1);
              opnd1 = Gen_Literal_TN(val, 4);
            }
             
            new_tn = Build_TN_Of_Mtype(MTYPE_U4);
            Build_OP (TOP_extui, new_tn, opnd0, opnd1, tn_const_one, ops);

            // Build_OP(new_xfer_top, new_tn, target_tn, ops);
            OP *new_op = Mk_OP(new_xfer_top, new_tn, target_tn);
            OPS_Append_Op(ops, new_op);
            Set_OP_no_generics(new_op);

            BB_Insert_Ops_After(bb, xfer_op, ops);
            BB_Remove_Op(bb,xfer_op);

          } else {

            opnd0 = OP_opnd(xfer_op, 0);
            opnd1 = OP_opnd(xfer_op, 1);
            target_tn = OP_opnd(xfer_op, 2);
            new_tn = Build_TN_Of_Mtype(TN_mtype(opnd0));
  
            Build_OP (TOP_sub, new_tn, opnd0, opnd1, ops);

            // Build_OP(new_xfer_top, new_tn, target_tn, ops);
            OP *new_op = Mk_OP(new_xfer_top, new_tn, target_tn);
            OPS_Append_Op(ops, new_op);
            Set_OP_no_generics(new_op);

            BB_Insert_Ops_After(bb, xfer_op, ops);
            BB_Remove_Op(bb,xfer_op);
          }
        } else if ((!rri8_to_rri12) &&
                   TI_ISA_LC_Value_In_Class(AbsInc(dist, IMM8_BUF_SIZE), LC_simm8)) {

          // From rri12 back to rri8 again
          // Make imm8 range bigger to be conservative

          TN *opnd0 = OP_opnd(xfer_op, 0);
          DEF_KIND kind;
          OP *sub_op = TN_Reaching_Value_At_Op(opnd0, xfer_op, &kind, TRUE);
          if ( (sub_op == NULL) || (OP_bb(sub_op) != bb) ||
               ((OP_code(sub_op) != TOP_sub) && 
                (OP_code(sub_op) != TOP_addi) && (OP_code(sub_op) != TOP_addmi)) ) {
            continue;
          }
          /*
             sub_op is either TOP_sub or TOP_addi/TOP_addmi (with a negative 
             immediate due to the lack of subi)
           */
          TN *opnd1 = OP_opnd(sub_op, 0);
          TN *opnd2 = OP_opnd(sub_op, 1);
          bool opnd_avaliable=true;
          if (OP_code(sub_op) == TOP_sub) {
             Is_True(TN_is_register(opnd1) && TN_is_register(opnd2),
                     ("TOP_sub's TN are not register TNs"));
             for (OP *op=OP_next(sub_op); op != xfer_op; op = OP_next(op)) {
               if (OP_Defs_Reg(op, TN_register_class(opnd1), TN_register(opnd1)) ||
                   OP_Defs_Reg(op, TN_register_class(opnd2), TN_register(opnd2))) {
                 opnd_avaliable=false;
                 break;
               }
             }
          } else {
             Is_True(TN_is_register(opnd1) && TN_is_constant(opnd2),
                     ("TOP_addi's TN are not register/const TNs"));
             for (OP *op=OP_next(sub_op); op != xfer_op; op = OP_next(op)) {
               if (OP_Defs_Reg(op, TN_register_class(opnd1), TN_register(opnd1))) {
                 opnd_avaliable=false;
                 break;
               }
             }
             if (TN_has_value(opnd2) && 
                 TI_ISA_LC_Value_In_Class(-TN_value(opnd2), LC_b4const)) {
               opnd_avaliable=false;
             }
             if (opnd_avaliable) {
               if (new_xfer_top == TOP_beq) {
                 new_xfer_top = TOP_beqi;
               } else {
                 Is_True(new_xfer_top == TOP_bne, ("Branch Selection: wrong TOP"));
                 new_xfer_top = TOP_bnei;
               }

               opnd2 = Gen_Literal_TN(-TN_value(opnd2), TN_size(opnd2));
             }
          }
          if (opnd_avaliable) {

            if (EBO_Trace_Select_Br) {
              #pragma mips_frequency_hint NEVER
              fprintf(TFile,"%s select_br[peep]: xfer OP  ==> br w/ imme8 at BB %d\n",
                      EBO_trace_pfx, BB_id(bb));
            }

            OPS *ops = OPS_Create();
            // Build_OP(new_xfer_top, opnd1, opnd2, OP_opnd(xfer_op, 1), ops);
            OP *new_op = Mk_OP(new_xfer_top, opnd1, opnd2, OP_opnd(xfer_op, 1));
            OPS_Append_Op(ops, new_op);
            Set_OP_no_generics(new_op);
            
            BB_Insert_Ops_After(bb, xfer_op, ops);
            BB_Remove_Op(bb,xfer_op);
            BB_Remove_Op(bb,sub_op);
          }
        }
      }

      // Transfer bnez(beqz) to bnez.n(beqz.n) if possible
      xfer_op = BB_xfer_op(bb);
      Is_True(xfer_op != NULL, ("Should be conditional branch"));
      new_density_xfer_top = Get_Branch_Density_Top(xfer_op);
      do_density_opt = EBO_in_peep && (new_density_xfer_top != TOP_UNDEFINED);

      // If make |dist| larger and it still fit into uimm6, then it's
      // very likely that this OP can be changed to a density OP.
      if (do_density_opt && 
          TI_ISA_LC_Value_In_Class(AbsInc(dist, IMM6_BUF_SIZE), LC_uimm6)) {
        OPS *ops = OPS_Create();
        Build_OP(new_density_xfer_top, OP_opnd(xfer_op, 0), OP_opnd(xfer_op, 1), ops);
        BB_Insert_Ops_After(bb, xfer_op, ops);
        BB_Remove_Op(bb,xfer_op);

        if (EBO_Trace_Select_Br) {
          #pragma mips_frequency_hint NEVER
          fprintf(TFile,"%s select_br[peep]: xfer OP  ==> density xfer OP at BB %d\n",
                  EBO_trace_pfx, BB_id(bb));
        }
      }
    }
  }
}

static void EBO_Select_WBranches(BB *first_bb)
{
  BB *bb;
  UINT32 nBBs = BB_MAP_idx_max + 2; // definitely larger than the number of BBs
  mUINT32 sizeOfBB[nBBs];

  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    Is_True ((BB_id(bb) < nBBs), ("BB.id out of bound"));
    sizeOfBB[BB_id(bb)] = BB_size(bb);
  }
 
  clear_bb_flag(first_bb);
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    Set_BB_visited(bb);

    // Skip swp-scheduled BBs
    if (BB_rotating_kernel(bb)) continue;

    OP *xfer_op = BB_xfer_op(bb);
    if (xfer_op == NULL) continue;

    BBLIST *target_edge = BB_succs(bb);
    if (target_edge == NULL) continue;

    BB *target_bb = BBLIST_item(target_edge);
    if (target_bb == BB_next(bb)) {
      // the first succ isn't a target, so try the second
      target_edge = BBLIST_next(target_edge);
      if (target_edge != NULL) {
        target_bb = BBLIST_item(target_edge);
      } else {
        target_bb = NULL;
      }
    }
    if (target_bb == NULL) continue;

    // Calculate the distance
    INT32 dist=0;
    if (target_bb == bb) {
      dist = -sizeOfBB[BB_id(bb)];
    } else if (BB_visited(target_bb)) {
      dist = -sizeOfBB[BB_id(target_bb)];
      for (BB *b=bb; b != target_bb; b = BB_prev(b)) {
        Is_True( (b != NULL), ("BB_prev isn't correct"));
        dist -= sizeOfBB[BB_id(b)];
      }
    } else {
      dist = 0;
      for (BB *b=BB_next(bb); b != target_bb; b = BB_next(b)) {
        Is_True( (b != NULL), ("BB_next isn't correct"));
        dist += sizeOfBB[BB_id(b)];
      }
    }

    bool need_change = false;
    TOP top = OP_code(xfer_op);
    if ( top == TOP_beqz || top == TOP_bnez || 
         top == TOP_bgez || top == TOP_bltz ) {
      /*
         If making dist smaller (smaller absolute value) and it cannot
         still be in the range, it is likely that we need to change it
         to a wide branch
       */
      if (!TI_ISA_LC_Value_In_Class(AbsDec(dist, IMM12_BUF_SIZE), LC_simm12)) {
        need_change = true;
      }
    } else {
      if (!TI_ISA_LC_Value_In_Class(AbsDec(dist, IMM8_BUF_SIZE), LC_simm8)) {
        need_change = true;
      }
    }

    if (need_change) {
      ISA_LITCLASS lc;
      TOP wbtop = TI_TOP_Get_WBranch_Top_Litclass(top, &lc);

      if ((wbtop != TOP_UNDEFINED) &&
	  (TI_TOP_No_Valid_Format(wbtop) == FALSE)) {
	if (TI_ISA_LC_Value_In_Class(dist, lc)) {
	  Set_OP_code(xfer_op, wbtop);
	  Set_OP_no_generics(xfer_op);
	} else {
	  DevWarn("Branch's target is too far away!");
	}
      }

#if 0
      if ((wbtop != TOP_UNDEFINED) &&
          (TI_ISA_LC_Value_In_Class(dist, lc))) {
        Set_OP_code(xfer_op, wbtop);
        Set_OP_no_generics(xfer_op);
      } else if (wbtop != TOP_UNDEFINED) {
        DevWarn("Branch's target is too far away!");
      }
#endif
    }
  }
}

/*
   is_stack_mem_dead() traverses the control flow, from start_bb to
   the end of BB (exit BB, return BB), to see if the memory location
   (used by memory OP 'op' in BB 'bb') is loaded again, if so, the value
   in the memory location isn't dead; otherwise, it is considered dead.

   This routine requires that BB_local_flag1 be reset before calling
   this routine.
*/ 
static BOOL is_stack_mem_dead (BB *bb, OP *op, BB *start_bb, OP *start_op)
{
  OP *op_b, *op_e;
  if (start_op == NULL) {
    op_b = BB_first_op(start_bb);
    op_e = (start_bb == bb) ? op : NULL;
  } else {
    op_b = OP_next(start_op);
    op_e = NULL;
  }

  for (OP *o1 = op_b; o1 != op_e; o1 = OP_next(o1))
  {
    if ( OP_code(o1) == TOP_asm || OP_call(o1) || 
         OP_code(o1) == TOP_movsp ) {
      return FALSE;
    }

    if (OP_load(o1) && CG_DEP_Mem_Ops_Alias (op, o1, NULL)) {
      return FALSE;
    }
  }

  if (start_op == NULL) {
    Set_BB_local_flag1(start_bb);
  }

  BOOL is_dead = TRUE;
  BBLIST *succ;
  FOR_ALL_BB_SUCCS(start_bb, succ) {
    BB *bb_succ = BBLIST_item(succ);
    if (!BB_local_flag1(bb_succ)) {
      if (!is_stack_mem_dead(bb, op, bb_succ, NULL)) {
        is_dead = FALSE;
        break;
      }
    }
  }
  return is_dead;
}
      
/*
   Check to see if any store to stack is no longer needed, and
   if so, remove them.
*/
static void Remove_unnecessary_stores (BB *first_bb)
{
  BB *bb;
  OP *op, *next;

  BOOL print_trace_header = TRUE;
  BOOL EBO_trace_store_removal = Get_Trace(TP_EBO, 0x100);

  if (CG_opt_level < 2) 
    return;

  for ( bb = first_bb; bb; bb = BB_next(bb) ) {
    for (op = BB_first_op(bb); op; op = next) {
      next = OP_next(op);

      if (OP_volatile(op) || OP_side_effects(op) ||
          !OP_store(op)) {
        continue;
      }
     

      TN *offset_tn, *base_tn;
      OP_Base_Offset_TNs(op, &offset_tn, &base_tn);
      if ( (base_tn == NULL) || (offset_tn == NULL) ||
           !TN_is_symbol(base_tn) ) {
        continue;
      }
      
      ST *base_st;
      INT64 base_ofst;
      Base_Symbol_And_Offset(TN_var(base_tn), &base_st, &base_ofst);
      if ( (base_st == SP_Sym) || (base_st == FP_Sym) )
      {
        // To prevent infinite loop by is_stack_mem_dead()
        for (BB *b1 = first_bb; b1; b1 = BB_next(b1)) {
          Reset_BB_local_flag1(b1);
        }
        if (is_stack_mem_dead(bb, op, bb, op)) {
          if (EBO_trace_store_removal) {
            if (print_trace_header) {
              fprintf(TFile, "<EBO:store removal>\n");
              fprintf(TFile, "<EBO:store removal> Remove unnecessary stores in PU: %s\n",
                             Cur_PU_Name);
              fprintf(TFile, "<EBO:store removal>\n");
              print_trace_header = FALSE;
            }
            fprintf(TFile, "<EBO:store removal> the following store in BB:%d removed!\n",
                           BB_id(bb));
            Print_OP_No_SrcLine(op);
          }
          BB_Remove_Op(bb, op);
        }
      }
    } // for (op....
  } // for ( bb = ...
}
    
/* 
 * Perform EBO
 */
static
void
EBO_Process ( BB *first_bb, RID *rid, BOOL allow_retry =FALSE )
{
  BB *bb;
  
  EBO_Trace_Execution    = Get_Trace(TP_EBO, 0x001);
  EBO_Trace_Optimization = Get_Trace(TP_EBO, 0x002);
  EBO_Trace_Block_Flow   = Get_Trace(TP_EBO, 0x004);
  EBO_Trace_Data_Flow    = Get_Trace(TP_EBO, 0x008);
  EBO_Trace_Hash_Search  = Get_Trace(TP_EBO, 0x010);

#ifdef TARG_XTENSA
  /* First we want to run a pass of copy propagation. For xtensa we've
     disabled the copy propagation within ebo proper since it doesn't
     work very well. */
  if (EBO_Copy_Prop && (CG_opt_level >= 2)) {
    if (EBO_in_peep) {
      if (EBO_Reg_Copy_Prop &&
          EBO_Reg_Copy_Propagation(first_bb) && !CG_localize_tns) {
	REG_LIVE_Finish();
	REG_LIVE_Analyze_Region();
      }
    } else { // not in peep
      if (EBO_Copy_Propagation(first_bb) && !CG_localize_tns)
        GRA_LIVE_Recalc_Liveness(rid);
    }
  }
#endif
  
  rerun_cflow = FALSE;
  ebo_pass = 0;

  /* We want to force a second pass of ebo, even if there are no
     changes during the first pass, if:

     1. If EBO_in_pre is true. During the first pass of EBO_in_pre we
     don't allow immediate moves to be changed into reg-reg copies
     (when a previous reg holding the immediate is available). We do
     this so local immediates produced by the code-selector can be
     cleaned up without having the local reg holding the immediate be
     propagated out of the block.  */

  BOOL force_second_pass = FALSE;
  if (EBO_in_pre)
    force_second_pass = TRUE;

  /* Perform ebo passes... */
  do
  {
    rerun_ebo = FALSE;

    if (ebo_pass != 0)
    {
      if (EBO_in_peep)
      {
	REG_LIVE_Finish();
	REG_LIVE_Analyze_Region();
      }
      else if (!CG_localize_tns)
	GRA_LIVE_Recalc_Liveness(rid);
    }

    /* Increment pass number (first pass is numbered 1, not 0), and
       force a second pass if necessary. */

    ebo_pass++;
    if ((ebo_pass == 1) && force_second_pass)
      rerun_ebo = TRUE;
    
    FmtAssert(((EBO_first_tninfo == NULL) && (EBO_first_opinfo == NULL)),
	      ("Initial pointers not NULL %o %o",EBO_first_tninfo,EBO_first_opinfo));
    
    EBO_Start();

    if (EBO_Trace_Data_Flow || EBO_Trace_Optimization) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,">>>> Before EBO (pass %d)", ebo_pass);
      if (EBO_in_peep) {
	fprintf(TFile," - in peep ");
      } else if (EBO_in_before_unrolling) {
	fprintf(TFile," - before unrolling ");
      } else if (EBO_in_after_unrolling) {
	fprintf(TFile," - after unrolling ");
      } else if (EBO_in_pre) {
	fprintf(TFile," - preprocessing ");
      } else {
	fprintf(TFile," - main ");
      }
      fprintf(TFile,"<<<<\n");

      if (EBO_Trace_Data_Flow) {
	Print_All_BBs ();
      }
    }

    /* TEMPORARY - EBO doesn't understand rotating registers, so skip blocks that use them. */
    for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
      if (BB_rotating_kernel(bb)) Set_BB_visited(bb);
    }

    for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
      RID *bbrid;
      if (( bbrid = BB_rid( bb )) &&
	  ( RID_level( bbrid ) >= RL_CGSCHED ) ) {
	/*
	 * There is no overlap in TN's between the current REGION
	 * and REGIONs which have already been through CG
	 */
	continue;
      }
      if (!BB_visited(bb)) {
	EBO_Add_BB_to_EB (bb);
//	if (EBO_in_loop) break;
      }
    }
    
    /* Clear the bb flag, in case some other phase uses it. */
    clear_bb_flag (first_bb);
    
    EBO_Finish();
    
    if (EBO_Trace_Data_Flow) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,">>>> After EBO (pass %d)", ebo_pass);
      if (EBO_in_peep) {
	fprintf(TFile," - in peep ");
      } else if (EBO_in_before_unrolling) {
	fprintf(TFile," - before unrolling ");
      } else if (EBO_in_after_unrolling) {
	fprintf(TFile," - after unrolling ");
      } else if (EBO_in_pre) {
	fprintf(TFile," - preprocessing ");
      } else {
	fprintf(TFile," - main ");
      }
      fprintf(TFile,"<<<<\n");
      fprintf(TFile,">>>> %d tninfo entries allocated and reused %d times\n",
	      EBO_num_tninfo_entries,EBO_tninfo_entries_reused);
      fprintf(TFile,">>>> %d opinfo entries allocated and reused %d times\n",
	      EBO_num_opinfo_entries,EBO_opinfo_entries_reused);
      Print_All_BBs ();
    }
  } while (rerun_ebo && allow_retry && (ebo_pass < MAX_EBO_PASSES));

  if (rerun_cflow && !EBO_in_loop) {
    INT32 cflow_flags = CFLOW_BRANCH | CFLOW_UNREACHABLE;
    if (EBO_in_peep)
      cflow_flags |= CFLOW_AFTER_LRA;
    CFLOW_Optimize(cflow_flags, "CFLOW (from ebo)");
  }

  // Identify obvious cases in which stores are not needed
  if ( (CG_opt_level > 1) && EBO_Remove_Store ) {
    Remove_unnecessary_stores (first_bb);
  }

  /* Select right branch based on pc-relative distance of branch's target */
  if (EBO_Select_Br) {
    if (TI_ISA_Has_Wide_Branches()) {
      if (EBO_in_peep) {
        EBO_Select_WBranches(first_bb);
      }
    } else {
      if (EBO_in_acgprep) {
        // Performed before GRA and the first scheduling pass
        EBO_Select_Branch(first_bb);
      } else if (EBO_in_peep) {
        // Performed after GRA and before the final scheduling pass
        EBO_Select_Branch(first_bb);
      }
    }
  }
}

/* =======================================================================
 * =======================================================================
 *
 * There are a number of different entry points to EBO.  They differ because
 * the specific information available changes throughout the compilation.
 * The flags that are set at each entry are used during EBO if a use is to
 * be made of information that is only available at certain times.
 *
 * =======================================================================
 * =======================================================================
 */

/* 
 * perform EB optimizations right after instruction translation.
 */
void
EBO_Pre_Process_Region ( RID *rid )
{
  BB *first_bb = (rid) ? CGRIN_first_bb(RID_cginfo( rid )) : REGION_First_BB;

  EBO_in_pre  = TRUE;
  EBO_in_before_unrolling = FALSE;
  EBO_in_after_unrolling = FALSE;
  EBO_in_peep = FALSE;
  EBO_in_acgprep = FALSE;

  EBO_in_loop = FALSE;

  if (EBO_Opt_Level < 5) return;

  clear_bb_flag (first_bb);
  EBO_Process (first_bb, rid, TRUE);
}

  
/* 
 * perform EBO optimizations during unrolling and pipelining
 */
void
EBO_before_unrolling(BB_SET *bbs )
{
  INT i;
  BB *first_bb = REGION_First_BB;
  EBO_in_pre  = FALSE;
  EBO_in_before_unrolling = TRUE;
  EBO_in_after_unrolling = FALSE;
  EBO_in_peep = FALSE;
  EBO_in_acgprep = FALSE;

  EBO_in_loop = TRUE;

  if (EBO_Opt_Level < 4) return;

  BB *bb;
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    if (BB_SET_MemberP(bbs, bb))
      Reset_BB_visited(bb);
    else
      Set_BB_visited(bb);
  }
  EBO_Process (first_bb, NULL);
}

  
/* 
 * perform EBO optimizations after unrolling and pipelining
 */
void
EBO_after_unrolling(BB_SET *bbs )
{
  INT i;
  BB *first_bb = REGION_First_BB;

  EBO_in_pre  = FALSE;
  EBO_in_before_unrolling = FALSE;
  EBO_in_after_unrolling = TRUE;
  EBO_in_peep = FALSE;
  EBO_in_acgprep = FALSE;

  EBO_in_loop = TRUE;

  if (EBO_Opt_Level < 3) return;
 
  BB *bb;
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    if (BB_SET_MemberP(bbs, bb))
      Reset_BB_visited(bb);
    else
      Set_BB_visited(bb);
  }
  EBO_Process (first_bb, NULL);
}

  
/* 
 * perform EB optimizations on a region
 */
void
EBO_Process_Region ( RID *rid )
{
  BB *first_bb = (rid) ? CGRIN_first_bb(RID_cginfo( rid )) : REGION_First_BB;

  EBO_in_pre  = FALSE;
  EBO_in_before_unrolling = FALSE;
  EBO_in_after_unrolling = FALSE;
  EBO_in_peep = FALSE;
  EBO_in_acgprep = FALSE;

  EBO_in_loop = FALSE;

  if (EBO_Opt_Level < 2) return;

  clear_bb_flag (first_bb);
  EBO_Process (first_bb, rid, TRUE);
}

/* 
 * perform EB optimizations after CGPREP (loop optimization and SWP).
 */
void
EBO_ACGPREP_Process_Region ( RID *rid )
{
  BB *first_bb = (rid) ? CGRIN_first_bb(RID_cginfo( rid )) : REGION_First_BB;

  EBO_in_pre  = FALSE;
  EBO_in_before_unrolling = FALSE;
  EBO_in_after_unrolling = FALSE;
  EBO_in_peep = FALSE;

  EBO_in_loop = FALSE;

  EBO_in_acgprep = TRUE;

  if (EBO_Opt_Level < 2) return;

  clear_bb_flag (first_bb);
  EBO_Process (first_bb, rid, TRUE);
}
  
/* 
 * perform EB optimizations after register assignment.
 */
void
EBO_Post_Process_Region ( RID *rid )
{
  BB *first_bb = (rid) ? CGRIN_first_bb(RID_cginfo( rid )) : REGION_First_BB;

  EBO_in_pre  = FALSE;
  EBO_in_before_unrolling = FALSE;
  EBO_in_after_unrolling = FALSE;
  EBO_in_peep = TRUE;
  EBO_in_acgprep = FALSE;

  EBO_in_loop = FALSE;

  if (EBO_Opt_Level < 1) return;

  /* compute live-in sets for physical registers */
  MEM_POOL_Push(&MEM_local_pool);

  REG_LIVE_Analyze_Region();
  clear_bb_flag (first_bb);
  EBO_Process (first_bb, rid, TRUE);
  REG_LIVE_Finish();

  /* Do EBO_Process() twice if Opt_Level == 3
     Note that Opt_Level is used only for this purpose because
     CG_opt_level cannot be >=3 for now.
   */
  if (Opt_Level > 2) {
    REG_LIVE_Analyze_Region();
    clear_bb_flag (first_bb);
    EBO_Process (first_bb, rid, TRUE);
    REG_LIVE_Finish();
  }

  MEM_POOL_Pop(&MEM_local_pool);
}

/*
 * Perform simple optimizations for a BB.
 *
 * Currently, it is invoked after GCM in the final IGLS is performed.
 */
bool
EBO_Post_Process_BB(BB *bb)
{
  bool changed = false;
  changed = EBO_reduce_redundant_const16(bb);
  return changed;
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
