
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
 *  Module: ebo_special.cxx
 *  $Revision: 1.88 $
 *  $Date: 2000/04/19 16:26:48 $
 *  $Author: dew $
 *  $Source: /osprey.src/osprey1.0/be/cg/ia64/RCS/ebo_special.cxx,v $
 *
 *  Revision comments:
 *
 *  17-June-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  EBO special case optimizations.
 *
 * =======================================================================
 * =======================================================================
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
#endif /* _KEEP_RCS_ID */

#include <stdarg.h>
#include "defs.h"
#include "config_targ_options.h"
#include "errors.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
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
#include "bitset.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cg_db_op.h"
#include "whirl2ops.h"
#include "cgtarget.h"
#include "gra_live.h"
#include "reg_live.h"
#include "cflow.h"
#include "cg_spill.h"
#include "cgexp_internals.h"
#include "data_layout.h"
#include "stblock.h"
#include "cxx_hash.h"
#include "op.h"
#include "fb_whirl.h"


#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"
#include "tie.h"

/* Define a macro to strip off any bits outside of the left most 4 bytes. */
#define TRUNC_32(val) (val & 0x00000000ffffffffll)

/* Define a macro to sign-extend the least signficant 32 bits */
#define SEXT_32(val) (((INT64)(val) << 32) >> 32)

/* ===================================================================== */

extern BOOL FREQ_freqs_computed;

typedef HASH_TABLE<ST_IDX, INITV_IDX> ST_TO_INITV_MAP;
static ST_TO_INITV_MAP *st_initv_map = NULL;
static BOOL st_initv_map_inited = FALSE;
static GTN_SET *work_gtn_set = NULL;
static BS *work_defined_set = NULL;
static MEM_POOL *work_pool = NULL;
static INT32 fixed_branch_cost, taken_branch_cost;

static INT BB_cost_est(BB *bb);

/* Initialize and finalize ebo special routines. */
void
EBO_Special_Start (MEM_POOL *pool)
{
  st_initv_map = CXX_NEW(ST_TO_INITV_MAP(31, pool), pool);
  st_initv_map_inited = FALSE;
  work_gtn_set = GTN_SET_Create_Empty(Last_TN + 1, pool);
  work_defined_set = BS_Create_Empty(Last_TN + 1, pool);
  work_pool = pool;

  INT32 idummy;
  double ddummy;
  CGTARG_Compute_Branch_Parameters(&idummy, &fixed_branch_cost,
				   &taken_branch_cost, &ddummy);
}

void
EBO_Special_Finish (void)
{
  st_initv_map = NULL;
  st_initv_map_inited = FALSE;
  work_gtn_set = NULL;
  work_defined_set = NULL;
  work_pool = NULL;
}


/*
 * Identify OP's that contain a constant and operate in a way that
 * will allow the constant to be added into an offset field of
 * a load or store instruction.
 */
BOOL
EBO_Can_Merge_Into_Offset (OP *op, UINT *index_ind, UINT *immed_ind)
{
  TOP top = OP_code(op);
  
  if ((top != TOP_add) && (top != TOP_add_n)
      && (top != TOP_addi) && (top != TOP_addi_n)
      && (top != TOP_addmi) && (top != TOP_sub))
    return FALSE;

  if ((op == BB_entry_sp_adj_op(OP_bb(op))) ||
      (op == BB_exit_sp_adj_op(OP_bb(op))))
    return FALSE;

  if (index_ind)
    *index_ind = 0;
  if (immed_ind)
    *immed_ind = 1;
  
  TN *tn = OP_opnd(op,1);
  if (TN_Is_Constant(tn))
    return TRUE;

  /* try to handle
   *
   * load_const	a9, 232
   * add	a3, a1, a9
   * l32i	a7, a3, 0
   */
  EBO_OP_INFO* addop_info = find_opinfo_entry(op);
  EBO_TN_INFO* offset_tninfo = addop_info ? addop_info->actual_opnd[1] : NULL;
  OP* ld_const_op = offset_tninfo ?  offset_tninfo->in_op : NULL;
  if (ld_const_op &&
      (OP_code(ld_const_op)==TOP_load_const ||
       OP_code(ld_const_op)==TOP_const16lo ||
       OP_code(ld_const_op)==TOP_movi))
    return TRUE;

  return FALSE;
}


static void
EBO_Set_OP_omega (OP *op, ...)
{
  INT opnds = OP_opnds(op);
  INT i;
  va_list tninfos;

  va_start(tninfos, op);
  CG_LOOP_Init_Op(op);
  for (i = 0; i < opnds; i++) {
    EBO_TN_INFO *tninfo = va_arg(tninfos, EBO_TN_INFO *);
    Set_OP_omega (op, i, ((tninfo != NULL) ? tninfo->omega : 0));
  }

  va_end(tninfos);
  return;
}


static void
EBO_Copy_OP_omega (OP *new_op, OP *old_op)
{
  INT opnds = OP_opnds(new_op);
  INT i;

  CG_LOOP_Init_Op(new_op);
  for (i = 0; i < opnds; i++) {
    Set_OP_omega (new_op, i, OP_omega(old_op,i));
  }

  return;
}


static void
EBO_OPS_omega (OPS *ops)
{
  OP *next_op = OPS_first(ops);
  while (next_op != NULL) {
    INT opnds = OP_opnds(next_op);
    INT i;

    CG_LOOP_Init_Op(next_op);
    for (i = 0; i < opnds; i++) {
      Set_OP_omega (next_op, i, 0);
    }

    next_op = OP_next(next_op);
  }
}


static TOP
top_injui (void)
{
  static BOOL top_init = FALSE;
  static TOP top = TOP_UNDEFINED;
  
  if (top_init)
    return top;
  
  top = TI_TOP_Topcode("injui");
  top_init = TRUE;

  if (EBO_Trace_Optimization && top == TOP_UNDEFINED)
    fprintf(TFile, "%sinjui opcode not found, injui sequence disabled.\n");
  
  return top;
}


static BOOL
find_literal_symbol (ST *lit, ST **st, INT64 *offset)
{
  Is_True(st_initv_map, ("expecting map from ST to INITV"));
  
  if (!st_initv_map_inited)
  {
    /* Visit all the initialized objects contained in literal
       symbols. */

    for (UINT i = 1; i < INITO_Table_Size(CURRENT_SYMTAB); i++)
    {
      INITO *ino = &Inito_Table(CURRENT_SYMTAB, i);
      ST *st = INITO_st(ino);
      if (ST_sclass(st) == SCLASS_LITERAL_POOL)
      {
	/* If 'ino' consists of one INITVKIND_SYMOFF, then enter
           'inv_idx' into the map. */

	INITV_IDX inv_idx = INITO_val(*ino);
	if ((inv_idx != 0) && (INITV_next(inv_idx) == 0) &&
	    (INITV_kind(inv_idx) == INITVKIND_SYMOFF))
	{
	  st_initv_map->Enter_If_Unique(ST_st_idx(st), inv_idx);
	}
      }
    }
    
    st_initv_map_inited = TRUE;
  }
  
  INITV_IDX initv = st_initv_map->Find(ST_st_idx(lit));
  if (initv == 0)
    return FALSE;

  *st = &St_Table[INITV_st(initv)];
  *offset = INITV_ofst(initv);

  return TRUE;
}


BOOL
combine_adjacent_loads(OP *op,
                       EBO_TN_INFO **opnd_tninfo,
		       EBO_OP_INFO *opinfo,
                       INT64 offset_pred,
                       INT64 offset_succ)
{
  /* We could potentially combine two narrow loads into a wider one
     and an extract. With the current microarch it doesn't seem too
     critical yet. */
//  Lmt_DevWarn(1, ("XTENSA: combine_adjacent_loads UNIMPLEMENTED."));
  return FALSE;
}

  
BOOL
delete_subset_mem_op(OP *op,
                     EBO_TN_INFO **opnd_tninfo,
		     EBO_OP_INFO *opinfo,
                     INT64 offset_pred,
                     INT64 offset_succ)
{
  OP *pred_op = opinfo->in_op;
  BB *bb = OP_bb(op);
  TOP top = OP_code(op);
  TOP pred_top = OP_code(pred_op);
  INT opcount = OP_opnds(op);
  INT pred_storeval_idx=TI_TOP_Find_Operand_Use(pred_top,OU_storeval);
  INT succ_storeval_idx=TI_TOP_Find_Operand_Use(top,OU_storeval);
  TN *pred_result = OP_store(pred_op) 
                      ? (pred_storeval_idx>=0?
			 OP_opnd(pred_op, pred_storeval_idx):NULL)
                      : OP_result(pred_op,0);  
  TN *succ_result = OP_store(op)
                      ? (succ_storeval_idx>=0?
			 OP_opnd(op, succ_storeval_idx):NULL)
                      : OP_result(op,0);

  if (EBO_Trace_Data_Flow)
  {
    fprintf(TFile,"%ssubset    OP in BB:%d    ",EBO_trace_pfx,BB_id(bb));
    Print_OP_No_SrcLine(op);
    fprintf(TFile,"      Matches   OP in BB:%d    ",BB_id(opinfo->in_bb));
    Print_OP_No_SrcLine(pred_op);
  }
  
  /* Can only optimize when 'pred_op' is a load of known data or a
     store. In either case the address must be known and aligned. */
  if ((OP_load(pred_op) && OP_unknown_memdata(pred_op)) ||
      OP_unalign_mem(pred_op) || OP_unknown_addr(pred_op))
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sCan only subset mem/addr known load/store.\n", EBO_trace_pfx);

    return FALSE;
  }

  /* Can only optimize when 'op' is a load of known data from a known
     aligned address. */
  if (!OP_load(op) || OP_unknown_memdata(op) ||
      OP_unalign_mem(op) || OP_unknown_addr(op))
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sCan only eliminate subset loads with known mem/addr.\n", EBO_trace_pfx);

    return FALSE;
  }

  /* Auto-increment must be preserved. */
  if (TI_TOP_Find_Operand_Use(top, OU_postincr) >= 0)
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sMust preserve auto-increment.\n", EBO_trace_pfx);

    return FALSE;
  }

  /* We only optimize if both result TNs are known */
  if (pred_result==NULL || succ_result==NULL)
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sCan only subset memory operations for known results.\n", EBO_trace_pfx);

    return FALSE;
  }

  /* We only understand how to extract values for integers... */
  if ((TN_register_class(pred_result) != TI_ISA_Regclass_Integer()) ||
      (TN_register_class(succ_result) != TI_ISA_Regclass_Integer()))
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sCan only subset memory operations for integers.\n", EBO_trace_pfx);

    return FALSE;
  }

  /* Global TN's aren't supported at low levels of optimization. */
  if ((CG_opt_level < 2) && (bb != opinfo->in_bb))
    return FALSE;

  if (!EBO_in_peep &&
      (bb != opinfo->in_bb) &&
      !TN_Is_Constant(pred_result) &&
      has_assigned_reg(pred_result))
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
              EBO_trace_pfx);

    return FALSE;
  }

  /* If the successor access is not a subset of the predecessor
     access, then we can't replace the successor. */
  UINT size_pred = CGTARG_Mem_Ref_Bytes(pred_op);
  UINT size_succ = CGTARG_Mem_Ref_Bytes(op);
  if ((size_pred < size_succ)
      || (offset_pred > offset_succ)
      || ((offset_pred + size_pred) < (offset_succ + size_succ)))
  {
    if (EBO_Trace_Data_Flow)
      fprintf(TFile,"%sSuccessor access is not subset of predecessor access.\n",
              EBO_trace_pfx);

    return FALSE;
  }

  /* 'byte_offset' is the offset into the predecessor object at which
     the successor object starts. 'bit_offset' and 'bit_size' indicate
     the first bit position and the number of bits that we need to
     extract. */

  UINT byte_offset = offset_succ - offset_pred;
  UINT bit_size = size_succ * 8;
  UINT bit_offset = (Target_Byte_Sex == BIG_ENDIAN ?
		     size_pred-byte_offset-size_succ : byte_offset) * 8;

  /* If 'op' is an unsigned load, then we can use an
     extract. Otherwise we can use a sext if it is available, and if
     'bit_offset' is 0. */

  if ((top == TOP_l8ui) || (top == TOP_l16ui))
  {
    if (bit_size <= 16)
    {
      OP *new_op = Mk_OP(TOP_extui, succ_result, pred_result,
			 Gen_Literal_TN(bit_offset, 4), Gen_Literal_TN(bit_size, 4));
      if (EBO_in_loop)
	EBO_Set_OP_omega(new_op, NULL, NULL, NULL);
      
      BB_Insert_Op_After(bb, op, new_op);
      
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sReplace subset load with extract.\n",EBO_trace_pfx);
      
      return TRUE;
    }
  }
  else if (xt_sext && (bit_offset == 0) && (bit_size >= 8) && (bit_size <= 23))
  {
    OP *new_op = Mk_OP(TOP_sext, succ_result, pred_result,
		       Gen_Literal_TN(bit_size-1, 4));
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, NULL, NULL);
      
    BB_Insert_Op_After(bb, op, new_op);
      
    if (EBO_Trace_Optimization)
      fprintf(TFile,"%sReplace subset load with sext.\n",EBO_trace_pfx);
      
    return TRUE;
  }
  
  if (EBO_Trace_Optimization)
    fprintf(TFile,"%sReplace subset load failed.\n",EBO_trace_pfx);

  return FALSE;
}


/* 
 * delete_memory_op
 *
 * For a given load or store and one it matches,
 * attempt to replace one of them.
 * Return TRUE if this op is no longer needed.
 */
static BOOL
delete_memory_op (OP *op,
                  EBO_TN_INFO **opnd_tninfo,
		  EBO_OP_INFO *opinfo)
{
  OPS ops = OPS_EMPTY;
  INT size_pred;
  INT size_succ;

  /* In each case below, before attempting to remove a load, store, or
     prefetch, we must make sure the instruction does not side-effect
     any state, etc. If it does, we can't remove it. This check is
     needed in addition to our general mechanism of making all state
     appear live on exit to the function. */

  INT results = OP_results(op);
  for (UINT i = 0; i < results; i++)
  {
    TN *tn = OP_result(op, i);
    if (TN_is_register(tn) && (TI_ISA_Regclass_Is_State(TN_register_class(tn))))
      return FALSE;
  }
  
  /* Remove the second OP for:
     Prefetch - Prefetch,
     Load - Prefetch,
     Store - Prefetch
  */
  if (OP_prefetch(op))
  {
    if (EBO_Trace_Optimization) {
      fprintf(TFile,"%sRemove following Prefetch combination\n",EBO_trace_pfx);
    }

    return TRUE;
  }

  /* Don't optimize:
     Prefetch - Load,
     Prefetch - Store,
  */
  if (OP_prefetch(opinfo->in_op))
  {
    return FALSE;
  }

  /* Don't try to optimize unaligned or unknown accesses. */
  if (OP_unalign_mem(op) || OP_unalign_mem(opinfo->in_op) ||
      OP_unknown_addr(op) || OP_unknown_addr(opinfo->in_op))
    return FALSE;

  /* Don't try to optimize unknown memory data. */
  if ((OP_load(op) && OP_unknown_memdata(op)) ||
      (OP_load(opinfo->in_op) && OP_unknown_memdata(opinfo->in_op)))
    return FALSE;
 
  size_pred = CGTARG_Mem_Ref_Bytes(opinfo->in_op);
  size_succ = CGTARG_Mem_Ref_Bytes(op);

  /* Replace the result tn of the second OP for:
     Load - Load,
  */
  if (OP_load(op) && OP_load(opinfo->in_op))
  {
    /* The increment must be preserved. */

    if (TI_TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0)
    {
      if (EBO_Trace_Optimization)
      {
	fprintf(TFile,"%sIncremented Load for Load - Load combination\n",
		EBO_trace_pfx);
      }
	
      return FALSE;
    }

    /* Make sure the result TNs' regclasses and ops match. */

    if (TN_register_class(OP_result(op,0)) !=
	TN_register_class(OP_result(opinfo->in_op, 0)))
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sRegclass mismatch for Load - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    /* This check should be moved after the subset check below, since
       here currently prevents us reaching the subsetting... (but too
       close to release to do it now...). */
    if (OP_code(op) != OP_code(opinfo->in_op))
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sMtype mismatch for Load - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    /* If the size of the data item loaded by the two loads is
       different, but the starting memory address is the same.
       There is a chance that the predecessor load is a wider load
       and that the new load's data can be extracted. */
    
    if ((size_pred != size_succ) ||
	(OP_results(op) != OP_results(opinfo->in_op)) ||
	(TN_size(OP_result(opinfo->in_op, 0)) != TN_size(OP_result(op, 0))))
    {
      if (EBO_Trace_Optimization)
      {
	fprintf(TFile,"%sSize mismatch for Load - Load combination: %d:%d %d:%d \n",
		EBO_trace_pfx,size_pred,size_succ,
		TN_size(OP_result(opinfo->in_op, 0)),TN_size(OP_result(op, 0)));
      }

      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
    
    if (!EBO_in_peep &&
	(OP_bb(op) != OP_bb(opinfo->in_op)) &&
	!TN_Is_Constant(OP_result(opinfo->in_op, 0)) &&
	has_assigned_reg(OP_result(opinfo->in_op, 0)))
    {
      if (EBO_Trace_Data_Flow)
      {
	fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
		EBO_trace_pfx);
      }
	
      return FALSE;
    }

    /* Remove the second load, and replace it with a copy of the first */
    
    if (EBO_Trace_Optimization)
    {
      fprintf(TFile,"%sRemove Load - Load combination\n",EBO_trace_pfx);
    }

    for (UINT i = 0; i < OP_results(op); i++)
      EBO_Exp_COPY(NULL, OP_result(op, i), OP_result(opinfo->in_op, i), &ops);
    
    if (EBO_in_loop)
      EBO_OPS_omega (&ops);
    
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  /* Replace the result tn of the second OP for:
     Store - Load
  */
  else if (OP_load(op) && OP_store(opinfo->in_op))
  {
    INT storeval_idx = TI_TOP_Find_Operand_Use(OP_code(opinfo->in_op),OU_storeval);
    /* The increment must be preserved. */
    if (storeval_idx < 0)
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,
		"%sStore value TN unknown for Load - Store combination\n",
		EBO_trace_pfx);
	  
      return FALSE;
    }

    TN *storeval_tn = OP_opnd(opinfo->in_op, storeval_idx);

    /* The increment must be preserved. */
    if (TI_TOP_Find_Operand_Use(OP_code(op), OU_postincr) >= 0)
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sIncremented Load for Load - Store combination\n",
		EBO_trace_pfx);
	  
      return FALSE;
    }

    /* Make sure the storeval/result TNs' regclasses and mtypes
       match. It isn't sufficient to just check regclasses since
       user-defined operations for two ctypes in the same regfile can
       have different semantics. Make an exception for 32-bit
       loads/stores to the integer register file, since we know that
       they have the same semantics for both signed and unsigned. */

    if (TN_register_class(OP_result(op,0)) !=
	TN_register_class(storeval_tn))
    {
      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sRegclass mismatch for Store - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    /* This check should be moved after the subset check below, since
       here currently prevents us reaching the subsetting... (but too
       close to release to do it now...). */
    if ((OP_code(op) != TOP_l32i) &&
	(TN_mtype(OP_result(op,0)) != TN_mtype(storeval_tn)))
    {
      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sMtype mismatch for Store - Load combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    if (!EBO_in_peep &&
	(OP_bb(op) != OP_bb(opinfo->in_op)) &&
	!TN_Is_Constant(storeval_tn) &&
	has_assigned_reg(storeval_tn))
    {
      if (EBO_Trace_Data_Flow)
	fprintf(TFile,"%sShouldn't move dedicated register references across blocks.\n",
		EBO_trace_pfx);
      
      return FALSE;
    }

    /* If the size of the data moved to and from memory is the same,
       but the size of the stored value is larger than the size of
       the value we want to load, then mask off the upper portion of
       the stored value and use that instead of the loaded value. */
    if (size_pred == size_succ)
    {
      if (TN_size(storeval_tn) > size_succ)
      {
	if (EBO_Trace_Data_Flow)
	  fprintf(TFile,"%sSize mismatch for Store - Load combination: %d %d %d\n",
		  EBO_trace_pfx,size_pred,TN_size(storeval_tn),size_succ);

	return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
      }

      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sRemove Store - Load combination\n",EBO_trace_pfx);

      EBO_Exp_COPY(NULL, OP_result(op, 0), storeval_tn, &ops);
      if (EBO_in_loop) {
	CG_LOOP_Init_Op(OPS_first(&ops));
	Set_OP_omega (OPS_first(&ops), 0, opinfo->actual_opnd[storeval_idx]->omega);
      }

      BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
      return TRUE;
    }
    /* The size of the memory accesses are different, but the starting
       memory address is the same.  There is a chance that the
       predecessor store is wider than the load. */
    else
    {
      return delete_subset_mem_op (op, opnd_tninfo, opinfo, 0, 0);
    }
  }
  /* Remove the first OP for:
     Store - Store
  */
  else if (OP_store(op) && OP_store(opinfo->in_op) &&
	   (OP_bb(op) == OP_bb(opinfo->in_op)))
  {
    if (size_pred > size_succ)
      return FALSE;

    if (opinfo->op_must_not_be_removed)
      return FALSE;

    /* The increment must be preserved. */
    if (TI_TOP_Find_Operand_Use(OP_code(opinfo->in_op), OU_postincr) >= 0)
    {
      if (EBO_Trace_Optimization)
	fprintf(TFile,"%sIncremented Store for Store - Store combination\n",
		EBO_trace_pfx);

      return FALSE;
    }

    if (EBO_Trace_Optimization)
      fprintf(TFile,"%sRemove Store - Store combination\n",EBO_trace_pfx);

    remove_op (opinfo);
    OP_Change_To_Noop(opinfo->in_op);
    opinfo->in_op = NULL;
    opinfo->in_bb = NULL;
    return FALSE;
  }
  /* Don't optimize:
     Load - Store
  */
  else {
    return FALSE;
  }

  return FALSE;
}


/* 
 * delete_duplicate_op
 *
 * For a given op and one it matches, attempt to replace 
 * one of them.
 * Return TRUE if this op is no longer needed.
 */
BOOL
delete_duplicate_op (OP *op,
		     EBO_TN_INFO **opnd_tninfo,
		     EBO_OP_INFO *opinfo)
{
  INT resnum;
  OPS ops = OPS_EMPTY;

  if (EBO_Trace_Data_Flow) {
    fprintf(TFile,"%sDuplicate OP in BB:%d    ",EBO_trace_pfx,BB_id(OP_bb(op)));
    Print_OP_No_SrcLine(op);
    fprintf(TFile,"      Matches   OP in BB:%d    ",BB_id(opinfo->in_bb));
    Print_OP_No_SrcLine(opinfo->in_op);
  }

  /* Global TN's aren't supported at low levels of optimization. */

  if ((CG_opt_level < 2) && (OP_bb(op) != opinfo->in_bb))
    return FALSE;

  for (resnum = 0; resnum < OP_results(op); resnum++) {
    TN* result_tn;
    result_tn = OP_result(op,resnum);
    TYPE_ID mtype = TN_mtype(result_tn);
    if (MTYPE_is_tie(mtype) &&
	tie_info->mtype_move_macro(mtype)->num_temps()!=0) {
      return FALSE;	// there may be no temp register for TIE types move
    } else if (MTYPE_is_xtbool(mtype)) {

      // disable for xtbool type because the simulated copy ops may have
      // been expanded and we should not create more copy ops
      // also for dedicated branch registers, we do not have proper types
      // to know whether it is xtbool or xtbool{2,4,8}
      return FALSE;
    }
  }

  /* Separate load/store processing, but logically it's just a special case. */

  if (OP_memory(op))
  {
    return delete_memory_op (op, opnd_tninfo, opinfo);
  }
  else
  {
    /* Create copies of the result TN's. */

    for (resnum = 0; resnum < OP_results(op); resnum++) {
      EBO_Exp_COPY(NULL, OP_result(op, resnum), OP_result(opinfo->in_op, resnum), &ops);
    }

    if (EBO_in_loop)
      EBO_OPS_omega (&ops);

    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    return TRUE;
  }
  
  return FALSE;
}



/* ===================================================================== */

/* Return TRUE if 'imm' is represented by a single range of
   bits. Return the index of the first and last bit in the range. */
static BOOL
Imm_Bit_Range (UINT64 imm, UINT *low_bit, UINT *high_bit)
{
  /* If 'imm' == 0 then we don't have any bit range. This test also
     makes sure the following loops terminate. */

  if (imm == 0)
    return FALSE;
  
  /* Remove low-order zeros from imm, and record the first 1 bit. */
  
  UINT start = 0;
  while ((imm & 1) == 0)
  {
    imm >>= 1;
    start++;
  }

  /* Find the last consecutive 1 bit. */

  UINT end = start;
  imm >>= 1;
  
  while ((imm & 1) == 1)
  {
    imm >>= 1;
    end++;
  }

  /* If 'imm' is now == 0, then there is only one bit range. */

  if (imm != 0)
    return FALSE;

  *low_bit = start;
  *high_bit = end;

  return TRUE;
}


/* Attempt to convert a mov 'tnr' <- 'imm_val' into a movi. Return
   TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Move (OP *op, TN *tnr, INT64 imm_val)
{
  /* If 'imm_val' doesn't fit in a movi then fail. */

  if (!TI_TOP_Can_Have_Immediate(imm_val, TOP_movi))
    return FALSE;
  
  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(TOP_movi));

  OP *new_op = Mk_OP(TOP_movi, tnr, Gen_Literal_TN(imm_val, TN_size(tnr)));
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, NULL);

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  return TRUE;
}


/* Attempt to convert an add of 'tn' + 'imm_val' into an addi. Return
   TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Add (OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
  ISA_LITCLASS add_lc;
  TOP top;
  OP *new_op = NULL;
  
  /* If 'imm_val' is 0, try a mov, otherwise try addi and addmi. */

  if (imm_val == 0)
  {
    top = TOP_mov_n;
    new_op = Mk_OP(top, tnr, tn);
    Set_OP_copy(new_op);
  }
  else if ((TI_TOP_Immediate_Operand(TOP_addi, &add_lc) == 1)
	   && (TI_ISA_LC_Value_In_Class(imm_val, add_lc)))
    top = TOP_addi;
  else if ((TI_TOP_Immediate_Operand(TOP_addmi, &add_lc) == 1)
	   && (TI_ISA_LC_Value_In_Class(imm_val, add_lc)))
    top = TOP_addmi;
  else
    return FALSE;

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  if (!new_op)
    new_op = Mk_OP(top, tnr, tn, Gen_Literal_TN(imm_val, TN_size(tnr)));

  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, tninfo, NULL);

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  return TRUE;
}


/* Attempt to convert an sub of 'tn' and 'imm_val' into a simpler
   operation. If 'rev' is TRUE, then 'op' is 'imm_val' - 'tn';
   otherwise 'op' is 'tn' - 'imm_val'. Return TRUE if we succeed,
   FALSE otherwise. */
static BOOL
Convert_Imm_Sub (OP *op, TN *tnr, TN *tn, INT64 imm_val,
		 EBO_TN_INFO *tninfo, BOOL rev)
{
  /* For 'tn' - 'imm' we can simplify as 'tn' + (-'imm'). */
  if (!rev)
    return Convert_Imm_Add(op, tnr, tn, -imm_val, tninfo);

  /* Hereafter we're just working with 'imm_val' - 'tn'. */

  TOP top;
  OP *new_op;

  /* If the immediate value is 0, then just negate 'tn'. */
  
  if (imm_val == 0)
  {
    top = TOP_neg;
    new_op = Mk_OP(top, tnr, tn);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, tninfo);
  }
  else
  {
    return FALSE;
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  return TRUE;
}


/* Attempt to convert an and of 'tn' + 'imm_val' into an mov, movi, or
   extui. Return TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_And (OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
  TOP top;
  OP *new_op;

  /* 'tn' & 0 == 0 */
  if (imm_val == 0)
  {
    top = TOP_movi;
    new_op = Mk_OP(top, tnr, Gen_Literal_TN(0, TN_size(tnr)));
  }
  /* 'tn' & 0xffffffff == 'tn' */
  else if (imm_val == -1)
  {
    top = TOP_mov_n;
    new_op = Mk_OP(top, tnr, tn);
    Set_OP_copy(new_op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, tninfo);
  }
  else {
    UINT low_bit, high_bit;
    
    /* If 'imm_val' is represented by one contiguous set of bits
       starting at 0, then we might be able to use an extract. */
    if (Imm_Bit_Range(TRUNC_32(imm_val), &low_bit, &high_bit)
	&& (low_bit == 0) && ((high_bit-low_bit) < 16))
    {
      top = TOP_extui;
      new_op = Mk_OP(top, tnr, tn, Gen_Literal_TN(low_bit, 4),
		     Gen_Literal_TN(high_bit-low_bit+1, 4));
      if (EBO_in_loop)
	EBO_Set_OP_omega(new_op, tninfo, NULL, NULL);
    }
    else
      return FALSE;
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  return TRUE;
}


/* Attempt to convert an or of 'tn' + 'imm_val' into an mov or
   movi. Return TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Or (OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
  TOP top;
  OP *new_op;

  /* 'tn' | 0 == 'tn' */
  if (imm_val == 0)
  {
    top = TOP_mov_n;
    new_op = Mk_OP(top, tnr, tn);
    Set_OP_copy(new_op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, tninfo);
  }
  /* 'tn' | 0xffffffff == 0xffffffff */
  else if (imm_val == -1)
  {
    top = TOP_movi;
    new_op = Mk_OP(top, tnr, Gen_Literal_TN(-1, TN_size(tnr)));
  }
  else {
    return FALSE;
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  return TRUE;
}


/* Attempt to convert an xor of 'tn' and 'imm_val' into a simpler
   sequence. Return TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Xor (OP *op, TN *tnr, TN *tn, INT64 imm_val, EBO_TN_INFO *tninfo)
{
  TOP top;
  OP *new_op;

  /* 'tn' ^ 0 == 'tn' */
  if (imm_val == 0)
  {
    top = TOP_mov_n;
    new_op = Mk_OP(top, tnr, tn);
    Set_OP_copy(new_op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, tninfo);
  }
  else
  {
    return FALSE;
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  return TRUE;
}

#if 0 /* NEVER CALLED */
/* Attempt to convert an src of 'tn' and 'imm_val' into a simpler
   sequence. Return TRUE if we succeed, FALSE otherwise. */
static BOOL
Convert_Imm_Src (OP *op, TN *tnr, TN *tn, TN *shift, 
		 INT64 imm_val, EBO_TN_INFO *tninfo, EBO_TN_INFO *shiftinfo)
{
  TOP top;
  OP *new_op;

  /* (0 | 'tn') >> 'shift' == 'tn' >> 'shift' */
  if (imm_val == 0)
  {
    if (TN_has_value(shift) && (TN_value(shift) < 16) && (TN_value(shift) >= 0)) {
      top = TOP_srli;
      new_op = Mk_OP(top, tnr, tn, Gen_Literal_TN(TN_value(shift), TN_size(shift)));
      if (EBO_in_loop)
	EBO_Set_OP_omega(new_op, tninfo, NULL);
    } else {
      top = TOP_srl;
      new_op = Mk_OP(top, tnr, tn, shift);
      if (EBO_in_loop)
	EBO_Set_OP_omega(new_op, tninfo, shiftinfo);
    }
  }
  else
  {
    return FALSE;
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));

  return TRUE;
}
#endif

/* Attempt to convert a conditional branch to an immediate version of
   the branch. If 'flip' is TRUE, then the branch operands must be
   flipped if we perform the conversion. Return TRUE if we succeed,
   FALSE otherwise. */
static BOOL
Convert_Imm_Branch (OP *op, TN *tn, INT64 imm_val, TN *targ,
		    EBO_TN_INFO *tninfo, EBO_TN_INFO *targinfo,
		    BOOL flip)
{
  TOP top = OP_code(op);
  TOP oldtop = top;
  OP *new_op;

  if (EBO_Trace_Execution)
    fprintf(TFile, "%s  convert branch OP :- %s, IMM :- %d\n",
            EBO_trace_pfx, TI_TOP_Name(top), imm_val);

  /* Try to convert 'top' to an immediate branch. */

  if (!TI_Equivalent_Immed_Branch(&top, &imm_val, flip))
    return FALSE;

  if (EBO_Trace_Optimization)
    fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(oldtop), TI_TOP_Name(top));

  const ISA_OPERAND_INFO *info = TI_ISA_Operand_Info(top);
  const INT ops = TI_ISA_Op_Operands(info);
  FmtAssert((ops == 2) || (ops == 3), ("expecting branch to have 2 or 3 operands, not %d", ops));
  
  if (ops == 2)
    new_op = Mk_OP(top, tn, targ);
  else
    new_op = Mk_OP(top, tn, Gen_Literal_TN(imm_val, TN_size(tn)), targ);

  OP_srcpos(new_op) = OP_srcpos(op);
  Set_OP_profile_bb_id(new_op, OP_profile_bb_id(op));
  Set_OP_unrolling(new_op, OP_unrolling(op));
  if (EBO_in_loop)
  {
    if (ops == 3)
      EBO_Set_OP_omega(new_op, tninfo, NULL, targinfo);
    else
      EBO_Set_OP_omega(new_op, tninfo, targinfo);
  }

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  return TRUE;
}


/* Attempt to convert a conditional-move that has a constant condition
   into a unconditional move or a nop. Return TRUE if we succeed,
   FALSE otherwise. */
static BOOL
Convert_Cmov (OP *op, TN *tnr, TN *src, TN *cond, EBO_TN_INFO *src_tninfo)
{
  FmtAssert(TN_has_value(cond), ("expecting constant condition"));

  INT64 cond_val = TN_value(cond);
  BOOL need_move;
  
  switch (OP_code(op))
  {
  case TOP_moveqz:
    need_move = (cond_val == 0);
    break;
    
  case TOP_movgez:
    need_move = (cond_val >= 0);
    break;

  case TOP_movltz:
    need_move = (cond_val < 0);
    break;

  case TOP_movnez:
    need_move = (cond_val != 0);
    break;

  default:
    return FALSE;
  }

  /* If 'need_move' then replace 'op' with a move, otherwise just turn
     it into a nop. */

  if (need_move)
  {
    TOP top = TOP_mov_n;
    OP *new_op = Mk_OP(top, tnr, src);
    Set_OP_copy(new_op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, src_tninfo);

    OP_srcpos(new_op) = OP_srcpos(op);
    BB_Insert_Op_After(OP_bb(op), op, new_op);

    if (EBO_Trace_Optimization)
      fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));
  }
  else
  {
    if (EBO_Trace_Optimization)
      fprintf(TFile,"\treplace %s with nop\n", TI_TOP_Name(OP_code(op)));

    OP_Change_To_Noop(op);
  }
  
  return TRUE;
}

/*
 * Look at an exression that has a constant first operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand0 (OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo,
		   INT o0_idx,
		   INT o1_idx)
{
  TOP opcode = OP_code(op);

  /* Nothing to optimize if no operands... */
  if (OP_opnds(op) < 1)
    return FALSE;

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d constant0 OP :- %s",
            EBO_trace_pfx, BB_id(OP_bb(op)),TI_TOP_Name(OP_code(op)));
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[o0_idx];
  TN *tn1 = (o1_idx >= 0) ? opnd_tn[o1_idx] : NULL;
  TN *tnr = (OP_results(op) == 0) ? NULL : OP_result(op,0);

  /* Don't mess with symbols. */
  if (TN_is_symbol(tn0))
    return FALSE;

  /* Conditional moves have two of the three operands marked as opnd1
     and opnd2, so we can reach here (operand representing the use of
     the result register is not marked). However we can't do anything
     special if 'tn0' is constant (we must have this check because
     'tn1' can also be constant when we reach here, we don't go to
     Fold_Constant_Expression because the operand representing the use
     of the result register is not constant). */
  if (TI_ISA_Property_Set(PROP_cond_move, opcode))
    return FALSE;
  
  /* We should only be called if tn0 is constant and tn1 is not. */
  FmtAssert(TN_Is_Constant(tn0) && 
	    ((OP_opnds(op) > 2) || !tn1 || !TN_Is_Constant(tn1)),
	    ("Unexpected constant/non-constant operands"));

  const INT64 imm_val = TN_Value(tn0);

  if (opcode == TOP_mov_n)
    return Convert_Imm_Move(op, tnr, imm_val);
  else if ((opcode == TOP_add) || (opcode == TOP_add_n))
    return Convert_Imm_Add(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);
  else if (opcode == TOP_addx2)
    return Convert_Imm_Add(op, tnr, tn1, imm_val*2, opnd_tninfo[o1_idx]);
  else if (opcode == TOP_addx4)
    return Convert_Imm_Add(op, tnr, tn1, imm_val*4, opnd_tninfo[o1_idx]);
  else if (opcode == TOP_addx8)
    return Convert_Imm_Add(op, tnr, tn1, imm_val*8, opnd_tninfo[o1_idx]);
  else if (TI_ISA_Property_Set(PROP_cond, opcode))
    return Convert_Imm_Branch(op, tn1, imm_val, opnd_tn[2],
			      opnd_tninfo[o1_idx], opnd_tninfo[2],
			      TRUE);
  else if (opcode == TOP_and)
    return Convert_Imm_And(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);
  else if (opcode == TOP_or)
    return Convert_Imm_Or(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);
  else if (opcode == TOP_sub)
    return Convert_Imm_Sub(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx], TRUE);
  else if (opcode == TOP_subx2)
    return Convert_Imm_Sub(op, tnr, tn1, imm_val*2, opnd_tninfo[o1_idx], TRUE);
  else if (opcode == TOP_subx4)
    return Convert_Imm_Sub(op, tnr, tn1, imm_val*4, opnd_tninfo[o1_idx], TRUE);
  else if (opcode == TOP_subx8)
    return Convert_Imm_Sub(op, tnr, tn1, imm_val*8, opnd_tninfo[o1_idx], TRUE);
  else if (opcode == TOP_xor)
    return Convert_Imm_Xor(op, tnr, tn1, imm_val, opnd_tninfo[o1_idx]);

  return FALSE;
}
  


/*
 * Look at an exression that has a constant second operand and attempt to
 * simplify the computations.
 */
BOOL
Constant_Operand1 (OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo,
		   INT o0_idx,
		   INT o1_idx)
{
  BB *bb = OP_bb(op);
  TOP opcode = OP_code(op);

  /* Nothing to optimize if no operands... */
  if (OP_opnds(op) < 1)
    return FALSE;

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d constant1 OP :- %s",
            EBO_trace_pfx, BB_id(OP_bb(op)), TI_TOP_Name(OP_code(op)));
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[o0_idx];
  TN *tn1 = opnd_tn[o1_idx];
  TN *tnr = (OP_results(op) == 0) ? NULL : OP_result(op,0);

  /* Don't mess with symbols. */
  if (TN_is_symbol(tn1))
    return FALSE;

  /* Conditional moves have two of the three operands marked as opnd1
     and opnd2, so we can reach here (operand representing the use of
     the result register is not marked). Since 'tn1' is constant, we
     can replace the cmov. We must do the conversion before the
     following FmtAssert since 'tn0' can also be constant when we
     reach here, we don't go to Fold_Constant_Expression because the
     operand representing the use of the result register is not
     constant). */
  if (TI_ISA_Property_Set(PROP_cond_move, opcode))
    return Convert_Cmov(op, tnr, OP_opnd(op, o0_idx), tn1, opnd_tninfo[o0_idx] /* this should be orig_tninfo once I pass that*/);
  
  /* We should only be called if tn1 is constant and tn0 is not. */
  FmtAssert(TN_Is_Constant(tn1) && ((OP_opnds(op) > 2) || !TN_Is_Constant(tn0)),
	    ("Unexpected constant/non-constant operands"));

  const INT64 imm_val = TN_Value(tn1);

  if ((opcode == TOP_add) || (opcode == TOP_add_n))
    return Convert_Imm_Add(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);
  else if (TI_ISA_Property_Set(PROP_cond, opcode))
    return Convert_Imm_Branch(op, tn0, imm_val, opnd_tn[2],
			      opnd_tninfo[o0_idx], opnd_tninfo[2],
			      FALSE);
  else if (opcode == TOP_and)
    return Convert_Imm_And(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);
  else if (opcode == TOP_or)
    return Convert_Imm_Or(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);
  else if (opcode == TOP_sub)
    return Convert_Imm_Sub(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx], FALSE);
  else if (opcode == TOP_xor)
    return Convert_Imm_Xor(op, tnr, tn0, imm_val, opnd_tninfo[o0_idx]);

  /*****************************************************************/
  /* Now, look for sequences ending in 'op' that can be optimized. */

  /* No opnd info if operand is constant. */
  if (opnd_tninfo[o0_idx] == NULL)
    return FALSE;

  OP *pred_op = opnd_tninfo[o0_idx]->in_op;
  if (pred_op == NULL)
    return FALSE;

  TOP pred_opcode = OP_code(pred_op);

  /* Look for a sequence of two addi that can be combined. */
  if (OP_iadd(op) && OP_iadd(pred_op))
  {
    INT ptn0_idx = TI_TOP_Find_Operand_Use(pred_opcode,OU_opnd1);
    INT ptn1_idx = TI_TOP_Find_Operand_Use(pred_opcode,OU_opnd2);
    TN *ptn0 = OP_opnd(pred_op, ptn0_idx);
    TN *ptn1 = OP_opnd(pred_op, ptn1_idx);

    if (TN_is_constant(ptn1) && !TN_is_symbol(ptn1))
    {
      EBO_OP_INFO *pred_opinfo = locate_opinfo_entry(opnd_tninfo[o0_idx]);
      EBO_TN_INFO *ptn0_tninfo = pred_opinfo->actual_opnd[ptn0_idx];

      if (EBO_tn_available(bb, ptn0_tninfo))
      {
	const INT64 new_val = imm_val + TN_Value(ptn1);
	if (Convert_Imm_Add(op, tnr, ptn0, new_val, ptn0_tninfo))
	{
	  if (EBO_Trace_Optimization)
	    fprintf(TFile,"\tcombine immediate adds\n");

	  return TRUE;
	}
      }
    }
  }

  return FALSE;
}


  
/* Look at a branch expression and attempt to evaluate the expression. 

   We can do this if the branch has all constant expressions,
   or it is a branch that compares a register with itself.
   
 */
BOOL
Resolve_Conditional_Branch (OP *op, TN **opnd_tn)
{
  BB *bb = OP_bb(op);

  /* We only concern ourselves with conditional branches, which we
     expect to have 2 successors, and a target. */
  if ((BBlist_Len(BB_succs(bb)) != 2)
      || (TI_TOP_Find_Operand_Use(OP_code(op),OU_target) < 0))
    return FALSE;

  // assuming that the 1st (and 2nd if existing) operands are the
  // comparison operands and that the last operand is the branch target
  INT num_operands = OP_opnds(op);
  if (num_operands!=2 && num_operands!=3)
    return FALSE;

  /* Evaluate the branch condition to determine if the branch is taken
     or not. */

  TN *tn0 = opnd_tn[0];
  if (!TN_has_value(tn0))
    return FALSE;

  INT64 tn0_val = TN_Value (tn0);
  UINT64 tn0_uval = TN_Value (tn0);

  TN *tn1 = NULL;
  INT64 tn1_val = 0;
  UINT64 tn1_uval = 0;

  // if there are two operands for comparison
  if (num_operands > 2)
  {
    tn1 = opnd_tn[1];
    if (!TN_has_value(tn1))
      return FALSE;

    tn1_val = TN_Value (tn1);
    tn1_uval = TN_Value (tn1);
  }
  
  BOOL taken;
  TOP top = OP_code(op);

  if (top == TOP_ball)
    taken = ((~tn0_uval & tn1_uval) == 0);
  else if (top == TOP_bnall)
    taken = ((~tn0_uval & tn1_uval) != 0);
  else if (top == TOP_bany)
    taken = ((tn0_uval & tn1_uval) != 0);
  else if (top == TOP_bnone)
    taken = ((tn0_uval & tn1_uval) == 0);
  else if ((top == TOP_bbc) || (top == TOP_bbci))
  {
    UINT bit = (tn1_uval & 0x01f);
    if (Target_Byte_Sex == BIG_ENDIAN)
      bit = 31 - bit;
    
    taken = ((tn0_uval & (1 << bit)) == 0);
  }
  else if ((top == TOP_bbs) || (top == TOP_bbsi))
  {
    UINT bit = (tn1_uval & 0x01f);
    if (Target_Byte_Sex == BIG_ENDIAN)
      bit = 31 - bit;
    
    taken = ((tn0_uval & (1 << bit)) != 0);
  }
  else if ((top == TOP_beq) || (top == TOP_beqi))
    taken = (tn0_uval == tn1_uval);
  else if ((top == TOP_beqz) || (top == TOP_beqz_n))
    taken = (tn0_uval == 0);
  else if ((top == TOP_bge) || (top == TOP_bgei))
    taken = (tn0_val >= tn1_val);
  else if ((top == TOP_bgeu) || (top == TOP_bgeui))
    taken = (tn0_uval >= tn1_uval);
  else if (top == TOP_bgez)
    taken = (tn0_val >= 0);
  else if ((top == TOP_blt) || (top == TOP_blti))
    taken = (tn0_val < tn1_val);
  else if ((top == TOP_bltu) || (top == TOP_bltui))
    taken = (tn0_uval < tn1_uval);
  else if (top == TOP_bltz)
    taken = (tn0_val < 0);
  else if ((top == TOP_bne) || (top == TOP_bnei))
    taken = (tn0_uval != tn1_uval);
  else if ((top == TOP_bnez) || (top == TOP_bnez_n))
    taken = (tn0_uval != 0);
  else
    return FALSE;

  if (EBO_Trace_Optimization)
  {
    INT opndnum = OP_opnds(op);
    fprintf(TFile, "%sin BB:%d Resolve conditional BR :- %s ",
              EBO_trace_pfx, BB_id(bb),TI_TOP_Name(OP_code(op)));
    for (UINT i = 0; i < opndnum; i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  /* Find the fall-through and target blocks. If the branch is taken,
     replace the conditional branch with an unconditional jump to the
     target. If the branch is not-taken, delete it and
     fall-through. */
  
  BB *fall_bb;
  BB *branch_bb;

  fall_bb = BB_next(bb);
  branch_bb = BBLIST_item(BB_succs(bb));
  if (branch_bb == fall_bb)
    branch_bb = BBLIST_item(BBLIST_next(BB_succs(bb)));

  if (taken)
  {
    OPS ops = OPS_EMPTY;

    Build_OP (TOP_j, OP_opnd(op,TI_TOP_Find_Operand_Use(OP_code(op),OU_target)), &ops);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    Unlink_Pred_Succ (bb, fall_bb);
    Change_Succ_Prob (bb, branch_bb, 1.0);
  }
  else
  {
    Unlink_Pred_Succ (bb, branch_bb);
    Change_Succ_Prob (bb, fall_bb, 1.0);
  }
  
  return TRUE;
}


/*
 * check if we have a branch comparing a register with itself
 * this can happen after copy propagation
 *
 */

BOOL
Resolve_Static_Branch(OP * op, TN **opnd_tn)
{
  BB *bb = OP_bb(op);

  TN *tn0 = opnd_tn[0];
  
  if (TN_has_value(tn0))
    return FALSE;

  /* We only concern ourselves with conditional branches, which we
     expect to have 2 successors, and a target. */
  if ((BBlist_Len(BB_succs(bb)) != 2)
      || (TI_TOP_Find_Operand_Use(OP_code(op),OU_target) < 0))
    return FALSE;

  // assuming that the 1st and 2nd operands are the
  // comparison operands and that the last operand is the branch target
  // if there are two operands for comparison
  INT num_operands = OP_opnds(op);
  if (num_operands != 3)
      return FALSE;
  
  TN *tn1 = opnd_tn[1];
  if (TN_has_value(tn1))
    return FALSE;

  if (!tn_registers_identical(tn0, tn1))
    return FALSE;

  BOOL taken;
  TOP top = OP_code(op);

  switch (top) 
  {
  case TOP_ball:  taken = true;  break;
  case TOP_bnall: taken = false; break;
  case TOP_bany:  taken = true;  break;
  case TOP_bnone: taken = false; break;
  case TOP_beq:   taken = true;  break;
  case TOP_bge:   taken = true;  break;
  case TOP_bgeu:  taken = true;  break;
  case TOP_blt:   taken = false; break;
  case TOP_bltu:  taken = false; break;
  case TOP_bne:   taken = false; break;

  default:
    /* all other cases are undeterminable statically */
    return FALSE;
  }

  if (EBO_Trace_Optimization)
  {
    INT opndnum = OP_opnds(op);
    fprintf(TFile, "%sin BB:%d Resolve static branch BR :- %s ",
              EBO_trace_pfx, BB_id(bb),TI_TOP_Name(OP_code(op)));
    for (UINT i = 0; i < opndnum; i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],FALSE);
    }
    fprintf(TFile,"\n");
  }

  /* Find the fall-through and target blocks. If the branch is taken,
     replace the conditional branch with an unconditional jump to the
     target. If the branch is not-taken, delete it and
     fall-through. */
  
  BB *fall_bb;
  BB *branch_bb;

  fall_bb = BB_next(bb);
  branch_bb = BBLIST_item(BB_succs(bb));
  if (branch_bb == fall_bb)
    branch_bb = BBLIST_item(BBLIST_next(BB_succs(bb)));

  if (taken)
  {
    OPS ops = OPS_EMPTY;

    Build_OP (TOP_j, OP_opnd(op,TI_TOP_Find_Operand_Use(OP_code(op),OU_target)), &ops);
    BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);
    Unlink_Pred_Succ (bb, fall_bb);
    Change_Succ_Prob (bb, branch_bb, 1.0);
  }
  else
  {
    Unlink_Pred_Succ (bb, branch_bb);
    Change_Succ_Prob (bb, fall_bb, 1.0);
  }
  
  return TRUE;
  
}

  
/*
 * Look at an exression that has all constant operands and attempt to
 * evaluate the expression. We can assume that all operands are
 * constant.
 */
BOOL
Fold_Constant_Expression (OP *op,
                          TN **opnd_tn,
                          EBO_TN_INFO **opnd_tninfo)
{
  TOP opcode = OP_code(op);
  TN *tnr = OP_result(op,0);

  if (OP_opnds(op) == 0)
    return FALSE;

  /* Only attempt to do compile-time arithmetic on integers. */

  if (TN_register_class(tnr) != TI_ISA_Regclass_Integer())
    return FALSE;

  if (EBO_Trace_Execution)
  {
    fprintf(TFile, "%sin BB:%d Constant OP :- %s ",
	    EBO_trace_pfx, BB_id(OP_bb(op)),TI_TOP_Name(opcode));

    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile," ");
      Print_TN(opnd_tn[i],TRUE);
    }
      
    fprintf(TFile,"\n");
  }

  TN *tn0 = opnd_tn[0];
  TN *tn1 = opnd_tn[1];

  INT64 result_val;
  INT64 tn0_val, tn1_val;
  UINT64 tn0_uval, tn1_uval;

  ST *result_sym = NULL;
  INT32 result_relocs;

  /* Determine the constant value of every operand. */
  
  if (TN_is_symbol(tn0))
  {
    /* Can we handle case where both tn's are symbols? How? */
    if ((OP_opnds(op) == 2) && (TN_is_symbol(tn1)))
      return FALSE;
      
    tn0_uval = TN_offset(tn0);
    tn0_val = TN_offset(tn0);
    result_sym = TN_var(tn0);
    result_relocs = TN_relocs(tn0);
  }
  else
  {
    tn0_uval = TN_Value (tn0);
    tn0_val = TN_Value (tn0);
  }

  if (OP_opnds(op) == 1)
  {
    tn1_val = 0;
    tn1_uval = 0;
  }
  else if (TN_is_symbol(tn1))
  {
    tn1_uval = TN_offset(tn1);
    tn1_val = TN_offset(tn1);
    result_sym = TN_var(tn1);
    result_relocs = TN_relocs(tn1);
  }
  else
  {
    tn1_uval = TN_Value (tn1);
    tn1_val = TN_Value (tn1);
  }

  if (!TN_has_value(tn0) || ((OP_opnds(op) > 1) && !TN_has_value(tn1)))
    return FALSE;

  /* Extracting bits from a const... */
  
  if (opcode == TOP_extui)
  {
    TN *tn2 = opnd_tn[2];
    UINT64 tn2_uval = TN_Value (tn2);

    if (!TN_has_value(tn2))
      return FALSE;

    if (tn2_uval > (32 - tn1_uval))
      return FALSE;

    result_val = (tn0_uval >> tn1_uval) & ((1 << tn2_uval) - 1);
    goto Constant_Created;
  }

  /* All the rest of the operations have at most two operands. */
  
  if (OP_opnds(op) > 2)
    return FALSE;

  /* Sign extend... */
  
  if (opcode == TOP_sext)
  {
    if (TN_is_symbol(tn0))
      return FALSE;

    if ((tn1_val < 7) || (tn1_val > 22))
      return FALSE;

    /* Do we sign extend, or zero extend... */

    result_val = tn0_uval;

    UINT32 mask = 1 << tn1_val;
    UINT sign_bit = tn0_uval & mask;
    for (UINT cnt = tn1_val; cnt < 32; cnt++)
    {
      if (sign_bit)
	result_val |= mask;
      else
	result_val &= ~mask;

      mask <<= 1;
    }

    result_val = SEXT_32(result_val);
    goto Constant_Created;
  }

  /* Absolute value... */

  if (opcode == TOP_abs)
  {
    result_val = ((tn0_val < 0) ? -tn0_val : tn0_val);
    goto Constant_Created;
  }

  /* Moves... */
  if (opcode == TOP_mov_n)
  {
    result_val = tn0_val;
    goto Constant_Created;
  }
  
  /* Addition... */

  if ((opcode == TOP_add) || (opcode == TOP_add_n)
      || (opcode == TOP_addi) || (opcode == TOP_addi_n)
      || (opcode == TOP_addmi))
  {
    result_val = tn0_val + tn1_val;
    goto Constant_Created;
  }

  if (opcode == TOP_addx2)
  {
    result_val = (tn0_val << 1) + tn1_val;
    goto Constant_Created;
  }
  else if (opcode == TOP_addx4)
  {
    result_val = (tn0_val << 2) + tn1_val;
    goto Constant_Created;
  }
  else if (opcode == TOP_addx8)
  {
    result_val = (tn0_val << 3) + tn1_val;
    goto Constant_Created;
  }

  /* Subtraction... */

  if (OP_opnds(op) > 1 && !TN_is_symbol(tn1))
  {
    if (opcode == TOP_sub)
    {
      result_val = tn0_val - tn1_val;
      goto Constant_Created;
    }
    
    if (opcode == TOP_subx2)
    {
      result_val = (tn0_val << 1) - tn1_val;
      goto Constant_Created;
    }
    else if (opcode == TOP_subx4)
    {
      result_val = (tn0_val << 2) - tn1_val;
      goto Constant_Created;
    }
    else if (opcode == TOP_subx8)
    {
      result_val = (tn0_val << 3) - tn1_val;
      goto Constant_Created;
    }
  }

  /* Logical... */
  
  if (opcode == TOP_and)
  {
    result_val = tn0_uval & tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_neg)
  {
    result_val = -tn0_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_or)
  {
    result_val = tn0_uval | tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_xor)
  {
    result_val = tn0_uval ^ tn1_uval;
    goto Constant_Created;
  }
    
  /* Min / Max... */
  
  if (opcode == TOP_max)
  {
    result_val = MAX(tn0_val, tn1_val);
    goto Constant_Created;
  }
  else if (opcode == TOP_maxu)
  {
    result_val = MAX(tn0_uval, tn1_uval);
    goto Constant_Created;
  }
  else if (opcode == TOP_min)
  {
    result_val = MIN(tn0_val, tn1_val);
    goto Constant_Created;
  }
  else if (opcode == TOP_minu)
  {
    result_val = MIN(tn0_uval, tn1_uval);
    goto Constant_Created;
  }

  /* Shift... */

  if (opcode == TOP_slli)
  {
    result_val = TRUNC_32(tn0_uval << tn1_uval);
    goto Constant_Created;
  }
  else if (opcode == TOP_srai)
  {
    result_val = tn0_val >> tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_srli)
  {
    result_val = TRUNC_32(tn0_val) >> tn1_uval;
    goto Constant_Created;
  }
  else if (opcode == TOP_src)
  {
    INT64 tn2_val = TN_Value(opnd_tn[2]);
    result_val = (TRUNC_32(tn1_val) >> tn2_val) | (tn0_val << (32-tn2_val));
    goto Constant_Created;
  }

  return FALSE;

  /* We've evaluated the expression, so replace it with the result. */

  Constant_Created:

  OPS ops = OPS_EMPTY;
  TN *tnc;

  if (result_sym != NULL)
  {
    tnc = Gen_Symbol_TN(result_sym, result_val, result_relocs);
  }
  else
  {
    tnc = Gen_Literal_TN(result_val, TN_size(tnr));
  }
  
  Expand_Immediate (tnr, tnc, OP_result_is_signed(op,0), &ops);

  /* If generating the literal requires more than one instruction,
     then just keep the original instruction. It's not clear that this
     is always the right thing, since by eliminating the instruction
     we could create dead code. */
  
  if (OP_next(OPS_first(&ops)) != NULL)
    return FALSE;

  if (EBO_in_loop)
    EBO_OPS_omega (&ops);

  BB_Insert_Ops(OP_bb(op), op, &ops, FALSE);

  if (EBO_Trace_Optimization)
  {
    fprintf(TFile, "%sin BB:%d Redefine ",
	    EBO_trace_pfx, BB_id(OP_bb(op)));
    Print_TN(tnr,TRUE);
    fprintf(TFile," with load of ");
    Print_TN(tnc,FALSE);
    fprintf(TFile, "\n");
  }

  return TRUE;
}


/*
 * CGTARG_Copy_Operand already catches most of the case we care about.
 * Here list special cases that should only be considered copies for EBO.
 */
INT
EBO_Copy_Operand (OP *op)
{
  INT opnd;

  if (OP_copy(op)) {
    return TI_ISA_Copy_Operand(OP_code(op), TRUE);
  }

  opnd = CGTARG_Copy_Operand(op);
  if (opnd >= 0) {
    return opnd;
  }

  /* For ebo we want to recognize immediate moves as being copies to
     allow constant propagation. */
  
  TOP opcode = OP_code(op);
  if ((opcode == TOP_movi)   ||
      (opcode == TOP_movi_n) ||
      (opcode == TOP_load_const))
  {
    return 0;
  }

  /* FIXME: add support for const16 */

  return -1;
}


#define SWAP_TN(a, b) (_tn_swap_tmp = a, a = b, b = _tn_swap_tmp)
#define SWAP_TNINFO(a, b) (_tninfo_swap_tmp = a, a = b, b = _tninfo_swap_tmp)
static TN *_tn_swap_tmp;
static EBO_TN_INFO *_tninfo_swap_tmp;


/* If 'sll_opinfo' is an immediate logical left shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_sll (EBO_OP_INFO *sll_opinfo, INT32 *shift =NULL)
{
  if (!sll_opinfo)
    return NULL;
  
  OP *sll = sll_opinfo->in_op;
  TOP top = OP_code(sll);
  if (top != TOP_slli)
    return NULL;

  TN *imm = OP_opnd(sll, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return sll_opinfo;
}

/* If 'sra_opinfo' is an immediate arithmetic right shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_sra (EBO_OP_INFO *sra_opinfo, INT32 *shift =NULL)
{
  if (!sra_opinfo)
    return NULL;
  
  OP *sra = sra_opinfo->in_op;
  TOP top = OP_code(sra);
  if (top != TOP_srai)
    return NULL;

  TN *imm = OP_opnd(sra, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (shift)
    *shift = TN_value(imm);
  
  return sra_opinfo;
}

/* If 'srl_opinfo' is an immediate logical right shift, return its
   EBO_OP_INFO and the immediate shift value in 'shift' (if 'shift' is
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
imm_srl (EBO_OP_INFO *srl_opinfo, INT32 *shift =NULL)
{
  if (!srl_opinfo)
    return NULL;
  
  OP *op = srl_opinfo->in_op;
  TOP top = OP_code(op);
  if (top != TOP_srli && top != TOP_extui)
    return NULL;

  TN *imm = OP_opnd(op, 1);
  if (!TN_has_value(imm))
    return NULL;
  
  if (top == TOP_extui) {
    TN *imm2 = OP_opnd(op, 2);
    if (!TN_has_value(imm2) || 
        TN_value(imm2) > 16 || 
        TN_value(imm) + TN_value(imm2) != 32)
      return NULL;
  }

  if (shift)
    *shift = TN_value(imm);
  
  return srl_opinfo;
}

/* If 'add_opinfo' is an immediate add, return its EBO_OP_INFO and the
   immediate value in 'imm' (if 'imm' is non-NULL). Return NULL
   otherwise. */
static EBO_OP_INFO *
index_add (EBO_OP_INFO *add_opinfo, TN *base, TN **index =NULL)
{
  if (!add_opinfo)
    return NULL;
  
  OP *add = add_opinfo->in_op;
  TOP top = OP_code(add);
  if ((top != TOP_add) && (top != TOP_addi) && (top != TOP_addmi))
    return NULL;

  TN *imm_tn = OP_opnd(add, 1);
  TN *other_tn = OP_opnd(add, 0);

  if (imm_tn == base) {
    if (index)
      *index = other_tn;
  } else if (other_tn == base) {
    if (index)
      *index = imm_tn;
  } else
    return NULL;
  
  return add_opinfo;
}

/* If 'srai_opinfo' is part of a sign-extend implemented as a
   slli/srai pair, return the EBO_OP_INFO of the slli and the
   immediate sext value in 'sext_imm' (if 'sext' is non-NULL). Return
   NULL otherwise. */
static EBO_OP_INFO *
decode_shift_sext (OP *srai, EBO_TN_INFO *src_tninfo, INT32 *sext_imm =NULL)
{
  if (!srai || (OP_code(srai) != TOP_srai))
    return NULL;

  /* We recognize slli t, src, imm ; srai res, t, imm as a
     sign-extend. We return the slli instruction as the sign-extend
     instruction since it contains both the 'src' TN being
     sign-extended, and the immediate. */
  EBO_OP_INFO *slli_opinfo = locate_opinfo_entry(src_tninfo);
  if (!slli_opinfo)
    return NULL;
    
  OP *slli = slli_opinfo->in_op;
  if (OP_code(slli) != TOP_slli)
    return NULL;

  TN *imm = OP_opnd(slli, 1);
  if (!TN_has_value(imm) ||
      (OP_opnd(srai, 1) != imm))
    return NULL;

  if (sext_imm)
    *sext_imm = 31 - TN_value(imm);

  return slli_opinfo;
}

/* If 'sext_opinfo' is a sign-extend, return its EBO_OP_INFO and the
   immediate sext value in 'sext_imm' (if 'sext' is non-NULL). Return
   NULL otherwise. We recognize TOP_sext as well as a slli/srai pair
   implementing a sign-extend. */
static EBO_OP_INFO *
decode_sext (EBO_OP_INFO *sext_opinfo, INT32 *sext_imm =NULL)
{
  if (!sext_opinfo)
    return NULL;
  
  OP *sext = sext_opinfo->in_op;
  TOP top = OP_code(sext);
  if (top != TOP_sext)
    return decode_shift_sext(sext, sext_opinfo->actual_opnd[0], sext_imm);

  TN *imm = OP_opnd(sext, 1);
  if (!TN_has_value(imm))
    return NULL;

  if (sext_imm)
    *sext_imm = TN_value(imm);
  
  return sext_opinfo;
}

/* If 'extui_opinfo' is an extui, return its EBO_OP_INFO and the
   immediate shift and mask values in 'shift' and 'mask' (if
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
decode_extui (EBO_OP_INFO *extui_opinfo, INT32 *shift, INT32 *mask)
{
  if (!extui_opinfo)
    return NULL;
  
  OP *extui = extui_opinfo->in_op;
  TOP top = OP_code(extui);
  if (top != TOP_extui)
    return NULL;

  TN *shift_imm = OP_opnd(extui, 1);
  TN *mask_imm = OP_opnd(extui, 2);
  if (!TN_has_value(shift_imm) || !TN_has_value(mask_imm))
    return NULL;

  if (shift)
    *shift = TN_value(shift_imm);
  if (mask)
    *mask = TN_value(mask_imm);
  
  return extui_opinfo;
}

/* If 'injui_opinfo' is an injui, return its EBO_OP_INFO and the
   immediate shift and mask values in 'shift' and 'mask' (if
   non-NULL). Return NULL otherwise. */
static EBO_OP_INFO *
decode_injui (EBO_OP_INFO *injui_opinfo, INT32 *shift, INT32 *mask)
{
  if (!injui_opinfo)
    return NULL;
  
  OP *injui = injui_opinfo->in_op;
  TOP top = OP_code(injui);
  if (top != top_injui())
    return NULL;

  TN *shift_imm = OP_opnd(injui, 2);
  TN *mask_imm = OP_opnd(injui, 3);
  if (!TN_has_value(shift_imm) || !TN_has_value(mask_imm))
    return NULL;

  if (shift)
    *shift = TN_value(shift_imm);
  if (mask)
    *mask = TN_value(mask_imm);
  
  return injui_opinfo;
}


/* If 'and_opinfo' is an and, return its EBO_OP_INFO and the
   operand tns. Return NULL otherwise. */
static EBO_OP_INFO *
decode_and (EBO_OP_INFO *and_opinfo, TN **left, TN **right)
{
  if (!and_opinfo)
    return NULL;
  
  OP *andop = and_opinfo->in_op;
  TOP top = OP_code(andop);
  if (top != TOP_and)
    return NULL;

  TN *left_tn = OP_opnd(andop, 0);
  TN *right_tn = OP_opnd(andop, 1);
  if (!TN_is_register(left_tn) || !TN_is_register(right_tn))
    return NULL;

  if (left)
    *left = left_tn;
  if (right)
    *right = right_tn;
  
  return and_opinfo;
}

static INT32
get_bbci_bbsi_imm (TN *tn, EBO_TN_INFO *tninfo)
{
  INT32 bit_pos = -1;
  TN *repl_tn = tninfo->replacement_tn;
  if (repl_tn != NULL && TN_is_constant(repl_tn) && TN_has_value(repl_tn)) {
    const INT64 val = TN_Value(repl_tn);
    for (INT i=0; i < 32; i++) {
      if ((val & (1LL << i)) != 0) {
        if (bit_pos == -1) {
          bit_pos = i;
        } else {
          bit_pos = -1;
          break;
        }
      }
    }
    if (bit_pos >= 0) {
      ISA_LITCLASS bbci_lc;  // bbci_lc is the same as bbsi_lc
      if ( (TI_TOP_Immediate_Operand(TOP_bbci, &bbci_lc) == 1) &&
           TI_ISA_LC_Value_In_Class(bit_pos, bbci_lc) ) {
        return bit_pos;
      }
    }
  }
  return -1;
}

/* Make sure 'bb' contains one or more OPs (with perhaps an
   unconditional jump) that produce a single result and that can be
   speculated. If it does, return the last OP in the sequence. Return
   the number of OPs in 'bb' (not counting any jump) in 'len'. If
   'no_define' is non-NULL, then return NULL if that TN is defined by
   any OP. If 'in_defined_tns' is non-NULL, then return NULL if any of
   those TNs are used by any OP. If 'out_defined_tns' is non-NULL,
   return the TNs defined in any OP that should not be allowed to be
   used by cmovable ops in another BB. */
static OP *
cmovable_op (BB *bb, UINT *len, TN *no_define,
	     BS *in_defined_tns, BS **out_defined_tns)
{
  *len = BB_length(bb);
  if (*len == 0)
    return NULL;
  
  OP *jump = BB_xfer_op(bb);
  if (jump && ((*len == 1) || (OP_code(jump) != TOP_j)))
    return NULL;

  *len = BB_cost_est(bb);

  /* Examine each OP, collecting the set of defined TNs. If any OP
     can't be speculated, return NULL. */

  work_gtn_set = GTN_SET_ClearD(work_gtn_set);

  OP *last = NULL;
  for (OP *op = BB_first_op(bb); op; op = OP_next(op))
  {
    if (OP_xfer(op))
      break;

    last = op;

    if (!CGTARG_Can_Be_Speculative(op))
      return NULL;

    for (UINT i = 0; i < OP_results(op); i++)
    {
      TN *res = OP_result(op, i);
      if (TN_is_register(res) && !TN_is_const_reg(res))
      {
	if (TN_is_global_reg(res))
	  work_gtn_set = GTN_SET_Union1D(work_gtn_set, res, work_pool);

	if (out_defined_tns)
	  *out_defined_tns = BS_Union1D(*out_defined_tns, TN_number(res), work_pool);
      }
    }

    if (in_defined_tns)
    {
      for (UINT i = 0; i < OP_opnds(op); i++)
      {
	TN *opnd = OP_opnd(op, i);
	if (TN_is_register(opnd) && BS_MemberP(in_defined_tns, TN_number(opnd)))
	  return NULL;
      }
    }
  }

  /* If 'last' defines not more than 1 TN, or if any of the TN's
     defined in non-'last' OPs are live out of the block, or any of
     the defined TN's are 'no_define'; then return NULL. */

  if (!last ||
      (OP_results(last) != 1) ||
      ((no_define != NULL) && GTN_SET_MemberP(work_gtn_set, no_define)))
    return NULL;
  
  work_gtn_set = GTN_SET_Difference1D(work_gtn_set, OP_result(last, 0));
  if (GTN_SET_IntersectsP(work_gtn_set, BB_live_out(bb)))
    return NULL;

  // If last's result TN is defined more than once, then return NULL.
  // See GNAT 12362
  for (OP *op = BB_first_op(bb); op != last; op = OP_next(op)) {
    if ( OP_Defs_TN(op, OP_result(last, 0)) )  {
      return NULL;
    }
  }

  if (out_defined_tns)
    *out_defined_tns = BS_Difference1D(*out_defined_tns,
					   TN_number(OP_result(last, 0)));
    
  if (OP_same_res(last))
    return NULL;

  return last;
}

/* Generate a conditional branch to 'target' and insert it after
   'op'. The generated branch is such that it will be eliminated by a
   subsequent pass of ebo. 'always_taken' indicates if the branch
   should be made to always be taken. */
static void
generate_known_branch (OP *op, TN *target, BOOL always_taken)
{
  TN *tmp = Build_TN_Of_Mtype(MTYPE_U4);
  OP *movi_op = Mk_OP(TOP_movi, tmp, Gen_Literal_TN(0, 4));
  OP *br_op = Mk_OP((always_taken) ? TOP_beqz : TOP_bnez, tmp, target);
  OP_srcpos(movi_op) = OP_srcpos(op);
  OP_srcpos(br_op) = OP_srcpos(op);
  if (EBO_in_loop)
  {
    EBO_Set_OP_omega(movi_op, NULL);
    EBO_Set_OP_omega(br_op, NULL, NULL, NULL);
  }

  BB_Insert_Op_After(OP_bb(op), op, br_op);
  BB_Insert_Op_After(OP_bb(op), op, movi_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Change branch to have known outcome\n");
}


/* Try to combine sll a, b, i ; add c, a, d into addx<2,4,8> c, b, d
   when i = 1,2,3. */
static BOOL
sll_add_sequence (OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  if (!xt_addx || ((top != TOP_add) && (top != TOP_add_n)))
    return FALSE;

  /* Both operands to the add must be registers. */

  TN *add0_tn = opnd_tn[0];
  TN *add1_tn = opnd_tn[1];
  if (!TN_is_register(add0_tn) || !TN_is_register(add1_tn))
    return FALSE;
  
  /* One of the operands to add must be defined by a sll by one, two,
     or three. If it is the second operand switch it to be the
     first. */

  INT32 shift;
  EBO_TN_INFO *add0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *add1_tninfo = opnd_tninfo[1];
  EBO_OP_INFO *sll = imm_sll(locate_opinfo_entry(add0_tninfo), &shift);
  if (!sll)
  {
    sll = imm_sll(locate_opinfo_entry(add1_tninfo), &shift);
    SWAP_TN(add0_tn, add1_tn);
    SWAP_TNINFO(add0_tninfo, add1_tninfo);
  }

  if (!sll)
    return FALSE;

  /* The shift tn must be available at the add. */

  OP *sll_op = sll->in_op;
  TN *sll_tn = OP_opnd(sll_op, 0);
  EBO_TN_INFO *sll_tninfo = sll->actual_opnd[0];

  if (!EBO_tn_available(OP_bb(op), sll_tninfo))
    return FALSE;

  /* Replace the add with an addx if 'shift' is 1, 2, or 3. */

  switch (shift)
  {
  case 1:
    top = TOP_addx2;
    break;
  case 2:
    top = TOP_addx4;
    break;
  case 3:
    top = TOP_addx8;
    break;
  default:
    return FALSE;
  }

  OP *new_op = Mk_OP(top, OP_result(op, 0), sll_tn, add1_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, sll_tninfo, add1_tninfo);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"Convert add(sll) to addx\n");

  return TRUE;
}
 
/* Try to combine sll a, b, i ; sub c, a, d into subx<2,4,8> c, b, d
   when i = 1,2,3. */
static BOOL
sll_sub_sequence (OP *op,
                  TN **opnd_tn,
                  EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  if (top != TOP_sub || !xt_addx)
    return FALSE;

  /* Both operands to the sub must be registers. */

  TN *sub0_tn = opnd_tn[0];
  TN *sub1_tn = opnd_tn[1];
  if (!TN_is_register(sub0_tn) || !TN_is_register(sub1_tn))
    return FALSE;
  
  /* The first operand to sub must be defined by a sll by one, two, or
     three. */

  INT32 shift;
  EBO_TN_INFO *sub0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *sub1_tninfo = opnd_tninfo[1];
  EBO_OP_INFO *sll = imm_sll(locate_opinfo_entry(sub0_tninfo), &shift);

  if (!sll)
    return FALSE;

  /* The shift tn must be available at the sub. */

  OP *sll_op = sll->in_op;
  TN *sll_tn = OP_opnd(sll_op, 0);
  EBO_TN_INFO *sll_tninfo = sll->actual_opnd[0];

  if (!EBO_tn_available(OP_bb(op), sll_tninfo))
    return FALSE;

  /* Replace the sub with an subx if 'shift' is 1, 2, or 3. */

  switch (shift)
  {
  case 1:
    top = TOP_subx2;
    break;
  case 2:
    top = TOP_subx4;
    break;
  case 3:
    top = TOP_subx8;
    break;
  default:
    return FALSE;
  }

  OP *new_op = Mk_OP(top, OP_result(op, 0), sll_tn, sub1_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, sll_tninfo, sub1_tninfo);
  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile,"Convert sub(sll) to subx\n");

  return TRUE;
}
 
/* Try to combine shift a, b, i ; bb<c,s>i a, ii into bb<c,s>i b, n
   where 'n' is specifies the same bit position as that specified by
   'i' and 'ii'. */
static BOOL
shift_bbi_sequence (OP *op,
		    TN **opnd_tn,
		    EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);

  /* Make sure the second operand to 'op' is an immediate. */

  TN *bbi0_tn = opnd_tn[0];
  TN *bbi1_tn = opnd_tn[1];
  if (!TN_is_register(bbi0_tn) || !TN_has_value(bbi1_tn))
    return FALSE;
  
  /* The first operand to 'op' must be defined by a srai or srli. */

  INT32 shift;
  EBO_TN_INFO *bbi0_tninfo = opnd_tninfo[0];
  EBO_OP_INFO *pshift = locate_opinfo_entry(bbi0_tninfo);
  EBO_OP_INFO *sh = imm_sra(pshift, &shift);
  if (!sh)
  {
    sh = imm_srl(pshift, &shift);
    if (!sh)
    {
      sh = imm_sll(pshift, &shift);
      shift = -shift;
    }
  }

  if (!sh)
    return FALSE;

  /* The shift tn must be available at the branch. */

  OP *shift_op = sh->in_op;
  TN *shift_tn = OP_opnd(shift_op, 0);
  EBO_TN_INFO *shift_tninfo = sh->actual_opnd[0];
  
  if (!EBO_tn_available(OP_bb(op), shift_tninfo))
    return FALSE;

  /* The new bit we want to test is 'shift' + 'bbi1_tn's value (we
     always want to manipulate the bbi value as little-endian, so we
     adjust appropriately). If the bit we are checking is > bit 31 or
     < bit 0, then we can determine the branch outcome from the sign
     of 'shift_tn' (for arithmetic shift), or we know the tested bit's
     value is 0 (for logical shift). */

  INT32 bbb = ((Target_Byte_Sex == BIG_ENDIAN) ? 31 - TN_value(bbi1_tn) : TN_value(bbi1_tn));
  INT32 tb = shift + bbb;
  if ((tb > 31) || (tb < 0))
  {
    switch (OP_code(shift_op))
    {
    case TOP_srai:
    {
      /* We just want to check the sign bit... */
      Is_True(tb > 31, ("unexpected bit position"));
      tb = 31;
      break;
    }
      
    case TOP_srli:
    case TOP_extui:
    case TOP_slli:
    {
      /* We know the tested bit is zero, so just generate a
         conditional branch that will be optimized away when ebo gets
         to it. */

      Is_True(((OP_code(shift_op) == TOP_srli ||
                OP_code(shift_op) == TOP_extui) && (tb > 31)) ||
	      ((OP_code(shift_op) == TOP_slli) && (tb < 0)), ("unexpected bit position"));

      generate_known_branch(op, opnd_tn[2], top == TOP_bbci);
      break;
    }
      
    default:
      FmtAssert(FALSE, ("unexpected immediate right shift opcode %d\n",
			TI_TOP_Name(OP_code(shift_op))));
      break;
    }
  }

  if ((tb <= 31) && (tb >= 0))
  {
    TN *test_bit = Gen_Literal_TN((Target_Byte_Sex == BIG_ENDIAN) ? 31 - tb : tb, 4);

    OP *new_op = Mk_OP(top, shift_tn, test_bit, opnd_tn[2]);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, shift_tninfo, NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);

    if (EBO_Trace_Optimization)
      fprintf(TFile, "Change tested bit for bbci/bbsi\n");
  }

  /* Change the old bbci/bbsi into a nop. */

  OP_Change_To_Noop(op);

  return TRUE;
}


/* Try to change load_const c, beq/bne into
   addmi, beqz/bnez.
*/
static BOOL
beq_branch_sequence(OP *op,
		       TN **opnd_tn,
		       EBO_TN_INFO **opnd_tninfo,
                       EBO_TN_INFO **actual_tninfo)
{
  // Cannot do this after register allocation
  if (EBO_in_peep) {
    return FALSE;
  }
  TOP top = OP_code(op);

  if ((top != TOP_beq) && (top != TOP_bne)) {
    return FALSE;
  }

  if (OP_is_no_generic(op)) {
    // if beq/bne is generated specially by EBO_Select_Branch(), 
    // don't convert it back. (can this happen ?)
    return FALSE;
  }

  TN *b0_tn = opnd_tn[0];
  TN *b1_tn = opnd_tn[1];
  EBO_TN_INFO *b0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *b1_tninfo = opnd_tninfo[1];
  TN *imm_tn, *ar_tn;
  EBO_TN_INFO *ar_tninfo, *imm_actual_tninfo;

  if (TN_is_constant(b0_tn) && TN_is_constant(b1_tn)) {
    // should be processed by other optimization
    return FALSE;
  } else {
    if (TN_is_constant(b1_tn) && TN_has_value(b1_tn)) {
      ar_tn = b0_tn;
      imm_tn = b1_tn;
      imm_actual_tninfo = actual_tninfo[1];
      ar_tninfo = b0_tninfo;
    } else if (TN_is_constant(b0_tn) && TN_has_value(b0_tn)) {
      ar_tn = b1_tn;
      imm_tn = b0_tn;
      imm_actual_tninfo = actual_tninfo[0];
      ar_tninfo = b1_tninfo;
    } else {
      return FALSE;
    }
  }

  /* if imm_tn can use movi, that is probably better than
     using addi/addmi because addi/addmi introduces more
     dependence among OPs.
   */
  if (TI_ISA_LC_Value_In_Class(TN_value(imm_tn), LC_simm12)) {
    return FALSE;
  }

  TOP sub_top;
  if (TI_ISA_LC_Value_In_Class(-TN_value(imm_tn), LC_simm8x256)) {
    /* Don't use addmi if l32r can't be removed. We don't know
       for sure wheather l32r can be removed. But if its result
       is used more than once or is global, it is likely that
       l32r cannot be removed.
    */
    if ( (imm_actual_tninfo != NULL) && 
         (   (imm_actual_tninfo->reference_count > 1)
           ||
             (imm_actual_tninfo->local_tn && 
              TN_is_global_reg(imm_actual_tninfo->local_tn))) ) {
      return FALSE;
    }
    sub_top = TOP_addmi;
  } else {
    return FALSE;
  }

  TOP brz_top = (top == TOP_bne) ? TOP_bnez : TOP_beqz;
  TN *tmp = Build_TN_Of_Mtype(MTYPE_I4);
  OP *sub_op = Mk_OP(sub_top, tmp, ar_tn, 
                     Gen_Literal_TN(-TN_value(imm_tn), TN_size(imm_tn)));
  OP *brz_op = Mk_OP(brz_top, tmp, opnd_tn[2]);
  OP_srcpos(sub_op) = OP_srcpos(op);
  OP_srcpos(brz_op) = OP_srcpos(op);
  if (EBO_in_loop) {
    EBO_Set_OP_omega(sub_op, NULL, ar_tninfo, NULL);
    EBO_Set_OP_omega(brz_op, NULL, opnd_tninfo[2]);
  }
  BB_Insert_Op_After(OP_bb(op), op, brz_op);
  BB_Insert_Op_After(OP_bb(op), op, sub_op);

  if (EBO_Trace_Optimization) {
    fprintf(TFile, 
      "<EBO:special_sequence> Change beq/bne to beqz/bnez in BB:%d\n",
      BB_id(OP_bb(op)));
  }

  OP_Change_To_Noop(op);
  return TRUE;
} 

 
/* Try to combine an extui a, b, shift, mask ; branch a into
   bb<c,s>i b, imm. */
static BOOL
extui_branch_sequence (OP *op,
		       TN **opnd_tn,
		       EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  
  /* One of the branch operands must be defined by an extui, and the
     other must be an immediate. */

  TN *b0_tn = opnd_tn[0];
  TN *b1_tn = ((OP_opnds(op) == 3) ? opnd_tn[1] : NULL);
  TN *target_tn = ((OP_opnds(op) == 3) ? opnd_tn[2] : opnd_tn[1]);
  EBO_TN_INFO *b0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *b1_tninfo = ((OP_opnds(op) == 3) ? opnd_tninfo[1] : NULL);

  INT32 shift, mask;
  EBO_OP_INFO *extui = decode_extui(locate_opinfo_entry(b0_tninfo), &shift, &mask);
  if (!extui && b1_tn)
  {
    SWAP_TN(b0_tn, b1_tn);
    SWAP_TNINFO(b0_tninfo, b1_tninfo);
    
    extui = decode_extui(locate_opinfo_entry(b0_tninfo), &shift, &mask);
  }

  if (!extui)
    return FALSE;

  if (b1_tn && !TN_has_value(b1_tn))
    return FALSE;

  /* The extui tn must be available at the branch. */

  INT64 imm = (b1_tn) ? TN_value(b1_tn) : 0;
  OP *extui_op = extui->in_op;
  TN *extui_tn = OP_opnd(extui_op, 0);
  EBO_TN_INFO *extui_tninfo = extui->actual_opnd[0];
  
  if (!EBO_tn_available(OP_bb(op), extui_tninfo))
    return FALSE;

  /* Determine the new branch opcode and arguments... */

  TOP newtop = TOP_UNDEFINED;
  BOOL replaced = FALSE;
  TN *newop0, *newop1, *newop2;
  EBO_TN_INFO *newtninfo0;
  
  switch (top)
  {
  case TOP_beqz:
    // if beqz was generated before (by EBO_Select_Branch(), etc) due 
    // to that 8-bit label in beq isn't enough, we should not convert it
    // back to instruction bbci/bbsi of 8-bit label.
    if (OP_is_no_generic(op)) break;
  case TOP_beqi:
  case TOP_beq:
  {
    /* If the extui 'mask' is 1, and 'imm' is 0 or 1, then we can use
       a bbci or bbsi. */
    if ((mask == 1) && ((imm == 0) || (imm == 1)))
    {
      newtop = ((imm == 0) ? TOP_bbci : TOP_bbsi);
      newop0 = extui_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == BIG_ENDIAN) ? 31 - shift : shift, 4); 
      newop2 = target_tn;
      newtninfo0 = extui_tninfo;
      break;
    }
    
    /* For other 'mask' values, we know the branch will always be
       false if 'imm' cannot be encoded in the field size of
       'mask'. */
    if ((imm < 0) || (imm >= (1 << mask)))
    {
      generate_known_branch(op, target_tn, FALSE);
      replaced = TRUE;
    }
    break;
  }
    
  case TOP_bnez:
    // if beqz was generated before (by EBO_Select_Branch(), etc) due 
    // to that 8-bit label in beq isn't enough, we should not convert it
    // back to instruction bbci/bbsi of 8-bit label.
    if (OP_is_no_generic(op)) break;
  case TOP_bnei:
  case TOP_bne:
  {
    /* If the extui 'mask' is 1, and 'imm' is 0 or 1, then we can use
       a bbci or bbsi. */
    if ((mask == 1) && ((imm == 0) || (imm == 1)))
    {
      newtop = ((imm == 0) ? TOP_bbsi : TOP_bbci);
      newop0 = extui_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == BIG_ENDIAN) ? 31 - shift : shift, 4); 
      newop2 = target_tn;
      newtninfo0 = extui_tninfo;
      break;
    }
    
    /* For other 'mask' values, we know the branch will always be
       true if 'imm' cannot be encoded in the field size of
       'mask'. */
    if ((imm < 0) || (imm >= (1 << mask)))
    {
      generate_known_branch(op, target_tn, TRUE);
      replaced = TRUE;
    }
    break;
  }
  
  /****** Could have more cases here (e.g. we know that the extracted
	  value is always positive). But be sure to be careful
	  handling cases where operands are swapped above. *******/
  }

  if (newtop != TOP_UNDEFINED)
  {
    OP *new_op = Mk_OP(newtop, newop0, newop1, newop2);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, newtninfo0, NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    replaced = TRUE;
    
    if (EBO_Trace_Optimization)
      fprintf(TFile, "Change extui/branch to branch\n");
  }

  /* Change the old branch into a nop if we replaced it. */

  if (replaced)
    OP_Change_To_Noop(op);

  return replaced;
}
 
/* Try to combine an slli a, b, imm0 ; branch a, imm1 into branch b,
   imm2. */
static BOOL
sll_branch_sequence (OP *op,
		     TN **opnd_tn,
		     EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  BOOL swapped = FALSE;
  
  /* One of the branch operands must be defined by a slli, and the
     other must be an immediate. */

  TN *b0_tn = opnd_tn[0];
  TN *b1_tn = ((OP_opnds(op) == 3) ? opnd_tn[1] : NULL);
  TN *target_tn = ((OP_opnds(op) == 3) ? opnd_tn[2] : opnd_tn[1]);
  EBO_TN_INFO *b0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *b1_tninfo = ((OP_opnds(op) == 3) ? opnd_tninfo[1] : NULL);

  INT32 shift;
  EBO_OP_INFO *sll = imm_sll(locate_opinfo_entry(b0_tninfo), &shift);
  if (!sll && b1_tn)
  {
    SWAP_TN(b0_tn, b1_tn);
    SWAP_TNINFO(b0_tninfo, b1_tninfo);
    swapped = TRUE;
    
    sll = imm_sll(locate_opinfo_entry(b0_tninfo), &shift);
  }

  if (!sll)
    return FALSE;

  if (b1_tn && !TN_has_value(b1_tn))
    return FALSE;

  /* The sll tn must be available at the branch. */

  INT64 imm = (b1_tn) ? TN_value(b1_tn) : 0;
  OP *sll_op = sll->in_op;
  TN *sll_tn = OP_opnd(sll_op, 0);
  EBO_TN_INFO *sll_tninfo = sll->actual_opnd[0];
  
  if (!EBO_tn_available(OP_bb(op), sll_tninfo))
    return FALSE;
  
  /* Determine the new branch opcode and arguments... */

  TOP newtop = TOP_UNDEFINED;
  BOOL replaced = FALSE;
  TN *newop0, *newop1, *newop2;
  EBO_TN_INFO *newtninfo0;
  
  switch (top)
  {
  case TOP_bgez:
  case TOP_bgei:
  case TOP_bge:
  {
    /* If 'imm' == 0 (-1 if swapped), we can just test the bit that
       becomes the sign bit after the shift. */
    if ((!swapped && (imm == 0)) || (swapped && (imm == -1)))
    {
      newtop = swapped ? TOP_bbsi : TOP_bbci;
      newop0 = sll_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == LITTLE_ENDIAN) ? 31 - shift : shift, 4); 
      newop2 = target_tn;
      newtninfo0 = sll_tninfo;
    }
    
    break;
  }
    
  case TOP_bltz:
  case TOP_blti:
  case TOP_blt:
  {
    /* If 'imm' == 0 (-1 if swapped), we can just test the bit that
       becomes the sign bit after the shift. */
    if ((!swapped && (imm == 0)) || (swapped && (imm == -1)))
    {
      newtop = swapped ? TOP_bbci : TOP_bbsi;
      newop0 = sll_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == LITTLE_ENDIAN) ? 31 - shift : shift, 4); 
      newop2 = target_tn;
      newtninfo0 = sll_tninfo;
    }
    
    break;
  }
  }

  if (newtop != TOP_UNDEFINED)
  {
    OP *new_op = Mk_OP(newtop, newop0, newop1, newop2);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, newtninfo0, NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    replaced = TRUE;
    
    if (EBO_Trace_Optimization)
      fprintf(TFile, "Change sll/branch to branch\n");
  }

  /* Change the old branch into a nop if we replaced it. */

  if (replaced)
    OP_Change_To_Noop(op);

  return replaced;
}
 
/* Try to combine an and a, b, c ; branch a, imm1 into 
   bany/bnone/bbci/bbsi b, c.
 */
static BOOL
and_branch_sequence (OP *op,
		     TN **opnd_tn,
		     EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  BOOL swapped = FALSE;
  
  /* One of the branch operands must be defined by an and, and the
     other must be an immediate. */

  TN *b0_tn = opnd_tn[0];
  TN *b1_tn = ((OP_opnds(op) == 3) ? opnd_tn[1] : NULL);
  TN *target_tn = ((OP_opnds(op) == 3) ? opnd_tn[2] : opnd_tn[1]);
  EBO_TN_INFO *b0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *b1_tninfo = ((OP_opnds(op) == 3) ? opnd_tninfo[1] : NULL);

  TN *and_tn0, *and_tn1;
  EBO_OP_INFO *and_opinfo = decode_and(locate_opinfo_entry(b0_tninfo),
				       &and_tn0, &and_tn1);
  if (!and_opinfo && b1_tn)
  {
    SWAP_TN(b0_tn, b1_tn);
    SWAP_TNINFO(b0_tninfo, b1_tninfo);
    swapped = TRUE;
    
    and_opinfo = decode_and(locate_opinfo_entry(b0_tninfo),
			    &and_tn0, &and_tn1);
  }

  if (!and_opinfo)
    return FALSE;

  if (b1_tn && !TN_has_value(b1_tn))
    return FALSE;

  /* The and tns must be available at the branch. */

  INT64 imm = (b1_tn) ? TN_value(b1_tn) : 0;
  OP *and_op = and_opinfo->in_op;
  EBO_TN_INFO *and_tninfo0 = and_opinfo->actual_opnd[0];
  EBO_TN_INFO *and_tninfo1 = and_opinfo->actual_opnd[1];
  
  if (!EBO_tn_available(OP_bb(op), and_tninfo0) ||
      !EBO_tn_available(OP_bb(op), and_tninfo1))
    return FALSE;
  
  /* Determine the new branch opcode and arguments... */

  TOP newtop = TOP_UNDEFINED;
  BOOL replaced = FALSE;
  TN *newop0 = NULL, *newop1 = NULL, *newop2 = NULL;
  EBO_TN_INFO *newtninfo0 = NULL, *newtninfo1 = NULL;
  INT32 bbci_bbsi_imm;
  
  switch (top)
  {
  case TOP_beqz:
  case TOP_beqi:
  case TOP_beq:
  {
    /* If 'imm' == 0, we can just test if no bits are set in either of
       the anded tns. */
    if (imm == 0)
    {
      if ((bbci_bbsi_imm = get_bbci_bbsi_imm (and_tn0, and_tninfo0)) >= 0) {
        newtop = TOP_bbci;
        newop0 = and_tn1;
        newop1 = Gen_Literal_TN (
          (Target_Byte_Sex == BIG_ENDIAN) ? 31 - bbci_bbsi_imm : bbci_bbsi_imm, 4);
        newop2 = target_tn;
        newtninfo0 = and_tninfo1;
        newtninfo1 = NULL;
      } else if ((bbci_bbsi_imm = get_bbci_bbsi_imm (and_tn1, and_tninfo1)) >= 0) {
        newtop = TOP_bbci;
        newop0 = and_tn0;
        newop1 = Gen_Literal_TN (
          (Target_Byte_Sex == BIG_ENDIAN) ? 31 - bbci_bbsi_imm : bbci_bbsi_imm, 4);
        newop2 = target_tn;
        newtninfo0 = and_tninfo0;
        newtninfo1 = NULL;
      } else {
        newtop = TOP_bnone;
        newop0 = and_tn0;
        newop1 = and_tn1;
        newop2 = target_tn;
        newtninfo0 = and_tninfo0;
        newtninfo1 = and_tninfo1;
      }
    }
    
    break;
  }
    
  case TOP_bnez:
  case TOP_bnei:
  case TOP_bne:
  {
    /* If 'imm' == 0, we can just test if any bits are set in either of
       the anded tns. */
    if (imm == 0)
    {
      if ((bbci_bbsi_imm = get_bbci_bbsi_imm (and_tn0, and_tninfo0)) >= 0) {
        newtop = TOP_bbsi;
        newop0 = and_tn1;
        newop1 = Gen_Literal_TN (
          (Target_Byte_Sex == BIG_ENDIAN) ? 31 - bbci_bbsi_imm : bbci_bbsi_imm, 4);
        newop2 = target_tn;
        newtninfo0 = and_tninfo1;
        newtninfo1 = NULL;
      } else if ((bbci_bbsi_imm = get_bbci_bbsi_imm (and_tn1, and_tninfo1)) >= 0) {
        newtop = TOP_bbsi;
        newop0 = and_tn0;
        newop1 = Gen_Literal_TN (
          (Target_Byte_Sex == BIG_ENDIAN) ? 31 - bbci_bbsi_imm : bbci_bbsi_imm, 4);
        newop2 = target_tn;
        newtninfo0 = and_tninfo0;
        newtninfo1 = NULL;
      } else {
        newtop = TOP_bany;
        newop0 = and_tn0;
        newop1 = and_tn1;
        newop2 = target_tn;
        newtninfo0 = and_tninfo0;
        newtninfo1 = and_tninfo1;
      }
    }
    
    break;
  }
  }

  if (newtop != TOP_UNDEFINED)
  {
    OP *new_op = Mk_OP(newtop, newop0, newop1, newop2);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, newtninfo0, newtninfo1, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    replaced = TRUE;
    
    if (EBO_Trace_Optimization)
      fprintf(TFile, "Change and/branch to branch\n");
  }

  /* Change the old branch into a nop if we replaced it. */

  if (replaced)
    OP_Change_To_Noop(op);

  return replaced;
}
 
/* Try to combine an sext a, b, imm ; branch a, imm1 into bbsi/bbci b, imm2. */
static BOOL
sext_branch_sequence (OP *op,
		      TN **opnd_tn,
		      EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  BOOL swapped = FALSE;
  
  /* One of the branch operands must be defined by a sext, and the
     other must be an immediate. */

  TN *b0_tn = opnd_tn[0];
  TN *b1_tn = ((OP_opnds(op) == 3) ? opnd_tn[1] : NULL);
  TN *target_tn = ((OP_opnds(op) == 3) ? opnd_tn[2] : opnd_tn[1]);
  EBO_TN_INFO *b0_tninfo = opnd_tninfo[0];
  EBO_TN_INFO *b1_tninfo = ((OP_opnds(op) == 3) ? opnd_tninfo[1] : NULL);

  INT32 sext_imm;
  EBO_OP_INFO *sext = decode_sext(locate_opinfo_entry(b0_tninfo), &sext_imm);
  if (!sext && b1_tn)
  {
    SWAP_TN(b0_tn, b1_tn);
    SWAP_TNINFO(b0_tninfo, b1_tninfo);
    swapped = TRUE;
    
    sext = decode_sext(locate_opinfo_entry(b0_tninfo), &sext_imm);
  }

  if (!sext)
    return FALSE;

  if (b1_tn && !TN_has_value(b1_tn))
    return FALSE;

  /* The sext register tn must be available at the branch. */

  INT64 imm = (b1_tn) ? TN_value(b1_tn) : 0;
  OP *sext_op = sext->in_op;
  TN *sext_tn = OP_opnd(sext_op, 0);
  EBO_TN_INFO *sext_tninfo = sext->actual_opnd[0];
  
  if (!EBO_tn_available(OP_bb(op), sext_tninfo))
    return FALSE;
  
  /* Determine the new branch opcode and arguments... */

  TOP newtop = TOP_UNDEFINED;
  BOOL replaced = FALSE;
  TN *newop0, *newop1, *newop2;
  EBO_TN_INFO *newtninfo0;
  
  switch (top)
  {
  case TOP_bgez:
  case TOP_bgei:
  case TOP_bge:
  {
    /* If 'imm' == 0 (-1 if swapped), we can just test the bit that
       becomes the sign bit after the sext. */
    if ((!swapped && (imm == 0)) || (swapped && (imm == -1)))
    {
      newtop = swapped ? TOP_bbsi : TOP_bbci;
      newop0 = sext_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == BIG_ENDIAN) ? 31 - sext_imm : sext_imm, 4); 
      newop2 = target_tn;
      newtninfo0 = sext_tninfo;
    }
    
    break;
  }
    
  case TOP_bltz:
  case TOP_blti:
  case TOP_blt:
  {
    /* If 'imm' == 0 (-1 if swapped), we can just test the bit that
       becomes the sign bit after the sext. */
    if ((!swapped && (imm == 0)) || (swapped && (imm == -1)))
    {
      newtop = swapped ? TOP_bbci : TOP_bbsi;
      newop0 = sext_tn;
      newop1 = Gen_Literal_TN((Target_Byte_Sex == BIG_ENDIAN) ? 31 - sext_imm : sext_imm, 4); 
      newop2 = target_tn;
      newtninfo0 = sext_tninfo;
    }
    
    break;
  }
  }

  if (newtop != TOP_UNDEFINED)
  {
    OP *new_op = Mk_OP(newtop, newop0, newop1, newop2);
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, newtninfo0, NULL, NULL);
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    replaced = TRUE;
    
    if (EBO_Trace_Optimization)
      fprintf(TFile, "Change sext/branch to branch\n");
  }

  /* Change the old branch into a nop if we replaced it. */

  if (replaced)
    OP_Change_To_Noop(op);

  return replaced;
}

static BOOL BB_has_mov_only (BB *bb)
{
  OP *op=BB_last_op(bb);
  if ((op != NULL) && OP_xfer(op)) {
    op = OP_prev(op);
  }
  if (op) {
    if ((OP_code(op) == TOP_mov_n) &&
        (OP_prev(op) == NULL)) {
      return TRUE;
    }
  }
  return FALSE;
}

/*
   Estimate the cycle cost for BB 'bb'. Assume every OP
   has one cycle except for div/rem OPs.
*/
static INT BB_cost_est(BB *bb)
{
#define SLOW_DIVIDE_LATENCY	12	
  INT c = 0;
  if (xt_div32) {
    for (OP *op = BB_first_op(bb); op; op = OP_next(op)) {
      if (OP_code(op) == TOP_quos || OP_code(op) == TOP_quou ||
	  OP_code(op) == TOP_rems || OP_code(op) == TOP_remu)
      {
        c += SLOW_DIVIDE_LATENCY;
      }
    }
  }

  return BB_length(bb) + c;
}
 
/* Try to convert a branch to a cmov. */
static BOOL
cmov_sequence (OP *op,
	       TN **opnd_tn,
	       EBO_TN_INFO **opnd_tninfo)
{
  /* Liveness info across blocks not available at lower optimization levels */
  if (CG_opt_level < 2)
    return FALSE;

  /* Can't perform this optimization after register allocation because we
     need to create a new TN. */
  if (EBO_in_peep)
    return FALSE;

  /* Can't perform this optimization before the frequency info is available */
  if (FREQ_freqs_computed==false)
    return FALSE;

  BOOL do_sub = FALSE; 
  BOOL need_sub = FALSE;
  TOP sub_top = TOP_UNDEFINED;
  TOP cmov_top = TOP_UNDEFINED;
  TOP cmov_top_same, cmov_top_invert;
  TOP top = OP_code(op);
  BB *bb = OP_bb(op);
  TN *target_tn = NULL;

  /* We can only convert branches that have direct equivalents in the
     cmov world, i.e. beqz, bnez, bltz, bgez. Ebo will already have
     created these forms of the branches if possible, so we don't have
     to check for beq with a constant tn == 0, for example. */

  switch (top)
  {
  case TOP_beqz:
    cmov_top_same = TOP_moveqz;
    cmov_top_invert = TOP_movnez;
    break;
    
  case TOP_bnez:
    cmov_top_same = TOP_movnez;
    cmov_top_invert = TOP_moveqz;
    break;
    
  case TOP_bgez:
    cmov_top_same = TOP_movgez;
    cmov_top_invert = TOP_movltz;
    break;

  case TOP_beqi:
  case TOP_bnei:
    TN *imm_tn = opnd_tn[1];
    Is_True(TN_is_constant(imm_tn) && TN_has_value(imm_tn),
	    ("TOP_beqi/TOP_bnei does not have constant immediate"));
    if (TI_ISA_LC_Value_In_Class(-TN_value(imm_tn), LC_simm8)) {
      sub_top = TOP_addi;
    } else if (TI_ISA_LC_Value_In_Class(-TN_value(imm_tn), LC_simm8x256)) {
      sub_top = TOP_addmi;
    } else {
      return FALSE;
    }
    need_sub = TRUE;
    if (top == TOP_beqi) {
      cmov_top_same = TOP_moveqz;
      cmov_top_invert = TOP_movnez;
    } else {
      cmov_top_same = TOP_movnez;
      cmov_top_invert = TOP_moveqz;
    }
    target_tn = opnd_tn[2];
    break;
    
  case TOP_beq:
    need_sub = TRUE;
    sub_top = TOP_sub;
    cmov_top_same = TOP_moveqz;
    cmov_top_invert = TOP_movnez;
    target_tn = opnd_tn[2];
    break;
    
  case TOP_bne:
    need_sub = TRUE;
    sub_top = TOP_sub;
    cmov_top_same = TOP_movnez;
    cmov_top_invert = TOP_moveqz;
    target_tn = opnd_tn[2];
    break;

  case TOP_bltz:
    cmov_top_same = TOP_movltz;
    cmov_top_invert = TOP_movgez;
    break;

  case TOP_bt:
    cmov_top_same = TOP_movt;
    cmov_top_invert = TOP_movf;
    break;

  case TOP_bf:
    cmov_top_same = TOP_movf;
    cmov_top_invert = TOP_movt;
    break;

  default:
    return FALSE;
  }

  /* If target_tn is NULL, the branch must have
     two operands, not three !
   */
  if (target_tn == NULL) {
    target_tn = opnd_tn[1];
  }

  /* Case 1:

       a = op0                a = op0
       ...                    ...
       br x, L1       or      br x, L2
       a = op1              L1:  
     L1:                      ...
                            L2:
			      a = op1
			      j L1

     can be converted to:

       a = op0
       ...
       b = op1
       cmov a, b, x

     if op1 can be speculated.
  */

  /* Case 2:

       ...                   ...  
       br x, L1              br x, L1
       a = op0               a = op0 
       j L2         or     L2:
     L1:                     ...
       a = op1             L1:
     L2:                     a = op1
       ...                   j L2

     can be converted to:

       b = op1
       a = op0
       cmov a, b, x

     if op0 and op1 can be speculated.
  */

  /* We can see a branch with a single successors (i.e. branch to the
     fall-through) before cflow. */
  if (BB_succs_len(bb) != 2)
    return FALSE;
  
  BB *fall_through = BB_next(bb);
  if (!fall_through || (BB_Find_Succ(bb, fall_through) == NULL))
    return FALSE;

  BB *target = BB_First_Succ(bb);
  if (target == fall_through)
    target = BBLIST_item(BBLIST_next(BB_succs(bb)));
  if (!target || (target == fall_through))
    return FALSE;

  BB *cmov_bb0, *cmov_bb1;
  BOOL force_taken;
  BBLIST* fall_through_list = BBlist_Find_BB(BB_succs(bb), fall_through);
  BBLIST* target_list = BBlist_Find_BB(BB_succs(bb), target);
  float prob_fall_through = BBLIST_prob(fall_through_list);
  float prob_taken = BBLIST_prob(target_list);
  float old_cost, new_cost;

  // the following has to match what happens in
  // CG_Convert_To_Predicted_Branch()
  BOOL predicted_taken = prob_taken >= xt_brt_threshold;

  // the following numbers are cost in addition to the branch itselef
  // for each branch scenarios
  INT mis_predicted_fall_through_cost = xt_e_stage + xt_i_latency;
  INT correctly_predicted_fall_through_cost = 0;
  INT mis_predicted_taken_cost = xt_e_stage + xt_i_latency;
  INT correctly_predicted_taken_cost = xt_b_stage + xt_i_latency;
  
  if ((BB_Unique_Successor(fall_through) == target) &&
      (BB_Unique_Predecessor(fall_through) == bb))
  {
    /* Case 1 */
    cmov_bb1 = fall_through;
    cmov_bb0 = NULL;
    cmov_top = cmov_top_invert;
    force_taken = FALSE;

    if (predicted_taken) {
      old_cost = 1.0 +
	         prob_taken *
	      		correctly_predicted_taken_cost +
	      	 prob_fall_through *
		 	(mis_predicted_fall_through_cost +
			 BB_cost_est(fall_through)) ;
    } else {
      old_cost = 1.0 +
	         prob_taken *
	      		mis_predicted_taken_cost +
	      	 prob_fall_through *
		 	(correctly_predicted_fall_through_cost +
		  	 BB_cost_est(fall_through));
    }
    if (BB_branch_op(fall_through)) {
      old_cost += prob_fall_through * correctly_predicted_taken_cost;
    } else {
      old_cost += prob_fall_through * correctly_predicted_fall_through_cost;
    }
  }
  else if ((BB_Unique_Successor(target) == fall_through) &&
	   (BB_Unique_Predecessor(target) == bb))
  {
    /* Case 1 */
    cmov_bb1 = target;
    cmov_bb0 = NULL;
    cmov_top = cmov_top_same;
    force_taken = TRUE;

    if (predicted_taken) {
      old_cost = 1.0 +
	         prob_taken *
	      		(correctly_predicted_taken_cost + BB_cost_est(target)) +
	      	 prob_fall_through *
		 	(mis_predicted_fall_through_cost) * prob_fall_through;
    } else {
      old_cost = 1.0 +
	         prob_taken *
	      		(mis_predicted_taken_cost + BB_cost_est(target)) +
	      	 prob_fall_through *
		 	(correctly_predicted_fall_through_cost);
    }

    Is_True(BB_branch_op(target),("Bad control flow"));
    old_cost += prob_taken * correctly_predicted_taken_cost;
  }
  else if ((BB_Unique_Successor(fall_through) != NULL) &&
	   (BB_Unique_Successor(fall_through) == BB_Unique_Successor(target)) &&
	   (BB_Unique_Predecessor(fall_through) == bb) &&
	   (BB_Unique_Predecessor(target) == bb))
  {
    /* Case 2 */
    cmov_bb1 = target;
    cmov_bb0 = fall_through;
    cmov_top = cmov_top_same;
    force_taken = TRUE;

    if (predicted_taken) {
      old_cost = 1.0 +
	         prob_taken *
	      		(correctly_predicted_taken_cost + BB_cost_est(target)) +
	      	 prob_fall_through *
		 	(mis_predicted_fall_through_cost +
			 BB_cost_est(fall_through));
    } else {
      old_cost = 1.0 +
	         prob_taken *
	      		(mis_predicted_taken_cost + BB_cost_est(target)) +
	      	 prob_fall_through *
		 	(correctly_predicted_fall_through_cost +
			 BB_cost_est(fall_through));
    }
    if (BB_branch_op(fall_through)) {
      old_cost += prob_fall_through * correctly_predicted_taken_cost;
    }
    if (BB_branch_op(target)) {
      old_cost += prob_taken * correctly_predicted_taken_cost;
    }

    do_sub = TRUE;
    if (OPT_Space) {
      /* When one of target/fall-thru has only mov OP, that mov
         will be likely removed by assigning to the same register.
         Thus, doing cmov will likely increase the size. 

        For example, given:
           beqi a4, 4, L
            addi a4, a6, 4
            j Q
         L: mov a4, a3
         Q:

         If doing cmov, it would be
            addi a4, a6, 4
            addi a2, a4, -4
            moveqz a4, a3, a2

         If not doing cmov, it would be:
            < assume a3 has been replaced with a4>
            beqi a4, 4, Q
              addi a4, a6, 4
          Q:

         So, not doing cmov results in a smaller code size.

         (Besize, when introducing more TN, it would likely increase
          register pressure. We probably should consider this...)
      */
      if (BB_has_mov_only(target) || BB_has_mov_only(fall_through)) {
        do_sub = FALSE;
      }
    }
  }
  else
    return FALSE;

  TN *x = opnd_tn[0];
  if (need_sub || 
      TN_is_register(x) && TN_is_dedicated(x)) {
    // Need to create a new TN x before creating cmov
    x = NULL;
  }
  /* Make sure 'cmov_bb1', and 'cmov_bb0' if it is non-NULL, contains
     one or more instructions (with perhaps an unconditional jump)
     that produce only a single result. And that all the instructions
     can be speculated. The last OP in the sequence is returned. */

  if ( OPT_Space && need_sub && !do_sub ) {
    // Under -Os,  do beq/bne for case 2 only
    return FALSE;
  }


  UINT len0=0, len1=0;

  /* If any of the OPs in 'cmov_bb1' define 'x', then we can't cmov
     them, since 'x' must be live down to the cmov. */
  work_defined_set = BS_ClearD(work_defined_set);
  OP *op1 = cmovable_op(cmov_bb1, &len1, x, NULL, &work_defined_set);
  if (!op1)
    return FALSE;
  
  OP *op0 = NULL;
  if (cmov_bb0)
  {
    /* If any of the OPs in 'cmov_bb0' define 'x', then we can't cmov
       them, since 'x' must be live down to the cmov. If any of the
       TNs defined in 'cmov_bb1' are used in 'cmov_bb0', then we can't
       cmov, since we are moving 'cmov_bb0' after 'cmov_bb1'. */
    op0 = cmovable_op(cmov_bb0, &len0, x, work_defined_set, NULL);
    if (!op0)
      return FALSE;
  }

  new_cost = (float)(len0 + len1 + 1);
  if (need_sub) {
    new_cost += 1.0f;
  }

  /* Is a cmov conversion profitable, if not don't do it. */
  if (!OPT_Space && new_cost >= old_cost) {
      return FALSE;
  }
    
  /* Change 'op1' to store it's result in 'b', and insert all the OPs
     from 'cmov_bb0' and the cmov into 'cmov_bb1'.*/

  Is_True((!op0 || (OP_results(op0) == 1)) &&
	  (OP_results(op1) == 1), ("expecting one result"));
  Is_True(cmov_top != TOP_UNDEFINED, ("expecting cmov topcode"));
  
  if ((op0 &&
       (TN_is_xtbool(OP_result(op0,0)) || 
	TN_is_tie(OP_result(op0,0)))) || 
      TN_is_xtbool(OP_result(op1, 0)) ||
      TN_is_tie(OP_result(op1, 0)))
    return FALSE;

  /* For now, this optimization only deals with two blocks that write the
     same result. */
  if (op0 && (OP_result(op0, 0) != OP_result(op1, 0)))
    return FALSE;

  TN *a = OP_result(op1, 0);

  /* TENSILICA: the result has to be in AR register if no FP-CP Option; */
  if ( ! ( (xt_hard_float && TN_mtype(a) == MTYPE_F4) ||
           MTYPE_is_integral(TN_mtype(a)) ) )
    return FALSE;

  if ( xt_hard_float && TN_mtype(a) == MTYPE_F4 ) {
    switch (cmov_top)
    {
    case TOP_movnez:
      cmov_top =  TI_TOP_Topcode("movnez.s");
      break;
      
    case TOP_moveqz:
      cmov_top = TI_TOP_Topcode("moveqz.s");
      break;
      
    case TOP_movltz:
      cmov_top = TI_TOP_Topcode("movltz.s");
      break;
      
    case TOP_movgez:
      cmov_top = TI_TOP_Topcode("movgez.s");
      break;
    case TOP_movt:
      cmov_top = TI_TOP_Topcode("movt.s");
      break;
    case TOP_movf:
      cmov_top = TI_TOP_Topcode("movf.s");
      break;
    }
  }

  TN* b = Build_TN_Of_Mtype(TN_mtype(a));
  OP *insert = op1;

  Set_OP_result(op1, 0, b);

  if (cmov_bb0)
  {
    for (OP *next_cop, *cop = BB_first_op(cmov_bb0); cop; cop = next_cop)
    {
      next_cop = OP_next(cop);
      if (OP_xfer(cop))
	break;
      
      BB_Remove_Op(OP_bb(cop), cop);
      BB_Insert_Op_After(OP_bb(insert), insert, cop);
      insert = cop;
    }
  }
  
  OP *new_last_op = op;
  /* Need to insert sub or mov. Make sure they are inserted
     after OP (conditional branch) so EBO will be able to 
     process the new OPs.
   */
  if (need_sub) {
    TN *tmp = Build_TN_Of_Mtype(MTYPE_I4);
    OP *sub_op;
    if (sub_top == TOP_sub) {
      sub_op = Mk_OP(TOP_sub, tmp, OP_opnd(op,0), OP_opnd(op, 1));
    } else {
      /* sub_op == addi/addmi */
      sub_op = Mk_OP(sub_top, tmp, OP_opnd(op, 0),
	             Gen_Literal_TN(-TN_value(opnd_tn[1]), TN_size(opnd_tn[1])));
    }
    OP_srcpos(sub_op) = OP_srcpos(op);

    if (EBO_in_loop) {
      EBO_Set_OP_omega(sub_op, NULL, opnd_tninfo[0], opnd_tninfo[1]);
    }

    BB_Insert_Op_After(OP_bb(op), op, sub_op);
    new_last_op = sub_op;
    x = tmp;
  } else {
    x = opnd_tn[0];
    // if condition is in a dedicated TN as in the case of a returned reg
    // we need to copy it first in order to avoid an returned reg reference
    // in a BB not immediately following the call
    if (TN_is_register(x) && TN_is_dedicated(x)) {
      TN *tmp = Build_TN_Of_Mtype(MTYPE_U4);
      OP *mov_op = Mk_OP(TOP_mov_n, tmp, x);
      OP_srcpos(mov_op) = OP_srcpos(op);

      if (EBO_in_loop)
      {
        EBO_Set_OP_omega(mov_op, opnd_tninfo[0]);
      }
  
      BB_Insert_Op_After(OP_bb(op), op, mov_op);
      new_last_op = mov_op;
      x = tmp;
    }
  }

  TN *cp_state = cpenable_tn(cmov_top);
  OP *cmov;
  if (cp_state != NULL) {
    cmov = Mk_OP(cmov_top, a, a, b, x, cp_state /* cpenable_tn */);
  } else {
    cmov = Mk_OP(cmov_top, a, a, b, x);
  }

  // TN a cannot be rematerializable (see PR10929)
  Is_True ( !TN_is_rematerializable(a), 
            ("cmov_sequence, the result TN of cmov cannot be rematerializable"));

  OP_srcpos(cmov) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(cmov, NULL, NULL, opnd_tninfo[0]);
  BB_Insert_Op_After(OP_bb(op1), insert, cmov);

  if (!TN_is_dedicated(x)) {
    GTN_UNIVERSE_Add_TN(x);
    GRA_LIVE_Add_Live_Out_GTN(OP_bb(op),x);
    GRA_LIVE_Add_Defreach_Out_GTN(OP_bb(op),x);
    GRA_LIVE_Add_Live_In_GTN(OP_bb(op1),x);
  }

  /* Change the branch to one that unconditionally jumps to
     'cmov_bb1'. The old branch will be removed, and the new
     unconditional one will be optimized by a subsequent pass of ebo
     and cflow. */

  if (new_last_op != op) {
    /* New OPs are inserted after 'op'. Because 'op' is
       a branch OP, so we need to duplicate 'op' and insert
       the duplicated 'op' to the last.
     */
    OP *new_xfer = Dup_OP(op);
    BB_Insert_Op_After(OP_bb(op), new_last_op, new_xfer);
    generate_known_branch(new_xfer, target_tn, force_taken);
    OP_Change_To_Noop(new_xfer);
  } else {
    generate_known_branch(op, target_tn, force_taken);
  }

  if (EBO_Trace_Optimization) {
    fprintf(TFile,
      "<EBO:special_sequence> replace with cmov for branch in BB:%d\n",
      BB_id(OP_bb(op)));
  }

  OP_Change_To_Noop(op);
  return TRUE;
}
 
/* Try to combine shift a, b, i ; extui c, a, shift, mask into
   extui c, b, shift+i, mask. */
static BOOL
shift_extui_sequence (OP *op,
		      TN **opnd_tn,
		      EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);

  /* The first operand to 'op' must be defined in a shift. */

  TN *extui0_tn = opnd_tn[0];
  TN *extui1_tn = opnd_tn[1];
  TN *extui2_tn = opnd_tn[2];
  if (!TN_is_register(extui0_tn) || !TN_has_value(extui1_tn) ||
      !TN_has_value(extui2_tn))
    return FALSE;
  
  INT32 shift;
  EBO_TN_INFO *extui0_tninfo = opnd_tninfo[0];
  EBO_OP_INFO *psh = locate_opinfo_entry(extui0_tninfo);
  EBO_OP_INFO *sh = imm_sra(psh, &shift);
  if (!sh)
    sh = imm_srl(psh, &shift);

  /* If we found a right shift, we can use it if the mask does not
     contain zero or sign-extended bits from the shift. */

  if (sh)
  {
    if ((32 - shift - TN_value(extui1_tn)) < TN_value(extui2_tn))
      return FALSE;
  }
  else
  {
    /* We can combine a left shift with the extui's right shift if
       none of the masked bits are shifted to position beyond bits 0
       or 31. */
    sh = imm_sll(psh, &shift);
    if (!sh || (shift > TN_value(extui1_tn)) ||
	(shift > (32 - TN_value(extui2_tn))))
      return FALSE;
    
    shift = -shift;
  }

  if (!sh)
    return FALSE;

  /* The sll tn must be available at the branch. */

  OP *shift_op = sh->in_op;
  TN *shift_tn = OP_opnd(shift_op, 0);
  EBO_TN_INFO *shift_tninfo = sh->actual_opnd[0];
  
  if (!EBO_tn_available(OP_bb(op), shift_tninfo))
    return FALSE;

  /* Find the new shift amount for the extui. */

  INT32 shift_val = TN_value(extui1_tn) + shift;
  if (shift_val < 0)
    return FALSE;
  
  OP *new_op = Mk_OP(top, OP_result(op, 0), shift_tn, Gen_Literal_TN(shift_val, 4), extui2_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, shift_tninfo, NULL, NULL);
  BB_Insert_Op_After(OP_bb(op), op, new_op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Change shift for extui\n");

  return TRUE;
}
 
/* Try to combine extui a, b, shift, mask ; extui c, a, shift, mask
   into a single extui. */
static BOOL
extui_extui_sequence (OP *op,
		      TN **opnd_tn,
		      EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);

  /* The first operand to 'op' must be defined in an extui. */

  TN *extui0_tn = opnd_tn[0];
  TN *extui0_shift = opnd_tn[1];
  TN *extui0_mask = opnd_tn[2];
  if (!TN_is_register(extui0_tn) || !TN_has_value(extui0_shift) ||
      !TN_has_value(extui0_mask))
    return FALSE;
  
  INT32 shift, mask;
  EBO_TN_INFO *extui0_tninfo = opnd_tninfo[0];
  EBO_OP_INFO *extui1_opinfo = decode_extui(locate_opinfo_entry(extui0_tninfo), &shift, &mask);
  if (!extui1_opinfo)
    return FALSE;

  /* Determine the bits that make it through both extui's. */

  INT32 low_bit = shift + TN_value(extui0_shift);
  INT32 high_bit = MIN(mask + shift - 1, TN_value(extui0_mask) + low_bit - 1);

  /* If 'low_bit' > 'high_bit', then the result of the extui's is 0,
     so we just load that. */

  if (high_bit < low_bit)
    return Convert_Imm_Move(op, OP_result(op, 0), 0);
  
  /* If the range 'low_bit' - 'high_bit' can be encoded in an extui,
     then replace 'op' with a new extui using extui_opinfo's operand
     tn as input. */

  if ((high_bit - low_bit) >= 16)
    return FALSE;

  /* 'extui1's operand tn must be available. */

  OP *extui1_op = extui1_opinfo->in_op;
  TN *extui1_tn = OP_opnd(extui1_op, 0);
  EBO_TN_INFO *extui1_tninfo = extui1_opinfo->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), extui1_tninfo))
    return FALSE;

  /* Make the new extui... */
  INT32 new_shift = low_bit;
  INT32 new_mask = high_bit - low_bit + 1;
  
  OP *new_op = Mk_OP(TOP_extui, OP_result(op, 0), extui1_tn,
		     Gen_Literal_TN(new_shift, 4), Gen_Literal_TN(new_mask, 4));
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, extui1_tninfo, NULL, NULL);
  BB_Insert_Op_After(OP_bb(op), op, new_op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace extui pair with single extui\n");

  return TRUE;
}
 
/* Try to combine extui a, b, shift, mask; sext c, a, imm into extui
   c, b, shift, mask. i.e. eliminate the redundant sext. */
static BOOL
extui_sext_sequence (OP *op,
		     TN **opnd_tn,
		     EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);

  /* The first operand to 'op' must be defined in an extui. */

  TN *sext0_tn = opnd_tn[0];
  TN *sext1_tn = opnd_tn[1];
  if (!TN_is_register(sext0_tn) || !TN_has_value(sext1_tn))
    return FALSE;
  
  INT32 shift, mask;
  EBO_TN_INFO *sext0_tninfo = opnd_tninfo[0];
  EBO_OP_INFO *extui_opinfo = decode_extui(locate_opinfo_entry(sext0_tninfo), &shift, &mask);
  if (!extui_opinfo)
    return FALSE;

  /* 'extui's operand tn must be available. */

  OP *extui_op = extui_opinfo->in_op;
  TN *extui_tn = OP_opnd(extui_op, 0);
  EBO_TN_INFO *extui_tninfo = extui_opinfo->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), extui_tninfo))
    return FALSE;

  /* If the highest bit coming out of the extract is less than the
     sext bit, we know the sign-extended bit is 0, and thus the sext
     is a nop. */

  if (TN_value(sext1_tn) <= (mask - 1))
    return FALSE;

  /* Make a new extui... */
  
  OP *new_op = Mk_OP(TOP_extui, OP_result(op, 0), extui_tn,
		     Gen_Literal_TN(shift, 4), Gen_Literal_TN(mask, 4));

  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, extui_tninfo, NULL, NULL);

  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace extui/sext with extui (i.e. eliminate sext)\n");

  return TRUE;
}

/* To transform 
    OP2:  clamps/sext  a, b, c
    OP1:  clamps/sext  e, a, d
   into
    OP2:  clamps/sext  a, b, c  
          mov          e, a    
   if d >= c.  Also, if e == a, no mov will be generated.
 */
static BOOL
clamps_sext_sequence (OP *op,
		      TN **opnd_tn,
		      EBO_TN_INFO **opnd_tninfo)
{
  OP *op1 = op;
  TOP top = OP_code(op1);
  Is_True((top == TOP_sext || top == TOP_clamps),
          ("clamps_sext_sequence: OP must be either clamps or sext!"));

  TN *op1_tn0 = opnd_tn[0];
  TN *op1_tn1 = opnd_tn[1];
  TN *op1_rtn = OP_result(op1, 0);
  if (!TN_is_register(op1_tn0) || !TN_has_value(op1_tn1) || 
      !TN_is_register(op1_rtn))
    return FALSE;
  
  // first, make sure op1_tn is defined by clamps/sext
  EBO_TN_INFO *op1_tninfo0 = opnd_tninfo[0];
  EBO_OP_INFO *op2_opinfo = locate_opinfo_entry(op1_tninfo0);
  if ( !op2_opinfo ) {
    return FALSE;
  }

  OP *op2 = op2_opinfo->in_op;
  if (!op2 || ((OP_code(op2) != TOP_clamps) && (OP_code(op2) != TOP_sext))) {
    return FALSE;
  }

  TN *op2_tn1 = OP_opnd(op2, 1);
  // It must be constant !
  if (!TN_has_value(op2_tn1))
    return FALSE;

  // Now, check if op1 (either sext or clamps) redundant
  if (TN_value(op1_tn1) < TN_value(op2_tn1))
    return FALSE;

  // Now, op1 is redundant. We replace it with a copy OP if op1's result
  // is different from op2's; if they are same, don't make any new OP.
  if (!(   ((TN_register(op1_rtn) != REGISTER_UNDEFINED) &&
            (TN_register(op1_tn0) != REGISTER_UNDEFINED) &&
            (TN_register(op1_rtn) == TN_register(op1_tn0))) 
        || (op1_rtn == op1_tn0)) ) {
    OP *new_op = Mk_OP(TOP_mov_n, op1_rtn, op1_tn0);
    Set_OP_copy(new_op);
    OP_srcpos(new_op) = OP_srcpos(op1);
    if (EBO_in_loop) {
      EBO_Set_OP_omega(new_op, op1_tninfo0);
    }
    BB_Insert_Op_After(OP_bb(op1), op1, new_op);
  }

  if (EBO_Trace_Optimization) {
    fprintf(TFile, "Replace %s/%s with %s (i.e. eliminate %s)\n",
                   TI_TOP_Name(OP_code(op2)), TI_TOP_Name(OP_code(op1)),
                   TI_TOP_Name(OP_code(op2)), TI_TOP_Name(OP_code(op1)));
  }

  return TRUE;
}

/*  Try to combine 
       slli t, src, imm
       srai r, t, imm
    into
       sext r, src, 31-imm
 */
static BOOL
srai_slli_sequence(OP *op,
                   TN **opnd_tn,
                   EBO_TN_INFO **opnd_tninfo)
{
  if (!xt_sext) return FALSE;

  TN *srai_tn0 = opnd_tn[0];
  TN *srai_tn1 = opnd_tn[1];
  TN *srai_rtn = OP_result(op, 0);

  // safety check
  if (!TN_is_register(srai_tn0) || !TN_has_value(srai_tn1) ||
      !TN_is_register(srai_rtn)) {
    return FALSE;
  }

  // imm is a legal immediate for sext
  INT64 imm_val = 31 - TN_value(srai_tn1);
  ISA_LITCLASS sext_lc;
  if ( ! ((TI_TOP_Immediate_Operand(TOP_sext, &sext_lc) == 1) &&
          TI_ISA_LC_Value_In_Class(imm_val, sext_lc))) {
    return FALSE;
  }

  // Make sure srai_tn1 is defined by slli
  EBO_TN_INFO *sari0_tninfo = opnd_tninfo[0];
  EBO_OP_INFO *slli_opinfo = locate_opinfo_entry(sari0_tninfo);
  if (slli_opinfo == NULL || OP_code(slli_opinfo->in_op) != TOP_slli) {
    return FALSE;
  }

  OP *slli_op = slli_opinfo->in_op;
  TN *slli_tn0 = OP_opnd(slli_op, 0);
  TN *slli_tn1 = OP_opnd(slli_op, 1);

  // safety check
  if (!TN_is_register(slli_tn0) || !TN_has_value(slli_tn1)) {
    return FALSE;
  }

  // Make sure slli_tn0 is available at OP, and slli_tn1 is equal to srai_tn1
  if ( TN_value(srai_tn1) != TN_value(slli_tn1) ||
       !EBO_tn_available(OP_bb(op), slli_opinfo->actual_opnd[0])) {
    return FALSE;
  }

  // Make a new sext 
  OP *new_op = Mk_OP(TOP_sext, srai_rtn, 
                               slli_tn0, 
                               Gen_Literal_TN(imm_val, TN_size(srai_tn1)));
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, slli_opinfo->actual_opnd[0], NULL);

  BB_Insert_Op_After(OP_bb(op), op, new_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace slli/srai with sext (i.e. eliminate slli/srai)\n");

  return TRUE;
}
 
/* Try to combine extui a, b, shift, mask; sext c, a, imm into extui
   c, b, shift, mask. i.e. eliminate the redundant sext. This is the
   same as "extui_sext_sequence" except we look for the sext
   implemented as a pair of shifts, slli/srai. */
static BOOL
extui_shift_extend_sequence (OP *op,
			     TN **opnd_tn,
			     EBO_TN_INFO **opnd_tninfo)
{
  /* Make sure 'op' is an slli/srai pair acting as a sign-extend. */
  INT32 sext_imm;
  EBO_OP_INFO *slli_opinfo = decode_shift_sext(op, opnd_tninfo[0], &sext_imm);
  if (!slli_opinfo)
    return FALSE;

  OP *slli = slli_opinfo->in_op;
  
  /* The first operand to 'op' must be defined in an extui. */

  TN *sext0_tn = OP_opnd(slli, 0);
  TN *sext1_tn = OP_opnd(slli, 1);
  if (!TN_is_register(sext0_tn) || !TN_has_value(sext1_tn))
    return FALSE;
  
  INT32 shift, mask;
  EBO_TN_INFO *sext0_tninfo = slli_opinfo->actual_opnd[0];
  EBO_OP_INFO *extui_opinfo = decode_extui(locate_opinfo_entry(sext0_tninfo), &shift, &mask);
  if (!extui_opinfo)
    return FALSE;

  /* 'extui's operand tn must be available. */

  OP *extui_op = extui_opinfo->in_op;
  TN *extui_tn = OP_opnd(extui_op, 0);
  EBO_TN_INFO *extui_tninfo = extui_opinfo->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), extui_tninfo))
    return FALSE;

  /* If the highest bit coming out of the extract is less than the
     sext bit, we know the sign-extended bit is 0, and thus the sext
     is a nop. */

  if (sext_imm <= (mask - 1))
    return FALSE;
  
  /* Make a new extui... */
  
  OP *new_op = Mk_OP(TOP_extui, OP_result(op, 0), extui_tn,
		     Gen_Literal_TN(shift, 4), Gen_Literal_TN(mask, 4));

  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, extui_tninfo, NULL, NULL);

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace extui/sext with extui (i.e. eliminate sext)\n");

  return TRUE;
}
 
/* Try to combine one of:

   1. extui a, b, 0, mask
   2. sext a, b, imm
   3. and a, b, c

   with mul16s or mul16u. Because the mul16* instructions only read
   the lower 16 bits of the operand registers, the preceeding extui,
   sext, or and may be unnecessary. */
static BOOL
narrow_mul16_sequence (OP *op,
		       TN **opnd_tn,
		       EBO_TN_INFO **opnd_tninfo,
		       EBO_TN_INFO **actual_tninfo,
		       UINT opnd_idx)
{
  Is_True((opnd_idx == 0) || (opnd_idx == 1), (""));
  
  TOP top = OP_code(op);

  /* 'opnd_idx' operand to 'op' must be defined in an extui, sext, or
     and. The other operand we just use the existing TN. */

  TN *mul0_tn = opnd_tn[opnd_idx];
  TN *mul1_tn = OP_opnd(op, ((opnd_idx == 0) ? 1 : 0));
  EBO_TN_INFO *mul0_tninfo = opnd_tninfo[opnd_idx];
  EBO_TN_INFO *mul1_tninfo = actual_tninfo[(opnd_idx == 0) ? 1 : 0];

  if (!TN_is_register(mul0_tn))
    return FALSE;

  TN *input_tn = NULL;
  EBO_TN_INFO *input_tninfo = NULL;
  EBO_OP_INFO *mul0_opinfo = locate_opinfo_entry(mul0_tninfo);

  /* extui... if it doesn't shift, or mask off any of the low 16-bits,
     then we don't need it for the mul. */
  INT32 shift, mask;
  EBO_OP_INFO *extui_opinfo = decode_extui(mul0_opinfo, &shift, &mask);
  if (extui_opinfo)
  {
    if ((shift != 0) || (mask < 16))
      return FALSE;

    OP *extui_op = extui_opinfo->in_op;
    input_tn = OP_opnd(extui_op, 0);
    input_tninfo = extui_opinfo->actual_opnd[0];
  }
  else
  {
    /* sext... if it doesn't sign-extend from any of the low 16-bits,
       then we don't need it for the mul. */
    INT32 sext_imm;
    EBO_OP_INFO *sext_opinfo = decode_sext(mul0_opinfo, &sext_imm);
    if (sext_opinfo)
    {
      if (sext_imm < 15)
	return FALSE;

      OP *sext_op = sext_opinfo->in_op;
      input_tn = OP_opnd(sext_op, 0);
      input_tninfo = sext_opinfo->actual_opnd[0];
    }
    else
    {
      /* and... if it passes all of the low 16-bits, then we don't
         need it for the mul. */
      TN *and_tn0, *and_tn1;
      EBO_OP_INFO *and_opinfo = decode_and(mul0_opinfo, &and_tn0, &and_tn1);
      if (and_opinfo)
      {
	OP *and_op = and_opinfo->in_op;
	INT32 idx = -1;
	
	if (TN_has_value(and_tn0) &&
	    ((TN_value(and_tn0) & 0x0ffff) == 0x0ffff))
	  idx = 1;
	else if (TN_has_value(and_tn1) &&
		 ((TN_value(and_tn1) & 0x0ffff) == 0x0ffff))
	  idx = 0;

	if (idx != -1)
	{
	  input_tn = OP_opnd(and_op, idx);
	  input_tninfo = and_opinfo->actual_opnd[idx];
	}
      }
    }
  }

  /* did we find one? */
  
  if (!input_tn || !input_tninfo)
    return FALSE;
    
  /* 'input_tn' must be available. */

  if (!EBO_tn_available(OP_bb(op), input_tninfo))
    return FALSE;

  /* Make a new mul16 that uses 'input_tn' in place of 'mul0_tn'. */
  
  OP *new_op = Mk_OP(top, OP_result(op, 0), input_tn, mul1_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, input_tninfo, mul1_tninfo);

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace narrowing/mul16 with mul16 (i.e. bypass narrowing instruction)\n");

  return TRUE;
}
 
/* Try to combine 'op' with a previous add and replace it with the
   updating version of 'op', 'updating_top'. */
static BOOL
updating_sequence (OP *op,
		   TN **opnd_tn,
		   EBO_TN_INFO **opnd_tninfo,
		   TOP updating_top)
{
  INT storeval_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_storeval);
  INT base_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_base);
  INT offset_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_offset);
  if ((base_idx == -1) || (offset_idx == -1))
    return FALSE;

  /* The offset value in 'op' must be zero, since we need that field
     to record the update amount. */
  
  TN *base_tn = opnd_tn[base_idx];
  TN *offset_tn = opnd_tn[offset_idx];
  if (!base_tn || !offset_tn ||
      !TN_is_register(base_tn) ||
      !TN_has_value(offset_tn) ||
      (TN_value(offset_tn) != 0))
    return FALSE;
  
  /* The first operand to 'op' must be defined by an add. */

  INT32 val;
  TN *index_tn;
  EBO_TN_INFO *base_tninfo = opnd_tninfo[base_idx];
  EBO_OP_INFO *add = index_add(locate_opinfo_entry(base_tninfo), base_tn,
			       &index_tn);
  if (!add)
    return FALSE;

  /* 'val' must fit in the updating topcode, and the src and
     destination registers of the add must be the same. */
  
  OP *add_op = add->in_op;

  if (TN_has_value(index_tn) &&
      !TI_TOP_Can_Have_Immediate(TN_value(index_tn), updating_top))
    return FALSE;

  if (TN_is_register(index_tn)) {
    TOP indexed_top = TI_TOP_Nonindex_To_Index(updating_top);
    if (indexed_top == TOP_UNDEFINED)
      return FALSE;
  }

  /* The result of the add cannot be used in any instructions between
     the currently location of the add and 'op', since we are going to
     remove the add. */
  OP *scan = OP_next(add_op);
  while (scan != op)
  {
    if (!scan || OP_Refs_TN(scan, OP_result(add_op, 0)))
      return FALSE;
    if (TN_is_register(index_tn) && OP_Defs_TN(scan, index_tn))
      return FALSE;

    scan = OP_next(scan);
  }
  
  /* We can convert to 'updating_top'. Change 'add's immediate to '0',
     so that it becomes a copy, and replace 'op' with an updating
     instruction. We assume the updating instruction has the same
     format as op, except that the base operand is also defined. */

  /* for store with unknow storeval operand, we use 0 for storeval_idx
     because
	1. some stores may not have valid storeval_idx values since
	   the values are not stored directly (e.g, with truncation)
	   but they still has updating version.
	2. the compiler learns updating opcodes using loadiu and loadxu
	   protos which require the storeval operand to be the first
	   operand
     this is a compormise so we can still generate updating store for
     stores with general store values
  */

  if (storeval_idx<0)
    storeval_idx = 0;
    
  OP *new_op = 
    CGTARG_Generate_Updating_Op(op, 0 /* for storeval_idx */, base_idx,
				offset_idx, index_tn);
  if (!new_op)
    return FALSE;
  
  int index_pos_in_add = 0;
  if (OP_opnd(add_op, 0) == index_tn)
    index_pos_in_add = 0;
  else
    index_pos_in_add = 1;

  if (EBO_in_loop)
  {
    if (OP_store(op)) {
      CG_LOOP_Init_Op(new_op);
      Set_OP_omega(new_op, storeval_idx, opnd_tninfo[storeval_idx]->omega);
      Set_OP_omega(new_op, base_idx, base_tninfo->omega);
    } else if (OP_load(op)) {
      CG_LOOP_Init_Op(new_op);
      Set_OP_omega(new_op, base_idx, base_tninfo->omega);
    }
    else
      FmtAssert(FALSE, (""));

    if (TN_is_register(index_tn))
      Set_OP_omega(new_op, offset_idx, OP_omega(add_op, index_pos_in_add));
  }

  Set_OP_opnd(add_op, index_pos_in_add, Gen_Literal_TN(0, 4));

  // move immedate to operand position 1 if it is not at the right position
  if (index_pos_in_add != 1) {
    TN* tmp_tn = OP_opnd(add_op,0);
    Set_OP_opnd(add_op, 0, OP_opnd(add_op, 1));
    Set_OP_opnd(add_op, 1, tmp_tn);
  }

  Set_OP_code(add_op, TOP_addi);
  if (EBO_in_loop)
    Set_OP_omega(add_op, 1, 0);	// reset omega for immediate operand of addi

  BB_Insert_Op_After(OP_bb(op), op, new_op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Change nonupdating to updating\n");

  return TRUE;
}

/* Look at previous instructions to find load constant that can be used to
   replace 'op' with an addi/addmi. If 'EBO_in_peep' is TRUE, then we
   also try to find l32r's that are loading symbol offset that are
   known to be constant; and so the l32r can be replace by a load of a
   constant. */
static BOOL
share_load_const (OP *op,
	    TN **opnd_tn,
	    EBO_TN_INFO **actual_tninfo)
{
  TOP opcode = OP_code(op);
  Is_True(((opcode == TOP_load_const) || (opcode == TOP_const16lo)), ("expecting load constant"));
  
  TN *lit_tn;
  if (opcode == TOP_load_const)
    lit_tn = OP_opnd(op, 0);
  else if (opcode == TOP_const16lo)
    lit_tn = OP_opnd(op, 1);
  else {
    Is_True(FALSE, ("expecting load constant"));
  }
   
  if (!TN_is_symbol(lit_tn))
    return FALSE;

  ST * sym = TN_var(lit_tn);
  INT64 offset = TN_offset(lit_tn);

  ST * base_sym;
  INT64 base_offset;

  Base_Symbol_And_Offset_For_Addressing(sym, offset, 
					&base_sym, &base_offset);

  /* If the base symbol is SP_Sym or FP_Sym, then don't try to share
     the load_const, since the relative location of symbols on the stack may
     change. But if we know that offsets are fixed, we can convert the
     const into a constant load. */
  if ((base_sym == SP_Sym) || (base_sym == FP_Sym))
  {
    if (!((ST_sclass(sym) == SCLASS_AUTO) ||
	  (EBO_in_peep && (ST_sclass(sym) == SCLASS_FORMAL))))
      return FALSE;

    TOP top;
    if (TI_ISA_LC_Value_In_Class(base_offset, LC_simm12))
      top = TOP_movi;
    else
      top = TOP_load_const;

    if (EBO_Trace_Optimization)
      fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));
    
    OP *new_op = Mk_OP(top, OP_result(op, 0), Gen_Literal_TN(base_offset, 4));
    OP_srcpos(new_op) = OP_srcpos(op);
    if (EBO_in_loop)
      EBO_Set_OP_omega(new_op, NULL);
    
    BB_Insert_Op_After(OP_bb(op), op, new_op);
    return TRUE;
  }
  /* Look through previous instructions for an l32r of 'base_sym'
     with an offset that is close enough to 'base_offset' that we can
     replace 'op' with an add using the result of the previous
     l32r. */
    
  for (EBO_TN_INFO *prev_tninfo = EBO_last_tninfo;
       prev_tninfo != NULL;
       prev_tninfo = prev_tninfo->prior)
  {
    TN *prev_tn = prev_tninfo->local_tn;
    OP *prev_op = prev_tninfo->in_op;
    
    if (!prev_tn || !prev_op 
	|| ((OP_code(prev_op) != TOP_load_const)
	    && (OP_code(prev_op) != TOP_const16lo)))
      continue;
    
    /* The TN defined by 'prev_op' must be available at 'op'. */
    
    if (EBO_tn_available(OP_bb(op), prev_tninfo))
    {
      Is_True(OP_result(prev_op, 0) == prev_tn, ("unexpected prev constant tn"));
      
      TN *prev_lit_tn;
      if (OP_code(prev_op) == TOP_load_const)
	prev_lit_tn = OP_opnd(prev_op, 0);
      else if (OP_code(prev_op) == TOP_const16lo)
	prev_lit_tn = OP_opnd(prev_op, 1);
      else {
	FmtAssert(FALSE, ("incorrect opcode"));
      }
      
      if (!TN_is_symbol(prev_lit_tn))
	continue;
      
      /* We can't use the previous literal if it can only be used to
	 point to the thing it explicitly references (e.g. jump tables
	 have this property, because jump tables can be eliminated by
	 cflow.  leaving us with a literal pointing to a symbol that
	 doesn't exist, see pr2916). */
      if (ST_explicit_literal_ref(TN_var(prev_lit_tn)))
	continue;
      
      ST *prev_base_sym;
      INT64 prev_base_offset;
      
      Base_Symbol_And_Offset_For_Addressing(TN_var(prev_lit_tn),
					    TN_offset(prev_lit_tn),
					    &prev_base_sym,
					    &prev_base_offset);

      if (prev_base_sym == base_sym)
      {
	/* Make sure the difference between 'base_offset' and
	   'prev_base_offset' can be encoded in the immediate field of
	   an addi or addmi. */
	
	ISA_LITCLASS add_lc;
	TOP top;
	INT64 immed = base_offset - prev_base_offset;
	
	if ((TI_TOP_Immediate_Operand(TOP_addi, &add_lc) == 1) &&
	    TI_ISA_LC_Value_In_Class(immed, add_lc))
	  top = TOP_addi;
	else if ((TI_TOP_Immediate_Operand(TOP_addmi, &add_lc) == 1) &&
		 TI_ISA_LC_Value_In_Class(immed, add_lc))
	  top = TOP_addmi;
	else
	  continue;
	
	/* Everything's ok, replace 'op' with 'top'. */
	
	if (EBO_Trace_Optimization)
	  fprintf(TFile,"\treplace %s with %s\n", TI_TOP_Name(OP_code(op)), TI_TOP_Name(top));
	
	OP *new_op = Mk_OP(top, OP_result(op, 0), prev_tn, Gen_Literal_TN(immed, 4));
	
	OP_srcpos(new_op) = OP_srcpos(op);
	if (EBO_in_loop)
	  EBO_Set_OP_omega(new_op, prev_tninfo, NULL);
	
	BB_Insert_Op_After(OP_bb(op), op, new_op);
	return TRUE;
      }
    }
  }
  return FALSE;
}


// Look for extw following memw or extw and replaced the latter with NOP
static BOOL
extw_sequence(OP* op) {
  if (op && OP_code(op)==TOP_extw) {
    OP* prev_op = OP_prev(op);
    if (OP_prev(op) &&
	(OP_code(prev_op)==TOP_extw ||
	 OP_code(prev_op)==TOP_extw_pseudo ||
	 OP_code(prev_op)==TOP_memw ||
	 OP_code(prev_op)==TOP_memw_pseudo)) {
      OP_Change_To_Noop(prev_op);
      return true;
    }
  } else if (op && OP_code(op)==TOP_extw_pseudo) {
    OP* prev_op = OP_prev(op);
    if (OP_prev(op) &&
	(OP_code(prev_op)==TOP_extw_pseudo)) {
      OP_Change_To_Noop(prev_op);
      return true;
    } else if (OP_prev(op) &&
	(OP_code(prev_op)==TOP_extw)) {
      OP_Change_Opcode(op, TOP_extw);
      OP_Change_To_Noop(prev_op);
      return true;
    }
  }
  return false;
}

// Look for memw following memw and replaced the latter with NOP
// or memw following extw and replaced the memw with extw and replace
// the latter with NOP
static BOOL
memw_sequence(OP* op) {
  if (op && OP_code(op)==TOP_memw) {
    OP* prev_op; 
    // change redundant memw to nop
    for ( prev_op = OP_prev(op); 
          prev_op != NULL;
          prev_op = OP_prev(prev_op))
    {
      if ( (OP_code(prev_op)==TOP_memw)	||
	   (OP_code(prev_op)==TOP_memw_pseudo) ) {
        OP_Change_To_Noop(prev_op);
        return true;  
      } else if (OP_code(prev_op)==TOP_extw) {
        OP_Change_To_Noop(op);
        return true; 
      } else if (OP_memory(prev_op) ||
		 OP_side_effects(prev_op) ||
		 OP_extern_effects(prev_op)) {
        break;
      }
    }
  } else if (op && OP_code(op)==TOP_memw_pseudo) {
    OP* prev_op;

    // change redundant memw_pseudo to nop 
    for ( prev_op = OP_prev(op); 
          prev_op != NULL;
          prev_op = OP_prev(prev_op))
    {
      if (OP_code(prev_op)==TOP_memw_pseudo) {
        OP_Change_To_Noop(prev_op);
        return true;
      } else if ( (OP_code(prev_op)==TOP_memw) ||
	          (OP_code(prev_op)==TOP_extw_pseudo) ||
	          (OP_code(prev_op)==TOP_extw) ) {
        OP_Change_To_Noop(op);
        return true;
      } else if (OP_memory(prev_op) ||
		 OP_side_effects(prev_op) ||
		 OP_extern_effects(prev_op)) {
        break;
      }
    }
  }
  return false;
}

// Look for wfr fr, ar1 followed by rfr ar2, fr
// and replaced it by mov ar2, ar1 (copy)
static BOOL
wfr_rfr_sequence(OP *op,
                 TN **opnd_tn,
                 EBO_TN_INFO **opnd_tninfo)
{
  EBO_OP_INFO *wfr_op_info = locate_opinfo_entry(opnd_tninfo[0]);
  if (!wfr_op_info)
    return FALSE;
  
  OP *wfr_op = wfr_op_info->in_op;
  if (OP_code(wfr_op) != TI_TOP_Topcode("wfr")) 
    return FALSE;
  
  EBO_TN_INFO *wfr_opnd_tninfo = wfr_op_info->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), wfr_opnd_tninfo))
    return FALSE;
  
  OP *rfr_op = op;
  OP *new_op = Mk_OP(TOP_mov_n, OP_result(rfr_op, 0), OP_opnd(wfr_op, 0));
  Set_OP_copy(new_op);
  OP_srcpos(new_op) = OP_srcpos(rfr_op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, wfr_opnd_tninfo);
  BB_Insert_Op_After(OP_bb(rfr_op), rfr_op, new_op);
  
  OP_Change_To_Noop(rfr_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace wfr/rfr with mov.n\n");

  return TRUE;
}


// Look for rsr ar1, n followed by wsr n, ar1
// and delete the wsr
static BOOL
rsr_wsr_sequence(OP *op,
                 TN **opnd_tn,
                 EBO_TN_INFO **opnd_tninfo)
{
  EBO_OP_INFO *rsr_op_info = locate_opinfo_entry(opnd_tninfo[0]);
  if (!rsr_op_info)
    return FALSE;
  
  OP *rsr_op = rsr_op_info->in_op;
  if (OP_code(rsr_op) == TOP_rsr && OP_code(op) == TOP_wsr) {
  } else if (OP_code(rsr_op) == TI_TOP_Topcode("rsr.acclo") &&
	     OP_code(op) == TI_TOP_Topcode("wsr.acclo")) {
  } else
    return FALSE;
  
  EBO_TN_INFO *rsr_opnd_tninfo = rsr_op_info->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), rsr_opnd_tninfo))
    return FALSE;
  
  if (OP_opnd(rsr_op, 0) != OP_result(op, 0))
    return FALSE;
  
  OP_Change_To_Noop(op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Delete wsr\n");

  return TRUE;
}


static BOOL is_live_tn(OP *current_op, TN *current_tn)
{
  OP *op;
  Is_True(GRA_LIVE_Phase_Invoked, ("Bad call to is_live_tn"));
  BOOL is_live = tn_has_live_def_into_BB(current_tn, OP_bb(current_op));
  BOOL past_current_op = FALSE;

  FOR_ALL_BB_OPs(OP_bb(current_op), op) {
    INT num_opnds = OP_opnds(op);
    INT num_results = OP_results(op);
    if (op == current_op) {
      past_current_op = TRUE;
      if (!is_live)
	return FALSE;
    }
    if (past_current_op) {
      for (int opndnum = 0; opndnum < num_opnds; opndnum++) {
	if (tn_registers_identical(current_tn, OP_opnd(op,opndnum)))
	  return TRUE;
      }
    }
    for (int resnum = 0; resnum < num_results; resnum++) {
      if (tn_registers_identical(current_tn, OP_result(op,resnum))) {
	if (past_current_op)
	  return FALSE;
	else
	  is_live = TRUE;
      }
    }
  }
  Is_True(past_current_op && is_live, ("Bad call to is_live_tn"));
  return GTN_SET_MemberP(BB_live_out(OP_bb(current_op)), current_tn);
}

// Look for srli ar1, ar2, n 
//          slli ar3, ar4, 32-n
//          or   ar5, ar1, ar3
// and replace it by 
//          ssai n
//          src ar5, ar3, ar1
static BOOL
src_sequence(OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo)
{
  TN *src0_tn = opnd_tn[0];
  TN *src1_tn = opnd_tn[1];
  if (!TN_is_register(src0_tn) || !TN_is_register(src1_tn))
    return FALSE;

  INT32 shift_right=0, shift_left=0;
  EBO_OP_INFO *srl_op_info, *sll_op_info;
  EBO_OP_INFO *src0_op_info = locate_opinfo_entry(opnd_tninfo[0]);
  EBO_OP_INFO *src1_op_info = locate_opinfo_entry(opnd_tninfo[1]);
  sll_op_info = imm_sll(src0_op_info, &shift_left);
  srl_op_info = imm_srl(src1_op_info, &shift_right);
  if ((sll_op_info == NULL) || (srl_op_info == NULL)) {
    sll_op_info = imm_sll(src1_op_info, &shift_left);
    srl_op_info = imm_srl(src0_op_info, &shift_right);
  }
  if ((sll_op_info == NULL) || (srl_op_info == NULL) ||
      (shift_left+shift_right != 32))
    return FALSE;

  EBO_TN_INFO *srl_opnd_tninfo = srl_op_info->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), srl_opnd_tninfo))
    return FALSE;
  EBO_TN_INFO *sll_opnd_tninfo = sll_op_info->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), sll_opnd_tninfo))
    return FALSE;
  
  // We can't check the liveness of sar until we've done at least one pass of
  // GRA_LIVE_Init().
  if (!GRA_LIVE_Phase_Invoked)
    return FALSE;

  TN *sar_reg = Build_Dedicated_TN(REGISTER_CLASS_sar, REGISTER_sar, 4);
  if (is_live_tn(op, sar_reg))
    return FALSE;

  OP *old_op = op;
  OP *ssai_op = Mk_OP(TOP_ssai, sar_reg, Gen_Literal_TN(shift_right, 4));
  OP *src_op = Mk_OP(TOP_src, OP_result(old_op,0), sll_opnd_tninfo->local_tn,
		     srl_opnd_tninfo->local_tn, sar_reg);
  OP_srcpos(ssai_op) = OP_srcpos(old_op);
  OP_srcpos(src_op) = OP_srcpos(old_op);
  if (EBO_in_loop) {
    CG_LOOP_Init_Op(ssai_op);
    EBO_Set_OP_omega(src_op, srl_opnd_tninfo, sll_opnd_tninfo, NULL);
  }
  BB_Insert_Op_After(OP_bb(old_op), old_op, ssai_op);
  BB_Insert_Op_After(OP_bb(old_op), ssai_op, src_op);
  OP_Change_To_Noop(old_op);

  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replace srli/slli/or with ssai/src\n");

  return TRUE;
}


/*********************************************************************
 *
 * INJUI pattern recognition
 *
 *********************************************************************/


/* Check if there's 'likely' only one use of 'tn' after 'op'. */
static BOOL
TN_is_probable_single_use (TN *tn, OP *op)
{
  if (!op)
    return FALSE;
  
  if (!TN_is_register(tn))
    return FALSE;
  
  if (TN_register(tn) != REGISTER_UNDEFINED)
    return FALSE;
  
  if (TN_is_global_reg(tn))
    return FALSE;

  INT cnt = 0;
  for (op = OP_next(op); op; op = OP_next(op)) {
    if (OP_Refs_TN(op, tn)) {
      cnt++;
      if (cnt > 1)
        return FALSE;
    }
    
    if (OP_Defs_TN(op, tn))
      break;
  }
  
  return (cnt == 1);
}


/* Check if 'val' can be encoded in the operand 'idx' of 'topcode'. */
static BOOL
value_fits_immediate (TOP topcode, INT idx, INT val)
{      
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);
  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, idx);
  ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
  if (lit_class == LC_UNDEFINED ||
      !TI_ISA_LC_Value_In_Class(val, lit_class))
    return FALSE;

  return TRUE;
}


/* If mask is of the form ((1 << bits) - 1), return bits. Return -1 otherwise. */
static INT
injui_mask_bits (INT mask)
{
  for (INT i = 1; i < 32; i++) {
    if (((1 << i) - 1) == mask)
      return i;
  }
  
  return -1;
}


/* Check if 'op_info' is of the form (a & mask). Handle 'and' and 'extui'. */
static BOOL
is_injui_masked_val (EBO_OP_INFO *op_info, INT &mask,
                     EBO_TN_INFO **val_tn_info)
{
  INT shift = 0;
  INT bits = 0;
  if (decode_extui(op_info, &shift, &bits)) {
    mask = ((1 << bits) - 1) << shift;
    
    if (val_tn_info)
      *val_tn_info = op_info->actual_opnd[0];
    
    return TRUE;
  }

  TN *left = NULL;
  TN *right = NULL;
  if (!decode_and(op_info, &left, &right))
    return FALSE;
      
  INT64 val = 0;
  if (TN_Value_At_Op(left, op_info->in_op, &val)) {
    mask = (INT)val;
    if (val_tn_info)
      *val_tn_info = op_info->actual_opnd[1];

    return TRUE;
  }

  if (TN_Value_At_Op(right, op_info->in_op, &val)) {
    mask = (INT)val;
    if (val_tn_info)
      *val_tn_info = op_info->actual_opnd[0];

    return TRUE;
  }
  
  return FALSE;
}


/* Look for:
   1. ((val & mask) << shift)
   2. (val & mask) --> (val & mask) << 0
   3. (val << shift) --> (val & mask(32 - shift)) << shift */
static BOOL
is_injui_mask_shift (EBO_OP_INFO *op_info, INT &shift, INT &bits,
                     EBO_TN_INFO **val_tn_info)
{
  if (!op_info)
    return FALSE;

  EBO_OP_INFO *cop_info = op_info;
  if (imm_sll(cop_info, &shift)) {
    cop_info = locate_opinfo_entry(cop_info->actual_opnd[0]);
  } else {
    shift = 0;
  }
  
  INT mask = 0;
  if (is_injui_masked_val(cop_info, mask, val_tn_info)) {
    bits = injui_mask_bits(mask);
    return (bits > 0);
  }
  
  if (shift <= 0)
    return FALSE;
  
  bits = 32 - shift;
  if (val_tn_info)
    *val_tn_info = op_info->actual_opnd[0];
  
  return TRUE;
}


/* Check if 'op_info' is a bit-inject sequence and return a bit-mask of
   bits that have been initialized or are known to be zero.

   FIXME: This function should really be replaced by something equivalent
   to 'value_bit_mask' in opt_insert.cxx
*/
static BOOL
is_injui_subsequence (EBO_OP_INFO *op_info, INT &inj_bits)
{
  while (op_info && OP_copy(op_info->in_op)) {
    INT32 copy_opnd = TI_ISA_Copy_Operand(OP_code(op_info->in_op), TRUE);
    op_info = locate_opinfo_entry(op_info->actual_opnd[copy_opnd]);
  }

  if (!op_info)
    return FALSE;
  
  INT shift = 0;
  INT bits = 0;
  if (is_injui_mask_shift(op_info, shift, bits, NULL)) {
    inj_bits = ((1 << bits) - 1) << shift;
    return TRUE;
  }

  if (!decode_injui(op_info, &shift, &bits))
    return FALSE;
  
  EBO_OP_INFO *src_op_info = locate_opinfo_entry(op_info->actual_opnd[0]);
  INT local_inj_bits = 0;
  if (!is_injui_subsequence(src_op_info, local_inj_bits))
    return FALSE;

  INT mask = (1 << bits) - 1;
  if ((local_inj_bits & (mask << shift)) != 0)
    return FALSE;

  inj_bits = local_inj_bits | (mask << shift);

  return TRUE;
}


/* Look for and replace the following patterns:
   1. ((a1 & mask1) << shift1) | ((a2 & mask2) << shift2) | ...
   2. (a & ~(mask << shift)) | ((b & mask ) << shift)

   Check if the bits from or-operand (1 - targ_idx) are inserted in
   or-operand targ_idx. */
static BOOL
injui_sequence(OP *op, TN **opnd_tn, EBO_TN_INFO **opnd_tninfo, INT targ_idx)
{
  if (!xt_injui || (top_injui() == TOP_UNDEFINED))
    return FALSE;
  
  TN *targ_tn = opnd_tn[targ_idx];
  TN *val_tn = opnd_tn[1 - targ_idx];
  if (!TN_is_register(targ_tn) || !TN_is_register(val_tn))
    return FALSE;

  EBO_OP_INFO *targ_op_info = locate_opinfo_entry(opnd_tninfo[targ_idx]);
  EBO_OP_INFO *val_op_info = locate_opinfo_entry(opnd_tninfo[1 - targ_idx]);
  
  if (!targ_op_info || !val_op_info)
    return FALSE;
  
  INT shift = 0;
  INT bits = 0;
  EBO_TN_INFO *val_tn_info = NULL;
  if (!is_injui_mask_shift(val_op_info, shift, bits, &val_tn_info))
    return FALSE;

  if (!EBO_tn_available(OP_bb(op), val_tn_info))
    return FALSE;

  INT val_mask = (((1 << bits) - 1) << shift);

  bool direct = false;
  EBO_TN_INFO *targ_tn_info = NULL;
  INT targ_mask = 0;
  if (is_injui_masked_val(targ_op_info, targ_mask, &targ_tn_info) &&
      (targ_mask == ~val_mask)) {
    /* Pattern 2 recognized. */
    direct = true;
  } else {
    INT inj_bits = 0;
    if (!is_injui_subsequence(targ_op_info, inj_bits))
      return FALSE;
    
    if ((inj_bits & val_mask) != 0)
      return FALSE;
    
    /* Pattern 1 recognized. */
    targ_tn_info = opnd_tninfo[targ_idx];
  }
  
  if (!EBO_tn_available(OP_bb(op), targ_tn_info))
    return FALSE;

  /* Since injui has an inout operand, this may require an extra copy
     instruction. Therefore, make sure that we can eliminate at least
     2 instructions before replacing the 'or' with an 'injui'. */
  if (!(TN_is_probable_single_use(val_tn, val_op_info->in_op) ||
        (direct && TN_is_probable_single_use(targ_tn, targ_op_info->in_op))))
    return FALSE;
  
  if (!value_fits_immediate(top_injui(), 2, shift))
    return FALSE;

  if (!value_fits_immediate(top_injui(), 3, bits))
    return FALSE;
  
  OPS ops = OPS_EMPTY;
  EBO_Exp_COPY(NULL, OP_result(op, 0), targ_tn_info->local_tn, &ops);
  OP *injui_op = Mk_OP(top_injui(), OP_result(op, 0),
                       OP_result(op, 0), val_tn_info->local_tn,
                       Gen_Literal_TN(shift, 4),
                       Gen_Literal_TN(bits, 4));
  OPS_Append_Op(&ops, injui_op);
  
  if (EBO_in_loop)
    EBO_OPS_omega(&ops);
  
  OP *tmp_op;
  FOR_ALL_OPS_OPs(&ops, tmp_op) {
    OP_srcpos(tmp_op) = OP_srcpos(op);
  }
  
  BB_Insert_Ops_After(OP_bb(op), op, &ops);
  OP_Change_To_Noop(op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Replaced injui sequence\n");
  
  return TRUE;
}


/* Recognize sext->injui sequences that the result of sext result
   is not used. Remove the sext. */
static BOOL
sext_injui_sequence (OP *op,
                     TN **opnd_tn,
                     EBO_TN_INFO **opnd_tninfo)
{
  TOP top = OP_code(op);
  
  /* The second operand to 'op' must be defined in a sext. */
  TN *injui0_tn = opnd_tn[0];
  TN *injui1_tn = opnd_tn[1];
  TN *injui_shift_tn = opnd_tn[2];
  TN *injui_mask_tn = opnd_tn[3];
  if (!TN_is_register(injui1_tn) || !TN_has_value(injui_shift_tn) ||
      !TN_has_value(injui_mask_tn))
    return FALSE;
  
  INT32 shift;
  EBO_TN_INFO *injui1_tninfo = opnd_tninfo[1];
  EBO_OP_INFO *psext = locate_opinfo_entry(injui1_tninfo);

  INT32 sext_imm;
  EBO_OP_INFO *sext_opinfo = decode_sext(psext, &sext_imm);
  if (!sext_opinfo)
    return FALSE;

  /* Check that the sign-extend bits are not used. */
  if (TN_value(injui_mask_tn) > (sext_imm + 1))
    return FALSE;
  
  /* The sext tn must be available at the injui. */
  EBO_TN_INFO *sext_tninfo = sext_opinfo->actual_opnd[0];
  if (!EBO_tn_available(OP_bb(op), sext_tninfo))
    return FALSE;

  OP *sext_op = sext_opinfo->in_op;
  TN *sext_tn = OP_opnd(sext_op, 0);
  
  OP *new_op = Mk_OP(top, OP_result(op, 0),
                     injui0_tn, sext_tn, injui_shift_tn, injui_mask_tn);
  OP_srcpos(new_op) = OP_srcpos(op);
  if (EBO_in_loop)
    EBO_Set_OP_omega(new_op, sext_tninfo, NULL, NULL);
  BB_Insert_Op_After(OP_bb(op), op, new_op);
  OP_Change_To_Noop(op);
  
  if (EBO_Trace_Optimization)
    fprintf(TFile, "Use sext's input instead of result in sext->injui\n");

  return TRUE;
}


/*********************************************************************
 *
 *********************************************************************/


/*
 * Look at an expression and it's inputs to identify special sequences
 * that can be simplified.  */
BOOL
Special_Sequence (OP *op,
		  TN **opnd_tn,
		  EBO_TN_INFO **opnd_tninfo,
		  EBO_TN_INFO **actual_tninfo)
{
  TOP top = OP_code(op);
  
#ifdef Is_True_On
  if (EBO_Trace_Optimization)
  {
    fprintf(TFile, "OP: ");
    Print_OP_No_SrcLine(op);
    for (UINT i = 0; i < OP_opnds(op); i++)
    {
      fprintf(TFile, "  local tn: ");
      Print_TN(OP_opnd(op, i), TRUE);
      fprintf(TFile, "\n  ebo tn: ");
      Print_TN(opnd_tn[i], TRUE);
      fprintf(TFile, "\n   local in_op: ");
      if (actual_tninfo[i] && actual_tninfo[i]->in_op)
	Print_OP_No_SrcLine(actual_tninfo[i]->in_op);
      else
	fprintf(TFile, "<null>\n");
      fprintf(TFile, "   ebo in_op: ");
      if (opnd_tninfo[i] && opnd_tninfo[i]->in_op)
	Print_OP_No_SrcLine(opnd_tninfo[i]->in_op);
      else
	fprintf(TFile, "<null>\n");

      if (opnd_tninfo[i] && opnd_tninfo[i]->replacement_tn)
      {
	fprintf(TFile, "  replace tn: ");
	Print_TN(opnd_tninfo[i]->replacement_tn, TRUE);
	fprintf(TFile, "\n   replace in_op: ");
	if (opnd_tninfo[i]->replacement_tninfo &&
	    opnd_tninfo[i]->replacement_tninfo->in_op)
	  Print_OP_No_SrcLine(opnd_tninfo[i]->replacement_tninfo->in_op);
	else
	  fprintf(TFile, "<null>\n");
      }
    }
  }
#endif
  
  if (OP_iadd(op) && sll_add_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if (OP_isub(op) && sll_sub_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if ((top == TOP_extui) &&
      (shift_extui_sequence(op, opnd_tn, opnd_tninfo) ||
       extui_extui_sequence(op, opnd_tn, opnd_tninfo)))
    return TRUE;

  if ((top == TOP_sext) && extui_sext_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if ((top == TOP_sext || top == TOP_clamps) && 
      clamps_sext_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if ((top == TOP_srai) && extui_shift_extend_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if ((top == TOP_srai) && xt_sext && srai_slli_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if (((top == TOP_mul16s) || (top == TOP_mul16u)) &&
      (narrow_mul16_sequence(op, opnd_tn, opnd_tninfo, actual_tninfo, 0) ||
       narrow_mul16_sequence(op, opnd_tn, opnd_tninfo, actual_tninfo, 1)))
    return TRUE;

  if (((top == TOP_bbci) || (top == TOP_bbsi)) &&
      shift_bbi_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if (OP_cond(op) &&
      (extui_branch_sequence(op, opnd_tn, opnd_tninfo) ||
       sll_branch_sequence(op, opnd_tn, opnd_tninfo) ||
       and_branch_sequence(op, opnd_tn, opnd_tninfo) ||
       sext_branch_sequence(op, opnd_tn, opnd_tninfo) ||
       beq_branch_sequence(op, opnd_tn, opnd_tninfo, actual_tninfo) ||
       cmov_sequence(op, opnd_tn, opnd_tninfo)))
    return TRUE;

  if (((top == TOP_load_const) || (top == TOP_const16lo)) &&
      share_load_const(op, opnd_tn, opnd_tninfo))
    return TRUE;
  
  if ((top == TOP_or) &&
      src_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;
  
  if (xt_injui)
  {
    if ((top == TOP_or) &&
        (injui_sequence(op, opnd_tn, opnd_tninfo, 0) ||
         injui_sequence(op, opnd_tn, opnd_tninfo, 1)))
      return TRUE;

    if ((top == top_injui()) &&
        (sext_injui_sequence(op, opnd_tn, opnd_tninfo)))
      return TRUE;
  }
  
  TOP updating_top = TI_TOP_Nonupdate_To_Update(OP_code(op));
  if ((updating_top != TOP_UNDEFINED) &&
      updating_sequence(op, opnd_tn, opnd_tninfo, updating_top))
    return TRUE;

  if (xt_hard_float && top == TI_TOP_Topcode("rfr"))
    return wfr_rfr_sequence(op, opnd_tn, opnd_tninfo);

  if ((top == TOP_wsr || top == TI_TOP_Topcode("wsr.acclo")) && 
      rsr_wsr_sequence(op, opnd_tn, opnd_tninfo))
    return TRUE;

  if ((top == TOP_extw || top == TOP_extw_pseudo) && extw_sequence(op))
    return TRUE;

  if ((top == TOP_memw || top == TOP_memw_pseudo) && memw_sequence(op))
    return TRUE;

  return FALSE;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
