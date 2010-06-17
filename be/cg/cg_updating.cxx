
/*

  Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.

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
 *  Module: cg_updating
 *
 *  Description:
 *  ============
 *
 *  This module attempts to combine immediate adds with load and store
 *  instructions to form updating load and store instructions.
 *
 *  It also attempts to reorder adds with load and store instructions
 *  so that a base-pointer adjustment outside the loop can be
 *  eliminated. For example:
 *
 *        addi   TN100, TN101, -256
 * loop:
 *        addi   TN100, TN100, 4
 *        l32i   TN200, TN100, 252
 *        ...
 *
 * becomes:
 *
 *        addi TN100, TN101, -256   // can eliminate if TN100 now unused
 * loop:  
 *        l32i   TN200, TN101, 0
 *        addi   TN101, TN101, 4
 *        ...
 *
 * =======================================================================
 * ======================================================================= */

#include "bb.h"
#include "bb_set.h"
#include "findloops.h"
#include "libti.h"
#include "cgtarget.h"
#include "op.h"
#include "tn.h"
#include "tracing.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cxx_memory.h"
#include "cgexp.h"
#include "gra_live.h"

/* If 'op' is an OP incrementing a TN, then return the TN; otherwise
   return NULL. */
static TN *
incremented_tn (OP *op)
{
  TOP top = OP_code(op);
    
  if ((top != TOP_addi) && (top != TOP_addmi))
    return NULL;

  if (OP_result(op, 0) != OP_opnd(op, 0))
    return NULL;

  return OP_opnd(op, 0);
}


/* Check some common requirements to combine or swap a load/store and
   increment. Return TRUE if the requirements are met, and the base
   index and offset index of 'ldst'. */
static BOOL
check_ldst_inc (OP *ldst, OP *inc, INT *base_index =NULL, INT *offset_index =NULL)
{
  Is_True((OP_load(ldst) || OP_store(ldst)) && incremented_tn(inc), (""));
  
  /* Check that the result of the 'inc' is not used in 'ldst', except
     perhaps as the base of 'ldst'.  Also check that 'inc' is not a
     result in 'ldst'. */
    
  TN *inc_tn = OP_opnd(inc, 0);
    
  INT base_idx = TI_TOP_Find_Operand_Use(OP_code(ldst), OU_base);
  INT offset_idx = TI_TOP_Find_Operand_Use(OP_code(ldst), OU_offset);
  if ((base_idx == -1) || (offset_idx == -1))
    return FALSE;

  for (UINT opnum = 0; opnum < OP_opnds(ldst); opnum++)
    if ((opnum != base_idx) && (OP_opnd(ldst, opnum) == inc_tn))
      return FALSE;

  for (UINT resnum = 0; resnum < OP_results(ldst); resnum++)
    if (OP_result(ldst, resnum) == inc_tn)
      return FALSE;

  if (base_index)
    *base_index = base_idx;
  if (offset_index)
    *offset_index = offset_idx;
    
  return TRUE;
}


/* Switch 'op' and 'inc' so that 'inc' is ahead of 'op'. Adjust 'op'
   appropriately. Return FALSE if we can't switch them. 'diff'
   specifies an additional constant to add to the offset when checking
   for a valid offset, but 'diff' is not actually included in the new
   offset used for the instruction (presumedly the caller will fix up
   when 'diff' != 0). */
static BOOL
move_inc_before_op (OP *op, OP *inc, INT64 diff =0)
{
  /* We only know how to switch a load/store past an 'inc'. */

  if (!OP_load(op) && !OP_store(op))
    return FALSE;

  /* Try to move 'inc' ahead of load/store 'op'. */
  
  OP *ldst = op;

  INT base_idx, offset_idx;
  if (!check_ldst_inc(ldst, inc, &base_idx, &offset_idx))
    return FALSE;

  /* If 'inc's TN is the base of 'ldst', then we must adjust 'ldst's
     offset. */
    
  if (OP_result(inc, 0) != OP_opnd(inc, 0)) 
    return FALSE;

  TN *inc_tn = OP_opnd(inc, 0);
  if (OP_opnd(ldst, base_idx) == inc_tn)
  {
    /* See if 'ldst's offset minus 'inc's constant can be encoded in
       'ldst's offset field. */

    INT inc_imm_idx = TI_TOP_Immediate_Operand(OP_code(inc), NULL);
    if (inc_imm_idx == -1)
      return FALSE;

    TN *inc_imm_tn = OP_opnd(inc, inc_imm_idx);
    TN *offset_tn = OP_opnd(ldst, offset_idx);
    if (!(TN_has_value(inc_imm_tn) && TN_has_value(offset_tn)))
      return FALSE;

    INT64 new_offset = TN_value(offset_tn) - TN_value(inc_imm_tn) + diff;

    if (!TI_TOP_Can_Have_Immediate(new_offset, OP_code(ldst)))
      return FALSE;
      
    /* Change 'ldst's offset value... */
    TN *new_offset_tn = Gen_Literal_TN(new_offset - diff, TN_size(offset_tn));
    Set_OP_opnd(ldst, offset_idx, new_offset_tn);
  }

  /* Move 'inc' from it's current location to just before 'ldst'. */
  if (OP_result(inc, 0) == OP_opnd(inc, 0)) {
    BB_Remove_Op(OP_bb(inc), inc);
    BB_Insert_Op_Before(OP_bb(ldst), ldst, inc);
  } else {
    OPS move_ops = OPS_EMPTY;
    Exp_COPY(OP_result(inc, 0), inc_tn, &move_ops);
    BB_Insert_Ops_Before(OP_bb(inc), inc, &move_ops);
    BB_Remove_Op(OP_bb(inc), inc);

    Set_OP_result(inc, 0, inc_tn);
    BB_Insert_Op_Before(OP_bb(ldst), ldst, inc);

  }

  return TRUE;
}


/* Switch 'op' and 'inc' so that 'inc' is after 'op'. Adjust 'op'
   appropriately. Return FALSE if we can't switch them. 'diff'
   specifies an additional constant to add to the offset when checking
   for a valid offset, but 'diff' is not actually included in the new
   offset used for the instruction (presumedly the caller will fix up
   when 'diff' != 0). */
static BOOL
move_inc_after_op (OP *op, OP *inc, INT64 diff =0)
{
  /* We only know how to switch a load/store past an 'inc'. */

  if (!OP_load(op) && !OP_store(op))
    return FALSE;

  /* Try to move 'inc' after load/store 'op'. */
  
  OP *ldst = op;

  INT base_idx, offset_idx;
  if (!check_ldst_inc(ldst, inc, &base_idx, &offset_idx))
    return FALSE;

  /* If 'inc's TN is the base of 'ldst', then we must adjust 'ldst's
     offset. */
    
  TN *inc_tn = OP_result(inc, 0);
  if (OP_opnd(ldst, base_idx) == inc_tn)
  {
    /* See if 'ldst's offset plus 'inc's constant can be encoded in
       'ldst's offset field. */

    INT inc_imm_idx = TI_TOP_Immediate_Operand(OP_code(inc), NULL);
    if (inc_imm_idx == -1)
      return FALSE;

    TN *inc_imm_tn = OP_opnd(inc, inc_imm_idx);
    TN *offset_tn = OP_opnd(ldst, offset_idx);
    if (!(TN_has_value(inc_imm_tn) && TN_has_value(offset_tn)))
      return FALSE;

    INT64 new_offset = TN_value(offset_tn) + TN_value(inc_imm_tn) + diff;

    if (!TI_TOP_Can_Have_Immediate(new_offset, OP_code(ldst)))
      return FALSE;
      
    /* Change 'ldst's offset value... */
    TN *new_offset_tn = Gen_Literal_TN(new_offset - diff, TN_size(offset_tn));
    Set_OP_opnd(ldst, offset_idx, new_offset_tn);
  }

  /* Move 'inc' from it's current location to just after 'ldst'. */
  BB_Remove_Op(OP_bb(inc), inc);
  BB_Insert_Op_After(OP_bb(ldst), ldst, inc);

  return TRUE;
}


// TN_OPS_MAP

class TN_OPS_MAP
{
private:
  /* Map from TNs to OPs (implemented as an array). */
  OP **_ops;
  UINT _ops_size;

public:
  TN_OPS_MAP (MEM_POOL *pool)
  {
    _ops_size = Last_TN + 1;
    _ops = TYPE_MEM_POOL_ALLOC_N(OP*, pool, _ops_size);
    Clear();
  }

  /* Clear the map from TNs to OPS. */
  void Clear (void)
  {
    memset(_ops, 0, sizeof(OP *) * _ops_size);
  }
  
  /* Show that 'tn' maps to 'op'. */
  void Set_Tn_Op (TN *tn, OP *op)
  {
    Is_True(TN_is_register(tn), ("TN_number only valid for register TNs"));
    Is_True(TN_number(tn) < _ops_size, ("unexpected tn number %d\n",
					TN_number(tn)));
    _ops[TN_number(tn)] = op;
  }

  /* Get the 'op' that 'tn' maps to. */
  OP *Get_Tn_Op (TN *tn)
  {
    if (!TN_is_register(tn))
      return NULL;
    
    Is_True(TN_number(tn) < _ops_size, ("unexpected tn number %d\n",
					TN_number(tn)));
    return _ops[TN_number(tn)];
  }

};


// UPDATING_REGION

class UPDATING_REGION
{
private:
  MEM_POOL *_pool;
  const BOOL _trace;
  
  /* BB's in the region over which we are looking for updating
     opportunities. */
  BB_SET *const _bbs;

  /* Map from TNs to OPs. */
  TN_OPS_MAP _tn_ops_map;


  /* Try to combine 'ldst' with 'inc' to form an updating version of
     'ldst'. If we do combine, return TRUE, and remove 'ldst' and
     'inc'. The new combined instruction is inserted before 'op'. */
  BOOL combine_ldst_inc (OP *ldst, OP *inc)
  {
    INT base_idx, offset_idx;
    if (!check_ldst_inc(ldst, inc, &base_idx, &offset_idx))
      return FALSE;

    /* We can only combine if 'inc's TN is the base of 'ldst'. */
    
    TN *inc_tn = OP_opnd(inc, 0);
    if (OP_opnd(ldst, base_idx) != inc_tn)
      return FALSE;
    
    /* 'ldst's offset must equal 'inc's constant. */

    INT inc_imm_idx = TI_TOP_Immediate_Operand(OP_code(inc), NULL);
    if (inc_imm_idx == -1)
      return FALSE;

    TN *inc_imm_tn = OP_opnd(inc, inc_imm_idx);
    TN *offset_tn = OP_opnd(ldst, offset_idx);
    if (!(TN_has_value(inc_imm_tn) && TN_has_value(offset_tn)))
      return FALSE;
    INT offset_val = TN_value(offset_tn);
    INT inc_val = TN_value(inc_imm_tn);
    if (TN_value(offset_tn) != TN_value(inc_imm_tn)) {
      return FALSE;
    }

    /* Create the updating version of 'ldst'. */
    INT storeval_idx = TI_TOP_Find_Operand_Use(OP_code(ldst), OU_storeval);
#ifdef TARG_XTENSA
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
#else    
    if (OP_store(ldst) && (storeval_idx == -1))
      return FALSE;
#endif
    
    OP *new_ldst = CGTARG_Generate_Updating_Op(ldst, storeval_idx, base_idx,
					       offset_idx, offset_tn);
    if (!new_ldst)
      return FALSE;
    
    BB_Insert_Op_Before(OP_bb(ldst), ldst, new_ldst);

    /* Remove 'ldst' and 'inc'. */
    if (OP_result(inc, 0) != OP_opnd(inc, 0)) {
      OPS move_ops = OPS_EMPTY;
      Exp_COPY(OP_result(inc, 0), inc_tn, &move_ops);
      BB_Insert_Ops_Before(OP_bb(inc), inc, &move_ops);
    }
    BB_Remove_Op(OP_bb(ldst), ldst);
    BB_Remove_Op(OP_bb(inc), inc);
    _tn_ops_map.Set_Tn_Op(inc_tn, NULL);

    return TRUE;
  }

  
  /* If 'op' is a load or store that can potentially be converted to
     an updating version, then return TRUE. */
  BOOL updateable_ldst (OP *op)
  {
    if (!OP_load(op) && !OP_store(op))
      return FALSE;
    
    return (TI_TOP_Nonupdate_To_Update(OP_code(op)) != TOP_UNDEFINED);
  }


  /* Attempt to combine increments in 'bb' with loads and stores to
     form updating versions of those loads and stores. */
  void optimize_bb (BB *bb)
  {

    RID* rid;

    /* guard against optimizing for SWP'ed inner loop */ 
    if (( rid = BB_rid(bb) ) && ( RID_level(rid) >= RL_CGSCHED ) )
        return;

    if (_trace)
    {
      fprintf(TFile, SBar);
      fprintf(TFile, "<cgup> optimizing BB\n");
      Print_BB(bb);
    }

    /* EBO is responsible for combining an increment with a following
       load/store. So we only worry about the case where the increment
       follows the load/store. Examine the OPs in reverse order,
       carrying TN's that have been incremented, and looking for
       previous loads/stores that can be combined with the increment
       to form an updating load/store. */

    _tn_ops_map.Clear();
    
    for (OP *prev, *op = BB_last_op(bb); op != NULL; op = prev)
    {
      prev = OP_prev(op);
      
      if (_trace)
      {
	fprintf(TFile, "<cgup> visiting ");
	Print_OP_No_SrcLine(op);
      }
      
      /* Record increment... */
      TN *inc_tn = incremented_tn(op);
      if (inc_tn)
      {
	if (_trace)
	  fprintf(TFile, "<cgup>    found increment\n");

	_tn_ops_map.Set_Tn_Op(inc_tn, op);
	continue;
      }

      /* Try to combine increments with load/store. If we do combine,
         'op' and 'inc' are removed, and the new instruction is
         inserted before 'op'. */
      if (updateable_ldst(op))
      {
	BOOL changed = false;
	for (UINT opnum = 0; opnum < OP_opnds(op); opnum++)
	{
	  TN *tn = OP_opnd(op, opnum);
	  OP *inc = _tn_ops_map.Get_Tn_Op(tn);
	  if (inc && combine_ldst_inc(op, inc))
	  {
	    if (_trace)
	      fprintf(TFile, "<cgup>    combined\n");
	    
	    changed = true;
	    break;
	  }
	}
	if (changed)
	  continue;
      }

      /* For each TN defined in 'op' show that increments of that TN
         cannot move above 'op'. */
      for (UINT resnum = 0; resnum < OP_results(op); resnum++)
      {
	TN *res = OP_result(op, resnum);
	if (TN_is_register(res)) 
	  _tn_ops_map.Set_Tn_Op(res, NULL);
      }

      /* If a TN with an increment is used in 'op', try to move the
         increment past 'op'. If we can't, then show that the
         increment can't be combined with any previous load/store. */
      for (UINT opnum = 0; opnum < OP_opnds(op); opnum++)
      {
	TN *tn = OP_opnd(op, opnum);
	OP *inc = _tn_ops_map.Get_Tn_Op(tn);
	if (inc)
	{
	  if (!move_inc_before_op(op, inc))
	  {
	    _tn_ops_map.Set_Tn_Op(tn, NULL);
	  }
	  else
	  {
	    if (_trace)
	    {
	      fprintf(TFile, "<cgup>    moving before ");
	      Print_OP_No_SrcLine(inc);
	    }
	  }
	}
      }
    }
  }


public:
  UPDATING_REGION (BB_SET *bbs, MEM_POOL *pool, BOOL trace) :
    _bbs(bbs),
    _pool(pool),
    _trace(trace),
    _tn_ops_map(_pool)
  { }

  
  /* Perform updating optimization over BBs in '_bbs'. */
  void optimize (void)
  {
    BB *bb;
    OP *op;

    if (_trace)
    {
      fprintf(TFile, DBar);
      fprintf(TFile, "<cgup> optimizing BBs ");
      BB_SET_Print(_bbs, TFile);
      fprintf(TFile, "\n");
    }

    /* For now we just optimize within a BB. The offset
       canonicalization is actually performed across
       control-equivalent regions, so at some point we may want to
       update this code to move an increment from one BB to combine
       with a load or store in another, control-equivalent BB. */

    FOR_ALL_BB_SET_members(_bbs, bb)
      optimize_bb(bb);
  }

};


// BP_ELIM_REGION

class BP_ELIM_REGION
{
private:
  class BP_ELIM_OPS
  {
  private:
    struct ELIM_OP_LIST
    {
      OP *const _op;
      const BOOL _is_inc;
      union
      {
	INT64 _offset_adjust;
	struct
	{
	  OP *_insert_before;
	  OP *_insert_after;
	} _s;
      };
      ELIM_OP_LIST *_prev;
      ELIM_OP_LIST *_next;
      
      ELIM_OP_LIST (OP *op, BOOL is_inc, ELIM_OP_LIST *rest) :
	_op(op), _is_inc(is_inc), _prev(NULL), _next(rest)
      {
	_offset_adjust = 0;
	_s._insert_before = NULL;
	_s._insert_after = NULL;
	if (_next)
	  _next->_prev = this;
      }
    };

  private:
    const BOOL _trace;

    /* Does this object contain valid data? */
    BOOL _valid;

    /* List of all OPs that def or use either 'bp' or 'alt_bp'. OPs
       within the list are in the reverse order as OPs in a BB. But
       OPs from different BBs are simply concatenated together. Thus,
       users of this list must be careful to distinquish between OPs
       from different BBs, since there is nothing in the list itself
       to indicate a change of BB. */
    ELIM_OP_LIST *_op_list;

    
    /* Return true if the define of 'tn' in 'op' allows 'tn' to be
       substituted. If you change this be sure to check the logic in
       "Fixup" that assumes where 'tn' can appear in an allowed OP. */
    BOOL allowed_def (OP *op, TN *tn)
    {
      TN* inc_tn = incremented_tn(op);
      return ((inc_tn == tn) && (OP_result(op,0) == OP_opnd(op,0)));
    }

    /* Return true if the use of 'tn' in 'op' allows 'tn' to be
       substituted. If you change this be sure to check the logic in
       "Fixup" that assumes where 'tn' can appear in an allowed OP. */
    BOOL allowed_use (OP *op, TN *tn)
    {
      /* We only allow loads and stores where 'tn' is the base
         register. */
      if (!OP_load(op) && !OP_store(op))
	return FALSE;
      
      INT base_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_base);
      if ((base_idx == -1) || (OP_opnd(op, base_idx) != tn))
	return FALSE;

      for (UINT opnum = 0; opnum < OP_opnds(op); opnum++)
	if ((opnum != base_idx) && (OP_opnd(op, opnum) == tn))
	  return FALSE;

      return TRUE;
    }

    /* Tentatively switch 'eop' and 'einc' so that 'einc' is ahead of
       'eop'. Return FALSE if we can't switch them. 'diff' specifies an
       additional constant to add to the offset when checking for a
       valid offset. No OP is actually changed, but information
       necessary to commit the changes is recorded in 'eop' and
       'einc'. */
    static BOOL
    try_inc_before_op (ELIM_OP_LIST *eop, ELIM_OP_LIST *einc, INT64 diff =0)
    {
      /* We only know how to switch a load/store past an increment. */

      OP *ldst = eop->_op;
      OP *inc = einc->_op;
      
      if (!OP_load(ldst) && !OP_store(ldst))
	return FALSE;
      
      /* Try to move 'einc' ahead of load/store 'eop'. */
      
      INT base_idx, offset_idx;
      if (!check_ldst_inc(ldst, inc, &base_idx, &offset_idx))
	return FALSE;
      
      /* If 'inc's TN is the base of 'ldst', then we must adjust
	 'ldst's offset. */
      
      TN *inc_tn = OP_result(inc, 0);
      if (OP_opnd(ldst, base_idx) == inc_tn)
      {
	/* See if 'ldst's offset minus 'inc's constant can be encoded in
	   'ldst's offset field. */
	
	INT inc_imm_idx = TI_TOP_Immediate_Operand(OP_code(inc), NULL);
	if (inc_imm_idx == -1)
	  return FALSE;
	
	TN *inc_imm_tn = OP_opnd(inc, inc_imm_idx);
	TN *offset_tn = OP_opnd(ldst, offset_idx);
	if (!(TN_has_value(inc_imm_tn) && TN_has_value(offset_tn)))
	  return FALSE;
	
	INT64 new_offset = (eop->_offset_adjust + TN_value(offset_tn) -
			    TN_value(inc_imm_tn) + diff);
	
	if (!TI_TOP_Can_Have_Immediate(new_offset, OP_code(ldst)))
	  return FALSE;
	
	/* Record the adjustment to 'ldst's ('eop's) offset value
           necessary if we move 'einc'. */
	eop->_offset_adjust -= TN_value(inc_imm_tn);
      }

      /* Record 'einc' new location... */
      einc->_s._insert_before = ldst;
      einc->_s._insert_after = NULL;

      return TRUE;
    }

    /* Tentatively switch 'eop' and 'einc' so that 'einc' is after
       'eop'. Return FALSE if we can't switch them. 'diff' specifies
       an additional constant to add to the offset when checking for a
       valid offset. No OP is actually changed, but information
       necessary to commit the changes is recorded in 'eop' and
       'einc'. */
    static BOOL
    try_inc_after_op (ELIM_OP_LIST *eop, ELIM_OP_LIST *einc, INT64 diff =0)
    {
      /* We only know how to switch a load/store past an increment. */

      OP *ldst = eop->_op;
      OP *inc = einc->_op;
      
      if (!OP_load(ldst) && !OP_store(ldst))
	return FALSE;

      /* Try to move 'inc' after load/store 'op'. */
  
      INT base_idx, offset_idx;
      if (!check_ldst_inc(ldst, inc, &base_idx, &offset_idx))
	return FALSE;
      
      /* If 'inc's TN is the base of 'ldst', then we must adjust
	 'ldst's offset. */
      
      TN *inc_tn = OP_result(inc, 0);
      if (OP_opnd(ldst, base_idx) == inc_tn)
      {
	/* See if 'ldst's offset plus 'inc's constant can be encoded
	   in 'ldst's offset field. */

	INT inc_imm_idx = TI_TOP_Immediate_Operand(OP_code(inc), NULL);
	if (inc_imm_idx == -1)
	  return FALSE;
	
	TN *inc_imm_tn = OP_opnd(inc, inc_imm_idx);
	TN *offset_tn = OP_opnd(ldst, offset_idx);
	if (!(TN_has_value(inc_imm_tn) && TN_has_value(offset_tn)))
	  return FALSE;
	
	INT64 new_offset = (eop->_offset_adjust + TN_value(offset_tn) +
			    TN_value(inc_imm_tn) + diff);
	
	if (!TI_TOP_Can_Have_Immediate(new_offset, OP_code(ldst)))
	  return FALSE;
	
	/* Record the adjustment to 'ldst's ('eop's) offset value
           necessary if we move 'einc'. */
	eop->_offset_adjust += TN_value(inc_imm_tn);
      }
      
      /* Record 'einc' new location... */
      einc->_s._insert_after = ldst;
      einc->_s._insert_before = NULL;
      
      return TRUE;
    }
    
    
  public:
    BP_ELIM_OPS (TN *bp, TN *alt_bp, BB_SET *_bbs, MEM_POOL *pool, BOOL trace) :
      _trace(trace)
    {
      /* Initially we show 'this' object as invalid, and it is only
         made valid if it passes all of the following tests. */

      _valid = FALSE;
      
      /* Traverse all the OPs in '_bbs' looking for defs and uses of
         'bp' and 'alt_bp'. Collect all OPs that def or use 'bp' into
         '_op_list'. */

      _op_list = NULL;
      
      BB *bb;
      FOR_ALL_BB_SET_members(_bbs, bb)
      {
	for (OP *op = BB_first_op(bb); op != NULL; op = OP_next(op))
	{
	  /* If 'alt_bp' is used or defined anywhere in the region,
             then we can't substitute it for 'bp'. */

	  if (OP_Defs_TN(op, alt_bp) || OP_Refs_TN(op, alt_bp))
	    return;

	  /* If 'bp' is defined in an OP that is not simply
             incrementing 'bp' by a constant, then we can't substitue
             'alt_bp'. Similarly, make sure 'tn' is not used in an
             instruction that would disallow substitution. */

	  if (OP_Defs_TN(op, bp))
	  {
	    if (!allowed_def(op, bp))
	      return;

	    _op_list = CXX_NEW(ELIM_OP_LIST(op, TRUE, _op_list), pool);
	  }
	  else if (OP_Refs_TN(op, bp))
	  {
	    if (!allowed_use(op, bp))
	      return;

	    _op_list = CXX_NEW(ELIM_OP_LIST(op, FALSE, _op_list), pool);
	  }
	}
      }

      /* If '_op_list' is NULL, then there is no opportunity here. */

      _valid = (_op_list != NULL);
    }

    /* Try to adjust OPs in '_op_list' so that all load and store
       offsets can be adjusted by 'diff'. Updates of 'bp' may be moved
       around to allow this, so after this call, the order of
       '_op_list' may no longer reflect the reverse order of the
       corresponding OPS. If we are able to fixup all the offsets so
       they can be decreased by 'diff', return TRUE ("Replace" handles
       the actual decrease). */
    BOOL Fixup (TN *bp, INT64 diff)
    {
      /* For each BB, traverse '_op_list' in order (equivalent to
         looking at the OPs in reverse order), trying to make all
         load/store offsets large enough that they can be adjusted by
         'diff'. */

      BB *bb = NULL;
      ELIM_OP_LIST *inc_op = NULL;
      ELIM_OP_LIST *last = NULL;
      
      for (ELIM_OP_LIST *scan = _op_list; scan != NULL; scan = scan->_next)
      {
	OP *op = scan->_op;
	last = scan;
	
	/* If we are starting a new BB clear existing information. */

	if (OP_bb(op) != bb)
	{
	  inc_op = NULL;
	  bb = OP_bb(op);
	}
      
	if (_trace)
	{
	  fprintf(TFile, "<cgbp> BB:%d fixup reverse ", BB_id(bb));
	  Print_OP_No_SrcLine(op);
	}
      
	/* If 'op' is an increment, record it... */

	if (scan->_is_inc)
	{
	  Is_True(TN_has_value(OP_opnd(op, 1)), ("expecting immediate in operand 1"));
	  
	  if (_trace)
	    fprintf(TFile, "<cgbp>    found increment\n");
	  
	  inc_op = scan;
	  continue;
	}

	/* 'op' is a load or store with 'bp' used as the base
	   register. If there is an increment of bp and it can be
	   moved ahead of 'op' without making 'op's offset + 'diff'
	   invalid, then move the increment. Otherwise show that the
	   increment can't be moved any further. */
	if (inc_op)
	{
	  if (!try_inc_before_op(scan, inc_op, diff))
	  {
	    inc_op = NULL;
	  }
	  else
	  {
	    if (_trace)
	    {
	      fprintf(TFile, "<cgup>    moving before ");
	      Print_OP_No_SrcLine(inc_op->_op);
	    }
	  }
	}
      }

      /* For each BB, traverse '_op_list' in reverse order (equivalent
         to looking at the OPs in forward order), trying to make all
         load/store offsets large enough that they can be adjusted by
         'diff'. */

      bb = NULL;
      inc_op = NULL;
      
      for (ELIM_OP_LIST *scan = last; scan != NULL; scan = scan->_prev)
      {
	OP *op = scan->_op;

	/* If we are starting a new BB clear existing information. */

	if (OP_bb(op) != bb)
	{
	  inc_op = NULL;
	  bb = OP_bb(op);
	}
      
	if (_trace)
	{
	  fprintf(TFile, "<cgbp> BB:%d fixup forward ", BB_id(bb));
	  Print_OP_No_SrcLine(op);
	}
      
	/* If 'op' is an increment, record it... */
	/* If we've already moved this increment up, don't move it back down
	   again, unless you reset the offsets of the instructions that we
	   moved it past. */
	if (scan->_is_inc && (scan->_s._insert_before == NULL))
	{
	  Is_True(TN_has_value(OP_opnd(op, 1)), ("expecting immediate in operand 1"));
	  
	  if (_trace)
	    fprintf(TFile, "<cgbp>    found increment\n");
	  
	  inc_op = scan;
	  continue;
	}

	/* 'op' is a load or store with 'bp' used as the base
	   register. If there is an increment of bp and it can be
	   moved after 'op' without making 'op's offset + 'diff'
	   invalid, then move the increment. Otherwise show that the
	   increment can't be moved any further. */
	if (inc_op)
	{
	  if (!try_inc_after_op(scan, inc_op, diff))
	  {
	    inc_op = NULL;
	  }
	  else
	  {
	    if (_trace)
	    {
	      fprintf(TFile, "<cgup>    moving after ");
	      Print_OP_No_SrcLine(inc_op->_op);
	    }
	  }
	}
      }

      /* We've moved the increments as best we can. Check each
         load/store and if all their offsets + 'diff' are valid, then
         indicate that the fixup was successful. */
      
      for (ELIM_OP_LIST *scan = _op_list; scan != NULL; scan = scan->_next)
      {
	if (!scan->_is_inc)
	{
	  OP *op = scan->_op;

	  INT offset_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_offset);
	  if (offset_idx == -1)
	    return FALSE;

	  TN *offset_tn = OP_opnd(op, offset_idx);
	  if (!TN_has_value(offset_tn))
	    return FALSE;
	  INT64 new_val = scan->_offset_adjust + TN_value(offset_tn) + diff;

	  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(OP_code(op));
	  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, offset_idx);
	  ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
	  if (!TI_ISA_LC_Value_In_Class(new_val, lit_class))
	    return FALSE;
	}
      }

      return TRUE;
    }

    /* Replace 'old' with 'new' in all the OPs in '_op_list'. Decrease
       all load/store offsets by 'diff'. Move increments as
       appropriate. */
    void Replace (TN *old_tn, TN *new_tn, INT64 diff)
    {
      for (ELIM_OP_LIST *scan = _op_list; scan != NULL; scan = scan->_next)
      {
	OP *op = scan->_op;

	for (UINT opnum = 0; opnum < OP_opnds(op); opnum++)
	  if (OP_opnd(op, opnum) == old_tn)
	    Set_OP_opnd(op, opnum, new_tn);

      	for (UINT resnum = 0; resnum < OP_results(op); resnum++)
	  if (OP_result(op, resnum) == old_tn)
	    Set_OP_result(op, resnum, new_tn);

	if (scan->_is_inc)
	{
	  if (scan->_s._insert_before)
	  {
	    BB_Remove_Op(OP_bb(scan->_op), scan->_op);
	    BB_Insert_Op_Before(OP_bb(scan->_s._insert_before),
				scan->_s._insert_before, scan->_op);
	  }
	  else if (scan->_s._insert_after)
	  {
	    BB_Remove_Op(OP_bb(scan->_op), scan->_op);
	    BB_Insert_Op_After(OP_bb(scan->_s._insert_after),
			       scan->_s._insert_after, scan->_op);
	  }
	}
	else
	{
	  Is_True(OP_load(op) || OP_store(op), ("expecting load or store"));

	  INT offset_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_offset);
	  FmtAssert(offset_idx != -1, ("expecting offset operand"));

	  TN *offset_tn = OP_opnd(op, offset_idx);
	  FmtAssert(TN_has_value(offset_tn), (""));
	  INT64 new_val = scan->_offset_adjust + TN_value(offset_tn) + diff;

#ifdef Is_True_On
	  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(OP_code(op));
	  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, offset_idx);
	  ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
	  Is_True(TI_ISA_LC_Value_In_Class(new_val, lit_class),
		  ("offset not in range"));
#endif

	  Set_OP_opnd(op, offset_idx, Gen_Literal_TN(new_val, 4));
	}

	Is_True(!OP_Defs_TN(op, old_tn), ("def not replaced"));
	Is_True(!OP_Refs_TN(op, old_tn), ("use not replaced"));
      }
    }
    
    BOOL Valid (void) const { return _valid; }

    void Print (void)
    {
      fprintf(TFile, "<cgbp> elim list (reverse order of OPS):\n");
      for (ELIM_OP_LIST *scan = _op_list; scan != NULL; scan = scan->_next)
      {
	OP *op = scan->_op;
	fprintf(TFile, "BB %d: ", BB_id(OP_bb(op)));
	Print_OP_No_SrcLine(op);
      }
    }
    
  };
    

private:
  MEM_POOL *_pool;
  const BOOL _trace;

  /* First BB containing base-pointer initializations that we are
     trying to eliminate. The full prolog searched consists of
     '_prolog' and the chain of unique successor BBs. */
  BB *_prolog;
  
  /* BB's in the region over which we are looking for updating
     opportunities. */
  BB_SET *const _bbs;

  /* Set of TNs live on any edge exiting the region formed by
     '_bbs'. */
  BOOL _live_out_inited;
  TN_SET *_live_out;

  
  /* Return TRUE if 'op' in 'bb' potentailly defines a new base
     pointer from another one. Also return the new base pointer in
     'bp', the base pointer it is derived from in 'alt_bp', and the
     constant difference between the two. */
  BOOL eliminable_bp_init (BB *bb, OP *op, TN **bp, TN **alt_bp, TN **diff)
  {
    Is_True(OP_bb(op) == bb, ("expecting OP in BB"));
    
    /* We can't know which TNs are acting as base pointers, other
       parts of the algorithm will determine that by how they are
       used. We just look for immediate adds. */

    TOP top = OP_code(op);
    if ((top != TOP_addi) && (top != TOP_addmi))
      return FALSE;

    *bp = OP_result(op, 0);
    *alt_bp = OP_opnd(op, 0);
    *diff = OP_opnd(op, 1);

    if (!TN_is_register(*bp) || !TN_is_register(*alt_bp) ||
	!TN_has_value(*diff))
      return FALSE;
    
    /* Don't mess with dedicated TNs. */

    if (TN_is_dedicated(*bp) || TN_is_dedicated(*alt_bp))
      return FALSE;
    
    /* 'bp' and 'alt_bp' must be different... */

    if (*bp == *alt_bp)
      return FALSE;
    
    /* Neither 'bp' or 'alt_bp' can be defined in any subsequent
       OPs. We search through 'bb' and the chain of unique
       successors. */

    OP *sop = OP_next(op);
    BB *sbb = bb;
    
    while (TRUE)
    {
      for (; sop != NULL; sop = OP_next(sop))
	if (OP_Defs_TN(sop, *bp) || OP_Defs_TN(sop, *alt_bp))
	  return FALSE;

      BB *succ = BB_Unique_Successor(sbb);
      if (!succ || (sbb != BB_Unique_Predecessor(succ)))
	break;

      sbb = succ;
      sop = BB_first_op(sbb);
    }

    return TRUE;
  }

  /* Initialize a bitset of TN live along any edge exiting the
     region formed by '_bbs'. */
  void init_live_out (void)
  {
    _live_out = TN_SET_Create_Empty(Last_TN + 1, _pool);

    /* Find each successor BB of '_bbs' that is not in '_bbs'. Collect
       the TNs live into those BBs. */

    BB *bb;
    FOR_ALL_BB_SET_members(_bbs, bb)
    {
      BBLIST *si;
      FOR_ALL_BB_SUCCS(bb, si)
      {
	BB *succ = BBLIST_item(si);
	if (!BB_SET_MemberP(_bbs, succ))
	{
	  TN *tn;
	  FOR_ALL_GTN_SET_members(BB_live_in(succ),tn)
	    _live_out = TN_SET_Union1D(_live_out, tn, _pool);
	}
      }
    }

    _live_out_inited = TRUE;
  }
  
  /* Return TRUE if 'tn' is live out along any edge exiting the region
     formed by '_bbs'. */
  BOOL live_out_of_region (TN *tn)
  {
    if (!_live_out_inited)
      init_live_out();
    
    return TN_SET_MemberP(_live_out, tn);
  }

  /* recompute the global TN liveness */
  void recompute_liveness (void)
  {

    if (CG_localize_tns)
      return;

    if (_prolog==NULL)
      return;

    /* Find each successor BB of '_bbs' that is not in '_bbs'. Collect
       the TNs live into those BBs. */

    GRA_LIVE_Region_Start();
    GRA_LIVE_Region_Entry(_prolog);

    BB *bb;
    FOR_ALL_BB_SET_members(_bbs, bb)
    {

      GRA_LIVE_Compute_Local_Info(bb);

      BBLIST *si;
      FOR_ALL_BB_SUCCS(bb, si)
      {
	BB *succ = BBLIST_item(si);
	if (!BB_SET_MemberP(_bbs, succ))
	{
	  GRA_LIVE_Region_Exit(succ);
	}
      }
    }

    GRA_LIVE_Region_Compute_Global_Live_Info();
  }
  
  
  public:
  BP_ELIM_REGION (BB *head, BB_SET *bbs, MEM_POOL *pool, BOOL trace) :
    _bbs(bbs),
    _pool(pool),
    _prolog(NULL),
    _trace(trace),
    _live_out_inited(FALSE),
    _live_out(NULL)
  {
    /* Move the prolog back through the chain of multually unique
       predecessors. */
    
    // find a prolog: a unique pred of the head
    BBLIST *plist;
    BB     *pred;
    for (plist = BB_preds(head); plist; plist = BBLIST_next(plist)) {
      pred = BBLIST_item(plist);
      if (BB_SET_MemberP(_bbs, pred)) {
        continue;
      } else if (_prolog == NULL) {
        _prolog = pred;
        continue;
      }
      // More than 1 preds for the loop, set prolog to NULL
      _prolog = NULL;
      break;
    }

    if (_prolog)
    {
      while (TRUE)
      {
	pred = BB_Unique_Predecessor(_prolog);
	if (!pred || (_prolog != BB_Unique_Successor(pred)))
	  break;
	
	_prolog = pred;
      }
    }
  }

  /* Perform base-pointer optimization over BBs in '_bbs'. */
  void optimize (void)
  {
    if (_trace)
    {
      fprintf(TFile, DBar);
      fprintf(TFile, "<cgbp> optimizing BBs ");
      BB_SET_Print(_bbs, TFile);
      if (_prolog)
	fprintf(TFile, "\n<cgbp> prolog start BB %d\n", BB_id(_prolog));

      init_live_out();
      fprintf(TFile, "<cgbp> region live out: ");
      TN_SET_Print(_live_out, TFile);
      fprintf(TFile, "\n");
    }

    /* We must have some prolog BBs to search for eliminable
       base-pointer inits. */
    
    if (!_prolog)
    {
      if (_trace)
	fprintf(TFile, "<cgbp> no prolog\n");
      
      return;
    }
    
    /* Walk through '_prolog' BBs looking for potentially eliminable
       base-pointer initializations. */

    BB *sbb = _prolog;
    while (TRUE)
    {
      RID* rid;

      /* guard against optimizing for SWP'ed inner loop */ 
      if (!( rid = BB_rid(sbb) ) || ( RID_level(rid) < RL_CGSCHED ) )
      for (OP *op = BB_first_op(sbb); op != NULL; op = OP_next(op))
      {
	if (_trace)
	{
	  fprintf(TFile, SBar);
	  fprintf(TFile, "<cgbp> trying BB %d: ", BB_id(sbb));
	  Print_OP_No_SrcLine(op);
	}
      
	TN *bp, *alt_bp, *diff;
	if (!eliminable_bp_init(sbb, op, &bp, &alt_bp, &diff))
	  continue;
      
	if (_trace)
	{
	  fprintf(TFile, "<cgbp> found potentially eliminable bp init ");
	  Print_TN(bp, TRUE);
	  fprintf(TFile, "\n<cgbp> trying to replace with ");
	  Print_TN(alt_bp, TRUE);
	  fprintf(TFile, " plus %d\n", TN_value(diff));
	}
      
	/* 'bp' nor 'alt_bp' must not be live out of the region formed
	   by '_bbs'. */

	if (live_out_of_region(bp) || live_out_of_region(alt_bp))
	{
	  if (_trace)
	    fprintf(TFile, "<cgbp> cannot eliminate, live out of region.\n");
	
	  continue;
	}

	/* Walk all the OPs in the region, collecting those that
	   def/use 'bp' and 'alt_bp'. If 'bp' is defed/used in a way
	   that precludes it from being eliminated, skip it. */

	BP_ELIM_OPS bp_ops(bp, alt_bp, _bbs, _pool, _trace);
	if (!bp_ops.Valid())
	{
	  if (_trace)
	    fprintf(TFile, "<cgbp> cannot eliminate, invalid def/use in region.\n");

	  continue;
	}

	if (_trace)
	  bp_ops.Print();
	
	/* Try to fixup the offsets so that we can replace 'bp' with
           'alt_bp' in '_bbs'. */

	if (!bp_ops.Fixup(bp, TN_value(diff)))
	{
	  if (_trace)
	    fprintf(TFile, "<cgbp> cannot eliminate, cannot fix offsets.\n");

	  continue;
	}
	
	bp_ops.Replace(bp, alt_bp, TN_value(diff));

	/* alt_bp may have been constant, but isn't any more */
	if (TN_is_rematerializable(alt_bp)) {
	  Reset_TN_is_rematerializable(alt_bp);
	  Set_TN_home(alt_bp, NULL);
	}

	if (_trace)
	  fprintf(TFile, "<cgbp> replace succeeded\n");

	recompute_liveness();

      }

      /* Move to the mutually unique successor. */

      BB *succ = BB_Unique_Successor(sbb);
      if (!succ || (sbb != BB_Unique_Predecessor(succ)))
	break;

      sbb = succ;
    }
  }

};


/* Entry point to find updating loads and stores in a 'loop'. */
void
Loop_Updating_Optimization (LOOP_DESCR *loop)
{
  MEM_POOL_Push(&MEM_local_nz_pool);

  /* Try to find updating opportunities in 'loop'. */

  if (TI_ISA_Has_Updating_Ops())
  {
    /* Create the updating region object for 'loop', and use it to
       perform updating opts. */
  
    UPDATING_REGION ur(LOOP_DESCR_bbset(loop),
		       &MEM_local_nz_pool,
		       Get_Trace(TP_CGPREP, 1));
    ur.optimize();
  }

  /* Try to eliminate unnecessary base-pointer adjustments outside of
     'loop'. */

  {
    BP_ELIM_REGION bpr(LOOP_DESCR_loophead(loop),
		       LOOP_DESCR_bbset(loop),
		       &MEM_local_nz_pool,
		       Get_Trace(TP_CGPREP, 1));
    bpr.optimize();
  }

  MEM_POOL_Pop(&MEM_local_nz_pool);
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
