
/* 
   Copyright (C) 2001-2006 Tensilica, Inc.  All Rights Reserved.
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



#include "defs.h"
#include "tracing.h"
#include "cg_flags.h"
#include "libti.h"
#include "cg_loop.h"
#include "cg_loop_mii.h"
#include "cg_loop_recur.h"
#include "cg_dep_graph.h"
#include "cg_sched_est.h"
#include "cg_swp.h"
#include "cg_swp_target.h"
#include "cgexp.h"
#include "const.h"
#include "tn_set.h"
#include "cgprep.h"
#include "ti_res_count.h"


void Fix_Recurrences_Before_Unrolling(CG_LOOP& cl)
{
  return;
}

/* the following is copied and modified from cg_updating.cxx in the be/cg directory */

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

/* Switch 'op' and 'inc' so that 'inc' is after 'op'. Adjust 'op'
   appropriately. Return FALSE if we can't switch them. 'doit'
   specifies whether code change is needed (if 'doit' is 1) or this
   is a test only ('doit' is 0). */
static BOOL
move_inc_after_op (OP *op, OP *inc, BOOL doit)
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

    INT64 new_offset = TN_value(offset_tn) + TN_value(inc_imm_tn);

    if (!TI_TOP_Can_Have_Immediate(new_offset, OP_code(ldst)))
      return FALSE;
      
    if (doit) {
      /* Change 'ldst's offset value... */
      TN *new_offset_tn = Gen_Literal_TN(new_offset, TN_size(offset_tn));
      Set_OP_opnd(ldst, offset_idx, new_offset_tn);
      Set_OP_omega(ldst, base_idx, OP_omega(inc, 0));
    }
  }

  if (doit) {
    /* Move 'inc' from it's current location to just after 'ldst'. */
    BB_Remove_Op(OP_bb(inc), inc);
    BB_Insert_Op_After(OP_bb(ldst), ldst, inc);
  }

  return TRUE;
}

/* try to move the base update to just one store to reduce the recurrence cycle */
static BOOL
combine_updating_stores(OP* src, OP* tgt) {

  // the two updating stores have to be identical except for the offset
  if (OP_code(src) != OP_code(tgt))
    return false;

  // there has to be a non-updating version of the store opcode
  TOP new_opcode = TI_TOP_Update_To_Nonupdate(OP_code(src));
  if (new_opcode == TOP_UNDEFINED)
    return false;

  INT base_idx = TI_TOP_Find_Operand_Use(OP_code(src), OU_base);
  INT offset_idx = TI_TOP_Find_Operand_Use(OP_code(src), OU_offset);

  if (offset_idx == -1)
    return false;

  TN* src_base = OP_opnd(src, base_idx);
  TN* tgt_base = OP_opnd(tgt, base_idx);

  if (src_base != tgt_base)
    return false;

  TN* src_offset_tn = OP_opnd(src, offset_idx);
  INT64 src_offset = TN_value(OP_opnd(src, offset_idx));
  INT64 tgt_offset = TN_value(OP_opnd(tgt, offset_idx));

  // check if the combined offset is encodable
  if (!TI_TOP_Can_Have_Immediate(src_offset+tgt_offset, OP_code(tgt)))
    return false;

  // check if the original offset is encodable in the non-updating version
  if (!TI_TOP_Can_Have_Immediate(src_offset, new_opcode))
    return false;

  // virtually separate the increment from the updating store
  OP* inc = Mk_OP(TOP_addi, src_base, src_base,
		  Gen_Literal_TN(src_offset, TN_size(src_offset_tn)));

  CG_LOOP_Init_Op(inc);
  Set_OP_omega(inc, 0, OP_omega(src, base_idx));

  OP* op = OP_next(src);
  BOOL ok = true;

  // scan between src and tgt to see if ok to move inc down to tgt
  while (op!=tgt) {
    if (OP_Refs_TN(op,src_base) && move_inc_after_op (op, inc, /*doit=*/false)==false) {
      ok = false;
      break;
    }
    op = OP_next(op);
  }

  if (!ok)
    return false;

  const ISA_OPERAND_INFO *src_info = TI_ISA_Operand_Info(OP_code(src));
  const ISA_OPERAND_INFO *info = TI_ISA_Operand_Info(new_opcode);
  INT num_opnds = TI_ISA_Op_Operands(info);
  INT num_results = TI_ISA_Op_Results(info);
  if (num_opnds >=10 || num_results>0 ||
      num_opnds != TI_ISA_Op_Operands(src_info))
    return false;

  // ok to combine so really do it in the second pass
  
  // create the new src which is a non-updating store
  // can handle up to 10 inputs
  OP* new_src = Mk_OP(new_opcode,
		      NULL, NULL, NULL, NULL, NULL,
		      NULL, NULL, NULL, NULL, NULL);
  CGPREP_Init_Op(new_src);
  CG_LOOP_Init_Op(new_src);
  Copy_WN_For_Memory_OP(new_src, src);
  OP_srcpos(new_src) = OP_srcpos(src);
  Set_OP_unroll_bb(new_src, OP_unroll_bb(src));

  // copy the operands from src to new_src plus setting the omegas
  for (INT i=0; i<num_opnds; i++) {
    Set_OP_opnd(new_src, i, OP_opnd(src,i));
    Set_OP_omega(new_src, i, OP_omega(src, i));
  }

  // insert the new src, the old one is removed later
  BB_Insert_Op_After(OP_bb(src), src, new_src);
  BB_Insert_Op_After(OP_bb(src), new_src, inc);

  // move the inc down toward tgt
  op = OP_next(inc);
  while (op!=tgt) {
    if (OP_Refs_TN(op,src_base))
      (void)move_inc_after_op (op, inc, /*doit=*/true);
    op = OP_next(op);
  }

  // update tgt offset to combine the updates from src and old tgt
  TN* new_offset_tn = Gen_Literal_TN(src_offset + tgt_offset, TN_size(src_offset_tn));
  Set_OP_opnd(tgt, offset_idx, new_offset_tn);
  Set_OP_omega(tgt, base_idx, OP_omega(src, base_idx));

  // src is removed here after all omega info is updated
  BB_Remove_Op(OP_bb(src), src);
  BB_Remove_Op(OP_bb(inc), inc);

  return true;

}

void Fix_Recurrences_After_Unrolling(CG_LOOP& cl)
{

  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);

  // try to combine identical updating stores resulted from unrolling
  // see gnats 8248
  OP* op = BB_first_op(body);
  while (op) {
    BOOL changed = false;
    if (OP_store(op) && OP_base_update(op) && !OP_unknown_addr(op)) {
      OP* op1 = OP_next(op);
      while (op1) {
	if (OP_store(op1) && OP_base_update(op1) && !OP_unknown_addr(op1)) {
	  if (combine_updating_stores(op, op1)) { // destroys op
	    changed = true;
	    break;
	  }
	}
	op1 = OP_next(op1);
      }
    }
    if (changed)
      op = BB_first_op(body);
    else
      op = OP_next(op);
  }
  return;
}


