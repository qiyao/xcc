
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
 *  Module: cg_swp_emit.cxx
 *  $Revision: 1.38 $
 *  $Date: 2000/10/24 17:54:52 $
 *  $Author: lesniak $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cg_swp_emit.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  Code generation phase after modulo scheduling and register allocation.
 *
 * =======================================================================
 * ======================================================================= */

// ************************************************************************
//
// This file implements the translation of modulo-scheduled and rotating-
// register-allocated code into the CGIR recognized by the rest of CG.
// The following lists of the constraints and possible interactions with
// downstream CG components.
// 
//
// Generation of SWP region:
// 
//  - SWP produces a region of type RID_TYPE_swp. (See be/region/region_util.h)
//  - The region contains a set of BBs that have been scheduled and
//    register allocated.
//  - There is one prolog/epilog block for each entry/exit of the region.
//  - A backpatch in the prolog/epilog blocks is converted into a TN copy.
//  - The TN copies are called the glue code.
//    They are marked OP_glue(). 
//    In the prolog, the result has already been assigned a register.
//    In the epilog, the operand has already been assigned a register.
//    Predicate copy is expanded into a sequence of operations.
//  - The SWP region is considered a black-box.  BBs in the region
//    are not processed by any subsequent optimizations, except
//    bundling in cgemit.cxx.
//  - The prolog/epilog blocks needs further optimizations,
//    but the glue code imposes some restrictions.
//
// GRA interactions:
//
//  - The TNs used in the SWP region are used in the
//    SWP region, used as results in prolog glue copies, or used 
//    as operands in the epilog copies.  Those TNs are not used elsewhere.
//    This is to relieve GRA from worrying about live ranges that span the 
//    boundary of a SWP region.  A consequence is that any invariants
//    must be assigned a new TN inside the SWP loop, and a glue
//    copy is introduced to copy the value of old TN into the new one.
//    Similar requirements for other live-in, live-out TNs are enforced
//    by the conversion of backpatches into glue copies.
//
// HB_SCHED/LRA interactions:
//
//  - The prolog/epilog are allowed to have a mix of glue and non-glue
//    operations, i.e., instructions are partially register allocated.
//    For example, the initialization of predicate registers,
//    the initialization of the loop-counter and epilog-counter are
//    inserted to the prolog block.
//
// EBO interactions:
//
//  - In general glue copies cannot be optimized away, except by GRA preferencing.
//  - EBO may combine a glue copy with other operations, but must preserve
//    the register assignment.
//
// CFLOW interactions:
//
//  - cflow may merge prolog/epilog blocks with their predecessors/successors,
//  - cflow may reorder the prolog/epilog blocks with the restriction that
//    the fall-through epilog block from the SWP region remains in the
//    fall-through path.
//  - cflow should not reorder blocks in the SWP region.
//
// REG_LIVE interaction:
//
//  - REG_LIVE information are needed by postpass GCM, and EBO after scheduling.
//  - If registers do not rotate, REG_LIVE (see reg_live.cxx) can figure out 
//    the register live-in and kill set by examining the SWP-scheduled basic
//    blocks.
//  - If registers rotate, a use of Rx refers to Ry defined in a previous 
//    iteration, it is impossible to determine the register live-in and kill
//    set without annotations.   
//  - We will add a new BB annotation kind:  ANNOT_ROTATING_KERNEL.
//    The info stored in ANNOT_ROTATING_KERNEL are the live-in, kill 
//    REGSET.
//  - We will also a BB_flag: BB_ROTATING_KERNEL to indicate the
//    existence of REGSET annotations.   The flag can be tested
//    using BB_rotating_kernel(bb).
//  - If GRA decides to rename any logical registers in the SWP kernel,
//    GRA must also to update the REGSETs in ANNOT_ROTATING_KERNEL.
//
// ************************************************************************


#define USE_STANDARD_TYPES

#include "defs.h"
#include "cg.h"
#include "tracing.h"
#include "op.h"
#include "op_list.h"
#include "bb.h"
#include "tn.h"
#include "tn_set.h"
#include "cgexp.h"
#include "cg_loop.h"
#include "register.h"
#include "annotations.h"
#include "ti_res_res.h"
#include "ti_res_count.h"
#include "gtn_universe.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "cg_swp_target.h"

#include "gra_grant.h"


// The function object returns TRUE when the schedule slot of OP[i] 
// is smaller than that of OP[j].  With bundling no two ops should
// have the same slot number; without bundling all ops with the same
// slot number (== modulo cycle number) will be sorted into
// arbitrary order.
//
struct Order_Op_State_by_Modulo_Cycles {
  const SWP_OP_vector& state;
  bool operator()(INT i, INT j) { 
    return state[i].slot < state[j].slot; 
  }
  Order_Op_State_by_Modulo_Cycles(const SWP_OP_vector& op_state):state(op_state) {}
};


// Rearrange the OPs according to the SWP schedule.
//   - first remove all OPs from the loop body
//   - then insert the OPs based on <modulo-cycle, slot> ordering
//   - also modify their controlling predicate
//  
static void 
SWP_Reorder_OPs(const SWP_OP_vector& op_state,
		const SWP_REG_ASSIGNMENT& reg_assign,
		BB *body, bool trace)
{
  BB_Remove_All(body);  // remove all OPs

  INT         i;
  vector<INT> sorted_op;
  for (i = 0; i < op_state.size(); i++) {
    if (op_state[i].op) 
      sorted_op.push_back(i);
  }
  sort(sorted_op.begin(), sorted_op.end(), Order_Op_State_by_Modulo_Cycles(op_state));

  INT ii = op_state.ii;
  for (i = 0; i < sorted_op.size(); i++) {
    OP *op =  op_state[sorted_op[i]].op;
    bool is_noop = op_state[sorted_op[i]].is_noop;
    if (!is_noop)
      OP_scycle(op) = op_state[sorted_op[i]].cycle;
    else
      OP_scycle(op) = -1;
    if (OP_has_predicate(op) &&
	TN_is_true_pred(OP_opnd(op, OP_PREDICATE_OPND)) &&
	!is_noop) {
      INT stage = op_state[sorted_op[i]].cycle / ii;
      Set_OP_opnd(op, OP_PREDICATE_OPND, reg_assign.Get_Control_Predicate(stage));
    }
    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      // removes omega to avoid false GTN promotion based on omega
      // since the omegas are not accurate anymore after reordering
      Set_OP_omega(op, opnd, 0);
    }
    BB_Append_Op(body, op);
  }

  if (trace) {
    for (i = 0; i < sorted_op.size(); i++) {
      OP *op =  op_state[sorted_op[i]].op;
      fprintf(TFile, "%d: OP %d cycle=%d\n", i, sorted_op[i], op_state[sorted_op[i]].cycle);
    }
  }
}


// Lookup the assigned register TN from the CLASS_REG_PAIR
//
static TN *
Lookup_Register_TN(SWP_REG_ASSIGNMENT::TN2TN_MAP&  tn2tn_map, 
		   CLASS_REG_PAIR                  rp,
		   TN                             *tn,
		   bool                            trace)
{
  Is_True(CLASS_REG_PAIR_rclass(rp) >= TI_ISA_Regclass_First() &&
	  CLASS_REG_PAIR_rclass(rp) <= TI_ISA_Regclass_Last(),
	  ("Lookup_Register_TN: invalid register class %d\n", CLASS_REG_PAIR_rclass(rp)));
	  
  TN *rtn;
  if (tn2tn_map.find(tn) == tn2tn_map.end()) {
    // Register TN not allocated --> Generate a new one
    rtn = Dup_TN(tn);
    Set_TN_class_reg(rtn, rp);
    // Set TN is dedicated.  Otherwise GRA will assign registers again.
    // Set_TN_is_dedicated(rtn);
    tn2tn_map[tn] = rtn;
    if (trace) {
      fPrint_TN(TFile, "SWP Allocator: Reference to %s", tn);
      fPrint_TN(TFile, " renamed as %s\n", rtn);
    }
  } else
    rtn = tn2tn_map[tn];
  return rtn;
}


// Determine the rotating register assigned for 'tn', assuming unbounded
// rotating register banks.
// The 'adjustment' is the offset in register numbering, when converting 
// from a physical pseudo-register location of the current reference to
// the corresponding (unbounded) rotating register number.
// 
TN *SWP_REG_ASSIGNMENT::Get_Register_TN(TN *tn, INT adjustment) 
{
  Is_True(adjustment >= 0,
	  ("SWP_REG_ASSIGNMENT: Unexpected negative register-number offset."));
  Is_True(reg_allocation.find(tn) != reg_allocation.end(),
	  ("SWP_REG_ASSIGNMENT: can't locate TN%d.", TN_number(tn)));
  CLASS_REG_PAIR rp = reg_allocation[tn];
  REGISTER r = CLASS_REG_PAIR_reg(rp);
  ISA_REGCLASS c = CLASS_REG_PAIR_rclass(rp);
    
  // Assuming a rotating logical register is numbered the same as the
  // corresponding physical pseudo-register location at stage 0, add in
  // the offset to get the logical register number:
  //
  // The correct register computation should be
  //   r = ((r + adjustment) % rotating_reg_avail[c]) + rotating_reg_base[c];
  // The issue is that we don't know the size of the rotating register file
  // at this time, since it is determined based on all loops in a PU, so 
  // delay the modulo computation to a postpass (see SWP_Fixup).
  //
  r = r + adjustment + rotating_reg_base[c];

  // workaround g++ bug:  Set_CLASS_REG_PAIR_reg(rp, r);
  Set_CLASS_REG_PAIR(rp, c, r);

  return Lookup_Register_TN(tn2tn_map, rp, tn, Trace());
}


TN *SWP_REG_ASSIGNMENT::Get_Control_Predicate(INT stage) const
{
  TN *tn = NULL;
#ifdef HAS_ROTATING_REGISTERS
  tn = Gen_Predicate_TN();
  REGISTER r = 
    (REGISTER)(rotating_reg_base[ISA_REGISTER_CLASS_predicate] + 
	       control_predicate_loc + 
	       stage);
  Set_TN_register(tn, r);
  // Set TN is dedicated.  Otherwise GRA will assign registers again.
  Set_TN_is_dedicated(tn);
  if (Trace()) {
    fPrint_TN(TFile, 
	      "SWP Allocator: Control predicate is %s", tn);
    fprintf(TFile, " in stage %d\n", stage);
  }
#endif
  return tn;
}

// Determine the register TN assigned for an invariant TN.
//
TN *SWP_REG_ASSIGNMENT::Get_Non_Rotating_Register_TN(TN *tn)
{
  CLASS_REG_PAIR rp;
  if (reg_allocation.find(tn) != reg_allocation.end())
    rp = reg_allocation[tn];
  else {
    ISA_REGCLASS rc = TN_register_class(tn);
    REGISTER r = REGISTER_UNDEFINED;

#ifdef TARG_XTENSA
    // try to use lower-numbered register to avoid register window overflow
    //
    // but for non-windowed abi, we do prefer caller save
    // also if this is not a leaf routine, use the higher numbered registers
    // that will be used by the calls to help GRA to have the lower numbered
    // registers
    if (!((Target_ABI == ABI_WINDOWED) && PU_Has_Calls==false)) {
#endif
      /* Try to get one that doesn't need to be saved. */
      r = REGISTER_SET_Choose_Intersection(non_rotating_reg[rc],
					   REGISTER_CLASS_caller_saves(rc));
#ifdef TARG_XTENSA
    }
#endif

    if (r == REGISTER_UNDEFINED ) r = REGISTER_SET_Choose(non_rotating_reg[rc]);
    if (r == REGISTER_UNDEFINED)
      return NULL;
    non_rotating_reg[rc] =  REGISTER_SET_Difference1(non_rotating_reg[rc], r);
    Set_CLASS_REG_PAIR(rp, rc, r);  
    reg_allocation[tn] = rp;
  }
    
  return Lookup_Register_TN(tn2tn_map, rp, tn, Trace());
}
  

// Assign the register to this local TN and this register is
// granted later by GRA.
// 
TN *SWP_REG_ASSIGNMENT:: Get_Non_Rotating_Register_Grantable_Local_TN
(
  BB *bb, TN *tn
)
{
  CLASS_REG_PAIR rp;
  if (reg_allocation.find(tn) != reg_allocation.end())
    rp = reg_allocation[tn];
  else {
    ISA_REGCLASS rc = TN_register_class(tn);
    REGISTER r = REGISTER_UNDEFINED;

    REGISTER_SET  rset;
    bool looping;

#ifdef TARG_XTENSA
    // try to use lower-numbered register to avoid register window overflow
    //
    // but for non-windowed abi, we do prefer caller save
    // also if this is not a leaf routine, use the higher numbered registers
    // that will be used by the calls to help GRA to have the lower numbered
    // registers
    if (!((Target_ABI == ABI_WINDOWED) && PU_Has_Calls==false)) {
#endif
      /* Try to get one that doesn't need to be saved. */
      rset = REGISTER_SET_Intersection(non_rotating_reg[rc], 
                                       REGISTER_CLASS_caller_saves(rc));
      looping = true;
      while (looping) {
        r = REGISTER_SET_Choose(rset);
        if (r == REGISTER_UNDEFINED) {
          looping = false; // not found, exit
        } else {
          if (GRA_GRANT_Local_Register_Okay(bb, rc, r)) {
            looping = false; // found, exit
          } else {
            rset = REGISTER_SET_Difference1(rset, r);
          }
        }
      }
#ifdef TARG_XTENSA
    }
#endif

    if (r == REGISTER_UNDEFINED ) {
      rset = non_rotating_reg[rc];
      looping = true;
      while (looping) {
        r = REGISTER_SET_Choose(rset);
        if (r == REGISTER_UNDEFINED) {
          looping = false; // not found, exit
        } else {
          if (GRA_GRANT_Local_Register_Okay(bb, rc, r)) {
            looping = false; // found, exit
          } else {
            rset = REGISTER_SET_Difference1(rset, r);
          }
        }
      }
    }

    if (r == REGISTER_UNDEFINED)
      return NULL;
    non_rotating_reg[rc] =  REGISTER_SET_Difference1(non_rotating_reg[rc], r);
    Set_CLASS_REG_PAIR(rp, rc, r);  
    reg_allocation[tn] = rp;
  }
    
  return Lookup_Register_TN(tn2tn_map, rp, tn, Trace());
}
  
// Return TRUE if all loop invariants can be assigned registers
//
bool SWP_REG_ASSIGNMENT::Enough_Non_Rotating_Registers(TN_SET *non_rotating) const
{

  bool enough_register = TRUE;

  INT reg_needed[TI_ISA_REGCLASS_MAX+1];
  ISA_REGCLASS i;
  FOR_ALL_ISA_REGCLASS(i) {
    reg_needed[i] = 0;
  }

  for (TN *tn = TN_SET_Choose(non_rotating);
       tn != TN_SET_CHOOSE_FAILURE;
       tn = TN_SET_Choose_Next(non_rotating,tn)) {
    ISA_REGCLASS rc = TN_register_class(tn);
    reg_needed[rc]++;
  }

  if (SWP_REG_ASSIGNMENT::Trace()) {
    fprintf(TFile, "\nSWP Allocator: Non-Rotating Register Usage\n");
    fprintf(TFile, "--------------------------\n");
  }
  const ISA_REGCLASS_INFO *info;
  FOR_ALL_ISA_REGCLASS(i) {
    info = TI_ISA_Regclass_Info(i);
    if (TI_ABI_Property_Set(ABI_PROPERTY_allocatable,i, /*reg=*/-1)) {
      if (SWP_REG_ASSIGNMENT::Trace()) {
        fprintf(TFile, "Reg class %10s has %3d allocatable and needs %3d registers\n",
		        TI_ISA_Regclass_Name(info),
			REGISTER_SET_Size(non_rotating_reg[i]), reg_needed[i]);
      }
      if (REGISTER_SET_Size(non_rotating_reg[i]) < reg_needed[i]) {
        enough_register = FALSE;
        if (SWP_REG_ASSIGNMENT::Trace()) {
          fprintf(TFile, "Reg class %10s does not have enough allocatable register\n",
		        TI_ISA_Regclass_Name(info));
        }
      }
    } else {
      if (SWP_REG_ASSIGNMENT::Trace()) {
        fprintf(TFile, "Reg class %10s is not allocated\n",
		        TI_ISA_Regclass_Name(info));
      }
    }
  }
  
  return enough_register;
}

  
// Update SWP BB annotation 
//  - the livein and kill REGSET
void SWP_REG_ASSIGNMENT::Update_Annotation(ROTATING_KERNEL_INFO *info)
{
  ISA_REGCLASS i;
  FOR_ALL_ISA_REGCLASS(i) {
    // The processing of int register are processed in the fixup
    //
    REGISTER_SET tmp = REGISTER_CLASS_allocatable(i);
    tmp = REGISTER_SET_Difference(tmp, non_rotating_reg[i]);

#ifdef HAS_ROTATING_REGISTERS
    if (i != TI_ISA_Regclass_Integer()) {
      // assume all rotating registers are killed 
      // for some reason, rotating reg are not in REGISTER_CLASS_allocatable()?
      tmp = REGISTER_SET_Union(tmp,
			       REGISTER_SET_Range(rotating_reg_base[i],
						  rotating_reg_base[i] + rotating_reg_avail[i] - 1));
    }
#endif

    ROTATING_KERNEL_INFO_live_in(info)[i] = tmp;
    ROTATING_KERNEL_INFO_kill(info)[i] = tmp;
  }
}


// Initialization of SWP_REG_ASSIGNMENT
//
SWP_REG_ASSIGNMENT::SWP_REG_ASSIGNMENT(ISA_REGSUBCLASS multireg_sc)
{
  ISA_REGCLASS i;
  FOR_ALL_ISA_REGCLASS(i) {

#ifdef HAS_ROTATING_REGISTERS
    rotating_reg_base[i] = REGISTER_First_Rotating_Registers(i);
    rotating_reg_avail[i] = REGISTER_Last_Rotating_Registers(i) - 
      REGISTER_First_Rotating_Registers(i) + 1;
    
    // mask out rotating register from non-rotating allocatable set.
    non_rotating_reg[i] =
      REGISTER_SET_Difference_Range(REGISTER_CLASS_allocatable(i),
				    rotating_reg_base[i],
				    rotating_reg_base[i] + rotating_reg_avail[i] - 1);

#else
    rotating_reg_base[i] = 0;
    rotating_reg_avail[i] = 0;
    non_rotating_reg[i] = REGISTER_CLASS_allocatable(i);

#endif
  }

#ifndef HAS_ROTATING_REGISTERS
  Init_for_Multireg_oprands(multireg_sc);

  // Save non_rotating_reg[ISA_REGCLASS_branch], so Clear() can
  // reset non_rotating_reg[ISA_REGCLASS_branch].
  multireg_boolean_reg = non_rotating_reg[ISA_REGCLASS_branch];
#endif

}

void
SWP_REG_ASSIGNMENT::Clear() {

  ISA_REGCLASS i;
  FOR_ALL_ISA_REGCLASS(i) {
    rotating_reg_used[i] = 0;
    non_rotating_reg[i] = REGISTER_CLASS_allocatable(i);
  }

#ifndef HAS_ROTATING_REGISTERS
  non_rotating_reg[ISA_REGCLASS_branch] = multireg_boolean_reg;
#endif

  tn2tn_map.clear();
  reg_allocation.clear();

}


// Generate a glue copy
//
static void
SWP_Add_Glue(TN *result, TN *opnd, BB *bb, bool append)
{
  OPS ops = OPS_EMPTY;
  SWP_Exp_COPY(result, opnd, &ops); 
  OP *op;
  FOR_ALL_OPS_OPs(&ops, op) {
    /*
      For TOP_movbr8 (from opnd to result), we generate
         TOP_get_tmp_ar tn1
         TOP_get_tmp_ar tn2
         TOP_movbr8 result opnd tn1 tn2
       we need to mark only the last one as glue, so let's
       skip TOP_get_tmp_ar !
     */
    if (OP_code(op) != TOP_get_tmp_ar)
      Set_OP_glue(op);
  }
  if (append)
    BB_Append_Ops(bb, &ops);
  else
    BB_Prepend_Ops(bb, &ops);

  Set_BB_has_glue_copy(bb);
}


// Replace the TN in the SWP loop by register-assigned TNs.
//  - A TN in SWP are used in different stages, hence have
//    different register allocated.   Since one TN only has
//    one assignment, it is impossible to keep the same TN.
//    
static bool
SWP_Rename_TNs(const SWP_OP_vector& op_state, 
	       SWP_REG_ASSIGNMENT& reg_assign,
	       BB *head, BB *body, BB *tail,
	       TN* trip_count_tn, TN** loop_var_tn_ptr)
{
  INT     ii = op_state.ii;
  INT     sc = op_state.sc;
  TN_SET *non_rotating = op_state.tn_non_rotating;
  TN_SET *invariants = op_state.tn_invariants;

  if (SWP_REG_ASSIGNMENT::Trace()) {
    fprintf(TFile, "\nSWP Allocator: TN Renaming\n");
    fprintf(TFile, "--------------------------\n");
    fprintf(TFile, "Invariants:\n");
  }

  // record which AR register is used in the body but is not live-in
  // this will be used to allocate 1 register to the loop instruction
  REGISTER_SET AR_used_but_not_live_in = REGISTER_SET_EMPTY_SET;

  {
    for (TN *tn = TN_SET_Choose(invariants);
	 tn != TN_SET_CHOOSE_FAILURE;
	 tn = TN_SET_Choose_Next(invariants,tn)) {
      if (!TN_is_dedicated(tn)) {
	TN *rtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	if (!rtn)
	  return SWP_Failure(body, REG_ALLOC_FAILED, TN_register_class(tn) );
	SWP_Add_Glue(rtn, tn, head, true/*append*/ );
	Set_TN_is_global_reg(rtn);
	GTN_UNIVERSE_Add_TN(rtn);
      }
    }
  }

  if (SWP_REG_ASSIGNMENT::Trace()) {
    fprintf(TFile, "--------------------------\n");
    fprintf(TFile, "Variants:\n");
  }

  // Rename SWP body; note that we will not rename the special control
  // registers created by SWP_REG_ASSIGNMENT::Get_Control_Predicate, since
  // these are marked as "dedicated".
  //
  for (INT i = 0; i < op_state.size(); i++) {
    OP *op = op_state[i].op;
    if (op) {
      for (INT j = 0; j < OP_opnds(op); j++) {
	TN *tn = OP_opnd(op, j);
	if (TN_is_register(tn) &&
	    !TN_is_dedicated(tn)) {
	  TN *newtn;
	  if (!TN_SET_MemberP(non_rotating, tn)) {
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, OP_omega(op,j));
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  } else {
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	    if (!newtn)
	      return SWP_Failure(body, REG_ALLOC_FAILED, TN_register_class(tn));
	  }
	  Set_OP_opnd(op, j, newtn);
	}
      }
      for (INT k = 0; k < OP_results(op); k++) {
	TN *tn = OP_result(op, k);
	if (TN_is_register(tn) &&
	    !TN_is_dedicated(tn)) {
	  Is_True(!TN_SET_MemberP(invariants, tn),
		  ("SWP_Rename_Body: result TN%d cannot be an invariant.", TN_number(tn)));
	  TN *newtn;
	  if (!TN_SET_MemberP(non_rotating, tn)) {
	    INT ofst = reg_assign.Get_Register_Offset(op_state[i].cycle, ii, 0);
	    newtn = reg_assign.Get_Register_TN(tn, ofst);
	  } else {
	    newtn = reg_assign.Get_Non_Rotating_Register_TN(tn);
	    if (!newtn)
	      return SWP_Failure(body, REG_ALLOC_FAILED, TN_register_class(tn));
	  }
	  Set_OP_result(op, k, newtn);
	  if (TN_register_class(newtn)==TI_ISA_Regclass_Integer()) {
	    AR_used_but_not_live_in =
		REGISTER_SET_Union1(AR_used_but_not_live_in, TN_register(newtn));
	  }
	}
      }
    }
  }

  CG_LOOP_BACKPATCH *bp;
  // Generate copies for prolog-backpatches.
  for (bp = CG_LOOP_Backpatch_First(head, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN *newtn;

    // if a body_tn is neither non_rotating nor invariant then
    // it is a induction variable used only in loh ops and should be
    // removed if SWP is successful or it is a variable used in
    // an op already removed (e.g., from copy propagation in EBO)
    if (!TN_is_dedicated(body_tn)
	&& (TN_SET_MemberP(non_rotating, body_tn) ||
	    TN_SET_MemberP(invariants, body_tn))) {
      if (!TN_SET_MemberP(non_rotating, body_tn)) {
	INT omega = CG_LOOP_BACKPATCH_omega(bp);
	INT ofst = reg_assign.Get_Livein_Register_Offset(omega);
	newtn = reg_assign.Get_Register_TN(body_tn, ofst);
      } else {
	newtn = reg_assign.Get_Non_Rotating_Register_TN(body_tn);
      }
      if (!newtn)
	return SWP_Failure(body, REG_ALLOC_FAILED,TN_register_class(body_tn));
      SWP_Add_Glue(newtn, tn, head, true/*append*/);
      Set_TN_is_global_reg(newtn);
      GTN_UNIVERSE_Add_TN(newtn);
      if (TN_register_class(newtn)==TI_ISA_Regclass_Integer()) {
	AR_used_but_not_live_in =
		REGISTER_SET_Difference1(AR_used_but_not_live_in, TN_register(newtn));
      }
    }
  }

  // Generate copies for epilog-backpatches.
  for (bp = CG_LOOP_Backpatch_First(tail, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp); 
    TN *newtn;

    // if a body_tn is neither non_rotating nor invariant then
    // it is a induction variable used only in loh ops and should be
    // removed if SWP is successful ir ut us a variable used in
    // an op already removed (e.g., from copy propagation in EBO)
    if (!TN_is_dedicated(body_tn) &&
	(TN_SET_MemberP(non_rotating, body_tn) ||
	 TN_SET_MemberP(invariants, body_tn))) {

      if (!TN_SET_MemberP(non_rotating, body_tn)) {
	INT omega = CG_LOOP_BACKPATCH_omega(bp);
	INT ofst = reg_assign.Get_Liveout_Register_Offset(sc, omega);
	newtn = reg_assign.Get_Register_TN(body_tn, ofst);
      } else {
	newtn = reg_assign.Get_Non_Rotating_Register_TN(body_tn);
      }
      if (!newtn)
	return SWP_Failure(body, REG_ALLOC_FAILED, TN_register_class(body_tn));
      SWP_Add_Glue(tn, newtn, tail, false/*prepend*/);
      Set_TN_is_global_reg(newtn);
      GTN_UNIVERSE_Add_TN(newtn);
    }
  }

  // try to use any AR that is not live-in to the body
  REGISTER loop_var_reg = REGISTER_SET_Choose(AR_used_but_not_live_in);
  TN *tmp_tn = Gen_Typed_Register_TN(TN_mtype(trip_count_tn),
                                       TN_size(trip_count_tn));

  if (loop_var_reg == REGISTER_UNDEFINED) {
    // if no non-live-in AR exists, then allocate a new one
    tmp_tn = reg_assign.Get_Non_Rotating_Register_Grantable_Local_TN(head,tmp_tn);
  } else {
    CLASS_REG_PAIR rp;
    Set_CLASS_REG_PAIR(rp, TI_ISA_Regclass_Integer(), loop_var_reg);  
    Set_TN_class_reg(tmp_tn, rp);
  }

  // exit if we fail to allocate an AR for the loop instruction
  if (tmp_tn==NULL) {
    if (SWP_REG_ASSIGNMENT::Trace()) {
      fprintf(TFile, "\nNot enough AR remains.\n\n");
    }
    return SWP_Failure(body, REG_ALLOC_FAILED, TI_ISA_Regclass_Integer());
  } else {

    // allocate for loop variable used in zero cost loop instruction
    // this is to make sure that GRA may not accidentally use the
    // only register to be allocated for the loop instruction (PR10952)
    // to restore spilled GRA lrange above the loop instruction causing
    // out of register failure in LRA when trying to allocate for the
    // loop instruction
    if (loop_var_tn_ptr) {
      *loop_var_tn_ptr = tmp_tn;
    }
  }

  return TRUE;
} // SWP_Rename_TNs


static TN*
Fixup_Rotating_Register_TN(TN *tn, const SWP_FIXUP &fixup, bool trace)
{
  TN *rtn = tn;

#ifdef HAS_ROTATING_REGISTERS
  // The allocation of rotating registers thus far has been unbounded and
  // based at the appropriate rotating_reg_base.  Now we know how many 
  // registers are assigned to each rotating register bank, and we can
  // adjust all registers such that they are in-sequence within the
  // rotating register bank.  We also adjust predicate registers such 
  // that the special control predicate is located in the n first logical
  // registers (n==stages).
  //
  const ISA_REGCLASS rc = TN_register_class(tn);

  REGISTER       r = TN_register(tn);

  const REGISTER old_r = r;
  if (REGISTER_Has_Rotating_Registers(rc)) {

    // Invariants will have been allocated at register numbers smaller than
    // the rotating register bank.  The following ignores such registers in
    // the fixup algorithm.
    //
    if (r >= REGISTER_First_Rotating_Registers(rc)) {
      if (rc == ISA_REGISTER_CLASS_predicate) {
	r -= fixup.control_loc;  // Control lifetime uses first rotating regs
	if (r < REGISTER_First_Rotating_Registers(rc))
	  r += REGISTER_Last_Rotating_Registers(rc) - 
	    REGISTER_First_Rotating_Registers(rc) + 1;
	else if (r > REGISTER_Last_Rotating_Registers(rc))
	  r -= REGISTER_Last_Rotating_Registers(rc) - 
	    REGISTER_First_Rotating_Registers(rc) + 1;
      }
      else if (rc == TI_ISA_Regclass_Integer()) {
	if (r >= REGISTER_First_Rotating_Registers(rc) +
	    REGISTER_Number_Stacked_Rotating(rc)) {
	  r -= REGISTER_Number_Stacked_Rotating(rc);
	}
      }
      else if (r > REGISTER_Last_Rotating_Registers(rc)) {
	r -= REGISTER_Last_Rotating_Registers(rc) - 
	  REGISTER_First_Rotating_Registers(rc) + 1;
      }

      Is_True(r <= REGISTER_Last_Rotating_Registers(rc) &&
	      r >= REGISTER_First_Rotating_Registers(rc),
	      ("cannot wrap around twice."));
    }
  }
  if (old_r != r) 
    Set_TN_register(tn, r);

  if (trace && old_r != r) {
    fprintf (TFile, "Rotated register %s;", REGISTER_name(rc, old_r));
    fPrint_TN(TFile, " result is %s\n", tn);
  }
#endif // TARG_IA64
  return rtn;
} // Fixup_Rotating_Register_TN


static void 
SWP_Fixup_Rotating_Registers(BB              *head, 
			     BB              *body, 
			     BB              *tail,
			     const SWP_FIXUP &fixup,
			     bool             trace)
{
  CXX_MEM_POOL local_pool("fixup local pool", FALSE);
  TN_SET *fixed_tn = TN_SET_Create_Empty(Last_TN + 1, local_pool());

  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	Set_OP_opnd(op,j, new_tn);
	fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
      }
    }
    for (INT k = 0; k < OP_results(op); k++) {
      TN *tn = OP_result(op, k);
      if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	Set_OP_result(op, k, new_tn);
	fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
      }
    }
  }
  FOR_ALL_BB_OPs(head, op) {
    if (OP_glue(op)) {
      for (INT k = 0; k < OP_results(op); k++) {
	TN *tn = OP_result(op, k);
	if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	  TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);	
	  Set_OP_result(op, k, new_tn);
	  fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
	}
      }
    }
  }
  FOR_ALL_BB_OPs(tail, op) {
    if (OP_glue(op)) {
      for (INT j = 0; j < OP_opnds(op); j++) {
	TN *tn = OP_opnd(op, j);
	if (TN_is_register(tn) && !TN_SET_MemberP(fixed_tn, tn)) {
	  TN *new_tn = Fixup_Rotating_Register_TN(tn, fixup, trace);
	  Set_OP_opnd(op, j, new_tn);
	  fixed_tn = TN_SET_Union1D(fixed_tn, new_tn, local_pool());
	}
      }
    }
  }
} // SWP_Fixup_Rotating_Registers

void
SWP_Emit_Loop_Inst(BB *head, BB *body,
		   TN *trip_count_tn, TN *loop_var_tn, INT sc)
{
  // insert the zero-overhead loop operation
  {
    TN *loop_trip_count_tn = trip_count_tn;
    OPS prolog_ops = OPS_EMPTY; 
    OPS body_ops = OPS_EMPTY; 
    LABEL_IDX body_label = Gen_Label_For_BB(body);
    TN *body_target_label = Gen_Label_TN(body_label,0);

    FmtAssert ( loop_var_tn != NULL && TN_register(loop_var_tn),
		("loop_var_tn not allocated in SWP_Emit_Loop_Inst"));

    if (TN_is_constant(trip_count_tn)) {
      TN *tmp_tn = Gen_Typed_Register_TN(TN_mtype(trip_count_tn),
                                       TN_size(trip_count_tn));
      Exp_COPY(tmp_tn, trip_count_tn, &prolog_ops);
      loop_trip_count_tn = tmp_tn;
    }

    if (sc!=1) {
      TN *neg_sc_tn = Gen_Literal_TN(-(sc-1),4);
      Build_OP(TOP_addi, loop_trip_count_tn, loop_trip_count_tn,
	       neg_sc_tn, &prolog_ops);
    }

    // we need to insert trip count calculation in prolog_ops
    // before glue copies
    CG_LOOP_Init_OPS(&prolog_ops);

    OP* scan = BB_last_op(head);
    while (scan && OP_glue(scan))
      scan = OP_prev(scan);

    if (scan) {
      BB_Insert_Ops_After(head, scan, &prolog_ops);
    } else {
      BB_Prepend_Ops(head, &prolog_ops);
    }

    OPS_Remove_All(&prolog_ops);

    // use the pre-allocated loop variable TN and provide a glue copy
    // from the trip count TN
    SWP_Add_Glue(loop_var_tn, loop_trip_count_tn, head, true/*append*/);

    // this is to ensure that the register remains allocated
    // otherwise it may be re-allocated since it is actually a local TN
    Set_TN_is_dedicated(loop_var_tn);

    CGTARG_Generate_Branch_Cloop(
			     head,
                             body,
                             NULL,
                             loop_var_tn,
                             /* ntimes not used */0,
                             body_target_label,
                             &prolog_ops,
                             &body_ops);

    CG_LOOP_Init_OPS(&prolog_ops);
    CG_LOOP_Init_OPS(&body_ops);

    BB_Append_Ops(head, &prolog_ops);
    BB_Append_Ops(body, &body_ops);
  }
}

BOOL 
SWP_Emit(SWP_OP_vector& op_state,
	 SWP_REG_ASSIGNMENT& reg_assign,
	 TN *trip_count_tn,
	 BB *head, BB *body, BB *tail, 
	 bool is_doloop, bool trace)
{
  if (trace) {
    CG_LOOP_Backpatch_Trace(head, NULL);
    CG_LOOP_Backpatch_Trace(tail, NULL);
  }

  // Create a SWP REGION
  RID *r = RID_Create(New_Region_Id(), 0, NULL);
  RID_has_reg_alloc_Set(r);
  RID_level(r) = RL_CGSCHED;
  RID_type(r) = RID_TYPE_swp;
  RID_bounds_exist(r) = REGION_BOUND_UNKNOWN;
  RID_has_return(r) = REGION_NO_RETURN;
  RID_num_exits(r) = 1;
  RID_is_glue_code(r) = FALSE;        
  RID *parent = BB_rid(body);
  RID_parent(r) = parent;
  RID_cginfo(r) = NULL; /* ?? this should have a value */
  if ( parent ) RID_Add_kid(r, parent);

  BB_rid(body) = r;
  Set_BB_reg_alloc(body);
  Set_BB_scheduled(body);
  
  // Rearrange OPs in modulo order
  // Loop overhead ops are also removed as a side-effect
  SWP_Reorder_OPs(op_state, reg_assign, body, trace);

  // Generate br.ctop
  OPS prolog_ops = OPS_EMPTY;
  OPS body_ops = OPS_EMPTY;
  OPS epilog_ops = OPS_EMPTY;

  INT32 prolog_epilog_count = op_state.sc;
  if (!is_doloop && op_state.loop_one_more_time)
    prolog_epilog_count--;

  SWP_Loop_Init_Fini(is_doloop, prolog_epilog_count, &prolog_ops, &body_ops, &epilog_ops);

  BB_Append_Ops(head, &prolog_ops);
  BB_Append_Ops(body, &body_ops);  
  BB_Append_Ops(tail, &epilog_ops);

  // Setting the following disables bb merge in CFLOW
  // Set_BB_mod_pred_rotating_registers(head);
  // Set_BB_mod_rotating_registers(tail);

  TN *loop_var_tn;

  // Rename body, backpatches and
  // generate glue copies for invariants
  bool rename_ok =
	SWP_Rename_TNs(op_state, reg_assign, head, body, tail,
		       trip_count_tn, &loop_var_tn);

  if (rename_ok==FALSE)
    return FALSE;

  // check and return boundary case early to avoid polluting annotations
  INT stage_count = op_state.sc;
  BOOL constant_trip_count = TN_has_value(trip_count_tn);
  INT64 trip_count = constant_trip_count? TN_value(trip_count_tn) : 0;
  if (constant_trip_count && trip_count<stage_count) {
    /* the loop is essentially fully unrolled in wind-up/down
     * since the caller may still have references to body, we cannot
     * simply remove the body.
     * fail this modulo schedule for short trip-count loop for now
     */
    return SWP_Failure(body, TOO_FEW_TRIPS);
  }

  {
    // Generate SWP ROTATING KERNEL Annotation
    ROTATING_KERNEL_INFO *info = TYPE_PU_ALLOC(ROTATING_KERNEL_INFO);
    memset(info, 0, sizeof(ROTATING_KERNEL_INFO));
    reg_assign.Update_Annotation(info);
    
    // Regenerate SWP resource statistics
    TI_RES_COUNT *res_counts = TI_RES_COUNT_Alloc(MEM_pu_pool_ptr);
    // Sum resources from each OP's resource usage.
    OP *op;
    INT real_op_count=0;
    for (INT i = 0; i < op_state.size(); i++) {
      OP *op = op_state[i].op;
      if (op && !op_state[i].is_noop) {
	TI_RES_COUNT_Add_Op_Resources(res_counts, OP_code(op));
	real_op_count++;
      }
    }

    // Save SWP statistics
    ROTATING_KERNEL_INFO_succeeded(info) = TRUE;
    ROTATING_KERNEL_INFO_ii(info) = op_state.ii;
    ROTATING_KERNEL_INFO_stage_count(info) = op_state.sc;
    ROTATING_KERNEL_INFO_min_ii(info) = op_state.min_ii;    
    ROTATING_KERNEL_INFO_res_min_ii(info) = op_state.res_min_ii;
    ROTATING_KERNEL_INFO_rec_min_ii(info) = op_state.rec_min_ii;
    ROTATING_KERNEL_INFO_sched_len(info) = op_state.sl;
    ROTATING_KERNEL_INFO_min_sched_len(info) = op_state.min_sl;
    ROTATING_KERNEL_INFO_res_counts(info) = res_counts;
    ROTATING_KERNEL_INFO_real_op_count(info) = real_op_count;

    FOR_ALL_BB_OPs(head, op) {
      if (OP_glue(op)) {
	for (INT k = 0; k < OP_results(op); k++) {
	  TN *tn = OP_result(op, k);
	  if (!TN_is_const_reg(tn))
	    ROTATING_KERNEL_INFO_copyin(info).push_back(tn);
	}
      }
    }
    FOR_ALL_BB_OPs(tail, op) {
      if (OP_glue(op)) {
	for (INT j = 0; j < OP_opnds(op); j++) {
	  TN *tn = OP_opnd(op, j);
	  if (!TN_is_const_reg(tn) && TN_is_global_reg(tn))
	    ROTATING_KERNEL_INFO_copyout(info).push_back(tn);
	}
      }
    }

    BB_Add_Annotation(body, ANNOT_ROTATING_KERNEL, (void *)info);
  }

  // insert the zero-overhead loop operation
  SWP_Emit_Loop_Inst(head, body, trip_count_tn, loop_var_tn, op_state.sc);

  Link_Pred_Succ_with_Prob(head, tail, 0.0f);

  if (trace) {
    Print_BB(head);
    Print_BB(body);
    Print_BB(tail);
  }

  if (SWP_Options.Enable_BRP) Gen_SWP_Branch_Predict(body, head, tail);

  if (stage_count>1) {
    BB* prolog = head;
    BB* epilog = tail;
    OP* op=NULL;
    OP* next_op=NULL;
    FmtAssert(TN_value(trip_count_tn)>0,("trip_count<=0"));
    // need to fixup wind-up/down to handle small trip count
    for (INT i=1; i<stage_count-1; i++) {
      BB* new_prolog_bb = Gen_And_Insert_BB_After(prolog);
      BB* new_epilog_bb = Gen_And_Insert_BB_Before(epilog);
      CG_LOOP_prolog = new_prolog_bb;
      CG_LOOP_epilog = new_epilog_bb;
      BB_flag(new_prolog_bb) = BB_flag(prolog);
      BB_flag(new_epilog_bb) = BB_flag(epilog);
      Reset_BB_has_label(new_epilog_bb);
      LABEL_IDX new_label = Gen_Label_For_BB(new_epilog_bb);
      LABEL_IDX old_label = Gen_Label_For_BB(epilog);
      TN* new_label_tn = Gen_Label_TN(new_label,0);
      TN* old_label_tn = Gen_Label_TN(old_label,0);
      OP* loop_op=BB_last_op(prolog);
      Set_OP_opnd(loop_op, 1, new_label_tn);

      op = BB_first_op(prolog);
      while (op) {
	next_op = OP_next(op);
	if (!Is_CG_LOOP_Op(op) || OP_swp_wind_up_section(op)!=i) {
	  BB_Move_Op_To_End(new_prolog_bb,prolog,op);
	}
	op = next_op;
      }
      Reset_BB_has_glue_copy(prolog); // all glue copies are moved

      op = BB_first_op(epilog);
      while (op) {
	next_op = OP_next(op);
	if (!Is_CG_LOOP_Op(op) || OP_swp_wind_down_section(op)!=i) {
	  BB_Move_Op_To_End(new_epilog_bb,epilog,op);
	}
	op = next_op;
      }
      Reset_BB_has_glue_copy(epilog); // all glue copies are moved

      Unlink_Pred_Succ(prolog, epilog);
      Unlink_Pred_Succ(prolog, body);
      Unlink_Pred_Succ(body, epilog);
      Link_Pred_Succ_with_Prob(prolog, new_prolog_bb, 1.0f);
      Link_Pred_Succ_with_Prob(new_prolog_bb, new_epilog_bb, 0.0f);
      Link_Pred_Succ_with_Prob(new_prolog_bb, body, 1.0f);
      Link_Pred_Succ_with_Prob(body, new_epilog_bb,
			1.0f - BBLIST_prob(BB_Find_Succ(body,body)));
      Link_Pred_Succ_with_Prob(new_epilog_bb, epilog, 1.0f);

      if (!constant_trip_count) {
	INT32 trip_size = TN_size(trip_count_tn);
	TN* threshold = Gen_Literal_TN(i+1,trip_size);
	OP* br_op = Mk_OP(TOP_blti, trip_count_tn, threshold, old_label_tn);
	BB_Append_Op(prolog, br_op);
	Link_Pred_Succ_with_Prob(prolog, epilog, 0.0f);
      }

      prolog = new_prolog_bb;
      epilog = new_epilog_bb;
    }
  }

  return TRUE;

}


// Postpass SWP Fixup 
//  - it is run after all SWP loop has been processed.
//  - called from Perform_Loop_Optimization
//
void SWP_Fixup(SWP_FIXUP& fixup)
{
  BB *prolog = fixup.prolog;
  BB *body = fixup.body;
  BB *epilog = fixup.epilog;

  SWP_Fixup_Rotating_Registers(prolog, body, epilog, fixup, 
			       SWP_REG_ASSIGNMENT::Trace());

  // Update the requirement of the configurable int rotating registers
  ANNOTATION *annot = ANNOT_Get(BB_annotations(body), ANNOT_ROTATING_KERNEL);
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
  ISA_REGCLASS rc = TI_ISA_Regclass_Integer();
  BOOL trace = Get_Trace(TP_SWPIPE, 2);

#ifdef HAS_ROTATING_REGISTERS
  const REGISTER first_rotating_reg =  REGISTER_First_Rotating_Registers(rc);
  const REGISTER num_rotating_reg = REGISTER_Number_Stacked_Rotating(rc);

  for (INT i = 0; i < num_rotating_reg; i++) {
    REGISTER reg = i + first_rotating_reg;
    ROTATING_KERNEL_INFO_live_in(info)[rc] =
      REGISTER_SET_Union1(ROTATING_KERNEL_INFO_live_in(info)[rc], reg);
    ROTATING_KERNEL_INFO_kill(info)[rc] = 
      REGISTER_SET_Union1(ROTATING_KERNEL_INFO_kill(info)[rc], reg);
  }
#endif

  if (trace) {
    fprintf(TFile, "Reminder: REGISTER SET number differs from real register by 1.\n");
    FOR_ALL_ISA_REGCLASS(rc) {
      fprintf(TFile, "SWP annotation: register class %d", rc);
      fprintf(TFile, "\nlivein: ");
      REGISTER_SET_Print(ROTATING_KERNEL_INFO_live_in(info)[rc], TFile);
      fprintf(TFile, "\nkill:   ");
      REGISTER_SET_Print(ROTATING_KERNEL_INFO_kill(info)[rc], TFile);
      fprintf(TFile,"\n");
    }
  }

#ifdef HAS_ROTATING_REGISTERS
#ifdef Is_True_On
  // Verify that p63 is not modified in the last cycle
  {
    OP *op;
    INT ii = ROTATING_KERNEL_INFO_ii(info);
    OP *br = BB_branch_op(body);
    INT br_cycle = OP_scycle(br) % ii;
    FOR_ALL_BB_OPs(body, op) {
      if (OP_scycle(op) % ii == br_cycle) {
	for (INT k = 0; k < OP_results(op); k++) {
	  TN *tn = OP_result(op, k);
	  if (TN_is_register(tn)) {
	    if (TN_register_class(tn) == ISA_REGISTER_CLASS_predicate &&
		TN_register(tn) == 64) {
	      Is_True(FALSE, ("SWP_Fixup: p63 is modified."));
	    }
	  }
	}
      }
    }
  }
#endif

#endif
}

