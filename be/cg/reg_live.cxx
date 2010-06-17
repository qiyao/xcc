
/* 
   Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.
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
 *  Module: reg_live.cxx
 *  $Revision: 1.48 $
 *  $Date: 2000/05/19 20:16:22 $
 *  $Author: murthy $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/reg_live.cxx,v $
 *
 *  Description:
 *  ============
 *
 *  Physical register global live range analysis.
 *  Original extracted from Suneel's gcm.cxx.
 *
 * =======================================================================
 * =======================================================================
 */

#include <alloca.h>
#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "pu_info.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "targ_sim.h"
#include "bb_set.h"
#include "freq.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_vector.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "data_layout.h"

#include "reg_live.h"

static BOOL Trace_Register_Liveness = FALSE;

static REGSET Register_Livein;
static REGSET Register_Kill;
static MEM_POOL Reg_Live_Pool;
static BOOL pool_initialized = FALSE;

#define BB_Register_Livein(bb) \
	  (Register_Livein + BB_id(bb)*TI_ISA_Num_Regclasses() - TI_ISA_Regclass_First())
#define BB_Register_Kill(bb) \
	  (Register_Kill + BB_id(bb)*TI_ISA_Num_Regclasses() - TI_ISA_Regclass_First())

static INT32 regset_max_size=0;
static bool *RegLiveValidForBB=NULL;

//
// Utility functions to manipulate REGSETs (arrays of REGISTER_SETs).
//

void REGSET_Print (REGSET set)
{
  ISA_REGCLASS rc;
  REGISTER reg;
  fprintf (TFile, "[");
  FOR_ALL_ISA_REGCLASS(rc) {
    FOR_ALL_REGISTER_SET_members (set[rc], reg) {
      fprintf (TFile, " %s", REGISTER_name (rc, reg));
    }
  }
  fprintf (TFile, " ]");
}

void 
REGSET_ASSIGN (REGSET set1, REGSET set2)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    set1[rc] = set2[rc];
  }
}

void 
REGSET_CLEAR (REGSET set)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    set[rc] = REGISTER_SET_EMPTY_SET;
  }
}

void 
REGSET_OR (REGSET set1, REGSET set2)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    set1[rc] = REGISTER_SET_Union(set1[rc], set2[rc]);
  }
}

BOOL 
REGSET_EQUALS (REGSET set1, REGSET set2)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    if (!REGISTER_SET_EqualP(set1[rc], set2[rc])) return FALSE;
  }
  return TRUE;
}

BOOL 
REGSET_INTERSECT (REGSET set1, REGSET set2)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    if (REGISTER_SET_IntersectsP(set1[rc], set2[rc])) return TRUE;
  }
  return FALSE;
}

static void 
REGSET_UPDATE_LIVEIN ( REGSET livein, REGSET liveout, REGSET kill)
{
  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    REGISTER_SET livein_set = REGISTER_SET_Difference(liveout[rc], kill[rc]);
    livein[rc] = REGISTER_SET_Union(livein[rc], livein_set);
  }
}


static void 
Add_PREG_To_REGSET (PREG_NUM preg, REGSET regset)
{
  if (preg != 0) {
    ISA_REGCLASS cl;
    REGISTER reg;

    FmtAssert(CGTARG_Preg_Register_And_Class(preg, &cl, &reg),
	      ("Don't know how to handle PREG%d", preg));

    regset[cl] = REGISTER_SET_Union1 (regset[cl], reg);
  }
}

// Utility routine to determine the parameter registers for:
//	- actual parameters for a procedure call
//	- formal parameters for a procedure
//
static void
Compute_Parameter_Regs (TY_IDX call_ty, WN *call_wn, REGSET parms)
{
  BOOL func_entry = WN_operator (call_wn) == OPR_FUNC_ENTRY;
  PLOC ploc;
  INT parm_count;
  if (func_entry) {
    parm_count = WN_num_formals(call_wn);
    ploc = Setup_Input_Parameter_Locations (call_ty);
  } else {
    parm_count = WN_num_actuals(call_wn);
    ploc = Setup_Output_Parameter_Locations (call_ty);
  }

  for (INT i = 0; i < parm_count; i++) {
    TY_IDX parm_ty = TY_Of_Parameter(WN_actual(call_wn,i));
    if (func_entry) {
      ploc = Get_Input_Parameter_Location (parm_ty);
      ploc = First_Input_PLOC_Reg (ploc, parm_ty);
    } else {
      ploc = Get_Output_Parameter_Location (parm_ty);
      ploc = First_Output_PLOC_Reg (ploc, parm_ty);
    }
    while (PLOC_is_nonempty(ploc)) {
    	if (PLOC_on_stack(ploc)) break;	// no more register parameters.
	Add_PREG_To_REGSET (PLOC_reg(ploc), parms);
        ploc = func_entry ? Next_Input_PLOC_Reg (ploc)
                          : Next_Output_PLOC_Reg (ploc);
    }
  }
}

static void
Compute_Return_Regs (TY_IDX call_ty, REGSET return_regs)
{

  PREG_NUM retpreg[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
  TYPE_ID retmtype[MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
  INT i;

  if (WHIRL_Return_Info_On) {

    RETURN_INFO return_info = Get_Return_Info (TY_ret_type(call_ty),
					       No_Simulated);
    FmtAssert (RETURN_INFO_count(return_info) <= MAX_NUMBER_OF_REGISTERS_FOR_RETURN, 
	("Compute_Return_Regs:  more return registers than can handle"));

    for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	retmtype[i] = RETURN_INFO_mtype (return_info, i);
	retpreg[i] = RETURN_INFO_preg (return_info, i);
	Add_PREG_To_REGSET (retpreg[i], return_regs);
    }
  }

  else {
    Get_Return_Mtypes (TY_ret_type(call_ty),
		       No_Simulated, &retmtype[0], &retmtype[1]);
    Get_Return_Pregs (retmtype[0], retmtype[1], &retpreg[0], &retpreg[1]);
    Add_PREG_To_REGSET (retpreg[0], return_regs);
    Add_PREG_To_REGSET (retpreg[1], return_regs);
  }
}


static void Compute_PU_Regs (REGSET livein, REGSET liveout)
{
  ST *pu_st = Get_Current_PU_ST ();
  ISA_REGCLASS rc;

  // Find the livein registers for the current procedure.
  if (livein != NULL) {
    // Add all the formal parameters to the livein set.
    Compute_Parameter_Regs (ST_pu_type(pu_st), 
	PU_Info_tree_ptr(Current_PU_Info), livein);
    // add sp, gp, ep, ra to the livein set.
    livein[REGISTER_CLASS_sp] = 
	REGISTER_SET_Union1 (livein[REGISTER_CLASS_sp], REGISTER_sp);
    livein[REGISTER_CLASS_gp] = 
	REGISTER_SET_Union1 (livein[REGISTER_CLASS_gp], REGISTER_gp);
    livein[REGISTER_CLASS_ep] = 
	REGISTER_SET_Union1 (livein[REGISTER_CLASS_ep], REGISTER_ep);
    livein[REGISTER_CLASS_ra] = 
	REGISTER_SET_Union1 (livein[REGISTER_CLASS_ra], REGISTER_ra);
    // add all the callee-save registers to the livein set.
    FOR_ALL_ISA_REGCLASS(rc) {
      if (TI_ISA_Regclass_Is_State(rc)) {
	// make TIE states live-in on entry
        livein[rc] = REGISTER_SET_Union (livein[rc], 
					REGISTER_CLASS_universe(rc));
      } else
        livein[rc] = REGISTER_SET_Union (livein[rc], 
					REGISTER_CLASS_callee_saves(rc));
    }
    // If current procedure is a nested function, add the static-link 
    // register to the livein set for the procedure.
    if (PU_is_nested_func(Pu_Table[ST_pu(pu_st)])) {
      livein[REGISTER_CLASS_static_link] = 
		  REGISTER_SET_Union1 (livein[REGISTER_CLASS_static_link], 
				       REGISTER_static_link);
    }
  }

  // Find the return registers for the current procedure.
  if (liveout != NULL) {
    Compute_Return_Regs (ST_pu_type(pu_st), liveout);

    // check return regs for each entry
    if (PU_has_altentry(Get_Current_PU())) {
	BB_LIST *bbl;
	BB *bb;
	ANNOTATION *ant;
	for (bbl = Entry_BB_Head; bbl; bbl = BB_LIST_rest(bbl)) {
		bb = BB_LIST_first(bbl);
		ant = ANNOT_Get (BB_annotations(bb), ANNOT_ENTRYINFO);
		pu_st = ENTRYINFO_name(ANNOT_entryinfo(ant));
	  Compute_Return_Regs (ST_pu_type(pu_st), liveout);
	}
    }

    // add all the callee-save registers to the liveout set.
    FOR_ALL_ISA_REGCLASS(rc) {
      if (TI_ISA_Regclass_Is_State(rc)) {
	// make TIE states live-out on exit
        liveout[rc] = REGISTER_SET_Union (liveout[rc], 
					REGISTER_CLASS_universe(rc));
      } else
        liveout[rc] = REGISTER_SET_Union (liveout[rc], 
					REGISTER_CLASS_callee_saves(rc));
    }

    // add sp to list of liveout registers.
    liveout[REGISTER_CLASS_sp] = 
	REGISTER_SET_Union1 (liveout[REGISTER_CLASS_sp], REGISTER_sp);
  }
}

//
// The livein, liveout and kill sets are computed for calls as follows:
//
//	livein:	parameter registers for the call
//		gp, sp
//		t9 (if pic-call)
//		static-link (if call to nested function)
//		ra, callee-saves (for tail calls)
//
//     liveout:	return registers for call
//
//	  kill:	all caller-save registers
//
static void 
Compute_Call_Regs (BB *bb, REGSET livein, REGSET liveout, REGSET kill)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_CALLINFO);
  WN *call_wn = CALLINFO_call_wn(ANNOT_callinfo(ant));
  INT num_parms = WN_num_actuals (call_wn);
  OPERATOR opr = WN_operator(call_wn);
  ST *call_st = (opr != OPR_ICALL) ? WN_st(call_wn) : NULL;
  TY_IDX call_ty = (call_st != NULL) ? ST_pu_type(call_st) : WN_ty(call_wn);
  ISA_REGCLASS cl;

  if (livein != NULL) {
    // add the parameter registers to the livein set.
    Compute_Parameter_Regs (call_ty, call_wn, livein);

    // If calling a nested function, add the static-link register to the 
    // livein set for the call.
    if (call_st != NULL && PU_is_nested_func(Pu_Table[ST_pu(call_st)])) {
      livein[REGISTER_CLASS_static_link] = 
		  REGISTER_SET_Union1 (livein[REGISTER_CLASS_static_link], 
				       REGISTER_static_link);
    }

    // add sp, gp to the livein set.
    livein[REGISTER_CLASS_gp] = 
		REGISTER_SET_Union1 (livein[REGISTER_CLASS_gp], REGISTER_gp);
    livein[REGISTER_CLASS_sp] = 
		REGISTER_SET_Union1 (livein[REGISTER_CLASS_sp], REGISTER_sp);

    // add t9 if PIC call.
    if (opr != OPR_CALL && Gen_PIC_Calls) {
      livein[REGISTER_CLASS_ep] = 
		  REGISTER_SET_Union1 (livein[REGISTER_CLASS_ep], REGISTER_ep);
    }

    // add ra and callee saves if tail call.
    if (BB_tail_call(bb)) {
      livein[REGISTER_CLASS_ra] = 
		  REGISTER_SET_Union1 (livein[REGISTER_CLASS_ra], REGISTER_ra);

      FOR_ALL_ISA_REGCLASS(cl) {
	REGISTER_SET callee_saves = REGISTER_CLASS_callee_saves(cl);
	livein[cl] = REGISTER_SET_Union(livein[cl], callee_saves);
      }
    }
  }

  if (liveout != NULL) {
    Compute_Return_Regs (call_ty, liveout);
  }

  if (kill != NULL) {
    // make the kill set the same as the set of caller-save registers.
    ISA_REGCLASS rc;
    FOR_ALL_ISA_REGCLASS(rc) {
      kill[rc] = REGISTER_SET_Union (kill[rc], REGISTER_CLASS_caller_saves(rc));
    }
  }
}


static void 
Compute_Asm_Regs (BB *bb, REGSET livein, REGSET liveout, REGSET kill)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ASMINFO);
  Is_True(ant, ("ASMINFO annotation info not present"));
  ASMINFO *info = ANNOT_asminfo(ant);
  ISA_REGCLASS rc;

  FOR_ALL_ISA_REGCLASS(rc) {
    if (livein)  livein[rc] = ASMINFO_livein(info)[rc];
    if (liveout) liveout[rc] = ASMINFO_liveout(info)[rc];
    if (kill)    kill[rc] = ASMINFO_kill(info)[rc];
  }
}


//
// The rotating register livein and kill sets cannot be determined
// by analyzing the OPs. The information need to come from SWP while
// it generates the schedule (and allocates the registers). This routine
// retreives this info previously stored by SWP as a BB annotation.
//
static void 
Compute_Rotating_Regs (BB *bb, REGSET livein, REGSET liveout, REGSET kill)
{
  ANNOTATION *ant = ANNOT_Get (BB_annotations(bb), ANNOT_ROTATING_KERNEL);
  Is_True(ant, ("Rotating kernel annotation info not present"));
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(ant);

  ISA_REGCLASS rc;
  FOR_ALL_ISA_REGCLASS(rc) {
    livein[rc] = ROTATING_KERNEL_INFO_live_in(info)[rc];
    kill[rc] = ROTATING_KERNEL_INFO_kill(info)[rc];
  }
}

static void
Update_REGSETs_For_Tail_Call (BB *bb, REGSET livein, REGSET kill)
{
  REGISTER_SET call_in[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (call_in);
  Compute_Call_Regs (bb, call_in, NULL, NULL);
  REGSET_UPDATE_LIVEIN (livein, call_in, kill);
}

static void
Update_REGSETs_For_Call (BB *bb, REGSET livein, REGSET kill)
{
  REGISTER_SET call_in[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET call_kill[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (call_in);
  REGSET_CLEAR (call_kill);
  Compute_Call_Regs (bb, call_in, NULL, call_kill);
  REGSET_UPDATE_LIVEIN (livein, call_in, kill);
  REGSET_OR (kill, call_kill);
}

static void
Update_REGSETs_For_Asm (BB *bb, REGSET livein, REGSET kill)
{
  REGISTER_SET asm_in[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET asm_kill[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (asm_in);
  REGSET_CLEAR (asm_kill);
  Compute_Asm_Regs (bb, asm_in, NULL, asm_kill);
  REGSET_UPDATE_LIVEIN (livein, asm_in, kill);
  REGSET_OR (kill, asm_kill);
}

static void
Update_REGSETs_For_Exit (REGSET livein, REGSET kill)
{
  REGISTER_SET pu_liveout[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (pu_liveout);
  // add the return registers to the livein set.
  Compute_PU_Regs (NULL, pu_liveout);
  REGSET_UPDATE_LIVEIN (livein, pu_liveout, kill);
}

static void
Update_REGSETs_For_Rotating_Kernel (BB *bb, REGSET livein, REGSET kill)
{
  REGISTER_SET kernel_in[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET kernel_kill[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (kernel_in);
  REGSET_CLEAR (kernel_kill);
  Compute_Rotating_Regs (bb, kernel_in, NULL, kernel_kill);
  REGSET_OR (livein, kernel_in);
  REGSET_OR (kill, kernel_kill);
}

static void
Reset_RegLiveValidForBB(INT from, INT end)
{
  INT i;
  for (i=from; i < end; i++) {
    RegLiveValidForBB[i] = false;
  }
}

static void
REGSET_grow(INT32 new_regset_ix)
{
  FmtAssert(pool_initialized, ("Reg_Live_Pool must be initialized before"));

  if (new_regset_ix >= regset_max_size) {
    INT32 old_size = regset_max_size;
    
    /* Allocate at least 10 more entries than what are expected */
    INT32 new_size = new_regset_ix + 1; 
    new_size += MAX(10, (INT32)(0.1*new_size));

    /* No need to grow Regsiter_Kill set, only grow Register_Livein */
    Register_Livein = TYPE_MEM_POOL_REALLOC_N(REGISTER_SET, &Reg_Live_Pool,
                                              Register_Livein,
                                              old_size * TI_ISA_Num_Regclasses(),
                                              new_size * TI_ISA_Num_Regclasses());

    /* need to grow RegLiveValidForBB as well */
    RegLiveValidForBB = TYPE_MEM_POOL_REALLOC_N(bool, &Reg_Live_Pool,
                                                RegLiveValidForBB,
                                                old_size, new_size);                      
    Reset_RegLiveValidForBB(old_size, new_size);

    regset_max_size = new_size;
  }
}


/*
   Update livein REGSET for a new empty BB 
 */
void Update_REGSETs_For_NewBB (BB *bb)
{
  REGSET_grow(BB_id(bb));

  REGSET livein = BB_Register_Livein(bb); 
  REGSET_CLEAR (livein);
  BBLIST *bl;
  FOR_ALL_BB_SUCCS (bb, bl) {
    BB *succ_bb = BBLIST_item(bl);
    REGSET succ_livein = BB_Register_Livein(succ_bb);
    REGSET_OR (livein, succ_livein);
  }

  RegLiveValidForBB[BB_id(bb)] = true;
}

void REG_LIVE_Analyze_Region(void)
{
  BB *bb;
  OP *op;
  BOOL changes;
  REGSET livein;
  REGSET kill;
  BB *last_bb = NULL;

  Is_True(Register_Livein == FALSE,
	  ("REG_LIVE_Analyze_Region called while facility is already in use"));

  if (!pool_initialized) {
    MEM_POOL_Initialize(&Reg_Live_Pool, "CG_Reg_Live_Pool", TRUE);
    MEM_POOL_Push(&Reg_Live_Pool);
    pool_initialized = TRUE;
  }

  Trace_Register_Liveness = Get_Trace (TP_GCM, 0x04);

  /* Allocate 10 more entries */
  regset_max_size = PU_BB_Count + 1 + 10;

  Register_Livein = 
    TYPE_MEM_POOL_ALLOC_N(REGISTER_SET, &Reg_Live_Pool,
			  regset_max_size*TI_ISA_Num_Regclasses());
  Register_Kill =
    TYPE_MEM_POOL_ALLOC_N(REGISTER_SET, &Reg_Live_Pool,
			  regset_max_size*TI_ISA_Num_Regclasses());

  RegLiveValidForBB = 
    TYPE_MEM_POOL_ALLOC_N(bool, &Reg_Live_Pool, regset_max_size);
  Reset_RegLiveValidForBB(0, regset_max_size);

  // Compute local reg-livein and reg-kill sets.
  for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
    livein = BB_Register_Livein(bb);
    kill = BB_Register_Kill(bb);

    RegLiveValidForBB[BB_id(bb)] = true;

#ifdef HAS_ROTATING_REGISTERS

    if (BB_rotating_kernel(bb)) {
      Update_REGSETs_For_Rotating_Kernel (bb, livein, kill);
    } else {

#endif
      bool scheduled=false;
      if (BB_scheduled(bb))
	scheduled=true;

      OP* scan = BB_first_op(bb);
      while (scan) {
	OP* end_group = scan;
	if (scheduled) {
	  while (!OP_end_group(end_group) && OP_code(end_group)!=TOP_loop_end)
	    end_group=OP_next(end_group);
	}

	op = scan;
	while (1) {
	  for (INT i = 0; i < OP_opnds(op); i++) {
	    TN *opnd_tn = OP_opnd(op,i);
	    if (TN_is_register(opnd_tn)) {
	      ISA_REGCLASS cl = TN_register_class(opnd_tn);
	      REGISTER reg = TN_register(opnd_tn);
              if ((reg != REGISTER_UNDEFINED) &&
		(!REGISTER_SET_MemberP (kill[cl], reg))) {
	        livein[cl] = REGISTER_SET_Union1 (livein[cl], reg);
	      }
	    }
	  }
	  if (op==end_group)
	    break;
	  op = OP_next(op);
	}

	op = scan;
	while (1) {
	  // Assume that conditional ops don't kill their definitions.
	  if (!OP_cond_def(op)) {
	    for (INT i = 0; i < OP_results(op); i++) {
	      TN *result_tn = OP_result(op,i);
 	      if (TN_register(result_tn) != REGISTER_UNDEFINED) {
	        ISA_REGCLASS cl = TN_register_class(result_tn);
	        kill[cl] =
		  REGISTER_SET_Union1 (kill[cl], TN_register(result_tn));
	      }
	    }
	  }
	  if (op==end_group)
	    break;
	  op = OP_next(op);
	}
	scan = OP_next(end_group);
      }
#ifdef HAS_ROTATING_REGISTERS

    }

#endif

    if (BB_tail_call(bb)) {
      Update_REGSETs_For_Tail_Call (bb, livein, kill);
    }
    else if (BB_call(bb)) {
      Update_REGSETs_For_Call (bb, livein, kill);
    }
    else if (BB_asm(bb)) {
      Update_REGSETs_For_Asm (bb, livein, kill);
    }
    else if (BB_exit(bb)) {
      Update_REGSETs_For_Exit (livein, kill);
    }
    last_bb = bb;
  }

  // Compute global reg-livein sets for each basic block.
  REGISTER_SET tmp[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET liveout[TI_ISA_REGCLASS_MAX+1];
  do {
    changes = FALSE;
    for (bb = last_bb; bb != NULL; bb = BB_prev(bb)) {
      BBLIST *bl;
      livein = BB_Register_Livein(bb);
      kill = BB_Register_Kill(bb);
      REGSET_ASSIGN (tmp, livein);
      REGSET_CLEAR (liveout);
      FOR_ALL_BB_SUCCS (bb, bl) {
	BB *succ_bb = BBLIST_item(bl);
	REGSET succ_livein = BB_Register_Livein(succ_bb);
	REGSET_OR (liveout, succ_livein);
      }
      REGSET_UPDATE_LIVEIN (livein, liveout, kill);
      if (!REGSET_EQUALS (tmp, livein)) {
	changes = TRUE;
      }
    }
  } while (changes);

  if (Trace_Register_Liveness) {
#pragma mips_frequency_hint NEVER
    for (bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      fprintf (TFile, "BB:%-3d livein: ", BB_id(bb));
      REGSET_Print (BB_Register_Livein (bb));
      fprintf (TFile, "\n         kill: ");
      REGSET_Print (BB_Register_Kill (bb));
      fprintf (TFile, "\n");
    }
  }
}

/* =======================================================================
 *
 *  REG_LIVE_Prolog_Temps
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
REG_LIVE_Prolog_Temps(
  BB *bb,
  OP *first,
  OP *last,
  REGISTER_SET *temps)
{
  REGISTER_SET live[TI_ISA_REGCLASS_MAX+1];
  ISA_REGCLASS cl;
  OP *op;
  TN *tn;

  /* First compute the registers that are live at the end of the
   * SP adjust sequence. Start with an empty set.
   */
  FOR_ALL_ISA_REGCLASS(cl) {
    live[cl] = REGISTER_SET_EMPTY_SET;
  }

  /* We'll compute the live regs by starting at the end of the BB
   * and working our way forward. Add in the registers that are live
   * out of the block.
   */
  if (! CG_localize_tns ) {
    for (tn = GTN_SET_Choose(BB_live_out(bb));
	 tn != GTN_SET_CHOOSE_FAILURE;
	 tn = GTN_SET_Choose_Next(BB_live_out(bb), tn)
    ) {
      if (TN_register(tn) != REGISTER_UNDEFINED) {
	cl = TN_register_class(tn);
	live[cl] = REGISTER_SET_Union1(live[cl], TN_register(tn));
      }
    }
  }

  /* Now scan the OPs backwards and update the liveness at each OP.
   * When we get to the end of the SP adjust sequence, we'll have
   * what's live at that point.
   */
  for (op = BB_last_op(bb); op != last; op = OP_prev(op)) {
    INT k;

    for (k = 0; k < OP_results(op); k++) {
      tn = OP_result(op,k);
      if (TN_register(tn) != REGISTER_UNDEFINED) {
      cl = TN_register_class(tn);
      live[cl] = REGISTER_SET_Difference1(live[cl], TN_register(tn));
    }
    }

    for (k = 0; k < OP_opnds(op); k++) {
      tn = OP_opnd(op,k);
      if ((TN_is_register(tn)) && (TN_register(tn) != REGISTER_UNDEFINED)) {
	cl = TN_register_class(tn);
	live[cl] = REGISTER_SET_Union1(live[cl], TN_register(tn));
      }
    }

    /* We need to be very conservative when we encounter a call, because
     * at this late stage, LRA has removed NOOP copies of the argument
     * registers. Therefore we may not know that particular arg register
     * is live or not. To be safe, just say they are all live.
     */
    if (OP_call(op)) {
      FOR_ALL_ISA_REGCLASS(cl) {
	live[cl] = REGISTER_SET_Union(live[cl], REGISTER_CLASS_function_argument(cl));
      }
    }
  }

  /* The first approximation to the available temps is the caller
   * saved registers with the live registers, determined above, removed.
   */
  FOR_ALL_ISA_REGCLASS(cl) {
    temps[cl] = REGISTER_SET_Difference(REGISTER_CLASS_caller_saves(cl),
					live[cl]);
  }

  /* The last step is to reject any temp which is used or killed by
   * the OPs in the range the new temp must be live though.
   */
  for (op = first; op != last; op = OP_next(op)) {
    INT k;

    for (k = 0; k < OP_results(op); k++) {
      tn = OP_result(op,k);
      if (TN_register(tn) != REGISTER_UNDEFINED) {
      cl = TN_register_class(tn);
      temps[cl] = REGISTER_SET_Difference1(temps[cl], TN_register(tn));
    }
    }

    for (k = 0; k < OP_opnds(op); k++) {
      tn = OP_opnd(op,k);
      if ((TN_is_register(tn)) && (TN_register(tn) != REGISTER_UNDEFINED)) {
	cl = TN_register_class(tn);
	temps[cl] = REGISTER_SET_Difference1(temps[cl], TN_register(tn));
      }
    }
  }
}


/* =======================================================================
 *
 *  REG_LIVE_Epilog_Temps
 *
 *  See interface description.
 *
 * =======================================================================
 */
void
REG_LIVE_Epilog_Temps(
  ST *pu_st,
  BB *bb,
  OP *adj_op,
  REGISTER_SET *temps)
{
  ISA_REGCLASS cl;
  OP *op;
  TN *tn;

  /* Get the return registers for the exit block.  */
  REGSET_CLEAR(temps);
  Compute_Return_Regs (ST_pu_type(pu_st), temps);

  /* The set of available temps at the end of the exit block is
   * the caller saved regs with the return regs removed.
   */
  FOR_ALL_ISA_REGCLASS(cl) {
    temps[cl] = REGISTER_SET_Difference(REGISTER_CLASS_caller_saves(cl),
					temps[cl]);
  }

  /* Scan the OPs in the exit block backwards and update the available
   * temps at each OP, stopping after updating for the adjustment
   * OP itself.
   */
  for (op = BB_last_op(bb); op != OP_prev(adj_op); op = OP_prev(op)) {
    INT k;

    for (k = 0; k < OP_results(op); k++) {
      tn = OP_result(op,k);
      if (TN_register(tn) != REGISTER_UNDEFINED) {
      cl = TN_register_class(tn);
      temps[cl] = REGISTER_SET_Union1(temps[cl], TN_register(tn));
    }
    }

    for (k = 0; k < OP_opnds(op); k++) {
      tn = OP_opnd(op,k);
      if ((TN_is_register(tn)) && (TN_register(tn) != REGISTER_UNDEFINED)) {
	cl = TN_register_class(tn);
	temps[cl] = REGISTER_SET_Difference1(temps[cl], TN_register(tn));
      }
    }
  }
}

// Returns true if there is an implicit use of <cl,reg> out of <bb>.
// The implicit uses are for function call parameters and return registers.
BOOL REG_LIVE_Implicit_Use_Outof_BB (ISA_REGCLASS cl, REGISTER reg, BB *bb)
{
  // Always mark unallocatable registers as liveout. This includes
  // registers like sp, fp, gp and dedicated register variables.
  if (!REGISTER_allocatable (cl, reg)) return TRUE;

  REGISTER_SET use[TI_ISA_REGCLASS_MAX+1];

  REGSET_CLEAR(use);
  if (BB_tail_call(bb) || BB_call(bb)) {
    Compute_Call_Regs (bb, use, NULL, NULL);
  }
  else if (BB_asm(bb)) {
    Compute_Asm_Regs (bb, use, NULL, NULL);
  }
  else if (BB_exit(bb)) {
    Compute_PU_Regs (NULL, use);
  }
  return REGISTER_SET_MemberP (use[cl], reg);
}

// Returns TRUE if there is an implicit definition of <cl,reg> that
// reaches the top of <bb>. The implicit definitions are either for
// procedure entry point or for the return registers for a call.
BOOL REG_LIVE_Implicit_Def_Into_BB (ISA_REGCLASS cl, REGISTER reg, BB *bb)
{
  // Always mark unallocatable registers as an implicit def into all bbs. 
  // This includes registers like sp, fp, gp and dedicated register variables.
  if (!REGISTER_allocatable (cl, reg)) return TRUE;

  REGISTER_SET def[TI_ISA_REGCLASS_MAX+1];
  REGSET_CLEAR (def);
  INT32 num_preds = BB_preds_len(bb);

  if (num_preds == 0) {
    Compute_PU_Regs (def, NULL);
  }
  else if (num_preds == 1) {
    BB *prev_bb = BB_Unique_Predecessor (bb);
    if (BB_call(prev_bb)) {
      Compute_Call_Regs (prev_bb, NULL, def, NULL);
    }
    else if (BB_asm(prev_bb)) {
      Compute_Asm_Regs (prev_bb, NULL, def, NULL);
    }
  }
  return REGISTER_SET_MemberP (def[cl], reg);
}



// Returns TRUE if the register (<cl>,<reg>) is live on entry to <bb>.
BOOL REG_LIVE_Into_BB(ISA_REGCLASS cl, REGISTER reg, BB *bb)
{
  // If we have not computed register liveness information, assume the 
  // worst case and return TRUE.
  if (Register_Livein == NULL) return TRUE;

  // Be conservative if we have no info for a newly created block.
  Is_True(RegLiveValidForBB[BB_id(bb)],
          ("New BB has not been updated for Register live-in"));
  if (!RegLiveValidForBB[BB_id(bb)]) return TRUE;

  REGSET livein = BB_Register_Livein (bb);
  return REGISTER_SET_MemberP (livein[cl], reg);
}


// Returns TRUE if the register (<cl>,<reg>) is live on exit from <bb>.
BOOL REG_LIVE_Outof_BB (ISA_REGCLASS cl, REGISTER reg, BB *bb)
{
  BBLIST *succs;

  // If we have not computed register liveness information, assume the 
  // worst case and return TRUE.
  if (Register_Livein == NULL) return TRUE;

  // Be conservative if we have no info for a newly created block.
  Is_True(RegLiveValidForBB[BB_id(bb)],
          ("New BB has not been updated for Register live-in"));
  if (!RegLiveValidForBB[BB_id(bb)]) return TRUE;

  FOR_ALL_BB_SUCCS(bb, succs) {
    REGSET livein = BB_Register_Livein (BBLIST_item(succs));
    if (REGISTER_SET_MemberP (livein[cl], reg))
        return TRUE;
  }

  return REG_LIVE_Implicit_Use_Outof_BB (cl, reg, bb);
}


// Adds (<cl>,<reg>) into live-in sets for <bb>.
void REG_LIVE_Update(ISA_REGCLASS cl, REGISTER reg, BB *bb)
{
  
  REGSET_grow(BB_id(bb));

  REGSET livein = BB_Register_Livein (bb);
  if (reg != REGISTER_UNDEFINED)
    livein[cl] = REGISTER_SET_Union1 (livein[cl], reg);
}

// The client is finished using the facility -- clean up.
void REG_LIVE_Finish(void)
{
  MEM_POOL_Pop(&Reg_Live_Pool);
  MEM_POOL_Delete(&Reg_Live_Pool);
  pool_initialized = FALSE;
  Register_Livein = NULL;
  Register_Kill = NULL;
  RegLiveValidForBB = NULL;
}
