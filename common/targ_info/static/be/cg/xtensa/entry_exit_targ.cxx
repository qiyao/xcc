
/* 
   Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.
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
#include "tn.h"
#include "op.h"
#include "bb.h"
#include "wn.h"
#include "symtab.h"
#include "errors.h"
#include "targ_sim.h"
#include "ttype.h"
#include "register.h"
#include "calls.h"
#include "entry_exit_targ.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "cgtarget.h"
#include "iselector.h"
#include "data_layout.h"
#include "cgtarget.h"
#include "stblock.h"

void
EETARG_Save_Pfs (TN *saved_pfs, OPS *ops)
{
}
 
void
EETARG_Restore_Pfs (TN *saved_pfs, OPS *ops)
{
}

#define ADDMI_INCR 256
#define ADDI_MAX 127
#define ADDI_MIN -128


void
EETARG_Adjust_SP_For_Entry( TN *incr, REGISTER reg, OPS *ops )
{
  if (Target_ABI == ABI_WINDOWED) {
    /* If 'ops' is empty, then we should just be able to use 'incr' in
       the entry instruction. Otherwise 'ops' contains the sequence of
       instructions that compute the frame size -16 into 'incr'. */
    
    if (OPS_length(ops) == 0)
      {
	FmtAssert(CGTARG_Can_Fit_Immediate_In_Entry_Instruction(TN_value(incr)),
		  ("entry immediate %d out-of-range", TN_value(incr)));
	
	TN *tmp = Gen_Literal_TN( ((INT32)TN_value(incr)), 4 );
	Build_OP( TOP_entry, SP_TN, SP_TN, tmp, ops );
      }
    else
      {
	OPS newops = OPS_EMPTY;
	Build_OP(TOP_entry, SP_TN, SP_TN, Gen_Literal_TN(16, 4), &newops);
	OPS_Append_Ops(&newops, ops);
	Build_OP(TOP_sub, incr, SP_TN, incr, &newops);
	Build_OP(TOP_movsp, SP_TN, incr, &newops);
	*ops = newops;
      }
  }
  else { /* ABI_CALL0 */
    int op_len = OPS_length(ops);
    TN * old_sp;
    Is_True(Target_ABI==ABI_CALL0, ("Illegal ABI"));
    if ( op_len == 0) {
      INT32 incr_val = -TN_value(incr);
      FmtAssert(CGTARG_Can_Fit_Immediate_In_Entry_Instruction(incr_val),
		("entry immediate %d out-of-range", incr_val));
      TN * incr_tn = Gen_Literal_TN(incr_val, MTYPE_I4);
      
      TOP top;
      if ((-incr_val) % ADDMI_INCR == 0)
	top = TOP_addmi;
      else
	top = TOP_addi;

      Build_OP( top, SP_TN, SP_TN, incr_tn, ops );
    }
    else 
      Build_OP( TOP_sub, SP_TN, SP_TN, incr, ops );
  }
}


void
EETARG_Adjust_SP_For_Exit( TN *incr, REGISTER reg, OPS *ops )
{
  if (Target_ABI == ABI_WINDOWED) {
    TN* a0 = Build_Dedicated_TN(TI_ISA_Regclass_Integer(), REGISTER_MIN+0, 4);
    Build_OP( TOP_retw, a0, ops );
  }
  else { /* ABI_CALL0 */
    Is_True(Target_ABI == ABI_CALL0, ("Illegal ABI"));

    UINT32 incr_val = TN_value(incr);
    if ( OPS_length(ops) == 0) {
#if 0
      // when we allow save tns to split live ranges again,
      // we will need to put this back.
      if (Gen_Frame_Pointer) {
	Exp_COPY(SP_TN, FP_TN, ops);
      }      
#endif
      FmtAssert(incr_val >= 0, ("Negative stack increment on exit"));
      if (!CGTARG_Can_Fit_Immediate_In_Exit_Instruction(incr_val)) {
	TN * source_tn = Build_Dedicated_TN(REGISTER_CLASS_sp, reg, MTYPE_U4);
	Generate_Constant(MTYPE_U4, source_tn, incr_val, ops);
	Build_OP (TOP_add, SP_TN, SP_TN, source_tn, ops);
      }
      else {
	TOP top;
	if ((-incr_val) % ADDMI_INCR == 0)
	  top = TOP_addmi;
	else
	  top = TOP_addi;
	TN * lit = Gen_Literal_TN(incr_val, MTYPE_I4);
      	Build_OP( top, SP_TN, SP_TN, lit, ops );
      }
    }
    else { 
#if 0
      // when we allow save tns to split live ranges again,
      // we will need to put this back.
      if (Gen_Frame_Pointer) {
	Exp_COPY(SP_TN, FP_TN, ops);
      }      
#endif
      Build_OP( TOP_add, SP_TN, SP_TN, incr, ops );
    }
  }
}

void
EETARG_Fixup_Entry_Code (BB *bb)
{
}

void
EETARG_Init_Entry_Exit_Code (WN *pu_wn, BOOL need_frame_pointer)
{
}

void EETARG_Save_Extra_Callee_Tns (OPS *ops)
{
}

void EETARG_Restore_Extra_Callee_Tns (OPS *ops)
{
}


OP *
EETARG_Build_Jump_Instead_Of_Call (OP *call_op)
{
  return NULL;
}


