
/* 
   Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.
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


/* CGEXP routines for expanding branches */

#include "defs.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_options.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "cgexp.h"
#include "cgexp_internals.h"

#include "iselector.h"

void
Initialize_Branch_Variants(void)
{
	// nothing to do
}

// Check that compare is of proper form,
// and return TOP to use for the compare.
// May modify the variant and src tns.
TOP
Pick_Compare_TOP (INT16 *variant, TN **src1, TN **src2, OPS *ops)
{
  return TOP_UNDEFINED;
}

void
Expand_Branch ( TN *targ, TN *src1, TN *src2, INT16 variant, OPS *ops)
{
  INT32 cond = variant & V_BR_MASK;
  OPERATOR br_opr;
  TYPE_ID br_type;

  switch (cond) {
  case V_BR_I8EQ:  case V_BR_I8NE:  case V_BR_I8GT:  case V_BR_I8GE:
  case V_BR_I8LT:  case V_BR_I8LE:  
  case V_BR_I4EQ:  case V_BR_I4NE:  case V_BR_I4GT:  case V_BR_I4GE:  
  case V_BR_I4LT:  case V_BR_I4LE:
    br_type = MTYPE_I4;
    break;
  case V_BR_U8EQ:  case V_BR_U8NE:  case V_BR_U8GT:  case V_BR_U8GE:
  case V_BR_U8LT:  case V_BR_U8LE:
  case V_BR_U4EQ:  case V_BR_U4NE:  case V_BR_U4GT:  case V_BR_U4GE:
  case V_BR_U4LT:  case V_BR_U4LE:
    br_type = MTYPE_U4;
    break;
  default:
    FmtAssert(FALSE, ("Not Yet Implemented"));
  }

  switch (cond) {
  case V_BR_I8EQ:  case V_BR_I4EQ:  case V_BR_U8EQ:  case V_BR_U4EQ:  
    br_opr = OPR_EQ;
    break;
  case V_BR_I8NE:  case V_BR_I4NE:  case V_BR_U8NE:  case V_BR_U4NE:  
    br_opr = OPR_NE;
    break;
  case V_BR_I8GT:  case V_BR_I4GT:  case V_BR_U8GT:  case V_BR_U4GT:  
    br_opr = OPR_GT;
    break;
  case V_BR_I8GE:  case V_BR_I4GE:  case V_BR_U8GE:  case V_BR_U4GE:
    br_opr = OPR_GE;
    break;
  case V_BR_I8LT:  case V_BR_I4LT:  case V_BR_U8LT:  case V_BR_U4LT:  
    br_opr = OPR_LT;
    break;
  case V_BR_I8LE:  case V_BR_I4LE:  case V_BR_U8LE:  case V_BR_U4LE:
    br_opr = OPR_LE;
    break;
  }

  BOOL success = FALSE;
  WN* tree = NULL;

  // special case when src2 is constant 0 to generate beqz and bnez
  // which helps some CG optimization
  if (TN_is_constant(src2) && TN_has_value(src2) &&
      TN_value(src2)==0) {

    if (br_opr==OPR_EQ)
      tree = WN_CreateFalsebr(TN_label(targ), WN_CreateTn(src1));
    else if (br_opr==OPR_NE)
      tree = WN_CreateTruebr(TN_label(targ), WN_CreateTn(src1));

  } else {

    WN *expr = WN_Relational(br_opr, br_type, WN_CreateTn(src1), 
			   WN_CreateTn(src2));
    tree = WN_CreateTruebr(TN_label(targ), expr);
  }
  success = ISEL_gen( tree, 0, ops );
  FmtAssert( success, ("ISEL_gen failed in Exp_Branch (0)"));
}

void Exp_Indirect_Branch (TN *targ_reg, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}

void Exp_Local_Jump(BB *bb, INT64 offset, OPS *ops)
{
  FmtAssert(FALSE, ("NYI: Exp_Local_Jump"));
}

void Exp_Return (TN *return_address, OPS *ops)
{
  FmtAssert(Target_ABI == ABI_CALL0, ("Not Yet Implemented"));
  TN* a0 = Build_Dedicated_TN(TI_ISA_Regclass_Integer(), REGISTER_MIN+0, 4);
  Build_OP( TOP_ret, a0, ops );
}

void Exp_Call (OPERATOR opr, TN *return_address, TN *target, OPS *ops)
{
  FmtAssert(FALSE, ("Not Yet Implemented"));
}
