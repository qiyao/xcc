
/* 
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: expand.c
 * $Revision: 1.107 $
 * $Date: 2000/05/08 21:50:15 $
 * $Author: mpm $
 * $Source: /osprey.src/osprey1.0/be/cg/ia64/RCS/expand.cxx,v $
 *
 * Description:
 *
 * This file contains the internals of code expansion. Its interface
 * is 'Exp_OP', which takes an OP, expands it into a list of OPs which
 * are appended to the oplist passed in.
 *
 * It handles all the macro expansions, special handling during 
 * expansion and all the nitty gritty stuff that goes along with it.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "config.h"
#include "erglob.h"
#include "ercg.h"
#include "glob.h"
#include "tracing.h"
#include "util.h"

#include "tn.h"
#include "cg_flags.h"
#include "bb.h"
#include "symtab.h"
#include "opcode.h"
#include "const.h"	/* needed to manipulate target/host consts */
#include "targ_const.h"	/* needed to manipulate target/host consts */
#include "op.h"
#include "data_layout.h"
#include "stblock.h"
#include "cgexp.h"
#include "cgexp_internals.h"
#include "w2op.h"
#include "label_util.h"
#include "cgtarget.h"
#include "whirl2ops.h"
#include "cg_tie.h"

#include "iselector.h"

extern TN *Move_To_Register (TYPE_ID rtype, TN *result, TN *val, OPS *ops); // in isel.pat

BOOL Reuse_Temp_TNs = FALSE;
BOOL Trace_Exp2 = FALSE;      /* extra cgexp trace*/

static TN * Create_low_16_TN(TN * lit);
static TN * Create_high_16_TN(TN * lit);

void
Expand_Immediate (TN *result, TN *src, BOOL is_signed, OPS *ops)
{
  FmtAssert( TN_is_constant(src), ("Expand_Immediate: illegal source tn"));

  WN *tree = WN_CreateTn(src);
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Immediate"));
}
extern TN *Generate_Constant (TYPE_ID rtype, TN *result, INT32 val, OPS *ops);

void
Expand_Const (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  FmtAssert( TN_is_symbol(src), ("Expand_Const: src not a symbol TN"));


  if (xt_const16 && xt_prefer_const16) {
    if (!MTYPE_is_float(TN_mtype(result))) {
      ST * st = TN_var(src);
      Generate_Constant(TN_mtype(result), result, 
		      TCON_uval(Tcon_Table[st->u1.tcon]), ops);
    } else {
      WN *tree = WN_CreateConst(OPR_CONST, TN_mtype(result), MTYPE_V, TN_var(src));
      BOOL success = ISEL_gen(tree, result, ops);
      FmtAssert(success, ("ISEL_gen failed in Expand_Add"));
    }
  }
  else
    Exp_Load(TN_mtype(result), TN_mtype(result), result, TN_var(src), 0, ops, 0);
}

void
Expand_Add (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Add(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Add"));
}

void
Expand_Sub (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Sub(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Sub"));
}


void
Expand_Neg (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Neg(mtype, WN_CreateTn(src));
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Neg"));
}


void
Expand_Abs (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Abs(mtype, WN_CreateTn(src));
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Abs"));
}


void
Expand_Shift (TN *result, TN *src1, TN *src2, TYPE_ID mtype, SHIFT_DIRECTION kind, OPS *ops)
{
  WN *tree;
  

  switch (kind)
  {
  case shift_left:
    tree = WN_Shl(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
    break;

  case shift_aright:
    tree = WN_Ashr(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
    break;

  case shift_lright:
    tree = WN_Lshr(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
    break;
  }
  
  BOOL success = ISEL_gen(tree, result, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Shift"));
}


void
Expand_Copy (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Convert_Length ( TN *dest, TN *src, TN *length_tn, TYPE_ID mtype, BOOL signed_extension, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Exp_Immediate (TN *dest, TN *src, BOOL is_signed, OPS *ops)
{
  Expand_Immediate(dest, src, is_signed, ops);
}

inline void
Expand_G_To_F (TN *ftn, TN *gtn, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

inline void
Expand_F_To_G (TN *gtn, TN *ftn, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


/* Generate wsr/rsr simulated instruction that writes or reads the
   boolean special register. */
void
Expand_Wsr_Br (TN *src, OPS *ops)
{
  TN *breg_state = Build_Dedicated_TN(REGISTER_CLASS_br, REGISTER_br, 4);

  ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();
  INT first_reg = TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(br_rc)) + REGISTER_MIN;
  TN *b[16];
  
  for (UINT i = 0; i < 16; i++)
    b[i] = Build_Dedicated_TN(br_rc, first_reg + i, 4);
  
  OPS_Append_Op(ops,
		Mk_OP(TOP_wsr_br,
		      breg_state,
		      b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
		      b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15],
		      src));
}

void
Expand_Rsr_Br (TN *dst, OPS *ops)
{
  TN *breg_state = Build_Dedicated_TN(REGISTER_CLASS_br, REGISTER_br, 4);

  ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();
  INT first_reg = TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(br_rc)) + REGISTER_MIN;
  TN *b[16];
  
  for (UINT i = 0; i < 16; i++)
    b[i] = Build_Dedicated_TN(br_rc, first_reg + i, 4);
  
  OPS_Append_Op(ops,
		Mk_OP(TOP_rsr_br,
		      dst,
		      breg_state,
		      b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
		      b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15]));
}


/*
 *
 * Helper routine for Expand_Small_Multiply
 *
 */
static void shladd(TN *r, TN *x1, INT s, TN *x2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


/*
 * Expand_Small_Multiply produces an optimized expansion of 
 * multiplication by any constant between -1 and 63. Multiplication is done for 64
 * bit quantities only. 
 *
 */
static void
Expand_Small_Multiply(TN *r,  // result
		      TN *x,  // source
		      INT16 val, // multiplicand
		      OPS * ops) // place to put the ops
{ FmtAssert(FALSE,("Unimplemented")); }



/* 
 * Expand the multiply into a series of shifts and adds,
 * unless the sequence is longer than "limit".
 */
static BOOL
Expand_Multiply_Into_Shifts (
  TN	   *result_tn,
  TN	   *var_tn,
  TARG_UINT constant,
  INT16	    limit,
  TYPE_ID   mtype,
  OPS 	*ops)
{ FmtAssert(FALSE,("Unimplemented")); 
 return FALSE; 
}

/*
 *  Try to expand a multiply into a sequence of less expensive operations.
 */
#define NUM_FAST_MPYS 8
static INT fast_mpys[NUM_FAST_MPYS] = {17,16,9,8,5,4,3,2};

static BOOL
Expand_Constant_Multiply (TN *result, TN *var_tn, TARG_INT constant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); 
 return FALSE; 
}

void
Expand_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

/* return high part of multiply result */
void
Expand_High_Multiply (TN *result, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Normalize_Logical (TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Logical_Not (TN *dest, TN *src, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Expand_Logical_And (TN *dest, TN *src1, TN *src2, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Logical_Or (TN *dest, TN *src1, TN *src2, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Expand_Binary_Complement (TN *dest, TN *src, TYPE_ID /* mtype */, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Expand_Binary_And (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Band(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
  BOOL success = ISEL_gen(tree, dest, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Binary_And"));
}

void
Expand_Binary_Or (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Bior(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
  BOOL success = ISEL_gen(tree, dest, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Binary_Or"));
}

void
Expand_Binary_Xor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Bxor(mtype, WN_CreateTn(src1), WN_CreateTn(src2));
  BOOL success = ISEL_gen(tree, dest, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Binary_Xor"));
}

void
Expand_Binary_Nor (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{
  WN *tree = WN_Bnot(mtype, WN_Bior(mtype, WN_CreateTn(src1), WN_CreateTn(src2)));
  BOOL success = ISEL_gen(tree, dest, ops);
  FmtAssert(success, ("ISEL_gen failed in Expand_Binary_Nor"));
}


static void
Expand_Int_Comparison (TOP action, TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Less (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Less_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Not_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Greater_Equal (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Int_Greater (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

static void
Expand_Bool_Comparison (BOOL equals, TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_Not_Equal (TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Bool_To_Int (TN *dest, TN *src, TYPE_ID rtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

typedef enum {
  ROUND_USER,
  ROUND_NEAREST,
  ROUND_CHOP,
  ROUND_NEG_INF,
  ROUND_PLUS_INF
} ROUND_MODE;

// TODO how do you trap on float val too big for [u]int32?
static void
Expand_Float_To_Int (ROUND_MODE rm, TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_To_Int_Cvt (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_USER, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Round (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_NEAREST, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Trunc (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_CHOP, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Floor (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_NEG_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Int_Ceil (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{
        Expand_Float_To_Int (ROUND_PLUS_INF, dest, src, imtype, fmtype, ops);
}

void
Expand_Float_To_Float (TN *dest, TN *src, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Expand_Int_To_Float (TN *dest, TN *src, TYPE_ID imtype, TYPE_ID fmtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static BOOL
Optimize_Select (
	TOP cmp,
  	TN *cond1, 
  	TN *cond2, 
  	TN *dest, 
  	TN *dest2,
  	TN *src1, 
  	TN *src2, 
	BOOL is_float,
	OPS *ops)
{

  return FALSE;
}


static void
Expand_Compare_And_Select (
	TOP cmp,
  	TN *cond1, 
  	TN *cond2, 
  	TN *dest, 
  	TN *opposite_dest, 
  	TN *true_tn, 
  	TN *false_tn, 
	BOOL is_float,
	OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Select (
  TN *dest_tn, 
  TN *cond_tn, 
  TN *true_tn, 
  TN *false_tn, 
  TYPE_ID mtype, 
  BOOL float_cond,
  OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Min (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Max (TN *dest, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_MinMax (TN *dest, TN *dest2, TN *src1, TN *src2, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

/* check whether to eval condition before select */
extern BOOL
Check_Select_Expansion (OPCODE compare)
{
  // in order to get optimal code,
  // don't evaluate the condition first,
  // but pass the condition and kids to exp_select,
  // which will do the compare and use the predicate results.
  return FALSE;
}

extern void 
Exp_Select_And_Condition (
        OPCODE select, TN *result, TN *true_tn, TN *false_tn,
        OPCODE compare, TN *cmp_kid1, TN *cmp_kid2, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }



static void
Expand_SGI_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(qp) frsqrta.s0 f6,p2=src	# y2 = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5 (0x3fe0000000000000)
   *	(p2) ldfd	f7=ah		# f7 = 0x3fe0000000000001
   *
   *	(p2) fmpy.d.s1	f3=src,f6	# g = x*y2
   *	(p2) fmpy.d.s1	f2=f4,f6	# y = 0.5*y2
   *
   *	(p2) fnma.d.s1	f5=f3,f3,src	# d = x - g*g
   *
   *	(p2) fma.d.s1	f3=f2,f5,f3	# g = g + y*d # 16 bit approximation
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 32 bit approximation
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s1   f3=f2,f5,f3     # g = g + y*d # 64 bit approximation before rounding
   *	(p2) fadd.d.s1  f6=f3,f3        # y2 = y + y
   *
   *	(p2) fnma.d.s1	f8=f2,f3,f7	# e = ah - y*g
   *	(p2) fnma.d.s1	f5=f3,f3,src    # d = x - g*g
   *	(p2) fma.d.s1	f2=f8,f6,f2	# y = y + e*y2
   *
   *	(p2) fma.d.s0   f6=f2,f5,f3	# result = g + y*d
   */
  // 3-mar-00/ken: this doesn't work for MTYPE_F10!!!!
}


#define RESET_COND_DEF_LAST(ops) Set_OP_cond_def_kind(OPS_last(ops),OP_ALWAYS_UNC_DEF)

static void
Expand_Intel_F10_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F8_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Min_Lat_F4_Sqrt(TN *result, TN *src, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


static void
Expand_Intel_Max_Thr_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Max_Thr_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Max_Thr_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Max_Thr_Sqrt"));
    /*NOTREACHED*/
  }
}


static void
Expand_Intel_Min_Lat_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  switch (mtype) {
  case MTYPE_F4:
    Expand_Intel_Min_Lat_F4_Sqrt(result, src, ops);
    break;
  case MTYPE_F8:
    Expand_Intel_Min_Lat_F8_Sqrt(result, src, ops);
    break;
  case MTYPE_F10:
    Expand_Intel_F10_Sqrt(result, src, ops);
    break;
  default:
    FmtAssert(FALSE, ("Bad type in Expand_Intel_Min_Lat_Sqrt"));
    /*NOTREACHED*/
  }
}


void
Expand_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  static BOOL initialized;
  static void (*exp_sqrt)(TN *, TN *, TYPE_ID, OPS *) = Expand_SGI_Sqrt;

  if (!initialized) {
    const char * const alg = CGEXP_sqrt_algorithm;
    if (strcasecmp(alg, "intel_max_thr") == 0) {
      exp_sqrt = Expand_Intel_Max_Thr_Sqrt;
    } else if (strcasecmp(alg, "intel_min_lat") == 0) {
      exp_sqrt = Expand_Intel_Min_Lat_Sqrt;
    } else if (strcasecmp(alg, "sgi") != 0) {
      DevWarn("invalid fdiv algorithm: %s", alg);
    }
    initialized = TRUE;
  }

  exp_sqrt(result, src, mtype, ops);
}


static void
Expand_Float_Compares(TOP cmp_opcode, TN *dest, TN *src1, TN *src2, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Less (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Greater (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Less_Equal (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Greater_Equal (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Equal (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Float_Not_Equal (TN *dest, TN *src1, TN *src2, INT16 variant, TYPE_ID mtype, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }

void
Expand_Recip_Sqrt (TN *result, TN *src, TYPE_ID mtype, OPS *ops)
{
  /*	(qp) frsqrta.s0 f2,p2=src	# y = ~1/sqrt(x)
   *
   *	(p2) ldfd	f4=half		# f4 = 0.5
   *	(p2) fmpy.d.s1	f5=f4,src	# hx = 0.5*x
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s1   f2=f2,f6,f2	# y = y + y*z
   *
   *	(p2) fmpy.d.s1	f3=f2,f2	# y2 = y*y
   *	(p2) fnma.d.s1	f6=f5,f3,f4	# z = 0.5 - 0.5*x*y*y
   *	(p2) fma.d.s0   result=f2,f6,f2	# result = y + y*z
   */

}

void
Expand_Flop (OPCODE opcode, TN *result, TN *src1, TN *src2, TN *src3, OPS *ops)
{ FmtAssert(FALSE,("Unimplemented")); }


/* Initialize the tracing flags for the expander. */
extern void
Init_CG_Expand (void)
{
  static BOOL Initialized = FALSE;

  // per PU:
  Trace_Exp = Get_Trace (TP_CGEXP, 1);
  /* whirl2ops uses -ttexp:2 */
  Trace_Exp2 = Get_Trace (TP_CGEXP, 4);

  if (Initialized) return;
  Initialized = TRUE;
  // once per file:
  Initialize_Branch_Variants();
}


/* ======================================================================
 * Exp_COPY
 * 
 * Generate a register transfer copy from 'src_tn' to 'tgt_tn'. 
 * ======================================================================*/
void 
Exp_COPY (TN *tgt_tn, TN *src_tn, OPS *ops)
{
  if( TN_is_constant(src_tn) )
  {
    FmtAssert (TN_has_value(src_tn), ("Exp_COPY: illegal source tn"));
    /* expansion for INTCONST doesn't depend on size */
    Exp_OP1 (OPC_I4INTCONST, tgt_tn, src_tn, ops);
  }
  else
  {
    ISA_REGCLASS tgt_rc = TN_register_class(tgt_tn);
    ISA_REGCLASS src_rc = TN_register_class(src_tn);

    if (tgt_rc == src_rc && tgt_rc == TI_ISA_Regclass_Integer()) {
      Build_OP( TOP_mov_n, tgt_tn, src_tn, ops );
      Set_OP_copy (OPS_last(ops));
    }
    else if (src_tn == tgt_tn)
    {
      /* We don't know how to do this copy, but since the source and
         target are the same, we can just return a nop (we must return
         some op). */
      Build_OP(TOP_noop, ops);
    }
    else if (MTYPE_is_tie(TN_mtype(tgt_tn)) ||
	     MTYPE_is_tie(TN_mtype(src_tn)))
    {
      TYPE_ID tgt_mtype = TN_mtype(tgt_tn);
      TYPE_ID src_mtype = TN_mtype(src_tn);

      if (tgt_mtype != src_mtype) {
        TIE_MACRO_p rtor_macro;

        if (tgt_rc == src_rc) {
          /* Use default move */
          TYPE_ID default_mtype = TI_ISA_Mtype_For_Regclass(tgt_rc);
          rtor_macro = tie_info->mtype_move_macro(default_mtype);         
        } else {
          /* Can this happen ? */
          rtor_macro = tie_info->mtype_rtor_macro(src_mtype,tgt_mtype);
        }
        if (rtor_macro == NULL) {
          FmtAssert(FALSE,("TN mtype mis-matches"));
        }

	TN* in_operands[2];
	in_operands[0]=tgt_tn;
	in_operands[1]=src_tn;
	expand_tie_macro_to_ops(ops, rtor_macro, in_operands);
      } else {
        Tie_Move_To_Result(tgt_tn, src_tn, ops);
      }
      // cannot do this yet as the op maybe a pseudo copy (e.g. "or", "and")
      // Set_OP_copy (OPS_last(ops));
    }
    else if (tgt_rc == src_rc && tgt_rc == TI_ISA_Regclass_Float()) {
      TOP mov_s = tie_info->xtfloat_move_topcode();
      TN *cpe = cpenable_tn(mov_s);
      if (cpe)
        Build_OP(mov_s, tgt_tn, src_tn, cpe, ops);
      else
        Build_OP(mov_s, tgt_tn, src_tn, ops);
      Set_OP_copy (OPS_last(ops));
    }
    else if (MTYPE_is_xtbool(TN_mtype(tgt_tn)) && TN_mtype(tgt_tn)==TN_mtype(src_tn))
    {
      Move_To_Register(TN_mtype(tgt_tn), tgt_tn, src_tn, ops);
    }
    else
    {
      fPrint_TN(stderr, "Copy %s <- ", tgt_tn);
      fPrint_TN(stderr, "%s\n", src_tn);
      FmtAssert(FALSE,("Unimplemented copy"));
    }
  }
}

void
Exp_Intrinsic_Op (INTRINSIC id, TN *result, TN *op0, TN *op1, OPS *ops)
{
  FmtAssert(FALSE, ("Exp_Intrinsic_Op NYI"));
}

/* ======================================================================
 * Expand_TOP_intrncall
 * 
 * Given a TOP_intrncall <op>, expand it into the sequence of instructions 
 * that must be generated. If <get_sequence_length> is TRUE, return only
 * the number of instructions in the sequence and don't actually do the 
 * expansion.
 * ======================================================================*/
static INT
Expand_TOP_intrncall (
  const OP *op, 
  OPS *ops, 
  BOOL get_sequence_length,
  INT pc_value)
{
  FmtAssert(FALSE, ("Expand_TOP_intrncall NYI"));
  return 0;
}

static TYPE_ID
Get_Intrinsic_Size_Mtype (INTRINSIC id)
{
  switch (id) {
  case INTRN_COMPARE_AND_SWAP_I4:
  case INTRN_LOCK_TEST_AND_SET_I4:
  case INTRN_LOCK_RELEASE_I4:
  case INTRN_FETCH_AND_ADD_I4:
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_FETCH_AND_SUB_I4:
  case INTRN_FETCH_AND_OR_I4:
  case INTRN_FETCH_AND_XOR_I4:
  case INTRN_FETCH_AND_AND_I4:
  case INTRN_FETCH_AND_NAND_I4:
	return MTYPE_I4;
  case INTRN_COMPARE_AND_SWAP_I8:
  case INTRN_LOCK_TEST_AND_SET_I8:
  case INTRN_LOCK_RELEASE_I8:
  case INTRN_FETCH_AND_ADD_I8:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I8:
  case INTRN_FETCH_AND_SUB_I8:
  case INTRN_FETCH_AND_OR_I8:
  case INTRN_FETCH_AND_XOR_I8:
  case INTRN_FETCH_AND_AND_I8:
  case INTRN_FETCH_AND_NAND_I8:
  case INTRN_SYNCHRONIZE:
	return MTYPE_I8;
  default:
  	FmtAssert(FALSE, ("Unexpected intrinsic %d", id));
	return MTYPE_UNKNOWN;
  }
}

static BOOL
Intrinsic_Returns_New_Value (INTRINSIC id)
{
  switch (id) {
  case INTRN_ADD_AND_FETCH_I4:
  case INTRN_SUB_AND_FETCH_I4:
  case INTRN_OR_AND_FETCH_I4:
  case INTRN_XOR_AND_FETCH_I4:
  case INTRN_AND_AND_FETCH_I4:
  case INTRN_NAND_AND_FETCH_I4:
  case INTRN_ADD_AND_FETCH_I8:
  case INTRN_SUB_AND_FETCH_I8:
  case INTRN_OR_AND_FETCH_I8:
  case INTRN_XOR_AND_FETCH_I8:
  case INTRN_AND_AND_FETCH_I8:
  case INTRN_NAND_AND_FETCH_I8:
	return TRUE;
  default:
	return FALSE;
  }
}

// initial expansion of intrinsic call (may not be complete lowering).
// return result TN (if set).
// If the intrinsic requires a label and loop (2 bb's)
// then ops is for first bb and ops2 is for bb after the label.
// Otherwise only ops is filled in.
TN *
Exp_Intrinsic_Call (INTRINSIC id, TN *op0, TN *op1, TN *op2, OPS *ops, 
	LABEL_IDX *label, OPS *loop_ops)
{
  return NULL;
}


/* Forward search from 'start', using 'visit' to keep track of visited
   blocks. If we reach 'end' return TRUE. */
static BOOL
Loop_End_Search (BB_VISITED_COUNTER *visit, BB *start, BB *end)
{
  if (start == end)
    return TRUE;

  visit->Set_visited(start);
  
  BBLIST *lst;
  FOR_ALL_BB_SUCCS(start, lst)
  {
    BB *succ = BBLIST_item(lst);
    if (!visit->Visited(succ))
    {
      if (Loop_End_Search(visit, succ, end))
	return TRUE;
    }
  }
  
  return FALSE;
}


/* Return true if simulated loop_end 'op' needs to be emitted as a
   nop. */
static BOOL
Loop_End_Needs_Nop (const OP *op)
{
  /* 'op' must be emitted as a nop if it is the first instruction in a
     basic-block, and:

     1 - the block is targeted by a branch from inside the loop. This
     is so a taken branch to the loop_end will not exit the loop.

     2 - the preceding block ends in a call. This is so a return from
     the call will not exit the loop.

     3 - the preceding block ends in a conditional branch back into
     the loop.  When the branch executes it will cause the loop count
     to be decremented even if it is taken (because it is the last
     instruction in the loop), so we need to nop after the branch to
     prevent the loop count from being decremented when the branch is
     taken.  */

  BB *bb = OP_bb(op);
  if ((BB_first_op(bb) != op))
    return FALSE;

  BB *prev_bb = BB_First_Pred(bb);
  if (prev_bb)
  {
    OP *pop = BB_last_op(prev_bb);
    if (pop)
    {
      /* case 2 */
      if (OP_call(pop))
	return TRUE;

      /* case 3 */
      if (OP_br(pop) && OP_cond(pop))
      {
	/* Search forward from the target of the branch. If there is a
           path that reaches 'prev_bb' before it reaches the loop
           entry, then the branch is targeting inside the loop, and we
           need the nop. */

	BB *targ = BB_Other_Successor(prev_bb, bb);
	FmtAssert(targ, ("unexpected successors for branch"));

	BB *ft = BB_Fall_Thru_Successor(bb);
	BB *loop_entry = BB_Other_Successor(bb, ft);
	FmtAssert(ft && loop_entry, ("unexpected successors for loop-end"));
	
	BB_VISITED_COUNTER visit(MEM_local_pool_ptr);
	visit.Init();
	visit.Set_visited(loop_entry);
	
	if (Loop_End_Search(&visit, targ, prev_bb))
	  return TRUE;
      }
    }
  }

  /* case 1. conservative as to whether the branch is from inside the
     loop or not. */
  if (!BB_has_label(bb))
    return FALSE;

  if (BB_preds_len(bb) > 1)
    return TRUE;
  
  return BBlist_Fall_Thru_Pred(bb) == NULL;
}

/* ======================================================================
 * Exp_Br_Reg_Simulated_Op
 *
 * Given a simulated <op> that moves/loads/stores branch registers,
 * expand it into the sequence of instructions supported by the target.
 * return TRUE if the expansion is done or FALSE otherwise
 * ======================================================================*/
BOOL
Exp_Br_Reg_Simulated_Op (const OP *op, OPS *ops)
{
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_extw_pseudo:
  case TOP_memw_pseudo:
  case TOP_loop_end:
  case TOP_load_const:
  case TOP_spadjust:
  case TOP_asm:
  case TOP_intrncall:
  case TOP_noop:
  case TOP_wsr_br:
  case TOP_rsr_br:
  case TOP_const16hi:
  case TOP_const16lo:
  {
    /* these will be handled in Exp_Simulated_Op (see below) */
    return FALSE;
  }

  case TOP_movbr:
  {
    TN* result = OP_result(op,0);
    TN* source = OP_opnd(op,0);
    FmtAssert(TN_is_register(result) && TN_is_register(source),
	      ("Wrong operand format for TOP_br_to_ar"));
    FmtAssert(TN_register(result) && TN_register(source),
		("Expect dedicated registers"));
    int result_reg_num = TN_register(result) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    int source_reg_num = TN_register(source) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;

    if (source_reg_num==result_reg_num)
      break;

    Build_OP(TOP_orb, OP_result(op, 0), OP_opnd(op, 0), OP_opnd(op, 0), ops);
    Set_OP_copy (OPS_last(ops));
    break;
  }

  case TOP_movbr2:
  case TOP_movbr4:
  {
    TN* result = OP_result(op,0);
    TN* source = OP_opnd(op,0);
    FmtAssert(TN_is_register(result) && TN_is_register(source),
	      ("Wrong operand format for TOP_br_to_ar"));
    FmtAssert(TN_register(result) && TN_register(source),
		("Expect dedicated registers"));
    int result_reg_num = TN_register(result) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    int source_reg_num = TN_register(source) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;

    if (source_reg_num==result_reg_num)
      break;

    int num_bits = (top==TOP_movbr2)?2:4;
    ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();
    INT first_reg = TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(br_rc)) + REGISTER_MIN;
    for (int i=0; i<num_bits; i++) {
      TN* tmp_result = Build_Dedicated_TN(
			TI_ISA_Regclass_Branch(), first_reg+result_reg_num+i,0);
      TN* tmp_source = Build_Dedicated_TN(
			TI_ISA_Regclass_Branch(), first_reg+source_reg_num+i,0);
      Build_OP(TOP_orb, tmp_result, tmp_source, tmp_source, ops);
      Set_OP_copy (OPS_last(ops));
    }
    break;
  }

  case TOP_movbr8:
  {
    TN* result = OP_result(op,0);
    TN* source = OP_opnd(op,0);
    TN* tmp1 = OP_opnd(op,1);
    TN* tmp2 = OP_opnd(op,2);
    FmtAssert(TN_is_register(result) && TN_is_register(source) &&
	      TN_is_register(tmp1) && TN_is_register(tmp2),
	      ("Wrong operand format for TOP_movb8"));
    FmtAssert(TN_register(result) && TN_register(source) &&
	      TN_register(tmp1) && TN_register(tmp2),
		("Expect dedicated registers"));
    int result_reg_num = TN_register(result) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    int source_reg_num = TN_register(source) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;

    if (source_reg_num!=result_reg_num) {
      Expand_Rsr_Br(tmp1, ops);
      Build_OP(TOP_extui,
		tmp1, tmp1, Gen_Literal_TN(source_reg_num,4),
		Gen_Literal_TN(8,4), ops);
      Build_OP(TOP_slli, tmp2, tmp1, Gen_Literal_TN(8,4), ops);
      Build_OP(TOP_or, tmp2, tmp2, tmp1, ops);
      Expand_Wsr_Br(tmp2, ops);
      Build_OP(TOP_esync, ops);
    }
    break;
  }


  case TOP_movbr16:
  {
    TN* result = OP_result(op,0);
    TN* source = OP_opnd(op,0);
    FmtAssert(TN_is_register(result) && TN_is_register(source),
	      ("Wrong operand format for TOP_br_to_ar"));
    FmtAssert(TN_register(result) && TN_register(source),
		("Expect dedicated registers"));
    int result_reg_num = TN_register(result) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    int source_reg_num = TN_register(source) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;

    FmtAssert(source_reg_num==result_reg_num,
		("Error in moving xtbool16 register values"));
    break;

  }

  case TOP_br_to_ar:
  case TOP_br2_to_ar:
  case TOP_br4_to_ar:
  case TOP_br8_to_ar:
  case TOP_br16_to_ar:
  {
    TN* br = OP_opnd(op,0);
    TN* ar = OP_result(op,0);
    FmtAssert(TN_is_register(br) && TN_is_register(ar),
	      ("Wrong operand format for TOP_br_to_ar"));
    INT br_num = TN_register(br) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    UINT32 num_bits = (top == TOP_br_to_ar ) ? 1 :
		      (top == TOP_br2_to_ar) ? 2 :
		      (top == TOP_br4_to_ar) ? 4 :
		      (top == TOP_br8_to_ar) ? 8 :
		      (top == TOP_br16_to_ar) ? 16 : 0;
    FmtAssert(num_bits, ("Unknown bit size"));
    Expand_Rsr_Br(ar, ops);
    if (num_bits!=16)
      Build_OP(TOP_extui,
		ar, ar, Gen_Literal_TN(br_num,4),
		Gen_Literal_TN(num_bits,4), ops);
    break;
  }
  case TOP_ar_to_br:
  case TOP_ar_to_br2:
  case TOP_ar_to_br4:
  case TOP_ar_to_br8:
  case TOP_ar_to_br16:
  {
    TN* ar = OP_opnd(op,0);
    TN* br = OP_result(op,0);
    TN* tmp = OP_opnd(op,1);
    TN* mask_tmp = OP_opnd(op,2);
    FmtAssert(TN_is_register(br) && TN_is_register(ar) &&
	      TN_is_register(tmp) && TN_is_register(mask_tmp),
	      ("Wrong operand format for TOP_ar_to_br"));
    if (tmp==mask_tmp || tmp==ar || mask_tmp==ar)
      FmtAssert(0, ("Needs two tmp register to expand TOP_ar_to_br"));

    INT br_num = TN_register(br) -
	TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(TI_ISA_Regclass_Branch())) - REGISTER_MIN;
    UINT32 num_bits = (top == TOP_ar_to_br ) ? 1 :
		      (top == TOP_ar_to_br2) ? 2 :
		      (top == TOP_ar_to_br4) ? 4 :
		      (top == TOP_ar_to_br8) ? 8 :
		      (top == TOP_ar_to_br16) ? 16 : 0;
    FmtAssert(num_bits, ("Unknown bit size"));
    if (num_bits==16) {
      Build_OP(TOP_mov_n, tmp, ar, ops);
      Build_OP(TOP_extui, tmp, tmp,
			Gen_Literal_TN(0,4), Gen_Literal_TN(16,4), ops);
      Expand_Wsr_Br(tmp, ops);
    } else {
      Expand_Rsr_Br(tmp, ops);
      UINT32 mask = ((1 << num_bits) - 1) << br_num;
      mask = mask ^ 0xffff;
      if (mask & 0xf800) {
	if (!xt_prefer_const16) {
	  ST* lc;
	  INITO_IDX ino;
	  INITV_IDX inv;
	  
	  {
	    /* Create a reference to the literal and put it in the literal pool */
	    lc = New_ST(CURRENT_SYMTAB);
	    ST_Init( lc, Save_Str(Exp_Unique_Literal_Name()), CLASS_VAR,
		     SCLASS_LITERAL_POOL, EXPORT_LOCAL, MTYPE_To_TY(MTYPE_U4));
	    Set_ST_is_initialized(lc);
	    Set_ST_is_temp_var(lc);
	    Set_ST_is_const_var(lc);
	    Allocate_Object(lc);
	  }
	  
	  ino = New_INITO( lc, inv = New_INITV());
	  INITV_Init_Integer(inv, MTYPE_U4, mask);
	  TN* mask_tn = Gen_Symbol_TN(lc, 0, TN_RELOC_NONE);
	  
	  Build_OP(TOP_l32r, mask_tmp, mask_tn, ops);
	  Set_OP_no_alias(OPS_last(ops));
	} else {
	  TN * mask_tn = Gen_Literal_TN(mask, 4);
	  TN * mask_tn_hi = Create_high_16_TN(mask_tn);
	  TN * mask_tn_lo = Create_low_16_TN(mask_tn);
	  Build_OP(TOP_const16, mask_tmp, mask_tmp, mask_tn_hi, ops);
	  Build_OP(TOP_const16, mask_tmp, mask_tmp, mask_tn_lo, ops);
	}
      } else {
        Build_OP(TOP_movi, mask_tmp, Gen_Literal_TN(mask,4), ops);
      }
      Build_OP(TOP_and, tmp, tmp, mask_tmp, ops);
      Build_OP(TOP_mov_n, mask_tmp, ar, ops);
      if (br_num!=0)
        Build_OP(TOP_slli, mask_tmp, mask_tmp,
				Gen_Literal_TN(br_num,4), ops);
      Build_OP(TOP_or, tmp, tmp, mask_tmp, ops);
      Expand_Wsr_Br(tmp, ops);
    }
    Build_OP(TOP_esync, ops);
    break;
  }

  case TOP_get_tmp_ar:
    break;

  default:
    if (OP_tie(op))
      break;
    FmtAssert(FALSE, ("simulated OP %s not handled", TI_TOP_Name(top)));
  }
  return TRUE;
}


/* ======================================================================
 * Create_low_16_TN
 * Create_high_16_TN
 *
 * Helper functions that extract the hi and low 16 bits from a TN
 * ======================================================================*/

static TN *
Create_low_16_TN(TN * lit)
{
  Is_True(TN_size(lit) == 4, ("low16 doesn't support size"));
  Is_True(TN_is_constant(lit), ("Non constant const16 tn"));
  TN *result = Dup_TN(lit);
  if (TN_is_symbol(lit) || TN_is_label(lit))
    Set_TN_is_reloc_low16(result);
  else
    Set_TN_value(result, (TN_value(lit) & 0xffff));
  return result;
}
		 
static TN *
Create_high_16_TN(TN * lit)
{
 Is_True(TN_size(lit) == 4, ("high16 doesn't support size"));
  Is_True(TN_is_constant(lit), ("Non constant const16 tn"));
  TN *result = Dup_TN(lit);
  if (TN_is_symbol(lit) || TN_is_label(lit))
    Set_TN_is_reloc_high16(result);
  else
    Set_TN_value(result, ((TN_value(lit) >> 16) & 0xffff));
  return result;
}


/* ======================================================================
 * Exp_Simulated_Op
 *
 * Given a simulated <op>, expand it into the sequence of instructions
 * supported by the target.
 * ======================================================================*/
void
Exp_Simulated_Op (const OP *op, OPS *ops, INT pc_value)
{
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_extw_pseudo:
  case TOP_memw_pseudo:
    break;

  case TOP_loop_end:
    if (Loop_End_Needs_Nop(op)) {
      Exp_Noop(ops);
    }
    break;

  case TOP_const16hi:
  {
    TN *lit_hi = Create_high_16_TN(OP_opnd(op, 0));
    
    OP * const16hi = Mk_OP(TOP_const16, OP_result(op, 0), 
			   OP_result(op, 0), lit_hi);
    OPS_Append_Op(ops, const16hi);
    break;
  }
  case TOP_const16lo:
  {
    TN *lit_lo = Create_low_16_TN(OP_opnd(op,1));

    Is_True(TN_register(OP_result(op, 0)) == TN_register(OP_opnd(op, 0)),
	    ("const16lo must have same input and output register"));
    OP * const16lo = Mk_OP(TOP_const16, OP_result(op, 0), 
			   OP_opnd(op, 0), lit_lo);
    OPS_Append_Op(ops, const16lo);
    break;
  }
  case TOP_load_const:
  {
    /* Most likely, TOP_load_const should be expanded by Exp_load_const
       already.  But it's possible that IGLS may generate load_const  
       rematerialized ?). So, this code shoule not be deleted.
     */

    if (xt_prefer_const16) 
    {
      DevWarn("load const with const16 should have been resolved earlier");
      TN *lit_hi = Create_high_16_TN(OP_opnd(op, 0));
      TN *lit_lo = Create_low_16_TN(OP_opnd(op, 0));
      TN *result = OP_result(op, 0);
      OP * const16hi = Mk_OP(TOP_const16, result, result, lit_hi, ops);
      OP * const16lo = Mk_OP(TOP_const16, result, result, lit_lo, ops);

      OPS_Append_Op(ops, const16hi);
      OPS_Append_Op(ops, const16lo);
    }
    else {
      /* When the "load_const" instruction expands to an l32r. We generated
	 the literal symbol in Finalize_Simulated_Op, so here we just
	 generate the l32r using the same operands. */

      OP *l32r = Mk_OP(TOP_l32r, OP_result(op, 0), OP_opnd(op, 0));
      OPS_Append_Op(ops, l32r);
    }
    break;
  }

  case TOP_wsr_br:
  {
    ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();
    INT first_reg = TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(br_rc)) + REGISTER_MIN;
    Build_OP(TI_TOP_Topcode("wsr.br"), Build_Dedicated_TN(br_rc, first_reg, 4),
	     OP_opnd(op, 0), ops);
    break;
  }
    
  case TOP_rsr_br:
  {
    ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();
    INT first_reg = TI_ISA_Regclass_First_Reg(TI_ISA_Regclass_Info(br_rc)) + REGISTER_MIN;
    Build_OP(TI_TOP_Topcode("rsr.br"), OP_result(op, 0),
	     Build_Dedicated_TN(br_rc, first_reg, 4), ops);
    break;
  }

  default:
    FmtAssert(FALSE, ("simulated OP %s not handled", TI_TOP_Name(top)));
  }
}

/* ======================================================================
 * Exp_load_const
 *
 * Given a TOP_load_const op (a simulated op), expand it into the sequence
 * of instructions supported by the target. The purpose of this function
 * is to let IGLS to handle l32r so l32r can be bundled in FLIX instructions
 * ======================================================================*/
BOOL
Exp_load_const (const OP *op, OPS *ops)
{
  BOOL has_expanded=FALSE;
  TOP top = OP_code(op);
  
  if (top == TOP_load_const)
  {
    if (xt_prefer_const16) 
    {
      DevWarn("load const with const16 should have been resolved earlier");
      TN *lit_hi = Create_high_16_TN(OP_opnd(op, 0));
      TN *lit_lo = Create_low_16_TN(OP_opnd(op, 0));
      TN *result = OP_result(op, 0);
      OP * const16hi = Mk_OP(TOP_const16, result, result, lit_hi, ops);
      OP * const16lo = Mk_OP(TOP_const16, result, result, lit_lo, ops);

      OPS_Append_Op(ops, const16hi);
      OPS_Append_Op(ops, const16lo);

      Set_OP_is_load_const(const16hi);
      Set_OP_is_load_const(const16lo);
    }
    else {
      TN *cv = OP_opnd(op,0);
      TN *res = OP_result(op,0);
      Is_True(TN_is_constant(cv), ("load_const expects constant operand"));
      if (TN_is_symbol(cv)) {
        ST * target_const = TN_var(cv);
        Allocate_Object(target_const);
      }

      /*
         This will expand to an l32r.
         Generate the literal symbol to hold the literal and
         replace 'cv' with it. 
       */
      INITV_IDX inv;
      INITO_IDX ino;
      /* create a literal location */
      ST *lc = New_ST(CURRENT_SYMTAB);
      ST_Init(lc, Save_Str(Exp_Unique_Literal_Name()), CLASS_VAR,
              SCLASS_LITERAL_POOL, EXPORT_LOCAL,
              MTYPE_To_TY(MTYPE_U4));
      Set_ST_is_initialized(lc);
      Set_ST_is_temp_var(lc);
      Clear_ST_is_not_used(lc);
      Set_ST_is_const_var(lc);
      Allocate_Object(lc);

      ino = New_INITO(lc, inv = New_INITV());
      if (TN_has_value(cv)) {
        INITV_Init_Integer(inv, MTYPE_I4, TN_value(cv));
      }
      else if (TN_is_label(cv)) {
        INITV_Init_Label(inv, TN_label(cv));
      }
      else if (TN_is_symbol(cv)) {
        INITV_Init_Symoff(inv, TN_var(cv), TN_offset(cv));
      }
      else {
        FmtAssert(FALSE, ("Unhandled literal constant"));
      }

      OP *l32r = Mk_OP(TOP_l32r, OP_result(op, 0), Gen_Symbol_TN(lc, 0, TN_RELOC_NONE));
      OPS_Append_Op(ops, l32r);
      Set_OP_is_load_const(l32r);
    }
    has_expanded = TRUE;
  } 

  return has_expanded;
}


/* ======================================================================
 * Finalize_Simulated_Op
 *
 * Given a simulated <op>, perform any symbol generation or other
 * finalization that must occur just before assembly emission.
 * ======================================================================*/
void
Finalize_Simulated_Op (OP *op)
{
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_load_const:
  {
    /* Most likely, TOP_load_const should be expanded by Exp_load_const
       already.  But it's possible that IGLS may generate load_const  
       rematerialized ?). So, this code shoule not be deleted.
     */
    TN *cv = OP_opnd(op,0);
    TN *res = OP_result(op,0);
    Is_True(TN_is_constant(cv), ("load_const expects constant operand"));
    if (TN_is_symbol(cv)) {
      ST * target_const = TN_var(cv);
      Allocate_Object(target_const);
    }	
    if (!xt_prefer_const16) {
      /* This will expand to an l32r. 
	 Generate the literal symbol to hold the literal and
	 replace 'cv' with it. A symbol is not a valid operand to
	 TOP_load_const, but we are expecting it in Exp_Simulated_Op. */
	
      INITV_IDX inv;
      INITO_IDX ino;
      /* create a literal location */
      ST *lc = New_ST(CURRENT_SYMTAB);
      ST_Init(lc, Save_Str(Exp_Unique_Literal_Name()), CLASS_VAR,
	      SCLASS_LITERAL_POOL, EXPORT_LOCAL,
	      MTYPE_To_TY(MTYPE_U4));
      Set_ST_is_initialized(lc);
      Set_ST_is_temp_var(lc);
      Clear_ST_is_not_used(lc);
      Set_ST_is_const_var(lc);
      Allocate_Object(lc);
	
      ino = New_INITO(lc, inv = New_INITV());
      if (TN_has_value(cv)) {
	INITV_Init_Integer(inv, MTYPE_I4, TN_value(cv));
      } 
      else if (TN_is_label(cv)) {
	INITV_Init_Label(inv, TN_label(cv));
      }
      else if (TN_is_symbol(cv)) {
	INITV_Init_Symoff(inv, TN_var(cv), TN_offset(cv));
      }	
      else
	FmtAssert(FALSE, ("Unhandled literal constant"));
      Set_OP_opnd(op, 0, Gen_Symbol_TN(lc, 0, TN_RELOC_NONE));
    }
  }
  break;
  case TOP_const16hi:
  {
    TN *cv = OP_opnd(op,0);
    Is_True(TN_is_constant(cv), ("const16 expects constant operand"));
    if (TN_is_symbol(cv)) {
      Allocate_Object(TN_var(cv));
    }	
    break;
  }
  }
}


/* ======================================================================
 * Simulated_Op_Real_Ops
 *
 * Return the number of instructions that will be generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Ops(const OP *op)
{
  INT num_ops = 0;
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_extw_pseudo:
  case TOP_memw_pseudo:
    num_ops = 0;
    break;

  case TOP_loop_end:
    /* The "loop_end" instruction expands to nothing or to a nop. */
    if (Loop_End_Needs_Nop(op))
      num_ops = 1;
    break;

  case TOP_load_const:
    /* Implemented with l32r or const16... */
    if (xt_prefer_const16)
      num_ops = 2;
    else
      num_ops = 1;
    break;
    
  case TOP_asm:
    /* We don't know how many instructions are "within" the asm, so we
       just assume 1. */
    num_ops = 1;
    break;

  case TOP_spadjust:
    /* This is almost always 1 instruction... */
    num_ops = 1;
    break;

  case TOP_br_to_ar:
  case TOP_br2_to_ar:
  case TOP_br4_to_ar:
  case TOP_br8_to_ar:
  case TOP_br16_to_ar:
    num_ops = 2;
    break;

  case TOP_ar_to_br:
  case TOP_ar_to_br2:
  case TOP_ar_to_br4:
  case TOP_ar_to_br8:
  case TOP_ar_to_br16:
    num_ops = 7;
    break;

  case TOP_movbr:
  case TOP_wsr_br:
  case TOP_rsr_br:
    num_ops = 1;
    break;
  case TOP_movbr2:
    num_ops = 2;
    break;
  case TOP_movbr4:
    num_ops = 4;
    break;
  case TOP_movbr8:
    num_ops = 6;
    break;
  case TOP_movbr16:
    num_ops = 0;
    break;

  case TOP_get_tmp_ar:
    num_ops = 0;
    break;

  case TOP_const16hi:
  case TOP_const16lo:
    num_ops = 1;
    break;

  default:
    if (OP_tie(op)) {
      num_ops = 0;
      break;
    }
    FmtAssert(FALSE, ("simulated OP %s not handled", TI_TOP_Name(OP_code(op))));
  }

  return num_ops;
}


/* ======================================================================
 * Simulated_Op_Real_Inst_Bytes
 *
 * Return the number of instruction bytes that will ultimately be emitted
 * for the expansion generated by Exp_Simulated_Op
 * ======================================================================*/
INT
Simulated_Op_Real_Inst_Bytes(const OP *op)
{
  INT num_bytes = 0;
  TOP top = OP_code(op);

  switch (top)
  {
  case TOP_extw_pseudo:
  case TOP_memw_pseudo:
    num_bytes = 0;
    break;

  case TOP_loop_end:
    /* The "loop_end" instruction expands to nothing or to a nop. */
    if (Loop_End_Needs_Nop(op))
      num_bytes = TI_ISA_Inst_Bytes(CGTARG_Noop_Top());
    break;
    
  case TOP_const16hi:
  case TOP_const16lo:
    num_bytes = 3;
    break;
  case TOP_load_const:
    /* Implemented with l32r or two const16s */
    if (xt_prefer_const16)
      num_bytes = 6;
    else
      num_bytes = 3;
    break;
    
  case TOP_asm:
    /* We don't know how many instructions are "within" the asm, so we
       just assume 3 bytes. */
    num_bytes = 3;
    break;

  case TOP_spadjust:
    /* This is almost always 1 instruction... */
    num_bytes = 3;
    break;

  case TOP_br_to_ar:
  case TOP_br2_to_ar:
  case TOP_br4_to_ar:
  case TOP_br8_to_ar:
  case TOP_br16_to_ar:
  {
    num_bytes = 6;
    break;
  }
  case TOP_ar_to_br:
  case TOP_ar_to_br2:
  case TOP_ar_to_br4:
  case TOP_ar_to_br8:
  case TOP_ar_to_br16:
  {
    num_bytes = 21;
    break;
  }

  case TOP_movbr:
  case TOP_wsr_br:
  case TOP_rsr_br:
    num_bytes = 3;
    break;
  case TOP_movbr2:
    num_bytes = 6;
    break;
  case TOP_movbr4:
    num_bytes = 12;
    break;
  case TOP_movbr8:
    num_bytes = 18;
    break;
  case TOP_movbr16:
    num_bytes = 3;
    break;

  case TOP_get_tmp_ar:
    num_bytes = 0;
    break;

  default:
    if (OP_tie(op)) {
      num_bytes = 0;
      break;
    }
    FmtAssert(FALSE, ("simulated OP %s not handled", TI_TOP_Name(OP_code(op))));
  }

  return num_bytes;
}


/* ======================================================================
 * Exp_Is_Large_Stack_Sym
 *
 * determine if a given symbol is a stack relative reference that will
 * require multiple instructions to load or store.
 * ======================================================================*/
BOOL
Exp_Is_Large_Stack_Sym(ST* sym,  INT64 ofst)
{
  ST *base_sym;
  INT64 base_ofst;
  
  if (sym == NULL)
    return FALSE;

  Allocate_Object(sym);
  Base_Symbol_And_Offset_For_Addressing (sym, ofst, &base_sym, &base_ofst);

  /* We can assume that 'sym' is a spill location for an integer
     register, so we can check for l32i/s32i range. */
  
  if ((base_sym == SP_Sym || base_sym == FP_Sym) &&
      !ISA_LC_Value_In_Class(base_ofst, LC_uimm8x4)) {
    return TRUE;
  }

  return FALSE;
}

void
Exp_Noop (OPS *ops)
{
  Build_OP (CGTARG_Noop_Top(), ops);
}

void 
Exp_Spadjust (TN *dest, TN *size, UINT16 variant, OPS *ops)
{
  Build_OP (TOP_spadjust, dest, SP_TN, size, ops);
  OP_variant(OPS_last(ops)) = variant;
  Set_OP_no_move_before_gra(OPS_last(ops));
}

/* Return a unique name for a symbol representing a literal. */
char *
Exp_Unique_Literal_Name (void)
{
  static int unique;
  static char name[32];

  sprintf(name, ".LC%d", unique);
  unique++;
  return name;
}


void 
Exp_Generic_Pred_Calc(TN* result1, TN *result2, COMPARE_TYPE ctype,
		      TN *qual_pred, OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }


void
Exp_Pred_Calc(TN* result, OP* cmp_op, COMPARE_TYPE ctype, BOOL false_result,
	      OPS* ops)
{ FmtAssert(FALSE,("Unimplemented")); }

///////////////////////////////////////////////////////////
//
// Setup the true_tn and false_tn for a BB. The true_tn is a TN such that
// it is true if the branch at the end of a BB is taken. If it false
// through the false_tn will be set true.
// 
// This routine works by trying to find the compare which generates the
// branch predicate for the block. Assuming it finds one, and it's of the
// right form (i.e. an unc form), it attempts to simply re-use the two TN's
// it generates. 
//
// Right now, if it doesn't find it, it asserts, but I don't think this is
// going to happen, given the current way we generate things.
//
// The above can happen if we are trying to generate the false predicate
// for a block that has a branch which came from a previous pass of
// hyperblock formation. In this case, we don't have a single defining
// compare. So if we have a predicate Pb, (which is the predicate used for
// the branch, we wan't Pf such that Pf is TRUE if Pb is false and the block
// is executed.  We can accomplish this by initializing Pf to 1 under the
// block predicate, and setting it to 0 if Pb is TRUE.
// 
void
Exp_True_False_Preds_For_Block(BB *bb, TN* &true_tn, TN * &false_tn) 
{ FmtAssert(FALSE,("Unimplemented")); }

BOOL
Target_Has_Immediate_Operand (WN *parent, WN *expr)
{
  FmtAssert(0, ("Unnecessary for xtensa"));
  return FALSE;
}

 
// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
