
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


#include "lnotarget.h"
#include "intrn_info.h"
#include "tie.h"
#include "w2op.h"
#include "config_targ_options.h"

extern DLL_SHARED TIE_INFO  *tie_info;             // TIE info pointer

TOP
LNOTARGET_Whirl_To_Top (WN* wn)
{
  return WHIRL_To_TOP(wn);
}

void
LNOTARGET_Loop_Inc_Test_Res (TI_RES_COUNT* resource_count)
{
  if (!xt_zero_cost_loop || !Enable_ZCL) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_addi);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_beqz);
  }
}

double
LNOTARGET_Cvt_Res (TI_RES_COUNT* resource_count, OPCODE opcode)
{
  // FIXME: this should look at the desc/rtype and
  // add the appropriate resources
  return 1.0;
}

double
LNOTARGET_FP_Madd_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 1.0;
}

double
LNOTARGET_FP_Min_Max_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 3.0;
}

double
LNOTARGET_FP_Div_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
    return 8.0;
}

double
LNOTARGET_FP_Recip_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
    return 7.0;
}

double
LNOTARGET_FP_Rsqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
    return 10.0;
}

double
LNOTARGET_FP_Sqrt_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 19.0;
}
 
double
LNOTARGET_FP_Exp_Res (TI_RES_COUNT* resource_count, 
                      INTRINSIC intr,
                      INT num_multiplies)
{
  return 0.0;
}

double
LNOTARGET_Complex_Add_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 2.0;
}

double
LNOTARGET_Complex_Mult_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 4.0;
}

double
LNOTARGET_Complex_Neg_Res (TI_RES_COUNT* resource_count, TYPE_ID mtype)
{
  return 2.0;
}

double
LNOTARGET_Int_Select_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movnez);
  return 2.0;
}

double
LNOTARGET_Int_Cvtl_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_extui);
  return 1.0;
}

double
LNOTARGET_Int_Neg_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_neg);
  return 1.0;
}

double
LNOTARGET_Int_Abs_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  if (xt_abs) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_abs);
    return 1.0;
  }
  else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_neg);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movgez);
    return 2.0;
  }
}

double
LNOTARGET_Int_Bnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
  return 1.0;
}

double
LNOTARGET_Int_Lnot_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movi);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movi);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_moveqz);
  return 3.0;
}

double
LNOTARGET_Int_Mult_Res (TI_RES_COUNT* resource_count, WN *wn)
{
  char *op_name = NULL;
  if (xt_mul16 && WN_Mpy_16Bit(wn)) {
    if (MTYPE_is_unsigned(WN_rtype(wn))) {
      op_name = "mul16u";
    } else {
      op_name = "mul16s";
    }
  } else if (xt_mul32) {
    op_name = "mull";
  }
  TOP top = op_name ? TI_TOP_Topcode(op_name) : TOP_UNDEFINED;
  if (top == TOP_UNDEFINED) {
    Lmt_DevWarn(1,("No MUL instruction found by LNOTARGET"));
    // FIXME:
    top = TOP_add;
  }
  TI_RES_COUNT_Add_Op_Resources(resource_count, top);
  return 1.0;
}

double
LNOTARGET_Int_Add_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);  
  return 1.0;
}

double
LNOTARGET_Int_Sub_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);  
  return 1.0;
}

double
LNOTARGET_Int_Div_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (eight_bytes ? 18.0 : 16.0);
}

double
LNOTARGET_Int_Mod_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (eight_bytes ? 25.0 : 23.0);
}

double
LNOTARGET_Int_Rem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (eight_bytes ? 21.0 : 19.0);
}

double
LNOTARGET_Int_DivRem_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  return (eight_bytes ? 22.0 : 20.0);
}

double
LNOTARGET_Int_Min_Max_Res (TI_RES_COUNT* resource_count, BOOL minmax)
{
  if (xt_minmax) {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_max);
    return 1.0;
  } else {
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
    TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movgez);
    return 3.0;
  }
}
  
double
LNOTARGET_Int_Band_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);  
  return 1.0;
}

double
LNOTARGET_Int_Bior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);  
  return 1.0;
}

double
LNOTARGET_Int_Bnor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);  
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);  
  return 2.0;
}

double
LNOTARGET_Int_Bxor_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_xor);
  return 1.0;
}

double
LNOTARGET_Int_Land_Res (TI_RES_COUNT* resource_count)
{
  // FIXME: This isn't probably right ...
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  return 1.0;
}

double
LNOTARGET_Int_Cand_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_and);
  return 1.0;
}

double
LNOTARGET_Int_Lior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  return 1.0;
}

double
LNOTARGET_Int_Cior_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  return 1.0;
}

double
LNOTARGET_Int_Shl_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sll);
  return 1.0;
}

double
LNOTARGET_Int_Ashr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sra);
  return 1.0;
}

double
LNOTARGET_Int_Lshr_Res (TI_RES_COUNT* resource_count, BOOL eight_bytes)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_srl);
  return 1.0;
}

double
LNOTARGET_Int_Eq_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_moveqz);
  return 3.0;
}

double
LNOTARGET_Int_Ne_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movnez);
  return 3.0;
}

double
LNOTARGET_Int_Gt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movltz);
  return 3.0;
}

double
LNOTARGET_Int_Ge_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movgez);
  return 3.0;
}

double
LNOTARGET_Int_Lt_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movltz);
  return 3.0;
}

double
LNOTARGET_Int_Le_Res (TI_RES_COUNT* resource_count)
{
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_sub);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_or);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_movgez);
  return 3.0;
}

double
LNOTARGET_Int_Lda_Res (TI_RES_COUNT* resource_count)
{
  // FIXME:
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);
  TI_RES_COUNT_Add_Op_Resources(resource_count, TOP_add);
  return 2.0;
}


// recursively add to 'resource_count' all TOPs that a TIE macro
// will be lowered to

static double
LNOTARGET_Tie_Macro_Res_Rec(TI_RES_COUNT* resource_count,
			    TIE_MACRO_p tie_macro) {
  double inst = 0.0;
  INT num_instr = tie_macro->num_instructions();
  for (INT i=0; i<num_instr; i++) {
    const char *opcode_name = tie_macro->inst_opcode_name(i);
    TOP top = TI_TOP_Topcode(opcode_name);
    if (top != TOP_UNDEFINED) {
      // real instruction
      TI_RES_COUNT_Add_Op_Resources(resource_count, top);
      inst += 1.0;
    } else {
      // should be another TIE proto
      TIE_MACRO_p nmacro = tie_info->tie_macro(opcode_name);
      if (nmacro==NULL) {
	Lmt_DevWarn(1,("No TIE macro found by LNOTARGET for %s",opcode_name));
	return -1.0;
      }
      double new_inst = LNOTARGET_Tie_Macro_Res_Rec(resource_count,nmacro);
      if (new_inst<0.0) {
	return -1.0;
      }
      inst += new_inst;
    }
  }
  return inst;
}

double
LNOTARGET_Tie_Intrinsic_Res (TI_RES_COUNT* resource_count,
			     INTRINSIC intrn) {
  
  TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
  TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
  
  double inst = LNOTARGET_Tie_Macro_Res_Rec(resource_count,tie_macro);
  return inst;
}


INT
LNOTARGET_Cvt_Lat (OPCODE opcode)
{
  // FIXME: 
  return 1;
}

INT
LNOTARGET_FP_Madd_Add_Lat (TYPE_ID mtype)
{
  return 1;
}

INT
LNOTARGET_FP_Madd_Mult_Lat (TYPE_ID mtype)
{
  return 1;
}

INT
LNOTARGET_FP_Min_Max_Lat (TYPE_ID mtype)
{
  return 1;
}

INT
LNOTARGET_FP_Div_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 20;
}

INT
LNOTARGET_FP_Recip_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 15;
}

INT
LNOTARGET_FP_Rsqrt_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 15;
}

INT
LNOTARGET_FP_Sqrt_Lat (TYPE_ID mtype)
{
  Lmt_DevWarn(1, ("TODO: Finish LNOTARGET_*_Lat for IA-64"));
  return 20;
}

INT
LNOTARGET_FP_Exp_Lat (INTRINSIC intr, INT num_multiplies)
{
  return -1;
}

INT
LNOTARGET_Complex_Add_Lat (TYPE_ID mtype)
{
  return 1;
}

INT
LNOTARGET_Complex_Mult_Lat (TYPE_ID mtype)
{
  return 1;
}

INT
LNOTARGET_Complex_Neg_Lat (TYPE_ID mtype)
{
  return 1;
}
