
// simd_ti_v2.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD Vectra2 target info interface routines                               *
 *                                                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_ti_v2.cxx $

#include "intrn_info.h"
#include "simd.h"
#include "simd_ti.h"
#include "simd_ti_v2.h"
#include "simd_loop_v2.h"

// returns the TARG_SIMD_INFO structure for the (vl,base_type) pair
TARG_SIMD_INFO *
SIMD_TI_V2::Targ_Simd_Info(TYPE_ID base_type, INT vl) 
{
    BIT_VECTOR *bv = Vector_Lengths(base_type);

    if (bv == NULL) {
	return NULL;
    }

    Is_True(bv->Pop_Count() >= 1, 
	    ("SIMD_TI_V2::Targ_Simd_Info: Empty vector length BV"));

    if (vl == 0) {
	vl = bv->Least_Non_Zero();
    }
    Is_True(vl > 0, ("SIMD_TI_V2::Targ_Simd_Info: vl <= 0"));

    return Type_Info_Map()->Find(TARG_SIMD_INFO_KEY(vl, base_type));
}

/* find a SIMD function in 'type' for 'op' corresponding the 'lohi' */
TIE_MACRO_ID
SIMD_TI_V2::Search_Vector_Macro(OPERATOR op,
				TYPE_ID dtype, TYPE_ID rtype,
				INT lohi, bool update)
{
    if (simd_debug) {
	fprintf(TFile,"Search Vector Macro: %s %s %s %d %s\n",
		OPERATOR_name(op),MTYPE_name(dtype),MTYPE_name(rtype),lohi,update?"UPDATE":"");
    }
    
    TIE_MACRO_ID macro_id = TIE_INVALID_ID;

    if (rtype==MTYPE_UNKNOWN) {
	rtype = dtype;
    }
  
    INT vl_r = Get_SIMD_Width_SIMD(rtype);
    INT vl_d = Get_SIMD_Width_SIMD(dtype);

    switch (op) {
        case OPR_ASHR:
        case OPR_LSHR:
        case OPR_SHL: {
	    INTRINSIC intrn = INTRINSIC_INVALID;
	    if (lohi == 1) { // variable shift
		intrn = Tie_Shift_Intrinsic(op, rtype);
	    } else {
		intrn = Tie_Shift_Intrinsic_Imm(op, rtype, 1);
	    }
	    if (intrn == INTRINSIC_INVALID) {
		return TIE_INVALID_ID;
	    }
	    return Intrinsic_To_Tie_Macro_Id(intrn);
	}
	    
	case OPR_MPY:
	case OPR_MADD:
	case OPR_MSUB:

	    if (vl_r < vl_d) {
		/* handle paired multiplication */
		Is_True(2*vl_r == vl_d, 
			("SIMD_TI_V2::Search_Vector_Macro: length mismatch"));
		TI_DIR   dirs[NUM_MPY_OPERANDS];
		TYPE_ID  types[NUM_MPY_OPERANDS];
		INT64    imm[NUM_MPY_OPERANDS];
		if (op == OPR_MADD || op == OPR_MSUB) {
		    dirs[0] = dirs[1] = TI_DIR_INOUT;
		} else {
		    dirs[0] = dirs[1] = TI_DIR_OUT;
		}
		dirs[2] = dirs[3] = TI_DIR_IN;
		
		types[0] = types[1] = rtype;
		types[2] = types[3] = dtype;
		imm[0] = imm[1] = imm[2] = imm[3] = 0;
		macro_id = Tie_Operator_Macro(op, NUM_MPY_OPERANDS,
					      dirs,types,imm);
	    } else {
		return SIMD_TI::Search_Vector_Macro(op, dtype, rtype, lohi, update);
	    }
	    break;
	default:
	    return SIMD_TI::Search_Vector_Macro(op, dtype, rtype, lohi, update);
    }

    return macro_id;
}


INTRINSIC
SIMD_TI_V2::Tie_Pack_Intrinsic(TYPE_ID out_type, TYPE_ID in_type) {
    if (simd_debug) {
	fprintf(TFile,"request for pack (%s,%s)\n",
		MTYPE_name(out_type), MTYPE_name(in_type));
    }
  
    TI_DIR dirs[3];
    TYPE_ID types[3];
    INT64 imm[3];
    
    dirs[0] = TI_DIR_OUT;
    dirs[1] = dirs[2] = TI_DIR_IN;
    
    types[0] = out_type;
    types[1] = types[2] = in_type;
  
    imm[0] = imm[1] = imm[2] = 0;
  
    INTRINSIC intrn = 
	Tie_TiOperator_Intrinsic_Exp(TI_OPR_VPACK,3,dirs,types,imm);
    return intrn;
}

INTRINSIC
SIMD_TI_V2::Tie_Shift_Intrinsic (OPERATOR oper, TYPE_ID vec_type) {
    Is_True(oper == OPR_ASHR || oper == OPR_LSHR || oper == OPR_SHL,
	    ("Unexpected operator %s", OPERATOR_name(oper)));
    
    if (simd_debug) {
	fprintf(TFile,"request for SHIFT_STATE (%s, %s)\n",
		OPERATOR_name(oper), MTYPE_name(vec_type));
    }
    
    TYPE_ID operands[2];
    TI_DIR dir[2];
    operands[0] = operands[1] = vec_type;
    dir[0] = TI_DIR_OUT;
    dir[1] = TI_DIR_IN;
    int num_operands = 2;
    
    return Tie_Operator_Intrinsic(oper, num_operands, dir, operands, NULL);
}


bool
SIMD_TI_V2::Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type)
{
  /* Currently we support only sum reduction for Vectra 2. */
  if (red_type != RED_ADD)
    return false;
  
  return SIMD_TI::Reduction_Possible(red_type, scalar_type);
}


bool
SIMD_TI_V2::Can_Cvt(TYPE_ID out_type, TYPE_ID in_type)
{
    return (Type_Conversion_Possible(in_type, out_type) ||
	    Has_Normal_Cvt(out_type, in_type) ||
	    Has_Pack_Cvt(out_type, in_type) ||
	    Has_Mul_Cvt(out_type, in_type));
}

bool
SIMD_TI_V2::Has_Pack_Cvt(TYPE_ID out_type, TYPE_ID in_type)
{
    if (Get_SIMD_Width_SIMD(in_type) * 2 == Get_SIMD_Width_SIMD(out_type)) {
	return (Tie_Pack_Intrinsic(out_type, in_type) != INTRINSIC_INVALID);
    }
    return false;
}

bool
SIMD_TI_V2::Has_Mul_Cvt(TYPE_ID out_type, TYPE_ID in_type)
{
    if (Get_SIMD_Width_SIMD(out_type) * 2 == Get_SIMD_Width_SIMD(in_type)) {
	return (Has_Paired_Mac(OPR_MPY, out_type, in_type) ||
		Has_Even_Odd_Mac(OPR_MPY, out_type, in_type));
    }
    return false;
}

bool
SIMD_TI_V2::Has_Normal_Cvt(TYPE_ID out_type, TYPE_ID in_type)
{
    /* return false for now */
    return false; 
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
