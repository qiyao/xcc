// Copyright (c) 2003-2007 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// at_op_xcc.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  Auto TIE Types and Operators                                             *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.

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

// $Id: at_op_xcc.cxx $

#include "at.h"
#include "at_op_xcc.h"
#include "at_xcc.h"
#include "cxx_hash.h"
#include "intrn_info.h"
#include "tie.h"
#include "tietypes.h"
#include "wintrinsic.h"


//
// AT_FACTORY
//


#define IS_POWER_OF_TWO(n) (((n) & ((n) - 1)) == 0)


AT_FACTORY::AT_FACTORY (XT_MEMPOOL *pool, AT_OP_TAB *op_tab) :
    _default_vl(1)
{
    _pool = XT_New(pool) XT_MEMPOOL("AT_FACTORY pool");
    set_op_tab(op_tab);
    default_wn_decode_func();
}


AT_FACTORY::~AT_FACTORY ()
{
}


void
AT_FACTORY::set_op_tab (AT_OP_TAB *op_tab) {
    _ty_tab = op_tab->Ty_Tab();
    _op_tab = op_tab;
}


AT_TY_ID
AT_FACTORY::get_at_ty_id (AT_TY *temp_ty) {
    AT_TY *ty = ty_tab()->Add_Type(temp_ty);
    Is_True(ty, ("Couldn't add ty (%s) to the type table!", temp_ty->Name()));
    return ty->Id();
}


AT_TY_ID
AT_FACTORY::get_at_ty_id (AT_TIE_TYPE *type, INT vl, UINT flags)
{
    XT_MEMPOOL::mark popper(pool());
    AT_TY *ty = XT_New(pool()) AT_TY(ty_tab(), type, vl, flags);
    return get_at_ty_id(ty);
}
    

AT_TY_ID
AT_FACTORY::get_at_ty_id (AT_TIE_STATE *state, INT vl, UINT flags)
{
    XT_MEMPOOL::mark popper(pool());
    AT_TY *ty = XT_New(pool()) AT_TY(ty_tab(), state, vl, flags);
    return get_at_ty_id(ty);
}
    

AT_TY_ID
AT_FACTORY::get_at_ty_id (TYPE_ID type, INT vl, UINT flags)
{
    XT_MEMPOOL::mark popper(pool());
    
    if (MTYPE_is_tie_packed(type)) {
	// We don't need to handle packed TIE types -- the compiler way of
	// handling multiple output TIE operations.
	return AT_TY_ID_UNKNOWN;
    }
    
    if (MTYPE_is_tie(type)) {
	const char *name = MTYPE_name(type);
	AT_TIE_TYPE *tie_type = AT_Tie_Info()->find_type(name);
	Is_True(tie_type != NULL,
		("Can't find AT_TIE_TYPE object for %s", name));
	return get_at_ty_id(tie_type, vl, flags);
    }
    
    ATYPE atype = mtype_to_atype(type);
    if (atype == ATYPE_UNKNOWN) {
	return AT_TY_ID_UNKNOWN;
    }

    AT_TY *ty = XT_New(pool()) AT_TY(ty_tab(), atype, vl, flags);
    return get_at_ty_id(ty);
}
    

AT_OP_ID
AT_FACTORY::get_at_op_id (AT_OP *temp_op)
{
  AT_OP *at_op = op_tab()->Add_Op(temp_op);
  Is_True(at_op, ("Couldn't add op (%s) to the op table!", temp_op->Name()));
  
  return at_op->Id();
}


AT_OP_ID
AT_FACTORY::get_at_op_id (const WN *wn) {
    OPERATOR oper = WN_operator(wn);
    switch (oper) {
    case OPR_ABS:
    case OPR_ADD:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BNOR:
    case OPR_BNOT:
    case OPR_BXOR:
    case OPR_CAND:
    case OPR_CIOR:
    case OPR_CVT:
    case OPR_LIOR:
    case OPR_LNOT:
    case OPR_LAND:
    case OPR_MADD:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MPY:
    case OPR_MSUB:
    case OPR_NEG:
    case OPR_SUB:
	return get_at_op_id_general(wn);

    case OPR_EQ:
    case OPR_GE:
    case OPR_GT:
    case OPR_LE:
    case OPR_LT:
    case OPR_NE:
	return get_at_op_id_compare(wn);

    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_SHL:
    case OPR_DIV:
	return get_at_op_id_shift(wn);

    case OPR_CVTL:
	return get_at_op_id_cvtl(wn);

    case OPR_ILOAD:
    case OPR_LDID:
	return get_at_op_id_load(wn);

    case OPR_ISTORE:
    case OPR_STID:
	return get_at_op_id_store(wn);
	
    case OPR_INTRINSIC_CALL:
    case OPR_INTRINSIC_OP:
	return get_at_op_id_intrinsic(wn);

    default:
	return AT_OP_ID_UNKNOWN;
    }
}


AT_OP_ID
AT_FACTORY::get_at_op_id_general (const WN *wn)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    OPERATOR oper = WN_operator(wn);
    ATOP atop = opr_to_atop(oper);
    Is_True(atop != ATOP_UNKNOWN,
	    ("Can't find ATOP for operator %s", OPERATOR_name(oper)));
    op->Set_Atop(atop);
    
    AT_TY_ID ty_id = get_at_ty_id(wn_decode_func()(wn).rtype,
				  default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);
    
    for (INT i = 0; i < WN_kid_count(wn); i++) {
	const WN *kid = WN_kid(wn, i);
	TYPE_ID kid_type = canonical_kid_type(wn, i, wn_decode_func());
	ty_id = get_at_ty_id(kid_type, default_vl(), AT_TY::NONE);
	param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
	op->Add_Param(param);
    }
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_compare (const WN *wn)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    OPERATOR oper = WN_operator(wn);
    ATOP atop = opr_to_atop(oper);
    Is_True(atop != ATOP_UNKNOWN,
	    ("Can't find ATOP for operator %s", OPERATOR_name(oper)));
    op->Set_Atop(atop);

    // boolean result
    AT_TY_ID ty_id = get_at_ty_id(MTYPE_XTBOOL, default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);

    // vector inputs
    ty_id = get_at_ty_id(wn_decode_func()(wn).rtype, default_vl(), AT_TY::NONE);
    
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);

    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_shift (const WN *wn)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    OPERATOR oper = WN_operator(wn);
    ATOP atop = (oper == OPR_DIV) ? ATOP_LSHR : opr_to_atop(oper);
    Is_True(atop != ATOP_UNKNOWN,
	    ("Can't find ATOP for operator %s", OPERATOR_name(oper)));
    
    AT_TY_ID ty_id = get_at_ty_id(wn_decode_func()(wn).rtype,
				  default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);
    
    TYPE_ID kid_type = canonical_kid_type(wn, 0, wn_decode_func());
    ty_id = get_at_ty_id(kid_type, default_vl(), AT_TY::NONE);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);
    
    // shift amount
    ty_id = get_at_ty_id(MTYPE_I4, 1, AT_TY::NONE);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    WN *shift_amount = WN_kid1(wn);
    if (WN_operator(shift_amount) == OPR_INTCONST) {
	INT64 val = WN_const_val(shift_amount);
	INT64 sa = val;
	if (oper == OPR_DIV) {
	    if (val <= 0 || !IS_POWER_OF_TWO(val)) {
		return AT_OP_ID_UNKNOWN;
	    }
	    sa = 0;
	    while ( (val & 0x1) != 0x1) {
		val >>= 1;
		sa++;
	    }
	}
	if (sa >= 0 && sa <= 64) {
	    XT_BIGNUM *at_val = XT_New(pool()) XT_BIGNUM(32, sa);
	    param->Set_Kind(AT_PARAM_KIND_IMM);
	    param->Set_Imm_Value(at_val);
	    atop = AT_atop_immediate_version(atop);
	}
    }
    op->Add_Param(param);
    
    op->Set_Atop(atop);
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_cvtl (const WN *wn)
{
  /* Vector CVTLs are preprocessed so the type information is
     attached to their e_infos. */
  if (default_vl() > 1)
    return get_at_op_id_general(wn);
  
  XT_MEMPOOL::mark popper(pool());
  
  AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
  OPERATOR oper = WN_operator(wn);
  ATOP atop = opr_to_atop(oper);
  Is_True(atop != ATOP_UNKNOWN,
	  ("Can't find ATOP for operator %s", OPERATOR_name(oper)));
  op->Set_Atop(atop);
  
  /* The result type for the cvtl is derived from the sign of the
     actual rtype, and the bit_size of the cvtl operator. There
     doesn't seem to be an existing api to go from the cvtl size to
     the correspoing mtype, so we have to do the lookup ourselves. */
  TYPE_ID rtype;
  switch (WN_cvtl_bits(wn)) {
  case 8:
    rtype = MTYPE_I1;
    break;
  case 16:
    rtype = MTYPE_I2;
    break;
  case 32:
    rtype = MTYPE_I4;
    break;
  case 64:
    rtype = MTYPE_I8;
    break;
  default:
    FmtAssert(false, ("unexpected OPR_CVTL with cvtl_bits = %d", WN_cvtl_bits(wn)));
  }
  
  rtype = Mtype_TransferSign(wn_decode_func()(wn).rtype, rtype);
  
  AT_TY_ID ty_id = get_at_ty_id(rtype, default_vl(), AT_TY::NONE);
  AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
  op->Add_Param(param);
  
  TYPE_ID kid_type = canonical_kid_type(wn, 0, wn_decode_func());
  ty_id = get_at_ty_id(kid_type, default_vl(), AT_TY::NONE);
  param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
  op->Add_Param(param);
  
  return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_load (const WN *wn,
			       bool scalar_mem, bool indexed, bool updating,
			       INT offset)
{
    Is_True(WN_operator(wn) == OPR_ILOAD ||
	    WN_operator(wn) == OPR_LDID,
	    ("get_at_op_id_load: Load expected, not %s", OPCODE_name(WN_opcode(wn))));
    
    TYPE_ID rtype = wn_decode_func()(wn).rtype;
    TYPE_ID desc = wn_decode_func()(wn).desc;
    
    return get_at_op_id_load(rtype, desc, scalar_mem, indexed,updating,offset);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_load (TYPE_ID rtype, TYPE_ID desc,
			       bool scalar_mem, bool indexed, bool updating,
			       INT offset)
{
    XT_MEMPOOL::mark popper(pool());

    ATOP atop = scalar_mem ? ATOP_LOADS_I : ATOP_LOAD_I;
    if (indexed)
	atop = AT_atop_indexed_version(atop);
    if (updating)
	atop = AT_atop_updating_version(atop);
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(atop);
    
    INT reg_vl = default_vl();
    INT mem_vl = scalar_mem ? 1 : default_vl();
    
    AT_TY_ID ty_id = get_at_ty_id(rtype, reg_vl, AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);
    
    // address register
    ty_id = get_at_ty_id(desc, mem_vl, AT_TY::POINTER);
    AT_PARAM_KIND kind = updating ? AT_PARAM_KIND_REG_INOUT : AT_PARAM_KIND_REG_IN;
    param = XT_New(pool()) AT_PARAM(ty_tab(), kind, ty_id);
    op->Add_Param(param);
    

    // Offset (immediate or index register). Indexed loads use an int32 index register.
    ty_id = indexed ? get_at_ty_id(MTYPE_I4, 1, AT_TY::NONE) : ty_id;
    XT_BIGNUM *at_val = NULL;
    if (indexed) {
	kind = AT_PARAM_KIND_REG_IN;
    } else if (offset) {
	kind = AT_PARAM_KIND_IMM;
	at_val = XT_New(pool()) XT_BIGNUM(32, offset);
    } else {
	kind = AT_PARAM_KIND_IMM_OFFSET;
    }
    param = XT_New(pool()) AT_PARAM(ty_tab(), kind, ty_id);
    if (at_val != NULL)
      param->Set_Imm_Value(at_val);
    op->Add_Param(param);
    
    return get_at_op_id(op);
}

AT_OP_ID
AT_FACTORY::get_at_op_id_store (const WN *wn, bool indexed, bool updating,
				INT offset) {
    Is_True(WN_operator(wn) == OPR_ISTORE ||
	    WN_operator(wn) == OPR_STID,
	    ("get_at_op_id_store: Store expected, not %s", OPCODE_name(WN_opcode(wn))));
    
    XT_MEMPOOL::mark popper(pool());
    
    ATOP   atop = ATOP_STORE_I;
    if (indexed) {
	atop = AT_atop_indexed_version(atop);
    }
    if (updating) {
	atop = AT_atop_updating_version(atop);
    }
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(atop);
    
    TYPE_ID val_type = canonical_kid_type(wn, 0, wn_decode_func());
    AT_TY_ID ty_id = get_at_ty_id(val_type, default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);
    
    ty_id = get_at_ty_id(wn_decode_func()(wn).desc, default_vl(), AT_TY::POINTER);
    AT_PARAM_KIND kind = updating ? AT_PARAM_KIND_REG_INOUT : AT_PARAM_KIND_REG_IN;
    param = XT_New(pool()) AT_PARAM(ty_tab(), kind, ty_id);
    op->Add_Param(param);
    
    // Immediate or index offset.
    XT_BIGNUM *at_val = NULL;
    ty_id = indexed ? get_at_ty_id(MTYPE_I4, 1, AT_TY::NONE) : ty_id;
    if (indexed) {
	kind = AT_PARAM_KIND_REG_IN;
    } else if (offset) {
	kind = AT_PARAM_KIND_IMM;
	at_val = XT_New(pool()) XT_BIGNUM(32, offset);
    } else {
	kind = AT_PARAM_KIND_IMM_OFFSET;
    }
    param = XT_New(pool()) AT_PARAM(ty_tab(), kind, ty_id);
    if (at_val != NULL)
      param->Set_Imm_Value(at_val);
    op->Add_Param(param);
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_prime (const WN *wn, bool store_flush) {
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_ILOAD || oper == OPR_ISTORE,
	    ("get_at_op_id_prime: expected ILOAD/ISTORE, got %s",
	     OPERATOR_name(oper)));
    
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    
    bool is_load = (oper == OPR_ILOAD);
    op->Set_Atop((is_load) ? ATOP_LOADA_P : 
		 ((store_flush) ? ATOP_STOREA_F : ATOP_STOREA_P));
    
    // Alignment register
    AT_PARAM_KIND param_kind = store_flush ? AT_PARAM_KIND_REG_IN : AT_PARAM_KIND_REG_OUT;
    AT_TY_ID ty_id = get_at_ty_id(wn_decode_func()(wn).rtype,
				  default_vl(), AT_TY::ALIGN);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), param_kind, ty_id);
    op->Add_Param(param);
    
    if (is_load || store_flush) {
	// address register -- priming store doesn't need it
	param_kind = is_load ? AT_PARAM_KIND_REG_INOUT : AT_PARAM_KIND_REG_IN;
	ty_id = get_at_ty_id(wn_decode_func()(wn).desc, default_vl(), AT_TY::POINTER);
	param = XT_New(pool()) AT_PARAM(ty_tab(), param_kind, ty_id);
	op->Add_Param(param);
    }
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_alignment (const WN *wn, bool updating) {
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_ILOAD || oper == OPR_ISTORE,
	    ("get_at_op_id_alignment: expected ILOAD/ISTORE, got %s",
	     OPERATOR_name(oper)));

    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    
    bool is_load = oper == OPR_ILOAD;
    ATOP atop = (is_load) ? 
	((updating) ? ATOP_LOADA_IU : ATOP_LOADA_I) :
	((updating) ? ATOP_STOREA_IU : ATOP_STOREA_I); 
    op->Set_Atop(atop);

    // vector register
    AT_PARAM_KIND param_kind = is_load ? AT_PARAM_KIND_REG_OUT : AT_PARAM_KIND_REG_IN;
    AT_TY_ID ty_id = get_at_ty_id(wn_decode_func()(wn).rtype,
				  default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), param_kind, ty_id);
    op->Add_Param(param);

    // alignment register
    ty_id = get_at_ty_id(wn_decode_func()(wn).rtype, default_vl(), AT_TY::ALIGN);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_INOUT, ty_id);
    op->Add_Param(param);
    
    // address register
    param_kind = updating ? AT_PARAM_KIND_REG_INOUT : AT_PARAM_KIND_REG_IN;
    ty_id = get_at_ty_id(wn_decode_func()(wn).desc, default_vl(), AT_TY::POINTER);
    param = XT_New(pool()) AT_PARAM(ty_tab(), param_kind, ty_id);
    op->Add_Param(param);

    // immediate offset
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_IMM_OFFSET, ty_id);
    op->Add_Param(param);
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_intrinsic (const WN *wn) {
    Is_True(WN_operator(wn) == OPR_INTRINSIC_CALL ||
	    WN_operator(wn) == OPR_INTRINSIC_OP,
	    ("Expected intrinsic call or op, not %s",
	     OPERATOR_name(WN_operator(wn))));
    
    XT_MEMPOOL::mark popper(pool());
    
    INTRINSIC intrn = (INTRINSIC) WN_intrinsic(wn);
    if (!INTRN_is_tie_intrinsic(intrn)) {
	return AT_OP_ID_UNKNOWN;
    }
    
    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_TIE_PROTO);
    op->Set_Tie_Proto(AT_Tie_Info()->find_proto(tie_macro->name()));

    
    INT vl = default_vl();
    
    for (INT i = 0; i < tie_macro->num_protos(); i++) {
	INT param_vl = vl;
	AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab());
	AT_PARAM_KIND param_kind = AT_PARAM_KIND_UNKNOWN;
	if (tie_macro->proto_is_immed(i)) {
	    param_kind = AT_PARAM_KIND_IMM;
	    param_vl = 1;  // no vector immediates
	    
	    // find the constant parameter value and request it;
	    // this generates separate AT_OPs for different immediate values but
	    // that's ok because autotie will take care of that
	    INT parm_idx = tie_macro->proto_to_whirl_index(i);
	    Is_True(parm_idx >= 0 && parm_idx < WN_kid_count(wn),
		    ("Invalid TIE parm index %d.", parm_idx));
	    WN *parm = WN_kid(wn, parm_idx);
	    Is_True(WN_operator(parm) == OPR_PARM && WN_kid_count(parm) == 1,
		    ("Expected OPR_PARM node!"));
	    WN *imm = WN_kid0(parm);
	    if (WN_operator(imm) == OPR_INTCONST) {
		INT64 val = WN_const_val(imm);
		INT val_bit_size = MTYPE_bit_size(WN_rtype(imm));
		XT_BIGNUM *at_val = XT_New(pool()) XT_BIGNUM(val_bit_size, val);
		param->Set_Imm_Value(at_val);
	    }
	} else if (tie_macro->proto_is_in(i)) {
	    param_kind = AT_PARAM_KIND_REG_IN;
	} else if (tie_macro->proto_is_out(i)) {
	    param_kind = AT_PARAM_KIND_REG_OUT;
	} else if (tie_macro->proto_is_inout(i)) {
	    param_kind = AT_PARAM_KIND_REG_INOUT;
	} else {
	    return AT_OP_ID_UNKNOWN;
	}
	
	param->Set_Kind(param_kind);
	TYPE_ID proto_mtype = tie_macro->proto_mtype_id(tie_info, i);
	AT_TY_ID ty_id = get_at_ty_id(proto_mtype, param_vl, AT_TY::NONE);
	param->Set_Type(ty_tab()->Get_Type(ty_id));
	op->Add_Param(param);
    }

    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_vselect (bool dual, TYPE_ID scalar_type,
				  UINT64 sel_val, INT sel_bits)
{
    XT_MEMPOOL::mark popper(pool());

    if (sel_bits > 64)
      sel_bits = 64;
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(),
                                     dual ? AT_OP_KIND_ATOP_PROTO : AT_OP_KIND_ATOP_OP);
    op->Set_Atop(dual ? ATOP_VDSEL : ATOP_VSEL);
    
    AT_TY_ID ty_id = get_at_ty_id(scalar_type, default_vl(), AT_TY::NONE);

    int outs = dual ? 2 : 1;
    for (INT i = 0; i < outs; i++)
    {
        AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
	op->Add_Param(param);
    }
    
    for (INT i = 0; i < 2; i++) {
        AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
	op->Add_Param(param);
    }
    
    ty_id = get_at_ty_id(scalar_type, 1, AT_TY::SELECT);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_IMM, ty_id);
    param->Set_Imm_Value(XT_New(pool()) XT_BIGNUM(sel_bits, sel_val));
    op->Add_Param(param);

    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_convert (AT_TY_ID from_type, AT_TY_ID to_type)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(ATOP_CVT);
    
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, to_type);
    op->Add_Param(param);
    
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, from_type);
    op->Add_Param(param);
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_mac (const WN *wn) {
    
    OPERATOR oper = WN_operator(wn);
    Is_True(((oper == OPR_ADD || oper == OPR_SUB) &&
	     WN_operator(WN_kid1(wn)) == OPR_MPY),
	    ("Bad operation to form MAC from."));
    
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(oper == OPR_ADD ? ATOP_MADD : ATOP_MSUB);

    WN *mpy_kid = WN_kid1(wn);

    AT_TY_ID ty_id = get_at_ty_id(wn_decode_func()(wn).rtype,
				  default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_INOUT, ty_id);
    op->Add_Param(param);
    
    for (INT i = 0; i < 2; i++) {
	TYPE_ID kid_type = canonical_kid_type(mpy_kid, i, wn_decode_func());
	ty_id = get_at_ty_id(kid_type, default_vl(), AT_TY::NONE);
	param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
	op->Add_Param(param);
    }
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_radd (TYPE_ID scalar_type)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(ATOP_RADD);
    
    AT_TY_ID ty_id = get_at_ty_id(scalar_type, 1, AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);

    ty_id = get_at_ty_id(scalar_type, default_vl(), AT_TY::NONE);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_rminmax (TYPE_ID scalar_type, bool is_min)
{
  XT_MEMPOOL::mark popper(pool());
  
  AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
  op->Set_Atop(is_min ? ATOP_RMIN : ATOP_RMAX);
    
  AT_TY_ID ty_id = get_at_ty_id(scalar_type, 1, AT_TY::NONE);
  AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
  op->Add_Param(param);
  
  ty_id = get_at_ty_id(scalar_type, default_vl(), AT_TY::NONE);
  param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
  op->Add_Param(param);
  
  return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_bxor (TYPE_ID scalar_type)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    op->Set_Atop(ATOP_BXOR);
    
    AT_TY_ID ty_id = get_at_ty_id(scalar_type, default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
    op->Add_Param(param);
    
    for (INT i = 0; i < 2; i++) {
        param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
 	op->Add_Param(param);
    }
    
    return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_zero (TYPE_ID scalar_type)
{
  XT_MEMPOOL::mark popper(pool());
    
  AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
  op->Set_Atop(ATOP_ZERO);
  
  AT_TY_ID ty_id = get_at_ty_id(scalar_type, default_vl(), AT_TY::NONE);
  AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_OUT, ty_id);
  op->Add_Param(param);
  
  return get_at_op_id(op);
}


AT_OP_ID
AT_FACTORY::get_at_op_id_cmov (TYPE_ID desc)
{
    XT_MEMPOOL::mark popper(pool());
    
    AT_OP *op = XT_New(pool()) AT_OP(pool(), op_tab(), AT_OP_KIND_ATOP_OP);
    
    ATOP atop = ATOP_MOVT;
    op->Set_Atop(atop);
    
    // vector input/output register
    AT_TY_ID ty_id = get_at_ty_id(desc, default_vl(), AT_TY::NONE);
    AT_PARAM *param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_INOUT, ty_id);
    op->Add_Param(param);
    
    // vector input register
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);
    
    // boolean register
    ty_id = get_at_ty_id(MTYPE_XTBOOL, default_vl(), AT_TY::NONE);
    param = XT_New(pool()) AT_PARAM(ty_tab(), AT_PARAM_KIND_REG_IN, ty_id);
    op->Add_Param(param);

    return get_at_op_id(op);
}

ATYPE
AT_FACTORY::mtype_to_atype (TYPE_ID type) {
    switch (type) {
    case MTYPE_I1: return ATYPE_I1;
    case MTYPE_I2: return ATYPE_I2;
    case MTYPE_I4: return ATYPE_I4;
    case MTYPE_I8: return ATYPE_I8;
    case MTYPE_U1: return ATYPE_U1;
    case MTYPE_U2: return ATYPE_U2;
    case MTYPE_U4: return ATYPE_U4;
    case MTYPE_U8: return ATYPE_U8;
    case MTYPE_F4: return ATYPE_F4;
    case MTYPE_F8: return ATYPE_F8;
    case MTYPE_XTBOOL: return ATYPE_B1;
    case MTYPE_XTBOOL2: return ATYPE_B2;
    case MTYPE_XTBOOL4: return ATYPE_B4;
    case MTYPE_XTBOOL8: return ATYPE_B8;
    case MTYPE_XTBOOL16: return ATYPE_B16;
    default: return ATYPE_UNKNOWN;
    }
}


TYPE_ID
AT_FACTORY::atype_to_mtype (TYPE_ID type) {
    switch (type) {
    case ATYPE_I1: return MTYPE_I1;
    case ATYPE_I2: return MTYPE_I2;
    case ATYPE_I4: return MTYPE_I4;
    case ATYPE_I8: return MTYPE_I8;
    case ATYPE_U1: return MTYPE_U1;
    case ATYPE_U2: return MTYPE_U2;
    case ATYPE_U4: return MTYPE_U4;
    case ATYPE_U8: return MTYPE_U8;
    case ATYPE_F4: return MTYPE_F4;
    case ATYPE_F8: return MTYPE_F8;
    case ATYPE_B1: return MTYPE_XTBOOL;
    case ATYPE_B2: return MTYPE_XTBOOL2;
    case ATYPE_B4: return MTYPE_XTBOOL4;
    case ATYPE_B8: return MTYPE_XTBOOL8;
    case ATYPE_B16: return MTYPE_XTBOOL16;
    default: return MTYPE_UNKNOWN;
    }
}


ATOP
AT_FACTORY::opr_to_atop (OPERATOR oper) {
    switch (oper) {
    case OPR_ABS: return ATOP_ABS;
    case OPR_ADD: return ATOP_ADD;
    case OPR_ASHR: return ATOP_ASHR;
    case OPR_BAND: return ATOP_BAND;
    case OPR_BIOR: return ATOP_BIOR;
    case OPR_BNOR: return ATOP_BNOR;
    case OPR_BNOT: return ATOP_BNOT;
    case OPR_BXOR: return ATOP_BXOR;
    case OPR_CAND: return ATOP_CAND;
    case OPR_CIOR: return ATOP_CIOR;
    case OPR_CVT: return ATOP_CVT;
    case OPR_CVTL: return ATOP_CVT;
    case OPR_EQ: return ATOP_EQ;
    case OPR_GE: return ATOP_GE;
    case OPR_GT: return ATOP_GT;
    case OPR_ILOAD: return ATOP_LOAD_I;
    case OPR_ISTORE: return ATOP_STORE_I;
    case OPR_LAND: return ATOP_LAND;
    case OPR_LDID: return ATOP_LOAD_I;
    case OPR_LE: return ATOP_LE;
    case OPR_LIOR: return ATOP_LIOR;
    case OPR_LNOT: return ATOP_LNOT;
    case OPR_LSHR: return ATOP_LSHR;
    case OPR_LT: return ATOP_LT;
    case OPR_MADD: return ATOP_MADD;
    case OPR_MAX: return ATOP_MAX;
    case OPR_MIN: return ATOP_MIN;
    case OPR_MPY: return ATOP_MPY;
    case OPR_MSUB: return ATOP_MSUB;
    case OPR_NE: return ATOP_NE;
    case OPR_NEG: return ATOP_NEG;
    case OPR_SHL: return ATOP_SHL;
    case OPR_STID: return ATOP_STORE_I;
    case OPR_SUB: return ATOP_SUB;
    default: return ATOP_UNKNOWN;
    }
}


tf_node_kind_t
AT_FACTORY::opr_to_node_kind (OPERATOR oper)
{
  switch (oper) {
  case OPR_ABS: return TFN_ABS;
  case OPR_ADD: return TFN_ADD;
  case OPR_ASHR: return TFN_ASHR;
  case OPR_BAND: return TFN_BAND;
  case OPR_BIOR: return TFN_BOR;
  case OPR_BNOR: return TFN_BNOR;
  case OPR_BNOT: return TFN_BNOT;
  case OPR_BXOR: return TFN_BXOR;
  case OPR_DIV: return TFN_LSHR;
  case OPR_EQ: return TFN_EQ;
  case OPR_LAND: return TFN_LAND;
  case OPR_LIOR: return TFN_LOR;
  case OPR_LNOT: return TFN_LNOT;
  case OPR_LSHR: return TFN_LSHR;
  case OPR_NE: return TFN_NEQ;
  case OPR_NEG: return TFN_NEG;
  case OPR_SHL: return TFN_SHL;
  case OPR_SUB: return TFN_SUB;
  default: return TFN_UNKNOWN;
  }
}


// Try to promote MPY's kids to the type of the larger kid, or to the MPY's result type
// if that's unsafe.
// E.g.
//  I4MPY(I1,I2) returns I2
//  I4MPY(U1,I2) returns I2
//  I4MPY(I1,U2) returns I4
//  I4MPY(I2,U2) returns I4

static TYPE_ID
mpy_canonical_kid_type (const WN *wn, AT_WN_DECODE_FUNC wn_decode_func) {
    
    TYPE_ID from_type0 = wn_decode_func(WN_kid0(wn)).rtype;
    TYPE_ID from_type1 = wn_decode_func(WN_kid1(wn)).rtype;
    
    if (from_type0 == from_type1) {
	return from_type0;
    }
    
    TYPE_ID res_type = wn_decode_func(wn).rtype;
    
    INT size0 = MTYPE_bit_size(from_type0);
    INT size1 = MTYPE_bit_size(from_type1);
    
    if (MTYPE_is_signed(from_type0) != MTYPE_is_signed(from_type1)) {
	if (size0 < size1 && MTYPE_is_signed(from_type1)) {
	    return from_type1;
	}
	if (size0 > size1 && MTYPE_is_signed(from_type0)) {
	    return from_type0;
	}
	return res_type;
    }
    
    if (size0 < size1) {
	return from_type1;
    }

    Is_True(size0 > size1,
	    ("Expected size0 %d > size1 %d of the kid types.", size0, size1));
    
    return from_type0;
}


// Try to promote a compare's kids to the type of the larger kid.
static TYPE_ID
cmp_canonical_kid_type (const WN *wn, AT_WN_DECODE_FUNC wn_decode_func) {
    TYPE_ID from_type0 = wn_decode_func(WN_kid0(wn)).rtype;
    TYPE_ID from_type1 = wn_decode_func(WN_kid1(wn)).rtype;
    TYPE_ID cmp_type   = WN_desc(wn);

    INT size0 = MTYPE_bit_size(from_type0);
    INT size1 = MTYPE_bit_size(from_type1);
    TYPE_ID canon_type = (size0>size1) ? from_type0 : from_type1;

    if (MTYPE_bit_size(cmp_type) < MTYPE_bit_size(canon_type)) {
	canon_type = cmp_type;
    }

    return Mtype_TransferSign(cmp_type, canon_type);
}


static TYPE_ID
cvt_canonical_kid_type (const WN *wn, AT_WN_DECODE_FUNC wn_decode_func)
{
  TYPE_ID type = wn_decode_func(wn).rtype;
  TYPE_ID kid_type = wn_decode_func(WN_kid0(wn)).rtype;

  if (!MTYPE_is_integral(type) || !MTYPE_is_integral(kid_type))
    return kid_type;
  
  if (MTYPE_is_signed(type) == MTYPE_is_signed(kid_type))
    return kid_type;
  
  INT type_bit_size = MTYPE_bit_size(type);
  INT kid_type_bit_size = MTYPE_bit_size(kid_type);

  if (type_bit_size <= kid_type_bit_size)
    return kid_type;
  
  /* To convert from a narrow to a wide type and change sign,
     we need to convert from the narrow to the wide type without
     changing the sign first. */
  return Mtype_TransferSign(kid_type, type);
}


TYPE_ID
AT_FACTORY::canonical_kid_type (const WN *wn, INT kid_idx,
				AT_WN_DECODE_FUNC wn_decode_func)
{
  WN *kid = WN_kid(wn, kid_idx);
  TYPE_ID kid_type = wn_decode_func(kid).rtype;
  if (!MTYPE_is_integral(kid_type)) {
    return kid_type;
  }
  
  TYPE_ID wn_rtype = wn_decode_func(wn).rtype;
  
  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_CVT || oper == OPR_CVTL) {
    TYPE_ID new_type = cvt_canonical_kid_type(wn, wn_decode_func);
    return new_type;
  }

  if (oper == OPR_ABS || oper == OPR_NEG || oper == OPR_BNOT) {
    return wn_rtype;
  }
  
  if (OPERATOR_is_load(oper) ||
      (oper != OPR_STID && oper != OPR_PARM && WN_kid_count(wn) == 1)) {
    return kid_type;
  }
  
  if (oper == OPR_INTRINSIC_CALL || oper == OPR_INTRINSIC_OP) {
    return kid_type;
  }
  
  if (kid_idx == 1 &&
      (OPERATOR_is_store(oper) || oper == OPR_DIV ||
       oper == OPR_ASHR || oper == OPR_LSHR || oper == OPR_SHL)) {
    return kid_type;
  }
  
  if (oper == OPR_MPY) {
    TYPE_ID new_type = mpy_canonical_kid_type(wn, wn_decode_func);
    return new_type;
  }
  
  if (OPERATOR_is_compare(oper)) {
    TYPE_ID new_type = cmp_canonical_kid_type(wn, wn_decode_func);
    return new_type;
  }
  
  if (!MTYPE_is_integral(wn_rtype)) {
    return kid_type;
  }
  
  return wn_rtype;
}


AT_WN_DECODE
at_wn_decode_regular (const WN *wn) {
    AT_WN_DECODE wn_decode = {
	OPERATOR_is_store(WN_operator(wn)) ? WN_desc(wn) : WN_rtype(wn),
	WN_desc(wn)
    };
    return wn_decode;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
