
// simd_loop_at.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Auto TIE SIMD                                                          *
 *                                                                           *
 *    Loop transformation routines                                           *
 *                                                                           *
 *---------------------------------------------------------------------------*/


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

// $Id: simd_loop_at.cxx $

#include <math.h>

#include "intrn_info.h"
#include "opt_du.h"
#include "simd_at.h"
#include "simd_imem.h"
#include "simd_loop_at.h"
#include "simd_ti.h"
#include "tie.h"
#include "tietypes.h"
#include "wintrinsic.h"
#include "wutil.h"
#include "simd.h"
#include "simd_if.h"
#include "config_targ_options.h"


//
// GUARD_RANGE
//


GUARD_RANGE::GUARD_RANGE (bool is_signed, INT bit_size)
{
  if (is_signed) {
    _flags = F_SIGNED;
    _minv = - (1 << (bit_size - 1));
    _maxv = (1 << (bit_size - 1)) - 1;
  } else {
    _flags = F_UNSIGNED;
    _minv = 0;
    _maxv = (1 << bit_size) - 1;
  }
}


void
GUARD_RANGE::union_range (const GUARD_RANGE &range)
{
  _flags |= range._flags;
  _minv = MIN(_minv, range._minv);
  _maxv = MAX(_maxv, range._maxv);
}


void
GUARD_RANGE::max_range (const GUARD_RANGE &range)
{
  _flags |= range._flags;
  _minv = MAX(_minv, range._minv);
  _maxv = MAX(_maxv, range._maxv);
}


void
GUARD_RANGE::min_range (const GUARD_RANGE &range)
{
  _flags |= range._flags;
  _minv = MIN(_minv, range._minv);
  _maxv = MIN(_maxv, range._maxv);
}


void
GUARD_RANGE::ashr_range (INT amount)
{
  _minv >>= amount;
  _maxv >>= amount;
}


void
GUARD_RANGE::lshr_range (INT amount)
{
  _minv = (_minv < 0) ? 0 : _minv >> amount;
  _maxv = (_maxv < 0) ? 0 : _maxv >> amount;
}


void
GUARD_RANGE::shl_range (INT amount)
{
  _minv <<= amount;
  _maxv <<= amount;
}


void
GUARD_RANGE::abs_range (void)
{
  INT64 tmin = _minv;
  INT64 tmax = _maxv;
  if (_maxv < 0) {
    _minv = -tmax;
    _maxv = -tmin;
  } else if (_minv < 0) {
    _minv = 0;
    _maxv = MAX(-tmin, tmax);
  }
}


void
GUARD_RANGE::negate_range (void)
{
  INT64 tmin = _minv;
  INT64 tmax = _maxv;
  _minv = -tmax;
  _maxv = -tmin;
}


void
GUARD_RANGE::add_range (const GUARD_RANGE &range)
{
  _flags |= range._flags;
  _minv += range._minv;
  _maxv += range._maxv;
}


void
GUARD_RANGE::sub_range (const GUARD_RANGE &range)
{
  _flags |= range._flags;
  _minv -= range._maxv;
  _maxv -= range._minv;
}


bool
GUARD_RANGE::contains (const GUARD_RANGE &range) const
{
  return (_minv <= range._minv && _maxv >= range._maxv);
}


INT
GUARD_RANGE::guards (INT type_bits) const
{
  bool signed_range = is_signed() || _minv < 0;
  for (INT bits = type_bits; bits < 32; bits++) {
    GUARD_RANGE range(signed_range, bits);
    if (range.contains(*this)) {
      return bits - type_bits;
    }
  }
  
  return 32 - type_bits;
}


//
// GUARD_INFO
//


void
GUARD_INFO::update_guards (void)
{
  INT guards = range().guards(type_bits());
  if (type_bits() == 8)
    _int8_guards = MAX(_int8_guards, guards);
  else {
    Is_True(type_bits() == 16, ("Unexpected type bit size %d", type_bits()));
    _int16_guards = MAX(_int16_guards, guards);
  }
}


//
// SIMD_LOOP_AT
//


SIMD_LOOP_AT::SIMD_LOOP_AT(MEM_POOL *pool, LOOP_MODEL *lm,
			   WN *simd_loop, INT simd_loop_level) :
  SIMD_LOOP(pool,lm,simd_loop,simd_loop_level),
  _vector_lengths(NULL), _v_unroll_factor(0),
  _req_int8_guards(0), _req_int16_guards(0)
{
  if (simd_debug) {
    fprintf(TFile, "*** New SIMD_LOOP_AT ***\n");
  }
  
  _at_analysis = Simd_Info->AT_Analysis_Phase();
  _at_transform = !_at_analysis;
  if (_at_analysis) {
    Set_V_Unroll_Factor(2);
  }

  Set_Max_Vector_Length(SIMD_TI::MAX_VECTOR_LENGTH);
  Simd_Ti->Set_Default_Vl(0);
}


// returns the possibly narrowed return type of "wn"
// based on 'bit_size' and wn's current rtype and desc
TYPE_ID
SIMD_LOOP_AT::Get_Res_Type(const WN *wn, INT bit_size) {
  OPERATOR oper = WN_operator(wn);
  
  TYPE_ID rtype = (oper == OPR_PARM) ? Parm_Type(wn) : WN_rtype(wn);
  TYPE_ID desc = WN_desc(wn);
  TYPE_ID res_type = (rtype == MTYPE_V) ? desc : rtype;
  
  if (OPERATOR_is_compare(WN_operator(wn))) {
    res_type = desc;
  }
  
  if (!MTYPE_is_integral(res_type)) {
    return res_type;
  }
  if (MTYPE_bit_size(res_type) <= bit_size) {
    return res_type;
  }
  res_type = Mtype_TransferSign(res_type, Bit_Size_To_Int(bit_size));
  return res_type;
}


void
SIMD_LOOP_AT::Screen_Scalar_To_Vector_Conversion (TYPE_ID scalar_type)
{
  BIT_VECTOR *vls_type = Simd_Ti->Vector_Lengths(scalar_type);
  if (!vls_type) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, Simd_Loop(), SIMD_MTYPE_Msg(scalar_type));
    return;
  }
  
  BIT_VECTOR *vls = Vector_Lengths();
  *vls &= *vls_type;
  if (vls->Pop_Count() == 0) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, Simd_Loop(), SIMD_MTYPE_Msg(scalar_type));
    return;
  }
  
  for (INT vl=vls->Least_Non_Zero(); vl<vls->Size(); vl++) {
    if (!vls->Test(vl)) {
      continue;
    }
    
    TYPE_ID vector_type = Simd_Ti->Simd_Reg_Type_Of_Scalar(scalar_type, vl);
    if (vector_type == MTYPE_UNKNOWN ||
        !Simd_Ti->Type_Conversion_Possible(scalar_type, vector_type)) {
      vls->Reset(vl);
    }
  }
  
  if (vls->Pop_Count() == 0) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_SCALAR_CONV, Simd_Loop(), SIMD_MTYPE_Msg(scalar_type));
    return;
  }
}


void
SIMD_LOOP_AT::Screen_Type (TYPE_ID res_type, WN *wn)
{
  BIT_VECTOR *vls_type = Simd_Ti->Vector_Lengths(res_type);
  if (!vls_type) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(res_type));
    return;
  }
  
  BIT_VECTOR *vls = Vector_Lengths();
  *vls &= *vls_type;
  if (vls->Pop_Count() == 0) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(res_type));
    return;
  }  

  /* Collect 'res_type' to check it needs any guard bits. */
  Collect_Guard_Type(res_type);
}


void
SIMD_LOOP_AT::Screen_Operator (WN *wn, SIMD_EINFO *e_info)
{
  OPERATOR oper = WN_operator(wn);
  
  if (oper == OPR_PARM || oper == OPR_OUTPART) {
    return;
  }
  
  TYPE_ID res_type = e_info->Res_Type();
  if (OPERATOR_is_compare(oper)) {
    res_type = MTYPE_XTBOOL;
    Screen_Type(res_type, wn);
#if 0
    /* necessary canonicalization */
    switch (oper) {
    case OPR_NE: oper = OPR_EQ;
      break;
    case OPR_GT: oper = OPR_LE;
      break;
    case OPR_GE: oper = OPR_LT;
      break;
    }
#endif
  }
  
  BIT_VECTOR *vls = Vector_Lengths();
  for (INT vl = vls->Least_Non_Zero(); vl < vls->Size(); vl++) {
    if (!vls->Test(vl)) {
      continue;
    }

    TYPE_ID rtype_scalar = res_type;
    
    TYPE_ID rtype = Simd_Ti->Simd_Reg_Type_Of_Scalar(rtype_scalar, vl);
    Is_True(rtype != MTYPE_UNKNOWN,
	    ("Expected to find SIMD type for %s.", MTYPE_name(rtype_scalar)));

    for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++) {
      WN *kid = WN_kid(wn, kid_idx);
      TYPE_ID from_desc_scalar = AT_WN_Decode_SIMD_EINFO(kid).rtype;
      TYPE_ID to_desc_scalar = AT_FACTORY::canonical_kid_type(wn, kid_idx,
							      &AT_WN_Decode_SIMD_EINFO);
      if (from_desc_scalar == to_desc_scalar) {
	continue;
      }
      
      Screen_Type(from_desc_scalar, kid);
      if (!vls->Test(vl)) {
	break;
      }
      
      Screen_Type(to_desc_scalar, kid);
      if (!vls->Test(vl)) {
	break;
      }

      TYPE_ID from_desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(from_desc_scalar, vl);
      Is_True(from_desc != MTYPE_UNKNOWN,
	      ("No SIMD type for %s x %d", MTYPE_name(from_desc_scalar), vl));

      /* When converting from a narrow to a wide type and changing the sign
	 at the same time, we need to convert to the complement wide type
	 first. */
      if (MTYPE_is_integral(from_desc_scalar) && MTYPE_is_integral(to_desc_scalar) &&
	  MTYPE_is_signed(from_desc_scalar) != MTYPE_is_signed(to_desc_scalar) &&
	  MTYPE_bit_size(from_desc_scalar) < MTYPE_bit_size(to_desc_scalar)) {
	TYPE_ID new_from_desc_scalar = Mtype_TransferSign(from_desc_scalar, to_desc_scalar);
	
	Screen_Type(new_from_desc_scalar, kid);
	if (!vls->Test(vl)) {
	  break;
	}
	
	TYPE_ID new_from_desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(new_from_desc_scalar, vl);
	Is_True(new_from_desc != MTYPE_UNKNOWN,
		("No SIMD type for %s x %d", MTYPE_name(new_from_desc_scalar), vl));
	
	if (!Simd_Ti->Type_Conversion_Possible(from_desc, new_from_desc)) {
	  vls->Reset(vl);
          if (vls->Pop_Count() == 0) {
            Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_TYPE_CONV, wn,
                             SIMD_MTYPE_Msg(from_desc), SIMD_MTYPE_Msg(new_from_desc));
          }
	  break;
	}

	from_desc = new_from_desc;
	from_desc_scalar = new_from_desc_scalar;
      }
      
      TYPE_ID to_desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(to_desc_scalar, vl);
      Is_True(to_desc != MTYPE_UNKNOWN,
	      ("No SIMD type for %s x %d", MTYPE_name(to_desc_scalar), vl));
      
      if (!Simd_Ti->Type_Conversion_Possible(from_desc, to_desc)) {
	vls->Reset(vl);
        if (vls->Pop_Count() == 0) {
          Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_TYPE_CONV, wn,
                           SIMD_MTYPE_Msg(from_desc), SIMD_MTYPE_Msg(to_desc));
        }
	break;
      }
    }
    
    // It may have been cleared in the kid loop above.
    if (!vls->Test(vl)) {
      continue;
    }
    
    if (OPERATOR_is_load(oper) || OPERATOR_is_store(oper)) {
      // we check load and stores later, in Test_Imem_Alignment
      continue; 
    }
    
    if (oper == OPR_INTRINSIC_OP) {
      INTRINSIC intrn = Tie_Vector_Intrinsic(wn, vl);
      if (intrn == INTRINSIC_INVALID) {
	vls->Reset(vl);
	if (simd_debug) {
	  fprintf(TFile, "No vector intrinsic available for %s, vl %d\n",
		  INTRINSIC_name(WN_intrinsic(wn)), vl);
	}
      }
    } else {
      TYPE_ID desc_scalar = AT_FACTORY::canonical_kid_type(wn, 0,
							   &AT_WN_Decode_SIMD_EINFO);
      Screen_Type(desc_scalar, WN_kid0(wn));
      if (!vls->Test(vl)) {
	continue;
      }
      
      TYPE_ID desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(desc_scalar, vl);
      Is_True(desc != MTYPE_UNKNOWN,
	      ("No SIMD type for %s x %d", MTYPE_name(desc_scalar), vl));
      
      if (oper == OPR_CVT || oper == OPR_CVTL || oper == OPR_TRUNC) {
	if (desc != rtype && !Simd_Ti->Type_Conversion_Possible(desc, rtype)) {
	  vls->Reset(vl);
	}
	
	continue;
      }
      
      bool immed = false;
      INT shift_amount = 0;
      if (Is_Shift(wn, immed, shift_amount)) {
	OPERATOR shift_op = (oper == OPR_DIV) ? OPR_LSHR : oper;
	if ((!immed ||
	     Simd_Ti->Tie_Shift_Intrinsic_Imm(shift_op, rtype,
					      shift_amount) == INTRINSIC_INVALID) &&
	    Simd_Ti->Tie_Shift_Intrinsic(shift_op, rtype) == INTRINSIC_INVALID) {
	  if (simd_debug) {
	    fprintf(TFile, "No shift available for %s, rtype %s, %s shift amount)",
		    OPERATOR_name(oper), MTYPE_name(rtype),
		    immed ? "immediate" : "variable");
	  }
	  vls->Reset(vl);
	}
	continue;
      }
      
      TIE_MACRO_ID macro_id = Simd_Ti->Search_Vector_Macro(oper, desc, rtype);
      if (macro_id == TIE_INVALID_ID) {
        if (OPERATOR_is_compare(oper)) {
          OPERATOR new_oper =
            (oper == OPR_NE || oper == OPR_EQ) ?
            SIMD_Negate_Comparison(oper) : SIMD_Swap_Comparison(oper);
          macro_id =
            Simd_Ti->Search_Vector_Macro(new_oper, desc, rtype);
        }
      }
      
      if (macro_id == TIE_INVALID_ID) {
        vls->Reset(vl);
	if (simd_debug) {
	  fprintf(TFile,"No intrinsic available for oper %s, rtype %s, desc %s\n",
		  OPERATOR_name(oper), MTYPE_name(rtype), MTYPE_name(desc));
	}
      }
    }
  }
}


SIMD_EINFO* 
SIMD_LOOP_AT::Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO_AT(wn,cur_size,p), p);
    return res;
}

SIMD_EINFO* 
SIMD_LOOP_AT::Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO_AT(wn,e_info), e_info->Pool());
    return res;
}


TYPE_ID
SIMD_LOOP_AT::Parm_Type (const WN *parm_wn)
{
  TYPE_ID rtype = WN_rtype(parm_wn);
  if (!MTYPE_is_integral(rtype)) {
    return rtype;
  }

  WN *tie_wn = LWN_Get_Parent(parm_wn);
  Is_True(WN_operator(tie_wn) == OPR_INTRINSIC_CALL ||
          WN_operator(tie_wn) == OPR_INTRINSIC_OP,
          ("Expected an intrinsic"));
  
  INTRINSIC intrn = (INTRINSIC)WN_intrinsic(tie_wn);
  Is_True(INTRN_is_tie_intrinsic(intrn), ("Expected a TIE intrinsic"));
  TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
  TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
  
  INT parm_idx = LWN_Kid_Index(tie_wn, parm_wn);
  Is_True(parm_idx >= 0, ("Broken parents"));

  INT proto_idx = tie_macro->whirl_to_proto_index(parm_idx);
  if (proto_idx < 0) {
    return rtype;
  }
  
  TYPE_ID proto_type = tie_macro->proto_mtype_id(tie_info, proto_idx);
  return proto_type;
}


/*---------------------------------------------------------------------------*
 * Simd compute bit size:                                                    *
 *     scan the loop body to compute size of each expr.                      *
 *     set has bad operator if an operation is not supported by SIMD         * 
 *---------------------------------------------------------------------------*/
INT
SIMD_LOOP_AT::Screen_Operator_Compute_Size_Rec(WN *wn, SIMD_IF_ANA *if_ana,
                                               INT trunc_bit_size)
{
  /* If trunc_bit_size is positive, then the parent needs only the
     'trunc_bit_size' least significant bits of the result. This
     value is used to the reduce the bit size of the result of
     some expressions. */

    // shortcut
    if (Bad_Operator()) {
	return 0;
    }
    
    OPERATOR oper  = WN_operator(wn);
    TYPE_ID  rtype = WN_rtype(wn);
    TYPE_ID  desc  = WN_desc(wn);
    
    bool sign_safe = false; // mark sign-relatively-safe operations
    bool create_einfo = true; // create expression info for the node
    
    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Screen_Operator_Compute_Size_Rec(kid, if_ana);
	}
	return 0;
    } else if (oper == OPR_DO_LOOP) {
	if (if_ana) {
	    Set_Bad_Oper_Msg(AT_MSG_SIMD_IF_HAS_LOOP, if_ana->If_Wn());
	    return 0;
	} else {
	    return Screen_Operator_Compute_Size_Rec(WN_do_body(wn), if_ana);
	}
    } else if (oper == OPR_IF) {
	if (LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || SIMD_LOOP::Has_If_Convert_Pragma(wn)) {
	    WN *if_test = WN_if_test(wn);
	    if (OPERATOR_is_compare(WN_operator(if_test)) &&
                !Invariant_In_Simd_Loop(if_test)) {
		SIMD_IF_ANA *new_if_ana = 
		    CXX_NEW(SIMD_IF_ANA(wn, Pool()), Pool());
		If_Ana_Map().Enter(wn, new_if_ana);
		if (if_ana) {
		    if_ana->Add_Inner_If(new_if_ana);
		}
		new_if_ana->Set_Is_Test();
		Screen_Operator_Compute_Size_Rec(if_test, new_if_ana);
		new_if_ana->Set_Is_Then();
		Screen_Operator_Compute_Size_Rec(WN_then(wn), new_if_ana);
		new_if_ana->Set_Is_Else();
		Screen_Operator_Compute_Size_Rec(WN_else(wn), new_if_ana);
	    } else {
		Set_Bad_Oper_Msg(wn);
	    }
	    return 0;
	} 
	Set_Bad_Oper_Msg(wn);
	return 0;
    }
 
    
    /* reassociate expression */
    Simd_Reassociate_Mulr(wn);
    
    INT         cur_size    = 0;
    INT         child_size  = 0;
    INT         child1_size = 0;
    SIMD_EINFO *child_info  = NULL;
  
    // no need to screen SIMD loop invariant expressions
    // unless leaves, which need to be screened for correct size
    if (oper == OPR_CONST ||
        (oper != OPR_PARM &&
         !OPERATOR_is_leaf(oper) &&
         !Varies_With_Loop(wn, Simd_Loop_Level()))) {
	SIMD_EINFO *e_info = E_Info().Find(wn);
	if (e_info == NULL) {
	    /* invariant can be moved out of the loop */
	    cur_size = Compute_Size(wn);
      
	    e_info = Create_Simd_EInfo(wn,cur_size,Pool());
	    E_Info().Enter(wn, e_info);
	    TYPE_ID res_type = Get_Res_Type(wn,cur_size);
	    e_info->Set_Res_Type(res_type);
	    if (_at_transform) {
		Screen_Scalar_To_Vector_Conversion(res_type);
	    }
	}
	return e_info->Bit_Size();
    }
  
    if (oper == OPR_INTCONST) {
	SIMD_EINFO *e_info = E_Info().Find(wn);
	if (e_info == NULL) {
	    INT32 val = WN_const_val(wn);
            bool use_unsigned = false;
            if (val < -32768) {
              cur_size = 32;
            } else if (val < -128) {
              cur_size = 16;
            } else if (val <= 127) {
              cur_size = 8;
            } else if (val <= 255) {
              use_unsigned = true;
              cur_size = 8;
            } else if (val <= 32767 && MTYPE_is_signed(rtype)) {
              cur_size = 16;
            } else if (val <= 65535) {
              use_unsigned = true;
              cur_size = 16;
            } else {
              cur_size = 32;
            }
            if (trunc_bit_size > 0) {
              cur_size = MIN(cur_size, trunc_bit_size);
            }
	    e_info = Create_Simd_EInfo(wn,cur_size,Pool());
	    E_Info().Enter(wn, e_info);
	    TYPE_ID res_type = Get_Res_Type(wn,cur_size);
            if (use_unsigned) {
              res_type = Mtype_TransferSign(MTYPE_U4, res_type);
            }
	    e_info->Set_Res_Type(res_type);
	}
	return e_info->Bit_Size();
    }
  
    switch (oper) {
    case OPR_LDID: { /* direct load */
	cur_size = MTYPE_bit_size(desc);
	if (MTYPE_is_integral(desc)) {
	    // Try to convert it to a narrow element...
	    INT max_def_bits = Get_Max_Def_Bits(wn);
	    if (max_def_bits == 0) {
		max_def_bits = cur_size;
	    }
	    // printf("*** LDID: cur_size = %d max_def_bits %d\n",
	    // cur_size,max_def_bits);
	    cur_size = MIN(cur_size,max_def_bits);
	}
	sign_safe = true;
	break;
    }
    
    case OPR_STID: { /* direct store */
	cur_size   = MTYPE_bit_size(desc);

	if (MTYPE_is_integral(desc)) {
	    // If it is a wide element def, we may want to convert it to a narrow
	    // element and possibly use double size.
	    // So, from the DEF we get the size of all its USEs based on their DEFs.
	    INT use_min_size = cur_size;
	    INT def_max_size = 0;
	
	    WN *simdloop = Simd_Loop();
	    Is_True(simdloop,("No model/simd loop!"));
	
	    USE_LIST* use_list = Du_Mgr->Du_Get_Use(wn);
	    if (use_list && !use_list->Incomplete()) {
		USE_LIST_ITER iter_use(use_list);
		for (DU_NODE *use_node = iter_use.First();
		     !iter_use.Is_Empty();
		     use_node = (DU_NODE *) iter_use.Next()) {
		    WN *use = use_node->Wn();
		
		    if (Is_Descendent(use,simdloop)) {
			Is_True(WN_operator(use)==OPR_LDID,
				("Usupported use operator!"));
			INT use_size = MTYPE_bit_size (WN_desc(use));
			INT def_size = Get_Max_Def_Bits(use);
			if (def_size == 0) {
			    def_size = use_size;
			}
			def_max_size = MAX(def_size,def_max_size);
			use_size = MIN(use_size, def_size);
			use_min_size = MIN(use_size,use_min_size);
		    }
		}
	    }
	
	    if (def_max_size>0 && use_min_size<cur_size) {
		Is_True(def_max_size>=use_min_size,
			("Bad relation def_max_size>=use_min_size!"));
		if (def_max_size==use_min_size) {
		    cur_size = use_min_size;
		}
	    }
	}
	
	child_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana, cur_size);
	if (Bad_Operator()) {
	    return 0;
	}
	sign_safe=true;
	break;
    }

    case OPR_ILOAD:  /* indirect load/store, get size from 'desc' */
	cur_size   = MTYPE_bit_size(desc);
	sign_safe=true;
	break;
    
    case OPR_ISTORE: 
	cur_size     = MTYPE_bit_size(desc);
	child_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana, cur_size);
	if (Bad_Operator()) {
	    return 0;
	}
	sign_safe=true;
	break;
    
    case OPR_BNOT:
	cur_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	if (Bad_Operator()) {
	    return 0;
	}
	sign_safe=true;
	break;

    case OPR_NEG:
	cur_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana, trunc_bit_size);
        if (trunc_bit_size > 0) {
          cur_size = MIN(cur_size, trunc_bit_size);
        }

	if (Bad_Operator()) {
	    return 0;
	}
	break;

    case OPR_ABS:
    case OPR_OUTPART:
    case OPR_RECIP:
    case OPR_RSQRT:
	cur_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	if (Bad_Operator()) {
	    return 0;
	}
	break;
    
    case OPR_PARM:
      Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
      if (Bad_Operator()) {
        return 0;
      }
      /* Because we can't add CVTs on top of PARM nodes, we need to initialize them
         with the correct type. Later, CVTs are added to the PARMs' kids. */
      cur_size = MTYPE_bit_size(Parm_Type(wn));
      break;

    case OPR_TRUNC:
    case OPR_CVT:
	cur_size   = MTYPE_bit_size(rtype);
	child_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	if (Bad_Operator()) {
          return 0;
	}

        /* The compiler converts to I8/U8 only if really necessary so
           honor it. */
        if (MTYPE_is_integral(rtype) && (cur_size < 64))
          cur_size = MIN(cur_size, child_size);
	break;
    
    case OPR_CVTL:
	cur_size   = WN_cvtl_bits(wn);
        if (trunc_bit_size > 0) {
          cur_size = MIN(cur_size, trunc_bit_size);
        }
        
	child_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana, cur_size);
	if (Bad_Operator()) {
          return 0;
	}
	cur_size   = MIN(cur_size, child_size);
	break;
    
	/* the following two need to be canonized to the next three */
    case OPR_NE:
    case OPR_GE:
    case OPR_GT:
    case OPR_EQ:
    case OPR_LE:
    case OPR_LT:
    {

      WN *parent = LWN_Get_Parent(wn);
      if (!parent ||
          WN_operator(parent) != OPR_IF ||
          WN_if_test(parent) != wn) {
        Set_Bad_Oper_Msg(wn);
        return 0;
      }
      
      WN *child1 = WN_kid0(wn);
      WN *child2 = WN_kid1(wn);
      
      cur_size  = Screen_Operator_Compute_Size_Rec(child1, if_ana);
      child_size= Screen_Operator_Compute_Size_Rec(child2, if_ana);
      if (Bad_Operator()) {
        return 0;
      }
      cur_size = MAX(cur_size, child_size);
      break;
    }

    case OPR_ADD:
    case OPR_SUB: 
    {
	WN *child1 = WN_kid0(wn);
	WN *child2 = WN_kid1(wn);
    
	cur_size  = Screen_Operator_Compute_Size_Rec(child1, if_ana, trunc_bit_size);
	child_size= Screen_Operator_Compute_Size_Rec(child2, if_ana, trunc_bit_size);
	if (Bad_Operator()) {
	    return 0;
	}
	cur_size = MAX(cur_size, child_size);

	// setup mulr for Vectra2
	Screen_Mulr(wn);
	break;
    }
  
    case OPR_BXOR:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_MAX:
    case OPR_MIN: /* maximum of the children's size */
	cur_size  = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	child_size= Screen_Operator_Compute_Size_Rec(WN_kid1(wn), if_ana);
	if (Bad_Operator()) {
	    return 0;
	}
	cur_size = MAX(cur_size, child_size);
	if (oper==OPR_BXOR || oper==OPR_BAND || oper==OPR_BIOR) {
	    sign_safe=true;
	}
	break;
    
    case OPR_DIV:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR: {
	bool immed = false;
	INT shift_amount = 0;
	if (!Is_Shift(wn, immed, shift_amount)) {

          /* Check and issue a special message for signed power-of-two
             division. */
          if (oper == OPR_DIV) {
            WN *expr = WN_kid0(wn);
            WN *sa = WN_kid1(wn);
            if ((WN_operator(sa) == OPR_INTCONST) &&
                (WN_const_val(sa) > 0) &&
                (LNO_IS_POWER_OF_TWO(WN_const_val(sa))) &&
                MTYPE_is_signed(WN_rtype(expr))) {
              Set_Bad_Oper_Msg(AT_MSG_SIMD_SIGNED_POW_TWO_DIV, wn);
              return 0;
            }
          }
          
          Set_Bad_Oper_Msg(wn);
          return 0;
	}

	if (Varies_With_Loop(WN_kid1(wn), Simd_Loop_Level())) {
          Set_Bad_Oper_Msg(AT_MSG_SIMD_VARIANT_SHIFT, wn, SIMD_OPCODE_Msg(wn));
          return 0;
	}
	
        cur_size   = MTYPE_bit_size(rtype);
        if (oper == OPR_SHL && trunc_bit_size > 0) {
          cur_size = MIN(cur_size, trunc_bit_size);
        }
        
	child_size = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	if (Bad_Operator()) {
	    return 0;
	}
	
        INT p_size = child_size;

        /* Shift-left may require type promotion. */
        if (oper == OPR_SHL && child_size < cur_size) {
          if (immed) {
            if (shift_amount > child_size / 4) {
              while (p_size < MIN(cur_size, child_size + shift_amount)) {
                p_size <<= 1;
              }
            } else {
              /* Rely on guard bits to handle shift-lefts by small amounts. */
            }
          } else {
            p_size = cur_size;
          }
        }
        
        cur_size = MIN(cur_size, p_size);
    }
    break;
    
    case OPR_MPY:  /* check for 16 bit size */
	child_size  = Screen_Operator_Compute_Size_Rec(WN_kid0(wn), if_ana);
	child1_size = Screen_Operator_Compute_Size_Rec(WN_kid1(wn), if_ana);
	if (Bad_Operator()) {
	    return 0;
	}
	
	cur_size = MIN(MTYPE_bit_size(rtype),2*MAX(child_size,child1_size));
	if (Simd_Target != SIMD_TARG_VECTRA_II && trunc_bit_size > 0) {
          if (trunc_bit_size == MAX(child_size, child1_size)) {
            cur_size = MIN(cur_size, trunc_bit_size);
          }
	}

	break;
    
    case OPR_INTRINSIC_OP: {
      /* TIE intrinsic calls are not handled at this time, only intrinsic ops are. */
      INTRINSIC intrn = (INTRINSIC) WN_intrinsic(wn);
      if (!INTRN_is_tie_intrinsic(intrn)) {
        Set_Bad_Oper_Msg(wn);
        return 0;
      }
      
      TIE_MACRO_p tie_macro = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intrn));
      Is_True(tie_macro,
              ("Can't find a TIE macro for intrinsic %s", INTRINSIC_name(intrn)));
      
      for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++) {
        INT proto_idx = tie_macro->whirl_to_proto_index(kid_idx);
        
        /* Don't recurse in TIE macro immediates because we don't
           vectorize those. */
        if (!tie_macro->proto_is_immed(proto_idx)) {
          Screen_Operator_Compute_Size_Rec(WN_kid(wn,kid_idx), if_ana);
        }
      }
      
      if (oper == OPR_INTRINSIC_CALL) {
        create_einfo = false;
      } else {
        cur_size = MTYPE_bit_size(tie_macro->output_mtype(tie_info));
      }
      break;
    }      
      
    default:
	Set_Bad_Oper_Msg(wn);
	return 0;
    }

    SIMD_EINFO *e_info = NULL;
    TYPE_ID res_type = MTYPE_UNKNOWN;

    if (create_einfo) {
	if (cur_size <= 0) {
	    Set_Bad_Oper_Msg(wn);
	    return 0;
	}
    
	/* set up the mapping from 'wn' to 'EINFO' */
	Is_True(E_Info().Find(wn) == NULL, ("E_Info().Find(wn) == NULL"));
	e_info = Create_Simd_EInfo(wn, cur_size,Pool());
	E_Info().Enter(wn, e_info);
	e_info->Set_Is_Vector();
    
        res_type = Get_Res_Type(wn, cur_size);
	e_info->Set_Res_Type(res_type);

        /* Override the result type of narrow multiplies to match the
           sign of the kid's canonical type. */
        if (oper == OPR_MPY && MTYPE_bit_size(res_type) < 32) {
          TYPE_ID can_kid_type = AT_FACTORY::canonical_kid_type(wn, 0,
                                                                &AT_WN_Decode_SIMD_EINFO);
          if (MTYPE_is_signed(res_type) != MTYPE_is_signed(can_kid_type)) {
            res_type = Mtype_TransferSign(can_kid_type, res_type);
            e_info->Set_Res_Type(res_type);
          }
        }
        
	if (if_ana) {
	    if_ana->Add_EInfo(e_info);
	}

	if (_at_transform) {
	    // screen res_type
	    Screen_Type(res_type,wn);
	    if (Bad_Operator()) {
		return 0;
	    }

	    // some invariant expressions may reach this point,
	    // so check if scalar to vector conversion is possible
	    if (!Varies_With_Loop(wn, Simd_Loop_Level())) {
		Screen_Scalar_To_Vector_Conversion(res_type);
		if (Bad_Operator()) {
		    return 0;
		}
	    }
	}
    }
  
    /* Compute memory load/store size */
    if (OPERATOR_is_load(oper) || OPERATOR_is_store(oper)) {
      /* Set up the IMEM_INFO */
      if (oper == OPR_ISTORE || oper == OPR_ILOAD) {
        WN* array_wn = (oper == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn);
        if (WN_operator(array_wn) == OPR_ARRAY &&
            (WN_operator(WN_array_base(array_wn)) == OPR_LDID ||
             WN_operator(WN_array_base(array_wn)) == OPR_LDA)) {
          IMEM_Map  &imem_map = IMem_Map();
          IMEM_INFO *imem_info = imem_map.Find(wn);
          if (imem_info == NULL) {
            imem_info = CXX_NEW(IMEM_INFO(wn, &imem_map, Pool()), Pool());
            imem_map.Enter(imem_info);
            
            if (imem_info->Too_Messy()) {
              Set_Bad_Oper_Msg(AT_MSG_SIMD_BAD_ACCESS, wn, SIMD_OPCODE_Msg(wn));
              return 0;
            }
          } else {
            if (cur_size != MTYPE_bit_size(imem_info->Scalar_Type())) {
              Set_Bad_Oper_Msg(AT_MSG_SIMD_BAD_ACCESS, wn, SIMD_OPCODE_Msg(wn));
              return 0;
            }
            imem_info->Add_Lod_Sto(wn);
          }
          imem_map.Add_Lod_Sto(wn);
          e_info->Set_IMem(imem_info);

          /* Fixup the einfo result type. */
          if (oper == OPR_ISTORE) {
            e_info->Set_Res_Type(imem_info->Scalar_Type());
          }
        } else {
          Set_Bad_Oper_Msg(AT_MSG_SIMD_BAD_ACCESS, wn, SIMD_OPCODE_Msg(wn));
          return 0;
        }
      } else {
        Is_True(oper == OPR_STID || oper == OPR_LDID, 
                ("Expecting LDID/STID"));
        SYMBOL symbol(wn);
        SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
        if (s_info == NULL) {
          s_info = CXX_NEW(SIMD_SCALAR(symbol, wn, Pool()), Pool());
          S_Info_Map().Enter(symbol, s_info);
          S_Info_Stack().Push(s_info);
        } else {
          s_info->Add_Lod_Sto(wn);
        }
      }
    }

    // fix up constant kid's type
    for (INT kid_idx=0; kid_idx<WN_kid_count(wn); kid_idx++) {
	WN *kid = WN_kid(wn,kid_idx);
	if (WN_operator(kid) == OPR_INTCONST) {
	    SIMD_EINFO *c_info = Get_E_Info(kid);
	    Is_True(c_info, ("Cannot find EINFO for integer constant (kid %d operator %s)",
			     kid_idx, OPERATOR_name(oper)));

	    // find the canonical type for the current kid
	    // if it is wider than the current type, set the
	    // constant to the new type.
	    TYPE_ID kid_type = c_info->Res_Type();
	    TYPE_ID new_kid_type = AT_FACTORY::canonical_kid_type(wn, kid_idx,
								  &AT_WN_Decode_SIMD_EINFO);
	    if (MTYPE_bit_size(new_kid_type) > MTYPE_bit_size(kid_type)) {
		if (simd_debug) {
		    fprintf(TFile,"Reset integer constant type from %s to %s.\n",
			    MTYPE_name(kid_type),MTYPE_name(new_kid_type));
		}
		c_info->Set_Res_Type(new_kid_type);
		c_info->Set_BitSize(MTYPE_bit_size(new_kid_type));
	    }

	    if (_at_transform) {
		Screen_Scalar_To_Vector_Conversion(c_info->Res_Type());
	    }
	}

	if (OPERATOR_is_store(oper) || oper == OPR_DIV ||
	    oper == OPR_SHL || oper == OPR_ASHR || oper == OPR_LSHR) {
	    break;
	}
    }
  
    if (_at_transform) {
	// check operator availability including any possible type conversion
	// on the kids
	Screen_Operator(wn,e_info);
	if (Vector_Lengths()->Pop_Count()==0) {
	    Set_Bad_Oper_Msg(wn);
	    return 0;
	}
    }

    return cur_size;
}
    

void
SIMD_LOOP_AT::Init_Vector_Lengths (BIT_VECTOR *vls)
{
  INT max_vl = Max_Vector_Length();
  if (max_vl <= 0)
    max_vl = vls->Size() - 1;
  
  for (INT vl = 2; vl <= max_vl; vl++)
    vls->Set(vl);
}


void
SIMD_LOOP_AT::Set_Default_Vl()
{
    Simd_Ti->Set_Default_Vl(V_Unroll_Factor());
}


void
SIMD_LOOP_AT::Screen_Mulr(WN *wn)
{
    // do nothing
}


INT
SIMD_LOOP_AT::Find_Unroll_Factor (void)
{
  /* Choose the largest common vectorization factor that doesn't exceed
     the estimated iteration count for the loop. */
  INT64 est_iter = Max_Iteration_Count();
  INT max_vl = Vector_Lengths()->Size() - 1;
  if (est_iter > 0 && est_iter < max_vl) {
    max_vl = (INT)est_iter;
  }
  for (INT vl = max_vl; vl > 0; vl--) {
    if (Vector_Lengths()->Test(vl)) {
      return vl;
    }
  }

  return 0;
}


bool
SIMD_LOOP_AT::Screen_Guards (void)
{
  BIT_VECTOR *vls = Vector_Lengths();
  for (INT vl = 0; vl < vls->Size(); vl++) {
    if (vls->Test(vl) && !Check_Guards(vl)) {
      vls->Reset(vl);
    }
  }
  
  return (vls->Pop_Count() > 0);
}


INT
SIMD_LOOP_AT::Screen_Operator_Compute_Size (WN *wn, SIMD_IF_ANA *if_ana)
{
  BIT_VECTOR *vls =
    CXX_NEW(BIT_VECTOR(SIMD_TI::MAX_VECTOR_LENGTH + 1, Pool()), Pool());
  
  Init_Vector_Lengths(vls);
  Set_Vector_Lengths(vls);
  
  for (INT i = 0; i < 4; i++) {
    _guard_types[i] = MTYPE_UNKNOWN;
  }
  
  Set_Cur_At_Simd_Loop(this); // for SIMD_EINFO decoder routine
  INT size = Screen_Operator_Compute_Size_Rec(wn, if_ana);
  Set_Cur_At_Simd_Loop();
  
  if (Bad_Operator()) {
    return 0;
  }
  
  if (_at_analysis) {
    return size;
  }
  
  if (vls->Pop_Count() == 0) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_COMMON_VL, Simd_Loop());
    return 0;
  }
  
  /* Check if there are enough available guard bits. */
  if (LNO_Simd_Check_Guards) {
    Find_Guard_Bits(wn, _req_int8_guards, _req_int16_guards);
    if (!Screen_Guards()) {
      Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_GUARDS, Simd_Loop());
      return 0;
    }
  }
  
  INT unroll = Find_Unroll_Factor();
  Is_True(Max_Vector_Length() < 0 || unroll <= Max_Vector_Length(),
          ("Unexpected unroll factor %d > %d",
           unroll, Max_Vector_Length()));
  
  Set_V_Unroll_Factor(unroll);
  Set_Default_Vl();
  
  if (unroll <= 0) {
    Set_Too_Small();
    Set_Bad_Oper_Msg(AT_MSG_SIMD_SMALL_TRIP_COUNT, Simd_Loop());
    return 0;
  }
  
  return size;  
}


SIMD_EINFO *
SIMD_LOOP_AT::Simd_Preprocess_Transform(WN* wn) {
    OPERATOR oper  = WN_operator(wn);
  
    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Simd_Preprocess_Transform(kid);
	}
	return NULL;
    } else if (oper == OPR_DO_LOOP) {
	return Simd_Preprocess_Transform(WN_do_body(wn));
    } else if (oper == OPR_IF) {
	Simd_Preprocess_Transform(WN_if_test(wn));
	if (WN_then(wn)) {
	    Simd_Preprocess_Transform(WN_then(wn));
	}
	if (WN_else(wn)) {
	    Simd_Preprocess_Transform(WN_else(wn));
	}
	return NULL;
    }
  
    SIMD_EINFO  *e_info  = Get_E_Info(wn);
    SIMD_EINFO  *c1_info = NULL;
    SIMD_EINFO  *c2_info = NULL;
    SIMD_EINFO  *c3_info = NULL;
    WN          *child1  = NULL;
    WN          *child2  = NULL;
    
    if (oper == OPR_CONST ||
        (!e_info->Is_Vector() && !OPERATOR_is_leaf(oper))) {
      return e_info;
    }
  
    switch (oper) {
    case OPR_STID:
	c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
	SIMD_EINFO_Add_Def_Use(c1_info, e_info);
	break;

    case OPR_LDID:
    {
	DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
	DEF_LIST_ITER iter_def(def_list);
	for (DU_NODE *def_node = iter_def.First(); 
	     !iter_def.Is_Empty(); 
	     def_node = (DU_NODE *) iter_def.Next()) {
	    WN *def = def_node->Wn();
	    SIMD_EINFO  *def_info = Get_E_Info(def);
	    if (def_info) {
		SIMD_EINFO_Add_Def_Use(def_info, e_info);
	    }
	}
	break;
    }
  
    case OPR_ISTORE:
    {
	IMEM_INFO *imem = e_info->IMem();
	Is_True(imem, ("Cannot find IMEM_INFO"));
	IMEM_OFFSET *io = imem->Imem_Offset();
	Is_True(io, ("IMEM_OFFSET missing."));
	IMEM_GROUP *ig = io->Parent_Group();
	Is_True(ig, ("IMEM_GROUP missing."));
	
	ig->Stores()->AddElement(wn);

	c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
	SIMD_EINFO_Add_Def_Use(c1_info, e_info);
	break;
    }
    
    case OPR_ILOAD:
    {
	IMEM_INFO *imem = e_info->IMem();
	Is_True(imem, ("Cannot find IMEM_INFO"));
	IMEM_OFFSET *io = imem->Imem_Offset();
	Is_True(io, ("IMEM_OFFSET missing."));
	IMEM_GROUP *ig = io->Parent_Group();
	Is_True(ig, ("IMEM_GROUP missing."));
	
	ig->Loads()->AddElement(wn);
	
	/* get the dependence edges */
	ARRAY_DIRECTED_GRAPH16 *adg = Array_Dependence_Graph;
	VINDEX16 v = adg->Get_Vertex(wn);
	if (v) {
	    EINDEX16 edge = adg->Get_In_Edge(v);
	    while (edge) {
		VINDEX16 srcv = adg->Get_Source(edge);
		WN *srcv_wn   = adg->Get_Wn(srcv);
		SIMD_EINFO *def_einfo = Get_E_Info(srcv_wn);
		if (def_einfo && 
		    def_einfo->
		    IMem()->
		    Imem_Offset()->Parent_Group() == ig) {
		    SIMD_EINFO_Add_Def_Use(def_einfo, e_info);
		}
		edge = adg->Get_Next_In_Edge(edge);
	    }
	}
	break;
    }

    case OPR_TRUNC:
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_ABS:    /* unary */
    case OPR_NEG:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_BNOT:
    case OPR_DIV:
    case OPR_PARM:
    case OPR_OUTPART:
    case OPR_RECIP:
    case OPR_RSQRT:
	c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
	SIMD_EINFO_Add_Def_Use(c1_info, e_info);
	break;
	
    case OPR_ADD: /* find MULA candidate */
    case OPR_SUB: { 
	child1 = WN_kid0(wn);
	child2 = WN_kid1(wn);
      
	c1_info = Simd_Preprocess_Transform(child1);
	c2_info = Simd_Preprocess_Transform(child2);
	
	SIMD_EINFO_Add_Def_Use(c1_info, e_info);
	SIMD_EINFO_Add_Def_Use(c2_info, e_info);
        
        /* Don't use floating-point fused multiply-add unless explicitly
           allowed to. */
        bool madd_allowed = (!MTYPE_is_float(e_info->Res_Type()) || Madd_Allowed);
        
        if (madd_allowed && (oper == OPR_SUB || oper == OPR_ADD) && 
	    WN_operator(child2) == OPR_MPY && c2_info->Is_Vector()) {
	    if (c1_info->Single_Use() || c1_info->Is_Over_Written()) {
		bool mula = false;
		// MADD/MSUB possible -- check if the operation is available
		if (AT_Analysis()) {
		    mula = true;
		} else {
		    SIMD_EINFO *desc_info = E_Info().Find(WN_kid0(child2));
		    Is_True(desc_info != NULL, ("Can't find child EINFO"));
		    
		    TYPE_ID res_type = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
		    TYPE_ID desc_type = Simd_Ti->Get_SIMD_Reg_Type_Scalar(desc_info->Res_Type());
		    
		    OPERATOR mac_op = oper == OPR_ADD ? OPR_MADD : OPR_MSUB;
		    TIE_MACRO_ID macro_id = Simd_Ti->Search_Vector_Macro(mac_op,
									 desc_type,
									 res_type);
		    if (macro_id != TIE_INVALID_ID) {
			mula = true;
		    }
		}
		
		if (mula) {
		    e_info->Set_Mula();
		    if (simd_debug) {
			fprintf(TFile, "Found MULA\n");
		    }
		}
	    }
	}
	break;
    } 
  
    case OPR_NE:
    case OPR_GE:
    case OPR_GT:
    case OPR_EQ:
    case OPR_LE:
    case OPR_LT:
    case OPR_MPY:  
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_MAX:
    case OPR_MIN: 
      c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
      c2_info = Simd_Preprocess_Transform(WN_kid1(wn));
      SIMD_EINFO_Add_Def_Use(c1_info, e_info);
      SIMD_EINFO_Add_Def_Use(c2_info, e_info);
      break;

    case OPR_SELECT:
    {
      /* Go into the test condition of the first select for each converted if. */
      SIMD_IF_CONV *if_conv = If_Conv_Map().Find(wn);
      if (if_conv) {
        c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
        SIMD_EINFO_Add_Def_Use(c1_info, e_info);
      }
      c2_info = Simd_Preprocess_Transform(WN_kid1(wn));
      c3_info = Simd_Preprocess_Transform(WN_kid2(wn));
      SIMD_EINFO_Add_Def_Use(c2_info, e_info);
      SIMD_EINFO_Add_Def_Use(c3_info, e_info);
    }
    break;

    case OPR_INTRINSIC_CALL:
    case OPR_INTRINSIC_OP:
    {
      INTRINSIC intrn = (INTRINSIC) WN_intrinsic(wn);
      Is_True(INTRN_is_tie_intrinsic(intrn),
              ("Expected a TIE intrinsic, not %s", INTRINSIC_name(intrn)));
      
      TIE_MACRO *tie_macro = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intrn));
      Is_True(tie_macro,
              ("Can't find a TIE macro for intrinsic %s", INTRINSIC_name(intrn)));
      
      for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++) {
        INT proto_idx = tie_macro->whirl_to_proto_index(kid_idx);
        
        /* Don't recurse into TIE macro immediate operands. */
        if (!tie_macro->proto_is_immed(proto_idx)) {
          c1_info = Simd_Preprocess_Transform(WN_kid(wn, kid_idx));
          if (c1_info) {
            SIMD_EINFO_Add_Def_Use(c1_info, e_info);
          }
        }
      }
    }
    break;

    case OPR_INTCONST:
      break;
	
    default:
      Is_True(0, ("Unexpected operator %s", OPERATOR_name(oper)));
      break;
    }
  
    All_EInfo().Push(e_info);
    
    return e_info;
}


SIMD_EINFO*
SIMD_LOOP_AT::Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd) {
    SIMD_EINFO  *e_info   = Get_E_Info(expr);
    Is_True(e_info, ("No SIMD_EINFO"));
    TYPE_ID simd_type =  simd->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
  
    WN         *source       = WN_kid0(expr);
    SIMD_EINFO *c_info = Simd_Transform(source, stmt, simd);
    Is_True(c_info, ("No SIMD_EINFO"));
  
    if (simd_type == c_info->Simd_Reg()->Type()) {
      e_info->Copy_Simd_Reg_Info(c_info);
    } else {
      /* generate SIMD registers */
      Generate_Simd_Reg_Info(e_info, simd);
      
      OPCODE  opc_cvt = OPCODE_make_op(OPR_CVT,
				       e_info->Simd_Reg()->Type(),
				       c_info->Simd_Reg()->Type());
      WN     *cvt     = LWN_CreateExp1(opc_cvt, c_info->Simd_Reg()->Simd_Preg_Ldid());
      WN     *stid    = e_info->Simd_Reg()->Simd_Preg_Stid(cvt);
      
      LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
      WN_linenum(stid) = LWN_Get_Linenum(stmt);
    }
  
    return e_info;
}

//
// Setup_Type_Conversion inserts explicit CVT/CVTL nodes wherever the vectorizer
// needs to do a type conversion based on the SIMD_EINFO result types
//

SIMD_EINFO *
SIMD_LOOP_AT::Setup_Type_Conversion_Rec (WN* wn)
{
  OPERATOR oper  = WN_operator(wn);
  
  /* scan statements in a block */
  if (oper == OPR_BLOCK) {
    for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
      Setup_Type_Conversion_Rec(kid);
    }
    return NULL;
  }

  if (oper == OPR_DO_LOOP) {
    return Setup_Type_Conversion_Rec(WN_do_body(wn));
  }
  
  if (oper == OPR_IF) {
    Is_True(LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || SIMD_LOOP::Has_If_Convert_Pragma(wn), 
	    ("SIMD_LOOP_AT::Setup_Type_Conversion_Rec: unexpected IF"));
    Setup_Type_Conversion_Rec(WN_if_test(wn));
    Setup_Type_Conversion_Rec(WN_then(wn));
    Setup_Type_Conversion_Rec(WN_else(wn));
    return NULL;
  }
  
  SIMD_EINFO  *e_info  = Get_E_Info(wn);
  if (!e_info->Is_Vector() || OPERATOR_is_load(oper)) {
    return e_info;
  }
  
  TIE_MACRO *tie_macro = NULL;
  if (oper == OPR_INTRINSIC_CALL || oper == OPR_INTRINSIC_OP) {
    INTRINSIC intrn = (INTRINSIC) WN_intrinsic(wn);
    Is_True(INTRN_is_tie_intrinsic(intrn),
            ("Expected a TIE intrinsic, not %s", INTRINSIC_name(intrn)));
      
    tie_macro = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intrn));
    Is_True(tie_macro,
            ("Can't find a TIE macro for intrinsic %s", INTRINSIC_name(intrn)));
  }
  
  for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++)
  {
    /* Don't recurse into TIE macro immediate operands. */
    if (tie_macro) {
      INT proto_idx = tie_macro->whirl_to_proto_index(kid_idx);
      if (tie_macro->proto_is_immed(proto_idx))
        continue;
    }

    WN *kid = WN_kid(wn, kid_idx);
    SIMD_EINFO *c_info = Get_E_Info(kid);
    Is_True(c_info, ("Null e_info."));

    /* Find the canonical type for the current kid. If it is different from
       the current type, insert a conversion. */
    TYPE_ID kid_type = c_info->Res_Type();
    TYPE_ID new_kid_type = AT_FACTORY::canonical_kid_type(wn, kid_idx,
							  &AT_WN_Decode_SIMD_EINFO);
    if (new_kid_type != kid_type) {
      if (simd_debug) {
	fprintf(TFile, "Adding a CVT from %s to %s.\n",
		MTYPE_name(kid_type), MTYPE_name(new_kid_type));
      }
      Is_True(MTYPE_is_integral(new_kid_type), ("Expected integral type."));

      TYPE_ID p_new_kid_type = Promote_Type(new_kid_type);
      TYPE_ID p_kid_type = WN_rtype(kid);
      
      WN *cvt = NULL;
      if (p_kid_type == p_new_kid_type) {
	cvt = WN_Create(OPR_CVTL, p_new_kid_type, MTYPE_V, 1);
	WN_kid0(cvt) = kid;
	WN_cvtl_bits(cvt) = MTYPE_bit_size(new_kid_type);
      } else {
	cvt = WN_Create(OPR_CVT, p_new_kid_type, p_kid_type, 1);
	WN_kid0(cvt) = kid;
      }
      
      WN_kid(wn, kid_idx) = cvt;
      LWN_Set_Parent(cvt, wn);
      LWN_Set_Parent(kid, cvt);
      
      // Create a new SIMD_EINFO for the new node.
      e_info = Create_Simd_EInfo(cvt, MTYPE_bit_size(new_kid_type), Pool());
      e_info->Set_Res_Type(new_kid_type);
      e_info->Set_Is_Vector();
      E_Info().Enter(cvt, e_info);
      
      kid = cvt;
      
      /* If the current node is CVT, it may need to be fixed if its current
         descriptor type doesn't match the return type of the new kid. */
      if (oper == OPR_CVT && WN_desc(wn) != WN_rtype(kid)) {
        if (WN_rtype(wn) != WN_rtype(kid)) {
          WN_set_desc(wn, WN_rtype(kid));
        } else {
          WN_set_operator(wn, OPR_CVTL);
          WN_set_desc(wn, MTYPE_V);
          WN_cvtl_bits(wn) = MTYPE_bit_size(WN_rtype(wn));
        }
      }
    }
    
    Setup_Type_Conversion_Rec(kid);
    
    if (OPERATOR_is_store(oper) || oper == OPR_DIV ||
	oper == OPR_SHL || oper == OPR_ASHR || oper == OPR_LSHR) {
      break;
    }
  }
  
  return e_info;
}


SIMD_EINFO *
SIMD_LOOP_AT::Setup_Type_Conversion(WN *wn) {
    Set_Cur_At_Simd_Loop(this); // for SIMD_EINFO decoder routine
    SIMD_EINFO *e_info = Setup_Type_Conversion_Rec(wn);
    Set_Cur_At_Simd_Loop();
}


void
SIMD_LOOP_AT::Generate_Sel_Imm (WN *stmt, SIMD_PREG *out,
				SIMD_PREG *in1, SIMD_PREG *in2, INT64 imm,
				WN *block) 
{
    INT64 line_num = 0;
    if (stmt != NULL) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }
    
    TYPE_ID vec_type = out->Type();
    Is_True(vec_type == in1->Type() && vec_type == in2->Type(),
	    ("SEL expects the same type for all vector arguments"));
    
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, vec_type, MTYPE_V);
    INTRINSIC intrin_id = Simd_Ti->Tie_Sel_Intrinsic_Imm(vec_type, imm);
    bool has_imm = true;
    if (intrin_id == INTRINSIC_INVALID) {
	has_imm = false;
	intrin_id = Simd_Ti->Tie_Sel_Intrinsic(vec_type);
    }
    Is_True(intrin_id != INTRINSIC_INVALID,
	    ("Can't find variable SEL (%s)", MTYPE_name(vec_type)));
    
    WN *apr[3];
    apr[0] = LWN_CreateParm(vec_type, in1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(vec_type, in2->Simd_Preg_Ldid(),
			    MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
    if (has_imm) {
	apr[2] = LWN_CreateParm(MTYPE_I4, WN_CreateIntconst(OPC_I4INTCONST, imm),
				MTYPE_To_TY(MTYPE_I4), WN_PARM_BY_VALUE);
    } else {
	SIMD_PREG *s = Create_Sel_Preg(imm);
	apr[2] = LWN_CreateParm(s->Type(), s->Simd_Preg_Ldid(),
				MTYPE_To_TY(s->Type()), WN_PARM_BY_VALUE);
    }
    
    WN  *sel = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN  *sto  = out->Simd_Preg_Stid(sel);
    LWN_Insert_Block_Before(block, stmt, sto);
    WN_linenum(sto) = line_num;
}


void
SIMD_LOOP_AT::Generate_Sel_Imm (WN *stmt, SIMD_PREG *out0, SIMD_PREG *out1,
				SIMD_PREG *in0, SIMD_PREG *in1, INT64 sel0,
				INT64 sel1, INT64 sel)
{
    Generate_Dsel_Imm(stmt, out0, out1, in0, in1, sel);
}


void
SIMD_LOOP_AT::Generate_Dsel_Imm(WN *stmt, SIMD_PREG *out1, SIMD_PREG *out2,
				SIMD_PREG *in1, SIMD_PREG *in2, INT64 imm,
				WN *block) 
{
    INT64 line_num = 0;
    if (stmt != NULL) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }
    
    TYPE_ID mtype = out1->Type();
    Is_True(mtype == out2->Type() && mtype == in1->Type() && mtype == in2->Type(),
	    ("DSEL expects the same type for all vector arguments"));
    
    INTRINSIC intrin_id = Simd_Ti->Tie_Dsel_Intrinsic_Imm(mtype, imm);
    bool has_imm = true;
    if (intrin_id == INTRINSIC_INVALID) {
	has_imm = false;
	intrin_id = Simd_Ti->Tie_Dsel_Intrinsic(mtype);
    }

    /* If no DSEL is available, try SEL. */
    if (intrin_id == INTRINSIC_INVALID) {
	SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
	Is_True(simd_sel != NULL,
		("NULL SIMD select (%s).", MTYPE_name(mtype)));
	INT64 sel_0, sel_1;
	simd_sel->Dsel_To_Sel(imm, sel_1, sel_0);

	Generate_Sel_Imm(stmt, out2, in1, in2, sel_0, block);
	Generate_Sel_Imm(stmt, out1, in1, in2, sel_1, block);
	return;
    }

    // Swap the output registers for Vectra 2 (only for DSEL)
    if (Simd_Target == SIMD_TARG_VECTRA_II) {
	SIMD_PREG *temp = out1;
	out1 = out2;
	out2 = temp;
    }
  
    TYPE_ID output_type = Get_Packed_Output_Type(intrin_id);
    Is_True(tie_info->num_scalar_mtypes(output_type) == 2, 
	    ("DSEL's output type is not a compound of two types"));
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, output_type, MTYPE_V);

    WN *apr[3];
    apr[0] = LWN_CreateParm(mtype, in1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, in2->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    if (has_imm) {
	apr[2] = LWN_CreateParm(MTYPE_U8, WN_CreateIntconst(OPC_U8INTCONST, imm),
				MTYPE_To_TY(MTYPE_U8), WN_PARM_BY_VALUE);
    } else {
	SIMD_PREG *sel = Create_Sel_Preg(imm);
	apr[2] = LWN_CreateParm(sel->Type(), sel->Simd_Preg_Ldid(),
				MTYPE_To_TY(sel->Type()), WN_PARM_BY_VALUE);
    }
    WN  *dsel = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    LWN_Parentize_One_Level(dsel);
    
    Simd_Ti->Store_Packed_Op_Output(out1, out2, dsel, block, stmt, line_num);
}


WN*
SIMD_LOOP_AT::Generate_Sum_Reduction (TYPE_ID scalar_type, SIMD_PREG *vec_reg,
				      WN *block, INT64 line_number)
{
  /* generate scalar = RADD(vec_reg) */
  TYPE_ID vec_type = vec_reg->Type();
  TYPE_ID pscalar_type = Promote_Type(scalar_type);

  INTRINSIC intrin_id = Simd_Ti->Radd(scalar_type, vec_type);
  Is_True(intrin_id != INTRINSIC_INVALID,
          ("Can't find sum reduction from type %s to type %s\n",
           MTYPE_name(vec_type), MTYPE_name(scalar_type)));

  TIE_MACRO_ID macro_id = Intrinsic_To_Tie_Macro_Id(intrin_id);
  TIE_MACRO *macro = tie_info->tie_macro(macro_id);
  bool is_op = macro->is_whirl_intrinsic_op();
  
  WN *apr[2];
  OPCODE intrin_op = OPCODE_UNKNOWN;

  SIMD_PREG *red_reg = NULL;

  if (is_op) {
    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, pscalar_type, MTYPE_V);
  } else {
    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    red_reg = Gen_Symbol(pscalar_type, Pool(), "red");
    apr[0] = LWN_CreateParm(pscalar_type, red_reg->Simd_Preg_Ldid(),
                            MTYPE_To_TY(pscalar_type), WN_PARM_BY_VALUE);
  }
  
  apr[is_op ? 0 : 1] = LWN_CreateParm(vec_type, vec_reg->Simd_Preg_Ldid(),
                                      MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
  WN *red = LWN_Create_Intrinsic(intrin_op, intrin_id, is_op ? 1 : 2, apr);  

  if (!is_op) {
    if (Enclosing_Do_Loop(block) != NULL) {
      Array_Dependence_Graph->Add_Vertex(red);
    }
    
    LWN_Insert_Block_Before(block, NULL, red);
    WN_linenum(red) = line_number;
    WN *ldid = WN_Ldid(pscalar_type, -1,
                       Tie_Output_Volatile_Preg,
                       Tie_Output_Volatile_Type);
    Du_Mgr->Add_Def_Use(red, ldid);
    
    WN *stid = red_reg->Simd_Preg_Stid(ldid);
    LWN_Insert_Block_Before(block, NULL, stid);
    WN_linenum(stid) = line_number;
    
    ldid = red_reg->Simd_Preg_Ldid();
    Du_Mgr->Add_Def_Use(stid, ldid);

    red = ldid;
  }
  
  return red;
}


WN *
SIMD_LOOP_AT::Generate_Reduction (TYPE_ID scalar_type, SIMD_PREG *reg, OPERATOR op,
				  WN *stmt, INT64 line_number)
{
  Is_True(op == OPR_MIN || op == OPR_MAX,
	  ("Unexpected reduction op %s", OPERATOR_name(op)));

  /* generate scalar = { RMIN , RMAX }(vec_reg) */
  WN *apr[1];
    
  TYPE_ID vec_type = reg->Type();
  INTRINSIC intrin_id = ((op == OPR_MIN) ?
			 Simd_Ti->Rmin(scalar_type, vec_type) :
			 Simd_Ti->Rmax(scalar_type, vec_type));

  Is_True(intrin_id != INTRINSIC_INVALID,
	  ("Can't find %s reduction from type %s to type %s\n",
	   MTYPE_name(vec_type), MTYPE_name(scalar_type), OPERATOR_name(op)));
  OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, Promote_Type(scalar_type), MTYPE_V);
  
  apr[0] = LWN_CreateParm(vec_type, reg->Simd_Preg_Ldid(),
			  MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
  WN *red = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);
  
  return red;
}


bool
SIMD_LOOP_AT::Test_If_Conversion (void)
{
  HASH_TABLE_ITER<WN*, SIMD_IF_ANA*> htit(&If_Ana_Map());
  WN *if_wn;
  SIMD_IF_ANA *simd_if_ana;
  while (htit.Step(&if_wn, &simd_if_ana)) {
    Is_True(if_wn && simd_if_ana, ("no If WN or SIMD_IF_ANA"));
    if (simd_if_ana->If_Conv_Analysis()) {
      return false;
    }
  }

  /* If-conversion requires the boolean coprocessor. */
  if ((If_Ana_Map().Num_Entries() > 0) && !xt_booleans) {
    SIMD_Msg(AT_MSG_SIMD_BOOLEAN, Simd_Loop());
    return false;
  }
  
  return true;
}
  

bool
SIMD_LOOP_AT::Test_Imem_Alignment (void)
{
  // no need to check for proper operations on the target
  // if doing analysis
  if (AT_Analysis())
    return true;
  
  for (INT g = 0; g < IMem_Map().Groups().Elements(); g++) {
    IMEM_GROUP *ig = IMem_Map().Groups()[g];
    if (ig->Is_Vector()) {
      TYPE_ID scalar_type = ig->Scalar_Type();
      TYPE_ID mem_type = Simd_Ti->Simd_Mem_Type_Of_Scalar(scalar_type);
      TYPE_ID reg_type = Simd_Ti->Simd_Reg_Type_Of_Scalar(scalar_type);
      
      if (!ig->Is_Aligned()) {
        if (ig->Is_Use() && !Simd_Ti->Load_Align_Possible(mem_type, reg_type)) {
          SIMD_Msg(AT_MSG_SIMD_UNALIGNED_LOAD, Simd_Loop(), SIMD_MTYPE_Msg(mem_type));
          return false;
        }
        
        if (ig->Is_Def() && !Simd_Ti->Store_Align_Possible(mem_type, reg_type)) {
          SIMD_Msg(AT_MSG_SIMD_UNALIGNED_STORE, Simd_Loop(), SIMD_MTYPE_Msg(mem_type));
          return false;
        }
      } 
      if (ig->Elem_Count() > 1) { // we will need to split a group, requiring alignment
        if (ig->Is_Use() && !Simd_Ti->Load_Align_Possible(mem_type, reg_type) &&
		IMem_Map().Need_Split_Group_No_Vsel()) { 
          SIMD_Msg(AT_MSG_SIMD_UNALIGNED_LOAD, Simd_Loop(), SIMD_MTYPE_Msg(mem_type));
          return false;
        } else if (ig->Is_Def() && !Simd_Ti->Store_Align_Possible(mem_type, reg_type) &&
		IMem_Map().Need_Split_Group_No_Vsel()) { 
          SIMD_Msg(AT_MSG_SIMD_UNALIGNED_STORE, Simd_Loop(), SIMD_MTYPE_Msg(mem_type));
          return false;
        }
      } 
      if (reg_type != mem_type) {
        // non-converting loads and stores should be available for all C types
        if (ig->Is_Use() && !Simd_Ti->Type_Conversion_Possible(mem_type, reg_type)) {
          SIMD_Msg(AT_MSG_SIMD_NO_LOAD_CONV, Simd_Loop(),
                   SIMD_MTYPE_Msg(mem_type), SIMD_MTYPE_Msg(reg_type));
          return false;
        }
        if (ig->Is_Def() && !Simd_Ti->Type_Conversion_Possible(reg_type, mem_type)) {
          SIMD_Msg(AT_MSG_SIMD_NO_STORE_CONV, Simd_Loop(),
                   SIMD_MTYPE_Msg(reg_type), SIMD_MTYPE_Msg(mem_type));
          return false;
        }
      }
    }
  }
  
  return true;
}


bool
SIMD_LOOP_AT::Test_Imem_Field_Selection (void)
{
  // no need to check for proper operations on the target
  // if doing analysis
  if (AT_Analysis())
    return true;
  
  // iterate through all IMEM_GROUPS and check that it is possible to
  // do intereave/deinterleave on the current target
  for (INT g = 0; g < IMem_Map().Groups().Elements(); g++) {
    IMEM_GROUP *ig = IMem_Map().Groups()[g];
    if (!ig->Is_Vector()) {
      continue;
    }
    for (INT e = 0; e < ig->Elem_Count(); e++) {
      IMEM_ELEM *ie = ig->Elem(e);
      if (ie->Offset_Count()<2) {
        continue;
      }
      bool done_load = false;
      bool done_store = false;
      for (INT o = 0; o < ie->Offset_Count(); o++) {
        IMEM_OFFSET *io = ie->Offset(o);
        IMEM_INFO *ii = io->Imem_Info();
        if (ii->Is_Use() && !done_load) {
          if (!ie->Test_Sel_Read_Into_Regs()) {
            SIMD_Msg(AT_MSG_SIMD_NO_SELECT, ii->Lod_Sto().Bottom_nth(0),
                     V_Unroll_Factor(), SIMD_MTYPE_Msg(ii->Scalar_Type()));
            return false;
          }
          done_load = true;
        }
        if (ii->Is_Def() && !done_store) {
          if (!ie->Test_Sel_Write_Out_Regs()) {
            SIMD_Msg(AT_MSG_SIMD_NO_SELECT, ii->Lod_Sto().Bottom_nth(0),
                     V_Unroll_Factor(), SIMD_MTYPE_Msg(ii->Scalar_Type()));
            return false;
          }
          done_store = true;
        }
      }
    }
  }
  
  return true;
}

bool
SIMD_LOOP_AT::Test_Imem_Load_Reuse_Supported(IMEM_GROUP *ig)
{
    if (AT_Analysis()) {
	return true;
    }
    
    // check LS_IU
    IMEM_OFFSET *cur       = ig->Elem(0)->Offset(0);
    WN          *orig_ls   = cur->Orig_Wn();
    TYPE_ID      desc_type = WN_desc(orig_ls);
    TYPE_ID      reg_type  = Simd_Ti->Get_SIMD_Reg_Type_Scalar(desc_type);
    if (!Simd_Ti->Load_Scalar_IU_Possible(desc_type, reg_type)) {
	return false;
    }
    
    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(reg_type);
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    
    INT64 sel = simd_sel->Sel_4321();
    if (!Simd_Ti->Tie_Sel_Possible(reg_type, sel)) {
	return false;
    }

    return true;
}

bool
SIMD_LOOP_AT::Test_Imem_Load_Variable_Stride (IMEM_GROUP *ig)
{
    if (AT_Analysis()) {
	return true;
    }
    
    Is_True(ig->Variable_Stride(),
	    ("SIMD_LOOP_AT::Test_Imem_LS_XU: IMEM_GROUP is variable stride"));
    FmtAssert(ig->Elem_Count() == 1, 
	      ("SIMD_LOOP_AT::Test_Imem_LS_XU: IMEM_GROUP element count > 1"));
    
    // check LS_XU
    IMEM_OFFSET *cur       = ig->Elem(0)->Offset(0);
    WN          *orig_ls   = cur->Orig_Wn();
    TYPE_ID      desc_type = WN_desc(orig_ls);
    TYPE_ID      reg_type  = Simd_Ti->Get_SIMD_Reg_Type_Scalar(desc_type);
    if (!Simd_Ti->Load_Scalar_XU_Possible(desc_type, reg_type)) {
	return false;
    }
    
    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(reg_type);
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    
    INT64 sel = simd_sel->Sel_4321();
    if (!Simd_Ti->Tie_Sel_Possible(reg_type, sel)) {
	return false;
    }

    return true;
}

INTRINSIC 
Tie_Vector_Intrinsic (WN *wn, INT vl) {
    INTRINSIC intrn = WN_intrinsic(wn);
  
    if (simd_debug) {
	fprintf(TFile,"request for vector TIE of %s\n",INTRINSIC_name(intrn));
    }
  
    if (!INTRN_is_tie_intrinsic(intrn)) {
	return INTRINSIC_INVALID;
    }
  
    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);

    INT operands = tie_macro->num_protos();

    TI_DIR dirs[operands];
    TYPE_ID types[operands];
    INT64 imm[operands];
  
    for (INT i = 0; i < tie_macro->num_protos(); i++) {
	INT parm_idx = tie_macro->proto_to_whirl_index(i);
	Is_True(parm_idx < WN_kid_count(wn),
		("Invalid TIE parm index %d.", parm_idx));
	WN *parm = parm_idx < 0 ? NULL : WN_kid(wn, parm_idx);
	
	if (tie_macro->proto_is_immed(i)) {
	    Is_True(parm != NULL, ("Null parameter for immediate"));
	    dirs[i] = TI_DIR_IN;
	    types[i] = MTYPE_UNKNOWN;
	    Is_True(WN_operator(parm) == OPR_PARM && WN_kid_count(parm) == 1,
		    ("Expected OPR_PARM node!"));
	    WN *immed = WN_kid0(parm);
	    Is_True(WN_operator(immed) == OPR_INTCONST,
		    ("Expected constant node for tie immediate"));
	    imm[i] = WN_const_val(immed);
	} else {
	    imm[i] =0;
	    TYPE_ID scalar_type = MTYPE_UNKNOWN;
	    if (parm == NULL) {
		scalar_type = tie_macro->proto_mtype_id(tie_info, i);
	    } else {
		SIMD_EINFO *e_info = Cur_Simd_Loop->E_Info().Find(parm);
		Is_True(e_info != NULL, ("Null e_info"));
		scalar_type = e_info->Res_Type();
	    }
	    Is_True(scalar_type != MTYPE_UNKNOWN,
		    ("Unknown scalar type for TIE intrinsic operand"));

	    TYPE_ID vec_type = Simd_Ti->Simd_Reg_Type_Of_Scalar(scalar_type, vl);
	    if (vec_type == MTYPE_UNKNOWN) {
		if (simd_debug) {
		    fprintf(TFile, "Can't find a vector type for %s, vl %d\n",
			    MTYPE_name(scalar_type), vl);
		}
		return INTRINSIC_INVALID;
	    }
	    types[i] = vec_type;
	    if (tie_macro->proto_is_in(i)) {
		dirs[i] = TI_DIR_IN;
	    } else if (tie_macro->proto_is_out(i)) {
		dirs[i] = TI_DIR_OUT;
	    } else if (tie_macro->proto_is_inout(i)) {
		dirs[i] = TI_DIR_INOUT;
	    }
	}
    }
    
    INTRINSIC vintrn = Simd_Ti->Tie_NamedOp_Intrinsic_Exp(tie_macro->name(),
							  operands, dirs, types, imm);
    return vintrn;
}


bool
SIMD_LOOP_AT::Generate_Tie_Op(WN *expr, WN *stmt) {
    INTRINSIC intrn = (INTRINSIC) WN_intrinsic(expr);
    if (!INTRN_is_tie_intrinsic(intrn)) {
	return false;
    }
  
    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
  
    INTRINSIC intrin_id = Tie_Vector_Intrinsic(expr);
    Is_True(intrin_id != INTRINSIC_INVALID,
	    ("Can't find intrinsic for %s", INTRINSIC_name(intrin_id)));
    
    INT whirl_operands = tie_macro->num_protos() - tie_macro->num_output_protos();
    Is_True(whirl_operands == WN_kid_count(expr),
	    ("Mismatch TIE intrinsic %s operand count %d != %d",
	     whirl_operands, WN_kid_count(expr)));
    SIMD_EINFO *e_info = Get_E_Info(expr);
    
    WN *apr[whirl_operands];
    for (INT i = 0; i < tie_macro->num_protos(); i++) {
	INT parm_idx = tie_macro->proto_to_whirl_index(i);
	if (parm_idx < 0) {
	    continue;
	}
	Is_True(parm_idx < whirl_operands,
		("Invalid TIE intrinsic whirl parm index %d, max %d",
		 parm_idx, whirl_operands));
	WN *parm = WN_kid(expr, parm_idx);
	if (tie_macro->proto_is_immed(i)) {
	    // if immediate, copy the original constant
	    apr[parm_idx] = LWN_Copy_Tree(parm);
	} else {
	    SIMD_EINFO *c_info = Get_E_Info(parm);
	    Is_True(c_info != NULL, ("No child e_info"));
	    apr[parm_idx] = LWN_CreateParm(c_info->Simd_Reg()->Type(),
                                           c_info->Simd_Reg()->Simd_Preg_Ldid(),
					   MTYPE_To_TY(c_info->Simd_Reg()->Type()),
					   WN_PARM_BY_VALUE);
	}
    }

    TYPE_ID mtype = e_info->Simd_Reg()->Type();
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    WN *tie = LWN_Create_Intrinsic(intrin_op, intrin_id, whirl_operands, apr);
    WN *sto = e_info->Simd_Reg()->Simd_Preg_Stid(tie);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = WN_Whirl_Linenum(stmt);
  
    return true;
}

SIMD_EINFO *
SIMD_LOOP_AT::Simd_Transform_Intrinsic(WN *expr, WN *stmt, SIMD_INFO *simd_info) {
    Is_True(WN_operator(expr)==OPR_INTRINSIC_OP,
	    ("Can't handle intrinsic calls yet"));
  
    SIMD_EINFO *e_info = Get_E_Info(expr);
    Is_True(e_info != NULL, ("e_info == NULL"));
  
    if (!e_info->Is_Vector()) {
	// handle invariant expressions in the base routine
	return SIMD_LOOP::Simd_Transform(expr,stmt,simd_info);
    }
  
    if (e_info->Get_Res_Reg() != NULL) {
	// already transformed
	return e_info;
    }
  
    TYPE_ID res_type = e_info->Res_Type();
  
    INTRINSIC intrn = (INTRINSIC) WN_intrinsic(expr);
    Is_True(INTRN_is_tie_intrinsic(intrn),
	    ("Expected TIE intrinsic, not %s",INTRINSIC_name(intrn)));
  
    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(intrn);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
   
    // transform the kids first
    for (INT kid_idx=0; kid_idx<WN_kid_count(expr); kid_idx++) {
	if (!tie_macro->proto_is_immed(tie_macro->whirl_to_proto_index(kid_idx))) {
	    Simd_Transform(WN_kid(expr,kid_idx),stmt,simd_info);
	}
    }
  
    Generate_Simd_Reg_Info(e_info, simd_info);
  
    bool tie_op_ok = Generate_Tie_Op(expr,stmt);
    Is_True(tie_op_ok,("Couldn't generate a tie operation"));

    return e_info;
}


SIMD_EINFO *
SIMD_LOOP_AT::Simd_Transform_Parm (WN *expr, WN *stmt, SIMD_INFO *simd_info) {
    Is_True(WN_operator(expr) == OPR_PARM,
	    ("Expected parm operator, not %s", OPERATOR_name(WN_operator(expr))));
    
    SIMD_EINFO *e_info = Get_E_Info(expr);
    Is_True(e_info != NULL, ("e_info == NULL"));
    
    if (e_info->Get_Res_Reg() != NULL) {
	// already transformed
	return e_info;
    }
    
    SIMD_EINFO *c_info = Simd_Transform(WN_kid0(expr), stmt, simd_info);
    e_info->Copy_Simd_Reg_Info(c_info);
    return e_info;
}


SIMD_EINFO *
SIMD_LOOP_AT::Simd_Transform_Outpart (WN *expr, WN *stmt, SIMD_INFO *simd_info) {
    Is_True(WN_operator(expr) == OPR_OUTPART,
	    ("Expected outpart operator, not %s", OPERATOR_name(WN_operator(expr))));
    
    SIMD_EINFO *e_info = Get_E_Info(expr);
    Is_True(e_info != NULL, ("e_info == NULL"));

    if (e_info->Get_Res_Reg() != NULL) {
	// already transformed
	return e_info;
    }
    
    SIMD_EINFO *c_info = Simd_Transform(WN_kid0(expr), stmt, simd_info);
    
    Generate_Simd_Reg_Info(e_info, simd_info);
    
    INT reg_count = e_info->Reg_Count();
    Is_True(reg_count == c_info->Reg_Count(),
	    ("Mismatched result register counts (%d != %d)",
	     reg_count, c_info->Reg_Count()));

    // Extract the field from the kid result register using OPR_OUTPART.
    for (INT i = 0; i < reg_count; i++) {
	TYPE_ID vec_type = e_info->Get_Res_Reg(i)->Type();
	
	WN *part_ldid = LWN_CreateOutpart(c_info->Get_Res_Reg(i)->Simd_Preg_Ldid(),
					  WN_outpart(expr), vec_type);
	WN *part_stid = e_info->Get_Res_Reg(i)->Simd_Preg_Stid(part_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, part_stid);
	WN_linenum(part_stid) = LWN_Get_Linenum(stmt);
    }
    
    return e_info;
}


SIMD_EINFO *
SIMD_LOOP_AT::Simd_Transform(WN *expr, WN *stmt, SIMD_INFO *simd_info) {
    OPERATOR oper = WN_operator(expr);
    switch (oper) {
    case OPR_INTRINSIC_CALL:
    case OPR_INTRINSIC_OP:
	return Simd_Transform_Intrinsic(expr, stmt, simd_info);
    case OPR_PARM:
	return Simd_Transform_Parm(expr, stmt, simd_info);
    case OPR_OUTPART:
	return Simd_Transform_Outpart(expr, stmt, simd_info);
    }    
    
    return SIMD_LOOP::Simd_Transform(expr, stmt, simd_info);
}


WN*
SIMD_LOOP_AT::Load_Shift(SIMD_EINFO *e_info, WN *stmt, SIMD_INFO *simd)
{
    Is_True(e_info->Simd_Exprs()->Elements()==1, ("Unexpected double type"));
    Is_True(e_info->Pre_Load(), ("Expecting pre-load"));
    
    WN  *simd_expr = e_info->Simd_Expr(0);

    /* increment the load address depending on the load type */
    /* (1) move the e_info->Pre_Load() out of the loop */
    WN *old_preload = e_info->Pre_Load();
    WN *old_addr    = WN_prev(old_preload);
    WN *t_loop      = Enclosing_Do_Loop(old_addr);
    Is_True(t_loop, ("Cannot find enclosing loop"));
    
    Is_True(WN_operator(old_addr) == OPR_STID, 
	    ("Expecting address assignment"));
    
    /* recompute the alignment */
    IMEM_INFO *imem = e_info->IMem();
    SIMD_PREG *addr_preg = 
	imem->Imem_Offset()->Parent_Group()->Load_Addr_Reg();
    SIMD_PREG *align_preg =
	imem->Imem_Offset()->Parent_Group()->Load_Align_Reg();
    
    /* not aligned */
    if (!imem || !imem->Is_2D_Aligned()) {
	WN *rhs         = WN_kid0(old_addr);
	Replace_Ldid_With_Exp_Copy(WN_index(t_loop), rhs,
				   WN_kid0(WN_start(t_loop)),
				   Du_Mgr, Array_Dependence_Graph);
	
	/* move the address assignment out */
	WN *old_blk     = LWN_Get_Parent(old_addr);
	WN *new_blk     = LWN_Get_Parent(t_loop);
	LWN_Extract_From_Block(old_blk, old_addr);
	LWN_Insert_Block_Before(new_blk, t_loop, old_addr);

	WN *wn_cur = old_preload;
	    
	/* move the old_preload out */
	for (INT cnt = 0; cnt < 3; cnt++) {
	    WN *tmp = wn_cur;
	    wn_cur = WN_next(wn_cur);
	    LWN_Extract_From_Block(old_blk, tmp);
	    LWN_Insert_Block_Before(new_blk, t_loop, tmp);
	}

	/* alignment register load, generate another load at addr+8 */
	Is_True(simd_expr == stmt, ("Mismatched load"));
	Is_True(WN_operator(simd_expr) == OPR_INTRINSIC_CALL,
		("Expecting alignment register load"));
	
	WN *pre_load = 
	    simd->Generate_LoadA_Update(e_info->Res_Type(), t_loop, addr_preg,
					e_info->Get_Res_Reg(), align_preg,
					simd->Get_SIMD_Mem_Bytes_Scalar(
					    e_info->Res_Type()));
	
	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(LWN_Get_Parent(pre_load)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(pre_load);
	}
	    
	/* create a new PREG to hold the address for LS */
	SIMD_PREG *old_addr_preg = addr_preg;
	addr_preg = Gen_Symbol(addr_preg->Type(), Pool(), "addr");

	/* Add vector byte size to the old address */
	WN *cst = WN_CreateIntconst(OPC_I4INTCONST,
				    2*simd->Get_SIMD_Mem_Bytes_Scalar(
					e_info->Res_Type())-
				    MTYPE_byte_size(e_info->Res_Type()));
	
	OPCODE opc   = OPCODE_make_op(OPR_ADD, addr_preg->Type(), MTYPE_V);
	WN *new_addr = addr_preg->Simd_Preg_Stid(
	    LWN_CreateExp2(opc, old_addr_preg->Simd_Preg_Ldid(), cst));

	/* insert it after the old address */
	LWN_Insert_Block_After(new_blk, old_addr, new_addr);
	
    } else { /* is aligned */
	/* extract the original address */
	WN *old_rhs = WN_kid0(old_addr);

	/* Add vector byte size to the old address */
	WN *cst = WN_CreateIntconst(OPC_I4INTCONST,
				    simd->Get_SIMD_Mem_Bytes_Scalar(
					e_info->Res_Type()));
	
	OPCODE opc = OPCODE_make_op(OPR_ADD, addr_preg->Type(), MTYPE_V);
	WN *new_rhs = LWN_CreateExp2(opc, old_rhs, cst);
	WN_kid0(old_addr) = new_rhs;
	LWN_Set_Parent(new_rhs, old_addr);
	    
	Replace_Ldid_With_Exp_Copy(WN_index(t_loop), new_rhs,
				   WN_kid0(WN_start(t_loop)),
				   Du_Mgr, Array_Dependence_Graph);
	    
	/* move the address assignment out */
	WN *old_blk     = LWN_Get_Parent(old_addr);
	WN *new_blk     = LWN_Get_Parent(t_loop);
	LWN_Extract_From_Block(old_blk, old_addr);
	LWN_Insert_Block_Before(new_blk, t_loop, old_addr);
	
	WN *wn_cur = old_preload;

	/* delete the old pre_load */
	for (INT cnt = 0; cnt < 3; cnt++) {
	    WN *tmp = wn_cur;
	    wn_cur = WN_next(wn_cur);
	    LWN_Extract_From_Block(old_blk, tmp);
	    LWN_Delete_Tree(tmp);
	}
	
	TYPE_ID desc_type = simd->Get_SIMD_Mem_Type_Scalar(e_info->Res_Type());
	SIMD_PREG *align_addr = imem->Imem_Offset()->Parent_Group()->
	    Load_Addr_Reg();
	
	WN *pre_load = 
	    simd->Generate_Load(
		OPR_ILOAD,
		desc_type,
		t_loop, 
		align_addr->Simd_Preg_Ldid(),
		0, 
		e_info->Get_Res_Reg(),
		desc_type);
	
	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(LWN_Get_Parent(pre_load)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(pre_load);
	}

	/* Add vector byte size to the old address */
	cst = WN_CreateIntconst(OPC_I4INTCONST,
				simd->Get_SIMD_Mem_Bytes_Scalar(
				    e_info->Res_Type()) 
				- MTYPE_byte_size(e_info->Res_Type()));
	
	WN *new_addr = addr_preg->Simd_Preg_Stid(
	    LWN_CreateExp2(opc, addr_preg->Simd_Preg_Ldid(), cst));
	
	/* insert it after the pre_load address */
	LWN_Insert_Block_Before(LWN_Get_Parent(t_loop), t_loop, new_addr);
    }
    
    SIMD_PREG *load_ahead_preg = e_info->Load_Ahead();
    if (load_ahead_preg == NULL) {
	load_ahead_preg = Gen_Symbol(simd, e_info->Res_Type(), Pool());
	e_info->Set_Load_Ahead(load_ahead_preg);
    }

    /* generate a LS.IU into e_info->Load_Ahead() */
    WN *load_wn = 
	simd->Generate_Update_Load(OPR_ILOAD, 
				   simd->Get_SIMD_Reg_Type_Scalar(
				       e_info->Res_Type()),
				   e_info->Res_Type(),
				   stmt, 
				   addr_preg,
				   e_info->Load_Ahead(),
				   MTYPE_byte_size(e_info->Res_Type()),
				   1 /* scalar */);
	
    /* generate a shift left (Get_Res_Reg()||Pre_Load()) << 1 to 
       Get_Res_Reg() and insert it at the end of loop body */
    simd->Shift_Into_Reg(e_info->Load_Ahead(), e_info->Get_Res_Reg(),
			 LWN_Get_Parent(stmt), NULL, WN_Whirl_Linenum(stmt));
    return load_wn;
}

void
SIMD_LOOP_AT::Generate_Load_Shift(WN *wn, SIMD_INFO *simd, 
				  E_Candidate &cand, WN *& wn_next)
{
    OPCODE opc = WN_opcode(wn);
    if (opc == OPC_BLOCK) {
	WN *kid = WN_first(wn);
	while (kid) {
	    WN *kid_next = WN_next(kid);
	    Generate_Load_Shift(kid, simd, cand, kid_next);
	    kid = kid_next;
	}
	return;
    } else if (OPCODE_is_scf(opc)) {
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    WN *dummy = NULL;
	    WN *kid = WN_kid(wn, i);
	    Generate_Load_Shift(kid, simd, cand, dummy);
	}
    }

    WN *cand_wn = cand.Cur_Wn();
    SIMD_EINFO *e_info = cand.Cur_EInfo();
    while (cand_wn != NULL && (!e_info->Load_Reuse())) {
	cand.Get_Next();
	cand_wn = cand.Cur_Wn();
	e_info = cand.Cur_EInfo();
    }

    if (cand_wn == NULL) {
	return;
    }

    WN *cand_stmt = (OPCODE_is_stmt(WN_opcode(cand_wn))) ? 
	cand_wn : LWN_Get_Parent(cand_wn);
    Is_True(OPCODE_is_stmt(WN_opcode(cand_stmt)), ("Not a statement"));

    if (cand_stmt == wn) {
	if (e_info->Pre_Load()) {
#if 0
	    bool is_call = OPCODE_is_call(WN_opcode(wn));
	    ARRAY_DIRECTED_GRAPH16 *adg = Array_Dependence_Graph;
	    
	    WN       *new_ld = Load_Shift(e_info, wn, simd);
	    VINDEX16  newv   = Array_Dependence_Graph->Add_Vertex(new_ld);

	    VINDEX16 v = adg->Get_Vertex(wn);
	    Is_True(v != 0, ("Cannot find the original in the Dgraph"));
		
	    /* copy the out edges */
	    EINDEX16 edge = adg->Get_Out_Edge(v);
	    while (edge) {
		VINDEX16    sinkv  = adg->Get_Sink(edge);
		DEPV_ARRAY *o_depv = adg->Depv_Array(edge);
		DEPV_ARRAY *depv   = Create_DEPV_ARRAY(o_depv, Pool());
		if (adg->Add_Edge(newv, sinkv, depv) == 0) {
		    Set_DGraph_Overflow();
		}
		edge = adg->Get_Next_Out_Edge(edge);
	    }
		
	    /* get in the edges, copy dependences */
	    edge = adg->Get_In_Edge(v);
	    while (edge) {
		VINDEX16    srcv   = adg->Get_Source(edge);
		DEPV_ARRAY *o_depv = adg->Depv_Array(edge);
		DEPV_ARRAY *depv   = Create_DEPV_ARRAY(o_depv, Pool());
		if (adg->Add_Edge(srcv, newv, depv) == 0) {
		    Set_DGraph_Overflow();
		}
		edge = adg->Get_Next_In_Edge(edge);
	    }
	    
	    /* delete the loads from LV16A_IU results
	       stid (ldid -1)
	       stid (ldid -2)                
	       stid (ldid -3) */
	    WN *next = WN_next(wn);
	    WN *nn   = WN_next(next);
	    WN *nnn  = WN_next(nn);
	    LWN_Delete_Tree(next);
	    LWN_Delete_Tree(nn);
	    LWN_Delete_Tree(nnn);
	    
	    /* move the upper level cursor */
	    wn_next = WN_next(nnn);
	    
	    /* delete the tree */
	    LWN_Delete_Tree(wn);
#endif
	    ;
	} else {/* load coefficient, generating a updating load */
	    WN *simd_expr = e_info->Simd_Expr(0);
	    if (WN_operator(simd_expr) == OPR_CVT) {
		simd_expr = WN_kid0(simd_expr);
	    }
	    Is_True(WN_operator(simd_expr) == OPR_ILOAD, ("Expect ILOAD"));

	    /* generate an address outside of the loop */
	    WN *old_addr = WN_kid0(simd_expr);
	    WN *t_loop = Enclosing_Do_Loop(simd_expr);
	    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), old_addr,
				       WN_kid0(WN_start(t_loop)),
				       Du_Mgr, Array_Dependence_Graph);
	    
	    WN        *old_blk  = LWN_Get_Parent(old_addr);
	    WN        *new_blk  = LWN_Get_Parent(t_loop);
	    SIMD_PREG *addr_reg = Generate_Align_Addr_Reg(simd, Pool());
	    INT  inc = MTYPE_byte_size(e_info->Res_Type());
	    WN  *cst = WN_CreateIntconst(OPC_I4INTCONST, inc);
	    OPCODE opc = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	    WN *addr = LWN_CreateExp2(opc, old_addr, cst);
	    WN *stid = addr_reg->Simd_Preg_Stid(addr);
	    LWN_Insert_Block_Before(new_blk, t_loop, stid);
	    
	    WN *res = Simd_Info->Generate_Update_Load(OPR_ILOAD,
						      WN_rtype(simd_expr),
						      WN_desc(simd_expr),
						      wn,
						      addr_reg,
						      e_info->Get_Res_Reg(),
						      inc, 1);
	    Array_Dependence_Graph->Add_Vertex(res);
	    WN_kid0(simd_expr) = NULL;
	    LWN_Delete_Tree(wn);
	}
	cand.Get_Next();
    }
}

// generate shift SEL for Has_Reuse() memory access 'A[i+j]'
void
SIMD_LOOP_AT::Further_Unroll_Loop(WN *loop, INT level, SIMD_INFO *simd,
				  EST_REGISTER_USAGE est_register_usage,
				  SX_INFO* pinfo,
				  INT pinfo_depth,
				  BOOL no_further_unroll,
				  SX_INFO** wdpinfo_ptr)
{
#if 1
    return;
#else
    // we now directly generate load shift from IMEM_INFO::Has_Reuse,
    // hence the following code is obsolete.
    Is_True(level < Num_Loops(), ("Cannot find current loop"));
    Is_True(Unroll_Candidate(level)->Elements() > 0, ("No unroll elements"));
    
    E_Candidate cand(Unroll_Candidate(level));
    Is_True(cand.Cur_Wn() != NULL, ("Cannot find combine candidates"));

    WN *dummy = NULL;
    Generate_Load_Shift(loop, simd, cand, dummy);
#endif
}


bool Is_Shift (WN *expr, bool &immed, INT &shift_amount)
{
    OPERATOR oper = WN_operator(expr);
    if (oper != OPR_DIV && oper != OPR_ASHR &&
	oper != OPR_LSHR && oper != OPR_SHL) {
	return false;
    }
    
    immed = false;
    WN *wn_sa = WN_kid1(expr);
    if (WN_operator(wn_sa) != OPR_INTCONST) {
	return oper != OPR_DIV;
    }
    
    INT64 val = WN_const_val(wn_sa);
    INT64 sa = val;
    if (oper == OPR_DIV) {
      WN *kid0 = WN_kid0(expr);
      TYPE_ID rtype = WN_rtype(kid0);
      TYPE_ID desc = WN_desc(kid0);
      // skip over converts from unsigned to signed
      if (WN_operator(kid0) == OPR_CVT &&
         ((rtype == MTYPE_I1 && desc == MTYPE_U1) ||
          (rtype == MTYPE_I2 && desc == MTYPE_U2) ||
          (rtype == MTYPE_I4 && desc == MTYPE_U4) ||
          (rtype == MTYPE_I8 && desc == MTYPE_U8))) {
               kid0 = WN_kid0(kid0);
	       rtype = desc;
      }

      if (!MTYPE_is_integral(rtype) ||
          MTYPE_is_signed(rtype) ||
          val <= 0 ||
          !LNO_IS_POWER_OF_TWO(val)) {
        return false;
      }
      
      sa = 0;
      while ((val & 0x1) != 0x1) {
        val >>= 1;
        sa++;
      }
    }
    
    if (sa >= 0 && sa <= 64) {
	immed = true;
	shift_amount = (INT)sa;
    }

    return true;
}


bool
SIMD_LOOP_AT::Check_Guards (INT vl)
{
  for (INT i = 0; i < 4; i++) {
    TYPE_ID scalar_type = _guard_types[i];
    if (scalar_type == MTYPE_UNKNOWN)
      break;
    
    INT req_guards =
      (MTYPE_bit_size(scalar_type) == 8) ? _req_int8_guards : _req_int16_guards;
    
    if (req_guards == 0)
      continue;
    
    INT guards = Simd_Ti->Guard_Bits(scalar_type, vl);
    if (guards < req_guards)
      return false;
    
  }
  
  return true;
}


void
SIMD_LOOP_AT::Collect_Guard_Type (TYPE_ID type)
{
  if (!MTYPE_is_integral(type))
    return;

  if (MTYPE_bit_size(type) != 8 && MTYPE_bit_size(type) != 16)
    return;

  for (INT i = 0; i < 4; i++) {
    if (_guard_types[i] == MTYPE_UNKNOWN) {
      _guard_types[i] = type;
      return;
    }
    
    if (_guard_types[i] == type) {
      return;
    }
  }
  
  Is_True(0, ("No room for type %s in the guard types.", MTYPE_name(type)));
}


/* Recursively find the guard bits given 'ginfo'. */
GUARD_RANGE
SIMD_LOOP_AT::Find_Guards_Rec (WN *wn, GUARD_INFO *ginfo)
{
  GUARD_RANGE null_range;
  
  OPERATOR oper = WN_operator(wn);
  
  switch (oper)
  {
  case OPR_BLOCK:
    for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
      /* Compute the guard bits based on the expressions in each
	 statement. */
      ginfo->reset_range();
      Find_Guards_Rec(kid, ginfo);
      ginfo->update_guards();
    }
    
    return null_range;
    
  case OPR_DO_LOOP:
    Find_Guards_Rec(WN_do_body(wn), ginfo);
    return null_range;

  case OPR_IF:
    ginfo->reset_range();
    Find_Guards_Rec(WN_if_test(wn), ginfo);
    ginfo->update_guards();
    
    Find_Guards_Rec(WN_then(wn), ginfo);
    Find_Guards_Rec(WN_else(wn), ginfo);
    
    return null_range;
    
  default:
    break;
  }
  
  SIMD_EINFO *e_info = E_Info().Find(wn);
  if (!e_info)
    return null_range;
  
  INT type_bits = ginfo->type_bits();
  GUARD_RANGE &grange = ginfo->range();
  
  TYPE_ID res_type = e_info->Res_Type();
  INT res_bit_size = e_info->Bit_Size();
  bool res_signed = MTYPE_is_signed(res_type);
  bool res_integral = MTYPE_is_integral(res_type);
  
  /* If invariant expression, return the maximum range for the type. */
  if (oper != OPR_PARM && oper != OPR_INTCONST &&
      !Varies_With_Loop(wn, Simd_Loop_Level())) {
    
    if (res_integral && res_bit_size == type_bits)
      return GUARD_RANGE(res_signed, type_bits);
    
    return null_range;
  }
  
  if (oper == OPR_INTRINSIC_CALL ||
      oper == OPR_INTRINSIC_OP ||
      !res_integral) {
    for (INT kid_idx = 0; kid_idx < WN_kid_count(wn); kid_idx++) {
      GUARD_RANGE range = Find_Guards_Rec(WN_kid(wn, kid_idx), ginfo);
      grange.union_range(range);
    }
    
    return null_range;
  }
  
  switch (oper) {
  case OPR_TRUNC:
  case OPR_LDID:
  case OPR_ILOAD:
    if (res_bit_size != type_bits)
      return null_range;
    
    return GUARD_RANGE(res_signed, type_bits);
    
  case OPR_INTCONST:
  {
    if (res_bit_size != type_bits)
      return null_range;
    
    INT64 val = WN_const_val(wn);
    return GUARD_RANGE(res_signed ? GUARD_RANGE::F_SIGNED : GUARD_RANGE::F_UNSIGNED,
		       val, val);
  }
  
  case OPR_NE:
  case OPR_GE:
  case OPR_GT:
  case OPR_EQ:
  case OPR_LE:
  case OPR_LT:
  {
    GUARD_RANGE range_left = Find_Guards_Rec(WN_kid0(wn), ginfo);
    GUARD_RANGE range_right = Find_Guards_Rec(WN_kid1(wn), ginfo);
    if (res_bit_size >= type_bits) {
      grange.union_range(range_left);
      grange.union_range(range_right);
    }
    
    return null_range;
  }
  
  case OPR_CVTL:
  case OPR_CVT:
  {
    GUARD_RANGE range = Find_Guards_Rec(WN_kid0(wn), ginfo);
    
    if (oper == OPR_CVTL) {
      INT cvtl_bits = WN_cvtl_bits(wn);
      if (cvtl_bits < type_bits) {
	return null_range;
      }
    }
    
    if (res_bit_size < type_bits) {
      return null_range;
    }
    
    if (res_bit_size > type_bits) {
      grange.union_range(range);
      return null_range;
    }
    
    /* The result is 'type_bits'. Check the source type. */
    SIMD_EINFO *c_info = E_Info().Find(WN_kid0(wn));
    Is_True(c_info, ("Null child e_info for convert"));
    
    TYPE_ID c_res_type = c_info->Res_Type();
    if (!MTYPE_is_integral(c_res_type)) {
      grange.union_range(range);
      return null_range;
    }
    
    if (c_res_type == res_type) {
      return range;
    }
    
    INT c_res_bit_size = MTYPE_bit_size(c_res_type);
    if (c_res_bit_size != res_bit_size) {
      Is_True(res_bit_size == type_bits, ("Bad size checks."));
      return GUARD_RANGE(res_signed, type_bits);
    }
    
    /* Type conversion from signed to unsigned requires extra bits to preserve
       the sign. */
    Is_True(res_signed != MTYPE_is_signed(c_res_type),
	    ("Sign mismatch (%s, %s).", MTYPE_name(res_type), MTYPE_name(c_res_type)));
    
    if (res_signed) {
      range.set_signed();
    } else {
      range.set_unsigned();
    }
    
    return range;
  }
  
  case OPR_STID:
  case OPR_ISTORE:
  case OPR_PARM:
  {
    GUARD_RANGE range = Find_Guards_Rec(WN_kid0(wn), ginfo);
    if (res_bit_size > type_bits) {
      grange.union_range(range);
    }
    
    return null_range;
  }
  
  case OPR_DIV:
  case OPR_ASHR:
  case OPR_LSHR:
  case OPR_SHL:
  {
    GUARD_RANGE range = Find_Guards_Rec(WN_kid0(wn), ginfo);
    
    if (res_bit_size > type_bits) {
      grange.union_range(range);
      return null_range;
    }
    
    /* We can be smarter here and take into account the shift-amount and any
       possible truncation up the expression tree. But we're conservative. */
    if (res_bit_size == type_bits) {
      grange.union_range(range);
      
      bool immed = false;
      INT shift_amount = 0;
      bool is_shift = Is_Shift(wn, immed, shift_amount);
      Is_True(is_shift, ("Not a shift %s", OPERATOR_name(oper)));
      
      if (immed) {
	if (oper == OPR_SHL) {
          if (shift_amount <= res_bit_size / 4) {
            range.shl_range(shift_amount);
          } else {
            /* The result type of the shift must have already been promoted
               by Screen_Operator_Compute_Size. */
          }
        } else if (oper == OPR_ASHR) {
	  range.ashr_range(shift_amount);
	} else {
	  range.lshr_range(shift_amount);
	}
      }
      
      return range;
    }
    
    return null_range;
  }
  
  case OPR_BNOT:
  {
    GUARD_RANGE range = Find_Guards_Rec(WN_kid0(wn), ginfo);
    
    INT guards = range.guards(type_bits);
    GUARD_RANGE wrange(res_signed, type_bits + guards);
    
    if (res_bit_size > type_bits) {
      grange.union_range(wrange);
      return null_range;
    }
    
    return wrange;
  }
  
  case OPR_BXOR:
  case OPR_BAND:
  case OPR_BIOR:
  {
    GUARD_RANGE range_left = Find_Guards_Rec(WN_kid0(wn), ginfo);
    GUARD_RANGE range_right = Find_Guards_Rec(WN_kid1(wn), ginfo);
    
    INT guards_left = range_left.guards(type_bits);
    INT guards_right = range_right.guards(type_bits);
    INT guards = MAX(guards_left, guards_right);
    GUARD_RANGE wrange(res_signed, type_bits + guards);
    
    if (res_bit_size > type_bits) {
      grange.union_range(wrange);
      return null_range;
    }
    
    return wrange;
  }
  
  case OPR_NEG:
  case OPR_ABS:
  {
    GUARD_RANGE range = Find_Guards_Rec(WN_kid0(wn), ginfo);
    grange.union_range(range);
    
    if (res_bit_size == type_bits) {
      if (oper == OPR_NEG) {
	range.negate_range();
      } else {
	range.abs_range();
      }
      
      return range;
    }
    
    return null_range;
  }
  
  case OPR_OUTPART:
    Find_Guards_Rec(WN_kid0(wn), ginfo);
    if (res_bit_size == type_bits)
      return GUARD_RANGE(res_signed, type_bits);
    
    return null_range;
    
  case OPR_ADD:
  case OPR_SUB:
  {
    GUARD_RANGE range_left = Find_Guards_Rec(WN_kid0(wn), ginfo);
    GUARD_RANGE range_right = Find_Guards_Rec(WN_kid1(wn), ginfo);
    
    if (res_bit_size == type_bits) {
      if (oper == OPR_ADD) {
	range_left.add_range(range_right);
      } else {
	range_left.sub_range(range_right);
      }
      
      return range_left;
    }
    
    if (res_bit_size > type_bits) {
      grange.union_range(range_left);
      grange.union_range(range_right);
    }
    
    return null_range;
  }
  
  case OPR_MAX:
  case OPR_MIN:
  {
    GUARD_RANGE range_left = Find_Guards_Rec(WN_kid0(wn), ginfo);
    GUARD_RANGE range_right = Find_Guards_Rec(WN_kid1(wn), ginfo);
    
    if (res_bit_size < type_bits)
      return null_range;
    
    grange.union_range(range_left);
    grange.union_range(range_right);
    
    if (res_bit_size > type_bits)
      return null_range;
    
    if (oper == OPR_MAX) {
      range_left.max_range(range_right);
    } else {
      range_left.min_range(range_right);
    }
    
    return range_left;
  }
  
  case OPR_MPY:
  {
    GUARD_RANGE range_left = Find_Guards_Rec(WN_kid0(wn), ginfo);
    GUARD_RANGE range_right = Find_Guards_Rec(WN_kid1(wn), ginfo);
    
    if (type_bits == 16 && res_bit_size == 16)
      return GUARD_RANGE(res_signed, 16 + ginfo->int8_guards() * 2);
    
    if (res_bit_size > type_bits) {
      grange.union_range(range_left);
      grange.union_range(range_right);
    }
    
    return null_range;
  }
  
  default:
    FmtAssert(0, ("Unexpected operator %s", OPERATOR_name(oper)));
    break;
  }
  
  return null_range;
}


void
SIMD_LOOP_AT::Find_Guard_Bits (WN *wn, INT &int8_guards, INT &int16_guards)
{
  GUARD_INFO ginfo;
  
  ginfo.set_type_bits(8);
  Find_Guards_Rec(wn, &ginfo);

  ginfo.set_type_bits(16);
  Find_Guards_Rec(wn, &ginfo);
  
  int8_guards = ginfo.int8_guards();
  int16_guards = ginfo.int16_guards();
}


void
SIMD_EINFO_AT::Generate_Simd_Expr(bool unary, WN *expr, WN *stmt, 
				  SIMD_EINFO *c1_info, SIMD_INFO *simd,
				  SIMD_EINFO *c2_info, INT idx) {
  OPERATOR op = WN_operator(expr);
  
  if (unary || c2_info!=NULL) {
    SIMD_EINFO::Generate_Simd_Expr(unary,expr,stmt,c1_info,simd,c2_info,idx);
    return;
  }
  
  // handle shift
  bool immed = false;
  INT shift_amount = 0;
  bool is_shift = Is_Shift(expr, immed, shift_amount);
  Is_True(is_shift, ("Not a shift %s", OPERATOR_name(op)));

  OPERATOR shift_op = (op == OPR_DIV) ? OPR_LSHR : op;
  TYPE_ID res_type  = Res_Type();
  TYPE_ID type      = Simd_Reg()->Type();
  
  WN *wn_sa = NULL;
  INTRINSIC intrin_id = INTRINSIC_INVALID;
  if (immed) {
      // Try to generate shift immediate.
      wn_sa = WN_CreateIntconst(OPC_I4INTCONST, shift_amount);
      intrin_id = Simd_Ti->Tie_Shift_Intrinsic_Imm(shift_op, type, shift_amount);
  } else {
      /* extract the shift amount out and preserve the dependence
	 and invariant information */
      WN *kid1 = WN_kid1(expr);
      wn_sa = LWN_Copy_Tree(kid1, TRUE, LNO_Info_Map);
      LWN_Copy_Def_Use(kid1, wn_sa, Du_Mgr);
      Simd_Copy_Dependence(kid1, wn_sa);
      INVAR_TABLE *invar_table = Cur_Simd_Loop->Invar_Table();
      Simd_Copy_Invariant(kid1, wn_sa, invar_table);
      
      if (WN_rtype(wn_sa) != MTYPE_I4) {
	  OPCODE opc_cvt = OPCODE_make_op(OPR_CVT, MTYPE_I4, WN_rtype(wn_sa));
	  wn_sa = LWN_CreateExp1(opc_cvt, wn_sa);
	  Simd_Copy_Invariant_Top_Level(kid1, wn_sa, invar_table);
      }
  }

  if (intrin_id == INTRINSIC_INVALID) {
      intrin_id = Simd_Ti->Tie_Shift_Intrinsic(shift_op, type);
  }
  
  Is_True(intrin_id != INTRINSIC_INVALID, ("Can't find shift operator"));
  
  WN* apr[2];
  apr[0] = LWN_CreateParm(type, c1_info->Gen_Lod_Reg(Eprop_Normal, idx),
			  MTYPE_To_TY(type), WN_PARM_BY_VALUE);
  apr[1] = LWN_CreateParm(MTYPE_I4, wn_sa,
			  MTYPE_To_TY(MTYPE_I4), WN_PARM_BY_VALUE);
  OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, type, MTYPE_V);
  WN  *op_node = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
  
  WN *sto     = Simd_Reg(idx)->Simd_Preg_Stid(op_node);
  LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
  WN_linenum(sto) = LWN_Get_Linenum(stmt);
}


/*---------------------------------------------------------------------------*
 * Generate MULA/MULS                                                        *
 *---------------------------------------------------------------------------*/
void 
SIMD_EINFO_AT::Generate_Mula (WN *expr, WN *child2, WN *stmt,
                              SIMD_EINFO *c1_info, SIMD_LOOP *doinfo,
                              SIMD_INFO *simd)
{
  WN *kid0 = WN_kid0(child2);
  WN *kid1 = WN_kid1(child2);
  
  SIMD_EINFO *s1_info = doinfo->Simd_Transform(kid0, stmt, simd);
  SIMD_EINFO *s2_info = doinfo->Simd_Transform(kid1, stmt, simd);
  
  Generate_Mula_Expr(expr, stmt, c1_info, s1_info, s2_info, child2, simd);
  Is_True(!Mulabc(), ("Unexpected MULABC set."));
}

/*---------------------------------------------------------------------------*
 * Generate MULA/MULS using intrinsic calls                                  *
 *---------------------------------------------------------------------------*/
WN *
SIMD_EINFO_AT::Generate_Mula_Expr (WN *expr, WN *stmt,
                                   SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
                                   SIMD_EINFO *s2_info, WN *mul,
                                   SIMD_INFO  *simd, INT idx)
{
    Is_True(WN_operator(mul) == OPR_MPY, ("Not a multiply"));
    Is_True(s1_info && s2_info, ("children info missing"));
    Is_True(idx == 0, ("SIMD_EINFO_AT: Illegal MUL part specification"));

    /* generate MULR/MULA/MULS */
    WN       *newExpr = NULL;
    OPERATOR  op      = WN_operator(expr);
    OPERATOR  op1     = (op == OPR_ADD) ? OPR_MADD : OPR_MSUB;
    Is_True(op == OPR_ADD || op == OPR_SUB, ("Unexpected operator"));
    EINFO_PROP prop   = Get_Required_Operand_Prop(true);

    Is_True(!c1_info->Is_Round_Reg(), ("Unexpected round register."));

    newExpr = simd->Generate_Binary_Call(stmt, op1,
                                         Get_Res_Reg(0),
                                         s1_info->Gen_Lod_Reg(prop),
                                         s2_info->Gen_Lod_Reg(prop));
    
    WN *res_stid = Get_Res_Reg()->Simd_Preg_Stid(newExpr);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, res_stid);
    WN_linenum(res_stid) = LWN_Get_Linenum(stmt);
    return res_stid;
}


void
SIMD_EINFO_AT::Generate_Mul_Expr (WN *expr, WN *stmt,
				  SIMD_EINFO *c1_info, SIMD_EINFO *c2_info,
                                  SIMD_INFO *simd)
{
  EINFO_PROP prop = Get_Required_Operand_Prop(true);
  WN *newExpr = simd->Generate_Binary_Call(stmt,
                                           WN_operator(expr),
                                           Get_Res_Reg(0),
                                           c1_info->Gen_Lod_Reg(prop),
                                           c2_info->Gen_Lod_Reg(prop));
  
  WN *res_stid = Get_Res_Reg(0)->Simd_Preg_Stid(newExpr);
  LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, res_stid);
  WN_linenum(res_stid) = LWN_Get_Linenum(stmt);
}


void
SIMD_EINFO_AT::Init_Sum_Reduction (WN *stmt)
{
  SIMD_PREG *res_reg = Get_Res_Reg();
  Is_True(res_reg != NULL, ("Init_Sum_Reduction: null result register"));
  
  TYPE_ID reg_type = res_reg->Type();
  
  bool use_zero = false;
  bool use_xor = false;
  bool use_sub = false;
  bool use_const = false;

  INTRINSIC zero_intrn = Simd_Ti->Zero(reg_type);
  if (zero_intrn == INTRINSIC_INVALID) {
    use_xor = Simd_Ti->Search_Vector_Macro(OPR_BXOR, reg_type) != TIE_INVALID_ID;
    if (!use_xor) {
      use_sub = Simd_Ti->Search_Vector_Macro(OPR_SUB, reg_type) != TIE_INVALID_ID;
      if (!use_sub) {
	use_const = Simd_Ti->Type_Conversion_Possible(Res_Type(), reg_type);
	Is_True(use_const, ("Can't convert from %s to %s",
			    MTYPE_name(Res_Type()), MTYPE_name(reg_type)));
      }
    }
  }
  
  for (INT i = 0; i < 2; i++) {
    SIMD_PREG *res_reg = Get_Res_Reg(i);
    if (!res_reg)
      continue;
    
    if (i > 0 && res_reg == Get_Res_Reg(0))
      continue;
    
    WN *sto = NULL;
    
    if (zero_intrn != INTRINSIC_INVALID) {
      OPCODE zero_op = OPCODE_make_op(OPR_INTRINSIC_OP, reg_type, MTYPE_V);
      WN *zero_wn = LWN_Create_Intrinsic(zero_op, zero_intrn, 0, NULL);
      sto = res_reg->Simd_Preg_Stid(zero_wn);
    } else if (use_xor || use_sub) {
      OPERATOR op = use_xor ? OPR_BXOR : OPR_SUB;
      WN *ldid0 = res_reg->Simd_Preg_Ldid();
      WN *ldid1 = res_reg->Simd_Preg_Ldid();
      WN *zero_op = Simd_Info->Generate_Binary_Call(stmt, op, res_reg, ldid0, ldid1);
      sto = res_reg->Simd_Preg_Stid(zero_op);
      
      // Just to shut off the DU chain warnings
      Du_Mgr->Add_Def_Use(sto, ldid0);
      Du_Mgr->Add_Def_Use(sto, ldid1);
    } else {
      Is_True(use_const, ("Bad sum reduction initialization."));
      
      SIMD_PREG *reg_zero =
	SIMD_CONST_Map_Get(Cur_Simd_Loop->Const_Map(), reg_type, 0, Res_Type(),
			   Cur_Simd_Loop->Simd_Loop(), false, "V_zero", Pool());
      Is_True(reg_zero, ("Null constant register"));
      
      WN *ldid_zero = reg_zero->Simd_Preg_Ldid();
      sto = res_reg->Simd_Preg_Stid(ldid_zero);
    }
    
    Is_True(sto, ("Can't initialize sum reduction."));
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = LWN_Get_Linenum(stmt);
  }
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
