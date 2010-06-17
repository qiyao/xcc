
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

  Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_loop_v2.cxx $

#include "intrn_info.h"
#include "opt_du.h"
#include "simd_at.h"
#include "simd_imem.h"
#include "simd_loop_v2.h"
#include "simd_ti.h"
#include "simd_ti_v2.h"
#include "tie.h"
#include "tietypes.h"
#include "wintrinsic.h"
#include "wutil.h"
#include "simd.h"


SIMD_LOOP_V2::SIMD_LOOP_V2(MEM_POOL *pool, LOOP_MODEL *lm,
			   WN *simd_loop, INT simd_loop_level) :
    SIMD_LOOP_AT(pool,lm,simd_loop,simd_loop_level)
{
    _at_analysis = false;
    _at_transform = true;
    Set_V_Unroll_Factor(-1);
}

void
SIMD_LOOP_V2::Screen_Scalar_To_Vector_Conversion(TYPE_ID scalar_type) 
{
    TARG_SIMD_INFO *tsi = Simd_Ti->Targ_Simd_Info(scalar_type);

    if (tsi == NULL) {
	Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, Simd_Loop(), SIMD_MTYPE_Msg(scalar_type));
	return;
    }

    INT vl = tsi->Vector_Length();
    if (vl > 1 && 
	Simd_Ti->Type_Conversion_Possible(scalar_type, tsi->Simd_Reg_Type())) {
	BIT_VECTOR *vls = Vector_Lengths();
	vls->Set(vl);
    } else {
      Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_SCALAR_CONV, Simd_Loop(), SIMD_MTYPE_Msg(scalar_type));
      if (simd_debug) {
        fprintf(TFile,"No scalar to vector conversion for vl %d, scalar %s\n",
                vl,MTYPE_name(scalar_type));
      }
    }
    
    return;
}


bool
SIMD_LOOP_V2::Screen_Guards (void)
{
  BIT_VECTOR *vls = Vector_Lengths();
  for (INT vl = 0; vl < vls->Size(); vl++) {
    if (vls->Test(vl) && !Check_Guards(vl)) {
      return false;
    }
  }
  
  return true;
}


void 
SIMD_LOOP_V2::Screen_Type(TYPE_ID scalar_type, WN *wn)
{
    TARG_SIMD_INFO *tsi = Simd_Ti->Targ_Simd_Info(scalar_type);

    if (tsi == NULL) {
	Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(scalar_type));
	return;
    }

    INT vl = tsi->Vector_Length();
    if (vl > 1) {
	BIT_VECTOR *vls = Vector_Lengths();
	vls->Set(vl);
    } else {
	Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(scalar_type));
	if (simd_debug) {
	    fprintf(TFile,"No vector length for scalar type %d, scalar %s\n",
		    vl,MTYPE_name(scalar_type));
	}
	return;
    }
}

void
SIMD_LOOP_V2::Screen_Operator(WN *wn, SIMD_EINFO *e_info) {
  OPERATOR oper = WN_operator(wn);

  if (oper == OPR_PARM || oper == OPR_OUTPART) {
    return;
  }

  TYPE_ID res_type = e_info->Res_Type();
  TARG_SIMD_INFO *tsi = Simd_Ti->Targ_Simd_Info(res_type);
  
  if (tsi == NULL) {
      Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(res_type));
      return;
  }

  int vl = tsi->Vector_Length();
  if (vl <= 1) {
      Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(res_type));
      if (simd_debug) {
	  fprintf(TFile,"No vector type for (vl %d, res_type %s)\n",
		  vl,MTYPE_name(res_type));
      }
      return;
  }

  // non-converting load and store should be available for the vector type
  if (OPERATOR_is_load(oper) || OPERATOR_is_store(oper)) {
      return;
  }
      
  TYPE_ID rtype = Simd_Ti->Simd_Reg_Type_Of_Scalar(res_type);
  Is_True(rtype!=MTYPE_UNKNOWN,
	  ("Expected to find SIMD type for %s.",MTYPE_name(res_type)));
    
  if (OPERATOR_is_compare(oper)) {
      res_type = MTYPE_XTBOOL;
      rtype = Simd_Ti->Get_SIMD_Xtbool(vl);
      if (rtype == MTYPE_UNKNOWN) {
	  Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, wn, SIMD_MTYPE_Msg(res_type));
	  if (simd_debug) {
	      fprintf(TFile,"No vector type for (vl %d, res_type %s)\n",
		      vl,MTYPE_name(res_type));
	  }
	  return;
      }
      
      /* necessary canonicalization */
      switch (oper) {
	  case OPR_NE: oper = OPR_EQ;
	      break;
	  case OPR_GT: oper = OPR_LT;
	      break;
	  case OPR_GE: oper = OPR_LE;
	      break;
      }
  }

  TYPE_ID rtype_scalar = res_type;
  
  for (INT kid_idx=0; kid_idx<WN_kid_count(wn); kid_idx++) {
      WN *kid = WN_kid(wn,kid_idx);
      TYPE_ID from_desc_scalar = AT_WN_Decode_SIMD_EINFO(kid).rtype;
      TYPE_ID to_desc_scalar = AT_FACTORY::canonical_kid_type(wn, kid_idx,
							      &AT_WN_Decode_SIMD_EINFO);
      if (from_desc_scalar==to_desc_scalar) {
	  continue;
      }
      TYPE_ID from_desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(from_desc_scalar);
      TYPE_ID to_desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(to_desc_scalar);
      
      if (from_desc==MTYPE_UNKNOWN || to_desc==MTYPE_UNKNOWN) {
	  Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, Simd_Loop(), 
			   SIMD_MTYPE_Msg(from_desc == MTYPE_UNKNOWN?
                                          from_desc_scalar:
                                          to_desc_scalar));
	  if (simd_debug) {
	      fprintf(TFile,"No SIMD type for vl %d, scalar %s\n",
		      vl,(from_desc==MTYPE_UNKNOWN?
			  MTYPE_name(from_desc_scalar):
			  MTYPE_name(to_desc_scalar)));
	  }
	  break;
      } 

      if (!Simd_Ti_V2->Can_Cvt(to_desc, from_desc)) {
        Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_TYPE_CONV, Simd_Loop(),
                         SIMD_MTYPE_Msg(from_desc), SIMD_MTYPE_Msg(to_desc));
	  if (simd_debug) {
	      fprintf(TFile,"No conversion available from %s to %s\n",
		      OPERATOR_name(oper),MTYPE_name(from_desc),MTYPE_name(to_desc));
	  }
	  break;
      }
  }

  if (Bad_Operator()) {
      return;
  }
    
  if (oper==OPR_INTRINSIC_OP) {
      INTRINSIC intrn = Tie_Vector_Intrinsic(wn);
      if (intrn==INTRINSIC_INVALID) {
	  Set_Bad_Oper_Msg(wn);
	  if (simd_debug) {
	      fprintf(TFile,"No vector intrinsic available for %s, vl %d\n",
		      INTRINSIC_name(WN_intrinsic(wn)),vl);
	  }
      }
  } else {
      // FIXME: the code below assumes that all vector kids should be the same
      // type for each operator. this needs to be fixed at some point and actually
      // do operator search with different type kids
      TYPE_ID desc_scalar = AT_FACTORY::canonical_kid_type(wn, 0,
							   &AT_WN_Decode_SIMD_EINFO);
      TYPE_ID desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(desc_scalar);
      if (desc==MTYPE_UNKNOWN) {
	  Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_VECTOR_TYPE, Simd_Loop(), SIMD_MTYPE_Msg(desc_scalar));
	  if (simd_debug) {
	      fprintf(TFile,"No SIMD type for vl %d, scalar %s\n",
		      vl,MTYPE_name(desc_scalar));
	  }
	  return;
      }
      
      if (oper==OPR_CVT || oper==OPR_CVTL) {
	if (desc!=rtype && !Simd_Ti_V2->Can_Cvt(rtype, desc)) {
	  Set_Bad_Oper_Msg(AT_MSG_SIMD_NO_TYPE_CONV, Simd_Loop(),
                           SIMD_MTYPE_Msg(desc), SIMD_MTYPE_Msg(rtype));
	  if (simd_debug) {
	      fprintf(TFile,"No conversion available from %s to %s\n",
		      OPERATOR_name(oper),MTYPE_name(desc),MTYPE_name(rtype));
	  }
	}
	return;
      }

      OPERATOR use_oper = (oper == OPR_DIV) ? OPR_ASHR : oper;
      TIE_MACRO_ID macro_id = Simd_Ti->Search_Vector_Macro(use_oper,desc,rtype);
      if (macro_id==TIE_INVALID_ID) {
	  if (oper == OPR_MPY && Simd_Ti->Has_Even_Odd_Mac(oper, rtype, desc)) {
	      ; // donothing;
	  } else {
	      Set_Bad_Oper_Msg(wn);
	      if (simd_debug) {
		  fprintf(TFile,"No intrinsic available for oper %s, rtype %s, desc %s\n",
			  OPERATOR_name(oper),MTYPE_name(rtype),MTYPE_name(desc));
	      }
	  }
      }
  }
}

void
SIMD_LOOP_V2::Screen_Mulr(WN *wn)
{
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_ADD || oper == OPR_SUB,
	    ("SIMD_LOOP_V2::Screen_Mulr: expecting ADD/SUB"));

    WN* child1 = WN_kid0(wn);
    WN* child2 = WN_kid1(wn);

    /* combine ADD(roundoff, MPY(x, y)) to MULR by setting **
    ** the child1 to roundoff register                     */
    if (oper == OPR_ADD && WN_operator(child2) == OPR_MPY) {
	SIMD_EINFO *child1_info = E_Info().Find(child1);
	if (Tree_Equiv(child1, Round_Expr())) {
	    child1_info->Set_Is_Round_Reg();
	} else if (Round_Expr() == NULL && Simd_Loop()) {
	    if (Invariant_In_Simd_Loop(child1)) {
		Set_Round_Expr(child1);
		child1_info->Set_Is_Round_Reg();
	    }
	}
    }
}

#define IS_POWER_OF_2(x) (((x) & ((x)-1))==0)

SIMD_EINFO*
SIMD_LOOP_V2::Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd)
{
  SIMD_EINFO_V2  *e_info  = dynamic_cast<SIMD_EINFO_V2*>(Get_E_Info(expr));
  WN             *source  = WN_kid0(expr);
  SIMD_EINFO     *c_info  = Get_E_Info(source);
  Is_True(e_info && c_info, ("No SIMD_EINFO"));

  TYPE_ID simd_type   = simd->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
  TYPE_ID kid_type    = simd->Get_SIMD_Reg_Type_Scalar(c_info->Res_Type());
  
  if (simd_type == kid_type) {
      c_info = Simd_Transform(source, stmt, simd);
      e_info->Copy_Simd_Reg_Info(c_info);
  } else {
    /* generate SIMD registers */
      Generate_Simd_Reg_Info(e_info, simd);

      EINFO_PROP prop = e_info->Get_Required_Operand_Prop();

      if (Simd_Ti_V2->Type_Conversion_Possible(kid_type, simd_type)) {
	  c_info = Simd_Transform(source, stmt, simd);
	  for (INT i = 0; i < e_info->Reg_Count(); i++) {
	      OPCODE  opc_cvt = OPCODE_make_op(OPR_CVT, simd_type, kid_type);
	      WN     *cvt     = LWN_CreateExp1(opc_cvt, 
					       c_info->Gen_Lod_Reg(prop,i));
	      WN     *stid    = e_info->Get_Res_Reg(i)->Simd_Preg_Stid(cvt);
	      
	      LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
	      WN_linenum(stid) = LWN_Get_Linenum(stmt);
	  }
      } else if (Simd_Ti_V2->Has_Normal_Cvt(simd_type, kid_type)) {
	  // nothing yet
	  FmtAssert(0, ("Normal Cvt not implemented"));
      } else if (Simd_Ti_V2->Has_Pack_Cvt(simd_type, kid_type)) {
	  WN         *shift_amount = NULL;

	  /* combine with shift right */
	  if (WN_operator(source) == OPR_ASHR ||
	      WN_operator(source) == OPR_LSHR ||
	      WN_operator(source) == OPR_DIV) {
	      
	      Is_True(c_info->Get_Res_Reg() == NULL, ("Shared kid"));
	      shift_amount = WN_kid1(source);

	      if (WN_operator(source) == OPR_DIV) {
		  Is_True(WN_operator(shift_amount) == OPR_INTCONST, 
			  ("DIV by non-const"));
		  INT64 val = WN_const_val(shift_amount);
		  Is_True(val > 0 && 
			  IS_POWER_OF_2(val), ("DIV is not a shift right"));
		  INT shift_a = 0;
		  while ( (val & 0x1) != 0x1) {
		      val >>= 1;
		      shift_a++;
		  }
		  shift_amount = WN_CreateIntconst(OPC_I4INTCONST, shift_a);
	      } else {
		  /* copy the shift amount */
		  WN *copy = LWN_Copy_Tree(shift_amount, TRUE, LNO_Info_Map);
		  LWN_Copy_Def_Use(shift_amount, copy, Du_Mgr);
		  WN_kid1(source) = copy;
		  LWN_Set_Parent(shift_amount, NULL);
	      }

	      /* skip the shift */
	      source = WN_kid0(source);

	      /* copy the need_interleave, need_deinterleave to the 
		 kid of the shift */
	      SIMD_EINFO *c1_info = Get_E_Info(source);
	      if (c1_info) {
		  if (c_info->Need_Interleave()) {
		      c1_info->Set_Need_Interleave();
		  }
		  if (c_info->Need_Deinterleave()) {
		      c1_info->Set_Need_Deinterleave();
		  }
	      }
	  } /* else shift_amount == NULL */
	  

	  /* transforming the source */
	  c_info = Simd_Transform(source, stmt, simd);

	  /* generate packing */
	  if (shift_amount != NULL) {
	      Generate_VSAR_Amount(shift_amount, stmt, simd);
	  } else {
	      OPCODE opc   = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
	      shift_amount = WN_CreateIntconst(opc, 0);
	      Generate_VSAR_Amount(shift_amount, stmt, simd);
	  }
	  
	  e_info->Generate_Conv_Pack(stmt, c_info);

      } else { /* generate double size MUL conversion */
	  c_info = Simd_Transform(source, stmt, simd);
	  FmtAssert(Simd_Ti_V2->Has_Mul_Cvt(simd_type, kid_type),
		    ("SIMD_LOOP_V2::Simd_Transform_CVT: No conversion exist"));
	  Is_True(2 * MTYPE_bit_size(c_info->Res_Type()) <=
		  MTYPE_bit_size(e_info->Res_Type()), 
		  ("Simd_Transform_CVT: expect size expansion"));
	  
	  e_info->Generate_Conv_Mul(stmt,c_info);
      }
  }

  /* interleave/deinterleave if required */
  e_info->Simd_Post_Processing(stmt, simd, this);

  return e_info;
}

void
SIMD_LOOP_V2::Init_Vector_Lengths(BIT_VECTOR *vls)
{
    // do nothing
}

void
SIMD_LOOP_V2::Set_Default_Vl()
{
    // do nothing
}


INT
SIMD_LOOP_V2::Find_Unroll_Factor (void)
{
  /* Choose the largest vectorization factor if it doesn't exceed
     the estimated iteration count for the loop. */
  INT64 est_iter = Max_Iteration_Count();
  INT max_vl = Vector_Lengths()->Size() - 1;
  for (INT vl = max_vl; vl > 0; vl--) {
    if (Vector_Lengths()->Test(vl)) {
      if (est_iter < 0 || vl <= est_iter)
	return vl;
      
      return 0;
    }
  }
  
  return 0;
}


SIMD_EINFO*
SIMD_LOOP_V2::Simd_Preprocess_Transform(WN* wn)
{
    return SIMD_LOOP::Simd_Preprocess_Transform(wn);
}

SIMD_EINFO* 
SIMD_LOOP_V2::Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO_V2(wn,cur_size,p), p);
    return res;
}

SIMD_EINFO* 
SIMD_LOOP_V2::Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO_V2(wn,e_info), e_info->Pool());
    return res;
}


void
SIMD_EINFO_V2::Generate_Simd_Expr(bool unary, WN *expr, WN *stmt, 
				  SIMD_EINFO *c1_info, SIMD_INFO *simd,
				  SIMD_EINFO *c2_info, INT idx)
{
    return SIMD_EINFO::Generate_Simd_Expr(unary, expr, stmt, c1_info, simd,
					  c2_info, idx);
}

/*---------------------------------------------------------------------------*
 * Generate MULA/MULS using intrinsic calls                                  *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO_V2::Generate_Mula_Expr(WN *expr, WN *stmt,
				  SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
				  SIMD_EINFO *s2_info, WN *mul,
				  SIMD_INFO  *simd, INT idx)
{
    Is_True(WN_operator(mul) == OPR_MPY, ("Not a multiply"));
    Is_True(s1_info && s2_info, ("children info missing"));
    Is_True(idx == 0, ("SIMD_EINFO_V2: Illegal MUL part specification"));

    /* generate MULR/MULA/MULS */
    WN       *newExpr = NULL;
    OPERATOR  op      = WN_operator(expr);
    OPERATOR  op1     = (op == OPR_ADD) ? OPR_MADD : OPR_MSUB;
    Is_True(op == OPR_ADD || op == OPR_SUB, ("Unexpected operator"));
    EINFO_PROP prop   = Get_Required_Operand_Prop(true);

    if (c1_info->Is_Round_Reg()) {
	if (Simd_Ti->Has_Paired_Mulr(Get_Res_Reg(0)->Type(), 
				     s1_info->Pick_Simd_Reg(prop)->Type())) {
	    newExpr = Generate_Mulr_Call(Get_Res_Reg(0)->Simd_Preg_Ldid(),
					 Get_Res_Reg(1)->Simd_Preg_Ldid(),
					 s1_info->Gen_Lod_Reg(prop),
					 s2_info->Gen_Lod_Reg(prop));
	    Simd_Ti->Store_Packed_Call_Output(Get_Res_Reg(0), Get_Res_Reg(1), 
					      newExpr, LWN_Get_Parent(stmt), 
					      stmt, LWN_Get_Linenum(stmt));
	} else {
	    newExpr = Generate_Mulr_Call(Get_Res_Reg(0)->Simd_Preg_Ldid(),
					 s1_info->Gen_Lod_Reg(prop),
					 s2_info->Gen_Lod_Reg(prop), true);
	    Simd_Ti->Store_Call_Output(Get_Res_Reg(0),
				       newExpr, LWN_Get_Parent(stmt), 
				       stmt, LWN_Get_Linenum(stmt));
	    newExpr = Generate_Mulr_Call(Get_Res_Reg(1)->Simd_Preg_Ldid(),
					 s1_info->Gen_Lod_Reg(prop),
					 s2_info->Gen_Lod_Reg(prop), false);
	    Simd_Ti->Store_Call_Output(Get_Res_Reg(1), 
				       newExpr, LWN_Get_Parent(stmt), 
				       stmt, LWN_Get_Linenum(stmt));
	}

    } else {
	if (Simd_Ti->Has_Paired_Mac(op1,
				    Get_Res_Reg(0)->Type(), 
				    s1_info->Pick_Simd_Reg(prop)->Type())) {
	    newExpr = Generate_Mac_Call(op1, 
					Get_Res_Reg(0)->Simd_Preg_Ldid(),
					Get_Res_Reg(1)->Simd_Preg_Ldid(),
					s1_info->Gen_Lod_Reg(prop),
					s2_info->Gen_Lod_Reg(prop));
	    Simd_Ti->Store_Packed_Op_Output(Get_Res_Reg(0), Get_Res_Reg(1), 
					    newExpr, LWN_Get_Parent(stmt), 
					    stmt, LWN_Get_Linenum(stmt));
	} else {
	    newExpr = Generate_Mac_Call(op1, 
					Get_Res_Reg(0)->Simd_Preg_Ldid(),
					s1_info->Gen_Lod_Reg(prop),
					s2_info->Gen_Lod_Reg(prop), true);
	    Simd_Ti->Store_Op_Output(Get_Res_Reg(0), 
				     newExpr, LWN_Get_Parent(stmt), 
				     stmt, LWN_Get_Linenum(stmt));

	    newExpr = Generate_Mac_Call(op1, 
					Get_Res_Reg(1)->Simd_Preg_Ldid(),
					s1_info->Gen_Lod_Reg(prop),
					s2_info->Gen_Lod_Reg(prop), false);
	    Simd_Ti->Store_Op_Output(Get_Res_Reg(1), 
				     newExpr, LWN_Get_Parent(stmt), 
				     stmt, LWN_Get_Linenum(stmt));
	}
    }
    return newExpr;
}

void
SIMD_EINFO_V2::Generate_Mul_Expr(WN *expr, WN *stmt,
				 SIMD_EINFO *c1_info, SIMD_EINFO *c2_info,
				 SIMD_INFO *simd)
{
    /* Vectra2 paired multiply */
    EINFO_PROP prop = Get_Required_Operand_Prop(true);
    if (Simd_Ti->Has_Paired_Mac(OPR_MPY,
				Get_Res_Reg(0)->Type(), 
				c1_info->Pick_Simd_Reg(prop)->Type())) {
	WN *newExpr = 
	    Generate_Mul_Call(WN_operator(expr),
			      Get_Res_Reg(0)->Type(),
			      c1_info->Gen_Lod_Reg(prop),
			      c2_info->Gen_Lod_Reg(prop));
    
	Simd_Ti->Store_Packed_Op_Output(Get_Res_Reg(0), 
					Get_Res_Reg(1), newExpr,
					LWN_Get_Parent(stmt), stmt, 
					LWN_Get_Linenum(stmt));
    } else {
	WN *newExpr = 
	    Generate_Mul_Call(WN_operator(expr),
			      Get_Res_Reg(0)->Type(),
			      c1_info->Gen_Lod_Reg(prop),
			      c2_info->Gen_Lod_Reg(prop), true);
	Simd_Ti->Store_Op_Output(Get_Res_Reg(0), newExpr, 
				 LWN_Get_Parent(stmt), stmt, 
				 LWN_Get_Linenum(stmt));

	newExpr = 
	    Generate_Mul_Call(WN_operator(expr),
			      Get_Res_Reg(1)->Type(),
			      c1_info->Gen_Lod_Reg(prop),
			      c2_info->Gen_Lod_Reg(prop), false);
	Simd_Ti->Store_Op_Output(Get_Res_Reg(1), newExpr,
				 LWN_Get_Parent(stmt), stmt, 
				 LWN_Get_Linenum(stmt));
    }
}

void
SIMD_EINFO_V2::Generate_Conv_Mul(WN *stmt, SIMD_EINFO *c_info)
{
    /* Vectra2 paired multiply */
    EINFO_PROP prop = Get_Required_Operand_Prop();
    Is_True(2 * Simd_Ti_V2->Get_SIMD_Width_Scalar(Res_Type()) == 
	    Simd_Ti_V2->Get_SIMD_Width_Scalar(c_info->Res_Type()),
	    ("SIMD_EINFO_V2::Generate_Conv_Pack: vector length mismatch"));
    
    for (INT i = 0; i < Reg_Count(); i += 2) {
	if (Simd_Ti->Has_Paired_Mac(OPR_MPY,
				    Get_Res_Reg(0)->Type(), 
				    c_info->Pick_Simd_Reg(prop)->Type())) {
	    WN *new_expr = Generate_Mul_Call(
		OPR_MPY,
		Get_Res_Reg(i)->Type(),
		Get_Narrow_One(Simd_Ti_V2, 
			       Cur_Simd_Loop,
			       c_info->Res_Type()),
		c_info->Gen_Lod_Reg(prop, i/2));
	    
	    Simd_Ti->Store_Packed_Op_Output(
		Get_Res_Reg(i), Get_Res_Reg(i+1), new_expr,
		LWN_Get_Parent(stmt), stmt, LWN_Get_Linenum(stmt));
	} else {
	    WN *new_expr = Generate_Mul_Call(
		OPR_MPY, Get_Res_Reg(i)->Type(),
		Get_Narrow_One(Simd_Ti_V2, Cur_Simd_Loop, c_info->Res_Type()),
		c_info->Gen_Lod_Reg(prop, i/2), true);
	    
	    Simd_Ti->Store_Op_Output(
		Get_Res_Reg(i), new_expr,
		LWN_Get_Parent(stmt), stmt, LWN_Get_Linenum(stmt));

	    new_expr = Generate_Mul_Call(
		OPR_MPY, Get_Res_Reg(i+1)->Type(),
		Get_Narrow_One(Simd_Ti_V2, Cur_Simd_Loop, c_info->Res_Type()),
		c_info->Gen_Lod_Reg(prop, i/2), false);
	    
	    Simd_Ti->Store_Op_Output(
		Get_Res_Reg(i+1), new_expr,
		LWN_Get_Parent(stmt), stmt, LWN_Get_Linenum(stmt));
	}
    }
}

void
SIMD_EINFO_V2::Generate_Conv_Pack(WN *stmt, SIMD_EINFO *c_info)
{
    EINFO_PROP prop  = Get_Required_Operand_Prop();
    TYPE_ID in_type  = c_info->Get_Res_Reg()->Type();
    TYPE_ID out_type = Get_Res_Reg()->Type();

    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
    INTRINSIC intrin_id = Simd_Ti_V2->Tie_Pack_Intrinsic(out_type, in_type);
    Is_True(intrin_id != INTRINSIC_INVALID, ("Cannot find PACK intrinsic"));
    
    Is_True(2*Simd_Ti_V2->Get_SIMD_Width_SIMD(in_type) == 
	    Simd_Ti_V2->Get_SIMD_Width_SIMD(out_type),
	    ("SIMD_EINFO_V2::Generate_Conv_Pack: vector length mismatch"));

    WN* apr[3];
    for (INT i = 0; i < Reg_Count(); i++) {
	apr[0] = LWN_CreateParm(out_type, Get_Res_Reg(i)->Simd_Preg_Ldid(), 
				MTYPE_To_TY(out_type), WN_PARM_BY_VALUE);
	apr[1] = LWN_CreateParm(in_type, c_info->Gen_Lod_Reg(prop, 2*i+1),
				MTYPE_To_TY(in_type), WN_PARM_BY_VALUE);
	apr[2] = LWN_CreateParm(in_type, c_info->Gen_Lod_Reg(prop, 2*i),
				MTYPE_To_TY(in_type), WN_PARM_BY_VALUE);
	WN  *pack = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);

	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(pack);
	}
	
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, pack);
	WN_linenum(pack) = LWN_Get_Linenum(stmt);
	WN *parm_ldid = WN_Ldid(Get_Res_Reg(i)->Type(), -1, 
				Tie_Output_Volatile_Preg,
				Tie_Output_Volatile_Type);
	Du_Mgr->Add_Def_Use(pack, parm_ldid);
	
	WN *sto = Get_Res_Reg(i)->Simd_Preg_Stid(parm_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
	WN_linenum(sto) = LWN_Get_Linenum(stmt);
    }	
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD intrinsic call to MULR                                   *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO_V2::Generate_Mulr_Call(WN *operand0, WN* operand1, WN *operand2, 
				  WN *operand3)
{
    TYPE_ID out_type = WN_rtype(operand0);
    TYPE_ID in_type  = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Ti->Tie_Mulr_Intrinsic(out_type, in_type);
    
    Is_True(intrin_id != INTRINSIC_INVALID, ("Vectra2: Cannot find MULR"));
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    WN *apr[4];
    apr[0] = LWN_CreateParm(out_type, operand0, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(out_type, operand1, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[3] = LWN_CreateParm(in_type, operand3,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 4, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD intrinsic call to MULR                                   *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO_V2::Generate_Mulr_Call(WN *operand0, WN *operand1, WN *operand2,
				  bool even)
{
    TYPE_ID out_type = WN_rtype(operand0);
    TYPE_ID in_type  = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Ti->Tie_Mulr_Intrinsic(out_type, in_type, even);
    
    Is_True(intrin_id != INTRINSIC_INVALID, ("Vectra2: Cannot find MULR"));
    OPCODE    intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);

    WN *apr[3];
    apr[0] = LWN_CreateParm(out_type, operand0, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(out_type, operand1, MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call to MULA/MULS                             *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO_V2::Generate_Mac_Call(OPERATOR op, WN *operand0, WN *operand1, 
				 WN *operand2, WN *operand3)
{
    TYPE_ID   out_type  = WN_rtype(operand0);
    TYPE_ID   in_type   = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Info->Find_Function(op, in_type, out_type);

    TYPE_ID    output_type = Get_Packed_Output_Type(intrin_id);
    Is_True(tie_info->num_scalar_mtypes(output_type) == 2, 
	    ("MAC's output type is not a compound of two types"));
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, output_type, MTYPE_V);
    
    WN *apr[4];
    apr[0] = LWN_CreateParm(out_type, operand0, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(out_type, operand1, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[3] = LWN_CreateParm(in_type, operand3,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 4, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Generate an SIMD instrinsic call to MULA/MULS                             *
 *---------------------------------------------------------------------------*/
WN*
SIMD_EINFO_V2::Generate_Mac_Call(OPERATOR op, WN *operand0, WN *operand1, 
				 WN *operand2, bool even)
{
    TYPE_ID   out_type  = WN_rtype(operand0);
    TYPE_ID   in_type   = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Ti->Tie_Mac_Intrinsic(op, out_type, in_type, even);
    
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, out_type, MTYPE_V);
    
    WN *apr[3];
    apr[0] = LWN_CreateParm(out_type, operand0, MTYPE_To_TY(out_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(out_type, operand1, MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    return callNode;
}

WN*
SIMD_EINFO_V2::Generate_Mul_Call(OPERATOR op, TYPE_ID out_type, 
				 WN *operand2, WN *operand3)
{
    TYPE_ID   in_type   = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Info->Find_Function(op, in_type, out_type);

    TYPE_ID    output_type = Get_Packed_Output_Type(intrin_id);
    Is_True(tie_info->num_scalar_mtypes(output_type) == 2, 
	    ("MUL's output type is not a compound of two types"));
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, output_type, MTYPE_V);
    
    WN *apr[2];
    apr[0] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(in_type, operand3,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
    return callNode;
}

WN*
SIMD_EINFO_V2::Generate_Mul_Call(OPERATOR op, TYPE_ID out_type, 
				 WN *operand2, WN *operand3, bool even)
{
    TYPE_ID   in_type   = WN_rtype(operand2);
    INTRINSIC intrin_id = Simd_Ti->Tie_Mac_Intrinsic(op, out_type, in_type, even);

    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, out_type, MTYPE_V);
    
    WN *apr[2];
    apr[0] = LWN_CreateParm(in_type, operand2,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(in_type, operand3,  MTYPE_To_TY(in_type), 
			    WN_PARM_BY_VALUE);
    WN *callNode = LWN_Create_Intrinsic(intrin_op, intrin_id, 2, apr);
    return callNode;
}

/*---------------------------------------------------------------------------*
 * Interleave the register of size 1                                         *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO_V2::Simd_Interleave_One(WN* stmt, SIMD_INFO *simd, 
					SIMD_LOOP *doinfo)
{
    Is_True(Need_Interleave(), ("Unexpected interleaving property"));
    Is_True(Simd_IRegs()->Elements() == 1, ("Unexpected IReg length"));
    
    /* Generate select interleave */
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(Simd_IReg(0)->Type());
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    INT64 sel_imm = simd_sel->Sel_75316420();
    dynamic_cast<SIMD_LOOP_V2*>(doinfo)->
	Generate_Sel_Imm(stmt, Simd_IReg(0), Simd_Reg(0), Simd_Reg(0), 
			 sel_imm);
}

/*---------------------------------------------------------------------------*
 * Interleave the registers                                                  *
 *---------------------------------------------------------------------------*/
void SIMD_EINFO_V2::Simd_Interleave(WN* stmt, SIMD_INFO *simd, 
				    SIMD_LOOP *doinfo)
{
    Is_True(Need_Interleave(), ("Unexpected interleaving property"));
    
    if (Simd_IRegs()->Elements() == 1) {
      Simd_Interleave_One(stmt, simd, doinfo);
      return;
    }
    
    Is_True(Simd_IRegs()->Elements() == 2, ("Unexpected IReg length"));

    /* Generate dselect interleave */
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(Simd_IReg(0)->Type());
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    INT64 sel_imm = simd_sel->Dsel_75316420();
    dynamic_cast<SIMD_LOOP_V2*>(doinfo)->
	Generate_Dsel_Imm(stmt, Simd_IReg(1), Simd_IReg(0), 
			  Simd_Reg(1), Simd_Reg(0), sel_imm);
    
}

void
SIMD_EINFO_V2::Simd_Deinterleave_One(WN *stmt, 
				     SIMD_INFO *simd, SIMD_LOOP *doinfo,
				     WN *block)
{
    Is_True(Simd_Reg(0) && !Simd_Reg(1), ("Null Simd_Reg"));
    
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(Simd_Reg(0)->Type());
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    
    /* Generate select deinterleave */
    INT64 sel_imm = simd_sel->Sel_73625140();
    dynamic_cast<SIMD_LOOP_V2*>(doinfo)->
	Generate_Sel_Imm(stmt, Simd_Reg(0), Simd_IReg(0), Simd_IReg(0), 
			 sel_imm, block);
}

/*---------------------------------------------------------------------------*
 * Deinterleave the Simd_IReg(1), Simd_IReg(0) into Simd_Reg(1..0)           *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO_V2::Simd_Deinterleave(WN *stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo,
				 WN *block)
{
    if (Simd_IReg(1) == NULL) {
      /* only one register */
      Simd_Deinterleave_One(stmt, simd, doinfo, block);
      return;
    }

    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(Simd_Reg(0)->Type());
    Is_True(simd_sel != NULL, ("NULL SIMD select."));
    
    INT64 sel_imm = simd_sel->Dsel_73625140();
    dynamic_cast<SIMD_LOOP_V2*>(doinfo)->
	Generate_Dsel_Imm(stmt, Simd_Reg(1), Simd_Reg(0), 
			  Simd_IReg(1), Simd_IReg(0), sel_imm, block);
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:




