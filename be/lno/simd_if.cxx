// simd_if.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Automatic SIMD                                                         *
 *                                                                           *
 *    Functions for SIMD if-conversion (MOVT/MOVF)                           *
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

// $Id: simd_if.cxx $

#include "simd_if.h"
#include "simd_imem.h"
#include "opt_du.h"
#include "simd_ti.h"
#include "simd_at.h"
#include "simd_model.h"

extern DU_MANAGER *Du_Mgr;          	// PU DU manager
extern MEM_POOL    SIMD_local_pool;

static
INT Count_Cycle(EINFO_Stack &stack)
{
    INT res = 0;
    for (INT i = 0; i < stack.Elements(); i++) {
	SIMD_EINFO *cur = stack.Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (!OPERATOR_is_leaf(oper) || oper == OPR_STID) {
	    res++;
	}
    }
}

static
float Count_Orig_Cycle(DYN_ARRAY<SIMD_IF_ANA*> &a)
{
    float res = 0;
    for (INT i = 0; i < a.Elements(); i++) {
	SIMD_IF_ANA *cur = a[i];
	res += cur->Original_Cycle();
    }
    return res;
}

#define BRANCH_PENALTY  3.3
/* 2.3 for 5 stage */
#define T_FREQ          0.7
#define F_FREQ          0.3

float
SIMD_IF_ANA::Original_Cycle()
{
    if (_orig_cycle != 0.0) {
	return _orig_cycle;
    }

    IF_INFO* ii = Get_If_Info(If_Wn(), TRUE);
    Is_True(ii, ("SIMD_IF_ANA::Missing IF info annotation"));

    INT test_cycle = Count_Cycle(Test_EInfo());
    INT then_cycle = Count_Cycle(Then_EInfo());
    INT else_cycle = Count_Cycle(Else_EInfo());

    float inner_then = Count_Orig_Cycle(Inner_If_Then());
    float inner_else = Count_Orig_Cycle(Inner_If_Else());

    INT common_cycle = 0;
    MEM_POOL_Push(&SIMD_local_pool);
    {
	IMEM_VIS_Map seen_load(101, &SIMD_local_pool);
	IMEM_VIS_Map seen_store(101, &SIMD_local_pool);
	common_cycle = Common_Load_Store_Cycle(seen_load, seen_store);
    }
    MEM_POOL_Pop(&SIMD_local_pool);

    float then_freq = T_FREQ;
    float else_freq = F_FREQ;
       
    if (ii->Freq_True >= 0 && ii->Freq_False >= 0) {
	then_freq = ii->Freq_True;
	else_freq = ii->Freq_False;
    }
    
    _orig_cycle = test_cycle + then_freq * (then_cycle + inner_then) 
	+ else_freq * (else_cycle + inner_else) - 0.5*common_cycle;

    if (then_freq >= else_freq) { /* assuming branch can be rearranged */
	_orig_cycle += BRANCH_PENALTY*else_freq;
    } else {
	_orig_cycle += BRANCH_PENALTY*then_freq;
    }
    Is_True(_orig_cycle >= 0, 
	    ("SIMD_IF_ANA::Original_Cycle: cycle < 0"));
    return _orig_cycle;
}

INT
SIMD_IF_ANA::Common_Load_Store_Cycle(EINFO_Stack &stack, 
				     IMEM_VIS_Map &seen_load,
				     IMEM_VIS_Map &seen_store)
{
    INT res = 0;
    for (INT i = 0; i < stack.Elements(); i++) {
	SIMD_EINFO *cur = stack.Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	    IMEM_INFO *imem = Cur_Simd_Loop->IMem_Map().Find(cur->Expr());
	    Is_True(imem, ("Common_Load_Store_Cycle: no IMEM_INFO"));
	    if (oper == OPR_ILOAD) {
		if (seen_load.Find(imem)) {
		    res++;
		} else {
		    seen_load.Enter(imem, 1);
		}
	    } else {
		if (seen_store.Find(imem)) {
		    res++;
		} else {
		    seen_store.Enter(imem, 1);
		}
	    }
	}
    }
    return res;
}

INT
SIMD_IF_ANA::Common_Load_Store_Cycle(IMEM_VIS_Map &seen_imem_load, 
				     IMEM_VIS_Map &seen_imem_store)
{
    INT res = 
	Common_Load_Store_Cycle(Test_EInfo(), seen_imem_load, seen_imem_store)
	+
	Common_Load_Store_Cycle(Then_EInfo(), seen_imem_load, seen_imem_store)
	+ 
	Common_Load_Store_Cycle(Else_EInfo(), seen_imem_load, seen_imem_store);
    for (INT i = 0; i < Inner_If_Then().Elements(); i++) {
	SIMD_IF_ANA *cur = Inner_If_Then()[i];
	res += cur->Common_Load_Store_Cycle(seen_imem_load, seen_imem_store);
    }
    for (INT i = 0; i < Inner_If_Else().Elements(); i++) {
	SIMD_IF_ANA *cur = Inner_If_Else()[i];
	res += cur->Common_Load_Store_Cycle(seen_imem_load, seen_imem_store);
    }
    return res;
}

INT
SIMD_IF_ANA::CMove_Cycle(EINFO_Stack &stack, SINFO_VIS_Map &seen_sinfo,
			 IMEM_VIS_Map &seen_imem)
{
    INT res = 0;
    for (INT i = 0; i < stack.Elements(); i++) {
	SIMD_EINFO *cur = stack.Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (oper == OPR_ISTORE) {
	    IMEM_INFO *imem = Cur_Simd_Loop->IMem_Map().Find(cur->Expr());
	    Is_True(imem, ("CMove_Cycle: no IMEM_INFO"));
	    if (!seen_imem.Find(imem)) {
		res++;
		Sel_EInfo().Push(cur);
		seen_imem.Enter(imem, 1);
	    }
	} else if (oper == OPR_STID) {
	    SIMD_SCALAR *sinfo = Cur_Simd_Loop->S_Info_Map().Find(cur->Expr());
	    Is_True(sinfo, ("CMove_Cycle: no SIMD_SCALAR"));
	    if (!seen_sinfo.Find(sinfo)) {
		res++;
		Sel_EInfo().Push(cur);
		seen_sinfo.Enter(sinfo, 1);
	    }
	}
    }
    return res;
}

INT
SIMD_IF_ANA::CMove_Cycle(SINFO_VIS_Map &seen_sinfo, IMEM_VIS_Map &seen_imem)
{
    INT res = CMove_Cycle(Then_EInfo(), seen_sinfo, seen_imem)
	+ CMove_Cycle(Else_EInfo(), seen_sinfo, seen_imem);
    for (INT i = 0; i < Inner_If_Then().Elements(); i++) {
	SIMD_IF_ANA *cur = Inner_If_Then()[i];
	res += cur->CMove_Cycle(seen_sinfo, seen_imem);
    }
    for (INT i = 0; i < Inner_If_Else().Elements(); i++) {
	SIMD_IF_ANA *cur = Inner_If_Else()[i];
	res += cur->CMove_Cycle(seen_sinfo, seen_imem);
    }
    return res;
}


INT 
SIMD_IF_ANA::Count_If_Cycle(DYN_ARRAY<SIMD_IF_ANA*> &a)
{
    INT res = 0;
    for (INT i = 0; i < a.Elements(); i++) {
	SIMD_IF_ANA *cur = a[i];
	res += Count_Cycle(cur->Test_EInfo());
	res += Count_Cycle(cur->Then_EInfo());
	res += Count_Cycle(cur->Else_EInfo());
	res += cur->Count_If_Cycle(cur->Inner_If_Then());
	res += cur->Count_If_Cycle(cur->Inner_If_Else());
    }
    return res;
}

INT 
SIMD_IF_ANA::CMove_Count() 
{
    if (_cmov_count == 0) {
	_cmov_count = 0;
	MEM_POOL_Push(&SIMD_local_pool);
	{
	    SINFO_VIS_Map seen_sinfo(101, &SIMD_local_pool);
	    IMEM_VIS_Map seen_imem(101, &SIMD_local_pool);
	    _cmov_count = CMove_Cycle(seen_sinfo, seen_imem);
	}
	MEM_POOL_Pop(&SIMD_local_pool);
    }
    return _cmov_count;
}

INT
SIMD_IF_ANA::If_Converted_Cycle()
{
    if (_if_conv_cycle != 0) {
	return _if_conv_cycle;
    }

    INT test_cycle = Count_Cycle(Test_EInfo());
    INT then_cycle = Count_Cycle(Then_EInfo());
    INT else_cycle = Count_Cycle(Else_EInfo());

    INT inner_then = Count_If_Cycle(Inner_If_Then());
    INT inner_else = Count_If_Cycle(Inner_If_Else());

    MEM_POOL_Push(&SIMD_local_pool);
    {
	IMEM_VIS_Map seen_load(101, &SIMD_local_pool);
	IMEM_VIS_Map seen_store(101, &SIMD_local_pool);
	INT common_cycle = Common_Load_Store_Cycle(seen_load, seen_store);
	_if_conv_cycle = test_cycle + then_cycle + else_cycle + 
	    inner_then + inner_else - common_cycle;
    }
    MEM_POOL_Pop(&SIMD_local_pool);
    Is_True(_if_conv_cycle >= 0, 
	    ("SIMD_IF_ANA::If_Converted_Cycle: cycle < 0"));
    return _if_conv_cycle;
}

bool 
SIMD_IF_ANA::Has_Mismatched_Vector_Length(DYN_ARRAY<SIMD_IF_ANA*> &a,
					  INT vec_length)
{
    for (INT i = 0; i < a.Elements(); i++) {
	SIMD_IF_ANA *cur = a[i];
	if (cur->Has_Mismatched_Vector_Length(vec_length)) {
	    return true;
	}
    }
    return false;
}

bool
SIMD_IF_ANA::Has_Mismatched_Vector_Length(EINFO_Stack &stack, INT vec_length)
{
    for (INT i = 0; i < stack.Elements(); i++) {
	SIMD_EINFO *cur = stack.Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (OPERATOR_is_store(oper)) {
	    INT cur_length = 
		Simd_Info->Get_SIMD_Width_Scalar(cur->Res_Type());
	    if (cur_length != vec_length) {
		return true;
	    }
	}
    }
    return false;
}

bool
SIMD_IF_ANA::Has_Mismatched_Vector_Length(INT p_v_length)
{
    SIMD_EINFO *test_einfo = Test_EInfo().Top();
    Is_True(OPERATOR_is_compare(WN_operator(test_einfo->Expr())),
	    ("SIMD_IF_ANA::Has_Mismatched_Vector_Length: unexpected if test"));
    INT vec_length = 
	Simd_Info->Get_SIMD_Width_Scalar(test_einfo->Res_Type());
    Is_True(vec_length>0, ("SIMD_IF_ANA::Has_Mismatched_Vector_Length: vector length is zero"));
    if (p_v_length != 0 && vec_length != p_v_length) {
	return true;
    }
    return (Has_Mismatched_Vector_Length(Then_EInfo(), vec_length) ||
	    Has_Mismatched_Vector_Length(Else_EInfo(), vec_length) ||
	    Has_Mismatched_Vector_Length(Inner_If_Then(), vec_length) ||
	    Has_Mismatched_Vector_Length(Inner_If_Else(), vec_length));
}

void
SIMD_IF_ANA::Enter_Unique_Speculative_LS(IMEM_INFO *imem)
{
    for (INT i = 0; i<_specu.Elements(); i++) {
	if (imem == _specu.Bottom_nth(i)) {
	    return;
	}
    }
    _specu.Push(imem);
}

void
SIMD_IF_ANA::Classify_One_Side(IMEM_VIS_Map &must_ls, IMEM_VIS_Map &specu_ls,
			       EINFO_Stack &einfos, 
			       DYN_ARRAY<SIMD_IF_ANA*> &simd_if_anas)
{
    /* decend on the inner ifs */
    for (INT i = 0; i<simd_if_anas.Elements(); i++) {
	SIMD_IF_ANA *cur = simd_if_anas.Get(i);
	cur->Classify_Load_Store_Rec();

	/* work on the inner must */
	for (INT j = 0; j < cur->_must.Elements(); j++) {
	    IMEM_INFO *imem = cur->_must.Bottom_nth(j);
	    Is_True(imem, ("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));
	    if (must_ls.Find(imem) == 0 && specu_ls.Find(imem) == 0) {
		specu_ls.Enter(imem, 1);
	    }
	}
    }
    
    /* work on the body */
    for (INT i = 0; i<einfos.Elements(); i++) {
	SIMD_EINFO *cur = einfos.Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	    IMEM_INFO *imem = Cur_Simd_Loop->IMem_Map().Find(cur->Expr());
	    Is_True(imem, ("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));
	    if (must_ls.Find(imem) == 0 && specu_ls.Find(imem) == 0) {
		specu_ls.Enter(imem, 1);
	    }
	}
    }
    
    /* work on the inner if speculative load/store */
    for (INT i = 0; i<simd_if_anas.Elements(); i++) {
	SIMD_IF_ANA *cur = simd_if_anas.Get(i);
	for (INT j = 0; j < cur->_specu.Elements(); j++) {
	    IMEM_INFO *imem = cur->_specu.Bottom_nth(j);
	    Is_True(imem, ("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));	    
	    if (must_ls.Find(imem) == 0 && specu_ls.Find(imem) == 0) {
		Enter_Unique_Speculative_LS(imem);
	    }
	}
    }
}

void
SIMD_IF_ANA::Classify_Load_Store_Rec()
{
    IMEM_VIS_Map must_ls(101, &SIMD_local_pool);
    IMEM_VIS_Map specu_then(101, &SIMD_local_pool);
    IMEM_VIS_Map specu_else(101, &SIMD_local_pool);

    
    for (INT i = 0; i<Test_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = Test_EInfo().Bottom_nth(i);
	OPERATOR oper = WN_operator(cur->Expr());
	if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	    IMEM_INFO *imem = Cur_Simd_Loop->IMem_Map().Find(cur->Expr());
	    Is_True(imem, ("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));
	    if (must_ls.Find(imem) == 0) {
		must_ls.Enter(imem, 1);
		_must.Push(imem);
	    }
	}
    }

    Classify_One_Side(must_ls, specu_then, Then_EInfo(), Inner_If_Then());
    Classify_One_Side(must_ls, specu_else, Else_EInfo(), Inner_If_Else());
    
    /* now merge the then/else speculative */
    HASH_TABLE_ITER<IMEM_INFO*, INT> htit(&specu_else);
    IMEM_INFO *imem;
    INT        val;
    while (htit.Step(&imem, &val)) {
	Is_True(imem && val, 
		("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));
	if (must_ls.Find(imem) != 0) {
	    /* do nothing */
	    ;
	} else if (specu_then.Find(imem) != 0) {
	    /* making it no speculative */
	    must_ls.Enter(imem, 1);
	    specu_then.Remove(imem);
	    _must.Push(imem);
	} else {
	    specu_then.Enter(imem, 1);
	}
    }
    
    HASH_TABLE_ITER<IMEM_INFO*, INT> specu_it(&specu_then);
    while (specu_it.Step(&imem, &val)) {
	Is_True(imem && val, 
		("SIMD_IF_ANA::Classify_Load_Store_Rec: no IMEM_INFO"));
	Is_True(must_ls.Find(imem) == 0, 
		("SIMD_IF_ANA::Classify_Load_Store_Rec: speculative cannot be must"));
	Enter_Unique_Speculative_LS(imem);
    }
}

void
SIMD_IF_ANA::Classify_Load_Store()
{
    MEM_POOL_Push(&SIMD_local_pool);
    {
	Classify_Load_Store_Rec();
	Screen_Unconditional_LS();
    }
    MEM_POOL_Pop(&SIMD_local_pool);
}

void
SIMD_IF_ANA::Screen_Unconditional_LS()
{
    if (Specu_IMem().Elements()>0) {
	IMEM_Stack temp_old(&SIMD_local_pool);
	IMEM_INFO *imem;
	IMEM_Stack &specu    = Specu_IMem();
	WN        *if_parent = LWN_Get_Parent(If_Wn());
	while (!specu.Is_Empty()) {
	    imem = specu.Pop();
	    temp_old.Push(imem);
	}
	for (INT i = 0; i < temp_old.Elements(); i++) {
	    imem = temp_old.Bottom_nth(i);
	    EXPR_Stack &lss = imem->Lod_Sto();
	    bool uncond = false;
	    for (INT j = 0; j < lss.Elements(); j++) {
		WN *ls = lss.Bottom_nth(j);
		if (LWN_Get_Parent(ls) == if_parent) {
		    uncond = true;
		    break;
		}
	    }
	    if (!uncond) {
		specu.Push(imem);
	    }
	}
    }
}

bool
SIMD_IF_ANA::Has_CMov(SIMD_EINFO *e_info, bool verbose)
{
    TYPE_ID mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
    INT     vl    = Simd_Ti->Get_SIMD_Width_Scalar(e_info->Res_Type());
    TYPE_ID xt_bool_type = Simd_Ti->Get_SIMD_Xtbool(vl);
    
    INTRINSIC intrin_id = Simd_Ti->CMov(mtype, xt_bool_type);
    bool res = intrin_id != INTRINSIC_INVALID;
    if (verbose && !res) {
      SIMD_Msg(AT_MSG_SIMD_NO_COND_MOVE, e_info->Expr(), SIMD_MTYPE_Msg(mtype));
    }
    return res;
}

bool
SIMD_IF_ANA::Check_Missing_CMov()
{
    // setup the Sel_EInfo for each sel
    CMove_Count();
    
    for (INT i = 0; i < Sel_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = Sel_EInfo().Bottom_nth(i);
	if (!Has_CMov(cur)) {
	    return true;
	}
    }
    return false;
}

void
SIMD_IF_ANA::Request_AT_CMov(SIMD_EINFO *e_info, SIMD_AT *simd_at)
{
    TYPE_ID rtype     = e_info->Res_Type();
    Is_True(rtype!=MTYPE_UNKNOWN, ("SIMD_IF_ANA::Request_AT_CMov: bad type"));
    AT_OP_ID at_op_id = simd_at->Factory()->get_at_op_id_cmov(rtype);
    simd_at->Request_Op(e_info->Expr(), false, at_op_id, 1);
}

void
SIMD_IF_ANA::Request_AT_CMov(SIMD_AT *simd_at) 
{
    // setup the Sel_EInfo for each sel
    CMove_Count();
    
    for (INT i = 0; i < Sel_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = Sel_EInfo().Bottom_nth(i);
	Request_AT_CMov(cur, simd_at);
    }
}

void
SIMD_IF_ANA::Request_Model_CMov(SIMD_EINFO *e_info, SIMD_MODEL *simd_m)
{
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	simd_m->Request_Scalar_Op(e_info->Expr(), false, 1);
	return;
    }
    TYPE_ID mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
    INT     vl    = Simd_Ti->Get_SIMD_Width_Scalar(e_info->Res_Type());
    TYPE_ID xt_bool_type = Simd_Ti->Get_SIMD_Xtbool(vl);
    
    INTRINSIC intrin_id = Simd_Ti->CMov(mtype, xt_bool_type);

    simd_m->Request_Op(e_info->Expr(), false, intrin_id, 1);
}

void
SIMD_IF_ANA::Request_Model_CMov(SIMD_MODEL *simd_m) 
{
    // setup the Sel_EInfo for each sel
    CMove_Count();
    
    for (INT i = 0; i < Sel_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = Sel_EInfo().Bottom_nth(i);
	Request_Model_CMov(cur, simd_m);
    }
}

bool
SIMD_IF_ANA::If_Conv_Analysis() 
{
  if (!Simd_Info->AT_Analysis_Phase()) {
    
    if (Has_Mismatched_Vector_Length()) {
      Cur_Simd_Loop->Set_Bad_Oper_Msg(AT_MSG_SIMD_IF_DIFF_VL, If_Wn());
      return true;
    }
    if (Check_Missing_CMov()) {
      Cur_Simd_Loop->Set_Bad_Oper_Msg(If_Wn());
      return true;
    }
  }
  
  Classify_Load_Store();
  
  if (Has_Specu() && !LNO_Simd_Aggressive_If_Conv && !SIMD_LOOP::Has_If_Convert_Pragma(If_Wn())) {
    Cur_Simd_Loop->Set_Bad_Oper_Msg(AT_MSG_SIMD_IF_UNSAFE_ACCESS, If_Wn());
    return true;
  }
  
  return false;
}
    
void
SIMD_IF_ANA::Add_EInfo(SIMD_EINFO *e_info) 
{
    if (Is_Test()) {
	Test_EInfo().Push(e_info);
    } else if (Is_Then()) {
	Then_EInfo().Push(e_info);
    } else {
	Else_EInfo().Push(e_info);
    }
}

void
SIMD_IF_ANA::Add_Inner_If(SIMD_IF_ANA *t) 
{
    if (Is_Then()) {
	Inner_If_Then().AddElement(t);
    } else {
	Is_True(Is_Else(), ("SIMD_IF_ANA::Add_Inner_If: nowhere to add"));
	Inner_If_Else().AddElement(t);
    }
}

SIMD_SEL_S* SIMD_IF::Get_Sel_S(SIMD_SCALAR* s_info)
{
    for (INT i = 0; i < _sel_s_info.Elements(); i++) {
	if (_sel_s_info[i]->S_Info() == s_info) {
	    return _sel_s_info[i];
	}
    }
    SIMD_SEL_S* new_sel_s = CXX_NEW(SIMD_SEL_S(s_info), 
				    _sel_s_info.Get_Mem_Pool());
    _sel_s_info.AddElement(new_sel_s);
    return new_sel_s;
}

SIMD_SEL_I* 
SIMD_IF::Get_Sel_I(IMEM_INFO* imem_info)
{
    for (INT i = 0; i < _sel_i_info.Elements(); i++) {
	if (_sel_i_info[i]->Imem_Info() == imem_info) {
	    return _sel_i_info[i];
	}
    }
    SIMD_SEL_I* new_sel_i = CXX_NEW(SIMD_SEL_I(imem_info), 
				    _sel_i_info.Get_Mem_Pool());
    _sel_i_info.AddElement(new_sel_i);
    return new_sel_i;
}

WN *
SIMD_IF::Generate_Sel(SIMD_PREG *res, WN *then_val, WN *else_val,
		      WN *last_def)
{
    WN *if_test = WN_if_test(_if_wn);
    WN *cond = LWN_Copy_Tree(if_test, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(if_test, cond, Du_Mgr);
    SIMD_EINFO *test_info = Cur_Simd_Loop->E_Info().Find(if_test);
    Is_True(test_info, ("SIMD_IF::Generate_Sel: if test doesn't have EINFO"));
    MEM_POOL   *pool = test_info->Pool();
    SIMD_EINFO      *cond_info = 
	Cur_Simd_Loop->Create_Simd_EInfo(cond, test_info);
    Cur_Simd_Loop->E_Info().Enter(cond, cond_info);
    
    if (!If_Converted()) {
	WN *t = WN_if_test(_if_wn); 
	WN_if_test(_if_wn) = cond;
	LWN_Set_Parent(cond, _if_wn);
	cond = t;
	Set_If_Converted();
    }

    /* FIXME: Currently we get confused if the select is simplified
       so we create it manually. This code needs to be smarter and
       use WN_Select. */
    WN *sel = WN_Create(OPR_SELECT, Promote_Type(res->Type()), MTYPE_V, 3);
    WN_kid(sel, 0) = cond;
    WN_kid(sel, 1) = then_val;
    WN_kid(sel, 2) = else_val;
    LWN_Set_Parent(cond, sel);
    LWN_Set_Parent(then_val, sel);
    LWN_Set_Parent(else_val, sel);
    
    SIMD_EINFO *kid_info = Cur_Simd_Loop->E_Info().Find(then_val);
    Is_True(kid_info, ("SIMD_IF::Generate_Sel: kid doesn't have EINFO"));
    SIMD_EINFO *e_info = Cur_Simd_Loop->Create_Simd_EInfo(sel, kid_info);
    Cur_Simd_Loop->E_Info().Enter(sel, e_info);
    
    WN *stid = res->Simd_Preg_Stid(sel);
    LWN_Insert_Block_Before(LWN_Get_Parent(_top_if_wn), _top_if_wn, stid);
    WN_linenum(stid) = LWN_Get_Linenum(_if_wn);
    e_info = Cur_Simd_Loop->Create_Simd_EInfo(stid, kid_info);
    Cur_Simd_Loop->E_Info().Enter(stid, e_info);
    
    SYMBOL symbol(stid);
    SIMD_SCALAR *s_info = CXX_NEW(SIMD_SCALAR(symbol, stid, pool), pool);
    Cur_Simd_Loop->S_Info_Map().Enter(symbol, s_info);
    Cur_Simd_Loop->S_Info_Stack().Push(s_info);

    /* Prepare the last_def for insertion in the block later. */
    if (last_def) {
	WN *ldid = res->Simd_Preg_Ldid();
	Du_Mgr->Add_Def_Use(stid, ldid);
	
	s_info->Add_Lod_Sto(ldid);
	e_info = Cur_Simd_Loop->Create_Simd_EInfo(ldid, kid_info);
	Cur_Simd_Loop->E_Info().Enter(ldid, e_info);
	
	WN_kid0(last_def) = ldid;
	LWN_Set_Parent(ldid, last_def);
    }
    
    return sel;
}

void
SIMD_IF::Generate_Sel(SIMD_IF *p_simd_if, bool is_then)
{
    WN *first_sel = NULL;
    for (INT i = 0; i < _sel_s_info.Elements(); i++) {
	SIMD_SEL_S *cur = _sel_s_info[i];
	SIMD_PREG  *then_preg = cur->Then_Preg();
	SIMD_PREG  *else_preg = cur->Else_Preg();
	SIMD_SCALAR *s_info = cur->S_Info();
	
	Is_True(then_preg || else_preg, 
		("SIMD_IF::Generate_Sel: no value on either side of an IF"));
	SIMD_PREG  *res = 
	  Gen_Symbol((then_preg) ? then_preg->Type() : else_preg->Type(), 
		     Cur_Simd_Loop->Pool(), s_info->Symbol().Name());
	WN *then_val = NULL;
	WN *else_val = NULL;
	if (then_preg) {
	    then_val = then_preg->Simd_Preg_Ldid_With_EInfo();
	} else {
	    then_val = s_info->Get_Load_Copy();
	}
	if (else_preg) {
	    else_val = else_preg->Simd_Preg_Ldid_With_EInfo();
	} else {
	    else_val = s_info->Get_Load_Copy();
	}
	WN *sel = Generate_Sel(res, then_val, else_val,
			       (p_simd_if == NULL) ? cur->Last_Def() : NULL);
	if (first_sel == NULL) {
	    first_sel = sel;
	}
	s_info->If_Conv_Sym_Push(res);
	if (p_simd_if) {
	    /* insert/update sel in p_simd_if */
	    p_simd_if->Update_Sel(s_info, res, cur->Last_Def(), is_then);
	}
    }

    for (INT i = 0; i < _sel_i_info.Elements(); i++) {
	SIMD_SEL_I *cur = _sel_i_info[i];
	SIMD_PREG  *then_preg = cur->Then_Preg();
	SIMD_PREG  *else_preg = cur->Else_Preg();
	IMEM_INFO  *imem_info = cur->Imem_Info();
	
	Is_True(then_preg || else_preg, 
		("SIMD_IF::Generate_Sel: no value on either side of an IF"));
	SIMD_PREG  *res = 
	    Gen_Symbol((then_preg) ? then_preg->Type() : else_preg->Type(), 
		       Cur_Simd_Loop->Pool());
	WN *then_val = NULL;
	WN *else_val = NULL;
	if (then_preg) {
	    then_val = then_preg->Simd_Preg_Ldid_With_EInfo();
	} else {
	    then_val = imem_info->Get_Load_Copy();
	}
	if (else_preg) {
	    else_val = else_preg->Simd_Preg_Ldid_With_EInfo();
	} else {
	    else_val = imem_info->Get_Load_Copy();
	}
	WN *sel = Generate_Sel(res, then_val, else_val,
			       (p_simd_if == NULL) ? cur->Last_Def() : NULL);
	if (first_sel == NULL) {
	    first_sel = sel;
	}
	imem_info->If_Conv_Sym_Push(res);
	if (p_simd_if) {
	    /* insert/update sel in p_simd_if */
	    p_simd_if->Update_Sel(imem_info, res, cur->Last_Def(), is_then);
	} 
    }

    /* Finalize all last defs if there're no parent ifs. We do that at the
       end so that we don't use the new values in the select condition. */
    if (!p_simd_if) {
      for (INT i = 0; i < _sel_s_info.Elements(); i++) {
	SIMD_SEL_S *cur = _sel_s_info[i];
	WN *last_def = cur->Last_Def();
	Is_True(last_def, ("Null last def."));
	
	LWN_Insert_Block_Before(LWN_Get_Parent(_top_if_wn), _top_if_wn, last_def);
      }
      
      for (INT i = 0; i < _sel_i_info.Elements(); i++) {
	SIMD_SEL_I *cur = _sel_i_info[i];
	
	WN *last_def = cur->Last_Def();
	Is_True(last_def, ("Null last def."));
	
	LWN_Insert_Block_Before(LWN_Get_Parent(_top_if_wn), _top_if_wn, last_def);
      }
    }

    INT count = _sel_s_info.Elements() + _sel_i_info.Elements();
    SIMD_IF_CONV *if_conv = CXX_NEW(SIMD_IF_CONV(first_sel, count), 
				    Cur_Simd_Loop->Pool());
    Cur_Simd_Loop->If_Conv_Map().Enter(first_sel, if_conv);
}

void
SIMD_IF::If_Conv_Sym_Pop() 
{
    for (INT i = 0; i < _sel_s_info.Elements(); i++) {
	SIMD_SEL_S *cur = _sel_s_info[i];
	SIMD_SCALAR *s_info = cur->S_Info();
	
	Is_True(s_info,	("SIMD_IF::If_Conv_Sym_Pop: no s_info"));
	s_info->If_Conv_Sym_Pop();
    }

    for (INT i = 0; i < _sel_i_info.Elements(); i++) {
	SIMD_SEL_I *cur = _sel_i_info[i];
	IMEM_INFO  *imem_info = cur->Imem_Info();
	
	Is_True(imem_info, ("SIMD_IF::If_Conv_Sym_Pop: no imem_info"));
	imem_info->If_Conv_Sym_Pop();
    }
}

void 
SIMD_SEL_S::Update_Last_Def(WN *last_def)
{
    Is_True(last_def, ("SIMD_SEL_S::Update_Last_Def: Null input"));
    if (_last_def) {
	Cur_Simd_Loop->Old_Stmt().Push(_last_def);
    }
    _last_def = last_def;
}

void 
SIMD_SEL_I::Update_Last_Def(WN *last_def)
{
    Is_True(last_def, ("SIMD_SEL_I::Update_Last_Def: Null input"));
    if (_last_def) {
	Cur_Simd_Loop->Old_Stmt().Push(_last_def);
    }
    _last_def = last_def;
}

void
SIMD_IF::Update_Sel(SIMD_SCALAR *s_info, SIMD_PREG *preg, 
		    WN *last_def, bool is_then)
{
    SIMD_SEL_S *cur = Get_Sel_S(s_info);
    if (is_then) {
	cur->Set_Then_Preg(preg);
    } else {
	cur->Set_Else_Preg(preg);
    }
    cur->Update_Last_Def(last_def);
}

void
SIMD_IF::Update_Sel(IMEM_INFO *imem, SIMD_PREG *preg, 
		    WN *last_def, bool is_then)
{
    SIMD_SEL_I *cur = Get_Sel_I(imem);
    if (is_then) {
	cur->Set_Then_Preg(preg);
    } else {
	cur->Set_Else_Preg(preg);
    }
    cur->Update_Last_Def(last_def);
}

void
SIMD_IF::Separate_Rhs(WN *wn, SIMD_PREG *preg)
{
  SIMD_EINFO *stid_info = Cur_Simd_Loop->E_Info().Find(wn);
  Is_True(stid_info, ("SIMD_IF::Separate_Rhs: no STID EINFO"));

  MEM_POOL *pool = stid_info->Pool();
  WN *stid = preg->Simd_Preg_Stid(WN_kid0(wn));
  SIMD_EINFO *e_info = Cur_Simd_Loop->Create_Simd_EInfo(stid, stid_info);
  Cur_Simd_Loop->E_Info().Enter(stid, e_info);
  LWN_Insert_Block_Before(LWN_Get_Parent(_top_if_wn), _top_if_wn, stid);
  WN_linenum(stid) = LWN_Get_Linenum(wn);
  
  /* extract the definition */
  WN_kid0(wn) = NULL;
  LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
  
  SYMBOL symbol(stid);
  SIMD_SCALAR *s_info = CXX_NEW(SIMD_SCALAR(symbol, stid, pool), pool);
  Cur_Simd_Loop->S_Info_Map().Enter(symbol, s_info);
  Cur_Simd_Loop->S_Info_Stack().Push(s_info);
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:





