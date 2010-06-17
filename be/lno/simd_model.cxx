
// simd_model.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD Auto Tie module:                                                     *
 *                                                                           *
 *    Functions for modeling the SIMD_LOOP                                   *
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

// $Id: simd_model.cxx $

#include <stdio.h>
#include "cxx_memory.h"
#include "intrn_info.h"
#include "opt_du.h"
#include "pu_info.h"
#include "reduc.h"
#include "simd.h"
#include "simd_imem.h"
#include "simd_if.h"
#include "simd_loop_at.h"
#include "simd_select.h"
#include "snl_utils.h"
#include "tie.h"
#include "tietypes.h"
#include "wn_util.h"
#include "wutil.h"
#include "ti_res_count.h"
#include "lnotarget.h"
#include "simd_model.h"
#include "simd_ti.h"
#include "simd_at.h"

/*---------------------------------------------------------------------------*
 * Constructor                                                               *
 *---------------------------------------------------------------------------*/
SIMD_MODEL::
SIMD_MODEL(MEM_POOL *pool): _pool(pool), _setup_scalar_count(0),
			    _loop_misc_count(0), _align_reg(0),
			    _res_ii(0), _orig_count(0.0), _loop_mem_count(0),
			    _model_vl(4)
{
    _loop_res_count = TI_RES_COUNT_Alloc(_pool);
    _setup_res_count = TI_RES_COUNT_Alloc(_pool);
}

SIMD_MODEL::~SIMD_MODEL()
{
}

// request an operator from SIMD model and trace it
// if setup is true, request a setup operation,
// if setup is false, request an inner loop operation
void
SIMD_MODEL::Request_Op(WN *wn, bool setup, INTRINSIC intrinsic, INT cnt) 
{
    Is_True(intrinsic != INTRINSIC_INVALID, 
	    ("SIMD_MODEL::Request_Op: Invalid intrinsic"));
    if (simd_debug) {
	fprintf(TFile,"\n(SIMD_MODEL): request %s vector intrinsic op %s (x%d)",
		setup ? "setup" : "loop", INTRINSIC_name(intrinsic), cnt);
	if (wn) {
	    fprintf(TFile,", line %d\n",
		    (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
	    fdump_tree(TFile,wn);
	} else {
	    fprintf(TFile,"\n");
	}
    }

    for (INT i = 0; i < cnt; i++) {
	if (setup) {
	    LNOTARGET_Tie_Intrinsic_Res(_setup_res_count, intrinsic);
	} else {
	    LNOTARGET_Tie_Intrinsic_Res(_loop_res_count, intrinsic);
	}
    }
}

void
SIMD_MODEL::Request_Load_Store(WN *wn, bool setup, INT cnt)
{
    if (simd_debug) {
	fprintf(TFile,"\n(SIMD_MODEL): request %s load/store %s (x%d)",
		setup ? "setup" : "loop", wn ? OPERATOR_name(WN_operator(wn)) : "unknown", cnt);
	if (wn) {
	    fprintf(TFile,", line %d\n",
		    (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
	    fdump_tree(TFile,wn);
	} else {
	    fprintf(TFile,"\n");
	}
    }

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	if (setup) {
	    Request_Scalar_Op(wn, setup, cnt);
	} else {
	    Request_Loop_Op(wn, cnt, true);
	}
	return;
    }

    SIMD_EINFO *e_info = Cur_Simd_Loop->E_Info().Find(wn);
    Is_True(e_info, ("SIMD_MODEL::Request_Load_Store: Null EINFO"));
    
    TYPE_ID mem_type = Simd_Ti->Get_SIMD_Mem_Type_Scalar(e_info->Res_Type());
    TYPE_ID reg_type = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
    TIE_MACRO_p tie_macro = NULL;
    if (mem_type == reg_type) {
	tie_macro = tie_info->mtype_loadi_macro(mem_type);
	Is_True(tie_macro, ("SIMD_MODEL::Request_Load_Store: No loadi"));
    }
    if (tie_macro == NULL) {
	tie_macro = tie_info->mtype_mtor_macro(mem_type, reg_type);
    }
    if (tie_macro == NULL) {
	tie_macro = tie_info->mtype_mtor_macro(mem_type, mem_type);
    }
    if (tie_macro == NULL) {
	tie_macro = tie_info->mtype_loadi_macro(mem_type);
    }
    Is_True(tie_macro != NULL,
	    ("SIMD_MODEL::Request_Load_Store: no TIE load/store"));

    INTRINSIC intrinsic = Tie_Macro_Id_To_Intrinsic(tie_macro->id());
    Is_True(intrinsic != INTRINSIC_INVALID, 
	    ("SIMD_MODEL::Request_Load_Store: Invalid intrinsic"));
    
    for (INT i = 0; i < cnt; i++) {
	if (setup) {
	    LNOTARGET_Tie_Intrinsic_Res(_setup_res_count, intrinsic);
	} else {
	    LNOTARGET_Tie_Intrinsic_Res(_loop_res_count, intrinsic);
	}
    }
}

void
SIMD_MODEL::Request_Loop_Op(WN *wn, INT cnt, bool is_mem)
{
    if (simd_debug) {
	fprintf(TFile,"\n(SIMD_MODEL): request %s load/store %s (x%d)",
		"loop", wn ? OPERATOR_name(WN_operator(wn)) : "unknown", cnt);
	if (wn) {
	    fprintf(TFile,", line %d\n",
		    (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
	    fdump_tree(TFile,wn);
	} else {
	    fprintf(TFile,"\n");
	}
    }
    
    if (is_mem) {
	_loop_mem_count++;
    } else {
	_loop_misc_count++;
    }
} 

void
SIMD_MODEL::Request_Scalar_Op(WN *wn, bool setup, INT cnt) 
{
    if (simd_debug) {
	fprintf(TFile,"\n(SIMD_MODEL): request %s scalar op %s (x%d)",
		setup ? "setup" : "loop", wn ? OPERATOR_name(WN_operator(wn)) : "unknown", cnt);
	if (wn) {
	    fprintf(TFile,", line %d\n",
		    (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
	    fdump_tree(TFile,wn);
	} else {
	    fprintf(TFile,"\n");
	}
    }
    
    if (setup) {
	_setup_scalar_count++;
    } else {
	_loop_misc_count++;
    }
}


void 
SIMD_MODEL::Add_Address_Overhead(WN *wn)
{
    OPERATOR oper = WN_operator(wn);
    if (Cur_Simd_Loop->Invariant_In_Simd_Loop(wn)) {
	return;
    }
    
    if (oper == OPR_LDID && ST_class(WN_st(wn)) == CLASS_PREG) {
	return;
    } 
    
    if (oper == OPR_ARRAY) {
	/* Don't add the cost for array node.  It should be the
	   same as in the original loop */
	return;
    }

    /* enter the kids first */
    for (INT i = 0; i < WN_kid_count(wn); i++) {
	Add_Address_Overhead(WN_kid(wn, i));
    }
    
    /* enter the current WN */
    Request_Scalar_Op(wn, /* setup */ true, 1);
}

/*--------------------------------------------------------------------------*
 * Add unaligned load/store counts                                          *
 *--------------------------------------------------------------------------*/ 
void
SIMD_MODEL::Add_Alignment_Overhead(WN *wn)
{
    SIMD_LOOP *simd_loop = Cur_Simd_Loop;

    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_ILOAD || OPR_ISTORE, 
	    ("SIMD_MODEL::Add_Simd_Loop_Unaligned: expect iload/istore"));

    /* add prime address overhead */
    Request_Scalar_Op(NULL, true, 1);

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	Request_Scalar_Op(wn, true, 1);
	return;
    }

    SIMD_EINFO *e_info = simd_loop->E_Info().Find(wn);
    Is_True(e_info, ("SIMD_MODEL::Add_Alignment_Overhead: Null EINFO"));
    	
    TYPE_ID mem_type = Simd_Ti->Get_SIMD_Mem_Type_Scalar(e_info->Res_Type());
    TYPE_ID reg_type = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
    if (oper == OPR_ILOAD) {
	INTRINSIC intrinsic = Simd_Ti->Load_Align(SIMD_TI::LOADA_PRIME, mem_type, reg_type);
        Request_Op(wn, true, intrinsic, 1);
    } else {
	INTRINSIC intrinsic = Simd_Ti->Store_Align(SIMD_TI::STOREA_PRIME, mem_type, reg_type);
        Request_Op(wn, true, intrinsic, 1);
	intrinsic = Simd_Ti->Store_Align(SIMD_TI::STOREA_FLUSH, mem_type, reg_type);
    }
}

INTRINSIC
SIMD_MODEL::Get_Select_Intrinsic(INT64 sel, TYPE_ID mtype, bool &has_dsel)
{
    INTRINSIC intrin_id = INTRINSIC_INVALID;
    if (has_dsel) {
	intrin_id = Simd_Ti->Tie_Dsel_Intrinsic_Imm(mtype, sel);
	if (intrin_id == INTRINSIC_INVALID) {
	    intrin_id = Simd_Ti->Tie_Dsel_Intrinsic(mtype);
	} 
    }
    if (intrin_id == INTRINSIC_INVALID) {
	has_dsel = false;
	SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
	INT64 sel_0, sel_1;
	simd_sel->Dsel_To_Sel(sel, sel_1, sel_0);
        INT64 sel = (Target_Byte_Sex == LITTLE_ENDIAN) ? sel_0 : sel_1;
	intrin_id = Simd_Ti->Tie_Sel_Intrinsic_Imm(mtype, sel);
	if (intrin_id == INTRINSIC_INVALID) {
          intrin_id = Simd_Ti->Tie_Sel_Intrinsic(mtype);
	}
    }
    Is_True(intrin_id != INTRINSIC_INVALID, 
	    ("Can't find VSEL for (%s)", MTYPE_name(mtype)));

    return intrin_id;
}

void
SIMD_MODEL::Add_Simd_Select_By_Table(bool setup,
				     WN *wn, TYPE_ID scalar_type,
				     INT num_fields,
				     bool is_read)
{
    VSEL_IMEMS &vsel_table = (is_read) ? 
	Cur_Simd_Loop->IMem_Map().Vsel_R_Table() :
	Cur_Simd_Loop->IMem_Map().Vsel_W_Table();

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	Request_Loop_Op(wn, 6 /* default for vl of 4 */, false);
	return;
    }

    TYPE_ID      mtype    = Simd_Ti->Get_SIMD_Reg_Type_Scalar(scalar_type);
    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
    INT          vl       = Simd_Ti->Get_SIMD_Width_Scalar(scalar_type);

    VSEL_OP *vsel_ops = Find_Vsel_Op(vsel_table, num_fields, vl);
    FmtAssert(vsel_ops, ("SIMD_MODEL::Add_Simd_Select_By_Table: entry not in the table"));    
    
    while (vsel_ops->input_0 != -1) {
	INT64 sel = simd_sel->Sel_Array_To_Imm(vsel_ops->sel_0);
	bool has_dsel = false;
	INTRINSIC intrin_id = Get_Select_Intrinsic(sel, mtype, has_dsel);
	Request_Op(wn, setup, intrin_id, 1);
	vsel_ops++;
    }
}

				     
/*---------------------------------------------------------------------------*
 * Add read select operation counts for a simd loop                          *
 *---------------------------------------------------------------------------*/
void
SIMD_MODEL::Add_Simd_Field_Select(bool setup,
				  WN *wn, TYPE_ID scalar_type, INT num_fields,
				  bool pairwise, bool is_read)
{
    if (pairwise) {
	if (LNO_IS_POWER_OF_TWO(num_fields)) {
	    TYPE_ID mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(scalar_type);
	    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
	    INT64 sel = simd_sel->Dsel_73625140();
	    bool has_dsel = true;
	    INTRINSIC intrin_id = Get_Select_Intrinsic(sel, mtype, has_dsel);

	    // compute the number of selects that we need in order to perform
	    // the recursive extraction of elements
	    INT sel_count_per_level = num_fields / 2;
	    INT levels = 0;
	    for (INT nf = num_fields; nf > 1; nf /= 2) {
		levels++;
	    }
	    INT sel_count = sel_count_per_level * levels;
	    if (!has_dsel) {
		sel_count = 2 * sel_count;
	    }
	    Request_Op(wn, setup, intrin_id, sel_count);
	} else {
	    Add_Simd_Select_By_Table(setup, wn, scalar_type, num_fields,
				     is_read);
	}
    } else {
	if (num_fields == 2) {
	    TYPE_ID mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(scalar_type);
	    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
	    INT64 sel = simd_sel->Dsel_75316420();
	    bool has_dsel = true;
	    INTRINSIC intrin_id = Get_Select_Intrinsic(sel, mtype, has_dsel);

	    Request_Op(wn, setup, intrin_id, (has_dsel) ? 1 : 2);
	} else {
	    Add_Simd_Select_By_Table(setup, wn, scalar_type, num_fields,
				     is_read);
	}
    }
}


/*---------------------------------------------------------------------------*
 * Add read select operation counts for a simd loop                          *
 *---------------------------------------------------------------------------*/
void
SIMD_MODEL::Add_Simd_Read_Select(bool setup, WN *wn, IMEM_ELEM *ie)
{
    INT num_fields = ie->Offset_Count();
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	if (num_fields == 2) {
	    Request_Scalar_Op(wn, setup, 2);
	} else {
	    Request_Scalar_Op(wn, setup, num_fields * 2 /* for vl of 4 */);
	}
	return;
    }
    TYPE_ID scalar_type = ie->Parent_Group()->Scalar_Type();
    INT reg_length = Simd_Ti->Get_SIMD_Width_Scalar(scalar_type);
    bool pairwise = (num_fields > 2) && (num_fields >= (reg_length >> 1));
    Add_Simd_Field_Select(setup, wn, scalar_type, num_fields, pairwise, true);
}


/*---------------------------------------------------------------------------*
 * Add write select operation counts for a simd loop                         *
 *---------------------------------------------------------------------------*/
void
SIMD_MODEL::Add_Simd_Write_Select(bool setup, WN *wn, IMEM_ELEM *ie)
{
    INT num_fields = ie->Offset_Count();
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	if (num_fields == 2) {
	    Request_Scalar_Op(wn, setup, 2);
	} else {
	    Request_Scalar_Op(wn, setup, num_fields * 2 /* for vl of 4 */);
	}
	return;
    }
    TYPE_ID scalar_type = ie->Parent_Group()->Scalar_Type();
    INT reg_length = Simd_Ti->Get_SIMD_Width_Scalar(scalar_type);
    bool pairwise = true;
    /* write use the same select as read, only the input are organized
       differently, see simd_imem.cxx */
    Add_Simd_Field_Select(setup, wn, scalar_type, num_fields, pairwise, false);
}

void
SIMD_MODEL::Add_Simd_Load_Shift_In (bool setup, IMEM_GROUP *ig)
{
    Is_True(ig->Elem_Count() == 1,
	    ("Expected a single element in the group"));
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	Request_Scalar_Op(NULL, true, 1);
	Request_Loop_Op(NULL, (_model_vl-1),  true);
	Request_Loop_Op(NULL, (_model_vl-1),  false);
	return;
    }
    
    IMEM_ELEM *ie = ig->Elem(0);
    WN *load = ig->Loads()->Get(0);
    TYPE_ID scalar_type = ig->Scalar_Type();
    INT vl = Simd_Ti->Get_SIMD_Width_Scalar(scalar_type);
    TYPE_ID mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(scalar_type);
    
    // request shift in select (vector length minus 1)
    SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
    INT64             sel = simd_sel->Sel_4321();
    bool has_dsel = false;
    INTRINSIC intrin_id = Get_Select_Intrinsic(sel, mtype, has_dsel);
    Request_Op(load, setup, intrin_id, ie->Offset_Count() * (vl - 1));

    // Get LS
    TIE_MACRO_p tie_macro = tie_info->mtype_mtor_macro(scalar_type, mtype);
    Is_True(tie_macro, ("SIMD_MODEL::Addd_Load_Shift_In: no LS"));
    intrin_id = Tie_Macro_Id_To_Intrinsic(tie_macro->id());

    // request an initial, non-updating load scalar
    Request_Op(load, true, intrin_id, ie->Offset_Count());
    
    // request (vl-1) LS in the loop 
    Request_Op(load, setup, intrin_id, ie->Offset_Count() * (vl - 1));

    return;
}

void
SIMD_MODEL::Add_Simd_Load_Reuse_Shift(bool setup, IMEM_ELEM *ie)
{
    IMEM_GROUP *ig         = ie->Parent_Group();
    WN         *load       = ig->Loads()->Get(0);
    TYPE_ID    scalar_type = ig->Scalar_Type();
    IMEM_INFO  *first_imem = ig->First_Imem_Info();
    IMEM_INFO  *cur_imem   = ie->Offset(0)->Imem_Info();

    INT idiff;
    cur_imem->Compare_Imem_Index(first_imem, &idiff);

    Is_True(idiff>=0, ("SIMD_MODEL::Add_Simd_Load_Reuse_Shift: negative distance"));

    INT         vl          = Simd_Ti->Get_SIMD_Width_Scalar(scalar_type);
    TYPE_ID     mtype       = Simd_Ti->Get_SIMD_Reg_Type_Scalar(scalar_type);

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	INT offset = idiff * ie->Offset_Count() % _model_vl;
	if (offset != 0) {
	    Request_Scalar_Op(load, setup, _model_vl);
	}
	return;
    }

    INT  offset       = idiff * ie->Offset_Count() % vl;
    if (offset != 0) {
	Is_True(vl > offset, ("SIMD_MODEL::Add_Simd_Load_Reuse_Shift: offset too big"));
	SIMD_SELECT *simd_sel = Simd_Ti->Get_SIMD_Select_SIMD(mtype);
	INT64 sel = simd_sel->Sel_4321(offset);
	bool has_dsel = false;
	INTRINSIC intrin_id = Get_Select_Intrinsic(sel, mtype, has_dsel);
	Request_Op(load, setup, intrin_id, vl);
    }
}

void
SIMD_MODEL::Request_Mpy_Op(WN *wn, OPERATOR oper, TYPE_ID out_type, 
			   TYPE_ID in_type, bool setup, INT cnt)
{
    INTRINSIC intrin_id;
    if (Simd_Target == SIMD_TARG_GENERIC) {
	intrin_id = Simd_Ti->Find_Function(oper, in_type, out_type);
	Request_Op(wn, setup, intrin_id, cnt);
    } else if (Simd_Ti->Has_Paired_Mac(oper, out_type, in_type)) {
	intrin_id  = Simd_Ti->Find_Function(oper, in_type, out_type);
	Request_Op(wn, setup, intrin_id, cnt);
    } else {
	intrin_id = 
	    Simd_Ti->Tie_Mac_Intrinsic(oper, out_type, in_type, true);
	Request_Op(wn, setup, intrin_id, cnt);
	intrin_id = 
	    Simd_Ti->Tie_Mac_Intrinsic(oper, out_type, in_type, false);
	Request_Op(wn, setup, intrin_id, cnt);
    }
    return;
}

void
SIMD_MODEL::Add_Simd_Orig_Op(WN *wn)
{
    SIMD_LOOP *simd_loop = Cur_Simd_Loop;
    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Add_Simd_Orig_Op(kid);
	}
	return;
    }
    
    if (oper == OPR_DO_LOOP) {
	Add_Simd_Orig_Op(WN_do_body(wn));
	return;
    }
    
    if (oper == OPR_IF) {
	/* conditional moves */
	SIMD_IF_ANA *if_ana = simd_loop->If_Ana_Map().Find(wn);
	Is_True(if_ana, ("SIMD_MODEL::Add_Simd_Loop_Op: No SIMD_IF_ANA"));
	Inc_Orig_Count(if_ana->Original_Cycle());
	return;
    }

    if (simd_loop->Invariant_In_Simd_Loop(wn)) {
      return;
    }
    
    if (oper == OPR_LDID) {
	return;
    }
    
    if (oper == OPR_STID || oper == OPR_ISTORE) {
	/* enter the RHS first */
	Add_Simd_Orig_Op(WN_kid0(wn));
	if (oper == OPR_STID) {
	    return;
	}
    } else if (oper == OPR_SHL || oper == OPR_ASHR || oper == OPR_LSHR ||
	       oper == OPR_DIV) {
	Add_Simd_Orig_Op(WN_kid0(wn));
    } else if (oper == OPR_INTRINSIC_CALL || oper == OPR_INTRINSIC_OP) {
	INTRINSIC intrn = (INTRINSIC)WN_intrinsic(wn);
	TIE_MACRO_p tie_macro = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intrn));
	Is_True(tie_macro!=NULL,
		("Can't find a TIE macro for intrinsic %s",INTRINSIC_name(intrn)));
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    INT proto_idx = tie_macro->whirl_to_proto_index(i);
	    if (!tie_macro->proto_is_immed(proto_idx)) {
		// don't request any operations for TIE macro immediates
		// (for example, int to vector of ints converts)
		Add_Simd_Orig_Op(WN_kid(wn, i));
	    }
	}
    } else if (!OPERATOR_is_load(oper)) {
	/* enter the kids first */
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    Add_Simd_Orig_Op(WN_kid(wn, i));
	}
	if (oper == OPR_PARM || oper == OPR_OUTPART) {
	    return;
	}
    }
    
    SIMD_EINFO *e_info = simd_loop->Get_E_Info(wn);
    
    IMEM_GROUP *ig = NULL;
    if (e_info != NULL) {
	IMEM_INFO  *imem = e_info->IMem();
	if (imem) {
	    Is_True(imem->Imem_Offset(),("No IMEM grouping info."));
	    ig = imem->Imem_Offset()->Parent_Group();
	}
    }
    
    if (ig && ig->Is_Vector()) {
	if (ig->AT_Vl() == 254) {
	    /* already processed */
	    return;
	}
	
	ig->Set_AT_Vl(254);

	// load
	if (ig->Is_Use()) {
	    for (INT i = 0; i < ig->Elem_Count(); i++) {
		IMEM_ELEM *ie = ig->Elem(i);
		Inc_Orig_Count(ie->Offset_Count());
	    }
	}
	
	// store
	if (ig->Is_Def()) {
	    // any store in the group would do
	    for (INT i = 0; i < ig->Elem_Count(); i++) {
		IMEM_ELEM *ie = ig->Elem(i);

		// a store for each offset in the group
		Inc_Orig_Count(ie->Offset_Count());
	    }
	}
	return;
    }
    
    /* enter the current WN */
    Inc_Orig_Count(1.0);
}

/*---------------------------------------------------------------------------*
 * Add operation counts for a simd loop                                      *
 *---------------------------------------------------------------------------*/
void
SIMD_MODEL::Add_Simd_Loop_Op(WN *wn, WN *enclosing_loop)
{
    SIMD_LOOP *simd_loop = Cur_Simd_Loop;

    // All operations in the inner loop go into the loop operations in that STAT.
    // The rest go into the setup operation map.
    bool setup = (enclosing_loop != Inner_Loop());
    
    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Add_Simd_Loop_Op(kid, enclosing_loop);
	}
	return;
    }
    
    if (oper == OPR_DO_LOOP) {
	Add_Simd_Loop_Op(WN_do_body(wn), wn);
	return;
    }
    
    if (oper == OPR_IF) {
	/* conditional moves */
	SIMD_IF_ANA *if_ana = simd_loop->If_Ana_Map().Find(wn);
	Is_True(if_ana, ("SIMD_MODEL::Add_Simd_Loop_Op: No SIMD_IF_ANA"));
	INT cmov_cnt = if_ana->CMove_Count();
	Is_True(cmov_cnt > 0, 
		("SIMD_MODEL::Add_Simd_Loop_Op: cmov count is zero"));

	Add_Simd_Loop_Op(WN_if_test(wn), enclosing_loop);
	Add_Simd_Loop_Op(WN_then(wn), enclosing_loop);
	Add_Simd_Loop_Op(WN_else(wn), enclosing_loop);

	if_ana->Request_Model_CMov(this);
	return;
    }

    if (!simd_loop->Varies_With_Loop(wn, simd_loop->Simd_Loop_Level())) {
	// TODO: handle immediates separately here
	if (oper == OPR_ILOAD ||
	    (oper == OPR_LDID && ST_class(WN_st(wn)) != CLASS_PREG)) {
	    Request_Load_Store(wn, true, 1);
	} else {
	    // request a register to register CVT
	    Request_Scalar_Op(wn, true, 1);
	}
	return;
    }

    SIMD_EINFO *e_info = simd_loop->Get_E_Info(wn);
    
    if (oper == OPR_LDID) {
	Is_True(e_info != NULL, ("Null EINFO!"));
	if (!e_info->Reduction()) {
	  return;
	}

	// request reduction
	DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
	WN *def_loop = def_list->Loop_stmt();
	if (def_loop != NULL && Is_Descendent(simd_loop->Simd_Loop(), def_loop)) {
	  
	  if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	    Request_Scalar_Op(wn, true, 1);
	    return;
	  }
	  
	  REDUCTION_TYPE red = red_manager->Which_Reduction(e_info->Expr());
	  TYPE_ID rtype        = e_info->Res_Type();
	  TYPE_ID mtype        = Simd_Ti->Get_SIMD_Reg_Type_Scalar(rtype);
	  INTRINSIC intrin_id = INTRINSIC_INVALID;
	  switch (red) {
	  case RED_ADD:
	    intrin_id  = Simd_Ti->Radd(rtype, mtype);
	    break;
	    
	  case RED_MIN:
	    intrin_id  = Simd_Ti->Rmin(rtype, mtype);
	    break;
	    
	  case RED_MAX:
	    intrin_id  = Simd_Ti->Rmax(rtype, mtype);
	    break;
	    
	  default:
	    break;
	  }
	  
	  if (intrin_id != INTRINSIC_INVALID) {
	    Request_Op(wn, /* setup */ true, intrin_id, 1);
	  }
	}

	return;
    }
    
    if (e_info != NULL && e_info->Mula()) {
	// request MADD/MSUB
	Is_True(((oper == OPR_ADD || oper == OPR_SUB) &&
		 (WN_operator(WN_kid1(wn)) == OPR_MPY)), ("Bad MULA expression."));
	if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	    return Request_Scalar_Op(wn, setup, 2);
	}
	WN *mpy_kid = WN_kid1(wn);
	Add_Simd_Loop_Op(WN_kid0(wn), enclosing_loop);
	Add_Simd_Loop_Op(WN_kid0(mpy_kid), enclosing_loop);
	Add_Simd_Loop_Op(WN_kid1(mpy_kid), enclosing_loop);
	
	TYPE_ID mtype      = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
	SIMD_EINFO *k_info = simd_loop->Get_E_Info(mpy_kid);
	TYPE_ID in_type    = Simd_Ti->Get_SIMD_Reg_Type_Scalar(k_info->Res_Type());
	return Request_Mpy_Op(wn, (oper == OPR_ADD) ? OPR_MADD : OPR_MSUB,
			      mtype, in_type, setup, 1);
    }
    
    if (oper == OPR_STID || oper == OPR_ISTORE) {
	/* enter the RHS first */
	Add_Simd_Loop_Op(WN_kid0(wn), enclosing_loop);
	if (oper == OPR_STID) {
	    return;
	}
    } else if (oper == OPR_SHL || oper == OPR_ASHR || oper == OPR_LSHR ||
	       oper == OPR_DIV) {
	Add_Simd_Loop_Op(WN_kid0(wn), enclosing_loop);
	if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	    return Request_Scalar_Op(wn, setup, 1);
	}
	bool immed = false;
	INT  shift_amount = 0;
	Is_Shift(wn, immed, shift_amount);
	if (oper == OPR_DIV) {
          oper = (Simd_Target == SIMD_TARG_VECTRA_I ||
                  Simd_Target == SIMD_TARG_VECTRA_II) ? OPR_ASHR : OPR_LSHR;
	}
	INTRINSIC intrin_id = INTRINSIC_INVALID;
	TYPE_ID  rtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
	if (immed) {
	    intrin_id = 
		Simd_Ti->Tie_Shift_Intrinsic_Imm(oper, rtype, shift_amount);
	}
	if (intrin_id == INTRINSIC_INVALID) {
	    intrin_id = Simd_Ti->Tie_Shift_Intrinsic(oper, rtype);
	}

	Is_True(intrin_id != INTRINSIC_INVALID, ("No shift intrinsic"));
	Request_Op(wn, setup, intrin_id, 1);
	return;	
    } else if (oper == OPR_INTRINSIC_CALL || oper == OPR_INTRINSIC_OP) {
	INTRINSIC intrn = (INTRINSIC)WN_intrinsic(wn);
	TIE_MACRO_p tie_macro = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intrn));
	Is_True(tie_macro!=NULL,
		("Can't find a TIE macro for intrinsic %s",INTRINSIC_name(intrn)));
	INT vl = 0;
	if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	    vl = _model_vl;
	} else if (e_info) {
	    vl = Simd_Ti->Get_SIMD_Width_Scalar(e_info->Res_Type());
	}
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    INT proto_idx = tie_macro->whirl_to_proto_index(i);
	    if (!tie_macro->proto_is_immed(proto_idx)) {
		// don't request any operations for TIE macro immediates
		// (for example, int to vector of ints converts)
		WN *kid = WN_kid(wn,i);
		Add_Simd_Loop_Op(kid, enclosing_loop);
		if (vl == 0) {
		    SIMD_EINFO *kid_info = simd_loop->Get_E_Info(kid);
		    vl = Simd_Ti->Get_SIMD_Width_Scalar(kid_info->Res_Type());
		}
	    }
	}
	FmtAssert(vl!=0,("SIMD_MODEL::Add_Simd_Loop_Op: No VL for intrinsic"));
	if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	    return Request_Scalar_Op(wn, setup, 1);
	} 
	INTRINSIC intrin_id = Tie_Vector_Intrinsic(wn, vl);
	Request_Op(wn, setup, intrin_id, 1);
	return;
    } else if (!OPERATOR_is_load(oper)) {
	/* enter the kids first */
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    Add_Simd_Loop_Op(WN_kid(wn, i), enclosing_loop);
	}
	if (oper == OPR_PARM || oper == OPR_OUTPART) {
	    return;
	}
    }
    
    Is_True(e_info != NULL || oper == OPR_INTRINSIC_CALL,
	    ("SIMD_MODEL::Add_Simd_Loop_Op: NULL EINFO"));
    
    IMEM_GROUP *ig = NULL;
    if (e_info != NULL) {
	IMEM_INFO  *imem = e_info->IMem();
	if (imem) {
	    Is_True(imem->Imem_Offset(),("No IMEM grouping info."));
	    ig = imem->Imem_Offset()->Parent_Group();
	}
    }
    
    if (ig) {
	if (ig->Is_Vector()) {
	    if (ig->AT_Vl() == 255) {
		/* already processed */
		return;
	    }
	
	    ig->Set_AT_Vl(255);

	    // load
	    if (ig->Is_Use()) {
		// any load in the group would do
		WN *load = ig->Loads()->Get(0);
		INTRINSIC intrin_id = INTRINSIC_INVALID;
		if (ig->First_Imem_Info()->Has_Reuse()) {
		    /* vsel */
		    Request_Scalar_Op(NULL, false, 1);
		}
		if (!ig->Is_Aligned()) {
		    // each unaligned load group uses one alignment register
		    Inc_Align_Reg_Count(1);
		
		    /* enter alignment load overhead */
		    Add_Alignment_Overhead(load);
		
		} else if (ig->Variable_Stride()) {
		    Add_Simd_Load_Shift_In(setup, ig);
		    return;
		}
	    
		for (INT i = 0; i < ig->Elem_Count(); i++) {
		    IMEM_ELEM *ie = ig->Elem(i);
		
		    // a normal or alignment load for each offset in the group
		    // they shall occupy the same resource, so use the normal one
		    Request_Load_Store(load, setup, ie->Offset_Count());
		    
		    // add deinterleave overhead
		    if (ig->Elem(i)->Offset_Count() > 1) {
			Add_Simd_Read_Select(setup, load, ie);
		    }

		    // add load reuse overhead
		    Add_Simd_Load_Reuse_Shift(setup, ie);
		}
	    }
	
	    // store
	    if (ig->Is_Def()) {
		// any store in the group would do
		WN *store = ig->Stores()->Get(0);
		if (!ig->Is_Aligned()) {
		    // each unaligned store group uses one alignment register
		    Inc_Align_Reg_Count(/* count */ 1);
		    
		    /* enter alignment store overhead */
		    Add_Alignment_Overhead(store);
		
		    /* enter final alignment store, use normal store
		       because they use the same resource */
		    Request_Load_Store(store, true, 1);
		}
	    
		for (INT i = 0; i < ig->Elem_Count(); i++) {
		    IMEM_ELEM *ie = ig->Elem(i);

		    // a store for each offset in the group
		    Request_Load_Store(store, setup, ie->Offset_Count());
		
		    // add interleave overhead
		    if (ig->Elem(i)->Offset_Count() > 1) {
			Add_Simd_Write_Select(setup, store, ie);
		    }
		}
	    }
	} else {
	    Request_Load_Store(wn, setup, 1);
	}
	return;
    }

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	if (e_info && (OPERATOR_is_load(oper) || OPERATOR_is_store(oper))) {
	    Request_Load_Store(wn, setup, 1);
	} else {
	    Request_Scalar_Op(wn, setup, 1);
	}
	return;
    }

    /* special case CVT/CVTL */
    if (oper == OPR_CVT ||
        oper == OPR_CVTL ||
        oper == OPR_TRUNC) {
	Request_Scalar_Op(wn, setup, 1);
	return;
    } 

    /* enter the current WN */
    TYPE_ID     mtype = Simd_Ti->Get_SIMD_Reg_Type_Scalar(e_info->Res_Type());
    TYPE_ID     desc_scalar = AT_FACTORY::canonical_kid_type(wn, 0, &AT_WN_Decode_SIMD_EINFO);
    TYPE_ID     desc = Simd_Ti->Simd_Reg_Type_Of_Scalar(desc_scalar);

    if (oper == OPR_MPY) {
	return Request_Mpy_Op(wn, oper, mtype, desc, setup, 1);
    }
    
    if (OPERATOR_is_compare(oper)) {
      int vl = Simd_Ti->Get_SIMD_Width_SIMD(mtype);
      mtype = Simd_Ti->Get_SIMD_Xtbool(vl);
    }
    
    TIE_MACRO_ID macro_id = Simd_Ti->Search_Vector_Macro(oper, desc, mtype);
    if (macro_id == TIE_INVALID_ID && OPERATOR_is_compare(oper)) {
      OPERATOR new_oper =
        (oper == OPR_NE || oper == OPR_EQ) ?
        SIMD_Negate_Comparison(oper) : SIMD_Swap_Comparison(oper);
      macro_id = Simd_Ti->Search_Vector_Macro(new_oper, desc, mtype);
      if (macro_id != TIE_INVALID_ID)
        oper = new_oper;
    }

    INTRINSIC intrin_id = Simd_Ti->Find_Function(oper, desc, mtype);
    Request_Op(wn, setup, intrin_id, 1);
}

/*---------------------------------------------------------------------------*
 * Create a new region for a simd loop                                       *
 *---------------------------------------------------------------------------*/
void 
SIMD_MODEL::Model_Simd_Loop(SIMD_LOOP *simd_loop)
{
    WN *loop_wn = simd_loop->Simd_Loop();
    Set_Cur_At_Simd_Loop(simd_loop); // for SIMD_EINFO decoder routine
    if (Simd_Target != SIMD_TARG_AT_ANALYSIS) {
	simd_loop->IMem_Map().Split_Group_No_Vsel();
    }
    
    Set_Inner_Loop(simd_loop->Loop_Model()->Model_Inner_Loop());   
    Add_Simd_Loop_Op(WN_do_body(loop_wn), loop_wn);
    Add_Simd_Orig_Op(WN_do_body(loop_wn));

    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	_res_ii = MAX(_loop_misc_count, _loop_mem_count);
    } else {
	_res_ii = TI_RES_COUNT_Min_II(_loop_res_count);
    }
}

void
SIMD_MODEL::Print(FILE *fp)
{
    fprintf(fp, "Setup scalar op count: %d\n", _setup_scalar_count);
    fprintf(fp, "Loop mem op count: %d\n", _loop_mem_count);
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	fprintf(fp, "Loop misc op count: %d\n", _loop_misc_count);
    } else {
	fprintf(fp, "Loop resource count:\n");
	TI_RES_COUNT_Print(fp, _loop_res_count);

	fprintf(fp, "\nSetup resource count:\n");
	TI_RES_COUNT_Print(fp, _setup_res_count);

	fprintf(fp, "Resource II: %d\n", _res_ii);
    }
    fprintf(fp, "Original resource count: %f\n", _orig_count);
}

bool
SIMD_MODEL::Beneficial()
{
    if (Simd_Target == SIMD_TARG_AT_ANALYSIS) {
	return (_model_vl*_orig_count>=Min_II());
    } else {
	return (Cur_Simd_Loop->V_Unroll_Factor()*_orig_count >=
		Min_II());
    }
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
