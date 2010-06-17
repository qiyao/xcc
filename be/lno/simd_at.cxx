
// simd_at.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD Auto Tie module:                                                     *
 *                                                                           *
 *    Functions for SIMD_AT                                                  *
 *                                                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_at.cxx $

#include <stdio.h>
#include "at_trace.h"
#include "cxx_memory.h"
#include "intrn_info.h"
#include "opt_du.h"
#include "pu_info.h"
#include "reduc.h"
#include "simd.h"
#include "simd_at.h"
#include "simd_imem.h"
#include "simd_if.h"
#include "simd_loop_at.h"
#include "simd_select.h"
#include "snl_utils.h"
#include "srcpos.h"
#include "tie.h"
#include "tietypes.h"
#include "wn_util.h"
#include "wutil.h"

static SIMD_LOOP *AT_SIMD_Loop = NULL;


static INT
Common_Refs_Count (IMEM_GROUP *ig, ARRAY_REF *arl)
{
  INT count = 0;
  for (INT elem = 0; elem < ig->Elem_Count(); elem++) {

    IMEM_ELEM *ie = ig->Elem(elem);

    for (INT ofs = 0; ofs < ie->Offset_Count(); ofs++) {

      IMEM_OFFSET *io = ie->Offset(ofs);
      IMEM_INFO *ii = io->Imem_Info();
      EXPR_Stack &lod_sto = ii->Lod_Sto();
      SYMBOL *ii_base = ii->Array_Base();
      
      for (INT lsi = 0; lsi < lod_sto.Elements(); lsi++) {

	WN *ls_wn = lod_sto.Bottom_nth(lsi);
	WN *ls_addr = WN_kid(ls_wn, OPERATOR_is_load(WN_operator(ls_wn)) ? 0 : 1);
	
	for (INT arli = 0; arli < arl->Elements(); arli++) {
	  ARRAY_REF_LIST *arll = arl->Array_Ref_List(arli);

	  if (*ii_base != *arll->Base_Array) {
	    continue;
	  }

	  ARRAY_REF_ITER iter(arll);
	  for (ARRAY_REF_NODE *node = iter.First(); node; node = iter.Next()) {
	    WN *arl_addr = node->Wn;
	    if (arl_addr == ls_addr) {
	      count++;
	    }
	  }
	}
      }
    }
  }

  return count;
}


/*---------------------------------------------------------------------------*
 * Constructor: set AT_PU and initialize SIMD_At_Pool                        *
 *---------------------------------------------------------------------------*/
SIMD_AT::SIMD_AT (MEM_POOL *pool, INT max_width) :
    _pool(pool), _max_width(max_width), _analyzed(false),
    _inner_loop(NULL), _dfg(NULL), _simd_loop(NULL)
{
  _trace = Get_Trace(TP_TEMP, 0x4000);
  
  _at_pool = AT_Libauto_Pool();
  _at_pu = (AT_PU *) PU_Info_autotie_ptr(Current_PU_Info);
  _factory = CXX_NEW(AT_FACTORY(AT_pool(), &(AT_Pu()->Op_Tab())), Pool());
  _simd_select = CXX_NEW(SIMD_SELECT(/* vl */ 0,
				     /* bits_per_idx */ 0,
				     /* elems_per_way */ 1),
			 Pool());
  Set_Cur_At_Simd_Loop();
  Clear_Messages();
}

void
SIMD_AT::Clear_Messages (void)
{
  _messages = CXX_NEW(MESSAGES(Pool()), Pool());
}


void
SIMD_AT::Add_Message (AT_MESSAGE *message)
{
  _messages->AddElement(message);  
}


AT_REGION *
SIMD_AT::Last_Region (void)
{
  AT_PU *at_pu = AT_Pu();
  INT    last  = at_pu->Region_Count() - 1;
  Is_True(last >= 0, ("last region number < 0"));
  
  AT_REGION *region = at_pu->Get_Region(last);
  return region;
}


/* Request an operator from Auto TIE. If setup is true, request a
   setup operation, otherwise request an inner loop operation. */
void
SIMD_AT::Request_Op(WN *wn, bool setup, AT_OP_ID at_op_id, INT cnt)
{
  
  if (_trace) {
    fprintf(TFile, "\nSIMD_AT: vl %d, %s %s at_op %d (cnt %d unroll/scale %d)",
	    _cur_vl, Pre_Model() ? "pre-model" : "request",
	    setup ? "setup" : "inner-loop", at_op_id, cnt, _cur_unroll_scale);
    if (wn) {
      fprintf(TFile, ", line %d\n",
	      (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
      fdump_tree(TFile, wn);
    } else {
      fprintf(TFile, "\n");
    }
  }
  
  if (Simd_Loop()->Is_Noop_Cvt(wn)) {
    if (_trace) {
      fprintf(TFile, "SIMD_AT: Ignoring no-op CVT.\n");
    }
    return;
  }
  
  if (!AT_Pu()->Op_Tab().Is_Valid_Op(at_op_id)) {
    Set_Bad_Oper_Msg(AT_MSG_SIMD_ANALYSIS_OP, wn, SIMD_OPCODE_Msg(wn));
    return;
  }
  
  if (Pre_Model())
    return;
  
  AT_TY_TAB &ty_tab = AT_Pu()->Ty_Tab();
  AT_OP *op = AT_Pu()->Op_Tab().Get_Op(at_op_id);
  Is_True(op != NULL,("Can't find at_op_id %d",at_op_id));
  
  if (op->Is_Atop() && op->Atop() == ATOP_CVT) {
    Is_True(op->Param_Count() == 2, ("ATOP_CVT needs 2 arguments, not %d",
				     op->Param_Count()));
    AT_TY *ty0 = ty_tab.Get_Type(op->Get_Param(0)->Ty_Id());
    AT_TY *ty1 = ty_tab.Get_Type(op->Get_Param(1)->Ty_Id());
    if ((*ty0 == *ty1) || ty0->Complement(ty1)) {
      if (_trace) {
	fprintf(TFile,"SIMD_AT: Ignoring no-op CVT.\n");
      }
      
      return;
    }
  }
  
  cnt *= _cur_unroll_scale;
  if (setup) {
    Stat()->Inc_Setup_Op_Count(at_op_id, cnt);
  } else {
    Stat()->Inc_Loop_Op_Count(at_op_id, cnt);
  }
}


/*---------------------------------------------------------------------------*
 * Create a new region for an original loop                                  *
 *---------------------------------------------------------------------------*/
void 
SIMD_AT::Add_Orig_Loop(WN *wn, INVAR_TABLE *invar_table, INT loop)
{
    Is_True(!Analyzed(),("Loop already analyzed for Auto TIE"));
    
    Reset_Bad_Operator();
    Factory()->set_default_vl(1);
    _start_vl = 0;
    
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_DO_LOOP, ("SIMD_AT::Add_Orig_Loop: not a loop"));
    
    char *name = XT_New(AT_pool()) char[strlen(AT_Pu()->Name()) + strlen("_L") + 64];
    sprintf(name,"%s_L%d",AT_Pu()->Name(),AT_Pu()->Region_Count());
    
    /* create a new region */
    AT_REGION *region = XT_New(AT_pool()) AT_REGION(AT_REGION_KIND_LOOP,name);

    /* Attach line number and source file to region. */
    SRCPOS pos = LWN_Get_Linenum(wn);
    region->Set_Linenum(Srcpos_To_Line(pos));
    region->Set_Filename(SIMD_Filename(pos, true));

    AT_Pu()->Add_Region(region);
    
    WN *loop_info = WN_do_loop_info(wn);
    Is_True(loop_info != NULL, ("Null do loop info"));
    WN_loop_at_region_id(loop_info) = region->Id();
    
    // Mark the current loop as analyzed.
    Set_Analyzed();
}


/*---------------------------------------------------------------------------*
 * Check loop invariant expression                                           *
 *---------------------------------------------------------------------------*/
static 
bool SIMD_At_Is_Loop_Invariant(WN *wn, SIMD_LOOP *simd_loop)
{
    Is_True(simd_loop && simd_loop->Invar_Table(), ("Null invar table"));
    return (!simd_loop->Varies_With_Loop(wn, simd_loop->Simd_Loop_Level()));
}


static 
bool SIMD_At_Is_Loop_Invariant(WN *wn, INVAR_TABLE *invar_table, INT loop)
{
    Is_True(invar_table, ("No invariant table"));
    if (WN_operator(wn) == OPR_INTCONST) {
	return true;
    }
    BIT_VECTOR *bv = invar_table->Find(wn);
    return (bv && bv->Test(loop));
}


/*---------------------------------------------------------------------------*
 * Create a new region for scalar loop                                       *
 *---------------------------------------------------------------------------*/
void
SIMD_AT::Add_Orig_Loop_Op(WN *wn, INVAR_TABLE *invar_table, INT loop)
{
    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Add_Orig_Loop_Op(kid, invar_table, loop);
	}
	return;
    } else if (oper == OPR_DO_LOOP) {
	/* TODO: build some structure for nested loop */
	Add_Orig_Loop_Op(WN_do_body(wn), invar_table, loop);
	return;
    }
    
    if (SIMD_At_Is_Loop_Invariant(wn, invar_table, loop)) {
	return;
    }
    
    if (oper == OPR_LDID) {
	return;
    } 

    if (oper == OPR_STID || oper == OPR_ISTORE) {
	/* enter the RHS first */
	Add_Orig_Loop_Op(WN_kid0(wn), invar_table, loop);
	if (oper == OPR_STID) {
	    return;
	}
    } else if (!OPERATOR_is_load(oper)) {
	/* enter the kids first */
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    Add_Orig_Loop_Op(WN_kid(wn, i), invar_table, loop);
	}
	if (oper == OPR_PARM || oper == OPR_OUTPART) {
	    return;
	}
    }
    
    /* enter the current WN */
    AT_OP_ID at_op_id = Factory()->get_at_op_id(wn);
    Request_Op(wn, /* setup */ false, at_op_id, 1);
}

void 
SIMD_AT::Add_Address_Overhead(WN *wn)
{
    SIMD_LOOP *simd_loop = Simd_Loop();

    OPERATOR oper = WN_operator(wn);
    if (SIMD_At_Is_Loop_Invariant(wn, simd_loop)) {
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
    AT_OP_ID at_op_id = Factory()->get_at_op_id(wn);
    Request_Op(wn, /* setup */ true, at_op_id, 1);
}

/*--------------------------------------------------------------------------*
 * Add unaligned load/store counts                                          *
 *--------------------------------------------------------------------------*/ 
void
SIMD_AT::Add_Alignment_Overhead(WN *wn)
{
    SIMD_LOOP *simd_loop = Simd_Loop();

    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_ILOAD || OPR_ISTORE, 
	    ("SIMD_AT::Add_Simd_Loop_Unaligned: expect iload/istore"));

#if 0
    // we don't really need to add this overhead for now because:
    // - setup operations are not used for cycle estimation yet;
    // - address overhead doesn't request any operations that otherwise
    //   wouldn't be in the configuration.

    /* add prime address overhead */
    WN *old_addr = (oper == OPR_ISTORE) ? WN_kid1(wn) : WN_kid0(wn);
    WN *addr = WN_COPY_Tree(old_addr);
    Simd_Copy_Invariant(old_addr, addr, simd_loop->Invar_Table());

    OPCODE  opc    = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
    WN     *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, 8);
    addr           = WN_CreateExp2(opc, addr, inc_wn);
    Simd_Copy_Invariant_Top_Level(addr, old_addr, simd_loop->Invar_Table());

    Factory()->default_wn_decode_func();
    Factory()->set_default_vl(1);
    Add_Address_Overhead(addr, simd_loop);
    Factory()->set_default_vl(simd_loop->V_Unroll_Factor());
    Factory()->set_wn_decode_func(&AT_WN_Decode_SIMD_EINFO);
#endif
    
    /* add prime load/store */
    AT_OP_ID at_op_id = Factory()->get_at_op_id_prime(wn);
    Request_Op(wn, /* setup */ true, at_op_id, 1);
    
    /* add final store overhead */
    if (oper == OPR_ISTORE) {
	at_op_id = Factory()->get_at_op_id_prime(wn, /* store_flush */ true);
	Request_Op(wn, /* setup */ true, at_op_id, 1);
    }

    /* Emit an info message for the unaligned load/store requirement. */
    if (!Pre_Model()) {
      SIMD_Msg((oper == OPR_ISTORE) ? AT_MSG_STAT_UNALIGNED_STORE : AT_MSG_STAT_UNALIGNED_LOAD,
               wn, Factory()->default_vl());
    }
}


void
SIMD_AT::Add_Simd_Field_Select_By_Table(bool setup,
					WN *wn, TYPE_ID scalar_type,
					INT num_fields, bool is_read)
{
    SIMD_SELECT *simd_sel = Simd_Select();
    VSEL_IMEMS &vsel_table = (is_read) ? 
	Simd_Loop()->IMem_Map().Vsel_R_Table() :
	Simd_Loop()->IMem_Map().Vsel_W_Table();
    INT vl = Factory()->default_vl();

    VSEL_OP *vsel_ops = Find_Vsel_Op(vsel_table, num_fields, vl);
    FmtAssert(vsel_ops, ("SIMD_AT::Add_Simd_Field_Select_By_Table: entry not in the table"));    
    
    while (vsel_ops->input_0 != -1) {
	INT64 sel = simd_sel->Sel_Array_To_Imm(vsel_ops->sel_0);
	AT_OP_ID at_op_id = 
	    Factory()->get_at_op_id_vselect(false /*dual*/, 
					    scalar_type, sel, 
					    simd_sel->Sel_Imm_Size());
	Request_Op(wn, setup, at_op_id, 1);
	vsel_ops++;
    }
}

/*---------------------------------------------------------------------------*
 * Add read select operation counts for a simd loop                          *
 *---------------------------------------------------------------------------*/
void
SIMD_AT::Add_Simd_Field_Select(bool setup,
			       WN *wn, TYPE_ID scalar_type, INT num_fields,
			       bool pairwise, bool is_read)
{
    if (pairwise) {
	if (LNO_IS_POWER_OF_TWO(num_fields)) {
	    SIMD_SELECT *simd_sel = Simd_Select();
	    INT64 sel = simd_sel->Dsel_73625140();
	    AT_OP_ID at_op_id = Factory()->get_at_op_id_vselect(/* dual */ true, scalar_type,
								sel, simd_sel->Dsel_Imm_Size());
	    
	    // compute the number of selects that we need in order to perform
	    // the recursive extraction of elements
	    INT sel_count_per_level = num_fields / 2;
	    INT vl = Factory()->default_vl();
	    INT levels = 0;
	    for (INT nf = vl; nf > 1; nf /= 2) {
		levels++;
	    }
	    INT sel_count = sel_count_per_level * levels;
	    Request_Op(wn, setup, at_op_id, sel_count);
	} else {
	    Is_True(!is_read, ("SIMD_AT::Add_Simd_Field_Select: "
			       "Unexpected no-power-of-2 fields"));
	    Add_Simd_Field_Select_By_Table(setup, wn, scalar_type, num_fields,
					   false);
	}
    } else {
	if (num_fields == 2) {
	    SIMD_SELECT *simd_sel = Simd_Select();
	    INT64 sel = simd_sel->Dsel_75316420();
	    AT_OP_ID at_op_id = Factory()->get_at_op_id_vselect(/* dual */ true, scalar_type,
								sel, simd_sel->Dsel_Imm_Size());
	    Request_Op(wn, setup, at_op_id, 1);
	} else {
	    Is_True(is_read, ("SIMD_AT::Add_Simd_Field_Select: unexpected field count"));
	    Add_Simd_Field_Select_By_Table(setup, wn, scalar_type, num_fields,
					   true);
	}
    }
}


/*---------------------------------------------------------------------------*
 * Add read select operation counts for a simd loop                          *
 *---------------------------------------------------------------------------*/
void
SIMD_AT::Add_Simd_Read_Select(bool setup, WN *wn, IMEM_ELEM *ie)
{
    INT num_fields = ie->Offset_Count();
    INT reg_length = _cur_vl;
    TYPE_ID scalar_type = ie->Parent_Group()->Scalar_Type();
    bool pairwise = (num_fields > 2) && (LNO_IS_POWER_OF_TWO(num_fields));
    Add_Simd_Field_Select(setup, wn, scalar_type, num_fields, pairwise, true);
}


/*---------------------------------------------------------------------------*
 * Add write select operation counts for a simd loop                         *
 *---------------------------------------------------------------------------*/
void
SIMD_AT::Add_Simd_Write_Select(bool setup, WN *wn, IMEM_ELEM *ie)
{
    INT num_fields = ie->Offset_Count();
    INT reg_length = _cur_vl;
    TYPE_ID scalar_type = ie->Parent_Group()->Scalar_Type();
    bool pairwise = true;
    /* write use the same select as read, only the input are organized
       differently, see simd_imem.cxx */
    Add_Simd_Field_Select(setup, wn, scalar_type, num_fields, pairwise, false);
}


void
SIMD_AT::Add_Simd_Load_Shift_In (bool setup, IMEM_GROUP *ig)
{
    Is_True(ig->Elem_Count() == 1,
	    ("Expected a single element in the group"));
    
    IMEM_ELEM *ie = ig->Elem(0);
    WN *load = ig->Loads()->Get(0);
    INT vl = ig->AT_Vl();
    
    SIMD_SELECT *simd_sel = Simd_Select();
    // request shift in select (vector length minus 1)
    INT64 shift_in_sel = simd_sel->Sel_4321();
    AT_OP_ID at_op_id = Factory()->get_at_op_id_vselect(/* dual */ false, ig->Scalar_Type(),
							shift_in_sel, simd_sel->Sel_Imm_Size());
    Request_Op(load, setup, at_op_id, ie->Offset_Count() * (vl - 1));
    
    // request an initial, non-updating load scalar
    at_op_id = Factory()->get_at_op_id_load(load,
					    /* scalar_mem */ true,
					    /* indexed */ false,
					    /* updating */ false);
    Request_Op(load, setup, at_op_id, ie->Offset_Count());
    
    if (vl > 2)
    {
	// request vector length minus two updating indexed loads scalar
	at_op_id = Factory()->get_at_op_id_load(load,
						/* scalar_mem */ true,
						/* indexed */ true,
						/* updating */ true);
	Request_Op(load, setup, at_op_id, ie->Offset_Count() * (vl - 2));
    }
    
    // request a non-updating indexed load scalar for the final load
    at_op_id = Factory()->get_at_op_id_load(load,
					    /* scalar_mem */ true,
					    /* indexed */ true,
					    /* updating */ false);
    Request_Op(load, setup, at_op_id, ie->Offset_Count());
    
    return;
}

void
SIMD_AT::Add_Simd_Indexed_Load_Store(bool setup, IMEM_GROUP *ig, WN *ls,
				     bool is_load)
{
    WN *index_val = ig->Index_Value();
    bool is_vector = ig->Is_Vector();
    AT_OP_ID at_op_id = AT_OP_ID_UNKNOWN;
    int imm_offset =  
	Factory()->default_vl() * MTYPE_byte_size(ig->Scalar_Type());
    Is_True(imm_offset > 0, 
	    ("SIMD_AT::Add_Simd_Indexed_Load_Store: vector size is 0"));

    if (is_load) {
	if (WN_operator(index_val) == OPR_INTCONST) {
	    /* normal immediate updating */
          at_op_id = 
            Factory()->get_at_op_id_load(ls, !is_vector, false, true, 
                                         Factory()->default_vl() * WN_const_val(index_val));
	} else {
	    at_op_id = 
		Factory()->get_at_op_id_load(ls, !is_vector, true, true);
	}

	// request a leading updating load
	Request_Op(ls, setup, at_op_id, 1);
    
	for (INT i = 0; i < ig->Elem_Count(); i++) {
	    IMEM_ELEM *ie = ig->Elem(i);
	    INT count = ie->Offset_Count();

	    if (count > 1) {
		/* normal updating load */
		at_op_id = Factory()->get_at_op_id_load(ls, !is_vector, false, 
							true, imm_offset);
		Request_Op(ls, setup, at_op_id, count-1);
		
		// add deinterleave overhead
		if (count > 1) {
		    Add_Simd_Read_Select(setup, ls, ie);
		}
	    }
	    
	    // add load reuse overhead
	    Add_Simd_Load_Reuse_Shift(setup, ie);
	}
    } else {
	if (WN_operator(index_val) == OPR_INTCONST) {
          /* normal immediate updating */
          at_op_id =
            Factory()->get_at_op_id_store(ls, false, true, 
                                          Factory()->default_vl() * WN_const_val(index_val));
	} else {
	    at_op_id = Factory()->get_at_op_id_store(ls, true, true);
	}
	
	// request a leading updating store 
	Request_Op(ls, setup, at_op_id, 1);

	for (INT i = 0; i < ig->Elem_Count(); i++) {
	    IMEM_ELEM *ie = ig->Elem(i);
	    INT count = ie->Offset_Count();
	    if (count > 1) {
		at_op_id = Factory()->get_at_op_id_store(ls, false, true,
							 imm_offset);
		/* normal updating store */
		Request_Op(ls, setup, at_op_id, count-1);
		
		// add interleave overhead
		Add_Simd_Write_Select(setup, ls, ie);
	    }
	}
    }

    return;
}

void
SIMD_AT::Add_Simd_Load_Reuse_Shift(bool setup, IMEM_ELEM *ie)
{
    IMEM_GROUP *ig         = ie->Parent_Group();
    WN         *load       = ig->Loads()->Get(0);
    INT         vl         = ig->AT_Vl();
    IMEM_INFO  *first_imem = ig->First_Imem_Info();
    IMEM_INFO  *cur_imem   = ie->Offset(0)->Imem_Info();

    INT idiff;
    cur_imem->Compare_Imem_Index(first_imem, &idiff);

    Is_True(idiff>=0, ("SIMD_AT::Add_Simd_Load_Reuse_Shift: negative distance"));

    INT  offset       = (idiff * ie->Offset_Count()) % vl;
    if (offset != 0) {
	Is_True(vl > offset, ("SIMD_AT::Add_Simd_Load_Reuse_Shift: offset too big"));
	SIMD_SELECT *simd_sel = Simd_Select();
	INT64 shift_sel = simd_sel->Sel_4321(offset);
	AT_OP_ID at_op_id = Factory()->get_at_op_id_vselect(/* dual */ false, 
							    ig->Scalar_Type(),
							    shift_sel, 
							    simd_sel->Sel_Imm_Size());
	Request_Op(load, setup, at_op_id, ie->Offset_Count());
    }
}

void
SIMD_AT::Request_Kid_Conversion (bool setup, WN *wn)
{
    // operation canonicalization: request necessary converts for the kids
    for (INT i = 0; i < WN_kid_count(wn); i++) {
	WN *kid = WN_kid(wn, i);
	TYPE_ID kid_type = AT_WN_Decode_SIMD_EINFO(kid).rtype;
	TYPE_ID new_kid_type =
	    AT_FACTORY::canonical_kid_type(wn, i, &AT_WN_Decode_SIMD_EINFO);
	if (new_kid_type != kid_type) {
	    AT_TY_ID at_ty_id = Factory()->get_at_ty_id(new_kid_type,
							Factory()->default_vl(),
							AT_TY::NONE);
	    AT_TY_ID kid_ty_id = Factory()->get_at_ty_id(kid_type,
							 Factory()->default_vl(),
							 AT_TY::NONE);
	    AT_OP_ID cvt_op_id = Factory()->get_at_op_id_convert(kid_ty_id, at_ty_id);
	    Request_Op(kid, setup, cvt_op_id, 1);
	}
    }
}

/*---------------------------------------------------------------------------*
 * Split group according to vector length 'vl'                               *
 *---------------------------------------------------------------------------*/
void 
SIMD_AT::Split_Group(IMEM_GROUP *ig, IMEM_GROUP_Array *ig_array, int vl)
{
    IMEM_INFO  *first_imem = ig->First_Imem_Info();
    IMEM_GROUP *cur_ig = CXX_NEW(IMEM_GROUP(ig, Pool()), Pool());
    cur_ig->Add_Elem(ig->Elem(0));
    ig_array->AddElement(cur_ig);
    
    for (INT i = 1; i < ig->Elem_Count(); i++) {
	IMEM_ELEM *ie       = ig->Elem(i);
	IMEM_INFO *cur_imem = ie->Offset(0)->Imem_Info();

	INT        idiff;
	cur_imem->Compare_Imem_Index(first_imem, &idiff);
	Is_True(idiff>=0, ("SIMD_AT::Split_Group: negative distance"));
	
	INT  offset = idiff;
	if (vl <= offset) {
	    // start a new group
	    cur_ig->Collect_Load_Store();
	    cur_ig = CXX_NEW(IMEM_GROUP(ig, Pool()), Pool());
	    ig_array->AddElement(cur_ig);
	    first_imem = cur_imem;
	} 
	cur_ig->Add_Elem(ie);
    }
    cur_ig->Collect_Load_Store();
}

/*---------------------------------------------------------------------------*
 * Because 'old_ig' still contains all the IMEM_ELEMs here, we only need to  *
 * reset the parent pointer of IMEM_ELEMs.                                   *
 *---------------------------------------------------------------------------*/
void 
SIMD_AT::Remerge_Group(IMEM_GROUP *old_ig, IMEM_GROUP_Array *ig_array)
{
    for (INT i = 0; i < ig_array->Elements(); i++) {
	IMEM_GROUP *ig = ig_array->Get(i);
	for (INT j = 0; j < ig->Elem_Count(); j++) {
	    IMEM_ELEM *ie = ig->Elem(j);
	    ie->Set_Parent_Group(old_ig);
	}
    }
}

/*---------------------------------------------------------------------------*
 * Test whether an updating indexed load/store shares its address            *
 *---------------------------------------------------------------------------*/
bool
SIMD_AT::Index_Address_Updated(WN *wn, WN *ls_wn, IMEM_GROUP *ig) 
{
    if (wn == ls_wn || !ig->Is_Def() || !ig->Is_Use()) return false;
    return (ig->Elem_Count() == 1 && ig->Elem(0)->Offset_Count() == 1);
}


INT
SIMD_AT::Get_Imem_Group_Unroll_Scale (IMEM_GROUP *ig)
{
  if (Pre_Model())
    return _cur_unroll_scale;

  INT orig_ls = Common_Refs_Count(ig, _orig_arl);
  if (orig_ls == 0)
    return _cur_unroll_scale;

  INT unroll_ls = Common_Refs_Count(ig, _unroll_arl);
  if (unroll_ls == 0 || unroll_ls < orig_ls)
    return _cur_unroll_scale;

  return (unroll_ls + orig_ls - 1) / orig_ls;
}


bool
SIMD_AT::Add_Simd_Load_Store (WN *wn, SIMD_EINFO *e_info, bool setup)
{
  if (!e_info)
    return false;

  IMEM_INFO *imem = e_info->IMem();
  if (!imem)
    return false;
  
  Is_True(imem->Imem_Offset() && imem->Imem_Offset()->Parent_Group(),
	  ("No imem group info."));
  
  IMEM_GROUP *ig = imem->Imem_Offset()->Parent_Group();
  INT vl = _cur_vl;
  
  /* Check if already processed for the current stat. */
  if (ig->AT_Vl() == vl) {
    return true;
  }

  ig->Set_AT_Vl(vl);
  _cur_unroll_scale = Get_Imem_Group_Unroll_Scale(ig);
  
  if (ig->Is_Vector()) {
    if (ig->Is_Use()) {
      /* Load */
      if (ig->First_Imem_Info()->Has_Reuse()) {
	// Request vsel.
	SIMD_SELECT *simd_sel = Simd_Select();
	INT64 shift_sel = simd_sel->Sel_4321();
	AT_OP_ID at_op_id = 
	  Factory()->get_at_op_id_vselect(/* dual */ false,
					  e_info->Res_Type(),
					  shift_sel,
					  simd_sel->Sel_Imm_Size());
	Request_Op(wn, setup, at_op_id, 1);
	
	// Request a LS
	at_op_id = Factory()->get_at_op_id_load(wn, 
						/* scalar_mem */ true,
						/* indexed */ false,
						/* updating */ true);
	Request_Op(wn, setup, at_op_id, 1);
      }
      
      {
	IMEM_GROUP *old_ig = ig;
	IMEM_GROUP_Array *ig_array = 
	  CXX_NEW(IMEM_GROUP_Array(Pool()), Pool());
	
	if (ig->Elem_Count() > 1) {
	  Split_Group(ig, ig_array, vl);
	} else {
	  ig_array->AddElement(ig);
	}
	
	for (INT j = 0; j < ig_array->Elements(); j++) {
	  IMEM_GROUP *ig = ig_array->Get(j);		
	  // any load in the group would do
	  WN *load = ig->Loads()->Get(0);
	  AT_OP_ID at_op_id = AT_OP_ID_UNKNOWN;
	  if (!ig->Is_Aligned()) {
	    // each unaligned load group uses one alignment register
	    AT_TY_ID align_ty_id = Factory()->get_at_ty_id(ig->Scalar_Type(),
							   vl, AT_TY::ALIGN);
	    if (!Pre_Model()) {
	      Stat()->Inc_Reg_Count(align_ty_id, /* count */ 1);
	    }
	    
	    /* enter alignment load overhead */
	    Add_Alignment_Overhead(load);
	    
	    /* enter alignment load */
	    at_op_id = Factory()->get_at_op_id_alignment(load, /* updating */ true);
	  } else if (ig->Variable_Stride()) {
	    Add_Simd_Load_Shift_In(setup, ig);
	    continue;
	  } else if (ig->Index_Value() && 
		     !Index_Address_Updated(wn, load, ig)) {
	    /* updating indexed load */
	    Add_Simd_Indexed_Load_Store(setup, ig, load, true);
	    continue;
	  } else {
	    at_op_id = Factory()->get_at_op_id(load);
	  }
	  
	  // A load for each offset in an element in the group.
	  INT load_count = ig->Get_Max_Offset_Count();
	  Request_Op(load, setup, at_op_id, load_count);
	  
	  for (INT i = 0; i < ig->Elem_Count(); i++) {
	    IMEM_ELEM *ie = ig->Elem(i);
	    
	    // add deinterleave overhead
	    if (ig->Elem(i)->Offset_Count() > 1) {
	      Add_Simd_Read_Select(setup, load, ie);
	    }
	    
	    // add load reuse overhead
	    Add_Simd_Load_Reuse_Shift(setup, ie);
	  }
	}
	if (old_ig->Elem_Count() > 1) {
	  /* Reset the IMEM_ELEMs back to the original IMEM_GROUP. We
	     need to do that whever we call Split_Group above because
	     it always assigns the elements to new groups. */
	  Remerge_Group(old_ig, ig_array);
	}
      }
    }
    
    // store
    if (ig->Is_Def()) {
      // any store in the group would do
      WN *store = ig->Stores()->Get(0);
      AT_OP_ID at_op_id = AT_OP_ID_UNKNOWN;
      if (!ig->Is_Aligned()) {
	// each unaligned store group uses one alignment register
	AT_TY_ID align_ty_id = Factory()->get_at_ty_id(ig->Scalar_Type(),
						       vl, AT_TY::ALIGN);
	if (!Pre_Model()) {
	  Stat()->Inc_Reg_Count(align_ty_id, /* count */ 1);
	}
	
	/* enter alignment store overhead */
	Add_Alignment_Overhead(store);
	
	/* enter alignment store */
	at_op_id = Factory()->get_at_op_id_alignment(store, /* updating */ true);
      } else if (ig->Index_Value() &&
		 !Index_Address_Updated(wn, store, ig)) {
	/* updating indexed store */
	Add_Simd_Indexed_Load_Store(setup, ig, store, false);
	return true;
      } else {
	at_op_id = Factory()->get_at_op_id(store);
      }
      
      for (INT i = 0; i < ig->Elem_Count(); i++) {
	IMEM_ELEM *ie = ig->Elem(i);
	INT count = ie->Offset_Count();
	
	// a store for each offset in the group
	Request_Op(store, setup, at_op_id, count);
	
	// add interleave overhead
	if (ig->Elem(i)->Offset_Count() > 1) {
	  Add_Simd_Write_Select(setup, store, ie);
	}
      }
    }
    return true;
  }

  if (imem->Is_Reduction()) {
    /* request reduction */
    if (red_manager->Which_Reduction(e_info->Expr()) == RED_ADD) {
      TYPE_ID  rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
      AT_OP_ID radd_op_id = Factory()->get_at_op_id_radd(rtype);
      AT_OP_ID zero_op_id = Factory()->get_at_op_id_zero(rtype);
      Request_Op(wn, /* setup */ true, radd_op_id, 1);
      Request_Op(wn, /* setup */ true, zero_op_id, 1);
    } else {
      Set_Bad_Oper_Msg(AT_MSG_SIMD_ANALYSIS_REDUCTION, LWN_Get_Parent(wn),
                       SIMD_OPCODE_Msg(LWN_Get_Parent(wn)));
    }
    
    return true;
  }

  return false;
}


void
SIMD_AT::Add_Simd_Reduction (WN *wn)
{
  SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
  Is_True(e_info, ("Null e_info"));

  if (!e_info->Reduction())
    return;
  
  /* Request reduction. */
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
  WN *def_loop = def_list->Loop_stmt();
  if (def_loop != NULL && Is_Descendent(Simd_Loop()->Simd_Loop(), def_loop)) {
    REDUCTION_TYPE red = red_manager->Which_Reduction(wn);
    
    switch (red)
    {
    case RED_ADD:
    {
      TYPE_ID  rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
      AT_OP_ID radd_op_id = Factory()->get_at_op_id_radd(rtype);
      AT_OP_ID zero_op_id = Factory()->get_at_op_id_zero(rtype);
      Request_Op(wn, /* setup */ true, radd_op_id, 1);
      Request_Op(wn, /* setup */ true, zero_op_id, 1);
    }
    break;
    
    case RED_MIN:
    case RED_MAX:
    {
      TYPE_ID  rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
      
      AT_OP_ID prime_op_id = AT_OP_ID_UNKNOWN;
      
      /* Prime min/max using the initial value. */
      if (ST_class(WN_st(wn)) != CLASS_PREG) {
	// request a LS (converting load scalar)
	prime_op_id = Factory()->get_at_op_id_load(wn, /* scalar_mem */ true);
      } else {
	// request a register to register CVT
	TYPE_ID  rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
	AT_TY_ID from_ty_id = Factory()->get_at_ty_id(rtype, 1, AT_TY::NONE);
	AT_TY_ID to_ty_id = Factory()->get_at_ty_id(rtype, Factory()->default_vl(),
						    AT_TY::NONE);
	prime_op_id = Factory()->get_at_op_id_convert(from_ty_id, to_ty_id);
      }
      
      AT_OP_ID red_op_id = Factory()->get_at_op_id_rminmax(rtype, red == RED_MIN);
      
      Request_Op(wn, /* setup */ true, prime_op_id, 1);
      Request_Op(wn, /* setup */ true, red_op_id, 1);
    }
    break;
    
    default:
      Is_True(0, ("Unexpected reduction kind."));
      break;
    }
  }
}


/*---------------------------------------------------------------------------*
 * Add operation counts for a simd loop                                      *
 *---------------------------------------------------------------------------*/
void
SIMD_AT::Add_Simd_Loop_Op(WN *wn, WN *enclosing_loop)
{
  SIMD_LOOP *simd_loop = Simd_Loop();
  
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
    Is_True(if_ana, ("SIMD_AT::Add_Simd_Loop_Op: No SIMD_IF_ANA"));
    INT cmov_cnt = if_ana->CMove_Count();
    Is_True(cmov_cnt > 0, 
	    ("SIMD_AT::Add_Simd_Loop_Op: cmov count is zero"));
    
    Add_Simd_Loop_Op(WN_if_test(wn), enclosing_loop);
    Add_Simd_Loop_Op(WN_then(wn), enclosing_loop);
    Add_Simd_Loop_Op(WN_else(wn), enclosing_loop);
    
    if_ana->Request_AT_CMov(this);
    return;
  }
  
  SIMD_EINFO *e_info = simd_loop->Get_E_Info(wn);
  
  if (SIMD_At_Is_Loop_Invariant(wn, simd_loop)) {
    AT_OP_ID at_op_id = AT_OP_ID_INVALID;
    TYPE_ID rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
    if (oper == OPR_ILOAD || oper == OPR_LDID || 
        rtype == MTYPE_I8 || rtype == MTYPE_U8) {
      /* Request a LS (converting load scalar). We request LS for
         I8/U8 scalar to vector conversion because TIE supports only
         pointer int64/uint64 types. */
      TYPE_ID desc = OPERATOR_is_load(oper) ? AT_WN_Decode_SIMD_EINFO(wn).desc : rtype;
      if (desc == MTYPE_I4) {
	INT size = simd_loop->Get_Max_Def_Bits(wn);
	if (size == 16) {
	    desc = MTYPE_I2;
	} else if (size == 8) { 
	    desc = MTYPE_I1;
	}
      } else if (desc == MTYPE_U4) {
	INT size = simd_loop->Get_Max_Def_Bits(wn);
	if (size == 16) {
	    desc = MTYPE_U2;
	} else if (size == 8) { 
	    desc = MTYPE_U1;
        }
      }
      at_op_id = Factory()->get_at_op_id_load(rtype, desc, /* scalar_mem */ true);
    } else if (oper == OPR_PARM) {
        Add_Simd_Loop_Op(WN_kid0(wn), enclosing_loop);
	return;
    } else {
      // request a register to register CVT
      AT_TY_ID from_ty_id = Factory()->get_at_ty_id(rtype, 1, AT_TY::NONE);
      AT_TY_ID to_ty_id = Factory()->get_at_ty_id(rtype, Factory()->default_vl(),
						  AT_TY::NONE);
      at_op_id = Factory()->get_at_op_id_convert(from_ty_id, to_ty_id);
    }
    Request_Op(wn, /* setup, if invariant all the way */  
	       simd_loop->Invariant_In_Simd_Loop(wn), at_op_id, 1);
    return;
  }
  
  if (oper == OPR_LDID) {
    /* Request reduction priming and finalization ops. */
    Add_Simd_Reduction(wn);
    return;
  }
  
  if (e_info != NULL && e_info->Mula()) {
    // request MADD/MSUB
    Is_True(((oper == OPR_ADD || oper == OPR_SUB) &&
	     (WN_operator(WN_kid1(wn)) == OPR_MPY)), ("Bad MULA expression."));
    WN *mpy_kid = WN_kid1(wn);
    Add_Simd_Loop_Op(WN_kid0(wn), enclosing_loop);
    Add_Simd_Loop_Op(WN_kid0(mpy_kid), enclosing_loop);
    Add_Simd_Loop_Op(WN_kid1(mpy_kid), enclosing_loop);
    AT_OP_ID at_op_id = Factory()->get_at_op_id_mac(wn);
    Request_Op(wn, setup, at_op_id, 1);
    return;
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
	Add_Simd_Loop_Op(WN_kid(wn, i), enclosing_loop);
      }
    }
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
	  ("SIMD_AT::Add_Simd_Loop_Op: NULL EINFO"));
  
  bool is_ls = Add_Simd_Load_Store(wn, e_info, setup);
  
  _cur_unroll_scale = _outer_unroll;

  if (!is_ls) {
    /* enter the current WN */
    AT_OP_ID at_op_id = Factory()->get_at_op_id(wn);
    Request_Op(wn, setup, at_op_id, 1);
  }
}


SIMD_LOOP *
SIMD_AT::Scan_Simd_Loop (SIMD_LOOP *simd_loop)
{
  /*
    Generally, we want to repeat in this routine all steps that would
    take place if we actually went ahead and transformed the loop.
  */

  if (Pre_Model())
  {
    Is_True(Cur_Simd_Loop == simd_loop, ("Unexpected simd loop."));

    /* Create a new SIMD_LOOP. Loop invariant hoisting may delete nodes, so
       some of the data structures need to be reset and reinitilized through
       a loop body scan.
       
       FIXME: Most of these steps need to be carefully reordered and combined
       into a common routine to be used by this code and the real transformation
       code (SNL_Simd_Loop). */
    SIMD_LOOP *new_simd_loop =
      SIMD_LOOP_New(simd_loop->Loop_Model(),
                    simd_loop->Simd_Loop(),
                    simd_loop->Simd_Loop_Level());
    
    simd_loop = new_simd_loop;
    Cur_Simd_Loop = new_simd_loop;

    INVAR_TABLE *inv_table = CXX_NEW(INVAR_TABLE(255, Pool()), Pool());
    SIMD_Move_Invariant(simd_loop->Simd_Loop(), inv_table);
    simd_loop->Set_Invar_Table(inv_table);

    simd_loop->Screen_Operator_Compute_Size(simd_loop->Simd_Loop(), NULL);
    FmtAssert(!simd_loop->Bad_Operator(),
              ("Unhandled operator in SIMD XPRES analysis stage."));
    
    if (!simd_loop->IMem_Map().Setup_Access_Properties(false))
      Is_True(0, ("Can't handle array accesses at XPRES analysis stage."));

    /* We need to run the scalar dependence test because it initialize
       the scalar liveness info.
       ... This code really needs to be reorganized, and tests must be separate
       from initialization. */
    if (!simd_loop->Test_Scalar_Dependence_Ok())
      Is_True(0, ("Can't handle scalar accesses at XPRES analysis stage."));

    simd_loop->IMem_Map().Order_Specific_Setup(simd_loop->Loop_Model(),
                                               simd_loop->Num_Loops(),
                                               simd_loop, false, -1, false);
    Is_True(!simd_loop->Bad_Stride(),
            ("Bad write stream in XPRES analysis stage."));
    
    simd_loop->IMem_Map().Group();
  }
  
  SIMD_LOOP_AT *simd_loop_at = dynamic_cast<SIMD_LOOP_AT *>(simd_loop);
  Set_Simd_Loop(simd_loop);
  Set_Cur_At_Simd_Loop(simd_loop);
  Factory()->set_wn_decode_func(&AT_WN_Decode_SIMD_EINFO);
  
  Set_Inner_Loop(simd_loop->Loop_Model()->Model_Inner_Loop());
  Is_True(Inner_Loop() != NULL, ("No inner loop in the loop model!"));
  
  AT_REGION *region = Last_Region();

  WN *loop_wn = simd_loop->Simd_Loop();
  
  if (Pre_Model()) {
    Init_Vector_Length_Range();

    simd_loop->Setup_Type_Conversion(WN_do_body(loop_wn));
    
    /* We need to set the current SIMD loop again after type conversion because
       it resets it. */
    Set_Cur_At_Simd_Loop(simd_loop);
    
    simd_loop->Simd_Preprocess_Transform(loop_wn);
    simd_loop->IMem_Map().Setup_Index_Value();
  }
  
  // Initialize the if-conversion map used by the DFG builder.
  WN_WN_MAP *if_map = CXX_NEW(WN_WN_MAP(101, Pool()), Pool());
  Set_If_Map(if_map);
  
  // Compute the necessary select bits per index for the starting vector length.
  INT sel_bits = LNO_Min_Pow_Of_Two_Exp(2 * _start_vl);
  bool build_dfg = !Pre_Model();

  if (Pre_Model()) {
    Set_Stat(NULL);
  }
  
  Reset_Bad_Operator();
  simd_loop->IMem_Map().Reset_AT_Vl();
  
  for (INT vl = _start_vl; vl <= _end_vl; vl <<= 1, sel_bits++) {
    _cur_vl = vl;
    
    if (!Pre_Model()) {
      AT_STAT *at_st  = XT_New(AT_pool()) AT_STAT(vl);
      at_st->Set_Unroll(_outer_unroll);
      at_st->Set_SWP(TRUE);	// instruct auto cycle estimator to treat this stat
      				// as SWP'ed stat
      Set_Stat(at_st);
    }
    
    simd_loop->Set_V_Unroll_Factor(vl);
    Factory()->set_default_vl(vl);
    Simd_Select()->Set_Vector_Length(vl);
    Simd_Select()->Set_Bits_Per_Index(sel_bits);
    
    /* Update the alignment information. */
    simd_loop->IMem_Map().Setup_Access_Properties(/* before_transform */ false);
    
    /* Fill in the AT_STAT. */
    Add_Simd_Loop_Op(WN_do_body(loop_wn), loop_wn);
    if (Bad_Operator() || Simd_Select()->Overflow()) {
      /* Break assuming that we won't be able to vectorize for any higher
         vector lengths. */
      _end_vl = vl / 2;
      Simd_Select()->Set_Overflow(false);
      break;
    }
    
    if (build_dfg) {
      /* Build the data-flow graph for the inner loop. */
      Build_DFG();
      Stat()->Set_DFG(DFG());
      
      /* The assumption is that if we fail building the DFG for a particular
	 vector length, then we would also fail doing it for higher factors. */
      if (DFG() == TF_DFG_INVALID) {
	build_dfg = false;
      }
    }

    if (!Pre_Model()) {
      if (vl == _start_vl) {
	INT int8_guards = 0;
	INT int16_guards = 0;
	simd_loop_at->Find_Guard_Bits(loop_wn, int8_guards, int16_guards);
	region->Set_Int8_Guards((UINT)int8_guards);
	region->Set_Int16_Guards((UINT)int16_guards);
      }
    
      region->Add_Stat(Stat());
    }
  }

  if (Pre_Model()) {
    /* Success or failure? */
    if (Bad_Operator()) {
      SIMD_Msg(AT_MSG_SIMD_LOOP_NON_VECTORIZABLE, simd_loop->Simd_Loop());
    } else {
      SIMD_Msg(AT_MSG_SIMD_LOOP_VECTORIZABLE, simd_loop->Simd_Loop());
    }
  }
  
  Set_Cur_At_Simd_Loop();
  Factory()->set_default_vl(1);
  Factory()->default_wn_decode_func();
  _cur_vl = 0;

  /* Delete any computed index value trees after the final scan. */
  if (!Pre_Model()) {
    simd_loop->IMem_Map().Delete_Index_Value();
  }

  /* remove the copies in if_map */
  HASH_TABLE_ITER<WN*, WN*> htit(_if_map);
  WN *if_wn, *copy_block;
  while (htit.Step(&if_wn, &copy_block)) {
    LWN_Delete_Tree(copy_block);
  }

  return simd_loop;
}


/*---------------------------------------------------------------------------*
 * Create a new region for a simd loop                                       *
 *---------------------------------------------------------------------------*/
void 
SIMD_AT::Add_Simd_Loop (SIMD_LOOP *simd_loop)
{
  Reset_Pre_Model();
  Setup_Outer_Unroll();
  Scan_Simd_Loop(simd_loop);
}


SIMD_LOOP *
SIMD_AT::Pre_Model_Simd_Loop (SIMD_LOOP *simd_loop)
{
  Set_Pre_Model();
  _outer_unroll = _cur_unroll_scale = 1;
  simd_loop = Scan_Simd_Loop(simd_loop);
  return simd_loop;
}


void
SIMD_AT::Init_Vector_Length_Range (void)
{
  _start_vl = 2;
  _end_vl = Max_Width();
  INT64 est_iters = Simd_Loop()->Max_Iteration_Count();
  if (est_iters > 0 && est_iters < _end_vl) {
    _end_vl = (INT)est_iters;
  }
  
  /* Round down to the nearest power-of-two value. */
  while (_end_vl > 2 && !LNO_IS_POWER_OF_TWO(_end_vl)) {
    _end_vl &= _end_vl - 1;
  }
}
  

void
SIMD_AT::Annotate_Vector_Symbol_Tree(SYMBOL_TREE_NODE *symbol_node,
				     LNO_REGCLASS_INFO *regclass_info) {
    if (symbol_node == NULL) {
	return;
    }
    
    TYPE_ID type = symbol_node->Symbol().Type;
    AT_TY_ID vec_ty_id = Factory()->get_at_ty_id(type, Factory()->default_vl(),
						 AT_TY::NONE);
    LNO_REGS_IDX vec_idx = regclass_info->Index_Of_At_Ty_Id(vec_ty_id);
    if (vec_idx == LNO_REGS_IDX_UNDEFINED) {
	DevWarn("No regclass index for %s (SIMD_REG, ty_id %d). "
		"Very bad register estimation.",
		MTYPE_name(type), vec_ty_id);
    } else {
	symbol_node->Set_Vec_Idx(vec_idx);
    }
    
    Annotate_Vector_Symbol_Tree(symbol_node->Left(), regclass_info);
    Annotate_Vector_Symbol_Tree(symbol_node->Right(), regclass_info);
}


void
SIMD_AT::Annotate_Vector_Info(ARRAY_REF *ar, SYMBOL_TREE *symbol_tree,
			      SIMD_LOOP *simd_loop)
{
    MEM_POOL_Popper popper(Pool());
    
    Is_True(ar, ("Null ARRAY_REF"));
    Is_True(Simd_Info->AT_Analysis_Phase(),
	    ("Non-AT analysis should use a different vector annotation function."));
    
    if (_trace) {
	fprintf(TFile,"SIMD_AT: Annotating vector information for the loop model\n");
    }
    
    LNO_REGCLASS_INFO *regclass_info = simd_loop->Loop_Model()->Regclass_Info();
    Is_True(regclass_info, ("Null LNO register class info"));
    
    // Use the minimum vector length for register estimation in the loop model.
    // later, we create the appropriate type for each vector length.
    if (_start_vl < 2)
	return;
    Factory()->set_default_vl(_start_vl);
    
    for (INT i = 0; i < ar->Elements(); i++) {
	ARRAY_REF_LIST *ref_list = ar->Array_Ref_List(i);

	bool is_se = ref_list->Is_Scalar_Expanded();
	
	ARRAY_REF_ITER iter(ref_list);
	for (ARRAY_REF_NODE *node = iter.First(); node; node = iter.Next()) {
	  bool is_vec = false;
	  bool need_align = false;
	  TYPE_ID type = node->Type();

	  if (is_se) {
	    is_vec = true;
	  } else {
	    Is_True(node->Wn, ("Null wn"));
	    WN* wn = LWN_Get_Parent(node->Wn);
	    OPERATOR oper = WN_operator(wn);
	    IMEM_INFO *ii = NULL;
	    if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	      ii = simd_loop->IMem_Map().Find(wn);
	    }
	    
	    if (ii && ii->Is_Vector()) {
	      type = ii->Scalar_Type();
	      is_vec = true;
	      if (!ii->Is_Aligned() && ii->Is_First_Imem_In_Group()) {
		need_align = true;
	      }
	    } else if (!OPERATOR_is_store(oper)) {
	      // use the invariant table for array references and expressions
	      // that participate in vector expressions
	      WN *parent = LWN_Get_Parent(wn);
	      Is_True(parent, ("Null parent of a load"));
	      OPERATOR parent_oper = WN_operator(parent);
	      if (parent_oper == OPR_SHL ||
		  parent_oper == OPR_ASHR ||
		  parent_oper == OPR_LSHR) {
		continue;
	      }
	      
	      // it's vector only if it's inside the SIMD loop and the parent
	      // is not SIMD loop invariant.
	      if (!SIMD_At_Is_Loop_Invariant(parent, simd_loop) &&
		  simd_loop->Get_E_Info(parent) != NULL)
	      {
		is_vec = true;
	      }
	    }
	  }
	  
	  if (is_vec) {
	    AT_TY_ID vec_ty_id = Factory()->get_at_ty_id(type, Factory()->default_vl(),
							 AT_TY::NONE);
	    LNO_REGS_IDX vec_idx = regclass_info->Index_Of_At_Ty_Id(vec_ty_id);
	    if (vec_idx == LNO_REGS_IDX_UNDEFINED) {
	      DevWarn("No regclass index for %s (SIMD_REG, ty_id %d). "
		      "Very bad register estimation.",
		      MTYPE_name(type), vec_ty_id);
	      continue;
	    }
	    
	    node->Set_Vec(vec_idx);
	    node->Set_Vec_Regs(1);
	    
	    if (need_align) {
	      AT_TY_ID vec_ty_id = Factory()->get_at_ty_id(type, Factory()->default_vl(),
							   AT_TY::ALIGN);
	      LNO_REGS_IDX vec_idx = regclass_info->Index_Of_At_Ty_Id(vec_ty_id);
	      if (vec_idx == LNO_REGS_IDX_UNDEFINED) {
		DevWarn("No regclass index for %s (SIMD_ALIGN_REG, ty_id %d). "
			"Very bad register estimation.",
			MTYPE_name(type), vec_ty_id);
		continue;
	      }
	      
	      node->Set_Align(vec_idx);
	    }
	  }
	}
    }
    
    // annotate the symbol references
    Annotate_Vector_Symbol_Tree(symbol_tree->Symbol_Node(), regclass_info);
    
    Factory()->set_default_vl(1);
}


void
SIMD_AT::Set_Reg_Usage (AT_REGION *region, AT_TY *ty, int count, bool vector_type)
{
  // Set the count in all stats.
  for (INT stat_idx = 0; stat_idx < region->Stat_Count(); stat_idx++) {
    AT_STAT *stat = region->Get_Stat(stat_idx);
    
    // create a new vector type with the appropriate vector length
    // if necessary.
    AT_TY *temp_ty = ty;
    if (ty->Vector_Length() > 1 || vector_type)
    {
      INT vl = stat->SIMD_Width();
      Is_True(vl > 1, ("Expected non-scalar STAT at index %d\n", stat_idx));
      
      temp_ty = CXX_NEW(AT_TY(ty), Pool());
      temp_ty->Set_Vector_Length(vl);
    }
    
    temp_ty = Ty_Tab()->Find_Type(temp_ty);
    INT count_delta = count;
    if (temp_ty)
    {
	if (temp_ty->Align()) {
	    /* alignment is counted in both unrolled arl in model.cxx
	       and Add_Simd_Loop_Op, remove the duplicates */
	    INT old_count = 0;
	    stat->Reg_Count().find(temp_ty->Id(), old_count);
	    count_delta -= old_count;
	    if (count_delta < 0) {
		count_delta = 0;
	    }
	}
	stat->Inc_Reg_Count(temp_ty->Id(), count_delta);
    }
  }
}


void
SIMD_AT::Output_Register_Usage(LNO_REGS *regs)
{
    MEM_POOL_Popper popper(Pool());

    AT_REGION *region = Last_Region();
    
    AT_TY_TAB *ty_tab = Ty_Tab();
    const LNO_REGCLASS_INFO *regclass_info = regs->Regclass_Info();
    LNO_REGS *regs_cpy = CXX_NEW(LNO_REGS(regs, Pool()), Pool());

    // If the starting vector length is less than 2, no vector analysis was performed.
    if (_start_vl < 2)
	return;
    
    for (AT_TY_ID ty_id = AT_TY_ID_FIRST; ty_id <= ty_tab->Last_Ty_Id(); ty_id++) {
	AT_TY *ty = ty_tab->Get_Type(ty_id);
	
	// The register estimation was done with minimum vector length,
	// so ignore any non-scalar types with different vector lengths.
	if (ty->Vector_Length() != 1 && ty->Vector_Length() != _start_vl)
	{
	    continue;
	}
	
	LNO_REGS_IDX idx = regclass_info->Index_Of_At_Ty_Id(ty_id);
	if (idx == LNO_REGS_IDX_UNDEFINED)
	{
	    continue;
	}
	
	INT cnt = regs_cpy->Count_Of_Idx(idx);
	if (cnt >= 0)
	{
	  // mark the register class as visited
	  regs_cpy->Count_Of_Idx(idx) = -1;
	  Set_Reg_Usage(region, ty, cnt, false);
	}
    }

    AT_TY_INT_MAP *fld_map = CXX_NEW(AT_TY_INT_MAP(101, Pool()), Pool());

    INT num_groups = Simd_Loop()->IMem_Map().Groups().Elements();
    for (INT i = 0; i < num_groups; i++) {
      IMEM_GROUP *ig = Simd_Loop()->IMem_Map().Groups()[i];
      
      AT_TY_ID ty_id = Factory()->get_at_ty_id(ig->Scalar_Type(), 1, AT_TY::NONE);
      AT_TY *ty = ty_tab->Get_Type(ty_id);
      
      INT reg_count = 0;
      INT reuse = ig->First_Imem_Info()->Has_Reuse() ? 1 : 0;
      
      INT num_elems = ig->Elem_Count();
      for (INT j = 0; j < num_elems; j++) {
	IMEM_ELEM *ie = ig->Elem(j);
	INT num_offsets = ie->Offset_Count();
	reg_count += num_offsets - 1; /* field selection */
	reg_count += num_offsets * reuse; /* coefficient reuse */
	if (j > 0)
	  reg_count += 2 * num_offsets; /* element reuse */
      }

      if (reg_count == 0)
        continue;

      /* Update the register usage based on the field selection only once
         per type using the highest number of registers that any
         group/elem/offset needs. */
      
      AT_TY *hash_ty = ty;
      if (MTYPE_is_integral(ig->Scalar_Type())) {
        TYPE_ID hash_mtype = Mtype_TransferSign(MTYPE_I4, ig->Scalar_Type());
        AT_TY_ID hash_ty_id = Factory()->get_at_ty_id(hash_mtype, 1, AT_TY::NONE);
        hash_ty = ty_tab->Get_Type(hash_ty_id);
      }
      
      int old_reg_count = fld_map->Find(hash_ty);
      if (reg_count <= old_reg_count)
        continue;
      
      Set_Reg_Usage(region, ty, reg_count - old_reg_count, true);
      fld_map->Remove(hash_ty);
      fld_map->Enter(hash_ty, reg_count);
    }
}


void
SIMD_AT::Finalize_Region (void)
{
  AT_REGION *region = Last_Region();
  for (int i = 0; i < Message_Count(); i++) {
    AT_MESSAGE *msg = Get_Message(i);
    
    AT_MESSAGE *vec_msg = NULL;
    if (msg->Id() == AT_MSG_STAT_UNALIGNED_LOAD ||
        msg->Id() == AT_MSG_STAT_UNALIGNED_STORE) {
      vec_msg = region->Messages()->Find(AT_MSG_SIMD_LOOP_VECTORIZABLE);
    }
    
    if (vec_msg)
      region->Messages()->Add_Before(vec_msg, msg);
    else
      region->Messages()->Append(msg);
  }
  
  Clear_Messages();
}


void
SIMD_AT::Set_Bad_Oper_Msg (const AT_MSG_ID msg_id, const WN *wn, ...)
{
  if (!Bad_Operator()) {
    va_list vargs;
    va_start (vargs, wn);
    SIMD_Msgv(msg_id, wn, vargs);
    va_end(vargs);

    Set_Bad_Operator();
  }
}


void
SIMD_AT::Setup_Outer_Unroll (void)
{
  /* Return the total unroll outside the SIMD loop. */
  Is_True(Simd_Loop() && Simd_Loop()->Simd_Loop_Level() >= 0,
	  ("No SIMD loop"));
  
  LOOP_MODEL *model = Simd_Loop()->Loop_Model();
  Is_True(model, ("No loop model"));

  _orig_arl = CXX_NEW(ARRAY_REF(model->Array_Ref(), Pool()), Pool());
  _unroll_arl = CXX_NEW(ARRAY_REF(model->Array_Ref(), Pool()), Pool());

  INT prod = 1;
  INT simd_new_order = model->Old_To_New_Order(Simd_Loop()->Simd_Loop_Level());
  for (INT i = 0; i < model->Num_Loops(); i++) {
    INT no = model->Old_To_New_Order(i);
    if (no < simd_new_order) {
      prod *= model->Block_Number(i);
    }

    _unroll_arl->Unroll(i, model->Block_Number(i));
  }
  
  _outer_unroll = prod;
  _cur_unroll_scale = _outer_unroll;
  
  INT new_inner = model->New_Order(model->Num_Loops() - 1);
  _orig_arl->Remove_Cse(new_inner, /* max_dist */ 0, /* step */ 0);
  _unroll_arl->Remove_Cse(new_inner, /* max_dist */ 0, /* step */ 0);
  
  if (_trace) {
    fprintf(TFile, "\nSIMD_AT: Original ARL:\n");
    _orig_arl->Print(TFile);
    fprintf(TFile, "\nSIMD_AT: Unrolled ARL:\n");
    _unroll_arl->Print(TFile);
    fprintf(TFile, "\nSIMD_AT: Outer unroll product = %d\n", _outer_unroll);
  }
}


static const char *
wn_opnd_name (XT_MEMPOOL *pool, WN *wn)
{
    if (!WN_has_sym(wn))
	return NULL;
    
    ST *st = WN_st(wn);
    if (!st)
	return NULL;

    char *name = ST_name(st);
    if (!name)
	return NULL;
    
    return xt_pool_strdup(pool, name);
}


tf_node_t
SIMD_AT::Build_DFG_Const (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_INTCONST,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));

    // Create a constant node for integer constants.
    TYPE_ID rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
    Is_True(MTYPE_is_integral(rtype),
	    ("Expected integer type, not %s", MTYPE_name(rtype)));
    
    INT64 val = WN_const_val(wn);
    
    // Some special handling of OPR_DIV: we convert it to LSHR
    // whenever possible.
    WN *parent = LWN_Get_Parent(wn);
    Is_True(parent, ("Null parent"));
    
    INT vl = Factory()->default_vl();
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    if (!e_info)
      vl = 1;
    
    OPERATOR parent_oper = WN_operator(parent);
    if (parent_oper == OPR_DIV && LWN_Kid_Index(parent, wn) == 1) {
      Is_True(val >= 0 && LNO_IS_POWER_OF_TWO(val),
              ("Expected division by positive power-of-two constant, not %ld",
               val));
      INT sa = 0;
      while ( (val & 0x1) != 0x1) {
        val >>= 1;
        sa++;
      }
      val = sa;
    }

    XT_BIGNUM *at_val = XT_New(AT_pool()) XT_BIGNUM(MTYPE_bit_size(rtype), val);
    tf_node_t node = tfex->dfg_new_node(DFG(), TFN_CONST);
    tfex->node_set_const_val(node, (tf_const_val_t)at_val);
    AT_TY_ID ty_id = Factory()->get_at_ty_id(rtype, vl, AT_TY::NONE);
    AT_TY *ty = Ty_Tab()->Get_Type(ty_id);
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    ty->Element_Bit_Size(),
					    ty->Vector_Length());
    tfex->node_add_output(node, opnd);
    tfex->opnd_set_user(opnd, ty);

    return node;
}

tf_node_t
SIMD_AT::Build_DFG_Default_Rec (WN *wn)
{
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));

    bool is_mac = e_info->Mula();

    AT_OP_ID at_op_id = AT_OP_ID_UNKNOWN;
    if (is_mac) {
	at_op_id = Factory()->get_at_op_id_mac(wn);
    } else {
	at_op_id = Factory()->get_at_op_id(wn);
    }

    if (at_op_id == AT_OP_ID_UNKNOWN) {
	Set_DFG(TF_DFG_INVALID);
	return TF_NODE_INVALID;
    }
    
    AT_OP *op = Factory()->op_tab()->Get_Op(at_op_id);
    
    // Ignore no-op converts -- just propagate its kid node up.
    if (Simd_Loop()->Is_Noop_Cvt(wn) ||
	(op->Is_Atop() && op->Atop() == ATOP_CVT)) {
      Is_True(op->Param_Count() == 2,
	      ("ATOP_CVT needs 2 arguments, not %d", op->Param_Count()));
      AT_TY *ty0 = Factory()->ty_tab()->Get_Type(op->Get_Param(0)->Ty_Id());
      AT_TY *ty1 = Factory()->ty_tab()->Get_Type(op->Get_Param(1)->Ty_Id());
      if ((*ty0 == *ty1) || ty0->Complement(ty1)) {
	return Build_DFG_Rec(WN_kid0(wn));
      }
    }
    
    tf_node_kind_t kind = AT_FACTORY::opr_to_node_kind(WN_operator(wn));
    if (kind == TFN_UNKNOWN || is_mac) {
	kind = TFN_USER;
    }
    
    tf_node_t node = tfex->dfg_new_node(DFG(), kind);
    tfex->node_set_user(node, op);
    tfex->node_set_input_count(node, op->In_Param_Count());
    tfex->node_set_output_count(node, op->Out_Param_Count());
    
    if (tfex->node_output_count(node) > 1) {
	Set_DFG(TF_DFG_INVALID);
	return TF_NODE_INVALID;
    }
    
    // Set input operand types and input edges.
    for (INT iidx = 0; iidx < tfex->node_input_count(node); iidx++) {
	AT_PARAM *param = op->Get_In_Param(iidx);
	Is_True(param != NULL,
		("NULL input param for index %d, op %s", iidx, op->Name()));
	
	AT_TY *pty = param->Type();
	tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
						pty->Element_Bit_Size(),
						pty->Vector_Length());
	tfex->node_set_input(node, iidx, opnd);
	tfex->opnd_set_user(opnd, pty);
	
	// FIXME: this should be more complicated, especially in
	// the presence of TIE ops and inout parameters.
	tf_node_t cnode = TF_NODE_INVALID;
	WN *kid = (iidx < WN_kid_count(wn)) ? WN_kid(wn, iidx) : NULL;
	if (is_mac) {
	    kid = ((iidx == 0) ? WN_kid0(wn) :
		   (iidx == 1) ? WN_kid0(WN_kid1(wn)) : WN_kid1(WN_kid1(wn)));
	}

        if (kid && op->Is_Tie_Proto() && param->Kind() == AT_PARAM_KIND_IMM) {
          Is_True(WN_operator(kid) == OPR_PARM, ("Expected a parm"));
          kid = WN_kid0(kid);
          Is_True(WN_operator(kid) == OPR_INTCONST, ("Expected an immediate INTCONST"));
        }
        
	if (kid) {
	    cnode = Build_DFG_Rec(kid);
	    if (DFG() == TF_DFG_INVALID)
		return TF_NODE_INVALID;
	}
	
	if (cnode) {
	    INT from_operand_idx = 0;
	    if (tfex->node_kind(cnode) == TFN_COPY) {
	      tf_opnd_t opnd = tfex->node_new_operand(cnode, TFO_PACKED,
						      pty->Element_Bit_Size(),
						      pty->Vector_Length());
	      from_operand_idx = tfex->node_add_output(cnode, opnd);
	      tfex->opnd_set_user(opnd, pty);
	    } else {
		Is_True(tfex->node_output_count(cnode) == 1 &&
			tfex->node_output(cnode, 0) != TF_OPND_INVALID,
			("Unexpected kid node output count (%d).",
			 tfex->node_output_count(cnode)));
	    }
	    
	    tfex->dfg_new_data_edge(DFG(),
				    tfex->node_index(cnode), from_operand_idx, 0,
				    tfex->node_index(node), iidx, 0);
	}
    }
    
    // Set output operand types
    for (INT oidx = 0; oidx < tfex->node_output_count(node); oidx++) {
	AT_PARAM *param = op->Get_Out_Param(oidx);
	Is_True(param != NULL,
		("NULL output param at index %d, op %s", oidx, op->Name()));
	AT_TY *pty = param->Type();

	tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
						pty->Element_Bit_Size(),
						pty->Vector_Length());
	tfex->node_set_output(node, oidx, opnd);
	tfex->opnd_set_user(opnd, pty);
    }    

    EInfo_Node_Map()->Enter(e_info, node);
    return node;
}


tf_node_t
SIMD_AT::Build_DFG_Iload (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_ILOAD,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));
    
    IMEM_INFO *imem = e_info->IMem();
    Is_True(imem, ("No IMEM info for ILOAD."));
    Is_True(imem->Imem_Offset(), ("No IMEM grouping info for ILOAD."));
    IMEM_GROUP *ig = imem->Imem_Offset()->Parent_Group();
    
    tf_node_t node = IInfo_Node_Map()->Find(imem);
    if (node != NULL) {
	EInfo_Node_Map()->Enter(e_info, node);
	return node;
    }
    
    // Here we need to load the IMEM_GROUP and initialize the IMEM_INFO to NODE
    // map for each different access in the group.

    tf_node_t load_node = TF_NODE_INVALID;
    
    AT_TY_ID res_ty_id = Factory()->get_at_ty_id(ig->Scalar_Type(),
						 Factory()->default_vl(),
						 AT_TY::NONE);
    Is_True(res_ty_id != AT_TY_ID_UNKNOWN, ("Can't create result type for load"));
    AT_TY *res_ty = Ty_Tab()->Get_Type(res_ty_id);
    
    bool bad_load = (!ig->Is_Vector() || !ig->Is_Aligned() || ig->Variable_Stride() ||
		     ig->Elem_Count() != 1 || ig->Elem(0)->Offset_Count() != 1);

    if (bad_load) {
	// If we can't handle the load completely, generate a COPY node.
	// If necessary, we could mark the DFG as incomplete in such cases.
	
      load_node = tfex->dfg_new_node(DFG(), TFN_COPY);

      tf_opnd_t opnd = tfex->node_new_operand(load_node, TFO_PACKED,
					      res_ty->Element_Bit_Size(),
					      res_ty->Vector_Length());
      tfex->node_add_input(load_node, opnd);
      tfex->opnd_set_user(opnd, res_ty);
      
      opnd = tfex->node_new_operand(load_node, TFO_PACKED,
				    res_ty->Element_Bit_Size(),
				    res_ty->Vector_Length());
      tfex->node_add_output(load_node, opnd);
      tfex->opnd_set_user(opnd, res_ty);
    } else {
	AT_OP_ID at_op_id = Factory()->get_at_op_id(wn);
	if (at_op_id == AT_OP_ID_UNKNOWN) {
	    Set_DFG(TF_DFG_INVALID);
	    return TF_NODE_INVALID;
	}
	
	AT_OP *op = Factory()->op_tab()->Get_Op(at_op_id);
	
	load_node = tfex->dfg_new_node(DFG(), TFN_USER);
	tfex->node_set_user(load_node, op);
	tfex->node_set_input_count(load_node, op->In_Param_Count());
	tfex->node_set_output_count(load_node, op->Out_Param_Count());
	
	for (INT iidx = 0; iidx < tfex->node_input_count(load_node); iidx++) {
	    AT_PARAM *param = op->Get_In_Param(iidx);
	    Is_True(param != NULL,
		    ("NULL input param for index %d, op %s", iidx, op->Name()));
	    AT_TY *pty = param->Type();
	    
	    tf_opnd_t opnd = tfex->node_new_operand(load_node, TFO_PACKED,
						    pty->Element_Bit_Size(),
						    pty->Vector_Length());
	    tfex->node_set_input(load_node, iidx, opnd);
	    tfex->opnd_set_user(opnd, pty);
	}
	
	// We handle only aligned loads for now.
	Is_True(tfex->node_output_count(load_node) == 1,
		("Unexpected load node output count %d", tfex->node_output_count(load_node)));
    
	AT_PARAM *out_param = op->Get_Out_Param(0);
	Is_True(out_param != NULL, ("NULL output param for load node"));
	AT_TY *out_ty = out_param->Type();
	Is_True(out_ty == res_ty, ("Load result types mismatch"));
	
	tf_opnd_t opnd = tfex->node_new_operand(load_node, TFO_PACKED,
						out_ty->Element_Bit_Size(),
						out_ty->Vector_Length());
	tfex->node_set_output(load_node, 0, opnd);
	tfex->opnd_set_user(opnd, out_ty);
    }

    // Extract the loaded data through a COPY node.
    node = tfex->dfg_new_node(DFG(), TFN_COPY);
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    res_ty->Element_Bit_Size(),
					    res_ty->Vector_Length());
    INT to_operand_idx = tfex->node_add_input(node, opnd);
    tfex->opnd_set_user(opnd, res_ty);
    
    tfex->dfg_new_data_edge(DFG(),
			    tfex->node_index(load_node), 0, 0,
			    tfex->node_index(node), to_operand_idx, 0);
    
    EInfo_Node_Map()->Enter(e_info, node);
    IInfo_Node_Map()->Enter(imem, node);
    return node;
}


tf_node_t
SIMD_AT::Build_DFG_Istore_Rec (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_ISTORE,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));
    
    IMEM_INFO *imem = e_info->IMem();
    Is_True(imem, ("No IMEM info for ISTORE."));
    Is_True(imem->Imem_Offset(), ("No IMEM grouping info for ISTORE."));
    IMEM_GROUP *ig = ig = imem->Imem_Offset()->Parent_Group();
	
    // For stores we want to create a COPY node that may feed the last store too.
	
    tf_node_t cnode = Build_DFG_Rec(WN_kid0(wn));
    if (DFG() == TF_DFG_INVALID)
	return TF_NODE_INVALID;
    
    AT_TY_ID res_ty_id = Factory()->get_at_ty_id(ig->Scalar_Type(),
						 Factory()->default_vl(),
						 AT_TY::NONE);
    Is_True(res_ty_id != AT_TY_ID_UNKNOWN, ("Can't create result type for load"));
    AT_TY *res_ty = Ty_Tab()->Get_Type(res_ty_id);
    
    tf_node_t node = tfex->dfg_new_node(DFG(), TFN_COPY);
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    res_ty->Element_Bit_Size(),
					    res_ty->Vector_Length());
    INT to_operand_idx = tfex->node_add_input(node, opnd);
    tfex->opnd_set_user(opnd, res_ty);

    if (cnode) {
	INT from_operand_idx = 0;
	if (tfex->node_kind(cnode) == TFN_COPY) {
	  tf_opnd_t opnd = tfex->node_new_operand(cnode, TFO_PACKED,
						  res_ty->Element_Bit_Size(),
						  res_ty->Vector_Length());
	  from_operand_idx = tfex->node_add_output(cnode, opnd);
	  tfex->opnd_set_user(opnd, res_ty);
	} else {
	    Is_True(tfex->node_output_count(cnode) == 1 &&
		    tfex->node_output(cnode, 0) != TF_OPND_INVALID,
		    ("Unexpected kid node output count (%d).",
		     tfex->node_output_count(cnode)));
	}
	tfex->dfg_new_data_edge(DFG(),
				tfex->node_index(cnode), from_operand_idx, 0,
				tfex->node_index(node), to_operand_idx, 0);
    }
    
    if (IInfo_Node_Map()->Find(imem)) {
	IInfo_Node_Map()->Remove(imem);
    }
    
    IInfo_Node_Map()->Enter(imem, node);
    EInfo_Node_Map()->Enter(e_info, node);

#if 0    
    // Because of if-conversion, we may not get the if-converted store wn
    // in the IMEM_GROUP store list. So for now, always perform stores.
    WN *last_store = ig->Stores()->Get(ig->Stores()->Elements()-1);
#endif
    
    bool bad_store = (!ig->Is_Vector() || !ig->Is_Aligned() || ig->Variable_Stride() ||
		      ig->Elem_Count() != 1 || ig->Elem(0)->Offset_Count() != 1);

#if 0    
    // For bad stores, add a COPY node with unknown output for each occurance.
    if (!bad_store && last_store != wn) {
	return node;
    }
#endif
    
    // Here we need to store the IMEM_GROUP.
    
    tf_opnd_t sopnd = tfex->node_new_operand(node, TFO_PACKED,
					     res_ty->Element_Bit_Size(),
					     res_ty->Vector_Length());
    INT from_operand_idx = tfex->node_add_output(node, sopnd);
    tfex->opnd_set_user(sopnd, res_ty);
    
    tf_node_t store_node = TF_NODE_INVALID;
    
    if (bad_store) {
	// If we can't handle the store completely, generate a COPY node.
	// If necessary, we could mark the DFG as incomplete in such cases.
	
        store_node = tfex->dfg_new_node(DFG(), TFN_COPY);

	tf_opnd_t opnd = tfex->node_new_operand(store_node, TFO_PACKED,
						res_ty->Element_Bit_Size(),
						res_ty->Vector_Length());
	tfex->node_add_input(store_node, opnd);
	tfex->opnd_set_user(opnd, res_ty);
	
	opnd = tfex->node_new_operand(store_node, TFO_PACKED,
				      res_ty->Element_Bit_Size(),
				      res_ty->Vector_Length());
	tfex->node_add_output(store_node, opnd);
	tfex->opnd_set_user(opnd, res_ty);

    } else {
	AT_OP_ID at_op_id = Factory()->get_at_op_id(wn);
	if (at_op_id == AT_OP_ID_UNKNOWN) {
	    Set_DFG(TF_DFG_INVALID);
	    return TF_NODE_INVALID;
	}
	
	AT_OP *op = Factory()->op_tab()->Get_Op(at_op_id);
	
	store_node = tfex->dfg_new_node(DFG(), TFN_USER);
	tfex->node_set_user(store_node, op);
	tfex->node_set_input_count(store_node, op->In_Param_Count());
	
	Is_True(op->Out_Param_Count() == 0,
		("Expected store with no output operands."));
	
	Is_True(tfex->node_input_count(store_node) > 0,
		("Unexpected store node input count %d",
		 tfex->node_input_count(store_node)));
	
	tf_opnd_t opnd = tfex->node_new_operand(store_node, TFO_PACKED,
						res_ty->Element_Bit_Size(),
						res_ty->Vector_Length());
	tfex->node_set_input(store_node, 0, opnd);
	tfex->opnd_set_user(opnd, res_ty);
	
	for (INT iidx = 1; iidx < tfex->node_input_count(store_node); iidx++) {
	    AT_PARAM *param = op->Get_In_Param(iidx);
	    Is_True(param != NULL,
		    ("NULL input param for index %d, op %s", iidx, op->Name()));
	    AT_TY *pty = param->Type();
	    
	    tf_opnd_t opnd = tfex->node_new_operand(store_node, TFO_PACKED,
						    pty->Element_Bit_Size(),
						    pty->Vector_Length());
	    tfex->node_set_input(store_node, iidx, opnd);
	    tfex->opnd_set_user(opnd, pty);
	}
    }
    
    tfex->dfg_new_data_edge(DFG(),
			    tfex->node_index(node), from_operand_idx, 0,
			    tfex->node_index(store_node), 0, 0);
	
    return node;
}

// copy the def uses for all STIDS in the IF to preserve liveness information
static void Copy_Def_Use(WN *orig, WN *copy)
{
  OPERATOR opr1 = WN_operator(orig);
  OPERATOR opr2 = WN_operator(copy);
  Is_True(opr1==opr2, ("Expected identical nodes"));
  if (opr1 == OPR_BLOCK) {
    WN *w1 = WN_first(orig);
    WN *w2 = WN_first(copy);
    while (w1) {
	Copy_Def_Use(w1, w2);
        w1 = WN_next(w1);
        w2 = WN_next(w2);
    }
  } else if (opr1 == OPR_STID) {
	LWN_Copy_Def_Use_For_Def(orig,copy, Du_Mgr);
  } else {
    for (INT kidno = 0; kidno < WN_kid_count(orig); kidno++) {
	Copy_Def_Use(WN_kid(orig,kidno), WN_kid(copy,kidno));
    }
  }
}


tf_node_t
SIMD_AT::Build_DFG_If_Rec (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_IF, ("Expected an if statement"));
    Is_True(LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || SIMD_LOOP::Has_If_Convert_Pragma(wn),
            ("If conversion is not enabled"));
    
    WN *block = If_Map()->Find(wn);
    if (!block) {
	block = WN_CreateBlock();
	LWN_Set_Parent(block,wn);
	WN *if_copy = LWN_Copy_Tree(wn, TRUE, LNO_Info_Map);
	Copy_Def_Use(wn, if_copy);
	LWN_Insert_Block_Before(block, NULL, if_copy);
	
	Simd_Loop()->Copy_EInfo_Rec(wn, if_copy);
        Simd_Copy_Invariant(wn, if_copy, Simd_Loop()->Invar_Table());
	
	/* rename and if-conversion */
	Simd_Loop()->Simd_If_Conv(if_copy);
	
	If_Map()->Enter(wn, block);
    }
    Is_True(block, ("Can't do if conversion"));
    
    for (WN *kid = WN_first(block); kid != NULL; kid = WN_next(kid)) {
	Build_DFG_Rec(kid);
    }

    return TF_NODE_INVALID;
}


tf_node_t
SIMD_AT::Build_DFG_Ldid (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_LDID,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SIMD_LOOP *simd_loop = Simd_Loop();
    bool invariant = SIMD_At_Is_Loop_Invariant(wn, simd_loop);
    
    SIMD_EINFO *e_info = simd_loop->Get_E_Info(wn);
    Is_True(e_info != NULL || invariant, ("Null einfo"));
    
    tf_node_t node = TF_NODE_INVALID;
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = simd_loop->S_Info_Map().Find(symbol);
    if (s_info == NULL) {
	// For shifts, we don't recurse or annotate with e_info or s_info
	// the shift amount expression. Just make sure the LDID is invariant here.
	Is_True(invariant, ("Non-invariant LDID, null SINFO"));
    } else {
	node = SInfo_Node_Map()->Find(s_info);
	if (node != NULL) {
	  Is_True(tfex->node_kind(node) == TFN_COPY,
		  ("Expected COPY node for LDID, not %s",
		   tfex->node_kind_str(tfex->node_kind(node))));
	  if (e_info) {
	    EInfo_Node_Map()->Enter(e_info, node);
	  }
	  return node;
	}
    }
    
    TYPE_ID rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
    AT_TY_ID ty_id = Factory()->get_at_ty_id(rtype,
					     s_info ? Factory()->default_vl() : 1,
					     AT_TY::NONE);
    AT_TY *ty = Factory()->ty_tab()->Get_Type(ty_id);
    
    node = tfex->dfg_new_node(DFG(), TFN_COPY);
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    ty->Element_Bit_Size(),
					    ty->Vector_Length());
    tfex->node_add_input(node, opnd);
    tfex->opnd_set_user(opnd, ty);
    
    if (s_info) {
      SInfo_Node_Map()->Enter(s_info, node);
    }
    if (e_info) {
      EInfo_Node_Map()->Enter(e_info, node);
    }
    
    return node;
}


static bool
Is_Last_Symbol_Def (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_STID,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SYMBOL symbol(wn);
    for (wn = WN_next(wn); wn; wn = WN_next(wn)) {
	if (WN_operator(wn) == OPR_STID) {
	    SYMBOL tsymb(wn);
	    if (symbol == tsymb) {
		return false;
	    }
	}
    }

    return true;
}


tf_node_t
SIMD_AT::Build_DFG_Stid_Rec (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_STID,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SIMD_LOOP *simd_loop = Simd_Loop();
    
    SIMD_EINFO *e_info = simd_loop->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));
    
    tf_node_t cnode = Build_DFG_Rec(WN_kid0(wn));
    if (DFG() == TF_DFG_INVALID)
	return TF_NODE_INVALID;
    
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = simd_loop->S_Info_Map().Find(symbol);
    Is_True(s_info != NULL, ("Null SINFO!"));
    
    TYPE_ID rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
    AT_TY_ID ty_id = Factory()->get_at_ty_id(rtype, Factory()->default_vl(),
					     AT_TY::NONE);
    AT_TY *ty = Factory()->ty_tab()->Get_Type(ty_id);
    
    // Create a copy node and add an edge from the value expression to it.
    
    tf_node_t node = tfex->dfg_new_node(DFG(), TFN_COPY);
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    ty->Element_Bit_Size(),
					    ty->Vector_Length());
    INT to_operand_idx = tfex->node_add_input(node, opnd);
    tfex->opnd_set_user(opnd, ty);

    Is_True(to_operand_idx == 0,
	    ("Expected input index 0, not %d", to_operand_idx));
    
    if (cnode) {
	int from_operand_idx = 0;
	if (tfex->node_kind(cnode) == TFN_COPY) {
	  tf_opnd_t opnd = tfex->node_new_operand(cnode, TFO_PACKED,
						  ty->Element_Bit_Size(),
						  ty->Vector_Length());
	  from_operand_idx = tfex->node_add_output(cnode, opnd);
	  tfex->opnd_set_user(opnd, ty);
	} else {
	    Is_True(tfex->node_output_count(cnode) == 1 &&
		    tfex->node_output(cnode, 0) != TF_OPND_INVALID,
		    ("Unexpected kid node output count (%d).",
		     tfex->node_output_count(cnode)));
	}
	
	tfex->dfg_new_data_edge(DFG(),
				tfex->node_index(cnode), from_operand_idx, 0,
				tfex->node_index(node), to_operand_idx, 0);
    }
    
    // If this is the last definition of a live-out variable, add an unknown output
    // operand to the COPY node holder.
    BOOL live_out = FALSE;
    WN *inner_loop = Inner_Loop(); 
    USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn);
    USE_LIST_ITER iter_use(use_list);
    for (DU_NODE *use_node = iter_use.First(); !iter_use.Is_Empty();
           use_node = (DU_NODE *) iter_use.Next()) {
        WN *use = use_node->Wn();
        Is_True(use, ("Null pointer in USE-DEF chain"));
        if (!Is_Descendent(use, WN_do_body(inner_loop))) {
          live_out = TRUE;
          break;
        }
    }

    if (live_out && Is_Last_Symbol_Def(wn)) {
      tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					      ty->Element_Bit_Size(),
					      ty->Vector_Length());
      tfex->node_add_output(node, opnd);
      tfex->opnd_set_user(opnd, ty);
    }

    if (SInfo_Node_Map()->Find(s_info)) {
	SInfo_Node_Map()->Remove(s_info);
    }
    
    SInfo_Node_Map()->Enter(s_info, node);
    EInfo_Node_Map()->Enter(e_info, node);

    return node;
}


tf_node_t
SIMD_AT::Build_DFG_Parm_Rec (WN *wn)
{
    Is_True(WN_operator(wn) == OPR_PARM,
	    ("Unexpected operator %s", OPERATOR_name(WN_operator(wn))));
    
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));

    tf_node_t node = Build_DFG_Rec(WN_kid0(wn));
    if (DFG() == TF_DFG_INVALID || node == TF_NODE_INVALID)
	return TF_NODE_INVALID;
    
    EInfo_Node_Map()->Enter(e_info, node);
    return node;
}


tf_node_t
SIMD_AT::Build_DFG_Select_Rec (WN *wn)
{
    SIMD_EINFO *e_info = Simd_Loop()->Get_E_Info(wn);
    Is_True(e_info != NULL, ("Null einfo"));
    
    SIMD_IF_CONV *if_conv = Simd_Loop()->If_Conv();
    if (if_conv == NULL) {
	if_conv = Simd_Loop()->If_Conv_Map().Find(wn);
	Simd_Loop()->Set_If_Conv(if_conv);
    }
    Is_True(if_conv, ("SIMD_AT::Build_DFG_Select_Rec: No SIMD_IF_CONV"));

    tf_node_t cond_node = TF_NODE_INVALID;
    if (if_conv->Is_First()) {
	WN *cond = WN_kid0(wn);
	cond_node = Build_DFG_Rec(cond);
	if (DFG() == TF_DFG_INVALID)
	    return TF_NODE_INVALID;

	SIMD_EINFO *cond_einfo = Simd_Loop()->Get_E_Info(cond);
	Is_True(cond_einfo != NULL, ("Null condition einfo"));
	if_conv->Set_Cond_Info(cond_einfo);
	
	// We may need to use the condition node multiple times, so
	// we need to extract its output through a COPY node.
	EInfo_Node_Map()->Remove(cond_einfo);
	Is_True(tfex->node_output_count(cond_node) == 1, ("Unexpected number of outputs"));
	cond_node = tfex->copy_out_reroute(cond_node, 0);
	EInfo_Node_Map()->Enter(cond_einfo, cond_node);
	
	// Get rid of the unknown output operand of the condition. It's not going
	// to be live-out of the loop.
	Is_True(tfex->node_output_count(cond_node) == 1 &&
		tfex->opnd_edge_idx(tfex->node_output(cond_node, 0), 0) == TF_EDGE_IDX_U,
		("Unexpected copy-out node."));
	tfex->node_delete_output(cond_node, 0);
    } else {
	SIMD_EINFO *cond_einfo = if_conv->Cond_Info();
	cond_node = EInfo_Node_Map()->Find(cond_einfo);
    }
    Is_True(cond_node, ("Can't find node for select condition"));
    
    if_conv->Inc_Processed();
    if (if_conv->Finished()) {
	if_conv->Reset_Processed();
	Simd_Loop()->Set_If_Conv(NULL);
    }
    
    TYPE_ID rtype = AT_WN_Decode_SIMD_EINFO(wn).rtype;
    AT_OP_ID at_op_id = Factory()->get_at_op_id_cmov(rtype);
    Is_True(at_op_id != AT_OP_ID_UNKNOWN, ("Can't create an AT_OP"));
    
    AT_OP *op = Factory()->op_tab()->Get_Op(at_op_id);
    Is_True(op->Is_Atop_Op() && op->Atop() == ATOP_MOVT,
	    ("Expected ATOP_MOVT, not %s", op->Name()));

    tf_node_t node = tfex->dfg_new_node(DFG(), TFN_USER);
    tfex->node_set_user(node, op);
    tfex->node_set_input_count(node, op->In_Param_Count());
    tfex->node_set_output_count(node, op->Out_Param_Count());
    
    Is_True(tfex->node_input_count(node) == 3,
	    ("Expected 3 select inputs, not %d", tfex->node_input_count(node)));
    Is_True(tfex->node_output_count(node) == 1,
	    ("Expected 1 select output, not %d", tfex->node_output_count(node)));
    
    // Set input operand types and input edges.
    for (INT iidx = 0; iidx < tfex->node_input_count(node); iidx++) {
	AT_PARAM *param = op->Get_In_Param(iidx);
	Is_True(param != NULL,
		("NULL input param for index %d, op %s", iidx, op->Name()));
	
	AT_TY *pty = param->Type();
	tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
						pty->Element_Bit_Size(),
						pty->Vector_Length());
	tfex->node_set_input(node, iidx, opnd);
	tfex->opnd_set_user(opnd, pty);
	
	tf_node_t cnode = TF_NODE_INVALID;
	if (iidx == 2) {
	    cnode = cond_node;
	} else {
	    WN *kid = WN_kid(wn, 2 - iidx);
	    cnode = Build_DFG_Rec(kid);
	    if (DFG() == TF_DFG_INVALID)
		return TF_NODE_INVALID;
	}
	
	if (cnode) {
	    INT from_operand_idx = 0;
	    if (tfex->node_kind(cnode) == TFN_COPY) {
	      tf_opnd_t opnd = tfex->node_new_operand(cnode, TFO_PACKED,
						      pty->Element_Bit_Size(),
						      pty->Vector_Length());
	      from_operand_idx = tfex->node_add_output(cnode, opnd);
	      tfex->opnd_set_user(opnd, pty);
	    } else {
		Is_True(tfex->node_output_count(cnode) == 1 &&
			tfex->node_output(cnode, 0) != TF_OPND_INVALID,
			("Unexpected kid node output count (%d).",
			 tfex->node_output_count(cnode)));
	    }
	    
	    tfex->dfg_new_data_edge(DFG(),
				    tfex->node_index(cnode), from_operand_idx, 0,
				    tfex->node_index(node), iidx, 0);
	}
    }
    
    // Set output operand
    AT_PARAM *param = op->Get_Out_Param(0);
    Is_True(param != NULL,
	    ("NULL output param at index 0, op %s", op->Name()));
    AT_TY *pty = param->Type();
    tf_opnd_t opnd = tfex->node_new_operand(node, TFO_PACKED,
					    pty->Element_Bit_Size(),
					    pty->Vector_Length());
    tfex->node_set_output(node, 0, opnd);
    tfex->opnd_set_user(opnd, pty);
    
    EInfo_Node_Map()->Enter(e_info, node);
    return node;
}


tf_node_t
SIMD_AT::Build_DFG_Rec (WN *wn)
{
    if (DFG() == TF_DFG_INVALID)
      return TF_NODE_INVALID;
    
    OPERATOR oper = WN_operator(wn);
    Is_True(oper != OPR_DO_LOOP, ("Unexpected loop operator!"));

    switch (oper) {
    case OPR_INTRINSIC_CALL:
    case OPR_OUTPART:
	Set_DFG(TF_DFG_INVALID);
	return TF_NODE_INVALID;
    
    case OPR_BLOCK:
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Build_DFG_Rec(kid);
	}
	return TF_NODE_INVALID;
	
    case OPR_IF:
	return Build_DFG_If_Rec(wn);
	
    case OPR_INTCONST:
	return Build_DFG_Const(wn);
	
    default:
	break;
    }
    
    SIMD_LOOP *simd_loop = Simd_Loop();
    bool invariant = SIMD_At_Is_Loop_Invariant(wn, simd_loop);
    if (invariant && oper != OPR_LDID && oper != OPR_PARM) {
      /* Invariant expressions are held in registers. We return invalid node
	 here so that later we create input operands with no incoming edges. */
      return TF_NODE_INVALID;
    }
    
    Is_True(simd_loop->Get_E_Info(wn) != NULL ||
	    (invariant && oper == OPR_LDID), ("Null EINFO!"));
    
    switch (oper)
    {
    case OPR_PARM:
	return Build_DFG_Parm_Rec(wn);
	
    case OPR_LDID:
	return Build_DFG_Ldid(wn);
	
    case OPR_STID:
	return Build_DFG_Stid_Rec(wn);
	
    case OPR_ILOAD:
	return Build_DFG_Iload(wn);
    
    case OPR_ISTORE:
	return Build_DFG_Istore_Rec(wn);
	
    case OPR_SELECT:
	return Build_DFG_Select_Rec(wn);

    default:
	return Build_DFG_Default_Rec(wn);
    }
    
    return TF_NODE_INVALID;
}

    
void
SIMD_AT::Build_DFG (void)
{
  /* The DFG is constructed only when libfusion is available. */
  if (!tfex) {
    Set_DFG(TF_DFG_INVALID);
    return;
  }
  
  tf_dfg_t dfg = tfex->dfg_init((tf_pool_t)AT_pool());
  Set_DFG(dfg);
  
  EINFO_NODE_MAP *einfo_node_map = CXX_NEW(EINFO_NODE_MAP(101, Pool()), Pool());
  Set_EInfo_Node_Map(einfo_node_map);
  
  SINFO_NODE_MAP *sinfo_node_map = CXX_NEW(SINFO_NODE_MAP(101, Pool()), Pool());
  Set_SInfo_Node_Map(sinfo_node_map);
  
  IINFO_NODE_MAP *iinfo_node_map = CXX_NEW(IINFO_NODE_MAP(101, Pool()), Pool());
  Set_IInfo_Node_Map(iinfo_node_map);
  
  Build_DFG_Rec(WN_do_body(Inner_Loop()));
  
  if (DFG() != TF_DFG_INVALID) {
    tfex->opt_prune_copies(DFG());
    tfex->dfg_compact_nodes_and_edges(DFG());
  }
}


AT_WN_DECODE
AT_WN_Decode_SIMD_EINFO (const WN *wn) {
    Is_True(wn!=NULL,("AT_WN_Decode_SIMD_EINFO: Null WN"));
    AT_WN_DECODE at_decode = { WN_rtype(wn), WN_desc(wn) };
    
    if (AT_SIMD_Loop == NULL) {
	DevWarn("No SIMD_LOOP_AT in AT_WN_Decode_SIMD_EINFO");
	return at_decode;
    }
    
    SIMD_EINFO *einfo = AT_SIMD_Loop->Get_E_Info((WN *)wn);
    if (einfo == NULL) {
	return at_decode;
    }
    
    at_decode.rtype = einfo->Res_Type();
    return at_decode;
}


void
Set_Cur_At_Simd_Loop(SIMD_LOOP *simd_loop) {
  AT_SIMD_Loop = simd_loop;
}



// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
