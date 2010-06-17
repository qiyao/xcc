// simd_loop.cpp
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Automatic SIMD                                                         *
 *                                                                           *
 *    Member functions for SIMD_LOOP.                                        *
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

// $Id: simd.cpp $

#include <stdio.h>
#include "model.h"
#include "simd.h"
#include "simd_imem.h"
#include "simd_select.h"
#include "opt_du.h"
#include "reduc.h"
#include "snl.h"
#include "prompf.h"
#include "lwn_util.h"
#include "wind_down.h"
#include "const.h"
#include "dep.h"
#include "lnoutils.h"
#include "fusion.h"
#include "be_symtab.h"
#include "config_opt.h"
#include "cond.h"
#include "simd_if.h"

// for generating TIE intrinsic
#include "tie.h"
#include "wintrinsic.h"
#include "intrn_info.h"

extern WN* Find_Stmt_Under(WN *stmt, WN *body);
extern ARRAY_DIRECTED_GRAPH16*	Array_Dependence_Graph; 
                                        // Dep graph
extern REDUCTION_MANAGER *red_manager;	// LNO reduction manager
extern DU_MANAGER *Du_Mgr;          	// PU DU manager
extern DLL_SHARED TIE_INFO  *tie_info;             // TIE info pointer
extern WN_MAP     LNO_Info_Map;
extern MEM_POOL   SIMD_local_pool;

/* counter for new PREG names */
INT name_counter = 0;

static void
dump_interleave (const char *msg, SIMD_EINFO *einfo, EINFO_Stack &e_infos)
{
  if (simd_debug) {
    fprintf(TFile, "\n--- %s ---", msg);
    if (einfo)
      fprintf(TFile, " current einfo %p ---", einfo);
    fprintf(TFile, "\n");
    for (INT i = 0; i < e_infos.Elements(); i++) {
      SIMD_EINFO *cur = e_infos.Bottom_nth(i);
      fprintf(TFile, "\nExpression @ %p: \n", cur);
      fdump_tree(TFile, cur->Expr());
      if (cur->Need_Interleave()) {
	fprintf(TFile, "need interleave, ");
      }
      if (cur->Interleaved()) {
	fprintf(TFile, "interleaved, ");
      }
      if (cur->Need_Deinterleave()) {
	fprintf(TFile, "need deinterleave.");
      }
      fprintf(TFile, "\n");
    }
  }
}


/*---------------------------------------------------------------------------*
 * Generate alignment addr symbol                                            *
 *---------------------------------------------------------------------------*/
SIMD_PREG*
Generate_Align_Addr_Reg(SIMD_INFO *simd, MEM_POOL *pool)
{
    char new_name[64];
    sprintf(new_name, "%s_%d", "ali_adr", name_counter++);
    TYPE_ID mtype = MTYPE_U4;
    PREG_NUM preg_num = Create_Preg(mtype, new_name);
    SIMD_PREG *addr_sym = CXX_NEW(SIMD_PREG(mtype, preg_num), pool);
    return addr_sym;
}


// addr is the address of the first vector element to load
// size is the size of the vector element (and power of 2)
//
// Returns a Whirl node that computes the address that an initial
// alignment load should be made from. Subsequent loads should use addresses
// at increments equal to size. The addr node is used in the Whirl expression.
//
// Expression: (addr-1)&NMASK + addr&MASK, where
// 
// MASK = size-1 (only least significant bits are 1)
// NMASK = ~MASK (only least significant bits are 0)

WN *Alignment_Load_Init_Addr (WN *addr, INT size) {
    Is_True((size & (size-1))==0,("Size should be power of 2."));
    
    INT mask = size-1;
    INT nmask = ~mask;
    
    WN *addr_cpy = LWN_Copy_Tree(addr, TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(addr, addr_cpy, Du_Mgr);

    WN *wn_const = WN_CreateIntconst(OPC_I4INTCONST, 1);
    OPCODE opc = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
    WN *wn_left = LWN_CreateExp2(opc, addr_cpy, wn_const);
    
    wn_const = WN_CreateIntconst(OPC_I4INTCONST, nmask);
    opc = OPCODE_make_op(OPR_BAND, MTYPE_I4, MTYPE_V);
    wn_left = LWN_CreateExp2(opc, wn_left, wn_const);
    
    wn_const = WN_CreateIntconst(OPC_I4INTCONST, mask);
    opc = OPCODE_make_op(OPR_BAND, MTYPE_I4, MTYPE_V);
    WN *wn_right = LWN_CreateExp2(opc, addr, wn_const);
    
    opc = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
    WN *init_addr = LWN_CreateExp2(opc, wn_left, wn_right);
    
    return init_addr;
}

/*---------------------------------------------------------------------------*
 * Simd compute bit size:                                                    *
 *   return the size of an expression *wn                                    *
 *---------------------------------------------------------------------------*/
INT
SIMD_LOOP::Compute_Size(WN *wn)
{
    OPERATOR oper   = WN_operator(wn);
    TYPE_ID  rtype  = WN_rtype(wn);
    TYPE_ID  desc   = WN_desc(wn);
    
    int cur_size    = MTYPE_bit_size(rtype);
    int child_size  = 0;
    int child1_size = 0;

    /* Constants */
    if (oper == OPR_INTCONST) {
	INT32 val = WN_const_val(wn);
	if (val < -32768 || val > 32767) {
	    cur_size = 32;
	} else if (val < -128 || val > 127) {
	    cur_size = 16;
	} else {
	    cur_size = Simd_Info->Get_Narrow_Element_Mem_Bits();
	}
    }

    switch (oper) {
	case OPR_LDID:   /* direct load */
	case OPR_ILOAD:  /* indirect load/store, get size from 'desc' */
	    cur_size   = MTYPE_bit_size(desc);
	    break;

	case OPR_ABS:
	case OPR_NEG:
	    cur_size = Compute_Size(WN_kid0(wn));
	    break;

	case OPR_CVT:
	    child_size = Compute_Size(WN_kid0(wn));
	    cur_size   = MIN(cur_size, child_size);
	    break;

	case OPR_CVTL:
	    cur_size   = WN_cvtl_bits(wn);
	    break;
	    
	case OPR_MAX:
	case OPR_MIN: /* maximum of the children's size */
	    cur_size   = Compute_Size(WN_kid0(wn));
	    child_size = Compute_Size(WN_kid1(wn));
	    cur_size   = MAX(cur_size, child_size);
	    break;

	case OPR_ASHR:
	case OPR_LSHR:
	case OPR_DIV:
	    child_size = Compute_Size(WN_kid0(wn));
	    cur_size = MIN(cur_size, child_size);
	    break;

    case OPR_MPY:
      child_size  = Compute_Size(WN_kid0(wn));
      child1_size = Compute_Size(WN_kid1(wn));
      cur_size = MIN(cur_size, 2 * MAX(child_size, child1_size));
      break;

    default:
      break;
    }
    return cur_size;
}

/*---------------------------------------------------------------------------*
 * Return the 'mul' kid if 'wn' is a candidate for reassociation             *
 *     0   : not a candidate for reassociation                               *
 *     0x1 : kid0 is the MUL                                                 *
 *     0x2 : kid1 is the MUL                                                 *
 *     0x3 : both kids are MUL                                               *
 *---------------------------------------------------------------------------*/
INT
SIMD_LOOP::Roundoff_Reassociable(WN *wn)
{
    INT res = 0;
    OPERATOR oper = WN_operator(wn);

    if (oper != OPR_ADD && oper != OPR_SUB) {
	return res;
    }

    WN* kid0 = WN_kid0(wn);
    WN* kid1 = WN_kid1(wn);
    if (Varies_With_Loop(kid0, Simd_Loop_Level()) && 
	(WN_operator(kid1) == OPR_MPY)) {
	res |= 0x2;
    }
    if (Varies_With_Loop(kid1, Simd_Loop_Level()) && 
	(WN_operator(kid0) == OPR_MPY)) {
	res |= 0x1;
    }
    return res;
}

/*---------------------------------------------------------------------------*
 *     +( inv, +/-( *(x, y), z ) ) ===> +/-( +( inv, *(x, y) ), z )  (1)     * 
 *     +( inv, +( z, *(x, y) ) )   ===> +( z, +( inv, *(x, y) ) )    (2)     *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Mulr_Reassociation(WN *kid0, WN *kid1, WN *wn)
{
    if (WN_rtype(kid0) != WN_rtype(wn) || WN_desc(kid0) != WN_desc(wn)) {
	return;
    }

    /* kid0 is loop invariant,  (ldid and constant for now) */
    /* find the mul in kid1 */
    INT mul_cand = Roundoff_Reassociable(kid1);
    
    if ((mul_cand & 0x1) != 0) {
	/* case (1) */
	/* this is messy, but it simplifies the pointer update */
	WN_set_operator(wn, WN_operator(kid1));
	WN_set_operator(kid1, OPR_ADD);
	WN_kid1(wn)   = WN_kid1(kid1);      /* +/-( ..., z)     */
	LWN_Set_Parent(WN_kid1(kid1), wn);  
	
	WN_kid1(kid1) = WN_kid0(kid1);      /* +( ..., *(x, y)) */
	WN_kid0(kid1) = kid0;               /* +( inv, *(x, y)) */
	LWN_Set_Parent(kid0, kid1);
	WN_kid0(wn)   = kid1;               /* +/-( +( inv, *(x, y)), z) */
    } else if ((mul_cand & 0x2)!=0 && WN_operator(kid1) == OPR_ADD){
	/* case (2) */
	WN_kid0(wn)   = WN_kid0(kid1);      /* +( z, +( ..., *(x, y))) */
	LWN_Set_Parent(WN_kid0(kid1), wn);

	WN_kid0(kid1) = kid0;               /* +( z, +( inv, *(x, y))) */
	LWN_Set_Parent(kid0, kid1);
    } 
    
    Is_True(LWN_Check_Parentize(wn), ("Invalid parent pointers"));
    Is_True(LWN_Check_Parentize(kid0), ("Invalid parent pointers"));
    Is_True(LWN_Check_Parentize(kid1), ("Invalid parent pointers"));
}    

bool
SIMD_LOOP::Invariant_In_Simd_Loop(WN *wn)
{
    Is_True(Invar_Table(), ("No invariant table"));
    if (WN_operator(wn) == OPR_INTCONST) {
	/* we sometimes generate constant but not putting them into the
	   invariant table (as in DIV to right shift) */
	return true;
    }
    BIT_VECTOR *bv = Invar_Table()->Find(wn);
    if (!bv || !bv->Pop_Count()) {
	return false;
    }

    for (INT i = Simd_Loop_Level(); i < bv->Size(); i++) {
	if (!bv->Test(i)) {
	    return false;
	}
    }
    return true;
}

bool
SIMD_LOOP::Varies_With_Loop (WN *wn, INT loop)
{
    Is_True(Invar_Table(), ("No invariant table."));
    if (WN_operator(wn) == OPR_INTCONST) {
	/* just in case we encountered compiler generated constant during
	   the transformation */
	return false;
    }
    BIT_VECTOR *bv = Invar_Table()->Find(wn);
    return !bv || !bv->Test(loop);
}

bool
SIMD_LOOP::Has_If_Convert_Pragma (WN *wn)
{
  while (wn != NULL) {
    if (WN_operator(wn) == OPR_DO_LOOP) {
      DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn, TRUE);
      if ((dli != NULL) && dli->SIMD_If_Convert_Directive)
        return true;
    }
      
    wn = Enclosing_Do_Loop(LWN_Get_Parent(wn));
  }

  return false;
}

/*---------------------------------------------------------------------------*
 * Simd reassociate expression:                                              *
 *     Reassociate the expression to uncover MUL16 + offset.                 *
 *                                                                           *
 *     +( inv, +/-( *(x, y), z ) ) ===> +/-( +( inv, *(x, y) ), z )  (1)     * 
 *     +( inv, +( z, *(x, y) ) )   ===> +( z, +( inv, *(x, y) ) )    (2)     *
 *     -( inv, -( z, *(x, y) ) )   ===> -( +( inv, *(x, y) ), z)     (3)     *
 *     -( -( z, *(x, y) ), inv )   ===> -( z, +( inv, *(x, y) ) )    (4)     *
 *                                                                           *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Simd_Reassociate_Mulr(WN *wn)
{
    OPERATOR oper  = WN_operator(wn);
    TYPE_ID  rtype = WN_rtype(wn);
    TYPE_ID  desc  = WN_desc(wn);

    if (!MTYPE_is_integral(rtype) || (oper != OPR_ADD && oper != OPR_SUB)) {
	return;
    }

    WN         *kid0 = WN_kid0(wn);
    WN         *kid1 = WN_kid1(wn);
    
    if (oper == OPR_ADD) {
	if (Invariant_In_Simd_Loop(kid0)) {
	    /* case (1) and (2) */
	    Mulr_Reassociation(kid0, kid1, wn); 
	} else if (Invariant_In_Simd_Loop(kid1)) {
	    /* swap the two kids, then case (1) and (2) */
	    WN_kid0(wn) = kid1;
	    WN_kid1(wn) = kid0;
	    Mulr_Reassociation(kid1, kid0, wn); 
	}
    } else if (WN_operator(kid1) == OPR_SUB && 
	      WN_rtype(kid1) == rtype && WN_desc(kid1) == desc) {
	Is_True(oper == OPR_SUB, ("Neither OPR_ADD nor OPR_SUB"));

	if (Invariant_In_Simd_Loop(kid0)) {
	    INT mul_cand = Roundoff_Reassociable(kid1);
	    if ((mul_cand & 0x2) != 0) {
		/* case (3) */
		WN_kid1(wn) = WN_kid0(kid1); /* -( ..., z) */
		LWN_Set_Parent(WN_kid0(kid1), wn);
		
		WN_set_operator(kid1, OPR_ADD);
		WN_kid0(kid1) = kid0;        /* kid1 = +(inv, *(x, y)) */
		LWN_Set_Parent(kid0, kid1); 
		
		WN_kid0(wn) = kid1;          /* -( kid1, z ) */
	    }
	}
    } else if (WN_operator(kid0) == OPR_SUB &&
	       WN_rtype(kid0) == rtype && WN_desc(kid0) == desc) {
	Is_True(oper == OPR_SUB, ("Neither OPR_ADD nor OPR_SUB"));
	
	if (Invariant_In_Simd_Loop(kid1)) {
	    INT mul_cand = Roundoff_Reassociable(kid0);
	    if ((mul_cand & 0x2) != 0) {
		/* case (3) */
		WN_kid0(wn) = WN_kid0(kid0);  /*  -( z, inv ) */
		LWN_Set_Parent(WN_kid0(kid0), wn);

		WN_set_operator(kid0, OPR_ADD);
		WN_kid0(kid0) = kid1;
		LWN_Set_Parent(kid1, kid0);   /* kid0 = +( inv, *(x, y)) */
		
		WN_kid1(wn) = kid0;           /* -( z, kid0 ) */
	    }
	}
    } 
    Is_True(LWN_Check_Parentize(wn), ("Invalid parent pointers"));
    Is_True(LWN_Check_Parentize(kid0), ("Invalid parent pointers"));
    Is_True(LWN_Check_Parentize(kid1), ("Invalid parent pointers"));
}

// Returns the widest DEF for this USE, or 0 if unable to determine
INT
SIMD_LOOP::Get_Max_Def_Bits(WN *use) {
    INT max_def_bits = 0;
    DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(use);
    if (def_list && !def_list->Incomplete()) {
	DEF_LIST_ITER iter_def(def_list);
	for (DU_NODE* def_node = iter_def.First(); 
	     !iter_def.Is_Empty(); 
	     def_node = (DU_NODE *) iter_def.Next()) {
	    WN* def = def_node->Wn();
	    if (WN_operator(def) == OPR_STID) {
		WN* rhs = WN_kid0(def);
		max_def_bits = MAX(Compute_Size(rhs), max_def_bits);
	    } else {
		max_def_bits=0;
		break;
	    }
	}
    }
    return max_def_bits;
}


void
SIMD_LOOP::Set_Bad_Oper_Msg (AT_MSG_ID msg_id, const WN *wn, ...)
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
SIMD_LOOP::Set_Bad_Oper_Msg (const WN *wn)
{
  Set_Bad_Oper_Msg(Simd_Info->AT_Analysis_Phase() ?
                   AT_MSG_SIMD_ANALYSIS_OP : AT_MSG_SIMD_NO_VECTOR_OP,
                   wn, SIMD_OPCODE_Msg(wn));
}

#define IS_POWER_OF_2(x) (((x) & ((x)-1))==0)

/*---------------------------------------------------------------------------*
 * Simd compute bit size:                                                    *
 *     scan the loop body to compute size of each expr.                      *
 *     set has bad operator if an operation is not supported by SIMD         * 
 *---------------------------------------------------------------------------*/
INT 
SIMD_LOOP::Screen_Operator_Compute_Size(WN* wn, SIMD_IF_ANA *if_ana)
{
    //
    // TODO: Handle TIE types and operations during Auto TIE analysis
    //

    // shortcut
    if (Bad_Operator()) {
	return 0;
    }
    
    Is_True(Simd_Target == SIMD_TARG_VECTRA_I,
	    ("Wrong screening function for non-Vectra I target."));
    
    OPERATOR oper  = WN_operator(wn);
    TYPE_ID  rtype = WN_rtype(wn);
    TYPE_ID  desc  = WN_desc(wn);

    bool double_size = false;
    bool unsigned_ok = false; // mark sign-relatively-safe operations

    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Screen_Operator_Compute_Size(kid, if_ana);
	}
	return 0;
    } else if (oper == OPR_DO_LOOP) {
	if (if_ana) {
	    SIMD_Msg(AT_MSG_SIMD_IF_HAS_LOOP, if_ana->If_Wn());
	    Set_Bad_Oper_Msg(if_ana->If_Wn());
	    return 0;
	} else {
	    return Screen_Operator_Compute_Size(WN_do_body(wn), if_ana);
	}
    } else if (oper == OPR_IF) {
	if (LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || Has_If_Convert_Pragma(wn)) {
	    WN *if_test = WN_if_test(wn);
	    if (OPERATOR_is_compare(WN_operator(if_test))) {
		SIMD_IF_ANA *new_if_ana = 
		    CXX_NEW(SIMD_IF_ANA(wn, Pool()), Pool());
		If_Ana_Map().Enter(wn, new_if_ana);
		if (if_ana) {
		    if_ana->Add_Inner_If(new_if_ana);
		}
		new_if_ana->Set_Is_Test();
		Screen_Operator_Compute_Size(if_test, new_if_ana);
		new_if_ana->Set_Is_Then();
		Screen_Operator_Compute_Size(WN_then(wn), new_if_ana);
		new_if_ana->Set_Is_Else();
		Screen_Operator_Compute_Size(WN_else(wn), new_if_ana);
		if (new_if_ana->If_Conv_Analysis()) {
		    Set_Bad_Oper_Msg(wn);
		}
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
    if (!OPERATOR_is_leaf(oper) &&
	!Varies_With_Loop(wn, Simd_Loop_Level())) {
	SIMD_EINFO *e_info = E_Info().Find(wn);
	if (e_info == NULL) {
	    /* invariant can be moved out of the loop */
	    cur_size = Compute_Size(wn);
	    
	    e_info = CXX_NEW(SIMD_EINFO(wn, cur_size,Pool()), Pool());
	    E_Info().Enter(wn, e_info);
	    e_info->Set_Res_Type(Bit_Size_To_Int(cur_size));
	}
	return cur_size;
    }

    if (oper == OPR_INTCONST) {
	SIMD_EINFO *e_info = E_Info().Find(wn);
	if (e_info == NULL) {
	    INT32 val = WN_const_val(wn);
	    if (val < -32768 || val > 32767) {
		cur_size = 32;
	    } else if (val < -128 || val > 127) {
		cur_size = 16;
	    } else {
		cur_size = Simd_Info->Get_Narrow_Element_Mem_Bits();
	    }
	    e_info = CXX_NEW(SIMD_EINFO(wn, cur_size,Pool()), Pool());
	    E_Info().Enter(wn, e_info);
	    e_info->Set_Res_Type(Bit_Size_To_Int(cur_size));
	}
	return e_info->Bit_Size();
    }

    switch (oper) {
	case OPR_LDID:   /* direct load */
	    cur_size = MTYPE_bit_size(desc);
	    if (cur_size >= Simd_Info->Get_Wide_Element_Mem_Bits()) {
		// Try to convert it to a narrow element...
		INT max_def_bits = Get_Max_Def_Bits(wn);
		if (max_def_bits == 0) {
		    max_def_bits = cur_size;
		}
		// but not too narrow...
		max_def_bits = MAX(max_def_bits,
				   Simd_Info->Get_Narrow_Element_Mem_Bits());
		// printf("*** LDID: cur_size = %d max_def_bits %d\n",
		// cur_size,max_def_bits);
		cur_size = MIN(cur_size,max_def_bits);
	    }
	    unsigned_ok = true;
	    break;

	case OPR_STID:   /* direct store */
	    cur_size   = MTYPE_bit_size(desc);
	    
	    if (cur_size >= Simd_Info->Get_Wide_Element_Mem_Bits()) {
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
			    if (use_size>=Simd_Info->Get_Wide_Element_Mem_Bits()) {
				INT def_size = Get_Max_Def_Bits(use);
				if (def_size == 0) {
				    def_size = use_size;
				}
				def_size = MAX(def_size,
					       Simd_Info->Get_Narrow_Element_Mem_Bits());
				def_max_size = MAX(def_size,def_max_size);
				use_size = MIN(use_size, def_size);
			    }
			    use_min_size = MIN(use_size,use_min_size);
			}
		    }
		}
		
		if (def_max_size>0 && use_min_size<cur_size) {
		    Is_True(def_max_size>=use_min_size,
			    ("Bad relation def_max_size>=use_min_size!"));
		    if (use_min_size==Simd_Info->Get_Narrow_Element_Mem_Bits() &&
			(def_max_size==use_min_size ||
			 def_max_size==Simd_Info->Get_Wide_Element_Mem_Bits())) {
			cur_size = use_min_size;
			if (def_max_size==Simd_Info->Get_Wide_Element_Mem_Bits()) {
			    double_size = true;
			}
		    }
		}
	    }
	    
	    child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    if (child_size < cur_size) {
		SIMD_EINFO *child_info = E_Info().Find(WN_kid0(wn));
		child_info->Set_Double_Size();
		Set_Has_N_To_W();
	    }
	    unsigned_ok=true;
	    break;

	case OPR_ILOAD:  /* indirect load/store, get size from 'desc' */
	    cur_size   = MTYPE_bit_size(desc);
	    unsigned_ok=true;
	    break;

	case OPR_ISTORE: 
	    cur_size     = MTYPE_bit_size(desc);
	    child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    if (child_size < cur_size) {
	      SIMD_EINFO *child_info = E_Info().Find(WN_kid0(wn));
	      child_info->Set_Double_Size();
	      Set_Has_N_To_W();
	    } else { 
	      unsigned_ok=true;
	    }
	    break;

	case OPR_BNOT:
	    cur_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    unsigned_ok=true;
	    break;
	    
	case OPR_ABS:
	case OPR_NEG:
	    cur_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    break;

	case OPR_CVT:
	    cur_size   = MTYPE_bit_size(rtype);
	    child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    cur_size   = MIN(cur_size, child_size);
	    break;

	case OPR_CVTL:
	    cur_size   = WN_cvtl_bits(wn);
	    if (cur_size < Simd_Info->Get_Narrow_Element_Mem_Bits()) {
		Set_Bad_Oper_Msg(wn);
		return 0;
	    }
	    child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
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
	    WN *child1 = WN_kid0(wn);
	    WN *child2 = WN_kid1(wn);
    
	    cur_size  = Screen_Operator_Compute_Size(child1, if_ana);
	    child_size= Screen_Operator_Compute_Size(child2, if_ana);
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

	    cur_size  = Screen_Operator_Compute_Size(child1, if_ana);
	    child_size= Screen_Operator_Compute_Size(child2, if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    cur_size = MAX(cur_size, child_size);
	    
	    /* visit the children in the expression tree and 
	       add size conversion */
	    SIMD_EINFO *child1_info = E_Info().Find(child1);
	    if (child1_info->Bit_Size() < cur_size) {
	      child1_info->Set_Double_Size();
	      Set_Has_N_To_W();
	    }
	    
	    SIMD_EINFO *child2_info = E_Info().Find(child2);
	    if (child2_info->Bit_Size() < cur_size) {
	      child2_info->Set_Double_Size();
	      Set_Has_N_To_W();
	    }
	    
	    /* combine ADD(roundoff, MPY(x, y)) to MULR by setting **
	    ** the child1 to roundoff register                     */
	    if (oper == OPR_ADD && WN_operator(child2) == OPR_MPY) {
	      if (Tree_Equiv(child1, Round_Expr())) {
		child1_info->Set_Is_Round_Reg();
	      } else if (Round_Expr() == NULL && Simd_Loop()) {
		if (Invariant_In_Simd_Loop(child1)) {
		    Set_Round_Expr(child1);
		    child1_info->Set_Is_Round_Reg();
		}
	      }
	    }
	    break;
	}

	case OPR_BXOR:
	case OPR_BAND:
	case OPR_BIOR:
	case OPR_MAX:
	case OPR_MIN: /* maximum of the children's size */
	    cur_size  = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    child_size= Screen_Operator_Compute_Size(WN_kid1(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    cur_size = MAX(cur_size, child_size);
	    
	    /* visit the children in the expression tree and 
	       add size conversion */
	    child_info = E_Info().Find(WN_kid0(wn));
	    if (child_info->Bit_Size() < cur_size) {
	      child_info->Set_Double_Size();
	      Set_Has_N_To_W();
	    }
	    child_info = E_Info().Find(WN_kid1(wn));
	    if (child_info->Bit_Size() < cur_size) {
	      child_info->Set_Double_Size();
	      Set_Has_N_To_W();
	    }
	    if (oper==OPR_BXOR || oper==OPR_BAND || oper==OPR_BIOR) {
		unsigned_ok=true;
	    }
	    break;

	case OPR_SHL:
	case OPR_ASHR:
	case OPR_LSHR:
	    {
		cur_size   = MTYPE_bit_size(rtype);
		child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
		if (Bad_Operator()) {
		    return 0;
		}
		cur_size = MIN(cur_size, child_size);
	    }
	    break;
	    
	case OPR_MPY:  /* check for 16 bit size */
	    child_size  = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
	    child1_size = Screen_Operator_Compute_Size(WN_kid1(wn), if_ana);
	    if (Bad_Operator()) {
		return 0;
	    }
	    if (child_size <= Simd_Info->Get_Narrow_Element_Mem_Bits() &&
		child1_size <= Simd_Info->Get_Narrow_Element_Mem_Bits()) {
	      cur_size = Simd_Info->Get_Wide_Element_Mem_Bits();
	    } else {
		Set_Bad_Oper_Msg(wn);
		return 0;
	    }
	    /* don't handle outshift for the first release */
	    if (Simd_Info->Mul_Out_Shift() != 0) {
		Set_Bad_Operator();
#if 0
		SIMD_Msg(AT_MSG_SIMD_MUL_SHIFT, wn);
#endif
		return 0;
	    }
	    break;
	    
	case OPR_DIV:
	    if (WN_operator(WN_kid1(wn)) == OPR_INTCONST) {
		INT64 val = WN_const_val(WN_kid1(wn));
		if (val >0 && IS_POWER_OF_2(val)) {
		    cur_size   = MTYPE_bit_size(rtype);
		    child_size = Screen_Operator_Compute_Size(WN_kid0(wn), if_ana);
		    if (Bad_Operator()) {
			return 0;
		    }
		    cur_size = MIN(cur_size, child_size);
		    break;
		}
		Set_Bad_Oper_Msg(wn);
		return 0;
	    }

	default:
	    Set_Bad_Oper_Msg(wn);
	    return 0;
    }
    
    if (cur_size <= 0 ||
	cur_size > Simd_Info->Get_Wide_Element_Reg_Bits()) {
      Set_Bad_Oper_Msg(wn);
      return 0;
    }
    if ((rtype != MTYPE_V && (!MTYPE_is_integral(rtype) ||
			      (!unsigned_ok && !MTYPE_is_signed(rtype)))) ||
	(desc != MTYPE_V && (!MTYPE_is_integral(desc) ||
			     (!unsigned_ok && !MTYPE_is_signed(desc))))) {
      Set_Bad_Oper_Msg(wn);
      return 0;
    }
    if (!unsigned_ok && !OPERATOR_is_load(oper) && !OPERATOR_is_store(oper)) {
      for (INT kid=0; kid<WN_kid_count(wn); kid++) {
	if (!MTYPE_is_signed(WN_rtype(WN_kid(wn,kid)))) {
	    Set_Bad_Oper_Msg(wn);
	    return 0;
	}
	if (oper == OPR_SHL || oper == OPR_ASHR || oper == OPR_LSHR || 
	    oper == OPR_DIV) {
	  break;
	}
      }
    }
    
    /* set up the mapping from 'wn' to 'EINFO' */
    Is_True(E_Info().Find(wn) == NULL, ("E_Info().Find(wn) == NULL"));
    SIMD_EINFO *e_info = CXX_NEW(SIMD_EINFO(wn, cur_size,Pool()), Pool());
    if (double_size) {
	e_info->Set_Double_Size();
	Set_Has_N_To_W();
    }
    E_Info().Enter(wn, e_info);
    e_info->Set_Is_Vector();

#if 0
    /* reset is vector property for loop invariant store */
    if (OPERATOR_is_store(oper)) {
	SIMD_EINFO *child_info = E_Info().Find(WN_kid0(wn));
	bool invar_addr = true;
	if (oper == OPR_ISTORE) {
	    WN *addr = WN_kid1(wn);
	    if (Varies_With_Loop(addr,Simd_Loop_Level())) {
		invar_addr = false;
	    }
	}
	if (invar_addr && !child_info->Is_Vector()) {
	    e_info->Reset_Is_Vector();
	}
    } 
#endif

    e_info->Set_Res_Type(Bit_Size_To_Int(cur_size));
    if (if_ana) {
	if_ana->Add_EInfo(e_info);
    }
    
    /* compute memory load/store size */
    if (OPERATOR_is_load(oper) || OPERATOR_is_store(oper)) {
	INT bits = cur_size;
	if (bits == Simd_Info->Get_Narrow_Element_Mem_Bits()) {
	  Set_Has_Narrow_Element_Size();
	} else if (bits == Simd_Info->Get_Wide_Element_Mem_Bits()) {
	  Set_Has_Wide_Element_Size();
	} else {
	  Set_Bad_Oper_Msg(wn);
	  return 0;
	}
	
	/* set up the IMEM_INFO */
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
		  imem_map.Add_Lod_Sto(wn);
		} else {
		  imem_info->Add_Lod_Sto(wn);
		  imem_map.Add_Lod_Sto(wn);
		}
		e_info->Set_IMem(imem_info);
	    } else {
		Set_Bad_Operator();
		SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS, wn, SIMD_OPCODE_Msg(wn));
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
    } else if (oper != OPR_CVT && oper != OPR_CVTL) {
	SIMD_INFO *simd = Simd_Info;
	int bit_size = (oper == OPR_MPY) ? child_size : cur_size;
	TYPE_ID type = Bit_Size_To_Int(bit_size);
	if (simd->Scalar_SIMD_Map()->Find(type)==NULL) {
	    Set_Bad_Oper_Msg(wn);
	    return 0;
	} else {
	    /* Screen for configuration parameter */
	    TYPE_ID simd_type =  simd->Get_SIMD_Reg_Type_Scalar(type);
	    TIE_MACRO_ID macro_id = simd->Search_Vector_Macro(oper, simd_type);
	    if (macro_id == TIE_INVALID_ID) {
		Set_Bad_Oper_Msg(wn);
		return 0;
	    }
	}
    }
    
    return cur_size;
}

/*---------------------------------------------------------------------------*
 * Record_Dependence_Subgraph                                                *
 *                                                                           *
 * Scan the array reference in the loop nest.                                *
 *  (1) Memorize the dependence edges at model loop level                    *
 *  (2) Setup two mappings: WN to vertex, vertex to WN                       *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Record_Dependence_Subgraph(
    BOOL *can_be_unrolled,                  // unroll candidate
    STACK<WN*> *v_to_wn,                    // vertex to wn map
    WN_VIDX_MAP *wn_to_v,                   // wn to vertex map
    ARRAY_DIRECTED_GRAPH16 *dg,             // array dependence graph    
    EDGE_STACK *edge_on_level               // output: edges for the model loop level
    )
{
    WN_ARRAY *wn_array = IMem_Map().All_Lod_Sto();
    for (INT i = 0; i < wn_array->Elements(); i++) {
	WN *wn = wn_array->Get(i);
	    
	Is_True(WN_operator(wn) == OPR_ILOAD || 
		WN_operator(wn) == OPR_ISTORE, 
		("WN is not an ILOAD/ISTORE"));
	
	/* set up the mappings; vertexes start from 1 */
	v_to_wn->Push(wn);
	wn_to_v->Enter(wn, v_to_wn->Elements());
	    
	/* memorize the dependence graph at the model loop level */
	VINDEX16 src = dg->Get_Vertex(wn);
	for (EINDEX16 e = dg->Get_Out_Edge(src); e; 
	     e = dg->Get_Next_Out_Edge(e)) {
          DEPV_ARRAY *depv_a = dg->Depv_Array(e);
          /* Record the edge if the dependence is carried by the model loop. */
          if (depv_a->Dependence_Carried_By_Loop(Simd_Loop_Level())) {
            edge_on_level->Push(e);
          }
	}
    }
}

/*--------------------------------------------------------------------------*
 * Setup scalar expansion                                                   *
 *--------------------------------------------------------------------------*/
void
SIMD_LOOP::Setup_Scalar_Expansion(SIMD_INFO *simd)
{
    for (INT i = 0; i < S_Info_Stack().Elements(); i++) {
	SIMD_SCALAR *cur = S_Info_Stack().Bottom_nth(i);
	if (cur->Has_Def()) {
	    WN         *wn     = cur->Lod_Sto().Bottom_nth(0);
	    SIMD_EINFO *e_info = Get_E_Info(wn);
	    Is_True(e_info, ("SIMD_LOOP::Setup_Scalar_Expansion: NULL einfo"));
	    ST *se_array = simd->Create_SE_Array(e_info->Res_Type(), 
						 V_Unroll_Factor(simd));
	    cur->Set_SE_Array(se_array);
	    cur->Set_Expand();
	}
    }
}
/*--------------------------------------------------------------------------*
 * Rebuild SIMD data structure                                              *
 *--------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Rescan_Loop_Body(WN *outerloop)
{
    Is_True(outerloop == Simd_Loop(),("Wrong loop to SIMD."));
    
    Screen_Operator_Compute_Size(WN_do_body(outerloop), NULL);
    if (V_Unroll_Factor() <= 0) {
      return;
    }
    
    FmtAssert(!Bad_Operator(),("Unhandled operator in SIMD transform stage."));
    
    bool access_ok = IMem_Map().Setup_Access_Properties(false);
    Is_True(access_ok,("Can't handle array accesses at SIMD transform stage."));
    
    IMem_Map().Order_Specific_Setup(Loop_Model(), Num_Loops(), this, false, -1, false);
    Is_True(!Bad_Stride(),("Bad write stream in SIMD transform stage."));
    IMem_Map().Group();
    if (simd_debug) {
	fprintf(TFile,"SIMD groups before transformation:\n");
	IMem_Map().Print_Groups(TFile);
    }

    Setup_Type_Conversion(WN_do_body(outerloop));
    Simd_If_Conv(WN_do_body(outerloop));
    Simd_Preprocess_Transform(WN_do_body(outerloop));
    Simd_Preprocess_Interleave();
}

// check if it is ok for 'inner' to be the inner loop
bool
SIMD_LOOP::Is_Inner_Ok(INT inner, bool msg) {
    // don't try the rearrange the loops inside the vector loop
    if (inner<Num_Loops()-1 && inner>=Simd_Loop_Level()) {
	return false;
    }

    // don't put loops into the vector loop if this is not an invariant SNL
    if (!Loop_Model()->Trying_Invariant() && inner < Simd_Loop_Level()) {
      return false;
    }
    
    // setup vector and alignment properties
    if (!IMem_Map().Setup_Access_Properties(/* before_transform */ true)) {
      /* Info messages printed inside. */
      return false;
    }
    
    IMem_Map().Order_Specific_Setup(Loop_Model(), Num_Loops(), this, true, inner, msg);
    if (Bad_Stride()) {
      return false;
    }

    return true;
}


void
SIMD_LOOP::Annotate_Vector_Symbol_Tree (SYMBOL_TREE_NODE *symbol_node,
					LNO_REGCLASS_INFO *regclass_info)
{
  if (!symbol_node) {
    return;
  }
  
  TYPE_ID type = symbol_node->Symbol().Type;
  TYPE_ID vec_type = Simd_Info->Get_SIMD_Reg_Type_Scalar(type);
  if (vec_type != MTYPE_UNKNOWN) {
    LNO_REGS_IDX vec_idx = regclass_info->Index_Of_Type(vec_type);
    symbol_node->Set_Vec_Idx(vec_idx);
  }
  Annotate_Vector_Symbol_Tree(symbol_node->Left(), regclass_info);
  Annotate_Vector_Symbol_Tree(symbol_node->Right(), regclass_info);
}


/* Annotate vector and alignment information on the array reference
   and symbol nodes. */
void
SIMD_LOOP::Annotate_Vector_Info(ARRAY_REF *ar, SYMBOL_TREE *symbol_tree)
{
  Is_True(ar, ("Null ARRAY_REF"));
  Is_True(!Simd_Info->AT_Analysis_Phase(),
	  ("AT analysis should use a different vector annotation function."));
    
  if (simd_debug) {
    fprintf(TFile, "Annotating vector information for the loop model\n");
  }
    
  LNO_REGCLASS_INFO *regclass_info = Loop_Model()->Regclass_Info();
  Is_True(regclass_info,("Null LNO register class info"));
  
  // FIXME: add these the SIMD TYPE structure
  TYPE_ID sel_type = Simd_Info->Get_Sel_Type();
  LNO_REGS_IDX sel_idx = regclass_info->Index_Of_Type(sel_type);
    
  for (INT i = 0; i < ar->Elements(); i++) {
    ARRAY_REF_LIST *ref_list = ar->Array_Ref_List(i);

    bool is_se = ref_list->Is_Scalar_Expanded();
	
    ARRAY_REF_ITER iter(ref_list);
    for (ARRAY_REF_NODE *node = iter.First(); node; node = iter.Next()) {
      TYPE_ID type = node->Type();
      TYPE_ID vec_type = Simd_Info->Get_SIMD_Reg_Type_Scalar(type);
      if (vec_type == MTYPE_UNKNOWN) {
	continue;
      }

      LNO_REGS_IDX vec_idx = regclass_info->Index_Of_Type(vec_type);
      if (vec_idx == LNO_REGS_IDX_UNDEFINED) {
	continue;
      }

      if (is_se) {
	node->Set_Vec(vec_idx);
      } else {
	WN* wn = LWN_Get_Parent(node->Wn);
	OPERATOR oper = WN_operator(wn);

	IMEM_INFO *ii = NULL;
	if (oper == OPR_ILOAD || oper == OPR_ISTORE) {
	  ii = IMem_Map().Find(wn);
	}
	
	if (ii && ii->Is_Vector()) {
	  node->Set_Vec(vec_idx);
	  if (!ii->Is_Aligned() && ii->Is_First_Imem_In_Group()) {
	    if (Simd_Target != SIMD_TARG_VECTRA_I ||
		ii->Is_Use()) {
	      TYPE_ID align_type = Simd_Info->Get_Alignment_Type_Scalar(type);
	      if (align_type != MTYPE_UNKNOWN) {
		LNO_REGS_IDX align_idx = regclass_info->Index_Of_Type(align_type);
		node->Set_Align(align_idx);
	      }
	    } else {
	      node->Set_Sel(sel_idx);
	    }
	  }
	} else if (!OPERATOR_is_store(oper)) {
	  // use the invariant table
	  // this is necessary for array references and expressions
	  // that participate in a vector expression
	  WN *parent = LWN_Get_Parent(wn);
	  Is_True(parent,("Null parent of a load"));
	  OPERATOR parent_oper = WN_operator(parent);
	  if (parent_oper == OPR_SHL ||
	      parent_oper == OPR_ASHR ||
	      parent_oper == OPR_LSHR ||
	      parent_oper == OPR_DIV) {
	    continue;
	  }
	  if (Varies_With_Loop(parent, Simd_Loop_Level())) {
	    node->Set_Vec(vec_idx);
	  }
	}
      }

      if (node->Vec() != LNO_REGS_IDX_UNDEFINED) {
	node->Set_Vec_Regs(1);
	INT width = Simd_Info->Get_SIMD_Width_Scalar(type);
	if (width > 0) {
	  INT unroll = V_Unroll_Factor(Simd_Info);
	  Is_True((unroll > 0) && (unroll % width == 0),
		  ("Bad unroll (%d) and SIMD width relation.",
		   unroll, width));
	  node->Set_Vec_Regs(unroll / width);
	}
      }
    }
  }
  
  /* Annotate the symbols. */
  Annotate_Vector_Symbol_Tree(symbol_tree->Symbol_Node(),regclass_info);
}


//
// Simd test vectorization
//
// Input:
//     BOOL *can_be_unrolled:         array of levels with
//                                    'can be unrolled' tags
//     INT   outermost_can_be_tiled:  outmost tilable level
//     ARRAY_DIRECTED_GRAPH16 *dg:    dependence graph
//     DOLOOP_STACK *dl_stack:        a stack of all loops in the SNL
//
// Output:
//     Return true if vectorizable, false otherwise
//     INT  *_simd_unroll: amount of unroll if vectorizable, 0 otherwise
//
bool
SIMD_LOOP::Test_Vectorization(BOOL *can_be_unrolled, INT outmost_can_be_tiled,
			      ARRAY_DIRECTED_GRAPH16 *dg,
			      DOLOOP_STACK *dl_stack) {
    {
	MEM_POOL_Popper popper(Pool());
	
	// run the simple checks first
	if (!Test_Model_Loop(dl_stack)) {
	    // every check prints its own info message
	    return false;
	}
	
	/* Scalar dependence tests. */
	if (!Test_Scalar_Dependence_Ok()) {
          /* Info messages already printed. */
          return false;
	}
	
	// array dependence tests
	if (!Test_Array_Dependences(can_be_unrolled,outmost_can_be_tiled,dg)) {
	    // info messages are printed inside
	    return false;
	}
	
	// all dependence checks passed -- vectorize the loop (tentatively)
	INT model_level = Simd_Loop_Level();
	_simd_unroll[model_level] = V_Unroll_Factor(Simd_Info);
	
#ifdef Is_True_On
	{
	    // check if the SIMD unroll amounts are set correctly
	    for (INT i = 0; i < Num_Loops(); i++) {
		Is_True((i!=model_level && _simd_unroll[i]==0) ||
			(i==model_level && _simd_unroll[i]>0), ("Bad SIMD unroll amounts"));
	    }
	}
#endif
    }
    
    // checks for vectorizability with the current loop order
    if (!Is_Inner_Ok(Num_Loops()-1, /* message */true)) {
	return false;
    }

    // test array references, invariant levels, groups, etc.
    // Test_Imem may create the group structure which should be live at exit, so   
    // we pop the pool before that
    if (!Test_Imem()) {
	// messages printed inside
	return false;
    }
    
    if (!Test_If_Conversion()) {
      return false;
    }
    
    // set trapezoidal
    if (Has_Inner_Trapezoidal_Loop(dl_stack, Simd_Loop_Level())) {
	Set_Trapezoidal();
    }

    return true; // vectorizable
} // SIMD_LOOP::Test_Vectorization

/*--------------------------------------------------------------------------*
 * Any inner loop is trapezoidal                                            *
 *--------------------------------------------------------------------------*/
bool 
SIMD_LOOP::Has_Inner_Trapezoidal_Loop(DOLOOP_STACK *dl_stack, INT loop)
{
    for (INT i = loop + 1; i < dl_stack->Elements(); i++) {
	WN *inner = dl_stack->Bottom_nth(i);
	if (Varies_With_Loop(WN_kid(WN_start(inner),0),loop) ||
	    Varies_With_Loop(WN_end(inner),loop) ||
	    Varies_With_Loop(WN_kid(WN_step(inner),0),loop)) {
	    return true;
	    /* the previous code was (let's see if the new code is working):
	      if (Loop_Is_Trapezoidal(inner, Array_Dependence_Graph, Du_Mgr)) {
	      return true;
	    */
	}
    }
    return false;
}

INT64
SIMD_LOOP::Max_Iteration_Count(void)
{
    WN *loop = Simd_Loop();
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(loop);
    if (dli->Est_Num_Iterations > 0 && dli->Est_Max_Iterations_Index > 0) {
	/* min */
	return (dli->Est_Max_Iterations_Index > dli->Est_Num_Iterations) ?
	    dli->Est_Num_Iterations : dli->Est_Max_Iterations_Index;
    } else {
	/* max */
	return (dli->Est_Max_Iterations_Index > dli->Est_Num_Iterations) ?
	    dli->Est_Max_Iterations_Index : dli->Est_Num_Iterations;
    }
}

//  Quick checks whether to vectorize the current model loop and
//  if so, return true
bool
SIMD_LOOP::Test_Model_Loop(DOLOOP_STACK *dl_stack)
{
    INT i;
    INT level = Simd_Loop_Level();
    WN *loop = Simd_Loop();
    
    Is_True(dl_stack,("Null DO_LOOP stack"));
    
    if (Simd_Target == SIMD_TARG_VECTRA_I &&
	Simd_Info->N_S_Regs()==0 &&
	Has_Wide_Element_Size() && Has_Narrow_Element_Size()) {
#if 0
	SIMD_Msg(AT_MSG_SIMD_OP_NO_TYPE_CONV_VSEL,loop);
#endif
	return false;
    }
    
    INT64 step = Step_Size(loop);
    if (step != 1) {
      SIMD_Msg(AT_MSG_SIMD_LOOP_STEP, loop, (INT)step);
      return false;
    }
    
    if ((level != dl_stack->Elements() - 2) && 
	Has_Inner_Trapezoidal_Loop(dl_stack, level)) {
	SIMD_Msg(AT_MSG_SIMD_TRAPEZOIDAL_INNER_LOOP,loop);
	return false;
    }

    INT64 est_iter = Max_Iteration_Count();
    if (est_iter > 0 && V_Unroll_Factor(Simd_Info) > est_iter) {
	Set_Too_Small();
	SIMD_Msg(AT_MSG_SIMD_SMALL_TRIP_COUNT, loop);
	return false;
    }
    
    if (!Upper_Bound_Standardize(WN_end(loop),TRUE)) {
      SIMD_Msg(AT_MSG_SIMD_BAD_LOOP_UPPER_BOUND, loop);
      return false;
    }
    
#if 0
    // screen out vector loop variant shift amount
    for (INT s = 0; s < Shift_Amount().Elements(); s++) {
	WN *shift_amount = Shift_Amount().Bottom_nth(s);
	if (Varies_With_Loop(shift_amount, Simd_Loop_Level())) {
	    Set_Bad_Shift();
	    SIMD_Msg(AT_MSG_SIMD_VARIANT_SHIFT, loop);
	    return false;
	}
    }
#endif
    
    // all simple checks pass
    return true;
} // SIMD_LOOP::Test_Model_Loop


bool
SIMD_LOOP::Test_Array_Reduction (SCC_DIRECTED_GRAPH16 *level_dg,
				 STACK<WN*> *v_to_wn)
{
  /* Check if there are cycles in the loop. */
  if (!level_dg->Has_Cycles()) {
    return true;
  }
  
  if (!red_manager) {
    SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, Simd_Loop());
    return false;
  }
  
  INT v_cnt = v_to_wn->Elements() + 1;
  EDGE_STACK *red_edges = CXX_NEW(EDGE_STACK(Pool()), Pool());
  
  for (INT v_id = 1; v_id < v_cnt; v_id++) {
    WN *v_wn = v_to_wn->Bottom_nth(v_id - 1);
    WN *v_addr =
      (WN_operator(v_wn) == OPR_ILOAD) ? WN_kid0(v_wn) :
      (WN_operator(v_wn) == OPR_ISTORE) ? WN_kid1(v_wn) : NULL;
      
    REDUCTION_TYPE red_v = red_manager->Which_Reduction(v_wn);
    
    bool red_possible = (red_v != RED_NONE) &&
      Simd_Info->Reduction_Possible(red_v, WN_desc(v_wn));
    
    /* Check self cycle. */
    EINDEX16 edge = level_dg->Get_Edge(v_id, v_id);
    if (edge) {
      if (!red_possible ||
          (v_addr && !Invariant_In_Simd_Loop(v_addr))) {
        
        SIMD_Msg(AT_MSG_SIMD_ARRAY_DEPENDENCE, v_wn, SIMD_Base_Name_Msg(v_wn),
                 SIMD_Base_Name_Msg(v_wn), (INT)Srcpos_To_Line(LWN_Get_Linenum(v_wn)));
        
	return false;
      }
      
      red_edges->Push(edge);
    }
    
    for (INT u_id = v_id + 1; u_id < v_cnt; u_id++) {
      if (level_dg->Get_Scc_Id(v_id) != level_dg->Get_Scc_Id(u_id)) {
	continue;
      }

      WN *u_wn = v_to_wn->Bottom_nth(u_id - 1);
      
      /* No dependences between two loads. */
      if (OPERATOR_is_load(WN_operator(v_wn)) &&
          OPERATOR_is_load(WN_operator(u_wn))) {
        continue;
      }
      
      WN *u_addr =
	(WN_operator(u_wn) == OPR_ILOAD) ? WN_kid0(u_wn) :
	(WN_operator(u_wn) == OPR_ISTORE) ? WN_kid1(u_wn) : NULL;
      
      REDUCTION_TYPE red_u = red_manager->Which_Reduction(u_wn);
      if (red_u == RED_NONE || red_u != red_v || !red_possible ||
          (v_addr && !Invariant_In_Simd_Loop(v_addr)) ||
          (u_addr && !Invariant_In_Simd_Loop(u_addr))) {
        
        SIMD_Msg(AT_MSG_SIMD_ARRAY_DEPENDENCE, v_wn, SIMD_Base_Name_Msg(v_wn),
                 SIMD_Base_Name_Msg(u_wn), (INT)Srcpos_To_Line(LWN_Get_Linenum(u_wn)));
        
	return false;
      }
      
      edge = level_dg->Get_Edge(v_id, u_id);
      if (edge) {
	red_edges->Push(edge);
      }
      
      edge = level_dg->Get_Edge(u_id, v_id);
      if (edge) {
	red_edges->Push(edge);
      }
    }
  }
  
  /* All SCCs are safe reductions, remove the reduction
     edges and rebuild the SCC. */
  for (INT j = 0; j < red_edges->Elements(); j++) {
    EINDEX16 e = red_edges->Bottom_nth(j);
    Is_True(e, ("Bad edge"));
    
    level_dg->Delete_Edge(e);
  }

  /* Check for SCCs again */
  if (level_dg->Has_Cycles()) {
    SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, Simd_Loop());
    return false;
  }
  
  return true;
}


bool
SIMD_LOOP::Test_Array_Dependences(BOOL *can_be_unrolled, INT outmost_can_be_tiled,
				  ARRAY_DIRECTED_GRAPH16 *dg) {

    // Note: this function relies on the caller to push/pop the memory pools
    INT model_level = Simd_Loop_Level();
    INT depv_level = model_level + Num_Good() - Num_Loops();

    /* Store the WN to be used for the dependence message. */
    WN *dep_wn = Simd_Loop();
    
    if (!can_be_unrolled[model_level]) {
	SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, dep_wn);
	return false;
    }
    
    // Test array dependences by building a dependence graph
    // with the dependence edges carried by the model loop

    /* maps from vertex index to WN * and reverse */
    STACK<WN *> *v_to_wn = CXX_NEW(STACK<WN *>(Pool()),Pool());
    WN_VIDX_MAP *wn_to_v = CXX_NEW(WN_VIDX_MAP(101, Pool()), Pool());
    
    /* dependence for the model loop level */
    EDGE_STACK *edge_on_level = CXX_NEW(EDGE_STACK(Pool()), Pool());
    
    /* memorize dependence graph by dependence level */
    Record_Dependence_Subgraph(can_be_unrolled, v_to_wn, wn_to_v,  
			       dg, edge_on_level);
    
    if (edge_on_level->Elements() == 0) {
      return true;
    } 

    bool can_simd = false;
    
    INT v_cnt = v_to_wn->Elements() + 1;
    SCC_DIRECTED_GRAPH16 *level_dg =
      CXX_NEW(SCC_DIRECTED_GRAPH16(v_cnt, v_cnt), Pool());
    
    /* add the vertices */
    for (INT j = 0; j < v_to_wn->Elements() + 1; j++) {
      level_dg->Add_Vertex();
    }
    
    /* add the edges */
    for (INT j = 0; j < edge_on_level->Elements(); j++) {
      EINDEX16 e = edge_on_level->Bottom_nth(j);
      Is_True(e, ("Bad edge"));
      
      /* Find the source and sink vertex. All dependences carried by
         the loop must be between ILOAD/ISTORE nodes. */
      WN *src = dg->Get_Wn(dg->Get_Source(e));
      Is_True(src, ("Cannot find the source, edge %d", (INT)e));
      if (WN_operator(src) != OPR_ILOAD && WN_operator(src) != OPR_ISTORE) {
        if (simd_debug) {
          fprintf(TFile, "source wn %p, operator %s\n",
                  src, OPERATOR_name(WN_operator(src)));
        }
        SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, src);
        return false;
      }
      
      WN *snk = dg->Get_Wn(dg->Get_Sink(e));
      Is_True(snk, ("Cannot find the sink, edge %d", (INT)e));
      if (WN_operator(snk) != OPR_ILOAD && WN_operator(snk) != OPR_ISTORE) {
        if (simd_debug) {
          fprintf(TFile, "sink wn %p, operator %s\n",
                  snk, OPERATOR_name(WN_operator(snk)));
        }
        SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, snk);
        return false;
      }
      
      VINDEX16 src_vertex = wn_to_v->Find(src);
      VINDEX16 snk_vertex = wn_to_v->Find(snk);
      if (simd_debug) {
	fprintf(TFile, "edge %d: source wn %p, vertex %d\n", (INT)e, src, src_vertex);
	fprintf(TFile, "edge %d: sink wn %p, vertex %d\n", (INT)e, snk, snk_vertex);
      }
      Is_True(src_vertex,
	      ("Cannot find the source vertex (edge %d, wn %p)", (INT)e, src));
      Is_True(snk_vertex,
	      ("Cannot find the source vertex (edge %d, wn %p)", (INT)e, snk));
      
      /* add the edge to the level graph */
      level_dg->Add_Unique_Edge(src_vertex, snk_vertex);
    }
    
    bool array_red = Test_Array_Reduction(level_dg, v_to_wn);

    if (array_red) {
      VINDEX16 scc_count = level_dg->Get_Scc_Count();
      
      /* level sort to get the right order */
      VINDEX16* top_ord = CXX_NEW_ARRAY(VINDEX16, scc_count, Pool());
      level_dg->Level_Sort(top_ord);
      
      bool has_backward = false;
      for (INT k = 1; k < scc_count && !has_backward; k++) {
	for (INT j = 0; j < k; j++) {
	  if (top_ord[j] > top_ord[k] /* lexically after */ &&
	      level_dg->Get_Edge(top_ord[j],top_ord[k])) {
            dep_wn = v_to_wn->Bottom_nth(top_ord[j] - 1);
	    has_backward = true;
	    break;
	  }
	}
      }
      
      if (!has_backward) {
	can_simd = true;
      }
    }
    
    // Graphs use their own memory pools, so free them.
    CXX_DELETE(level_dg, Pool());
    
    if (!can_simd) {
      /* If the array reduction test fails, the info message is printed
         inside the Test_Array_Reduction method. */
      if (array_red) {
        SIMD_Msg(AT_MSG_SIMD_DATA_DEPENDENCE, dep_wn);
      }
      
      return false;
    }
    
    return true;
}


/*-------------------------------------------------------------------------*
 * Test scalar dependence with respect to the model loop                   *
 *-------------------------------------------------------------------------*/
bool 
SIMD_LOOP::Test_Scalar_Dependence_Ok()
{
  WN *loop = Simd_Loop();
  INT level = Simd_Loop_Level();
  
  for (INT i = 0; i < S_Info_Stack().Elements(); i++) {
    SIMD_SCALAR *s_info = S_Info_Stack().Bottom_nth(i);
    s_info->Setup_Scalar_Dependence_Info(loop);
    if (s_info->Bad_Dep()) {
      WN *wn = s_info->Lod_Sto().Bottom_nth(0);
      SIMD_Msg(AT_MSG_SIMD_SCALAR_DEPENDENCE, wn, SIMD_Base_Name_Msg(wn));
      return false;
    }
  }
  
  for (INT i = 0; i < S_Info_Stack().Elements(); i++) {
    SIMD_SCALAR *s_info = S_Info_Stack().Bottom_nth(i);
    s_info->Check_Reduction(loop, level, this);
    if (s_info->Bad_Dep()) {
      WN *wn = s_info->Lod_Sto().Bottom_nth(0);
      SIMD_Msg(AT_MSG_SIMD_SCALAR_DEPENDENCE, wn, SIMD_Base_Name_Msg(wn));
      return false;
    }
  }
  
  return true;
}

bool
SIMD_LOOP::Test_Imem_Alignment (void)
{
  INT n_a_regs = Simd_Info->N_A_Regs();
  INT n_s_regs = Simd_Info->N_S_Regs();
  if (n_a_regs != 0 && n_s_regs != 0)
    return true;
  
  INT num_groups = IMem_Map().Groups().Elements();
  for (INT i = 0; i < num_groups; i++) {
    IMEM_GROUP *ig = IMem_Map().Groups()[i];
    // give up if there is a problem with the read or the write stream
    if (ig->Is_Vector() && !ig->Is_Aligned()) {
      if (ig->Is_Use() && n_a_regs == 0) {
        SIMD_Msg(AT_MSG_SIMD_UNALIGNED_LOAD, Simd_Loop(), SIMD_MTYPE_Msg(ig->Scalar_Type()));
        return false;
      }
      
      if (ig->Is_Def() && n_s_regs == 0) {
        SIMD_Msg(AT_MSG_SIMD_UNALIGNED_STORE, Simd_Loop(), SIMD_MTYPE_Msg(ig->Scalar_Type()));
        return false;
      }
    }
  }
  
  return true;
}

bool
SIMD_LOOP::Test_Imem_Field_Selection (void) {
    if (Simd_Info->N_S_Regs() == 0 && IMem_Map().Get_Max_Offset_Count() > 1) {
      SIMD_Msg(AT_MSG_SIMD_NO_SELECT, Simd_Loop(), 1, SIMD_MTYPE_Msg(MTYPE_UNKNOWN));
      return false;
    }
    
    return true;
}

bool
SIMD_LOOP::Test_Imem_Load_Variable_Stride(IMEM_GROUP *ig)
{
    return true;
}

bool
SIMD_LOOP::Test_Imem_Load_Reuse_Supported(IMEM_GROUP *ig)
{
    // it is supported but not beneficial, so return false.
    return false;
}


bool
SIMD_LOOP::Test_If_Conversion (void)
{
  return false;
}


bool
SIMD_LOOP::Test_Imem()
{
  // group and check for bad grouping 
  IMem_Map().Group();
  IMem_Map().Collect_Load_Store();
  if (simd_debug) {
    fprintf(TFile,"SIMD groups at model time:\n");
    IMem_Map().Print_Groups(TFile);
  }
  
  INT num_groups = IMem_Map().Groups().Elements();
  for (INT i = 0; i < num_groups; i++) {
    IMEM_GROUP *ig = IMem_Map().Groups()[i];
    if (ig->Is_Vector()) {
      if (ig->Has_Gaps()) {
        WN *wn = ig->First_Imem_Info()->Orig_Wn();
        SIMD_Msg(AT_MSG_SIMD_ACCESS_GAPS, wn, SIMD_Base_Name_Msg(wn));
        Set_Struct_Gap();
        return false;
      }
      
      if (ig->Has_Bad_Elem()) {
        WN *wn = ig->First_Imem_Info()->Orig_Wn();
        SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS_STRIDE,
                 wn, SIMD_Base_Name_Msg(wn));
        Set_Struct_Gap();
        return false;
      }
      if (ig->Variable_Stride()) {
        if (!Test_Imem_Load_Variable_Stride(ig)) { 
          SIMD_Msg(AT_MSG_SIMD_VAR_STRIDE, Simd_Loop());
          return false;
        }
      }
    } else if (ig->Is_Def() && !ig->Is_Reduction()) {
      Is_True(!ig->Is_Vector(), ("Expected a non-vector group."));
      WN *wn = ig->Stores()->Get(0);
      SIMD_Msg(AT_MSG_SIMD_SCALAR_ARRAY_STORE, wn, SIMD_Base_Name_Msg(wn));
      return false;
    }
  }
  
  if (!Test_Imem_Alignment())
    return false;
  
  if (!Test_Imem_Field_Selection())
    return false;
  
  return true;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for a block                                          *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Transform_Block(WN *blk, SIMD_INFO *simd, WN *copy_block)
{
    Is_True(WN_operator(blk) == OPR_BLOCK &&
	    WN_operator(copy_block) == OPR_BLOCK, ("Not a block"));

    /* scan the block to transform loads/stores */
    WN *orig_stmt = WN_first(blk); 
    WN *copy_stmt = WN_first(copy_block);
    WN *trap_loop = NULL;
    while (orig_stmt != NULL) {
	Is_True(WN_operator(copy_stmt) == WN_operator(orig_stmt),
		("Original and copy do not match"));

	/* advanced the current copy */
	WN *cur_copy = copy_stmt;
	WN *cur_orig = orig_stmt;
	copy_stmt = WN_next(copy_stmt);
	orig_stmt = WN_next(orig_stmt);

	if (OPCODE_is_scf(WN_opcode(cur_orig))) {
	    if (WN_operator(cur_orig) == OPR_DO_LOOP) {
		Simd_Transform_Loop_Body(cur_orig, simd, cur_copy);
		if (Trapezoidal() && cur_orig != Simd_Loop()) {
			trap_loop = cur_orig;
		} 
	    } else if (WN_operator(cur_orig) == OPR_IF) {
		/* push the orig_stmt to the stack to delete it later */
		Old_Stmt().Push(cur_orig);

		Simd_Transform_If(cur_orig, simd, cur_copy);
	    } else {
		FmtAssert(0, ("Unhandled statement type"));
	    }
	} else {
	    Is_True(OPCODE_is_stmt(WN_opcode(cur_orig)),
		    ("Unhandled statement type"));

	    /* push the orig_stmt to the stack to delete it later */
	    Old_Stmt().Push(cur_orig);

	    /* Expand expression into SIMD expression */
	    Simd_Transform(cur_orig, cur_copy, simd);
	    LWN_Delete_Tree(cur_copy);
	}
    }
    /* finalize scalar expansion, after the inner loop */
    if (trap_loop) {
	Simd_Finalize_Scalar_Expansion(trap_loop, simd, copy_stmt ?
						copy_stmt: WN_last(copy_block));
    }
}

/*---------------------------------------------------------------------------*
 * Simd code generation for an IF                                            *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Transform_If(WN *orig_if, SIMD_INFO *simd, WN *copy_if)
{
    Is_True(WN_operator(orig_if) == OPR_IF &&
	    WN_operator(copy_if) == OPR_IF, ("Not an IF stmt"));

    WN *if_test = WN_if_test(orig_if);
    Simd_Transform(if_test, copy_if, simd);
    Is_True(0, ("Need to update the if test"));
    Simd_Transform_Block(WN_then(orig_if), simd, WN_then(copy_if));
    Simd_Transform_Block(WN_else(orig_if), simd, WN_else(copy_if));
}

/*---------------------------------------------------------------------------*
 * Transform vload to vload pair                                             *
 * Transform ldid to LC                                                      *
 *---------------------------------------------------------------------------*/
WN*
SIMD_LOOP::Combine_Load(SIMD_EINFO *e_info, WN *stmt, SIMD_INFO *simd)
{
    Is_True(e_info->Simd_Exprs()->Elements()==1, ("Unexpected double type"));
    
    WN  *simd_expr = e_info->Simd_Expr(0);

    /* increment the load address depending on the load type */
    if (e_info->Pre_Load()) {
	/* (1) move the e_info->Pre_Load() out of the loop */
	WN *old_preload = e_info->Pre_Load();
	WN *old_addr    = WN_prev(old_preload);
	WN *t_loop      = Enclosing_Do_Loop(old_addr);
	Is_True(t_loop, ("Cannot find enclosing loop"));
	
	Is_True(WN_operator(old_addr) == OPR_STID, 
		("Expecting address assignment"));

	/* recompute the alignment */
	IMEM_INFO *imem = e_info->IMem();

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

	    char macro_name[64];
	    INT  bits = simd->Get_Narrow_Element_Mem_Bits();

	    /* alignment register load, generate another load at addr+8 */
	    Is_True(simd_expr == stmt, ("Mismatched load"));
	    Is_True(WN_operator(simd_expr) == OPR_INTRINSIC_CALL,
		    ("Expecting alignment register load"));
	    INTRINSIC intrin_id = WN_intrinsic(simd_expr);
	    Simd_Make_Vectra_Macro("LV", "A_IU", bits, macro_name);
	    Is_True(intrin_id == 
		    Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name)),
		    ("Expecting narrow element alignment register IU load"));
	    
	    WN *pre_load = 
		e_info->Generate_Load_Ahead(e_info->Pre_Load(), simd, Pool(), true);

	    /* add a dependence graph vertex */
	    if (Enclosing_Do_Loop(LWN_Get_Parent(pre_load)) != NULL) {
		Array_Dependence_Graph->Add_Vertex(pre_load);
	    }
	    
	    /* copy LoadAhead() register value to Res_Reg() */
	    WN *copy = 
		e_info->Get_Res_Reg()->Simd_Preg_Stid(
		    e_info->Load_Ahead()->Simd_Preg_Ldid());
	    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), simd_expr, copy);

	    /* generate load ahead */
	    WN *load_ahead =
		e_info->Generate_Load_Ahead(simd_expr, simd, Pool(), false);
	    
	    return load_ahead;
	} else { /* is aligned */
	    /* extract the original address */
	    WN *old_rhs = WN_kid0(old_addr);
	    Is_True(WN_operator(old_rhs) == OPR_ADD, ("Mismatched old addr"));

	    WN *new_rhs = WN_kid0(old_rhs);
	    WN_kid0(old_rhs) = NULL;
	    LWN_Set_Parent(new_rhs, NULL);
	    LWN_Delete_Tree(old_rhs);
	    old_rhs = new_rhs;

	    Is_True(WN_operator(old_rhs) == OPR_BAND, ("Mismatched old addr"));
	    new_rhs = WN_kid0(old_rhs);
	    WN_kid0(old_rhs) = NULL;
	    LWN_Set_Parent(new_rhs, NULL);
	    LWN_Delete_Tree(old_rhs);
	    old_rhs = new_rhs;

	    Is_True(WN_operator(old_rhs) == OPR_SUB ||
		    WN_operator(old_rhs) == OPR_ADD, ("Mismatched old addr"));
	    new_rhs = WN_kid0(old_rhs);
	    WN_kid0(old_rhs) = NULL;
	    LWN_Set_Parent(new_rhs, NULL);
	    LWN_Delete_Tree(old_rhs);

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

	    SIMD_PREG *load_ahead_preg = e_info->Load_Ahead();
	    if (load_ahead_preg == NULL) {
		load_ahead_preg = Gen_Symbol(simd, e_info->Res_Type(), Pool());
		e_info->Set_Load_Ahead(load_ahead_preg);
	    }

	    WN *pre_load = 
		simd->Generate_Load(
		    OPR_ILOAD,
		    desc_type,
		    t_loop, 
		    align_addr->Simd_Preg_Ldid(),
		    0, 
		    load_ahead_preg,
		    desc_type);
	    
	    /* add a dependence graph vertex */
	    if (Enclosing_Do_Loop(LWN_Get_Parent(pre_load)) != NULL) {
		Array_Dependence_Graph->Add_Vertex(pre_load);
	    }


	    /* copy LoadAhead() register value to Res_Reg() */
	    WN *copy = 
		e_info->Get_Res_Reg()->Simd_Preg_Stid(
		    load_ahead_preg->Simd_Preg_Ldid());
	    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), simd_expr,copy);
					       
	    /* increment the address */
	    OPCODE opc = OPCODE_make_op(OPR_ADD, align_addr->Type(), MTYPE_V);
	    
	    INT    inc    = MTYPE_byte_size(desc_type);
	    WN    *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, inc);
	    WN    *add    = LWN_CreateExp2(opc, align_addr->Simd_Preg_Ldid(),
					   inc_wn);
	    
	    WN    *stid   = align_addr->Simd_Preg_Stid(add);
	    LWN_Insert_Block_Before(LWN_Get_Parent(simd_expr), simd_expr,stid);
	    
	    /* generate another load ahead */
	    WN *load_ahead =
		simd->Generate_Load(
		    OPR_ILOAD,
		    desc_type,
		    simd_expr, 
		    align_addr->Simd_Preg_Ldid(),
		    0, 
		    load_ahead_preg,
		    desc_type);
	    
	    return load_ahead;
	}
    } else {
	/* constant register load */
	Is_True(simd_expr == WN_kid0(stmt), ("Mismatched load"));
	if (WN_operator(simd_expr) == OPR_CVT) {
	    simd_expr = WN_kid0(simd_expr);
	}
	Is_True(WN_operator(simd_expr) == OPR_ILOAD, ("Expecting iload"));
	
	/* allocate a coefficient register */
	Is_True(e_info->Coef_Reg() == NULL, 
		("Expect coefficient register to be empty"));
	
	TYPE_ID    coef_type = simd->Coeff_type();
	PREG_NUM   preg_num  = Create_Preg(coef_type, "coeff");
	SIMD_PREG *coef      = CXX_NEW(SIMD_PREG(coef_type, preg_num), Pool());
	
	e_info->Set_Coef_Reg(coef);
	IMEM_INFO *imem = e_info->IMem();

	/* copy the address */
	WN* addr = LWN_Copy_Tree(WN_kid0(simd_expr), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_kid0(simd_expr), addr, Du_Mgr);
	
	if (!imem || !imem->Is_Aligned()) {
	    /* The address is not aligned, need to *
	     * (1) preload alignment register         *
	     * (2) use alignment load                 */
	    
	    /* allocate an alignment register */
	    SIMD_PREG *align    = simd->Gen_Align_Symbol(e_info->Res_Type(),Pool());
	    SIMD_PREG *addr_reg = Generate_Align_Addr_Reg(simd, Pool());
	    
	    /* preload alignment register */
	    WN *loop = Enclosing_Do_Loop(LWN_Get_Parent(stmt));
	    Is_True(loop != NULL, ("Cannot find enclosing loop"));
	    
	    Replace_Ldid_With_Exp_Copy(WN_index(loop), addr,
				       WN_kid0(WN_start(loop)),
				       Du_Mgr, Array_Dependence_Graph);

	    INT inc = simd->Get_SIMD_Mem_Bytes_Scalar(e_info->Res_Type());
	    Is_True(inc != 0, ("unexpected increment amount"));
	    
	    WN *callNode = simd->Generate_LoadA_Prime(e_info->Res_Type(),
						      loop,
						      addr, addr_reg,
						      e_info->Get_Res_Reg(),
						      align);
	    
	    /* add a dependence graph vertex */
	    if (Enclosing_Do_Loop(LWN_Get_Parent(loop)) != NULL) {
	      Array_Dependence_Graph->Add_Vertex(callNode);
	    }
	    
	    /* alignment load */
	    callNode = simd->Generate_LoadA_Update(e_info->Res_Type(),
						   stmt,
						   addr_reg,
						   e_info->Get_Res_Reg(),
						   align, inc);

	    /* generate a WC (write coefficient register) */
	    e_info->Generate_Write_Coef(stmt, simd);

	    return callNode;

	} else { /* aligned load */

	    /* generate a load c */
	    OPCODE opc = OPCODE_make_op(OPR_ILOAD, coef_type, coef_type);
	    WN* iload  = LWN_CreateIload(opc, WN_offset(simd_expr),
					 MTYPE_To_TY(coef_type),
					 Make_Pointer_Type(MTYPE_To_TY(coef_type)),
					 addr);
#if 0
	    /* generate alias id */
	    Duplicate_alias_info(Alias_Mgr, e_info->Expr(), iload);
#endif

	    /* generate stid to hold the result of load c */
	    WN* stid     = coef->Simd_Preg_Stid(iload);
	    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
	    WN_linenum(stid) = LWN_Get_Linenum(stmt);

	    return iload;
	}

    }
}

WN*
SIMD_LOOP::Combine_Mulabc(SIMD_EINFO *e_info, WN *stmt, SIMD_INFO *simd,
			  INT imm, SIMD_EINFO *s1_info, SIMD_EINFO *s2_info)
{
    Is_True(s1_info->Load_Reuse() && s2_info->Load_Reuse(), 
	    ("Unexpected MULABC operands"));
    Is_True(WN_operator(stmt) == OPR_STID, ("Not a STID"));
    WN  *mac = WN_kid0(stmt);
    Is_True(WN_operator(mac) == OPR_INTRINSIC_OP, ("Not an MULA"));
    char macro_name1[64];
    char macro_name2[64];
    INT  bits = simd->Mul_In1_Bits();
    INTRINSIC intrin_id = WN_intrinsic(mac);
    Simd_Make_Vectra_Macro("MULA", "_0", bits, macro_name1);
    Simd_Make_Vectra_Macro("MULA", "_1", bits, macro_name2);
    
    Is_True(intrin_id == Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name1)) ||
	    intrin_id == Tie_Macro_Id_To_Intrinsic(tie_info->tie_macro_id(macro_name2)), ("Expecting MULA"));

    INT even_odd = 
	(intrin_id == Tie_Macro_Id_To_Intrinsic(
	    tie_info->tie_macro_id(macro_name1))) ? 0 : 1;

    OPERATOR op   = OPR_XMPY; /* to get MULABC */

    /* copy the original accumulator's DEF/USE */
    WN        *acc     = WN_kid0(WN_kid0(mac));
    Is_True(WN_operator(acc) == OPR_LDID, ("Unexpected accumulator"));
    WN        *new_acc = e_info->Get_Res_Reg(even_odd)->Simd_Preg_Ldid();
    LWN_Copy_Def_Use(acc, new_acc, Du_Mgr);
    
    SIMD_PREG *pair_0, *pair_1;
    SIMD_PREG *coef = NULL;
    if (s1_info->Coef_Reg()) {
	coef   = s1_info->Coef_Reg();
	Is_True(s2_info->Load_Ahead(), ("No load ahead operand in MULABC"));
	pair_0 = s2_info->Get_Res_Reg();
	pair_1 = s2_info->Load_Ahead();
    } else {
	Is_True(s2_info->Coef_Reg(), ("No coefficient in MULABC operand"));
	Is_True(s1_info->Load_Ahead(), ("No load ahead operand in MULABC"));
	coef   = s2_info->Coef_Reg();
	pair_0 = s1_info->Get_Res_Reg();
	pair_1 = s1_info->Load_Ahead();
    }
    if (imm == 0) {
	/* avoid the interlock in the first MULABC */
	pair_1 = pair_0;
    }

    WN *cst = WN_CreateIntconst(OPC_I4INTCONST, imm);
    WN *mulabc = 
	simd->Generate_Mac_Call(op, e_info->Get_Res_Reg(even_odd)->Type(),
				new_acc,
				coef->Simd_Preg_Ldid(),
				even_odd,
				pair_1->Simd_Preg_Ldid(),
				pair_0->Simd_Preg_Ldid(),
				cst);
    WN *sto = e_info->Get_Res_Reg(even_odd)->Simd_Preg_Stid(mulabc);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = WN_linenum(stmt);

    return mulabc;
}

/*---------------------------------------------------------------------------*
 * Generate last store for unaligned stores                                  *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Finalize_Unaligned_Store(WN *loop, SIMD_INFO *simd, WN *stmt)
{
    IMEM_GROUP_Array &imem_groups = IMem_Map().Groups();
    for (INT i = 0; i < imem_groups.Elements(); i++) {
	IMEM_GROUP *cur = imem_groups[i];
	if (cur->Is_Def() && !cur->Is_Reduction() &&
	    !cur->Is_Aligned() && !cur->Prime_In()) {
	    simd->Generate_StoreA_Flush(cur, stmt, /* ins_after */ true);
	}
    }
}

WN*
SIMD_LOOP::Generate_Sum_Reduction (TYPE_ID scalar_type, SIMD_PREG *vec_reg,
				   WN *block, INT64 line_number) {
    /* generate scalar = MOVAR(RADD(vec_reg)) */
    WN *apr[1];
    
    TYPE_ID vec_type = vec_reg->Type();
    INTRINSIC intrin_id = Simd_Info->Radd(vec_type, vec_type);
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, vec_type, MTYPE_V);
    
    apr[0] = LWN_CreateParm(vec_type, vec_reg->Simd_Preg_Ldid(),
			    MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
    WN *red = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);
    
    intrin_id = Simd_Info->Find_Movar(vec_type);
    intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, Promote_Type(scalar_type), MTYPE_V);
    apr[0]    = LWN_CreateParm(vec_type, red,
			       MTYPE_To_TY(vec_type), WN_PARM_BY_VALUE);
    WN *movar = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);
    
    return movar;
}


/*---------------------------------------------------------------------------*
 * Generate across elment reduction                                          *
 *---------------------------------------------------------------------------*/
WN *
SIMD_LOOP::Generate_Reduction(TYPE_ID scalar_type, SIMD_PREG *reg, OPERATOR op,
			      WN *block, INT64 line_number)
{
    /* reduction intrinsic */
    TYPE_ID   mtype  = reg->Type();
    INTRINSIC red_id = Simd_Info->Find_Function(op, mtype);
    OPCODE    red_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    
    /* select intrinsic */
    OPERATOR  op_sel = OPR_SELECT;
    INTRINSIC sel_id = Simd_Info->Find_Function(op_sel, mtype);
    OPCODE    sel_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);

    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(mtype);
    INT       length = Simd_Info->Get_SIMD_Width_SIMD(mtype);
    WN* apr[3];
    
    char new_name[64];
    sprintf(new_name, "%s_%d", "swap", name_counter++);
    PREG_NUM   preg_num = Create_Preg(mtype, new_name);
    SIMD_PREG *reg_swap = CXX_NEW(SIMD_PREG(mtype, preg_num), Pool());
    
    for (INT i = 1; i < length; i *= 2) {
	/* reg_swap = sel(reg, reg, sel_mask) */
	INT64 sel_mask = simd_sel->Sel_1032(i);
	SIMD_PREG *sel_preg = Create_Sel_Preg(sel_mask);
	apr[0] = LWN_CreateParm(mtype, reg->Simd_Preg_Ldid(),
				MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
	apr[1] = LWN_CreateParm(mtype, reg->Simd_Preg_Ldid(),
				MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
	apr[2] = LWN_CreateParm(sel_preg->Type(), sel_preg->Simd_Preg_Ldid(),
				MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
	WN *perm = LWN_Create_Intrinsic(sel_op, sel_id, 3, apr);

	WN *sto = reg_swap->Simd_Preg_Stid(perm);
	LWN_Insert_Block_Before(block, NULL, sto);
	WN_linenum(sto) = line_number;

	/* reg = red(reg, reg_swap) */
	apr[0] = LWN_CreateParm(mtype, reg->Simd_Preg_Ldid(),
				MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
	apr[1] = LWN_CreateParm(mtype, reg_swap->Simd_Preg_Ldid(),
				MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);

	WN *red = LWN_Create_Intrinsic(red_op, red_id, 2, apr);
	sto = reg->Simd_Preg_Stid(red);
	LWN_Insert_Block_Before(block, NULL, sto);
	WN_linenum(sto) = line_number;
    }

    /* generate scalar = MOVAR{16/32}(LDID(reg)) */
    INTRINSIC intrin_id = Simd_Info->Find_Movar(mtype);
    OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, Promote_Type(scalar_type), MTYPE_V);
    apr[0]    = LWN_CreateParm(mtype, reg->Simd_Preg_Ldid(),
			       MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    WN *red = LWN_Create_Intrinsic(intrin_op, intrin_id, 1, apr);
    return red;
}


/*---------------------------------------------------------------------------*
 * Generate across elment reduction and finalize the result                  *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Finalize_Scalar_Reduction(WN *loop, SIMD_INFO *simd, WN *stmt)
{
    INT simd_level        = Simd_Loop_Level();
    EINFO_Stack    &red_s = Scalar_Redu();
    WN             *block = WN_CreateBlock();
    for (int i = 0; i < red_s.Elements(); i++) {
	SIMD_EINFO *e_info   = red_s.Bottom_nth(i);
	INT         res_type = e_info->Res_Type();
	WN         *expr     = e_info->Expr();
	SIMD_PREG  *reg      = e_info->Pick_Simd_Reg(Eprop_Donot_Care);
	SIMD_PREG  *reg1     = e_info->Pick_Simd_Reg(Eprop_Donot_Care,1);
	IMEM_INFO  *imem     = e_info->IMem();

	INT64 line_number      = LWN_Get_Linenum(stmt);
	REDUCTION_TYPE red_type = red_manager->Which_Reduction(expr);
	TYPE_ID        mtype    = reg->Type();
	WN *res = NULL;
	
	if (red_type == RED_ADD) {
	  if (reg1 && reg1 != reg) { /* generate reg = ADD(reg, reg1) */
	    WN *r_ldid = reg->Simd_Preg_Ldid();
	    WN *r1_ldid = reg1->Simd_Preg_Ldid();
	    WN *add = simd->Generate_Binary_Call(NULL, OPR_ADD, reg,
						 r_ldid, r1_ldid);
	    WN *st_r = reg->Simd_Preg_Stid(add);
	    LWN_Insert_Block_Before(block, NULL, st_r);
	  }
	  
	  WN *red_val = Generate_Sum_Reduction(res_type, reg,
					       block, line_number);

	  WN *ldid = NULL;
	  OPERATOR red_op = REDUCTION_TYPE_to_OPERATOR(red_type);
	  OPCODE   opc    = OPCODE_make_op(red_op, WN_rtype(expr), MTYPE_V);
	  
	  if (WN_operator(expr) == OPR_ILOAD) {
	    Is_True(imem, ("Null IMEM_INFO for reduction on array"));
	    ldid = LWN_Copy_Tree(expr, TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(expr, ldid, Du_Mgr);
	    if (simd_level > 0)
	      imem->Imem_Offset()->Parent_Group()->V_Loads()->AddElement(ldid);
	  } else {
	    OPCODE opc = OPCODE_make_op(OPR_LDID,WN_rtype(expr),WN_desc(expr));
	    ldid  = LWN_CreateLdid(opc, expr);
	    
	    /* update the DU chains */
	    LWN_Copy_Def_Use(expr, ldid, Du_Mgr);
	    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ldid);
	    if (def_list && def_list->Loop_stmt() == loop) {
	      def_list->Set_loop_stmt(Enclosing_Do_Loop(LWN_Get_Parent(loop)));
	    }
	  }
	  
	  res   = LWN_CreateExp2(opc, ldid, red_val);
	} else if (red_type == RED_MAX || red_type == RED_MIN) {
	  /* generate general reduction */
	  res = Generate_Reduction(res_type, reg,
				   REDUCTION_TYPE_to_OPERATOR(red_type),
				   block, line_number);
	} else {
	    FmtAssert(0, ("Unexpected reduction type"));
	}
	
	WN *stid  = NULL;
	if (WN_operator(expr) == OPR_ILOAD) {
	  WN *istore = imem->Get_IStore();
	  WN *addr = LWN_Copy_Tree(WN_kid1(istore), TRUE, LNO_Info_Map);
	  LWN_Copy_Def_Use(WN_kid1(istore), addr, Du_Mgr);
	  stid = 
	    LWN_CreateIstore( OPCODE_make_op(OPR_ISTORE, MTYPE_V, WN_desc(istore)),
			      0, Make_Pointer_Type(MTYPE_To_TY(WN_desc(istore))),
			      res, addr);
	  Copy_alias_info(Alias_Mgr, istore, stid);
	  
	  if (simd_level > 0)
	    imem->Imem_Offset()->Parent_Group()->V_Stores()->AddElement(stid);
	} else {
	  OPCODE opc    = OPCODE_make_op(OPR_STID, MTYPE_V, WN_desc(expr));
	  stid   = LWN_CreateStid(opc, expr, res);
	}
	LWN_Insert_Block_Before(block, NULL, stid);
	WN_linenum(stid) = line_number;
	
	/* update DU information */
	Du_Mgr->Create_Use_List(stid);
	Du_Mgr->Du_Set_Incomplete(stid);
    }

    /* now move from 'block' to after loop */
    WN *kid = WN_last(block);
    WN *new_block = LWN_Get_Parent(stmt);
    while (kid) {
	WN *cur = kid;
	kid = WN_prev(kid);
	LWN_Extract_From_Block(block, cur);
	if (new_block) {
	    LWN_Insert_Block_After(new_block, stmt, cur);
	} else {
	    WN_next(cur) = WN_next(stmt);
	    if (WN_next(stmt)) {
		WN_prev(WN_next(stmt)) = cur;
	    }
	    WN_next(stmt) = cur;
	    WN_prev(cur) = stmt;
	}
    }
    WN_Delete(block);
}

/*---------------------------------------------------------------------------*
 * Replace scalar references in 'wn' with                                    *
 *    _vse[index - LB]                                                       *
 *---------------------------------------------------------------------------*/
WN *
SIMD_LOOP::Substitute_Scalar_Expansion_Ldid(WN *wn)
{
    Is_True(WN_operator(wn) == OPR_LDID, 
	    ("Substitute_Scalar_Expansion_Ldid: illegal input"));
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
    if (s_info && s_info->Expand()) {
	WN *loop = TD_Rem_Loop();
	WN *wn_start = WN_start(loop);
	WN *wn_step = WN_step(loop);
	OPCODE opc_ldid = 
	    OPCODE_make_op(OPR_LDID, Promote_Type(WN_desc(wn_start)),
			   WN_desc(wn_start));
	WN *ldid_idx = LWN_CreateLdid(opc_ldid, wn_start);
	Du_Mgr->Add_Def_Use(wn_start, ldid_idx);
	Du_Mgr->Add_Def_Use(wn_step, ldid_idx);

	WN *wn_init = LWN_Copy_Tree(WN_kid0(wn_start), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_kid0(wn_start), wn_init, Du_Mgr);

	OPCODE opc_sub = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	WN *offset_amount = LWN_CreateExp2(opc_sub, ldid_idx, wn_init);
	
	ST *st_array = s_info->SE_Array();
	Is_True(st_array, ("Substitute_Scalar_Expansion_Ldid: no SE ST"));
	TY_IDX st_array_ty = ST_type(st_array);
	TY_IDX st_array_ty_ptr = Make_Pointer_Type(st_array_ty);
	INT element_count  = V_Unroll_Factor(Simd_Info);
	TY_IDX  ele_ty     = TY_etype(st_array_ty);
	TY_IDX  ele_ty_ptr = Make_Pointer_Type(ele_ty);
	TYPE_ID mtype      = TY_mtype(ele_ty);

	OPCODE  op_lda     = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
	WN*     wn_lda     = WN_CreateLda(op_lda, 0, st_array_ty_ptr,st_array);
	WN*     wn_sz      = LWN_Make_Icon(MTYPE_U4, element_count);
	OPCODE  op_array   = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
	WN*     wn_array   = WN_Create(op_array, 3);
	WN_element_size(wn_array) = MTYPE_byte_size(mtype);
	WN_array_base(wn_array) = wn_lda;
	WN_array_index(wn_array, 0) = offset_amount;
	WN_array_dim(wn_array, 0) = wn_sz;
	LWN_Parentize(wn_array);
	
	OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, Promote_Type(mtype), 
					 mtype);
	WN* wn_iload = LWN_CreateIload(op_iload,0,ele_ty,ele_ty_ptr,wn_array);
	Create_lda_array_alias(Alias_Mgr, wn_lda, wn_iload);
	Array_Dependence_Graph->Add_Vertex(wn_iload);
	return wn_iload;
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Replace scalar references in 'wn' with                                    *
 *    _vse[index - LB] = ...                                                 *
 *---------------------------------------------------------------------------*/
WN *
SIMD_LOOP::Substitute_Scalar_Expansion_Stid(WN *wn)
{
    Is_True(WN_operator(wn) == OPR_STID, 
	    ("Substitute_Scalar_Expansion_Stid: illegal input"));
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
    if (s_info && s_info->Expand()) {
	WN *loop = TD_Rem_Loop();
	WN *wn_start = WN_start(loop);
	WN *wn_step = WN_step(loop);
	OPCODE opc_ldid = 
	    OPCODE_make_op(OPR_LDID, Promote_Type(WN_desc(wn_start)),
			   WN_desc(wn_start));
	WN *ldid_idx = LWN_CreateLdid(opc_ldid, wn_start);
	Du_Mgr->Add_Def_Use(wn_start, ldid_idx);
	Du_Mgr->Add_Def_Use(wn_step, ldid_idx);

	WN *wn_init = LWN_Copy_Tree(WN_kid0(wn_start), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_kid0(wn_start), wn_init, Du_Mgr);

	OPCODE opc_sub = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	WN *offset_amount = LWN_CreateExp2(opc_sub, ldid_idx, wn_init);
	
	ST *st_array = s_info->SE_Array();
	Is_True(st_array, ("Substitute_Scalar_Expansion_Ldid: no SE ST"));
	TY_IDX st_array_ty = ST_type(st_array);
	TY_IDX st_array_ty_ptr = Make_Pointer_Type(st_array_ty);
	INT element_count  = V_Unroll_Factor(Simd_Info);
	TY_IDX  ele_ty     = TY_etype(st_array_ty);
	TY_IDX  ele_ty_ptr = Make_Pointer_Type(ele_ty);
	TYPE_ID mtype      = TY_mtype(ele_ty);

	OPCODE  op_lda     = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
	WN*     wn_lda     = WN_CreateLda(op_lda, 0, st_array_ty_ptr,st_array);
	WN*     wn_sz      = LWN_Make_Icon(MTYPE_U4, element_count);
	OPCODE  op_array   = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
	WN*     wn_array   = WN_Create(op_array, 3);
	WN_element_size(wn_array) = MTYPE_byte_size(mtype);
	WN_array_base(wn_array) = wn_lda;
	WN_array_index(wn_array, 0) = offset_amount;
	WN_array_dim(wn_array, 0) = wn_sz;
	LWN_Parentize(wn_array);
	
	OPCODE op_istore = OPCODE_make_op(OPR_ISTORE, MTYPE_V, mtype);

	WN* wn_istore = LWN_CreateIstore(op_istore, 0, ele_ty_ptr, 
					WN_kid0(wn), wn_array);
	Create_lda_array_alias(Alias_Mgr, wn_lda, wn_istore);
	Array_Dependence_Graph->Add_Vertex(wn_istore);
	return wn_istore;
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Replace scalar references in 'wn' with                                    *
 *    _vse[index - LB]                                                       *
 *---------------------------------------------------------------------------*/
WN *
SIMD_LOOP::Substitute_Scalar_Expansion(WN *wn)
{
    OPERATOR oper = WN_operator(wn);

    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    WN *new_kid = Substitute_Scalar_Expansion(kid);
	    if (new_kid) {
		LWN_Insert_Block_Before(wn, kid, new_kid);
		LWN_Extract_From_Block(wn, kid);
		WN_kid0(kid) = NULL;
		LWN_Delete_Tree(kid);
		kid = new_kid;
	    }
	}
	return NULL;
    } else if (oper == OPR_DO_LOOP) {
	return Substitute_Scalar_Expansion(WN_do_body(wn));
    }
    
    if (oper == OPR_LDID) {
	/* process the LDID */
	return Substitute_Scalar_Expansion_Ldid(wn);

    } else if (oper == OPR_STID) {
	/* process the RHS */
	WN *new_rhs = Substitute_Scalar_Expansion(WN_kid0(wn));
	if (new_rhs) {
	    WN_kid0(wn) = new_rhs;
	    LWN_Set_Parent(new_rhs, wn);
	}

	/* process the STID */
	return Substitute_Scalar_Expansion_Stid(wn);

    } else if (oper == OPR_ILOAD) {
	/* skip the kid */
	return NULL;
    } else if (oper == OPR_ISTORE) {
	WN *new_rhs = Substitute_Scalar_Expansion(WN_kid0(wn));
	if (new_rhs) {
	    WN_kid0(wn) = new_rhs;
	    LWN_Set_Parent(new_rhs, wn);
	}
    } else {
	for (INT kid_idx=0; kid_idx<WN_kid_count(wn); kid_idx++) {
	    WN *new_kid = Substitute_Scalar_Expansion(WN_kid(wn, kid_idx));
	    if (new_kid) {
		WN_kid(wn, kid_idx) = new_kid;
		LWN_Set_Parent(new_kid, wn);
	    }
	}
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Store scalar vector to SE_Array if they are required                      *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Finalize_Scalar_Expansion(WN *loop, SIMD_INFO *simd, WN *stmt)
{
    WN    *block      = WN_CreateBlock();
    INT64 line_number = LWN_Get_Linenum(stmt);

    for (INT i = 0; i < S_Info_Stack().Elements(); i++) {
	SIMD_SCALAR *cur = S_Info_Stack().Bottom_nth(i);
	SIMD_EINFO *e_info = cur->E_Info();
	Is_True(e_info != NULL, 
		("Simd_Finalize_Scalar_Expansion: NULL EINFO"));

	if (cur->Expand()) {
	    ST     *st_array  = cur->SE_Array();
	    TY_IDX  arr_ty_ptr= Make_Pointer_Type(ST_type(st_array));
	    OPCODE  op_lda    = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
	    WN*     wn_lda    = WN_CreateLda(op_lda, 0, arr_ty_ptr, st_array);
	    
	    /* generate 'addr = lda(se_array) */
	    SIMD_PREG *addr_reg= Generate_Align_Addr_Reg(simd, Pool());
	    WN        *stid    = addr_reg->Simd_Preg_Stid(wn_lda);
	    LWN_Insert_Block_Before(block, NULL, stid);
	    WN_linenum(stid) = line_number;
	    
	    TYPE_ID   res_type   = e_info->Res_Type();
	    INT       inc_u      = simd->Get_SIMD_Mem_Bytes_Scalar(res_type);
	    INT       vec_length = Vec_Length(simd, res_type);

	    /* deinterleave the result if there is only interleaved version */
	    if (e_info->Interleaved() && !e_info->Need_Deinterleave() &&
		!e_info->Need_Interleave()) {

		/* deinterleave the result */
		e_info->Set_Need_Deinterleave();
		Generate_Simd_Reg_Info(e_info, simd);
		e_info->Simd_Deinterleave(NULL, simd, this, block);
	    }
		
	    /* generating updating stores */
	    for (INT j = 0; j < vec_length; j++) {
		SIMD_PREG *reg     = e_info->Pick_Simd_Reg(Eprop_Normal, j);
		WN        *lod_val = reg->Simd_Preg_Ldid();
		TYPE_ID    vtype   = simd->Get_SIMD_Mem_Type_Scalar(res_type);
		simd->Generate_Update_Store(reg->Type(), vtype, NULL, 
					    lod_val, addr_reg, j*inc_u, block);
	    }
	}
    }

    /* now move from 'block' to after loop */
    WN *kid = WN_last(block);
    WN *new_block = LWN_Get_Parent(stmt);
    while (kid) {
	WN *cur = kid;
	WN_linenum(kid) = line_number;
	kid = WN_prev(kid);
	LWN_Extract_From_Block(block, cur);
	if (new_block) {
	    LWN_Insert_Block_After(new_block, stmt, cur);
	} else {
	    WN_next(cur) = WN_next(stmt);
	    if (WN_next(stmt)) {
		WN_prev(WN_next(stmt)) = cur;
	    }
	    WN_next(stmt) = cur;
	    WN_prev(cur) = stmt;
	}
    }
    WN_Delete(block);
}

#define SHIFT_TOP 31
/*---------------------------------------------------------------------------*
 * Generate a v_x = v_x >> shift_scale                                       *
 *---------------------------------------------------------------------------*/
WN*
SIMD_LOOP::Simd_Finalize_Shift_Scale(WN *loop, SIMD_INFO *simd, WN *stmt,
				     SHIFT_SCALE *ss)
{
    SIMD_EINFO *e_info      = ss->e_info;
    TYPE_ID     res_type    = e_info->Res_Type();
    INT64       line_number = LWN_Get_Linenum(stmt);
    WN         *block       = WN_CreateBlock();
    SIMD_PREG  *reg         = e_info->Get_Final_Reg();
    TYPE_ID     mtype       = reg->Type();
    INT vec_length          = Vec_Length(simd, res_type);

    WN *newExpr = NULL;
    WN *stid    = NULL;

    WN         *expr = ss->shift;
    OPERATOR    op   = WN_operator(expr);
    
    Is_True(op == OPR_ASHR || op == OPR_LSHR || op == OPR_SHL ||
	    op == OPR_DIV, ("Not a shift"));

    WN *kid1 = WN_kid1(expr);
    if (op == OPR_DIV || WN_operator(kid1) == OPR_INTCONST) {
	WN *amount = LWN_Copy_Tree(kid1);
	if (op == OPR_DIV) {
	    Is_True(WN_operator(kid1) == OPR_INTCONST, ("DIV by non-const"));
	    INT64 val = WN_const_val(kid1);
	    Is_True(val > 0 && 
		    IS_POWER_OF_2(val), ("DIV is not a shift right"));
	    INT shift_amount = 0;
	    while ( (val & 0x1) != 0x1) {
		val >>= 1;
		shift_amount++;
	    }
	    amount = WN_CreateIntconst(OPC_I4INTCONST, shift_amount);

	}
	for (INT i = 0; i < vec_length; i++) {
          SIMD_PREG *reg = e_info->Get_Final_Reg(i);
          if (i > 0) {
            if (e_info->Get_Final_Reg(0) == reg)
              continue;
            amount = LWN_Copy_Tree(amount);
          }
          
          newExpr = simd->Generate_Binary_Call(NULL, OPR_ASHR, reg, 
                                               reg->Simd_Preg_Ldid(),
                                               amount);
          stid = reg->Simd_Preg_Stid(newExpr);
          WN_linenum(stid) = line_number;
          LWN_Insert_Block_Before(block, NULL, stid);
	}
    } else { /* variable shift */
	/* extract the shift amount out and preserve the dependence
	   and invariant information */
	WN *shift_amount = LWN_Copy_Tree(kid1, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(kid1, shift_amount, Du_Mgr);
	Simd_Copy_Dependence(kid1, shift_amount);
	INVAR_TABLE *invar_table = Cur_Simd_Loop->Invar_Table();
	Simd_Copy_Invariant(kid1, shift_amount, invar_table);
	
	if (op == OPR_SHL) {
	    /* update the invariant table */
	    OPCODE opc = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	    WN *top    = WN_CreateIntconst(OPC_I4INTCONST, SHIFT_TOP);
	    shift_amount = LWN_CreateExp2(opc, top, shift_amount);
	    
	    // copy invariant info to the new expression to allow
	    // moving the WSAR instruction to its invariant level
	    Simd_Copy_Invariant_Top_Level(kid1, shift_amount, invar_table);
	}
	
	/* generate v_x = ldid (-1) */
	newExpr   = WN_Ldid(mtype, -1, Tie_Output_Volatile_Preg, 
			    Tie_Output_Volatile_Type);
	WN  *stid = reg->Simd_Preg_Stid(newExpr);
	WN_linenum(stid) = line_number;
	LWN_Insert_Block_Before(block, NULL, stid);

	Cur_Simd_Loop->Generate_VSAR_Amount(shift_amount, stid, simd);
	
	WN *vsar = WN_prev(stid);

	WN *shift = 
	    simd->Generate_SRL_Call(op, reg->Simd_Preg_Ldid(),
				    reg->Simd_Preg_Ldid());
	if (Enclosing_Do_Loop(LWN_Get_Parent(loop)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(shift);
	    Array_Dependence_Graph->Add_Vertex(vsar);
	}

	Du_Mgr->Add_Def_Use(shift, newExpr);
	LWN_Insert_Block_Before(block, stid, shift);
	WN_linenum(shift) = line_number;

	/* part 2 if necessary */
	if (simd->Get_SIMD_Width_Scalar(res_type) < V_Unroll_Factor(simd)) {
	    SIMD_PREG  *reg1 = e_info->Get_Final_Reg(1);
	    if (reg1 != reg) {
		/* generate v_x = ldid (-1) */
		newExpr   = WN_Ldid(mtype, -1, Tie_Output_Volatile_Preg, 
				    Tie_Output_Volatile_Type);
		stid = reg1->Simd_Preg_Stid(newExpr);
		WN_linenum(stid) = line_number;
		LWN_Insert_Block_Before(block, NULL, stid);

		shift = 
		    simd->Generate_SRL_Call(op, reg1->Simd_Preg_Ldid(),
					    reg1->Simd_Preg_Ldid());
		if (Enclosing_Do_Loop(LWN_Get_Parent(loop)) != NULL) {
		    Array_Dependence_Graph->Add_Vertex(shift);
		}

		Du_Mgr->Add_Def_Use(shift, newExpr);
		LWN_Insert_Block_Before(block, stid, shift);
		WN_linenum(shift) = line_number;
	    }
	}
    }
    
    /* now move from 'block' to after loop */
    WN *last = WN_last(block);
    WN *kid  = last;
    WN *new_block = LWN_Get_Parent(stmt);
    while (kid) {
	WN *cur = kid;
	kid = WN_prev(kid);
	LWN_Extract_From_Block(block, cur);
	if (new_block) {
	    LWN_Insert_Block_After(new_block, stmt, cur);
	} else {
	    WN_next(cur) = WN_next(stmt);
	    if (WN_next(stmt)) {
		WN_prev(WN_next(stmt)) = cur;
	    }
	    WN_next(stmt) = cur;
	    WN_prev(cur) = stmt;
	}
    }
    WN_Delete(block);
    return last;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for an loop body                                     *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Transform_Loop_Body(WN *loop, SIMD_INFO *simd, WN *copy_loop)
{
    Is_True(WN_operator(loop) == OPR_DO_LOOP, ("Not a do loop"));
    Is_True(WN_operator(copy_loop) == OPR_DO_LOOP, ("Not a do loop"));

    Simd_Transform_Block(WN_do_body(loop), simd, WN_do_body(copy_loop));

    /* move store invariant candidate out */
    MOVE_CAND* move_cand = Get_Move_To(loop);
    if (move_cand) {
	for (INT i = 0; i < move_cand->Cand().Elements(); i++) {
	    MOVE_RANGE *mr = move_cand->Cand().Bottom_nth(i);
	    WN *wn_prev = NULL;
	    WN *block   = LWN_Get_Parent(copy_loop);
	    WN *cur     = mr->Move_End();
	    do {
		wn_prev = WN_prev(cur);
		LWN_Extract_From_Block(LWN_Get_Parent(cur), cur);
		if (block) {
		    LWN_Insert_Block_After(block, copy_loop, cur);
		} else {
		    WN_next(cur) = WN_next(copy_loop);
		    if (WN_next(copy_loop)) {
			WN_prev(WN_next(copy_loop)) = cur;
		    }
		    WN_next(copy_loop) = cur;
		    WN_prev(cur) = copy_loop;
		}
		cur = wn_prev;
	    } while (cur != mr->Move_After());
	}
    }
    
    if (loop == Simd_Loop()) {
	/* Generate last store for unaligned stores */
	Simd_Finalize_Unaligned_Store(loop, simd, copy_loop);

	/* Generate reduction finalization */
	Simd_Finalize_Scalar_Reduction(loop, simd, copy_loop);
    }

    WN *insert_after = copy_loop;
    /* generate shift scale */
    for (INT i = 0; i < Shift_Scale().Elements(); i++) {
	SHIFT_SCALE *ss = Shift_Scale().Bottom_nth(i);
	if (Enclosing_Do_Loop(ss->st_wn) == loop) {
	    /* Generate shift scale if the are move out */
	    insert_after=Simd_Finalize_Shift_Scale(loop, simd, copy_loop, ss);
	}
    }

}

E_Candidate::E_Candidate(EINFO_Stack *can) : 
    _candidate(can), _pos(0), _cur_wn(NULL), _cur_einfo(NULL) {
    if (_pos < _candidate->Elements()) {
	_cur_einfo = _candidate->Bottom_nth(_pos);
	_cur_wn = _cur_einfo->Simd_Expr();
	
	Is_True(_cur_wn && 
		(_cur_einfo->Load_Reuse() || _cur_einfo->Mulabc()),
		("Unexpected EINFO properties"));
    }
}

/*---------------------------------------------------------------------------*
 * Get next candidate                                                        *
 *---------------------------------------------------------------------------*/
WN* 
E_Candidate::Get_Next(void) 
{
    if (_pos < _candidate->Elements()) {
	Is_True(_cur_einfo && _cur_wn, ("NULL _cur_einfo or _cur_wn"));
	if (_cur_wn == _cur_einfo->Simd_Expr(0) &&
	    _cur_einfo->Simd_Exprs()->Elements() == 2) {
	    Is_True(_cur_einfo->Mulabc(), ("Expect mulabc"));
	    _cur_wn = _cur_einfo->Simd_Expr(1);
	} else {
	    _pos++;
	    if (_pos < _candidate->Elements()) {
		_cur_einfo = _candidate->Bottom_nth(_pos);
		_cur_wn = _cur_einfo->Simd_Expr(0);
	    } else {
		_cur_einfo = NULL;
		_cur_wn = NULL;
	    }
	}
    } else {
	Is_True(_cur_wn == NULL && _cur_einfo == NULL,
		("_cur_wn != NULL && _cur_einfo != NULL"));
    }
    return _cur_wn;
}

void    
SIMD_LOOP::Operator_Combine_Unrolled_Loop(WN **bodies,
					  INT u,
					  SIMD_INFO *simd, 
					  E_Candidate &cand,
					  WN **next_bodies)
{
    WN *cand_wn = cand.Cur_Wn();
    if (cand_wn == NULL) {
	return;
    }

    WN *cand_stmt = (OPCODE_is_stmt(WN_opcode(cand_wn))) ? 
	cand_wn : LWN_Get_Parent(cand_wn);
    Is_True(OPCODE_is_stmt(WN_opcode(cand_stmt)), ("Not a statement"));
    
    if (cand_stmt == bodies[0]) {
	/* transform load ahead and/or MULABC */
	SIMD_EINFO *e_info = cand.Cur_EInfo();
	Is_True(e_info, ("NULL SIMD_EINFO"));
	if (e_info->Load_Reuse()) {
	    bool is_call = OPCODE_is_call(WN_opcode(bodies[0]));
	    ARRAY_DIRECTED_GRAPH16 *adg = Array_Dependence_Graph;

	    WN       *new_ld = Combine_Load(e_info, bodies[0], simd);
	    VINDEX16  newv   = Array_Dependence_Graph->Add_Vertex(new_ld);

	    for (INT i = 0; i < u; i++) {

		/* Update dependences */
		WN *cur_wn = is_call ? bodies[i] : WN_kid0(bodies[i]);
		if (WN_operator(cur_wn) == OPR_CVT) {
		    cur_wn = WN_kid0(cur_wn);
		}
		     
		VINDEX16 v = adg->Get_Vertex(cur_wn);
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

		/* delete the tree */
		LWN_Delete_Tree(bodies[i]);
		
		if (is_call) {
		    /* delete the loads from LV16A_IU results
		       stid (ldid -1)
		       stid (ldid -2)                
		       stid (ldid -3) */
		    WN *next = WN_next(bodies[i]);
		    WN *nn   = WN_next(next);
		    WN *nnn  = WN_next(nn);
		    next_bodies[i] = WN_next(nnn);
		    LWN_Delete_Tree(next);
		    LWN_Delete_Tree(nn);
		    LWN_Delete_Tree(nnn);

		    if (i != 0) {
			for (INT k = 0; k < 5; k++) {
			    /* delete the preload LV16A_IU and preload addr */
			    WN *prev = WN_prev(next_bodies[i]);
			    LWN_Delete_Tree(prev);
			}
		    }
		}
	    }

	} else {
	    Is_True(e_info->Mulabc(), ("Unknown operation"));
	    for (INT i = 0; i < u; i++) {
		WN *mulabc =
		    Combine_Mulabc(e_info, bodies[i], simd, i,
				   e_info->Kid_EInfo(0), e_info->Kid_EInfo(1));

		/* delete the old tree */
		LWN_Delete_Tree(bodies[i]);
	    }
	}
	cand.Get_Next();
	return;
    }

    if (bodies[0]) {
	OPCODE opc = WN_opcode(bodies[0]);
	if (opc == OPC_BLOCK) {
	    WN **next_bodies = CXX_NEW_ARRAY(WN*, u, Pool());
	    WN **new_bodies = CXX_NEW_ARRAY(WN*, u, Pool());
	    for (INT i=0; i<u; i++) {
		next_bodies[i] = WN_first(bodies[i]);
	    }
	    while (next_bodies[0]) {
		for (INT i = 0; i < u; i++) {
		    /* get the next stmt before transformation,
		       because current stmt may be deleted in the trans. */
		    new_bodies[i] = next_bodies[i];
		    next_bodies[i] = WN_next(next_bodies[i]);
		}
		Operator_Combine_Unrolled_Loop(new_bodies, u, simd, cand,
					       next_bodies);
	    }
	} else if (OPCODE_is_scf(WN_opcode(bodies[0])) &&
		   WN_kid_count(bodies[0])) {
	    WN **new_bodies = CXX_NEW_ARRAY(WN *,u,&LNO_local_pool);
	    for (INT kidno=0; kidno<WN_kid_count(bodies[0]); kidno++) {
		for (INT i=0; i<u; i++) {
		    new_bodies[i] = WN_kid(bodies[i], kidno);
		}
		Operator_Combine_Unrolled_Loop(new_bodies, u, simd, cand);
	    }
	}
    }
}

/*---------------------------------------------------------------------------*
 * Combine LV/LS to load ahead                                               *
 * Combine MULA to MULABC                                                    *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Operator_Combine_Unrolled_Loop(WN **unroll_body, 
					  INT u,
					  SIMD_INFO *simd,
					  INT level)
{
    Is_True(level < Num_Loops(), ("Cannot find current loop"));
    Is_True(Unroll_Candidate(level)->Elements() > 0, ("No unroll elements"));

    E_Candidate cand(Unroll_Candidate(level));
    Is_True(cand.Cur_Wn() != NULL, ("Cannot find combine candidates"));
    
    Operator_Combine_Unrolled_Loop(unroll_body, u, simd, cand); 
}

/*---------------------------------------------------------------------------*
 * Simd further unroll loop                                                  *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Further_Unroll_Loop(WN *loop, INT level, SIMD_INFO *simd,
			       EST_REGISTER_USAGE est_register_usage,
			       SX_INFO* pinfo,
			       INT pinfo_depth,
			       BOOL no_further_unroll,
			       SX_INFO** wdpinfo_ptr
			       )
{
    Is_True(level < Num_Loops(), ("Cannot find current loop"));
    Is_True(Unroll_Candidate(level)->Elements() > 0, ("No unroll elements"));
    
    WN*                       wdloop = NULL;
    SX_INFO*                  wdpinfo = NULL;
    HASH_TABLE<WN*,WN*>*      loop_map = NULL;
    INT                       u        = simd->Type1_Simd();
    
    INT64 iters = Iterations(loop, Pool());
    BOOL do_winddown = (iters < 0 || iters % u);
    
    if (do_winddown) {
	// if the unrolled fit, so does this.  Otherwise, who knows.
	EST_REGISTER_USAGE ru;
	ru.Set_Fits(est_register_usage.Fits());
	wdloop = Wind_Down(loop, u, FALSE, ru);
	
	DO_LOOP_INFO* dli = Get_Do_Loop_Info(loop);
	dli->Est_Max_Iterations_Index = u;
	dli->Est_Num_Iterations = u /2;
	
	loop_map = Make_Loop_Mapping(loop, wdloop, &SNL_local_pool);
	
	// If the lower bound and upper bound are constant, replace the lower
	// bound of the wind down loop. 
	WN* lower_bound = WN_kid0(WN_start(loop)); 
	WN* upper_bound = SNL_UBexp(WN_end(loop)); 
	if (WN_operator(lower_bound) == OPR_INTCONST 
	    && WN_operator(upper_bound) == OPR_INTCONST) { 

	    // Replace the lower bound of the wind down loop.
	    INT64 lb = WN_const_val(lower_bound); 
	    INT64 ub = WN_const_val(upper_bound); 
	    INT64 wdlb = lb + iters/u * u; 
	    
	    LWN_Delete_Tree(WN_kid0(WN_start(wdloop)));
	    WN* wd_lower_bound = LWN_Copy_Tree(lower_bound, TRUE, LNO_Info_Map); 
	    WN_kid0(WN_start(wdloop)) = wd_lower_bound; 
	    LWN_Copy_Frequency(WN_start(wdloop), wd_lower_bound);
	    LWN_Set_Parent(wd_lower_bound, WN_start(wdloop));
	    WN_const_val(wd_lower_bound) = wdlb; 
	}
	if ((wdloop && !no_further_unroll &&
	     u >= LNO_Outer_Unroll_Min_For_Further_Unroll) ||
	    wdpinfo_ptr) {
	    wdpinfo = CXX_NEW(SX_INFO(*pinfo, loop,
				      loop_map, &SNL_local_pool), 
			      &SNL_local_pool);
	} 
	
	// make blocked loop have upper bound of u-1 less, step of u;
	// unnecessary in !do_winddown case
	Increase_By(SNL_UBexp(WN_end(loop)), -(u-1), WN_end(loop));
    }

    // adjust step

    INT64  ostep = Step_Size(loop, u);
    FmtAssert(ostep == 1, ("Non-unit step %" LLD_FMT " for loop %s",
			   ostep, SYMBOL(WN_index(loop)).Name()));

    // Now unroll body (simd_u) with index increased 1 further each time.
    
    WN** unroll_body = CXX_NEW_ARRAY(WN*, u, Pool());
    unroll_body[0] = loop;
    LWN_Scale_Frequency(WN_end(loop), 1.0/u);
    LWN_Scale_Frequency(WN_step(loop), 1.0/u);

    INT i;
    for (i = 1; i < u; i++) {
	unroll_body[i] = LWN_Copy_Tree(loop);
	LWN_Scale_Frequency_Tree(unroll_body[i], 1.0/u);
    }

    /* update the dependence graph */
    if (!Array_Dependence_Graph->
	Unrolled_Dependences_Update(unroll_body, u, Do_Depth(loop))) {
	/* failed dependence update */
	for (i = 0; i < u; i++) {
	    LNO_Erase_Dg_From_Here_In(unroll_body[i], Array_Dependence_Graph);
	}
	Unmapped_Vertices_Here_Out(LWN_Get_Parent(loop)); 
    }

    /* update reduction */
    if (red_manager) {
	red_manager->Unroll_Update(unroll_body, u);
    }

    /* update DU */
    Unrolled_DU_Update(unroll_body, u, Do_Depth(loop));

    /* perform inner loop operator combining */
    Operator_Combine_Unrolled_Loop(unroll_body, u, simd, level);

    if (DGraph_Overflow()) {
	for (i = 0; i < u; i++) {
	    LNO_Erase_Dg_From_Here_In(unroll_body[i], Array_Dependence_Graph);
	}
	Unmapped_Vertices_Here_Out(LWN_Get_Parent(loop)); 
    }

    /* put the unrolled bodies back in */
    WN *body_0 = unroll_body[0];
    for (i = 1; i < u; i++) {
	WN* cur = unroll_body[i];
	WN* bdy = WN_do_body(cur);
	SNL_Add_Du_To_Index_Ldid(body_0, bdy, Du_Mgr, TRUE);
	WN_do_body(cur) = WN_CreateBlock();
	LWN_Insert_Block_Before(WN_do_body(body_0), NULL, bdy);
	LWN_Delete_Tree(cur);
    }

    /* add an final value assignment to the loop index variable after loop */
    Finalize_Index_Variable(loop, TRUE, FALSE);    
}

// in fusion.cxx 
extern SX_INFO* Pre_loop_peeling_simd(WN* in_loop, WN *stid,
				      SX_INFO *pinfo, WN* permloop[],
				      INT num_loops, MEM_POOL *pool,
				      INT peel_count);

/*---------------------------------------------------------------------------*
 * Simd loop peeling for store alignment                                     *
 *---------------------------------------------------------------------------*/
SX_INFO*
SIMD_LOOP::Simd_Loop_Peeling(WN *loop, SX_INFO *pinfo, 
			     WN* permloop[], SIMD_INFO *simd, MEM_POOL *pool,
			     INT simd_u)
{
    Is_True(Peel_Align(),("No need to peel!"));
    
    // get the memory access that needs to be aligned
    // note that because we don't rescan the loop between modeling and peeling,
    // if we need to vectorize a loop several times (main and wind-down), only
    // one of them can be peeled
    IMEM_INFO *imem = IMem_Map().Only_Write_Imem_Info();
    WN *ref = imem->Lod_Sto().Bottom_nth(0);
    
    WN *addr = NULL;
    INT wn_offset = WN_offset(ref);
    if (OPERATOR_is_load(WN_operator(ref))) {
	addr = LWN_Copy_Tree(WN_kid0(ref), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_kid0(ref), addr, Du_Mgr);
    } else {
	Is_True(OPERATOR_is_store(WN_operator(ref)), ("Invalid operator"));
	addr = LWN_Copy_Tree(WN_kid1(ref), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_kid1(ref), addr, Du_Mgr);
    }
    
    /* initial address out of the loop */
    Replace_Ldid_With_Exp_Copy(WN_index(loop), addr,
			       WN_kid0(WN_start(loop)),
			       Du_Mgr, Array_Dependence_Graph);
    
    /* address + offset */
    if (wn_offset != 0) {
	WN    *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, wn_offset);
	OPCODE opc    = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	addr          = LWN_CreateExp2(opc, addr, inc_wn);
    }
    
    /* peel amount = (-addr)&MASK, which is the same as
       peel amount = (ALIGN-(addr%ALIGN))%ALIGN, where
       ALIGN = simd->Simd_Reg_Bytes()
       MASK = ALIGN - 1 */
    INT       align = simd->Get_SIMD_Mem_Bytes_Scalar(WN_desc(ref));
    INT        mask = align - 1;
    
    OPCODE opc      = OPCODE_make_op(OPR_NEG, MTYPE_I4, MTYPE_V);
    WN *peel_amount = LWN_CreateExp1(opc,addr);
    
    WN     *wn_mask = WN_CreateIntconst(OPC_I4INTCONST, mask);
    opc             = OPCODE_make_op(OPR_BAND, MTYPE_I4, MTYPE_V);
    peel_amount     = LWN_CreateExp2(opc, peel_amount, wn_mask);

    // correct the peel amount by the element size
    INT        elm_sz = 
	imem->Imem_Offset()->Parent_Elem()->Elem_Byte_Size() >> 1;
    INT        shift = 0;
    while (elm_sz != 0) {
	elm_sz = elm_sz >> 1;
	shift++;
    }
    if (shift != 0) {
	opc             = OPCODE_make_op(OPR_ASHR, MTYPE_I4, MTYPE_V);
	WN     *wn_sft  = WN_CreateIntconst(OPC_I4INTCONST, shift);
	peel_amount     = LWN_CreateExp2(opc, peel_amount, wn_sft);
    }
    
    /* generate a 'preg = peel_amount' */
    PREG_NUM   preg = Create_Preg(MTYPE_I4, "peel"); 
    WN        *stid = WN_StidPreg(MTYPE_I4, preg, peel_amount);
    LWN_Set_Parent(peel_amount, stid);
    
    LWN_Insert_Block_Before(LWN_Get_Parent(loop), loop, stid);
    WN_linenum(stid) = LWN_Get_Linenum(loop);

    Is_True(loop==Simd_Loop(),("Bad SIMD loop to peel."));
    SX_INFO* new_pinfo =
	Pre_loop_peeling_simd(loop, stid, pinfo, permloop, Num_Loops(), pool,
			      align/2);
    loop = WN_next(loop);
    Is_True((loop && WN_operator(loop)==OPR_DO_LOOP),
	    ("Wrong peeling in SIMD."));
    Set_Simd_Loop(loop);
    Set_Peeled();
    return new_pinfo;
}

/*---------------------------------------------------------------------------*
 * Copy from one SCALAR_STACK to another                                     *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Update_Global_Scalar(SCALAR_STACK *f_st, SCALAR_STACK *t_r,
				     SCALAR_STACK *t_w)
{
    for (INT i=0; i<f_st->Elements(); i++) {
	SCALAR_NODE *cur = f_st->Bottom_nth(i);
	for (INT j=0; j<cur->Elements(); j++) {
	    WN *cur_wn = cur->Bottom_nth(j)->Wn;
	    if (WN_operator(cur_wn) == OPR_LDID) {
		t_r->Add_Scalar(cur_wn, 0);
	    } else {
		Is_True(WN_operator(cur_wn) == OPR_STID, 
			("Invalid operator"));
		t_w->Add_Scalar(cur_wn, 0);
	    }
	}
    }
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for backedge                                           *
 * (1) from writes to the reads                                              *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Add_Du(SCALAR_STACK *reads, SCALAR_STACK *writes)
{
    for (INT i=0; i<reads->Elements(); i++) {
	SCALAR_NODE *cur = reads->Bottom_nth(i);
	SYMBOL &symbol = cur->_scalar;
	for (INT k=0; k<writes->Elements(); k++) {
	    SCALAR_NODE *match = writes->Bottom_nth(k);
	    if (symbol == match->_scalar) {
		for (INT j=0; j<match->Elements(); j++) {
		    WN *wn_w = match->Bottom_nth(j)->Wn;
		    Is_True(WN_operator(wn_w) == OPR_STID,
			    ("Expecting an OPR_STID"));
		    for (INT l=0; l<cur->Elements(); l++) {
			WN *wn_r = cur->Bottom_nth(l)->Wn;
			Is_True(WN_operator(wn_r) == OPR_LDID,
			    ("Expecting an OPR_LDID"));
			Du_Mgr->Add_Def_Use(wn_w, wn_r);
		    }
		}
		break;
	    }
	}
    }
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for backedge                                           *
 * (1) from local writes to locally exposed reads                            *
 * (2) from local writes to inner loop exposed reads                         *
 * (3) from inner loop writes to locally exposed reads                       *
 * (4) from inner loop writes to the inner loop exposed reads                *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Build_Backedge_Du(SCALAR_STACK *rw, SCALAR_STACK *loop_r,
				  SCALAR_STACK *loop_w)
{
    for (INT i=0; i<rw->Elements(); i++) {
	SCALAR_NODE *cur = rw->Bottom_nth(i);
	WN *last_wn = cur->Top_nth(0)->Wn;
	SYMBOL &symbol = cur->_scalar;

	/* internal to rw */
	if (WN_operator(last_wn) == OPR_STID) {
	    /* add backedge DU to the reads in 'rw' */
	    for (INT j=0; j<cur->Elements()-1; j++) {
		WN *cur_wn = cur->Bottom_nth(j)->Wn;
		Is_True(WN_operator(cur_wn) == OPR_LDID,
			("Expecting an OPR_LDID"));
		Du_Mgr->Add_Def_Use(last_wn, cur_wn);
	    }

	    /* between rw and loop_r */
	    for (INT k=0; k<loop_r->Elements(); k++) {
		SCALAR_NODE *match = loop_r->Bottom_nth(k);
		if (symbol == match->_scalar) {
		    for (INT j=0; j<match->Elements(); j++) {
			WN *cur_wn = match->Bottom_nth(j)->Wn;
			Is_True(WN_operator(cur_wn) == OPR_LDID,
				("Expecting an OPR_LDID"));
			Du_Mgr->Add_Def_Use(last_wn, cur_wn);
		    }
		    break;
		}
	    }
	} else {
	    Is_True(WN_operator(last_wn) == OPR_LDID,
		    ("Expecting an OPR_LDID"));

	    /* between rw and loop_w */
	    for (INT k=0; k<loop_w->Elements(); k++) {
		SCALAR_NODE *match = loop_w->Bottom_nth(k);
		if (symbol == match->_scalar) {
		    for (INT j=0; j<match->Elements(); j++) {
			WN *cur_wn = match->Bottom_nth(j)->Wn;
			Is_True(WN_operator(cur_wn) == OPR_STID,
				("Expecting an OPR_STID"));
			Du_Mgr->Add_Def_Use(cur_wn, last_wn);
		    }
		    break;
		}
	    }
	}
    }

    /* between loop_r and loop_w */
    Simd_Add_Du(loop_r, loop_w);
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for WN:                                                *
 *  (1) From the last write of the same block                                *
 *  (2) From the exposed writes of any inner loop                            * 
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Build_Du_Chain(WN *wn, SCALAR_STACK *rw, SCALAR_STACK *loop_w)
{
    SYMBOL symbol(wn);
    SCALAR_REF sref(wn, 0);
    INT i;

    /* update local read/write */
    for (i=0; i<rw->Elements(); i++) {
	SCALAR_NODE *cur = rw->Bottom_nth(i);
	if (symbol == cur->_scalar) {
	    /* same scalar */
	    SCALAR_REF *last_ref = cur->Top_nth(0);
	    WN *last_wn = last_ref->Wn;
	    if (WN_operator(wn) == OPR_LDID) {
		if (WN_operator(last_wn) == OPR_LDID) {
		    /* both loads are exposed */
		    cur->_scalar_ref_stack->Push(sref);
		} else {
		    Is_True(WN_operator(last_wn) == OPR_STID, 
			    ("Invalid operator"));
		    /* link up the DU from STID to LDID */
		    Du_Mgr->Add_Def_Use(last_wn, wn);
		}
	    } else {
		Is_True(WN_operator(wn) == OPR_STID, ("Invalid operator"));
		if (WN_operator(last_wn) == OPR_LDID) {
		    /* never seen a write before */
		    cur->_scalar_ref_stack->Push(sref);
		} else {
		    Is_True(WN_operator(last_wn) == OPR_STID, 
			    ("Invalid operator"));
		    /* new write covers the old write */
		    cur->_scalar_ref_stack->Settop(sref);
		}
	    }
	    break;
	}
    }

    /* new symbol */
    if (i == rw->Elements()) {
	rw->Add_Scalar(wn, 0);
    }

    /* Update the inner loop writes */
    for (i=0; i<loop_w->Elements(); i++) {
	SCALAR_NODE *cur = loop_w->Bottom_nth(i);
	if (cur->Elements()>0 && symbol == cur->_scalar) {
	    /* same scalar */
	    SCALAR_REF *last_ref = cur->Top_nth(0);
	    WN *last_wn = last_ref->Wn;

	    if (WN_operator(wn) == OPR_LDID) {
		/* link up the DU from STID to LDID */
		for (INT j=0; j<cur->Elements(); j++) {
		    WN *last_wn = cur->Bottom_nth(j)->Wn;
		    Du_Mgr->Add_Def_Use(last_wn, wn);
		}
	    } else {
		Is_True(WN_operator(wn) == OPR_STID, ("Invalid operator"));
		/* all the inner loop writes are no longer exposed */
		for (INT j=0; j<cur->Elements(); j++) {
		    cur->_scalar_ref_stack->Clear();
		}
	    }
	    break;
	}
    }

}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for WN                                                 *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Simd_Build_Du_Info_Wn(WN *wn, SCALAR_STACK *rw, 
				 SCALAR_STACK *loop_w)
{
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
	WN *kid = WN_kid(wn, kidno);
	Simd_Build_Du_Info_Wn(kid, rw, loop_w);
    }
	
    if ((WN_operator(wn) == OPR_LDID || WN_operator(wn) == OPR_STID) &&
	WN_Is_Simd_Preg(wn)) {
	Simd_Build_Du_Chain(wn, rw, loop_w);
    }
}

/*---------------------------------------------------------------------------*
 * Add DU chains to loop_read_c if it is covered by local_rw,                *
 * otherwise, copy over to the loop_reads                                    *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Simd_Exclude_Covered_Read(SCALAR_STACK *local_rw, 
				     SCALAR_STACK *loop_reads_c, 
				     SCALAR_STACK *loop_reads)
{
    for (INT i=0; i<loop_reads_c->Elements(); i++) {
	SCALAR_NODE *cur_r = loop_reads_c->Bottom_nth(i);
	SYMBOL &symbol = cur_r->_scalar;
	INT j;
	for (j=0; j<local_rw->Elements(); j++) {
	    SCALAR_NODE *cur_rw = local_rw->Bottom_nth(j);
	    if (symbol == cur_rw->_scalar) {
		/* same scalar */
		SCALAR_REF *last_ref = cur_rw->Top_nth(0);
		WN *last_wn = last_ref->Wn;
		if (WN_operator(last_wn) == OPR_LDID) {
		    /* there is no write for symbol, copy over to reads */
		    for (INT k=0; k<cur_r->Elements(); k++) {
			WN* cur_r_wn = cur_r->Bottom_nth(k)->Wn;
			loop_reads->Add_Scalar(cur_r_wn, 0);
		    }
		} else {
		    Is_True(WN_operator(last_wn) == OPR_STID, 
			    ("Invalid operator"));
		    /* link up the DU from the STID to LDIDs */
		    for (INT k=0; k<cur_r->Elements(); k++) {
			WN* cur_r_wn = cur_r->Bottom_nth(k)->Wn;
			Du_Mgr->Add_Def_Use(last_wn, cur_r_wn);
		    }
		}
		break;
	    }
	}

	/* no matching local_rw, copy over to reads */
	if (j == local_rw->Elements()) {
	    for (INT k=0; k<cur_r->Elements(); k++) {
		WN* cur_r_wn = cur_r->Bottom_nth(k)->Wn;
		loop_reads->Add_Scalar(cur_r_wn, 0);
	    }
	}
    }
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for block                                              *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Simd_Build_Du_Info_Block(WN *blk, SCALAR_STACK *reads,
				    SCALAR_STACK *writes)
{
    /* inner loop read/write */
    SCALAR_STACK loop_reads(&SIMD_local_pool);
    SCALAR_STACK loop_writes(&SIMD_local_pool);
    SCALAR_STACK loop_reads_c(&SIMD_local_pool);
    SCALAR_STACK loop_writes_c(&SIMD_local_pool);

    /* IF statement read/write */
    SCALAR_STACK then_r(&SIMD_local_pool);
    SCALAR_STACK else_r(&SIMD_local_pool);
    SCALAR_STACK then_w(&SIMD_local_pool);
    SCALAR_STACK else_w(&SIMD_local_pool);

    /* local read/write excluding the inner loops */
    SCALAR_STACK local_rw(&SIMD_local_pool);
    
    for (WN *stmt = WN_first(blk); stmt; stmt = WN_next(stmt)) {
	if (WN_operator(stmt) == OPR_DO_LOOP) {
	    /* compute the loop read/write */
	    Simd_Build_Du_Info_Loop(stmt, &loop_reads_c, &loop_writes_c);

	    /* from early inner loop writes to the loop_reads_c */
	    Simd_Add_Du(&loop_reads_c, &loop_writes);

	    /* copy the loop_writes_c to loop_writes */
	    Simd_Update_Global_Scalar(&loop_writes_c, &loop_reads, 
				      &loop_writes);

	    /* exclude the reads that are covered by the local writes */
	    Simd_Exclude_Covered_Read(&local_rw, &loop_reads_c, 
				      &loop_reads);
	    
	    /* clear the loop_read_c, loop_write_c */
	    loop_reads_c.Clear();
	    loop_writes_c.Clear();
	} else if (WN_operator(stmt) == OPR_IF) {
	    Simd_Build_Du_Info_Wn(stmt, &local_rw, &loop_writes);

	    /* local read/write excluding the inner loops */
	    Simd_Build_Du_Info_Block(WN_then(stmt), &then_r, &then_w);
	    Simd_Build_Du_Info_Block(WN_else(stmt), &else_r, &else_w);

	    /* from early inner loop writes to the then/else reads */
	    Simd_Add_Du(&then_r, &loop_writes);
	    Simd_Add_Du(&else_r, &loop_writes);
	    
	    /* exclude the reads that are covered by the local writes */
	    Simd_Exclude_Covered_Read(&local_rw, &then_r, &loop_reads);
	    Simd_Exclude_Covered_Read(&local_rw, &else_r, &loop_reads);

	    /* copy the loop_writes_c to loop_writes */
	    Simd_Update_Global_Scalar(&then_w, &loop_reads, &loop_writes);
	    Simd_Update_Global_Scalar(&else_w, &loop_reads, &loop_writes);
	    
	    /* clear the IF read/write */
	    then_r.Clear();
	    then_w.Clear();
	    else_r.Clear();
	    else_w.Clear();
	} else {
	    /* loop writes removed if they are covered by new local writes */
	    Simd_Build_Du_Info_Wn(stmt, &local_rw, &loop_writes);
	}
    }

    if (WN_operator(LWN_Get_Parent(blk)) == OPR_DO_LOOP) {
	/* linkup the DU chains from the backedge */
	Simd_Build_Backedge_Du(&local_rw, &loop_reads, &loop_writes);
    }
    
    /* copy the local writes/reads to the global writes/reads */
    Simd_Update_Global_Scalar(&local_rw, reads, writes);
    Simd_Update_Global_Scalar(&loop_reads, reads, writes);
    Simd_Update_Global_Scalar(&loop_writes, reads, writes);
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for the loop                                           *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Build_Du_Info_Loop(WN *loop, SCALAR_STACK *reads,
				   SCALAR_STACK *writes)
{
    Is_True(WN_operator(loop) == OPR_DO_LOOP, ("Not a do loop"));
    SCALAR_STACK loop_reads(&SIMD_local_pool);
    SCALAR_STACK loop_writes(&SIMD_local_pool);

    /* Build DU chains for a block */
    Simd_Build_Du_Info_Block(WN_do_body(loop), reads, writes);
}

/*---------------------------------------------------------------------------*
 * Simd add UD chains for the new SIMD symbols                               *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Build_Du_Info(WN *parent)
{
    MEM_POOL_Push(&SIMD_local_pool);
    {
	SCALAR_STACK reads(&SIMD_local_pool);
	SCALAR_STACK writes(&SIMD_local_pool);

	Simd_Build_Du_Info_Block(parent, &reads, &writes);
    }
    MEM_POOL_Pop(&SIMD_local_pool);
}

/*---------------------------------------------------------------------------*
 * Simd code generation for a loop                                           *
 *---------------------------------------------------------------------------*/
void 
SIMD_LOOP::Simd_Transform_Loop(WN *loop, SIMD_INFO *simd, WN *copy_loop)
{
    IMem_Map().Split_Group();
    IMem_Map().Split_Group_No_Vsel();
    IMem_Map().Setup_Index_Value();
    IMem_Map().Allocate_Regs();
    
    /* transform the loop body */
    Simd_Transform_Loop_Body(loop, simd, copy_loop);
    if (Trapezoidal() && loop != Simd_Loop()) {
        // DEM: I moved out call Simd_Finalize_Scalar_Expansion
	// I believe that this call is only called on simd loops
	// so don't need it for this case
    	FmtAssert(0,
	      ("Need to call Simd_Finalize_Scalar_Expansion."));
    } 
    
    /* move the assignment to VSAR out of the loop if possible */
    if (VSAR_Cnt() == 1 && Invariant_In_Simd_Loop(VSAR())) {
	WN *vsar_stmt = VSAR_Expr();
	Is_True(vsar_stmt, ("Cannot find the VSAR statement"));
	if (LWN_Get_Parent(vsar_stmt)) {
	    WN *pre_hd = LWN_Get_Parent(Simd_Loop());
	    LWN_Extract_From_Block(LWN_Get_Parent(vsar_stmt), vsar_stmt);
	    LWN_Insert_Block_Before(pre_hd, Simd_Loop(), vsar_stmt);
	    if (Enclosing_Do_Loop(pre_hd) == NULL) {
		VINDEX16 ver = Array_Dependence_Graph->Get_Vertex(vsar_stmt);
		if (ver) {
		    Array_Dependence_Graph->Delete_Vertex(ver);
		}
	    }
	}
    }
}

/*---------------------------------------------------------------------------*
 * Simd code generation for a constant                                       *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Constant(WN *expr, SIMD_INFO *simd, bool double_size)
{
    SIMD_EINFO  *e_info = Get_E_Info(expr);
    Is_True(e_info, ("NULL pointer"));

    TYPE_ID res_type = double_size ? simd->S32type() : e_info->Res_Type();

    SIMD_PREG  *simd_reg = Get_Constant(expr, res_type, simd);
    
    INT vec_length = Vec_Length(simd, res_type);

    /* copy simd_reg to all required simd registers since they are the same */
    e_info->Duplicate_To_All(vec_length, simd_reg);
    return e_info;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for UNARY                                            *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Unary(WN *expr, WN *stmt, SIMD_INFO *simd)
{
    SIMD_EINFO  *e_info   = Get_E_Info(expr);
    Is_True(e_info, ("No SIMD_EINFO"));

    /* generate SIMD registers */
    Generate_Simd_Reg_Info(e_info, simd);

    /* generate a STO of 'expr' result into vector registers */
    SIMD_EINFO *c_info  = Simd_Transform(WN_kid0(expr), stmt, simd);
    for (INT i = 0; i < e_info->Reg_Count(); i++) {
	e_info->Generate_Simd_Expr(true, expr, stmt, c_info, simd, NULL, i);
    }
    
    /* interleave/deinterleave if required */
    e_info->Simd_Post_Processing(stmt, simd, this);

    return e_info;
}

/*---------------------------------------------------------------------------*
 * Assign shift_amount to VSAR before stmt                                   *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Generate_VSAR_Amount(WN *shift_amount, WN *stmt, SIMD_INFO *simd)
{
    WN *shift = VSAR_Expr();
    /* set shift amount register */
    if (shift == NULL || !Tree_Equiv(VSAR(), shift_amount)) {
	shift = simd->Initialize_VSAR(stmt, shift_amount);
	Set_VSAR(shift_amount);
	Set_VSAR_Expr(shift);
    } else {
	LWN_Delete_Tree(shift_amount);
    }
}

/*---------------------------------------------------------------------------*
 * Simd code generation for CVT/CVTL                                         *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd)
{
    SIMD_EINFO  *e_info   = Get_E_Info(expr);
    Is_True(e_info, ("No SIMD_EINFO"));

    /* generate SIMD registers */
    Generate_Simd_Reg_Info(e_info, simd);

    /* generate a STO of 'expr' result into vector register */
    WN         *source       = WN_kid0(expr);
    SIMD_EINFO *c_info       = Get_E_Info(source);
    bool        need_pack    = (MTYPE_bit_size(c_info->Res_Type()) >=
				2 * MTYPE_bit_size(e_info->Res_Type()));
    EINFO_PROP  prop         = e_info->Get_Required_Operand_Prop();

    WN         *shift_amount = NULL;

    if (need_pack) {
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
		/* extract the shift amount out to preserve all the dependence,
		   invariant, and other information */
		WN *copy = LWN_Copy_Tree(shift_amount, TRUE, LNO_Info_Map);
		LWN_Copy_Def_Use(shift_amount, copy, Du_Mgr);
		WN_kid1(source) = copy;
		LWN_Set_Parent(shift_amount, NULL);
	    }

	    /* skip the shift */
	    source = WN_kid0(source);

	    /* before skip, copy the need_interleave, need_deinterleave */
	    SIMD_EINFO *c1_info = Get_E_Info(source);
	    if (c1_info) {
		if (c_info->Need_Interleave()) {
		    c1_info->Set_Need_Interleave();
		}
		if (c_info->Need_Deinterleave()) {
		    c1_info->Set_Need_Deinterleave();
		}
	    }
	}
	/* else shift_amount == NULL */
    }

    if (WN_operator(source) == OPR_INTCONST && !need_pack) {
	Is_True(2 * MTYPE_bit_size(c_info->Res_Type()) <=
		MTYPE_bit_size(e_info->Res_Type()), 
		("Simd_Transform_CVT: expect size expansion for constant"));
	c_info = Simd_Transform_Constant(source, simd, true);
    } else {
	/* transforming the source */
	c_info = Simd_Transform(source, stmt, simd);
    }

    if (need_pack) {
	/* generate packing */
	if (shift_amount != NULL) {
	    Generate_VSAR_Amount(shift_amount, stmt, simd);
	} else {
	    OPCODE opc   = OPCODE_make_op(OPR_INTCONST, MTYPE_I4, MTYPE_V);
	    shift_amount = WN_CreateIntconst(opc, 0);
	    Generate_VSAR_Amount(shift_amount, stmt, simd);
	}
	    
	WN      *pack = 
	    simd->Generate_Pack_Call(e_info->Get_Res_Reg()->Type(),
				     c_info->Gen_Lod_Reg(prop, 1),
				     c_info->Gen_Lod_Reg(prop, 0),
				     e_info->Get_Res_Reg()->Simd_Preg_Ldid());

	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(pack);
	}

	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, pack);
	WN_linenum(pack) = LWN_Get_Linenum(stmt);
	WN *parm_ldid = WN_Ldid(e_info->Get_Res_Reg()->Type(), -1, 
				Tie_Output_Volatile_Preg,
				Tie_Output_Volatile_Type);
	Du_Mgr->Add_Def_Use(pack, parm_ldid);

	WN      *sto = e_info->Get_Res_Reg()->Simd_Preg_Stid(parm_ldid);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
	WN_linenum(sto) = LWN_Get_Linenum(stmt);

    } else {/* generate double size conversion */
	Is_True(2 * MTYPE_bit_size(c_info->Res_Type()) <=
		MTYPE_bit_size(e_info->Res_Type()), 
		("Simd_Transform_CVT: expect size expansion"));
	
	if (WN_operator(source) == OPR_INTCONST) {
	    /* constant is already converted */
	    for (INT i = 0; i < e_info->Reg_Count(); i++) {
		WN *ldid = c_info->Gen_Lod_Reg(prop, i);
		WN *sto = e_info->Get_Res_Reg(i)->Simd_Preg_Stid(ldid);
		LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
		WN_linenum(sto) = LWN_Get_Linenum(stmt);
	    }
	} else {
	    for (INT i = 0; i < e_info->Reg_Count(); i++) {
		simd->Insert_Mul_Call(stmt, expr, e_info->Get_Res_Reg(i),
				      Get_Narrow_One(simd, this), // shared 1
				      c_info->Gen_Lod_Reg(prop), i);
	    }
	}
    }

    /* interleave/deinterleave if required */
    e_info->Simd_Post_Processing(stmt, simd, this);

    return e_info;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for BINARY                                           *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Binary(WN *expr, WN *stmt, SIMD_INFO *simd)
{
    SIMD_EINFO  *e_info   = Get_E_Info(expr);
    Is_True(e_info, ("No SIMD_EINFO"));

    OPERATOR     op       = WN_operator(expr);

    /* generate SIMD registers */
    Generate_Simd_Reg_Info(e_info, simd);

    /* generate a STO of 'expr' result into vector register */
    WN *child1   = WN_kid0(expr);
    WN *child2   = WN_kid1(expr);

    SIMD_EINFO *c1_info  = Get_E_Info(child1);
    Is_True(c1_info != NULL, ("Missing child1 EINFO"));

    SIMD_EINFO *c2_info  = NULL;

    if (e_info->Mula()) {
	/* skip shift scale if they are moved out */
	if (c1_info->Shift_Scale_Moved()) {
	    child1 = WN_kid0(child1);
	    c1_info = Get_E_Info(child1);
	}
	SIMD_EINFO *t2_info = Get_E_Info(child2);
	if (t2_info->Shift_Scale_Moved()) {
	    child2 = WN_kid0(child2);
	}

	Is_True((op == OPR_SUB || op == OPR_ADD) &&
		WN_operator(child2) == OPR_MPY, ("Invalid MULA form"));
	if (c1_info->Is_Round_Reg()) {
	    Is_True(op == OPR_ADD, ("Invalid MULR form"));

	    if (!Used_Round_Reg()) {
		WN *round_exp = child1;
		/* skip inserted CVT/CVTL */
		if (WN_operator(round_exp) == OPR_CVT ||
		    WN_operator(round_exp) == OPR_CVTL) {
		    SIMD_EINFO *g_info = Get_E_Info(round_exp);
		    if (g_info && g_info->Is_Round_Reg()) {
			round_exp = WN_kid0(round_exp);
		    }
		}
		    
		/* initialize rounding register outside of the loop */
		simd->Initialize_Rounding(round_exp, Simd_Loop());
		Set_Used_Round_Reg();
	    }
	    
	    e_info->Generate_Mula(expr, child2, stmt, c1_info, this, simd);

	    /* interleave/deinterleave if required */
	    e_info->Simd_Post_Processing(stmt, simd, this);

	    return e_info;
	}

	/* transform child1 */
	c1_info = Simd_Transform(child1, stmt, simd);

	/* generate mula/mulabc */
	if (e_info->Interleaved()) {
	    SIMD_PREGS_Copy(c1_info->Simd_IRegs(),e_info->Simd_IRegs());
	} else {
	    SIMD_PREGS_Copy(c1_info->Simd_Regs(), e_info->Simd_Regs());
	}
	e_info->Generate_Mula(expr, child2, stmt, c1_info, this, simd);

	/* interleave/deinterleave if required */
	e_info->Simd_Post_Processing(stmt, simd, this);

	return e_info;

    } else {
	/* transform child1 */
	c1_info = Simd_Transform(child1, stmt, simd);
    }

    /* shift scale moved out of the loop, skip the current expr */
    if ((op == OPR_ASHR || op == OPR_LSHR) && 
	e_info->Shift_Scale_Moved()) {
	e_info->Copy_Simd_Reg_Info(c1_info);
	return e_info;
    }

    if (op != OPR_ASHR && op != OPR_LSHR && op != OPR_SHL && op != OPR_DIV) {
	c2_info = Simd_Transform(child2, stmt, simd);
    } 
    
    /* Store directly to the reduction register. We have to do this when we've
       duplicated the same register for all results of an expression, otherwise
       subsequent assignment will overwrite the reduction value. */
    if (c1_info->Reduction() &&
        c1_info->Get_Res_Reg(0) == c1_info->Get_Res_Reg(1)) {
      if (WN_operator(child1) == OPR_LDID) {
        SYMBOL symbol(child1);
        if (Redu_Scalar().Find(symbol)) {
          e_info->Copy_Simd_Reg_Info(c1_info);
        }
      } else if (WN_operator(child1) == OPR_ILOAD) {
        if (c1_info->IMem()->Imem_Offset()->Parent_Group()->Is_Reduction())
        {
          e_info->Copy_Simd_Reg_Info(c1_info);
        }
      }
    } else if (c2_info && c2_info->Reduction() &&
               c2_info->Get_Res_Reg(0) == c2_info->Get_Res_Reg(1)) {
      if (WN_operator(child2) == OPR_LDID) {
        SYMBOL symbol(child2);
        if (Redu_Scalar().Find(symbol)) {
          e_info->Copy_Simd_Reg_Info(c2_info);
        }
      } else if (WN_operator(child2) == OPR_ILOAD) {
        if (c2_info->IMem()->Imem_Offset()->Parent_Group()->Is_Reduction())
        {
          e_info->Copy_Simd_Reg_Info(c2_info);
        }
      }
    }

    if (op == OPR_MPY) {
	/* Generate MUL */
	e_info->Generate_Mul_Expr(expr, stmt, c1_info, c2_info, simd);
	
	/* interleave/deinterleave if required */
	e_info->Simd_Post_Processing(stmt, simd, this);
	
	return e_info;
    }

    for (INT i = 0; i < e_info->Reg_Count(); i++) {
	e_info->Generate_Simd_Expr(false, expr, stmt, c1_info, simd, c2_info, i);
    }

    /* interleave/deinterleave if required */
    e_info->Simd_Post_Processing(stmt, simd, this);

    return e_info;
}

/*---------------------------------------------------------------------------*
 * Store motion candidate                                                    *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Add_Move_Candidate(WN *loop, WN *a, WN *e)
{
    MOVE_RANGE *mr = CXX_NEW(MOVE_RANGE(a, e), Pool());
    for (INT i = 0; i < Move_Cand().Elements(); i++) {
	MOVE_CAND *cur = Move_Cand().Bottom_nth(i);
	if (cur->To_Loop() == loop) {
	    cur->Cand().Push(mr);
	    return;
	}
    }
    MOVE_CAND *mc = CXX_NEW(MOVE_CAND(loop, Pool()), Pool());
    mc->Cand().Push(mr);
    Move_Cand().Push(mc);
}

MOVE_CAND*     
SIMD_LOOP::Get_Move_To(WN *loop) 
{
    for (INT i = 0; i < Move_Cand().Elements(); i++) {
	MOVE_CAND *cur = Move_Cand().Bottom_nth(i);
	if (cur->To_Loop() == loop) {
	    return cur;
	}
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for vector compare                                   *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Compare(WN *expr, WN *stmt, SIMD_INFO *simd,
				  bool &swap_branch)
{
    OPERATOR oper = WN_operator(expr);

    SIMD_EINFO *c1_info = Simd_Transform(WN_kid0(expr), stmt, simd);
    SIMD_EINFO *c2_info = Simd_Transform(WN_kid1(expr), stmt, simd);

    SIMD_EINFO  *e_info = Get_E_Info(expr);
    Is_True(e_info, ("SIMD_Transform_Select: No SIMD_EINFO"));
    
    /* generate SIMD registers */
    Generate_Simd_Reg_Info(e_info, simd);
    
    for (INT i = 0; i < e_info->Reg_Count(); i++) {
      e_info->Generate_Compare_Expr(oper, expr, stmt, c1_info, simd, c2_info, i,
                                    swap_branch);
    }
    
    return e_info;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for SELECT                                           *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Select(WN *expr, WN *stmt, SIMD_INFO *simd)
{
    SIMD_IF_CONV *if_conv = If_Conv();
    if (if_conv == NULL || if_conv->Finished()) {
	if_conv = If_Conv_Map().Find(expr);
	Set_If_Conv(if_conv);
    }
    Is_True(if_conv, ("SIMD_LOOP::Simd_Transform_Select: No SIMD_IF_CONV"));
    
    SIMD_EINFO *cond_info = NULL;
    if (if_conv->Is_First()) {
	/* generate vector compare */
	WN  *cond = WN_kid0(expr);
	bool swap_branch = false;
	cond_info = Simd_Transform_Compare(cond, stmt, simd, swap_branch);
	if_conv->Set_Cond_Info(cond_info);
	if_conv->Set_Swap_Branch(swap_branch);
    } else {
	cond_info = if_conv->Cond_Info();
    }

    if_conv->Inc_Processed();

    /* generate MOVT/MOVF */
    SIMD_EINFO *true_info  = Simd_Transform(WN_kid1(expr), stmt, simd);
    SIMD_EINFO *false_info = Simd_Transform(WN_kid2(expr), stmt, simd);

    if (if_conv->Swap_Branch()) {
      /* compare negated */
      SIMD_EINFO *t = true_info;
      true_info = false_info;
      false_info = t;
    }

    SIMD_EINFO  *e_info = Get_E_Info(expr);
    Is_True(e_info, ("SIMD_Transform_Select: No SIMD_EINFO"));

    /* generate SIMD registers */
    Generate_Simd_Reg_Info(e_info, simd);

    for (INT i = 0; i < e_info->Reg_Count(); i++) {
	e_info->Generate_CMov_Expr(expr, stmt, cond_info, true_info, false_info, simd, i);
    }

    /* interleave/deinterleave if required */
    e_info->Simd_Post_Processing(stmt, simd, this);
    return e_info;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for an EXPR                                          *
 *---------------------------------------------------------------------------*/
SIMD_EINFO * 
SIMD_LOOP::Simd_Transform(WN *expr, WN* stmt, SIMD_INFO *simd)
{
    OPERATOR        op            = WN_operator(expr);
    WN             *result        = NULL;
    SIMD_EINFO     *eInfo         = Get_E_Info(expr);
    Is_True(eInfo != NULL, ("eInfo == NULL"));
    TYPE_ID res_type              = eInfo->Res_Type();
    INT             unroll_factor = V_Unroll_Factor(simd);
    SIMD_EINFO     *c_info        = NULL;


    /* Reload from SIMD register if the expression is already transformed */
    if (eInfo->Get_Res_Reg() != NULL) {
	return eInfo;
    }

    /* special handling of invariant */
    if (op == OPR_CONST ||
        (!eInfo->Is_Vector() && !OPERATOR_is_leaf(op))) {
	if (OPERATOR_is_store(op)) {
	    /* move the whole statement */
	    WN *pop_st = Old_Stmt().Pop();
	    Is_True(pop_st == expr, 
		    ("SIMD_LOOP::Simd_Transform: original stmt mismatch"));
	    
	    expr = LWN_Extract_From_Block(expr);
	    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, expr);
	} else {
	    /* move the expression */
	    Replace_Wnexp_With_Exp_Copy(expr, expr, Du_Mgr, NULL,
					Array_Dependence_Graph, FALSE);

	    /* initialize SIMD register for invariant expression */
	    eInfo->Init_Simd_Reg_Invariant(stmt, this, simd, Pool());
	}
	return eInfo;
    }
	
    /* store or load  */
    if (OPERATOR_is_load(op)) {
	if (op == OPR_LDID) {
	    c_info = Simd_Transform_Scalar(expr, stmt, simd);
	} else {
	    Is_True(op == OPR_ILOAD, ("Not an indirect load"));
	    c_info = Simd_Transform_Indirect(expr, stmt, simd);
	}
	Is_True(c_info == eInfo, ("c_info != eInfo after transforming LOAD"));
	return eInfo;
    } else if (OPERATOR_is_store(op)) {
	/* visit the RHS */
	WN         *source = WN_kid0(expr);

	/* transforming the source */
	SIMD_EINFO *c1_info = Simd_Transform(source, stmt, simd);
	
	/* transform the store address */
	if (op == OPR_STID) {
	    c_info = Simd_Transform_Scalar(expr, stmt, simd);
	} else {
	    Is_True(op == OPR_ISTORE, ("Not an indirect store"));
	    c_info = Simd_Transform_Indirect(expr, stmt, simd);
	}
	Is_True(c_info == eInfo, ("c_info != eInfo after transforming STORE"));

	/* copy the value from c1_info to c_info */
	EINFO_PROP prop      = c_info->Get_Required_Operand_Prop();
	SIMD_PREG *newAddr   = NULL;
	SIMD_PREG *newSpreg  = NULL;
	WN        *newSource = NULL;
	WN        *sto       = NULL;
	
	for (INT i = 0; i < c_info->Reg_Count(); i++) {
	    newAddr   = c_info->Get_Res_Reg(i);
	    newSpreg  = c1_info->Pick_Simd_Reg(prop, i);
	    newSource = newSpreg->Simd_Preg_Ldid();
	    sto       = newAddr->Simd_Preg_Stid(newSource);
	    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
	    WN_linenum(sto) = LWN_Get_Linenum(stmt);
	}

	/* set up the range of invariant store motion */
	WN*        move_after = NULL;
	WN*        move_end   = NULL;

	/* before store back, deinterleave interleaved result */
	if (c_info->Store_Back() &&
	    !c_info->IMem()->Imem_Offset()->Parent_Group()->Is_Reduction() &&
	    c_info->Interleaved() && 
	    !c_info->Need_Deinterleave() && !c_info->Need_Interleave()) {
	    c_info->Set_Need_Deinterleave();
	    
	    /* the deinterleave is only used by the store so move it out */
	    move_after = WN_prev(stmt);
	}

	/* generate deinterleave before store back */
	if (c_info->Need_Deinterleave()) {
	    if (!c1_info->Need_Interleave()) {
		/* generate deinterleave */
		c_info->Simd_Deinterleave(stmt, simd, this);
	    } else { /* copy over from c1_info */
		for (INT i = 0; i < c_info->Simd_Regs()->Elements(); i++) {
		    newAddr   = c_info->Simd_Reg(i);
		    newSpreg  = c1_info->Simd_Reg(i);
		    newSource = newSpreg->Simd_Preg_Ldid();
		    sto       = newAddr->Simd_Preg_Stid(newSource);
		    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
		    WN_linenum(sto) = LWN_Get_Linenum(stmt);
		}
	    }
	} else if (c_info->Need_Interleave()) {
	    if (!c1_info->Interleaved() && !c1_info->Need_Interleave()) {
		c_info->Simd_Interleave(stmt, simd, this);
	    } else { /* copy over from c1_info */
		for (INT i = 0; i < c_info->Reg_Count(); i++) {
		    newAddr   = c_info->Simd_IReg(i);
		    newSpreg  = c1_info->Simd_IReg(i);
		    newSource = newSpreg->Simd_Preg_Ldid();
		    sto       = newAddr->Simd_Preg_Stid(newSource);
		    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
		    WN_linenum(sto) = LWN_Get_Linenum(stmt);
		}
	    }
	}

	/* store back if necessary */
	if (c_info->Store_Back()) {
	    Is_True(c_info->IMem()->Imem_Offset()->Parent_Group()->Is_Reduction() ||
		    !c_info->Interleaved() ||
		    c_info->Need_Deinterleave(),
		    ("Try to store back interleaved value"));

	    if  (c_info->IMem()->To_Loop() && move_after == NULL) {
		move_after = WN_prev(stmt);
	    }
		
	    IMEM_OFFSET *io = c_info->IMem()->Imem_Offset();
	    if (!io->Parent_Group()->Is_Reduction()) {
		io->Transform_Store(expr, stmt);
		
		if (move_after) {
		    move_end = WN_prev(stmt);
		    Is_True(move_after != move_end, ("No store to move"));
		    Add_Move_Candidate(c_info->IMem()->To_Loop(), 
				       move_after, move_end);
		}
	    }
	}
	return c_info;

    } else {  

        /* Visit the children in the expression tree                *
	   This is performed only if the parent of the expression   *
	   is not a load/store to avoid scalar expanding an address */
	switch (op) {
	    case OPR_INTCONST:
		return Simd_Transform_Constant(expr, simd);
		
	    case OPR_ABS:
	    case OPR_NEG:
	    case OPR_BNOT:
            case OPR_RECIP:
            case OPR_RSQRT:
		return Simd_Transform_Unary(expr, stmt, simd);

            case OPR_TRUNC:
	    case OPR_CVT:
	    case OPR_CVTL:
		return Simd_Transform_CVT(expr, stmt, simd);

	    case OPR_BXOR:
	    case OPR_BAND:
	    case OPR_BIOR:
	    case OPR_ADD:
	    case OPR_SUB:
	    case OPR_MAX:
	    case OPR_MIN:
	    case OPR_ASHR:
	    case OPR_LSHR:
	    case OPR_SHL:
	    case OPR_MPY:
	    case OPR_DIV:
		return Simd_Transform_Binary(expr, stmt, simd);
		
	    case OPR_SELECT:
		return Simd_Transform_Select(expr, stmt, simd);

	    default:
		Is_True(0, ("Unhandled operator"));
	}
    }
    return eInfo;
}

/*---------------------------------------------------------------------------*
 * Find the corresponding symbol for a constant.                             *
 * If none exists, generate a new SYMBOL to hold the constant                *
 *---------------------------------------------------------------------------*/
SIMD_PREG *
SIMD_LOOP::Get_Constant(WN *expr, TYPE_ID val_type, SIMD_INFO *simd)
{
    Is_True(WN_operator(expr) == OPR_INTCONST, ("Not an integer constant"));
    TYPE_ID preg_type = simd->Get_SIMD_Reg_Type_Scalar(val_type);
    INT val = WN_const_val(expr);
    SIMD_PREG *res = SIMD_CONST_Map_Get(Const_Map(),preg_type,val,
					val_type,Simd_Loop(),false,"V_con",Pool());
    return res;
}

/*---------------------------------------------------------------------------*
 * Create a SIMD_PREG for SELECT                                             *
 *---------------------------------------------------------------------------*/
SIMD_PREG *
SIMD_LOOP::Create_Sel_Preg (INT64 val)
{
    /* create a new SIMD_PREG */
    TYPE_ID mtype  = Simd_Info->Vsel_type();
    TYPE_ID val_type = 
	(MTYPE_byte_size(mtype) <= MTYPE_byte_size(MTYPE_U4)) ?
	MTYPE_U4 : MTYPE_U8;
    SIMD_PREG *res = SIMD_CONST_Map_Get(Const_Map(), mtype, val,
					val_type, Simd_Loop(), true, "sel", Pool());
    return res;
}

/*---------------------------------------------------------------------------*
 * Simd code generation for a direct load/store                              *
 * Return void_flags if the load/store is not from SIMD register, otherwise, *
 * return the appropriate SIMD type.                                         *
 *---------------------------------------------------------------------------*/
SIMD_EINFO * 
SIMD_LOOP::Simd_Transform_Scalar(WN *ls, WN *stmt, SIMD_INFO *simd)
{
    SYMBOL symbol(ls);
    SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
    Is_True(s_info, ("Cannot find SIMD_SCALAR"));
    SIMD_EINFO  *e_old  = s_info->E_Info();
    SIMD_EINFO  *e_info = Get_E_Info(ls);

    if (!s_info->Transformed()) {
	s_info->Set_Transformed();
	e_old = NULL;
    }

    bool is_reduction = false;
    /* setup the reduction scalar map */
    if (e_info->Reduction() && WN_operator(ls) == OPR_LDID) {
	DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ls);
	WN *def_loop = def_list->Loop_stmt();
	if (def_loop != NULL && Is_Descendent(Simd_Loop(), def_loop) &&
	    Redu_Scalar().Find(symbol) == NULL) {
	    Redu_Scalar().Enter(symbol, e_info);
	    Scalar_Redu().Push(e_info);
	    is_reduction = true;
	}
    }

    /* if the scalar is already processed before, using the existing address */
    if (e_old != NULL) {
	/* copy the old SIMD symbols to the new SIMD_EINFO */
	/* copy register info */
	e_info->Copy_Simd_Reg_Info(e_old);

	/* the e_old may not have all the required registers */
	Generate_Simd_Reg_Info(e_info, simd);
	
	if (WN_operator(ls) == OPR_LDID) {
	    /* interleave/deinterleave if required */
	    e_info->Simd_Post_Processing(stmt, simd, this);
	} else { /* OPR_STID */
	    /* we don't assume anything in the registers are valid */
	}
	s_info->Set_E_Info(e_info);
	return e_info;
    }

    /* the scalar is not processed */
    s_info->Set_E_Info(e_info);

    if (WN_operator(ls) == OPR_LDID) {
	if (is_reduction) {
	    bool generate_pair = false;
	    WN *parent = LWN_Get_Parent(ls);
	    SIMD_EINFO *p_info = Get_E_Info(parent);
	    if (p_info->Mula()) {
		WN *other_kid = 
		    (WN_kid0(parent) == ls) ? WN_kid1(parent) : WN_kid0(parent);
		WN *g_child = WN_kid0(other_kid);
		SIMD_EINFO *g_info = Get_E_Info(g_child);
		generate_pair = 
		    simd->Has_Paired_Mac(OPR_MPY,
			simd->Get_SIMD_Reg_Type_Scalar(p_info->Res_Type()),
			simd->Get_SIMD_Reg_Type_Scalar(g_info->Res_Type()));
	    }
	    if (generate_pair || (If_Ana_Map().Num_Entries() > 0)) {
              /* Use paired register, so cannot reuse */
              /* generate simd reg/ireg */
              Generate_Simd_Reg_Info(e_info, simd);
	    } else {
              /* use the same register for all elements */
              SIMD_PREG *res_preg = Gen_Symbol(simd, e_info->Res_Type(), Pool());
              INT vec_length = Vec_Length(simd, e_info->Res_Type());
              e_info->Duplicate_To_All(vec_length, res_preg);
	    }
	    if (red_manager->Which_Reduction(e_info->Expr()) == RED_ADD) {
		e_info->Init_Sum_Reduction(Simd_Loop());
	    } else {
		/* Load/store from the SIMD register.                    *
		   Scalar invariant: load from a SIMD register that is  *
		   initialized outside of the loop by a 'LS' assignment  */
		e_info->Init_Simd_Reg_Ldid(Simd_Loop(), this, simd, Pool());
	    }
	} else { /* not a reduction */
	    /* generate simd reg/ireg */
	    Generate_Simd_Reg_Info(e_info, simd);

	    /* Load/store from the SIMD register.                   *
	       Scalar invariant: load from a SIMD register that is  *
	       initialized outside of the loop by a 'LS' assignment */
	    e_info->Init_Simd_Reg_Ldid(Simd_Loop(), this, simd, Pool());

	    SIMD_PREGS *pregs = 
		e_info->Interleaved() ? 
		e_info->Simd_IRegs() : e_info->Simd_Regs();
	    SIMD_PREG *preg_0 = SIMD_PREGS_Get(pregs, 0);

	    /* copy the result over to other elements */
	    for (INT i = 1; i < pregs->Elements(); i++) {
		SIMD_PREG *cur = SIMD_PREGS_Get(pregs, i);
		WN *sto = cur->Simd_Preg_Stid(preg_0->Simd_Preg_Ldid());
		LWN_Insert_Block_Before(LWN_Get_Parent(Simd_Loop()), 
					Simd_Loop(), sto);
		WN_linenum(sto) = LWN_Get_Linenum(Simd_Loop());
	    }
	    
	    /* copy the result over to interleaved/deinterleaved regs */
	    if (e_info->Need_Interleave()) {
		SIMD_PREG *ld_preg = e_info->Simd_Reg();
		Is_True(ld_preg != NULL, 
			("Transform_Scalar: Interleaving without Simd_Reg"));
		for (INT i = 0; i < e_info->Simd_IRegs()->Elements(); i++) {
		    SIMD_PREG *cur = e_info->Simd_IReg(i);
		    WN *sto = cur->Simd_Preg_Stid(ld_preg->Simd_Preg_Ldid());
		    LWN_Insert_Block_Before(LWN_Get_Parent(Simd_Loop()), 
					    Simd_Loop(), sto);
		    WN_linenum(sto) = LWN_Get_Linenum(Simd_Loop());
		}
	    } else if (e_info->Need_Deinterleave()) {
		SIMD_PREG *ld_preg = e_info->Simd_IReg();
		Is_True(ld_preg != NULL, 
			("Transform_Scalar:Deinterleaving without Simd_IReg"));
		for (INT i = 0; i < e_info->Simd_Regs()->Elements(); i++) {
		    SIMD_PREG *cur = e_info->Simd_Reg(i);
		    WN *sto = cur->Simd_Preg_Stid(ld_preg->Simd_Preg_Ldid());
		    LWN_Insert_Block_Before(LWN_Get_Parent(Simd_Loop()), 
					    Simd_Loop(), sto);
		    WN_linenum(sto) = LWN_Get_Linenum(Simd_Loop());
		}
	    }
	}
    } else { /* store, must be privatizable.  Add a check here later */
	/* Scalar expansion: load/store a SIMD register.   */
	/* Generate SIMD register symbols */
	Generate_Simd_Reg_Info(e_info, simd);
    }
    
    return e_info;
}

/*---------------------------------------------------------------------------*
 * Generate a SIMD register (possibly a pair of registers) to hold the       *
 * interleaved result of an expression.                                      *
 *---------------------------------------------------------------------------*/
void SIMD_LOOP::Generate_Simd_IReg_Info(SIMD_EINFO *e_info, SIMD_INFO *simd)
{
    /* generate SIMD registers to hold interleaved values */
    TYPE_ID res_type = e_info->Res_Type();

    if (e_info->Simd_IReg())
      return;

    if (!e_info->Interleaved() && !e_info->Need_Interleave())
      return;
    
    INT vec_length = Vec_Length(simd, res_type);
    if (OPERATOR_is_compare(WN_operator(e_info->Expr()))) {
      /* generate corresponding XTBOOL */
      TYPE_ID xt_bool_type = 
	simd->Get_SIMD_Xtbool(simd->Get_SIMD_Width_Scalar(res_type));
      for (INT i = 0; i < vec_length; i++) {
	SIMD_PREG *vec_sym = Gen_Symbol(xt_bool_type, Pool(), "v_ifcc");
	e_info->Add_Simd_IReg(vec_sym);
      }
    } else {
      for (INT i = 0; i < vec_length; i++) {
	SIMD_PREG *vec_sym = Gen_Symbol(simd, res_type, Pool());
	e_info->Add_Simd_IReg(vec_sym);
      }
    }
}


/*---------------------------------------------------------------------------*
 * Generate a SIMD register (possibly a pair of registers) to hold the result*
 * of an expression.                                                         *
 *---------------------------------------------------------------------------*/
void SIMD_LOOP::Generate_Simd_Reg_Info(SIMD_EINFO *e_info, SIMD_INFO *simd)
{
    /* generate SIMD register to hold interleaved value */
    Generate_Simd_IReg_Info(e_info, simd);

    /* generate SIMD register to hold normal values */
    TYPE_ID res_type = e_info->Res_Type();
    
    if (e_info->Simd_Reg())
      return;

    if (e_info->Interleaved() && !e_info->Need_Deinterleave())
      return;

    INT vec_length = Vec_Length(simd, res_type);
    if (OPERATOR_is_compare(WN_operator(e_info->Expr()))) {
      /* generate corresponding XTBOOL */
      TYPE_ID xt_bool_type = 
	simd->Get_SIMD_Xtbool(simd->Get_SIMD_Width_Scalar(res_type));
      for (INT i = 0; i < vec_length; i++) {
	SIMD_PREG *vec_sym = Gen_Symbol(xt_bool_type, Pool(), "v_ifcc");
	e_info->Add_Simd_Reg(vec_sym);
      }
    } else {
      for (INT i = 0; i < vec_length; i++) {
	SIMD_PREG *vec_sym = Gen_Symbol(simd, res_type, Pool());
	e_info->Add_Simd_Reg(vec_sym);
      }
    }
}

/*---------------------------------------------------------------------------*
 * Simd code generation for a indirect load/store                            *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Transform_Indirect(WN *ls, WN *stmt, SIMD_INFO *simd)
{
    SIMD_EINFO  *e_info = Get_E_Info(ls);
    Is_True(e_info, ("Cannot find EINFO"));
    IMEM_INFO   *imem   = e_info->IMem();
    Is_True(imem, ("Cannot find IMEM"));

    SIMD_EINFO  *e_old  = imem->E_Info();

    if (!imem->Transformed()) {
	/* first visit */
	imem->Set_Transformed();
	e_old = NULL;
    }

    /* seen the IMEM_INFO before */
    if (e_old == e_info) {
	return e_info;
    } else if (e_old != NULL) {
	/* copy the old SIMD symbols to the new SIMD_EINFO */
	e_info->Copy_Simd_Reg_Info(e_old);

	/* the e_old may not have all the required registers */
	Generate_Simd_Reg_Info(e_info, simd);
	
	if (WN_operator(ls) == OPR_ILOAD) {
	    /* interleave/deinterleave if required */
	    e_info->Simd_Post_Processing(stmt, simd, this);

	} else { /* OPR_ISTORE */
	    if (e_info->Store_Back()) {
		e_info->Set_Updating();
	    }
	}

	/* recompute the invariant loop, because the current 'ls' may
	 lies at different level */
	MEM_POOL_Push(Pool());
	{
	    DOLOOP_STACK stack(Pool());
	    Is_True(imem->Non_Const_Level()>=0, 
		    ("Unexpected Non_Const_Level"));
	    Build_Doloop_Stack(ls, &stack);
	    
	    INT out_most_invariant = imem->Non_Const_Level() + 1;
	    if (out_most_invariant < stack.Elements()) {
		WN *invar_loop = stack.Bottom_nth(out_most_invariant);
		e_info->Set_Depth_Diff(stack.Elements() - out_most_invariant);
		imem->Set_To_Loop(invar_loop);
	    }
	}
	MEM_POOL_Pop(Pool());
	/* move the cursor */
	imem->Set_E_Info(e_info);
	return e_info;
    }

    /* move the cursor to point to the new e_info */
    imem->Set_E_Info(e_info);
    
    IMEM_OFFSET *io = imem->Imem_Offset();
    io->Transform_Load(ls, stmt);
    FmtAssert((io->Res_Reg_Count()>0 && io->Res_Reg_Count()<3),
	      ("Invalid number of result registers. Arbitrary unroll factor not supported."));
    SIMD_PREGS_Copy(io->Res_Regs(), e_info->Simd_Regs());

    if (io->Parent_Group()->Is_Reduction()) {
	Scalar_Redu().Push(e_info);
    }

    /* allocate interleave register if necessary */
    Generate_Simd_IReg_Info(e_info, simd);
    
    if (WN_operator(ls) == OPR_ILOAD) {
	WN *invar_pt = io->Invar_Point();
	if (!invar_pt) {
	    invar_pt = stmt;
	}

	/* interleave/deinterleave if required */
	e_info->Simd_Post_Processing(invar_pt, simd, this);
    } else {
	Is_True(WN_operator(ls) == OPR_ISTORE, ("Expecting ISTORE"));
	
	/* set the last store to update its address */
	if (e_info->Store_Back()) {
	    e_info->Set_Updating();
	}
    }
    
    return e_info;
}


extern 
INT Add_Edge_Combine(VINDEX16 src, VINDEX16 sink, DEPV_ARRAY *depv, 
		     MEM_POOL *pool)
{
    ARRAY_DIRECTED_GRAPH16  *adg      = Array_Dependence_Graph; 
    MEM_POOL                *adg_pool = adg->Pool();
    
    EINDEX16 edge = adg->Get_Edge(src, sink);
    if (edge == 0) {
	return (adg->Add_Edge(src, sink, depv));
    }

    MEM_POOL_Push(pool);
    {
	DEPV_ARRAY *orig_depv= adg->Depv_Array(edge);
	DEPV_LIST  *dl       = CXX_NEW(DEPV_LIST(orig_depv, pool), pool);
	DEPV_LIST  *result   = 
	    CXX_NEW(DEPV_LIST(dl->Num_Dim(), dl->Num_Unused_Dim(), pool), 
		    pool);
	DEPV_LIST  *dl_new   = CXX_NEW(DEPV_LIST(depv, pool), pool);
	DEPV_ITER iter(dl);
	for (DEPV_NODE *node = iter.First(); !iter.Is_Empty(); 
	     node = iter.Next()) {
	    DEPV *depv = node->Depv;
	    result->Append(CXX_NEW(DEPV_NODE(depv), pool));
	}
	DEPV_ARRAY *depv_r = Create_DEPV_ARRAY(result, adg_pool);
	adg->Set_Depv_Array(edge, depv_r);
    }
    MEM_POOL_Pop(pool);
    return 1;
}

/*---------------------------------------------------------------------------*
 * Add dependence graph with edge information                                *
 *---------------------------------------------------------------------------*/
INT SIMD_LOOP::Add_Dependence_Edges(VINDEX16_ARRAY *newvs,
				    VINDEX16_ARRAY *new_sinkvs,
				    EINDEX16 edge, INT dim)
{
    ARRAY_DIRECTED_GRAPH16  *adg      = Array_Dependence_Graph; 
    MEM_POOL                *adg_pool = adg->Pool();

    MEM_POOL_Push(Pool());
    {
	DEPV_ARRAY *orig_depv= adg->Depv_Array(edge);
	DEPV_LIST  *dl       = CXX_NEW(DEPV_LIST(orig_depv, Pool()), Pool());
	DEPV_LIST  *result   = 
	    CXX_NEW(DEPV_LIST(dl->Num_Dim(), dl->Num_Unused_Dim(), Pool()), 
		    Pool());
	INT  loopno = Do_Depth(Simd_Loop());
	UINT ln_pos = loopno - dl->Num_Unused_Dim();
	
	DEPV_ITER iter(dl);
	for (DEPV_NODE *node=iter.First(); !iter.Is_Empty(); node = iter.Next()) {
	    DEPV *Depv = node->Depv;
	    DEP   dep  = DEPV_Dep(Depv, ln_pos);
	    DEPV *r    = DEPV_Copy(Pool(), Depv, dl->Num_Dim());
	    
	    if (DEP_IsDistance(dep)) {
		INT dis = DEP_Distance(dep);
		if (dis % V_Unroll_Factor(Simd_Info) == 0) {
		    DEPV_Dep(r, ln_pos) = 
			DEP_SetDistance(dis / V_Unroll_Factor(Simd_Info));
		} else {
		    DEPV_Dep(r, ln_pos) = 
			DEP_UnionDirection(dep, 
					   (dis>0) ? DIR_POSEQ : DIR_NEGEQ);
		}
	    } else {
		DEPV_Dep(r, ln_pos) = DEP_UnionDirection(dep, DIR_EQ);
	    }
	    result->Append(CXX_NEW(DEPV_NODE(r), Pool()));
	}
	DEPV_ARRAY *depv_tmp = Create_DEPV_ARRAY(result, Pool());

	/* add the edges */
	for (INT source=0; source<newvs->Elements(); source++) {
	    VINDEX16 v_source = newvs->Get(source);
	    for (INT sink=0; sink<new_sinkvs->Elements(); sink++) {
		VINDEX16 v_sink = new_sinkvs->Get(sink);
		DEPV_ARRAY *depv = depv_tmp->Shorten(dim, adg_pool);
		if (Add_Edge_Combine(v_source, v_sink, depv, Pool()) == 0) {
		    MEM_POOL_Pop(Pool());
		    return 0;
		}
	    }
	}
    }
    MEM_POOL_Pop(Pool());
    return 1;
}

/*---------------------------------------------------------------------------*
 * Update dependence graph                                                   *
 *---------------------------------------------------------------------------*/
INT SIMD_LOOP::Update_Dependence() {
    return IMem_Map().Update_Dependence();
}

IMEM_INFO*
SIMD_LOOP::Add_Imem(SIMD_EINFO *e_info, WN *wn)
{
    Is_True(WN_operator(wn) == OPR_ILOAD, 
	    ("SIMD_LOOP::Add_Imem: Expecting an ILOAD"));
    WN *array_wn = WN_kid0(wn);
    if (WN_operator(array_wn) == OPR_ARRAY &&
	(WN_operator(WN_array_base(array_wn)) == OPR_LDID ||
	 WN_operator(WN_array_base(array_wn)) == OPR_LDA)) {
	IMEM_Map  &imem_map = IMem_Map();
	IMEM_INFO *imem_info = imem_map.Find(wn);
	if (imem_info == NULL) {
	    imem_info = CXX_NEW(IMEM_INFO(wn, &imem_map, Pool()), Pool());
	    imem_map.Enter(imem_info);  
	    imem_map.Add_Lod_Sto(wn);
	    imem_info->Setup_Load_Property(Loop_Model(), Num_Loops(),  this, false, -1, false);
	} else {
	    imem_info->Add_Lod_Sto(wn);
	    imem_map.Add_Lod_Sto(wn);
	}
	e_info->Set_IMem(imem_info);
	
	return imem_info;
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Simd find mulabc:                                                         *
 *     (1) find mulabc                                                       *
 *     (2) setup mulabc properties                                           * 
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Find_Mulabc(SIMD_EINFO *e_info, WN *wn, WN *child2, 
			    SIMD_EINFO *c1_info)
{
  if (Simd_Target != SIMD_TARG_VECTRA_I)
    return;
  
    OPERATOR op = WN_operator(wn);
    if (op == OPR_SUB) {
	return;
    }
    Is_True(op == OPR_ADD, ("Expecting OPR_ADD"));

    WN *kid0 = WN_kid0(child2);
    WN *kid1 = WN_kid1(child2);

    SIMD_EINFO *s1_info = Get_E_Info(kid0);
    SIMD_EINFO *s2_info = Get_E_Info(kid1);

    /* check for MULABC candidate */
    if (WN_operator(kid0) == OPR_ILOAD && WN_operator(kid1) == OPR_ILOAD) {
	IMEM_INFO *m1_info = s1_info->IMem();
	IMEM_INFO *m2_info = s2_info->IMem();

	if (m1_info == NULL && m2_info == NULL) {
	    return;
	}

	if (m1_info == NULL) {
	    /* loop invariant may have skipped the ILOAD */
	    m1_info = Add_Imem(s1_info, kid0);
	} else if (m2_info == NULL) {
	    m2_info = Add_Imem(s2_info, kid1);
	}

	if ((m1_info->Non_Const_Level() == m2_info->Non_Const_Level()) &&
	    (s1_info->Is_Vector() && !s2_info->Is_Vector() &&
	     m1_info->Has_Reuse() && 
	     (m2_info->Stride_At_Loop(m1_info->Non_Const_Level()) == 1)
	     ||
	     s2_info->Is_Vector() && !s1_info->Is_Vector() &&
	     m2_info->Has_Reuse() && 
	     (m1_info->Stride_At_Loop(m2_info->Non_Const_Level()) == 1))) {
	    INT loop_level = m1_info->Non_Const_Level();
	    
	    /* indicate load pair or load const for s1 */
	    s1_info->Set_Load_Reuse();
	    Push_Unroll_Candidate(loop_level, s1_info);

	    /* indicate load pair or load const for s2 */
	    s2_info->Set_Load_Reuse();
	    Push_Unroll_Candidate(loop_level, s2_info);

	    /* indicate MULABC for MULA_0 */
	    e_info->Set_Kid_EInfo(s1_info, 0);
	    /* indicate MULABC for MULA_1 */
	    e_info->Set_Kid_EInfo(s2_info, 1);

	    Push_Unroll_Candidate(loop_level, e_info);
	    e_info->Set_Mulabc();

	    MEM_POOL_Push(&SIMD_local_pool);
	    {
		DOLOOP_STACK *dl_stack = 
		    CXX_NEW(DOLOOP_STACK(&SIMD_local_pool), &SIMD_local_pool);
		Build_Doloop_Stack(wn, dl_stack);
		WN *loop = dl_stack->Bottom_nth(loop_level);
		if (m1_info->Has_Reuse()) {
		    m2_info->Set_Alignment(this, loop, loop_level);
		    m1_info->Set_Alignment(this, (WN*) NULL, -1, loop, loop_level);
		} else {
		    Is_True(m2_info->Has_Reuse(),
			    ("SIMD_LOOP::Simd_Find_Mulabc: inconsistent IMEM"));
		    m1_info->Set_Alignment(this, loop, loop_level);
		    m2_info->Set_Alignment(this, (WN*) NULL, -1, loop, loop_level);
		}
	    }
	    MEM_POOL_Pop(&SIMD_local_pool);
	}
    }
}

/*---------------------------------------------------------------------------*
 * For moving shift_scale out of a loop                                      *
 *                                                                           *
 * for (...) {                                                               *
 *    x = x + (a * b >> scale);                                              *
 * }                                                                         *
 *                                                                           *
 * x is a vector                                                             *
 *                                                                           *
 * v_x = 0;                                                                  *
 * for (...) {                                                               *
 *   v_x = v_x + a*b;                                                        *
 * }                                                                         *
 * v_x = v_x >> scale;                                                       *
 *---------------------------------------------------------------------------*/
static void Check_Use(SHIFT_SCALE &ss)
{
    WN *wn = ss.st_wn;
    WN *res = NULL;

    Is_True(WN_operator(wn) == OPR_STID, ("Expecting a STID"));
    USE_LIST *use_list = Du_Mgr->Du_Get_Use(wn);
    USE_LIST_ITER iter_use(use_list);
    for (DU_NODE *use_node = iter_use.First(); !iter_use.Is_Empty();
	 use_node = (DU_NODE *) iter_use.Next()) {
	WN *use = use_node->Wn();
	WN *stmt = LWN_Get_Statement(use);
	if (stmt != wn) {
	    if (Find_Stmt_Under(use, LWN_Get_Parent(wn))) {
		ss.has_inner_use = true;
	    }
	    if (res != NULL) {
		ss.has_multiple_use = true;
	    } else {
		res = use;
	    }
	}
    }

    ss.after_use = res;
}

static void Check_Shift_Amount(SHIFT_SCALE &ss, TYPE_ID scalar_type)
{
  if (!LNO_Simd_Shr_Safe || (LNO_SIMD_Level < 2)) {
    ss.shift_is_ok = false;
    return;
  }
  
  INT guard_bits = Simd_Info->Guard_Bits(scalar_type);
  if (guard_bits <= 0) {
    ss.shift_is_ok = false;
    return;
  }
  
  ss.shift_is_ok = true;
  WN *wn = ss.shift_scale;
  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_INTCONST) {
    return;
  }
  
  Is_True(oper == OPR_LDID, ("Expecting a LDID"));
  DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(wn);
  DEF_LIST_ITER iter_def(def_list);
  for (DU_NODE *def_node = iter_def.First(); !iter_def.Is_Empty();
       def_node = (DU_NODE *) iter_def.Next()) {
    WN *def = def_node->Wn();
    if (Find_Stmt_Under(def, LWN_Get_Parent(ss.st_wn))) {
      ss.shift_is_ok = false;
    }
  }
}

static void Check_Shift_Scale_And_Def_Use(SHIFT_SCALE &ss)
{
    WN *wn = ss.shift;
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_LSHR || oper == OPR_ASHR, ("Expecting shift right"));

    WN *kid0 = WN_kid0(wn);
    WN *kid1 = WN_kid1(wn);
    if (WN_operator(kid0) == OPR_MPY && 
	OPERATOR_is_leaf(WN_operator(kid1))) {
	ss.shift_scale = kid1;
    } else {
	return;
    }

    Check_Use(ss);

    if (!ss.Use_Ok()) {
	return;
    }

    WN *ld_wn = ss.ld_wn;
    Is_True(ld_wn && WN_operator(ld_wn) == OPR_LDID, ("Expecting LDID"));

    WN *res = NULL;
    ss.init_is_zero = true;
    DEF_LIST *def_list = Du_Mgr->Ud_Get_Def(ld_wn);
    DEF_LIST_ITER iter_def(def_list);
    for (DU_NODE *def_node = iter_def.First(); !iter_def.Is_Empty();
	 def_node = (DU_NODE *) iter_def.Next()) {
	WN *def = def_node->Wn();
	if (def != ss.st_wn) {
	    if (Find_Stmt_Under(def, LWN_Get_Parent(ss.st_wn))) {
		ss.has_inner_def = true;
	    }
	    if (res != NULL) {
		ss.init_is_zero = false;
	    } else {
		res = def;
	    }
	}
    }

    if (ss.init_is_zero && res != NULL) {
	if (WN_operator(res) == OPR_STID && 
	    WN_operator(WN_kid0(res)) == OPR_INTCONST && 
	    WN_const_val(WN_kid0(res)) == 0) {
	    ; // do nothing
	} else {
	    ss.init_is_zero = false;
	}
    }
}

void
SIMD_LOOP::Move_Shift_Scale_Out(WN *wn, SIMD_EINFO *einfo)
{
    OPERATOR oper = WN_operator(wn);
    Is_True(oper == OPR_STID, ("SIMD_Move_Shift_Scale_Out: not a STID"));
    
    if (oper == OPR_STID) {
	REDUCTION_TYPE red_type = red_manager->Which_Reduction(wn);
	if (red_type == RED_ADD) {
	    WN *rhs = WN_kid0(wn);
	    if (WN_operator(rhs) != OPR_ADD && WN_operator(rhs) != OPR_SUB) {
		return;
	    }
	    SHIFT_SCALE *ss = CXX_NEW(SHIFT_SCALE(wn, einfo), Pool());
	    if (red_manager->Which_Reduction(WN_kid0(rhs)) != RED_ADD) {
		if (red_manager->Which_Reduction(WN_kid1(rhs)) != RED_ADD) {
		    return;
		} else {
		    /* swap the two kids */
		    WN *t = WN_kid0(rhs);
		    WN_kid0(rhs) = WN_kid1(rhs);
		    WN_kid1(rhs) = t;
		}
	    }

	    ss->ld_wn = WN_kid0(rhs);
	    ss->shift = WN_kid1(rhs);
	
	    if (WN_operator(ss->shift) != OPR_ASHR &&
		WN_operator(ss->shift) != OPR_LSHR) {
		return;
	    }

	    // check shift scale and def/use of the reduction 
	    Check_Shift_Scale_And_Def_Use(*ss);
	    if (!ss->Def_Use_Ok()) {
		return;
	    }

	    // check the shift amount for movability
	    Check_Shift_Amount(*ss, einfo->Res_Type());

	    if (!ss->shift_is_ok) {
		return;
	    }
	    
	    einfo->Set_Shift_Scale_Moved();
	    SIMD_EINFO *shift_info = Get_E_Info(ss->shift);
	    Is_True(shift_info != NULL, 
		    ("SIMD_Move_Shift_Scale_Out: Missing EINFO"));
	    shift_info->Set_Shift_Scale_Moved();
	    Shift_Scale().Push(ss);
	}
    }
}

void SIMD_EINFO_Add_Def_Use(SIMD_EINFO *def_einfo, SIMD_EINFO *use_einfo)
{
    def_einfo->Add_Use(use_einfo);
    use_einfo->Add_Def(def_einfo);
}


bool
SIMD_LOOP::Is_Noop_Cvt (WN *wn)
{
  OPERATOR oper = WN_operator(wn);

  if (oper != OPR_CVT && oper != OPR_CVTL) {
    return false;
  }
  
  SIMD_EINFO  *e_info = Get_E_Info(wn);
  if (!e_info) {
    return false;
  }

  WN *kid = WN_kid0(wn);
  SIMD_EINFO *c_info = Get_E_Info(kid);
  if (!c_info) {
    return false;
  }

  TYPE_ID kid_type = c_info->Res_Type();
  TYPE_ID res_type = e_info->Res_Type();

  if (kid_type == res_type) {
    return true;
  }

  return false;
}


//
// Setup_Type_Conversion inserts explicit CVT/CVTL nodes wherever the 
// vectorizer needs to do a type conversion based on the SIMD_EINFO 
// result types
SIMD_EINFO *
SIMD_LOOP::Setup_Type_Conversion_Rec(WN* wn) 
{
    OPERATOR oper  = WN_operator(wn);
 
    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
	    Setup_Type_Conversion_Rec(kid);
	}
	return NULL;
    } else if (oper == OPR_DO_LOOP) {
	return Setup_Type_Conversion_Rec(WN_do_body(wn));
    }
    
    SIMD_EINFO  *e_info  = Get_E_Info(wn);
    if (!e_info->Is_Vector() || OPERATOR_is_load(oper)) {
	return e_info;
    }
    
    for (INT kid_idx=0; kid_idx<WN_kid_count(wn); kid_idx++) {
	WN *kid = WN_kid(wn,kid_idx);
	SIMD_EINFO *c_info = Setup_Type_Conversion_Rec(kid);
	
	// find the canonical type for the current kid
	// if it is different from the current type, insert a conversion
	TYPE_ID kid_type = c_info->Res_Type();
	TYPE_ID res_type = e_info->Res_Type();
	if (kid_type != res_type && MTYPE_complement(kid_type) != res_type &&
	    !(WN_operator(wn) == OPR_CVT || WN_operator(wn) == OPR_CVTL ||
	      WN_operator(wn) == OPR_MPY)) {
	    if (simd_debug) {
		fprintf(TFile,"Adding a CVT from %s to %s.\n",
			MTYPE_name(kid_type),MTYPE_name(res_type));
	    }
	    Is_True(MTYPE_is_integral(res_type),("Expected integral type."));
	    INT from_bit_size = MTYPE_bit_size(kid_type);
	    INT to_bit_size = MTYPE_bit_size(res_type);
	    WN *cvt = NULL;
	    if (from_bit_size < MTYPE_bit_size(MTYPE_I4) ||
		to_bit_size < MTYPE_bit_size(MTYPE_I4)) {
		cvt = WN_Create(OPR_CVTL, Promote_Type(res_type),MTYPE_V,1);
		WN_kid0(cvt) = kid; 
		WN_cvtl_bits(cvt) = to_bit_size;
	    } else {
		cvt = WN_Create(OPR_CVT,Promote_Type(res_type),
				WN_rtype(kid),1);
		WN_kid0(cvt) = kid;
	    }
	    WN_kid(wn,kid_idx)=cvt;
	    LWN_Set_Parent(cvt,wn);
	    LWN_Set_Parent(kid,cvt);
	    
	    // create a new SIMD_EINFO for the new node
	    e_info = CXX_NEW(SIMD_EINFO(cvt, to_bit_size, Pool()), Pool());
	    e_info->Set_Res_Type(res_type);
	    e_info->Set_Is_Vector();
	    E_Info().Enter(cvt, e_info);
	    if (c_info->Is_Round_Reg()) {
		e_info->Set_Is_Round_Reg();
	    }
#ifdef Is_True_On
	    SIMD_EINFO *e1_info = Get_E_Info(cvt);
	    Is_True(e1_info == e_info, ("Mismatch EINFO"));
#endif
	}
	if (OPERATOR_is_store(oper) ||
	    oper==OPR_SHL || oper==OPR_ASHR || oper==OPR_LSHR ||
	    oper==OPR_DIV) {
	    break;
	}
    }
	
    return e_info;
}
 

SIMD_EINFO *
SIMD_LOOP::Setup_Type_Conversion(WN *wn) {
    SIMD_EINFO *e_info = Setup_Type_Conversion_Rec(wn);
}                      

WN *
SIMD_LOOP::Simd_If_Conv_Ldid(WN *wn)
{
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
    Is_True(s_info, ("SIMD_LOOP::Simd_If_Conv_Ldid: NULL s_info"));
    SIMD_PREG *tos = s_info->If_Conv_Sym_Top();

    if (tos) {
      // return the current TOS
      return tos->Simd_Preg_Ldid_With_EInfo();
    }
    return NULL;
}

WN *
SIMD_LOOP::Simd_If_Conv_Iload(WN *wn)
{
    IMEM_INFO *imem = IMem_Map().Find(wn);
    Is_True(imem, ("SIMD_LOOP::Simd_If_Conv_Iload: NULL imem"));
    SIMD_PREG *tos = imem->If_Conv_Sym_Top();

    if (tos) {
      // return the current TOS
      return tos->Simd_Preg_Ldid_With_EInfo();
    }
    return NULL;
}

WN *
SIMD_LOOP::Simd_If_Conv_Read(WN *wn)
{
  if (Invariant_In_Simd_Loop(wn))
    return NULL;

    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_LDID) {
	return Simd_If_Conv_Ldid(wn);
    } else if (oper == OPR_ILOAD) {
	Simd_If_Conv_Iload(wn);
    } else if (oper == OPR_INTRINSIC_CALL) {
	FmtAssert(0, ("SIMD_LOOP::Simd_If_Conv_Read doesn't work for TIE (yet)"));
    } else {
	for (INT i = 0; i < WN_kid_count(wn); i++) {
	    WN *kid = WN_kid(wn, i);
	    WN *new_kid = Simd_If_Conv_Read(kid);
	    if (new_kid) {
	      LWN_Set_Parent(kid, NULL);
	      WN_kid(wn, i) = new_kid;
	      LWN_Set_Parent(new_kid, wn);
	      Old_Stmt().Push(kid);
	    }
	}
    }
    return NULL;
}

WN *
SIMD_LOOP::Simd_If_Conv_Stid(WN *wn, SIMD_IF *simd_if, bool is_then)
{
    SYMBOL symbol(wn);
    SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
    Is_True(s_info, ("SIMD_LOOP::Simd_If_Conv_Ldid: NULL s_info"));
    
    SIMD_PREG *preg = Gen_Symbol(Promote_Type(WN_desc(wn)), Pool(), symbol.Name());
    s_info->If_Conv_Sym_Push(preg);

    simd_if->Separate_Rhs(wn, preg);
    simd_if->Update_Sel(s_info, preg, wn, is_then);
}

WN *
SIMD_LOOP::Simd_If_Conv_Istore(WN *wn, SIMD_IF *simd_if, bool is_then)
{
    IMEM_INFO *imem = IMem_Map().Find(wn);
    Is_True(imem, ("SIMD_LOOP::Simd_If_Conv_Iload: NULL imem"));
    
    SIMD_PREG *preg = Gen_Symbol(Promote_Type(WN_desc(wn)), Pool(), "v_ifci");
    imem->If_Conv_Sym_Push(preg);

    simd_if->Separate_Rhs(wn, preg);
    simd_if->Update_Sel(imem, preg, wn, is_then);
}

WN *
SIMD_LOOP::Simd_If_Conv_Write(WN *wn, SIMD_IF *simd_if, bool is_then)
{
    
    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_INTRINSIC_CALL ||
	oper == OPR_INTRINSIC_OP) {
	FmtAssert(0, ("SIMD_LOOP::Simd_If_Conv_Read doesn't work for TIE (yet)"));
	return NULL;
    } else {
	WN *rhs = WN_kid0(wn);
	WN *new_rhs = Simd_If_Conv_Read(rhs);

	if (new_rhs) {
	  LWN_Set_Parent(rhs, NULL);
	  WN_kid0(wn) = new_rhs;
	  LWN_Set_Parent(new_rhs, wn);
	}

	if (oper == OPR_STID) {
	    Simd_If_Conv_Stid(wn, simd_if, is_then);
	} else {
	    Is_True(oper == OPR_ISTORE, ("Expecting a store"));
	    Simd_If_Conv_Istore(wn, simd_if, is_then);
	}
	return NULL;
    }
}

void
SIMD_LOOP::Simd_If_Conv_Pop(WN *wn)
{
    OPERATOR oper = WN_operator(wn);
    if (oper == OPR_INTRINSIC_CALL ||
	oper == OPR_INTRINSIC_OP) {
	FmtAssert(0, ("SIMD_LOOP::Simd_If_Conv_Pop doesn't work for TIE (yet)"));
	return;
    } else if (oper == OPR_STID) {
	SYMBOL symbol(wn);
	SIMD_SCALAR *s_info = S_Info_Map().Find(symbol);
	if (s_info != NULL) {
	    Is_True(s_info, ("SIMD_LOOP::Simd_If_Conv_Ldid: NULL s_info"));
	    s_info->If_Conv_Sym_Pop();
	}
    } else if (oper == OPR_ISTORE) {
	IMEM_INFO *imem = IMem_Map().Find(wn);
	Is_True(imem, ("SIMD_LOOP::Simd_If_Conv_Pop: NULL imem"));
	imem->If_Conv_Sym_Pop();
    } else if (oper == OPR_IF) {
	SIMD_IF *simd_if = If_Map().Find(wn);
	Is_True(simd_if, ("SIMD_LOOP::Simd_If_Conv_Pop: NULL simd_if"));
	simd_if->If_Conv_Sym_Pop();
    }
}

void
SIMD_LOOP::Simd_If_Conv(WN *wn, SIMD_IF *simd_if, bool is_then)
{
    OPERATOR oper  = WN_operator(wn);

    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	MEM_POOL_Push(&SIMD_local_pool);
	{
	    EXPR_Stack stmts(&SIMD_local_pool);
	    for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
		stmts.Push(kid);
	    }
	    /* rename */
	    for (INT i = 0; i < stmts.Elements(); i++) {
		WN *kid = stmts.Bottom_nth(i);
		Simd_If_Conv(kid, simd_if, is_then);
	    }
	    /* pop the rename stack */
	    while (stmts.Elements() >0) {
		WN *kid = stmts.Pop();
		Simd_If_Conv_Pop(kid);
	    }
	}
	MEM_POOL_Pop(&SIMD_local_pool);
    } else if (oper == OPR_DO_LOOP) {
	Simd_If_Conv(WN_do_body(wn), simd_if, is_then);
    } else if (oper == OPR_IF) {
	Simd_If_Conv_If(wn, simd_if, is_then);
    } else {
	Simd_If_Conv_Write(wn, simd_if, is_then);
    }
    return;
}

void
SIMD_LOOP::Simd_If_Conv_If(WN *wn, SIMD_IF *p_simd_if, bool is_then)
{
  WN *top_if_wn = p_simd_if ? p_simd_if->Top_If_Wn() : wn;
  SIMD_IF *simd_if = CXX_NEW(SIMD_IF(wn, top_if_wn, Pool()), Pool());
  If_Map().Enter(wn, simd_if);

  WN *res = Simd_If_Conv_Read(WN_if_test(wn));
  Is_True(!res, ("Unexpected NULL on condition."));
  
  Simd_If_Conv(WN_then(wn), simd_if, true);
  Simd_If_Conv(WN_else(wn), simd_if, false);
  
  simd_if->Generate_Sel(p_simd_if, is_then);
  LWN_Extract_From_Block(LWN_Get_Parent(wn), wn);
  Old_Stmt().Push(wn);
}

void
SIMD_LOOP::Simd_If_Conv(WN *wn) 
{
    OPERATOR oper = WN_operator(wn);
    /* scan statements in a block */
    if (oper == OPR_BLOCK) {
	WN *kid = WN_first(wn);
	while(kid) {
	    WN *next_kid = WN_next(kid);
	    Simd_If_Conv(kid);
	    kid = next_kid;
	}
	return;
    } else if (oper == OPR_DO_LOOP) {
	return Simd_If_Conv(WN_do_body(wn));
    } else if (oper == OPR_IF) {
	Is_True(LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || Has_If_Convert_Pragma(wn), 
		("SIMD_LOOP::Simd_If_Conv: unexpected IF"));
	Simd_If_Conv_If(wn, NULL, false);
    }
}

/*---------------------------------------------------------------------------*
 * Simd pre_transform:                                                       *
 *     (1) decide and set up 'mula'                                          *
 *     (2) decide and set up 'mulabc'                                        * 
 *     (3) decide and set up 'interleave' property                           *
 *---------------------------------------------------------------------------*/
SIMD_EINFO *
SIMD_LOOP::Simd_Preprocess_Transform(WN* wn)
{
  OPERATOR oper  = WN_operator(wn);
  TYPE_ID  rtype = WN_rtype(wn);
  TYPE_ID  desc  = WN_desc(wn);
  
  /* scan statements in a block */
  if (oper == OPR_BLOCK) {
    for (WN *kid = WN_first(wn); kid != NULL; kid = WN_next(kid)) {
      Simd_Preprocess_Transform(kid);
    }
    return NULL;
  } else if (oper == OPR_DO_LOOP) {
    return Simd_Preprocess_Transform(WN_do_body(wn));
  } else if (oper == OPR_IF) {
    Is_True(LNO_Simd_If_Conv || LNO_Simd_Aggressive_If_Conv || Has_If_Convert_Pragma(wn), 
            ("SIMD_LOOP::Simd_Preprocess_Transform: unexpected IF"));
    Simd_Preprocess_Transform(WN_if_test(wn));
    Is_True(WN_first(WN_then(wn))==NULL && WN_first(WN_else(wn)) == NULL, 
            ("SIMD_LOOP::Simd_Preprocess_Transform: then/else not converted"));
    return NULL;
  }
  
  SIMD_EINFO  *e_info  = Get_E_Info(wn);
  SIMD_EINFO  *c1_info = NULL;
  SIMD_EINFO  *c2_info = NULL;
  SIMD_EINFO  *c3_info = NULL;
  WN          *child1  = NULL;
  WN          *child2  = NULL;
  SIMD_SCALAR *s_info  = NULL;
  
  if (!e_info->Is_Vector() && !OPERATOR_is_leaf(oper)) {
    return e_info;
  }
  
  /* build DU chains */
  switch (oper) {
  case OPR_STID:   /* scalar */
    /* move invariant shift scale out if possible */
    Move_Shift_Scale_Out(wn, e_info);
    
    /* fall through */
  case OPR_LDID:
  {
    if (oper == OPR_STID) {
      c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
      SIMD_EINFO_Add_Def_Use(c1_info, e_info);
    } else { /* LDID */
      /* chase the UD chain to get all the defs */
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
    }
    break;
  }
  
  case OPR_ISTORE: /* indirect load/store */
  case OPR_ILOAD:
  {
    IMEM_INFO  *imem      = e_info->IMem();
    Is_True(imem, ("Cannot find IMEM_INFO"));
    
    IMEM_OFFSET *io=imem->Imem_Offset();
    Is_True(io,("IMEM_OFFSET missing."));
    IMEM_GROUP *ig=io->Parent_Group();
    Is_True(ig,("IMEM_GROUP missing."));
    if (oper == OPR_ISTORE) {
      ig->Stores()->AddElement(wn);
    } else {
      Is_True(oper==OPR_ILOAD,("Wrong case label?"));
      ig->Loads()->AddElement(wn);
    }
    
    if (oper == OPR_ISTORE) {
      c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
      SIMD_EINFO_Add_Def_Use(c1_info, e_info);
      if (MTYPE_bit_size(c1_info->Res_Type()) >= 
          2 * MTYPE_bit_size(e_info->Res_Type())) {
        /* pack */
        c1_info->Set_Need_Interleave();
      }
    } else { /* ILOAD */
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
              (def_einfo->IMem()->Imem_Offset()->Parent_Group() == ig))
	  {
	    DEPV_ARRAY *depv_array = adg->Depv_Array(edge);
	    bool has_all_equal = false;
	    MEM_POOL_Push(&SIMD_local_pool);
	    {
	      DEPV_LIST depv_list(depv_array,&SIMD_local_pool);
	      has_all_equal = depv_list.Contains_All_Equals();
	    }
	    MEM_POOL_Pop(&SIMD_local_pool);
	    if (has_all_equal ||
		e_info->IMem()->Imem_Offset()->Parent_Group()->Is_Reduction())
	      SIMD_EINFO_Add_Def_Use(def_einfo, e_info);
          }
          edge = adg->Get_Next_In_Edge(edge);
        }
      }
    }
    break;
  }
  
  case OPR_CVT:
  case OPR_CVTL:
    c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
    SIMD_EINFO_Add_Def_Use(c1_info, e_info);
    if (MTYPE_bit_size(c1_info->Res_Type()) >= 
        2 * MTYPE_bit_size(e_info->Res_Type())) {
      /* pack */
      c1_info->Set_Need_Interleave();
    } else if (MTYPE_bit_size(c1_info->Res_Type()) * 2 <=
               MTYPE_bit_size(e_info->Res_Type())) {
      /* double size */
      e_info->Set_Interleaved();
      e_info->Set_Need_Deinterleave();
      if (WN_operator(WN_kid0(wn)) == OPR_INTCONST) {
        c1_info->Set_Need_Interleave();
      }
    }
    break;
    
  case OPR_ABS:    /* unary */
  case OPR_NEG:
  case OPR_SHL:
  case OPR_ASHR:
  case OPR_LSHR:
  case OPR_BNOT:
  case OPR_DIV:
  case OPR_PARM:
    c1_info = Simd_Preprocess_Transform(WN_kid0(wn));
    SIMD_EINFO_Add_Def_Use(c1_info, e_info);
    break;
    
  case OPR_ADD: /* find MULA candidate */
  case OPR_SUB: 
  { 
    child1 = WN_kid0(wn);
    child2 = WN_kid1(wn);
    
    /* set interleaving property */
    c1_info = Simd_Preprocess_Transform(child1);
    c2_info = Simd_Preprocess_Transform(child2);
    
    SIMD_EINFO_Add_Def_Use(c1_info, e_info);
    SIMD_EINFO_Add_Def_Use(c2_info, e_info);
    
    /* skip shift scale if they are moved out */
    if (c1_info->Shift_Scale_Moved()) {
      child1 = WN_kid0(child1);
      c1_info = Get_E_Info(child1);
    }
    if (c2_info->Shift_Scale_Moved()) {
      child2  = WN_kid0(child2);
      c2_info = Get_E_Info(child2);
    }
    
    if ((oper == OPR_SUB || oper == OPR_ADD) && 
        WN_operator(child2) == OPR_MPY && c2_info->Is_Vector()) {
      if (oper == OPR_ADD && c1_info->Is_Round_Reg()) {
        e_info->Set_Mula();
        Simd_Find_Mulabc(e_info, wn, child2, c1_info);
        break;
      }
      
      if (c1_info->Single_Use() || c1_info->Is_Over_Written()) {
        e_info->Set_Mula();
        Simd_Find_Mulabc(e_info, wn, child2, c1_info);
      }
    }
    break;
  } 
  
  case OPR_MPY:  
    /* set interleaving property */
    e_info->Set_Interleaved();
    e_info->Set_Need_Deinterleave();
    
    /* fall through */
  case OPR_NE:
  case OPR_GE:
  case OPR_GT:
  case OPR_EQ:
  case OPR_LE:
  case OPR_LT:
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
    
  case OPR_INTRINSIC_CALL:
  case OPR_INTRINSIC_OP:
    for (INT kid_idx = 0; kid_idx<WN_kid_count(wn); kid_idx++) {
      c1_info = Simd_Preprocess_Transform(WN_kid(wn, kid_idx));
      SIMD_EINFO_Add_Def_Use(c1_info, e_info);
    }
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

  case OPR_INTCONST:
    break;
    
  default:
    Is_True(0, ("Unexpected operator %s", OPERATOR_name(oper)));
    break;
  }
  
  All_EInfo().Push(e_info);
  
  return e_info;
}

/*---------------------------------------------------------------------------*
 *  if "Need_Deinterleave" is set,                                           *
 *  Reset it if all uses are "Interleaved"                                   *
 *---------------------------------------------------------------------------*/
void
SIMD_EINFO::Fold_Deinterleave()
{
    if (Interleaved()) {
	Reset_Need_Interleave();
    }

    if (Need_Deinterleave()) {
	for (INT i = 0; i < Uses()->Elements(); i++) {
	    SIMD_EINFO *cur = Use(i);
	    if (!cur->Interleaved() &&
		!((WN_operator(cur->Expr()) == OPR_CVT ||
		   WN_operator(cur->Expr()) == OPR_CVTL) &&
		  MTYPE_bit_size(Res_Type()) >= 
		  2 * MTYPE_bit_size(cur->Res_Type()))) {
		return;
	    }
	}
	Reset_Need_Deinterleave();
    }
}

/*---------------------------------------------------------------------------*
 * Check if it is better to move deinterleave operation up to its uses       *
 *---------------------------------------------------------------------------*/
bool
SIMD_EINFO::Better_Move_Deinterleave_Up() 
{
    INT cost = 0;
    for (INT i = 0; i < Uses()->Elements(); i++) {
	SIMD_EINFO *cur = Use(i);

	/* cost for making 'cur' interleave */
	INT local_cost = 0;
	
	/* cancel need_interleave with deinterleave */
	if (cur->Need_Interleave()) {
	    local_cost--;
	}

	/* combine mula */
	if (cur->Mula()) {
	    local_cost--;
	}

	/* pack */
	if ((WN_operator(cur->Expr()) == OPR_CVT ||
	     WN_operator(cur->Expr()) == OPR_CVTL) &&
	    MTYPE_bit_size(Res_Type()) >= 
	    2 * MTYPE_bit_size(cur->Res_Type())) {
	    local_cost--;
	}

	for (INT j = 0; j < cur->Defs()->Elements(); j++) {
	    SIMD_EINFO *def = cur->Def(j);
	    if (!def->Interleaved() && !def->Need_Interleave() &&
		!def->Reduction() && WN_operator(def->Expr()) != OPR_INTCONST) {
		local_cost++;
	    }
	}

	cost += local_cost;

	/* estimate the loop depth effect */
	cost += (cur->Do_Depth() - Do_Depth()) * 100;
    }

    /* slight favor move it up, hence (cost <= 0) */
    return (cost <= 0);
}

/*---------------------------------------------------------------------------*
 * Check if it is better to move interleave operation down to its defs       *
 *---------------------------------------------------------------------------*/
bool
SIMD_EINFO::Better_Move_Interleave_Down() 
{
    INT cost = 0;
	
    for (INT i = 0; i < Defs()->Elements(); i++) {
	SIMD_EINFO *cur = Def(i);

	/* cost for making 'cur' interleave */
	INT local_cost = 0;
	
	if (cur->Need_Interleave() || cur->Interleaved() ||
	    cur->Reduction() || WN_operator(cur->Expr()) == OPR_INTCONST) {
	    /* no need to do anything */
	    ;
	} else {
	    local_cost++;
	}

	cost += local_cost;

	/* estimate the loop depth effect */
	cost += (cur->Do_Depth() - Do_Depth()) * 100;
    }
    
    return (cost <= 0);
}


/*---------------------------------------------------------------------------*
 * Simd preprocess to place interleave/deinterleave operations               *
 *---------------------------------------------------------------------------*/
void
SIMD_LOOP::Simd_Preprocess_Interleave()
{
    EINFO_Stack deinterleave_einfo(Pool());
    EINFO_Stack interleave_einfo(Pool());

    dump_interleave("Before SIMD preprocess interleave", NULL, All_EInfo());

    for (INT i = 0; i < All_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = All_EInfo().Bottom_nth(i);
	cur->Fold_Deinterleave();
	if (cur->Need_Interleave()) {
	    interleave_einfo.Push(cur);
	} else if (cur->Need_Deinterleave()) {
	    deinterleave_einfo.Push(cur);
	}
    }
    
    bool changed = true;

    while (changed) {
	changed = false;
	/* Move deinterleave one level up */
	for (INT i = 0; i < deinterleave_einfo.Elements(); i++) {
	    SIMD_EINFO *cur = deinterleave_einfo.Bottom_nth(i);
	    /* check to see if it is beneficial to move the deinterleave up */
	    if (cur->Need_Deinterleave() &&
		cur->Uses()->Elements() > 0 && 
		cur->Better_Move_Deinterleave_Up()) {

		/* move the deinterleave up */
		changed = true;

		for (INT j = 0; j < cur->Uses()->Elements(); j++) {
		    SIMD_EINFO *use = cur->Use(j);
		    if (use->Interleaved() ||
			(WN_operator(use->Expr()) == OPR_CVT ||
			 WN_operator(use->Expr()) == OPR_CVTL) &&
			MTYPE_bit_size(cur->Res_Type()) >= 
			2 * MTYPE_bit_size(use->Res_Type())) {
			/* already interleaved or pack, do nothing */
			continue;
		    }
		    if (use->Need_Interleave()) {
			/* need interleave, set already interleaved */
			use->Set_Interleaved();
			use->Fold_Deinterleave();
		    } else {
			use->Set_Need_Deinterleave();
			use->Set_Interleaved();
		    }

		    /* propagate need interleave down */
		    for (INT k = 0; k < use->Defs()->Elements(); k++) {
			SIMD_EINFO *def = use->Def(k);
			def->Set_Need_Interleave();
			def->Fold_Deinterleave();
			if (def->Need_Interleave()) {
			    interleave_einfo.Push(def);
			}
		    }
		    if (use->Need_Deinterleave()) {
			deinterleave_einfo.Push(use);
		    }
		}
		cur->Set_Need_Interleave();
		cur->Fold_Deinterleave();
		    
		dump_interleave("SIMD preprocess interleave: move deinterleave up",
				cur, All_EInfo());
	    }
	}

	/* Move need interleave down */
	for (INT i = 0; i < interleave_einfo.Elements(); i++) {
	    SIMD_EINFO *cur = interleave_einfo.Bottom_nth(i);
	    /* check to see if it is beneficial to move interleave down */
	    if (cur->Need_Interleave() &&
		cur->Defs()->Elements() > 0 &&
		cur->Better_Move_Interleave_Down()) {

		/* move the interleave down */
		changed = true;
		for (INT j = 0; j < cur->Defs()->Elements(); j++) {
		    SIMD_EINFO *def = cur->Def(j);
		    def->Set_Need_Interleave();
		    def->Fold_Deinterleave();
		    if (def->Need_Interleave()) {
			interleave_einfo.Push(def);
		    }
		}
		cur->Reset_Need_Interleave();
		cur->Set_Interleaved();
		
		dump_interleave("SIMD preprocess interleave: move need interleave down",
				cur, All_EInfo());
	    }
	}
    }

#ifdef Is_True_On
    for (INT i = 0; i < All_EInfo().Elements(); i++) {
	SIMD_EINFO *cur = All_EInfo().Bottom_nth(i);
	if (cur->Interleaved() && 
	    WN_operator(cur->Expr()) != OPR_MPY &&
	    WN_operator(cur->Expr()) != OPR_CVT &&
	    WN_operator(cur->Expr()) != OPR_CVTL) {
	    for (INT j = 0; j < cur->Defs()->Elements(); j++) {
		SIMD_EINFO *def = cur->Def(j);
		Is_True(def->Interleaved() || def->Need_Interleave(),
			("Expect all operands be interleave"));
		;
	    }
	}
	if (cur->Need_Deinterleave()) {
	    Is_True(cur->Interleaved(), ("Deinterleave on non-interleave"));
	    ;
	}
	if (cur->Need_Interleave()) {
	    for (INT j = 0; j < cur->Defs()->Elements(); j++) {
		SIMD_EINFO *def = cur->Def(j);
		Is_True(!def->Interleaved() || def->Need_Deinterleave(),
		    ("Expect all defs be non-interleaved"));
	    }
	}
    }
#endif

    dump_interleave("After SIMD preprocess interleave", NULL, All_EInfo());
}


void
SIMD_LOOP::Setup_Peeling (void)
{
    // no peeling if opt space is on
    if (OPT_Space) {
	return;
    }

    /* The SNL_2D_Tile routine needs more work to handle the peeled
       version, don't peel for now */
    if (Trapezoidal()) {
	return;
    }
    
    // screen out small trip count loop
    INT simd_level = Simd_Loop_Level();
    INT simd_u = Simd_Unroll()[simd_level];
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(Simd_Loop());
    if (dli->Est_Max_Iterations_Index > 0 &&
	dli->Est_Max_Iterations_Index < 2*simd_u ||
	dli->Est_Num_Iterations < 2*simd_u) {
	return;
    }
    
    // determine if there will be multiple store streams from
    // outer loop unroll
    INT simd_new_order = Loop_Model()->Old_To_New_Order(Simd_Loop_Level());
    for (INT i = 0; i < Num_Loops(); i++) {
	if (Loop_Model()->Block_Number(i) > 1 &&
	    Loop_Model()->Old_To_New_Order(i) < simd_new_order) {
	    return;
	}
    }
    
    // if no write streams at all, no need to peel
    // if more than one write streams, align may cause the others to be unaligned
    if (IMem_Map().Write_Group_Count() != 1) {
	return;
    }

    // single store -- if aligned, no need to peel
    if (IMem_Map().Unaligned_Write_Group_Count() == 0) {
	return;
    }

    // if more than one element, we may never reach the alignment requirement
    IMEM_INFO *imem = IMem_Map().Only_Write_Imem_Info();
    if (imem->Imem_Offset()->Parent_Elem()->Offset_Count() > 1) {
	return;
    }

    // don't peel if
    // this will result in too many unaligned loads, or
    // there are no alignment registers to handle the unaligned loads
    INT in_s_els = IMem_Map().Read_Group_Count();
    INT n_a_regs = Simd_Info->N_A_Regs();
    INT unaligned_loads = IMem_Map().Unaligned_Read_Group_Count();
    if ((n_a_regs==0 && in_s_els!=0) ||
	(in_s_els>n_a_regs && in_s_els!=unaligned_loads)) {
	return;
    }

    // all checks passed, peeling may help
    Set_Peel_Align();
}

/*---------------------------------------------------------------------------*
 * SIMD transformation setup for a given loop model                          *
 *---------------------------------------------------------------------------*/
bool
SIMD_LOOP::Setup_With_Loop_Model() {
    // this is the last good place to give up vectorization
    // at model time
    
    Is_True(Simd_Loop_Level() >= 0, 
	    ("Try to setup SIMD where there is no simd loop"));
    
    IMem_Map().Order_Specific_Setup(Loop_Model(), Num_Loops(), this, true, -1, true);
    
    // Give up if there is a bad write stream
    if (Bad_Stride()) {
      return false;
    }
    
    Setup_Peeling();
    
    return true;
}

void SIMD_LOOP::Set_Has_N_To_W(void)
{ 
    _flag |= N_TO_W;
    if (Simd_Info->Mul_Out_Shift() != 0) {
	/* in this release, we use MUL to get type conversion,
	** if the mul_out_shift is not zero, the value is not preserved */
#if 0
	SIMD_Msg(AT_MSG_SIMD_MUL_SHIFT, Simd_Loop());
#endif
	Set_Bad_Operator();
    }
}

SIMD_LOOP::SIMD_LOOP(MEM_POOL *pool, LOOP_MODEL *lm,
		     WN *simd_loop, INT simd_loop_level) :
    _lm(lm), _pool(pool), _eInfo(101, pool), _orig_ver(pool),
    _num_loops(lm->Num_Loops()), _num_good(lm->Num_Good()),
    _max_vl(0),
    _simd_loop(simd_loop), _simd_loop_level(simd_loop_level),
    _permloop(NULL), _flag(0),
    _VSAR(NULL), _VSAR_expr(NULL), _VSAR_cnt(0), _round_expr(NULL),
    _level_flag(NULL),
    _scalar_map(101, pool), _sinfo_map(101, pool), _cur_scalar(101, pool), 
    _redu_scalar(101, pool), _old_stmt(pool), 
    _inv_ldid(pool), _post_loop(pool),  _scalar_redu(pool), 
    _all_einfo(pool), _shift_scale(pool), _move_cand(pool), 
    _sinfo_stack(pool), _invar_table(NULL), _td_rem(NULL),
    _first_preg(0), _last_preg(0), _if_map(19, pool), _if_conv_map(19, pool),
    _if_ana_map(19, pool), _if_conv(NULL)
{
    
    if (simd_debug) {
	fprintf(TFile,"\n"
		"-----------------------------------------------------------\n"
		"SIMD_LOOP: loop line %d, depth %d\n"
		"-----------------------------------------------------------\n"
		"\n",
		Srcpos_To_Line(LWN_Get_Linenum(simd_loop)), simd_loop_level);
    }
    
    _unroll_candidate = CXX_NEW_ARRAY(EINFO_Stack*, Num_Loops(), pool);
    for (INT i = 0; i < Num_Loops(); ++i) {
      _unroll_candidate[i] = CXX_NEW(EINFO_Stack(pool), pool);
    }
    
    _simd_unroll = CXX_NEW_ARRAY(INT, Num_Loops(), pool);
    memset(_simd_unroll, 0, sizeof(INT) * Num_Loops());
    
    _level_flag  = CXX_NEW_ARRAY(mUINT16, Num_Loops(), pool);
    memset(_level_flag, 0, sizeof(mUINT16) * Num_Loops());
    
    _const_map = CXX_NEW(SIMD_CONST_Map(101,pool),pool);
    _imem_map = CXX_NEW(IMEM_Map(this,pool),pool);
}

void
SIMD_LOOP::Generate_Sel_Imm(WN *stmt, SIMD_PREG *out0, SIMD_PREG *out1,
			    SIMD_PREG *in0, SIMD_PREG *in1, INT64 sel0,
			    INT64 sel1, INT64 sel)
{

    SIMD_PREG *psel_0  = Create_Sel_Preg(sel0);
    SIMD_PREG *psel_1  = Create_Sel_Preg(sel1);

    TYPE_ID    mtype     = out0->Type();
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = Simd_Info->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    WN*        apr[3];

    apr[0] = LWN_CreateParm(mtype, in0->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, in1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(psel_0->Type(), psel_0->Simd_Preg_Ldid(),
			    MTYPE_To_TY(psel_0->Type()), 
			    WN_PARM_BY_VALUE);
    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN  *sto  = out0->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = WN_Whirl_Linenum(stmt);
	
    apr[0] = LWN_CreateParm(mtype, in0->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, in1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(psel_1->Type(), psel_1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(psel_1->Type()), 
			    WN_PARM_BY_VALUE);
    perm = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    sto = out1->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, sto);
    WN_linenum(sto) = WN_Whirl_Linenum(stmt);
}

void
SIMD_LOOP::Generate_Sel_Imm(WN *stmt, SIMD_PREG *out0, 
			    SIMD_PREG *in0, SIMD_PREG *in1, INT64 imm,
			    WN *block)
{
    INT64 line_num = 0;
    if (stmt != NULL) {
	block = LWN_Get_Parent(stmt);
	line_num = LWN_Get_Linenum(stmt);
    }

    SIMD_PREG *psel_0    = Create_Sel_Preg(imm);
    TYPE_ID    mtype     = out0->Type();
    OPERATOR   op        = OPR_SELECT;
    INTRINSIC  intrin_id = Simd_Info->Find_Function(op, mtype);
    OPCODE     intrin_op = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    WN*        apr[3];

    apr[0] = LWN_CreateParm(mtype, in0->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, in1->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(psel_0->Type(), psel_0->Simd_Preg_Ldid(),
			    MTYPE_To_TY(psel_0->Type()), 
			    WN_PARM_BY_VALUE);
    WN  *perm       = LWN_Create_Intrinsic(intrin_op, intrin_id, 3, apr);
    WN  *sto        = out0->Simd_Preg_Stid(perm);
    LWN_Insert_Block_Before(block, stmt, sto);
    WN_linenum(sto) = line_num;
}

SIMD_EINFO* 
SIMD_LOOP::Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO(wn,cur_size,p), p);
    return res;
}

SIMD_EINFO* 
SIMD_LOOP::Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info)
{
    SIMD_EINFO *res = CXX_NEW(SIMD_EINFO(wn,e_info), e_info->Pool());
    return res;
}

void
SIMD_LOOP::Copy_EInfo_Rec(WN *orig_wn, WN *copy_wn)
{
    OPERATOR oper = WN_operator(orig_wn);
    switch (oper) {
	case OPR_BLOCK:
	{
	    WN *kid   = WN_first(orig_wn);
	    WN *kid_c = WN_first(copy_wn);
	    while (kid) {
		Copy_EInfo_Rec(kid, kid_c);
		kid   = WN_next(kid);
		kid_c = WN_next(kid_c);
	    }
	    return;
	}
	
	case OPR_DO_LOOP:
	    return Copy_EInfo_Rec(WN_do_body(orig_wn), WN_do_body(copy_wn));
	    
	case OPR_IF:
	    Copy_EInfo_Rec(WN_if_test(orig_wn), WN_if_test(copy_wn));
	    Copy_EInfo_Rec(WN_then(orig_wn), WN_then(copy_wn));
	    Copy_EInfo_Rec(WN_else(orig_wn), WN_else(copy_wn));
	    return;
	    
	default:
	{
	    SIMD_EINFO *orig_einfo = Get_E_Info(orig_wn);
	    Is_True(orig_einfo != NULL, ("Null einfo"));
	    SIMD_EINFO *copy_einfo = Create_Simd_EInfo(copy_wn, orig_einfo);
	    copy_einfo->Set_IMem(orig_einfo->IMem());
	    E_Info().Enter(copy_wn, copy_einfo);

	    if (OPERATOR_is_store(oper) || oper == OPR_SHL || oper == OPR_ASHR
		|| oper == OPR_LSHR || oper == OPR_DIV) {
		Copy_EInfo_Rec(WN_kid0(orig_wn), WN_kid0(copy_wn));
	    } else if (!OPERATOR_is_load(oper)) {
		for (INT i = 0; i < WN_kid_count(orig_wn); i++) {
		    Copy_EInfo_Rec(WN_kid(orig_wn, i), WN_kid(copy_wn, i));
		}
	    }
	    return;
	}
    }
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:


