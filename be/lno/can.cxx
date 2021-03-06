
/* 
   Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.
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


//-*-c++-*-
//                     CAN
//                     ---
//
// Description:
//
//     Loop level canonincization.
//	Mark loops with all sorts of information. 
//
//

/* ====================================================================
 * ====================================================================
 *
 * Module: can.c  
 * $Revision: 1.129 $
 * $Date: 2000/09/14 17:42:40 $
 * $Author: mpm $
 * $Source: /isms/cmplrs.src/osprey1.0/be/lno/RCS/can.cxx,v $
 *
 * Revision history:
 *  11-28-94 - Original Version
 *
 * Description: Build up do loops and ifs, mark do loops
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

static char *source_file = __FILE__;
static char *rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/be/lno/RCS/can.cxx,v $ $Revision: 1.129 $";

#include "call_info.h"
#include "lnopt_main.h"
#include "lnoutils.h"
#include "lwn_util.h"
#include "opt_alias_interface.h"
#include "opt_du.h"
#include "config_targ.h"
#include "targ_const.h"
#include "targ_sim.h"
#include "wn_simp.h"
#include "reverse.h"
#include "errors.h"
#include "erbe.h"
#include "snl_utils.h"
#include "lego_util.h"
#include "config.h"
#include "be_util.h"
#include "simd.h"
#include "tie.h"
#include "tietypes.h"
#include "simd.h"

#ifdef sun
/* sun wants its long long constants explicitly declared to be so:
   (9223372036854775807LL) instead of (9223372036854775807), but
   because of the elf and dwarf mess we aren't picking up sun's 
   limits.h
*/

#undef INT64_MIN
#define INT64_MIN (0x8000000000000000ll) 
#undef INT64_MAX 
#define INT64_MAX (9223372036854775807ll) 
#endif /*sun*/

extern BOOL Run_autopar_save; 

//
// Exported functions:
//
//     BOOL Mark_Code(WN *func_nd, BOOL promote_pointers=FALSE, 
//	 BOOL strict_limit=TRUE)
//
//              Mark all the do loops and all the IFs in the function using 
//		the DO_LOOP_INFO data structure and the LNO_Info_Map map. 
//		Use LNO_default_pool to store the annotations.
//
//		Get rid of all the labels inside "Good" do loops.
//		These have to be dead.
//
//		For all do loops with indirect bounds, copy the bounds
//		into pregs
//
//	        Also promote pointers inside DO loops into arrays
//
//		Return TRUE if there is a do loop in the code
//

typedef STACK<WN *> STACK_OF_WN;
typedef STACK<DO_LOOP_INFO *> DLI_STACK;
typedef STACK<IF_INFO *> II_STACK;
static void Mark_Code(WN *, WN *, DOLOOP_STACK *,DLI_STACK *, II_STACK *,
			STACK_OF_WN *, HASH_TABLE<INT, WN*> *,
			mUINT8 depth, INT *inner_depth,
			BOOL promote_pointers, BOOL inside_bound, 
		        BOOL strict_limit=TRUE);
static void Dismantle_Do(WN *);
static void Dismantle_Dos(WN *, BOOL);
static BOOL did_dismantle;
static BOOL has_dos;
static INT64 Find_Average(ACCESS_VECTOR *av, BOOL *know_val, 
					DOLOOP_STACK *do_stack);
static void Copy_Loads_In_Bound(WN *do_loop, WN *tmp, BOOL is_start);
static void Remove_16bit_IV_Truncation(WN *stid);
static void Promote_Pointer(WN *wn, INT kid_num, INT load_size);
static void Fold_Array(WN *wn, INT kid_num);
static void Fold_Base(WN *array);
static void Fold_Offset(WN *wn, WN *array);
static void Fold_Intconst(WN *ld_st, WN *intconst, BOOL negate);
static void Enter_Label_Goto_Hash(WN *func_nd,
                                  HASH_TABLE<INT32, WN*> *label_hash,
                                  HASH_TABLE<INT32, WN*> *goto_hash);
static void Delete_Unused_Labels(HASH_TABLE<INT32, WN*> *label_hash,
                                 HASH_TABLE<INT32, WN*> *goto_hash);

static WN *Convert_Alloca_To_Intrinsic   (WN *wn);
static WN *Convert_Dealloca_To_Intrinsic (WN *wn);
extern WN *Convert_Intrinsic_To_Alloca_Dealloca   (WN *wn);

static WN *Remove_Redundant_Convert(WN *wn);

static INT64 constval;

extern BOOL Mark_Code(WN *func_nd, 
		      BOOL promote_pointers, 
		      BOOL strict_limit)
{
  Is_True(WN_opcode(func_nd) == OPC_FUNC_ENTRY,
	("non func_entry in Mark_Code"));
  INT inner_depth=0;
  did_dismantle=FALSE;
  has_dos = FALSE;
  extern BOOL PU_has_manual_prefetch;
  PU_has_manual_prefetch = FALSE;
  MEM_POOL_Push(&LNO_local_pool);
  {
   DLI_STACK *dlistack = CXX_NEW(DLI_STACK(&LNO_local_pool),&LNO_local_pool); 
   DOLOOP_STACK *stack=CXX_NEW(DOLOOP_STACK(&LNO_local_pool),&LNO_local_pool);

   II_STACK *if_stack = CXX_NEW(II_STACK(&LNO_local_pool),&LNO_local_pool); 
   STACK_OF_WN *label_stack = 
	CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool); 
   HASH_TABLE<INT32, WN*> label_hash(50,&LNO_local_pool);
   HASH_TABLE<INT32, WN*> goto_hash(50,&LNO_local_pool);
   Enter_Label_Goto_Hash(func_nd,&label_hash, &goto_hash);
   Mark_Code(func_nd,func_nd,stack,dlistack,if_stack,label_stack,&label_hash,
	0,&inner_depth, promote_pointers,FALSE,strict_limit);
   Delete_Unused_Labels (&label_hash, &goto_hash);
   LWN_Simplify_Tree(func_nd);
   if (did_dismantle) Remark_Depth(func_nd,0);
  }
  MEM_POOL_Pop(&LNO_local_pool);
  return has_dos;
}

static void Move_To_PU_Pragma_List(WN* pragma_wn, WN* func_nd);
static void Error_Check_MP_Pragmas (WN* wn);
static void Mark_Concurrent_Call(WN *wn);

// Fix for 653090. 

static void Patch_Loop_Statement_Expression(WN* wn_exp,
					    WN* wn_loop)
{
  DEF_LIST* def_list = Du_Mgr->Ud_Get_Def(wn_exp);
  if (def_list != NULL && SYMBOL(wn_exp) != SYMBOL(WN_index(wn_loop))
      && def_list->Loop_stmt() == wn_loop) { 
    WN* wn_enclosing_loop = Enclosing_Proper_Do_Loop(wn_exp);
    def_list->Set_loop_stmt(wn_enclosing_loop);
    DEF_LIST_ITER iter(def_list);
    const DU_NODE* node = NULL;
    const DU_NODE* nnext = NULL;
    for (node = iter.First(); !iter.Is_Empty(); node = nnext) {
      nnext = iter.Next();
      WN* wn_def = node->Wn();
      if (Wn_Is_Inside(wn_def, wn_loop))
	Du_Mgr->Delete_Def_Use(wn_def, wn_exp);
    }
  }
  for (INT i = 0; i < WN_kid_count(wn_exp); i++)
    Patch_Loop_Statement_Expression(WN_kid(wn_exp, i), wn_loop);
} 

static void Patch_Loop_Statements(WN* wn_loop)
{ 
  Patch_Loop_Statement_Expression(WN_start(wn_loop), wn_loop);
  Patch_Loop_Statement_Expression(WN_end(wn_loop), wn_loop);
  Patch_Loop_Statement_Expression(WN_step(wn_loop), wn_loop);
} 

static INT Loop_Depth_Limit(BOOL strict_limit)
{
  INT upper_limit = LNO_MAX_DO_LOOP_DEPTH - 1;
  if (strict_limit)
    upper_limit = MIN(LNO_MAX_DO_LOOP_DEPTH - 1, LNO_Max_Do_Loop_Depth_Strict);
  return upper_limit; 
} 

static INT Nest_Total(WN* wn_region)
{ 
  WN* wn_first = WN_first(WN_region_pragmas(wn_region));
  for (WN* wn = wn_first; wn != NULL; wn = WN_next(wn)) {
    if (WN_opcode(wn) == OPC_PRAGMA) { 
      switch (WN_pragma(wn)) { 
      case WN_PRAGMA_PDO_BEGIN:
      case WN_PRAGMA_DOACROSS:
      case WN_PRAGMA_PARALLEL_DO:
	return WN_pragma_arg2(wn);
      } 
    } 
  } 
  return -1; 
} 

//-----------------------------------------------------------------------
// NAME: Dismantle_Nested_Region 
// FUNCTION: Remove the region 'wn_region' while retaining the code within
//   the region.
//-----------------------------------------------------------------------

static void Dismantle_Nested_Region(WN* wn_region)
{
  WN* wnn = NULL;
  WN* wn_after = wn_region;
  WN* wn_first = WN_first(WN_region_body(wn_region));
  for (WN* wn = wn_first; wn != NULL; wn = wnn) {
    wnn = WN_next(wn);
    LWN_Extract_From_Block(wn);
    LWN_Insert_Block_After(LWN_Get_Parent(wn_region), wn_after, wn);
    wn_after = wn;
  }
  LWN_Extract_From_Block(wn_region);
  LWN_Delete_Tree(wn_region);
}

//-----------------------------------------------------------------------
// NAME: Dismantle_Nested_Doacross
// FUNCTION: Discard remnants of an incomplete nested doacross and print
//   a warning saying that they have been discarded.
//-----------------------------------------------------------------------

static WN* Dismantle_Nested_Doacross(WN* wn_region,
				     INT tile_count) 
{ 
  WN* wn_start = WN_first(WN_region_body(wn_region));
  WN* wn = 0;
  for (wn = wn_start; wn != NULL; wn = WN_next(wn))
    if (WN_operator(wn) == OPR_DO_LOOP)
      break; 
  FmtAssert(wn != NULL, ("Dismantle_Nested_Doacross: Could not find DO"));
  WN* wn_outer_loop = wn; 
  WN* wn_inner_loop = SNL_Get_Inner_Snl_Loop(wn_outer_loop, tile_count);
  wn_start = wn_inner_loop;
  INT i;
  for (i = 0; i < tile_count; i++) {
    WN* wn = 0;
    for (wn = wn_start; wn != NULL; wn = LWN_Get_Parent(wn))
      if (WN_opcode(wn) == OPC_REGION)
        break;
    WN* wn_region = wn;
    FmtAssert(wn_region != NULL, ("Could not find enclosing region"));
    wn_start = LWN_Get_Parent(wn_region);
    Dismantle_Nested_Region(wn_region);
  }
  ErrMsgSrcpos(EC_LNO_Generic, WN_Get_Linenum(wn_outer_loop),
    "Nested Do Across Loop is Too Deep, Directive Ignored\n");
  return wn_outer_loop; 
}


// func_nd is the pointer to the PU. This is used to move "PU-level"
// pragma nodes (e.g. prefetch(manual) on/off) to the beginning of the PU.
// depth is how many do loop surround you
// stack contains the WNs of all the outermore do loops
// dlistack describes all the outermore do loop infos
// if_stack describes all the outermore ifs
// if the loop is good, label_stack contains all the labels in the do loop
//   otherwise, label_stack is undefined
// label_hash is a hash table mapping every label number to its WN
// innner depth is set to the maximum depth of do loops inside of you
static void Mark_Code(WN *wn, WN *func_nd, DOLOOP_STACK *stack,
                      DLI_STACK *dlistack, II_STACK *if_stack,
                      STACK_OF_WN *label_stack, 
		      HASH_TABLE<INT32,WN*> *label_hash,
		      mUINT8 depth,INT *inner_depth,
                      BOOL promote_pointers, BOOL inside_bound,
		      BOOL strict_limit)
{
  WN *kid;
  DO_LOOP_INFO *dli;

  Is_True(wn,("Null wn in Mark_Code"));
  
  OPCODE opcode = WN_opcode(wn);
  OPERATOR oper = OPCODE_operator(opcode);
  
  if (opcode == OPC_BLOCK) {
    // walk the kids backwards so that we'll see a do before its pragmas
    *inner_depth=0;
    UINT8 tmp=0;
    kid = WN_last (wn);
    while (kid) {
      if (!LNO_Ignore_Pragmas && 
	   WN_operator(kid) == OPR_PRAGMA) {
        if ((WN_pragma(kid) == WN_PRAGMA_PREFETCH) ||
            (WN_pragma(kid) == WN_PRAGMA_PREFETCH_MANUAL) ||
            (WN_pragma(kid) == WN_PRAGMA_PREFETCH_REF_DISABLE)) {
          WN* prev_kid = WN_prev(kid);
          Move_To_PU_Pragma_List (kid, func_nd);
          kid = prev_kid;
          continue;
        }
        if (WN_pragma(kid) == WN_PRAGMA_PREFETCH_REF) {
          extern BOOL PU_has_manual_prefetch;
          PU_has_manual_prefetch = TRUE;
          WN* pf_wn = WN_next(kid);
          FmtAssert (pf_wn && (WN_opcode(pf_wn) == OPC_PREFETCH),
                     ("FE Error: pragma prefetch_ref not followed by prefetch\n"));
          WN_pf_set_confidence(pf_wn, 3);
        } else if (WN_pragma(kid) == WN_PRAGMA_INLINE_BODY_START ||
	           WN_pragma(kid) == WN_PRAGMA_INLINE_BODY_END) {
          WN* prev_kid = WN_prev(kid);
          LWN_Delete_Tree_From_Block (kid);
	  kid = prev_kid;
          continue;
	}
      }
      WN *prev_kid = WN_prev(kid);
      Mark_Code(kid,func_nd,stack,dlistack,if_stack,label_stack,label_hash,
		depth,inner_depth,promote_pointers,inside_bound,strict_limit);
      tmp = MAX(tmp,*inner_depth);
      kid = prev_kid;
    }
    *inner_depth = tmp;
    return;
  }
  
  if (!LNO_Ignore_Pragmas && oper==OPR_PRAGMA) {
    if (WN_pragma(wn) == WN_PRAGMA_KAP_ASSERT_PERMUTATION) {
      ST *st = WN_st(wn);
      if (Permutation_Arrays->Elements() > 50) {
	// avoid n squared problems in extremely weird cases
	  ErrMsgSrcpos(EC_LNO_Generic,WN_Get_Linenum(wn),
  		"Two many permuatation directives.  Extra one ignored.");
      } else {
	BOOL found = FALSE;
        for (INT i=0; i<Permutation_Arrays->Elements() && !found; i++) {
	  ST *tmp = Permutation_Arrays->Bottom_nth(i)._st;
	  if (tmp == st) {
	    found = TRUE;
          }
        }
	if (!found) Permutation_Arrays->Push(PERMUTATION_DESCRIPTOR(st));
      }
    } else if (WN_pragma(wn) == WN_PRAGMA_KAP_OPTIMIZE) {
      if (WN_pragma_arg1(wn) == 0) {
	Run_autopar = FALSE;
      }
    } else if (WN_pragma(wn) == WN_PRAGMA_KAP_CONCURRENTIZE) {
      if (Run_autopar_save) 
        Run_autopar = TRUE; 
    } else if (WN_pragma(wn) == WN_PRAGMA_KAP_NOCONCURRENTIZE) {
	Run_autopar = FALSE;
    } else if (WN_pragma(wn) == WN_PRAGMA_KAP_ROUNDOFF) {
      if (WN_pragma_arg1(wn) <= 1) {
	Roundoff_Level = ROUNDOFF_NONE;
      }
    } else if (WN_pragma(wn) == WN_PRAGMA_ALIGNED) {
      if (Simd_Info) {
	ST *st = WN_st(wn);
	SYMBOL sym(st, 0, ST_mtype(st));
	UINT32 align = WN_pragma_arg1(wn);
	Is_True(st, ("Invalid ST in #pragma aligned"));
	Is_True((align != 0 && (align & (align - 1)) == 0),
		("Invalid alignment in #pragma"));
	if (simd_debug) {
	  fprintf(TFile, "Found #pragma aligned(%s, %d)\n", sym.Name(), align);
	}

	ALIGNMENT_TABLE *atab = Simd_Info->Alignment_Table();
	INT old_align = atab->Find(sym);
	if (old_align) {
	  /* Already there, record the more conservative alignment. */
	  if (align < old_align) {
	    atab->Remove(sym);
	    atab->Enter(sym, align);
	  }
	} else {
	  atab->Enter(sym, align);
	}
      }
    }
  }
  
  if (oper==OPR_CVT) {
    // optimize out redundant converts -- helps autotie analysis
    // also, may benefit modeling
    WN *new_wn = Remove_Redundant_Convert(wn);
    if (new_wn!=NULL) {
      Mark_Code(new_wn,func_nd,stack,dlistack,if_stack,label_stack,label_hash,
                depth,inner_depth,promote_pointers,inside_bound,strict_limit);
      return;
    }
  }

  if (opcode == OPC_DO_LOOP) {
    label_stack = CXX_NEW(STACK_OF_WN(&LNO_local_pool),&LNO_local_pool); 
    has_dos = TRUE;
    dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (!dli) {
      dli = (DO_LOOP_INFO *) 
       CXX_NEW(DO_LOOP_INFO(&LNO_default_pool,NULL,NULL,NULL,FALSE,FALSE,FALSE,
        FALSE,FALSE,FALSE,FALSE,TRUE), &LNO_default_pool);
      dli->Depth = depth;
      dli->Is_Backward = FALSE;
      WN_MAP_Set(LNO_Info_Map,wn,(void *)dli);
    } else {
      dli->Has_Calls=FALSE;
      dli->Has_Non_Intrinsic_Calls=FALSE;
      dli->Has_Unsummarized_Calls=FALSE;
      dli->Has_Gotos_This_Level=FALSE;
      dli->Has_Exits=FALSE;
      dli->Is_Inner=TRUE;
    }
    WN* wn_region = LWN_Get_Parent(LWN_Get_Parent(wn));
    if (Is_Mp_Region(wn_region)) {
      Contains_MP = TRUE;
      Patch_Loop_Statements(wn);
      WN* wn_first_pragma = WN_first(WN_region_pragmas(wn_region));
      FmtAssert(WN_opcode(wn_first_pragma) == OPC_PRAGMA,
                ("Found a non-pragma as first statement in MP pragma region"));
      while (wn_first_pragma) {
        if (WN_opcode(wn_first_pragma) == OPC_PRAGMA &&
            (WN_pragma(wn_first_pragma) == WN_PRAGMA_DOACROSS ||
             WN_pragma(wn_first_pragma) == WN_PRAGMA_PDO_BEGIN ||
             WN_pragma(wn_first_pragma) == WN_PRAGMA_PARALLEL_DO))
          break;
        wn_first_pragma = WN_next(wn_first_pragma);
      }
      if (wn_first_pragma &&
          (WN_pragma(wn_first_pragma) == WN_PRAGMA_DOACROSS ||
           WN_pragma(wn_first_pragma) == WN_PRAGMA_PDO_BEGIN ||
           WN_pragma(wn_first_pragma) == WN_PRAGMA_PARALLEL_DO)) {
        Error_Check_MP_Pragmas (wn_first_pragma);
        if (dli->Mp_Info == NULL)
          dli->Mp_Info = CXX_NEW(MP_INFO(WN_region_pragmas(wn_region)),
                                 &LNO_default_pool);
      }
    }
    depth++;
    INT i;
    for (i=0; i<dlistack->Elements(); i++) {
      dlistack->Bottom_nth(i)->Is_Inner = FALSE;
    }
    for (i=0; i<if_stack->Elements(); i++) {
      if_stack->Bottom_nth(i)->Contains_Do_Loops = TRUE;
    }
    stack->Push(wn);
    dlistack->Push(dli);

  } else if (opcode == OPC_REGION) {
    if (Is_Mp_Region(wn) && Nest_Total(wn) > Loop_Depth_Limit(strict_limit)) {
      wn = Dismantle_Nested_Doacross(wn, Nest_Total(wn));
      Mark_Code(wn ,func_nd, stack, dlistack,if_stack, label_stack, label_hash,
        depth, inner_depth, promote_pointers, inside_bound, strict_limit);
      return; 
    } 
    if (Is_Mp_Region(wn)) 
      Contains_MP = TRUE; 
    for (INT i=0; i<if_stack->Elements(); i++) {
      if_stack->Bottom_nth(i)->Contains_Regions = TRUE;
    }
    REGION_INFO* rgi = NULL; 
    WN* wn_first = WN_first(WN_region_pragmas(wn)); 
    if (wn_first != NULL && WN_opcode(wn_first) == OPC_PRAGMA 
	&& WN_pragma(wn_first) == WN_PRAGMA_PARALLEL_BEGIN) { 
      rgi = (REGION_INFO *) CXX_NEW(REGION_INFO(FALSE), &LNO_default_pool); 
      WN_MAP_Set(LNO_Info_Map, wn, (void *) rgi);  
    } 

    // Disable pseudo-lowering for parallel loops containing exception
    // handling regions (EH regions). This is because pseudo-lowering
    // creates pregs that span EH regions, which is a bad thing ---
    // GRA in CG may not preserve them across these EH regions.
    if (WN_region_is_EH(wn)) {
      WN *pwn = LWN_Get_Parent(wn);
      while (pwn) {
	if (WN_opcode(pwn) == OPC_DO_LOOP) {
	  DO_LOOP_INFO *dli = Get_Do_Loop_Info(pwn);
	  if (dli && dli->Mp_Info) {
	    // must be a parallel loop
	    dli->Mp_Info->Disable_Plowering();
	  }
	}
	pwn = LWN_Get_Parent(pwn);
      }
    }
  } else if (opcode == OPC_IF) {
    IF_INFO *ii = (IF_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    if (!ii) {
      ii = CXX_NEW(IF_INFO(&LNO_default_pool,FALSE,FALSE),&LNO_default_pool);
      WN_MAP_Set(LNO_Info_Map,wn,(void *)ii);
    } else {
      ii->Contains_Do_Loops = FALSE;
      ii->Contains_Regions  = FALSE;
    }
    if_stack->Push(ii);
  } else if (OPCODE_is_call(opcode)) {
    if (!Has_Call_Info(wn)) {
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
        dlistack->Bottom_nth(i)->Has_Unsummarized_Calls = TRUE;
      }
    }
    for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
      dlistack->Bottom_nth(i)->Has_Calls = TRUE;
      if (OPCODE_operator(opcode)!=OPR_INTRINSIC_CALL) {
	dlistack->Bottom_nth(i)->Has_Non_Intrinsic_Calls = TRUE;
      }
    }
  } else if (OPCODE_operator(opcode) == OPR_ALLOCA ||
             OPCODE_operator(opcode) == OPR_DEALLOCA) {

    // This is a hack. Since LNO handles intrinsic calls,
    // we're lazy and convert ALLOCA/DEALLOCA WHIRL nodes to the
    // corresponding intrinsic call equivalents.
    // The right solution, of course, would be to make LNO recognize
    // alloca/dealloca and treat them right. This is not trivial ---
    // e.g. thr dependence graph updates are tricky...

    FmtAssert (Alloca_Dealloca_On,
               ("Saw an ALLOCA/DEALLOCA, but Alloca_Dealloca_On is false"));

    WN *new_wn = ((OPCODE_operator(opcode) == OPR_ALLOCA) ?
                  Convert_Alloca_To_Intrinsic (wn) :
                  Convert_Dealloca_To_Intrinsic (wn));
    if (new_wn) {
      wn = new_wn;
      Mark_Code(wn,func_nd,stack,dlistack,if_stack,label_stack,label_hash,
                depth,inner_depth,promote_pointers,inside_bound,strict_limit);
    }
  } else if (opcode == OPC_IO ||
	     (oper==OPR_FORWARD_BARRIER) || (oper==OPR_BACKWARD_BARRIER)) {
    if (!Array_Dependence_Graph || !Array_Dependence_Graph->Get_Vertex(wn)) {
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
	DO_LOOP_INFO *dli = dlistack->Bottom_nth(i);
        dli->Has_Calls = TRUE;
        dli->Has_Non_Intrinsic_Calls = TRUE;
        dli->Has_Unsummarized_Calls = TRUE;
        dli->Has_Bad_Mem = TRUE;
        dli->Has_Barriers = TRUE;
      }
    }
  } else if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)) {
    // initialize Has_Bad_Mem, we need this to catch the cases that
    // dependence analysis isn't called on a loop for some other bad reason
    // In such cases Has_Bad_Mem is TRUE if there are any indirect refs
    if ((oper != OPR_LDID) && (oper != OPR_STID)) {  
      if (!Array_Dependence_Graph || !Array_Dependence_Graph->Get_Vertex(wn)) {
        for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
          dlistack->Bottom_nth(i)->Has_Bad_Mem = TRUE;
        }
      }
      if (promote_pointers && dlistack->Elements()) {
        if ((oper == OPR_ILOAD) && 
	    (WN_operator(WN_kid0(wn)) != OPR_ARRAY)) {
          Promote_Pointer(wn,0,0);
        } else if ((oper == OPR_ISTORE) &&
		   (WN_operator(WN_kid1(wn)) != OPR_ARRAY)) {
          Promote_Pointer(wn,1,0);
        }
      }
      if (LNO_Fold_Arrays && dlistack->Elements()) {
        if ((oper == OPR_ILOAD) && 
	    (WN_operator(WN_kid0(wn)) == OPR_ARRAY)) {
          Fold_Array(wn,0);
        } else if ((oper == OPR_ISTORE) &&
		   (WN_operator(WN_kid1(wn)) == OPR_ARRAY)) {
          Fold_Array(wn,1);
        }
      }
    } 
  } else if (opcode == OPC_LABEL) {
    label_stack->Push(wn);
  } else if (opcode == OPC_IO_ITEM &&
	  (WN_io_item(wn) == IOC_END || WN_io_item(wn) == IOC_ERR ||
				WN_io_item(wn) == IOC_EOR)) {
      // be conservative for weird I/O (it doesn't hurt to be conservative since
      // we don't optimize I/O anyway yet)
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
        dlistack->Bottom_nth(i)->Has_Gotos = TRUE;
        dlistack->Bottom_nth(i)->Has_Gotos_This_Level = TRUE;
        dlistack->Bottom_nth(i)->Has_Exits = TRUE;
      }
  } else if (OPCODE_is_non_scf(opcode) ||
	     (opcode == OPC_DO_WHILE) ||
	     (opcode == OPC_WHILE_DO) ||
	     (opcode == OPC_COMPGOTO) ) {
    if (opcode == OPC_GOTO || opcode == OPC_TRUEBR || opcode == OPC_FALSEBR) {
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
        dlistack->Bottom_nth(i)->Has_Gotos = TRUE;
      }
      // For which loops does the goto exit the loops
      WN *label = label_hash->Find(WN_label_number(wn));
      FmtAssert (label, ("goto to non-existant label"));
      MEM_POOL_Push(&LNO_local_pool);
      {
        DOLOOP_STACK label_loops(&LNO_local_pool);
	Build_Doloop_Stack(label,&label_loops);
	if (label_loops.Elements()) {
	  Get_Do_Loop_Info(label_loops.Top_nth(0))->Has_Gotos_This_Level = TRUE;
        }
	INT i=0;
	INT min = MIN(stack->Elements(),label_loops.Elements());
	while (i<min &&
	       (stack->Bottom_nth(i) == label_loops.Bottom_nth(i))) i++;
	for (i=i; i<dlistack->Elements()-inside_bound; i++) {
          dlistack->Bottom_nth(i)->Has_Exits = TRUE;
        }
      }
      MEM_POOL_Pop(&LNO_local_pool);
    } else if (opcode == OPC_DO_WHILE || opcode == OPC_WHILE_DO) {
      SIMD_Msg(AT_MSG_SIMD_NON_COUNTABLE_LOOP, wn);
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
        dlistack->Bottom_nth(i)->Has_Gotos = TRUE;
      }
    } else {
      for (INT i=0; i<dlistack->Elements()-inside_bound; i++) {
        dlistack->Bottom_nth(i)->Has_Gotos = TRUE;
        dlistack->Bottom_nth(i)->Has_Gotos_This_Level = TRUE;
        dlistack->Bottom_nth(i)->Has_Exits = TRUE;
      }
    }
  } 

  UINT8 tmp=0;
  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    if ((WN_operator(kid) == OPR_LDID) &&
        (Wn_Is_Intconst(kid,&constval))) {
      WN_kid(wn,kidno) = LWN_Make_Icon(WN_rtype(kid),constval);
      LWN_Set_Parent(WN_kid(wn,kidno),wn);
      LWN_Delete_Tree(kid);
    } else {
      BOOL ib = inside_bound;
      if (opcode == OPC_DO_LOOP) {
	if (kid == WN_do_body(wn)) {
	  ib = 0;
        } else {
	  ib = 1;
        }
      }
      Mark_Code(kid,func_nd, stack,dlistack,if_stack,label_stack,label_hash,
	depth, inner_depth, promote_pointers,ib,strict_limit);
      tmp = MAX(tmp,*inner_depth);
    }
  }
  *inner_depth=tmp;

  if (opcode == OPC_DO_LOOP) {
    (*inner_depth)++;
    if ((*inner_depth) > Loop_Depth_Limit(strict_limit)) { 
      Dismantle_Dos(wn, strict_limit);
    } else {
      if (!dlistack->Top_nth(0)->Has_Gotos) {
	// remove all the labels from the code

        // This is dangerous --- we know that this label
        // is not jumped to within this loop, but 
        // it could be a target from sibling code
        // So delay this deletion until later, in
        // Delete_Unsed_Labels
	// for (INT i=0; i<label_stack->Elements(); i++) {
	  // LWN_Delete_Tree(label_stack->Bottom_nth(i));
        // }
      }
      // Do this later, after hoisting array bounds in phase 2 
      // Copy_Loads_In_Bound(wn,WN_kid0(WN_start(wn)),TRUE);
      // Copy_Loads_In_Bound(wn,WN_end(wn),FALSE);

      // The following is commented out since an int
      // induction variable casted to short in loop body
      // will have its CVTL removed.
      //// Remove CVTL 16 in the induction variable
      //if (OPT_IV_Short_Wrap_Around_Safe) {
	//Remove_16bit_IV_Truncation(WN_start(wn));
      //}

      DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
      dli->Is_Backward = Do_Loop_Is_Backward(wn);
    }
    stack->Pop();
    dlistack->Pop();
    depth--;
    CXX_DELETE(label_stack,&LNO_local_pool);
  } else if (opcode == OPC_IF) {
    if_stack->Pop();
  } else if (opcode == OPC_PRAGMA) {
    // deal with loop directives
    if (!LNO_Ignore_Pragmas && 
	 ((WN_pragma(wn) == WN_PRAGMA_IVDEP) ||
	  (WN_pragma(wn) == WN_PRAGMA_CRI_CNCALL) ||
	  (WN_pragma(wn) == WN_PRAGMA_NORECURRENCE) ||
	  (WN_pragma(wn) == WN_PRAGMA_SIMD_IF_CONVERT) ||
	  ((WN_pragma(wn) == WN_PRAGMA_KAP_ASSERT_DO) &&
	   (WN_pragma_arg1(wn) == ASSERT_DO_CONCURRENT)) || 
	  (WN_pragma(wn) == WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL))) {
      WN *tmp = WN_next(wn);
      WN *do_after_pragma = NULL;
      BOOL done=FALSE;
      while (!done) {
	if (!tmp) {
	  done = TRUE;
        } else {
	  OPCODE opcode = WN_opcode(tmp);
	  if (OPCODE_is_scf(opcode)) {
	    if (opcode == OPC_DO_LOOP) {
	      done = TRUE;
	      do_after_pragma = tmp;
            } else if (Is_Mp_Region(tmp)) {
	      tmp = WN_first(WN_region_body(tmp));
	    } else {
	      done = TRUE;
            }
	  } else {
	    tmp = WN_next(tmp);
          }
        }
      }
      if (do_after_pragma) {
	DO_LOOP_INFO *dli = Get_Do_Loop_Info(do_after_pragma);
	if (WN_pragma(wn) == WN_PRAGMA_IVDEP) {
          if (!dli->Is_Inner) {
	    ErrMsgSrcpos(EC_LNO_Generic,WN_Get_Linenum(wn),
		"IVDEP on non-inner loop ignored");
	  } else {
	    dli->Is_Ivdep = TRUE;
	    LWN_Delete_From_Block(LWN_Get_Parent(wn),wn);
	  }
	} else if ((WN_pragma(wn) == WN_PRAGMA_KAP_ASSERT_DO) &&
	           (WN_pragma_arg1(wn) == ASSERT_DO_CONCURRENT)) {
	  dli->Concurrent_Directive = TRUE;
	  dli->Pragma_Prefer_Concurrentize = TRUE; 
	  LWN_Delete_From_Block(LWN_Get_Parent(wn),wn);
	} else if (WN_pragma(wn) == WN_PRAGMA_NORECURRENCE) {
	  dli->Concurrent_Directive = TRUE;
	  LWN_Delete_From_Block(LWN_Get_Parent(wn),wn);
	} else if (WN_pragma(wn) == WN_PRAGMA_SIMD_IF_CONVERT) {
	  dli->SIMD_If_Convert_Directive = TRUE;
	  LWN_Delete_From_Block(LWN_Get_Parent(wn),wn);
        } else if ((WN_pragma(wn) == WN_PRAGMA_CRI_CNCALL) ||
		  (WN_pragma(wn) == WN_PRAGMA_KAP_ASSERT_CONCURRENT_CALL)) {
          dli->Is_Concurrent_Call = TRUE;
	  LWN_Delete_From_Block(LWN_Get_Parent(wn),wn);
	  if (!dli->Is_Inner) {
	    Mark_Concurrent_Call(WN_do_body(do_after_pragma));
          }
        }
      } else {
        WN_PRAGMA_ID pid = (WN_PRAGMA_ID)WN_pragma(wn);
        if (Simd_Target != SIMD_TARG_UNKNOWN &&
            (pid == WN_PRAGMA_KAP_ASSERT_DO ||
             pid == WN_PRAGMA_SIMD_IF_CONVERT)) {
          WN *line_wn = tmp ?: wn;
          SIMD_Msg(AT_MSG_SIMD_PRAGMA_IGNORED, line_wn,
                   (INT)Srcpos_To_Line(LWN_Get_Linenum(wn)));
        } else {
	  ErrMsgSrcpos(EC_LNO_Generic,WN_Get_Linenum(wn),
                       "DO loop corresponding to pragma not found");
        }
      }
    } 
  } else if (OPCODE_operator(opcode) == OPR_ARRAY) {
    Fold_Base(wn);
  }
}

// Mark all dos nested below here as concurrent calls
static void Mark_Concurrent_Call(WN *wn)
{
  OPCODE opc = WN_opcode(wn);
  if (opc == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Mark_Concurrent_Call(kid);
      kid = WN_next(kid);
    }
    return;
  } 
  if (opc == OPC_DO_LOOP) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(wn);
    dli->Is_Concurrent_Call = TRUE;
  }
  if (OPCODE_is_scf(opc)) {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Mark_Concurrent_Call(WN_kid(wn,kidno));
    }
  }
}


/***********************************************************************
 *
 * Given an MP pragma node (one of doacross, pdo_begin, or parallel_do)
 * perform the following error checks:
 *  If nested parallelism, then
 *      - cannot have schedtype == dynamic/gss/runtime
 *      - cannot have both onto and affinity
 *      - cannot have thread affinity
 *
 * Give warnings and delete erroneous pragmas, if any.
 *
 ***********************************************************************/
static void Error_Check_MP_Pragmas (WN* wn) {
  if (WN_pragma(wn) != WN_PRAGMA_DOACROSS &&
      WN_pragma(wn) != WN_PRAGMA_PDO_BEGIN &&
      WN_pragma(wn) != WN_PRAGMA_PARALLEL_DO) {
    DevWarn ("Error_Check_MP_Pragmas -- expected doacross/pdo/parallel-do\n");
    return;
  }

  INT nest_total = WN_pragma_arg2(wn);
  if (nest_total > 1) {
    /* nested parallel loop */
    WN* pwn = WN_first(LWN_Get_Parent(wn));
    WN *onto_wn = NULL, *aff_wn = NULL;

    /* cannot have dynamic/gss/runtime schedtypes */
    while (pwn) {
      if (WN_opcode(pwn) == OPC_PRAGMA) {
        if (WN_pragma(pwn) == WN_PRAGMA_MPSCHEDTYPE &&
            (WN_pragma_arg1(pwn) == WN_PRAGMA_SCHEDTYPE_DYNAMIC ||
             WN_pragma_arg1(pwn) == WN_PRAGMA_SCHEDTYPE_GSS ||
             WN_pragma_arg1(pwn) == WN_PRAGMA_SCHEDTYPE_RUNTIME)) {
          /* these schedtypes not allowed with nested parallelism */
          ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(pwn),
                WN_pragmas[WN_pragma(pwn)].name,
                "currently not supported with nested parallelism (ignoring).");
          WN_pragma_arg1(pwn) = WN_PRAGMA_SCHEDTYPE_SIMPLE;
        }
      }
      if (WN_opcode(pwn) == OPC_XPRAGMA) {
        if (!onto_wn && WN_pragma(pwn) == WN_PRAGMA_ONTO) onto_wn = pwn;
        if (!aff_wn && WN_pragma(pwn) == WN_PRAGMA_AFFINITY) aff_wn = pwn;
      }
      pwn = WN_next(pwn);
    }

    /* cannot have both onto and affinity */
    if (onto_wn && aff_wn) {
      ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(onto_wn),
                   WN_pragmas[WN_pragma(onto_wn)].name,
                   "not allowed with affinity (ignoring).");
      /* delete the ONTO nodes */
      while (onto_wn &&
             WN_opcode(onto_wn) == OPC_XPRAGMA &&
             WN_pragma(onto_wn) == WN_PRAGMA_ONTO) {
        WN* tmp = WN_next(onto_wn);
        LWN_Delete_Tree_From_Block (onto_wn);
        onto_wn = tmp;
      }
    }

    /* cannot have thread affinity */
    if (aff_wn) {
      WN* tmp_wn = aff_wn;
      while (tmp_wn &&
             WN_opcode(tmp_wn) == OPC_XPRAGMA &&
             WN_pragma(tmp_wn) == WN_PRAGMA_AFFINITY) {
        tmp_wn = WN_next(tmp_wn);
      }
      Is_True (tmp_wn && WN_opcode(tmp_wn) == OPC_XPRAGMA &&
               (WN_pragma(tmp_wn) == WN_PRAGMA_DATA_AFFINITY ||
                WN_pragma(tmp_wn) == WN_PRAGMA_THREAD_AFFINITY),
               ("Must have either data or thread affinity\n"));

      if (WN_pragma(tmp_wn) == WN_PRAGMA_THREAD_AFFINITY) {
        ErrMsgSrcpos(EC_LNO_Bad_Pragma_String, WN_Get_Linenum(tmp_wn),
                     WN_pragmas[WN_pragma(tmp_wn)].name,
                     "not supported with nested parallelism (ignoring).");
        /* delete the affinity nodes */
        while (aff_wn &&
               WN_opcode(aff_wn) == OPC_XPRAGMA &&
               (WN_pragma(aff_wn) == WN_PRAGMA_AFFINITY ||
                WN_pragma(aff_wn) == WN_PRAGMA_THREAD_AFFINITY)) {
          WN* tmp = WN_next(aff_wn);
          LWN_Delete_Tree_From_Block(aff_wn);
          aff_wn = tmp;
        }
        /* delete the affinity-pragma replication in the do-body */
        WN* do_wn=WN_first(WN_region_body(LWN_Get_Parent(LWN_Get_Parent(wn))));
        while (do_wn) {
          if (WN_operator(do_wn) == OPR_DO_LOOP) break;
          do_wn = WN_next(do_wn);
        }
        Is_True (do_wn, ("Missing parallel do loop\n"));
          
        INT i;
        for (i=1; i<nest_total; i++) {
          do_wn = WN_first(WN_do_body(do_wn));
          while (do_wn && (WN_operator(do_wn) != OPR_REGION)) {
            do_wn = WN_next(do_wn);
          }
          FmtAssert (do_wn,
                     ("nested-doacross: cannot find nested region %d\n", i));
          do_wn = WN_first(WN_region_body(do_wn));
          while (do_wn && (WN_operator(do_wn) != OPR_DO_LOOP)) {
            do_wn = WN_next(do_wn);
          }
          FmtAssert(do_wn,
                    ("nested-doacross: missing doloop in MP region %d\n",i));
        }
        /* now do_wn is the last do loop. Find the body,
         * then the affinity xpragmas.
         */
        do_wn = WN_first(WN_do_body(do_wn));
        while (do_wn &&
               WN_opcode(do_wn) == OPC_XPRAGMA &&
               (WN_pragma(do_wn) == WN_PRAGMA_AFFINITY ||
                WN_pragma(do_wn) == WN_PRAGMA_THREAD_AFFINITY)) {
          WN* tmp = WN_next(do_wn);
          LWN_Delete_Tree_From_Block(do_wn);
          do_wn = tmp;
        }
      }
    }
  }
}

/***********************************************************************
 *
 * Given a PU-level pragma node:
 *  - make a duplicate
 *  - insert duplicate at end of pragma-list at the head of the PU
 *  - delete original from the tree
 *
 ***********************************************************************/
static void Move_To_PU_Pragma_List (WN* pragma_wn, WN* func_nd) {
  WN* copy_wn = LWN_Copy_Tree (pragma_wn);
  LWN_Delete_From_Block (NULL, pragma_wn);
  WN* plist_wn = WN_func_pragmas(func_nd);
  Is_True (WN_opcode(plist_wn) == OPC_BLOCK,
           ("Body of PU is not a block"));
  LWN_Insert_Block_Before (plist_wn, NULL, copy_wn);
}

// Set the estimated number of iterations for a do loop given that
// on the stack we have the WNs for the outer loops
void DO_LOOP_INFO::Set_Est_Num_Iterations(DOLOOP_STACK *do_stack)
{
  Num_Iterations_Symbolic = FALSE; 
  ACCESS_VECTOR *svec = Step;
  if (svec->Too_Messy || !svec->Is_Const() || (svec->Const_Offset == 0)) {
    Est_Num_Iterations = LNO_Num_Iters;
    Num_Iterations_Symbolic = TRUE; 
    return;
  }
  INT64 step = svec->Const_Offset;
  ACCESS_ARRAY *la, *ua;
  if (step > 0) {
    la = LB;
    ua = UB;
  } else {
    la = UB;
    ua = LB;
    step = -step;
  }
  
  if (la->Too_Messy || ua->Too_Messy) {
    Est_Num_Iterations = LNO_Num_Iters;
    if (Est_Max_Iterations_Index >= 0 &&
        Est_Max_Iterations_Index < Est_Num_Iterations) {
      Est_Num_Iterations = Est_Max_Iterations_Index;
      Num_Iterations_Symbolic = FALSE; 
    } else {
      Num_Iterations_Symbolic = TRUE; 
    }
    return;
  }

  INT64 low,up,r;
  BOOL know_val;

  MEM_POOL_Push(&LNO_local_pool);

  // common case
  INT num_dim = la->Dim(0)->Nest_Depth();
  if ((la->Num_Vec() == 1) && (ua->Num_Vec() == 1) &&
      (!la->Dim(0)->Too_Messy) && (!ua->Dim(0)->Too_Messy) &&
      (abs(la->Dim(0)->Loop_Coeff(num_dim-1)) == 1) &&
      (abs(ua->Dim(0)->Loop_Coeff(num_dim-1)) == 1)) {
    ACCESS_VECTOR *range = Add(ua->Dim(0),la->Dim(0),&LNO_local_pool);
    range->Negate_Me();
    range->Const_Offset = -range->Const_Offset;
    r = Find_Average(range,&know_val,do_stack);
    if (!know_val) {
      Est_Num_Iterations = LNO_Num_Iters;
      if (Est_Max_Iterations_Index >= 0 &&
          Est_Max_Iterations_Index < Est_Num_Iterations) {
        Est_Num_Iterations = Est_Max_Iterations_Index;
        Num_Iterations_Symbolic = FALSE; 
      } else {
        Num_Iterations_Symbolic = TRUE; 
      }
    } else {
      Est_Num_Iterations = (r + 1) / step;
    }
    if (Est_Num_Iterations < 0)
      Est_Num_Iterations = 1; 
    MEM_POOL_Pop(&LNO_local_pool);
    return;
  }

  // multiple upper or lower bounds, need max of lower, min of upper
  // if all are symbolic, we set to Num_Iterations_Symbolic
  // if some are symbolic, we set to the minimum of the non-symbolic ranges
  //	and Num_Iterations_Symbolic (i.e. given do i = max(1,n),min(5,m),
  //	we know there can't be more than 4 iterations)

  BOOL seen_symb = FALSE;

  low = INT64_MIN;
  up = INT64_MAX;
  ACCESS_VECTOR *lb = la->Dim(0)->Convert_Bound_To_Exp(&LNO_local_pool);
  INT64 this_low;
  if (la->Dim(0)->Loop_Coeff(num_dim-1)) {
    this_low = Find_Average(lb,&know_val,do_stack)/
      abs(la->Dim(0)->Loop_Coeff(num_dim-1));
  }
  else {
    this_low = Find_Average(lb,&know_val,do_stack)/1;
  }
  if (!know_val) {
    seen_symb = TRUE;
    Num_Iterations_Symbolic = TRUE; 
  } else {
    low = this_low;
  }
  INT i;
  for (i=1; i<la->Num_Vec(); i++) {
    lb = la->Dim(i)->Convert_Bound_To_Exp(&LNO_local_pool);
    if (la->Dim(0)->Loop_Coeff(num_dim-1)) {
      this_low = Find_Average(lb,&know_val,do_stack)/
	abs(la->Dim(0)->Loop_Coeff(num_dim-1));
    } else {
      this_low = Find_Average(lb,&know_val,do_stack)/1;
    }
    if (!know_val) {
      seen_symb = TRUE;
      Num_Iterations_Symbolic = TRUE; 
    } else {
      low = MAX(low,this_low);
    }
  }

  if (ua->Dim(0)->Too_Messy) {
    Est_Num_Iterations = LNO_Num_Iters;
    if (Est_Max_Iterations_Index >= 0 &&
        Est_Max_Iterations_Index < Est_Num_Iterations) {
      Est_Num_Iterations = Est_Max_Iterations_Index;
      Num_Iterations_Symbolic = FALSE; 
    } else {
      Num_Iterations_Symbolic = TRUE; 
    }
    MEM_POOL_Pop(&LNO_local_pool);
    return;
  }
  ACCESS_VECTOR *ub = ua->Dim(0)->Convert_Bound_To_Exp(&LNO_local_pool);
  INT64 this_up = Find_Average(ub,&know_val,do_stack)/ 
		abs(ua->Dim(0)->Loop_Coeff(num_dim-1));
  if (!know_val) {
    seen_symb = TRUE;
    Num_Iterations_Symbolic = TRUE; 
  } else {
    up = this_up;
  }
  for (i=1; i<ua->Num_Vec(); i++) {
    ub = ua->Dim(i)->Convert_Bound_To_Exp(&LNO_local_pool);
    this_up = Find_Average(ub,&know_val,do_stack)/
	abs(ua->Dim(0)->Loop_Coeff(num_dim-1));
    if (!know_val) {
      seen_symb = TRUE;
      Num_Iterations_Symbolic = TRUE; 
    } else {
      up = MIN(up,this_up);
    }
  }

  if (seen_symb) {
    Num_Iterations_Symbolic = TRUE; 
    if ((up != INT64_MAX) && (low != INT64_MIN)) {
      Est_Num_Iterations = MIN(LNO_Num_Iters,(up - low + 1) / step);
    } else {
      Est_Num_Iterations = LNO_Num_Iters;
    }
    if (Est_Max_Iterations_Index >= 0 &&
        Est_Max_Iterations_Index < Est_Num_Iterations) {
      Est_Num_Iterations = Est_Max_Iterations_Index;
      Num_Iterations_Symbolic = FALSE; 
    }
  } else {
    Est_Num_Iterations = (up - low + 1) / step;
  }
  if (Est_Num_Iterations < 0) 
    Est_Num_Iterations = 1; 
  MEM_POOL_Pop(&LNO_local_pool);
}


// Find the average value of this access vector
// set *know_val to true iff we know the average value
static INT64 Find_Average(ACCESS_VECTOR *av, BOOL *know_val, 
					DOLOOP_STACK *do_stack) 
{
  if (av->Too_Messy || av->Contains_Lin_Symb() || 
      av->Contains_Non_Lin_Symb()) {
    *know_val = FALSE;
    return 0;
  }

  INT64 result = av->Const_Offset;
  if (av->Has_Loop_Coeff()) {  // check for triangular components
    for (INT i = 0; i<av->Nest_Depth(); i++) {
      if (av->Loop_Coeff(i)) {  // find bounds on triangular component
	INT64 up,low;  // average value for lower and upper bound of 'i'
	BOOL know_bound;

	DO_LOOP_INFO *dli = 
	   (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,do_stack->Bottom_nth(i));
	ACCESS_VECTOR *step = dli->Step;
	if (!step->Is_Const()) {
	  *know_val = FALSE;
	  return 0;
        }

	ACCESS_ARRAY *la,*ua;
	if (step->Const_Offset > 0) {
	  la = dli->LB;
	  ua = dli->UB;
        } else {
	  la = dli->UB;
	  ua = dli->LB;
        }

	if (la->Too_Messy || ua->Too_Messy) {
	  *know_val = FALSE;
	  return(0);
        }
        ACCESS_VECTOR *lb = la->Dim(0)->Convert_Bound_To_Exp(&LNO_local_pool);
	if (lb->Too_Messy) {
	  *know_val = FALSE;
	  return(0);
        }
	low = Find_Average(lb,&know_bound,do_stack) /
		abs(la->Dim(0)->Loop_Coeff(la->Dim(0)->Nest_Depth()-1));
	if (!know_bound) {
	  *know_val = FALSE;
	  return(0);
        }
        INT b;
	for (b=1; b<la->Num_Vec(); b++) {
          lb = la->Dim(b)->Convert_Bound_To_Exp(&LNO_local_pool);
	  INT tmp = Find_Average(lb,&know_bound,do_stack)/
	               abs(la->Dim(b)->Loop_Coeff(la->Dim(b)->Nest_Depth()-1));
	  low = MAX(low, tmp);
	  if (!know_bound) {
	    *know_val = FALSE;
	    return(0);
          }
        }

        ACCESS_VECTOR *ub = ua->Dim(0)->Convert_Bound_To_Exp(&LNO_local_pool);
	if (ub->Too_Messy) {
	  *know_val = FALSE;
	  return(0);
        }
	up = Find_Average(ub,&know_bound,do_stack) /
		abs(ua->Dim(0)->Loop_Coeff(ua->Dim(0)->Nest_Depth()-1));
	if (!know_bound) {
	  *know_val = FALSE;
	  return(0);
        }
	for (b=1; b<ua->Num_Vec(); b++) {
          ub = ua->Dim(b)->Convert_Bound_To_Exp(&LNO_local_pool);
	  INT tmp = Find_Average(ub,&know_bound,do_stack)/
	    abs(ua->Dim(b)->Loop_Coeff(ua->Dim(b)->Nest_Depth()-1));
	  up = MIN(up,tmp);
	  if (!know_bound) {
	    *know_val = FALSE;
	    return(0);
          }
        }

	result = result + av->Loop_Coeff(i) * (low + up)/2;
      }
    }
  }
  *know_val = TRUE;
  return (result);
}

// set to NULL the loop stmt of anything pointing to "loop"
// this is used when loop is dismantled
static void Dismantle_Do_Fix_Loop_Stmt(WN *loop, WN *wn)
{
  DEF_LIST *dl = Du_Mgr->Ud_Get_Def(wn);
  if (dl && (dl->Loop_stmt() == loop)) {
    dl->Set_loop_stmt(NULL);
  }
  if (WN_opcode(wn) == OPC_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Dismantle_Do_Fix_Loop_Stmt(loop,kid);
      kid = WN_next(kid);
    }
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      WN *kid = WN_kid(wn,kidno);
      Dismantle_Do_Fix_Loop_Stmt(loop,kid);
    }
  }
}

// convert a DO into a while
static void Dismantle_Do(WN *wn)
{
  SIMD_Msg(AT_MSG_SIMD_LOOP_TOO_DEEP, wn);
  did_dismantle = TRUE;
  Dismantle_Do_Fix_Loop_Stmt(wn,wn);
  WN *whiledo = LWN_CreateWhileDo(WN_end(wn),WN_do_body(wn));
  LWN_Insert_Block_Before(LWN_Get_Parent(wn),wn,whiledo);
  LWN_Insert_Block_Before(LWN_Get_Parent(wn),whiledo,WN_start(wn));
  LWN_Insert_Block_Before(WN_while_body(whiledo),NULL,WN_step(wn));
  CXX_DELETE(Get_Do_Loop_Info(wn), &LNO_default_pool);
  LWN_Delete_From_Block(LWN_Get_Parent(wn),wn); 
}

// Walk the tree rooted at 'wn_loop', which is enclosed by the loops in
// 'st_loops' and add loops to 'st_dismantle' which should be dismantled.
// We try dismantling the outermost DO which is not an MP loop.  If none
// exists, we will dismantle an MP loop.  

static void Dismantle_Do_Walk(WN* wn_loop, 
			      DOLOOP_STACK* st_loops,
			      DOLOOP_STACK* st_dismantle,
			      BOOL strict_limit)
{
  if (WN_opcode(wn_loop) == OPC_DO_LOOP) {
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
    if (Do_Depth(wn_loop) >= Loop_Depth_Limit(strict_limit)) {
      INT i;
      for (i = 0; i < st_loops->Elements(); i++) {
	WN* wn_candidate = st_loops->Bottom_nth(i);
	DO_LOOP_INFO* dli_candidate = Get_Do_Loop_Info(wn_candidate); 
        if (dli_candidate->Mp_Info == NULL) {
          INT j;
	  for (j = 0; j < st_dismantle->Elements(); j++)
	    if (st_dismantle->Bottom_nth(j) == wn_candidate)
	      break;
	  if (j == st_dismantle->Elements())
	    st_dismantle->Push(wn_candidate); 
	  break;
        }
      }  
      if (i > 0 && i == st_loops->Elements())
	st_dismantle->Push(st_loops->Bottom_nth(0));
    } else {
      st_loops->Push(wn_loop);
      for (WN* wn = WN_first(WN_do_body(wn_loop)); wn != NULL; wn = WN_next(wn))
	Dismantle_Do_Walk(wn, st_loops, st_dismantle, strict_limit);
      st_loops->Pop();  
    }
  } else if (WN_opcode(wn_loop) == OPC_BLOCK) {
    for (WN* wn = WN_first(wn_loop); wn != NULL; wn = WN_next(wn))
      Dismantle_Do_Walk(wn, st_loops, st_dismantle, strict_limit);
  } else {
    for (INT i = 0; i < WN_kid_count(wn_loop); i++)
      Dismantle_Do_Walk(WN_kid(wn_loop, i), st_loops, st_dismantle,
	strict_limit); 
  }
} 
     
static void Dismantle_Dos(WN* wn_loop,
			  BOOL strict_limit)
{
  FmtAssert(strict_limit, 
    ("Dismantle_Dos: Should not need to dismantle dos in this case"));
  if (!Contains_MP) {
    Dismantle_Do(wn_loop); 
    return; 
  }
  DOLOOP_STACK st_loops(&LNO_local_pool);  
  DOLOOP_STACK st_dismantle(&LNO_local_pool);  
  Dismantle_Do_Walk(wn_loop, &st_loops, &st_dismantle, strict_limit);
  for (INT i = 0; i < st_dismantle.Elements(); i++) 
    Dismantle_Do(st_dismantle.Bottom_nth(i)); 
}  

// Go through the code agin to set the depth field as this has been changed
// by dismantling
static void Remark_Depth(WN *wn, DLI_STACK *do_stack,mUINT8 depth)
{
  WN *kid;
  DO_LOOP_INFO *dli;

  Is_True(wn,("Null wn in Remark_Depth"));

  OPCODE opcode = WN_opcode(wn);

  if (opcode == OPC_BLOCK) {
    kid = WN_first (wn);
    while (kid) {
      Remark_Depth(kid,do_stack,depth);
      kid = WN_next(kid);
    }
    return;
  } 

  if (opcode == OPC_DO_LOOP) {
    dli = (DO_LOOP_INFO *) WN_MAP_Get(LNO_Info_Map,wn);
    Is_True(dli,("no mapping in Remark_Depth"));
    dli->Depth = depth;
    depth++;
    do_stack->Push(dli);
  } 

  for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
    kid = WN_kid(wn,kidno);
    Remark_Depth(kid,do_stack,depth);
  }

  if (opcode == OPC_DO_LOOP) {
    do_stack->Pop();
    depth--;
  } 
}

// copy all non-ldid loads from the bound to before the loop
static void Copy_Loads_In_Bound(WN *do_loop, WN *wn, BOOL is_start)
{
  OPCODE opcode = WN_opcode(wn);
  if (OPCODE_is_load(opcode) && (OPCODE_operator(opcode) != OPR_LDID)) {
    WN *parent = LWN_Get_Parent(wn);
    TYPE_ID type = OPCODE_rtype(opcode);
    ST *preg_st = MTYPE_To_PREG(type);
    OPCODE ldid_opc = OPCODE_make_op(OPR_LDID,Promote_Type(type),type);
    OPCODE stid_opc = OPCODE_make_op(OPR_STID,MTYPE_V,type);
    WN_OFFSET preg_num;
#ifdef _NEW_SYMTAB
    if (is_start) {
      preg_num = Create_Preg(type, "lb");
    } else {
      preg_num = Create_Preg(type, "ub");
    }
#else
    if (is_start) {
      preg_num = Create_Preg(type, "lb",NULL);
    } else {
      preg_num = Create_Preg(type, "ub",NULL);
    }
#endif
    WN *ldid = WN_CreateLdid(ldid_opc,preg_num,preg_st,Be_Type_Tbl(type));
    LWN_Set_Parent(ldid,parent);
    Create_alias(Alias_Mgr,ldid);
    WN *stid = LWN_CreateStid(stid_opc,preg_num,preg_st,
                              Be_Type_Tbl(type),wn);
    Create_alias(Alias_Mgr,stid);
    LWN_Copy_Linenumber(do_loop,stid);
    LWN_Insert_Block_Before(LWN_Get_Parent(do_loop),do_loop,stid);

    INT kidno = 0;
    while (WN_kid(parent,kidno) != wn) kidno++;

    WN_kid(parent,kidno) = ldid;
    Du_Mgr->Add_Def_Use(stid,ldid);
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Copy_Loads_In_Bound(do_loop,WN_kid(wn,kidno),is_start);
    }
  }
}

static void Remove_16bit_IV_Truncation(WN *stid)
{
  Is_True(WN_operator(stid) == OPR_STID, 
	  ("Remove_16bit_Induction_Variable_Truncation: Not a STID"));

  USE_LIST* use_list = Du_Mgr->Du_Get_Use(stid);
  if (use_list) {
    USE_LIST_ITER iter_use(use_list);
    for (DU_NODE *use_node = iter_use.First(); !iter_use.Is_Empty();
	 use_node = (DU_NODE *) iter_use.Next()) {
      WN *use = use_node->Wn();
      Is_True(use, ("Null pointer in DEF-USE chain"));
      WN *p_wn = LWN_Get_Parent(use);
      while (p_wn != NULL && 
	     OPERATOR_is_expression(WN_operator(p_wn)) &&
	     !OPERATOR_is_load(WN_operator(p_wn))) {
	OPERATOR oper = WN_operator(p_wn);
	if (oper == OPR_CVTL && WN_offset(p_wn) == 16 &&
	    (WN_rtype(p_wn) == MTYPE_I4 || WN_rtype(p_wn) == MTYPE_U4)) {
	  WN *g_wn = LWN_Get_Parent(p_wn);
	  if (g_wn) {
	    for (INT kid_no = 0; kid_no < WN_kid_count(g_wn); kid_no++) {
	      if (WN_kid(g_wn, kid_no) == p_wn) {
		WN_kid(g_wn, kid_no) = WN_kid0(p_wn);
		LWN_Set_Parent(WN_kid0(p_wn), g_wn);
		break;
	      }
	    }
	    WN_Delete(p_wn);
	    p_wn = g_wn;
	    continue;
	  }
	}
	p_wn = LWN_Get_Parent(p_wn);
      }
    }
  }
}


static bool
is_likely_base (WN *wn)
{
  OPERATOR oper = WN_operator(wn);
  if (oper == OPR_LDA)
    return true;
  
  if (oper == OPR_LDID) {
    if ((TY_kind(ST_type(WN_st(wn))) == KIND_POINTER))
      return true;
    
    if ((TY_kind(WN_ty(wn)) == KIND_POINTER))
      return true;
  }
  
  return false;
}


/* For 1-byte loads, try to transform the address expression 'addr'
   so that we can extract the correct base and index. */
static bool
init_char_canon (WN *addr)
{
  OPERATOR addr_oper = WN_operator(addr);
  if ((addr_oper != OPR_ADD) && (addr_oper != OPR_SUB)) {
    return false;
  }
  
  OPERATOR kid0_oper = WN_operator(WN_kid0(addr));
  OPERATOR kid1_oper = WN_operator(WN_kid1(addr));
  
  if ((kid0_oper == OPR_MPY) || (kid1_oper == OPR_MPY)) {
    return false;
  }
  
  /* Try to guess which kid is the base and which is the index
     if we guess wrong, we won't promote, but nothing else bad happens. */

  /* Initial guess -- the LDID/LDA kid is the base. */
  INT base_idx = 1;
  if (((kid0_oper == OPR_LDID) || (kid0_oper == OPR_LDA)) ||
      ((kid1_oper != OPR_LDID) && (kid1_oper != OPR_LDA))) {
    base_idx = 0;
  }

  /* Try to improve the guess by prefering LDA or pointer LDIDs. */
  for (INT i = 0; i < 2; i++)
    {
      if (is_likely_base(WN_kid(addr, i))) {
        base_idx = i;
        break;
      }
    }

  INT index_idx = 1 - base_idx;
  
  BOOL simp_state_save = WN_Simplifier_Enable(FALSE);

  TYPE_ID rtype = WN_rtype(WN_kid(addr, index_idx));
  WN *multiply = LWN_CreateExp2(OPCODE_make_op(OPR_MPY, rtype, MTYPE_V),
                                LWN_Make_Icon(rtype, 1), WN_kid(addr, index_idx));
  WN_kid(addr, index_idx) = multiply;
  LWN_Set_Parent(multiply, addr);
  
  WN_Simplifier_Enable(simp_state_save);

  return true;
}


// find the integer constant coefficient for expressions of the form (coeff * (...)),
// and return its WN. return NULL if no coefficient can be found.
static WN *
Find_Term_Coeff(WN *wn) {
  if (WN_operator(wn) == OPR_MPY) {
    WN *coeff = Find_Term_Coeff(WN_kid0(wn));
    if (coeff != NULL) {
      return coeff;
    }
    coeff = Find_Term_Coeff(WN_kid1(wn));
    return coeff;
  }
  
  if (WN_operator(wn) == OPR_INTCONST) {
    return wn;
  }
  
  return NULL;
}


static bool
Check_Addr_Base_and_Mult (WN *addr, INT &base_idx, INT &mult_idx)
{
  OPERATOR addr_oper = WN_operator(addr);
  if (addr_oper != OPR_ADD && addr_oper != OPR_SUB)
    return false;
  
  for (INT i = 0; i < 2; i++) {
    if (WN_operator(WN_kid(addr, i)) != OPR_MPY)
      continue;

    /* The base must be kid 0 for OPR_SUB. */
    if (i == 0 && addr_oper == OPR_SUB)
      continue;
    
    base_idx = 1 - i;
    mult_idx = i;
    return true;
  }
  
  return false;
}


// Try to promote a pointer load/store into an array
//
// At the top level, wn is a load/store and kid_num is the number of
//  the kid of wn that points to the address
// We recurse on addtion in which case wn is the parent of an add and
//  kid_num is the number of the kid of wn that points to the add
// This is just a pattern search.  After then cannonicization of 
// preopt, we hope that all "pointer array" expressions will 
// look like this.
// We're searching for the pattern load/store(+/- base (* ... c)) or
// its communitative equivalents, where 'c' is an INTCONST the size
// of the descriptor of the load/store
// base is an LDID, an LDA, an ARRAY or an ADD/SUB
// if it's an array, it must be one-d with an element size a multiple of c
//
// If base is an array, we try to move the child of the multiply into the 
// array.  If base is an ldid/lda, we create a new array
//
// Now we also promote the patter load/store(ldid/lda) into 
// load/store(array ldid 0)
static void Promote_Pointer(WN *wn, INT kid_num, INT load_size)
{
  OPCODE opcode = WN_opcode(wn);
  WN *addr;
  INT addr_num = kid_num;
  addr = WN_kid(wn,addr_num);
  OPERATOR addr_oper = WN_operator(addr);
  WN *mult, *base, *index;
  INT kid_that_is_base, kid_that_is_mult;
  
  // how big is the load
  if (!load_size) {
    if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)) {
      TYPE_ID mtype = OPCODE_desc(opcode);
      if (MTYPE_is_tie(mtype) || MTYPE_is_xtbool(mtype)) {
	// need to go through the TY because it has the correct size
	// with respect to alignment, i.e. a 20-byte type that has
	// a 16-byte alignment requirement has a size of 32
	TY_IDX ty_idx = MTYPE_To_TY(mtype);
	load_size = TY_size(ty_idx);
      } else {
	switch (mtype) {
        case MTYPE_I1 : case MTYPE_U1: load_size=1; break;
        case MTYPE_I2 : case MTYPE_U2: load_size =2; break;
        case MTYPE_I4 : case MTYPE_U4: case MTYPE_F4: load_size=4; break;
        case MTYPE_I8 : case MTYPE_U8: case MTYPE_F8: case MTYPE_C4:
	  load_size = 8; break;
        case MTYPE_C8 : case MTYPE_FQ: 
	  load_size = 16; break;
        case MTYPE_CQ :
	  load_size = 32; break;
        default: return;
	}
      }
    } else {
      return;
    }
  }

  // promote load/store(ldid/lda)
  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)) {
   if ((addr_oper == OPR_LDID) || (addr_oper == OPR_LDA)) {
    OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type, MTYPE_V);
    WN *array = WN_Create(op_array,3);
    WN_element_size(array) = abs(load_size);
    WN_array_base(array) = addr;
    LWN_Set_Parent(addr,array);
    WN_array_index(array,0) = WN_CreateIntconst(OPC_I4INTCONST,0);
    LWN_Set_Parent(WN_array_index(array,0),array);
    WN_array_dim(array,0) = WN_CreateIntconst(OPC_I4INTCONST,0);
    LWN_Set_Parent(WN_array_dim(array,0),array);
    WN_kid(wn,addr_num) = array;
    LWN_Set_Parent(array,wn);
    Fold_Offset(wn,array);
    return;
   }
  }

  // cannonicize character arrays if the multiply by 1 is missing
  BOOL char_canon = FALSE;
  if (load_size == 1) {

    // reassociate if there are two ADD, #3317
    if (addr_oper == OPR_ADD) {
      WN *k0 = WN_kid0(addr);
      WN *k1 = WN_kid1(addr);
      if (WN_operator(k0) == OPR_LDID && 
	  (ST_class(WN_st(k0)) == CLASS_PREG ||
	   TY_kind(ST_type(WN_st(k0))) != KIND_POINTER) &&
	  WN_operator(k1) == OPR_ADD) {
	WN *g0 = WN_kid0(k1);
	WN *g1 = WN_kid1(k1);
	if ((WN_operator(g0) == OPR_LDID || WN_operator(g0) == OPR_LDA) && 
	    TY_kind(ST_type(WN_st(g0))) == KIND_POINTER) {
	  WN_kid0(addr) = g0;
	  LWN_Set_Parent(g0, addr);
	  WN_kid0(k1) = k0;
	  LWN_Set_Parent(k0, k1);
	} else if ((WN_operator(g1) == OPR_LDID || WN_operator(g1) == OPR_LDA)
		   && TY_kind(ST_type(WN_st(g1))) == KIND_POINTER) {
	  WN_kid0(addr) = g1;
	  LWN_Set_Parent(g1, addr);
	  WN_kid1(k1) = k0;
	  LWN_Set_Parent(k0, k1);
	} else if (WN_operator(g0) == OPR_ADD) {
	  WN *gg0 = WN_kid0(g0);
	  if ((WN_operator(gg0) == OPR_LDID || WN_operator(gg0) == OPR_LDA) && 
	      TY_kind(ST_type(WN_st(gg0))) == KIND_POINTER) {
	    WN_kid0(addr) = gg0;
	    LWN_Set_Parent(gg0, addr);
	    WN_kid0(g0) = k0;
	    LWN_Set_Parent(k0, g0);
	  }
	} else if (WN_operator(g1) == OPR_ADD) {
	  WN *gg1 = WN_kid0(g1);
	  if ((WN_operator(gg1) == OPR_LDID || WN_operator(gg1) == OPR_LDA) && 
	      TY_kind(ST_type(WN_st(gg1))) == KIND_POINTER) {
	    WN_kid0(addr) = gg1;
	    LWN_Set_Parent(gg1, addr);
	    WN_kid0(g1) = k0;
	    LWN_Set_Parent(k0, g1);
	  }
	}
      }
    }

    /* Tweak the address expression, if possible, to make it easier to identify
       the base and the index for 'char' accesses. */
    char_canon = init_char_canon(addr);
  }

  // Given LD/ST (+ INT ...) fold the INT into the LD/ST offset
  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)) {
    if ((addr_oper == OPR_ADD) || (addr_oper == OPR_SUB)) {
      if (WN_operator(WN_kid0(addr)) == OPR_INTCONST) {
        Fold_Intconst(wn,WN_kid0(addr),FALSE);
        addr = WN_kid(wn,addr_num);
        addr_oper = WN_operator(addr);
      } else if (WN_operator(WN_kid1(addr)) == OPR_INTCONST) {
        if (addr_oper == OPR_ADD) {
          Fold_Intconst(wn,WN_kid1(addr),FALSE);
        } else {
          Fold_Intconst(wn,WN_kid1(addr),TRUE);
        }
        addr = WN_kid(wn,addr_num);
        addr_oper = WN_operator(addr);
      }
    }
  } 

  if (Check_Addr_Base_and_Mult(addr, kid_that_is_base, kid_that_is_mult)) {
    Is_True(kid_that_is_base != kid_that_is_mult,
            ("Unexpected base/mult indices."));
    base = WN_kid(addr, kid_that_is_base);
    mult = WN_kid(addr, kid_that_is_mult);
  } else {
    /* Can't promote. */
    if (char_canon) {
      LWN_Simplify_Tree(wn);
    }
    return;
  }
  
  // If the base is an add/subtract, build an array for the base and
  // then try to build an array for the load/store
  OPERATOR base_oper = WN_operator(base);
  if ((base_oper == OPR_ADD) || (base_oper == OPR_SUB)) {
    /* Recurse. */
    Promote_Pointer(addr, kid_that_is_base, load_size);
    
    /* The recursive call may optimize away and delete node. We need
       to reinitialize some of the pointers here. */
    addr = WN_kid(wn, addr_num);
    addr_oper = WN_operator(addr);
    
    if (Check_Addr_Base_and_Mult(addr, kid_that_is_base, kid_that_is_mult)) {
      Is_True(kid_that_is_base != kid_that_is_mult,
              ("Unexpected base/mult indices."));
      base = WN_kid(addr, kid_that_is_base);
      mult = WN_kid(addr, kid_that_is_mult);
    } else {
      /* Can't promote. */
      if (char_canon) {
        LWN_Simplify_Tree(wn);
      }
      return;
    }
    
    base_oper = WN_operator(base);
  }
  
  // Make sure the expression is simplified.
  if (!char_canon) {
    mult = LWN_Simplify_Tree(mult);
    if (WN_operator(mult) != OPR_MPY)
      return;
  }
  
  // Try to find the constant coefficient in the expression. We would like to have
  // an expression of the form coeff * index_expr.
  int reassoc_idx = 0;
  WN *index_expr = WN_kid1(mult);
  
  WN *intconst = Find_Term_Coeff(index_expr);
  if (intconst == NULL) {
    reassoc_idx = 1;
    index_expr = WN_kid0(mult);
    intconst = Find_Term_Coeff(index_expr);
    if (intconst == NULL) {
      if (char_canon) LWN_Simplify_Tree(wn);
      return;
    }
  }
  
  INT64 val = WN_const_val(intconst);
  if (abs(val) >= INT32_MAX ||
      (val % load_size) != 0) { // must be a multiple of the element size
    if (char_canon) LWN_Simplify_Tree(wn);
    return;
  }
  
  // Reassociate the expression if necessary to form coeff * index_expr.
  
  WN *intconst_parent = LWN_Get_Parent(intconst);
  Is_True(intconst_parent != NULL, ("Missing parent"));
  
  INT intconst_idx = LWN_Kid_Index(intconst_parent, intconst);
  Is_True(intconst_idx < WN_kid_count(intconst_parent),
	  ("Can't find the intconst node in its parent"));
  
  if (intconst_parent != mult) {
    // The constant coefficient is not an immediate kid of 'mult' so
    // reassociation is necessary.
    WN *reassoc_expr = WN_kid(mult, reassoc_idx);
    WN_kid(intconst_parent, intconst_idx) = reassoc_expr;
    LWN_Set_Parent(reassoc_expr, intconst_parent);
    WN_kid(mult, reassoc_idx) = intconst;
    LWN_Set_Parent(intconst, mult);
    
    // Reset any 16-bit multiplication info on the INTCONST parent MPY node
    Is_True(WN_operator(intconst_parent) == OPR_MPY,
	    ("Expected MPY operator not %s", OPERATOR_name(WN_operator(intconst_parent))));
    WN_Reset_Mpy_16Bit(intconst_parent);
    WN_Reset_Mpy_16Bit_Set(intconst_parent);
  } else {
    // Expression is already in the desired form, so set the index_expr to
    // the non-intconst kid
    index_expr = WN_kid(mult, reassoc_idx);
  }

#if 0  
  // FIXME: Don't do this for now. Apparently we use the coefficient so that
  // the correct array access gets generated for array of structures.
  // Using the original coefficient as the array access is not ok only for whirl2c.
  // I guess we can use the high-level type to make a smarter decision here.

  // To make sure that we generate an array access with the correct element size,
  // we split "coeff * index" into "load_size * (coeff' * index)". The subsequent
  // code assumes that (coeff' * index) is the actual array access index.
  OPCODE mpy_op = OPCODE_make_op(OPR_MPY, WN_rtype(index_expr), MTYPE_V);
  INT index_expr_idx = LWN_Kid_Index(mult, index_expr);
  index_expr = LWN_CreateExp2(mpy_op,
			      LWN_Make_Icon(WN_rtype(index_expr),
					    val / load_size),
			      index_expr);
  WN_kid(mult, index_expr_idx) = index_expr;
  LWN_Set_Parent(index_expr, mult);
  WN_const_val(intconst) = load_size;
  val = load_size;
#endif
  
  if (base_oper == OPR_ARRAY) {
    // base is an array
    if (WN_kid_count(base) != 3) {
      if (char_canon) LWN_Simplify_Tree(wn);
      return;  // not 1-d
    }
    if ((WN_element_size(base) % val) != 0) {
      if (val % WN_element_size(base) == 0) { // separate out element size
        OPCODE index_op = WN_opcode(index_expr);
        OPCODE mpy_op = OPCODE_make_op(OPR_MPY, OPCODE_rtype(index_op), MTYPE_V);
        index_expr = LWN_CreateExp2(mpy_op,
				    LWN_Make_Icon(OPCODE_rtype(index_op),
						  val / WN_element_size(base)),
				    index_expr);
        for (INT i=0; i<WN_kid_count(index_expr); i++) {
          LWN_Set_Parent(WN_kid(index_expr,i),index_expr);
        }
        val = WN_element_size(base);
      } else {
        if (char_canon) LWN_Simplify_Tree(wn);
        return;
      }
    }
  } else if ((base_oper != OPR_LDID) && (base_oper != OPR_LDA)) {
    if (char_canon) LWN_Simplify_Tree(wn);
    return;
  }

  // The pattern matches, do the substitution.

  WN *array;

  if (base_oper == OPR_ARRAY) { // push the array up
    array = base;
    INT64 ratio = abs(WN_element_size(array) / val);
    WN_kid(wn,addr_num) = array;
    LWN_Set_Parent(array,wn);

    WN *array_index;
    if (ratio == 1) {
      array_index = WN_array_index(array,0);
      WN_Delete(intconst);
      WN_Delete(mult);
    } else {
      // multiply array by ratio
      WN_kid0(mult) = intconst;
      WN_const_val(intconst) = ratio;
      WN_kid1(mult) = WN_array_index(array,0);
      LWN_Set_Parent(WN_array_index(array,0),mult);
      array_index = mult;
    }
    WN_element_size(array) = abs(val);

    WN_kid(addr,kid_that_is_base) = array_index;
    LWN_Set_Parent(array_index,addr);

    WN_array_index(array,0) = addr;
    LWN_Set_Parent(addr,array);

    if (val < 0) {
      OPCODE index_op = WN_opcode(index_expr);
      index_expr = LWN_CreateExp1(OPCODE_make_op(OPR_NEG,OPCODE_rtype(index_op),
			MTYPE_V),index_expr);
    }
    WN_kid(addr,kid_that_is_mult) = index_expr;
    LWN_Set_Parent(index_expr,addr); 

  } else { // create an array

    BOOL negate;
    if (addr_oper == OPR_SUB) {
      negate = (val > 0);
    } else {
      negate = (val < 0);
    }

    WN *new_index = index_expr;
    if (negate) {
      OPCODE index_op = WN_opcode(index_expr);
      new_index = LWN_CreateExp1(OPCODE_make_op(OPR_NEG,OPCODE_rtype(index_op),
			MTYPE_V),index_expr);
    }

    WN_const_val(intconst) = 0;  // we're going to use it for the dimension size
				// zero is what's used for real*8 a(*)
    OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type, MTYPE_V);
    array = WN_Create(op_array,3);
    WN_element_size(array) = abs(val);
    WN_array_base(array) = base;
    LWN_Set_Parent(base,array);
    WN_array_index(array,0) = new_index;
    LWN_Set_Parent(new_index,array);
    WN_array_dim(array,0) = intconst;
    LWN_Set_Parent(intconst,array);

    WN_kid(wn,addr_num) = array;
    LWN_Set_Parent(array,wn);

    WN_Delete(addr);
    WN_Delete(mult);
  }

  if (OPCODE_is_load(opcode) || OPCODE_is_store(opcode)) {
    Fold_Offset(wn,array);
  }

  return;
}

// wn is a load/store
// kid_num is the address, which is an array node
// convert accesses array[i] of array[j] to array[i][j]
// do it recursively to the base
static void Fold_Array (WN *wn, INT kid_num) {
  OPCODE opcode = WN_opcode(wn);
  WN *addr = WN_kid(wn,kid_num);
  
  if (WN_operator(addr) != OPR_ARRAY)
    return;
  
  while (WN_operator(WN_array_base(addr))==OPR_ARRAY) {
    // pattern matched

    WN *addr_sub = WN_array_base(addr);
    INT num_dim_sub = WN_num_dim(addr_sub);
    INT num_dim = WN_num_dim(addr);

    // check legality
    // - all dimensions should be positive constants
    // - the element size of the base should equal the index array size

    for (INT dim_sub =0; dim_sub < num_dim_sub; dim_sub++) {
      WN *wn_dim_sub = WN_array_dim(addr_sub,dim_sub);
      if (WN_operator(wn_dim_sub)!=OPR_INTCONST ||
	  WN_const_val(wn_dim_sub)<=0)
	return;
    }
    
    WN_ESIZE addr_size = WN_element_size(addr);
    WN_ESIZE addr_sub_el_size = WN_element_size(addr_sub);

    if (addr_size<=0 || addr_sub_el_size<=0)
      return;
    
    for (INT dim = 0; dim < num_dim; dim++) {
      WN *wn_dim = WN_array_dim(addr,dim);
      if (WN_operator(wn_dim)!=OPR_INTCONST ||
	  WN_const_val(wn_dim)<=0)
	return;
      addr_size*=WN_const_val(wn_dim);
    }
    
    if (addr_size!=addr_sub_el_size)
      return;
    
    // everything is ok -- make a new array node and set the appropriate fields
	
    INT num_dim_new = num_dim + num_dim_sub;
    
    OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type, MTYPE_V);
    WN *addr_new = WN_Create(op_array,num_dim_new*2+1);
    
    LWN_Set_Parent(addr_new,wn);
    WN_kid(wn,kid_num) = addr_new;
    
    WN_element_size(addr_new) = WN_element_size(addr);
    
    // Set the kids -- base, indexes and dimensions
    
    WN_array_base(addr_new) = WN_array_base(addr_sub);
    LWN_Set_Parent(WN_array_base(addr_new),addr_new);
    
    INT index_new = 0;
    for (INT index_sub = 0; index_sub < num_dim_sub; index_sub++) {
      WN_array_index(addr_new,index_new) = WN_array_index(addr_sub,index_sub);
      LWN_Set_Parent(WN_array_index(addr_new,index_new),addr_new);
      index_new++;
    }
    for (INT index = 0; index < num_dim; index++) {
      WN_array_index(addr_new,index_new) = WN_array_index(addr,index);
      LWN_Set_Parent(WN_array_index(addr_new,index_new),addr_new);
      index_new++;
    }
    
    INT dim_new = 0;
    for (INT dim_sub = 0; dim_sub < num_dim_sub; dim_sub++) {
      WN_array_dim(addr_new,dim_new) = WN_array_dim(addr_sub,dim_sub);
      LWN_Set_Parent(WN_array_index(addr_new,dim_new),addr_new);
      dim_new++;
    }
    for (INT dim = 0; dim < num_dim; dim++) {
      WN_array_dim(addr_new,dim_new) = WN_array_dim(addr,dim);
      LWN_Set_Parent(WN_array_index(addr_new,dim_new),addr_new);
      dim_new++;
    }

    // cleanup
    WN_Delete(addr);
    WN_Delete(addr_sub);

    addr = addr_new;
  }
  return;
}

// fold the offset into the array
static void Fold_Offset(WN *wn, WN *array) 
{
  if (WN_offset(wn) && ((abs(WN_offset(wn)) % WN_element_size(array)) == 0)) {
    TYPE_ID rtype;
// >> WHIRL 0.30: Added MTYPE_A4
// TODO WHIRL 0.30: get rid of MTYPE_U4
    if ((Pointer_type == MTYPE_A4 || Pointer_type == MTYPE_U4) &&
// << WHIRL 0.30: Added MTYPE_A4
        (WN_offset(wn) <= INT32_MAX) &&
	(WN_offset(wn) >= INT32_MIN)) {
      rtype = MTYPE_I4;
    } else {
      rtype = MTYPE_I8;
    }
    OPCODE add_op = OPCODE_make_op(OPR_ADD,rtype, MTYPE_V);
    WN_array_index(array,0) = LWN_CreateExp2(add_op,WN_array_index(array,0),
		LWN_Make_Icon(rtype,WN_offset(wn)/WN_element_size(array)));
    LWN_Set_Parent(WN_array_index(array,0),array);
    WN_offset(wn) = 0;
  }
}

// Given ld_st (+ ... some_int)
// fold the int into the WN_offset of the ld/st
static void Fold_Intconst(WN *ld_st, WN *intconst, BOOL negate)
{
  INT64 result;
  if (negate) {
    result = WN_offset(ld_st) - WN_const_val(intconst);
  } else {
    result = WN_offset(ld_st) + WN_const_val(intconst);
  }
  if ((result <= INT32_MAX) && (result >= INT32_MIN)) {
    WN_offset(ld_st) = result;
    WN_const_val(intconst) = 0;
    LWN_Simplify_Tree(ld_st);
  }
}

// given that an array base is an add of type add
// given that the index is of type index
// can we move an argument of the add into the index
static BOOL Compatible_Type(MTYPE add, MTYPE index)
{
  if (add == index) return TRUE;
  if (add == MTYPE_U4 && index == MTYPE_I4) return TRUE;
  if (add == MTYPE_U8 && index == MTYPE_I8) return TRUE;
  return FALSE;
}

// Given an array base that is x+-y, try to fold
// one of the terms into the array
// only works for 1-d arrays (otherwise might
// break separability rule)
static void Fold_Base(WN *array)
{
  if (WN_kid_count(array) != 3) return;
  WN *base = WN_array_base(array);
  OPERATOR oper = WN_operator(base);
  if (oper != OPR_ADD && (oper != OPR_SUB)) {
    return;
  }
  WN *kid0 = WN_kid0(base);
  OPCODE kid0o = WN_opcode(kid0);
  WN *kid1 = WN_kid1(base);
  OPCODE kid1o = WN_opcode(kid1);

  WN *index = WN_array_index(array,0);
  TYPE_ID index_type = WN_rtype(index);
  TYPE_ID add_type = WN_rtype(base);
  if (!Compatible_Type(add_type,index_type)) return;

  // turn off simplifier temporarily
  BOOL simp_state_save = WN_Simplifier_Enable(FALSE);

  // First check for intconstants
  if (OPCODE_operator(kid0o) == OPR_INTCONST && (oper == OPR_ADD)) {
    if (abs(WN_const_val(kid0)) < INT32_MAX) {
      if (abs(WN_const_val(kid0)) % WN_element_size(array) == 0) {
        if (OPCODE_rtype(kid0o) == add_type) {
	  OPCODE add_op = OPCODE_make_op(OPR_ADD,add_type,MTYPE_V);
	  WN_array_index(array,0) = LWN_CreateExp2(add_op,WN_array_index(array,0),
		LWN_Make_Icon(add_type,WN_const_val(kid0)/WN_element_size(array)));
          LWN_Set_Parent(WN_array_index(array,0), array);
          WN_Delete(kid0);
          WN_Delete(base);
          WN_array_base(array) = kid1;
          LWN_Set_Parent(kid1,array);

          // change the array dimension to be unknown (0)
          WN_array_dim(array, 0) = 
            LWN_Make_Icon(WN_rtype(WN_array_dim(array, 0)), 0);
          LWN_Set_Parent(WN_array_dim(array, 0), array);
                                                         
          Fold_Base(array);
          WN_Simplifier_Enable(simp_state_save);
          return;
        }
      }
    }
  } else if (OPCODE_operator(kid1o) == OPR_INTCONST) {
    if (abs(WN_const_val(kid1)) < INT32_MAX) {
      if (abs(WN_const_val(kid1)) % WN_element_size(array) == 0) {
        if (OPCODE_rtype(kid1o) == add_type) {
	  OPCODE add_op = OPCODE_make_op(oper,add_type,MTYPE_V);
	  WN_array_index(array,0) = LWN_CreateExp2(add_op,WN_array_index(array,0),
		  LWN_Make_Icon(add_type,WN_const_val(kid1)/WN_element_size(array)));
          LWN_Set_Parent(WN_array_index(array,0), array);
          WN_Delete(kid1);
          WN_Delete(base);
          WN_array_base(array) = kid0;
          LWN_Set_Parent(kid0,array);

          // change the array dimension to be unknown (0)
          WN_array_dim(array, 0) = 
            LWN_Make_Icon(WN_rtype(WN_array_dim(array, 0)), 0);
          LWN_Set_Parent(WN_array_dim(array, 0), array);
                                                         
          Fold_Base(array);
          WN_Simplifier_Enable(simp_state_save);
          return;
        }
      }
    }

  // now check for multiplies
  } else if (OPCODE_operator(kid0o) == OPR_MPY && (oper == OPR_ADD)) { 
    WN *mult_kid0 = WN_kid0(kid0);
    WN *mult_kid1 = WN_kid1(kid0);
    WN *mult_kid = NULL;
    if (WN_operator(mult_kid0) == OPR_INTCONST) {
      mult_kid = mult_kid0;
    } else if (WN_operator(mult_kid1) == OPR_INTCONST) {
      mult_kid = mult_kid1;
    } 
    if (mult_kid) {
      if (abs(WN_const_val(mult_kid)) < INT32_MAX) {
        if (abs(WN_const_val(mult_kid)) % WN_element_size(array) == 0) {
          if (WN_rtype(kid0) == add_type) {
	    OPCODE add_op = OPCODE_make_op(OPR_ADD,add_type,MTYPE_V);
	    WN_array_index(array,0) = LWN_CreateExp2(add_op,WN_array_index(array,0),kid0);
	    WN_const_val(mult_kid) = WN_const_val(mult_kid) / WN_element_size(array);
            LWN_Set_Parent(WN_array_index(array,0), array);
	    WN_Delete(base);
	    WN_array_base(array) = kid1;
	    LWN_Set_Parent(kid1,array);

            // change the array dimension to be unknown (0)
            WN_array_dim(array, 0) = 
              LWN_Make_Icon(WN_rtype(WN_array_dim(array, 0)), 0);
            LWN_Set_Parent(WN_array_dim(array, 0), array);
                                                         
	    Fold_Base(array);
            WN_Simplifier_Enable(simp_state_save);
	    return;
          }
        }
      }
    }
  } else if (OPCODE_operator(kid1o) == OPR_MPY) {
    WN *mult_kid0 = WN_kid0(kid1);
    WN *mult_kid1 = WN_kid1(kid1);
    WN *mult_kid = NULL;
    if (WN_operator(mult_kid0) == OPR_INTCONST) {
      mult_kid = mult_kid0;
    } else if (WN_operator(mult_kid1) == OPR_INTCONST) {
      mult_kid = mult_kid1;
    } 
    if (mult_kid) {
      if (abs(WN_const_val(mult_kid)) < INT32_MAX) {
        if (abs(WN_const_val(mult_kid)) % WN_element_size(array) == 0) {
          if (WN_rtype(kid1) == add_type) {
	    OPCODE add_op = OPCODE_make_op(oper,add_type,MTYPE_V);
	    WN_array_index(array,0) = LWN_CreateExp2(add_op,WN_array_index(array,0),kid1);
	    WN_const_val(mult_kid) = WN_const_val(mult_kid) / WN_element_size(array);
            LWN_Set_Parent(WN_array_index(array,0), array);
	    WN_Delete(base);
	    WN_array_base(array) = kid0;
	    LWN_Set_Parent(kid0,array);

            // change the array dimension to be unknown (0)
            WN_array_dim(array, 0) = 
              LWN_Make_Icon(WN_rtype(WN_array_dim(array, 0)), 0);
            LWN_Set_Parent(WN_array_dim(array, 0), array);
                                                         
	    Fold_Base(array);
            WN_Simplifier_Enable(simp_state_save);
	    return;
          }
        }
      }
    }
  }
  WN_Simplifier_Enable(simp_state_save);
}

static void Enter_Label_Goto_Hash(WN *wn,
                                  HASH_TABLE<INT32, WN*> *label_hash,
                                  HASH_TABLE<INT32, WN*> *goto_hash)
{
  OPERATOR oper = WN_operator(wn);

  if (oper == OPR_BLOCK) {
    WN *kid = WN_first (wn);
    while (kid) {
      Enter_Label_Goto_Hash(kid,label_hash,goto_hash);
      kid = WN_next(kid);
    }
  } else if (oper == OPR_LABEL) {
    label_hash->Enter(WN_label_number(wn),wn);
  } else if (oper == OPR_GOTO ||
             oper == OPR_TRUEBR ||
             oper == OPR_FALSEBR ||
             oper == OPR_REGION_EXIT) {
    goto_hash->Enter(WN_label_number(wn),wn);
  } else {
    for (INT kidno=0; kidno<WN_kid_count(wn); kidno++) {
      Enter_Label_Goto_Hash(WN_kid(wn,kidno),label_hash,goto_hash);
    }
  }
}

static void Delete_Unused_Labels (HASH_TABLE<INT32, WN*> *label_hash,
                                  HASH_TABLE<INT32, WN*> *goto_hash) {

  HASH_TABLE_ITER<INT32, WN*> iter (label_hash);
  INT32 label;
  WN *wn;

  while (iter.Step (&label, &wn)) {
    if (!LABEL_addr_saved(label)) {
      WN* goto_wn = goto_hash->Find(label);
      if (goto_wn == NULL) {
        // no jumps to this label, so delete it
        LWN_Delete_Tree(wn);
      }
    }
  }
}

/***********************************************************************
 *
 * Given a WHIRL-alloca node, replace it by the intrinsic version.
 * Return the pointer to the generated intrinsic call, if any.
 *
 ***********************************************************************/
static WN *Convert_Alloca_To_Intrinsic   (WN *wn) {
  WN *stmt_wn = LWN_Get_Parent(wn);

  while (1) {
    if (WN_opcode(stmt_wn) == OPC_IO) {
      // don't go underneath IO, since ALLOCA under IO can be in a
      // pretty messy form, not a simple tmp = alloca() form.
      // hopefully LNO will not analyze IO, and black-box it.
      return NULL;
    }

    if (OPERATOR_is_stmt(WN_operator(stmt_wn)) &&
        (WN_opcode(LWN_Get_Parent(stmt_wn)) == OPC_BLOCK)) {
      break;
    }
    stmt_wn = LWN_Get_Parent(stmt_wn);
  }

  BOOL alloca_zero = FALSE;
  WN *sz = WN_kid0(wn);
  if (WN_operator(sz) == OPR_INTCONST && WN_const_val(sz) == 0) {
    alloca_zero = TRUE;
  }
  
  WN *iwn;
  OPCODE op = OPCODE_make_op(OPR_INTRINSIC_CALL, Pointer_type, MTYPE_V);
  if (alloca_zero) {
    // generate read-stack-pointer
    iwn = WN_Create(op, 0);
    WN_intrinsic(iwn) =  (Pointer_Size == 8 ?
                          INTRN_U8READSTACKPOINTER :
                          INTRN_U4READSTACKPOINTER);
  }
  else {
    // generate alloca
    iwn = WN_Create(op, 1);
    WN_intrinsic(iwn) = (Pointer_Size == 8 ? INTRN_U8I8ALLOCA :
                         INTRN_U4I4ALLOCA); 
    WN* parm_wn = WN_CreateParm (MTYPE_U8, sz, Be_Type_Tbl(MTYPE_U8),
                                 WN_PARM_BY_VALUE);
    LWN_Set_Parent(sz, parm_wn);
    WN_kid0(wn) = NULL;     // sz no longer kid of alloca
    WN_kid0(iwn) = parm_wn;
    LWN_Set_Parent(parm_wn, iwn);
  }
  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_set_in_out_same(iwn);
    
  LWN_Insert_Block_Before(LWN_Get_Parent(stmt_wn), stmt_wn, iwn);
  LWN_Copy_Linenumber(stmt_wn, iwn);

  PREG_NUM rreg1, rreg2;
  ST* rst = Find_Return_Registers(Pointer_type, &rreg1, &rreg2);
  WN *rreg = WN_CreateLdid (OPCODE_make_op(OPR_LDID, Pointer_type,
                                           Pointer_type),
                            rreg1, rst,
                            OPCODE_rtype(WN_opcode(wn)));
  Create_alias(Alias_Mgr, rreg);
  Du_Mgr->Add_Def_Use (iwn, rreg);

  WN *pwn = LWN_Get_Parent(wn);
  INT i;
  for (i=0; i<WN_kid_count(pwn); i++) {
    if (WN_kid(pwn,i) == wn) break;
  }

  WN_kid(pwn,i) = rreg;
  LWN_Set_Parent(rreg, pwn);
  LWN_Delete_Tree(wn);      // sz is kid if alloca(0), not otherwise
  return iwn;
}

/***********************************************************************
 *
 * Given a WHIRL dealloca node, replace it by an intrinsic version.
 * Return the pointer to the generated intrinsic call.
 *
 ***********************************************************************/
static WN *Convert_Dealloca_To_Intrinsic (WN *wn) {

  OPCODE icallop = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V,
                                  MTYPE_V);
  WN *iwn = WN_Create(icallop, WN_kid_count(wn));
  WN_intrinsic(iwn) = (Pointer_Size == 8 ?
                       INTRN_U8I8SETSTACKPOINTER :
                       INTRN_U4I4SETSTACKPOINTER);
  for (INT i=0; i<WN_kid_count(wn); i++) {
    WN *parm_wn = WN_CreateParm(Pointer_type, WN_kid(wn,i),
                                Be_Type_Tbl(Pointer_type),
                                WN_PARM_BY_VALUE);
    LWN_Set_Parent(WN_kid(wn,i), parm_wn);
    WN_kid(iwn, i) = parm_wn;
    LWN_Set_Parent(parm_wn, iwn);
    WN_kid(wn,i) = NULL;
  }
  LWN_Insert_Block_Before (LWN_Get_Parent(wn), wn, iwn);
  LWN_Copy_Linenumber(wn, iwn);
  LWN_Copy_Frequency_Tree(wn, iwn);

  LWN_Delete_Tree(wn);
  return iwn;
}

/***********************************************************************
 *
 * Given a WHIRL statement, it must have a kid that is the ldid of the
 * return-reg (preg 2). Find and return that LDID.
 *
 ***********************************************************************/
static WN *Find_RReg_Ldid (WN *wn, INT *idx) {
  if (wn == NULL) return wn;
  
  if (WN_operator(wn) == OPR_LDID &&
      ST_class(WN_st(wn)) == CLASS_PREG &&
      WN_offset(wn) == Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Integer())) {
    *idx = -1;
    return wn;
  }

  for (INT i=0; i<WN_kid_count(wn); i++) {
    WN *tmp = Find_RReg_Ldid(WN_kid(wn,i), idx);
    if (tmp) {
      if (*idx == -1) *idx = i;
      return tmp;
    }
  }
  return NULL;
}

/***********************************************************************
 *
 * Given a WHIRL tree, replace each of 
 *  readstackpointer
 *  alloca
 *  setstackpointer
 * intrinsics with corresponding alloca() and dealloca() calls.
 *
 ***********************************************************************/
extern WN *Convert_Intrinsic_To_Alloca_Dealloca (WN *wn) {

  if (wn == NULL) return wn;

  if (WN_opcode(wn) == OPC_BLOCK) {
    WN *kid = WN_first(wn);
    while (kid) {
      kid = Convert_Intrinsic_To_Alloca_Dealloca(kid);
      kid = WN_next(kid);
    }
    return wn;
  }

  if (WN_opcode(wn) == OPC_IO) {
    // don't go underneath IO, since stuff under IO can be in a
    // pretty messy form.
    return wn;
  }

  if (WN_operator(wn) != OPR_INTRINSIC_CALL) {
    for (INT i=0; i<WN_kid_count(wn); i++) {
      Convert_Intrinsic_To_Alloca_Dealloca(WN_kid(wn,i));
    }
    return wn;
  }

  WN *ret_wn = wn;

  switch (WN_intrinsic(wn)) {

  case INTRN_U8READSTACKPOINTER:
  case INTRN_U4READSTACKPOINTER:
    // convert to alloca(0)
    wn = WN_next(wn);
    ret_wn = wn;
    LWN_Delete_Tree(WN_prev(wn));

    INT idx;
    wn = Find_RReg_Ldid (wn, &idx);
    FmtAssert (wn, ("Could not find return-value from intrinsic"));

    WN_kid (LWN_Get_Parent(wn),idx) =
      WN_CreateAlloca(WN_CreateIntconst(OPC_I4INTCONST,0));
    LWN_Parentize(LWN_Get_Parent(wn));
    LWN_Set_Parent(wn,NULL);
    LWN_Delete_Tree(wn);

    break;

  case INTRN_U8I8ALLOCA:
  case INTRN_U4I4ALLOCA: {
      // convert to alloca(sz)
      WN *sz = WN_kid0(WN_kid0(wn));
      LWN_Set_Parent(WN_kid0(WN_kid0(wn)), NULL);
      WN_kid0(WN_kid0(wn)) = NULL;
      wn = WN_next(wn);
      ret_wn = wn;
      LWN_Delete_Tree(WN_prev(wn));

      wn = Find_RReg_Ldid (wn, &idx);
      FmtAssert (wn, ("Could not find return-value from intrinsic"));

      WN_kid (LWN_Get_Parent(wn),idx) = WN_CreateAlloca(sz);
      LWN_Parentize(LWN_Get_Parent(wn));
      LWN_Set_Parent(wn,NULL);
      LWN_Delete_Tree(wn);
    } 
    break;

  case INTRN_U8I8SETSTACKPOINTER:
  case INTRN_U4I4SETSTACKPOINTER:
    // convert to dealloca(p, ...)
    WN *dwn = WN_CreateDealloca(WN_kid_count(wn));
    for (INT i=0; i<WN_kid_count(wn); i++) {
      WN_kid(dwn,i) = WN_kid0(WN_kid(wn,i));
      WN_kid0(WN_kid(wn,i)) = NULL;
    }
    LWN_Parentize(dwn);
    LWN_Insert_Block_Before (LWN_Get_Parent(wn), wn, dwn);
    LWN_Delete_Tree(wn);
    ret_wn = dwn;
    break;
  }
  return ret_wn;
}

//
// try to fold a (tie) convert into a kid load or a parent store
//
static WN *
Remove_Redundant_Convert(WN *wn) {
  Is_True(WN_operator(wn)==OPR_CVT,("Expected OPR_CVT."));
  
  if (tie_info==NULL) {
    return NULL;
  }
  
  TYPE_ID to_type = WN_rtype(wn);
  TYPE_ID from_type = WN_desc(wn);
  
  if (!MTYPE_is_tie(to_type) && !MTYPE_is_tie(from_type)) {
    return NULL;
  }
  
  WN *kid = WN_kid0(wn);
  if (WN_rtype(kid)!=from_type) { // not possible?
    return NULL;
  }
  
  WN *parent = LWN_Get_Parent(wn);
  bool fold_into_load = false;
  if (WN_operator(kid)==OPR_ILOAD && from_type==WN_desc(kid) &&
      tie_info->mtype_mtor_macro(from_type,to_type)!=NULL) {
    fold_into_load = true;
    // remove is ok
#if 0
    // apparently, it's not safe to remove the convert for a store
  } else if (WN_operator(parent)==OPR_ISTORE && WN_desc(parent)==to_type &&
	     tie_info->mtype_rtom_macro(from_type,to_type)!=NULL) {
    // remove is ok
#endif
  } else {
    return NULL;
  }
  
  for (INT kid_idx=0; kid_idx<WN_kid_count(parent); kid_idx++) {
    if (WN_kid(parent,kid_idx)==wn) {
      WN_kid(parent,kid_idx)=kid;
      LWN_Set_Parent(kid,parent);
      if (fold_into_load) {
	WN_set_rtype(kid,to_type);
      }
      WN_Delete(wn);
      return kid;
    }
  }
  
  return NULL;
}


