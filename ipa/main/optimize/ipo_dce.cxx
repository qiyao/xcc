
/* 
   Copyright (C) 2004 Tensilica, Inc.  All Rights Reserved.
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


//* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#ifdef _WIN32
#include <windows.h>
#endif

#include "defs.h"
#include "symtab.h"			// symbol table
#include "targ_sim.h"			// Get_Return_Mtypes ()
#include "lwn_util.h"			// WN_EXTRACE_FromBlock ()

#include "ipa_option.h"			// Trace_IPA
#include "ipa_cg.h"			// call graphs
#include "ipo_defs.h"			// IPA_NODE_CONTEXT
#include "ipo_parent.h"			// WN_Get_Parent()
#include "ipo_dce.h"

//--------------------------------------------------------------
// figure out the stid preg's that we need to be looking for
//--------------------------------------------------------------
static UINT32 
Compute_Return_Pregs(TY_IDX callee_ty)
{
    PREG_NUM Reg1, Reg2;
    TYPE_ID ty1, ty2;
    TY_IDX typ;

    // get the return type from the callee's opcode
    if (typ = Tylist_Table[TY_tylist(Ty_Table[callee_ty])])
	if (TY_kind(typ) != KIND_VOID) {

	    if (WHIRL_Return_Info_On) {

		RETURN_INFO return_info = Get_Return_Info (typ, Use_Simulated,
							   Return_Info_Incoming);
		return RETURN_INFO_count(return_info);
	    }
#ifdef __OBSOLETE__
	    else {
		Get_Return_Mtypes(typ, Use_Simulated, &ty1, &ty2);
		Get_Return_Pregs(ty1, ty2, &Reg1, &Reg2);
	    }
	    
	    if (Reg1 && Reg2)
		return 2;
	    else if (Reg1 || Reg2)
		return 1;
#endif
	}

    return 0;
}

//--------------------------------------------------------------
// delete the call whirl node
//--------------------------------------------------------------
BOOL
Delete_Call (IPA_NODE *caller, const IPA_NODE *callee, IPA_EDGE *edge,
	     IPA_CALL_GRAPH* cg)
{
    IPA_NODE_CONTEXT context (caller);
    cg->Map_Callsites (caller);           // map callsites to WN nodes
    WN* call = edge->Whirl_Node();
    WN* b = WN_Get_Parent(call, Parent_Map, Current_Map_Tab);

    if ((WN_operator(b) != OPR_BLOCK))
	return FALSE;

    UINT32 Reg_count = (WN_operator (call) == OPR_ICALL) ?
	Compute_Return_Pregs (WN_ty (call)) :
	Compute_Return_Pregs (ST_pu_type (WN_st (call)));

    if (Reg_count) {
	for (INT i = 0; i < Reg_count; ++i) {
	    WN* stid = WN_next(call);
	    if (stid && (WN_operator(stid) == OPR_STID)) {
		if ((WN_operator(WN_kid0(stid)) == OPR_LDID) && 
		    (ST_sclass(WN_st(WN_kid0(stid))) == SCLASS_REG) &&
		    Preg_Is_Dedicated(WN_offset(WN_kid0(stid)))) {
		    WN_EXTRACT_FromBlock(b, stid);
		}
	    }
	}
    }
    WN_EXTRACT_FromBlock(b, call);
    if (Trace_IPA || Trace_Perf) {
	fprintf (TFile, "%s called from ", DEMANGLE(ST_name(callee->Func_ST())));
	fprintf (TFile, "%s deleted\n", DEMANGLE(ST_name(caller->Func_ST())));
    }
    return TRUE;
} // Delete_Call
