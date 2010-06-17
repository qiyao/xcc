
/* 
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: ipa_inline.cxx
// $Source: /isms/cmplrs.src/osprey1.0/ipa/main/analyze/RCS/ipa_inline.cxx,v $
//
// Revision history:
//  16-Nov-96 - Original Version
//
// Description:
//
// Inlining analysis.
//
// ====================================================================
// ====================================================================

#include <math.h>
#ifdef _WIN32
#include <windows.h>
#endif

#include "defs.h"
#include "tracing.h"			// trace flags
#include "errors.h"			// error handling

#include "ipa_option.h"			// ipa options

#include "ipa_inline.h"
#include "ipa_nested_pu.h"
#include "ipa_summary.h"
#include "ipc_symtab_merge.h"		// IPC_GLOBAL_IDX_MAP

INT Total_Prog_Size = 0;	// Size of the final program
INT Total_Inlined = 0;
INT Total_Not_Inlined = 0;
static UINT32 Max_Total_Prog_Size; // max. program size allowed
static INT Real_Orig_Prog_Weight; // Orig_Prog_Weight - dead code
static BOOL Trace_Inline = FALSE;

#define BASETYPE TY_mtype

static OPCODE OPC_UNKNOWN = (OPCODE)0;

/* the order of the element in Stid_Opcode need to correspond to the mtype
   orders in Mtype_initialize() of mtypes.cxx and there are 128 mtypes
   allowed
*/
static
OPCODE Stid_Opcode [MTYPE_MAX] = {
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_UNKNOWN,    /* MTYPE_UNKNOWN */
  OPC_I1STID,     /* MTYPE_I1 */
  OPC_I2STID,     /* MTYPE_I2 */
  OPC_I4STID,     /* MTYPE_I4 */
  OPC_I8STID,     /* MTYPE_I8 */
  OPC_U1STID,     /* MTYPE_U1 */
  OPC_U2STID,     /* MTYPE_U2 */
  OPC_U4STID,     /* MTYPE_U4 */
  OPC_U8STID,     /* MTYPE_U8 */
  OPC_F4STID,     /* MTYPE_F4 */
  OPC_F8STID,     /* MTYPE_F8 */
  OPC_UNKNOWN,    /* MTYPE_F10 */
  OPC_UNKNOWN,    /* MTYPE_F16 */
  OPC_UNKNOWN,    /* MTYPE_STR */
  OPC_FQSTID,     /* MTYPE_FQ */
  OPC_UNKNOWN,    /* MTYPE_M */
  OPC_C4STID,     /* MTYPE_C4 */
  OPC_C8STID,     /* MTYPE_C8 */
  OPC_CQSTID,     /* MTYPE_CQ */
  OPC_UNKNOWN,     /* MTYPE_V */

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,
  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN,

  OPC_UNKNOWN, OPC_UNKNOWN, OPC_UNKNOWN
};


// ====================================================================
//
// Report_Reason / Report_Limit_Reason
//
// We're not inlining a call.  Report why.  For Report_Reason, the
// parameter reason is a simple string.  For Report_Limit_Reason, it
// is a printf format string for printing one (limit1) or two (limit1,
// limit2) integer limits.
//
// WARNING:  The DEMANGLE routine always returns the same static
// buffer, so the caller and callee names must not be live
// simultaneously.
//
// ====================================================================

void
Report_Reason (const IPA_NODE *callee, const IPA_NODE *caller,
	       const char *reason , const IPA_EDGE *edge)
{
  if (!INLINE_List_Actions && !Trace_IPA && !callee->Has_Must_Inline_Attrib()) 
    return;

  FILE *fp = Trace_IPA ? TFile : stderr;
  char *callee_name = DEMANGLE(callee->Name());
  char *caller_name = DEMANGLE(caller->Name());

  if (INLINE_List_Actions || Trace_IPA) {
    fprintf(fp, "%s not inlined into %s (edge# %d): %s\n", 
          callee_name, caller_name, edge->Edge_Index(), reason);
  } else {
    fprintf(fp, "Warning: requested function %s not inlined into %s (edge# %d): %s\n", 
          callee_name, caller_name, edge->Edge_Index(), reason);
  }
  fflush(fp);
}

void
Report_Limit_Reason (const IPA_NODE *callee, const IPA_NODE *caller,
		     const IPA_EDGE *edge,
		     const char *reason, float limit1, float limit2)
{
  if (!INLINE_List_Actions && !Trace_IPA) 
    return;

  FILE *fp = Trace_IPA ? TFile : stderr;
  char *callee_name = DEMANGLE(callee->Name());
  char *caller_name = DEMANGLE(caller->Name());

  fprintf(fp, "%s not inlined into %s (edge# %d): ", 
          callee_name, caller_name, edge->Edge_Index());
  fprintf(fp, reason, limit1, limit2);
  fprintf(fp, "\n");
  fflush(fp);
}

/* the combined weight after inlining two procedures */
/* we need to subtract the bb count and call count by 1 to reflect the
   removal of a call.  We add the number of copy stmt created for copying
   the formal parameters */
static UINT32
Get_combined_weight (PU_SIZE s1, PU_SIZE s2, IPA_NODE *callee)
{
    s1 += s2;
    /* 1 less bb and 1 less callfrom removing the call, add copy stmt for
       formals */ 
    s1.Inc_PU_Size (-1, callee->Num_Formals(), -1);
    return s1.Weight ();
}

static UINT32
Get_combined_olimit (PU_SIZE s1, PU_SIZE s2, IPA_NODE *callee)
{
    s1 += s2;
    /* 1 less bb and 1 less callfrom removing the call, add copy stmt for
       formals */ 
    s1.Inc_PU_Size (-1, callee->Num_Formals(), -1);
    return (s1.Olimit() * 115) / 100;
}



#ifndef _STANDALONE_INLINER

typedef AUX_IPA_NODE<UINT32> INLINE_COUNTER_ARRAY;

// For each PU, keep track of the number of calls (to this PU) that are
// inlined.  If all calls to this PU are inlined, the PU might be deleted.
static INLINE_COUNTER_ARRAY* inline_count;

static inline BOOL
All_Calls_Inlined (const IPA_NODE* node, const IPA_CALL_GRAPH* cg)
{
    Is_True (IPA_Enable_DFE, ("Expecting -IPA:dfe=on"));

    return cg->Num_In_Edges (node) == (*inline_count)[node];
}

#endif // _STANDALONE_INLINER

static void
Init_inline_parameters (void)
{
    UINT64 current_size = Real_Orig_Prog_Weight =
	MIN (Total_Prog_Size, Orig_Prog_Weight);

#ifndef _STANDALONE_INLINER
    if (!INLINE_Aggressive_Set && !OPT_Space)
      INLINE_Aggressive = TRUE;
#endif

    if (OPT_Space && !IPA_Bloat_Factor_Set && !IPA_Force_Depth_Set)
      IPA_Bloat_Factor = 75;

    if (OPT_Space && !IPA_PU_Minimum_Size_Set)
      IPA_PU_Minimum_Size >>= 1;
          
    UINT64 bloat_size = current_size * (UINT64) IPA_Bloat_Factor;

    if (bloat_size > UINT32_MAX || IPA_Bloat_Factor == UINT32_MAX)
	Max_Total_Prog_Size = UINT32_MAX; // possible mult overflow
    else
	Max_Total_Prog_Size = current_size + bloat_size / 100;

    if (Total_cycle_count.Known() && Trace_IPA ) {
	fprintf (TFile, "\tTotal number of calls = ");
        Total_call_freq.Print(TFile);
	fprintf (TFile, "\n\tTotal cycle count = ");
        Total_cycle_count.Print(TFile);
	fprintf (TFile, "\n\tTotal ccount = ");
        Total_ccount.Print(TFile);
	fprintf (TFile, "\n");
    }

    if (Trace_IPA) {
	fprintf(TFile, "Bloat factor = %u%% \n",  IPA_Bloat_Factor);
	fprintf(TFile, "PU Limit = %u \n", IPA_PU_Limit);
	fprintf(TFile, "Depth Level = %u \n", IPA_Max_Depth);
	if (IPA_Bloat_Factor_Set)
	    fprintf(TFile, "Bloat Factor set = TRUE \n");
	else
	    fprintf(TFile, "Bloat Factor set = FALSE \n");

	if (IPA_PU_Limit_Set)
	    fprintf(TFile, "PU Limit Set = TRUE \n");
	else
	    fprintf(TFile, "PU Limit Set = FALSE \n");
    }

} // Init_inline_parameters


#ifndef _STANDALONE_INLINER
static void
Update_Total_Prog_Size (const IPA_NODE *caller, IPA_NODE *callee,
			const IPA_CALL_GRAPH *cg)
{
    ++((*inline_count)[callee]);

    if (IPA_Enable_Cloning && caller->Is_Clone_Candidate()) {
	callee->Set_Undeletable ();
	return;
    }
	
    if (! callee->Is_Undeletable () &&
	All_Calls_Inlined (callee, cg) &&
	! callee->Is_Externally_Callable ()) {

	callee->Set_Deletable ();
	Total_Prog_Size -= callee->Weight();
    }
} // Update_Total_Prog_Size
#endif // _STANDALONE_INLINER


inline static float
compute_hotness (IPA_EDGE *edge, IPA_NODE *callee, INT callee_size)
{
  double cycle_ratio =	
    ((OPT_Estimate_Ccount || Total_ccount.Zero()) ?
     ((edge->Get_frequency().Value() / callee->Get_frequency().Value() *
       callee->Get_cycle_count().Value()) / Total_cycle_count.Value())
     :
     ((edge->Get_frequency().Value() / callee->Get_frequency().Value() *
       callee->Get_ccount().Value()) / Total_ccount.Value()));

  double size_ratio = (double) callee_size / (double) Real_Orig_Prog_Weight;
  double result = (cycle_ratio / size_ratio * 100.0);
  return (float) result;
}

static UINT32
Effective_weight (const IPA_NODE* node)  {
#ifndef _STANDALONE_INLINER
#ifdef __FIXME__
  if (IPA_Use_Effective_Size && node->Has_frequency()) {
    SUMMARY_FEEDBACK *fb = node->Get_feedback();
    return PU_Weight(fb->Get_effective_bb_count(),
                     fb->Get_effective_stmt_count(),
                     node->PU_Size().Call_Count());
  } else
#endif
#endif
    return node->Weight ();
}

static BOOL
check_size_and_freq(IPA_EDGE *ed, 
                    IPA_NODE *caller,
                    IPA_NODE *callee, 
                    const IPA_CALL_GRAPH *cg)
{
  FILE *fp = Trace_IPA ? TFile : stderr;
  char *callee_name = DEMANGLE(callee->Name());
  char *caller_name = DEMANGLE(caller->Name());

  UINT32 remaining_calls = cg->Num_In_Edges(callee);
#ifndef _STANDALONE_INLINER
  remaining_calls -= (*inline_count)[callee];
#endif

  double callsite_bias = cg->Num_In_Edges(callee);
  callsite_bias /= (callsite_bias + 1);
  callsite_bias = pow(callsite_bias, 1.0/IPA_Callsite_Bias);

  double level_bias = cg->Node_Depth(callee) + 1;
  level_bias /= (level_bias + 1);
  level_bias = pow(level_bias, 1.0/IPA_Level_Bias);

  UINT32 caller_weight = caller->Weight();
  UINT32 callee_weight = Effective_weight(callee);
  double biased_callee_weight = callee_weight * callsite_bias * level_bias;
  
  UINT32 combined_weight =
    Get_combined_weight(caller->PU_Size(), callee->PU_Size(), callee);
  UINT32 combined_olimit = 
    Get_combined_olimit(caller->PU_Size(), callee->PU_Size(), callee);
  INT loopnest = ed->Summary_Callsite()->Get_loopnest();

  // With -INLINE:requested we inline all the functions
  // explicitly marked as 'inline'
  if (INLINE_Requested)
    if (callee->Has_Inline_Attrib())
      goto return_true;

  if (INLINE_Requested_Only)
    if (callee->Has_Inline_Attrib())
      goto return_true;
    else return FALSE;

  // With random inlining edges are explicitly marked
  //if (INLINE_Random_Seed_Set && !ed->Has_Must_Inline_Attrib())
    //return FALSE;

  // Inline functions below or at given depth
  if (IPA_Force_Depth_Set &&
      cg->Node_Depth(callee) <= IPA_Force_Depth &&
      !callee->Has_Noinline_Attrib()) 
    goto return_true;

  // Inline all or specifically requested edges or functions
  if (INLINE_All ||
      ed->Has_Must_Inline_Attrib() ||
      callee->Has_Must_Inline_Attrib())
    goto return_true;

#ifndef _STANDALONE_INLINER
  if (ed->Has_frequency() &&
      ed->Get_frequency().Known() && 
      ed->Get_frequency().Value() <= 0.0) {
    Report_Reason(callee, caller, "callee never called", ed);
    return FALSE;
  }
#endif

  // Try to recognize and not inline print, error, assert, abort, exit, trace 
  if (INLINE_Skip_Error_Funcs &&
      (strstr(callee_name, "print")  ||
       strstr(callee_name, "Print")  ||
       strstr(callee_name, "flush")  ||
       strstr(callee_name, "Flush")  ||
       strstr(callee_name, "err")    ||
       strstr(callee_name, "Err")    ||
       strstr(callee_name, "assert") ||
       strstr(callee_name, "Assert") ||
       strstr(callee_name, "abort")  ||
       strstr(callee_name, "Abort")  ||
       strstr(callee_name, "exit")   ||
       strstr(callee_name, "Exit")   ||
       strstr(callee_name, "trace")  ||
       strstr(callee_name, "Trace"))) {
    Report_Reason(callee, caller, "callee should execute infrequently", ed);
    return FALSE;
  }
  
  if (combined_olimit > Olimit) {
    Report_Limit_Reason(callee, caller, ed, 
                        "combined Olimit (%.0f) exceeds -OPT:Olimit=%.0f",
                        combined_olimit, Olimit);
    return FALSE;
  }

  // Always inline very small callees
  if (callee_weight <= IPA_PU_Minimum_Size ||
      (callee_weight <= (IPA_PU_Minimum_Size * 3) / 2 &&
       remaining_calls < IPA_Few_Calls))
    goto return_true;
  
  // With -Os only inline callees that are called only once
  if (OPT_Space && remaining_calls > 1) {
    Report_Reason(callee, caller, "inlining would increase code size", ed);
    return FALSE;
  }
  
  if (Total_Prog_Size >= Max_Total_Prog_Size) {
    if (Trace_IPA || INLINE_List_Actions) {
      static BOOL reported = FALSE;
      if (!reported) {
        fprintf(fp, "Inlining stopped: total program size limit exceeded\n");
        reported = TRUE;
      }
    }
    return FALSE;
  }
	
  if (Trace_IPA)
    fprintf(fp, "caller: %s (%u) loopnest = %u, callee: %s (%u) %s\n"
            " biased weight: %u * %.3f * %.3f = %.3f\n",
            caller_name, caller_weight, loopnest, callee_name, callee_weight,
            callee->PU_Size().Call_Count() == 0 ? "(leaf)" : "",
            callee_weight, callsite_bias, level_bias, biased_callee_weight);

#ifndef _STANDALONE_INLINER
  if (ed->Has_frequency () && 
      callee->Has_frequency() &&
      ed->Get_frequency().Known() && 
      callee->Get_frequency().Known()) {
    float hotness = compute_hotness(ed, callee, callee_weight); 
    float min_hotness = (float) IPA_Min_Hotness;
    if (hotness < min_hotness) {
      Report_Limit_Reason(callee, caller, ed,
                          "hotness (%f) < -IPA:min_hotness (%f)",
                          hotness, min_hotness);
      return FALSE;
    }
  } 
#endif

  if (combined_weight > IPA_PU_Limit && remaining_calls > 1) {
    Report_Limit_Reason(callee, caller, ed, 
                        "combined size (%.0f) exceeds "
                        "function size limit (%.0f)",
                        combined_weight, IPA_PU_Limit);
    return FALSE;
  }

  // Always inline into caller that looks like:
  // foo() { one(); two(); three(); four(); }
  if (loopnest == 0 && 
      cg->Num_Out_Edges(caller) < IPA_Few_Calls &&
      caller->Orig_Weight() <= IPA_Small_Caller_Size)
    goto return_true;

  // Don't inline callees with loops unless
  // 1) they are very small, or
  // 2) the caller is very small the callsite is not in a loop
  if ((loopnest > 0 || 
       cg->Num_Out_Edges(caller) > IPA_Few_Calls || 
       (cg->Num_Out_Edges(caller) == IPA_Few_Calls && 
	caller->Orig_Weight() > IPA_Small_Caller_Size))
      && 
      ((callee->Max_Loop_Depth() >= 2 && callee_weight > IPA_Callee_Loop_2) ||
       (callee->Max_Loop_Depth() >= 1 && callee_weight > IPA_Callee_Loop_1))) {
    Report_Limit_Reason(callee, caller, ed,
                        "callee has enough work: "
                        "max_loop_dept = %.0f, size = %.0f",
                        callee->Max_Loop_Depth(),
                        callee_weight);
    return FALSE;
  }

  if (loopnest >= 3 ||
      (loopnest >= 2 && callee_weight > IPA_Callsite_Loop_2) ||
      (loopnest >= 1 && callee_weight > IPA_Callsite_Loop_1)) {
    Report_Limit_Reason(callee, caller, ed,
                        "call site is enclosed within %.0f loops",
                        loopnest, 0);
    return FALSE;
  }

  if (biased_callee_weight > IPA_Small_Callee_Limit &&
      remaining_calls > IPA_Min_Callsites) {
    Report_Limit_Reason(callee, caller, ed,
                        "callee size (%.0f) exceeds "
                        "small callee size limit (%.0f)",
                        biased_callee_weight,
                        IPA_Small_Callee_Limit); 
    return FALSE;
  }

  if (!INLINE_Aggressive &&
      remaining_calls > 1 &&
      loopnest == 0 &&
      callee->PU_Size().Call_Count() > 0 &&
      biased_callee_weight > IPA_Nonaggr_Callee_Limit) {
    // Less aggressive inlining: don't inline unless it is
    // either small, leaf, or called from a loop.
    Report_Limit_Reason(callee, caller, ed, 
                        "callee size (%.0f) exceeds "
                        "non-aggressive callee size limit (%.0f)",
                        callee_weight, IPA_Nonaggr_Callee_Limit);
    return FALSE;
  }

  if (!INLINE_Aggressive && 
      cg->Num_Out_Edges(caller) >= IPA_Max_Callsites) {
    Report_Limit_Reason(callee, caller, ed, 
                        "number of callsites in the caller %.0f > %.0f",
                        cg->Num_Out_Edges(caller), IPA_Max_Callsites);
    return FALSE;
  }

 return_true:
  
  if (Trace_IPA) {
    fprintf(fp, "%s inlined into %s (edge# %d) (size: %d + %d = %d)\n", 
            callee_name, caller_name, ed->Edge_Index(),
            callee_weight, caller_weight, combined_weight);
  }
    
  Total_Prog_Size += (combined_weight - caller_weight);
  caller->UpdateSize (callee, ed);

#ifndef _STANDALONE_INLINER
  if (IPA_Enable_DFE)
    Update_Total_Prog_Size (caller, callee, cg);
#endif

  // propagate the bit up
  if (callee->Summary_Proc()->Has_var_dim_array()) 
    caller->Summary_Proc()->Set_has_var_dim_array();

  return TRUE;

} // check_size_and_freq


struct find_st_attr_secname {
    ST_IDX st;
    find_st_attr_secname (const ST *s) : st (ST_st_idx (s)) {}
    BOOL operator () (UINT, const ST_ATTR *st_attr) const {
        return (ST_ATTR_kind (*st_attr) == ST_ATTR_SECTION_NAME &&
                    ST_ATTR_st_idx (*st_attr) == st);
    }
};


static STR_IDX
Find_Section_Name_For_ST (const ST *st)
{
    ST_IDX idx = ST_st_idx (st);
    ST_ATTR_IDX d;
    d = For_all_until (St_Attr_Table, ST_IDX_level (idx),
    find_st_attr_secname(st));
    return ST_ATTR_section_name(St_Attr_Table(ST_IDX_level (idx), d));
}


#define GNU_LINKONCE_PREFIX ".gnu.linkonce."


// Return true if caller and callee are in different named sections
// or if one but not the other is in a named section
// We don't want to inline across named sections because that would effectively move 
// the code into a different section
// We make an exception for GNU linkonce sections since there is no reason why we should
// not inline a template or member function into something else
static BOOL Different_Named_Nonlinkonce_Sections(IPA_NODE *caller_node, IPA_NODE *callee_node)
{
    ST* callee = callee_node->Func_ST();
    ST* caller = caller_node->Func_ST();
    STR_IDX caller_idx=0;
    STR_IDX callee_idx=0;

    if (ST_has_named_section(caller)) {
	caller_idx  = Find_Section_Name_For_ST(caller);
        if (caller_idx && !strncmp(Index_To_Str(caller_idx), 
				GNU_LINKONCE_PREFIX, strlen(GNU_LINKONCE_PREFIX))) {
	  caller_idx = 0;
        }
    }
    if (ST_has_named_section(callee)) {
	callee_idx  = Find_Section_Name_For_ST(callee);
        if (callee_idx && !strncmp(Index_To_Str(callee_idx), 
				GNU_LINKONCE_PREFIX, strlen(GNU_LINKONCE_PREFIX))) {
	  callee_idx = 0;
        }
    }
    return (caller_idx != callee_idx);
}

//--------------------------------------------------------------------------
// check if return types are okay
//--------------------------------------------------------------------------
static BOOL
return_types_are_compatible (IPA_NODE* callee_node, IPA_EDGE *ed)
{
    if (ed->Summary_Callsite()->Get_return_type() == MTYPE_V)
        return TRUE;                    // caller ignoring the return type

    ST* callee = callee_node->Func_ST();

    Is_True (ST_sym_class (callee) == CLASS_FUNC,
             ("Expecting a function ST"));

    TY_IDX ty_idx = ST_pu_type (callee);
    TY& ty = Ty_Table[ty_idx];

    Is_True (TY_kind (ty) == KIND_FUNCTION, ("Expecting a function ST")
);

    TY_IDX ret_ty_idx = TYLIST_type (Tylist_Table[TY_tylist (ty)]);

    if (ret_ty_idx == 0)
        return FALSE;

    TY& ret_ty = Ty_Table[ret_ty_idx];

    if (TY_kind (ret_ty) == KIND_VOID)
        return FALSE;
    else {
        TYPE_ID callee_mtype = BASETYPE(ret_ty);
        TYPE_ID caller_mtype = ed->Summary_Callsite()->Get_return_type();

        // check if types are the same or the return type is
        // a structure return

        if ((callee_mtype == caller_mtype) || (callee_mtype == MTYPE_M))
            return TRUE;
    }

    return FALSE;
} // return_types_are_compatible

static TY_IDX
base_type_of_array(TY_IDX array_type)
{
    if (TY_kind(TY_AR_etype(array_type)) != KIND_ARRAY)
	return (TY_AR_etype(array_type));
    else
	return (base_type_of_array(TY_AR_etype(array_type)));
}

//--------------------------------------------------------------------------
// check if the types are compatible. If they are then we are fine,
// else we don't inline
//--------------------------------------------------------------------------
static BOOL
types_are_compatible (TY_IDX ty_actual, TY_IDX ty_formal, BOOL lang)
{

    // if it is not a value parameter then check to see
    // if it is of type KIND_SCALAR, if true then return
    // FALSE

    TYPE_ID formal_element_size, actual_element_size;

    if ((ty_actual == 0) || (ty_formal == 0))
	return FALSE;		// No type info

    if (lang) {
        BOOL formal_is_array, actual_is_array;
    
        if (TY_kind(ty_formal) == KIND_POINTER) {
    	    ty_formal = TY_pointed(ty_formal);
    	    formal_is_array = (TY_kind(ty_formal) == KIND_ARRAY);
	}
        else
    	    formal_is_array = (TY_kind(ty_formal) == KIND_ARRAY);
    
        if (TY_kind(ty_actual) == KIND_POINTER) {
    	    ty_actual = TY_pointed(ty_actual);
    	    actual_is_array = (TY_kind(ty_actual) == KIND_ARRAY);
        }
        else
    	    actual_is_array = (TY_kind(ty_actual) == KIND_ARRAY);
    
        // PV 374125, don't inline in this case
        // where one of the parameters is an array and the
        // other is a scalar
        if ((!actual_is_array && formal_is_array))
    	    return FALSE;
    
        if (actual_is_array) {
	    ty_actual = base_type_of_array(ty_actual);
    	    actual_element_size = BASETYPE(ty_actual);
	}
        else 
    	    actual_element_size = BASETYPE(ty_actual);
    
        if (formal_is_array)  {
	    ty_formal = base_type_of_array(ty_formal);
    	    formal_element_size = BASETYPE(ty_formal);
	}
        else 
    	    formal_element_size = BASETYPE(ty_formal);
        
        if (formal_element_size == actual_element_size)
    	    return TRUE;
    }
	
    TYPE_ID desc = BASETYPE(ty_formal);
    if ((desc == 0) && (TY_kind (ty_formal) == KIND_FUNCTION)) {
      TY_IDX ret_ty_idx = TYLIST_type (Tylist_Table[TY_tylist (ty_formal)]);

      if (ret_ty_idx == 0)
        return FALSE;
      else
        desc = BASETYPE(ret_ty_idx);
    }

    if (desc == 0)
      return FALSE;	// Don't know what the basetype is

    OPCODE stid  = Stid_Opcode[desc];

    if (desc == MTYPE_M)
      /* we just check the size of the formal and actual */
      return (TY_size(ty_formal) == TY_size(ty_actual));
    
    if (stid == OPC_UNKNOWN)
      return FALSE;

    TYPE_ID rtype = BASETYPE(ty_actual);
    TYPE_ID ltype = OPCODE_desc(stid);
    if (IPO_Types_Are_Compatible(ltype, rtype))
      return TRUE;

    return FALSE;
}

//--------------------------------------------------------------------------
// check if return types are okay
//--------------------------------------------------------------------------
static BOOL
param_types_are_compatible (IPA_NODE* caller_node, IPA_NODE* callee_node, IPA_EDGE *ed)
{
    INT num_formals = callee_node->Num_Formals();

    if (!num_formals) // No types to check
	return TRUE;

    SUMMARY_FORMAL* callee_formal = IPA_get_formal_array(callee_node);
    SUMMARY_ACTUAL* call_actual = IPA_get_actual_array(caller_node);
    SUMMARY_SYMBOL* caller_symbols = IPA_get_symbol_array(caller_node);
    SUMMARY_SYMBOL* callee_symbols = IPA_get_symbol_array(callee_node);

    SUMMARY_ACTUAL *actuals = &call_actual[ed->Summary_Callsite()->Get_actual_index()];
    SUMMARY_FORMAL *formals = &callee_formal[callee_node->Summary_Proc()->Get_formal_index()];

#ifndef _STANDALONE_INLINER
    const IPC_GLOBAL_IDX_MAP* callee_idx_maps = 
      IP_FILE_HDR_idx_maps(callee_node->File_Header());
    const IPC_GLOBAL_IDX_MAP* caller_idx_maps = 
      IP_FILE_HDR_idx_maps(caller_node->File_Header());
    Is_True(callee_idx_maps && caller_idx_maps, 
            ("idx_maps for caller and callee are not set up\n"));
#endif // _STANDALONE_INLINER

    BOOL lang = ((callee_node->Summary_Proc()->Get_lang() == LANG_F77) || 
    		(callee_node->Summary_Proc()->Get_lang() == LANG_F90));

    for (INT i=0; i<num_formals; ++i) {
	TY_IDX ty_formal = formals[i].Get_ty();
	TY_IDX ty_actual = actuals[i].Get_ty();

	if (!IPA_Enable_Inline_Char_Array) {
            if (((TY_kind(ty_formal) == KIND_SCALAR) && 
			(formals[i].Is_ref_parm() || lang)) &&
	    		actuals[i].Is_value_parm())
	        return FALSE;   // formal is decl. as a scalar formal reference parm and 
			        // actual is passed-by-value, don't match
	}

	if (!IPA_Enable_Inline_Var_Dim_Array && formals[i].Is_var_dim_array())
	    return FALSE;	// Don't inline var-dim array

        // turn off inlining when the formal is of
        // sclass: scalar_formal_ref and its kind is STRUCT
        // We can only inline scalar or array FORMAL_REFS
        if (formals[i].Is_ref_parm() && (TY_kind(ty_formal) == KIND_STRUCT)) {
	    if (!IPA_Enable_Inline_Struct) 
                return FALSE;
	    else {
		if (TY_kind(ty_actual) == KIND_POINTER) {
    	    	    if (TY_kind(TY_pointed(ty_actual)) == KIND_ARRAY) 
		  	if (!IPA_Enable_Inline_Struct_Array_Actual)
			    return FALSE;
                }
                else {
    	            if (TY_kind(ty_actual) == KIND_ARRAY)
		  	if (!IPA_Enable_Inline_Struct_Array_Actual)
			    return FALSE;
	        }
	    }
	}

	SUMMARY_SYMBOL* s = callee_symbols + formals[i].Get_symbol_index();

	if (actuals[i].Get_symbol_index () >= 0) {
            SUMMARY_SYMBOL* caller_sym =
            	caller_symbols + actuals[i].Get_symbol_index ();

            if (s->Is_addr_f90_target () != caller_sym->Is_addr_f90_target ())
            	return FALSE;
	} else if (s->Is_addr_f90_target ())
	    return FALSE;

	if (IPA_Enable_Inline_Optional_Arg && s->Is_optional() &&
		(ty_actual == 0))  // Skip over optional argument
	    continue;

#ifdef TARG_XTENSA
        if (!TY_has_prototype(ST_pu_type(callee_node->Func_ST())) &&
            (ty_formal == MTYPE_To_TY(MTYPE_U1) ||
             ty_formal == MTYPE_To_TY(MTYPE_U2) ||
             ty_formal == MTYPE_To_TY(MTYPE_I1) ||
             ty_formal == MTYPE_To_TY(MTYPE_I2))) {
          ty_formal = MTYPE_To_TY(MTYPE_I4);
        }
#endif        
	if (!types_are_compatible(ty_actual, ty_formal, lang))
	    return FALSE;
    }
    return TRUE;
}

void
IPA_NODE::UpdateSize (IPA_NODE *callee, IPA_EDGE *ed)
{
    _pu_size += callee->PU_Size();
    _pu_size.Inc_PU_Size (-1, callee->Num_Formals(), -1);

    UINT32 combined_loop_depth = 
      ed->Summary_Callsite()->Get_loopnest() + callee->Max_Loop_Depth();
    if (combined_loop_depth > _max_loop_depth) {
      _max_loop_depth = combined_loop_depth;
    }

#ifndef _STANDALONE_INLINER
    if (IPA_Use_Effective_Size && 
        Has_frequency() &&
	callee->Has_frequency () && 
        ed->Summary_Callsite()->Get_frequency_count().Known()) {
      SUMMARY_FEEDBACK *fb = Get_feedback ();
      SUMMARY_FEEDBACK *callee_fb = callee->Get_feedback ();
      fb->Inc_effective_bb_count (callee_fb->Get_effective_bb_count () - 1);
      fb->Inc_effective_stmt_count (callee_fb->Get_effective_stmt_count () + 
                                    callee->Num_Formals());
      fb->Set_cycle_count(fb->Get_cycle_count() + 
                          (ed->Get_frequency() / callee->Get_frequency()) * 
                          callee_fb->Get_cycle_count());
      fb->Set_ccount(fb->Get_ccount() +
                     (ed->Get_frequency() / callee->Get_frequency()) * 
                     callee->Get_ccount());

      if (Trace_IPA) {
        fprintf(TFile, " updated cycle count: estimated = ");
        Get_cycle_count().Print(TFile);
        fprintf(TFile, ", rsr.ccount-based = ");
        Get_ccount().Print(TFile);
        fprintf(TFile, "\n");
      }
    }
#endif // _STANDALONE_INLINER
    
} // IPA_NODE::UpdateSize


//--------------------------------------------------------------
// now update the call graph, i.e. simply increment the inline
// count, set the edge to no_inline, adjust the total program
// size
//--------------------------------------------------------------
void
Update_Call_Graph (IPA_NODE *n)
{

    /* by removing a call, we decrease the number of basic block by 1 */
    PU_SIZE size = n->PU_Size ();
    size.Inc_PU_Size (-1, 0, -1);
    n->Set_PU_Size (size);

    size.Set_PU_Size (1, 0, 1);
    Total_Prog_Size -= size.Weight ();

} // Update_Call_Graph 


/*-------------------------------------------------------------*/
/* check to see if the callee, being procedure with nested PU, */
/* can be inlined only if all its nested PU are inlined        */
/*-------------------------------------------------------------*/
static BOOL
no_inline_pu_with_nested_pus(IPA_NODE* caller, IPA_GRAPH* cg)
{
    const PU_Info* pu = caller->PU_Info ();
    if (pu == NULL) 			// alt.entry
	return TRUE;
    for (pu = PU_Info_child (pu); pu; pu = PU_Info_next (pu)) {

	const AUX_PU& aux_pu =
	    Aux_Pu_Table [ST_pu (St_Table [PU_Info_proc_sym (pu)])];
	const IPA_NODE* child = cg->Node_User (AUX_PU_node (aux_pu));
#ifdef _STANDALONE_INLINER
	if (child && (!(child->Has_Inline_Attrib() || 
                  child->Has_Must_Inline_Attrib())))
#else // _STANDALONE_INLINER
	if (child && !child->Is_Deletable ())
#endif // _STANDALONE_INLINER
	    return TRUE;
    }
    return FALSE;
}


/*-------------------------------------------------------------*/
/* check to see if we should be inlining                       */
/*-------------------------------------------------------------*/
static BOOL
do_inline (IPA_EDGE *ed, IPA_NODE *caller,
	   IPA_NODE *callee, const IPA_CALL_GRAPH *cg)
{
    BOOL result = TRUE;
    char *reason = 0;

    if (ed->Has_Noinline_Attrib()) {
	reason = "edge is skipped";
	result = FALSE;
    }
    else if (IPA_Enable_DCE && ed->Is_Deletable ()) {
        // call deleted by DCE
	reason = "call deleted by DCE";
	result = FALSE;
    }
    else if (!IPA_Enable_Inline_Nested_PU && caller->Is_Nested_PU ()) {
        // Check for nested PU
	result = FALSE;
	reason = "caller is a nested procedure";
    } else if ( PU_uplevel (callee->Get_PU ()) &&
		((!IPA_Enable_Inline_Nested_PU) ||
		no_inline_pu_with_nested_pus(callee, cg->Graph ()))) {
	if (callee->Has_Must_Inline_Attrib()) {
	    callee->Clear_Must_Inline_Attrib ();
	    reason = "callee has nested procedure(s) so ignore user MUST inline request";
	}
	else 
	    reason = "callee has nested procedure(s)";
	callee->Set_Noinline_Attrib ();
	result = FALSE;
    } else if (cg->Graph()->Is_Recursive_Edge (ed->Edge_Index())) {
#ifndef _STANDALONE_INLINER
	BOOL set_recursive_in_edge = TRUE;

#ifdef __FIXME__
	if (ed->Has_frequency () && callee->Has_frequency () &&
            ed->Get_frequency().Known() && callee->Get_frequency ().Known()) {
          if (compute_hotness (ed, callee, Effective_weight (callee))
              < IPA_Min_Hotness)
            set_recursive_in_edge = FALSE;
	}
#endif

	if (set_recursive_in_edge)
#endif //  _STANDALONE_INLINER
	    callee->Set_Recursive_In_Edge ();
	result = FALSE;
	reason = "callee is recursive";
    } else if (callee->Has_Varargs()) {
	result = FALSE;
	reason = "callee is varargs";
    } else if (callee->Summary_Proc()->Is_alt_entry() ||
	       callee->Summary_Proc()->Has_alt_entry() || 
	       caller->Summary_Proc()->Is_alt_entry()) {
	result = FALSE;
	reason = "function with alternate entry point";
    } else if (ed->Num_Actuals() < callee->Num_Formals()) {
	result = FALSE;
	reason = "number of parameters mismatched";
    } else if (callee->Summary_Proc()->Has_formal_pragma()) {
	result = FALSE;
	reason = "callee has pragmas which are associated with formals";
    } else if (callee->Summary_Proc()->Has_mp_needs_lno()) {
	result = FALSE;
	reason = "callee has flag that suggested that it should be MP'ed";
    } else if (callee->Summary_Proc()->Has_noinline_parallel_pragma()) {
	result = FALSE;
	reason = "callee has parallel pragmas that suggest turning off inlining";
    } else if ((caller->Summary_Proc()->Has_parallel_pragma() ||
	       caller->Summary_Proc()->Has_parallel_region_pragma()) &&
	       callee->Summary_Proc()->Has_var_dim_array()) {
	result = FALSE;
	reason = "callee has VLAs and caller has parallel_pragma"; 
    } else if (callee->Summary_Proc()->Has_restrict()) {
      result = FALSE;
      reason = "callee has restrict locals or formals";
    } else if (caller->Summary_Proc()->Has_parallel_region_pragma() &&
	       callee->Summary_Proc()->Has_pdo_pragma()) {
	result = FALSE;
	reason = "callee has PDO pramgas and caller has parallel_pragma"; 
    } else if (ed->Summary_Callsite()->Is_no_inline())  {

#ifdef _STANDALONE_INLINER
        // check for pragmas and command line options before setting this
  	// call to no inline
	if ( !ed->Has_Must_Inline_Attrib() && !callee->Has_Must_Inline_Attrib()) {
#endif // _STANDALONE_INLINER 

            result = FALSE;
	    reason = "callsite pragma requested not to inline";
#ifdef _STANDALONE_INLINER
	}
#endif // _STANDALONE_INLINER 

    } else if (ed->Summary_Callsite()->Is_must_inline() &&
	     !callee->Has_Noinline_Attrib())  {
        // Pragmas override commandline options
        // set the MustInline bit so that we inline regardless
        // of size
        ed->Set_Must_Inline_Attrib();

#ifdef _STANDALONE_INLINER
    } else if (callee->Summary_Proc()->Is_exc_inline() && !INLINE_Exceptions) {
#else // _STANDALONE_INLINER
    } else if (callee->Summary_Proc()->Is_exc_inline() && !IPA_Enable_Exc) {
#endif // _STANDALONE_INLINER
	result = FALSE;
	reason = "exception handling function";
    } else if (callee->Summary_Proc()->Is_exc_inline() &&
	     callee->Summary_Proc()->Has_pstatic()) {
	result = FALSE;
	reason = "exception handling code with pstatics";
    } else if ((UINT) cg->Node_Depth(callee) > IPA_Max_Depth) {
	result = FALSE;
	reason = "depth in call graph exceeds specified maximum";
    } else if (!ed->Has_Must_Inline_Attrib() &&
	     (callee->Has_Noinline_Attrib() ||
	      (callee->Summary_Proc()->Is_no_inline() && result) ||
	      (!callee->Has_Must_Inline_Attrib() && INLINE_None ))) {
	result = FALSE;
	reason = "user requested not to inline";
#ifdef _STANDALONE_INLINER
    // if an inline function has local statics the front end marks the function
    // pre-emptible (in an attempt to not inline it) and weak
    // The inliner doesn't inline this fn and should emit a message
    // that distinguishes it from the case where a function was not inlined
    // because it was NOT marked inline
    } else if ( ( (!callee->Summary_Proc()->Is_may_inline() &&
	    !callee->Summary_Proc()->Is_must_inline()) && 
	    !INLINE_Preemptible ) && 
	    ( !callee->Has_Must_Inline_Attrib() ) && 
	    !ed->Summary_Callsite()->Is_must_inline() && 
	    !ed->Has_Must_Inline_Attrib()) {
	result = FALSE;
	if ( callee->Summary_Proc()->Has_fstatic()) 
            reason = "function has local fstatics and is set preemptible";
	else
            reason = "function is preemptible and has not been set to mustinline";

#endif // _STANDALONE_INLINER
    }
    else if (!return_types_are_compatible(callee, ed)) {
	reason = "incompatible return types";
	result = FALSE;
    }
    else if (!param_types_are_compatible(caller, callee, ed)) {
	reason = "incompatible parameter types";
	result = FALSE;
    } else if (!callee->Has_Inline_Attrib() &&
		    Different_Named_Nonlinkonce_Sections(caller, callee) &&
		    !callee->Has_Must_Inline_Attrib()) {
      result = FALSE;
      reason = "not inlining across named sections";
    }
    else if (!IPA_Enable_Lang) {
	if ((callee->Summary_Proc()->Get_lang() == LANG_F77) || 
	    (caller->Summary_Proc()->Get_lang() == LANG_F77)) {
	    if ((callee->Summary_Proc()->Get_lang() != LANG_F77) || 
		(caller->Summary_Proc()->Get_lang() != LANG_F77)) {
		result = FALSE;
		reason = "not inlining across language boundaries";
	    }
	}
        else if ((callee->Summary_Proc()->Get_lang() == LANG_F90) || 
                 (caller->Summary_Proc()->Get_lang() == LANG_F90)) {
	    if ((callee->Summary_Proc()->Get_lang() != LANG_F90) || 
		(caller->Summary_Proc()->Get_lang() != LANG_F90)) {
		result = FALSE;
		reason = "not inlining across language boundaries";
	    }
	}
    } 

    if ( result == FALSE ) {
	Report_Reason ( callee, caller, reason , ed);
	return FALSE;
    } 

    return check_size_and_freq (ed, caller, callee, cg);

} // do_inline



// invocation cost for each IPA_EDGE during inline analysis
typedef AUX_IPA_EDGE<INT32> INVOCATION_COST;

// Assign to each call a "cost", which is used to determine the priority of
// inlining.  There are three factors, loopnest of the call, number of calls
// from the callee (0 means leaf), and size of the callee.  Also, we isolated
// out several boundary cases which we give higher priority:
//
// 1) loopnest > 0 && call_count == 0
// 2) loopnest == 0 && call_count == 0
// 3) loopnest > 0 && call_count > 0
// 4) loopnest == 0 && call_count > 0
//
// In the first 3 cases, the value of call_count does not matter,
// all we care is > or == 0.  We just sort by size.  In the 4th cases, we sort
// first by call_count, and then by the size.
static INT32
Estimated_Invocation_Cost(IPA_EDGE* edge, 
                          const IPA_CALL_GRAPH* cg, 
                          BOOL trace = FALSE)
{
  IPA_NODE *callee = cg->Callee(edge);
  INT loopnest = edge->Summary_Callsite()->Get_loopnest();

#ifdef _STANDALONE_INLINER
  INT32 cost = callee->Weight();
#else
  INT32 cost = Effective_weight(callee);

#ifdef __FIXME__
  // if feedback information is available, ignore the heuristics and
  // use the "hotness" of the callee instead
  if (edge->Has_frequency () && 
      callee->Has_frequency () &&
      edge->Get_frequency().Known() && 
      callee->Get_frequency ().Known()) {
    return INT32_MAX - (INT32) compute_hotness(edge, callee, cost);
  }
#endif
#endif // _STANDALONE_INLINER

  if (trace) {
    fprintf(TFile, "INL>  %s [%d BBs, %d STMTs, %d CALLs] -> %d | %d calls\n",
            callee->Name(), 
            callee->PU_Size().BB_Count(),
            callee->PU_Size().Stmt_Count(),
            callee->PU_Size().Call_Count(),
            cost, 
            cg->Num_In_Edges(callee));
  }

#ifndef TARG_XTENSA
  if (loopnest < 100)
    /* assume we never have loopnest > 100 */
    cost += ((100 - loopnest) << 11);

  if (callee->PU_Size().Call_Count () != 0) {
    if (loopnest > 0)
      cost += (1 << 22);
    else
      cost += (callee->PU_Size().Call_Count () << 22);
  }
#else
  // Order them simply by size, but favor leaves
  if (callee->PU_Size().Call_Count () != 0) 
    cost += (callee->PU_Size().Call_Count () << 5);
#endif    

  if (trace) {
    fprintf(TFile, "INL>   Loop depth: %d | Invocation cost = %d\n",
            loopnest, cost);
  }

  return cost;
}


// comparision function object for sorting the callsites
struct INVOCATION_COST_COMP
{
  const INVOCATION_COST& cost_vector;

  INVOCATION_COST_COMP(const INVOCATION_COST& c) : cost_vector (c) {}

  BOOL operator() (IPA_EDGE_INDEX e1, IPA_EDGE_INDEX e2) const {
    return cost_vector[e1] < cost_vector[e2];
  }
};

// For each node, create a list of call sites and sort them based on the
// cost function defined in Estimated_Invocation_Cost so that more
// desirable callees are inlined first.   
typedef vector<IPA_EDGE_INDEX> EDGE_INDEX_VECTOR;

static void
Get_Sorted_Callsite_List (IPA_NODE *n, IPA_CALL_GRAPH *cg,
			  INVOCATION_COST& cost_vector,
			  EDGE_INDEX_VECTOR& callsite_list)
{
  if (cg->Num_Out_Edges(n) == 0)
    return;

  IPA_SUCC_ITER edge_iter(cg, n);
  for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
    IPA_EDGE *edge = edge_iter.Current_Edge();
    if (edge) {
      IPA_EDGE_INDEX idx = edge->Array_Index();
      cost_vector[idx] = Estimated_Invocation_Cost(edge, cg);
      callsite_list.push_back(idx);
    }
  }   

  sort(callsite_list.begin(), callsite_list.end(), 
       INVOCATION_COST_COMP(cost_vector));
  
  if (Trace_Inline) {
    fprintf(TFile, 
            "INL> Processing call sites in %s "
            "[%d BBs, %d STMTs, %d CALLs] -> %d\n", 
            n->Name(), 
            n->PU_Size().BB_Count(),
            n->PU_Size().Stmt_Count(),
            n->PU_Size().Call_Count(),
            n->Weight());
    for (size_t i = 0; i < callsite_list.size(); i++) {
      Estimated_Invocation_Cost(cg->Edge(callsite_list[i]), cg, TRUE);
    }
  }
}


// decide if the given call could be deleted or inlined
static void
Analyze_call (IPA_NODE* caller, IPA_EDGE* edge, const IPA_CALL_GRAPH* cg)
{
    IPA_NODE* callee = cg->Callee (edge);

#ifndef _STANDALONE_INLINER
	    
    if (IPA_Enable_DCE) {
	// Do dead call elimination analysis
		
	if (!callee->Summary_Proc()->Has_side_effect() &&
	    ((edge->Is_Deletable () || // set by const. propagation

	      (cg->Node_Depth (callee) == 0 &&
	       !callee->Has_Direct_Mod_Ref() &&
	       !callee->Summary_Proc()->Is_alt_entry () &&
	       !callee->Summary_Proc()->Has_alt_entry () &&
	       !caller->Summary_Proc()->Is_alt_entry () &&
	       return_types_are_compatible (callee, edge))))) {

	    edge->Set_Deletable();
	    if (Trace_IPA) {
		fprintf (TFile, "%s called from ",
			 DEMANGLE (callee->Name()));
		fprintf(TFile, "%s deleted\n",
			DEMANGLE (caller->Name())); 
	    }

	    Update_Call_Graph (caller);
			
	    if (IPA_Enable_DFE)
		Update_Total_Prog_Size (caller, callee, cg);

	    return;		// edge deleted, skip the inline analysis
	}
    }

    if (! IPA_Enable_Inline)
	return;
    
#endif // _STANDALONE_INLINER

    if (do_inline (edge, caller, callee, cg)) {
	edge->Set_Inline_Attrib ();
	Total_Inlined++;
    } else {
	edge->Clear_All_Inline_Attrib ();
	if (callee->Has_Inline_Attrib())
	    callee->Clear_Inline_Attrib ();
	Total_Not_Inlined++;
    }
} // Analyze_call

#if 0
static void
Update_Exclusive_Ccounts(IPA_CALL_GRAPH *cg, MEM_POOL *pool)
{
  IPA_NODE_ITER cg_iter(cg, LEVELORDER, pool);

  // traverse all nodes at levelorder
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
    IPA_NODE *caller = cg_iter.Current();
    if (caller && cg->Num_Out_Edges(caller) > 0) {
      IPA_SUCC_ITER edge_iter(cg, caller);
      for (edge_iter.First(); !edge_iter.Is_Empty(); edge_iter.Next()) {
        IPA_EDGE *edge = edge_iter.Current_Edge();
        if (edge) {
          IPA_NODE *callee = cg->Callee(edge);
          if (caller->Has_frequency() &&
              callee->Has_frequency() && 
              edge->Summary_Callsite()->Get_frequency_count().Known()) {

            SUMMARY_FEEDBACK *fb = caller->Get_feedback();
            fb->Set_ccount_exclusive(fb->Get_ccount_exclusive() -
              (edge->Get_frequency() / callee->Get_frequency()) * 
              callee->Get_ccount_inclusive());
          }
        }
      }
    }
    if (Trace_IPA && caller) {
      fprintf(TFile, "ccount_exclusive(%s): ", caller->Name());
      caller->Get_ccount_exclusive().Print(TFile);
      fprintf(TFile, "\n");
    }
  }
}
#endif

/*-------------------------------------------------------------------------*/
/* Solve the interprocedural analysis phase of inlining.                   */
/*-------------------------------------------------------------------------*/
void
Perform_Inline_Analysis (IPA_CALL_GRAPH* cg, MEM_POOL* pool)
{
  Trace_Inline = Get_Trace(TP_IPA, IPA_TRACE_INLINING);
#if 0
  Update_Exclusive_Ccounts(cg, pool);
#endif
  INVOCATION_COST cost_vector(cg, pool);
    
  Init_inline_parameters();

#ifndef _STANDALONE_INLINER
  inline_count = CXX_NEW(INLINE_COUNTER_ARRAY(cg, pool), pool);
#endif

  EDGE_INDEX_VECTOR callsite_list;
  IPA_NODE_ITER cg_iter(cg, INLINEORDER, pool);

  // traverse all nodes at levelorder
  for (cg_iter.First(); !cg_iter.Is_Empty(); cg_iter.Next()) {
    IPA_NODE* caller = cg_iter.Current();
    if (caller) {
      callsite_list.clear();
      Get_Sorted_Callsite_List(caller, cg, cost_vector, callsite_list);

      EDGE_INDEX_VECTOR::const_iterator last = callsite_list.end();
      for (EDGE_INDEX_VECTOR::iterator first = callsite_list.begin();
           first != last; ++first) {
        Analyze_call(caller, cg->Edge(*first), cg);
      }
    }
  }

#ifndef _STANDALONE_INLINER
  CXX_DELETE (inline_count, pool);
  inline_count = NULL;
#endif  // _STANDALONE_INLINER

} // Perform_Inline_Analysis
