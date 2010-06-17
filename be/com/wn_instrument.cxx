/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: wn_instrument.cxx
// $Revision: 1.36 $
// $Date: 2000/05/31 19:41:27 $
// $Author: dlstephe $
// $Source: /isms/cmplrs.src/osprey1.0/be/com/RCS/wn_instrument.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// The procedures WN_Instrument and WN_Annotate, declared in
// wn_instrument.h, invoke the instrumentation and annotation phases of
// feedback collection.
//
// The class WN_INSTRUMENT_WALKER implements the instrumentation and
// annotation phases of feedback collection.  Only WN_Instrument and
// WN_Annotate should need to reference the WN_INSTRUMENT_WALKER class.
//
// Interface of WN_INSTRUMENT_WALKER to WN_Instrument and WN_Annotate:
//
// WN_INSTRUMENT_WALKER( BOOL instrumenting, PROFILE_PHASE phase );
//
//   If instrumenting is TRUE, insert instrumentation.
//   If instrumenting is FALSE, annotate the WHIRL with feedback data.
//   In either case, set phase number that the instrumenter is in.
//
// ~WN_INSTRUMENT_WALKER();
//
// void Tree_Walk( WN *wn );
//
//   Walk the tree and perform instrumentation or annotate feedback.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */

#pragma hdrstop
#define USE_STANDARD_TYPES

#include "defs.h"

#include <stdlib.h>
#include <vector>
#include <stack>

using namespace std;

// #include "stab.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "tracing.h"
#include "config_opt.h"         // for Instrumentation_Enabled
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include "profile_com.h"
#include "instr_reader.h"
#include "targ_sim.h"
#include "wn_pragmas.h"
#include "ir_reader.h"		// fdump_tree
#include "glob.h"
#include "errors.h"
#include "be_symtab.h"
#include "vho_lower.h"
#include "tie.h"
#include "intrn_info.h"
#include "fb_whirl.h"
#include "wn_instrument.h"
#include "erglob.h"

// Use this switch to turn on/off additional debugging messages

#define Instrumenter_DEBUG 0


// ALSO, SEE:
//   common/com/profile_com.h
//   common/com/instr_reader.h
//   common/instrument/instr_reader.cxx

// Invokes instrumentation:
//   be/be/driver.cxx (through WN_Instrument and wiw_wopt.Tree_Walk)


// ====================================================================
//
// Instrumentation_File_Name is the prefix for the names of the
// feedback data files.
//
// ====================================================================


static char * Instrumentation_File_Name = "";
char * fb_entry_point = "main";


// ====================================================================
//
// Types from the instrumentation library, initialized once per file.
//
// ====================================================================


static TY_IDX Freq_Int_Type;

static TY_IDX Invoke_Profile_Type;
static TY_IDX Invoke_Profile_Vector_Type;
static TY_IDX Ptr_Invoke_Profile_Type;
static TY_IDX Ptr_Invoke_Profile_Vector_Type;

static TY_IDX Branch_Profile_Type;
static TY_IDX Branch_Profile_Vector_Type;
static TY_IDX Ptr_Branch_Profile_Type;
static TY_IDX Ptr_Branch_Profile_Vector_Type;

static TY_IDX Loop_Profile_Type;
static TY_IDX Loop_Profile_Vector_Type;
static TY_IDX Ptr_Loop_Profile_Type;
static TY_IDX Ptr_Loop_Profile_Vector_Type;

static TY_IDX Circuit_Profile_Type;
static TY_IDX Circuit_Profile_Vector_Type;
static TY_IDX Ptr_Circuit_Profile_Type;
static TY_IDX Ptr_Circuit_Profile_Vector_Type;

static TY_IDX Call_Profile_Type;
static TY_IDX Call_Profile_Vector_Type;
static TY_IDX Ptr_Call_Profile_Type;
static TY_IDX Ptr_Call_Profile_Vector_Type;

static INTRINSIC RSR_CCOUNT_Intrinsic = INTRINSIC_NONE;

static BOOL Profile_Types_Initialized = FALSE;


static void
Initialize_Profile_Types()
{
  Profile_Types_Initialized = TRUE;

  FLD_HANDLE fld;
  
  Freq_Int_Type = 
    MTYPE_To_TY(Instrumentation_Bits == 32 ? MTYPE_U4 : MTYPE_U8);

  // Invoke_Profile struct
  TY& invoke_profile_ty = New_TY(Invoke_Profile_Type);
  TY_Init(invoke_profile_ty,
          TY_size(Freq_Int_Type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Invoke_Profile"));
  Set_TY_align(Invoke_Profile_Type, TY_align(Freq_Int_Type));

  Ptr_Invoke_Profile_Type = Make_Pointer_Type(Invoke_Profile_Type);

  // Invoke_Profile fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("invoke_count"), Freq_Int_Type, 0);
  Set_TY_fld(invoke_profile_ty, fld);
  Set_FLD_last_field(fld);

  // Invoke_Profile_Vector struct
  TY& invoke_profile_vector_ty = New_TY(Invoke_Profile_Vector_Type);
  TY_Init(invoke_profile_vector_ty,
          2 * MTYPE_byte_size(MTYPE_U4) + MTYPE_byte_size(Pointer_type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Invoke_Profile_Vector"));
  Set_TY_align(Invoke_Profile_Vector_Type, Pointer_Size);
  
  Ptr_Invoke_Profile_Vector_Type = 
    Make_Pointer_Type(Invoke_Profile_Vector_Type);

  // Invoke_Profile_Vector fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_size"), MTYPE_To_TY(MTYPE_U4), 0);
  Set_TY_fld(invoke_profile_vector_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_lastidx"), MTYPE_To_TY(MTYPE_I4), 4);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_array"), Ptr_Invoke_Profile_Type, 8);
  Set_FLD_last_field(fld);


  // Branch_Profile struct
  TY& branch_profile_ty = New_TY(Branch_Profile_Type);
  TY_Init(branch_profile_ty,
          2 * TY_size(Freq_Int_Type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Branch_Profile"));
  Set_TY_align(Branch_Profile_Type, TY_align(Freq_Int_Type));

  Ptr_Branch_Profile_Type = Make_Pointer_Type(Branch_Profile_Type);

  // Branch_Profile fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("taken"), Freq_Int_Type, 0);
  Set_TY_fld(branch_profile_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("not_taken"), Freq_Int_Type, TY_size(Freq_Int_Type));
  Set_FLD_last_field(fld);

  // Branch_Profile_Vector struct
  TY& branch_profile_vector_ty = New_TY(Branch_Profile_Vector_Type);
  TY_Init(branch_profile_vector_ty,
          2 * MTYPE_byte_size(MTYPE_U4) + MTYPE_byte_size(Pointer_type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Branch_Profile_Vector"));
  Set_TY_align(Branch_Profile_Vector_Type, Pointer_Size);
  
  Ptr_Branch_Profile_Vector_Type = 
    Make_Pointer_Type(Branch_Profile_Vector_Type);

  // Branch_Profile_Vector fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_size"), MTYPE_To_TY(MTYPE_U4), 0);
  Set_TY_fld(branch_profile_vector_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_lastidx"), MTYPE_To_TY(MTYPE_I4), 4);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_array"), Ptr_Branch_Profile_Type, 8);
  Set_FLD_last_field(fld);


  // Loop_Profile struct
  TY& loop_profile_ty = New_TY(Loop_Profile_Type);
  TY_Init(loop_profile_ty,
          4 * TY_size(Freq_Int_Type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Loop_Profile"));
  Set_TY_align(Loop_Profile_Type, TY_align(Freq_Int_Type));

  Ptr_Loop_Profile_Type = Make_Pointer_Type(Loop_Profile_Type);

  // Loop_Profile fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("invocation_count"), Freq_Int_Type, 0);
  Set_TY_fld(loop_profile_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("total_trip_count"), Freq_Int_Type, 
           TY_size(Freq_Int_Type));
  fld = New_FLD();
  FLD_Init(fld, Save_Str("last_trip_count"), Freq_Int_Type, 
           2 * TY_size(Freq_Int_Type));
  fld = New_FLD();
  FLD_Init(fld, Save_Str("num_zero_trips"), Freq_Int_Type, 
           3 * TY_size(Freq_Int_Type));
  Set_FLD_last_field(fld);

  // Loop_Profile_Vector struct
  TY& loop_profile_vector_ty = New_TY(Loop_Profile_Vector_Type);
  TY_Init(loop_profile_vector_ty,
          2 * MTYPE_byte_size(MTYPE_U4) + MTYPE_byte_size(Pointer_type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Loop_Profile_Vector"));
  Set_TY_align(Loop_Profile_Vector_Type, Pointer_Size);
  
  Ptr_Loop_Profile_Vector_Type = 
    Make_Pointer_Type(Loop_Profile_Vector_Type);

  // Loop_Profile_Vector fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_size"), MTYPE_To_TY(MTYPE_U4), 0);
  Set_TY_fld(loop_profile_vector_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_lastidx"), MTYPE_To_TY(MTYPE_I4), 4);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_array"), Ptr_Loop_Profile_Type, 8);
  Set_FLD_last_field(fld);


  // Circuit_Profile struct
  TY& circuit_profile_ty = New_TY(Circuit_Profile_Type);
  TY_Init(circuit_profile_ty,
          2 * TY_size(Freq_Int_Type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Circuit_Profile"));
  Set_TY_align(Circuit_Profile_Type, TY_align(Freq_Int_Type));

  Ptr_Circuit_Profile_Type = Make_Pointer_Type(Circuit_Profile_Type);

  // Circuit_Profile fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("right_taken"), Freq_Int_Type, 0);
  Set_TY_fld(circuit_profile_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("neither_taken"), Freq_Int_Type, 
           TY_size(Freq_Int_Type));
  Set_FLD_last_field(fld);

  // Circuit_Profile_Vector struct
  TY& circuit_profile_vector_ty = New_TY(Circuit_Profile_Vector_Type);
  TY_Init(circuit_profile_vector_ty,
          2 * MTYPE_byte_size(MTYPE_U4) + MTYPE_byte_size(Pointer_type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Circuit_Profile_Vector"));
  Set_TY_align(Circuit_Profile_Vector_Type, Pointer_Size);
  
  Ptr_Circuit_Profile_Vector_Type = 
    Make_Pointer_Type(Circuit_Profile_Vector_Type);

  // Circuit_Profile_Vector fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_size"), MTYPE_To_TY(MTYPE_U4), 0);
  Set_TY_fld(circuit_profile_vector_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_lastidx"), MTYPE_To_TY(MTYPE_I4), 4);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_array"), Ptr_Circuit_Profile_Type, 8);
  Set_FLD_last_field(fld);


  // Call_Profile struct
  TY& call_profile_ty = New_TY(Call_Profile_Type);
  TY_Init(call_profile_ty,
          2 * TY_size(Freq_Int_Type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Call_Profile"));
  Set_TY_align(Call_Profile_Type, TY_align(Freq_Int_Type));

  Ptr_Call_Profile_Type = Make_Pointer_Type(Call_Profile_Type);

  // Call_Profile fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("entry_count"), Freq_Int_Type, 0);
  Set_TY_fld(call_profile_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("exit_count"), Freq_Int_Type, TY_size(Freq_Int_Type));
  Set_FLD_last_field(fld);

  // Call_Profile_Vector struct
  TY& call_profile_vector_ty = New_TY(Call_Profile_Vector_Type);
  TY_Init(call_profile_vector_ty,
          2 * MTYPE_byte_size(MTYPE_U4) + MTYPE_byte_size(Pointer_type),
          KIND_STRUCT,
          MTYPE_M,
          Save_Str("Call_Profile_Vector"));
  Set_TY_align(Call_Profile_Vector_Type, Pointer_Size);
  
  Ptr_Call_Profile_Vector_Type = 
    Make_Pointer_Type(Call_Profile_Vector_Type);

  // Call_Profile_Vector fields
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_size"), MTYPE_To_TY(MTYPE_U4), 0);
  Set_TY_fld(call_profile_vector_ty, fld);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_lastidx"), MTYPE_To_TY(MTYPE_I4), 4);
  fld = New_FLD();
  FLD_Init(fld, Save_Str("_array"), Ptr_Call_Profile_Type, 8);
  Set_FLD_last_field(fld);
  
  // RSR_CCOUNT intrinsic
  if (!OPT_Estimate_Ccount) {
    TIE_MACRO *rsr_ccount_macro = tie_info->tie_macro("_TIE_xt_timer_RSR_CCOUNT");
    if (rsr_ccount_macro)
      RSR_CCOUNT_Intrinsic = Tie_Macro_Id_To_Intrinsic(rsr_ccount_macro->id());
    else
      DevWarn("Couldn't find tie_macro for _TIE_xt_timer_RSR_CCOUNT");
  }
}


// ====================================================================
//
// WN_INSTRUMENT_WALKER class declaration
//
// ====================================================================


class WN_INSTRUMENT_WALKER {
private:

  // ------------------------------------------------------------------
  // Private members of WN_INSTRUMENT_WALKER should be accessed only
  // through the private methods provided further below.
  // ------------------------------------------------------------------

  // _mempool is the local memory pool

  MEM_POOL *         _mempool;

  // Phase at which instrumentation/annotation is occuring
  //  (PROFILE_PHASE declared in common/com/profile_com.h)

  PROFILE_PHASE      _phase;

  // _instrumenting is TRUE  if inserting instrumentation
  // _instrumenting is FALSE if annotating WHIRL with feedback data
  // _vho_lower is TRUE iff introduced instrumentation code requires
  //   VHO Lowering after tree walk finishes (to lower commas)
  // _in_preamble is TRUE iff the tree walker is currently within a
  //   PU's preamble (after FUNCT_ENTRY or ALTENTRY and before the
  //   WN_PRAGMA_PREAMBLE_END pragma.  The tree walker should skip all
  //   WHIRL nodes within a preamble.

  BOOL               _instrumenting;
  BOOL               _vho_lower;
  BOOL               _in_preamble;

  // Counter for each type of instrumentation

  UINT32             _count_invoke;
  UINT32             _count_branch;
  UINT32             _count_loop;
  UINT32             _count_circuit;
  UINT32             _count_call;
  UINT32             _count_switch;
  UINT32             _count_compgoto;

  // _instrument_count is total number of WHIRL nodes that have been
  //   instrumented/annotated; used as a checksum

  UINT32             _instrument_count;

  // _call_level is used to avoid nestedccount instrumentation 
  // when there is a call within a call foo(bar())

  UINT32 _call_level;

  // _pu_handle is the handle for all profile calls in PU
  // _fb_handle is a list of handles for feedback info for this PU

  ST *               _pu_handle;
  PU_PROFILE_HANDLES _fb_handle;

  // These STs are used for inlining calls to profiling functions

  ST * _cycle_count;
  ST * _invoke_table;
  ST * _invoke_info;
  ST * _branch_table;
  ST * _branch_info;
  ST * _loop_table;
  ST * _loop_info;
  ST * _circuit_table;
  ST * _circuit_info;
  ST * _call_table;
  ST * _call_info;

  // Instrumentation initialization code will be inserted just before
  // the WN_PRAGMA_PREAMBLE_END pragma that occurs after each entry's
  // preamble.  During the tree walk:
  // _entry_pragma_stmt and _entry_pragma_block are NULL if no
  //   WN_PRAGMA_PREAMBLE_END pragma have been encountered yet.
  //   Otherwise _entry_pragma_stmt holds one WN_PRAGMA_PREAMBLE_END
  //   pragma, and _entry_pragma_block is the BLOCK in which it appears.
  // _other_entry_pragmas contains (stmt, block) pairs for all other
  //   WN_PRAGMA_PREAMBLE_END pragmas that have been encountered.
  // These three members should only be accessed through the private
  // methods provided further below.

  typedef mempool_allocator<WN *>  ALLOC_TYPE;
  typedef deque<WN *, ALLOC_TYPE > DEQUE_TYPE;
  typedef stack<WN *, DEQUE_TYPE > STACK_TYPE;

  WN *               _entry_pragma_stmt;
  WN *               _entry_pragma_block;
  STACK_TYPE         _other_entry_pragmas;

  // _instrumentation_nodes contains all instrumentation WHIRL nodes
  //   that were inserted into the WHIRL tree but have not yet been
  //   passed by the tree walker.  Instrumentation nodes should not
  //   be instrumented or annotated.
  // This vector should only be accessed through the private methods
  //   provided further below.

  vector<WN *, mempool_allocator<WN *> >   _instrumentation_nodes;

  // SWITCH and COMPGOTO case information

  vector<INT32, mempool_allocator<INT32> > _switch_num_targets;
  vector<INT64, mempool_allocator<INT64> > _switch_case_values;
  vector<INT32, mempool_allocator<INT32> > _compgoto_num_targets;

  // ------------------------------------------------------------------
  // Undefined default methods made private to detect errors
  // ------------------------------------------------------------------

  WN_INSTRUMENT_WALKER(void);
  WN_INSTRUMENT_WALKER(const WN_INSTRUMENT_WALKER&);
  WN_INSTRUMENT_WALKER& operator=(const WN_INSTRUMENT_WALKER&);

  // ------------------------------------------------------------------

  // Get the PU handle.
  WN *PU_Handle() const {
    return WN_Ldid( Pointer_type, 0, _pu_handle, ST_type( _pu_handle ) );
  }

  // Get the feedback handle.
  PU_PROFILE_HANDLES& FB_Handle() { return _fb_handle; }

  // Is the feedback handle empty?
public:
  BOOL FB_Handle_Empty() { return _fb_handle.empty(); }
private:

  // Perform VHO_Lower on the tree after finishing tree walk
  void Set_VHO_Lower_Tree() { _vho_lower = TRUE; }

  // ------------------------------------------------------------------
  // List of WN_PRAGMA_PREAMBLE_END pragma WHIRL nodes and BLOCKs
  // ------------------------------------------------------------------

  // During the instrumention tree walk, the WN_PRAGMA_PREAMBLE_END
  // pragma WHIRL node for each PU entry (along with the BLOCK that
  // contains it) is stored in a list.  After the tree walk,
  // instrumentation initialization code is inserted at the end of each
  // entry's preamble.
  //
  // Implementation: The top pragma statement and block are kept in
  // _entry_pragma_stmt and _entry_pragma_block.  Any other pragmas are
  // stored in _other_entry_pragmas.  Since most PUs only have one
  // entry point, _other_entry_pragmas is usually not used.
  //
  // The methods below maintain the list of pragma nodes and blocks:
  //
  // Entry_List_Empty returns TRUE iff the list is empty.
  // Entry_Pragma returns the pragma at the top of the list.
  // Entry_Block returns the block which contains the Entry_Pragma.
  //   (If the list is empty, Entry_Pragma and Entry_Block return NULL)
  // Pop_Entry_Pragma discards the top pragma and block from the list.
  // Push_Entry_Pragma inserts a pragma and its block into the list.

  BOOL Entry_List_Empty() const { return _entry_pragma_stmt == NULL; }
  WN * Entry_Pragma()     const { return _entry_pragma_stmt; }
  WN * Entry_Block()      const { return _entry_pragma_block; }

  void Pop_Entry_Pragma() {
    if ( _other_entry_pragmas.empty() )
      _entry_pragma_stmt  = _entry_pragma_block = NULL;
    else {
      _entry_pragma_stmt  = _other_entry_pragmas.top();
      _other_entry_pragmas.pop();
      _entry_pragma_block = _other_entry_pragmas.top();
      _other_entry_pragmas.pop();
    }
  }

  void Push_Entry_Pragma( WN *stmt, WN *block ) {
    Is_True( stmt != NULL, ( "WN_INSTRUMENT_WALKER::Push_Entry_Pragma"
			     " stmt is NULL" ) );
    if ( _entry_pragma_stmt == NULL ) {
      _entry_pragma_stmt  = stmt;
      _entry_pragma_block = block;
    } else {
      _other_entry_pragmas.push( block );
      _other_entry_pragmas.push( stmt );
    }
  }

  // ------------------------------------------------------------------
  // List of instrumentation nodes that have not yet been passed
  // ------------------------------------------------------------------

  // _instrumentation_nodes contains all instrumentation WHIRL nodes
  // that were inserted into the WHIRL tree but have not yet been
  // passed by the tree walker.  Instrumentation nodes should not be
  // instrumented or annotated.  The following methods search and
  // update that list:

  // Record_Instrument_Node appends wn to the list of not-yet-passed
  //   instrumentation nodes.  It should be invoked on any node
  //   inserted _after_ the tree walker's current position.
  // Test_Instrument_Node returns TRUE iff wn is in the list of WHIRL
  //   not-yet-passed instrumentation nodes.  If Test_Instrument_Node
  //   does find a match, it removes that node from the list, so it
  //   should only be called once per node.

  void Record_Instrument_Node( WN *wn ) {
    if (WN_operator(wn) == OPR_BLOCK) {
      for (WN *n = WN_first(wn); n; n = WN_next(n))
        _instrumentation_nodes.push_back(n);
    }
    else
      _instrumentation_nodes.push_back(wn);
  }

  BOOL Test_Instrument_Node( WN *wn ) {
    INT t, last = _instrumentation_nodes.size() - 1;
    // Search from back to front, since next match is usually at end
    // reverse order
    for ( t = last; t >= 0; --t )
      if ( _instrumentation_nodes[t] == wn ) {
	_instrumentation_nodes[t] = _instrumentation_nodes[last];
	_instrumentation_nodes.pop_back();
	return TRUE;
      }
    return FALSE;
  }

  // ------------------------------------------------------------------
  // Methods to insert the instrumentation code into the WHIRL tree
  // ------------------------------------------------------------------

  // Is_Return_Store_Stmt returns TRUE iff the statement wn (after a
  //   call) is saving the return value to a pseudo-register.
  //   Instrumentation cannot be placed between a call and such a
  //   statement.
  // Is_Return_Store_Comma returns TRUE iff the statement wn (after a
  //   call) is a comma returning the return value of the last call in
  //   the comma's block.  Instrumentation cannot be placed after the
  //   call in the block.
  // Test_Dedicated_Reg returns TRUE iff the tree rooted at wn refers
  //   to a hardware register that instrumentation may overwrite.
  // Instrument_Before inserts an instrumentation WHIRL node in the
  //   current block before the current statement.
  // Instrument_After inserts an instrumentation WHIRL node in the
  //   current block before the current statement.
  // Instrument_Entry inserts an instrumentation WHIRL node at the end
  //   of the preamble of each of the program unit's entry points.
  // Create_Comma_Kid ensures that WN_kid( wn, kid_idx ) is a COMMA
  //   WHIRL node, so that instrumentation code can be inserted into
  //   it.  Create_Comma_Kid returns a pointer to the COMMA node.

  BOOL Is_Return_Store_Stmt( WN *wn );
  BOOL Is_Return_Store_Comma( WN *wn );
  BOOL Test_Dedicated_Reg( WN *wn );

  void Instrument_Before( WN *call, WN *current_stmt, WN *block );
  void Instrument_After( WN *call, WN *current_stmt, WN *block );
  void Instrument_Entry( WN *call );

  WN *Create_Comma_Kid( WN *wn, INT kid_idx );

  void Read_Ccount_Begin( WN *block );
  void Read_Ccount_End( WN *block );
  
  // ------------------------------------------------------------------
  // Instrumentation and Annotation of each type of WHIRL node
  // ------------------------------------------------------------------

  void Instrument_Invoke( WN *wn, INT32 id, WN *block );
  void Initialize_Instrumenter_Invoke( INT32 count );
  void Annotate_Invoke( WN *wn, INT32 id );
  void Instrument_Branch( WN *wn, INT32 id, WN *block );
  void Instrument_Cselect( WN *wn, INT32 id );
  void Initialize_Instrumenter_Branch( INT32 count );
  void Annotate_Branch( WN *wn, INT32 id );
  void Instrument_Loop( WN *wn, INT32 id, WN *block );
  void Initialize_Instrumenter_Loop( INT32 count );
  void Annotate_Loop( WN *wn, INT32 id );
  void Instrument_Circuit( WN *wn, INT32 id );
  void Initialize_Instrumenter_Circuit( INT32 count );
  void Annotate_Circuit( WN *wn, INT32 id );
  void Instrument_Call( WN *wn, INT32 id, WN *block );
  void Initialize_Instrumenter_Call( INT32 count );
  void Annotate_Call( WN *wn, INT32 id );
  void Instrument_Return( WN *wn, WN *block );
  void Instrument_Switch( WN *wn, INT32 id, WN *block );
  void Initialize_Instrumenter_Switch( INT32 count, WN *block );
  void Annotate_Switch( WN *wn, INT32 id );
  void Instrument_Compgoto( WN *wn, INT32 id, WN *block );
  void Initialize_Instrumenter_Compgoto( INT32 count );
  void Annotate_Compgoto( WN *wn, INT32 id );

  // ------------------------------------------------------------------

  // Walk the tree and perform instrumentation or annotate feedback
  void Tree_Walk_Node( WN *wn, WN *stmt, WN *block );

  // ------------------------------------------------------------------
  // Public interface to WN_Instrument and WN_Annotate
  // ------------------------------------------------------------------

public:

  // If instrumenting is TRUE, insert instrumentation.
  // If instrumenting is FALSE, annotate the WHIRL with feedbackd data.
  // In either case, set phase number that the instrumenter is in.

  WN_INSTRUMENT_WALKER( BOOL instrumenting, PROFILE_PHASE phase,
			MEM_POOL *local_mempool,
			PU_PROFILE_HANDLES fb_handles );
  ~WN_INSTRUMENT_WALKER() {}

  // Walk the tree and perform instrumentation or annotate feedback.

  void Tree_Walk( WN *wn );
};


// ====================================================================
//
// Utility functions to generate calls to instrumentation functions.
//
// ====================================================================

inline WN *
Gen_Param( WN *arg, UINT32 flag )
{
  return WN_CreateParm( WN_rtype( arg ), arg,
			MTYPE_To_TY( WN_rtype( arg ) ), flag );
}

int START_PARAM;

WN *
Gen_Call_Shell( char *name, TYPE_ID rtype, INT32 argc )
{
  START_PARAM = 0;

  TY_IDX  ty = Make_Function_Type( MTYPE_To_TY( rtype ) );
  ST     *st = Gen_Intrinsic_Function( ty, name );

  Clear_PU_no_side_effects( Pu_Table[ST_pu( st )] );
  Clear_PU_is_pure( Pu_Table[ST_pu( st )] );
  Set_PU_no_delete( Pu_Table[ST_pu( st )] );

  WN *wn_call = WN_Call( rtype, MTYPE_V, argc, st );

  WN_Set_Call_Default_Flags(  wn_call );
  // WN_Reset_Call_Non_Parm_Mod( wn_call );
  // WN_Reset_Call_Non_Parm_Ref( wn_call );

  return wn_call;
}

WN *
Gen_Call( char *name, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 0 );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 1 );
  WN_actual( call, START_PARAM ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  return call;
}


WN *
Gen_Call( char *name, WN *arg1, WN *arg2, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 2 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  return call;
}


WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 3 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  return call;
}


WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 4 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  return call;
}


WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 5 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  return call;
}


WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 6 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, WN *arg7, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 7 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 6 ) = Gen_Param( arg7, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, WN *arg7, WN *arg8, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 8 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 6 ) = Gen_Param( arg7, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 7 ) = Gen_Param( arg8, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, WN *arg7, WN *arg8, WN *arg9, 
	  TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 9 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 6 ) = Gen_Param( arg7, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 7 ) = Gen_Param( arg8, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 8 ) = Gen_Param( arg9, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, WN *arg7, WN *arg8, WN *arg9, WN *arg10,
	  TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 10 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 6 ) = Gen_Param( arg7, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 7 ) = Gen_Param( arg8, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 8 ) = Gen_Param( arg9, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 9 ) = Gen_Param(arg10, WN_PARM_BY_VALUE );
  return call;
}

WN *
Gen_Call( char *name, WN *arg1, WN *arg2, WN *arg3, WN *arg4,
	  WN *arg5, WN *arg6, WN *arg7, WN *arg8, WN *arg9, WN *arg10, 
	  WN *arg11,
	  TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 11 );
  WN_actual( call, START_PARAM +  0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  2 ) = Gen_Param( arg3, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  4 ) = Gen_Param( arg5, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  5 ) = Gen_Param( arg6, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  6 ) = Gen_Param( arg7, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  7 ) = Gen_Param( arg8, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  8 ) = Gen_Param( arg9, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM +  9 ) = Gen_Param(arg10, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 10 ) = Gen_Param(arg11, WN_PARM_BY_VALUE );
  return call;
}

// Some parameters are by reference, not by value


WN *
Gen_Call_ref3( char *name, WN *arg1, WN *arg2, WN *arg3,
	       TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 3 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_REFERENCE );
  return call;
}


WN *
Gen_Call_ref35( char *name, WN *arg1, WN *arg2, WN *arg3,
		WN *arg4, WN *arg5, TYPE_ID rtype = MTYPE_V )
{
  WN *call = Gen_Call_Shell( name, rtype, 5 );
  WN_actual( call, START_PARAM + 0 ) = Gen_Param( arg1, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 1 ) = Gen_Param( arg2, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 2 ) = Gen_Param( arg3, WN_PARM_BY_REFERENCE );
  WN_actual( call, START_PARAM + 3 ) = Gen_Param( arg4, WN_PARM_BY_VALUE );
  WN_actual( call, START_PARAM + 4 ) = Gen_Param( arg5, WN_PARM_BY_REFERENCE );
  return call;
}


// ====================================================================


WN_INSTRUMENT_WALKER::WN_INSTRUMENT_WALKER( BOOL instrumenting,
					    PROFILE_PHASE phase,
					    MEM_POOL *local_mempool,
					    PU_PROFILE_HANDLES fb_handles )
  : _mempool( local_mempool ),
    _phase( phase ),
    _instrumenting( instrumenting ),
    _vho_lower( FALSE ),
    _in_preamble( FALSE ),
    _count_invoke( 0 ),
    _count_branch( 0 ),
    _count_loop( 0 ),
    _count_circuit( 0 ),
    _count_call( 0 ),
    _count_switch( 0 ),
    _count_compgoto( 0 ),
    _instrument_count( 0 ),
    _call_level( 0 ),
    _pu_handle( 0 ),
    _fb_handle( fb_handles ),
    _entry_pragma_stmt( NULL ),
    _entry_pragma_block( NULL ),
    _other_entry_pragmas( DEQUE_TYPE( ALLOC_TYPE( local_mempool ) ) ),
    _instrumentation_nodes( local_mempool ),
    _switch_num_targets( local_mempool ),
    _switch_case_values( local_mempool ),
    _compgoto_num_targets( local_mempool )
{
  if ( _instrumenting ) {

    if (!Profile_Types_Initialized)
      Initialize_Profile_Types();
    
    _pu_handle = New_ST(CURRENT_SYMTAB);
    ST_Init(_pu_handle, 
            Save_Str("pu_instrument_handle"),
            CLASS_VAR, 
            SCLASS_PSTATIC, 
            EXPORT_LOCAL,
            MTYPE_To_TY(Pointer_type));
    
    _cycle_count = New_ST(CURRENT_SYMTAB);
    ST_Init(_cycle_count,
            Save_Str("cycle_count"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            MTYPE_To_TY(MTYPE_U4));
    
    _invoke_table = New_ST(CURRENT_SYMTAB);
    ST_Init(_invoke_table,
            Save_Str("invoke_table"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Invoke_Profile_Vector_Type);
    
    _invoke_info = New_ST(CURRENT_SYMTAB);
    ST_Init(_invoke_info,
            Save_Str("invoke_info"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Invoke_Profile_Type);

    _branch_table = New_ST(CURRENT_SYMTAB);
    ST_Init(_branch_table,
            Save_Str("branch_table"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Branch_Profile_Vector_Type);
    
    _branch_info = New_ST(CURRENT_SYMTAB);
    ST_Init(_branch_info,
            Save_Str("branch_info"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Branch_Profile_Type);

    _loop_table = New_ST(CURRENT_SYMTAB);
    ST_Init(_loop_table,
            Save_Str("loop_table"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Loop_Profile_Vector_Type);
    
    _loop_info = New_ST(CURRENT_SYMTAB);
    ST_Init(_loop_info,
            Save_Str("loop_info"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Loop_Profile_Type);

    _circuit_table = New_ST(CURRENT_SYMTAB);
    ST_Init(_circuit_table,
            Save_Str("circuit_table"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Circuit_Profile_Vector_Type);
    
    _circuit_info = New_ST(CURRENT_SYMTAB);
    ST_Init(_circuit_info,
            Save_Str("circuit_info"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Circuit_Profile_Type);

    _call_table = New_ST(CURRENT_SYMTAB);
    ST_Init(_call_table,
            Save_Str("call_table"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Call_Profile_Vector_Type);
    
    _call_info = New_ST(CURRENT_SYMTAB);
    ST_Init(_call_info,
            Save_Str("call_info"),
            CLASS_VAR, 
            SCLASS_AUTO, 
            EXPORT_LOCAL,
            Ptr_Call_Profile_Type);
  }
}


// ====================================================================
//
// Procedures to insert the instrumentation code into the WHIRL tree
//
// ====================================================================


BOOL 
WN_INSTRUMENT_WALKER::Is_Return_Store_Stmt( WN *wn )
{
  if ( wn && WN_operator( wn ) == OPR_STID ) {
    WN *val = WN_kid( wn, 0 );
    if ( WN_operator( val ) == OPR_LDID ) {
      ST *st = WN_st( val );
      if ( ST_sym_class( st ) == CLASS_PREG
	   && ( Preg_Is_Dedicated_Outgoing_Ret( WN_offset( val ) )
		|| st == Return_Val_Preg ) )
	return TRUE;
    }
  }
  
  return FALSE;
}


BOOL 
WN_INSTRUMENT_WALKER::Is_Return_Store_Comma( WN *wn )
{
  if ( wn && WN_operator( wn ) == OPR_COMMA ) {
    WN *val = WN_kid( wn, 1 );
    if ( WN_operator( val ) == OPR_LDID ) {
      ST *st = WN_st( val );
      if ( ST_sym_class( st ) == CLASS_PREG
 	   && ( Preg_Is_Dedicated_Incoming_Ret( WN_offset( val ) )
 		|| st == Return_Val_Preg ) )
 	return TRUE;
    }
  }
  
  return FALSE;
}


BOOL
WN_INSTRUMENT_WALKER::Test_Dedicated_Reg( WN *wn )
{
  if ( wn == NULL )
    return FALSE;

  OPERATOR opr = WN_operator( wn );

  if ( opr == OPR_LDID ) {
    ST *st = WN_st( wn );
    if ( ST_sym_class( st ) == CLASS_PREG
	 && Preg_Is_Dedicated( WN_offset( wn ) ) )
      return TRUE;
  }
  
  // traverse the tree starting with this node.
  if ( opr == OPR_BLOCK ) {
    // Special traversal case for BLOCK structure
    WN *node;
    for ( node = WN_first( wn ); node; node = WN_next( node ) )
      if ( Test_Dedicated_Reg( node ) )
	return TRUE;
  }
  else { // Traverse the kids
    for ( INT32 i = 0; i < WN_kid_count( wn ); i++ )
      if ( Test_Dedicated_Reg( WN_kid( wn, i ) ) )
	return TRUE;
  }
  return FALSE;
}


void
WN_INSTRUMENT_WALKER::Instrument_Before( WN *wn, WN *current_stmt,
					 WN *block )
{
#ifndef TARG_XTENSA
  if ( Test_Dedicated_Reg( current_stmt ) ) {
    DevWarn( "Instrumenter Warning: Hardware registers used in "
	     "instrumented node - program may behave differently!" );
    // fdump_tree( TFile, current_stmt );
  }
#endif

  WN *stmt_prev = WN_prev( current_stmt );
  WN_INSERT_BlockAfter( block, stmt_prev, wn );
}


void
WN_INSTRUMENT_WALKER::Instrument_After( WN *wn, WN *current_stmt,
					WN *block )
{
  WN *stmt = WN_next( current_stmt );

  // If at a call, place instrumentation after return args saved.
  // if ( _phase != PROFILE_PHASE_BEFORE_VHO )
  int i = 0;
  if ( OPERATOR_is_call( WN_operator( current_stmt ) ) )
    while ( stmt && Is_Return_Store_Stmt( stmt ) ) {
      if ( WN_rtype( current_stmt ) == MTYPE_V ) {
	DevWarn( "Instrumenter Warning: Should NOT have skipped!" );
	// fdump_tree( TFile, current_stmt );
	// fdump_tree( TFile, stmt );
      }
      i++;
      stmt = WN_next( stmt );
    }

  if ( WN_rtype( current_stmt ) != MTYPE_V && i == 0 ) {
    DevWarn( "Instrumenter Warning: Should have skipped!" );
    // fdump_tree( TFile, current_stmt );
    // fdump_tree( TFile, stmt );
  }
  
  Record_Instrument_Node( wn );

  WN_INSERT_BlockBefore( block, stmt, wn );
}


void
WN_INSTRUMENT_WALKER::Instrument_Entry( WN *wn )
{
  WN_INSERT_BlockBefore( Entry_Block(), Entry_Pragma(), wn );
}


WN *
WN_INSTRUMENT_WALKER::Create_Comma_Kid( WN *wn, INT kid_idx ) {
  WN *wn_comma;
  WN *wn_kid = WN_kid( wn, kid_idx );
  OPERATOR opr_kid = WN_operator( wn_kid );
  if ( opr_kid == OPR_COMMA ) {
    wn_comma = wn_kid;
  } else {
    wn_comma = WN_Create( OPR_COMMA, WN_rtype( wn_kid ), MTYPE_V, 2 );
    WN_kid( wn_comma, 0 ) = WN_CreateBlock();
    WN_kid( wn_comma, 1 ) = wn_kid;
    WN_kid( wn, kid_idx ) = wn_comma;
  }
  _vho_lower = TRUE;
  return wn_comma;
}


// Generate intrinsic call to read CCOUNT, and
// store its value into _cycle_count local variable
void
WN_INSTRUMENT_WALKER::Read_Ccount_Begin( WN *block )
{
  PREG_NUM ccount_preg = Create_Preg(MTYPE_U4, "rsr_ccount_begin");
  WN *ccount_preg_ldid = WN_LdidPreg(MTYPE_U4, ccount_preg);
  WN *rsr_ccount_parm = 
    WN_CreateParm(MTYPE_U4, ccount_preg_ldid, MTYPE_To_TY(MTYPE_U4), 
                  WN_PARM_BY_VALUE);
  WN *rsr_ccount_call = 
    WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, RSR_CCOUNT_Intrinsic, 1, 
                        &rsr_ccount_parm);
  WN_Set_Call_Default_Flags(rsr_ccount_call);
      
  WN *ccount_preg_stid = 
    WN_StidIntoPreg(MTYPE_U4, ccount_preg, MTYPE_To_PREG(MTYPE_U4),
                    WN_Ldid(MTYPE_U4, -1, Tie_Output_Volatile_Preg, 
                            Tie_Output_Volatile_Type));
  WN *cycle_count_stid = 
    WN_Stid(MTYPE_U4, 0, _cycle_count, MTYPE_To_TY(MTYPE_U4),
            WN_LdidPreg(MTYPE_U4, ccount_preg));
                                          
  WN_INSERT_BlockLast(block, rsr_ccount_call);
  WN_INSERT_BlockLast(block, ccount_preg_stid);
  WN_INSERT_BlockLast(block, cycle_count_stid);
}


// Generate intrinsic call to read CCOUNT, and
// increment the PU cycle_count by the difference
// between the current and previous value of ccount
void
WN_INSTRUMENT_WALKER::Read_Ccount_End( WN *block )
{
  PREG_NUM ccount_preg = Create_Preg(MTYPE_U4, "rsr_ccount_end");
  WN *ccount_preg_ldid = WN_LdidPreg(MTYPE_U4, ccount_preg);
  WN *rsr_ccount_parm = 
    WN_CreateParm(MTYPE_U4, ccount_preg_ldid, MTYPE_To_TY(MTYPE_U4), 
                  WN_PARM_BY_VALUE);
  WN *rsr_ccount_call = 
    WN_Create_Intrinsic(OPC_VINTRINSIC_CALL, RSR_CCOUNT_Intrinsic, 1, 
                        &rsr_ccount_parm);
  WN_Set_Call_Default_Flags(rsr_ccount_call);
      
  WN *ccount_preg_stid = 
    WN_StidIntoPreg(MTYPE_U4, ccount_preg, MTYPE_To_PREG(MTYPE_U4),
                    WN_Ldid(MTYPE_U4, -1, Tie_Output_Volatile_Preg, 
                            Tie_Output_Volatile_Type));
  WN *cycle_count_stid = 
    WN_Stid(MTYPE_U4, 0, _cycle_count, MTYPE_To_TY(MTYPE_U4),
            WN_Sub(MTYPE_U4,
                   WN_LdidPreg(MTYPE_U4, ccount_preg),
                   WN_Ldid(MTYPE_U4,0, _cycle_count,MTYPE_To_TY(MTYPE_U4))));

  WN *pu_cycle_count_incr = 
    WN_Istore(MTYPE_U4,
              84, // cycle_count offset in PU_Profile_Handle
              Make_Pointer_Type(MTYPE_To_TY(MTYPE_U4)),
              PU_Handle(),
              WN_Add(MTYPE_U4,
                     WN_Iload(MTYPE_U4,
                              84, // cycle_count offset in PU_Profile_Handle
                              MTYPE_To_TY(MTYPE_U4),
                              PU_Handle()),
                     WN_Ldid(MTYPE_U4,0,_cycle_count,MTYPE_To_TY(MTYPE_U4))));
    
  WN_INSERT_BlockLast(block, rsr_ccount_call);
  WN_INSERT_BlockLast(block, ccount_preg_stid);
  WN_INSERT_BlockLast(block, cycle_count_stid);
  WN_INSERT_BlockLast(block, pu_cycle_count_incr);
}


// ====================================================================
//
// Below are instrumentation and annotation procedures for each type
// of instrumentation:
// -> Invoke
// -> Branch
// -> Loop
// -> Circuit
// -> Call
// -> Return (just record cycle count)
// -> Switch
// -> Compgoto
//
// Three procedures are defined for each type or subtype:
//
// Instrument_*
// Initialize_Instrumenter_*
// Annotate_*
//
// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Invoke( WN *wn, INT32 id, WN *block )
{
  // invoke_table = pu_handle + 0 (offset of Invoke_Profile_Table)
  WN *invoke_table_stid = 
    WN_Stid(Pointer_type, 0, _invoke_table, Ptr_Invoke_Profile_Vector_Type, 
	    WN_Add(Pointer_type, WN_Intconst(Pointer_type, 0), PU_Handle()));
  
  // invoke_info = *(invoke_table + 8) + 4*id
  WN *invoke_info_stid =
    WN_Stid(Pointer_type, 0, _invoke_info, Ptr_Invoke_Profile_Type,
	    WN_Add(Pointer_type,
		   WN_Iload(Pointer_type,
			    8, // offset of _array in DYN_ARRAY
			    Invoke_Profile_Vector_Type,
			    WN_Ldid(Pointer_type, 0, _invoke_table,
				    Ptr_Invoke_Profile_Vector_Type),
			    3), // field_id of _array DYN_ARRAY
		   WN_Intconst(Pointer_type,
			       id * TY_size(Invoke_Profile_Type))));
  
  // invoke_info->invoke_count = invoke_info->invoke_count + 1
  WN *invoke_count_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      0, // offset of invoke_count in Invoke_Profile
	      Ptr_Invoke_Profile_Type,
	      WN_Ldid(Pointer_type,0, _invoke_info, Ptr_Invoke_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      0, // offset of invoke_count in Invoke_Profile
			      Invoke_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _invoke_info, 
				      Ptr_Invoke_Profile_Type),
			      1), // field_id of invoke_count in Invoke_Prof
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      1); // field_id of invoke_count in Invoke_Profile
  
  WN *instr_invoke = WN_CreateBlock();
  WN_INSERT_BlockLast(instr_invoke, invoke_table_stid);
  WN_INSERT_BlockLast(instr_invoke, invoke_info_stid);
  WN_INSERT_BlockLast(instr_invoke, invoke_count_incr);
  
  if (RSR_CCOUNT_Intrinsic != INTRINSIC_NONE)
    Read_Ccount_Begin( instr_invoke );
  
  Instrument_After( instr_invoke, wn, block );
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Invoke( INT32 count )
{
  if ( count == 0 ) return;

  WN *total_invokes = WN_Intconst( MTYPE_I4, count );
  // __profile_invoke_init( handle, total_invokes )
  Instrument_Entry( Gen_Call( INVOKE_INIT_NAME,
			      PU_Handle(), total_invokes ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Invoke( WN *wn, INT32 id )
{
  // Sum profile frequency counts
  PU_PROFILE_HANDLES& handles = FB_Handle();
  FB_Info_Invoke info_invoke( FB_FREQ_ZERO );
  for ( PU_PROFILE_ITERATOR i( handles.begin() );
	i != handles.end (); ++i ) {
    FB_Info_Invoke& info = Get_Invoke_Profile( *i, id );
    info_invoke.freq_invoke += info.freq_invoke;
  }

  // Attach profile information to node.
  Cur_PU_Feedback->Annot_invoke( wn, info_invoke );
}


// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Branch( WN *wn, INT32 id, WN *block )
{
  if (WN_operator(wn) == OPR_IF) {

    // branch_table = pu_handle + 12 (offset of Branch_Profile_Table)
    WN *branch_table_stid = 
      WN_Stid(Pointer_type, 0, _branch_table, Ptr_Branch_Profile_Vector_Type, 
              WN_Add(Pointer_type, WN_Intconst(Pointer_type,12), PU_Handle()));
    
    // branch_info = *(branch_table + 8) + 8*id
    WN *branch_info_stid =
      WN_Stid(Pointer_type, 0, _branch_info, Ptr_Branch_Profile_Type,
              WN_Add(Pointer_type,
                     WN_Iload(Pointer_type,
                              8, // offset of _array in DYN_ARRAY
                              Branch_Profile_Vector_Type,
                              WN_Ldid(Pointer_type, 0, _branch_table,
                                      Ptr_Branch_Profile_Vector_Type),
                              3), // field_id of _array DYN_ARRAY
                     WN_Intconst(Pointer_type,
                                 id * TY_size(Branch_Profile_Type))));

    // branch_info->taken = branch_info->taken + 1
    WN *branch_taken_incr =
      WN_Istore(TY_mtype(Freq_Int_Type),
                0, // offset of taken in Branch_Profile
                Ptr_Branch_Profile_Type,
                WN_Ldid(Pointer_type,0, _branch_info, Ptr_Branch_Profile_Type),
                WN_Add(TY_mtype(Freq_Int_Type),
                       WN_Iload(TY_mtype(Freq_Int_Type),
                                0, // offset of taken in Branch_Profile
                                Branch_Profile_Type,
                                WN_Ldid(Pointer_type, 0, _branch_info, 
                                        Ptr_Branch_Profile_Type),
                                1), // field_id of taken in Branch_Profile
                       WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
                1); // field_id of taken in Branch_Profile
    
    // branch_info->not_taken = branch_info->not_taken + 1
    WN *branch_not_taken_incr =
      WN_Istore(TY_mtype(Freq_Int_Type),
                TY_size(Freq_Int_Type), // offset of not_taken in Branch_Prof
                Ptr_Branch_Profile_Type,
                WN_Ldid(Pointer_type,0, _branch_info, Ptr_Branch_Profile_Type),
                WN_Add(TY_mtype(Freq_Int_Type),
                       WN_Iload(TY_mtype(Freq_Int_Type),
                                TY_size(Freq_Int_Type), // not_taken offset
                                Branch_Profile_Type,
                                WN_Ldid(Pointer_type, 0, _branch_info, 
                                        Ptr_Branch_Profile_Type),
                                2), // field_id of not_taken in Branch_Profile
                       WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
                2); // field_id of not_taken in Branch_Profile

    WN *instr_branch_taken = WN_CreateBlock();
    WN_INSERT_BlockLast(instr_branch_taken, branch_table_stid);
    WN_INSERT_BlockLast(instr_branch_taken, branch_info_stid);
    WN_INSERT_BlockLast(instr_branch_taken, branch_taken_incr);

    Record_Instrument_Node(instr_branch_taken);
    WN_INSERT_BlockFirst(WN_then(wn), instr_branch_taken);

    branch_table_stid = WN_COPY_Tree(branch_table_stid);
    branch_info_stid  = WN_COPY_Tree(branch_info_stid);

    WN *instr_branch_not_taken = WN_CreateBlock();
    WN_INSERT_BlockLast(instr_branch_not_taken, branch_table_stid);
    WN_INSERT_BlockLast(instr_branch_not_taken, branch_info_stid);
    WN_INSERT_BlockLast(instr_branch_not_taken, branch_not_taken_incr);

    Record_Instrument_Node(instr_branch_not_taken);
    WN_INSERT_BlockFirst(WN_else(wn), instr_branch_not_taken);

  }

  else {
    // Compute condition once and save to preg.
    TYPE_ID cond_type = WN_rtype( WN_kid0( wn ) );
    PREG_NUM cond = Create_Preg( cond_type, "__branch_cond" );
    Instrument_Before( WN_StidIntoPreg( cond_type, cond,
                                        MTYPE_To_PREG( cond_type ),
                                        WN_kid0( wn ) ),
                       wn, block );

    // Replace condition by preg
    WN_kid0( wn ) = WN_LdidPreg( cond_type, cond );

    // profile_branch( handle, b_id, condition, taken_if_true );
    OPERATOR opr = WN_operator( wn );
    WN *taken = WN_Relational( ( opr == OPR_FALSEBR ) ? OPR_EQ : OPR_NE,
                               MTYPE_I4, WN_LdidPreg( cond_type, cond ),
                               WN_Intconst( MTYPE_I4, 0 ) );
    WN *instr = Gen_Call( BRANCH_INSTRUMENT_NAME, PU_Handle(),
                          WN_Intconst( MTYPE_I4, id ), taken );
    Instrument_Before( instr, wn, block );
  }
}


void
WN_INSTRUMENT_WALKER::Instrument_Cselect( WN *wn, INT32 id )
{
  // Create comma for kid0
  WN *comma = Create_Comma_Kid( wn, 0 );

  // Compute condition once and save to preg.
  TYPE_ID cond_type = WN_rtype( WN_kid( comma, 1 ) );
  PREG_NUM cond = Create_Preg( cond_type, "__cselect_cond" );
  WN *stid = WN_StidIntoPreg( cond_type, cond, MTYPE_To_PREG( cond_type ),
			      WN_kid( comma, 1 ) );
  WN_INSERT_BlockLast( WN_kid( comma, 0 ), stid );

  // Replace condition by preg
  WN_kid( comma, 1 ) = WN_LdidPreg( cond_type, cond );

  // Insert instrumentation call
  WN *taken;
  if (MTYPE_is_xtbool(cond_type)) {
    taken = WN_Cvt(cond_type, MTYPE_I4, WN_LdidPreg(cond_type, cond));
  } else {
    taken = WN_Relational( OPR_NE, MTYPE_I4,
    	                   WN_LdidPreg( cond_type, cond ),
			   WN_Intconst( MTYPE_I4, 0 ) );
  }

  // branch_table = pu_handle + 12 (offset of Branch_Profile_Table)
  WN *branch_table_stid = 
    WN_Stid(Pointer_type,0, _branch_table, Ptr_Branch_Profile_Vector_Type, 
	    WN_Add(Pointer_type, WN_Intconst(Pointer_type,12), PU_Handle()));
  
  // branch_info = *(branch_table + 8) + 8*id
  WN *branch_info_stid =
    WN_Stid(Pointer_type, 0, _branch_info, Ptr_Branch_Profile_Type,
	    WN_Add(Pointer_type,
		   WN_Iload(Pointer_type,
			    8, // offset of _array in DYN_ARRAY
			    Branch_Profile_Vector_Type,
			    WN_Ldid(Pointer_type, 0, _branch_table,
				    Ptr_Branch_Profile_Vector_Type),
			    3), // field_id of _array DYN_ARRAY
		   WN_Intconst(Pointer_type,
			       id * TY_size(Branch_Profile_Type))));
  
  // branch_info->taken = branch_info->taken + 1
  WN *branch_taken_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      0, // offset of taken in Branch_Profile
	      Ptr_Branch_Profile_Type,
	      WN_Ldid(Pointer_type,0,_branch_info,Ptr_Branch_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      0, // offset of taken in Branch_Profile
			      Branch_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _branch_info, 
				      Ptr_Branch_Profile_Type),
			      1), // field_id of taken in Brnach_Profile
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      1); // field_id of taken in Branch_Profile
  
  // branch_info->not_taken = branch_info->not_taken + 1
  WN *branch_not_taken_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      TY_size(Freq_Int_Type), // not_taken offset in Branch_Profile
	      Ptr_Branch_Profile_Type,
	      WN_Ldid(Pointer_type,0,_branch_info,Ptr_Branch_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      TY_size(Freq_Int_Type), // not_taken offset
			      Branch_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _branch_info, 
				      Ptr_Branch_Profile_Type),
			      2), // field_id of not_taken in Branch_Profile
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      2); // field_id of not_taken in Branch_Profile
  
  WN * instr_branch_taken = WN_CreateBlock();
  WN_INSERT_BlockLast(instr_branch_taken, branch_table_stid);
  WN_INSERT_BlockLast(instr_branch_taken, branch_info_stid);
  WN_INSERT_BlockLast(instr_branch_taken, branch_taken_incr);
  
  branch_table_stid = WN_COPY_Tree(branch_table_stid);
  branch_info_stid  = WN_COPY_Tree(branch_info_stid);
  
  WN * instr_branch_not_taken = WN_CreateBlock();
  WN_INSERT_BlockLast(instr_branch_not_taken, branch_table_stid);
  WN_INSERT_BlockLast(instr_branch_not_taken, branch_info_stid);
  WN_INSERT_BlockLast(instr_branch_not_taken, branch_not_taken_incr);
  
  WN * branch_instr = 
    WN_CreateIf(taken, instr_branch_taken, instr_branch_not_taken);
  
  WN_INSERT_BlockLast(WN_kid(comma, 0), branch_instr);
  
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Branch( INT32 count )
{
  if ( count == 0 ) return;

  // __profile_branch_init(handle, total_branches)
  WN *total_branches = WN_Intconst( MTYPE_I4, count );
  Instrument_Entry( Gen_Call( BRANCH_INIT_NAME,
			      PU_Handle(), total_branches ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Branch(WN *wn, INT32 id)
{
  PU_PROFILE_HANDLES& handles = FB_Handle();
  FB_Info_Branch info_branch( FB_FREQ_ZERO, FB_FREQ_ZERO );
  for (PU_PROFILE_ITERATOR i( handles.begin() ); i != handles.end(); ++i ) {
    FB_Info_Branch& info = Get_Branch_Profile( *i, id );
    info_branch.freq_taken += info.freq_taken;
    info_branch.freq_not_taken += info.freq_not_taken;
  }
  // Attach profile information to node.
  Cur_PU_Feedback->Annot_branch( wn, info_branch );
}


// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Loop( WN *wn, INT32 id, WN *block )
{
  WN *body = ( WN_operator( wn ) == OPR_DO_LOOP
	       ? WN_do_body( wn ) : WN_while_body( wn ) );
  
  // loop_table = pu_handle + 48 (offset of Loop_Profile_Table)
  WN *loop_table_stid = 
    WN_Stid(Pointer_type, 0, _loop_table, Ptr_Loop_Profile_Vector_Type, 
	    WN_Add(Pointer_type, WN_Intconst(Pointer_type,48), PU_Handle()));
  
  // loop_info = *(loop_table + 8) + 16*id
  WN *loop_info_stid =
    WN_Stid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type,
	    WN_Add(Pointer_type,
		   WN_Iload(Pointer_type,
			    8, // offset of _array in DYN_ARRAY
			    Loop_Profile_Vector_Type,
			    WN_Ldid(Pointer_type, 0, _loop_table,
				    Ptr_Loop_Profile_Vector_Type),
			    3), // field_id of _array DYN_ARRAY
		   WN_Intconst(Pointer_type,
			       id * TY_size(Loop_Profile_Type))));
  
  // loop_info->invocation_count > 0
  WN *invocation_count_gt0 =
    WN_GT(TY_mtype(Freq_Int_Type),
	  WN_Iload(TY_mtype(Freq_Int_Type),
		   0, // invocation_count offset
		   Loop_Profile_Type,
		   WN_Ldid(Pointer_type,0,_loop_info,Ptr_Loop_Profile_Type),
		   1), // invocation_count field_id
	  WN_Intconst(TY_mtype(Freq_Int_Type), 0));
  
  // loop_info->last_trip_count == 0
  WN *last_trip_count_eq0 =
    WN_EQ(TY_mtype(Freq_Int_Type),
	  WN_Iload(TY_mtype(Freq_Int_Type),
		   2 * TY_size(Freq_Int_Type), // last_trip_count offset
		   Loop_Profile_Type,
		   WN_Ldid(Pointer_type,0,_loop_info,Ptr_Loop_Profile_Type),
		   3), // last_trip_count field_id
	  WN_Intconst(TY_mtype(Freq_Int_Type), 0));
  
  // loop_info->num_zero_trips = call_info->num_zero_trips + 1
  WN *num_zero_trips_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      3 * TY_size(Freq_Int_Type), // num_zero_trips offset
	      Ptr_Loop_Profile_Type,
	      WN_Ldid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      3 * TY_size(Freq_Int_Type), // num_zero_trips
			      Loop_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _loop_info, 
				      Ptr_Loop_Profile_Type),
			      4), // num_zero_trips field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      4); // field_id of num_zero_trips in Loop_Profile
  
  WN *then_block = WN_CreateBlock();
  WN *else_block = WN_CreateBlock();
  WN_INSERT_BlockLast(then_block, num_zero_trips_incr);
  
  WN *count_zero_trips = 
    WN_CreateIf(WN_CAND(invocation_count_gt0, last_trip_count_eq0),
		then_block, else_block);
  
  // loop_info->invocation_count = call_info->invocation_count + 1
  WN *invocation_count_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      0, // invocation_count offset
	      Ptr_Loop_Profile_Type,
	      WN_Ldid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      0, // invocation_count offset
			      Loop_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _loop_info, 
				      Ptr_Loop_Profile_Type),
			      1), // invocation_count field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      1); // invocation_count field_id
  
  // loop_info->last_trip_count = 0
  WN *last_trip_count_reset =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      2 * TY_size(Freq_Int_Type), // last_trip_count offset
	      Ptr_Loop_Profile_Type,
	      WN_Ldid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type),
	      WN_Intconst(TY_mtype(Freq_Int_Type), 0),
	      3); // last_trip_count field_id
  
  WN *instr_loop = WN_CreateBlock();
  WN_INSERT_BlockLast(instr_loop, loop_table_stid);
  WN_INSERT_BlockLast(instr_loop, loop_info_stid);
  WN_INSERT_BlockLast(instr_loop, count_zero_trips);
  WN_INSERT_BlockLast(instr_loop, invocation_count_incr);
  WN_INSERT_BlockLast(instr_loop, last_trip_count_reset);
  
  // inlined __profile_loop goes before the loop
  Instrument_Before(instr_loop, wn, block);
  
  // inlined __profile_loop_iter goes at the beginning of the loop body
  
  loop_table_stid = WN_COPY_Tree(loop_table_stid);
  loop_info_stid  = WN_COPY_Tree(loop_info_stid);
  
  // loop_info->total_trip_count = call_info->total_trip_count + 1
  WN *total_trip_count_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      TY_size(Freq_Int_Type), // total_trip_count offset
	      Ptr_Loop_Profile_Type,
	      WN_Ldid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      TY_size(Freq_Int_Type), // total_trip_count off
			      Loop_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _loop_info, 
				      Ptr_Loop_Profile_Type),
			      2), // total_trip_count field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      2); // field_id of total_trip_count in Loop_Profile
  
  // loop_info->last_trip_count = call_info->last_trip_count + 1
  WN *last_trip_count_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      2 * TY_size(Freq_Int_Type), // last_trip_count offset
	      Ptr_Loop_Profile_Type,
	      WN_Ldid(Pointer_type, 0, _loop_info, Ptr_Loop_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      2 * TY_size(Freq_Int_Type), // last_trip_count
			      Loop_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _loop_info, 
				      Ptr_Loop_Profile_Type),
			      3), // last_trip_count field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      3); // field_id of last_trip_count in Loop_Profile
  
  WN *instr_loop_iter = WN_CreateBlock();
  WN_INSERT_BlockLast(instr_loop_iter, loop_table_stid);
  WN_INSERT_BlockLast(instr_loop_iter, loop_info_stid);
  WN_INSERT_BlockLast(instr_loop_iter, total_trip_count_incr);
  WN_INSERT_BlockLast(instr_loop_iter, last_trip_count_incr);
  
  Record_Instrument_Node(instr_loop_iter);
  WN_INSERT_BlockFirst(body, instr_loop_iter);
  
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Loop( INT32 count )
{
  if ( count == 0 ) return;

  WN *total_loops = WN_Intconst( MTYPE_I4, count );
  // __profile_loop_init( handle, total_loops )
  Instrument_Entry( Gen_Call( LOOP_INIT_NAME,
				   PU_Handle(), total_loops ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Loop( WN *wn, INT32 id )
{
  PU_PROFILE_HANDLES& handles = FB_Handle();
  FB_Info_Loop info_loop( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO,
			  FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
  for ( PU_PROFILE_ITERATOR i( handles.begin() ); i != handles.end(); ++i ) {
    FB_Info_Loop& info = Get_Loop_Profile ( *i, id ); 
    info_loop.freq_zero += info.freq_zero;
    info_loop.freq_positive += info.freq_positive;
    info_loop.freq_out += info.freq_out;
    info_loop.freq_back += info.freq_back;
    info_loop.freq_exit += info.freq_exit;
    info_loop.freq_iterate += info.freq_iterate;
  }
  // Attach profile information to node.
  Cur_PU_Feedback->Annot_loop( wn, info_loop );
}


// ====================================================================


// WN * 
// SHORT_CIRCUIT_INSTRUMENTER::Instrument_Clause(WN *clause, WN *call)
// {
//   OPERATOR opr = WN_operator(clause);
//   WN *comma;
//
//   if (opr == OPR_COMMA) {
//     comma = clause;
//   } else {
//     comma = WN_Create(OPR_COMMA, WN_rtype(clause), MTYPE_V, 2);
//     WN_kid(comma, 0) = WN_CreateBlock();
//     WN_kid(comma, 1) = clause;
//   }
//
//   WN_INSERT_BlockFirst(WN_kid(comma, 0), call);
//   return comma;
// }


void
WN_INSTRUMENT_WALKER::Instrument_Circuit( WN *wn, INT32 id )
{
  // No need to instrument left branch (kid 0)

  // Create comma for right branch (kid 1)
  WN *comma = Create_Comma_Kid( wn, 1 );

  // Compute condition once and save to preg
  TYPE_ID cond_type = WN_rtype( WN_kid( comma, 1 ) );
  PREG_NUM cond = Create_Preg( cond_type, "__circuit_cond" );
  WN *stid = WN_StidIntoPreg( cond_type, cond, MTYPE_To_PREG( cond_type ),
			      WN_kid( comma, 1 ) );
  WN_INSERT_BlockLast( WN_kid( comma, 0 ), stid );

  // Replace condition in left branch by preg
  WN_kid( comma, 1 ) = WN_LdidPreg( cond_type, cond );

  // Insert instrumentation call
  //   IDEA: Use BRANCH instead of SHORT_CIRCUIT
  OPERATOR opr = WN_operator( wn );
  WN *taken = WN_Relational( opr == OPR_CAND ? OPR_EQ : OPR_NE,
			     MTYPE_I4, WN_LdidPreg( cond_type, cond ),
			     WN_Intconst( MTYPE_I4, 0 ) );
  
  // circuit_table = pu_handle + 60 (offset of Circuit_Profile_Table)
  WN *circuit_table_stid = 
    WN_Stid(Pointer_type,0, _circuit_table, Ptr_Circuit_Profile_Vector_Type, 
	    WN_Add(Pointer_type, WN_Intconst(Pointer_type,60), PU_Handle()));
  
  // circuit_info = *(circuit_table + 8) + 8*id
  WN *circuit_info_stid =
    WN_Stid(Pointer_type, 0, _circuit_info, Ptr_Circuit_Profile_Type,
	    WN_Add(Pointer_type,
		   WN_Iload(Pointer_type,
			    8, // offset of _array in DYN_ARRAY
			    Circuit_Profile_Vector_Type,
			    WN_Ldid(Pointer_type, 0, _circuit_table,
				    Ptr_Circuit_Profile_Vector_Type),
			    3), // field_id of _array DYN_ARRAY
		   WN_Intconst(Pointer_type,
			       id * TY_size(Circuit_Profile_Type))));
  
  // circuit_info->right_taken = circuit_info->right_taken + 1
  WN *circuit_right_taken_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      0, // offset of right_taken in Circuit_Profile
	      Ptr_Circuit_Profile_Type,
	      WN_Ldid(Pointer_type,0,_circuit_info,Ptr_Circuit_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      0, // offset of right_taken in Circuit_Profile
			      Circuit_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _circuit_info, 
				      Ptr_Circuit_Profile_Type),
			      1), // right_taken field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      1); // field_id of right_taken in Circuit_Profile
  
  // circuit_info->neither_taken = circuit_info->neither_taken + 1
  WN *circuit_neither_taken_incr =
    WN_Istore(TY_mtype(Freq_Int_Type),
	      TY_size(Freq_Int_Type), // neither_taken offset in Circuit_Prof
	      Ptr_Circuit_Profile_Type,
	      WN_Ldid(Pointer_type,0,_circuit_info,Ptr_Circuit_Profile_Type),
	      WN_Add(TY_mtype(Freq_Int_Type),
		     WN_Iload(TY_mtype(Freq_Int_Type),
			      TY_size(Freq_Int_Type), // neither_taken offset
			      Circuit_Profile_Type,
			      WN_Ldid(Pointer_type, 0, _circuit_info, 
				      Ptr_Circuit_Profile_Type),
			      2), // neither_taken field_id
		     WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
	      2); // field_id of neither_taken in Circuit_Profile
  
  WN * right_taken = WN_CreateBlock();
  WN_INSERT_BlockLast(right_taken, circuit_table_stid);
  WN_INSERT_BlockLast(right_taken, circuit_info_stid);
  WN_INSERT_BlockLast(right_taken, circuit_right_taken_incr);
  
  circuit_table_stid = WN_COPY_Tree(circuit_table_stid);
  circuit_info_stid  = WN_COPY_Tree(circuit_info_stid);
  
  WN * neither_taken = WN_CreateBlock();
  WN_INSERT_BlockLast(neither_taken, circuit_table_stid);
  WN_INSERT_BlockLast(neither_taken, circuit_info_stid);
  WN_INSERT_BlockLast(neither_taken, circuit_neither_taken_incr);
  
  WN * circuit_instr = WN_CreateIf(taken, right_taken, neither_taken);
  
  WN_INSERT_BlockLast(WN_kid(comma, 0), circuit_instr);
  
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Circuit( INT32 count )
{
  if ( count == 0 ) return;

  WN *total_short_circuits = WN_Intconst( MTYPE_I4, count );
  // __profile_short_circuit_init( handle, total_short_circuits )
  Instrument_Entry( Gen_Call( SHORT_CIRCUIT_INIT_NAME,
				   PU_Handle(), total_short_circuits ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Circuit( WN *wn, INT32 id )
{
  PU_PROFILE_HANDLES& handles = FB_Handle();
  FB_Info_Circuit info_circuit( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
  for (PU_PROFILE_ITERATOR i( handles.begin () ); i != handles.end(); ++i ) {
    FB_Info_Circuit& info = Get_Short_Circuit_Profile( *i, id );
    info_circuit.freq_left += info.freq_left;
    info_circuit.freq_right += info.freq_right;
    info_circuit.freq_neither += info.freq_neither;
  }
  // Attach profile information to node.
  Cur_PU_Feedback->Annot_circuit( wn, info_circuit );
}


// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Call( WN *wn, INT32 id, WN *block )
{
  //due to the differences between how the return value of 
  //a tie intrinsic and a normal call and is lowered.
  //it is better not to instrument tie intrinsics. They end up
  //as single instructions anyway, so feedback data on them is not
  //particularly helpful.
  INTRINSIC tie_id = (INTRINSIC) WN_intrinsic(wn);
  if (!INTRN_is_tie_intrinsic(tie_id)) {
    // Get the name of the called function.
    WN *called_func_name;
    if ( WN_has_sym( wn ) ) {
      char *name = ST_name( WN_st( wn ) );
      called_func_name = WN_LdaString( name, 0, strlen( name ) + 1 );
    } else
      called_func_name = WN_Zerocon( Pointer_type );
    
    // call_table = pu_handle + 72 (offset of Call_Profile_Table)
    WN *call_table_stid = 
      WN_Stid(Pointer_type, 0, _call_table, Ptr_Call_Profile_Vector_Type, 
	      WN_Add(Pointer_type, WN_Intconst(Pointer_type,72), PU_Handle()));
    
    // call_info = *(call_table + 8) + 8*id
    WN *call_info_stid =
      WN_Stid(Pointer_type, 0, _call_info, Ptr_Call_Profile_Type,
	      WN_Add(Pointer_type,
		     WN_Iload(Pointer_type,
			      8, // offset of _array in DYN_ARRAY
			      Call_Profile_Vector_Type,
			      WN_Ldid(Pointer_type, 0, _call_table,
				      Ptr_Call_Profile_Vector_Type),
			      3), // field_id of _array DYN_ARRAY
		     WN_Intconst(Pointer_type,
				 id * TY_size(Call_Profile_Type))));
    
    // call_info->entry_count = call_info->entry_count + 1
    WN *entry_count_incr =
      WN_Istore(TY_mtype(Freq_Int_Type),
		0, // offset of entry_count in Call_Profile
		Ptr_Call_Profile_Type,
		WN_Ldid(Pointer_type, 0, _call_info, Ptr_Call_Profile_Type),
		WN_Add(TY_mtype(Freq_Int_Type),
		       WN_Iload(TY_mtype(Freq_Int_Type),
				0, // offset of entry_count in Call_Profile
				Call_Profile_Type,
				WN_Ldid(Pointer_type, 0, _call_info, 
					Ptr_Call_Profile_Type),
				1), // field_id of entry_count in Call_Profile
		       WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
		1); // field_id of entry_count in Call_Profile
    
    // call_info->exit_count = call_info->exit_count + 1
    WN *exit_count_incr =
      WN_Istore(TY_mtype(Freq_Int_Type),
		TY_size(Freq_Int_Type), // offset of exit_count in Call_Profile
		Ptr_Call_Profile_Type,
		WN_Ldid(Pointer_type, 0, _call_info, Ptr_Call_Profile_Type),
		WN_Add(TY_mtype(Freq_Int_Type),
		       WN_Iload(TY_mtype(Freq_Int_Type),
				TY_size(Freq_Int_Type), // exit_count offset
				Call_Profile_Type,
				WN_Ldid(Pointer_type, 0, _call_info, 
					Ptr_Call_Profile_Type),
				2), // field_id of exit_count in Call_Profile
		       WN_Intconst(TY_mtype(Freq_Int_Type), 1)),
		2); // field_id of exit_count in Call_Profile
    
    WN *instr_call_entry = WN_CreateBlock();
    WN_INSERT_BlockLast(instr_call_entry, call_table_stid);
    WN_INSERT_BlockLast(instr_call_entry, call_info_stid);
    WN_INSERT_BlockLast(instr_call_entry, entry_count_incr);
    
    if (RSR_CCOUNT_Intrinsic != INTRINSIC_NONE && _call_level == 1)
      Read_Ccount_End( instr_call_entry );
    
    Instrument_Before(instr_call_entry, wn, block);
    
    call_table_stid = WN_COPY_Tree(call_table_stid);
    call_info_stid = WN_COPY_Tree(call_info_stid);
    
    WN *instr_call_exit = WN_CreateBlock();
    WN_INSERT_BlockLast(instr_call_exit, call_table_stid);
    WN_INSERT_BlockLast(instr_call_exit, call_info_stid);
    WN_INSERT_BlockLast(instr_call_exit, exit_count_incr);
    
    if (RSR_CCOUNT_Intrinsic != INTRINSIC_NONE && _call_level == 1)
      Read_Ccount_Begin( instr_call_exit );
  
    Instrument_After(instr_call_exit, wn, block);
  }
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Call( INT32 count )
{
  if ( count == 0 ) return;

  WN *total_calls = WN_Intconst( MTYPE_I4, count );
  // __profile_call_init( handle, total_calls )
  Instrument_Entry( Gen_Call( CALL_INIT_NAME,
			      PU_Handle(), total_calls ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Call( WN *wn, INT32 id )
{
  FB_Info_Call info_call( FB_FREQ_ZERO );
  //due to the differences between how the return value of 
  //a tie intrinsic and a normal call and is lowered.
  //it is better not to instrument tie intrinsics. They end up
  //as single instructions anyway, so feedback data on them is not
  //particularly helpful.
  INTRINSIC tie_id = (INTRINSIC) WN_intrinsic(wn);
  if (!INTRN_is_tie_intrinsic(tie_id)) {
    PU_PROFILE_HANDLES& handles = FB_Handle();
    for (PU_PROFILE_ITERATOR i( handles.begin() ); i != handles.end (); ++i) {
      FB_Info_Call& info = Get_Call_Profile( *i, id );
      info_call.freq_entry += info.freq_entry;
      info_call.freq_exit += info.freq_exit;
    }
    
    info_call.in_out_same = ( info_call.freq_entry == info_call.freq_exit );
  }
  else {
    info_call = FB_FREQ_UNINIT;
    info_call.in_out_same = true;
  }
  // Attach profile information to node.
  Cur_PU_Feedback->Annot_call( wn, info_call );
}


// ====================================================================

static TY_IDX
get_field_type (TY_IDX struct_type, UINT field_id)
{
  Is_True (TY_kind (struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (struct_type, field_id, cur_field_id);
  Is_True (! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
			     field_id, struct_type));
  return FLD_type (fld);
}


void
WN_INSTRUMENT_WALKER::Instrument_Return( WN *wn, WN *block )
{
  if (RSR_CCOUNT_Intrinsic != INTRINSIC_NONE) {

    WN *instr_return = WN_CreateBlock();
    Read_Ccount_End( instr_return );

    if (WN_operator(wn) == OPR_RETURN_VAL) {
      TYPE_ID return_mtype = WN_rtype(wn);
      TY_IDX return_ty_idx;
      WN *value = WN_kid0(wn);
      if (return_mtype != MTYPE_M) {
	return_ty_idx = Be_Type_Tbl(return_mtype);
      }
      else {
	OPERATOR opr = WN_operator(value);
	if (opr == OPR_COMMA || opr == OPR_RCOMMA) {
	  value = WN_kid1(value);
	  opr = WN_operator(value);
	}
	return_ty_idx = WN_ty(value);
	if (OPERATOR_is_load(opr) && WN_field_id(value) != 0) {
	  if (opr == OPR_ILOAD || opr == OPR_LDID) 
	    return_ty_idx = get_field_type(return_ty_idx, WN_field_id(value));
	  else {
	    return_ty_idx = TY_pointed(Ty_Table[return_ty_idx]);
	    return_ty_idx = get_field_type(return_ty_idx, WN_field_id(value));
	  }
	}
      }
      ST * return_temp = Gen_Temp_Symbol(return_ty_idx, "_return");
      WN *return_val_stid = 
	WN_Stid(WN_rtype(WN_kid0(wn)), 0, return_temp, return_ty_idx, WN_kid0(wn), 0);

      WN_kid0(wn) = 
	WN_Ldid(WN_rtype(WN_kid0(wn)), 0, return_temp, return_ty_idx, 0);
      
      WN_INSERT_BlockBefore( block, wn, return_val_stid );
    }
    
    WN_INSERT_BlockBefore( block, wn, instr_return );
  }
}


// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Switch( WN *wn, INT32 id, WN *block )
{
  // Record number of targets
  _switch_num_targets.push_back( WN_num_entries( wn ) );

  // Record case values (one for each target)
  for ( WN *wn_casegoto = WN_first( WN_kid1( wn ) );
	wn_casegoto != NULL;
	wn_casegoto = WN_next( wn_casegoto ) ) {
    _switch_case_values.push_back( WN_const_val( wn_casegoto ) );
  }

  // Compute target once and save to preg.
  TYPE_ID cond_type = WN_rtype( WN_kid0( wn ) );
  PREG_NUM cond = Create_Preg( cond_type, "__switch_cond" );

  Instrument_Before( WN_StidIntoPreg( cond_type, cond,
				      MTYPE_To_PREG( cond_type ),
				      WN_kid0( wn ) ),
		     wn, block );
  WN_kid0( wn ) = WN_LdidPreg( cond_type, cond );

  WN *case_value = WN_LdidPreg(cond_type, cond);
  
  // Convert case_value to I8 if necessary 
  if (!MTYPE_is_longlong(cond_type))
    case_value = WN_Cvt(cond_type, MTYPE_I8, case_value);
  
  // profile_switch(handle, switch_id, target, num_targets)
  WN *instr = Gen_Call( SWITCH_INSTRUMENT_NAME, PU_Handle(),
			WN_Intconst( MTYPE_I4, id ),
                        case_value,
		        WN_Intconst( MTYPE_I4, WN_num_entries( wn ) ) );

  Instrument_Before( instr, wn, block );
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Switch( INT32 count, WN *block )
{
  if ( count == 0 ) return;

  // Build switch length array from vector.
  INT32 num_switches = count;
      
  TY_IDX arrayTY = Make_Array_Type( MTYPE_I4, 1, num_switches );
  ST *arrayST = New_ST( CURRENT_SYMTAB );
  ST_Init( arrayST, Save_Str( "switch_num_targets" ),
	   CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, arrayTY );
      
  // This should be only initialized once - instead of every time.
  // How to do this?
  for ( INT32 i = 0; i < num_switches; i++ ) {
    WN *st = WN_Stid(MTYPE_I4, i * MTYPE_RegisterSize( MTYPE_I4 ),
		     arrayST, arrayTY,
		     WN_Intconst( MTYPE_I4, _switch_num_targets[i] ) );
    WN_INSERT_BlockLast(block, st);
  }

  WN *total_switches = WN_Intconst( MTYPE_I4, num_switches );
  WN *switch_num_targets = WN_Lda( Pointer_type, 0, arrayST );


  // Build case value array from vector.
  INT32 num_case_values = _switch_case_values.size();

  arrayTY = Make_Array_Type( MTYPE_I8, 1, num_case_values );
  arrayST = New_ST( CURRENT_SYMTAB );
  ST_Init( arrayST, Save_Str( "switch_case_values" ),
	   CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, arrayTY );
      
  // This should be only initialized once - instead of every time.
  // How to do this?
  for ( INT32 j = 0; j < num_case_values; j++ ) {
    WN *st = WN_Stid( MTYPE_I8, j * MTYPE_RegisterSize( MTYPE_I8 ),
		      arrayST, arrayTY,
		      WN_Intconst( MTYPE_I8, _switch_case_values[j] ) );
    WN_INSERT_BlockLast(block, st);
  }

  WN *total_case_values = WN_Intconst( MTYPE_I4, num_case_values );
  WN *switch_case_values = WN_Lda( Pointer_type, 0, arrayST );

  // __profile_switch_init(handle, total_switches, target_count_array)
  WN *instr = Gen_Call_ref35( SWITCH_INIT_NAME, PU_Handle(),
			      total_switches, switch_num_targets,
			      total_case_values, switch_case_values );
  WN_INSERT_BlockLast(block, instr);
}


static inline void
Handle_Switch_Profile( PU_PROFILE_HANDLES& handles, WN* wn, INT32 id,
		       FB_Info_Switch& (*get_profile) ( PU_PROFILE_HANDLE,
							INT32 ) )
{
  if (handles.empty()) {
    FB_Info_Switch info_switch( WN_num_entries( wn ) + 1, FB_FREQ_ZERO );
    Cur_PU_Feedback->Annot_switch( wn, info_switch );
    return;
  }
  
  FB_Info_Switch& info = (*get_profile) ( handles[0], id );
  if ( handles.size() == 1 ) {
    // Attach profile information to node.
    Cur_PU_Feedback->Annot_switch( wn, info );
  } else {
    FB_Info_Switch info_switch;
    info_switch.freq_targets.insert( info_switch.freq_targets.begin(),
				     info.freq_targets.begin(),
				     info.freq_targets.end() );
    PU_PROFILE_ITERATOR i (handles.begin ());
    for (++i; i != handles.end (); ++i) {
      FB_Info_Switch& info = (*get_profile) (*i, id);
      FmtAssert( info.size () == info_switch.size (),
		 ("Inconsistent profile data from different files"));
      transform( info.freq_targets.begin(),
		 info.freq_targets.end(),
		 info_switch.freq_targets.begin(),
		 info_switch.freq_targets.begin(),
		 plus<FB_FREQ>() );
    }
    Cur_PU_Feedback->Annot_switch( wn, info_switch );
  }
}


void
WN_INSTRUMENT_WALKER::Annotate_Switch( WN *wn, INT32 id )
{
  Handle_Switch_Profile( FB_Handle(), wn, id, Get_Switch_Profile );
}


// ====================================================================


void
WN_INSTRUMENT_WALKER::Instrument_Compgoto( WN *wn, INT32 id, WN *block )
{
  _compgoto_num_targets.push_back( WN_num_entries( wn ) );
  
  // Compute target once and save to preg.
  TYPE_ID cond_type = WN_rtype( WN_kid0( wn ) );
  PREG_NUM cond = Create_Preg( cond_type, "__compgoto_cond" );

  WN *target = WN_StidIntoPreg( cond_type, cond,
				MTYPE_To_PREG( cond_type ),
				WN_kid0( wn ) );
  Instrument_Before( target, wn, block );
  WN_kid0( wn ) = WN_LdidPreg( cond_type, cond );

  // profile_compgoto( handle, compgoto_id, target, num_targets )
  WN *instr = Gen_Call( COMPGOTO_INSTRUMENT_NAME, PU_Handle(),
			WN_Intconst( MTYPE_I4, id ),
			WN_LdidPreg( cond_type, cond ),
			WN_Intconst( MTYPE_I4,
				     WN_num_entries( wn ) ) );
  Instrument_Before( instr, wn, block );
}


void
WN_INSTRUMENT_WALKER::Initialize_Instrumenter_Compgoto( INT32 count )
{
  if ( count == 0 ) return;

  // Build compgoto length array from vector.
  INT32 num_compgotos = count;

  TY_IDX arrayTY = Make_Array_Type( MTYPE_I4, 1, num_compgotos );
  ST *arrayST = New_ST( CURRENT_SYMTAB );
  ST_Init( arrayST, Save_Str( "compgoto_num_targets" ),
	   CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL, arrayTY );

  // This should be only initialized once - instead of every time.
  // How to do this?
  for ( INT32 i = 0; i < num_compgotos; i++ ) {
    WN *st = WN_Stid( MTYPE_I4, i * MTYPE_RegisterSize( MTYPE_I4 ),
		      arrayST, arrayTY, 
		      WN_Intconst( MTYPE_I4, _compgoto_num_targets[i] ) );
    Instrument_Entry( st );
  }

  WN *total_compgotos = WN_Intconst( MTYPE_I4, num_compgotos );
  WN *compgoto_num_targets = WN_Lda( Pointer_type, 0, arrayST );

  // __profile_compgoto_init(handle, total_compgotos, target_count_array)
  Instrument_Entry( Gen_Call_ref3( COMPGOTO_INIT_NAME, PU_Handle(),
				   total_compgotos,
				   compgoto_num_targets ) );
}


void
WN_INSTRUMENT_WALKER::Annotate_Compgoto( WN *wn, INT32 id )
{
  Handle_Switch_Profile( FB_Handle(), wn, id, Get_Compgoto_Profile );
}


// ====================================================================
//
// This is the main driver of the instrumenter. It traverses the tree
// calling appropriate instrumentation procedures when needed.
//
// Tree_Walk_Node must traverse the WHIRL tree in the same order
// during annotation as during instrumentation.
//
// Tree_Walk_Node and Tree_Walk must proceed FORWARD through the
// statements within a BLOCK (so that _instrumentation_nodes only need
// record upcoming WHIRL nodes).
//
// ====================================================================


void
WN_INSTRUMENT_WALKER::Tree_Walk_Node( WN *wn, WN *stmt, WN *block )
{
  OPERATOR opr = WN_operator( wn );

  // Pay special attention to ALT_ENTRY, PRAGMA, and REGION

  // ALT_ENTRY indicates we are entering preamble
  if ( opr == OPR_ALTENTRY ) {
    Is_True( ! _in_preamble, ( "WN_INSTRUMENT_WALKER::Tree_Walk_Node found"
			       " no WN_PRAGMA_PREAMBLE_END pragma" ) );
    _in_preamble = TRUE;
  }

  // WN_PRAGMA_PREAMBLE_END pragma indicates the end of the preamble;
  // Record the PRAGMA for later insertion of instrumentation
  //   initialization code.
  else if ( opr == OPR_PRAGMA
	    && WN_pragma( wn ) == WN_PRAGMA_PREAMBLE_END ) {
    Is_True( _in_preamble, ( "WN_INSTRUMENT_WALKER::Tree_Walk_Node found"
			     " extra WN_PRAGMA_PREAMBLE_END pragma" ) );
    _in_preamble = FALSE;
    Push_Entry_Pragma( wn, block );
  }

  else if ( opr == OPR_REGION ) {

    // Don't instrument exception-handling regions
    if (WN_region_kind(wn) & REGION_KIND_EH)
      return;

    // PREG for _pu_handle must be scoped SHARED within PARALLEL regions
    WN *regn_prag = WN_first( WN_region_pragmas( wn ) );
    if ( regn_prag ) {
      switch ( WN_pragma( regn_prag ) ) {
      case WN_PRAGMA_PARALLEL_BEGIN:
      case WN_PRAGMA_PARALLEL_SECTIONS:
      case WN_PRAGMA_PARALLEL_DO:
      case WN_PRAGMA_DOACROSS:
	{
	  WN *prag = WN_CreatePragma( WN_PRAGMA_SHARED, _pu_handle, 0, 0 );
	  WN_set_pragma_compiler_generated( prag );
	  WN_INSERT_BlockLast( WN_region_pragmas ( wn ), prag );
	}
	break;
      default:
	break;  // not a PARALLEL region
      }
    }
  }

  // Do not instrument or annotate code within the preamble
  // Skip over any nodes added by instrumentation
  if ( _in_preamble || Test_Instrument_Node( wn ) )
    return;

  // Count call nesting
  if ( opr == OPR_CALL || opr == OPR_ICALL || opr == OPR_INTRINSIC_CALL)
    _call_level++;

  // Traverse WHIRL subtree.
  if ( opr == OPR_BLOCK ) {

    // Special traversal case for BLOCK structure
    WN *node;
    for ( node = WN_first( wn ); node; node = WN_next( node ) )
      Tree_Walk_Node( node, node, wn );

    Is_True( ! _in_preamble, ( "WN_INSTRUMENT_WALKER::Tree_Walk_Node found"
			       " no WN_PRAGMA_PREAMBLE_END pragma" ) );

  } else if ( OPERATOR_is_expression( opr ) ) {

    // Watch out for special case:
    if ( Is_Return_Store_Comma( wn ) ) {

      // Handle COMMA holding CALL with return value --- convert:
      //   BLOCK                   --->      BLOCK
      //    ... PARAMs ...         --->       ... PARAMs ...
      //   CALL                    --->      CALL
      //                           --->       LDID preg_return_val
      //                           --->      STID temp
      //   END_BLOCK               --->      END_BLOCK
      //   LDID preg_return_val    --->     LDID temp
      //  COMMA                    --->     COMMA
      // This convertion allow the CALL to be instrumented correctly.

      // Store return value into preg
      TYPE_ID val_type = WN_rtype( WN_kid( wn, 1 ) );
      PREG_NUM val = Create_Preg( val_type, "__call_comma" );
      WN *stid = WN_StidIntoPreg( val_type, val, MTYPE_To_PREG( val_type ),
				  WN_kid( wn, 1 ) );
      WN_INSERT_BlockLast( WN_kid( wn, 0 ), stid );
      
      // Comma now returns value of preg
      WN_kid( wn, 1 ) = WN_LdidPreg( val_type, val );
    }

    // Traverse the kids of the current expression
    for ( INT32 i = 0; i < WN_kid_count( wn ); i++ )
      Tree_Walk_Node( WN_kid( wn, i ), stmt, block );

  } else {

    // Traverse the kids of the current statement
    for ( INT32 i = 0; i < WN_kid_count( wn ); i++ )
      Tree_Walk_Node( WN_kid( wn, i ), wn, block );
  }

  // Perform the instrumentation or annotation of the current node
  switch ( opr ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    {
      _instrument_count++;
      INT32 id = _count_invoke++;
      if ( _instrumenting )
	Instrument_Invoke( wn, id, block );
      else
	Annotate_Invoke( wn, id );
    }
    break;

  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPR_IF:
    {
      _instrument_count++;
      INT32 id = _count_branch++;
      if ( _instrumenting )
	Instrument_Branch( wn, id, block );
      else
	Annotate_Branch( wn, id );
    }
    break;

  case OPR_CSELECT:
    {
      _instrument_count++;
      INT32 id = _count_branch++;
      if ( _instrumenting )
	Instrument_Cselect( wn, id );
      else
	Annotate_Branch( wn, id );
    }
    break;

  case OPR_DO_LOOP:
  case OPR_WHILE_DO:
  case OPR_DO_WHILE:
    {
      _instrument_count++;
      INT32 id = _count_loop++;
      if ( _instrumenting )
	Instrument_Loop( wn, id, block );
      else
	Annotate_Loop( wn, id );
    }
    break;

  case OPR_CAND:
  case OPR_CIOR:
    {
      _instrument_count++;
      INT32 id = _count_circuit++;
      if ( _instrumenting )
	Instrument_Circuit( wn, id );
      else
	Annotate_Circuit( wn, id );
    }
    break;

  case OPR_PICCALL:
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_IO:
    {
      _instrument_count++;
      INT32 id = _count_call++;
      if ( _instrumenting )
	Instrument_Call( wn, id, block );
      else
	Annotate_Call( wn, id );
    }
    break;

  case OPR_RETURN:
  case OPR_RETURN_VAL:
    {
      if ( _instrumenting )
	Instrument_Return( wn, block );
    }
    break;

  case OPR_SWITCH:
    {
      _instrument_count++;
      INT32 id = _count_switch++;
      if ( _instrumenting )
	Instrument_Switch( wn, id, block );
      else
	Annotate_Switch( wn, id );
    }
    break;

  case OPR_COMPGOTO:
  case OPR_XGOTO:
    {
      _instrument_count++;
      INT32 id = _count_compgoto++;
      if ( _instrumenting )
	Instrument_Compgoto( wn, id, block );
      else
	Annotate_Compgoto( wn, id );
    }
    break;
  }

  // This shouldn't be necessary, but....
  if ( opr == OPR_REGION ) {
    if ( _vho_lower ) {
      WN_region_body( wn ) = VHO_Lower( WN_region_body( wn ) );
      _vho_lower = FALSE;
    }
  }

  // Leaving this call
  if ( opr == OPR_CALL || opr == OPR_ICALL || opr == OPR_INTRINSIC_CALL)
    _call_level--;
}


void
WN_INSTRUMENT_WALKER::Tree_Walk( WN *root ) 
{
  // Root must be a func entry!!
  Is_True( WN_operator( root ) == OPR_FUNC_ENTRY,
	   ( "WN_INSTRUMENT_WALKER::Tree_Walk:"
	     " OPR_FUNC_ENTRY expected at top of PU tree." ) );
  // Cur_PU_Feedback should not be NULL if annotating
  Is_True( _instrumenting || Cur_PU_Feedback,
	   ( "WN_INSTRUMENT_WALKER::Tree_Walk:"
	     " NULL Cur_PU_Feedbackduring annotation" ) );

  // Don't instrument twice!
  // Can we come up with an Is_True to check for multiple instrumentation?
  // Is_True( ! _entry_is_instrumented,
  //	   ( "Instrumenter Error: Whirl tree already instrumented "
  //	     "(initialization done)!" ) );

#if Instrumenter_DEBUG
  fdump_tree( TFile, root );
#endif

  // Instrument all statements after (and including) the preamble end;
  // Do not instrument statements in the preamble
  _in_preamble = TRUE;  // will be set FALSE at WN_PRAGMA_PREAMBLE_END
  WN* body = WN_func_body( root );
  WN* stmt;
  for ( stmt = WN_first( body ); stmt; stmt = WN_next( stmt ) )
    Tree_Walk_Node( stmt, stmt, body );
  Is_True( ! _in_preamble, ( "WN_INSTRUMENT_WALKER::Tree_Walk found"
			     " no WN_PRAGMA_PREAMBLE_END pragma" ) );

  // Is there any instrumentation to be initialized?
  if ( _instrumenting && _instrument_count > 0 ) {
    unsigned int instr_flags = Calc_Feedback_Flags();
    // Add initialization to each entry point.
    while ( ! Entry_List_Empty() ) {

      // Pass output file name to __profile_pu_init_new, which
      // will call __profile_init exactly once.
      // phasenum is always 0 and unique_output is always TRUE
      WN *output_file_name = 
        WN_LdaString( Instrumentation_File_Name, 0,
                      strlen( Instrumentation_File_Name ) + 1 );
	  
      // Initialize the PU instrumentation.
      WN *src_file_name = WN_LdaString ( Src_File_Name, 0,
					 strlen( Src_File_Name ) + 1 );
      WN *pu_name = WN_LdaString ( Cur_PU_Name, 0, strlen( Cur_PU_Name ) + 1 );
      WN *pc = WN_Lda( Pointer_type, 0, WN_st( root ) );
      WN *checksum = WN_Intconst( MTYPE_I4, _instrument_count );

      WN * total_invokes =   WN_Intconst( MTYPE_I4, _count_invoke );
      WN * total_branches =  WN_Intconst( MTYPE_I4, _count_branch );
      WN * total_loops =     WN_Intconst( MTYPE_I4, _count_loop );
      WN * total_circuits =  WN_Intconst( MTYPE_I4, _count_circuit );
      WN * total_calls =     WN_Intconst( MTYPE_I4, _count_call );
      WN * iflags =          WN_Intconst( MTYPE_I4, instr_flags );

      const char * call_name = 
	Instrumentation_Bits == 32 ?  
	"__profile_pu_init_new_32" :
	"__profile_pu_init_new_64";
	

      WN * pu_init_call = Gen_Call( (char *) call_name, 
                                    src_file_name,
                                    pu_name, 
                                    pc, 
                                    checksum, 
                                    total_invokes, 
                                    total_branches,
                                    total_loops, 
                                    total_circuits,
                                    total_calls,
				    output_file_name,
				    iflags,
                                    Pointer_type );
      
      // Get current handle.
      WN * ldid_return_preg = 
        WN_Ldid(Pointer_type, -1, Return_Val_Preg, MTYPE_To_TY(Pointer_type));

      WN * pu_init_ret = WN_Stid( Pointer_type, 0, _pu_handle,
                                  MTYPE_To_TY( Pointer_type ), 
                                  ldid_return_preg );

      WN * else_block = WN_CreateBlock(); // empty
      WN * then_block = WN_CreateBlock();
    
      ST * pu_handle_inited = New_ST( CURRENT_SYMTAB );
      ST_Init( pu_handle_inited, 
               Save_Str( "pu_handle_inited" ),
               CLASS_VAR, 
               SCLASS_PSTATIC, 
               EXPORT_LOCAL,
               MTYPE_To_TY( MTYPE_I4 ) );
      WN * stid_inited = WN_Stid( MTYPE_I4, 0, pu_handle_inited, 
                                  MTYPE_To_TY( MTYPE_I4 ),
                                  WN_Intconst( MTYPE_I4, 1 ) );

      WN_INSERT_BlockLast( then_block, stid_inited );
      WN_INSERT_BlockLast( then_block, pu_init_call );
      WN_INSERT_BlockLast( then_block, pu_init_ret );

      WN * ldid_inited = WN_Ldid( MTYPE_I4, 0, pu_handle_inited, 
                                  MTYPE_To_TY( MTYPE_I4 ) );
    
      WN * cond = WN_EQ( MTYPE_I4, ldid_inited, WN_Intconst( MTYPE_I4, 0 ) );

      WN * conditional_pu_init = WN_CreateIf( cond, then_block, else_block );
    
      Instrument_Entry( conditional_pu_init );

      // Initialize specific instrumentation.
      Initialize_Instrumenter_Switch( _count_switch, then_block );
      Initialize_Instrumenter_Compgoto( _count_compgoto );

      Pop_Entry_Pragma();
    }

    // Perform additional lowering - if necessary.
    if ( _vho_lower ) {
      WN_func_body( root ) = VHO_Lower( WN_func_body( root ) );
      _vho_lower = FALSE;
    }

  } else if ( ! _instrumenting ) { // feedback

    // Compare checksums!
    for ( PU_PROFILE_ITERATOR i( _fb_handle.begin() );
	  i != _fb_handle.end(); ++i ) {
	  
      UINT32 checksum = Get_PU_Checksum( *i );
	
      if ( _instrument_count != checksum )
	Fatal_Error( "Instrumenter Error: Feedback file has invalid "
		     "checksum for program unit %s in file %s "
		     "The instrumented code has likely changed.",
		     Cur_PU_Name, Src_File_Name ) ;

      if ( Calc_Feedback_Flags() != Get_PU_Optimization_Level( *i )) {
	ErrMsg(EC_FB_File_Options, Src_File_Name);
      }

    }
  }

#if Instrumenter_DEBUG
  fdump_tree(TFile, root);
#endif
}


static void
Annotate_Cycle_Count( PU_PROFILE_HANDLES& handles )
{
  // Sum profile cycle counts
  FB_FREQ cycle_count( FB_FREQ_ZERO );
  for ( PU_PROFILE_ITERATOR i( handles.begin() ); i != handles.end (); ++i )
    cycle_count += FB_FREQ( (*i)->cycle_count );

  // Attach profile information to feedback handle
  Cur_PU_Feedback->Annot_cycle_count( cycle_count );

  if (!Run_ipl && cycle_count.Known() && !Total_Cycle_Count.Zero()) {
    FB_FREQ cycle_contrib = cycle_count / Total_Cycle_Count;
    if (Get_Trace(TP_FEEDBACK, TP_FEEDBACK_WN))
      fprintf(TFile, "%s contribution: %g / 1000\n",
              Cur_PU_Name, cycle_contrib.Value() * 1000);
    if (cycle_contrib.Value() * 1000 < OPT_Space_Threshold) {
      Set_PU_optimize_for_space(Get_Current_PU());
      if (Get_Trace(TP_FEEDBACK, TP_FEEDBACK_WN))
        fprintf(TFile, "  should be optimized for space\n");
    }
  }
}


/* This is a list of library functions that the instrumentation 
   library calls. If a user overrides them with feedback enabled,
   bad things happen. */

#define NUM_UI_FUNCS 16

const char * uninstrumentable_funcs[NUM_UI_FUNCS] = 
  {
    "atexit",
    "close",
    "delete",
    "exit",
    "fclose",
    "fflush",
    "fopen",
    "fprintf",
    "fputc",
    "free",
    "fseek",
    "fwrite",
    "malloc",
    "new",
    "open",
    "realloc"
  };


static int strcmp_v(const void * s1, const void * s2)
{
  return strcmp(*(const char **)s1, *(const char **)s2);
}

extern "C" BOOL Is_Uninstrumentable_Function(const char * pu)
{
  return bsearch (&pu, uninstrumentable_funcs, NUM_UI_FUNCS,
		  sizeof (const char *), strcmp_v) != NULL;
}


// ====================================================================
//
// Interface to the rest of the compiler backend -- see fuller
// descriptions in the header file wn_instrument.h.
//
// WN_Instrument performs feedback instrumentation on the WHIRL tree
// rooted at wn.
//
// WN_Annotate reads frequency counts from one or more previously
// generated feedback data files and stores the feedback data into
// the FEEDBACK object Cur_PU_Feedback.
//
// Set_Instrumentation_File_Name records the prefix for the names of
// the feedback data files.
//
// ====================================================================


void
WN_Instrument( WN *wn, PROFILE_PHASE phase )
{
  Set_Error_Phase( "WN_Instrument" );
  if ( Instrumenter_DEBUG )
    DevWarn( "WN_Instrument, phase == %d", phase );

  ST * pu = Get_Current_PU_ST();
  if (Is_Uninstrumentable_Function(ST_name(pu))) {
    ErrMsg(EC_FB_Cant_Instrument, ST_name(pu));
    return;
  }

  // Create and initialize local memory pool
  MEM_POOL local_mempool;
  MEM_POOL_Initialize( &local_mempool, "WN_INSTRUMENT_WALKER_Pool", FALSE );
  MEM_POOL_Push( &local_mempool );
  {
    // Walk the WHIRL tree -- instrument
    PU_PROFILE_HANDLES fb_handles; // empty for instrumentation
    WN_INSTRUMENT_WALKER wiw( TRUE, phase, &local_mempool, fb_handles );
    wiw.Tree_Walk( wn );
  }
  // Dispose of local memory pool
  MEM_POOL_Pop( &local_mempool );
  MEM_POOL_Delete( &local_mempool );
}


void
WN_Annotate( WN *wn, PROFILE_PHASE phase, MEM_POOL *MEM_pu_pool )
{
  Set_Error_Phase( "WN_Annotate" );
  if ( Instrumenter_DEBUG )
    DevWarn( "WN_Annotate, phase == %d", phase );

  // Prepare Cur_PU_Feedback, allocating a new FEEDBACK object if
  // necessary.  Note that feedback info might not be always available
  // (e.g., function never called)
  PU_PROFILE_HANDLES fb_handles
    = Get_PU_Profile( Cur_PU_Name, Src_File_Name,
		      Feedback_File_Info[phase] );

  //MEM_POOL_Pop will deallocate it for us
  if ( fb_handles.empty() ) {
    Cur_PU_Feedback = NULL;
    Set_PU_optimize_for_space(Get_Current_PU());
    if (Get_Trace(TP_FEEDBACK, TP_FEEDBACK_WN))
        fprintf(TFile, "  should be optimized for space\n");
    return;
  } 

  if ( Cur_PU_Feedback == NULL )
    Cur_PU_Feedback = CXX_NEW( FEEDBACK( wn, MEM_pu_pool ), MEM_pu_pool );

  Annotate_Cycle_Count(fb_handles);

  // Create and initialize local memory pool
  MEM_POOL local_mempool;
  MEM_POOL_Initialize( &local_mempool, "WN_INSTRUMENT_WALKER_Pool", FALSE );
  MEM_POOL_Push( &local_mempool );
  {
    // Walk the WHIRL tree -- annotate
    WN_INSTRUMENT_WALKER wiw( FALSE, phase, &local_mempool, fb_handles );
    wiw.Tree_Walk( wn );
  }
  // Dispose of local memory pool
  MEM_POOL_Pop( &local_mempool );
  MEM_POOL_Delete( &local_mempool );

  Cur_PU_Feedback->Verify("after annotation");
}


void
Set_Instrumentation_File_Name( char *fname ) 
{
  if ( fname ) {
    Instrumentation_File_Name
      = (char *) malloc( sizeof( char ) *
			 ( strlen(fname)
			   + Instrumentation_Phase_Num / 10 + 2 ) );
    sprintf( Instrumentation_File_Name, "%s%d", fname,
	     Instrumentation_Phase_Num );
    // Instrumentation_File_Name = fname;
  } else {
    DevWarn( "Instrumenter Warning: Invalid instrumentation file name." );
    Instrumentation_File_Name = "";
  }
}


// ====================================================================

