
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
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
// ====================================================================
// ====================================================================
//
// Module: fb_whirl.cxx
// $Revision: 1.81 $
// $Date: 2000/05/19 20:19:35 $
// $Author: murthy $
// $Source: /isms/cmplrs.src/osprey1.0/be/com/RCS/fb_whirl.cxx,v $
//
// Description:
//
// Code to read feedback file and annotate WHIRL tree with the
// frequency information found there.
//
// Revision history:
// 960321 Initial Revision (rkennedy)
// 960425 Major changes for new .cfb format with column
//	  numbers (rkennedy)
// 980228  if be -tt16:4 then call dV_show_whirl() after
//            feedback info is added (gwe).
// 9803    Initial hooks for new (edge) feedback.
// 9804    More work on new feedback; esp. annotation/verification.
// 9806    Through May and June restructure to include support for
//         FEEDBACK during CG.
// ======================================================================
// ======================================================================

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#define USE_STANDARD_TYPES

#include "defs.h"
#include <cmplrs/fb.h>
#include <stdlib.h>
#include "wn_util.h"
#include "wn_map.h"
#include "glob.h"		// for Cur_PU_Name, Feedback_File_Name
#include "errors.h"		// for ErrMsg
#include "erglob.h"		// for EC_FB_File_Old
#include "ir_reader.h"
#include "tracing.h"
#include "wn_tree_util.h"	// for tree iterators
#include "pu_info.h"

#include "fb_whirl.h"
#include "fb_cfg.h"

#include "cxx_graph.h"

#include "wb_util.h"      // more: move this to another file (gwe).

// ====================================================================

FEEDBACK *Cur_PU_Feedback = NULL;
void *Cur_PU_Profile = NULL;

FEEDBACK::FEEDBACK( WN *wn, MEM_POOL *m,
		    INT32 invoke_size,
		    INT32 branch_size,
		    INT32 loop_size,
		    INT32 circuit_size,
		    INT32 call_size,
		    INT32 switch_size, 
		    WN_MAP_TAB *maptab ) :
  _m( m ),
  _maptab( maptab ),
  _root_wn( wn ),
  _trace(      Get_Trace( TP_FEEDBACK, TP_FEEDBACK_WN ) ),
  // note: skiping first entry so the 0 index yields FB_FREQ_UNINIT.
  _invokes ( 1, FB_Info_Invoke(),  m ),
  _branches( 1, FB_Info_Branch(),  m ),
  _loops   ( 1, FB_Info_Loop(),    m ),
  _circuits( 1, FB_Info_Circuit(), m ),
  _calls   ( 1, FB_Info_Call(),    m ),
  _switches( 1, FB_Info_Switch(),  m )
{
  if ( _trace )
    fprintf( TFile, "==================================================\n"
	     "Constructing FEEDBACK for " );

  _invokes.reserve (invoke_size);
  _branches.reserve (branch_size);
  _loops.reserve (loop_size);
  _circuits.reserve (circuit_size);
  _calls.reserve (call_size);
  _switches.reserve (switch_size);
  
  OPERATOR opr = WN_operator( wn );

  FmtAssert( opr == OPR_FUNC_ENTRY || opr == OPR_REGION,
	     ( "FEEDBACK constructor: unexpected opr %d", opr ) );

  if ( _trace ) {
    if ( opr == OPR_FUNC_ENTRY )
      fprintf( TFile, "FUNC_ENTRY %s", ST_name( WN_entry_name( wn ) ) );
    else
      fprintf( TFile, "REGION" );
    fprintf( TFile, "\n==================================================\n" );
  }
}

// ====================================================================
// Get_index_* functions retrieve index into vectors of feedback data
// ====================================================================

INT32
FEEDBACK::Get_index_invoke( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_invoke expects non-NULL wn" ) );
  Is_True( FB_valid_opr_invoke( wn ),
	   ( "FEEDBACK::Get_index_invoke found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _invokes.size(),
	   ( "FEEDBACK::Get_index_invoke found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_branch( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_branch expects non-NULL wn" ) );
  Is_True( FB_valid_opr_branch( wn ),
	   ( "FEEDBACK::Get_index_branch found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _branches.size(),
	   ( "FEEDBACK::Get_index_branch found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_loop( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_loop expects non-NULL wn" ) );
  Is_True( FB_valid_opr_loop( wn ),
	   ( "FEEDBACK::Get_index_loop found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _loops.size(),
	  ( "FEEDBACK::Get_index_loop found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_circuit( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_circuit expects non-NULL wn" ) );
  Is_True( FB_valid_opr_circuit( wn ),
	   ( "FEEDBACK::Get_index_circuit found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _circuits.size(),
	  ( "FEEDBACK::Get_index_circuit found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_call( const WN *wn ) const
{
  Is_True( wn != NULL,
	   ( "FEEDBACK::Get_index_call expects non-NULL wn" ) );
  Is_True( FB_valid_opr_call( wn ),
	   ( "FEEDBACK::Get_index_call found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _calls.size(),
	   ( "FEEDBACK::Get_index_call found out of range fb_index" ) );
  return fb_index;
}

INT32
FEEDBACK::Get_index_switch( const WN *wn ) const
{
  Is_True( wn != NULL,
	  ( "FEEDBACK::Get_index_switch expects non-NULL wn" ) );
  Is_True( FB_valid_opr_switch( wn ),
	  ( "FEEDBACK::Get_index_switch found unexpected operator" ) );

  INT32 fb_index = IPA_WN_MAP32_Get( _maptab, WN_MAP_FEEDBACK, wn );
  Is_True( fb_index >= 0 && fb_index < _switches.size(),
	   ( "FEEDBACK::Get_index_switch found out of range fb_index" ) );
  return fb_index;
}

// ====================================================================
// Add_index_* functions retrieve index into vectors of feedback data,
//   adding a new slot for feedback data if necessary
// ====================================================================

INT32
FEEDBACK::Add_index_invoke( WN *wn )
{
  INT32 fb_index = Get_index_invoke( wn );
  Is_True( fb_index >= 0 && fb_index < _invokes.size(),
	   ( "FEEDBACK::Add_index_invoke found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _invokes.size();
    _invokes.push_back(FB_Info_Invoke());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_branch( WN *wn )
{
  INT32 fb_index = Get_index_branch( wn );
  Is_True( fb_index >= 0 && fb_index < _branches.size(),
	  ( "FEEDBACK::Add_index_branch found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _branches.size();
    _branches.push_back(FB_Info_Branch());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_loop( WN *wn )
{
  INT32 fb_index = Get_index_loop( wn );
  Is_True( fb_index >= 0 && fb_index < _loops.size(),
	  ( "FEEDBACK::Add_index_loop found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _loops.size();
    _loops.push_back(FB_Info_Loop());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_circuit( WN *wn )
{
  INT32 fb_index = Get_index_circuit( wn );
  Is_True( fb_index >= 0 && fb_index < _circuits.size(),
	  ( "FEEDBACK::Add_index_circuit found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _circuits.size();
    _circuits.push_back(FB_Info_Circuit());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_call( WN *wn )
{
  INT32 fb_index = Get_index_call( wn );
  Is_True( fb_index >= 0 && fb_index < _calls.size(),
	   ( "FEEDBACK::Add_index_call found out of range fb_index" ) );

  if (fb_index == 0) {
    fb_index = _calls.size();
    _calls.push_back(FB_Info_Call());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

INT32
FEEDBACK::Add_index_switch( WN *wn )
{
  INT32 fb_index = Get_index_switch( wn );
  Is_True( fb_index >= 0 && fb_index < _switches.size(),
	   ( "FEEDBACK::Add_index_switch found out of range fb_index" ) );

  if ( fb_index == 0 ) {
    fb_index = _switches.size();
    _switches.push_back(FB_Info_Switch());
    IPA_WN_MAP32_Set( _maptab, WN_MAP_FEEDBACK, wn, fb_index );
  }
  return fb_index;
}

// ====================================================================
// Are the total incoming and outgoing frequencies known to be equal?
// ====================================================================

bool
FEEDBACK::Same_in_out( const WN *wn ) {
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {
  case OPR_FUNC_ENTRY: case OPR_ALTENTRY:
  case OPR_RETURN: case OPR_RETURN_VAL:
    return false;

  fb_opr_cases_call:
    {
      INT32 fb_index = Get_index_call( wn );
      return _calls[fb_index].in_out_same;
    }

  default:
    break;
  }
  return true;
}


void
FEEDBACK::FB_set_in_out_same_node( WN *wn )
{
  // Set in_out_same true for all calls
  if ( FB_valid_opr_call( wn ) ) {

    if ( _trace )
      fprintf( TFile, "FEEDBACK::FB_set_in_out_same_node(0x%lx):\n", wn );

    FB_Info_Call info_call = Query_call( wn );
    info_call.in_out_same = true;
    Annot_call( wn, info_call );
  }
}


void
FEEDBACK::FB_set_in_out_same( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_in_out_same(0x%lx):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter )
    FB_set_in_out_same_node( iter.Wn() );
}


// ====================================================================
// Print function displays feedback info for tracing purposes
// ====================================================================

void
FEEDBACK::Print( FILE *fp, const WN *wn ) const
{
  if ( wn == NULL ) return;
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  switch ( opr ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    fb_index = Get_index_invoke( wn );
    _invokes[fb_index].Print( fp );
    break;

  fb_opr_cases_branch:
    fb_index = Get_index_branch( wn );
    _branches[fb_index].Print( fp );
    break;

  fb_opr_cases_loop:
    fb_index = Get_index_loop( wn );
    _loops[fb_index].Print( fp );
    break;

  fb_opr_cases_circuit:
    fb_index = Get_index_circuit( wn );
    _circuits[fb_index].Print( fp );
    break;

  fb_opr_cases_call:
    fb_index = Get_index_call( wn );
    _calls[fb_index].Print( fp );
    break;

  fb_opr_cases_switch:
    fb_index = Get_index_switch( wn );
    _switches[fb_index].Print( fp );
    break;

  default:
    break;
  }
}

void
FEEDBACK::Print_with_wn( FILE *fp, WN *wn ) const
{
  Print( fp, wn );
  fprintf( fp, "\nfor " );
  fdump_wn( TFile, wn );
}

// ====================================================================
// Query_* functions retrieve feedback data or FB_FREQ_UNINIT
// ====================================================================

const FB_Info_Invoke
FEEDBACK::Query_invoke( const WN *wn ) const
{
  INT32 fb_index = Get_index_invoke( wn );
  return _invokes[fb_index];
}

const FB_Info_Branch
FEEDBACK::Query_branch( const WN *wn ) const
{
  INT32 fb_index = Get_index_branch( wn );
  return _branches[fb_index];
}

const FB_Info_Loop
FEEDBACK::Query_loop( const WN *wn ) const
{
  INT32 fb_index = Get_index_loop( wn );
  return _loops[fb_index];
}

const FB_Info_Circuit
FEEDBACK::Query_circuit( const WN *wn ) const
{
  INT32 fb_index = Get_index_circuit( wn );
  return _circuits[fb_index];
}

const FB_Info_Call
FEEDBACK::Query_call( const WN *wn ) const
{
  INT32 fb_index = Get_index_call( wn );
  return _calls[fb_index];
}

const FB_Info_Switch
FEEDBACK::Query_switch( const WN *wn ) const
{
  INT32 fb_index = Get_index_switch( wn );
  return _switches[fb_index];
}


FB_FREQ
FEEDBACK::Query( const WN *wn, const FB_EDGE_TYPE type ) const
{
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT;

  switch( type ) {

  case FB_EDGE_UNINIT:
    Is_True( false, ( "FEEDBACK::Query found edge type FB_EDGE_UNINT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Get_index_invoke( wn );
    freq     = _invokes[fb_index].freq_invoke;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_taken;
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_not_taken;
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_zero;
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_positive;
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_out;
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_back;
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_left;
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_right;
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_neither;
    break;

  case FB_EDGE_CALL_INCOMING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_entry;
    break;

  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_IO_OUTGOING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_exit;
    break;

  case FB_EDGE_CALL_INOUTSAME:
    fb_index = Get_index_call( wn );
    Is_True( _calls[fb_index].in_out_same,
	     ( "FEEDBACK::Query found in_out_same discrepancy" ) );
    freq     = _calls[fb_index].freq_exit;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index][0];
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Get_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      freq = _switches[fb_index][index_target];

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      fb_index = Get_index_call( wn );
      freq = ( _calls[fb_index].in_out_same ? FB_FREQ_ZERO : FB_FREQ_UNKNOWN );

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Query(0x%lx, %s) == ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
  }
  return freq;
}


FB_FREQ
FEEDBACK::Query_prob( const WN *wn, const FB_EDGE_TYPE type ) const
{
  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT, total = FB_FREQ_UNINIT;

  switch( type ) {

  case FB_EDGE_UNINIT:
    Is_True( false,
	     ( "FEEDBACK::Query_prob found edge type FB_EDGE_UNINT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Get_index_invoke( wn );
    freq     = _invokes[fb_index].freq_invoke;
    total    = freq;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_taken;
    total    = _branches[fb_index].Total();
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].freq_not_taken;
    total    = _branches[fb_index].Total();
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_zero;
    total    = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_positive;
    total    = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_out;
    total    = _loops[fb_index].freq_exit;
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_back;
    total    = _loops[fb_index].freq_iterate;
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    total    = _loops[fb_index].Total();
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_iterate;
    total    = _loops[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_left;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_right;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].freq_neither;
    total    = _circuits[fb_index].Total();
    break;

  case FB_EDGE_CALL_INCOMING:
  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_CALL_INOUTSAME:
  case FB_EDGE_IO_OUTGOING:
    fb_index = Get_index_call( wn );
    freq     = _calls[fb_index].freq_exit;
    total    = _calls[fb_index].freq_entry;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index][0];
    total    = _switches[fb_index].Total();
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Get_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      freq  = _switches[fb_index][index_target];
      total = _switches[fb_index].Total();

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      fb_index = Get_index_call( wn );
      freq  = ( _calls[fb_index].in_out_same
		? FB_FREQ_ZERO : FB_FREQ_UNKNOWN );
      total = _calls[fb_index].freq_entry;

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Query_prob(0x%lx, %s) == ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, ", total = " );
    total.Print( TFile );
    fprintf(TFile, "\n" );
  }
  return freq / total;
}


FB_FREQ
FEEDBACK::Query_total_out( const WN *wn ) const
{
  Is_True( wn != NULL, ("FEEDBACK::Query_total_out expects non-NULL wn") );

  INT32 fb_index;
  OPERATOR opr = WN_operator( wn );
  FB_FREQ freq = FB_FREQ_UNINIT;

  switch( opr ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    if ( opr == OPR_RETURN || opr == OPR_RETURN_VAL )
      freq     = FB_FREQ_ZERO;
    else {
      fb_index = Get_index_invoke( wn );
      freq     = _invokes[fb_index].freq_invoke;
    }
    break;

  fb_opr_cases_branch:
    fb_index = Get_index_branch( wn );
    freq     = _branches[fb_index].Total();
    break;

  fb_opr_cases_loop:
    fb_index = Get_index_loop( wn );
    freq     = _loops[fb_index].freq_exit;
    break;

  fb_opr_cases_circuit:
    fb_index = Get_index_circuit( wn );
    freq     = _circuits[fb_index].Total();
    break;

  fb_opr_cases_call:
    fb_index = Get_index_call( wn );
    freq     = ( opr == OPR_IO
		 ? _calls[fb_index].freq_entry
		 : _calls[fb_index].freq_exit  );
    break;

  fb_opr_cases_switch:
    fb_index = Get_index_switch( wn );
    freq     = _switches[fb_index].Total();
    break;

  default:
    break;
  }

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Query_total_out(0x%lx: %s) == ",
	     wn, OPERATOR_name(opr) );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
  }
  return freq;
}


// ====================================================================
// Annot_* functions store feedback data
// ====================================================================

void
FEEDBACK::Annot_invoke( WN *wn, const FB_Info_Invoke& fb_info )
{
  INT32 fb_index = Add_index_invoke( wn );
  _invokes[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_invoke(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_branch( WN *wn, const FB_Info_Branch& fb_info )
{
  INT32 fb_index = Add_index_branch( wn );
  _branches[fb_index] = fb_info;

  Is_True((fb_info.freq_taken.Initialized() || (fb_info.freq_not_taken.Initialized())), ("Must know at least half of the branch frequency")); 

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_branch(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_loop( WN *wn, const FB_Info_Loop& fb_info )
{
  FB_Info_Loop fb = fb_info;

  INT32 fb_index = Add_index_loop( wn );
  if ((fb.freq_positive + fb.freq_back != fb.freq_iterate) &&
      fb.freq_positive.Known() && 
      fb.freq_back.Known()) {
    DevWarn("Loop 0x%x annotated inconsistently. Repairing", wn);
    fb.freq_iterate = fb.freq_positive + fb.freq_back;
  }

  if ((fb.freq_zero + fb.freq_out != fb.freq_exit) &&
      fb.freq_zero.Known() && 
      fb.freq_out.Known()) {
    DevWarn("Loop 0x%x annotated inconsistently. Repairing", wn);
    fb.freq_exit = fb.freq_zero + fb.freq_out;
  }

  _loops[fb_index] = fb;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_loop(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_circuit( WN *wn, const FB_Info_Circuit& fb_info )
{
  INT32 fb_index = Add_index_circuit( wn );
  _circuits[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_circuit(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_call( WN *wn, const FB_Info_Call& fb_info )
{
  INT32 fb_index = Add_index_call( wn );
  _calls[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_call(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}

void
FEEDBACK::Annot_switch( WN *wn, const FB_Info_Switch& fb_info )
{
  INT32 fb_index = Add_index_switch( wn );
  _switches[fb_index] = fb_info;

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::Annot_switch(0x%lx):\n", wn );
    Print_with_wn( TFile, wn );
  }
}


void
FEEDBACK::Annot( WN *wn, FB_EDGE_TYPE type, FB_FREQ freq )
{
  INT32 fb_index;

  switch(type) {

  case FB_EDGE_UNINIT:
    Is_True( false, ( "FEEDBACK::Annot found edge type FB_EDGE_UNINIT" ) );
    break;

  case FB_EDGE_INCOMING:
  case FB_EDGE_OUTGOING:
  case FB_EDGE_ENTRY_OUTGOING:
    fb_index = Add_index_invoke( wn );
    _invokes[fb_index].freq_invoke = freq;
    break;

  case FB_EDGE_BRANCH_TAKEN:
    fb_index = Add_index_branch( wn );
    _branches[fb_index].freq_taken = freq;
    break;

  case FB_EDGE_BRANCH_NOT_TAKEN:
    fb_index = Add_index_branch( wn );
    _branches[fb_index].freq_not_taken = freq;
    break;

  case FB_EDGE_LOOP_ZERO:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_zero = freq;
      if ( ! freq.Known() || loop.freq_out.Known() )
	loop.freq_exit = freq + loop.freq_out;
      else if ( loop.freq_exit.Known() )
	loop.freq_out = loop.freq_exit - freq;
      loop.freq_verify();
    }
    break;

  case FB_EDGE_LOOP_POSITIVE:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_positive = freq;
      if ( ! freq.Known() || loop.freq_back.Known() )
	loop.freq_iterate = freq + loop.freq_back;
      else if ( loop.freq_iterate.Known() )
	loop.freq_back = loop.freq_iterate - freq;
      loop.freq_verify();
    }
    break;

  case FB_EDGE_LOOP_OUT:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_out = freq;
      if ( ! freq.Known() || loop.freq_zero.Known() )
	loop.freq_exit = freq + loop.freq_zero;
      else if ( loop.freq_exit.Known() )
	loop.freq_zero = loop.freq_exit - freq;
      loop.freq_verify();
    }
    break;

  case FB_EDGE_LOOP_BACK:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_back = freq;
      if ( ! freq.Known() || loop.freq_positive.Known() )
	loop.freq_iterate = freq + loop.freq_positive;
      else if ( loop.freq_iterate.Known() )
	loop.freq_positive = loop.freq_iterate - freq;
    loop.freq_verify();
    }
    break;

  case FB_EDGE_LOOP_EXIT:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_exit = freq;
      if ( ! freq.Known() )
	loop.freq_zero = loop.freq_out = freq;
      else if ( loop.freq_zero.Known() )
	if ( ! loop.freq_out.Known() )
	  loop.freq_out = freq - loop.freq_zero;
	else
	  loop.freq_zero = freq - loop.freq_out;
    loop.freq_verify();
    }
    break;

  case FB_EDGE_LOOP_ITERATE:
    fb_index = Add_index_loop( wn );
    {
      FB_Info_Loop& loop = _loops[fb_index];
      loop.freq_iterate = freq;
      if ( ! freq.Known() )
	loop.freq_positive = loop.freq_back = freq;
      else if ( loop.freq_positive.Known() )
	if ( ! loop.freq_back.Known() )
	  loop.freq_back = freq - loop.freq_positive;
	else if ( loop.freq_back.Known() )
	  loop.freq_positive = freq - loop.freq_back;
    loop.freq_verify();
    }
    break;

  case FB_EDGE_CIRCUIT_LEFT:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_left = freq;
    break;

  case FB_EDGE_CIRCUIT_RIGHT:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_right = freq;
    break;

  case FB_EDGE_CIRCUIT_NEITHER:
    fb_index = Add_index_circuit( wn );
    _circuits[fb_index].freq_neither = freq;
    break;

  case FB_EDGE_CALL_INCOMING:
    {
      fb_index = Add_index_call( wn );
      FB_Info_Call& fb_info = _calls[fb_index];
      fb_info.freq_entry = freq;

      if ( ! fb_info.in_out_same &&
	   fb_info.freq_entry.Exact() &&
	   fb_info.freq_exit.Exact() &&
	   fb_info.freq_entry == fb_info.freq_exit )
	fb_info.in_out_same = true;
    }
    break;

  case FB_EDGE_CALL_OUTGOING:
  case FB_EDGE_IO_OUTGOING:
    {
      fb_index = Add_index_call( wn );
      FB_Info_Call& fb_info = _calls[fb_index];
      fb_info.freq_exit = freq;

      if ( ! fb_info.in_out_same &&
	   fb_info.freq_entry.Exact() &&
	   fb_info.freq_exit.Exact() &&
	   fb_info.freq_entry == fb_info.freq_exit )
	fb_info.in_out_same = true;
    }
    break;

  case FB_EDGE_CALL_INOUTSAME:
    fb_index = Add_index_call( wn );
    _calls[fb_index].in_out_same = true;
    _calls[fb_index].freq_entry = freq;
    _calls[fb_index].freq_exit  = freq;
    break;

  case FB_EDGE_SWITCH_DEFAULT:
    fb_index = Add_index_switch( wn );
    _switches[fb_index][0] = freq;
    break;

  default:
    if ( type >= FB_EDGE_SWITCH_BASE ) {

      // case FB_EDGE_SWITCH_BASE:
      fb_index = Add_index_switch( wn );
      const INT32 index_target = FB_EDGE_SWITCH_INDEX( type );
      _switches[fb_index][index_target] = freq;

    } else if ( type >= FB_EDGE_IO_ESCAPE_BASE &&
		type <  FB_EDGE_IO_ESCAPE_BASE + FB_IO_ESCAPE_EDGES_MAX ) {

      // case FB_EDGE_IO_ESCAPE_BASE:
      DevWarn( "Annot called with type FB_EDGE_IO_ESCAPE(br) -- not handled" );

    } else
      Is_True( false, ( "FEEDBACK::Query encounted unexpected edge type" ) );

    break;
  }

  if ( _trace ) {
    char buffer[FB_EDGE_TYPE_NAME_LENGTH];
    FB_EDGE_TYPE_sprintf( buffer, type );

    fprintf( TFile, "FEEDBACK::Annot(0x%lx, %s)  = ", wn, buffer );
    freq.Print( TFile );
    fprintf( TFile, "\n" );
    Print_with_wn( TFile, wn );
  }
}

// ====================================================================
// The following procedures update the feedback information for whirl
// nodes created or modified during lowering transformations.
// ====================================================================

// *IF                                *(FALSEBR <cond> <else_lbl>)
//    <cond>                           <then_clause>
//  THEN                               (GOTO <cont_lbl>)
//    <then_clause>        ===>        (LABEL <else_lbl>)
//  ELSE                               <else_clause>
//    <else_clause>                    (LABEL <cont_lbl>)
//  END_IF
//
//  IF                                *(FALSEBR <cond> <cont_lbl>)
//    <cond>               ===>        <then_clause>
//  THEN                               (LABEL <cont_lbl>)
//    <then_clause>
//  END_IF
//
//  IF                                *(TRUEBR <cond> <cont_lbl>)
//    <cond>               ===>        <else_clause>
//  ELSE                               (LABEL <cont_lbl>)
//    <else_clause>
//  END_IF
//
//  IF                     ===>        (EVAL <cond>)
//    <cond>
//  END_IF
//
// *CSELECT                            *IF
//   <test>                              <test>
//   <then_expr>                        THEN
//   <else_expr>            ===>          <then_expr>
//                                       STID <preg>
//                                      ELSE
//                                        <else_expr>
//                                       STID <preg>
//                                      END_IF
void
FEEDBACK::FB_lower_branch( WN *wn_br, WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_branch(0x%lx, 0x%lx):\n",
	     wn_br, wn_branch );

  Is_True( wn_br != NULL,
	   ( "FEEDBACK::FB_lower_branch expects non-NULL wn_branch" ) );

  // Retrieve IF or CSELECT frequency data
  OPERATOR opr = WN_operator( wn_br );
  Is_True( opr == OPR_IF || opr == OPR_CSELECT,
	   ( "FEEDBACK::FB_lower_branch expects IF or CSELECT wn_br" ) );

  if ( wn_branch != NULL ) {
    OPERATOR opr_lowered = WN_operator( wn_branch );
    Is_True( opr_lowered == OPR_IF || opr_lowered == OPR_CSELECT 
	     || opr_lowered == OPR_TRUEBR || opr_lowered == OPR_FALSEBR,
	     ( "FEEDBACK::FB_lower_branch expects IF, CSELECT or conditional wn_br" ) );
    const FB_Info_Branch& info_branch = Query_branch( wn_br );
    Annot_branch( wn_branch, FB_Info_Branch( info_branch.freq_taken,
					     info_branch.freq_not_taken,
					     WN_operator( wn_branch ) ) );
  }

  Delete( wn_br );
}


//   <cond_1st>               *TRUEBR/FALSEBR <cond_left>  <label>
//   <cond_2nd>      ===>     *TRUEBR/FALSEBR <cond_right> <label>
// *CAND/CIOR                  GOTO <neither_label>
void
FEEDBACK::FB_lower_circuit( WN *wn_circuit, WN *wn_left_br, WN *wn_right_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_circuit(0x%lx, 0x%lx, 0x%lx):\n",
	     wn_circuit, wn_left_br, wn_right_br );

  Is_True( wn_circuit != NULL,
	   ( "FEEDBACK::FB_lower_circuit expects non-NULL wn_circuit" ) );

  // Retrieve CAND/CIOR frequency data
  OPERATOR opr = WN_operator( wn_circuit );
  Is_True( opr == OPR_CAND || opr == OPR_CIOR,
	   ( "FEEDBACK::FB_lower_cand expects CAND wn_circuit" ) );
  const FB_Info_Circuit& info_circuit = Query_circuit( wn_circuit );

  // Annot frequency data for left branch
  FB_Info_Branch fb_info_branch;
  if ( wn_left_br != NULL ) {
    FB_FREQ freq_tkn = info_circuit.freq_left;
    FB_FREQ freq_not = info_circuit.freq_right + info_circuit.freq_neither;
    OPERATOR opr_br  = WN_operator( wn_left_br );
    if ( opr == OPR_CAND )
      fb_info_branch = FB_Info_Branch( freq_not, freq_tkn, opr_br );
    else
      fb_info_branch = FB_Info_Branch( freq_tkn, freq_not, opr_br );
    Annot_branch( wn_left_br, fb_info_branch );
  }

  // Annot frequency data for right branch
  if ( wn_right_br != NULL ) {
    FB_FREQ freq_tkn = info_circuit.freq_right;
    FB_FREQ freq_not = info_circuit.freq_neither;
    OPERATOR opr_br  = WN_operator( wn_right_br );
    if ( opr == OPR_CAND )
      fb_info_branch = FB_Info_Branch( freq_not, freq_tkn, opr_br );
    else
      fb_info_branch = FB_Info_Branch( freq_tkn, freq_not, opr_br );
    Annot_branch( wn_right_br, fb_info_branch );
  }

  Delete( wn_circuit );
}

//				INTCONST 1
//				INTCONST 0
//   <cond_1st>                <cond_1st>
//   <cond_2nd>      ===>      <cond_2nd>
// *CAND/CIOR                 CAND/CIOR
//			     CSELECT 
void
FEEDBACK::FB_lower_circuit_to_cselect ( WN *wn_circuit, WN *wn_cselect)
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_circuit_to_cselect(0x%lx, 0x%lx):\n",
	     wn_circuit, wn_cselect);

  const FB_Info_Circuit& info_circuit =
			Cur_PU_Feedback->Query_circuit( wn_circuit );

  // construct cselect feedback info from circuit feedback info
  if (WN_operator(wn_circuit)==OPR_CIOR) {

    // for CIOR, the branch is taken if either left or right is taken
    FB_Info_Branch info_branch(
		info_circuit.freq_left+info_circuit.freq_right,
		info_circuit.freq_neither);
    Annot_branch(wn_cselect, info_branch);
  } else {

    // for CAND, the taken info is for the negative condition
    // so only if neither left nor right is taken, the test is true
    FB_Info_Branch info_branch(
		info_circuit.freq_neither,
		info_circuit.freq_left+info_circuit.freq_right);
    Annot_branch(wn_cselect, info_branch);
  }
}


//  ( e1 *CIOR e2 ) CAND ( e1 *CIOR e3 )    ===>    e1 *CIOR ( e2 *CAND e3 )
//  ( e1 *CAND e2 ) CIOR ( e1 *CAND e3 )    ===>    e1 *CAND ( e2 *CIOR e3 )
void
FEEDBACK::FB_factor_circuit( WN *wn_left, WN *wn_right,
			     WN *wn_outer, WN *wn_inner )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_factor_circuit(0x%lx, 0x%lx, 0x%lx, 0x%lx):\n",
	     wn_left, wn_right, wn_outer, wn_inner );

  // Retrieve middle, left, and right circuit frequency data
  const FB_Info_Circuit& info_left  = Query_circuit( wn_left  );
  const FB_Info_Circuit& info_right = Query_circuit( wn_right );

  // Annot frequency data for outer and inner circuits
  Annot_circuit( wn_outer, FB_Info_Circuit( info_left.freq_left,
					    info_right.freq_right,
					    info_left.freq_neither
					    + info_right.freq_neither ) );
  Annot_circuit( wn_inner, FB_Info_Circuit( info_left.freq_neither,
					    info_right.freq_neither,
					    info_right.freq_right ) );
  Delete( wn_left  );
  Delete( wn_right );
}


// ====================================================================
// The following procedures lower the feedback information for loop
// (DO_LOOP, DO_WHILE, WHILE_DO) lowering transformations.
// ====================================================================


// *DO_LOOP                            <init_stmt>
//    <index_var>                     *(FALSEBR <end_cond> <cont_lbl>)
//  INIT                               (LABEL <top_lbl> <loop_info>)
//    <init_stmt>          ===>        <body>
//  COMP                               <incr_stmt>
//    <end_cond>                      *(TRUEBR <end_cond> <top_lbl>)
//  INCR                               (LABEL <cont_lbl>)
//    <incr_stmt>
//  BODY
//    <body>
//
// *DO_WHILE                           (LABEL <top_lbl>)
//    <test>               ===>        <body>
//  BODY                              *(TRUEBR <test> <top_lbl>)
//    <body>
//
// *WHILE_DO                          *(FALSEBR <test> <cont_lbl>)
//    <test>               ===>        (LABEL <top_lbl>)
//  BODY                               <body>
//    <body>                          *(TRUEBR <end_cond> <top_lbl>)
//                                     (LABEL <cont_lbl>)
void
FEEDBACK::FB_lower_loop( WN *wn_loop, WN *wn_top_br, WN *wn_back_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_loop(0x%lx, 0x%lx, 0x%lx):\n",
	     wn_loop, wn_top_br, wn_back_br );

  Is_True( wn_loop != NULL,
	   ( "FEEDBACK::FB_lower_loop expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( wn_loop ),
	   ( "FEEDBACK::FB_lower_loop expects loop WHIRL operator" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );
  if ( WN_operator( wn_loop ) == OPR_DO_WHILE && ! info_loop.freq_zero.Zero() )
    DevWarn( "FEEDBACK::FB_lower_loop found freq_zero == %f",
	     info_loop.freq_zero.Value() );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL ) {
    if ( WN_operator( wn_top_br ) == OPR_GOTO ) {
      Is_True( OPT_Space, 
               ( "Loop top branch can be GOTO only with OPT_Space" ) );
      Annot_invoke( wn_top_br, FB_Info_Invoke( info_loop.freq_positive + 
                                               info_loop.freq_zero ) );
    }
    else 
      Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_positive,
                                               info_loop.freq_zero,
                                               WN_operator( wn_top_br ) ) );
  }
  
  // Annote frequency data for back branch
  if ( wn_back_br != NULL ) {
    if ( WN_operator( wn_back_br ) == OPR_GOTO )
      Annot_invoke( wn_back_br, FB_Info_Invoke( info_loop.freq_back ) );
    else
      Annot_branch( wn_back_br, FB_Info_Branch( info_loop.freq_back,
						info_loop.freq_out,
						WN_operator( wn_back_br ) ) );
  }

  // Don't deleted wn_loop data; PREOPT may need it!
}

// 'in_loop' is the original loop
// for pre_peel: we have
// 'in_loop' going to 'peel_iter'
// 'new_loop' going to 'orig' - 'peel_iter'
//
// for post_peel: we have
// 'in_loop' going to the 'orig' - 'peel_iter'
// 'new_loop going to 'peel_iter'
// 
void FEEDBACK::
FB_peel_loop ( WN *in_loop, WN *new_loop, INT64 peel_iter, bool pre_peel)
{
  if ( _trace )
    if (pre_peel) {
      fprintf( TFile, "FEEDBACK::FB_peel_loop(0x%lx, 0x%lx, 0x%lx, PRE):\n",
	     in_loop, new_loop, peel_iter );
    } else {
      fprintf( TFile, "FEEDBACK::FB_peel_loop(0x%lx, 0x%lx, 0x%lx, POST):\n",
	       in_loop, new_loop, peel_iter );
    }

  Is_True( in_loop != NULL,
	   ( "FEEDBACK::FB_peel_loop expects non-NULL in_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( in_loop ),
	   ( "FEEDBACK::FB_peel_loop expects loop WHIRL operator" ) );
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::peel_loop (in_loop)\n" );
    fdump_tree_with_freq( TFile, in_loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
    fprintf( TFile, "\n===== FEEDBACK::peel_loop (in_loop) before\n" );
    Print_with_wn( TFile, in_loop );
    fprintf( TFile, "\n" );
    fprintf( TFile, "\n\n\n\n===== FEEDBACK::peel_loop (new_loop)\n" );
    fdump_tree_with_freq( TFile, new_loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n===== FEEDBACK::peel_loop (new_loop) before\n" );
    Print_with_wn( TFile, new_loop );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& info_in_loop = Query_loop( in_loop );
  if (info_in_loop.freq_positive.Value() <= 0.0) {
    return;
  }
  
  float trip_count = info_in_loop.freq_iterate.Value()/
    info_in_loop.freq_positive.Value();
  float peel_scale = peel_iter/trip_count;

  FB_FREQ peel_freq(peel_scale, false);
  FB_FREQ main_freq(1.0-peel_scale, false);
  
  if (pre_peel) {
    FB_scale(in_loop, peel_freq);
    FB_scale(new_loop, main_freq);
  } else {
    FB_scale(in_loop, main_freq);
    FB_scale(new_loop, peel_freq);
  }
}

//move wn outside the loop body

void FEEDBACK::
FB_hoist_outside_loop ( WN *loop, WN *wn)
{
  if ( _trace )
      fprintf( TFile, "FEEDBACK::FB_hoist_outside_loop (0x%lx, 0x%lx):\n",
	       loop, wn );

  Is_True( loop != NULL,
	   ( "FEEDBACK::FB_hoist_outside_loop expects non-NULL in_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( loop ),
	   ( "FEEDBACK::FB_hoist_outisde_loop expects loop WHIRL operator" ) );
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK: hoisting code outside of loop scale = %f\n" );
    fdump_tree_with_freq( TFile, loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& info_loop = Query_loop( loop );
  if (info_loop.freq_positive.Value() <= 0.0) {
    return;
  }

  FB_FREQ scale(1.0/(info_loop.freq_iterate.Value()/info_loop.freq_positive.Value()), FALSE);
  
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK: hoisting code outside of loop scale = %f\n", scale.Value());
    fdump_tree_with_freq( TFile, loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  FB_scale(wn, scale);
}

// 'in_loop' is the original loop
//  adjust the trip count by iter_count
void FEEDBACK::
FB_adjust_loop ( WN *in_loop, INT64 iter_count)
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_adjust_loop(0x%lx,, 0x%lx):\n",
	     in_loop, iter_count);
  
  Is_True( in_loop != NULL,
	   ( "FEEDBACK::FB_adjust_loop expects non-NULL in_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( in_loop ),
	   ( "FEEDBACK::FB_adjust_loop expects loop WHIRL operator" ) );
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::adjust_loop (in_loop)\n" );
    fdump_tree_with_freq( TFile, in_loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
    fprintf( TFile, "\n===== FEEDBACK::adjust_loop (in_loop) before\n" );
    Print_with_wn( TFile, in_loop );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& info_in_loop = Query_loop( in_loop );

  if (info_in_loop.freq_positive.Value() <= 0.0) {
    return;
  }
  
  float trip_count = info_in_loop.freq_iterate.Value()/
    info_in_loop.freq_positive.Value();
  float scale = (trip_count + iter_count) / trip_count;

  FB_FREQ scale_freq((scale >= 0.0 ? scale : 0.0), false);
  WN * loop_body;
  if (WN_operator(in_loop) == OPR_DO_LOOP)
    loop_body = WN_do_body(in_loop);
  else 
    loop_body = WN_while_body(in_loop);
  FB_scale(loop_body, scale_freq);

  FB_Info_Loop final_loop = info_in_loop;
  final_loop.freq_back *= scale_freq;
  final_loop.freq_iterate = final_loop.freq_positive + final_loop.freq_back;
  Annot_loop(in_loop, final_loop);
}

//set the loop to have new_trip iterations

void FEEDBACK::
FB_wind_down_loop ( WN *in_loop, INT new_trip)
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_wind_down_loop(0x%lx,, 0x%lx):\n",
	     in_loop, new_trip);
  
  Is_True( in_loop != NULL,
	   ( "FEEDBACK::FB_wind_down_loop expects non-NULL in_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( in_loop ),
	   ( "FEEDBACK::FB_wind_down_loop expects loop WHIRL operator" ) );
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::wind_down_loop (in_loop)\n" );
    fdump_tree_with_freq( TFile, in_loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
    fprintf( TFile, "\n===== FEEDBACK::wind_down_loop (in_loop) before\n" );
    Print_with_wn( TFile, in_loop );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& info_in_loop = Query_loop( in_loop );

  if (info_in_loop.freq_positive.Value() <= 0.0) {
    return;
  }
  
  if (new_trip == 0) {
    FB_Info_Loop new_info;
    new_info.freq_zero = info_in_loop.freq_positive;
    new_info.freq_exit = info_in_loop.freq_positive;
    new_info.freq_positive = 0;
    new_info.freq_iterate = 0;
    new_info.freq_back = 0;
    new_info.freq_out = 0;
    FB_scale(in_loop, FB_FREQ_ZERO);
    Annot_loop(in_loop, new_info);
    return;
  }

  float pos = info_in_loop.freq_positive.Value();
  bool exact = info_in_loop.freq_positive.Exact();
  float new_iter = pos * new_trip;
  float new_back = new_iter - pos;
  if (new_back < 0)
    new_back = 0;

  FB_Info_Loop wind_down_freq = info_in_loop;
  wind_down_freq.freq_back = FB_FREQ(new_back, exact);
  wind_down_freq.freq_iterate = FB_FREQ(new_iter, exact);

  WN * loop_body;
  if (WN_operator(in_loop) == OPR_DO_LOOP)
    loop_body = WN_do_body(in_loop);
  else 
    loop_body = WN_while_body(in_loop);
  
  FB_FREQ scale = (wind_down_freq.freq_iterate / info_in_loop.freq_iterate);

  FB_scale(loop_body, scale);
  Annot_loop(in_loop, wind_down_freq);
}

// 'in_loop' is the original loop
// 'tile_loop' is the tile loop
// 'factor' is the tile size
void FEEDBACK::
FB_tile_loop ( WN *in_loop, WN *tile_loop, INT factor)
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_tile_loop(0x%lx,, 0x%lx, 0x%lx):\n",
	     in_loop, tile_loop, factor);
  
  Is_True( in_loop != NULL && tile_loop != NULL && factor>0,
	   ( "FEEDBACK::FB_tile_loop expects non-NULL in_loop and tile_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( in_loop ),
	   ( "FEEDBACK::FB_tile_loop expects loop WHIRL operator" ) );
  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::FB_tile_loop (in_loop)\n" );
    fdump_tree_with_freq( TFile, in_loop, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
    fprintf( TFile, "\n===== FEEDBACK::FB_tile_loop (in_loop) before\n" );
    Print_with_wn( TFile, in_loop );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& in_loop_info = Query_loop( in_loop );

  FB_Info_Loop tile_loop_info(in_loop_info);
  tile_loop_info.freq_back /= factor;
  tile_loop_info.freq_iterate /= factor;
  Annot_loop(tile_loop, tile_loop_info);
}

// if jamming is true, we reduce the inner loops' counter by unroll_factor
void FEEDBACK::
FB_unroll_loop    ( WN *wn_loop, INT unroll_factor, bool jamming )
{
  Is_True( wn_loop != NULL,
	   ( "FEEDBACK::FB_unroll_loop expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  Is_True( FB_valid_opr_loop( wn_loop ),
	   ( "FEEDBACK::FB_unroll_loop expects loop WHIRL operator" ) );

  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_unroll_loop(0x%lx, 0x%x):\n",
	     wn_loop, unroll_factor );
    fprintf( TFile, "\n===== FEEDBACK::FB_unroll_loop before\n" );
    Print_with_wn( TFile, wn_loop );
    fprintf( TFile, "\n" );
  }

  const FB_Info_Loop& orig_loop = Query_loop( wn_loop );
  FB_Info_Loop unrolled_loop(orig_loop);

  //we want integer arithmetic here to prevent rounding 
  //creating imbalances in the tree (you can't execute a code
  //path a fractional amount anyway)
  INT64 freq_back = ((INT64) unrolled_loop.freq_back.Value() -
		       (INT64) unrolled_loop.freq_positive.Value() *
		       (INT64) (unroll_factor-1)) / unroll_factor;

  float old_execs = unrolled_loop.freq_positive.Value() + unrolled_loop.freq_back.Value();
  float new_execs = unrolled_loop.freq_positive.Value() + freq_back;

  FB_FREQ scale(((old_execs > 0) ? (new_execs / old_execs) : 1), false);

  unrolled_loop.freq_back = freq_back;
  unrolled_loop.freq_iterate = freq_back + unrolled_loop.freq_positive;;
  if (jamming) {
    FB_scale(wn_loop, scale);
    /* this will scale the wn_loop as well, but we don't want to
       scale the wn_loop's positive frequency.  This is taken cared
       of by the following Annot_loop */ 
  }
  Annot_loop(wn_loop, unrolled_loop);
}

void 
FEEDBACK::FB_adjust_frequency( WN* parent, WN*new_child)
{
   FB_Info_Branch info_b = Query_branch(parent);
  FB_FREQ parent_f = info_b.freq_not_taken;
  FB_FREQ child_f = Query_total_out(new_child);
  FB_FREQ scale = parent_f/child_f;
  if (scale.Initialized())
    FB_scale (new_child, scale);
  else
    FB_scale (new_child, FB_FREQ(1.0, FB_FREQ_TYPE_GUESS));
}



//  DO_LOOP/WHILE_DO                   LABEL <top_label>
//    <test>               ===>        FALSEBR <test> <cont_label>
//  BODY                               <body>
//    <body>                           GOTO <top_label>
//                                     LABEL <cont_lbl>
void
FEEDBACK::FB_lower_loop_alt ( WN *wn_loop, WN *wn_top_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_loop_alt(0x%lx, 0x%lx):\n",
	     wn_loop, wn_top_br );

  Is_True( wn_loop != NULL,
	   ( "FEEDBACK::FB_lower_loop_alt expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_loop );
  Is_True( opr == OPR_WHILE_DO || opr == OPR_DO_LOOP,
	   ( "FEEDBACK::FB_lower_loop_alt expects"
	     " WHILE_DO or DO_LOOP wn_loop" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL )
    Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_iterate,
					     info_loop.freq_exit,
					     WN_operator( wn_top_br ) ) );

  // Don't deleted wn_loop data; PREOPT may need it!
}


//  WHILE_DO                           IF
//    <test>                             <test>
//  BODY                               THEN
//    <body>               ===>          DO_WHILE
//                                         <test>
//                                       BODY
//                                         <body>
//                                       END_DO_WHILE
//                                     END_IF
//
// wn_loop WN is not deleted, but modified ( WHILE_DO --> DO_WHILE )
void
FEEDBACK::FB_lower_while_do_to_do_while ( WN *wn_loop, WN *wn_top_br )
{
  // NOTE: the same WN wn_loop is both WHILE_DO and DO_WHILE
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_while_do(0x%lx, 0x%lx):\n",
	     wn_loop, wn_top_br );

  Is_True( wn_loop != NULL,
	  ( "FEEDBACK::FB_lower_while_do expects non-NULL wn_loop" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_loop );
  Is_True( opr == OPR_WHILE_DO, ( "FEEDBACK::FB_lower_while_do_to_do_while"
				  " expects WHILE_DO wn_loop" ) );
  const FB_Info_Loop& info_loop = Query_loop( wn_loop );

  // Annote frequency data for top branch
  if ( wn_top_br != NULL )
    Annot_branch( wn_top_br, FB_Info_Branch( info_loop.freq_positive,
					     info_loop.freq_zero,
					     WN_operator( wn_top_br ) ) );

  // Update frequency data for DO_WHILE loop
  Annot( wn_loop, FB_EDGE_LOOP_ZERO, FB_FREQ_ZERO );
}


// ====================================================================
// The following procedure lowers the feedback information for COMPGOTO
// ====================================================================

void
FEEDBACK::FB_lower_compgoto ( WN *wn_compgoto, WN *wn_xgoto, WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_compgoto(0x%lx, 0x%lx, 0x%lx):\n",
	     wn_compgoto, wn_xgoto, wn_branch );

  Is_True( wn_compgoto != NULL,
	   ( "FEEDBACK::FB_lower_compgoto expects non-NULL wn_compgoto" ) );

  // Retrieve loop frequency data
  OPERATOR opr = WN_operator( wn_compgoto );
  Is_True( opr == OPR_COMPGOTO, ( "FEEDBACK::FB_lower_compgoto"
				  " expects COMPGOTO compgoto" ) );
  FB_Info_Switch info_switch = Query_switch( wn_compgoto );

  // Strip off default frequency
  FB_FREQ freq_default = ( WN_kid_count( wn_compgoto ) == 3
			   ? info_switch[0] : FB_FREQ_UNINIT );
  info_switch[0] = FB_FREQ_ZERO;

  // Annote frequency data for top branch
  if ( wn_branch != NULL )
    Annot_branch( wn_branch, FB_Info_Branch( freq_default, info_switch.Total(),
					     WN_operator( wn_branch ) ) );

  // Annot frequency data for XGOTO loop
  Annot_switch( wn_xgoto, info_switch );

  Delete( wn_compgoto );
}


// ====================================================================
// The following procedures lower the feedback information for CALL
// and RETURN_VAL and MSTORE and MLOAD lowering transformations.
// ====================================================================

void
FEEDBACK::FB_lower_call( WN *wn_call, WN *wn_new_call )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_call(0x%lx, 0x%lx):\n",
	     wn_call, wn_new_call );

  Is_True( wn_call != NULL,
	   ( "FEEDBACK::FB_lower_call expects non-NULL wn_call" ) );

  Is_True( FB_valid_opr_call( wn_call ),
	   ( "FEEDBACK::FB_lower_call encounted unexpected operator" ) );
  FB_Info_Call info_call = Query_call( wn_call );

  if ( ! info_call.in_out_same )
    info_call.freq_entry = FB_FREQ_UNKNOWN;

  Is_True( FB_valid_opr_call( wn_new_call ),
	   ( "FEEDBACK::FB_lower_call encounted unexpected operator" ) );
  Annot_call( wn_new_call, info_call );

  if ( wn_call != wn_new_call )
    Delete( wn_call );
}


// RETURNVAL                            <expr>
//  <expr>                 ===>        STID <preg>
//                                     RETURN
void
FEEDBACK::FB_lower_return_val( WN *wn_return_val, WN *wn_return )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_return_val(0x%lx, 0x%lx):\n",
	     wn_return_val, wn_return );

  Is_True( wn_return_val != NULL,
	   ( "FEEDBACK::FB_lower_return_val expects non-NULL wn_return_val" ) );

  OPERATOR opr = WN_operator( wn_return_val );
  Is_True( opr == OPR_RETURN_VAL, ( "FEEDBACK::FB_lower_return_val"
				    " expects RETURN_VAL wn_return_val" ) );
  const FB_Info_Invoke& info_invoke = Query_invoke( wn_return_val );

  Annot_invoke( wn_return, info_invoke );

  Delete( wn_return_val );
}

// MSTORE                  ===>        DO_LOOP  (constant # of iterations)
// MSTORE                  ===>        WHILE_DO (variable # of iterations)

void
FEEDBACK::FB_lower_mstore_to_loop ( WN *wn_mstore, WN *wn_loop, INT64 nMoves )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::lower_mstore_to_loop(0x%lx, 0x%lx, %" LLD_FMT "):\n",
	     wn_mstore, wn_loop, nMoves );

  Is_True( wn_mstore != NULL, ( "FEEDBACK::FB_lower_mstore_to_loop"
				" expects non-NULL wn_mstore" ) );

  Is_True( WN_operator( wn_mstore ) == OPR_MSTORE,
	   ( "FEEDBACK::FB_lower_mstore_to_loop expects MSTORE" ) );
  const FB_Info_Invoke& info_invoke = Query_invoke( wn_mstore );

  OPERATOR opr = WN_operator( wn_loop );
  FB_Info_Loop info_loop;
  if ( opr == OPR_DO_LOOP ) {

    // Constant number of iterations == nMoves
    Is_True( nMoves > 0, ( "FEEDBACK::lower_mstore_to_loop found"
			   " non-positive nMoves == %d", nMoves ) );
    info_loop = FB_Info_Loop( FB_FREQ_ZERO,
			      info_invoke.freq_invoke,
			      info_invoke.freq_invoke,
			      FB_FREQ( nMoves - 1, true /*exact*/ )
			      * info_invoke.freq_invoke );
  } else {

    // Variable/unknown number of iterations
    Is_True( opr == OPR_WHILE_DO, ( "FEEDBACK::FB_lower_mstore_to_loop"
				    " expects DO_LOOP or WHILE_DO" ) );
    info_loop = FB_Info_Loop( info_invoke.freq_invoke, FB_FREQ_UNKNOWN );

  }

  Annot_loop( wn_loop, info_loop );

  Delete( wn_mstore );
}

void
FEEDBACK::FB_lower_mstore_to_call ( WN *wn_mstore, WN *wn_call )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_lower_mstore_to_call(0x%lx, 0x%lx):\n",
	     wn_mstore, wn_call );

  Is_True( wn_mstore && WN_operator (wn_mstore) == OPR_MSTORE,
	   ( "FEEDBACK::FB_lower_mstore_to_call: unexpected wn_mstore" ) );

  Is_True( wn_call && WN_operator (wn_call) == OPR_CALL,
	   ( "FEEDBACK::FB_lower_mstore_to_call: unexpected wn_call" ) );

  const FB_Info_Invoke& info_invoke = Query_invoke( wn_mstore );

  FB_Info_Call info_call( info_invoke.freq_invoke );

  Annot_call( wn_call, info_call );

  Delete( wn_mstore );
}


// ====================================================================
// Converting GOTOs into IFs and DO_WHILEs
// ====================================================================

// IF                                       IF
//   <expr>                                   <expr>
// THEN/ELSE                                THEN/ELSE
//   <statements1>                            <statements1>
//   TRUEBR <cond> <label>                    <goto_preg> = <cond>
//   <statements2>              ===>          IF
// END_IF                                       <! goto_preg>
//                                            THEN
//                                              <statements2>
//                                            ENDIF
//                                          END_IF
//                                          TRUEBR <goto_preg> <label>
void
FEEDBACK::FB_move_goto_out( WN *wn_branch, WN *wn_inner_br, WN *wn_outer_br )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_move_goto_out(0x%lx, 0x%lx, 0x%lx):\n",
	     wn_branch, wn_inner_br, wn_outer_br );

  Is_True( wn_branch != NULL,
	   ( "FEEDBACK::FB_move_goto_out expects non-NULL wn_branch" ) );

  // Retrieve goto (GOTO or TRUEBR or FALSEBR) frequency data
  FB_FREQ freq_taken, freq_not;
  OPERATOR opr = WN_operator( wn_branch );
  if ( opr == OPR_GOTO ) {
    const FB_Info_Invoke& info_invoke = Query_invoke( wn_branch );
    freq_taken = info_invoke.freq_invoke;
    freq_not   = FB_FREQ_ZERO;
  } else {
    Is_True( opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	     ( "FEEDBACK::FB_move_goto_out expects TRUE/FALSEBR wn_branch" ) );
    const FB_Info_Branch& info_branch = Query_branch( wn_branch );
    freq_taken = info_branch.freq_taken;
    freq_not   = info_branch.freq_not_taken;
  }

  // Annote frequency data for inner IF
  if ( wn_inner_br != NULL ) {
    opr = WN_operator( wn_inner_br );
    Is_True( opr == OPR_IF,
	     ( "FEEDBACK::FB_move_goto_out expects NULL or IF wn_inner_br" ) );
    Annot_branch( wn_inner_br, FB_Info_Branch( freq_not, freq_taken ) );
  }

  // Update frequency data for outer TRUEBR
  opr = WN_operator( wn_outer_br );
  Is_True( opr == OPR_TRUEBR,
	   ( "FEEDBACK::FB_move_goto_out expects TRUEBR wn_outer_br" ) );
  Annot_branch( wn_outer_br, FB_Info_Branch( freq_taken, FB_FREQ_UNKNOWN ) );

  Delete( wn_branch );
}


// TRUEBR <cond> <label>                    IF
// <statements>                               <! cond>
// LABEL <label>                ===>        THEN
//                                            <statements>
//                                          END_IF
void
FEEDBACK::FB_convert_goto_to_if( WN *wn_branch, WN *wn_if )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_convert_goto_to_if(0x%lx, 0x%lx):\n",
	     wn_branch, wn_if );

  Is_True( wn_branch != NULL,
	   ( "FEEDBACK::FB_convert_goto_to_if expects non-NULL wn_branch" ) );

  // Retrieve goto (TRUEBR or FALSEBR) frequency data
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	   ( "FEEDBACK::FB_convert_goto_to_if"
	     " expects TRUE/FALSEBR wn_branch" ) );
  const FB_Info_Branch& info_branch = Query_branch( wn_branch );

  // Annote frequency data for inner IF
  opr = WN_operator( wn_if );
  Is_True( opr == OPR_IF,
	   ( "FEEDBACK::FB_convert_goto_to_if expects IF wn_if" ) );
  Annot_branch( wn_if, FB_Info_Branch( info_branch.freq_not_taken,
				       info_branch.freq_taken ) );

  Delete( wn_branch );
}


// LABEL <label>                            DO_WHILE
// <statements>                               <cond>
// TRUEBR <cond> <label>        ===>        BODY
//                                            <statements>
//                                          END_DO_WHILE
void
FEEDBACK::FB_convert_goto_to_loop( WN *wn_branch, WN *wn_loop )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_convert_goto_to_loop(0x%lx, 0x%lx):\n",
	     wn_branch, wn_loop );

  Is_True( wn_branch != NULL, ( "FEEDBACK::FB_convert_goto_to_loop"
				" expects non-NULL wn_branch" ) );

  // Retrieve goto (TRUEBR or FALSEBR) frequency data
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_GOTO || opr == OPR_TRUEBR || opr == OPR_FALSEBR,
	   ( "FEEDBACK::FB_convert_goto_to_loop"
	     " expects GOTO, TRUEBR or FALSEBR wn_branch" ) );

  FB_FREQ freq_taken, freq_not_taken;
  if ( opr == OPR_GOTO ) {    // Unconditional branch
    const FB_Info_Invoke& info_invoke = Query_invoke( wn_branch );
    freq_taken     = info_invoke.freq_invoke;
    freq_not_taken = FB_FREQ_ZERO;
  } else {                  // Conditional branch
    const FB_Info_Branch& info_branch = Query_branch( wn_branch );
    freq_taken     = info_branch.freq_taken;
    freq_not_taken = info_branch.freq_not_taken;
  }

  // Annote frequency data for inner DO_WHILE loop
  opr = WN_operator( wn_loop );
  Is_True( opr == OPR_DO_WHILE,
	   ( "FEEDBACK::FB_convert_goto_to_loop expects DO_WHILE wn_loop" ) );
  Annot_loop( wn_loop, FB_Info_Loop( FB_FREQ_ZERO, FB_FREQ_UNKNOWN,
				     freq_not_taken, freq_taken ) );

  Delete( wn_branch );
}


// ====================================================================
// Convert a TRUEBR/FALSEBR branch with a constant condition into a GOTO
// ====================================================================

void
FEEDBACK::FB_simplify_branch_to_goto( WN *wn_branch )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_simplify_branch_to_goto(0x%lx):\n",
	     wn_branch );

  Is_True( wn_branch != NULL, ( "FEEDBACK::FB_simplify_branch_to_goto"
				" expects non-NULL wn_branch" ) );
  OPERATOR opr = WN_operator( wn_branch );
  Is_True( opr == OPR_GOTO, ( "FEEDBACK::FB_simplify_branch_to_goto"
			      " expects GOTO wn_branch" ) );

  // Retrieve branch (TRUEBR or FALSEBR) frequency data
  WN_set_operator( wn_branch, OPR_TRUEBR );
  const FB_Info_Branch& info_branch = Query_branch( wn_branch );
  WN_set_operator( wn_branch, OPR_GOTO );

  Delete( wn_branch );

  if ( info_branch.freq_not_taken.Known()
       && ! info_branch.freq_not_taken.Zero() ) {
    DevWarn( "FEEDBACK::FB_simplify_branch_to_goto: nonzero freq_not_taken" );
  }

  // Annote frequency data for GOTO
  Annot_invoke( wn_branch, FB_Info_Invoke( info_branch.freq_taken ) );
}


// ====================================================================
// Scaling, Duplicating, Cloning, and Recombining whirl nodes and trees
// ====================================================================

void
FEEDBACK::FB_set_zero_node( WN *wn )
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info( FB_FREQ_ZERO );
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO,
			    FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info( FB_FREQ_ZERO, FB_FREQ_ZERO, FB_FREQ_ZERO );
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info( FB_FREQ_ZERO );
      Annot_call( wn, fb_info );
    }
    break;

  fb_opr_cases_switch:
    {
      // Count number of cases
      INT32 number_cases = ( WN_kid_count(wn) == 3 ) ? 1 : 0;
      for ( WN *wn1 = WN_first( WN_kid1( wn ) ); wn1; wn1 = WN_next( wn1 ) )
	++number_cases;

      FB_Info_Switch fb_info( number_cases );
      for (INT32 t = number_cases - 1; t >= 0; --t ) {
	fb_info[t] = FB_FREQ_ZERO;
      }
      Annot_switch( wn, fb_info );
    }
    break;

  default:
    break;
  }
}


void
FEEDBACK::FB_set_zero( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_zero(0x%lx):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Zero out the node data
    FB_set_zero_node( iter.Wn() );
  }
}

void
FEEDBACK::FB_set_unknown_node( WN *wn )
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info( FB_FREQ_UNKNOWN );
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN,
			    FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN,
			    FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info( FB_FREQ_UNKNOWN,
			       FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info( FB_FREQ_UNKNOWN, FB_FREQ_UNKNOWN );
      Annot_call( wn, fb_info );
    }
    break;

  fb_opr_cases_switch:
    {
      // Count number of cases
      INT32 number_cases = ( WN_kid_count(wn) == 3 ) ? 1 : 0;;
      for ( WN *wn1 = WN_first( WN_kid1( wn ) ); wn1; wn1 = WN_next( wn1 ) )
	++number_cases;

      FB_Info_Switch fb_info( number_cases );
      for (INT32 t = number_cases - 1; t >= 0; --t ) {
	fb_info[t] = FB_FREQ_UNKNOWN;
      }
      Annot_switch( wn, fb_info );
    }
    break;

  default:
    break;
  }
}


void
FEEDBACK::FB_set_unknown( WN *wn )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_set_unknown(0x%lx):\n", wn );

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Zero out the node data
    FB_set_unknown_node( iter.Wn() );
  }
}


void
FEEDBACK::FB_scale_node( WN *wn, FB_FREQ freq_scale )
{
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0,
	   ( "FEEDBACK::FB_scale_node: freq_scale == %f", 
	     freq_scale.Value() ) );

  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info = Query_invoke( wn );
      fb_info.freq_invoke *= freq_scale;
      Annot_invoke( wn, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info = Query_branch( wn );
      fb_info.freq_taken     *= freq_scale;
      fb_info.freq_not_taken *= freq_scale;
      Annot_branch( wn, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info = Query_loop( wn );
      fb_info.freq_zero     *= freq_scale;
      fb_info.freq_positive *= freq_scale;
      fb_info.freq_out      *= freq_scale;
      fb_info.freq_back     *= freq_scale;
      fb_info.freq_exit     *= freq_scale;
      fb_info.freq_iterate  *= freq_scale;
      Annot_loop( wn, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info = Query_circuit( wn );
      fb_info.freq_left    *= freq_scale;
      fb_info.freq_right   *= freq_scale;
      fb_info.freq_neither *= freq_scale;
      Annot_circuit( wn, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info = Query_call( wn );
      fb_info.freq_entry *= freq_scale;
      fb_info.freq_exit  *= freq_scale;
      Annot_call( wn, fb_info );
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info = Query_switch( wn );
      for ( INT t = fb_info.size() - 1; t >= 0; t-- )
	fb_info[t] *= freq_scale;
      Annot_switch( wn, fb_info );
    }
    break;

  default:
    break;
  }
}


void
FEEDBACK::FB_scale( WN *wn, FB_FREQ freq_scale )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_scale(0x%lx, ", wn );
    freq_scale.Print( TFile );
    fprintf( TFile, "):\n" );
  }

  for ( TREE_ITER iter( wn ); iter.Wn() != NULL; ++iter ) {

    // Scale the node data
    FB_scale_node( iter.Wn(), freq_scale );
  }
}


void
FEEDBACK::FB_duplicate_node( WN *wn_origl, WN *wn_dupli )
{
  // Annotate the duplicate whirl node
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      const FB_Info_Invoke fb_info = Query_invoke( wn_origl );
      Annot_invoke( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_branch:
    {
      const FB_Info_Branch fb_info = Query_branch( wn_origl );
      Annot_branch( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_loop:
    {
      const FB_Info_Loop fb_info = Query_loop( wn_origl );
      Annot_loop( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_circuit:
    {
      const FB_Info_Circuit fb_info = Query_circuit( wn_origl );
      Annot_circuit( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_call:
    {
      const FB_Info_Call fb_info = Query_call( wn_origl );
      Annot_call( wn_dupli, fb_info );
    }
    break;

  fb_opr_cases_switch:
    {
      const FB_Info_Switch fb_info = Query_switch( wn_origl );
      Annot_switch( wn_dupli, fb_info );
    }
    break;

  default:
    break;
  }
}


void
FEEDBACK::FB_duplicate( WN *wn_origl, WN *wn_dupli )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_duplicate(0x%lx, 0x%lx):\n",
	     wn_origl, wn_dupli );

  for ( TREE_ITER origl( wn_origl ), dupli( wn_dupli );
	origl.Wn() != NULL && dupli.Wn() != NULL;
	++origl, ++dupli ) {
    
    // Duplicate the node data
    FB_duplicate_node( origl.Wn(), dupli.Wn() );
  }
}


void
FEEDBACK::FB_recombine_node( WN *wn_origl, WN *wn_extra )
{
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = Query_invoke( wn_origl );
      const FB_Info_Invoke& fb_info_extra = Query_invoke( wn_extra );
      fb_info_origl.freq_invoke += fb_info_extra.freq_invoke;
      Annot_invoke( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );
      const FB_Info_Branch& fb_info_extra = Query_branch( wn_extra );
      fb_info_origl.freq_taken     += fb_info_extra.freq_taken    ;
      fb_info_origl.freq_not_taken += fb_info_extra.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = Query_loop( wn_origl );
      const FB_Info_Loop& fb_info_extra = Query_loop( wn_extra );
      fb_info_origl.freq_zero     += fb_info_extra.freq_zero    ;
      fb_info_origl.freq_positive += fb_info_extra.freq_positive;
      fb_info_origl.freq_out      += fb_info_extra.freq_out     ;
      fb_info_origl.freq_back     += fb_info_extra.freq_back    ;
      fb_info_origl.freq_exit     += fb_info_extra.freq_exit    ;
      fb_info_origl.freq_iterate  += fb_info_extra.freq_iterate ;
      Annot_loop( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      const FB_Info_Circuit& fb_info_extra = Query_circuit( wn_extra );
      fb_info_origl.freq_left    += fb_info_extra.freq_left   ;
      fb_info_origl.freq_right   += fb_info_extra.freq_right  ;
      fb_info_origl.freq_neither += fb_info_extra.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = Query_call( wn_origl );
      const FB_Info_Call& fb_info_extra = Query_call( wn_extra );
      fb_info_origl.freq_entry += fb_info_extra.freq_entry;
      fb_info_origl.freq_exit  += fb_info_extra.freq_exit;
      Annot_call( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = Query_switch( wn_origl );
      const FB_Info_Switch& fb_info_extra = Query_switch( wn_extra );
      for ( INT t = fb_info_origl.size() - 1; t >= 0; t-- )
	fb_info_origl[t] += fb_info_extra[t];
      Annot_switch( wn_origl, fb_info_origl );
      Delete( wn_extra );
    }
    break;

  default:
    break;
  }
}


void
FEEDBACK::FB_recombine( WN *wn_origl, WN *wn_extra )
{
  if ( _trace )
    fprintf( TFile, "FEEDBACK::FB_recombine(0x%lx, 0x%lx):\n",
	     wn_origl, wn_extra );

  for ( TREE_ITER origl( wn_origl ), extra(wn_extra);
	origl.Wn() != NULL && extra.Wn() != NULL;
	++origl, ++extra ) {

    // Recombine the node data
    FB_recombine_node( origl.Wn(), extra.Wn() );
  }
}


void
FEEDBACK::FB_clone_node( WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale )
{
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_scale: freq_scale == %f", freq_scale.Value() ) );

  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = Query_invoke( wn_origl );
      FB_Info_Invoke fb_info_clone( fb_info_origl.freq_invoke * freq_scale );
      fb_info_origl.freq_invoke -= fb_info_clone.freq_invoke;
      Annot_invoke( wn_origl, fb_info_origl );
      Annot_invoke( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );
      FB_Info_Branch fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
				    fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Annot_branch( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = Query_loop( wn_origl );
      FB_Info_Loop fb_info_clone( fb_info_origl.freq_zero     * freq_scale,
				  fb_info_origl.freq_positive * freq_scale,
				  fb_info_origl.freq_out      * freq_scale,
				  fb_info_origl.freq_back     * freq_scale,
				  fb_info_origl.freq_exit     * freq_scale,
				  fb_info_origl.freq_iterate  * freq_scale );
      fb_info_origl.freq_zero     -= fb_info_clone.freq_zero    ;
      fb_info_origl.freq_positive -= fb_info_clone.freq_positive;
      fb_info_origl.freq_out      -= fb_info_clone.freq_out     ;
      fb_info_origl.freq_back     -= fb_info_clone.freq_back    ;
      fb_info_origl.freq_exit     -= fb_info_clone.freq_exit    ;
      fb_info_origl.freq_iterate  -= fb_info_clone.freq_iterate ;
      Annot_loop( wn_origl, fb_info_origl );
      Annot_loop( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left    * freq_scale,
				     fb_info_origl.freq_right   * freq_scale,
				     fb_info_origl.freq_neither * freq_scale );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = Query_call( wn_origl );
      FB_Info_Call fb_info_clone( fb_info_origl.freq_entry * freq_scale,
				  fb_info_origl.freq_exit  * freq_scale,
				  fb_info_origl.in_out_same );
      fb_info_origl.freq_entry -= fb_info_clone.freq_entry;
      fb_info_origl.freq_exit  -= fb_info_clone.freq_exit;
      Annot_call( wn_origl, fb_info_origl );
      Annot_call( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = Query_switch( wn_origl );
      FB_Info_Switch fb_info_clone = fb_info_origl;
      for ( INT t = fb_info_origl.size() - 1; t >= 0; t-- ) {
	fb_info_clone[t] *= freq_scale;
	fb_info_origl[t] -= fb_info_clone[t];
      }
      Annot_switch( wn_origl, fb_info_origl );
      Annot_switch( wn_clone, fb_info_clone );
    }
    break;

  default:
    break;
  }
}

void
FEEDBACK::FB_clone( WN *wn_origl, WN *wn_clone, FB_FREQ freq_scale )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_clone(0x%lx, 0x%lx", wn_origl, wn_clone );
    freq_scale.Print( TFile );
    fprintf( TFile, "):\n" );
  }

  for ( TREE_ITER origl( wn_origl ), clone( wn_clone );
	origl.Wn() != NULL && clone.Wn() != NULL;
	++origl, ++clone ) {

    // Clone the node data
    FB_clone_node( origl.Wn(), clone.Wn(), freq_scale );
  }
}


void
FEEDBACK::FB_clone_test( WN *wn_origl, WN *wn_clone,
			 FB_FREQ freq_origl_taken, FB_FREQ freq_origl_not,
			 FB_FREQ freq_clone_taken, FB_FREQ freq_clone_not )
{
  if ( ! ( freq_origl_taken.Known() && freq_origl_not.Known() &&
	   freq_clone_taken.Known() && freq_clone_not.Known() )) {

    DevWarn( "FEEDBACK::FB_clone_test found unknown frequency" );

    // Guess a half-half split
    FB_clone( wn_origl, wn_clone, FB_FREQ( 0.5, false ) );
    return;
  }

  // First, annotate the original and duplicate whirl nodes
  OPERATOR opr = WN_operator( wn_origl );
  switch( opr ) {

  case OPR_LNOT:
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_not, freq_origl_taken,
		   freq_clone_not, freq_clone_taken );
    break;

  case OPR_CAND:
    {
      FB_FREQ freq_scale =
	freq_clone_not / ( freq_origl_not + freq_clone_not );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      // Update the CAND frequencies
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left  * freq_scale,
				     fb_info_origl.freq_right * freq_scale,
				     freq_clone_taken );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );

      // Update the two kids
      FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		     fb_info_origl.freq_right + fb_info_origl.freq_neither,
		     fb_info_origl.freq_left,
		     fb_info_clone.freq_right + fb_info_clone.freq_neither,
		     fb_info_clone.freq_left );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     fb_info_origl.freq_neither, fb_info_origl.freq_right,
		     fb_info_clone.freq_neither, fb_info_clone.freq_right );
    }
    break;

  case OPR_CIOR:
    {
      FB_FREQ freq_scale =
	freq_clone_taken / ( freq_origl_taken + freq_clone_taken );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      // Update the CIOR frequencies
      FB_Info_Circuit fb_info_origl = Query_circuit( wn_origl );
      FB_Info_Circuit fb_info_clone( fb_info_origl.freq_left  * freq_scale,
				     fb_info_origl.freq_right * freq_scale,
				     freq_clone_not );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      Annot_circuit( wn_origl, fb_info_origl );
      Annot_circuit( wn_clone, fb_info_clone );

      // Update the two kids
      FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		     fb_info_origl.freq_left,
		     fb_info_origl.freq_right + fb_info_origl.freq_neither,
		     fb_info_clone.freq_left,
		     fb_info_clone.freq_right + fb_info_clone.freq_neither );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     fb_info_origl.freq_right, fb_info_origl.freq_neither,
		     fb_info_clone.freq_right, fb_info_clone.freq_neither );
    }
    break;

  case OPR_COMMA:
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    break;

  case OPR_RCOMMA: // NOTE: This is not quite right
    FB_clone_test( WN_kid0( wn_origl ), WN_kid0( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		   freq_origl_taken, freq_origl_not,
		   freq_clone_taken, freq_clone_not );
    break;

  case OPR_CSELECT:
    {
      // Compute the scale factor
      FB_FREQ freq_origl_total = freq_origl_taken + freq_origl_not;
      FB_FREQ freq_clone_total = freq_clone_taken + freq_clone_not;
      FB_FREQ freq_scale =
	freq_clone_total / ( freq_origl_total + freq_clone_total );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      FB_Info_Branch fb_info_origl = Query_branch( wn_origl );

      // Calculate the scale for kid1
      FB_FREQ freq_scale_kid1 =
	fb_info_origl.freq_taken / fb_info_origl.Total();
      if ( ! freq_scale_kid1.Known() )
	freq_scale_kid1 = FB_FREQ( 0.5, false );  // Guess 0.5

      // Scale the CSELECT
      FB_Info_Branch
	fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
		       fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      Annot_branch( wn_origl, fb_info_origl );
      Annot_branch( wn_clone, fb_info_clone );

      // Compute the scaled frequencies for kid1 and kid2
      FB_FREQ freq_kid1_origl_taken = freq_origl_taken * freq_scale_kid1;
      FB_FREQ freq_kid1_origl_not   = freq_origl_not   * freq_scale_kid1;
      FB_FREQ freq_kid1_clone_taken = freq_clone_taken * freq_scale_kid1;
      FB_FREQ freq_kid1_clone_not   = freq_clone_not   * freq_scale_kid1;
      freq_origl_taken -= freq_kid1_origl_taken;
      freq_origl_not   -= freq_kid1_origl_not;
      freq_clone_taken -= freq_kid1_clone_taken;
      freq_clone_not   -= freq_kid1_clone_not;

      // Scale the three kids
      FB_clone( WN_kid0( wn_origl ), WN_kid0( wn_clone ), freq_scale );
      FB_clone_test( WN_kid1( wn_origl ), WN_kid1( wn_clone ),
		     freq_kid1_origl_taken, freq_kid1_origl_not,
		     freq_kid1_clone_taken, freq_kid1_clone_not );

      FB_clone_test( WN_kid2( wn_origl ), WN_kid2( wn_clone ),
		     freq_origl_taken, freq_origl_not,
		     freq_clone_taken, freq_clone_not );
    }
    break;

  default:
    {
      // Compute the scale factor
      FB_FREQ freq_origl_total = freq_origl_taken + freq_origl_not;
      FB_FREQ freq_clone_total = freq_clone_taken + freq_clone_not;
      FB_FREQ freq_scale =
	freq_clone_total / ( freq_origl_total + freq_clone_total );
      if ( ! freq_scale.Known() )
	freq_scale = FB_FREQ( 0.5, false );  // Guess 0.5

      FB_clone( wn_origl, wn_clone, freq_scale );
    }
    break;
  }
}


void
FEEDBACK::FB_clone_loop_test( WN *wn_origl, WN *wn_clone, WN *wn_loop )
{
  if ( _trace ) {
    fprintf( TFile, "FEEDBACK::FB_clone_loop_test(0x%lx, 0x%lx, 0x%lx):\n",
	     wn_origl, wn_clone, wn_loop );
    Print_with_wn( TFile, wn_origl );
    Print_with_wn( TFile, wn_loop );
  }
  const FB_Info_Loop& fb_info = Query_loop( wn_loop );

  if ( fb_info.freq_back.Known()     && fb_info.freq_out.Known() &&
       fb_info.freq_positive.Known() && fb_info.freq_zero.Known() ) {
    FB_clone_test( wn_origl, wn_clone,
		   fb_info.freq_back,     fb_info.freq_out,
		   fb_info.freq_positive, fb_info.freq_zero );
  } else {
    // Guess that the loop iterates an average of eight times
    FB_clone( wn_origl, wn_clone, FB_FREQ( 0.125, false ) );
  }
}


// ====================================================================
// IPA Cloning and Inlining
// ====================================================================
      
void
FB_IPA_Clone_node( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
		   WN             *wn_origl, WN             *wn_clone,
		   FB_FREQ freq_scale )
{
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_IPA_Clone_node: freq_scale == %f",
	     freq_scale.Value() ) );

  Is_True( feedback_origl != NULL,
	   ( "FEEDBACK::FB_IPA_Clone_node: feedback_origl == NULL" ) );

  // First, annotate the original and clone whirl nodes
  switch( WN_operator( wn_origl ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn_origl ) != WN_PRAGMA_PREAMBLE_END )
      break;
    // else fall through

  fb_opr_cases_invoke:
    {
      FB_Info_Invoke fb_info_origl = feedback_origl->Query_invoke( wn_origl );
      FB_Info_Invoke fb_info_clone( fb_info_origl.freq_invoke * freq_scale );
      fb_info_origl.freq_invoke -= fb_info_clone.freq_invoke;
      feedback_origl->Annot_invoke( wn_origl, fb_info_origl );
      feedback_clone->Annot_invoke( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_branch:
    {
      FB_Info_Branch fb_info_origl = feedback_origl->Query_branch( wn_origl );
      FB_Info_Branch fb_info_clone( fb_info_origl.freq_taken     * freq_scale,
				    fb_info_origl.freq_not_taken * freq_scale );
      fb_info_origl.freq_taken     -= fb_info_clone.freq_taken    ;
      fb_info_origl.freq_not_taken -= fb_info_clone.freq_not_taken;
      feedback_origl->Annot_branch( wn_origl, fb_info_origl );
      feedback_clone->Annot_branch( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_loop:
    {
      FB_Info_Loop fb_info_origl = feedback_origl->Query_loop( wn_origl );
      FB_Info_Loop fb_info_clone( fb_info_origl.freq_zero     * freq_scale,
				  fb_info_origl.freq_positive * freq_scale,
				  fb_info_origl.freq_out      * freq_scale,
				  fb_info_origl.freq_back     * freq_scale,
				  fb_info_origl.freq_exit     * freq_scale,
				  fb_info_origl.freq_iterate  * freq_scale );
      fb_info_origl.freq_zero     -= fb_info_clone.freq_zero    ;
      fb_info_origl.freq_positive -= fb_info_clone.freq_positive;
      fb_info_origl.freq_out      -= fb_info_clone.freq_out     ;
      fb_info_origl.freq_back     -= fb_info_clone.freq_back    ;
      fb_info_origl.freq_exit     -= fb_info_clone.freq_exit    ;
      fb_info_origl.freq_iterate  -= fb_info_clone.freq_iterate ;
      feedback_origl->Annot_loop( wn_origl, fb_info_origl );
      feedback_clone->Annot_loop( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_circuit:
    {
      FB_Info_Circuit
	fb_info_origl = feedback_origl->Query_circuit( wn_origl );
      FB_Info_Circuit
	fb_info_clone( fb_info_origl.freq_left    * freq_scale,
		       fb_info_origl.freq_right   * freq_scale,
		       fb_info_origl.freq_neither * freq_scale );
      fb_info_origl.freq_left    -= fb_info_clone.freq_left   ;
      fb_info_origl.freq_right   -= fb_info_clone.freq_right  ;
      fb_info_origl.freq_neither -= fb_info_clone.freq_neither;
      feedback_origl->Annot_circuit( wn_origl, fb_info_origl );
      feedback_clone->Annot_circuit( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_call:
    {
      FB_Info_Call fb_info_origl = feedback_origl->Query_call( wn_origl );
      FB_Info_Call fb_info_clone( fb_info_origl.freq_entry * freq_scale,
				  fb_info_origl.freq_exit  * freq_scale,
                                  fb_info_origl.in_out_same );
      fb_info_origl.freq_entry -= fb_info_clone.freq_entry;
      fb_info_origl.freq_exit  -= fb_info_clone.freq_exit;
      feedback_origl->Annot_call( wn_origl, fb_info_origl );
      feedback_clone->Annot_call( wn_clone, fb_info_clone );
    }
    break;

  fb_opr_cases_switch:
    {
      FB_Info_Switch fb_info_origl = feedback_origl->Query_switch( wn_origl );
      FB_Info_Switch fb_info_clone = fb_info_origl;
      for ( INT32 t = fb_info_origl.size() - 1; t >= 0; --t ) {
	fb_info_clone[t] *= freq_scale;
	fb_info_origl[t] -= fb_info_clone[t];
      }
      feedback_origl->Annot_switch( wn_origl, fb_info_origl );
      feedback_clone->Annot_switch( wn_clone, fb_info_clone );
    }
    break;

  default:
    break;
  }
}


void
FB_IPA_Clone( FEEDBACK *feedback_origl, FEEDBACK *feedback_clone,
	      WN             *wn_origl, WN             *wn_clone,
	      FB_FREQ freq_scale )
{
  Is_True( freq_scale.Known() &&
	   freq_scale.Value() >= 0.0 && freq_scale.Value() <= 1.0,
	   ( "FEEDBACK::FB_IPA_Clone: freq_scale == %f",
	     freq_scale.Value() ) );

  if ( feedback_origl == NULL ) {
    if ( freq_scale.Exact() && freq_scale.Zero() )
      feedback_clone->FB_set_zero( wn_clone );
    return;
  }

  for ( TREE_ITER origl( wn_origl ), clone( wn_clone );
	origl.Wn() != NULL && clone.Wn() != NULL;
	++origl, ++clone ) {

    // Clone the node data
    FB_IPA_Clone_node( feedback_origl, feedback_clone,
		       origl.Wn(),     clone.Wn(),     freq_scale );
  }
}


void
FB_IPA_Inline( FEEDBACK *feedback_origl, FEEDBACK *feedback_inlin,
	       WN             *wn_origl, WN             *wn_inlin,
	       FB_FREQ freq_scale )
{
  Is_True( WN_operator( wn_origl ) == OPR_FUNC_ENTRY,
	   ( "FEEDBACK::FB_IPA_Inline: wn_origl should be FUNC_ENTRY" ) );

  FB_FREQ freq_origl = FB_FREQ( 1, true ) - freq_scale;

  WN *func_body = WN_func_body( wn_origl );

  if ( feedback_origl ) {

    // Scale the FUNC_ENTRY
    FB_Info_Invoke fb_info = feedback_origl->Query_invoke( wn_origl );
    fb_info.freq_invoke *= freq_origl;
    feedback_origl->Annot_invoke( wn_origl, fb_info );

    // Scale kids other than function body
    for ( INT32 t = 0; t < WN_kid_count( wn_origl ); ++t )
      if ( WN_kid( wn_origl, t ) != func_body )
	feedback_origl->FB_scale( WN_kid( wn_origl, t ), freq_origl );
  }

  // Clone the function body
  FB_IPA_Clone( feedback_origl, feedback_inlin,
		func_body, wn_inlin, freq_scale );
}


// ====================================================================
// Transfer FB from one PU's FEEDBACK object to another, for MP lowerer
// ====================================================================
      
void
FB_Transfer_node(FEEDBACK *feedback_origl, FEEDBACK *feedback_new, WN *wn)
{
  switch( WN_operator( wn ) ) {

  case OPR_PRAGMA:
    if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
      break;  // process only WN_PRAGMA_PREAMBLE_END

  fb_opr_cases_invoke:
    feedback_new->Annot_invoke(wn, feedback_origl->Query_invoke(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_branch:
    feedback_new->Annot_branch(wn, feedback_origl->Query_branch(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_loop:
    feedback_new->Annot_loop(wn, feedback_origl->Query_loop(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_circuit:
    feedback_new->Annot_circuit(wn, feedback_origl->Query_circuit(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_call:
    feedback_new->Annot_call(wn, feedback_origl->Query_call(wn));
    feedback_origl->Delete(wn);
    break;

  fb_opr_cases_switch:
    feedback_new->Annot_switch(wn, feedback_origl->Query_switch(wn));
    feedback_origl->Delete(wn);
    break;

  default:
    break;
  }
} // FB_Transfer_node


void
FB_Transfer(FEEDBACK *feedback_origl, FEEDBACK *feedback_new, WN *wn)
{
  Is_True(feedback_origl, ("NULL feedback_origl"));
  Is_True(feedback_new, ("NULL feedback_new"));
  Is_True(wn, ("NULL wn"));

  for ( TREE_ITER it (wn) ; it.Wn() != NULL; ++it ) {
    FB_Transfer_node(feedback_origl, feedback_new, it.Wn());
  }

} // FB_Transfer

// for interactive debugging, call this from dbx
void dump_fb ( const FEEDBACK *feedback, const WN *wn )
{
  feedback->Print( stdout, wn );
  fflush(stdout);
}


// ====================================================================
// Verifier
// ====================================================================

FB_VERIFY_STATUS
FEEDBACK::Verify( const char *caller, bool abort_if_error ) const
{
  Is_True( this != NULL, ( "FEEDBACK::Verify encountered NULL FEEDBACK" ) );

  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::Verify (%s)\n", caller );
    fdump_tree_with_freq( TFile, _root_wn, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  FB_CFG fb_cfg;
  fb_cfg.Construct_from_whirl( _root_wn, caller );
  FB_VERIFY_STATUS fb_status = fb_cfg.Verify_frequencies();
  fb_cfg.Patch_whirl_frequencies();

  switch( fb_status ) {
  case FB_VERIFY_CONSISTENT:
    break;
  case FB_VERIFY_UNBALANCED:
    DevWarn("Feedback unbalanced %s", caller );
    break;
  case FB_VERIFY_INVALID:
    // nenad: temporarily replace DevWarn with Is_True to catch more errors
    if (PU_has_exc_scopes(Get_Current_PU()))
      DevWarn( "Feedback invalid %s", caller );
    else
      Is_True( FALSE, ( "Feedback invalid %s", caller ) );
    break;
  default:
    Is_True( false, ( "FEEDBACK::Verify found unexpected branch" ) );
    break;
  }

  return fb_status;
}


FB_VERIFY_STATUS
FEEDBACK::Verify_and_guess( const char *caller, bool abort_if_error ) const
{
  Is_True( this != NULL,
	   ( "FEEDBACK::Verify_and_guess encountered NULL FEEDBACK" ) );

  if ( _trace ) {
    fprintf( TFile, "\n===== FEEDBACK::Verify_and_guess (%s)\n", caller );
    fdump_tree_with_freq( TFile, _root_wn, WN_MAP_FEEDBACK );
    fprintf( TFile, "\n" );
  }

  FB_CFG fb_cfg;
  fb_cfg.Construct_from_whirl( _root_wn, caller );
  fb_cfg.Guess_unknowns( _root_wn, caller );
  FB_VERIFY_STATUS fb_status = fb_cfg.Verify_frequencies();
  fb_cfg.Patch_whirl_frequencies();

  return fb_status;
}


// ====================================================================
// Convert the Feedback info from the internal form to the file format
// ====================================================================

INT
Convert_Feedback_Info (const FEEDBACK* fb, const WN* tree,
		       PU_Profile_Handle& pu_handle)
{
  Is_True (fb != NULL && tree != NULL,
	   ("Convert_Feedback_Info: invalid FEEDBACK* or WN* "));
  
  INT count = 0;
  for ( CONST_TREE_ITER iter (tree); iter.Wn () != NULL; ++iter ) {
    const WN* wn = iter.Wn ();

    switch ( WN_operator( wn ) ) {

    case OPR_PRAGMA:
      if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
	break;
      // else fall through

    fb_opr_cases_invoke:
      pu_handle.Get_Invoke_Table ().push_back (fb->Query_invoke (wn));
      ++count;
      break;

    fb_opr_cases_branch:
      pu_handle.Get_Branch_Table ().push_back (fb->Query_branch (wn));
      ++count;
      break;

    fb_opr_cases_loop:
      pu_handle.Get_Loop_Table ().push_back (fb->Query_loop (wn));
      ++count;
      break;

    fb_opr_cases_circuit:
      pu_handle.Get_Short_Circuit_Table ().push_back (fb->Query_circuit (wn));
      ++count;
      break;

    fb_opr_cases_call:
      pu_handle.Get_Call_Table ().push_back (fb->Query_call (wn));
      ++count;
      break;

    fb_opr_cases_switch:
      pu_handle.Get_Switch_Table ().push_back (fb->Query_switch (wn));
      ++count;
      break;

    default:
      break;
    }
  }
  return count;
} // Convert_Feedback_Info


// Convert the Feedback info from the file buffer to internal format
void
Read_Feedback_Info (FEEDBACK* fb, WN* tree, const Pu_Hdr& pu_hdr)
{
  Is_True (fb != NULL && tree != NULL,
	   ("Read_Feedback_Info: invalid FEEDBACK* or WN* "));

  const char * baseaddr = (const char *) (&pu_hdr);
  const FB_Info_Invoke* fb_invoke =
    (const FB_Info_Invoke*) (baseaddr + pu_hdr.pu_inv_offset);
  const FB_Info_Invoke* fb_invoke_last = fb_invoke + pu_hdr.pu_num_inv_entries;

  const FB_Info_Branch* fb_branch =
    (const FB_Info_Branch*) (baseaddr + pu_hdr.pu_br_offset);
  const FB_Info_Branch* fb_branch_last = fb_branch + pu_hdr.pu_num_br_entries;

  const FB_FREQ* fb_switch =
    (const FB_FREQ*) (baseaddr + pu_hdr.pu_switch_offset);
  const INT32* fb_switch_target =
    (const INT32*) (baseaddr + pu_hdr.pu_switch_target_offset);
  const INT32* fb_switch_target_last =
      fb_switch_target + pu_hdr.pu_num_switch_entries;

  const FB_Info_Loop* fb_loop =
    (const FB_Info_Loop*) (baseaddr + pu_hdr.pu_loop_offset);
  const FB_Info_Loop* fb_loop_last = fb_loop + pu_hdr.pu_num_loop_entries;

  const FB_Info_Circuit* fb_circuit =
    (const FB_Info_Circuit*) (baseaddr + pu_hdr.pu_scircuit_offset);
  const FB_Info_Circuit* fb_circuit_last =
    fb_circuit + pu_hdr.pu_num_scircuit_entries;

  const FB_Info_Call* fb_call =
    (const FB_Info_Call*) (baseaddr + pu_hdr.pu_call_offset);
  const FB_Info_Call* fb_call_last = fb_call + pu_hdr.pu_num_call_entries;

  INT count = 0;
  
  for ( TREE_ITER iter (tree); iter.Wn () != NULL; ++iter ) {
    WN* wn = iter.Wn ();

    switch ( WN_operator( wn ) ) {

    case OPR_PRAGMA:
      if ( WN_pragma( wn ) != WN_PRAGMA_PREAMBLE_END )
	break;
      // else fall through

    fb_opr_cases_invoke:
      fb->Annot_invoke( wn, *fb_invoke );
      ++fb_invoke;
      ++count;
      break;

    fb_opr_cases_branch:
      fb->Annot_branch( wn, *fb_branch );
      ++fb_branch;
      ++count;
      break;

    fb_opr_cases_loop:
      fb->Annot_loop( wn, *fb_loop );
      ++fb_loop;
      ++count;
      break;

    fb_opr_cases_circuit:
      fb->Annot_circuit( wn, *fb_circuit );
      ++fb_circuit;
      ++count;
      break;

    fb_opr_cases_call:
      fb->Annot_call( wn, *fb_call );
      ++fb_call;
      ++count;
      break;

    fb_opr_cases_switch:
      {
	FB_Info_Switch info;
	info.freq_targets.insert( info.freq_targets.begin (),
				  fb_switch, fb_switch + *fb_switch_target );
	fb->Annot_switch( wn, info );
	fb_switch += *fb_switch_target;
	++fb_switch_target;
	++count;
      }
      break;

    default:
      break;
    }
  }

  if ( !(count == pu_hdr.pu_checksum &&
	 fb_invoke == fb_invoke_last &&
	 fb_branch == fb_branch_last &&
	 fb_switch_target == fb_switch_target_last &&
	 fb_loop == fb_loop_last &&
	 fb_circuit == fb_circuit_last &&
	 fb_call == fb_call_last)) {
    ErrMsg (EC_FB_File_Old, Cur_PU_Name);
  }
	   
} // Read_Feedback_Info

#if 1
#include <sys/types.h>
#include <elf.h>
#include <ctype.h>
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include <stdio.h>
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "dep_graph.h"
#include "prompf.h"
#include "ir_reader.h"
#include "wb_util.h"
#include "wb_buffer.h"
#include "wb_carray.h"
#include "wb_browser.h"
#include <wb.h>

void
wb_gwe(WN *wn_root)
{
  WB_BROWSER   wb;

  WB_Set_Phase( WBP_LOWER );
  WB_Initialize( &wb, wn_root, &Get_Current_PU(), NULL, NULL, -1 );
  wb.Sdebug( "" );
  WB_Set_Phase( WBP_NONE ); 
  WB_Terminate( &wb ); 
}
#endif
