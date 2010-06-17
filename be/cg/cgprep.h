
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: cgprep.h
 *  $Revision: 1.38 $
 *  $Date: 2000/10/05 21:16:06 $
 *  $Author: lesniak $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cgprep.h,v $
 *
 *  Revision comments:
 *
 *  4-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  CG pre-scheduling analysis and transformation module.  For details
 *  see doc/Mongoose/cgprep.txt.  Quick summary of what this module
 *  provides:
 *	- dependence graph builder (including loop-carried deps)
 *	  (implemented separately - see "cg_dep_graph.h")
 *	- renames TNs such that each TN defined at most once per BB
 *  general optimizations:
 *	- if-conversion
 *	- madd generation
 *	- FP divide generation
 *  loop optimizations:
 *	- inter- and intra-iteration read/write elimination
 *	- cross-iteration CSE
 *  innermost loop optimizations:
 *	- unrolling
 *	- recurrence breaking
 *
 *  Interface:
 *
 *	void CGPREP_Init()
 *	  Initialization routine that should be called at the start
 *	  of each invocation of CG.
 *
 *	void CGPREP_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform for scheduling the region specified by
 *	  <rid>, or the whole PU if <rid> is NULL.
 *
 *    The following routines can be called only within CGPREP (i.e.,
 *    after CGPREP_Process_Region has been invoked on the current
 *    compilation unit):
 *
 *	void CGPREP_Init_Op(OP *op)
 *	  Initialize CGPREP data structures for newly-created <op>.
 *
 *	INT16 CGPREP_Same_Res_Opnd(OP *op, INT res_idx)
 *	  Requires: OP_same_res(op) 
 *	  Return the operand number for the operand of <op> that can
 *	  be the same as result <res_idx>, or -1 if no operand qualifies.
 *
 *	BOOL CGPREP_Fix_Same_Res_Ops(BB *bb, BOOL assign_tns)
 *	  Requires: CG dep graph and global liveness info up-to-date
 *		    <assign_tns> == FALSE || <bb> isn't cyclic
 *	  If there are any OPs in <bb> for which OP_same_res(op) is TRUE,
 *	  assign the same TN to the result and one of the (appropriate)
 *	  source TNs, introducing a copy before the OP if necessary.
 *	  Actually assign the same TN to the result and operand only if
 *	  <assign_tns> is TRUE, otherwise just make sure it's possible to
 *	  assign them this way (by inserting copies when necessary).
 *	  Return TRUE if any code was modified, or FALSE otherwise.
 *
 *	TN *CGPREP_Dup_TN(TN *old_tn)
 *	  Return a TN equivalent to <old_tn>, except that it has a new
 *	  TN_number.  This works even for dedicated TNs, and differs from
 *	  Dup_TN_Even_If_Dedicated because it doesn't reset the flag
 *	  for TN_is_dedicated or the register assignment for dedicated TNs.
 *
 *	void CGPREP_Copy_TN(TN *dest, TN *src, OP *point, UINT8 omega,
 *			    BOOL before)
 *	  Insert a copy from <src>[<omega>] to <dest>, either just
 *	  before <point> if <before> is TRUE, or just after <point>
 *	  otherwise.
 *
 *	void CGPREP_Copy_TN_Into_BB(TN *dest, TN *src, BB *bb, OP *point,
 *				    UINT8 omega, BOOL before)
 *	  Insert a copy from <src>[<omega>] to <dest>.
 *        Arguments are the same as in CGPREP_Copy_TN with the addition of 
 *        BB *bb. This can be used in cases where there may be no OPs
 *        in the BB yet.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef CGPREP_INCLUDED
#define CGPREP_INCLUDED


#include "tn_map.h"
#include "region_util.h"

void CGPREP_Init(void);

void CGPREP_Process_Region(RID *rid);

extern BOOL CGPREP_combine_invariants;
extern BOOL CGPREP_remove_dead_code;
extern BOOL CGPREP_create_madds;
extern BOOL CGPREP_remove_dead_stores;
extern BOOL CGPREP_remove_copies;
extern BOOL CGPREP_propagate_fpu_int;
extern BOOL CGPREP_change_to_copy;
extern BOOL CGPREP_fold_constants;
extern BOOL CGPREP_optimize_all_bbs;
extern BOOL CGPREP_optimize_lno_winddown_cache;
extern BOOL CGPREP_optimize_lno_winddown_reg;
extern BOOL CGPREP_optimize_non_innermost;
extern BOOL CGPREP_optimize_multi_targ;
extern BOOL CGPREP_optimize_non_trip_countable;
extern BOOL CGPREP_optimize_swp_bbs;
extern BOOL CGPREP_force_copy_before_select;
extern BOOL CGPREP_fold_expanded_daddiu;
extern BOOL CGPEEP_remove_dead_code;
extern BOOL CGPREP_choose_same_res_opnd_carefully;
extern BOOL CGPREP_remove_guard_branch;
extern BOOL CGPREP_guard_branch_aggressive;
extern BOOL CGPREP_guard_branch_unsigned;
extern BOOL CGPREP_skip_local;

extern BOOL CGPEEP_skip_local;

void CGPREP_Copy_TN(TN *dest, TN *src, OP *point, UINT8 omega, BOOL before);
void CGPREP_Copy_TN_Into_BB(TN *dest, TN *src, BB *bb, OP *point, UINT8 omega, BOOL before);

void CGPREP_dead_code_copy_removal(BB *bb);

INT16 CGPREP_Same_Res_Opnd(OP *op, INT res_idx);
BOOL CGPREP_Fix_Same_Res_Ops(BB *bb, BOOL assign_tns);

void CGPREP_Init_Op(OP *op);

TN * CGPREP_Dup_TN(TN *old_tn);

#endif /* CGPREP_INCLUDED */
