
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
 *  Module: cgprep.cxx
 *  $Revision: 1.337 $
 *  $Date: 2000/10/05 21:16:04 $
 *  $Author: lesniak $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cgprep.cxx,v $
 *
 *  Revision comments:
 *
 *  6-Apr-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 *  CG pre-scheduling analysis and transformation module.  For details
 *  see doc/Mongoose/cgprep.txt.  "cgprep.h" contains interface spec.
 *
 * =======================================================================
 * =======================================================================
 */


#include <stdarg.h>
#include "defs.h"
#include "errors.h"
#include "mempool.h"
#include "wn.h"
#include "opcode.h"
#include "w2op.h"
#include "cgir.h"
#include "tn_map.h"
#include "register.h"
#include "op_list.h"
#include "cg_loop.h"
#include "config.h"
#include "config_targ_options.h"
#include "cg.h"
#include "tracing.h"
#include "timing.h"
#include "cgexp.h"
#include "cgtarget.h"

#include "cio.h"
#include "cio_rwtran.h"
#include "cg_dflow.h"
#include "cg_cflow.h"
#include "resource.h"
#include "loopanal.h"
#include "cg_db_op.h"
#include "cg_flags.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "bb_set.h"
#include "dominate.h"
#include "wn.h"
#include "wn_map.h"
#include "whirl2ops.h"
#include "gra_live.h"
#include "reg_live.h"
#include "cflow.h"
#include "findloops.h"
#include "cg_loop.h"
#include "cg_loop_scc.h"
#include "cg_loop_mii.h"
#include "cg_loop_recur.h"
#include "findloops.h"
#include "freq.h"
#include "cg_sched_est.h"
#include "label_util.h"
#include "cg_swp.h"
#include "cgprep.h"
#include "cg_region.h"

BOOL CGPREP_combine_invariants = TRUE;
BOOL CGPREP_remove_dead_code = TRUE;
BOOL CGPREP_remove_dead_stores = TRUE;
BOOL CGPREP_create_madds = TRUE;
BOOL CGPREP_change_to_copy = TRUE;
BOOL CGPREP_remove_copies = TRUE;
BOOL CGPREP_propagate_fpu_int = TRUE;
BOOL CGPREP_fold_constants = TRUE;
BOOL CGPREP_optimize_swp_bbs = FALSE;
BOOL CGPREP_optimize_non_trip_countable = TRUE;
BOOL CGPREP_optimize_non_innermost = TRUE;
BOOL CGPREP_optimize_multi_targ = FALSE;
BOOL CGPREP_optimize_lno_winddown_cache = TRUE;
BOOL CGPREP_optimize_lno_winddown_reg = TRUE;
BOOL CGPREP_force_copy_before_select = FALSE;
BOOL CGPREP_opt_cflow_before = TRUE;
BOOL CGPREP_opt_cflow_after = TRUE;
BOOL CGPREP_fold_expanded_daddiu = FALSE;	/* see cgdriver.c */
BOOL CGPREP_choose_same_res_opnd_carefully = TRUE;
BOOL CGPREP_remove_guard_branch = TRUE;
BOOL CGPREP_guard_branch_aggressive = TRUE;
BOOL CGPREP_guard_branch_unsigned = TRUE;
BOOL CGPEEP_remove_dead_code = TRUE;
BOOL CGPREP_skip_local = FALSE;
BOOL CGPEEP_skip_local = FALSE;


void CGPREP_Init(void)
/* -----------------------------------------------------------------------
 * See "cgprep.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_Init();
}


void CGPREP_Init_Op(OP * /* op */)
/* -----------------------------------------------------------------------
 * See "cgprep.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  /* nothing to do (yet) */
}


TN *CGPREP_Dup_TN(TN *old_tn)
/* -----------------------------------------------------------------------
 * See "cgprep.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  if (TN_is_dedicated(old_tn)) return old_tn;

  TN *new_tn = Dup_TN_Even_If_Dedicated(old_tn);
  if (TN_is_dedicated(old_tn)) {
    Set_TN_is_dedicated(new_tn);
    Set_TN_register_and_class(new_tn, TN_register_and_class(old_tn));
  }
  return new_tn;
}


static BOOL use_crosses_def(OP *op, UINT8 opnd)
/* -----------------------------------------------------------------------
 * Requires: CG dep graph up-to-date
 *
 * Return TRUE iff some path from the definition of OP_opnd(op,opnd) to
 * a local use crosses the definition.
 * -----------------------------------------------------------------------
 */
{
  if (CG_DEP_Graph_Is_Cyclic(OP_bb(op))) {
    ARC *arc;
    if (OP_omega(op, opnd) > 1) return TRUE;
    if (arc = ARC_LIST_Find_First(OP_preds(op), CG_DEP_REGIN, opnd)) {
      OP *def = ARC_pred(arc);
      ARC_LIST *uses = ARC_LIST_Find(OP_succs(def), CG_DEP_REGIN, DONT_CARE);
      while (uses) {
        ARC *use = ARC_LIST_first(uses);
	UINT8 om = ARC_omega(use);
        if (om > 1 || om == 1 && OP_Precedes(def, ARC_succ(use)))
          return TRUE;
        uses = ARC_LIST_Find(ARC_LIST_rest(uses), CG_DEP_REGIN, DONT_CARE);
      }
    }
  }
  return FALSE;
}

INT16 CGPREP_Same_Res_Opnd(OP *op, INT res_idx)
/* ----------------------------------------------------------------------
 * See "cgprep.h" for interface specification.
 * ----------------------------------------------------------------------
 */
{
  Is_True(OP_same_res(op), ("op not same-res OP"));
  Is_True(OP_results(op) > res_idx,
	    ("Can't access result %d, OP_same_res OP has %d operands",
	     res_idx, OP_results(op)));

  TN *res = OP_result(op, res_idx);

  /* Search operands for one equivalent to 'res. */
  for (INT i = 0; i < OP_opnds(op); ++i)
    {
      if (res == OP_opnd(op, i))
	return i;
    }

  return -1;
}

void CGPREP_Copy_TN(TN *dest, TN *src, OP *point, UINT8 omega, BOOL before)
/* -----------------------------------------------------------------------
 * See "cgprep.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  OPS ops = OPS_EMPTY;
  OP *op;

  Exp_COPY(dest,src,&ops);

  FOR_ALL_OPS_OPs(&ops, op) {
    CGPREP_Init_Op(op);
    if (omega > 0 || Is_CG_LOOP_Op(point)) {
      UINT8 opnd;
      CG_LOOP_Init_Op(op);
      for (opnd=0; opnd < OP_opnds(op); opnd++) {
        if (OP_opnd(op,opnd) == src)
          Set_OP_omega(op, opnd, omega);
      }
    }
  }

  BB_Insert_Ops(OP_bb(point), point, &ops, before);
}


void CGPREP_Copy_TN_Into_BB(TN *dest, TN *src, BB *bb, OP *point, UINT8 omega,
                            BOOL before)
/* -----------------------------------------------------------------------
 * See "cgprep.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  OPS ops = OPS_EMPTY;
  OP *op;

  FmtAssert(bb || point, ("called with BB *bb == NULL and OP *point == NULL"));

  Exp_COPY(dest,src,&ops);

  FOR_ALL_OPS_OPs(&ops, op) {
    CGPREP_Init_Op(op);
    if (omega > 0 || point && Is_CG_LOOP_Op(point) ||
        bb && BB_first_op(bb) && Is_CG_LOOP_Op(BB_first_op(bb))) {
      UINT8 opnd;
      CG_LOOP_Init_Op(op);
      for (opnd=0; opnd < OP_opnds(op); opnd++) {
        if (OP_opnd(op,opnd) == src)
          Set_OP_omega(op, opnd, omega);
      }
    }
  }

  BB_Insert_Ops(bb, point, &ops, before);
}


void CGPREP_Process_Region(RID *rid)
// -----------------------------------------------------------------------
// See "cgprep.h" for interface description.
// -----------------------------------------------------------------------
//
{
  if (!CG_localize_tns)
  {
    // Optimize loops (mostly innermost)
    if (CG_opt_level > 1)
      Perform_Loop_Optimizations();

#ifdef TARG_XTENSA
    /* Show that trip count tns should no longer be considered live,
       unless they are explicitly referenced in the loop. */
    GRA_LIVE_include_trip_count = FALSE;
#endif
    
    GRA_LIVE_Recalc_Liveness(rid);  // detect GTN
    GRA_LIVE_Rename_TNs();  // rename TNs -- required by LRA
  }

  if (Get_Trace(TP_CGPREP, 0x4000000)) {
    fprintf ( TFile, "%s After CGPREP: \n%s", DBar, DBar );
    Print_All_BBs();
  }
}



