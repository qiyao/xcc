
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


//-*-c++-*-
/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_loop.cxx
 *  $Revision: 1.360 $
 *  $Date: 2000/10/13 19:10:12 $
 *  $Author: lesniak $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cg_loop.cxx,v $
 *
 *
 * =======================================================================
 * ======================================================================= */

/*
  
  The CG loop analysis and transformation module. 
  
  This file contains the main driver of all CG loop optimizations.  The
  loop optimizations involved are:
  
    - scalar variable renaming
    - vector read/write removal
    - induction variable removal
    - recurrence breaking (or height reduction)
    - predicate promotion
    - unrolling 
    - prefetch pruning and generation
    - various preconditioning to make modulo scheduling more effectively
    - modulo scheduling
    - rotating register allocation
  
  The IA-64 loop optimizer is developed by the team of Raymond Lo, Rune
  Dahl, and David Stephenson during the period from Apr 1999 to Feb
  2000.
  
  The cyclic intermediate representation is carried over from the
  Mongoose compiler, along with most of the utility functions.  Several
  key components are newly developed for the IA-64 architecture, namely,
  the modulo scheduler, the rotating register allocator, recurrence
  breaking, vector read/write removal, and the unrolling heuristics.
  Other optimizations are modified significantly to support the
  predicated instruction set.
  
  The modulo scheduler is based on "Lifetime-Sensitive Modulo
  Scheduling", by Richard Huff, in Programming Language Design and
  Implementation. SIGPLAN, 1993.
  
  The rotating register allocator is based on "Register Allocation for
  Software Pipelined Loops", by B. R. Rau, M. Lee, P. P. Tirumalai,
  M. S. Schlansker, in ACM SIGPLAN '92 PLDI-6/92/CA.  Some idea of the
  paper was conceived by Ross Towle and Jim Dehnert when they're working
  on the Cydra 5 compiler.
  
  The basic idea of the recurrence breaking and vector read/write
  removal can be found in "Compiling for the Cydra 5", by Jim Dehnert,
  Ross Towle, in the Journal of Supercomputing, 7, 181-227 (1993).
  
  The relatively straightforward part of the loop optimizer is the
  modulo scheduler and the register allocator because there have been
  many years of compiler researches available.  The modulo scheduler and
  register allocator almost find the optimal schedules and
  close-to-optimal allocations, right after their initial
  implementation.  The more complicated part of the loop optimizer is
  the preconditioning of a loop for modulo scheduling.  The throughput
  of a modulo scheduled loop is bound by MII=max(RecMII, ResMII).  The
  objective of preconditioning is to lower the RecMII and/or the ResMII
  by various means.  One example is to apply recurrence height reduction
  to decrease RecMII such that RecMII is no longer the dominant factor
  in determining MII.  Sometimes extra resources, at the expense of
  ResMII, can be consumed to further reduce the recurrence height if the
  final MII is reduced.  Another example is to use unrolling to fill all
  slots of VLIW instructions to improve ResMII.  Sometimes instructions
  can be combined after unrolling.  The difficulty is that unrolling,
  recurrence height reduction, and other optimizations interacts with
  each other because they are changing the number and mix of
  instructions at the same time.  An optimization that seems to be
  profitable might be not after the loop is unrolled.  Therefore, the
  final phases of the loop optimizer development will focus mostly on
  the fine-tuning of the interactions among optimizations.
   
*/

#include "defs.h"

#include <math.h>
#include <stdarg.h>
#include <float.h>
#include <set>
#include <vector>

using std::set;
using std::vector;
using std::pair;

#include "config.h"
#include "errors.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "cg_flags.h"
#include "cgir.h"
#include "tracing.h"
#include "config_asm.h"
#include "config_targ_options.h"
#include "tn_map.h"
#include "cio_rwtran.h"
#include "cgprep.h"
#include "op.h"
#include "op_list.h"
#include "bb.h"
#include "cgtarget.h"
#include "cg_swp_target.h"
#include "wn.h"
#include "wn_core.h"
#include "wn_util.h"
#include "wn_simp.h"
#include "whirl2ops.h"
#include "pu_info.h"
#include "cg_fusion.h"
#include "data_layout.h"

#include "register.h"	/* needed for "gra.h" */
#include "tn_set.h"	/* needed for "gra.h" */
#include "gra.h"
#include "gra_live.h"
#include "bb_set.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "pf_cg.h"
#include "note.h"
#include "cgexp.h"
#include "cio.h"
#include "cio_rwtran.h"
#include "cg_dflow.h"
#include "cg_cflow.h"
#include "resource.h"
#include "loopanal.h"
#include "cg_ir.h"
#include "cg_db_op.h"
#include "dominate.h"
#include "ti_res_count.h"
#include "cg_loop_scc.h"
#include "cg_loop_recur.h"
#include "cg_loop_mii.h"
#include "freq.h"
#include "cg_region.h"
#include "cg_sched_est.h"
#include "cg_loop.h"
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "findloops.h"
#include "dominate.h"
#include "ebo.h"
#include "hb.h"
#include "gra_live.h"
#include "gtn_tn_set.h"
#include "cg_updating.h"
#include "instr_reader.h"
#include "cg_autotie.h"
#include "fb_whirl.h"

/* Error tolerance for feedback-based frequency info */
#define FB_TOL 0.05

CG_LOOP *Current_CG_LOOP;

BB *CG_LOOP_prolog;
BB *CG_LOOP_epilog;
BB *CG_LOOP_prolog_start;
BB *CG_LOOP_epilog_end;

OP_MAP _CG_LOOP_info_map;
  
BOOL CG_LOOP_unroll_fully = TRUE;
BOOL CG_LOOP_unroll_remainder_fully = FALSE;
UINT32 CG_LOOP_unroll_min_trip = 5;

#ifdef MIPS_UNROLL
BOOL CG_LOOP_unroll_analysis = TRUE;
#else
BOOL CG_LOOP_unroll_analysis = FALSE;
#endif
BOOL CG_LOOP_multi_bb_unroll_analysis = TRUE;
UINT32 CG_LOOP_unroll_analysis_threshold = 10;
BOOL CG_LOOP_ooo_unroll_heuristics = FALSE;
BOOL CG_LOOP_ooo_unroll_heuristics_set = FALSE;
UINT32 CG_LOOP_reorder_buffer_size = 16;
UINT32 CG_LOOP_cache_miss_threshold = 33;

BOOL CG_LOOP_unroll_multi_bb = TRUE;
BOOL CG_LOOP_unroll_non_trip_countable = TRUE;
BOOL CG_LOOP_unroll_multi_make_remainder_loop = TRUE;
BOOL CG_LOOP_create_loop_prologs;	/* see Configure_CG_Options() */
BOOL CG_LOOP_create_loop_epilogs = TRUE;
INT32 CG_LOOP_force_ifc = 1;

UINT32 CG_LOOP_max_zcl_bytes = 250;

/* Note: To set default unroll parameters, modify the initialization
 *	 of OPT_unroll_times/size in "config.c".
 */
UINT32 CG_LOOP_unroll_times_max;
UINT32 CG_LOOP_unrolled_size_max;

INT32 CG_LOOP_skip_after = INT32_MAX;
INT32 CG_LOOP_skip_before = -1;
INT32 CG_LOOP_skip_equal = -1;

static BOOL unroll_names_valid = FALSE;

static CG_LOOP_BACKPATCH *prolog_backpatches, *epilog_backpatches;

static BB_MAP bb_fb_exec_cnts = 0;
static bool has_fb_freq = false;
#define CGLOOP_get_bb_fb_exec_cnt(bb) ((has_fb_freq) ?			\
			(BB_MAPFP_Get(bb_fb_exec_cnts, (bb))) : 0)
#define CGLOOP_set_bb_fb_exec_cnt(bb,cnt) if (has_fb_freq) 		\
				BB_MAPFP_Set(bb_fb_exec_cnts, (bb), (cnt))
#define CGLOOP_copy_bb_fb_exec_cnt(new_bb,old_bb) if (has_fb_freq)	 \
				BB_MAPFP_Set(bb_fb_exec_cnts, (new_bb), \
				BB_MAPFP_Get(bb_fb_exec_cnts, (old_bb)))

static void Feedback_Freq_Init ();
static void Feedback_Freq_Finish ();

BOOL Remove_Guard_Branch (BB *prolog, TN *trip_count_tn);

/* ---------------------------------------------------------------------
 *			   Unrolling Notes
 * ---------------------------------------------------------------------
 */

typedef struct {
  mUINT16 ntimes;
  mBOOL const_trip;
} NOTE_REMAINDER_HEAD;
  
typedef struct {
  char *reason;
} NOTE_NOT_UNROLLED;
  

#define NOTE_ntimes(n)		((n)->ntimes)
#define NOTE_const_trip(n)	((n)->const_trip)
#define NOTE_reason(n)		((n)->reason)


static void remainder_head_note_handler(NOTE_ACTION action, NOTE_INFO *info,
					FILE *file);

static void note_remainder_head(BB *head, UINT16 ntimes, UINT16 trips_if_const);

/* Statically check a few assumptions .. */
#if MAX_OMEGA > 255
error OP_omega/Set_OP_omega assume MAX_OMEGA <= 255
#endif

_CG_LOOP_INFO *free_loop_info;
#define next_free_loop_info(info) (*(_CG_LOOP_INFO **)(info))

static void unroll_xfer_annotations(BB *unrolled_bb, BB *orig_bb);

void CG_LOOP_Init(void)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  free_loop_info = NULL;
  // No loop optimizations at -O0 and -O1
  if (CG_opt_level < 2) return;
  unroll_names_valid = FALSE;
}

void CG_LOOP_Init_Op(OP *op)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  _CG_LOOP_INFO *info = free_loop_info;
  INT sizeof_info = _CG_LOOP_info_sizeof(op);
  if (info && OP_opnds(op) <= OP_MAX_FIXED_OPNDS) {

    // Since we don't track the size of the _CG_LOOP_INFO structs on
    // the free list, don't use one for a variable operand OP uses more
    // than OP_MAX_FIXED_OPNDS, instead allocate a new one. We can however 
    // guarantee that all _CG_LOOP_INFO structs allocate are large enough 
    // to handle all fixed operand OPs, so anything on the free list will 
    // do in that case.
    free_loop_info = next_free_loop_info(info);
  } else {
    info = (_CG_LOOP_INFO *)MEM_POOL_Alloc(&MEM_phase_nz_pool, sizeof_info);
  }
  memset(info, 0, sizeof_info);
  OP_MAP_Set(_CG_LOOP_info_map, op, info);
}


void CG_LOOP_Init_OPS(OPS *ops)
{
  for (OP *op = OPS_first(ops); op != NULL; op = OP_next(op)) 
    CG_LOOP_Init_Op(op);
}


INT Branch_Target_Operand(OP *br_op)
/* -----------------------------------------------------------------------
 * Return the branch target operand of <br_op>.
 * -----------------------------------------------------------------------
 */
{
  Is_True(br_op, ("br_op is NULL."));
  INT opnd = OP_find_opnd_use(br_op, OU_target);
  Is_True(opnd >= 0,
	  ("couldn't find branch target for %s", TI_TOP_Name(OP_code(br_op))));
  return opnd;
}


static
BOOL Negate_Branch(OP *br)
/* -----------------------------------------------------------------------
 * Negate the branch <br> and return TRUE successful
 * -----------------------------------------------------------------------
 */
{
  TOP top = OP_code(br);
  TOP new_top = CGTARG_Invert_Branch(top);

  if (new_top != TOP_UNDEFINED) {
    OP_Change_Opcode(br, new_top);
    return TRUE;
  }

  if (OP_has_predicate(br)) {
    OP *cmp;
    TN *tn1, *tn2;
    BB *br_bb = OP_bb(br);
    CGTARG_Analyze_Compare(br, &tn1, &tn2, &cmp);
    if (cmp != NULL && OP_results(cmp) == 2) {
      BB *cmp_bb = OP_bb(cmp);
      TN *r0 = OP_result(cmp,0);
      TN *r1 = OP_result(cmp,1);
      TN *pred = OP_opnd(br,OP_PREDICATE_OPND);
      TN *neg_tn = r0 == pred ? r1 : r0;

      if (neg_tn == True_TN) {
        DevWarn("negative cmp result is a sink");
        return FALSE;
      }

      Set_OP_opnd(br, OP_PREDICATE_OPND, neg_tn);

      if (br_bb != cmp_bb && !CG_localize_tns) {
        GRA_LIVE_Compute_Local_Info(br_bb);
        GRA_LIVE_Region_Start();
        GRA_LIVE_Region_Entry(cmp_bb);
        GRA_LIVE_Region_Exit(br_bb);
        GRA_LIVE_Region_Compute_Global_Live_Info();
      }

      return TRUE;
    }
  }

  return FALSE;
}


static void loop_info_detach(OP *op)
/* -----------------------------------------------------------------------
 * Detach _CG_LOOP_INFO from <op>.
 * -----------------------------------------------------------------------
 */
{
  _CG_LOOP_INFO *info = (_CG_LOOP_INFO *)OP_MAP_Get(_CG_LOOP_info_map, op);
  Is_True(info, ("no loop info to delete"));
  next_free_loop_info(info) = free_loop_info;
  free_loop_info = info;
}

static float sum_succ_probs(BB *bb)
/* -----------------------------------------------------------------------
 * Return the sum of the probabilities on the successor edges from <bb>.
 * (Should be 1.0 in consistent CFG.)
 * -----------------------------------------------------------------------
 */
{
  BBLIST *succs;
  float sum = 0.0;

  FOR_ALL_BB_SUCCS(bb, succs) {
    if (CG_warn_bad_freqs && !BB_freq_fb_based(bb)
	&& BBLIST_prob(succs) == 0.0)
      DevWarn("succ edge from BB:%d to BB:%d has probability 0",
	      BB_id(bb), BB_id(BBLIST_item(succs)));
    sum += BBLIST_prob(succs);
  }
  return sum;
}

static void retarget_loop_exits(LOOP_DESCR *loop, BB *from, BB *to)
/* -----------------------------------------------------------------------
 * Requires: <to> is inserted where intended in BB chain
 *
 * Change all exits from <loop> to <from> so they exit to <to> instead.
 * Updates pred/succ lists and frequency info.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *preds = BB_preds(from);
  while (preds) {
    BB *pred = BBLIST_item(preds);
    preds = BBLIST_next(preds);
    if (BB_SET_MemberP(LOOP_DESCR_bbset(loop), pred))
      if (!BB_Retarget_Branch(pred, from, to))
	/* must fall-through to <from> */
	Change_Succ(pred, from, to);
  }
}

static void insert_fall_thru(BB *pred, BB *new_ftbb)
/* -----------------------------------------------------------------------
 * Requires: BB_Fall_Thru_Successor(pred) != NULL && new_ftbb not in BB chain
 *
 * Inserts <new_ftbb> between <pred> and its old fall-through successor,
 * so that <pred> falls through to <new_ftbb> which falls through to the
 * old fall-through successor.  Updates pred/succ lists and frequency info
 * (if present).
 * -----------------------------------------------------------------------
 */
{
  BOOL freqs = FREQ_Frequencies_Computed();
  BB *old_ftbb = BB_Fall_Thru_Successor(pred);
  float other_succ_probs = 0.0, old_freq;
  OP *new_br_op = BB_branch_op(new_ftbb);

  Is_True(old_ftbb, ("<pred> has no fall-through successor"));
  Is_True(new_br_op == NULL || OP_cond(new_br_op),
	    ("<new_ftbb> BB:%d doesn't fall-through to anything",
	     BB_id(new_ftbb)));
  Is_True(BB_next(new_ftbb) == NULL,
	    ("<new_ftbb> BB:%d has a BB_next", BB_id(new_ftbb)));
  Is_True(BB_prev(new_ftbb) == NULL,
	    ("<new_ftbb> BB:%d has a BB_prev", BB_id(new_ftbb)));
  if (freqs) {
    other_succ_probs = sum_succ_probs(new_ftbb);
    if (CG_warn_bad_freqs && 1.0 - other_succ_probs <= 0.0)
      DevWarn("existing succ edge probabilities from BB:%d total %s1.0",
	      BB_id(new_ftbb), other_succ_probs > 1.0 ? ">" : "");
  } else if (BB_freq_fb_based(old_ftbb)) {
    /* Guess that P(succ) is same for all succs */
    UINT32 len = BBlist_Len(BB_succs(new_ftbb)) + 1;
    other_succ_probs = (len - 1.0) / len;
  }
  if (freqs || BB_freq_fb_based(old_ftbb))
    old_freq = BB_freq(old_ftbb);

  Chain_BBs(pred, new_ftbb);
  Change_Succ(pred, old_ftbb, new_ftbb);
  Chain_BBs(new_ftbb, old_ftbb);
  Link_Pred_Succ_with_Prob(new_ftbb, old_ftbb, 1.0-other_succ_probs);
  if (freqs || BB_freq_fb_based(old_ftbb))
    BB_freq(old_ftbb) = old_freq;
}


inline void append_to_prolog(BB *bb)
/* -----------------------------------------------------------------------
 * Attach <bb> to the end of CG_LOOP_prolog, initializing GRA liveness
 * info and updating pred/succ lists and frequency info.  Upon return,
 * CG_LOOP_prolog is <bb>.
 * -----------------------------------------------------------------------
 */
{
  GRA_LIVE_Compute_Local_Info(bb);
  insert_fall_thru(CG_LOOP_prolog, bb);
  CG_LOOP_prolog = bb;
}



inline void extend_prolog(void)
/* -----------------------------------------------------------------------
 * Insert a new block at the end of CG_LOOP_prolog, and point
 * CG_LOOP_prolog to it.  Also initializes GRA liveness info and
 * updates pred/succ lists and frequency info.
 * -----------------------------------------------------------------------
 */
{
  BB *new_bb = Gen_BB_Like(CG_LOOP_prolog);
  // disable the following since we should let append_to_prolog() update
  // the freqency of new_bb
  // if (BB_freq_fb_based(CG_LOOP_prolog)) Set_BB_freq_fb_based(new_bb);
  // BB_freq(new_bb) = BB_freq(CG_LOOP_prolog);
  CGLOOP_copy_bb_fb_exec_cnt(new_bb,CG_LOOP_prolog);
  append_to_prolog(new_bb);
}



static void prepend_to_epilog(LOOP_DESCR *loop, BB *bb)
/* -----------------------------------------------------------------------
 * Attach <bb> to the start of CG_LOOP_epilog, initializing GRA liveness
 * info and updating pred/succ lists and frequency info.  Upon return,
 * CG_LOOP_epilog is <bb>.  Retargets exits from <loop> to CG_LOOP_epilog
 * to <bb>.
 * -----------------------------------------------------------------------
 */
{
  BB *ftp = BB_Fall_Thru_Predecessor(CG_LOOP_epilog);
  GRA_LIVE_Compute_Local_Info(bb);
  if (ftp) {
    insert_fall_thru(ftp, bb);
  } else {
    Chain_BBs(BB_prev(CG_LOOP_epilog), bb);
    Chain_BBs(bb, CG_LOOP_epilog);
  }
  retarget_loop_exits(loop, CG_LOOP_epilog, bb);
  CG_LOOP_epilog = bb;
}



inline void extend_epilog(LOOP_DESCR *loop)
/* -----------------------------------------------------------------------
 * Insert a new block at the start of CG_LOOP_epilog, and point
 * CG_LOOP_epilog to it.  Initializes GRA liveness info and
 * updates pred/succ lists and frequency info.  Also retargets
 * exits from <loop> to CG_LOOP_epilog to the new BB.
 * -----------------------------------------------------------------------
 */
{
  BB *new_bb = Gen_BB_Like(CG_LOOP_epilog);
  // disable the following since we should let prepend_to_epilog() update
  // the freqency of new_bb
  // if (BB_freq_fb_based(CG_LOOP_epilog)) Set_BB_freq_fb_based(new_bb);
  // BB_freq(new_bb) = BB_freq(CG_LOOP_epilog);
  CGLOOP_copy_bb_fb_exec_cnt(new_bb,CG_LOOP_epilog);
  prepend_to_epilog(loop, new_bb);
}



void CG_LOOP_Trace_Prolog(void)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  BB *bb;
  fprintf(TFile, "<prolog> loop prolog:\n");
  for (bb = CG_LOOP_prolog_start;
       bb && BB_prev(bb) != CG_LOOP_prolog;
       bb = BB_next(bb))
    Print_BB(bb);
}



void CG_LOOP_Trace_Epilog(void)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  BB *bb;
  fprintf(TFile, "<epilog> loop epilog:\n");
  for (bb = CG_LOOP_epilog;
       bb && BB_prev(bb) != CG_LOOP_epilog_end;
       bb = BB_next(bb))
    Print_BB(bb);
}


static BB_SET *
mark_prolog_epilog_bbs(
  BB     *self,
  BB     *limit,
  BB_SET *visited
)
{
  BB        *pred;
  BBLIST    *lst;

  visited = BB_SET_Union1(visited,self,&MEM_local_pool);

  if ( self != limit ) {
    FOR_ALL_BB_PREDS (self, lst) {
      pred = BBLIST_item(lst);
      if ( ! BB_SET_MemberP(visited,pred) ) {
        visited = mark_prolog_epilog_bbs(pred,limit,
					 visited);
      }
    }
  }

  return visited;
}

/* -----------------------------------------------------------------------
 * static void remove_prolog_or_epilog_bbs(BB *head, BB *tail, BB *keep)
 *
 *  Remove all the prolog/epilog BB's between {<head>,<tail>} except <keep>. 
 *
 * -----------------------------------------------------------------------
 */
static void
remove_prolog_or_epilog_bbs(BB *head, BB *tail, BB *keep, BB_SET *inp_visited)
{
  BB *bb;
  BB *bb_next;
  BB_SET *visited;
  BB *pred;
  BB *succ;
  BBLIST *lst;
  BBLIST *lst_next;
  RID *bbrid;

  if ( inp_visited == NULL ) {
    MEM_POOL_Push(&MEM_local_pool);
    visited = BB_SET_Create_Empty(PU_BB_Count+2,&MEM_local_pool);
    visited = mark_prolog_epilog_bbs(tail,head,visited);
  } else {
    visited = inp_visited;
  }

  for (bb = REGION_First_BB; bb; bb = bb_next) {
    bb_next = BB_next(bb);

    if (    ( bbrid = BB_rid( bb ) )
	&& ( RID_level( bbrid ) >= RL_CGSCHED ) )
      /*
       * don't look inside already compiled bb's
       */
      continue;

    if ( bb != keep ) {
      if ( BB_SET_MemberP(visited,bb) ) {
	for ( lst = BB_succs(bb); lst != NULL; lst = lst_next ) {
	  lst_next = BBLIST_next(lst);
	  if (succ = BBLIST_item(lst))
	    Unlink_Pred_Succ(bb, succ);
	}

	for ( lst = BB_preds(bb); lst != NULL; lst = lst_next ) {
	  lst_next = BBLIST_next(lst);
	  if (pred = BBLIST_item(lst))
	    Unlink_Pred_Succ(pred, bb);
	}
	BB_Remove_All(bb);
	Remove_BB(bb);
      }
    }
  }

  if ( inp_visited == NULL )
    MEM_POOL_Pop(&MEM_local_pool);

}


/* -----------------------------------------------------------------------
 * void CG_LOOP_Remove_Prolog_OPs(BB *head)
 *
 *  Remove all the prolog BB's except CG_LOOP_prolog_start, 
 *  Update the BB chain and pred/succ lists to reflect this.
 *  Reset CG_LOOP_prolog to CG_LOOP_prolog_start. Where <head> is the
 *  first DoBodyBB(1) in the loop.
 *
 * -----------------------------------------------------------------------
 */
void CG_LOOP_Remove_Prolog_OPs(BB *head)
{
  BB *succ;
  BBLIST *lst;
  BBLIST *lst_next;
  float prob;

  /* Grab probability for CG_LOOP_prolog=>head edge before the edge
   * gets removed.
   */
  lst = BB_Find_Succ(CG_LOOP_prolog, head);
  if (lst) {
    prob = BBLIST_prob(lst);
  } else {
    Is_True(FALSE, ("couldn't find BB:%d => BB:%d edge",
		      BB_id(CG_LOOP_prolog), BB_id(head)));
    prob = 0;
  }

  remove_prolog_or_epilog_bbs(CG_LOOP_prolog_start, 
			      CG_LOOP_prolog, 
			      CG_LOOP_prolog_start,
			      NULL);

  /* since we are keeping CG_LOOP_prolog_start, we need to clear its ops */
  BB_Remove_All(CG_LOOP_prolog_start);

  Reset_BB_has_glue_copy(CG_LOOP_prolog_start);

  for ( lst = BB_succs(CG_LOOP_prolog_start); lst != NULL; lst = lst_next ) {
    lst_next = BBLIST_next(lst);
    if (succ = BBLIST_item(lst))
      Unlink_Pred_Succ(CG_LOOP_prolog_start, succ);
  }

  Unlink_Pred_Succ(CG_LOOP_prolog, head);
  Link_Pred_Succ_with_Prob(CG_LOOP_prolog_start, head, prob);
  BB_next(CG_LOOP_prolog_start) = head;
  BB_prev(head) = CG_LOOP_prolog_start;

  CG_LOOP_prolog = CG_LOOP_prolog_start;

}

/* -----------------------------------------------------------------------
 * void CG_LOOP_Remove_Epilog_OPs(BB *tail)
 *
 *  Remove all the epilog BB's except CG_LOOP_epilog_end, 
 *  Update the BB chain and pred/succ lists to reflect this.
 *  Reset CG_LOOP_epilog to CG_LOOP_epilog_end. Where <tail> is the 
 *  last DoBodyBB(Original_DB_count) in the loop.
 *
 * -----------------------------------------------------------------------
 */
void CG_LOOP_Remove_Epilog_OPs(BB *tail)
{
  BB *pred;
  BBLIST *lst;
  BBLIST *lst_next;
  float prob;

  BB_Remove_All(CG_LOOP_epilog_end);

  /* Grab probability for tail=>CG_LOOP_epilog edge before the edge
   * gets removed.
   */
  lst = BB_Find_Succ(tail, CG_LOOP_epilog);
  if (lst) {
    prob = BBLIST_prob(lst);
  } else {
    Is_True(FALSE, ("couldn't find BB:%d => BB:%d edge",
		      BB_id(tail), BB_id(CG_LOOP_epilog)));
    prob = 0;
  }

  remove_prolog_or_epilog_bbs(CG_LOOP_epilog, 
			      CG_LOOP_epilog_end, 
			      CG_LOOP_epilog_end,
			      NULL);

  /* since we are keeping CG_LOOP_epilog_end, we need to clear its ops */
  BB_Remove_All(CG_LOOP_epilog_end);

  Reset_BB_has_glue_copy(CG_LOOP_epilog_end);

  for ( lst = BB_preds(CG_LOOP_epilog_end); lst != NULL; lst = lst_next ) {
    lst_next = BBLIST_next(lst);
    if (pred = BBLIST_item(lst))
      Unlink_Pred_Succ(pred, CG_LOOP_epilog_end);
  }

  Unlink_Pred_Succ(tail, CG_LOOP_epilog);
  Link_Pred_Succ_with_Prob(tail, CG_LOOP_epilog_end, prob);
  BB_next(tail) = CG_LOOP_epilog_end;
  BB_prev(CG_LOOP_epilog_end) = tail;
  CG_LOOP_epilog = CG_LOOP_epilog_end;

}

static void
duplicate_loop_info_annotation_from(BB* new_bb, BB* old_bb)
{

  FmtAssert(old_bb && new_bb && old_bb != new_bb, ("Bad input BBs"));

  ANNOTATION *annot = ANNOT_Get(BB_annotations(old_bb), ANNOT_LOOPINFO);
  LOOPINFO *old_info = annot ? ANNOT_loopinfo(annot) : NULL;
  LOOPINFO *new_info = NULL;

  if (old_info) {
    WN *wn = WN_COPY_Tree(LOOPINFO_wn(old_info));
    new_info = TYPE_P_ALLOC(LOOPINFO);
    LOOPINFO_wn(new_info) = wn;
    LOOPINFO_srcpos(new_info) = LOOPINFO_srcpos(old_info);
    LOOPINFO_swp_failure_code(new_info) = LOOPINFO_swp_failure_code(old_info);
    LOOPINFO_swp_unallococatable_rc(new_info) =
	    				LOOPINFO_swp_unallococatable_rc(old_info);
    TN *trip_count_tn = LOOPINFO_trip_count_tn(old_info);
    if (TN_is_constant(trip_count_tn))
    {
      LOOPINFO_trip_count_tn(new_info) =
		Gen_Literal_TN(TN_value(trip_count_tn), TN_size(trip_count_tn));
      LOOPINFO_max_trip_count_tn(new_info) = LOOPINFO_trip_count_tn(new_info);
    } else {
      LOOPINFO_trip_count_tn(new_info) = trip_count_tn;
      LOOPINFO_max_trip_count_tn(new_info) = NULL;
    }

    BB_Add_Annotation(new_bb, ANNOT_LOOPINFO, new_info);
  }
}


// There are several routines designed to allow back-tracking in SWP 
// optimizations. The first pair is
//	CG_LOOP_Save_Loop() and CG_LOOP_Restore_Loop()
// which works when there is a single-BB body and no prolog/epilog are
// created. It should be used to save a copy of a SWP candidate loop
// and restore it if SWP fails and the normal loop optimization resumes.
//
// The second pair is
//	CG_LOOP::Save() and CG_LOOP:Restore()
// in which all blocks between CG_LOOP_prolog_start and CG_LOOP_epilog_end
// is saved/restored. This is designed to save a copy of SWP'ed loop that
// has gone through Perform_Read_Write_Removal() hence prolog/epilog are
// created and before unrolling. The intention is to try different unrollings
// to pick the best one. Note that there should be no control flow between
// prolog/epilog blocks as the branch targets are not handled.
//
// The same routines are also used when a better SWP scheduled is found
// after a new unrolling and we want to save this partial result.
// At the end of SWP pass after all unrollings are tried, the best scheduled
// is restored. In this case, there will be control flow between prolog/epilog
// blocks due to wind up/down.

//
// CG_LOOP_Save_Loop
//
// Create a copy of the body of a single BB loop and insert it in place
// of the original body with annotations and loop info copied.
// The loop descriptor in 'cg_loop' is updated to contain the new body.
// The orignal body is removed from the CFG, but is not deleted
// and can be restored with CG_LOOP_Restore_Loop().
//
// This needs to be called before any prolog and epilog and their backpatches
// are created since these will not be restored.
//
// return the saved body and replace it with a copy in cg_loop

BB* CG_LOOP_Save_Loop(CG_LOOP& cg_loop) {

  Is_True(cg_loop.Single_BB(), ("Expecting single BB loop"));
  Is_True(cg_loop.Op_map()==NULL,
		("Should be called before op_map is initialized"));
  Is_True((CG_LOOP_prolog_start==CG_LOOP_prolog &&
	  BB_length(CG_LOOP_prolog)==0 && prolog_backpatches==NULL),
				("Non-empty prolog"));
  Is_True((CG_LOOP_epilog_end==CG_LOOP_epilog &&
	  BB_length(CG_LOOP_epilog)==0 && epilog_backpatches==NULL),
				("Non-empty epilog"));

  BB* body = cg_loop.Loop_header();
  BB *new_body = Gen_BB_Like(body);
  BB_flag(new_body) = BB_flag(body);
  BB_nest_level(new_body) = BB_nest_level(body);
  BB_rid(new_body) = BB_rid(body);
  BB_freq(new_body) = BB_freq(body);
  if (BB_freq_fb_based(body)) Set_BB_freq_fb_based(new_body);
  CGLOOP_copy_bb_fb_exec_cnt(new_body,body);
  Reset_BB_has_label(new_body);
  Set_BB_unrollings(new_body, BB_unrollings(body));

  // Associate the DFG for the old body with the new one.
  AUTOTIE_Copy_BB_DFG(body, new_body);
  
  OP* op;
  LOOP_DESCR *loop = cg_loop.Loop();
  INT64 loop_trip_est =
	WN_loop_trip_est(LOOPINFO_wn(LOOP_DESCR_loopinfo(loop)));

  FOR_ALL_BB_OPs(body, op) {

    UINT8 opnd;
    UINT8 res;
    OP *new_op = Dup_OP(op);
    CGPREP_Init_Op(new_op);
    Copy_WN_For_Memory_OP(new_op, op);

    BB_Append_Op(new_body, new_op);

  }

  OP *br_op = BB_branch_op(body);
  OP *new_br_op = BB_branch_op(new_body);

  if (br_op) {
    INT opnd;
    INT opnd_count;
    CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
    Is_True(opnd_count==1,("Expect single target branch"));
    if (opnd_count == 1) {

      TN *br_targ = OP_opnd(br_op, opnd);
      Is_True(Is_Label_For_BB(TN_label(br_targ), body),("Loop label mismatch"));
      LABEL_IDX label = Gen_Label_For_BB(new_body);
      Set_OP_opnd(new_br_op, opnd, Gen_Label_TN(label,0));
    }
  }

  Link_Pred_Succ_with_Prob(new_body, new_body,
                             loop_trip_est > 0 ?
			     (loop_trip_est - 1.0) / loop_trip_est : 0.0);

  /* Replace <body> with <new_body> in BB chain & CFG */
  Chain_BBs(BB_prev(body), new_body);
  Chain_BBs(new_body, BB_next(body));
  LOOP_DESCR_Retarget_Loop_Entrances(loop, new_body);
  Unlink_Pred_Succ(body, BB_next(body));
  Link_Pred_Succ_with_Prob(new_body, BB_next(body),
			   loop_trip_est > 0 ? 1.0 / loop_trip_est : 1.0);
  BB_next(body) = BB_prev(body) = NULL;

  /* Update loop descriptor for new loop body */
  LOOP_DESCR_loophead(loop) = new_body;
  LOOP_DESCR_Add_BB(loop, new_body);
  LOOP_DESCR_Delete_BB(loop, body);

  BB_Copy_Annotations(new_body, body, ANNOT_ENTRYINFO);

  // we need to duplicate the LOOPINFO in the annotation
  // since unrolling may change the trip_count_tn in the LOOPINFO
  duplicate_loop_info_annotation_from(new_body, body);

  ANNOTATION *annot = ANNOT_Get(BB_annotations(new_body), ANNOT_LOOPINFO);
  LOOP_DESCR_loopinfo(loop) = ANNOT_loopinfo(annot);

  unroll_xfer_annotations(new_body, body);

  cg_loop.Build_CG_LOOP_Info();

  return body;

}

//
// CG_LOOP_Restore_Loop
//
// remove all prolog/epilog and loop body and restore the 'orig_body'
// the loop descriptor in 'cg_loop' is updated to contain the new body.
// However, other info in 'cg_loop' are not restored so a new 'cg_loop'
// should be created based on the restored loop descriptor right after
// CG_LOOP_Restore_Loop() in order to re-initialize 'cg_loop' info.
//
// it is assumed that 'orig_body' was saved from previous call to
// CG_LOOP_Save_Loop() therefore the original annotation are preserved.
// We only copy the SWP annotation to record SWP attempts in assembly output.
//

void CG_LOOP_Restore_Loop(CG_LOOP& cg_loop, BB* orig_body) {

  Is_True(cg_loop.Single_BB(), ("Expecting single BB loop"));

  LOOP_DESCR *loop = cg_loop.Loop();
  ANNOTATION *annot = ANNOT_Get(BB_annotations(orig_body), ANNOT_LOOPINFO);
  loop->loopinfo = ANNOT_loopinfo(annot);
  INT64 loop_trip_est =
	WN_loop_trip_est(LOOPINFO_wn(LOOP_DESCR_loopinfo(loop)));

  BB* body = cg_loop.Loop_header();
  ANNOTATION *new_annot = ANNOT_Get(BB_annotations(body), ANNOT_LOOPINFO);
  LOOPINFO *new_loop_info = ANNOT_loopinfo(new_annot);
  SWP_RETURN_CODE swp_return_code =
		(SWP_RETURN_CODE)LOOPINFO_swp_failure_code(new_loop_info);

  CG_LOOP_Remove_Prolog_OPs(body);
  CG_LOOP_Remove_Epilog_OPs(body);

  LOOPINFO_swp_failure_code(loop->loopinfo) = swp_return_code;
  LOOPINFO_swp_unallococatable_rc(loop->loopinfo) =
	    			LOOPINFO_swp_unallococatable_rc(new_loop_info);

  // the loop body should already have an edge with proper probability to itself
  // doing the following will mess up (by doubling) the probability
  //Link_Pred_Succ_with_Prob(orig_body, orig_body,
                             //loop_trip_est > 0 ?
			     //(loop_trip_est - 1.0) / loop_trip_est : 0.0);
  float loop_back_prob = BBLIST_prob(BB_Find_Succ(orig_body,orig_body));

  /* Replace <body> with <orig_body> in BB chain & CFG */
  Chain_BBs(BB_prev(body), orig_body);
  Chain_BBs(orig_body, BB_next(body));
  LOOP_DESCR_Retarget_Loop_Entrances(loop, orig_body);
  Unlink_Pred_Succ(body, BB_next(body));
  Link_Pred_Succ_with_Prob(orig_body, BB_next(body), 1.0f - loop_back_prob);
  BB_next(body) = BB_prev(body) = NULL;
  BB_live_in(orig_body) = NULL;

  /* Update loop descriptor for new loop body */
  LOOP_DESCR_loophead(loop) = orig_body;
  LOOP_DESCR_Add_BB(loop, orig_body);
  LOOP_DESCR_Delete_BB(loop, body);

  // do not copy rotating kernel annotation when restoring from a failed
  // swp attempt.
  // the swp failure code is also recorded in the loop annotation
  // BB_Copy_Annotations(orig_body, body, ANNOT_ROTATING_KERNEL);

  Unlink_Pred_Succ(body, CG_LOOP_epilog_end);
  Unlink_Pred_Succ(CG_LOOP_prolog_start, body);
  BB_next(orig_body) = CG_LOOP_epilog_end;
  BB_next(CG_LOOP_prolog_start) = orig_body;

  GRA_LIVE_Recalc_Liveness(0);

}


void CG_LOOP_Trace_Loop(LOOP_DESCR *loop, const char *fmt, ...)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  BB *bb;
  va_list args;

  fprintf(TFile, DBar);
  va_start(args, fmt);
  vfprintf(TFile, fmt, args);
  va_end(args);
  fprintf(TFile, "\n");

  CG_LOOP_Trace_Prolog();
  fprintf(TFile, SBar);
  CG_LOOP_Backpatch_Trace(CG_LOOP_prolog, NULL);
  fprintf(TFile, "Loop body:\n");

  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
    Print_BB(bb);

  fprintf(TFile, SBar);
  if (CG_LOOP_epilog == NULL) {
    fprintf(TFile, "<epilog> no loop epilog\n");
  } else {
    CG_LOOP_Backpatch_Trace(CG_LOOP_epilog, NULL);
    fprintf(TFile, SBar);
    CG_LOOP_Trace_Epilog();
  }

  fprintf(TFile, DBar);
}


static BOOL any_bbs_in(BBLIST *list, BB_SET *bbs)
/* -----------------------------------------------------------------------
 * Return TRUE iff any BBs in <list> are members of <bbs>.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *item;
  FOR_ALL_BBLIST_ITEMS(list, item)
    if (BB_SET_MemberP(bbs, BBLIST_item(item))) return TRUE;
  return FALSE;
}


static BOOL all_bbs_in(BBLIST *list, BB_SET *bbs)
/* -----------------------------------------------------------------------
 * Return TRUE iff all BBs in <list> are members of <bbs>.
 * -----------------------------------------------------------------------
 */
{
  BBLIST *item;
  FOR_ALL_BBLIST_ITEMS(list, item)
    if (!BB_SET_MemberP(bbs, BBLIST_item(item))) return FALSE;
  return TRUE;
}


static BB *
farthest_unique_predecessor (BB *bb, BB *barrier =NULL)
/* -----------------------------------------------------------------------
 * Follow the chain of unique predecessors of 'bb', returning the
 * one "farthest" away. Return 'bb' if it has multiple predecessors.
 * -----------------------------------------------------------------------
 */
{
  BB *pbb = bb;
  
  if (pbb)
  {
    do {
      if (!BB_Has_One_Pred(pbb))
	break;

      BB *pred = BB_First_Pred(pbb);
      if (barrier && (pred == barrier))
	break;
      
      pbb = pred;

    } while (pbb != bb);
  }

  return pbb;
}


static BB *
farthest_unique_successor (BB *bb, BB *barrier =NULL)
/* -----------------------------------------------------------------------
 * Follow the chain of unique successors of 'bb', returning the
 * one "farthest" away. Return 'bb' if it has multiple successors.
 * -----------------------------------------------------------------------
 */
{
  BB *sbb = bb;
  
  if (sbb)
  {
    do {
      if (!BB_Has_One_Succ(sbb))
	break;

      BB *succ = BB_First_Succ(sbb); 
      if (barrier && (succ == barrier))
	break;

      if (sbb->loop_head_bb == succ)
	break;

      sbb = succ;

    } while (sbb != bb);
  }

  return sbb;
}


/* -----------------------------------------------------------------------
 *  BOOL CG_LOOP::Attach_Prolog_And_Epilog(LOOP_DESCR *loop)
 *    Create and attach CG_LOOP_prolog/epilog BBs to <loop>'s head
 *    and tail.  It returns TRUE unless <loop> is of some weird
 *    form that it can't handle (e.g. multiple tails).
 * -----------------------------------------------------------------------
 */
void CG_LOOP::Attach_Prolog_And_Epilog(LOOP_DESCR *loop)
{
  BB *loop_head = LOOP_DESCR_loophead(loop);
  BB *loop_tail = LOOP_DESCR_Find_Unique_Tail(loop), *bb;
  const char *trace_pfx = Get_Trace(TP_CGPREP, 1) ? "<cgprep> " : NULL;
  BBLIST *preds;
  BOOL freqs = FREQ_Frequencies_Computed();

  // Identify the BB containing the trip count TN.
  if (CG_LOOP_Trip_Count(loop) != NULL) {
    preds = BB_preds(loop_head);
    while (preds && BB_SET_MemberP(LOOP_DESCR_bbset(loop), BBLIST_item(preds)))
      preds = BBLIST_next(preds);
    if (preds) 
      trip_count_bb = BBLIST_item(preds);
    Is_True(trip_count_bb, 
	    ("unable to identify trip count bb."));
  }

  /* create backpatches */
  prolog_backpatches = epilog_backpatches = NULL;

  CG_LOOP_prolog = CG_LOOP_prolog_start = NULL;

  Set_has_prolog();
  CG_LOOP_prolog = CG_LOOP_Gen_And_Prepend_To_Prolog(loop_head, loop);
  CG_LOOP_prolog_start = CG_LOOP_prolog;
  GRA_LIVE_Compute_Liveness_For_BB(CG_LOOP_prolog);
  if (trace_pfx)
    fprintf(TFile, "%screating loop prolog BB:%d\n", trace_pfx,
	    BB_id(CG_LOOP_prolog));


  /* Disallow loops without (exactly) one loop tail.
   *
   * TODO: Allow multiple tail loops by adding new BB which branches
   *	   to <loop_head> (and making internal branches retarget to
   *	   this new BB).  Or get rid of algorithms requiring a unique
   *	   tail.  I think this is pretty easy for most of them.
   */
  if (loop_tail == NULL)
    return;

  /* If 'loop_tail' falls-through to a block "B", containing only a
     jump, then we want to remember the target of that jump, so we can
     detect when other exits go to that same target. Any exits that go
     to the target can be redirected to "B", allowing the loop to have
     a common epilog. */

  BB *alt_exit = NULL, *retarg = NULL;
  BB *ft = BB_Fall_Thru_Successor(loop_tail);
  if (ft)
  {
    switch (BB_length(ft))
    {
    case 0:
      alt_exit = BB_Fall_Thru_Successor(ft);
      retarg = ft;
      break;
      
    case 1:
    {
      OP *br = BB_branch_op(ft);
      if (br && OP_jump(br) && (BB_succs_len(ft) == 1))
      {
	alt_exit = BB_First_Succ(ft);
	retarg = ft;
      }
      break;
    }
    }
  }
  
  /* See whether all loop exits go to the same BB.  If so, we can
   * either use that BB as an epilog, or create a new epilog just
   * before that BB.  Otherwise we can't insert a single epilog BB
   * without adding extra branches, so we'll return with a NULL
   * epilog to disallow transformations requiring a common epilog.
   */
  CG_LOOP_epilog = CG_LOOP_epilog_end = NULL;
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
  {
    for (BBLIST *next, *succs = BB_succs(bb); succs; succs = next)
    {
      next = BBLIST_next(succs);
      BB *succ = BBLIST_item(succs);
      if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop), succ))
      {
	/* If 'succ' is 'alt_exit', then change 'bb' to instead target
           'retarg'. Since 'retarg' is a fall-through exit to the
           loop, this increases the likelyhood that we will have a
           common epilog. */
	if (succ == alt_exit)
	  if (BB_Retarget_Branch(bb, succ, retarg))
	    continue;
	   
	if (CG_LOOP_epilog && succ != CG_LOOP_epilog)
	{
	  CG_LOOP_epilog  = NULL;
	  if (trace_pfx)
	    fprintf(TFile, "%scan't find or create suitable loop epilog; "
		    "disallows some optimization\n", trace_pfx);
	  return;
	}
	
	CG_LOOP_epilog = succ;
      }
    }
  }
  
  Set_has_epilog();
  if (CG_LOOP_epilog == NULL) {
    /*
     * We have an infinite loop, but we'll create and insert an epilog
     * at the end of BB chain to allow maximum optimization of the loop
     * body.  The epilog will later be removed since it's unreachable.
     *
     * TODO: (Compspeed) Avoid walking BB chain to find last one by
     *       tracking REGION_Last_BB?  Can we just insert it before
     *	     REGION_First_BB?
     */
    BB *last_bb = REGION_First_BB;
    while (BB_next(last_bb)) last_bb = BB_next(last_bb);
    CG_LOOP_epilog = Gen_And_Insert_BB_After(last_bb);
    if (trace_pfx)
      fprintf(TFile, "%screating loop epilog BB:%d\n", trace_pfx,
	      BB_id(CG_LOOP_epilog));
    BB_rid(CG_LOOP_epilog) = BB_rid(loop_head);
    if (!CG_localize_tns) Set_BB_gra_spill(CG_LOOP_epilog);
    if (BB_freq_fb_based(CG_LOOP_prolog)) Set_BB_freq_fb_based(CG_LOOP_epilog);
    GRA_LIVE_Compute_Liveness_For_BB(CG_LOOP_epilog);
  } else {
    BB *next = CG_LOOP_epilog;
    BB *ftp = BB_Fall_Thru_Predecessor(next);
    BBKIND ftp_kind = ftp ? BB_kind(ftp) : BBKIND_UNKNOWN;
    LOOP_DESCR *enclosing = LOOP_DESCR_Next_Enclosing_Loop(loop);
    CG_LOOP_epilog = Gen_And_Insert_BB_Before(next);
    if (trace_pfx)
      fprintf(TFile, "%screating loop epilog BB:%d\n", trace_pfx,
	      BB_id(CG_LOOP_epilog));
    BB_rid(CG_LOOP_epilog) = BB_rid(loop_head);
    if (!CG_localize_tns) Set_BB_gra_spill(CG_LOOP_epilog);
    if (BB_freq_fb_based(CG_LOOP_prolog)) Set_BB_freq_fb_based(CG_LOOP_epilog);
    if (ftp && !BB_SET_MemberP(LOOP_DESCR_bbset(loop), ftp)) {
      /*
       * Since <ftp> isn't in loop, we don't want it to fall through
       * to CG_LOOP_epilog.  It was falling through to <next>, so
       * we'll make it either branch to <next> or fall through to a
       * new BB that branches to <next>.
       */
      if (ftp_kind == BBKIND_GOTO) {
	BBLIST *bbl = BB_Find_Succ(ftp, next);
	Add_Goto(ftp, next);
	BBLIST_prob(bbl) = 1.0F;	/* otherwise is incremented to 2.0 */
      } else {
	BB *new_bb = Gen_And_Insert_BB_After(ftp);
	BB_rid(new_bb) = BB_rid(loop_head);
	if (BB_freq_fb_based(CG_LOOP_epilog)) Set_BB_freq_fb_based(new_bb);
	Change_Succ(ftp, next, new_bb);
	Add_Goto(new_bb, next);
	GRA_LIVE_Compute_Liveness_For_BB(new_bb);
      }
    }
    retarget_loop_exits(loop, next, CG_LOOP_epilog);
    Link_Pred_Succ_with_Prob(CG_LOOP_epilog, next, 1.0);
    if (freqs || BB_freq_fb_based(next))
      BB_freq(next) += BB_freq(CG_LOOP_epilog);
    GRA_LIVE_Compute_Liveness_For_BB(CG_LOOP_epilog);

    /* Add CG_LOOP_epilog to appropriate LOOP_DESCRs, if any.
     * It can belong only to loops enclosing this one, so
     * we don't bother checking any others.  */
    if (enclosing &&
	all_bbs_in(BB_preds(CG_LOOP_epilog), LOOP_DESCR_bbset(enclosing)))
      LOOP_DESCR_Add_BB(enclosing, CG_LOOP_epilog);
  }
  CG_LOOP_epilog_end = CG_LOOP_epilog;

  if (CG_warn_bad_freqs && CG_LOOP_epilog &&
      (freqs || BB_freq_fb_based(CG_LOOP_prolog)) &&
      !FREQ_Match(BB_freq(CG_LOOP_prolog), BB_freq(CG_LOOP_epilog)))
    DevWarn("CG_LOOP prolog (BB:%d) and epilog (BB:%d) frequencies disagree",
	    BB_id(CG_LOOP_prolog), BB_id(CG_LOOP_epilog));
}


/* =======================================================================
 *
 * CG_LOOP_DEF::Get(TN *tn)
 *   Returns the first def-op for 'tn' in the loop body.
 *   Returns NULL if the TN is not defined.
 *
 * =======================================================================
 */
OP *CG_LOOP_DEF::Get(TN *tn)
{
  if (TN_is_register(tn) && !TN_is_const_reg(tn))
    return (OP*) TN_MAP_Get(tn_map, tn); 
  else
    return NULL;
}

/* =======================================================================
 *
 * CG_LOOP_DEF::Is_invariant(TN *tn)
 *  Returns TRUE if TN is a loop invariant.
 *
 * =======================================================================
 */
BOOL CG_LOOP_DEF::Is_invariant(TN *tn)
{
  return Get(tn) == NULL;
}

/* =======================================================================
 *
 * CG_LOOP_DEF::CG_LOOP_DEF(BB *)
 *   Allocate TN_MAP and setup the TN -> first_def_op mapping.
 *
 * =======================================================================
 */
CG_LOOP_DEF::CG_LOOP_DEF(BB *body)
{
  tn_map = TN_MAP_Create();
  OP *op;
  FOR_ALL_BB_OPs(body, op) {

    for (INT i = 0; i < OP_results(op); i++) {
      TN *res = OP_result(op,i);
      if (TN_is_register(res) && 
	  !TN_is_const_reg(res) &&
	  !Get(res))
	TN_MAP_Set(tn_map, res, op);
    }
  }
}

/* =======================================================================
 *
 * CG_LOOP_DEF::~CG_LOOP_DEF() 
 *   Release TN_MAP.
 *
 * =======================================================================
 */
CG_LOOP_DEF::~CG_LOOP_DEF()
{
  TN_MAP_Delete(tn_map);
}


/* =======================================================================
 *
 *  CG_LOOP::Build_CG_LOOP_Info
 *
 *    Attach the loop info to each OP in the loop.
 *    Operand TNs that are not invariants and used before definition
 *    in the loop body are entered to the prolog backpatch.
 *    Result TNs that are liveout are entered to the epilog backpatch
 *
 * =======================================================================
 */
void CG_LOOP::Build_CG_LOOP_Info()
{
  Is_True(Single_BB(), ("LOOP has multiple BB."));

  // Build_CG_LOOP_Info might be called multiple times.
  if (_CG_LOOP_info_map) {
    OP_MAP_Delete(_CG_LOOP_info_map);
    _CG_LOOP_info_map = NULL;
  }

  Recompute_Liveness();

  _CG_LOOP_info_map = OP_MAP_Create(/* body */);
  op_map = _CG_LOOP_info_map;

  BB *body = Loop_header();
  BB *prolog = Prolog_end();
  BB *epilog = Epilog_start();

  CG_LOOP_DEF tn_def(body);

  /* Create _CG_LOOP_info_map entries.  For exposed uses, fill in
   * omega=1 and create a prolog backpatch.  For live-out defs, create
   * an epilog backpatch.
   */
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    CG_LOOP_Init_Op(op);
    for (INT i = 0; i < OP_results(op); i++) {
      TN *res = OP_result(op,i);
      if (TN_is_register(res) &&
	  TN_is_global_reg(res) &&
	  !TN_is_const_reg(res)) {
	if (GTN_SET_MemberP(BB_live_in(epilog), res)) {
	  Is_True(TN_is_global_reg(res), ("TN in GTN_SET not a global_reg."));

	  if (!TN_is_dedicated(res))
	    CG_LOOP_Backpatch_Add(epilog, res, res, 0);

	  if (GTN_SET_MemberP(BB_live_in(body), res))
	    if (!TN_is_dedicated(res))
	      CG_LOOP_Backpatch_Add(prolog, res, res, 1);
	}
      }
    }
    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      TN *tn = OP_opnd(op,opnd);
      if (TN_is_register(tn) &&
	  !TN_is_const_reg(tn)) {
	OP *def_op = tn_def.Get(tn);
	// TN is not an invariant and
	// TN is not defined before this OP.
	if (def_op && 
	    !OP_Precedes(def_op, op)) {
	  if ( !CG_LOOP_Backpatch_Find_Non_Body_TN(prolog, tn, 1) )
	    if (!TN_is_dedicated(tn))
	      CG_LOOP_Backpatch_Add(prolog, tn, tn, 1);
	  Set_OP_omega(op, opnd, 1);
	}
      }
    }
  }

#ifdef Is_True_On
  Verify();
#endif
}


/* =======================================================================
 *
 *  CG_LOOP::CG_LOOP
 *
 *    Setup prolog/epilog of the loop.
 *
 * =======================================================================
 */
CG_LOOP::CG_LOOP(LOOP_DESCR *loop)
{
  Current_CG_LOOP = this;

  BB *body = LOOP_DESCR_loophead(loop);

  trace = Get_Trace(TP_CGPREP,0x4000,BB_id(body));

  unroll_fully = FALSE;
  unroll_factor = -1; 
  flags = CG_LOOP_NONE;
  trip_count_bb = NULL;
  prolog_start = prolog_end = NULL;
  epilog_start = epilog_end = NULL;

  Attach_Prolog_And_Epilog(loop);

  prolog_start = CG_LOOP_prolog_start;
  prolog_end = CG_LOOP_prolog;
  epilog_start = CG_LOOP_epilog;
  epilog_end = CG_LOOP_epilog_end;
  remainder_loop = NULL;

  prolog_start_save = NULL;
  prolog_end_save = NULL;
  epilog_start_save = NULL;
  epilog_end_save = NULL;
  remainder_loop_save = NULL;
  trip_count_bb_save = NULL;
  body_save = NULL;
  unroll_fully_save = FALSE;
  unroll_factor_save = 1;
  flags_save = 0;

  prolog_start_best = NULL;
  prolog_end_best = NULL;
  epilog_start_best = NULL;
  epilog_end_best = NULL;
  remainder_loop_best = NULL;
  trip_count_bb_best = NULL;
  body_best = NULL;
  unroll_fully_best = FALSE;
  unroll_factor_best = 1;
  flags_best = 0;

  multireg_sc = ISA_REGSUBCLASS_branch_1;

  /* info_map */
  op_map = NULL;
  _CG_LOOP_info_map = NULL;

  prolog_backpatches = epilog_backpatches = NULL;

  // copy to CG_LOOP structure
  this->loop = loop;

  need_super_swp = false;
  super_swp_ii = 0;       // ii used for exhaustive search
			  // 0 means no exhaustive search
  super_swp_unroll = 0;   // unrolling used for exhaustive search
  swp_schedule_pragma_wn = NULL;

  if ( BB_has_pragma(body) ) {

    ANNOTATION *ant;
    for ( ant = ANNOT_First (BB_annotations(body), ANNOT_PRAGMA);
	  ant != NULL;
	  ant = ANNOT_Next (ant, ANNOT_PRAGMA)) {

      WN *wn = ANNOT_pragma(ant);
      WN_PRAGMA_ID pragma_id = (WN_PRAGMA_ID) WN_pragma(wn);

      if (pragma_id == WN_PRAGMA_SUPER_SWP) {
	need_super_swp = true;
        super_swp_ii = WN_pragma_arg1(wn);
        super_swp_unroll = WN_pragma_arg2(wn);
      } else if (pragma_id == WN_PRAGMA_SWP_SCHEDULE) {

	swp_schedule_pragma_wn = wn;

      }
    }
  }
}

// Used by Copy_Loop_BBs() to create an (almost) exact copy of the 'in_bb' that
// can be restored later.

static BB*
Copy_BB(BB* in_bb) {

  OP* op;
  BB *new_bb = Gen_BB_Like(in_bb);
  BB_flag(new_bb) = BB_flag(in_bb);
  BB_nest_level(new_bb) = BB_nest_level(in_bb);
  BB_rid(new_bb) = BB_rid(in_bb);
  BB_freq(new_bb) = BB_freq(in_bb);
  if (BB_freq_fb_based(in_bb)) Set_BB_freq_fb_based(new_bb);
  CGLOOP_copy_bb_fb_exec_cnt(new_bb,in_bb);
  Reset_BB_has_label(new_bb);
  Set_BB_unrollings(new_bb, BB_unrollings(in_bb));

  FOR_ALL_BB_OPs(in_bb, op) {

    INT id;

    OP *new_op = Dup_OP(op);
    CGPREP_Init_Op(new_op);
    Copy_WN_For_Memory_OP(new_op, op);

    if (swp_op_map && (id=(INT)(OP_MAP_Get(swp_op_map, op)))!=0) {
      OP_MAP_Set(swp_op_map, new_op, (void*)id);
    }

    BB_Append_Op(new_bb, new_op);
  }

  BB_Copy_Annotations(new_bb, in_bb, ANNOT_ENTRYINFO);

  // we need to duplicate the LOOPINFO in the annotation
  // since unrolling may change the trip_count_tn in the LOOPINFO
  duplicate_loop_info_annotation_from(new_bb, in_bb);

  // Associate the DFG of 'in_bb' with the new BB.
  AUTOTIE_Copy_BB_DFG(in_bb, new_bb);

  return new_bb;

}

// Dupliate BBs between 'prolog_start' and 'epilog_end' (both included)
// and their control flows. It assumes a single-BB loop body 'body'.
// The mapping between newly created blocks and the input blocks are recorded
// in the 'bb_map'.
//

static void
Copy_Loop_BBs(BB* prolog_start, BB* body, BB* epilog_end, BB_MAP bb_map) {

  BB* prolog_end = NULL;
  BB* epilog_start = NULL;

  BB* bb = Copy_BB(body);
  BB_MAP_Set(bb_map, body, bb);
  BB* new_body = bb;

  BB* last_bb = NULL;
  BB* new_bb = NULL;
  BB* next_bb = NULL;

  next_bb=prolog_start;
  do {
    bb = next_bb;
    new_bb = Copy_BB(bb);
    BB_MAP_Set(bb_map, bb, new_bb);

    if (last_bb) {
      // edges will be updated below
      // Link_Pred_Succ(last_bb, new_bb);
      Chain_BBs(last_bb, new_bb);
    } else
      BB_prev(new_bb) = NULL;

    last_bb = new_bb;

    if (BB_next(bb)==body)
      next_bb=NULL;
    else
      next_bb=BB_next(bb);
  } while (next_bb);
  prolog_end = last_bb;
  Chain_BBs(new_bb, new_body);

  last_bb = NULL;
  new_bb = NULL;
  next_bb=epilog_end;
  do {
    bb = next_bb;
    new_bb = Copy_BB(bb);
    BB_MAP_Set(bb_map, bb, new_bb);

    if (last_bb) {
      // edges will be updated below
      // Link_Pred_Succ(new_bb, last_bb);
      Chain_BBs(new_bb, last_bb);
    } else
      BB_next(new_bb) = NULL;

    last_bb = new_bb;

    if (BB_prev(bb)==body)
      next_bb=NULL;
    else
      next_bb=BB_prev(bb);
  } while (next_bb);
  epilog_start = last_bb;
  Chain_BBs(new_body, new_bb);

  next_bb = prolog_start;
  do {
    BBLIST* succs;
    bb = next_bb;
    if (bb!=epilog_end) {
      FOR_ALL_BB_SUCCS(bb, succs) {

	BB *succ = BBLIST_item(succs);
	BB* new_pred = (BB*)BB_MAP_Get(bb_map, bb);
	BB* new_succ = (BB*)BB_MAP_Get(bb_map, succ);

	OP *br_op = BB_branch_op(bb);
	OP *new_br_op = BB_branch_op(new_pred);

	if (br_op) {
	  INT opnd;
	  INT opnd_count;
	  CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
	  Is_True(opnd_count==1,("Expect single target branch"));
	  if (opnd_count == 1) {

	    TN *br_targ = OP_opnd(br_op, opnd);
	    if (Is_Label_For_BB(TN_label(br_targ), succ)) {
	      LABEL_IDX label = Gen_Label_For_BB(new_succ);
	      Set_OP_opnd(new_br_op, opnd, Gen_Label_TN(label,0));
	    }
	  }
	}
	Link_Pred_Succ_with_Prob(new_pred, new_succ, BBLIST_prob(succs));
      }
      next_bb = BB_next(bb);
    } else
      next_bb=NULL;
  } while (next_bb!=NULL);

  unroll_xfer_annotations(new_body, body);

  return;

}

/* =======================================================================
 *
 *  CG_LOOP::Save
 *
 *    Save a copy of blocks between CG_LOOP_prolog_start and
 *    CG_LOOP_epilog_end for a single-BB loop.
 *
 * =======================================================================
 */
BOOL CG_LOOP::Save(BOOL is_best)
{

  if (trace) {
    Recompute_Liveness();
    if (is_best)
      CG_LOOP_Trace_Loop(loop, "*** Before Save Best ***");
    else
      CG_LOOP_Trace_Loop(loop, "*** Before Save ***");
  }

  Is_True(Single_BB(), ("Try to save non-single BB loop"));

  if (!Single_BB())
    return FALSE;

  BB_MAP bb_map = BB_MAP_Create();
  Is_True(bb_map, ("Fail to create bb_map"));

  if (!bb_map)
    return FALSE;

  BB* body = LOOP_DESCR_loophead(loop);

  // since we are not saving/restoring backpatches, we need to make sure
  // they can be safely ignored

  CG_LOOP_Remove_Notations(*this, CG_LOOP_prolog, CG_LOOP_epilog);

  Copy_Loop_BBs(CG_LOOP_prolog_start, body, CG_LOOP_epilog_end, bb_map);

  if (is_best) {
    flags |= CG_LOOP_HAS_BEST;
    unroll_fully_best = unroll_fully;
    unroll_factor_best = unroll_factor;
    flags_best = flags;
    trip_count_bb_best = trip_count_bb;
    prolog_start_best = (BB*)BB_MAP_Get(bb_map,CG_LOOP_prolog_start);
    prolog_end_best = (BB*)BB_MAP_Get(bb_map,CG_LOOP_prolog);
    epilog_start_best = (BB*)BB_MAP_Get(bb_map,CG_LOOP_epilog);
    epilog_end_best = (BB*)BB_MAP_Get(bb_map,CG_LOOP_epilog_end);
    body_best = (BB*)BB_MAP_Get(bb_map, body);
    BB_Copy_Annotations(body_best, body, ANNOT_ROTATING_KERNEL);
    if (remainder_loop) {
      remainder_loop_best = (BB*)BB_MAP_Get(bb_map, remainder_loop);
      NOTE_REMAINDER_HEAD *note =
	      (NOTE_REMAINDER_HEAD*)NOTE_Retrieve_Note_For_Handler(
				remainder_loop,remainder_head_note_handler);
      if (note) {
	note_remainder_head(remainder_loop_best,
			    NOTE_ntimes(note),NOTE_const_trip(note)?1:0);
      }
    } else
      remainder_loop_best = NULL;
  } else {
    flags |= CG_LOOP_HAS_SAVE;
    unroll_fully_save = unroll_fully;
    unroll_factor_save = unroll_factor;
    flags_save = flags;
    trip_count_bb_save = trip_count_bb;
    prolog_start_save = (BB*)BB_MAP_Get(bb_map,CG_LOOP_prolog_start);
    prolog_end_save = (BB*)BB_MAP_Get(bb_map,CG_LOOP_prolog);
    epilog_start_save = (BB*)BB_MAP_Get(bb_map,CG_LOOP_epilog);
    epilog_end_save = (BB*)BB_MAP_Get(bb_map,CG_LOOP_epilog_end);
    body_save = (BB*)BB_MAP_Get(bb_map, body);
    if (remainder_loop) {
      remainder_loop_save = (BB*)BB_MAP_Get(bb_map, remainder_loop);
      NOTE_REMAINDER_HEAD *note =
	      (NOTE_REMAINDER_HEAD*)NOTE_Retrieve_Note_For_Handler(
				remainder_loop,remainder_head_note_handler);
      if (note) {
	note_remainder_head(remainder_loop_save,
			    NOTE_ntimes(note),NOTE_const_trip(note)?1:0);
      }
    } else
      remainder_loop_save = NULL;
  }

  prolog_end = CG_LOOP_prolog;
  epilog_start = CG_LOOP_epilog;

  BB_MAP_Delete(bb_map);

  prolog_backpatches = epilog_backpatches = NULL;

  Build_CG_LOOP_Info();
  Recompute_Liveness();

  return TRUE;
}

/* =======================================================================
 *
 *  CG_LOOP::Restore
 *
 *    Retore a copy of blocks between CG_LOOP_prolog_start and
 *    CG_LOOP_epilog_end for a single-BB loop. The old blocks and codes
 *    are removed. The new CG_LOOP_prolog_start and CG_LOOP_epilog_end
 *    will be empty blocks with single successor/predecessor being the
 *    restored copy of CG_LOOP_prolog_start and CG_LOOP_epilog_end,
 *    respectively. The empty blocks can be merged in later CFLOW pass.
 *
 * =======================================================================
 */
BOOL CG_LOOP::Restore(BOOL is_best)
{

  if (trace) {
    Recompute_Liveness();
    if (is_best)
      CG_LOOP_Trace_Loop(loop, "*** Before Restore Best ***");
    else
      CG_LOOP_Trace_Loop(loop, "*** Before Restore ***");
  }

  if (!(flags & CG_LOOP_HAS_SAVE))
    return FALSE;

  BB_MAP bb_map = BB_MAP_Create();
  Is_True(bb_map, ("Fail to create bb_map"));

  if (!bb_map)
    return FALSE;

  // remove old blocks and codes
  BB* body = LOOP_DESCR_loophead(loop);
  LOOP_DESCR_Delete_BB(loop, body);
  CG_LOOP_Remove_Prolog_OPs(body);
  CG_LOOP_Remove_Epilog_OPs(body);
  BB_Remove_All(body);
  Remove_BB(body);
  BB_Delete_Successors(CG_LOOP_prolog_start);
  BB_Delete_Predecessors(CG_LOOP_epilog_end);

  BB* new_prolog_start = NULL;
  BB* new_epilog_end = NULL;
  BB* new_body = NULL;

  if (is_best) {
  
    Copy_Loop_BBs(prolog_start_best, body_best, epilog_end_best, bb_map);

    unroll_fully = unroll_fully_best;
    unroll_factor = unroll_factor_best;
    flags_best |= (flags & CG_LOOP_HAS_SAVE);
    flags_best |= (flags & CG_LOOP_HAS_BEST);
    flags = flags_best;
    trip_count_bb = trip_count_bb_best;

    new_prolog_start = (BB*)BB_MAP_Get(bb_map, prolog_start_best);
    new_epilog_end = (BB*)BB_MAP_Get(bb_map, epilog_end_best);
    new_body = (BB*)BB_MAP_Get(bb_map, body_best);
    prolog_end = CG_LOOP_prolog = (BB*)BB_MAP_Get(bb_map, prolog_end_best);
    epilog_start = CG_LOOP_epilog = (BB*)BB_MAP_Get(bb_map, epilog_start_best);
    BB_Copy_Annotations(new_body, body_best, ANNOT_ROTATING_KERNEL);
    if (remainder_loop_best) {
      remainder_loop = (BB*)BB_MAP_Get(bb_map, remainder_loop_best);
      NOTE_REMAINDER_HEAD *note =
	      (NOTE_REMAINDER_HEAD*)NOTE_Retrieve_Note_For_Handler(remainder_loop_best,
			      		     remainder_head_note_handler);
      if (note) {
	note_remainder_head(remainder_loop,
			    NOTE_ntimes(note),NOTE_const_trip(note)?1:0);
      }
    } else
      remainder_loop = NULL;

  } else {

    Copy_Loop_BBs(prolog_start_save, body_save, epilog_end_save, bb_map);

    unroll_fully = unroll_fully_save;
    unroll_factor = unroll_factor_save;
    flags_save |= (flags & CG_LOOP_HAS_SAVE);
    flags_save |= (flags & CG_LOOP_HAS_BEST);
    flags = flags_save;
    trip_count_bb = trip_count_bb_save;

    new_prolog_start = (BB*)BB_MAP_Get(bb_map, prolog_start_save);
    new_epilog_end = (BB*)BB_MAP_Get(bb_map, epilog_end_save);
    new_body = (BB*)BB_MAP_Get(bb_map, body_save);
    prolog_end = CG_LOOP_prolog = (BB*)BB_MAP_Get(bb_map, prolog_end_save);
    epilog_start = CG_LOOP_epilog = (BB*)BB_MAP_Get(bb_map, epilog_start_save);
    if (remainder_loop_save) {
      remainder_loop = (BB*)BB_MAP_Get(bb_map, remainder_loop_save);
      NOTE_REMAINDER_HEAD *note =
	      (NOTE_REMAINDER_HEAD*)NOTE_Retrieve_Note_For_Handler(remainder_loop_save,
			      		     remainder_head_note_handler);
      if (note) {
	note_remainder_head(remainder_loop,
			    NOTE_ntimes(note),NOTE_const_trip(note)?1:0);
      }
    } else
      remainder_loop = NULL;

  }

  Chain_BBs(CG_LOOP_prolog_start, new_prolog_start);
  Link_Pred_Succ_with_Prob(CG_LOOP_prolog_start, new_prolog_start, 1.0f);
  Chain_BBs(new_epilog_end, CG_LOOP_epilog_end);
  Link_Pred_Succ_with_Prob(new_epilog_end, CG_LOOP_epilog_end, 1.0f);

  LOOP_DESCR_loophead(loop) = new_body;

  BB_MAP_Delete(bb_map);

  prolog_end = CG_LOOP_prolog;
  epilog_start = CG_LOOP_epilog;

  /* Update loop descriptor for new loop body */
  LOOP_DESCR_loophead(loop) = new_body;
  LOOP_DESCR_Add_BB(loop, new_body);
  ANNOTATION *annot = ANNOT_Get(BB_annotations(new_body), ANNOT_LOOPINFO);
  loop->loopinfo = ANNOT_loopinfo(annot);

  prolog_backpatches = epilog_backpatches = NULL;

  Recompute_Liveness();

  GRA_LIVE_Recalc_Liveness(0);

  Build_CG_LOOP_Info();

  if (trace) {
    if (is_best)
      CG_LOOP_Trace_Loop(loop, "*** After Restore Best ***");
    else
      CG_LOOP_Trace_Loop(loop, "*** After Restore ***");
  }

  return TRUE;
}

/* =======================================================================
 *
 *  CG_LOOP::~CG_LOOP
 *
 * =======================================================================
 */
CG_LOOP::~CG_LOOP()
{
  if (_CG_LOOP_info_map) {
    OP_MAP_Delete(_CG_LOOP_info_map);
    _CG_LOOP_info_map = NULL;
  }
  prolog_backpatches = epilog_backpatches = NULL;
  Current_CG_LOOP = NULL;
}


/* =======================================================================
 *
 *  CG_LOOP::Verify
 *
 * =======================================================================
 */
void CG_LOOP::Verify()
{
  BB *body = Loop_header();
  if (_CG_LOOP_info_map) {
    OP *op;
    FOR_ALL_BB_OPs(body, op) {
      // Verify TN omega
      //    
      for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
	TN *tn = OP_opnd(op,opnd);
	if (TN_is_dedicated(tn))
	  FmtAssert(OP_omega(op, opnd) <= 1,
		    ("CG_LOOP: Dedicated TN cannot have omega > 1."));
      }
    }
  }
}


/* =======================================================================
 *
 *  Recompute all the live info for the blocks between and including the
 *  loop head and tail.   Include the trip-count expressions, otherwise
 *  it will be deleted after GRA live.
 *
 * =======================================================================
 */
void
CG_LOOP::Recompute_Liveness()
{
  BB *entry = Trip_count_bb() ? Trip_count_bb() : Prolog_start();
  BB *far_entry = farthest_unique_predecessor(entry);
  BB_REGION region(Malloc_Mem_Pool);
  BBLIST *succs;

  region.entries.push_back(far_entry);
  
  // If no epilog, search all exit BBs; otherwise, use epilog
  if (Epilog_end() == NULL) {
    BB *bb;
    FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(Loop()), bb) {
      for (succs = BB_succs(bb); succs; succs = BBLIST_next(succs)) {
        BB *succ = BBLIST_item(succs);
        if (!BB_SET_MemberP(LOOP_DESCR_bbset(Loop()), succ)) {  
          region.exits.push_back(farthest_unique_successor(succ, far_entry));
        }
      }
    }
  } else {
    FOR_ALL_BB_SUCCS(Epilog_end(), succs) {
      BB *succ = BBLIST_item(succs);
      region.exits.push_back(farthest_unique_successor(succ, far_entry));
    }
  }

  // Promote to omega TN to GTN
  if (_CG_LOOP_info_map) {
    BB *body = Loop_header();
    OP *op;
    FOR_ALL_BB_OPs(body, op) {
      for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
	TN *tn = OP_opnd(op,opnd);
	if (Is_CG_LOOP_Op(op) &&
	    OP_omega(op, opnd) != 0 &&
	    !TN_is_dedicated(tn) &&
	    !TN_is_global_reg(tn)) 
	  GTN_UNIVERSE_Add_TN(tn);
      }
    }
  }

  GRA_LIVE_Init_Loop(CG_LOOP_prolog, 
		     _CG_LOOP_info_map ? Loop_header() : NULL,
		     CG_LOOP_epilog,
		     prolog_backpatches, 
		     epilog_backpatches);

  BB_REGION_Recompute_Global_Live_Info(region, TRUE /* recompute local info */);

  GRA_LIVE_Fini_Loop();
}


void CG_LOOP::Redo_Prolog_Epilog()
{
  prolog_start = CG_LOOP_prolog_start;
  prolog_end = CG_LOOP_prolog;
  epilog_start = CG_LOOP_epilog;
  epilog_end = CG_LOOP_epilog_end;
}

void CG_LOOP::LI_Hoisting_BB()
{
  // Need Build_CG_LOOP_Info() to be invoked before

  BB *body=Loop_header();

  Is_True (Single_BB(), ("LI_Hoisting_BB: not a single BB loop."));
  Is_True (Has_prolog(), ("LI_Hoisting_BB: no loop prolog."));

  CG_DEP_Compute_Graph(body,
                       INCLUDE_ASSIGNED_REG_DEPS,
                       CYCLIC,
                       INCLUDE_MEMREAD_ARCS,
                       INCLUDE_MEMIN_ARCS,
                       NO_CONTROL_ARCS,
                       NULL);

  // Use OP_flag1 of OP to identify loop-invariant
  int len=0;  // number of loop-invariant ops
  OP  *op;
  FOR_ALL_BB_OPs(body, op) {
    Reset_OP_flag1(op);
  }

  bool  is_candidate;
  FOR_ALL_BB_OPs(body, op) {
    ARC_LIST *al = OP_preds(op);
    if ( al == NULL ) {
      // dependent on nothing, must be li
      is_candidate = true;
    } else {
      is_candidate = true;
      for (; al; al = ARC_LIST_rest(al)) {
        ARC *arc  = ARC_LIST_first(al);
        OP  *pred = ARC_pred(arc);
        bool pred_precedes_op = OP_Precedes(pred, op);

        // Check if it is loop-invariant.
        // It is a li if there is 
	//    1. No unknown dependence, AND
        //    2. (1) no input dep upon any op of the loop, OR
        //       (2) input dep on ops that precedes the current one
        //       and those ops are all li.
        if ( !ARC_is_misc(arc) && 
             (!ARC_is_input(arc) ||  (pred_precedes_op && OP_flag1(pred))) )
        { // Now, op is a LI, check if it can be moved out
          if (   ARC_is_output(arc) 
              || (ARC_is_anti(arc) && pred_precedes_op))
          {
            /*  
               The 'anti' condition checks the following:
                     LOOP:
                          =  x
                  op:   x = ...  
                        .....
                     ENDLOOP
               and op cannot be moved out of the loop.

               Checking if pred_precedes_op is true is necessary. Without
               checking it, the following op will not be moved out even it
               can be moved out.
                     LOOP:
                  op:   x = ...  
                        ...
                        x =...
                          =  x
                        .....
                     ENDLOOP
             */
            is_candidate = false;
            break;
          } else {
            continue;
          }
        } else {
          is_candidate = false;
          break;
        }
      } 
    }

    if (is_candidate) {
      /* To prevent from increasing register pressure, only
         move OP whose result TN is dedicated or rematerializable,
         or global. Moving Local TNs out can increase register
         pressure, thus degrade the performance.

         P.S.
           We can also estimate the register pressure (the number of GTNs)
           and use it for deciding whether a local TN can be moved out.
           Not sure if it's worth trying at this time.
       */
      int nres = OP_results(op);
      if (nres == 1) {
        TN *res_tn = OP_result(op, 0);
        if (TN_is_rematerializable(res_tn) || TN_is_dedicated(res_tn) ||
            TN_is_global_reg(res_tn)) {
          Set_OP_flag1(op);
          len++;
        }
      }
    }
  }
         
  // Now, move out all OPs to Prolog BB
  if ( len > 0 ) {
    OP *next_op;
    BB *bb_prolog = Prolog_start();
    OP *op_xfer = BB_xfer_op(bb_prolog);
    bool trace_li_hoisting = Get_Trace(TP_CGPREP, 0x8000000);

    if (trace_li_hoisting) {
      fprintf(TFile, "%s", DBar);
      fprintf(TFile, "||||--- Begin of Loop-Invariant Hoisting ---\n");
      fprintf(TFile, "|||| Following Ops are hoisted from loop in BB %d into BB %d \n\n", 
		     BB_id(body), BB_id(bb_prolog));
    }

    if (op_xfer == NULL) {
      for (op = BB_first_op(body); op != NULL; op = next_op) {
        next_op =  OP_next(op);
        if (OP_flag1(op)) {
          BB_Move_Op_To_End(bb_prolog, body, op);

	  if (trace_li_hoisting) {
            Print_OP(op);
          } 
        }
      }
    } else {
      for (op = BB_first_op(body); op != NULL; op = next_op) {
        next_op =  OP_next(op);
        if (OP_flag1(op)) {
          BB_Move_Op_Before(bb_prolog, op_xfer, body, op);

	  if (trace_li_hoisting) {
            Print_OP(op);
          } 
        }
      }
    }

    if (trace_li_hoisting) {
      fprintf(TFile, "\n||||--- BBs after hoisting ---\n\n");
      Print_BB(bb_prolog);
      Print_BB(body);
      fprintf(TFile, "\n||||--- End of Loop-Invariant Hoisting ---\n");
      fprintf(TFile, "%s", DBar);
    }
  }

  CG_DEP_Delete_Graph(body);
}

// old interface
void
CG_LOOP_Recompute_Liveness(LOOP_DESCR *loop)
{
  Is_True(loop == Current_CG_LOOP->Loop(),
	  ("loop != Current_CG_LOOP"));
  Current_CG_LOOP->Recompute_Liveness();
}


TN *CG_LOOP_Backpatch_Find_Body_TN(BB *bb, TN *tn, UINT8 *omptr)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp = CG_LOOP_Backpatch_First(bb, NULL);
  while (bp && CG_LOOP_BACKPATCH_non_body_tn(bp) != tn) 
    bp = CG_LOOP_Backpatch_Next(bp);
  if (bp && omptr) *omptr = CG_LOOP_BACKPATCH_omega(bp);
  return bp ? CG_LOOP_BACKPATCH_body_tn(bp) : NULL;
}


TN *CG_LOOP_Backpatch_Find_Non_Body_TN(BB *bb, TN *tn, UINT8 om)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp = CG_LOOP_Backpatch_First(bb, tn);
  while (bp && CG_LOOP_BACKPATCH_omega(bp) != om) 
    bp = CG_LOOP_Backpatch_Next(bp);
  return bp ? CG_LOOP_BACKPATCH_non_body_tn(bp) : NULL;
}

CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Add(BB *bb, TN *non_body_tn,
					 TN *body_tn, UINT8 omega)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  BOOL isprolog = (bb == CG_LOOP_prolog);
  CG_LOOP_BACKPATCH *bps = isprolog ? prolog_backpatches : epilog_backpatches;
  CG_LOOP_BACKPATCH *bp;

  Is_True(bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	    ("bb not CG_LOOP_prolog or CG_LOOP_epilog"));

  /* Special case: see PV 389526. */
  if (!isprolog && TN_is_zero_reg(non_body_tn)) return NULL;

  for (bp = bps; bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    if (isprolog && bp->body_tn == body_tn && bp->omega == omega) {
      FmtAssert(bp->non_body_tn == non_body_tn,
		("conflicting prolog backpatch for TN%d[%d]",
		 TN_number(body_tn), omega));
      return bp;
    } else if (!isprolog && bp->non_body_tn == non_body_tn) {
      FmtAssert(bp->body_tn == body_tn && bp->omega == omega,
		("conflicting epilog backpatch for TN%d",
		 TN_number(non_body_tn)));
      return bp;
    }
  }

  bp = TYPE_MEM_POOL_ALLOC(CG_LOOP_BACKPATCH, &MEM_phase_nz_pool);
  bp->non_body_tn = non_body_tn;
  bp->body_tn = body_tn;
  bp->omega = omega;
  bp->next = bps;
  if (isprolog)
    prolog_backpatches = bp;
  else
    epilog_backpatches = bp;

  return bp;
}


CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_First(BB *bb, TN *body_tn)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp;
  Is_True(bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	  ("bb not CG_LOOP_prolog or CG_LOOP_epilog"));
  bp = (bb == CG_LOOP_prolog) ? prolog_backpatches : epilog_backpatches;
  while (bp && body_tn && bp->body_tn != body_tn) 
      bp = bp->next;
  if (bp == NULL)
    return bp;
  return body_tn ? _CG_LOOP_BP_limit_iter(bp) : bp;
}


CG_LOOP_BACKPATCH *CG_LOOP_Backpatch_Next(CG_LOOP_BACKPATCH *bp)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  if (_CG_LOOP_BP_iter_limited(bp)) {
    TN *body_tn;
    bp = _CG_LOOP_BP_actual_ptr(bp);
    body_tn = bp->body_tn;
    do {
      bp = bp->next;
    } while (bp && bp->body_tn != body_tn);
    if (bp == NULL)
      return bp;
    return _CG_LOOP_BP_limit_iter(bp);
  } else {
    return bp->next;
  }
}


void CG_LOOP_Backpatch_Delete(BB *bb, CG_LOOP_BACKPATCH *the_bp)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp;
  CG_LOOP_BACKPATCH *prev_bp;
  CG_LOOP_BACKPATCH **bpp;

  Is_True(bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	  ("bb not CG_LOOP_prolog or CG_LOOP_epilog"));

  the_bp = _CG_LOOP_BP_actual_ptr(the_bp);
  bpp = (bb == CG_LOOP_prolog) ? &prolog_backpatches : &epilog_backpatches;

  prev_bp = NULL;
  for (bp = *bpp; bp; bp = bp->next) {
    if (bp == the_bp) break;
    prev_bp = bp;
  }

  if (!bp) return;

  if (prev_bp) {
    prev_bp->next = bp->next;
  } else {
    *bpp = bp->next;
  }
}



void CG_LOOP_Backpatch_Replace_Non_Body_TN(BB *bb, TN *tn, TN *newtn)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp;

  Is_True(bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	  ("bb not CG_LOOP_prolog or CG_LOOP_epilog"));

  bp = (bb == CG_LOOP_prolog) ? prolog_backpatches : epilog_backpatches;
  
  for ( ; bp ; bp = bp->next )
    if ( bp->non_body_tn == tn ) bp->non_body_tn = newtn;

  return;
}

void CG_LOOP_Backpatch_Replace_Body_TN(BB *bb, TN *tn, TN *newtn,
				       INT16 om_adj)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp;

  Is_True(bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	  ("bb not CG_LOOP_prolog or CG_LOOP_epilog"));

  bp = (bb == CG_LOOP_prolog) ? prolog_backpatches : epilog_backpatches;
  
  for ( ; bp ; bp = bp->next ) 
    if ( bp->body_tn == tn ) {
      bp->body_tn = newtn;
      bp->omega+= om_adj;
    }

  return;

}


void CG_LOOP_Backpatch_Trace(BB *bb, CG_LOOP_BACKPATCH *bp)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  Is_True(bb == NULL || bb == CG_LOOP_prolog || bb == CG_LOOP_epilog,
	  ("bb not NULL, CG_LOOP_prolog or CG_LOOP_epilog"));

  bp = _CG_LOOP_BP_actual_ptr(bp);
  if (bp) {
    if (bb == CG_LOOP_epilog) {
      fprintf(TFile, "<bp>   TN%d <- TN%d[%d]\n", TN_number(bp->non_body_tn),
	      TN_number(bp->body_tn), bp->omega);
    } else {
      fprintf(TFile, "<bp>   TN%d[%d] %s TN%d\n", TN_number(bp->body_tn),
	      bp->omega, bb == CG_LOOP_prolog ? "<-" : "",
	      TN_number(bp->non_body_tn));
    }
  } else if (bb) {
    BOOL prolog = bb == CG_LOOP_prolog;
    fprintf(TFile, "<bp> %slog backpatches:\n", prolog ? "pro" : "epi");
    for (bp = prolog ? prolog_backpatches : epilog_backpatches; bp;
	 bp = bp->next)
      CG_LOOP_Backpatch_Trace(bb, bp);
    fprintf(TFile,"\n");
  } else {
    CG_LOOP_Backpatch_Trace(CG_LOOP_prolog, NULL);
    CG_LOOP_Backpatch_Trace(CG_LOOP_epilog, NULL);
  }
}


/* ====================================================================
 *
 * CG_LOOP_Remove_Notations
 *
 * When CG_LOOP_Remove_Notations has finished, all omega and backpatch
 * notation can be safely discarded without changing the behavior of
 * the loop.  In particular:
 * - All prolog backpatches will have the form  TN200[1] <-- TN200;
 * - All epilog backpatches will have the form  TN200 <-- TN200[0]; and
 * - Non-zero omegas, namely omega == 1, will only occur in loop
 *   operands referencing TNs whose definitions do not precede the use.
 *
 * CG_LOOP_Remove_Notations accomplishes this goal by identifying TNs
 * appearing as body TNs in prolog and epilog backpatches, or occurring
 * as operands with omega > 1 (or with omega == 1 but succeeding its
 * first definition).  Then new TNs and TN copies are inserted to store
 * past values of these identified TNs.  Finally, these new TNs are
 * substituted for old TNs and omegas appearing in loop body operands
 * and prolog and epilog backpatches.
 *
 * In order to keep the number of new TNs and TN copies as low as
 * possible, the TN copies are inserted immediately before the first
 * definition of the original TN.  (There may be multiple definitions.)
 * For example:
 *
 *                             BEFORE                     AFTER
 *
 * Prolog:                                           TN100 <-- TN201
 *                                                   TN101 <-- TN202
 *                                                   TN102 <-- TN203
 *                                                   TN103 <-- TN204
 *
 * Prolog backpatches:  TN100[1] <-- TN201           TN100[1] <-- TN100
 *                      TN100[2] <-- TN202           TN101[1] <-- TN101
 *                      TN100[3] <-- TN203           TN102[1] <-- TN102
 *                      TN100[4] <-- TN204           TN103[1] <-- TN103
 *
 * In the loop body:    ..... <-- TN100[1]           ..... <-- TN100[1]
 *                      ..... <-- TN100[2]           ..... <-- TN101[1]
 *                      ..... <-- TN100[3]           ..... <-- TN102[1]
 *                      ..... <-- TN100[4]           ..... <-- TN103[1]
 *
 *                                                   TN104 <-- TN103[1]
 *                                                   TN103 <-- TN102[1]
 *                                                   TN102 <-- TN101[1]
 *                                                   TN101 <-- TN100[1]
 *                      TN100 <-- ........           TN100 <-- .....
 *
 *                      ..... <-- TN100[0]           ..... <-- TN100[0]
 *                      ..... <-- TN100[1]           ..... <-- TN101[0]
 *                      ..... <-- TN100[2]           ..... <-- TN102[0]
 *                      ..... <-- TN100[3]           ..... <-- TN103[0]
 *
 * Epilog backpatches:  TN300 <-- TN100[0]           TN100 <-- TN100[0]
 *                      TN301 <-- TN100[1]           TN101 <-- TN101[0]
 *                      TN302 <-- TN100[2]           TN102 <-- TN102[0]
 *                      TN303 <-- TN100[3]           TN103 <-- TN103[0]
 *
 * Epilog:                                           TN300 <-- TN100
 *                                                   TN301 <-- TN101
 *                                                   TN302 <-- TN102
 *                                                   TN303 <-- TN103
 *
 * The first definition of a TN is handled uniquely.  If the first
 * definition of TN100 has TN100[1] as an operand, the operand TN100[1]
 * is left alone (and not replaced by TN101[0]).  However, TN100[2],
 * TN100[3], TN100[4] would still be replaced by TN102[0], TN103[0],
 * TN104[0] if they appeared as operands in the first definition of
 * TN100.
 *
 * In order to minimize the number of TN copies inserted into the
 * prolog and epilog, the implementation attempts to reuse the non-body
 * TNs from the prolog and epilog backpatches.  (In the above example,
 * TN300, ..., TN303 would replace TN101, ..., TN104 if the former TNs
 * do not already appear within the loop.  The secondary choices would
 * be TN201,..., TN204, with new TNs generated as a last resort.)
 *
 *
 * Several TN_MAPs are used to store information during the work of
 * Remove_Notations:
 *
 * tn_def_map stores the first definition of each TN within the loop
 * body.
 *
 * tn_copies_map stores the number of new TNs and TN copies that will
 * be needed for each non-global TN within the loop body.  In the
 * example above, tn_copies_map of TN100 is 3.
 *
 * new_tn_map stores all the new TNs after they are generated.  In the
 * example above, new_tn_map of TN100 is the TN* array new_tn_array,
 * where new_tn_array[0] == TN101, new_tn_array[1] == TN102, and
 * new_tn_array[2] == TN103.  After tn_copies_map is initialized,
 * if copies = hTN_MAP32_Get(tn_copies_map, tn) is positive, then
 * hTN_MAP_Get(new_tn_map, tn) is an array of TN* with copies - 1
 * entries.
 *
 * need_copies is a TN_LIST of all TNs with tn_copies_map[] > 0.
 *
 *
 * CG_LOOP_Remove_Notations uses two helper procedures to complete its
 * work: Count_Copies_Needed and Generate_Copy_TNs
 * These helper procedures are also invoked during loop unrolling by
 * Unroll_Make_Remainder_Loop.
 *
 * Count_Copies_Needed examines prolog and epilog backpatches and loop
 * body operands for omega values that will require additional TNs and
 * TN copies to remove.  It initializes the values of tn_def_map and
 * tn_copies_map for the current loop, and it allocates, creates and
 * returns the TN_LIST need_copies.  Count_Copies_Needed also removes
 * unnecessary prolog backpatches.
 *
 * Generate_Copies_TN generates new TNs for new_tn_map.  It attempts to
 * reuse non-body TNs from epilog and prolog backpatches, in order to
 * minimize the number of copies that will need to be inserted.
 *
 * Remove_Notations_Without_Copies performs the code transformation for
 * the case in which no additional TNs are needed.  This only requires
 * updating the prolog and epilog backpatches, and possible inserting
 * TN copy OPs into the prolog and epilog.
 *
 * Remove_Notations_With_Copies performs the code transformation for
 * the general case, which requires the insertion of TN copy OPs into
 * the loop body, and the transformations if loop body operands and
 * prolog and epilog backpatches described above.
 *
 * ====================================================================
 */


static TN_LIST *Count_Copies_Needed(BB *body, hTN_MAP tn_def_map,
				    hTN_MAP32 tn_copies_map,
				    MEM_POOL *pool)
{
  TN_LIST *need_copies = NULL;

  // For bodies that has been SWP'ed do not generate any more copies
  // This is normally not a problem but when the main algorithm does not
  // handle the case when there is an register anti dependence among two
  // ops that are bundled in the same cycle and the last use is assigned a
  // slot number larger than the first def.
  ANNOTATION *annot = ANNOT_Get(BB_annotations(body), ANNOT_ROTATING_KERNEL);
  if (annot) {
    ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);
    if (info && ROTATING_KERNEL_INFO_succeeded(info))
      return NULL;
  }

  // Examine each OP within the loop body, and initialize values
  // for the maps tn_def_map and tn_copies_map.

  for (OP *op = BB_first_op(body); op != NULL; op = OP_next(op)) {

    // Initialize tn_def_map to be the first definition OP for each TN.
    // Handle results before operands, because of the case
    //   TN100 <-- TN100[i]
    for (INT res = OP_results(op) - 1; res >= 0; --res) {
      TN *tn = OP_result(op, res);
      if (hTN_MAP_Get(tn_def_map, tn) == NULL)
	hTN_MAP_Set(tn_def_map, tn, (void *) op);
    }

    for (INT opnd = OP_opnds(op) - 1; opnd >= 0; --opnd) {
      TN *tn = OP_opnd(op, opnd);
      if ( ! TN_is_register(tn)) continue;
      INT copies = OP_omega(op, opnd);
      OP *tn_def_op = (OP *) hTN_MAP_Get(tn_def_map, tn);
      if (tn_def_op == NULL || (tn_def_op == op && copies == 1))
	--copies;
      if (copies > 0) {
	INT max_copies = hTN_MAP32_Get(tn_copies_map, tn);
	if (copies > max_copies) {
	  hTN_MAP32_Set(tn_copies_map, tn, copies);
	  if (max_copies == 0)
	    need_copies = TN_LIST_Push(tn, need_copies, pool);
	}
      }
    }
  }

  // Update tn_copies_map for all TNs appearing in loop epilog
  // backpatches

  CG_LOOP_BACKPATCH *bp;
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL);
       bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    INT copies = CG_LOOP_BACKPATCH_omega(bp);
    if (copies > 0) {
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      INT max_copies = hTN_MAP32_Get(tn_copies_map, body_tn);
      if (copies > max_copies) {
	hTN_MAP32_Set(tn_copies_map, body_tn, copies);
	if (max_copies == 0)
	  need_copies = TN_LIST_Push(body_tn, need_copies, pool);
      }
    }
  }

  // Remove any unnecessary backpatches from the loop prolog.
  // These may appear as a result of the earlier elimination of loop TNs

  if (need_copies == NULL) {
    CG_LOOP_BACKPATCH *bp_next;
    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL);
	 bp; bp = bp_next) {
      bp_next = CG_LOOP_Backpatch_Next(bp);
      if (CG_LOOP_BACKPATCH_omega(bp) > 1)
	CG_LOOP_Backpatch_Delete(CG_LOOP_prolog, bp);
    }
  } else {
    CG_LOOP_BACKPATCH *bp_next;
    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL);
	 bp; bp = bp_next) {
      bp_next = CG_LOOP_Backpatch_Next(bp);
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      INT omega = CG_LOOP_BACKPATCH_omega(bp);
      if (hTN_MAP32_Get(tn_copies_map, body_tn) < omega - 1)
	CG_LOOP_Backpatch_Delete(CG_LOOP_prolog, bp);
    }
  }

  return need_copies;
}


void Generate_Copy_TNs(BB *body, hTN_MAP32 tn_copies_map,
		       hTN_MAP new_tn_map, TN_LIST *need_copies,
		       MEM_POOL *pool)
{
  // Allocate memory for all replacement TNs, initialized to NULL

  TN_LIST *p;
  for (p = need_copies; p != NULL; p = TN_LIST_rest(p)) {
    TN *tn = TN_LIST_first(p);
    INT copies = hTN_MAP32_Get(tn_copies_map, tn);

    // Verify that Count_Copies_Needed worked
    Is_True(copies > 0, ("Generate_Copy_TNs: need_copies has TN%d with"
			 " copies == 0", TN_number(tn)));
    Is_True(hTN_MAP_Get(new_tn_map, tn) == NULL,
	    ("Generate_Copy_TNs: need_copies contains duplicates"));

    TN **new_tn_array = TYPE_MEM_POOL_ALLOC_N(TN *, pool, copies);
    hTN_MAP_Set(new_tn_map, tn, (void *) new_tn_array);
    for (INT i = copies - 1; i >= 0; --i)
      new_tn_array[i] = NULL;
  }

  // Try to use the same TN within the loop body as in the epilog so
  // that epilog backpatches will not require additional copy OPs, but
  // only if this TN is not already used within the loop

  CG_LOOP_BACKPATCH *bp;
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp)) {
    INT copy = CG_LOOP_BACKPATCH_omega(bp);
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    if (copy > 0 && ! TN_is_global_reg(non_body_tn)) {
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
      if (new_tn_array[copy - 1] == NULL) {
	GTN_UNIVERSE_Add_TN(non_body_tn);
	new_tn_array[copy - 1] = non_body_tn;
      }
    }
  }

  // Try to use the same TN within the loop body as in the prolog so
  // that prolog backpatches will not require additional copy OPs, but
  // make sure this TN is not already live within the loop, and don't
  // use the same one twice.

  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp)) {
    INT copy = CG_LOOP_BACKPATCH_omega(bp) - 1;
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    if (copy > 0 && TN_is_register(non_body_tn)
	&& ! TN_is_dedicated(non_body_tn)
	&& ! TN_is_global_reg(non_body_tn)) {
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
      if (new_tn_array[copy - 1] == NULL) {
	GTN_UNIVERSE_Add_TN(non_body_tn);
	new_tn_array[copy - 1] = non_body_tn;
      }
    }
  }

  // Initialize all remaining replacement TNs to newly created TNs

  for (p = need_copies; p != NULL; p = TN_LIST_rest(p)) {
    TN *tn = TN_LIST_first(p);
    INT copies = hTN_MAP32_Get(tn_copies_map, tn);
    TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, tn);
    for (INT i = 0; i < copies; ++i)
      if (new_tn_array[i] == NULL)
	new_tn_array[i] = Build_TN_Like(tn);
  }
}


void Remove_Notations_Without_Copies(BB *head, BB *tail)
{
  // No new TNs are required, so only the prolog and epilog
  // backpatches may require updating

  // Update the epilog backpatches, inserting new copy OPs into the
  // epilog when necessary

  CG_LOOP_BACKPATCH *bp, *bp_next;
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp)) {
    Is_True(CG_LOOP_BACKPATCH_omega(bp) == 0,
	    ("Remove_Notations_Without_Copies: omega > 0"));
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);

    // Update the epilog backpatch, inserting a TN copy if necessary
    if (non_body_tn != body_tn) {
      CGPREP_Copy_TN_Into_BB(non_body_tn, body_tn, tail, NULL, 0, TRUE);
      CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
    }
  }

  // Update loop prolog backpatches, inserting new copy OPs into the
  // prolog when necessary

  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL); bp;
       bp = bp_next) {
    bp_next = CG_LOOP_Backpatch_Next(bp);
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);

    // Update the prolog backpatch, inserting a TN copy if necessary
    if (body_tn != non_body_tn) {
      CGPREP_Copy_TN_Into_BB(body_tn, non_body_tn,
			     head, BB_last_op(head), 0, FALSE);
      CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
    }
  }
}


void Remove_Notations_With_Copies(BB *body, BB *head, BB *tail,
				  hTN_MAP tn_def_map, hTN_MAP32 tn_copies_map,
				  TN_LIST *need_copies, hTN_MAP new_tn_map)
{
  // Replace the epilog backpatches with new TNs, inserting new
  // copy OPs into the epilog when necessary.

  CG_LOOP_BACKPATCH *bp, *bp_next;
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp)) {

    // Look up new body_tn, and update omega
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    INT omega = CG_LOOP_BACKPATCH_omega(bp);
    if (omega > 0) {
      CG_LOOP_BACKPATCH_Set_omega(bp, 0);
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
      body_tn = new_tn_array[omega - 1];
      CG_LOOP_BACKPATCH_Set_body_tn(bp, body_tn);
    }

    // Update non_body_tn, inserting copy if necessary
    if (non_body_tn != body_tn) {
      CGPREP_Copy_TN_Into_BB(non_body_tn, body_tn, tail, NULL, 0, TRUE);
      CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
    }
  }

  // Replace old operand TNs and omegas with newly generated TNs

  for (OP *op = BB_first_op(body); op != NULL; op = OP_next(op))
    for (INT opnd = OP_opnds(op) - 1; opnd >= 0; --opnd) {

      // Skip if omega == 0; also skip special case TN100 <-- TN100[1]
      INT omega = OP_omega(op, opnd);
      if (omega == 0) continue;
      TN *tn = OP_opnd(op, opnd);
      OP *def_op = (OP *) hTN_MAP_Get(tn_def_map, tn);
      Is_True(def_op, ("CG_LOOP_Remove_Notations: NULL def_op"));
      if (omega == 1 && op == def_op) continue;

      // Update operand omega
      if (OP_Precedes(op, def_op)) {
	if (omega == 1) continue;
	--omega;
	Set_OP_omega(op, opnd, 1);
      } else
	Set_OP_omega(op, opnd, 0);

      // Update operand TN
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, tn);
      Set_OP_opnd(op, opnd, new_tn_array[omega - 1]);
    }

  // Update loop prolog backpatches with new TNs

  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL);
       bp; bp = bp_next) {
    bp_next = CG_LOOP_Backpatch_Next(bp);

    // Look up new body_tn, and update omega
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp); // old body_tn
    INT omega = CG_LOOP_BACKPATCH_omega(bp);
    if (omega > 1) {

      CG_LOOP_BACKPATCH_Set_omega(bp, 1);
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
      body_tn = new_tn_array[omega - 2];  // new body_tn
      CG_LOOP_BACKPATCH_Set_body_tn(bp, body_tn);
    }

    // Update non_body_tn, inserting copy if necessary
    TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    if (body_tn != non_body_tn) {
      CGPREP_Copy_TN_Into_BB(body_tn, non_body_tn,
			     head, BB_last_op(head), 0, FALSE);
      CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
    }
  }

  // Insert TN copies before that TN's first definition

  for (TN_LIST *p = need_copies; p != NULL; p = TN_LIST_rest(p)) {
    TN *tn = TN_LIST_first(p);
    OP *op = (OP *) hTN_MAP_Get(tn_def_map, tn);
    INT copies = hTN_MAP32_Get(tn_copies_map, tn);
    TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, tn);

    // Insert new TN copies, in order, before first definition OP
    // Index cp must be positive and decreasing
    for (INT cp = copies - 1; cp > 0; --cp)
      CGPREP_Copy_TN(new_tn_array[cp], new_tn_array[cp - 1], op, 1, TRUE);
    CGPREP_Copy_TN(new_tn_array[0], tn, op, 1, TRUE);
  }
}


void CG_LOOP_Remove_Notations(CG_LOOP& cl, BB *head, BB *tail)
{
  LOOP_DESCR *loop = cl.Loop();
  BB *body = cl.Loop_header();

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop(loop, "Before CG_LOOP_Remove_Notations");
  }

  // Determine the number of TN copies required for the TNs in this loop;

  // tn_def_map stores the first definition of each TN within the loop body
  // tn_copies_map stores the number of new TNs and TN copies that will be
  //   needed for each non-global TN within the loop body.  In the example
  //   above, tn_copies_map of TN100 is 3
  // need_copies is a list of all TNs with tn_copies_map[] > 0

  CXX_MEM_POOL pool("CG_LOOP_Remove_Notations", FALSE);
  hTN_MAP   tn_def_map    = hTN_MAP_Create(pool());
  hTN_MAP32 tn_copies_map = hTN_MAP32_Create(pool());
  TN_LIST *need_copies = Count_Copies_Needed(body, tn_def_map,
					     tn_copies_map, pool());

  if (need_copies == NULL) {

    // If no new TNs are required, then only the prolog and epilog
    // backpatches may require updating
    Remove_Notations_Without_Copies(head, tail);

  } else {  // need_copies != NULL

    // Generate a map to keep track of new TNs.  The values of this
    // map are arrays of new TNs.

    hTN_MAP new_tn_map = hTN_MAP_Create(pool());
    Generate_Copy_TNs(body, tn_copies_map, new_tn_map, need_copies, pool());

    Remove_Notations_With_Copies(body, head, tail, tn_def_map,
				 tn_copies_map, need_copies, new_tn_map);
  }

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop( loop, "After CG_LOOP_Remove_Notations" );
  }
}


/* ====================================================================
 *
 *  Readers_Reach
 *
 *  Can all the readers of 'op' be rewritten to offset their loop
 *  invariant operands by 'count' * 'op's loop invariant operand?
 *  This is really checking of memory references that cannot be
 *  converted to the indexed form and whose literal fields cannot hold
 *  the offset implied by skipping count instances of 'op'.
 *
 *  Example:
 *
 *      iv = iv + 0x2000
 *         = 4(iv)
 *
 *      The memory reference reaches with a count of < 4 but not with
 *      a higher count, since 0x8000 is a negative 16 bit number.
 *
 * ====================================================================
 */

static BOOL
Readers_Reach(
  OP    *op,            /* IV update */
  INT32  count          /* Of loop overhead operations */
)
{
  INT64     offset;
  INT32     store_offset;
  TN       *liv_tn, *lit_tn;
  ARC_LIST *al;
  TN       *iv_tn = OP_result(op,0 /*???*/);

  if ( OP_opnd(op,0 /*???*/) == iv_tn )
    liv_tn = OP_opnd(op,1 /*???*/);
  else
    liv_tn = OP_opnd(op,0 /*???*/);

  for ( al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
    ARC *arc    = ARC_LIST_first(al);
    OP  *reader = ARC_succ(arc);

    if ( reader == op )
      continue;

    if ( OP_br(reader) )
      continue;

    if (    CGTARG_Have_Indexed_Mem_Insts()
         && OP_Is_Float_Mem(reader)
    ) {
      /* These can always be made to work by using a register to hold
       * the offset and doing an indexed load.
       */
      continue;
    }

    /* TODO: do we need to handle symbolic constants in the offset
     * fields here?  Having them there seems like an incorrect design,
     * so I'll guess we won't have to handle them.
     */

    store_offset = OP_store(reader) ? 1 : 0; /*???*/

    if ( OP_opnd(reader,store_offset) == iv_tn )
      lit_tn = OP_opnd(reader,store_offset + 1);
    else
      lit_tn = OP_opnd(reader,store_offset);

    FmtAssert(OP_iadd(op) || OP_isub(op),("Expected iadd or isub."));

    if ( OP_iadd(op) )
      offset = TN_value(lit_tn) + TN_value(liv_tn) * count;
    else
      offset = TN_value(lit_tn) - TN_value(liv_tn) * count;

    if ( ! TI_TOP_Can_Have_Immediate(offset,OP_code(reader)) )
       return FALSE;
   }

   return TRUE;
 }



void CG_LOOP_Clear_SCCs(LOOP_DESCR *loop)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface description.
 * -----------------------------------------------------------------------
 */
{
  BB *bb = LOOP_DESCR_loophead(loop);
  OP *op;
  FOR_ALL_BB_OPs(bb, op) {
    _CG_LOOP_INFO *info = _CG_LOOP_info(op);
    /*
     * Each ARC is in one ancestor list and one descendent list.
     * To avoid deleting the same ARC twice, we'll just delete ARCs
     * from ancestor lists (could just as well use descendent
     * lists instead).
     */
    CG_DEP_Delete_SCC_Arcs(info->scc_ancestors);
    info->scc_ancestors = NULL;
    info->scc_descendents = NULL;
    info->scc = NULL;
  }
}

/* =====================================================================
 *			 Inner Loop Unrolling
 * =====================================================================
 */

 
/* ---------------------------------------------------------------------
 *			      TN mapping
 *
 * The following functions provide a service to rename TNs defined in
 * the loop body, and map the original names to new names given an
 * unrolling number.
 *
 * void unroll_names_init(LOOP_DESCR *loop, UINT16 ntimes, MEM_POOL *pool)
 *   Initialization function to be called before attempting to unroll
 *   <loop> <ntimes>.  Allocates memory from <pool> that must not be
 *   released until unroll_names_finish is called.
 *
 * TN *unroll_names_get(TN *tn, UINT16 unrolling)
 *   Requires: unrolling < ntimes (from last call to unroll_names_init).
 *   Return a TN representing <tn> in the given <unrolling>.  If <tn>
 *   isn't defined in the loop, it is returned as is.
 *
 * void unroll_names_finish(void)
 *   Cleanup function to be called after last call to unroll_names_get
 *   for a given unrolling attempt.
 *
 * ---------------------------------------------------------------------
 */

/* We mark TNs with this value in the map to indicate that they cannot
   be renamed in the loop. */
#define TN_NO_RENAME ((TN **)-1)
  
static TN_MAP unroll_names;


static void unroll_names_finish(void)
{
  TN_MAP_Delete(unroll_names);
  unroll_names_valid = FALSE;
}


static void unroll_names_init_tn(TN *result, UINT16 ntimes, MEM_POOL *pool)
{
  TN **entry = TYPE_MEM_POOL_ALLOC_N(TN *, pool, ntimes);
  UINT16 unrolling;
  TN_MAP_Set(unroll_names, result, entry);
  for (unrolling = 0; unrolling < ntimes; ++unrolling)
    if (TN_is_dedicated(result))
      entry[unrolling] = result;
    else
      entry[unrolling] = Dup_TN(result);
}
  

static void unroll_names_init(LOOP_DESCR *loop, UINT16 ntimes, MEM_POOL *pool)
/* -----------------------------------------------------------------------
 * See above for interface description.
 * -----------------------------------------------------------------------
 */
{
  BB_SET *bbs = LOOP_DESCR_bbset(loop);
  Is_True(BB_SET_Size(bbs) == 1, ("unroll_names_init:  only support single BB loop."));

  BB *bb = LOOP_DESCR_loophead(loop);
  OP *op;

  Is_True(!unroll_names_valid, ("unroll_names_finish not called."));

  unroll_names = TN_MAP_Create();
  unroll_names_valid = TRUE;

  FOR_ALL_BB_OPs(bb, op) {
    for (INT i = 0; i < OP_results(op); ++i) {
      TN *result_tn = OP_result(op,i);
      
      if (!OP_cond_def(op) || 
	  !TN_is_global_reg(result_tn)) {

	/* Don't rename same_res TN's. */
	if (OP_same_res(op) && (CGPREP_Same_Res_Opnd(op, i) != -1) &&
	    !TN_MAP_Get(unroll_names, result_tn))
	  TN_MAP_Set(unroll_names, result_tn, TN_NO_RENAME);
	else if (!TN_MAP_Get(unroll_names, result_tn))
	  unroll_names_init_tn(result_tn, ntimes, pool);
      }
    }
  }

#ifdef Is_True_On
  FOR_ALL_BB_OPs(bb, op) {
    for (INT i = 0; i < OP_opnds(op); ++i) {
      TN *tn = OP_opnd(op,i);
      if (TN_is_register(tn) &&
	  OP_omega(op, i) >= 2)
	Is_True((TN_MAP_Get(unroll_names, tn) != NULL) &&
		(TN_MAP_Get(unroll_names, tn) != TN_NO_RENAME),
		("unroll_names_init: must rename omega TN."));
    }
  }
#endif
}


inline TN *unroll_names_get(TN *tn, UINT16 unrolling)
{
  if (TN_is_register(tn)) {
    TN **entry = (TN **)TN_MAP_Get(unroll_names, tn);
    return ((entry && (entry != TN_NO_RENAME)) ? entry[unrolling] : tn);
  }
  return tn;
}

/* hack just to export unroll_names_get quickly for beta */
TN *CG_LOOP_unroll_names_get(TN *tn, UINT16 unrolling)
{
  if ( tn == NULL ) return NULL;
  return unroll_names_get(tn, unrolling);
}


static void remainder_head_note_handler(NOTE_ACTION action, NOTE_INFO *info,
					FILE *file)
/* -----------------------------------------------------------------------
 *  Handler for NOTE_REMAINDER_HEAD note.
 * -----------------------------------------------------------------------
 */
{
  NOTE_REMAINDER_HEAD *info_u = (NOTE_REMAINDER_HEAD *)info;
  UINT16 ntimes = NOTE_ntimes(info_u);
  BOOL ctrip = NOTE_const_trip(info_u);

  switch (action) {
  case NOTE_PRINT_TO_FILE:
    if (ctrip) {
      fprintf(file,
	      "%s<loop> Unrolling remainder loop (%d iteration%s)\n",
	      ASM_CMNT_LINE, ntimes, ntimes == 1 ? "" : "s");
    } else {
      fprintf(file,
	      "%s<loop> Unrolling remainder loop (at most %d iteration%s)\n",
	      ASM_CMNT_LINE, ntimes-1, ntimes-1 == 1 ? "" : "s");
    }
    if (ntimes == 0)
      DevWarn("Found remainder head note with ntimes = 0");
    break;
  case NOTE_PRINT_TO_ANL_FILE:
    /* ignore for now */
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file, "remainder_head_note_handler");
    break;
  default:
    Is_True(FALSE, ("Didn't recognize action"));
  }
}



static void note_remainder_head(BB *head, UINT16 ntimes, UINT16 trips_if_const)
/* -----------------------------------------------------------------------
 * Attach a note to <head> saying it's the remainder loop for a loop
 * unrolled <ntimes>.  If <trips_if_const> is nonzero, it indicates
 * the trip count is the constant given.
 * -----------------------------------------------------------------------
 */
{
  NOTE_REMAINDER_HEAD *note;
  note = TYPE_MEM_POOL_ALLOC(NOTE_REMAINDER_HEAD, &MEM_pu_nz_pool);
  NOTE_const_trip(note) = trips_if_const > 0;
  NOTE_ntimes(note) = trips_if_const ? trips_if_const : ntimes;
  NOTE_Add_To_BB(head, remainder_head_note_handler, (NOTE_INFO *)note);
}



static void not_unrolled_note_handler(NOTE_ACTION action, NOTE_INFO *info,
				      FILE *file)
/* -----------------------------------------------------------------------
 *  Handler for NOTE_NOT_UNROLLED note.
 * -----------------------------------------------------------------------
 */
{
  NOTE_NOT_UNROLLED *info_u = (NOTE_NOT_UNROLLED *)info;
  char *reason = NOTE_reason(info_u);

  switch (action) {
  case NOTE_PRINT_TO_FILE:
    fprintf(file, "%s<loop> Not unrolled: %s\n", ASM_CMNT_LINE, reason);
    break;
  case NOTE_PRINT_TO_ANL_FILE:
    /* ignore for now */
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file, "not_unrolled_note_handler");
    break;
  default:
    Is_True(FALSE, ("Didn't recognize action"));
  }
}



static void note_not_unrolled(BB *head, char *reason, ...)
/* -----------------------------------------------------------------------
 * Attach a note to <head> saying it wasn't unrolled because <reason>.
 * <reason> and the following args are printf arguments.  <reason> must
 * expand to less than 256 characters (and should really by less than 80
 * for readability).
 * -----------------------------------------------------------------------
 */
{
  NOTE_NOT_UNROLLED *note;
  va_list args;
  char buf[256];

  /*
   * Emit reason into buf
   */
  va_start(args, reason);
  vsprintf(buf, reason, args);
  va_end(args);
  Is_True(strlen(buf) < 256, ("note_not_unrolled buffer overrun"));
  buf[255] = (char)0;	/* for robustness (though it's a little late) */

  note = TYPE_MEM_POOL_ALLOC(NOTE_NOT_UNROLLED, &MEM_pu_nz_pool);
  NOTE_reason(note) = TYPE_MEM_POOL_ALLOC_N(char, &MEM_pu_nz_pool,
					    strlen(buf)+1);
  strcpy(NOTE_reason(note), buf);
  NOTE_Add_To_BB(head, not_unrolled_note_handler, (NOTE_INFO *)note);
}


/* --------------------------------------------------------------------- */



#define is_power_of_two(i) (((i) & ((i)-1)) == 0)



inline UINT16 int_log2(UINT32 n)
/* -----------------------------------------------------------------------
 * Requires: n > 0.
 * Return the base-2 logarithm of <n> (truncated if <n> isn't a power of
 * two).
 * -----------------------------------------------------------------------
 */
{
  UINT16 result = 0;
  Is_True(n > 0, ("can't take logarithm of zero"));
  while ((1 << result) <= n)
    ++result;
  return --result;
}



static void unroll_guard_unrolled_body(LOOP_DESCR *loop,
				       LOOPINFO *unrolled_info,
				       TN *orig_trip_count_tn,
				       UINT32 ntimes)
/* -----------------------------------------------------------------------
 * Requires: !TN_is_constant(orig_trip_count_tn) && is_power_of_two(ntimes)
 *
 * Generates prolog/epilog code to avoid executing loop body when
 * <orig_trip_count_tn> / <ntimes> < 1.  Also replaces the trip count
 * TN in <unrolled_info> with a new one defined in CG_LOOP_prolog as the
 * number of trips for the unrolled loop.
 * -----------------------------------------------------------------------
 */
{
  if (!TN_is_constant(orig_trip_count_tn)) {
    INT64 trip_est = WN_loop_trip_est(LOOPINFO_wn(unrolled_info));
    TN *new_trip_count_tn = Build_TN_Like(orig_trip_count_tn);
    INT32 trip_size = TN_size(orig_trip_count_tn);
    TYPE_ID ttype = TN_mtype(orig_trip_count_tn);
    float ztrip_prob = 1.0 / MAX(trip_est, 1);
    float orig_post_prolog_freq = BB_freq(BB_next(CG_LOOP_prolog));
    OPS ops = OPS_EMPTY;
    BB *continuation_bb;
    LABEL_IDX continuation_lbl;

    Is_True(is_power_of_two(ntimes), ("not power of two"));
    Is_True(!TN_is_constant(orig_trip_count_tn), ("trip count is constant"));

    LOOPINFO_trip_count_tn(unrolled_info) = new_trip_count_tn;
    LOOPINFO_max_trip_count_tn(unrolled_info) = new_trip_count_tn;
    LOOPINFO_swp_failure_code(unrolled_info) = SWP_NOT_ATTEMPTED;
    LOOPINFO_swp_unallococatable_rc(unrolled_info) = ISA_REGCLASS_UNDEFINED;

    extend_epilog(loop);
    continuation_bb = CG_LOOP_epilog;
    continuation_lbl = Gen_Label_For_BB(continuation_bb);

    /* We know <orig_trip_count_tn's> value is positive, and <ntimes> is a power
   * of two, so we can divide <orig_trip_count_tn> by <ntimes> with:
   *   <new_trip_count_tn> <- [d]sra <orig_trip_count_tn> int_log2(ntimes)
   * and guard the unrolled loop with:
   *   beq continuation_lbl <new_trip_count_tn> 0
   */
    Exp_OP2((MTYPE_is_unsigned(ttype) ?
	     OPCODE_make_op(OPR_LSHR, ttype, MTYPE_V) :
	     OPCODE_make_op(OPR_ASHR, ttype, MTYPE_V)),
	    new_trip_count_tn,
	    orig_trip_count_tn,
	    Gen_Literal_TN(int_log2(ntimes), trip_size),
	    &ops);
    Exp_OP3v(OPC_FALSEBR,
	     NULL,
	     Gen_Label_TN(continuation_lbl,0),
	     new_trip_count_tn,
	     Gen_Literal_TN(0, trip_size),
	     V_BR_I8EQ,
	     &ops);
    BB_Append_Ops(CG_LOOP_prolog, &ops);
    Link_Pred_Succ_with_Prob(CG_LOOP_prolog, continuation_bb, ztrip_prob);
    Change_Succ_Prob(CG_LOOP_prolog, BB_next(CG_LOOP_prolog), 1.0 - ztrip_prob);
    BB_freq(BB_next(CG_LOOP_prolog)) = orig_post_prolog_freq;

    /* Extend prolog and epilog in case any further optimizations
   * want to use them.
   */
    extend_prolog();
    extend_epilog(loop);
  }
}



static void unroll_xfer_annotations(BB *unrolled_bb, BB *orig_bb)
/* -----------------------------------------------------------------------
 * Requires: <unrolled_bb> has unrolled OPs
 *
 * <unrolled_bb> is an unrolled version of <orig_bb>.  Look at the
 * annotations on <orig_bb>, handling them as follows:
 *   LABEL	do nothing (BBs must have unique labels)
 *   PRAGMA	copy to <unrolled_bb> (pragma WN shared, not copied)
 *   ENTRYINFO	do nothing (unrolled replicas shouldn't be entries)
 *   EXITINFO	copy to <unrolled_bb> and point sp_adj to spadjust OP
 *   CALLINFO	copy to <unrolled_bb> (call ST/WN shared, not copied)
 *   NOTE	copy to <unrolled_bb> (NOTE shared, not copied)
 *   LOOPINFO	do nothing (handled specially by unrolling routines)
 *   SWITCH 	do nothing
 *   ROTATING_KERNEL	copy to <unrolled_bb>
 *   ASMINFO	set asm_clobber for unrolled_bb if orig_bb has it
 * -----------------------------------------------------------------------
 */
{
  if (BB_has_pragma(orig_bb)) {
    BB_Copy_Annotations(unrolled_bb, orig_bb, ANNOT_PRAGMA);
  }

  if (BB_exit(orig_bb)) {
    ANNOTATION *ant = ANNOT_Get(BB_annotations(orig_bb), ANNOT_EXITINFO);
    if (ant == NULL) {
      DevWarn("BB_exit(BB:%d) set but no ANNOT_EXITINFO attached",
	      BB_id(orig_bb));
    } else {
      EXITINFO *orig_info = ANNOT_exitinfo(ant);
      EXITINFO *unrolled_info = TYPE_PU_ALLOC(EXITINFO);
      *unrolled_info = *orig_info;
      if (EXITINFO_sp_adj(orig_info)) {
	OP *sp_adj;
	FOR_ALL_BB_OPs_REV(unrolled_bb, sp_adj)
	  if (OP_code(sp_adj) == TOP_spadjust) break;
	if (sp_adj == NULL)
	  DevWarn("missing spadjust OP in unrolled BB:%d", BB_id(unrolled_bb));
	EXITINFO_sp_adj(unrolled_info) = sp_adj;
      }
      BB_Add_Annotation(unrolled_bb, ANNOT_PRAGMA, orig_info);
    }
  }

  if (BB_call(orig_bb)) {
    BB_Copy_Annotations(unrolled_bb, orig_bb, ANNOT_CALLINFO);
  }

  if (BB_has_note(orig_bb)) {
    BB_Copy_Annotations(unrolled_bb, orig_bb, ANNOT_NOTE);
  }
  if (BB_asm_clobber(orig_bb)) {
    Set_BB_asm_clobber(unrolled_bb);
  }
}
  

static BB *Unroll_Replicate_Body(LOOP_DESCR *loop, INT32 ntimes, BOOL unroll_fully)
/* -----------------------------------------------------------------------
 * Requires: unroll_make_remainder_loop has not yet been called
 *	     BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1
 * 
 * Construct <ntimes> unrollings of <loop>, attaching result in place
 * of <loop> before returning it.  Old loop body is left disconnected
 * from the control flow graph.  All TNs defined in <loop> will have
 * new names that can be found with unroll_names_get.  The resulting
 * loop will have a new label.
 * -----------------------------------------------------------------------
 */
{
  BB *body = LOOP_DESCR_loophead(loop);
  BB *unrolled_body = Gen_BB_Like(body);
  UINT16 unrolling;
  OP *op;
  ANNOTATION *annot = ANNOT_Get(BB_annotations(body), ANNOT_LOOPINFO);
  LOOPINFO *info = ANNOT_loopinfo(annot);
  TN *trip_count = LOOPINFO_trip_count_tn(info);
  INT16 new_trip_count_val;
  LOOPINFO *unrolled_info = TYPE_P_ALLOC(LOOPINFO);
  WN *wn = WN_COPY_Tree(LOOPINFO_wn(info));
  TYPE_ID ttype = WN_rtype(WN_loop_trip(wn));
  OPCODE opc_intconst = OPCODE_make_op(OPR_INTCONST, ttype, MTYPE_V);
  WN *ntimes_wn = WN_CreateIntconst(opc_intconst, ntimes);
  OPCODE opc_div = OPCODE_make_op(OPR_DIV, ttype, MTYPE_V);
  float old_freq;

  /* Catch calls that should be going to unroll_multi_bb instead */
  Is_True(BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1,
	    ("unroll_replicate_body passed multi-bb loop body"));

  if (BB_freq_fb_based(body)) Set_BB_freq_fb_based(unrolled_body);

  if (TN_is_constant(trip_count)) {
    new_trip_count_val = TN_value(trip_count) / ntimes;
    Is_True((unroll_fully && new_trip_count_val == 1) ||
	    (!unroll_fully && new_trip_count_val > 1),
	    ("new_trip_count_val does not make sense."));
  }

  /* Setup new <unrolled_body> LOOPINFO.
   */
  WN_set_loop_trip(wn, WN_CreateExp2(opc_div, WN_loop_trip(wn), ntimes_wn));
  WN_loop_trip_est(wn) = WN_loop_trip_est(wn) / ntimes;
  LOOPINFO_wn(unrolled_info) = wn;
  LOOPINFO_srcpos(unrolled_info) = LOOPINFO_srcpos(info);
  LOOPINFO_swp_failure_code(unrolled_info) = LOOPINFO_swp_failure_code(info);
  LOOPINFO_swp_unallococatable_rc(unrolled_info) =
	  				LOOPINFO_swp_unallococatable_rc(info);
  if (TN_is_constant(trip_count))
  {
    LOOPINFO_trip_count_tn(unrolled_info) =
      Gen_Literal_TN(new_trip_count_val, TN_size(trip_count));
    LOOPINFO_max_trip_count_tn(unrolled_info) =
      LOOPINFO_trip_count_tn(unrolled_info);
  }
  Set_BB_unrollings(unrolled_body, ntimes);
  if (unroll_fully) Set_BB_unrolled_fully(unrolled_body);
  BB_Add_Annotation(unrolled_body, ANNOT_LOOPINFO, unrolled_info);

  bool trace_pref = Get_Trace(TP_CGPREP, 2);

  /* Replicate the body <ntimes>.  Note that we don't emit a branch
   * if this isn't <!unroll_fully>.
   */
  for (unrolling = 0; unrolling < ntimes; unrolling++) {
    FOR_ALL_BB_OPs(body, op) {

      // Perform Prefetch pruning at unroll time
      if (OP_prefetch(op)) {

	WN *mem_wn = Get_WN_From_Memory_OP(op);
	Is_True(WN_operator(mem_wn) == OPR_PREFETCH,
		("wrong prefetch WHIRL node."));

	if (trace_pref && unrolling == 0)  // trace once per loop
	  if (mem_wn)
	    fprintf(TFile, "<cgpref> - 1L cache stride = %d, 2L cache stride = %d,"
		    " confidence = %d\n",
		    WN_pf_stride_1L(mem_wn),
		    WN_pf_stride_2L(mem_wn),
		    WN_pf_confidence(mem_wn));
	  else
	    fprintf(TFile, "<cgpref> pref wn not found.\n");

	if (Prefetch_Kind_Enabled(mem_wn)) {
	  int stride = WN_pf_stride_2L( wn ) ?  WN_pf_stride_2L( wn ) :  WN_pf_stride_1L(wn);
	  if (stride != 0 && (unrolling % stride) != 0) {
	    if (trace_pref)
	      fprintf(TFile, "<cgpref> pref pruned at unrolling %d.\n", unrolling);
	    continue;
	  }
	}
      }
      if (!OP_br(op) || !unroll_fully && unrolling == ntimes-1) {
	UINT8 opnd;
	UINT8 res;
	OP *new_op = Dup_OP(op);
	CGPREP_Init_Op(new_op);
	CG_LOOP_Init_Op(new_op);
	Copy_WN_For_Memory_OP(new_op, op);

        if (OP_loh(op)) 
          Set_OP_loh(new_op);

	Set_OP_unrolling(new_op, unrolling);
	Set_OP_orig_idx(new_op, OP_map_idx(op));
	Set_OP_unroll_bb(new_op, unrolled_body);
	for (res = 0; res < OP_results(op); ++res) {
	  TN *new_res = unroll_names_get(OP_result(op,res), unrolling);
	  Set_OP_result(new_op, res, new_res);
	}
	for (opnd = 0; opnd < OP_opnds(op); opnd++) {
	  INT omega = OP_omega(op,opnd);
	  INT adjust = omega + (ntimes - unrolling - 1);
	  INT new_omega = adjust / ntimes;
	  INT which = ntimes - 1 - (adjust - (new_omega * ntimes));

	  //  I think the above is equivalent to this. -Raymond
	  Is_True(new_omega * ntimes + (unrolling - which) == omega,
		  ("new_omega * ntimes + (unrolling - which) == omega."));

	  TN *new_tn = unroll_names_get(OP_opnd(op,opnd), which);
	  Is_True(omega == 0 || new_tn != NULL,
		  ("Replicate_Unroll_Body: TN with non-zero omega must be renamed."));
	  Set_OP_opnd(new_op, opnd, new_tn);

	  // omega of OP_br unchanged because it has only 1 copy
	  if (OP_br(op))
	    new_omega = omega;

	  Set_OP_omega(new_op, opnd, new_omega);
	}
	BB_Append_Op(unrolled_body, new_op);
      }
    }
  }

  unroll_xfer_annotations(unrolled_body, body);

  UINT loop_trip_est = WN_loop_trip_est(wn);
  
  if (!unroll_fully) {
    /*
     * Generate new label for top of loop and fix branch instruction,
     * and place <unrolled_body> on its own succs/preds list.
     */
    op = BB_branch_op(unrolled_body);
    Is_True(op, ("didn't insert loopback branch correctly"));
    Set_OP_opnd(op,
		Branch_Target_Operand(op),
		Gen_Label_TN(Gen_Label_For_BB(unrolled_body), 0));
    Link_Pred_Succ_with_Prob(unrolled_body, unrolled_body,
                             loop_trip_est > 0 ?
			     (loop_trip_est - 1.0) / loop_trip_est : 0.0);
  }

  if (FREQ_Frequencies_Computed() || BB_freq_fb_based(body)) {
    old_freq = BB_freq(body);
    BB_freq(unrolled_body) = old_freq / ntimes;
  }
  
  /* Replace <body> with <unrolled_body> in BB chain & CFG */
  Chain_BBs(BB_prev(body), unrolled_body);
  Chain_BBs(unrolled_body, BB_next(body));
  LOOP_DESCR_Retarget_Loop_Entrances(loop, unrolled_body);
  /* Restore correct frequencies messed up by Change_Succ */
  if (FREQ_Frequencies_Computed() || BB_freq_fb_based(body)) {
    BB_freq(body) = old_freq;
    BB_freq(unrolled_body) = old_freq / ntimes;
  }
  Unlink_Pred_Succ(body, BB_next(body));
  Link_Pred_Succ_with_Prob(unrolled_body, BB_next(body),
			   loop_trip_est > 0 ? 1.0 / loop_trip_est : 1.0);
  BB_next(body) = BB_prev(body) = NULL;

  /* GRA liveness info */
  GRA_LIVE_Compute_Local_Info(unrolled_body);
  
  /* If 'body' has a DFG, move it to 'unrolled_body'. */
  AUTOTIE_Move_BB_DFG(body, unrolled_body);
  
  return unrolled_body;
}



void Unroll_Make_Remainder_Loop(CG_LOOP& cl, INT32 ntimes)
/* -----------------------------------------------------------------------
 * Turn LOOP_DESCR_loophead(loop) into a "remainder" loop in preparation
 * for a version being unrolled <ntimes>.  The remainder loop executes
 * <trip_count> % <ntimes> iterations.  If the remainder loop will execute
 * less than two times, we remove the ending branch, so it's not really a
 * loop.
 *
 * TODO: Add LOOP_DESCR for remainder loop?
 * -----------------------------------------------------------------------
 */
{
  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);
  ANNOTATION *annot = ANNOT_Get(BB_annotations(body), ANNOT_LOOPINFO);
  LOOPINFO *info = ANNOT_loopinfo(annot);
  TN *trip_count = LOOPINFO_trip_count_tn(info);
  TN *new_trip_count, *label_tn;
  OP *br_op = BB_branch_op(body);
  BB *remainder_tail;
  LABEL_IDX continuation_label = (LABEL_IDX)0;
  CG_LOOP_BACKPATCH *bp;
  BOOL freqs = FREQ_Frequencies_Computed();
  float loop_entry_freq;
  if (freqs)
    loop_entry_freq = BB_freq(CG_LOOP_prolog);

  Is_True(info == LOOP_DESCR_loopinfo(loop),
	    ("LOOPINFO for head BB disagrees with that in loop descriptor"));
  Is_True(BB_unrollings(body) < 2,
	    ("unrolled loophead passed to unroll_make_remainder_loop"));

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop(loop, "Before Unroll_Make_Remainder_Loop");
  }

  /* Now we call CG_LOOP_Remove_Notations for the remainder loop (since
   * it won't be SWPd).  We use the original prolog backpatches, but
   * not the original epilog backpatches since those belong on the
   * unrolled loop.  We'll construct new epilog backpatches for the
   * remainder loop and new prolog backpatches for the unrolled loop to
   * shuttle exposed-use values from the remainder loop to the unrolled
   * loop.  For example, if the original prolog backpatches are:
   *   TN200[2] <- ...
   *   TN200[1] <- ...
   *   TN201[1] <- ...
   * we'll add the following epilog backpatches for the remainder loop:
   *   temp1 <- TN200[1]
   *   temp2 <- TN200[0]
   *   temp3 <- TN201[0]
   * (We simply subtract one from each omega, since we're now at the end
   * of an iteration.  Note that prolog backpatches must have positive
   * omegas.)  To complete the transfer, the prolog backpatches for the
   * unrolled loop are now:
   *   TN200[2] <- temp1
   *   TN200[1] <- temp2
   *   TN201[1] <- temp3
   * Note that these new prolog backpatches (as well as the old epilog
   * backpatches) are written in terms of the body TNs before unrolling.
   * The unroller will later rename these to the new body TNs.
   */ 

  // Determine the number of TN copies required for the TNs in this loop;

  // tn_def_map stores the first definition of each TN within the loop body
  // tn_copies_map stores the number of new TNs and TN copies that will be
  //   needed for each non-global TN within the loop body.  In the example
  //   above, tn_copies_map of TN100 is 3
  // need_copies is a list of all TNs with tn_copies_map[] > 0

  CXX_MEM_POOL pool("Unroll_Make_Remainder_Loop", FALSE);
  hTN_MAP   tn_def_map    = hTN_MAP_Create(pool());
  hTN_MAP32 tn_copies_map = hTN_MAP32_Create(pool());
  TN_LIST *need_copies = Count_Copies_Needed(body, tn_def_map,
					     tn_copies_map, pool());
  hTN_MAP new_tn_map = NULL;

  // Modify the prolog and epilog backpatches so that the non_body TNs
  // match the TNs that they will be assigned to.

  if (need_copies == NULL) {

    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL); bp;
	 bp = CG_LOOP_Backpatch_Next(bp)) {

      // Update non_body_tn, inserting copy if necessary
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      if (non_body_tn != body_tn) {
	CGPREP_Copy_TN_Into_BB(body_tn, non_body_tn, CG_LOOP_prolog,
			       BB_last_op(CG_LOOP_prolog), 0, FALSE);
	CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
      }
    }

    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
	 bp = CG_LOOP_Backpatch_Next(bp)) {

      // Update non_body_tn, inserting copy if necessary
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      if (non_body_tn != body_tn) {
	CGPREP_Copy_TN_Into_BB(non_body_tn, body_tn,
			       CG_LOOP_epilog, NULL, 0, TRUE);
	CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
      }
    }

  } else {  // need_copies != NULL

    // Generate a map to keep track of new TNs.  The values of this
    // map are arrays of new TNs.

    new_tn_map = hTN_MAP_Create(pool());
    Generate_Copy_TNs(body, tn_copies_map, new_tn_map, need_copies, pool());

    // Modify the prolog backpatches so that the non_body TNs match the
    // TNs that they will be assigned to.

    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL); bp;
	 bp = CG_LOOP_Backpatch_Next(bp)) {

      // Look up non_body_tn and new body_tn
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      INT omega = CG_LOOP_BACKPATCH_omega(bp) - 1;
      if (omega > 0) {
	TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
	body_tn = new_tn_array[omega - 1];
      }

      // Update non_body_tn, inserting copy if necessary
      if (non_body_tn != body_tn) {
	CGPREP_Copy_TN_Into_BB(body_tn, non_body_tn, CG_LOOP_prolog,
			       BB_last_op(CG_LOOP_prolog), 0, FALSE);
	CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
      }
    }

    // Modify the epilog backpatches so that the non_body TNs match the
    // TNs that they will be assigned to.

    for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
	 bp = CG_LOOP_Backpatch_Next(bp)) {

      // Look up non_body_tn and new body_tn
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      TN *non_body_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
      INT omega = CG_LOOP_BACKPATCH_omega(bp);
      if (omega > 0) {
	TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);
	body_tn = new_tn_array[omega - 1];
      }

      // Update non_body_tn, inserting copy if necessary
      if (non_body_tn != body_tn) {
	CGPREP_Copy_TN_Into_BB(non_body_tn, body_tn,
			       CG_LOOP_epilog, NULL, 0, TRUE);
	CG_LOOP_BACKPATCH_Set_non_body_tn(bp, body_tn);
      }
    }
  }

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop(loop, "1st Check Unroll_Make_Remainder_Loop");
  }

  // Generate the backpatches for the remainer loop and the main loop;
  // This requires a hack: Since the CG_LOOP_BACKPATCH_* procedures only
  // act on CG_LOOP_prolog and CG_LOOP_epilog, the values of
  // prolog_backpatches and epilog_backpatches are redefined to the
  // backpatch lists we want to work on at any given time.

  CG_LOOP_BACKPATCH *remainder_prolog_backpatches = prolog_backpatches;
  CG_LOOP_BACKPATCH *remainder_epilog_backpatches = NULL;
  CG_LOOP_BACKPATCH *main_loop_prolog_backpatches = NULL;
  CG_LOOP_BACKPATCH *main_loop_epilog_backpatches = epilog_backpatches;

  // Generate remainder_epilog_backpatches and main_loop_prolog_backpatches

  prolog_backpatches = NULL;
  epilog_backpatches = NULL;

  // Insert prolog GTN400[1] <-- GTN400 and epilog GTN400 <-- GTN400[0]
  // backpatches for all GTNs appearing in loop

  CG_LOOP_DEF tn_def(body);

  OP *op;
  FOR_ALL_BB_OPs(body, op) {

    // For live-out defs, create an epilog backpatch.
    for (INT res = 0; res < OP_results(op); res++) {
      TN *tn = OP_result(op, res);
      if (TN_is_register(tn) && ! TN_is_dedicated(tn)
	  && ! TN_is_const_reg(tn) && TN_is_global_reg(tn)
	  && GTN_SET_MemberP(BB_live_in(CG_LOOP_epilog), tn)) {
	CG_LOOP_Backpatch_Add(CG_LOOP_epilog, tn, tn, 0);
	CG_LOOP_Backpatch_Add(CG_LOOP_prolog, tn, tn, 1);
      }
    }

    // For exposed uses, create a prolog backpatch.
    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      TN *tn = OP_opnd(op, opnd);
      if (TN_is_register(tn) && ! TN_is_dedicated(tn) &&
	  ! TN_is_const_reg(tn)) {
	OP *def_op = tn_def.Get(tn);
	// TN is not an invariant and TN is not defined before this OP
	if (def_op && ! OP_Precedes(def_op, op)
	    && ! CG_LOOP_Backpatch_Find_Non_Body_TN(CG_LOOP_prolog, tn, 1) ) {
	  CG_LOOP_Backpatch_Add(CG_LOOP_epilog, tn, tn, 0);
	  CG_LOOP_Backpatch_Add(CG_LOOP_prolog, tn, tn, 1);
	}
      }
    }
  }

  // Add backpatches for higher omega TN copies

  if (need_copies) {

    for (TN_LIST *p = need_copies; p != NULL; p = TN_LIST_rest(p)) {
      TN *body_tn = TN_LIST_first(p);
      INT copies = hTN_MAP32_Get(tn_copies_map, body_tn);
      TN **new_tn_array = (TN **) hTN_MAP_Get(new_tn_map, body_tn);

      for (INT cp = copies; cp > 0; --cp) {
	TN *tn = new_tn_array[cp - 1];
	CG_LOOP_Backpatch_Add(CG_LOOP_epilog, tn, body_tn, cp);
	CG_LOOP_Backpatch_Add(CG_LOOP_prolog, tn, body_tn, cp + 1);
      }
      if (! CG_LOOP_Backpatch_Find_Non_Body_TN(CG_LOOP_prolog, body_tn, 1)) {
	CG_LOOP_Backpatch_Add(CG_LOOP_epilog, body_tn, body_tn, 0);
	CG_LOOP_Backpatch_Add(CG_LOOP_prolog, body_tn, body_tn, 1);
      }
    }
  }

  remainder_epilog_backpatches = epilog_backpatches;
  main_loop_prolog_backpatches = prolog_backpatches;

  // Remove notations from the remainder loop
  remainder_tail = Gen_BB_Like(body);
  if (BB_freq_fb_based(CG_LOOP_prolog))
    Set_BB_freq_fb_based(remainder_tail);

  prolog_backpatches = remainder_prolog_backpatches;
  epilog_backpatches = remainder_epilog_backpatches;

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop(loop, "2nd Check Unroll_Make_Remainder_Loop");
  }

  // WAS:  CG_LOOP_Remove_Notations(cl, CG_LOOP_prolog, remainder_tail);
  if (need_copies == NULL) {

    // If no new TNs are required, then only the prolog and epilog
    // backpatches may require updating
    Remove_Notations_Without_Copies(CG_LOOP_prolog, remainder_tail);

  } else {  // need_copies != NULL

    Remove_Notations_With_Copies(body, CG_LOOP_prolog, remainder_tail,
				 tn_def_map,
				 tn_copies_map, need_copies, new_tn_map);
  }

  // Recompute liveness
  cl.Recompute_Liveness();

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop(loop, "3rd Check Unroll_Make_Remainder_Loop");
  }

  OPS zero_trip_guard_ops = OPS_EMPTY;
  OPS prolog_ops = OPS_EMPTY;
  BOOL const_trip = TN_is_constant(trip_count);
  float ztrip_prob;

  if (const_trip) {
    /* If trip count is a constant, see how many times the remainder
     * loop will execute (if at all) before proceeding.  If it won't
     * execute at all, don't make a remainder loop.  Otherwise, create
     * a constant TN for the new trip count.
     */
    INT16 new_trip_count_val = TN_value(trip_count) % ntimes;
    Is_True(new_trip_count_val > 0,
	    ("unroll_make_remainder_loop: trip count is negative or zero"));
    new_trip_count = Gen_Literal_TN(new_trip_count_val, 4);
    if (new_trip_count_val==0)
      ztrip_prob = 1.0;
    else
      ztrip_prob = 0.0;

  } else {

    /* Add zero-trip guard for remainder loop if necessary:
     *   if (trip_count % ntimes <= 0) skip remainder
     * We know <trip_count>'s value is positive, and <ntimes> is
     * a power of two, so we can perform this with:
     *   <new_trip_count> <- andi <trip_count> <ntimes>-1
     *   beq continuation_label <new_trip_count> 0
     */
    ztrip_prob = 1.0 / ntimes;
    INT32 trip_size = TN_size(trip_count);
    TYPE_ID ttype = TN_mtype(trip_count);
    new_trip_count = Build_TN_Like(trip_count);
    if (is_power_of_two(ntimes)) 
      Exp_OP2(OPCODE_make_op(OPR_BAND, ttype, MTYPE_V),
	      new_trip_count,
	      trip_count,
	      Gen_Literal_TN(ntimes-1, trip_size),
	      &prolog_ops);
    else
      Exp_OP2(OPCODE_make_op(OPR_REM, ttype, MTYPE_V),
	      new_trip_count,
	      trip_count,
	      Gen_Literal_TN(ntimes, trip_size),
	      &prolog_ops);
    
    continuation_label = Gen_Label_For_BB(remainder_tail);
    Exp_OP3v(OPC_FALSEBR,
	     NULL,
	     Gen_Label_TN(continuation_label,0),
	     new_trip_count,
	     Gen_Literal_TN(0, trip_size),
	     V_BR_I8EQ,
	     &zero_trip_guard_ops);

    if (BB_freq_fb_based(CG_LOOP_prolog)) {
      ztrip_prob = (WN_loop_trip_est(LOOPINFO_wn(info)) % ntimes ? 0.0 : 1.0);
    }

  }

  /* Remove the branch at the end of <body>, remembering the label TN
   * and dbnum (if any) for later use if we're going to replace it with
   * another branch.
   */
  Is_True(br_op && OP_br(br_op), ("loop body doesn't end in branch"));

  label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));
  Is_True(TN_is_label(label_tn), ("branch operand not a label"));
  BB_Remove_Op(body, br_op);

  BB *first_remainder_body = body;
  float body_freq = 0.0;
  
  if (const_trip && TN_value(new_trip_count) < 2) {
    /*
     * Remainder isn't really a loop, so remove it from its own BB_succs/
     * BB_preds and remove the LOOPINFO annotation.
     */
    BB_annotations(body) = ANNOT_Unlink(BB_annotations(body), annot);
    Unlink_Pred_Succ(body, body);
    Set_BB_loop_head_bb(body, NULL);
    body_freq = loop_entry_freq;
    append_to_prolog(body);
    append_to_prolog(remainder_tail);

  } else {

    if (ntimes == 2 || (const_trip && CG_LOOP_unroll_remainder_fully)) {
      // fully unroll the remainder loop

      INT32 unroll_times = (const_trip ? (TN_value(new_trip_count) % ntimes)
			    : (ntimes - 1));
      BB *unrolled_body = Gen_BB_Like(body);
      first_remainder_body = unrolled_body;

      OPS body_ops = OPS_EMPTY;
      if (!const_trip) {
	Link_Pred_Succ_with_Prob(CG_LOOP_prolog, remainder_tail, ztrip_prob);
	if (freqs || BB_freq_fb_based(CG_LOOP_prolog)) 
	  BB_freq(remainder_tail) += loop_entry_freq * ztrip_prob;
	if (freqs) {
	  // Save the original loop-body frequency 
	  float orig_body_freq = BB_freq(BB_next(CG_LOOP_prolog));
	  Change_Succ_Prob(CG_LOOP_prolog, BB_next(CG_LOOP_prolog),
					1.0 - ztrip_prob);
	  // Restore the original loop-body frequency
	  BB_freq(BB_next(CG_LOOP_prolog)) = orig_body_freq;
	}

	BB_Append_Ops(CG_LOOP_prolog, &prolog_ops);
	BB_Append_Ops(CG_LOOP_prolog, &zero_trip_guard_ops);

	INT32 trip_size = TN_size(new_trip_count);
	TYPE_ID ttype = TN_mtype(new_trip_count);
	Exp_OP2(OPCODE_make_op(OPR_ADD, ttype, MTYPE_V),
		new_trip_count,
		new_trip_count,
		Gen_Literal_TN(-1, trip_size),
		&body_ops);
	Exp_OP3v(OPC_TRUEBR,
		 NULL,
		 Gen_Label_TN(continuation_label,0),
		 new_trip_count,
		 Gen_Literal_TN(0, trip_size),
		 V_BR_I8EQ,
		 &body_ops);
      }
      else {
        Set_BB_unrollings(unrolled_body, unroll_times);
      }

      for (INT32 unrolling = 1; unrolling <= unroll_times; unrolling++) {
	OP *op;
	FOR_ALL_BB_OPs(body, op) {
	  // keep the prefetches ops of the first unrolling
	  if (OP_prefetch(op) && unrolling != unroll_times)
	    continue;
	  OP *new_op = Dup_OP(op);
          if (const_trip) {
            Set_OP_unrolling(new_op, unrolling-1);
            Set_OP_orig_idx(new_op, OP_map_idx(op));
            Set_OP_unroll_bb(new_op, unrolled_body);
          }
	  Copy_WN_For_Memory_OP(new_op, op);
	  BB_Append_Op(unrolled_body, new_op);
	}
	// do not generate branch for the last unrolling
	if (unrolling < unroll_times && !const_trip) {
	  FOR_ALL_OPS_OPs(&body_ops, op) {
	    OP *new_op = Dup_OP(op);
	    Copy_WN_For_Memory_OP(new_op, op);
	    BB_Append_Op(unrolled_body, new_op);
	  }
	  Link_Pred_Succ_with_Prob(unrolled_body,
				   remainder_tail, 
				   1.0 / (unroll_times - unrolling + 1.0));
	  append_to_prolog(unrolled_body);
	  if (freqs || BB_freq_fb_based(body))
	    BB_freq(unrolled_body)
	      = loop_entry_freq * (unroll_times + 1.0 - unrolling)
	      / (unroll_times + 1.0);
	  unrolled_body = Gen_BB_Like(body);
	}
      }
      if (BB_freq_fb_based(body)) {
        Set_BB_freq_fb_based(unrolled_body);
        body_freq = (WN_loop_trip_est(LOOPINFO_wn(info)) % ntimes ? 
                     BB_freq(CG_LOOP_prolog) : 0.0);
      }
      else if (freqs)
	body_freq = const_trip ? 
	  BB_freq(CG_LOOP_prolog) : loop_entry_freq / ((float)unroll_times + 1.0f);
      body = unrolled_body;
      append_to_prolog(body);
      append_to_prolog(remainder_tail);

    } else {

      // generate the remainder loop

      /* Correct loop info for remainder loop.
       */
      INT64 trip_est = (const_trip ? TN_value(new_trip_count)
			: (1 + ntimes) / 2);
      if (info) {
	WN *wn = LOOPINFO_wn(info);
	TYPE_ID ttype = WN_rtype(WN_loop_trip(wn));
	OPCODE opc_intconst = OPCODE_make_op(OPR_INTCONST, ttype, MTYPE_V);
	WN *ntimes_wn = WN_CreateIntconst(opc_intconst, ntimes);
	OPCODE opc_rem = OPCODE_make_op(OPR_REM, ttype, MTYPE_V);
	WN_set_loop_trip(wn,
			 WN_CreateExp2(opc_rem, WN_loop_trip(wn), ntimes_wn));
	WN_loop_trip_est(wn) = trip_est;
	WN_Set_Loop_Unimportant_Misc(wn);
	LOOPINFO_trip_count_tn(info) = new_trip_count;
	LOOPINFO_max_trip_count_tn(info) = new_trip_count;
      }

      extend_prolog();
      BB *remainder_prolog=CG_LOOP_prolog;
      append_to_prolog(body);
      append_to_prolog(remainder_tail);
      Change_Succ_Prob(body, body, (trip_est - 1.0) / trip_est);
      Change_Succ_Prob(body, remainder_tail, (1.0) / trip_est);
      LABEL_IDX body_label = Gen_Label_For_BB(body);
      TN *body_target_label = Gen_Label_TN(body_label,0);
      if (xt_zero_cost_loop && Enable_ZCL) {
        OPS body_ops = OPS_EMPTY;
        CGTARG_Generate_Branch_Cloop(remainder_prolog,
				body,
				/* unrolled_trip_count not used */NULL,
				new_trip_count,
				/* ntimes not used */0,
				body_target_label,
				&prolog_ops,
				&body_ops);

        CG_LOOP_Init_OPS(&prolog_ops);
        CG_LOOP_Init_OPS(&body_ops);

        BB_Append_Ops(remainder_prolog, &prolog_ops);
        BB_Append_Ops(body, &body_ops);

        /* Mark enclosing loops so they won't use a zcl. We don't use
           LOOP_DESCR_Next_Enclosing_Loop since it checks to make sure the
           potentially enclosing loop contains all the blocks of this
           loop. Since we don't correctly maintain the blocks in a loop
           during the loop optimizations, we do a more conservative walk up
           the containing loops. */

        if (LOOP_DESCR_nestlevel(loop) > 1)
        {
          INT nestlevel = LOOP_DESCR_nestlevel(loop)-1;
          LOOP_DESCR *enclosing = LOOP_DESCR_next(loop);
          while (enclosing && nestlevel>0)
          {
            if (LOOP_DESCR_nestlevel(enclosing)==nestlevel) {
              Set_Zcl_Enclosing_Loop(enclosing);
	      nestlevel--;
            }
            enclosing = LOOP_DESCR_next(enclosing);
          }
        }

      } else {

        OPS body_ops = OPS_EMPTY;
	INT32 trip_size = TN_size(new_trip_count);
	TYPE_ID ttype = TN_mtype(new_trip_count);
	TN* trip_counter = NULL;
	if (const_trip) {
	  trip_counter = Gen_Typed_Register_TN (ttype, trip_size);
	  Exp_COPY(trip_counter, new_trip_count, &prolog_ops);
	} else {
	  trip_counter = new_trip_count;
	}
	Exp_OP2(OPCODE_make_op(OPR_ADD, ttype, MTYPE_V),
		trip_counter,
		trip_counter,
		Gen_Literal_TN(-1, trip_size),
		&body_ops);
	Exp_OP3v(OPC_TRUEBR,
	     NULL,
	     body_target_label,
	     trip_counter,
	     Gen_Literal_TN(0, trip_size),
	     V_BR_I8NE,
	     &body_ops);

	Link_Pred_Succ_with_Prob(remainder_prolog, remainder_tail, ztrip_prob);
	if (freqs) {
	  Change_Succ_Prob(remainder_prolog, body, 1.0 - ztrip_prob);
	}

        CG_LOOP_Init_OPS(&prolog_ops);
        CG_LOOP_Init_OPS(&body_ops);

        BB_Append_Ops(remainder_prolog, &prolog_ops);
	BB_Append_Ops(remainder_prolog, &zero_trip_guard_ops);
        BB_Append_Ops(body, &body_ops);
      }

      if (freqs || BB_freq_fb_based(body)) {
	/* BB_freq(body) doesn't yet include edge from prolog. */
	body_freq = BB_freq(CG_LOOP_prolog) * trip_est;
      }

      if (CGPREP_remove_guard_branch) {
	// Try to eliminate a guard branch, it there is one.
	Remove_Guard_Branch(remainder_prolog);
      }

      if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
	#pragma mips_frequency_hint NEVER
	CG_LOOP_Trace_Loop( loop, "Remainder Loop Structure" );
      }

      cl.Set_remainder_loop(body);
    }
  }

  if (freqs || BB_freq_fb_based(body))
    BB_freq(body) = body_freq;

  // Add remainder_tail, even if the block is empty and has no label,
  // because previous block may terminate in branch if remainder loop
  // was not completely unrolled.
  // INADEQUATE OLD CODE :
  if (freqs || BB_freq_fb_based(body))
    BB_freq(remainder_tail) = loop_entry_freq;

  /* Add an unrolling note for the assembly listing.
   */
  note_remainder_head(first_remainder_body, ntimes,
		      TN_is_constant(new_trip_count)
		      ? TN_value(new_trip_count) : 0);

  // Delete all remainder loop backpatchs
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp))
    CG_LOOP_Backpatch_Delete(CG_LOOP_prolog, bp);
  for (bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL); bp;
       bp = CG_LOOP_Backpatch_Next(bp))
    CG_LOOP_Backpatch_Delete(CG_LOOP_epilog, bp);

  // Restore main loop backpatches
  prolog_backpatches = main_loop_prolog_backpatches;
  epilog_backpatches = main_loop_epilog_backpatches;

  if (Get_Trace(TP_CGPREP, 0x400000)) { // Change this flag?
    #pragma mips_frequency_hint NEVER
    CG_LOOP_Trace_Loop( loop, "After Unroll_Make_Remainder_Loop" );
  }

}



void unroll_rename_backpatches(CG_LOOP_BACKPATCH *bpatches, UINT16 n,
			       UINT16 ntimes)
/* -----------------------------------------------------------------------
 * Rename the body TNs and omegas in <bpatches> as if they are uses in
 * the <n>th unrolling out of <ntimes>.
 * -----------------------------------------------------------------------
 */
{
  while (bpatches) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bpatches);
    UINT16 adjust = CG_LOOP_BACKPATCH_omega(bpatches) + (ntimes - n - 1);
    UINT8 new_omega = adjust / ntimes;
    UINT8 which = ntimes - 1 - (adjust - (new_omega * ntimes));
    TN *new_body_tn = unroll_names_get(body_tn, which);
    CG_LOOP_BACKPATCH_Set_body_tn(bpatches, new_body_tn);
    CG_LOOP_BACKPATCH_Set_omega(bpatches, new_omega);
    bpatches = CG_LOOP_Backpatch_Next(bpatches);
  }
}


void unroll_remove_notations(BB *fully_unrolled_body)
/* -----------------------------------------------------------------------
 * Requires: <fully_unrolled_body> is not a loop since it's been fully
 *	     unrolled (i.e., BB_branch_op(fully_unrolled_body) == NULL)
 *
 * Gets rid of backpatches and nonzero omegas in <fully_unrolled_body>.
 * -----------------------------------------------------------------------
 */
{
  CG_LOOP_BACKPATCH *bp;
  OP *op;

  /* We'll execute at most a single iteration, so each use of a TN with
   * nonzero omega is replaced with the source TN from the appropriate
   * prolog backpatch.
   */
  FOR_ALL_BB_OPs(fully_unrolled_body, op) {
    UINT8 i;
    for (i = 0; i < OP_opnds(op); i++) {
      UINT8 omega = OP_omega(op,i);
      if (omega) {
	TN *old_tn = OP_opnd(op,i);
	TN *new_tn = CG_LOOP_Backpatch_Find_Non_Body_TN(CG_LOOP_prolog,
							old_tn, omega);
	Is_True((new_tn == NULL) == (TN_is_dedicated(old_tn) != 0),
		("missing prolog backpatch for TN%d[%d]",
		 TN_number(old_tn), omega));
	if (new_tn)
	  Set_OP_opnd(op, i, new_tn);
	Set_OP_omega(op, i, 0);
      }
    }
  }

  /* Add copies at the end of <fully_unrolled_body> for the epilog
   * backpatches (but skip self-copies).  Many of these will turn
   * out to be unnecessary, but we'll let copy removal get rid of
   * them.  Remove each epilog backpatch after adding the appropriate
   * copy (if any).
   */

  bp = CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL);
  while (bp) {
    CG_LOOP_BACKPATCH *next = CG_LOOP_Backpatch_Next(bp);
    TN *src_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    UINT8 omega = CG_LOOP_BACKPATCH_omega(bp);
    TN *dest_tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    if (omega) {
      TN *new_src_tn = CG_LOOP_Backpatch_Find_Non_Body_TN(CG_LOOP_prolog,
							  src_tn, omega);
      Is_True(new_src_tn, ("missing prolog backpatch for TN%d[%d]",
			   TN_number(src_tn), omega));
      src_tn = new_src_tn;
    }
    if (src_tn != dest_tn) {
      CGPREP_Copy_TN(dest_tn, src_tn, BB_last_op(fully_unrolled_body), 0,
		     FALSE);
    }
    CG_LOOP_Backpatch_Delete(CG_LOOP_epilog, bp);
    bp = next;
  }
  
  /* Get rid of the prolog backpatches.  We used them in the last two
   * steps, so we weren't able to get rid of them until now.
   */
  bp = CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL);
  while (bp) {
    CG_LOOP_BACKPATCH *next = CG_LOOP_Backpatch_Next(bp);
    CG_LOOP_Backpatch_Delete(CG_LOOP_prolog, bp);
    bp = next;
  }
}


static void trace_loop_cflow(LOOP_DESCR *loop, const char *prefix)
{
  BB *bb;

  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb) {
    BBLIST *bbl;
    fprintf(TFile, "%sBB:%d  preds = { ", prefix, BB_id(bb));
    FOR_ALL_BB_PREDS(bb, bbl)
      fprintf(TFile, "BB:%d ", BB_id(BBLIST_item(bbl)));
    fprintf(TFile, "}  succs = { ");
    FOR_ALL_BB_SUCCS(bb, bbl)
      fprintf(TFile, "BB:%d ", BB_id(BBLIST_item(bbl)));
    fprintf(TFile, "}\n");
  }
}


static BOOL sort_topologically(LOOP_DESCR *loop, BB **result)
/* -----------------------------------------------------------------------
 * Fill in <result> so that it contains a topological ordering of the BBs
 * in <loop>.  Return TRUE if done, or FALSE if not possible (i.e., loop
 * has irreducible flowgraph).
 * -----------------------------------------------------------------------
 */
{
  BB_SET *bbs = LOOP_DESCR_bbset(loop);
  UINT32 num_bbs = BB_SET_Size(bbs);
  BB_MAP topo_map = BB_Topological_Map(bbs, LOOP_DESCR_loophead(loop));
  BB *bb;

  FOR_ALL_BB_SET_members(bbs, bb) {
    INT32 i = BB_MAP32_Get(topo_map, bb);
    Is_True(i >= 0 && i <= num_bbs, ("bad <topo_map> value"));
    if (i == 0) {
      BB_MAP_Delete(topo_map);
      return FALSE;
    }
    result[i-1] = bb;
  }

  if (Get_Trace(TP_CGPREP, 2)) {
    UINT32 bbi;
    fprintf(TFile, "<unroll> topological sort of loop BBs:");
    for (bbi = 0; bbi < num_bbs; bbi++)
      fprintf(TFile, " %d", BB_id(result[bbi]));
    fprintf(TFile, "\n");
  }

  BB_MAP_Delete(topo_map);

  return TRUE;
}

static BOOL unroll_multi_make_remainder_loop(LOOP_DESCR *loop, UINT8 ntimes,
					     BB **topo_vec, UINT32 num_bbs)
/* -----------------------------------------------------------------------
 * Requires: CG_LOOP_Trip_Count(loop) != NULL &&
 *
 *           Since <loop> is trip-countable, it has a unique loop tail,
 *	     the last entry in <topo_vec> that is the only loop exit.
 *	     Otherwise it's not really trip-countable since it's possible
 *	     to execute a partial iteration (or else the impossible exits
 *	     should be removed). &&
 *
 *	     BB_freq(bb) for all <bb> in <loop> has the same value it did
 *	     before unrolling started.
 *
 * TURN LOOP_DESCR_bbset(loop) into a "remainder" loop in preparation
 * for a version being unrolled <ntimes>.  The remainder loop executes
 * <trip_count> % <ntimes> iterations.  If the remainder loop will execute
 * less than two times, we remove the ending branch, so it's not really a
 * loop.  Returns FALSE if a remainder loop isn't necessary, TRUE otherwise.
 *
 * TODO: Add LOOP_DESCR for remainder loop?
 * -----------------------------------------------------------------------
 */
{
  BB *head = LOOP_DESCR_loophead(loop), *tail = topo_vec[num_bbs-1];
  ANNOTATION *annot = ANNOT_Get(BB_annotations(head), ANNOT_LOOPINFO);
  LOOPINFO *info = ANNOT_loopinfo(annot);
  TN *trip_count = LOOPINFO_trip_count_tn(info), *trip_counter;
  TN *new_trip_count = TN_is_constant(trip_count) ?
    Gen_Literal_TN(TN_value(trip_count) % ntimes, 4) : Build_TN_Like(trip_count);
  OP *op, *backedge_br_op = BB_branch_op(tail);
  BOOL freqs = FREQ_Frequencies_Computed();
  float ztrip_prob = !freqs || TN_is_constant(trip_count) ? 0.0 : 1.0 / ntimes;
  INT64 orig_trip_est = WN_loop_trip_est(LOOPINFO_wn(info));
  INT64 trip_est = TN_is_constant(new_trip_count) ?
    TN_value(new_trip_count) : MIN((1 + ntimes) / 2, 2);
  float orig_prolog_freq = BB_freq(CG_LOOP_prolog);
  float orig_post_prolog_freq = BB_freq(BB_next(CG_LOOP_prolog));
  float freq_factor = (1.0 - ztrip_prob) * trip_est / MAX(orig_trip_est, 1);
  float fts_freq, head_freq;
  BB *remainder_epilog = NULL, *fts, *orig_prolog = CG_LOOP_prolog;
  INT16 dbnum = 0;
  UINT32 bbi;

  if (TN_is_constant(new_trip_count) && TN_value(new_trip_count) % ntimes == 0)
    return FALSE;

  Is_True(CG_LOOP_epilog != NULL,
	    ("remainder loop generation requires non-NULL CG_LOOP_epilog"));
  Is_True(BB_unrollings(head) < 2,
	    ("unrolled loophead passed to unroll_multi_make_remainder_loop"));
  Is_True(topo_vec[0] == head, ("<head> not first in <topo_vec>"));
  Is_True(LOOP_DESCR_Find_Unique_Tail(loop), 
	    ("<loop> has no unique tail"));
  Is_True(tail == LOOP_DESCR_Find_Unique_Tail(loop),
	    ("<loop> tail not last in <topo_vec>"));
  
  /* If trip count is a constant, see how many times the remainder
   * loop will execute (if at all) before proceeding.  If it won't
   * execute at all, don't make a remainder loop.  Otherwise, create
   * a constant TN for the new trip count.
   */
  if (TN_is_constant(trip_count)) {
    INT16 new_trip_count_val = TN_value(trip_count) % ntimes;
    if (new_trip_count_val < 0)
      DevWarn("unroll_multi_make_remainder_loop: trip count is negative");
    new_trip_count = Gen_Literal_TN(new_trip_count_val, 4);
  } else {
    /* Add zero-trip guard for remainder loop:
     *   if (trip_count % ntimes <= 0) skip remainder
     * We know <trip_count>'s value is positive, and <ntimes> is
     * a power of two, so we can perform this with:
     *   <new_trip_count> <- andi <trip_count> <ntimes>-1
     *   beq continuation_label <new_trip_count> 0
     */
    LABEL_IDX continuation_label;
    INT32 trip_size = TN_size(trip_count);
    TYPE_ID ttype = TN_mtype(trip_count);
    OPS ops = OPS_EMPTY;
    remainder_epilog = Gen_BB_Like(CG_LOOP_prolog);
    if (BB_freq_fb_based(CG_LOOP_prolog))
      Set_BB_freq_fb_based(remainder_epilog);
    continuation_label = Gen_Label_For_BB(remainder_epilog);
    Is_True(is_power_of_two(ntimes), ("unroll amount not power of two"));
    new_trip_count = Build_TN_Like(trip_count);
    Exp_OP2(OPCODE_make_op(OPR_BAND, ttype, MTYPE_V),
	    new_trip_count,
	    trip_count,
	    Gen_Literal_TN(ntimes-1, trip_size),
	    &ops);
    Exp_OP3v(OPC_FALSEBR,
	     NULL,
	     Gen_Label_TN(continuation_label,0),
	     new_trip_count,
	     Gen_Literal_TN(0, trip_size),
	     V_BR_I8EQ,
	     &ops);
    BB_Append_Ops(CG_LOOP_prolog, &ops);
    Link_Pred_Succ_with_Prob(CG_LOOP_prolog, remainder_epilog, ztrip_prob);
    if (freqs)
      Change_Succ_Prob(CG_LOOP_prolog, BB_next(CG_LOOP_prolog),
		       1.0 - ztrip_prob);
  }

  /* Append loop BBs in topological order.  It shouldn't be necessary to
   * insert branches since there's a single loop tail/exit block.  Set
   * BB_freqs by multiplying their pre-unrolling values by <freq_factor>,
   * which compensates for the zero-trip guard and new trip count.
   */
  fts = BB_Fall_Thru_Successor(CG_LOOP_prolog);
  Is_True(fts, ("CG_LOOP_prolog has no fall-thru successor"));
  fts_freq = BB_freq(fts);
  head_freq = BB_freq(head);
  Change_Succ(CG_LOOP_prolog, fts, head);
  BB_freq(fts) = fts_freq;
  BB_freq(head) = head_freq;
  fts = BB_Fall_Thru_Successor(tail);
  FmtAssert(fts,
	    /* This indicates that the loop isn't really trip-countable. */
	    ("trip-countable loop (line %d) tail has no fall-thru successor",
	     BB_Loop_Lineno(head)));
  Unlink_Pred_Succ(tail, fts);
  for (bbi = 0; bbi < num_bbs; bbi++) {
    BB *loop_bb = topo_vec[bbi];
    BB *post_prolog = BB_next(CG_LOOP_prolog);
    BB *prev = BB_prev(loop_bb);
    BB *next = BB_next(loop_bb);
    RID *rid = BB_rid(loop_bb);
    BB *fall_thru = BB_Fall_Thru_Successor(loop_bb);
    Is_True(BB_SET_MemberP(LOOP_DESCR_bbset(loop), loop_bb),
	      ("topo_vec[%d] = BB:%d not in LOOP_DESCR_bbset",
	       bbi, BB_id(loop_bb)));
    if (prev && BB_next(prev) == loop_bb) BB_next(prev) = next;
    if (next && BB_prev(next) == loop_bb) BB_prev(next) = prev;
    BB_prev(loop_bb) = BB_next(loop_bb) = NULL;
    if (REGION_First_BB == loop_bb) REGION_First_BB = next;
    if (rid && RID_cginfo(rid)) {
      /* update cgrin pointers */
      CGRIN *cgrin = RID_cginfo(rid);
	if (CGRIN_first_bb(cgrin) == loop_bb) CGRIN_first_bb(cgrin) = next;
      if (CGRIN_last_bb(cgrin) == loop_bb) CGRIN_last_bb(cgrin) = prev;
    }
    Chain_BBs(CG_LOOP_prolog, loop_bb);
    Chain_BBs(loop_bb, post_prolog);
    BB_freq(loop_bb) *= freq_factor;
    CG_LOOP_prolog = loop_bb;
    if (bbi != num_bbs -1 && fall_thru && fall_thru != topo_vec[bbi+1]) {
      /* Insert fall-thru BB that branches to <fall_thru> */
      BB *new_bb = Gen_And_Insert_BB_After(loop_bb);
      BB_rid(new_bb) = BB_rid(loop_bb);
      if (BB_freq_fb_based(loop_bb)) Set_BB_freq_fb_based(new_bb);
      Add_Goto(new_bb, fall_thru);
      Change_Succ(loop_bb, fall_thru, new_bb);
      if (freqs || BB_freq_fb_based(fall_thru))
	BB_freq(fall_thru) += BB_freq(loop_bb);
      if (BB_SET_MemberP(LOOP_DESCR_bbset(loop), fall_thru))
	Set_BB_loop_head_bb(new_bb, head);
      CG_LOOP_prolog = new_bb;
    }
  }

  /* Remove the branch at the end of <tail>, remembering the dbnum
   * (if any) for later use if we're going to replace it with
   * another branch.
   */
  if (backedge_br_op) {
    if (Is_DB_OP_Init(backedge_br_op))
      dbnum = OP_dbnum(backedge_br_op);
    BB_Remove_Op(tail, backedge_br_op);
    Unlink_Pred_Succ(tail, head);
  }
  
  if (ntimes == 2 ||
      TN_is_constant(new_trip_count) && TN_value(new_trip_count) < 2) {
    /*
     * Remainder isn't really a loop, so remove the LOOPINFO annotation.
     */
    BB_annotations(head) = ANNOT_Unlink(BB_annotations(head), annot);
    for (bbi = 0; bbi < num_bbs; bbi++)
      Set_BB_loop_head_bb(topo_vec[bbi], NULL);
    Link_Pred_Succ_with_Prob(tail, BB_next(tail), 1.0);

  } else {

    /* Correct loop info for remainder loop.
     */
    OPS ops = OPS_EMPTY;
    INT32 trip_size = TN_size(new_trip_count);
    WN *wn = LOOPINFO_wn(info);
    TYPE_ID ttype = WN_rtype(WN_loop_trip(wn));
    OPCODE opc_intconst = OPCODE_make_op(OPR_INTCONST, ttype, MTYPE_V);
    WN *ntimes_wn = WN_CreateIntconst(opc_intconst, ntimes);
    OPCODE opc_rem = OPCODE_make_op(OPR_REM, ttype, MTYPE_V);
    float backedge_prob = (trip_est - 1.0) / trip_est;
    WN_set_loop_trip(wn,
		     WN_CreateExp2(opc_rem, WN_loop_trip(wn), ntimes_wn));
    WN_loop_trip_est(wn) = trip_est;
    WN_Set_Loop_Unimportant_Misc(wn);
    LOOPINFO_trip_count_tn(info) = new_trip_count;
    LOOPINFO_max_trip_count_tn(info) = new_trip_count;

    /*
     * Modify actual trip count of remainder loop:
     *   Append to prolog (if <new_trip_count> is constant, otherwise
     *     <trip_counter> is <new_trip_count>):
     *	   <trip_counter> <- <new_trip_count>
     *   Replace tail branch OP with:
     *	   <trip_counter> <- [d]addi <trip_counter> -1
     *	   bne <head> <trip_counter> $0
     */
    if (TN_is_constant(new_trip_count)) {
      OPS copy_ops = OPS_EMPTY;
      trip_counter = Gen_Typed_Register_TN (ttype, trip_size);
      Exp_COPY(trip_counter, new_trip_count, &copy_ops);
      BB_Append_Ops(orig_prolog, &copy_ops);
    } else {
      trip_counter = new_trip_count;
    }
    Exp_OP2(OPCODE_make_op(OPR_ADD, ttype, MTYPE_V),
	    trip_counter,
	    trip_counter,
	    Gen_Literal_TN(-1, trip_size),
	    &ops);
    Exp_OP3v(OPC_TRUEBR,
	     NULL,
	     Gen_Label_TN(Gen_Label_For_BB(head), 0),
	     trip_counter,
	     Gen_Literal_TN(0, trip_size),
	     V_BR_I8NE,
	     &ops);
    BB_Append_Ops(tail, &ops);
    if (dbnum > 0) {
      FOR_ALL_OPS_OPs(&ops, op) DB_Initialize_OP(op, dbnum);
    }
    Link_Pred_Succ_with_Prob(tail, head, backedge_prob);
    Link_Pred_Succ_with_Prob(tail, BB_next(tail), 1.0 - backedge_prob);
  }
  
  /* Attach <remainder_epilog> to prolog if a zero-trip guard was
   * generated.  We'll branch here when not executing the remainder
   * loop.
   */
  if (remainder_epilog) {
    append_to_prolog(remainder_epilog);
    BB_freq(remainder_epilog) = orig_prolog_freq;
  }
  BB_freq(BB_next(CG_LOOP_prolog)) = orig_post_prolog_freq;

  /* Add an unrolling note for the assembly listing.
   */
  note_remainder_head(head, ntimes, TN_is_constant(new_trip_count) ?
		      TN_value(new_trip_count) : 0);

  return TRUE;
}


static BOOL unroll_can_fold_constant_into(OP *iv_def, OP *iv_use, UINT8 i,
					  UINT8 ntimes)
/* -----------------------------------------------------------------------
 * Requires: OP_opnd(iv_use, i) == OP_result(iv_def)
 * 
 * Return TRUE iff the increment <iv_def> can be folded into <iv_use>
 * up to <ntimes> times.
 *
 * TODO: Get rid of MIPS-specific assumptions about which operands are
 *       immediate (currently assuming OP_iadds have immediate second
 *       operand, and <iv_use> has immediate operand immediately following
 *	 the IV.
 * -----------------------------------------------------------------------
 */
{
  OP *iop = iv_use;
  TOP iopc = OP_code(iop);
  OP *itn_op = iv_def;
  BOOL tracing = Get_Trace(TP_CGPREP, 0x040);
  TN *itn;
  INT64 imm;
  UINT8 u;

  /* If a daddu, then the constant is loaded into a register (r4k workaround).
   */
  if (CGTARG_Is_OP_daddu(itn_op)) itn_op = OP_prev(itn_op);

  itn = OP_opnd(itn_op, 1 /*???*/);

  Is_True(TN_has_value(itn), ("expected a constant TN"));
  Is_True(OP_opnd(iv_use, i) == OP_result(iv_def,0 /*???*/),
	    ("expected opnd %d to be GTN%d", i, TN_number(OP_result(iv_def,0))));

  if (CGPREP_fold_expanded_daddiu && i == 0 && 
      CGTARG_Can_daddu_Be_Folded(OP_prev(iv_use), iv_use)) {
    TN *tmp = OP_result(OP_prev(iv_use),0);
    if (!TN_is_global_reg(tmp)) {
      OP *op = OP_next(iv_use);
      while (op && !OP_Refs_TN(op, tmp)) op = OP_next(op);
      if (op == NULL) {
	iop = OP_prev(iv_use);
	iopc = OP_code(iop);
      }
    }
  }

  if ((!OP_load(iv_use) && !OP_store(iv_use) && !OP_prefetch(iv_use) &&
      !OP_iadd(iv_use)) ||
      i+1 >= OP_opnds(iv_use) ||
      !TN_has_value(OP_opnd(iop,i+1 /*???*/)))
    /* don't clutter trace file with these rejections */
    return FALSE;

  imm = TN_value(OP_opnd(iop, i+1 /*???*/));
  for (u = 1; u <= ntimes; u++) {
    /* Check these individually to guard against false positives due
     * to overflow (extremely unlikely, but possible in theory).
     */
    imm += TN_value(itn);
    if (!TI_TOP_Can_Have_Immediate(imm, iopc)) {
      if (tracing) {
	fprintf(TFile, "\n<constant folding> in BB:%d not folding:\n    ",
		BB_id(OP_bb(iv_use)));
	Print_OP_No_SrcLine(iv_def);
	fprintf(TFile, "  into:\n    ");
	Print_OP_No_SrcLine(iv_use);
	fprintf(TFile, "  ");
	fprintf(TFile, "folded value 0x%" LLX_FMT " out of range for %s\n",
		imm, TI_TOP_Name(OP_code(iv_use)));
      }
      return FALSE;
    }
  }

  return TRUE;
}


static void unroll_fold_constant_into(OP *iv_def, OP *iv_use, UINT8 i,
				      UINT8 multiple)
/* -----------------------------------------------------------------------
 * Requires: unroll_can_fold_constant_into(iv_def, iv_use, i, multiple)
 *
 * Fold <iv_def> into <iv_use>, multiplying the increment by <multiple>.
 *
 * TODO: Get rid of MIPS-specific assumptions about which operands are
 *       immediate (currently assuming OP_iadds have immediate second
 *       operand, and <iv_use> has immediate operand immediately following
 *	 the IV.
 * -----------------------------------------------------------------------
 */
{
  BOOL tracing = Get_Trace(TP_CGPREP, 0x040);
  OP *iop = iv_use;
  OP *itn_op = iv_def;
  UINT8 iopnd = i + 1; /*???*/
  INT64 imm;
  TN *itn;

  if (multiple == 0) return;

  /* If a daddu, then the constant is loaded into a register (r4k workaround).
   */
  if (CGTARG_Is_OP_daddu(itn_op)) itn_op = OP_prev(itn_op);

  itn = OP_opnd(itn_op, 1 /*???*/);
  imm = TN_value(itn) * multiple;

  if (   CGPREP_fold_expanded_daddiu
      && i == 0
      && CGTARG_Can_daddu_Be_Folded(OP_prev(iv_use), iv_use))
    iop = OP_prev(iv_use);

  Is_True(TN_has_value(OP_opnd(iop, iopnd)), ("expected immediate value"));
  imm += TN_value(OP_opnd(iop, iopnd));

  if (tracing) {
    fprintf(TFile, "\n<constant folding> in BB:%d folding:\n    ",
	    BB_id(OP_bb(iv_use)));
    if (CGTARG_Is_OP_daddu(iv_def)) {
      Print_OP_No_SrcLine(itn_op);
      fprintf(TFile, "    ");
    }
    Print_OP_No_SrcLine(iv_def);
    fprintf(TFile, "  into:\n    ");
    if (CGTARG_Is_OP_daddu(iv_use)) {
      Print_OP_No_SrcLine(iop);
      fprintf(TFile, "    ");
    }
    Print_OP_No_SrcLine(iv_use);
  }
  Set_OP_opnd(iop, iopnd, Gen_Literal_TN(imm, TN_size(OP_opnd(iop,iopnd))));
  if (tracing) {
    fprintf(TFile, "  yielding:\n    ");
    if (CGTARG_Is_OP_daddu(iv_use)) {
      Print_OP_No_SrcLine(iop);
      fprintf(TFile, "    ");
    }
    Print_OP_No_SrcLine(iv_use);
  }
}



static BOOL unroll_multi_bb(LOOP_DESCR *loop, UINT8 ntimes)
/* -----------------------------------------------------------------------
 * Requires: ntimes > 1
 *
 * Unroll <loop> <ntimes>.  Return TRUE if successful, FALSE otherwise.
 * -----------------------------------------------------------------------
 */
{
  BB *head = LOOP_DESCR_loophead(loop);
  UINT32 num_bbs = BB_SET_Size(LOOP_DESCR_bbset(loop));
  BB *replicas;
  BB_SET *new_bbs;
  BB **orig_bbs, **orig_br_targ_bbs, **orig_fall_thru_bbs, *bb, *replica;
  float *orig_br_probs;
  float orig_head_freq = BB_freq(head);
  BB_MAP orig_bb_index_map;
  UINT32 bbi, i, unrolling;
  BOOL unrolling_fully = FALSE;
  ANNOTATION *annot = ANNOT_Get(BB_annotations(head), ANNOT_LOOPINFO);
  LOOPINFO *info = annot ? ANNOT_loopinfo(annot) : NULL;
  TN *trip_count_tn = info ? LOOPINFO_trip_count_tn(info) : NULL;
  LOOPINFO *unrolled_info = NULL;
  BOOL freqs = FREQ_Frequencies_Computed();
  BOOL gen_remainder_loop = trip_count_tn && is_power_of_two(ntimes) &&
    CG_LOOP_unroll_multi_make_remainder_loop;
  UINT32 removed_exits = 0;

  MEM_POOL_Push(&MEM_local_nz_pool);
  orig_bbs = TYPE_MEM_POOL_ALLOC_N(BB *, &MEM_local_nz_pool, num_bbs);
  if (!sort_topologically(loop, orig_bbs)) {
    char *reason = "loop has irreducible flow graph";
    MEM_POOL_Pop(&MEM_local_nz_pool);
    if (Get_Trace(TP_CGPREP, 2))
	fprintf(TFile, "<unroll> aborting; %s\n", reason);
    note_not_unrolled(head, reason);
    return FALSE;
  }

  replicas = Gen_BB_N(num_bbs * ntimes);
  if (info) {
    WN *wn = WN_COPY_Tree(LOOPINFO_wn(info));
    unrolled_info = TYPE_P_ALLOC(LOOPINFO);
    LOOPINFO_wn(unrolled_info) = wn;
    WN_loop_trip_est(wn) /= ntimes;
    LOOPINFO_srcpos(unrolled_info) = LOOPINFO_srcpos(info);
    if (trip_count_tn) {
      INT16 new_trip_count_val;
      TYPE_ID ttype = WN_rtype(WN_loop_trip(wn));
      OPCODE opc_intconst = OPCODE_make_op(OPR_INTCONST, ttype, MTYPE_V);
      WN *ntimes_wn = WN_CreateIntconst(opc_intconst, ntimes);
      OPCODE opc_div = OPCODE_make_op(OPR_DIV, ttype, MTYPE_V);
      if (TN_is_constant(trip_count_tn))
	new_trip_count_val = TN_value(trip_count_tn) / ntimes;
      unrolling_fully = CG_LOOP_unroll_fully &&
	TN_is_constant(trip_count_tn) && TN_value(trip_count_tn) == ntimes;
      WN_set_loop_trip(wn, WN_CreateExp2(opc_div, WN_loop_trip(wn),
					 ntimes_wn));
      if (TN_is_constant(trip_count_tn))
      {
	LOOPINFO_trip_count_tn(unrolled_info) =
	  Gen_Literal_TN(new_trip_count_val, TN_size(trip_count_tn));
	LOOPINFO_max_trip_count_tn(unrolled_info) =
	  LOOPINFO_trip_count_tn(unrolled_info);
      }
    }
    LOOPINFO_swp_failure_code(unrolled_info) = LOOPINFO_swp_failure_code(info);
    LOOPINFO_swp_unallococatable_rc(unrolled_info) =
	    				LOOPINFO_swp_unallococatable_rc(info);
  }

  /* Attach new info to head of unrolled body */
  if (unrolled_info)
    BB_Add_Annotation(&replicas[0], ANNOT_LOOPINFO, unrolled_info);
  Set_BB_unrollings(&replicas[0], ntimes);

  /* Setup some data structures, such that, for all 0 <= bbi < num_bbs:
   *   BB_MAP32_Get(orig_bb_index_map, orig_bbs[bbi]) is <bbi> + 1
   *	 (we add 1 to distinguish index 0 from "uninitialized"; this
   *	 is much quicker than checking for membership in BB_SET).
   *   orig_br_targ_bbs[bbi] is the explicit branch target (if any) of
   *     orig_bbs[bbi].  NULL indicates no branch or an indirect branch.
   *   orig_fall_thru_bbs[bbi] is the fall-through successor of orig_bbs[bbi].
   *	 NULL indicates an unconditional branch or an indirect branch.
   */
  orig_bb_index_map = BB_MAP32_Create();
  orig_br_targ_bbs = TYPE_MEM_POOL_ALLOC_N(BB *, &MEM_local_nz_pool, num_bbs);
  orig_br_probs = TYPE_MEM_POOL_ALLOC_N(float, &MEM_local_nz_pool, num_bbs);
  orig_fall_thru_bbs = TYPE_MEM_POOL_ALLOC_N(BB *, &MEM_local_nz_pool,num_bbs);
  for (bbi = 0; bbi < num_bbs; bbi++) {
    BB *orig_bb = orig_bbs[bbi];
    BBLIST *fts_list = BBlist_Fall_Thru_Succ(orig_bb);
    BB *fall_thru = fts_list ? BBLIST_item(fts_list) : NULL;
    float fall_thru_prob = fts_list ? BBLIST_prob(fts_list) : 0.0;
    OP *br_op = BB_branch_op(orig_bb);
    BB *br_targ = NULL;
    float br_prob = 0.0;
    if (br_op) {
      INT br_targ_opnd = Branch_Target_Operand(br_op);
      if (TN_is_label(OP_opnd(br_op, br_targ_opnd))) {
	BBLIST *tlist = (fts_list == BB_succs(orig_bb)) ?
	  BBLIST_next(BB_succs(orig_bb)) : BB_succs(orig_bb);
	LABEL_IDX label = TN_label(OP_opnd(br_op, br_targ_opnd));
	Is_True(tlist, ("BB_succs(BB:%d) missing succ labelled %s",
			BB_id(orig_bb), LABEL_name(label)));
	br_targ = BBLIST_item(tlist);
	Is_True(Is_Label_For_BB(label, br_targ),
		 ("BB_succs(BB:%d) has succ BB:%d, expected one labelled %s",
		 BB_id(orig_bb), BB_id(br_targ), ST_name(label)));
	br_prob = freqs ? BBLIST_prob(tlist) : (fall_thru ? 0.5 : 1.0);
      }
    }
    if (CG_warn_bad_freqs && freqs && br_targ &&
	!FREQ_Match(1.0, br_prob + fall_thru_prob))
      DevWarn("for BB:%d, fall_thru_prob + br_prob != 1.0", BB_id(orig_bb));
    BB_MAP32_Set(orig_bb_index_map, orig_bb, bbi+1);
    orig_br_targ_bbs[bbi] = br_targ;
    orig_br_probs[bbi] = br_prob;
    orig_fall_thru_bbs[bbi] = fall_thru;
  }

  /* Splice the replicas into the old BB chain.  The old loop BBs are
   * intentionally left in the chain since we use their prev/next/pred/succ
   * info while unrolling.
   */
  new_bbs = BB_SET_Create_Empty(PU_BB_Count, &MEM_local_nz_pool);
  Chain_BBs(BB_prev(head), &replicas[0]);
  new_bbs = BB_SET_Union1D(new_bbs, &replicas[0], &MEM_local_nz_pool);
  for (i = 1; i < num_bbs * ntimes; i++) {
    new_bbs = BB_SET_Union1D(new_bbs, &replicas[i], &MEM_local_nz_pool);
    Chain_BBs(&replicas[i-1], &replicas[i]);
  }
  Chain_BBs(&replicas[i-1], head);

  /* Retarget non-loop preds of head to unrolled head. */
  LOOP_DESCR_Retarget_Loop_Entrances(loop, &replicas[0]);

  if (freqs || BB_freq_fb_based(head)) {
    /*
     * Compute frequency of unrolled head.  We'll use this to derive
     * the frequencies of the other replicas as we unroll.
     *
     * TODO: When we emit a remainder loop for trip-countable loops,
     *	     BB_freq(&replicas[0]) should be set to BB_freq(prolog) *
     *	     WN_loop_trip_est(LOOPINFO_wn(unrolled_info)).  The rounding
     *	     down of the trip_est to an integer correctly subtracts the
     *	     portion of the frequency that goes through the remainder loop.
     */
    if (info) {
      INT64 orig_trip_est = WN_loop_trip_est(LOOPINFO_wn(info));
      if (CG_warn_bad_freqs &&
	  !FREQ_Match(orig_head_freq,
		      orig_trip_est * BB_freq(CG_LOOP_prolog)))
	DevWarn("BB_freq(orig head BB:%d) != BB_freq(prolog BB:%d) * trip_est",
		BB_id(head), BB_id(CG_LOOP_prolog));
    }
    BB_freq(&replicas[0]) = gen_remainder_loop ?
      BB_freq(CG_LOOP_prolog) * WN_loop_trip_est(LOOPINFO_wn(unrolled_info)) :
      orig_head_freq / ntimes;
  }

  /* Build the replicas.
   */
  replica = &replicas[0];
  for (unrolling = 0; unrolling < ntimes; unrolling++) {
    for (bbi = 0; bbi < num_bbs; bbi++, replica++) {
      BB *orig_bb = orig_bbs[bbi];
      BB *br_targ = orig_br_targ_bbs[bbi];
      float br_prob = orig_br_probs[bbi];
      BB *fall_thru_dest = orig_fall_thru_bbs[bbi];
      OP *op, *replica_br_op = NULL;
      BOOL fall_thru_dest_in_loop;

      /* Initialize BB info in <replica> */
      Set_BB_loop_head_bb(replica, &replicas[0]);
      Set_BB_unrollings(replica, ntimes);
      if (unrolling_fully) Set_BB_unrolled_fully(replica);
      BB_rid(replica) = BB_rid(head);
      if (BB_freq_fb_based(orig_bb)) Set_BB_freq_fb_based(replica);

      /* Replicate OPs from <orig_bb> into <replica>, renaming TNs as we go
       */
      FOR_ALL_BB_OPs(orig_bb, op) {
	OP *rop;
	UINT8 opi;
	UINT8 resi;
	rop = Dup_OP(op);
	Set_OP_unrolling(rop, unrolling);
	Set_OP_orig_idx(rop, OP_map_idx(op));
	Set_OP_unroll_bb(rop, replica);
	Copy_WN_For_Memory_OP(rop, op);
        for (resi = 0; resi < OP_results(rop); resi++) {
	  TN *res = OP_result(rop,resi);
	  if (!TN_is_global_reg(res)) {
	    TN *new_res = unroll_names_get(res, unrolling);
	    Set_OP_result(rop, resi, new_res);
	  }
	}
	for (opi = 0; opi < OP_opnds(rop); opi++) {
	  TN *opnd = OP_opnd(rop, opi);
	  if (TN_is_register(opnd)) {
	    if (!TN_is_global_reg(opnd)) {
	      Set_OP_opnd(rop, opi, unroll_names_get(opnd, unrolling));
	    }
	  }
	}
	BB_Append_Op(replica, rop);
	if (OP_br(rop)) replica_br_op = rop;
      }
      Is_True(br_targ == NULL || replica_br_op, ("no replica branch op"));
      unroll_xfer_annotations(replica, orig_bb);

      /* Retarget <fall_thru_dest> if it's in loop */
      if (fall_thru_dest) {
	INT32 ftd_bbi = BB_MAP32_Get(orig_bb_index_map, fall_thru_dest) - 1;
	fall_thru_dest_in_loop = ftd_bbi >= 0;
	if (fall_thru_dest_in_loop) {
	  /* If <ftd_bbi> is zero, want loop head in next unrolling */
	  UINT32 iter = ftd_bbi > 0 ? unrolling : (unrolling + 1) % ntimes;
	  fall_thru_dest = &replicas[iter * num_bbs + ftd_bbi];
	}
      }

      /* Retarget intra-loop branches */
      if (replica_br_op) {
	INT replica_targ_opnd = Branch_Target_Operand(replica_br_op);
	if (br_targ == NULL) {	/* Indirect branch */
	  BBLIST *succs;
	  Is_True(!TN_is_label(OP_opnd(replica_br_op, replica_targ_opnd)),
		    ("expected indirect branch"));
	  FOR_ALL_BB_SUCCS(orig_bb, succs) {
	    BB *succ = BBLIST_item(succs);
	    Is_True(!BB_SET_MemberP(LOOP_DESCR_bbset(loop), succ),
		      /* CG_LOOP_Unroll should filter these out */
		      ("not yet retargeting intra-loop indirect branches"));
	    Link_Pred_Succ_with_Prob(replica, succ, BBLIST_prob(succs));
	  }
	} else {
	  INT32 bt_bbi = BB_MAP32_Get(orig_bb_index_map, br_targ) - 1;
	  BOOL br_targ_in_loop = bt_bbi >= 0;
	  if (br_targ_in_loop) {
	    /* If <bt_bbi> is zero, want loop head in next unrolling */
	    UINT32 iter = bt_bbi > 0 ? unrolling : (unrolling + 1) % ntimes;
	    br_targ = &replicas[iter * num_bbs + bt_bbi];
	  }
	  if (unrolling_fully && br_targ == &replicas[0]) {
	    /* No need for branch from tail to head if unrolling fully */
	    BB_Remove_Op(replica, replica_br_op);
	    br_prob = 0.0;
	  } else if (br_targ == BB_next(replica)) {
	    /*
	     * <br_targ> is next.  To minimize branching within the
	     * loop, we'll reverse this branch to target <fall_thru_dest>
	     * (or remove it if there's no <fall_thru_dest> or we know it's
	     * always taken), and let <replica> fall through to <br_targ>.
	     */
	    if (gen_remainder_loop && bt_bbi == 0) {
	      /* Remove always-taken branch to next iter head */
	      BB_Remove_Op(replica, replica_br_op);
	      br_prob = 0.0;
	    } else if (fall_thru_dest && fall_thru_dest != br_targ) {
	      if (Negate_Branch(replica_br_op)) {
		Set_OP_opnd(replica_br_op, replica_targ_opnd,
			    Gen_Label_TN(Gen_Label_For_BB(fall_thru_dest), 0));
		br_prob = 1.0 - br_prob;
		Link_Pred_Succ_with_Prob(replica, fall_thru_dest, br_prob);
		if (fall_thru_dest_in_loop && fall_thru_dest != &replicas[0] &&
		    (freqs || BB_freq_fb_based(fall_thru_dest)))
		BB_freq(fall_thru_dest) += br_prob * BB_freq(replica);
	      } else {
		#pragma mips_frequency_hint NEVER
		DevWarn("unable to negate branch %s", TI_TOP_Name(OP_code(replica_br_op)));
	      }
	    } else {
	      BB_Remove_Op(replica, replica_br_op);
	      br_prob = 0.0;
	    }
	    fall_thru_dest = br_targ;
	    fall_thru_dest_in_loop = br_targ_in_loop;
	  } else if (br_targ_in_loop) {
	    /*
	     * Retarget internal branch to <br_targ>.
	     */
	    Set_OP_opnd(replica_br_op, replica_targ_opnd,
			Gen_Label_TN(Gen_Label_For_BB(br_targ), 0));
	    Link_Pred_Succ_with_Prob(replica, br_targ, br_prob);
	    if (br_targ != &replicas[0] &&
		(freqs || BB_freq_fb_based(br_targ)))
	      BB_freq(br_targ) += br_prob * BB_freq(replica);
	  } else if (br_targ) {
	    /*
	     * Branch to external target - no change.
	     */
	    Link_Pred_Succ_with_Prob(replica, br_targ, br_prob);
	  }
	}
      }

      /* If non-nil <fall_thru_dest> isn't next in the BB chain,
       * insert a fall-through block that branches directly to it.
       */
      if (fall_thru_dest) {
	float ft_prob = 1.0 - br_prob;
	if (BB_next(replica) != fall_thru_dest) {
	  BB *new_bb = Gen_And_Insert_BB_After(replica);
	  BB_rid(new_bb) = BB_rid(head);
	  if (BB_freq_fb_based(replica)) Set_BB_freq_fb_based(new_bb);
	  if (freqs || BB_freq_fb_based(new_bb))
	    BB_freq(new_bb) = ft_prob * BB_freq(replica);
	  Add_Goto(new_bb, fall_thru_dest);
	  if (fall_thru_dest_in_loop) {
	    new_bbs = BB_SET_Union1D(new_bbs, new_bb, &MEM_local_nz_pool);
	    Set_BB_loop_head_bb(new_bb, &replicas[0]);
	    Set_BB_unrollings(new_bb, ntimes);
	  }
	}
	Link_Pred_Succ_with_Prob(replica, BB_next(replica), ft_prob);
	if (fall_thru_dest_in_loop && fall_thru_dest != &replicas[0] &&
	    (freqs || BB_freq_fb_based(fall_thru_dest)))
	  BB_freq(fall_thru_dest) += ft_prob * BB_freq(replica);
      }
    }
  }


  /* Restore original head freq so remainder freqs can be set correctly.
   */
  BB_freq(head) = orig_head_freq;

  /* Add "remainder" loop to prolog to perform first (trip_count %
   * ntimes) iterations (if necessary).  Note that this causes a
   * new set of prolog backpatches for the unrolled body to be
   * generated, and changes <head>.  Restore original head frequency
   * so remainder frequencies can be set correctly.
   */
  if (!gen_remainder_loop ||
      !unroll_multi_make_remainder_loop(loop, ntimes, orig_bbs, num_bbs)) {
    /*
     * Remainder loop wasn't necessary.  Remove original loop BBs
     * from flow graph and BB chain.
     */
    BB *post_prolog = BB_next(CG_LOOP_prolog);
    gen_remainder_loop = FALSE;
    for (bbi = 0; bbi < num_bbs; bbi++) {
      BB *loop_bb = orig_bbs[bbi];
      BB *prev = BB_prev(loop_bb);
      BB *next = BB_next(loop_bb);
      RID *rid = BB_rid(loop_bb);
      while (BB_succs(loop_bb))
	Unlink_Pred_Succ(loop_bb, BBLIST_item(BB_succs(loop_bb)));
      while (BB_preds(loop_bb))
	Unlink_Pred_Succ(BBLIST_item(BB_preds(loop_bb)), loop_bb);
      if (prev && BB_next(prev) == loop_bb) BB_next(prev) = next;
      if (next && BB_prev(next) == loop_bb) BB_prev(next) = prev;
      BB_prev(loop_bb) = BB_next(loop_bb) = NULL;
      if (REGION_First_BB == loop_bb) REGION_First_BB = next;
      if (rid && RID_cginfo(rid)) {
	/* update cgrin pointers */
	CGRIN *cgrin = RID_cginfo(rid);
	if (CGRIN_first_bb(cgrin) == loop_bb) CGRIN_first_bb(cgrin) = next;
	if (CGRIN_last_bb(cgrin) == loop_bb) CGRIN_last_bb(cgrin) = prev;
      }
    }
    BB_next(CG_LOOP_prolog) = post_prolog;
  }

  /* Update loop descriptor */
  LOOP_DESCR_loophead(loop) = &replicas[0];
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
    LOOP_DESCR_Delete_BB(loop, bb);
  FOR_ALL_BB_SET_members(new_bbs, bb)
    LOOP_DESCR_Add_BB(loop, bb);
  LOOP_DESCR_loopinfo(loop) = unrolled_info;
  LOOP_DESCR_num_exits(loop) =
    LOOP_DESCR_num_exits(loop) * ntimes - removed_exits;

  if (gen_remainder_loop && !TN_is_constant(trip_count_tn)) {
    /*
     * Add zero-trip guard around unrolled body.  Must wait until
     * remainder loop has been placed in prolog before doing this.
     * This also creates a new trip count TN for <unrolled_info>.
     */
    unroll_guard_unrolled_body(loop, unrolled_info, trip_count_tn, ntimes);
  }

  BB_MAP_Delete(orig_bb_index_map);
  MEM_POOL_Pop(&MEM_local_nz_pool);
  return TRUE;
}


void CG_LOOP_Finish(void)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  /* Nothing to do yet */
  /* TODO: Decide when to delete _CG_LOOP_bb_unrollings_map and
   * _CG_LOOP_op_unrolling_map.  Either don't do it here, or (better,
   * I think) don't call this until after scheduling (which should
   * make me finally invoke the scheduler correctly).  But unrollings
   * info needs to live past GRA through second optional scheduling
   * phase, right?  So maybe these should be OP/BB attributes.
   */
}

BB* 
CG_LOOP_Gen_And_Prepend_To_Prolog(BB *loop_head, LOOP_DESCR* loop)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  BB *ftp = BB_Fall_Thru_Predecessor(loop_head);
  LOOP_DESCR *enclosing = LOOP_DESCR_Next_Enclosing_Loop(loop);
  BB *new_prolog = Gen_BB_Like(loop_head);
  BOOL freqs = FREQ_Frequencies_Computed();

  if (!CG_localize_tns) Set_BB_gra_spill(new_prolog);
  if (BB_freq_fb_based(loop_head)) Set_BB_freq_fb_based(new_prolog);
  LOOP_DESCR_Retarget_Loop_Entrances(loop, new_prolog);

  if (ftp && BB_SET_MemberP(LOOP_DESCR_bbset(loop), ftp)) {
    /*
     * Can't make prolog fall-thru predecessor of <loop_head> without
     * adding branches in loop, so insert it at end of BB chain and
     * make it branch to <loop_head>.
     *
     * TODO: (Compspeed) Avoid walking BB chain to find last one by
     *       tracking REGION_Last_BB?  Can we just insert it before
     *       REGION_First_BB?
     */
    BB *last_bb = REGION_First_BB;
    while (BB_next(last_bb)) last_bb = BB_next(last_bb);
    Insert_BB(new_prolog, last_bb);
    Add_Goto(new_prolog, loop_head);
  } else {
    Insert_BB(new_prolog, BB_prev(loop_head));
    Link_Pred_Succ_with_Prob(new_prolog, loop_head, 1.0);
  }

  if (freqs || BB_freq_fb_based(new_prolog))
    BB_freq(loop_head) += BB_freq(new_prolog);

  /* Add CG_LOOP_prolog to appropriate LOOP_DESCRs, if any.
   * It can belong only to loops enclosing this one, so
   * we don't bother checking any others.
   */

  if (enclosing && 
      all_bbs_in(BB_preds(new_prolog), LOOP_DESCR_bbset(enclosing))) {
    LOOP_DESCR_Add_BB(enclosing, new_prolog);
    BB_nest_level(new_prolog) = LOOP_DESCR_nestlevel(enclosing);
  }

  return new_prolog;
}

BB*
CG_LOOP_Append_BB_To_Prolog(BB *loop_prolog, BB *loop_head)
/* -----------------------------------------------------------------------
 * See "cg_loop.h" for interface.
 * -----------------------------------------------------------------------
 */
{
  OP *br_op = BB_branch_op(loop_prolog);
  BB *new_bb = Gen_And_Insert_BB_After(loop_prolog);
  BOOL freqs = FREQ_Frequencies_Computed();

  LOOP_DESCR *prolog_loop = LOOP_DESCR_Find_Loop(loop_prolog);
  if (BB_freq_fb_based(loop_prolog)) Set_BB_freq_fb_based(new_bb);
  BB_rid(new_bb) = BB_rid(loop_prolog);
  Is_True(!TN_is_label(OP_opnd(br_op, Branch_Target_Operand(br_op))) ||
	  Is_Label_For_BB(TN_label(OP_opnd(br_op, Branch_Target_Operand(br_op))), loop_head),
	  ("explicit branch target should be <loop_head>"));

  BB_Remove_Op(loop_prolog, br_op);
  Change_Succ(loop_prolog, loop_head, new_bb);
  Add_Goto(new_bb, loop_head);

  if (freqs || BB_freq_fb_based(loop_prolog))
    BB_freq(loop_head) += BB_freq(new_bb);

  if (prolog_loop) LOOP_DESCR_Add_BB(prolog_loop, new_bb);

  return new_bb;
}


/* =======================================================================
 *
 *  CG_LOOP_Coalesce_Backedges
 *
 *    See interface description.
 *
 * =======================================================================
 */
void 
CG_LOOP_Coalesce_Backedges(LOOP_DESCR* loop)
{
  BB* bb_back;
  BB* head = LOOP_DESCR_loophead(loop);

  //
  // Add new bb before loop head and have it fall through.  Set freq
  // to 0.  When the branches are added to it in BB_Retarget_Branch,
  // the frequency will get updated.
  //
  bb_back = Gen_And_Insert_BB_Before(head);
  BB_freq(bb_back) = 0.0;
  LOOP_DESCR_Add_BB(loop, bb_back);

  //
  // Move all of the backedge branches to this new block.
  //
  BBLIST* preds = BB_preds(head);
  while (preds) {
    BB* pred = BBLIST_item(preds); 
    preds = BBLIST_next(preds);
    if (BB_SET_MemberP(LOOP_DESCR_bbset(loop), pred)) {
      if (!BB_Retarget_Branch(pred, head, bb_back)) {
	// must fall-thru to loop head 
	Change_Succ(pred, head, bb_back);
      }
    }
  }

  //
  // Make new backedge block branch to loop head, and 
  // calculate liveness information.
  //
  Add_Goto(bb_back, head);
  GRA_LIVE_Compute_Liveness_For_BB(bb_back);
}
	
/* For debugging (can't call varargs function CG_LOOP_Trace_Loop from dbx!) */
void trace_loop(LOOP_DESCR *loop)
{
  CG_LOOP_Trace_Loop(loop, "");
}


static void Unroll_Do_Loop_guard(LOOP_DESCR *loop,
				 LOOPINFO *unrolled_info,
				 TN *unrolled_trip_count)
{
  INT64 trip_est = WN_loop_trip_est(LOOPINFO_wn(unrolled_info));
  float ztrip_prob = (trip_est > 0 && BB_freq_fb_based(BB_next(CG_LOOP_prolog))
                      ? 0.0 : 1.0 / MAX(trip_est, 1));
  float orig_post_prolog_freq = BB_freq(BB_next(CG_LOOP_prolog));
  OPS ops = OPS_EMPTY;
  BB *continuation_bb;
  LABEL_IDX continuation_lbl;

  extend_epilog(loop);
  continuation_bb = CG_LOOP_epilog;
  continuation_lbl = Gen_Label_For_BB(continuation_bb);

  Exp_OP3v(OPC_FALSEBR,
	   NULL,
	   Gen_Label_TN(continuation_lbl,0),
	   unrolled_trip_count,
	   Gen_Literal_TN(0, TN_size(unrolled_trip_count)),
	   V_BR_I8EQ,
	   &ops);
  BB_Append_Ops(CG_LOOP_prolog, &ops);
  Link_Pred_Succ_with_Prob(CG_LOOP_prolog, continuation_bb, ztrip_prob);
  Change_Succ_Prob(CG_LOOP_prolog, BB_next(CG_LOOP_prolog), 1.0 - ztrip_prob);
  BB_freq(BB_next(CG_LOOP_prolog)) = orig_post_prolog_freq;

  /* Extend prolog and epilog in case any further optimizations
   * want to use them.
   */
  extend_prolog();
  extend_epilog(loop);
}


//  Unroll a single-bb do-loop
//
void Unroll_Do_Loop(CG_LOOP& cl, UINT32 ntimes)
{
  LOOP_DESCR *loop = cl.Loop();
  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "Before unroll BB:%d %d times:",
		       BB_id(LOOP_DESCR_loophead(loop)), ntimes);

  BB *head = LOOP_DESCR_loophead(loop);
  TN *trip_count_tn = CG_LOOP_Trip_Count(loop);
  BB *unrolled_body;
  ANNOTATION *annot;
  LOOPINFO *info=LOOP_DESCR_loopinfo(loop);
  LOOPINFO *unrolled_info;
  TN *unrolled_trip_count = NULL;
#ifdef TARG_XTENSA
  // Expand uses the instruction selector by converting TNs to WHIRL. The
  // simplifier can't handle these kinds of WHIRL nodes.
  BOOL simp = WN_Simplifier_Enable(FALSE);
#endif

  if (!TN_is_constant(trip_count_tn)) {
    BB* prolog_bb = CG_LOOP_prolog;
    while (prolog_bb) {
      if (BB_length(prolog_bb)!=0)
        break;
      prolog_bb = BB_prev(prolog_bb);
    }
    if (prolog_bb && BB_length(prolog_bb)>0) {
      DEF_KIND kind;
      OP* trip_count_def = TN_Reaching_Value_At_Op(trip_count_tn, BB_last_op(prolog_bb),
		    		&kind, /*reaching def=*/TRUE);
      if (kind==VAL_KNOWN && OP_code(trip_count_def)==TOP_movi) {
        trip_count_tn = OP_opnd(trip_count_def,0);
        LOOPINFO_trip_count_tn(info) = trip_count_tn;
      }
    }
  }

  OPS ops = OPS_EMPTY;
  BOOL gen_remainder_loop = TRUE;
  BOOL gen_unrolled_loop_guard = TRUE;
  INT64 trip_est = WN_loop_trip_est(LOOPINFO_wn(info));
  // Impose the restriction that the unrolled body must execute at least two times.
  // There is no point in handling that here because such loop should be fully unrolled.

  if (TN_is_constant(trip_count_tn))
    trip_est = TN_value(trip_count_tn);
  if (trip_est < 2 * ntimes ) {
    if (Get_Trace(TP_CGPREP, 2))
      CG_LOOP_Trace_Loop(loop, "disable unrolling because trip_count(%" LLD_FMT ") < 2 * ntimes(%d).",
			   trip_est, ntimes);
    note_not_unrolled(head, "trip_count(%" LLD_FMT ") < 2 * unroll_factor(%d)",
			trip_est, ntimes);
#ifdef TARG_XTENSA
    WN_Simplifier_Enable(simp);
#endif
    cl.Set_unroll_factor(1);
    return;
  } else if (BB_length(head)*ntimes > 2048) {
    if (Get_Trace(TP_CGPREP, 2))
      CG_LOOP_Trace_Loop(loop, "disable unrolling because BB length(%d) * ntimes(%d) > 2048.",
			   BB_length(head), ntimes);
    note_not_unrolled(head, "BB length(%d) * unroll_factor(%d) > 2048",
			BB_length(head), ntimes);
#ifdef TARG_XTENSA
    WN_Simplifier_Enable(simp);
#endif
    cl.Set_unroll_factor(1);
    return;
  }
  if (TN_is_constant(trip_count_tn)) {
    gen_unrolled_loop_guard = FALSE;  // because unrolling is disabled for 0-trip loops
    if (TN_value(trip_count_tn) % ntimes == 0)
      gen_remainder_loop = FALSE;
    INT32 trip_size = TN_size(trip_count_tn);
    INT32 trip_count = TN_value(trip_count_tn);
    unrolled_trip_count = Gen_Literal_TN(trip_count/ntimes, trip_size);
  } else {
    INT32 trip_size = TN_size(trip_count_tn);
    TYPE_ID ttype = TN_mtype(trip_count_tn);
    unrolled_trip_count = Build_TN_Like(trip_count_tn);
    if (is_power_of_two(ntimes)) 
      Exp_OP2((MTYPE_is_unsigned(ttype) ?
	       OPCODE_make_op(OPR_LSHR, ttype, MTYPE_V) :
	       OPCODE_make_op(OPR_ASHR, ttype, MTYPE_V)),
	      unrolled_trip_count,
	      trip_count_tn,
	      Gen_Literal_TN(int_log2(ntimes), trip_size),
	      &ops);
    else {
      DevWarn("unrolling by a factor not a power of two");
      Exp_OP2(OPCODE_make_op(OPR_DIV, ttype, MTYPE_V),
	      unrolled_trip_count,
	      trip_count_tn,
	      Gen_Literal_TN(ntimes, trip_size),
	      &ops);
    }
  }

#if 0 /* TENSILICA : fixme */
  // Replace the loop-back branch with the counted loop branch
  // instruction.  It is a nop for the MIPS architecture.
  {
    OPS body_ops = OPS_EMPTY;
    OP *br_op = BB_branch_op(head);
    TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));

    CGTARG_Generate_Branch_Cloop(br_op, unrolled_trip_count, trip_count_tn,
				 ntimes, label_tn, &ops, &body_ops);
    if (OPS_length(&body_ops) > 0) {
      BB_Remove_Op(head, br_op);
      BB_Append_Ops(head, &body_ops);
      CGPREP_Init_Op(BB_branch_op(head));
      CG_LOOP_Init_Op(BB_branch_op(head));
    }
  }
#endif

  /* TENSILICA */
  /* The remainder loop seems to break things... we need to rework all
     this for xtensa anyway... */
#if 0
  if (gen_remainder_loop) {
    WN_Simplifier_Enable(simp);
    return;
  }
#endif

  /* if this loop was produced for a do-while loop, there is
     no entry guard test, and so overflows in the trip-count
     calculation can cause incorrect trip counts. */
  if (info) {
    WN *wn_loopinfo = LOOPINFO_wn(info);
    if (wn_loopinfo) {
      if (WN_Loop_Dowhile(wn_loopinfo)) {
#ifdef TARG_XTENSA
	WN_Simplifier_Enable(simp);
#endif
	cl.Set_unroll_factor(1);
	return;
      }
    }
  }

  /* Initialize the TN renamer */
  unroll_names_init(loop, ntimes, &MEM_phase_nz_pool);

  /* Replicate the loop body <ntimes> and replace <head>
   * with unrolled version. */
  unrolled_body = Unroll_Replicate_Body(loop, ntimes, FALSE);
  annot = ANNOT_Get(BB_annotations(unrolled_body), ANNOT_LOOPINFO);
  FmtAssert(annot, ("unrolled body has no LOOPINFO annotation"));
  unrolled_info = ANNOT_loopinfo(annot);

  /* Creates a new trip count TN for <unrolled_info>.  */
  LOOPINFO_trip_count_tn(unrolled_info) = unrolled_trip_count;
  LOOPINFO_max_trip_count_tn(unrolled_info) = unrolled_trip_count;

  /* Add "remainder" loop to prolog to perform first (trip_count %
   * ntimes) iterations (if necessary).  Note that this causes a
   * new set of prolog backpatches for the unrolled body to be
   * generated, and changes <head>.
   */
  if (gen_remainder_loop) {
    // CG_DEP_Delete_Graph(head);
    Unroll_Make_Remainder_Loop(cl, ntimes);
  }

  /* Update loop descriptor for unrolled loop */
  LOOP_DESCR_loophead(loop) = unrolled_body;
  LOOP_DESCR_Delete_BB(loop, head);
  LOOP_DESCR_Add_BB(loop, unrolled_body);
  LOOP_DESCR_loopinfo(loop) = unrolled_info;

  // Insert unrolled trip count computation! 
  // It cannot be inserted earlier because unroll_make_remainder_loop
  // used/modified CG_LOOP_prolog!
  BB_Append_Ops(CG_LOOP_prolog, &ops);

  /* Add zero-trip guard around unrolled body if necessary.  Must wait
   * until remainder loop has been placed in prolog before doing this.
   */
  if (gen_unrolled_loop_guard)
    Unroll_Do_Loop_guard(loop, unrolled_info, unrolled_trip_count);

  /* Fixup prolog backpatches.  Replace body TNs and omegas as if
   * they're uses in the zeroth unrolling.  */
  unroll_rename_backpatches(CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL),
			    0, ntimes);

  /* Fixup epilog backpatches.  Replace body TNs and omegas as if
   * they're uses in the last unrolling.
   */
  unroll_rename_backpatches(CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL),
			    ntimes-1, ntimes);

  unroll_names_finish();

  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "After unrolling BB:%d %d times:",
		       BB_id(head), ntimes);
#ifdef TARG_XTENSA
  WN_Simplifier_Enable(simp);
#endif
}

//  Fully unroll a single-bb do-loop
//
void Unroll_Do_Loop_Fully(LOOP_DESCR *loop, UINT32 ntimes)
{
  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "Before unrolling BB:%d %d times:",
		       BB_id(LOOP_DESCR_loophead(loop)), ntimes);

  BB *head = LOOP_DESCR_loophead(loop);
  TN *trip_count_tn = CG_LOOP_Trip_Count(loop);
  BB *unrolled_body;
  ANNOTATION *annot;
  LOOPINFO *unrolled_info;

  Is_True(TN_is_constant(trip_count_tn) &&
	  TN_value(trip_count_tn) == ntimes,
	  ("Unroll_Do_Loop_Fully: unable to fully unroll do_loop."));

  /* Initialize the TN renamer */
  unroll_names_init(loop, ntimes, &MEM_phase_nz_pool);

  /* Replicate the loop body <ntimes> and replace <head>
   * with unrolled version. */
  unrolled_body = Unroll_Replicate_Body(loop, ntimes, TRUE);
  annot = ANNOT_Get(BB_annotations(unrolled_body), ANNOT_LOOPINFO);
  FmtAssert(annot, ("unrolled body has no LOOPINFO annotation"));
  unrolled_info = ANNOT_loopinfo(annot);

  /* Update loop descriptor for unrolled loop */
  LOOP_DESCR_loophead(loop) = unrolled_body;
  LOOP_DESCR_Delete_BB(loop, head);
  LOOP_DESCR_Add_BB(loop, unrolled_body);
  LOOP_DESCR_loopinfo(loop) = unrolled_info;

  /* Fixup prolog backpatches.  Replace body TNs and omegas as if
   * they're uses in the zeroth unrolling.  */
  unroll_rename_backpatches(CG_LOOP_Backpatch_First(CG_LOOP_prolog, NULL),
			    0, ntimes);

  /* Fixup epilog backpatches.  Replace body TNs and omegas as if
   * they're uses in the last unrolling.
   */
  unroll_rename_backpatches(CG_LOOP_Backpatch_First(CG_LOOP_epilog, NULL),
			    ntimes-1, ntimes);

  // Rename unrolled_body using prolog and epilog TNs.
  unroll_remove_notations(unrolled_body);

  unroll_names_finish();

  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "After unrolling BB:%d %d times:",
		       BB_id(head), ntimes);
}


// Unroll single BB dowhile loop
//  * It is different from unroll_doloop because it does not rely on
//    CG_LOOP data structure, i.e.,  there is no CG_LOOP_prolog/epilog.
//    And there is CG_LOOP_info_map ...
//
void
Unroll_Dowhile_Loop(LOOP_DESCR *loop, UINT32 ntimes)
{
  if (ntimes <= 1)
    return;

  MEM_POOL_Push(&MEM_local_nz_pool);

  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "Before unrolling BB:%d %d times:",
		       BB_id(LOOP_DESCR_loophead(loop)), ntimes);

  BB *head = LOOP_DESCR_loophead(loop);
  BB_SET *new_bbs;
  ANNOTATION *annot = ANNOT_Get(BB_annotations(head), ANNOT_LOOPINFO);
  LOOPINFO *info = annot ? ANNOT_loopinfo(annot) : NULL;
  LOOPINFO *unrolled_info = NULL;
  BOOL freqs = FREQ_Frequencies_Computed() || BB_freq_fb_based(head) ;

  BB *replicas = Gen_BB_N(ntimes-1);
  Set_BB_unrollings(&replicas[0], ntimes);

  if (info) {
    WN *wn = WN_COPY_Tree(LOOPINFO_wn(info));
    unrolled_info = TYPE_P_ALLOC(LOOPINFO);
    LOOPINFO_wn(unrolled_info) = wn;
    WN_loop_trip_est(wn) /= ntimes;
    WN_loop_trip_est(wn) += 1;
    LOOPINFO_srcpos(unrolled_info) = LOOPINFO_srcpos(info);
    LOOPINFO_swp_failure_code(unrolled_info) = LOOPINFO_swp_failure_code(info);
    LOOPINFO_swp_unallococatable_rc(unrolled_info) =
	    				LOOPINFO_swp_unallococatable_rc(info);
    BB_Add_Annotation(&replicas[0], ANNOT_LOOPINFO, unrolled_info);
  }

  /* Initialize the TN renamer */
  unroll_names_init(loop, ntimes, &MEM_phase_nz_pool);

  if (freqs || BB_freq_fb_based(head)) {
    /*
     * Compute frequency of unrolled head.  We'll use this to derive
     * the frequencies of the other replicas as we unroll.
     *
     * TODO: When we emit a remainder loop for trip-countable loops,
     *	     BB_freq(&replicas[0]) should be set to BB_freq(prolog) *
     *	     WN_loop_trip_est(LOOPINFO_wn(unrolled_info)).  The rounding
     *	     down of the trip_est to an integer correctly subtracts the
     *	     portion of the frequency that goes through the remainder loop.
     */
    if (info) {
      INT64 orig_trip_est = WN_loop_trip_est(LOOPINFO_wn(info));
      float orig_head_freq = BB_freq(head);
      if (CG_warn_bad_freqs &&
	  !FREQ_Match(orig_head_freq,
		      orig_trip_est * BB_freq(CG_LOOP_prolog)))
	DevWarn("BB_freq(orig head BB:%d) != BB_freq(prolog BB:%d) * trip_est",
		BB_id(head), BB_id(CG_LOOP_prolog));
    }
  }

  OP *br_op = BB_branch_op(head);
  INT br_targ_opnd = Branch_Target_Operand(br_op);
  BBLIST *fts_list = BBlist_Fall_Thru_Succ(head);
  BB *loop_merge_bb = BBLIST_item(fts_list);
  TN *loop_merge_label = Gen_Label_TN(Gen_Label_For_BB(loop_merge_bb), 0);
  float br_prob = freqs ? (1.0 - BBLIST_prob(fts_list)) : 0.5;
  float replica_prob = (freqs || BB_freq_fb_based(head)) ?
    (BB_freq(head) + ntimes - 1)/ ntimes : 1.0;

  /* Splice the replicas into the old BB chain.  The old loop BBs are
   * intentionally left in the chain since we use their prev/next/pred/succ
   * info while unrolling.
   */
  new_bbs = BB_SET_Create_Empty(PU_BB_Count, &MEM_local_nz_pool);
  Chain_BBs(BB_prev(head), &replicas[0]);
  for (INT i = 0; i < ntimes - 1; i++) {
    new_bbs = BB_SET_Union1D(new_bbs, &replicas[i], &MEM_local_nz_pool);
    Chain_BBs(&replicas[i], (i == ntimes - 2) ? head : &replicas[i+1]);
  }

  while (BB_preds(head)) {
    BB *pred = BBLIST_item(BB_preds(head));
    Change_Succ(pred, head, &replicas[0]);
  }

  /* Build the replicas.
   */
  for (INT unrolling = 0; unrolling < ntimes - 1; unrolling++) {
    /* Initialize BB info in <replica> */
    BB *replica = &replicas[unrolling];
    Set_BB_loop_head_bb(replica, &replicas[0]);
    Set_BB_unrollings(replica, ntimes);
    BB_rid(replica) = BB_rid(head);
    if (BB_freq_fb_based(head)) Set_BB_freq_fb_based(replica);

    /* Replicate OPs from <orig_bb> into <replica>, renaming TNs as we go
     */
    OP *op;
    FOR_ALL_BB_OPs(head, op) {
      OP *rop = Dup_OP(op);
      Set_OP_unrolling(rop, unrolling);
      Set_OP_orig_idx(rop, OP_map_idx(op));
      Set_OP_unroll_bb(rop, replica);
      Copy_WN_For_Memory_OP(rop, op);
      for (INT resi = 0; resi < OP_results(rop); resi++) {
	TN *res = OP_result(rop,resi);
	if (!TN_is_global_reg(res)) {
	  TN *new_res = unroll_names_get(res, unrolling);
	  Set_OP_result(rop, resi, new_res);
	}
      }
      for (INT opi = 0; opi < OP_opnds(rop); opi++) {
	TN *opnd = OP_opnd(rop, opi);
	if (TN_is_register(opnd)) {
	  if (!TN_is_global_reg(opnd)) {
	    Set_OP_opnd(rop, opi, unroll_names_get(opnd, unrolling));
	  }
	}
      }
      BB_Append_Op(replica, rop);
    }

    OP *replica_br_op = BB_branch_op(replica);
    BOOL ok = Negate_Branch(replica_br_op);
    Is_True(ok, ("unable to negate branch %s", TI_TOP_Name(OP_code(replica_br_op))));
    Set_OP_opnd(replica_br_op, Branch_Target_Operand(replica_br_op),
		loop_merge_label);
    Link_Pred_Succ_with_Prob(replica, loop_merge_bb, 1.0 - br_prob);
    Link_Pred_Succ_with_Prob(replica, BB_next(replica), br_prob);
    if (freqs)
      BB_freq(replica) = replica_prob;
    replica_prob *= br_prob;
  }
  if (freqs)
    BB_freq(head) = replica_prob;

  {
    // update loopback edge
    unroll_xfer_annotations(&replicas[0], head);
    OP *br = BB_branch_op(head);
    Set_OP_opnd(br,
		Branch_Target_Operand(br),
		Gen_Label_TN(Gen_Label_For_BB(&replicas[0]),0));
  }

  {
    /* Update loop descriptor */
    LOOP_DESCR_loophead(loop) = &replicas[0];
    BB *bb;
    FOR_ALL_BB_SET_members(new_bbs, bb)
      LOOP_DESCR_Add_BB(loop, bb);
    LOOP_DESCR_loopinfo(loop) = unrolled_info;
  }

  unroll_names_finish();

  if (Get_Trace(TP_CGPREP, 2))
    CG_LOOP_Trace_Loop(loop, "After unrolling BB:%d %d times:",
		       BB_id(head), ntimes);

  MEM_POOL_Pop(&MEM_local_nz_pool);
}


static BB *
Find_nonempty_succ(BB* bb)
{
  BOOL done = FALSE;

  while (!done) {
    OP *op;
    BOOL empty_bb = TRUE;
    if ((bb == CG_LOOP_prolog) || (bb == CG_LOOP_epilog)) {
      empty_bb = (CG_LOOP_Backpatch_First(bb, NULL) == NULL);
    }
    FOR_ALL_BB_OPs(bb, op) {
      if (!OP_dummy(op)) {
	empty_bb = FALSE;
	break;
      }
    }
    if (empty_bb && BB_Unique_Successor(bb))
      bb = BB_Unique_Successor(bb);
    else
      done = TRUE;
  }
  return bb;
}

/* Try to remove the conditional branch guarding 'prolog', given that
   'prolog' ends in a zcl */
BOOL
Remove_Guard_Branch (BB *prolog)
{
  BOOL trace = Get_Trace(TP_CGPREP, 2);
  if (trace)
  {
    fprintf(TFile, "<zcl> attempting to remove guard branch in BB:%d,\n",
		    BB_id(prolog));
  }

  /* 'prolog' must have a chain of unique predecessors reaching a
     conditional branch. We determine if this branch is acting as a
     guard by examining the instructions in the prolog, and the TNs
     used in the branch. If we find the branch is redundant with the
     zcl instruction, and all the prolog OPs can be speculated, we
     remove the branch (based on some estimation of the benefit using
     the number of OPs in the prolog, likely branch outcome,
     etc.). Even if we find that the branch is redundant, and that
     removing it appears beneficial, removing it could still be the
     wrong thing perfomance-wise, because it could be short-circuiting
     some other test that occurs after the loop. However, it would be
     difficult to determine this, so we don't even try. */

  OP *loop_op = BB_branch_op(prolog);
  TOP loop_top = ((loop_op) ? OP_code(loop_op) : TOP_UNDEFINED);
  if (!loop_op ||
      ((loop_top != TOP_loopgtz) && (loop_top != TOP_loopnez)))
  {
    if (trace)
      fprintf(TFile, "<zcl> prolog does not end in loopgtz or loopnez\n");
    return FALSE;
  }

  if (!CGPREP_guard_branch_unsigned && (loop_top == TOP_loopnez))
  {
    if (trace)
      fprintf(TFile, "<zcl> CGPREP_guard_branch_unsigned prevents"
	      " guard removal with loopnez\n");
    return FALSE;
  }

  INT trip_count_op_idx = TI_TOP_Find_Operand_Use(loop_top, OU_trip_count);
  FmtAssert(trip_count_op_idx != -1,("Missing trip count operand"));
  TN* trip_count_tn = OP_opnd(loop_op, trip_count_op_idx);

  if (trace)
  {
    fprintf(TFile, "<zcl> trip_count_tn = ");
    Print_TN(trip_count_tn, FALSE);
    fprintf(TFile, "\n");
  }

  BB *guard_bb = BB_Unique_Predecessor(prolog);
  BB *guard_bb_succ = prolog;
  OP *guard_br = NULL;
  while (TRUE)
  {
    if (!guard_bb)
    {
      if (trace)
	fprintf(TFile, "<zcl> prolog has multiple predecessors\n");
      return FALSE;
    }

    guard_br = BB_xfer_op(guard_bb);
    if (guard_br &&
	(OP_call(guard_br) || OP_ijump(guard_br) || OP_cond(guard_br)))
      break;

    guard_bb_succ = guard_bb;
    guard_bb = BB_Unique_Predecessor(guard_bb);
  }

  if (!guard_br || !OP_cond(guard_br))
  {
    if (trace)
      fprintf(TFile, "<zcl> no guarding conditional branch\n");
    return FALSE;
  }

  /* Check to see if the guard branches to the loop epilog. */
  BB *guard_bb_other_succ = BB_Other_Successor(guard_bb, guard_bb_succ);
  BB *epilog = BB_Other_Successor(prolog, BB_next(prolog));
  BB *non_empty_epilog = Find_nonempty_succ(epilog);
  BB *target_bb = Find_nonempty_succ(guard_bb_other_succ);
  UINT epilog_op_cnt = 0;
  if (non_empty_epilog != target_bb) 
  {
    BB *bb;
    BB *next_bb = non_empty_epilog;

    // Used to prevent infinite loop. 
    BBLIST *bbs_visited = NULL;

    do {
      bb = next_bb;

      // Keep trace of BBs that have been visited
      BBlist_Add_BB (&bbs_visited, bb); 

      OP *op;
      FOR_ALL_BB_OPs(bb,op) {
	if (!CGTARG_Can_Be_Speculative(op))
	{
	  if (trace)
	  {
	    fprintf(TFile, "<zcl> unable to speculate epilog op");
	    Print_OP_No_SrcLine(op);
	  }

          BBlist_Free (&bbs_visited);
	  return FALSE;
	}

	if (!OP_copy(op))
	  epilog_op_cnt++;
      }
      next_bb = BB_Unique_Successor(bb);

      // Prevent infinite loop
      if ((next_bb != NULL) && 
          BBlist_Find_BB(bbs_visited, next_bb)) {
        next_bb = NULL;
      }
    } while (next_bb!=target_bb && next_bb!=NULL);

    if (next_bb) {
      bb = next_bb;
    }

    BBlist_Free (&bbs_visited);

    if (bb!=target_bb) {
      if (trace)
        fprintf(TFile, "<zcl> guard branch does not jump to loop epilog\n");
      return FALSE;
    }
  }

  /*
     Need to check if GTNs that are modifiled in prolog or epilog and 
     are live out into the common target of the zcl and the guard, 
     do not change their values, i.e. prolog and epilog do not change
     those GTNs' values. Particularly, the following cases are recognized:

           if (c) { // guard
              x = GTN200
              ...
              <LOOP>
       
              GTN200 = x   
              ..
           }
           <GTN200 is alive> 

         while GTN200 is modified in epilog. But GTN200's value isn't
         changed by epilog/prolog. So, the guard above may still be
         redundant.

     all_tns_map are used to keep track of all GTNs that are modifiled
     in epilog or prolog and live out into the target.  Only copy OPs are
     considered, that is, if a GTN is modified by non-copy OPs, we assume
     this GTN's value may be changed by epilog or prolog.
   */
  MEM_POOL_Push(&MEM_local_nz_pool);
  hTN_MAP all_tns_map = hTN_MAP_Create(&MEM_local_nz_pool);
  BB *bb0;
  BB *next_bb0 = guard_bb_succ;
  void *unknown_tn = (void *)1;
  do {
    bb0 = next_bb0;

    OP *op_end = (bb0 == prolog) ? loop_op : NULL;
    OP *op;
    for (op = BB_first_op(bb0); op != op_end; op = OP_next(op))
    {
      int nres = OP_results(op);
      if ( (nres == 1) && OP_copy(op) ) {
        TN *res = OP_result(op, 0);
        TN *opnd_tn = OP_opnd(op, TI_ISA_Copy_Operand(OP_code(op), TRUE));
        void *val = hTN_MAP_Get(all_tns_map, opnd_tn);
        if (val == NULL) {
          hTN_MAP_Set(all_tns_map, res, opnd_tn);
        } else {
          hTN_MAP_Set(all_tns_map, res, val);
        }
      } else {
        for (INT i = 0; i < nres; i++) {
          TN *res = OP_result(op, i);
          hTN_MAP_Set (all_tns_map, res, unknown_tn);
        }
      }
    }

    next_bb0 = BB_Unique_Successor(bb0);
    FmtAssert((next_bb0 || bb0 == prolog), ("next_bb0 must not be NULL"));
  } while (bb0 != prolog);

  next_bb0 = non_empty_epilog;
  while (next_bb0 != target_bb)
  {
    bb0 = next_bb0;

    OP *op;
    for (op = BB_first_op(bb0); op != NULL; op = OP_next(op))
    {
      int nres = OP_results(op);
      if ( (nres == 1) && OP_copy(op)) {
        TN *res = OP_result(op, 0);
        TN *opnd_tn = OP_opnd(op, TI_ISA_Copy_Operand(OP_code(op), TRUE));
        void *val = hTN_MAP_Get(all_tns_map, opnd_tn);
        if (val == NULL) {
          hTN_MAP_Set(all_tns_map, res, opnd_tn);
        } else {
          hTN_MAP_Set(all_tns_map, res, val);
        }
      } else {
        for (INT i = 0; i < nres; i++) {
          TN *res = OP_result(op, i);
          hTN_MAP_Set (all_tns_map, res, unknown_tn);
        }
      }
    }

    next_bb0 = BB_Unique_Successor(bb0);
    FmtAssert(next_bb0, ("next_bb0 must not be NULL"));
  };
    
  hTN_MAP_ITERATOR iter = htn_map_iterator_create(all_tns_map);
  while (!is_htn_map_iterator_empty(iter)) {
    TN *lhs_tn = get_htn_map_iterator_tn(iter);
    if ( TN_is_register(lhs_tn) && 
         !TN_is_const_reg(lhs_tn) &&
         TN_is_global_reg(lhs_tn) && 
         (GTN_SET_MemberP(BB_live_in(target_bb), lhs_tn) ||  
          TN_is_dedicated(lhs_tn)) )
    {
      TN *rhs_tn = (TN *)get_htn_map_iterator_value(iter);
      if (lhs_tn != rhs_tn) {
        if (trace) {
          fprintf(TFile, "<zcl> value of GTN%d changed in epilog or prolog");
        }
        return FALSE;
      }
    }
    htn_map_iterator_next(iter);
  }
  MEM_POOL_Pop(&MEM_local_nz_pool);


  /* Examine OPs in BBs between 'guard_bb' and 'prolog' in reverse
     order, tracking how 'trip_count_tn' is computed and checking to
     make sure all OPs can be speculated. */

  OP *trip_tn_def = NULL;
  TN *opnd0 = NULL, *opnd1 = NULL;
  UINT prolog_op_cnt = 1; // init to 1 for loop op
  
  for (BB *bb = prolog; bb != guard_bb; bb = BB_Unique_Predecessor(bb))
  {
    OP *op = ((bb == prolog) ? OP_prev(loop_op) : BB_last_op(bb));
    for (; op; op = OP_prev(op))
    {
      if (trace)
      {
	fprintf(TFile, "<zcl> examining ");
	Print_OP_No_SrcLine(op);
      }

      if (!CGTARG_Can_Be_Speculative(op))
      {
	if (trace)
	{
	  fprintf(TFile, "<zcl> unable to speculate prolog op");
	  Print_OP_No_SrcLine(op);
	}
      
	return FALSE;
      }

      /* Count non-copy OPs. We optimistically assume copies will be
         eliminated by ebo or regalloc. */

      if (!OP_copy(op))
	prolog_op_cnt++;
      
      /* If 'trip_count_tn' is defined in 'op', determine if it is a
         type of definition that we understand. If not, we can't
         eliminate the guard. */

      if (OP_Defs_TN(op, trip_count_tn))
      {
	/* Can't handle 'trip_count_tn' being defined in more than one
           OP. */

	if (trip_tn_def)
	{
	  if (trace)
	  {
	    fprintf(TFile, "<zcl> second trip count def ");
	    Print_OP_No_SrcLine(op);
	  }
	  
	  return FALSE;
	}

	trip_tn_def = op;
	
	switch (OP_code(op))
	{
	case TOP_mov_n:
	  /* Change 'trip_count_tn' to be the source TN, and continue
             looking for its definition. */
	  trip_count_tn = OP_opnd(op, 0);
	  trip_tn_def = NULL;
	  break;
	  
	case TOP_sub:
	case TOP_addi:
	  opnd0 = OP_opnd(op, 0);
	  opnd1 = OP_opnd(op, 1);
	  break;

	default:
	  if (trace)
	  {
	    fprintf(TFile, "<zcl> can't handle trip count def ");
	    Print_OP_No_SrcLine(op);
	  }
	  
	  return FALSE;
	}
      }
    }
  }

  /* Compare the OP defining 'trip_count_tn' (which could be NULL is
     there is no OP in the prolog defining it), with 'guard_br' to see
     if they allow 'guard_br' to be removed. */

  BOOL fail = TRUE;
  
  if (trace)
  {
    fprintf(TFile, "<zcl> checking guard branch ");
    Print_OP_No_SrcLine(guard_br);
    fprintf(TFile, "<zcl>    with trip_count_tn def op ");
    if (trip_tn_def)
      Print_OP_No_SrcLine(trip_tn_def);
    else
      fprintf(TFile, "<null>\n");
  }

  /* For the following tests, we assume 'guard_br' falls-through to
     the prolog. If it doesn't, use the inverted topcode. */

  TOP guard_br_top = OP_code(guard_br);
  if (BB_next(guard_bb) != guard_bb_succ)
    guard_br_top = TI_TOP_Invert_Branch(guard_br_top);
  if (guard_br_top == TOP_bltz) 
    guard_br_top = TOP_blti;

  TN *br_opnd0 = OP_opnd(guard_br, 0);
  TN *br_opnd1 = ((OP_opnds(guard_br) == 2) ? NULL : OP_opnd(guard_br, 1));
  INT64 br_imm = ((br_opnd1 && TN_has_value(br_opnd1)) ? TN_value(br_opnd1) : 0);

  if (trip_tn_def == NULL)
  {
    /* If 'trip_count_tn' is not defined in the prolog, then
       'guard_br' is redundant if it is taken when 'trip_count_tn' <=
       0. */

    if ((br_opnd0 == trip_count_tn) &&
	((guard_br_top == TOP_beqz) ||
	 (guard_br_top == TOP_beqzt) ||
	 ((guard_br_top == TOP_blti) && (br_imm <= 1)) ||
	 ((guard_br_top == TOP_bltui) && (br_imm <= 1)) ||
	 (guard_br_top == TOP_bltz)))
      fail = FALSE;
  }
  else
  {
    /* 'trip_count_tn' is defined in prolog... */
    
    switch (OP_code(trip_tn_def))
    {
    case TOP_sub:
      /* 'trip_count_tn' is 'opnd0' - 'opnd1'. 'guard_br' is redundant
         if it is taken when 'opnd1' >= 'opnd0'. */
      
      // Subtract may mess up on overflow conditions
      if (CGPREP_guard_branch_aggressive && 
	  (br_opnd0 == opnd1) && (br_opnd1 == opnd0) &&
	  ((guard_br_top == TOP_bge) || (guard_br_top == TOP_bgeu)))
	fail = FALSE;
      break;
      
    case TOP_addi:
      /* 'trip_count_tn' is 'opnd0' + 'opnd1', where 'opnd1' is an
         immediate. 'guard_br' is redundant if it is taken when
         'opnd0' <= -'opnd1'. */
      {
	if (!opnd1 || !TN_has_value(opnd1))
	  break;

	INT64 imm = TN_value(opnd1);
	if ((br_opnd0 == opnd0) && (br_imm <= (-imm + 1)) &&
	    (CGPREP_guard_branch_aggressive || (imm >= 0)) && 
	    ((guard_br_top == TOP_blti) || (guard_br_top == TOP_bltui)))
	  fail = FALSE;
      }
      break;
      
    default:
      FmtAssert(FALSE, ("unexpected trip_count def OP"));
      break;
    }
  }

  if (fail)
  {
    if (trace)
    {
      fprintf(TFile, "<zcl> unable to remove loop guard branch ");
      Print_OP_No_SrcLine(guard_br);
    }
    return FALSE;
  }

  /* We can remove 'guard_br', but we don't want to if it is not
     profitable:

     1. If OPT_Space is true, always remove it.

     2. If OPT_Space is false, use profile information to determine if
     the extra cycles needed to always execute the prolog instructions
     are outweighed by the benefit of not having the branch. We bias
     towards removing the guard branch by multiplying the savings by a
     small factor.  */

  if (!OPT_Space)
  {
    float extra = (BB_freq(guard_bb) - BB_freq(prolog)) *
	    			(prolog_op_cnt+epilog_op_cnt);
    float savings = BB_freq(guard_bb) * 1.05;
    if (savings < extra)
    {
      if (trace)
	fprintf(TFile, "<zcl> not beneficial, cost = %.3f, savings = %.3f\n",
		extra, savings);
      return FALSE;
    }
  }
  
  /* We've satisfied all the conditions, so remove 'guard_br'. */

  if (trace)
  {
    fprintf(TFile, "<zcl> removing redundant loop guard branch ");
    Print_OP_No_SrcLine(guard_br);
  }
  
  Unlink_Pred_Succ(guard_bb, BB_Other_Successor(guard_bb, guard_bb_succ));
  Change_Succ_Prob(guard_bb, guard_bb_succ, 1.0);
  BB_Remove_Op(guard_bb, guard_br);

  /* If 'loop_op' is loopnez, then we must change it to loopgtz, since
     we have removed the guard test that would detect when the trip
     count is negative. Note that this causes loops with very large
     unsigned trip counts ( > 2G) to not iterate at all since the trip
     count will appear negative. But a loop iterating > 2G times is
     unlikely anyway. Use CGPREP_guard_branch_unsigned == FALSE to
     disable. */

  if ((loop_top == TOP_loopnez) && (guard_br_top != TOP_beqz) && 
      (guard_br_top != TOP_beqzt))
  {
    if (trace)
      fprintf(TFile, "<zcl> changing loopnez to loopgtz\n");

    OP_Change_Opcode(loop_op, TOP_loopgtz);
  }
  
  return TRUE;
}

/*
   To apply ZCL conversion, a loop needs to have a prolog and a tail
   BB. The tail BB should be the source of a backedge and ends with a 
   conditional branch that will fall-through out of the loop.

   Select_Zcl_Tail_BB() determines whether cg_loop can be zcl'ed.
   If so, it returns the tail BB whose fall-thru BB is to used to
   insert a loopend.  It returns NULL if the loop cannot be zcl'ed.
 */
static BB *Select_Zcl_Tail_BB (CG_LOOP cg_loop, BOOL check, BOOL trace)
{
  LOOP_DESCR *loop = cg_loop.Loop();

  /*
     The loop must have a prolog (where we can place the loop
     instruction). The loop should have at least one tail ending with 
     a conditional branch.
   */

  if (!cg_loop.Has_prolog()) {
    if (trace)
      fprintf(TFile, "<zcl> no prolog\n");

    return NULL;
  }

  // Select a tail BB. 
  //  If more than one, use one whose backedge to the loophead
  //  has the highest frequency among them.
  BB    *tail_bb = NULL;
  float tail_bb_freq = 0.0f;
  BBLIST *preds; 
  FOR_ALL_BB_PREDS(LOOP_DESCR_loophead(loop), preds) {
    BB *pred_bb = BBLIST_item(preds);
    OP *br_op = BB_branch_op(pred_bb);
    if ( BB_SET_MemberP(LOOP_DESCR_bbset(loop), pred_bb) &&
         ((br_op != NULL) && OP_cond(br_op)) ) {
      /*
         Check if all pred_bb's succ are in the loop, if so
         pred_bb is not considered as a tail.
       */
      BOOL is_tail = FALSE;
      BBLIST *succs;
      float cur_freq=0;
      FOR_ALL_BB_SUCCS(pred_bb, succs) {
        BB *succ_bb = BBLIST_item(succs);
        if (succ_bb == LOOP_DESCR_loophead(loop)) {
           cur_freq = BB_freq(pred_bb) * BBLIST_prob(succs);
        } else if (!BB_SET_MemberP(LOOP_DESCR_bbset(loop),  succ_bb)) {
          is_tail = TRUE;
        }       
      }

      if (is_tail) {
        if (tail_bb == NULL) {
          tail_bb = pred_bb;
          tail_bb_freq = cur_freq;
        } else {
          if (cur_freq > tail_bb_freq) {
            tail_bb = pred_bb;
            tail_bb_freq = cur_freq;
          }
        }
      }
    }
  } // FOR_ALL_BB_PREDS

  if (tail_bb == NULL) {
    if (trace) {
      fprintf(TFile, "<zcl> cannot find a tail BB\n");
    }
    return NULL;
  }

  if (cg_loop.Epilog_start() == NULL) {
    // Need to create a epilog for adding loopend
    /* BB_next(tail_bb) is the fall-thru BB, ie the exit of the loop */
    BB *next_bb = BB_next(tail_bb);
    if (BB_Unique_Predecessor(next_bb) == NULL) {
      // Need to create a epilog 
      BB *epilog = Gen_And_Insert_BB_Before(next_bb);
      if (trace ) {
        fprintf(TFile, "<zcl> create epilog (BB%d) for ZCL conversion\n", BB_id(epilog));
      }
      BB_rid(epilog) = BB_rid(LOOP_DESCR_loophead(loop));
      if (BB_freq_fb_based(LOOP_DESCR_loophead(loop))) {
        Set_BB_freq_fb_based(epilog);
      }
      retarget_loop_exits(loop, next_bb, epilog);
      Link_Pred_Succ_with_Prob(epilog, next_bb, 1.0);
      if (FREQ_Frequencies_Computed() || BB_freq_fb_based(tail_bb)) {
        BBLIST *bbl;
        BB_freq(epilog) = 0.0F;
        FOR_ALL_BB_PREDS (epilog, bbl) {
          BB *pred = BBLIST_item(bbl);
          BBLIST *tmp = BBlist_Find_BB(BB_succs(pred), epilog);
          BB_freq(epilog) += BB_freq(pred) * BBLIST_prob(tmp);
        }
      }
      GRA_LIVE_Compute_Liveness_For_BB(epilog);

      LOOP_DESCR *enclosing = LOOP_DESCR_Next_Enclosing_Loop(loop);
      if (enclosing && all_bbs_in(BB_preds(epilog), LOOP_DESCR_bbset(enclosing))) {
        LOOP_DESCR_Add_BB(enclosing, epilog);
      }
    } else {
      if (trace ) {
        fprintf(TFile, "<zcl> use the existing BB%d as epilog for ZCL convertion\n", 
                       BB_id(next_bb));
      }
    }
  } else if (BB_next(tail_bb) != cg_loop.Epilog_start())
  {
    if (trace)
      fprintf(TFile, "<zcl> cannot find loop tail block\n");

    return NULL;
  }

  return tail_bb;
}

/* Attempt to convert 'loop' to use a zero-cost loop. Return true if
   we succeed, false otherwise. If 'check' is true, we only check to
   see if we can convert the loop, and return true if we can. */
static BOOL
Zcl_Loop (CG_LOOP cg_loop, BOOL check)
{
  OPS tail_ops = OPS_EMPTY, prolog_ops = OPS_EMPTY;
  LOOP_DESCR *loop = cg_loop.Loop();
  LOOPINFO *info = LOOP_DESCR_loopinfo(loop);
  BB *prolog, *epilog;
  BOOL remove_guard = FALSE;
  BOOL trace = Get_Trace(TP_CGPREP, 2);

  /* Don't use a zcl if the estimated number of iterations is 1. */

  if (info && LOOPINFO_wn(info) && WN_loop_trip_est(LOOPINFO_wn(info)) <= 1)
  {
    if (trace)
      fprintf(TFile, "<zcl> estimated number of iterations is <= 1\n");

    return FALSE;
  }

  /* If a loop is marked as infrequent loop AND is an 
     inner loop,  don't do zcl (so that an outer loop
     has chance to do zcl).
   */
  if ( Is_Frequency_Never_Loop(loop) &&
       (LOOP_DESCR_nestlevel(loop) > 1) ) {
    if (trace)
      fprintf(TFile, "<zcl> the loop under #pragma frequency_hint NEVER\n");

    return FALSE;
  }

  /* Can't use a zcl if there is a call in the loop, or if an enclosed
     loop is already using it. */

  /* Q: Why not just use LOOP_DESCR::Is_Call_Loop? A: Because it
     doesn't seem to get initialized except in gcm. */

  {
    BB_SET *bbs = LOOP_DESCR_bbset(loop);
    BB *bb;
    FOR_ALL_BB_SET_members(bbs, bb)
    {
      if (BB_call(bb))
      {
	if (trace)
	  fprintf(TFile, "<zcl> contains call\n");

	return FALSE;
      }
    }
  }

  if (Is_Zcl_Enclosing_Loop(loop))
  {
    if (trace)
      fprintf(TFile, "<zcl> zcl used on inner loop\n");

    return FALSE;
  }

  /* Don't use a zcl if loop is too large, as specified by
     CG_LOOP_max_zcl_bytes. */
  
  {
    BB *bb;
    UINT loop_size = 0;
    FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
      loop_size += BB_size(bb);
    
    if (loop_size > CG_LOOP_max_zcl_bytes)
    {
      if (trace)
	fprintf(TFile, "<zcl> loop too big, size = %d, max allowed = %d\n",
		loop_size, CG_LOOP_max_zcl_bytes);
      
      return FALSE;
    }

    if (trace)
      fprintf(TFile, "<zcl> loop size = %d, max allowed = %d\n",
	      loop_size, CG_LOOP_max_zcl_bytes);
  }

  BB *tail_bb = Select_Zcl_Tail_BB (cg_loop, check, trace);
  if (tail_bb == NULL) 
  {
    if (trace) {
      fprintf(TFile, "<zcl> loop (loophead: BB%d) cannot be zcl\n",
                     BB_id(LOOP_DESCR_loophead(loop)));
    }
    return FALSE;
  }

  prolog = cg_loop.Prolog_end();
  epilog = BB_next(tail_bb);
  OP *br_op = BB_branch_op(tail_bb);
  if ((br_op == NULL) || !OP_cond(br_op)) {
    // Select_Zcl_Tail_BB() has checked this already !
    if (trace)
      fprintf(TFile, "<zcl> loop exit block does not end in conditional branch\n");

    return FALSE;
  }
     
  /* Check to see if prolog precedes epilog, if not, don't do zcl */
  BB *tmp_bb=prolog;
  while ( (tmp_bb != NULL) && (tmp_bb != epilog) ) {
        tmp_bb = BB_next(tmp_bb);
  }
  if (tmp_bb == NULL) {
    if (trace) {
      fprintf(TFile, "<zcl> loop's prolog does not precede its epilog\n");
    }
    return FALSE;
  }
   
  /* There are two possible zcl transformations. One requires a trip
     count and the other doesn't. We use the trip count tn that is
     valid even if the loop has early exits. There are some trip count
     expression vulnerable to overflow in common cases, for these we
     assume we don't know the trip count.

     case 1: if this loop was produced for a do-while loop, there is
     no entry guard test, and so overflows in the trip-count
     calculation can cause incorrect trip counts.
     
  */
  
  TN *trip_count_tn = CG_LOOP_Max_Trip_Count(loop);
  if (info)
  {
    WN *wn_loopinfo = LOOPINFO_wn(info);
    if (wn_loopinfo)
    {
      /* case 1 */
      if (WN_Loop_Dowhile(wn_loopinfo))
	trip_count_tn = NULL;
    }
  }
  
  if (trip_count_tn != NULL)
  {
    /* If we are just checking and we reach this point, then we can
       return that a trip-counted zcl conversion is possible. */

    if (check)
    {
      if (trace)
	fprintf(TFile, "<zcl> loop can be converted to use trip-counted zcl\n");
      
      return TRUE;
    }

    /* Convert the loop to use zcl. Since we have a trip-count tn, we
       just use that as the count for the loop instruction. */
    
    if (trace)
      fprintf(TFile, "<zcl> converting loop to use trip-counted zcl\n");

    /* The zero-cost loop is represented with instructions in the
       prolog and body of the loop. */
  
    TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));

    CGTARG_Generate_Branch_Cloop(prolog, tail_bb, trip_count_tn, trip_count_tn,
				 0, label_tn, &prolog_ops, &tail_ops);
    remove_guard = TRUE;
  }
  else
  {
    /* We don't have a trip count tn. We can still use a zcl by
       applying this transformation. We use $a0 since it is likely to
       have a large value.

       L:                       Lstart:
          <body>                        loop $a0, Lloop
	  bcond  L                   L:
       Lend:                            <body>
                                        !bcond Lend
					loop_end L
				 Lloop:
				        br Lstart
				  Lend:
    */

    /* We must be able to invert 'br_op'... */

    if (CGTARG_Invert_Branch(OP_code(br_op)) == TOP_UNDEFINED)
    {
      if (trace)
	fprintf(TFile, "<zcl> can't negate loop branch for non-trip-counted zcl\n");
      
      return FALSE;
    }
    
    /* Don't perform this transformation if OPT_Space is on */
    if (OPT_Space) {
      if (trace)
	fprintf(TFile, "<zcl> OPT_space disable non-trip-counted zcl\n");
      
      return FALSE;
    }

    /* Do not perform this transformation if the estimated trip count
     * is less than 1
     */
    BBLIST* fall_through_edge = BBlist_Fall_Thru_Succ(tail_bb);
    float fall_through_prob = BBLIST_prob(fall_through_edge);
    float branch_back_prob = 1.0f - fall_through_prob;
    float branch_back_trip_est = BB_freq(tail_bb) * branch_back_prob;

    if (branch_back_trip_est <= 1.0f) {
      if (trace)
	fprintf(TFile,
		"<zcl> Disable non-trip-counted zcl for low (<=1) trip count\n");
      
      return FALSE;
    }

    /* If we are just checking and we reach this point, then we can
       return that a trip-counted zcl conversion is possible. */

    if (check)
    {
      if (trace)
	fprintf(TFile, "<zcl> loop can be converted to use non-trip-counted zcl\n");
      
      return TRUE;
    }

    /* Convert the loop to use zcl. */
    
    if (trace)
      fprintf(TFile, "<zcl> converting loop to use non-trip-counted zcl\n");

    /* If the last block in the prolog is not empty, then append a new
       block to the end of the prolog to hold the loop instruction. We
       can't just add the loop instruction to the end of the current
       prolog block since it requires a label. */
    
    LOOP_DESCR *enclosing = LOOP_DESCR_Next_Enclosing_Loop(loop);
    if (BB_first_op(prolog) != NULL)
    {
      prolog = Gen_BB_Like(prolog);
      append_to_prolog(prolog);
      if (enclosing && all_bbs_in(BB_preds(prolog), LOOP_DESCR_bbset(enclosing))) {
        LOOP_DESCR_Add_BB(enclosing, prolog);
      }
    }
  
    TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));
    LABEL_IDX loop_label = Gen_Label_For_BB(prolog);
    TN *loop_label_tn = Gen_Label_TN(loop_label,0);
      
    /* Create a new block to hold the loop_end pseudo instruction. We
       copy 'br_op' into this new block so that when it is converted
       to the loop_end by CGTARG_Generate_Branch_Cloop it will have
       the correct target. 'tail_bb' falls through to this new
       block. Also, create the safety block to restart the loop if the
       lcount reaches zero. */

    BB *loop_end_bb = Gen_BB_Like(tail_bb);
    OP *loop_end_br = Dup_OP(br_op);
    BB_Insert_Op(loop_end_bb, NULL, loop_end_br, FALSE);
    GRA_LIVE_Compute_Local_Info(loop_end_bb);

    BB *safety_bb = Gen_BB_Like(loop_end_bb);
    OP *safety_br = Mk_OP(TOP_j, loop_label_tn);
    BB_Insert_Op(safety_bb, NULL, safety_br, FALSE);
    GRA_LIVE_Compute_Local_Info(safety_bb);

    Chain_BBs(tail_bb, loop_end_bb);
    Chain_BBs(loop_end_bb, safety_bb);
    Chain_BBs(safety_bb, epilog);

    Change_Succ(tail_bb, epilog, loop_end_bb);
    Link_Pred_Succ(loop_end_bb, safety_bb);
    Link_Pred_Succ(loop_end_bb, LOOP_DESCR_loophead(loop));
    Link_Pred_Succ(safety_bb, prolog);

    // loop_end_bb is in the current loop && enclosing loops
    LOOP_DESCR_Add_BB(loop, loop_end_bb);

    // safety_bb is not in the current loop, but it
    // may be in the enclosing loop.
    if (enclosing && all_bbs_in(BB_preds(safety_bb), LOOP_DESCR_bbset(enclosing))) {
      LOOP_DESCR_Add_BB(enclosing, safety_bb);
    }

    if (FREQ_Frequencies_Computed())
    {
      BBLIST *s0 = BB_succs(tail_bb);
      BBLIST *s1 = BBLIST_next(s0);
      float s0_prob = BBLIST_prob(s0);
      float s1_prob = BBLIST_prob(s1);
      BBLIST_prob(s0) = s1_prob;
      BBLIST_prob(s1) = s0_prob;

      BB_freq(loop_end_bb) = BB_freq(tail_bb) * s1_prob;
      s0 = BB_succs(loop_end_bb);
      s1 = BBLIST_next(s0);
      BBLIST_prob(s0) = 0.0;
      BBLIST_prob(s1) = 1.0;

      s0 = BB_succs(safety_bb);
      BBLIST_prob(s0) = 1.0;
    }

    /* Flip the sense of 'br_op' and show that it now targets 'br_op's
       original fall-through block (which we checked was
       'epilog' above). We do this ourselves instead of using
       BB_Retarget_Branch, to avoid mucking with the probabilities. */

    {
      if (!Negate_Branch(br_op))
	Is_True(FALSE, ("unable to negate branch %s!", TI_TOP_Name(OP_code(br_op))));

      LABEL_IDX epilog_label = Gen_Label_For_BB(epilog);
      TN *epilog_label_tn = Gen_Label_TN(epilog_label,0);
      INT opnd, opnd_count;

      CGTARG_Branch_Info(br_op, &opnd, &opnd_count);
      Is_True(opnd_count == 1, ("Conditional branch with multiple targets"));
      Set_OP_opnd(br_op, opnd, epilog_label_tn);

      BBLIST *tail_succs = BB_Find_Succ(tail_bb, LOOP_DESCR_loophead(loop));
      BBLIST_item(tail_succs) = epilog;
      BBlist_Delete_BB(&BB_preds(LOOP_DESCR_loophead(loop)), tail_bb);
      BBlist_Add_BB(&BB_preds(epilog), tail_bb);
      BB_freq(epilog) = BB_freq(tail_bb) * BBLIST_prob(tail_succs);
    }
    
    /* The zero-cost loop is represented with instructions in the
       prolog and body of the loop. */

    /* The windowed abi doesn't have a return address register in the
       way that the xcc abi description uses them. So we don't have an
       easy abi call to get it. */

    trip_count_tn = Build_Dedicated_TN(ISA_REGCLASS_integer, 1, 4);
    CGTARG_Generate_Branch_Cloop(prolog, loop_end_bb, trip_count_tn, trip_count_tn,
				 0, label_tn, &prolog_ops, &tail_ops);

    tail_bb = loop_end_bb;
    br_op = loop_end_br;
  }

  /* Attach new instruction to prolog and replace loop tail branch. */
  BB_Append_Ops(prolog, &prolog_ops);
  
  if (OPS_length(&tail_ops) > 0)
  {
    BB_Remove_Op(tail_bb, br_op);
    BB_Append_Ops(tail_bb, &tail_ops);
  }

  /* Mark enclosing loops so they won't use a zcl. We don't use
     LOOP_DESCR_Next_Enclosing_Loop since it checks to make sure the
     potentially enclosing loop contains all the blocks of this
     loop. Since we don't correctly maintain the blocks in a loop
     during the loop optimizations, we do a more conservative walk up
     the containing loops. */

  if (LOOP_DESCR_nestlevel(loop) > 1)
  {
    INT nestlevel = LOOP_DESCR_nestlevel(loop)-1;
    LOOP_DESCR *enclosing = LOOP_DESCR_next(loop);
    while (enclosing && nestlevel>0)
    {
      if (LOOP_DESCR_nestlevel(enclosing)==nestlevel) {
	Set_Zcl_Enclosing_Loop(enclosing);
	nestlevel--;
      }
      enclosing = LOOP_DESCR_next(enclosing);
    }
  }

  /* Try to eliminate a guard branch, it there is one. */

  if (remove_guard && CGPREP_remove_guard_branch)
    Remove_Guard_Branch(prolog);

  return TRUE;
}


// If loop has a constant trip count and a unique tail ending
// with a conditional branch whose offset range is smaller than
// the loop size, try to replace the branch with one that has
// larger offset range in order to avoid branch relaxation.
//
static BOOL
Remove_Back_Edge_Relaxation(CG_LOOP cg_loop)
{
  // must have prolog and epilog
  if (!cg_loop.Has_prolog_epilog())
    return FALSE;

  // must have constant trip count
  TN *trip_count_tn = cg_loop.Trip_count_tn();
  if (!trip_count_tn || !TN_is_constant(trip_count_tn))
    return FALSE;

  // must have unique tail
  LOOP_DESCR *loop_descr = cg_loop.Loop();
  BB *tail_bb = LOOP_DESCR_Find_Unique_Tail(loop_descr);
  if (!tail_bb || BB_next(tail_bb) != cg_loop.Epilog_start())
    return FALSE;

  // must end with a conditional branch
  OP *branch_op = BB_branch_op(tail_bb);
  if (!branch_op || !OP_cond(branch_op))
    return FALSE;

  // check if the size exceeds the offset range of the loop-back branch
  // but it would fit in the simm12 offset range of TOP_bnez
  BB *bb;
  INT64 loop_size = 0;
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop_descr), bb)
    loop_size += BB_size(bb);

  UINT target_pos = Branch_Target_Operand(branch_op);
  TN *target_tn = OP_opnd(branch_op, target_pos);
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(OP_code(branch_op));
  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, target_pos);
  ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
  if (TI_ISA_LC_Value_In_Class(-loop_size, lit_class) ||
      !TI_ISA_LC_Value_In_Class(-loop_size, LC_simm12))
    return FALSE;

  // printf("Loop size is %" LLD_FMT ", try avoiding relaxation for\n", loop_size);
  // Print_OP_No_SrcLine(branch_op);

  // initialize backward counter with loop trip count
  BB *prolog_bb = cg_loop.Prolog_end();
  OPS prolog_ops = OPS_EMPTY;
  TN *back_count_tn = Gen_Typed_Register_TN(TN_mtype(trip_count_tn), 
                                            TN_size(trip_count_tn));
  Exp_COPY(back_count_tn, trip_count_tn, &prolog_ops);
  BB_Append_Ops(prolog_bb, &prolog_ops);

  // decrement backward counter and change the loop-back branch
  OPS tail_ops = OPS_EMPTY;
  TN *negone_tn = Gen_Literal_TN(-1,4);
  Build_OP(TOP_addi, back_count_tn, back_count_tn, negone_tn, &tail_ops);
  Build_OP(TOP_bnez, back_count_tn, target_tn, &tail_ops);
  BB_Remove_Op(tail_bb, branch_op);
  BB_Append_Ops(tail_bb, &tail_ops);

  return TRUE;
}

/*
 * Unroll constant trip count loop fully iff:
 *   (a) CG:unroll_fully not turned off, and either
 *   (b1) unrolled size <= OPT:unroll_size, or
 *   (b2) OPT:unroll_size=0 and OPT:unroll_times_max >= trip count
 */
bool CG_LOOP::Determine_Unroll_Fully()
{
  if (!CG_LOOP_unroll_fully)
    return false;
  
  if (OPT_Space) {
    if (trace)
      fprintf(TFile, "Optimize for space, set not unroll fully\n");
    return false;
  }

  LOOPINFO *info = LOOP_DESCR_loopinfo(Loop());
  TN *trip_count_tn = info ? LOOPINFO_trip_count_tn(info) : NULL;
  BB *head = LOOP_DESCR_loophead(loop);

  if (BB_Has_Exc_Label(head))
    return false;

  if (CG_LOOP_unroll_times_max < 2) 
    return false;

  if (trip_count_tn == NULL)  
    return false;

  if (!TN_is_constant(trip_count_tn))
    return false;

  /* pr8308. Don't unroll fully when analysing if loop has vector stats. */
  if (Run_Autotie && info && AUTOTIE_Suppress_Unroll_Fully(info))
    return false;
      
  INT32 const_trip_count = TN_value(trip_count_tn);
  INT32 body_len = BB_length(head);

  if (body_len <= CG_LOOP_unrolled_size_max &&
      const_trip_count <= CG_LOOP_unrolled_size_max &&
      body_len * const_trip_count <= CG_LOOP_unrolled_size_max ||
      CG_LOOP_unrolled_size_max == 0 &&
      CG_LOOP_unroll_times_max >= const_trip_count) {

    if (trace)
      fprintf(TFile, "<unroll> unrolling fully (%d times)\n", const_trip_count);

    Set_unroll_fully();
    Set_unroll_factor(const_trip_count);
    return true;
  }

  return false;
}


void CG_LOOP::Determine_Unroll_Factor()
{ 
  LOOPINFO *info = LOOP_DESCR_loopinfo(Loop());
  TN *trip_count_tn = info ? LOOPINFO_trip_count_tn(info) : NULL;
  BB *head = LOOP_DESCR_loophead(loop);

  Set_unroll_factor(1);

  if (BB_Has_Exc_Label(head)) {
    char *reason = "in exception region or handler";
    note_not_unrolled(head, reason);
    if (trace) fprintf(TFile, "<unroll> not unrolling; %s\n", reason);
    return;
  }

  if (CG_LOOP_unroll_times_max < 2) {
    char *reason = "-OPT:unroll_times_max=%d";
    note_not_unrolled(head, reason, CG_LOOP_unroll_times_max);
    if (trace) {
      fprintf(TFile, "<unroll> not unrolling; ");
      fprintf(TFile, reason, CG_LOOP_unroll_times_max);
      fprintf(TFile, "\n");
    }
    return;
  }

  if (trip_count_tn == NULL) {

    UINT32 ntimes = MAX(1, OPT_unroll_times-1);
    INT32 body_len = BB_length(head);
    while (ntimes > 1 && ntimes * body_len > CG_LOOP_unrolled_size_max)
      ntimes/=2;
    Set_unroll_factor(ntimes);

  } else {

    BOOL const_trip = TN_is_constant(trip_count_tn);
    INT32 const_trip_count = const_trip ? TN_value(trip_count_tn) : 0;
    INT32 body_len = BB_length(head);
  
    CG_LOOP_unroll_min_trip = MAX(CG_LOOP_unroll_min_trip, 1);
    if (const_trip && CG_LOOP_unroll_fully &&
	const_trip_count <= CG_LOOP_unrolled_size_max &&
	body_len <= CG_LOOP_unrolled_size_max &&
	/*
	 * Unroll constant trip count loop fully iff:
	 *   (a) CG:unroll_fully not turned off, and either
	 *   (b1) unrolled size <= OPT:unroll_size, or
	 *   (b2) OPT:unroll_size=0 and OPT:unroll_times_max >= trip count
	 */
	(body_len * const_trip_count <= CG_LOOP_unrolled_size_max ||
	 CG_LOOP_unrolled_size_max == 0 &&
	 CG_LOOP_unroll_times_max >= const_trip_count)) {

      if (trace)
	fprintf(TFile, "<unroll> unrolling fully (%d times)\n", const_trip_count);

      Set_unroll_fully();
      Set_unroll_factor(const_trip_count);
    } else {
      UINT32 ntimes = OPT_unroll_times;
      ntimes = MIN(ntimes, CG_LOOP_unroll_times_max);
      if (!is_power_of_two(ntimes)) {
	ntimes = 1 << int_log2(ntimes); 
	if (trace)
	  fprintf(TFile, "<unroll> rounding down to power of two = %d times\n", ntimes);
      }
      while (ntimes > 1 && ntimes * body_len > CG_LOOP_unrolled_size_max)
	ntimes /= 2;
      if (const_trip) {
	while (ntimes > 1 && const_trip_count < 2 * ntimes) 
	  ntimes /= 2;
      }
      Set_unroll_factor(ntimes);
    }
  }

#ifdef TARG_XTENSA
  /* If we are optimizing for space, and we didn't unroll fully, then
     don't unroll at all. */

  if (OPT_Space)
  {
    if (!Unroll_fully() || Unroll_factor()!=1) {
      Reset_unroll_fully();
      Set_unroll_factor(1);

      if (BB_Has_Exc_Label(head))
      {
        char *reason = "optimizing for space";
        note_not_unrolled(head, reason);
        if (trace)
	  fprintf(TFile, "<unroll> not unrolling; %s\n", reason);
      }
    }
  } else if (CG_LOOP_unroll_times_max == 2 && // default unroll times
	     (BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1)) {
    // a hack to work around the "work around" by the assembler that
    // nop is inserted for very small loops
    INT32 body_len = BB_length(head) - 2; // ignore addi and br
    if (body_len<3 && earliest_arch==Alameda_Xtensa) {
      int ntimes = body_len==1 ? 4 :
		   body_len==2 ? 2 : Unroll_factor();
      if (Unroll_factor()<ntimes)
	Set_unroll_factor(ntimes);

      BOOL const_trip = TN_is_constant(trip_count_tn);
      INT32 const_trip_count = const_trip ? TN_value(trip_count_tn) : 0;

      if (const_trip && const_trip_count<= Unroll_factor()) {
	Set_unroll_factor(const_trip_count);
	Set_unroll_fully();
      }

    }
  }
#endif
}


// Returns TRUE if OP is live
//   
static BOOL
CG_LOOP_OP_is_live(OP *op, TN_SET *live_set, bool keep_prefetch)
{
  if (OP_store(op))
    return TRUE;
  if (OP_has_implicit_interactions(op))
    return TRUE;
  if (keep_prefetch && OP_prefetch(op))
    return TRUE;
  if (OP_results(op) == 0)
    return TRUE;

  for (INT i = 0; i < OP_results(op); i++)
  {
    TN *res = OP_result(op, i);
    if (TN_is_register(res) && !TN_is_const_reg(res))
      if (TN_SET_MemberP(live_set, res) || TN_is_dedicated(res))
	return TRUE;
  }

  return FALSE;
}

//  Remove dead variables or mark them as LOH ops
//
void
Induction_Variables_Removal(BB_SET	*loop_bbset,
			    BB *epilog,
			    bool remove, 
			    bool keep_prefetch,
			    OP*  dead_loop_branch,
			    bool trace)
{
  CXX_MEM_POOL pool("Temp TN_SET", FALSE);
  TN_SET *tnset = TN_SET_Create_Empty(Last_TN + 1, pool());
  
  /* Initialize live TN set to be those live into the epilog
     (i.e. live on exit from the loop). */
  {
    TN *tn;
    FOR_ALL_GTN_SET_members(BB_live_in(epilog),tn)
      tnset = TN_SET_Union1D(tnset, tn, pool());

    if (trace)
    {
      fprintf(TFile, "<remove ind var> live-on-loop-exit: " );
      TN_SET_Print(tnset, TFile);
      fprintf(TFile, "\n");
    }
  }

  /* Find the ops that define a tn that is live out of the loop, or
     that is used by an op that defines a tn live out of the loop,
     ... For these ops, add the used tns to the live set. */
  BOOL changed = true;
  while (changed)
  {
    BB *bb;
    OP *op;
    
    changed = false;

    FOR_ALL_BB_SET_members(loop_bbset, bb)
    {  
      FOR_ALL_BB_OPs_REV(bb, op)
      {
	if (!remove)
	  Reset_OP_loh(op);
  
	if (CG_LOOP_OP_is_live(op, tnset, keep_prefetch) && op!=dead_loop_branch)
	{
	  BOOL changed_for_op = FALSE;
	
	  for (INT i = 0; i < OP_opnds(op); i++)
	  {
	    TN *opnd = OP_opnd(op, i);
	    if (TN_is_register(opnd) &&
		!TN_is_const_reg(opnd) &&
		!TN_SET_MemberP(tnset, opnd))
	    {
	      changed = changed_for_op = true;
	      tnset = TN_SET_Union1D(tnset, opnd, pool());
	    }
	  }

	  if (trace && changed_for_op)
	  {
	    fprintf(TFile, "<remove ind var> live: " );
	    Print_OP_No_SrcLine(op);
	    fprintf(TFile, "\t");
	    TN_SET_Print(tnset, TFile);
	    fprintf(TFile, "\n");
	  }
	}
      }
    }
  }
  
  /* Remove dead ops from loop bbs. */
  {
    BB *bb;
    OP *op;

    FOR_ALL_BB_SET_members(loop_bbset, bb)
    {
      if (!remove)
      {
	FOR_ALL_BB_OPs(bb, op)
	{
	  if (!CG_LOOP_OP_is_live(op, tnset, keep_prefetch) ||
	      op==dead_loop_branch || OP_pseudo(op))
	  {
	    Set_OP_loh(op);
	    if (trace) {
	      fprintf(TFile, "<remove ind var> mark loh OP: ");
	      Print_OP_No_SrcLine(op);
	    }
	  }
	}
      }
      else
      {
	OP *next_op;
	for (op = BB_first_op(bb); op != NULL;  op = next_op)
	{
	  next_op = OP_next(op);
	  if (!CG_LOOP_OP_is_live(op, tnset, keep_prefetch) ||
	      op==dead_loop_branch)
	  {
	    if (trace)
	    {
	      fprintf(TFile, "<remove ind var> delete OP: ");
	      Print_OP_No_SrcLine(op);
	    }
	    
	    for (INT i = 0; i < OP_results(op); i++)
	    {
	      TN *tn = OP_result(op, i);
	      if (TN_is_register(tn) && !TN_is_const_reg(tn))
	      {
		GTN_SET_Difference1D(BB_defreach_out(bb), tn);
		GTN_SET_Difference1D(BB_live_in(bb), tn);
	      }
	    }
	    
	    BB_Remove_Op(bb, op);
	  }
	}
      }
    }
  }
}


void CG_LOOP::Determine_SWP_Unroll_Factor()
{
  if (OPT_Space) {
    if (trace)
      fprintf(TFile, "Optimize for space, set unroll factor to 1\n");
    Set_unroll_factor(1);
    return;
  }

  MEM_POOL_Push(&MEM_local_nz_pool);

  Is_True(SWP_Options.Min_Unroll_Times <= SWP_Options.Max_Unroll_Times,
	  ("CG_LOOP:  -SWP:min_unroll_times > -SWP:max_unroll_times"));

  Is_True(!Unroll_fully(),
	  ("CG_LOOP:  loop will be fully unrolled."));

  BB *head =  Loop_header();

  INT loop_size = 0;
  OP *op;
  INT num_prefetches = 0;
  FOR_ALL_BB_OPs(head, op) {
    loop_size++;
    if (OP_prefetch(op)) 
      num_prefetches++;
  }

  OP *br_op = BB_branch_op(head);
  
  // Prefetches are considered LOH Ops
  Induction_Variables_Removal(LOOP_DESCR_bbset(this->Loop()),
		  	      CG_LOOP_epilog,
			      false/*mark loh ops*/,
			      false/*mark prefetch as loh*/,
			      br_op/*mark loop back branch as loh*/,
			      trace);

  CG_SCHED_EST *loop_se = CG_SCHED_EST_Create(head, &MEM_local_nz_pool, 
					      SCHED_EST_FOR_UNROLL |
					      SCHED_EST_IGNORE_LOH_OPS |
					      SCHED_EST_IGNORE_PREFETCH);

#ifdef TARG_IA64
  // Model the implicit/explicit prefetching
  for (INT i = 0; i < num_prefetches; i++) 
    CG_SCHED_EST_Add_Op_Resources(loop_se, 
				  SWP_Options.Implicit_Prefetch ? 
				  TOP_adds : TOP_lfetch);
#endif

  CG_SCHED_EST *additional_se = CG_SCHED_EST_Create(head, &MEM_local_nz_pool, 
						    SCHED_EST_FOR_UNROLL |
						    SCHED_EST_IGNORE_LOH_OPS |
						    SCHED_EST_IGNORE_BRANCH |
						    SCHED_EST_IGNORE_PREFETCH);

  INT min_unr = SWP_Options.Min_Unroll_Times;
  INT max_unr = SWP_Options.Max_Unroll_Times;
  INT min_recurrence = num_prefetches > 0 && SWP_Options.Implicit_Prefetch ?
    4 : 0;

  const bool trace = Get_Trace(TP_SWPIPE, 2);
  vector<double> swp_cycles(SWP_Options.Max_Unroll_Times+1, 0.0);
  INT i;
  for (i = min_unr; i <= max_unr; i++) {
    swp_cycles[i] = CG_SCHED_EST_Resource_Cycles(loop_se) * (1.0 / i);
    if (trace) {
      fprintf(TFile, "<ti resource count> %d: ", i);
      TI_RES_COUNT_Print(TFile, loop_se->res_count);
      fprintf(TFile, "\n");
    }
    CG_SCHED_EST_Append_Scheds(loop_se, additional_se);
  }

  if (trace)
    for (i = min_unr; i <= max_unr; i++) {
      fprintf(TFile, "<swp unroll resource>  swp_cycles[%d] = %g\n", i, swp_cycles[i]);
    }

  // take into account of the recurrence
  if (min_recurrence != 0) 
    for (i = min_unr; i <= max_unr; i++) {
      if (swp_cycles[i] < (double) min_recurrence / i)
	swp_cycles[i] = (double) min_recurrence / i;
    }

  if (trace)
    for (i = min_unr; i <= max_unr; i++) {
      fprintf(TFile, "<swp unroll resource + recur>  swp_cycles[%d] = %g\n", i, swp_cycles[i]);
    }
  
  INT unroll_times = SWP_Options.Min_Unroll_Times;
  INT loop_size_limit = MIN(SWP_OPS_LIMIT, CG_LOOP_unrolled_size_max);
  for (i = min_unr; i <= max_unr; i++) {
    if (i * loop_size < loop_size_limit) {
      if (swp_cycles[i] < (swp_cycles[unroll_times] * (1.0 - (i - unroll_times) * 0.01)))
	unroll_times = i;
    }
  }

  unroll_times = 2;	// FIXME

  DevWarn("FIXME: unroll_times fixed at 1");
  if (trace) 
    fprintf(TFile, "<swp unroll factor>  swp_cycles[%d] = %g\n", unroll_times, swp_cycles[unroll_times]);
  
  Set_unroll_factor(unroll_times);

  MEM_POOL_Pop(&MEM_local_nz_pool);
}


void CG_LOOP::EBO_Before_Unrolling()
{
  MEM_POOL_Push(&MEM_local_pool);

  {
    BB_REGION bb_region(&MEM_local_pool);
    BB *far_entry = farthest_unique_predecessor(Loop_header());
    bb_region.entries.push_back(far_entry);
    bb_region.exits.push_back(farthest_unique_successor(Epilog_start(), far_entry));
    bb_region.Set_has_omega();

    // verify that all OPs in the region has omega and other invariants ...
    bb_region.Verify();  
    BB_SET *bbs = BB_SET_Create(0, &MEM_local_pool);
    bbs = BB_REGION_to_BB_SET(bbs, bb_region, &MEM_local_pool);

    EBO_before_unrolling(bbs);
  }

  MEM_POOL_Pop(&MEM_local_pool);
}

void CG_LOOP::EBO_After_Unrolling()
{
  MEM_POOL_Push(&MEM_local_pool);
  {
    BB_REGION bb_region(&MEM_local_pool);
    BB *far_entry = farthest_unique_predecessor(Prolog_start());
    bb_region.entries.push_back(far_entry);
    BBLIST *succs;
    FOR_ALL_BB_SUCCS(Epilog_end(), succs) {
      BB *succ = BBLIST_item(succs);
      bb_region.exits.push_back(farthest_unique_successor(succ, far_entry));
    }
    BB_SET *bbs = BB_SET_Create(0, &MEM_local_pool);
    bbs = BB_REGION_to_BB_SET(bbs, bb_region, &MEM_local_pool);

    EBO_after_unrolling(bbs);
  }
  MEM_POOL_Pop(&MEM_local_pool);
}

void CG_LOOP::EBO_Before_Unrolling_For_SWP()
{
  MEM_POOL_Push(&MEM_local_pool);

  {
    BB* body = Loop_header();
    BB_SET *bbs = BB_SET_Create(0, &MEM_local_pool);
    bbs = BB_SET_Union1D(bbs, body, &MEM_local_pool);

    EBO_before_unrolling(bbs);
  }

  MEM_POOL_Pop(&MEM_local_pool);
}

void CG_LOOP::EBO_After_Unrolling_For_SWP()
{
  MEM_POOL_Push(&MEM_local_pool);
  {
    OP* op = NULL;
    OP* copy_op = NULL;
    BB* body = Loop_header();
    BB_SET *bbs = BB_SET_Create(0, &MEM_local_pool);
    bbs = BB_SET_Union1D(bbs, body, &MEM_local_pool);

    EBO_after_unrolling(bbs);

    /* after second EBO, we might have identity copy as x <= x[1] which
       basically propagate a value from the first iter to the last.
       Find and remove those here.
    */
    DYN_ARRAY<OP *> to_be_removed(&MEM_local_pool);
    BOOL trace_copy_removal = Get_Trace(TP_CGPREP, 2);
    FOR_ALL_BB_OPs(body, op) {
      copy_op = op;
      if (copy_op && OP_copy(copy_op) &&
	  OP_results(copy_op)==1 && OP_opnds(copy_op)==1) {
	BOOL can_be_removed=TRUE;
	TN* src = OP_opnd(copy_op, 0);
	TN* dest = OP_result(copy_op, 0);
	if (dest == src && OP_omega(copy_op, 0)==1) {
	  OP* scan;
	  FOR_ALL_BB_OPs(body, scan) {
	    if (scan != copy_op) {
	      if (OP_Defs_TN(scan, dest))
		can_be_removed = FALSE;
	      if (OP_Refs_TN(scan, dest)) {
		for (int i=0; i<OP_opnds(copy_op); i++) {
		  TN* opnd = OP_opnd(copy_op,i);
		  if (opnd == dest && OP_omega(copy_op,i)==0)
		    can_be_removed = FALSE;
		}
	      }
	      if (can_be_removed==FALSE)
		break;
	    }
	  }
	  if (can_be_removed) {
	    to_be_removed.AddElement(copy_op);
	  }
	}
      }
    }
    for (int i=to_be_removed.Elements(); i>0; i--) {
      OP* op = to_be_removed[i-1];
      if (trace_copy_removal) {
	#pragma mips_frequency_hint NEVER
	fprintf(TFile, "<cgprep> removing loop propagating copy: ");
	Print_OP_No_SrcLine(op);
      }
      BB_Remove_Op(body, op);
    }
  }
  MEM_POOL_Pop(&MEM_local_pool);
}


void CG_LOOP::Print(FILE *fp)
{
  for (BB *bb = epilog_start; ; bb = BB_next(bb)) {
    Print_BB_No_Srclines(bb);
    if (bb == epilog_end)
      break;
  }
}


static BOOL
Skip_Loop_For_Reason(LOOP_DESCR *loop)
{
  char	*reason = NULL;
  BB	*bb;
  BOOL trace_general = Get_Trace(TP_CGPREP, 1);

  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb)
  {
    if (BB_compile(bb) == FALSE)
    {
      reason = "contains BB from already-scheduled region";
      break;
    }
  }
	  
  if (!reason)
  {
    BB *head = LOOP_DESCR_loophead(loop);
    TN *trip_count = CG_LOOP_Trip_Count(loop);
    LOOPINFO *info = LOOP_DESCR_loopinfo(loop);

    if (info && WN_Loop_Unimportant_Misc(LOOPINFO_wn(info)))
    {
      reason = "marked UNIMPORTANT_MISC";
    }
    else if (info &&
	    !CGPREP_optimize_lno_winddown_cache &&
	     WN_Loop_Winddown_Cache(LOOPINFO_wn(info)))
    {
      reason = "marked WINDDOWN_CACHE; see -CG:opt_lno_winddown_cache";
    }
    else if (info &&
	    !CGPREP_optimize_lno_winddown_reg &&
	     WN_Loop_Winddown_Reg(LOOPINFO_wn(info)))
    {
      reason = "marked WINDDOWN_REG; see -CG:opt_lno_winddown_reg";
    }
    else if (!CGPREP_optimize_non_trip_countable &&
	      trip_count == NULL)
    {
      reason = "not trip-countable; see -CG:opt_non_trip_countable";
    }
    else if (!CGPREP_optimize_non_innermost &&
	     !BB_innermost(head))
    {
      reason = "not innermost loop; see -CG:opt_non_innermost";
    }
#ifdef MIPS_UNROLL
    else if (!CG_LOOP_Attach_Prolog_And_Epilog(loop))
    {
      reason = "loop has multiple back edges";
    }
#endif
#ifdef MIPS_UNROLL
    else if (!CG_LOOP_epilog && !CGPREP_optimize_multi_targ)
    {
      reason = "loop exits to multiple targets; see -CG:opt_multi_targ";
    }
#endif
    else if ( BBlist_Fall_Thru_Succ(head) == NULL)
    {
      reason = "loop never exits";
    }
  }
	
  if (reason)
  {
    if (trace_general) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, "<cgprep> rejecting: %s\n", reason);
    }
    return TRUE;
  }
  return FALSE;
}


/* 
   This function is a filter function used to avoid processing large loops
   bodies. The heuristics used exactly match the ones used in if-conversion 
   (phase-2), a pre-cursor to SWP. TODO: Need to fix sched_est interface to
   make it more efficient.
*/
static BOOL Loop_Amenable_For_SWP(LOOP_DESCR *loop, BOOL trace)
{
  BB *bb;
  UINT32 bb_ctnt = 0;
  UINT32 insts_ctnt = 0;
  
  FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb) {
    bb_ctnt++;
    insts_ctnt += BB_length(bb);
    if (bb_ctnt > CG_maxblocks ||
	insts_ctnt > CG_maxinss) {
      if (trace) 
	fprintf(TFile,"<loop> swp disabled: Loop body size too big");
      return FALSE;
    }
  }

  /* Estimate the number of cycles in the loop before full if-conversion. 
   * This serves as a upper-bound (max_ii) for SWP to beat. Sometimes, SWP 
   * ends up generating schedules which are more worse than otherwise.
   * For more details, see 583256, 582711, 583446.
   * Currently, the sched_interface doesn't seem to handle loops with
   * internal cycles (?). Return INT32_MAX in those cases.
   */
  
  double cycles_before_if_cvt =  LOOP_DESCR_Estimate_Cycles (loop);
  INT32 old_num_loop_bbs = BB_SET_Size(LOOP_DESCR_bbset(loop));

  // If loh_ops is removed, we can get more accurate schedule estimate.
  // But don't unless loh_ops are removed from the consideration of
  // LOOP_DESCR_Estimate_Cycles as well.
  MEM_POOL_Push(&MEM_local_nz_pool);

  BB *head =  LOOP_DESCR_loophead(loop);
  BB_SET *bbs = LOOP_DESCR_bbset(loop);
  CG_SCHED_EST *loop_se = CG_SCHED_EST_Create(head, &MEM_local_nz_pool, 
					      SCHED_EST_FOR_UNROLL);
  FOR_ALL_BB_SET_members(bbs, bb) {
    if (bb != head) {
      CG_SCHED_EST *se = CG_SCHED_EST_Create(bb, &MEM_local_nz_pool, 
					     SCHED_EST_FOR_UNROLL);
      CG_SCHED_EST_Append_Scheds(loop_se, se);
    }
  }
  
  double estimate_swp_cycles = CG_SCHED_EST_Resource_Cycles(loop_se);

  MEM_POOL_Pop(&MEM_local_nz_pool);
  
  if (estimate_swp_cycles <= cycles_before_if_cvt) {
    if (trace) { 
      #pragma mips_frequency_hint NEVER
      fprintf(TFile,
	      "<loop> swp enabled: estimate_swp_cycles=%g <= cycles_before_if_cvt=%g",
	      estimate_swp_cycles, cycles_before_if_cvt);
    }
    return TRUE;
  } else {
    if (trace) {
      #pragma mips_frequency_hint NEVER
      fprintf(TFile, 
	      "<loop> swp disabled: estimate_swp_cycles=%g > cycles_before_if_cvt=%g",
	      estimate_swp_cycles, cycles_before_if_cvt);
    }
    return FALSE;
  }
}


// Replace the loop-back branch with the counted loop branch
// instruction.  It is a nop for the MIPS architecture.
//
void Gen_Counted_Loop_Branch(CG_LOOP& cl)
{
  TN *trip_count_tn = cl.Trip_count_tn();
  BB *head = cl.Loop_header();
  BB *prolog = cl.Prolog_end();

  OPS ops = OPS_EMPTY;
  OPS body_ops = OPS_EMPTY;
  OP *br_op = BB_branch_op(head);

  // Already converted into counted loop!
  if (CGTARG_OP_is_counted_loop(br_op)) return;

#if 0 /* TENSILICA : fixme */
  TN *label_tn = OP_opnd(br_op, Branch_Target_Operand(br_op));
  CGTARG_Generate_Branch_Cloop(br_op, trip_count_tn, trip_count_tn, 1,
			       label_tn, &ops, &body_ops);
  if (OPS_length(&body_ops) > 0) {
    BB_Remove_Op(head, br_op);
    BB_Append_Ops(head, &body_ops);
    // Insert loop counter initialization to prolog
    BB_Append_Ops(prolog, &ops);
  }
#endif
}


//  Fix backpatches.  Some backpatches are obsoleted because
//  EBO and other optimizations has changed the def and uses
//
void Fix_Backpatches(CG_LOOP& cl, bool trace)
{
  vector<pair<BB*, CG_LOOP_BACKPATCH *> > dead_bp;
  set<TN*> epilog_tns;
  BB *body = cl.Loop_header();
  BB *prolog = CG_LOOP_prolog;
  BB *epilog = CG_LOOP_epilog;
  CG_LOOP_BACKPATCH *bp;
  for (bp = CG_LOOP_Backpatch_First(epilog, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    if (!GTN_SET_MemberP(BB_live_in(epilog), tn))
      dead_bp.push_back(std::make_pair(epilog,bp));
    else
      epilog_tns.insert(body_tn);
  }
  for (bp = CG_LOOP_Backpatch_First(prolog, NULL); bp; bp = CG_LOOP_Backpatch_Next(bp)) {
    TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
    if (!GTN_SET_MemberP(BB_live_in(body), body_tn) &&
	!(epilog_tns.find(body_tn) != epilog_tns.end()))
      dead_bp.push_back(std::make_pair(prolog,bp));
  }
  while (!dead_bp.empty()) {
    BB *bb = (*(dead_bp.end()-1)).first;
    bp = (*(dead_bp.end()-1)).second;
    if (trace) {
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      fprintf(TFile, "Fix_Backpatches: delete backpatches with body TN%d\n", 
	      TN_number(body_tn));
    }
    CG_LOOP_Backpatch_Delete(bb, bp);
    dead_bp.pop_back();
  }
}

/*
 * Some loop's trip count is constant (but not known before) and 
 * known at this point. This function tries to find out if it
 * is a constant by recomputing it.
 */
static void
Recompute_Trip_Count(LOOP_DESCR *loop)
{
  TN *trip_count = CG_LOOP_Trip_Count(loop);

  /* for now, only do it for a loop with s single BB */
  if (trip_count == NULL ||
      TN_is_constant(trip_count) ||
      (BB_SET_Size(LOOP_DESCR_bbset(loop)) != 1)) {
    return;
  }

  BB *bb = LOOP_DESCR_loophead(loop);

  /* If trip_count is defined within this loop, then it's not. */
  if (BB_Defs_TN(bb, trip_count)) {
    return;
  }
  

  /*
   *  Assume the loop has the unique pred BB (except loop's backedge)
   *  as shown below:
   *
   *       pred
   *        |
   *        V
   *  <- bb (loop)  <--
   *        |_________|
   * 
   *  For other loops, we simply stop for now !
   */  
  BB *pred=NULL, *cur_bb;
  BBLIST *edge;
  FOR_ALL_BB_PREDS(bb, edge) {
    cur_bb = BBLIST_item(edge);
    if (cur_bb == bb) { // loop's back edge
      continue;
    }
    if (pred == NULL) {
      pred = cur_bb;
    } else {
      pred = NULL;
      break;
    }
  }
  if (pred != NULL) {
    INT64 val;
    if (TN_Value_At_Op (trip_count, BB_last_op(pred),  &val)) {
      TN *const_tn = Gen_Literal_TN(val, TN_size(trip_count));
      LOOPINFO *linfo = LOOP_DESCR_loopinfo(loop);
      LOOPINFO_trip_count_tn(linfo) = const_tn;
      LOOPINFO_max_trip_count_tn(linfo) = const_tn;

      /*
       * CG uses trip-count/est either from wn or from LOOPINFO, which
       * require both wn and LOOPINFO have the same trip_count/est info. 
       * Hence, whenver LOOPINFO's are changd,  the wn's should be
       * changed as well.
       */
      WN *wn = LOOPINFO_wn(linfo);
      TYPE_ID rtype = WN_rtype(WN_loop_trip(wn));
      OPCODE opc_intconst = OPCODE_make_op(OPR_INTCONST, rtype, MTYPE_V);
      WN *const_wn = WN_CreateIntconst(opc_intconst, val);
      WN_set_loop_trip(wn, const_wn);
      WN_loop_trip_est(wn) = val;
    }
  } 
}


/* Perform loop optimizations for one loop */

BOOL
CG_LOOP_Optimize (LOOP_DESCR *loop, vector<SWP_FIXUP>& fixup)
{
  enum LOOP_OPT_ACTION {
    NO_LOOP_OPT,
    SINGLE_BB_DOLOOP_SWP,
    SINGLE_BB_DOLOOP_UNROLL,
    SINGLE_BB_WHILELOOP_UNROLL,
  };

  /* Recompute the loop count, it could be constant by now */
  Recompute_Trip_Count(loop);

  /* This checks, among other things, that 'loop' is an innermost
     loop. */
  if (Skip_Loop_For_Reason(loop))
    return FALSE;

  BOOL trace_loop_opt = Get_Trace(TP_CGPREP, 0x400000);
 
  /* Determine how to optimize the loop. */
  
  LOOP_OPT_ACTION action = NO_LOOP_OPT;
  BOOL has_trip_count = (CG_LOOP_Trip_Count(loop) != NULL);

  /* Why not use 'Is_Multibb_Loop' here? Because it seems
     LOOP_DESCR_Detect_Loops doesn't initialize it. Why not? It is
     initialized in gcm... dwg */

  BOOL single_bb = (BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1);

  /* We can attempt to unroll single block, innermost loops. We try to
     use a zcl for all loops, whether we unroll them or not.

     But we neither do SWP nor unroll if loop is marked as FREQ Never
   */

  if (single_bb && BB_innermost(LOOP_DESCR_loophead(loop)) &&
      !Is_Frequency_Never_Loop(loop))
  {
    if (has_trip_count)
    {
      if (xt_zero_cost_loop && Enable_ZCL && Enable_SWP) {
	action = SINGLE_BB_DOLOOP_SWP;
      } else {
	action = SINGLE_BB_DOLOOP_UNROLL;
      }
    }
    else
    {
      action = SINGLE_BB_WHILELOOP_UNROLL;
    }
  }

  if (action == SINGLE_BB_DOLOOP_SWP)
  {
      CG_LOOP cg_swp_loop(loop);

      // save a copy of the loop before SWP attempt
      BB* orig_body = CG_LOOP_Save_Loop(cg_swp_loop);

      BB *head = cg_swp_loop.Loop_header();
      OP *br_op = BB_branch_op(head);
      OP* op;

      float loop_fb_exec_freq=0.0;

      INT unroll_times_min = SWP_Options.Min_Unroll_Times;
      INT unroll_times_max = SWP_Options.Max_Unroll_Times;
      INT unroll_times = 1;

      if (OPT_unroll_times_overridden && unroll_times_max > CG_LOOP_unroll_times_max)
	unroll_times_max = CG_LOOP_unroll_times_max;

      if (OPT_Space)
	unroll_times_max = 1;

      if (cg_swp_loop.Need_super_swp() && cg_swp_loop.Super_swp_unroll() != 0) {
	unroll_times_min = unroll_times_max = cg_swp_loop.Super_swp_unroll();
      }

      WN* wn = cg_swp_loop.SWP_schedule_pragma_wn();
      INT specified_unroll= 0;
      if (wn) {
	STR_IDX str_idx = WN_pragma_arg1(wn);
	const char* str = Index_To_Str(str_idx);
	str = strchr(str, ',');
	if (!strncmp(str+2,"unroll =",8)) {
	  specified_unroll = atoi(str+11);
	}
      }

      float current_ii = DBL_MAX;
      float best_ii = DBL_MAX;
      INT best_unroll_times = -1;
      INT best_stage_count = -1;
      INT best_length = INT_MAX;
      LOOPINFO *info = LOOP_DESCR_loopinfo(loop);

      if (has_fb_freq) {
	loop_fb_exec_freq = CGLOOP_get_bb_fb_exec_cnt(head);
	/* do not swp if the pu is executed but the
	 * loop is not executed according to feedback data to avoid
	 * messing up later phases (mainly GRA splitting) unnecessarily.
	 * See pr9512.
	 */
	if (CGLOOP_get_bb_fb_exec_cnt(REGION_First_BB)!=0.0 && loop_fb_exec_freq==0.0)
	  goto swp_failed;
      }

      /* Don't SWP if the estimated number of iterations is 1. */
      if (info && LOOPINFO_wn(info) && WN_loop_trip_est(LOOPINFO_wn(info)) <= 1)
	goto swp_failed;

      if (!cg_swp_loop.Has_prolog_epilog())
	goto swp_failed;

      if (trace_loop_opt)
	CG_LOOP_Trace_Loop(loop, "*** Before loop canonicalization swp ***");

      if (!Prepare_Loop_For_SWP_1(cg_swp_loop, trace_loop_opt))
	goto swp_failed;

      if (!br_op)
	goto swp_failed;

      cg_swp_loop.LI_Hoisting_BB();

      // the following constraints are handled in Detect_SWP_Constraints() in
      // Perform_SWP() now
#if 0
      FOR_ALL_BB_OPs(head, op) {
	if (OP_has_multireg_operand(op) || OP_privileged(op) ||
	    OP_simulated(op) || OP_code(op) == TOP_asm) {
	  goto swp_failed;
	}
      }
#endif

#ifndef TARG_XTENSA
      // Replace regular branch with loop-count branches.
      // There will be a call EBO to delete the loop-exit test evaluations.
      Gen_Counted_Loop_Branch(cg_swp_loop);
      Gen_SWP_Branch(cg_swp_loop, true /* is_doloop */);
#endif

      cg_swp_loop.Recompute_Liveness();
      Rename_TNs_For_BB(cg_swp_loop.Loop_header(), NULL);
      cg_swp_loop.Recompute_Liveness();

      cg_swp_loop.Build_CG_LOOP_Info();
      cg_swp_loop.Recompute_Liveness();

      if (cg_swp_loop.Determine_Unroll_Fully()) {
	Unroll_Do_Loop_Fully(loop, cg_swp_loop.Unroll_factor());
	cg_swp_loop.Recompute_Liveness();
	return TRUE;
      }

      Perform_Read_Write_Removal(loop);
      cg_swp_loop.Recompute_Liveness();

      if (trace_loop_opt)
	CG_LOOP_Trace_Loop(loop, "*** Before Postincr generation / After RW removal swp ***");

      //  Form postincr form
      //  currently a no op, should be taken care by
      //  Loop_Updating_Optimization already
#ifndef TARG_XTENSA
      if (!Prepare_Loop_For_SWP_2(cg_swp_loop, trace_loop_opt)) {
	goto swp_failed;
      }
#endif

      if (trace_loop_opt)
	CG_LOOP_Trace_Loop(loop, "*** Before Fix_Recurrences / After Postincr swp ***");

      // Break recurrences will compute dep-graph itself
      Induction_Variables_Removal(LOOP_DESCR_bbset(cg_swp_loop.Loop()),
				  CG_LOOP_epilog,
				  true/*delete*/,
				  true/*keep prefetch*/,
				  NULL/*keep loop back branch*/,
				  trace_loop_opt);
      Fix_Recurrences_Before_Unrolling(cg_swp_loop);

      if (trace_loop_opt)
	CG_LOOP_Trace_Loop(loop, "*** before ebo 1 and unrolling / after fix recurrences swp **");

      cg_swp_loop.Recompute_Liveness();
      cg_swp_loop.EBO_Before_Unrolling_For_SWP();

#ifdef TARG_IA64
      if (SWP_Options.Predicate_Promotion) {
	list<BB*> bbl;
	bbl.push_front(cg_swp_loop.Loop_header());
	CG_DEP_Prune_Dependence_Arcs(bbl, TRUE, trace_loop_opt);
	if (trace_loop_opt)
	  CG_LOOP_Trace_Loop(loop, "*** after ebo 1 and prune predicate / before unrolling swp **");
      }
#endif

      // We do not call the following routine anymore.
      // Instead we are going to try SWP with unrollings (of power of 2) within
      // allowed range.
      // This is because schedule estimation for unrolling under complex slot
      // constraint can be very inaccurate. Another reason for scheduling for
      // different unrolling is to benefit from renaming of unrolled live-ranges
      // while at the same time get better code than traditional
      // modulo renaming, which simply replicate the loop body after it is
      // modulo scheduled. The repliacation method will not pack ops from
      // different replicated bodies in a cycle.

      // cg_swp_loop.Determine_SWP_Unroll_Factor();

      // determing starting point for searching best unrollings
      while (unroll_times < unroll_times_min)
	unroll_times <<= 1;

      // save a copy of the loop before unrolling attempts
      cg_swp_loop.Save(/*is_best=*/FALSE);

      INT32 unroll_times_save;
      // search for the best unrolling
      while (specified_unroll ||
	     (unroll_times!=0 && unroll_times <= unroll_times_max)) {

	if (specified_unroll) {
	  unroll_times_save = unroll_times;
	  unroll_times = specified_unroll;
	}

	head = cg_swp_loop.Loop_header();
	br_op = BB_branch_op(head);
	cg_swp_loop.Set_unroll_factor(unroll_times);

	if (cg_swp_loop.Unroll_factor() > 1) {
	  cg_swp_loop.Recompute_Liveness();
	  Induction_Variables_Removal(LOOP_DESCR_bbset(cg_swp_loop.Loop()),
				  CG_LOOP_epilog,
				  false/*delete*/,
				  true/*mark prefetch as loh*/,
				  br_op/*mark loop back branch as loh*/,
				  trace_loop_opt);
	  cg_swp_loop.Recompute_Liveness();

	  Unroll_Do_Loop(cg_swp_loop, cg_swp_loop.Unroll_factor());

	  if (cg_swp_loop.Unroll_factor()!=unroll_times) {
	    if (!specified_unroll)
	      break;	// break the unrolling search if unroll fails
	  }

	  cg_swp_loop.Redo_Prolog_Epilog();
	  cg_swp_loop.Recompute_Liveness();

	  // we need to remove the notations here to flush out the glue copy
	  // collected so far so that wind-up/down codes are generated
	  // after glue copies are generated
	  CG_LOOP_Remove_Notations(cg_swp_loop, CG_LOOP_prolog, CG_LOOP_epilog);

	  head = cg_swp_loop.Loop_header();
	  br_op = BB_branch_op(head);
        }

	// if there are instructions in the prolog/epilog, we need to
	// extend the prolog/epilog to separate them from the wind-up/down
	// code to be generated later
	if (BB_length(CG_LOOP_prolog)>0 || BB_length(CG_LOOP_epilog)>0) {
	  if (BB_length(CG_LOOP_prolog)>0)
	    extend_prolog();
	  if (BB_length(CG_LOOP_epilog)>0)
	    extend_epilog(loop);
	  // GRA_LIVE_Compute_Local_Info is called with
	  // extend_prolog/extend_epilog so we need to recompute
	  // in order not to mess up mem pool usage in gra_live.cxx
	  cg_swp_loop.Redo_Prolog_Epilog();
	  cg_swp_loop.Recompute_Liveness();
	}

	if (trace_loop_opt)
	  CG_LOOP_Trace_Loop(loop, "*** Before ebo 2/ after unrolling swp ***");

	cg_swp_loop.EBO_After_Unrolling_For_SWP();

	BB_Verify_OP_Order(cg_swp_loop.Loop_header());

	// Break recurrences will compute dep-graph itself
	Fix_Recurrences_After_Unrolling(cg_swp_loop);

	if (trace_loop_opt)
	  CG_LOOP_Trace_Loop(loop,
		"*** Before implicit prefetch / after fix recurrences 2 swp ***");

	Gen_Implicit_Prefetches(cg_swp_loop, trace_loop_opt);

	if (trace_loop_opt)
	  CG_LOOP_Trace_Loop(loop,
		"*** Before Ind. Var. Removal / after implicit prefetch  swp ***");

	// try another round of ebo to propagate copies resulted from last ebo
	cg_swp_loop.Recompute_Liveness();
	cg_swp_loop.EBO_After_Unrolling_For_SWP();

	// Update backpatches
	cg_swp_loop.Recompute_Liveness();
	Induction_Variables_Removal(LOOP_DESCR_bbset(cg_swp_loop.Loop()),
				  CG_LOOP_epilog,
				  false/*delete*/,
				  true/*mark prefetch as loh*/,
				  br_op/*mark loop back branch as loh*/,
				  trace_loop_opt);
	cg_swp_loop.Recompute_Liveness();
	Fix_Backpatches(cg_swp_loop, trace_loop_opt);

	if (!OP_loh(br_op)) {
	  goto swp_failed;
	}

	if (trace_loop_opt)
	  CG_LOOP_Trace_Loop(loop,
		"*** Before swp / after Ind. Var. Removal  swp ***");

	if (Perform_SWP(cg_swp_loop, fixup,
			(best_unroll_times== -1)?INT_MAX:
			(INT)(best_ii*unroll_times)-1, true /*doloop*/)) {
	  // SWP succeeded

	  // try to remove loh in the remainder loop now that the loh in unrolled
	  // loop is removed
	  BB *remainder_loop = cg_swp_loop.Remainder_loop();
	  if (remainder_loop) {
	    OP *br_op = BB_branch_op(head);
	    if (br_op && OP_code(br_op)==TOP_loop_end) {
	      OP* op;
	      OP* next_op = BB_first_op(remainder_loop);
	      while (next_op) {
	        op = next_op;
	        next_op = OP_next(op);
	        if (OP_loh(op)) {
		  BB_Remove_Op(remainder_loop, op);
	        }
	      }
	    }
	  }

	  BB *next_bb = CG_LOOP_prolog_start;
	  BB *bb;
	  INT total_length = 0;

	  // compute the code size
	  do {
	    bb = next_bb;
	    total_length += BB_length(bb);
	    next_bb = BB_next(bb);
	  } while (bb!=CG_LOOP_epilog_end);

	  ANNOTATION *loop_annot = ANNOT_Get(
		BB_annotations(cg_swp_loop.Loop_header()), ANNOT_LOOPINFO);
	  LOOPINFO *loop_info = ANNOT_loopinfo(loop_annot);
	  LOOPINFO_swp_failure_code(loop_info) = SWP_OK;

	  ANNOTATION *annot = ANNOT_Get(
		BB_annotations(cg_swp_loop.Loop_header()), ANNOT_ROTATING_KERNEL);
	  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);

	  // compute the effective ii
	  current_ii = ((float)ROTATING_KERNEL_INFO_ii(info)) / unroll_times;
	  INT stage_count = ROTATING_KERNEL_INFO_stage_count(info);

	  if (trace_loop_opt) {
	    fprintf(TFile, "--------------------------------------\n");
	    fprintf(TFile, "SWP with unroll_times=%d, ii=%f, kernel_length=%d,"
			   " total_length=%d\n", 
			unroll_times, current_ii,
			ROTATING_KERNEL_INFO_ii(info), total_length);
	  }

	  bool better = false;

	  if (best_unroll_times == -1) {
	    better = true;
	  } else if (current_ii == best_ii && total_length < best_length) {
	    better = true;
	  } else if (current_ii >= best_ii) {
	    better = false;
	  } else {

	    if (has_fb_freq) {
	      double total_cycle_reduction =
		    (best_ii - current_ii) * (double)loop_fb_exec_freq;
	      double delta = total_cycle_reduction / (double)Total_Cycle_Count.Value();
	      if (delta >= (double)0.01)
		better = true;
	    }

	    if (!better) {
	      double cycle_reduction = (best_ii - current_ii);
	      double delta = cycle_reduction / best_ii;
	      if (delta >= (double)0.05)
		better = true;
	    }
	  }

	  if (better)
	  {
	    if (trace_loop_opt) {
	      fprintf(TFile, "  Found better SWP unroll_times=%d\n", unroll_times);
	    }
	    best_ii = current_ii;
	    best_unroll_times = unroll_times;
	    best_length = total_length;
	    best_stage_count = stage_count;

	    // save the best schedule found so far
	    cg_swp_loop.Save(/*is_best=*/TRUE);

	  } else {

	    break;
	  }

	  // check if the body was empty and was inserted a nop
	  // by Perform_SWP to do the modulo scheduling
	  // stop further unrolling if this is the case
	  if (BB_length(head)==2) {
	    // just the nop and the loop_end
	    OP* op = BB_first_op(head);
	    if (OP_code(op)==TOP_nop || OP_code(op)==TOP_nop_n)
	      break;
	  }

	  // restore to before unrolling and try the next unrolling factor
	  cg_swp_loop.Restore(/*is_best=*/FALSE);

	  if (specified_unroll) {
	    unroll_times = unroll_times_save;
	    specified_unroll = 0;
	  } else
	    unroll_times <<= 1;	// power of 2 unrollings only

	  cg_swp_loop.Recompute_Liveness();

	  continue;

	} else {
	  // SWP failed
	  ANNOTATION *loop_annot = ANNOT_Get(
		BB_annotations(cg_swp_loop.Loop_header()), ANNOT_LOOPINFO);
	  LOOPINFO *loop_info = ANNOT_loopinfo(loop_annot);

	  ANNOTATION *annot = ANNOT_Get(
		BB_annotations(cg_swp_loop.Loop_header()), ANNOT_ROTATING_KERNEL);
	  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(annot);

	  LOOPINFO_swp_failure_code(loop_info)=
			ROTATING_KERNEL_INFO_failure_code(info);
	  if (LOOPINFO_swp_failure_code(loop_info)==REG_ALLOC_FAILED)
		  LOOPINFO_swp_unallococatable_rc(loop_info) =
		  	ROTATING_KERNEL_INFO_unallocatable_rc(info);

	  if (specified_unroll) {

	    // restore to before unrolling and try the next unrolling factor
	    cg_swp_loop.Restore(/*is_best=*/FALSE);

	    unroll_times = unroll_times_save;
	    specified_unroll = 0;
	    continue;
	  }
	  break;
	}

      }

      if (cg_swp_loop.Has_best()) {
	unroll_times = best_unroll_times;
	cg_swp_loop.Restore(/*is_best=*/TRUE);
	if (CGPREP_remove_guard_branch) {
	  // Try to eliminate a guard branch, it there is one.
	  Remove_Guard_Branch(CG_LOOP_prolog);
	}
	return TRUE;
      }

swp_failed:
      // restore to the original loop since SWP failed
      CG_LOOP_Restore_Loop(cg_swp_loop, orig_body);
      cg_swp_loop.Recompute_Liveness();
      Rename_TNs_For_BB(cg_swp_loop.Loop_header(), NULL);
      cg_swp_loop.Recompute_Liveness();
      action = SINGLE_BB_DOLOOP_UNROLL;	// try unrolling and no SWP

  }

  CG_LOOP cg_loop(loop);

  if (action == SINGLE_BB_DOLOOP_UNROLL)
  {
    if (!cg_loop.Has_prolog_epilog()) 
      goto try_zcl;

    if (trace_loop_opt) 
      CG_LOOP_Trace_Loop(loop, "*** Before LOOP CANONICALIZATION ***");

    /* Perform_Read_Write_Removal requires that non-definite
       dependence are removed. */
    
    if (!Remove_Non_Definite_Dependence(cg_loop, false, trace_loop_opt))
      goto try_zcl;

    cg_loop.Build_CG_LOOP_Info();

    cg_loop.LI_Hoisting_BB();

    if (trace_loop_opt) 
      CG_LOOP_Trace_Loop(loop, "*** Before SINGLE_BB_DOLOOP_UNROLL ***");

    cg_loop.Recompute_Liveness();
    cg_loop.Determine_Unroll_Factor();

    if (cg_loop.Unroll_fully())
    {
      /* No need to call RW removal because EBO should find all such
         CSEs after full unrolling. */
      Unroll_Do_Loop_Fully(loop, cg_loop.Unroll_factor());
    }
    else
    {
      Perform_Read_Write_Removal(loop);

      /* Break recurrences will compute dep-graph itself. */
      Fix_Recurrences_Before_Unrolling(cg_loop);

      if (cg_loop.Unroll_factor() > 1)
	Unroll_Do_Loop(cg_loop, cg_loop.Unroll_factor());

      cg_loop.Recompute_Liveness();
      CG_LOOP_Remove_Notations(cg_loop, CG_LOOP_prolog, CG_LOOP_epilog);
    }

    cg_loop.Recompute_Liveness();
    cg_loop.EBO_After_Unrolling();
  }
  else if (action != NO_LOOP_OPT)
  {
    if (action == SINGLE_BB_WHILELOOP_UNROLL) {
      cg_loop.Build_CG_LOOP_Info();
      cg_loop.LI_Hoisting_BB();
    }

    /* TENSILICA: I don't think we want to do this since it just
       replicates the loop body, ending branch and all, and that won't
       buy us anything. ...perhaps for small constant loop trips so we
       can unroll fully. */
#if 0
    Is_True(action == SINGLE_BB_WHILELOOP_UNROLL,
	    ("Unexpected unroll action %d", action));

    if (trace_loop_opt) 
      CG_LOOP_Trace_Loop(loop, "*** Before SINGLE_BB_WHILELOOP_UNROLL ***");

    cg_loop.Build_CG_LOOP_Info();
    cg_loop.Determine_Unroll_Factor();
    Unroll_Dowhile_Loop(loop, cg_loop.Unroll_factor());
    cg_loop.Recompute_Liveness();
    cg_loop.EBO_After_Unrolling();
#endif
  }

  /* Attempt to use a zero-cost loop for 'loop'. */

  try_zcl:
  
  if (xt_zero_cost_loop && Enable_ZCL)
  {
    cg_loop.Redo_Prolog_Epilog();
    if (Zcl_Loop(cg_loop, FALSE))
    {
      cg_loop.Recompute_Liveness();

      // Induction_Var...() works only if both prolog and epilog are present.
      if (cg_loop.Has_prolog_epilog()) {
        if (trace_loop_opt) 
          CG_LOOP_Trace_Loop(loop, "*** Before Induction Variable Removal ***");
    
        Induction_Variables_Removal(LOOP_DESCR_bbset(cg_loop.Loop()), CG_LOOP_epilog,
		      				   TRUE, TRUE, NULL, trace_loop_opt);
      }
      
      return TRUE;
    }
  }

  // Try to eliminate branch relaxation for loop back edge
  if (Remove_Back_Edge_Relaxation(cg_loop)) {
    cg_loop.Recompute_Liveness();
    Induction_Variables_Removal(LOOP_DESCR_bbset(cg_loop.Loop()), CG_LOOP_epilog,
		    				   TRUE, TRUE, NULL, trace_loop_opt);
  }

  return TRUE;
}


static void
Feedback_Freq_Init ()
{
  has_fb_freq = (Cur_PU_Feedback!=NULL);
  if (has_fb_freq) {
    /* Get the actual execution count for each BB (BB_freq is normalized
       to per-function-invocation, so we don't want that). */
    bb_fb_exec_cnts = FREQ_Denormalized_Counts();
  } else
    bb_fb_exec_cnts=0;
}

static void
Feedback_Freq_Finish ()
{
  if (bb_fb_exec_cnts) {
    BB_MAP_Delete(bb_fb_exec_cnts);
    bb_fb_exec_cnts=0;
  }
}


/*
**	Obey -CG:skip_local_prep
*/
BOOL
CG_LOOP_Skip(BB *bb)
{
  return (BB_id(bb) < CG_LOOP_skip_before ||
	  BB_id(bb) > CG_LOOP_skip_after ||
	  BB_id(bb) == CG_LOOP_skip_equal);
}


void
CG_LOOP_Statistics (LOOP_DESCR *loop)
{
  BB *head = LOOP_DESCR_loophead(loop);
  BB_SET *bbs = LOOP_DESCR_bbset(loop);
  TN *trip_count = CG_LOOP_Trip_Count(loop);
  bool inner = BB_innermost(head);
  bool has_call = false;
  bool early_exit = false;
  bool multi_exit = false;
  bool has_return = false;
  INT32  nbb= BB_SET_Size(bbs);
  INT32  nops = 0;
  BB *bb;
  FOR_ALL_BB_SET_members(bbs, bb) {
    if (BB_call(bb))
      has_call = true;
    nops += BB_length(bb);
  }

  BBLIST *succs;
  BB *loop_merge = NULL;
  FOR_ALL_BB_SUCCS(head, succs) {
    BB *succ = BBLIST_item(succs);
    if (!BB_SET_MemberP(bbs, succ)) {
      loop_merge = succ;
    }
  }
  FOR_ALL_BB_SET_members(bbs,bb) {
    if (bb != head) {
      FOR_ALL_BB_SUCCS(bb, succs) {
	BB *succ = BBLIST_item(succs);
	if (!BB_SET_MemberP(bbs, succ)) {
	  early_exit = true;
	  if (succ != loop_merge)
	    if (BB_exit(succ)) 
	      has_return = true;
	    else
	      multi_exit = true;
	}
      }
    }
  }

  fprintf(TFile, "<loopstat> %s %s\n", 
	  inner ? "inner" : "no_inner",
	  trip_count ? "has_trip_count" : "no_trip_count");
  if (inner) {
    fprintf(TFile, "<loopstat_inner> nbb=%d nops=%d %s %s %s %s\n",
	    nbb, nops,
	    has_call ? "has_call" : "no_call",
	    has_return ? "has_return" : "no_return",
	    early_exit ? "has_early_exit" : "no_early_exit",
	    multi_exit ? "has_multi_exit" : "no_multi_exit");
  }
}



// Perform loop optimizations for all inner loops
// in the PU.
//
void Perform_Loop_Optimizations()
{
  MEM_POOL loop_descr_pool;
  MEM_POOL_Initialize(&loop_descr_pool, "loop_descriptors", TRUE);
  MEM_POOL_Push (&loop_descr_pool);
  BOOL trace_general = Get_Trace(TP_CGPREP, 1);

  SWP_Options.PU_Configure();

  Calculate_Dominators();		/* needed for loop recognition */

  /* Apply fusion. */
  if (CG_fusion)
    CG_FUSION_Optimize_Region();
  
  SWP_FIXUP_VECTOR fixup;
  
  LOOP_DESCR *loops = LOOP_DESCR_Detect_Loops(&loop_descr_pool);

  if (trace_general)
  {
    fprintf(TFile, "<cgprep> Perform_Loop_Optimizations\n");
    LOOP_DESCR_Print_List();
  }

  /* For zero-cost-loops we rely on the fact that we are visiting
     loops innermost first, so that we can mark the outerloops when we
     use a zero-cost loop. */
  for (LOOP_DESCR *loop = loops ; loop; loop = LOOP_DESCR_next(loop)) {
    BB *head = LOOP_DESCR_loophead(loop);
	  
    if (CG_LOOP_Skip(head)) {
      DevWarn("CG_LOOP skip BB%d.", BB_id(head));
      continue;
    }
      
    if (trace_general)
    {
      fprintf(TFile, "%s<cgprep> %sloop line %d, BBs ",
	      DBar, BB_innermost(head) ? "innermost " : "",
	      BB_Loop_Lineno(head));
      BB_SET_Print(LOOP_DESCR_bbset(loop), TFile);
      fprintf(TFile, ", head BB:%d, nest level %d\n",
	      BB_id(head), LOOP_DESCR_nestlevel(loop));
      CG_LOOP_Statistics(loop);
    }

    // Attempt to use updating loads/store in 'loop'.
    // this has to come before SWP which happens in CG_LOOP_Optimize.
    Loop_Updating_Optimization(loop);
    
    /* Apply fusion. */
    if (CG_fusion)
      CG_FUSION_Optimize_Loop(loop);
    
  }


  /* Determine the iteration count for each AT_REGION representing
     a loop, based on the BB execution counts. We want to do this
     before unrolling, SWP, etc. because we want an iteration
     count that corresponds to what will be seen by the vectorizer. */
  AUTOTIE_Set_Region_Exec_Counts();

  /* Collect the DFG for BB's in the PU before the loop optimizations
     are applied. We want the pre-loop-opt DFGs because those are what
     will be used then applying the fusions. */
  AUTOTIE_Build_DFGs();
    
  // we have to run CG_LOOP_Optimize() in separate pass because it may change
  // inner loop blocks (from SWP, unrolling, etc)
  // which causes stale bb sets for outer loop nests which
  // in turn causes memory problem when e.g. cg_fusion trying to work on outer loop nests
  //
  // the current arrangement is ok until we start heavily optimization for non-innermost loops

  for (LOOP_DESCR *loop = loops ; loop; loop = LOOP_DESCR_next(loop)) {
    BB *head = LOOP_DESCR_loophead(loop);
	  
    if (CG_LOOP_Skip(head)) {
      DevWarn("CG_LOOP skip BB%d.", BB_id(head));
      continue;
    }
      
    // prepare feedback based execution freq
    Feedback_Freq_Init ();

    /* Unroll, SWP, ZCL... add fixup requirement to 'fixup'. */
    CG_LOOP_Optimize(loop, fixup);

    Feedback_Freq_Finish ();
  }

#ifdef TARG_XTENSA
  if (Run_Autotie)
    {
      /* Collect autotie analysis information. We first want to find the
	 loops again because unrolling, SWP, etc. may create remainder
	 loops, or change what loops are the innermost ones. */
      Free_Dominators_Memory();
      Calculate_Dominators();
      LOOP_DESCR *at_loops = LOOP_DESCR_Detect_Loops(&loop_descr_pool);
      if (trace_general)
	{
	  fprintf(TFile, "<cgprep> Autotie_Analysis\n");
	  LOOP_DESCR_Print_List();
	}
      
      AUTOTIE_Analyze(at_loops);
    }
#endif

#ifdef HAS_ROTATING_REGISTERS
  // Compute correct wrap around values for SWP loops
  for (INT i = 0; i < fixup.size(); i++)
    SWP_Fixup(fixup[i]);

  if (Enable_SWP) {
    // Let GRA knows how many rotating register are needed!
    REGISTER_Request_Stacked_Rotating_Register();
  }
#endif
  
  MEM_POOL_Pop (&loop_descr_pool);
  MEM_POOL_Delete(&loop_descr_pool);

  Free_Dominators_Memory ();
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:


