
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


/*

  Modulo Scheduling

  Implemented by Raymond Lo, Apr 1999.

  The modulo scheduler is based on the paper.  Several modifications
  are made to reduce the searching time for some corner cases.

  Richard Huff,
  "Lifetime-Sensitive Modulo Scheduling",
  in Programming Language Design and Implementation. SIGPLAN, 1993. 
  http://www.cs.cornell.edu/Info/Projects/Bernoulli/home.html#PLDI93-2

*/

#define USE_STANDARD_TYPES
#include "defs.h"
#include <math.h>
#include "mempool.h"
#include "errors.h"
#include "tracing.h"
#include "op.h"
#include "cg_dep_graph.h"
#include "ti_res_res.h"
#include "ti_bundle.h"
#include "cg_loop.h"
#include "cg_loop_mii.h"
#include "matrix.h"
#include "cxx_template.h"
#include "cxx_memory.h"
#include "config_targ_options.h"	/* for xt_density */
#include "cg_swp.h"
#include "cg_swp_options.h"
#include "cg_swp_bundle.h"

#include <vector>
#include <utility>

using namespace std;

static bool Trace_Swp_Bundling = false;

typedef mempool_allocator<INT32>	INT32_MEMALLOC;
typedef MATRIX<INT32, INT32_MEMALLOC>	INT32_MATRIX;
typedef vector<INT32, INT32_MEMALLOC>	INT32_VECTOR;

void MinDist::Print(FILE *fp) const
{
  const int n_col = 16;
  fprintf(fp, "MinDist %dx%d:\n", size(), size());
  fprintf(fp, "     ");
  for (INT j = 0; j < size(); j++) {
    if (j != 0 && j % n_col == 0)
      fprintf(fp, "\n     ");
    fprintf(fp,"%4d", j);
  }
  fprintf(fp, "\n");
  for (INT i = 0; i < size(); i++) {
    fprintf(fp, "%3d: ", i);
    for (INT j = 0; j < size(); j++) {
      if (j != 0 && j % n_col == 0)
	fprintf(fp, "\n     ");
      fprintf(fp,"%4d", mindist[i][j]);
    }
    fprintf(fp, "\n");
  }
}

MinDist::MinDist(const SWP_OP_vector& v, INT start, INT stop, INT ii)
{
  int n_ops = v.size();
  mindist_size = n_ops;
  
  INT step = ii;
  INT ii_lower = ii;
  INT ii_try;

  while (1) {
    // ii >= ii_lower
    ii_try = ii_lower;
    while (Compute(v, start, stop, ii_try) > 0) {
      ii_lower = ii_try + 1;
      ii_try += step;
    }
    // Compute(v, start, stop, ii_try) == 0
    // ii >= ii_Lower && ii <= ii_try

    if (ii_lower == ii_try) {
      found_ii = ii_lower;
      return;
    }
    step = Max(1, step / 2);
  }
}


INT MinDist::Compute(const SWP_OP_vector& v, INT start, INT stop, INT ii)
{
  int n_ops = v.size();
  INT i, j, k;
  
  // initialization 
  //
  for (i = 0; i < n_ops; i++) 
    for (j = 0; j < n_ops; j++)
      mindist[i][j] = NEG_INF;

  for (i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op) {
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	// Skip all PREBR dependence because they will be taken care by
	// the SWP stage control predicates
	if (ARC_kind(arc) == CG_DEP_PREBR) 
	  continue;
	OP  *succ = ARC_succ(arc);

	// Skip loh successors
	if (OP_loh(succ))
	  continue;
	mindist[i][SWP_index(succ)] =
	  Max(mindist[i][SWP_index(succ)], ARC_latency(arc) - ARC_omega(arc) * ii);
	Is_True(succ == v[SWP_index(succ)].op, ("MinDIST: wrong SWP_index."));
      }
      mindist[start][i] = 0;
      mindist[i][stop] = 0;
    }
  }
  mindist[start][stop] = 0;

  // Floyd's all pairs shortest paths algorithm.
  // It is based on dynamic programming.
  for (k = 0; k < n_ops; k++) 
    for (i = 0; i < n_ops; i++) 
      for (j = 0; j < n_ops; j++) 
	mindist[i][j] = Max(mindist[i][j], mindist[i][k] + mindist[k][j]);

  for (i = 0; i < n_ops; i++) {
    if (mindist[i][i] > 0) 
      return mindist[i][i];
    else
      mindist[i][i] = 0;
  }
  return 0;
}


//************************************************************************
//   MinLT calculation
//     Assume the first result is the *important* result for all operation
//************************************************************************

class MinLT {
  vector<INT>  minlt;
public:
  INT size() const { return minlt.size(); }
  INT operator()(INT i) const { return minlt[i]; }
  void Print(FILE *fp) const;
  MinLT(const SWP_OP_vector& v, INT ii, const MinDist& mindist);
};

void MinLT::Print(FILE *fp) const
{
  fprintf(fp, "MinLT: ");
  for (INT i = 0; i < minlt.size(); i++) {
    if (minlt[i] >= 0) 
      fprintf(fp, "(%d,%d) ", i, minlt[i]);
  }
  fprintf(fp, "\n");
}

MinLT::MinLT(const SWP_OP_vector& v, INT ii, const MinDist& mindist)
  :minlt(v.size(), 0)
{
  for (INT i = 0; i < v.size(); i++) {
    minlt[i] = -1;
    OP *op = v[i].op;
    if (op) {
      minlt[i] = 1;
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  OP  *succ = ARC_succ(arc);

	  // Skip loh successors
	  if (OP_loh(succ))
	    continue;

	  INT succ_idx = SWP_index(succ);
	  if (OP_opnd(succ,ARC_opnd(arc)) == OP_result(op,0)) {
	    INT live_range = ARC_omega(arc) * ii + mindist(i,succ_idx);
	    minlt[i] = Max(minlt[i], live_range);
	  }
	}
      }
    }
  }
}


//************************************************************************
//   MinAvg calculation
//************************************************************************

INT MinAvg(INT ii, const MinLT& minlt) 
{
  INT minavg = 0;
  for (INT i = 0; i < minlt.size(); i++) {
    if (minlt(i) > 0) 
      minavg += minlt(i);
  }
  return (INT) ceil(minavg/ii);
}


//************************************************************************
//   Slack calculation
//************************************************************************

class Slack {
  vector<INT> estart;
  vector<INT> lstart;
  INT start;    // the START node index
  INT stop;     // the STOP node index

  
public:
  INT Start()           const { return start; }
  INT Stop()            const { return stop; }
  INT Estart(INT i) const     { return estart[i]; }
  INT Lstart(INT i) const     { return lstart[i]; }
  INT operator()(INT i) const { return lstart[i] - estart[i]; }
  void Verify() const;
  void Print(FILE *fp) const;
  bool Set_last_cycle(const SWP_OP_vector& v, INT last_cycle, const MinDist& mindist);
  void Relax_Precedence(const SWP_OP_vector& v, const vector<INT>& unplaced, const MinDist& mindist);
  void Relax_Precedence(const SWP_OP_vector& v, const INT unplaced_op, const MinDist& mindist);
  void Update_Slack_From_Placed_Ops(const SWP_OP_vector& v, const MinDist& mindist);
  bool Update_Slack_From_Placed_Op(INT candidate, 
				   const SWP_OP_vector& v, const MinDist& mindist);
  void Update_Slack_From_Op(INT candidate, 
				   const SWP_OP_vector& v, const MinDist& mindist);
  Slack(const SWP_OP_vector& v, INT start_idx, INT stop_index, INT ii, const MinDist& m);
};

// Verify that forall i 
//   0 <= estart[start] <= estart[i] <= lstart[i] <= lstart[stop]
void Slack::Verify() const 
{
  INT min_cycle = estart[start];
  INT max_cycle = lstart[stop];
  FmtAssert(min_cycle >= 0, ("Slack: min_cycle (%d) < 0", min_cycle));
  for (INT i = 0; i < estart.size(); i++) {
    FmtAssert(estart[i] <= lstart[i],
	      ("Slack: estart (%d) > lstart (%d) for OP %d\n",estart[i], lstart[i], i));
    FmtAssert(min_cycle <= estart[i], 
	      ("Slack: min_cycle (%d) > estart (%d) for OP %d\n", min_cycle, estart[i], i));
    FmtAssert(max_cycle >= lstart[i], 
	      ("Slack: max_cycle (%d) < lstart (%d) for OP %d\n", max_cycle, lstart[i], i));
  }
}

void Slack::Print(FILE *fp) const 
{
  for (INT i = 0; i < estart.size(); i++) {
    fprintf(fp, "[%d] e=%d l=%d s=%d\n", i, estart[i], lstart[i],
	    lstart[i] - estart[i]);
  }
}

bool Slack::Set_last_cycle(const SWP_OP_vector& v, INT last_cycle, const MinDist& mindist)
{
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op) {
      estart[i] = mindist(start, i);
      lstart[i] = last_cycle - mindist(i, stop);
    } else {
      estart[i] = 0;
      lstart[i] = 0;
    }
    if (estart[i]>lstart[i]) {
      return false;
    }
  }
  estart[start] = 0;
  lstart[start] = last_cycle - mindist(start, stop);
  estart[stop] = mindist(start, stop);
  lstart[stop] = last_cycle;
  return true;
}

void Slack::Relax_Precedence(const SWP_OP_vector& v, const INT unplaced_op,
			     const MinDist& mindist)
{
  vector<INT> unplaced_ops;
  unplaced_ops.push_back(unplaced_op);
  Relax_Precedence(v, unplaced_ops, mindist);
}
void Slack::Relax_Precedence(const SWP_OP_vector& v, const vector<INT>& unplaced,
			     const MinDist& mindist)
{
  vector<bool> processed(v.size(), false);
  vector<INT> need_process;
  {
    for (INT u = 0; u < unplaced.size(); u++) {
      INT i = unplaced[u];
      if (v[i].op)
	need_process.push_back(i);
    }
    while (need_process.size() > 0) {
      INT i = need_process.back();
      need_process.pop_back();
      processed[i] = true;
      ARC_LIST* al;
      for (al = OP_preds(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);

	// Skip loh predecessors
	if (OP_loh(pred))
	  continue;

	INT pred_idx = SWP_index(pred);
	if (!v[pred_idx].placed && !processed[pred_idx]) {
	  processed[pred_idx] = true;
	  need_process.push_back(pred_idx);
	}
      }
      for (al = OP_succs(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);

	// Skip loh successors
	if (OP_loh(succ))
	  continue;

	INT succ_idx = SWP_index(succ);
	if (!v[succ_idx].placed && !processed[succ_idx]) {
	  processed[succ_idx] = true;
	  need_process.push_back(succ_idx);
	}
      }
    }
  }

  // The number of unplaced OPs is usually small,
  // therefore process unplaced OPs in the outer loop.
  //
  INT last_cycle = lstart[stop];
  for (INT i=0; i < processed.size(); i++) {
    if (!processed[i]) continue;
    estart[i] = mindist(start, i);
    lstart[i] = last_cycle - mindist(i, stop);
    for (INT j = 0; j < v.size(); j++) {
      if (v[j].placed) {
	INT cycle = v[j].cycle;
	estart[i] = Max(estart[i], cycle + mindist(j, i));
	lstart[i] = Min(lstart[i], cycle - mindist(i, j));
      }
    }
  }
  
}

void Slack::Update_Slack_From_Op(INT candidate, 
					const SWP_OP_vector& v, const MinDist& mindist)
{
  INT cycle = v[candidate].cycle;
  for (INT i = 0; i < v.size(); i++) {
      estart[i] = Max(estart[i], cycle + mindist(candidate, i));
      lstart[i] = Min(lstart[i], cycle - mindist(i, candidate));
  }
}

bool Slack::Update_Slack_From_Placed_Op(INT candidate, 
					const SWP_OP_vector& v, const MinDist& mindist)
{
  INT cycle = v[candidate].cycle;
  bool satisfiable = true;
  for (INT i = 0; i < v.size(); i++) {
    if (!v[i].placed) {
      estart[i] = Max(estart[i], cycle + mindist(candidate, i));
      lstart[i] = Min(lstart[i], cycle - mindist(i, candidate));
      if (estart[i]>lstart[i])
	satisfiable = false;
    }
  }
  return satisfiable;
}

void Slack::Update_Slack_From_Placed_Ops(const SWP_OP_vector& v, const MinDist& mindist)
{
  INT last_cycle = lstart[stop];
  for (INT i = 0; i < v.size(); i++) {
    if (v[i].op && !v[i].placed) {
      estart[i] = mindist(start, i);
      lstart[i] = last_cycle - mindist(i, stop);
      for (INT j = 0; j < v.size(); j++) {
	if (v[j].placed) {
	  INT cycle = v[j].cycle;
	  estart[i] = Max(estart[i], cycle + mindist(j, i));
	  lstart[i] = Min(lstart[i], cycle - mindist(i, j));
	}
      }
    }
  }
}


Slack::Slack(const SWP_OP_vector& v, INT start_idx, INT stop_idx, INT ii, const MinDist &mindist)
  :start(start_idx),stop(stop_idx),estart(stop_idx+1,0),lstart(stop_idx+1,0)
{
  // A difference from Huff's paper to reduce backtracking.
  // The schedule length is at least the critical path length+2 and roundup to next ii
  // so each operation on the critical path has three cycles to schedule on.
  INT len = (INT) ceil(((double) mindist(start, stop) + 1) / ii) * ii;

  // disable the following because we want to schedule starting from a tight slack
#if 0
  if (mindist(start, stop) > 8) {
    len = Max(mindist(start,stop) + 1 + 2 /* slack==2 */, len);
    len = (INT) ceil(((double) len) / ii) * ii;
  }
#endif
  (void)Set_last_cycle(v, len-1, mindist);
}


//************************************************************************
//  Modulo Reservation Table (MRT)
//************************************************************************

class MRT {
  INT ii;
  INT grainy_resources_length;
  INT max_issue_alignment;
  TI_RES_RES *resources;

public:
  TI_RES_RES *Res() { return resources; }

  void Reserve_Op_Resources(const SWP_OP& swp_op, INT cycle) {
    TI_RES_RES_Reserve_Resources(resources, OP_code(swp_op.op), cycle, TOP_UNDEFINED);
  }
  void Unreserve_Op_Resources(const SWP_OP& swp_op) {
    TI_RES_RES_Unreserve_Resources(resources, OP_code(swp_op.op), swp_op.cycle, TOP_UNDEFINED);
  }
  bool Resources_Available(const SWP_OP& swp_op, INT cycle) const {
    return TI_RES_RES_Resources_Available(resources, OP_code(swp_op.op), cycle, TOP_UNDEFINED);
  }
  bool Resources_Grainy(const SWP_OP& swp_op) const {
    // return TI_RES_RES_Resources_Grainy(resources, OP_code(swp_op.op));
    return TI_RES_RES_Resources_Length(resources, OP_code(swp_op.op)) >= grainy_resources_length;
  }
  bool Resources_Equivalent(const SWP_OP& sop1, const SWP_OP& sop2) const {
    return
      sop1.op == sop2.op ||
      TI_RES_RES_Resources_Equivalent(resources, OP_code(sop1.op), OP_code(sop2.op));
  }
  bool Format_Compatible(const SWP_OP& sop, INT cycle) const {
    return TI_RES_RES_Compatible_Format(
		    resources, OP_code(sop.op), cycle, TOP_UNDEFINED) !=
	   (TI_SI_FORMAT_ID_SET)0;
  }
  bool Resources_Relevant(const SWP_OP& sop1, const SWP_OP& sop2) const {
    if (sop1.cycle < sop2.cycle) 
      return TI_RES_RES_Resources_Relevant(resources, OP_code(sop1.op), OP_code(sop2.op),
					   sop2.cycle - sop1.cycle);
    else
      return TI_RES_RES_Resources_Relevant(resources, OP_code(sop2.op), OP_code(sop1.op),
					   sop1.cycle - sop2.cycle);
  }
  void Verify() const {
    FmtAssert(! TI_RES_RES_Is_Bad_II(resources, ii), ("MRT: bad II."));
  }
  INT Find_Resources_In_Range(INT candidate, const SWP_OP_vector& v, 
			     INT earliest, INT latest, INT alignment_cycle, bool top_down) const;
  bool Verify2(const SWP_OP_vector& v);
  MRT(const SWP_OP_vector& v, INT _ii, MEM_POOL *pool);
};

INT MRT::Find_Resources_In_Range(INT candidate, const SWP_OP_vector& v, 
				INT earliest, INT latest, INT alignment_cycle, bool top_down) const {
  INT incr = top_down ? 1 : -1;
  INT begin = top_down ? earliest : latest;
  INT finish = top_down ? (latest+1) : (earliest-1);
  INT cycle;
  for (cycle = begin; cycle != finish; cycle += incr) {
    if (v[candidate].issue_alignment!=1 && alignment_cycle != -1 &&
	cycle%max_issue_alignment != alignment_cycle)
      continue;
    if (Resources_Available(v[candidate], cycle)) {
      if (!v.May_Stall(candidate, cycle))
        return cycle;
    }
  }
  return cycle;
}


bool MRT::Verify2(const SWP_OP_vector& v) 
{
  CXX_MEM_POOL local_mem_pool("MRT verify", FALSE);
  MRT mrt_verify(v, ii, local_mem_pool());
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op && v[i].placed)
      mrt_verify.Reserve_Op_Resources(v[i], v[i].cycle);
  }
  Is_True(TI_RES_RES_Equal(Res(), mrt_verify.Res()),
	    ("resource table inconsistent state."));
  if (!(TI_RES_RES_Equal(Res(), mrt_verify.Res())))
    return false;
  return true;
}


MRT::MRT(const SWP_OP_vector& v, INT _ii, MEM_POOL *pool):ii(_ii),max_issue_alignment(1)
{
  resources = TI_RES_RES_Alloc(TRUE /*cyclic*/, pool);
  for (INT i = 0; i < v.size(); i++) {
    OP *op = v[i].op;
    if (op)
      TI_RES_RES_Has_TOP(resources, OP_code(op));
    if (v[i].issue_alignment!=1 && max_issue_alignment==1)
      max_issue_alignment = TI_TOP_Issue_Alignment(OP_code(v[i].op));
  }
  TI_RES_RES_Set_BB_Cycle_Count(resources, ii);
  grainy_resources_length = SWP_Options.Grainy_Resources_Length;
}


//************************************************************************
//  The LifeTime modulo scheduler heuristcs:
//   - This heuristics is based on the paper by Richard A. Huff, SIGPLAN'93
//     "Lifetime-Sensitive Modulo Scheduling."
//************************************************************************
class LT_Heuristics {
  
  bool trace;
  bool trace_details;
  bool min_retry;
  vector<INT> unplaced;
  INT max_sched_length;
  INT ii;
  INT alignment_cycle;
  INT max_issue_alignment;
  INT num_aligned_op_placed;
  INT sched_len_changes;
  INT32 heuristics;
  INT32* order;

  const MinLT& minlt;
  
  // An operation should schedule 
  //   1) top-down if it has more stretchable inputs than outputs
  //   2) bottom-up if it has less stretchable inputs than outputs
  //   3) schedules closer to the placed operations,
  //       i.e., top-down if more predecessors are placed than successors
  //             bottom-up if more successors are placed than predecessors
  //   4) top-down if both it ties on both the lifetime and the 
  //      placed-operations criteria.
  // Note: it returns top-down if the earliest and latest start is identical and there
  // 	   is no direction specified for the candidate op.
  bool Sched_Top_Down(INT candidate, SWP_OP_vector& v, const Slack& slack)
  {
    if (v[candidate].direction == SWP_TOP_DOWN) return true;
    if (v[candidate].direction == SWP_BOTTOM_UP) return false;
    if (slack.Estart(candidate) == slack.Lstart(candidate)) return true;
    INT placed_preds = 0;
    INT placed_succs = 0;
    ARC_LIST* al;
    for (al = OP_preds(v[candidate].op) ; al; al = ARC_LIST_rest(al) ) {
      ARC *arc = ARC_LIST_first(al);
      OP *pred = ARC_pred(arc);

      // Skip loh predecessors
      if (OP_loh(pred))
	continue;

      INT pred_idx = SWP_index(pred);
      if (v[pred_idx].placed)
	placed_preds++;
    }
    for (al = OP_succs(v[candidate].op) ; al; al = ARC_LIST_rest(al) ) {
      ARC *arc = ARC_LIST_first(al);
      OP *succ = ARC_succ(arc);

      // Skip loh successors
      if (OP_loh(succ))
	continue;

      INT succ_idx = SWP_index(succ);
      if (v[succ_idx].placed)
	placed_succs++;
    }

    // Note: this is a derivation from Huff's paper.  In his paper, an operation's
    // schedule direction may change.  That might cause certain issue slot not
    // searched.  We fixed the direction of scheduling.
    if (placed_preds >= placed_succs) {
      v[candidate].direction = SWP_TOP_DOWN;
      return true;
    } else {
      v[candidate].direction = SWP_BOTTOM_UP;
      return false;
    }
  }

  void Recalculate_Top_Down(SWP_OP_vector& v, const Slack& slack)
  {
	// this is the same calculation as in Init_SWP_OP_state()
	// if this is called each time the schedule length is increased
	// the result would reflect the new Estart and Lstart values

    for (INT i = 0; i < v.size(); i++) {
      OP *op = v[i].op;
      if (!op) continue;

      if (SWP_Options.Sched_Direction == 1)
	v[i].direction = SWP_TOP_DOWN;
      else if (SWP_Options.Sched_Direction == 2)
	v[i].direction = SWP_BOTTOM_UP;
      else {
	// Initialize based on lifetime
	INT stretchable_inputs = 0;
	INT stretchable_outputs = 1;
	for (INT j = 0; j < OP_opnds(op); j++) {
	
	  // skip duplicated operands
	  for (INT k = j+1; k < OP_opnds(op); k++) {
	    if (OP_opnd(op, j) == OP_opnd(op, k))
	      goto next_opnd; 
	  }

	  ARC *arc;
	  arc = ARC_LIST_Find_First(OP_preds(op), CG_DEP_REGIN, j);
	  if (arc == NULL)	  // skip invariants
	    goto next_opnd;
	  if (ARC_pred(arc) == op)  // skip self recurrence
	    goto next_opnd;

	  // Determine if input is stretchable?
	  INT pred_op_idx;
	  OP* pred;
	  pred = ARC_pred(arc);

	  // Skip loh predecessors
	  if (OP_loh(pred))
	    continue;

	  pred_op_idx = SWP_index(pred);
	  if (slack.Estart(pred_op_idx) + minlt(pred_op_idx) < 
	      ARC_omega(arc) * ii + slack.Lstart(i))
	    stretchable_inputs++;

	next_opnd: ;
	}

	// TODO: skip predicated definitions from stretchable outputs
	// TODO: skip predicate TNs because there are plenty of predicate registers
	if (stretchable_inputs > stretchable_outputs)
	  v[i].direction = SWP_TOP_DOWN;
	else if (stretchable_inputs < stretchable_outputs)
	  v[i].direction = SWP_BOTTOM_UP;
	else
	  v[i].direction = SWP_UNKOWN;
      }
    }
  }

public:
  
  //  Initialize an operation's static properties:
  //    1) priority scale based on critical or grainy resources
  //    2) schedule direction based on stretchable inputs and outputs
  //
  // Note: randomize the priority and the schedule-direction can help
  // to force more backtracking and helps to flush the bugs!  -Raymond
  //
  void Init_SWP_OP_state(SWP_OP_vector& v,
			 const Slack& slack, const MRT& mrt,
			 bool mop_critical, bool flop_critical)
  {
    INT start = v.start;
    INT stop  = v.stop;
    
    // Place the START and STOP node
    v[start].placed = true;
    v[start].cycle = slack.Estart(start);
    v[stop].placed = true;
    v[stop].cycle = slack.Lstart(stop);
    max_sched_length = slack.Lstart(stop) + SWP_Options.Max_Schedule_Incr;

    for (INT i = 0; i < v.size(); i++) {
      OP *op = v[i].op;
      if (!op) continue;

      v[i].placed = false;
      v[i].cycle = 0;
      v[i].scale = 1.0;
      v[i].trials = 0;

      // Initialize based on resources
      if (mrt.Resources_Grainy(v[i]))
	v[i].scale *= 0.5;

      // critical resource heuristics
      if (mop_critical && OP_memory(op))
	v[i].scale *= 0.1;
      if (flop_critical && OP_flop(op))
	v[i].scale *= 0.2;

      if (SWP_Options.Sched_Direction == 1)
	v[i].direction = SWP_TOP_DOWN;
      else if (SWP_Options.Sched_Direction == 2)
	v[i].direction = SWP_BOTTOM_UP;
      else {
	// Initialize based on lifetime
	INT stretchable_inputs = 0;
	INT stretchable_outputs = 1;
	for (INT j = 0; j < OP_opnds(op); j++) {
	
	  // skip duplicated operands
	  for (INT k = j+1; k < OP_opnds(op); k++) {
	    if (OP_opnd(op, j) == OP_opnd(op, k))
	      goto next_opnd; 
	  }

	  ARC *arc;
	  arc = ARC_LIST_Find_First(OP_preds(op), CG_DEP_REGIN, j);
	  if (arc == NULL)	  // skip invariants
	    goto next_opnd;
	  if (ARC_pred(arc) == op)  // skip self recurrence
	    goto next_opnd;

	  // Determine if input is stretchable?
	  INT pred_op_idx;
	  OP* pred;
	  pred = ARC_pred(arc);

	  // Skip loh predecessors
	  if (OP_loh(pred))
	    continue;

	  pred_op_idx = SWP_index(pred);
	  if (slack.Estart(pred_op_idx) + minlt(pred_op_idx) < 
	      ARC_omega(arc) * ii + slack.Lstart(i))
	    stretchable_inputs++;

	next_opnd: ;
	}

	// TODO: skip predicated definitions from stretchable outputs
	// TODO: skip predicate TNs because there are plenty of predicate registers
	if (stretchable_inputs > stretchable_outputs)
	  v[i].direction = SWP_TOP_DOWN;
	else if (stretchable_inputs < stretchable_outputs)
	  v[i].direction = SWP_BOTTOM_UP;
	else
	  v[i].direction = SWP_UNKOWN;
      }
    }
  }

  //  Choose an operation based on slack * scale.
  //  Return -1 if none is found.
  INT Choose_Op(const SWP_OP_vector& v, const Slack& slack)
  {
    INT candidate = -1;
    double highest_priority = 100000;
    switch (heuristics) {
    case 1:
      {
	// based on given schedulign order only
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  INT pri = order[i];
	  if (pri < highest_priority) {
	    highest_priority = pri;
	    candidate = i;
	  }
	}
	break;
      }
    case 2:
      {
	// based on slack, scaled by critical resources
	//
	INT candidate_lstart = 0;
	INT trials = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  // so that slack == 0 can still be distinguish by scale!
	  INT sl = (INT)slack(i);
	  if (v[i].issue_alignment!=1) {
	    INT new_sl = sl;
	    INT a_cycle = alignment_cycle;
	    if (a_cycle== -1) a_cycle = 0;
	    INT l_cycle = slack.Lstart(i);
	    INT e_cycle = slack.Estart(i);
	    if (e_cycle < a_cycle) e_cycle = a_cycle;
	    if (l_cycle<e_cycle)
	      new_sl=0;
	    else if (l_cycle==e_cycle)
	      new_sl = 1;
	    else {
	      new_sl =
		    (l_cycle-a_cycle)/max_issue_alignment -
		    (e_cycle-a_cycle)/max_issue_alignment + 1;
	    }
	    if (new_sl<sl)
	      sl = new_sl;
	  }
	  INT pri = (INT) (sl * v[i].scale);
	  if (pri < highest_priority ||
	      pri == highest_priority && 
	      (v[i].trials < trials ||
	       (v[i].trials == trials &&
		slack.Lstart(i) < candidate_lstart))) {
	    highest_priority = pri;
	    candidate_lstart = slack.Lstart(i);
	    trials = v[i].trials;
	    candidate = i;
	  }
	}
	break;
      }
    case 3:
      {
	// operation with smallest Lstart are scheduled first
	//
	INT candidate_slack = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  INT pri = slack.Lstart(i);
	  if (pri < highest_priority ||
	      pri == highest_priority && slack(i) < candidate_slack) {
	    highest_priority = pri;
	    candidate_slack = slack(i);
	    candidate = i;
	  }
	}
	break;
      }
    case 4:
      {
	// operation with largest Estart are scheduled first
	//
	INT last_cycle = slack.Lstart(slack.Stop());
	INT candidate_slack = 0;
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  INT pri = last_cycle - slack.Estart(i);
	  if (pri < highest_priority ||
	      pri == highest_priority && slack(i) < candidate_slack) {
	    highest_priority = pri;
	    candidate_slack = slack(i);
	    candidate = i;
	  }
	}
	break;
      }
    case 5:
      {
	// choose the first un-placed op in forward order
	//
	for (INT i = 0; i < v.size(); i++) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  candidate = i;
	  break;
	}
	break;
      }
    case 6:
      {
	// choose the first un-placed op in reverse order
	//
	for (INT i = v.size(); i>=0; i--) {
	  if (v[i].op == NULL) continue;
	  if (v[i].placed) continue;
	  if (i == slack.Start()) continue;
	  if (i == slack.Stop()) continue;
	  candidate = i;
	  break;
	}
	break;
      }
    default:
	Is_True(false,("Unknown SWP heuristics"));
    }
    return candidate;
  }

  //  Choose An issue cycle
  //   - Return value is a pair<bool, bool>
  //   - The first bool is set to TRUE if precedence constraints might be violated
  //   - The second bool is set to TRUE if resource constraints might be violated.
  pair<bool, bool>
  Choose_Issue_Cycle(INT candidate, SWP_OP_vector& v,
		     const Slack& slack, const MRT& mrt)
  {
    v[candidate].trials++;
    v[candidate].placed = true;
    INT earliest = slack.Estart(candidate);
    INT latest   = Min(earliest + ii - 1, slack.Lstart(candidate));
    bool top_down = Sched_Top_Down(candidate, v, slack);
    INT cycle;

    Is_True(earliest < 128 * ii, ("SWP Choose_Issue_Cycle: earliest=%d\n", earliest));
    Is_True(latest < 128 * ii, ("SWP Choose_Issue_Cycle: latest=%d\n", latest));

    // If the candidate has previous trial and there is resource for the next slot
    // try to use the next slot instead retry from beginning!
    if (min_retry && v[candidate].trials > 1) {
      INT e = top_down ? Max(earliest, v[candidate].cycle+1) : earliest;
      INT l = top_down ? latest : Min(latest, v[candidate].cycle-1);
      if (e <= l) {
	cycle = mrt.Find_Resources_In_Range(candidate, v, e, l, alignment_cycle, top_down);
	if (e <= cycle && cycle <= l) {
	  v[candidate].cycle = cycle;
	  if (v[candidate].issue_alignment!=1) {
	    num_aligned_op_placed++;
	    if (alignment_cycle == -1)
	      alignment_cycle = cycle % max_issue_alignment;
	  }
	  return pair<bool,bool>(false, false);
	}
      }
    }
    // else retry the whole range

    if (earliest <= latest) {
      cycle = mrt.Find_Resources_In_Range(candidate, v, earliest, latest, alignment_cycle, top_down);
      if (earliest <= cycle && cycle <= latest) {
        v[candidate].cycle = cycle;
	if (v[candidate].issue_alignment!=1) {
	  num_aligned_op_placed++;
	  if (alignment_cycle == -1)
	    alignment_cycle = cycle % max_issue_alignment;
	}
        return pair<bool,bool>(false, false);
      }
    }

    // unable to find available resources in the issue range,
    //  choose an issue slot.
#if 0
    if (earliest <= latest) {
      if (v[candidate].trials == 1)
        v[candidate].cycle = top_down ? earliest : latest;
      else 
        top_down ? ++v[candidate].cycle : --v[candidate].cycle;
    } else
#endif
    cycle = top_down ? latest+1 : earliest-1;
    if (v[candidate].issue_alignment!=1) {
      num_aligned_op_placed++;
      if (alignment_cycle == -1)
	alignment_cycle = cycle % max_issue_alignment;
      else {
	INT adjustment;
	if (top_down) {
	  adjustment = (alignment_cycle - cycle%max_issue_alignment);
	  cycle += adjustment;
	} else {
	  adjustment = (cycle%max_issue_alignment - alignment_cycle);
	  cycle -= adjustment;
	}
      }
    }

    v[candidate].cycle = cycle;
    return pair<bool,bool>(v[candidate].cycle < earliest || v[candidate].cycle > latest
			   /*has precedence conflicts*/,
			   !mrt.Resources_Available(v[candidate], v[candidate].cycle) ||
			   v.May_Stall(candidate,cycle)
			   /*has resources conflicts*/);
  }

  // Eject operation that violates the precedence constraints with
  // the candidate OP.
  void Eject_Precedence_Conflict_OPs(INT candidate, SWP_OP_vector& v, 
				     const Slack& slack, const MinDist& mindist, MRT& mrt)
  {
    insert_iterator<vector<INT> > iter(unplaced, unplaced.end());
    INT sched_cycle = v[candidate].cycle;
    for (INT i = 0; i < v.size(); i++) {
      if (v[i].placed) {
	INT estart = sched_cycle + mindist(candidate,i);
	INT lstart = sched_cycle - mindist(i, candidate);
	if (v[i].cycle < estart || v[i].cycle > lstart) {
	  *iter = i;
	  v[i].placed = false;
	  if (v[i].issue_alignment!=1 && --num_aligned_op_placed==0)
	    alignment_cycle = -1;
	  if (trace_details)
	    fprintf(TFile, "  eject OP %d due to precedence constraints.\n", i);
	}
      }
    }
  }

  //  Eject OPs with reources conflicts
  void Eject_Resources_Conflict_OPs(INT candidate, SWP_OP_vector& v, MRT& mrt)
  {
    insert_iterator<vector<INT> > ins(unplaced, unplaced.end());
    INT sched_cycle = v[candidate].cycle;
    bool ops_unplaced = false;
    for (INT i = 0; i < v.size(); i++) {
      bool eject = false;
      if (v[i].placed &&
	  v[i].op &&
	  i != candidate) {
	if ((v[i].cycle - sched_cycle) % ii == 0) {
	  eject = true;
	  if (trace_details)
	    fprintf(TFile, "  eject OP %d due to same cycle.\n", i);
	}
	
	if (!eject && mrt.Resources_Relevant(v[i], v[candidate])) {
	    eject = true;
	    if (trace_details)
	      fprintf(TFile, "  eject OP %d due to relevant resources.\n", i);
	}

	if (!eject && v.May_Stall(i, candidate,sched_cycle)) {
	    eject = true;
	    if (trace_details)
	      fprintf(TFile, "  eject OP %d due to potential stall.\n", i);
	}
	if (eject) {
	  *ins = i;
	  v[i].placed = false;
	  if (v[i].issue_alignment!=1 && --num_aligned_op_placed==0)
	    alignment_cycle = -1;
	  ops_unplaced = true;
	}
      }
    }
    Is_True(ops_unplaced, ("Eject_Resources_Conflict_OPs: cannot eject."));
  }

  //  Update Resources Requirements
  void Update_Resources(INT candidate, SWP_OP_vector& v, MRT& mrt)
  {
    for (INT u = 0; u < unplaced.size(); u++) {
      INT i = unplaced[u];
      if (v[i].op)
	mrt.Unreserve_Op_Resources(v[i]);
    }
    Is_True(mrt.Resources_Available(v[candidate], v[candidate].cycle),
	    ("Update_Reources: cannot eject enough resources for OP %d.", candidate));
    mrt.Reserve_Op_Resources(v[candidate], v[candidate].cycle);
  }

  //  Update Precedence Requirements
  bool Update_Precedence(INT candidate, SWP_OP_vector& v, Slack& slack, const MinDist& mindist, MRT& mrt)
  {

    if (unplaced.size() > 0) 
      slack.Relax_Precedence(v, unplaced, mindist);
    slack.Update_Slack_From_Placed_Op(candidate, v, mindist);
    
    INT start = slack.Start();
    INT stop = slack.Stop();
    
    if (!v[start].placed || !v[stop].placed) {
#pragma mips_frequency_hint NEVER
      INT adjustment = Max(slack.Estart(start) - slack.Lstart(start), 0);
      INT sched_len = Max(slack.Estart(stop), slack.Lstart(stop) + adjustment);
      if (sched_len > max_sched_length)
	return false;
      sched_len_changes++;
      if (trace) 
	fprintf(TFile, "Increase sched_len to %d.  Adjust start by %d.\n", sched_len, adjustment); 
      if (sched_len_changes%8 == 0) {
        for (INT i = 0; i < v.size(); i++) {
	  OP *op = v[i].op;
	  if (op && v[i].placed) {
	    mrt.Unreserve_Op_Resources(v[i]);
	    v[i].placed = false;
	    if (v[i].issue_alignment!=1 && --num_aligned_op_placed==0)
	      alignment_cycle = -1;
	    if (trace) 
	      fprintf(TFile, "  eject OP %d due to new sched_len\n", i);
	  }
        }
      } else {
        if (adjustment > 0) {
	  INT i;
	  for (i = 0; i < v.size(); i++) {
	    OP *op = v[i].op;
	    if (op && v[i].placed) {
	      mrt.Unreserve_Op_Resources(v[i]);
	      v[i].cycle += adjustment;
	      if (trace) 
	        fprintf(TFile, "Adjust OP %d to cycle %d\n", i, v[i].cycle);
	    }
	  }
	  for (i = 0; i < v.size(); i++) {
	    OP *op = v[i].op;
	    if (op && v[i].placed)
	      mrt.Reserve_Op_Resources(v[i], v[i].cycle);
	  }
	  if (alignment_cycle != -1) {
	    alignment_cycle = (alignment_cycle+adjustment)%max_issue_alignment;
	  }
        }
      }
      (void)slack.Set_last_cycle(v, sched_len, mindist);
      slack.Update_Slack_From_Placed_Ops(v, mindist);
      v[start].placed = true;
      v[start].cycle = slack.Estart(start);
      v[stop].placed = true;
      v[stop].cycle = slack.Lstart(stop);
      Recalculate_Top_Down(v, slack);
    }
    unplaced.erase(unplaced.begin(), unplaced.end());
    return true;
  }

  void Print(FILE *fp) {
    if (unplaced.size() > 0) {
      fprintf(fp, "unplaced Ops: ");
      for (INT i = 0; i < unplaced.size(); i++)
	fprintf(fp, "%d ", unplaced[i]);
      fprintf(fp, "\n");
    }
  }

  INT Allocatable(SWP_OP_vector& v,
		  INT ii,
		  const INT variant_regs[],
		  MEM_POOL* pool) const;

  LT_Heuristics(INT i, INT alignment, const MinLT& l, INT32 h, INT32* o,
		bool t, bool d):
	  ii(i), sched_len_changes(0), max_issue_alignment(alignment),
  	  alignment_cycle(-1), num_aligned_op_placed(0),
  	  minlt(l), heuristics(h), order(o),
	  trace(t), trace_details(d), min_retry(SWP_Options.Min_Retry) {}
};

INT LT_Heuristics::Allocatable(
		SWP_OP_vector& v, INT ii, const INT variant_regs[],
		MEM_POOL* pool) const
{
    INT retval = -1;
    MEM_POOL_Push(pool);
    /* There must be an extra scope here so that
       the C++ templates are destroyed before the pop. */
  {
    INT32_MATRIX cycle2op(ii, 
			  TI_ISA_Max_Num_Slots(),
			  -1, 
			  INT32_MATRIX::allocator_type(pool));
    INT32_VECTOR numops(ii,
			0,
			INT32_VECTOR::allocator_type(pool));

    TN_SET* invariants = v.tn_invariants;
    hTN_MAP32 allocated_map = hTN_MAP32_Create(pool);
    int i;
    INT regs[TI_ISA_REGCLASS_MAX+1];
    ISA_REGCLASS rc;
    FOR_ALL_ISA_REGCLASS(rc) {
      regs[rc] = variant_regs[rc];
    }

    for (i = 0; i < v.size(); ++i)
    {
      if (v[i].op != NULL)
      {
	const INT32 mcycle = v[i].cycle % ii;
	v[i].modulo_cycle = mcycle;
	cycle2op(mcycle, numops[mcycle]++) = i;
      }
    }

    if (trace) {
      fprintf(TFile,"Checking allocatability for modulo_schedule:\n");
      v.ii = ii;
      v.Print_Modulo_Schedule(TFile);
    }
    for (int cycle = 2*ii-1; cycle>=0; cycle--) {
      int i = cycle % ii;
      for (int j=0; j<numops[i]; j++) {
	OP* op = v[cycle2op(i,j)].op;
	if (op) {
	  for (int k=0; k< OP_results(op); k++) {
	    TN* tn = OP_result(op, k);
	    if (TN_is_register(tn) && !TN_is_dedicated(tn) && 
		!TN_SET_MemberP(invariants, tn)) {
	      int allocated =  hTN_MAP32_Get(allocated_map, tn);
	      if (allocated) {
		hTN_MAP32_Set(allocated_map, tn, 0);
		rc = TN_register_class(tn);
		regs[rc]++;
	      }
	    }
	  }
	}
      }
      for (int j=0; j<numops[i]; j++) {
	OP* op = v[cycle2op(i,j)].op;
	if (op) {
	  for (int k=0; k< OP_opnds(op); k++) {
	    TN* tn = OP_opnd(op, k);
	    if (TN_is_register(tn) && !TN_is_dedicated(tn) && 
		!TN_SET_MemberP(invariants, tn)) {
	      int allocated =  hTN_MAP32_Get(allocated_map, tn);
	      if (!allocated) {
		hTN_MAP32_Set(allocated_map, tn, 1);
		rc = TN_register_class(tn);
		if (regs[rc]==0) {
		  int candidate = cycle2op(i,j);
		  if (trace_details) {
		    fprintf(TFile,
			"Unable to allocate register for TN%d of OP %d"
		        " at cycle %d\n",
			TN_number(tn), candidate, i);
		  }
                  retval = candidate;
                  goto exit;
		  /* we are simulating "return candidate;" here
                     but must do it with a goto and scopes to
                     get the destructors run in the right order
                     with the mempool. */
		}
		regs[rc]--;
	      }
	    }
	  }
	}
      }
    }
  }
exit:
    MEM_POOL_Pop(pool);
    return retval;
}

bool Modulo_Schedule_Verify(SWP_OP_vector &v, INT ii, MRT& mrt)
{
  // Verify Resources
  if (!mrt.Verify2(v))
    return false;

    // Verify schedule
  for (INT i = 0; i < v.size(); i++) {
    if (v[i].op) {
      FmtAssert(v[i].placed, ("op is not placed."));
      INT sched_cycle = v[i].cycle;
      ARC_LIST *al;
      for (al = OP_preds(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);

	// Skip loh predecessors
	if (OP_loh(pred))
	  continue;

	INT pred_idx = SWP_index(pred);
	Is_True(sched_cycle - v[pred_idx].cycle >= ARC_latency(arc) - ARC_omega(arc) * ii,
		  ("OP %d at cycle %d and OP %d at cycle %d violated precedence constraints of %d cycles.",
		   pred_idx, v[pred_idx].cycle, i, v[i].cycle, ARC_latency(arc)));
	if (!(sched_cycle - v[pred_idx].cycle >= ARC_latency(arc) - ARC_omega(arc) * ii))
	  return false;
      }
      for (al = OP_succs(v[i].op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);

	// Skip loh successors
	if (OP_loh(succ))
	  continue;

	INT succ_idx = SWP_index(succ);
	Is_True((v[succ_idx].cycle - sched_cycle >= ARC_latency(arc) - ARC_omega(arc) * ii) ||
		  ARC_is_marked(arc),
		  ("OP %d at cycle %d and OP %d at cycle %d violated precedence constraints of %d cycles.",
		   i, v[i].cycle, succ_idx, v[succ_idx].cycle, ARC_latency(arc)));
	if (!((v[succ_idx].cycle - sched_cycle >= ARC_latency(arc) - ARC_omega(arc) * ii) || ARC_is_marked(arc)))
	  return false;
      }
    }
  }
  return true;
}


// get the stage count from the successful scheduling which failed register allocation
int Modulo_Schedule_Get_Stage_Count(SWP_OP_vector &v, INT ii)
{
  // the following is copied from Modulo_Schedule_Succeeded() below
  INT max_cycle;
  INT min_cycle;
  min_cycle = 1000;
  max_cycle = 0;

  for (INT i = 0; i < v.size(); i++) {

    if (v[i].op && !OP_dummy(v[i].op)) {
      min_cycle = Min(min_cycle, v[i].cycle);
      max_cycle = Max(max_cycle, v[i].cycle);
    }
  }

  max_cycle -= min_cycle;

  return (max_cycle / ii + 1);

}

void Modulo_Schedule_Succeeded(SWP_OP_vector &v,
			       INT ii, const MinDist& mindist,
			       MRT& mrt, bool trace)
{
#ifdef Is_True_On
  Modulo_Schedule_Verify(v, ii, mrt);
#endif

  // Produce Statistics
  v.succeeded = true;
  v.min_ii = CG_LOOP_min_ii;
  v.res_min_ii = CG_LOOP_res_min_ii;
  v.rec_min_ii = CG_LOOP_rec_min_ii;
  v.ii = ii;
  v.ii_slots = ii*TI_ISA_Max_Num_Slots();
  v.min_sl = mindist(v.start,v.stop)+1;
  
  // shift the schedule so the min_cycle is 0
  INT max_cycle;
  INT min_cycle;
  min_cycle = 1000;
  max_cycle = 0;

  for (INT i = 0; i < v.size(); i++) {

    if (v[i].op && OP_dummy(v[i].op))
      v[i].op = NULL;  // remove dummy ops 

    if (v[i].op) {
      min_cycle = Min(min_cycle, v[i].cycle);
      max_cycle = Max(max_cycle, v[i].cycle);

      // reset the bundling info which can come from
      // last scheduling which failed allocation
      OP* op = v[i].op;
      Set_OP_format_num(op,-1);
      Set_OP_slot_num(op,-1);
      Reset_OP_end_group(op);
    }
  }

  INT adjustment = min_cycle;
  INT mod_cycle_adjustment = adjustment % ii;

  for (INT i = 0; i < v.size(); i++) {
    v[i].cycle -= adjustment;
  }

  // min_cycle -= adjustment; has to be zero now
  max_cycle -= adjustment;

  v[v.start].cycle = 0;
  v[v.stop].cycle = max_cycle;

  v.sc = max_cycle / ii + 1;
  v.sl = max_cycle + 1;

  if (!v.is_doloop) {
    FmtAssert(FALSE,("SWP check for doloop failed"));
  }

  // Group and bundle the ops such that they can be issued in one cycle.  
  // this replaces the need for SWP_Bundle().

  MEM_POOL bundle_pool;

  Trace_Swp_Bundling = trace;

  MEM_POOL_Initialize(&bundle_pool, "CG_SWP_BUNDLE POOL", FALSE);
  MEM_POOL_Push(&bundle_pool);
  {
    // Define a matrix mapping each modulo cycle into the set of ops to be
    // executed at that cycle. The set of ops may have at most 
    // TI_MAX_Num_Slot(), and cycle2op will assert if
    // we go beyond that number of entries for any set.
    //
    const INT32  empty_slot = -1;
    INT32        i;
    INT32_MATRIX cycle2op(v.ii, 
			  TI_ISA_Max_Num_Slots(),
			  empty_slot, 
			  INT32_MATRIX::allocator_type(&bundle_pool));
    INT32_VECTOR numops(v.ii,
			0,
			INT32_VECTOR::allocator_type(&bundle_pool));

    INT32* format_last_used = CXX_NEW_ARRAY(INT32, TI_ISA_Num_Bundles(), &bundle_pool);

    for (i=0; i<TI_ISA_Num_Bundles(); i++) {
      format_last_used[i] = -1;
    }

    // First put each op into the first available slot in the cycle2ops map,
    // and while doing so also set the modulo_cycle for each op
    //
    for (i = 0; i < v.size(); ++i)
    {
      if (v[i].op != NULL)
      {
	const INT32 mcycle = v[i].cycle % v.ii;
	v[i].modulo_cycle = mcycle;
	cycle2op(mcycle, numops[mcycle]++) = i;
      }
    }

    INT32 slot_op[TI_TIE_SLOTS_MAX];	/* op assignment for each slot */
    INT32 op_slot[TI_TIE_SLOTS_MAX];	/* slot assignment for each op */
    INT32 last_cycle_using_shared_functional_unit = -1;
    INT32 last_cycle_num_slots = -1;

    for (INT32 c = 0; c < v.ii; c++)
    {
      if (numops[c] > 0)
      {
	MEM_POOL_Push(&bundle_pool);

	TI_RES_RES *res = mrt.Res();
	INT orig_cycle = c + mod_cycle_adjustment;
	TI_SI_FORMAT_ID_SET valid_formats =
		TI_RES_RES_Valid_Formats(res,orig_cycle);

	/* op assignment for each slot for the last cycle */
	INT32 last_slot_op[TI_TIE_SLOTS_MAX];
	INT32 num_ops = numops[c];
	INT32 num_slots = 0;

	INT32 i;
	bool use_shared_functional_unit = false;
	for (i=0; i<num_ops; i++) {
	  if (TI_TOP_Use_Shared_Functional_Unit(OP_code(v[cycle2op(c,i)].op))) {
	    use_shared_functional_unit = true;
	    break;
	  }
	}

	INT fmt = -1;
	INT min_fmt_num_bytes = 999;
	INT min_fmt_last_used = INT_MAX;
	for (i=0; i<TI_ISA_Num_Bundles(); i++) {
	  if (valid_formats & (1LL<<i)) {
	    if (use_shared_functional_unit) {
	      if (format_last_used[i]<min_fmt_last_used) {
	        fmt = i;
	        min_fmt_last_used = format_last_used[i];
	      }
	    } else {
	      if (TI_ISA_Bundle_Bytes(i)<min_fmt_num_bytes) {
	        fmt = i;
	        min_fmt_num_bytes = TI_ISA_Bundle_Bytes(i);
	      }
	    }
	  }
	}

	FmtAssert(fmt!= -1, ("Fail to find valid format"));

	format_last_used[fmt] = c;

	// find slot assignment using format 'fmt'
	num_slots = TI_ISA_Num_Slots(fmt);
	for (i=0; i<num_slots; i++) {
	  slot_op[i] = -1;
	  op_slot[i] = -1;
	}

	INT32 num_possible_slotting = 0;
	INT32 slotting[8][TI_TIE_SLOTS_MAX];

	// the following loop will backtrack and exhaustively search all
	// possible slot available for each operation
	for (i=0; i<num_ops; i++) {
	  INT32 j = op_slot[i] + 1;
	  while (j<num_slots) {
	    if (slot_op[j] == -1) {
	      TOP opc = OP_code(v[cycle2op(c,i)].op);
	      if (opc==TOP_mov_n && !xt_density)
		opc=TOP_or;
	      ISA_EXEC_UNIT_PROPERTY opc_prop = TI_ISA_Exec_Unit_Prop(opc);
	      ISA_EXEC_UNIT_PROPERTY slot_prop = TI_ISA_Exec_Slot_Prop(fmt,j);
	      if ((opc_prop & slot_prop) !=0) {
	        slot_op[j] = i;
	        op_slot[i] = j;
	        break;
	      }
	    }
	    j++;
	  }
	  if (j==num_slots) {
	    if (i!=0) {
	      op_slot[i] = -1;
	      slot_op[op_slot[i-1]] = -1;
	      i = i-2;
	    } else
	      break;	// got all possible slotting
	  } else if (i==num_ops-1) {
	    // found a slotting
	    for (int s=0; s<num_slots; s++) {
	      slotting[num_possible_slotting][s] = slot_op[s];
	    }
	    num_possible_slotting++;

	    if (!use_shared_functional_unit) // no need to collect more
	      break;

	    // collect at most 8 slottings
	    if (num_possible_slotting<8) {
	      slot_op[op_slot[i]] = -1;
	      i--;
	    }
	  }
	}

	FmtAssert(num_possible_slotting>0,("SWP bundling fails"));

	if (last_cycle_using_shared_functional_unit>=0 && use_shared_functional_unit &&
	    num_possible_slotting>1) {
	  // select best slotting
	  int best_slotting = 0;
	  int best_num_conflicts = 999;
	  for (int k=0; k<num_possible_slotting; k++) {
	    int num_conflicts = 0;
	    for (int s=0; s<num_slots; s++) {
	      if (s>=last_cycle_num_slots || last_slot_op[s]== -1 ||
		  slotting[k][s]== -1)
		continue;

	      OP* op_last =
		 v[cycle2op(last_cycle_using_shared_functional_unit,last_slot_op[s])].op;
	      OP* op_current = v[cycle2op(c,slotting[k][s])].op;
	      if (TI_TOP_Use_Shared_Functional_Unit(OP_code(op_last)) &&
		  TI_TOP_Use_Shared_Functional_Unit(OP_code(op_current)))
		num_conflicts++;
	    }
	    if (num_conflicts<best_num_conflicts) {
	      best_slotting = k;
	      best_num_conflicts = num_conflicts;
	    }
	  }

	  for (int s=0; s<num_slots; s++) {
	    slot_op[s] = slotting[best_slotting][s];
	  }
	} else {
	  for (int s=0; s<num_slots; s++) {
	    slot_op[s] = slotting[0][s];
	  }
	}

	OP* last_op = NULL;
	for (i=0; i<num_slots; i++) {
	  INT32 j = slot_op[i];
	  if (j!= -1) {
	    OP* op = v[cycle2op(c,j)].op;
	    Set_OP_bundled(op);
	    Set_OP_format_num(op,fmt);
	    Set_OP_slot_num(op,i);
	    v[cycle2op(c,j)].slot = c * TI_ISA_Max_Num_Slots() + i;
	    last_op = op;
	  } else {
	    // insert a no-op
	    //
	    ISA_EXEC_UNIT_PROPERTY unit = TI_ISA_Exec_Slot_Prop(fmt,i);
	    const TOP ntop     = TI_ISA_Noop_Top(unit);
	    OP       *noop     = Mk_OP(ntop);
	    CG_LOOP_Init_Op(noop);
	    SWP_OP    swp_noop = v[cycle2op(c,0)]; // Similar attributes
	    Set_OP_format_num(noop,fmt);
	    Set_OP_slot_num(noop,i);
	    swp_noop.op = noop;
	    swp_noop.is_noop = TRUE;
	    swp_noop.slot = c * TI_ISA_Max_Num_Slots() + i;
	    v.push_back(swp_noop);
	    BB_Insert_Op(OP_bb(v[0].op), NULL, noop, TRUE);
	    OP_scycle(noop) = -1;
	    Set_OP_bundled(noop);
	    last_op = noop;
	  }
	}
	Set_OP_end_group(last_op);
	MEM_POOL_Pop(&bundle_pool);

	if (use_shared_functional_unit) {
	  last_cycle_using_shared_functional_unit = c;
	  last_cycle_num_slots = num_slots;
	  for (i=0; i<num_slots; i++)
	    last_slot_op[i] = slot_op[i];
	}
      }
    }
    
  }
  
  MEM_POOL_Pop(&bundle_pool);
  MEM_POOL_Delete(&bundle_pool);

}

static bool
schedule_is_allocatable(SWP_OP_vector &v,
			SWP_REG_ASSIGNMENT& swp_assign,
			MinDist& mindist,
			MRT& mrt, INT ii,
			BB* head, BB* tail,
			bool trace, bool trace_bundling)
{
  SWP_OP_vector test_v(v);

  if (!Modulo_Schedule_Verify(test_v, ii, mrt)) 
    return false;

  Modulo_Schedule_Succeeded(test_v, ii, mindist, mrt, trace);

  SWP_Bundle(test_v, trace_bundling);

  swp_assign.Clear();

  ISA_REGCLASS rc = ISA_REGCLASS_UNDEFINED;
  bool ok = swp_assign.Allocate_Loop_Variants(test_v, head, tail, &rc) &&
	    swp_assign.Invariants_Allocatable(test_v);

  swp_assign.Clear();
  return ok;
}

// Given a vector of ops in SWP loop which is topological sorted in 'ordered_in'
// find a schedule that is allocatable using standard branch-and-bound
// return 1 if successful or 0 if failed
static INT
schedule_one(INT32* ordered_in, SWP_OP_vector &v,
		SWP_REG_ASSIGNMENT& swp_assign,
		MinDist& mindist, Slack& slack,
		BB* head, BB* tail,
		INT max_sched_length,
		LT_Heuristics& heur, MRT& mrt, INT ii, INT variant_regs[],
		bool is_super_swp, INT* specified,
		MEM_POOL* pool, bool trace, bool trace_details)
{
  BB* body=NULL;
  const bool trace_bundling = Get_Trace(TP_SWPIPE, 0x1000);
  INT L[v.size()];	// latest cycle allowed
  INT C[v.size()];	// currently assigned cycle
  INT B[v.size()];	// number of backtracking
  INT32 ordered[v.size()];	// stack for backtracking
  INT fat_point= -1;
  INT current = 0;
  INT candidate;
  INT failed_allocation = 0;

  INT earliest;
  INT latest=0;
  INT cycle;
  INT trouble = -1;
  INT max_issue_alignment = 1;
  INT alignment_cycle = -1;
  INT num_aligned_op_placed = 0;

  bool use_specified = (specified!=NULL);

  if (ordered_in) {
    for (INT i=0; i<v.size(); i++) {
      ordered[i] = ordered_in[i];
    }
    candidate = ordered[current];
  } else {
    candidate = heur.Choose_Op(v,slack);
    ordered[current]=candidate;
  }

  for (INT i=0; i<v.size(); i++) {
      OP* op = v[i].op;
      if (body==NULL && op) {

	bool match = true;
	body = OP_bb(op);

      }
      if (op && v[i].issue_alignment!=1 && v[i].placed) {
	if (alignment_cycle == -1) {
	  max_issue_alignment = TI_TOP_Issue_Alignment(OP_code(op));
	  alignment_cycle = v[i].cycle % max_issue_alignment;
	}
	num_aligned_op_placed++;
      }
      B[i] = 0;
  }

  earliest = slack.Estart(candidate);
  latest   = Min(earliest + ii - 1, slack.Lstart(candidate));
  cycle = earliest;
  L[candidate] = latest;
  C[candidate] = cycle;

  while (1) {

      if (v[candidate].op) {

	if (v[candidate].placed) {
	  cycle = v[candidate].cycle;
	  slack.Update_Slack_From_Op(candidate, v, mindist);
	  earliest = cycle;
	  latest   = Max(cycle, Min(slack.Estart(candidate) + ii - 1, slack.Lstart(candidate)));
	  L[candidate] = latest;
	  C[candidate] = cycle;
	} else  if (use_specified) {
	  INT cycle_tmp;
	  cycle = specified[candidate];
          cycle_tmp = mrt.Find_Resources_In_Range(
			  candidate, v, cycle, latest, alignment_cycle, /*top_down=*/true);
	  if (cycle_tmp != cycle) {
	    // specified cycle is not available
	    cycle = cycle_tmp;
	    use_specified = false;
	  }
	} else if (cycle<=latest) {
          cycle = mrt.Find_Resources_In_Range(
			  candidate, v, cycle, latest, alignment_cycle, /*top_down=*/true);
        }

        if (cycle> latest) {

	  trouble = candidate;
	  use_specified = false;

	  if (!is_super_swp && ++B[trouble] > 10) {
	    if (trace) {
	      fprintf(TFile,
		      "Schedule_one failed : too many trials for OP %d current %d\n",
		      trouble, current);
	    }
	    return 0;
	  }


	  // back-track (with pruning)
	  if (current<=1) {
	    if (trace) {
	      fprintf(TFile, "End of search for II=%d\n", ii);
	    }
	    SWP_Undo_Bundle(v, body);
	    swp_assign.Clear();
	    return 0;
	  }

	  current--;
	  candidate = ordered[current];
	 
          if (v[candidate].placed) {
            v[candidate].placed = false;
	    if (v[candidate].issue_alignment!=1 && --num_aligned_op_placed==0)
	      alignment_cycle = -1;
            mrt.Unreserve_Op_Resources(v[candidate]);
            slack.Relax_Precedence(v, candidate, mindist);
	  }
	  cycle = C[candidate]+1;
	  latest = L[candidate];

	  continue;
        }
      }

      if (v[candidate].placed == false) {
        v[candidate].placed = true;
        v[candidate].cycle = cycle;
        C[candidate] = cycle;
        if (v[candidate].issue_alignment!=1) {
          num_aligned_op_placed++;
          if (alignment_cycle == -1)
	    alignment_cycle = cycle % max_issue_alignment;
        }
        if (v[candidate].op)
          mrt.Reserve_Op_Resources(v[candidate], v[candidate].cycle);

        bool satisfiable =
	      slack.Update_Slack_From_Placed_Op(candidate, v, mindist);
        if (!satisfiable) {
          if (v[candidate].placed) {
	    v[candidate].placed = false;
            mrt.Unreserve_Op_Resources(v[candidate]);
            slack.Relax_Precedence(v, candidate, mindist);
	    earliest = slack.Estart(candidate);
	    latest   = Min(earliest + ii - 1, slack.Lstart(candidate));
	    L[candidate] = latest;
	  }
	  cycle++;
	  // need to back-track
	  continue;
        }

        if (trace_details) {
          fprintf(TFile, "%d[%d]@%d (%d,%d)\n", current, candidate,
		v[candidate].cycle,
		slack.Estart(candidate), slack.Lstart(candidate));
        }
      }

      // Get next op
      current++;
      if (current == v.size()-2) {

	// all ops are scheduled, check allocation
        fat_point = heur.Allocatable(v, ii, variant_regs, pool);
	if (fat_point < 0) {

	  bool ok = schedule_is_allocatable(v, swp_assign, mindist, mrt, ii,
					    head, tail, trace, trace_bundling);

	  if (ok) {
	      // successfully scheduled and allocated
	      if (swp_assign.Trace()) {
		fprintf(TFile, "  Est. register allocation ok\n");
	      }
	      if (trace) {
		fprintf(TFile, "Allocatable schedule found by schedule_one\n");
	      }

	      swp_assign.Clear();
	      use_specified = false;
	      return 1;
	  }

	  if (swp_assign.Trace()) {
	    fprintf(TFile, "  Est. reg alloc fails, try larger II\n");
	  }

          swp_assign.Clear();
	}

	failed_allocation++;

	if (trace) {
	  fprintf(TFile, "Allocation failed: %d (II=%d)\n", failed_allocation, ii);
	}
	// return after too many allocation failure
	if (!is_super_swp && failed_allocation>=5) {
	  if (trace) {
	    fprintf(TFile,
		"Schedule_one failed : too many failed allocations\n");
	  }
	  SWP_Undo_Bundle(v, body);
	  swp_assign.Clear();
	  return 0;
	}

	INT fat_cycle = (fat_point<0) ? -1 : v[fat_point].cycle;

	// back-track until a candidate is found to change register pressure
	do {

	    if (current<=1)
	      return 0;

	    current--;
	    candidate = ordered[current];
	 
            v[candidate].placed = false;
	    if (v[candidate].issue_alignment!=1 && --num_aligned_op_placed==0)
	      alignment_cycle = -1;
            if (v[candidate].op)
              mrt.Unreserve_Op_Resources(v[candidate]);
            slack.Relax_Precedence(v, candidate, mindist);
	    earliest = slack.Estart(candidate);
	    latest   = Min(earliest + ii - 1, slack.Lstart(candidate));
	    L[candidate] = latest;
	    cycle = C[candidate]+1;

	} while (fat_point>=0 && (cycle-fat_cycle)*(latest-fat_cycle)>0);

	continue;
      } else {
	if (ordered_in)
	  candidate = ordered[current];
	else {
          candidate = heur.Choose_Op(v,slack);
          ordered[current] = candidate;
	}
      }

      earliest = slack.Estart(candidate);
      latest   = Min(earliest + ii - 1, slack.Lstart(candidate));
      cycle = earliest;
      L[candidate] = latest;
      C[candidate] = cycle;

  }

  SWP_Undo_Bundle(v, body);
  swp_assign.Clear();
  return 0;
}

// toplogical sorting based on dependence graph
void top_sort(const SWP_OP_vector& v, INT32 ordered[], bool trace,
	      MEM_POOL* pool) {

  MEM_POOL_Push(pool);

  // add a scope to allow destructor to run before the mem pool pop
  {

  // number of in-arcs for each node
  INT32* num_preds = CXX_NEW_ARRAY(INT32, v.size(), pool);

  DYN_ARRAY<INT32> ready(pool);

  for (INT i=0; i<v.size(); i++) {
    num_preds[i]=0;
    OP* op = v[i].op;
    if (op) {
      for ( ARC_LIST *al = OP_preds(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);
	if (ARC_kind(arc) == CG_DEP_REGIN && ARC_omega(arc)==0 &&
	    SWP_index(pred) != -1) {
	  num_preds[i]++;
	}
      }
      if (op && num_preds[i]==0)
	ready.AddElement(i);
    }
  }


  INT count = 0;
  while (count <= ready.Lastidx()) {
    INT32 candidate = ready[count];
    ordered[count++]=candidate;
    OP* op = v[candidate].op;
    if (op) {
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);
	INT32 succ_index = SWP_index(succ);
	if (ARC_kind(arc) == CG_DEP_REGIN && ARC_omega(arc)==0 &&
	    succ_index != -1) {
	  num_preds[succ_index]--;
	  if (num_preds[succ_index]==0) {
	    ready.AddElement(succ_index);
	  }
	}
      }
    }
  }

  if (trace) {
    fprintf(TFile, "Toplogical Sort:\n");
    for (INT i=0; i<count; i++) {
      fprintf(TFile, "%2d: ", i);
      v[ordered[i]].Print(TFile);
    }
  }

  FmtAssert(count==v.size()-2, ("Error in sorting"));

  }

  MEM_POOL_Pop(pool);
}


// sorting based on dependence graph and bundling constraints
// 'v' contains v.size()-2 real ops to be scheduled plus 2 pseudo
// start and stop
// on return, ordered[i] contains the i-th op if a linear scheduling
// is performed
// the linear scheduling is a backward scheduling prioritized on
// critical path length
void linear_schedule_sort(const SWP_OP_vector& v, INT32 ordered[], bool trace,
	      MEM_POOL* pool) {

  MEM_POOL_Push(pool);

  // add a scope to allow destructor to run before the mem pool pop
  {

  INT32* num_succs = CXX_NEW_ARRAY(INT32, v.size(), pool);
  INT32* num_preds = CXX_NEW_ARRAY(INT32, v.size(), pool);
  INT32* ready_cycle = CXX_NEW_ARRAY(INT32, v.size(), pool);
  INT32* height = CXX_NEW_ARRAY(INT32, v.size(), pool);

  // buffer to check bundling constraints
  TOP* bundled_tops = CXX_NEW_ARRAY(TOP, v.size(), pool);

  DYN_ARRAY<INT32> ready(pool);
  DYN_ARRAY<INT32> top_down_ready(pool);

  // preparation step
  //
  // for each op, we compute the number of preds/succs
  // and initialize the height (max critical path) and ready_cycle
  // since this is a backward scheduling, 
  // we also initialize the ready queue with ops with no successors
  for (INT i=0; i<v.size(); i++) {
    num_preds[i]=0;
    num_succs[i]=0;
    ready_cycle[i] = 0;
    height[i] = 0;
  }
  for (INT i=0; i<v.size(); i++) {
    OP* op = v[i].op;
    if (op) {
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);
	if (ARC_omega(arc)==0 && SWP_index(succ) != -1 &&
	    (ARC_kind(arc) == CG_DEP_REGIN ||
	     ARC_kind(arc) == CG_DEP_REGANTI ||
	     ARC_kind(arc) == CG_DEP_REGOUT)) {
	  num_succs[i]++;
	  INT32 succ_index = SWP_index(succ);
	  num_preds[succ_index]++;
	}
      }
      if (op && num_succs[i]==0) {
	ready.AddElement(i);
	ready_cycle[i] = 0;
      }
    }
  }

  // next we want to compute the max critical path length
  // we use a different queue (top_down_ready) and initialize
  // it with ops with no predecessor
  for (INT i=0; i<v.size(); i++) {
    OP* op = v[i].op;
    if (op) {
      if (op && num_preds[i]==0) {
	top_down_ready.AddElement(i);
      }
    }
  }

  // compute max critical path length
  int current_cycle = 0;
  INT count = 0;
  while (count <= top_down_ready.Lastidx()) {
    INT32 candidate = top_down_ready[count++];
    OP* op = v[candidate].op;
    if (op) {
      int h = height[candidate];
      for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	OP *succ = ARC_succ(arc);
	INT32 succ_index = SWP_index(succ);
	if (ARC_omega(arc)==0 && succ_index != -1 &&
	    (ARC_kind(arc) == CG_DEP_REGIN ||
	     ARC_kind(arc) == CG_DEP_REGANTI ||
	     ARC_kind(arc) == CG_DEP_REGOUT)) {
	  num_preds[succ_index]--;
	  if (height[succ_index]<h+1)
	    height[succ_index] = h+1;
	  if (num_preds[succ_index]==0) {
	    top_down_ready.AddElement(succ_index);
	  }
	}
      }
    }
  }

  // main scheduling step
  int num_bundled = 0;
  count = 0;

  // loop until the ready queue is empty
  while (count <= ready.Lastidx()) {

    // if nothing can be bundled in the current cycle
    // then advance cycle and clear the bundle and continue
    if (num_bundled == TI_ISA_Max_Num_Slots()) {
      current_cycle++;
      num_bundled = 0;
      continue;
    }

    // choose the next op
    // the candidate is tested to see if
    //   1. it is ready in the current cycle
    //   2. meet the bundling constraints given other
    //      ops already scheduled in the current cycle
    //   3. has the highest priority
    int scan = count;
    int best_priority = -1;
    int best_candidate = -1;
    int best_index = -1;
    while (scan <= ready.Lastidx()) {
      int candidate = ready[scan];
      OP* op = v[candidate].op;
      if (ready_cycle[candidate]<=current_cycle) {
	bundled_tops[num_bundled++] = OP_code(op);
	if ((num_bundled==0 ||
	     TI_SI_Check_Resource_Ok(bundled_tops, num_bundled)) &&
	    ((height[candidate] > best_priority) ||
	     (height[candidate] == best_priority) && candidate>best_candidate)) {
	  best_candidate = candidate;
	  best_priority = height[candidate];
	  best_index = scan;
	}
	num_bundled--;
      }
      scan++;
    }

    // if nothing can be bundled in the current cycle
    // then advance cycle and clear the bundle and continue
    if (best_candidate == -1) {
      current_cycle++;
      num_bundled = 0;
      continue;
    }

    OP* best_op = v[best_candidate].op;
    bundled_tops[num_bundled++] = OP_code(best_op);

    // if a valid candidate is found
    // swap it the current head of the ready list
    INT32 candidate = ready[best_index];
    if (best_index != count) {
      ready[best_index] = ready[count];
    }

    // put the candidate in the output array in reverse order
    // and update the successor status and ready cycle
    // for the predecessors
    // enter the predecessors into ready queue if there is no
    // more sucessor
    ordered[(v.size()-3-count++)]=candidate;
    OP* op = v[candidate].op;
    if (op) {
      for ( ARC_LIST *al = OP_preds(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc    = ARC_LIST_first(al);
	OP *pred = ARC_pred(arc);
	INT32 pred_index = SWP_index(pred);
	if (ARC_omega(arc)==0 && pred_index != -1 &&
	    (ARC_kind(arc) == CG_DEP_REGIN ||
	     ARC_kind(arc) == CG_DEP_REGANTI ||
	     ARC_kind(arc) == CG_DEP_REGOUT)) {
	  num_succs[pred_index]--;
	  int pred_cycle = current_cycle + ARC_latency(arc);
	  if (pred_cycle > ready_cycle[pred_index])
	    ready_cycle[pred_index] = pred_cycle;
	  if (num_succs[pred_index]==0) {
	    ready.AddElement(pred_index);
	  }
	}
      }
    }
  }

  if (trace) {
    fprintf(TFile, "Linear Schedule Sort:\n");
    for (INT i=0; i<count; i++) {
      fprintf(TFile, "%2d: ", i);
      v[ordered[i]].Print(TFile);
    }
  }

  FmtAssert(count==v.size()-2, ("Error in sorting"));

  }

  MEM_POOL_Pop(pool);
}

//************************************************************************
// The (exhaustive, register allocation backtracking) super SWP algorithm
//
// only the first scheduling_order is needed/used since it is exhaustive
//************************************************************************
SWP_RETURN_CODE
Modulo_Schedule_Super(
		SWP_OP_vector &swp_op_vector, SWP_REG_ASSIGNMENT& swp_assign,
		BB* head, BB* tail,
		INT min_ii, INT max_ii, 
		double incr_alpha, double incr_beta, INT max_issue_alignment,
		INT32** scheduling_order,
		INT budget, INT variant_regs[], INT specified_ii, INT specified[],
		bool trace, bool trace_details)
{
#ifdef Is_True_On
  swp_op_vector.Verify();
#endif
  swp_op_vector.ii = 0;
  swp_op_vector.succeeded = false;
  if (swp_op_vector.size() >= SWP_OPS_LIMIT) {
    if (trace)
      fprintf(TFile, "MOD SCHED FAILED: loop too big!");
    return MOD_SCHED_FAILED;
  }

  CXX_MEM_POOL local_mem_pool("modulo schedule pool", FALSE);

  for (INT ii = min_ii; 
       ii <= max_ii;
       ii = Max(ii+1, (INT)((ii + incr_alpha) * incr_beta - incr_alpha))) {

    swp_op_vector.ii = ii;

    if (trace) 
      fprintf(TFile, "============================\nSUPER SWP SCHED with ii %d\n", ii);

    if (ii%max_issue_alignment) {
      if (trace) 
        fprintf(TFile, "swp: ii is skipped since it is not a multiple of max_issue_alignment (%d)\n", max_issue_alignment);
      continue;
    }

    MEM_POOL_Push(local_mem_pool());

    bool mop_critical = ((double) swp_op_vector.num_mops / ii > 
			 2 * SWP_Options.Critical_Threshold / 100.0);
    bool flop_critical = ((double) swp_op_vector.num_flops / ii >
			 2 * SWP_Options.Critical_Threshold / 100.0);
    if (trace) {
      fprintf(TFile, "swp: %d memop is %s critical\n",
	      swp_op_vector.num_mops,  mop_critical ? "" : "not");
      fprintf(TFile, "swp: %d flop is %s critical\n", 
	      swp_op_vector.num_flops, flop_critical ? "" : "not");
    }

    MinDist mindist(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii);
    if (ii != mindist.Found_ii()) {
      if (trace)
        fprintf(TFile, "swp:: ii is skipped since it is too small for MinDist.\n");
      continue;
    }

    MinLT minlt(swp_op_vector, ii, mindist);
    Slack slack(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii, mindist);

    slack.Verify();   // Verify precedence constraints

    if (trace) {
      swp_op_vector.Print(TFile);
      mindist.Print(TFile);
      slack.Print(TFile);
      minlt.Print(TFile);
    }

    // we will try to search for schdule twice
    // first time with shorter max_sched_length to avoid multi-staged
    // pipeline which requires wind-up/down code

    INT max_sched_length;
    INT status;

    // skip the first search if the schedule is specified
    if (ii!=specified_ii) {

      MRT mrt(swp_op_vector, ii, local_mem_pool());
      LT_Heuristics heur(ii, max_issue_alignment, minlt,
			 1, scheduling_order[0], trace, trace_details);
      heur.Init_SWP_OP_state(swp_op_vector, slack, mrt, mop_critical, flop_critical);
      mrt.Verify();     // Verify sanity of resources allocation

      if (trace) {
        slack.Print(TFile);
      }
      max_sched_length = ii;
      if (slack.Set_last_cycle(swp_op_vector, max_sched_length, mindist)) {
        swp_op_vector[swp_op_vector.stop].cycle = max_sched_length;

        status = schedule_one(NULL, swp_op_vector, swp_assign, mindist, slack,
		head, tail, max_sched_length,
		heur, mrt, ii, variant_regs, /* is_super_swp= */ true,
		NULL,
		local_mem_pool(), trace, trace_details);

        if (status == 1) {
	  if (Modulo_Schedule_Verify(swp_op_vector, ii, mrt)) {
	    Modulo_Schedule_Succeeded(swp_op_vector, ii, 
				  mindist, mrt, trace);
	    return MOD_SCHED_SUCCEEDED;
	  }
	}
      }
    }


    // second schedule uses longer max_sched_length to have more freedom
    // in search at the cost of potential multi-staged pipeline
    // and wind-up/down code

    Slack slack1(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii, mindist);
    max_sched_length = slack1.Lstart(swp_op_vector.stop) +
	      		     SWP_Options.Max_Schedule_Incr;
    if (slack1.Set_last_cycle(swp_op_vector, max_sched_length, mindist)==false)
	  continue;

    MRT mrt1(swp_op_vector, ii, local_mem_pool());
    LT_Heuristics heur1(ii, max_issue_alignment, minlt,
			1, scheduling_order[0],
			trace, trace_details);
    heur1.Init_SWP_OP_state(swp_op_vector, slack1, mrt1, mop_critical, flop_critical);
    mrt1.Verify();     // Verify sanity of resources allocation

    // set to max allowed single iteration schedule length
    if (trace) {
      slack1.Print(TFile);
    }
    swp_op_vector[swp_op_vector.stop].cycle = max_sched_length;

    status = schedule_one(NULL, swp_op_vector, swp_assign, mindist, slack1,
		head, tail, max_sched_length,
		heur1, mrt1, ii, variant_regs, /* is_super_swp= */ true,
		(ii==specified_ii)?specified:NULL,
		 local_mem_pool(), trace, trace_details);

    if (status == 1) {
	if (Modulo_Schedule_Verify(swp_op_vector, ii, mrt1)) {
	  Modulo_Schedule_Succeeded(swp_op_vector, ii, 
				  mindist, mrt1, trace);
	  return MOD_SCHED_SUCCEEDED;
	}
    }

    MEM_POOL_Pop(local_mem_pool());
  }
  if (trace)
    fprintf(TFile, "MOD SCHED FAILED: cannot find any schedule smaller than maxII cycles.\n");
  return MOD_SCHED_FAILED;
}


//************************************************************************
//  The standard backtracking SWP algorithm
//************************************************************************
SWP_RETURN_CODE
Modulo_Schedule(SWP_OP_vector &swp_op_vector, SWP_REG_ASSIGNMENT& swp_assign,
		BB* head, BB* tail,
		INT min_ii, INT max_ii, 
		double incr_alpha, double incr_beta, INT max_issue_alignment,
		INT32** scheduling_order,
		INT budget, INT variant_regs[],
		INT specified_ii, INT specified[],
		bool trace, bool trace_details)
{
#ifdef Is_True_On
  swp_op_vector.Verify();
#endif
  swp_op_vector.ii = 0;
  swp_op_vector.succeeded = false;
  if (swp_op_vector.size() >= SWP_OPS_LIMIT) {
    if (trace)
      fprintf(TFile, "MOD SCHED FAILED: loop too big!");
    return MOD_SCHED_FAILED;
  }

  CXX_MEM_POOL local_mem_pool("modulo schedule pool", FALSE);

  for (INT ii = min_ii; 
       ii <= max_ii;
       ii = Max(ii+1, (INT)((ii + incr_alpha) * incr_beta - incr_alpha))) {

    swp_op_vector.ii = ii;

    if (trace) 
      fprintf(TFile, "============================\nSWP SCHED with ii %d\n", ii);

    if (ii%max_issue_alignment) {
      if (trace) 
        fprintf(TFile, "swp: ii is skipped since it is not a multiple of max_issue_alignment (%d)\n", max_issue_alignment);
      continue;
    }

    bool mop_critical = ((double) swp_op_vector.num_mops / ii > 
			 2 * SWP_Options.Critical_Threshold / 100.0);
    bool flop_critical = ((double) swp_op_vector.num_flops / ii >
			 2 * SWP_Options.Critical_Threshold / 100.0);
    if (trace) {
      fprintf(TFile, "swp: %d memop is %s critical\n",
	      swp_op_vector.num_mops,  mop_critical ? "" : "not");
      fprintf(TFile, "swp: %d flop is %s critical\n", 
	      swp_op_vector.num_flops, flop_critical ? "" : "not");
    }

    MinDist mindist(swp_op_vector, swp_op_vector.start, swp_op_vector.stop, ii);
    if (ii != mindist.Found_ii()) {
      if (trace)
        fprintf(TFile, "swp:: ii is skipped since it is too small for MinDist.\n");
      continue;
    }

    MinLT minlt(swp_op_vector, ii, mindist);
    Slack slack_static(swp_op_vector, swp_op_vector.start, swp_op_vector.stop,
		       ii, mindist);

    // scheduling orders: 0 -- original order, 1 -- linear scheduling order
    for (INT current_scheduling_order = 0;
	 scheduling_order[current_scheduling_order]!=NULL;
	 current_scheduling_order++) {

     INT heuristics = SWP_Options.Heuristics;
     INT first_heuristics;
     INT last_heuristics;
     if (heuristics==0) {
       first_heuristics = 1;
       last_heuristics = 6;
     } else {
       first_heuristics = heuristics;
       last_heuristics = heuristics;
     }

     for (INT current_heuristics = first_heuristics;
	 current_heuristics <= last_heuristics;
	 current_heuristics++) {

      MEM_POOL_Push(local_mem_pool());

      if (trace) {
	fprintf(TFile,"SWP heuristics %d\n", current_heuristics);
      }

      MRT mrt(swp_op_vector, ii, local_mem_pool());
      mrt.Verify();     // Verify sanity of resources allocation

      Slack slack(swp_op_vector, swp_op_vector.start, swp_op_vector.stop,
		  ii, mindist);
      slack.Verify();   // Verify precedence constraints

      LT_Heuristics heur(ii, max_issue_alignment, minlt, current_heuristics,
			 scheduling_order[current_scheduling_order],
			 trace, trace_details);
      heur.Init_SWP_OP_state(swp_op_vector, slack, mrt,
			     mop_critical, flop_critical);

      if (trace) {
	swp_op_vector.Print(TFile);
	mindist.Print(TFile);
	slack.Print(TFile);
	minlt.Print(TFile);
      }

      INT num_placed;
      for (num_placed = 0; true; num_placed++) {

#if SWP_DEBUG
        // This is controlled under a separate flag because they
        // can double the compilation time!
        slack.Verify();   // Verify precedence constraints
        (void)mrt.Verify2(swp_op_vector);
			  // Verify sanity of resources allocation
#endif

        if (trace_details) {
	  fprintf(TFile,"... trial %d\n", num_placed);
	  for (INT i = 0; i < swp_op_vector.size(); i++) {

	    // 'D' => SWP_TOP_DOWN
	    // 'U' => SWP_BOTTOM_UP
	    // '?' => SWP_UNKOWN
	    // 'P' => placed
	    // 'S' => start node
	    // 'E' => stop node

	    fprintf(TFile, "[%d] e=%d l=%d s=%d c=%d t=%d %c %c %c\n", i,
		    slack.Estart(i), slack.Lstart(i), slack(i),
		    swp_op_vector[i].cycle,
		    swp_op_vector[i].trials,
		    (swp_op_vector[i].direction == SWP_TOP_DOWN)?'D':
		    ((swp_op_vector[i].direction == SWP_BOTTOM_UP)?'U':'?'),
		    (swp_op_vector[i].placed)?'P':' ',
		    (i==swp_op_vector.start)?'S':
					(i==swp_op_vector.stop)?'E':' ');
	  }
        }

	// Get next highest priority op
        INT candidate = heur.Choose_Op(swp_op_vector, slack);

        if (candidate < 0) {
	  // scheduling succeeded now check for allocation
	  candidate = heur.Allocatable(swp_op_vector, ii, variant_regs,
			  local_mem_pool());
	  if (candidate < 0) {
	    const bool trace_bundling = Get_Trace(TP_SWPIPE, 0x1000);
	    bool ok = schedule_is_allocatable(
			swp_op_vector, swp_assign, mindist, mrt, ii, head, tail,
			trace, trace_bundling);
	    if (ok) {
	      if (Modulo_Schedule_Verify(swp_op_vector, ii, mrt)) {
	        Modulo_Schedule_Succeeded(
				swp_op_vector, ii, mindist, mrt, trace);
	        return MOD_SCHED_SUCCEEDED;
	      }
	    }
	  }


	  int stage_count=Modulo_Schedule_Get_Stage_Count(swp_op_vector, ii);

	  // if the stage count is greater 1 than we can still increase II
	  // to reduce the register presure
	  if (stage_count<2 && current_heuristics == last_heuristics) {
	    if (trace) {
	      fprintf(TFile, "MOD SCHED FAILED: failed to allocate with "
			     "pipe stage = 1. Increase II will not help. "
			     "Will try different heurirsics\n");
	    }
	    return MOD_SCHED_FAILED;
	  } else {
	    if (trace) {
	      fprintf(TFile, "MOD SCHED FAILED: failed to allocate with "
			     "pipe stage %d. "
			     "Will try different heurirsics or larger II\n",
			     stage_count);
	    }
	    break;
	  }
	}
    
        pair<bool,bool> issue =
		heur.Choose_Issue_Cycle(candidate, swp_op_vector, slack, mrt);

        if (SWP_Options.SWP_Opt_Level == 0) {
	  if (swp_op_vector[candidate].cycle > slack_static.Lstart(candidate) ||
	      swp_op_vector[candidate].cycle < slack_static.Estart(candidate)) {
	    if (trace)
	      fprintf(TFile,
		    "MOD SCHED FAILED: OP %d at cycle %d exceed estart (%d) "
		    "lstart (%d) range.\n",
		    candidate, swp_op_vector[candidate].cycle,
		    slack_static.Estart(candidate),
		    slack_static.Lstart(candidate));
	    break;
	  }
        }
        if (swp_op_vector[candidate].trials > budget) {
	  if (trace)
	    fprintf(TFile, "MOD SCHED FAILED: OP %d has %d trials exceed "
			    "trial budget (%d).\n",
		    candidate, swp_op_vector[candidate].trials, budget);
	  break;
        }

        if (trace) {
	  fprintf(TFile, "placed OP %d at cycle %d estart %d lstart %d\n",
		candidate, swp_op_vector[candidate].cycle,
		slack.Estart(candidate), slack.Lstart(candidate));
        }

        if (issue.second) 
	  heur.Eject_Resources_Conflict_OPs(candidate, swp_op_vector, mrt);

        if (issue.first)
	  heur.Eject_Precedence_Conflict_OPs(candidate, swp_op_vector, slack,
			  		     mindist, mrt);
        if (trace)
	  heur.Print(TFile); // print unplaced OPs

        heur.Update_Resources(candidate, swp_op_vector, mrt);
      
        bool satisfiable =
	  heur.Update_Precedence(candidate, swp_op_vector, slack, mindist, mrt);
        if (!satisfiable) {
	  if (trace) {
	    swp_op_vector.Print(TFile);
	    slack.Print(TFile);
	    fprintf(TFile,
		"failed to SWP due to  exceeding max sched length.\n");
	    fprintf(TFile,
		"current II=%d  max II=%d\n", ii, max_ii);
	  }
	  break;
        }
      }
      swp_op_vector.previous_trials += num_placed;
      MEM_POOL_Pop(local_mem_pool());
     }
    }
  }


  if (trace)
    fprintf(TFile, "MOD SCHED FAILED: cannot find any schedule smaller "
		   "than maxII cycles.\n");
  return MOD_SCHED_FAILED;
}
