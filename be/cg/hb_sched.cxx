
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


// =======================================================================
// =======================================================================
//
//  Module: hb_sched.cxx
//  $Revision: 1.53 $
//  $Date: 2000/10/18 14:26:07 $
//  $Author: dew $
//  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/hb_sched.cxx,v $
//
//  Description:
//  ============
//
//  Hyberblock (HB) Scheduling routines.
//
// =======================================================================
// =======================================================================

#include <alloca.h>
#include <math.h>
#include "defs.h"
#include "config.h"
#include "config_targ_options.h"
#include "mempool.h"
#include "bb.h"
#include "bb_set.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "glob.h"
#include "tn_map.h"
#include "cg.h"
#include "cg_flags.h"
#include "ercg.h"
#include "cgtarget.h"
#include "cg_vector.h"
#include "dominate.h"
#include "findloops.h"
#include "note.h"
#include "lra.h"
#include "gcm.h"
#include "ti_res.h"
#include "ti_res_res.h"
#include "ti_latency.h"
#include "ti_errors.h"
#include "cg_region.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "cxx_memory.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "ti_bundle.h"
#include "whirl2ops.h"
#include "gra.h" /* for GRA_Local_Register_Grant() */

class reg_graph {
  struct reg_graph_node {
    OP *op;
    INT num_reg_results;
    INT num_dead_opnds;
    INT num_exposed_opnds;
    INT max_pred_usage;
    INT total_pred_usage;
    INT live_opnd_mask;
    INT split_opnd_mask;
    INT new_pred_live_vals;
    INT dfsnum;
    VECTOR preds;
    VECTOR succs;
    void init(OP *new_op, INT pred_length, INT succ_length, INT new_dfsnum, MEM_POOL *pool)
    { 
      op = new_op; live_opnd_mask = 0; split_opnd_mask = 0;
      num_reg_results = 0; num_dead_opnds = 0; 
      max_pred_usage = 0; total_pred_usage = 0; 
      new_pred_live_vals = 0; num_exposed_opnds = 0; dfsnum = new_dfsnum;
      if (pred_length > 0)
	preds = VECTOR_Init(pred_length, pool);
      else
	preds = NULL;
      if (succ_length > 0)
	succs = VECTOR_Init(succ_length, pool);
      else
	succs = NULL;
    }
  };

  TN_SET *_live_tns;
  OP_MAP _op_map;
  BOOL _op_map_ok;
  BB_MAP _opsch_map;
  MEM_POOL *_pool;
  void add_tree_branch(struct reg_graph_node *node, struct reg_graph_node *pred_node, 
		       BOOL reg_dep);
  void update_live_count(struct reg_graph_node *node);
  void recompute_node(struct reg_graph_node *node);
  
public:
  void init(BB *bb, BB_MAP opsch_map, MEM_POOL *pool);
  INT new_live_vals(OP *op);
  INT op_live_vals(OP *op);
  INT min_register_usage(OP *op);
  void schedule_op(OP *op);
  void add_op_to_graph(OP *op);
  void print_op_stats(OP *op);
  void free();
};

// ======================================================================
// Declarations (macros, variables)
// ======================================================================

BOOL Trace_HB = FALSE;
BOOL HB_Sched_With_Lra_Request = TRUE;
BOOL HB_Dfs_Uses_Last_Sched_Inst = TRUE;
class reg_graph HB_reg_graph;

static INT BBs_Processed = 0;

// The current cycle in which we are trying to schedule OPs.
static INT Clock;
static INT MAX_Clock;

// the last cycle with known issue alignment
static INT Last_aligned_cycle;

// the last known issue alignment
static INT Last_issue_alignment;

static INT aligned_op_count;

static void
Print_OPSCH (OP *op, BB_MAP value_map)
{
  OPSCH *opsch = OP_opsch(op, value_map);
  Print_OP_No_SrcLine (op);
  fprintf (TFile, "    <dfs:%3d cyc:%2d reg:%2d est:%2d lst:%2d succs:%d preds:%d",
	   OPSCH_dfsnum(opsch), OPSCH_scycle(opsch), OPSCH_regcost(opsch),
	   OPSCH_estart(opsch), OPSCH_lstart(opsch), 
	   OPSCH_num_succs(opsch), OPSCH_num_preds(opsch));
  if (!Get_Trace(TP_SCHED, 0x800)) {
    fprintf(TFile, ">\n");
    HB_reg_graph.print_op_stats(op);
  } else {
    fprintf(TFile, " splt:%d live:%d align:%d res:%c, (%3d:%3d)>\n",
	    OPSCH_num_splits(opsch), OPSCH_live_vals(opsch), 
	    OPSCH_issue_alignment(opsch),
	    OPSCH_use_shared_resource(opsch)?'Y':'N',
	    OPSCH_min_usage(opsch), OPSCH_total_usage(opsch));
  }
}

static void
Print_BB_For_HB (BB *bb, BB_MAP value_map)
{
  OP *op;

  fprintf (TFile, "*************** BB:%d ******************\n", BB_id(bb));
  FOR_ALL_BB_OPs_FWD (bb, op) {
    Print_OPSCH (op, value_map);
  }
  fprintf (TFile, "****************************************\n");
}

void
Print_BB_For_HB (list<BB*> bblist, BB_MAP value_map)
{
  list<BB*>::iterator bbiter;

  fprintf (TFile, "\n********** HyperBlock (HB) ******************\n");
  fprintf (TFile, "******* Contains :");
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbiter) {
    fprintf (TFile, " BB:%d ", BB_id(*bbiter));
  }
  fprintf (TFile, "**********\n");

  CG_DEP_Trace_HB_Graph (bblist);
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbiter) {
    Print_BB_For_HB(*bbiter, value_map);
  }
  fprintf (TFile, "****************************************\n");

}

// ======================================================================
// Check to see if there is a need to reschedule this block. Sometimes
// it's better to reschedule the block with different heuristics (#622253). 
// Currently, we limit it to single-BB loops where the benefits are more
// pronounced.
// ======================================================================
BOOL
Reschedule_BB(BB *bb)
{

  // At the moment, target single_BB loops ONLY.
  if (BB_loop_head_bb(bb) == bb) {
    BBLIST *succ_list;
    FOR_ALL_BB_SUCCS (bb, succ_list) {
      BB *succ_bb = BBLIST_item(succ_list);
      if (succ_bb == bb) return TRUE;
    }
  }

  return FALSE;
}

// ======================================================================
// Check to see if the given HB can be scheduled, i.e prior not SWP'd.
// ======================================================================
BOOL
Can_Schedule_HB(list<BB*> hb_blocks)
{

  list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD (hb_blocks, bb_iter) {
    // if <reschedule> flag is already set, then return FALSE.
    if (BB_scheduled(*bb_iter) && !BB_scheduled_hbs(*bb_iter)) return FALSE;
  }

  return TRUE;
}

// ======================================================================
// Return the operand number of 'op's base tn. 'op' must be a memory
// operation.
// ======================================================================
INT
Memory_OP_Base_Opndnum (OP *op)
{
  FmtAssert(OP_memory(op), ("expecting memory operation"));
  return TI_TOP_Find_Operand_Use(OP_code(op), OU_base);
}

// ======================================================================
// Return the operand number of 'op's offset immediate. 'op' must be a
// memory operation. Return -1 if 'op' has no immediate.
// ======================================================================
INT
Memory_OP_Offset_Opndnum (OP *op)
{
  FmtAssert(OP_memory(op), ("expecting memory operation"));
  return TI_TOP_Find_Operand_Use(OP_code(op), OU_offset);
}

// ======================================================================
// Initialize <regs_map> for the basic block. The defs for each TN
// are counted. Also mark all global TNs as having a register 
// already assigned to them.
// ======================================================================
void HB_Schedule::Init_Register_Map (BB *bb)
{
  OP *op;

  _regs_map = hTN_MAP_Create (&_hb_pool);
  FOR_ALL_BB_OPs_FWD (bb, op) {
    INT i;
    for (i = 0; i < OP_results(op); i++) {
      TN *result_tn = OP_result(op, i);
      REG_ENTRY reginfo;
      REG_ENTRY_ptr(reginfo) =  hTN_MAP_Get (_regs_map, result_tn);
      REG_ENTRY_def_count(reginfo)++;
      if (TN_is_global_reg(result_tn) || TN_is_dedicated(result_tn)) 
	REG_ENTRY_reg_assigned(reginfo) = TRUE;
      hTN_MAP_Set (_regs_map, result_tn, REG_ENTRY_ptr(reginfo));
    }
    for (i = 0; i < OP_opnds(op); i++) {
      TN *opnd_tn = OP_opnd(op,i);
      if (TN_is_constant(opnd_tn)) continue;
      if (TN_is_global_reg(opnd_tn) || TN_is_dedicated(opnd_tn)) {
        REG_ENTRY reginfo;
        REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
        REG_ENTRY_reg_assigned(reginfo) = TRUE;
        hTN_MAP_Set (_regs_map, opnd_tn, REG_ENTRY_ptr(reginfo));
      }
    }
  }
}

// ======================================================================
// Estimate the register cost for scheduling <op> next. This cost depends
// on the following factors:
//  - how many registers are available for allocation at this point ?
//  - how many registers are used and freed up by <op> ?
//  - is the number of registers available below a threshold ?
// ======================================================================
void
HB_Schedule::Estimate_Reg_Cost_For_OP (OP *op)
{
  INT cost = 0;
  INT32 local_regs_avail[TI_ISA_REGCLASS_MAX+1];
  ISA_REGCLASS cl;
  REG_ENTRY reginfo;
  BB* cur_bb = OP_bb(op);

  FOR_ALL_ISA_REGCLASS(cl) {
    local_regs_avail[cl] = _Cur_Regs_Avail[cl];
  }

  INT i;
  for (i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op, i);
    // If the result tn is also referenced in the OP, don't consider the 
    // register being freed above the def.
    if (!OP_Refs_TN (op, result_tn)) {
      REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, result_tn);
      if (REG_ENTRY_def_count(reginfo) == 1 &&
	  REG_ENTRY_reg_assigned(reginfo)) 
      {
        cl = TN_register_class(result_tn);
	INT reg_pressure =
	  (LRA_Register_Request(cur_bb,cl) >
	  REGISTER_SET_Size(GRA_Local_Register_Grant(cur_bb,cl)))? 
	  (LRA_Register_Request(cur_bb,cl) -
	  REGISTER_SET_Size(GRA_Local_Register_Grant(cur_bb,cl))):0;
	if (HB_Sched_With_Lra_Request)
	  reg_pressure += 2 - local_regs_avail[cl];
	else
	  reg_pressure = 2 - local_regs_avail[cl];
	if (reg_pressure > 0) {
	  cost -= reg_pressure;
	}
	local_regs_avail[cl]++;
      }
    }
  }
  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    if (TN_is_constant(opnd_tn)) continue;
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
    if (!REG_ENTRY_reg_assigned(reginfo)) {
      // check for an earlier occurence of the opnd_tn.
      BOOL reg_handled = FALSE;
      for (INT j = 0; j < i; j++) {
	if (OP_opnd(op,j) == opnd_tn)
	  reg_handled = TRUE;
      }
      if (!reg_handled) {
        cl = TN_register_class(opnd_tn);
        local_regs_avail[cl]--;
	INT reg_pressure =
	  (LRA_Register_Request(cur_bb,cl) >
	  REGISTER_SET_Size(GRA_Local_Register_Grant(cur_bb,cl)))? 
	  (LRA_Register_Request(cur_bb,cl) -
	  REGISTER_SET_Size(GRA_Local_Register_Grant(cur_bb,cl))):0;
	if (HB_Sched_With_Lra_Request)
	  reg_pressure += 2 - local_regs_avail[cl];
	else
	  reg_pressure = 2 - local_regs_avail[cl];
        if (reg_pressure > 0) {
          cost += reg_pressure;
        }
      }
    }
  }   
  OPSCH *opsch = OP_opsch(op, _hb_map);
  OPSCH_regcost(opsch) = cost;
}

// ======================================================================
// Update the number of registers available for allocation after <op> is
// scheduled.
// ======================================================================
void
HB_Schedule::Update_Regs_For_OP (OP *op)
{
  REG_ENTRY reginfo;

  INT i;
  for (i = 0; i < OP_results(op); i++) {
    TN *result_tn = OP_result(op, i);
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, result_tn);
    REG_ENTRY_def_count(reginfo)--;
    if (REG_ENTRY_def_count(reginfo) == 0 &&
	REG_ENTRY_reg_assigned(reginfo)) 
    {
      ISA_REGCLASS cl = TN_register_class(result_tn);
      _Cur_Regs_Avail[cl]++;
      REG_ENTRY_reg_assigned(reginfo) = FALSE;
    }
    hTN_MAP_Set (_regs_map, result_tn, REG_ENTRY_ptr(reginfo));
  }
  for (i = 0; i < OP_opnds(op); i++) {
    TN *opnd_tn = OP_opnd(op,i);
    if (TN_is_constant(opnd_tn)) continue;
    REG_ENTRY_ptr(reginfo) = hTN_MAP_Get (_regs_map, opnd_tn);
    if (!REG_ENTRY_reg_assigned(reginfo)) {
      ISA_REGCLASS cl = TN_register_class(opnd_tn);
      _Cur_Regs_Avail[cl]--;
      REG_ENTRY_reg_assigned(reginfo) = TRUE;
      hTN_MAP_Set (_regs_map, opnd_tn, REG_ENTRY_ptr(reginfo));
    }
  }
}

// ======================================================================
// Return TRUE if <op1> and <op2> are addiu and load/store instructions
// such that the addiu and the load/store can be interchanged.
// ======================================================================
BOOL
Is_Ldst_Addiu_Pair (OPSCH *opsch1, OPSCH *opsch2, OP *op1,OP *op2)
{
  OP *addiu_op;
  OP *ldst_op;
  INT64 multiplier;

  if (((OPSCH_flags(opsch1) | OPSCH_flags(opsch2)) & OPSCH_ADDIU_LDST_PAIR) !=
      OPSCH_ADDIU_LDST_PAIR) 
  {
    return FALSE;
  }

  if (OPSCH_addiu(opsch1)) {
    addiu_op = op1;
    ldst_op = op2;
    multiplier = 1;
  }
  else {
    addiu_op = op2;
    ldst_op = op1;
    multiplier = -1;
  }

  // Check that the result of the addiu is the same as the base of the ldst.
  // Also check that if the memory OP is a store, the source is not the same
  // as the result of the addiu.
  INT base_opndnum = Memory_OP_Base_Opndnum(ldst_op);
  if ((base_opndnum == -1) ||
      (OP_result(addiu_op,0 /*???*/) != OP_opnd(ldst_op,base_opndnum)) ||
      (OP_store(ldst_op) &&
       (OP_result(addiu_op,0 /*???*/) == OP_opnd(ldst_op,0))))
  {
    return FALSE;
  }

  INT64 addiu_const = TN_value (OP_opnd(addiu_op, 1));
  INT offset_opndnum = Memory_OP_Offset_Opndnum(ldst_op);
  if (offset_opndnum== -1)
    return FALSE;
  if (!TN_has_value (OP_opnd(ldst_op, offset_opndnum)))
    return FALSE;
  INT64 ldst_const = TN_value (OP_opnd(ldst_op, offset_opndnum));

  return TI_TOP_Can_Have_Immediate (ldst_const + addiu_const*multiplier, OP_code(ldst_op));
}

// ======================================================================
// Change the offset field in load/store OP after it has been moved across
// an addiu OP that defines the base register for the <ldst_op>. The 
// <multiplier> can be either +1 or -1 to indicate direction of movement.
// ======================================================================
void
Fixup_Ldst_Offset (OP *ldst_op, INT64 addiu_const, INT64 multiplier, 
		   HBS_TYPE type)
{
  TN *old_ofst_tn, *ofst_tn;
  INT index;

  index = Memory_OP_Offset_Opndnum (ldst_op);

  if (index == -1)
    return;

  old_ofst_tn = OP_opnd(ldst_op, index);
  if (!TN_has_value(old_ofst_tn))
    return;

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "old: %" LLD_FMT ", new: %" LLD_FMT "\n", TN_value(old_ofst_tn), 
		      TN_value(old_ofst_tn) + addiu_const * multiplier);
    fprintf (TFile, "offset changed:");
    Print_OP_No_SrcLine (ldst_op);
  }

  ofst_tn = Gen_Literal_TN (TN_value(old_ofst_tn) + addiu_const * multiplier,
			    TN_size(old_ofst_tn));
  Set_OP_opnd (ldst_op, index, ofst_tn);

}

// ======================================================================
// Traverse through the list of scheduled instructions and look for load
// or store OPs that have been moved across corresponding addiu OPs. For
// all such load/store OPs, adjust their offset field.
// ======================================================================
void
HB_Schedule::Adjust_Ldst_Offsets (void)
{
  for (INT i = VECTOR_count(_sched_vector)-1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Set_OPSCH_visited (opsch);
    if (!OPSCH_addiu (opsch)) continue;
    INT64 addiu_const = TN_value (OP_opnd(op,1));
    ARC_LIST *arcs;
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);
      if (OPSCH_ldst (succ_opsch) && OPSCH_visited (succ_opsch)) {
	Fixup_Ldst_Offset (succ_op, addiu_const, +1, type());
      }
    }
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);
      if (OPSCH_ldst (pred_opsch) && !OPSCH_visited (pred_opsch)) {
	Fixup_Ldst_Offset (pred_op, addiu_const, -1, type());
      }
    }
  }
}

// ======================================================================
// Set_Resource_Usage
//
// Given an 'op' and a start 'cycle', reserve all the resources needed
// to schedule the op at that cycle. It is assumed that we have already
// verified that the resources are available.
// ======================================================================
void
HB_Schedule::Set_Resource_Usage (OP *op)
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  INT cycle = OPSCH_scycle (opsch);
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TOP alt_topcode = TOP_UNDEFINED;
  
  if (Convert_TNs_To_INT(op, tn_num_array))
    alt_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY) -1,
				tn_num_array);
  TI_RES_RES_Reserve_Resources(_rr_tab, OP_code(op), cycle, alt_topcode);
  Clock = cycle;
  INT issue_alignment = OPSCH_issue_alignment(opsch);
  if (issue_alignment != 1) {
    Last_aligned_cycle = Clock;
    Last_issue_alignment = issue_alignment;
  }
}

// Check if there are any resource conflicts in scheduling 'op' at
// 'cycle'. Returns TRUE if all required resources can be reserved.
BOOL 
HB_Schedule::Check_Resource_Usage(OP *op, INT cycle) {
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TOP alt_topcode = TOP_UNDEFINED;
  
  if (Convert_TNs_To_INT(op, tn_num_array))
    alt_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY) -1,
				tn_num_array);

  if (TI_RES_RES_Resources_Available(_rr_tab, OP_code(op), cycle, alt_topcode)==false)
    return false;

  // check stalls against scheduled ops
  INT sched_count = VECTOR_count(_sched_vector);

  for (INT i = sched_count-1; i >= 0; i--) {
    OP *sched_op = OP_VECTOR_element(_sched_vector,i);
    OPSCH *sched_opsch = OP_opsch (sched_op, _hb_map);
    INT sched_scycle = OPSCH_scycle(sched_opsch);
    if (cycle==sched_scycle)
      continue;

    OPSCH *opsch = OP_opsch (op, _hb_map);
    if ((sched_scycle<cycle && sched_scycle+OP_Stall(sched_op,op)+1>cycle) ||
	(sched_scycle>cycle && sched_scycle-OP_Stall(op,sched_op)-1<cycle))
	return false;
  }

  return true;
}

// ======================================================================
// Find_Schedule_Cycle
//
// Find the cycle in which 'op' can be scheduled. Also, update the scycle
// for the OP so that future searches are faster.
// ======================================================================
INT
HB_Schedule::Find_Schedule_Cycle (OP *op, BOOL is_fwd)
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  INT cycle = OPSCH_scycle(opsch);
  INT cyc;
  INT issue_alignment = OPSCH_issue_alignment(opsch);

  if (is_fwd) {

    INT init_cycle = cycle;
    INT delta = 1;
    if (issue_alignment == Last_issue_alignment) {
      INT next_aligned_cycle = Last_aligned_cycle+issue_alignment;
      while (init_cycle > next_aligned_cycle)
	next_aligned_cycle += issue_alignment;
      init_cycle = next_aligned_cycle;
      delta = issue_alignment;
    }

    // keep looking forward till we can schedule the op.
    for (cyc = init_cycle; cyc <= MAX_Clock; cyc+=delta) {
      if (Check_Resource_Usage (op, cyc)) break;
    }
    FmtAssert (cyc <= MAX_Clock, ("HB_SCHED: no valid cycle for scheduling"));
  } else {

    INT init_cycle = cycle;
    INT delta = 1;
    if (issue_alignment == Last_issue_alignment) {
      INT next_aligned_cycle = Last_aligned_cycle-issue_alignment;
      while (init_cycle < next_aligned_cycle)
	next_aligned_cycle -= issue_alignment;
      init_cycle = next_aligned_cycle;
      delta = issue_alignment;
    }

    // Keep looking back till we can schedule the op.
    for (cyc = init_cycle; cyc >= 0; cyc-=delta) {
      if (Check_Resource_Usage (op, cyc)) break;
    }
    FmtAssert (cyc >= 0, ("HB_SCHED: no valid cycle for scheduling"));
  }

  // update the scycle for the OP.
  OPSCH_scycle(opsch) = cyc;
  return cyc;
}

#define MAX_XBB_DELAY 5  // used to set up resource resevation table

static void Init_OPSCH_CROSS_BB_LATENCY(BB *bb, BB_MAP value_map)
{
  // note: what about self-cycle block ? schedule it twice ?

  /*
     Given an OP 'op' in block bb, it may depend upon OPs in the 
     predecessors of bb; it may also produce values that are used
     in OPs in the successors of bb.  This class is used to keep
     track of those dependence information.  

     All OPs upon which 'op' is dependent in bb's predecessors are
     recorded by a chain of objects of OP_DEP_Info, and each object
     records one dependence. Similarly, all OPs that use what the 'op' 
     produces are also recorded by a chain of objects of OP_DEP_Info. 

     Not all dependence relations are recorded. Only those that could
     introduce stalls are recorded.  For example:

      pred:                        bb:
        1.  tn1 = ...                1.  tn1 = ...
             ...                          ...
        2.     = tn1                 2.    = tn1
      bb:   ...                    succ: ...
        3.   = tn1                   3.     = tn1

        Dependence b/w 1 and 3 is not recorded because it does not
        introduce any stalls. (The delay between 1 and 3 is hidden
        by the dependence b/w 1 and 2 completely.)

     Based on those dependence information, estart_offset and lstart_offset
     is calculated, and those values are further used to adjust estart
     and lstart in Init_OPSCH_For_BB and Compute_Bkwd_OPSCH.

     For each OP, the chain of objects is not saved, it is deleted 
     after estart_offset/lstart_offset of the OP is calculated. 
   */
  class OP_DEP_Info {
    private:
      TN  *_tn;
      int _def_tn_ix;
      int _use_tn_ix;
      union {
        OP  *_def_op;
        OP  *_use_op;
      } u1;
      union {
        BB  *_def_bb;
        BB  *_use_bb;
      } u2;
      OP_DEP_Info *_next;
     

    public:
      OP_DEP_Info(BB *bb, OP *op, TN *tn, 
                  int def_tn_ix, int use_tn_ix,
                  bool is_pred) 
        : _tn(tn),
          _def_tn_ix (def_tn_ix),
          _use_tn_ix (use_tn_ix)
      {
        _next = NULL;
        if (is_pred) {
          u1._def_op = op;
          u2._def_bb = bb;
        } else {
          u1._use_op = op;
          u2._use_bb = bb;
        }
      }  


      ~OP_DEP_Info() { _next = NULL; }

      TN *ODI_tn() const        { return _tn; }
      void ODI_Set_tn(TN *tn)    { _tn = tn; }
      int ODI_def_tn_ix() const { return _def_tn_ix; }
      void ODI_Set_def_tn_ix(int ix) { _def_tn_ix = ix; }
      int ODI_use_tn_ix() const { return _use_tn_ix; }
      void ODI_Set_use_tn_ix(int ix) { _use_tn_ix = ix; }
      OP *ODI_def_op() const    { return u1._def_op; }
      void ODI_Set_def_op(OP *op) { u1._def_op = op; }
      BB *ODI_def_bb() const    { return u2._def_bb; }
      void ODI_Set_def_bb(BB *b) { u2._def_bb = b; }
      OP *ODI_use_op() const    { return u1._use_op; }
      void ODI_Set_use_op(OP *op) { u1._use_op = op; }
      BB *ODI_use_bb() const    { return u2._use_bb; }
      void ODI_Set_use_bb(BB *b) { u2._use_bb = b; }

      OP_DEP_Info *ODI_next() const   { return _next; }
      void ODI_Set_next(OP_DEP_Info *o)
                            { _next = o; }

      static void chain_delete(OP_DEP_Info *head) {
        OP_DEP_Info *tmp = head;
        while(tmp != NULL) {
          OP_DEP_Info *t = tmp;
          tmp = tmp->_next;
          CXX_DELETE(t, &MEM_local_pool);
        }
      }

  };

  BBLIST *bb_listp;
  OP *op;
  ARC_LIST *arcs;

  OP_DEP_Info odi_sentinel(NULL, NULL, NULL, -1, -1, false);
  OP_DEP_Info *odi_head = &odi_sentinel;
  OP_DEP_Info *odi_tail = NULL;
  odi_head->ODI_Set_next(NULL);

  // memory for temporary use only
  MEM_POOL_Push(&MEM_local_pool);

  // Processing predecessors
  INT num_opnds = TI_ISA_Operand_Max();
  bool tn_opnd_array[num_opnds];
  FOR_ALL_BB_OPs(bb, op) {
    OP_DEP_Info::chain_delete(odi_head->ODI_next());
    odi_head = &odi_sentinel;
    odi_tail = odi_head;
    odi_tail->ODI_Set_next(NULL);

    int i;
    for (i=0; i < num_opnds; i++) tn_opnd_array[i] = true;
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      if ( ARC_kind(arc) == CG_DEP_REGIN ) {
        //  Mark it as this TN does not need to be considered.
        tn_opnd_array[ARC_opnd(arc)] = false;
      }
    }

    for (i=0; i < OP_opnds(op); i++) {
      if (!tn_opnd_array[i]) continue;
      TN *tmp_tn = OP_opnd(op, i);
      /* find tmp_tn's def OP in bb's pred */
      FOR_ALL_BB_PREDS(bb, bb_listp) {
        BB *pred_bb = BBLIST_item(bb_listp);
        OP *pred_op;

        // Only consider preds that have been scheduled already.
        if (!BB_scheduled(pred_bb)) continue;

        FOR_ALL_BB_OPs_REV(pred_bb, pred_op) {
          INT res_ix = TN_Resnum_In_OP (pred_op, tmp_tn);
          if (res_ix != -1) {
            OP_DEP_Info *new_odi = CXX_NEW (
                OP_DEP_Info(pred_bb, pred_op, tmp_tn, res_ix, i, true), &MEM_local_pool);
            odi_tail->ODI_Set_next(new_odi);
            odi_tail = new_odi;

            break;
          } else {
            if (TN_Opernum_In_OP(pred_op, tmp_tn) != -1) {
               // no need to consider this tmp_tn
               break;
            }
          }
        }
      }
    }

    // Set up OPSCH's estart_offset 
    OP_DEP_Info *odi;
    INT16 latency = 0;
    for (odi = odi_head->ODI_next(); odi != NULL; odi = odi->ODI_next())
    {
      INT16 l = 
         CG_DEP_Latency(odi->ODI_def_op(), op, CG_DEP_REGIN, odi->ODI_use_tn_ix()); 
      BB *def_bb = odi->ODI_def_bb();
      OP *last_op = BB_last_op(def_bb);
      Is_True(last_op, ("BB has no last OP"));
      INT16 branch_cost = 0;
      if (OP_code(last_op) != TOP_loop_end) {
        branch_cost = (BB_next(def_bb) != bb) ? 1 : 0;
      }
      INT16 scycles = OP_scycle(last_op) - OP_scycle(odi->ODI_def_op()) +
                      branch_cost + 1; // 1 is the cost for ODI_def_op()
      if (scycles < 0) scycles = 0; // to be safe
      l -= scycles;
      if (l < 0) l = 0;
      if (l > latency) {
        latency = l;
        if (Trace_HB) {
          #pragma mips_frequency_hint NEVER
          fprintf (TFile, "\n   XBB-Latency BB[%d]: From BB[%d], latency %d\n",
                   bb->id, def_bb->id, l);
          fprintf (TFile, "   Src (From) OP: ");
          Print_OP_No_SrcLine (odi->ODI_def_op());
        }
      }
    }

    // Make sure estart_offset be not bigger than MAX_XBB_DELAY
    OPSCH *opsch = OP_opsch(op, value_map);
    OPSCH_estart_offset(opsch) = (latency > MAX_XBB_DELAY) ? MAX_XBB_DELAY : latency;

    if (Trace_HB && latency > 0) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, " XBB-Latency BB[%d]: To ", bb->id);
      Print_OP_No_SrcLine (op);
      fprintf (TFile, "\n");
    }
  }

  // for successors
  INT num_res = TI_ISA_Result_Max();
  bool tn_res_array[num_res];

  FOR_ALL_BB_OPs(bb, op) {
    OP_DEP_Info::chain_delete(odi_head->ODI_next());
    odi_head = &odi_sentinel;
    odi_tail = odi_head;
    odi_tail->ODI_Set_next(NULL);

    int i;
    for (i=0; i < num_res; i++) tn_res_array[i] = true;
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      TN *tn;
      if ( ARC_kind(arc) == CG_DEP_REGIN ) {
        tn = OP_opnd(ARC_succ(arc), ARC_opnd(arc));
        int res_ix = TN_Resnum_In_OP(op, tn);
        if (res_ix < 0 ) {
          Lmt_DevWarn(1,("A REGIN dep does not have a pred !"));
        } else {
          tn_res_array[res_ix] = false;
        }
      } else if ( ARC_kind(arc) == CG_DEP_REGOUT ) {
        for (i=0; i < OP_results(op); i++) {
          if (tn_res_array[i]) {
            tn = OP_result(op, i);
            if ( TN_Resnum_In_OP(ARC_succ(arc), tn) >= 0 ) {   
              tn_res_array[i] = false;
            }
          }
        }
      }
    }

    for (i=0; i < OP_results(op); i++) {
      if (!tn_res_array[i]) continue;
      TN *tmp_tn = OP_result(op, i);
      /* find tmp_tn's use OP in bb's succ */
      FOR_ALL_BB_SUCCS(bb, bb_listp) {
        BB *succ_bb = BBLIST_item(bb_listp);
        OP *succ_op;

        // Only consider scheduled succs
        if (!BB_scheduled(succ_bb)) continue;

        // If it is a self-cycle bb, don't consider it again
        // because it is considered in a predecessor case.
        // (note: this is only useful if the bb is scheduled twice)
        if (bb == succ_bb) continue;

        FOR_ALL_BB_OPs(succ_bb, succ_op) {
          INT use_ix = TN_Opernum_In_OP(succ_op, tmp_tn);
          if (use_ix != -1) {
            OP_DEP_Info *new_odi = CXX_NEW (
                OP_DEP_Info(succ_bb, succ_op, tmp_tn, i, use_ix, false), &MEM_local_pool);
            odi_tail->ODI_Set_next(new_odi);
            odi_tail = new_odi;

            break;
          } else {
            if (TN_Resnum_In_OP(succ_op, tmp_tn) != -1) {
               // no need to consider this tmp_tn
               break;
            }
          }
        }
      }
    }

    // Set up OPSCH's estart at this time.
    OP_DEP_Info *odi;
    INT16 latency = 0;
    for (odi = odi_head->ODI_next(); odi != NULL; odi = odi->ODI_next())
    {
      INT16 l = 
         CG_DEP_Latency(op, odi->ODI_use_op(), CG_DEP_REGIN, odi->ODI_use_tn_ix()); 
      BB *use_bb = odi->ODI_use_bb();
      OP *first_op = BB_first_op(use_bb);
      Is_True(first_op, ("BB has no first OP"));
      INT16 branch_cost = 0;
      if (OP_code(BB_last_op(bb)) != TOP_loop_end) {
        branch_cost = (BB_next(bb) != use_bb) ? 1 : 0;
      }
      INT16 scycles = OP_scycle(odi->ODI_use_op()) - OP_scycle(first_op) +
                      branch_cost + 1;  // 1 is cost for op
      if (scycles < 0) scycles = 0; // to be safe
      l -= scycles;
      if (l < 0) l = 0;
      if (l > latency) {
        latency = l;
        if (Trace_HB) {
          #pragma mips_frequency_hint NEVER
          fprintf (TFile, "\n   XBB-Latency BB[%d]: To BB[%d], latency %d\n",
                   bb->id, use_bb->id, l);
          fprintf (TFile, "   Dest (To) OP: ");
          Print_OP_No_SrcLine (odi->ODI_use_op());
        }
      }
    }

    // Make sure lstart_offset be not bigger than MAX_XBB_DELAY
    OPSCH *opsch = OP_opsch(op, value_map);
    OPSCH_lstart_offset(opsch) = (latency > MAX_XBB_DELAY) ? MAX_XBB_DELAY : latency;

    if (Trace_HB && latency > 0) {
      #pragma mips_frequency_hint NEVER
      fprintf (TFile, " XBB-Latency BB[%d]: From ", bb->id);
      Print_OP_No_SrcLine (op);
      fprintf (TFile, "\n");
    }
  }
  
  OP_DEP_Info::chain_delete(odi_head->ODI_next());

  MEM_POOL_Pop (&MEM_local_pool);
  
}

static INT cur_dfsnum;

// ======================================================================
// Initialize the OPSCH data structure for each OP in <bb>. Identify
// OPs with the OPSCH_addiu, OPSCH_ldst, OPSCH_def_xfer_opnd attributes.
// ======================================================================
static void
Init_OPSCH_For_BB (BB *bb, BB_MAP value_map, MEM_POOL *pool, bool do_xblock_latency)
{
  OP *op;
  ARC_LIST *arcs;
  ARC *arc;
  OPSCH *opsch;

  FOR_ALL_BB_OPs_FWD (bb, op) {
    opsch = TYPE_MEM_POOL_ALLOC (OPSCH, pool);
    memset(opsch, 0, sizeof (OPSCH));
    OPSCH_lstart(opsch) = 0x7fff;
    BB_OP_MAP bb_map = (BB_OP_MAP) BB_MAP_Get(value_map, bb);
    BB_OP_MAP_Set (bb_map, op, opsch);
  }

  if (do_xblock_latency) {
    // During a local scheduling, consider cross-block dependence
    Init_OPSCH_CROSS_BB_LATENCY(bb, value_map);
  }

  FOR_ALL_BB_OPs_FWD (bb, op) {
    opsch = OP_opsch(op, value_map);
    // Identify LDST/ADDIU instructions with non-relocatable offsets.
    if (CGTARG_Is_OP_Addr_Incr(op) && 
	!TN_is_sp_reg(OP_result(op,0 /*???*/))) {
	
      BOOL addiu_ok = TRUE;
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGOUT) {
	  addiu_ok = FALSE;
	  break;
	}
      }
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  addiu_ok = FALSE;
	  break;
	}
      }
      if (addiu_ok) Set_OPSCH_addiu (opsch);
    }
    else if (OP_memory(op)) {
      // check if the memory OP has an offset field (i.e. it is not an 
      // indexed load/store/prefx.
      INT offset_opndnum = Memory_OP_Offset_Opndnum (op);
      if ((offset_opndnum != -1) && TN_has_value(OP_opnd(op,offset_opndnum))) {
	Set_OPSCH_ldst (opsch);
      }
    }

    OPSCH_issue_alignment(opsch) = TI_TOP_Issue_Alignment(OP_code(op));
    OPSCH_use_shared_resource(opsch) =
	    		TI_ISA_Property_Set (PROP_use_shared_resource, OP_code(op));
    OPSCH_estart(opsch) = OPSCH_estart_offset(opsch);

#ifdef TARG_MIPS
    if (Is_Target_T5() && OP_xfer(op) && Get_Trace (TP_SCHED, 0x1000)) {
      for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc) == CG_DEP_REGIN) {
	  OP *pred_op = ARC_pred(arc);
	  OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
	  Set_OPSCH_def_xfer_opnd(pred_opsch);
	}
      }
    }
#endif
  }
}

// ======================================================================
// return TRUE if opsch1 has a larger 'estart' value than opsch2.
// ======================================================================
static BOOL
sort_by_estart (const void *opsch1, const void *opsch2)
{
  return (OPSCH_estart((OPSCH*) opsch1) > OPSCH_estart((OPSCH*) opsch2));
}
 
// ======================================================================
// return TRUE if opsch1 has a smaller 'slack' value than opsch2.
// ======================================================================
static BOOL
sort_by_slack (const void *opsch1, const void *opsch2)
{
  INT slack1 = OPSCH_lstart((OPSCH*) opsch1) - OPSCH_estart((OPSCH*) opsch1);
  INT slack2 = OPSCH_lstart((OPSCH*) opsch2) - OPSCH_estart((OPSCH*) opsch2);
 
  return ((OPSCH*) slack1 < (OPSCH*) slack2);
}
 
// ======================================================================
// Calculate_Adjust_Latency
// Placeholder to make all latency adjustments (both static/OOO effects).
// ======================================================================
static inline INT
Calculate_Adjust_Latency (ARC *arc)
{
  INT adjust_latency;
  // OOO adjustments necessary (or not)
  BOOL ooo_adjust = (TI_PROC_Property_Set(PROP_is_out_of_order)
		     && CG_DEP_Adjust_OOO_Latency);

  // For OOO machine (eg. T5), non-definite memory dependences can be 
  // relaxed to edges with zero latency. The belief is that this can 
  // help avoid creating false dependences with biased critical info.

  adjust_latency = (ooo_adjust && ARC_is_mem(arc) &&
		    !ARC_is_definite(arc)) ? 0 : ARC_latency(arc);

  // Similary, anti-dependences (reg) can be relaxed as well. Since, OOO
  // machines do dynamic renaming, having static estimates due to such
  // dependences can be avoided.
  adjust_latency = (ooo_adjust && ARC_is_reg(arc) &&
		    (ARC_is_anti(arc) || ARC_is_output(arc))) ?
		    0 : adjust_latency;
#ifdef TARG_XTENSA
  // For VLIW Xtensa, minimum latency between instructions with an output 
  // dependency is 1 cycle 
  if (ARC_is_reg(arc) && ARC_is_output(arc) && (adjust_latency < 1))
    adjust_latency = 1;
#endif
  return adjust_latency;
}

// ======================================================================
// Recursive depth first search of the dep-graph starting at <op>. The 
// OPSCH_dfsnum field is marked as each OP is visited.
// ======================================================================
static void
DFS_Search (OP *op, BB_MAP value_map, BOOL is_fwd)
{
  OPSCH *opsch = OP_opsch (op, value_map);
  ARC_LIST *arcs;
  INT num_arcs=0;
  INT num_reg_arcs=0;
  INT max_usage=0;
  INT total_usage=0;
  INT max_inst_dist=0;
  INT max_cyc_dist=1;
  INT num_splits=0;
  INT num_split_arcs=0;
  INT num_saved=0;

  if (OPSCH_visited(opsch)) return;

  OPSCH_dfsnum(opsch) = cur_dfsnum;
  Set_OPSCH_visited(opsch);
  Set_OPSCH_tree(opsch);
  cur_dfsnum++;

  if (is_fwd) {
    // visit all the successors.
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, value_map);
      if (!OPSCH_visited(succ_opsch)) DFS_Search (succ_op, value_map, TRUE);
      if (OPSCH_num_unique_preds(succ_opsch)>1)
	Reset_OPSCH_tree(opsch);
      if (ARC_kind(arc)==CG_DEP_REGIN) {
        num_arcs++;
	TN* reg_tn = OP_opnd(ARC_succ(arc),ARC_opnd(arc));
	if (TN_is_register(reg_tn) &&
	    !TN_is_global_reg(reg_tn) && !TN_is_dedicated(reg_tn) &&
	    !TI_ISA_Regclass_Is_State(TN_register_class(reg_tn))) {
	  if (OPSCH_num_unique_preds(succ_opsch)!=1)
	    num_split_arcs++;
	  else
	    num_reg_arcs++;
	}
      }
      INT arc_latency = Calculate_Adjust_Latency(arc);
      INT issue_alignment_delay = (aligned_op_count<2) ? 
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(succ_opsch));
      arc_latency = MAX(arc_latency, issue_alignment_delay);
      if (OPSCH_cyc_dist(succ_opsch)+arc_latency >= max_cyc_dist)
	max_cyc_dist = OPSCH_cyc_dist(succ_opsch) + arc_latency;
      if (OPSCH_inst_dist(succ_opsch)>max_inst_dist)
	max_inst_dist = OPSCH_inst_dist(succ_opsch);
      if (OPSCH_num_unique_preds(succ_opsch)==1) {
	if (ARC_kind(arc)==CG_DEP_REGIN) {
	  if (OPSCH_min_usage(succ_opsch)>max_usage)
	    max_usage = OPSCH_min_usage(succ_opsch);
	  total_usage += OPSCH_total_usage(succ_opsch);
	}
	if (ARC_LIST_Find_Succ(ARC_LIST_rest(arcs), succ_op) == NULL) {
	  num_splits += OPSCH_num_splits(succ_opsch);
	}
      }
    }
//      if (OPSCH_num_unique_preds(opsch)>1 || !OPSCH_tree(opsch))
//        for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
//          ARC *arc = ARC_LIST_first(arcs);
//          OP *pred_op = ARC_pred(arc);
//          OPSCH *pred_opsch = OP_opsch (pred_op, value_map);
//            Reset_OPSCH_tree(pred_opsch);
//        }
  } else {
    // visit all the predecessors.
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch (pred_op, value_map);
      if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	if (!OPSCH_visited(pred_opsch)) DFS_Search (pred_op, value_map, FALSE);
	if (OPSCH_num_unique_succs(pred_opsch)>1)
	  Reset_OPSCH_tree(opsch);
	if (ARC_kind(arc)==CG_DEP_REGIN) {
          num_arcs++;
	  TN* reg_tn = OP_opnd(ARC_succ(arc),ARC_opnd(arc));
	  if (TN_is_register(reg_tn) &&
	      !TN_is_global_reg(reg_tn) && !TN_is_dedicated(reg_tn) &&
	      !TI_ISA_Regclass_Is_State(TN_register_class(reg_tn))) {
	    if (OPSCH_num_unique_succs(pred_opsch)!=1)
	      num_split_arcs++;
	    else
	      num_reg_arcs++;
	  }
	}
	INT arc_latency = Calculate_Adjust_Latency(arc);
	INT issue_alignment_delay = (aligned_op_count<2) ? 
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(pred_opsch));
	arc_latency = MAX(arc_latency, issue_alignment_delay);
	if (OPSCH_cyc_dist(pred_opsch)+arc_latency >= max_cyc_dist)
	  max_cyc_dist = OPSCH_cyc_dist(pred_opsch) + arc_latency;
	if (OPSCH_inst_dist(pred_opsch)>max_inst_dist)
	  max_inst_dist = OPSCH_inst_dist(pred_opsch);
	if (OPSCH_num_unique_succs(pred_opsch)==1) {
	  if (ARC_kind(arc)==CG_DEP_REGIN) {
	    if (OPSCH_min_usage(pred_opsch)>max_usage)
	      max_usage = OPSCH_min_usage(pred_opsch);
	    total_usage += OPSCH_total_usage(pred_opsch);
	  }
	  if (ARC_LIST_Find_Pred(ARC_LIST_rest(arcs), pred_op) == NULL) {
	    num_splits += OPSCH_num_splits(pred_opsch);
	  }
	}
      }
    }
//      if (OPSCH_num_unique_succs(opsch)>1 || !OPSCH_tree(opsch))
//        for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
//          ARC *arc = ARC_LIST_first(arcs);
//          OP *succ_op = ARC_succ(arc);
//          OPSCH *succ_opsch = OP_opsch (succ_op, value_map);
//            Reset_OPSCH_tree(succ_opsch);
//        }
  }
  if (!Get_Trace(TP_SCHED, 0x800) && !is_fwd) HB_reg_graph.add_op_to_graph(op);
  INT num_regs = 0;
  INT i;
  if (is_fwd)
    for (i=0; i<OP_opnds(op); i++) {
      TN* reg_tn = OP_opnd(op,i);
      if (TN_is_register(reg_tn) &&
	  !TN_is_global_reg(reg_tn) && !TN_is_dedicated(reg_tn) &&
	  !TI_ISA_Regclass_Is_State(TN_register_class(reg_tn))) {
	num_regs++;
      }
    }
  else
    for (i=0; i<OP_results(op); i++) {
      TN* reg_tn = OP_result(op,i);
      if (TN_is_register(reg_tn) &&
	  !TN_is_global_reg(reg_tn) && !TN_is_dedicated(reg_tn) &&
	  !TI_ISA_Regclass_Is_State(TN_register_class(reg_tn))) {
	num_regs++;
      }
    }
  if (!Get_Trace(TP_SCHED, 0x400)) {
    OPSCH_min_usage(opsch)=max_usage+num_reg_arcs;
    OPSCH_total_usage(opsch)=total_usage+num_reg_arcs+num_split_arcs;
    OPSCH_live_vals(opsch)=num_splits+num_split_arcs-num_regs;
    OPSCH_new_live_vals(opsch)=num_split_arcs+num_reg_arcs-num_regs;
    OPSCH_num_splits(opsch)=num_splits+num_split_arcs;
    OPSCH_inst_dist(opsch)=max_inst_dist+1;
    OPSCH_cyc_dist(opsch)=max_cyc_dist;
  } else {
    if (num_arcs>0) {
      OPSCH_min_usage(opsch)=max_usage+num_arcs-1;
      OPSCH_total_usage(opsch)=total_usage;
      OPSCH_inst_dist(opsch)=max_inst_dist+1;
      OPSCH_cyc_dist(opsch)=max_cyc_dist;
    } else {
      OPSCH_min_usage(opsch)=num_regs;
      OPSCH_total_usage(opsch)=num_regs;
      OPSCH_inst_dist(opsch)=1;
      OPSCH_cyc_dist(opsch)=1;
    }
  }
}

// ======================================================================
// Compute depth first ordering.
// ======================================================================
static void
Compute_DFO (HB_Schedule *sched, BB_MAP value_map, BOOL is_fwd)
{
  INT i;

  cur_dfsnum = 1;
  for (i = 0; i < VECTOR_count(sched->ready_vector()); i++) {
    DFS_Search (OP_VECTOR_element(sched->ready_vector(), i), value_map,is_fwd);
  }
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Add_Element_Sorted (VECTOR vector, void *element, VECTOR_ELEMENT_COMPARE comp_func)
{
  INT i;
  INT count = VECTOR_count(vector);
  FmtAssert (count < VECTOR_size(vector), ("VECTOR overflow"));
  for (i = count; i > 0; i--) {
    void *cur_element = VECTOR_element(vector, i - 1);
    void *cur_opsch = OP_opsch((OP*) cur_element, _cur_sched->hb_map());
    void *opsch = OP_opsch((OP*) element, _cur_sched->hb_map());
    if (comp_func(cur_opsch, opsch)) break;
    VECTOR_element(vector, i) = cur_element;
  }
  VECTOR_element(vector, i) = element;
  count++;
  VECTOR_count(vector) = count;
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Build_Ready_Vector (BB* bb, BOOL is_fwd)
{
  OP *op;

  if (is_fwd) {
    FOR_ALL_BB_OPs_FWD (bb, op) {
      OPSCH *opsch = OP_opsch(op, _cur_sched->hb_map());
      // Add it to the ready vector if there are no successors.
      if (OPSCH_num_preds(opsch) == 0) {
	Add_Element_Sorted (_cur_sched->ready_vector(), op, sort_by_slack);
      }
    }
  } else {
    FOR_ALL_BB_OPs_REV (bb, op) {
      OPSCH *opsch = OP_opsch(op, _cur_sched->hb_map());
      // Add it to the ready vector if there are no successors.
      if (OPSCH_num_succs(opsch) == 0) {
	Add_Element_Sorted (_cur_sched->ready_vector(), op, sort_by_estart);
      }
    }
  }
}

// ======================================================================
// Traverse the OPs in the basic block backwards and build a list of OPs
// that are ready to schedule. 
// Sort the ready vector in decreasing order or 'estart'.
// ======================================================================
void
Priority_Selector::Build_Ready_Vector (list<BB*> bblist, BOOL is_fwd)
{

  list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD (bblist, bb_iter) {
    Build_Ready_Vector (*bb_iter, is_fwd);
  }
}

// ======================================================================
// Return the maximum number of cycles in which a resource is used for
// the OP. This is used to estimate the size of the resource table
// required. It could also be used as a coarse estimate of the latency
// of an OP that has no successors.
// ======================================================================
inline INT
Resource_Cycles_For_OP (OP *op)
{
  return TI_RES_Cycle_Count(OP_code(op));
}

// ======================================================================
// Do a forward pass and compute the OPSCH data structure for the bb.
// ======================================================================
static void
Compute_Fwd_OPSCH (BB *bb, BB_MAP value_map, INT *max_lstart, MEM_POOL *pool)
{
  OP *op;
  TI_RES_RES* tmp_tab = TI_RES_RES_Alloc(FALSE, pool);
  TI_RES_RES_Set_BB_Cycle_Count(tmp_tab, MAX_Clock);

  // Initialize the OPSCH_estart and OPSCH_num_succs fields of all OPs.
  // Also compute the max_lstart.
  FOR_ALL_BB_OPs_FWD (bb, op) {
    OPSCH *opsch = OP_opsch(op, value_map);
    INT op_estart = OPSCH_estart(opsch);
    ARC_LIST *arcs;
    INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
    INT tn_num_array[array_size];

    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
      TOP alt_topcode = TOP_UNDEFINED;
      if (Convert_TNs_To_INT(pred_op, tn_num_array))
	alt_topcode = TI_Convert_OP(OP_code(pred_op),
				    (ISA_EXEC_UNIT_PROPERTY) -1, tn_num_array);

      if (!OPSCH_flag1(pred_opsch)) {
	Set_OPSCH_flag1(pred_opsch);
	INT cycle = OPSCH_estart(pred_opsch);
	while (!TI_RES_RES_Resources_Available(tmp_tab, OP_code(pred_op), 
					       cycle, alt_topcode))
	  cycle++;
	TI_RES_RES_Reserve_Resources(tmp_tab, OP_code(pred_op), cycle, 
				     alt_topcode);
	op_estart = MAX(op_estart, Calculate_Adjust_Latency(arc) + cycle);
      }
    }
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
      Reset_OPSCH_flag1(pred_opsch);
    }
    TOP alt_topcode = TOP_UNDEFINED;
    if (Convert_TNs_To_INT(op, tn_num_array))
      alt_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY) -1,
				  tn_num_array);
    while (!TI_RES_RES_Resources_Available(tmp_tab, OP_code(op), op_estart, 
					   alt_topcode))
      op_estart++;
    TI_RES_RES_Clear(tmp_tab);
    OPSCH_estart(opsch) = op_estart;

    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch(succ_op, value_map);
      if (!Is_Ldst_Addiu_Pair (opsch, succ_opsch, op, succ_op)) {
	INT arc_latency = Calculate_Adjust_Latency(arc);
	INT issue_alignment_delay = (aligned_op_count<2) ?
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(succ_opsch));
	arc_latency = MAX(arc_latency, issue_alignment_delay);
        INT cur_estart = arc_latency + op_estart;
        if (OPSCH_estart(succ_opsch) < cur_estart) {
          OPSCH_estart(succ_opsch) = cur_estart;
        }
        OPSCH_num_succs(opsch)++;
	if (ARC_LIST_Find_Succ(ARC_LIST_rest(arcs), succ_op) == NULL)
	  OPSCH_num_unique_succs(opsch)++;
      }
    }
    *max_lstart = MAX (*max_lstart, op_estart);
  }
}

// ======================================================================
// Do a backward pass and compute the OPSCH data structure for the bb.
// ======================================================================
static void
Compute_Bkwd_OPSCH (BB *bb, BB_MAP value_map, INT max_lstart)
{
  OP *op;

  // Initialize the OPSCH_scycle, OPSCH_lstart and OPSCH_num_preds fields of
  // all OPs.
  FOR_ALL_BB_OPs_REV (bb, op) {
    OPSCH *opsch = OP_opsch(op, value_map);
    ARC_LIST *arcs;
    if (OPSCH_lstart(opsch) > max_lstart) {
      // initialization
      OPSCH_lstart(opsch) = max_lstart - OPSCH_lstart_offset(opsch);
    }
    OPSCH_scycle(opsch) = OPSCH_lstart(opsch);
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch(pred_op, value_map);
      if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	INT arc_latency = Calculate_Adjust_Latency(arc);
	INT issue_alignment_delay = (aligned_op_count<2) ? 
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(pred_opsch));
	arc_latency = MAX(arc_latency, issue_alignment_delay);
        INT cur_lstart = OPSCH_lstart(opsch) - arc_latency;
        if (OPSCH_lstart(pred_opsch) > cur_lstart) {
          OPSCH_lstart(pred_opsch) = cur_lstart;
        }
        OPSCH_num_preds(opsch)++;
	if (ARC_LIST_Find_Pred(ARC_LIST_rest(arcs), pred_op) == NULL)
	  OPSCH_num_unique_preds(opsch)++;
      }
    }
  }
}

// ======================================================================
// Given a <bb>, build the OPSCH data structure for it.
// ======================================================================
void
Compute_OPSCH (BB *bb, bool do_xblock_latency, BB_MAP value_map, MEM_POOL *pool)
{
  INT max_lstart = 0;

  Init_OPSCH_For_BB (bb, value_map, pool, do_xblock_latency);

  Compute_Fwd_OPSCH (bb, value_map, &max_lstart, pool);
  max_lstart = MAX_Clock;
  Compute_Bkwd_OPSCH (bb, value_map, max_lstart);
}

// ======================================================================
// Given a list of single-entry multiple exit blocks, build the OPSCH data 
// structure for it.
// ======================================================================
void
Compute_OPSCHs (list<BB*> bblist, BB_MAP value_map, MEM_POOL *pool)
{
  list<BB*>::iterator bb_iter;

  // Initialize all data structures.
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    Init_OPSCH_For_BB (*bb_iter, value_map, pool, false);
  }

  // Do a forward pass first.
  INT max_lstart = 0;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    Compute_Fwd_OPSCH (*bb_iter, value_map, &max_lstart, pool);
  }

  // Do a backward pass.
  list<BB*>::reverse_iterator bb_riter;
  FOR_ALL_BB_STLLIST_ITEMS_BKWD(bblist, bb_riter) {
    Compute_Bkwd_OPSCH (*bb_riter, value_map, max_lstart);
  }
}

// ======================================================================
// After the <bb> is scheduled, build the BBSCH data structure for GCM phase.
// ======================================================================
void
HB_Schedule::Compute_BBSCH (BB *bb, BBSCH *bbsch)
{
  INT critical_length = 0;

  BBSCH_schedule_length (bbsch) = OP_scycle(BB_last_op(bb)) + 1;
  // computes the longest (or critical) latency for this bb
  // in inverse order
  for (INT i = 0; i < VECTOR_count(_sched_vector); i++) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    if (!critical_length &&
	(OPSCH_lstart(opsch) - OPSCH_estart(opsch)) == 0) { 
      critical_length = OPSCH_scycle(opsch) - Clock;
    }
    if (CGTARG_Is_OP_Barrier(op)) {
      Set_BB_MEM_BARRIER(bbsch);
      if (critical_length) break;
    }
  }

  // block parallelism is computed as the ratio of the number of
  // ops present in this block divided by the longest critical latency
  // the idea is that this would give a rough feel of the amount of 
  // parallelism present in this bb when compared to CGTARG_Peak_Rate
  BBSCH_block_parallelism (bbsch) =
    ((VECTOR_count(_sched_vector) != 0) ?
     (mINT16) ceil(VECTOR_count(_sched_vector) / (critical_length + 1.)) :
     -1);

 if (Cur_Gcm_Type & GCM_MINIMIZE_REGS) {

   GTN_SET *local_set =  GTN_SET_Intersection(BB_live_in(bb), BB_live_out(bb),
					      &_hb_pool);
   local_set = GTN_SET_Union(local_set, BB_live_in(bb), &_hb_pool);
   local_set = GTN_SET_Intersection(local_set, BB_live_def(bb), 
				    &_hb_pool);

   BBSCH_global_regcost (bbsch) = GTN_SET_Alloc_Size(local_set);
 }

}

// ======================================================================
// Add the selected <op> to the list of scheduled instructions. Update
// various data structures that are affected by this addition.
// ======================================================================
void
HB_Schedule::Add_OP_To_Sched_Vector (OP *op, BOOL is_fwd)
{
  ARC_LIST *arcs;
  OPSCH *opsch = OP_opsch (op, _hb_map);

  Set_OPSCH_scheduled (opsch);

  // Adjust the resource table to account for this OP. Change 'Clock' 
  // to be the scycle of this OP.
  if (!OP_dummy(op)) Set_Resource_Usage (op);

  if (HBS_Minimize_Regs() && !HBS_Before_GRA() && !OP_dummy(op)) {
    Update_Regs_For_OP (op);
  }

  if (HBS_Minimize_Regs()) {
    if (!Get_Trace(TP_SCHED, 0x800)) {
      _block_fatpoint = MAX(_block_fatpoint,_live_vals+HB_reg_graph.min_register_usage(op));
      _live_vals += HB_reg_graph.op_live_vals(op);
    } else {
      _block_fatpoint = MAX(_block_fatpoint,_live_vals+OPSCH_live_vals(opsch)+
			    OPSCH_min_usage(opsch));
      _live_vals += OPSCH_new_live_vals(opsch);
    }
  }
  if (!Get_Trace(TP_SCHED, 0x800) && !is_fwd && 
      (HBS_Depth_First() || HBS_Minimize_Regs())) 
    HB_reg_graph.schedule_op(op);

  // Remove <op> from Ready_Vector.
  VECTOR_Delete_Element (_ready_vector, op);

  INT count = VECTOR_count(_ready_vector);
  INT max_lstart = 0;
  INT min_estart = 0x7fff;

  // revise the scycle for all OPs in the Ready_Vector.
  for (INT i = 0; i < count; i++) {
    OP *ready_op = OP_VECTOR_element(_ready_vector,i);
    OPSCH *ready_opsch = OP_opsch (ready_op, _hb_map);
    if (is_fwd) {
      OPSCH_scycle(ready_opsch) = MAX (Clock, OPSCH_scycle(ready_opsch));
      min_estart = MIN(min_estart, OPSCH_estart(ready_opsch));
    } else {
      OPSCH_scycle(ready_opsch) = MIN (Clock, OPSCH_scycle(ready_opsch));
      max_lstart = MAX(max_lstart, OPSCH_lstart(ready_opsch));
    }
  }

  // Update the estart and lstart numbers for instructions in the ready vector.
  // There should be no unscheduled instructions with smaller estart or larger
  // lstart values than the ones in the ready vector. 
  if (is_fwd) {
    min_estart = MAX(Clock, min_estart);
  } else {
    max_lstart = MIN(Clock, max_lstart);
  }
  for (INT i = 0; i < count; i++) {
    OP *ready_op = OP_VECTOR_element(_ready_vector,i);
    OPSCH *ready_opsch = OP_opsch (ready_op, _hb_map);
    if (is_fwd) {
      OPSCH_estart(ready_opsch) = MAX (min_estart, OPSCH_estart(ready_opsch));
    } else {
      OPSCH_lstart(ready_opsch) = MIN (max_lstart, OPSCH_lstart(ready_opsch));
    }
  }

  if (is_fwd) {
    // Add any OPs that are now ready to be scheduled to the Ready_Vector.
    for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *succ_op = ARC_succ(arc);
      OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);

      // Adjust the scycle, num_preds if <succ_op> not prior scheduled.

      if (!OPSCH_scheduled(succ_opsch)) {
	INT scycle = Clock + ARC_latency(arc);
	// update the OPSCH_scycle field for the predecessor OP.
	OPSCH_scycle(succ_opsch) = MAX (scycle, OPSCH_scycle(succ_opsch));
	OPSCH_num_preds(succ_opsch)--;
	if (OPSCH_num_preds(succ_opsch) == 0) {
	  VECTOR_Add_Element (_ready_vector, succ_op);
	}
	INT arc_latency = Calculate_Adjust_Latency(arc);
	INT issue_alignment_delay = (aligned_op_count<2) ?
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(succ_opsch));
	arc_latency = MAX(arc_latency, issue_alignment_delay);
	INT cur_estart = OPSCH_estart(opsch) + arc_latency;
	if (OPSCH_estart(succ_opsch) < cur_estart) {
	  OPSCH_estart(succ_opsch) = cur_estart;
	}
      }
    }
  } else {
    // Add any OPs that are now ready to be scheduled to the Ready_Vector.
    for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      OP *pred_op = ARC_pred(arc);
      OPSCH *pred_opsch = OP_opsch (pred_op, _hb_map);

      // Adjust the scycle, num_succs if <pred_op> not prior scheduled.

      if (!OPSCH_scheduled(pred_opsch)) {
	INT scycle = Clock - ARC_latency(arc);
	// update the OPSCH_scycle field for the predecessor OP.
	OPSCH_scycle(pred_opsch) = MIN (scycle, OPSCH_scycle(pred_opsch));
	if (!Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op)) {
	  FmtAssert (OPSCH_num_succs(pred_opsch) != 0, 
		     ("HBS: invalid count of succs"));
	  
	  OPSCH_num_succs(pred_opsch)--;
	  if (OPSCH_num_succs(pred_opsch) == 0) {
	    VECTOR_Add_Element (_ready_vector, pred_op);
	  }
	  INT arc_latency = Calculate_Adjust_Latency(arc);
	  INT issue_alignment_delay = (aligned_op_count<2) ? 
              ((aligned_op_count < 1) ? 0 : 1) :
	      MAX(OPSCH_issue_alignment(opsch),
		  OPSCH_issue_alignment(pred_opsch));
	  arc_latency = MAX(arc_latency, issue_alignment_delay);
	  INT cur_lstart = OPSCH_lstart(opsch) - arc_latency;
	  if (OPSCH_lstart(pred_opsch) > cur_lstart) {
	    OPSCH_lstart(pred_opsch) = cur_lstart;
	  }
	  if (!Get_Trace(TP_SCHED, 0x400) && 
	      (_hbs_type & HBS_MINIMIZE_REGS) &&
	      (ARC_kind(arc) == CG_DEP_REGIN) &&
	      (OPSCH_num_unique_succs(pred_opsch) > 1)) {
	    TN *arc_tn = OP_opnd(ARC_succ(arc), ARC_opnd(arc));
	    BOOL need_update = false;
	    if (TN_is_register(arc_tn) &&
		!TN_is_global_reg(arc_tn) && !TN_is_dedicated(arc_tn) &&
		!TI_ISA_Regclass_Is_State(TN_register_class(arc_tn))) {
	      for (int i=0; i<OP_results(pred_op); i++) {
		TN* reg_tn = OP_result(pred_op,i);
		if (reg_tn == arc_tn) {
		  if ((OPSCH_live_op_mask(pred_opsch) & (1<<i)) == 0)
		    need_update = true;
		  OPSCH_live_op_mask(pred_opsch) |= (1<<i);
		}
	      }
	    }
	    if (need_update) {
	      for (ARC_LIST* pred_arcs = OP_succs(pred_op); pred_arcs != NULL; 
		   pred_arcs = ARC_LIST_rest(pred_arcs)) {
		ARC *pred_arc = ARC_LIST_first(pred_arcs);
		if ((ARC_kind(pred_arc) == CG_DEP_REGIN) && 
		    (OP_opnd(ARC_succ(pred_arc),ARC_opnd(pred_arc))==arc_tn)) {
		  OP *arc_op = ARC_succ(pred_arc);
		  OPSCH *arc_opsch = OP_opsch (arc_op, _hb_map);
		  OPSCH_num_splits(arc_opsch)--;
		  OPSCH_live_vals(arc_opsch)--;
		  while (OPSCH_num_unique_succs(arc_opsch) == 1) {
		    arc_op = ARC_succ(ARC_LIST_first(OP_succs(arc_op)));
		    arc_opsch = OP_opsch(arc_op, _hb_map);
		    OPSCH_num_splits(arc_opsch)--;
		    OPSCH_live_vals(arc_opsch)--;
		  }
		}
	      }
	    }
	  }
	}
      }
    }

    // If current OP is a load/store, check if it has been scheduled before 
    // an addiu. If yes, we need to account for the latency between the 
    // addiu and the current OP.
    if (OPSCH_ldst(opsch)) {
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	OP *succ_op = ARC_succ(arc);
	OPSCH *succ_opsch = OP_opsch (succ_op, _hb_map);
	if (!OPSCH_scheduled(succ_opsch)) {
	  INT opndnum = Memory_OP_Base_Opndnum (op);
	  if (opndnum != -1) {
	    INT scycle = Clock - CG_DEP_Latency (succ_op, op, CG_DEP_REGIN, opndnum);
	    OPSCH_scycle(succ_opsch) = MIN (scycle, OPSCH_scycle(succ_opsch));
	  }
	}
      }
    }
  }

  VECTOR_Add_Element (_sched_vector, op);

#if 0
  INT sched_count = VECTOR_count(_sched_vector);
  INT ready_count = VECTOR_count(_ready_vector);

  // revise the scycle for all OPs in the Ready_Vector for stalls
  for (INT i = 0; i < sched_count; i++) {
    OP *sched_op = OP_VECTOR_element(_sched_vector,i);
    OPSCH *sched_opsch = OP_opsch (sched_op, _hb_map);
    INT sched_scycle = OPSCH_scycle(sched_opsch);
    for (INT j = 0; j < ready_count; j++) {
      OP *ready_op = OP_VECTOR_element(_ready_vector,j);
      OPSCH *ready_opsch = OP_opsch (ready_op, _hb_map);
      if (is_fwd) {
	if (OPSCH_scycle(ready_opsch)!=sched_scycle)
          OPSCH_scycle(ready_opsch) =
		MAX (sched_scycle+OP_Stall(sched_op,ready_op)+1,
		     OPSCH_scycle(ready_opsch));
      } else {
	if (OPSCH_scycle(ready_opsch)!=sched_scycle)
          OPSCH_scycle(ready_opsch) =
		MIN (sched_scycle-OP_Stall(ready_op,sched_op)-1,
		     OPSCH_scycle(ready_opsch));
      }
    }
  }
#endif

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "Cycle: %d :: ", Clock);
    Print_OP_No_SrcLine (op);
  }
}

// ======================================================================
// Compare two OPs to see which one is better for scheduling.
// ======================================================================
BOOL
Priority_Selector::Is_OP_Better (OP *cur_op, OP *best_op)
{
  OPSCH *cur_opsch = OP_opsch(cur_op, _cur_sched->hb_map());
  OPSCH *best_opsch = OP_opsch(best_op, _cur_sched->hb_map());
  INT cur_scycle = OPSCH_scycle(cur_opsch);
  INT best_scycle = OPSCH_scycle(best_opsch);

  if (_hbs_type & HBS_MINIMIZE_REGS) {

    if (!Get_Trace(TP_SCHED, 0x400)) {
      if (!(_hbs_type & HBS_IGNORE_LATENCY)) {
	if (cur_scycle != best_scycle)
	  return (cur_scycle > best_scycle);
      }
      // OPSCH_live_vals is the number of live ranges left after
      // evaluating the tree rooted at op. OPSCH_min_usage is the
      // minimum number of registers needed to evaluate the tree, but
      // will be freed up afterwards. Thus, if we choose to evaluate the tree
      // rooted at best_op before the tree rooted at cur_op, the total number
      // of registers needed is given below. 
      // The goal is to evaluate the tree that requires the most registers 
      // first before they're all taken up by leftover live ranges.
      // The limitations are that register classes are not differentiated,
      // OPSCH_live_vals may be overestimated if the live ranges are already
      // made live by previously scheduled OPs, and that OPSCH_min_usage
      // may be too small.
      if (!Get_Trace(TP_SCHED, 0x800)) {
	INT best_fatpoint = MAX(HB_reg_graph.min_register_usage(best_op),
				HB_reg_graph.min_register_usage(cur_op)+
				HB_reg_graph.new_live_vals(best_op));
	INT cur_fatpoint = MAX(HB_reg_graph.min_register_usage(cur_op),
				HB_reg_graph.min_register_usage(best_op)+
				HB_reg_graph.new_live_vals(cur_op));
	if (!Get_Trace(TP_SCHED, 0x200) || (_hbs_type & HBS_IGNORE_LATENCY) ||
	    (cur_fatpoint+_cur_sched->_live_vals>_cur_sched->_block_fatpoint) || 
	    (best_fatpoint+_cur_sched->_live_vals>_cur_sched->_block_fatpoint)) {
	  if (cur_fatpoint != best_fatpoint)
	    return (cur_fatpoint < best_fatpoint);
	  if (HB_reg_graph.new_live_vals(cur_op) != 
	      HB_reg_graph.new_live_vals(best_op))
	    return (HB_reg_graph.new_live_vals(cur_op) < 
		    HB_reg_graph.new_live_vals(best_op));
	}
      } else {
	INT best_fatpoint = 
	  MAX(OPSCH_min_usage(best_opsch)+OPSCH_live_vals(best_opsch),
	      OPSCH_min_usage(cur_opsch)+OPSCH_live_vals(cur_opsch)+
	      OPSCH_live_vals(best_opsch));
	INT cur_fatpoint = 
	  MAX(OPSCH_min_usage(cur_opsch)+OPSCH_live_vals(cur_opsch),
	      OPSCH_min_usage(best_opsch)+OPSCH_live_vals(best_opsch)+
	      OPSCH_live_vals(cur_opsch));
	if (!Get_Trace(TP_SCHED, 0x200) || (_hbs_type & HBS_IGNORE_LATENCY) ||
	    (cur_fatpoint+_cur_sched->_live_vals>_cur_sched->_block_fatpoint) || 
	    (best_fatpoint+_cur_sched->_live_vals>_cur_sched->_block_fatpoint)) {
	  if (cur_fatpoint != best_fatpoint)
	    return (cur_fatpoint < best_fatpoint);
	  if (OPSCH_live_vals(cur_opsch) != OPSCH_live_vals(best_opsch))
	    return (OPSCH_live_vals(cur_opsch) < OPSCH_live_vals(best_opsch));
	}
      }
//        if (jons_test_flags[3]) {
//  	    if (cur_scycle != best_scycle)
//  	    return (cur_scycle > best_scycle);
//        }

      if (OPSCH_regcost(cur_opsch) != OPSCH_regcost(best_opsch));
        return (OPSCH_regcost(cur_opsch) < OPSCH_regcost(best_opsch));

      INT cur_slack, best_slack;
      cur_slack = OPSCH_lstart(cur_opsch) - OPSCH_estart(cur_opsch);
      best_slack = OPSCH_lstart(best_opsch) - OPSCH_estart(best_opsch);
      if (cur_slack != best_slack) 
	return (cur_slack < best_slack);

      return (OPSCH_estart(cur_opsch) > OPSCH_estart(best_opsch));

    } else {
      if (!Get_Trace(TP_SCHED, 0x100)) {
	if (OPSCH_tree(cur_opsch) && OPSCH_tree(best_opsch)) {
	  if (OPSCH_total_usage(cur_opsch) != OPSCH_total_usage(best_opsch))
	    return (OPSCH_total_usage(cur_opsch)<OPSCH_total_usage(best_opsch));
	} else if (OPSCH_tree(cur_opsch) && !OPSCH_tree(best_opsch)) {
	  return TRUE;
	} else if (!OPSCH_tree(cur_opsch) && OPSCH_tree(best_opsch)) {
	  return FALSE;
	}
      }
      INT cur_op_better = (cur_scycle - best_scycle);
      if ((cur_op_better == 0) && !HB_Dfs_Uses_Last_Sched_Inst)
	cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
      else if (cur_op_better == 0) {
	
	OPSCH *last_opsch = NULL;
	OP* last_op = NULL;
	int last_op_dfsnum = -1;
	int cur_op_dfsnum = -1;
	int best_op_dfsnum = -1;
	
	if (VECTOR_count(_cur_sched->_sched_vector)>0) {
	  int i = VECTOR_count(_cur_sched->_sched_vector)-1;
	  last_op = OP_VECTOR_element(_cur_sched->_sched_vector,i);
	  last_opsch = OP_opsch(last_op, _cur_sched->hb_map());
	  last_op_dfsnum = OPSCH_dfsnum(last_opsch);
	  cur_op_dfsnum = OPSCH_dfsnum(cur_opsch);
	  best_op_dfsnum = OPSCH_dfsnum(best_opsch);
	  if (last_op_dfsnum<cur_op_dfsnum && last_op_dfsnum<best_op_dfsnum)
	    cur_op_better= (cur_op_dfsnum<best_op_dfsnum);
	  else if (last_op_dfsnum<cur_op_dfsnum && last_op_dfsnum>best_op_dfsnum)
	    cur_op_better= 1;
	  else if (last_op_dfsnum>cur_op_dfsnum && last_op_dfsnum<best_op_dfsnum)
	    cur_op_better= 0;
	  else
	    cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
	} else
	  cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
      }
      cur_op_better += (OPSCH_regcost(best_opsch) - OPSCH_regcost(cur_opsch));
      return (cur_op_better > 0);
    }
  }

  BOOL manual_pref = FALSE;
  if (OP_prefetch(cur_op)) {
    WN *pref_wn = Get_WN_From_Memory_OP(cur_op);
    if (pref_wn && WN_pf_manual(pref_wn)) manual_pref = TRUE;
  }

  if (cur_scycle > best_scycle)  {

    // Special case manual prefetch nodes. 
    if (manual_pref) return FALSE;
    else return TRUE;
  }

  if (cur_scycle < best_scycle) return FALSE;

  if (manual_pref) return FALSE;

  // For T5, try to schedule the OPs defining the operands of a terminating
  // branch as far away from the branch as possible.
  if (OPSCH_def_xfer_opnd(cur_opsch) ^ OPSCH_def_xfer_opnd(best_opsch)) {
    return OPSCH_def_xfer_opnd(best_opsch);
  }

  if (_hbs_type & HBS_DEPTH_FIRST) {
    return (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
  }

  if (_hbs_type & HBS_CRITICAL_PATH) {

    // give priority to the current op with issue alignment or
    // external resource requirement
    if (aligned_op_count>1)
      if (OPSCH_issue_alignment(cur_opsch)>OPSCH_issue_alignment(best_opsch))
        return TRUE;
      else if (OPSCH_issue_alignment(cur_opsch)<OPSCH_issue_alignment(best_opsch))
        return FALSE;

    if (OPSCH_use_shared_resource(cur_opsch)!= OPSCH_use_shared_resource(best_opsch))
      if (OPSCH_use_shared_resource(cur_opsch))
	return TRUE;
      else
	return FALSE;

    INT cur_slack, best_slack;
    cur_slack = OPSCH_lstart(cur_opsch) - OPSCH_estart(cur_opsch);
    best_slack = OPSCH_lstart(best_opsch) - OPSCH_estart(best_opsch);

#ifdef TARG_XTENSA
    /* NOTE: This Is_OP_Better is called only for backward
       scheduling. For forward scheduling,
       List_Based_Fwd::Is_OP_Better is invoked.
       
       The HBS_CRITICAL_PATH heuristic is not filling load delay slots
       at the top of a block because it doesn't recognize when an op
       has no predecessors and thus it's placement can no longer
       change the critical path length. The ops were being scheduled
       before other ops that did have predecessors. The fix is to
       prefer is op that has predecessors over one that doesn't. */

    if ((OPSCH_num_preds(best_opsch) == 0) &&
	(OPSCH_num_preds(cur_opsch) != 0))
      return TRUE;
    if ((OPSCH_num_preds(best_opsch) != 0) &&
	(OPSCH_num_preds(cur_opsch) == 0))
      return FALSE;
#endif
    
    if (cur_slack < best_slack) return TRUE;
    if (cur_slack > best_slack) return FALSE;
    
    if (OPSCH_estart(cur_opsch) > OPSCH_estart(best_opsch)) return TRUE;
    if (OPSCH_estart(cur_opsch) < OPSCH_estart(best_opsch)) return FALSE;
  }
  return FALSE;
}

// ======================================================================
// Pick an OP to schedule in the delay slot of the terminating branch.
// ======================================================================
OP*
Priority_Selector::Select_OP_For_Delay_Slot (OP *xfer_op)
{

  // If the <xfer_op> has a successor, then that op has to remain in the
  // delay slot. Make sure we return this OP.
  OPSCH *opsch = OP_opsch(xfer_op, _cur_sched->hb_map());
  if (OPSCH_num_succs(opsch) != 0) {
    ARC_LIST *arcs = OP_succs(xfer_op);
    ARC *first_arc = ARC_LIST_first(arcs);
    if (OPSCH_num_succs(opsch) > 1) {
    	ARC *second_arc = ARC_LIST_first(ARC_LIST_rest(arcs));
        DevAssert (ARC_succ(first_arc) == ARC_succ(second_arc), 
		("More than 1 successor for xfer_op"));
    }
    return ARC_succ(first_arc);
  }

  // If this optimization has been disabled or 
  // If we are scheduling to minimize registers, don't put anything 
  // in the delay slot. Trying to spill something in the delay slot 
  // is no fun.
  if (!Enable_Fill_Delay_Slots || 
      (_hbs_type & HBS_MINIMIZE_REGS)) return NULL;

  OP *best_op = NULL;

  for (INT i = VECTOR_count(_cur_sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (_cur_sched->ready_vector(), i);

    // Don't schedule any dummy OPs or OPs that expand into 
    // more than 1 instruction in the delay slot.
    if (OP_xfer(cur_op) || OP_Real_Ops(cur_op) != 1) continue;

    // Don't put instructions that have hazards in the delay slot.
    if (OP_has_hazard(cur_op)) continue;

    // R10k chip bug workaround: Avoid placing integer mult/div in delay
    // slots of unconditional branches. (see pv516598) for more details.
    if (OP_uncond(xfer_op) && (OP_imul(cur_op) || OP_idiv(cur_op)))
      continue;

    // Don't put manual prefetches into delay slots as well.
    if (OP_prefetch(cur_op)) {
      WN *pref_wn = Get_WN_From_Memory_OP(cur_op);
      if (pref_wn && WN_pf_manual(pref_wn)) continue;
    }

    // When GCM is calling it, don't try to schedule the <op> that has
    // moved from a different block in the delay slot. frequent observance
    // is that this will unneccessarily restrict further code motion.
    if ((_hbs_type & HBS_FROM_GCM) && OP_moved(cur_op)) continue;

    if (best_op == NULL || Is_OP_Better(cur_op, best_op)) {
      best_op = cur_op;
    }
  }
  return best_op;
}

// ======================================================================
// Put the scheduled list of instructions back into the basic block. 
// This is done by emptying the basic block and inserting the scheduled
// instructions back into the basic block.
// ======================================================================
INT32
HB_Schedule::Put_Sched_Vector_Into_BB (BB *bb, BBSCH *bbsch, BOOL is_fwd)
{
  INT i;

  // Set the OP_scycle field for all the OPs. Also, reset the OPSCH_visited
  // flag. It is used in the Adjust_Ldst_Offsets routine.
  for (i = VECTOR_count(_sched_vector) - 1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Reset_OPSCH_visited (opsch);
    OP_scycle(op) = (is_fwd) ? OPSCH_scycle(opsch) : OPSCH_scycle(opsch) - Clock;
  }

  INT32 cur_cycle = OP_scycle(BB_last_op(bb)) + 1;

  // If current cycle estimate is better than <max_sched>, then ONLY dump
  // the Sched_Vector buffer. Otherwise, preserve the previous one.

  if (cur_cycle < _max_sched) {
    Adjust_Ldst_Offsets ();

    if (bbsch != NULL) {
      Compute_BBSCH (bb, bbsch);
    }

    BB_Remove_All(bb);

    // Dump the <sched_vector> backward (or forward) depending on the
    // type of the schedule.
    for (i = (is_fwd) ? 0 : VECTOR_count(_sched_vector) - 1; 
	 (is_fwd) ? i < VECTOR_count(_sched_vector) : i >= 0; 
	 (is_fwd) ? i++ : i--) {
      BB_Append_Op(bb, OP_VECTOR_element(_sched_vector, i));
    }
    return _max_sched-cur_cycle;
  }
  return 0;
}

// ======================================================================
// Put the scheduled list of instructions back into the basic block. 
// This is done by emptying the basic block and inserting the scheduled
// instructions back into the basic block.
// ======================================================================
void
HB_Schedule::Put_Sched_Vector_Into_HB (list<BB*>& bblist)
{
  INT i;

  // Set the OP_scycle field for all the OPs. Also, reset the OPSCH_visited
  // flag. It is used in the Adjust_Ldst_Offsets routine.
  for (i = VECTOR_count(_sched_vector)-1; i >= 0; i--) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    OPSCH *opsch = OP_opsch(op, _hb_map);
    Reset_OPSCH_visited (opsch);
    OP_scycle(op) = OPSCH_scycle(opsch);
  }

  list<BB*>::iterator bb_iter;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bb_iter) {
    BB_Remove_All(*bb_iter);
  }

  bb_iter = bblist.begin();
  for (i = 0; i < VECTOR_count(_sched_vector); i++) {
    OP *op = OP_VECTOR_element(_sched_vector, i);
    FmtAssert (bb_iter != bblist.end(), ("bb_iter is NULL info"));
    BB_Append_Op(*bb_iter, op);

    // Advance to the next block when noticed an <xfer_op>.
    if (OP_xfer(op)) { bb_iter++; }
  }
}

// ======================================================================
// Allocate a RFlag_Table with <cycles> entries. Initialize the entries.
// ======================================================================
void
HB_Schedule::Init_RFlag_Table (list<BB*>& bblist, BOOL is_fwd)
{
  INT rtable_size = 0;
  INT max_resource_cycles = 0;
  aligned_op_count = 0;

  _rr_tab = TI_RES_RES_Alloc(FALSE, &_hb_pool);

  list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    OP *op;
    FOR_ALL_BB_OPs_FWD (*bbi, op) {
      INT cur_resource_cycles = Resource_Cycles_For_OP (op);
      if (cur_resource_cycles > max_resource_cycles) {
	max_resource_cycles = cur_resource_cycles;
      }
      INT op_latency = cur_resource_cycles;
      ARC_LIST *arcs;
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if (ARC_latency(arc) > op_latency) {
	  op_latency = ARC_latency(arc);
	}
      }
      INT issue_alignment = TI_TOP_Issue_Alignment(OP_code(op));
      if (issue_alignment>1) {
	if (aligned_op_count>0)
	  op_latency += issue_alignment;
	aligned_op_count++;
      }
      if (TI_TOP_Has_Export_State(OP_code(op))) {
	op_latency = MAX(2,op_latency);	// needs additional cycles to accomodate
					// stall from export state write to tie port
					// access (see OP_Stall() in oputil.cxx)
      }
      rtable_size += op_latency;
    }
  }

  // cross-block dependence requirement
  if (_do_xblock_latency) {
    rtable_size += 2*MAX_XBB_DELAY;
  }

  // start scheduling OPs at this <rtable_size>.
  Clock = (is_fwd) ? 0 : rtable_size;
  MAX_Clock = rtable_size;
  Last_aligned_cycle = -1;
  Last_issue_alignment = -1;

  // increase table size by the maximum number of resource cycles needed by
  // any OP.
  rtable_size += max_resource_cycles;

  TI_RES_RES_Set_BB_Cycle_Count(_rr_tab, rtable_size);
}

List_Based_Bkwd::List_Based_Bkwd (BB *bb, HB_Schedule *sched, HBS_TYPE type, 
				  MEM_POOL *pool) : 
  Priority_Selector(bb, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bb, this->Is_Fwd_Schedule());

  if (sched->HBS_Depth_First() || sched->HBS_Minimize_Regs()) 
    Compute_DFO (sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bb, sched->hb_map());

}

List_Based_Bkwd::List_Based_Bkwd (list<BB*> bblist, HB_Schedule *sched, 
				  HBS_TYPE type, MEM_POOL *pool) : 
  Priority_Selector(bblist, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bblist, this->Is_Fwd_Schedule());

  if (sched->HBS_Depth_First() || sched->HBS_Minimize_Regs()) 
    Compute_DFO (sched, sched->hb_map(), this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bblist, sched->hb_map());

}

// ======================================================================
// Select an OP to schedule next from the Ready_Vector.
// ======================================================================
void*
Priority_Selector::Get_Next_Element(HB_Schedule *Cur_Sched)
{
  _best_op = NULL;

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "-------------------------------------------\n");
    fprintf (TFile, "Candidates for Scheduling:\n");
    fprintf (TFile, "-------------------------------------------\n");
  }

  for (INT i = VECTOR_count(Cur_Sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (Cur_Sched->ready_vector(), i);

    // update the scycle for the <cur_op>.
    if (!OP_dummy(cur_op)) Cur_Sched->Find_Schedule_Cycle (cur_op, FALSE);

    if (Cur_Sched->HBS_Minimize_Regs() && !Cur_Sched->HBS_Before_GRA() && 
	!OP_dummy(cur_op)) {
      Cur_Sched->Estimate_Reg_Cost_For_OP (cur_op);
    }

    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      Print_OPSCH (cur_op, Cur_Sched->hb_map());
    }

    // Replace the best_op by the cur_op if any of the following is true:
    //   1. best_op is NULL, i.e. cur_op is the first one we have seen.
    //   2. The cur_op is better based on some heuristics.
    if (_best_op == NULL || Is_OP_Better (cur_op, _best_op)) {
      _best_op = cur_op;
    }
  }

  return (void *) _best_op;
}

// ======================================================================
// Compare two OPs to see which one is better for scheduling.
// ======================================================================
BOOL
List_Based_Fwd::Is_OP_Better (OP *cur_op, OP *best_op)
{
  OPSCH *cur_opsch = OP_opsch(cur_op, _cur_sched->hb_map());
  OPSCH *best_opsch = OP_opsch(best_op, _cur_sched->hb_map());
  INT cur_scycle = OPSCH_scycle(cur_opsch);
  INT best_scycle = OPSCH_scycle(best_opsch);

  if (_hbs_type & HBS_MINIMIZE_REGS) {
     INT cur_op_better = (best_scycle - cur_scycle);
     if (cur_op_better == 0) {
       cur_op_better = (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
     }
     cur_op_better += (OPSCH_regcost(cur_opsch) - OPSCH_regcost(best_opsch));
     return (cur_op_better > 0);
  }

  if (cur_scycle < best_scycle) return TRUE;

  if (cur_scycle > best_scycle) return FALSE;

  // branch-predict instructions ALWAYS need to be scheduled early but not
  // too early. Need to check if the BB_length doesn't exceed the offset
  // limits imposed by the ISA. It's assumed that about 1/3 nops will be
  // added later, so include the expansion factor. 

  if (OP_branch_predict(cur_op) && 
      ((BB_length(OP_bb(cur_op)) * 1.3 * INST_BYTES) 
       < DEFAULT_BRP_BRANCH_LIMIT)) 
    return TRUE;

  if (_hbs_type & HBS_DEPTH_FIRST) {
    return (OPSCH_dfsnum(cur_opsch) < OPSCH_dfsnum(best_opsch));
  }

  if (_hbs_type & HBS_CRITICAL_PATH) {

    // give priority to the current op with issue alignment or
    // external resource requirement
    if (aligned_op_count>1)
      if (OPSCH_issue_alignment(cur_opsch)>OPSCH_issue_alignment(best_opsch))
        return TRUE;
      else if (OPSCH_issue_alignment(cur_opsch)<OPSCH_issue_alignment(best_opsch))
        return FALSE;

    if (OPSCH_use_shared_resource(cur_opsch)!= OPSCH_use_shared_resource(best_opsch))
      if (OPSCH_use_shared_resource(cur_opsch))
	return TRUE;
      else
	return FALSE;

    INT cur_slack, best_slack;
    cur_slack = OPSCH_lstart(cur_opsch) - OPSCH_estart(cur_opsch);
    best_slack = OPSCH_lstart(best_opsch) - OPSCH_estart(best_opsch);

#ifdef TARG_XTENSA
    /* NOTE: This Is_OP_Better is called only for forward
       scheduling. For backward scheduling,
       Priority_Selector::Is_OP_Better is invoked.
       
       The HBS_CRITICAL_PATH heuristic is not filling load delay slots
       at the bottom of a block because it doesn't recognize when an
       op has no successors and thus it's placement can no longer
       change the critical path length. The ops were being scheduled
       before other ops that did have successors. The fix is to prefer
       is op that has successors over one that doesn't. */

    if ((OPSCH_num_succs(best_opsch) == 0) &&
	(OPSCH_num_succs(cur_opsch) != 0))
      return TRUE;
    if ((OPSCH_num_succs(best_opsch) != 0) &&
	(OPSCH_num_succs(cur_opsch) == 0))
      return FALSE;
#endif
    
    if (cur_slack < best_slack) return TRUE;
    if (cur_slack > best_slack) return FALSE;

    if (OPSCH_estart(cur_opsch) > OPSCH_estart(best_opsch)) return TRUE;
    if (OPSCH_estart(cur_opsch) < OPSCH_estart(best_opsch)) return FALSE;
  }
  return FALSE;
}

List_Based_Fwd::List_Based_Fwd (BB *bb, HB_Schedule *sched, HBS_TYPE type, 
				MEM_POOL *pool) : 
  Priority_Selector(bb, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bb, this->Is_Fwd_Schedule());

  if (sched->HBS_Depth_First()) Compute_DFO (sched, sched->hb_map(),
					     this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bb, sched->hb_map());

}

List_Based_Fwd::List_Based_Fwd (list<BB*> bblist, HB_Schedule *sched, 
				HBS_TYPE type, MEM_POOL *pool) : 
  Priority_Selector(bblist, sched, type, pool)
{

  // Build Ready list (list of ready candidates).
  Build_Ready_Vector (bblist, this->Is_Fwd_Schedule());

  if (sched->HBS_Depth_First()) Compute_DFO (sched, sched->hb_map(),
					     this->Is_Fwd_Schedule());

  if (Trace_HB) Print_BB_For_HB (bblist, sched->hb_map());

}

// ======================================================================
// Select an OP to schedule next from the Ready_Vector.
// ======================================================================
void*
List_Based_Fwd::Get_Next_Element(HB_Schedule *Cur_Sched)
{
  _best_op = NULL;

  if (Trace_HB) {
    #pragma mips_frequency_hint NEVER
    fprintf (TFile, "-------------------------------------------\n");
    fprintf (TFile, "Candidates for Scheduling:\n");
    fprintf (TFile, "-------------------------------------------\n");
  }

  for (INT i = VECTOR_count(Cur_Sched->ready_vector())-1; i >= 0; i--) 
  {
    OP *cur_op = OP_VECTOR_element (Cur_Sched->ready_vector(), i);

    // update the scycle for the <cur_op>.
    if (!OP_dummy(cur_op)) Cur_Sched->Find_Schedule_Cycle (cur_op, TRUE);

    if (Cur_Sched->HBS_Minimize_Regs() && !OP_dummy(cur_op)) {
      Cur_Sched->Estimate_Reg_Cost_For_OP (cur_op);
    }

    if (Trace_HB) {
      #pragma mips_frequency_hint NEVER
      Print_OPSCH (cur_op, Cur_Sched->hb_map());
    }

    // Replace the best_op by the cur_op if any of the following is true:
    //   1. best_op is NULL, i.e. cur_op is the first one we have seen.
    //   2. The cur_op is better based on some heuristics.
    if (_best_op == NULL || Is_OP_Better (cur_op, _best_op)) {
      _best_op = cur_op;
    }
  }

  return (void *) _best_op;
}

// ======================================================================
// ::Invoke_Pre_HBS_Phase
// All the scheduling preparatory stuff (eg. separating out the special
// instructions to prolog, epilog blocks, etc..) should be done here,
// before the actual scheduling begins.
// ===================================================================
void
HB_Schedule::Invoke_Pre_HBS_Phase(BB* bb)
{

  OP *op, *prev_op;
  OP *next_op;

  // When we are scheduling before register allocation, we don't want 
  // to schedule SP adjustment OPs in the entry/exit blocks and OPs
  // that are marked with the OP_glue attribute. We also don't want 
  // to schedule COPY instructions for save/restore of callee save 
  // registers. We check for them and move them away to temporary basic 
  // blocks. After we are done scheduling the remaining OPs, we merge the 
  // moved instructions back.

  if (HBS_Before_LRA()) {
    if (BB_entry(bb)) {
      _prolog_bb = Gen_BB_Like (bb);
      for (op = BB_entry_sp_adj_op (bb); op != NULL; op = prev_op) {
        prev_op = OP_prev(op);
        BB_Move_Op_To_Start (_prolog_bb, bb, op);
      }
    }
    if (BB_exit(bb)) {
      _epilog_bb = Gen_BB_Like (bb);
      for (op = BB_exit_sp_adj_op (bb); op != NULL; op = next_op) {
	next_op = OP_next(op);
	BB_Move_Op_To_End (_epilog_bb, bb, op);
      }
    }
    for (op = BB_first_op(bb); op != NULL; op = next_op) {
      /* Stop on the first instruction from a previous scheduling attempt */
      if (OP_flag1(op)) break;
      if (!OP_copy(op) && !OP_glue(op) && !OP_no_move_before_gra(op) && !OP_access_reg_bank(op)) break;

      // some TIE copy instructions cannot copy-propagate into its sink
      // but needs to be scheduled
      // to reduce register pressure (see PR8310)
      if (OP_tie(op) && OP_copy(op) && !OP_glue(op)) break;

      // perform more test on copy that cannot be moved
      if (OP_copy(op) && !OP_glue(op)) {

	// if this bb is not an entry, there is no
	// callee-saved copies so all copy ops can be scheduled
	// and do not need to be moved to prolog block
	if (!BB_entry(bb))
	  break;

	// for copies in entry bb, check if it has no
	// dedicated TN reference
	bool reference_dedicated_tn = false;
	for (INT i=0; i<OP_opnds(op); i++) {
	  TN* tn = OP_opnd(op,i);
	  if (TN_is_dedicated(tn)) {
	    reference_dedicated_tn = true;
	    break;
	  }
	}
	if (reference_dedicated_tn==false) {
	  for (INT i=0; i<OP_results(op); i++) {
	    TN* tn = OP_result(op,i);
	    if (TN_is_dedicated(tn)) {
	      reference_dedicated_tn = true;
	      break;
	    }
	  }
	}

	// if this copy has no dedicated TN reference
	// then it can be scheduled and does not need
	// to be moved to the prolog block
	if (reference_dedicated_tn==false)
	  break;
      }

      next_op = OP_next(op);
      if (_prolog_bb == NULL) {
	_prolog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_End (_prolog_bb, bb, op);
    }
    if (op != NULL)
      Reset_OP_flag1(op);
    for (op = BB_last_op(bb); op != NULL; op = prev_op) {
      prev_op = OP_prev(op);
      /* Stop on the last instruction from a previous scheduling attempt */
      if (OP_flag1(op)) break;
      if (!OP_copy(op) && !OP_glue(op) && !OP_no_move_before_gra(op) && !OP_access_reg_bank(op)) {
	// check for glue copies before a branch also.
	if (!OP_xfer(op) || 
	    prev_op == NULL || 
	    (!OP_copy(prev_op) && !OP_glue(prev_op) && !OP_no_move_before_gra(op)))
	break;
      }
      if (_epilog_bb == NULL) {
	_epilog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_Start (_epilog_bb, bb, op);

      // PRE-GCM can sometimes fill the delay slot with a copy op. In such
      // instances, need to move the branch as well so as to avoid later
      // filling of its delay slot.
      if (prev_op && OP_xfer(prev_op))
        BB_Move_Op_To_Start (_epilog_bb, bb, prev_op);
    }
    if (op != NULL)
      Reset_OP_flag1(op);
  }
  else {

    // Alloca instructions can't be reordered.
    for (op = BB_first_op(bb); op != NULL; op = next_op) {
      if (!OP_side_effects(op)) break;
      next_op = OP_next(op);
      if (_prolog_bb == NULL) {
	_prolog_bb = Gen_BB_Like (bb);
      }
      BB_Move_Op_To_End (_prolog_bb, bb, op);
    }

    // Special case handling for regions.
    if (BB_rid(bb) != NULL && RID_cginfo(BB_rid(bb)) != NULL) {
      CGRIN *cgrin = RID_cginfo(BB_rid(bb));
      if (CGRIN_first_bb(cgrin) != NULL && 
	  BB_next(CGRIN_first_bb(cgrin)) == NULL) {
	/* only 1 bb */
	/* make sure pregtns don't get moved, even after regalloc */
	for (op = BB_first_op(bb); op != NULL; op = next_op) {
	  if (OP_code(op) != TOP_begin_pregtn) break;
	  next_op = OP_next(op);
	  if (_prolog_bb == NULL) {
	    _prolog_bb = Gen_BB_Like (bb);
	  }
	  BB_Move_Op_To_End (_prolog_bb, bb, op);
	}
      
	for (op = BB_last_op(bb); op != NULL; op = prev_op) {
	  prev_op = OP_prev(op);
	  if (OP_code(op) != TOP_end_pregtn) {
	    // check for glue copies before a branch also.
	    if (!OP_xfer(op) || prev_op == NULL || 
		(OP_code(prev_op) != TOP_end_pregtn))
	      break;
	  }
	  if (_epilog_bb == NULL) {
	    _epilog_bb = Gen_BB_Like (bb);
	  }
	  BB_Move_Op_To_Start (_epilog_bb, bb, op);
	}
      }
    }
  }
}

// ======================================================================
// ::Invoke_Pre_HBB_Phase
// All the scheduling preparatory stuff for hyperblocks (eg. separating out 
// the special instructions to prolog, epilog blocks) should be done here,
// before the actual scheduling begins. The assumption is that this needs
// to be done only for the entry/exit blocks in the hyperblock.
// ===================================================================
void
HB_Schedule::Invoke_Pre_HBB_Phase(list<BB*> bblist)
{

  list<BB*>::iterator bb_iter;
  list<BB*>::reverse_iterator bb_riter;

  bb_iter = bblist.begin();
  bb_riter = bblist.rbegin();

  BB *first_bb = *bb_iter; BB *last_bb = *bb_riter;

  OP *op, *prev_op;
  OP *next_op;

  // When we are scheduling before register allocation, we don't want 
  // to schedule SP adjustment OPs in the entry/exit blocks.
  // We check for them and move them away to temporary basic 
  // blocks. After we are done scheduling the remaining OPs, we merge the 
  // moved instructions back.

  if (HBS_Before_LRA()) {
    if (BB_entry(first_bb)) {
      _prolog_bb = Gen_BB_Like (first_bb);
      for (op = BB_entry_sp_adj_op (first_bb); op != NULL; op = prev_op) {
        prev_op = OP_prev(op);
        BB_Move_Op_To_Start (_prolog_bb, first_bb, op);
      }
    }
    if (BB_exit(last_bb)) {
      _epilog_bb = Gen_BB_Like (last_bb);
      for (op = BB_exit_sp_adj_op (last_bb); op != NULL; op = next_op) {
	next_op = OP_next(op);
	BB_Move_Op_To_End (_epilog_bb, last_bb, op);
      }
    }
  }

  for (op = BB_first_op(first_bb); op != NULL; op = next_op) {
    if (!OP_side_effects(op)) break;
    next_op = OP_next(op);
    if (_prolog_bb == NULL) {
      _prolog_bb = Gen_BB_Like (first_bb);
    }
    BB_Move_Op_To_End (_prolog_bb, first_bb, op);
  }
}

void
HB_Schedule::Invoke_Post_HBS_Phase(BB* bb)
{
  /* If we plan to reschedule the block with a different heuristic, mark
   * the first and last scheduled instructions so they don't get removed
   * in Invoke_Pre_HBS_Phase. Otherwise, we may not get a fair 
   * comparison. */
  if (HBS_Before_LRA() && HBS_Minimize_Regs() && (BB_length(bb)>0)) {
    Set_OP_flag1(BB_first_op(bb));
    Set_OP_flag1(BB_last_op(bb));
  }
    
  // If we had moved aside any instructions in the prolog or epilog of 
  // the bb, put them back in.
  if (_prolog_bb != NULL) {
    BB_Prepend_All (bb, _prolog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (bb);
  }

  if (_epilog_bb != NULL) {
    BB_Append_All(bb, _epilog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (bb);
  }

}

void
HB_Schedule::Invoke_Post_HBB_Phase(list<BB*> bblist)
{

  list<BB*>::iterator bb_iter;
  list<BB*>::reverse_iterator bb_riter;

  bb_iter = bblist.begin();
  bb_riter = bblist.rbegin();

  BB *first_bb = *bb_iter; BB *last_bb = *bb_riter;

  // If we had moved aside any instructions in the prolog or epilog of 
  // the bb, put them back in.
  if (_prolog_bb != NULL) {
    BB_Prepend_All (first_bb, _prolog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (first_bb);
  }

  if (_epilog_bb != NULL) {
    BB_Append_All(last_bb, _epilog_bb);
    if (HBS_Before_LRA()) Reset_BB_scheduled (last_bb);
  }

}

INT 
HB_Schedule::Calculate_Etime(OP *op) 
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  return OPSCH_estart(opsch);
}

INT 
HB_Schedule::Calculate_Ltime(OP *op) 
{
  OPSCH *opsch = OP_opsch(op, _hb_map);
  return OPSCH_lstart(opsch);
}

BOOL
HB_Schedule::Can_Schedule_Op (OP *cur_op, INT cur_time)
{
  if (Check_Resource_Usage (cur_op, cur_time)) return TRUE;
  
  return FALSE;
}

void
HB_Schedule::Schedule_Block (BB *bb, BBSCH *bbsch)
{
  _sched_vector = VECTOR_Init (BB_length(bb), &_hb_pool);

  list<BB*> blocks;
  blocks.push_back(bb);

  if ( LOCS_Enable_Xblock_Latency ) {
    _do_xblock_latency = HBS_Local_Sched() && (CG_opt_level >= 2) &&
                         (BB_length(bb) > 1);
  } else {
    _do_xblock_latency = false;
  }

#ifdef TARG_IA64
  Init_RFlag_Table (blocks, TRUE);
#else
  Init_RFlag_Table (blocks, FALSE);
#endif

  Compute_OPSCH (bb, _do_xblock_latency, _hb_map, &_hb_pool);
  if (!Get_Trace(TP_SCHED, 0x800) && 
      (HBS_Depth_First() || HBS_Minimize_Regs())) 
    HB_reg_graph.init(bb, _hb_map, &_hb_pool);

  if (HBS_Minimize_Regs() && !HBS_Before_GRA()) {
    Init_Register_Map (bb);
  }

  Priority_Selector *priority_fn;
  Cycle_Selector *cycle_fn;

#ifdef TARG_IA64
    // Do forward scheduling and cycle selector.
    priority_fn = 
      CXX_NEW(List_Based_Fwd(bb, this, _hbs_type, &_hb_pool), &_hb_pool);
    cycle_fn = CXX_NEW(Fwd_Cycle_Sel(), &_hb_pool);

#else 
    // Do backward scheduling and cycle selector.
    priority_fn = 
      CXX_NEW(List_Based_Bkwd(bb, this, _hbs_type, &_hb_pool), &_hb_pool);
    cycle_fn = CXX_NEW(Bkwd_Cycle_Sel(), &_hb_pool);

#endif

  OP *cur_op;
  OP *xfer_op = BB_xfer_op(bb);

  // If backward schedule and the basic block ends in a control transfer,
  // try to schedule the delay slot (if present) first and then schedule
  // the xfer_op.

  if (!priority_fn->Is_Fwd_Schedule()) {
    if (xfer_op) {
      if (TI_PROC_Property_Set(PROP_has_branch_delay_slot)) {
	cur_op = priority_fn->Select_OP_For_Delay_Slot (xfer_op);
	if (cur_op) {
	  Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
	}
      } 
      Add_OP_To_Sched_Vector(xfer_op, priority_fn->Is_Fwd_Schedule());
    }
  }

  INT cur_time;
  // Now iterate through the rest of the ops.
  for (cur_op = (OP*) priority_fn->Get_Next_Element(this); cur_op != NULL; 
       cur_op = (OP*) priority_fn->Get_Next_Element(this)) {
#if 0
    if (!OP_dummy(cur_op)) {
      INT etime = Calculate_Etime(cur_op);
      INT ltime = Calculate_Ltime(cur_op);
      cycle_fn->Init(cur_op, etime, ltime);

      for (cur_time = cycle_fn->Get_Cycle(); cur_time >= cycle_fn->Bound(); 
	   cur_time = cycle_fn->Next_Cycle()) {
	if (Can_Schedule_Op(cur_op, cur_time)) break;
      }
    }

    Is_True(cur_time < cycle_fn->Bound(),("Invalid cycle boundary, HB_SCHED"));
#endif
    Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
  }
    
  // Insert the scheduled list of instructions into the bb.
  INT32 cycles_saved = Put_Sched_Vector_Into_BB (bb, bbsch, 
					    priority_fn->Is_Fwd_Schedule());
  if (!Get_Trace(TP_SCHED, 0x800) && 
      (HBS_Depth_First() || HBS_Minimize_Regs())) 
    HB_reg_graph.free();

  // We tried the critical path heuristic before GRA and got a better schedule,
  // but we may be using too many registers, so save the current fatpoint
  // from a minimal register schedule in case we need to be less aggressive.
  if ((cycles_saved > 0) && (_hbs_type & HBS_BEFORE_GRA) && 
      (_hbs_type & HBS_CRITICAL_PATH)) {
    bb->secondary_cost = cycles_saved;
    LRA_Save_Register_Request(bb);
    if (Get_Trace(TP_SCHED, 0x8000))
      FmtAssert(FALSE, ("expecting memory operation"));
  }
}

void
HB_Schedule::Schedule_Blocks (list<BB*>& bblist)
{
  list<BB*>::iterator bbi;
  UINT32 length = 0;

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    length += BB_length(*bbi);
  }

  _sched_vector = VECTOR_Init (length, &_hb_pool);

  Init_RFlag_Table (bblist, TRUE);
  Compute_OPSCHs (bblist, _hb_map, &_hb_pool);

  /* TODO: Need to model register usages for hyperblocks soon. 
  if (Cur_Locs_Type & HBS_MINIMIZE_REGS) {
    Init_Register_Map (bb);
  } */

  List_Based_Fwd *priority_fn = 
    CXX_NEW(List_Based_Fwd(bblist, this, _hbs_type, &_hb_pool), &_hb_pool);

  Fwd_Cycle_Sel *cycle_fn = CXX_NEW(Fwd_Cycle_Sel(), &_hb_pool);

  OP *cur_op;
  INT cur_time;

  // Now iterate through the rest of the ops.
  for (cur_op = (OP*) priority_fn->Get_Next_Element(this); cur_op != NULL; 
       cur_op = (OP*) priority_fn->Get_Next_Element(this)) {

    if (!OP_dummy(cur_op)) {
      INT etime = Calculate_Etime(cur_op);
      INT ltime = Calculate_Ltime(cur_op);
      cycle_fn->Init(cur_op, etime, ltime);

      for (cur_time = cycle_fn->Get_Cycle(); cur_time != cycle_fn->Bound(); 
	   cur_time = cycle_fn->Next_Cycle()) {

	if (Can_Schedule_Op(cur_op, cur_time)) break;
      }
    }

    // Is_True(cur_time >= cycle_fn->Bound(), ("Invalid cycle boundary, HB_SCHED"));

    Add_OP_To_Sched_Vector(cur_op, priority_fn->Is_Fwd_Schedule());
  }
    
  // Insert the scheduled list of instructions into the bb.
  Put_Sched_Vector_Into_HB (bblist);

}

// ======================================================================
// HB_Schedule
//
// Algorithm:
//
//    1. Build a list of OPs that is ready to be scheduled. This is
//       the list of OPs with no successors in the dependence graph.
//    2. Select one of the OPs from the ready list based on some
//       heuristics as the next OP to schedule.
//    3. Delete the scheduled OP from the ready list and add any new
//       ones that might be ready now.
//    4. Repeat steps 2. and 3. till all OPs have been scheduled.
//    5. Put the scheduled list of OPs back into the basic block.
// ======================================================================

HB_Schedule::HB_Schedule() :
  _do_xblock_latency(false)
{
  _prolog_bb = NULL;
  _epilog_bb = NULL;

  // Initialize memory pool for use in the scheduling this bb.
  MEM_POOL_Initialize (&_hb_pool, "HB_pool", FALSE);
  MEM_POOL_Initialize (&_hb_map_pool, "HB_map_pool", FALSE);
  MEM_POOL_Push(&_hb_pool);
  MEM_POOL_Push (&_hb_map_pool);

  _hb_map = BB_MAP_Create ();
  Trace_HB = Get_Trace (TP_SCHED, 1);
}

void
HB_Schedule::Init(BB *bb, HBS_TYPE hbs_type, INT32 max_sched,
		  BBSCH *bbsch, mINT8 *regs_avail)
{
  _hbs_type = hbs_type;
  _max_sched = max_sched;
  if (regs_avail) {
    ISA_REGCLASS i;
    FOR_ALL_ISA_REGCLASS(i)
      {
	_Cur_Regs_Avail[i] = regs_avail[i];
  	if (!(_hbs_type & HBS_BEFORE_GRA) && (_hbs_type & HBS_MINIMIZE_REGS)) {
  	  if (_Cur_Regs_Avail[i] < LRA_Secondary_Register_Request(bb,i))
  	    _hbs_type |= HBS_IGNORE_LATENCY;
  	}
      }
  }

  BB_OP_MAP omap = BB_OP_MAP_Create(bb, &_hb_map_pool);
  BB_MAP_Set(_hb_map, bb, omap);

  _ready_vector = VECTOR_Init (BB_length(bb), &_hb_pool);
  _block_fatpoint = 0;
  _live_vals = 0;
}

void
HB_Schedule::Init(list<BB*> bblist, HBS_TYPE hbs_type, mINT8 *regs_avail)
{
  _hbs_type = hbs_type;
  if (regs_avail) {
    ISA_REGCLASS i;
    FOR_ALL_ISA_REGCLASS(i)
      {
	_Cur_Regs_Avail[i] = regs_avail[i];
      }
  }

  UINT32 length = 0;
  list<BB*>::iterator bbi;

  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    BB_OP_MAP omap = BB_OP_MAP_Create(*bbi, &_hb_map_pool);
    BB_MAP_Set(_hb_map, *bbi, omap);
    length += BB_length(*bbi);
  }

  _ready_vector = VECTOR_Init (length, &_hb_pool);
  _block_fatpoint = 0;
  _live_vals = 0;
}

void
HB_Schedule::Schedule_BB (BB *bb, BBSCH *bbsch)
{

  Invoke_Pre_HBS_Phase(bb);

  list<BB*> bblist;
  bblist.push_back(bb);

  if (CG_DEP_Prune_Dependence &&  // if the flag is turned ON.
      CGTARG_Can_Predicate() &&
      !BB_predicate_promote(bb))   // if the target arch provides predication.
    {
      CG_DEP_Prune_Dependence_Arcs(bblist, PRUNE_PREDICATE_ARCS, FALSE);
      Set_BB_predicate_promote(bb);
    }

  // if there are only zero or one instructions in the basic block,
  // there is nothing to schedule.
  BOOL skip_sched = FALSE;;
  if (CG_skip_local_sched) {
    BBs_Processed++;
    skip_sched =  (BBs_Processed < CG_local_skip_before ||
		   BBs_Processed > CG_local_skip_after ||
		   BBs_Processed == CG_local_skip_equal);
    if (!skip_sched)
      fprintf (TFile, "[%d] BB:%d processed in HB_Schedule_BB\n", 
	BBs_Processed, BB_id(bb));
  }

  if (BB_length(bb) > 0 && !skip_sched) {
    if (BB_length(bb) > 1) {

      CG_DEP_Compute_Graph (
	  bb, 
	  (this->HBS_From_CGPREP()) ? NO_ASSIGNED_REG_DEPS : 
	                               INCLUDE_ASSIGNED_REG_DEPS,
	  NON_CYCLIC,
	  INCLUDE_MEMREAD_ARCS,
	  INCLUDE_MEMIN_ARCS,
	  (Is_Target_Itanium()) ? INCLUDE_CONTROL_ARCS : NO_CONTROL_ARCS,
	  NULL);

      if (Trace_HB) CG_DEP_Trace_Graph (bb);

      Schedule_Block (bb, bbsch);

      CG_DEP_Delete_Graph (bb);
    } else {
      if (bbsch)
        BBSCH_schedule_length (bbsch) = 1; // record 1 cycle without scheduling
    }
    Set_BB_scheduled (bb);
    Set_BB_scheduled_hbs (bb);  // scheduled from hbs
    if (Assembly) Add_Scheduling_Note (bb, (void*) bbsch);
  }
  
  Invoke_Post_HBS_Phase(bb);

  // If we are scheduling before GRA, compute an estimate of the registers
  // that will be needed by LRA. No need to compute register request, if
  // HBS is invoked from CGPREP or for the second time within GCM. We model
  // the local register usage counts separately.

  if (HBS_Before_GRA() && !HBS_From_CGPREP()) {

    if (!Get_Trace (TP_SCHED, 0x2000)) {

      // Assumes that <bbsch> is computed fopass      
      mINT8 *fatpoint = (_hbs_type & HBS_FROM_PRE_GCM_SCHED_AGAIN) ?
      BBSCH_local_regcost(bbsch) : LRA_Compute_Register_Request(bb, &_hb_pool);
      if (HBS_From_Pre_GCM_Sched()) {
        Set_BB_local_regcost(bbsch, fatpoint);
      }
    }
  }
}

void
HB_Schedule::Schedule_HB (list<BB*> bblist)
{

  Invoke_Pre_HBB_Phase(bblist);

  if (CG_DEP_Prune_Dependence &&  // if the flag is turned ON.
      CGTARG_Can_Predicate())   // if the target arch provides predication.
    {
      CG_DEP_Prune_Dependence_Arcs(bblist, PRUNE_PREDICATE_ARCS, FALSE);
    }

  CG_DEP_Compute_Region_Graph(bblist, 
			      (this->HBS_From_CGPREP()) ? 
			      NO_ASSIGNED_REG_DEPS : INCLUDE_ASSIGNED_REG_DEPS,
			      INCLUDE_MEMREAD_ARCS,
			      INCLUDE_CONTROL_ARCS);

  Schedule_Blocks (bblist);

  CG_DEP_Delete_Graph (&bblist);

  Invoke_Post_HBB_Phase(bblist);
  list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(bblist, bbi) {
    Set_BB_scheduled (*bbi);
    Set_BB_scheduled_hbs (*bbi);  // scheduled from hbs
  }

  // if (Assembly) Add_Scheduling_Note (bb, (void*) bbsch);
  
}

HB_Schedule::~HB_Schedule()
{
  BB_MAP_Delete (_hb_map);

  MEM_POOL_Pop (&_hb_pool);
  MEM_POOL_Pop (&_hb_map_pool);
  MEM_POOL_Delete (&_hb_pool);
  MEM_POOL_Delete (&_hb_map_pool);
}

void reg_graph::init(BB *bb, BB_MAP opsch_map, MEM_POOL *pool)
{
  _pool = pool;
  _op_map = OP_MAP_Create();
  _opsch_map = opsch_map;
  _op_map_ok = true;
  struct reg_graph_node *head_node = TYPE_MEM_POOL_ALLOC(struct reg_graph_node, pool);
  head_node->init((OP*)1, 0, BB_length(bb), 0, pool);
  OP_MAP_Set(_op_map, (OP*)1, head_node);
  _live_tns = TN_SET_Create_Empty(Last_TN + 1, pool);
  TN *tn;
  for (tn = GTN_SET_Choose(BB_live_out(bb));
       tn != GTN_SET_CHOOSE_FAILURE;
       tn = GTN_SET_Choose_Next(BB_live_out(bb), tn)) {
    _live_tns = TN_SET_Union1D(_live_tns, tn, pool);
  }
}

INT reg_graph::new_live_vals(OP *op)
{
  struct reg_graph_node *node = (struct reg_graph_node *)OP_MAP_Get(_op_map, op);
  return node->new_pred_live_vals + node->num_dead_opnds - node->num_exposed_opnds
    - node->num_reg_results;
}

INT reg_graph::min_register_usage(OP *op)
{
  struct reg_graph_node *node = (struct reg_graph_node *)OP_MAP_Get(_op_map, op);
  return MAX(0, node->max_pred_usage + node->new_pred_live_vals) + node->num_dead_opnds - 
    node->num_reg_results;
}

INT reg_graph::op_live_vals(OP *op)
{
  struct reg_graph_node *node = (struct reg_graph_node *)OP_MAP_Get(_op_map, op);
  return node->num_dead_opnds - node->num_exposed_opnds - node->num_reg_results;
}

void reg_graph::add_op_to_graph(OP *op)
{
  struct reg_graph_node *node = TYPE_MEM_POOL_ALLOC(struct reg_graph_node, _pool);
  OPSCH *opsch = OP_opsch (op, _opsch_map);
  node->init(op, OPSCH_num_preds(opsch)+OP_opnds(op), OPSCH_num_succs(opsch), 
	     OPSCH_dfsnum(opsch), _pool);
  OP_MAP_Set(_op_map, op, node);
  ARC_LIST *arcs;
  node->num_dead_opnds = OP_opnds(op);
  for (INT i=0; i<OP_results(op); i++) {
    TN* tn = OP_result(op,i);
    if (TN_is_register(tn) && 
	!TI_ISA_Regclass_Is_State(TN_register_class(tn))) {
      node->num_reg_results++;
    }
  }
  for (INT i=0; i<OP_opnds(op); i++) {
    TN* tn = OP_opnd(op,i);
    if (!(TN_is_register(tn) && 
	  !TI_ISA_Regclass_Is_State(TN_register_class(tn)))) {
      node->num_dead_opnds--;
    } else if (TN_is_global_reg(tn) && TN_SET_MemberP(_live_tns, tn)) {
      BOOL read_write = FALSE;
      for (INT j=0; j<OP_results(op); j++) {
	if (tn == OP_result(op, j))
	  read_write = TRUE;
      }
      for (arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if (ARC_kind(arc)==CG_DEP_REGANTI) {
	  TN* reg_tn = OP_opnd(ARC_pred(arc),ARC_opnd(arc));
	  if (reg_tn == tn)
	    read_write = TRUE;
	}
      }
      if (!read_write) {
	node->live_opnd_mask |= 1<<i;
	node->num_dead_opnds--;
      }
    }
  }
  INT exposed_opnd_mask = -1;
  for (arcs = OP_preds(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    OP *pred_op = ARC_pred(arc);
    struct reg_graph_node *pred_node = 
      (struct reg_graph_node *)OP_MAP_Get(_op_map, pred_op);
    OPSCH *pred_opsch = OP_opsch (pred_op, _opsch_map);
    if (Is_Ldst_Addiu_Pair (pred_opsch, opsch, pred_op, op))
      continue;
    if (!VECTOR_Member_Element(node->preds, pred_node)) {
      VECTOR_Add_Element(node->preds, pred_node);
      VECTOR_Add_Element(pred_node->succs, node);
      if (OPSCH_num_unique_succs(pred_opsch)==1)
	add_tree_branch(node, pred_node, TRUE);
    }
    if (ARC_kind(arc)==CG_DEP_REGIN) {
      TN* reg_tn = OP_opnd(ARC_succ(arc),ARC_opnd(arc));
      exposed_opnd_mask &= ~(1<<ARC_opnd(arc));
      if (TN_is_register(reg_tn) &&
	  !TI_ISA_Regclass_Is_State(TN_register_class(reg_tn)) &&
	  (OPSCH_num_unique_succs(pred_opsch)!=1) &&
	  (((1<<ARC_opnd(arc))&node->live_opnd_mask) == 0)) {
	node->split_opnd_mask |= 1<<ARC_opnd(arc);
      }
    }
  }
  for (INT i=0; i<OP_opnds(op); i++) {
    TN* tn = OP_opnd(op,i);
    if (TN_is_register(tn) && 
	!TI_ISA_Regclass_Is_State(TN_register_class(tn)) &&
	(((1<<i)&exposed_opnd_mask) != 0)) {
      if (TN_is_global_reg(tn)) {
	struct reg_graph_node *pred_node = 
	  (struct reg_graph_node *)OP_MAP_Get(_op_map, (OP*)1);
	if (!VECTOR_Member_Element(node->preds, pred_node)) {
	  VECTOR_Add_Element(node->preds, pred_node);
	  VECTOR_Add_Element(pred_node->succs, node);
	}
	if (((1<<i)&node->live_opnd_mask) == 0)
	  node->split_opnd_mask |= 1<<i;
      } else {
	node->num_exposed_opnds++;
      }
    }
  }
}

void reg_graph::add_tree_branch(struct reg_graph_node *node, 
				struct reg_graph_node *pred_node, BOOL first_time)
{
  ARC_LIST *arcs;
  INT dep_mask = 0;
  INT reg_deps = 0;
  for (arcs = OP_preds(node->op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
    ARC *arc = ARC_LIST_first(arcs);
    if ((ARC_pred(arc) == pred_node->op) && (ARC_kind(arc)==CG_DEP_REGIN)) {
      dep_mask |= 1<<ARC_opnd(arc);
      if (((1<<ARC_opnd(arc)) & node->live_opnd_mask) == 0)
	  reg_deps++;
    }
  }
  if (0 < new_live_vals(pred_node->op)) {
    node->split_opnd_mask |= (dep_mask & ~node->live_opnd_mask);
    return;
  }
  if ((node->split_opnd_mask & dep_mask) != 0) {
    node->split_opnd_mask &= ~dep_mask;
    // node->num_dead_opnds += reg_deps;
  }
  node->max_pred_usage = 
    MAX(node->max_pred_usage, 
	min_register_usage(pred_node->op)-new_live_vals(pred_node->op));
  node->total_pred_usage += pred_node->total_pred_usage;
  node->new_pred_live_vals += new_live_vals(pred_node->op);
  if (Trace_HB) {
    fprintf(TFile, "add_tree_branch %2d to %2d:\n", pred_node->dfsnum, node->dfsnum);
  }
  while (node->succs && (VECTOR_count(node->succs) == 1)) {
    struct reg_graph_node *succ_node = 
      (struct reg_graph_node *)VECTOR_element(node->succs, 0);
    recompute_node(succ_node);
    node = succ_node;
  }
}

void reg_graph::schedule_op(OP *op)
{
  INT i;
  struct reg_graph_node *node = (struct reg_graph_node *)OP_MAP_Get(_op_map, op);
  INT count = (node->preds ? VECTOR_count(node->preds) : 0);
  for (i=0; i<OP_results(op); i++) {
    _live_tns = TN_SET_Difference1D(_live_tns, OP_result(op, i));
  }
  if (node->split_opnd_mask != 0) {
    for (i=0; i<OP_opnds(op); i++) {
      if (((1<<i) & node->split_opnd_mask) != 0) {
	_live_tns = TN_SET_Union1D(_live_tns, OP_opnd(op, i), _pool);
      }
    }
    for (i=0; i<count; i++) {
      struct reg_graph_node *pred_node = 
	(struct reg_graph_node *)VECTOR_element(node->preds, i);
      for (INT j=0; j<VECTOR_count(pred_node->succs); j++) {
	struct reg_graph_node *succ_node = 
	  (struct reg_graph_node *)VECTOR_element(pred_node->succs, j);
	if ((succ_node != node) && (succ_node->split_opnd_mask != 0)) {
	  update_live_count(succ_node);
	}
      }
    }
  }
  for (i=0; i<count; i++) {
    struct reg_graph_node *pred_node = 
      (struct reg_graph_node *)VECTOR_element(node->preds, i);
    VECTOR_Delete_Element(pred_node->succs, node);
    if (VECTOR_count(pred_node->succs) == 1) {
      OP *pred_op = pred_node->op;
      struct reg_graph_node *succ_node = 
	(struct reg_graph_node *)VECTOR_element(pred_node->succs, 0);
      OP *succ_op = succ_node->op;
      if (pred_op != (OP*)1) {
	add_tree_branch(succ_node, pred_node, FALSE);
      }
    }
  }
  if (node->preds)
    VECTOR_Reset(node->preds);
}

void reg_graph::update_live_count(struct reg_graph_node *node)
{
  INT newly_live_opnds = 0;
  INT old_live_vals = new_live_vals(node->op);
  OP *op = node->op;
  for (INT i=0; i<OP_opnds(op); i++) {
    if (((1<<i) & node->split_opnd_mask) != 0) {
      TN *tn = OP_opnd(op, i);
      if (TN_SET_MemberP(_live_tns, tn)) {
	BOOL read_write = FALSE;
	for (INT j=0; j<OP_results(op); j++) {
	  if (tn == OP_result(op, j))
	    read_write = TRUE;
	}
	for (ARC_LIST *arcs = OP_succs(op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	  ARC *arc = ARC_LIST_first(arcs);
	  OP *succ_op = ARC_succ(arc);
	  OPSCH *succ_opsch = OP_opsch(succ_op, _opsch_map);
	  if ((ARC_kind(arc)==CG_DEP_REGANTI) && !OPSCH_scheduled(succ_opsch)) {
	    TN* reg_tn = OP_opnd(ARC_pred(arc),ARC_opnd(arc));
	    if (reg_tn == tn)
	      read_write = TRUE;
	  }
	}
	if (!read_write) {
	  newly_live_opnds++;
	  node->live_opnd_mask |= 1<<i;
	  node->split_opnd_mask &= ~(1<<i);
	  node->num_dead_opnds--;
	}
      }
    }
  }
  if (newly_live_opnds > 0) {
    if (Trace_HB) {
      Print_OP_No_SrcLine (node->op);
      fprintf(TFile, "update_live_count %2d:", node->dfsnum);
      print_op_stats(node->op);
    }
    while (node->succs && (VECTOR_count(node->succs) == 1)) {
      struct reg_graph_node *succ_node = 
	(struct reg_graph_node *)VECTOR_element(node->succs, 0);
      recompute_node(succ_node);
#if 0
      ARC_LIST *arcs;
      INT reg_deps = 0;
      for (arcs = OP_succs(node->op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if ((ARC_succ(arc) == succ_node->op) && (ARC_kind(arc)==CG_DEP_REGIN))
	  reg_deps++;
      }
      if (0 < new_live_vals(node->op)) {
	break;
      } else if (reg_deps < old_live_vals) {
	old_live_vals = new_live_vals(succ_node->op);
	succ_node->num_dead_opnds += reg_deps;
	succ_node->max_pred_usage = 
	  MAX(succ_node->max_pred_usage, 
	      min_register_usage(node->op)-new_live_vals(node->op));
	succ_node->total_pred_usage += node->total_pred_usage;
	succ_node->new_pred_live_vals += new_live_vals(node->op);
      } else {
	old_live_vals = new_live_vals(succ_node->op)
      }
#endif
      node = succ_node;
    }
  }
}

void reg_graph::free()
{
  OP_MAP_Delete(_op_map);
  _op_map_ok = false;
}

void reg_graph::print_op_stats(OP *op)
{
  struct reg_graph_node *node = _op_map_ok?
    (struct reg_graph_node *)OP_MAP_Get(_op_map, op):NULL;
  if (node != NULL) {
    fprintf(TFile, "    <pred usage:%d pred live:%d live_mask:%#x split mask:%#x live:%d usage:%d>\n", 
	    node->max_pred_usage, node->new_pred_live_vals, node->live_opnd_mask, 
	    node->split_opnd_mask,
	    new_live_vals(op), min_register_usage(op));
  }
}

void reg_graph::recompute_node(struct reg_graph_node *node)
{
  node->max_pred_usage = 0; 
  node->total_pred_usage = 0; 
  node->new_pred_live_vals = 0;
  for (INT i=0; i<VECTOR_count(node->preds); i++) {
    struct reg_graph_node *pred_node = 
      (struct reg_graph_node *)VECTOR_element(node->preds, i);
    if (VECTOR_count(pred_node->succs) == 1) {
      ARC_LIST *arcs;
      INT dep_mask = 0;
      INT reg_deps = 0;
      for (arcs = OP_preds(node->op); arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
	ARC *arc = ARC_LIST_first(arcs);
	if ((ARC_pred(arc) == pred_node->op) && (ARC_kind(arc)==CG_DEP_REGIN)) {
	  dep_mask |= 1<<ARC_opnd(arc);
	  if (((1<<ARC_opnd(arc)) & node->live_opnd_mask) == 0)
	    reg_deps++;
	}
      }
      if (0 >= new_live_vals(pred_node->op)) {
	if ((node->split_opnd_mask & dep_mask) != 0) {
	  // node->num_dead_opnds += reg_deps;
	  node->split_opnd_mask &= ~dep_mask;
	}
	node->max_pred_usage = 
	  MAX(node->max_pred_usage, 
	      min_register_usage(pred_node->op)-new_live_vals(pred_node->op));
	node->total_pred_usage += pred_node->total_pred_usage;
	node->new_pred_live_vals += new_live_vals(pred_node->op);
      }
    }
  }
  if (Trace_HB) {
    Print_OP_No_SrcLine (node->op);
    fprintf(TFile, "recompute_node %2d:", node->dfsnum);
    print_op_stats(node->op);
  }
}


/* Topological sort of BBs. The return value is the list in which
   all predecesors of a BB will appear before this BB, except for
   a loop.  For a loop, the loop head will appear first.
 */
BBLIST *TopSort_BBs (BB *entry_bb)
{
  BB *bb;

}
