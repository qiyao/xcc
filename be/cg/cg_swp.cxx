
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



#define USE_STANDARD_TYPES
#include "defs.h"
#include "cg.h"
#include "errors.h"
#include "tracing.h"
#include "timing.h"
#include "glob.h"    // for Cur_PU_Name
#include "op.h"
#include "bb.h"
#include "cg_loop.h"
#include "cg_loop_mii.h"
#include "cgtarget.h"
#include "findloops.h"
#include "cgprep.h"
#include "ti_res_count.h"
#include "cg_special_op.h"
#include "cg_swp.h"
#include "cg_swp_bundle.h"
#include "cg_swp_options.h"
#include "cg_swp_target.h"
#include "cg_dep_graph.h"

#include <vector>

using std::vector;

static INT loop_index;

// Contains SWP options
//
SWP_OPTIONS SWP_Options;

OP_MAP swp_op_map=NULL;

#ifndef HAS_ROTATING_REGISTERS
void SWP_REG_ASSIGNMENT::Init_for_Multireg_oprands(ISA_REGSUBCLASS multireg_sc)
{
  /*
     non_rotating_reg[] has been initialized to be 1-bit register set that is
     available for allocation. To let SWP work on loops with boolean operands
     of the same size, we need to hack this for this particular size. 
     For instance,  
       assume
            non_rotating_reg[ISA_REGCLASS_branch] = 0xff3c  
       and assume that the size of boolean operands is 4 bits, we want
       to change non_rotating_reg[] to be:

            non_rotating_reg[ISA_REGCLASS_branch] = 0x1100
       which means only b8 and b12 can be used. 
   */
       
  // calculate the number of bits for this subclass
  const ISA_REGCLASS_INFO *icinfo = TI_ISA_Regclass_Info(ISA_REGCLASS_branch);
  const ISA_REGSUBCLASS_INFO *scinfo = TI_ISA_Regsubclass_Info(multireg_sc);
  INT reg_1bit_count  = TI_ISA_Regclass_Last_Reg(icinfo) -
                        TI_ISA_Regclass_First_Reg(icinfo) + 1;
  INT reg_count = TI_ISA_Regsubclass_Count(scinfo);
  int bit_size = reg_1bit_count / reg_count;

  Is_True( (reg_1bit_count % reg_count) == 0,
           ("Boolean register size imcompatible")); 
  
  UINT32 mask;
  switch (bit_size) {
  case 1:
    return;
  case 2:
    mask = 0x3;
    break;
  case 4:
    mask = 0xF;
    break;
  case 8:
    mask = 0xFF;
    break;
  case 16:
    mask = 0xFFFF;
    break;
  default:
    Is_True(false, ("bit_size: %d - not supported boolean operand size", bit_size));
  }

  REGISTER_SET allocatable = REGISTER_SET_EMPTY_SET;
  REGISTER_SET set = non_rotating_reg[ISA_REGCLASS_branch];

  INT i=0;
  do {
    REGISTER_SET_WORD w = REGISTER_SET_ELEM(set, i);
    INT bit = i * 8 * sizeof(REGISTER_SET_WORD); 
    do {
      if ((w & mask) == mask) {
        allocatable = REGISTER_SET_Union1(allocatable,bit+1);
      }
      bit += bit_size;
    } while (w >>= bit_size);
  } while (++i <= MAX_REGISTER_SET_IDX);

  non_rotating_reg[ISA_REGCLASS_branch] = allocatable;
}
#endif

void SWP_OPTIONS::PU_Configure()
{
  if (SWP_Opt_Level == 0) {
    Sched_Direction = 2;
    Heuristics = 2;
  }

  // if scheduling order is not explicitly specified, set it according to the
  // SWP opt level
  if (Scheduling_Order ==0) {
    if (SWP_Opt_Level == 3)
      Scheduling_Order = 3;
    else
      Scheduling_Order = 2;
  }

  if (!CGTARG_Can_Predicate())
    Enable_While_Loop = FALSE;

  if (!Max_Unroll_Times_Set)
    Max_Unroll_Times = INT_MAX;

  Min_Unroll_Times = Max(1, Min_Unroll_Times);
  Max_Unroll_Times = Max(1, Max_Unroll_Times);

  if (Min_Unroll_Times_Set)
    Max_Unroll_Times = Max(Max_Unroll_Times, Min_Unroll_Times);
  
  if (Max_Unroll_Times_Set)
    Min_Unroll_Times = Min(Min_Unroll_Times, Max_Unroll_Times);

  if (!Implicit_Prefetch_Set) {
    // Not all processors implement implicit prefetch -- disable
    // by default on those processors
    if (Is_Target_Itanium()) Implicit_Prefetch = FALSE;
  }
}


void SWP_OP::Print(FILE *fp) const {
  if (op) 
    fprintf(fp, "[%d] %s scale=%g cycle=%d mod=%d slot=%d trials=%d dir=%s\n",
	    Index(), placed?"placed":"not-placed", scale, cycle, modulo_cycle, 
	    slot, trials,
	    (direction==SWP_TOP_DOWN) ? "top_down" : 
	    ((direction==SWP_BOTTOM_UP) ? "bottom_up" : "unknown"));
  else
    fprintf(fp, "not an SWP op");
}

void SWP_OP_vector::Verify() const {
  for (INT i = 0; i < size(); i++) {
    if (v[i].op)
      FmtAssert(v[i].Index() == i, ("SWP_OP_vector::Verify: v[i].Index() != i"));
  }
}

void SWP_OP_vector::Print(FILE *fp) const {
  for (INT i = 0; i < size(); i++) {
    if (v[i].op) 
      v[i].Print(fp);
  }
  fprintf(TFile, "Invariants: ");
  TN_SET_Print(tn_invariants, fp);
  fprintf(TFile, "\n");
  fprintf(TFile, "Non-rotating: ");
  TN_SET_Print(tn_non_rotating, fp);
  fprintf(TFile, "\n");
}

void SWP_OP_vector::Print_Modulo_Schedule(FILE *fp) const {

  fprintf(TFile, "Modulo_Schedule (II=%d):\n===============\n", ii);
  for (INT i = 0; i < ii; i++) {
    for (INT j = 0; j < size(); j++) {
      if (v[j].op && v[j].modulo_cycle==i) {
	fprintf(fp,"%2d:", i);
	Print_OP_No_SrcLine(v[j].op);
      }
    }
    fprintf(TFile, "---------------\n");
  }
}


// check for other stalls not modeled by the resource between two operations
 
bool SWP_OP_vector::May_Stall(INT i, INT j, INT cycle_j) const {

    OP* op_i = v[i].op;
    OP* op_j = v[j].op;

    if (!OP_extern_effects(op_i) || !OP_extern_effects(op_j))
      return false;

    INT cycle_i = v[i].cycle % ii;
    cycle_j = cycle_j % ii;
    if (cycle_i==cycle_j)
      return false;

    INT cycle_ij = (cycle_j - cycle_i)%ii;
    INT ij_stall = OP_Stall(op_i,op_j);
    if (cycle_ij<0) cycle_ij += ii;
    if (ij_stall+1>cycle_ij)
      return true;

    INT cycle_ji = (cycle_i - cycle_j)%ii;
    INT ji_stall = OP_Stall(op_j,op_i);
    if (cycle_ji<0) cycle_ji += ii;
    if (ji_stall+1>cycle_ji)
      return true;

    return false;
}


// check for other stalls not modeled by the resource between
// already placed ops and the candidate 'op'

bool SWP_OP_vector::May_Stall(INT candidate, INT cycle) const {

    if (num_extern_effects_ops<=1 || !OP_extern_effects(v[candidate].op))
      return false;

    for (INT i = 0; i < size(); i++) {
      if (v[i].op == NULL || v[i].placed == false)
	continue;

      if (May_Stall(i, candidate, cycle))
	return true;
    }

    return false;

}

INT *swp_map_tbl;
INT  swp_map_tbl_max;

SWP_OP_vector::SWP_OP_vector(BB *body, BOOL doloop, MEM_POOL *pool)
{
  OP *op;
  INT max_idx = 0;

  // the following has to be done before the max_idx is calculated
  OP* next_op = BB_first_op(body);
  while (next_op) {
    op = next_op;
    next_op = OP_next(op);
    if (OP_code(op)==TOP_mov_n && !xt_density) {
      OP* new_op =
	    Mk_OP(TOP_or, OP_result(op,0), OP_opnd(op,0), OP_opnd(op,0));
      CGPREP_Init_Op(new_op);
      CG_LOOP_Init_Op(new_op);
      Set_OP_omega(new_op, 0, OP_omega(op,0));
      Set_OP_omega(new_op, 1, OP_omega(op,0));
      BB_Insert_Op_Before(body, op, new_op);
      BB_Remove_Op(body, op);
    }
  }

  FOR_ALL_BB_OPs(body, op) {
    max_idx = Max(max_idx, OP_map_idx(op));
  }
  swp_map_tbl_max = max_idx + 1;
  swp_map_tbl = TYPE_MEM_POOL_ALLOC_N(INT, pool, swp_map_tbl_max);

  for (INT i=0; i<swp_map_tbl_max; i++)
    swp_map_tbl[i] = -1;

  const bool trace = Get_Trace(TP_SWPIPE, 2);
  INT count = 0;
  FOR_ALL_BB_OPs(body, op) {
    if (!OP_loh(op)) {
      INT idx = OP_map_idx(op);
      swp_map_tbl[idx] = count++;
      if (trace)
	fprintf(TFile, "OP %3d maps to [%2d]\n", idx, swp_map_tbl[idx]);
    }
  }
  
  INT size = count;
  start = size++;
  stop = size++;
  previous_trials = 0;

#if SWP_USE_STL
  v.insert(v.begin(), size, SWP_OP());
#else
  {
    Is_True(size <= SWP_OPS_LIMIT, 
	    ("SWP_OP_vector: loop has too many (%d) ops.", size));
    v_size = size;
    for (INT i = 0; i < size; i++)
      v[i] = SWP_OP();
  }
#endif

  tn_non_rotating = TN_SET_Create_Empty(Last_TN + 1, pool);
  TN_SET *tn_defs = TN_SET_Create_Empty(Last_TN + 1, pool);
  TN_SET *tn_uses = TN_SET_Create_Empty(Last_TN + 1, pool);
  num_mops = 0;
  num_flops = 0;
  num_extern_effects_ops = 0;
  FOR_ALL_BB_OPs(body, op) if (!OP_loh(op)) {
    INT idx = SWP_index(op);
    v[idx].op = op;
    v[idx].issue_alignment = TI_TOP_Issue_Alignment(OP_code(op));
    if (OP_memory(op))
      num_mops++;
    if (OP_flop(op))
      num_flops++;
    if (OP_extern_effects(op))
      num_extern_effects_ops++;
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_register(tn)) {
	tn_defs = TN_SET_Union1D(tn_defs, tn, pool);
	tn_non_rotating = TN_SET_Union1D(tn_non_rotating, tn, pool);
      }
    }
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_register(tn)) {
	tn_uses = TN_SET_Union1D(tn_uses, tn, pool);
	tn_non_rotating = TN_SET_Union1D(tn_non_rotating, tn, pool);
      }
    }
  }
  // Identify loop invariants!
  tn_invariants = TN_SET_Difference(tn_uses, tn_defs, pool);
#ifdef TARG_IA64
  control_predicate_tn = OP_has_predicate(br_op) ? OP_opnd(br_op, OP_PREDICATE_OPND) : NULL;
#else
  control_predicate_tn = NULL;
#endif
  is_doloop = doloop;
  succeeded = false;
  loop_one_more_time = false;
}

SWP_OP_vector::SWP_OP_vector(SWP_OP_vector& vec, MEM_POOL *pool)
{
  v_size = vec.v_size;

  for (int i=0; i<v_size; i++) {
    v[i] = vec[i];
  }

  start=vec.start;
  stop=vec.stop;
  num_mops=vec.num_mops;
  num_flops=vec.num_flops;
  num_extern_effects_ops=vec.num_extern_effects_ops;

  control_predicate_tn=vec.control_predicate_tn;
  is_doloop=vec.is_doloop;
  loop_one_more_time=vec.loop_one_more_time;
  ii=vec.ii;
  ii_slots=vec.ii_slots;
  tn_invariants=vec.tn_invariants;
  tn_non_rotating=vec.tn_non_rotating;
  succeeded=vec.succeeded;

  min_ii=vec.min_ii;
  res_min_ii=vec.res_min_ii;
  rec_min_ii=vec.rec_min_ii;
  min_sl=vec.min_sl;
  sl=vec.sl;
  sc=vec.sc;
  previous_trials=vec.previous_trials;
  prep_time=vec.prep_time;
  sched_time=vec.sched_time;
  reg_alloc_time=vec.reg_alloc_time;
  code_gen_time=vec.code_gen_time;

}

template <class T1>
inline T1 linear_func(T1 x, double alpha, double beta)
{
  return (T1)((double)x * beta + alpha);
}

void SWP_Show_Statistics(const SWP_OP_vector& swp_op_vector, BB *body)
{
  INT nops = 0;
  INT trials = 0;
  for (INT i = 0; i < swp_op_vector.size(); i++) {
    if (swp_op_vector[i].op) {
      nops++;
      trials += swp_op_vector[i].trials;
    }
  }
  const char *banner = "<swps>";
  INT ii = swp_op_vector.ii;
  fprintf(TFile, "%s SWP for PU %s BB %d: %s\n", banner, 
	  Cur_PU_Name ? Cur_PU_Name : "noname", BB_id(body), 
	  (ii == 0) ? "failed" : 
	  ((ii == CG_LOOP_min_ii) ? "optimal" : "non-optimal"));
  fprintf(TFile, "%s  min II: %d\n", banner, swp_op_vector.min_ii);
  fprintf(TFile, "%s  ResMII: %d\n", banner, swp_op_vector.res_min_ii);
  fprintf(TFile, "%s  RecMII: %d\n", banner, swp_op_vector.rec_min_ii);
  fprintf(TFile, "%s  min SL: %d\n", banner, swp_op_vector.min_sl);   
  fprintf(TFile, "%s  found II: %d\n", banner, swp_op_vector.ii);
  fprintf(TFile, "%s  found SL: %d\n", banner, swp_op_vector.sl);
  fprintf(TFile, "%s  found SC: %d\n", banner, swp_op_vector.sc);
  fprintf(TFile, "%s  # ops: %d\n", banner, nops);
  fprintf(TFile, "%s  # trials: %d\n", banner, trials);
  fprintf(TFile, "%s  # total trials: %d\n", banner, trials + swp_op_vector.previous_trials);
  fprintf(TFile, "%s  prep time: %g\n", banner, swp_op_vector.prep_time);
  fprintf(TFile, "%s  sched time: %g\n", banner, swp_op_vector.sched_time);
  fprintf(TFile, "%s  reg alloc time: %g\n", banner, swp_op_vector.reg_alloc_time);
  fprintf(TFile, "%s  code gen time: %g\n", banner, swp_op_vector.code_gen_time);

  ANNOTATION *ant = ANNOT_Get(BB_annotations(body), ANNOT_ROTATING_KERNEL);
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(ant);
  const char *banner1 = "<swps>  ";
  TI_RES_COUNT_Emit_Note(banner1, TFile, ROTATING_KERNEL_INFO_res_counts(info), 
			   ROTATING_KERNEL_INFO_ii(info));
}


BOOL SWP_Failure(BB *body, SWP_RETURN_CODE code, ISA_REGCLASS rc)
{
  // Generate SWP ROTATING KERNEL Annotation
  ROTATING_KERNEL_INFO *info = TYPE_PU_ALLOC(ROTATING_KERNEL_INFO);
  memset(info, 0, sizeof(ROTATING_KERNEL_INFO));
  ROTATING_KERNEL_INFO_succeeded(info) = FALSE;
  ROTATING_KERNEL_INFO_failure_code(info) = code;
  ROTATING_KERNEL_INFO_unallocatable_rc(info) = rc;
  BB_Add_Annotation(body, ANNOT_ROTATING_KERNEL, (void *)info);
  Reset_BB_rotating_kernel(body);

  if (Get_Trace(TP_SWPIPE, 2)) {
    Emit_SWP_Note(body, TFile);
  }

  return FALSE;
}

bool
SWP_allowed_simulated_op(OP* op) {

  if (op==NULL) return false;

  if (OP_pseudo(op))
    return true;

  return false;
}

// Identify potential hardware/simulator problem
// 
SWP_RETURN_CODE Detect_SWP_Constraints(CG_LOOP &cl, bool trace)
{
  if (SWP_Options.Prep_Only)
    return SWP_PREP_ONLY;

  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);
  OP *op;
  INT op_count = 0;
  INT total_op_count = 0;
  ISA_REGSUBCLASS multireg_sc = ISA_REGSUBCLASS_UNDEFINED;
  FOR_ALL_BB_OPs(body, op) {
    if (!OP_br(op) && !OP_dummy(op) && !OP_loh(op))
      op_count++;
    total_op_count++;

    if (OP_code(op) == TOP_asm) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to TOP_asm.\n");
      return SWP_ASM;
    }
    if (OP_simulated(op) && !SWP_allowed_simulated_op(op)) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to simulated op (%s).\n",
			TI_TOP_Name(OP_code(op)));
      return SWP_SIMULATED_OP;
    }
    if (OP_has_multireg_operand(op)) {
      // for now, handle multireg_operands that are of the same
      // size within a loop. For example, the loop contains only 2bit 
      // boolean operands, or 4bit boolean operands; but never both.
      bool is_failed = false;
      ISA_REGSUBCLASS sc;
      const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(OP_code(op));
      const ISA_OPERAND_VALTYP *valtyp;
      for (int i=0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);
        if (TN_is_register(tn) && (TN_register_class(tn) == ISA_REGCLASS_branch)) {
          valtyp = TI_ISA_Op_Result (opinfo, i);
          sc =  TI_ISA_Valtyp_Regsubclass(valtyp);
          if ( sc == ISA_REGSUBCLASS_UNDEFINED) {
            sc = TI_ISA_Regsubclass_Branch1(); 
          }
          if (multireg_sc == ISA_REGSUBCLASS_UNDEFINED ) {
            multireg_sc = sc;
          } else if (multireg_sc != sc) {
            is_failed = true;
          }  
        }
      }

      for (int i=0; i < OP_opnds(op); i++) {
        TN *tn = OP_opnd(op, i);
        if (TN_is_register(tn) && (TN_register_class(tn) == ISA_REGCLASS_branch)) {
          valtyp = TI_ISA_Op_Operand (opinfo, i);
          sc =  TI_ISA_Valtyp_Regsubclass(valtyp);
          if ( sc == ISA_REGSUBCLASS_UNDEFINED) {
            sc = TI_ISA_Regsubclass_Branch1(); 
          }
          if (multireg_sc == ISA_REGSUBCLASS_UNDEFINED ) {
            multireg_sc = sc;
          } else if (multireg_sc != sc) {
            is_failed = true;
          }  
        }
      }

      if (is_failed) {
        if (trace) 
          fprintf(TFile, "SWP: skip optimization due to multireg_operand op (%s).\n",
			TI_TOP_Name(OP_code(op)));
        return SWP_MULTIREG_OP;
      } else {
        if (multireg_sc != ISA_REGSUBCLASS_UNDEFINED) {
          // do this if there are multireg boolean operands
          cl.Set_multireg_sc (multireg_sc);
        }
      }
    }
    if (OP_privileged(op)) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to privileged op (%s).\n",
			TI_TOP_Name(OP_code(op)));
      return SWP_PRIVILEGED_OP;
    }

    if (SWP_Options.Enable_Workaround) {
#ifdef TARG_IA64
      if (OP_code(op) == TOP_setf_sig) {
	DevWarn("SWP: skip optimization due to simulation of frcpa");
	if (trace) 
	  fprintf(TFile, "SWP: skip optimization due to simulation of frcpa.\n");
	return SWP_WORKAROUND;
      }
#endif
    }
    
    TN *found_ded_tn = NULL;
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_dedicated(tn) &&
	  !TN_is_const_reg(tn) &&
	  REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	found_ded_tn = tn;
	break;
      }
    }
    for (INT j = 0; j < OP_opnds(op); j++) {
      TN *tn = OP_opnd(op, j);
      if (TN_is_dedicated(tn) && 
	  !TN_is_const_reg(tn) && 
	  REGISTER_Is_Rotating(TN_register_class(tn), TN_register(tn))) {
	found_ded_tn = tn;
	break;
      }
    }
    if (found_ded_tn) {
      if (trace) 
	fprintf(TFile, "SWP: skip optimization due to rotating dedicated TN%d.\n",
		TN_number(found_ded_tn));
      return SWP_DEDICATED_ROT_REG;
    }
  }

  if (op_count == 0) 
    return SWP_LOOP_EMPTY;     // don't bother to swp empty loops

  if (total_op_count + SWP_OPS_OVERHEAD > SWP_OPS_LIMIT)
    return SWP_LOOP_LIMIT;
    
  return SWP_OK;
}


// prune loop-carried REGOUT dependences on rotating registers
static void
Prune_Regout_Deps(BB *body, TN_SET *non_rotating)
{
  vector<ARC*> arcs_to_delete;
  OP *op;
  FOR_ALL_BB_OPs(body, op) {
    if (_CG_DEP_op_info(op)) {
      for (ARC_LIST *arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs)) {
        ARC *arc = ARC_LIST_first(arcs);
        // look for loop-carried REGOUT dependences
        if (ARC_kind(arc) == CG_DEP_REGOUT && ARC_omega(arc) > 0) {
          // check that none of the OP results is in the non-rotating set
          bool redundant = true;
          for (INT i = 0; i < OP_results(op); i++) {
            TN *tn = OP_result(op,i);
            if (!TN_is_register(tn) ||
                TN_is_dedicated(tn) ||
                TN_SET_MemberP(non_rotating, tn)) {
              redundant = false;
              break;
            }
          }
          if (redundant) {
            arcs_to_delete.push_back(arc);
          }
        }
      }
    }
  }
  for (size_t i = 0; i < arcs_to_delete.size(); i++) {
    CG_DEP_Detach_Arc(arcs_to_delete[i]);
  }
}


static BOOL Is_Loop_Skipped(void)
/* -----------------------------------------------------------------------
 * Return a boolean that indicates if we should skip pipelining
 * the specified loop.
 * -----------------------------------------------------------------------
 */
{
  const BOOL skip_it = (   loop_index < CG_local_skip_before
			|| loop_index > CG_local_skip_after
			|| loop_index == CG_local_skip_equal);

  if (CG_skip_local_swp) {
    DevWarn("%s swp for loop: index=%d",
	    skip_it ? "Skipping" : "Attempting",
	    loop_index);
  }

  ++loop_index;

  return skip_it;
}


static void
SWP_Generate_Wind_Up_Wind_Down(
	CG_LOOP& cl, const SWP_OP_vector& swp_op_vector, BOOL trace)
/* -----------------------------------------------------------------------
 * Generate wind-up and wind-down code and append/prepend to the prolog
 * and epilog. Add backpatches to glue the wind-up/down code with the
 * body. This needs to be called after the loop is SWP scheduled and before
 * register allocation or code generation (i.e., right after call to
 * Modulo_Schedule()).
 * -----------------------------------------------------------------------
 */
{
  int stage_count = swp_op_vector.sc;
  int ii = swp_op_vector.ii;
  OPS *wind_up_ops = OPS_Create();
  OPS *wind_down_ops = OPS_Create();
  BB *head = cl.Prolog_end();
  BB *body = cl.Loop_header();
  BB *tail = cl.Epilog_start();
  CG_LOOP_DEF tn_def(body);
  OP *op;

  /* generate wind up/down operations and mark them with sections so that they
     can be properly guarded to handle small loop count case.

     for a 4-stage pipe (stage_count==4, op_stage=0..stage_count-1)
     Note all a1,..,a4 have exactly the same code, similarly for b, c, d stages.

	 1  2  3  4	<== iteration

	a1
	b1 a2		<== wind-up
	c1 b2 a3

	d1 c2 b3 a4	<== kernel

	   d2 c3 b4
	      d3 c4	<== wind-down
	         d4

     the code generated here will be:

	a1(section=1)
	b1(section=1)
	c1(section=1)
	a2(section=2)	<== wind-up
	b2(section=2)
	a3(section=3)
	glue_copies
	---
	d1 c2 b3 a4
	---
	glue_copies
	d2(section=3)
	c3(section=3)
	d3(section=2)
	b4(section=3)	<== wind-down
	c4(section=2)
	d4(section=1)

     in fix up in SWP_Emit(), the code will be transformed to:

	a1(section=1)
	b1(section=1)
	c1(section=1)
	blti tn101, 2, L1	# tn101 is the trip count

	a2(section=2)	<== wind-up
	b2(section=2)
	blti tn101, 3, L2

	a3(section=3)
	glue_copies
	---
	d1 c2 b3 a4
	---
	glue_copies
	d2(section=3)
	c3(section=3)
	b4(section=3)	<== wind-down

     L2:
	d3(section=2)
	c4(section=2)

     L1:
	d4(section=1)

  */

  for (int iter = 1; iter < stage_count; iter++) {
    int k=2*stage_count-1-iter;
    for (int i=0; i<swp_op_vector.size(); i++) {
      op =  swp_op_vector[i].op;

      if (op==NULL)	/* for start and stop nodes */
	continue;

      if (!OP_noop(op) && !OP_xfer(op)) {
        INT op_stage = swp_op_vector[i].cycle / ii;
	OP* new_op = Dup_OP(op);
	CG_LOOP_Init_Op(new_op);
	Copy_WN_For_Memory_OP(new_op, op);

	Set_OP_unroll_bb(new_op, OP_unroll_bb(op));
	// the following is necessary since this new_op can be re-scheduled with
	// other windup/winddown code from neighboring loops
	// without the unroll bb info, the LNO dep graph will say they are not
	// dependent since they were from different loop nests
	if (OP_unroll_bb(new_op)==NULL)
	  Set_OP_unroll_bb(new_op, body);

	Reset_OP_end_group(new_op);
	Reset_OP_bundled(new_op);
	Set_OP_format_num(new_op,-1);
	Set_OP_slot_num(new_op, -1);
	CG_Specialize_Op(new_op);

        if (op_stage<stage_count-iter) {
	  ARC_LIST *al;

	  if (SWP_Options.Enable_Mem_Offset_Relaxation)
	  for (al = OP_preds(op); al; al = ARC_LIST_rest(al)) {
	    ARC *arc = ARC_LIST_first(al);
	    if (ARC_is_marked(arc) && ARC_kind(arc)==CG_DEP_REGIN) {
	      // inc is an updating memop or addi-equivalent
	      // and current new_op is a load moved across
	      OP *inc = ARC_pred(arc);
	      INT inc_idx = SWP_index(inc);
	      INT inc_sched_cycle = swp_op_vector[inc_idx].cycle;
	      INT adjust=0;
	      INT inc_stage = inc_sched_cycle / ii;

	      // in wind-up, all dependences are preserved except when the
	      // source is in the kernel loop
	      // check if the source is in the kernel
	      // the following loop may be simplified to an expression but
	      // this form is easier to understand
	      //
	      // the idea is to find out how many instances of predecessors
	      // of the current and previous iterations have been delayed
	      // and are not executed in wind-up
	      //

	      // in the current iteration, check if the source of the
	      // dependence is a predecessor and is in the kernel
	      if (iter+inc_stage >= stage_count && inc_idx<i) {
		adjust++;
	      }

	      // next scan (with j) from the previous iteration
	      // for each iteration
	      // 	j+inc_stage >= stage_count is true
	      // if in j-iteration, the predecessor is executed as kernel
	      // and is missing from the wind-up

	      for (INT j=iter-1; j>0; j--) {
		if (j+inc_stage >= stage_count)
		  adjust++;
	      }
	      if (adjust>0) {
	        UINT8 succ_offset_idx = OP_find_opnd_use(new_op, OU_offset);
		UINT8 inc_offset_idx = 1;
		if ((OP_code(inc)==TOP_addi ||
		    (OP_generic(inc) && Is_Generic_Addi_Op(inc)))) {
		  inc_offset_idx = 1;	// for increment using addi
		} else {
		  inc_offset_idx = OP_find_opnd_use(inc, OU_offset);
		}

	        TN* inc_offset = OP_opnd(inc, inc_offset_idx);
	        TN* succ_offset = OP_opnd(new_op, succ_offset_idx);

	        INT new_offset = TN_value(succ_offset) + adjust*TN_value(inc_offset);
	        TN* new_offset_tn = Gen_Literal_TN(new_offset, TN_size(succ_offset));
	        Set_OP_opnd(new_op, succ_offset_idx, new_offset_tn);
	      }
	    }
	  }

	  // it is not necessary to patch for relaxed anti-depencens for induction
	  // variable because the patch is on source. Suppose an instance of the
	  // source is in wind-up. If the corresponding sink is in kernel then
	  // no patch is needed. If the sink is also in wind-up, it will be
	  // executed in original source order so no patch is needed, either.

          OPS_Append_Op(wind_up_ops, new_op);
	  Set_OP_swp_wind_up_section(new_op, iter);
	} else {
	  ARC_LIST *al;

	  if (SWP_Options.Enable_Mem_Offset_Relaxation)
	  for (al = OP_preds(op); al; al = ARC_LIST_rest(al)) {
	    ARC *arc = ARC_LIST_first(al);
	    if (ARC_is_marked(arc) && ARC_kind(arc)==CG_DEP_REGIN) {

	      // in wind-down, the code executed is the same as in kernel
	      // so we patch the sink (new_op) the same way as in kernel
	      // except for the same stage ops they are executed in iter order
	      // see Restore_Dependences()
	      OP *pred = ARC_pred(arc);
	      INT pred_idx = SWP_index(pred);
	      INT pred_sched_cycle = swp_op_vector[pred_idx].cycle;
	      INT pred_stage = pred_sched_cycle / ii;
	      INT sched_cycle = swp_op_vector[i].cycle;
	      INT adjust=0;
	      if (sched_cycle<=pred_sched_cycle) {
	        while (sched_cycle<=pred_sched_cycle) {
		  sched_cycle += ii;
		  adjust++;
		  pred_stage--;
	        }
		if (pred_stage<op_stage) {
	          adjust--;	// the predecessor in the same stage is
				// executed ahead of new_op in wind-down
				// so we over-adjust in that stage
				// this is different from modulo schedule
		}
	      }
	      // have to use the saved omega since the dependence has not been
	      // restored yet
	      adjust -= ARC_saved_omega(arc);
	      if (adjust>0) {
		UINT8 inc_offset_idx = 1;
		if ((OP_code(pred)==TOP_addi ||
		    (OP_generic(pred) && Is_Generic_Addi_Op(pred)))) {
		  inc_offset_idx = 1;   // for increment using addi
		} else {
		  inc_offset_idx = OP_find_opnd_use(pred, OU_offset);
		}
	        UINT8 succ_offset_idx = OP_find_opnd_use(new_op, OU_offset);
	        TN* pred_offset = OP_opnd(pred, inc_offset_idx);
	        TN* succ_offset = OP_opnd(new_op, succ_offset_idx);

	        INT new_offset = TN_value(succ_offset) + adjust*TN_value(pred_offset);
	        TN* new_offset_tn = Gen_Literal_TN(new_offset, TN_size(succ_offset));
	        Set_OP_opnd(new_op, succ_offset_idx, new_offset_tn);
	      }
	    }
	  }

	  if (SWP_Options.Enable_Mem_Offset_Relaxation)
	  for (al = OP_succs(op); al; al = ARC_LIST_rest(al)) {
	    ARC *arc = ARC_LIST_first(al);
	    if (ARC_is_marked(arc) && ARC_kind(arc)==CG_DEP_REGANTI) {

	      // patch in the same way as in kernel (see Restore_Dependences)
	      // current load/store (new_op) is the source of an anti-dependence
	      // and the sink is a base increment
	      OP *inc = ARC_succ(arc);
	      INT inc_idx = SWP_index(inc);
	      INT inc_sched_cycle = swp_op_vector[inc_idx].cycle;
	      INT sched_cycle = swp_op_vector[i].cycle;
	      INT inc_stage = inc_sched_cycle / ii;
	      INT adjust = 0;
	      INT inc_iter = iter;
	      // see how many inc has moved across the current load/store
	      if (inc_sched_cycle<sched_cycle) {
	        while (inc_sched_cycle<sched_cycle) {
	          inc_sched_cycle += ii;
	          if (inc_iter >= stage_count)
		    break;
		  adjust++;
		  inc_iter++;
		  inc_stage++;
	        }
		if (inc_stage>op_stage) {
	          adjust--;	// the predecessor (new_op) in the same stage
				// is executed ahead of inc in wind-down
				// so we over-adjust in that stage
				// this is different from modulo schedule
		}
	      }
	      // have to use the saved omega since the dependence has not been
	      // restored yet
	      adjust -= ARC_saved_omega(arc);
	      if (adjust>0) {
	        UINT8 offset_idx = OP_find_opnd_use(new_op, OU_offset);
		UINT8 inc_offset_idx = 1;
		if ((OP_code(inc)==TOP_addi ||
		    (OP_generic(inc) && Is_Generic_Addi_Op(inc)))) {
		  inc_offset_idx = 1;	// for increment using addi
		} else {
		  inc_offset_idx = OP_find_opnd_use(inc, OU_offset);
		}
	        TN* offset = OP_opnd(new_op, offset_idx);
	        TN* inc_offset = OP_opnd(inc, inc_offset_idx);

	        INT64 new_offset = (INT64)TN_value(offset) -
			(INT64)adjust * (INT64)TN_value(inc_offset);
	        TN* new_offset_tn = Gen_Literal_TN(new_offset, TN_size(offset));
	        Set_OP_opnd(new_op, offset_idx, new_offset_tn);
	      }
	    }
	  }
          OPS_Append_Op(wind_down_ops, new_op);
	  Set_OP_swp_wind_down_section(new_op, k-op_stage);
        }
      }
    }
  }
#if 0
  for (int stage = stage_count - 1; stage > 0; stage--) {
    for (int i=0; i<swp_op_vector.size(); i++) {
      op =  swp_op_vector[i].op;

      if (op==NULL)	/* for start and stop nodes */
	continue;

      if (!OP_noop(op) && !OP_xfer(op)) {
        INT op_stage = swp_op_vector[i].cycle / ii;
	OP* new_op = Dup_OP(op);
	CG_LOOP_Init_Op(new_op);
	Copy_WN_For_Memory_OP(new_op, op);
	Reset_OP_end_group(new_op);
	Reset_OP_bundled(new_op);

        if (op_stage<stage) {
          OPS_Append_Op(wind_up_ops, new_op);
	  Set_OP_swp_wind_up_stage(new_op, stage);
        } else {
          OPS_Append_Op(wind_down_ops, new_op);
	  Set_OP_swp_wind_down_stage(new_op, stage);
        }

      }
    }
  }
#endif
  BB_Append_Ops(head, wind_up_ops);
  BB_Prepend_Ops(tail, wind_down_ops);

  // remove dead backpatches
  TN_MAP first_use = TN_MAP_Create();
  FOR_ALL_BB_OPs(body, op) {

    if (OP_noop(op) || OP_xfer(op) || OP_loh(op))
      continue;

    int cycle = swp_op_vector[SWP_index(op)].cycle;
    int mod_cycle = cycle % ii;
    int stage = cycle / ii;

    // record the first read in the kernel loop
    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      TN *tn = OP_opnd(op,opnd);
      if (TN_is_register(tn) &&
	  !TN_is_dedicated(tn)) {
	if (TN_MAP_Get(first_use, tn)==NULL ||
	    (int)(TN_MAP_Get(first_use, tn))>mod_cycle) {
	  // in order to distinguish the cases of cycle 0 vs the NULL search
	  // result, we record the mod_cycle+1, i.e. all valid cycles [0..]
	  // is recorded as [1..]
	  TN_MAP_Set(first_use, tn, (void*)(mod_cycle+1));
	}
      }
    }
  }

  FOR_ALL_BB_OPs(body, op) {

    if (OP_noop(op) || OP_xfer(op) || OP_loh(op))
      continue;

    int cycle = swp_op_vector[SWP_index(op)].cycle;
    int mod_cycle = cycle % ii;
    int stage = cycle / ii;

    for (INT result = 0; result < OP_results(op); result++) {
      TN *tn = OP_result(op,result);
      if (TN_is_register(tn) &&
	  !TN_is_dedicated(tn)) {
        CG_LOOP_BACKPATCH* bp = CG_LOOP_Backpatch_First(head, tn);
	if (bp) {
	  Is_True(CG_LOOP_BACKPATCH_body_tn(bp)==tn &&
 		  CG_LOOP_BACKPATCH_omega(bp)==1 &&
 		  CG_LOOP_BACKPATCH_non_body_tn(bp)==tn &&
		  CG_LOOP_Backpatch_Next(bp)==NULL,
		  ("Backpatch error"));
	  TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);

	  // remove a backpatch if it is not live-in for the kernel loop
	  // this can occur if
	  // 1. there is a def in the wind-up
	  // 2. it is write-only
	  // 3. a def precedes the first read in the kernel loop
	  if (stage != stage_count-1 ||
	      TN_MAP_Get(first_use, body_tn)==NULL ||
	      (int)(TN_MAP_Get(first_use, body_tn))>mod_cycle+1) {
	    if (trace) {
	      fprintf(TFile, "SWP WUWD: delete prolog backpatches "
			      "with body TN%d[1]\n", 
			     TN_number(body_tn));
	    }
	    CG_LOOP_Backpatch_Delete(head, bp);
	  }
	}
	if (stage != 0) {
          bp = CG_LOOP_Backpatch_First(tail, tn);
	  if (bp) {
	    Is_True(CG_LOOP_BACKPATCH_body_tn(bp)==tn &&
 		    CG_LOOP_BACKPATCH_omega(bp)==0 &&
 		    CG_LOOP_BACKPATCH_non_body_tn(bp)==tn &&
		    CG_LOOP_Backpatch_Next(bp)==NULL,
		    ("Backpatch error"));
	    if (trace) {
	      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
	      fprintf(TFile, "SWP WUWD: delete epilog backpatches "
			      "with body TN%d\n", 
			     TN_number(body_tn));
	    }
	    CG_LOOP_Backpatch_Delete(tail, bp);
	  }
	}
      }
    }
  }
  TN_MAP_Delete(first_use);

  // Add epilog backpatch to prolog in case the kernel is skipped
  // due to small trip count
  CG_LOOP_BACKPATCH *bp;
  for (bp = CG_LOOP_Backpatch_First(tail, NULL);
       bp; bp
       = CG_LOOP_Backpatch_Next(bp)) {
    TN *tn = CG_LOOP_BACKPATCH_non_body_tn(bp);
    if (!CG_LOOP_Backpatch_Find_Non_Body_TN(head, tn, 1)) {
      TN *body_tn = CG_LOOP_BACKPATCH_body_tn(bp);
      CG_LOOP_Backpatch_Add(head, tn, body_tn, 1);
    }
  }

  // create backpatches for wind-up and wind-down
  FOR_ALL_BB_OPs(body, op) {

    if (OP_noop(op) || OP_xfer(op) || OP_loh(op))
      continue;

    int use_cycle = swp_op_vector[SWP_index(op)].cycle;
    int use_stage = use_cycle/ii;

    for (INT opnd = 0; opnd < OP_opnds(op); opnd++) {
      TN *tn = OP_opnd(op,opnd);
      if (TN_is_register(tn) &&
	  !TN_is_dedicated(tn) &&
	  !TN_is_const_reg(tn)) {
        OP *def_op = tn_def.Get(tn);
        if (def_op && !OP_loh(def_op)) {
	  int def_cycle = swp_op_vector[SWP_index(def_op)].cycle;
	  int def_stage = def_cycle/ii;
	  int omega = OP_omega(op,opnd);

	  // TN is defined in different stage
          if ( def_stage < use_stage+omega ) {

	    if (def_stage != stage_count-1 &&
		!CG_LOOP_Backpatch_Find_Non_Body_TN(head, tn, 1) )
	      CG_LOOP_Backpatch_Add(head, tn, tn, 1);
	    if (use_stage != 0 &&
		!CG_LOOP_Backpatch_Find_Non_Body_TN(tail, tn, 0) )
	      CG_LOOP_Backpatch_Add(tail, tn, tn, 0);
	  }
	}
      }
    }
  }
}

// Relax Deps temporarily remove the following kinds of dependences to
// reduce the recurrence in SWP in order to get smaller II
//
// case 1:
//	 	*(p++) <- x
// 		y <- *p		the dependence on p is removed
//				the increment in source can be part
//				of an updating op or addi
// case 2:
//
//		.. <- *p
//		  ..
//		*p <- ..
//		  ..
//		p++		the anti-dependence of p from earlier
//				load/store is removed
//				the increment can be part of
//				an updating op
//
// only the dependences that are patchable (e.g., no encoding problem)
// are removed.
//
// the code has to be patched after the scheduling if the final
// schedule violates the dependences (by Restore_Dependences())
//
static bool
Relax_Dependences(const SWP_OP_vector& swp_op_vector) {

  if (SWP_Options.Enable_Mem_Offset_Relaxation==false)
    return false;

  INT i;
  int marked = 0;
  for (i = 0; i < swp_op_vector.size(); i++) {
    OP *op = swp_op_vector[i].op;
    if (op && !OP_noop(op) && !OP_xfer(op) && !OP_loh(op)) {
      if ((OP_code(op)==TOP_addi ||
	   (OP_generic(op) && Is_Generic_Addi_Op(op))) ||
	  (OP_base_update(op) && !OP_unknown_addr(op))) {

	TN* base = OP_opnd(op,0);
	TN* offset = OP_opnd(op,1);

	if ((OP_code(op)==TOP_addi ||
	     (OP_generic(op) && Is_Generic_Addi_Op(op)))) {

	  if (OP_result(op,0)!=OP_opnd(op,0))
	    goto case_1_failed;

	  base = OP_opnd(op,0);
	  offset = OP_opnd(op,1);

	} else {

	  if (OP_unknown_addr(op))
	    goto case_1_failed;

	  INT8 base_idx = OP_find_opnd_use(op, OU_base);
	  INT8 offset_idx = OP_find_opnd_use(op, OU_offset);

	  if (base_idx == -1 || offset_idx == -1)
	    goto case_1_failed;

	  base = OP_opnd(op, base_idx);
	  offset = OP_opnd(op, offset_idx);

	}
        for ( ARC_LIST *al = OP_succs(op) ; al; al = ARC_LIST_rest(al) ) {
	  ARC *arc    = ARC_LIST_first(al);
	  OP  *succ = ARC_succ(arc);

	  // mark register flow dependences from updating load/store
	  // ignore them in modulo scheduling
	  // fix up later if schedule moves the sink above the source
	  INT8 succ_offset_idx = OP_find_opnd_use(succ, OU_offset);
	  if (ARC_kind(arc) == CG_DEP_REGIN && OP_memory(succ) &&
	      !OP_unknown_addr(succ) && !OP_base_update(succ) &&
	      succ_offset_idx != -1) {

	    // case 1.
	    TN* pred_base = base;
	    TN* pred_offset = offset;
	    INT8 succ_base_idx = OP_find_opnd_use(succ, OU_base);
	    TN* succ_base = OP_opnd(succ, succ_base_idx);
	    TN* succ_offset = OP_opnd(succ, succ_offset_idx);

	    if (ARC_opnd(arc)==succ_base_idx && pred_base == succ_base &&
	        TN_has_value(pred_offset) && TN_has_value(succ_offset)) {
	      INT i,k, pred_count, succ_count;

	      pred_count=0;
	      succ_count=0;

	      k = OP_results(op);
	      for (i=0; i<k; i++)
	        if (OP_result(op, i)==pred_base)
		  pred_count++;
	      k = OP_opnds(succ);
	      for (i=0; i<k; i++)
	        if (OP_opnd(succ, i)==succ_base)
		  succ_count++;

	      if (pred_count==1 && succ_count==1) {

	        INT j, def_count;
	        def_count=0;
	        for (j = 0; j < swp_op_vector.size(); j++) {
		  OP *scan = swp_op_vector[j].op;
		  if (scan && OP_Defs_TN(scan, pred_base)) {
		    def_count++;
		  }
	        }

	        INT adjust = 0;
	        INT64 new_offset = TN_value(succ_offset);
	        // allow moving across 4 iteration (also limited by omega field size)
	        for (j=0; j<4; j++) {
	          new_offset += TN_value(pred_offset);
		  if (TI_TOP_Can_Have_Immediate(new_offset, OP_code(succ)))
		    adjust++;
		  else
		    break;
	        }
	        INT old_omega = ARC_omega(arc);
	        if (def_count==1 && adjust>0 && CG_DEP_Replace_SCC_ARC_omega(arc,old_omega+adjust)) {
		  Set_ARC_is_marked(arc,1);
		  marked++;
		}
	      }
	    }
	  }
	}
      }
case_1_failed:

      // check for self increment/decrement
      if (OP_code(op)==TOP_addi ||
	  (OP_generic(op) && Is_Generic_Addi_Op(op)) ||
	  OP_base_update(op)) {

	// case 2.
	TN* base = OP_opnd(op,0);
	TN* offset = OP_opnd(op,1);
	INT def_count=0;

	if ((OP_code(op)==TOP_addi ||
	     (OP_generic(op) && Is_Generic_Addi_Op(op)))) {

	  if (OP_result(op,0)!=OP_opnd(op,0))
	    goto case_2_failed;

	  base = OP_opnd(op,0);
	  offset = OP_opnd(op,1);

	} else {

	  if (OP_unknown_addr(op))
	    goto case_2_failed;

	  INT8 base_idx = OP_find_opnd_use(op, OU_base);
	  INT8 offset_idx = OP_find_opnd_use(op, OU_offset);

	  if (base_idx == -1 || offset_idx == -1)
	    goto case_2_failed;

	  base = OP_opnd(op, base_idx);
	  offset = OP_opnd(op, offset_idx);

	}

	for (INT j = 0; j < swp_op_vector.size(); j++) {
	  OP *scan = swp_op_vector[j].op;
	  if (scan && OP_Defs_TN(scan, base)) {
	    def_count++;
	  }
	}

	if (def_count>1) goto case_2_failed;

        for ( ARC_LIST *al = OP_preds(op) ; al; al = ARC_LIST_rest(al) ) {
	  ARC *arc    = ARC_LIST_first(al);
	  OP  *pred = ARC_pred(arc);

	  // mark register anti dependences from load/store to base increment
	  // ignore them in modulo scheduling
	  // fix up later if schedule moves the sink above the source
	  INT8 pred_offset_idx = OP_find_opnd_use(pred, OU_offset);
	  INT8 pred_base_idx = OP_find_opnd_use(pred, OU_base);
	  if (ARC_kind(arc) == CG_DEP_REGANTI && ARC_omega(arc)==0 &&
	      OP_memory(pred) && !OP_unknown_addr(pred)		&&
	      pred_offset_idx != -1				&&
	      OP_opnd(pred,pred_base_idx)==base			&&
	      ARC_opnd(arc)==pred_base_idx			&&
	      TN_has_value(OP_opnd(pred, pred_offset_idx))) {

	    TN* pred_offset = OP_opnd(pred, pred_offset_idx);
	    TN* pred_base = OP_opnd(pred, pred_base_idx);

	    INT i,k, pred_count;

	    pred_count=0;
	    k = OP_opnds(pred);
	    for (i=0; i<k; i++)
	      if (OP_opnd(pred, i)==pred_base)
		pred_count++;

	    if (pred_count==1) {

	      INT adjust = 0;
	      INT64 new_offset = TN_value(pred_offset);
	      INT j;
	      // allow moving across 4 iteration (also limited by omega field size)
	      for (j=0; j<4; j++) {
	        new_offset -= (INT64)TN_value(offset);
		if (TI_TOP_Can_Have_Immediate(new_offset, OP_code(pred)))
		  adjust++;
		else
		  break;
	      }
	      INT old_omega = ARC_omega(arc);
	      if (adjust>0 && CG_DEP_Replace_SCC_ARC_omega(arc,old_omega+adjust)) {
		Set_ARC_is_marked(arc,1);
		marked++;
	      }
	    }
	  }
	}

case_2_failed:
	continue;
      }
    }
  }

  return marked>0;
}

// see comments on Relax_Dependences()

static void
Restore_Dependences(const SWP_OP_vector& swp_op_vector, bool schedule_ok) {

    if (SWP_Options.Enable_Mem_Offset_Relaxation==false)
      return;

    INT i;
    INT ii = swp_op_vector.ii;
    for (i = 0; i < swp_op_vector.size(); i++) {

      OP *op = swp_op_vector[i].op;
      if (op==NULL || OP_noop(op) || OP_xfer(op) || OP_loh(op))
	continue;

      ARC_LIST *al;
      for (al = OP_preds(op) ; al; al = ARC_LIST_rest(al) ) {
	ARC *arc = ARC_LIST_first(al);
	if (ARC_is_marked(arc)) {

	  Set_ARC_is_marked(arc, 0);
	  if (CG_DEP_Restore_SCC_ARC_omega(arc) && schedule_ok) {
	    if (ARC_kind(arc)==CG_DEP_REGIN) {
	      // case 1. in Relax_Dependences()
	      // need to adjust offset for loads moved across updating stores
	      // the predecessor is updating store and the current op is load
	      OP *pred = ARC_pred(arc);
	      INT pred_idx = SWP_index(pred);
	      INT pred_sched_cycle = swp_op_vector[pred_idx].cycle;
	      INT sched_cycle = swp_op_vector[SWP_index(op)].cycle;
	      INT adjust = 0;
	      while (sched_cycle<=pred_sched_cycle) {
	        sched_cycle += ii;
	        adjust++;
	      }
	      adjust -= ARC_omega(arc);
	      if (adjust>0) {
		UINT8 inc_offset_idx = 1;
		if ((OP_code(pred)==TOP_addi ||
		    (OP_generic(pred) && Is_Generic_Addi_Op(pred)))) {
		  inc_offset_idx = 1;	// for increment using addi
		} else {
		  inc_offset_idx = OP_find_opnd_use(pred, OU_offset);
		}
		UINT8 succ_offset_idx = OP_find_opnd_use(op, OU_offset);
		TN* pred_offset = OP_opnd(pred, inc_offset_idx);
		TN* succ_offset = OP_opnd(op, succ_offset_idx);

		INT64 new_offset = TN_value(succ_offset) + adjust * TN_value(pred_offset);
	        TN* new_offset_tn = Gen_Literal_TN(new_offset, TN_size(succ_offset));
	        Set_OP_opnd(op, succ_offset_idx, new_offset_tn);
	      }
	    } else if (ARC_kind(arc)==CG_DEP_REGANTI) {
	      // case 2. in Relax_Dependences()
	      // need to adjust offset for loads/stores moved across addi
	      // the predecessor is a load/store and the current op is addi
	      OP *pred = ARC_pred(arc);
	      INT pred_idx = SWP_index(pred);
	      INT pred_sched_cycle = swp_op_vector[pred_idx].cycle;
	      INT sched_cycle = swp_op_vector[SWP_index(op)].cycle;
	      INT adjust = 0;
	      while (sched_cycle<pred_sched_cycle) {
	        sched_cycle += ii;
	        adjust++;
	      }
	      adjust -= ARC_omega(arc);
	      if (adjust>0) {
		UINT8 pred_offset_idx = OP_find_opnd_use(pred, OU_offset);
		UINT8 inc_offset_idx = 1;
		if ((OP_code(op)==TOP_addi ||
		    (OP_generic(op) && Is_Generic_Addi_Op(op)))) {
		  inc_offset_idx = 1;	// for increment using addi
		} else {
		  inc_offset_idx = OP_find_opnd_use(op, OU_offset);
		}
		TN* pred_offset = OP_opnd(pred, pred_offset_idx);
		TN* inc_offset = OP_opnd(op, inc_offset_idx);

		INT64 new_offset = (INT64)TN_value(pred_offset) -
			(INT64)adjust * (INT64)TN_value(inc_offset);
	        TN* new_offset_tn = Gen_Literal_TN(new_offset, TN_size(pred_offset));
	        Set_OP_opnd(pred, pred_offset_idx, new_offset_tn);
	      }
	    }
	  }
	}
      }
    }
}

BOOL Perform_SWP(CG_LOOP& cl, SWP_FIXUP_VECTOR& fixup, INT best_ii,
		bool is_doloop)
{
  Set_Error_Phase("Software Pipelining");
  LOOP_DESCR *loop = cl.Loop();
  BB *body = LOOP_DESCR_loophead(loop);
  BB *head = CG_LOOP_prolog;
  BB *tail = CG_LOOP_epilog;
  Is_True(BB_SET_Size(LOOP_DESCR_bbset(loop)) == 1,("can't SWP multi-bb loops."));

  const bool trace = Get_Trace(TP_SWPIPE, 2);
  const bool trace_details = Get_Trace(TP_SWPIPE, 4);
  const bool trace_bundling = Get_Trace(TP_SWPIPE, 0x1000);
  const bool show_result = Get_Trace(TP_SWPIPE, 1);
  INT max_issue_alignment = 1;

  if (CG_skip_local_swp && Is_Loop_Skipped()) return FALSE;

  SWP_RETURN_CODE code = Detect_SWP_Constraints(cl, trace);

  if (code == SWP_LOOP_EMPTY) {

    // for empty loop body (after unrolling removal of identity copy)
    // add a nop to the body to allow modulo scheduling

    TOP ntop     = TI_TOP_Topcode("nop");
    if (ntop==TOP_UNDEFINED) {
      if (!xt_density)
	return SWP_Failure(body, code, ISA_REGCLASS_UNDEFINED);
      ntop = TOP_nop_n;
    }
    OP       *noop     = Mk_OP(ntop);
    CG_LOOP_Init_Op(noop);
    Set_OP_format_num(noop,-1);
    Set_OP_slot_num(noop,-1);
    BB_Prepend_Op(body, noop);
    Reset_OP_bundled(noop);
    code = SWP_OK;

  } else if (code != SWP_OK)
    return SWP_Failure(body, code, ISA_REGCLASS_UNDEFINED);

  INT op_issue_alignment;
  OP* op;
  FOR_ALL_BB_OPs(body,op)
  {
    CG_Relax_Special_Op(op);
    op_issue_alignment = TI_TOP_Issue_Alignment(OP_code(op));
    if (op_issue_alignment!=1)
      max_issue_alignment = op_issue_alignment;
  }

  if (trace)
    CG_LOOP_Trace_Loop(loop, "**** Before SWP ****");

  // SWP compile-time tuning parameters
  double max_ii_alpha = SWP_Options.Max_II_Alpha;
  double max_ii_beta  =  SWP_Options.Max_II_Beta;
  double ii_incr_alpha =  SWP_Options.II_Incr_Alpha;
  double ii_incr_beta =  1.0 + (SWP_Options.II_Incr_Beta - 1.0) / Max(1,SWP_Options.SWP_Opt_Level);
  INT sched_budget = SWP_Options.Budget * Max(1,SWP_Options.SWP_Opt_Level);

  {
    Start_Timer(T_SWpipe_CU);
    double time0 = Get_User_Time(T_SWpipe_CU);

    CXX_MEM_POOL swp_local_pool("swp pool", FALSE);
    SWP_OP_vector swp_op_vector(body, is_doloop, swp_local_pool());

    // Make sure we have enough non-rotating registers for the loop
    SWP_REG_ASSIGNMENT swp_assign (cl.Multireg_sc());
#if 0
    // the estimation is based on names and not by real allocation so
    // it is too pessimistic
    // skip this check for now
    if (!swp_assign.Enough_Non_Rotating_Registers(swp_op_vector.tn_non_rotating)) {
      // TODO: we might be able to convert some invariants into copy
      // and thus uses the rotating registers.
      return SWP_Failure(body, NON_ROT_REG_ALLOC_FAILED );
    }
#endif
    
    CG_LOOP_rec_min_ii = CG_LOOP_res_min_ii = CG_LOOP_min_ii = 0;

    // invokes CG_DEP_Compute_Graph, deconstructor deletes graph
    CYCLIC_DEP_GRAPH cyclic_graph( body, swp_local_pool()); 

    if (trace)
      CG_DEP_Trace_Graph(body);

#ifdef TARG_IA64
    // prune loop-carried REGOUT dependences on rotating registers
    Prune_Regout_Deps(body, swp_op_vector.tn_non_rotating);

    if (trace)
      CG_DEP_Trace_Graph(body);
#endif
    
    bool dep_changed = Relax_Dependences(swp_op_vector);

    {
      // Compute CG_LOOP_min_ii.
      MEM_POOL_Push(&MEM_local_pool);
      BOOL ignore_non_def_mem_deps = FALSE;
      CG_LOOP_Make_Strongly_Connected_Components(body, &MEM_local_pool, ignore_non_def_mem_deps); 
      CG_LOOP_Calculate_Min_Resource_II(body, NULL, FALSE /*include pref*/, TRUE /*ignore pref stride*/);
      CG_LOOP_Calculate_Min_Recurrence_II(body, ignore_non_def_mem_deps);

      CG_LOOP_Clear_SCCs(loop);
      MEM_POOL_Pop(&MEM_local_pool);
    }

    double time1 = (double)0.0;
    double time2 = (double)0.0;

    // Modulo Scheduling
    CG_LOOP_min_ii = Max(CG_LOOP_min_ii, SWP_Options.Starting_II);
    INT max_ii = (INT)linear_func(CG_LOOP_min_ii, max_ii_alpha, max_ii_beta);
    INT max_ii_from_inst =
	    (INT)linear_func(BB_length(body), max_ii_alpha, max_ii_beta);
    if (max_ii<max_ii_from_inst)
      max_ii = max_ii_from_inst;
    if (best_ii<max_ii)
      max_ii = best_ii;

    INT super_swp_ii = 0;	// ii used for exhaustive search
				// 0 means no exhaustive search
    bool is_super_swp = cl.Need_super_swp();

    if ( is_super_swp ) {
      super_swp_ii = cl.Super_swp_ii();
    }

    // update CG_LOOP_min_ii using MinDist
    MinDist mindist(swp_op_vector, swp_op_vector.start, swp_op_vector.stop,
		    CG_LOOP_min_ii);

    if (mindist.Found_ii() != CG_LOOP_min_ii) {
      DevWarn("CG_LOOP_min_ii (%d) is different from RecMII (%d) identified by MinDist.",
	      CG_LOOP_min_ii, mindist.Found_ii());
      CG_LOOP_min_ii = mindist.Found_ii();
    }

    INT current_ii = CG_LOOP_min_ii;

    if (super_swp_ii !=0) {
      max_ii = current_ii = super_swp_ii;
    }

    INT old_swp_op_size = swp_op_vector.size();

    // estimate how many registers are available for loop variants
    INT variant_regs[TI_ISA_REGCLASS_MAX+1];
    {
      ISA_REGCLASS rc;
      FOR_ALL_ISA_REGCLASS(rc) {
        variant_regs[rc] = REGISTER_SET_Size(swp_assign.non_rotating_reg[rc]);
      }

      // reserve 1 AR (integer) register for the loop instruction
      variant_regs[TI_ISA_Regclass_Integer()]--;

      TN_SET *invariants = swp_op_vector.tn_invariants;
      for (TN *tn = TN_SET_Choose(invariants);
	tn != TN_SET_CHOOSE_FAILURE;
	tn = TN_SET_Choose_Next(invariants,tn)) {

	if (!TN_is_dedicated(tn)) {
	  ISA_REGCLASS rc = TN_register_class(tn);
	  variant_regs[rc]--;
	}
      }

      FOR_ALL_ISA_REGCLASS(rc) {
	if (variant_regs[rc]<0)
	  variant_regs[rc]=0;
	if (swp_assign.Trace()) {
	  const ISA_REGCLASS_INFO *rc_info = TI_ISA_Regclass_Info(rc);
	  fprintf(TFile,
		"Reg class %10s has %3d available regs for variants\n",
		TI_ISA_Regclass_Name(rc_info), variant_regs[rc]);
	}
      }
    }

    WN* wn = cl.SWP_schedule_pragma_wn();
    INT specified_ii= 0;
    INT* specified = NULL;
    if (wn) {
      STR_IDX str_idx = WN_pragma_arg1(wn);
      const char* str = Index_To_Str(str_idx);
      const char* ii_str;
      if (!strncmp(str,"ii =",4)) {
	ii_str=str+5;
	str = strchr(str, '[');
	int num_ops = atoi(str+1);
	if (num_ops+2 == swp_op_vector.size()) {
	  specified_ii = atoi(ii_str);
	  specified = CXX_NEW_ARRAY(INT, num_ops, swp_local_pool());
	  str = strchr(str, '=')+1;
	  for (int i=0; i<num_ops; i++) {
	    specified[i] = atoi(str+1);
	    str = strchr(str+1, ' ');
	  }
	}
      }
    }

    INT32* ordered =
              CXX_NEW_ARRAY(INT32, swp_op_vector.size(), swp_local_pool());
    INT32** scheduling_order = CXX_NEW_ARRAY(INT32*, 3, swp_local_pool());
    for (int i=0; i<3; i++) scheduling_order[i] = NULL;

    int current_order = 0;
    // Scheduling Order: 0x1 -- original order, 0x2 -- linear scheduling order
    if (SWP_Options.Scheduling_Order & 0x1) {
      scheduling_order[current_order] = 
              CXX_NEW_ARRAY(INT32, swp_op_vector.size(), swp_local_pool());
      for (INT i=0; i<swp_op_vector.size()-2; i++) {
        scheduling_order[current_order][i] = i;
      }
      current_order++;
    }
    if ((SWP_Options.Scheduling_Order & 0x2) || current_order==0) {
      scheduling_order[current_order] = 
              CXX_NEW_ARRAY(INT32, swp_op_vector.size(), swp_local_pool());
      // declare the linear_schedule_sort() from cg_swp_sched.cxx
      // it was moved here to amortize the sorting overhead
      extern void linear_schedule_sort(
		const SWP_OP_vector& v, INT32 ordered[], bool trace,
		MEM_POOL* pool);

      // sorting that simulates the linear scheduling
      linear_schedule_sort(swp_op_vector, ordered,
                       trace, swp_local_pool());
      for (INT i=0; i<swp_op_vector.size()-2; i++) {
        scheduling_order[current_order][ordered[i]] = i;
      }
      current_order++;
    }

    while (1) {

      swp_assign.Clear();

      time1 = Get_User_Time(T_SWpipe_CU);

      // try modulo scheduling until either we schedule and register allocate
      // successfully and break (see below near the end of the while block)
      // or we return with scheduling failure

      if (specified_ii!=0)
        Modulo_Schedule_Super(swp_op_vector, swp_assign, head, tail,
		      specified_ii, specified_ii, ii_incr_alpha,
		      ii_incr_beta, max_issue_alignment, scheduling_order,
		      sched_budget, variant_regs,
		      specified_ii, specified,
		      trace, trace_details);
      else if (is_super_swp)
        Modulo_Schedule_Super(swp_op_vector, swp_assign, head, tail,
		      current_ii, max_ii, ii_incr_alpha,
		      ii_incr_beta, max_issue_alignment, scheduling_order,
		      sched_budget, variant_regs,
		      0, NULL,
		      trace, trace_details);
      else
        Modulo_Schedule(swp_op_vector, swp_assign, head, tail,
		      current_ii, max_ii, ii_incr_alpha,
		      ii_incr_beta, max_issue_alignment, scheduling_order,
		      sched_budget, variant_regs,
		      0, NULL,
		      trace, trace_details);

      if (!swp_op_vector.succeeded) {
	if (dep_changed)
	  Restore_Dependences(swp_op_vector, false);
        return SWP_Failure(body, MOD_SCHED_FAILED );
      }

      specified_ii = 0;

      // Bundling
      if (SWP_Options.Enable_Bundling)
        SWP_Bundle(swp_op_vector, trace_bundling);
      else
        SWP_Dont_Bundle(swp_op_vector);

      if (trace) {
        swp_op_vector.Print(TFile);
        swp_op_vector.Print_Modulo_Schedule(TFile);
      }

      time2 = Get_User_Time(T_SWpipe_CU);

      // Perform Register Allocation to rotating register banks.  The
      // resultant allocation will be in terms of a map from TNs to 
      // positive unbounded locations (swp_assign.reg_allocation), a 
      // lower bound on the number of rotating registers required for
      // the allocation (swp_assign.rotating_reg_used), and a location
      // assigned to swp_assign.control_predicate_loc.
      //
      ISA_REGCLASS rc = ISA_REGCLASS_UNDEFINED;
      bool ok = swp_assign.Allocate_Loop_Variants(swp_op_vector, head, tail, &rc) &&
		swp_assign.Invariants_Allocatable(swp_op_vector);

      if (ok) {
	  if (swp_op_map) {
	    // record the index of swp_op_vector for each op
	    // so it can be printed at emit time
	    // and then be used to specify swp schedule
	    // using #pragma swp_schedule to avoid long SWP scheduling
	    INT i = old_swp_op_size-1;
	    while (i>=0) {
	      OP* op = swp_op_vector[i].op;
	      if (op) {
		OP_MAP_Set(swp_op_map, op, (void*)(i+1)); // increment to avoid 0
	      }
	      i--;
	    }
	  }
	  break;
      }

      if (swp_assign.Trace()) {
	fprintf(TFile, "  Est. reg alloc fails, try larger II\n");
      }

      // try to enlarge II to reduce register pressure
      current_ii = swp_op_vector.ii + 1;
      SWP_Undo_Bundle(swp_op_vector, body);
      INT i = swp_op_vector.size();
      while (i>old_swp_op_size) {
	swp_op_vector.pop_back();
	i--;
      }
      i--;
      while (i>=0) {
	swp_op_vector[i].Clear();
	i--;
      }
    }

    SWP_Generate_Wind_Up_Wind_Down(cl, swp_op_vector,trace);

    // Restore_Dependences() has to be called after wind-up/down code
    // is generated
    if (dep_changed)
      Restore_Dependences(swp_op_vector, true);
#ifdef TARG_IA64
    // Reserve rotating registers to cover the ones needed for this loop.
    // Only integer registers can vary the size of the rotating segment,
    // so there is no need to do it for other register classes.
    //
    REGISTER_Reserve_Rotating_Registers(TI_ISA_Regclass_Integer(), 
		     swp_assign.rotating_reg_used[TI_ISA_Regclass_Integer()]);
#endif

    double time3 = Get_User_Time(T_SWpipe_CU);

    // Code Generation
    LOOPINFO *info = LOOP_DESCR_loopinfo(loop);
    TN *trip_count_tn = info ? LOOPINFO_trip_count_tn(info) : NULL;
    bool emit_ok = SWP_Emit(swp_op_vector, swp_assign, trip_count_tn, 
			    head, body, tail, is_doloop, trace);
    if (!emit_ok)
      return FALSE;

    fixup.push_back( SWP_FIXUP(head, body, tail,
			       swp_assign.control_predicate_loc) );

    double time4 = Get_User_Time(T_SWpipe_CU);

    if (trace)
      CG_LOOP_Trace_Loop(loop, "**** After SWP ****");

    // Generate Statistics
    swp_op_vector.prep_time = time1 - time0;
    swp_op_vector.sched_time = time2 - time1;
    swp_op_vector.reg_alloc_time = time3 - time2;
    swp_op_vector.code_gen_time = time4 - time3;
    if (show_result) 
      SWP_Show_Statistics(swp_op_vector, body);

    Stop_Timer(T_SWpipe_CU);
  }

  FOR_ALL_BB_OPs(body,op)
  {
    CG_Specialize_Op(op);
  }

  return TRUE;
}


/* ====================================================================
 *
 * Emit_SWP_Failure_Note
 *
 * Emit a single line swp failure note to trace file or .s file, ...
 *
 * ====================================================================
 */
void
Emit_SWP_Failure_Note(SWP_RETURN_CODE code, FILE *file, const char* prefix,
			const ISA_REGCLASS rc)
{
    char *failure_msg;
    switch (code) {
    case SWP_OK:
      failure_msg = "swp ok";
      break;
    case SWP_NOT_ATTEMPTED:
      failure_msg = "swp not attempted";
      break;
    case SWP_PREP_ONLY:
      failure_msg = "disable by -SWP:prep_only";
      break;
    case SWP_ASM:
      failure_msg = "unable to swp a loop containing ASM statements";
    case SWP_SIMULATED_OP:
      failure_msg = "unable to swp a loop containing simulated opcode";
      break;
    case SWP_MULTIREG_OP:
      failure_msg = "unable to swp a loop containing opcode with multi-register operand(s)";
      break;
    case SWP_PRIVILEGED_OP:
      failure_msg = "unable to swp a loop containing privileged opcode";
      break;
    case SWP_WORKAROUND:
      failure_msg = "disable swp to workaround hardware bugs";
      break;
    case SWP_DEDICATED_ROT_REG:
      failure_msg = "unable to swp a loop with dedicated rotating register binding";
      break;
    case MOD_SCHED_FAILED:
      failure_msg = "unable to find a modulo schedule";
      break;
    case MOD_NOT_BETTER:
      failure_msg = "modulo schedue found but is not better";
      break;
    case REG_ALLOC_FAILED:
#ifdef TARG_XTENSA
      if (rc != ISA_REGCLASS_UNDEFINED) {
        const char* rc_name = REGISTER_CLASS_name(rc);
        fprintf(file, "%s not enough register in regfile (%s)\n",
		      prefix, rc_name);
        return;
      }
      failure_msg = "not enough register";
#else
      failure_msg = "not enough rotating register";
#endif
      break;
    case NON_ROT_REG_ALLOC_FAILED:
#ifdef TARG_XTENSA
      failure_msg = "not enough register";
#else
      failure_msg = "not enough non-rotating register";
#endif
      break;
    case TOO_FEW_TRIPS:
      failure_msg = "too few trips";
      break;
    case SWP_LOOP_EMPTY:
      failure_msg = "loop is empty";
      break;
    case SWP_LOOP_LIMIT:
      failure_msg = "loop is too big";
      break;
    default:
      Is_True(FALSE, ("unknown SWP RETURN CODE."));
    }
    fprintf(file, "%s %s\n", prefix, failure_msg);
}

/* ====================================================================
 *
 * Emit_SWP_Note
 *
 * Emit a loop note to trace file or .s file, ...
 *
 * ====================================================================
 */
void
Emit_SWP_Note(BB *bb, FILE *file)
{
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_ROTATING_KERNEL);
  ROTATING_KERNEL_INFO *info = ANNOT_rotating_kernel(ant);

  if (ROTATING_KERNEL_INFO_succeeded(info)) {
    const char *prefix = "#<swps> ";
    fprintf(file, "%s\n", prefix);

    if (BB_unrollings(bb)==0)
      fprintf(file,
	"%s%3d cycles per pipeline stage in steady state with unroll=1\n",
	prefix, ROTATING_KERNEL_INFO_ii(info));
    else
      fprintf(file,
	"%s%3d cycles per pipeline stage in steady state with unroll=%d\n",
	prefix, ROTATING_KERNEL_INFO_ii(info), BB_unrollings(bb));

    fprintf(file, "%s%3d pipeline stages\n", 
	    prefix, ROTATING_KERNEL_INFO_stage_count(info));
    fprintf(file, "%s%3d real ops (excluding nop)\n", 
	    prefix, ROTATING_KERNEL_INFO_real_op_count(info));
    fprintf(file, "%s\n", prefix);

    prefix = "#<swps>      ";

    fprintf(file, "%smin %d cycles required by resources\n",
	    prefix, ROTATING_KERNEL_INFO_res_min_ii(info));
    fprintf(file, "%smin %d cycles required by recurrences\n",
	    prefix, ROTATING_KERNEL_INFO_rec_min_ii(info));
    fprintf(file, "%smin %d cycles required by resources/recurrence\n", 
	    prefix, ROTATING_KERNEL_INFO_min_ii(info)); 
    fprintf(file, "%smin %d cycles required for critical path\n",
	    prefix, ROTATING_KERNEL_INFO_min_sched_len(info));
    fprintf(file, "%s%5d cycles non-loop schedule length\n",
	    prefix, ROTATING_KERNEL_INFO_sched_len(info));

    // if -SWP:Op_Info=true then print the
    // #pragma swp_schedule
    if (swp_op_map) {

      ANNOTATION *annot = ANNOT_Get(BB_annotations(bb), ANNOT_LOOPINFO);
      if (annot) {
	LOOPINFO *loopinfo = ANNOT_loopinfo(annot);
	if (loopinfo) {

	  int line_no = LOOPINFO_line(loopinfo);

	  ST* pu_st = Get_Current_PU_ST();
	  if (pu_st) {

	    const char *prefix = "#<swps> ";
	    const char* pu_name = ST_name(pu_st);
	    fprintf(file, "%s\n", prefix);
	    fprintf(file, "%suse the following pragma to reproduce the same "
			  "schedule\n", prefix);
	    fprintf(file, "%s#pragma swp_schedule ", prefix);
	    fprintf(file, "ii=%d, unroll=%d, sched[%d]= ",
		ROTATING_KERNEL_INFO_ii(info),
		BB_unrollings(bb)==0?1:BB_unrollings(bb),
		ROTATING_KERNEL_INFO_real_op_count(info));

	    INT bb_length = BB_length(bb);
	    INT cycles[bb_length];
	    INT num_ops = 0;
	    INT id;
	    OP* op;
	    FOR_ALL_BB_OPs_FWD(bb,op) {
	      if ((id=(INT)(OP_MAP_Get(swp_op_map,op)))!=0) {
		cycles[id-1] = OP_scycle(op);
		num_ops++;
	      }
	    }
	    for (INT i=0; i<num_ops; i++) {
	      fprintf(file, "%d ", cycles[i]);
	    }
	    fprintf(file, "\n");
	  }
	}
      }
    }

    fprintf(file, "%s\n", prefix);
  } else {
    const char *prefix = "#<swpf> ";
    fprintf(file, "%s\n", prefix);
    Emit_SWP_Failure_Note((SWP_RETURN_CODE)ROTATING_KERNEL_INFO_failure_code(info), file, prefix, ROTATING_KERNEL_INFO_unallocatable_rc(info));
    fprintf(file, "%s\n", prefix);
  }
}
