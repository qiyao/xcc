
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cgtarget.cxx
 * $Revision: 1.134 $
 * $Date: 2000/04/28 21:58:34 $
 * $Author: srinivas $
 * $Source: /osprey.src/osprey1.0/be/cg/ia64/RCS/cgtarget.cxx,v $
 *
 * Description:
 *
 * Support routines for target-specific code generator functionality.
 *
 * ====================================================================
 * ====================================================================
 */

#include <ctype.h>

#include "defs.h"
#include "util.h"
#include "config.h"
#include "config_targ_options.h"
#include "erglob.h"
#include "tracing.h"
#include "data_layout.h"
#include "const.h"
#include "wn.h"
#include "import.h"
#include "opt_alias_interface.h"
#include "opt_alias_mgr.h"
#include "cgir.h"
#include "cg.h"
#include "void_list.h"
#include "cg_dep_graph.h"
#include "cg_spill.h"
#include "cg_vector.h"
#include "whirl2ops.h"
#include "ti_errors.h"
#include "ti_latency.h"
#include "w2op.h"
#include "cgexp.h"
#include "cg_loop_recur.h"
#include "ti_bundle.h"
#include "hb_sched.h"
#include "hb_hazards.h"
#include "bb.h"
#include "op.h"
#include "op_list.h"
#include "cg_grouping.h"

#include "cgtarget.h"
#include "iselector.h"

UINT32 CGTARG_branch_taken_penalty;
BOOL CGTARG_branch_taken_penalty_overridden = FALSE;

OPCODE *CGTARG_Assoc_Base_Opr_Table;
mTOP *CGTARG_Assoc_Base_Top_Table;
mTOP *CGTARG_Assoc_Base_Fnc_Table;

mTOP CGTARG_Inter_RegClass_Copy_Table[TI_ISA_REGCLASS_MAX+1][TI_ISA_REGCLASS_MAX+1][2];

/* Trace flags: */
BOOL Trace_TD = FALSE;	/* Target-dependent prep trace */
BOOL Trace_Eager = FALSE; /* gcm used to set this... */
extern BOOL Trace_Call_Exp;	/* Trace call expansion, from cgexp */

UINT32
CGTARG_Mem_Ref_Bytes(const OP *memop)
/* -----------------------------------------------------------------------
 * Requires: OP_load(memop) || OP_store(memop)
 * See interface description.
 * -----------------------------------------------------------------------
 */
{
  FmtAssert(OP_load(memop) || OP_store(memop), ("not a load or store"));

  UINT32 size = TI_ISA_Mem_Ref_Bytes(OP_code(memop));
  FmtAssert(size != 0, ("unrecognized op (%s) in CGTARG_Mem_Ref_Bytes",
			TI_TOP_Name(OP_code(memop))));
  return size;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative(OP *op)
{
  if (!OP_load(op))
    return FALSE;

  // speculative and advanced loads are safe to speculate.
  if (CGTARG_Is_OP_Advanced_Load(op) || CGTARG_Is_OP_Speculative_Load(op))
    return TRUE;

  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Perform_THR_Code_Generation
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Perform_THR_Code_Generation (OP *load_op, OP *chk_load,
					 THR_TYPE type)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_ARC_Sched_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_ARC_Sched_Latency(
  ARC *arc
)
{
  if ( ARC_kind(arc) == CG_DEP_PREBR
       && TI_PROC_Property_Set(PROP_has_same_cycle_branch_shadow) )
    return 0;
  else
    return ARC_latency(arc);
}



static ISA_EXEC_UNIT_PROPERTY
clear_one_unit(ISA_EXEC_UNIT_PROPERTY set, ISA_EXEC_UNIT_PROPERTY mask)
{
  for (int i=0; i<TI_ISA_Exec_Num_Units(); i++) {
    ISA_EXEC_UNIT_PROPERTY bit = (1<<i);
    if ((bit & mask & set) != 0)
      return set & ~bit;
  }
  return set;
}

BOOL Convert_TNs_To_INT(OP *op, INT *tn_num_array)
{
  INT num_tns = OP_results(op)+OP_opnds(op);
  for (INT i=0; i<num_tns; i++) {
    TN *curr_tn;
    if (i >= OP_opnds(op)) {
      curr_tn = OP_result(op, i-OP_opnds(op));
    } else {
      curr_tn = OP_opnd(op, i);
    }
    if (TN_is_register(curr_tn))
      tn_num_array[i] = TN_number(curr_tn);
    else if (TN_has_value(curr_tn))
      tn_num_array[i] = TN_value(curr_tn);
    else if (TN_is_symbol(curr_tn)) {
      ST *base_st;
      INT64 base_ofst;

      Base_Symbol_And_Offset(TN_var(curr_tn), &base_st, &base_ofst);
      if ((base_st == SP_Sym) || (base_st == FP_Sym))
      {
	tn_num_array[i] = base_ofst + TN_offset(curr_tn);
      } else
	return FALSE;
    } else
      return FALSE;
  }
  return TRUE;
}

static ISA_EXEC_UNIT_PROPERTY Equivalent_OP_Slots(OP *op)
{
  ISA_EXEC_UNIT_PROPERTY opc_mask = TI_ISA_Exec_Unit_Prop(OP_code(op));
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TN *tn_array[array_size];

  if (Convert_TNs_To_INT(op, tn_num_array)) {
    TOP new_topcode = TI_Convert_OP(OP_code(op), (ISA_EXEC_UNIT_PROPERTY)-1,
				    tn_num_array);
    if (new_topcode != TOP_UNDEFINED)
      opc_mask |= TI_ISA_Exec_Unit_Prop(new_topcode);
  }
  return opc_mask;
}

static OP *Match_OP_To_Slot(OP *op, INT format_num, INT slot_num)
{
  INT array_size = TI_ISA_Operand_Max()+TI_ISA_Result_Max();
  INT tn_num_array[array_size];
  TN *tn_array[array_size];
  if (!Convert_TNs_To_INT(op, tn_num_array))
    return NULL;

  TOP new_opcode = 
    TI_Convert_OP(OP_code(op), TI_ISA_Exec_Slot_Prop(format_num, slot_num),
		      tn_num_array);
  if (new_opcode != TOP_UNDEFINED) {
    OP *new_op = Mk_OP(new_opcode, NULL, NULL, NULL, NULL, NULL, NULL);
    const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(new_opcode);
    for (INT i=0; i<OP_results(new_op); i++) {
      const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Result(opinfo, i);
      TN *new_tn;
      if (TI_ISA_Valtyp_Is_Register(vtype)) {
	new_tn = TNvec(tn_num_array[i+OP_opnds(new_op)]);
      } else {
	new_tn = Gen_Literal_TN(tn_num_array[i+OP_opnds(new_op)],
				TI_ISA_Valtyp_Size(vtype));
      }
      Set_OP_result(new_op, i, new_tn);
    }
    for (INT i=0; i<OP_opnds(new_op); i++) {
      const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, i);
      TN *new_tn;
      if (TI_ISA_Valtyp_Is_Register(vtype)) {
	new_tn = TNvec(tn_num_array[i]);
      } else {
	new_tn = Gen_Literal_TN(tn_num_array[i], TI_ISA_Valtyp_Size(vtype));
      }
      Set_OP_opnd(new_op, i, new_tn);
    }
    return new_op;
  } 

  // Do special checking for TOP_mov_n
  if (OP_code(op) == TOP_mov_n) {
    if (!TI_TOP_Is_Legal_In_Format_Slot (format_num, 
                   slot_num, TOP_mov_n)) {
      if (TI_TOP_Is_Legal_In_Format_Slot (format_num,
                     slot_num, TOP_or)) {
        OP *new_op = Mk_OP(TOP_or, OP_result(op, 0),
                      OP_opnd(op, 0), OP_opnd(op, 0));
        return new_op;
      }
    }
  }

  return NULL;
}

static bool pick_slots(int index, int max_index, int format, int *mapping, 
		       ISA_EXEC_UNIT_PROPERTY filled_slots, 
		       ISA_EXEC_UNIT_PROPERTY *valid_slots)
{
  for (int i=0; i<TI_ISA_Num_Slots(format); i++) {
    if (valid_slots[index] & ~filled_slots & TI_ISA_Exec_Slot_Prop(format,i)) {
      mapping[index] = i;
      if (index+1 == max_index)
	return true;
      else if (pick_slots(index+1, max_index, format, mapping,
			  filled_slots | TI_ISA_Exec_Slot_Prop(format,i),
			  valid_slots))
	return true;
    }
  }
  return false;
}

static void
Pick_Better_Bundle_Format(TI_BUNDLE              *bundle,
			  OP                     *op,
			  BOOL                    stop_bit_reqd)
{
  int best_format = -1;
  int best_mapping[ISA_BUNDLE_MAX_SLOTS];
  int i,j;
  
  // This iterates over every possible slot mapping for each bundle.
  // If this is too slow for large bundles, try the maximum bipartite 
  // matching algorithm.
  for (i=0; i<TI_ISA_Num_Bundles(); i++) {
    ISA_EXEC_UNIT_PROPERTY format_mask = 0;
    ISA_EXEC_UNIT_PROPERTY opc_mask;
    for (j=0; j<TI_ISA_Num_Slots(i); j++) 
      format_mask |= TI_ISA_Exec_Slot_Prop(i,j);
    int slot_mapping[ISA_BUNDLE_MAX_SLOTS];
    ISA_EXEC_UNIT_PROPERTY valid_slots[ISA_BUNDLE_MAX_SLOTS];
    opc_mask = Equivalent_OP_Slots(op);
    valid_slots[0] = format_mask & opc_mask;
    BOOL good_format = (valid_slots[0] != 0);
    int index = 1;
    FOR_ALL_SLOT_MEMBERS(bundle, j) {
      if (TI_BUNDLE_slot_filled(bundle, j)) {
	valid_slots[index] = format_mask & TI_BUNDLE_exec_property(bundle, j);
	if (valid_slots[index] == 0)
	  good_format = false;
	index++;
      }
    }
    if (good_format)
      good_format = pick_slots(0, index, i, slot_mapping, 0, valid_slots);
    if (good_format && 
	((best_format == -1) || 
	 (TI_ISA_Bundle_Bytes(i) < TI_ISA_Bundle_Bytes(best_format)))) {
      best_format = i;
      for (j=0; j<index; j++) {
	best_mapping[j] = slot_mapping[j];
      }
    }
  }

  /* Found a better format. Try to repack the current bundle */
  if (best_format != -1) {
    ISA_EXEC_UNIT_PROPERTY prop_array[ISA_BUNDLE_MAX_SLOTS];
    int index = 1;
    FOR_ALL_SLOT_MEMBERS(bundle, j) {
      if (TI_BUNDLE_slot_filled(bundle, j)) {
	prop_array[index] = TI_BUNDLE_exec_property(bundle, j);
	index++;
      }
    }
    TI_BUNDLE_Clear(bundle);
    Set_TI_BUNDLE_pack_code(bundle, best_format);
    for (j=1; j<index; j++) {
      TI_BUNDLE_Reserve_Slot(bundle, best_mapping[j], prop_array[j]);
    }
  }
}

/* ====================================================================
 *
 * CGTARG_Bundle_Slot_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Slot_Available(TI_BUNDLE              *bundle,
			     OP                     *op,
			     INT                     slot,
			     ISA_EXEC_UNIT_PROPERTY *prop, 
			     BOOL                    stop_bit_reqd,
			     const CG_GROUPING      *grouping)
{
  INT ti_err = TI_RC_OKAY;

  // Can only schedule independent instructions in the same bundle
  if (!TI_BUNDLE_Is_Empty(bundle, &ti_err)) {
    if (OP_side_effects(op))
      return FALSE;
    if (stop_bit_reqd)
      return FALSE;
    if ((OP_scycle(op) != -1) && (OP_scycle(Last_Real_OP(op))!=OP_scycle(op)))
      return FALSE;
    if (OP_simulated(op) || OP_simulated(Last_Real_OP(op)))
      return FALSE;
  }

  if (!TI_BUNDLE_Is_Empty(bundle, &ti_err) && (slot == 0))
    Pick_Better_Bundle_Format(bundle, op, stop_bit_reqd);

  // If slot already filled, return FALSE.
  if (TI_BUNDLE_slot_filled(bundle, slot)) return FALSE;

  *prop = Equivalent_OP_Slots(op);
  return TI_BUNDLE_Slot_Available (bundle, *prop, slot);
}

/* ====================================================================
 *
 * CGTARG_Bundle_Stop_Bit_Available
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL 
CGTARG_Bundle_Stop_Bit_Available(TI_BUNDLE *bundle, INT slot)
{
  /* Return TRUE the stop-bit is already set. */

  if (TI_BUNDLE_stop_bit(bundle, slot))
    return TRUE;

  return TI_BUNDLE_Stop_Bit_Available(bundle, slot);
}

/* ====================================================================
 *
 * CGTARG_Handle_Bundle_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Bundle_Hazard (OP                          *op, 
			     TI_BUNDLE                   *bundle, 
			     VECTOR                      *bundle_vector,
			     BOOL                        can_fill, 
			     INT                         slot_pos, 
			     INT                         max_pos,
			     BOOL                        stop_bit_reqd,
			     ISA_EXEC_UNIT_PROPERTY      prop) 
{
  INT ti_err = TI_RC_OKAY;
  INT i,j;

  OP* slot_op[TI_TIE_SLOTS_MAX];    /* op assignment for each slot */
  INT32 op_slot[TI_TIE_SLOTS_MAX];  /* slot assignment for op in bundle_vector */
  static bool has_previous_shared_functional_unit_usage = false;

  /* saved op assignment for each slot from last usage of
   * shared functional unit*/
  static OP* last_slot_op[TI_TIE_SLOTS_MAX];

  // if <bundle> is empty, no need to do anything. just check to see if
  // a stop_bit is required.
  if (TI_BUNDLE_Is_Empty(bundle, &ti_err)) {
    OP *last_real_op = Last_Real_OP(op);
    FmtAssert(ti_err != TI_RC_ERROR, ("%s", TI_errmsg));
    if (stop_bit_reqd) Set_OP_end_group(last_real_op);
    return;
  }

  // reset at beginning of a basic block
  if (Last_Real_OP(op)==NULL)
    has_previous_shared_functional_unit_usage=false;

  INT template_bit = TI_BUNDLE_Return_Template(bundle);

  FmtAssert (template_bit != -1, ("Illegal template encoding"));

  // Process simulated ops first.
  if (OP_simulated(op)) {
    TOP adjust_top = CGTARG_Simulated_Top(op, prop);
    OP_Change_Opcode(op, adjust_top);
  }

  // Because we have variable length bundles, we're never sure when a bundle 
  // is "full". So assume that a bundle is "full" when no more slots are 
  // available in the current bundle and Check_For_Bundle_Hazards() is 
  // trying to fill the bundle with ISA_BUNDLE_MAX_SLOTS nops.
  if (max_pos != ISA_BUNDLE_MAX_SLOTS) {
    /* Can't bundle with instructions with side effects (i.e. entry) */
    if (can_fill && OP_side_effects(op)) {
      Set_OP_end_group(op);
      Set_OP_format_num(op, 0);
      Set_OP_slot_num(op, 0);
      HB_Hazard_current_cycle++;
      VECTOR_Reset(*bundle_vector);
      TI_BUNDLE_Clear (bundle);
    }
  } else {
    TOP nop;
    INT index = 0;
    OP *prev_op = (can_fill ? op : Last_Real_OP(op));
    OP *last_sched_op = prev_op;
    BB* op_bb = OP_bb(prev_op);
    OP* xfer_op = NULL; // remember any branch encountered

    for (j=0; j<VECTOR_count(*bundle_vector); j++) {
      OP* bundle_op = (OP*)VECTOR_element(*bundle_vector, j);
      Set_OP_format_num(bundle_op, template_bit);
      Set_OP_slot_num(bundle_op, ISA_BUNDLE_MAX_SLOTS);
    }

    FOR_ALL_SLOT_MEMBERS(bundle, i) {
      if (i >= TI_ISA_Num_Slots(template_bit)) break;
      if (!TI_BUNDLE_slot_filled(bundle, i)) {
	nop = CGTARG_Noop_Top (TI_ISA_Exec_Slot_Prop(template_bit, i));
	  
	OP *noop = Mk_OP (nop);
	OP_scycle(noop) = -1;
	if (can_fill)
	  BB_Insert_Op_After(op_bb, prev_op, noop);
	else
	  BB_Insert_Op_Before(op_bb, op, noop);
	prev_op = noop;
	Set_OP_bundled (noop);
	Set_OP_format_num(noop, template_bit);
	Set_OP_slot_num(noop, i);
	VECTOR_Add_Element (*bundle_vector, noop);
	TI_BUNDLE_Reserve_Slot (bundle, i, 
				TI_ISA_Exec_Slot_Prop(template_bit, i));
      } else {
	for (j=0; j<VECTOR_count(*bundle_vector); j++) {
	  OP* bundle_op = (OP*)VECTOR_element(*bundle_vector, j);
	  ISA_EXEC_UNIT_PROPERTY opc_mask;
	  opc_mask = Equivalent_OP_Slots(bundle_op);
	  if ((OP_slot_num(bundle_op) == ISA_BUNDLE_MAX_SLOTS) &&
	      (opc_mask == TI_BUNDLE_exec_property(bundle, i))) {
	    if ((TI_ISA_Exec_Unit_Prop(OP_code(bundle_op)) & 
		 TI_ISA_Exec_Slot_Prop(template_bit, i)) == 0) {
	      OP *new_op = Match_OP_To_Slot(bundle_op, template_bit, i);
	      OP_srcpos(new_op) = OP_srcpos(bundle_op);
	      FmtAssert((new_op != NULL), 
			("Cannot find alternate version of OP for bundling"));
	      FmtAssert(((TI_ISA_Exec_Unit_Prop(OP_code(new_op)) & 
			  TI_ISA_Exec_Slot_Prop(template_bit, i)) != 0),
			("Alternate version of OP does not fit in bundle"));
	      Set_OP_bundled (new_op);
	      Set_OP_format_num(new_op, template_bit);
	      OP_scycle(new_op) = OP_scycle(bundle_op);
	      if (prev_op == bundle_op)
		prev_op = new_op;
	      VECTOR_Add_Element (*bundle_vector, new_op);
	      VECTOR_Delete_Element (*bundle_vector, bundle_op);
	      BB_Insert_Op_After(op_bb, bundle_op, new_op);
	      BB_Remove_Op(op_bb, bundle_op);
	      bundle_op = new_op;
	    }
	    if (OP_xfer(bundle_op))
	      xfer_op = bundle_op;
	    Set_OP_slot_num(bundle_op, i);
	    last_sched_op = bundle_op;
	    break;
	  }
	}
      }
    }

    // if the <bundle> is full, set the <end_group> marker appropriately.
    // Note that we only allow one bundle per cycle
    if (prev_op != NULL) {
      if (xfer_op && xfer_op != prev_op) {
	OP* scan = prev_op;
	while (scan && scan!=xfer_op) {
	  Is_True(OP_noop(scan), ("Non no-op filled after branch/call"));
	  scan = OP_prev(scan);
	}
	Is_True(scan, ("Missing branch/call"));
	if (scan) {
	  BB_Remove_Op(op_bb, xfer_op);
	  BB_Insert_Op_After(op_bb, prev_op, xfer_op);
	  prev_op = xfer_op;
	}
      }
      Set_OP_end_group(prev_op);
    }

    HB_Hazard_current_cycle++;
    if (op && last_sched_op && OP_scycle(op)>=0 && OP_scycle(last_sched_op)>=0) {
      int cycles = OP_scycle(op) - OP_scycle(last_sched_op);
      if (cycles>0)
        HB_Hazard_current_cycle += (cycles-1);
    }

    // last chance to change slotting

    bool use_shared_functional_unit = false;
    for (i=0; i<VECTOR_count(*bundle_vector); i++) {
      OP* op1 = (OP*)VECTOR_element(*bundle_vector,i);
      if (TI_TOP_Use_Shared_Functional_Unit(OP_code(op1))) {
	use_shared_functional_unit = true;
        break;
      }
    }

    if (use_shared_functional_unit)
    {

	INT fmt = TI_BUNDLE_Return_Template(bundle);
	INT num_ops = VECTOR_count(*bundle_vector);

	// find slot assignment using format 'fmt'
	INT num_slots = TI_ISA_Num_Slots(fmt);
	for (i=0; i<num_slots; i++) {
	  slot_op[i] = NULL;
	  op_slot[i] = -1;
	}

	INT32 num_possible_slotting = 0;
	OP* slotting[8][TI_TIE_SLOTS_MAX];

	// the following loop will backtrack and exhaustively search all
	// possible slot available for each operation
	for (i=0; i<num_ops; i++) {
	  INT32 j = op_slot[i] + 1;
	  while (j<num_slots) {
	    if (slot_op[j] == NULL) {
	      OP* opi = (OP*)VECTOR_element(*bundle_vector,i);
	      TOP opc = OP_code(opi);
	      ISA_EXEC_UNIT_PROPERTY opc_prop = TI_ISA_Exec_Unit_Prop(opc);
	      ISA_EXEC_UNIT_PROPERTY slot_prop = TI_ISA_Exec_Slot_Prop(fmt,j);
	      if ((opc_prop & slot_prop) !=0) {
	        slot_op[j] = opi;
	        op_slot[i] = j;
	        break;
	      }
	    }
	    j++;
	  }
	  if (j==num_slots) {
	    if (i!=0) {
	      op_slot[i] = -1;
	      slot_op[op_slot[i-1]] = NULL;
	      i = i-2;
	    } else
	      break;	// got all possible slotting
	  } else if (i==num_ops-1) {
	    // found a slotting
	    for (int s=0; s<num_slots; s++) {
	      slotting[num_possible_slotting][s] = slot_op[s];
	    }
	    num_possible_slotting++;
	    // collect at most 8 slottings
	    if (num_possible_slotting<8) {
	      slot_op[op_slot[i]] = NULL;
	      i--;
	    }
	  }
	}

	FmtAssert(num_possible_slotting>0,("Bundling fails"));

	int best_slotting = 0;
	if (has_previous_shared_functional_unit_usage &&
	    num_possible_slotting>1) {
	  // select best slotting
	  int best_num_conflicts = 999;
	  for (int k=0; k<num_possible_slotting; k++) {
	    int num_conflicts = 0;
	    for (int s=0; s<num_slots; s++) {
	      if (last_slot_op[s]== NULL)
		continue;

	      OP* op_last = last_slot_op[s];
	      OP* op_current = slotting[k][s];
	      if (TI_TOP_Use_Shared_Functional_Unit(OP_code(op_last)) &&
		  TI_TOP_Use_Shared_Functional_Unit(OP_code(op_current)))
		num_conflicts++;
	    }
	    if (num_conflicts<best_num_conflicts) {
	      best_slotting = k;
	      best_num_conflicts = num_conflicts;
	    }
	  }

	}

	int s;
	for (s=0; s<num_slots; s++) {
	  Set_OP_slot_num(slotting[best_slotting][s],s);
	  last_slot_op[s] = slotting[best_slotting][s];
	}
	for (; s<TI_TIE_SLOTS_MAX; s++)
	  last_slot_op[s] = NULL;

	has_previous_shared_functional_unit_usage=true;
    }

			    
    VECTOR_Reset (*bundle_vector);
  }
}

/* ====================================================================
 *
 * CGTARG_Handle_Errata_Hazard
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Handle_Errata_Hazard (OP *op, INT erratnum, INT ops_to_check)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * Reduce_Fraction
 *
 * Half hearted attempt to reduce a fraction. If we don't succeed
 * the only problem will be that we might round incorrectly on a
 * instruction rate.
 *
 * The algorithm is to first try the denominator as a factor and
 * then a few small primes.
 *
 * ====================================================================
 */
static void
Reduce_Fraction(INT frac[2])
{
  INT i;
  static const INT primes[] = {2, 3, 5, 7, 11, 13};
  INT n = frac[0];
  INT d = frac[1];
  INT p = d;

  if (d < -1 || d > 1) {
    for (i = sizeof(primes) / sizeof(primes[0]); ; p = primes[--i]) {
      while (n % p == 0 && d % p == 0) {
	n = n / p;
	d = d / p;
      }
      if (i == 0) break;
    }
  }

  frac[0] = n;
  frac[1] = d;
}


/* ====================================================================
 *
 * Harmonic_Mean
 *
 * Compute the harmonic weighted mean of two rates as follows:
 *
 *	  1        a                    b
 *	---- = ( ----- * a_rate ) + ( ----- * b_rate )
 *	mean     a + b                a + b
 *
 * Where:
 *
 *	"a" is the number of operations of class "a"
 *	"b" is the number of operations of class "b"
 *
 * ====================================================================
 */
static void
Harmonic_Mean(
  INT mean[2],
  INT a,
  const INT a_rate[2],
  INT b,
  const INT b_rate[2]
) {
  if (a == 0) {
    mean[0] = b_rate[0];
    mean[1] = b_rate[1];
  } else if (b == 0) {
    mean[0] = a_rate[0];
    mean[1] = a_rate[1];
  } else {
    mean[1] =   (a * a_rate[1] * b_rate[0]) 
	      + (b * b_rate[1] * a_rate[0]);
    mean[0] = (a + b) * a_rate[0] * b_rate[0];
    Reduce_Fraction(mean);
  }
}


/* ====================================================================
 *
 * CGTARG_Peak_Rate
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Peak_Rate( PEAK_RATE_CLASS prc, PRC_INFO *info, INT ratio[2] )
{
  ratio[0] = 1;
  ratio[1] = 1;
  
  switch (prc) {
  case PRC_INST:
    ratio[0] = 1;
    break;
  case PRC_MADD:
  case PRC_MEMREF:
    ratio[0] = 1;
    break;
  case PRC_FLOP:
  case PRC_FADD:
  case PRC_FMUL:
    ratio[0] = 1;
    break;
  case PRC_IOP:
    ratio[0] = 1;
    break;
  default:
    ratio[0] = 1;
    break;
  }
}

/* =======================================================================
 *
 *  Plural
 *
 *  Return "s" if i != 1, "" otherwise.  Used to get the number of nouns
 *  right when printing.
 *
 * =======================================================================
 */
#define Plural(i) ((i) != 1 ? "s" : "")


/* =======================================================================
 *
 *  Percent_Of_Peak
 *
 *  Compute the percentage of peak instructions executed. Both the
 *  actual number of instructions executed and the peak attainable
 *  are expressed as a fraction of insts/cycle.
 *
 * =======================================================================
 */
static INT
Percent_Of_Peak(INT numer, INT denom, INT peak[2])
{
  if (numer == 0) return 0;
  return (numer * peak[1] * 100) / ((denom * peak[0]) + peak[1] - 1);
}


/* =======================================================================
 *
 *  CGTARG_Print_PRC_INFO
 *
 *  Print statistics for the PRC_INFO to a 'file'.
 *
 * =======================================================================
 */
void
CGTARG_Print_PRC_INFO(
  FILE       *file,
  PRC_INFO   *info,
  INT32      ii,
  const char *prefix,
  const char *suffix
)
{
  char *s;
  INT madds_per_cycle[2];
  INT memrefs_per_cycle[2];
  INT flops_per_cycle[2];
  INT fadds_per_cycle[2];
  INT fmuls_per_cycle[2];
  INT iops_per_cycle[2];
  INT insts_per_cycle[2];
  INT insts = info->refs[PRC_INST];
  INT memrefs = info->refs[PRC_MEMREF];
  INT flops = info->refs[PRC_FLOP];
  INT madds = info->refs[PRC_MADD];
  INT fadds = info->refs[PRC_FADD];
  INT fmuls = info->refs[PRC_FMUL];
  INT iops = info->refs[PRC_IOP];

  if (ii==0)
    return;

  CGTARG_Peak_Rate(PRC_INST, info, insts_per_cycle);
  CGTARG_Peak_Rate(PRC_MEMREF, info, memrefs_per_cycle);
  CGTARG_Peak_Rate(PRC_FLOP, info, flops_per_cycle);
  CGTARG_Peak_Rate(PRC_MADD, info, madds_per_cycle);
  CGTARG_Peak_Rate(PRC_FADD, info, fadds_per_cycle);
  CGTARG_Peak_Rate(PRC_FMUL, info, fmuls_per_cycle);
  CGTARG_Peak_Rate(PRC_IOP, info, iops_per_cycle);

  if (flops != 0) {
    BOOL unbalanced_fpu = FALSE;

    if ( madds_per_cycle[0] != 0 ) {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak) (madds count as 2)%s"
                   "%s%5d flop%1s        (%3d%% of peak) (madds count as 1)%s"
                   "%s%5d madd%1s        (%3d%% of peak)%s",
		 prefix,
		 flops + madds,
		 Plural(flops + madds),
		 Percent_Of_Peak(flops + madds, ii * 2, madds_per_cycle),
		 suffix,
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix,
		 prefix,
		 madds,
		 Plural(madds),
		 Percent_Of_Peak(madds, ii, madds_per_cycle),
		 suffix);
    }
    else {
      fprintf(file,"%s%5d flop%1s        (%3d%% of peak)%s",
		 prefix,
		 flops,
		 Plural(flops),
		 Percent_Of_Peak(flops, ii, flops_per_cycle),
		 suffix);
    }

    if ( unbalanced_fpu ) {
      INT fmuls2_per_cycle[2]; /* combined fmul/madd peak rate */
      INT fadds2_per_cycle[2]; /* combined fadd/madd peak rate */
      INT fadds2 = fadds + madds;
      INT fmuls2 = fmuls + madds;

      Harmonic_Mean(fmuls2_per_cycle,
		    fmuls, fmuls_per_cycle,
		    madds, madds_per_cycle);
      Harmonic_Mean(fadds2_per_cycle,
		    fadds, fadds_per_cycle,
		    madds, madds_per_cycle);

      fprintf(file,"%s%5d fmul%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fmuls2,
		 Plural(fmuls2),
		 Percent_Of_Peak(fmuls2, ii, fmuls2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
      fprintf(file,"%s%5d fadd%1s        (%3d%% of peak)%s%s",
		 prefix,
		 fadds2,
		 Plural(fadds2),
		 Percent_Of_Peak(fadds2, ii, fadds2_per_cycle),
		 madds_per_cycle[0] ? " (madds count as 1)" : "",
		 suffix);
    }
  }

  s = "";
  if (FALSE) {
    iops += memrefs;
    s = " (mem refs included)";
  }

  fprintf(file,"%s%5d mem ref%1s     (%3d%% of peak)%s"
               "%s%5d integer op%1s  (%3d%% of peak)%s%s"
               "%s%5d instruction%1s (%3d%% of peak)%s",
               prefix,
               memrefs,
               Plural(memrefs),
               Percent_Of_Peak(memrefs, ii, memrefs_per_cycle),
	       suffix,
               prefix,
               iops,
               Plural(iops),
               Percent_Of_Peak(iops, ii, iops_per_cycle),
	       s,
	       suffix,
               prefix,
               insts,
               Plural(insts),
               Percent_Of_Peak(insts, ii, insts_per_cycle),
	       suffix);
}



/* =======================================================================
 *
 *  CGTARG_Compute_PRC_INFO
 *
 *  Compute some basic information about the given 'bb'. 
 *
 * =======================================================================
 */
void
CGTARG_Compute_PRC_INFO(
  BB *bb,
  PRC_INFO *info
)
{
  OP *op;

  memset(info, 0, sizeof (PRC_INFO));

  for ( op = BB_first_op(bb); op != NULL; op = OP_next(op) ) {
    INT num_insts = OP_Real_Ops (op);

    if (num_insts == 0) continue;

    info->refs[PRC_INST] += num_insts;

    if ( OP_flop(op) ) {
      BOOL is_single = (OP_result_size(op,0) == 32);

      ++info->refs[PRC_FLOP];
      info->refs[PRC_FLOP_S] += is_single;
      if (OP_madd(op)) {
        ++info->refs[PRC_MADD];
	info->refs[PRC_MADD_S] += is_single;
      }
      else if (OP_fadd(op) || OP_fsub(op)) {
	++info->refs[PRC_FADD];
	info->refs[PRC_FADD_S] += is_single;
      }
      else if (OP_fmul(op)) {
	++info->refs[PRC_FMUL];
	info->refs[PRC_FMUL_S] += is_single;
      }
    }
    else if (OP_memory(op))
      ++info->refs[PRC_MEMREF];
    else {
      INT k;

      /* Conditional moves and m[tf]c1 are not tagged as flops.
       * We certainly don't want to call them integer ops, so assume
       * anything that uses FP regs isn't an integer instruction.
       */
      if (OP_has_result(op) && TN_is_float(OP_result(op,0))) goto not_iop;

      for (k = 0; k < OP_opnds(op); k++) {
	if (TN_is_float(OP_opnd(op,k))) goto not_iop;
      }

      info->refs[PRC_IOP] += num_insts;

    not_iop:
      ;
    }
  }
}


/* ====================================================================
 *
 * CG_TARG_Branch_Info
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Branch_Info ( const OP  *op,
		     INT *tfirst,  /* Which operand is the first target? */
		     INT *tcount ) /* How many target operands are there? */
{
  INT i;
  TN *tn;

  /* Initialize results: */
  *tfirst = -1;
  *tcount = 0;

  /* Find the first target: */
  for ( i = 0; ; i++ ) {
    if ( i >= OP_opnds(op) ) return;
    tn = OP_opnd(op,i);
    if ( tn != NULL && TN_is_label(tn) ) break;
  }
  *tfirst = i;

  /* Count the targets: */
  *tcount = 1;
  for ( i++; i < OP_opnds(op); i++ ) {
    tn = OP_opnd(op,i);
    if ( tn == NULL || ! TN_is_label(tn) ) return;
    (*tcount)++;
  }
  return;
}


/* ====================================================================
 *
 * CGTARG_Can_Be_Speculative
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Be_Speculative( OP *op )
{
  WN *wn;
  INT offset_idx;

  /* not allowed to speculate anything. */
  if (Eager_Level == EAGER_NONE) return FALSE;

  /* don't speculate volatile memory references. */
  if (OP_volatile(op)) return FALSE;

  if (TOP_Can_Be_Speculative(OP_code(op))) return TRUE;

  if (OP_is_load_const(op)) return TRUE;

  if (!OP_load(op) || OP_unknown_addr(op)) return FALSE;

  /* Try to identify simple scalar loads than can be safely speculated:
   *  a) read only loads (literals, GOT-loads, etc.)
   *  b) load of a fixed variable (directly referenced)
   *  c) load of a fixed variable (base address is constant or
   *     known to be in bounds)
   *  d) speculative, advanced and advanced-speculative loads are safe.
   */

  /*  a) read only loads (literals, GOT-loads, etc.)
   */
  if (OP_no_alias(op)) goto scalar_load;

  /*  b) load of a fixed variable (directly referenced); this
   *     includes spill-restores.
   *  b') exclude cases of direct loads of weak symbols (#622949).
   */
  offset_idx = TI_TOP_Find_Operand_Use(OP_code(op), OU_offset);
  if (offset_idx == -1)
    return FALSE;

  if (TN_is_symbol(OP_opnd(op, offset_idx)) &&
      !ST_is_weak_symbol(TN_var(OP_opnd(op, offset_idx)))) goto scalar_load;

  /*  c) load of a fixed variable (base address is constant or
   *     known to be in bounds), comment out the rematerizable bit check 
   *     since it doesn;t guarantee safeness all the time.
   */
  if (/*   TN_is_rematerializable(OP_opnd(op, 0)) || */
      (   (wn = Get_WN_From_Memory_OP(op))
	  && Alias_Manager->Safe_to_speculate(wn))) goto scalar_load;

  /* d) speculative, advanced, speculative-advanced loads are safe to 
   *    speculate. 
   */
  if (CGTARG_Is_OP_Speculative(op)) goto scalar_load;

  /* If we got to here, we couldn't convince ourself that we have
   * a scalar load -- no speculation this time...
   */
  return FALSE;

  /* We now know we have a scalar load of some form. Determine if they
   * are allowed to be speculated.
   */
scalar_load:
  return TRUE; 
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Speculative_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Speculative_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Advanced_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Advanced_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Is_OP_Check_Load
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Is_OP_Check_Load( OP *memop )
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_OP_Defs_TN
 * CGTARG_OP_Refs_TN
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_OP_Defs_TN( OP *op, TN *tn )
{
  return FALSE;
}

BOOL
CGTARG_OP_Refs_TN( OP *op, TN *tn )
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Interference implementation starts here
 *
 * ====================================================================
 */

static MEM_POOL interference_pool;
static VOID_LIST** writing;     /* writing[i] is a list of live ranges being
                                   written into registers in cycle i */
static BOOL is_loop;            /* Are we working on a loop? */
static INT32 assumed_longest_latency = 40;
                                /* We need to allocate <writing> to be somewhat
                                   longer than the number of cycles in the
                                   schedule in order to accommodate writes
                                   initiated near the end of the schedule.
                                   We'll check and grow this number as
                                   necessary. */
static INT32 cycle_count;       /* Number of cycles in the schedule under
                                   consideration. */
static void (*make_interference)(void*,void*);
                                /* Client's interference call back. */

/* ====================================================================
 *
 * Increase_Assumed_Longest_Latency
 *
 * We need to increase our assumptions about the longest latency operation
 * in our target.  Also reallocate <writing>.
 *
 * ====================================================================
 */
static void
Increase_Assumed_Longest_Latency(INT32 new_longest_latency )
{
  DevWarn("Assumed longest latency should be at least %d",
          new_longest_latency);
  writing = TYPE_MEM_POOL_REALLOC_N(VOID_LIST*,&interference_pool,writing,
                                    cycle_count + assumed_longest_latency,
                                    cycle_count + new_longest_latency);
  assumed_longest_latency = new_longest_latency;
}

/* ====================================================================
 *
 * CGTARG_Interference_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Interference_Required(void)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Interference_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Initialize( INT32 cycle_count_local, BOOL is_loop_local,
                                void (*make_interference_local)(void*,void*) )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Result_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Result_Live_Range( void* lrange, OP* op, INT32 offset )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Operand_Live_Range
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Operand_Live_Range( void* lrange, INT   opnd, OP*   op, INT32 offset )
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}

/* ====================================================================
 *
 * CGTARG_Interference_Finalize
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Interference_Finalize(void)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_Preg_Register_And_Class
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Preg_Register_And_Class(
  WN_OFFSET preg,
  ISA_REGCLASS *p_rclass,
  REGISTER *p_reg
)
{
  /* Get the target register number and class associated with the
   * preg, if there is one that is.
   */
  if (!Preg_Is_Dedicated(preg))
    return FALSE;

  ISA_REGCLASS rclass = Dedicated_Preg_Regclass(preg);
  if (rclass == ISA_REGCLASS_UNDEFINED)
    return FALSE;
  
  INT regnum = Dedicated_Preg_Isa_Reg(preg);

  /* Find the CG register for the target register and class. */
  for ( REGISTER reg = REGISTER_MIN;
	reg <= REGISTER_CLASS_last_register(rclass);
	reg++ )
  {
    if ( REGISTER_machine_id(rclass,reg) == regnum )
    {
      *p_reg = reg;
      *p_rclass = rclass;
      return TRUE;
    }
  }

  FmtAssert(FALSE, ("failed to map preg %d", preg));
  /*NOTREACHED*/
}


/* ====================================================================
 *
 * CGTARG_Compute_Branch_Parameters
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Compute_Branch_Parameters(INT32 *mispredict, INT32 *fixed,
				 INT32 *brtaken, double *factor)
{
  *mispredict = 0;
  *fixed = 0;
  *brtaken = 0;
  *factor = 0.0;

  if (Is_Target_Xtensa())
  {
    *mispredict= 1; *fixed= 1; *brtaken= 2; *factor = 1.0;
  }
  else
  {
    FmtAssert(FALSE, ("invalid target"));
  }

 /*
  * override for command line options
  *	-CG:mispredicted_branch=N
  *	-CG:mispredicted_factor=N
  */
  if (CG_branch_mispredict_penalty >= 0)
    *mispredict= CG_branch_mispredict_penalty ;

  if (CG_branch_mispredict_factor >= 0)
    *factor= CG_branch_mispredict_factor * (.01);
}


/* ====================================================================
 *
 * CGTARG_Can_Change_To_Brlikely
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Can_Change_To_Brlikely(OP *xfer_op, TOP *new_opcode)
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Is_Long_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_Is_Long_Latency(TOP op)
{
  return FALSE;
}

/* ====================================================================
 *
 * CGTARG_Analyze_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
INT
CGTARG_Analyze_Branch( OP *br,
		       TN **tn1,
		       TN **tn2)
{
  INT variant;

  /* Branch operands are usually number 0 and 1. Where something
     different is used, we override below. */

  *tn1 = OP_opnd(br, 0);
  *tn2 = OP_opnd(br, 1);

  switch (OP_code(br))
  {
  case TOP_beq:
  case TOP_beqt:
  case TOP_beqi:
    variant = V_BR_I4EQ;
    break;

  case TOP_bne:
  case TOP_bnei:
  case TOP_bnet:
    variant = V_BR_I4NE;
    break;

  case TOP_bge:
  case TOP_bgei:
    variant = V_BR_I4GE;
    break;

  case TOP_bgeu:
  case TOP_bgeui:
    variant = V_BR_U4GE;
    break;

  case TOP_blt:
  case TOP_blti:
    variant = V_BR_I4LT;
    break;

  case TOP_bltu:
  case TOP_bltui:
    variant = V_BR_U4LT;
    break;


  case TOP_beqz:
  case TOP_beqz_n:
  case TOP_beqzt:
    //
    // Gen_Literal_TN will automatically fold the TNs
    //
    variant = V_BR_I4EQ;
    *tn2 = Gen_Literal_TN(0,4);
    break;
    
  case TOP_bnez:
  case TOP_bnez_n:
  case TOP_bnezt:
    variant = V_BR_I4NE;
    *tn2 = Gen_Literal_TN(0,4);
    break;
    
  case TOP_bgez:
    variant = V_BR_I4GE;
    *tn2 = Gen_Literal_TN(0,4);
    break;
    
  case TOP_bltz:
    variant = V_BR_I4LT;
    *tn2 = Gen_Literal_TN(0,4);
    break;

  case TOP_bf:
  case TOP_bt:
    /* FIXME: Need to add a varient for these. */
    variant = V_BR_NONE;
    *tn1 = NULL;
    *tn2 = NULL;
    break;

  case TOP_ball:  variant = V_BR_ALL_BITS_SET;   break;
  case TOP_bany:  variant = V_BR_ANY_BIT_SET;    break;
  case TOP_bnall: variant = V_BR_ANY_BIT_CLEAR;  break;
  case TOP_bnone: variant = V_BR_ALL_BITS_CLEAR; break; 
  case TOP_bbc:
  case TOP_bbci:  variant = V_BR_BIT_CLEAR;      break;
  case TOP_bbs:
  case TOP_bbsi:  variant = V_BR_BIT_SET;        break;
    break;

  default:
    variant = V_BR_NONE;
    *tn1 = NULL;
    *tn2 = NULL;
    break;
  }

  return variant;
}


/* ====================================================================
 *
 * CGTARG_Analyze_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
INT CGTARG_Analyze_Compare(OP *br,
			   TN **tn1,
			   TN **tn2,
			   OP **compare_op)
{
  TN *cond_tn1;
  TN *cond_tn2;

  /* Classify the condition based on the branch instruction.  */

  INT variant = CGTARG_Analyze_Branch(br, &cond_tn1, &cond_tn2);

  /* Once we have varients for 'bf' and 'bt' (and possibly some
     others), we can attempt to find the comparison. For now, none of
     the branches have associated comparisons. */

  *compare_op = NULL;
  *tn1 = cond_tn1;
  *tn2 = cond_tn2;

  return variant;
}


/* ====================================================================
 *
 * CGTARG_Equivalent_Nonindex_Memory_Op
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Equiv_Nonindex_Memory_Op ( OP *op )
{
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * CGTARG_Which_OP_Select
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Which_OP_Select ( UINT16 bit_size, BOOL is_float, BOOL is_fcc )
{
  /* Seems to be used only for targets that support predication. */

  FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * Is_OP_fp_op1
 *
 * ====================================================================
 */
static BOOL
Is_OP_fp_op1(OP *op)
{
  return FALSE;
}

/* ====================================================================
 *
 * Insert_Stop_Bits
 *
 * ====================================================================
 */
void
Insert_Stop_Bits(BB *bb)
{
  OP *last_op = BB_last_real_op(bb);
  while (last_op && (OP_dummy(last_op) || OP_simulated(last_op)))
    last_op = OP_prev(last_op);
  if (last_op)
    Set_OP_end_group(last_op);
}

/* ====================================================================
 *
 * CGTARG_Special_Min_II
 *
 * See interface description
 *
 * ====================================================================
 */
INT32 CGTARG_Special_Min_II(BB* loop_body, BOOL trace)
{
  return 0;
}

/* ====================================================================
 *
 * Hardware_Workarounds
 *
 * Placeholder for all Hardware workarounds. 
 *
 * ====================================================================
 */
void
Hardware_Workarounds(void)
{
  /* Don't need for Xtensa at this point.  Comment in cgemit.cxx says T5 workaround */
}

/* ====================================================================
 *
 * CGTARG_Initialize
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Initialize(void)
{
  INT tcnt = TI_TOP_Count();
  
  CGTARG_Assoc_Base_Opr_Table =
    TYPE_MEM_POOL_ALLOC_N(OPCODE, MEM_phase_pool_ptr, tcnt);
  CGTARG_Assoc_Base_Top_Table =
    TYPE_MEM_POOL_ALLOC_N(mTOP, MEM_phase_pool_ptr, tcnt);
  CGTARG_Assoc_Base_Fnc_Table =
    TYPE_MEM_POOL_ALLOC_N(mTOP, MEM_phase_pool_ptr, tcnt);

  /* Init all table entries to TOP_UNDEFINED. */

  for (UINT i = 0; i <= TI_ISA_Num_Regclasses(); ++i)
    {
      for (UINT j = 0; j <= TI_ISA_Num_Regclasses(); ++j)
	{
	  CGTARG_Inter_RegClass_Copy_Table[i][j][FALSE] = TOP_UNDEFINED;
	  CGTARG_Inter_RegClass_Copy_Table[i][j][TRUE] = TOP_UNDEFINED;
	}
    }
}


/* ====================================================================
 *
 * CGTARG_Load_From_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Load_From_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID desc = TN_mtype(tn);
  TYPE_ID rtype;
  if (desc != MTYPE_UNKNOWN && 
      (MTYPE_is_tie(desc) || MTYPE_is_xtbool(desc) || MTYPE_is_float(desc)))
    rtype = desc;
  else {
    desc = TY_mtype(ST_type(mem_loc));
    rtype = Mtype_TransferSize(MTYPE_U4, desc);
  }
  Exp_Load(rtype, desc, tn, mem_loc, 0, ops, 0);
}

/* ====================================================================
 *
 * CGTARG_Store_To_Memory
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Store_To_Memory(TN *tn, ST *mem_loc, OPS *ops)
{
  TYPE_ID mtype = TN_mtype(tn);
  if (mtype == MTYPE_UNKNOWN ||
      !(MTYPE_is_tie(mtype) || MTYPE_is_xtbool(mtype) || MTYPE_is_float(mtype)))
    mtype = TY_mtype(ST_type(mem_loc));
  Exp_Store(mtype, tn, mem_loc, 0, ops, 0);
}


/* ====================================================================
 *
 * CGTARG_Init_Assoc_Base
 *
 * See interface description
 *
 * ====================================================================
 */
void CGTARG_Init_Assoc_Base(void)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_Copy_Operand
 *
 * See interface description
 *
 * ====================================================================
 */
INT
CGTARG_Copy_Operand(OP *op)
{
  TOP opr = OP_code(op);
  switch (opr)
  {
  case TOP_mov_n:
    return 0;
    
  case TOP_addi:
  case TOP_addmi:
  case TOP_slli:
  case TOP_srli:
  case TOP_srai:
    if (TN_has_value(OP_opnd(op,1)) && TN_value(OP_opnd(op,1)) == 0)
      return 0;
    break;

  case TOP_and:
  case TOP_max:
  case TOP_maxu:
  case TOP_min:
  case TOP_minu:
  case TOP_or:
    if (OP_opnd(op,0) == OP_opnd(op,1))
      return 0;
    break;
  }

  /* If 'op' is marked as being a copy, we check targ_info for tie and
     other cases. We can only use this interface for op's marked as
     copies, since we can't otherwise tell when a tie operation is
     acting as a copy or not. */

  if (OP_copy(op))
    return TI_ISA_Copy_Operand(opr, FALSE);

  return -1;
}


/* ====================================================================
 *
 * CGTARG_Can_Fit_Immediate_In_Entry_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */

BOOL
CGTARG_Can_Fit_Immediate_In_Entry_Instruction (INT64 immed)
{
  if (Target_ABI == ABI_WINDOWED)
    return TI_ISA_LC_Value_In_Class(immed, LC_uimm12x8);
  else
    return TI_ISA_LC_Value_In_Class(immed, LC_simm8);
}

/* ====================================================================
 *
 * CGTARG_Can_Fit_Immediate_In_Exit_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */

BOOL
CGTARG_Can_Fit_Immediate_In_Exit_Instruction (INT64 immed)
{
  if (Target_ABI == ABI_WINDOWED)
    return TRUE;
  else
    return TI_ISA_LC_Value_In_Class(immed, LC_simm8) 
      || TI_ISA_LC_Value_In_Class(immed, LC_simm8x256);
}


/* ====================================================================
 *
 * CGTARG_Can_Load_Immediate_In_Single_Instruction
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Can_Load_Immediate_In_Single_Instruction (INT64 immed)
{
  return TI_ISA_LC_Value_In_Class(immed, LC_simm12);
}



/* ====================================================================
 *
 * CGTARG_Predicate_OP
 *
 * See interface description
 *
 * ====================================================================
 */
/*ARGSUSED*/
void
CGTARG_Predicate_OP(BB* bb, OP* op, TN* pred_tn)
{
  if (OP_has_predicate(op)) {
    FmtAssert( FALSE, ( "CGTARG_Which_OP_Select: Unsupported Target") );
  }
}

/* ====================================================================
 *
 * CGTARG_Branches_On_True
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Branches_On_True(OP* br_op, OP* cmp_op)
{
  return FALSE;
}



/* ====================================================================
 *
 * CGTARG_Parallel_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
TOP
CGTARG_Parallel_Compare(OP* cmp_op, COMPARE_TYPE ctype)
{
  return TOP_UNDEFINED;
}

/* ====================================================================
 *
 * CGTARG_Dependence_Required
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Dependence_Required(OP *pred_op, OP *succ_op)
{

  static BOOL rsr_ccount_top_initialized=false;
  static BOOL rsr_ccount_top=TOP_UNDEFINED;

  if ( CGTARG_Is_OP_Barrier(pred_op) || CGTARG_Is_OP_Barrier(succ_op) )
    return TRUE;

  if (rsr_ccount_top_initialized==false) {
    rsr_ccount_top=TI_TOP_Topcode("RSR.CCOUNT");
    rsr_ccount_top_initialized=true;
  }

  TOP pred_top = OP_code(pred_op);
  TOP succ_top = OP_code(succ_op);

  switch (pred_top)
  {
  case TOP_s32c1i:
  case TOP_l32ai:
  case TOP_dsync:
  case TOP_esync:
  case TOP_isync:
  case TOP_rsync:
    return TRUE;
  case TOP_extw:
  case TOP_extw_pseudo:
    if (OP_memory(succ_op) || OP_side_effects(succ_op) ||
	OP_extern_effects(succ_op) || CGTARG_Is_OP_Memory_Asm(succ_op))
	    return TRUE;
    break;
  case TOP_memw:
  case TOP_memw_pseudo:
    return ((OP_memory(succ_op) &&
	     OP_code(succ_op) != TOP_l32r &&
	     OP_code(pred_op) != TOP_load_const) ||
	     OP_code(succ_op)==TOP_asm &&
	     	(OP_volatile(succ_op) || CGTARG_Is_OP_Memory_Asm(succ_op)));
  }

  switch (succ_top)
  {
  case TOP_s32c1i:
  case TOP_s32ri:
  case TOP_dsync:
  case TOP_esync:
  case TOP_isync:
  case TOP_rsync:
    return TRUE;
  case TOP_extw:
  case TOP_extw_pseudo:
    if (OP_memory(pred_op) || OP_side_effects(pred_op) ||
	OP_extern_effects(pred_op) || CGTARG_Is_OP_Memory_Asm(pred_op))
	    return TRUE;
    break;
  case TOP_memw:
  case TOP_memw_pseudo:
    return ((OP_memory(pred_op) &&
	     OP_code(pred_op) != TOP_l32r &&
	     OP_code(pred_op) != TOP_load_const) ||
	     OP_code(pred_op)==TOP_asm &&
	     	(OP_volatile(succ_op) || CGTARG_Is_OP_Memory_Asm(succ_op)));
  }

  if (pred_top==rsr_ccount_top ||
      succ_top==rsr_ccount_top)
    return TRUE;

  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Adjust_Latency
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Adjust_Latency(OP *pred_op, OP *succ_op, CG_DEP_KIND kind, UINT8 opnd, INT *latency)
{
  // TENSILICA: xtensa doesn't have any target-specific latency adjustments */
}

/* ====================================================================
 *
 * CGTARG_Generate_Remainder_Branch
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Remainder_Branch(TN *trip_count, TN *label_tn,
				 OPS *prolog_ops, OPS *body_ops)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_OP_is_counted_loop
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL CGTARG_OP_is_counted_loop(OP *op) 
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Generate_Branch_Cloop
 *
 * See interface description
 *
 * ====================================================================
 */
void
CGTARG_Generate_Branch_Cloop(BB *prolog_bb,
			     BB *loopend_bb,
			     TN *unrolled_trip_count,
			     TN *trip_count_tn,
			     INT32 ntimes,
			     TN *label_tn,
			     OPS *prolog_ops,
			     OPS *body_ops)
{
  /* We assume that 'loopend_bb' has two successors, and 'prolog_bb'
     has one; and that 'loopend_bb' has a fall-through block. */
  Is_True((BB_succs_len(prolog_bb) == 1)
	  && (BB_succs_len(loopend_bb) == 2)
	  && (BB_next(loopend_bb) != NULL),
	  ("Unexpected block successors"));
  
  /* tns representing the loop count registers... */
  TN *lcount = Build_Dedicated_TN(REGISTER_CLASS_lcount, REGISTER_lcount, 4);
  TN *lbeg = Build_Dedicated_TN(REGISTER_CLASS_lbeg, REGISTER_lbeg, 4);
  TN *lend = Build_Dedicated_TN(REGISTER_CLASS_lend, REGISTER_lend, 4);

  /* Find the fall-through target of 'br_op' (loop epilog) and create
     a label tn referencing that block. The loop instruction at the
     start of the loop targets that label. */

  BB *epilog = BB_next(loopend_bb);
  LABEL_IDX label = Gen_Label_For_BB(epilog);
  TN *target = Gen_Label_TN(label,0);

  /* Insert the loop instruction at the end of the loop prolog. If the
     trip count is constant, then we must first copy it into a
     register. */
  if (TN_is_constant(trip_count_tn))
  {
    TN *tmp_tn = Gen_Typed_Register_TN(TN_mtype(trip_count_tn),
				       TN_size(trip_count_tn));
    Exp_COPY(tmp_tn, trip_count_tn, prolog_ops);
    trip_count_tn = tmp_tn;
  }

  /* The loop instruction we use is based on the type of the
     trip-count tn. Or if the trip count is a0 (this is a special case
     for non-trip-counted loops) use "loop". */
  
  TOP top_loop = ((MTYPE_signed(TN_mtype(trip_count_tn))) ? TOP_loopgtz : TOP_loopnez);

  /* The windowed abi doesn't have a return address register in the
     way that the xcc abi description uses them. So we don't have an
     easy abi call to get it. */
  if (trip_count_tn == Build_Dedicated_TN(ISA_REGCLASS_integer, 1, 4))
    top_loop = TOP_loop;

  Build_OP(top_loop, 
	   lbeg,
	   lend,
	   lcount,
	   trip_count_tn,
	   target, 
	   prolog_ops);

  Link_Pred_Succ_with_Prob(prolog_bb, epilog, 0);

  /* Insert the loop_end pseudo-instruction at the end of the loop, to
     replace 'br_op'. It targets the same label as 'br_op'. */
  
  Build_OP(TOP_loop_end, 
	   lcount,
	   lcount,
	   label_tn, 
	   body_ops);
}


/* ====================================================================
 *
 * CGTARG_Generate_Updating_Op
 *
 * See interface description
 *
 * ====================================================================
 */
OP *
CGTARG_Generate_Updating_Op (OP *op, INT storeval_idx, INT base_idx,
			     INT offset_idx, TN *new_offset_tn)
{
  FmtAssert(OP_load(op) || OP_store(op), ("expecting load or store OP"));

  TOP updating_top = TI_TOP_Nonupdate_To_Update(OP_code(op));
  Is_True(updating_top != TOP_UNDEFINED, ("expecting updating version of %s\n",
					  TI_TOP_Name(OP_code(op))));
  if (TN_is_register(new_offset_tn)) {
    updating_top = TI_TOP_Nonindex_To_Index(updating_top);
    Is_True(updating_top != TOP_UNDEFINED, ("expecting indexed version of %s\n",
					    TI_TOP_Name(updating_top)));
  }

  /* We must incrementally add the results and operands, so we make an
     OP with room for lots of them. But if we haven't enough room,
     return NULL to indicate we can't create the updating op. We can
     increase MAX_UPDATING_RES_OPNDS and the number of NULL args in
     Mk_OP below if necessary. */

#define MAX_UPDATING_RES_OPNDS 32
  
  const ISA_OPERAND_INFO *update_info = TI_ISA_Operand_Info(updating_top);
  const UINT opnds = TI_ISA_Op_Operands(update_info);
  const UINT results = TI_ISA_Op_Results(update_info);

  if ((opnds + results) > MAX_UPDATING_RES_OPNDS)
  {
    DevWarn("Can't create updating op %s, too many operands and results, max %d",
	    TI_TOP_Name(updating_top), MAX_UPDATING_RES_OPNDS);
    return NULL;
  }
  
  OP *new_op = Mk_OP(updating_top,
		     /* MAX_UPDATING_RES_OPNDS NULLs */
		     NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		     NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		     NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
		     NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

  /* We allow there to be state register results in 'updating_top',
     'base_idx' TN result (it is being updated after all), and if 'op'
     is a load, the result TN of the load. */

  TN *base_tn = OP_opnd(op, base_idx);
  TN *result_tn = NULL;

  Is_True(TN_is_register(base_tn), ("expecting register for base tn"));

  if (OP_load(op))
  {
    if (OP_results(op) != 1)
    {
      DevWarn("expecting 1 load result for load being converted to updating");
      return NULL;
    }

    result_tn = OP_result(op, 0);
    Is_True(TN_is_register(result_tn), ("expecting register for load result"));
  }

  /* Add result TN's to 'new_op'... */
  
  for (UINT i = 0; i < results; i++)
  {
    const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Result(update_info, i);

    /* If result 'i' is a register:
       1. first one must be load result (if 'op' is a load)
       2. next one must be base register
       3. any others must be state registers
    */
    
    if (TI_ISA_Valtyp_Is_Register(vtype))
    {
      const UINT result_cnt = ((OP_store(op)) ? i + 1 : i);
      ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(vtype);

      switch (result_cnt)
      {
      case 0:
	Is_True(OP_load(op), ("expecting load"));
	if (rc != TN_register_class(result_tn))
	{
	  DevWarn("updating top %s has different result regclass than non-updating top %s",
		  TI_TOP_Name(updating_top), TI_TOP_Name(OP_code(op)));
	  return NULL;
	}
	
	Set_OP_result(new_op, i, result_tn);
	break;
	
      case 1:
	if (rc != TN_register_class(base_tn))
	{
	  DevWarn("updating top %s has different base regclass than non-updating top %s",
		  TI_TOP_Name(updating_top), TI_TOP_Name(OP_code(op)));
	  return NULL;
	}
	
	Set_OP_result(new_op, i, base_tn);
	break;

      default:
	if (!TI_ISA_Regclass_Is_State(rc))
	{
	  DevWarn("updating top %s has non-state register result",
		  TI_TOP_Name(updating_top));
	  return NULL;
	}
	else
	{
	  ISA_REGSUBCLASS rsc =	TI_ISA_Valtyp_Regsubclass(vtype);
	  UINT rsc_idx = TI_ISA_Regsubclass_Member(TI_ISA_Regsubclass_Info(rsc), 0);
	  Set_OP_result(new_op, i,
			Build_Dedicated_TN(rc,
					   REGISTER_MIN + rsc_idx,
					   0));
	}
	break;
      }
    }
    else
    {
      DevWarn("Non-register return TN in updating top");
      return NULL;
    }
  }
    
  /* We allow there to be state register operands in 'updating_top',
     'base_idx' TN, 'offset_idx' TN, and if 'op' is a store, the
     'storeval_idx' TN. */

  TN *storeval_tn = NULL;
  
  if (OP_store(op))
  {
    storeval_tn = OP_opnd(op, storeval_idx);
    Is_True(TN_is_register(storeval_tn), ("expecting register for storeval"));
  }
  
  /* Add operand TN's to 'new_op'... */
  
  for (UINT i = 0; i < opnds; i++)
  {
    const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(update_info, i);

    /* If operand 'i' is a register:
       1. first one must be stored value (if 'op' is a store)
       2. next one must be base register
       3. next one must be offset immediate
       4. any others must be state registers
    */
    
    if (TI_ISA_Valtyp_Is_Register(vtype))
    {
      const UINT operand_cnt = ((OP_load(op)) ? i + 1 : i);
      ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(vtype);

      switch (operand_cnt)
      {
      case 0:
	Is_True(OP_store(op), ("expecting store"));
	if (rc != TN_register_class(storeval_tn))
	{
	  DevWarn("updating top %s has different storeval regclass than non-updating top %s",
		  TI_TOP_Name(updating_top), TI_TOP_Name(OP_code(op)));
	  return NULL;
	}
	
	Set_OP_opnd(new_op, i, storeval_tn);
	break;
	
      case 1:
	if (rc != TN_register_class(base_tn))
	{
	  DevWarn("updating top %s has different base regclass than non-updating top %s",
		  TI_TOP_Name(updating_top), TI_TOP_Name(OP_code(op)));
	  return NULL;
	}
	
	Set_OP_opnd(new_op, i, base_tn);
	break;

      case 2:
	if (TN_has_value(new_offset_tn)) {
	  DevWarn("updating top %s does not have constant offset operand", TI_TOP_Name(updating_top));
	  return NULL;
	}
	Set_OP_opnd(new_op, i, new_offset_tn);
	break;

      default:
	if (!TI_ISA_Regclass_Is_State(rc))
	{
	  DevWarn("updating top %s has non-state register operand",
		  TI_TOP_Name(updating_top));
	  return NULL;
	}
	else
	{
	  ISA_REGSUBCLASS rsc =	TI_ISA_Valtyp_Regsubclass(vtype);
	  UINT rsc_idx = TI_ISA_Regsubclass_Member(TI_ISA_Regsubclass_Info(rsc), 0);
	  Set_OP_opnd(new_op, i,
		      Build_Dedicated_TN(rc,
					 REGISTER_MIN + rsc_idx,
					 0));
	}
	break;
      }
    }
    else if (TI_ISA_Valtyp_Is_Literal(vtype))
    {
      const UINT operand_cnt = ((OP_load(op)) ? i + 1 : i);
      if (operand_cnt != 2)
      {
	DevWarn("updating top %s has literal tn in non-offset position",
		TI_TOP_Name(updating_top));
	return NULL;
      }

      /* The litclass of 'updating_top's offset can be different than
         'op's. Make sure the offset fits. */
      
      if (!TN_has_value(new_offset_tn)) {
	DevWarn("updating top %s does not have register offset operand", TI_TOP_Name(updating_top));
	return NULL;
      }

      INT64 new_offset_value = TN_value(new_offset_tn);
      ISA_LITCLASS lc = TI_ISA_Valtyp_Litclass(vtype);
      if (!TI_ISA_LC_Value_In_Class(new_offset_value, lc))
      {
	DevWarn("top %s offset %" LLD_FMT " does not fit in updating top %s",
		TI_TOP_Name(OP_code(op)), new_offset_value, TI_TOP_Name(updating_top));
	return NULL;
      }

      Set_OP_opnd(new_op, i, new_offset_tn);
    }
    else
    {
      DevWarn("Non-register/non-literal operand TN in updating top");
      return NULL;
    }
  }

  OP_srcpos(new_op) = OP_srcpos(op);
  if (OP_memory(op))
    Copy_WN_For_Memory_OP(new_op, op);

  return new_op;
}

			     
static TN* asm_constraint_tn[10];
static ISA_REGSUBCLASS asm_constraint_sc[10];
static char asm_constraint_name[10][8];
static INT asm_constraint_index;

// must be called once per asm
void
CGTARG_Init_Asm_Constraints (void)
{
  // can use any type; it will be ignored
  Setup_Output_Parameter_Locations (MTYPE_To_TY(MTYPE_I8));
  for (INT i = 0; i < 10; ++i) {
    asm_constraint_tn[i] = NULL;
    asm_constraint_sc[i] = ISA_REGSUBCLASS_UNDEFINED;
    asm_constraint_name[i][0] = '\0';
  }
  asm_constraint_index = 0;
}


// -----------------------------------------------------------------------
// Given a constraint for an ASM parameter, and the load of the matching
// argument passed to ASM (possibly NULL), choose an appropriate TN for it
// -----------------------------------------------------------------------
extern TN* 
CGTARG_TN_For_Asm_Operand (const char* constraint, 
                           const WN* load,
                           TN* pref_tn,
                           ISA_REGSUBCLASS* subclass,
                           const WN* asm_stmt,
			   TYPE_ID preg_rtype)
{
  // skip constraint modifiers:
  // = input and output parameters are separated in the WHIRL for ASM
  // & early_clobber flag is set in Handle_ASM
  // % commutativity of operands is ignored for now
  static const char* modifiers = "=&%";
  while (strchr(modifiers, *constraint))
  {
    constraint++;
  }
  
  // TODO: we should really look at multiple specified constraints
  // and the load in order to select the best TN, but for now we
  // assume that when we get here we can safely pick a TN

  // if 'm' is one of the choices, always prefer that one
  // TODO: we decide this in the front end, but it's not optimal
  if (*constraint != 'm')
  {
    const char* m = constraint;
    while (*++m)
    {
      if (*m == 'm')
      {
        constraint = m;
        break;
      }
    }
  }
  
  // prefer register/memory over immediates; this isn't optimal, 
  // but we may not always be able to generate an immediate
  static const char* immediates = "in";
  while (strchr(immediates, *constraint) && *(constraint+1))
  {
    constraint++;
  }

  TN* ret_tn;
  
  // TODO: check that the operand satisifies immediate range constraint
  if (strchr(immediates, *constraint))
  {
    if (load && WN_operator(load)==OPR_LDID && WN_class(load)==CLASS_PREG)
    {
      // immediate could have been put in preg by wopt
      load = Preg_Is_Rematerializable(WN_load_offset(load), NULL);
    }
    FmtAssert(load && WN_operator(load) == OPR_INTCONST, 
              ("Cannot find immediate operand for ASM"));
    ret_tn = Gen_Literal_TN(WN_const_val(load), 
                            MTYPE_bit_size(WN_rtype(load))/8);
  }
  // digit constraint means that we should reuse a previous operand
  else if (isdigit(*constraint))
  {
    INT prev_index = *constraint - '0';
    FmtAssert(asm_constraint_tn[prev_index], 
              ("numeric matching constraint refers to NULL value"));
    ret_tn = asm_constraint_tn[prev_index];
  }
  else if (strchr("m", *constraint))
  {
    TYPE_ID rtype = (load != NULL ? WN_rtype(load) : MTYPE_I4);
    FmtAssert(MTYPE_is_integral(rtype),
              ("ASM operand does not satisfy its constraint"));
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  else if ((*constraint == 'r') || (*constraint == 'a') ||
	   (*constraint == 'b') || (*constraint == 'v'))
  {
    /* 'r' indicates that we need some kind of register. 'a' indicates
       ar registers and 'b' indicates 'br' registers. 'v' indicates
       some user-defined tie register. We determine the mtype or the
       register from 'load'. */

    TYPE_ID rtype;
    if (load != NULL) {
      rtype = WN_rtype(load);
    } else if (preg_rtype != MTYPE_UNKNOWN) {
      rtype = preg_rtype;
    } else {
      /* We can't figure out what type the operand is, probably because the
	 optimizer deleted the references to it, so return some default
	 type based on the constraint. */
      rtype = ((*constraint == 'b') ? MTYPE_B : MTYPE_I4);
    }
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(rtype));
  }
  else if (*constraint == 't')
  {
    FmtAssert(pref_tn && TN_is_dedicated(pref_tn) &&
		TN_register_class(pref_tn)==TI_ISA_Regclass_Special(),
		("ASM constraint 't' requires a state register"));
    ret_tn = pref_tn;
  }
  else if (*constraint == 'f')
  {
    if (load && (WN_desc(load) != MTYPE_F4 || WN_rtype(load) != MTYPE_F4)) {
      ErrMsgSrcpos(EC_Inv_Asm_Opnd, WN_Get_Linenum(asm_stmt));
    }
    ret_tn = (pref_tn ? pref_tn : Build_TN_Of_Mtype(MTYPE_F4));
  }
  else
  {
    FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
  }

  asm_constraint_tn[asm_constraint_index] = ret_tn;
  asm_constraint_index++;
  
  return ret_tn;
}


static char *
Get_TN_Assembly_Name (TN *tn)
{
  return "moo";
}

void
CGTARG_TN_And_Name_For_Asm_Constraint (char *constraint, TYPE_ID mtype,
	TYPE_ID desc, TN **tn, char **name)
{
	INT i;
	if (*constraint == '=') {
		// ignore
		CGTARG_TN_And_Name_For_Asm_Constraint (constraint+1, 
			mtype, desc, tn, name);
		return;
	}
	if (mtype == MTYPE_V) {
		// unknown parameter, so pick mtype from constraint
		if (*constraint == 'f') mtype = MTYPE_F8;
		else mtype = MTYPE_I8;
	}
	switch (*constraint) {
	case 'r':
		FmtAssert(MTYPE_is_integral(mtype), 
			("ASM constraint is integer but parameter is not"));
		break;
	case 'f':
		FmtAssert(MTYPE_is_float(mtype), 
			("ASM constraint is float but parameter is not"));
		break;
	case 'm':
		break;
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		i = *constraint - '0';
		FmtAssert(asm_constraint_tn[i], 
		    ("numeric matching constraint refers to NULL value"));
		++asm_constraint_index;
		*tn = asm_constraint_tn[i];
		*name = asm_constraint_name[i];
		return;
	case 'i':
		// let caller figure out the name
		*tn = NULL;
		*name = NULL;
		return;
	default:
		FmtAssert(FALSE, ("ASM constraint <%s> not supported", constraint));
	}
	PLOC ploc = Get_Output_Parameter_Location (MTYPE_To_TY(mtype));
	*tn = PREG_To_TN (MTYPE_To_PREG(mtype), PLOC_reg(ploc));
	asm_constraint_tn[asm_constraint_index] = *tn;
	*name = Get_TN_Assembly_Name(*tn);
	if (*constraint == 'm') {
	    	sprintf(asm_constraint_name[asm_constraint_index], "[%s]", 
			*name);
	} else {
	    	sprintf(asm_constraint_name[asm_constraint_index], "%s", 
			*name);
	}
	*name = asm_constraint_name[asm_constraint_index];
	++asm_constraint_index;
}


/* ====================================================================
 * target specific modifiers for printing different versions
 * of register names when they appear as AM operands 
 * ====================================================================
 */
char CGTARG_Asm_Opnd_Modifiers[] = { 'r' };
INT  CGTARG_Num_Asm_Opnd_Modifiers = 1;

const char* 
CGTARG_Modified_Asm_Opnd_Name(char modifier, TN* tn, char *tn_name)
{
  if (modifier == 'r') {
    return tn_name;
  }
  else {
    FmtAssert(FALSE, ("Unknown ASM operand modifier '%c'", modifier));
  }
  /*NOTREACHED*/
}


/* ====================================================================
 *
 * CGTARG_Postprocess_Asm_String: currently no-op for IA-64
 *
 * ====================================================================
 */
void 
CGTARG_Postprocess_Asm_String (char*)
{
}


/* ====================================================================
 *
 * CGTARG_Unconditional_Compare
 *
 * See interface description
 *
 * ====================================================================
 */
BOOL
CGTARG_Unconditional_Compare(OP *op, TOP* uncond_ver)
{
  return FALSE;
}


/* ====================================================================
 *
 * CGTARG_Init_OP_cond_def_kind
 *
 *  See interface description
 *
 * ====================================================================
 */
void
CGTARG_Init_OP_cond_def_kind(OP *op)
{ 
  if( OP_has_predicate(op) ) {
    FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  } else {
#if 0
    TOP top = OP_code(op);
    if( top == TOP_movltz ||
	top == TOP_movnez ||
	top == TOP_moveqz ||
	top == TOP_movgez ||
	top == TOP_movf ||
	top == TOP_movt ||
        top == TI_TOP_Topcode("movltz.s") ||
        top == TI_TOP_Topcode("movnez.s") ||
        top == TI_TOP_Topcode("moveqz.s") ||
        top == TI_TOP_Topcode("movgez.s") ||
        top == TI_TOP_Topcode("movf.s") ||
        top == TI_TOP_Topcode("movt.s") )
      {
	Set_OP_cond_def_kind(op, OP_ALWAYS_COND_DEF);
      }
    else
#endif
      {
	Set_OP_cond_def_kind(op, OP_ALWAYS_UNC_DEF);
      }
  }
}


/* ====================================================================
 *
 * CGTARG_Get_unc_Variant
 *
 *  Given a compare opcode, return the unconditional variant form. 
 *  Return the opcode if there is no such form.
 *
 * ====================================================================
 */
TOP
CGTARG_Get_unc_Variant (TOP top)
{
  /* This doesn't seem to be used. */
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
  return TOP_UNDEFINED;
}

////////////////////////////////////////////////////////////////
// If a BB ends in an unconditional branch, turn it into a conditional branch 
// With TRUE predicate, so we can predicate with something else later.
// If we can't find an unconditional branch, just give up and do nothing
//
void
Make_Branch_Conditional(BB *bb)
{
  FmtAssert(FALSE,("NOT YET IMPLEMENTED"));
}


/* ====================================================================
 *
 * CGTARG_Check_OP_For_HB_Suitability
 *
 * Returns TRUE if OP is a suitable candidate for HBF. Otherwise, return
 * FALSE.
 *
 * ====================================================================
 */
BOOL 
CGTARG_Check_OP_For_HB_Suitability(OP *op)
{

  // If <op> has a qualifying predicate operand, return TRUE.
  if (OP_has_predicate(op)) return TRUE;

  // <xfer_op> without any qualifying predicate operand are still OK, since
  // they are eliminated as a result of predication.
  if (OP_xfer(op)) return TRUE;

  return FALSE;  // default case
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
