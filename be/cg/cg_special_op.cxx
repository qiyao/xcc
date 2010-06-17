
// Copyright (c) 2003-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

#include "defs.h"
#include "config.h"
#include "errors.h"
#include "cgir.h"
#include "libti.h"
#include "tn.h"
#include "config_targ_options.h"
#include "cg_flags.h"

extern DLL_SHARED ST *SP_Sym;             /* stack pointer */
extern DLL_SHARED ST *FP_Sym;             /* frame pointer */

// check that <tn> is of the type indicated by <valtyp>
static bool
tn_ok(TN* tn, const ISA_OPERAND_VALTYP* valtyp) {

  if (TI_ISA_Valtyp_Is_Register(valtyp)) {
    if (TN_has_value(tn))
      return false;

    int reg = TN_register(tn);

    ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(valtyp);
    if (TN_register_class(tn) != rc)
      return false;

  } else if (TI_ISA_Valtyp_Is_Literal(valtyp)) {

    INT64 value;

    // handle constant and SP/FP offsets (for spill location)
    if (TN_has_value(tn)) {
      value = TN_value(tn);
    } else if (TN_is_symbol(tn)) {
      ST *base_st;
      INT64 base_ofst;

      ST *st = TN_var(tn);
      Base_Symbol_And_Offset (st, &base_st, &base_ofst);
      if (base_st == SP_Sym || base_st == FP_Sym) {
	value = base_ofst + TN_offset(tn);
      } else
	return false;
    } else if (TN_is_label(tn)) {
      /* Treat label literal as okay, so normal branch and the
         corresponding wide branch can form a generic OP.
       */
      return true;
    } else
      return false;

    ISA_LITCLASS lc = TI_ISA_Valtyp_Litclass(valtyp);
    if (TI_ISA_LC_Value_In_Class(value, lc)==false)
      return false;
  }
  return true;
}

// check that all results/operands of <op> are ok if the opcode os replaced
// with <topcode>

static bool
new_top_ok(TOP topcode, OP* op) {

  TOP old_topcode = OP_code(op);

  if (old_topcode==topcode)
    return true;

  const ISA_OPERAND_INFO* new_operand_info = TI_ISA_Operand_Info (topcode);
  const ISA_OPERAND_INFO* old_operand_info = TI_ISA_Operand_Info (old_topcode);

  int num_operands_old = TI_ISA_Op_Operands(old_operand_info);
  int num_results_old = TI_ISA_Op_Results(old_operand_info);
  int num_operands_new = TI_ISA_Op_Operands(new_operand_info);
  int num_results_new = TI_ISA_Op_Results(new_operand_info);

  if (num_operands_old != num_operands_new || num_results_old != num_results_new)
    return false;

  for (int i=0; i<num_operands_new; i++) {
    const ISA_OPERAND_VALTYP* valtyp = TI_ISA_Op_Operand(new_operand_info, i);
    TN* tn = OP_opnd(op, i);
    if (tn_ok(tn, valtyp)==false)
      return false;
  }

  for (int i=0; i<num_results_new; i++) {
    const ISA_OPERAND_VALTYP* valtyp = TI_ISA_Op_Result(new_operand_info, i);
    TN* tn = OP_result(op, i);
    if (tn_ok(tn, valtyp)==false)
      return false;
  }

  return true;

}


// see interface description

TOP
CG_Get_Generic_Top(TOP top) {

  TOP tops[256];

  int num_tops = TI_TOP_Get_Compatible_Tops(top, tops, 256);
  if (num_tops>1) {
    TOP generic_top = TI_TOP_Get_Generic_Top(tops, num_tops);
    return generic_top;
  }

  return top;
}

// see interface description

bool
Is_Generic_Addi_Op(OP* op) {

  if (op==NULL || OP_generic(op)==false)
    return false;

  TOP tops[256];
  TOP generic_top = OP_code(op);

  int num_tops = TI_TOP_Get_Special_Tops(generic_top, tops, 256);
  for (int i=0; i<num_tops; i++) {
    if (tops[i] == TOP_addi)
      return true;
  }

  return false;

}



// see interface description

void
CG_Relax_Special_Op(OP* op) {

  TOP tops[256];
  TOP good_tops[256];

  if (CG_specialization==false || op==NULL)
    return;

  TOP top = OP_code(op);
  int num_tops = TI_TOP_Get_Compatible_Tops(top, tops, 256);
  int num_good_tops=0;
  for (int j=0; j<num_tops; j++) {
    if (new_top_ok(tops[j], op) && TI_TOP_No_Valid_Format(tops[j])==false) {
      good_tops[num_good_tops++] = tops[j];
    }
  }
  if (num_good_tops>1) {
    TOP generic_top = TI_TOP_Get_Generic_Top(good_tops, num_good_tops);

    if (xt_density && generic_top==TOP_UNDEFINED && num_good_tops>2) {
      // purge the density version which may conflict with special version
      // generated by XPRES which causes generic top
      // not being generated (see PR10966)
      bool purged = false;
      for (int j=0; j<num_good_tops; j++) {
	TOP top = good_tops[j];
	const char* top_name = TI_TOP_Name(top);
        if (!strcasecmp(top_name+strlen(top_name)-2, ".n")) {
	  good_tops[j] = TOP_UNDEFINED;
	  purged = true;
	}
      }
      if (purged) {
        int current = 0;
        int last = num_good_tops-1;
        while (current<=last) {
	  if (good_tops[current]==TOP_UNDEFINED) {
	    good_tops[current] = good_tops[last--];
	  } else
	    current++;
        }
        num_good_tops = last+1;
	generic_top = TI_TOP_Get_Generic_Top(good_tops, num_good_tops);
      }
    }
    if (generic_top!=TOP_UNDEFINED) {
      Set_OP_code(op,generic_top);
    }
  }
}


// see interface description

void
CG_Relax_Special_Ops() {

  BB* bb;

  for (bb = REGION_First_BB; bb; bb = BB_next(bb)) {
    OP* op;
    FOR_ALL_BB_OPs(bb,op) {
      CG_Relax_Special_Op(op);
    }
  }

}


// see interface description

void
CG_Specialize_Op(OP* op) {

  TOP tops[256];

  if (op==NULL || OP_generic(op)==false)
    return;

  TOP generic_top = OP_code(op);
  int fmt = OP_format_num(op);
  int slot = OP_slot_num(op);

  int num_tops = TI_TOP_Get_Special_Tops(generic_top, tops, 256);

  if (fmt == -1 || slot == -1) {
    // for op which is not bundled yet, choose the first variation
    if (num_tops>0)
      Set_OP_code(op,tops[0]);
    return;
  }

  for (int i=0; i<num_tops; i++) {
    TOP specialized_top = tops[i];
    if (TI_ISA_Exec_Unit_Prop(specialized_top) &
	TI_ISA_Exec_Slot_Prop(fmt,slot)) {
      Set_OP_code(op,specialized_top);
      return;
    }
  }

  FmtAssert(0, ("Failed to specialize generic op %s", TI_TOP_Name(generic_top)));

}

void
CG_Convert_To_Predicted_Branch(BB * bb) 
{
  FmtAssert(xt_brt, ("Config doesn't support predicted branches"));
  /* we know that there will only be two successors
     to the branches we care about */

  Set_Error_Phase("Convert Predicted Branches");

  OP * op = BB_branch_op(bb);
  if (!op || (BB_succs_len(bb) != 2))
    return;

  BBLIST * not_taken_list = BBlist_Fall_Thru_Succ(bb);
  if (not_taken_list == NULL)
    return;
  BB * not_taken = BBLIST_item(not_taken_list);
  if (not_taken == NULL)
    return;
  BB * taken = BB_Other_Successor(bb, not_taken);
  if (taken == NULL)
    return;
  BBLIST * taken_list = BBlist_Find_BB(BB_succs(bb), taken);
  if (taken_list == NULL)
    return;

  //    printf("taken prob = %f\n", BBLIST_prob(taken_list));
      
  //xt_brt_threshold is a float percentage value
  if (BBLIST_prob(taken_list) > xt_brt_threshold) {
      
    switch (OP_code(op)) {
    case TOP_beq:
      OP_Change_Opcode(op, TOP_beqt); 
      break;
    case TOP_beqz:
    case TOP_beqz_n:
      OP_Change_Opcode(op, TOP_beqzt); 
      break;
    case TOP_bne:
      OP_Change_Opcode(op, TOP_bnet);
      break;
    case TOP_bnez:
    case TOP_bnez_n:
      OP_Change_Opcode(op, TOP_bnezt);
      break;
    default:
      break;
    }
  }
  if (BBLIST_prob(taken_list) > xt_immed_brt_threshold &&
      (OP_code(op) == TOP_beqi || OP_code(op) == TOP_bnei)) {
    TN * tn = Build_TN_Of_Mtype(MTYPE_I4);
    BB_Append_Op(bb, Mk_OP(TOP_movi, tn, OP_opnd(op, 1)));
    BB_Append_Op(bb, Mk_OP((OP_code(op) == TOP_beqi) ? TOP_beqt : TOP_bnet, 
			   OP_opnd(op, 0), tn, OP_opnd(op, 2)));
    BB_Remove_Op(bb, op);
  }
}