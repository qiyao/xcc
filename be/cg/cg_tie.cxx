
//
// CG TIE intrinsic support
//

// $Id: cg_tie.cxx,v 1.1 2004/12/23 20:33:29 bwilson Exp bwilson $


/*

  Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.

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

*/

#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "errors.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "id_map.h"
#include "intrn_info.h"
#include "wn_util.h"
#include "be_util.h"
#include "ir_reader.h"
#include "config_asm.h"
#include "whirl2ops.h"
#include "tie.h"
#include "cg_tie.h"

using idmap::ID_MAP;

extern DLL_SHARED BOOL xt_reorder_tieport;
extern DLL_SHARED BOOL xt_flush_tieport;

/* Return the register class given type 'name'. */

static ISA_REGCLASS
ctype_to_regclass(const char *name)
{

  ISA_REGCLASS cl;

  if (!strcmp(name, "immediate"))
    return ISA_REGCLASS_UNDEFINED;
  else if (!strncmp(name, "_TIE_xtbool", 11)) {
    cl = TI_ISA_Regclass_Branch();
    if (cl == ISA_REGCLASS_UNDEFINED) {
      FmtAssert(0,("Missing BR register file"));
    } else
      return cl;
  }

  int i = 0;
  while ((*(name+i) != '\0') && (*(name+i) != ' ') && (*(name+i) !='*'))
    i++;

  int length = i;
  bool is_pointer = false;

  while (*(name+i) != '\0') {
    if (*(name+i)=='*') {
      FmtAssert(*((name+i)+1) == '\0',("Bad ctype name"));
      is_pointer = true;
      break;
    } else {
      FmtAssert(*(name+i) == ' ', ("Bad ctype name"));
    }
    i++;
  }

  if (is_pointer)
    return TI_ISA_Regclass_Integer();

  char buffer[256];
  strncpy(buffer,name,length);
  buffer[length]='\0';

  TYPE_ID mtype=tie_info->mtype_id(buffer);

  if (mtype == MTYPE_UNKNOWN) {
    /* assume anything other else goes to AR */
    return TI_ISA_Regclass_Integer();
  }

  return TI_ISA_Regclass_For_Mtype(mtype);
}


/* Return the register class given type id 'type'. */

static ISA_REGCLASS
ctype_to_regclass(const TIE_TYPE_ID type)
{

  return TI_ISA_Regclass_For_Mtype(Tie_Type_Id_To_Mtype(type));

}

static void
mark_copies (OPS *ops)
{
  /* Mark all OPs in 'ops' that are definately copies (i.e. they
     always act as copies, so we wouldn't mark addi as a copy since it
     is only a copy when the immediate is 0. */
  for (OP *op = OPS_first(ops); op; op = OP_next(op))
  {
    if (TI_ISA_Property_Set(PROP_move, OP_code(op)))
      Set_OP_copy(op);
  }
}

static int
expand_tie_macro_rec(
                     OPS* const ops,
                     const TIE_MACRO_p tie_macro,
                     TN* const in_operands[])
{
    FmtAssert(ops!=NULL,("NULL ops encountered."));

    xtensa_isa isa_info = TI_TIE_Libisa_Info();

    int num_protos=tie_macro->num_protos();
    int num_temps=tie_macro->num_temps();
    int num_instructions=tie_macro->num_instructions();
    int i;

    TN* operands[TI_TIE_OPERANDS_MAX];
    if (num_protos+num_temps>TI_TIE_OPERANDS_MAX) {
      fprintf(stderr,"Too many (>%d) tie macro arguments for %s\n",
		      TI_TIE_OPERANDS_MAX, tie_macro->name());
      return 0;
    }

    for (i=0; i<num_protos; i++) {
      operands[i]=in_operands[i];
      if (tie_macro->proto_is_out(i) && tie_macro->proto_is_live_in(i)) {
	TYPE_ID mtype = tie_macro->proto_mtype_id(tie_info,i);
	ISA_REGCLASS rc = TI_ISA_Regclass_For_Mtype(mtype);
	TOP def_topcode = TI_ISA_Regclass_Def_Topcode(rc);
	OP* op = Mk_VarOP(def_topcode, 1, 0, &operands[i], NULL);
        OPS_Append_Op(ops, op);
      }
    }

    for (i=0; i<num_temps; i++) {
      operands[num_protos+i] =
		Build_TN_Of_Mtype(
		tie_info->mtype_id(tie_macro->temp_type_name(i)));
      if (tie_macro->temp_is_live_in(i)) {
	TYPE_ID mtype = tie_macro->temp_mtype_id(tie_info,i);
	ISA_REGCLASS rc = TI_ISA_Regclass_For_Mtype(mtype);
	TOP def_topcode = TI_ISA_Regclass_Def_Topcode(rc);
	OP* op = Mk_VarOP(def_topcode, 1, 0, &operands[num_protos+i], NULL);
        OPS_Append_Op(ops, op);
      }
    }

    if (Get_Trace(TP_TIE, TP_TIE_macro_expansion)) {
      fprintf(TFile, "TIE:Expanding macro:  %s", tie_macro->name());
      for (i=0; i<num_protos; i++) {
	fprintf(TFile, " ");
        Print_TN(operands[i], FALSE);
      }
      if (num_temps>0) {
	fprintf(TFile, "[temps:");
        for (i=0; i<num_temps; i++) {
	  fprintf(TFile, " ");
          Print_TN(operands[num_protos+i], FALSE);
        }
	fprintf(TFile, "]");
      }
      fprintf(TFile, "\n");
    }

    i = 0;
    while (i<num_protos) {
      FmtAssert(operands[i]!=NULL,("Missing operand"));

#ifdef DEBUG
      const char* ctype_name = tie_macro->proto_type_name(i);
      ISA_REGCLASS rc = ctype_to_regclass(ctype_name);
      TN* tn = operands[i];
      if (rc == ISA_REGCLASS_UNDEFINED) {
	FmtAssert(!strcmp(ctype_name,"immediate"),("Unlnown register class"));
	FmtAssert(TN_has_value(tn) || TN_is_symbol(tn),("Unknown operand"));
      } else {
	FmtAssert(TN_is_register(tn),("Expecting register operand"));
	FmtAssert(TN_register_class(tn) == rc,("Register class mis-match"));
      }
#endif

      i++;
    }

    i = 0;
    while (i<num_temps) {
      FmtAssert(operands[i],("Missing operand"));

#ifdef DEBUG
      const char* ctype_name = tie_macro->temp_type_name(i);
      ISA_REGCLASS rc = ctype_to_regclass(ctype_name);
      TN* tn = operands[i+num_protos];

      /* temp has to be a register */
      FmtAssert(rc!=ISA_REGCLASS_UNDEFINED,("Unknown register class"));
      FmtAssert(TN_is_register(tn),("Expecting register temp operand"));
      FmtAssert(TN_register_class(tn) == rc,("Register class mis-match"));
#endif

      i++;
    }

    for (i=0; i<num_instructions; i++) {
      const char* opcode_name = tie_macro->inst_opcode_name(i);
      TOP top = TI_TOP_Topcode(opcode_name);
      TN* out_operands[TI_TIE_OPERANDS_MAX];
      int num_operands = tie_macro->num_inst_operands(i);
      for (int opnd=0; opnd<num_operands; opnd++) {
	int offset=0;
        int operand_index = tie_macro->operand_index(i, opnd, offset);
	if (operand_index == -1) {
	  out_operands[opnd] = Gen_Literal_TN(offset,4);
	} else {
	  out_operands[opnd] = operands[operand_index];
	  if (offset!=0) {
	    TN* tn = Dup_TN(out_operands[opnd]);
	    if (TN_has_value(tn))
	      Set_TN_value(tn,TN_value(tn)+offset);
	    else if (TN_is_symbol(tn))
	      Set_TN_offset(tn,TN_offset(tn)+offset);
	    else
	      FmtAssert(0,("Unexpected TN"));
	    out_operands[opnd]=tn;
	  }
	}
      }

      if (top != TOP_UNDEFINED && !strcmp(opcode_name, TI_TOP_Name(top))) {

	xtensa_opcode xopc = xtensa_opcode_lookup(isa_info, opcode_name);

	TN* op_operands[TI_ISA_OPERANDS_MAX];
	TN* op_results[TI_ISA_RESULTS_MAX];
	int num_op_operands=0;
	int num_op_results=0;

	const ISA_OPERAND_INFO *operands_info = TI_ISA_Operand_Info (top);

        int code_opnd_index = 0;
	int isa_opnd_index = 0;
	const ISA_OPERAND_VALTYP *operand_value_type;
        while (code_opnd_index<num_operands) {

	  char inout;

	  if (xopc == XTENSA_UNDEFINED) {
	    /* this is a "def_" prepended in the loadi proto to terminate
	       incorrect live-in of an inout operands
	    */
	    inout = 'o';
	  } else {
	    isa_opnd_index =
	      tie_macro->proto_insn_arg_to_opnd(i, code_opnd_index, 0);
	    inout = xtensa_operand_inout(isa_info, xopc, isa_opnd_index);
	  }
	  int operand_index_increment = 1;
	  if (inout == 'i' || inout == 'm') {
	    op_operands[num_op_operands] = out_operands[code_opnd_index];
	    operand_value_type =
		TI_ISA_Op_Operand (operands_info, num_op_operands);
	    if (xopc != XTENSA_UNDEFINED &&
		xtensa_operand_is_register(isa_info, xopc, isa_opnd_index) == 1) {

	      // check for non-allocatable register operand and replace
	      // the immediate value with dedicated registger TN

	      ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(operand_value_type);
	      if (TI_ABI_Property_Set(ABI_PROPERTY_allocatable, rc, -1)==false) {
		TN* immed_tn = op_operands[num_op_operands];
		FmtAssert(TN_has_value(immed_tn),
			  ("Expect immediate value for non-allocatable register"
			  " operand of %s", opcode_name));
		op_operands[num_op_operands]=
			  Build_Dedicated_TN(rc,
					     REGISTER_MIN+TN_value(immed_tn),0);
	      }

	      int isa_num_regs =
		xtensa_operand_num_regs(isa_info, xopc, isa_opnd_index);
	      int cg_num_regs = TI_ISA_Valtyp_Num_Regs(operand_value_type);
	      if (isa_num_regs != cg_num_regs) {
		/* expand a multi-reg operand */
		operand_index_increment = isa_num_regs;
		FmtAssert(cg_num_regs == 1,
			("Inconsistent usage of multi-reg operand for %s",
			opcode_name));
		for (int k=1; k<isa_num_regs; k++) {
		  op_operands[num_op_operands+k] =
			out_operands[code_opnd_index+k];
		  operand_value_type =
			TI_ISA_Op_Operand (operands_info, num_op_operands+k);
		  FmtAssert(TI_ISA_Valtyp_Num_Regs(operand_value_type)==1,
			("Inconsistent usage of multi-reg operand for %s",
			opcode_name));
		}
		num_op_operands += (isa_num_regs-1);
	      }
	    }
	    num_op_operands++;
	  }
	  if (inout == 'o' || inout == 'm') {
	    op_results[num_op_results] = out_operands[code_opnd_index];
	    operand_value_type =
		TI_ISA_Op_Result (operands_info, num_op_results);
	    if (xopc != XTENSA_UNDEFINED &&
		xtensa_operand_is_register(isa_info, xopc, isa_opnd_index) == 1) {

	      // check for non-allocatable register operand and replace
	      // the immediate value with dedicated registger TN

	      ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(operand_value_type);
	      if (TI_ABI_Property_Set(ABI_PROPERTY_allocatable, rc, -1)==false) {
		TN* immed_tn = op_results[num_op_results];
		FmtAssert(TN_has_value(immed_tn),
			  ("Expect immediate value for non-allocatable register"
			  " operand of %s", opcode_name));
		op_results[num_op_results]=
			  Build_Dedicated_TN(rc,
					     REGISTER_MIN+TN_value(immed_tn),0);
	      }
	      int isa_num_regs =
		xtensa_operand_num_regs(isa_info, xopc, isa_opnd_index);
	      int cg_num_regs = TI_ISA_Valtyp_Num_Regs(operand_value_type);
	      if (isa_num_regs != cg_num_regs) {
		/* expand a multi-reg result */
		operand_index_increment = isa_num_regs;
		FmtAssert(cg_num_regs == 1,
			("Inconsistent usage of multi-reg operand for %s",
			opcode_name));
		for (int k=1; k<isa_num_regs; k++) {
		  op_results[num_op_results+k] =
			out_operands[code_opnd_index+k];
		  operand_value_type =
			TI_ISA_Op_Result (operands_info, num_op_results+k);
		  FmtAssert(TI_ISA_Valtyp_Num_Regs(operand_value_type)==1,
			("Inconsistent usage of multi-reg operand for %s",
			opcode_name));
		}
		num_op_results += (isa_num_regs-1);
	      }
	    }
	    num_op_results++;
	  }

	  if (TN_is_register(out_operands[code_opnd_index])) {
	    FmtAssert(TN_register_class(out_operands[code_opnd_index]) ==
			  TI_ISA_Valtyp_Regclass(operand_value_type),
			  ("Operand register file mis-match\n"));
	  }
	  code_opnd_index+= operand_index_increment;
        }
	/* now handle the input state(s) */
	while (num_op_operands<TI_ISA_Op_Operands(operands_info)) {
	  operand_value_type =
		TI_ISA_Op_Operand (operands_info, num_op_operands);
	  if (TI_ISA_Valtyp_Regclass(operand_value_type)==
	      TI_ISA_Regclass_Special()) {
	    ISA_REGSUBCLASS core_state_id =
		TI_ISA_Valtyp_Regsubclass(operand_value_type);
	    op_operands[num_op_operands] =
		Build_Dedicated_TN(
			TI_ISA_Regclass_Special(),
			REGISTER_MIN+TI_ISA_Regsubclass_Member(
				TI_ISA_Regsubclass_Info(core_state_id),0),
			0);
	  } else {
	    ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(operand_value_type);
	    FmtAssert(TI_ISA_Regclass_Is_State(rc), ("Missing operand"));
	    ISA_REGSUBCLASS state_id =
		TI_ISA_Valtyp_Regsubclass(operand_value_type);
	    op_operands[num_op_operands] =
		Build_Dedicated_TN(
			rc,
			REGISTER_MIN+TI_ISA_Regsubclass_Member(
				TI_ISA_Regsubclass_Info(state_id),0),
			0);
	  }
	  num_op_operands++;
	}
	/* now handle the result state(s) */
	while (num_op_results<TI_ISA_Op_Results(operands_info)) {
	  operand_value_type =
		TI_ISA_Op_Result (operands_info, num_op_results);
	  FmtAssert(TI_ISA_Valtyp_Is_Register(operand_value_type),
		("Missing result"));
	  if (TI_ISA_Valtyp_Regclass(operand_value_type)==
	      TI_ISA_Regclass_Special()) {
	    ISA_REGSUBCLASS core_state_id =
		TI_ISA_Valtyp_Regsubclass(operand_value_type);
	    op_results[num_op_results] =
		Build_Dedicated_TN(
			TI_ISA_Regclass_Special(),
			REGISTER_MIN+TI_ISA_Regsubclass_Member(
				TI_ISA_Regsubclass_Info(core_state_id),0),
			0);
	  } else {
	    ISA_REGCLASS rc = TI_ISA_Valtyp_Regclass(operand_value_type);
	    FmtAssert(TI_ISA_Regclass_Is_State(rc), ("Missing result"));
	    ISA_REGSUBCLASS state_id =
		TI_ISA_Valtyp_Regsubclass(operand_value_type);
	    op_results[num_op_results] =
		Build_Dedicated_TN(
			rc,
			REGISTER_MIN+TI_ISA_Regsubclass_Member(
				TI_ISA_Regsubclass_Info(state_id),0),
			0);
	  }
	  num_op_results++;
	}
	OP* op = Mk_VarOP(top, num_op_results, num_op_operands,
		       op_results, op_operands);
	bool needs_extw = xt_flush_tieport && OP_extern_effects(op);
	bool needs_extw_pseudo = !xt_reorder_tieport && !xt_flush_tieport &&
				  OP_extern_effects(op);

	if (needs_extw || needs_extw_pseudo) {
	  OP* extw_op = Mk_OP(needs_extw? TOP_extw:TOP_extw_pseudo,
			  	0, 0, NULL, NULL);
          OPS_Append_Op(ops, extw_op);
          OPS_Append_Op(ops, op);
	  extw_op = Mk_OP(needs_extw? TOP_extw: TOP_extw_pseudo,
			  	0, 0, NULL, NULL);
          OPS_Append_Op(ops, extw_op);
	} else
          OPS_Append_Op(ops, op);

      } else {
	/* a tie macro in a tie macro */
	TIE_MACRO_p a_macro =
			tie_info->tie_macro(tie_macro->inst_opcode_name(i));
	FmtAssert(a_macro,
		("Missing information for TIE intrinsic %s",
		 tie_macro->inst_opcode_name(i)));
	if (a_macro==NULL) return 0;
	if (a_macro->num_protos()>TI_TIE_OPERANDS_MAX) {
	  fprintf(stderr,"Too many (>%d) tie macro arguments for %s\n",
		      TI_TIE_OPERANDS_MAX, a_macro->name());
	  return 0;
	}

	/* now expand one more level */
	expand_tie_macro_to_ops(ops, a_macro->name(), out_operands);
      }
    }

    return 1;

}

int expand_tie_macro_to_ops(
		     OPS* const ops,
                     const char* macro_name,
                     TN* const in_operands[])
{
  const TIE_MACRO_p tie_macro = tie_info->tie_macro(macro_name);
  int res = expand_tie_macro_rec(ops, tie_macro, in_operands);
  if (res)
    mark_copies(ops);
  return res;
}

int expand_tie_macro_to_ops(
		     OPS* const ops,
                     const TIE_MACRO_p tie_macro,
                     TN* const in_operands[])
{
  int res = expand_tie_macro_rec(ops, tie_macro, in_operands);
  if (res)
    mark_copies(ops);
  return res;
}

int expand_tie_macro_to_ops(
		     OPS* const ops,
                     const TIE_MACRO_ID macro_id,
                     TN* const in_operands[])
{
  if (macro_id < 0)
    return 0;

  const TIE_MACRO_p tie_macro=tie_info->tie_macro(macro_id);
  int res = expand_tie_macro_rec(ops, tie_macro, in_operands);
  if (res)
    mark_copies(ops);
  return res;
}

extern TN* Tie_Move_To_Result( TN *result, TN *val, OPS *ops )
{
  OP *last_op = OPS_last(ops);
  TYPE_ID tgt_mtype = TN_mtype(result);
  TYPE_ID src_mtype = TN_mtype(val);

  FmtAssert(TN_register_class(result)==TN_register_class(val),
  	    ("TN register class mis-matches"));

  FmtAssert(MTYPE_is_tie(tgt_mtype) || MTYPE_is_tie(src_mtype) ||
	    MTYPE_is_xtbool(tgt_mtype) || MTYPE_is_xtbool(src_mtype),
	    ("Wrong TN mtype"));

  if (tgt_mtype != src_mtype) {
    if (TN_is_dedicated(result))
      tgt_mtype = src_mtype;
    else if (TN_is_dedicated(val))
      src_mtype = tgt_mtype;
  }

  FmtAssert(tgt_mtype == src_mtype,("TN mtype mis-matches"));

  TN* in_operands[2];
  TYPE_ID tid = Mtype_To_Tie_Type_Id(tgt_mtype);
  TIE_MACRO_p tie_macro = tie_info->ctype_move_macro(tid);
  in_operands[0]=result;
  in_operands[1]=val;
  expand_tie_macro_to_ops( ops, tie_macro->id(), in_operands);

  /* If the move prototype is implemented with a single instruction,
     and that instruction is recognized as a copy, then mark it as a
     copy. Because it is the only instruction in the prototype, we can
     always mark it as a copy, even if it doesn't always have to
     operate as a copy (e.g. addi acts as a copy when the immediate is
     0). If there is more than one instruction, then mark as copies
     those that must always be copies. */

  OP *pcopy = OPS_last(ops);
  if ((pcopy != NULL) && (OP_prev(pcopy) == last_op) &&
      (TI_ISA_Copy_Operand(OP_code(pcopy), FALSE) >= 0))
    Set_OP_copy(pcopy);
  else
    mark_copies(ops);
  
  return result;
}


/* Return a list in 'lcs' of litclasses that represent the range of
   immediates that are acceptable for 'macro' operand
   'opnd_idx'. LC_UNDEFINED in the list indicates that only immediate
   0 is allowed. If all immediates are acceptable, return NULL. */
TIE_LITCLASS_LIST *
tie_offset_litclass (const TIE_MACRO_p macro, const UINT opnd_idx)
{
  static TIE_LITCLASS_LIST lc_undef(LC_UNDEFINED, NULL);

  /* If 'opnd_idx' does not refer to an immediate operand, then return
     'lc_undef', indicating that only immediate 0 is allowed. */

  if ((opnd_idx >= macro->num_protos()) || !macro->proto_is_immed(opnd_idx))
    return &lc_undef;

  return macro->litclass_usage(opnd_idx);
}

// ====================================================================
//
// the code below is for identifying TIE branches and re-write trees
// to enable TIE branches generation
//
// ====================================================================

// main algorithm:
//
//	The entry point is convert_tie_branches() which calls
//	convert_tie_branch_process() to recursively
//	traverse the tree to find out where are the TIE branch
//	intrinsics and their corresponding conditional branches. The conditional
//	branches can be in one of the following form
//
//		TRUEBR ( I4I4EQ ( I4LDID preg, I4INTCONST ) )
//		TRUEBR ( I4I4NE ( I4LDID preg, I4INTCONST ) )
//		FALSEBR ( I4I4EQ ( I4LDID preg, I4INTCONST ) )
//		FALSEBR ( I4I4NE ( I4LDID preg, I4INTCONST ) )
//
//	and we will handle all of them by flipping FALSEBR to TRUEBR and/or
//	flipping NE to EQ and generating additional GOTO if necessary.
//
//	Each TIE branch intrinsic should be followed with an STID that
//	assigns a PREG with the return I4 value. This PREG is the link between
//	the intrinsic and the conditional branches.
//
//	During the traversal, when we see a TIE branch intrinsic, we call
//	register_tie_branch() to add it in a DYN_ARRAY<TIE_BRANCH*>
//	tie_branches[]. We also add an entry in the map tie_branch_return_pregs
//	that returns an index into tie_branches[] when given a PREG_NUM.
//	When a TRUEBR or FALSEBR is encountered and is in one of the forms
//	listed above, we lookup the map tie_branch_return_pregs to get the
//	TIE_BRANCH and record the branch as one of the target.
//
//	Should any check fails, e.g. the case value is not in 1..num_labels
//	or if there are multiple conditional branches using the same case
//	value, we set_return_value_needed(TRUE) which will force the return
//	I4 value be generated in additional to the TIE branch intrinsic.
//	Similarly, if the return values are used in places other than the
//	conditional branches, it also forces the return value be generated.
//	Another requirement is that the conditional branches have to follow
//	immedately the associated TIE branch intrinsic with maybe only
//	STID/ISTORE in between.
//
//	After the traversal is done, for each TIE branch that is in the right
//	form, we change the label parameters to encode the label numbers
//	in the conditional branches. The label numbers have to be non-zero.
//	We delete the conditional branches as the TIE intrinsic will do
//	the branches. The STID for the return I4 value will also be deleted.
//	The code selection phase (aka isel.pat) should now be able
//	to generate the TIE intrinsics.
//
//	For the TIE branches that does not have the right form, we will add
//	GOTOs and LABELs to materialize the return values. So the code generation
//	for both the successful or the failed cases should be the same
//	in isel.pat.


class TIE_BRANCH {
#define MAX_TIE_BRANCH_TARGETS	10
private:
  WN*		_branch;	// should be an I4 intrinsic call
  WN*		_parent_block;	// should be a block
  WN*		_targets[MAX_TIE_BRANCH_TARGETS]; // should be all TRUEBRs
  TIE_MACRO_p	_tie_macro;
  BOOL		_return_value_needed;
  BOOL		_is_complementary_comparison;

public:
  TIE_BRANCH()	{ _branch=NULL; _tie_macro=NULL; _parent_block = NULL;
		  for (int i=0; i<MAX_TIE_BRANCH_TARGETS; i++) _targets[i]=NULL;
		  _return_value_needed = FALSE;
		  _is_complementary_comparison = FALSE;
		}
  ~TIE_BRANCH() {}
  void	set_branch(WN* branch)
		{
		  FmtAssert(WN_opcode(branch)==OPC_I4INTRINSIC_CALL,
			    ("Invalid opcode"));
		  _branch = branch;
		}
  WN*	branch() const { return _branch; }
  void	set_parent_block(WN* block)
		{
		  FmtAssert(WN_opcode(block)==OPC_BLOCK, ("Invalid opcode"));
		  _parent_block = block;
		}
  WN*	parent_block() const { return _parent_block; }
  void	set_tie_macro(TIE_MACRO_p macro)
		{ _tie_macro = macro;
		  FmtAssert(macro->num_labels()<MAX_TIE_BRANCH_TARGETS,
			    ("Too many labels (>%d) for tie branch macro %s\n",
			      MAX_TIE_BRANCH_TARGETS, macro->name()));
		}
  TIE_MACRO_p	tie_macro() const { return _tie_macro; }
  void	set_target(const int i, WN* target)
		{
		  FmtAssert(i<MAX_TIE_BRANCH_TARGETS, ("Invalid label index"));
		  FmtAssert(i<=num_targets(), ("Invalid label index"));
		  FmtAssert(i>=0, ("Invalid label index"));
		  FmtAssert(WN_opcode(target)==OPC_TRUEBR, ("Invalid opcode"));
		  _targets[i]=target;
		}
  WN*	target(const int i) const
		{ FmtAssert(i<MAX_TIE_BRANCH_TARGETS, ("Invalid label index"));
		  FmtAssert(i<=num_targets(), ("Invalid label index"));
		  FmtAssert(i>=0, ("Invalid label index"));
		  return _targets[i];
		}
  BOOL	is_good() const
		{ if (_tie_macro==NULL)
		    return FALSE;

		  BOOL is_bad = FALSE;
		  if (num_targets()==1 &&
		      ((_targets[0]!=NULL && _targets[1]==NULL) ||
		       (_targets[0]==NULL && _targets[1]!=NULL))) {
		    // for single label case, we can also handle the branch
		    // on complement value
		  } else
		    for (int i=1; i<=_tie_macro->num_labels(); i++)
		      if (_targets[i]==NULL)
			is_bad = TRUE;

		   return !is_bad && (_branch!=NULL) && !_return_value_needed;
		}
  BOOL	is_bad() const { return !is_good(); }
  void	set_return_value_needed(BOOL needed)
		{ _return_value_needed = needed; }
  void	set_is_complementary_comparison(BOOL value)
		{ _is_complementary_comparison = value; }
  BOOL	is_complementary_comparison() { return _is_complementary_comparison; }
  int	num_targets() const {
		  FmtAssert(_tie_macro!=NULL, ("Missing TIE intrinsic"));
		  FmtAssert(_tie_macro->num_labels()!=0, ("bad TIE branch intrinsic"));
		  return _tie_macro->num_labels();
		}

#undef MAX_TIE_BRANCH_TARGETS
};

DYN_ARRAY<TIE_BRANCH*>* tie_branches;
typedef ID_MAP<PREG_NUM, int> preg_int_map_t;
preg_int_map_t* tie_branch_return_pregs;
MEM_POOL MEM_tie_branch_pool;
BOOL Trace_TIE_branch=FALSE;

#define INVALID_TIE_BRANCH_ID		-1
#define FORWARD_REF_TIE_BRANCH_ID	-2

static WN*
wn_new_label() {

  LABEL_IDX new_label;
  LABEL& lab = New_LABEL(CURRENT_SYMTAB, new_label);

  // create label name
  char *name = (char *) alloca (strlen(".L..") + 8 + 8 + 1);
  sprintf(name, ".L%s%d%s%d", Label_Name_Separator, Current_PU_Count(),
        	      Label_Name_Separator, new_label);
  LABEL_Init (lab, Save_Str(name), LKIND_DEFAULT);
  WN* label_wn = WN_CreateLabel(new_label, 0, NULL);

  return label_wn;

}

static void
register_tie_branch(WN* branch, WN* block) {

  if (WN_opcode(branch)!=OPC_I4INTRINSIC_CALL)
    return;
  if (INTRN_is_tie_intrinsic(WN_intrinsic(branch))==FALSE)
    return;

  TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(WN_intrinsic(branch));
  TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
  if (tie_macro->is_conditional_branch()==FALSE)
    return;

  ST* i4preg_st = MTYPE_To_PREG(MTYPE_I4);
  PREG_NUM return_value_preg = -1;

  WN* stmt = WN_next(branch);

  if (WN_opcode(stmt)==OPC_I4STID) {
    WN* ldid = WN_kid0(stmt);
    if (WN_opcode(ldid)==OPC_I4I4LDID &&
	WN_st(ldid)==Tie_Output_Volatile_Preg &&
	WN_st(stmt)==i4preg_st &&
	WN_offset(ldid)== -1)
      return_value_preg = WN_offset(stmt);
  }
  if (return_value_preg == -1) {
    if (Trace_TIE_branch) {
      fprintf(TFile,
		"TIE:Found bad tie branch with missing return value:\n");
      fdump_tree(TFile,branch);
    }
    return;
  }

  BOOL return_value_used=FALSE;
  int idx = tie_branch_return_pregs->Lookup(return_value_preg);
  if (idx != INVALID_TIE_BRANCH_ID) {
    return_value_used=TRUE;
  }

  idx = tie_branches->Newidx();
  
  tie_branch_return_pregs->Insert(return_value_preg, idx);

  (*tie_branches)[idx]=CXX_NEW(TIE_BRANCH(),&MEM_tie_branch_pool);
  (*tie_branches)[idx]->set_branch(branch);
  (*tie_branches)[idx]->set_parent_block(block);
  (*tie_branches)[idx]->set_tie_macro(tie_macro);

  if (return_value_used)
    (*tie_branches)[idx]->set_return_value_needed(TRUE);

}

static void
convert_tie_branch_process(WN* root, WN* block) {

  if (root==NULL)
    return;

  OPCODE opc = WN_opcode(root);

  WN* kid0 = WN_kid0(root);

  if ((opc==OPC_TRUEBR || opc==OPC_FALSEBR) &&
      (WN_opcode(kid0)==OPC_I4I4EQ || WN_opcode(kid0)==OPC_I4I4NE)) {

    WN* cmp_kid0 = WN_kid0(kid0);
    WN* cmp_kid1 = WN_kid1(kid0);

    if (WN_opcode(cmp_kid0)==OPC_I4I4LDID &&
	WN_opcode(cmp_kid1)==OPC_I4INTCONST &&
	WN_st(cmp_kid0) == MTYPE_To_PREG(MTYPE_I4)) {

      PREG_NUM preg = WN_offset(cmp_kid0);
      INT64 label = WN_const_val(cmp_kid1);
      int idx = tie_branch_return_pregs->Lookup(preg);

      if (idx != INVALID_TIE_BRANCH_ID && idx != FORWARD_REF_TIE_BRANCH_ID) {
        TIE_BRANCH* tb = (*tie_branches)[idx];
        TIE_MACRO_p tie_macro=tb->tie_macro();
	if (label>0 && label<=tie_macro->num_labels()) {
	  if (tb->target(label)==NULL) {

	    /* now we need to get the tree right */

  	    if (opc==OPC_TRUEBR && WN_opcode(kid0)==OPC_I4I4EQ) {
	      /* this is what we want */
	    } else if (opc==OPC_FALSEBR && WN_opcode(kid0)==OPC_I4I4NE) {
	      WN_set_opcode(root, OPC_TRUEBR); opc = OPC_TRUEBR;
	      WN_set_opcode(kid0, OPC_I4I4EQ);
	    } else {

	      /* need to create a label */
	      WN* label_wn = wn_new_label();

	      LABEL_IDX old_label = label;
	      LABEL_IDX new_label = WN_label_number(label_wn);

	      FmtAssert(block!=NULL,("Missing block"));
	      WN_INSERT_BlockAfter(block, root, label_wn);

	      WN* goto_wn = WN_CreateGoto(old_label);
	      WN_INSERT_BlockAfter(block, root, goto_wn);

	      WN_set_opcode(root, OPC_TRUEBR); opc = OPC_TRUEBR;
	      WN_label_number(root)=new_label;
	      WN_set_opcode(kid0, OPC_I4I4EQ);
	      label = new_label;
	    }
	    tb->set_target(label,root);

	    // the return is needed to avoid the LDID to be prcessed again
	    return;
	  } else
	    tb->set_return_value_needed(TRUE);
	} else if (((opc==OPC_FALSEBR && WN_opcode(kid0)==OPC_I4I4EQ) ||
		    (opc==OPC_TRUEBR && WN_opcode(kid0)==OPC_I4I4NE)) &&
		   label==0 && tb->num_targets()==1 &&
		   tb->target(1)==NULL) {
	  // this is to handle the special case where if (tie_br()) would
	  // generate
	  //	      FALSEBR (I4EQ (tie_br, 0)), or
	  //	      TRUEBR  (I4NE (tie_br, 0))
	  // we will remember this branch as if it is
	  //          TRUEBR (I4NE (tie_br, 0))
	  // and use it if all other checks go well
	  WN_set_opcode(root, OPC_TRUEBR); opc = OPC_TRUEBR;
	  WN_set_opcode(kid0, OPC_I4I4NE);
	  tb->set_target(1,root);
	  tb->set_is_complementary_comparison(TRUE);

	  // the return is needed to avoid the LDID to be prcessed again
	  return;
	} else if (((opc==OPC_TRUEBR && WN_opcode(kid0)==OPC_I4I4EQ) ||
		    (opc==OPC_FALSEBR && WN_opcode(kid0)==OPC_I4I4NE)) &&
		   label==0 && tb->num_targets()==1 &&
		   tb->target(1)==NULL) {
	  // this is to handle the special case where if (tie_br()) would
	  // generate
	  //	      TRUEBR  (I4EQ (tie_br, 0)), or
	  //	      FALSEBR (I4NE (tie_br, 0))
	  // we will remember this branch as if it is
	  //          TRUEBR (I4EQ (tie_br, 0))
	  // and use it if all other checks go well
	  WN_set_opcode(root, OPC_TRUEBR); opc = OPC_TRUEBR;
	  WN_set_opcode(kid0, OPC_I4I4EQ);
	  tb->set_target(0,root);

	  // the return is needed to avoid the LDID to be prcessed again
	  return;
	} else
	  tb->set_return_value_needed(TRUE);
      } else
	tie_branch_return_pregs->Insert(preg, FORWARD_REF_TIE_BRANCH_ID);
    }
  } else if (opc==OPC_I4INTRINSIC_CALL && 
	     INTRN_is_tie_intrinsic(WN_intrinsic(root))==TRUE) {
    register_tie_branch(root,block);
  } else if (WN_opcode(root)==OPC_I4I4LDID &&
	    WN_st(root) == MTYPE_To_PREG(MTYPE_I4)) {
    // see if there is any non-conforming use of the branch result
    PREG_NUM preg = WN_offset(root);
    int idx = tie_branch_return_pregs->Lookup(preg);
    if (idx==INVALID_TIE_BRANCH_ID)
      tie_branch_return_pregs->Insert(preg, FORWARD_REF_TIE_BRANCH_ID);
    else if (idx!=FORWARD_REF_TIE_BRANCH_ID) {
      TIE_BRANCH* tb = (*tie_branches)[idx];
      tb->set_return_value_needed(TRUE);
    }
  } else if (WN_opcode(root)==OPC_I4STID &&
	    WN_st(root) == MTYPE_To_PREG(MTYPE_I4)) {
    // see if there is any non-conforming def of the branch result
    PREG_NUM preg = WN_offset(root);
    int idx = tie_branch_return_pregs->Lookup(preg);
    if (idx==INVALID_TIE_BRANCH_ID)
      tie_branch_return_pregs->Insert(preg, FORWARD_REF_TIE_BRANCH_ID);
    else if (idx!=FORWARD_REF_TIE_BRANCH_ID) {
      TIE_BRANCH* tb = (*tie_branches)[idx];
      // there should be only one STID of the preg which is right after
      // the TIE branch
      if (WN_next(tb->branch())!=root)
        tb->set_return_value_needed(TRUE);
    }
  }

  if (opc==OPC_BLOCK) {
    WN* stmt = WN_first(root);
    while (stmt) {
      convert_tie_branch_process(stmt, root);
      stmt = WN_next(stmt);
    }
  } else {
    for (int i=0; i<WN_kid_count(root); i++) {
      WN* stmt = WN_kid(root,i);
      convert_tie_branch_process(stmt, NULL);
    }
  }
}

void convert_tie_branches(WN* pu) {

  MEM_POOL_Initialize (&MEM_tie_branch_pool, "tie_branch_pool",
		       TRUE /* zero-memory */);

  MEM_POOL_Push(&MEM_tie_branch_pool);

  if (Get_Trace(TP_TIE, TP_TIE_branch))
    Trace_TIE_branch=TRUE;

  tie_branches = CXX_NEW(DYN_ARRAY<TIE_BRANCH*>(&MEM_tie_branch_pool),&MEM_tie_branch_pool);
  tie_branch_return_pregs= CXX_NEW(
	preg_int_map_t(128,INVALID_TIE_BRANCH_ID,&MEM_tie_branch_pool,FALSE),
	&MEM_tie_branch_pool);
  tie_branch_return_pregs->Init();

  convert_tie_branch_process(pu,NULL);

  for (int i=0; i<=tie_branches->Lastidx(); i++) {
    TIE_BRANCH* tb = (*tie_branches)[i];
    WN* branch = tb->branch();
    WN* stid = WN_next(branch);
    WN* block = tb->parent_block();

    // for cloning stmts between intrinsic and branch
    WN* clone_begin = NULL;
    WN* clone_end = NULL;
    BOOL branch_seen = FALSE;

    TIE_MACRO_p tie_macro = tb->tie_macro();
    BOOL failed = FALSE;
    if (tb->is_good()) {

      int num_targets_found = 0;
      WN* stmt = WN_next(branch);
      stmt = WN_next(stmt);	// skip the return value stid
      while (stmt && !failed && num_targets_found<tb->num_targets()) {
	if (WN_opcode(stmt)==OPC_TRUEBR) {
	  int target_num = WN_const_val(WN_kid1(WN_kid0(stmt)));
	  if (tb->target(target_num)==stmt)
	    num_targets_found++;
	  else if (target_num==0 && tb->is_complementary_comparison() &&
		   tb->num_targets()==1 && tb->target(1)==stmt) {
	    num_targets_found++;
	  } else {
	    failed = TRUE;
	    if (Trace_TIE_branch) {
	      fprintf(TFile,
		"TIE:Found bad tie branch with duplicate targets:\n");
	      fdump_tree(TFile,tb->branch());
	      fprintf(TFile,"target 1:\n");
	      fdump_tree(TFile,stmt);
	      fprintf(TFile,"target 2:\n");
	      fdump_tree(TFile,tb->target(target_num));
	    }
	    break;
	  }
	  branch_seen=TRUE;
	} else if (OPCODE_is_non_scf(WN_opcode(stmt))) {
	  failed = TRUE;
	  if (Trace_TIE_branch) {
	    fprintf(TFile,
		"TIE:Found bad tie branch with extra branch inserted before branch:\n");
	    fdump_tree(TFile,tb->branch());
	    fprintf(TFile,"extra branch:\n");
	    fdump_tree(TFile,stmt);
	  }
	  break;
	} else if (branch_seen) {
	  // no other statements is allowed once a TRUEBR statment
	  // is encountered
	  failed = TRUE;
	  if (Trace_TIE_branch) {
	    fprintf(TFile,
		"TIE:Found bad tie branch with extra statement inserted before branch:\n");
	    fdump_tree(TFile,tb->branch());
	    fprintf(TFile,"extra statment:\n");
	    fdump_tree(TFile,stmt);
	  }
	  break;
	} else {
	  if (clone_begin==NULL)
	    clone_begin = stmt;
	  clone_end = stmt;
	}
	stmt = WN_next(stmt);
      }
      if (!failed && num_targets_found != tb->num_targets()) {
	failed = TRUE;
	if (Trace_TIE_branch) {
	  fprintf(TFile,
		"TIE:Found bad tie branch with too few targets:\n");
	  fdump_tree(TFile,tb->branch());
	  fprintf(TFile, "Missing targets --");
	  for (int i=1; i<=tb->num_targets(); i++)
	    if (tb->target(i)==NULL)
	      fprintf(TFile, " %d", i);
	  fprintf(TFile, "\n");
	}
      }
    } else {
      failed = TRUE;
      if (Trace_TIE_branch) {
	fprintf(TFile,
		"TIE:Found non-confirming tie branch:\n");
	fdump_tree(TFile,tb->branch());
      }
    }

    if (failed==FALSE) {
      if (Trace_TIE_branch) {
        fprintf(TFile,"TIE:Found good tie branch:\n");
        fprintf(TFile,"TIE:%s cloning\n",(clone_begin)?"Needs":"Does not need");
        fdump_tree(TFile,tb->branch());
      }

      WN* clone_block = NULL;
      if (clone_begin) {
	// the cloning starting point should be right after the STID of
	// return value
	FmtAssert(WN_prev(clone_begin)==stid,("Missing return value"));
	FmtAssert(clone_end,("Missing cloning end point"));
	clone_block = WN_CreateBlock();
	while (clone_begin!=clone_end) {
	  WN* next=WN_next(clone_begin);
	  WN_EXTRACT_FromBlock(block,clone_begin);
	  WN_INSERT_BlockLast(clone_block,clone_begin);
	  clone_begin=next;
	}
	WN_EXTRACT_FromBlock(block,clone_begin);
	WN_INSERT_BlockLast(clone_block,clone_begin);
      }
      if (tb->num_targets()==1 && tb->target(0)!=NULL) {
	int label_proto_index = tie_macro->label_proto_index(1);
	WN* arg_wn = WN_kid0(WN_kid(branch, label_proto_index));

	WN* truebr = tb->target(0);
	FmtAssert(WN_opcode(truebr)==OPC_TRUEBR, ("Expecting branch"));
	// force a label name to be created in case of tracing
	(void)Get_WN_Label(truebr);
	LABEL_IDX continue_target_label=WN_label_number(truebr);
	WN_DELETE_FromBlock(block,truebr);

	/* need to create a label */
	WN* label_wn = wn_new_label();
	WN_INSERT_BlockAfter(block,branch,label_wn);
	LABEL_IDX target_label=WN_label_number(label_wn);
	WN_const_val(arg_wn)=target_label;

	WN* goto_wn = WN_CreateGoto(continue_target_label);
	WN_INSERT_BlockAfter(block, branch, goto_wn);

      } else {
	for (int i=1; i<=tie_macro->num_labels(); i++) {
	  int label_proto_index = tie_macro->label_proto_index(i);
	  WN* arg_wn = WN_kid0(WN_kid(branch, label_proto_index));
	  FmtAssert(WN_opcode(arg_wn)==OPC_I4INTCONST,
		    ("Expecting integer constant"));
	  WN* truebr = tb->target(i);
	  FmtAssert(WN_opcode(truebr)==OPC_TRUEBR, ("Expecting branch"));
	  // force a label name to be created in case of tracing
	  (void)Get_WN_Label(truebr);
	  LABEL_IDX target_label=WN_label_number(truebr);
	  WN_const_val(arg_wn)=target_label;
	  WN_DELETE_FromBlock(block,truebr);
	}
      }
      if (clone_block) {

	WN* fall_thru_label_wn = wn_new_label();
	LABEL_IDX fall_thru_label = WN_label_number(fall_thru_label_wn);
	WN_INSERT_BlockAfter(block,branch,fall_thru_label_wn);

	for (int i=1; i<=tie_macro->num_labels(); i++) {
	  int label_proto_index = tie_macro->label_proto_index(i);
	  WN* arg_wn = WN_kid0(WN_kid(branch, label_proto_index));
	  LABEL_IDX target_label=WN_const_val(arg_wn);

	  WN* new_block = WN_COPY_Tree_With_Map(clone_block);
	  WN* goto_wn = WN_CreateGoto(target_label);
	  WN_INSERT_BlockLast(new_block,goto_wn);
	  WN* label_wn = wn_new_label();
	  WN_INSERT_BlockFirst(new_block,label_wn);
	  WN_const_val(arg_wn)=WN_label_number(label_wn);

	  WN_INSERT_BlockAfter(block,branch,new_block);
	}
	{
	  // for the fall thru case
	  WN* goto_wn = WN_CreateGoto(fall_thru_label);
	  WN_INSERT_BlockLast(clone_block,goto_wn);
	  WN_INSERT_BlockAfter(block,branch,clone_block);
	}
      }
    } else {

      WN* label_wn = wn_new_label();
      WN_INSERT_BlockBefore(block,stid,label_wn);
      LABEL_IDX continue_target_label = WN_label_number(label_wn);

      for (int i=tie_macro->num_labels(); i>0; i--) {
	int label_proto_index = tie_macro->label_proto_index(i);
	WN* arg_wn = WN_kid0(WN_kid(branch, label_proto_index));
	FmtAssert(WN_opcode(arg_wn)==OPC_I4INTCONST,
		  ("Expecting integer constant"));

	WN* goto_wn = WN_CreateGoto(continue_target_label);
	// force a label name to be created in case of tracing
	(void)Get_WN_Label(goto_wn);
	WN_INSERT_BlockAfter(block, branch, goto_wn);

	WN* stid_wn = WN_COPY_Tree(stid);
	WN_DELETE_Tree(WN_kid0(stid_wn));
	WN_kid0(stid_wn) = WN_CreateIntconst (OPC_I4INTCONST, i);
	WN_INSERT_BlockAfter(block, branch, stid_wn);

	WN* label_wn = wn_new_label();
	WN_INSERT_BlockAfter(block,branch,label_wn);
	LABEL_IDX target_label=WN_label_number(label_wn);
	WN_const_val(arg_wn)=target_label;
      }

      // now create code for fall thru case
      WN* goto_wn = WN_CreateGoto(continue_target_label);
      WN_INSERT_BlockAfter(block, branch, goto_wn);

      WN* stid_wn = WN_COPY_Tree(stid);
      WN_DELETE_Tree(WN_kid0(stid_wn));
      WN_kid0(stid_wn) = WN_CreateIntconst (OPC_I4INTCONST, 0);
      WN_INSERT_BlockAfter(block, branch, stid_wn);

    }

    // delete the STID that gets the I4 return value
    WN_DELETE_FromBlock(block,stid);

  }

  tie_branches->Free_array();
  CXX_DELETE(tie_branch_return_pregs, &MEM_tie_branch_pool);
  CXX_DELETE(tie_branches, &MEM_tie_branch_pool);

  MEM_POOL_Pop(&MEM_tie_branch_pool);
  MEM_POOL_Delete (&MEM_tie_branch_pool);
}

#undef INVALID_TIE_BRANCH_ID
#undef FORWARD_REF_TIE_BRANCH_ID

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
