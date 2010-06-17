//
// TIE intrinsic support
//

// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/com/tie.cxx#2 $


/*

  Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.

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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cxx_memory.h"
#include "errors.h"
#include "xtmap.h"
#include "intrn_info.h"		/* needed for TIE intrinsics initialization */
#include "tie.h"
#include "libti.h"
#include <alloca.h>
#ifdef BACK_END
#include "tracing.h"
#endif
#include "config.h"
/* declared in
   common/targ_info/static/common/com/xtensa/config_targ_options.h */
/* needed to determine if xtfloat should be substituted with float */
extern DLL_SHARED BOOL xt_hard_float;
/* needed to determine if the target register file for float to int conversion */
extern DLL_SHARED BOOL xt_vectorfpu2005;

// was Pointer_Mtype but it is initialized in config_targ.cxx after
// uses such as in Base_Tie_Type_Name_To_Mtype()
CLASS_INDEX pointer_mtype = MTYPE_U4;

#ifdef sun
extern "C" {
/* strsep is not standard */
extern char * strsep(char **str, const char *delim);
}
#endif

TIE_INFO* tie_info=NULL;

TIE_INFO::TIE_INFO():
  _ctypes(NULL),
  _macros(NULL),
  _num_ctypes(0),
  _num_macros(0),
  _has_tie_branch_macro(TRUE),
  _xtfloat_ctype(XTENSA_UNDEFINED),
  _xtfloat_move_opcode(XTENSA_UNDEFINED),
  _xtfloat_ar_converts(false),
  _num_packed_mtypes(0),
  _max_num_output(0),
  _immed_to_reg_macro_map(NULL),
  _reg_to_immed_macro_map(NULL)

{
  MEM_POOL_Initialize(&_pool, "tie_info_pool", TRUE);
  MEM_POOL_Push(&_pool);

  _isa = TI_TIE_Libisa_Info();
  init_user_types();
  init_xtfloat_info();

  /* Xcalibur TIE intrinsics initialization */
  Intrinsic_Last += _num_macros;
}

TIE_INFO::~TIE_INFO()
{
  MEM_POOL_Pop(&_pool);
  MEM_POOL_Delete(&_pool);
}

static const char * 
demangle_tie_name(const char * mangled_name, const char * package_name);

/* Return the mtype index into 'types' given type 'name'. */

TYPE_ID TIE_INFO::mtype_id (const char *name) const
{
  if (name==NULL)
    return MTYPE_UNKNOWN;

  TYPE_ID mtype = Base_Tie_Type_Name_To_Mtype(name);

  if (mtype != MTYPE_UNKNOWN)
    return mtype;

  int i = xtensa_ctype_lookup(_isa, name);
  if (i != XTENSA_UNDEFINED)
    return Tie_Type_Id_To_Mtype(i);

  return MTYPE_UNKNOWN;
}

int
TIE_INFO::get_packed_mtype_index(TYPE_ID packed_mtype) const {

  for (int i=0; i<_num_packed_mtypes; i++) {
    if (_packed_mtype[i].packed_mtype()==packed_mtype)
      return i;
  }
  return -1;
}

int
TIE_INFO::num_scalar_mtypes(TYPE_ID packed_mtype) const {

  if (MTYPE_is_tie_packed(packed_mtype)==FALSE)
    return -1;

  int i = get_packed_mtype_index(packed_mtype);

  FmtAssert(i!=-1,("Un-recognized packed mtype (%d) found",packed_mtype));

  return _packed_mtype[i].num_scalar_mtypes();
}

TYPE_ID
TIE_INFO::get_scalar_mtype(TYPE_ID packed_mtype, INT index) const {

  if (MTYPE_is_tie_packed(packed_mtype)==FALSE)
    return MTYPE_UNKNOWN;

  int i = get_packed_mtype_index(packed_mtype);

  FmtAssert(i!=-1,("Un-recognized packed mtype (%d) found",packed_mtype));

  if (index>=_packed_mtype[i].num_scalar_mtypes())
    return MTYPE_UNKNOWN;

  return _packed_mtype[i].scalar_mtype(index);
}

TYPE_ID
TIE_INFO::find_output_mtype(TIE_MACRO_p macro) {

  char type_name[10+_max_num_scalar_mtypes*4+1];

  if (macro==NULL || (macro->num_inout_protos()+macro->num_output_protos())==0
      || !macro->is_whirl_intrinsic_op())
    return MTYPE_V;

  if (macro->is_conditional_branch())
    return MTYPE_I4;

  TYPE_ID proto_mtype = MTYPE_V;
  char* p = type_name;
  sprintf(p,".ty_");
  p+=4;
  int num_results=0;
  for (int i=0; i<macro->num_protos(); i++)
    if (!macro->proto_is_in(i)) {
      FmtAssert(num_results<_max_num_scalar_mtypes,
		("Too many (>%d) output argument for %s",
		  _max_num_scalar_mtypes,macro->name()));
      proto_mtype = macro->proto_mtype_id(this,i);
      sprintf(p,"%03d_", proto_mtype);
      num_results++;
      p+=4;
    }

  if (num_results==1) {
    return proto_mtype;
  }

  FmtAssert(num_results>1,
	    ("Expect more than 1 output for TIE intrinsic %s", macro->name()));

  for (int i=0; i<_num_packed_mtypes; i++)
    if (!strcmp(_packed_mtype[i].signature(), type_name)) {
      return _packed_mtype[i].packed_mtype();
  }

  FmtAssert(_num_packed_mtypes<_max_num_packed_mtypes,
	    ("Too many (>%d) TIE intrinsic output signatures",
	     _max_num_packed_mtypes));

  TYPE_ID return_mtype=_packed_mtype[_num_packed_mtypes].init(type_name);

  _num_packed_mtypes++;

  return return_mtype;

}


void
TIE_INFO::init_xtfloat_info ()
{
  _xtfloat_ctype = XTENSA_UNDEFINED;
  _xtfloat_move_opcode = XTENSA_UNDEFINED;
  _xtfloat_ar_converts = false;

  if (!xt_hard_float)
    return;

  /* Look for "xtfloat" C type. */
  for (xtensa_ctype ct = 0; ct < xtensa_isa_num_ctypes(_isa); ct++) {
    const char *name = get_demangled_ctype_name (ct);
    if (!strcmp(name, "xtfloat")) {
      _xtfloat_ctype = ct;
      break;
    }
  }
  
  if (_xtfloat_ctype == XTENSA_UNDEFINED)
    return;
  
  xtensa_proto proto = xtensa_ctype_move_proto(_isa, _xtfloat_ctype);
  FmtAssert(proto != XTENSA_UNDEFINED,
            ("Unable to find move proto for type '%s'",
             xtensa_ctype_name(_isa, _xtfloat_ctype)));
  FmtAssert(xtensa_proto_num_insns(_isa, proto) == 1,
            ("Expected single opcode move proto '%s' for type '%s'",
             xtensa_proto_name(_isa, proto),
             xtensa_ctype_name(_isa, _xtfloat_ctype)));
  
  _xtfloat_move_opcode = xtensa_proto_insn_opcode(_isa, proto, 0);
  FmtAssert(_xtfloat_move_opcode != XTENSA_UNDEFINED,
            ("Unable to find an opcode in proto '%s'",
             xtensa_proto_name(_isa, proto)));

  _xtfloat_ar_converts = !xt_vectorfpu2005;
}


TYPE_ID
TIE_INFO::xtfloat_mtype_id () const
{
  if (_xtfloat_ctype == XTENSA_UNDEFINED)
    return MTYPE_UNKNOWN;
  
  return Tie_Type_Id_To_Mtype(_xtfloat_ctype);
}


TOP
TIE_INFO::xtfloat_move_topcode () const
{
  if (_xtfloat_move_opcode == XTENSA_UNDEFINED)
    return TOP_UNDEFINED;
  
  const char *opcode_name = xtensa_opcode_name(_isa, _xtfloat_move_opcode);
  return TI_TOP_Topcode(opcode_name);
}


void
TIE_INFO::init_user_types (void)
{
  xtensa_isa isa = TI_TIE_Libisa_Info();

  _has_tie_branch_macro = FALSE;
  _num_packed_mtypes = 0;

  /* Initialize a TIE_MACRO object for each macro. */
  _num_macros = TI_TIE_Num_Macros();
  if (_num_macros > 0)
  {
    _macros = CXX_NEW_ARRAY(TIE_MACRO *, _num_macros, &_pool);
    for (int i=0; i<_num_macros; i++) {
      int num_output;
      xtensa_proto xt_proto = i;
      _macros[i] = CXX_NEW(TIE_MACRO(&_pool, isa, xt_proto,i), &_pool);
      if (_macros[i]->is_conditional_branch())
	_has_tie_branch_macro = TRUE;
      num_output =
	_macros[i]->num_output_protos() + _macros[i]->num_inout_protos();
      if (num_output > _max_num_output)
	_max_num_output = num_output;
    }

    /* Now that all the macros are initialized, determine the litclass
       usage of each. We can't do this when first initializing the
       macros, since we need to now about nested macros. */
    for (int i=0; i<_num_macros; i++)
      _macros[i]->init_macro_litclass_usage(this);
  }

  /* Initialize the user-defined type objects. */

  _num_ctypes = TI_TIE_Num_Ctypes();
  if (_num_ctypes > 0)
  {
    _ctypes = CXX_NEW_ARRAY(xtensa_ctype, _num_ctypes, &_pool);
  }
}

void
TIE_MACRO::check_exposed_uses()
{
  int num_proto_operands = xtensa_proto_num_operands(_isa, _macro);
  int num_proto_temps = xtensa_proto_num_tmps(_isa, _macro);
  bool operand_defined[128];
  bool temp_defined[128];
  _operand_live_in = CXX_NEW_ARRAY(bool, num_proto_operands,_pool);
  _temp_live_in = CXX_NEW_ARRAY(bool, num_proto_temps,_pool);
  for (int opnd=0; opnd<num_proto_operands; opnd++) {
    operand_defined[opnd]=false;
    _operand_live_in[opnd]=false;
  }
  for (int tmp=0; tmp<num_proto_temps; tmp++) {
    temp_defined[tmp]=false;
    _temp_live_in[tmp]=false;
  }
  int inst=0;
  int num_insts=xtensa_proto_num_insns(_isa,_macro);
  for (inst=0; inst<num_insts; inst++) {
      xtensa_opcode xt_opc = xtensa_proto_insn_opcode(_isa, _macro, inst);
      int num_args = xtensa_proto_insn_num_args(_isa, _macro, inst);
      int arg, opnd, var_or_tmp, is_tmp;

      for (arg = 0; arg < num_args; arg++) {

	/* ignore immediate arguments */
	if (xtensa_proto_insn_arg_is_immed(_isa, _macro, inst, arg) == 1)
	  continue;

	xtensa_proto_insn_arg_variable(_isa, _macro, inst, arg,
				       &var_or_tmp, &is_tmp);

	opnd = xtensa_proto_insn_arg_to_opnd(_isa, _macro, inst, arg, 0);
	char opnd_inout = xtensa_operand_inout(_isa, xt_opc, opnd);
	if (opnd_inout == 'i' || opnd_inout == 'm') {
	  if (is_tmp) {
	    if (!temp_defined[var_or_tmp])
	      _temp_live_in[var_or_tmp] = true;
	  } else {
	    if (!operand_defined[var_or_tmp])
	      _operand_live_in[var_or_tmp] = true;
	  }
	}
	if (opnd_inout == 'o' || opnd_inout == 'm') {
	  if (is_tmp)
	    temp_defined[var_or_tmp] = true;
	  else
	    operand_defined[var_or_tmp] = true;
	}
      }
  }
}

void
TIE_INFO::init_macro_output_mtypes (void)
{
    for (int i=0; i<_num_macros; i++)
      (void)_macros[i]->output_mtype(this);
}

TYPE_ID
TIE_INFO::TIE_PACKED_MTYPE::init(char* signature)
{
  _signature = strdup(signature);

  char* p = signature+4;
  while (*p!='\0') {
    TYPE_ID mtype = atoi(p);
    FmtAssert(_num_scalar_mtypes<_max_num_scalar_mtypes,
	      ("Too many (>%d) scalar mtypes", _max_num_scalar_mtypes));
    _scalar_mtype[_num_scalar_mtypes++] = mtype;
    p+=4;
  }

  UINT32 bit_size = 32;
  UINT32 byte_alignment = 4;
  Mtype_add(++Mtype_Last,
	    bit_size, bit_size, _num_scalar_mtypes,
	    byte_alignment, byte_alignment, byte_alignment,
	    0, 0, 0, strdup(signature),
	    MTYPE_CLASS_TIE | MTYPE_CLASS_TIE_PACKED, 0,
	    MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);

  _packed_mtype = Mtype_Last;
  return _packed_mtype;

}

void
TIE_INFO::init_immed_to_reg_map(void) {

  if (_immed_to_reg_macro_map==NULL) {

    _immed_to_reg_macro_map = CXX_NEW(MACRO_MAP(&_pool), &_pool);
    _reg_to_immed_macro_map = CXX_NEW(MACRO_MAP(&_pool), &_pool);

    xtie_phase xp = TI_TIE_Xtie_Compiler();
    if (xp) {
      for (xtie_property_iter it = xtie_get_property_iter(xp);
	   it; it = xtie_property_iter_step(it)) {
	xtie_property p = xtie_property_iter_get(it);
	if (xtie_property_get_type(p) != XTIE_PROPERTY_SPECIALIZED_OP)
	  continue;

	const char *id1 = xtie_property_get_arg_id(p, 0);
	const char *mangled_id1 = mangle_macro_name(id1);
	TIE_MACRO* macro1 = tie_macro(mangled_id1);
	const char *id2 = xtie_property_get_arg_id(p, 1);
	const char *mangled_id2 = mangle_macro_name(id2);
	TIE_MACRO* macro2 = tie_macro(mangled_id2);
	if (macro1 && macro2) {
	  int count1 =  macro1->num_immed_protos();
	  int count2 =  macro2->num_immed_protos();
	  if (count1+1 == count2) {
	    _immed_to_reg_macro_map->insert(macro2, macro1);
	    _reg_to_immed_macro_map->insert(macro1, macro2);
	  } else if (count2+1 == count1) {
	    _immed_to_reg_macro_map->insert(macro1, macro2);
	    _reg_to_immed_macro_map->insert(macro2, macro1);
	  }
	}
      }
    }
  }
}

bool
TIE_MACRO::is_instruction_macro(void) const {

  if (num_instructions()==1) {
    const char* nonmangled_name = inst_opcode_name(0);
    char * underscored_name = (char *) alloca(strlen(nonmangled_name));
    strcpy (underscored_name, nonmangled_name);
    while (char * dot = strchr(underscored_name, '.') )
      *dot = '_';

    if (!strcasecmp(underscored_name, demangled_name()))
      return TRUE;
  }

  return FALSE;

}


TIE_MACRO_p
TIE_INFO::immediate_to_register_form(TIE_MACRO_p macro) {

  if (macro==NULL)
    return NULL;

  if (_immed_to_reg_macro_map==NULL) {
    init_immed_to_reg_map();
  }

  TIE_MACRO_p ret_val=NULL;
  if (_immed_to_reg_macro_map && _immed_to_reg_macro_map->find(macro, ret_val))
    return ret_val;
  else
    return NULL;
}

TIE_MACRO_p
TIE_INFO::register_to_immediate_form(TIE_MACRO_p macro) {

  if (macro==NULL)
    return NULL;

  if (_reg_to_immed_macro_map==NULL) {
    init_immed_to_reg_map();
  }

  TIE_MACRO_p ret_val=NULL;
  if (_reg_to_immed_macro_map && _reg_to_immed_macro_map->find(macro, ret_val))
    return ret_val;
  else
    return NULL;
}

TIE_MACRO::TIE_MACRO (MEM_POOL *pool, xtensa_isa isa,
		      xtensa_proto macro, TIE_MACRO_ID id)
{

  int num_in=0;
  int num_out=0;
  int num_inout=0;
  _isa = isa;
  _pool = pool;
  _name = strdup(xtensa_proto_name(_isa,macro));
  _id = id;
  _num_protos = _num_temps = _num_instructions = 0;
  _return_proto_index = TIE_INVALID_ID;
  _num_inout_protos = 0;
  _num_output_protos = 0;
  _num_immed_protos = 0;
  _num_labels = 0;
  _has_branch = true;
  _is_simple_addr_load = false;
  _is_simple_addr_store = false;
  _simple_addr_load_store_base_proto_index = -1;
  _simple_addr_load_store_offset_proto_index = -1;
  _simple_addr_load_store_size = -1;
  _is_pure = false;
  _has_side_effect = true;
  _has_external_effect = true;
  _reads_memory = true;
  _writes_memory = true;
  _pure_and_side_effect_set = false;
  _operand_live_in = NULL;
  _temp_live_in = NULL;

  _macro = macro;
  check_exposed_uses();

  _num_protos = xtensa_proto_num_operands(_isa, _macro);
  _num_temps = xtensa_proto_num_tmps(_isa, _macro);
  _num_instructions = xtensa_proto_num_insns(_isa, _macro);

  _output_mtype = MTYPE_UNKNOWN;
  _output_mtype_set = false;
  int i;
  int num_operands = xtensa_proto_num_operands(_isa, _macro);
  for (i=0; i<num_operands; i++) {
    if (xtensa_proto_operand_is_immed(_isa, _macro, i))
      _num_immed_protos++;
    if (xtensa_proto_operand_inout(_isa, _macro, i)=='i')
      num_in++;
    else if (xtensa_proto_operand_inout(_isa, _macro, i)=='o') {
      num_out++;
      _return_proto_index = i;
    } else {
      num_inout++;
      _return_proto_index = i;
    }
    if (proto_is_label(i))
      _num_labels++;

    FmtAssert(!proto_has_const_prefix(i) || proto_is_pointer(i),
	      ("Only pointer type is allowed to have const prefix"));

  }

  _num_inout_protos = num_inout;
  _num_output_protos = num_out;
  if ((num_inout+num_out)!=1)
    _return_proto_index = TIE_INVALID_ID;
}

void
TIE_MACRO::init_macro_litclass_usage (TIE_INFO *tie_info)
{
  /* For each immediate prototype, find all the literal classes
     representing operands where that immediate is used in the macro
     instructions. */

  _proto_lc_list = CXX_NEW_ARRAY(TIE_LITCLASS_LIST *, _num_protos, _pool);
  for (UINT p = 0; p < _num_protos; p++)
  {
    if (proto_is_immed(p)) {
      const char* operand_name = xtensa_proto_operand_name(_isa, _macro, p);
      _proto_lc_list[p] = CXX_NEW(TIE_LITCLASS_LIST(LC_simm32, NULL), _pool);
      find_litclasses_for_operand(tie_info, operand_name, &_proto_lc_list[p]);
    } else {
      _proto_lc_list[p] = NULL;
    }
  }
}

void
TIE_MACRO::find_litclasses_for_operand (TIE_INFO *tie_info, const char *proto_op_name,
					TIE_LITCLASS_LIST **lcs)
{
  /* Examine each instruction in 'this'. Collect the litclasses that
     represent operands using prototype 'proto_op_name'. */

  for (UINT i = 0; i < _num_instructions; i++)
  {
      xtensa_opcode isa_opc = xtensa_proto_insn_opcode(_isa, _macro, i);
      const char* opcode_name = xtensa_opcode_name(_isa, isa_opc);
      TOP top = TI_TOP_Topcode(opcode_name);
      const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(top);
      const UINT num_isa_args = xtensa_proto_insn_num_args(_isa, _macro, i);

      /* there are three indices that we need to keep track of:
		top_operand_idx: operand index for Xcalibur's TOP
		top_result_idx:	 result index for Xcalibur's TOP

				 these are needed to get the value type
				 which tells whether the operand is a literal
				 and how to skip multi-reg operands

		arg:		 proto instruction argument index
      */

      UINT top_operand_idx = 0;
      UINT top_result_idx = 0;

      /* Examine the operands of this instruction to find the one(s)
	 that matches 'proto_op_name'. Collect the litclass of that
	 operand. */

      for (int arg = 0; arg < num_isa_args; arg++) {
	int var, is_tmp, immed;

	if (xtensa_proto_insn_arg_is_immed(_isa, _macro, i, arg) == 1) {
	  xtensa_proto_insn_arg_immed(_isa, _macro, i, arg, &var, &immed);
	  is_tmp = 0;
	} else {
	  xtensa_proto_insn_arg_variable(_isa, _macro, i, arg, &var, &is_tmp);
	}

	const char* operand_name=NULL;
	if (var==XTENSA_UNDEFINED) {
	  /* immediate operand may be just a constant integer */
	} else if (is_tmp) {
	  operand_name = temp_name(var);
	} else {
	  operand_name = proto_name(var);
	}

	UINT isa_operand_idx =
	  xtensa_proto_insn_arg_to_opnd(_isa, _macro, i, arg, 0);
	char inout = xtensa_operand_inout(_isa, isa_opc, isa_operand_idx);
	const ISA_OPERAND_VALTYP *vtype;

	if (inout != 'o')
	    vtype = TI_ISA_Op_Operand(opinfo, top_operand_idx);
	else
	    vtype = TI_ISA_Op_Result(opinfo, top_result_idx);

	if (inout == 'i')
	{
	  char* plus;

	  if (operand_name!=NULL && !strcmp(proto_op_name, operand_name)) {
	    ISA_LITCLASS lc;
	    if (TI_ISA_Valtyp_Is_Literal(vtype)) {
	      lc = TI_ISA_Valtyp_Litclass(vtype);
	      if (immed != 0) {
	        lc = TI_ISA_LC_Create(lc, -immed);
	      }
	    } else if (TI_ISA_Valtyp_Is_Register(vtype))
	      lc = TI_ISA_LC_Find(isa_opc, isa_operand_idx);
	    else
	      FmtAssert(FALSE,
		("Unexpected operand value type in "
		 "TIE_MACRO::find_litclasses_for_operand()"));
	    *lcs = CXX_NEW(TIE_LITCLASS_LIST(lc, *lcs), _pool);
	  }
	} else if (!strcmp(proto_op_name, operand_name)) {
	  /* inout == 'm' or 'o' */
	  FmtAssert((TI_ISA_Valtyp_Is_Register(vtype)),
		    ("Unexpected operand value type in "
		     "TIE_MACRO::find_litclasses_for_operand()"));
	  ISA_LITCLASS lc;
	  lc = TI_ISA_LC_Find(isa_opc, isa_operand_idx);
	  *lcs = CXX_NEW(TIE_LITCLASS_LIST(lc, *lcs), _pool);
	}

	UINT step = 1;
	if (TI_ISA_Valtyp_Is_Register(vtype) &&
	    TI_ISA_Valtyp_Num_Regs(vtype)==1)
	  step = xtensa_operand_num_regs(_isa, isa_opc, isa_operand_idx);

	if (inout == 'i' || inout == 'm')
	  top_operand_idx += step;
	if (inout == 'o' || inout == 'm')
	  top_result_idx += step;
      }
  }
}


const char *
TIE_MACRO::demangled_name () const
{
  return demangle_tie_name(_name, package());
}


BOOL
TIE_MACRO::immediate_range (UINT opnd_idx, INT32 *low, INT32 *high)
{
  FmtAssert((opnd_idx < _num_protos) &&
	    proto_is_immed(opnd_idx),
	    ("unexpected non-immediate prototype operand"));

  TIE_LITCLASS_LIST *lcs = litclass_usage(opnd_idx);

  /* If the 'opnd_idx' is not use in the macro, return false
     indicating that we don't know what the acceptable range is. */

  if (lcs == NULL)
    return FALSE;

  /* Initially assume the maximum range, then reduce it for each
     litclass. */

  INT32 tlow = INT32_MIN, thigh = INT32_MAX;
  while (lcs != NULL)
  {
    INT32 tl = INT32_MIN, th = INT32_MAX;
    if (!TI_ISA_LC_Range(lcs->_lc, &tl, &th))
      return FALSE;

    tlow = MAX(tlow, tl);
    thigh = MIN(thigh, th);
    if (tlow > thigh)
      return FALSE;

    lcs = lcs->_next;
  }

  *low = tlow;
  *high = thigh;
  return TRUE;
}

BOOL
TIE_MACRO::immediate_ok (UINT opnd_idx, INT64 imm)
{
  FmtAssert((opnd_idx < _num_protos) &&
	    proto_is_immed(opnd_idx),
	    ("unexpected non-immediate prototype operand"));

  TIE_LITCLASS_LIST *lcs = litclass_usage(opnd_idx);

  /* If the 'opnd_idx' is not use in the macro, return false
     indicating that we don't know what the acceptable range is. */

  if (lcs == NULL)
    return FALSE;

  /* Initially assume the maximum range, then reduce it for each
     litclass. */

  while (lcs != NULL)
  {
    if (!TI_ISA_LC_Value_In_Class(imm, lcs->_lc))
      return FALSE;

    lcs = lcs->_next;
  }

  return TRUE;
}

const char*
TIE_MACRO::proto_type_mangled_name(const int i) const
{
  if (proto_is_label(i))
    return "label";
  else if (proto_is_immed(i))
    return "immediate";

  int is_pointer;
  xtensa_ctype xt_ct = xtensa_proto_operand_type(_isa,_macro,i, &is_pointer, 0);
  const char* s = xtensa_ctype_name(_isa,xt_ct);
  return s;
}

const char*
TIE_MACRO::proto_type_demangled_name(const int i) const
{
  if (proto_is_label(i))
    return "label";
  else if (proto_is_immed(i))
    return "immediate";

  int is_pointer;
  xtensa_ctype xt_ct = xtensa_proto_operand_type(_isa,_macro,i, &is_pointer, 0);
  return tie_info->get_demangled_ctype_name(xt_ct);
}

const char*
TIE_MACRO::temp_type_name(const int i) const
{
  int is_pointer;
  xtensa_ctype xt_ct = xtensa_proto_tmp_type(_isa, _macro, i, &is_pointer);
  const char* s = xtensa_ctype_name(_isa,xt_ct);
  return s;
}

bool
TIE_MACRO::proto_is_pointer(const int i) const
{
  int is_pointer;
  xtensa_ctype xt_ct = xtensa_proto_operand_type(_isa,_macro,i, &is_pointer, 0);
  return is_pointer!=0;
}

bool
TIE_MACRO::temp_is_pointer(const int i) const
{
  int is_pointer;
  xtensa_ctype xt_ct = xtensa_proto_tmp_type(_isa,_macro,i, &is_pointer);
  return is_pointer!=0;
}

int
TIE_MACRO::whirl_to_proto_index(const int i)
{
  if (!is_whirl_intrinsic_op()) {
    return i;
  } else if (is_c_function()) {
    return (i + (i>=_return_proto_index ? 1:0));
  } else {
    int in_and_inout_proto_cnt=0;
    int proto_cnt=0;
    while (in_and_inout_proto_cnt<=i) {
      if (!proto_is_out(proto_cnt))
        in_and_inout_proto_cnt++;
      proto_cnt++;
    }
    return proto_cnt-1;
  }
}

int
TIE_MACRO::proto_to_whirl_index(const int i)
{
  if (!is_whirl_intrinsic_op()) {
    return i;
  } else if (proto_is_out(i)) {
    return -1;
  } else if (is_c_function()) {
    return (i - (i>_return_proto_index ? 1:0));
  } else {
    int out_proto_cnt=0;
    int proto_index=0;
    while (proto_index<i) {
      if (proto_is_out(proto_index))
        out_proto_cnt++;
      proto_index++;
    }
    return proto_index-out_proto_cnt;
  }
}

bool
TIE_MACRO::proto_is_immed(const int i) const
{
  return xtensa_proto_operand_is_immed(_isa, _macro, i);
}

bool
TIE_MACRO::proto_has_const_prefix(const int i)
{
  int is_pointer, is_const;
  xtensa_ctype xt_ct = xtensa_proto_operand_type(_isa,_macro,i,&is_pointer,
						 &is_const);
  return (is_pointer && is_const);
}

bool
TIE_MACRO::proto_is_label(const int i) const
{
  return xtensa_proto_operand_is_label(_isa, _macro, i);
}

TYPE_ID
TIE_INFO::Base_Tie_Type_Name_To_Mtype (const char* base_type_name) const
{
  if (base_type_name==NULL)
    return MTYPE_UNKNOWN;

  TYPE_ID mtype_id;
  const char* type_name = base_type_name;
  if (!strncmp(type_name, "const ", 6))
    type_name+=6;
  if (type_name[strlen(type_name)-1]=='*')
    mtype_id = pointer_mtype;
  else if (!strcmp(type_name,"_TIE_int64"))
    mtype_id = MTYPE_I8;
  else if (!strcmp(type_name,"_TIE_uint64"))
    mtype_id = MTYPE_U8;
  else if (!strcmp(type_name,"_TIE_int") ||
           !strcmp(type_name,"_TIE_int32") ||
           !strcmp(type_name,"immediate"))
    mtype_id = MTYPE_I4;
  else if (!strcmp(type_name,"_TIE_unsigned") ||
           !strcmp(type_name,"_TIE_uint32"))
    mtype_id = MTYPE_U4;
  else if (!strcmp(type_name,"_TIE_short") ||
           !strcmp(type_name,"_TIE_int16"))
    mtype_id = MTYPE_I2;
  else if (!strcmp(type_name,"_TIE_uint16"))
    mtype_id = MTYPE_U2;
  else if (!strcmp(type_name,"_TIE_int8"))
    mtype_id = MTYPE_I1;
  else if (!strcmp(type_name,"_TIE_char") ||
           !strcmp(type_name,"_TIE_uint8"))
    mtype_id = MTYPE_U1;
  else if (!strcmp(type_name,"_TIE_void"))
    mtype_id = MTYPE_V;
  else if (!strcmp(type_name,"_TIE_xtbool"))
    mtype_id = MTYPE_XTBOOL;
  else if (!strcmp(type_name,"_TIE_xtbool2"))
    mtype_id = MTYPE_XTBOOL2;
  else if (!strcmp(type_name,"_TIE_xtbool4"))
    mtype_id = MTYPE_XTBOOL4;
  else if (!strcmp(type_name,"_TIE_xtbool8"))
    mtype_id = MTYPE_XTBOOL8;
  else if (!strcmp(type_name,"_TIE_xtbool16"))
    mtype_id = MTYPE_XTBOOL16;
  else if (xtfloat_ctype_id() != XTENSA_UNDEFINED &&
           !strcmp(type_name, ctype_name(xtfloat_ctype_id())))
    mtype_id = MTYPE_F4;
  else
    mtype_id = MTYPE_UNKNOWN;

  return mtype_id;
}


const char*
TIE_INFO::mtype_name (TYPE_ID mid) const
{
  if (MTYPE_is_tie_packed(mid))
    return NULL;

  if (MTYPE_is_tie(mid))
    return ctype_name(Mtype_To_Tie_Type_Id(mid));

  switch (mid) {
    case MTYPE_V: return "_TIE_void";
    case MTYPE_I1: return "_TIE_int8";
    case MTYPE_U1: return "_TIE_uint8";
    case MTYPE_I2: return "_TIE_int16";
    case MTYPE_U2: return "_TIE_uint16";
    case MTYPE_I4: return "_TIE_int32";
    case MTYPE_U4: return "_TIE_uint32";
    case MTYPE_I8: return "_TIE_int64";
    case MTYPE_U8: return "_TIE_uint64";
    case MTYPE_F4:
      return ((xtfloat_ctype_id() != XTENSA_UNDEFINED) ?
              ctype_name(xtfloat_ctype_id()) : NULL);
    default: return NULL;
  }

}

static const char * 
demangle_tie_name(const char * mangled_name, const char * package_name)
{
    const char* type_name;
    const char* cp;

    type_name = mangled_name;
    
    if (strncmp(type_name, "const ", strlen("const ")) == 0)
      type_name += strlen("const ");
    
    /* skip _TIE_ if its there */
    if (strncmp(type_name, "_TIE_", strlen("_TIE_")) == 0)
      type_name += strlen("_TIE_");

    /* skip the package prefix */
    if (package_name && 
	(strncmp(type_name, package_name, strlen(package_name)) == 0)) {
      // extra one for the underscore separating the package from the real name
      type_name += strlen(package_name) + 1;
    }
    
    return type_name;
}

const char *
TIE_INFO::get_demangled_ctype_name (xtensa_ctype ct) const
{
  const char * mangled_name = xtensa_ctype_name(_isa, ct);
  const char * package_name = xtensa_ctype_package(_isa, ct);
  return demangle_tie_name(mangled_name, package_name);
}


const char *
TIE_INFO::mangle_ctype_name (const char* nonmangled_name) const
{
  char * underscored_name = (char *) alloca(strlen(nonmangled_name));
  strcpy (underscored_name, nonmangled_name);
  while (char * dot = strchr(underscored_name, '.') )
    *dot = '_';

  for (xtensa_ctype ct = 0; ct < xtensa_isa_num_ctypes(_isa); ct++) {
    const char * demangled_candidate = get_demangled_ctype_name(ct);
    if (strcmp(underscored_name, demangled_candidate) == 0)
      return xtensa_ctype_name(_isa, ct);
  }
  return nonmangled_name;
}


const char *
TIE_INFO::get_demangled_macro_name (const char *mangled_name) const
{
  xtensa_proto p = xtensa_proto_lookup(_isa, mangled_name);
  Is_True(p != XTENSA_UNDEFINED, ("Unrecognized mangled proto name"));
  const char * package_name = xtensa_proto_package(_isa, p);
  return demangle_tie_name(mangled_name, package_name);
}


const char *
TIE_INFO::mangle_macro_name (const char* nonmangled_name) const
{
  char * underscored_name = (char *) alloca(strlen(nonmangled_name));
  strcpy (underscored_name, nonmangled_name);
  while (char * dot = strchr(underscored_name, '.') )
    *dot = '_';

  for (xtensa_proto pr = 0; pr < xtensa_isa_num_protos(_isa); pr++) {
    const char * package_name = xtensa_proto_package(_isa, pr);
    const char * mangled_cand = xtensa_proto_name(_isa, pr);
    const char * demangled_cand = demangle_tie_name(mangled_cand, package_name);
    if (strcmp(underscored_name, demangled_cand) == 0)
      return mangled_cand;
  }
  return nonmangled_name;
}


TYPE_ID
TIE_MACRO::proto_mtype_id(const TIE_INFO* tie, const int i) const
{
  if (proto_is_pointer(i))
    return pointer_mtype;

  TYPE_ID mtype_id = tie->mtype_id(proto_type_mangled_name(i));

  return mtype_id;
}

TYPE_ID
TIE_MACRO::proto_pointed_mtype_id(const TIE_INFO* tie, const int i) const
{
  if (proto_is_pointer(i)==false) {
    FmtAssert(0,("Expect pointer type"));
    return MTYPE_UNKNOWN;
  }

  TYPE_ID mtype_id = tie->mtype_id(proto_type_mangled_name(i));

  return mtype_id;
}

TYPE_ID
TIE_MACRO::temp_mtype_id(const TIE_INFO* tie, const int i) const
{
  if (temp_is_pointer(i))
    return pointer_mtype;

  TYPE_ID mtype_id = tie->mtype_id(temp_type_name(i));

  return mtype_id;
}

// determines the input argument index and offset value for
// an operand expression of instructions in TIE proto
// the accepted forms are:
//
//	16,			(case 1)
//	+16, -32		(case 2)
//	v,			(case 3)
//	v+16 or v+-16,		(case 4)
//
// it returns -1 when there is no variable argument (i.e., case 1 and 2)
// the offset is a signed value and is 0 for case 3

int TIE_MACRO::operand_index(int inst, int opnd, int& offset) const
{
  int operand_index, immed = 0;

  if (xtensa_proto_insn_arg_is_immed(_isa, _macro, inst, opnd) == 1) {
    xtensa_proto_insn_arg_immed(_isa, _macro, inst, opnd,
				&operand_index, &immed);
    if (operand_index == XTENSA_UNDEFINED)
      operand_index = -1;

  } else {

    /* register operand */
    int is_tmp;
    xtensa_proto_insn_arg_variable(_isa, _macro, inst, opnd,
				     &operand_index, &is_tmp);
    if (is_tmp)
      operand_index += num_protos();
  }

  offset = immed;
  return operand_index;
}

int TIE_MACRO::proto_insn_arg_to_opnd(int inst, int arg, int *offsetp)
{
  return xtensa_proto_insn_arg_to_opnd(_isa, _macro, inst, arg, offsetp);
}

int TIE_MACRO::num_inst_operands(int inst) const
{
  return xtensa_proto_insn_num_args(_isa,_macro,inst);
}

void TIE_MACRO::set_pure_and_side_effect() {

  int i;
  INT32 base_operand_idx = -1;
  INT32 offset_operand_idx = -1;

  if (_pure_and_side_effect_set)
    return;

  _has_branch = false;
  _is_simple_addr_load = false;
  _is_simple_addr_store = false;
  _simple_addr_load_store_base_proto_index = -1;
  _simple_addr_load_store_offset_proto_index = -1;
  _simple_addr_load_store_size = -1;
  _is_pure = true;
  _has_side_effect = false;
  _has_external_effect = false;
  _reads_memory = false;
  _writes_memory = false;

  // check how operands and states are used by instructions/protos
  // in this proto to determine if it is pure or has side effect

  int num_insts=num_instructions();

  if (num_insts==1) {
    const char* opcode_name = inst_opcode_name(0);
    TOP top = TI_TOP_Topcode(opcode_name);
    if ((TI_ISA_Property_Set(PROP_load, top) ||
	 TI_ISA_Property_Set(PROP_store, top)) &&
	TI_ISA_Property_Set(PROP_unalign_store, top)==false &&
	TI_ISA_Property_Set(PROP_unknown_addr, top)==false) {

      base_operand_idx = TI_TOP_Find_Operand_Use(top, OU_base);
      offset_operand_idx = TI_TOP_Find_Operand_Use(top, OU_offset);
      _simple_addr_load_store_size = TI_ISA_Mem_Ref_Bytes(top);

      if (base_operand_idx!= -1)
	if (TI_ISA_Property_Set(PROP_load, top))
	  _is_simple_addr_load = true;
	else
	  _is_simple_addr_store = true;
    }
  }

  for (i=0; i<num_insts; i++) {

      // found a real instruction
      // the number of operands/results to the instruction here may be
      // different than that of libisa because of multi-reg operands

      xtensa_opcode xopc = xtensa_proto_insn_opcode(_isa,_macro,i);
      const char* opcode_name = xtensa_opcode_name(_isa, xopc);
      TOP top = TI_TOP_Topcode(opcode_name);

      int num_op_arguments = xtensa_proto_insn_num_args(_isa, _macro, i);

      // construct a map back to the protos of the current macro
      int operand_map[128];
      for (int arg=0; arg<num_op_arguments; arg++) {
	int var, is_tmp;
        operand_map[arg] = -1;
	if (xtensa_proto_insn_arg_is_immed(_isa, _macro, i, arg) == 1) {
	  xtensa_proto_insn_arg_immed(_isa, _macro, i, arg, &var, 0);
	  if (var != XTENSA_UNDEFINED)
	    operand_map[arg] = var;
	} else {
	  xtensa_proto_insn_arg_variable(_isa, _macro, i, arg, &var, &is_tmp);
	  if (!is_tmp)
	    operand_map[arg] = var;
	}
      }

      if (TI_ISA_Property_Set(PROP_side_effects,top))
	_has_side_effect = true;

      if (TI_ISA_Property_Set(PROP_extern_effects,top)) {
	_has_side_effect = true;
	_has_external_effect = true;
	_is_pure = false;
      }

      int num_op_operands=0;
      int num_op_results=0;
      int isa_argument_idx=0;

      const ISA_OPERAND_INFO *operands_info = TI_ISA_Operand_Info (top);
      int j=0;
      while (j<num_op_arguments) {

	isa_argument_idx =
	  xtensa_proto_insn_arg_to_opnd(_isa, _macro, i, j, 0);

	char inout = xtensa_operand_inout(_isa, xopc, isa_argument_idx);
	const ISA_OPERAND_VALTYP *operand_value_type =
		(inout=='i' || inout=='m') ?
		TI_ISA_Op_Operand(operands_info, num_op_operands) :
		TI_ISA_Op_Result(operands_info, num_op_results);
	int skip = 1;
	if (xtensa_operand_is_register(_isa, xopc, isa_argument_idx) == 1) {

	  ISA_REGCLASS rc= TI_ISA_Valtyp_Regclass(operand_value_type);

	  if (TI_ABI_Property_Set(ABI_PROPERTY_allocatable, rc, -1)==false) {
	    if (inout=='i' || inout=='m')
	      _is_pure = false;
	    if (inout=='o' || inout=='m')
	      _has_side_effect = true;
	  }

	  int num_regs = xtensa_operand_num_regs(_isa, xopc, isa_argument_idx);
	  if (num_regs>1 &&
	      TI_ISA_Valtyp_Num_Regs(operand_value_type)==num_regs)
	    num_regs = 1;
	  skip = num_regs;
	}
	if (inout == 'i' || inout == 'm') {
	  // if this is an input operand for the inst but not the macro
	  // this macro is not pure
	  for (int k=0; k<skip; k++) {
	    int index = operand_map[j+k];
	    if (index != -1 && index < num_protos() && proto_is_out(index))
	      _is_pure = false;
	    if ((_is_simple_addr_load || _is_simple_addr_store) &&
		num_op_operands==base_operand_idx) {
	      if (index != -1 && index < num_protos())
		_simple_addr_load_store_base_proto_index = index;
	      else {
		_is_simple_addr_load=false;
		_is_simple_addr_store=false;
	      }
	    }
	    if ((_is_simple_addr_load || _is_simple_addr_store) &&
		num_op_operands==offset_operand_idx) {
	      if (index != -1 && index < num_protos())
		_simple_addr_load_store_offset_proto_index = index;
	      else {
		_is_simple_addr_load=false;
		_is_simple_addr_store=false;
	      }
	    }
	    num_op_operands++;
	  }
        }
	if (inout == 'o' || inout == 'm') {
	  // if this is an output operand for the inst but not the macro
	  // this macro has side effect
	  for (int k=0; k<skip; k++) {
	    int index = operand_map[j+k];
	    if (index != -1 && index < num_protos() && proto_is_in(index))
	      _has_side_effect = true;
	    num_op_results++;
	  }
	}
	j+=skip;
      }

      // if there are additional implicit input operands then
      // this macro is not pure
      if (TI_ISA_Property_Set(PROP_load,top)) {
        _is_pure = false;
        _reads_memory = true;
      }
      else if (num_op_operands<TI_ISA_Op_Operands(operands_info)) {

	INT num_isa_operands = TI_ISA_Op_Operands(operands_info);
	BOOL has_cpenable = false;
	if (num_op_operands+1==num_isa_operands) {
	  for (INT i=0; i<num_isa_operands; i++) {
	    const ISA_OPERAND_VALTYP *operand_type = TI_ISA_Op_Operand(operands_info,i);
	    if (TI_ISA_Regclass_Is_State(TI_ISA_Valtyp_Regclass(operand_type)) &&
		TI_ISA_Valtyp_Regsubclass(operand_type)== TI_ISA_Regsubclass("CPENABLE")) {
	      has_cpenable = true;
	    }
	  }
	  if (has_cpenable==false)
	    _is_pure = false;
	} else
	  _is_pure = false;
      }

      // if there are additional implicit output operands then
      // this macro has side effect
      if (TI_ISA_Property_Set(PROP_store,top)) {
	_has_side_effect = true;
        _writes_memory = true;
      }
      else if (num_op_results<TI_ISA_Op_Results(operands_info))
	_has_side_effect = true;

      if (TI_ISA_Property_Set(PROP_xfer, top)) {
	_has_branch = true;
	_has_external_effect = true;
      }

  }

  if (num_labels()) {
    _has_side_effect = true;
    _has_external_effect = true;
  }

#ifdef BACK_END
  if (Get_Trace(TP_TIE, TP_TIE_general)) {
    fprintf(TFile, "TIE:Init macro %s:%s %s\n",
		_name,
		(_is_pure)? "is_pure": "is_not_pure",
		(_has_side_effect)? "has_side_effect": "has_no_side_effect");

  }
#endif

  _pure_and_side_effect_set = true;
}

int TIE_MACRO::label_proto_index(const int i) const {

  FmtAssert(i>0, ("label index starts from 1"));
  FmtAssert(is_conditional_branch(),
	    ("Getting label index for non-branching instruction."));
  if (!is_conditional_branch() || i<=0)
    return -1;

  int j = 0;
  int labels = 0;
  while (j<num_protos() && labels<i) {
    if (proto_is_label(j))
      labels++;
    j++;
  }

  FmtAssert(labels==i, ("label index %d too large", i));
  if (labels!=i)
    return -1;

  return j-1;

}

bool TIE_MACRO::is_pure() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _is_pure;

}

bool TIE_MACRO::has_side_effect() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _has_side_effect;

}

bool TIE_MACRO::has_external_effect() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _has_external_effect;

}

bool TIE_MACRO::reads_memory() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _reads_memory;

}

bool TIE_MACRO::has_branch() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _has_branch;

}

bool TIE_MACRO::is_simple_addr_load_store() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _is_simple_addr_load || _is_simple_addr_store;

}

bool TIE_MACRO::is_simple_addr_load() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _is_simple_addr_load;

}

bool TIE_MACRO::is_simple_addr_store() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _is_simple_addr_store;

}

int TIE_MACRO::simple_addr_load_store_base_proto_index() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _simple_addr_load_store_base_proto_index;

}

int TIE_MACRO::simple_addr_load_store_offset_proto_index() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _simple_addr_load_store_offset_proto_index;

}

int TIE_MACRO::simple_addr_load_store_size() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _simple_addr_load_store_size;

}

bool TIE_MACRO::writes_memory() {

  if (_pure_and_side_effect_set==false)
    set_pure_and_side_effect();

  return _writes_memory;

}

TYPE_ID TIE_MACRO::output_mtype(TIE_INFO* tie_info) {

  if (_output_mtype_set==false) {
    _output_mtype = tie_info->find_output_mtype(this);
    _output_mtype_set = true;
  }

  return _output_mtype;

}

void
TIE_MACRO::add_mapping(TIE_MACRO_p new_tie_macro,
		       TIE_MAPPING_FLAGS flags)
{

  TIE_MACRO_MAPPING_p a_mapping =
    CXX_NEW(TIE_MACRO_MAPPING(new_tie_macro,flags,_mappings), _pool);
  _mappings = a_mapping;

}


TIE_MACRO_p
TIE_MACRO::find_tie_macro(TIE_MAPPING_FLAGS flags) const
{

  TIE_MACRO_MAPPING_p a_mapping = _mappings;

  while (a_mapping) {

    if (flags.equal(a_mapping->flags()))
      return a_mapping->tie_macro();
    a_mapping = a_mapping->next();

  }

  return NULL;

}


TIE_MACRO_ID TIE_INFO::tie_macro_id(const char* name) const {

  TIE_MACRO_ID tie_id;

  for (tie_id=0; tie_id<_num_macros; tie_id++)
    if (!strcmp(macro_name(tie_id), name))
      return tie_id;

  return TIE_INVALID_ID;
}


TIE_MACRO_ID
TIE_INFO::tie_macro_id_demangled(const char* name) const
{
  TIE_MACRO_ID tie_id;

  for (tie_id=0; tie_id<_num_macros; tie_id++)
    if (!strcmp(macro_demangled_name(tie_id), name))
      return tie_id;

  return TIE_INVALID_ID;
}


TIE_MACRO_p TIE_INFO::
find_convert_macro(const char* src_type_name,
		   int from_mem,
		   int to_mem,
		   const char* dst_type_name) const
{
  const char *macro_name = 0;

  if (src_type_name==NULL || dst_type_name==NULL)
    return NULL;

  xtensa_ctype src_ctype = xtensa_ctype_lookup(_isa, src_type_name);
  xtensa_ctype dst_ctype = xtensa_ctype_lookup(_isa, dst_type_name);
  xtensa_proto xt_proto = XTENSA_UNDEFINED;

  if (!from_mem && !to_mem)
    xt_proto = xtensa_ctype_rtor_proto(_isa, src_ctype, dst_ctype);
  else if (from_mem && !to_mem)
    xt_proto = xtensa_ctype_mtor_proto(_isa, src_ctype, dst_ctype);
  else if (!from_mem && to_mem)
    xt_proto = xtensa_ctype_rtom_proto(_isa, src_ctype, dst_ctype);

  if (xt_proto!=XTENSA_UNDEFINED)
    macro_name = xtensa_proto_name(_isa, xt_proto);

  if (!macro_name)
    return NULL;

  TIE_MACRO_ID macro_id = tie_macro_id(macro_name);
  if (macro_id==TIE_INVALID_ID)
    return NULL;
  else
    return tie_macro(macro_id);

}

TIE_MACRO_p TIE_INFO::
find_convert_macro(const TYPE_ID src_mtype,
		   int from_mem,
		   int to_mem,
		   const TYPE_ID dst_mtype) const
{

  const char* src_type_name;
  const char* dst_type_name;
  char* mtype_names[10];
  int mtype_name_count=0;
  TYPE_ID mtype_id;

  FmtAssert(MTYPE_is_tie(src_mtype) || MTYPE_is_tie(dst_mtype),
		("Requesting TIE type conversion for non-TIE types"));

  if (MTYPE_is_tie(src_mtype)==false || MTYPE_is_tie(dst_mtype)==false) {

    if (MTYPE_is_tie(src_mtype)==false)
      mtype_id = src_mtype;
    else if (MTYPE_is_tie(dst_mtype)==false)
      mtype_id = dst_mtype;

    switch (mtype_id) {
	case MTYPE_I8:
		mtype_names[mtype_name_count++]="_TIE_int64";
		break;
	case MTYPE_U8:
		mtype_names[mtype_name_count++]="_TIE_uint64";
		break;
	case MTYPE_I4:
		mtype_names[mtype_name_count++]="_TIE_int32";
		mtype_names[mtype_name_count++]="_TIE_int";
		break;
	case MTYPE_U4:
		mtype_names[mtype_name_count++]="_TIE_uint32";
		mtype_names[mtype_name_count++]="_TIE_unsigned";
		break;
	case MTYPE_I2:
		mtype_names[mtype_name_count++]="_TIE_int16";
		mtype_names[mtype_name_count++]="_TIE_short";
		break;
	case MTYPE_U2:
		mtype_names[mtype_name_count++]="_TIE_uint16";
		break;
	case MTYPE_I1:
		mtype_names[mtype_name_count++]="_TIE_int8";
		break;
	case MTYPE_U1:
		mtype_names[mtype_name_count++]="_TIE_char";
		mtype_names[mtype_name_count++]="_TIE_uint8";
		break;
    }
    if (MTYPE_is_tie(src_mtype)==false) {
      dst_type_name = mtype_name(dst_mtype);
      TIE_MACRO_p tie_macro=NULL;
      while (mtype_name_count && tie_macro==NULL) {
	tie_macro = find_convert_macro(mtype_names[mtype_name_count],
				       from_mem, to_mem,
				       dst_type_name);
	mtype_name_count--;
      }
      return tie_macro;
    } else if (MTYPE_is_tie(dst_mtype)==false) {
      src_type_name = mtype_name(src_mtype);
      TIE_MACRO_p tie_macro=NULL;
      while (mtype_name_count && tie_macro==NULL) {
	tie_macro = find_convert_macro(src_type_name,
				       from_mem, to_mem,
				       mtype_names[mtype_name_count]);
	mtype_name_count--;
      }
      return tie_macro;
    }
  } else {
    return find_convert_macro(mtype_name(src_mtype),
			      from_mem, to_mem,
			      mtype_name(dst_mtype));
  }

}

TIE_MACRO_p
TIE_INFO::ctype_rtor_macro(const TIE_TYPE_ID src_id,
			   const TIE_TYPE_ID dst_id) const
{
  return find_convert_macro(ctype_name(src_id),
			    0 /* from_mem */,
			    0 /* to_mem */,
			    ctype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::ctype_mtor_macro(const TIE_TYPE_ID src_id,
			   const TIE_TYPE_ID dst_id) const
{
  return find_convert_macro(ctype_name(src_id),
			    1 /* from_mem */,
			    0 /* to_mem */,
			    ctype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::ctype_rtom_macro(const TIE_TYPE_ID src_id,
			   const TIE_TYPE_ID dst_id) const
{
  return find_convert_macro(ctype_name(src_id),
			    0 /* from_mem */,
			    1 /* to_mem */,
			    ctype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::ctype_mtom_macro(const TIE_TYPE_ID src_id,
			   const TIE_TYPE_ID dst_id) const
{
  Is_True(0,("Memory to memory conversion not supported"));
#if 0
  return find_convert_macro(ctype_name(src_id),
			    1 /* from_mem */,
			    1 /* to_mem */,
			    ctype_name(dst_id));
#endif
  return NULL;
}

TIE_MACRO_p
TIE_INFO::mtype_rtor_macro(const TYPE_ID src_id,
			   const TYPE_ID dst_id) const
{
  return find_convert_macro(mtype_name(src_id),
			    0 /* from_mem */,
			    0 /* to_mem */,
			    mtype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::mtype_mtor_macro(const TYPE_ID src_id,
			   const TYPE_ID dst_id) const
{
  return find_convert_macro(mtype_name(src_id),
			    1 /* from_mem */,
			    0 /* to_mem */,
			    mtype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::mtype_rtom_macro(const TYPE_ID src_id,
			   const TYPE_ID dst_id) const
{
  return find_convert_macro(mtype_name(src_id),
			    0 /* from_mem */,
			    1 /* to_mem */,
			    mtype_name(dst_id));
}

TIE_MACRO_p
TIE_INFO::mtype_mtom_macro(const TYPE_ID src_id,
			  const TYPE_ID dst_id) const
{
  Is_True(0,("Memory to memory conversion not supported"));
#if 0
  return find_convert_macro(mtype_name(src_id),
			    1 /* from_mem */,
			    1 /* to_mem */,
			    mtype_name(dst_id));
#endif
  return NULL;
}

TIE_MACRO_p
TIE_INFO::find_tie_macro(const char* base_macro_name,
			const TIE_MAPPING_FLAGS flags) const
{
  TIE_MACRO_p base_tie_macro = tie_macro(base_macro_name);

  return (base_tie_macro->find_tie_macro(flags));

}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
