
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
//
// TIE intrinsic support
//

// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/com/tie.h#2 $

//
// class TIE_INFO
// --------------
//
//	TIE_INFO contains all TIE related information and provides
//	methods to access those information. It obtains the TIE
//	data from libisa.so.
//
// TIE_INFO Methods
// ----------------
//
//	TIE_INFO()
//	~TIE_INFO()
//
//	methods for TIE types
//	---------------------
//
//	int num_ctypes() const
//
//		return the number of C data types defined for TIE
//		register files
//
//	TYPE_ID mtype_id(const char* name) const
//
//		lookup the MTYPE type id for given a TIE type name
//
//		return MTYPE_UNKNOWN if there is no MTYPE corresponding
//		to 'name'
//
//	TYPE_ID xtfloat_mtype_id() const
//
//		return the mtype id for xtfloat. This is skips
//		the translation in mtype_id() which returns MTYPE_F4
//		and is useful to get TIE protos for xtfloat.
//
//		return MTYPE_UNKNOWN if there is no xtfloat TIE type
//		or xt_hard_float is FALSE
//
//      TOP xtfloat_move_topcode () const
//
//              return the TOP to be used to move xtfloat values.
//              return TOP_UNDEFINED if there is no xtfloat TIE type
//              or xt_hard_float is FALSE
//
//      bool xtfloat_ar_converts () const
//
//              Return true if the convert opcodes between xtfloat and
//              int/uint work between the FP register file and AR. Return
//              false otherwise (i.e. return false if the converts work
//              within the FP register file). That's relevant only if
//              xt_hard_float is TRUE.
//
//	int ctype_size(const char* name) const
//	int ctype_size(const TIE_TYPE_ID id) const
//	int ctype_byte_size(const char* name) const
//	int ctype_byte_size(const TIE_TYPE_ID id) const
//
//		return the size of a TIE data type spcified in the .tie
//		file
//
//	int ctype_alignment(const char* name) const
//	int ctype_alignment(const TIE_TYPE_ID id) const
//	int ctype_byte_alignment(const char* name) const
//	int ctype_byte_alignment(const TIE_TYPE_ID id) const
//
//		return the alignment of a TIE data type specified in the
//		.tie file
//
//	const char* ctype_name(const TIE_TYPE_ID id) const
//
//		return the TIE C data type name given the TIE type id
//
//	const char* ctype_package(const TIE_TYPE_ID id) const
//
//		return the TIE C data type package name of TIE type id
//
//	TIE_MACRO_p ctype_move_macro(const TIE_TYPE_ID id) const
//	TIE_MACRO_p ctype_loadi_macro(const TIE_TYPE_ID id) const
//	TIE_MACRO_p ctype_storei_macro(const TIE_TYPE_ID id) const
//
//		return the handle of the TIE move/loadi/storei macro
//
//	TIE_MACRO_p ctype_rtor_macro(const TIE_TYPE_ID src_id,
//				     const TIE_TYPE_ID dst_id) const
//	TIE_MACRO_p ctype_mtor_macro(const TIE_TYPE_ID src_id,
//				     const TIE_TYPE_ID dst_id) const
//	TIE_MACRO_p ctype_rtom_macro(const TIE_TYPE_ID src_id,
//				     const TIE_TYPE_ID dst_id) const
//	TIE_MACRO_p ctype_mtom_macro(const TIE_TYPE_ID src_id,
//				     const TIE_TYPE_ID dst_id) const
//
//		return the handle of the TIE type converting macro
//		the last mtom is currently not used so it always return NULL
//
//	BOOL has_tie_branch_macro() const
//
//		return whether any TIE branch macro is present
//
//	const char* get_demangled_ctype_name (xtensa_ctype) const
//
//		returns ctype name with _TIE_ prefix and package name removed
//
//	const char* mangle_ctype_name (const char* nonmangled_name) const
//
//              returns ctype name with _TIE_ prefix and package name added
//
//
//	methods for TIE macros
//	---------------------
//
//	int num_macros() const
//
//		return the number of defined TIE macros
//
//	TIE_MACRO_ID tie_macro_id(const char* name) const
//
//		lookup the id for a TIE macro name
//
//		return TIE_INVALID_ID if there is no TIE type corresponding
//		to 'name'
//
//	TIE_MACRO_p tie_macro(const char* name) const
//	TIE_MACRO_p tie_macro(const TIE_MACRO_ID id) const
//
//		return the handle of the specified TIE macro
//
//	char* macro_name(const TIE_MACRO_ID i) const
//
//		return the name of the specified TIE macro
//
//	TIE_MACRO_p find_tie_macro(
//			const char* macro_name,
//			const TIE_MAPPING_FLAGS flags) const
//
//	void init_macro_output_mtypes()
//
//		initialize output mtypes for all macros
//		this needs to be done after TI_TIE_Mtypes_Init() and
//		will create additional mtypes for packed output to
//		be used by TIE intrinsic ops
//
//	int num_scalar_mtypes(TYPE_ID packed_mtype) const
//
//		return the number of components of
//		the 'packed_mtype'. returns -1 if
//		'packed_mtype' is not an packed mtype (i.e.,
//		MTYPE_is_tie_packed() returns false) or if the index is
//		out of range.
//
//	TYPE_ID get_scalar_mtype(TYPE_ID packed_mtype, INT index) const
//
//		return the mtype of the 'index'-th (0 based) component of
//		the 'packed_mtype'. returns MTYPE_UNKNOWN if
//		'packed_mtype' is not an packed mtype (i.e.,
//		MTYPE_is_tie_packed() returns false) or if the index is
//		out of range.
//
//      const char *get_demangled_macro_name (const char *) const
//
//		returns macro name with _TIE_ prefix and package name removed
//
//      const char *mangle_macro_name (const char* nonmangled_name) const
//
//              returns macro name with _TIE_ prefix and package name added
//   
//
// class TIE_MACRO
// ---------------
//
//	TIE_MACRO contains all information realated to a TIE macro and
//	provides methods to access those information.
//
// TIE_MACRO Methods
// ----------------
//
//	TIE_MACRO()
//	TIE_MACRO(MEM_POOL *pool, xtensa_isa isa,
//		  xtensa_proto macro, TIE_MACRO_ID id)
//	~TIE_MACRO()
//
//		the second constructor is used by TIE_INFO object to
//		construct a macro from a xtensa_proto
//
//	char* name()
//	const TIE_MACRO_ID id()
//
//		return the name/id of the macro
//
//	bool is_c_function() const
//
//		return TRUE if this macro has only one out parameter
//		and is not a conditional branch
//		such macro will be declared as C function with return value
//
//		note inout is NOT the same as out
//
//	bool is_whirl_intrinsic_op() const
//
//		return TRUE if this macro translates to a WHIRL intrinsic_op
//		the current criteria are
//
//		1. is pure
//		2. has no side effect
//
//	bool is_instruction_macro()
//
//		return TRUE if this macro has a single instruction which
//		has the same name as the macro
//
//	bool is_pure()
//
//		return TRUE if this macro relies only on explicit input
//
//	bool has_branch()
//
//		return TRUE if this macro lower to instructions that transfers
//		control
//
//	bool is_simple_addr_load_store()
//	bool is_simple_addr_load()
//	bool is_simple_addr_store()
//
//		return TRUE if this macro lower to a single load/store
//		instruction which does not have unknown address
//
//	int simple_addr_load_store_base_proto_index()
//
//		return the index for the proto which is the base of
//		a simple addressed load/store macro or -1 if there
//		is no such base
//		caller should check for is_simple_addr_load_store()
//		before calling this
//
//	int simple_addr_load_store_offset_proto_index()
//
//		return the index for the proto which is the offset of
//		a simple addressed load/store macro or -1 if there
//		is no such offset
//		caller should check for is_simple_addr_load_store()
//		before calling this
//
//	int simple_addr_load_store_size()
//
//		return the size loaded/stored by the proto
//		or -1 if the proto is not a simple load/store
//		caller should check for is_simple_addr_load_store()
//		before calling this
//
//	bool has_side_effect()
//
//		return TRUE if this macro has implicit output
//		or if it is a conditional branch
//
//	bool has_external_effect()
//
//		return TRUE if this macro touches external ports
//		or if it is a conditional branch
//
//	bool is_conditional_branch()
//
//		return TRUE if this macro has label arguments
//
//	int label_proto_index(const int i) const
//
//		return the index for the proto which is the i-th label
//		(starting from 1) or -1 if there is no such label
//		caller should check for num_labels() before calling this
//
//	const char* return_type_name() const
//
//		return the C type name of the out parameter
//		this is meaningful only if the macro has exactly one out/inout
//		argument
//
//	TYPE_ID output_mtype() const
//
//		return the machine type id for the output of the macro
//		the values returned are
//		MTYPE_V  -- if the macro is NULL, has no output, or
//			    is not an intrinsic op
//		MTYPE_I4 -- if the macro is a conditional branch
//		a scalar mtype
//			 -- if there is a unique output/inout argument
//		a packed mtype
//			 -- if there are more than one output/inout arguments
//
//	int operand_index(int inst, int opnd, int& offset) const
//
//		return the index for the operand 'opnd' of instruction 'i'
//		the index is determined from the parameter sequence in the
//		proto statement of the tie file
//		temp operands follow the explicit operands
//
//		'offset' will return the numerical portion of the expression
//		or 0
//
//		if 'expr' is pure numerical, a -1 is returned and the 'offset'
//		contains the value
//
//	int num_protos() const
//	int num_temps() const
//	int num_instructions() const
//	int num_labels() const
//
//		return number of protos/temps/instructions/labels based on
//		information in the proto/temp/code clauses of
//		a proto statement
//
//	int num_inout_protos() const
//
//		return the number of inout protos
//
//	int num_output_protos() const
//
//		return the number of output protos
//
//	int num_immed_protos() const
//
//		return the number of immediate protos
//
//	int whirl_to_proto_index(const int i)
//
//		return the (0 based) operand index in TIE proto given
//		the (0 based) parameter index in WHIRL
//
//	int proto_to_whirl_index(const int i)
//
//		return the (0 based) parameter index in WHIRL given
//		the (0 based) operand index in TIE proto.
//              return -1 if there is no corresponding WHIRL parameter
//
//	proto methods
//	-------------
//
//	TYPE_ID proto_mtype_id(const TIE_INFO* tie, const int i) const
//	TYPE_ID proto_pointed_mtype_id(const TIE_INFO* tie, const int i) const
//	const char* proto_type_mangled_name(const int i) const
//	const char* proto_type_demangled_name(const int i) const
//	const char* proto_name(const int i) const
//	bool proto_is_in(const int i) const
//	bool proto_is_out(const int i) const
//	bool proto_is_inout(const int i) const
//      bool proto_is_immed(const int i) const
//	bool proto_has_const_prefix(const int i)
//	bool proto_is_pointer(const int i) const
//	bool proto_is_label(const int i) const
//
//		return information about a 'proto'
//		the proto here actually means a formal parameter for the macro
//		to get the type id we also need to know the TIE_INFO object
//
//	temp methods
//	------------
//
//	TYPE_ID temp_mtype_id(const TIE_INFO* tie, const int i) const
//	const char* temp_type_name(const int i) const
//	const char* temp_name(const int i) const
//
//		return information about a 'temp' parameter for the macro
//		to get the type id we also need to know the TIE_INFO object
//
//	code methods
//	------------
//
//	const char* inst_opcode_name(const int i) const
//
//		return information about an instruction in the code clause
//		of a proto statement.
//
//	int num_inst_operands(int i) const
//
//		return the number of operand for instruction 'i' in the
//		proto, the instruction can be a real isntruction or a
//		proto
//
//
//	macro mapping methods
//	---------------------
//
//	void add_mapping()
//	TIE_MACRO* find_tie_macro()
//
//
//		These are not completed yet. They are desinged to support
//		general mapping from one TIE macro to another.
//
//
//

#ifndef _TIE_H_
#define _TIE_H_

#include <stdio.h>
#include "defs.h"
#include "mempool.h"
#include "xtensa-isa.h"
#include "libti.h"
#include "tietypes.h"
#include "mtypes.h"
#include "errors.h"
#include "xtmap.h"

#define	TP_TIE_general		0x0001
#define	TP_TIE_macro_expansion	0x0002
#define	TP_TIE_branch		0x0004

class TIE_INFO;

struct TIE_LITCLASS_LIST
{
  ISA_LITCLASS _lc;
  TIE_LITCLASS_LIST *_next;

  TIE_LITCLASS_LIST (ISA_LITCLASS lc, TIE_LITCLASS_LIST *next) :
    _lc(lc), _next(next)
  { }
};
  

class TIE_MAPPING_FLAGS {

private:
  union {
    UINT32	value;
    struct {
      UINT32	factor : 8;
    } bits;
  } _u;

public:
  TIE_MAPPING_FLAGS() { _u.value = 0; }
  TIE_MAPPING_FLAGS(const TIE_MAPPING_FLAGS& flags)
			{ _u.value = flags._u.value; }
  ~TIE_MAPPING_FLAGS() {};

  bool equal(const TIE_MAPPING_FLAGS& flags) const
			{ return (_u.value == flags._u.value); }
  void set_simd_factor(const UINT8 factor)
			{ _u.bits.factor = factor; }
  UINT8 simd_factor(const UINT8 factor) const { return _u.bits.factor; }

};

class TIE_MACRO {

private:

  class TIE_MACRO_MAPPING {
  private:
    TIE_MACRO*		_mapped_macro;
    TIE_MAPPING_FLAGS	_flags;
    TIE_MACRO_MAPPING	*_next;
  public:
    TIE_MACRO_MAPPING()	: _mapped_macro(NULL), _flags(), _next(NULL){}
    TIE_MACRO_MAPPING(TIE_MACRO* tie_macro,
		      TIE_MAPPING_FLAGS flags,
		      TIE_MACRO_MAPPING *next) :
		      _mapped_macro(tie_macro), _flags(flags), _next(next){}
    ~TIE_MACRO_MAPPING(){}
    TIE_MACRO* tie_macro() const { return _mapped_macro; }
    const TIE_MAPPING_FLAGS& flags() const { return _flags; }
    TIE_MACRO_MAPPING* next() const { return _next; }
  };
  typedef TIE_MACRO_MAPPING* TIE_MACRO_MAPPING_p;

  MEM_POOL *_pool;
  xtensa_isa _isa;
  char*	_name;
  TIE_MACRO_ID	_id;
  int	_num_protos;
  int	_num_temps;
  int	_num_instructions;
  int	_num_labels;
  int	_num_inout_protos;
  int	_num_output_protos;
  int	_num_immed_protos;
  int	_return_proto_index;
  TYPE_ID _output_mtype;
  bool	_output_mtype_set;
  bool	_is_pure;
  bool	_has_side_effect;
  bool	_has_external_effect;
  bool  _reads_memory;
  bool  _writes_memory;
  bool  _has_branch;
  bool  _is_simple_addr_load;
  bool  _is_simple_addr_store;
  int	_simple_addr_load_store_base_proto_index;
  int	_simple_addr_load_store_offset_proto_index;
  int	_simple_addr_load_store_size;
  bool	_pure_and_side_effect_set;
  bool*	_operand_live_in;
  bool*	_temp_live_in;
  TIE_MACRO_MAPPING_p
	_mappings;
  xtensa_proto	_macro;
  TIE_LITCLASS_LIST **_proto_lc_list;
  
  char  proto_inout(const int i) const {
	  return xtensa_proto_operand_inout(_isa,_macro,i); }
  void	set_pure_and_side_effect();
  void	find_litclasses_for_operand (TIE_INFO *tie_info, const char *proto_op_name,
				    TIE_LITCLASS_LIST **lcs);
  void	check_exposed_uses();

public:
  TIE_MACRO() { _pool = NULL; _macro = XTENSA_UNDEFINED;
	  	_num_protos = _num_temps = 0;
		_num_instructions = 0; _return_proto_index = TIE_INVALID_ID;
		_num_inout_protos = 0; _num_output_protos = 0;
		_num_immed_protos = 0;
		_mappings = NULL;
		_pure_and_side_effect_set = false;
		_output_mtype_set = false;
	      }
  TIE_MACRO(MEM_POOL *pool, xtensa_isa isa,
	    xtensa_proto macro, TIE_MACRO_ID id);
  ~TIE_MACRO() {}

  void init_macro_litclass_usage (TIE_INFO *tie_info);
  BOOL immediate_range (UINT op_idx, INT32 *low, INT32 *high);
  BOOL immediate_ok (UINT op_idx, INT64 imm);
  
  char* name() const { return _name; }
  const char *demangled_name () const;
  const TIE_MACRO_ID id() const { return _id; }
  const char *package () const { return xtensa_proto_package(_isa, _macro); }
  int	unique_out_or_inout() const { return _return_proto_index; }
  bool	is_c_function() { return (_return_proto_index!= TIE_INVALID_ID &&
				proto_inout(_return_proto_index)=='o' &&
				!is_conditional_branch()); }
  bool	is_whirl_intrinsic_op() { return (is_pure() && !has_side_effect()); }
  bool  is_instruction_macro(void) const;
  bool  no_output() const { return (_num_output_protos+_num_inout_protos==0); }
  bool	is_pure();
  bool	has_side_effect();
  bool	has_external_effect();
  bool  reads_memory();
  bool  writes_memory();
  bool  has_branch();
  bool  is_simple_addr_load_store();
  bool  is_simple_addr_load();
  bool  is_simple_addr_store();
  int   simple_addr_load_store_base_proto_index();
  int   simple_addr_load_store_offset_proto_index();
  int   simple_addr_load_store_size();
  bool  is_conditional_branch() const { return _num_labels>0; }
  int	label_proto_index(const int i) const;
  const char* return_type_name() const
			{ FmtAssert(_return_proto_index!=TIE_INVALID_ID,
				    ("No return type name"));
			  return proto_type_mangled_name(_return_proto_index);
			}
  TYPE_ID output_mtype(TIE_INFO* tie_info);
  int	operand_index(int inst, int opnd, int& offset) const;
  int	proto_insn_arg_to_opnd(int inst, int arg, int *offsetp);

  int	num_inout_protos() const { return _num_inout_protos; }
  int	num_output_protos() const { return _num_output_protos; }
  int	num_immed_protos() const { return _num_immed_protos; }
  int	num_protos() const { return _num_protos; }
  int	num_temps() const { return _num_temps; }
  int	num_instructions() const { return _num_instructions; }
  int	num_inst_operands(int inst) const;
  int	num_labels() const { return _num_labels; }
  int	whirl_to_proto_index(const int i);
  int	proto_to_whirl_index(const int i);

  TIE_TYPE_ID proto_type_id(const TIE_INFO* tie, const int i) const;
  
  TYPE_ID proto_mtype_id(const TIE_INFO* tie, const int i) const;
  TYPE_ID proto_pointed_mtype_id(const TIE_INFO* tie, const int i) const;
  const char* proto_type_mangled_name(const int i) const;
  const char* proto_type_demangled_name(const int i) const;
  const char* proto_name(const int i) const {
	  		return xtensa_proto_operand_name(_isa,_macro,i); }
  bool  proto_is_in(const int i) const { return proto_inout(i)=='i'; }
  bool  proto_is_out(const int i) const { return proto_inout(i)=='o'; }
  bool  proto_is_inout(const int i) const { return proto_inout(i)=='m'; }
  bool  proto_is_immed(const int i) const;
  bool  proto_has_const_prefix(const int i);
  bool  proto_is_pointer(const int i) const;
  bool  proto_is_label(const int i) const;
  bool	proto_is_live_in(const int i) const { return _operand_live_in[i]; }

  TYPE_ID temp_mtype_id(const TIE_INFO* tie, const int i) const;
  const char* temp_type_name(const int i) const;
  const char* temp_name(const int i) const {
	  		return xtensa_proto_tmp_name(_isa,_macro,i); }
  bool  temp_is_pointer(const int i) const;
  bool	temp_is_live_in(const int i) const { return _temp_live_in[i]; }

  const char* inst_opcode_name(const int i) const
		{ return xtensa_opcode_name(_isa,
			xtensa_proto_insn_opcode(_isa,_macro,i)); }

  void add_mapping(TIE_MACRO* macro, TIE_MAPPING_FLAGS flags);
  TIE_MACRO* find_tie_macro(const TIE_MAPPING_FLAGS flags) const;

  TIE_LITCLASS_LIST *litclass_usage (UINT op_idx) const
  {
    Is_True(op_idx < _num_protos, (""));
    return _proto_lc_list[op_idx];
  }
};

class TIE_INFO {

private:

  MEM_POOL _pool;

  /* Tables to hold user-defined register file, type, conversion, and
     scheduling information. */

  xtensa_ctype*		_ctypes;
  TIE_MACRO**		_macros;

  xtensa_isa _isa;

  int _num_ctypes;
  int _num_macros;
  int _has_tie_branch_macro;

  // this is needed to initialize asm_neg_preg in front end
  // since in Whirl the output from ASM need to be
  // unique negative offset
  int _max_num_output;

  xtensa_ctype _xtfloat_ctype;
  xtensa_opcode _xtfloat_move_opcode;
  bool _xtfloat_ar_converts;

  typedef UTL_Map<TIE_MACRO_p, TIE_MACRO_p, UTL_Map_PtrHash> MACRO_MAP;
  MACRO_MAP* _immed_to_reg_macro_map;
  MACRO_MAP* _reg_to_immed_macro_map;

  void init_immed_to_reg_map(void);

  void init_user_types (void);
  TIE_MACRO_p find_convert_macro(
		   const char* src_type_name,
		   int from_mem, int to_mem,
                   const char* dst_type_name) const;
  TIE_MACRO_p find_convert_macro(
		   const TYPE_ID src_mtype,
		   int from_mem, int to_mem,
                   const TYPE_ID dst_mtype) const;

  // the following is to register the signatures of output mtype
  // for intr ops with multiple outputs
  // new mtype is created for each new signature

  static const int _max_num_scalar_mtypes = 16;
  static const int _max_num_packed_mtypes = 16;

  class TIE_PACKED_MTYPE {
    private:
	char*		_signature;
	TYPE_ID		_packed_mtype;
	TYPE_ID		_scalar_mtype[_max_num_scalar_mtypes];
	int		_num_scalar_mtypes;
    public:
	TIE_PACKED_MTYPE() { _signature=NULL;
				_packed_mtype=MTYPE_UNKNOWN;
				_num_scalar_mtypes=0;
			      }
	~TIE_PACKED_MTYPE() {};
	TYPE_ID	init(char* signature);
	char*	signature() const { return _signature; }
	TYPE_ID	packed_mtype() const { return _packed_mtype; }
	TYPE_ID	scalar_mtype(int i) const { return _scalar_mtype[i]; }
	int	num_scalar_mtypes() const { return _num_scalar_mtypes; }
  };

  TIE_PACKED_MTYPE _packed_mtype[_max_num_packed_mtypes];
  int _num_packed_mtypes;
  int get_packed_mtype_index(TYPE_ID packed_mtype) const;
  TIE_TYPE_ID ctype_id(const char* name) const;

  void init_xtfloat_info ();

  const char *mtype_name (TYPE_ID mid) const;
  TYPE_ID Base_Tie_Type_Name_To_Mtype (const char *base_type_name) const;

public:

  TIE_INFO();
  ~TIE_INFO();

  int num_ctypes() const { return _num_ctypes; }
  TYPE_ID mtype_id(const char* name) const;

  TIE_TYPE_ID xtfloat_ctype_id () const { return _xtfloat_ctype; }
  TYPE_ID xtfloat_mtype_id () const;

  TOP xtfloat_move_topcode () const;
  bool xtfloat_ar_converts () const { return _xtfloat_ar_converts; }

  int ctype_size(const char* name) const
			{ return xtensa_ctype_num_bits(_isa, ctype_id(name)); }
  int ctype_size(const TIE_TYPE_ID id) const
			{ return xtensa_ctype_num_bits(_isa, id); }
  int ctype_byte_size(const char* name) const
			{ return (xtensa_ctype_num_bits(
					_isa, ctype_id(name))+7)/8; }
  int ctype_byte_size(const TIE_TYPE_ID id) const
			{ return (xtensa_ctype_num_bits(_isa, id)+7)/8; }
  int ctype_alignment(const char* name) const
			{ return xtensa_ctype_alignment(_isa, ctype_id(name)); }
  int ctype_alignment(const TIE_TYPE_ID id) const
			{ return xtensa_ctype_alignment(_isa, id); }
  int ctype_byte_alignment(const char* name) const
			{ return (xtensa_ctype_alignment(
					_isa, ctype_id(name))+7)/8; }
  int ctype_byte_alignment(const TIE_TYPE_ID id) const
			{ return (xtensa_ctype_alignment(_isa, id)+7)/8; }
  const char* ctype_name(const TIE_TYPE_ID id) const
				{ return xtensa_ctype_name(_isa,id); }
  const char *ctype_package(const TIE_TYPE_ID id) const
				{ return xtensa_ctype_package(_isa,id); }
  TIE_MACRO_p ctype_move_macro(const TIE_TYPE_ID id) const
			{ return id==XTENSA_UNDEFINED? NULL:
			  tie_macro(xtensa_ctype_move_proto(_isa,id)); }
  TIE_MACRO_p ctype_loadi_macro(const TIE_TYPE_ID id) const
			{ return id==XTENSA_UNDEFINED? NULL:
			  tie_macro(xtensa_ctype_loadi_proto(_isa,id)); }
  TIE_MACRO_p ctype_storei_macro(const TIE_TYPE_ID id) const
			{ return id==XTENSA_UNDEFINED? NULL:
			  tie_macro(xtensa_ctype_storei_proto(_isa,id)); }

  TIE_MACRO_p mtype_move_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false || MTYPE_is_tie_packed(id)? NULL:
		tie_macro(
		xtensa_ctype_move_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_loadi_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false || MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_loadi_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_storei_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false || MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_storei_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_loadiu_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_loadiu_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_storeiu_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_storeiu_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_loadx_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_loadx_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_storex_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_storex_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_loadxu_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_loadxu_proto(_isa,Mtype_To_Tie_Type_Id(id)));}
  TIE_MACRO_p mtype_storexu_macro(const TYPE_ID id) const
	{return MTYPE_is_tie(id)==false|| MTYPE_is_tie_packed(id)? NULL:
		tie_macro
		(xtensa_ctype_storexu_proto(_isa,Mtype_To_Tie_Type_Id(id)));}

  TIE_MACRO_p ctype_rtor_macro(const TIE_TYPE_ID src_id,
			       const TIE_TYPE_ID dst_id) const;
  TIE_MACRO_p ctype_rtom_macro(const TIE_TYPE_ID src_id,
			       const TIE_TYPE_ID dst_id) const;
  TIE_MACRO_p ctype_mtor_macro(const TIE_TYPE_ID src_id,
			       const TIE_TYPE_ID dst_id) const;
  /* fixme the following always return NULL for now */
  TIE_MACRO_p ctype_mtom_macro(const TIE_TYPE_ID src_id,
			       const TIE_TYPE_ID dst_id) const;

  TIE_MACRO_p mtype_rtor_macro(const TYPE_ID src_id,
			       const TYPE_ID dst_id) const;
  TIE_MACRO_p mtype_rtom_macro(const TYPE_ID src_id,
			       const TYPE_ID dst_id) const;
  TIE_MACRO_p mtype_mtor_macro(const TYPE_ID src_id,
			       const TYPE_ID dst_id) const;
  /* fixme the following always return NULL for now */
  TIE_MACRO_p mtype_mtom_macro(const TYPE_ID src_id,
			       const TYPE_ID dst_id) const;

  /* returns the macro which is the register form for the
     input 'tie_macro' which has an immediate form
     return NULL if there is no immedate form macro */
  TIE_MACRO_p immediate_to_register_form(TIE_MACRO_p tie_macro);

  /* returns the macro which is the immediate form for the
     input 'tie_macro' which has a register form
     return NULL if there is no register form macro */
  TIE_MACRO_p register_to_immediate_form(TIE_MACRO_p tie_macro);

  const char *get_demangled_ctype_name (xtensa_ctype ctype) const;
  const char *mangle_ctype_name (const char* nonmangled_name) const;

  int num_macros() const { return _num_macros; }
  TIE_MACRO_ID tie_macro_id(const char* name) const;
  TIE_MACRO_ID tie_macro_id_demangled(const char* name) const;
  TIE_MACRO_p tie_macro(const char* name) const
				{ TIE_MACRO_ID id = tie_macro_id(name);
				  if (id!=TIE_INVALID_ID)
				    return _macros[id];
				  else
				    return NULL;
				}
  TIE_MACRO_p tie_macro(const TIE_MACRO_ID id) const
				{ return _macros[id]; }

  char* macro_name(const TIE_MACRO_ID i) const
				{ return _macros[i]->name(); }

  const char* macro_demangled_name(const TIE_MACRO_ID i) const
                                { return _macros[i]->demangled_name(); }

  const char *get_demangled_macro_name (const char *mangled_name) const;
  const char *mangle_macro_name (const char* nonmangled_name) const;

  TYPE_ID find_output_mtype(TIE_MACRO_p macro);
  TIE_MACRO_p find_tie_macro(const char* macro_name,
			     const TIE_MAPPING_FLAGS flags) const;
  BOOL has_tie_branch_macro() const { return _has_tie_branch_macro; }
  void init_macro_output_mtypes();
  int num_scalar_mtypes(TYPE_ID packed_mtype) const;
  int max_num_output() const { return _max_num_output; }
  TYPE_ID get_scalar_mtype(TYPE_ID packed_mtype, INT index) const;
};


#endif // _TIE_H_

