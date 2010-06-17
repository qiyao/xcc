
/*

  Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.

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
// Class XT_Architecture
// Reserved prefix XT
//////////////////////////////////////////////////
//
// classes to represent register files, instructions, and operands for tie
// instructions.


#ifndef _XTARCH_H_
#define _XTARCH_H_

#include "errors.h"
#include "defs.h"
#include "cxx_memory.h"
#include "xtmap.h"
#include "xtensa-isa.h"
#include "xtensa-tie.h"
#include "topcode.h"

typedef class XT_Architecture *XT_Architecture_p;

//
// XT_RegFile
//

typedef class XT_RegFile *XT_RegFile_p;
typedef const class XT_RegFile *XT_RegFile_cp;

#define XT_REGFILE_FLAG_NO_ALLOC	0x0001	// non-allocatable reg file
#define XT_REGFILE_FLAG_VARIABLE_SIZE	0x0002	// num entries not set at init
#define XT_REGFILE_FLAG_STRETCH_SPLIT_PIPE	0x0004	// special

class XT_RegFile {
private:
  MEM_POOL *_pool;
  ISA_REGCLASS_INFO _info;
  const UINT _flags;
  const ISA_REGCLASS _regclass;
  const UINT _dbg_start;
  TOP _def_topcode;
  mINT16 _first_callee_save_reg;  // registers following this one (including this one) are
                                  // callee-saved registers. Setting to -1 if not used.
  UINT flags (void) const { return _flags; }
  
public:
  XT_RegFile (MEM_POOL *pool, const char *n, const UINT r,
	      const UINT s, const UINT f,
	      const ISA_REGCLASS rc, const UINT dbg_start);

  const ISA_REGCLASS_INFO *info (void) const { return &_info; }
  const char *name (void) const { return _info.name; }
  const char *reg_name (UINT r) const { return _info.reg_name[r]; }
  UINT registers (void) const { return _info.max_regnum - _info.min_regnum + 1; }
  UINT bit_size (void) const { return _info.bit_size; }
  bool allocatable(void) const { return (_flags & XT_REGFILE_FLAG_NO_ALLOC)==0;}
  bool not_allocatable(void) const { return _flags & XT_REGFILE_FLAG_NO_ALLOC; }
  bool variable_size(void) const
  	{ return _flags & XT_REGFILE_FLAG_VARIABLE_SIZE; }
  ISA_REGCLASS regclass (void) const { return _regclass; }
  bool has_split_pipe(void) const {
	  return (_flags & XT_REGFILE_FLAG_STRETCH_SPLIT_PIPE)!=0;}
  void set_def_topcode (TOP top) { _def_topcode = top; }
  void set_reg_name (UINT r, const char* name)
  			{ _info.reg_name[r+_info.min_regnum] = name; }
  TOP def_topcode (void) const { return _def_topcode; }
  UINT debug_number_start (void) const { return _dbg_start; }
  INT16 add_entry(const char* name);
  void set_first_callee_save_reg(INT m) { 
    _first_callee_save_reg = (mINT16)m + _info.min_regnum; }
  bool is_callee_saved (UINT r);

  void print (FILE *file);
};


//
// XT_Litclass
//

typedef class XT_Litclass *XT_Litclass_p;
typedef const class XT_Litclass *XT_Litclass_cp;

class XT_Litclass {
private:
  xtensa_isa _isa;
  xtensa_opcode _xtopc;
  int _xtopnd;
  const ISA_LITCLASS _lc;
  char *_name;
  bool _range_inited, _range_valid;
  INT32 _range_low, _range_high;

  INT64 range_search (UINT start_bit, BOOL mask_bit, INT64 start_range,
		      bool& ok);

public:
  XT_Litclass (MEM_POOL *pool, xtensa_isa isa, xtensa_opcode opc, int opnd,
	       ISA_LITCLASS lc);

  const ISA_LITCLASS litclass (void) const { return _lc; }
  const char *name (void) const { return (const char *)_name; }
  bool value_ok (INT64 val);
  bool range (INT32 *low, INT32 *high);
};


//
// XT_State
//
typedef class XT_State *XT_State_p;
typedef const class XT_State *XT_State_cp;

class XT_State {
private:
  const ISA_REGSUBCLASS _subclass;
  const UINT _size;
  ISA_REGSUBCLASS_INFO _info;
  const bool _is_volatile;
  const bool _is_artificial;
  
public:
  XT_State (const char *name, ISA_REGSUBCLASS rsc,
	    ISA_REGCLASS rc, UINT rc_index, UINT size,
	    bool is_volatile=false, bool is_artificial=false);

  ISA_REGCLASS regclass (void) const { return (ISA_REGCLASS)(_info.rclass); }
  ISA_REGSUBCLASS regsubclass (void) const { return _subclass; }
  const ISA_REGSUBCLASS_INFO *info (void) const { return &_info; }
  UINT bit_size (void) const { return _size; }
  const char* name (void) const { return _info.name; }
  const bool is_volatile (void) const { return _is_volatile; }
  const bool is_artificial (void) const { return _is_artificial; }
};


//
// XT_Operand
//

typedef class XT_Operand *XT_Operand_p;
typedef const class XT_Operand *XT_Operand_cp;

class XT_Operand {
  friend class XT_Architecture;

private:
  XT_RegFile_p _rf;
  XT_State_p _state;
  ISA_OPERAND_VALTYP _info;
  ISA_OPERAND_USE _use;
  
public:
  XT_Operand ();

  XT_RegFile_p regfile (void) const { return _rf; }
  XT_State_p state (void) const { return _state; }
  const ISA_OPERAND_VALTYP *info (void) const { return &_info; } 
  ISA_OPERAND_USE use (void) const { return _use; }
  
  void print (FILE *file, XT_Architecture_p arch) const;
};


//
// XT_Instruction
//

typedef class XT_Instruction *XT_Instruction_p;
typedef const class XT_Instruction *XT_Instruction_cp;

class XT_Instruction {
  friend class XT_Architecture;

private:
  ISA_OPERAND_INFO _op_info;
  ISA_PRINT_INFO _print_info;
  const TOP _top;
  const char *const _name;
  const UINT _size;
  const UINT _mem_size;
  const UINT _numOperands;
  const UINT _numResults;
  const UINT _issueAlignment;
  const UINT _latestDefStage;
  XT_Operand_cp *_operands;
  XT_Operand_cp *_results;
  UINT64 _op_exec_property;
  UINT  _min_slot_count;
  INT	_value_idx;
  INT	_base_idx;
  INT	_offset_idx;


  const UINT _flags;

  BOOL _isLoad, _isStore;
  BOOL _sameRes, _baseUpdate;
  BOOL _isSometimesCopy, _isAlwaysCopy;
  BOOL _isXfer;
  BOOL _isCond;
  BOOL _useSharedResource;
  BOOL _isSimulated;
  BOOL _isGeneric;
  BOOL _hasAggregateOperand;
  BOOL _isNoop;
  BOOL _unknownAddr;
  BOOL _unknownMemData;
  BOOL _unalignStore;
  BOOL _hasSideEffects;
  BOOL _hasTiePort;
  BOOL _hasTieQueue;
  BOOL _hasExportState;
  BOOL _isStreamGet;
  BOOL _isStreamPut;
  BOOL _useSharedFunctionalUnit;
  BOOL _isWBranch;
  
  INT _copySrc, _copyDest;


public:
  XT_Instruction (TOP top,
		  const char *name,
		  UINT size,
		  UINT mem_size,
		  UINT issue_alignment,
		  UINT operands,
		  UINT results,
		  UINT  latest_def_stage,
		  UINT flags);

  UINT flags (void) const { return _flags; }
  
  BOOL is_load (void) const { return _isLoad; }
  BOOL is_store (void) const { return _isStore; }
  BOOL same_res (void) const { return _sameRes; }
  BOOL base_update (void) const { return _baseUpdate; }
  BOOL is_sometimes_copy (void) const { return _isSometimesCopy; }
  BOOL is_always_copy (void) const { return _isAlwaysCopy; }
  BOOL is_xfer (void) const { return _isXfer; }
  BOOL is_cond (void) const { return _isCond; }
  BOOL use_shared_resource (void) const { return _useSharedResource; }
  BOOL is_simulated (void) const { return _isSimulated; }
  BOOL is_generic (void) const { return _isGeneric; }
  BOOL has_multireg_operand (void) const { return _hasAggregateOperand; }
  BOOL is_noop (void) const { return _isNoop; }
  BOOL has_unknown_addr (void) const { return _unknownAddr; }
  BOOL has_unknown_mem_data (void) const { return _unknownMemData; }
  BOOL unalign_store (void) const { return _unalignStore; }
  BOOL has_side_effects (void) const { return _hasSideEffects; }
  BOOL has_export_state (void) const { return _hasExportState; }
  BOOL has_tie_port (void) const { return _hasTiePort; }
  BOOL has_tie_queue (void) const { return _hasTieQueue; }
  BOOL is_stream_get (void) const { return _isStreamGet; }
  BOOL is_stream_put (void) const { return _isStreamPut; }
  BOOL use_shared_functional_unit (void) const
  					{ return _useSharedFunctionalUnit; }
  BOOL is_wbranch() const { return _isWBranch; };
  void set_wbranch() { _isWBranch = TRUE; }
  UINT64 exec_unit_prop() { return _op_exec_property; }
  UINT min_slot_count() { return _min_slot_count; }
  INT  value_idx (void) const { return _value_idx; }
  INT  base_idx (void) const { return _base_idx; }
  INT  offset_idx (void) const { return _offset_idx; }
  INT  Get_Branch_Target_Operand();

  INT copy_src (void) const
  {
    Is_True(_isSometimesCopy || _isAlwaysCopy, (""));
    return _copySrc;
  }
  
  INT copy_dest (void) const
  {
    Is_True(_isSometimesCopy || _isAlwaysCopy, (""));
    return _copyDest;
  }
  
  TOP topcode (void) const { return _top; }
  const char *name (void) const { return _name; }
  UINT size (void) const { return _size; }
  UINT memory_size (void) const { return _mem_size; }
  UINT issue_alignment (void) const { return _issueAlignment; }
  UINT latest_def_stage (void) const { return _latestDefStage; }
  UINT num_operands (void) const { return _numOperands; }
  UINT num_results (void) const { return _numResults; }

  XT_Operand_cp operand (const UINT i) const
  {
    Is_True(i < _numOperands, ("unexpected operand number %d", i));
    return _operands[i];
  }

  XT_Operand_cp result (const UINT i) const
  {
    Is_True(i < _numResults, ("unexpected result number %d", i));
    return _results[i];
  }

  const ISA_OPERAND_INFO *operand_info (void) const { return &_op_info; }
  const ISA_PRINT_INFO *print_info (void) const { return &_print_info; }
  
  void print (FILE *file, XT_Architecture_p arch) const;
};

//
// XT_Architecture
//

typedef class XT_Architecture *XT_Architecture_p;
typedef const class XT_Architecture *XT_Architecture_cp;


class Wide_Branch_Record {
  public:
    XT_Instruction_p xti_br;       // regular branch
    XT_Instruction_p xti_wbr;      // corresponding wide branch
    int              label_opr_ix; // index for branch's target operand
};

class XT_Architecture {
private:
  typedef UTL_Map<const char *, XT_Instruction_p, UTL_Map_StringNoCaseHash> instNameMap;
  typedef UTL_Map<TOP, XT_Instruction_p, UTL_Map_IntHash> instTopMap;
  typedef UTL_Map<const char *, XT_Litclass_p, UTL_Map_PtrHash> lcOpMap;
  typedef UTL_Map<ISA_LITCLASS, XT_Litclass_p, UTL_Map_IntHash> lclcMap;
  typedef UTL_Map<const char *, XT_State_p, UTL_Map_StringNoCaseHash> stateNameMap;
  typedef UTL_Map<ISA_REGSUBCLASS, const ISA_REGSUBCLASS_INFO*, UTL_Map_IntHash> subclassInfoMap;
  typedef UTL_Map<const char *, xtensa_state, UTL_Map_StringHash> isaStateMap;
  typedef UTL_Map<xtensa_opcode, xtensa_ctype, UTL_Map_IntHash> tieTypeMap;
  typedef UTL_Map<UINT32, INT32, UTL_Map_IntHash> uint32int32Map;
  typedef UTL_Map<TOP, Wide_Branch_Record *, UTL_Map_IntHash> wbTopMap;

  typedef struct generic_inst_record {
  				// for recording compatible tops and the
	  			// corresponding top
    TOP   generic_top;
    TOP*  tops;
    int   num_tops;
    struct generic_inst_record* next;
  } Generic_Inst_Record;

  MEM_POOL *_pool;
  BOOL _initialized;
  BOOL _init_failed;
  
  xtensa_isa _isa;
  xtensa_tie _xtie;
  xtie_phase _xtie_post_rewrite;
  xtie_phase _xtie_compiler;

  UINT _macro_cnt;
  UINT _ctype_cnt;
  
  UINT _inst_cnt;
  UINT _max_results, _max_operands;
  XT_Instruction_p _max_operands_op;
  XT_Instruction_p _max_results_op;
  instNameMap _instructionNameMap;
  instTopMap _instructionTopMap;

  UINT _bundle_cnt;
  UINT _exec_unit_cnt;
  UINT _max_num_slots;
  UINT64 *_op_exec_property;

  UINT _regfile_cnt;
  UINT _regclass_cnt, _regsubclass_cnt;
  UINT _state_cnt;
  UINT _maxRegs, _totalRegs;
  XT_RegFile_p *_regfiles;
  XT_RegFile_p _regclass_to_regfile[TI_ISA_REGCLASS_MAX + 1];
  ISA_REGCLASS _state_regclass_begin;
  ISA_REGCLASS _state_regclass_end;
  ISA_REGCLASS _float_regclass;
  ISA_REGCLASS _hifi2_pr_regclass;
  ISA_REGCLASS _hifi2_qr_regclass;
  
  UINT _litclass_cnt;
  lcOpMap _xtopToLcMap;
  lclcMap _lcToLcMap;
  
  XT_RegFile_p _stateRegfile;
  stateNameMap _name_to_state_map;
  subclassInfoMap _subclass_to_subclassInfo_map;
  isaStateMap _nameToIsaStateMap;

  tieTypeMap _tieMoveMap;
  uint32int32Map _sameResOpMap;	// map (top,result) to opnd
  uint32int32Map _sameResResMap;	// map (top,opnd) to result

  UINT _numUpdatingOps;
  UINT _updatingMapSize;
  TOP *_updatingMap;
  UINT _nonUpdatingMapSize;
  TOP *_nonUpdatingMap;

  UINT _numIndexedOps;
  UINT _indexedMapSize;
  TOP *_indexedMap;

  BOOL _has_new_core_opcode;

  TOP* _top_next;
  Generic_Inst_Record* _generic_inst_record_head;

  // TOPs of core branches (not narrow ones) to its wide branch inst
  wbTopMap _wbranchMap; 
  bool     _has_wide_branch;
  INT      _wbranch_range;  // target range for wide branch.

  void initialization();
  void init_format_strings (void);
  void init_types (void);
  void init_bundles (void);
  void init_regfiles (void);
  void init_states (void);
  void record_tie_copies (void);
  void record_updating (void);
  void record_indexed (void);
  void record_multireg_operands(void);
  XT_State_p create_state (const char *name, bool required=false, bool is_artificial=false);
  XT_State_p create_narf_state (const char *regfile_name);
  XT_State_p create_interface_class_state (const int interface_class_id);
  UINT memory_state_bit_size (const char *name);
  void add_operand (XT_Instruction_p inst, XT_Operand_p op, INT print_op_num,
		    char inout, UINT *oCnt, UINT *iCnt);
  void create_operands (xtensa_opcode opNum, XT_Instruction_p inst,
		  	UINT num_operands,
			UINT copySrc, UINT copyDest,
			UINT num_state_ops, XT_State_p *state_ops,
			UINT num_state_res, XT_State_p *state_res);
  XT_Instruction *create_instruction (const xtensa_opcode opNum,
		  		      const TOP topcode, BOOL core);
  void create_regfile_def_instruction (UINT rf_index, const TOP topcode);
  void record_same_res(TOP topcode, INT32 op_index, INT32 res_index);
  ISA_REGSUBCLASS get_regsubclass(ISA_REGCLASS rc, int num_regs, int offset);
  void print_regsubclass(FILE* file, ISA_REGSUBCLASS sc);

  void create_generic_instruction (const TOP tops_array[], int num_tops);
  void init_generic_instructions (void);

public:

  XT_Architecture(MEM_POOL *pool, xtensa_isa isa, xtensa_tie xtie);

  void init_wbranches();
  BOOL initialized (void) const { return _initialized; }
  xtensa_isa isa() { return _isa; }
  xtensa_tie xtie () { return _xtie; }
  xtie_phase xtie_post_rewrite () { return _xtie_post_rewrite; }
  xtie_phase xtie_compiler () { return _xtie_compiler; }

  // this needs to be public as libti needs to create new literal class
  // and to find literal class even after xtarch initialization
  XT_Litclass_p create_litclass (xtensa_opcode isa_opc, int isa_opnd);
  XT_Litclass_p find_litclass (xtensa_opcode isa_opc, int isa_opnd) const;

  UINT num_macros (void) const { return _macro_cnt; }
  UINT num_ctypes (void) const { return _ctype_cnt; }

  UINT num_instructions (void) const { return _inst_cnt; }
  UINT max_inst_results (void) const { return _max_results; }
  UINT max_inst_operands (void) const { return _max_operands; }
  XT_Instruction_p find_instruction (const char *name);
  XT_Instruction_p find_instruction (TOP topcode);

  UINT num_bundles (void) const { return _bundle_cnt; }
  UINT num_exec_units (void) const { return _exec_unit_cnt; }
  UINT num_slots (INT bundle);
  UINT max_num_slots () const { return _max_num_slots; }
  UINT64 exec_unit_prop(TOP topcode);
  UINT64 exec_slot_prop(INT bundle, INT slot);
  TOP noop_opcode(INT bundle, INT slot);
  UINT bundle_bytes (INT bundle);
  const char *bundle_name (INT bundle);

  UINT num_regfiles (void) const { return _regfile_cnt; }
  UINT num_regclasses (void) const { return _regclass_cnt; }
  UINT num_regsubclasses (void) const { return _regsubclass_cnt; }
  UINT max_registers (void) const { return _maxRegs; }
  UINT total_registers (void) const { return _totalRegs; }
  bool has_wide_branch() const { return _has_wide_branch; }
  XT_RegFile_p find_regfile (const char* name);
  XT_RegFile_p find_regfile (ISA_REGCLASS rc);
  const ISA_REGCLASS_INFO *regclass_info (ISA_REGCLASS rc);

  bool multireg_need_expansion(xtensa_opcode isa_opc, UINT opnd);

  ISA_REGCLASS float_regclass (void);
  ISA_REGCLASS hifi2_pr_regclass (void);
  ISA_REGCLASS hifi2_qr_regclass (void);
  BOOL is_state_regclass (ISA_REGCLASS rc);
  ISA_REGCLASS state_regclass (const char *name);
  ISA_REGSUBCLASS state_regsubclass (const char *name);
  XT_State_p find_state (const char *name);
  XT_State_p find_narf_state (const char *regfile_name);
  XT_State_p find_interface_class_state (const int interface_class_id);

  ISA_LITCLASS litclass_max (void) const { return (ISA_LITCLASS)(LC_max + _litclass_cnt); }
  const char *litclass_name (ISA_LITCLASS lc);
  bool litclass_value_ok (ISA_LITCLASS lc, INT64 val);
  bool litclass_range (ISA_LITCLASS lc, INT32 *low, INT32 *high);

  const ISA_REGSUBCLASS_INFO *regsubclass_info (ISA_REGSUBCLASS sc);
  TOP updating_version (TOP topcode) const;
  TOP non_updating_version (TOP topcode) const;
  void set_updating_version (TOP non_update, TOP update);
  UINT num_updating_ops (void) const;
  TOP indexed_version (TOP topcode) const;
  UINT num_indexed_ops (void) const;
  
  INT32 same_res_get_opnd(TOP topcode, UINT32 res_index);
  INT32 same_res_get_res(TOP topcode, UINT32 op_index);

  UINT get_special_tops (TOP generic_top, TOP return_tops[], UINT size);
  UINT get_compatible_tops (TOP top, TOP return_tops[], UINT size);
  TOP get_generic_top (const TOP tops_array[], int num_tops);
  bool top_is_legal_in_format_slot (int format, int slot, TOP topcode);

  TOP  get_wbranch_top_litclass(TOP normal_br_top, ISA_LITCLASS *wbr_lc);
  INT  get_wbranch_range() const { return _wbranch_range; }

};

#endif /* _XTARCH_H_ */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
