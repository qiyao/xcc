
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
#include "libti.h"
#include "trace.h"
#include "xtarch.h"
#include "xtmap.h"
#include "targ_isa_registers.h"
#include "util.h"

#if TI_ISA_REGISTER_MAX > ISA_MAX_REGISTER
#error "TI_ISA_REGISTER_MAX must be <= ISA_MAX_REGISTER"
#endif

#if TI_TIE_SLOTS_MAX > ISA_BUNDLE_MAX_SLOTS
#error "TI_TIE_SLOTS_MAX must be <= ISA_BUNDLE_MAX_SLOTS"
#endif

#if TI_TIE_SLOTS_MAX > (1<< TI_TIE_SLOTS_MAX_BITS)-1
#error "TI_TIE_SLOTS_MAX must be <= (1<< TI_TIE_SLOTS_MAX_BITS)-1"
#endif

#if XTENSA_TIE_VERSION != 1
#error "XTENSA_TIE_VERSION mis-match"
#endif

#define NUM_SPECIAL_REGFILES 1
#define SPECIAL_REGFILE_BASE_NAME "__xt_special__"
const char * special_regfile_names[NUM_SPECIAL_REGFILES] = {
    "__xt_special__0", 
};

#define SPECIAL_REGFILE_ENTRIES 128
#define SPECIAL_REGFILE_BITS_PER_ENTRY 32
    

#if SPECIAL_REGFILE_ENTRIES > TI_ISA_REGISTER_MAX+1
#error "SPECIAL_REGFILE_ENTRIES must be <= TI_ISA_REGISTER_MAX+1"
#endif

#define NUM_STATE_REGFILES 8
#define STATE_REGFILE_BASE_NAME "__xt_state__"
const char * state_regfile_names[NUM_STATE_REGFILES] = {
    "__xt_state__0", 
    "__xt_state__1", 
    "__xt_state__2", 
    "__xt_state__3", 
    "__xt_state__4", 
    "__xt_state__5", 
    "__xt_state__6", 
    "__xt_state__7", 
};

#define NUM_STATE_REGFILE_ENTRIES 128
#define NUM_STATE_REGFILE_BITS_PER_ENTRY 256

#if NUM_STATE_REGFILES * NUM_STATE_REGFILE_ENTRIES < 1024
#error "Not enough room in state register files to meet TIE requirements"
#endif

extern DLL_SHARED BOOL xt_reorder_tieport;
extern DLL_SHARED BOOL xt_flush_tieport;
extern DLL_SHARED BOOL xt_density;
extern DLL_SHARED BOOL xt_flix;

typedef UTL_Map<const char *, int, UTL_Map_PtrHash> opnd2int_Map;
static opnd2int_Map* isMultiRegOperand;

/* The maximum number of operands we know how to print, and format strings to
   print assembly instructions consisting of each number of operands. */
static UINT32 max_print_operands = 0;
static char **formats = NULL;


XT_Architecture::XT_Architecture (MEM_POOL *pool, xtensa_isa isa, xtensa_tie xtie)
  : _pool(pool),
    _isa(isa),
    _xtie(xtie),
    _instructionTopMap(_pool),
    _instructionNameMap(_pool),
    _xtopToLcMap(_pool, 31),
    _lcToLcMap(_pool, 31),
    _op_exec_property(NULL),
    _regsubclass_cnt(0),
    _regfiles(NULL),
    _name_to_state_map(_pool, 31),
    _subclass_to_subclassInfo_map(_pool, 31),
    _nameToIsaStateMap(_pool, 63),
    _tieMoveMap(_pool, 31),
    _sameResOpMap(_pool, 2048),
    _sameResResMap(_pool, 2048),
    _generic_inst_record_head(NULL),
    _wbranchMap(_pool, 31),
    _top_next(NULL)
{
  M_ENTER(1,"XTARCH","XT_Architecture::XT_Architecture");

  _initialized = FALSE;
  _init_failed = FALSE;
  _updatingMapSize = 0;
  _nonUpdatingMapSize = 0;
  _numUpdatingOps = 0;
  _indexedMapSize = 0;
  _numIndexedOps = 0;
  _has_new_core_opcode = FALSE;
  _has_wide_branch = false;

  initialization();
}

void
XT_Architecture::initialization()
{
  /* If we don't have '_isa' or '_xtie' then there is nothing to do. */

  if (!_isa || !_xtie)
    RETURNV;  // initialize == FALSE

  _xtie_post_rewrite = xtie_get_post_rewrite_phase(_xtie);
  _xtie_compiler = xtie_get_compiler_phase(_xtie);
  Is_True(_xtie_compiler && _xtie_post_rewrite,
          ("Failed to initialize libtie phases."));
  
  /* Create the format strings used to emit assembly instructions. */

  init_format_strings();
  
  /* Initialize types and macros. */

  _litclass_cnt = 0;
  init_types();

  /* Initialize register files... */

  init_regfiles();

  /* Initialize states... */
    
  init_states();

  /* Recognize copy instructions for tie types. */

  record_tie_copies();

  if (_init_failed)
    RETURNV;

  /* Recognize multi-register operands */

  record_multireg_operands();

  /* Initialize instructions... */

  _inst_cnt = 0;
  _max_results = _max_operands = 0;
  _max_results_op = _max_operands_op = NULL;
  
  for (UINT i = 0; i < xtensa_isa_num_opcodes(_isa); i++ )
  {
    const char *opcode = xtensa_opcode_name(_isa, i);
    BOOL core = (TI_TOP_Topcode(opcode) != TOP_UNDEFINED);
    BOOL user = xtensa_opcode_is_user(_isa, i);
    if (core && user) {
      // a core instruction over-written by user TIE instruction
      core = false;
      TI_TOP_Topcode_Remove_From_Core(opcode);
    }
    if (!xt_density && core && !strcasecmp(opcode+strlen(opcode)-2, ".n")) {
      // removes narrow instructions if density option is off
    } else if (core) {
      create_instruction(i, TI_TOP_Topcode(opcode), core);
    } else if (create_instruction(i, (TOP)(TOP_count + _inst_cnt), core))
      _inst_cnt++;
  }

  if (xtensa_opcode_lookup(_isa, "nop")==XTENSA_UNDEFINED)
    TI_TOP_Topcode_Remove_From_Core("nop");

  //Is_True(_has_new_core_opcode==FALSE,("Found new core opcode"));

  /* create additional instructions to terminate loadi live-range from the
     top. See PR3991 and PR4386.
  */
  for (UINT i=0; i< _regfile_cnt; i++) {
    if (_regfiles[i]->allocatable()) {
      create_regfile_def_instruction(i,(TOP)(TOP_count+_inst_cnt));
      _inst_cnt++;
    }
  }

  /* All state registers should now be created, so print the regfiles. */
  if (TRACE_IS_ON(2))
  {
    for (UINT i = 0; i < _regfile_cnt; i++)
      _regfiles[i]->print(stderr);
  }

  /* Initialize bundles... */

  init_bundles();

  /* Check if there are wide branches */
  //init_wbranches();

  /* generic opcodes have to be created after init_bundle() to make
   * sure the _op_exec_property are set correctly for each special opcodes
   */
  init_generic_instructions();

  /* Record which loads and stores have updating versions. */

  record_updating();

  /* Record which loads and stores have indexed versions. */

  record_indexed();

  if (_max_operands>ISA_OPERAND_max_operands_limit) {
    XT_Instruction_p inst = _max_operands_op;
    fprintf(stderr,"%s has too many (%d) operands, max is %d\n",
		inst->name(), inst->num_operands(), ISA_OPERAND_max_operands_limit);
    _init_failed = TRUE;
  }

  if (_max_results>ISA_OPERAND_max_results_limit) {
    XT_Instruction_p inst = _max_results_op;
    fprintf(stderr,"%s has too many (%d) results, max is %d\n",
		inst->name(), inst->num_results(), ISA_OPERAND_max_results_limit);
    _init_failed = TRUE;
  }

  if (_init_failed)
    RETURNV;

  _initialized = TRUE;

  RETURNV;
}


void
XT_Architecture::init_format_strings (void)
{
  M_ENTER(1,"XTARCH","XT_Architecture::init_format_strings");

  max_print_operands = 64;
  formats = CXX_NEW_ARRAY(char *, max_print_operands + 1, _pool);

  for (UINT i=0; i < max_print_operands; i++)
  {
    formats[i] = CXX_NEW_ARRAY(char, 8 + 3 * i, _pool);
    strcpy(formats[i], "%s");
    if (i != 0)
      strcat(formats[i], "\t%s");

    for (UINT j = 1; j < i; j++)
      strcat(formats[i], ",%s");

    if (TRACE_IS_ON(2))
      fprintf(stderr, "%s\n", formats[i]);
  }
}


void
XT_Architecture::init_types (void)
{
  M_ENTER(1,"XTARCH","XT_Architecture::init_types");

  _macro_cnt = xtensa_isa_num_protos(_isa);
  _ctype_cnt = xtensa_isa_num_ctypes(_isa);

}

void
XT_Architecture::init_bundles (void)
{
  M_ENTER(1,"XTARCH","XT_Architecture::init_bundles");
  xtensa_insnbuf slotbuf = xtensa_insnbuf_alloc(_isa);

  _bundle_cnt = xtensa_isa_num_formats(_isa);
  _exec_unit_cnt = 0;
  _max_num_slots = 1;

  for (xtensa_format format = 0; format < _bundle_cnt; format++) {
    int num_slots = xtensa_format_num_slots(_isa, format); 
    _exec_unit_cnt += num_slots;
    if (num_slots > _max_num_slots) _max_num_slots = num_slots;

  /* the following is turned off because a NOP depends on the format */
#if 0
    for (int slot = 0; slot < num_slots; slot++) {
      int opc = xtensa_format_slot_nop_opcode(_isa, format, slot);
      if (opc == -1)
	continue;

      const char* opcode = xtensa_opcode_name(_isa, opc);
      XT_Instruction_p inst = find_instruction(opcode);
      if (inst)
	inst->_isNoop = TRUE;
    }
#endif
  }

  /* for combining or's properties into mov.n */ 
  xtensa_opcode opc_mov_n = xtensa_opcode_lookup(_isa, TI_TOP_Name(TOP_mov_n));
  xtensa_opcode opc_or = xtensa_opcode_lookup(_isa, TI_TOP_Name(TOP_or));

  UINT num_opcodes = xtensa_isa_num_opcodes(_isa);
  _op_exec_property = CXX_NEW_ARRAY(UINT64, TOP_count+num_instructions(), _pool);
  for (UINT i = 0; i < num_opcodes; i++ ) {
    const char *opcode = xtensa_opcode_name(_isa, i);
    XT_Instruction_p inst = find_instruction(opcode);
    TOP xt_opc = TI_TOP_Topcode(opcode);
    /* TI_TOP_Topcode may not work for TIE because we haven't initialized 
       xtarch yet. But at the same time, we want to look in the precompiled 
       topcode list first for core TIE. */
    if (xt_opc == TOP_UNDEFINED) {
      if (inst)
	xt_opc = inst->topcode();
      else
	continue;
    }
    xtensa_opcode opc = xtensa_opcode_lookup(_isa, opcode);
    UINT64 current_exec_unit_set = 0;
    UINT64 current_exec_unit = 1;
    UINT min_opcode_slot_count = _max_num_slots;
    // init slot set
    for (xtensa_format format = 0; format < _bundle_cnt; format++) {

      bool valid_format = true;
      // if flix option is turned off, ignore formats longer than
      // 24 bits
      if (!xt_flix) {
        int format_byte_count=xtensa_format_length(_isa,format);
	if (format_byte_count>3)
          valid_format=false;
      }

      int valid_slot_count = 0;
      for (int slot=0; slot < xtensa_format_num_slots(_isa, format); slot++) {
	BOOL valid_slot= valid_format && 
	     ((xtensa_opcode_encode(_isa, format, slot, slotbuf, opc)==0) ||
              ((opc == opc_mov_n) &&
               (xtensa_opcode_encode(_isa, format, slot, slotbuf, opc_or)==0)));

	if (valid_slot) {
	  current_exec_unit_set |= current_exec_unit;
	  valid_slot_count++;
	}
	current_exec_unit <<= 1;
      }
      if (valid_slot_count<min_opcode_slot_count)
	min_opcode_slot_count = valid_slot_count;
    }
    inst->_op_exec_property = current_exec_unit_set;
    inst->_min_slot_count = min_opcode_slot_count;
    _op_exec_property[xt_opc] = current_exec_unit_set;
  }
  xtensa_insnbuf_free(_isa, slotbuf);
}


static ISA_REGCLASS
core_regfile_class (const char *rfn)
{
  extern BOOL xt_booleans;
  extern BOOL xt_mac16;

  if (!strcmp("a", rfn))
    return ISA_REGCLASS_integer;
  else if (xt_booleans && !strcmp("b", rfn))
    return ISA_REGCLASS_branch;
  else if (xt_mac16 && !strcmp("m", rfn))
    return ISA_REGCLASS_macc;
  else {
      int len = strlen(SPECIAL_REGFILE_BASE_NAME);
      if (!strncmp(SPECIAL_REGFILE_BASE_NAME, rfn, len) &&
	  (rfn[len + 1] - '0' >= 0) &&
	  (rfn[len + 1] - '0' < NUM_SPECIAL_REGFILES)) {
	  return ISA_REGCLASS_special;
      }
  }

  return ISA_REGCLASS_UNDEFINED;
}


void
XT_Architecture::init_regfiles (void)
{
  M_ENTER(1,"XTARCH","XT_Architecture::init_regfiles");

  extern BOOL xt_hard_float;
  extern BOOL xt_vectorfpu2005;
  extern BOOL xt_hifi2;

  _regfile_cnt = 0;
  _regclass_cnt = _regsubclass_cnt;
  _state_cnt = 0;
  _totalRegs = _maxRegs = 0;
  _state_regclass_begin = ISA_REGCLASS_UNDEFINED;
  _state_regclass_end = ISA_REGCLASS_UNDEFINED;
  _float_regclass = ISA_REGCLASS_UNDEFINED;
  _hifi2_pr_regclass = ISA_REGCLASS_UNDEFINED;
  _hifi2_qr_regclass = ISA_REGCLASS_UNDEFINED;
  memset(_regclass_to_regfile, 0, sizeof(_regclass_to_regfile));
  

  /* Count the number of register files, including the core ones since they
     can be referred to by tie instructions, and so we must have a regfile
     object for them to reference. */
  
  /* count from libisa includes the 'view' ones such as br2, etc */
  int xt_isa_regfile_cnt = xtensa_isa_num_regfiles(_isa);
  for (xtensa_regfile regfile = 0 ; regfile < xt_isa_regfile_cnt; regfile++) {
    if (xtensa_regfile_num_entries(_isa, regfile) > TI_ISA_REGISTER_MAX) {
	fprintf(stderr, "### This version of XCC supports register files with %d entries or fewer.\n", TI_ISA_REGISTER_MAX);
	fprintf(stderr, "### However, a version which supports bigger register files is available.\n");
	fprintf(stderr, "### Contact your Tensilica Support Representative.\n");
	exit(1);
    }
  }
  xtensa_regfile xtrf;
  
  /* exclude 'view' regfiles from register file count */
  for (xtrf=0; xtrf<xt_isa_regfile_cnt; xtrf++) {
      if (xtensa_regfile_view_parent(_isa, xtrf)==xtrf)
	  _regfile_cnt++;
  }
  
  /* Use special regfiles to represent core state register. Each state is
   * represented as a subclass of the special regclass. 
   */
  
  _regfile_cnt += NUM_SPECIAL_REGFILES;
  
  /* Use NUM_STATE_REGFILES register files to represent tie state.
   * Each state is represented as a subclass of the state regclass.
   * We increment '_regfile_cnt' to include it. */
  
  _regfile_cnt += NUM_STATE_REGFILES;
  
  /* Create a XT_RegFile for each register file. For the core register files,
     we initialize the regclass to match the core regclass; for non-core
     regfiles, we assign a new regclass. */
  
  UINT next_dbg = 0;
  _regfiles = CXX_NEW_ARRAY(XT_RegFile_p, _regfile_cnt, _pool);
  for (UINT i = 0, xtrf=0; i < _regfile_cnt; i++,xtrf++)
    {
      while (xtrf < xt_isa_regfile_cnt &&
	     xtensa_regfile_view_parent(_isa, xtrf)!=xtrf)
	  xtrf++;
	  
      UINT dbg_start = 0;
      BOOL is_state_rf = (i >= (_regfile_cnt - NUM_STATE_REGFILES));

      const char* rf_name;
      UINT rf_num_entries;
      UINT rf_num_bits;
      UINT rf_flags;

      if ((i >= _regfile_cnt - NUM_SPECIAL_REGFILES - NUM_STATE_REGFILES) &&
	  (i <  _regfile_cnt - NUM_STATE_REGFILES)) {
	  rf_name = special_regfile_names[i - (_regfile_cnt - NUM_STATE_REGFILES - NUM_SPECIAL_REGFILES)];
	  rf_num_entries = SPECIAL_REGFILE_ENTRIES;
	  rf_num_bits = SPECIAL_REGFILE_BITS_PER_ENTRY;
	  rf_flags = XT_REGFILE_FLAG_NO_ALLOC;
      } 
      else if (i >= _regfile_cnt - NUM_STATE_REGFILES) {
	  // a state register file
	rf_name = state_regfile_names[i - (_regfile_cnt - NUM_STATE_REGFILES)];
	rf_num_entries = NUM_STATE_REGFILE_ENTRIES;
	rf_num_bits = NUM_STATE_REGFILE_BITS_PER_ENTRY;
	rf_flags = XT_REGFILE_FLAG_NO_ALLOC | XT_REGFILE_FLAG_VARIABLE_SIZE;
      } 
      else {
	  rf_name = xtensa_regfile_shortname(_isa, xtrf);
	  rf_num_entries = xtensa_regfile_num_entries(_isa, xtrf);
	  rf_num_bits = xtensa_regfile_num_bits(_isa, xtrf);
	  rf_flags = ((xtensa_regfile_is_allocatable(_isa, xtrf)==1?
		       0 : XT_REGFILE_FLAG_NO_ALLOC) |
		      (xtensa_regfile_has_split_pipe(_isa, xtrf)==1?
		       XT_REGFILE_FLAG_STRETCH_SPLIT_PIPE : 0));
      }
      
      ISA_REGCLASS rc = core_regfile_class(rf_name);
      if (rc == ISA_REGCLASS_UNDEFINED)
      {
	rc = (ISA_REGCLASS)(ISA_MAX_REGCLASS + 1 + _regclass_cnt);
	dbg_start = next_dbg;
	next_dbg += rf_num_entries;
	_regclass_cnt++;
      }
      
      if (!strcmp(rf_name, "a"))
	rf_num_entries = 16;	/* only use 16 AR registers for compiler */

      _regfiles[i] = CXX_NEW(XT_RegFile(_pool, rf_name, rf_num_entries,
					rf_num_bits, rf_flags, rc, dbg_start),
			     _pool);

      // Record caller/callee save property for TIE regfiles here
      if (rc > ISA_MAX_REGCLASS) {
        INT num_callee_regs = xtensa_regfile_num_callee_saved(_isa, xtrf);
        if (num_callee_regs < 0) {
          // Undefined, all are caller-defined.
          num_callee_regs = 0;
        }

        FmtAssert(rf_num_entries > num_callee_regs, ("Too many callee-saved registers"));
        _regfiles[i]->set_first_callee_save_reg(rf_num_entries - num_callee_regs);
      }
        
      if (is_state_rf)
      {
	if (i==(_regfile_cnt - NUM_STATE_REGFILES)) 
	  _state_regclass_begin = rc;
	else if (i==(_regfile_cnt - 1)) 
	  _state_regclass_end = rc;

      }
      else
        {
          if (xt_hard_float)
          {
            const char *frc = xt_vectorfpu2005 ? "v" : "f";
            if (!strcmp(rf_name, frc))
            {
              _float_regclass = rc;
            }
          }

          if (xt_hifi2)
          {
            if (!strcmp(rf_name, "aep"))
              _hifi2_pr_regclass = rc;
            else if (!strcmp(rf_name, "aeq"))
              _hifi2_qr_regclass = rc;
          }
        }
      
      /* Create a map from regclass to regfile. If we have too many
	 regclasses, then just skip the map init, we will fail later when
	 libti checks the number of regclasses. */
      
      if (rc <= TI_ISA_REGCLASS_MAX)
	_regclass_to_regfile[rc] = _regfiles[i];
	
      _totalRegs += rf_num_entries;
      if (_maxRegs < rf_num_entries)
	_maxRegs = rf_num_entries;
    }
}


void
XT_Architecture::init_states (void)
{
  M_ENTER(1,"XTARCH","XT_Architecture::readStates");

  xtensa_state xtstate;
  for (xtstate=0; xtstate < xtensa_isa_num_states(_isa); xtstate++) {
    const char* state_name = xtensa_state_name(_isa, xtstate);
    if (!strncmp(state_name, "_TIE_", 5))
    {
      TRACE(1,"Skipping state \"" << state_name << "\"");
    }
    else
    {
      TRACE(1,"Inserting state \"" << state_name << "\"");
      _nameToIsaStateMap.insert(state_name, xtstate);
    }
  }

  /* create XT_State for core special registers here before any
   * TIE instructions are processed
   */

  ISA_REGSUBCLASS rsc;
  FORALL_REGSUBCLASS(rsc) {

    const ISA_REGSUBCLASS_INFO *rsc_info = ISA_REGSUBCLASS_Info(rsc);
    const ISA_REGCLASS rc = (ISA_REGCLASS)(rsc_info->rclass);
    if (rc == ISA_REGCLASS_special) {
      const char* name = rsc_info->name;
      if (rsc_info->count!=1)
	DevWarn("Invalid count for special register subclass %s",
			name);
      int special_reg_index = rsc_info->members[0];
      xtensa_state xs = xtensa_state_lookup(_isa, name);
      int num_bits = xtensa_state_num_bits(_isa, xs);

      /* Create a new state... */
      XT_State_p state = CXX_NEW(XT_State(name,
			       rsc,
			       rc, special_reg_index, num_bits),
		    	       _pool);

      _name_to_state_map.insert(name, state);
      _subclass_to_subclassInfo_map.insert(rsc, rsc_info);
    }
  }

  LEAVE;
}

UINT64 
XT_Architecture::exec_unit_prop(TOP topcode) {

  XT_Instruction_p xt_inst = find_instruction(topcode);
  if (xt_inst)
    return xt_inst->exec_unit_prop();

  return 0;
}

UINT64 
XT_Architecture::exec_slot_prop(INT bundle, INT slot)
{
  UINT cnt = 0;
  for (xtensa_format format = 0; format < bundle; format++) 
    cnt += xtensa_format_num_slots(_isa, format); 
  if (slot >= xtensa_format_num_slots(_isa, bundle))
    return 0;
  cnt += slot;
  return (1ULL<<cnt);
}


UINT
XT_Architecture::num_slots(INT bundle)
{
  return xtensa_format_num_slots(_isa, bundle);
}


TOP 
XT_Architecture::noop_opcode(INT bundle, INT slot)
{
  xtensa_opcode opc = xtensa_format_slot_nop_opcode(_isa, bundle, slot);
  if (opc == XTENSA_UNDEFINED)
    return TOP_UNDEFINED;

  const char* opcname = xtensa_opcode_name(_isa, opc);
  return TI_TOP_Topcode(opcname);
}


UINT 
XT_Architecture::bundle_bytes(INT bundle)
{
  return xtensa_format_length(_isa, bundle);
}


const char * 
XT_Architecture::bundle_name(INT bundle)
{
  return xtensa_format_name(_isa, bundle);
}

XT_RegFile_p
XT_Architecture::find_regfile (const char *name)
{
  for (UINT i=0; i < num_regfiles(); i++)
    if (!strcmp(_regfiles[i]->name(), name))
      return _regfiles[i];

  return NULL;
}


XT_RegFile_p
XT_Architecture::find_regfile (ISA_REGCLASS rc)
{
  Is_True(rc <= TI_ISA_Regclass_Last(), (""));
  return _regclass_to_regfile[rc];
}


const ISA_REGCLASS_INFO *
XT_Architecture::regclass_info (ISA_REGCLASS rc)
{
  Is_True(rc <= TI_ISA_Regclass_Last(), (""));
  XT_RegFile_p rf = _regclass_to_regfile[rc];
  return (rf) ? rf->info() : NULL;
}


ISA_REGCLASS
XT_Architecture::float_regclass (void)
{
  return _float_regclass;
}

ISA_REGCLASS
XT_Architecture::hifi2_pr_regclass (void)
{
  return _hifi2_pr_regclass;
}

ISA_REGCLASS
XT_Architecture::hifi2_qr_regclass (void)
{
  return _hifi2_qr_regclass;
}

BOOL
XT_Architecture::is_state_regclass (ISA_REGCLASS rc)
{

  if (_state_regclass_begin <= rc && rc <= _state_regclass_end)
    return TRUE;
  else
    return FALSE;
}

ISA_REGCLASS
XT_Architecture::state_regclass (const char *name)
{
  XT_State_p state = find_state(name);
  if (!state)
    return ISA_REGCLASS_UNDEFINED;

  return state->regclass();
}

ISA_REGSUBCLASS
XT_Architecture::state_regsubclass (const char *name)
{
  XT_State_p state = find_state(name);
  if (!state)
    return ISA_REGSUBCLASS_UNDEFINED;

  return state->regsubclass();
}


XT_State::XT_State (const char *name, ISA_REGSUBCLASS rsc,
		    ISA_REGCLASS rc, UINT reg_index, UINT size,
		    bool is_volatile, bool is_artificial):
  _subclass(rsc), _size(size),
_is_volatile(is_volatile), _is_artificial(is_artificial)
{
  _info.name = name;
  _info.rclass = rc;
  _info.count = 1;
  _info.members[0] = reg_index;
  _info.reg_name[0] = name;
}

XT_State_p
XT_Architecture::find_state (const char *name)
{
  XT_State_p state;
  if (!_name_to_state_map.find(name, &state))
    return NULL;

  return state;
}


void
XT_Architecture::print_regsubclass (FILE* file, const ISA_REGSUBCLASS sc)
{
  const ISA_REGSUBCLASS_INFO* info;
  if (!_subclass_to_subclassInfo_map.find(sc, &info))
    FmtAssert(0,("Missing reg subclass info for sc=%d",sc));

  fprintf(file, "register subclass %d %s\n", sc, info->name);
  fprintf(file, "\tcount = %d\n", info->count);
  fprintf(file, "\tregclass = %d", info->rclass);
  for (UINT i = 0; i < info->count; i++)
  {
    if ((i % 8) == 0)
      fprintf(file, "\n\t");
    fprintf(file, (i == (info->count-1)) ? "%s" : "%s, ", info->reg_name[i]);
  }
  
  fprintf(file, "\n");
}

/* This method creates/returns a register subclass.
   The subclass includes entries in 'rc' that are 'offset' mod ('num_regs').
   If the same subclass was created previously, it will be returned. Otherwise,
   a new one will be created. When 'num_regs' is 1,
   ISA_REGSUBCLASS_UNDEFINED is returned.
*/
ISA_REGSUBCLASS
XT_Architecture::get_regsubclass (
			ISA_REGCLASS rc, int num_regs, int offset)
{

  M_ENTER(1,"XTARCH","XT_Architecture::get_regsubclass_info");

  static UTL_Map<int, ISA_REGSUBCLASS, UTL_Map_IntHash>
						subclass_map(_pool,31);
  const int NUM_REGS_MAX = 99;

  ISA_REGSUBCLASS sc;
  ISA_REGSUBCLASS_INFO *info;
  if (num_regs == 1)
    RETURN ISA_REGSUBCLASS_UNDEFINED;

  FmtAssert(offset<num_regs,
		("Offset (%d) has to be less than %d", offset, num_regs));
  int key =
	rc*(NUM_REGS_MAX+1)*(NUM_REGS_MAX+1)+num_regs*(NUM_REGS_MAX+1)+offset;
  if (subclass_map.find(key, &sc))
    RETURN sc;

  sc = (ISA_REGSUBCLASS)(ISA_MAX_REGSUBCLASS + 1 + _regsubclass_cnt);
  _regsubclass_cnt++;

  FmtAssert(sc <= TI_ISA_REGSUBCLASS_MAX,
	("too many register subclasses, maximum %d", TI_ISA_REGSUBCLASS_MAX+1));

  XT_RegFile_p regfile = _regclass_to_regfile[rc];

  FmtAssert(regfile != NULL, ("Register file not found"));
  FmtAssert(num_regs < (NUM_REGS_MAX+1) && num_regs>1,
		("Illegal register subclass num_regs (%d)",num_regs));

  int total_regs = regfile->registers();
 
  char *name = CXX_NEW_ARRAY(char, strlen(regfile->name())+6, _pool);
  sprintf(name, "%s%d:%d", regfile->name(), num_regs, offset);
  info = CXX_NEW(ISA_REGSUBCLASS_INFO, _pool);
  info->name = name;
  info->rclass = rc;
  info->count = (total_regs)/num_regs;
  int reg_index = offset;
  for (int i=0; i<info->count; i++) {
    info->members[i] = reg_index;
    info->reg_name[i] = regfile->reg_name(reg_index);
    reg_index += num_regs;
  }
  subclass_map.insert(key, sc);
  _subclass_to_subclassInfo_map.insert(sc, info);

  if (TRACE_IS_ON(1))
    print_regsubclass(stderr,sc);

  RETURN sc;
}

/* create a state for a interface class */
XT_State_p
XT_Architecture::create_interface_class_state (const int interface_class_id)
{
  char* state_name = CXX_NEW_ARRAY(char, 22, _pool);
  sprintf(state_name,"_XT_INTF_CL_state_%03d", interface_class_id);
  XT_State_p state = create_state(state_name, /*required=*/ true, /*is_artificial=*/ true);
  CXX_DELETE_ARRAY(state_name,_pool); 
  return state;
}

/* look up the state for a interface class */
XT_State_p
XT_Architecture::find_interface_class_state (const int interface_class_id)
{
  char* state_name = CXX_NEW_ARRAY(char, 22, _pool);
  sprintf(state_name,"_XT_INTF_CL_state_%03d", interface_class_id);
  XT_State_p state = find_state(state_name);
  CXX_DELETE_ARRAY(state_name,_pool);
  return state;
}

/* create a state for a NARF (Non-Allocatable Register File) */
XT_State_p
XT_Architecture::create_narf_state (const char *name)
{
  char* state_name = CXX_NEW_ARRAY(char, strlen(name) + 16, _pool);
  sprintf(state_name,"_XT_NARF_state_%s", name);
  XT_State_p state = create_state(state_name, /*required=*/ true, /*is_artificial=*/ true);
  CXX_DELETE_ARRAY(state_name,_pool);
  return state;
}

/* look up the state for a NARF (Non-Allocatable Register File) */
XT_State_p
XT_Architecture::find_narf_state (const char *name)
{
  char* state_name = CXX_NEW_ARRAY(char, strlen(name) + 16, _pool);
  sprintf(state_name,"_XT_NARF_state_%s", name);
  XT_State_p state = find_state(state_name);
  CXX_DELETE_ARRAY(state_name,_pool);
  return state;
}

XT_State_p
XT_Architecture::create_state (const char *name, bool required, bool is_artificial)
{
  M_ENTER(1,"XTARCH","XT_Architecture::create_state");

  /* If we already have a state called 'name' then use that, otherwise create a
     new state. Return NULL if 'name' is not a state, but is instead a
     function-unit resource. */

  XT_State_p state;
  if (!_name_to_state_map.find(name, &state))
  {
    xtensa_state xtstate=XTENSA_UNDEFINED;
    xtensa_interface xtiface=XTENSA_UNDEFINED;
    int num_bits=0;

    /* If the state is not in the state table, then it is a resource, and if it
       is not required, return NULL to indicate that it is not a state. */

    if (!_nameToIsaStateMap.find(name, &xtstate)) {

      xtiface = xtensa_interface_lookup(_isa, name);
      if (xtiface != XTENSA_UNDEFINED) {
	// a TIE port treated as state by compiler
	num_bits = xtensa_interface_num_bits(_isa, xtiface);
      } else if (required==false)
	RETURN (XT_State_p)NULL;
      else
        num_bits = 32;	// this state is used for aux state created for, e.g. NARF
			// so will not be used, can use any number
    } else
      num_bits = xtensa_state_num_bits(_isa, xtstate);

    /* Some states we don't want to recognize as states:

       1. MEM : In core instructions to indicate a memory access, but we don't
       want it since then we expect schedule information for it but there isn't
       any.

    */
    if (!strcmp(name, "MEM"))
      RETURN (XT_State_p)NULL;

    /* Use a copy of 'name'. */
    char *name_cpy = CXX_NEW_ARRAY(char, strlen(name) + 1, _pool);
    strcpy(name_cpy, name);
    name = name_cpy;
    
    ISA_REGSUBCLASS sc = TI_ISA_Regsubclass(name);

    if (sc!=ISA_REGSUBCLASS_UNDEFINED) {

      // this is actually a system special state
      const ISA_REGSUBCLASS_INFO *info = TI_ISA_Regsubclass_Info (sc);
      ISA_REGCLASS rc = TI_ISA_Regsubclass_Class (info);

      state = CXX_NEW(XT_State(name, sc, rc, /*state_cnt*/0,
			      		     /*num_bits*/num_bits),
		    _pool);
      TRACE(1,"Recognize state \"" << name << "\"" << " as system special state\n");
      _name_to_state_map.insert(name, state);
      _subclass_to_subclassInfo_map.insert(sc, info);
      return state;
    }

    /* we store user state registers in several register files (classes)
     * starting with _state_regclass_begin, with each containing
     * NUM_STATE_REGFILE_ENTRIES states.
     */
    ISA_REGSUBCLASS rsc =
	    (ISA_REGSUBCLASS)(ISA_MAX_REGSUBCLASS + 1 + _regsubclass_cnt);
    ISA_REGCLASS state_rc_index =
	    (ISA_REGCLASS)(_state_cnt / NUM_STATE_REGFILE_ENTRIES);
    ISA_REGCLASS rc =(ISA_REGCLASS)(_state_regclass_begin + state_rc_index);

    int state_reg_index = _regclass_to_regfile[rc]->add_entry(name);
    Is_True(state_reg_index != -1, ("State init error"));
    if (state_reg_index == -1)
      return NULL;

    if (_state_cnt >= NUM_STATE_REGFILE_ENTRIES * NUM_STATE_REGFILES) {
      fprintf(stderr, "TIE user state limit %d exceeded\n",
		    NUM_STATE_REGFILE_ENTRIES * NUM_STATE_REGFILES);
      _init_failed = true;
      return NULL;
    }

    bool is_volatile =	xtstate!=XTENSA_UNDEFINED?
	    			xtensa_state_is_exported(_isa, xtstate):
				(xtiface!=XTENSA_UNDEFINED? true: false);

    /* Create a new state... */
    state = CXX_NEW(XT_State(name, rsc, rc, state_reg_index,
			     num_bits, is_volatile, is_artificial),
		    _pool);

    FmtAssert(state->regsubclass() <= TI_ISA_REGSUBCLASS_MAX,
	      ("too many register subclasses, maximum %d", TI_ISA_REGSUBCLASS_MAX+1));
    _regclass_to_regfile[rc]->set_reg_name(state_reg_index,name);
    
    TRACE(1,"Create state \"" << name << "\"" << " as __xt_state__" << _state_cnt);
    _name_to_state_map.insert(name, state);
    _subclass_to_subclassInfo_map.insert(rsc, state->info());
    _regsubclass_cnt++;
    _state_cnt++;
  }
  
  RETURN state;
}


const ISA_REGSUBCLASS_INFO *
XT_Architecture::regsubclass_info (ISA_REGSUBCLASS sc)
{
  const ISA_REGSUBCLASS_INFO* info;
  if (!_subclass_to_subclassInfo_map.find(sc, &info))
    FmtAssert(FALSE, ("Unexpected regsubclass %d", sc));

  return info;
}


XT_Litclass::XT_Litclass (MEM_POOL *pool, xtensa_isa isa, xtensa_opcode opc,
			  int opnd, ISA_LITCLASS lc) :
  _isa(isa), _xtopc(opc), _xtopnd(opnd), _lc(lc),
  _range_inited(FALSE), _range_valid(FALSE)
{
  char buf[128];

  sprintf(buf, "tielc%d", lc - LC_max);
  _name = CXX_NEW_ARRAY(char, strlen(buf) + 2, pool);
  strcpy(_name, buf);
}


bool
XT_Litclass::value_ok (INT64 val)
{
  UINT32 val32 = val;
  if (xtensa_operand_encode(_isa, _xtopc, _xtopnd, &val32))
    return FALSE;

  return TRUE;
}


INT64
XT_Litclass::range_search (UINT start_bit, BOOL positive, INT64 start_range, bool& ok)
{
  INT64 range = start_range;
  ok = false;

  /* range should be representable in signed int of 32bits
     because xtensa_operand_encode(), used indirectly from
     value_ok(), takes 32bit number only.
   */
  INT i;
  if (positive) {
    for (i=start_bit; i >= 0; i--)
    {
      INT64 lrange = range;
      lrange = lrange | (1 << i);

      if (value_ok(lrange)) {
        ok=true;
        range = lrange;
      }
    }
  } else {
    if (value_ok(range)) {
      ok = true;
    } else {

      /* To find the smallest negative number, use a 
         max positive number and negate it.
       */
      range = 0;
      for (i=start_bit; i >= 0; i--)
      {
        INT64 lrange = range;
        lrange = lrange | (1 << i);
        INT64 lrange1 = - lrange;
        if (value_ok(lrange1)) {
          ok=true;
          range = lrange;
        }
      }
      range = -range;
    }
  }

  return range;
}


bool
XT_Litclass::range (INT32 *low, INT32 *high)
{
  if (!_range_inited)
  {
    bool ok = false;
    _range_inited = TRUE;
    
    /* We try to do the best we can here... unfortunately we don't know if the
       literal is signed or unsigned.
       
       contains immediates are signed and centered (perhaps skewed) on 0, or
       are unsigned (positive or negative) with 0 as a bound. */

    /* Search for the maximum positive literal value. If we don't find one then
       assume the maximum is zero (i.e. range_search will return 0). */

    _range_high = range_search(30, TRUE, 0, ok);

    /* Search for the minimum negative literal value. If we don't find one then
       assume the minimum is zero (i.e. range_search will return 0). */

    _range_low = range_search(30, FALSE, INT32_MIN, ok);

    /* Indicate if we found a range. Leaving '_range_valid' FALSE indicates
       that only 0 is allowed. */

    if (_range_low < _range_high)
      _range_valid = TRUE;
  }
  
  *low = _range_low;
  *high = _range_high;
  return _range_valid;
}


XT_Litclass_p
XT_Architecture::create_litclass (xtensa_opcode isa_opc, int isa_opnd)
{
  /* If we already have a litclass created for 'isa_opnd' then use that,
     otherwise create a new litclass. */

  const char *opnd_name = xtensa_operand_name(_isa, isa_opc, isa_opnd);
  XT_Litclass_p lc;
  if (!_xtopToLcMap.find(opnd_name, &lc))
  {
    lc = CXX_NEW(XT_Litclass(_pool, _isa, isa_opc, isa_opnd, (ISA_LITCLASS)(LC_max + _litclass_cnt)), _pool);

    _xtopToLcMap.insert(opnd_name, lc);
    _lcToLcMap.insert(lc->litclass(), lc);
    _litclass_cnt++;
  }
  
  return lc;
}

XT_Litclass_p
XT_Architecture::find_litclass (xtensa_opcode isa_opc, int isa_opnd) const
{
  /* If we already have a litclass created for 'isa_opnd' then use that,
     otherwise create a new litclass. */

  const char *opnd_name = xtensa_operand_name(_isa, isa_opc, isa_opnd);
  XT_Litclass_p lc;
  if (!_xtopToLcMap.find(opnd_name, &lc))
  {
    return NULL;
  }
  
  return lc;
}


const char *
XT_Architecture::litclass_name (ISA_LITCLASS lc)
{
  XT_Litclass_p xtlc;
  if (!_lcToLcMap.find(lc, &xtlc))
    return NULL;

  return xtlc->name();
}


bool
XT_Architecture::litclass_value_ok (ISA_LITCLASS lc, INT64 val)
{
  XT_Litclass_p xtlc;
  if (!_lcToLcMap.find(lc, &xtlc))
    FmtAssert(FALSE, ("Unexpected litclass %d", lc));

  return xtlc->value_ok(val);
}


bool
XT_Architecture::litclass_range (ISA_LITCLASS lc, INT32 *low, INT32 *high)
{
  XT_Litclass_p xtlc;
  if (!_lcToLcMap.find(lc, &xtlc))
    FmtAssert(FALSE, ("Unexpected litclass %d", lc));

  return xtlc->range(low, high);
}


TOP
XT_Architecture::updating_version (TOP topcode) const
{
  Is_True((int)topcode < _updatingMapSize, ("unexpected topcode %d", topcode));
  return _updatingMap[topcode];
}

TOP
XT_Architecture::non_updating_version (TOP topcode) const
{
  Is_True((int)topcode < _updatingMapSize, ("unexpected topcode %d", topcode));
  return _nonUpdatingMap[topcode];
}


void
XT_Architecture::set_updating_version (TOP non_update, TOP update)
{
  Is_True((int)non_update < _updatingMapSize, ("unexpected topcode %d", non_update));
  Is_True((int)update < _nonUpdatingMapSize, ("unexpected topcode %d", non_update));
  
  if (_updatingMap[non_update] == TOP_UNDEFINED)
  {
    _numUpdatingOps++;
    _updatingMap[non_update] = update;
  }
  if (_nonUpdatingMap[update] == TOP_UNDEFINED)
  {
    _nonUpdatingMap[update] = non_update;
  }
}


UINT
XT_Architecture::num_updating_ops (void) const
{
  return _numUpdatingOps;
}


void
XT_Architecture::record_updating (void)
{
#define MULTI_MAPPINGS ((TOP)-1)
  
  M_ENTER(1, "XTARCH", "XT_Architecture::record_updating");

  /* We identify updating versions of load instructions by finding loadi/loadiu
     prototypes consisting of a single instruction. We then now that the loadiu
     prototype instruction is an updating version of the loadi prototype
     instruction. */

  _updatingMapSize = TOP_count + _inst_cnt;
  _nonUpdatingMapSize = TOP_count + _inst_cnt;
  _updatingMap = CXX_NEW_ARRAY(TOP, _updatingMapSize, _pool);
  _nonUpdatingMap = CXX_NEW_ARRAY(TOP, _nonUpdatingMapSize, _pool);
  for (UINT i = 0; i < _updatingMapSize; i++) {
    _updatingMap[i] = TOP_UNDEFINED;
    _nonUpdatingMap[i] = TOP_UNDEFINED;
  }
  
  for (xtensa_ctype ctype=0; ctype<xtensa_isa_num_ctypes(_isa); ctype++ )
  {
    /* Updating loads... */

    xtensa_proto loadiu_proto = xtensa_ctype_loadiu_proto(_isa, ctype);
    if (loadiu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, loadiu_proto)==1)
      {
	xtensa_opcode loadiu_opcode =
		    xtensa_proto_insn_opcode(_isa, loadiu_proto, 0);
	XT_Instruction_p loadiu_inst = NULL;

	if (loadiu_opcode!=XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  loadiu_inst=find_instruction(xtensa_opcode_name(_isa,loadiu_opcode));
	}
	Is_True(loadiu_inst, ("can't find updating load opcode"));

	loadiu_inst->_baseUpdate = TRUE;
      
	xtensa_proto loadi_proto = xtensa_ctype_loadi_proto(_isa, ctype);
	if (loadi_proto != XTENSA_UNDEFINED)
	{
	  if (xtensa_proto_num_insns(_isa, loadi_proto)==1)
	  {
	    xtensa_opcode loadi_opcode =
		    xtensa_proto_insn_opcode(_isa, loadi_proto, 0);
	    XT_Instruction_p loadi_inst = NULL;

	    if (loadi_opcode!=XTENSA_UNDEFINED) {
	      loadi_inst=
		      find_instruction(xtensa_opcode_name(_isa,loadi_opcode));
	    }
	    Is_True(loadi_inst, ("can't find load opcode"));

	    TOP loadi_top = loadi_inst->topcode();
	    TOP loadiu_top = loadiu_inst->topcode();

	    Is_True((int)loadi_top < _updatingMapSize, ("unexpected topcode %s", loadi_inst->name()));
	    Is_True((int)loadiu_top < _nonUpdatingMapSize, ("unexpected topcode %s", loadiu_inst->name()));

	    if (loadi_inst->has_unknown_addr() || loadiu_inst->has_unknown_addr())
	      continue;

	    if (_updatingMap[loadi_top] != MULTI_MAPPINGS)
	    {
	      if ((_updatingMap[loadi_top] != TOP_UNDEFINED) &&
		  (_updatingMap[loadi_top] != loadiu_top))
	      {
		DevWarn("%s maps to multiple updating topcodes, including %d and %d\n",
			loadi_inst->name(), _updatingMap[loadi_top], loadiu_top);
		_updatingMap[loadi_top] = MULTI_MAPPINGS;
		_nonUpdatingMap[loadiu_top] = MULTI_MAPPINGS;
		_numUpdatingOps--;
	      }
	      else
	      {
		if (loadiu_inst->offset_idx() != -1)
		  _updatingMap[loadi_top] = loadiu_top;
		if (loadi_inst->offset_idx() != -1)
		  _nonUpdatingMap[loadiu_top] = loadi_top;
		_numUpdatingOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Updating Stores... */
    
    xtensa_proto storeiu_proto = xtensa_ctype_storeiu_proto(_isa, ctype);
    if (storeiu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, storeiu_proto)==1)
      {
        xtensa_opcode storeiu_opcode =
		    xtensa_proto_insn_opcode(_isa, storeiu_proto, 0);
	XT_Instruction_p storeiu_inst = NULL;

	if (storeiu_opcode!=XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  storeiu_inst =
		  find_instruction(xtensa_opcode_name(_isa,storeiu_opcode));
	}
	Is_True(storeiu_inst, ("can't find updating store opcode"));

	storeiu_inst->_baseUpdate = TRUE;
      
        xtensa_proto storei_proto = xtensa_ctype_storei_proto(_isa, ctype);
        if (storei_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, storei_proto)==1)
          {
            xtensa_opcode storei_opcode =
		    xtensa_proto_insn_opcode(_isa, storei_proto, 0);
	    XT_Instruction_p storei_inst = NULL;

	    if (storei_opcode!=XTENSA_UNDEFINED) {
	      storei_inst = find_instruction(xtensa_opcode_name(_isa,storei_opcode));
	    }
	    Is_True(storei_inst, ("can't find store opcode"));

	    TOP storei_top = storei_inst->topcode();
	    TOP storeiu_top = storeiu_inst->topcode();

	    Is_True((int)storei_top < _updatingMapSize, ("unexpected topcode %s", storei_inst->name()));
	    Is_True((int)storeiu_top < _nonUpdatingMapSize, ("unexpected topcode %s", storeiu_inst->name()));

	    if (storei_inst->has_unknown_addr() || storeiu_inst->has_unknown_addr())
	      continue;

	    if (_updatingMap[storei_top] != MULTI_MAPPINGS)
	    {
	      if ((_updatingMap[storei_top] != TOP_UNDEFINED) &&
		  (_updatingMap[storei_top] != storeiu_top))
	      {
		DevWarn("%s maps to multiple updating topcodes, including %d and %d\n",
			storei_inst->name(), _updatingMap[storei_top], storeiu_top);
		_updatingMap[storei_top] = MULTI_MAPPINGS;
		_nonUpdatingMap[storeiu_top] = MULTI_MAPPINGS;
		_numUpdatingOps--;
	      }
	      else
	      {
		if (storeiu_inst->offset_idx() != -1)
		  _updatingMap[storei_top] = storeiu_top;
		if (storei_inst->offset_idx() != -1)
		  _nonUpdatingMap[storeiu_top] = storei_top;
		_numUpdatingOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Updating indexed loads... */

    xtensa_proto loadxu_proto = xtensa_ctype_loadxu_proto(_isa, ctype);
    if (loadxu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, loadxu_proto)==1)
      {
	xtensa_opcode loadxu_opcode =
		    xtensa_proto_insn_opcode(_isa, loadxu_proto, 0);
	XT_Instruction_p loadxu_inst = NULL;

	if (loadxu_opcode!=XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  loadxu_inst=find_instruction(xtensa_opcode_name(_isa,loadxu_opcode));
	}
	Is_True(loadxu_inst, ("can't find updating load opcode"));

	loadxu_inst->_baseUpdate = TRUE;
      
        xtensa_proto loadx_proto = xtensa_ctype_loadx_proto(_isa, ctype);
        if (loadx_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, loadx_proto)==1)
          {
	    xtensa_opcode loadx_opcode =
		    xtensa_proto_insn_opcode(_isa, loadx_proto, 0);
	    XT_Instruction_p loadx_inst = NULL;

	    if (loadx_opcode!=XTENSA_UNDEFINED) {
	      loadx_inst=
		      find_instruction(xtensa_opcode_name(_isa,loadx_opcode));
	    }
	    Is_True(loadx_inst, ("can't find load opcode"));

	    TOP loadx_top = loadx_inst->topcode();
	    TOP loadxu_top = loadxu_inst->topcode();

	    Is_True((int)loadx_top < _updatingMapSize, ("unexpected topcode %s", loadx_inst->name()));
	    Is_True((int)loadxu_top < _nonUpdatingMapSize, ("unexpected topcode %s", loadxu_inst->name()));

	    if (loadx_inst->has_unknown_addr() || loadxu_inst->has_unknown_addr())
	      continue;

	    if (_updatingMap[loadx_top] != MULTI_MAPPINGS)
	    {
	      if ((_updatingMap[loadx_top] != TOP_UNDEFINED) &&
		  (_updatingMap[loadx_top] != loadxu_top))
	      {
		DevWarn("%s maps to multiple updating topcodes, including %d and %d\n",
			loadx_inst->name(), _updatingMap[loadx_top], loadxu_top);
		_updatingMap[loadx_top] = MULTI_MAPPINGS;
		_nonUpdatingMap[loadxu_top] = MULTI_MAPPINGS;
		_numUpdatingOps--;
	      }
	      else
	      {
		if (loadxu_inst->offset_idx() != -1)
		  _updatingMap[loadx_top] = loadxu_top;
		if (loadx_inst->offset_idx() != -1)
		  _nonUpdatingMap[loadxu_top] = loadx_top;
		_numUpdatingOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Updating indexed Stores... */
    
    xtensa_proto storexu_proto = xtensa_ctype_storexu_proto(_isa, ctype);
    if (storexu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, storexu_proto)==1)
      {
        xtensa_opcode storexu_opcode =
		    xtensa_proto_insn_opcode(_isa, storexu_proto, 0);
	XT_Instruction_p storexu_inst = NULL;

	if (storexu_opcode!=XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  storexu_inst=
		  find_instruction(xtensa_opcode_name(_isa,storexu_opcode));
	}
	Is_True(storexu_inst, ("can't find updating store opcode"));

	storexu_inst->_baseUpdate = TRUE;
      
        xtensa_proto storex_proto = xtensa_ctype_storex_proto(_isa, ctype);
        if (storex_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, storex_proto)==1)
          {
            xtensa_opcode storex_opcode =
		    xtensa_proto_insn_opcode(_isa, storex_proto, 0);
	    XT_Instruction_p storex_inst = NULL;

	    if (storex_opcode!=XTENSA_UNDEFINED) {
	      storex_inst=find_instruction(xtensa_opcode_name(_isa,storex_opcode));
	    }
	    Is_True(storex_inst, ("can't find store opcode"));

	    TOP storex_top = storex_inst->topcode();
	    TOP storexu_top = storexu_inst->topcode();

	    Is_True((int)storex_top < _updatingMapSize, ("unexpected topcode %s", storex_inst->name()));
	    Is_True((int)storexu_top < _nonUpdatingMapSize, ("unexpected topcode %s", storexu_inst->name()));

	    if (storex_inst->has_unknown_addr() || storexu_inst->has_unknown_addr())
	      continue;

	    if (_updatingMap[storex_top] != MULTI_MAPPINGS)
	    {
	      if ((_updatingMap[storex_top] != TOP_UNDEFINED) &&
		  (_updatingMap[storex_top] != storexu_top))
	      {
		DevWarn("%s maps to multiple updating topcodes, including %d and %d\n",
			storex_inst->name(), _updatingMap[storex_top], storexu_top);
		_updatingMap[storex_top] = MULTI_MAPPINGS;
		_nonUpdatingMap[storexu_top] = MULTI_MAPPINGS;
		_numUpdatingOps--;
	      }
	      else
	      {
		if (storexu_inst->offset_idx() != -1)
		  _updatingMap[storex_top] = storexu_top;
		if (storex_inst->offset_idx() != -1)
		  _nonUpdatingMap[storexu_top] = storex_top;
		_numUpdatingOps++;
	      }
	    }
	  }
	}
      }
    }
  }

  /* Show that multiple updating maps have no update. */

  for (UINT i = 0; i < _updatingMapSize; i++)
  {
    if (_updatingMap[i] == MULTI_MAPPINGS)
      _updatingMap[i] = TOP_UNDEFINED;

    if (_nonUpdatingMap[i] == MULTI_MAPPINGS)
      _nonUpdatingMap[i] = TOP_UNDEFINED;

    if (TRACE_IS_ON(2))
    {
      if (_updatingMap[i] != TOP_UNDEFINED)
	fprintf(stderr, "non-updating %d -> updating %d\n", i, _updatingMap[i]);
      if (_nonUpdatingMap[i] != TOP_UNDEFINED)
	fprintf(stderr, "updating %d -> non-updating %d\n", i, _nonUpdatingMap[i]);
    }
  }
}


TOP
XT_Architecture::indexed_version (TOP topcode) const
{
  Is_True((int)topcode < _indexedMapSize, ("unexpected topcode %d", topcode));
  return _indexedMap[topcode];
}


UINT
XT_Architecture::num_indexed_ops (void) const
{
  return _numIndexedOps;
}


void
XT_Architecture::record_indexed (void)
{
#define MULTI_MAPPINGS ((TOP)-1)
  
  M_ENTER(1, "XTARCH", "XT_Architecture::record_indexed");

  /* We identify indexed versions of load instructions by finding loadi/loadiu
     prototypes consisting of a single instruction. We then now that the loadiu
     prototype instruction is an indexed version of the loadi prototype
     instruction. */

  _indexedMapSize = TOP_count + _inst_cnt;
  _indexedMap = CXX_NEW_ARRAY(TOP, _indexedMapSize, _pool);
  for (UINT i = 0; i < _indexedMapSize; i++)
    _indexedMap[i] = TOP_UNDEFINED;
  
  for (xtensa_ctype ctype =0; ctype < xtensa_isa_num_ctypes(_isa); ctype++)
  {
    /* Indexed loads... */

    xtensa_proto loadx_proto = xtensa_ctype_loadx_proto(_isa, ctype);
    if (loadx_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, loadx_proto)==1)
      {
	xtensa_opcode loadx_opcode =
		xtensa_proto_insn_opcode(_isa, loadx_proto, 0);
	XT_Instruction_p loadx_inst = NULL;

	if (loadx_opcode != XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  loadx_inst = find_instruction(xtensa_opcode_name(_isa,loadx_opcode));
	}
	Is_True(loadx_inst, ("can't find indexed load opcode"));

        xtensa_proto loadi_proto = xtensa_ctype_loadi_proto(_isa, ctype);
        if (loadi_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, loadi_proto)==1)
          {
	    xtensa_opcode loadi_opcode =
		xtensa_proto_insn_opcode(_isa, loadi_proto, 0);
	    XT_Instruction_p loadi_inst = NULL;

	    if (loadi_opcode != XTENSA_UNDEFINED) {
	      loadi_inst=
		      find_instruction(xtensa_opcode_name(_isa,loadi_opcode));
	    }
	    Is_True(loadi_inst, ("can't find load opcode"));

	    TOP loadi_top = loadi_inst->topcode();
	    TOP loadx_top = loadx_inst->topcode();

	    Is_True((int)loadi_top < _indexedMapSize, ("unexpected topcode %s", loadi_inst->name()));

	    if (_indexedMap[loadi_top] != MULTI_MAPPINGS)
	    {
	      if ((_indexedMap[loadi_top] != TOP_UNDEFINED) &&
		  (_indexedMap[loadi_top] != loadx_top))
	      {
		DevWarn("%s maps to multiple indexed topcodes, including %d and %d\n",
			loadi_inst->name(), _indexedMap[loadi_top], loadx_top);
		_indexedMap[loadi_top] = MULTI_MAPPINGS;
		_numIndexedOps--;
	      }
	      else
	      {
		_indexedMap[loadi_top] = loadx_top;
		_numIndexedOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Indexed Stores... */
    
    xtensa_proto storex_proto = xtensa_ctype_storex_proto(_isa, ctype);
    if (storex_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, storex_proto)==1)
      {
	xtensa_opcode storex_opcode =
		xtensa_proto_insn_opcode(_isa, storex_proto, 0);
	XT_Instruction_p storex_inst = NULL;

	if (storex_opcode != XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  storex_inst=find_instruction(xtensa_opcode_name(_isa,storex_opcode));
	}
	Is_True(storex_inst, ("can't find indexed store opcode"));

        xtensa_proto storei_proto = xtensa_ctype_storei_proto(_isa, ctype);
        if (storei_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, storei_proto)==1)
          {
	    xtensa_opcode storei_opcode =
		xtensa_proto_insn_opcode(_isa, storei_proto, 0);
	    XT_Instruction_p storei_inst = NULL;

	    if (storei_opcode != XTENSA_UNDEFINED) {
	      storei_inst=
		      find_instruction(xtensa_opcode_name(_isa,storei_opcode));
	    }
	    Is_True(storei_inst, ("can't find store opcode"));

	    TOP storei_top = storei_inst->topcode();
	    TOP storex_top = storex_inst->topcode();

	    Is_True((int)storei_top < _indexedMapSize, ("unexpected topcode %s", storei_inst->name()));

	    if (_indexedMap[storei_top] != MULTI_MAPPINGS)
	    {
	      if ((_indexedMap[storei_top] != TOP_UNDEFINED) &&
		  (_indexedMap[storei_top] != storex_top))
	      {
		DevWarn("%s maps to multiple indexed topcodes, including %d and %d\n",
			storei_inst->name(), _indexedMap[storei_top], storex_top);
		_indexedMap[storei_top] = MULTI_MAPPINGS;
		_numIndexedOps--;
	      }
	      else
	      {
		_indexedMap[storei_top] = storex_top;
		_numIndexedOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Indexed Updating loads... */

    xtensa_proto loadxu_proto = xtensa_ctype_loadxu_proto(_isa, ctype);
    if (loadxu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, loadxu_proto)==1)
      {
	xtensa_opcode loadxu_opcode =
		xtensa_proto_insn_opcode(_isa, loadxu_proto, 0);
	XT_Instruction_p loadxu_inst = NULL;

	if (loadxu_opcode != XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  loadxu_inst=
		  find_instruction(xtensa_opcode_name(_isa,loadxu_opcode));
	}
	Is_True(loadxu_inst, ("can't find indexed load opcode"));

        xtensa_proto loadiu_proto = xtensa_ctype_loadiu_proto(_isa, ctype);
        if (loadiu_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, loadiu_proto)==1)
          {
	    xtensa_opcode loadiu_opcode =
		xtensa_proto_insn_opcode(_isa, loadiu_proto, 0);
	    XT_Instruction_p loadiu_inst = NULL;

	    if (loadiu_opcode != XTENSA_UNDEFINED) {
	      loadiu_inst = find_instruction(xtensa_opcode_name(_isa,loadiu_opcode));
	    }
	    Is_True(loadiu_inst, ("can't find load opcode"));

	    TOP loadiu_top = loadiu_inst->topcode();
	    TOP loadxu_top = loadxu_inst->topcode();

	    Is_True((int)loadiu_top < _indexedMapSize, ("unexpected topcode %s", loadiu_inst->name()));

	    if (_indexedMap[loadiu_top] != MULTI_MAPPINGS)
	    {
	      if ((_indexedMap[loadiu_top] != TOP_UNDEFINED) &&
		  (_indexedMap[loadiu_top] != loadxu_top))
	      {
		DevWarn("%s maps to multiple indexed topcodes, including %d and %d\n",
			loadiu_inst->name(), _indexedMap[loadiu_top], loadxu_top);
		_indexedMap[loadiu_top] = MULTI_MAPPINGS;
		_numIndexedOps--;
	      }
	      else
	      {
		_indexedMap[loadiu_top] = loadxu_top;
		_numIndexedOps++;
	      }
	    }
	  }
	}
      }
    }

    /* Indexed Updating Stores... */
    
    xtensa_proto storexu_proto = xtensa_ctype_storexu_proto(_isa, ctype);
    if (storexu_proto != XTENSA_UNDEFINED)
    {
      if (xtensa_proto_num_insns(_isa, storexu_proto)==1)
      {
	xtensa_opcode storexu_opcode =
		xtensa_proto_insn_opcode(_isa, storexu_proto, 0);
	XT_Instruction_p storexu_inst = NULL;

	if (storexu_opcode != XTENSA_UNDEFINED) {
	  /* We can't use TI_TOP_Topcode since xtarch is not yet intialized. */
	  storexu_inst=
		  find_instruction(xtensa_opcode_name(_isa,storexu_opcode));
	}
	Is_True(storexu_inst, ("can't find indexed store opcode"));

        xtensa_proto storeiu_proto = xtensa_ctype_storeiu_proto(_isa, ctype);
        if (storeiu_proto != XTENSA_UNDEFINED)
        {
          if (xtensa_proto_num_insns(_isa, storeiu_proto)==1)
          {
	    xtensa_opcode storeiu_opcode =
		xtensa_proto_insn_opcode(_isa, storeiu_proto, 0);
	    XT_Instruction_p storeiu_inst = NULL;

	    if (storeiu_opcode != XTENSA_UNDEFINED) {
	      storeiu_inst=
		      find_instruction(xtensa_opcode_name(_isa,storeiu_opcode));
	    }
	    Is_True(storeiu_inst, ("can't find store opcode"));

	    TOP storeiu_top = storeiu_inst->topcode();
	    TOP storexu_top = storexu_inst->topcode();

	    Is_True((int)storeiu_top < _indexedMapSize, ("unexpected topcode %s", storeiu_inst->name()));

	    if (_indexedMap[storeiu_top] != MULTI_MAPPINGS)
	    {
	      if ((_indexedMap[storeiu_top] != TOP_UNDEFINED) &&
		  (_indexedMap[storeiu_top] != storexu_top))
	      {
		DevWarn("%s maps to multiple indexed topcodes, including %d and %d\n",
			storeiu_inst->name(), _indexedMap[storeiu_top], storexu_top);
		_indexedMap[storeiu_top] = MULTI_MAPPINGS;
		_numIndexedOps--;
	      }
	      else
	      {
		_indexedMap[storeiu_top] = storexu_top;
		_numIndexedOps++;
	      }
	    }
	  }
	}
      }
    }
  }

  /* Show that multiple indexed maps have no index. */

  for (UINT i = 0; i < _indexedMapSize; i++)
  {
    if (_indexedMap[i] == MULTI_MAPPINGS)
      _indexedMap[i] = TOP_UNDEFINED;

    if (TRACE_IS_ON(2))
    {
      if (_indexedMap[i] != TOP_UNDEFINED)
	fprintf(stderr, "non-indexed %d -> indexed %d\n", i, _indexedMap[i]);
    }
  }
}


void
XT_Architecture::record_tie_copies (void)
{
  /* We identify tie opcodes that are copies by looking for a move prototype
     consisting of a single instruction. There are additional constraints on
     weather we can mark the opcode as a move, but we check those in
     create_instruction. */
  
  xtensa_ctype xtct;
  int num_isa_ctypes = xtensa_isa_num_ctypes(_isa);
  for (xtct=0; xtct < num_isa_ctypes; xtct++)
  {
    xtensa_proto xtproto = xtensa_ctype_move_proto(_isa, xtct);
    if (xtproto != XTENSA_UNDEFINED && xtensa_proto_num_insns(_isa, xtproto)==1)
    {
	xtensa_opcode opc = xtensa_proto_insn_opcode(_isa, xtproto, 0);
	if ((opc != XTENSA_UNDEFINED) && !_tieMoveMap.find(opc))
	{
	  _tieMoveMap.insert(opc, xtct);
	}
    }
  }
}


void
XT_Architecture::record_multireg_operands (void)
{
  int num_isa_protos = xtensa_isa_num_protos(_isa);
  int num_insts, inst, num_args, arg, opnd, offset;
  xtensa_proto xt_proto;

  isMultiRegOperand = CXX_NEW(opnd2int_Map(_pool, 31),_pool);
  for (xt_proto = 0; xt_proto < num_isa_protos; xt_proto++) {
    num_insts = xtensa_proto_num_insns(_isa, xt_proto);
    for (inst = 0; inst < num_insts; inst++) {
      num_args = xtensa_proto_insn_num_args(_isa, xt_proto, inst);
      for (arg = 0; arg < num_args; arg++) {
	opnd = xtensa_proto_insn_arg_to_opnd(_isa, xt_proto, inst, arg,
					     &offset);
	if (offset > 0) {
	  xtensa_opcode isa_opc =
	    xtensa_proto_insn_opcode(_isa, xt_proto, inst);
	  const char *opnd_name = xtensa_operand_name(_isa, isa_opc, opnd);
	  isMultiRegOperand->insert(opnd_name, 1);
	}
      }
    }
  }
}


XT_RegFile::XT_RegFile (MEM_POOL *pool, const char *n, const UINT r,
			const UINT s, const UINT f,
			const ISA_REGCLASS rc, const UINT dbg_start) :
  _pool(pool),
  _flags(f),
  _regclass(rc),
  _def_topcode(TOP_UNDEFINED),
  _dbg_start(dbg_start),
  _first_callee_save_reg (-1)
{
  _info.isa_mask = 0xff;
  _info.min_regnum = 0;
  _info.bit_size = s;
  _info.can_store = TRUE;
  _info.multiple_save = FALSE;
  _info.name = n;
  
  if (variable_size())
    _info.max_regnum = -1;
  else {
    _info.max_regnum = r - 1;
    UINT nsz = strlen(_info.name);
    for (UINT i = _info.min_regnum; i <= _info.max_regnum; i++)
    {
      char *rn = CXX_NEW_ARRAY(char, nsz + 5, pool);
      sprintf(rn, "%s%d", _info.name, i);
      _info.reg_name[i] = (const char *)rn;
    }
  }
}

INT16
XT_RegFile::add_entry (const char* name) {

  Is_True(variable_size(), ("Register File Error"));

  if (variable_size()) {
    _info.max_regnum++;
    if (_info.max_regnum<=TI_ISA_REGISTER_MAX) {
      char *rn = CXX_NEW_ARRAY(char, strlen(name) + 1, _pool);
      sprintf(rn, "%s", name);
      _info.reg_name[_info.max_regnum] = (const char *)rn;
      return _info.max_regnum;
    }
  }

  return -1;
}

bool XT_RegFile::is_callee_saved (UINT r)
{
  FmtAssert( (_regclass > ISA_MAX_REGCLASS), ("Shouldn't call this method"));

  return ((_first_callee_save_reg > _info.min_regnum) &&
          (r >= _first_callee_save_reg));
}

XT_Operand::XT_Operand ()
{
  _info.rclass = ISA_REGCLASS_UNDEFINED;
  _info.rsubclass = ISA_REGSUBCLASS_UNDEFINED;
  _info.lclass = LC_UNDEFINED;
  _info.eclass = EC_UNDEFINED;
  _info.size = 0;  // don't have this information...
  _info.flags = 0;
  _use = OU_UNDEFINED;
  _rf = NULL;
  _state = NULL;
}


void
XT_Operand::print (FILE *file, XT_Architecture_p arch) const
{
  if (_info.rclass != ISA_REGCLASS_UNDEFINED)
  {
    fprintf(file, "[ REGISTER  %s",
	    (!_rf) ? "unknown" : ISA_REGCLASS_INFO_Name(_rf->info()));

    if (_info.rsubclass != ISA_REGSUBCLASS_UNDEFINED) {
      if (_state)
        fprintf(file, "  %s", ISA_REGSUBCLASS_INFO_Name(_state->info()));
      else {
        fprintf(file, "  %s %s",
	      ISA_REGSUBCLASS_INFO_Name(arch->regsubclass_info(
		(ISA_REGSUBCLASS)_info.rsubclass)),
	      ((ISA_OPERAND_VALTYP_Is_Continuation(&_info)) ? "cont." : ""));
      }
    }
  }
  else if (_info.lclass != LC_UNDEFINED)
  {
    INT32 min, max;
    if (!arch->litclass_range((ISA_LITCLASS)_info.lclass, &min, &max))
      min = min = 0;
    
    fprintf(file, "[ IMMEDIATE %s %s (range %d - %d)",
	    arch->litclass_name((ISA_LITCLASS)_info.lclass),
	    ((ISA_OPERAND_VALTYP_Is_PCRel(&_info)) ? "PCREL" : ""),
	    min, max);
  }

  switch (_use)
  {
  case OU_loadval:
    fprintf(file, " loadval");
    break;
    
  case OU_storeval:
    fprintf(file, " storeval");
    break;
    
  case OU_base:
    fprintf(file, " base");
    break;
    
  case OU_offset:
    fprintf(file, " offset");
    break;
    
  }

  fprintf(file, " ]");
}


XT_Instruction::XT_Instruction (TOP top,
				const char *name,
				UINT size,
				UINT mem_size,
				UINT issue_alignment,
				UINT operands,
				UINT results,
				UINT latest_def_stage,
				UINT flags) :
  _top(top), _name(name),
  _size(size), _mem_size(mem_size), _issueAlignment(issue_alignment),
  _numOperands(operands), _numResults(results),
  _latestDefStage(latest_def_stage),
  _operands(NULL), _results(NULL),
  _flags(flags),
  _isLoad(FALSE), _isStore(FALSE),
  _isSometimesCopy(FALSE), _isAlwaysCopy(FALSE),
  _sameRes(FALSE), _baseUpdate(FALSE),
  _isXfer(FALSE), _isCond(FALSE),
  _useSharedResource(FALSE),
  _hasAggregateOperand(FALSE),
  _isSimulated(FALSE),
  _isGeneric(FALSE),
  _unknownAddr(FALSE),
  _unknownMemData(FALSE),
  _unalignStore(FALSE),
  _hasSideEffects(FALSE),
  _hasTiePort(FALSE),
  _hasTieQueue(FALSE),
  _hasExportState(FALSE),
  _isStreamGet(FALSE),
  _isStreamPut(FALSE),
  _useSharedFunctionalUnit(FALSE),
  _isNoop(FALSE),
  _isWBranch(FALSE),
  _copySrc(-1), _copyDest(-1),
  _op_exec_property(UINT64_MAX),
  _value_idx(-1), _base_idx(-1), _offset_idx(-1)
{
  _op_info.tie_info = this;
  _print_info.format = "";
}

/*
 If inst has a target (relocatable) operand, return its operand
 number by value; if there isn't one, return -1.
 */
INT
XT_Instruction::Get_Branch_Target_Operand()
{
  if (!is_cond()) return -1;

  const ISA_OPERAND_VALTYP *valtyp;
  for (int i=0; i < num_operands(); i++) {
    valtyp = _operands[i]->info();
    if ( ISA_OPERAND_VALTYP_Is_Literal(valtyp) &&
         ISA_OPERAND_VALTYP_Is_PCRel(valtyp) )
      return i;
  }
  return -1;
}

void
XT_Instruction::print (FILE * file, XT_Architecture_p arch) const
{
  fprintf(file, "%u : %s\n", _top, _name);
  fprintf(file, "size = %u, mem_size = %u issue_alignment=%u\n",
		  _size, _mem_size, _issueAlignment);
  fprintf(file, "_latestDefStage = %d\n", _latestDefStage);
  fprintf(file, "flags = %x\n", _flags);
  fprintf(file, "isLoad = %u, isStore = %u\n", _isLoad, _isStore);
  fprintf(file, "sameRes = %u, baseUpdate = %u\n", _sameRes, _baseUpdate);
  fprintf(file, "isSometimesCopy = %d, isAlwaysCopy = %d\n",
	  _isSometimesCopy, _isAlwaysCopy);
  fprintf(file, "isXfer = %d, isCond = %d\n", _isXfer, _isCond);
  fprintf(file, "useSharedResource = %d\n", _useSharedResource);
  fprintf(file, "isSimulated = %d\n", _isSimulated);
  fprintf(file, "isGeneric = %d\n", _isGeneric);
  fprintf(file, "hasAggregateOperand = %d\n", _hasAggregateOperand);
  fprintf(file, "_unknownAddr = %d\n", _unknownAddr);
  fprintf(file, "_unknownMemData = %d\n", _unknownMemData);
  fprintf(file, "_unalignStore = %d\n", _unalignStore);
  fprintf(file, "_hasSideEffects = %d\n", _hasSideEffects);
  fprintf(file, "_hasTiePort = %d\n", _hasTiePort);
  fprintf(file, "_hasTieQueue = %d\n", _hasTieQueue);
  fprintf(file, "_hasExportState = %d\n", _hasExportState);
  fprintf(file, "_isStreamGet = %d\n", _isStreamGet);
  fprintf(file, "_isStreamPut = %d\n", _isStreamPut);
  fprintf(file, "_useSharedFunctionalUnit = %d\n", _useSharedFunctionalUnit);
  fprintf(file, "isNoop = %d\n", _isNoop);
  if (_isSometimesCopy || _isAlwaysCopy)
    fprintf(file, "copySrc = %u, copyDest = %u\n", _copySrc, _copyDest);

  fprintf(file, "format: %s  ::  ", TI_ISA_Print_Info_Format(print_info()));
  BOOL comma = FALSE;
  for (UINT i = 0; ; i++)
  {
    INT comp = TI_ISA_Print_Info_Comp(print_info(), i);

    if (comp == ISA_PRINT_COMP_end)
    {
      fprintf(stderr, "\n");
      break;
    }

    if (comma)
      fprintf(stderr, ", ");

    if (comp == ISA_PRINT_COMP_name)
    {
      fprintf(stderr, "<name>  ");
    }
    else if ((comp >= ISA_PRINT_COMP_opnd)
	     && (comp < ISA_PRINT_COMP_result))
    {
      fprintf(stderr, "o%d", comp - ISA_PRINT_COMP_opnd);
      comma = TRUE;
    }
    else if ((comp >= ISA_PRINT_COMP_result)
	     && (comp <= ISA_PRINT_COMP_MAX))
    {
      fprintf(stderr, "r%d", comp - ISA_PRINT_COMP_result);
      comma = TRUE;
    }
  }
  
  fprintf(file, "results: %u\n", _numResults);
  for(UINT i=0; i < _numResults ; i++) {
    fputc('\t', file);
    result(i)->print(file, arch);
    fputc('\n', file);
  }

  fprintf(file, "operands: %u\n", _numOperands);
  for(UINT i=0; i < _numOperands ; i++) {
    fputc('\t', file);
    operand(i)->print(file, arch);
    fputc('\n', file);
  }
}
   

void
XT_Architecture::add_operand (XT_Instruction_p inst,
			      XT_Operand_p op, INT print_op_num,
			      char inout, UINT *oCnt, UINT *iCnt)
{
  switch (inout)
  {
  case 'i':
  {
    Is_True(*iCnt < inst->num_operands(), ("too many operands"));
    if (print_op_num >= 0)
      inst->_print_info.comp[print_op_num] = ISA_PRINT_COMP_opnd + *iCnt;
    inst->_operands[(*iCnt)++] = op;
    break;
  }
  
  case 'o':
  {
    Is_True(*oCnt < inst->num_results(), ("too many results"));
    if (print_op_num >= 0)
      inst->_print_info.comp[print_op_num] = ISA_PRINT_COMP_result + *oCnt;
    inst->_results[(*oCnt)++] = op;
    break;
  }
  
  case 'm':
    Is_True((*iCnt < inst->num_operands()) && (*oCnt < inst->num_results()),
	    ("too many operands or results"));
    if (print_op_num >= 0)
      inst->_print_info.comp[print_op_num] = ISA_PRINT_COMP_result + *oCnt;
    inst->_operands[(*iCnt)++] = op;
    inst->_results[(*oCnt)++] = op;
    break;

  default:
    FmtAssert(FALSE, ("unknown access type: %c", inout));
    break;
  }
}


/* determine if an multireg operand in ISA is expanded in C proto
   return TRUE if the proto shows expanded form or FALSE otherwise
*/
bool
XT_Architecture::multireg_need_expansion(xtensa_opcode isa_opc, UINT opnd)
{
    int dummy;
    const char *opnd_name = xtensa_operand_name(_isa, isa_opc, opnd);
    if (!isMultiRegOperand->find(opnd_name, &dummy))
      return FALSE;
    else
      return TRUE;
}

void
XT_Architecture::create_operands (xtensa_opcode opNum, XT_Instruction_p inst,
				  UINT num_isa_operands,
				  UINT copySrc, UINT copyDest,
				  UINT num_state_ops, XT_State_p *state_ops,
				  UINT num_state_res, XT_State_p *state_res)
{
  UINT oCnt = 0, iCnt = 0;

  /* Create operands for the isa registers and literals. */
  
  int load_base = -1;	/* base operand index for load */

  const char *opcode = xtensa_opcode_name( _isa, opNum );

  /* number of operands adjusted for expanded multireg operands in proto */

  INT visible_opnd_index = 0;
  for (UINT j = 0; j < num_isa_operands; j++)
  {
    int num_regs = xtensa_operand_num_regs(_isa, opNum, j);
    char inout = xtensa_operand_inout(_isa, opNum, j);
    bool is_literal = false;
    bool is_areg = false;
    XT_RegFile_p rf = NULL;

    /* Create a generic operand object, we specialize below... */

    XT_Operand_p oper = CXX_NEW(XT_Operand, _pool);

    /* 'oper' is either a literal or a register. For literals, find the literal
       class that represents it, creating it if necessary. For registers find
       the regclass, which have already been created. */
    
    if (xtensa_operand_is_register (_isa, opNum, j) == 0)
    {
      oper->_info.lclass = create_litclass(opNum, j)->litclass();
      if (xtensa_operand_is_PCrelative(_isa, opNum, j) == 1)
	oper->_info.flags |= ISA_OPERAND_INFO_flag_is_pcrel;
      is_literal = true;
    }
    else {
      xtensa_regfile opnd_regfile = xtensa_operand_regfile (_isa, opNum, j);
      const char *opnd_regname = xtensa_regfile_shortname (_isa, opnd_regfile);
      rf = find_regfile(opnd_regname);
      FmtAssert(rf != NULL, ("register operand can't find regfile %s",
			     opnd_regname));
      if (rf->not_allocatable()) {
	oper->_info.lclass = create_litclass(opNum, j)->litclass();
      }

      oper->_rf = rf;
      oper->_info.rclass = rf->regclass();
      oper->_info.size = num_regs * rf->bit_size();
      if (num_regs!=1) {
	const ISA_REGCLASS rc = rf->regclass();
	const ISA_REGSUBCLASS sc=get_regsubclass(rc, num_regs, 0);
	FmtAssert(sc!=ISA_REGSUBCLASS_UNDEFINED,
		("Missing register subclass information"));
        oper->_info.rsubclass = sc;
      } else
        oper->_info.rsubclass = ISA_REGSUBCLASS_UNDEFINED;
      oper->_info.flags |= ISA_OPERAND_INFO_flag_is_register;
      if (!strcmp(opnd_regname, "a"))
	is_areg = true;
    }

    /* For a copy, the copy source and destination must be set to match the
       position in the operands/results list of XT_Instruction, not the isa
       operand position. If the copy destination is not the first result, then
       we can't mark 'inst' as a copy, since xcalibur assumes the copy
       destination is the first result. Set operand properties for loads and
       stores. */
    
    if (inst->is_sometimes_copy() || inst->is_always_copy())
    {
      if (j == copySrc)
	inst->_copySrc = iCnt;
      if (j == copyDest)
      {
	inst->_copyDest = oCnt;
	if (oCnt != 0)
	  inst->_isSometimesCopy = inst->_isAlwaysCopy = FALSE;
      }
    }
    else if (inst->is_xfer() && inout == 'i' &&
	     xtensa_operand_is_PCrelative (_isa, opNum, j) == 1)
    {
      oper->_use = OU_target;
    }


    if (j==xtensa_opcode_ldst_base_operand(_isa, opNum)) {
      inst->_base_idx = j;
      oper->_use = OU_base;
    } else if (j==xtensa_opcode_ldst_offset_operand(_isa, opNum)) {
      inst->_offset_idx = j;
      oper->_use = OU_offset;
    } else if (j==xtensa_opcode_ldst_value_operand(_isa, opNum)) {
      inst->_value_idx = j;
      if (inst->is_load())
        oper->_use = OU_loadval;
      else if (inst->is_store())
        oper->_use = OU_storeval;
    }

    int is_visible = xtensa_operand_is_visible (_isa, opNum, j);
    add_operand(inst, oper, is_visible ? visible_opnd_index + 1 : -1,
		inout, &oCnt, &iCnt);

    if (is_visible)
      visible_opnd_index += 1;

    /* check for multireg operand types that need expansion and create
       additional operands
    */
    if (!is_literal && num_regs >1 && multireg_need_expansion(opNum, j)) {

      /* reset the operand bit size to the register size */
      FmtAssert(rf != NULL, ("register operand can't find regfile %s",
			     xtensa_regfile_shortname
			     (_isa, xtensa_operand_regfile (_isa, opNum, j))));
      oper->_info.size = rf->bit_size();
      for (int offset=1; offset<num_regs; offset++) {
	XT_Operand_p new_oper = CXX_NEW(XT_Operand, _pool);
	*new_oper = *oper;
	const ISA_REGCLASS rc = new_oper->_rf->regclass();
	const ISA_REGSUBCLASS sc=get_regsubclass(rc, num_regs, offset);
	FmtAssert(sc!=ISA_REGSUBCLASS_UNDEFINED,
		  ("Missing register subclass information"));
	new_oper->_info.rsubclass = sc;
	new_oper->_info.flags |= ISA_OPERAND_INFO_flag_is_continuation;
	add_operand(inst, new_oper, -1, inout, &oCnt, &iCnt);
      }
    }
  }

  inst->_print_info.comp[visible_opnd_index + 1] = ISA_PRINT_COMP_end;

  /* Add the state operands and results. */

  for (UINT j = 0; j < num_state_ops; j++)
  {
    XT_Operand_p oper = CXX_NEW(XT_Operand, _pool);

    ISA_REGSUBCLASS sc = state_ops[j]->regsubclass();
    const ISA_REGSUBCLASS_INFO *info;
    _subclass_to_subclassInfo_map.find(sc, &info);
    ISA_REGCLASS rc = TI_ISA_Regsubclass_Class (info);

    oper->_rf = _regclass_to_regfile[rc];
    oper->_state = state_ops[j];
    oper->_info.rclass = rc;
    oper->_info.rsubclass = sc;
    oper->_info.flags |= ISA_OPERAND_INFO_flag_is_register;

    add_operand(inst, oper, -1, 'i', &oCnt, &iCnt);
  }

  for (UINT j = 0; j < num_state_res; j++)
  {
    XT_Operand_p oper = CXX_NEW(XT_Operand, _pool);

    ISA_REGSUBCLASS sc = state_res[j]->regsubclass();
    const ISA_REGSUBCLASS_INFO *info;
    _subclass_to_subclassInfo_map.find(sc, &info);
    ISA_REGCLASS rc = TI_ISA_Regsubclass_Class (info);

    oper->_rf = _regclass_to_regfile[rc];
    oper->_state = state_res[j];
    oper->_info.rclass = rc;
    oper->_info.rsubclass = sc;
    oper->_info.flags |= ISA_OPERAND_INFO_flag_is_register;

    add_operand(inst, oper, -1, 'o', &oCnt, &iCnt);
  }
}

#define _same_res_map_key(top,index) ((top)<<8 | (index))
void
XT_Architecture::record_same_res(TOP topcode, INT32 op_index, INT32 res_index)
{
  UINT32 key = _same_res_map_key(topcode,op_index);
  _sameResResMap.insert(key, res_index);
  key = _same_res_map_key(topcode,res_index);
  _sameResOpMap.insert(key, op_index);
}

INT32
XT_Architecture::same_res_get_opnd(TOP topcode, UINT32 res_index)
{
  const ISA_OPERAND_INFO* info = TI_ISA_Operand_Info(topcode);
  Is_True(res_index < TI_ISA_Op_Results(info),("res_index error"));
  UINT32 key = _same_res_map_key(topcode,res_index);
  INT32 op_index;
  if (_sameResOpMap.find(key, &op_index))
    return op_index;
  return -1;
}

INT32
XT_Architecture::same_res_get_res(TOP topcode, UINT32 op_index)
{
  const ISA_OPERAND_INFO* info = TI_ISA_Operand_Info(topcode);
  Is_True(op_index < TI_ISA_Op_Operands(info),("op_index error"));
  UINT32 key = _same_res_map_key(topcode,op_index);
  INT32 res_index;
  if (_sameResResMap.find(key, &res_index))
    return res_index;
  return -1;
}

int top_compare(const void* p1, const void* p2) {
   TOP* top1 = (TOP*) p1;
   TOP* top2 = (TOP*) p2;
   return (*top1 == *top2)? 0 :
	  (*top1 < *top2)? -1 : 1;
}


void
XT_Architecture::init_wbranches()
{
  const int max_8bit = 0x80;
  int wb_min = 0x7ffffff0;  // big enough than any target

  for (xtie_property_iter it = xtie_get_property_iter(_xtie_compiler);
       it; it = xtie_property_iter_step(it)) {
    xtie_property p = xtie_property_iter_get(it);
    
    if (xtie_property_get_type(p) != XTIE_PROPERTY_SPECIALIZED_OP)
      continue;

    const char *id1 = xtie_property_get_arg_id(p, 0);
    const char *id2 = xtie_property_get_arg_id(p, 1);
    
    XT_Instruction_p xt_inst1 = find_instruction(id1);
    int ix;
    if ( (xt_inst1 != NULL) && TI_TOP_Is_CoreBranch(xt_inst1->topcode()) ) {

      ix = xt_inst1->Get_Branch_Target_Operand();
      if ( ix < 0 ) continue;

      XT_Instruction_p xt_inst2 = find_instruction(id2);
      if ( (xt_inst2 == NULL) || !xt_inst2->is_cond() ) continue;

      int ix_tmp = xt_inst2->Get_Branch_Target_Operand();
      if ( ix_tmp != ix ) continue;

      ISA_LITCLASS ltc1 = ISA_OPERAND_VALTYP_Literal_Class (
                           xt_inst1->operand(ix)->info());
      ISA_LITCLASS ltc2 = ISA_OPERAND_VALTYP_Literal_Class (
                           xt_inst2->operand(ix)->info());

      int max1, max2, min1, min2;
      BOOL b1 = TI_ISA_LC_Range(ltc1, &min1, &max1);
      BOOL b2 = TI_ISA_LC_Range(ltc2, &min2, &max2);
      if (b1 && b2 && (max2 > max1) && (max2 > max_8bit)) {
        /* xt_inst2 is a wide branch, not narrow one! */
        if (max2 < wb_min) {
          wb_min = max2;
        }
        Wide_Branch_Record *wbr = CXX_NEW(Wide_Branch_Record, _pool);
        wbr->xti_br  = xt_inst1;
        wbr->xti_wbr = xt_inst2;
        wbr->label_opr_ix = ix;

        _wbranchMap.insert(xt_inst1->topcode(), wbr);

        _has_wide_branch = true;
        xt_inst2->set_wbranch();
      }
    }
  }

  if (_has_wide_branch) {
    _wbranch_range = (INT)wb_min;
  }
}

TOP
XT_Architecture::get_wbranch_top_litclass(TOP normal_br_top, ISA_LITCLASS *lc)
{
  Wide_Branch_Record *wbr;
  *lc = LC_UNDEFINED;
  if ( _wbranchMap.find(normal_br_top, &wbr)) {
    const ISA_OPERAND_VALTYP *valtyp = wbr->xti_wbr->operand(wbr->label_opr_ix)->info();
    *lc = ISA_OPERAND_VALTYP_Literal_Class(valtyp);
    return wbr->xti_wbr->topcode();
  }
  return TOP_UNDEFINED;
}

/* Digest the specialized_op info and create generic opcodes
 * corresponding to combination of specialized opcodes.
 * It is done in 2 steps:
 *   1. Scan the specialized_op properties and chain all compatible topcodes
 *      together.
 *   2. For each sets of compatible topcodes, construct the power sets
 *      and create a generic opcode for each of the power sets.
 */

void
XT_Architecture::init_generic_instructions ()
{
  TOP top_array[100];
  TOP tops[100];

  _generic_inst_record_head = NULL;

  UINT top_count = TOP_count + _inst_cnt;
  TOP* top_prev = CXX_NEW_ARRAY(TOP, top_count, _pool);
  _top_next = CXX_NEW_ARRAY(TOP, top_count, _pool);
  for (UINT i = 0; i < top_count; i++) {
    top_prev[i] = (TOP)i;
    _top_next[i] = (TOP)i;
  }

  for (xtie_property_iter it = xtie_get_property_iter(_xtie_compiler);
       it; it = xtie_property_iter_step(it)) {
    xtie_property p = xtie_property_iter_get(it);
    
    if (xtie_property_get_type(p) != XTIE_PROPERTY_SPECIALIZED_OP)
      continue;

    const char *id1 = xtie_property_get_arg_id(p, 0);
    const char *id2 = xtie_property_get_arg_id(p, 1);

    /* Ignore specialized opcode property on virtual opcodes that
       haven't been instantiated in any slots. We rely here on the
       fact that an opcode that appears in the tie, but not in
       libisa is virtual. An alternate way of calculating this
       would be to use libtie's xtie_opcode_is_virtual, but that
       would involve adding a lot more information to the compiler
       tie and therefore slow things down a lot.  */
    
    xtensa_opcode opc1 = xtensa_opcode_lookup(_isa, id1);
    if (opc1 == XTENSA_UNDEFINED)
      continue;

    xtensa_opcode opc2 = xtensa_opcode_lookup(_isa, id2);
    if (opc2 == XTENSA_UNDEFINED)
      continue;
    
    XT_Instruction_p xt_inst1 = find_instruction(id1);
    TOP top1 = xt_inst1 ? xt_inst1->topcode() : TOP_UNDEFINED;
    
    XT_Instruction_p xt_inst2 = find_instruction(id2);
    TOP top2 = xt_inst2 ? xt_inst2->topcode() : TOP_UNDEFINED;
    
    Is_True(top1 != TOP_UNDEFINED,
            ("Unknown specialized topcode %s", id1));
    Is_True(top2 != TOP_UNDEFINED,
            ("Unknown specialized topcode %s", id2));
    
    TOP next_top = _top_next[top1];
    TOP prev_top = top_prev[top2];
    _top_next[top1] = top2;
    top_prev[top2] = top1;
    _top_next[prev_top] = next_top;
    top_prev[next_top] = prev_top;
  }

  if (xt_density) {
    for (UINT i = 1; i < TOP_count; i++) {
      TOP top1 = (TOP)i;
      XT_Instruction_p xt_inst1 = find_instruction(top1);

      // core instruction can be removed due to name conflict with user inst
      // or options
      if (xt_inst1==NULL)
	continue;

      if (top1 == TOP_break || top1 == TOP_ill ||
	  top1 == TOP_nop || top1 == TOP_ret ||
	  top1 == TOP_retw)
	continue;

      const char* inst1_name = xt_inst1->name();
      int len1 = strlen(inst1_name);
      char inst2_name[256];

      // avoid array overflow
      if (len1+3 > 256)
        continue;

      sprintf(inst2_name,"%s.n", inst1_name);

      // check if narrow version exists
      XT_Instruction_p xt_inst2 = find_instruction(inst2_name);

      // break and break.n has different number of operands
      if (xt_inst2==NULL ||
	  xt_inst1->num_operands() != xt_inst2->num_operands() ||
	  xt_inst1->num_results() != xt_inst2->num_results())
        continue;
    
      TOP top2 = xt_inst2->topcode();
      TOP next_top = _top_next[top1];
      TOP prev_top = top_prev[top2];
      _top_next[top1] = top2;
      top_prev[top2] = top1;
      _top_next[prev_top] = next_top;
      top_prev[next_top] = prev_top;

    }
  }

  for (UINT i = 0; i < top_count; i++) {
    if (top_prev[i]==i)
      continue;

    TOP top = (TOP)i;
    TOP prev_top = (TOP)i;
    int count = 0;
    do {
      top_array[count++] = top;
      prev_top = top_prev[top];
      top_prev[top] = top;
      top = prev_top;
    } while (top != i);

    Is_True(count<8, ("Too many compatible TOPs for %s",
			find_instruction(top)->name()));
    if (count>=8)
      continue;

    UINT mask = (1 << count) - 1;
    for (UINT j=1; j<=mask; j++) {
      UINT num_tops = TARG_INT_Pop_Count(j);
      if (num_tops<2)
	continue;

      UINT k = 0;
      UINT kk = 0;
      UINT mask_j=j;
      while (mask_j) {
	if (mask_j & 1)
	  tops[k++] = top_array[kk];
	mask_j >>= 1;
	kk++;
      }

      FmtAssert(k==num_tops, ("k != num_tops"));
      create_generic_instruction(tops, num_tops);
    }
  }
}

/* Given a <generic_top>, return the specialized topcodes in <return_tops[]>
 * that realize the <generic_top>. <return_tops[]> is allocated by the caller
 * and its size indicated by <size>.
 */

UINT
XT_Architecture::get_special_tops (TOP generic_top,
				   TOP return_tops[], UINT size)
{
  XT_Instruction_p xt_inst = find_instruction(generic_top);
  if (xt_inst==NULL || xt_inst->is_generic()==false)
    return 0;

  Generic_Inst_Record* scan = _generic_inst_record_head;
  while (scan && scan->generic_top != generic_top)
    scan = scan->next;

  if (!scan || size<scan->num_tops)
    return 0;

  UINT num_tops = scan->num_tops;
  for (int i=0; i<num_tops; i++) {
    return_tops[i] = scan->tops[i];
  }

  return num_tops;
}

/* Given a real topcode <top>, return the sets of compatible real topcodes in
 * <return_tops[]>. The returned set should be checked to see if some of them
 * can replace <top> in an CG op. If there are more than one that can replace
 * <top> in an CG op, we can get a generic topcode and use it in the CG op
 * in order to delay the topcode decision until bundling is done.
 * <return_tops[]> is allocated by the caller and its size indicated by <size>.
 */

UINT
XT_Architecture::get_compatible_tops (TOP top, TOP return_tops[], UINT size)
{
  FmtAssert(size>0,("array size too small"));

  XT_Instruction_p xt_inst = find_instruction(top);
  if (xt_inst==NULL)
    return 0;

  if (xt_inst->is_generic()) {
    TOP generic_top = top;
    Generic_Inst_Record* scan = _generic_inst_record_head;
    while (scan && scan->generic_top != generic_top)
      scan = scan->next;

    Is_True(scan!=NULL,
	("Missing generic top information for %s",TI_TOP_Name(generic_top)));

    if (scan && scan->num_tops>0)
      top = scan->tops[0];
  }

  xt_inst = find_instruction(top);
  Is_True (xt_inst && xt_inst->is_generic()==false,
		("Unexpected generic top %s", TI_TOP_Name(top)));

  if (xt_inst->is_generic())
    return 0;

  TOP scan = top;
  UINT count = 0;
  do {
    return_tops[count++] = scan;
    scan = _top_next[scan];
  } while (scan != top && count<size);

  return count;

}

/* Given a set of real topcodes, find the generic topcode that represent this
 * set. The generic topcode is allowed to be bundled in any format/slot
 * that accepts one of the real topcodes
 */

TOP
XT_Architecture::get_generic_top (const TOP tops_array[], int num_tops)
{
  TOP top = TOP_UNDEFINED;
  TOP* tops = CXX_NEW_ARRAY(TOP, num_tops, _pool);
  for (int i=0; i<num_tops; i++) {
    tops[i] = tops_array[i];
  }
  qsort(tops, num_tops, sizeof(TOP), top_compare);

  Generic_Inst_Record* scan = _generic_inst_record_head;
  while (scan) {
    if (scan->num_tops == num_tops) {
      bool matches = true;
      for (int i=0; i<num_tops; i++) {
	if (tops[i] != scan->tops[i]) {
	  matches = false;
	  break;
	}
      }
      if (matches) {
	top = scan->generic_top;
	break;
      }
    }
    scan = scan->next;
  }
  CXX_DELETE_ARRAY(tops, _pool);
  return top;
}

/* create a generic opcode which represent a set of specialized opcodes.
 * The format of all topcodes in tops_array[] has to match, except for the
 * ranges of immediate operands. The generic opcode will choose the largest
 * range. All instruction properties have to match for all topcodes.
 * The scheduling information for the generic topcode is constructed
 * in xtmicroarch.
 */
void
XT_Architecture::create_generic_instruction (const TOP tops_array[],
					     int num_tops)
{
  M_ENTER(2, "XTARCH", "XT_Architecture::create_generic_def_instruction" );

  TOP* tops = CXX_NEW_ARRAY(TOP, num_tops, _pool);
  for (int i=0; i<num_tops; i++) {
    tops[i] = tops_array[i];
  }
  qsort(tops, num_tops, sizeof(TOP), top_compare);

  TOP generic_top = (TOP)(TOP_count + _inst_cnt);
  bool compatible = true;
  
  XT_Instruction_p inst0 = find_instruction(tops[0]);
  
  const char* opcode_name = inst0->name();
  char* generic_opcode_name = CXX_NEW_ARRAY(char,strlen(opcode_name)+10,_pool);
  sprintf(generic_opcode_name,"%s_gen%d", opcode_name, generic_top);
  
  ISA_EXEC_UNIT_PROPERTY op_exec_property = inst0->exec_unit_prop();

  int base_idx = inst0->base_idx();
  int offset_idx = inst0->offset_idx();
  int value_idx = inst0->value_idx();
  ISA_OPERAND_USE value_use = OU_UNDEFINED;
  if (value_idx != -1)
    value_use = inst0->operand(value_idx)->use();
  
  bool has_unknown_addr = inst0->has_unknown_addr();
  bool has_unknown_mem_data = inst0->has_unknown_mem_data();
  bool unalign_store = inst0->unalign_store();
  bool has_side_effects = inst0->has_side_effects();
  bool has_tie_port = inst0->has_tie_port();
  bool has_tie_queue = inst0->has_tie_queue();
  bool has_export_state = inst0->has_export_state();
  bool is_stream_get = inst0->is_stream_get();
  bool is_stream_put = inst0->is_stream_put();
  bool use_shared_functional_unit = inst0->use_shared_functional_unit();

  UINT issue_alignment = inst0->issue_alignment();
  INT latest_def_stage = inst0->latest_def_stage();

  for (int i=1; i<num_tops && compatible; i++) {
    XT_Instruction_p inst = find_instruction(tops[i]);
    op_exec_property |= inst->exec_unit_prop();

    Is_True(inst0->size() == inst->size(), ("size mismatch"));
    Is_True(inst0->memory_size() == inst->memory_size(),
		    			("memory_size mismatch"));
    Is_True(inst0->num_operands() == inst->num_operands(),
		    			("num_operands mismatch"));
    Is_True(inst0->num_results() == inst->num_results(),
		    			("num_results mismatch"));
    Is_True((inst0->flags() == inst->flags()), ("flags mismatch"));
    Is_True(inst0->is_load() == inst->is_load(), ("is_load mismatch"));
    Is_True(inst0->is_store() == inst->is_store(), ("is_store mismatch"));
    Is_True(inst0->same_res() == inst->same_res(), ("same_res mismatch"));
    Is_True(inst0->base_update() == inst->base_update(),
		    			("base_update mismatch"));
    Is_True(inst0->is_sometimes_copy() == inst->is_sometimes_copy(),
		    			("is_sometimes_copy mismatch"));
    Is_True(inst0->is_always_copy() == inst->is_always_copy(),
		    			("is_always_copy mismatch"));
    Is_True(inst0->is_xfer() == inst->is_xfer(), ("is_xfer mismatch"));
    Is_True(inst0->is_cond() == inst->is_cond(), ("is_cond mismatch"));
    Is_True(inst0->is_simulated() == false, ("is_simulated mismatch"));
    Is_True(inst0->is_generic() == false, ("is_generic mismatch"));
    Is_True(inst0->has_multireg_operand() == inst->has_multireg_operand(),
		    			("has_multiple_operand mismatch"));
    if (inst->has_unknown_addr()==false)
      has_unknown_addr = false;
    if (inst->has_unknown_mem_data()==false)
      has_unknown_mem_data = false;
    if (inst->unalign_store())
      unalign_store = true;
    if (inst->has_side_effects())
      has_side_effects = true;
    if (inst->has_tie_port())
      has_tie_port = true;
    if (inst->has_tie_queue())
      has_tie_queue = true;
    if (inst->has_export_state())
      has_export_state = true;
    if (!inst->is_stream_get())
      is_stream_get = false;
    if (!inst->is_stream_put())
      is_stream_put = false;
    if (inst->use_shared_functional_unit())
      use_shared_functional_unit = true;
    if (inst->base_idx() != -1)
      base_idx = inst->base_idx();
    if (inst->offset_idx() != -1)
      offset_idx = inst->offset_idx();
    if (inst->value_idx() != -1) {
      value_idx = inst->value_idx();
      value_use = inst->operand(value_idx)->use();
    }

    if (inst->issue_alignment() > issue_alignment)
      issue_alignment = inst->issue_alignment();
    if (inst->latest_def_stage() > latest_def_stage)
      latest_def_stage = inst->latest_def_stage();

#if 0
    if (inst0->has_unknown_addr() != inst->has_unknown_addr() ||
	inst0->has_unknown_mem_data() != inst->has_unknown_mem_data() ||
	inst0->has_side_effects() != inst->has_side_effects())
      compatible = false;
#endif
  }


  XT_Instruction_p inst = CXX_NEW(XT_Instruction(generic_top,
						 generic_opcode_name,
						 inst0->size(), /* length */
						 inst0->memory_size(), /* memsize */
						 issue_alignment,
						 inst0->num_operands(), /* num opnd */
						 inst0->num_results(), /* num result */
						 latest_def_stage,
						 inst0->flags() /* flags */),
						 _pool);
  inst->_op_exec_property=op_exec_property;
  inst->_isLoad = inst0->is_load();
  inst->_isStore = inst0->is_store();
  inst->_sameRes = inst0->same_res();
  inst->_baseUpdate = inst0->base_update();
  inst->_isSometimesCopy = inst0->is_sometimes_copy();
  inst->_isAlwaysCopy = inst0->is_always_copy();
  inst->_isXfer = inst0->is_xfer();
  inst->_isCond = inst0->is_cond();
  inst->_isSimulated = false;
  inst->_isGeneric = true;
  inst->_unknownAddr = has_unknown_addr;
  inst->_unknownMemData = has_unknown_mem_data;
  inst->_unalignStore = unalign_store;
  inst->_hasSideEffects = has_side_effects;
  inst->_hasTiePort = has_tie_port;
  inst->_hasTieQueue = has_tie_queue;
  inst->_hasExportState = has_export_state;
  inst->_isStreamGet = is_stream_get;
  inst->_isStreamPut = is_stream_put;
  inst->_useSharedFunctionalUnit = use_shared_functional_unit;
  inst->_base_idx = base_idx;
  inst->_offset_idx = offset_idx;
  inst->_value_idx = value_idx;

  if (inst->_isSometimesCopy || inst->_isAlwaysCopy) {
    inst->_copySrc = inst0->copy_src();
    inst->_copyDest = inst0->copy_dest();
  }

  inst->_operands = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_operands(), _pool);
  inst->_results = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_results(), _pool);

  memcpy(&(inst->_print_info), &(inst0->_print_info), sizeof(ISA_PRINT_INFO));
  
  _max_results = Max(_max_results, inst->num_results());
  _max_operands = Max(_max_operands, inst->num_operands());
  if (_max_operands == inst->num_operands())
    _max_operands_op = inst;
  if (_max_results == inst->num_results())
    _max_results_op = inst;
  
  for (int i=0; i<inst0->num_operands() && compatible; i++) {

    // find the best operand among the special instruction to use
    // if one of them is designated with the operand usage info
    XT_Operand_cp candidate_oper = NULL;
    for (int j=0; j<num_tops; j++) {
	XT_Instruction_p inst = find_instruction(tops[j]);
	XT_Operand_cp oper = inst->_operands[i];
	if (candidate_oper==NULL || oper->use() != OU_UNDEFINED)
	  candidate_oper = oper;
    }
    inst->_operands[i] = candidate_oper;
    const ISA_OPERAND_VALTYP* info = candidate_oper->info();
    if ((info->flags & ISA_OPERAND_INFO_flag_is_register)==0)
    {
      INT32 low = INT32_MIN;
      INT32 high = INT32_MAX;
      int top_found = -1;
      for (int j=0; j<num_tops; j++) {
	XT_Instruction_p inst = find_instruction(tops[j]);
	XT_Operand_cp j_oper = inst->_operands[i];
	const ISA_OPERAND_VALTYP* j_info = j_oper->info();
	ISA_LITCLASS lc = (ISA_LITCLASS)(j_oper->_info.lclass);
	if ((info->flags & ISA_OPERAND_INFO_flag_is_register)!=0 ||
	    lc == LC_UNDEFINED) {
	  compatible = false;
	  break;
	}
	INT32 new_low; 
	INT32 new_high; 
	litclass_range(lc, &new_low, &new_high);
	if (low<=new_low && new_high<=high) {
	  top_found = j;
	  low = new_low;
	  high = new_high;
	} else {
	  if ((new_low<=low && new_high>=high)==false) {
	    compatible = false;
	    break;
	  }
	}
      }

      if (!compatible)
	break;

      if (top_found != -1) {

	XT_Operand_cp old_oper = find_instruction(tops[top_found])->operand(i);
	inst->_operands[i] = old_oper;

	// if we are going to loose the operand usage info then we need to
	// make a copy of the operand and keep the usage info
	if (old_oper->use() != candidate_oper->use()) {
	  XT_Operand_p oper = CXX_NEW(XT_Operand, _pool);
	  memcpy(oper, old_oper, sizeof(XT_Operand));
	  oper->_use = candidate_oper->use();
	  inst->_operands[i] = oper;
	}
      } else {
	compatible = false;
	break;
      }
    }
  }

  for (int i=0; i<inst0->num_results() && compatible; i++) {
    // find the best result among the special instruction to use
    // if one of them is designated with the result usage info
    XT_Operand_cp candidate_result = NULL;
    for (int j=0; j<num_tops; j++) {
	XT_Instruction_p inst = find_instruction(tops[j]);
	XT_Operand_cp result = inst->_results[i];
	if (candidate_result==NULL || result->use() != OU_UNDEFINED)
	  candidate_result = result;
    }
    inst->_results[i] = candidate_result;
  }

  if (compatible==false) {
    if (TRACE_IS_ON(2)) {
      fprintf(stderr, "Generic instruction creation failed for: ");
      for (int j=0; j<num_tops; j++) {
        fprintf(stderr, "%s ", find_instruction(tops[j])->name());
      }
      fprintf(stderr, "\n");
    }

    CXX_DELETE(inst,_pool);

    RETURNV;
  }

  _inst_cnt++;
  if (TRACE_IS_ON(2)) {
    fprintf(stderr, "Generic instruction %s created for: ", inst->name());
    for (int j=0; j<num_tops; j++) {
      fprintf(stderr, "%s ", find_instruction(tops[j])->name());
    }
    fprintf(stderr, "\n");
    inst->print(stderr, this);
  }

  Generic_Inst_Record *generic_record = CXX_NEW(Generic_Inst_Record, _pool);
  generic_record->generic_top = generic_top;
  generic_record->num_tops = num_tops;
  generic_record->tops = tops;
  generic_record->next = _generic_inst_record_head;
  _generic_inst_record_head = generic_record;

  _instructionNameMap.insert(generic_opcode_name, inst);
  _instructionTopMap.insert(generic_top, inst);

  RETURNV;
}

/* create a simulated opcode which defines a single regfile operand */
void
XT_Architecture::create_regfile_def_instruction (
	UINT rf_index, const TOP topcode)
{
  M_ENTER(2, "XTARCH", "XT_Architecture::create_regfile_def_instruction" );

  char* opcode_name =
	CXX_NEW_ARRAY(char,strlen(_regfiles[rf_index]->name())+10,_pool);
  sprintf(opcode_name,"def_%s",_regfiles[rf_index]->name());
  XT_Instruction_p inst = CXX_NEW(XT_Instruction(topcode,
						 opcode_name,
						 1, /* length */
						 0, /* memsize */
						 1, /* issue_alignment */
						 0, /* num opnd */
						 1, /* num result */
						 1, /* latest def */
						 0 /* flags */),
						 _pool);
  inst->_isLoad = false;
  inst->_isStore = false;
  inst->_sameRes = false;
  inst->_isSometimesCopy = false;
  inst->_isAlwaysCopy = false;
  inst->_isXfer = false;
  inst->_isCond = false;
  inst->_useSharedResource = false;
  inst->_isSimulated = true;
  inst->_isGeneric = false;
  inst->_unknownAddr = false;
  inst->_unknownMemData = false;
  inst->_unalignStore = false;
  inst->_hasSideEffects = false;
  inst->_hasTiePort = false;
  inst->_hasTieQueue = false;
  inst->_hasExportState = false;
  inst->_isStreamGet = false;
  inst->_isStreamPut = false;
  inst->_useSharedFunctionalUnit = false;

  inst->_operands = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_operands(), _pool);
  inst->_results = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_results(), _pool);

  inst->_print_info.format = formats[1];
  inst->_print_info.comp[0] = ISA_PRINT_COMP_name;
  
  _instructionNameMap.insert(opcode_name, inst);
  _instructionTopMap.insert(topcode, inst);

  _max_results = Max(_max_results, inst->num_results());
  _max_operands = Max(_max_operands, inst->num_operands());
  if (_max_operands == inst->num_operands())
    _max_operands_op = inst;
  if (_max_results == inst->num_results())
    _max_results_op = inst;
  
  /* Add the isa operands/results followed by the state operands/results. For
     copies, set copy source and destination to be the correct indices into the
     operands or results list. */

  XT_Operand_p oper = CXX_NEW(XT_Operand, _pool);
  XT_RegFile_p rf = _regfiles[rf_index];
  oper->_rf = rf;
  oper->_info.rclass = rf->regclass();
  oper->_info.flags |= ISA_OPERAND_INFO_flag_is_register;

  oper->_info.size = rf->bit_size();
  oper->_info.rsubclass = ISA_REGSUBCLASS_UNDEFINED;
  oper->_info.flags |= ISA_OPERAND_INFO_flag_is_register;

  UINT oCnt=0;
  UINT iCnt=0;

  add_operand(inst, oper, 1, 'o', &oCnt, &iCnt);

  inst->_print_info.comp[2] = ISA_PRINT_COMP_end;

  if (TRACE_IS_ON(2))
    inst->print(stderr, this);

  rf->set_def_topcode(inst->topcode());
  
  RETURNV;
}

bool
XT_Architecture::top_is_legal_in_format_slot (int format, int slot, TOP topcode)
{
  xtensa_insnbuf slotbuf = xtensa_insnbuf_alloc(_isa);
  xtensa_opcode opc = xtensa_opcode_lookup(_isa, TI_TOP_Name(topcode));
  bool ret;
  if (xtensa_opcode_encode(_isa, format, slot, slotbuf, opc) == 0) {
    ret = true;
  } else {
    ret = false;
  }
  xtensa_insnbuf_free(_isa, slotbuf);
  return ret;
}


/*
 * XCC operand limits : This is checked by TC as well
 *
 * There are 64 input operands ("operands" in xcc) maximum.
 * There are 64 output operands ("results" in xcc) maximum.
 *
 * States and Regfile operands and all other opreands (immediates, table, etc.)
 * are counted as excpected.  They count as 1 input if of type in, 1 output if of 
 * type out, or 1 input & 1 output if of type inout.
 *
 * Exceptions to states & regfiles:
 * Export states are a special case - they always count as 1 input & 1 output,
 * whether they are of type in, out, or inout.
 * Non-allocatable register files of type in (or inout) count as 2 inputs.
 * Non-allocatable register files of type out (or inout) count as 2 outputs.
 *
 * Interfaces (non-external, e.g. VAddr, MemDataOut64, MemDataIn32) count as 0.
 * External interfaces (e.g. import wires & queues, both control and data wires)
 * count as 2 inputs & 2 outputs.  This is just how the compiler works.
 *
 */

#undef _same_res_map_key
XT_Instruction *
XT_Architecture::create_instruction (const xtensa_opcode opNum, const TOP topcode,
	       			     BOOL core)
{
#define MAX_STATE_OPS 128

  M_ENTER(2, "XTARCH", "XT_Architecture::create_instruction" );

  UINT flags = 0;
  BOOL isLoad = false, isStore = false;
  BOOL sameRes = false, baseUpdate = false;
  BOOL isSometimesCopy = false, isAlwaysCopy = false;
  BOOL isXfer = false;
  BOOL isCond = false;
  BOOL useSharedResource = false;
  BOOL hasAggregateOperand = false;
  UINT copySrc = 0, copyDest = 0;
  UINT latestDefStage = 0;
  BOOL hasUnknownAddr = false;
  BOOL hasUnknownMemData = false;
  BOOL isUnalignStore = false;
  BOOL hasSideEffects = false;
  BOOL hasTiePort = false;
  BOOL hasTieQueue = false;
  BOOL hasExportState = false;
  BOOL isStreamGet = false;
  BOOL isStreamPut = false;
  BOOL useSharedFunctionalUnit = false;
  UINT memory_size = 0;	/* '0' means unknown size */
    
  UINT num_state_ops = 0, num_state_res = 0;
  XT_State_p state_ops[TI_ISA_OPERANDS_MAX], state_res[TI_ISA_RESULTS_MAX];
  
  UINT numIsaOperands = xtensa_opcode_num_operands(_isa, opNum );
  const char *opcode = xtensa_opcode_name( _isa, opNum );

  if (!strncasecmp(opcode, "RSR.",4) || !strncasecmp(opcode, "WSR.",4) ||
      !strncasecmp(opcode, "XSR.",4)) {
    if (!strcasecmp(opcode+4, "CCOUNT") || !strcasecmp(opcode+4, "ICOUNT") ||
	!strcasecmp(opcode+4, "SCOMPARE1")) {
      hasSideEffects = TRUE;
    } else {
      const char* state_name = opcode+4;
      xtensa_sysreg sysreg = xtensa_sysreg_lookup_name(_isa, state_name);
      if (sysreg != XTENSA_UNDEFINED && xtensa_sysreg_is_user (_isa, sysreg)==0) {
	int sysreg_num = xtensa_sysreg_number(_isa, sysreg);
	if (sysreg_num != XTENSA_UNDEFINED && 64 <= sysreg_num && sysreg_num <=255) {
	  hasSideEffects = TRUE;
	}
      }
    }
  }

  UINT issue_alignment = xtensa_opcode_issue_alignment(_isa, opNum);

  /* PR4686. Check if this opcode is a new opcode that we need to hard-code to
     the built-in opcode list
  */
#if 0
  Is_True(core == ((flags & XT_TIE_CORE_OPCODE)!=0),
	  ("Unexpected core opcode %s", opcode));
#endif
  if (core != xtensa_opcode_has_possible_side_effect(_isa, opNum)) {
    if (!core) {
      fprintf(stderr, "Warning: inconsistent core flag for %s in libti\n", opcode);
      _has_new_core_opcode = true;
    }
  }

  if (xtensa_opcode_is_load(_isa, opNum)) {
    isLoad = true;
    if (xtensa_opcode_is_stream_op(_isa, opNum))
      isStreamGet = true;
  }
  if (xtensa_opcode_is_store(_isa, opNum)) {
    isStore = true;
    if (xtensa_opcode_is_stream_op(_isa, opNum))
      isStreamPut = true;
  }
  FmtAssert(!(isLoad && isStore),
	 ("Cannot handle opcode %s that is both load and store yet.",opcode));
  
  if (xtensa_opcode_num_funcUnit_uses(_isa, opNum)>0)
    useSharedFunctionalUnit = true;

  if (xtensa_opcode_is_base_update(_isa, opNum))
    baseUpdate = true;
  
  if (isLoad || isStore) {

    if (xtensa_opcode_ldst_base_operand(_isa,opNum)!= XTENSA_UNDEFINED)
      hasUnknownAddr=false;
    else
      hasUnknownAddr=true;

    if (xtensa_opcode_ldst_value_operand(_isa,opNum)!= XTENSA_UNDEFINED)
      hasUnknownMemData=false;
    else
      hasUnknownMemData=true;

    if (xtensa_opcode_ldst_bytes(_isa,opNum)!= XTENSA_UNDEFINED)
      memory_size = xtensa_opcode_ldst_bytes(_isa,opNum);

    /* Load with byte disable will have unknown mem data */
    /* Store with byte disable will be unalign store with unknown mem data */
    if (xtensa_opcode_has_byte_disable(_isa, opNum)) {
      hasUnknownMemData=true;
      if (isStore) {
	isUnalignStore=true;
      }
    }
  }

  if (xtensa_opcode_is_branch(_isa, opNum) == 1 ||
      xtensa_opcode_is_jump(_isa, opNum) == 1) {
    isXfer = true;
    // FIXME: tie doesn't correctly report unconditional branches
    // as unconditional, so unconditionals will look conditional.
    isCond = xtensa_opcode_is_branch(_isa, opNum) == 1;
  }

  int num_state_operands = xtensa_opcode_num_stateOperands(_isa, opNum);
  if (num_state_operands == XTENSA_UNDEFINED) {
    _init_failed = true;
    RETURN (XT_Instruction*)NULL;
  }
  int i;
  for (i=0; i<num_state_operands; i++) {
    xtensa_state state_operand = xtensa_stateOperand_state(_isa, opNum, i);
    if (state_operand == XTENSA_UNDEFINED) {
      _init_failed = true;
      RETURN (XT_Instruction*)NULL;
    }
    char inout = xtensa_stateOperand_inout(_isa, opNum, i);
    const char* state_name = xtensa_state_name(_isa, state_operand);
    XT_State_p state = create_state(xtensa_state_name(_isa, state_operand));
    if (inout=='o' || inout=='m') {
      UINT def_stage = xtensa_stateOperand_def_stage(_isa, opNum, i);
      if (state)
      {
	FmtAssert(num_state_res < TI_ISA_RESULTS_MAX,
		  ("too many results, max = %d\n", TI_ISA_RESULTS_MAX));
	state_res[num_state_res++] = state;

	if (state->is_volatile()) {
          hasExportState = true;
	}
      }
      if (def_stage > latestDefStage)
	latestDefStage = def_stage;
    }
    if (inout=='i' || inout=='m' || state->is_volatile()) {
      if (state)
      {
	FmtAssert(num_state_ops < TI_ISA_OPERANDS_MAX,
		  ("too many operands, max = %d\n", TI_ISA_OPERANDS_MAX));
	state_ops[num_state_ops++] = state;
      }
		    
      if (!strncmp(state_name,"_ext_resource",13)) {
	useSharedResource = true;
      }
    }
  }

  /* check for any implicit operand/result and create special state usage
     for each non-allocatable register file operand/result
  */
  int num_operands = xtensa_opcode_num_operands(_isa, opNum);
  if (num_operands == XTENSA_UNDEFINED) {
    _init_failed = true;
    RETURN (XT_Instruction*)NULL;
  }
  for (i=0; i< num_operands; i++) {
    if (xtensa_operand_is_register(_isa, opNum, i)) {
      xtensa_regfile reg_file = xtensa_operand_regfile(_isa, opNum, i);
      const char *reg_file_name = xtensa_regfile_shortname (_isa, reg_file);
      XT_RegFile_p rf = find_regfile(reg_file_name);
      char inout = xtensa_operand_inout(_isa, opNum, i);
      if (inout == 'o' || inout == 'm') {
	UINT def_stage = xtensa_operand_def_stage(_isa, opNum, i);
	if (def_stage > latestDefStage)
	  latestDefStage = def_stage;
      }
      if ((inout == 'o' || inout == 'm') && rf->not_allocatable()) {
	/* a non-allocatable register file operand */
	XT_State_p state = create_narf_state(reg_file_name);
	FmtAssert(state, ("Unable to create implicit state for non-allocatable"
			  "register file %s", reg_file_name));
	FmtAssert(num_state_res < MAX_STATE_OPS,
		  ("too many state results, max = %d\n", MAX_STATE_OPS));
	state_res[num_state_res++] = state;
      }
      if ((inout == 'i' || inout == 'm') && rf->not_allocatable()) {
	/* a non-allocatable register file operand */
	XT_State_p state = create_narf_state(reg_file_name);
	FmtAssert(state, ("Unable to create implicit state for non-allocatable"
			  "register file %s", reg_file_name));
	FmtAssert(num_state_ops < MAX_STATE_OPS,
		  ("too many state operands, max = %d\n", MAX_STATE_OPS));
	state_ops[num_state_ops++] = state;
      }
    }
  }

  /* record TIE port references */
  {
    int numTieWires = xtensa_opcode_num_interfaceOperands(_isa, opNum);
    if (numTieWires == XTENSA_UNDEFINED) {

      _init_failed = true;
      RETURN (XT_Instruction*)NULL;

    } else if (numTieWires > 0) {

      hasTiePort = true;

      // record TIE port references as states

      bool tie_wire_as_store = false;

      for (int i=0; i<numTieWires; i++) {
	xtensa_interface xt_interface = 
		xtensa_interfaceOperand_interface(_isa, opNum, i);
	if (xtensa_interface_has_side_effect(_isa, xt_interface))
	  hasTieQueue = true;
	char inout = xtensa_interface_inout(_isa, xt_interface);
	const char* interface_name = xtensa_interface_name(_isa, xt_interface);
	XT_State_p state = create_state(interface_name, TRUE);
	XT_State_p class_state = NULL;
	int interface_class_id = xtensa_interface_class_id(_isa, xt_interface);
	if (interface_class_id != XTENSA_UNDEFINED)
	  class_state = create_interface_class_state(interface_class_id);
	if (inout=='o' || state->is_volatile()) {
	  tie_wire_as_store = true;
	  FmtAssert(num_state_res < TI_ISA_RESULTS_MAX,
		    ("too many state results, max = %d\n", TI_ISA_RESULTS_MAX));
	  state_res[num_state_res++] = state;
	  if (class_state)
	    state_res[num_state_res++] = class_state;
	}
        if (inout=='i' || state->is_volatile())	{
	  if (xtensa_interface_has_side_effect(_isa, xt_interface))
	    tie_wire_as_store = true;
	  FmtAssert(num_state_ops < TI_ISA_OPERANDS_MAX,
		    ("too many state operands, max = %d\n",
		     TI_ISA_OPERANDS_MAX));
	  state_ops[num_state_ops++] = state;
	  if (class_state)
	    state_ops[num_state_ops++] = class_state;
	}
      }

      if (xt_flush_tieport || !xt_reorder_tieport) {
        // treat all TIE wire instructions as memory operations
        if (tie_wire_as_store)
          isStore = true;
        else
	  isLoad = true;

        hasUnknownAddr=true;
        hasUnknownMemData=true;
      }
    }
  }

  /* We derive properties we can't deduce from the state uses/defs from the
     opcode. */

  xtensa_ctype xtct;
  if (_tieMoveMap.find(opNum, &xtct)) {
    /* for TIE copy instructions, find out the positions for src and dest by
       comparing operand names with parameter names of the move proto */

    /* 'opcode' must meet the following conditions to be marked as a move:
       
       1. 'opcode' must be the only instruction in the move prototype

       2. the destination parameter must be the first result operand in
       'opcode'. This is because xcalibur expects to always find a copies
       result as result 0. We can't check this here, it is done in
       create_operands.  */
    
    xtensa_proto move_proto = xtensa_ctype_move_proto(_isa,xtct);
    Is_True (move_proto != XTENSA_UNDEFINED,(""));
    if (xtensa_proto_num_insns(_isa,move_proto)==1)
    {
      int arg, num_args;
      bool src_found=false;
      bool dest_found=false;

      num_args = xtensa_proto_insn_num_args(_isa, move_proto, 0);
      for (arg = 0; arg < num_args; arg++) {
	int opnd, offset, var, is_tmp;

	/* ignore immediates */
	if (xtensa_proto_insn_arg_is_immed(_isa, move_proto, 0, arg) == 1)
	  continue;

	opnd = xtensa_proto_insn_arg_to_opnd(_isa, move_proto, 0, arg,
					     &offset);
	/* skip multireg operands */
	if (offset > 0)
	  continue;

	xtensa_proto_insn_arg_variable(_isa, move_proto, 0, arg,
				       &var, &is_tmp);
	if (!is_tmp) {
	  if (var == 1) {
	    copySrc = opnd;
	    src_found = true;
	  } else if (var == 0) {
	    copyDest = opnd;
	    dest_found = true;
	  }
	}
      }
      Is_True(src_found, ("Copy source not found for move macro: %s",
			      xtensa_proto_name(_isa,move_proto)));
      Is_True(dest_found, ("Copy destination not found for move macro: %s",
			      xtensa_proto_name(_isa,move_proto)));

      isSometimesCopy = TRUE;

      /* If there are only two operands to the single instruction in the move
         prototype, then the instruction is always acting as a move (i.e. an
         addi can act as a move when the immediate is 0, so if it was in a move
         prototype we could not mark is as always being a move). */

      if (num_args == 2)
	isAlwaysCopy = TRUE;
    }
  }

  /* Count the number of operands and results. */

  UINT numOps = 0, numRes = 0;
  UINT numVisibleOperands = 0;
  
  for(UINT j=0; j < numIsaOperands; j++ ) {
    char inout = xtensa_operand_inout(_isa, opNum, j);
    int num_regs = 1;
    if (xtensa_operand_is_visible(_isa, opNum, j) == 1)
      numVisibleOperands += 1;
    if (xtensa_operand_is_register(_isa, opNum, j) == 1)
      num_regs = xtensa_operand_num_regs(_isa, opNum, j);
    if (num_regs!=1) {
      hasAggregateOperand = TRUE;
      if (!multireg_need_expansion(opNum, j))
	num_regs = 1;
    }

    switch (inout)
    {
    case 'm':
      for (int r=0; r<num_regs; r++)
	record_same_res(topcode, numOps+r, numRes+r);
      sameRes = true;
      numOps+=num_regs;
      numRes+=num_regs;
      break;

    case 'i':
      numOps+=num_regs;
      break;

    case 'o':
      numRes+=num_regs;
      break;
    }
  }

  /* Create the instruction.... */
  
  /* TODO - FIXME: add VLIW support; remove fixed 3 below */
  XT_Instruction_p inst = CXX_NEW(XT_Instruction(topcode,
						 opcode,
						 3 /* xtensa_format_length(_isa, opNum?) */,
						 memory_size,
						 issue_alignment,
						 numOps + num_state_ops,
						 numRes + num_state_res,
						 latestDefStage,
						 flags),
				  		_pool);

  inst->_isLoad = isLoad;
  inst->_isStore = isStore;
  inst->_sameRes = sameRes;
  inst->_baseUpdate = baseUpdate;
  inst->_isSometimesCopy = isSometimesCopy;
  inst->_isAlwaysCopy = isAlwaysCopy;
  inst->_isXfer = isXfer;
  inst->_isCond = isCond;
  inst->_useSharedResource = useSharedResource;
  inst->_hasAggregateOperand = hasAggregateOperand;
  inst->_isSimulated = FALSE;
  inst->_isGeneric = false;
  inst->_isNoop = FALSE;
  inst->_unknownAddr = hasUnknownAddr;
  inst->_unknownMemData = hasUnknownMemData;
  inst->_unalignStore = isUnalignStore;
  inst->_hasSideEffects = hasSideEffects;
  inst->_hasTiePort = hasTiePort;
  inst->_hasTieQueue = hasTieQueue;
  inst->_hasExportState = hasExportState;
  inst->_isStreamGet = isStreamGet;
  inst->_isStreamPut = isStreamPut;
  inst->_useSharedFunctionalUnit = useSharedFunctionalUnit;

  inst->_operands = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_operands(), _pool);
  inst->_results = CXX_NEW_ARRAY(XT_Operand_cp, inst->num_results(), _pool);

  FmtAssert(numIsaOperands < max_print_operands,
	    ("too many operands to print, maximum %d, requested %d",
	     max_print_operands, numIsaOperands));
  
  inst->_print_info.format = formats[numVisibleOperands];
  inst->_print_info.comp[0] = ISA_PRINT_COMP_name;
  
  _instructionNameMap.insert(opcode, inst);
  _instructionTopMap.insert(topcode, inst);

  _max_results = Max(_max_results, inst->num_results());
  _max_operands = Max(_max_operands, inst->num_operands());
  if (_max_operands == inst->num_operands())
    _max_operands_op = inst;
  if (_max_results == inst->num_results())
    _max_results_op = inst;
  
  /* Add the isa operands/results followed by the state operands/results. For
     copies, set copy source and destination to be the correct indices into the
     operands or results list. */

  create_operands(opNum, inst, numIsaOperands, copySrc, copyDest,
		  num_state_ops, state_ops, num_state_res, state_res);
  
  if (TRACE_IS_ON(2))
    inst->print(stderr, this);
  
  RETURN inst;
}


XT_Instruction_p
XT_Architecture::find_instruction (const char *opc)
{
  XT_Instruction_p inst;
  if (!_instructionNameMap.find(opc, &inst))
      return NULL;

  return inst;
}


XT_Instruction_p
XT_Architecture::find_instruction (TOP topcode)
{
  XT_Instruction_p inst;
  if (!_instructionTopMap.find(topcode, &inst))
      return NULL;

  return inst;
}


void
XT_RegFile::print (FILE *file)
{
  fprintf(file, "register file %s\n", name());
  fprintf(file, "\tentries = %d, bit width = %d\n", registers(), bit_size());
  fprintf(file, "\tregclass = %d\n", regclass());
  fprintf(file, "\tflags = %x\n", flags());
  fprintf(file, "\t");
  for (UINT i = 0; i < registers(); i++)
  {
    fprintf(file, (i == (registers()-1)) ? "%s" : "%s, ", reg_name(i));
    if ((i % 8) == 7)
      fprintf(file, "\n\t");
  }
  
  if ((registers() % 8) != 7)
    fprintf(file, "\n");
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
