
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

/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_fusion
 *
 *  Description:
 *  ============
 *
 *  See header.
 *
 * =======================================================================
 * ======================================================================= */

#include <stdio.h>
#include <stdlib.h>

#include "at_xcc.h"
#include "cg_dep_graph.h"
#include "cg_fusion.h"
#include "cgexp.h"
#include "config_targ_options.h"
#include "cxx_hash.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "defs.h"
#include "ercg.h"
#include "gra_live.h"
#include "libti.h"
#include "mempool.h"
#include "tie.h"
#include "tietypes.h"
#include "tn_map.h"
#include "tracing.h"
#include "whirl2ops.h"
#include "xtensa-isa.h"
#include "dep_graph.h"
#include "opt_alias_interface.h"

// the following is in cg_dep_graph.cxx
extern BOOL get_mem_dep(OP *pred_op, OP *succ_op, BOOL *definite, UINT8 *omega);

extern struct ALIAS_MANAGER *Alias_Manager;

class CG_FUSION;
class FUSION_OP;
class OP_SIGNAL;

static MEM_POOL CG_Fusion_Pool;
static MEM_POOL CG_Fusion_Local_Pool;

static XT_MEMPOOL XT_Temp_DFG_Pool("Fusion temp dfg pool");
static XT_MEMPOOL XT_Temp_OPT_Pool("Fusion temp opt pool");

static CG_FUSION *CG_Fusion = NULL;


#define AT_REFERENCE_PREFIX_LEN strlen(AT_REFERENCE_PREFIX)

#define CG_FUSION_ERROR_PHASE "CG Fusion"
#define CG_FUSION_INIT_ERROR_PHASE "CG Fusion Init"

#define CG_FUSION_LINE "##################################################################\n"


typedef DYN_ARRAY<tf_node_t *> tf_node_array;
typedef DYN_ARRAY<FUSION_OP *> FUSION_OPS;
typedef DYN_ARRAY<OP_SIGNAL *> SIGNAL_VEC;
typedef HASH_TABLE<TOP, FUSION_OP *> FUSION_OP_MAP;
typedef USER_HASH_TABLE<const char *, tf_op_dfg_t, String_Hash, String_Equal> FUNCTION_DFG_MAP;
typedef USER_HASH_TABLE<const char *, bool, String_Hash, String_Equal> STRSET_MAP;
typedef USER_HASH_TABLE<const char *, unsigned int, String_Hash, String_Equal> STRINT_MAP;
typedef USER_HASH_TABLE_ITER<const char *, unsigned int, String_Hash, String_Equal> STRINT_MAP_ITER;

//
// OP_SIGNAL
//

class OP_SIGNAL
{
private:
  xtie_signal _xsig;
  
  /* The name of this signal in the body (if any). If we use the
     operation body from the post-parse TIE, this will be the argument
     name for the operand signals. */
  const char *_body_name;

public:
  OP_SIGNAL (xtie_signal xsig, const char *body_name) :
    _xsig(xsig),
    _body_name(body_name)
  { }
  
  xtie_signal xsig () const { return _xsig; }
  const char *body_name () const { return _body_name; }
};


//
// FUSION_OP
//

class FUSION_OP
{
private:
  CG_FUSION *_cg_fusion;
  TOP _top;
  tf_op_dfg_t _op_dfg;
  xtie_signals _xsigs;
  SIGNAL_VEC *_osigs;
  bool _split_in_interfaces;
  INT _node_count;

public:
  FUSION_OP (CG_FUSION *cg_fusion,
             TOP top, tf_op_dfg_t op_dfg, xtie_signals xsigs, SIGNAL_VEC *osigs);

  void init_node_count (void); 

  // Return a new FUSION_OP which is the same as this one but with a new copy of '_op_dfg'.
  FUSION_OP *clone_op_dfg (MEM_POOL *pool);
    
  TOP top (void) const { return _top; }
  tf_op_dfg_t op_dfg (void) const { return _op_dfg; }
  xtie_signals xsigs (void) const { return _xsigs; }
  SIGNAL_VEC *osigs (void) const { return _osigs; }
  INT node_count (void) const { return _node_count; }

  const char *name (void) const { return TI_TOP_Name(top()); }
  const ISA_OPERAND_INFO *isa_operand_info (void) const { return TI_ISA_Operand_Info(top()); }
  tf_dfg_t dfg (void) const { return tfex->op_dfg_dfg(op_dfg()); }
  
  OP_SIGNAL *find_signal (INT idx, bool inputs, bool outputs);
  
  void hide_interfaces (void);
  void calls_to_node_kinds (void);

  /* Split the output operands of calls that correspond to multiple
     output opcodes. */
  bool split_call_output (void);
  
  /* Extract input interfaces that may not belong to the incident nodes
     in separate COPY nodes to make matching easier. */
  void split_input_interfaces (void);
  
  /* Check if 'fusion_op' is a potential fusion opcode and we should try matching
     it when optimizing BBs. This method may modify the fusion DFG. */
  bool check_fusion ();
  bool valid_reference_functions (void);
  
  // Check if 'fusion_op' may become a fusion opcode after additional work on it
  // (e.g. replacing inlined opcodes).
  bool maybe_fusion (void);
    
  // Check if 'fusion_op' is an opcode that may be inlined in a fusion opcode.
  bool maybe_inlined_in_fusion (void);
  
  bool is_load (void) const { return TI_ISA_Property_Set(PROP_load, top()); }
  bool is_store (void) const { return TI_ISA_Property_Set(PROP_store, top()); }
  bool is_base_update (void) const { return TI_ISA_Property_Set(PROP_base_update, top()); }

  INT base_idx (void) const { return TI_TOP_Find_Operand_Use(top(), OU_base); }
  INT offset_idx (void) const { return TI_TOP_Find_Operand_Use(top(), OU_offset); }
  INT value_idx (void) const;

  INT register_inputs (void) const;

  void print (FILE *file, INT tab =0);
  void print_debug (void);
};


//
// BB_MATCH_CBACK_USER
//


class BB_MATCH_CBACK_USER
{
private:
  tf_match_callback_t *_inherited_cback;
  FUSION_OP *_fusion_op;
  tf_node_node_set_map_t _bb_dfg_preds;

  bool dependence_out_and_in (tf_node_t node, tf_node_node_map_t node_map);

public:
  BB_MATCH_CBACK_USER (tf_match_callback_t *inherited_cback) :
    _inherited_cback(inherited_cback), _fusion_op(NULL) { }
  
  tf_match_callback_t *inherited_cback (void) const { return _inherited_cback; }
  
  FUSION_OP *fusion_op (void) const { return _fusion_op; }
  void set_fusion_op (FUSION_OP *fusion_op) { _fusion_op = fusion_op; }
  
  tf_node_node_set_map_t bb_dfg_preds (void) const { return _bb_dfg_preds; }
  void set_bb_dfg_preds (tf_node_node_set_map_t preds) { _bb_dfg_preds = preds; }

  bool found_match_check_operands (tf_match_map_t match_map);
  bool found_match_check_dependences (tf_match_map_t match_map);

};
  
  
//
// TIE_MATCH_CBACK_USER
//

class TIE_MATCH_CBACK_USER
{
private:
  FUSION_OP *_sub_fusion_op;
  FUSION_OP *_fusion_op;
  
public:
  TIE_MATCH_CBACK_USER () :
    _sub_fusion_op(NULL), _fusion_op(NULL)
  { }
  
  FUSION_OP *sub_fusion_op (void) const { return _sub_fusion_op; }
  void set_sub_fusion_op (FUSION_OP *fop) { _sub_fusion_op = fop; }
  
  FUSION_OP *fusion_op (void) const { return _fusion_op; }
  void set_fusion_op (FUSION_OP *fop) { _fusion_op = fop; }
};
  
  
//
// UPD_MATCH_CBACK_USER
//
  
class UPD_MATCH_CBACK_USER
{
private:
  FUSION_OP *_non_updating_op;
  FUSION_OP *_updating_op;
  
  tf_op_dfg_t _created_op_dfg;
  tf_op_dfg_t _updating_op_dfg;
  
public:
  UPD_MATCH_CBACK_USER () :
    _non_updating_op(NULL), _updating_op(NULL),
    _created_op_dfg(NULL), _updating_op_dfg(NULL)
  { }
  
  FUSION_OP *non_updating_op (void) const { return _non_updating_op; }
  void set_non_updating_op (FUSION_OP *fop) { _non_updating_op = fop; }
  
  FUSION_OP *updating_op (void) const { return _updating_op; }
  void set_updating_op (FUSION_OP *fop) { _updating_op = fop; }
  
  tf_op_dfg_t created_op_dfg (void) const { return _created_op_dfg; }
  void set_created_op_dfg (tf_op_dfg_t op_dfg) { _created_op_dfg = op_dfg; }
  
  tf_op_dfg_t updating_op_dfg (void) const { return _updating_op_dfg; }
  void set_updating_op_dfg (tf_op_dfg_t op_dfg) { _updating_op_dfg = op_dfg; }
};
  
  
//
// CG_FUSION
//

class CG_FUSION
{
  friend class FUSION_OP;

private:

  //
  // VECTOR_TYPE_INFO
  //
  
  class VECTOR_TYPE_INFO
  {
  private:
    CG_FUSION *_cg_fusion;
    TYPE_ID _scalar_type;
    INT _vector_length;
    TYPE_ID _vector_mem_type;
    TYPE_ID _vector_reg_type;
    
    // rtor scalar to vector conversion opcode
    TOP _rtor_top;
    FUSION_OP *_rtor_fusion_op;
    
    void init_rtor (void);
    
  public:
    VECTOR_TYPE_INFO (CG_FUSION *cg_fusion,
		      TYPE_ID scalar_type, INT vector_length,
		      TYPE_ID vector_mem_type, TYPE_ID vector_reg_type);
	
    TYPE_ID scalar_type (void) const { return _scalar_type; }
    INT vector_length (void) const { return _vector_length; }
    TYPE_ID vector_mem_type (void) const { return _vector_mem_type; }
    TYPE_ID vector_reg_type (void) const { return _vector_reg_type; }

    INT scalar_bit_size (void) const { return MTYPE_bit_size(scalar_type()); }
    INT scalar_byte_size (void) const { return MTYPE_byte_size(scalar_type()); }

    TOP rtor_top (void) const { return _rtor_top; }
    FUSION_OP *rtor_fusion_op (void) const { return _rtor_fusion_op; }

    void print (FILE *file, INT tab =0);
    void print_debug (void);
  };

  bool _tracing;

  tf_match_callback_t *_match_cback;
  tf_match_callback_t *_bb_match_cback;
  tf_match_callback_t *_tie_match_cback;
  tf_match_callback_t *_upd_match_cback;
  tf_dfg_callback_t *_dfg_cback;
  tf_dfg_callback_t *_op_dfg_cback;
    
  /* All TIE functions. */
  FUNCTION_DFG_MAP *_function_dfg_map;

  /* A subset of _function_dfg_map containing all xt_reference functions
     with equivalent opcode references. */
  STRSET_MAP *_reference_function_map;
    
  FUSION_OPS *_all_ops;
  FUSION_OPS *_fusion_ops;
  FUSION_OP_MAP *_all_op_map;
  FUSION_OP_MAP *_rtor_op_map;
    
  typedef HASH_TABLE<TYPE_ID, VECTOR_TYPE_INFO *> VECTOR_TYPE_INFO_MAP;
  VECTOR_TYPE_INFO_MAP *_vector_type_info;
    
  // Fields related to the basic block currently being optimized.
  BB *_bb;
  tf_dfg_t _bb_dfg;
  tf_node_node_set_map_t _bb_dfg_preds;
  BB_OP_MAP _bb_op_node_map;
    
  // Current fusion operation being matched.
  FUSION_OP *_fusion_op;
    
  // Given a TIE op 'op', build its DFG.
  tf_op_dfg_t build_op_dfg (XT_MEMPOOL *pool, xtie_opcode xop,
                            xtie_signals *xsigs, SIGNAL_VEC **osigs);

  // Given a TIE imap 'imap', build its DFG.
  tf_op_dfg_t build_op_dfg (XT_MEMPOOL *xt_pool, xtie_imap imap);

  
  void replace_matched_tie_nodes (FUSION_OP *fusion_op, tf_dfg_t dfg,
				  tf_match_map_t match_map);
    
  tf_node_t replace_matched_bb_nodes (OP *fop, OPS *fops, FUSION_OP *fusion_op,
                                      tf_match_map_t match_map);
    
  // Use 'fusion_op' to optimize the current basic block using 'match_map'.
  // Return false if the matched operations could not be replaced.
  bool apply_fusion (FUSION_OP *fusion_op, tf_match_map_t match_map);
  OP *build_fusion_op (FUSION_OP *fusion_op, tf_match_map_t match_map, OPS *ops);
  bool replace_fused_ops (OP *fop, OPS *fops, tf_match_map_t match_map);

  // Find and replace any vector operands with constant values.
  void find_vector_constants (tf_dfg_t dfg);
  bool set_vector_constant_operand (tf_opnd_t opnd, FUSION_OP *rtor, INT64 val);
  
  /* Return the rtor op and set 'val' to the scalar value feeds the rtor opcode.
     Return NULL if unable to find the def value. */
  FUSION_OP *def_tn_vector_value (OP *op, TN *tn, INT64 &val);
  FUSION_OP *rematerializable_tn_vector_value (TN *tn, INT64 &val);
  FUSION_OP *tn_vector_value_at_op (OP *op, TN *tn, INT64 &val);
    
  // Return the bit size of the value represented by 'tn'.
  INT tn_bit_size (TN *tn, bool use_reg_size);
    
  // Return the first operation that follows 'op' in its basic block and defines 'tn',
  // or null if no such operation.
  OP *tn_next_def (TN *tn, OP *op);
    
  // Return true if 'op' is a live-out definition of 'tn'.
  bool is_live_out_def (OP *op, TN *tn);

  // Add the dependence edges to 'dfg' based on its dependence graph. Return
  // 'false' on error.
  bool add_dependence_edges (BB *bb, tf_dfg_t dfg, BB_OP_MAP op_node_map);

  void add_fusion_op (FUSION_OP *fusion_op);
  
  FUSION_OP *find_updating (FUSION_OP *fop);
  FUSION_OP *find_updating_match (FUSION_OP *fop, tf_op_dfg_t update_op_dfg);

  bool imap_scan (void);
  bool fusion_operation_scan (void);
  bool reference_functions_match (FUSION_OP *op, xtie_function xfunc);
  
  SIGNAL_VEC *ordered_signals (xtie_opcode xop, xtie_opcode xop_rewrite,
                               bool use_parse_body);
  
  void init_dfg_callbacks (void);
  void init_match_callbacks (void);
  void init_tie_functions (void);
  void init_tie_tables (void);
  void init_all_ops (void);
  void init_updating_ops (void);
  void init_fusion_ops (void);
  void init_imaps (void);
  void init_reference_functions (void);
    
  void init_vector_type_info (void);

  bool bad_match(tf_match_map_t match_map);

public:
  CG_FUSION (bool tracing);
    
  MEM_POOL *fusion_pool (void) const { return &CG_Fusion_Pool; }
  MEM_POOL *local_pool (void) const { return &CG_Fusion_Local_Pool; }
  XT_MEMPOOL *xt_dfg_pool (void) const { return &XT_Temp_DFG_Pool; }
  XT_MEMPOOL *xt_init_pool (void) const { return xt_dfg_pool(); }
  XT_MEMPOOL *xt_opt_pool (void) const { return &XT_Temp_OPT_Pool; }
    
  tf_match_callback_t *bb_match_cback (void) const { return _bb_match_cback; }
  tf_match_callback_t *tie_match_cback (void) const { return _tie_match_cback; }
  tf_match_callback_t *upd_match_cback (void) const { return _upd_match_cback; }
  tf_dfg_callback_t *dfg_cback (void) const { return _dfg_cback; }
  tf_dfg_callback_t *op_dfg_cback (void) const { return _op_dfg_cback; }

  BB_MATCH_CBACK_USER *bb_match_cback_user (void) const;
  TIE_MATCH_CBACK_USER *tie_match_cback_user (void) const;
  UPD_MATCH_CBACK_USER *upd_match_cback_user (void) const;

  FUSION_OP_MAP *all_op_map (void) const { return _all_op_map; }
  
  FUSION_OP_MAP *rtor_op_map (void) const { return _rtor_op_map; }
  void add_rtor_op (TOP rtor_top, FUSION_OP *rtor_op);

  bool tracing (void) const { return _tracing; }
  FILE *trace_file (void) const { return TFile; }
    
  // Inline all functions that are being called in 'dfg'. Return false
  // if any called function is not available in the TIE function map.
  bool inline_all_functions (tf_dfg_t dfg);

  // Build DFG for 'bb'. Use the register entry bit size as operand's bit size
  // when 'use_reg_size' is true; use type bit size otherwise.
  tf_dfg_t build_dfg (XT_MEMPOOL *xt_pool, BB *bb, bool use_reg_size,
		      bool cyclic = false);
  void optimize_bb (BB *bb, bool aggressive);
};


//
// Enter and exit fusion error phase
//

static const char *
enter_fusion_phase (const char *fusion_phase)
{
  const char *phase = Get_Error_Phase();
  Set_Error_Phase(fusion_phase);
  return phase;
}

static void
exit_fusion_phase (const char *phase)
{
  Set_Error_Phase(phase);
}


//
// Exports
//


void
CG_FUSION_Initialize (void)
{
  const char *phase = enter_fusion_phase(CG_FUSION_INIT_ERROR_PHASE);
    
  Is_True(!CG_Fusion, ("CG_FUSION already initialized."));

  bool tracing = Get_Trace(TP_TEMP, 0x8000);
  if (tfex) {
    /* The CG_FUSION object and its memory pools are initialized only
       if the libfusion DLL was successfully loaded. */
    MEM_POOL_Initialize(&CG_Fusion_Pool, "CG_Fusion_Pool", FALSE);
    MEM_POOL_Initialize(&CG_Fusion_Local_Pool, "CG_Fusion_Local_Pool", FALSE);
    MEM_POOL_Push(&CG_Fusion_Pool);
    MEM_POOL_Push(&CG_Fusion_Local_Pool);
    
    CG_Fusion = CXX_NEW(CG_FUSION(tracing), &CG_Fusion_Pool);
  } else {
    if (tracing) {
      fprintf(TFile, CG_FUSION_LINE);
      fprintf(TFile, "CG_FUSION: libfusion handle not available.\n");
      fprintf(TFile, CG_FUSION_LINE);
    }
  }
  
  exit_fusion_phase(phase);
}


void
CG_FUSION_Finalize (void)
{
  if (CG_Fusion) {
    CG_Fusion = NULL;
    
    MEM_POOL_Pop(&CG_Fusion_Local_Pool);
    MEM_POOL_Pop(&CG_Fusion_Pool);
    MEM_POOL_Delete(&CG_Fusion_Local_Pool);
    MEM_POOL_Delete(&CG_Fusion_Pool);
  } else {
    Is_True(!tfex, ("CG_FUSION is not active."));
  }
}


tf_node_kind_t
CG_FUSION_Top_To_Node_Kind (TOP top)
{
  switch (top)
  {
  case TOP_add:
  case TOP_add_n:
    return TFN_ADD;

  case TOP_and:
    return TFN_BAND;

  case TOP_mov_n:
    if (!xt_density)
      return TFN_COPY;
    
    break;

  case TOP_neg:
    return TFN_NEG;
	
  case TOP_or:
    return TFN_BOR;

  case TOP_slli:
    return TFN_SHL;

  case TOP_srl:
  case TOP_srli:
    return TFN_LSHR;

  case TOP_sub:
    return TFN_SUB;

  case TOP_xor:
    return TFN_BXOR;
	
  default:
    break;
  }
    
  return TFN_UNKNOWN;
}


tf_dfg_t 
CG_FUSION_Build_DFG (XT_MEMPOOL *pool, BB *bb)
{
  const char *phase = enter_fusion_phase(CG_FUSION_ERROR_PHASE);
  
  tf_dfg_t dfg = TF_DFG_INVALID;
  
  if (CG_Fusion) {
    if (!BB_rotating_kernel(bb)) {
      dfg = CG_Fusion->build_dfg(pool, bb, /* use_reg_size */ true, true);
    }
  } else {
    Is_True(!tfex, ("CG_FUSION is not active."));
  }

  exit_fusion_phase(phase);
    
  return dfg;
}


void
CG_FUSION_Optimize_BB (BB *bb, bool aggressive)
{
  const char *phase = enter_fusion_phase(CG_FUSION_ERROR_PHASE);
  
  if (CG_Fusion) {
    if (!BB_rotating_kernel(bb)) {
      CG_Fusion->optimize_bb(bb, aggressive);
    }
  } else {
    Is_True(!tfex, ("CG_FUSION is not active."));
  }
    
  exit_fusion_phase(phase);
}


void
CG_FUSION_Optimize_Loop (LOOP_DESCR *loop)
{
  if (CG_Fusion) {
    BB *bb;
    FOR_ALL_BB_SET_members(LOOP_DESCR_bbset(loop), bb) {
      CG_FUSION_Optimize_BB(bb);
    }
  } else {
    Is_True(!tfex, ("CG_FUSION is not active."));
  }
}


void
CG_FUSION_Optimize_Region (void)
{
  if (CG_Fusion) {
    for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb)) {
      CG_FUSION_Optimize_BB(bb);
    }
  } else {
    Is_True(!tfex, ("CG_FUSION is not active."));
  }
}


//
// Statics
//


static TOP
call_id_top (const char *call_id)
{
  Is_True(call_id, ("Null call id"));
    
  if (strncmp(call_id, AT_REFERENCE_PREFIX, AT_REFERENCE_PREFIX_LEN))
    return TOP_UNDEFINED;
    
  call_id += AT_REFERENCE_PREFIX_LEN;    
  TOP top = TI_TOP_Topcode(call_id);
  return top;
}


static TOP
node_top (tf_node_t node)
{
  tf_node_kind_t kind = tfex->node_kind(node);
  if (kind == TFN_CALL) {
    const char *node_call_id = tfex->node_call_id(node);
    TOP top = call_id_top(node_call_id);
    return top;
  }

  if (kind == TFN_USER) {
    TOP top = (TOP)(unsigned int)tfex->node_user(node);
    return top;
  }

  return TOP_UNDEFINED;
}


static bool
compatible_valtyps (const ISA_OPERAND_VALTYP *opnd_valtyp_a,
		    const ISA_OPERAND_VALTYP *opnd_valtyp_b)
{
  Is_True(opnd_valtyp_a && opnd_valtyp_b, ("Null arguments"));
  
  if (TI_ISA_Valtyp_Is_Register(opnd_valtyp_a)) {
    if (!TI_ISA_Valtyp_Is_Register(opnd_valtyp_b))
      return false;
    
    ISA_REGCLASS opnd_rcl_a = TI_ISA_Valtyp_Regclass(opnd_valtyp_a);
    Is_True(opnd_rcl_a != ISA_REGCLASS_UNDEFINED, ("Can't find the regclass for an operand"));

    ISA_REGCLASS opnd_rcl_b = TI_ISA_Valtyp_Regclass(opnd_valtyp_b);
    Is_True(opnd_rcl_b != ISA_REGCLASS_UNDEFINED, ("Can't find the regclass for an operand"));
    
    if (opnd_rcl_a != opnd_rcl_b)
      return false;
    
    ISA_REGSUBCLASS opnd_rscl_a = TI_ISA_Valtyp_Regsubclass(opnd_valtyp_a);
    ISA_REGSUBCLASS opnd_rscl_b = TI_ISA_Valtyp_Regsubclass(opnd_valtyp_b);
    if (opnd_rscl_a != opnd_rscl_b)
      return false;
  } else if (TI_ISA_Valtyp_Is_Literal(opnd_valtyp_a)) {
    if (!TI_ISA_Valtyp_Is_Literal(opnd_valtyp_b))
      return false;
  } else if (TI_ISA_Valtyp_Is_Enum(opnd_valtyp_a)) {
    if (!TI_ISA_Valtyp_Is_Enum(opnd_valtyp_b))
      return false;
  } else {
    return false;
  }
  
  return true;
}


static bool
compatible_valtyp_tn (const ISA_OPERAND_VALTYP *opnd_valtyp, TN *tn)
{
  Is_True(opnd_valtyp && tn, ("Null arguments"));

  if (TN_is_constant(tn)) {
    if (!TN_has_value(tn))
      return false;
    
    if (!TI_ISA_Valtyp_Is_Literal(opnd_valtyp))
      return false;
    
    ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(opnd_valtyp);
    if (lit_class == LC_UNDEFINED)
      return false;
    
    INT64 tn_val = TN_value(tn);
    
    if (!TI_ISA_LC_Value_In_Class(tn_val, lit_class))
      return false;

    return true;
  }
  
  if (!TN_is_register(tn))
    return false;
  
  if (!TI_ISA_Valtyp_Is_Register(opnd_valtyp))
    return false;
    
  ISA_REGCLASS opnd_rcl = TI_ISA_Valtyp_Regclass(opnd_valtyp);
  Is_True(opnd_rcl != ISA_REGCLASS_UNDEFINED, ("Can't find the regclass for an operand"));
    
  TYPE_ID mtype = TN_mtype(tn);
  if (mtype != MTYPE_UNKNOWN) {
    ISA_REGCLASS mtype_rcl = TI_ISA_Regclass_For_Mtype(mtype);
    Is_True(mtype_rcl != ISA_REGCLASS_UNDEFINED, ("Can't find the regclass for a TYPE_ID"));
	
    return mtype_rcl == opnd_rcl;
  }

  if (!TN_is_dedicated(tn))
    return false;
    
  ISA_REGCLASS tn_rcl = TN_register_class(tn);
  if (tn_rcl != opnd_rcl)
    return false;
    
  ISA_REGSUBCLASS opnd_rscl = TI_ISA_Valtyp_Regsubclass(opnd_valtyp);
  ISA_REGSUBCLASS tn_rscl = ISA_REGSUBCLASS_UNDEFINED;
  if (tn_rcl == TI_ISA_Regclass_Special()) {
    tn_rscl = TI_ISA_Regsubclass_Special_Reg_Num(TN_register(tn) - REGISTER_MIN);
  } else if (TI_ISA_Regclass_Is_State(tn_rcl)) {
    tn_rscl = TI_ISA_Regsubclass_State(REGISTER_name(TN_register_class(tn),
						     TN_register(tn)));
  } else {
    return false;
  }
    
  return tn_rscl == opnd_rscl;
}


static void
sort_fusion_ops (FUSION_OPS *fusion_ops)
{
  // Sort descending by the number of covered ops.
  for (INT i = 0; i < fusion_ops->Elements(); i++) {
    for (INT j = i + 1; j < fusion_ops->Elements(); j++) {
      if (fusion_ops->Get(i)->node_count() < fusion_ops->Get(j)->node_count()) {
	FUSION_OP *temp = fusion_ops->Get(i);
	fusion_ops->Set(i, fusion_ops->Get(j));
	fusion_ops->Set(j, temp);
      }
    }
  }
  bool tracing = Get_Trace(TP_TEMP, 0x8000);
  if (tracing) {
    fprintf(TFile, "Sorted fusion ops:\n");
    for (INT i = 0; i < fusion_ops->Elements(); i++) {
      fprintf(TFile, "[idx %d] %s\n", i, fusion_ops->Get(i)->name());
    }
  }
}
    
    
static void
find_first_last_ops (tf_match_map_t match_map, OP *&first_op, OP *&last_op)
{
  first_op = last_op = NULL;
  
  tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
  for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
    tf_node_t node = tfex->node_node_pair_key(node_pair);
    OP *op = (OP *)tfex->node_user(node);
    if (op) {
      if (!first_op || OP_Precedes(op, first_op))
	first_op = op;
      if (!last_op || OP_Follows(op, last_op))
	last_op = op;
    }
  }
}


static BB_OP_MAP
create_op_node_map (MEM_POOL *pool, BB *bb, tf_dfg_t dfg)
{
  BB_OP_MAP op_node_map = BB_OP_MAP_Create(bb, pool);
  for (tf_node_t node = tfex->dfg_first_node(dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    OP *op = (OP *)tfex->node_user(node);
    if (op) {
      Is_True(OP_bb(op) == bb, ("Op's basic block mismatches"));
      BB_OP_MAP_Set(op_node_map, op, node);
    }
  }
    
  return op_node_map;
}


static void
add_iclass_arg_signal (MEM_POOL *pool, SIGNAL_VEC *sigs,
                       xtie_signals xsigs,
                       xtie_iclass_arg xia, const char *body_name)
{
  const char *name = xtie_iclass_arg_get_name(xia);
  xtie_signal xsig = xtie_signals_find_signal(xsigs, name);
  Is_True(xsig, ("Unable to find iclass arg signal '%s'", name));

  if (!body_name)
    body_name = xtie_signal_get_name(xsig);
  OP_SIGNAL *op_signal = CXX_NEW(OP_SIGNAL(xsig, body_name), pool);
  
  sigs->AddElement(op_signal);
}


static bool
is_special_top (TOP top)
{
  // Branches are special -- don't do anything fancy with them.
  if (TI_ISA_Property_Set(PROP_xfer, top))
    return true;
    
  // Read, write, exchange special registers -- not interesting.
  const char *name = TI_TOP_Name(top);
  if (!strncasecmp(name, "wsr.", 4) ||
      !strncasecmp(name, "rsr.", 4) ||
      !strncasecmp(name, "xsr.", 4))
    return true;
    
  return false;
}


static const char *
tn_opnd_name (XT_MEMPOOL *pool, TN *tn)
{
  if (!tn || !TN_is_register(tn))
    return NULL;
    
  return xt_pool_printf(pool, "%sTN%d", TN_is_global_reg(tn) || TN_is_dedicated(tn) ? "G" : "", TN_number(tn));
}


/* Check if the 'xi' contains any xt_reference_ function calls. */
static bool
quick_xml_fusion_scan (xtie_xml_item xi)
{
  if (xtie_xml_item_get_tag(xi) == XTIE_TAG_CALL) {
    xtie_xml_item call_id = xtie_xml_item_get_first_item(xi);
    const char *id_name = xtie_xml_item_get_attr_value(call_id, XTIE_ATTR_NAME);
    Is_True(id_name, ("Unable to obtain the call id name"));
    
    if (!strncmp(id_name, AT_REFERENCE_PREFIX, AT_REFERENCE_PREFIX_LEN))
      return true;
  }
  
  for (xtie_xml_item it = xtie_xml_item_get_first_item(xi); it;
       it = xtie_xml_item_get_next(it))
    if (quick_xml_fusion_scan(it))
      return true;
  
  return false;
}


/* Return the state iclass argument 'name'. */
static xtie_iclass_arg
iclass_find_state (xtie_iclass ic, const char *name)
{
  xtie_iclass_foreach_state(ic, st)
    {
      const char *arg_name = xtie_iclass_arg_get_name(st);
      if (!strcasecmp(name, arg_name))
        return st;
    }
  end_xtie_iclass_foreach_state;

  return NULL;
}


//
// FUSION_OP
//


FUSION_OP::FUSION_OP (CG_FUSION *cg_fusion,
		      TOP top, tf_op_dfg_t op_dfg, xtie_signals xsigs, SIGNAL_VEC *osigs) :
  _cg_fusion(cg_fusion), _top(top), _op_dfg(op_dfg), _xsigs(xsigs), _osigs(osigs),
  _split_in_interfaces(false)
{
  init_node_count();
}


FUSION_OP *
FUSION_OP::clone_op_dfg (MEM_POOL *pool)
{
  tf_dfg_t dfg = tfex->op_dfg_dfg(op_dfg());
  tf_pool_t tf_pool = tfex->dfg_pool(dfg);
    
  tf_op_dfg_t op_dfg_copy = tfex->op_dfg_copy(tf_pool, op_dfg());
  FUSION_OP *fusion_op =
    CXX_NEW(FUSION_OP(_cg_fusion, top(), op_dfg_copy, xsigs(), osigs()), pool);

  return fusion_op;
}


void
FUSION_OP::init_node_count(void)
{
  tf_dfg_t dfg = tfex->op_dfg_dfg(_op_dfg);
  _node_count = tfex->dfg_next_node_index(dfg);
}


bool
FUSION_OP::split_call_output (void)
{
  bool split = false;
  for (tf_node_t node = tfex->dfg_first_node(dfg());
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    tf_node_kind_t kind = tfex->node_kind(node);
    if (kind != TFN_CALL)
      continue;

    /* Output already split. */
    if (tfex->node_output_count(node) > 1)
      continue;
    
    const char *node_call_id = tfex->node_call_id(node);
    TOP top = call_id_top(node_call_id);
    Is_True(top != TOP_UNDEFINED, ("Expected a topcode for call %s", node_call_id));
    
    FUSION_OP *fop = _cg_fusion->all_op_map()->Find(top);
    if (!fop)
      return false;
    
    /* Check if the fused opcode has multiple output operands. If so, then the
       call output operand needs to be split accordingly. */
    tf_op_dfg_t fop_op_dfg = fop->op_dfg();
    if (tfex->op_dfg_output_count(fop_op_dfg) <= 1)
      continue;

    tf_opnd_t out_opnd = tfex->node_output(node, 0);
    Is_True(tfex->opnd_kind(out_opnd) == TFO_WIRE, ("Expected a WIRE operand."));
    
    INT edge_pos = tfex->opnd_bit_size(out_opnd);
    tf_node_idx_t node_idx = tfex->node_index(node);

    /* Split 'out_opnd'. */
    tf_opnd_vec_t fouts = tfex->op_dfg_outputs(fop_op_dfg);
    INT fouts_size = tfex->opnd_vec_size(fouts);
    
    for (INT fidx = 0; fidx < fouts_size; fidx++) {
      tf_opnd_t fopnd = tfex->opnd_vec_get(fouts, fidx);
      if (fopnd == TF_OPND_INVALID)
        continue;
      
      Is_True(tfex->opnd_kind(fopnd) == TFO_WIRE, ("Expected a WIRE operand."));
      
      tf_opnd_t new_opnd = tfex->node_new_operand(node,
                                                  tfex->opnd_kind(fopnd),
                                                  tfex->opnd_elem_bit_size(fopnd),
                                                  tfex->opnd_vector_length(fopnd));
      tf_opnd_idx_t new_idx = tfex->node_add_output(node, new_opnd);

      /* Reroute the edges from 'out_opnd' to 'new_opnd'. */
      for (INT pos = tfex->opnd_bit_size(new_opnd) - 1; pos >= 0; pos--) {
        edge_pos--;
        Is_True(edge_pos >= 0, ("Edge count mismatch."));
        
        tf_edge_t edge = tfex->opnd_edge(out_opnd, edge_pos);
        if (edge == TF_EDGE_INVALID) {
          tf_edge_idx_t eidx = tfex->opnd_edge_idx(out_opnd, edge_pos);
          Is_True(eidx == TF_EDGE_IDX_X, ("Expected an X edge."));
          
          tfex->opnd_set_edge_idx(new_opnd, pos, eidx);
        } else {
          tfex->edge_detach_from_source(edge);
          tfex->edge_attach_to_source(edge, node_idx, new_idx, pos);
        }
      }
    }
    
    Is_True(edge_pos == 0,
            ("Output operand mismatch for fusion %s, call %s.",
             name(), node_call_id));
    
    /* Finally, delete the original call output operand. All edges were rerouted. */
    tfex->node_delete_output(node, 0);
    
    split = true;
  }
  
  if (split)
    tfex->opt_prune_copies(dfg());

  return true;
}


bool
FUSION_OP::check_fusion ()
{
  XT_ASSERTX(!is_special_top(top()));

  INT op_nodes = 0;
  for (tf_node_t node = tfex->dfg_first_node(dfg());
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    tf_node_kind_t kind = tfex->node_kind(node);
    if (kind == TFN_COPY)
      continue;
    
    TOP top = node_top(node);
    if (top == TOP_UNDEFINED)
      return false;
    
    op_nodes++;

  }
  
  /* Don't match "trivial" operations (such as MOV or ADD). */
  if (op_nodes < 2)
    return false;
  
  if (!split_call_output())
    return false;
  
  /* Check if the operands of the enclosing opcode are compatible
     with the operands of the fused opcodes. */
  for (INT dir = 0; dir < 2; dir++) {
    bool is_input = dir == 0;
    tf_opnd_vec_t opnds = is_input ?
      tfex->op_dfg_inputs(op_dfg()) : tfex->op_dfg_outputs(op_dfg());

    int osize = tfex->opnd_vec_size(opnds);
    for (INT idx = 0; idx < osize; idx++) {
      tf_opnd_t opnd = tfex->opnd_vec_get(opnds, idx);
      if (opnd == TF_OPND_INVALID)
	continue;
	    
      OP_SIGNAL *osig = find_signal(idx, true, true);
      Is_True(osig,
	      ("Can't find signal table for opcode %s operand %d.", name(), idx));

      xtie_signal xsig = osig->xsig();
      xtie_signal_kind skind = xtie_signal_get_kind(xsig);
      if (skind == XTIE_SIG_EXCEPTION)
	return false;

      if (skind != XTIE_SIG_INTERFACE)
	continue;
      
      // We must check if the interface is attached to the same interface
      // operand in an enclosed opcode.
	    
      tf_node_t fused_node = tfex->opnd_node(opnd);
      tf_node_kind_t fused_node_kind = tfex->node_kind(fused_node);
      if (fused_node_kind != TFN_CALL)
	return false;
	    
      tf_opnd_idx_t fused_idx = tfex->opnd_index(opnd);
      Is_True(fused_idx >= 0, ("Can't find operand in node for %s", name()));
	    
      const char *fused_node_call_id = tfex->node_call_id(fused_node);
      TOP fused_top = call_id_top(fused_node_call_id);
      Is_True(fused_top != TOP_UNDEFINED, ("Bad check for %s.", name()));
	    
      FUSION_OP *fused_fop = _cg_fusion->all_op_map()->Find(fused_top);
      if (!fused_fop)
	return false;
	    
      OP_SIGNAL *fused_osig =
	fused_fop->find_signal(fused_idx, is_input, !is_input);
      Is_True(fused_osig,
	      ("Can't find signal table for opcode %s operand %d.",
	       fused_fop->name(), fused_idx));
      
      xtie_signal fused_xsig = fused_osig->xsig();
      xtie_signal_kind fused_skind = xtie_signal_get_kind(fused_xsig);
      if (fused_skind != XTIE_SIG_INTERFACE)
	return false;
      
      if (strcmp(xtie_signal_get_name(xsig),
                 xtie_signal_get_name(fused_xsig)))
        return false;
    }
  }
    
  return true;
}


bool
FUSION_OP::valid_reference_functions (void)
{
  for (tf_node_t node = tfex->dfg_first_node(dfg());
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    tf_node_kind_t kind = tfex->node_kind(node);
    if (kind != TFN_CALL)
      continue;
    
    const char *node_call_id = tfex->node_call_id(node);
    
    /* If the called function is a real function but it's not in the reference
       function map, then it can't be a call to an opcode reference. */
    if (!_cg_fusion->_reference_function_map->Find(node_call_id))
      return false;
  }
  
  return true;
}


OP_SIGNAL *
FUSION_OP::find_signal (INT idx, bool inputs, bool outputs)
{
  for (INT i = 0; i < (INT)_osigs->Elements(); i++) {
    OP_SIGNAL *osig = _osigs->Get(i);
    xtie_signal xsig = osig->xsig();
    xtie_dir xdir = xtie_signal_get_dir(xsig);
    if (xdir == XTIE_DIR_INOUT ||
        (inputs && xdir == XTIE_DIR_IN) ||
        (outputs && xdir == XTIE_DIR_OUT)) {
      idx--;
      if (idx < 0) {
        return osig;
      }
    }
  }

  return NULL;
}
  

void
FUSION_OP::hide_interfaces (void)
{
  if (!check_fusion()) {
    Is_True(0, ("Expected a fusion operation"));
  }
  
  for (INT dir = 0; dir < 2; dir++) {
    bool is_input = dir == 0;
    tf_opnd_vec_t opnds = is_input ?
      tfex->op_dfg_inputs(op_dfg()) : tfex->op_dfg_outputs(op_dfg());
    tf_opnd_vec_t other_opnds = is_input ?
      tfex->op_dfg_outputs(op_dfg()) : tfex->op_dfg_inputs(op_dfg());

    INT osize = tfex->opnd_vec_size(opnds);
    Is_True(osize == tfex->opnd_vec_size(other_opnds),
	    ("OP_DFG operand sizes mismatch"));
    
    for (INT idx = osize - 1; idx >= 0; idx--) {
      tf_opnd_t opnd = tfex->opnd_vec_get(opnds, idx);
      if (opnd == TF_OPND_INVALID)
	continue;
      
      OP_SIGNAL *osig = find_signal(idx, true, true);
      Is_True(osig,
	      ("Can't find signal table for opcode %s operand %d.", name(), idx));

      xtie_signal xsig = osig->xsig();
      xtie_signal_kind skind = xtie_signal_get_kind(xsig);
      if (skind != XTIE_SIG_INTERFACE)
	continue;
      
      Is_True(tfex->opnd_vec_get(other_opnds, idx) == TF_OPND_INVALID,
	      ("Unexpected inout interface"));
      
      tf_node_t fused_node = tfex->opnd_node(opnd);
      Is_True(tfex->node_kind(fused_node) == TFN_CALL,
                ("Expected a call node for interface operand"));
        
      tf_opnd_idx_t fused_idx = tfex->opnd_index(opnd);
      Is_True(fused_idx >= 0, ("Can't find operand in node for %s", name()));
        
      // Remove the operand for the OP_DFG input/output operands and from the
      // node that it is attached to.
      if (is_input)
        tfex->node_delete_input(fused_node, fused_idx);
      else
        tfex->node_delete_output(fused_node, fused_idx);
      
      tfex->opnd_vec_remove(opnds, idx);
      tfex->opnd_vec_remove(other_opnds, idx);
    }
  }
}


void
FUSION_OP::split_input_interfaces (void)
{
  if (_split_in_interfaces)
    return;

  _split_in_interfaces = true;

  /* libfusion's matcher has problems matching COPY nodes with unknown single-in,
     single-out operands when they need to be matched to an input operand. If an input
     interface is attached to an opcode that doesn't access that interface, then
     the interface must have come from an inlined opcode with single-in, single-out
     COPY node. Extract those interfaces in explicit COPY nodes so libfusion matching
     works.
     
     This should be irrelevant once we start representing fusions as explicit proto-like
     list of opcodes. */
  tf_opnd_vec_t opnds = tfex->op_dfg_inputs(op_dfg());
  INT osize = tfex->opnd_vec_size(opnds);
  for (INT idx = 0; idx < osize; idx++) {
    tf_opnd_t opnd = tfex->opnd_vec_get(opnds, idx);
    if (opnd == TF_OPND_INVALID)
      continue;
    
    OP_SIGNAL *osig = find_signal(idx, true, true);
    Is_True(osig,
            ("Can't find signal table for opcode %s operand %d.", name(), idx));
    
    xtie_signal xsig = osig->xsig();
    xtie_signal_kind skind = xtie_signal_get_kind(xsig);
    if (skind != XTIE_SIG_INTERFACE)
      continue;
    
    tf_node_t fused_node = tfex->opnd_node(opnd);
    tf_opnd_idx_t fused_idx = tfex->opnd_index(opnd);
    Is_True(fused_idx >= 0, ("Can't find operand in node for %s", name()));
    
    if (tfex->node_kind(fused_node) == TFN_CALL) {
      const char *fused_node_call_id = tfex->node_call_id(fused_node);
      TOP fused_top = call_id_top(fused_node_call_id);
      Is_True(fused_top != TOP_UNDEFINED, ("Bad check for %s.", name()));
      
      FUSION_OP *fused_fop = _cg_fusion->all_op_map()->Find(fused_top);
      if (!fused_fop)
        continue;
      

      OP_SIGNAL *fused_osig = fused_fop->find_signal(fused_idx, true, false);
      Is_True(fused_osig,
              ("Can't find signal table for opcode %s operand %d.",
               fused_fop->name(), fused_idx));
      
      xtie_signal fused_xsig = fused_osig->xsig();
      xtie_signal_kind fused_skind = xtie_signal_get_kind(fused_xsig);
      if (fused_skind == XTIE_SIG_INTERFACE)
        continue;
    } else if (tfex->node_kind(fused_node) == TFN_COPY) {
      /* Don't split the interface if it's attached to a COPY node with
         no outputs or a single output with known output edges. Either form
         seems OK for matching as it is. */
      if (tfex->node_output_count(fused_node) == 0 ||
          (tfex->node_output_count(fused_node) == 1 &&
           !tfex->opnd_is_unknown(tfex->node_output(fused_node, 0))))
        continue;
    } else {
      continue;
    }

    tfex->copy_in_reroute(fused_node, fused_idx);
  }
}


void
FUSION_OP::calls_to_node_kinds (void)
{
  /* Try to convert all call nodes of the form "reference_<opcode>" into
     nodes with kinds corresponding to <opcode>, and convert all remaining
     call ids to lower case.
     During the first pass, convert the call ids to lower case. Then,
     canonicalize the DFG so that it's consistent with the way we
     canonicalize the BB DFG. Then, in the second pass, convert the calls
     to nodes with kinds if possible. */
  for (int pass = 0; pass < 2; pass++) {
    for (tf_node_t node = tfex->dfg_first_node(dfg());
         node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
      if (tfex->node_kind(node) != TFN_CALL)
        continue;
      
      const char *call_id = tfex->node_call_id(node);
      TOP top = call_id_top(call_id);
      if (top == TOP_UNDEFINED)
        continue;
      
      /* In the first pass, convert the call id to lower case so that
         the canonicalizer works correctly. */
      if (pass == 0) {
        char *tmp_id = xt_pool_strdup((XT_MEMPOOL *)tfex->dfg_pool(dfg()),
                                      call_id);
        for (char *ch = tmp_id; *ch; ch++) {
          *ch = tolower(*ch);
        }
        tfex->node_set_call_id(node, tmp_id);
        continue;
      }

      /* In the second pass, convert the call to a node with a kind. */
      if (pass == 1) {
        tf_node_kind_t node_kind = CG_FUSION_Top_To_Node_Kind(top);
        if (node_kind != TFN_UNKNOWN) {
          Is_True(node_kind != TFN_CALL, ("Did not expect CALL node kind"));
          tfex->node_set_call_id(node, NULL);
          tfex->node_set_kind(node, node_kind);
        }
        continue;
      }
    }

    /* Canonicalize at the end of the first pass. */
    if (pass == 0) {
      tfex->match_canonicalize(dfg(), NULL);
    }
  }
}


bool
FUSION_OP::maybe_fusion (void)
{
  if (xtie_signals_num_interfaces(xsigs()) == 0)
    return false;
  
  if (xtie_signals_num_exceptions(xsigs()) > 0)
    return false;
  
  XT_ASSERTX(!is_special_top(top()));
  
  return true;
}


bool
FUSION_OP::maybe_inlined_in_fusion (void)
{
  return maybe_fusion();
} 


INT
FUSION_OP::value_idx (void) const
{
  if (is_load())
    return TI_TOP_Find_Result_Use(top(), OU_loadval);
  
  if (is_store())
    return TI_TOP_Find_Operand_Use(top(), OU_storeval);

  return -1;
}
    

INT
FUSION_OP::register_inputs (void) const
{
  const ISA_OPERAND_INFO *opnd_info = isa_operand_info();
  INT operands = TI_ISA_Op_Operands(opnd_info);
  INT count = 0;
  for (INT oidx = 0; oidx < operands; oidx++) {
    const ISA_OPERAND_VALTYP *opnd_valtyp = TI_ISA_Op_Operand(opnd_info, oidx);

    if (TI_ISA_Valtyp_Is_Register(opnd_valtyp))
      count++;
  }

  return count;
}


void
FUSION_OP::print (FILE *file, INT tab)
{
  fprintf(file, "%*sFUSION_OP: %s top %d nodes %d%s%s%s",
	  tab, "", name(), (INT)top(), node_count(),
	  is_load() ? " LOAD" : "",
	  is_store() ? " STORE" : "",
	  is_base_update() ? " BASE_UPDATE" : "");
  
  if (is_load() || is_store()) {
    if (base_idx() >= 0)
      fprintf(file, " base %d", base_idx());
    
    if (offset_idx() >= 0)
      fprintf(file, " offset %d", offset_idx());
  }
  
  fprintf(file, "\n");
  
  tfex->op_dfg_print(op_dfg(), file, tab + 2);
}


void
FUSION_OP::print_debug (void)
{
  print(stderr, 0);
}


//
// bb_match_callback
//


static
int bb_match_node_compare (tf_match_callback_t *cback,
			   tf_node_t node_a, tf_node_t node_b)
{
  Is_True(node_a != TF_NODE_INVALID && node_b != TF_NODE_INVALID,
	  ("Invalid node"));
  
  tf_node_kind_t kind_a = tfex->node_kind(node_a);
  if ((kind_a == TFN_CALL || kind_a == TFN_TIECOMP || kind_a == TFN_USER) &&
      tfex->node_user(node_b) != NULL) {
    /* 'node_a' is a TFN_CALL or TFN_TIECOMP coming from a post-processed TIE
       reference and 'node_b' is an operation node coming from a basic block's DFG.
       Check if 'node_a' actually calls an opcode function and perform the
       the correct comparison. */
    
    TOP top_a = node_top(node_a);
    if (top_a == TOP_UNDEFINED)
      return 0;
    
    OP *op = (OP *)tfex->node_user(node_b);
    Is_True(op, ("Null op in a BB DFG node"));
    
    TOP top_op = OP_code(op);

    if (top_op == top_a && CG_Fusion->tracing()) {
      fprintf(CG_Fusion->trace_file(), "matched %s\n", TI_TOP_Name(top_op));
    }
    
    return (top_op == top_a);
  }
  
  BB_MATCH_CBACK_USER *user = (BB_MATCH_CBACK_USER *)cback->user;
  return user->inherited_cback()->node_compare(cback, node_a, node_b);
}


static int
bb_match_canonical_order_nodes (tf_match_callback_t *cback,
				tf_node_t node_a, tf_node_t node_b)
{
  Is_True(node_a != TF_NODE_INVALID && node_b != TF_NODE_INVALID,
	  ("Invalid node"));
  
  OP *op_a = (OP *)tfex->node_user(node_a);
  OP *op_b = (OP *)tfex->node_user(node_b);
  
  if (op_a && op_b) {
    const char *opcode_name_a = TI_TOP_Name(OP_code(op_a));
    const char *opcode_name_b = TI_TOP_Name(OP_code(op_b));
    INT order = strcmp(opcode_name_a, opcode_name_b);
    if (order != 0)
      return order;
  }
  
  BB_MATCH_CBACK_USER *user = (BB_MATCH_CBACK_USER *)cback->user;
  return user->inherited_cback()->canonical_order_nodes(cback, node_a, node_b);
}


static int
bb_match_found_match (tf_match_callback_t *cback, tf_match_map_t match_map)
{
  /* This method is called everytime a set of ops that can be matched
     by a fusion is found. We need to check whether replacing these ops
     with the fusion is legal. */
  
  BB_MATCH_CBACK_USER *user = (BB_MATCH_CBACK_USER *)cback->user;

  // Check if the matched operand register classes are compatible.
  if (!user->found_match_check_operands(match_map))
    return 0;
  
  // Check if the dependences between the ops allow fusion.
  if (!user->found_match_check_dependences(match_map))
    return 0;
  
  return 1;
}
  
static int
op_dfg_canonical_order_nodes (tf_match_callback_t *cback,
				tf_node_t node_a, tf_node_t node_b)
{
  Is_True(node_a != TF_NODE_INVALID && node_b != TF_NODE_INVALID,
	  ("Invalid node"));
  
  TOP top_a = (TOP)(unsigned int)tfex->node_user(node_a);
  TOP top_b = (TOP)(unsigned int)tfex->node_user(node_b);
  
  const char *opcode_name_a = TI_TOP_Name(top_a);
  const char *opcode_name_b = TI_TOP_Name(top_b);
  INT order = strcmp(opcode_name_a, opcode_name_b);
  if (order != 0)
    return order;
  
  BB_MATCH_CBACK_USER *user = (BB_MATCH_CBACK_USER *)cback->user;
  return user->inherited_cback()->canonical_order_nodes(cback, node_a, node_b);
}


bool
BB_MATCH_CBACK_USER::found_match_check_operands (tf_match_map_t match_map)
{
  tf_op_dfg_t op_dfg = fusion_op()->op_dfg();
  const ISA_OPERAND_INFO *opnd_info = fusion_op()->isa_operand_info();

  tf_opnd_opnd_map_t opnd_map = tfex->match_map_opnd_map(match_map);
    
  // Check the inputs.
  INT in_idx = 0;
  tf_opnd_vec_t inputs = tfex->op_dfg_inputs(op_dfg);
  INT isize = tfex->opnd_vec_size(inputs);
  for (INT oidx = 0; oidx < isize; oidx++) {
    tf_opnd_t tf_opnd = tfex->opnd_vec_get(inputs, oidx);
    if (tf_opnd == TF_OPND_INVALID)
      continue;
    
    tf_opnd_t bb_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, tf_opnd, &bb_opnd);
    Is_True(bb_opnd != TF_OPND_INVALID, ("Can't find a matching operand"));
    
    TN *tn = (TN *)tfex->opnd_user(bb_opnd);
    Is_True(tn != NULL, ("Null TN in operand"));
	
    const ISA_OPERAND_VALTYP *opnd_valtyp = TI_ISA_Op_Operand(opnd_info, in_idx);
    if (!compatible_valtyp_tn(opnd_valtyp, tn)) {
      DevWarn("Fusion %s matched but input operands [%d] are not compatible",
	      fusion_op()->name(), in_idx);
      return false;
    }
	
    in_idx++;
  }
  Is_True(in_idx == TI_ISA_Op_Operands(opnd_info),
	  ("Input operand counts mismatch (%d != %d)", in_idx, TI_ISA_Op_Operands(opnd_info)));
    
  // Check the outputs.
  INT out_idx = 0;
  tf_opnd_vec_t outputs = tfex->op_dfg_outputs(op_dfg);
  INT osize = tfex->opnd_vec_size(outputs);
  for (INT oidx = 0; oidx < osize; oidx++) {
    tf_opnd_t tf_opnd = tfex->opnd_vec_get(outputs, oidx);
    if (tf_opnd == TF_OPND_INVALID)
      continue;
	
    tf_opnd_t bb_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, tf_opnd, &bb_opnd);
    Is_True(bb_opnd != TF_OPND_INVALID, ("Can't find a matching operand"));
	
    TN *tn = (TN *)tfex->opnd_user(bb_opnd);
    Is_True(tn != NULL, ("Null TN in operand"));
    
    const ISA_OPERAND_VALTYP *opnd_valtyp = TI_ISA_Op_Result(opnd_info, out_idx);
    if (!compatible_valtyp_tn(opnd_valtyp, tn)) {
      DevWarn("Fusion %s matched but output operands [%d] are not compatible",
	      fusion_op()->name(), out_idx);
      return false;
    }
	
    out_idx++;
  }
  Is_True(out_idx == TI_ISA_Op_Results(opnd_info),
	  ("Output operand counts mismatch (%d != %d)", out_idx, TI_ISA_Op_Results(opnd_info)));

  return true;
}


bool
BB_MATCH_CBACK_USER::dependence_out_and_in (tf_node_t node, tf_node_node_map_t node_map)
{
  if (tfex->node_node_map_find(node_map, node, NULL)) {
    return false;
  }
  
  // Found a path going out of the matched BB op node. Check if it goes back in.
  tf_node_set_t node_preds = TF_NODE_SET_INVALID;
  tfex->node_node_set_map_find(bb_dfg_preds(), node, &node_preds);
  Is_True(node_preds != TF_NODE_SET_INVALID, ("Can't find node's predecessors"));
  
  for (tf_node_set_iter_t piter = tfex->node_set_iter_first(node_preds);
       piter != TF_NODE_SET_ITER_INVALID;
       piter = tfex->node_set_iter_next(piter)) {
    tf_node_t pred = tfex->node_set_iter_elem(piter);
    Is_True(pred != TF_NODE_INVALID, ("Invalid node"));
    
    if (tfex->node_node_map_find(node_map, pred, NULL)) {
      DevWarn("Fusion %s matched but dependences go out and back in",
	      fusion_op()->name());
      return true;
    }
  }

  return false;
}


bool
BB_MATCH_CBACK_USER::found_match_check_dependences (tf_match_map_t match_map)
{
  Is_True(bb_dfg_preds(), ("Null DFG predecessor sets"));

  tf_dfg_t dfg = tfex->match_map_dfg(match_map);
  tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);

  for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
    tf_node_t node = tfex->node_node_pair_key(node_pair);

    /* Traverse all input operands. */
    INT in_size = tfex->node_input_count(node);
    for (INT idx = 0; idx < in_size; idx++) {
      tf_opnd_t in_opnd = tfex->node_input(node, idx);
      Is_True(in_opnd != TF_OPND_INVALID, ("Iinvalid operand"));

      tf_node_t last_source_node = TF_NODE_INVALID;
      INT edges = tfex->opnd_edge_idxs_count(in_opnd);
      for (INT pos = 0; pos < edges; pos++) {
	tf_edge_t in_edge = tfex->opnd_edge(in_opnd, pos);
	if (in_edge == TF_EDGE_INVALID)
	  continue;

	tf_node_t source_node = tfex->edge_source_node(in_edge);
	Is_True(source_node != TF_NODE_INVALID, ("Invalid source node"));
	
	if (source_node == last_source_node)
	  continue;

	last_source_node = source_node;
	if (dependence_out_and_in(source_node, rev_node_map)) {
	  return false;
	}
      }
    }

    /* Traverse all predecessors. */
    tf_edge_idx_dlist_t preds = tfex->node_preds(node);
    for (tf_edge_idx_dlist_iter_t piter = tfex->edge_idx_dlist_iter_first(preds);
	 piter != TF_EDGE_IDX_DLIST_ITER_INVALID;
	 piter = tfex->edge_idx_dlist_iter_next(piter)) {
      tf_edge_idx_t in_eidx = tfex->edge_idx_dlist_iter_elem(piter);
      tf_edge_t in_edge = tfex->dfg_edge(dfg, in_eidx);
      Is_True(in_edge != TF_EDGE_INVALID, ("Invalid predecessor edge"));

      tf_node_t source_node = tfex->edge_source_node(in_edge);
      Is_True(source_node != TF_NODE_INVALID, ("Invalid source node"));
      
      if (dependence_out_and_in(source_node, rev_node_map)) {
	return false;
      }
    }
  }
  
  return true;
}


//
// tie_match_callback
//


static int
tie_match_found_match (tf_match_callback_t *cback, tf_match_map_t match_map)
{
  TIE_MATCH_CBACK_USER *user = (TIE_MATCH_CBACK_USER *)cback->user;

  Is_True(user->sub_fusion_op(), ("Null sub fusion op"));
  Is_True(user->fusion_op(), ("Null fusion op"));
  
  tf_op_dfg_t sub_op_dfg = user->sub_fusion_op()->op_dfg();
  tf_op_dfg_t op_dfg = user->fusion_op()->op_dfg();

  FUSION_OP *sub_fusion_op = user->sub_fusion_op();
  FUSION_OP *fusion_op = user->fusion_op();

  tf_opnd_opnd_map_t opnd_map = tfex->match_map_opnd_map(match_map);
    
  for (INT dir = 0; dir < 2; dir++) {
    bool inputs = (dir == 0);
    tf_opnd_vec_t sub_opnds = inputs ?
      tfex->op_dfg_inputs(sub_op_dfg) : tfex->op_dfg_outputs(sub_op_dfg);
    tf_opnd_vec_t opnds = inputs ?
      tfex->op_dfg_inputs(op_dfg) : tfex->op_dfg_outputs(op_dfg);
    
    int sub_size = tfex->opnd_vec_size(sub_opnds);
    for (INT sub_idx = 0; sub_idx < sub_size; sub_idx++) {
      tf_opnd_t sub_opnd = tfex->opnd_vec_get(sub_opnds, sub_idx);
      if (sub_opnd == TF_OPND_INVALID)
	continue;
      
      tf_opnd_t opnd = TF_OPND_INVALID;
      tfex->opnd_opnd_map_find(opnd_map, sub_opnd, &opnd);
      Is_True(opnd != TF_OPND_INVALID, ("Null sub operand not mapped"));
	    
      OP_SIGNAL *sub_osig = sub_fusion_op->find_signal(sub_idx, true, true);
      Is_True(sub_osig, ("Can't find signal"));

      xtie_signal sub_xsig = sub_osig->xsig();

      xtie_signal_kind sub_skind = xtie_signal_get_kind(sub_xsig);
      Is_True(sub_skind != XTIE_SIG_UNKNOWN,
	      ("Unknown signal kind for signal %s", xtie_signal_get_name(sub_xsig)));
      
      switch (sub_skind) {
      case XTIE_SIG_OPERAND:
      case XTIE_SIG_FIELD:
      case XTIE_SIG_STATE:
      {
	if (tfex->opnd_is_input(opnd) != inputs)
	  continue;

	if (!tfex->opnd_is_unknown(opnd))
	  continue;
        
	INT idx = tfex->opnd_vec_find(opnds, opnd);
	Is_True(idx >= 0, ("Can't find input/output operand"));

        OP_SIGNAL *osig = fusion_op->find_signal(idx, true, true);
	Is_True(osig, ("Can't find signal"));

	xtie_signal xsig = osig->xsig();
        if (xtie_signal_get_kind(xsig) != sub_skind)
	  return false;
        
	// TODO: Check register classes compatibility.
	// Also, we can use the source and the sink edges to
	// check if they go into compatible call nodes.
		
	break;
      }

      case XTIE_SIG_INTERFACE:
      {
	// Match interface operands by name.
		
	if (!tfex->opnd_is_unknown(opnd))
	  return false;
		
	// We don't deal yet with certain funky input/output copy for
	// interfaces mappings.
	if (tfex->opnd_is_input(opnd) != inputs)
	  return false;
		
	INT idx = tfex->opnd_vec_find(opnds, opnd);
	Is_True(idx >= 0, ("Can't find input/output operand"));

	OP_SIGNAL *osig = fusion_op->find_signal(idx, true, true);
	Is_True(osig, ("Can't find signal"));

	xtie_signal xsig = osig->xsig();
	
	if (strcmp(xtie_signal_get_name(sub_xsig),
                   xtie_signal_get_name(xsig)))
	  return false;

	Is_True(xtie_signal_get_kind(xsig) == XTIE_SIG_INTERFACE,
		("Unexpected signal kind"));
        
	break;
      }

      default:
	return false;
      }
    }
  }
	
  tf_node_node_map_t node_map = tfex->match_map_node_map(match_map);
  for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
    tf_node_t node_a = tfex->node_node_pair_key(node_pair);
    
    /* For TFN_TABLE nodes, check if the table names match. */
    if (tfex->node_kind(node_a) == TFN_TABLE) {
      tf_node_t node_b = tfex->node_node_pair_value(node_pair);
      Is_True(tfex->node_kind(node_b) == TFN_TABLE, ("Bad match."));
      tf_opnd_t opnd_a_tbl = tfex->node_input(node_a, 0);
      tf_opnd_t opnd_b_tbl = tfex->node_input(node_b, 0);
      const char *tbl_a_name = tfex->opnd_name(opnd_a_tbl);
      const char *tbl_b_name = tfex->opnd_name(opnd_b_tbl);
      Is_True(tbl_a_name && tbl_b_name, ("NULL table name in TFN_TABLE node."));
      if (strcmp(tbl_a_name, tbl_b_name)) {
        return false;
      }
    }
  }
  
  return true;
}


//
// upd_match_callback
//


static int
upd_match_found_match (tf_match_callback_t *cback, tf_match_map_t match_map)
{
  UPD_MATCH_CBACK_USER *user = (UPD_MATCH_CBACK_USER *)cback->user;

  Is_True(user->non_updating_op(), ("Null non-updating op"));
  Is_True(user->updating_op(), ("Null updating op"));
  Is_True(user->created_op_dfg(), ("Null created op DFG"));
  
  tf_op_dfg_t nu_op_dfg = user->non_updating_op()->op_dfg();
  tf_op_dfg_t u_op_dfg = user->updating_op_dfg();
  Is_True(u_op_dfg, ("Null op dfg"));
  
  const ISA_OPERAND_INFO *nu_opnd_info = user->non_updating_op()->isa_operand_info();
  const ISA_OPERAND_INFO *u_opnd_info = user->updating_op()->isa_operand_info();
  
  INT nu_operands = TI_ISA_Op_Operands(nu_opnd_info);
  INT nu_results = TI_ISA_Op_Results(nu_opnd_info);
  INT u_operands = TI_ISA_Op_Operands(u_opnd_info);
  INT u_results = TI_ISA_Op_Results(u_opnd_info);
  Is_True(nu_operands == u_operands && (nu_results + 1) == u_results,
	  ("Unexpected operand / result counts"));

  tf_opnd_opnd_map_t opnd_map = tfex->match_map_opnd_map(match_map);

  tf_opnd_vec_t created_inputs = tfex->op_dfg_inputs(user->created_op_dfg());
  tf_opnd_vec_t u_inputs = tfex->op_dfg_inputs(u_op_dfg);
  tf_opnd_vec_t nu_inputs = tfex->op_dfg_inputs(nu_op_dfg);

  // Check the inputs.
  INT nu_in_idx = 0;  
  INT isize = tfex->opnd_vec_size(nu_inputs);
  for (INT oidx = 0; oidx < isize; oidx++) {
    tf_opnd_t nu_opnd = tfex->opnd_vec_get(created_inputs, oidx);
    if (nu_opnd == TF_OPND_INVALID)
      continue;
    
    tf_opnd_t u_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, nu_opnd, &u_opnd);
    Is_True(u_opnd != TF_OPND_INVALID, ("Can't find a matching operand"));
    
    INT u_oidx = tfex->opnd_vec_find(u_inputs, u_opnd);
    if (oidx != u_oidx)
      return false;
    
    INT u_in_idx = tfex->op_dfg_skip_null_find_input(u_op_dfg, u_opnd);
    Is_True(u_in_idx == nu_in_idx,
	    ("Operand indexes mismatch (%d != %d)\n", u_in_idx, nu_in_idx));
    
    if (u_in_idx < u_operands) {
      const ISA_OPERAND_VALTYP *nu_opnd_valtyp = TI_ISA_Op_Operand(nu_opnd_info, nu_in_idx);
      const ISA_OPERAND_VALTYP *u_opnd_valtyp = TI_ISA_Op_Operand(u_opnd_info, u_in_idx);

      if (!compatible_valtyps(nu_opnd_valtyp, u_opnd_valtyp)) {
	return false;
      }
    } else {
      const char *u_name = tfex->opnd_name(u_opnd);
      const char *nu_name = tfex->opnd_name(nu_opnd);
      Is_True(u_name && nu_name, ("Unexpected anonymous input operands."));
      if (strcasecmp(u_name, nu_name))
	return false;
    }
    
    nu_in_idx++;
  }
  
  // Check the outputs.
  INT base_idx = user->non_updating_op()->base_idx();
  Is_True(base_idx >= 0, ("Can't find base index for %s",
			  user->non_updating_op()->name()));
  
  tf_opnd_t base_opnd = tfex->op_dfg_skip_null_input(nu_op_dfg, base_idx);
  Is_True(base_opnd != TF_OPND_INVALID,
	  ("Can't find base operand at index %d (op %s)", base_idx,
	   user->non_updating_op()->name()));
  
  INT base_opnd_idx = tfex->opnd_vec_find(nu_inputs, base_opnd);
  Is_True(base_opnd_idx >= 0,
	  ("Can't find base input operand (op %s)", user->non_updating_op()->name()));
  
  tf_opnd_vec_t created_outputs = tfex->op_dfg_outputs(user->created_op_dfg());
  tf_opnd_vec_t u_outputs = tfex->op_dfg_outputs(u_op_dfg);

  INT nu_out_idx = 0;
  INT osize = tfex->opnd_vec_size(tfex->op_dfg_outputs(nu_op_dfg));
  for (INT oidx = 0; oidx < osize; oidx++) {
    tf_opnd_t nu_opnd = tfex->opnd_vec_get(created_outputs, oidx);
    if (nu_opnd == TF_OPND_INVALID)
      continue;
    
    tf_opnd_t u_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, nu_opnd, &u_opnd);
    Is_True(u_opnd != TF_OPND_INVALID, ("Can't find a matching result"));
    
    INT u_oidx = tfex->opnd_vec_find(u_outputs, u_opnd);
    if (oidx != u_oidx)
      return false;
    
    INT u_out_idx = tfex->op_dfg_skip_null_find_output(u_op_dfg, u_opnd);
    Is_True(u_out_idx == nu_out_idx,
	    ("Operand indexes mismatch (%d != %d)\n", u_out_idx, nu_out_idx));
    
    if (u_out_idx < u_results) {
      /* The non-updating opcode has no base result operand so we need to
	 use the input operand to check compatibility. Also, we need to shift
	 all indexes after the base operand index by 1 in order to compensate
	 for its absence. */
      const ISA_OPERAND_VALTYP *nu_opnd_valtyp = NULL;
      if (oidx < base_opnd_idx) {
	nu_opnd_valtyp = TI_ISA_Op_Result(nu_opnd_info, nu_out_idx);
      } else if (oidx == base_opnd_idx) {
	nu_opnd_valtyp = TI_ISA_Op_Operand(nu_opnd_info, base_idx);
      } else {
	nu_opnd_valtyp = TI_ISA_Op_Result(nu_opnd_info, nu_out_idx - 1);
      }
      
      const ISA_OPERAND_VALTYP *u_opnd_valtyp = TI_ISA_Op_Result(u_opnd_info, u_out_idx);
      
      if (!compatible_valtyps(nu_opnd_valtyp, u_opnd_valtyp)) {
	return false;
      }
    } else {
      const char *u_name = tfex->opnd_name(u_opnd);
      const char *nu_name = tfex->opnd_name(nu_opnd);
      Is_True(u_name && nu_name, ("Unexpected anonymous output operands."));
      if (strcasecmp(u_name, nu_name))
	return false;
    }
    
    nu_out_idx++;
  }
  
  return true;
}


//
// dfg_callback
//


static void
dfg_cback_opnd_print (tf_dfg_callback_t *, tf_opnd_t opnd, FILE *file)
{
  Is_True(opnd != TF_OPND_INVALID, ("Invalid operand"));
  TN *tn = (TN *)tfex->opnd_user(opnd);
  if (tn)
    fPrint_TN(file, "%s", tn);
}


static void
dfg_cback_node_print (tf_dfg_callback_t *, tf_node_t node, FILE *file)
{
  Is_True(node != TF_NODE_INVALID, ("Invalid node"));
  OP *op = (OP *)tfex->node_user(node);
  if (op)
  {
    const char *op_name = TI_TOP_Name(OP_code(op));
    fprintf(file, "%s", op_name);
  }
}


static int
dfg_cback_node_emit_vcg_label (tf_dfg_callback_t *, tf_node_t node, FILE *file)
{
  Is_True(node != TF_NODE_INVALID, ("Null node"));
  OP *op = (OP *)tfex->node_user(node);
  if (op)
  {
    const char *op_name = TI_TOP_Name(OP_code(op));
    fprintf(file, " label: \"%s\"", op_name);
    return 1;
  }
  
  return 0;
}

static void
op_dfg_cback_node_print (tf_dfg_callback_t *, tf_node_t node, FILE *file)
{
  Is_True(node != TF_NODE_INVALID, ("Invalid node"));
  TOP top = (TOP)(unsigned int)tfex->node_user(node);
  if (top!=TOP_UNDEFINED)
  {
    const char *op_name = TI_TOP_Name(top);
    fprintf(file, "%s", op_name);
  }
}


//
// CG_FUSION::VECTOR_TYPE_INFO
//


CG_FUSION::VECTOR_TYPE_INFO::VECTOR_TYPE_INFO (CG_FUSION *cg_fusion,
					       TYPE_ID scalar_type,
					       INT vector_length,
					       TYPE_ID vector_mem_type,
					       TYPE_ID vector_reg_type) :
  _cg_fusion(cg_fusion),
  _scalar_type(scalar_type), _vector_length(vector_length),
  _vector_mem_type(vector_mem_type), _vector_reg_type(vector_reg_type)
{
  init_rtor();
}


void
CG_FUSION::VECTOR_TYPE_INFO::init_rtor (void)
{
  _rtor_top = TOP_UNDEFINED;
  _rtor_fusion_op = NULL;

  TIE_MACRO *rtor_macro = tie_info->mtype_rtor_macro(scalar_type(), vector_reg_type());
  if (!rtor_macro || !rtor_macro->is_pure() || rtor_macro->has_side_effect())
    return;
    
  // Check the macro operands
  if (rtor_macro->num_protos() != 2 ||
      !rtor_macro->proto_is_out(0) ||	!rtor_macro->proto_is_in(1) ||
      rtor_macro->proto_mtype_id(tie_info, 0) != vector_reg_type() ||
      rtor_macro->proto_mtype_id(tie_info, 1) != scalar_type())
    return;
    
  // The conversion macro should consist of a single instruction.
  if (rtor_macro->num_instructions() != 1)
    return;
    
  const char *opcode_name = rtor_macro->inst_opcode_name(0);
  TOP top = TI_TOP_Topcode(opcode_name);
  Is_True(top != TOP_UNDEFINED, ("Can't find topcode for %s", opcode_name));
    
  const ISA_OPERAND_INFO *isa_opnd_info = TI_ISA_Operand_Info(top);
  if (TI_ISA_Op_Operands(isa_opnd_info) != 1 || TI_ISA_Op_Results(isa_opnd_info) != 1)
    return;

  FUSION_OP *fusion_op = _cg_fusion->rtor_op_map()->Find(top);
  if (!fusion_op) {
    fusion_op = _cg_fusion->all_op_map()->Find(top);
    if (!fusion_op) {
      SIGNAL_VEC *osigs = NULL;
      xtie_signals xsigs = NULL;
      xtie_opcode xop = xtie_find_opcode(TI_TIE_Xtie_Post_Rewrite(), opcode_name);
      tf_op_dfg_t op_dfg = _cg_fusion->build_op_dfg(_cg_fusion->xt_init_pool(),
						    xop, &xsigs, &osigs);
      if (op_dfg == TF_OP_DFG_INVALID) {
        return;
      }

      fusion_op =
        CXX_NEW(FUSION_OP(_cg_fusion, top, op_dfg, xsigs, osigs),
			  _cg_fusion->fusion_pool());
    }
    
    // Make a copy of the rtor op dfg.
    fusion_op = fusion_op->clone_op_dfg(_cg_fusion->fusion_pool());
    
    // Inline all called TIE functions.
    if (!_cg_fusion->inline_all_functions(fusion_op->dfg()))
      return;
    
    _cg_fusion->add_rtor_op(top, fusion_op);
  }

  Is_True(fusion_op, ("Null fusion op"));
  _rtor_top = top;
  _rtor_fusion_op = fusion_op;
}


void
CG_FUSION::VECTOR_TYPE_INFO::print (FILE *file, INT tab)
{
  fprintf(file, "%*sCG_FUSION::VECTOR_TYPE_INFO {\n", tab, "");
  tab += 2;
  {
    fprintf(file, "%*sscalar_type:     %s [%d]\n", tab, "",
	    MTYPE_name(scalar_type()), (INT)scalar_type());
    fprintf(file, "%*svector_mem_type: %s [%d]\n", tab, "",
	    MTYPE_name(vector_mem_type()), (INT)vector_mem_type());
    fprintf(file, "%*svector_reg_type: %s [%d]\n", tab, "",
	    MTYPE_name(vector_reg_type()), (INT)vector_reg_type());
    fprintf(file, "%*srtor_top:        %s [%d]\n", tab, "",
	    TI_TOP_Name(rtor_top()), (INT)rtor_top());
    if (rtor_fusion_op())
      rtor_fusion_op()->print(file, tab + 4);
  }
  tab -= 2;
  fprintf(file, "%*s}\n");
}


void
CG_FUSION::VECTOR_TYPE_INFO::print_debug (void)
{
  print(stderr, 0);
}


//
// CG_FUSION
//


CG_FUSION::CG_FUSION (bool tracing) :
  _tracing(tracing),
  _match_cback(NULL),
  _bb_match_cback(NULL), 
  _tie_match_cback(NULL),
  _upd_match_cback(NULL),
  _dfg_cback(NULL),
  _op_dfg_cback(NULL),
  _all_ops(NULL),
  _fusion_ops(NULL),
  _all_op_map(NULL),
  _rtor_op_map(NULL),
  _vector_type_info(NULL)
{
  if (_tracing) {
    fprintf(trace_file(),
            CG_FUSION_LINE "CG_FUSION Initialization\n" CG_FUSION_LINE);
  }

  /* Quick, inaccurate screen for fusions. */
  bool has_imap = imap_scan();
  bool has_fusion_op = fusion_operation_scan();

  if (!CG_find_updating && !has_imap && !has_fusion_op)
    return;
  
  init_dfg_callbacks();
  
  init_match_callbacks();

  _all_ops = CXX_NEW(FUSION_OPS(fusion_pool()), fusion_pool());
  _all_op_map = CXX_NEW(FUSION_OP_MAP(127, fusion_pool()), fusion_pool());
  _rtor_op_map = CXX_NEW(FUSION_OP_MAP(29, fusion_pool()), fusion_pool());
  
  if (has_fusion_op) {
  
    /* Initialize all global TIE tables that we care about. */
    init_tie_tables();

    /* Initialize all global functions that we care about. */
    init_tie_functions();

    /* Build the DFGs for all TIE ops. */
    init_all_ops();
  }
  
  if (CG_find_updating) {
    /* Identify all updating/non-updating load/store pairs. */
    init_updating_ops();
  }
  
  if (CG_fusion) {

    _fusion_ops = CXX_NEW(FUSION_OPS(fusion_pool()), fusion_pool());

    /* Identify all imaps that can be used as fusions. */
    init_imaps();
    
    /* Verify that the opcode references and the corresponding xt_reference
       functions are equivalent. */
    init_reference_functions();
    
    /* Identify all ops that can be used as fusions. */
    init_fusion_ops();
    
    /* Initialize the vector type info. This must be done after parsing and
       building DFGs for all ops because of the converting protos. */
    init_vector_type_info();
  }
}


BB_MATCH_CBACK_USER *
CG_FUSION::bb_match_cback_user (void) const
{
  return (BB_MATCH_CBACK_USER *)bb_match_cback()->user;
}


TIE_MATCH_CBACK_USER *
CG_FUSION::tie_match_cback_user (void) const
{
  return (TIE_MATCH_CBACK_USER *)tie_match_cback()->user;
}


UPD_MATCH_CBACK_USER *
CG_FUSION::upd_match_cback_user (void) const
{
  return (UPD_MATCH_CBACK_USER *)upd_match_cback()->user;
}


void
CG_FUSION::init_vector_type_info (void)
{
  /* If there're no fusion opcodes, then any vector type information will
     be useless. */
  if (!_fusion_ops || _fusion_ops->Elements() == 0)
    return;
  
  _vector_type_info = CXX_NEW(VECTOR_TYPE_INFO_MAP(127, fusion_pool()), fusion_pool());
  
  ISA_SIMD_TYPE_INFO isi;
  FOR_ALL_ISA_SIMD_TYPE_INFO(isi) {
    INT vl = TI_ISA_Simd_Type_Vl(isi);
    TYPE_ID scalar_type = TI_ISA_Simd_Base_Mtype(isi);
    TYPE_ID reg_type = TI_ISA_Simd_Vector_Register_Mtype(isi);
    TYPE_ID mem_type = TI_ISA_Simd_Vector_Memory_Mtype(isi);
	
    if (vl < 2 || scalar_type == MTYPE_UNKNOWN ||
	reg_type == MTYPE_UNKNOWN || mem_type == MTYPE_UNKNOWN) {
      DevWarn("Bad VECTOR_TYPE_INFO structure ignored (%s x%d)",
	      MTYPE_name(scalar_type), vl);
      continue;
    }
    
    VECTOR_TYPE_INFO *vti = CXX_NEW(VECTOR_TYPE_INFO(this,
						     scalar_type, vl, mem_type, reg_type),
				    fusion_pool());
	
    _vector_type_info->Enter_If_Unique(vti->vector_mem_type(), vti);
    _vector_type_info->Enter_If_Unique(vti->vector_reg_type(), vti);
	
    if (tracing()) {
      vti->print(trace_file());
    }
  }
}


bool
CG_FUSION::imap_scan (void)
{
  /* Return 'true' if we have imap */
  xtie_phase xp_compiler = TI_TIE_Xtie_Compiler();
  xtie_foreach_imap(xp_compiler,x) {
    return true;
  }
  end_xtie_foreach_imap;
  return false;
}


bool
CG_FUSION::fusion_operation_scan (void)
{
  return TI_TIE_Need_Post_Rewrite_TIE();
}


void
CG_FUSION::init_dfg_callbacks (void)
{
  _dfg_cback = TYPE_MEM_POOL_ALLOC(tf_dfg_callback_t, fusion_pool());
  _op_dfg_cback = TYPE_MEM_POOL_ALLOC(tf_dfg_callback_t, fusion_pool());

  tfex->dfg_callback_init(_dfg_cback);
  tfex->dfg_callback_init(_op_dfg_cback);

  _dfg_cback->opnd_print = dfg_cback_opnd_print;
  _dfg_cback->node_print = dfg_cback_node_print;
  _dfg_cback->node_emit_vcg_label = dfg_cback_node_emit_vcg_label;

  _op_dfg_cback->node_print = op_dfg_cback_node_print;
}


void
CG_FUSION::init_match_callbacks (void)
{
  _match_cback = TYPE_MEM_POOL_ALLOC(tf_match_callback_t, fusion_pool());
  tfex->match_callback_init(_match_cback);

  /* Initialize the basic block match callback object so we can properly compare
     DFG nodes coming from TIE with basic block ops. */
  _bb_match_cback = TYPE_MEM_POOL_ALLOC(tf_match_callback_t, fusion_pool());
  tfex->match_callback_init(_bb_match_cback);

  BB_MATCH_CBACK_USER *bb_user = CXX_NEW(BB_MATCH_CBACK_USER(_match_cback), fusion_pool());
  _bb_match_cback->user = bb_user;
  
  /* Override the default behavior of these functions. */
  _bb_match_cback->node_compare = bb_match_node_compare;
  _bb_match_cback->canonical_order_nodes = bb_match_canonical_order_nodes;
  _bb_match_cback->found_match = bb_match_found_match;
  

  /* Initialize the TIE match callback object so we can properly compare TIE DFGs. */
  _tie_match_cback = TYPE_MEM_POOL_ALLOC(tf_match_callback_t, fusion_pool());
  tfex->match_callback_init(_tie_match_cback);
  
  TIE_MATCH_CBACK_USER *tie_user = CXX_NEW(TIE_MATCH_CBACK_USER, fusion_pool());
  _tie_match_cback->user = tie_user;

  /* Override the default behavior of found_match. */
  _tie_match_cback->found_match = tie_match_found_match;
  
  /* Initialize the updating match callback object so we can properly
     identify updating memory opcode pairs. */
  _upd_match_cback = TYPE_MEM_POOL_ALLOC(tf_match_callback_t, fusion_pool());
  tfex->match_callback_init(_upd_match_cback);
  
  UPD_MATCH_CBACK_USER *upd_user = CXX_NEW(UPD_MATCH_CBACK_USER, fusion_pool());
  _upd_match_cback->user = upd_user;

  /* Override the default behavior of found_match. */
  _upd_match_cback->found_match = upd_match_found_match;
}

void
CG_FUSION::init_tie_functions (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nInitializing TIE functions:\n");
  }

  _function_dfg_map = CXX_NEW(FUNCTION_DFG_MAP(127, fusion_pool()), fusion_pool());

  tf_sems_t sems = tfex->sems_default();
  
  xtie_foreach_function(TI_TIE_Xtie_Post_Rewrite(), xf)
    {
      const char *fname = xtie_function_get_name(xf);

      // Parse only autotie generated functions for now.
      if (strncmp(fname, AT_REFERENCE_PREFIX, AT_REFERENCE_PREFIX_LEN))
        continue;
      
      xtie_function_io fout = xtie_function_get_output(xf);
      int fmsb = xtie_function_io_get_from_index(fout);
      int flsb = xtie_function_io_get_to_index(fout);
      
      if (tracing())
        fprintf(trace_file(), "function [%d:%d] %s\n",
                fmsb, flsb, fname);

      tf_sem_tree_t func_io = tfex->sems_new_io(sems);
      for (xtie_function_io_iter it = xtie_function_get_input_iter(xf);
           it; it = xtie_function_io_iter_step(it)) {
        xtie_function_io xio = xtie_function_io_iter_get(it);
        int msb = xtie_function_io_get_from_index(xio);
        int lsb = xtie_function_io_get_to_index(xio);
        Is_True(msb >= lsb, ("MSB must be >= LSB"));
        tf_sem_tree_t sig = tfex->sems_new_signal(sems, TFS_OPERAND,
                                                  xtie_function_io_get_name(xio),
                                                  msb, lsb,
                                                  TF_SEM_DIR_IN);
        tfex->sem_tree_append_child(func_io, sig);
      }
      
      xtie_xml_item xml_body = xtie_function_get_statements(xf);
      tf_sem_tree_t func_body = xml2sem_fn(sems, xml_body);
      tf_sem_tree_t func = tfex->sems_new_function(sems, fname, fmsb, flsb,
                                                   func_io, func_body);
      tfex->sems_add_global_object(sems, func);
      
      if (tracing())
        fprintf(trace_file(), "%s: building DFG...\n", fname);
      tf_op_dfg_t tf_op_dfg = tfex->sem2dfg((tf_pool_t)xt_init_pool(), sems, func);
      if (tf_op_dfg == TF_OP_DFG_INVALID)
        continue;
      
      tfex->match_canonicalize(tfex->op_dfg_dfg(tf_op_dfg), NULL);
      
      _function_dfg_map->Enter(fname, tf_op_dfg);
      if (tracing()) {
        fprintf(trace_file(), "%s: DFG collected.\n", fname);
        tfex->op_dfg_print(tf_op_dfg, trace_file(), 4);
      }
    }
  end_xtie_foreach_function;
}


void
CG_FUSION::init_tie_tables (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nInitializing TIE tables:\n");
  }

  tf_sems_t sems = tfex->sems_default();
  
  xtie_foreach_table(TI_TIE_Xtie_Post_Rewrite(), xt)
    {
      const char *name = xtie_table_get_name(xt);
      int width = xtie_table_get_width(xt);
      int depth = xtie_table_get_depth(xt);
      if (tracing())
        fprintf(trace_file(), "table %s width %d depth %d\n",
                name, width, depth);
      
      tf_sem_tree_t tf_table =
        tfex->sems_new_table(sems, name, width, depth);
      
      tfex->sems_add_global_object(sems, tf_table);
    }
  end_xtie_foreach_table;
}


FUSION_OP *
CG_FUSION::find_updating_match (FUSION_OP *fop, tf_op_dfg_t update_op_dfg)
{
  XT_MEMPOOL::mark mark(xt_init_pool());
  
  if (tracing()) {
    fprintf(trace_file(), "%s: looking for updating match with DFG:\n", fop->name());
    tfex->op_dfg_print(update_op_dfg, trace_file(), 2);
  }
  
  const ISA_OPERAND_INFO *fop_opnd_info = fop->isa_operand_info();
  INT fop_operands = TI_ISA_Op_Operands(fop_opnd_info);
  INT fop_results = TI_ISA_Op_Results(fop_opnd_info);
  INT fop_reg_inputs = fop->register_inputs();
  
  upd_match_cback_user()->set_non_updating_op(fop);
  upd_match_cback_user()->set_created_op_dfg(update_op_dfg);
  
  for (INT i = 0; i < _all_ops->Elements(); i++) {
    FUSION_OP *update_fop = _all_ops->Get(i);
    if (update_fop == fop)
      continue;
    
    if (update_fop->is_load() != fop->is_load() || update_fop->is_store() != fop->is_store())
      continue;
    
    if (!update_fop->is_base_update() || update_fop->base_idx() < 0)
      continue;

    const ISA_OPERAND_INFO *update_opnd_info = update_fop->isa_operand_info();
    INT update_operands = TI_ISA_Op_Operands(update_opnd_info);
    INT update_results = TI_ISA_Op_Results(update_opnd_info);
    if (update_operands != fop_operands || update_results != (fop_results + 1))
      continue;

    INT update_reg_inputs = update_fop->register_inputs();
    if (fop_reg_inputs != update_reg_inputs)
      continue;

    INT fop_val_idx = fop->value_idx();
    INT update_val_idx = update_fop->value_idx();
    if ((fop_val_idx >= 0) != (update_val_idx >= 0))
      continue;
    
    if (fop_val_idx >= 0) {
      const ISA_OPERAND_VALTYP *fop_val = NULL;
      const ISA_OPERAND_VALTYP *update_val = NULL;
      if (fop->is_load()) {
	fop_val = TI_ISA_Op_Result(fop_opnd_info, fop_val_idx);
	update_val = TI_ISA_Op_Result(update_opnd_info, update_val_idx);
      } else {
	fop_val = TI_ISA_Op_Operand(fop_opnd_info, fop_val_idx);
	update_val = TI_ISA_Op_Operand(update_opnd_info, update_val_idx);
      }
      
      if (!compatible_valtyps(fop_val, update_val))
	continue;
    }

    for (INT u = 0; u < 2; u++) {
      bool cse = (u == 1);

      tf_op_dfg_t op_dfg = update_fop->op_dfg();
      if (cse) {
	op_dfg = tfex->op_dfg_copy((tf_pool_t)xt_init_pool(), op_dfg);
	tf_dfg_t dfg = tfex->op_dfg_dfg(op_dfg);
	if (!tfex->opt_optimize_cse(dfg))
	  continue;
	
	tfex->match_canonicalize(dfg, NULL);
	if (tracing()) {
	  fprintf(trace_file(), "  CSE'd updating DFG of %s:\n", update_fop->name());
	  tfex->op_dfg_print(op_dfg, trace_file(), 4);
	}
      }
      
      if (tracing())
	fprintf(trace_file(), "%s: matching with potentially updating %s%s...\n",
		fop->name(), update_fop->name(), cse ? " (CSE'd)" : "");
      
      upd_match_cback_user()->set_updating_op(update_fop);
      upd_match_cback_user()->set_updating_op_dfg(op_dfg);
      
      if (tfex->match_isomorphic((tf_pool_t)xt_init_pool(),
				    tfex->op_dfg_dfg(update_op_dfg),
				    tfex->op_dfg_dfg(op_dfg),
				    upd_match_cback()))
	return update_fop;
    }
  }
  
  return NULL;
}


FUSION_OP *
CG_FUSION::find_updating (FUSION_OP *fop)
{
  if (!fop->is_load() && !fop->is_store())
    return NULL;
  
  if (tracing())
    fprintf(trace_file(), "%s: checking...\n", fop->name());
  
  INT base_idx = fop->base_idx();
  if (base_idx < 0 || fop->offset_idx() < 0) {
    if (tracing())
      fprintf(trace_file(), "%s: no base/offset indexes, skipped.\n", fop->name());
    
    return NULL;
  }
  
  tf_op_dfg_t op_dfg = fop->op_dfg();
  
  tf_opnd_t base_opnd = tfex->op_dfg_skip_null_input(op_dfg, base_idx);
  Is_True(base_opnd,
	  ("Can't find base operand at index %d (op %s)", base_idx, fop->name()));
  
  tf_opnd_vec_t iopnds = tfex->op_dfg_inputs(op_dfg);
  INT base_opnd_idx = tfex->opnd_vec_find(iopnds, base_opnd);
  Is_True(base_opnd_idx >= 0, ("Can't find base input operand (op %s)", fop->name()));

  tf_opnd_vec_t oopnds = tfex->op_dfg_outputs(op_dfg);
  if (tfex->opnd_vec_get(oopnds, base_opnd_idx) != TF_OPND_INVALID) {
    if (tracing())
      fprintf(trace_file(), "%s: base updated, skipped.\n", fop->name());
    
    return NULL;
  }
  
  TOP updating_top = TI_TOP_Nonupdate_To_Update(fop->top());
  if (updating_top != TOP_UNDEFINED) {
    if (tracing())
      fprintf(trace_file(), "%s: updating opcode already set to %s, skipped.\n",
	      fop->name(), TI_TOP_Name(updating_top));

    return NULL;
  }

  INT vaddr_opnd_idx = tfex->op_dfg_find_output_by_name(op_dfg, "VAddr");
  if (vaddr_opnd_idx < 0) {
    if (tracing())
      fprintf(trace_file(), "%s: can't find VAddr operand, skipped.\n", fop->name());
    
    return NULL;
  }

  tf_opnd_t vaddr_opnd = tfex->opnd_vec_get(oopnds, vaddr_opnd_idx);
  Is_True(vaddr_opnd != TF_OPND_INVALID,
	  ("Can't find VAddr operand at index %d", vaddr_opnd_idx));

  if (tfex->opnd_bit_size(vaddr_opnd) != tfex->opnd_bit_size(base_opnd)) {
    if (tracing())
      fprintf(trace_file(), "%s: VAddr and base bit sizes don't match (%d != %d)\n",
	      fop->name(), tfex->opnd_bit_size(vaddr_opnd),
	      tfex->opnd_bit_size(base_opnd));
    
    return NULL;
  }
  
  const ISA_OPERAND_INFO *opnd_info = fop->isa_operand_info();
  const ISA_OPERAND_VALTYP *base_opnd_valtyp = TI_ISA_Op_Operand(opnd_info, base_idx);
  Is_True(TI_ISA_Valtyp_Is_Register(base_opnd_valtyp),
	  ("Expected register base operand for %s", fop->name()));
  
  for (INT ridx = 0; ridx < TI_ISA_Op_Results(opnd_info); ridx++) {
    const ISA_OPERAND_VALTYP *res_opnd_valtyp = TI_ISA_Op_Result(opnd_info, ridx);
    if (!TI_ISA_Valtyp_Is_Register(res_opnd_valtyp))
      continue;
    
    if (compatible_valtyps(base_opnd_valtyp, res_opnd_valtyp)) {
      if (tracing())
	fprintf(trace_file(), "%s: writes to the same regfile as the base, skipped.\n",
		fop->name());
      
      return NULL;
    }
  }
  
  /* 'fop' is a non-updating load/store. Try to create its updating version
     and match it with the other opcodes. */

  if (tracing())
    fprintf(trace_file(), "%s: creating updating version...\n", fop->name());

  {
    XT_MEMPOOL::mark mark(xt_init_pool());
    tf_op_dfg_t update_op_dfg = tfex->op_dfg_copy((tf_pool_t)xt_init_pool(), op_dfg);
    tf_opnd_vec_t update_oopnds = tfex->op_dfg_outputs(update_op_dfg);

    tf_opnd_t vaddr_opnd = tfex->opnd_vec_get(update_oopnds, vaddr_opnd_idx);
    Is_True(vaddr_opnd != TF_OPND_INVALID,
	    ("Can't find VAddr operand at index %d", vaddr_opnd_idx));
    
    tf_node_t addr_node = tfex->copy_reroute(vaddr_opnd);
    
    /* The copy of the base input operand is used as an output although
       its name may be suffixed by "_in" (e.g. "ars_in" instead of "ars_out").
       This shouldn't be a problem because when we match the DFGs we use the
       operand names only for operands that are not registers or states. */
    tf_opnd_t base_opnd_copy = tfex->opnd_copy((tf_pool_t)xt_init_pool(), base_opnd);
    tfex->node_add_output(addr_node, base_opnd_copy);
    
    tfex->opnd_vec_set(update_oopnds, base_opnd_idx, base_opnd_copy);
    
    tf_dfg_t update_dfg = tfex->op_dfg_dfg(update_op_dfg);
    tfex->opt_prune_copies(update_dfg);
    tfex->match_canonicalize(update_dfg, NULL);
    
    FUSION_OP *updating_match = find_updating_match(fop, update_op_dfg);
    if (updating_match)
      return updating_match;
  }

  return NULL;
}


void
CG_FUSION::init_updating_ops (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nIdentifying (non-)updating load/store opcode pairs:\n");
  }
  
  for (INT i = 0; i < _all_ops->Elements(); i++) {
    FUSION_OP *fop = _all_ops->Get(i);
    FUSION_OP *update_fop = find_updating(fop);
    if (update_fop) {
      if (tracing()) {
	fprintf(trace_file(), "$$$ Updating opcode pair identified: [%s, %s]\n",
		fop->name(), update_fop->name());
      }
      
      TI_TOP_Set_Nonupdate_To_Update(fop->top(), update_fop->top());
    }
  }
}


void
CG_FUSION::init_fusion_ops (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nIdentifying fusion opcodes:\n");
  }
    
  for (INT i = 0; i < _all_ops->Elements(); i++) {
    FUSION_OP *fop = _all_ops->Get(i);
    if (tfex->op_dfg_input_count(fop->op_dfg()) == 0 &&
	tfex->op_dfg_output_count(fop->op_dfg()) == 0)
      continue;

    if (!fop->valid_reference_functions()) {
      continue;
    }

    if (fop->check_fusion()) {
      add_fusion_op(fop);
      continue;
    }
    
    if (!fop->maybe_fusion()) {
      continue;
    }
    
    /* 'fop' maybe a fusion opcode with inlined sub-opcodes. Prepare it for
       sub-opcode matching. */
    fop->split_input_interfaces();
    
    for (INT j = 0; j < _all_ops->Elements(); j++) {
      if (i == j)
        continue;
      
      FUSION_OP *sub_fop = _all_ops->Get(j);
      if (tfex->op_dfg_input_count(sub_fop->op_dfg()) == 0 &&
	  tfex->op_dfg_output_count(sub_fop->op_dfg()) == 0)
	continue;
      
      if (!sub_fop->maybe_inlined_in_fusion())
	continue;

      sub_fop->split_input_interfaces();
    
      if (tracing())
	fprintf(trace_file(), "Matching sub-opcode %s in opcode %s...\n",
		sub_fop->name(), fop->name());
	    
      tie_match_cback_user()->set_fusion_op(fop);
      tie_match_cback_user()->set_sub_fusion_op(sub_fop);
	    
      tf_match_map_t match_map =
	tfex->match_find_match((tf_pool_t)xt_opt_pool(), sub_fop->dfg(), fop->dfg(),
				  /* match_io */ 0, tie_match_cback());
      
      // Reset the callbacks for debugging purposes...
      tie_match_cback_user()->set_sub_fusion_op(NULL);
      tie_match_cback_user()->set_fusion_op(NULL);
	    
      if (match_map == TF_MATCH_MAP_INVALID)
	continue;
	    
      if (tracing())
	fprintf(trace_file(), "Replacing sub-opcode %s in opcode %s...\n",
		sub_fop->name(), fop->name());
	    
      FUSION_OP *replace_fop = fop;
	    
      // Next time in we'll use 'fop's copy because we'll mess up the original now.
      // We do that because 'match_map' uses the original 'fop' pointers.
      fop = fop->clone_op_dfg(fusion_pool());
      _all_ops->Set(i, fop);
      _all_op_map->Remove(fop->top());
      _all_op_map->Enter(fop->top(), fop);
	    
      replace_matched_tie_nodes(sub_fop, replace_fop->dfg(), match_map);
      if (replace_fop->check_fusion()) {
	replace_fop->init_node_count();
	add_fusion_op(replace_fop);
      }
    }
  }
    
  // Prioritize the fusions to increase the chances of finding an optimal cover.
  sort_fusion_ops(_fusion_ops);
    
  if (tracing())
    fprintf(trace_file(), "%d total fusion opcode candidates.\n",
	    _fusion_ops->Elements());
}


SIGNAL_VEC *
CG_FUSION::ordered_signals (xtie_opcode xop, xtie_opcode xop_rewrite,
                            bool use_parse_body)
{
  const char *op_name = xtie_opcode_get_name(xop);

  xtie_iclass xiclass = xtie_opcode_get_iclass(xop_rewrite);
  if (!xiclass)
    return NULL;

  xtie_reference xref = xtie_opcode_get_reference(xop_rewrite);
  if (!xref)
    return NULL;

  xtie_operation_arg_iter op_arg_iter = NULL;
  if (use_parse_body) {
    xtie_operation xoper = xtie_opcode_get_operation(xop);
    if (xoper && xtie_operation_get_statements(xoper))
      op_arg_iter = xtie_operation_get_arg_iter(xoper);
  }
  
  xtie_signals xsigs = xtie_reference_get_signals(TI_TIE_Xtie_Post_Rewrite(), xref);
  Is_True(xsigs, ("Unable to obtain reference signals '%s'", op_name));
  
  SIGNAL_VEC *sigs = CXX_NEW(SIGNAL_VEC(fusion_pool()), fusion_pool());
  
  /* We ignore the decode signals. If they are used in the reference,
     the DFG builder will fail so we'll skip the opcode. */
  
  xtie_iclass_foreach_operand(xiclass, opnd)
    {
      const char *body_name = NULL;
      if (op_arg_iter)
      {
        xtie_operation_arg arg = xtie_operation_arg_iter_get(op_arg_iter);
        body_name = xtie_operation_arg_get_name(arg);
        op_arg_iter = xtie_operation_arg_iter_step(op_arg_iter);
      }
      add_iclass_arg_signal(fusion_pool(), sigs, xsigs, opnd, body_name);
    }
  end_xtie_iclass_foreach_operand;
  
  xtie_iclass_foreach_state(xiclass, st)
    {
      add_iclass_arg_signal(fusion_pool(), sigs, xsigs, st, NULL);
    }
  end_xtie_iclass_foreach_state;

  xtie_iclass_foreach_interface(xiclass, intf)
    {
      add_iclass_arg_signal(fusion_pool(), sigs, xsigs, intf, NULL);
    }
  end_xtie_iclass_foreach_interface;
  
  xtie_iclass_foreach_exception(xiclass, exc)
    {
      add_iclass_arg_signal(fusion_pool(), sigs, xsigs, exc, NULL);
    }
  end_xtie_iclass_foreach_exception;

  return sigs;
}


void
CG_FUSION::init_imaps (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nInitializing all imaps:\n");
  }

  // use a different node canonicalizer that can handle the user node
  // properly during the imap initialization
  bb_match_cback()->canonical_order_nodes = op_dfg_canonical_order_nodes;

  xtie_phase xp_parse = TI_TIE_Xtie_Compiler();
  xtie_foreach_imap(xp_parse,x)
    {
	const char* imap_name = xtie_imap_get_name(x);
	xtie_imap_pattern out_pattern = xtie_imap_get_impl(x);
	int num_out_operations = xtie_imap_pattern_num_codes(out_pattern);
	if (num_out_operations != 1) {
	  if (tracing()) {
	    fprintf(trace_file(), "skip %s: num operation != 1\n", imap_name);
	  }
	}

	// build op dfg for the imap target
	tf_op_dfg_t op_dfg = build_op_dfg(xt_init_pool(), x);
	if (op_dfg == TF_OP_DFG_INVALID) {
	  if (tracing()) {
	    fprintf(trace_file(), "skip %s: op_dfg failed\n", imap_name);
	  }
	  continue;
	}
	xtie_imap_pattern_code fusion_output =
		xtie_imap_pattern_get_code(out_pattern, 0);
	const char* fusion_top_name = xtie_imap_pattern_code_get_name(fusion_output);
	TOP fusion_top = TI_TOP_Topcode(fusion_top_name);

	// build fusion op based on the op dfg
	FUSION_OP *fusion_op =
	  CXX_NEW(FUSION_OP(this, fusion_top, op_dfg, NULL, NULL), fusion_pool());

	/* Raise the WIRE operands to PACKED if possible. */

	tfex->opt_convert_edges_to_packed(fusion_op->dfg());

	_fusion_ops->AddElement(fusion_op);
    
	if (tracing()) {
	  fprintf(trace_file(), "\nIdentified fusion:\n");
	  fusion_op->print(trace_file());
	}
    }
  end_xtie_foreach_imap;

  // Prioritize the fusions to increase the chances of finding an optimal cover.
  sort_fusion_ops(_fusion_ops);
    
  if (tracing())
    fprintf(trace_file(), "%d total fusion opcode candidates.\n",
	    _fusion_ops->Elements());

   // restore the node canonicalizer
   bb_match_cback()->canonical_order_nodes = bb_match_canonical_order_nodes;

}

void
CG_FUSION::init_all_ops (void)
{
  if (tracing()) {
    fprintf(trace_file(), "\nInitializing all opcodes:\n");
  }
  
  // Collect the DFGs of all opcodes.
  xtie_foreach_opcode(TI_TIE_Xtie_Post_Rewrite(), xop)
    {
      if (!xtie_opcode_is_instruction(xop))
        continue;
      
      const char *op_name = xtie_opcode_get_name(xop);
      if (tracing())
        fprintf(trace_file(), "%s: initializing...\n", op_name);

      TOP top = TI_TOP_Topcode(op_name);

      /* Sometimes user TIE fails to initialize so there's no guarantee that
         the topcode is there. */
      if (top == TOP_UNDEFINED) {
        if (tracing())
          fprintf(trace_file(), "%s: unable to find topcode.\n", op_name);
        
        continue;
      }
      
      if (is_special_top(top)) {
        if (tracing())
          fprintf(trace_file(), "%s: special opcode ignored.\n", op_name);
        
        continue;
      }

      SIGNAL_VEC *osigs = NULL;
      xtie_signals xsigs = NULL;
      tf_op_dfg_t op_dfg = build_op_dfg(xt_init_pool(), xop, &xsigs, &osigs);
      if (op_dfg == TF_OP_DFG_INVALID) {
        continue;
      }

      FUSION_OP *fusion_op =
        CXX_NEW(FUSION_OP(this, top, op_dfg, xsigs, osigs), fusion_pool());
      _all_ops->AddElement(fusion_op);
      _all_op_map->Enter(top, fusion_op);
      
      if (tracing()) {
        fprintf(trace_file(), "%s: DFG collected.\n", op_name);
        fusion_op->print(trace_file(), 2);
      }
    }
  end_xtie_foreach_opcode;
  
  // Prioritize the fusions to increase the chances of finding an optimal cover.
  sort_fusion_ops(_all_ops);
  
  if (tracing())
    fprintf(trace_file(), "%d total opcode DFGs.\n",
	    _all_ops->Elements());
}


void
CG_FUSION::init_reference_functions (void)
{
  if (!TI_TIE_Need_Post_Rewrite_TIE())
    return;

  if (tracing()) {
    fprintf(trace_file(), "\nInitializing reference functions:\n");
  }
  
  _reference_function_map = CXX_NEW(STRSET_MAP(127, fusion_pool()), fusion_pool());

  xtie_foreach_function(TI_TIE_Xtie_Post_Rewrite(), xf)
    {
      const char *func_name = xtie_function_get_name(xf);
      if (strncmp(func_name, AT_REFERENCE_PREFIX, AT_REFERENCE_PREFIX_LEN))
        continue;
      
      if (tracing()) {
        fprintf(trace_file(), "%s: looking for function DFG...\n", func_name);
      }
      
      tf_op_dfg_t func_op_dfg = _function_dfg_map->Find(func_name);
      if (func_op_dfg == TF_OP_DFG_INVALID)
        continue;
      
      if (tracing()) {
        fprintf(trace_file(), "%s: looking for opcode...\n", func_name);
      }
      
      TOP top = call_id_top(func_name);
      if (top == TOP_UNDEFINED)
        continue;
      
      if (tracing()) {
        fprintf(trace_file(), "%s: looking for reference DFG...\n", func_name);
      }
      
      FUSION_OP *op = _all_op_map->Find(top);
      if (!op)
        continue;
      
      if (!reference_functions_match(op, xf) &&
	  strcmp(op->name(), "extui") && // BP chagned core TIE for these so
	  strcmp(op->name(), "slli")) {  // disable warning for BV XPRESS TIE
        ErrMsg (EC_Reference_Changed, op->name(), func_name);
      } else {
        _reference_function_map->Enter(func_name, true);
      }
    }
  end_xtie_foreach_function;
}


bool
CG_FUSION::reference_functions_match (FUSION_OP *op, xtie_function xfunc)
{
  const char *func_name = xtie_function_get_name(xfunc);
  
  if (tracing()) {
    fprintf(trace_file(), "%s: matching reference functions DFGs...\n", func_name);
  }

  tf_op_dfg_t func_op_dfg = _function_dfg_map->Find(func_name);
  tf_dfg_t func_dfg = tfex->op_dfg_dfg(func_op_dfg);
  tf_dfg_t op_dfg = op->dfg();
  
  /* If the op's reference is a simple call to 'xfunc', then assume they
     are equivalent. */
  bool single_call = true;
  bool single_call_found = false;
  for (tf_node_t node = tfex->dfg_first_node(op_dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    tf_node_kind_t node_kind = tfex->node_kind(node);
    if (node_kind == TFN_COPY)
      continue;
    
    if (node_kind != TFN_CALL) {
      single_call = false;
      break;
    }
    
    const char *call_id = tfex->node_call_id(node);
    if (strcmp(call_id, func_name)) {
      single_call = false;
      break;
    }
    
    if (single_call_found) {
      single_call = false;
      break;
    }

    single_call_found = true;
  }

  if (single_call && single_call_found) {
    if (tracing()) {
      fprintf(trace_file(), "%s: matched OK (opcode is a function call).\n",
              func_name);
    }
    return true;
  }
  
  int func_node_count = tfex->dfg_node_count(func_dfg);
  int op_node_count = tfex->dfg_node_count(op_dfg);
  if (func_node_count != op_node_count &&
      func_node_count != op_node_count + 1) {
    if (tracing()) {
      fprintf(trace_file(), "%s: mismatched node counts (function %d, reference %d).\n",
              func_name, func_node_count, op_node_count);
    }

    return false;
  }
  
  XT_MEMPOOL::mark mark(xt_init_pool());
  
  /* The opcode reference DFG should be a subgraph in the function
     DFG. The only difference may be a single COPY node for the function
     result. */
  
  tf_match_map_t match_map =
    tfex->match_find_match((tf_pool_t)xt_init_pool(), op_dfg, func_dfg,
                           /* match_io */ 1, NULL);
  if (match_map == TF_MATCH_MAP_INVALID) {
    if (tracing()) {
      fprintf(trace_file(), "%s: unable to match.\n", func_name);
    }
    return false;
  }
   
  bool unmatched_node = false;
  tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
  for (tf_node_t func_node = tfex->dfg_first_node(func_dfg);
       func_node != TF_NODE_INVALID; func_node = tfex->dfg_next_node(func_node)) {
    tf_node_t op_node = TF_NODE_INVALID;
    if (tfex->node_node_map_find(rev_node_map, func_node, &op_node)) {
      Is_True(op_node != TF_NODE_INVALID, ("Bad match map."));

      /* For TFN_TABLE nodes, check if the table names match. */
      if (tfex->node_kind(func_node) == TFN_TABLE) {
        Is_True(tfex->node_kind(op_node) == TFN_TABLE, ("Bad match."));
        tf_opnd_t op_opnd_tbl = tfex->node_input(op_node, 0);
        tf_opnd_t func_opnd_tbl = tfex->node_input(func_node, 0);
        const char *op_tbl_name = tfex->opnd_name(op_opnd_tbl);
        const char *func_tbl_name = tfex->opnd_name(func_opnd_tbl);
        Is_True(op_tbl_name && func_tbl_name, ("NULL table name in TFN_TABLE node."));
        if (strcmp(op_tbl_name, func_tbl_name)) {
          if (tracing()) {
            fprintf(trace_file(), "%s: table mismatch (%s != %s).\n", func_name,
                    op_tbl_name, func_tbl_name);
          }
          return false;
        }
      }
      
      continue;
    }
    
    if (unmatched_node) {
      if (tracing()) {
        fprintf(trace_file(), "%s: more than 1 unmatched node.\n", func_name);
      }
      return false;
    }
    
    unmatched_node = true;
    if (tfex->node_kind(func_node) != TFN_COPY) {
      if (tracing()) {
        fprintf(trace_file(), "%s: unmatched function node is not a COPY.\n", func_name);
      }
      return false;
    }
  }
  
  if (tracing()) {
    fprintf(trace_file(), "%s: matched OK.\n", func_name);
  }

  return true;
}


void
CG_FUSION::replace_matched_tie_nodes (FUSION_OP *fusion_op, tf_dfg_t dfg,
				      tf_match_map_t match_map)
{
  tf_op_dfg_t sub_op_dfg = fusion_op->op_dfg();
  tf_node_t sub_node = tfex->match_replace_sub_dfg(sub_op_dfg, match_map);
  Is_True(sub_node != TF_NODE_INVALID &&
	  tfex->node_kind(sub_node) == TFN_USER &&
	  tfex->node_user(sub_node) == sub_op_dfg,
	  ("Unexpected result from replace_sub_dfg"));
  
  tfex->node_set_kind(sub_node, TFN_CALL);
  tfex->node_set_call_id(sub_node,
			 xt_pool_strcat((XT_MEMPOOL *)tfex->dfg_pool(dfg),
                                        AT_REFERENCE_PREFIX, fusion_op->name()));
  tfex->node_set_user(sub_node, NULL);
}


tf_node_t
CG_FUSION::replace_matched_bb_nodes (OP *fop, OPS *fops, FUSION_OP *fusion_op,
				     tf_match_map_t match_map)
{
  tf_op_dfg_t sub_op_dfg = fusion_op->op_dfg();
  tf_node_t sub_node = tfex->match_replace_sub_dfg(sub_op_dfg, match_map);
  Is_True(sub_node != TF_NODE_INVALID &&
	  tfex->node_kind(sub_node) == TFN_USER &&
	  tfex->node_user(sub_node) == sub_op_dfg,
	  ("Unexpected result from replace_sub_dfg"));
  
  tfex->node_set_user(sub_node, fop);

  /* Associate all ops in the fusion sequence with the new op. We must associate
     all of them because subsequent fusions may need to move ops around this fusion
     so they need to know the dependences. We could create additional nodes for the
     extra moves in 'fops' but this shouldn't be necessary. */
  Is_True(_bb_op_node_map != NULL, ("NULL BB op node map"));
  {
    OP *op;
    FOR_ALL_OPS_OPs(fops, op) {
      Is_True(op == fop || OP_copy(op),
	      ("Expected fusion or copy op, not %s", TI_TOP_Name(OP_code(op))));
      BB_OP_MAP_Set(_bb_op_node_map, op, sub_node);
    }
  }

  return sub_node;
}


void
CG_FUSION::add_fusion_op (FUSION_OP *fusion_op)
{

  for (int i=_fusion_ops->Lastidx(); i>=0; i--) {
    FUSION_OP* fop = _fusion_ops->Get(i);

    /* if another fusion op with the same TOP exists,
       e.g., from imap, then do not add this one */
    if (fop->top() == fusion_op->top())
      return;
  }

  /* Since CG does not expose the interfaces, we hide them so it's easier to match. */
  fusion_op->hide_interfaces();
    
  /* Raise the WIRE operands to PACKED if possible. */
  tfex->opt_convert_edges_to_packed(fusion_op->dfg());

  /* Convert calls to the node kinds corresponding to their TOPs, if possible. */
  fusion_op->calls_to_node_kinds();

  tfex->opt_copy_propagate_call_args(fusion_op->dfg());

  _fusion_ops->AddElement(fusion_op);
    
  if (tracing()) {
    fprintf(trace_file(), "\nIdentified fusion:\n");
    fusion_op->print(trace_file());
  }
}


tf_op_dfg_t 
CG_FUSION::build_op_dfg (XT_MEMPOOL *pool, xtie_opcode xop,
                         xtie_signals *xsigs, SIGNAL_VEC **osigs)
{
  const char *op_name = xtie_opcode_get_name(xop);
  
  xtie_phase post_rewrite = TI_TIE_Xtie_Post_Rewrite();
  xtie_opcode xop_rewrite = xtie_find_opcode(post_rewrite, op_name);
  Is_True(xop_rewrite, ("Unable to find '%s' in the post-rewrite TIE", op_name));

  /* Use the reference from the rewrite TIE. */
  xtie_reference xref_rewrite = xtie_opcode_get_reference(xop_rewrite);
  if (!xref_rewrite) {
    if (tracing())
      fprintf(trace_file(), "%s: no reference available.\n", op_name);
    
    return TF_OP_DFG_INVALID;
  }
  
  bool use_parse_body = true;
  xtie_operation xoper = xtie_opcode_get_operation(xop);
  xtie_xml_item xml_body = xoper ? xtie_operation_get_statements(xoper) : NULL;

  /* Backward compatibility.  Use the post-rewrite reference body. */
  xml_body = xtie_reference_get_statements(xref_rewrite);
  use_parse_body = false;
  
  if (!xml_body) {
    if (tracing())
      fprintf(trace_file(), "%s: no XML body...\n", op_name);
    
    return TF_OP_DFG_INVALID;
  }

  SIGNAL_VEC *_osigs = ordered_signals(xop, xop_rewrite, use_parse_body);
  if (!_osigs) {
    if (tracing())
      fprintf(trace_file(), "%s: unable to obtain ordered signals.\n", op_name);
    
    return TF_OP_DFG_INVALID;
  }
  
  xtie_signals _xsigs = xtie_reference_get_signals(post_rewrite, xref_rewrite);
  if (!_xsigs) {
    if (tracing())
      fprintf(trace_file(), "%s: no reference signals available.\n", op_name);
    
    return TF_OP_DFG_INVALID;
  }
  
  /* We want to handle exceptions only for updating loads and stores.
     So don't parse or build the DFG for any other ops with exceptions. */
  if (!xtie_signals_find_signal(_xsigs, "VAddr") &&
      xtie_signals_num_exceptions(_xsigs) > 0) {
    if (tracing())
      fprintf(trace_file(), "%s: non-memory opcode uses exceptions, ignored.\n",
	      op_name);
    
    return TF_OP_DFG_INVALID;
  }
  
  if (xtie_signals_num_kills(_xsigs) > 0) {
    if (tracing())
      fprintf(trace_file(), "%s: opcode uses kills, ignored.\n",
	      op_name);
    
    return TF_OP_DFG_INVALID;
  }
  
  // Parse the opcode and build a DFG.
  if (tracing())
    fprintf(trace_file(), "%s: parsing reference...\n", op_name);

  tf_sems_t sems = tfex->sems_default();

  tf_sem_tree_t op_io = tfex->sems_new_io(sems);
  for (INT i = 0; i < _osigs->Elements(); i++) {
    OP_SIGNAL *osig = _osigs->Get(i);
    xtie_signal xsig = osig->xsig();
    tf_sem_dir_t dir = TF_SEM_DIR_UNKNOWN;
    switch (xtie_signal_get_dir(xsig))
    {
    case XTIE_DIR_IN:
      dir = TF_SEM_DIR_IN;
      break;
      
    case XTIE_DIR_OUT:
      dir = TF_SEM_DIR_OUT;
      break;

    case XTIE_DIR_INOUT:
      dir = TF_SEM_DIR_INOUT;
      break;

    default:
      Is_True(0, ("Unexpected signal direction"));
      break;
    }
      
    tf_sem_kind_t skind = TFS_UNKNOWN;
    switch (xtie_signal_get_kind(xsig))
    {
    case XTIE_SIG_EXCEPTION:
      skind = TFS_EXCEPTION;
      break;
          
    case XTIE_SIG_FIELD:
    case XTIE_SIG_OPERAND:
      skind = TFS_OPERAND;
      break;

    case XTIE_SIG_INTERFACE:
      skind = TFS_INTERFACE;
      break;
      
    case XTIE_SIG_STATE:
      skind = TFS_STATE;
      break;
      
    default:
      Is_True(0, ("Unexpected signal kind"));
      break;
    }

    const char *sname = osig->body_name();
    tf_sem_tree_t sig = tfex->sems_new_signal(sems, skind, sname,
                                              xtie_signal_get_from_index(xsig),
                                              xtie_signal_get_to_index(xsig),
                                              dir);
    tfex->sem_tree_append_child(op_io, sig);
    if (tracing())
      fprintf(trace_file(), "  %s %s [%d:%d] (%s)\n",
              tfex->sem_kind_str(skind),
              sname,
              xtie_signal_get_from_index(xsig),
              xtie_signal_get_to_index(xsig),
              tfex->sem_dir_str(dir));
  }
  
  tf_sem_tree_t op_body = xml2sem_fn(sems, xml_body);
  tf_sem_tree_t op_semt = tfex->sems_new_semantic(sems, op_name, op_io, op_body);
  
  if (tracing())
    fprintf(trace_file(), "%s: building DFG...\n", op_name);
  
  tf_op_dfg_t tf_op_dfg = tfex->sem2dfg((tf_pool_t)pool, sems, op_semt);
  
  if (tf_op_dfg == TF_OP_DFG_INVALID) {
    if (tracing()) {
      fprintf(trace_file(), "%s: can't build the DFG.\n", op_name);
    }
    
    return TF_OP_DFG_INVALID;
  }
  
  tf_dfg_t dfg = tfex->op_dfg_dfg(tf_op_dfg);
  tfex->match_canonicalize(dfg, NULL);
  
  if (osigs)
    *osigs = _osigs;
  
  if (xsigs)
    *xsigs = _xsigs;
    
  return tf_op_dfg;
}    


void
CG_FUSION::add_rtor_op (TOP rtor_top, FUSION_OP *rtor_op)
{
  _rtor_op_map->Enter(rtor_top, rtor_op);
}


bool
CG_FUSION::inline_all_functions (tf_dfg_t dfg)
{
  Is_True(dfg != TF_DFG_INVALID, ("Invalid input DFG."));

  bool done = false;
  while (!done) {
    done = true;
    for (tf_node_t node = tfex->dfg_first_node(dfg);
	 node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
      if (tfex->node_kind(node) != TFN_CALL)
	continue;

      const char *call_id = tfex->node_call_id(node);
      tf_op_dfg_t func_op_dfg = _function_dfg_map->Find(call_id);
      if (func_op_dfg == TF_OP_DFG_INVALID)
	return false;
	    
      tfex->inline_dfg(node, func_op_dfg);
      done = false;
      break;
    }
  }

  return true;
}


bool
CG_FUSION::add_dependence_edges (BB *bb, tf_dfg_t dfg, BB_OP_MAP op_node_map)
{

  bool is_innermost = BB_innermost(bb);
  is_innermost = false;

  CG_DEP_Compute_Graph(bb,
		       NO_ASSIGNED_REG_DEPS,
		       is_innermost? CYCLIC : NON_CYCLIC,
		       INCLUDE_MEMREAD_ARCS,
		       INCLUDE_MEMIN_ARCS,
		       NO_CONTROL_ARCS,
		       NULL);
    
  OP *op;
  FOR_ALL_BB_OPs(bb, op) {
    tf_node_t source_node = (tf_node_t )BB_OP_MAP_Get(op_node_map, op);
    if (source_node == TF_NODE_INVALID)
      continue;
	
    for (ARC_LIST *arcs = OP_succs(op); arcs; arcs = ARC_LIST_rest(arcs)) {
      ARC *arc = ARC_LIST_first(arcs);
      CG_DEP_KIND kind = ARC_kind(arc);
      int omega = ARC_omega(arc);

      if (kind == CG_DEP_REGIN && omega == 0) {
        // non-cyclic true dependences are represented through the data-flow edges.
	continue;
      }

      OP *succ_op = ARC_succ(arc);
      tf_node_t sink_node = (tf_node_t )BB_OP_MAP_Get(op_node_map, succ_op);
      if (sink_node == TF_NODE_INVALID)
	continue;
	    
      tf_edge_t edge = tfex->dfg_new_dep_edge(dfg,
			     tfex->node_index(source_node),
			     tfex->node_index(sink_node));

      tf_dep_kind_t dep_kind = TFD_UNKNOWN;
      switch (kind) {
	case CG_DEP_REGIN: dep_kind = TFD_REG_IN; break;
	case CG_DEP_REGOUT: dep_kind = TFD_REG_OUT; break;
	case CG_DEP_REGANTI: dep_kind = TFD_REG_ANTI; break;

	case CG_DEP_MEMIN: dep_kind = TFD_MEM_IN; break;
	case CG_DEP_MEMOUT: dep_kind = TFD_MEM_OUT; break;
	case CG_DEP_MEMANTI: dep_kind = TFD_MEM_ANTI; break;

	default: dep_kind = TFD_MISC;
      }

      tfex->edge_set_dep_kind(edge, dep_kind);
      tfex->edge_set_omega(edge, omega);

    }
  }
    
  CG_DEP_Delete_Graph(bb);
    
  return true;
}


static void
add_loop_carried_dep( tf_dfg_t dfg, OP* pred, OP* succ,
		      int omega, BB_OP_MAP op_node_map,
		      tf_dep_kind_t dep_kind) {

  if (!dfg || !pred || !succ)
    return;

  tf_node_t source_node = (tf_node_t)BB_OP_MAP_Get(op_node_map, pred);
  tf_node_t sink_node = (tf_node_t)BB_OP_MAP_Get(op_node_map, succ);

  tf_edge_idx_dlist_t preds = tfex->node_preds(sink_node);
  for (tf_edge_idx_dlist_iter_t piter = tfex->edge_idx_dlist_iter_first(preds);
	 piter != TF_EDGE_IDX_DLIST_ITER_INVALID;
	 piter = tfex->edge_idx_dlist_iter_next(piter)) {

      tf_edge_idx_t in_eidx = tfex->edge_idx_dlist_iter_elem(piter);
      tf_edge_t in_edge = tfex->dfg_edge(dfg, in_eidx);

      if (tfex->edge_dep_kind(in_edge)==dep_kind &&
	  tfex->edge_omega(in_edge)==omega &&
	  tfex->edge_source_node(in_edge)==source_node)

      return;
  }

  tf_edge_t edge = tfex->dfg_new_dep_edge(dfg,
				tfex->node_index(source_node),
				tfex->node_index(sink_node));
  tfex->edge_set_dep_kind(edge, dep_kind);
  tfex->edge_set_omega(edge, omega);
}

tf_op_dfg_t 
CG_FUSION::build_op_dfg (XT_MEMPOOL *xt_pool, xtie_imap imap)
{

  const char* imap_name = xtie_imap_get_name(imap);
  if (tracing()) {
    fprintf(trace_file(), "imap %s:\n", imap_name);
  }

  // currently allows only single target (fusion) instruction
  int num_args=xtie_imap_num_args(imap);
  xtie_imap_pattern out_pattern = xtie_imap_get_impl(imap);
  int num_out_operations = xtie_imap_pattern_num_codes(out_pattern);
  if (num_out_operations != 1) {
    if (tracing()) {
      fprintf(trace_file(), "  skip: num output operation != 1\n", imap_name);
    }
    return NULL;
  }

  xtie_imap_pattern_code fusion_output =
		xtie_imap_pattern_get_code(out_pattern, 0);
  const char* fusion_top_name = xtie_imap_pattern_code_get_name(fusion_output);
  TOP fusion_top = TI_TOP_Topcode(fusion_top_name);

  xtie_imap_pattern in_pattern = xtie_imap_get_pattern(imap);
  int num_instructions=xtie_imap_pattern_num_codes(in_pattern);

  tf_dfg_t dfg = tfex->dfg_init((tf_pool_t)xt_dfg_pool());
  tfex->dfg_set_cback(dfg, op_dfg_cback());

  MEM_POOL_Popper popper(local_pool());
  hTN_MAP tn_node_map = hTN_MAP_Create(local_pool());

  // record the copy node of the most recent def node for a given name
  STRINT_MAP* operand_name_map =
	CXX_NEW(STRINT_MAP(127, local_pool()), local_pool());
  // record the input operand for state
  STRINT_MAP* livein_state_name_map =
	CXX_NEW(STRINT_MAP(127, local_pool()), local_pool());
  // record the last output operand for state
  STRINT_MAP* liveout_state_name_map =
	CXX_NEW(STRINT_MAP(127, local_pool()), local_pool());

  // record the input and output operand for impa arguments
  tf_opnd_t* input = CXX_NEW_ARRAY(tf_opnd_t, num_args, local_pool());
  tf_opnd_t* output = CXX_NEW_ARRAY(tf_opnd_t, num_args, local_pool());

  int i;

  // initialize input arguments
  // create a copy node for each input argument
  for (i=0; i<num_args; i++) {

    xtie_imap_arg arg = xtie_imap_get_arg(imap,i);
    xtie_dir dir = xtie_imap_arg_get_dir(arg);
    const char* arg_name = xtie_imap_arg_get_name(arg);
    const char* arg_type_name = xtie_imap_arg_get_type_name(arg);

    input[i] = NULL;
    output[i] = NULL;
    if (dir!=XTIE_DIR_IN && dir!=XTIE_DIR_INOUT)
      continue;

    int bit_size = 32; // for immediate operand    

    // TODO: we should add an interface to libti to get this information,
    // and then convert these to use that interface
      
    xtensa_isa isa = TI_TIE_Libisa_Info();
    xtensa_regfile rf = xtensa_regfile_lookup(isa, arg_type_name);
    if (rf != XTENSA_UNDEFINED) {
      bit_size = xtensa_regfile_num_bits(isa, rf);
    }

    tf_node_t op_node = tfex->dfg_new_node(dfg, TFN_COPY);
    operand_name_map->Enter(arg_name,(unsigned int)op_node);

    tf_opnd_t opnd = tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
    tfex->opnd_set_name(opnd, arg_name, /* copy */ 0);
    tfex->node_add_input(op_node, opnd);

    input[i] = opnd;

    opnd = tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
    tfex->opnd_set_name(opnd, arg_name, /* copy */ 0);
    tfex->node_add_output(op_node, opnd);
  }

  xtensa_isa isa_info = TI_TIE_Libisa_Info();
  int num_livein_states=0;
  int num_liveout_states=0;

  bool ok = true;
  for (i=0; i<num_instructions; i++) {

      xtie_imap_pattern_code code = xtie_imap_pattern_get_code(in_pattern,i);
      const char* opcode_name = xtie_imap_pattern_code_get_name(code);

      // handle copy pseudo node
      if (!strcmp(opcode_name,"=")) {

	xtie_imap_pattern_code_arg in_arg =
		xtie_imap_pattern_code_get_arg(code,1);
	const char* in_name = xtie_imap_pattern_code_arg_get_name(in_arg);
	xtie_imap_pattern_code_arg out_arg =
		xtie_imap_pattern_code_get_arg(code,0);
	const char* out_name = xtie_imap_pattern_code_arg_get_name(out_arg);
	tf_node_t def_node = (tf_node_t)operand_name_map->Find(in_name);

	int bit_size;
	if (!def_node) {
	      // first copy of a literal constant

	      def_node = tfex->dfg_new_node(dfg, TFN_COPY);

	      const char *binary = xt_const_to_binary_string(xt_dfg_pool(), in_name, 0);
	      bit_size = strlen(binary);

	      // Setup the input COPY operand.
	      tf_opnd_t in = tfex->node_new_operand(def_node, TFO_WIRE, bit_size, 1);
	      tf_opnd_idx_t in_idx = tfex->node_add_input(def_node, in);
	      tfex->opnd_set_user(in, NULL);
	      tfex->opnd_set_name(in, in_name, /* copy */ 1);
	      for (int pos = 0; pos < bit_size; pos++) {
		// Careful with LSB/MSB.
		char digit = binary[bit_size - pos - 1];
		tf_edge_idx_t eidx =
		  ((digit == '0') ? TF_EDGE_IDX_0 :
		   (digit == '1') ? TF_EDGE_IDX_1 : TF_EDGE_IDX_X);
		tfex->opnd_set_edge_idx(in, pos, eidx);
	      }

	      tf_opnd_t const_opnd =
		tfex->node_new_operand(def_node, TFO_PACKED, bit_size, 1);
	      tfex->opnd_set_user(const_opnd, NULL);
	      tfex->opnd_set_name(const_opnd, in_name, /* copy */ 1);
	      tf_opnd_idx_t const_oidx = tfex->node_add_output(def_node, const_opnd);

	}

	// materialize the copy

	tf_opnd_t def_out = tfex->node_output(def_node,0);
	bit_size = tfex->opnd_bit_size(def_out);

	tf_node_t copy_node = tfex->dfg_new_node(dfg, TFN_COPY);
	tfex->node_set_user(copy_node,(void*)NULL);
	tf_opnd_t copy_in = tfex->node_new_operand(copy_node, TFO_PACKED, bit_size, 1);
	tfex->opnd_set_name(copy_in, in_name, /* copy */ 0);
	tfex->node_add_input(copy_node, copy_in);

	// check if we are the first use,
	// if we are not the first use then need to add new output
	// operand
	tf_opnd_idx_t oidx = 0;
	if (!tfex->opnd_is_unknown(def_out)) {
	  tf_opnd_t def_out =
		tfex->node_new_operand(def_node, TFO_PACKED, bit_size, 1);
	  tfex->opnd_set_name(def_out, in_name, /* copy */ 0);
	  tfex->opnd_set_user(def_out, NULL);
	  oidx = tfex->node_add_output(def_node, def_out);
	}

	// connect the def output to the copy input
	tfex->dfg_new_data_edge(dfg,
		tfex->node_index(def_node), oidx, 0,
		tfex->node_index(copy_node), 0, 0);

	// make the copy the last def of the output name
	tf_opnd_t copy_out = tfex->node_new_operand(copy_node, TFO_PACKED, bit_size, 1);
	tfex->opnd_set_name(copy_out, out_name, /* copy */ 0);
	tfex->node_add_output(copy_node, copy_out);
	operand_name_map->Enter(out_name,(unsigned int)copy_node);

	int k;

	// update the last def of output arguments
	for (k=0; k<num_args; k++) {

	  xtie_imap_arg arg = xtie_imap_get_arg(imap,k);
	  xtie_dir dir = xtie_imap_arg_get_dir(arg);
	  const char* arg_name = xtie_imap_arg_get_name(arg);
	  if (!strcmp(out_name, arg_name)) {
	    output[k] = copy_out;
	    break;
	  }
	}

	continue;
      }

      TOP top = TI_TOP_Topcode(opcode_name);

      if (top != TOP_UNDEFINED) {

        int num_operands = xtie_imap_pattern_code_num_args(code);
        xtensa_opcode xopc = xtensa_opcode_lookup(isa_info, opcode_name);

	// create a user node in op dfg and store the TOP as the user info
	tf_node_t op_node = tfex->dfg_new_node(dfg, TFN_USER);
	tfex->node_set_user(op_node,(void*)top);

	// for each operation in the input pattern, we process
	// 1. the input operands
	// 2. the output operands
	// 3. the input states
	// 4. the output states

        int num_op_operands=0;
        int num_op_results=0;

        const ISA_OPERAND_INFO *operands_info = TI_ISA_Operand_Info (top);

        int code_opnd_index = 0;
        const ISA_OPERAND_VALTYP *operand_value_type;
	// loop for input operands
        while (code_opnd_index<num_operands) {

          char inout;

          int isa_opnd_index = code_opnd_index;
          inout = xtensa_operand_inout(isa_info, xopc, isa_opnd_index);

	  xtie_imap_pattern_code_arg code_arg =
		xtie_imap_pattern_code_get_arg(code,code_opnd_index);
	  const char* opnd_name =
		xtie_imap_pattern_code_arg_get_name(code_arg);

          if (inout == 'i' || inout == 'm') {

            operand_value_type =
                TI_ISA_Op_Operand (operands_info, num_op_operands);
	    int bit_size = TI_ISA_Valtyp_Size(operand_value_type);
	    tf_node_t def_node = (tf_node_t)operand_name_map->Find(opnd_name);
	    const char *binary = NULL;
	    if (!def_node) {
	      // if no def_node then it is a literal and the size
	      // deppends on the literal
	      binary = xt_const_to_binary_string(xt_dfg_pool(), opnd_name, 0);
	      bit_size = strlen(binary);
	    } else if (TI_ISA_Valtyp_Is_Literal(operand_value_type)) {
	      // found a use of an input immediate operand, need to fixup size
	      bit_size = 32;	// immediate is always 32 bit wide
	    }

	    // we re-route the input to a temp copy node then connect
	    // the def node to the temp copy node
	    // the extra copies will be cleaned up near the end
	    tf_opnd_t in_opnd =
		tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
	    tfex->opnd_set_user(in_opnd, NULL);
	    tf_opnd_idx_t opnd_idx = tfex->node_add_input(op_node, in_opnd);

	    tf_node_t tmp_node = tfex->copy_in_reroute(op_node, opnd_idx);
	    in_opnd = tfex->node_input(op_node,opnd_idx);
	    tf_opnd_t tmp_in_opnd = tfex->node_input(tmp_node,0);
	    tf_opnd_t tmp_out_opnd = tfex->node_output(tmp_node,0);


	    if (def_node) {

	      tfex->opnd_set_name(tmp_in_opnd, opnd_name, /* copy */ 0);
	      tfex->opnd_set_name(tmp_out_opnd, opnd_name, /* copy */ 0);
	      tfex->opnd_set_name(in_opnd, opnd_name, /* copy */ 0);

	      // check if we are the first use,
	      // if we are not the first use then need to add new output
	      // operand
	      tf_opnd_idx_t oidx = 0;
	      if (!tfex->opnd_is_unknown(tfex->node_output(def_node,0))) {
		tf_opnd_t nopnd =
			tfex->node_new_operand(def_node,
					       TFO_PACKED, bit_size, 1);
		tfex->opnd_set_name(nopnd, opnd_name, /* copy */ 0);
		tfex->opnd_set_user(nopnd, NULL);
		oidx = tfex->node_add_output(def_node, nopnd);
	      }
	      tfex->dfg_new_data_edge(dfg,
				      tfex->node_index(def_node), oidx, 0,
				      tfex->node_index(tmp_node), 0, 0);

	    } else {

	      // this is similar to copy from a literal constant

	      // not an argument, has to be a literal
	      def_node = tfex->dfg_new_node(dfg, TFN_COPY);

	      // Setup the input COPY operand.
	      tf_opnd_t in = tfex->node_new_operand(def_node, TFO_WIRE, bit_size, 1);
	      tf_opnd_idx_t in_idx = tfex->node_add_input(def_node, in);
	      tfex->opnd_set_user(in, NULL);
	      tfex->opnd_set_name(in, opnd_name, /* copy */ 1);
	      //tfex->opnd_init_edge_idxs(in,TF_EDGE_IDX_X);
	      for (int pos = 0; pos < bit_size; pos++) {
		// Careful with LSB/MSB.
		char digit = binary[bit_size - pos - 1];
		tf_edge_idx_t eidx =
		  ((digit == '0') ? TF_EDGE_IDX_0 :
		   (digit == '1') ? TF_EDGE_IDX_1 : TF_EDGE_IDX_X);
		tfex->opnd_set_edge_idx(in, pos, eidx);
	      }

	      tfex->opnd_set_name(tmp_in_opnd, opnd_name, /* copy */ 1);
	      tfex->opnd_set_name(tmp_out_opnd, opnd_name, /* copy */ 1);
	      tfex->opnd_set_name(in_opnd, opnd_name, /* copy */ 1);

	      tf_opnd_t const_opnd =
		tfex->node_new_operand(def_node, TFO_PACKED, bit_size, 1);
	      tfex->opnd_set_user(const_opnd, NULL);
	      tfex->opnd_set_name(const_opnd, opnd_name, /* copy */ 1);
	      tf_opnd_idx_t const_oidx =
		tfex->node_add_output(def_node, const_opnd);

	      tfex->dfg_new_data_edge(dfg,
				      tfex->node_index(def_node), const_oidx, 0,
				      tfex->node_index(tmp_node), 0, 0);

	    }

            num_op_operands++;
          }

          code_opnd_index+= 1;
        }

        code_opnd_index = 0;
	// loop for output operands
        while (code_opnd_index<num_operands) {

          char inout;

          int isa_opnd_index = code_opnd_index;
          inout = xtensa_operand_inout(isa_info, xopc, isa_opnd_index);

	  xtie_imap_pattern_code_arg code_arg =
		xtie_imap_pattern_code_get_arg(code,code_opnd_index);
	  const char* opnd_name =
		xtie_imap_pattern_code_arg_get_name(code_arg);

          if (inout == 'o' || inout == 'm') {
            operand_value_type =
                TI_ISA_Op_Result (operands_info, num_op_results);
	    int bit_size = TI_ISA_Valtyp_Size(operand_value_type);

	    tf_opnd_t out_opnd =
		tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
	    tfex->opnd_set_user(out_opnd, NULL);
	    tf_opnd_idx_t opnd_idx = tfex->node_add_output(op_node, out_opnd);

	    // we re-route the output to a temp copy node
	    // the copy node becomes the new def node
	    // the extra copies will be cleaned up near the end
	    tf_node_t tmp_node = tfex->copy_out_reroute(op_node, opnd_idx);

	    out_opnd = tfex->node_output(op_node,opnd_idx);
	    tfex->opnd_set_name(out_opnd, opnd_name, /* copy */ 0);

	    tf_opnd_t in_opnd = tfex->node_input(tmp_node,0);
	    tfex->opnd_set_user(in_opnd, NULL);
	    tfex->opnd_set_name(in_opnd, opnd_name, /* copy */ 0);

	    tf_opnd_t copy_out_opnd = tfex->node_output(tmp_node,0);
	    tfex->opnd_set_user(copy_out_opnd, NULL);
	    tfex->opnd_set_name(copy_out_opnd, opnd_name, /* copy */ 0);

	    int k;
	    for (k=0; k<num_args; k++) {

	      xtie_imap_arg arg = xtie_imap_get_arg(imap,k);
	      xtie_dir dir = xtie_imap_arg_get_dir(arg);
	      const char* arg_name = xtie_imap_arg_get_name(arg);
	      if (!strcmp(opnd_name, arg_name)) {
		output[k] = copy_out_opnd;
		break;
	      }
	    }

	    // update with the new def
	    operand_name_map->Remove(opnd_name);
	    operand_name_map->Enter(opnd_name, (unsigned int)tmp_node);

            num_op_results++;
          }

          code_opnd_index+= 1;
        }

        /* now handle the input state(s) */
	int isa_state_cnt = 0;
        while (num_op_operands<TI_ISA_Op_Operands(operands_info)) {
          operand_value_type =
                TI_ISA_Op_Operand (operands_info, num_op_operands);
          FmtAssert(TI_ISA_Valtyp_Is_Register(operand_value_type),
                ("Missing state operand"));

	  // we have to get the bit size info from libisa
	  while (xtensa_stateOperand_inout(isa_info, xopc, isa_state_cnt)=='o')
	    isa_state_cnt++;
	  xtensa_state isa_state =
		xtensa_stateOperand_state(isa_info, xopc, isa_state_cnt);
	  isa_state_cnt++;

	  int bit_size = xtensa_state_num_bits(isa_info, isa_state);
	  const char* opnd_name = xtensa_state_name(isa_info, isa_state);

	  tf_opnd_t in_opnd =
		tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
	  tfex->opnd_set_user(in_opnd, NULL);
	  tf_opnd_idx_t opnd_idx = tfex->node_add_input(op_node, in_opnd);

	  tf_node_t tmp_node = tfex->copy_in_reroute(op_node, opnd_idx);
	  tf_opnd_t out_opnd = tfex->node_output(tmp_node,0);

	  tfex->opnd_set_name(tfex->node_input(op_node, opnd_idx),
				opnd_name, /* copy */ 0);
	  tfex->opnd_set_name(tfex->node_input(tmp_node, 0),
				opnd_name, /* copy */ 0);
	  tfex->opnd_set_name(tfex->node_output(tmp_node, 0),
				opnd_name, /* copy */ 0);

	  tf_node_t def_node = (tf_node_t)operand_name_map->Find(opnd_name);
	  if (!def_node) {

	    // create a def copy for the first use of a live-in state
	    tf_node_t def_node = tfex->copy_in_reroute(tmp_node, 0);
	    operand_name_map->Enter(opnd_name, (unsigned int)def_node);
	    tf_opnd_t livein_state_opnd = tfex->node_input(def_node,0);
	    livein_state_name_map->Enter(opnd_name,
					 (unsigned int)livein_state_opnd);
	    num_livein_states++;
	  } else {

	    // check if we are the first use,
	    // if we are not the first use then need to add new output
	    // operand
	    tf_opnd_idx_t oidx = 0;
	    if (!tfex->opnd_is_unknown(tfex->node_output(def_node,0))) {
	      tf_opnd_t nopnd = tfex->node_new_operand(def_node,
					       TFO_PACKED, bit_size, 1);
	      tfex->opnd_set_name(nopnd, opnd_name, /* copy */ 0);
	      tfex->opnd_set_user(nopnd, NULL);
	      oidx = tfex->node_add_output(def_node, nopnd);
	    }
	    tfex->dfg_new_data_edge(dfg,
				    tfex->node_index(def_node), oidx, 0,
				    tfex->node_index(tmp_node), 0, 0);

	  }

          num_op_operands++;
        }
	isa_state_cnt = 0;
        /* now handle the result state(s) */
        while (num_op_results<TI_ISA_Op_Results(operands_info)) {
          operand_value_type =
                TI_ISA_Op_Result (operands_info, num_op_results);
          FmtAssert(TI_ISA_Valtyp_Is_Register(operand_value_type),
                ("Missing state result"));

	  // we have to get the bit size info from libisa
	  while (xtensa_stateOperand_inout(isa_info, xopc, isa_state_cnt)=='i')
	    isa_state_cnt++;
	  xtensa_state isa_state =
		xtensa_stateOperand_state(isa_info, xopc, isa_state_cnt);
	  isa_state_cnt++;

	  int bit_size = xtensa_state_num_bits(isa_info, isa_state);
	  const char* opnd_name = xtensa_state_name(isa_info, isa_state);

	  tf_opnd_t out_opnd =
		tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
	  tf_opnd_idx_t opnd_idx = tfex->node_add_output(op_node, out_opnd);
	  tfex->opnd_set_user(out_opnd, NULL);

	  tf_node_t tmp_node = tfex->copy_out_reroute(op_node, opnd_idx);

	  tfex->opnd_set_name(tfex->node_output(op_node, opnd_idx),
				opnd_name, /* copy */ 0);
	  tfex->opnd_set_name(tfex->node_input(tmp_node, 0),
				opnd_name, /* copy */ 0);
	  tfex->opnd_set_name(tfex->node_output(tmp_node, 0),
				opnd_name, /* copy */ 0);

	  if (!operand_name_map->Find(opnd_name)) {
	    // increment the number of live-out state from 1st encounter
	    num_liveout_states++;
	  } else {
	    // remove the old def opnd
	    operand_name_map->Remove(opnd_name);
	    liveout_state_name_map->Remove(opnd_name);
	  }
	  // update the current def and the live-out state map
	  operand_name_map->Enter(opnd_name, (unsigned int)tmp_node);
	  tf_opnd_t maybe_liveout_state_opnd = tfex->node_output(tmp_node,0);
	  liveout_state_name_map->Enter(opnd_name,
					(unsigned int)maybe_liveout_state_opnd);

          num_op_results++;
        }
      } else {

	// unknown opcode, stop
	ok=false;
	break;
      }
  }

  if (!ok) {
    if (tracing()) {
      fprintf(trace_file(), "%s: DFG failed.\n", fusion_top_name);
    }
    return NULL;
  } else {

    if (tracing()) {
      fprintf(trace_file(), "%s: New DFG collected.\n", fusion_top_name);
      tfex->dfg_print(dfg, trace_file(), 4);
    }
  }

  xtensa_opcode f_xopc = xtensa_opcode_lookup(isa_info, fusion_top_name);
  int num_states = xtensa_opcode_num_stateOperands(isa_info, f_xopc);

  // create the op dfg and fill out the input/output args and states
  tf_op_dfg_t op_dfg =
	tfex->op_dfg_init ((tf_pool_t)xt_pool, dfg, num_args+num_states);

  tf_opnd_vec_t inputs = tfex->op_dfg_inputs(op_dfg);
  tf_opnd_vec_t outputs = tfex->op_dfg_outputs(op_dfg);
  int num_inputs = 0;
  int num_outputs = 0;
  int io_index = 0;
  for (i=0; i<num_args; i++) {

    xtie_imap_arg arg = xtie_imap_get_arg(imap,i);
    xtie_dir dir = xtie_imap_arg_get_dir(arg);

    if (dir==XTIE_DIR_IN || dir==XTIE_DIR_INOUT) {
      tfex->opnd_vec_set(inputs, io_index, input[i]);
    }
    if (dir==XTIE_DIR_OUT || dir==XTIE_DIR_INOUT) {
      tfex->opnd_vec_set(outputs, io_index, output[i]);
    }
    io_index++;
  }

  tf_opnd_t opnd;
  for (i=0; i<num_states; i++) {
    xtensa_state isa_state = xtensa_stateOperand_state(isa_info, f_xopc, i);
    const char* isa_state_name = xtensa_state_name(isa_info, isa_state);
    const char inout = xtensa_stateOperand_inout(isa_info, f_xopc, i);
    if (inout=='i' || inout=='m') {
      opnd = (tf_opnd_t)livein_state_name_map->Find(isa_state_name);
      tfex->opnd_vec_set(inputs, io_index, opnd);
    }
    if (inout=='o' || inout=='m') {
      opnd = (tf_opnd_t)liveout_state_name_map->Find(isa_state_name);
      // if the liveout state opnd were consumed, we need to
      // create an additional one so it is an unknown edge
      if (!tfex->opnd_is_unknown(opnd)) {
        tf_node_t node = tfex->opnd_node(opnd);
        tf_opnd_t new_opnd =
		tfex->node_new_operand(node, tfex->opnd_kind(opnd),
				       tfex->opnd_bit_size(opnd), 1);
     
        tfex->opnd_set_user(new_opnd, tfex->opnd_user(opnd));
        tfex->opnd_set_name(new_opnd, tfex->opnd_name(opnd), 0);
        tfex->node_add_output(node, new_opnd);
	opnd = new_opnd;
      }
      tfex->opnd_vec_set(outputs, io_index, opnd);
    }
    io_index++;
  }

  // clean up
  tfex->opt_convert_edges_to_wires(dfg);
  tfex->opt_prune_copies(dfg);
  tfex->dfg_compact_nodes_and_edges(dfg);

  // the outer loop in init_imaps has installed proper node canonicalizer 
  // in bb_match_cback()
  tfex->match_canonicalize(dfg, bb_match_cback());

  if (tracing()) {
    fprintf(trace_file(), "%s: New OP_DFG collected.\n", fusion_top_name);
    tfex->op_dfg_print(op_dfg, trace_file(), 4);
  }

  return op_dfg;

}


tf_dfg_t 
CG_FUSION::build_dfg (XT_MEMPOOL *xt_pool, BB *bb, bool use_reg_size,
		      bool cyclic)
{
  Is_True(xt_pool != xt_dfg_pool(), ("Bad pool."));
    
  XT_MEMPOOL::mark mark(xt_dfg_pool());
  MEM_POOL_Popper popper(local_pool());

  bool is_single_bb_loop = false;
  if (bb && BB_prev(bb) && BB_next(bb) &&
     BB_succs_len(BB_prev(bb))==1 &&
     BB_succs_len(bb)==2 &&
     BB_in_succs(bb,bb) && BB_in_succs(bb,BB_next(bb))) {
 
    is_single_bb_loop = true;
    
  }

  tf_dfg_t dfg = tfex->dfg_init((tf_pool_t)xt_dfg_pool());
  BB_OP_MAP op_node_map = BB_OP_MAP_Create(bb, local_pool());
  hTN_MAP tn_node_map = hTN_MAP_Create(local_pool());

  hTN_MAP first_def_tn_op_map = hTN_MAP_Create(local_pool());
  hTN_MAP last_def_tn_op_map = hTN_MAP_Create(local_pool());
    
  OP *op;
  FOR_ALL_BB_OPs(bb, op) {
    TOP top = OP_code(op);

    if (OP_is_load_const(op)) continue; // treat it as TOP_load_const
    
    if (OP_simulated(op)) {
      switch (top) {
      case TOP_loop_end:
	continue;
        
      case TOP_load_const:
	continue;
        
      case TOP_spadjust:
        if (!OP_next(op) || !OP_prev(op))
          continue;
        
        /* fall through */
      default:
        Lmt_DevWarn(1, ("No support for simulated op %s", TI_TOP_Name(top)));
        return TF_DFG_INVALID;
      }
    }
    
    tf_node_kind_t node_kind = CG_FUSION_Top_To_Node_Kind(top);
    if (node_kind == TFN_UNKNOWN)
      node_kind = TFN_USER;
    
    tf_node_t op_node = tfex->dfg_new_node(dfg, node_kind);
    
    if (node_kind != TFN_COPY)
      tfex->node_set_user(op_node, op);
    
    BB_OP_MAP_Set(op_node_map, op, op_node);
	
    /* Setup the data inputs */
    for (INT i = 0; i < OP_opnds(op); i++) {
      TN *tn = OP_opnd(op, i);

      INT bit_size = tn_bit_size(tn, use_reg_size);
      if (bit_size < 0) {
	Lmt_DevWarn(1, ("Bad operand [%d] TN bit size for op %s", i, TI_TOP_Name(top)));
	return TF_DFG_INVALID;
      }

      const char *tn_name = tn_opnd_name((XT_MEMPOOL *)tfex->dfg_pool(dfg), tn);

      tf_opnd_t opnd = tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
      tfex->opnd_set_name(opnd, tn_name, /* copy */ 0);
      tfex->opnd_set_user(opnd, tn);
      tf_opnd_idx_t opnd_idx = tfex->node_add_input(op_node, opnd);

      INT64 val = 0;
      if (TN_Value_At_Op(tn, op, &val)) {
	XT_BIGNUM *xt_val = XT_New(xt_dfg_pool()) XT_BIGNUM(bit_size, val);

	tf_node_t tn_node = tfex->dfg_new_node(dfg, TFN_CONST);
	tfex->node_set_const_val(tn_node, (tf_const_val_t)xt_val);
	tfex->opnd_set_name(opnd, tn_name, /* copy */ 0);

	tf_opnd_t const_opnd = tfex->node_new_operand(tn_node, TFO_PACKED, bit_size, 1);
	tfex->opnd_set_user(const_opnd, tn);
	tfex->opnd_set_name(const_opnd, tn_name, /* copy */ 0);
	tf_opnd_idx_t const_oidx = tfex->node_add_output(tn_node, const_opnd);
	
	tfex->dfg_new_data_edge(dfg,
				tfex->node_index(tn_node), const_oidx, 0,
				tfex->node_index(op_node), opnd_idx, 0);
      } else if (TN_is_register(tn)) {
	tf_node_t tmp_node = tfex->copy_in_reroute(op_node, opnd_idx);
        tf_opnd_t out_opnd = tfex->node_output(tmp_node,0);
        tfex->opnd_set_name(out_opnd, tn_name, /* copy */ 0);
        tf_opnd_t in_opnd = tfex->node_input(op_node,opnd_idx);
        tfex->opnd_set_name(in_opnd, tn_name, /* copy */ 0);
		
	// Check if we already have a node that defines this TN and if we do,
	// connect it to the new copy 'tmp_node'.
	tf_node_t tn_node = (tf_node_t )hTN_MAP_Get(tn_node_map, tn);
	if (tn_node != TF_NODE_INVALID) {
	  // We don't want to replace the unknown input of live-out TN's that
	  // are defined in this block.
	  tf_opnd_idx_t oidx = 0;
	  if (tfex->node_output_count(tn_node) > 1 ||
	      !tfex->opnd_is_unknown(tfex->node_output(tn_node, 0)) ||
	      TN_is_dedicated(tn) ||
	      is_live_out_def(op, tn)) {
	    tf_opnd_t nopnd = tfex->node_new_operand(tn_node, TFO_PACKED, bit_size, 1);
	    tfex->opnd_set_name(nopnd, tn_name, /* copy */ 0);
	    tfex->opnd_set_user(nopnd, tn);
	    oidx = tfex->node_add_output(tn_node, nopnd);
	  }

	  tfex->dfg_new_data_edge(dfg,
				  tfex->node_index(tn_node), oidx, 0,
				  tfex->node_index(tmp_node), 0, 0);
	} else {
	  hTN_MAP_Set(tn_node_map, tn, tmp_node);
	}
      } else {
	Is_True(TN_is_constant(tn), ("Unknown TN operand kind [%d] for op %s",
				     i, TI_TOP_Name(top)));
	// Treat all other TNs as unknown input operands.
      }
    }
	
    /* Setup the data outputs */
    for (INT i = 0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);

      // record defs for global TN
      if (TN_is_register(tn) && TN_is_global_reg(tn)) {
        OP* first_def = (OP*)hTN_MAP_Get(first_def_tn_op_map, tn);
        if (first_def==NULL)
          hTN_MAP_Set(first_def_tn_op_map, tn, op);
        hTN_MAP_Set(last_def_tn_op_map, tn, op);
      }

      INT bit_size = tn_bit_size(tn, use_reg_size);
      if (bit_size < 0) {
	Lmt_DevWarn(1, ("Bad result [%d] TN bit size for op %s", i, TI_TOP_Name(top)));
	return TF_DFG_INVALID;
      }
	    
      const char *tn_name = tn_opnd_name((XT_MEMPOOL *)tfex->dfg_pool(dfg), tn);
	    
      tf_opnd_t opnd = tfex->node_new_operand(op_node, TFO_PACKED, bit_size, 1);
      tfex->opnd_set_name(opnd, tn_name, /* copy */ 0);
      tfex->opnd_set_user(opnd, tn);
      tf_opnd_idx_t opnd_idx = tfex->node_add_output(op_node, opnd);
      tf_node_t tn_node = tfex->copy_out_reroute(op_node, opnd_idx);
      tf_opnd_t in_opnd = tfex->node_input(tn_node,0);
      tfex->opnd_set_name(in_opnd, tn_name, /* copy */ 0);
      tf_opnd_t out_opnd = tfex->node_output(op_node,opnd_idx);
      tfex->opnd_set_name(out_opnd, tn_name, /* copy */ 0);
      hTN_MAP_Set(tn_node_map, tn, tn_node);		
    }
  }

  // the following is turned off until loop-back edges is needed
  // in XPRES (e.g., unrolling)
  if (cyclic && is_single_bb_loop) {

    FOR_ALL_BB_OPs(bb, op) {
      tf_node_t op_node = (tf_node_t)BB_OP_MAP_Get(op_node_map, op);

      if (op_node==NULL)
        continue;

      for (INT i = 0; i < OP_opnds(op); i++) {
        TN *tn = OP_opnd(op, i);

	if (!TN_is_register(tn) || !TN_is_global_reg(tn))
	  continue;

        OP* first_def = (OP*)hTN_MAP_Get(first_def_tn_op_map, tn);
        OP* last_def = (OP*)hTN_MAP_Get(last_def_tn_op_map, tn);

	// handle loop-carried register flow-dependence
	// for upward-exposed uses
	if (last_def && OP_Order(last_def,op)>=0 &&
	    OP_Order(first_def,op)>=0) {
	  add_loop_carried_dep(dfg,last_def,op,1,op_node_map, TFD_REG_IN);
	}

	// handle loop-carried register anti-dependence
	// if the use is the source of cross-iteration
	// anti-dependence
	if (first_def && OP_Order(op,first_def)>=0 &&
	    OP_Order(op,last_def)>=0) {
	  add_loop_carried_dep(dfg,op,first_def,1,op_node_map, TFD_REG_ANTI);
	}
      }

      for (INT i = 0; i < OP_results(op); i++) {
        TN *tn = OP_result(op, i);

	if (!TN_is_register(tn) || !TN_is_global_reg(tn))
	  continue;

        OP* first_def = (OP*)hTN_MAP_Get(first_def_tn_op_map, tn);
        OP* last_def = (OP*)hTN_MAP_Get(last_def_tn_op_map, tn);

	// handle loop-carried register output-dependence
	if (op==first_def) {
	  add_loop_carried_dep(dfg,last_def,op,1,op_node_map, TFD_REG_OUT);
	}
      }

      // handle loop-carried memory dependence
      if (OP_memory(op)) {
	OP* src_op = op;
	BOOL definite;
	UINT8 omega;

	// look for sources of lexically backwards dependences
	while (src_op) {
	  if (OP_memory(src_op) && get_mem_dep(src_op,op,&definite,&omega)) {
	    add_loop_carried_dep(dfg,src_op,op,omega,op_node_map, TFD_MISC);
	  }
	  src_op = OP_next(src_op);
	}
      }
    }
  }

  if (!add_dependence_edges(bb, dfg, op_node_map))
    return TF_DFG_INVALID;

  Is_True(dfg != TF_DFG_INVALID, ("Invalid DFG."));
  
  tfex->opt_prune_copies(dfg);
  tfex->dfg_compact_nodes_and_edges(dfg);
  
  // Copy the data-flow graph into the provided pool.
  dfg = tfex->dfg_copy((tf_pool_t)xt_pool, dfg);
    
  return dfg;
}


OP *
CG_FUSION::build_fusion_op (FUSION_OP *fusion_op, tf_match_map_t match_map, OPS *ops)
{
  OPS copy_in_ops = OPS_EMPTY;
  OPS copy_out_ops = OPS_EMPTY;

  TOP top = fusion_op->top();

  const ISA_OPERAND_INFO *opnd_info = TI_ISA_Operand_Info(top);
  INT in_opnds = TI_ISA_Op_Operands(opnd_info);
  INT out_opnds = TI_ISA_Op_Results(opnd_info);
    
  TN **in_tns = CXX_NEW_ARRAY(TN *, in_opnds, local_pool());
  TN **out_tns = CXX_NEW_ARRAY(TN *, out_opnds, local_pool());
    
  tf_op_dfg_t op_dfg = fusion_op->op_dfg();
  tf_opnd_vec_t in_dfg_opnds = tfex->op_dfg_inputs(op_dfg);
  tf_opnd_vec_t out_dfg_opnds = tfex->op_dfg_outputs(op_dfg);
  INT in_size = tfex->opnd_vec_size(in_dfg_opnds);
  INT out_size = tfex->opnd_vec_size(out_dfg_opnds);

  tf_opnd_opnd_map_t opnd_map = tfex->match_map_opnd_map(match_map);

  // Setup the inputs.
  INT in_idx = 0;
  for (INT oidx = 0; oidx < in_size; oidx++) {
    tf_opnd_t tf_opnd = tfex->opnd_vec_get(in_dfg_opnds, oidx);
    if (tf_opnd == TF_OPND_INVALID)
      continue;
	
    tf_opnd_t bb_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, tf_opnd, &bb_opnd);
    Is_True(bb_opnd != TF_OPND_INVALID, ("Can't find a matching operand"));
	
    TN *tn = (TN *)tfex->opnd_user(bb_opnd);
    Is_True(tn != NULL, ("Null TN in operand"));
	
    Is_True(compatible_valtyp_tn(TI_ISA_Op_Operand(opnd_info, in_idx), tn),
	    ("Expected compatible input operands [%d]", in_idx));

    // Check if we need to generate inout copy-in ops.
    tf_opnd_t out_tf_opnd = tfex->opnd_vec_get(out_dfg_opnds, oidx);
    if (out_tf_opnd != TF_OPND_INVALID) {
      tf_opnd_t out_bb_opnd = TF_OPND_INVALID;
      tfex->opnd_opnd_map_find(opnd_map, out_tf_opnd, &out_bb_opnd);
      Is_True(out_bb_opnd != TF_OPND_INVALID, ("Can't find a matching output operand"));
	    
      TN *out_tn = (TN *)tfex->opnd_user(out_bb_opnd);
      Is_True(out_tn != NULL, ("Null TN in operand"));
	    
      // The input and the output TNs are different. Create a new TN
      // and add copy-in, copy-out ops.
      if (out_tn != tn) {
	TN *new_tn = Build_TN_Like(tn);
	Exp_COPY(new_tn, tn, &copy_in_ops);
	Exp_COPY(out_tn, new_tn, &copy_out_ops);
	tn = new_tn;
      }
    }
	
    in_tns[in_idx++] = tn;
  }
  Is_True(in_idx == in_opnds,
	  ("Input operand counts mismatch (%d != %d)", in_idx, in_opnds));
    
  // Setup the outputs.
  INT out_idx = 0;
  for (INT oidx = 0; oidx < out_size; oidx++) {
    tf_opnd_t tf_opnd = tfex->opnd_vec_get(out_dfg_opnds, oidx);
    if (tf_opnd == TF_OPND_INVALID)
      continue;
	
    tf_opnd_t bb_opnd = TF_OPND_INVALID;
    tfex->opnd_opnd_map_find(opnd_map, tf_opnd, &bb_opnd);
    Is_True(bb_opnd != TF_OPND_INVALID, ("Can't find a matching operand"));
	
    TN *tn = (TN *)tfex->opnd_user(bb_opnd);
    Is_True(tn, ("Null TN in operand"));
	
    Is_True(compatible_valtyp_tn(TI_ISA_Op_Result(opnd_info, out_idx), tn),
	    ("Expected compatible output operands [%d]", out_idx));

    // Check it this is an inout operand and use the tn already allocated
    // when the inputs were processed.
    tf_opnd_t in_tf_opnd = tfex->opnd_vec_get(in_dfg_opnds, oidx);
    if (in_tf_opnd != TF_OPND_INVALID) {
      int in_opnd_idx = tfex->op_dfg_skip_null_find_input(op_dfg, in_tf_opnd);
      Is_True(in_opnd_idx >= 0 && in_opnd_idx < in_opnds,
	      ("Bad input operand index %d.", in_opnd_idx));

      tn = in_tns[in_opnd_idx];
      Is_True(tn, ("Null TN for inout operand."));
    }
	
    out_tns[out_idx++] = tn;
  }
  Is_True(out_idx == out_opnds,
	  ("Output operand counts mismatch (%d != %d)", out_idx, out_opnds));
    
  OP *op = Mk_VarOP(top, out_opnds, in_opnds, out_tns, in_tns);
  if (TI_ISA_Property_Set(PROP_move, OP_code(op)))
    Set_OP_copy(op);

  OPS_Append_Ops(ops, &copy_in_ops);
  OPS_Append_Op(ops, op);
  OPS_Append_Ops(ops, &copy_out_ops);
    
  if (tracing()) {
    fprintf(trace_file(), "Fusion sequence created: \n");
    Print_OPS(ops);
  }

  return op;
}


bool
CG_FUSION::replace_fused_ops (OP *fop, OPS *fops, tf_match_map_t match_map)
{
  if (!_bb_op_node_map) {
    _bb_op_node_map = create_op_node_map(local_pool(), _bb, _bb_dfg);
  }
    
  // Out of all matched ops, find the first and the last one in the BB op list.
  OP *first_op = NULL;
  OP *last_op = NULL;
  find_first_last_ops(match_map, first_op, last_op);
  Is_True(first_op, ("No first op (fusion %s).", TI_TOP_Name(OP_code(fop))));
  Is_True(last_op, ("No last op (fusion %s).", TI_TOP_Name(OP_code(fop))));
  Is_True(first_op != last_op,
	  ("Unexpected single op fusion %s.", TI_TOP_Name(OP_code(fop))));

  // Collect the set all of basic block's DFG nodes that have been matched.
  tf_node_set_t matched_nodes = tfex->node_set_init((tf_pool_t)xt_opt_pool(), 31);
  tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
  for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
    tf_node_t node = tfex->node_node_pair_key(node_pair);
    tfex->node_set_add_node(matched_nodes, node);

    OP *op = (OP *)tfex->node_user(node);
    if (!op)
      continue;

    /* FIXME: This is a bit hacky but... Whenever we apply fusion A with inout
       operands, we create copy-in, copy-out ops that are associated with the
       fusion node. If later another fusion B matched fusion A (and some other nodes),
       we may not be able to update the BB correctly because of the surrounding
       copy-in/copy-out code. So, check if the surrounding ops are mapped to the same BB
       DFG node. This is safe because in the same fusion pass the copy-in/copy-out code will
       be kept with the fusion.
       
       Note that if we end up in this case then something is wrong with the
       fusion matching because a bigger fusion didn't match earlier in the process. */
    OP *prev_op = OP_prev(op);
    if (prev_op && ((tf_node_t)BB_OP_MAP_Get(_bb_op_node_map, prev_op) == node)) {
      if (tracing()) {
	fprintf(trace_file(),
		"Op %s and predecessor %s mapped to the same DFG node, can't apply fusion '%s'.\n",
		TI_TOP_Name(OP_code(op)), TI_TOP_Name(OP_code(prev_op)),
                TI_TOP_Name(OP_code(fop)));
      }
      return false;
    }

    OP *next_op = OP_next(op);
    if (next_op && ((tf_node_t)BB_OP_MAP_Get(_bb_op_node_map, next_op) == node)) {
      if (tracing()) {
	fprintf(trace_file(),
		"Op %s and successor %s mapped to the same DFG node, can't apply fusion '%s'.\n",
		TI_TOP_Name(OP_code(op)), TI_TOP_Name(OP_code(next_op)),
                TI_TOP_Name(OP_code(fop)));
      }
      return false;
    }
  }

  // Check if we have a node for each op in the (first_op, last_op) range of the op list.
  // Also, check how many attached WNs we need to copy. If none or more than one, don't
  // copy any WNs.
  OP *op_wn = NULL;
  bool copy_wn = true;
  for (OP *op = first_op; op != OP_next(last_op); op = OP_next(op)) {
    Is_True(op, ("End of basic block reached before encountering the last op"));

    /* load_const simulated ops don't have nodes in the DFG. It should be safe
       to leave them before the fusion op. */
    if (OP_code(op) == TOP_load_const)
      continue;
    
    tf_node_t op_node = (tf_node_t )BB_OP_MAP_Get(_bb_op_node_map, op);
    if (!op_node) {
      if (tracing())
	fprintf(trace_file(),
		"Op %s has no node in the bb, can't apply fusion %s.\n",
		TI_TOP_Name(OP_code(op)), TI_TOP_Name(OP_code(fop)));
	    
      return false;
    }

    if (tfex->node_set_contains(matched_nodes, op_node)) {
      WN *wn = Get_WN_From_Memory_OP(op);
      if (wn) {
	if (op_wn) {
	  op_wn = NULL;
	  copy_wn = false;
	} else if (copy_wn) {
	  op_wn = op;
	}
      }
    }
  }
    
  // A backward pass to move any operations dependent on the fused ops after the last
  // fused op.
  OP *prev_op = NULL;
  for (OP *op = last_op; op != first_op; op = prev_op) {
    Is_True(op, ("Begin of basic block reached before encountering the first op"));
	
    // Get the next op now because we may move the current one and
    // mess up the iterator.
    prev_op = OP_prev(op);

    /* It should be safe to leave all load_const ops before the fusion. Dead
       code elimination should get rid of them. */
    if (OP_code(op) == TOP_load_const)
      continue;
    
    tf_node_t op_node = (tf_node_t )BB_OP_MAP_Get(_bb_op_node_map, op);
    Is_True(op_node, ("Expected to find node for %s", TI_TOP_Name(OP_code(op))));
	
    if (tfex->node_set_contains(matched_nodes, op_node))
      continue;
    
    tf_node_set_t preds = TF_NODE_SET_INVALID;
    tfex->node_node_set_map_find(_bb_dfg_preds, op_node, &preds);
    Is_True(preds != TF_NODE_SET_INVALID, ("Can't find node's predecessors"));
	
    if (tfex->node_set_intersects(matched_nodes, preds)) {
      /* Move 'op' after 'last_op'. We need to reset and set the mapped
	 node because the move routines mess up the op hash value. */
      BB_OP_MAP_Set(_bb_op_node_map, op, NULL);
      BB_Move_Op_After(_bb, last_op, _bb, op);
      BB_OP_MAP_Set(_bb_op_node_map, op, op_node);
    }
  }

  // Copy the attached WN for memory ops to preserve the alias info.
  if (copy_wn && op_wn && OP_memory(op_wn)) {
    Copy_WN_For_Memory_OP(fop, op_wn);
  }
    
  // Update the source line info.
  {
    OP *op;
    FOR_ALL_OPS_OPs(fops, op) {
      OP_srcpos(op) = OP_srcpos(last_op);
    }
  }
    
  // Insert the newly generated fusion sequence.
  BB_Insert_Ops_After(_bb, last_op, fops);
    
  // Remove all matched ops from the BB.
  for (tf_node_set_iter_t niter = tfex->node_set_iter_first(matched_nodes);
       niter != TF_NODE_SET_ITER_INVALID;
       niter = tfex->node_set_iter_next(niter)) {
    tf_node_t node = tfex->node_set_iter_elem(niter);
    OP *op = (OP *)tfex->node_user(node);
    if (op) {
      BB_OP_MAP_Set(_bb_op_node_map, op, NULL);
      BB_Remove_Op(_bb, op);
    }
  }
    
  if (tracing()) {
    fprintf(trace_file(), CG_FUSION_LINE);
    fprintf(trace_file(), "BB after applying %s\n", TI_TOP_Name(OP_code(fop)));
    fprintf(trace_file(), CG_FUSION_LINE);
    Print_BB(_bb);
  }

  return true;
}


bool
CG_FUSION::apply_fusion (FUSION_OP *fusion_op, tf_match_map_t match_map)
{
  if (tracing())
    fprintf(trace_file(), "%s: applying...\n", fusion_op->name());
    
  OPS fops = OPS_EMPTY;
  OP *fop = build_fusion_op(fusion_op, match_map, &fops);
  if (!replace_fused_ops(fop, &fops, match_map)) {
    DevWarn("Fusion %s matched but ops could not be replaced.", fusion_op->name());
    return false;
  }
  
  /* Recomputing the DFG predecessor sets is expensive for large BB's so we try
     to simply update them. Associate the new node with the combined predecessor
     set of the replaced nodes. */
  tf_node_set_t fnode_preds = tfex->node_set_init((tf_pool_t)xt_opt_pool(), 31);
  tf_node_set_t fused_nodes = tfex->node_set_init((tf_pool_t)xt_opt_pool(), 31);
  tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
  for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {

    tf_node_t node = tfex->node_node_pair_key(node_pair);
    tfex->node_set_add_node(fused_nodes, node);
    
    tf_node_set_t node_preds = TF_NODE_SET_INVALID;
    tfex->node_node_set_map_find(_bb_dfg_preds, node, &node_preds);
    Is_True(node_preds != TF_NODE_SET_INVALID, ("Can't find node's predecessors")); 
    
    tfex->node_set_add_set(fnode_preds, node_preds);
  }
  
  Is_True(_bb_dfg == tfex->match_map_dfg(match_map), ("Mismatched DFGs."));
  tf_node_t fnode = replace_matched_bb_nodes(fop, &fops, fusion_op, match_map);
  
  tfex->node_node_set_map_insert(_bb_dfg_preds, fnode, fnode_preds);
  
  for (tf_node_t node = tfex->dfg_first_node(_bb_dfg);
       node != TF_NODE_INVALID;
       node = tfex->dfg_next_node(node)) {
    tf_node_set_t node_preds = TF_NODE_SET_INVALID;
    tfex->node_node_set_map_find(_bb_dfg_preds, node, &node_preds);

    if (node_preds == TF_NODE_SET_INVALID) {
      /* Sometimes sub-graph replacement creates extra copy nodes. Manually update them. */
      Is_True(tfex->node_kind(node) == TFN_COPY,
              ("Expected a copy node, not %s.",
               tfex->node_kind_str(tfex->node_kind(node))));
      
      tf_node_set_t cnode_preds = tfex->node_set_init((tf_pool_t)xt_opt_pool(), 31);
      tfex->node_node_set_map_insert(_bb_dfg_preds, node, cnode_preds);
      
      tf_opnd_t opnd = tfex->node_input(node, 0);
      Is_True(tfex->opnd_kind(opnd) == TFO_PACKED, ("Expected a PACKED operand"));
      
      tf_edge_t edge = tfex->opnd_edge(opnd, 0);
      if (edge == TF_EDGE_INVALID)
        continue;
      
      tf_node_t snode = tfex->edge_source_node(edge);
      tf_node_set_t snode_preds = TF_NODE_SET_INVALID;
      tfex->node_node_set_map_find(_bb_dfg_preds, snode, &snode_preds);
      Is_True(snode_preds != TF_NODE_SET_INVALID, ("No predecessor set"));
      
      tfex->node_set_add_set(cnode_preds, snode_preds);
      tfex->node_set_add_node(cnode_preds, snode);
    } else if ((node != fnode) &&
               tfex->node_set_intersects(node_preds, fused_nodes)) {
      /* Update the predecessor sets of all nodes that depend on fused nodes.
         In their predecessor sets, include the newly created fusion node as
         well as its predecessors. */
      tfex->node_set_add_set(node_preds, fnode_preds);
      tfex->node_set_add_node(node_preds, fnode);
    }
  }
  
  return true;
}


FUSION_OP *
CG_FUSION::def_tn_vector_value (OP *op, TN *tn, INT64 &val)
{
  DEF_KIND kind;
  OP *def_op = NULL;
  FUSION_OP *fusion_op = NULL;
  while (1) {
    def_op = TN_Reaching_Value_At_Op(tn, op, &kind, true);
    if (!def_op || kind != VAL_KNOWN)
      return NULL;
    
    TOP top = OP_code(def_op);
    fusion_op = rtor_op_map()->Find(top);
    if (fusion_op)
      break;
    else if (OP_copy(def_op)) {
      tn = OP_opnd(def_op,0);
      continue;
    } else
      return NULL;
  }
  
  TOP top = OP_code(def_op);
  Is_True(OP_opnds(def_op) == 1,
          ("Expected single input operand for %s", TI_TOP_Name(top)));
  TN *scalar_tn = OP_opnd(def_op, 0);
  
  if (!TN_Value_At_Op(scalar_tn, def_op, &val))
    return NULL;

  return fusion_op;
}


FUSION_OP *
CG_FUSION::rematerializable_tn_vector_value (TN *tn, INT64 &val)
{
  if (!TN_is_rematerializable(tn))
    return NULL;
    
  TYPE_ID vec_type = TN_mtype(tn);
  WN *home = TN_home(tn);
  if (!home ||
      WN_operator(home) != OPR_CVT ||
      WN_rtype(home) != vec_type ||
      !MTYPE_is_integral(WN_desc(home)))
    return NULL;
    
  WN *intconst = WN_kid0(home);
  if (WN_operator(intconst) != OPR_INTCONST)
    return NULL;
  
  VECTOR_TYPE_INFO *vti = _vector_type_info->Find(vec_type);
  if (!vti ||
      !vti->rtor_fusion_op() ||
      vti->scalar_type() != WN_desc(home))
    return NULL;
  
  val = WN_const_val(intconst);
  return vti->rtor_fusion_op();
}


FUSION_OP *
CG_FUSION::tn_vector_value_at_op (OP *op, TN *tn, INT64 &val)
{
  // Try to find the vector operand constant value using the reaching definitions
  FUSION_OP *fop = def_tn_vector_value(op, tn, val);
  if (fop)
    return fop;

  // Try to find the vector operand constant value using any available home information. 
  fop = rematerializable_tn_vector_value(tn, val);
  if (fop)
    return fop;
    
  return NULL;
}


INT
CG_FUSION::tn_bit_size (TN *tn, bool use_reg_size)
{
  INT bit_size_unknown = -1;
    
  TYPE_ID mtype = TN_mtype(tn);
  if (mtype != MTYPE_UNKNOWN) {
    if (!use_reg_size)
      return MTYPE_bit_size(mtype);
	
    ISA_REGCLASS isa_rc = TI_ISA_Regclass_For_Mtype(mtype);
    if (isa_rc == ISA_REGCLASS_UNDEFINED)
      return bit_size_unknown;

    const ISA_REGCLASS_INFO *isa_rc_info = TI_ISA_Regclass_Info(isa_rc);
    Is_True(isa_rc_info, ("Null ISA_REGCLASS_INFO"));
	
    INT bit_size =
      TI_ISA_Regsize_For_Mtype(mtype) * TI_ISA_Regclass_Bit_Size(isa_rc_info);
    return bit_size;
  }
    
  if (!TN_is_register(tn))
    return bit_size_unknown;
    
  // Check for special or state registers.
  ISA_REGSUBCLASS subclass = ISA_REGSUBCLASS_UNDEFINED;
  if (TN_register_class(tn) == TI_ISA_Regclass_Special()) {
    subclass = TI_ISA_Regsubclass_Special_Reg_Num(TN_register(tn) - REGISTER_MIN);
  } else if (TI_ISA_Regclass_Is_State(TN_register_class(tn))) {
    subclass = TI_ISA_Regsubclass_State(REGISTER_name(TN_register_class(tn),
						      TN_register(tn)));
  }
    
  if (subclass == ISA_REGSUBCLASS_UNDEFINED)
    return bit_size_unknown;
  
  const char *state_name = TI_ISA_Regsubclass_Name(TI_ISA_Regsubclass_Info(subclass));
  if (state_name == NULL)
    return bit_size_unknown;

  return TI_ISA_State_Bit_Size(state_name);
}


OP *
CG_FUSION::tn_next_def (TN *tn, OP *op)
{
  for (op = OP_next(op); op; op = OP_next(op))
    if (OP_Defs_TN(op, tn))
      return op;

  return NULL;
}


bool
CG_FUSION::is_live_out_def (OP *op, TN *tn)
{
  // Return true if 'tn' is live out of 'op's basic block and 'op' is the last
  // defition of 'tn' in the basic block.
  return (GRA_LIVE_TN_Live_Outof_BB(tn, OP_bb(op)) && !tn_next_def(tn, op));
}


bool
CG_FUSION::set_vector_constant_operand (tf_opnd_t opnd, FUSION_OP *rtor, INT64 val)
{
  // Use the rtor conversion proto OP_DFG to determine the vector operand value
  // given the input scalar 'val'.
  
  FUSION_OP *rtor_fusion_op = rtor;
  Is_True(rtor_fusion_op, ("No rtor fusion op in VECTOR_TYPE_INFO"));
  
  // Make a copy of the OP_DFG so we can optimize it after plugging in
  // the input scalar 'val'.
  tf_op_dfg_t rtor_op_dfg =
    tfex->op_dfg_copy(tfex->opnd_pool(opnd), rtor_fusion_op->op_dfg());
  
  Is_True(tfex->op_dfg_input_count(rtor_op_dfg) == 1 &&
	  tfex->op_dfg_output_count(rtor_op_dfg) == 1,
	  ("Expected single-input/single-output converting proto %s",
	   rtor_fusion_op->name()));
    
  tf_opnd_t input_opnd = tfex->op_dfg_skip_null_input(rtor_op_dfg, 0);
  Is_True(input_opnd != TF_OPND_INVALID &&
	  tfex->opnd_kind(input_opnd) == TFO_WIRE &&
	  tfex->opnd_vector_length(input_opnd) == 1 &&
	  tfex->opnd_is_unknown(input_opnd), ("Bad input operand"));

  // Plug-in the input constant and optimize the DFG.
  INT ibits = tfex->opnd_bit_size(input_opnd);
  for (INT pos = 0; pos < ibits; pos++) {
    tf_edge_idx_t eidx = (val & ((INT64)1 << pos)) ? TF_EDGE_IDX_1 : TF_EDGE_IDX_0;
    tfex->opnd_set_edge_idx(input_opnd, pos, eidx);
  }

  tf_dfg_t rtor_dfg = tfex->op_dfg_dfg(rtor_op_dfg);
  tfex->opt_optimize(rtor_dfg);

  // After optimization we expect to see a single COPY node with an unknown output
  // and a costant input.

  if (tfex->dfg_node_count(rtor_dfg) != 1)
    return false;
  
  tf_opnd_t output_opnd = tfex->op_dfg_skip_null_output(rtor_op_dfg, 0);
  Is_True(output_opnd != TF_OPND_INVALID, ("Null output operand"));
    
  tf_node_t output_node = tfex->opnd_node(output_opnd);
  if (tfex->node_kind(output_node) != TFN_COPY)
    return false;
    
  tf_opnd_t const_input = tfex->node_input(output_node, 0);
  Is_True(const_input != TF_OPND_INVALID &&
	  !tfex->opnd_is_unknown(const_input),
	  ("Bad input operand of the result copy node"));
    
  Is_True(tfex->opnd_bit_size(const_input) == tfex->opnd_bit_size(opnd),
	  ("Bit sizes mismatch (%d != %d)",
	   tfex->opnd_bit_size(const_input), tfex->opnd_bit_size(opnd)));
    
  // Now we can replace 'opnd' with the calculated constant.
  tf_edge_t edge = tfex->opnd_edge(opnd, 0);
  if (edge != TF_EDGE_INVALID) {
    tf_opnd_t source_opnd = tfex->edge_source_opnd(edge);
    tfex->opnd_set_edge_idx(source_opnd, 0, TF_EDGE_IDX_X);
    tf_dfg_t dfg = tfex->edge_dfg(edge);
    tfex->dfg_delete_edge(dfg, edge);
  }
    
  INT bits = tfex->opnd_bit_size(opnd);
    
  // Convert the operand to a WIRE kind and initialize the bits to the
  // match the calculated constant.
  tfex->opnd_set_kind(opnd, TFO_WIRE);
  tfex->opnd_set_edge_idxs_count(opnd, bits, TF_EDGE_IDX_U);
  
  for (INT pos = 0; pos < bits; pos++) {
    tf_edge_idx_t eidx = tfex->opnd_edge_idx(const_input, pos);
    tfex->opnd_set_edge_idx(opnd, pos, eidx);
  }

  return true;
}


void
CG_FUSION::find_vector_constants (tf_dfg_t dfg)
{
  Is_True(_vector_type_info, ("Vector type info is not initialized"));

  // Save some time when we don't have any SIMD data.
  if (_vector_type_info->Num_Entries() == 0)
    return;
    
  // Traverse all nodes' input operands and for any vector ones, follow the
  // reaching definitions to determine if they have constant values.
  for (tf_node_t node = tfex->dfg_first_node(dfg);
       node != TF_NODE_INVALID; node = tfex->dfg_next_node(node)) {
    INT icount = tfex->node_input_count(node);
    for (tf_opnd_idx_t oidx = 0; oidx < icount; oidx++) {
      tf_opnd_t opnd = tfex->node_input(node, oidx);
      TN *tn = (TN *)tfex->opnd_user(opnd);
      if (!tn || !TN_is_register(tn))
	continue;
	    
      TYPE_ID mtype = TN_mtype(tn);
      VECTOR_TYPE_INFO *vti = _vector_type_info->Find(mtype);
      if (!vti)
	continue;
	    
      // Found a vector operand. Try to determine if it has a constant value.
      if (tfex->opnd_kind(opnd) != TFO_PACKED)
	continue;
	    
      OP *op = (OP *)tfex->node_user(node);
      if (!op)
	continue;
	    
      INT64 val = 0;
      FUSION_OP *rtor = tn_vector_value_at_op(op, tn, val);
      if (!rtor)
        continue;
	    
      if (tracing()) {
	fPrint_TN(trace_file(), "%s", tn);
	fprintf(trace_file(),": constant scalar-to-vector value 0x%" LLX_FMT "\n", val);
      }
	    
      if (set_vector_constant_operand(opnd, rtor, val)) {
	if (tracing())
	  tfex->opnd_print(opnd, trace_file(), 2);
      } else {
	if (tracing())
	  fprintf(trace_file(), "Can't calculate vector operand\n");
      }
    }
  }
}

bool
is_store_addi_fusion(tf_match_map_t match_map) {

    OP* store_op = NULL;
    OP* addi_op = NULL;
    tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
    for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
      tf_node_t node = tfex->node_node_pair_key(node_pair);
      OP *op = (OP *)tfex->node_user(node);

      if (op && (OP_code(op)==TOP_s32i || OP_code(op)==TOP_s16i || OP_code(op)==TOP_s8i)) {
	if (store_op!=NULL)
	  return false;
	store_op = op;
      } else if (op && OP_code(op)==TOP_addi) {
	if (addi_op!=NULL)
	  return false;
	addi_op = op;
      } else
	return false;
    }

    if (store_op==NULL || addi_op==NULL)
      return false;

    if (OP_opnd(addi_op,0)==OP_result(addi_op,0) &&
	OP_opnd(addi_op,0)==OP_opnd(store_op,1))
      return true;

    return false;
}


// check if current fusion can potentially hurt later optimization/analysis
bool
CG_FUSION::bad_match(tf_match_map_t match_map) {

  if (_fusion_op->is_load() || _fusion_op->is_store() &&
      (_fusion_op->base_idx()<0 || _fusion_op->offset_idx()<0)) {

    // if the current fusion can make CG memory depepdence analysis difficult,
    // check if LNO dependence info is available

    tf_node_node_map_t rev_node_map = tfex->match_map_rev_node_map(match_map);
    for (tf_node_node_pair_t node_pair = tfex->node_node_map_first_pair(rev_node_map);
       node_pair != TF_NODE_NODE_PAIR_INVALID;
       node_pair = tfex->node_node_map_next_pair(node_pair)) {
      tf_node_t node = tfex->node_node_pair_key(node_pair);
      OP *op = (OP *)tfex->node_user(node);

      if (op && (OP_load(op) || OP_store(op))) {

        WN* wn = Get_WN_From_Memory_OP(op);
        if (wn==NULL)
	  return true;

	bool has_lno_alias_info = 
	    (Current_Dep_Graph && Current_Dep_Graph->Get_Vertex(wn)!=0);

	if (has_lno_alias_info==false && is_store_addi_fusion(match_map))
	  return true;
      }
    }
  }

  return false;
}

void
CG_FUSION::optimize_bb (BB *bb, bool aggressive)
{
  if (!_fusion_ops || _fusion_ops->Elements() == 0)
    return;
    
  XT_MEMPOOL::mark mark(xt_opt_pool());
  MEM_POOL_Popper popper(local_pool());

  if (tracing()) {
    fprintf(trace_file(), CG_FUSION_LINE);
    fprintf(trace_file(), "BB for fusion\n");
    fprintf(trace_file(), CG_FUSION_LINE);
    Print_BB(bb);
  }
    
  _bb_dfg = build_dfg(xt_opt_pool(), bb, /* use_reg_size */ true);
  if (_bb_dfg == TF_DFG_INVALID) {
    if (tracing())
      fprintf(trace_file(), "Unable to build BB DFG.\n");
    return;
  }

  // For debugging...
  tfex->dfg_set_cback(_bb_dfg, dfg_cback());
    
  _bb = bb;
  _bb_op_node_map = NULL;
    
  // Find and initiliaze any operands that have constant vector values.
  find_vector_constants(_bb_dfg);

  /* Break down all edges to wires first to forward propagate immediates.
     Then raise the rest of the operands back to PACKED. */
  tfex->opt_convert_edges_to_wires(_bb_dfg);
  tfex->opt_convert_edges_to_packed(_bb_dfg);
  tfex->opt_prune_copies(_bb_dfg);
  tfex->dfg_compact_nodes_and_edges(_bb_dfg);
  
  // Canonicalize the BB dfg to increase the chance of finding a match.
  tfex->match_canonicalize(_bb_dfg, bb_match_cback());
    
  /* Initialize the BB DFG node predecessor sets. */
  _bb_dfg_preds = tfex->dfg_reachable_sets((tf_pool_t)xt_opt_pool(), _bb_dfg,
                                           /* successors */ 0,
                                           /* include_dependence_edges */ 1);
  Is_True(_bb_dfg_preds != TF_NODE_NODE_SET_MAP_INVALID, ("Invalid bb dfg predecessors"));
  bb_match_cback_user()->set_bb_dfg_preds(_bb_dfg_preds);
  
  for (INT fidx = 0; fidx < _fusion_ops->Elements(); fidx++) {
    _fusion_op = _fusion_ops->Get(fidx);
    bb_match_cback_user()->set_fusion_op(_fusion_op);

    /* Disallow certain fusions through compiler options for
       debugging purposes... */
    if (CG_fusion_before >= 0 || CG_fusion_after >= 0) {
      bool skip = false;
      if (CG_fusion_before < CG_fusion_after) {
        /* Case: +++ b --- a +++ */
        skip = (fidx > CG_fusion_before && fidx < CG_fusion_after);
      } else {
        /* Case: --- a +++ b --- */
        skip = (fidx < CG_fusion_after || fidx > CG_fusion_before);
      }
      
      if (skip) {
        if (tracing())
          fprintf(trace_file(), "[idx %d] %s: skipping...\n", fidx, _fusion_op->name());
        continue;
      }
    }

    if (TI_TOP_No_Valid_Format(_fusion_op->top())) {
      if (tracing())
	fprintf(trace_file(), "[idx %d] %s: no valid format, skipping...\n", fidx, _fusion_op->name());
      continue;
    }

    while (true) {
      if (tracing())
	fprintf(trace_file(), "[idx %d] %s: matching...\n", fidx, _fusion_op->name());
      
      tf_dfg_t sub_dfg = tfex->op_dfg_dfg(_fusion_op->op_dfg());
      tf_match_map_t match_map =
	tfex->match_find_match((tf_pool_t)xt_opt_pool(),
			       sub_dfg, _bb_dfg,
			       /* match_io */ 0, bb_match_cback());
      if (match_map == TF_MATCH_MAP_INVALID)
	break;

      if (!aggressive && bad_match(match_map))
	break;

      if (!apply_fusion(_fusion_op, match_map))
	break;
    }
  }

  bb_match_cback_user()->set_fusion_op(NULL);
  bb_match_cback_user()->set_bb_dfg_preds(NULL);
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
