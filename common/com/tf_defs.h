/* Copyright (c) 2004-2006 Tensilica, Inc.  All Rights Reserved.

   This program is the copyrighted work of Tensilica Inc.  You may use
   it under the terms of version 2 of the GNU General Public License as
   published by the Free Software Foundation.  Other use is prohibited
   without the prior written consent of Tensilica Inc.

   This program is distributed WITHOUT ANY WARRANTY; without even the
   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  */


/* $Id: //depot/rel/BadgerPass/Xtensa/Software/libfusion/tf_defs.h#2 $ */

#ifndef __TF_DEFS_H__
#define __TF_DEFS_H__

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#if 0
} /* fix emacs auto-indent */
#endif
#endif

  
/* Library status. */
typedef enum tf_status_enum
{
  TF_STATUS_SUCCESS,
  TF_STATUS_FAIL,
} tf_status_t;
  

typedef int tf_node_idx_t;
typedef int tf_edge_idx_t;
typedef int tf_opnd_idx_t;


#define TF_NODE_IDX_U ((tf_node_idx_t)(-1)) /* unknown node index */
#define TF_OPND_IDX_U ((tf_opnd_idx_t)(-1)) /* unknown operand index */

#define TF_EDGE_IDX_U ((tf_edge_idx_t)(-1)) /* unknown edge */
#define TF_EDGE_IDX_X ((tf_edge_idx_t)(-2)) /* 'don't care' edge (input or output) */
#define TF_EDGE_IDX_0 ((tf_edge_idx_t)(-3)) /* constant 0 edge (shortcut) */
#define TF_EDGE_IDX_1 ((tf_edge_idx_t)(-4)) /* constant 1 edge (shortcut) */


/* Opaque handles used to reference libfusion objects. */
typedef struct tf_dfg_ext { } *tf_dfg_t;
#define TF_DFG_INVALID  ((tf_dfg_t)NULL)
typedef struct tf_node_ext { } *tf_node_t;
#define TF_NODE_INVALID  ((tf_node_t)NULL)
typedef struct tf_edge_ext { } *tf_edge_t;
#define TF_EDGE_INVALID  ((tf_edge_t)NULL)
typedef struct tf_opnd_ext { } *tf_opnd_t;
#define TF_OPND_INVALID  ((tf_opnd_t)NULL)
typedef struct tf_op_dfg_ext { } *tf_op_dfg_t;
#define TF_OP_DFG_INVALID  ((tf_op_dfg_t)NULL)
typedef struct tf_match_map_ext { } *tf_match_map_t;
#define TF_MATCH_MAP_INVALID  ((tf_match_map_t)NULL)
typedef struct tf_sems_ext { } *tf_sems_t;
#define TF_SEMS_INVALID  ((tf_sems_t)NULL)
typedef struct tf_sem_tree_ext { } *tf_sem_tree_t;
#define TF_SEM_TREE_INVALID  ((tf_sem_tree_t)NULL)

typedef struct tf_node_node_map_ext { } *tf_node_node_map_t;
#define TF_NODE_NODE_MAP_INVALID  ((tf_node_node_map_t)NULL)
typedef struct tf_node_node_pair_ext { } *tf_node_node_pair_t;
#define TF_NODE_NODE_PAIR_INVALID  ((tf_node_node_pair_t)NULL)
typedef struct tf_node_set_ext { } *tf_node_set_t;
#define TF_NODE_SET_INVALID  ((tf_node_set_t)NULL)
typedef struct tf_node_set_iter_ext { } *tf_node_set_iter_t;
#define TF_NODE_SET_ITER_INVALID  ((tf_node_set_iter_t)NULL)
typedef struct tf_node_node_set_map_ext { } *tf_node_node_set_map_t;
#define TF_NODE_NODE_SET_MAP_INVALID  ((tf_node_node_set_map_t)NULL)
typedef struct tf_opnd_vec_ext { } *tf_opnd_vec_t;
#define TF_OPND_VEC_INVALID  ((tf_opnd_vec_t)NULL)
typedef struct tf_opnd_opnd_map_ext { } *tf_opnd_opnd_map_t;
#define TF_OPND_OPND_MAP_INVALID  ((tf_opnd_opnd_map_t)NULL)
typedef struct tf_edge_idx_dlist_ext { } *tf_edge_idx_dlist_t;
#define TF_EDGE_IDX_DLIST_INVALID  ((tf_edge_idx_dlist_t)NULL)
typedef struct tf_edge_idx_dlist_iter_ext { } *tf_edge_idx_dlist_iter_t;
#define TF_EDGE_IDX_DLIST_ITER_INVALID  ((tf_edge_idx_dlist_iter_t)NULL)

typedef struct tf_const_val_ext { } *tf_const_val_t;
#define TF_CONST_VAL_INVALID  ((tf_const_val_t)NULL)
typedef struct tf_dyn_buffer_ext { } *tf_dyn_buffer_t;
#define TF_DYN_BUFFER_INVALID  ((tf_dyn_buffer_t)NULL)
typedef struct tf_pool_ext { } *tf_pool_t;
#define TF_POOL_INVALID  ((tf_pool_t)NULL)

/* Node kinds. Note: keep this enum in-sync with the node kind property table
   in tf_node.cc. */
typedef enum tf_node_kind_enum
{
  TFN_UNKNOWN = 0,      /* unknown node kind */
  TFN_ABS,              /* abs(in) -> out */
  TFN_ADD,              /* in + in -> out */
  TFN_ASHR,             /* sign-extend(in) >> in -> out */
  TFN_BAND,	        /* in & in -> out */
  TFN_BNOR,             /* ~(in | in) -> out */
  TFN_BNOT,             /* ~in -> out */
  TFN_BOR,	        /* in | in -> out */
  TFN_BXNOR,	        /* in ~^ in -> out */
  TFN_BXOR,	        /* in ^ in -> out */
  TFN_CALL,             /* id ([in]*) -> [out]* */
  TFN_COND,	        /* in ? in : in -> out */
  TFN_CONST,            /* constant */
  TFN_COPY,             /* copy node (in -> [out]*) */
  TFN_EQ,               /* in == in -> out */
  TFN_FLOP,             /* flip-flop (in -> out) */
  TFN_GEQ,              /* in >= in -> out */
  TFN_GT,               /* in > in -> out */
  TFN_IDX,              /* in[in] -> out */
  TFN_LAND,             /* in && in -> out */
  TFN_LEQ,              /* in <= in -> out */
  TFN_LNOT,             /* !in -> out */
  TFN_LOR,              /* in || in -> out */
  TFN_LSHR,             /* zero-extend(in) >> in -> out */
  TFN_LT,               /* in < in -> out */
  TFN_MACRO,            /* macro([in]*) -> out */
  TFN_MUL,              /* in * in -> out */
  TFN_NEG,              /* -in -> out */
  TFN_NEQ,              /* in != in -> out */
  TFN_RAND,             /* &in -> out */
  TFN_RNAND,            /* ~&in -> out */
  TFN_RNOR,             /* ~|in -> out */
  TFN_ROR,              /* |in -> out */
  TFN_RXNOR,            /* ~^in -> out */
  TFN_RXOR,             /* ^in -> out */
  TFN_SHL,              /* in << in -> out */
  TFN_SUB,              /* in - in -> out */
  TFN_TABLE,            /* in[in] -> out */
  TFN_TIECOMP,          /* TIEcomp(id, in*) -> out* */
  TFN_TIEPRINT,         /* TIEprint(in, format, in*) */
  TFN_USER,             /* user specified node */
  TFN_LAST
} tf_node_kind_t;

  
/* Operand kinds. */
typedef enum tf_opnd_kind_enum
{
  TFO_UNKNOWN = 0,           /* unknown operand kind */
  TFO_PACKED,                /* packed typed data (a single edge) */
  TFO_ELEM_WIRE,             /* wire representing a single (vector) element */
  TFO_WIRE,                  /* wire representing each data bit */
} tf_opnd_kind_t;


#define TF_OPND_KIND_STR { \
  "UNKNOWN",               \
  "PACKED",                \
  "ELEM_WIRE",             \
  "WIRE",                  \
}

/* Dependence kinds. */
typedef enum tf_dep_kind_enum
{
  TFD_UNKNOWN = 0,           /* unknown dependence kind */

  TFD_REG_IN,                 /* register flow dependence */
  TFD_REG_OUT,                /* register output dependence */
  TFD_REG_ANTI,               /* register anti dependence */

  TFD_MEM_IN,                 /* memory flow dependence */
  TFD_MEM_OUT,                /* memory output dependence */
  TFD_MEM_ANTI,               /* memory anti dependence */

  TFD_TIE_PRINT,              /* tie print order dependence */

  TFD_MISC, 	              /* misc dependence */

} tf_dep_kind_t;


#define TF_DEP_KIND_STR { \
  "UNKNOWN",              \
  "REG_IN",               \
  "REG_OUT",              \
  "REG_ANTI",             \
  "MEM_IN",               \
  "MEM_OUT",              \
  "MEM_ANTI",             \
  "TIE_PRINT",            \
  "MISC",                 \
}


/* Semantic signal direction. */
typedef enum tf_sem_dir_enum
{
  TF_SEM_DIR_UNKNOWN = 0,
  TF_SEM_DIR_IN,
  TF_SEM_DIR_OUT,
  TF_SEM_DIR_INOUT
} tf_sem_dir_t;

#define TF_SEM_DIR_LAST TF_SEM_DIR_INOUT

#define TF_SEM_DIR_STR { \
  "unknown",             \
  "in",                  \
  "out",                 \
  "inout",               \
}


/* Semantic node kind. Note: keep this enum in-sync with the property table
   in tf_sem.cc. */
typedef enum tf_sem_kind_enum
{
  TFS_UNKNOWN = 0,
  TFS_ADD,		    /* (NODE exp, NODE exp) */
  TFS_ASSIGNMENT,	    /* (NODE lvalue, NODE exp) */ 
  TFS_BITWISE_AND,	    /* (NODE exp, NODE exp) */
  TFS_BITWISE_NEGATION,     /* (NODE exp) */
  TFS_BITWISE_OR,	    /* (NODE exp, NODE exp) */
  TFS_BITWISE_XNOR,	    /* (NODE exp, NODE exp) */
  TFS_BITWISE_XOR,	    /* (NODE exp, NODE exp) */
  TFS_CALL,	  	    /* (ID id, [NODE exp]*) */
  TFS_COMPMOD,  	    /* (ID op name, COMPMOD_ARG_LIST, COMPMOD_ARG_LIST) */
  TFS_COMPMOD_ARG,  	    /* (ID arg name, EXPRESSION) */
  TFS_COMPMOD_ARG_LIST,     /* ([COMPMOD_ARG]*) */
  TFS_CONCATENATION,	    /* ((NODE exp)+) */
  TFS_CONDITIONAL,	    /* (NODE exp, NODE exp, NODE exp) */
  TFS_CONST,		    /* leaf, string value */
  TFS_DECODE,               /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_EQ,		    /* (NODE exp, NODE exp) */
  TFS_EXCEPTION,            /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_FLOP_WIRE,            /* (INT from, INT to, (ID var)+) */
  TFS_FUNCTION,             /* (ID id, INT from, INT to, NODE IO, NODE STATEMENTS) */
  TFS_GEQ,		    /* (NODE exp, NODE exp) */
  TFS_GT,		    /* (NODE exp, NODE exp) */
  TFS_ID,		    /* string value (possible index kids) */
  TFS_INT,		    /* leaf, integer value */
  TFS_INTERFACE,            /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_IO,                   /* (NODE (operand | state | interface | exception | decode | kill))* */
  TFS_KILL,                 /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_LEQ,		    /* (NODE exp, NODE exp) */
  TFS_LOGICAL_AND,	    /* (NODE exp, NODE exp) */
  TFS_LOGICAL_NEGATION,     /* (NODE exp) */
  TFS_LOGICAL_OR,	    /* (NODE exp, NODE exp) */
  TFS_LT,		    /* (NODE exp, NODE exp) */
  TFS_MULT,		    /* (NODE exp, NODE exp) */
  TFS_NEQ,		    /* (NODE exp, NODE exp) */
  TFS_OPERAND,              /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_PRINT,                /* (NODE exp, STRING format, PRINT_ARG_LIST) */
  TFS_PRINT_ARG_LIST,       /* ((NODE exp)*) */
  TFS_REDUCTION_AND,	    /* (NODE exp) */
  TFS_REDUCTION_NAND,       /* (NODE exp) */
  TFS_REDUCTION_NOR,	    /* (NODE exp) */
  TFS_REDUCTION_OR,	    /* (NODE exp) */
  TFS_REDUCTION_XNOR,       /* (NODE exp) */
  TFS_REDUCTION_XOR,	    /* (NODE exp) */
  TFS_REPLICATION,	    /* (INT num, NODE exp) */
  TFS_SHIFT_LEFT,	    /* (NODE exp, NODE exp) */
  TFS_SHIFT_RIGHT,	    /* (NODE exp, NODE exp) */
  TFS_SEMANTIC,             /* (ID id, NODE IO, NODE STATEMENTS) */
  TFS_STATE,                /* (ID id, INT from, INT to, TF_SEM_DIR dir) */
  TFS_STATEMENTS,	    /* ((WIRE | ASSIGNMENT | CALL)*) */
  TFS_STRING,               /* leaf, string value */
  TFS_SUB,		    /* (NODE exp, NODE exp) */
  TFS_TABLE,                /* (ID id, INT width, INT depth, (CONST|INT v)*) */
  TFS_TREE_TOP,             /* ((NODE)*) */
  TFS_WIRE,		    /* (INT from, INT to, (ID var)+) */
  TFS_LAST		    /* ** must be last ** */
} tf_sem_kind_t;

#define TFS_SEM_KIND_CNT TFS_LAST


/* DFG callback handle. */
typedef struct tf_dfg_callback_struct tf_dfg_callback_t;
struct tf_dfg_callback_struct
{
  void *user;
  
  void (*opnd_read) (tf_dfg_callback_t *, tf_opnd_t, tf_dyn_buffer_t, int *);
  void (*opnd_write) (tf_dfg_callback_t *, tf_opnd_t, tf_dyn_buffer_t);
  void (*opnd_print) (tf_dfg_callback_t *, tf_opnd_t, FILE *);
  unsigned int (*opnd_hash) (tf_dfg_callback_t *, tf_opnd_t);
  
  void (*node_read) (tf_dfg_callback_t *, tf_node_t, tf_dyn_buffer_t, int *);
  void (*node_write) (tf_dfg_callback_t *, tf_node_t, tf_dyn_buffer_t);
  void (*node_print) (tf_dfg_callback_t *, tf_node_t, FILE *);
  unsigned int (*node_hash) (tf_dfg_callback_t *, tf_node_t);
  int (*node_emit_vcg_label) (tf_dfg_callback_t *, tf_node_t, FILE *);
  int (*node_optimize)(tf_dfg_callback_t *, tf_node_t);
};


/* DFG matcher callback handle. */
typedef struct tf_match_callback_struct tf_match_callback_t;
struct tf_match_callback_struct
{
  void *user;
  
  int (*node_compare) (tf_match_callback_t *, tf_node_t node_a, tf_node_t node_b);
  
  int (*commutative_inputs) (tf_match_callback_t *, tf_node_t node);
  int (*commutative_outputs) (tf_match_callback_t *, tf_node_t node);
  
  int (*canonical_order_nodes) (tf_match_callback_t *, tf_node_t node_a, tf_node_t node_b);
  
  /* Called by TF_MATCH::find_match for each possible match that is found.
     Return non-zero if the search should stop returning 'match_map'.
     Return 0 if the search should continue. */
  int (*found_match) (tf_match_callback_t *, tf_match_map_t match_map);
};


/* libfusion handle for dynamic linking. */
typedef struct tf_exports_struct
{
  /* libfusion. */
  tf_status_t (*init) (void);
  tf_status_t (*free) (void);

  /* Tracing. */
  tf_status_t (*enable_trace) (const char *trace_name);
  tf_status_t (*disable_trace) (const char *trace_name);

  /* Callback. */
  void (*dfg_callback_init) (tf_dfg_callback_t *cback);
  void (*match_callback_init) (tf_match_callback_t *cback);

  /* DFG */
  tf_dfg_t (*dfg_init) (tf_pool_t pool);
  tf_dfg_t (*dfg_read) (tf_dyn_buffer_t buf, int *offset, tf_pool_t pool,
			tf_dfg_callback_t *cback);
  tf_dfg_t (*dfg_copy) (tf_pool_t pool, tf_dfg_t dfg);

  void (*dfg_write) (tf_dfg_t dfg, tf_dyn_buffer_t buf);
  void (*dfg_print) (tf_dfg_t dfg, FILE *file, int tab);
  void (*dfg_dump) (tf_dfg_t dfg);

  tf_pool_t (*dfg_pool) (tf_dfg_t dfg);
  int (*dfg_node_count) (tf_dfg_t dfg);
  tf_node_idx_t (*dfg_next_node_index) (tf_dfg_t dfg);
  tf_dfg_callback_t * (*dfg_cback) (tf_dfg_t dfg);
  void (*dfg_set_cback) (tf_dfg_t dfg, tf_dfg_callback_t *cback);

  tf_node_t (*dfg_first_node) (tf_dfg_t dfg);
  tf_node_t (*dfg_next_node) (tf_node_t node);

  tf_node_t (*dfg_new_node) (tf_dfg_t dfg, tf_node_kind_t kind);
  tf_edge_t (*dfg_new_data_edge) (tf_dfg_t dfg,
				  tf_node_idx_t from_node_idx,
				  tf_opnd_idx_t from_opnd_idx,
				  int from_pos,
				  tf_node_idx_t to_node_idx,
				  tf_opnd_idx_t to_opnd_idx,
				  int to_pos);
  tf_edge_t (*dfg_new_dep_edge) (tf_dfg_t dfg,
				 tf_node_idx_t from_node_idx,
				 tf_node_idx_t to_node_idx);
  void (*dfg_compact_nodes_and_edges) (tf_dfg_t dfg);
  tf_node_t (*dfg_node) (tf_dfg_t dfg, tf_node_idx_t idx);
  tf_edge_t (*dfg_edge) (tf_dfg_t dfg, tf_edge_idx_t idx);
  void (*dfg_delete_node) (tf_dfg_t dfg, tf_node_t node);
  void (*dfg_delete_edge) (tf_dfg_t dfg, tf_edge_t edge);
  
  /* Operands. */
  tf_opnd_t (*opnd_copy) (tf_pool_t pool, tf_opnd_t opnd);
  tf_node_t (*opnd_node) (tf_opnd_t opnd);
  tf_pool_t (*opnd_pool) (tf_opnd_t opnd);
  tf_opnd_idx_t (*opnd_index) (tf_opnd_t opnd);
  tf_opnd_kind_t (*opnd_kind) (tf_opnd_t opnd);
  void (*opnd_set_kind) (tf_opnd_t opnd, tf_opnd_kind_t kind);
  const char * (*opnd_name) (tf_opnd_t opnd);
  void (*opnd_set_name) (tf_opnd_t opnd, const char *name, int copy);
  void * (*opnd_user) (tf_opnd_t opnd);
  void (*opnd_set_user) (tf_opnd_t opnd, void *user);
  int (*opnd_vector_length) (tf_opnd_t opnd);
  int (*opnd_elem_bit_size) (tf_opnd_t opnd);
  void (*opnd_set_elem_bit_size) (tf_opnd_t opnd, int elem_bit_size);
  int (*opnd_bit_size) (tf_opnd_t opnd);
  int (*opnd_is_input) (tf_opnd_t opnd);
  int (*opnd_is_output) (tf_opnd_t opnd);
  tf_edge_idx_t (*opnd_edge_idx) (tf_opnd_t opnd, int pos);
  void (*opnd_set_edge_idx) (tf_opnd_t opnd, int pos, tf_edge_idx_t eidx);
  tf_edge_t (*opnd_edge) (tf_opnd_t opnd, int pos);
  int (*opnd_edge_idxs_count) (tf_opnd_t opnd);
  void (*opnd_set_edge_idxs_count) (tf_opnd_t opnd, int count, tf_edge_idx_t eidx);
  int (*opnd_is_unknown) (tf_opnd_t opnd);
  void (*opnd_print) (tf_opnd_t opnd, FILE *file, int tab);
  void (*opnd_dump) (tf_opnd_t opnd);

  /* Edges. */
  tf_dfg_t (*edge_dfg) (tf_edge_t edge);
  tf_node_t (*edge_source_node) (tf_edge_t edge);
  tf_opnd_t (*edge_source_opnd) (tf_edge_t edge);
  int (*edge_source_pos) (tf_edge_t edge);
  void (*edge_detach_from_source) (tf_edge_t edge);
  void (*edge_attach_to_source) (tf_edge_t edge,
                                 tf_node_idx_t node_idx, tf_opnd_idx_t opnd_idx, int pos);
  void (*edge_set_dep_kind) (tf_edge_t edge, tf_dep_kind_t dep_kind);
  void (*edge_set_omega) (tf_edge_t edge, int omega);
  tf_dep_kind_t (*edge_dep_kind) (tf_edge_t edge);
  int (*edge_omega) (tf_edge_t edge);
  tf_node_t (*edge_sink_node) (tf_edge_t edge);
  tf_opnd_t (*edge_sink_opnd) (tf_edge_t edge);
  int (*edge_sink_pos) (tf_edge_t edge);
  void (*edge_detach_from_sink) (tf_edge_t edge);
  void (*edge_attach_to_sink) (tf_edge_t edge,
                               tf_node_idx_t node_idx, tf_opnd_idx_t opnd_idx, int pos);

  /* Nodes. */
  tf_node_idx_t (*node_index) (tf_node_t node);
  tf_node_kind_t (*node_kind) (tf_node_t node);
  void (*node_set_kind) (tf_node_t node, tf_node_kind_t kind);
  void * (*node_user) (tf_node_t node);
  void (*node_set_user) (tf_node_t node, void *user);
  tf_const_val_t (*node_const_val) (tf_node_t node);
  void (*node_set_const_val) (tf_node_t node, tf_const_val_t val);
  const char * (*node_call_id) (tf_node_t node);
  void (*node_set_call_id) (tf_node_t node, const char *call_id);
  const char * (*node_tiecomp_id) (tf_node_t node);
  void (*node_set_tiecomp_id) (tf_node_t node, const char *tiecomp_id);

  tf_opnd_t (*node_new_operand) (tf_node_t node, tf_opnd_kind_t kind,
				 int elem_bit_size, int vector_length);

  /* Node kind. */
  const char * (*node_kind_str) (tf_node_kind_t kind);

  /* Node inputs. */
  tf_opnd_vec_t (*node_inputs) (tf_node_t node);
  tf_opnd_t (*node_input) (tf_node_t node, tf_opnd_idx_t idx);
  int (*node_input_count) (tf_node_t node);
  void (*node_set_input_count) (tf_node_t node, int count);
  tf_opnd_idx_t (*node_add_input) (tf_node_t node, tf_opnd_t opnd);
  void (*node_set_input) (tf_node_t node, tf_opnd_idx_t idx, tf_opnd_t opnd);
  tf_opnd_t (*node_delete_input) (tf_node_t node, tf_opnd_idx_t idx);
  
  tf_edge_idx_dlist_t (*node_preds) (tf_node_t node);

  /* Node outputs. */
  tf_opnd_vec_t (*node_outputs) (tf_node_t node);
  tf_opnd_t (*node_output) (tf_node_t node, tf_opnd_idx_t idx);
  int (*node_output_count) (tf_node_t node);
  void (*node_set_output_count) (tf_node_t node, int count);
  tf_opnd_idx_t (*node_add_output) (tf_node_t node, tf_opnd_t opnd);
  void (*node_set_output) (tf_node_t node, tf_opnd_idx_t idx, tf_opnd_t opnd);
  tf_opnd_t (*node_delete_output) (tf_node_t node, tf_opnd_idx_t idx);

  tf_edge_idx_dlist_t (*node_succs) (tf_node_t node);

  /* Op DFG */
  tf_op_dfg_t (*op_dfg_init) (tf_pool_t pool, tf_dfg_t dfg, int arg_count);
  tf_op_dfg_t (*op_dfg_copy) (tf_pool_t pool, tf_op_dfg_t op_dfg);
  void (*op_dfg_print) (tf_op_dfg_t op_dfg, FILE *file, int tab);
  void (*op_dfg_dump) (tf_op_dfg_t op_dfg);
  tf_dfg_t (*op_dfg_dfg) (tf_op_dfg_t op_dfg);
  tf_opnd_vec_t (*op_dfg_inputs) (tf_op_dfg_t op_dfg);
  tf_opnd_vec_t (*op_dfg_outputs) (tf_op_dfg_t op_dfg);
  int (*op_dfg_input_count) (tf_op_dfg_t op_dfg);
  int (*op_dfg_output_count) (tf_op_dfg_t op_dfg);
  tf_opnd_t (*op_dfg_skip_null_input) (tf_op_dfg_t op_dfg, int idx);
  tf_opnd_t (*op_dfg_skip_null_output) (tf_op_dfg_t op_dfg, int idx);
  int (*op_dfg_skip_null_find_input) (tf_op_dfg_t op_dfg, tf_opnd_t opnd);
  int (*op_dfg_skip_null_find_output) (tf_op_dfg_t op_dfg, tf_opnd_t opnd);
  int (*op_dfg_find_input_by_name) (tf_op_dfg_t op_dfg, const char *name);
  int (*op_dfg_find_output_by_name) (tf_op_dfg_t op_dfg, const char *name);
  
  /* Node data structures. */
  int (*node_node_map_find) (tf_node_node_map_t node_map,
			     tf_node_t node_key, tf_node_t *node_val);
  void (*node_node_map_insert) (tf_node_node_map_t node_map,
                                tf_node_t node_key, tf_node_t node_val);
  tf_node_node_pair_t (*node_node_map_first_pair) (tf_node_node_map_t node_map);
  tf_node_node_pair_t (*node_node_map_next_pair) (tf_node_node_pair_t node_pair);
  tf_node_t (*node_node_pair_key) (tf_node_node_pair_t node_pair);
  tf_node_t (*node_node_pair_value) (tf_node_node_pair_t node_pair);
  tf_node_set_t (*node_set_init) (tf_pool_t pool, int table_size);
  int (*node_set_add_node) (tf_node_set_t node_set, tf_node_t node);
  void (*node_set_add_set) (tf_node_set_t node_set, tf_node_set_t add_set);
  int (*node_set_contains) (tf_node_set_t node_set, tf_node_t node);
  int (*node_set_intersects) (tf_node_set_t set_a, tf_node_set_t set_b);
  tf_node_set_iter_t (*node_set_iter_first) (tf_node_set_t nset);
  tf_node_set_iter_t (*node_set_iter_next) (tf_node_set_iter_t niter);
  tf_node_t (*node_set_iter_elem) (tf_node_set_iter_t niter);
  int (*node_node_set_map_find) (tf_node_node_set_map_t map,
				 tf_node_t node, tf_node_set_t *node_set);
  void (*node_node_set_map_insert) (tf_node_node_set_map_t map,
                                    tf_node_t node, tf_node_set_t node_set);

  /* Operand data structures. */
  int (*opnd_vec_size) (tf_opnd_vec_t opnds);
  tf_opnd_t (*opnd_vec_get) (tf_opnd_vec_t opnds, int idx);
  void (*opnd_vec_set) (tf_opnd_vec_t opnds, int idx, tf_opnd_t opnd);
  tf_opnd_t (*opnd_vec_remove) (tf_opnd_vec_t opnds, int idx);
  int (*opnd_vec_find) (tf_opnd_vec_t opnds, tf_opnd_t opnd);
  int (*opnd_opnd_map_find) (tf_opnd_opnd_map_t opnd_map,
			     tf_opnd_t opnd_key, tf_opnd_t *opnd_val);

  /* Edge data structures. */
  tf_edge_idx_dlist_iter_t (*edge_idx_dlist_iter_first) (tf_edge_idx_dlist_t elist);
  tf_edge_idx_dlist_iter_t (*edge_idx_dlist_iter_next) (tf_edge_idx_dlist_iter_t eiter);
  tf_edge_idx_t (*edge_idx_dlist_iter_elem) (tf_edge_idx_dlist_iter_t eiter);


  /* Utilities. */
  tf_node_t (*copy_in_reroute) (tf_node_t node, tf_opnd_idx_t oidx);
  tf_node_t (*copy_out_reroute) (tf_node_t node, tf_opnd_idx_t oidx);
  tf_node_t (*copy_reroute) (tf_opnd_t opnd);
  tf_node_node_set_map_t (*dfg_reachable_sets) (tf_pool_t map_pool, tf_dfg_t dfg,
                                                int successors,
                                                int include_dependence_edges);
  void (*inline_dfg) (tf_node_t node, tf_op_dfg_t op_dfg);
  int (*validate_op_dfg) (tf_op_dfg_t op_dfg);
  
  /* DFG optimizations. */
  int (*opt_prune_copies) (tf_dfg_t dfg);
  void (*opt_convert_edges_to_wires) (tf_dfg_t dfg);
  void (*opt_convert_edges_to_packed) (tf_dfg_t dfg);
  int (*opt_optimize) (tf_dfg_t dfg);
  int (*opt_optimize_cse) (tf_dfg_t dfg);
  int (*opt_copy_propagate_call_args) (tf_dfg_t dfg);
  void (*opt_set_tcopt) (int tcopt);
  int (*opt_get_tcopt) (void);

  /* DFG matching. */
  tf_match_map_t (*match_find_match) (tf_pool_t match_pool,
				      tf_dfg_t sub_dfg, tf_dfg_t dfg,
				      int match_io, tf_match_callback_t *cback);
  tf_match_map_t (*match_isomorphic) (tf_pool_t match_pool,
				      tf_dfg_t dfg_a, tf_dfg_t dfg_b,
				      tf_match_callback_t *cback);
  void (*match_canonicalize) (tf_dfg_t dfg, tf_match_callback_t *cback);
  tf_node_t (*match_replace_sub_dfg) (tf_op_dfg_t sub_op_dfg, tf_match_map_t match_map);
  tf_dfg_t (*match_map_sub_dfg) (tf_match_map_t match_map);
  tf_dfg_t (*match_map_dfg) (tf_match_map_t match_map);
  tf_opnd_opnd_map_t (*match_map_opnd_map) (tf_match_map_t match_map);
  tf_opnd_opnd_map_t (*match_map_rev_opnd_map) (tf_match_map_t match_map);
  tf_node_node_map_t (*match_map_node_map) (tf_match_map_t match_map);
  tf_node_node_map_t (*match_map_rev_node_map) (tf_match_map_t match_map);
  
  /* TIE/Verilog semantics. */
  const char *(*sem_dir_str) (tf_sem_dir_t dir);
  const char *(*sem_kind_str) (tf_sem_kind_t kind);
  tf_sem_kind_t (*sem_tree_get_kind) (tf_sem_tree_t semt);
  tf_sems_t (*sems_default) (void);
  tf_sem_tree_t (*sems_new_signal) (tf_sems_t sems, tf_sem_kind_t kind, const char *id,
				    int from, int to, tf_sem_dir_t dir);
  tf_sem_tree_t (*sems_new_io) (tf_sems_t sems);
  tf_sem_tree_t (*sems_new_body) (tf_sems_t sems, const char *statements);
  tf_sem_tree_t (*sems_new_semantic) (tf_sems_t sems, const char *id,
				      tf_sem_tree_t io, tf_sem_tree_t body);
  tf_sem_tree_t (*sems_new_function) (tf_sems_t sems, const char *id, int from, int to,
				      tf_sem_tree_t io, tf_sem_tree_t body);
  tf_sem_tree_t (*sems_new_table) (tf_sems_t sems, const char *id, int width, int depth);
  void (*sems_new_tiecomp) (tf_sems_t sems, const char *id, int num_opnds);
  void (*sems_tiecomp_set_opnd) (tf_sems_t sems, const char *id, int idx,
                                 const char *name, tf_sem_dir_t dir, int bit_size);
  void (*sems_add_global_object) (tf_sems_t sems, tf_sem_tree_t obj);
  void (*sem_tree_append_child) (tf_sem_tree_t semt, tf_sem_tree_t child);
  tf_sem_tree_t (*sem_tree_get_first_child) (tf_sem_tree_t semt);
  tf_sem_tree_t (*sem_tree_get_last_child) (tf_sem_tree_t semt);
  tf_sem_tree_t (*sem_tree_get_next_sibling) (tf_sem_tree_t semt);
  tf_sem_tree_t (*sem_tree_get_prev_sibling) (tf_sem_tree_t semt);
  const char *(*sem_tree_get_const) (tf_sem_tree_t semt);
  const char *(*sem_tree_get_id) (tf_sem_tree_t semt);
  int (*sem_tree_get_int) (tf_sem_tree_t semt);

  /* Semantic to DFG */
  tf_op_dfg_t (*sem2dfg) (tf_pool_t pool, tf_sems_t sems, tf_sem_tree_t unit);

  /* DFG to Semantic */
  void (*dfg2sem_init) (void);
  tf_sem_tree_t (*dfg2sem) (tf_pool_t pool, tf_sems_t sems, tf_dfg_t unit);


} tf_exports_t;

typedef void (*tf_exports_init_fn_t) (tf_exports_t *);

#ifdef __cplusplus
}
#endif

#endif /* __TF_DEFS_H__ */

/*
 * Local Variables:
 * mode:c
 * fill-column: 79
 * comment-column: 0
 * c-file-style: "gnu"
 * End:
 */

