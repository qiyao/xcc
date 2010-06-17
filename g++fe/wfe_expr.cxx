
/* 
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
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

#ifdef _WIN32
#define BITSPERBYTE 8
#else
#include <values.h>
#endif

#include "defs.h"
#include <cmplrs/rcodes.h>		// for RC_USER_ERROR
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"
#include "const.h"

#include "gnu_config.h"
#include "gnu/system.h"

#include "gnu/machmode.h"

extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
extern void warning (char*,...);	// from gnu
extern void error (char*,...);		// from gnu
#ifdef GPLUSPLUS_FE
#include "gnu/cp/cp-tree.h"
#endif /* GPLUSPLUS_FE */
extern int is_xtbool_type(tree type); // from wfe_misc.cxx
extern int is_xtbool1_type(tree type); // from wfe_misc.cxx
extern int fixed_type_p(tree type);  // from expr.c
}

#include "ir_reader.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "tree_symtab.h"
#include "wfe_decl.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "intrn_info.h"
#include "tie.h"

extern char * ABI_Name;

extern int flag_merge_constants;

extern BOOL xt_rw_str;        /* in config_targ_options.h */
//#define WFE_DEBUG

LABEL_IDX loop_expr_exit_label = 0; // exit label for LOOP_EXPRs

struct operator_from_tree_t {
  int      tree_code;
  char*    name;
  char     code;
  int      nkids;
  OPERATOR opr;
} Operator_From_Tree [] = {
  ERROR_MARK,              "error_mark",              'x', 0,  OPERATOR_UNKNOWN,
  IDENTIFIER_NODE,         "identifier_node",         'x', -1, OPERATOR_UNKNOWN,
  OP_IDENTIFIER,           "op_identifier",           'x', 2,  OPERATOR_UNKNOWN,
  TREE_LIST,               "tree_list",               'x', 2,  OPERATOR_UNKNOWN,
  TREE_VEC,                "tree_vec",                'x', 2,  OPERATOR_UNKNOWN,
  BLOCK,                   "block",                   'b', 0,  OPERATOR_UNKNOWN,
  VOID_TYPE,               "void_type",               't', 0,  OPERATOR_UNKNOWN,
#ifdef TENSILICA_CHANGES
  TIE_TYPE,                "tie_type",                't', 0,  OPERATOR_UNKNOWN,
#endif /* TENSILICA_CHANGES */
  INTEGER_TYPE,            "integer_type",            't', 0,  OPERATOR_UNKNOWN,
  REAL_TYPE,               "real_type",               't', 0,  OPERATOR_UNKNOWN,
  COMPLEX_TYPE,            "complex_type",            't', 0,  OPERATOR_UNKNOWN,
  ENUMERAL_TYPE,           "enumeral_type",           't', 0,  OPERATOR_UNKNOWN,
  BOOLEAN_TYPE,            "boolean_type",            't', 0,  OPERATOR_UNKNOWN,
  CHAR_TYPE,               "char_type",               't', 0,  OPERATOR_UNKNOWN,
  POINTER_TYPE,            "pointer_type",            't', 0,  OPERATOR_UNKNOWN,
  OFFSET_TYPE,             "offset_type",             't', 0,  OPERATOR_UNKNOWN,
  REFERENCE_TYPE,          "reference_type",          't', 0,  OPERATOR_UNKNOWN,
  METHOD_TYPE,             "method_type",             't', 0,  OPERATOR_UNKNOWN,
  FILE_TYPE,               "file_type",               't', 0,  OPERATOR_UNKNOWN,
  ARRAY_TYPE,              "array_type",              't', 0,  OPERATOR_UNKNOWN,
  SET_TYPE,                "set_type",                't', 0,  OPERATOR_UNKNOWN,
  RECORD_TYPE,             "record_type",             't', 0,  OPERATOR_UNKNOWN,
  UNION_TYPE,              "union_type",              't', 0,  OPERATOR_UNKNOWN,
  QUAL_UNION_TYPE,         "qual_union_type",         't', 0,  OPERATOR_UNKNOWN,
  FUNCTION_TYPE,           "function_type",           't', 0,  OPERATOR_UNKNOWN,
  LANG_TYPE,               "lang_type",               't', 0,  OPERATOR_UNKNOWN,
  INTEGER_CST,             "integer_cst",             'c', 2,  OPERATOR_UNKNOWN,
  REAL_CST,                "real_cst",                'c', 3,  OPERATOR_UNKNOWN,
  COMPLEX_CST,             "complex_cst",             'c', 3,  OPERATOR_UNKNOWN,
  STRING_CST,              "string_cst",              'c', 3,  OPERATOR_UNKNOWN,
  FUNCTION_DECL,           "function_decl",           'd', 0,  OPERATOR_UNKNOWN,
  LABEL_DECL,              "label_decl",              'd', 0,  OPERATOR_UNKNOWN,
  CONST_DECL,              "const_decl",              'd', 0,  OPERATOR_UNKNOWN,
  TYPE_DECL,               "type_decl",               'd', 0,  OPERATOR_UNKNOWN,
  VAR_DECL,                "var_decl",                'd', 0,  OPERATOR_UNKNOWN,
  PARM_DECL,               "parm_decl",               'd', 0,  OPERATOR_UNKNOWN,
  RESULT_DECL,             "result_decl",             'd', 0,  OPERATOR_UNKNOWN,
  FIELD_DECL,              "field_decl",              'd', 0,  OPERATOR_UNKNOWN,
  NAMESPACE_DECL,          "namespace_decl",          'd', 0,  OPERATOR_UNKNOWN,
  COMPONENT_REF,           "component_ref",           'r', 2,  OPERATOR_UNKNOWN,
  BIT_FIELD_REF,           "bit_field_ref",           'r', 3,  OPERATOR_UNKNOWN,
  INDIRECT_REF,            "indirect_ref",            'r', 1,  OPERATOR_UNKNOWN,
  BUFFER_REF,              "buffer_ref",              'r', 1,  OPERATOR_UNKNOWN,
  ARRAY_REF,               "array_ref",               'r', 2,  OPERATOR_UNKNOWN,
  CONSTRUCTOR,             "constructor",             'e', 2,  OPERATOR_UNKNOWN,
  COMPOUND_EXPR,           "compound_expr",           'e', 2,  OPERATOR_UNKNOWN,
  MODIFY_EXPR,             "modify_expr",             'e', 2,  OPERATOR_UNKNOWN,
  INIT_EXPR,               "init_expr",               'e', 2,  OPERATOR_UNKNOWN,
  TARGET_EXPR,             "target_expr",             'e', 4,  OPERATOR_UNKNOWN,
  COND_EXPR,               "cond_expr",               'e', 3,  OPERATOR_UNKNOWN,
  BIND_EXPR,               "bind_expr",               'e', 3,  OPERATOR_UNKNOWN,
  CALL_EXPR,               "call_expr",               'e', 3,  OPERATOR_UNKNOWN,
  METHOD_CALL_EXPR,        "method_call_expr",        'e', 4,  OPERATOR_UNKNOWN,
  WITH_CLEANUP_EXPR,       "with_cleanup_expr",       'e', 3,  OPERATOR_UNKNOWN,
  CLEANUP_POINT_EXPR,      "cleanup_point_expr",      'e', 1,  OPERATOR_UNKNOWN,
  PLACEHOLDER_EXPR,        "placeholder_expr",        'x', 0,  OPERATOR_UNKNOWN,
  WITH_RECORD_EXPR,        "with_record_expr",        'e', 2,  OPERATOR_UNKNOWN,
  PLUS_EXPR,               "plus_expr",               '2', 2,  OPR_ADD,
  MINUS_EXPR,              "minus_expr",              '2', 2,  OPR_SUB,
  MULT_EXPR,               "mult_expr",               '2', 2,  OPR_MPY,
  TRUNC_DIV_EXPR,          "trunc_div_expr",          '2', 2,  OPR_DIV,
  CEIL_DIV_EXPR,           "ceil_div_expr",           '2', 2,  OPR_DIV,
  FLOOR_DIV_EXPR,          "floor_div_expr",          '2', 2,  OPERATOR_UNKNOWN,
  ROUND_DIV_EXPR,          "round_div_expr",          '2', 2,  OPERATOR_UNKNOWN,
  TRUNC_MOD_EXPR,          "trunc_mod_expr",          '2', 2,  OPR_REM,
  CEIL_MOD_EXPR,           "ceil_mod_expr",           '2', 2,  OPERATOR_UNKNOWN,
  FLOOR_MOD_EXPR,          "floor_mod_expr",          '2', 2,  OPERATOR_UNKNOWN,
  ROUND_MOD_EXPR,          "round_mod_expr",          '2', 2,  OPERATOR_UNKNOWN,
  RDIV_EXPR,               "rdiv_expr",               '2', 2,  OPR_DIV,
  EXACT_DIV_EXPR,          "exact_div_expr",          '2', 2,  OPR_DIV,
  FIX_TRUNC_EXPR,          "fix_trunc_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FIX_CEIL_EXPR,           "fix_ceil_expr",           '1', 1,  OPERATOR_UNKNOWN,
  FIX_FLOOR_EXPR,          "fix_floor_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FIX_ROUND_EXPR,          "fix_round_expr",          '1', 1,  OPERATOR_UNKNOWN,
  FLOAT_EXPR,              "float_expr",              '1', 1,  OPERATOR_UNKNOWN,
  EXPON_EXPR,              "expon_expr",              '2', 2,  OPERATOR_UNKNOWN,
  NEGATE_EXPR,             "negate_expr",             '1', 1,  OPR_NEG,
  MIN_EXPR,                "min_expr",                '2', 2,  OPR_MIN,
  MAX_EXPR,                "max_expr",                '2', 2,  OPR_MAX,
  ABS_EXPR,                "abs_expr",                '1', 1,  OPR_ABS,
  FFS_EXPR,                "ffs_expr",                '1', 1,  OPERATOR_UNKNOWN,
  LSHIFT_EXPR,             "lshift_expr",             '2', 2,  OPR_SHL,
  RSHIFT_EXPR,             "rshift_expr",             '2', 2,  OPERATOR_UNKNOWN,
  LROTATE_EXPR,            "lrotate_expr",            '2', 2,  OPR_RROTATE,
  RROTATE_EXPR,            "rrotate_expr",            '2', 2,  OPR_RROTATE,
  BIT_IOR_EXPR,            "bit_ior_expr",            '2', 2,  OPR_BIOR,
  BIT_XOR_EXPR,            "bit_xor_expr",            '2', 2,  OPR_BXOR,
  BIT_AND_EXPR,            "bit_and_expr",            '2', 2,  OPR_BAND,
  BIT_ANDTC_EXPR,          "bit_andtc_expr",          '2', 2,  OPERATOR_UNKNOWN,
  BIT_NOT_EXPR,            "bit_not_expr",            '1', 1,  OPR_BNOT,
  TRUTH_ANDIF_EXPR,        "truth_andif_expr",        'e', 2,  OPR_CAND,
  TRUTH_ORIF_EXPR,         "truth_orif_expr",         'e', 2,  OPR_CIOR,
  TRUTH_AND_EXPR,          "truth_and_expr",          'e', 2,  OPR_BAND,
  TRUTH_OR_EXPR,           "truth_or_expr",           'e', 2,  OPR_BIOR,
  TRUTH_XOR_EXPR,          "truth_xor_expr",          'e', 2,  OPR_BXOR,
  TRUTH_NOT_EXPR,          "truth_not_expr",          'e', 1,  OPR_LNOT,
  LT_EXPR,                 "lt_expr",                 '<', 2,  OPR_LT,
  LE_EXPR,                 "le_expr",                 '<', 2,  OPR_LE,
  GT_EXPR,                 "gt_expr",                 '<', 2,  OPR_GT,
  GE_EXPR,                 "ge_expr",                 '<', 2,  OPR_GE,
  EQ_EXPR,                 "eq_expr",                 '<', 2,  OPR_EQ,
  NE_EXPR,                 "ne_expr",                 '<', 2,  OPR_NE,
  UNORDERED_EXPR,          "unordered_expr",          '<', 2,  OPERATOR_UNKNOWN,
  ORDERED_EXPR,            "ordered_expr",            '<', 2,  OPERATOR_UNKNOWN,
  UNLT_EXPR,               "unlt_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNLE_EXPR,               "unle_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNGT_EXPR,               "ungt_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNGE_EXPR,               "unge_expr",               '<', 2,  OPERATOR_UNKNOWN,
  UNEQ_EXPR,               "uneq_expr",               '<', 2,  OPERATOR_UNKNOWN,
  IN_EXPR,                 "in_expr",                 '2', 2,  OPERATOR_UNKNOWN,
  SET_LE_EXPR,             "set_le_expr",             '<', 2,  OPERATOR_UNKNOWN,
  CARD_EXPR,               "card_expr",               '1', 1,  OPERATOR_UNKNOWN,
  RANGE_EXPR,              "range_expr",              '2', 2,  OPERATOR_UNKNOWN,
  CONVERT_EXPR,            "convert_expr",            '1', 1,  OPERATOR_UNKNOWN,
  NOP_EXPR,                "nop_expr",                '1', 1,  OPERATOR_UNKNOWN,
  NON_LVALUE_EXPR,         "non_lvalue_expr",         '1', 1,  OPERATOR_UNKNOWN,
  SAVE_EXPR,               "save_expr",               'e', 3,  OPERATOR_UNKNOWN,
  UNSAVE_EXPR,             "unsave_expr",             'e', 1,  OPERATOR_UNKNOWN,
  RTL_EXPR,                "rtl_expr",                'e', 2,  OPERATOR_UNKNOWN,
  ADDR_EXPR,               "addr_expr",               'e', 1,  OPERATOR_UNKNOWN,
  REFERENCE_EXPR,          "reference_expr",          'e', 1,  OPERATOR_UNKNOWN,
  ENTRY_VALUE_EXPR,        "entry_value_expr",        'e', 1,  OPERATOR_UNKNOWN,
  COMPLEX_EXPR,            "complex_expr",            '2', 2,  OPR_PAIR,
  CONJ_EXPR,               "conj_expr",               '1', 1,  OPERATOR_UNKNOWN,
  REALPART_EXPR,           "realpart_expr",           '1', 1,  OPR_FIRSTPART,
  IMAGPART_EXPR,           "imagpart_expr",           '1', 1,  OPR_SECONDPART,
  PREDECREMENT_EXPR,       "predecrement_expr",       'e', 2,  OPR_SUB,
  PREINCREMENT_EXPR,       "preincrement_expr",       'e', 2,  OPR_ADD,
  POSTDECREMENT_EXPR,      "postdecrement_expr",      'e', 2,  OPR_SUB,
  POSTINCREMENT_EXPR,      "postincrement_expr",      'e', 2,  OPR_ADD,
  VA_ARG_EXPR,             "va_arg_expr",             'e', 1,  OPERATOR_UNKNOWN,
  TRY_CATCH_EXPR,          "try_catch_expr",          'e', 2,  OPERATOR_UNKNOWN,
  TRY_FINALLY_EXPR,        "try_finally",             'e', 2,  OPERATOR_UNKNOWN,
  GOTO_SUBROUTINE_EXPR,    "goto_subroutine",         'e', 2,  OPERATOR_UNKNOWN,
  POPDHC_EXPR,             "popdhc_expr",             's', 0,  OPERATOR_UNKNOWN,
  POPDCC_EXPR,             "popdcc_expr",             's', 0,  OPERATOR_UNKNOWN,
  LABEL_EXPR,              "label_expr",              's', 1,  OPERATOR_UNKNOWN,
  GOTO_EXPR,               "goto_expr",               's', 1,  OPERATOR_UNKNOWN,
  RETURN_EXPR,             "return_expr",             's', 1,  OPERATOR_UNKNOWN,
  EXIT_EXPR,               "exit_expr",               's', 1,  OPERATOR_UNKNOWN,
  LOOP_EXPR,               "loop_expr",               's', 1,  OPERATOR_UNKNOWN,
  LABELED_BLOCK_EXPR,      "labeled_block_expr",      'e', 2,  OPERATOR_UNKNOWN,
  EXIT_BLOCK_EXPR,         "exit_block_expr",         'e', 2,  OPERATOR_UNKNOWN,
  EXPR_WITH_FILE_LOCATION, "expr_with_file_location", 'e', 3,  OPERATOR_UNKNOWN,
  SWITCH_EXPR,             "switch_expr",             'e', 2,  OPERATOR_UNKNOWN,
  LAST_AND_UNUSED_TREE_CODE,"last_and_unused_tree_code",0, 0,  OPERATOR_UNKNOWN,
#ifdef GPLUSPLUS_FE
  OFFSET_REF,              "offset_ref",              'r', 2,  OPERATOR_UNKNOWN,
  PTRMEM_CST,              "ptrmem_cst",              'c', 2,  OPERATOR_UNKNOWN,
  NEW_EXPR,                "nw_expr",                 'e', 3,  OPERATOR_UNKNOWN,
  VEC_NEW_EXPR,            "vec_nw_expr",             'e', 3,  OPERATOR_UNKNOWN,
  DELETE_EXPR,             "dl_expr",                 'e', 2,  OPERATOR_UNKNOWN,
  VEC_DELETE_EXPR,         "vec_dl_expr",             'e', 2,  OPERATOR_UNKNOWN,
  SCOPE_REF,               "scope_ref",               'r', 2,  OPERATOR_UNKNOWN,
  MEMBER_REF,              "member_ref",              'r', 2,  OPERATOR_UNKNOWN,
  TYPE_EXPR,               "type_expr",               'e', 1,  OPERATOR_UNKNOWN,
  AGGR_INIT_EXPR,          "aggr_init_expr",          'e', 3,  OPERATOR_UNKNOWN,
  THROW_EXPR,              "throw_expr",              'e', 1,  OPERATOR_UNKNOWN,
  EMPTY_CLASS_EXPR,        "empty_class_expr",        'e', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_DECL,           "template_decl",           'd', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_PARM_INDEX,     "template_parm_index",     'x', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_TYPE_PARM,      "template_type_parm",      't', 0,  OPERATOR_UNKNOWN,
  TEMPLATE_TEMPLATE_PARM,  "template_template_parm",  't', 0,  OPERATOR_UNKNOWN,
  TYPENAME_TYPE,           "typename_type",           't', 0,  OPERATOR_UNKNOWN,
  TYPEOF_TYPE,             "typeof_type",             't', 0,  OPERATOR_UNKNOWN,
  USING_DECL,              "using_decl",              'd', 0,  OPERATOR_UNKNOWN,
  DEFAULT_ARG,             "default_arg",             'c', 2,  OPERATOR_UNKNOWN,
  TEMPLATE_ID_EXPR,        "template_id_expr",        'e', 2,  OPERATOR_UNKNOWN,
  CPLUS_BINDING,           "binding",                 'x', 2,  OPERATOR_UNKNOWN,
  OVERLOAD,                "overload",                'x', 1,  OPERATOR_UNKNOWN,
  WRAPPER,                 "wrapper",                 'x', 1,  OPERATOR_UNKNOWN,
  SRCLOC,                  "srcloc",                  'x', 2,  OPERATOR_UNKNOWN,
  LOOKUP_EXPR,             "lookup_expr",             'e', 1,  OPERATOR_UNKNOWN,
  FUNCTION_NAME,           "function_name",           'e', 0,  OPERATOR_UNKNOWN,
  MODOP_EXPR,              "modop_expr",              'e', 3,  OPERATOR_UNKNOWN,
  CAST_EXPR,               "cast_expr",               '1', 1,  OPERATOR_UNKNOWN,
  REINTERPRET_CAST_EXPR,   "reinterpret_cast_expr",   '1', 1,  OPERATOR_UNKNOWN,
  CONST_CAST_EXPR,         "const_cast_expr",         '1', 1,  OPERATOR_UNKNOWN,
  STATIC_CAST_EXPR,        "static_cast_expr",        '1', 1,  OPERATOR_UNKNOWN,
  DYNAMIC_CAST_EXPR,       "dynamic_cast_expr",       '1', 1,  OPERATOR_UNKNOWN,
  SIZEOF_EXPR,             "sizeof_expr",             '1', 1,  OPERATOR_UNKNOWN,
  ALIGNOF_EXPR,            "alignof_expr",            '1', 1,  OPERATOR_UNKNOWN,
  ARROW_EXPR,              "arrow_expr",              'e', 1,  OPERATOR_UNKNOWN,
  DOTSTAR_EXPR,            "dotstar_expr",            'e', 2,  OPERATOR_UNKNOWN,
  TYPEID_EXPR,             "typeid_expr",             'e', 1,  OPERATOR_UNKNOWN,
  PSEUDO_DTOR_EXPR,        "pseudo_dtor_expr",        'e', 3,  OPERATOR_UNKNOWN,
  EXPR_STMT,               "expr_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  COMPOUND_STMT,           "compound_stmt",           'e', 1,  OPERATOR_UNKNOWN,
  DECL_STMT,               "decl_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  IF_STMT,                 "if_stmt",                 'e', 3,  OPERATOR_UNKNOWN,
  FOR_STMT,                "for_stmt",                'e', 4,  OPERATOR_UNKNOWN,
  WHILE_STMT,              "while_stmt",              'e', 2,  OPERATOR_UNKNOWN,
  DO_STMT,                 "do_stmt",                 'e', 2,  OPERATOR_UNKNOWN,
  RETURN_STMT,             "return_stmt",             'e', 1,  OPERATOR_UNKNOWN,
  BREAK_STMT,              "break_stmt",              'e', 0,  OPERATOR_UNKNOWN,
  CONTINUE_STMT,           "continue_stmt",           'e', 0,  OPERATOR_UNKNOWN,
  SWITCH_STMT,             "switch_stmt",             'e', 2,  OPERATOR_UNKNOWN,
  GOTO_STMT,               "goto_stmt",               'e', 1,  OPERATOR_UNKNOWN,
  LABEL_STMT,              "label_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  ASM_STMT,                "asm_stmt",                'e', 5,  OPERATOR_UNKNOWN,
  SUBOBJECT,               "subobject",               'e', 1,  OPERATOR_UNKNOWN,
  CTOR_STMT,               "ctor_stmt",               'e', 0,  OPERATOR_UNKNOWN,
  CLEANUP_STMT,		   "cleanup_stmt",	      'e', 2,  OPERATOR_UNKNOWN,
  START_CATCH_STMT,        "start_catch_stmt",        'e', 0,  OPERATOR_UNKNOWN,
  SCOPE_STMT,              "scope_stmt",              'e', 1,  OPERATOR_UNKNOWN,
  CTOR_INITIALIZER,        "ctor_initializer",        'e', 2,  OPERATOR_UNKNOWN,
  CASE_LABEL,              "case_label",              'e', 2,  OPERATOR_UNKNOWN,
  RETURN_INIT,             "return_init",             'e', 2,  OPERATOR_UNKNOWN,
  TRY_BLOCK,               "try_block",               'e', 2,  OPERATOR_UNKNOWN,
  HANDLER,                 "handler",                 'e', 2,  OPERATOR_UNKNOWN,
  STMT_EXPR,               "stmt_expr",               'e', 1,  OPERATOR_UNKNOWN,
  TAG_DEFN,                "tag_defn",                'e', 0,  OPERATOR_UNKNOWN,
  IDENTITY_CONV,           "identity_conv",           'e', 1,  OPERATOR_UNKNOWN,
  LVALUE_CONV,             "lvalue_conv",             'e', 1,  OPERATOR_UNKNOWN,
  QUAL_CONV,               "qual_conv",               'e', 1,  OPERATOR_UNKNOWN,
  STD_CONV,                "std_conv",                'e', 1,  OPERATOR_UNKNOWN,
  PTR_CONV,                "ptr_conv",                'e', 1,  OPERATOR_UNKNOWN,
  PMEM_CONV,               "pmem_conv",               'e', 1,  OPERATOR_UNKNOWN,
  BASE_CONV,               "base_conv",               'e', 1,  OPERATOR_UNKNOWN,
  REF_BIND,                "ref_bind",                'e', 1,  OPERATOR_UNKNOWN,
  USER_CONV,               "user_conv",               'e', 2,  OPERATOR_UNKNOWN,
  AMBIG_CONV,              "ambig_conv",              'e', 1,  OPERATOR_UNKNOWN,
  RVALUE_CONV,             "rvalue_conv",             'e', 1,  OPERATOR_UNKNOWN,
  LAST_CPLUS_TREE_CODE,    "last_cplus_tree_code",     0,  0,  OPERATOR_UNKNOWN
#endif /* GPLUSPLUSFE */
};

// check whether the WHIRL operator has subsumed cvtl in its semantics
// (intended only for integer operations)
bool
Has_Subsumed_Cvtl(OPERATOR opr)
{
  if (OPERATOR_is_load(opr) || OPERATOR_is_leaf(opr))
    return TRUE;
  if (opr == OPR_CVT || opr == OPR_CVTL || opr == OPR_TAS)
    return TRUE;
  if (opr == OPR_EQ || opr == OPR_NE ||
      opr == OPR_GE || opr == OPR_GT ||
      opr == OPR_LE || opr == OPR_LT ||
      opr == OPR_LNOT || opr == OPR_LAND || opr == OPR_LIOR ||
      opr == OPR_CAND || opr == OPR_CIOR)
    return TRUE;
  return FALSE;
}

// Round up an object size to the size it would require in the parameter
// area on the stack.  This is defined to be the difference between its
// start address and the lowest possible starting address of the next parameter.
inline UINT64 Parameter_Size(UINT64 sz)
{
  if (Target_Byte_Sex == BIG_ENDIAN)
    return sz;
  else
    return (sz + UNITS_PER_WORD - 1) & ~(UNITS_PER_WORD - 1);
}

inline TYPE_ID
Widen_Mtype (TYPE_ID t)
{
  if (MTYPE_is_tie(t) || MTYPE_is_xtbool(t))
    return t;
  if (MTYPE_is_m(t))
    return t;
  if (MTYPE_is_void(t) || t == MTYPE_BS) {
    Fail_FmtAssertion ("Widen_Mtype: for MTYPE_V or MTYPE_BS");
    return t;
  }
  if (MTYPE_byte_size(t) >= 4)
    return t;
  return Mtype_TransferSize(MTYPE_I4, t);
}


static WN *
WFE_CreateImplicitIndirectLoad (TYPE_ID rtype, TYPE_ID desc,
				WN_OFFSET offset, ST *st,
				TY_IDX ty_idx, UINT field_id)
{
  WN *ldid = WN_CreateLdid(OPR_LDID, Pointer_Mtype, Pointer_Mtype,
			   0, ST_st_idx(st), ty_idx);

#if 0
  if (TY_kind(ty_idx) != KIND_POINTER)
  {
    fprintf(stderr, "LDID ***********\n");
    fdump_tree(stderr, ldid);
    fprintf(stderr, "ST ***********\n");
    Print_ST(stderr, st, TRUE);
    fprintf(stderr, "TY ***********\n");
    Print_TY(stderr, ty_idx);
  }
#endif
  
  return WN_CreateIload(OPR_ILOAD, rtype, desc, offset,
			TY_pointed(ty_idx), ty_idx,
			ldid, field_id);
}

static WN *
WFE_CreateImplicitIndirectStore (TYPE_ID desc, WN_OFFSET offset, ST *st,
				 TY_IDX ty_idx, WN *rhs_wn, UINT field_id)
{
  WN *ldid = WN_CreateLdid(OPR_LDID, Pointer_Mtype, Pointer_Mtype,
			   0, ST_st_idx(st), ty_idx);

#if 0
  if (TY_kind(ty_idx) != KIND_POINTER)
  {
    fprintf(stderr, "LDID ***********\n");
    fdump_tree(stderr, ldid);
    fprintf(stderr, "RHS  ***********\n");
    fdump_tree(stderr, rhs_wn);
    fprintf(stderr, "TY ***********\n");
    Print_TY(stderr, ty_idx);
  }
#endif

  return WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, offset,
			 ty_idx, rhs_wn, ldid, field_id);
}


// Traverse the tree to see if the address of a variable is being taken

void
WFE_Set_ST_Addr_Saved (WN *wn)
{
  OPERATOR  Operator;
  ST       *st;

  Operator = WN_operator (wn);

  switch ( Operator ) {

    case OPR_LDA:
    case OPR_LDMA:

      st = WN_st (wn);

      if (ST_class(st) == CLASS_VAR || ST_class(st) == CLASS_FUNC)
        Set_ST_addr_saved (st);
      break;

    case OPR_ARRAY:

      WFE_Set_ST_Addr_Saved (WN_kid0 (wn));
      break;

    case OPR_LDID:

      st = WN_st (wn);
      if (ST_pt_to_unique_mem (st))
        Clear_ST_pt_to_unique_mem (st);
      break;

    case OPR_CONST:
    case OPR_ILOAD:
    case OPR_MLOAD:
    case OPR_INTCONST:
    case OPR_INTRINSIC_OP:
    case OPR_CALL:
    case OPR_EQ:
    case OPR_NE:
    case OPR_GT:
    case OPR_GE:
    case OPR_LT:
    case OPR_LE:
    case OPR_ALLOCA:
      break;

    case OPR_EVAL:
    case OPR_TAS:
    case OPR_CVT:
    case OPR_CVTL:
    case OPR_NEG:
    case OPR_ABS:
    case OPR_SQRT:
    case OPR_REALPART:
    case OPR_IMAGPART:
    case OPR_PAREN:
    case OPR_RND:
    case OPR_TRUNC:
    case OPR_CEIL:
    case OPR_FLOOR:
    case OPR_BNOT:
    case OPR_LNOT:
    case OPR_LOWPART:
    case OPR_HIGHPART:
    case OPR_MINPART:
    case OPR_MAXPART:
    case OPR_RECIP:
    case OPR_RSQRT:
    case OPR_PARM:
    case OPR_EXTRACT_BITS:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    case OPR_CSELECT:

      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      WFE_Set_ST_Addr_Saved (WN_kid2(wn));
      break;

    case OPR_SELECT:
    case OPR_ADD:
    case OPR_SUB:
    case OPR_MPY:
    case OPR_DIV:
    case OPR_MOD:
    case OPR_REM:
    case OPR_DIVREM:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MINMAX:
    case OPR_BAND:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_BNOR:
    case OPR_LAND:
    case OPR_LIOR:
    case OPR_SHL:
    case OPR_ASHR:
    case OPR_LSHR:
    case OPR_COMPLEX:
    case OPR_HIGHMPY:
    case OPR_RROTATE:
    case OPR_COMPOSE_BITS:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_CAND:
    case OPR_CIOR:

      break;

    case OPR_COMMA:

      WFE_Set_ST_Addr_Saved (WN_kid1(wn));
      break;

    case OPR_RCOMMA:

      WFE_Set_ST_Addr_Saved (WN_kid0(wn));
      break;

    default:

      DevWarn ("WFE_Set_ST_Addr_Saved not implemented");
  }
} /* WFE_Set_ST_Addr_Saved */

#ifndef GPLUSPLUS_FE
typedef struct wfe_bind_expr_t {
  tree  rtl_expr;
  WN   *block;
} WFE_BIND_EXPR;

WFE_BIND_EXPR *wfe_bind_expr_stack       = NULL;
INT32          wfe_bind_expr_stack_last  = -1;
INT32          wfe_bind_expr_stack_max   = 0;

void
WFE_Expand_Start_Stmt_Expr (tree t)
{
  WN *block = WN_CreateBlock ();
  WFE_Stmt_Push (block, wfe_stmk_comma, Get_Srcpos ());
} /* WFE_Start_Stmt_Expr */

void
WFE_Expand_End_Stmt_Expr (tree t)
{
  WN *block = WFE_Stmt_Pop (wfe_stmk_comma);
  ++wfe_bind_expr_stack_last;
  if (wfe_bind_expr_stack_last == wfe_bind_expr_stack_max) {
    if (wfe_bind_expr_stack == NULL) {
      wfe_bind_expr_stack_max = 32;
      wfe_bind_expr_stack     =
        (WFE_BIND_EXPR *) malloc (wfe_bind_expr_stack_max *
                                  sizeof (WFE_BIND_EXPR));
    }
    else {
      wfe_bind_expr_stack_max = wfe_bind_expr_stack_max +
                                (wfe_bind_expr_stack_max >> 1);
      wfe_bind_expr_stack     =
        (WFE_BIND_EXPR *) realloc (wfe_bind_expr_stack,
                                   wfe_bind_expr_stack_max *
                                   sizeof (WFE_BIND_EXPR));
    }
  }
  wfe_bind_expr_stack [wfe_bind_expr_stack_last].rtl_expr = t;
  wfe_bind_expr_stack [wfe_bind_expr_stack_last].block    = block;
} /* WFE_End_Stmt_Expr */
#endif /* GPLUSPLUS_FE */

typedef struct wfe_save_expr_t {
  tree  exp;
  ST   *st;
} WFE_SAVE_EXPR;

WFE_SAVE_EXPR *wfe_save_expr_stack      = NULL;
INT32          wfe_save_expr_stack_last = -1;
INT32          wfe_save_expr_stack_max  = 0;

static WN*
WFE_Save_Expr (tree save_exp,
               bool need_result,
               TY_IDX nop_ty_idx,
               TY_IDX component_ty_idx,
               INT64 component_offset,
               UINT16 field_id)
{
  INT32     i;
  tree      exp     = TREE_OPERAND (save_exp, 0);
  TY_IDX    ty_idx  = Get_TY (TREE_TYPE (exp));
  TYPE_ID   mtype   = TY_mtype (ty_idx);
  ST       *st;
  WN       *wn;
  bool     found = false;  

  for (i = wfe_save_expr_stack_last; i >= 0; i--) {
    if (wfe_save_expr_stack [i].exp == save_exp) {
      st = wfe_save_expr_stack [i].st;
      FmtAssert (st != 0,
                 ("WFE_Save_Expr: st not yet assigned"));
      found = true;
      break;
    }
  }
  
  if (!found) {
    i = ++wfe_save_expr_stack_last;
    if (i == wfe_save_expr_stack_max) {
      if (wfe_save_expr_stack == NULL) {
        wfe_save_expr_stack_max = 32;
        wfe_save_expr_stack     =
          (WFE_SAVE_EXPR *) malloc (wfe_save_expr_stack_max *
                                    sizeof (WFE_SAVE_EXPR));
      }
      else {
        wfe_save_expr_stack_max = wfe_save_expr_stack_max +
                                  (wfe_save_expr_stack_max >> 1);
        wfe_save_expr_stack     =
          (WFE_SAVE_EXPR *) realloc (wfe_save_expr_stack,
                                     wfe_save_expr_stack_max *
                                     sizeof (WFE_SAVE_EXPR));
      }
    }
    wfe_save_expr_stack [i].exp = save_exp;
    wfe_save_expr_stack [i].st  = 0;
    wn = WFE_Expand_Expr (exp);
    st = Gen_Temp_Symbol (ty_idx, "__save_expr");
    WFE_Set_ST_Addr_Saved (wn);
    wn = WN_Stid (mtype, 0, st, ty_idx, wn);
    WFE_Stmt_Append (wn, Get_Srcpos());
    wfe_save_expr_stack [i].st = st;
  }

#if 0
  fprintf(stderr, "exp %x, st %x\n",
	  wfe_save_expr_stack [i].exp,
	  wfe_save_expr_stack [i].st);    
  Print_ST(stderr, wfe_save_expr_stack [i].st, TRUE);
  debug_tree_limit(wfe_save_expr_stack [i].exp, 100);
#endif
  
  if (component_ty_idx == 0)
    wn = WN_Ldid (mtype, 0, st, ty_idx);
  else {
    TYPE_ID desc  = TY_mtype(component_ty_idx);
    TYPE_ID rtype = Widen_Mtype(desc);
    wn = WN_CreateLdid(OPR_LDID, rtype, desc, component_offset, st,
		     field_id? ty_idx : component_ty_idx, field_id);  
  }
  return wn;
} /* WFE_Save_Expr */

/* Evalutate 'exp' and store into 'target'. */
static WN *
WFE_Store_Expr (tree exp, ST *target)
{
  /* 'target' must be an MTYPE_M symbol, or the symbol representing
     the invisible first arg used to return function results. */
  Is_True(target, ("expecting non-NULL target for WFE_Store_Expr"));
  Is_True((target == Current_Function_Result_ST()) ||
	  (ST_mtype(target) == MTYPE_M),
	  ("expecting target to be MTYPE_M or return decl"));
  
  if (TREE_CODE (exp) == COMPOUND_EXPR)
  {
    /* Assign the second part of the COMPOUND_EXPR to 'target', and
       then evaluate the first part. */
    tree res = TREE_OPERAND (exp, 1);
    FmtAssert((TREE_CODE (res) == VAR_DECL) || (TREE_CODE (res) == PARM_DECL),
	      ("expecting store_expr or compound_expr to be var or parm decl"));
    FmtAssert((DECL_ST(res) == NULL) || (DECL_ST(res) == target),
	      ("st already assigned in store_expr"));

    DECL_ST(res) = target;
    WFE_Stmt_Append(WN_CreateEval(WFE_Expand_Expr (TREE_OPERAND (exp, 0))),
		    Get_Srcpos());
    return NULL;
  }
  else if (TREE_CODE (exp) == COND_EXPR)
  {
    /* For conditional expression, evaluate each arm into 'target'.
       This avoids the creation of unnecessary temporaries (and
       potentially erroneous object copies that don't use the copy
       constructor). */

    Push_Conditional_Cleanup_Level();
    WN *wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type, NULL);
    Pop_Conditional_Cleanup_Level();

    WN *then_block = WN_CreateBlock ();
    WN *else_block = WN_CreateBlock ();
    WN *if_stmt    = WN_CreateIf(wn0, then_block, else_block);

    WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos());
    Push_Conditional_Cleanup_Level();
    WN *wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						   MTYPE_V, target);
    if (wn1)
    {
      wn1 = WN_CreateEval (wn1);
      WFE_Stmt_Append (wn1, Get_Srcpos());
    }
    Pop_Conditional_Cleanup_Level();
    WFE_Stmt_Pop (wfe_stmk_if_then);

    WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
    Push_Conditional_Cleanup_Level();
    WN *wn2 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 2),
						   MTYPE_V, target);
    if (wn2)
    {
      wn2 = WN_CreateEval (wn2);
      WFE_Stmt_Append (wn2, Get_Srcpos());
    }
    Pop_Conditional_Cleanup_Level();
    WFE_Stmt_Pop (wfe_stmk_if_else);

    WFE_Stmt_Append (if_stmt, Get_Srcpos());
    return NULL;
  }

  WN *wn = WFE_Expand_Expr (exp, target);

  /* If 'wn' is just a load of 'target', then we don't need to store
     it, so just EVAL it and return NULL. */
  if (wn)
  {
    if (WN_operator(wn) == OPR_COMMA)
      wn = WN_kid1(wn);
    
    if ((WN_operator(wn) == OPR_LDID) && (WN_st(wn) == target))
    {
      WFE_Stmt_Append (WN_CreateEval (wn), Get_Srcpos());
      return NULL;
    }
  }

  return wn;
}

/* process the tree doing array indicing and return the WN that performs
 * the address computation; ty_idx returns the high-level array type if it
 * is a DECL node, and the element type if it is an ARRAY_REF node.
 */
static WN *
WFE_Array_Expr(tree exp, 
	       TY_IDX *ty_idx, 
	       TY_IDX component_ty_idx,
	       INT64 component_offset,
	       UINT32 field_id)
{
  WN *wn;
  enum tree_code code = TREE_CODE (exp);
  if (code == COMPONENT_REF) {
    TY_IDX ty_idx0;
    tree arg0 = TREE_OPERAND(exp, 0); 
    tree arg1 = TREE_OPERAND(exp, 1); 
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(TREE_TYPE(exp));
    else ty_idx0 = component_ty_idx;
    // for g++ ensure that the WHIRL type for the enclosing structure has been
    // created in order to set the field id to a non zero value
    (void) Get_TY (TREE_TYPE (arg0));
    Is_True(! DECL_BIT_FIELD(arg1),
	    ("WFE_Array_Expr: address arithmetic cannot use bitfield addresses"));
    INT64 ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			      / BITSPERBYTE;
    return WFE_Array_Expr(arg0, ty_idx, ty_idx0, ofst + component_offset,
			  field_id + DECL_FIELD_ID(arg1));
  }
  else if (code == VAR_DECL || code == PARM_DECL) {
    ST *st = Get_ST (exp);
    ST *base_st = ST_base (st);
    // for VLAs the instead of using the ST use its base st
    if (st != base_st) {
      wn = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
      if (component_offset != 0) {
	WN *wn0 = WN_Intconst(MTYPE_U4, component_offset);
	wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, wn0);
      }
    }
    else
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }

    if (TY_is_volatile(ST_type(st)))
      Set_TY_is_volatile(*ty_idx);

    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == CONSTRUCTOR) {
    ST *st = WFE_Generate_Temp_For_Initialized_Aggregate (exp, "");
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else *ty_idx = component_ty_idx;
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == STRING_CST) {
    wn = WFE_Expand_Expr(exp);
    *ty_idx = ST_type (TREE_STRING_ST (exp));
    return wn;
  }
  else if (code == INDIRECT_REF) {
    wn = WFE_Expand_Expr(TREE_OPERAND (exp, 0));
    if (component_ty_idx == 0)
      *ty_idx = Get_TY (TREE_TYPE(exp));
    else {
      *ty_idx = component_ty_idx;
      INT node_align = TYPE_ALIGN(TREE_TYPE(exp)) / BITSPERBYTE;
      if (node_align < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, node_align);//pick more stringent align
    }

    if (TREE_THIS_VOLATILE(exp))
      Set_TY_is_volatile(*ty_idx);

    if (component_offset != 0) { // TODO: use ILDA instead
      WN *wn0 = WN_Intconst(MTYPE_I4, component_offset);
      wn = WN_Binary(OPR_ADD, Pointer_Mtype, wn, wn0);
    }
    return wn;
  }
  else if (code == CALL_EXPR) {
    wn = WFE_Expand_Expr(exp);
    FmtAssert (WN_opcode (wn) == OPC_MCOMMA,
               ("opcode other than OPC_MCOMMA for call underneath ARRAY_REF"));
    WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
    ST *st = WN_st (WN_kid1 (wn));
    WN_Delete (WN_kid1 (wn));
    WN_Delete (wn);
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));//pick more stringent align
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else if (code == ARRAY_REF) { // recursive call
    WN *wn0, *wn1, *wn2;
    TY_IDX ty_idx0;
    wn0 = WFE_Array_Expr(TREE_OPERAND (exp, 0), &ty_idx0, component_ty_idx,
			 component_offset, field_id);
    Is_True(TY_kind(ty_idx0) == KIND_ARRAY,
	    ("WFE_Array_Expr: arg 0 of ARRAY_REF not of type KIND_ARRAY"));
    ARB_HANDLE arb = TY_arb(ty_idx0);
    if (ARB_dimension(arb) == 1 && 
	ARB_first_dimen(arb) && ARB_last_dimen(arb) &&
	ARB_const_lbnd(arb)) {
      if (ARB_const_ubnd(arb))
        wn1 = WN_Intconst(MTYPE_I4, ARB_ubnd_val(arb) - ARB_lbnd_val(arb) + 1);
      else if (ARB_ubnd_var(arb)==ST_IDX_ZERO)
	wn1 = WN_Intconst(MTYPE_I4, 0);
      else {
	ST* st = ST_ptr(ARB_ubnd_var (arb));
	TY_IDX ty_idx = ST_type(st);
	wn1 = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
      }

      wn2 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
      wn = WN_Ternary(OPR_ARRAY, Pointer_Mtype, wn0, wn1, wn2);
      WN_element_size(wn) = TY_size(Get_TY (TREE_TYPE(exp)));
    }
    else Is_True(FALSE,
		 ("WFE_Array_Expr: only const-bounds 1-dimension arrays handled now"));
    if (component_ty_idx == 0) {
      *ty_idx = TY_etype(ty_idx0);
      if (TY_align(ty_idx0) < TY_align(*ty_idx))
	Set_TY_align(*ty_idx, TY_align(ty_idx0));// pick more stringent align
    }
    else *ty_idx = component_ty_idx;

    if (TY_is_volatile(ty_idx0))
      Set_TY_is_volatile(*ty_idx);

    return wn;
  }
  else if (code == TARGET_EXPR) {
    wn = WFE_Expand_Expr (exp, NULL, true, 0, 
                          component_ty_idx, component_offset, field_id);
    ST *st = WN_st (wn);
    WN_Delete (wn);
    wn = WN_Lda (Pointer_Mtype, ST_ofst(st)+component_offset, st, field_id);
    if (component_ty_idx == 0)
      *ty_idx = ST_type(st);
    else {
      *ty_idx = component_ty_idx;
      if (TY_align(ST_type(st)) < TY_align(component_ty_idx))
	Set_TY_align(*ty_idx, TY_align(ST_type(st)));
    }
    Is_True(TY_kind(*ty_idx) == KIND_ARRAY,
	    ("WFE_Array_Expr: ARRAY_REF base not of type KIND_ARRAY"));
    return wn;
  }
  else {
    Is_True(FALSE,
	    ("WFE_Array_Expr: unsupported node for base of ARRAY_REF"));
    return NULL;
  }
}


/* rhs_wn is the WN representing the rhs of a MODIFY_EXPR node; this
 * routine processes the lhs of the node and generate the appropriate
 * form of store.
 *
 * In special cases where the RHS of the store is unknown but the
 * statement being expanded is nonetheless semantically a store,
 * rhs_wn can be NULL. This happens, for example, for each output
 * operand of an asm statement. When rhs_wn is NULL, we manufacture an
 * RHS that is an LDID of a PREG specified by rhs_preg_num (generally
 * a negative-numbered PREG). If rhs_st is non-NULL, rhs_preg_num is
 * ignored.  assign_code tells if it is {PRE,POST}{IN,DE}CREMENT_EXPR.
 * Ordinarily, it is MODIFY_EXPR.
 */
WN *
WFE_Lhs_Of_Modify_Expr(tree_code assign_code,
		       tree lhs, 
		       bool need_result,
		       TY_IDX component_ty_idx, 
		       INT64 component_offset,
		       UINT32 field_id,
		       bool is_bit_field,
		       WN *rhs_wn,
		       PREG_NUM rhs_preg_num,
		       bool is_realpart,
		       bool is_imagpart)
{
  WN *wn;
  ST *st;
  bool result_in_temp = FALSE;
  ST *result_preg_st;
  PREG_NUM result_preg;
  PREG_NUM lhs_preg_num = 0;
  enum tree_code code = TREE_CODE (lhs);
  BOOL volt = FALSE;
  if (rhs_wn != NULL) {
    WFE_Set_ST_Addr_Saved (rhs_wn);
  }
  if (code == COMPONENT_REF) {
    INT64 ofst;
    TY_IDX ty_idx0;

    tree arg0 = TREE_OPERAND(lhs, 0);
    tree arg1 = TREE_OPERAND(lhs, 1);
#ifdef GPLUSPLUS_FE
    // for g++ ensure that the WHIRL type for the enclosing structure has been
    // created in order to set the field id to a non zero value
    (void) Get_TY (TREE_TYPE (arg0));
#endif /* GPLUSPLUS_FE */
    if (component_ty_idx == 0)
      ty_idx0 = Get_TY(TREE_TYPE(lhs));
    else ty_idx0 = component_ty_idx;
    if (DECL_BIT_FIELD(arg1)) 
      is_bit_field = TRUE;
    if (! is_bit_field)
      ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
			    Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			  / BITSPERBYTE;
    else ofst = 0;
    if (TREE_THIS_VOLATILE(lhs))
      Set_TY_is_volatile(ty_idx0);
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0, 
				ofst+component_offset,
			        field_id + DECL_FIELD_ID(arg1), is_bit_field, 
				rhs_wn, rhs_preg_num, is_realpart, is_imagpart);
    return wn;
  }

  if (code == REALPART_EXPR) {
    tree arg0 = TREE_OPERAND(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
				component_offset, field_id, is_bit_field,
				rhs_wn, rhs_preg_num, TRUE, FALSE);
    return wn;
  }

  if (code == IMAGPART_EXPR) {
    tree arg0 = TREE_OPERAND(lhs, 0);
    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(arg0));
    wn = WFE_Lhs_Of_Modify_Expr(assign_code, arg0, need_result, ty_idx0,
				component_offset, field_id, is_bit_field,
				rhs_wn, rhs_preg_num, FALSE, TRUE);
    return wn;
  }

  if (code == PARM_DECL || code == VAR_DECL || code == RESULT_DECL || code == TARGET_EXPR) {

    // for target expression, expand it first before the assignment
    if (code == TARGET_EXPR) {
      WFE_Expand_Expr (lhs);
      lhs = TREE_OPERAND(lhs, 0);
    }
    TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(lhs)); // type associated with field_id
    if (TREE_THIS_VOLATILE(lhs) || TY_is_volatile(component_ty_idx)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }
    st = Get_ST (lhs);
    if (ST_assigned_to_dedicated_preg (st)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
    Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	    ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));

    TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
    TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

    if (rhs_wn == NULL) {
      // Manufacture a negative-PREG RHS for the STID we are about to
      // generate. This feature is used in preparing WHIRL ASM
      // statements.
      // TODO: How to support a bit-field output of non-integral
      // number of bytes?
      if (rtype == MTYPE_M && desc == MTYPE_M) {
        if ( TY_size(desc_ty_idx) != MTYPE_byte_size(Def_Int_Mtype)) {
          error ("Size of ASM struct opnd must be equal to register size");
          //exit (RC_USER_ERROR);
        } else {
          desc = rtype = Def_Int_Mtype;
          desc_ty_idx = hi_ty_idx = MTYPE_To_TY(Def_Int_Mtype);
        }
      }
      ST *rhs_st = MTYPE_To_PREG(desc);
      rhs_wn = WN_CreateLdid (OPR_LDID, rtype,
			      desc, rhs_preg_num, rhs_st,
			      desc_ty_idx, 0);
    }
    else {
      WN *result_wn;	// the result wn to be returned
      if (assign_code == MODIFY_EXPR) {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Unary(OPR_IMAGPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateLdid(OPR_LDID, rtype, desc,
						    ST_ofst(st) + component_offset,
						    st, hi_ty_idx, field_id)));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Unary(OPR_REALPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateLdid(OPR_LDID, rtype, desc,
						    ST_ofst(st) + component_offset,
						    st, hi_ty_idx, field_id)),
			     rhs_wn);
      }
      else {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
			     rhs_wn);
      }

      if (assign_code == PREINCREMENT_EXPR ||
	  assign_code == PREDECREMENT_EXPR) {
        if (ST_implicit_indirect(st)) {
	   if (field_id == 0)
	     rtype = desc = MTYPE_M;

	   wn = WFE_CreateImplicitIndirectLoad(rtype, desc,
			ST_ofst(st)+component_offset, st,
			hi_ty_idx , field_id);
	} else {
          wn = WN_CreateLdid (OPR_LDID, rtype, desc, 
			    ST_ofst(st) + component_offset,
			    st, hi_ty_idx, field_id);
	}
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		           rtype, wn, rhs_wn);
	result_wn = rhs_wn;
      }
      else if (assign_code == POSTINCREMENT_EXPR ||
	       assign_code == POSTDECREMENT_EXPR) {
          if (ST_implicit_indirect(st)) {
	     if (field_id == 0)
	        rtype = desc = MTYPE_M;

	      result_wn = WFE_CreateImplicitIndirectLoad(rtype, desc,
			ST_ofst(st)+component_offset, st,
			hi_ty_idx, field_id);

	  } else {
            result_wn = WN_CreateLdid (OPR_LDID, rtype, desc, 
				   ST_ofst(st) + component_offset,
				   st, hi_ty_idx, field_id);
          }
      }
      else result_wn = rhs_wn;

      if (need_result && 
	  (volt ||
	   assign_code == POSTINCREMENT_EXPR ||
	   assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
        result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == POSTINCREMENT_EXPR ||
	  assign_code == POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
		           rtype, result_wn, rhs_wn);
      }
      else rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WFE_Keep_Zero_Length_Structs &&
        desc == MTYPE_M               &&
        TY_size (hi_ty_idx) == 0) {
      // ignore zero length structs
    }
    else {
      /* If accesses to 'st' have an implicit indirection, then we
	 must use an ISTORE. If 'field_id' is 0 then we are storing
	 the entire object, so use MTYPE_M. */
      if (ST_implicit_indirect(st))
	wn = WFE_CreateImplicitIndirectStore((field_id == 0) ? MTYPE_M : desc,
					     ST_ofst(st) + component_offset + lhs_preg_num,
					     st,
					     hi_ty_idx, rhs_wn, field_id);
      else
	wn = WN_Stid (desc, ST_ofst(st) + component_offset + lhs_preg_num, st,
		      hi_ty_idx, rhs_wn, field_id);

      WFE_Stmt_Append(wn, Get_Srcpos());
    }
    if (need_result) {
      if (! result_in_temp)
        wn = WN_CreateLdid(OPR_LDID, rtype, desc, 
			   ST_ofst(st) + component_offset, st, hi_ty_idx,
			   field_id);
      else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      if (is_realpart)
	wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
      else
      if (is_imagpart)
	wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
    }
    else wn = NULL;
  }
  else if (code == INDIRECT_REF) {
    TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(lhs));
    if (TREE_THIS_VOLATILE(lhs) || TY_is_volatile(component_ty_idx)) {
      Set_TY_is_volatile(hi_ty_idx);
      volt = TRUE;
    }
    tree op = TREE_OPERAND(lhs, 0);
    WN *addr_wn = WFE_Expand_Expr (TREE_OPERAND (lhs, 0));
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = hi_ty_idx;
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }
    Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	    ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
    if (WN_has_side_effects(addr_wn) &&
	(need_result || 
	 assign_code == PREINCREMENT_EXPR ||
	 assign_code == PREDECREMENT_EXPR ||
	 assign_code == POSTINCREMENT_EXPR ||
	 assign_code == POSTDECREMENT_EXPR)) {
      ST       *preg_st;
      PREG_NUM  preg;
      TY_IDX    address_ty_idx = Get_TY (TREE_TYPE (TREE_OPERAND (lhs, 0)));
      preg_st = MTYPE_To_PREG(Pointer_Mtype);
      preg    = Create_Preg (Pointer_Mtype, NULL);
      wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
      WFE_Set_ST_Addr_Saved (addr_wn);
      WFE_Stmt_Append (wn, Get_Srcpos());
      addr_wn = WN_Ldid (Pointer_Mtype, preg, preg_st, address_ty_idx);
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
    TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

    if (rhs_wn == NULL) {
      // Manufacture a negative-PREG RHS for the ISTORE we are about to
      // generate. This feature is used in preparing WHIRL ASM
      // statements.
      ST *rhs_st;
      // TODO: How to support a bit-field output of non-integral
      // number of bytes?
      rhs_st = MTYPE_To_PREG(desc);
      // Types are likely to be wrong in the following
      rhs_wn = WN_CreateLdid (OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
			      desc_ty_idx, 0);
    }
    else {
      WN *result_wn;	// the result wn to be returned

      if (assign_code == MODIFY_EXPR) {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Unary(OPR_IMAGPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateIload(OPR_ILOAD, rtype, desc,
						     component_offset,
						     field_id != 0 ? hi_ty_idx : desc_ty_idx,
						     Make_Pointer_Type(hi_ty_idx, FALSE),
						     WN_COPY_Tree (addr_wn),
						     field_id)));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Unary(OPR_REALPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateIload(OPR_ILOAD, rtype, desc,
						     component_offset,
						     field_id != 0 ? hi_ty_idx : desc_ty_idx,
						     Make_Pointer_Type(hi_ty_idx, FALSE),
						     WN_COPY_Tree (addr_wn),
						     field_id)),
			     rhs_wn);
      }
      else {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
			     rhs_wn);
      }

      if (assign_code == PREINCREMENT_EXPR ||
	  assign_code == PREDECREMENT_EXPR) {
        wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
			     field_id != 0 ? hi_ty_idx : desc_ty_idx,
			     Make_Pointer_Type(hi_ty_idx, FALSE),
			     WN_COPY_Tree (addr_wn),
			     field_id);
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                           rtype, wn, rhs_wn);
        result_wn = rhs_wn;
      }
      else if (assign_code == POSTINCREMENT_EXPR ||
	       assign_code == POSTDECREMENT_EXPR) {
	result_wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				    field_id != 0 ? hi_ty_idx : desc_ty_idx,
				    Make_Pointer_Type(hi_ty_idx, FALSE),
				    WN_COPY_Tree (addr_wn),
				    field_id);
      }
      else result_wn = rhs_wn;

      if (need_result && 
	  (volt ||
           assign_code == POSTINCREMENT_EXPR ||
           assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
	result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == POSTINCREMENT_EXPR ||
	  assign_code == POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                           rtype, result_wn, rhs_wn);
      }
      else rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WFE_Keep_Zero_Length_Structs &&
        desc == MTYPE_M               &&
        TY_size (hi_ty_idx) == 0) {
      // ignore zero length structs
      if (WN_has_side_effects (addr_wn)) {
	wn = WN_CreateEval (addr_wn);
	WFE_Stmt_Append (wn, Get_Srcpos());
      }
      wn = NULL;
    }
    else {
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			   Make_Pointer_Type (hi_ty_idx, FALSE),
			   rhs_wn, addr_wn, field_id);
      WFE_Stmt_Append(wn, Get_Srcpos());
      if (need_result) {
	if (! result_in_temp)
          wn = WN_CreateIload(OPR_ILOAD, rtype, desc, component_offset,
			      field_id != 0 ? hi_ty_idx : desc_ty_idx,
			      Make_Pointer_Type (hi_ty_idx, FALSE),
			      WN_COPY_Tree (addr_wn),
			      field_id);
	else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
	if (is_realpart)
	  wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
	else
	if (is_imagpart)
	  wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
      }
      else wn = NULL;
    }
  }
  else if (code == ARRAY_REF) {
    TY_IDX elem_ty_idx;
    // generate the WHIRL array node
    WN *addr_wn = WFE_Array_Expr(lhs, &elem_ty_idx, 0, 0, 0);
    if (TREE_THIS_VOLATILE(lhs))
      Set_TY_is_volatile(elem_ty_idx);
    if (TY_is_volatile(elem_ty_idx))
      volt = TRUE;
    TY_IDX desc_ty_idx = component_ty_idx;
    if (desc_ty_idx == 0)
      desc_ty_idx = Get_TY (TREE_TYPE(lhs));
    if (TY_is_volatile(desc_ty_idx)) {
      Clear_TY_is_volatile(desc_ty_idx);
      volt = TRUE;
    }
    Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
	    ("WFE_Lhs_Of_Modify_Expr: field id for bit-field exceeds limit"));
    if (WN_has_side_effects(addr_wn) &&
        (need_result ||
         assign_code == PREINCREMENT_EXPR ||
         assign_code == PREDECREMENT_EXPR ||
         assign_code == POSTINCREMENT_EXPR ||
	 assign_code == POSTDECREMENT_EXPR)) {
      ST       *preg_st;
      PREG_NUM  preg;
      TY_IDX    address_ty_idx = Make_Pointer_Type(elem_ty_idx, FALSE);
      preg_st = MTYPE_To_PREG(Pointer_Mtype);
      preg    = Create_Preg (Pointer_Mtype, NULL);
      wn      = WN_Stid (Pointer_Mtype, preg, preg_st, address_ty_idx, addr_wn);
      WFE_Set_ST_Addr_Saved (addr_wn);
      WFE_Stmt_Append (wn, Get_Srcpos());
      addr_wn = WN_Ldid (Pointer_Mtype, preg, preg_st, address_ty_idx);
    }

    TYPE_ID rtype = Widen_Mtype(TY_mtype(desc_ty_idx));
    TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(desc_ty_idx);

    if (rhs_wn == NULL) {
      // Manufacture a negative-PREG RHS for the ISTORE we are about to
      // generate. This feature is used in preparing WHIRL ASM
      // statements.
      ST *rhs_st;
      // TODO: How to support a bit-field output of non-integral
      // number of bytes?
      rhs_st = MTYPE_To_PREG(desc);
      rhs_wn = WN_CreateLdid (OPR_LDID, rtype, desc, rhs_preg_num, rhs_st,
			      desc_ty_idx, 0);
    }
    else {
      WN *result_wn;    // the result wn to be returned

      if (assign_code == MODIFY_EXPR) {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Unary(OPR_IMAGPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateIload(OPR_ILOAD, rtype, desc,
						     component_offset,
						     field_id != 0 ? elem_ty_idx : desc_ty_idx,
						     Make_Pointer_Type(elem_ty_idx, FALSE),
						     WN_COPY_Tree (addr_wn),
						     field_id)));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Unary(OPR_REALPART,
				      Mtype_complex_to_real (rtype),
				      WN_CreateIload(OPR_ILOAD, rtype, desc,
						     component_offset,
						     field_id != 0 ? elem_ty_idx : desc_ty_idx,
						     Make_Pointer_Type(elem_ty_idx, FALSE),
						     WN_COPY_Tree (addr_wn),
						     field_id)),
			     rhs_wn);
      }
      else {
	if (is_realpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     rhs_wn,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0));
	else
	if (is_imagpart)
	  rhs_wn = WN_Binary(OPR_COMPLEX, rtype,
			     WN_Floatconst (Mtype_complex_to_real (rtype), 0.0),
			     rhs_wn);
      }

      if (assign_code == PREINCREMENT_EXPR ||
          assign_code == PREDECREMENT_EXPR) {
        wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
                             field_id != 0 ? elem_ty_idx : desc_ty_idx,
                             Make_Pointer_Type(elem_ty_idx, FALSE),
                             WN_COPY_Tree (addr_wn),
                             field_id);
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                           rtype, wn, rhs_wn);
	result_wn = rhs_wn;
      }
      else if (assign_code == POSTINCREMENT_EXPR ||
	       assign_code == POSTDECREMENT_EXPR) {
        result_wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
				    field_id != 0 ? elem_ty_idx : desc_ty_idx,
				    Make_Pointer_Type(elem_ty_idx, FALSE),
				    WN_COPY_Tree (addr_wn),
				    field_id);
      }
      else result_wn = rhs_wn;

      if (need_result && 
	  (volt ||
           assign_code == POSTINCREMENT_EXPR ||
	   assign_code == POSTDECREMENT_EXPR)) { // save result in a preg
        result_in_temp = TRUE;
        result_preg_st = MTYPE_To_PREG(rtype);
        result_preg = Create_Preg(rtype, NULL);
        wn = WN_Stid(rtype, result_preg, result_preg_st, desc_ty_idx, result_wn,
		     0);
        WFE_Stmt_Append (wn, Get_Srcpos());;
        result_wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
      }

      if (assign_code == POSTINCREMENT_EXPR ||
          assign_code == POSTDECREMENT_EXPR) {
        rhs_wn = WN_Binary(Operator_From_Tree [assign_code].opr,
                           rtype, result_wn, rhs_wn);
      }
      else rhs_wn = result_wn;

      // rhs_wn is now always the right-hand-side of the assignment
    }

    // the assignment
    if (!WFE_Keep_Zero_Length_Structs &&
        desc == MTYPE_M               &&
        TY_size (elem_ty_idx) == 0) {
      // ignore zero length structs
      if (WN_has_side_effects (addr_wn)) {
        wn = WN_CreateEval (addr_wn);
        WFE_Stmt_Append (wn, Get_Srcpos());
      }
      wn = NULL;
    }
    else {
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, desc, component_offset, 
			   Make_Pointer_Type(elem_ty_idx, FALSE), rhs_wn,
			   addr_wn, field_id);
      WFE_Stmt_Append(wn, Get_Srcpos());
      if (need_result) {
        if (! result_in_temp)
	  wn = WN_CreateIload (OPR_ILOAD, rtype, desc, component_offset,
			       field_id != 0 ? elem_ty_idx : desc_ty_idx,
                               Make_Pointer_Type (elem_ty_idx, FALSE),
			       WN_COPY_Tree (addr_wn),
			       field_id);
	else wn = WN_Ldid(rtype, result_preg, result_preg_st, desc_ty_idx, 0);
	if (is_realpart)
	  wn = WN_Unary (OPR_REALPART, Mtype_complex_to_real (rtype), wn);
	else
	if (is_imagpart)
	  wn = WN_Unary (OPR_IMAGPART, Mtype_complex_to_real (rtype), wn);
      }
      else wn = NULL;
    }
  } else {
      error("invalid lvalue \n");
      exit (RC_USER_ERROR);
  }

  return wn;
}

/* ============================================================================
 *
 * WFE_Expand_Expr_With_Sequence_Point
 *
 * This routine is invoked instead of WN_Expand_Expr to handle the
 * following expression nodes
 *
 *   both operands of && and ||
 *   all three operands of conditional ?
 *   controlling expression of if
 *   controlling expression of switch
 *   controlling expression of while
 *   statement expression
 *
 * In order to generate WHIRL for an expression with side effects,
 * we would like to move operations such as calls, pre increment/decrement
 * into a comma operator, and operations such as post increment/decrement
 * into a rcomma operator.
 *
 * Sequence points related to function call and return are not handled
 * here as we cannot generate RCOMMA nodes in these cases.
 *
 *
 * Most (all?) calls to WFE_Expand_Expr_With_Sequence_Point should be
 * surrounded by Push_Conditional_Cleanup_Level /
 * Pop_Conditional_Cleanup_Level because this function may start a
 * temp cleanup region inside the OPR_COMMA generated below. For
 * proper nesting, we must start a conditional exception region, even
 * though this code is not necessarily conditional. Otherwise, the
 * OPR_COMMA block will end before the temp cleanup region, causing an
 * error.
 *
 * ============================================================================
 */

WN*
WFE_Expand_Expr_With_Sequence_Point (tree exp, TYPE_ID mtype, ST *target)
{
  /* 'target' must be an MTYPE_M symbol, or the symbol representing
     the invisible first arg used to return function results. */
  FmtAssert(!target ||
	    (target == Current_Function_Result_ST()) ||
	    (ST_mtype(target) == MTYPE_M),
	    ("expecting target to be MTYPE_M or return decl"));
  
  WN *wn;

  if (mtype == MTYPE_V) {
    wn = WFE_Expand_Expr (exp, target, FALSE);

  } else {

    WN *comma_block      = WN_CreateBlock ();

    WFE_Stmt_Push (comma_block, wfe_stmk_comma, Get_Srcpos ());
    wn = WFE_Expand_Expr (exp, target);
    WFE_Stmt_Pop (wfe_stmk_comma);
    if (WN_first (comma_block)) {
      if (wn && WN_rtype(wn)==MTYPE_XTBOOL && mtype==Boolean_type)
        wn = WN_Cvt(MTYPE_XTBOOL, Boolean_type, wn);
      wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (mtype), MTYPE_V,
			   comma_block, wn);
    } else {
      WN_Delete (comma_block);
    }
  }

  return wn;
} /* WFE_Expand_Expr_With_Sequence_Point */

static void
emit_barrier (bool type, tree list, INT32 k)
{
  INT32  i;
  WN    *wn = WN_CreateBarrier (type, k);

  for (i = 0; i < k; i++) {
    tree exp = TREE_VALUE (list);
    ST *st   = Get_ST (exp);
    WN_kid (wn, i) = WN_Lda (Pointer_Mtype, 0, st,
                             Make_Pointer_Type (ST_type (st), FALSE));
    list = TREE_CHAIN (list);
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* emit_barrier */

static WN *
emit_builtin_lock_test_and_set (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [2];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = TREE_CHAIN (list);

  if (obj_mtype == MTYPE_I4) {
    opc  = OPC_I4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  }
  else
  if (obj_mtype == MTYPE_U4) {
    opc  = OPC_U4INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I4;
  }
  else
  if (obj_mtype == MTYPE_I8) {
    opc  = OPC_I8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  }
  else
  if (obj_mtype == MTYPE_U8) {
    opc  = OPC_U8INTRINSIC_CALL;
    iopc = INTRN_LOCK_TEST_AND_SET_I8;
  }
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 2, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(obj_mtype);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(obj_mtype);
  PREG_NUM  preg = Create_Preg (obj_mtype, NULL);

  wn = WN_Ldid (obj_mtype, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (obj_mtype, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (obj_mtype, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_lock_test_and_set */

static void
emit_builtin_lock_release (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [1];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);

  emit_barrier (TRUE, list, k);

  opc = OPC_VINTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else
  if (obj_mtype == MTYPE_U4)
    iopc = INTRN_LOCK_RELEASE_I4;
  else
  if (obj_mtype == MTYPE_I8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else
  if (obj_mtype == MTYPE_U8)
    iopc = INTRN_LOCK_RELEASE_I8;
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 1, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* emit_builtin_lock_release */

static WN *
emit_builtin_compare_and_swap (tree exp, INT32 k)
{
  WN        *wn;
  WN        *arg_wn;
  WN        *ikids [3];
  TYPE_ID    obj_mtype;
  TY_IDX     arg_ty_idx;
  TYPE_ID    arg_mtype;
  tree       list = TREE_OPERAND (exp, 1);
  OPCODE     opc;
  INTRINSIC  iopc;

  obj_mtype  = TY_mtype (TY_pointed (Get_TY(TREE_TYPE(TREE_VALUE(list)))));
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [0]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [1]  = arg_wn;
  list       = TREE_CHAIN (list);
  arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
  arg_mtype  = TY_mtype (arg_ty_idx);
  arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
  arg_wn     = WN_CreateParm (arg_mtype, arg_wn, arg_ty_idx, WN_PARM_BY_VALUE);
  ikids [2]  = arg_wn;
  list       = TREE_CHAIN (list);

  emit_barrier (TRUE, list, k);

  opc = OPC_I4INTRINSIC_CALL;
  if (obj_mtype == MTYPE_I4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else
  if (obj_mtype == MTYPE_U4)
    iopc = INTRN_COMPARE_AND_SWAP_I4;
  else
  if (obj_mtype == MTYPE_I8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else
  if (obj_mtype == MTYPE_U8)
    iopc = INTRN_COMPARE_AND_SWAP_I8;
  else {
    Fail_FmtAssertion ("unknown object type in __builtin_lock_test_and_set");
    opc  = OPCODE_UNKNOWN;
    iopc = INTRINSIC_NONE;
  }

  wn = WN_Create_Intrinsic (opc, iopc, 3, ikids);
  WFE_Stmt_Append (wn, Get_Srcpos());

  ST       *preg_st = MTYPE_To_PREG(MTYPE_I4);
  TY_IDX    preg_ty_idx = Be_Type_Tbl(MTYPE_I4);
  PREG_NUM  preg = Create_Preg (MTYPE_I4, NULL);

  wn = WN_Ldid (MTYPE_I4, -1, Return_Val_Preg, preg_ty_idx);
  wn = WN_Stid (MTYPE_I4, preg, preg_st, preg_ty_idx, wn),
  WFE_Stmt_Append (wn, Get_Srcpos());

  emit_barrier (FALSE, list, k);

  wn = WN_Ldid (MTYPE_I4, preg, preg_st, preg_ty_idx);

  return wn;
} /* emit_builtin_compare_and_swap */

static void
emit_builtin_synchronize (tree exp, INT32 k)
{
  WN *wn;
  tree list = TREE_OPERAND (exp, 1);
  emit_barrier (TRUE,  list, k);
  wn = WN_Create_Intrinsic (OPC_VINTRINSIC_CALL, INTRN_SYNCHRONIZE, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos());
  emit_barrier (FALSE, list, k);
} /* emit_builtin_synchronize */

static char *
get_string_pointer (WN *wn)
{
  char *ptr = NULL;

  if (WN_operator (wn) == OPR_LDA) {
    ST *st = WN_st (wn);
    if (ST_class (st) == CLASS_CONST) {
      TCON tcon = Tcon_Table [ST_tcon (st)];
      if (TCON_ty (tcon) == MTYPE_STRING)
        ptr = ((char *) Targ_String_Address (tcon)) + WN_offset (wn);
    }
  }

  return ptr;
} /* get_string_pointer */


/* Create whirl to save the non-copied parts 'list' of an expr
   'lhs'. Return in 'block' the whirl needed to restore the saved
   parts. */
static WN *
save_noncopied_parts (tree lhs, tree list, WN *block)
{
  tree tail;
  tree parts = 0;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
  {
    if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
      save_noncopied_parts (lhs, TREE_VALUE (tail), block);
    else
    {
      tree part = TREE_VALUE (tail);
      tree part_type = TREE_TYPE (part);
      tree to_be_saved = build (COMPONENT_REF, part_type, lhs, part);
      
      TY_IDX ty_idx = Get_TY(part_type);
      ST *st = Gen_Temp_Symbol(ty_idx, "__noncopied_part");
      
      WN *val = WFE_Expand_Expr(to_be_saved);
      WN *wn = WN_Stid(TY_mtype (ty_idx), 0, st, ty_idx, val);
      WFE_Stmt_Append(wn, Get_Srcpos());
      
      WFE_Stmt_Push (block, wfe_stmk_scope, Get_Srcpos ());	
      WFE_Lhs_Of_Modify_Expr(MODIFY_EXPR, to_be_saved, FALSE, 
			     0, 0, 0, FALSE,
			     WN_Ldid(TY_mtype(ty_idx), 0, st, ty_idx),
			     0, FALSE, FALSE);
      WFE_Stmt_Pop (wfe_stmk_scope);
    }
  }
  
  return block;
}

/* Create the whirl to initialize the parts 'list' of an expr
   'lhs'. Return the initializations in 'block.' */
static WN *
collect_noninit_parts (tree lhs, tree list, WN *block)
{
  tree tail;
  tree parts = 0;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
  {
    if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
      collect_noninit_parts (lhs, TREE_VALUE (tail), block);
    else if (TREE_PURPOSE (tail))
    {
      tree part = TREE_VALUE (tail);
      tree part_type = TREE_TYPE (part);
      tree to_be_initialized = build (COMPONENT_REF, part_type, lhs, part);
      
      WFE_Stmt_Push (block, wfe_stmk_scope, Get_Srcpos ());	
      WFE_Lhs_Of_Modify_Expr(MODIFY_EXPR, to_be_initialized, FALSE, 
			     0, 0, 0, FALSE,
			     WFE_Expand_Expr(TREE_PURPOSE (tail)),
			     0, FALSE, FALSE);
      WFE_Stmt_Pop (wfe_stmk_scope);
    }
  }
  
  return block;
}

static void
restore_noncopied_parts (WN *noncopied_parts)
{
  if (noncopied_parts)
    WFE_Stmt_Append(noncopied_parts, Get_Srcpos());
}


// Auxiliary function for WFE_Expand_Expr, return the address of
// a tree operand.  (Used for ADDR_EXPR.)
WN*
WFE_Address_Of(tree arg0)
{
  enum tree_code code0 = TREE_CODE (arg0);
  ST *st = 0;
  WN* wn = 0;
  WN* wn0;
  WN* wn1;
  TY_IDX ty_idx;

  switch (code0) {
  case VAR_DECL:
  case PARM_DECL:
  case FUNCTION_DECL:
    {
      st = Get_ST (arg0);
      ty_idx = ST_type (st);
      // Taking the address of a nested function requires the use of
      // a trampoline dynamically allocated on the stack
      if (code0 == FUNCTION_DECL && PU_is_nested_func(Pu_Table[ST_pu(st)]))
        Set_PU_has_alloca(Get_Current_PU());
      // for VLAs, use the base_st instead of st
      if (code0 == VAR_DECL &&
          st != ST_base(st))
      {
        FmtAssert (ST_ofst (st) == 0,
                   ("VLA within struct not currently implemented"));
        wn = WN_Ldid (Pointer_Mtype, 0, ST_base(st), ST_type(ST_base(st)));
      }
      else if (!WFE_Keep_Zero_Length_Structs &&
	       code0 == PARM_DECL            &&
	       TY_mtype (ty_idx) == MTYPE_M  &&
	       TY_size (ty_idx) == 0)
      {
	// taking address of zero length struct passed as parameter
	DevWarn ("taking address of zero length struct %s at line %d",
		 ST_name (st), lineno);
	wn = WN_Intconst (Pointer_Mtype, 0);
      }
      else
      {
	/* We want to load the address of a symbol that already has an
           implicit indirection, then we just use and LDID. */
	if (ST_implicit_indirect(st))
	  wn = WN_Ldid (ST_mtype(st), ST_ofst(st), st, ST_type(st));
	else
	  wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
    }
    break;

  case INDIRECT_REF:
    wn = WFE_Expand_Expr (TREE_OPERAND(arg0, 0));
    break;
  case ARRAY_REF:
    {
      OPCODE op_array = OPCODE_make_op(OPR_ARRAY,Pointer_type, MTYPE_V);
      wn =  WN_Create(op_array,3);
      WN_array_base(wn) = WFE_Address_Of (TREE_OPERAND(arg0, 0));
      WN_array_index(wn,0) = WFE_Expand_Expr (TREE_OPERAND(arg0, 1));
      WN_array_dim(wn,0) = WN_CreateIntconst(OPC_I4INTCONST,0);
      WN_element_size(wn) = TY_size(Get_TY (TREE_TYPE(arg0)));
    }
    break;

  case STRING_CST:
    {
      TCON tcon;
      tcon = Host_To_Targ_String (MTYPE_STRING,
                                  TREE_STRING_POINTER(arg0),
                                  TREE_STRING_LENGTH(arg0));
      ty_idx = Get_TY(TREE_TYPE(arg0));
      st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
      tree curr_func_decl = Current_Function_Decl();

      /* try to determine if this is a c++ type string name in the type_info function */
      bool type_string_name = false;
      if (!xt_rw_str && curr_func_decl) {
        ST* func_st = Get_ST(Current_Function_Decl());
	const char* func_st_name = ST_name(func_st);
	const char* str = TREE_STRING_POINTER(arg0);

	/* make sure it is the type_info function */
	if (!strncmp(func_st_name,"__tf",4) && /* has to match init_rtti_processing */
	    !strcmp(func_st_name+4,str) &&
	    DECL_SECTION_NAME(curr_func_decl)) {
	  const char* func_section_name =
		  TREE_STRING_POINTER(DECL_SECTION_NAME(curr_func_decl));

	  /* make sure the type_info function has its own section */
	  if (!strncmp(func_section_name,".gnu.linkonce.t.",16) &&
	      !strcmp(func_section_name+16,func_st_name)) {

	    /* create a linkonce.r section for the type name string */
	    type_string_name = true;
	    char* str_section_name = (char*)malloc(strlen(func_section_name)+1);
	    strcpy(str_section_name,func_section_name);
	    str_section_name[14]='r';
	    SYMTAB_IDX level = GLOBAL_SYMTAB;
	    ST_ATTR_IDX st_attr_idx;
	    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
	    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
		Save_Str (str_section_name));
	    Set_ST_has_named_section (st);
	  }
	}
      }
      ST *pu_st = Get_Current_PU_ST();
      bool has_rodata_section = pu_st ? ST_has_named_ro_section(pu_st) : false;
      if (!type_string_name && flag_merge_constants && TY_align(ty_idx)==1 &&
	  !has_rodata_section) {
	  Set_ST_sclass(st,SCLASS_MERGE_STRING);
	  Set_ST_explicit_literal_ref(st);
      }
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      TREE_STRING_ST (arg0) = st;
    }
    break;

  case CONSTRUCTOR:
    {
      st = WFE_Generate_Temp_For_Initialized_Aggregate (arg0, "");
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case LABEL_DECL:
    {
      DevWarn ("taking address of a label at line %d", lineno);
      LABEL_IDX label_idx = WFE_Get_LABEL (arg0, FALSE);
#if 0
      FmtAssert (arg0->decl.symtab_idx == CURRENT_SYMTAB,
                 ("line %d: taking address of a label not defined in current function currently not implemented", lineno));
#endif
      wn = WN_LdaLabel (Pointer_Mtype, label_idx);
      Set_LABEL_addr_saved (label_idx);
    }
    break;

  case TARGET_EXPR:
    {
      WFE_Expand_Expr (arg0);
      st = Get_ST (TREE_OPERAND(arg0, 0));
      wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
    }
    break;

  case COMPOUND_EXPR:
    {
      // fix pr15780 as in PathScale
      wn = WFE_Expand_Expr(arg0);
      if (WN_operator(wn) == OPR_CSELECT) {
	// If WN is a CSELECT, get the ST from CSELECT's kids.  Bug 8472.
	//
	// Handle case where CSELECT's kid1 and kid2 are LDID or COMMAs that
	// return the same ST:
	//     ...
	//     U8U8LDID 0 <2,21,anon954> T<1402,anon_ptr.,8>
	//    U8COMMA
	//    U8U8LDID 0 <2,21,anon954> T<1402,anon_ptr.,8>
	//   U8CSELECT
	//
	// Handle other cases as they arise.
	int i;
	ST *st1 = NULL, *st2 = NULL;
	for (i = 0; i < 2; i++) {
	  WN *kid = (i == 0) ? WN_kid1(wn) : WN_kid2(wn);
	  WN *comma_kid1;
	  ST **st_ptr = (i == 0) ? &st1 : &st2;
	  switch (WN_operator(kid)) {
	    case OPR_LDID:
	      *st_ptr = WN_st(kid);
	      break;
	    case OPR_COMMA:
	      comma_kid1 = WN_kid1(kid);
	      Is_True(WN_operator(comma_kid1) == OPR_LDID,
		      ("WFE_Address_Of: kid1 of COMMA is not LDID"));
	      *st_ptr = WN_st(comma_kid1);
	      break;
	    default:
	      FmtAssert(FALSE, ("WFE_Address_Of: CSELECT kid NYI"));
	  }
	}
	Is_True((st1 != NULL) && (st1 == st2),
		("WFE_Address_Of: CSELECT kids returns different STs"));
	st = st1;
      } else
	st = WN_st(wn);
      wn = WN_Lda (Pointer_Mtype,  ST_ofst (st), st);
    }
    break;

  case NOP_EXPR:
  case SAVE_EXPR:
    {
      wn = WFE_Address_Of(TREE_OPERAND(arg0, 0));
    }
    break;

  case MIN_EXPR:
  case MAX_EXPR:
    {
      // &(a <? b) or &(a >? b)
      tree op0 = TREE_OPERAND(arg0, 0);
      tree op1 = TREE_OPERAND(arg0, 1);
      WN* a = WFE_Expand_Expr(op0);
      WN* b = WFE_Expand_Expr(op1);
      FmtAssert(!WN_has_side_effects(a) && !WN_has_side_effects(b),
                ("Addr of MIN/MAX_EXPR with side effects not yet supported"));

      FmtAssert(same_type_p(TREE_TYPE(op0), TREE_TYPE(op1)),
                ("Types of MIN/MAX_EXPR operands differ"));
      TY_IDX  ptr_ty    = Make_Pointer_Type (Get_TY(TREE_TYPE(op0)), FALSE);
      TYPE_ID ptr_mtype = TY_mtype(ptr_ty);
      TY_IDX  arg_ty    = Get_TY(TREE_TYPE(TREE_OPERAND(arg0, 0)));
      TYPE_ID arg_mtype = TY_mtype(arg_ty);

      WN* aptr = WFE_Address_Of(op0);
      WN* bptr = WFE_Address_Of(op1);
      wn = WN_Select(Widen_Mtype(ptr_mtype),
                     WN_Relational(code0 == MIN_EXPR ? OPR_LT : OPR_GT,
                                   Widen_Mtype(arg_mtype),
                                   a, b),
                     aptr, bptr);
      Set_PU_has_very_high_whirl (Get_Current_PU ());
    }
    break;


  case COMPONENT_REF:
    {
      wn = WFE_Expand_Expr (arg0);
      ty_idx = Get_TY(TREE_TYPE(arg0));
      if (WN_operator (wn) == OPR_LDID) {
        WN_set_operator (wn, OPR_LDA);
        WN_set_desc (wn, MTYPE_V);
      }
      else
      if (WN_operator (wn) == OPR_ILOAD) {
        wn0 = WN_kid0 (wn);
        wn1 = WN_Intconst (Pointer_Mtype, WN_offset (wn));
        wn  = WN_Binary (OPR_ADD, Pointer_Mtype, wn0, wn1);
      }
      else
        Fail_FmtAssertion ("WFE_Address_Of has unhandled %s",
                           Operator_From_Tree [code0].name);
    }
    break;

  default:
    {
      Fail_FmtAssertion ("WFE_Address_Of: Unexpected operand %s",
                         Operator_From_Tree [code0].name);
    }
    break;
  }

  FmtAssert(wn != 0, ("WFE_Address_Of: null WHIRL tree for %s",
                      Operator_From_Tree [code0].name));
  return wn;
}

static WN*
XTBOOL_Expand_Unary_Expr(tree exp, WN* wn0)
{
  enum tree_code code = TREE_CODE (exp);
  WN *wn;

  TYPE_ID mtype0 = WN_rtype(wn0);
  wn = NULL;

  FmtAssert(mtype0==MTYPE_XTBOOL, ("Expect xtbool type"));
  if (code == TRUTH_NOT_EXPR) {
	wn = WN_Bnot(mtype0, wn0);
  } else
    FmtAssert(0, ("Unknown tree code"));

  return wn;
}

static WN*
WFE_Component_Ref_Of_Modify_Expr(WN *store,
                                 TY_IDX ty_idx,
                                 INT64 offset,
                                 UINT16 field_id,
                                 bool is_bit_field)
{
  FmtAssert(field_id, ("Expected non-zero field_id"));
  FmtAssert(ty_idx, ("Expected non-zero ty_idx"));

  TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
  TYPE_ID desc = is_bit_field ? MTYPE_BS : TY_mtype(ty_idx);
  
  OPERATOR opr = WN_operator(store);
  FmtAssert(opr == OPR_STID || opr == OPR_ISTORE, ("Expected a store"));

  if (opr == OPR_STID)
    return WN_CreateLdid(OPR_LDID, rtype, desc, offset, 
                         WN_st(store), WN_ty(store), field_id);
  else
    return WN_CreateIload(OPR_ILOAD, rtype, desc, offset,
                          TY_pointed(WN_ty(store)), WN_ty(store),
                          WN_COPY_Tree(WN_kid1(store)), field_id);
}

// The words in 'buf' are in target order. Convert them to host order
// in place. 'buf' is a two word array.
static inline void
WFE_Convert_To_Host_Order (long *buf)
{
  if (!Same_Byte_Sex) {
    long tmp = buf[0]; buf[0] = buf[1]; buf[1] = tmp;
  }
}

// any volatile pointers in the expression routed at wn 
static BOOL Contains_Volatile(WN *wn)
{
    if (WN_operator(wn) == OPR_LDID &&
		TY_kind(WN_ty(wn)) == KIND_POINTER) {
	TY_IDX pointed_type = TY_pointed(WN_ty(wn));
	if (TY_is_volatile(pointed_type)) {
	    return TRUE;
	}
    } else if (WN_operator(wn) == OPR_LDA) {
	TY_IDX pointed_type = TY_pointed(WN_ty(wn));
	if (TY_is_volatile(pointed_type)) {
	    return TRUE;
	}
    } else {
	for (INT kid=0; kid<WN_kid_count(wn); kid++) {
	    if (Contains_Volatile(WN_kid(wn,kid))) {
		return TRUE;
	    }
	}
    }
    return FALSE;
}

/* expand gnu expr tree into symtab & whirl */
WN *
WFE_Expand_Expr (tree exp,
		 ST *target,
		 bool need_result,
		 TY_IDX nop_ty_idx, 
		 TY_IDX component_ty_idx, 
		 INT64 component_offset,
		 UINT16 field_id,
		 bool is_bit_field,
		 bool is_aggr_init_via_ctor)
{
  FmtAssert(exp != NULL_TREE, ("WFE_Expand_Expr: null argument"));

  /* 'target' must be an MTYPE_M symbol, or the symbol representing
     the invisible first arg used to return function results. */
  FmtAssert(!target ||
	    (target == Current_Function_Result_ST()) ||
	    (ST_mtype(target) == MTYPE_M),
	    ("expecting target to be MTYPE_M or return decl"));
  
  enum tree_code code = TREE_CODE (exp);
  WN *wn, *wn0, *wn1, *wn2;
  ST *st;
  TY_IDX ty_idx;
  TY_IDX desc_ty_idx;
  TY_IDX target_ty_idx = TY_IDX_ZERO;
  tree arg0, arg1, arg2;
  WN *noncopied_parts = NULL;

  wn = NULL;

#ifdef WFE_DEBUG
  fprintf (stderr,
           "{( WFE_Expand_Expr: %s\n", Operator_From_Tree [code].name); // ")}"
#endif /* WFE_DEBUG */

  switch (code)
    {
    // leaves
    case ADDR_EXPR:
      wn = WFE_Address_Of(TREE_OPERAND(exp, 0));
      break;

    case FUNCTION_DECL:
      {
	 st = Get_ST (exp);
	 ty_idx = ST_type (st);
	 wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
      }
      break;

    case TREE_LIST: 
      {
	tree stmt;
	for (stmt = TREE_PURPOSE(exp); stmt; stmt = TREE_CHAIN(stmt))
	  WFE_Expand_Stmt (stmt);
	wn = WFE_Expand_Expr (TREE_VALUE(exp));
      }
      break;

    case DECL_STMT:
      {
        tree decl = DECL_STMT_DECL(exp);
	WFE_Expand_Decl (decl);
	wn = WFE_Expand_Expr (decl);
      }
      break;

    case BIND_EXPR:
#ifdef GPLUSPLUS_FE
      // DevWarn ("Encountered BIND_EXPR at line %d", lineno);
      // ignore the first operand as it ia a list of temporary variables
      wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
      break;
#else
      {
        INT32    i;
        WN      *block;
        TYPE_ID  mtype;
        tree     t;

	// DevWarn ("Encountered BIND_EXPR at line %d", lineno);

        for (i = wfe_bind_expr_stack_last; i >= 0; --i) {

          if (wfe_bind_expr_stack [i].rtl_expr == TREE_OPERAND (exp, 1)) {

            block = wfe_bind_expr_stack [i].block;
            t     = wfe_bind_expr_stack [i].rtl_expr;
            wfe_bind_expr_stack [i] = wfe_bind_expr_stack [wfe_bind_expr_stack_last];
            --wfe_bind_expr_stack_last;
            break;
          }
        }

        FmtAssert (i >= 0,
                   ("BIND_EXPR: did not find tree"));
	ty_idx = Get_TY (TREE_TYPE(t));
        mtype  = TY_mtype (ty_idx);
	if (mtype == MTYPE_V) {
	  WFE_Stmt_Append (block, Get_Srcpos ());
          break;
	}
	else {
	  wn0 = block;
	  wn1 = WN_COPY_Tree (WN_last (wn0));
	  WN_DELETE_FromBlock (wn0, WN_last (wn0));
	  WFE_Stmt_Append (wn0, Get_Srcpos ());
	  if (nop_ty_idx == 0 && component_ty_idx == 0) {
	    wn = WN_kid0 (wn1);
            break;
	  }
          if (WN_operator (WN_kid0 (wn1)) == OPR_LDID)
            st = WN_st (WN_kid0 (wn1));
          else {
            st = Gen_Temp_Symbol (ty_idx, "__bind_expr");
            WFE_Set_ST_Addr_Saved (WN_kid0 (wn1));
            wn0 = WN_Stid (mtype, 0, st, ty_idx, WN_kid0 (wn1));
            WFE_Stmt_Append (wn0, Get_Srcpos ());
          }
	}
      }
      /*FALLTHRU*/
#endif /* GPLUSPLUS_FE */

    case TARGET_EXPR:
      {
#ifdef GPLUSPLUS_FE
	/* for g++ ensure that the WHIRL type for the enclosing
	   structure (if any) has been created. */
	(void) Get_TY (TREE_TYPE(TREE_OPERAND (exp, 0)));
#endif /* GPLUSPLUS_FE */

        st            = Get_ST (TREE_OPERAND(exp, 0));
	tree type     = TREE_TYPE(TREE_OPERAND(exp, 0));
	TY_IDX ty     = ST_type(st);
	TYPE_ID mtype = TY_mtype (ty);

	/*
	 * Usually operand 1 of the target_expr will be an aggr_init_expr
	 * for which AGGR_INIT_VIA_CTOR_P holds.  Such a node has the
	 * annoying property that its first argument is not the expected
	 * argument to the constructor call.  Instead the argument whose
	 * address should be passed to the constructor appears as 
	 * operand 2.  The value in operand 2, however, is not always
 	 * right:  it is the original temporary var_decl from the
	 * target_expr.  What we really want is the current operand 0
	 * of the target_expr, which may have been changed (see INIT_EXPR).
	 * This is really ugly, but we can't help it because at the
	 * expression level we need to stay compatible with the current
	 * rtl generation.
	 * So we're going to replace the first argument of the aggr_init_expr
	 * with the var_decl from operand 0 of the target_expr, and pass
	 * is_aggr_init_via_ctor to WFE_Expand_Expr, so it can be dealt
	 * with by the AGGR_INIT_EXPR/CALL_EXPR code.
	 *
	 * If a target expression is passed a 'target', then we don't
	 * need to do the cleanup associated with this target_expr.
	 */
	WN *wn = NULL;
	tree t = TREE_OPERAND(exp, 1);
 	if (target)
	  TREE_OPERAND(exp, 2) = 0;
	if (TREE_CODE(t) == AGGR_INIT_EXPR && AGGR_INIT_VIA_CTOR_P(t))
	{
	  FmtAssert(target == 0, ("not expecting target for AGGR_INIT_EXPR"));
	  tree args = TREE_OPERAND(t, 1);
	  TREE_VALUE(args) = TREE_OPERAND(exp, 0);
	  WFE_Expand_Expr (t, NULL, false, 0, 0, 0, 0, false, true);
	}
	else if (target || WFE_Addressable_Type(type))
	{
	  if (!target)
	    target = st;

	  DECL_ST(TREE_OPERAND(exp, 0)) = target;
	  wn = WFE_Store_Expr (t, target);
	  target_ty_idx = ST_type(target);

	  st = target;
	  ty = ST_type(st);
	  mtype = TY_mtype(ty);
	}
	else
	{
	  wn = WFE_Expand_Expr (t);
	}

	/* If 'wn' is non-NULL, then we must store the expansion of
           't' into 'target'. */
	if (wn)
	{
	  /* If accesses to 'st' have an implicit indirection, then we
	     must use an ISTORE. */
	  if (target && ST_implicit_indirect(st))
	    WFE_Stmt_Append(WFE_CreateImplicitIndirectStore(MTYPE_M, ST_ofst(st),
							    st, ty, wn, 0),
			    Get_Srcpos());
	  else
	    WFE_Stmt_Append(WN_Stid (mtype, ST_ofst(st), st, ty, wn),
			    Get_Srcpos());
	  WFE_Set_ST_Addr_Saved(wn);
	}

        if (TREE_OPERAND(exp, 2))
          Push_Temp_Cleanup(TREE_OPERAND(exp, 2), false, true);
      }

    case CONSTRUCTOR:
    case PARM_DECL: // for formal parms
    case VAR_DECL:
      {
        PREG_NUM preg_num = 0;
	desc_ty_idx = component_ty_idx;
	TY_IDX hi_ty_idx = ((target_ty_idx != TY_IDX_ZERO) ? target_ty_idx :
			    Get_TY (TREE_TYPE(exp)));
	if (desc_ty_idx == 0)
	  desc_ty_idx = hi_ty_idx;

        TY_IDX ldid_ty_idx = desc_ty_idx;
        
	if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}

	UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
	    cvtl_size = TY_size(ty_idx) * 8;
	    ty_idx = desc_ty_idx;
	  }
	}
	else {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) 
	    ty_idx = desc_ty_idx;
	}

        TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
        TYPE_ID desc = TY_mtype(desc_ty_idx);
        if (MTYPE_is_integral(desc)) {
          if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
            if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
              rtype = Mtype_TransferSign(desc, rtype);
            else desc = Mtype_TransferSign(rtype, desc);
          }
        }

        TYPE_ID cvt_rtype = 0;
#ifdef TARG_XTENSA
        if (MTYPE_is_integral(desc) && MTYPE_size_min(desc) < 32) {
          if (rtype == MTYPE_I8)
            cvt_rtype = MTYPE_I8, rtype = MTYPE_I4;
          else if (rtype == MTYPE_U8)
            cvt_rtype = MTYPE_U8, rtype = MTYPE_U4;
        }
#endif
	if (TREE_THIS_VOLATILE(exp) || TY_is_volatile(component_ty_idx)) {
	  Set_TY_is_volatile(hi_ty_idx);
	  Set_TY_is_volatile(ldid_ty_idx);
	}

	if (code == PARM_DECL || code == VAR_DECL) {
	  st = Get_ST (exp);
          if (ST_assigned_to_dedicated_preg (st)) {
	    Set_TY_is_volatile(hi_ty_idx);
	    Set_TY_is_volatile(ldid_ty_idx);
	  }
        }
	else if (code == CONSTRUCTOR) {
	  // DevWarn ("Encountered CONSTRUCTOR at line %d", lineno);
	  st = WFE_Generate_Temp_For_Initialized_Aggregate (exp, "");
	}

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));

	/* If accesses to 'st' have an implicit indirection, then we
           must use an ILOAD. */
	if (ST_implicit_indirect(st))
	{
	  FmtAssert(code != CONSTRUCTOR, ("unexpected implicit indirect for CONSTRUCTOR"));

	  /* If 'field_id' is 0 then we are loading the entire object,
             so use MTYPE_M. */
	  if (field_id == 0)
	    rtype = desc = MTYPE_M;
	  
	  wn = WFE_CreateImplicitIndirectLoad(rtype,
					      is_bit_field ? MTYPE_BS : desc,
					      ST_ofst(st)+component_offset+preg_num,
					      st,
					      field_id != 0 ? hi_ty_idx : ldid_ty_idx,
					      field_id);
	}
	else
	{
	  wn = WN_CreateLdid (OPR_LDID, rtype,
			      is_bit_field ? MTYPE_BS : desc,
			      ST_ofst(st)+component_offset+preg_num, st,
			      field_id != 0 ? hi_ty_idx : ldid_ty_idx, 
                              field_id);
	}
	
        if (cvt_rtype)
          wn = WN_Cvt(rtype, cvt_rtype, wn);

	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case CONST_DECL:
        wn = WFE_Expand_Expr(DECL_INITIAL(exp), NULL, need_result);
	break;

    case INTEGER_CST:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	mtyp = (mtyp == MTYPE_V) ? MTYPE_I4 : Widen_Mtype(mtyp);
	wn = WN_Intconst(mtyp, Get_Integer_Value(exp));
      }
      break;

    case PTRMEM_CST:
      {
	wn = WFE_Expand_Expr(cplus_expand_constant (exp), NULL,
			     need_result, nop_ty_idx, component_ty_idx,
			     component_offset, field_id);
      }
      break;

    case REAL_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
	REAL_VALUE_TYPE real = TREE_REAL_CST(exp);
	long rval;
	long rbuf[2];
	switch (TY_mtype (ty_idx)) {
	  case MTYPE_F4:
	    REAL_VALUE_TO_TARGET_SINGLE (real, rval);
	    tcon = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &rval);
	    break;
	  case MTYPE_F8:
	    REAL_VALUE_TO_TARGET_DOUBLE (real, rbuf);
	    WFE_Convert_To_Host_Order(rbuf);
	    tcon = Host_To_Targ_Float (MTYPE_F8, *(double *) &rbuf);
	    break;
	  default:
	    FmtAssert(FALSE, ("WFE_Expand_Expr unexpected float size"));
	    break;
	}
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    case COMPLEX_CST:
      {
	TCON tcon;
	ty_idx = Get_TY (TREE_TYPE(exp));
	REAL_VALUE_TYPE real = TREE_REAL_CST(TREE_REALPART(exp));
	REAL_VALUE_TYPE imag = TREE_REAL_CST(TREE_IMAGPART(exp));
        long rval;
	long ival;
	long rbuf[2];
	long ibuf[2];
	switch (TY_mtype (ty_idx)) {
	  case MTYPE_C4:
	    REAL_VALUE_TO_TARGET_SINGLE (real, rval);
	    REAL_VALUE_TO_TARGET_SINGLE (imag, ival);
	    tcon = Host_To_Targ_Complex_4 (MTYPE_C4,
					   *(float *) &rval,
					   *(float *) &ival);
	    break;
	  case MTYPE_C8:
	    REAL_VALUE_TO_TARGET_DOUBLE (real, rbuf);
	    REAL_VALUE_TO_TARGET_DOUBLE (imag, ibuf);
	    WFE_Convert_To_Host_Order(rbuf);
	    WFE_Convert_To_Host_Order(ibuf);
	    tcon = Host_To_Targ_Complex (MTYPE_C8,
					 *(double *) &rbuf,
					 *(double *) &ibuf);
	    break;
	  default:
	    FmtAssert(FALSE, ("WFE_Expand_Expr unexpected float size"));
	    break;
	}
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	wn = WN_CreateConst (OPR_CONST, TY_mtype (ty_idx), MTYPE_V, st);
      }
      break;

    // this should occur only if string is a statement expression
    case STRING_CST:
      {
	TCON tcon;
	tcon = Host_To_Targ_String (MTYPE_STRING,
				    TREE_STRING_POINTER(exp),
				    TREE_STRING_LENGTH(exp));
	ty_idx = Get_TY(TREE_TYPE(exp));
	ST *pu_st = Get_Current_PU_ST();
	bool has_rodata_section = ST_has_named_ro_section(pu_st);
	st = New_Const_Sym (Enter_tcon (tcon), ty_idx);
	if (flag_merge_constants && TY_align(ty_idx)==1 &&
	    !has_rodata_section) {
	  Set_ST_sclass(st,SCLASS_MERGE_STRING);
	  Set_ST_explicit_literal_ref(st);
	}

	wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
	TREE_STRING_ST (exp) = st;
      }
      break;

    // unary ops
    case BIT_NOT_EXPR:
    case ABS_EXPR:
    case NEGATE_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	if (WN_rtype(wn0)==MTYPE_XTBOOL)
	  wn = XTBOOL_Expand_Unary_Expr(exp,wn0);
	else
	  wn  = WN_Unary (Operator_From_Tree [code].opr,
                        Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn0);
      }
      break;

    case TRUTH_NOT_EXPR:
      wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
      if (WN_rtype(wn0)==MTYPE_XTBOOL) {
	wn = XTBOOL_Expand_Unary_Expr(exp,wn0);
      } else {
        wn1 = WN_Intconst (MTYPE_I4, 0);
        wn  = WN_Relational (OPR_EQ, MTYPE_I4, wn0, wn1);
      }
      break;

    case CONJ_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID complex_mtype = TY_mtype(ty_idx);
        TYPE_ID float_mtype   = Mtype_complex_to_real (complex_mtype);
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	if (WN_has_side_effects (wn0)) {
	  ST       *preg_st;
	  PREG_NUM  preg;
	  preg_st = MTYPE_To_PREG(complex_mtype);
	  preg    = Create_Preg (complex_mtype, NULL);
	  wn0     = WN_Stid (complex_mtype, preg, preg_st, ty_idx, wn0);
	  WFE_Stmt_Append (wn0, Get_Srcpos());
	  wn0 = WN_Ldid (complex_mtype, preg, preg_st, ty_idx);
	}
        wn = WN_Binary (OPR_COMPLEX, complex_mtype,
			WN_Unary (OPR_REALPART, float_mtype, wn0),
			WN_Unary (OPR_NEG, float_mtype,
				  WN_Unary (OPR_REALPART, float_mtype, wn0)));
      }
      break;

    case NOP_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID mtyp = TY_mtype(ty_idx);
	// do not pass struct type down because will cause rtype of MTYPE_M
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), target, TRUE, 
			      (mtyp == MTYPE_M) ? 0 : ty_idx,
			       component_ty_idx, component_offset,
			       field_id, is_bit_field);
	if (mtyp == MTYPE_V) 
	  break;
	if (mtyp == MTYPE_M) 
	  break;
	if (MTYPE_is_integral(mtyp) && MTYPE_is_integral(WN_rtype(wn))) {
#if 0  /* #ifdef TARG_XTENSA --- This change is commented out because
	  I don't thing we need it... the optimizer seems to eliminate
	  the CVTL/CVT pair... */
        if (MTYPE_size_min(WN_rtype(wn)) <= MTYPE_size_min(MTYPE_U4)) {
#endif
	  if (MTYPE_size_min(mtyp) < MTYPE_size_min(WN_rtype(wn))) {
	    if (MTYPE_size_min(mtyp) != 32) {
#ifdef TARG_XTENSA
              if (MTYPE_is_longlong(WN_rtype(wn)))
                wn = WN_Cvt(WN_rtype(wn), Widen_Mtype(mtyp), wn);
#endif
	      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp), MTYPE_V,
			         MTYPE_size_min(mtyp), wn);
            }
	    else wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	  }
	  else {
	    TY_IDX ty_idx0 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 0)));
	    TYPE_ID mtyp0 = TY_mtype(ty_idx0);

	    if (MTYPE_size_min(mtyp) > MTYPE_size_min(mtyp0) &&
		! Has_Subsumed_Cvtl(WN_operator(wn)) &&
                Widen_Mtype(mtyp0) != mtyp0)
	      wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
				 MTYPE_size_min(mtyp0), wn);

	    if (MTYPE_size_min(mtyp) > MTYPE_size_min(WN_rtype(wn)))
	      wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	    else { // same size
	      if (mtyp != WN_rtype(wn)) 
	        wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	    }
	  }
#if 0 /* #ifdef TARG_XTENSA */
	  }
#endif
	}
	else {
	  if (mtyp != WN_rtype(wn)) 
	    wn = WN_Cvt(WN_rtype(wn), mtyp, wn);
	}
      }
      break;

    case COMPONENT_REF:
      {
	INT64 ofst;
	arg0 = TREE_OPERAND (exp, 0);
	arg1 = TREE_OPERAND (exp, 1);
	// If this is an indirect of a nop_expr, we may need to fix the
	// type of the nop_expr:
	(void) Get_TY(TREE_TYPE(arg0));
   
	if (component_ty_idx == 0)
	  ty_idx = Get_TY (TREE_TYPE(exp));
	else ty_idx = component_ty_idx;
	if (DECL_BIT_FIELD(arg1)) 
	  is_bit_field = TRUE;

	if (! is_bit_field && 
	    component_ty_idx == 0) {  // only for top-level COMPONENT_REF
          // if size does not agree with ty_idx, fix ty_idx
          tree sizenode = DECL_SIZE(arg1);
          if (TREE_CODE(sizenode) == INTEGER_CST) {
	    TYPE_ID c_mtyp = TY_mtype(ty_idx);
	    INT32 bsize = Get_Integer_Value(sizenode);
	    if (MTYPE_size_min(c_mtyp) > bsize) {
	      FmtAssert(MTYPE_is_integral(c_mtyp), 
	        ("COMPONENT_REF: integer type expected at inconsistent field size"));
	      c_mtyp = Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(c_mtyp));
	      ty_idx = MTYPE_To_TY(c_mtyp);
	    }
	  }
        }

	if (! is_bit_field)
	  ofst = (BITSPERBYTE * Get_Integer_Value(DECL_FIELD_OFFSET(arg1)) +
			        Get_Integer_Value(DECL_FIELD_BIT_OFFSET(arg1)))
			      / BITSPERBYTE;
	else ofst = 0;
	if (TREE_THIS_VOLATILE(exp))
	  Set_TY_is_volatile(ty_idx);
        wn = WFE_Expand_Expr (arg0, NULL, TRUE, nop_ty_idx, ty_idx, ofst+component_offset,
			      field_id + DECL_FIELD_ID(arg1), is_bit_field);
      }
      break;

    case INDIRECT_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));

	TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(exp));
	desc_ty_idx = component_ty_idx;
	if (desc_ty_idx == 0)
	  desc_ty_idx = hi_ty_idx;

        TY_IDX iload_ty_idx = desc_ty_idx;

        if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}
        UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
            /* For non-volatiles, we could use smaller load when the result
               is smaller. However, it seems like this optimization can and
               should be done in the back-end. */
            cvtl_size = TY_size(ty_idx) * 8;
            ty_idx = desc_ty_idx;
	  }
	}
	else {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) 
	    ty_idx = desc_ty_idx;
	}

	TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	TYPE_ID desc = TY_mtype(desc_ty_idx);
	if (MTYPE_is_integral(desc)) {
	  if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
	    if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
	      rtype = Mtype_TransferSign(desc, rtype);
	    else desc = Mtype_TransferSign(rtype, desc);
	  }
	}

	if (TREE_THIS_VOLATILE(exp) || TY_is_volatile(component_ty_idx))
	  Set_TY_is_volatile(hi_ty_idx);

	Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
		("WFE_Expand_Expr: field id for bit-field exceeds limit"));

        if (!WFE_Keep_Zero_Length_Structs &&
            rtype == MTYPE_M              &&
            TY_size (hi_ty_idx) == 0) {
	  if (WN_has_side_effects (wn0)) {
	    wn = WN_CreateEval (wn0);
	    WFE_Stmt_Append (wn, Get_Srcpos());
	  }
	  wn = NULL;
        }
        else {
	  // special case indexing into a constant string
	  if (WN_operator (wn0) == OPR_LDA          &&
	      ST_class (WN_st (wn0)) == CLASS_CONST &&
	      is_bit_field == FALSE                 &&
	      field_id == 0) {
            st = WN_st (wn0);
	    TCON tcon = Tcon_Table [ST_tcon (st)];
	    if (TCON_ty (tcon) == MTYPE_STRING &&
                TY_size (Be_Type_Tbl (desc)) == 1) {
	      mUINT32 len = Targ_String_Length (tcon);
	      mUINT64 offset = component_offset + xtra_BE_ofst + WN_offset (wn0);
	      if (offset <= len    &&
		  desc == MTYPE_U1 &&
		  (rtype == MTYPE_U4 || rtype == MTYPE_U8)) {
		unsigned char *cp = (unsigned char *) Targ_String_Address (tcon);
		unsigned long long val = cp [offset];
		wn = WN_Intconst (rtype, val);
		break;
	      }
	      else
	      if (offset <= len    &&
		  desc == MTYPE_I1 &&
		  (rtype == MTYPE_I4 || rtype == MTYPE_I8)) {
		signed char *cp = (signed char *) Targ_String_Address (tcon);
		signed long long val = cp [offset];
		wn = WN_Intconst (rtype, val);
		break;
	      }
	    }
	  }
	  if (need_result || TREE_THIS_VOLATILE(exp))
	    wn = WN_CreateIload(OPR_ILOAD, rtype,
				is_bit_field ? MTYPE_BS : desc, 
				component_offset+xtra_BE_ofst,
                                field_id != 0 ? hi_ty_idx : iload_ty_idx, 
				Make_Pointer_Type (hi_ty_idx, FALSE),
				wn0, field_id);
	  else
	  if (WN_has_side_effects (wn0))
	    wn = wn0;
	}
	if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case CONVERT_EXPR:
    case FLOAT_EXPR:
      {
	TYPE_ID src_mtype = TY_mtype(Get_TY(TREE_TYPE(TREE_OPERAND(exp,0))));
	TYPE_ID dst_mtype = TY_mtype(Get_TY(TREE_TYPE(exp)));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = TY_mtype(ty_idx);
	if (mtyp == MTYPE_V)
	  wn = wn0;
	else {
	  mtyp = Widen_Mtype(TY_mtype(ty_idx));
	  if (mtyp == WN_rtype(wn0) || mtyp == MTYPE_V)
	    wn = wn0;
	  else {
            if (MTYPE_is_tie(src_mtype) || MTYPE_is_xtbool(src_mtype) ||
                MTYPE_is_tie(dst_mtype) || MTYPE_is_xtbool(dst_mtype)) {
	      if (mtyp == dst_mtype) {
		TIE_MACRO_p rtor_macro =
		  tie_info->mtype_rtor_macro(src_mtype,dst_mtype);
		TIE_MACRO_p mtor_macro =
		  tie_info->mtype_mtor_macro(src_mtype,dst_mtype);
		if ((WN_operator(wn0)==OPR_LDID ||
		     WN_operator(wn0)==OPR_ILOAD) &&
		    rtor_macro==NULL && mtor_macro!=NULL) {
		  /* if no rtor macro, use conerting load */
		  WN_set_rtype(wn0,mtyp);
		  WN_set_desc(wn0,src_mtype);
		  wn = wn0;
		} else
		  wn = WN_Cvt(src_mtype, dst_mtype, wn0);
	      } else {
                /* for TIE or xtbool types, preserve the original mtype
                   in cvt operator to be expanded in isel
                */
		wn = WN_Cvt(src_mtype, mtyp, wn0);
		wn = WN_CreateCvtl(OPR_CVTL, mtyp, MTYPE_V,
					MTYPE_bit_size(dst_mtype), wn);
	      }
	    } else
	      wn = WN_Cvt(WN_rtype(wn0), mtyp, wn0);
	    // The following opcodes are not valid for MIPS
	    if (WN_opcode(wn) == OPC_I4U4CVT ||
	        WN_opcode(wn) == OPC_U4I4CVT ||
	        WN_opcode(wn) == OPC_I8U8CVT ||
	        WN_opcode(wn) == OPC_U8I8CVT) {
	      wn = WN_kid0 (wn);
	    }
	  }
	}
      }
      break;

    case FIX_TRUNC_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	ty_idx = Get_TY (TREE_TYPE(exp));
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(ty_idx));
	wn = WN_Trunc(WN_rtype(wn0), mtyp, wn0);
      }
      break;

#ifdef GPLUSPLUS_FE
    case EXPR_STMT:
      {
	wn = WFE_Expand_Expr (EXPR_STMT_EXPR(exp), NULL, need_result);
      }
      break;

    case STMT_EXPR:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), NULL, need_result);
      }
      break;

    case SUBOBJECT:
      break;

    case CLEANUP_POINT_EXPR: 
      {
        WN * cleanup_block = WN_CreateBlock ();
        WFE_Stmt_Push (cleanup_block, wfe_stmk_cleanup_point, Get_Srcpos ());
        Push_Temp_Cleanup(exp, true, false);
	Push_Conditional_Cleanup_Level(cleanup_block, NULL);

	/* I'm not expecting 'target' non-NULL here. But if it happens
           we can probably handle it like we do in
           WFE_Expand_Return. */
	FmtAssert(!target, ("unexpected target in CLEANUP_POINT_EXPR"));
	
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), target);
	if (wn && (WN_operator(wn) == OPR_COMMA))
	{
	  WFE_Stmt_Append(WN_kid0(wn), Get_Srcpos());
	  wn = WN_kid1(wn);
	}

	if (wn &&
	    (WN_has_side_effects(wn) ||
	     ((WN_operator(wn) == OPR_LDID) &&
	      (WN_st(wn) == Return_Val_Preg)))) {
	  DevWarn("CLEANUP_POINT_EXPR: expressson has side effects");
	  ty_idx = Get_TY (TREE_TYPE(exp));
	  st = Gen_Temp_Symbol (ty_idx, "__cleanup_point_expr");
	  TYPE_ID mtype = TY_mtype (ty_idx);
	  WFE_Set_ST_Addr_Saved (wn);
	  wn = WN_Stid (mtype, 0, st, ty_idx, wn);
	  WFE_Stmt_Append (wn, Get_Srcpos ());
	  wn = WN_Ldid (mtype, 0, st, ty_idx);
	}

	Do_Temp_Cleanups(exp);
	Pop_Conditional_Cleanup_Level();
        WFE_Stmt_Pop (wfe_stmk_cleanup_point);
	WFE_Stmt_Append (cleanup_block, Get_Srcpos ());
      }
      break;

    case THROW_EXPR:
      WFE_One_Stmt (TREE_OPERAND (exp, 0));
      Call_Throw();
      break;

    case TRY_CATCH_EXPR:
      {
	tree handler = TREE_OPERAND (exp, 1);
	WN *region = WFE_Start_Try_Block();
	WFE_Stmt_Push(WN_region_body(region), wfe_stmk_try_block, Get_Srcpos());
	wn = WFE_Expand_Expr(TREE_OPERAND (exp, 0), NULL, need_result);
	WFE_Stmt_Pop(wfe_stmk_try_block);
	WFE_Finish_Try_Block(region, handler);
      }
      break;

    case COMPOUND_STMT:
      {
	tree t = COMPOUND_BODY(exp);
	tree last_expr_stmt = 0;
	wn = NULL;
	while (t) {
	  if (TREE_CODE(t) == EXPR_STMT)
	    last_expr_stmt = t;
	  t = TREE_CHAIN(t);
	}

	t = COMPOUND_BODY(exp);

	while (t != last_expr_stmt) {
	  WFE_Expand_Stmt (t);
	  t = TREE_CHAIN(t);
	}

	if (t) {
	  wn =  WFE_Expand_Expr(t, NULL, need_result);
	  t = TREE_CHAIN(t);
	}

	while (t) {
	  WFE_Expand_Stmt(t);
	  t = TREE_CHAIN(t);
	}

      }
      break;

    case EMPTY_CLASS_EXPR:
      // DevWarn ("Encountered EMPTY_CLASS_EXPR at line %d\n", lineno);
      ty_idx = Get_TY (TREE_TYPE(exp));
      st = Gen_Temp_Symbol (ty_idx, "__empty_class_expr");
      wn = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
      break;
#endif /* GLPUSPLUFE */

    // binary ops
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LSHIFT_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case COMPLEX_EXPR:
    case CEIL_DIV_EXPR:
      {
	TYPE_ID etype = TY_mtype(Get_TY(TREE_TYPE(exp)));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
	if (WN_rtype(wn0)==MTYPE_XTBOOL)
	  wn0 = WN_Cvt(WN_rtype(wn0), MTYPE_I4, wn0);
	if (WN_rtype(wn1)==MTYPE_XTBOOL)
	  wn1 = WN_Cvt(WN_rtype(wn1), MTYPE_I4, wn1);
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn0, wn1);
	if ((MTYPE_is_integral(etype)) &&
	    (Widen_Mtype(etype) != etype) &&
	    (TY_size (Get_TY(TREE_TYPE(exp))) < 32) &&
	    (code == PLUS_EXPR || code == MINUS_EXPR || code == MULT_EXPR))
		wn = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(etype), MTYPE_V,
				TY_size (Get_TY(TREE_TYPE(exp))) * 8, wn);

      }
      break;

    case LROTATE_EXPR:
      {
	ty_idx = Get_TY(TREE_TYPE(exp));
	TYPE_ID mtype = TY_mtype (ty_idx);
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
	wn1 = WN_Binary (OPR_SUB, Widen_Mtype (mtype),
			 WN_Intconst (Widen_Mtype (mtype),
				      TY_size (ty_idx) * 8),
			 wn1);
	wn  = WN_Rrotate (TY_mtype(Get_TY(TREE_TYPE(exp))), wn0, wn1);

	TYPE_ID rtype = WN_rtype(wn);
	TYPE_ID desc = WN_desc(wn);
	if (MTYPE_bit_size(rtype) > MTYPE_bit_size(desc))
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, MTYPE_bit_size(desc), wn);
      }
      break;

    case RROTATE_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
	wn  = WN_Rrotate (TY_mtype(Get_TY(TREE_TYPE(exp))), wn0, wn1);

	TYPE_ID rtype = WN_rtype(wn);
	TYPE_ID desc = WN_desc(wn);
	if (MTYPE_bit_size(rtype) > MTYPE_bit_size(desc))
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, MTYPE_bit_size(desc), wn);

      }
      break;

    case RSHIFT_EXPR:
      {
	TYPE_ID mtyp = Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp))));
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));
        wn  = WN_Binary (MTYPE_signed(mtyp) ? OPR_ASHR : OPR_LSHR,
                         mtyp, wn0, wn1);
      }
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      {
	Push_Conditional_Cleanup_Level();
	wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type, target);
	Pop_Conditional_Cleanup_Level();
	Push_Conditional_Cleanup_Level();
	wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						   Boolean_type, target);
	Pop_Conditional_Cleanup_Level();
	if (WN_rtype(wn0)==MTYPE_XTBOOL)
	  wn0 = WN_Cvt(WN_rtype(wn0), MTYPE_I4, wn0);
	if (WN_rtype(wn1)==MTYPE_XTBOOL)
	  wn1 = WN_Cvt(WN_rtype(wn1), MTYPE_I4, wn1);
        wn  = WN_Binary (Operator_From_Tree [code].opr,
                         Boolean_type, wn0, wn1);
        if (Boolean_type != MTYPE_B &&
	    Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))) != Boolean_type)
	  wn = WN_Cvt (Boolean_type, Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))), wn);
      }
      break;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      {
        wn0 = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1));

	// check if conversion is needed
	ty_idx = Get_TY (TREE_TYPE(exp));
        TYPE_ID mtyp = TY_mtype(ty_idx);
	TY_IDX ty_idx0 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 0)));
	TYPE_ID mtyp0 = TY_mtype(ty_idx0);
	TY_IDX ty_idx1 = Get_TY(TREE_TYPE(TREE_OPERAND (exp, 1)));
	TYPE_ID mtyp1 = TY_mtype(ty_idx1);

	if (MTYPE_size_min(mtyp1) > MTYPE_size_min(mtyp0) &&
	    ! Has_Subsumed_Cvtl(WN_operator(wn0)) &&
            Widen_Mtype(mtyp0) != mtyp0)
	  wn0 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp0), MTYPE_V,
			      MTYPE_size_min(mtyp0), wn0);
	if (MTYPE_size_min(mtyp0) > MTYPE_size_min(mtyp1) &&
	    ! Has_Subsumed_Cvtl(WN_operator(wn1)) &&
            Widen_Mtype(mtyp1) != mtyp1)
	  wn1 = WN_CreateCvtl(OPR_CVTL, Widen_Mtype(mtyp1), MTYPE_V,
			      MTYPE_size_min(mtyp1), wn1);

#ifdef TARG_XTENSA
	// gcc front end some times fold expressions (in fold-const.c)
	// using integer_zero_node or integer_one_node which expands to
	// I4INTCONST
	// we need to widen it if the other operand is I8/U8 (see PR11571)

	if (MTYPE_is_longlong(WN_rtype(wn0)) &&
	    WN_operator(wn1)==OPR_INTCONST &&
	    !MTYPE_is_longlong(WN_rtype(wn1))) {
	  TYPE_ID wn0_mtype = WN_rtype(wn0);
	  TYPE_ID wn1_mtype = WN_rtype(wn1);
	  wn1_mtype = Mtype_TransferSize(wn0_mtype, wn1_mtype);
	  WN_set_rtype(wn1, wn1_mtype);
	}
#endif

	wn = WN_CreateExp2(Operator_From_Tree [code].opr, Boolean_type,
			   Widen_Mtype(mtyp0), wn0, wn1);
        if (Widen_Mtype(mtyp) != Boolean_type)
	  wn = WN_Cvt(Boolean_type, Widen_Mtype(mtyp), wn);
      }
      break;

    case COND_EXPR:
      {
	ty_idx = Get_TY (TREE_TYPE(exp));

	Push_Conditional_Cleanup_Level();
	wn0 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 0),
						   Boolean_type, NULL);
	Pop_Conditional_Cleanup_Level();

	if (TY_mtype (ty_idx) == MTYPE_V) {
	  WN *then_block = WN_CreateBlock ();
	  WN *else_block = WN_CreateBlock ();
	  WN *if_stmt    = WN_CreateIf (wn0, then_block, else_block);
	  WFE_Stmt_Append (if_stmt, Get_Srcpos());
	  WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos());
	  wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1), NULL, FALSE);
	  if (wn1) {
	    wn1 = WN_CreateEval (wn1);
	    WFE_Stmt_Append (wn1, Get_Srcpos());
	  }
	  WFE_Stmt_Pop (wfe_stmk_if_then);
	  WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
	  wn2 = WFE_Expand_Expr (TREE_OPERAND (exp, 2), NULL, FALSE);
	  if (wn2) {
	    wn2 = WN_CreateEval (wn2);
	    WFE_Stmt_Append (wn2, Get_Srcpos());
	  }
	  WFE_Stmt_Pop (wfe_stmk_if_else);
        }
	else {
	  Push_Conditional_Cleanup_Level();
	  wn1 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 1),
						     TY_mtype (ty_idx), target);
	  Pop_Conditional_Cleanup_Level();
	  Push_Conditional_Cleanup_Level();
	  wn2 = WFE_Expand_Expr_With_Sequence_Point (TREE_OPERAND (exp, 2),
						     TY_mtype (ty_idx), target);
	  Pop_Conditional_Cleanup_Level();
#ifdef TARG_XTENSA
          // Fix for PR 2910: If void COND_EXPR has function references
          // as its arguments, the rtype should also be function reference
          TYPE_ID rtype = Mtype_comparison(TY_mtype(ty_idx));
          if (rtype == MTYPE_UNKNOWN) {
            FmtAssert(TREE_CODE(TREE_OPERAND(exp,1)) == FUNCTION_DECL &&
                      TREE_CODE(TREE_OPERAND(exp,2)) == FUNCTION_DECL &&
                      TY_kind(ty_idx) == KIND_FUNCTION,
                      ("Unknown result type for conditional expression"));
            rtype = Pointer_Mtype;
          }
#endif
	  wn  = WN_CreateExp3 (OPR_CSELECT, rtype, MTYPE_V, wn0, wn1, wn2);
	  Set_PU_has_very_high_whirl (Get_Current_PU ());
        }
      }
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
    {
      tree lhs = TREE_OPERAND (exp, 0);
      tree rhs = TREE_OPERAND (exp, 1);
      tree lhs_type = TREE_TYPE (lhs);

      /* Collect the parts of 'lhs' that should not be initialized or
         copied to by 'rhs'. We use these below to restore 'lhs' after
         initializing/copying to with 'rhs'.  */
      if (TYPE_NONCOPIED_PARTS (lhs_type) != 0)
      {
	if ((code == INIT_EXPR) && !fixed_type_p (rhs))
	{
	  noncopied_parts = WN_CreateBlock();
	  noncopied_parts = collect_noninit_parts (lhs, TYPE_NONCOPIED_PARTS (lhs_type), noncopied_parts);
	}
	else if ((code == MODIFY_EXPR) && ! (fixed_type_p (lhs) && fixed_type_p (rhs)))
	{
	  noncopied_parts = WN_CreateBlock();
	  noncopied_parts = save_noncopied_parts (lhs, TYPE_NONCOPIED_PARTS (lhs_type), noncopied_parts);
	}
      }

      /* When operand 1 (rhs) of an init_expr or modify_expr is a
	 target_expr, then the temporary in the target_expr needs to
	 be replaced by operand 0 (lhs) of the init_expr or
	 modify_expr and the cleanup (operand 2) of the target_expr
	 needs to be zeroed out, since no temporary will be generated
	 so none should be destroyed.  */
      if (TREE_CODE(rhs) == TARGET_EXPR) {
	TREE_OPERAND(rhs, 2) = NULL_TREE;
	if (TREE_CODE(lhs) == VAR_DECL    ||
	    TREE_CODE(lhs) == RESULT_DECL ||
	    TREE_CODE(lhs) == PARM_DECL) {
 	  TREE_OPERAND(rhs, 0) = lhs;
	  wn = WFE_Expand_Expr(rhs);
	  restore_noncopied_parts(noncopied_parts);
	  break;
	}
	// DevWarn ("INIT_EXPR/MODIFY_EXPR kid1 is TARGET_EXPR, kid0 is %s\n",
	//          Operator_From_Tree [TREE_CODE(TREE_OPERAND(exp, 0))].name);
      }
    }
		
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      {
	if (TREE_CODE(TREE_OPERAND(exp, 1)) == ERROR_MARK)
	    break;
        wn1 = WFE_Expand_Expr (TREE_OPERAND (exp, 1)); // r.h.s.
        if (code == MODIFY_EXPR && field_id != 0  && need_result) {
          // this needs to be special-cased to handle the idiom
          // (a = b).x   (where a and b are structs)
          wn = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND(exp, 0), FALSE,
                                      0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
          wn = WFE_Component_Ref_Of_Modify_Expr(WN_last(WFE_Stmt_Top()),
                                                component_ty_idx,
                                                component_offset,
                                                field_id,
                                                is_bit_field);
        }
        else {
          wn = WFE_Lhs_Of_Modify_Expr(code, TREE_OPERAND(exp, 0), need_result, 
                                      0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
        }

	restore_noncopied_parts(noncopied_parts);
      }
      break;

    // ternary ops

    case BIT_FIELD_REF:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), NULL, TRUE, nop_ty_idx, 
			      component_ty_idx, component_offset,
			      field_id, FALSE);
	INT bofst = Get_Integer_Value(TREE_OPERAND(exp, 2));
	INT bsiz =Get_Integer_Value(TREE_OPERAND(exp, 1));
	if ((WN_operator(wn) == OPR_LDID) || (WN_operator(wn) == OPR_ILOAD)) {
	  ty_idx = Get_TY (TREE_TYPE(exp));
	  TYPE_ID rtype = TY_mtype(ty_idx);
	  UINT siz = TY_size(ty_idx);
	  TYPE_ID desc;
	  if (siz <= 8) {
	    if (MTYPE_signed(rtype))
	      desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_INTEGER);
	    else desc = Mtype_AlignmentClass(siz, MTYPE_CLASS_UNSIGNED_INTEGER);
	    rtype = Widen_Mtype(desc);
	  }
	  else desc = rtype;
	  WN_set_rtype(wn, rtype);
	  WN_set_desc(wn, desc);
	  if ((bsiz & 7) == 0 &&	// field size multiple of bytes
	      MTYPE_size_min(desc) % bsiz == 0 && // accessed loc multiple of bsiz
	      bofst % bsiz == 0) {		// bofst multiple of bsiz
	    // not really a bit-field extraction!
	    if (MTYPE_signed(rtype))
	      WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3, MTYPE_CLASS_INTEGER));
	    else WN_set_desc(wn, Mtype_AlignmentClass(bsiz >> 3, MTYPE_CLASS_UNSIGNED_INTEGER));
	    WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
	  } else {
	    if (WN_operator(wn) == OPR_LDID)
	      WN_set_operator(wn, OPR_LDBITS);
	    else WN_set_operator(wn, OPR_ILDBITS);
	    WN_set_bit_offset_size(wn, bofst, bsiz);
	  }
	  if (MTYPE_byte_size (WN_desc(wn)) != TY_size(WN_ty(wn)))
	    // the container is smaller than the entire struct
	    WN_set_ty (wn, MTYPE_To_TY (WN_desc(wn)));
	} else {
	  if (WN_rtype(wn)==MTYPE_XTBOOL)
	    wn = WN_Cvt(WN_rtype(wn), MTYPE_I4, wn);
	  if (bsiz < 32) {
	    wn  = WN_CreateExp1 (OPR_EXTRACT_BITS,
				 Widen_Mtype(TY_mtype(Get_TY(TREE_TYPE(exp)))),
				 MTYPE_V, wn);
	    WN_set_bit_offset_size(wn, bofst, bsiz);
	  }
	}
      }
      break;

    // n-ary ops

    case ARRAY_REF:
      {
	UINT xtra_BE_ofst = 0; 	// only needed for big-endian target
	TY_IDX elem_ty_idx;
	// generate the WHIRL array node
        wn0 = WFE_Array_Expr(exp, &elem_ty_idx, 0, 0, 0);

	// generate the iload node
	TY_IDX hi_ty_idx = Get_TY (TREE_TYPE(exp));
	desc_ty_idx = component_ty_idx;
	if (desc_ty_idx == 0)
          desc_ty_idx = hi_ty_idx;

        TY_IDX iload_ty_idx = desc_ty_idx;

        if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
	  ty_idx = desc_ty_idx;
	else {
	  ty_idx = nop_ty_idx;
	  if (ty_idx == 0) 
	    ty_idx = desc_ty_idx;
	}

        UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
	if (! is_bit_field) {
	  if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
            /* For non-volatiles, we could use smaller load when the result
               is smaller. However, it seems like this optimization can and
               should be done in the back-end. */
            cvtl_size = TY_size(ty_idx) * 8;
            ty_idx = desc_ty_idx;
	  }
	}
        else {
          if (TY_size(desc_ty_idx) > TY_size(ty_idx))
            ty_idx = desc_ty_idx;
        }

        TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
        TYPE_ID desc = TY_mtype(desc_ty_idx);
        if (MTYPE_is_integral(desc)) {
          if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
            if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		is_bit_field)
              rtype = Mtype_TransferSign(desc, rtype);
            else desc = Mtype_TransferSign(rtype, desc);
          }
        }

	if (TREE_THIS_VOLATILE(exp) || TY_is_volatile(component_ty_idx)) {
	  Set_TY_is_volatile(elem_ty_idx);
	  Set_TY_is_volatile(hi_ty_idx);
	}

	if (is_bit_field && field_id > MAX_FIELD_ID)
	  error("field id for bit-field exceeds limit %d", MAX_FIELD_ID);

	wn = WN_CreateIload(OPR_ILOAD, rtype,
			    is_bit_field ? MTYPE_BS : desc, 
			    component_offset+xtra_BE_ofst,
                            field_id != 0 ? hi_ty_idx : iload_ty_idx, 
			    Make_Pointer_Type(elem_ty_idx, FALSE),
			    wn0, field_id);
        if (cvtl_size != 0)
	  wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
      }
      break;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      {
	tree arglist = TREE_OPERAND (exp, 1);
        TYPE_ID ret_mtype;
        WN *call_wn;
        WN *arg_wn;
	TY_IDX  arg_ty_idx;
        TYPE_ID arg_mtype;
	ST *invret_st = NULL;
	bool invisible_arg = false;
        INT num_args = 0;
	INT num_handlers = 0;
	TIE_MACRO* tie_macro=NULL;
        INT i;
	tree list, next;
	arg0 = TREE_OPERAND (exp, 0);
	enum tree_code code0 = TREE_CODE (arg0);
	for (list = TREE_OPERAND (exp, 1); list; list = TREE_CHAIN (list)) {
          arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
          if (!WFE_Keep_Zero_Length_Structs    &&
              TY_mtype (arg_ty_idx) == MTYPE_M &&
              TY_size (arg_ty_idx) == 0) {
            // zero length struct parameter
          }
          else
            num_args++;
        }
        ty_idx = Get_TY(TREE_TYPE(exp));
        if (need_result) {
          if (!WFE_Keep_Zero_Length_Structs  &&
              TY_mtype (ty_idx) == MTYPE_M   &&
              TY_size (ty_idx) == 0) {
            // zero length struct return
            ret_mtype = MTYPE_V;
          }
          else
            ret_mtype = TY_mtype (ty_idx);
        }
        else
          ret_mtype = MTYPE_V;
        st = NULL;
        if (code0 == ADDR_EXPR                  &&
            TREE_CODE (TREE_OPERAND (arg0, 0))) {
	  tree func = TREE_OPERAND (arg0, 0);
	  BOOL intrinsic_op = FALSE;
          BOOL whirl_generated = FALSE;
	  INTRINSIC iopc = INTRINSIC_NONE;

	  if (DECL_BUILT_IN (func)) {

	    if (DECL_BUILT_IN_CLASS (func) == BUILT_IN_MD)

	      switch (DECL_FUNCTION_CODE (func)) {

	      case XTENSA_BUILTIN_UMULSIDI3:
		if (xt_mul32h) {
		  arg1 = TREE_VALUE (arglist);
		  arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		  WN *arg1_wn = WFE_Expand_Expr(arg1);
		  WN *arg2_wn = WFE_Expand_Expr(arg2);
		  arg1_wn = WN_Cvt(WN_rtype(arg1_wn), MTYPE_U8, arg1_wn);
		  arg2_wn = WN_Cvt(WN_rtype(arg2_wn), MTYPE_U8, arg2_wn);
		  wn = WN_Mpy(MTYPE_U8, arg1_wn, arg2_wn);
		  whirl_generated = TRUE;
		} else {
		  iopc = INTRN_UMULSIDI3;
		  intrinsic_op = TRUE;
		}
		break;

	      default:
		DevWarn ("Encountered BUILT_IN_MD: %d at line %d\n",
			 DECL_FUNCTION_CODE (func), lineno);
		break;
	      }
	    else switch (DECL_FUNCTION_CODE (func)) {

	      case END_BUILTINS:
		break;

	      case BUILT_IN_STDARG_START:
	      {
		arg1 = TREE_VALUE (arglist);
		arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		WN *arg_wn = WFE_Expand_Expr (arg1);
		ST *st1 = WN_st (arg_wn);
		while (TREE_CODE (arg2) == NOP_EXPR
		       || TREE_CODE (arg2) == CONVERT_EXPR
		       || TREE_CODE (arg2) == NON_LVALUE_EXPR
		       || TREE_CODE (arg2) == INDIRECT_REF)
		  arg2 = TREE_OPERAND (arg2, 0);
		ST *st2 = Get_ST (arg2);
		wn = WN_Lda (Pointer_Mtype, 
                             ((TY_size (ST_type (st2)) + 7) & (-8)),
                             st2);
		wn = WN_Stid (Pointer_Mtype, 0, st1, ST_type (st1), wn);
		WFE_Stmt_Append (wn, Get_Srcpos());
		whirl_generated = TRUE;
		wn = NULL;
		break;
	      }

	      case BUILT_IN_VA_END:
	      {
		whirl_generated = TRUE;
		break;
	      }

  	      case BUILT_IN_SAVEREGS:
	      {
		iopc = INTRN_SAVEREGS;
		break;
	      }

	      case BUILT_IN_NEXT_ARG:
	      {
                tree last_parm = tree_last 
				   (DECL_ARGUMENTS (Current_Function_Decl()));
		while (TREE_CODE (last_parm) == NOP_EXPR
		       || TREE_CODE (last_parm) == CONVERT_EXPR
		       || TREE_CODE (last_parm) == NON_LVALUE_EXPR
		       || TREE_CODE (last_parm) == INDIRECT_REF)
		  last_parm = TREE_OPERAND (last_parm, 0);
		ST *st = Get_ST (last_parm);
		arg_wn = WN_Lda (Pointer_Mtype, ST_ofst(st), st);
		wn = WN_Binary (OPR_ADD, Pointer_Mtype, arg_wn,
				WN_Intconst (Pointer_Mtype,
					     Parameter_Size(ST_size(st))));
                whirl_generated = TRUE;
		break;
	      }

              case BUILT_IN_ALLOCA:
		Set_PU_has_alloca (Get_Current_PU ());
		Set_PU_has_user_alloca (Get_Current_PU ());
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
	        wn = WN_CreateAlloca (arg_wn);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_MEMCPY:
		iopc = INTRN_MEMCPY;
                break;

              case BUILT_IN_MEMCMP:
		iopc = INTRN_MEMCMP;
                break;

              case BUILT_IN_MEMSET:
		iopc = INTRN_MEMSET;
                break;

              case BUILT_IN_STRCPY:
		iopc = INTRN_STRCPY;
                break;

              case BUILT_IN_STRCMP:
#ifdef GPLUSPLUS_FE
		iopc = INTRN_STRCMP;
#else
		if (arglist == 0
		    /* Arg could be non-pointer if user redeclared this fcn wrong.  */
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
		    || TREE_CHAIN (arglist) == 0
		    || TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arglist)))) != POINTER_TYPE)
		  break;
		else {
		  arg1 = TREE_VALUE (arglist);
		  arg2 = TREE_VALUE (TREE_CHAIN (arglist));
		  tree len1 = c_strlen (arg1);
		  if (len1) {
		    tree len2 = c_strlen (arg2);
		    if (len2) {
		      char *ptr1 = get_string_pointer (WFE_Expand_Expr (arg1));
		      char *ptr2 = get_string_pointer (WFE_Expand_Expr (arg2));
		      if (ptr1 && ptr2) {
			wn = WN_Intconst (MTYPE_I4,
					  strcmp (ptr1, ptr2));
			whirl_generated = TRUE;
			break;
		      }
		    }
		  }
		  iopc = INTRN_STRCMP;
//		  intrinsic_op = TRUE;
		}
#endif /* GPLUSPLUS_FE */
                break;

              case BUILT_IN_STRLEN:
#ifdef GPLUSPLUS_FE
		iopc = INTRN_STRLEN;
#else
		if (arglist == 0
		/* Arg could be non-pointer if user redeclared this fcn wrong.  */
		   || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE)
		  break;
		else {
		  tree src = TREE_VALUE (arglist);
		  tree len = c_strlen (src);
		  if (len) {
		    wn = WFE_Expand_Expr (len);
		    whirl_generated = TRUE;
		  }
		  else {
		    iopc = INTRN_STRLEN;
//		    intrinsic_op = TRUE;
		  }
		}
#endif /* GPLUSPLUS_FE */
                break;

              case BUILT_IN_FSQRT:
                arg_wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                wn = WN_CreateExp1 (OPR_SQRT, ret_mtype, MTYPE_V, arg_wn);
                whirl_generated = TRUE;
		break;

              case BUILT_IN_SIN:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		     if (ret_mtype == MTYPE_F4) iopc = INTRN_F4SIN;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8SIN;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'sin'");
		intrinsic_op = TRUE;
                break;

              case BUILT_IN_COS:
		if (ret_mtype == MTYPE_V) ret_mtype = MTYPE_F8;
		     if (ret_mtype == MTYPE_F4) iopc = INTRN_F4COS;
                else if (ret_mtype == MTYPE_F8) iopc = INTRN_F8COS;
                else Fail_FmtAssertion ("unexpected mtype for intrinsic 'cos'");
		intrinsic_op = TRUE;
                break;

              case BUILT_IN_CONSTANT_P:
              {
		DevWarn ("Encountered BUILT_IN_CONSTANT_P: at line %d\n",
                         lineno);
                tree arg = TREE_VALUE (TREE_OPERAND (exp, 1));
                STRIP_NOPS (arg);
#ifdef __ADDR_OF_GLOBAL_IS_CONSTANT__
                // This test would allow the address of a global variable
                // to be treated as constant which causes failure in PR 3456.
                if (really_constant_p (arg)
#else
                if (TREE_CODE_CLASS (TREE_CODE (arg)) == 'c'
                    || (TREE_CODE (arg) == CONSTRUCTOR && TREE_CONSTANT (arg))
#endif
                    || (TREE_CODE (arg) == ADDR_EXPR
                        && TREE_CODE (TREE_OPERAND (arg, 0)) == STRING_CST))
                  wn = WN_Intconst (MTYPE_I4, 1);

                else
                  wn = WN_Intconst (MTYPE_I4, 0);
//                wn = WFE_Expand_Expr (TREE_VALUE (TREE_OPERAND (exp, 1)));
                whirl_generated = TRUE;
                break;
              }

              case BUILT_IN_RETURN_ADDRESS:
		if (TREE_CODE(TREE_VALUE(TREE_OPERAND(exp, 1))) != INTEGER_CST)
		  exit (RC_USER_ERROR);
                i = Get_Integer_Value (TREE_VALUE (TREE_OPERAND (exp, 1)));
		if (strcmp(ABI_Name, "windowed") == 0) {
		  if (i < 0 || i > 100) {
		    warning("__builtin_return_address(%d) will return 0", i);
		    wn = WN_Intconst(Pointer_Mtype, 0);
		  }
		  else {
		    extern WN *WFE_Builtin_Return_Address(INT);
		    wn = WFE_Builtin_Return_Address(i);
		  }
		}
		else {
		  assert(strcmp(ABI_Name, "call0") == 0);
		  if (i > 0) {
		    // currently don't handle levels > 0,
		    // which requires iterating thru call-stack
		    // and finding $ra in fixed place.
		    warning("non-zero levels not supported for builtin_return_address");
		    wn = WN_Intconst(Pointer_Mtype, 0);
		  }
		  else {
		    st = WFE_Get_Return_Address_ST (i);
		    wn = WN_Ldid (Pointer_Mtype, 0, st, ST_type (st));
		  }
		}
                whirl_generated = TRUE;
		break;

#if 0
              case BUILT_IN_LOCK_TEST_AND_SET:
                wn = emit_builtin_lock_test_and_set (exp, num_args-2);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_LOCK_RELEASE:
                emit_builtin_lock_release (exp, num_args-1);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_COMPARE_AND_SWAP:
                wn = emit_builtin_compare_and_swap (exp, num_args-3);
                whirl_generated = TRUE;
                break;

              case BUILT_IN_SYNCHRONIZE:
                emit_builtin_synchronize (exp, num_args);
                whirl_generated = TRUE;
                break;

#endif

	      case BUILT_IN_FFS:
		/* GCC will expand this in-line if the NSA option is available
		   but it probably doesn't really matter if it stays a
		   function call here.  */
		iopc = INTRN_I4FFS;
		intrinsic_op = TRUE;
		break;

	      case BUILT_IN_CLZ:
		if (xt_nsa) {
		  tie_macro = tie_info->tie_macro("_TIE_xt_misc_NSAU");
		  iopc = Tie_Macro_Id_To_Intrinsic(tie_macro->id());
		} else {
		  iopc = INTRN_I4CLZ;
		}
		intrinsic_op = TRUE;
		break;

	      case BUILT_IN_CTZ:
		/* GCC will expand this in-line if the NSA option is available
		   but it probably doesn't really matter if it stays a
		   function call here.  */
		iopc = INTRN_I4CTZ;
		intrinsic_op = TRUE;
		break;

	      case BUILT_IN_TIE_MACRO:
		tie_macro =
		  tie_info->tie_macro(IDENTIFIER_POINTER(DECL_NAME(func)));
		iopc = Tie_Macro_Id_To_Intrinsic(tie_macro->id());

		// we generate intrinsic op if the tie macro
		//  1. is pure
		//  2. has no side effect
		// otherwise, we generate intrinsic call

		// in case of intrinsic op, the out argument is NOT generated
		// but the inout argument is generated
		if (tie_macro->is_whirl_intrinsic_op()) {
		  intrinsic_op = TRUE;
		} else {
		  // we will generate extra out arg when turning an op into
		  // a call
		  intrinsic_op = FALSE;
		  if (tie_macro->is_c_function())
		    num_args++;
		}
		break;

	      default:
		DevWarn ("Encountered BUILT_IN: %d at line %d\n",
			 DECL_FUNCTION_CODE (func), lineno);
		break;
            }
	  }

          if (whirl_generated) {
            break;
          }

	  if (intrinsic_op) {
	    WN *ikids [128];
	    UINT8 proto_index = 0;
	    INT8 inout_index = -1;
	    WN* tie_arg_wn[128];
	    for (i = 0, list = TREE_OPERAND (exp, 1);
		 list;
		 i++, list = TREE_CHAIN (list)) {

	      arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
	      if (i==128) {
		error (
		  "too many (>128) arguments to TIE intrinsic %s",
		  tie_macro->name());
		exit (RC_USER_ERROR);
	      }
	      tie_arg_wn[i]=arg_wn;
	      arg_ty_idx = Get_TY(TREE_TYPE(TREE_VALUE(list)));
	      arg_mtype  = TY_mtype(arg_ty_idx);
	      arg_wn     = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
					  arg_ty_idx, WN_PARM_BY_VALUE);

	      ikids [i]  = arg_wn;
	    }

	    bool packed_output = false;
	    if (tie_macro) {

	      if (MTYPE_is_tie_packed(tie_macro->output_mtype(tie_info)))
		packed_output = true;

	      if (tie_macro->is_c_function() == false) {

		// this TIE intrinsic is not a C function but is a WHIRL intr op
		// because it has either no output or 1 inout or >1 output argument

		if (i < tie_macro->num_protos()) {
		  // the following is already reported by gnu
		  //error("too few arguments to TIE intrinsic %s", tie_macro->name());
		  exit (RC_USER_ERROR);
		}
		if (i > tie_macro->num_protos()) {
		  error("too many arguments to TIE intrinsic %s", tie_macro->name());
		  exit (RC_USER_ERROR);
		}
	      } else {
	        if (i < tie_macro->num_protos()-1) {
		  // the following is already reported by gnu
	          //error("too few arguments to TIE intrinsic %s", tie_macro->name());
		  exit (RC_USER_ERROR);
		}
	        if (i > tie_macro->num_protos()-1) {
	          error("too many arguments to TIE intrinsic %s", tie_macro->name());
		  exit (RC_USER_ERROR);
		}
	      }

	      int arg_index=0;
	      int proto_index=0;
	      for (proto_index=0;
		   proto_index < tie_macro->num_protos();
		   proto_index ++) {
		FmtAssert(tie_macro->proto_is_label(proto_index)==false,
			  ("TIE branch cannot be an intrinsic op"));
		if (tie_macro->proto_is_out(proto_index)) {
		  if (packed_output)
		    arg_index++;
		  continue;
		}
		if (tie_macro->proto_is_in(proto_index) &&
		    tie_macro->proto_is_immed(proto_index)) {
		  if (WN_opcode(tie_arg_wn[arg_index])!=OPC_I4INTCONST) {
		    error("expecting integer constant for TIE intrinsic %s immediate argument %d", tie_macro->name(), arg_index);
		    exit (RC_USER_ERROR);
		  }
		  INT64 imm = WN_const_val(tie_arg_wn[arg_index]);
		  if (tie_macro->immediate_ok(proto_index,imm)==FALSE) {
		    TIE_MACRO* tie_macro_1 = tie_info->immediate_to_register_form(tie_macro);
		    if (tie_macro_1 == NULL) {
		      error("integer immediate '%" LLD_FMT "' for TIE intrinsic %s out of range", imm,  tie_macro->name());
		      exit (RC_USER_ERROR);
		    } else {
		      static int tmp_count = 0;
		      char buf[64];

		      TYPE_ID mtype = tie_macro->proto_mtype_id(tie_info, proto_index);
		      ST* preg_st = MTYPE_To_PREG(mtype);
		      TY_IDX preg_ty = MTYPE_To_TY(mtype);
		      if (mtype!=MTYPE_I4 && mtype!=MTYPE_U4) {
			error("integer immediate '%" LLD_FMT "' for TIE intrinsic %s out of range", imm,  tie_macro->name());
			exit (RC_USER_ERROR);
		      }

		      sprintf(buf, "__imm_2_reg_x_%d\0", tmp_count++);

		      PREG_NUM preg_num = Create_Preg(mtype, buf);
		      WN* old_value = tie_arg_wn[arg_index];
		      WN* stid = WN_StidPreg(mtype, preg_num, old_value);
		      WN_Set_Linenum (stid, Get_Srcpos());
		      WFE_Stmt_Append (stid, Get_Srcpos());
		      WN* ldid = WN_LdidPreg(mtype, preg_num);
		      tie_arg_wn[arg_index] = ldid;
		      WN* arg_wn = WN_CreateParm (mtype, ldid, MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
		      ikids [arg_index]  = arg_wn;
		      tie_macro = tie_macro_1;
		      iopc = Tie_Macro_Id_To_Intrinsic(tie_macro->id());
		    }
		  }
		}
		arg_index++;
	      }
	    }

	    if (tie_macro && tie_macro->is_c_function() == false) {
		// we are turning a TIE intr call to op
		// there are two cases:
		// case 1. tie_macro() becomes
		//	   eval tie_macro()
		// case 1. tie_macro(a,b,c) becomes
		//	   b = tie_macro(a,b,c) assuming b is the inout arg
		// case 2. tie_macro(a,b,c,d) becomes
		//	   T = tie_macro(c,d)
		//	   a = outpart(T,1)
		//	   b = outpart(T,2) assuming a, b are output args

		int inout_index;
		WN* store=NULL;
		WN* load;

		// the following are for packed output
		ST* packed_st = NULL;
		TY_IDX packed_ty = 0;
		PREG_NUM packed_preg = 0;

		inout_index = tie_macro->unique_out_or_inout();

		TYPE_ID new_ret_mtype;

		if (tie_macro->no_output()) {
		  // case 1
		  wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, MTYPE_I4,
						MTYPE_V, iopc, num_args, ikids);
		  wn = WN_CreateEval (wn);
		  WN_Set_Linenum (wn, Get_Srcpos());
		  WFE_Stmt_Append (wn, Get_Srcpos());

		} else if (packed_output == false) {
		  // case 2
		  new_ret_mtype = tie_macro->proto_mtype_id(tie_info,inout_index);
		  wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, new_ret_mtype,
				  MTYPE_V, iopc, num_args, ikids);

		  // TIE macros are treated as cg macro therefore is not lowered
		  // so we have to convert any I1/I2/U1/U2 types to I4/U4 with
		  // CVTL to make optimizer happy

		  if (new_ret_mtype==MTYPE_I1 || new_ret_mtype==MTYPE_I2 ||
		      new_ret_mtype==MTYPE_U1 || new_ret_mtype==MTYPE_U2) {
		    int cvtl_size= MTYPE_bit_size(new_ret_mtype);
		    WN_set_rtype(wn, Mtype_TransferSign(new_ret_mtype, MTYPE_I4));
		    wn = WN_CreateCvtl(
				OPR_CVTL, WN_rtype(wn), MTYPE_V, cvtl_size, wn);
		  }

		} else {
		  // case 3

		  // remove the output arguments
		  int num_in_args=0;
		  for (int i=0; i<num_args; i++) {
		    if (tie_macro->proto_is_out(i))
		      continue;
		    ikids[num_in_args++] = ikids[i];
		  }

		  new_ret_mtype = tie_macro->output_mtype(tie_info);
		  wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, new_ret_mtype,
					MTYPE_V, iopc, num_in_args, ikids);

		  packed_st = MTYPE_To_PREG(new_ret_mtype);
		  packed_ty = MTYPE_To_TY(new_ret_mtype);

		  packed_preg = Create_Preg(new_ret_mtype, "tie_intr_struct_out");
		  WN* intr_op_stid =
			WN_Stid(new_ret_mtype, packed_preg, packed_st,
				packed_ty, wn, 0);
		  wn0 = WN_CreateBlock ();
		  WN_Set_Linenum (wn0, Get_Srcpos());
		  WN_INSERT_BlockLast (wn0, intr_op_stid);

		  WFE_Stmt_Append (wn0, Get_Srcpos());
		}

		int outpart_index=0;
		for (i=0; i<num_args; i++) {
		  if (tie_macro->proto_is_in(i))
		    continue;

		  load = tie_arg_wn[i];
		  store = NULL;

		  TYPE_ID proto_mtype = MTYPE_UNKNOWN;
		  WN* ldid = NULL;
		  if (inout_index != TIE_INVALID_ID) {
		    FmtAssert(inout_index==i,
			      ("Bad index (%d) for inout argument in %s",
				i, tie_macro->name()));
		    proto_mtype = new_ret_mtype;
		    ldid = wn;
		  } else {
		    proto_mtype = tie_macro->proto_mtype_id(tie_info, i);
		    ldid = WN_Ldid (new_ret_mtype, packed_preg,
					packed_st, packed_ty, 0);
		    ldid = WN_CreateOutpart(ldid, ++outpart_index, proto_mtype);

		    // TIE macros are treated as cg macro therefore is not lowered
		    // so we have to convert any I1/I2/U1/U2 types to I4/U4 with
		    // CVTL to make optimizer happy

		    if (proto_mtype==MTYPE_I1 || proto_mtype==MTYPE_I2 ||
			proto_mtype==MTYPE_U1 || proto_mtype==MTYPE_U2) {
		      int cvtl_size= MTYPE_bit_size(proto_mtype);
		      WN_set_rtype(ldid, Mtype_TransferSign(proto_mtype, MTYPE_I4));

		      ldid = WN_CreateCvtl(OPR_CVTL, WN_rtype(ldid), MTYPE_V, cvtl_size, ldid);
		    }
		  }

		  /* handle built-in conversion CVTLs */
		  while (WN_operator(load)==OPR_CVTL)
		    load = WN_kid0(load);

		  char maybe_pointer = tie_macro->proto_is_pointer(i)?'*':' ';
		  if (WN_operator(load)==OPR_LDID) {
		    if (WN_desc(load) != proto_mtype
		      && (MTYPE_is_tie(WN_desc(load)) ||
			  MTYPE_is_tie(proto_mtype))) {
		      error("implicit type conversion at an inout or out argument of type \"%s%c\" not allowed for TIE intrinsic %s", 
			    tie_macro->proto_type_demangled_name(i), 
			    maybe_pointer, tie_macro->name());
		      exit (RC_USER_ERROR);
		    }
		    store = WN_Stid(WN_desc(load),
				    WN_offset(load), WN_st(load), WN_ty(load),
				    ldid, WN_field_id(load));
		  } else if (WN_operator(load)==OPR_ILOAD) {
		    if (WN_desc(load) != proto_mtype
		      && (MTYPE_is_tie(WN_desc(load)) ||
			  MTYPE_is_tie(proto_mtype))) {
		      error("implicit type conversion at an inout or out argument of type \"%s%c\" not allowed for TIE intrinsic %s", 
			    tie_macro->proto_type_demangled_name(i), 
			    maybe_pointer, tie_macro->name());
		      exit (RC_USER_ERROR);
		    }
		    store = WN_Istore(WN_desc(load),
				WN_offset(load), WN_load_addr_ty(load),
				WN_COPY_Tree(WN_kid0(load)),
				ldid, WN_field_id(load));
		  } else if (WN_operator(load)==OPR_CVT) {
		    error("type conversion is not allowed at an inout argument of type \"%s%c\" for TIE intrinsic %s", 
			  tie_macro->proto_type_demangled_name(i), 
			  maybe_pointer, tie_macro->name());
		    exit (RC_USER_ERROR);
		  } else {
		    error("expecting an l-value inout argument of type \"%s%c\" for TIE intrinsic %s", 
			  tie_macro->proto_type_demangled_name(i), 
			  maybe_pointer, tie_macro->name());
		    exit (RC_USER_ERROR);
		  }
		  FmtAssert(store,
			    ("Error generating store for %s", tie_macro->name()));

		  WN_Set_Linenum (store, Get_Srcpos());
		  WFE_Stmt_Append (store, Get_Srcpos());
		}

		FmtAssert(ret_mtype==MTYPE_V,
			  ("Return value expected at line", lineno));
		wn = NULL;
	    } else {

	      TYPE_ID new_ret_mtype =  ret_mtype;
	      if (tie_macro && ret_mtype==MTYPE_V) {
		// result is not used
		int inout_index = tie_macro->unique_out_or_inout();
		FmtAssert(inout_index!= TIE_INVALID_ID,
			("Missing inout argument for TIE intrinsic %s", tie_macro->name()));
		new_ret_mtype = tie_macro->proto_mtype_id(tie_info,inout_index);
	      }
	      wn = WN_Create_Intrinsic (OPR_INTRINSIC_OP, new_ret_mtype,
					MTYPE_V, iopc, num_args, ikids);

	      // TIE macros are treated as cg macro therefore is not lowered
	      // so we have to convert any I1/I2/U1/U2 types to I4/U4 with
	      // CVTL to make optimizer happy

	      if (new_ret_mtype==MTYPE_I1 || new_ret_mtype==MTYPE_I2 ||
		  new_ret_mtype==MTYPE_U1 || new_ret_mtype==MTYPE_U2) {
		int cvtl_size= MTYPE_bit_size(new_ret_mtype);
		WN_set_rtype(wn, Mtype_TransferSign(new_ret_mtype, MTYPE_I4));
		wn = WN_CreateCvtl(
			OPR_CVTL, WN_rtype(wn), MTYPE_V, cvtl_size, wn);
	      }
	    }

	    break;
	  }

	  if (iopc) {
	    if (tie_macro) {
	      if (tie_macro->is_conditional_branch()) {
		// for TIE conditional branch intrinsic, it always
		// returns an integer showing which branch is taken
		num_args += tie_macro->num_labels();
		call_wn = WN_Create (OPR_INTRINSIC_CALL, MTYPE_I4, MTYPE_V,
				     num_args);
	      } else {
		// for tie macro, it might be in operator form so a non-void
		// return type does exist but we are turning the operator
		// into a call with no return value because it is
		// not pure or has side effect
		call_wn = WN_Create (OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V,
				     num_args);
	      }
	    } else {
	      call_wn = WN_Create (OPR_INTRINSIC_CALL, ret_mtype, MTYPE_V, num_args);
	    }
	    WN_intrinsic (call_wn) = iopc;
	  }
	  else {
	    /* If the returned type is addressable, then we must
               return it by reference in an invisible first paramater
               instead of by value. Make room for it here... we handle
               it with the args below. */
	    if (WFE_Addressable_Type(TREE_TYPE(exp)))
	    {
	      invisible_arg = true;
	      num_args++;
	      ret_mtype = MTYPE_V;
	    }

	    num_handlers = Current_Handler_Count();
            call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V,
                                 num_args + num_handlers);
	    if (DECL_NORETURN(func)) {
                WN_Set_Call_Never_Return(call_wn);
            }

            st = Get_ST (TREE_OPERAND (arg0, 0));
            WN_st_idx (call_wn) = ST_st_idx (st);
	  }
        }

        else {
	  /* If the returned type is addressable, then we must return
	     it by reference in an invisible first paramater instead
	     of by value. Make room for it here... we handle it with
	     the args below. */
	  if (WFE_Addressable_Type(TREE_TYPE(exp)))
	  {
	    invisible_arg = true;
	    num_args++;
	    ret_mtype = MTYPE_V;
	  }

	  num_args++;
	  num_handlers = Current_Handler_Count();
          call_wn = WN_Create (OPR_ICALL, ret_mtype, MTYPE_V,
			       num_args + num_handlers);
	  WN_kid(call_wn, num_args-1) = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	  WN_set_ty (call_wn, TY_pointed(Get_TY(TREE_TYPE (TREE_OPERAND (exp, 0)))));
	}

	/* If we are returning value by invisible first arg, then we
           must have been given the location to use for that return
           value. */
	FmtAssert(invisible_arg == (target != NULL),
		  ("failed: invisible_arg == (target != NULL)"));

	WN_Set_Call_Default_Flags (call_wn);
	WN_Set_Linenum (call_wn, Get_Srcpos());

        if (st) {
          tree func = TREE_OPERAND (arg0, 0);
          if (DECL_INLINE (func)) {
            wfe_invoke_inliner = TRUE;
          }
        }

	WN* tie_arg_wn[128];
	WN* return_wn=NULL;
	// we do a few things special for TIE macros
	// 1. we create dummy for out arguments to avoid false live-in
	// 2. we create one extra dummy out argument for calls created from
	//    operator (there was one missing arg) and we return a copy of
	//    the load from the dummy as the returned WN
	// 3. we create store after the call for each out/inout argument

        i = 0;
	INT label_count=0;      // count labels for tie branches
	for (list = TREE_OPERAND (exp, 1); list || invisible_arg; list = next) {
	  if (list)
	    next = TREE_CHAIN(list);
	  
	  if (i == 0 && is_aggr_init_via_ctor) {
	    ST * st = Get_ST(TREE_VALUE(list));
	    arg_wn = WN_Lda(Pointer_Mtype, ST_ofst(st), st);
	    arg_ty_idx = Get_TY(
			   build_pointer_type(TREE_TYPE(TREE_VALUE(list))));
	    arg_mtype  = TY_mtype(arg_ty_idx);
	    if (tie_macro)
	      Fail_FmtAssertion("Internal error for TIE intrinsic %s\n",
				tie_macro->name());
	  }
	  else {
	    /* If the argument is addressable, then we must pass it by
               reference instead of by value. This corresponds to what
               is done in calls.cxx::initialize_argument_information
               (there is also a check there for variable sized
               arguments, but I'm not handling those yet). If
               'invisible_arg' is true, then make this arg be the one
               passing the pointer used to return the object. */
	    tree type = TREE_TYPE((invisible_arg) ? exp : (TREE_VALUE (list)));
	    arg_ty_idx = Get_TY(type);
	    arg_mtype  = TY_mtype(arg_ty_idx);
	    
            if (!tie_macro && (invisible_arg || WFE_Addressable_Type(type)))
	    {
	      Is_True(arg_mtype == MTYPE_M,
		      ("expecting addressable type to be MTYPE_M, not %s",
		       MTYPE_name(arg_mtype)));

	      ST *tmp;
	      if (invisible_arg)
	      {
		tmp = target;
		if (ST_mtype(tmp) != MTYPE_M)
		  arg_wn = WN_Ldid (ST_mtype(tmp), ST_ofst(tmp), tmp, ST_type(tmp));
		else
		  arg_wn = WN_Lda (Pointer_Mtype, ST_ofst(tmp), tmp);
	      }
	      else
	      {
		WN *targ = WFE_Expand_Expr (TREE_VALUE (list));
		tmp = WN_st(targ); 
		Set_ST_addr_saved (tmp);
		arg_wn     = WN_Lda (Pointer_Mtype, ST_ofst(tmp), tmp);
	      }

	      arg_ty_idx = Get_TY(build_pointer_type(type));
	      arg_mtype  = TY_mtype(arg_ty_idx);

	      /* Don't do the invisible arg again, instead do the
                 first "real" arg. */
	      if (invisible_arg)
	      {
		invisible_arg = false;
		next = list;
		invret_st = tmp;
	      }
	    }
	    else
	    {
	      arg_wn     = WFE_Expand_Expr (TREE_VALUE (list));
	      if (tie_macro &&
		  (tie_macro->reads_memory() || tie_macro->writes_memory())) {
		if (Contains_Volatile(arg_wn)) {
		  Set_TY_is_volatile(arg_ty_idx);
		}
	      }
	    }

	    if (tie_macro) {
	      // see item 1. in comment above for TIE macro
	      if (tie_macro->is_c_function() &&
		  tie_macro->unique_out_or_inout()==i) {
		i++;
	      }
	      if (i==128) {
		error (
		  "too many (>128) arguments to TIE intrinsic %s",
		  tie_macro->name());
		exit (RC_USER_ERROR);
	      }
	      tie_arg_wn[i]=arg_wn;
	      if (tie_macro->proto_is_out(i)) {
		/* for out only parameter, create a dummy */
		ST* dummy_preg_st;
		PREG_NUM dummy_preg;
		dummy_preg_st = MTYPE_To_PREG(arg_mtype);
		dummy_preg = Create_Preg(arg_mtype, "tie_out_dummy");
		arg_wn = WN_Ldid(arg_mtype, dummy_preg, dummy_preg_st,
			       arg_ty_idx);
	      }
	      arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
				    arg_ty_idx, WN_PARM_BY_VALUE);
	    }
	    else
	    {
	      arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
				      arg_ty_idx, WN_PARM_BY_VALUE);
	    }

	    WN_kid (call_wn, i++) = arg_wn;

	    if (tie_macro && tie_macro->is_conditional_branch()) {
	      while (i<tie_macro->num_protos() &&
		     tie_macro->proto_is_label(i)) {
		WN* label_wn = WN_CreateIntconst(OPC_I4INTCONST, ++label_count);
		tie_arg_wn[i]=label_wn;
		WN_kid (call_wn, i++) = WN_CreateParm(MTYPE_I4, label_wn,
						MTYPE_To_TY(MTYPE_I4),
						WN_PARM_BY_VALUE);
	      }
	    }
	  }
	}

#ifdef ADD_HANDLER_INFO
	if (num_handlers) 
	  Add_Handler_Info (call_wn, i, num_handlers);
#endif

	if (tie_macro && tie_macro->is_c_function()) {
	  // see item 2. in comment above for TIE macro

	  if (tie_macro->unique_out_or_inout()==i)
	    i++;

	  int out_index = tie_macro->unique_out_or_inout();
	  FmtAssert(out_index != TIE_INVALID_ID,
		    ("Missing out index for TIE intrinsic %s",tie_macro->name()));

	  ST* dummy_preg_st;
	  PREG_NUM dummy_preg;
	  TYPE_ID out_mtype = tie_macro->proto_mtype_id(tie_info,out_index);
	  dummy_preg_st = MTYPE_To_PREG(out_mtype);
	  dummy_preg = Create_Preg(out_mtype, "tie_aux_out_dummy");
	  WN* ldid = WN_Ldid(out_mtype, dummy_preg, dummy_preg_st,
			   MTYPE_To_TY(out_mtype));
	  tie_arg_wn[out_index]=ldid;
	  WN_kid (call_wn, out_index) =
		WN_CreateParm (Mtype_comparison(out_mtype), ldid, MTYPE_To_TY(out_mtype),
			       WN_PARM_BY_VALUE);
	  if (ret_mtype == MTYPE_V)
	    return_wn = NULL;
	  else
	    return_wn = WN_COPY_Tree(ldid);
	}

	if (tie_macro) {
	  if (i < tie_macro->num_protos()) {
	    // the following is already reported by gnu
	    //error("too few arguments to TIE intrinsic %s", tie_macro->name());
	    exit (RC_USER_ERROR);
	  }
	  if (i > tie_macro->num_protos()) {
	    error("too many arguments to TIE intrinsic %s", tie_macro->name());
	    exit (RC_USER_ERROR);
	  }
	  int arg_index=0;
	  int proto_index=0;
	  for (proto_index=0;
	       proto_index < tie_macro->num_protos();
	       proto_index ++) {
	    if (tie_macro->proto_is_in(proto_index) &&
		tie_macro->proto_is_immed(proto_index))
	    {
	      if (WN_opcode(tie_arg_wn[arg_index])!=OPC_I4INTCONST) {
		error("expecting integer constant for TIE intrinsic %s immediate argument %d", tie_macro->name(), arg_index);
		exit (RC_USER_ERROR);
	      }
	      INT64 imm = WN_const_val(tie_arg_wn[arg_index]);
	      if (tie_macro->immediate_ok(proto_index,imm)==FALSE) {
		 TIE_MACRO* tie_macro_1 = tie_info->immediate_to_register_form(tie_macro);
		 if (tie_macro_1 == NULL) {
		   error("integer immediate '%" LLD_FMT "' for TIE intrinsic %s out of range", imm,  tie_macro->name());
		   exit (RC_USER_ERROR);
		 } else {
		   static int tmp_count = 0;
		   char buf[64];

		   TYPE_ID mtype = tie_macro->proto_mtype_id(tie_info, proto_index);
		   ST* preg_st = MTYPE_To_PREG(mtype);
		   TY_IDX preg_ty = MTYPE_To_TY(mtype);
		   if (mtype!=MTYPE_I4 && mtype!=MTYPE_U4) {
		     error("integer immediate '%" LLD_FMT "' for TIE intrinsic %s out of range", imm,  tie_macro->name());
		     exit (RC_USER_ERROR);
		   }

		   sprintf(buf, "__imm_2_reg_x_%d\0", tmp_count++);

		   PREG_NUM preg_num = Create_Preg(mtype, buf);
		   WN* old_value = tie_arg_wn[arg_index];
		   WN* stid = WN_StidPreg(mtype, preg_num, old_value);
		   WN_Set_Linenum (stid, Get_Srcpos());
		   WFE_Stmt_Append (stid, Get_Srcpos());
		   WN* ldid = WN_LdidPreg(mtype, preg_num);
		   tie_arg_wn[arg_index] = ldid;
		   WN_kid (call_wn, arg_index) = WN_CreateParm(mtype, ldid, MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
		   tie_macro = tie_macro_1;
		   WN_intrinsic (call_wn) = Tie_Macro_Id_To_Intrinsic(tie_macro->id());
		 }
	      }
	    }
	    arg_index++;
	  }
	}

	if (tie_macro) {
	  // see item 3. in comment above for TIE macro
	  wn0 = WN_CreateBlock ();
	  WN_Set_Linenum (wn0, Get_Srcpos());
	  WN_INSERT_BlockLast (wn0, call_wn);

	  int j=1;
	  if (tie_macro->is_conditional_branch()) {
	    WN* ldid = WN_Ldid (MTYPE_I4, -j, Tie_Output_Volatile_Preg,
					       Tie_Output_Volatile_Type);
	    ST* dummy_preg_st = MTYPE_To_PREG(MTYPE_I4);
	    TY_IDX dummy_preg_ty_idx = MTYPE_To_TY(MTYPE_I4);
	    PREG_NUM dummy_preg = Create_Preg(MTYPE_I4, "tie_aux_out_dummy");
	    WN* stid =
		WN_Stid(MTYPE_I4, dummy_preg, dummy_preg_st, dummy_preg_ty_idx,
					ldid, 0);
	    WN_Set_Linenum (stid, Get_Srcpos());
	    WN_INSERT_BlockLast (wn0, stid);
	    return_wn =
		WN_Ldid(MTYPE_I4, dummy_preg, dummy_preg_st, dummy_preg_ty_idx);
	    j++;
	  }
	  for (i=0; i<tie_macro->num_protos(); i++) {
	    if (tie_macro->proto_is_inout(i) ||
		tie_macro->proto_is_out(i)) {

	      WN* load = tie_arg_wn[i];
	      TYPE_ID out_mtype = WN_rtype(load);
	      TYPE_ID proto_mtype = tie_macro->proto_mtype_id(tie_info, i);

	      TY_IDX out_ty_idx = MTYPE_To_TY(out_mtype);

	      WN* ldid = WN_Ldid (out_mtype, -j, Tie_Output_Volatile_Preg,
						 Tie_Output_Volatile_Type);
	      WN* store = NULL;
	      /* handle built-in conversion CVTLs */
	      while (WN_operator(load)==OPR_CVTL)
		load = WN_kid0(load);

	      char maybe_pointer = tie_macro->proto_is_pointer(i)?'*':' ';
	      if (WN_operator(load)==OPR_LDID) {
		if (WN_desc(load) != proto_mtype
		    && (MTYPE_is_tie(WN_desc(load)) ||
			MTYPE_is_tie(proto_mtype))) {
		  error("implicit type conversion at an out or inout argument of type \"%s%c\" not allowed for TIE intrinsic %s",
			tie_macro->proto_type_demangled_name(i), 
			maybe_pointer, tie_macro->name());
		  exit (RC_USER_ERROR);
	        }

		store = WN_Stid(WN_desc(load),
					WN_offset(load), WN_st(load), WN_ty(load),
					ldid, WN_field_id(load));
	      } else if (WN_operator(load)==OPR_ILOAD) {
		if (WN_desc(load) != proto_mtype
		    && (MTYPE_is_tie(WN_desc(load)) ||
			MTYPE_is_tie(proto_mtype))) {
		  error("implicit type conversion at an out or inout argument of type \"%s%c\" not allowed for TIE intrinsic %s",
			tie_macro->proto_type_demangled_name(i), 
			maybe_pointer, tie_macro->name());
		  exit (RC_USER_ERROR);
	        }

		store = WN_Istore(WN_desc(load),
					WN_offset(load), WN_load_addr_ty(load),
					WN_COPY_Tree(WN_kid0(load)),
					ldid, WN_field_id(load));
	      } else if (WN_operator(load)==OPR_CVT) {
		  error("type conversion is not allowed at an out or inout argument of type \"%s%c\" for TIE intrinsic %s",
			tie_macro->proto_type_demangled_name(i), 
			maybe_pointer, tie_macro->name());
		  exit (RC_USER_ERROR);
	      } else {
		  error("expecting an l-value out or inout argument of type \"%s%c\" for TIE intrinsic %s",
			tie_macro->proto_type_demangled_name(i), 
			maybe_pointer, tie_macro->name());
		  exit (RC_USER_ERROR);
	      }

	      if (store) {
		WN_Set_Linenum (store, Get_Srcpos());
		WN_INSERT_BlockLast (wn0, store);
	      }
	      j++;
	    }
	  }

	  WFE_Stmt_Append (wn0, Get_Srcpos());

	  if (tie_macro->is_c_function() || tie_macro->is_conditional_branch())
	    wn = return_wn;
	}

        else if (ret_mtype == MTYPE_V) {
	  WFE_Stmt_Append (call_wn, Get_Srcpos());

	  /* If we changed this call from one that returned a
             structure by value, to one that returns it using an
             invisible first arg, then return NULL since we assume the
             caller is not expecting a result. */
	  if (invret_st)
	    wn = NULL;
        }

	else {
          wn0 = WN_CreateBlock ();
	  WN_Set_Linenum (wn0, Get_Srcpos());
          WN_INSERT_BlockLast (wn0, call_wn);

	  wn1 = WN_Ldid (ret_mtype, -1, Return_Val_Preg, ty_idx);

	  if (ret_mtype == MTYPE_M) { // copy the -1 preg to a temp area
	    TY_IDX ret_ty_idx = ty_idx;
	    if (Aggregate_Alignment > 0 &&
		Aggregate_Alignment > TY_align (ret_ty_idx))
	      Set_TY_align (ret_ty_idx, Aggregate_Alignment);
            if (TY_align (ret_ty_idx) < MTYPE_align_best(Spill_Int_Mtype))
              Set_TY_align (ret_ty_idx, MTYPE_align_best(Spill_Int_Mtype));
	    ST *ret_st = Gen_Temp_Symbol(ret_ty_idx, 
		  st ? Index_To_Str(Save_Str2(".Mreturn.",
					      ST_name(ST_st_idx(st))))
		     : ".Mreturn.");
	    wn1 = WN_Stid (ret_mtype, 0, ret_st, ty_idx, wn1);
	    WN_Set_Linenum (wn1, Get_Srcpos());
            WN_INSERT_BlockLast (wn0, wn1);

	    // ritual for determining the right mtypes to be used in the LDID
            UINT xtra_BE_ofst = 0;  // only needed for big-endian target
            TY_IDX hi_ty_idx = Get_TY(TREE_TYPE(exp));
            desc_ty_idx = component_ty_idx;
            if (desc_ty_idx == 0)
              desc_ty_idx = hi_ty_idx;
            
            TY_IDX ldid_ty_idx = desc_ty_idx;
        
            if (! MTYPE_is_integral(TY_mtype(desc_ty_idx)))
              ty_idx = desc_ty_idx;
            else { 
              ty_idx = nop_ty_idx;
              if (ty_idx == 0)
                ty_idx = desc_ty_idx;
            }

            UINT cvtl_size = 0; // if non-zero, need to generate CVTL with this size
	    if (! is_bit_field) {
	      if (TY_size(desc_ty_idx) > TY_size(ty_idx)) {
                /* For non-volatiles, we could use smaller load when the result
                   is smaller. However, it seems like this optimization can and
                   should be done in the back-end. */
                cvtl_size = TY_size(ty_idx) * 8;
                ty_idx = desc_ty_idx;
	      }
            }
	    else {
	      if (TY_size(desc_ty_idx) > TY_size(ty_idx))
		ty_idx = desc_ty_idx;
	    }

	    TYPE_ID rtype = Widen_Mtype(TY_mtype(ty_idx));
	    TYPE_ID desc = TY_mtype(desc_ty_idx);
	    if (MTYPE_is_integral(desc)) {
	      if (MTYPE_signed(rtype) != MTYPE_signed(desc)) {
		if (MTYPE_size_min(rtype) > MTYPE_size_min(desc) ||
		    is_bit_field)
		  rtype = Mtype_TransferSign(desc, rtype);
		else desc = Mtype_TransferSign(rtype, desc);
	      }
	    }

            Is_True(! is_bit_field || field_id <= MAX_FIELD_ID,
                    ("WFE_Expand_Expr: field id for bit-field exceeds limit"));
  
	    wn1 = WN_CreateLdid(OPR_LDID, rtype,
			        is_bit_field ? MTYPE_BS : desc,
			        ST_ofst(ret_st)+component_offset+xtra_BE_ofst, 
				ret_st,
				field_id != 0 ? hi_ty_idx : ldid_ty_idx,
				field_id);
	    if (cvtl_size != 0)
	      wn = WN_CreateCvtl(OPR_CVTL, rtype, MTYPE_V, cvtl_size, wn);
	  }

          wn  = WN_CreateComma (OPR_COMMA, WN_rtype (wn1), MTYPE_V,
                                wn0, wn1);
        }
      }
      break;

    case COMPOUND_EXPR:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0), NULL, FALSE);
        if (wn && WN_has_side_effects(wn)) {
          wn = WN_CreateEval (wn);
          WFE_Stmt_Append (wn, Get_Srcpos ());
        }
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 1), NULL, need_result);
      }
      break;

    case NON_LVALUE_EXPR:
      {
        wn = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
      }
      break;

    case SAVE_EXPR:
      {
	// DevWarn ("Encountered SAVE_EXPR at line %d", lineno);
        wn = WFE_Save_Expr (exp, need_result, nop_ty_idx,
			    component_ty_idx, component_offset, field_id);
      }
      break;

    case ERROR_MARK:
      // This is not necessarily an error:  return a constant 0.
      wn = WN_Intconst(MTYPE_I4, 0);
      break;

    case LOOP_EXPR:
      {
        // DevWarn ("Encountered LOOP_EXPR at line %d\n", lineno);
        LABEL_IDX saved_loop_expr_exit_label = loop_expr_exit_label;
        loop_expr_exit_label = 0;
        tree body = LOOP_EXPR_BODY(exp);
        WN *loop_test = WN_Intconst (Boolean_type, 1);
        WN *loop_body = WN_CreateBlock ();
        if (body) {
          WFE_Stmt_Push (loop_body, wfe_stmk_while_body, Get_Srcpos());
          wn = WFE_Expand_Expr (body);
          WFE_Stmt_Pop (wfe_stmk_while_body);
        }
        WN *loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
        WFE_Stmt_Append (loop_stmt, Get_Srcpos());
        if (loop_expr_exit_label)
          WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, loop_expr_exit_label, 0, NULL),
                           Get_Srcpos ());
        loop_expr_exit_label = saved_loop_expr_exit_label;
      }
      break;

    case EXIT_EXPR:
      {
        // DevWarn ("Encountered EXIT_EXPR at line %d\n", lineno);
	WN *test = WFE_Expand_Expr (TREE_OPERAND(exp, 0));
        New_LABEL (CURRENT_SYMTAB, loop_expr_exit_label);
        WN *stmt = WN_CreateTruebr (loop_expr_exit_label, test);
        WFE_Stmt_Append (stmt, Get_Srcpos ());
      }
      break;

    case VA_ARG_EXPR:
      {
        // code swiped from builtins.c (std_expand_builtin_va_arg)
	INT64 align;
	INT64 rounded_size;
	tree type = TREE_TYPE (exp);
	TY_IDX ty_idx = Get_TY (type);
	TYPE_ID mtype = TY_mtype (ty_idx);

        /* Compute the rounded size of the type.  */
	align = PARM_BOUNDARY / BITS_PER_UNIT;
	rounded_size = (((int_size_in_bytes (type) + align - 1) / align) * align);

	/* Get AP.  */
	WN *ap = WFE_Expand_Expr (TREE_OPERAND (exp, 0));
	st = WN_st (ap);

	if (Target_Byte_Sex == BIG_ENDIAN) {
	  Fail_FmtAssertion ("VA_ARG_EXPR not implemented for BIG_ENDIAN");
	  INT64 adj;
	  adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
	  if (rounded_size > align)
	    adj = rounded_size;

	  wn = WN_Binary (OPR_ADD, Pointer_Mtype, wn,
			  WN_Intconst (Pointer_Mtype, rounded_size - adj));
	}

	/* Compute new value for AP.  */
	wn = WN_Binary (OPR_ADD, Pointer_Mtype, ap,
			WN_Intconst (Pointer_Mtype, rounded_size));
	wn = WN_Stid (Pointer_Mtype, 0, st, ST_type (st), wn);
        WFE_Stmt_Append (wn, Get_Srcpos ());
        wn = WN_CreateIload (OPR_ILOAD, Widen_Mtype (mtype), mtype, -rounded_size,
			     ty_idx, Make_Pointer_Type (ty_idx, FALSE),
			     WN_Ldid (Pointer_Mtype, 0, st, ST_type (st)));
      }
      break;

    case UNORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:      
    case UNGE_EXPR:     
    case UNEQ_EXPR:    
        error("built-in ISO C99 unordered compares not supported.");
        return NULL;
        break;


    default:
      {
        Fail_FmtAssertion ("WFE_Expand_Expr: not implemented %s",
                           Operator_From_Tree [code].name);
      }
      break;
    }

#ifdef WFE_DEBUG
  /* Calling fdump_tree will cause be to fail... */
  if (wn)
    fdump_tree (stderr, wn);
  
  fprintf (stderr, // "{("
           ")} WFE_Expand_Expr: %s\n", Operator_From_Tree [code].name);
#endif /* WFE_DEBUG */

  if (need_result)
    FmtAssert (wn != 0 || code == CALL_EXPR || code == BIND_EXPR ||
               code == COMPOUND_STMT ||
               code == STMT_EXPR     ||
 	       code == SUBOBJECT     ||
               code == COMPOUND_EXPR ||
               code == INDIRECT_REF  ||
               code == COMPONENT_REF ||
               code == LOOP_EXPR     ||
               code == NOP_EXPR      ||
               ((code == COND_EXPR) && (TY_mtype (ty_idx) == MTYPE_V)),
	       ("WFE_Expand_Expr: NULL WHIRL tree for %s",
		Operator_From_Tree [code].name));

  return wn;
}

void WFE_One_Stmt (tree exp)
{
  WN *wn;
#ifndef TARG_XTENSA
  /* SAVE_EXPRs can span statements, so we can't reset this. The
     particular case I've found is when constructing an object for a
     throw. */
  wfe_save_expr_stack_last = -1; // to minimize searches
#endif
  Push_Conditional_Cleanup_Level();
  wn = WFE_Expand_Expr_With_Sequence_Point (exp, MTYPE_V, NULL);
  Pop_Conditional_Cleanup_Level();

  if (wn) {
    for (;;) {
      if (WN_operator (wn) == OPR_COMMA) {
	WN *crwn = wn;
	if (WN_operator (WN_kid1 (wn)) == OPR_LDID                 &&
	    WN_st (WN_kid1 (wn)) == Return_Val_Preg                &&
	    (WN_operator (WN_last (WN_kid0 (wn))) == OPR_CALL   ||
	     WN_operator (WN_last (WN_kid0 (wn))) == OPR_ICALL)) {
	  WN_set_rtype (WN_last (WN_kid0 (wn)), MTYPE_V);
	  WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  WN_Delete (crwn);
	  break;
	}
	else {
	  WFE_Stmt_Append (WN_kid0 (wn), Get_Srcpos ());
	  wn = WN_kid1 (wn);
	  WN_Delete (crwn);
	}
      }
      else {
	if (WN_has_side_effects (wn)) {
	  wn = WN_CreateEval (wn);
	  WFE_Stmt_Append (wn, Get_Srcpos ());
	}
	break;
      }
    }
  }
}

void WFE_Null_Return (void)
{
  WN *wn = WN_CreateReturn ();
  WFE_Stmt_Append (wn, Get_Srcpos());
}

UINT64
Get_Integer_Value (tree exp)
{
	FmtAssert (TREE_CODE(exp) == INTEGER_CST, 
		("Get_Integer_Value unexpected tree"));
#ifdef _LP64
	return TREE_INT_CST_LOW (exp);
#else
	UINT64 h = TREE_INT_CST_HIGH (exp);
	UINT64 l = TREE_INT_CST_LOW (exp);
	l = l << 32 >> 32;	// zero-out high 32 bits
	h = h << 32;
	return (h | l);
#endif /* _LP64 */
}

void
WFE_Expr_Init (void)
{
  INT i;
  for (i = 0; i < LAST_CPLUS_TREE_CODE; i++)
    FmtAssert (Operator_From_Tree [i].tree_code == i,
               ("Operator_From_Tree[%d] incorrect, value = %d (last is %d)",
                i, Operator_From_Tree [i].tree_code, LAST_CPLUS_TREE_CODE));
}

char *
WFE_Tree_Node_Name (tree op)
{
  return Operator_From_Tree [TREE_CODE (op)].name;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
