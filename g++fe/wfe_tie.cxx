
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#include <sys/types.h>
#include <cmplrs/rcodes.h>		// for RC_USER_ERROR
#include "defs.h"
#include "gnu_config.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/c-common.h"
#include "gnu/errors.h"
}
#include "wn.h"
#include "symtab.h"
#include "tree_symtab.h"
#include "tie.h"
#include "libti.h"

extern "C" tree lookup_name(tree name, int prefer_type);

/* return a type tree node for a given type_name, assuming "const" and "*"
   qualifiers have been removed */

static tree
get_type_node(const char *input_type_name) {

  char type_name[256];
  tree type_node = NULL_TREE;

  if (strlen(input_type_name)>255) {
    error("type name %s too long", input_type_name);
    exit(RC_INTERNAL_ERROR);
  }

  if (!strncmp(input_type_name,"xtbool",6))
    sprintf(type_name,"_TIE_%s", input_type_name);
  else
    strcpy(type_name, input_type_name);

  if (!strcmp(type_name,"_TIE_int64"))

    type_node = long_long_integer_type_node;

  else if (!strcmp(type_name,"_TIE_uint64"))

    type_node = long_long_unsigned_type_node;

  else if (!strcmp(type_name,"_TIE_int") ||
	   !strcmp(type_name,"_TIE_int32") ||
	   !strcmp(type_name,"immediate"))

    type_node = integer_type_node;

  else if (!strcmp(type_name,"_TIE_unsigned") ||
	   !strcmp(type_name,"_TIE_uint32"))

    type_node = unsigned_type_node;

  else if (!strcmp(type_name,"_TIE_short") ||
	   !strcmp(type_name,"_TIE_int16"))

    type_node = short_integer_type_node;

  else if (!strcmp(type_name,"_TIE_uint16"))

    type_node = short_unsigned_type_node;

  else if (!strcmp(type_name,"_TIE_int8"))

    type_node = signed_char_type_node;

  else if (!strcmp(type_name,"_TIE_char") ||
	   !strcmp(type_name,"_TIE_uint8"))

    type_node = unsigned_char_type_node;

  else if (!strcmp(type_name,"_TIE_void"))

    type_node = void_type_node;

  else if (tie_info->xtfloat_ctype_id() != XTENSA_UNDEFINED &&
           !strcmp(type_name, tie_info->ctype_name(tie_info->xtfloat_ctype_id())))

    type_node = float_type_node;

  else {

    tree id  = get_identifier(type_name);
    if (id != NULL_TREE) {
      tree name = lookup_name(id, 1);
      if (name != NULL_TREE)
	type_node = TREE_TYPE(lookup_name(get_identifier(type_name), 1));
    }

  }

  return type_node;
}

/* return a type tree node for a given qualified type_name
   currently handle CONST only
   a pointer type can be indicated with either
   1. a '*' at the end of the 'input_qual_type_name', or with
   2. 'is_pointer' == true
*/
static tree
get_qualified_type_node(const char *input_qual_type_name,
		bool is_const, bool is_pointer) {

  const char* non_qual_type_name;
  char type_name[256];
  tree type_node = NULL_TREE;

  strcpy(type_name, input_qual_type_name);
  int i = strlen(type_name)-1;
  if (type_name[i]=='*' || is_pointer) {

    tree base_type = NULL_TREE;

    if (type_name[i]=='*') {
      i -= 1;
      while (type_name[i]==' ')
        i--;

      type_name[i+1]='\0';
    }

    base_type = get_qualified_type_node(type_name,is_const,false);
    if (base_type == NULL_TREE)
      type_node = NULL_TREE;
    else {
      if (is_const) {
        base_type = build_qualified_type (base_type, TYPE_QUAL_CONST);
        is_const = FALSE;	/* so that upper level does not see the
				   const qualifier */
      }
      type_node = build_pointer_type(base_type);      /* a pointer type */
    }
    return type_node;
  } else if (!strncmp(input_qual_type_name, "const ",6)) {
    /* we should not see a const on a non-pointer type from libcc for now */
    error("Unexpected type %s", input_qual_type_name);
    exit(RC_INTERNAL_ERROR);
    non_qual_type_name = input_qual_type_name+6;
    tree type_tree = get_type_node(non_qual_type_name);
    //type_tree = build_qualified_type (type_tree, TYPE_QUAL_CONST);
    return type_tree;
  } else {
    non_qual_type_name = input_qual_type_name;
    return get_type_node(non_qual_type_name);
  }
}

/* additional initialization for Xcalibur */
extern "C" void WFE_XC_Decl_Init(void) {

  int tie_type_index=0;
  TYPE_ID mtype_index;

  /* the following is necessary because gnu use line number to determine if
     this declaration is for standard built-in types or not.
     see pushdecl() in gnu/c-decl.c
  */
  int save_lineno = lineno;
  lineno = 0;

  if (xt_booleans) {
    for (mtype_index=MTYPE_XTBOOL; mtype_index<=MTYPE_XTBOOL16; mtype_index++) {
      tie_type_node(tie_type_index) = make_tie_type (
		      MTYPE_bit_size(mtype_index),
		      MTYPE_alignment(mtype_index));
      pushdecl (build_decl (TYPE_DECL, get_identifier (MTYPE_name(mtype_index)),
		      tie_type_node(tie_type_index)));

      tie_type_index++;
    }
  }
  mtype_index=MTYPE_CORE_LAST+1;

  while (mtype_index<=Mtype_Last) {
    if (tie_info->xtfloat_mtype_id() != mtype_index &&
        strncmp(MTYPE_name(mtype_index), "_TIE_xtbool", 11)) {
      tie_type_node(tie_type_index) = make_tie_type (
			MTYPE_bit_size(mtype_index),
			MTYPE_alignment(mtype_index));
      pushdecl (build_decl (TYPE_DECL, get_identifier (MTYPE_name(mtype_index)),
			tie_type_node(tie_type_index)));

      tie_type_index++;
    }
    mtype_index++;
  }

  /* declare 'immediate' type */
  pushdecl(build_decl(TYPE_DECL, get_identifier ("immediate"),
		    integer_type_node));

  TIE_MACRO_ID mid=0;
  tree endlink = void_list_node;

  for (mid=0; mid < tie_info->num_macros(); mid++) {
    TIE_MACRO* tie_macro = tie_info->tie_macro(mid);
    const char* macro_name = tie_macro->name();
    tree return_type = void_type_node;
    tree param_type = void_type_node;
    tree params = endlink;
    BOOL is_branch = tie_macro->is_conditional_branch();
    BOOL is_c_function = tie_macro->is_c_function();

    if (is_c_function) {
      bool is_const = tie_macro->proto_has_const_prefix(
			tie_macro->unique_out_or_inout());
      bool is_pointer = tie_macro->proto_is_pointer(
			tie_macro->unique_out_or_inout());
      const char* return_type_name = tie_macro->return_type_name();
      return_type = get_qualified_type_node(return_type_name,is_const,is_pointer);
    } else if (is_branch) {
      return_type = integer_type_node;
    }
    int p = tie_macro->num_protos();
    while (p) {
      p--;
      if (is_c_function && tie_macro->proto_is_out(p))
	continue;
      if (is_branch && tie_macro->proto_is_label(p))
        continue;
      bool is_const = tie_macro->proto_has_const_prefix(p);
      bool is_pointer = tie_macro->proto_is_pointer(p);
      const char* param_type_name = tie_macro->proto_type_mangled_name(p);
      param_type = get_qualified_type_node(param_type_name,is_const,is_pointer);
      if (param_type!=NULL_TREE) {
        params = tree_cons(NULL_TREE, param_type, params);
      }

    }

    if (return_type != NULL_TREE) {
      builtin_function (macro_name,
		      build_function_type (return_type, params),
		      BUILT_IN_TIE_MACRO, BUILT_IN_NORMAL, NULL);
    }

  }

  asm_neg_preg = (PREG_NUM)(-(tie_info->max_num_output()+1));

  lineno = save_lineno;
}

extern "C" int is_tie_intrinsic(tree fundecl) {

  if (fundecl==NULL || TREE_CODE(fundecl)!=FUNCTION_DECL ||
      DECL_FUNCTION_CODE(fundecl)!=BUILT_IN_TIE_MACRO)
    return 0;

  return 1;

}

extern "C" int is_out_or_inout_arg(tree fundecl, int parm_idx) {

  if (is_tie_intrinsic(fundecl)==0 || parm_idx<0) return 0;

  TIE_MACRO_p tie_macro =
		  tie_info->tie_macro(IDENTIFIER_POINTER(DECL_NAME(fundecl)));

  if (tie_macro==NULL) return 0;
  if (tie_macro->is_c_function() &&
      parm_idx>=tie_macro->unique_out_or_inout())
    parm_idx++;

  if (parm_idx>=tie_macro->num_protos())
    return 0;

  return !tie_macro->proto_is_in(parm_idx);

}

extern "C" int is_xtbool1_type (tree type)
{
  TYPE_ID mtype = TY_mtype(Get_TY(TYPE_MAIN_VARIANT(type)));
  return (mtype==MTYPE_XTBOOL);
}

extern "C" int is_xtbool_type (tree type)
{
  TYPE_ID mtype = TY_mtype(Get_TY(TYPE_MAIN_VARIANT(type)));
  return MTYPE_is_xtbool(mtype);
}

extern "C" int convert_for_tie_type_ok(tree src_type, tree dst_type)
{

  enum tree_code code_src = TREE_CODE(src_type);
  enum tree_code code_dst = TREE_CODE(dst_type);

  Is_True(code_src==TIE_TYPE || code_dst==TIE_TYPE, ("Expecting TIE types"));

  if (code_src!=TIE_TYPE && code_dst!=TIE_TYPE)
    return FALSE;

  TYPE_ID mtype_src = TY_mtype(Get_TY(TYPE_MAIN_VARIANT(src_type)));
  TYPE_ID mtype_dst = TY_mtype(Get_TY(TYPE_MAIN_VARIANT(dst_type)));

  if (mtype_src == mtype_dst)
    return TRUE;

  if (MTYPE_is_xtbool(mtype_src) || MTYPE_is_xtbool(mtype_dst)) {
    if (MTYPE_is_xtbool(mtype_src) &&
	(mtype_dst==MTYPE_U4 || mtype_dst==MTYPE_I4)) {
	return TRUE;
    } else if (MTYPE_is_xtbool(mtype_dst) &&
	(mtype_src==MTYPE_U4 || mtype_src==MTYPE_I4)) {
      return TRUE;
    } else {
      error("No type casting from \"%s\" to \"%s\"",
		IDENTIFIER_POINTER(DECL_NAME(TYPE_NAME(src_type))),
		IDENTIFIER_POINTER(DECL_NAME(TYPE_NAME(dst_type))));
      return FALSE;
    }
  }

  int has_conversion_macro = FALSE;
  if (MTYPE_is_tie(mtype_src) && MTYPE_is_tie(mtype_dst)) {

    has_conversion_macro =
	(tie_info->mtype_rtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_rtom_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_mtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	/* un-comment the next line when mtom macro is supported */
	/*(tie_info->mtype_mtom_macro(mtype_src,mtype_dst)!=NULL)?TRUE:*/
	FALSE;
  } else if (MTYPE_is_tie(mtype_src)==false) {
    has_conversion_macro =
	(tie_info->mtype_rtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_rtom_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_mtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	FALSE;
  } else if (MTYPE_is_tie(mtype_dst)==false) {
    has_conversion_macro =
	(tie_info->mtype_rtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_rtom_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	(tie_info->mtype_mtor_macro(mtype_src,mtype_dst)!=NULL)?TRUE:
	FALSE;
  }

  return has_conversion_macro;
}


