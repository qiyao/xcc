
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
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


// translate gnu decl trees to whirl

#ifndef _WIN32
#include <values.h>
#else
#define BITSPERBYTE 8
#endif
#include <sys/types.h>
#include <elf.h>
#include <cmplrs/rcodes.h>              // RC_USER_ERROR
#include "defs.h"
#include "errors.h"
#include "gnu_config.h"
#include "gnu/flags.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/toplev.h"
#include "gnu/tree.h"
#include "cp-tree.h"
#include "c-pragma.h"
}
#undef TARGET_PENTIUM // hack around macro definition in gnu
#include "glob.h"
#include "wn.h"
#include "wn_util.h"
#include "symtab.h"
#include "const.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_reader.h"
#include "wfe_decl.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "tree_symtab.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "symtab.h"
#include "libti.h"

extern "C" void check_gnu_errors (int *, int *);
extern const char *reg_names[];

static tree *deferred_function_stack;
static INT32 deferred_function_i;
static INT32 deferred_function_max;

static void 
Init_Deferred_Function_Stack()
{
  deferred_function_max   = 32;
  deferred_function_i     = -1;
  deferred_function_stack =
    (tree *) malloc (sizeof (tree) * deferred_function_max);
}

static void
Push_Deferred_Function (tree decl)
{
  for (INT32 i = deferred_function_i; i != -1; --i)
    if (deferred_function_stack [i] == decl)
      return;
  if (++deferred_function_i == deferred_function_max) {
    deferred_function_max = 2 * deferred_function_max;
    deferred_function_stack =
      (tree *) realloc (deferred_function_stack,
			deferred_function_max * sizeof(tree));
  }

  deferred_function_stack[deferred_function_i] = decl;
}

static tree 
Pop_Deferred_Function (void)
{
  return deferred_function_stack[deferred_function_i--];
}

static BOOL thunk_stack_inited=false;
static tree *deferred_thunk_stack;
static INT32 deferred_thunk_i;
static INT32 deferred_thunk_max;

static void 
Init_Deferred_Thunk_Stack()
{
  deferred_thunk_max   = 32;
  deferred_thunk_i     = -1;
  deferred_thunk_stack =
    (tree *) malloc (sizeof (tree) * deferred_thunk_max);
  thunk_stack_inited=true;
}

static void
Push_Deferred_Thunk (tree decl)
{
  for (INT32 i = deferred_thunk_i; i != -1; --i)
    if (deferred_thunk_stack [i] == decl)
      return;
  if (++deferred_thunk_i == deferred_thunk_max) {
    deferred_thunk_max = 2 * deferred_thunk_max;
    deferred_thunk_stack =
      (tree *) realloc (deferred_thunk_stack,
			deferred_thunk_max * sizeof(tree));
  }

  deferred_thunk_stack[deferred_thunk_i] = decl;
}

static tree 
Pop_Deferred_Thunk (void)
{
  return deferred_thunk_stack[deferred_thunk_i--];
}


static tree *deferred_decl_init_stack;
static INT32 deferred_decl_init_i;
static INT32 deferred_decl_init_max;

static void 
Init_Deferred_Decl_Init_Stack ()
{
  deferred_decl_init_max   = 32;
  deferred_decl_init_i     = -1;
  deferred_decl_init_stack =
    (tree *) malloc (sizeof (tree) * deferred_decl_init_max);
} /* Init_Deferred_Decl_Init_Stack */

void
Push_Deferred_Decl_Init (tree decl)
{
  if (++deferred_decl_init_i == deferred_decl_init_max) {
    deferred_decl_init_max = 2 * deferred_decl_init_max;
    deferred_decl_init_stack =
      (tree *) realloc (deferred_decl_init_stack,
			deferred_decl_init_max * sizeof(tree));
  }

  deferred_decl_init_stack[deferred_decl_init_i] = decl;
} /* Push_Deferred_Decl_Init */

static tree
Pop_Deferred_Decl_Init (void)
{
  tree decl;
  decl = deferred_decl_init_stack[deferred_decl_init_i--];
  return decl;
} /* Pop_Deferred_Decl_Init */

extern PU_Info *PU_Tree_Root;
static PU_Info *PU_Info_Table     [258] = {0};
static ST      *Return_Address_ST [258] = {0};
static BOOL map_mempool_initialized = FALSE;
static MEM_POOL Map_Mem_Pool;

static tree curr_func_decl = NULL_TREE;
static tree curr_func_result_decl = NULL_TREE;


static int __ctors = 0;
static int __dtors = 0;

static void Set_Current_Function_Decl (tree decl)
{
  curr_func_decl = decl;
}

tree
Current_Function_Decl (void)
{
  return curr_func_decl;
}

tree
Current_Function_Result_Decl (void)
{
  return curr_func_result_decl;
}

ST *
Current_Function_Result_ST (void)
{
  return (curr_func_result_decl) ? Get_ST(curr_func_result_decl) : NULL;
}

bool
WFE_Addressable_Type (tree exp)
{
  tree type = (TYPE_P (exp)) ? exp : TREE_TYPE (exp);

  if (TREE_CODE (type) == VOID_TYPE)
    return false;

  /* Types that are TREE_ADDRESSABLE must be constructed in memory,
     and thus can't be returned in registers.  */
  if (TREE_ADDRESSABLE (type))
    return true;

  return false;
}


void (*back_end_hook) (tree) = &WFE_Expand_Decl;

static
void WFE_Expand_Function_Body (tree decl)
{
  tree body;
  (void) WFE_Start_Function(decl);
  Set_Current_Function_Decl(decl);

  for (body = DECL_SAVED_TREE(decl); body; body = TREE_CHAIN(body))
    Mark_Scopes_And_Labels (body);

  for (body = DECL_SAVED_TREE(decl); body; body = TREE_CHAIN(body))
    WFE_Expand_Stmt(body);
  WFE_Finish_Function();
}

/*
 * WFE_Expand_Decl is called with the root of the g++ tree (always a
 * NAMESPACE_DECL) as argument and is thus the top-level routine in
 * tree-to-whirl tranlsation.  WFE_Expand_Decl is not called for
 * every declaration, but only for FUNCTION_DECL, NAMESPACE_DECL, 
 * TYPE_DECL, and VAR_DECL:  that is, for those declarations that
 * can appear in namespace scope.
 */

void WFE_Finish_Function(void);

static void
WFE_Generate_Thunk (tree decl)
{
  Is_True(decl != NULL &&
          TREE_CODE(decl) == FUNCTION_DECL &&
          DECL_THUNK_P(decl) &&
          TREE_CODE(CP_DECL_CONTEXT(decl)) != NAMESPACE_DECL,
          ("Argument to WFE_Generate_Thunk isn't a thunk"));

  Is_True(DECL_INITIAL(decl) != NULL,
          ("Argument to WFE_Generate_Thunk has null DECL_INITIAL"));

  ST      *thunk_st  = Get_ST (decl);
  ST      *func_st   = Get_ST (TREE_OPERAND (DECL_INITIAL(decl), 0));
  TYPE_ID  ret_mtype = TY_mtype (TY_ret_type (ST_pu_type (func_st)));
  WN      *entry_wn  = WFE_Start_Function (decl);
  INT32    nargs     = WN_kid_count (entry_wn) - 3;
  INT32    i;
  ST      *arg_st;
  TY_IDX   arg_ty;
  TYPE_ID  arg_mtype;
  WN      *arg_wn;
  WN      *wn;
  WN      *call_wn;

  // modify this parameter by the delta
  tree fndecl = TREE_OPERAND(DECL_INITIAL(decl),0);
  if (DECL_RESULT(fndecl) && WFE_Addressable_Type(DECL_RESULT (fndecl))) { 
    // this is second parameter since return value has been made first
    arg_st = WN_st (WN_kid1 (entry_wn));
  } else {
    arg_st = WN_st (WN_kid0 (entry_wn));
  }
  arg_ty = ST_type (arg_st);
  arg_mtype = TY_mtype (arg_ty);

  // Pseudocode:
  //     this += delta;
  //     if (vcall_offset != 0)
  //       this += (*((ptrdiff_t **) this))[vcall_offset];
  wn = WN_Binary (OPR_ADD, arg_mtype,
                  WN_Ldid (arg_mtype, 0, arg_st, arg_ty),
                  WN_Intconst (arg_mtype, THUNK_DELTA(decl)));
  if (THUNK_VCALL_OFFSET(decl) != 0) {
    DevWarn ("Generating thunk with vcall adjustment at line %d\n", lineno);

    TY_IDX pdiff    = Get_TY(ptrdiff_type_node);
    TY_IDX p_pdiff  = Make_Pointer_Type(pdiff, FALSE);
    TY_IDX pp_pdiff = Make_Pointer_Type(p_pdiff, FALSE);

    WN* deref = WN_CreateIload(OPR_ILOAD,
                               TY_mtype(p_pdiff), TY_mtype(pp_pdiff),
                               0,
                               p_pdiff, pp_pdiff,
                               WN_Tas(pp_pdiff, TY_mtype(pp_pdiff),
                                      WN_COPY_Tree(wn)));

    wn = WN_Binary (OPR_ADD, arg_mtype,
                    wn,
                    WN_CreateIload(OPR_ILOAD,
                                   TY_mtype(pdiff), TY_mtype(p_pdiff),
                                   THUNK_VCALL_OFFSET(decl),
                                   pdiff, p_pdiff,
                                   deref));
  }
  wn = WN_Stid (arg_mtype, 0, arg_st, arg_ty, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());

  // generate call to base function
  call_wn = WN_Create (OPR_CALL, ret_mtype, MTYPE_V, nargs);
  WN_st_idx (call_wn) = ST_st_idx (func_st);
  WN_Set_Call_Default_Flags (call_wn);
  WN_Set_Call_Replace_By_Jump (call_wn);
  for (i = 0; i < nargs; i++) {
    arg_st = WN_st (WN_kid (entry_wn, i));
    arg_ty = ST_type (arg_st);
    arg_mtype = TY_mtype (arg_ty);
    arg_wn = WN_Ldid (arg_mtype, 0, arg_st, arg_ty);
    arg_wn = WN_CreateParm (Mtype_comparison (arg_mtype), arg_wn,
                            arg_ty, WN_PARM_BY_VALUE);
    WN_kid (call_wn, i) = arg_wn;
  }

  if (ret_mtype == MTYPE_V) {

    WFE_Stmt_Append (call_wn, Get_Srcpos());
    wn = WN_CreateReturn ();
    WFE_Stmt_Append (wn, Get_Srcpos());
  }

  else {

    WN *block_wn = WN_CreateBlock ();
    WN_INSERT_BlockLast (block_wn, call_wn);
    wn = WN_Ldid (ret_mtype, -1, Return_Val_Preg, Be_Type_Tbl (ret_mtype));
    wn = WN_CreateComma (OPR_COMMA, Mtype_comparison (ret_mtype), MTYPE_V,
			 block_wn, wn);
    wn = WN_CreateReturn_Val (OPR_RETURN_VAL, ret_mtype, MTYPE_V, wn);
    WFE_Stmt_Append (wn, Get_Srcpos());
  }

  Set_PU_is_thunk (Get_Current_PU ());
  WFE_Finish_Function ();
}

static void process_local_classes()
{
  for (int i = 0; i < local_classes->elements_used; ++i) {
    tree t = VARRAY_TREE(local_classes, i);
      if ((t->common.code == RECORD_TYPE ||
           t->common.code == UNION_TYPE) &&
          !uses_template_parms(t))
	Get_TY(t);
  }
}

void WFE_Expand_Decl(tree decl)
{
  static bool stack_initialized = false;

  Is_True(decl != NULL && TREE_CODE_CLASS(TREE_CODE(decl)) == 'd',
          ("Argument to WFE_Expand_Decl isn't a decl node"));

  int error_count, sorry_count;
  if (stack_initialized==false) {
    Init_Deferred_Function_Stack();
    Init_Deferred_Decl_Init_Stack();
    stack_initialized=true;
  }
  if (decl == global_namespace) {
   check_gnu_errors (&error_count, &sorry_count);
   if (error_count || sorry_count)
     return;
  }

  switch (TREE_CODE(decl)) { 

    case CONST_DECL:
      Lmt_DevWarn(1, ("WFE_Expand_Decl:  don't know what to do with CONST_DECL"));
      break;

    case FUNCTION_DECL:
      if (DECL_THUNK_P(decl) &&
          TREE_USED(decl) && 
          TREE_CODE(CP_DECL_CONTEXT(decl)) != NAMESPACE_DECL) {
        WFE_Generate_Thunk(decl);
      }
      else {
      tree body = DECL_SAVED_TREE(decl);
      if (body != NULL_TREE &&
	  (!DECL_EXTERNAL(decl) || (DECL_INLINE(decl) && TREE_USED(decl))) &&
	  (DECL_TEMPLATE_INFO(decl) == NULL 		   ||
	   DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(decl) ||
	   DECL_TEMPLATE_INSTANTIATED(decl) 		   ||
	   DECL_TEMPLATE_SPECIALIZATION(decl))) {
        (void) Get_ST(decl);
	if (CURRENT_SYMTAB != GLOBAL_SYMTAB ||
 	    DECL_FUNCTION_MEMBER_P(decl)    ||
	  strncmp (IDENTIFIER_POINTER(DECL_NAME(decl)), "__tcf", 5) == 0)
	  Push_Deferred_Function (decl);
	else {
	  WFE_Expand_Function_Body (decl);
	  while (deferred_function_i != -1)
	    WFE_Expand_Function_Body (Pop_Deferred_Function ());
	}
      }
    }

      break;

    case NAMESPACE_DECL: {
      /* We assume for now that there are no TREE_LIST nodes among
       * the namespace declarations.  
       */

      if (decl == std_node)
        break; // ignore namespace std
      if (DECL_NAMESPACE_ALIAS(decl))
	break;
      if(!thunk_stack_inited) {
	Init_Deferred_Thunk_Stack();
      }
      tree subdecl;
      for (subdecl = cp_namespace_decls(decl);
	   subdecl != NULL_TREE;
	   subdecl = TREE_CHAIN(subdecl)) {
         if (TREE_CODE(subdecl) == FUNCTION_DECL &&   // do thunks last
             DECL_THUNK_P(subdecl) && TREE_USED(subdecl) && 
             TREE_CODE(CP_DECL_CONTEXT(subdecl)) != NAMESPACE_DECL) { 
            Push_Deferred_Thunk (subdecl);
	 } else {
	    WFE_Expand_Decl(subdecl);
	 }
      }
      while (deferred_thunk_i != -1)
	WFE_Expand_Decl (Pop_Deferred_Thunk ());
      if (decl == global_namespace)
	process_local_classes(); 
      while (deferred_function_i != -1)
	WFE_Expand_Function_Body (Pop_Deferred_Function ());
      break;
    }

    case TEMPLATE_DECL: {
      if (DECL_CONTEXT(decl) && TREE_CODE(DECL_CONTEXT(decl)) == RECORD_TYPE)
	Get_TY(DECL_CONTEXT(decl));
      tree gentemp = most_general_template(decl);
      for (tree t = DECL_TEMPLATE_SPECIALIZATIONS(gentemp);
           t; t = TREE_CHAIN(t))
	if (TREE_CODE(TREE_VALUE(t)) == FUNCTION_DECL &&
	    !DECL_EXTERNAL(TREE_VALUE(t))	      &&
	    !uses_template_parms (TREE_VALUE(t)))
          WFE_Expand_Decl (TREE_VALUE(t));
	DECL_TEMPLATE_SPECIALIZATIONS(gentemp) = 0; // don't do these twice

      for (tree t = DECL_TEMPLATE_INSTANTIATIONS(gentemp);
	   t; t = TREE_CHAIN(t)) {
	  tree val = TREE_VALUE(t);
	  if (TREE_CODE(val) == RECORD_TYPE &&
	      !uses_template_parms(val))
	    Get_TY(val);
      }

      break;
    }

    case TREE_VEC:
      break;
   
    case TYPE_DECL: {
      tree type = TREE_TYPE(decl);
      (void) Get_TY(type);
      break;
    }

    case VAR_DECL: {
      if (DECL_VIRTUAL_P (decl) && TREE_ASM_WRITTEN (decl)) {
      } else if (DECL_ARTIFICIAL (decl) &&
	  TREE_STATIC (decl) && 
          !TREE_USED (decl) &&
          (DECL_COMDAT (decl) || !TREE_PUBLIC (decl))) {
	// ignore initialization unless really used
	// e.g. FUNCTION and PRETTY_FUNCTION
#ifdef WFE_DEBUG
	fprintf(stderr, "VAR_DECL: ignoring initialization of %s\n",
		IDENTIFIER_POINTER(DECL_NAME(decl)));
#endif
	return;
      }

      ST *st = Get_ST(decl);
#ifdef TARG_XTENSA
      /* If 'decl' is a variable length array, then we need an
         alloca. */
      {
	tree type_tree = TREE_TYPE(decl);
	tree type_size_unit = TYPE_SIZE_UNIT(type_tree);
	if ((type_size_unit != NULL) &&
	    (TREE_CODE(type_size_unit) != INTEGER_CST) &&
	    (TREE_CODE(type_tree) == ARRAY_TYPE))
	  WFE_Alloca_ST(decl);
      }      
#endif
      
      // For a local anonymous union use the ST of the first member
      // as the ST_base for all other members
      if (!TREE_STATIC(decl) &&
          !DECL_EXTERNAL(decl) &&
          ANON_AGGR_TYPE_P(TREE_TYPE(decl))) {
        ST *base = NULL;
        for (tree t = DECL_ANON_UNION_ELEMS(decl); t; t = TREE_CHAIN(t)) {
          ST *st = Get_ST(TREE_VALUE(t));
          if (base == NULL)
            base = st;
          else
            Set_ST_base(st, base);
        }
      }

      if (DECL_INITIAL(decl) && !DECL_EXTERNAL(decl)) {
	tree init = DECL_INITIAL(decl);
      	if (TREE_CODE(init) == ERROR_MARK)
	  return;

	if (TREE_CODE(init) == PTRMEM_CST)  {
	  init = cplus_expand_constant(init);
	  DECL_INITIAL(decl) = init;
  	}

	if (TREE_CODE(init) == CONSTRUCTOR) {
  	  tree init_type = TREE_TYPE(init);
	  if (TREE_CODE(init_type) != RECORD_TYPE &&
	      TREE_CODE(init_type) != ARRAY_TYPE  &&
	      TREE_CODE(init_type) != UNION_TYPE)
	    return;
	  }

	WFE_Initialize_Decl(decl);
      }
      break;
    }

    case LABEL_DECL:
      DevWarn("Not doing anything for LABEL_DECL?");
      break;
      
    default:
      Is_True(FALSE, ("Unexpected tree code"));
      break;

  } /* switch */
} /* WFE_Expand_Decl */


// This function is called from finish_anon_union in gnu/cp/decl2.c
// to set the ST of the first member as the ST_base for all other 
// members of an anonymous union, when that union is declared globally
// (local anonymous unions are dealt with in WFE_Expand_Decl for VAR_DECL).
extern "C" void
WFE_Expand_Anon_Union_Decl(tree anon_union_decl)
{
  ST *base = NULL;
  for (tree t = DECL_ANON_UNION_ELEMS(anon_union_decl); t; t = TREE_CHAIN(t)) {
    ST *st = Get_ST(TREE_VALUE(t));
    if (base == NULL)
      base = st;
    else
      Set_ST_base(st, base);
  }
}


static BOOL
function_has_varargs(tree fndecl)
{
  tree fntype  = TREE_TYPE(fndecl);
  tree arglist = TYPE_ARG_TYPES(fntype);

  while (arglist != NULL_TREE) {
    if (TREE_VALUE(arglist) == void_type_node)
      return FALSE;
    arglist = TREE_CHAIN(arglist);
  }

  return TRUE;
}


/* Generate WHIRL representing an asm at file scope (between
  functions). This is an awful hack. Copied from gccfe. */
void
WFE_Assemble_Asm(char *asm_string)
{
  ST *asm_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(asm_st,
	  Str_To_Index (Save_Str (asm_string),
			Global_Strtab),
	  CLASS_NAME,
	  SCLASS_UNKNOWN,
	  EXPORT_LOCAL,
	  (TY_IDX) 0);

  Set_ST_asm_function_st(*asm_st);

  WN *func_wn = WN_CreateEntry(0,
			       asm_st,
			       WN_CreateBlock(),
			       NULL,
			       NULL);

  /* Not sure how much setup of WN_MAP mechanism, etc. we need to do.
   * Pretty certainly we need to set up some PU_INFO stuff just to get
   * this crazy hack of a FUNC_ENTRY node written out to the .B file.
   */

  /* This code patterned after "wfe_decl.cxx":WFE_Start_Function, and
     specialized for the application at hand. */

#ifdef ASM_NEEDS_WN_MAP
    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);
#endif

    // This non-PU really doesn't need a symbol table and the other
    // trappings of a local scope, but if we create one, we can keep
    // all the ir_bread/ir_bwrite routines much more blissfully
    // ignorant of the supreme evil that's afoot here.

    if (CURRENT_SYMTAB != GLOBAL_SYMTAB) {
      error("misplaced asm ");
    }

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    if (Show_Progress) {
      fprintf (stderr, "Asm(%s)\n", ST_name (asm_st));
      fflush (stderr);
    }
    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, func_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(asm_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (asm_st,/*tree=*/0);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;
    else
      PU_Tree_Root = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;

  /* This code patterned after "wfe_decl.cxx":WFE_Finish_Function, and
     specialized for the application at hand. */

    // write out all the PU information
    pu_info = PU_Info_Table [CURRENT_SYMTAB];

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    PU_IDX pu_idx;
    PU &pu = New_PU(pu_idx);
    PU_Init(pu, (TY_IDX) 0, CURRENT_SYMTAB);
    Set_PU_no_inline(pu);
    Set_PU_no_delete(pu);
    Set_ST_pu(*asm_st, pu_idx);

    Write_PU_Info (pu_info);

    // What does the following line do?
    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    Delete_Scope(CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
}

extern WN *
WFE_Start_Function (tree fndecl)
{
    Is_True(fndecl != NULL && TREE_CODE(fndecl) == FUNCTION_DECL,
            ("Bad argument to WFE_Start_Function"));

    WN   *entry_wn;
    BOOL  thunk = DECL_THUNK_P(fndecl) &&
                  TREE_CODE(CP_DECL_CONTEXT(fndecl)) != NAMESPACE_DECL;

    /* If the returned object must be returned by an invisible first
       argument, then create an ST to represent that argument and
       prepend it to the other args. */
    tree fntype = TREE_TYPE (fndecl);
    tree fnargs = DECL_ARGUMENTS (fndecl);
    curr_func_result_decl = NULL_TREE;

    if (DECL_RESULT(fndecl) && WFE_Addressable_Type(DECL_RESULT (fndecl)))
    {
      FmtAssert(!thunk, ("not expecting thunk for invisible-first-arg return"));
      tree type = build_pointer_type (TREE_TYPE (fntype));
      tree function_result_decl = build_decl (PARM_DECL, NULL_TREE, type);

      curr_func_result_decl = function_result_decl;
      
      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
      DECL_ARGUMENTS(fndecl) = fnargs;
    }
    
    if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
      Set_PU_uplevel (Get_Current_PU ());

    /* deallocate the old map table */
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    /* set up the mem pool for the map table and predefined mappings */
    if (!map_mempool_initialized) {
        MEM_POOL_Initialize(&Map_Mem_Pool,"Map_Mem_Pool",FALSE);
        map_mempool_initialized = TRUE;
    } else {
        MEM_POOL_Pop(&Map_Mem_Pool);
    }

    MEM_POOL_Push(&Map_Mem_Pool);

    /* create the map table for the next PU */
    (void)WN_MAP_TAB_Create(&Map_Mem_Pool);

    New_Scope (CURRENT_SYMTAB + 1, Malloc_Mem_Pool, TRUE);

    if (DECL_SOURCE_FILE (fndecl)) {
      lineno = DECL_SOURCE_LINE (fndecl);
      WFE_Set_Line_And_File (lineno, DECL_SOURCE_FILE (fndecl));
    }

    // handle VLAs in the declaration
    WN *vla_block = WN_CreateBlock ();
    WFE_Stmt_Push (vla_block, wfe_stmk_func_body, Get_Srcpos());

    ST        *func_st;
    ST_EXPORT  eclass = ((TREE_PUBLIC(fndecl) && !DECL_INLINE(fndecl)
			 || DECL_WEAK(fndecl)
			  /* vxworks requires thunks that invoke
                             global constructors and destructors to be
                             global. If this causes trouble for other
                             platforms (due to name collisions or
                             something else), we can add a flag to
                             conditionalize it. */
			 || DECL_GLOBAL_DTOR_P(fndecl)
			 || DECL_GLOBAL_CTOR_P(fndecl)) ?
			 EXPORT_PREEMPTIBLE :
			 EXPORT_LOCAL);

#ifndef GPLUSPLUS_FE
    if (DECL_INLINE (fndecl) && TREE_PUBLIC (fndecl)) {
      if (DECL_EXTERNAL (fndecl)) {
        // encountered first extern inline definition
        ST *oldst = DECL_ST (fndecl);
        DECL_ST (fndecl) = 0;
        func_st =  Get_ST (fndecl);
        DECL_ST (fndecl) = oldst;
      }
      else {
        // encountered second definition, the earlier one was extern inline
        func_st = Get_ST (fndecl);
      }
    }
    else
#else
      func_st = Get_ST (fndecl);
#endif /* GPLUSPLUS_FE */

    if (ST_export(func_st)==EXPORT_INTERNAL) {
      /* for inline function used, put it in linkonce section
         and allow inliner to delete it */
      eclass = EXPORT_INTERNAL;
    }

    Set_ST_sclass (func_st, SCLASS_TEXT);
    Set_PU_lexical_level (Pu_Table [ST_pu (func_st)], CURRENT_SYMTAB);
    Set_PU_cxx_lang (Pu_Table [ST_pu (func_st)]);
    if (fndecl->decl.used_attribute) Set_PU_no_delete(Pu_Table [ST_pu (func_st)]);

    if (DECL_UNINLINABLE_ATTRIBUTE(fndecl)) {
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
    }

    if (DECL_ALWAYS_INLINE(fndecl)) {
      Set_PU_must_inline (Pu_Table [ST_pu (func_st)]);
    }


    if (DECL_INLINE(fndecl)) {
      Set_PU_is_inline_function (Pu_Table [ST_pu (func_st)]);
      wfe_invoke_inliner = TRUE;
      if (eclass==EXPORT_INTERNAL) {
        Set_ST_is_weak_symbol (func_st);
      }
    }

    if (TREE_READONLY(fndecl)) {
        Set_PU_is_pure(Pu_Table[ST_pu(func_st)]);
    } else if (DECL_IS_PURE(fndecl)) {
        Set_PU_no_side_effects(Pu_Table[ST_pu(func_st)]);
    }

    Set_ST_export (func_st, eclass);

    if (Show_Progress) {
      fprintf (stderr, "Compiling %s \n", ST_name (func_st));
      fflush (stderr);
    }

    Scope_tab [Current_scope].st = func_st;

    INT num_args = 0;
    tree pdecl;
    for (pdecl = thunk ? DECL_ARGUMENTS (TREE_OPERAND (DECL_INITIAL (fndecl), 0))
                       : fnargs;
         pdecl;
         pdecl = TREE_CHAIN (pdecl)) {

      /* See if this arg was passed by invisible reference. If it is,
         we need to change it's type to be a pointer. */
      if (pdecl == curr_func_result_decl)
	{
	  Set_ST_implicit_indirect(Get_ST(pdecl));
	}
      else if (WFE_Addressable_Type(TREE_TYPE(pdecl)))
	{
	  FmtAssert(!thunk, ("not expecting thunk to have invisible reference args"));
	  TREE_TYPE(pdecl) = build_pointer_type(TREE_TYPE(pdecl));
	  Set_ST_implicit_indirect(Get_ST(pdecl));
	}

      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else {
	++num_args;
      }
    }

    WN *body, *wn;
    body = WN_CreateBlock ( );
    entry_wn = WN_CreateEntry ( num_args, func_st, body, NULL, NULL );
    /* from 1..nkids=num_args, create IDNAME args for OPR_FUNC_ENTRY */
    INT i = 0;
    for (pdecl = thunk ? DECL_ARGUMENTS (TREE_OPERAND (DECL_INITIAL (fndecl), 0))
                       : fnargs;
         pdecl;
         pdecl = TREE_CHAIN (pdecl) )
    {
      TY_IDX arg_ty_idx = Get_TY(TREE_TYPE(pdecl));
      ST *st;
      if (thunk) {
        st = New_ST ();
        ST_Init (st, Save_Str2i(".arg", "", i), CLASS_VAR,
		 SCLASS_FORMAL, EXPORT_LOCAL, arg_ty_idx);
      }
      else
	st = Get_ST(pdecl);

      if (!WFE_Keep_Zero_Length_Structs   &&
          TY_mtype (arg_ty_idx) == MTYPE_M &&
          TY_size (arg_ty_idx) == 0) {
        // zero length struct parameter
      }
      else {
        if (TY_mtype (arg_ty_idx) == MTYPE_F4 &&
            !TY_has_prototype (ST_pu_type (func_st)))
          Set_ST_promote_parm (st);
          WN_kid(entry_wn,i) = WN_CreateIdname ( 0, ST_st_idx(st) );
          ++i;
      }
    }

    PU_Info *pu_info;
    /* allocate a new PU_Info and add it to the list */
    pu_info = TYPE_MEM_POOL_ALLOC(PU_Info, Malloc_Mem_Pool);
    PU_Info_init(pu_info);

    Set_PU_Info_tree_ptr (pu_info, entry_wn);
    PU_Info_maptab (pu_info) = Current_Map_Tab;
    PU_Info_proc_sym (pu_info) = ST_st_idx(func_st);
    PU_Info_pu_dst (pu_info) = DST_Create_Subprogram (func_st,fndecl);
    PU_Info_cu_dst (pu_info) = DST_Get_Comp_Unit ();

    Set_PU_Info_state(pu_info, WT_SYMTAB, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_TREE, Subsect_InMem);
    Set_PU_Info_state(pu_info, WT_PROC_SYM, Subsect_InMem);

    Set_PU_Info_flags(pu_info, PU_IS_COMPILER_GENERATED);

    if (PU_Info_Table [CURRENT_SYMTAB])
      PU_Info_next (PU_Info_Table [CURRENT_SYMTAB]) = pu_info;

    else
    if (CURRENT_SYMTAB == GLOBAL_SYMTAB + 1)
      PU_Tree_Root = pu_info;

    else
      PU_Info_child (PU_Info_Table [CURRENT_SYMTAB -1]) = pu_info;

    PU_Info_Table [CURRENT_SYMTAB] = pu_info;

    WFE_Stmt_Pop (wfe_stmk_func_body);

    WFE_Stmt_Push (entry_wn, wfe_stmk_func_entry, Get_Srcpos());
    WFE_Stmt_Push (body, wfe_stmk_func_body, Get_Srcpos());

    wn = WN_CreatePragma (WN_PRAGMA_PREAMBLE_END, (ST_IDX) NULL, 0, 0);
    WFE_Stmt_Append (wn, Get_Srcpos());
    WFE_Stmt_Append (vla_block, Get_Srcpos());

    if (function_has_varargs(fndecl) && !thunk) {
      // the function uses varargs.h
      // throw off the old type declaration as it did not 
      // take into account any arguments
      PU& pu = Pu_Table[ST_pu (func_st)];
      TY_IDX ty_idx;
      TY &ty = New_TY (ty_idx);
      TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
      Set_TY_align (ty_idx, 1);
      TYLIST tylist_idx;
      Set_TYLIST_type (New_TYLIST (tylist_idx),
                       Get_TY(TREE_TYPE(TREE_TYPE(fndecl))));
      Set_TY_tylist (ty, tylist_idx);
      for (pdecl = fnargs; pdecl; pdecl = TREE_CHAIN (pdecl) ) {
	ST *arg_st = Get_ST(pdecl);
        Set_TYLIST_type (New_TYLIST (tylist_idx), ST_type(arg_st));
      }
      Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
      Set_TY_is_varargs (ty_idx);
      Set_TY_has_prototype (ty_idx);
      Set_PU_prototype (pu, ty_idx);
    }

    if (!thunk && DECL_GLOBAL_CTOR_P(fndecl)) {
      INITV_IDX initv = New_INITV ();
      INITV_Init_Symoff (initv, func_st, 0, 1);
      Set_ST_addr_saved (func_st);
      ST *init_st = New_ST (GLOBAL_SYMTAB);
      ST_Init (init_st, Save_Str2i ("__ctors", "_", ++__ctors),
               CLASS_VAR, SCLASS_FSTATIC,
               EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
      Set_ST_is_initialized (init_st);
      INITO_IDX inito = New_INITO (init_st, initv);
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
      ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                    Save_Str2i (".ctors", ".", DEFAULT_INIT_PRIORITY-GLOBAL_INIT_PRIORITY(fndecl)));
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
    }

    if (!thunk && DECL_GLOBAL_DTOR_P(fndecl)) {
      INITV_IDX initv = New_INITV ();
      INITV_Init_Symoff (initv, func_st, 0, 1);
      ST *init_st = New_ST (GLOBAL_SYMTAB);
      ST_Init (init_st, Save_Str2i ("__dtors", "_", ++__dtors),
               CLASS_VAR, SCLASS_FSTATIC,
               EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
      Set_ST_is_initialized (init_st);
      INITO_IDX inito = New_INITO (init_st, initv);
      ST_ATTR_IDX st_attr_idx;
      ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
      ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                    Save_Str2i (".dtors", ".", DEFAULT_INIT_PRIORITY-GLOBAL_INIT_PRIORITY(fndecl)));
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
      Set_ST_addr_saved (func_st);
      Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
      Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
    }

    return entry_wn;
}

extern INT  WFE_Check_Noreturn(WN *func_nd);

extern void
WFE_Finish_Function (void)
{
    WFE_Check_Undefined_Labels ();
    PU_Info *pu_info = PU_Info_Table [CURRENT_SYMTAB];

    if (CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) {
      Set_PU_is_nested_func (Get_Current_PU ());
    }

    // Insert a RETURN if it does not exist
    WN * wn = WN_last (WFE_Stmt_Top ());
    if (wn == NULL || 
        (WN_operator (wn) != OPR_RETURN &&
         WN_operator (wn) != OPR_RETURN_VAL)) {


      WFE_Stmt_Append (WN_CreateReturn (), Get_Srcpos ());
    }


    // Add any handler code
    WFE_Expand_Handlers_And_Cleanups ();

    // write out all the PU information
    WFE_Stmt_Pop (wfe_stmk_func_body);
    WN *func_nd = WFE_Stmt_Pop (wfe_stmk_func_entry);

    if (warn_return_type &&
          ! DECL_CONSTRUCTOR_P (Current_Function_Decl ()) &&
          TY_kind (TY_ret_type (ST_pu_type (PU_Info_proc_sym (pu_info)))) !=
          KIND_VOID) {
	if (WFE_Check_Noreturn(func_nd)) {
          cp_warning ("control reaches end of non-void function `%D'",
                    Current_Function_Decl ());
	}
    }

    // deallocate the old map table
    if (Current_Map_Tab) {
        WN_MAP_TAB_Delete(Current_Map_Tab);
        Current_Map_Tab = NULL;
    }

    Write_PU_Info (pu_info);

    PU_Info_Table [CURRENT_SYMTAB+1] = NULL;

    if (Return_Address_ST [CURRENT_SYMTAB]) {
      Set_PU_has_return_address (Get_Current_PU ());
      Set_PU_no_inline (Get_Current_PU ());
      Return_Address_ST [CURRENT_SYMTAB] = NULL;
    }

    Delete_Scope (CURRENT_SYMTAB);
    --CURRENT_SYMTAB;
//  if (CURRENT_SYMTAB > GLOBAL_SYMTAB)
//    Current_pu = &Pu_Table[ST_pu (Scope_tab[CURRENT_SYMTAB].st)];
}

// Because we build inito's piecemeal via calls into wfe for each piece,
// need to keep track of current inito and last initv that we append to.
static INITO_IDX aggregate_inito = 0;
static INITV_IDX last_aggregate_initv = 0;	
static BOOL not_at_root = FALSE;

void
WFE_Start_Aggregate_Init (tree decl)
{
  if (TREE_STATIC(decl)) {
	ST *st = Get_ST(decl);
	Set_ST_is_initialized(st);
	if (ST_sclass(st) == SCLASS_UGLOBAL ||
	    ST_sclass(st) == SCLASS_EXTERN  ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
  }
}

void
WFE_Add_Aggregate_Init_Padding (INT size)
{
  if (aggregate_inito == 0) return;
  if (size < 0) return;	// actually happens from assemble_zeroes
  INITV_IDX inv = New_INITV();
  INITV_Init_Pad (inv, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Integer (INT64 val, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TYPE_ID mtype;
  if (size == 1) mtype = MTYPE_I1;
  else if (size == 2) mtype = MTYPE_I2;
  else if (size == 4) mtype = MTYPE_I4;
  else if (size == 8) mtype = MTYPE_I8;
  else FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Integer unexpected size"));
  INITV_Init_Integer (inv, mtype, val);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

static void
WFE_Add_Init_Block(void)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv_blk = New_INITV();
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv_blk);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv_blk);
  last_aggregate_initv = inv_blk;
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

static void 
WFE_Add_Aggregate_Init_Real (REAL_VALUE_TYPE real, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    tc;
  long    t1;
  long    buffer[2];
  switch (size) {
    case 4:
      REAL_VALUE_TO_TARGET_SINGLE (real, t1);
      tc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      break;
    case 8:
      REAL_VALUE_TO_TARGET_DOUBLE (real, buffer);
      WFE_Convert_To_Host_Order(buffer);
      tc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
      break;
    default:
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Real unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(tc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void 
WFE_Add_Aggregate_Init_Complex (REAL_VALUE_TYPE rval, REAL_VALUE_TYPE ival, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  TCON    rtc;
  TCON    itc;
  long    t1;
  long    buffer[2];
  switch (size) {
    case 8:
      REAL_VALUE_TO_TARGET_SINGLE (rval, t1);
      rtc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      REAL_VALUE_TO_TARGET_SINGLE (ival, t1);
      itc = Host_To_Targ_Float_4 (MTYPE_F4, *(float *) &t1);
      break;
    case 16:
      REAL_VALUE_TO_TARGET_DOUBLE (rval, buffer);
      WFE_Convert_To_Host_Order(buffer);
      rtc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
      REAL_VALUE_TO_TARGET_DOUBLE (ival, buffer);
      WFE_Convert_To_Host_Order(buffer);
      itc = Host_To_Targ_Float (MTYPE_F8, *(double *) &buffer);
      break;
    default:
      FmtAssert(FALSE, ("WFE_Add_Aggregate_Init_Complex unexpected size"));
      break;
  }
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(rtc), 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  inv = New_INITV();
  INITV_Set_VAL (Initv_Table[inv], Enter_tcon(itc), 1);
  Set_INITV_next(last_aggregate_initv, inv);
  last_aggregate_initv = inv;
}

void 
WFE_Add_Aggregate_Init_String (char *s, INT size)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_String (inv, s, size);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Symbol (ST *st, WN_OFFSET offset = 0)
{
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Symoff (inv, st, offset);
  Set_ST_addr_saved (st);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
}

void
WFE_Add_Aggregate_Init_Label (LABEL_IDX lab)
{
  DevWarn ("taking address of a label at line %d", lineno);
  if (aggregate_inito == 0) return;
  INITV_IDX inv = New_INITV();
  INITV_Init_Label (inv, lab, 1);
  if (last_aggregate_initv != 0)
    Set_INITV_next(last_aggregate_initv, inv);
  else if (! not_at_root)
    Set_INITO_val(aggregate_inito, inv);
  last_aggregate_initv = inv;
  Set_LABEL_addr_saved (lab);
}

void
WFE_Add_Aggregate_Init_Address (tree init)
{
  switch (TREE_CODE (init)) {

  case VAR_DECL:
  case FUNCTION_DECL:
	WFE_Add_Aggregate_Init_Symbol (Get_ST (init));
	break;

  case STRING_CST:
	{
	TCON tcon = Host_To_Targ_String (MTYPE_STRING,
				       TREE_STRING_POINTER(init),
				       TREE_STRING_LENGTH(init));
	ST *const_st = New_Const_Sym (Enter_tcon (tcon), 
		Get_TY(TREE_TYPE(init)));
      	WFE_Add_Aggregate_Init_Symbol (const_st);
	ST *pu_st = Get_Current_PU_ST();
	bool has_rodata_section = pu_st ? ST_has_named_ro_section(pu_st) : false;
	if (flag_merge_constants && TY_align(Get_TY(TREE_TYPE(init)))==1
	    && !has_rodata_section) {
	  Set_ST_sclass(const_st,SCLASS_MERGE_STRING);
	  Set_ST_explicit_literal_ref(const_st);
	}

	}
    	break;

  case PLUS_EXPR:
	if ( TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
	  && TREE_CODE(TREE_OPERAND(init,1)) == INTEGER_CST)
	{
		tree addr_kid = TREE_OPERAND(TREE_OPERAND(init,0),0);
		FmtAssert(TREE_CODE(addr_kid) == VAR_DECL
			|| TREE_CODE(addr_kid) == FUNCTION_DECL,
			("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(init,1)) );
	}
	else
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected decl under plus_expr"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
		WN_Delete (init_wn);
	}
	break;

  case INTEGER_CST:
	WFE_Add_Aggregate_Init_Integer (Get_Integer_Value (init), Pointer_Size);
	break;

  case LABEL_DECL:
	{
	 	LABEL_IDX label_idx = WFE_Get_LABEL (init, FALSE);
		WFE_Add_Aggregate_Init_Label (label_idx);
	}
	break;

  default:
	{
		WN *init_wn = WFE_Expand_Expr (init);
		FmtAssert (WN_operator (init_wn) == OPR_LDA,
				("expected operator encountered"));
		WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
					       WN_offset (init_wn));
		WN_Delete (init_wn);
	}
      	break;
  }
} /* WFE_Add_Aggregate_Init_Address */

void
WFE_Finish_Aggregate_Init (void)
{
  if (aggregate_inito == 0) return;
  ST *st = INITO_st(aggregate_inito);
  TY_IDX ty = ST_type(st);
  UINT ty_size = TY_size(ty);
  UINT inito_size = Get_INITO_Size(aggregate_inito);
  
  // incomplete struct/union or array whose size is determined by inito
  if (ty_size == 0) {
    Set_TY_size(ty, inito_size);
    if (TY_kind(ty) == KIND_ARRAY) {
      Set_ARB_const_ubnd(TY_arb(ty));
      Set_ARB_ubnd_val(TY_arb(ty), ty_size / TY_size(TY_etype(ty)) - 1);
    }
  }
  else {
    Is_True(!(TY_kind(ty) == KIND_ARRAY 
              && !ARB_const_ubnd(TY_arb(ty)) 
              && ty_size <= inito_size),
            ("Unexpected INITO for a VLA"));
  }
  
  // struct containing zero-length array as the last field
  if (TY_kind(ty) == KIND_STRUCT && !TY_fld (ty).Is_Null()) {
    FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
    while (!FLD_last_field(fld_iter)) fld_iter++;
    TY_IDX last_fld_ty = FLD_type(fld_iter);
    if (TY_kind(last_fld_ty) == KIND_ARRAY && TY_size(last_fld_ty) == 0) {
      Set_TY_size(ty, inito_size);
      UINT new_array_size = inito_size - ty_size;
      Set_TY_size(last_fld_ty, new_array_size);
      Set_ARB_ubnd_val(TY_arb(last_fld_ty),
                       new_array_size / TY_size(TY_etype(last_fld_ty)) - 1);
    }
  }

  if (last_aggregate_initv == 0) {
    WFE_Add_Aggregate_Init_Padding (0);
  }
  aggregate_inito = 0;
  not_at_root = FALSE;
}


static BOOL
Has_Non_Constant_Init_Value (tree init)
{
  if (init == NULL) {
	return FALSE;
  }
  switch (TREE_CODE(init)) {
  case CONSTRUCTOR:
	if (!CONSTRUCTOR_ELTS(init))
	    return TRUE;
	return Has_Non_Constant_Init_Value (CONSTRUCTOR_ELTS(init));
  case TREE_LIST:
	{
	tree p;
	for (p = init; p != NULL; p = TREE_CHAIN(p)) {
		if (Has_Non_Constant_Init_Value (TREE_VALUE(p))) {
			return TRUE;
		}
/*
		if (TREE_PURPOSE(p) != NULL_TREE 	     &&
		    TREE_CODE(TREE_PURPOSE(p)) == FIELD_DECL &&
		    DECL_BIT_FIELD(TREE_PURPOSE(p)))
		{
			// if bitfield, then do each element separately
			// rather than combine into initv field.
			return TRUE;
		}
*/
	}
	return FALSE;
	}
  case INTEGER_CST:
  case REAL_CST:
  case STRING_CST:
	return FALSE;
  case NOP_EXPR:
	if (TREE_CODE(TREE_OPERAND(init,0)) == ADDR_EXPR
    	    && TREE_CODE(TREE_OPERAND(TREE_OPERAND(init,0),0)) == STRING_CST) 
		return FALSE;
	else
		return TRUE;
  case ADDR_EXPR: {
	tree t = TREE_OPERAND(init, 0);
	if (DECL_CONTEXT(t) == 0 ||
	    TREE_CODE(DECL_CONTEXT(t)) == NAMESPACE_DECL)
		return FALSE;
	return TRUE;
  }
  default:
	return TRUE;
  }
}

// For a dynamic initialization, we can either
// do a series of moves for each element,
// or we can create a static INITO and structure copy that value.
// GCC allows non-constant initial values, 
// so if any of those exist, we need to assign each element.
// Also, if the init is small we can optimize better
// if we make each element assignment be explicit.
// But otherwise, we create the static INITO since that saves code space.

static BOOL
Use_Static_Init_For_Aggregate (ST *st, tree init)
{
	return !Has_Non_Constant_Init_Value(init);
}


static void
Add_Initv_For_Tree (tree val, UINT size)
{
	switch (TREE_CODE(val)) {
	case INTEGER_CST:
		WFE_Add_Aggregate_Init_Integer (
			Get_Integer_Value(val), size);
		break;
	case REAL_CST:
		WFE_Add_Aggregate_Init_Real (
			TREE_REAL_CST(val), size);
		break;
	case STRING_CST:
		WFE_Add_Aggregate_Init_String (
			TREE_STRING_POINTER(val), 
			MIN(size, TREE_STRING_LENGTH(val)));
	        if (size > TREE_STRING_LENGTH(val)) {
		  WFE_Add_Aggregate_Init_Padding (size - 
                                       TREE_STRING_LENGTH(val));
		}
		break;
#if 0
	case PLUS_EXPR:
		if ( TREE_CODE(TREE_OPERAND(val,0)) == ADDR_EXPR
		     && TREE_CODE(TREE_OPERAND(val,1)) == INTEGER_CST)
		{
			tree addr_kid = TREE_OPERAND(TREE_OPERAND(val,0),0);
			FmtAssert(TREE_CODE(addr_kid) == VAR_DECL
				  || TREE_CODE(addr_kid) == FUNCTION_DECL,
				("expected decl under plus_expr"));
			WFE_Add_Aggregate_Init_Symbol ( Get_ST (addr_kid),
			Get_Integer_Value(TREE_OPERAND(val,1)) );
		}
		else
			FmtAssert(FALSE, ("unexpected tree code %s", 
				tree_code_name[TREE_CODE(val)]));
		break;
#endif
	case NOP_EXPR:
		tree kid;
		kid = TREE_OPERAND(val,0);
		if (TREE_CODE(kid) == ADDR_EXPR
	    		&& TREE_CODE(TREE_OPERAND(kid,0)) == STRING_CST) 
		{
			kid = TREE_OPERAND(kid,0);
			WFE_Add_Aggregate_Init_Address (kid);
			break;
              }
              else
              if (TREE_CODE(kid) == INTEGER_CST) {
                      WFE_Add_Aggregate_Init_Integer (
                              Get_Integer_Value(kid), size);
		      break;
		}
		// fallthru
	default:
		{
		WN *init_wn;
		init_wn = WFE_Expand_Expr (val);
		if (WN_operator (init_wn) == OPR_LDA) {
			WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn),
						       WN_offset (init_wn));
			WN_DELETE_Tree (init_wn);
			break;
		}
		// handle converts over LDA
		if ((WN_opcode (init_wn) == OPC_I4U4CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U4LDA) ||
		    (WN_opcode (init_wn) == OPC_I8U8CVT &&
		     WN_opcode (WN_kid0 (init_wn)) == OPC_U8LDA)) {
			WN *kid0 = WN_kid0(init_wn);
			WFE_Add_Aggregate_Init_Symbol (WN_st (kid0), WN_offset(kid0));
			WN_DELETE_Tree (init_wn);
			break;
		}
		// following cases for ADD and SUB are needed because the
		// simplifier may be unable to fold due to overflow in the
		// 32-bit offset field
		else if (WN_operator(init_wn) == OPR_ADD) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) + WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		 	else if (WN_operator(kid1) == OPR_LDA &&
			    WN_operator(kid0) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid1),
				     WN_offset(kid1) + WN_const_val(kid0));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		else if (WN_operator(init_wn) == OPR_SUB) {
			WN *kid0 = WN_kid0(init_wn);
			WN *kid1 = WN_kid1(init_wn);
		 	if (WN_operator(kid0) == OPR_LDA &&
			    WN_operator(kid1) == OPR_INTCONST) {
			  WFE_Add_Aggregate_Init_Symbol (WN_st (kid0),
				     WN_offset(kid0) - WN_const_val(kid1));
			  WN_DELETE_Tree (init_wn);
			  break;
			}
		}
		FmtAssert(FALSE, ("unexpected tree code %s", 
			tree_code_name[TREE_CODE(val)]));
		}
	}
}

// buffer for simulating the initialized memory unit; it is managed independent
// of host's endianness
class INITBUF { 
public:
  UINT64 ival;
  UINT64 ival1;  // byte 8, for 64 bitfield

  INITBUF(void) {}
  INITBUF(UINT64 i): ival(i), ival1(0) {}
  ~INITBUF(void) {}
  mUINT8 Nth_byte(INT i) { // i must be from 0 to 8
                      if (i <= 7) {
                        INT rshft_amt = (Target_Byte_Sex == BIG_ENDIAN) ? 7-i : i;
                        return (ival >> (rshft_amt * 8)) & 0xff;
                      } else {
                        INT i1 = i - 8;
                        INT rshft_amt = (Target_Byte_Sex == BIG_ENDIAN) ? 7-i1 : i1;
                        return (ival1 >> (rshft_amt * 8)) & 0xff;
                      }
                    }
};

// at entry, assumes that in the current struct, initv for "bytes" bytes have 
// been generated; at exit, "bytes" will be updated with the additional
// bytes that this invocation generates.
static void
Add_Bitfield_Initv_For_Tree (tree val, FLD_HANDLE fld, INT &bytes)
{
  FmtAssert(TREE_CODE(val) == INTEGER_CST,
	    ("initialization value of bitfield expected to be integer, not %s",
	     tree_code_name[TREE_CODE(val)]));
  INT bofst = FLD_bofst(fld);
  INT bsize = FLD_bsize(fld);
  if (bsize == 0)
    return;

  INITBUF ib(Get_Integer_Value(val));
  // truncate ival according to the bitfield size and leave it left-justified
  ib.ival = ib.ival << (64 - bsize);
  // shift the value back right to the precise position within INITBUF
  if (bofst + bsize > 64) {
    // need to use ival1
    ib.ival1 = ib.ival;
    if (Target_Byte_Sex == BIG_ENDIAN) {
      ib.ival  = ib.ival  >> bofst;
      ib.ival1 = ib.ival1 << (64 - bofst);
    } else  {
      ib.ival  = ib.ival  << (bofst + bsize - 64);
      ib.ival1 = ib.ival1 >> (128 - bofst - bsize);
    }
  } else {
    if (Target_Byte_Sex == BIG_ENDIAN)
      ib.ival = ib.ival >> bofst;
    else ib.ival = ib.ival >> (64 - bofst - bsize);
  }

  // find number of bytes to output
  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
  // find number of bytes that have been output with previous bitfields
  INT bytes_out = bytes - FLD_ofst(fld);
  INT i;
  if (bytes_out > 0) {
    // verify that, other than the last output byte, the earlier bytes in 
    // ib are all 0
    for (i = 0; i < bytes_out - 1; i++)
      FmtAssert(ib.Nth_byte(i) == 0, 
		("processing error in Add_Bitfield_Initv_For_Tree"));
    if (ib.Nth_byte(bytes_out-1) != 0) {// merge and change last_aggregate_initv
      if (INITV_kind(last_aggregate_initv) == INITVKIND_VAL) {
        TCON &tc = INITV_tc_val(last_aggregate_initv);
        mUINT8 last_ival = TCON_k0(tc);
        tc.vals.k0 = last_ival | ib.Nth_byte(bytes_out-1);
      }
      else { // need to create a new TCON
        if (INITV_kind(last_aggregate_initv) == INITVKIND_ONE) 
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     1 | ib.Nth_byte(bytes_out-1));
	else {
	  FmtAssert(INITV_kind(last_aggregate_initv) == INITVKIND_ZERO,
		    ("processing error in static bit field initialization"));
	  INITV_Init_Integer(last_aggregate_initv, MTYPE_I1, 
			     ib.Nth_byte(bytes_out-1));
	}
      }
    }
  }
  // output the remaining bytes
  for (i = bytes_out; i < num_of_bytes; i++)
    WFE_Add_Aggregate_Init_Integer(ib.Nth_byte(i), 1);
  bytes += num_of_bytes - bytes_out;
}

// "bytes" will be updated with the additional bytes that this invocation
// generates stores into
static void
Gen_Assign_Of_Init_Val (ST *st, tree init, UINT offset, UINT array_elem_offset,
	TY_IDX ty, BOOL is_bit_field, UINT field_id, FLD_HANDLE fld, INT &bytes)
{
    WN *init_wn = WFE_Expand_Expr (init);
    if (TREE_CODE(init) == STRING_CST && TY_kind(ty) == KIND_ARRAY)
    {
      UINT init_size = TREE_STRING_LENGTH(init);
      // have to store string into address,
      // rather than directy copy assignment,
      // so need special code.
      TY_IDX ptr_ty = Make_Pointer_Type(ty);
      WN *load_wn = WN_CreateMload (0, ptr_ty, init_wn,
				    WN_Intconst(MTYPE_I4, init_size));
      WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
      WFE_Stmt_Append(
		      WN_CreateMstore (offset, ptr_ty,
				       load_wn,
				       addr_wn,
				       WN_Intconst(MTYPE_I4,init_size)),
		      Get_Srcpos());
      bytes += init_size;
      UINT size = TY_size(ty);
      if (size > init_size) {
	/* pad out the remaining bytes to zero. See PR 10092 and  ansi: 6.5.7. 

	   char foo[3] = ""; 
	   printf("%d\n"foo[2]);

	   should produce 0 instead of garbage */
	UINT pad_size = size - init_size;
	WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
	WN *pad_wn = WN_Intconst(MTYPE_U4, pad_size);
	TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
	WFE_Stmt_Append (WN_CreateMstore (init_size, mstore_ty,
					  zero_wn, addr_wn, pad_wn),
			 Get_Srcpos());
	bytes += pad_size;
      }
    }
    else {
	TYPE_ID mtype = is_bit_field ? MTYPE_BS : TY_mtype(ty);
	if (is_bit_field) { 
	    offset = array_elem_offset;	// uses array element offset instead
	} else
	    field_id = 0;	// uses offset instead
	WFE_Set_ST_Addr_Saved (init_wn);
	WN *wn = WN_Stid (mtype, ST_ofst(st) + offset, st,
		ty, init_wn, field_id);
	WFE_Stmt_Append(wn, Get_Srcpos());
	if (! is_bit_field) 
	  bytes += TY_size(ty);
	else {
	  INT bofst = FLD_bofst(fld);
	  INT bsize = FLD_bsize(fld);
	  // find number of bytes to output
	  INT num_of_bytes = ((bofst + bsize - 1) >> 3) + 1;
	  // find number of bytes that have been output with previous bitfields
	  INT bytes_out = bytes - FLD_ofst(fld);
	  bytes += num_of_bytes - bytes_out;
	}
    }
}

UINT
Traverse_Aggregate_Constructor (
  ST   *st, tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

UINT
Traverse_Aggregate_Struct (
  ST   *st, tree init_list, tree type, BOOL gen_initv,
  UINT current_offset, UINT array_elem_offset, UINT field_id);

// For the specified symbol, generate padding at the offset specified.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Pad (
  ST     *st,
  BOOL   gen_initv,
  UINT   pad,
  UINT   current_offset)
{
  if (gen_initv) {
     WFE_Add_Aggregate_Init_Padding (pad);
  }
  else {
    WN *zero_wn = WN_Intconst(MTYPE_U4, 0);
    WN *pad_wn = WN_Intconst(MTYPE_U4, pad);
    WN *addr_wn = WN_Lda(Pointer_Mtype, 0, st);
    TY_IDX mstore_ty = Make_Pointer_Type(MTYPE_To_TY(MTYPE_U1)); // char *
    WFE_Stmt_Append (WN_CreateMstore (current_offset, mstore_ty,
                                      zero_wn, addr_wn, pad_wn),
                     Get_Srcpos());
  }
} /* Traverse_Aggregate_Pad */

// The aggregate element for the specified symbol at the current_offset
// is an array having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.

void
Traverse_Aggregate_Array (
  ST   *st,            // symbol being initialized
  tree init_list,      // list of initializers for each array element
  tree type,           // type of array
  BOOL gen_initv,      // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset) // offset of array from start of symbol
{
  INT    emitted_bytes = 0;
  INT    pad;
  TY_IDX ty            = Get_TY(type);
  TY_IDX ety           = TY_etype (ty);
  UINT   esize         = TY_size (ety);
  tree   init;
  tree   next;

  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = next) {
    // loop through each array element

    next = TREE_CHAIN(init);

    if (TREE_CODE(TREE_VALUE(init)) == PTRMEM_CST)  {
      TREE_VALUE(init) = cplus_expand_constant(TREE_VALUE(init));
    }

    if (TREE_CODE(TREE_VALUE (init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      // update array_elem_offset to current_offset to
      // keep track of where each array element starts
      Traverse_Aggregate_Constructor (st, TREE_VALUE(init), TREE_TYPE(type),
                                      gen_initv, current_offset, current_offset,
                                      0);
      emitted_bytes += esize;
    }

    else {
      // initialize SCALARs and POINTERs
      // note that we should not be encountering bit fields
      if (gen_initv) {
        Add_Initv_For_Tree (TREE_VALUE(init), esize);
        emitted_bytes += esize;
      }
      else
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init), current_offset, 0,
                                ety, FALSE, 0, FLD_HANDLE (), emitted_bytes);
    }

    current_offset += esize;
  }

  // If the entire array has not been initialized, pad till the end
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);

} /* Traverse_Aggregate_Array */

// The aggregate element for the specified symbol at the current_offset
// is a struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the struct, and returns the field_id
// of the last element in the struct if it has elements, otherwise
// it returns the field_id passed in for empty structs

UINT
Traverse_Aggregate_Struct (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initializers for elements in STRUCT
  tree type,              // type of struct
  BOOL gen_initv,         // TRUE if initializing with INITV, FALSE for statements
  UINT current_offset,    // offset from start of symbol for current struct
  UINT array_elem_offset, // if struct is with an array, then it is the
                          //   offset of the outermost struct from the
                          //   array enclosing the struct
                          // if struct is not within an array, it is zero
                          // this is needed when field_id is used to generate
                          //   stores for initialization
  UINT field_id)          // field_id of struct
{
  TY_IDX     ty    = Get_TY(type);       // get WHIRL type
  tree       field = TYPE_FIELDS(type);  // get first field in gcc
  FLD_HANDLE fld   = TY_fld (ty);        // get first field in WHIRL

  INT        emitted_bytes = 0;          // keep track of # of bytes initialize;
  INT        current_offset_base = current_offset;
  INT        pad;
  BOOL       is_bit_field;
  tree       init;
  TY_IDX     fld_ty;

  // account for anonymous WHIRL fields being generated for every direct,
  // nonempty nonvirtual base class.
  // these are generated first in Create_TY_For_Tree (tree_symtab.cxx)

  if (TYPE_BINFO(type) &&
      BINFO_BASETYPES(TYPE_BINFO(type))) {
    tree basetypes = BINFO_BASETYPES(TYPE_BINFO(type));
    INT32 i;
    for (i = 0; i < TREE_VEC_LENGTH(basetypes); ++i) {
      tree binfo = TREE_VEC_ELT(basetypes, i);
      tree basetype = BINFO_TYPE(binfo);
      if (!is_empty_base_class(basetype) ||
          !TREE_VIA_VIRTUAL(binfo)) {
        ++field_id;
        fld = FLD_next (fld);
        field_id += TYPE_FIELD_IDS_USED(basetype);
      }
    }
  }

  while (field && TREE_CODE(field) != FIELD_DECL)
    field = next_real_or_virtual_field(type, field);

  for (init = CONSTRUCTOR_ELTS(init_list);
       init;
       init = TREE_CHAIN(init)) {
    // loop through each initializer specified

    ++field_id; // compute field_id for current field

    // if the initialization is not for the current field,
    // advance the fields till we find it
    if (field && TREE_PURPOSE(init) && TREE_CODE(TREE_PURPOSE(init)) == FIELD_DECL) {
      DevWarn ("Encountered FIELD_DECL during initialization");
      for (;;) {
        if (field == TREE_PURPOSE(init)) {
          break;
        }
        ++field_id;
        fld = FLD_next (fld);
        field = next_real_or_virtual_field(type, field);
        while (field && TREE_CODE(field) != FIELD_DECL)
          field = next_real_or_virtual_field(type, field);
      }
    }

    // check if we need to pad upto the offset of the field
    pad = FLD_ofst (fld) - emitted_bytes;

    if (pad > 0) {
      Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);
      current_offset += pad;
      emitted_bytes  += pad;
    }

    fld_ty = FLD_type(fld);

    if (TREE_CODE(TREE_VALUE(init)) == PTRMEM_CST)  {
      TREE_VALUE(init) = cplus_expand_constant(TREE_VALUE(init));
    }

    if (TREE_CODE(TREE_VALUE(init)) == CONSTRUCTOR) {
      // recursively process nested ARRAYs and STRUCTs
      tree element_type;
      element_type = TREE_TYPE(field);
      field_id = Traverse_Aggregate_Constructor (st, TREE_VALUE(init),
                                                 element_type, gen_initv,
                                                 current_offset,
                                                 array_elem_offset, field_id);
      emitted_bytes += TY_size(fld_ty);
    }
    else {
      // initialize SCALARs and POINTERs
      is_bit_field = FLD_is_bit_field(fld);
      if (gen_initv) {
        if (! is_bit_field) {
          Add_Initv_For_Tree (TREE_VALUE(init), TY_size(fld_ty));
          emitted_bytes += TY_size(fld_ty);
        }
        else { // do 1 byte a time
          Add_Bitfield_Initv_For_Tree (TREE_VALUE(init), fld, emitted_bytes);
          // emitted_bytes updated by the call as reference parameter
        }
      }
      else {
        Gen_Assign_Of_Init_Val (st, TREE_VALUE(init),
                                current_offset, array_elem_offset,
                                is_bit_field ? ty : fld_ty,
                                is_bit_field, field_id, fld, emitted_bytes);
        // emitted_bytes updated by the call as reference parameter
      }
    }

    // advance ot next field
    current_offset = current_offset_base + emitted_bytes;
    fld = FLD_next(fld);
    field = next_real_or_virtual_field(type, field);
    while (field && TREE_CODE(field) != FIELD_DECL)
      field = next_real_or_virtual_field(type, field);
  }

  // if not all fields have been initialized, then loop through
  // the remaining fields to update field_id
  while ( ! fld.Is_Null()) {
    ++field_id;
    fld = FLD_next(fld);
    field = next_real_or_virtual_field(type, field);
    while (field && TREE_CODE(field) != FIELD_DECL)
      field = next_real_or_virtual_field(type, field);
  }

  // if not all fields have been initilaized, then check if
  // padding is needed to the end of struct
  pad = TY_size (ty) - emitted_bytes;

  if (pad > 0)
    Traverse_Aggregate_Pad (st, gen_initv, pad, current_offset);

  return field_id;
} /* Traverse_Aggregate_Struct */

// The aggregate element for the specified symbol at the current_offset
// is either an array or  struct/class/union having the gcc tree type 'type'.
// If gen_initv is TRUE build an initv, otherwise generate a sequence
// of stores.
// It accepts the field_id of the element in the enclosing struct
// used for computing field_ids (0 if no such struct exists)
// If the aggregate element is non-array, it returns the field_id of 
// last field within the aggregate element.
// If the aggregate element is array, then it returns the field_id passed in

UINT
Traverse_Aggregate_Constructor (
  ST   *st,               // symbol being initialized
  tree init_list,         // list of initilaizers for this aggregate
  tree type,              // type of aggregate being initialized
  BOOL gen_initv,         // TRUE  if initializing with INITV,
                          // FALSE if initializing with statements
  UINT current_offset,    // offset from start of symbol for this aggregate
  UINT array_elem_offset,
  UINT field_id)
{
  TY_IDX ty = Get_TY(type);

  INITV_IDX last_aggregate_initv_save;

  if (gen_initv) {

    WFE_Add_Init_Block();
    INITV_Init_Block(last_aggregate_initv, INITV_Next_Idx());
    not_at_root = TRUE;
    last_aggregate_initv_save = last_aggregate_initv;
    last_aggregate_initv = 0;
  }

  if (TY_kind (ty) == KIND_STRUCT) {

    field_id = Traverse_Aggregate_Struct (st, init_list, type, gen_initv,
                                          current_offset, array_elem_offset,
                                          field_id);
  }

  else
  if (TY_kind (ty) == KIND_ARRAY) {

    Traverse_Aggregate_Array (st, init_list, type, gen_initv, current_offset);
  }

  else
    Fail_FmtAssertion ("Traverse_Aggregate_Constructor: non STRUCT/ARRAY");

  // for empty list; set to reserved value (fix for PR15262 from PathScale)
  if (gen_initv && last_aggregate_initv == 0)
    INITV_Init_Block(last_aggregate_initv_save, INITV_IDX_ZERO);

  // restore current level's last_aggregate_initv and return
  last_aggregate_initv = last_aggregate_initv_save;

  return field_id;
} /* Traverse_Aggregate_Constructor */

static void
Add_Inito_For_Tree (tree init, ST *st)
{
  tree kid;
  last_aggregate_initv = 0;
  switch (TREE_CODE(init)) {
  case INTEGER_CST:
	UINT64 val;
	val = Get_Integer_Value (init);
	// if -mzero-init-data, keep as dglobal inito
	if (val == 0 && ! xt_zero_init_data) {
		Set_ST_init_value_zero(st);
		if (ST_sclass(st) == SCLASS_DGLOBAL)
			Set_ST_sclass(st, SCLASS_UGLOBAL);
		return;
	}
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (val, TY_size(ST_type(st)));
	return;
  case REAL_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Real (TREE_REAL_CST(init), 
		TY_size(ST_type(st)));
	return;
  case COMPLEX_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Complex (TREE_REAL_CST(TREE_REALPART(init)), 
					TREE_REAL_CST(TREE_IMAGPART(init)), 
					TY_size(ST_type(st)));
	return;
  case STRING_CST:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_String (TREE_STRING_POINTER(init),
                                       TY_size(ST_type(st)) <
                                       TREE_STRING_LENGTH(init) ?
                                       TY_size(ST_type(st)) :
                                       TREE_STRING_LENGTH(init));
	if (TY_size (ST_type(st)) > TREE_STRING_LENGTH(init))
		WFE_Add_Aggregate_Init_Padding ( TY_size (ST_type(st)) -
                                       TREE_STRING_LENGTH(init));
	return;
  case NOP_EXPR:
	Add_Inito_For_Tree (TREE_OPERAND(init,0), st);
	return;
  case ADDR_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == VAR_DECL ||
	    TREE_CODE(kid) == FUNCTION_DECL ||
	    TREE_CODE(kid) == STRING_CST) {
		aggregate_inito = New_INITO (st);
		not_at_root = FALSE;
		WFE_Add_Aggregate_Init_Address (kid);
		return;
	}
  case PLUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
		// symbol+offset
		Add_Inito_For_Tree (kid, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				Get_Integer_Value(kid));
			return;
		}
	}
	break;
  case MINUS_EXPR:
	kid = TREE_OPERAND(init,0);
	if (TREE_CODE(kid) == ADDR_EXPR) {
		// symbol-offset
		Add_Inito_For_Tree (kid, st);
		kid = TREE_OPERAND(init,1);
		if (INITV_kind(last_aggregate_initv) == INITVKIND_SYMOFF
			&& TREE_CODE(kid) == INTEGER_CST)
		{
			Set_INITV_ofst (last_aggregate_initv,
				-Get_Integer_Value(kid));
			return;
		}
	}
	break;
  case CONSTRUCTOR:
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
					TRUE /*gen_initv*/, 0, 0, 0);
	return;
  }

  // not recognized, so try to simplify
  WN *init_wn = WFE_Expand_Expr (init);
  if (WN_operator(init_wn) == OPR_INTCONST) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Integer (
		WN_const_val(init_wn), TY_size(ST_type(st)));
	return;
  }
  else 
  if (WN_operator(init_wn) == OPR_LDA) {
	aggregate_inito = New_INITO (st);
	not_at_root = FALSE;
	WFE_Add_Aggregate_Init_Symbol (WN_st (init_wn), WN_offset (init_wn));
	return;
  }
  else
  if (WN_operator(init_wn) == OPR_ADD) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) + WN_const_val(WN_kid1(init_wn)));
      return;
    }
  }
  else
  if (WN_operator(init_wn) == OPR_SUB) {
    if (WN_operator(WN_kid0(init_wn)) == OPR_LDA &&
        WN_operator(WN_kid1(init_wn)) == OPR_INTCONST) {
      aggregate_inito = New_INITO (st);
      not_at_root = FALSE;
      WFE_Add_Aggregate_Init_Symbol (WN_st(WN_kid0(init_wn)),
		WN_offset(WN_kid0(init_wn)) - WN_const_val(WN_kid1(init_wn)));
      return;
    }
  }
  Fail_FmtAssertion ("unexpected static init tree for %s", ST_name(st));
}


extern ST *
WFE_Generate_Temp_For_Initialized_Aggregate (tree init, char * name)
{
  TY_IDX ty_idx = Get_TY(TREE_TYPE(init));
  ST *temp = New_ST (CURRENT_SYMTAB);
  ST_Init (temp,
	Save_Str2 (name, ".init"),
	CLASS_VAR, SCLASS_PSTATIC, EXPORT_LOCAL,
	ty_idx );
  if (TREE_CODE(init) == CONSTRUCTOR
	&& ! Use_Static_Init_For_Aggregate (temp, init)) 
  {
	// do sequence of stores to temp
	Set_ST_sclass(temp, SCLASS_AUTO);	// put on stack
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
                                        FALSE /*gen_initv*/, 0, 0, 0);
  }
  else {
	// setup inito for temp
	Set_ST_is_initialized(temp);
	aggregate_inito = New_INITO (temp);
	not_at_root = FALSE;
	last_aggregate_initv = 0;
	Traverse_Aggregate_Constructor (temp, init, TREE_TYPE(init),
                                        TRUE /*gen_initv*/, 0, 0, 0);
	WFE_Finish_Aggregate_Init ();
  }
  return temp;
}

static tree init_decl = NULL;

extern void
WFE_Initialize_Decl (tree decl)
{
  if (TREE_CODE (decl) == VAR_DECL && DECL_ARTIFICIAL (decl) &&
      TREE_STATIC (decl) && !TREE_PUBLIC (decl) &&
      !TREE_USED (decl)) {
    // ignore initialization unless really used
    // e.g. FUNCTION and PRETTY_FUNCTION
#ifdef WFE_DEBUG
    fprintf(stderr, "Init_Decl: ignoring initialization of %s\n",
	    IDENTIFIER_POINTER(DECL_NAME(decl)));
#endif
    return;
  }
  ST *st = Get_ST(decl);
  tree init = DECL_INITIAL(decl);
  if (init->common.code == VAR_DECL &&
      DECL_CONTEXT(init)	    &&
      TREE_CODE(DECL_CONTEXT(init)) == RECORD_TYPE)
    Get_TY(DECL_CONTEXT(init));

  if (TREE_STATIC(decl) || DECL_CONTEXT(decl) == NULL) 
  {
	// static or global context, so needs INITO
	if (ST_sclass(st) == SCLASS_UGLOBAL && !ST_init_value_zero(st)  ||
	    ST_sclass(st) == SCLASS_EXTERN  			        ||
	    ST_sclass(st) == SCLASS_COMMON)
		Set_ST_sclass(st, SCLASS_DGLOBAL);
	if (!ST_is_initialized(st)) {
		Set_ST_is_initialized(st);
		if (init_decl) {
		  Push_Deferred_Decl_Init (decl);
		  return;
		}
		init_decl = decl;
		Add_Inito_For_Tree (init, st);
		while (deferred_decl_init_i >= 0) {
		  init_decl = Pop_Deferred_Decl_Init ();
		  Add_Inito_For_Tree (DECL_INITIAL(init_decl),
				      Get_ST(init_decl));
		}
		init_decl = NULL;
	}
	if (TREE_READONLY(decl))
		Set_ST_is_const_var (st);
  }
  else {
	// mimic an assign
	if (TREE_CODE(init) == CONSTRUCTOR) {
		// is aggregate
		if (Use_Static_Init_For_Aggregate (st, init)) {
			// create inito for initial copy
			// and store that into decl
			ST *copy = WFE_Generate_Temp_For_Initialized_Aggregate(
					init, ST_name(st));
			WN *init_wn = WN_CreateLdid (OPR_LDID, MTYPE_M, MTYPE_M,
				0, copy, ST_type(copy));
			WFE_Stmt_Append(
				WN_CreateStid (OPR_STID, MTYPE_V, MTYPE_M,
					0, st, ST_type(st), init_wn),
				Get_Srcpos());
		}
		else {
			// do sequence of stores for each element
			Traverse_Aggregate_Constructor (st, init, TREE_TYPE(init),
                                FALSE /*gen_initv*/, 0, 0, 0);
		}
		return;
	}
	else {
		INT emitted_bytes;
		Gen_Assign_Of_Init_Val (st, init, 
			0 /*offset*/, 0 /*array_elem_offset*/,
			ST_type(st), FALSE, 0 /*field_id*/,
			FLD_HANDLE(), emitted_bytes);
	}
  }
}

void
WFE_Decl (tree decl)
{
  if (DECL_INITIAL (decl) != 0) return;	// already processed
  if (DECL_IGNORED_P(decl)) return;
  if (TREE_CODE(decl) != VAR_DECL) return;
  if (DECL_CONTEXT(decl) != 0) return;	// local
  if ( ! TREE_PUBLIC(decl)) return;	// local
  if ( ! TREE_STATIC(decl)) return;	// extern
  // is something that we want to put in symtab
  // (a global var defined in this file).
  (void) Get_ST(decl);
}

void
WFE_Assemble_Alias (tree decl, tree target)
{
  DevWarn ("__attribute alias encountered at line %d", lineno);
  tree base_decl = lookup_name (target, 1);
  ST *base_st;
  ST *st = Get_ST (decl);
  if (!base_decl) {
    base_st = Copy_ST(st);
    Set_ST_name(base_st, Save_Str(IDENTIFIER_POINTER (target)));
    Clear_ST_is_weak_symbol(base_st);
  } else {
    base_st = Get_ST (base_decl);
  }
  if (ST_is_weak_symbol(st)) {
    Set_ST_strong_idx (*st, ST_st_idx (base_st));
    Set_ST_sclass (st, SCLASS_EXTERN);
  }
  else {
    Set_ST_base_idx (st, ST_st_idx (base_st));
    Set_ST_emit_symbol(st);     // for cg
    Set_ST_sclass (st, ST_sclass (base_st));
    if (ST_is_initialized (base_st))
      Set_ST_is_initialized (st);
  }
/*
  if (ST_is_initialized (base_st)) {
    Set_ST_is_initialized (st);
    if (ST_init_value_zero (base_st))
      Set_ST_init_value_zero (st);
  }
*/
} /* WFE_Assemble_Alias */

#if 0
void
WFE_Assemble_Constructor (char *name)
{
  DevWarn ("__attribute__ ((constructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name), 1);
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__ctors", "_", ++__ctors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".ctors"));
  Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
  Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
  Set_ST_addr_saved (func_st);
}

void
WFE_Assemble_Destructor (char *name)
{
  DevWarn ("__attribute__ ((destructor)) encountered at line %d", lineno);
  tree func_decl = lookup_name (get_identifier (name), 1);
  ST *func_st = Get_ST (func_decl);
  INITV_IDX initv = New_INITV ();
  INITV_Init_Symoff (initv, func_st, 0, 1);
  ST *init_st = New_ST (GLOBAL_SYMTAB);
  ST_Init (init_st, Save_Str2i ("__dtors", "_", ++__dtors),
           CLASS_VAR, SCLASS_FSTATIC,
           EXPORT_LOCAL, Make_Pointer_Type (ST_pu_type (func_st), FALSE));
  Set_ST_is_initialized (init_st);
  INITO_IDX inito = New_INITO (init_st, initv);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (init_st), ST_ATTR_SECTION_NAME,
                Save_Str (".dtors"));
  Set_PU_no_inline (Pu_Table [ST_pu (func_st)]);
  Set_PU_no_delete (Pu_Table [ST_pu (func_st)]);
  Set_ST_addr_saved (func_st);
}

#endif

ST *
WFE_Get_Return_Address_ST (int level)
{
  ST *return_address_st = Return_Address_ST [CURRENT_SYMTAB - level];
  if (return_address_st == NULL) {
    return_address_st = New_ST (CURRENT_SYMTAB - level);
    ST_Init (return_address_st, Save_Str ("__return_address"), CLASS_VAR,
             SCLASS_AUTO, EXPORT_LOCAL, 
             Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE));
    Set_ST_is_return_var (return_address_st);
    Return_Address_ST [CURRENT_SYMTAB - level] = return_address_st;
  }

  return return_address_st;
} /* WFE_Get_Return_Address_ST */

ST *
WFE_Alloca_0 (void)
{
  WN *wn;
  TY_IDX ty_idx = Make_Pointer_Type (Be_Type_Tbl (MTYPE_V), FALSE);
  ST* alloca_st = Gen_Temp_Symbol (ty_idx, "__alloca");
  wn = WN_CreateAlloca (WN_CreateIntconst (OPC_I4INTCONST, 0));
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ty_idx, wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  Set_PU_has_alloca (Get_Current_PU ());
  return alloca_st;
} /* WFE_Alloca_0 */

ST *
WFE_Alloca_ST (tree decl)
{
  ST *st = Get_ST (decl);
  ST *alloca_st = New_ST (CURRENT_SYMTAB);
  ST_Init (alloca_st, Save_Str (ST_name (st)),
           CLASS_VAR, SCLASS_AUTO, EXPORT_LOCAL,
           Make_Pointer_Type (ST_type (st), FALSE));
  Set_ST_is_temp_var (alloca_st);
  Set_ST_pt_to_unique_mem (alloca_st);
  Set_ST_base_idx (st, ST_st_idx (alloca_st));
  WN *swn = WFE_Expand_Expr (TYPE_SIZE_UNIT(TREE_TYPE(decl)));

#ifndef TARG_XTENSA
  /* For xtensa we handle this in code selection. */
  /* bump the alloca size to next 16 multiples */
  TYPE_ID rtype = WN_rtype(swn);
  swn = WN_Binary (OPR_ADD, rtype, WN_Intconst (rtype, 15), swn);
  swn = WN_Binary (OPR_BAND, rtype, WN_Intconst (rtype, -16), swn);
#endif

  WN *wn  = WN_CreateAlloca (swn);
  wn = WN_Stid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st), wn);
  WFE_Stmt_Append (wn, Get_Srcpos());
  Set_PU_has_alloca (Get_Current_PU ());
  return st;
} /* WFE_Alloca_ST */

void
WFE_Dealloca (ST *alloca_st, tree vars)
{
  int  nkids = 0;
  tree decl;
  WN   *wn;
  ST   *st;
  ST   *base_st;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        ++nkids;
  }

  wn = WN_CreateDealloca (nkids+1);
  WN_kid0 (wn) = WN_Ldid (Pointer_Mtype, 0, alloca_st, ST_type (alloca_st));
  nkids = 0;

  for (decl =vars; decl; decl = TREE_CHAIN (decl))
    if (TREE_CODE (decl) == VAR_DECL && DECL_ST (decl)) {
      st = DECL_ST (decl);
      base_st = ST_base (st);
      if (st != base_st)
        WN_kid (wn, ++nkids) = WN_Ldid (Pointer_Mtype, 0, base_st, ST_type (base_st));
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Dealloca */

void
WFE_Record_Asmspec_For_ST (ST *st, tree decl, char *asmspec, int reg)
{
  TY_IDX ty_idx = ST_type (st);

  ISA_REGCLASS rc = TI_ISA_Regclass_For_Mtype(TY_mtype(ty_idx));
  const ISA_REGCLASS_INFO *info = TI_ISA_Regclass_Info(rc);
  UINT cnt = TI_ISA_Regclass_Last_Reg(info) - TI_ISA_Regclass_First_Reg(info) + 1;

  bool found=false;
  for (UINT i = 0; i < cnt; i++) {
    if (!strcmp(reg_names[reg], TI_ISA_Regclass_Reg_Name(info, i)))
      found = true;
  }

  if (!found && rc==TI_ISA_Regclass_Integer()) {
    rc = TI_ISA_Regclass_Special();
    info = TI_ISA_Regclass_Info(rc);
    cnt = TI_ISA_Regclass_Last_Reg(info) - TI_ISA_Regclass_First_Reg(info) + 1;

    for (UINT i = 0; i < cnt; i++) {
      if (!strcmp(reg_names[reg], TI_ISA_Regclass_Reg_Name(info, i)))
	found = true;
    }
  }

  if (!found) {
    error_with_decl(decl,
		"register number for `%s' isn't suitable for data type");
  }

  Set_TY_is_volatile (ty_idx);
  Set_ST_type (st, ty_idx);
  Set_ST_assigned_to_dedicated_preg (st);
  ST_ATTR_IDX st_attr_idx;
  ST_ATTR&    st_attr = New_ST_ATTR (CURRENT_SYMTAB, st_attr_idx);
  ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_DEDICATED_REGISTER, reg);
} /* WFE_Record_Asmspec_For_ST */

void
WFE_Record_Asmspec_For_Decl (tree decl, char *asmspec, int reg)
{
  ST *st = Get_ST (decl);
  WFE_Record_Asmspec_For_ST (st, decl, asmspec, reg);
}

void
WFE_Resolve_Duplicate_Decls (tree olddecl, tree newdecl)
{
  ST     *st      = DECL_ST(olddecl);
  tree    newtype = TREE_TYPE(newdecl);
  tree    newsize = TYPE_SIZE(newtype);
  TY_IDX  ty      = ST_type (st);

  if (TREE_STATIC(olddecl) == FALSE &&
      TREE_STATIC(newdecl) == TRUE  &&
      TREE_PUBLIC(olddecl) == TRUE  &&
      TREE_PUBLIC(newdecl) == FALSE) {
    Set_ST_sclass (st, SCLASS_FSTATIC);
    Set_ST_export (st, EXPORT_LOCAL);
  }

  if (newsize                           &&
      TREE_CODE(newsize) == INTEGER_CST &&
      TY_size (ty) <= Get_Integer_Value (newsize) / BITSPERBYTE) {
    UINT64 size = Get_Integer_Value (newsize) / BITSPERBYTE;
    Set_TY_size (ty, size);
    if (TY_kind (ty) == KIND_ARRAY) {
      Set_ARB_const_ubnd (TY_arb(ty));
      Set_ARB_ubnd_val (TY_arb(ty), (size / TY_size(TY_etype(ty))) - 1);
    }
  } 
} /* WFE_Resolve_Duplicate_Decls */


void
WFE_Add_Weak (tree decl)
{
  if (decl) {
    ST *st = DECL_ST (decl);
    if (st==NULL) {
      st = Get_ST (decl);
      DECL_ST (decl) = st;
    }
    Set_ST_is_weak_symbol (st);
  }
} /* WFE_Add_Weak */


void
WFE_Weak_Finish ()
{
  struct weak_syms *t;
  for (t = weak_decls; t; t = t->next) {
    if (t->name) {
      tree decl = lookup_name (get_identifier (t->name), 1);
      if (!decl) {
        INT i;
        BOOL found = FALSE;
        ST *st;
        FOREACH_SYMBOL (GLOBAL_SYMTAB, st, i) {
          if (strcmp (ST_name(st), t->name) == 0) {
            found = TRUE;
            Set_ST_is_weak_symbol (st);
            break;
          }
        }
        if (!found)
          warning ("did not find declaration `%s' for used in #pragma weak", t->name);
      }
      else {
        ST *st = DECL_ST (decl);
	if (st == NULL && t->value) {
	  st = Get_ST (decl);
	}
        if (st) {
          Set_ST_is_weak_symbol (st);
          if (t->value) {
            tree base_decl = lookup_name (get_identifier (t->value), 1);
            if (!base_decl)
               warning ("did not find declaration for `%s' used in #pragma weak", t->value);
            else {
              ST *base_st = DECL_ST (base_decl);
              if (base_st)
                Set_ST_strong_idx (*st, ST_st_idx (base_st));
            }
          }
        }
      }
    }
  }
} /* WFE_Weak_Finish */
