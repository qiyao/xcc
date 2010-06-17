
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

#include "gnu_config.h"
#include "gnu/system.h"

#include "srcpos.h"
#include "gnu/machmode.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "cp-tree.h"
#include "gnu/output.h"         // For decode_reg_name
#include "eh-common.h"
}
#include "gnu/flags.h"
#undef TARGET_PENTIUM  // hack around macro definition in gnu
#include "insn-config.h"	// MAX_RECOG_OPERANDS
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include "wfe_expr.h"
#include "wfe_stmt.h"
#include "wfe_decl.h"
#include "tree_symtab.h"
#include "rt_symtab.h"
#include "targ_sim.h"
#include "config_targ_options.h"

extern "C" void error (const char *, ...);	// in gnu/errors.h

//#define WFE_DEBUG 

char *WFE_Tree_Node_Name (tree op);

#define ENLARGE(x) (x + (x >> 1))

static BOOL  *if_else_info_stack;
static INT32  if_else_info_i;
static INT32  if_else_info_max;

typedef struct case_info_t {
  INT64     case_lower_bound_value;
  INT64     case_upper_bound_value;
  LABEL_IDX case_label_idx;
} CASE_INFO;

static CASE_INFO   *case_info_stack;
static INT32        case_info_i;
static INT32        case_info_max;


typedef struct switch_info_t {
  WN        *index;
  INT32      start_case_index;
  LABEL_IDX  default_label_idx;
} SWITCH_INFO;

static SWITCH_INFO *switch_info_stack;
static INT32        switch_info_i;
static INT32        switch_info_max;

typedef struct break_continue_info_t {
  int       tree_code;
  LABEL_IDX break_label_idx;
  LABEL_IDX continue_label_idx;
  tree	    scope;
} BREAK_CONTINUE_INFO;

static BREAK_CONTINUE_INFO *break_continue_info_stack;
static INT32		    break_continue_info_i;
static INT32		    break_continue_info_max;

typedef struct label_info_t {
  LABEL_IDX         label_idx;
  unsigned char     symtab_idx;
  unsigned char     defined;
} LABEL_INFO;

static LABEL_INFO  *undefined_labels_stack;
static INT32        undefined_labels_i;
static INT32        undefined_labels_max;

typedef struct eh_cond_cleanup_info_t {
  INT32             id;
  WN *              region_block;
  WN *              region_after;
} EH_COND_CLEANUP_INFO;

#define EH_COND_GUARD_MTYPE  MTYPE_U4
static INT32                 next_cond_cleanup_id;
static EH_COND_CLEANUP_INFO *eh_cond_cleanup_stack;
static INT32	    	     eh_cond_cleanup_i;
static INT32	    	     eh_cond_cleanup_max;

typedef struct scope_cleanup_info_t {
  tree		    stmt;
  LABEL_IDX	    label_idx;
  WN *		    region;
} SCOPE_CLEANUP_INFO;

static SCOPE_CLEANUP_INFO *scope_cleanup_stack;
static INT32	    	   scope_cleanup_i;
static INT32	    	   scope_cleanup_max;

typedef struct partials_cleanup_info_t {
  tree		    stmt;
  WN *		    region;
} PARTIALS_CLEANUP_INFO;

static PARTIALS_CLEANUP_INFO *partials_cleanup_stack;
static INT32	    	      partials_cleanup_i;
static INT32	    	      partials_cleanup_max;

static tree	   *scope_stack;
static INT32	    scope_i;
static INT32	    scope_max;

typedef struct temp_cleanup_info_t {
  tree		    expr;
  INT32		    scope_depth;
  BOOL	            is_cleanup;
  WN *		    region;
  ST *              st;
  WN *              guard_init_block;
  WN *              guard_init_after;
} TEMP_CLEANUP_INFO;

static TEMP_CLEANUP_INFO *temp_cleanup_stack;
static INT32	    	  temp_cleanup_i;
static INT32	    	  temp_cleanup_max;

typedef struct handler_info_t {
  tree		    handler;
  LABEL_IDX	    label_idx;
} HANDLER_INFO;

static HANDLER_INFO *handler_info_stack;
static INT32	     handler_info_i;
static INT32	     handler_info_max;

typedef struct eh_cleanup_info {
  tree		     cleanup;
  LABEL_IDX	     handler_label_idx;
  LABEL_IDX	     goto_label_idx;
  LABEL_IDX	     goto_idx;
} EH_CLEANUP_INFO;

static EH_CLEANUP_INFO *eh_cleanup_stack;
static INT32		eh_cleanup_i;
static INT32		eh_cleanup_max;

static INT32	    scope_number;
static INT32        scope_depth;

static ST_IDX
Tid_For_Handler (tree handler)
{
  tree t = HANDLER_BODY (handler);
  while (t && (TREE_CODE(t) != COMPOUND_STMT))
    t = TREE_CHAIN(t);

  if (t)
  {
    t = COMPOUND_BODY(t);
    while (t && (TREE_CODE(t) != START_CATCH_STMT))
      t = TREE_CHAIN(t);

    if (t)
      t = TREE_TYPE(t);
  }

  return t ? ST_st_idx(Get_ST (TREE_OPERAND(t, 0))) : ST_IDX_ZERO;
}


/* Create and return the TY that represents an EH-region info
   struct. */
static INITO_IDX
EH_Region_Info_Ty_Idx (UINT size)
{
  static UINT unique_id = 0;
  
  TY_IDX ty_idx;
  TY& ty = New_TY(ty_idx);
  TY_Init(ty, size, KIND_STRUCT, MTYPE_M,
	  Save_Str2i(".ehinfo.", ST_name(Get_Current_PU_ST()), unique_id++));
  Set_TY_align(ty_idx, 4);

  return ty_idx;
}


/* Create a symbol holding the EH info for a try region; type,
   'handler' and 'rtti'. */
static INITV_IDX
Create_Try_Region_Info_Block (INITV_IDX block, xtensa_eh_types eh_type,
			      LABEL_IDX handler, ST_IDX rtti)
{
  TYPE_ID type_mtype = Target_ABI == ABI_CALL0 ? MTYPE_U2 : MTYPE_U4;

  INITV_IDX prev_inv;
  INITV_IDX inv = New_INITV();
  INITV_Init_Block(block, inv);

  INITV_Init_Integer(inv, type_mtype, eh_type);
  prev_inv = inv;

  if (Target_ABI == ABI_CALL0)
  {
    inv = New_INITV();
    INITV_Init_Integer(inv, type_mtype, 0);
    Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    prev_inv = inv;
  }

  inv = New_INITV();
  INITV_Init_Label(inv, handler, 1);
  Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  prev_inv = inv;

  inv = New_INITV();
  if (rtti != ST_IDX_ZERO)
    INITV_Init_Symoff(inv, ST_ptr(rtti), 0, 1);
  else if (eh_type == XT_EH_TYPE_TRY)
    INITV_Init_Integer(inv, MTYPE_U4, (unsigned int)CATCH_ALL_TYPE);
  else
    INITV_Init_Integer(inv, MTYPE_U4, 0);
  Append_INITV(inv, INITO_IDX_ZERO, prev_inv);

  return block;
}


/* Create a symbol holding the EH info, 'handler_idx' and 'rtti_idx'. Return
   the INITO referred to by the symbol. */
static INITO_IDX
Create_Try_Region_Info (xtensa_eh_types eh_type, tree t,
			LABEL_IDX handler_idx, ST_IDX rtti_idx)
{
#define TRY_REGION_INFO_BLOCK_SZ 12
  
  /* Determine the number of handler entries... */
  UINT handler_cnt = ((handler_idx != LABEL_IDX_ZERO) ? 1 : 0);
  
  if (t && (TREE_CODE(t) == TRY_BLOCK))
  {
    for (tree handler = TRY_HANDLERS(t); handler; handler = TREE_CHAIN(handler))
      if (HANDLER_LABEL(handler))
	handler_cnt++;
  }

  /* Build type and symbol... */
  TY_IDX ty_idx = EH_Region_Info_Ty_Idx(4 + (handler_cnt * TRY_REGION_INFO_BLOCK_SZ));

  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, TY_name_idx(ty_idx),
	  CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  Set_ST_is_not_used(st);

  /* The INITO contains the count of how many handler info entries,
     and then the entries themselves, each in it's own block. */
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U4, handler_cnt);
  INITV_IDX prev_inv = Append_INITV(inv, inito, INITV_IDX_ZERO);
  
  /* First add all the handlers from tree 't'. */
  if (t && (TREE_CODE(t) == TRY_BLOCK))
  {
    for (tree handler = TRY_HANDLERS(t); handler; handler = TREE_CHAIN(handler))
      if (HANDLER_LABEL(handler))
      {
	inv = Create_Try_Region_Info_Block(New_INITV(), eh_type,
					   HANDLER_LABEL(handler),
					   Tid_For_Handler(handler));
	prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
      }
  }

  /* If 'handler_idx' is not LABEL_IDX_ZERO, then add an INITV for it. */
  if (handler_idx != LABEL_IDX_ZERO)
  {
    inv = Create_Try_Region_Info_Block(New_INITV(), eh_type, handler_idx, rtti_idx);
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  }

  return inito;
}


/* Create a symbol holding the EH info, 'obj_idx' and
   'dest_idx'. Return the INITO referred to by the symbol. */
static INITO_IDX
Create_Cleanup_Region_Info (xtensa_eh_types eh_type, ST_IDX obj_idx, ST_IDX dest_idx)
{
  int block_size;

  if (Target_ABI == ABI_CALL0 && eh_type == XT_EH_TYPE_CATCH)
    block_size = 8;
  else
    block_size = 12;
  
  /* Build type and symbol... */
  TY_IDX ty_idx = EH_Region_Info_Ty_Idx(4 + block_size);

  ST *st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, TY_name_idx(ty_idx),
	  CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, ty_idx);
  Set_ST_is_initialized(st);
  Set_ST_is_not_used(st);

  /* The INITO contains the count of how many entries, and then the
     entries themselves, each in it's own block. */
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U4, 1);
  INITV_IDX prev_inv = Append_INITV(inv, inito, INITV_IDX_ZERO);

  INITV_IDX block = New_INITV();
  inv = New_INITV();
  INITV_Init_Block(block, inv);
  Append_INITV(block, INITO_IDX_ZERO, prev_inv);

  TYPE_ID type_mtype = Target_ABI == ABI_CALL0 ? MTYPE_U2 : MTYPE_U4;
  INITV_Init_Integer(inv, type_mtype, eh_type);
  prev_inv = inv;

  if (Target_ABI == ABI_CALL0)
  {
    inv = New_INITV();
    INITV_Init_Integer(inv, type_mtype, 0);
    Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    prev_inv = inv;
  }
  inv = New_INITV();
  if (obj_idx != ST_IDX_ZERO)
    INITV_Init_Symoff(inv, ST_ptr(obj_idx), 0, 1);
  else
    INITV_Init_Integer(inv, MTYPE_U4, 0);

  Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  prev_inv = inv;

  if (dest_idx != ST_IDX_ZERO) {
    inv = New_INITV();
    INITV_Init_Symoff(inv, ST_ptr(dest_idx), 0, 1);
    Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
  }
  else
    if (Target_ABI != ABI_CALL0) {
      inv = New_INITV();
      INITV_Init_Integer(inv, MTYPE_U4, 0);
      Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    }

  return inito;
}

static void
Push_Handler_Info (tree handler, LABEL_IDX label_idx)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));
  
  if (++handler_info_i == handler_info_max) {
    handler_info_max = ENLARGE (handler_info_max);
    handler_info_stack =
      (HANDLER_INFO *) realloc (handler_info_stack,
				handler_info_max * sizeof (HANDLER_INFO));
  }
  
  handler_info_stack [handler_info_i].handler   = handler;
  handler_info_stack [handler_info_i].label_idx = label_idx;
}

static void
Push_EH_Cleanup (tree cleanup, LABEL_IDX handler_label_idx,
		 LABEL_IDX goto_label_idx, LABEL_IDX goto_idx)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));

  if (++eh_cleanup_i == eh_cleanup_max) {
    eh_cleanup_max = ENLARGE (eh_cleanup_max);
    eh_cleanup_stack =
      (EH_CLEANUP_INFO *) realloc (eh_cleanup_stack,
			    eh_cleanup_max * sizeof (EH_CLEANUP_INFO));
  }

  eh_cleanup_stack[eh_cleanup_i].cleanup   = cleanup;
  eh_cleanup_stack[eh_cleanup_i].handler_label_idx = handler_label_idx;
  eh_cleanup_stack[eh_cleanup_i].goto_label_idx  = goto_label_idx;
  eh_cleanup_stack[eh_cleanup_i].goto_idx  = goto_idx;
}

static void Call_Rethrow (void);
static void Call_Terminate (void);

static void
Do_Handlers (tree t, LABEL_IDX label_idx)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));

  while (t)
  {
    WN *handler_label = WN_CreateLabel((ST_IDX) 0, HANDLER_LABEL(t), 0, NULL);
    WN_Set_Label_Is_Handler_Begin(handler_label);
    WFE_Stmt_Append(handler_label, Get_Srcpos());
  
    if (TREE_CODE(t) == HANDLER)
    {
      tree body = HANDLER_BODY(t);
      for (; body; body = TREE_CHAIN(body))
	WFE_Expand_Stmt (body);
      WFE_Stmt_Append (WN_CreateGoto ((ST_IDX) NULL, label_idx),
		       Get_Srcpos());
    }
    else
    {
      WFE_One_Stmt (t);
      Call_Throw();
    }

    t = TREE_CHAIN(t);
  }
}

static void
Do_EH_Cleanups (void)
{
#ifdef TARG_XTENSA
  /* We emit the cleanups right after the eh region, not at the end of
     the function; so we shouldn't have any pending cleanups that need
     emission... */
  FmtAssert(eh_cleanup_i == -1, ("unexpected non-emitted eh cleanups at end of function"));
#else
  for (int i = 0; i <= eh_cleanup_i; ++i) {
    Is_True(flag_exceptions, ("expecting exceptions to be enabled"));

    WN *handler_label = WN_CreateLabel((ST_IDX) 0,
				       eh_cleanup_stack[i].handler_label_idx,
				       0, NULL);
    WN *goto_label = WN_CreateLabel((ST_IDX) 0,
				    eh_cleanup_stack[i].goto_label_idx,
				    0, NULL);
    WFE_Stmt_Append(handler_label, Get_Srcpos());
    WFE_Stmt_Append(goto_label, Get_Srcpos());

    /* We mark 'handler_label' as being a handler so that the block
       won't be eliminated as being unreachable (and probably for
       other reasons important to be). But we need another label for
       the handler, 'goto_label', that is not marked as a handler so
       that we can target the cleanup from another cleanup (and opt
       doesn't seem to like a handler label being targeted by a
       goto. */
    WN_Set_Label_Is_Handler_Begin(handler_label);

    WFE_One_Stmt (CLEANUP_EXPR(eh_cleanup_stack [i].cleanup));
    LABEL_IDX goto_idx = eh_cleanup_stack [i].goto_idx;
    if (goto_idx)
      WFE_Stmt_Append (
        WN_CreateGoto ((ST_IDX) 0, goto_idx), Get_Srcpos());
    else
      Call_Rethrow();
  }

  eh_cleanup_i = -1;
#endif
}


void
Push_Conditional_Cleanup_Level (WN *uncond_region, WN *uncond_after)
{
  if (++eh_cond_cleanup_i == eh_cond_cleanup_max) {
    eh_cond_cleanup_max = ENLARGE (eh_cond_cleanup_max);
    eh_cond_cleanup_stack =
      (EH_COND_CLEANUP_INFO *) realloc (eh_cond_cleanup_stack,
					eh_cond_cleanup_max * 
					sizeof (EH_COND_CLEANUP_INFO));
			
  }

  eh_cond_cleanup_stack [eh_cond_cleanup_i].id = next_cond_cleanup_id++;
  eh_cond_cleanup_stack [eh_cond_cleanup_i].region_block = uncond_region;
  eh_cond_cleanup_stack [eh_cond_cleanup_i].region_after = uncond_after;
}

void
Pop_Conditional_Cleanup_Level (void)
{
  FmtAssert(eh_cond_cleanup_i >= 0,
	    ("Can't pop empty eh_cond_cleanup stack"));
  --eh_cond_cleanup_i;
}

static void
Init_Conditional_Cleanup_Guard (TEMP_CLEANUP_INFO *tcu)
{
  /* If 'tcu' is not a cleanup, or if we aren't currently in a
     conditional context (indicated by 'region_block' != NULL), then
     initialize to NULL to indicate that there is no guarding
     symbol. */
  if (!tcu->is_cleanup ||
      (eh_cond_cleanup_i == -1) ||
      eh_cond_cleanup_stack[eh_cond_cleanup_i].region_block)
  {
    tcu->st = NULL;
    tcu->guard_init_block = NULL;
    tcu->guard_init_after = NULL;
    return;
  }

  /* Search back through the cond cleanup stack to find an entry
     indicating the start of an unconditional region. */
  int scan;
  for (scan = eh_cond_cleanup_i; scan != -1; scan--)
  {
    if (eh_cond_cleanup_stack[scan].region_block)
      break;
  }

  FmtAssert(scan != -1, ("can't find unconditional EH region"));

  /* 'tcu' is a cleanup, and we are in a conditional context, so
     collect the information we need to conditionalize this
     cleanup. */
  tcu->st = Gen_Temp_Symbol (MTYPE_To_TY(EH_COND_GUARD_MTYPE), "__eh_cond");
  tcu->guard_init_block = eh_cond_cleanup_stack[scan].region_block;
  tcu->guard_init_after = eh_cond_cleanup_stack[scan].region_after;
}


static void
Start_Guarded_Cleanup (ST *st)
{
  WN *block = WN_CreateBlock();

  WN *ldid = WN_Ldid(EH_COND_GUARD_MTYPE, 0, st,
		     MTYPE_To_TY(EH_COND_GUARD_MTYPE));
  WN *test = WN_Relational(OPR_NE, EH_COND_GUARD_MTYPE,
			   ldid, WN_Intconst(EH_COND_GUARD_MTYPE, 0));
  WN *if_stmt = WN_CreateIf(test, block, WN_CreateBlock());
  WFE_Stmt_Append(if_stmt, Get_Srcpos());

  WFE_Stmt_Push(block, wfe_stmk_eh_cond, Get_Srcpos());
}

static void
Finish_Guarded_Cleanup (void)
{
  WFE_Stmt_Pop(wfe_stmk_eh_cond);
}

  
static void
Emit_Cleanup (tree cleanup, ST *st, WN *cond_init_block, WN *cond_init_after)
{
  /* If this cleanup should only take place for some conditional
     context, then guard the cleanup with 'st'. */
  if (st)
  {
    /* Initialize the guard condition to 0 at the start of the
       statement. See "Push_Temp_Cleanup" for the insertion of the
       instruction that sets the guard to 1. */
    Is_True(cond_init_block, ("no block to initialize cond cleanup guards"));
    WN *init = WN_Stid(EH_COND_GUARD_MTYPE, 0, st,
		       MTYPE_To_TY(EH_COND_GUARD_MTYPE),
		       WN_Intconst(EH_COND_GUARD_MTYPE, 0));
    WN_INSERT_BlockAfter(cond_init_block, cond_init_after, init);
    
    Start_Guarded_Cleanup(st);
  }
  
  WFE_One_Stmt (cleanup);

  if (st)
    Finish_Guarded_Cleanup();
}
    

/* If 'wn' is a direct call to a destructor, return TRUE, and set
   'call' to point to the OPC_VCALL, and 'obj' to point to the OPR_LDA
   of the object being destructed. Otherwise return FALSE. */
static BOOL
Destructor_Call (WN *wn, WN **call, WN **obj)
{
  if (!wn || (WN_opcode(wn) != OPC_VCALL))
    return FALSE;

  if ((WN_kid0(wn) == NULL) || (WN_operator(WN_kid0(wn)) != OPR_PARM))
    return FALSE;

  *call = wn;
  *obj = WN_kid0(WN_kid0(*call));

  /* If the first parameter to the call is the load of a symbol,
     search back to find the assignment to the symbol. */
  if (WN_operator(*obj) == OPR_LDID)
  {
    ST *st = WN_st(*obj);
    *obj = NULL;

    WN *prev = WN_prev(*call);
    while (prev)
    {
      if ((WN_operator(prev) == OPR_STID) &&
	  (WN_st(prev) == st) &&
	  (WN_operator(WN_kid0(prev)) == OPR_LDA))
      {
	*obj = WN_kid0(prev);
	break;
      }
      
      prev = WN_prev(prev);
    }
  }

  /* If 'obj' is an OPR_LDA, assume it is the object being destructed,
     otherwise we didn't find the object. */
  if (*obj && (WN_operator(*obj) != OPR_LDA))
    *obj = NULL;
  
  return TRUE;
}


static WN *
Start_Cleanup_Region (WFE_STMT_KIND kind)
{
  WN *body = WN_CreateBlock();
  WFE_Stmt_Push(body, kind, Get_Srcpos());
  return body;
}


static WN *
Finish_Cleanup_Region (tree cu_expr, ST *cu_guard, WN *body,
		       WFE_STMT_KIND kind, LABEL_IDX cleanup_label_idx)
{
  WFE_Stmt_Pop(kind);

  /* If there is nothing in the 'body' of the region, or there is no
     cleanup, then don't generate an eh region. */
  if (!WN_first(body) || !cu_expr)
    return body;

  WN *region = WN_CreateRegion(REGION_KIND_CLEANUP,
			       body,
			       WN_CreateBlock(),
			       WN_CreateBlock(),
			       -1,
			       NULL);

  /* The cleanup expression (call to destructor or general constructor
     code) goes in the region's pragmas (but see below for changes to
     pragmas for general destructor code). */
  WN *pragmas = WN_region_pragmas(region);
  WFE_Stmt_Push(pragmas, wfe_stmk_region_pragmas, Get_Srcpos());
  if (cu_guard)
    Start_Guarded_Cleanup(cu_guard);
  WFE_One_Stmt (cu_expr);
  if (cu_guard)
    Finish_Guarded_Cleanup();
  WFE_Stmt_Pop(wfe_stmk_region_pragmas);

  /* Usually 'region' should be a cleanup region, but sometimes we
     want to generate a catch region instead. If the cleanup
     expression is a call to "__cp_pop_exception", then this region
     spans a catch block, and we want to use a catch region. Cleanups
     have two general forms:
	   
     1. A call to a destructor. We recognize these by the first
     parameter being an LDA of a this pointer.

     2. General set of statements performing the cleanup (e.g. code to
     destruct an array, or a guarded cleanup).  */
  WN *call = WN_first(pragmas);
  FmtAssert(call, ("expecting cleanup statement."));
  if ((WN_opcode(call) == OPC_VCALL) &&
      !strcmp(ST_name(WN_st(call)), "__cp_pop_exception"))
  {
    /* Catch region... represented with REGION_KIND_CLEANUP, but
       the ereg_supp information is different. */
    WN_set_region_kind(region, REGION_KIND_CLEANUP);

    /* There should be one LDID parameter loading the
       "exception_info". We put this in the INITO describing the
       catch, but remove it from the call, since it will confuse
       "be". */
    FmtAssert((WN_kid_count(call) == 1) &&
	      (WN_operator(WN_kid0(call)) == OPR_PARM) &&
	      (WN_operator(WN_kid0(WN_kid0(call))) == OPR_LDID),
	      ("expecting call to __cp_pop_exception to have one parameter"));

    /* We must mark 'exception_info' as having a nested reference to
       ensure that it is assigned to the stack (and not to a
       register), because the runtime assumes it can find it on the
       stack. */
    ST_IDX exception_info = WN_st_idx(WN_kid0(WN_kid0(call)));
    Set_ST_has_nested_ref(ST_ptr(exception_info));
    
    WN_DELETE_Tree(WN_kid0(call));
    WN_kid(call, 0) = NULL;
    WN_set_kid_count(call, 0);
    
    /* Add the INITO describing the cleanup. */
    WN_ereg_supp(region) = Create_Cleanup_Region_Info(XT_EH_TYPE_CATCH,
						      exception_info,
						      ST_IDX_ZERO);
  }
  else if ((WN_opcode(call) == OPC_VCALL) &&
	   (WN_operator(WN_kid0(call)) == OPR_PARM) &&
	   (WN_operator(WN_kid0(WN_kid0(call))) == OPR_LDA))
  {
    /* Cleanup region... call to a destructor. */
    Is_True((WN_operator(WN_kid0(call)) == OPR_PARM) &&
	    (WN_operator(WN_kid0(WN_kid0(call))) == OPR_LDA),
	    ("expecting cleanup pragma call to contain destructor address"));

    /* Gcc sometimes passes a "in_charge" argument to the destructor
       in addition to the 'this' pointer. xcalibur is not expecting
       this in the pragma, and it seems to count on the fact that the
       operand to the call is just the 'this' pointer. Clean up the
       call here... it doesn't matter that we remove the other
       argument because this whirl is not used to actually generate
       code. */
    WN *new_call = WN_Call(MTYPE_V, MTYPE_V, 1, WN_st(call));
    WN_actual(new_call, 0) = WN_kid0(call);
    WN_call_flag(new_call) = WN_call_flag(call);
    WN_first(pragmas) = new_call;
    
    /* Add the INITO describing the cleanup. */
    WN_ereg_supp(region) = Create_Cleanup_Region_Info(XT_EH_TYPE_CLEANUP_CALL,
						      WN_st_idx(WN_kid0(WN_kid0(call))),
						      WN_st_idx(call));
  }
  else {
    /* Cleanup region... general destructor code. */

    /* Because of the way regions must be nested, we emit the cleanup
       immediately after the region; not at the end of the function
       like gcc. So we generate a label that we use to mark the
       "continue" point after the cleanup. If there is no exception,
       control-flow after the region jumps to that label to continue
       execution. We enclose the region and the cleanup in a block so
       that we can return that block as being all the eh region
       code. */
    WN *region_block = WN_CreateBlock();
    WFE_Stmt_Push(region_block, wfe_stmk_gen_cleanup, Get_Srcpos());
    WFE_Stmt_Append(region, Get_Srcpos());

    LABEL_IDX cont_label_idx;
    New_LABEL (CURRENT_SYMTAB, cont_label_idx);
    WFE_Stmt_Append(WN_CreateGoto((ST_IDX)NULL, cont_label_idx), Get_Srcpos());
  
    /* Move the cleanup code from the pragma to here. The runtime will
       do a non-local goto to the cleanup code, so we need to throw
       again when it completes. */
    if (cleanup_label_idx == LABEL_IDX_ZERO)
      New_LABEL (CURRENT_SYMTAB, cleanup_label_idx);
    
    WN *clean_label = WN_CreateLabel((ST_IDX)NULL, cleanup_label_idx, 0, NULL);
    WN_Set_Label_Is_Handler_Begin(clean_label);
    WFE_Stmt_Append(clean_label, Get_Srcpos());

    for (WN *stmt = WN_first(pragmas); stmt; stmt = WN_next(stmt))
      WFE_Stmt_Append(stmt, Get_Srcpos());
    
    Call_Throw();

    /* The pragma should GOTO the cleanup code. */
	  
    WN_first(pragmas) = WN_CreateGoto((ST_IDX)NULL, cleanup_label_idx);

    /* Emit "continue" label after cleanup. */

    WFE_Stmt_Append (WN_CreateLabel((ST_IDX) 0, cont_label_idx, 0, NULL), Get_Srcpos());

    /* Add the INITO describing the cleanup. */

    WN_ereg_supp(region) = Create_Try_Region_Info(XT_EH_TYPE_CLEANUP_GENERAL, NULL,
						  cleanup_label_idx, ST_IDX_ZERO);

    WFE_Stmt_Pop(wfe_stmk_gen_cleanup);
    region = region_block;
  }

  /* Return the region or block containing the cleanup eh region and
     any handlers. */
  Set_PU_has_exc_scopes(Get_Current_PU());

  return region;
}

static WN *
Finish_Skip_Region (WN *body, LABEL_IDX label_idx, WFE_STMT_KIND kind)
{
  WFE_Stmt_Pop(kind);

  /* If there is nothing in the 'body' of the region, then don't
     generate an eh region. */
  if (!WN_first(body))
    return body;

  WN *call, *obj;
  if (!Destructor_Call(WN_last(body), &call, &obj))
  {
    call = obj = NULL;
  }
  else
  {
    /* Ignore cleanup expressions that call "__cp_pop_exception",
       because those represent catch blocks and not actual object
       cleanups. */
    if ((WN_opcode(call) == OPC_VCALL) &&
	!strcmp(ST_name(WN_st(call)), "__cp_pop_exception"))
      return body;

    FmtAssert((WN_opcode(call) == OPC_VCALL) && obj && (WN_operator(obj) == OPR_LDA),
	      ("expecting skip cleanup pragma call to contain destructor address"));
  }
  
  WN *region = WN_CreateRegion(REGION_KIND_CLEANUP,
			       body,
			       WN_CreateBlock(),
			       WN_CreateBlock(),
			       -1,
			       NULL);

  /* pr4318: if 'call' is VCALL, then WN_st_idx(call) is the symbol
     representing the destructor and we encode that in the skip
     region. Otherwise, we don't know the destructor (it is being
     called indirectly or there is a loop of destructor calls, etc.),
     so instead we encode the handler label 'label_idx' within which
     the destructor call will be placed. */
  if (call && (WN_opcode(call) == OPC_VCALL))
  {
    WN_ereg_supp(region) =
      Create_Cleanup_Region_Info(XT_EH_TYPE_SKIP, WN_st_idx(obj), WN_st_idx(call));
  }
  else
  {
    WN_ereg_supp(region) = Create_Try_Region_Info(XT_EH_TYPE_SKIP_GENERAL, NULL,
						  label_idx, ST_IDX_ZERO);
  }
  
  Set_PU_has_exc_scopes(Get_Current_PU());
  return region;
}


static void
Do_Some_Temp_Cleanups (INT32 start_idx, INT32 end_idx)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "doing temp cleanups from %d - %d\n",
	  end_idx, start_idx);
#endif

  FmtAssert(!temp_cleanup_stack[end_idx].is_cleanup,
	    ("temp cleanup end tree is also cleanup"));

  /* If 'end_idx' represents a shared region, then record that region
     to be used for all cleanups that don't specify their own
     region. */
  WN *shared_cleanup_region = NULL;
  if (temp_cleanup_stack [end_idx].region &&
      !temp_cleanup_stack [end_idx].is_cleanup)
    shared_cleanup_region = temp_cleanup_stack[end_idx].region;
 
  /* Generate destructor call and eh region for each cleanup. We make
     two passes over the cleanups, on the first pass we finalize
     regions for cleanups that have their own regions. On the second
     pass, we finalize the shared region for each cleanup that is
     sharing it. */
  {
    /* Pass 1... */
    for (INT32 i = start_idx; i > end_idx; i--)
    {
      tree cleanup = temp_cleanup_stack [i].expr;
      ST *st = temp_cleanup_stack [i].st;
      WN *region = temp_cleanup_stack [i].region;

      FmtAssert(temp_cleanup_stack [i].is_cleanup,
		("Not expecting nested, non-cleanup temp cleanups."));
      
      if (region)
      {
	if (flag_exceptions)
	{
	  region = Finish_Cleanup_Region(cleanup, st, region,
					 wfe_stmk_temp_cleanup,
					 LABEL_IDX_ZERO);
	  WFE_Stmt_Append(region, Get_Srcpos());
	}

	Emit_Cleanup(cleanup,
		     st,
		     temp_cleanup_stack [i].guard_init_block,
		     temp_cleanup_stack [i].guard_init_after);
      }
    }
  }

  {
    /* Pass 2... */
    for (INT32 i = start_idx; i > end_idx; i--)
    {
      tree cleanup = temp_cleanup_stack [i].expr;
      ST *st = temp_cleanup_stack [i].st;
      WN *region = temp_cleanup_stack [i].region;

      if (!region)
      {
	if (flag_exceptions)
	{
	  FmtAssert(shared_cleanup_region,
		    ("cleanup expects shared cleanup region"));

	  WN *curr_cleanup_region = Finish_Cleanup_Region(cleanup, st,
							  shared_cleanup_region,
							  wfe_stmk_temp_cleanup,
							  LABEL_IDX_ZERO);
	  shared_cleanup_region = Start_Cleanup_Region(wfe_stmk_temp_cleanup);
	  WFE_Stmt_Append(curr_cleanup_region, Get_Srcpos());
	}
	
	Emit_Cleanup(cleanup,
		     st,
		     temp_cleanup_stack [i].guard_init_block,
		     temp_cleanup_stack [i].guard_init_after);
      }
    }

    /* If we have a shared cleanup region, then emit it... */
    if (shared_cleanup_region)
    {
      shared_cleanup_region = Finish_Cleanup_Region(NULL, NULL,
						    shared_cleanup_region,
						    wfe_stmk_temp_cleanup,
						    LABEL_IDX_ZERO);
      WFE_Stmt_Append(shared_cleanup_region, Get_Srcpos());
    }
  }
}


static void
Push_Scope_Cleanup (tree t)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "pushing scope cleanup %x, scope_cleanup_i = %d\n",
	  t, scope_cleanup_i + 1);
#endif

  // Don't push a cleanup without a scope
  if (scope_cleanup_i == -1 && TREE_CODE(t) == CLEANUP_STMT)
    return;

  if (++scope_cleanup_i == scope_cleanup_max) {
    scope_cleanup_max = ENLARGE (scope_cleanup_max);
    scope_cleanup_stack =
      (SCOPE_CLEANUP_INFO *) realloc (scope_cleanup_stack,
	 	        scope_cleanup_max * sizeof (SCOPE_CLEANUP_INFO));
  }

  scope_cleanup_stack [scope_cleanup_i].stmt = t;
  if (TREE_CODE(t) == CLEANUP_STMT)
    New_LABEL (CURRENT_SYMTAB, 
	       scope_cleanup_stack [scope_cleanup_i].label_idx);
  else
    scope_cleanup_stack [scope_cleanup_i].label_idx = 0;

  /* If 't' is a cleanup and exceptions are enabled, then we want to
     start a new region into which we want to emit all the statements
     between here and the end of the scope. */
  scope_cleanup_stack [scope_cleanup_i].region = NULL;
  if (flag_exceptions && (TREE_CODE(t) == CLEANUP_STMT))
  {
    WN *region = Start_Cleanup_Region(wfe_stmk_scope);
    scope_cleanup_stack [scope_cleanup_i].region = region;
  }
}

static void
Pop_Scope_And_Do_Cleanups (BOOL pop, BOOL cleanup)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "doing scope cleanups from %d\n", scope_cleanup_i);
#endif

  Is_True(scope_cleanup_i != -1,
	  ("Pop_Scope_And_Do_Cleanups: scope_cleanup_stack is empty"));

  while (true) {
    tree t = scope_cleanup_stack [scope_cleanup_i].stmt;

    /* If there are no cleanups to do in this scope, simply pop the
       scope and return. */
    if (TREE_CODE(t) != CLEANUP_STMT) {
      if (pop && (TREE_CODE(t) == SCOPE_STMT))
 	--scope_cleanup_i;
      break;
    }

    Is_True(scope_cleanup_i != -1,
	    ("Pop_Scope_And_Do_Cleanups: no scope_stmt on stack"));

    if (flag_exceptions)
    {
      /* End and emit the region representing this cleanup. */
      WN *region = scope_cleanup_stack [scope_cleanup_i].region;
      LABEL_IDX label_idx = scope_cleanup_stack [scope_cleanup_i].label_idx;
      if (region)
      {
	FmtAssert(label_idx != LABEL_IDX_ZERO,
		  ("expected handler label for cleanup region"));
	region = Finish_Cleanup_Region(CLEANUP_EXPR(t), NULL, region,
				       wfe_stmk_scope, label_idx);
	WFE_Stmt_Append(region, Get_Srcpos());
      }
    }

    /* Emit the cleanup expression at the end of the scope (i.e. the
       destructor). */
    if (cleanup)
      WFE_One_Stmt (CLEANUP_EXPR(t));
    
    --scope_cleanup_i;
  }
}   


static void
Do_All_Other_Scope_Cleanups (void)
{
  /* We want to call the cleanups for all scopes other than the
     current one. These cleanups must be wrapped in XT_EH_TYPE_SKIP or
     XT_EG_TYPE_SKIP_GENERAL regions so that if we take an exception,
     we skip containing cleanups that have already been done. */
  INT i = scope_cleanup_i;
  while ((i != -1) && (TREE_CODE(scope_cleanup_stack [i].stmt) != SCOPE_STMT))
    i--;

#ifdef WFE_DEBUG
  fprintf(stderr, "skipping from %d, doing all other scope cleanups from %d\n",
	  scope_cleanup_i, i);
#endif

  if (i == -1)
    return;

  i--;
  while (i != -1)
  {
    tree t = scope_cleanup_stack [i].stmt;
    if (TREE_CODE(t) == CLEANUP_STMT)
    {
      WN *region = NULL;
      if (flag_exceptions)
	region = Start_Cleanup_Region(wfe_stmk_scope);
      WFE_One_Stmt (CLEANUP_EXPR(t));
      if (region)
      {
	LABEL_IDX label_idx = scope_cleanup_stack [i].label_idx;
	FmtAssert(label_idx != LABEL_IDX_ZERO,
		  ("expected handler label for cleanup region"));
	region = Finish_Skip_Region(region, label_idx, wfe_stmk_scope);
	WFE_Stmt_Append(region, Get_Srcpos());
      }
    }

    i--;
  }
}


static UINT
Num_Scope_Cleanups (tree t)
{
  UINT cnt = 0;
  
  for (INT32 i = scope_cleanup_i;
       (i >= 0) && (scope_cleanup_stack[i].stmt != t);
       i--)
  {
    if (TREE_CODE(scope_cleanup_stack[i].stmt) == CLEANUP_STMT)
      cnt++;
  }

  return cnt;
}


static tree
Get_Current_Scope (INT *current_scope_idx = NULL)
{
  for (INT32 i = scope_cleanup_i; i >= 0; i--)
  {
    tree t = scope_cleanup_stack[i].stmt;
    if (TREE_CODE(t) == SCOPE_STMT)
    {
      if (current_scope_idx)
	*current_scope_idx = i;

      return t;
    }
  }

  return NULL;
}


static void
Push_Partials_Cleanup (tree t)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "pushing partials cleanup %x, partials_cleanup_i = %d\n",
	  t, partials_cleanup_i + 1);
#endif

  /* We must be in a CTOR region if we see a SUBOBJECT. */
  FmtAssert((partials_cleanup_i != -1) || (TREE_CODE(t) != SUBOBJECT),
	    ("unexpected SUBOBJECT outside of ctor region"));

  if (++partials_cleanup_i == partials_cleanup_max) {
    partials_cleanup_max = ENLARGE (partials_cleanup_max);
    partials_cleanup_stack =
      (PARTIALS_CLEANUP_INFO *) realloc (partials_cleanup_stack,
	 	        partials_cleanup_max * sizeof (PARTIALS_CLEANUP_INFO));
  }

  partials_cleanup_stack [partials_cleanup_i].stmt = t;
  partials_cleanup_stack [partials_cleanup_i].region = NULL;

  /* If 't' is a subobject and exceptions are enabled, then we want to
     start a new region into which we want to emit all the statements
     between here and the end of the partials. */
  if (flag_exceptions && (TREE_CODE(t) == SUBOBJECT))
  {
    WN *region = Start_Cleanup_Region(wfe_stmk_partials);
    partials_cleanup_stack [partials_cleanup_i].region = region;
  }
}

static void
Pop_Partials_And_Do_Cleanups (void)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "doing partials cleanups from %d\n", partials_cleanup_i);
#endif

  /* Apparently we can have CTOR end without a corresponding begin. */
  if (partials_cleanup_i == -1)
    return;

  while (true) {
    tree t = partials_cleanup_stack [partials_cleanup_i].stmt;

    /* If there are no cleanups to do in this partials, simply pop the
       partials and return. */
    if (TREE_CODE(t) != SUBOBJECT) {
      if (TREE_CODE(t) == CTOR_STMT)
 	--partials_cleanup_i;
      break;
    }

    Is_True(partials_cleanup_i != -1,
	    ("Pop_Partials_And_Do_Cleanups: no partials_stmt on stack"));

    if (flag_exceptions)
    {
      /* End and emit the region representing this cleanup. */
      WN *region = partials_cleanup_stack [partials_cleanup_i].region;
      if (region)
      {
	region = Finish_Cleanup_Region(SUBOBJECT_CLEANUP(t), NULL, region,
				       wfe_stmk_partials, LABEL_IDX_ZERO);
	WFE_Stmt_Append(region, Get_Srcpos());
      }
    }

    --partials_cleanup_i;
  }
}   


void
Push_Temp_Cleanup (tree t, bool start_cleanup_region, bool is_cleanup)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "pushing temp cleanup %d, start_region %d, is_cleanup %d\n",
	  temp_cleanup_i + 1, start_cleanup_region, is_cleanup);
#endif

  /* Record the cleanup... */
  if (++temp_cleanup_i == temp_cleanup_max)
  {
    temp_cleanup_max = ENLARGE (temp_cleanup_max);
    temp_cleanup_stack =
      (TEMP_CLEANUP_INFO *) realloc (temp_cleanup_stack,
				     temp_cleanup_max * 
				     sizeof (TEMP_CLEANUP_INFO));
			
  }

  temp_cleanup_stack [temp_cleanup_i].expr = t;
  temp_cleanup_stack [temp_cleanup_i].scope_depth = scope_depth;
  temp_cleanup_stack [temp_cleanup_i].is_cleanup = is_cleanup;
  temp_cleanup_stack [temp_cleanup_i].region = NULL;

  Init_Conditional_Cleanup_Guard(&temp_cleanup_stack [temp_cleanup_i]);

  /* We start a cleanup region if:

     1. 'start_cleanup_region' is true, or

     2. 'is_cleanup' is true and we are not in a conditional
     context. If we are in a conditional context, then the cleanup
     region for this cleanup will already have been started. */
  if (flag_exceptions &&
      (start_cleanup_region ||
       (is_cleanup && !temp_cleanup_stack [temp_cleanup_i].st)))
  {
    WN *region = Start_Cleanup_Region(wfe_stmk_temp_cleanup);
    temp_cleanup_stack [temp_cleanup_i].region = region;
  }

  /* If this cleanup should only take place for some conditional
     context, then initialize the guard symbol to 1, to indicate that
     the cleanup is needed. */
  if (temp_cleanup_stack [temp_cleanup_i].st)
  {
    WN *init = WN_Stid(EH_COND_GUARD_MTYPE, 0,
		       temp_cleanup_stack [temp_cleanup_i].st,
		       MTYPE_To_TY(EH_COND_GUARD_MTYPE),
		       WN_Intconst(EH_COND_GUARD_MTYPE, 1));
    WFE_Stmt_Append(init, Get_Srcpos());
  }
}

void
Do_Temp_Cleanups (tree t)
{
#ifdef WFE_DEBUG
  fprintf(stderr, "temp cleanups for tree 0x%x, temp_cleanup_i = %d\n",
	  t, temp_cleanup_i);
#endif
  
  Is_True(temp_cleanup_i != -1, ("Do_Temp_Cleanups: stack empty"));

  /* We want to do all the temp cleanups in the stack back to 't'. We
     do them in chunks delineated by cleanup entries representing
     shared regions. */
  INT32 i, start_idx = temp_cleanup_i;
  for (i = temp_cleanup_i;
       (i >= 0) && (temp_cleanup_stack[i].expr != t);
       i--)
  {
    if (temp_cleanup_stack [i].region &&
	!temp_cleanup_stack [i].is_cleanup)
    {
      Do_Some_Temp_Cleanups(start_idx, i);
      start_idx = i - 1;
    }
  }

  FmtAssert(i >= 0, ("can't find temp cleanup end tree"));
  Do_Some_Temp_Cleanups(start_idx, i);

  temp_cleanup_i = i - 1;
}


static UINT
Num_Temp_Cleanups (tree t)
{
  UINT cnt = 0;
  
  for (INT32 i = temp_cleanup_i;
       (i >= 0) && (temp_cleanup_stack[i].expr != t);
       i--)
  {
    if (temp_cleanup_stack [i].is_cleanup)
      cnt++;
  }

  return cnt;
}


static void
WFE_Record_Loop_Switch (int tree_code)
{
  INT32 i;
  Is_True(tree_code == DO_STMT    ||
	  tree_code == FOR_STMT   ||
 	  tree_code == WHILE_STMT ||
          tree_code == SWITCH_STMT,
	  ("WFE_Record_Loop_Switch: unexpected tree_code"));

  if (++break_continue_info_i == break_continue_info_max) {
    break_continue_info_max = ENLARGE (break_continue_info_max);
    break_continue_info_stack =
      (BREAK_CONTINUE_INFO *) realloc (break_continue_info_stack,
				       break_continue_info_max *
					 sizeof (BREAK_CONTINUE_INFO));
  }

  break_continue_info_stack 
    [break_continue_info_i].tree_code          = tree_code;
  break_continue_info_stack 
    [break_continue_info_i].break_label_idx    = 0;
  break_continue_info_stack 
    [break_continue_info_i].continue_label_idx = 0;
  if (scope_cleanup_i == -1)
    break_continue_info_stack
      [break_continue_info_i].scope = NULL_TREE;
  else {
    for (i = scope_cleanup_i;
	 TREE_CODE(scope_cleanup_stack[i].stmt) == CLEANUP_STMT;
	 --i);
    Is_True (i != -1 && 
	     (TREE_CODE(scope_cleanup_stack[i].stmt) == SCOPE_STMT ||
	      TREE_CODE(scope_cleanup_stack[i].stmt) == TRY_BLOCK),
	    ("WFE_Record_Loop_Switch: no scope_stmt on stack"));
    break_continue_info_stack
      [break_continue_info_i].scope = scope_cleanup_stack[i].stmt;
  }
} /* WFE_Record_Loop_Switch */

static void
WFE_Expand_Case (tree low, tree high)
{
  WN        *wn;
  WN        *lower_bound;
  WN        *upper_bound;
  LABEL_IDX  case_label_idx;

  /* If 'switch_info_i' == -1, then we are not within switch. */
  if (switch_info_i == -1)
  {
    error ("case or default label not within switch statement");
    return;
  }

  if (low)
    low = check_cp_case_value (low);
  if (high)
    high = check_cp_case_value (high);
  
  if (high != NULL_TREE)
    DevWarn("encountered case range");

  if (low == NULL_TREE) {
    if (switch_info_stack [switch_info_i].default_label_idx != 0)
      error ("duplicate default label");
    else {
      New_LABEL (CURRENT_SYMTAB, case_label_idx);
      switch_info_stack [switch_info_i].default_label_idx = case_label_idx;
    }
  }

  else {
    if (TREE_CODE(low) == VAR_DECL)
      low = DECL_INITIAL(low);
    if (high != NULL_TREE && TREE_CODE(high) == VAR_DECL)
      high = DECL_INITIAL(high);
    lower_bound = WFE_Expand_Expr (low);
    upper_bound = (high == NULL_TREE) ? lower_bound
				      : WFE_Expand_Expr(high);
    if (++case_info_i == case_info_max) {
      case_info_max   = ENLARGE(case_info_max);
      case_info_stack = (CASE_INFO *) realloc (case_info_stack,
                                               case_info_max * sizeof (CASE_INFO));
    }

    case_info_stack 
      [case_info_i].case_lower_bound_value = 
        (low  == NULL_TREE) ? 0 : WN_const_val (lower_bound);
    case_info_stack 
      [case_info_i].case_upper_bound_value = 
        (high == NULL_TREE) ? WN_const_val (lower_bound) 
			    : WN_const_val (upper_bound);
    for (int i = switch_info_stack [switch_info_i].start_case_index;
         i < case_info_i; ++i) 
      if (WN_const_val(lower_bound) == 
          case_info_stack [i].case_lower_bound_value)
  	error ("duplicate case");
    New_LABEL (CURRENT_SYMTAB, case_label_idx);
    case_info_stack [case_info_i].case_label_idx = case_label_idx;
  }

  wn = WN_CreateLabel ((ST_IDX) 0, case_label_idx, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos ());
} /* WFE_Expand_Case */


static void
WFE_Expand_Cleanup (tree stmt)
{
  /* If we have exceptions, then we might have started at temp-cleanup
     region at the start of the statement that will incorrectly
     overlap this scope cleanup.

     stmt start   ---
                   |
		   |
		   |
		   |Temp cleanup region
		   |
		   |                       --- cleanup_stmt
		   |                        |
     stmt end     ---		            |
                                            |
                                            |
                                            |
                                            |
                                            |
                                           ___ end of scope

     To handle this we finish the temp cleanup region, and then
     restart it after the cleanup_stmt region is started. This keeps
     things correctly nested. We only have to do this if the temp
     cleanup region started in the same scope as 'stmt'. Otherwise we
     assume that the scope containing 'stmt' will be closed before the
     end of the temp cleanup region. */
  tree restart_stmt = NULL;
  if (flag_exceptions && (WFE_Stmt_Top_Kind() == wfe_stmk_temp_cleanup))
  {
    /* If there are actual cleanups at the top of the temp_cleanup
       stack, then don't do anything. We're assuming that either this
       won't happen, or when it does the scope will end before the
       temp cleanup region so that things will nest properly (we will
       fail later if that is not the case). */
    if ((temp_cleanup_stack[temp_cleanup_i].scope_depth == scope_depth) &&
	temp_cleanup_stack[temp_cleanup_i].region &&
	!temp_cleanup_stack[temp_cleanup_i].is_cleanup)
    {
#ifdef WFE_DEBUG
      fprintf(stderr, "restarting temp cleanup region for tree 0x%x\n",
	      temp_cleanup_stack[temp_cleanup_i].expr);
#endif
      
      restart_stmt = temp_cleanup_stack[temp_cleanup_i].expr;
      Do_Temp_Cleanups(restart_stmt);
    }
  }

  Push_Scope_Cleanup (stmt);

  if (restart_stmt)
    Push_Temp_Cleanup(restart_stmt, true, false);
}


static void
WFE_Declare_Nonlocal_Label (tree label)
{
  WFE_Get_LABEL (label, FALSE);
} /* WFE_Expand_Label */


/* Generate WHIRL for an asm statement with arguments.
   For now, we don't do all the stuff done by expand_asm_operands;
   instead, we leave much of that stuff until asm lowering time.
   Here, we just build the OPR_ASM node that records the relevant
   information about the asm statement. */

extern ST* WFE_Dedicated_Preg_ST(int preg_num); // in wfe_misc.cxx

static WN *
idname_from_regnum (int gcc_reg)
{
  PREG_NUM preg = gcc_reg;
  if ((preg < 0) || !Preg_Is_Dedicated(preg)) {
	DevWarn("unrecognized register name in asm");
  	return NULL;
  }
  else {
	ST *st;
	if (Preg_Is_Dedicated_Integer(preg))
		st = Int_Preg;
	else if (Preg_Is_Dedicated_Float(preg))
		st = Float_Preg;
	else
		st = WFE_Dedicated_Preg_ST(gcc_reg);

	if (st==NULL)
	  return NULL;
  	return WN_CreateIdname((WN_OFFSET) preg, st);
  }
}

char *
remove_plus_modifier(char *s)
{
#define MAX_NON_PLUS_CONSTRAINT_CHARS 7
  static char out[MAX_NON_PLUS_CONSTRAINT_CHARS + 1];
  int i = 0;
  while (i <= MAX_NON_PLUS_CONSTRAINT_CHARS)
    {
      while (*s == '+')
	{
	  ++s;
	}
      out[i++] = *s;
      if (*s == '\0')
	{
	  return out;
	}
      else
	{
	  ++s;
	}
    }
  Fail_FmtAssertion("Constraint string too long");
  /*NOTREACHED*/
}

BOOL
constraint_supported (const char *s)
{
  while (*s != 0) {
  if (*s != 'a' &&  /* AR */
	*s != 'f' &&    /* floating pt */
	*s != 'b' &&    /* boolean */
	*s != 'r' &&    /* same as 'a' */
	*s != 't' &&    /* state */
	*s != 'v' &&    /* TIE register file */
	*s != 'i' &&	/* immediate */
	*s != 'n' &&	/* immediate */
	*s != '+' &&
	*s != ',' &&
	*s != '=' &&
        *s != '&' &&
        *s != '%' &&
	(*s < '0' ||
	 *s > '9')) {
      return FALSE;
    }
    if (*s == 'f' && !xt_hard_float) {
      return FALSE;
    }
    ++s;
  }
  return TRUE;
}

ST *
st_of_new_temp_for_expr(const WN *expr)
{
  static unsigned int temp_count = 0;

  static char temp_name[64];

  sprintf(temp_name, "asm.by.address.temp_%u\0", temp_count++);

  ST *retval = New_ST(CURRENT_SYMTAB);
  
  ST_Init (retval,
	   Save_Str (temp_name),
	   CLASS_VAR,
	   SCLASS_AUTO,
	   EXPORT_LOCAL,
	   MTYPE_To_TY(WN_rtype(expr)));
  return retval;
}

// need to keep track of what kind of constraint a numeric constraint
// refers to (by address or not).  So keep list of constraints.

static char *operand_constraint_array[MAX_RECOG_OPERANDS];

static BOOL
constraint_by_address (const char *s)
{
  if (strchr (s, 'm')) {
    return TRUE;
  }
  else if (isdigit(*s)) {
    return constraint_by_address (operand_constraint_array[*s - '0']);
  }
  else {
    return FALSE;
  }
}

static WN *
add_offset(WN_OFFSET  ofst,
	   WN        *address)	// not const; some simplification may occur.
{
  return WN_Binary(OPR_ADD, Pointer_Mtype,
		   WN_Intconst(MTYPE_I8, ofst),
		   address);
}

static WN *
address_of (const WN *wn)
{
  if (WN_operator(wn) == OPR_ILOAD ||
      WN_operator(wn) == OPR_MLOAD) {
    return add_offset(WN_offset(wn), WN_kid0(wn));
  }
  else if ((WN_operator(wn) == OPR_LDID) &&
	   (ST_sclass(WN_st(wn)) != SCLASS_REG)) {
    return WN_Lda (Pointer_Mtype,
		   WN_offset(wn),
		   WN_st(wn),
		   (UINT) 0);
  }
  // No address for this object. This expression is not an lvalue.
  return NULL;
}

/* What OPR_ASM looks like:
 *
 *   Kids: 0 is a block of IDNAMEs referring to
 *         registers that get clobbered. Clobbering of memory and
 *         condition codes is encoded in WN_Asm_Clobbers_Cc() and
 *         WN_Asm_Clobbers_Mem().
 *       1 is a block of PRAGMA or XPRAGMA nodes giving information
 *         about copy-out output operands and their constraints.
 *       2 .. WN_kid_count() - 1 are OPR_ASM_INPUT nodes, each of
 *         which gives a constraint and an rvalue for the
 *         corresponding input to the asm statement.
 *
 * Inputs originate either as input operands to the ASM, or as output
 * operands that are passed by address.
 */

void
Wfe_Expand_Asm_Operands (tree  string,
			 tree  outputs,
			 tree  inputs,
			 tree  clobbers,
			 int   vol,
			 char *filename,
			 int   line)
{
  // filename and line are ignored for now; eventually maybe they
  // should be used to generate SRCPOS information on the OPR_ASM_STMT
  // WN.
  //
  // I don't know yet why filename and line are passed for
  // expand_asm_operands but not for other expand_* routines in
  // gnu/stmt.c.

  int ninputs = list_length (inputs);

  tree tail;
  char *constraint_string;

  // Keep list of output operand constraints so that we know
  // what a numeric constraint refers to.
  int i = 0;
  // Store the constraint strings
  for (tail = outputs; tail; tail = TREE_CHAIN (tail)) {
    constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));
    operand_constraint_array[i] = constraint_string;
    ++i;
  }
  FmtAssert(i < MAX_RECOG_OPERANDS, ("Too many asm operands"));
  for ( ; i < MAX_RECOG_OPERANDS; ++i) {
    operand_constraint_array[i] = NULL;
  }
  
  // Each occurrence of the "+" constraint modifier is converted to a
  // numeric matching constraint on a new input. In the following
  // loop, we count the number of "+" constraint modifiers so we know
  // how many inputs there will be.
  //
  // Also for the time being we discard the entire ASM construct if
  // there is a constraint we don't recognize. This is so we can
  // test-compile code containing ASM statements that apply to targets
  // we don't support. At the moment, we support only "r", "f", and
  // "m" constraints for IA64, so those are the only ones on which we
  // don't barf. Ideally we would check with some target-specific
  // routine to see which constraints are valid, but we don't want to
  // link gccfe with targ_info or other similar stuff for now.
  for (tail = outputs;
       tail;
       tail = TREE_CHAIN (tail))
    {
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));

      if (strchr (constraint_string, '+') ||
	  constraint_by_address (constraint_string))
	{
	  ++ninputs;
	}
      if (!constraint_supported (constraint_string)) {
	error ("Unrecognized asm statement constraint %s",
	      constraint_string);
	return;
      }
    }

  WN *asm_wn = WN_CreateAsm_Stmt (ninputs + 2,
				  TREE_STRING_POINTER (string));

  WN *clobber_block = WN_CreateBlock ();

  WN_kid0(asm_wn) = clobber_block;

  for (tail = clobbers; tail; tail = TREE_CHAIN (tail))
    {
      char *clobber_string =
	TREE_STRING_POINTER (TREE_VALUE (tail));

      WN *clobber_pragma = NULL;

      int gcc_reg = decode_reg_name(clobber_string);
      if (gcc_reg == -3)
	WN_Set_Asm_Clobbers_Cc(asm_wn);
      else if (gcc_reg == -4)
	WN_Set_Asm_Clobbers_Mem(asm_wn);
      else {
	WN *clobbered_idname = idname_from_regnum (gcc_reg);

      	if (clobbered_idname) {
	  // This is a clobbered register that can be expressed as a
	  // WHIRL dedicated PREG.

	  ST *clobber_st = New_ST(CURRENT_SYMTAB);
	  ST_Init(clobber_st,
		Str_To_Index (Save_Str (clobber_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	  clobber_pragma = WN_CreateXpragma (WN_PRAGMA_ASM_CLOBBER,
			    ST_st_idx(clobber_st),
			    1);
	  WN_kid0 (clobber_pragma) = clobbered_idname;
      	}
      	else {
	  // This is a clobbered register that cannot be expressed as a
	  // WHIRL dedicated PREG. Make the "asm" volatile because it
	  // clobbers something WHIRL can't see.

	  ST *clobber_st = New_ST(CURRENT_SYMTAB);
	  ST_Init(clobber_st,
		Str_To_Index (Save_Str (clobber_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	  clobber_pragma = WN_CreatePragma (WN_PRAGMA_ASM_CLOBBER,
			   ST_st_idx(clobber_st),
			   (INT32) 0,
			   (INT32) 0);

	  WN_Set_Asm_Volatile (asm_wn);
        }
      }

      if (clobber_pragma != NULL)
      	WN_INSERT_BlockAfter (clobber_block,
			    WN_last (clobber_block),
			    clobber_pragma);
    }

  WN *output_constraint_block = WN_CreateBlock ();

  WN_kid1(asm_wn) = output_constraint_block;

  i = 2;

  // Expand the by-address output operands before appending the
  // ASM_STMT node so side effects of these operands occur in the
  // right place.
  UINT32 opnd_num = 0;

  for (tail = outputs;
       tail;
       tail = TREE_CHAIN (tail))
    {
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));

      if (constraint_by_address(constraint_string)) {
	// This operand is by address, and gets represented as an
	// ASM_INPUT even though the user told us it's an output.
	WN *lhs_rvalue = WFE_Expand_Expr(TREE_OPERAND(tail, 0));
	WN *addr_of_lvalue = address_of(lhs_rvalue);
	FmtAssert(addr_of_lvalue != NULL,
		  ("WFE_Expand_Asm_Operands: output operand must be lvalue"));
	WN_kid (asm_wn, i) =
	  WN_CreateAsm_Input (constraint_string, opnd_num, addr_of_lvalue);
	++i;
      }
      ++opnd_num;
    }

  for (tail = inputs;
       tail;
       tail = TREE_CHAIN (tail))
    {
      if (TREE_PURPOSE (tail) == NULL_TREE)
	{
	  Fail_FmtAssertion ("hard register `%s' listed as "
			     "input operand to `asm'",
			     TREE_STRING_POINTER (TREE_VALUE (tail)) );
	  return;
	}

      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));

      if (!constraint_supported (constraint_string)) {
	error ("Unrecognized asm statement constraint %s",
		constraint_string);

	return;
      }

      WN *input_rvalue = WFE_Expand_Expr (TREE_VALUE (tail));

      if (strchr(constraint_string,'i') &&
	  WN_operator(input_rvalue) != OPR_INTCONST) {
	error("integer immediate value expected for constraint `i' in an `asm'");
      }

      if (constraint_by_address(constraint_string)) {
	WN *addr_of_rvalue = address_of(input_rvalue);
	if (addr_of_rvalue != NULL) {
	  // Pass the address of the input rvalue, because the
	  // constraint says we pass the operand by its address.
	  input_rvalue = addr_of_rvalue;
	}
	else {
	  // Create a temporary to hold the value of the expression,
	  // and pass the address of that temporary.
	  ST *temp_st = st_of_new_temp_for_expr(input_rvalue);
	  WN *store_wn = WN_Stid(WN_rtype(input_rvalue),
				 (WN_OFFSET) 0,
				 temp_st,
				 // We may want to get high-level type
				 // of the RHS in the cases where that
				 // information exists, but for now,
				 // just put the low-level type on the
				 // store.
				 MTYPE_To_TY(WN_rtype(input_rvalue)),
				 input_rvalue);
	  WFE_Stmt_Append (store_wn, Get_Srcpos ());
	  input_rvalue = WN_Lda (Pointer_Mtype,
				 (WN_OFFSET) 0,
				 temp_st,
				 (UINT) 0);
	}
      }

      WN_kid (asm_wn, i) =
	WN_CreateAsm_Input (constraint_string, opnd_num, input_rvalue);
      ++i;
      ++opnd_num;
    }

  // Is Get_Srcpos the right thing to use?
  WFE_Stmt_Append (asm_wn, Get_Srcpos ());

  // Side effects of copy-out operands occur after the asm. Kind of
  // weird, but that's what GCC does.
  opnd_num = 0;
  for (tail = outputs;
       tail;
       tail = TREE_CHAIN (tail), ++opnd_num)
    {
      constraint_string = TREE_STRING_POINTER (TREE_PURPOSE (tail));

      if (!constraint_by_address(constraint_string)) {
	// This operand is copy-in/copy-out.

	BOOL plus_modifier = (strchr (constraint_string, '+') != NULL);

	char input_opnd_constraint[8];

	if (plus_modifier)
	  {
	    // de-plus the output operand's constraint string.
	    constraint_string = remove_plus_modifier(constraint_string);

	    // Make up a numeric matching constraint string for the
	    // input operand we're going to add.
	    sprintf(input_opnd_constraint, "%d", opnd_num);
	  }

	WN *output_rvalue_wn = WFE_Lhs_Of_Modify_Expr (MODIFY_EXPR,
						       TREE_VALUE (tail),
						       plus_modifier,
						       (TY_IDX) 0, // component type
						       (INT64) 0,  // component offset
						       (UINT32) 0, // field ID
						       FALSE,      // is bit field?
						       NULL,       // dummy rhs kid
						       asm_neg_preg, // preg num
						       FALSE,      // is realpart
						       FALSE);     // is imagpart

	if (plus_modifier)
	  {
	    WN_kid (asm_wn, i) =
	      WN_CreateAsm_Input (input_opnd_constraint,
				  opnd_num,
				  output_rvalue_wn);
	    ++i;
	  }

	// Compute the ST used as the base for the negative PREG
	// reference in the output operand. This duplicates work done in
	// WFE_Lhs_Of_Modify_Expr.
	TYPE_ID desc = TY_mtype (Get_TY (TREE_TYPE (TREE_VALUE (tail))));
	ST *preg_st = MTYPE_To_PREG(desc);

	ST *constraint_st = New_ST(CURRENT_SYMTAB);
	ST_Init(constraint_st,
		Str_To_Index (Save_Str (constraint_string),
			      Current_Strtab),
		CLASS_NAME,
		SCLASS_UNKNOWN,
		EXPORT_LOCAL,
		(TY_IDX) 0);

	WN *constraint_pragma =
	  WN_CreatePragma (WN_PRAGMA_ASM_CONSTRAINT,
			   (ST_IDX) ST_st_idx(preg_st),
			   (INT32) ST_st_idx(constraint_st),
			   asm_neg_preg,
			   opnd_num);

	WN_INSERT_BlockAfter (output_constraint_block,
			      WN_last (output_constraint_block),
			      constraint_pragma);
	--asm_neg_preg;
      }
    }

  if (vol)
    {
      WN_Set_Asm_Volatile (asm_wn);
    }
}

LABEL_IDX
WFE_Get_LABEL (tree label, int def)
{
  LABEL_IDX label_idx =  DECL_LABEL_IDX(label);
  SYMTAB_IDX symtab_idx = DECL_SYMTAB_IDX(label);

  if (label_idx == 0) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    DECL_LABEL_IDX(label) = label_idx;
    DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;
    if (!def) {
      if (++undefined_labels_i == undefined_labels_max) {
        undefined_labels_max   = ENLARGE(undefined_labels_max);
        undefined_labels_stack =
          (LABEL_INFO *) realloc (undefined_labels_stack,
                                  undefined_labels_max * sizeof (LABEL_INFO));
      }
      undefined_labels_stack [undefined_labels_i].label_idx  = label_idx;
      undefined_labels_stack [undefined_labels_i].symtab_idx = CURRENT_SYMTAB;
      undefined_labels_stack [undefined_labels_i].defined    = FALSE;
    }
  }
  else {
    if (def) {
      for (int i = undefined_labels_i; i >= 0; --i) {
        if (undefined_labels_stack [i].label_idx  == label_idx &&
            undefined_labels_stack [i].symtab_idx == CURRENT_SYMTAB) {
          undefined_labels_stack [i].defined = TRUE;
          break;
        }
      }
    }
/*
    else {
      if (label->decl.label_defined)
        FmtAssert (label->decl.symtab_idx == CURRENT_SYMTAB,
                   ("jumping to a label not defined in current function"));
    }
*/
  }

  return label_idx;
} /* WFE_Get_LABEL */

void
WFE_Check_Undefined_Labels (void)
{
  INT32 i;
  for (i = undefined_labels_i; i >= 0; --i) {
    LABEL_IDX  label_idx  = undefined_labels_stack [undefined_labels_i].label_idx;
    SYMTAB_IDX symtab_idx = undefined_labels_stack [undefined_labels_i].symtab_idx;
//  fprintf (stderr, "WFE_Check_Undefined_Labels: %d idx = %8x [%d]\n", i, label_idx, symtab_idx);
    if (symtab_idx < CURRENT_SYMTAB)
      break;
    FmtAssert (undefined_labels_stack [undefined_labels_i].defined,
               ("label not defined within current function scope"));
  }
  undefined_labels_i = i;
} /* WFE_Check_Undefined_Labels */

WN *
WFE_Builtin_Return_Address(INT level)
{
  char asm_buf[400+50*level];
  char *asm_string = asm_buf;
  
  ST *preg_st = MTYPE_To_PREG(Pointer_Mtype);
  TY_IDX preg_ty = MTYPE_To_TY(Pointer_Mtype);

  PREG_NUM tmp1_preg = Create_Preg(Pointer_Mtype, "__ra_tmp1__");
  PREG_NUM tmp2_preg = Create_Preg(Pointer_Mtype, "__ra_tmp2__");

  asm_string += sprintf(asm_string, 
                        "#\n\t#BEGIN __builtin_return_address(%d)\n\t#\n\t",
                        level);

  if (level > 0) {
    asm_string += sprintf(asm_string, 
                          "call8   __xtensa_libgcc_window_spill\n\t");
  }
  
  for (INT i = 1; i <= level; i++) {
    asm_string += sprintf(asm_string, 
                          "addi    %%1, %s, %d \n\t",
                          (i == 1 ? "sp" : "%1"), 
                          (i == level ? -16 : -12));
    asm_string += sprintf(asm_string, 
                          "l32i    %s, %%1, 0 \n\t",
                          (i == level ? "%0" : "%1"));
  }
  
  sprintf(asm_string, 
          "mov     %%1, a0       \n\t" 
          "call0   0f            \n\t" 
          ".align  4             \n\t" 
          "0:                    \n\t" 
          "mov     %%2, a0       \n\t" 
          "mov     a0, %%1       \n\t" 
          "srli    %%2, %%2, 30  \n\t" 
          "slli    %%0, %s, 2    \n\t"
          "ssai    2             \n\t" 
          "src     %%0, %%2, %%0 \n\t"
          "#\n\t#END __builtin_return_address(%d)\n\t#",
          (level > 0 ? "%0" : "a0"),
          level);
  
  WN *asm_wn = WN_CreateAsm_Stmt(4, asm_buf);

  WN_Set_Asm_Volatile(asm_wn);

  WN *tmp1_preg_ldid = WN_Ldid(Pointer_Mtype, tmp1_preg, preg_st, preg_ty);
  WN *tmp2_preg_ldid = WN_Ldid(Pointer_Mtype, tmp2_preg, preg_st, preg_ty);

  WN_kid(asm_wn, 0) = WN_CreateBlock(); // empty block for clobber pragmas
  WN_kid(asm_wn, 1) = WN_CreateBlock(); // output constraints
  WN_kid(asm_wn, 2) = WN_CreateAsm_Input("r", 1, tmp1_preg_ldid);
  WN_kid(asm_wn, 3) = WN_CreateAsm_Input("r", 2, tmp2_preg_ldid);
      
  WFE_Stmt_Append(asm_wn, Get_Srcpos());

  ST *constraint_st = New_ST(CURRENT_SYMTAB);
  ST_Init(constraint_st, 
          Str_To_Index (Save_Str ("=&r"), Current_Strtab),
          CLASS_NAME, SCLASS_UNKNOWN, EXPORT_LOCAL, TY_IDX_ZERO);

  WN_INSERT_BlockLast(WN_kid1(asm_wn),
                      WN_CreatePragma(WN_PRAGMA_ASM_CONSTRAINT,
                                      ST_st_idx(preg_st),
                                      (INT32) ST_st_idx(constraint_st),
                                      asm_neg_preg,
                                      0));

  WN *asm_preg_ldid = WN_Ldid(Pointer_Mtype, asm_neg_preg, preg_st, preg_ty);
  
  PREG_NUM ret_val_preg = Create_Preg(Pointer_Mtype, "__ra_val__");

  WN *ret_val_stid = WN_Stid(Pointer_Mtype, ret_val_preg, 
                             preg_st, preg_ty, asm_preg_ldid);

  WFE_Stmt_Append(ret_val_stid, Get_Srcpos());

  --asm_neg_preg;

  return WN_Ldid(Pointer_Mtype, ret_val_preg, preg_st, preg_ty);
}

void
WFE_Stmt_Init (void)
{
  if_else_info_max   = 32;
  if_else_info_i     = -1;
  if_else_info_stack = 
    (BOOL *) malloc (sizeof (BOOL) * if_else_info_max);

  case_info_max      = 32;
  case_info_i        = -1;
  case_info_stack    = 
    (CASE_INFO *) malloc (sizeof (CASE_INFO) * case_info_max);

  switch_info_max    = 32;
  switch_info_i      = -1;
  switch_info_stack  = 
    (SWITCH_INFO *) malloc (sizeof (SWITCH_INFO) * switch_info_max);

  break_continue_info_max   = 32;
  break_continue_info_i     = -1;
  break_continue_info_stack = 
    (BREAK_CONTINUE_INFO *) malloc (sizeof (BREAK_CONTINUE_INFO) *
                                    break_continue_info_max);

  undefined_labels_max   = 32;
  undefined_labels_i     = -1;
  undefined_labels_stack = 
    (LABEL_INFO *) malloc (sizeof (LABEL_INFO) * undefined_labels_max);

  scope_cleanup_max      = 32;
  scope_cleanup_i  	 = -1;
  scope_cleanup_stack    =
    (SCOPE_CLEANUP_INFO *) malloc (sizeof (SCOPE_CLEANUP_INFO) * 
				   scope_cleanup_max);

  partials_cleanup_max   = 32;
  partials_cleanup_i  	 = -1;
  partials_cleanup_stack =
    (PARTIALS_CLEANUP_INFO *) malloc (sizeof (PARTIALS_CLEANUP_INFO) * 
				      partials_cleanup_max);

  scope_max    	         = 32;
  scope_i  	         = -1;
  scope_stack            =
    (tree *) malloc (sizeof (tree) * scope_max);

  temp_cleanup_max       = 32;
  temp_cleanup_i	 = -1;
  temp_cleanup_stack	 =
    (TEMP_CLEANUP_INFO *) malloc (sizeof (TEMP_CLEANUP_INFO) * 
				  temp_cleanup_max);

  eh_cond_cleanup_max    = 32;
  eh_cond_cleanup_i	 = -1;
  eh_cond_cleanup_stack	 =
    (EH_COND_CLEANUP_INFO *) malloc (sizeof (EH_COND_CLEANUP_INFO) * 
				     eh_cond_cleanup_max);

  handler_info_max	 = 32;
  handler_info_i	 = -1;
  handler_info_stack     =
    (HANDLER_INFO *) malloc (sizeof(HANDLER_INFO) * handler_info_max);

  eh_cleanup_max	 = 32;
  eh_cleanup_i		 = -1;
  eh_cleanup_stack	 =
    (EH_CLEANUP_INFO *) malloc (sizeof (EH_CLEANUP_INFO) * eh_cleanup_max);

  scope_number           = 0;
  scope_depth            = 0;
} /* WFE_Stmt_Init */

static void
Cleanup_To_Scope(tree scope)
{
  INT32 i = scope_cleanup_i;
  INT32 j = -1;
  Is_True(i != -1, ("Cleanup_To_Scope: scope_cleanup_stack empty"));
  while (scope_cleanup_stack [i].stmt != scope) {
    if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT)
      j = i;
    --i;
  }

  if (j != -1) {
    i = scope_cleanup_i;
    while (i != j) {
      if (TREE_CODE(scope_cleanup_stack [i].stmt) == CLEANUP_STMT)
        WFE_One_Stmt (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
    --i;
    }
  }
}
 
static void
WFE_Expand_Break (void)
{
  /* If 'break_continue_info_i' == -1, then we are not within a loop
     or switch. */
  if (break_continue_info_i == -1)
  {
    error ("break statement not within loop or switch");
    return;
  }
  
  INT32     i  	      = break_continue_info_i;
  LABEL_IDX label_idx = break_continue_info_stack[i].break_label_idx;
  tree      scope     = break_continue_info_stack[i].scope;
  WN *      wn;

  if (label_idx == 0) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    break_continue_info_stack [i].break_label_idx = label_idx;
  }

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);

  if (scope)
    Cleanup_To_Scope (scope);
   
  WFE_Stmt_Append (wn, Get_Srcpos());
}
/* WFE_Expand_Break */

static void
WFE_Expand_Continue (void)
{
  /* If 'break_continue_info_i' == -1, then we are not within a loop */
  if (break_continue_info_i == -1)
  {
    error ("continue statement not within loop");
    return;
  }
  
  INT32     i = break_continue_info_i;
  LABEL_IDX label_idx;
  tree      scope = break_continue_info_stack [i].scope;
  WN *      wn;
  
  /* find the enclosing loop. Error if we don't find one. */
  while ((i >= 0) && (break_continue_info_stack [i].tree_code == SWITCH_STMT))
    --i;
  
  if (i == -1)
  {
    error ("continue statement not within loop");
    return;
  }

  label_idx = break_continue_info_stack [i].continue_label_idx;
  if (label_idx == 0) {
    New_LABEL (CURRENT_SYMTAB, label_idx);
    break_continue_info_stack [i].continue_label_idx = label_idx;
  }

  if (scope)
    Cleanup_To_Scope (scope);

  wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Continue */


static WN *
WFE_Expand_Loop_Cond (tree cond, BOOL seq)
{
  WN *loop_test = NULL;
  if (TREE_CODE(cond) == TREE_LIST &&
      TREE_VALUE(cond) == NULL) {
    // handle non terminating loops
    tree stmt;
    WN   *cond_block;
    cond_block = WN_CreateBlock ();
    WFE_Stmt_Push (cond_block, wfe_stmk_while_cond, Get_Srcpos());
    for (stmt = TREE_PURPOSE(cond); stmt; stmt = TREE_CHAIN(stmt))
      WFE_Expand_Stmt (stmt);
    WFE_Stmt_Pop (wfe_stmk_while_cond);
    loop_test = WN_Intconst (Boolean_type, 1);
    if (WN_first (cond_block)) {
      loop_test = WN_CreateComma (OPR_COMMA, Boolean_type, MTYPE_V,
                                  cond_block, loop_test);
    }
    else
      WN_Delete (cond_block);
  }
  else if (seq)
  {
    Push_Conditional_Cleanup_Level();
    loop_test = WFE_Expand_Expr_With_Sequence_Point (cond, Boolean_type, NULL);
    Pop_Conditional_Cleanup_Level();
  }
  else
  {
    loop_test = WFE_Expand_Expr(cond, NULL);
  }

  return loop_test;
}


/* Called from calls_setjmp_p via walk_tree.  */

static tree
starts_eh_r (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == CLEANUP_STMT)
    return *tp;

  return NULL_TREE;
}

/* Return TRUE if EXP contains any expressions that start an eh
   region. */
static BOOL
WFE_Has_Eh_Region (tree exp)
{
  return(walk_tree(&exp, starts_eh_r, NULL) != NULL_TREE);
}


static void
WFE_Expand_Loop (tree stmt)
{
  tree cond, body, incr = NULL_TREE, init = NULL_TREE;
  tree loop_body_cond;
  
  WN * loop_stmt;
  WN * loop_test;
  WN * loop_block;
  WN * loop_body;
  
  switch (TREE_CODE(stmt)) {
    case WHILE_STMT:
      cond = WHILE_COND(stmt);
      body = WHILE_BODY(stmt);
      break;

    case DO_STMT:
      cond = DO_COND(stmt);
      body = DO_BODY(stmt);
      break;

    case FOR_STMT:
      incr = FOR_EXPR(stmt);
      cond = FOR_COND(stmt);
      body = FOR_BODY(stmt);
      for (init = FOR_INIT_STMT(stmt); init; init = TREE_CHAIN(init))
	WFE_Expand_Stmt(init);
      break;

    default:
      Is_True(FALSE, ("WFE_Expand_Loop: unexpected TREE_CODE"));
      break;
  }

  WFE_Record_Loop_Switch (TREE_CODE(stmt));

  /* If the 'cond'ition requires an eh region, we change the condition
     to "1", and instead conditionalize the body of the loop. We don't
     do this for DO_STMTs since that is semantically incorrect and
     unnecessary (I think). We must do this so that the eh region
     started for 'cond' nests correctly with the loop body. */
  if ((TREE_CODE(stmt) != DO_STMT) && WFE_Has_Eh_Region(cond))
  {
    loop_body_cond = cond;
    loop_test = WN_Intconst (Boolean_type, 1);
  }
  else
  {
    loop_body_cond = NULL;
    loop_test = WFE_Expand_Loop_Cond(cond, TRUE);
  }
  
  loop_body = WN_CreateBlock ();

  if (TREE_CODE(stmt) == WHILE_STMT ||
      TREE_CODE(stmt) == FOR_STMT)
    loop_stmt = WN_CreateWhileDo (loop_test, loop_body);
  else
    loop_stmt = WN_CreateDoWhile (loop_test, loop_body);

  WFE_Stmt_Append (loop_stmt, Get_Srcpos());

  if (body || loop_body_cond) {
    WFE_Stmt_Push (loop_body, wfe_stmk_while_body, Get_Srcpos());

    if (loop_body_cond) {
      /* We must expand the condition before the exit so that
         WFE_Expand_Break cleans up any objects constructed in the
         condition. */
      WN *if_test = WFE_Expand_Loop_Cond(loop_body_cond, FALSE);

      /* If test fails we exit the loop, cleaning up objects. */
      WN *loop_exit = WN_CreateBlock ();
      WFE_Stmt_Push (loop_exit, wfe_stmk_conditional_while_exit, Get_Srcpos());
      WFE_Expand_Break();
      WFE_Stmt_Pop (wfe_stmk_conditional_while_exit);

      WFE_Stmt_Append (WN_CreateIf(if_test, WN_CreateBlock(), loop_exit),
		       Get_Srcpos());
    }
    
    while (body) {
      WFE_Expand_Stmt (body);
      body = TREE_CHAIN(body);
    }

    if (break_continue_info_stack
	  [break_continue_info_i].continue_label_idx) {
      WFE_Stmt_Append (
	WN_CreateLabel ((ST_IDX) 0,
			break_continue_info_stack
			  [break_continue_info_i].continue_label_idx,
			0, NULL),
	Get_Srcpos());
    }

    if (incr)
      WFE_One_Stmt(incr);

    WFE_Stmt_Pop (wfe_stmk_while_body);
  }

  if (break_continue_info_stack [break_continue_info_i].break_label_idx) {
    WFE_Stmt_Append (
      WN_CreateLabel ((ST_IDX) 0,
		      break_continue_info_stack
			[break_continue_info_i].break_label_idx,
		      0, NULL),
      Get_Srcpos());
  }

  --break_continue_info_i;
} /* WFE_Expand_Loop */
  
static void
WFE_Expand_Goto (tree label)
{
  WN *wn;
  LABEL_IDX label_idx = WFE_Get_LABEL (label, FALSE);
  if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
      (DECL_SYMTAB_IDX(label) < CURRENT_SYMTAB))
    wn = WN_CreateGotoOuterBlock (label_idx, DECL_SYMTAB_IDX(label));
  else {
    tree scope = LABEL_SCOPE(label);
    if (scope != NULL_TREE && scope_cleanup_i != -1) {
      INT32 scope_number = SCOPE_NUMBER(scope);
      INT32 i = scope_cleanup_i;
      INT32 j = -1;
      while (i != -1) {
	if (TREE_CODE(scope_cleanup_stack [i].stmt) == SCOPE_STMT) {
	  if (SCOPE_NUMBER(scope_cleanup_stack [i].stmt) >= scope_number)
	    break;
	  j = i;
	} // xtensa: added this brace and the matching one...
	
        --i;
      }
      if (j != -1) {
        i = scope_cleanup_i;
	while (i != j) {
	  if (TREE_CODE(scope_cleanup_stack[i].stmt) == CLEANUP_STMT)
	    WFE_One_Stmt (CLEANUP_EXPR(scope_cleanup_stack [i].stmt));
	--i;
        }
      }
    }

    wn = WN_CreateGoto ((ST_IDX) NULL, label_idx);
  }

  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Goto */

static void
WFE_Expand_Computed_Goto (tree exp)
{
  DevWarn ("encountered indirect jump");
  WN *addr = WFE_Expand_Expr (exp);
  WN *wn   = WN_CreateAgoto (addr);
  WFE_Stmt_Append (wn, Get_Srcpos());
} /* WFE_Expand_Computed_Goto */

static void 
WFE_Expand_If (tree stmt)
{
  WN * if_stmt;
  WN * test;
  WN * then_block;
  WN * else_block;

  test = WFE_Expand_Expr (IF_COND(stmt), NULL);
  if (OPCODE_is_load(WN_opcode(test)) &&
      MTYPE_is_xtbool(WN_rtype(test))==false) {
    test = WN_Relational(OPR_NE, WN_rtype(test), test,
			   WN_Intconst(WN_rtype(test), 0));
  }

  then_block = WN_CreateBlock ();
  else_block = WN_CreateBlock ();
  if_stmt    = WN_CreateIf (test, then_block, else_block);
  WFE_Stmt_Append (if_stmt, Get_Srcpos ());
  if (THEN_CLAUSE(stmt)) {
    WFE_Stmt_Push (then_block, wfe_stmk_if_then, Get_Srcpos ());
    for (tree t = THEN_CLAUSE(stmt); t; t = TREE_CHAIN(t))
      WFE_Expand_Stmt (t);
    WFE_Stmt_Pop (wfe_stmk_if_then);
  }
  if (ELSE_CLAUSE(stmt)) {
    WFE_Stmt_Push (else_block, wfe_stmk_if_else, Get_Srcpos());
    for (tree t = ELSE_CLAUSE(stmt); t; t = TREE_CHAIN(t))
      WFE_Expand_Stmt (t);
    WFE_Stmt_Pop (wfe_stmk_if_else);
  }
} /* WFE_Expand_If */

static void
WFE_Expand_Label (tree label)
{
  LABEL_IDX label_idx = WFE_Get_LABEL (label, TRUE);
  DECL_SYMTAB_IDX(label) = CURRENT_SYMTAB;
  if (!DECL_LABEL_DEFINED(label)) {
    WN *wn;
    DECL_LABEL_DEFINED(label) = TRUE;
    wn = WN_CreateLabel ((ST_IDX) 0, label_idx, 0, NULL);
    WFE_Stmt_Append (wn, Get_Srcpos ());
  }
} /* WFE_Expand_Label */

static void
WFE_Expand_Return (tree stmt, tree retval, BOOL do_temp_cleanups)
{
  WN *wn;

  if (retval == NULL_TREE) {
    if (do_temp_cleanups)
      Do_Temp_Cleanups (stmt);
    if (scope_cleanup_i >= 0) {
      Pop_Scope_And_Do_Cleanups(FALSE, TRUE);
      Do_All_Other_Scope_Cleanups();
    }
    wn = WN_CreateReturn ();
  }
  else {
    /* 'target' non-NULL indicates that we should return the function
       value by defining it into 'target' (used to support return
       through invisible first argument). */
    tree ret_decl = Current_Function_Result_Decl();
    ST *target = ((ret_decl == NULL_TREE) ? NULL : Get_ST(ret_decl));

    WN *rhs_wn;
    TY_IDX ret_ty_idx = ((target) ? MTYPE_To_TY(MTYPE_V) :
			 Get_TY(TREE_TYPE(TREE_TYPE(Current_Function_Decl()))));

    /* We may start a temp cleanup region inside the OPR_COMMA
       generated while expanding the return value. For proper nesting,
       we must start a conditional exception region, even though this
       code is not conditional. Otherwise, the OPR_COMMA block will
       end before the temp cleanup region, causing an error. */
    Push_Conditional_Cleanup_Level();
    rhs_wn = WFE_Expand_Expr_With_Sequence_Point (retval, TY_mtype (ret_ty_idx),
						  target);
    Pop_Conditional_Cleanup_Level();

    /* If we have any cleanups and there is something to return
       (i.e. we're not just returning it through 'target'), then we
       may need to make a copy of the return value before we do the
       cleanups. */
    if (!target &&
	((Num_Temp_Cleanups(stmt) != 0) ||
	 (Num_Scope_Cleanups(NULL) != 0))) {
      /* If 'rhs_wn' is an OPR_COMMA, then we can insert the body of
	 the OPR_COMMA before the cleanups. */
      if (WN_operator(rhs_wn) == OPR_COMMA) {
	WFE_Stmt_Append(WN_kid0(rhs_wn), Get_Srcpos());
	rhs_wn = WN_kid1(rhs_wn);
      }
      
      ST *ret_st = Gen_Temp_Symbol (ret_ty_idx, "__return_val");
      TYPE_ID ret_mtype = TY_mtype (ret_ty_idx);
      WFE_Set_ST_Addr_Saved (rhs_wn);
      wn = WN_Stid (ret_mtype, 0, ret_st, ret_ty_idx, rhs_wn);
      WFE_Stmt_Append(wn, Get_Srcpos());
      rhs_wn = WN_Ldid (ret_mtype, 0, ret_st, ret_ty_idx);
    }

    /* Finish any cleanups... */
    if (do_temp_cleanups)
      Do_Temp_Cleanups (stmt);

    if (scope_cleanup_i >= 0) {
      Pop_Scope_And_Do_Cleanups(FALSE, TRUE);
      Do_All_Other_Scope_Cleanups();
    }

    if (target ||
	(!WFE_Keep_Zero_Length_Structs    &&
	 TY_mtype (ret_ty_idx) == MTYPE_M &&
	 TY_size (ret_ty_idx) == 0))
    {
      // function returning zero length struct
      if (WN_has_side_effects (rhs_wn)) {
        rhs_wn = WN_CreateEval (rhs_wn);  
        WFE_Stmt_Append(rhs_wn, Get_Srcpos());
      }
      wn = WN_CreateReturn ();
    }
    else {
      WFE_Set_ST_Addr_Saved (rhs_wn);
      wn = WN_CreateReturn_Val(OPR_RETURN_VAL, WN_rtype(rhs_wn), MTYPE_V, rhs_wn);
    }
  }
  WFE_Stmt_Append(wn, Get_Srcpos());
} /* WFE_Expand_Return */


static void
Push_Scope (tree t)
{
  if (++scope_i == scope_max) {
    scope_max = ENLARGE (scope_max);
    scope_stack =
      (tree *) realloc (scope_stack,
	 	        scope_max * sizeof (tree));
  }
  scope_stack[scope_i] = t;
}


void
Mark_Scopes_And_Labels (tree stmt)
{
  if (!stmt) return;

  switch (TREE_CODE(stmt)) {
    case COMPOUND_STMT: {
      tree t;
      for (t = COMPOUND_BODY(stmt); t; t = TREE_CHAIN(t))
	Mark_Scopes_And_Labels (t);
      break;
    }

    case DO_STMT:
      Mark_Scopes_And_Labels (DO_BODY(stmt));
      break;

    case FOR_STMT: {
      tree init = FOR_INIT_STMT(stmt);
      tree cond = FOR_COND(stmt);
      tree body = FOR_BODY(stmt);
      while (init) {
	Mark_Scopes_And_Labels (init);
	init = TREE_CHAIN(init);
      }
      if (TREE_CODE(cond) == TREE_LIST)
	Mark_Scopes_And_Labels(cond);
      while (body) {
	Mark_Scopes_And_Labels (body);
  	body = TREE_CHAIN(body);
      }
      break;
    }

    case IF_STMT:
      Mark_Scopes_And_Labels (IF_COND(stmt));
      Mark_Scopes_And_Labels (THEN_CLAUSE(stmt));
      Mark_Scopes_And_Labels (ELSE_CLAUSE(stmt));
      break;

    case LABEL_STMT:
      if (scope_i == -1)
	LABEL_SCOPE(LABEL_STMT_LABEL(stmt)) = NULL_TREE;
      else
	LABEL_SCOPE(LABEL_STMT_LABEL(stmt)) = scope_stack [scope_i];
      break;

    case SCOPE_STMT:
      if (SCOPE_BEGIN_P(stmt)) {
	Push_Scope(stmt);
      }
      else {
	SCOPE_NUMBER(scope_stack [scope_i]) = ++scope_number;
	--scope_i;
      }
      break;

    case SWITCH_STMT:
      Mark_Scopes_And_Labels (SWITCH_COND(stmt));
      Mark_Scopes_And_Labels (SWITCH_BODY(stmt));
      break;

    case TREE_LIST:
      for (tree t = TREE_PURPOSE(stmt); stmt; stmt = TREE_CHAIN(stmt))
	Mark_Scopes_And_Labels(t);
      break;

    case TRY_BLOCK: {
      tree handler;
      Mark_Scopes_And_Labels (TRY_STMTS(stmt));
      for (handler = TRY_HANDLERS(stmt);
		     handler;
	 	     handler = TREE_CHAIN(handler))
	  for (tree t = HANDLER_BODY(handler); t; t = TREE_CHAIN(t))
	    Mark_Scopes_And_Labels (t);
      break;
      }

    case WHILE_STMT:
      Mark_Scopes_And_Labels (WHILE_COND(stmt));
      Mark_Scopes_And_Labels (WHILE_BODY(stmt));
      break;
    
    default:
      break;
  }
}

static void
WFE_Expand_Start_Case (tree selector)
{
  TYPE_ID index_mtype = Mtype_comparison (
                          TY_mtype (Get_TY (TREE_TYPE (TREE_VALUE(selector))))); 
  WN *switch_block = WN_CreateBlock ();
  WN *index;
  index = WFE_Expand_Expr (selector, NULL);
  WFE_Stmt_Push (switch_block, wfe_stmk_switch, Get_Srcpos());
  if (++switch_info_i == switch_info_max) {
    switch_info_max   = ENLARGE(switch_info_max);
    switch_info_stack = (SWITCH_INFO *) realloc (switch_info_stack,
                                                 switch_info_max * sizeof (SWITCH_INFO));
  }
  switch_info_stack [switch_info_i].index             = index;
  switch_info_stack [switch_info_i].start_case_index  = case_info_i + 1;
  switch_info_stack [switch_info_i].default_label_idx = 0;
  WFE_Record_Loop_Switch (SWITCH_STMT);
} /* WFE_Expand_Start_Case */

static void
WFE_Expand_End_Case (void)
{
  INT32  i;
  INT32  n;
  WN    *switch_wn;
  WN    *switch_block;
  WN    *case_block;
  WN    *case_entry;
  WN    *def_goto;
  WN    *wn;
  LABEL_IDX exit_label_idx;

  n = 0;
  if (break_continue_info_stack [break_continue_info_i].break_label_idx)
    exit_label_idx = break_continue_info_stack [break_continue_info_i].break_label_idx;
  else
    New_LABEL (CURRENT_SYMTAB, exit_label_idx);
  if (switch_info_stack [switch_info_i].default_label_idx)
    def_goto = WN_CreateGoto (switch_info_stack [switch_info_i].default_label_idx);
  else
    def_goto = WN_CreateGoto (exit_label_idx);
  case_block = WN_CreateBlock ();
  for (i = switch_info_stack [switch_info_i].start_case_index;
       i <= case_info_i;
       i++) {
    INT64     case_value;
    LABEL_IDX case_label_idx = case_info_stack [i].case_label_idx;
    for (case_value  = case_info_stack [i].case_lower_bound_value;
         case_value <= case_info_stack [i].case_upper_bound_value;
         case_value++) {
      case_entry = WN_CreateCasegoto (case_value, case_label_idx);
      n++;
      WN_INSERT_BlockLast (case_block, case_entry);
    }
  }
  switch_wn = WN_CreateSwitch (n,
                               switch_info_stack [switch_info_i].index,
                               case_block,
                               def_goto,
                               exit_label_idx);
  switch_block = WFE_Stmt_Pop (wfe_stmk_switch);
  WFE_Stmt_Append (switch_wn, Get_Srcpos ());
  WFE_Stmt_Append (switch_block, Get_Srcpos ());
  wn = WN_CreateLabel ((ST_IDX) 0, exit_label_idx, 0, NULL);
  WFE_Stmt_Append (wn, Get_Srcpos ());
  case_info_i = switch_info_stack [switch_info_i].start_case_index - 1;
  --switch_info_i;
} /* WFE_Expand_End_Case */

static void
WFE_Expand_Switch (tree stmt)
{
  WFE_Expand_Start_Case (SWITCH_COND(stmt));
  WFE_Expand_Stmt       (SWITCH_BODY(stmt));
  WFE_Expand_End_Case   ();
  --break_continue_info_i;

  /* If we are currently emitting into a scope, then we must end that
     scope before finishing the switch. A scope started inside the
     switch condition will be ended after the switch, but for proper
     region nesting we must end it now. */
  if (WFE_Stmt_Top_Kind() == wfe_stmk_scope)
    Pop_Scope_And_Do_Cleanups(FALSE, TRUE);
}

static void
Set_Handler_Labels (tree stmt)
{
  for (tree handler = TRY_HANDLERS(stmt);
       handler;
       handler = TREE_CHAIN (handler)) {
    LABEL_IDX handler_label;
    New_LABEL (CURRENT_SYMTAB, handler_label);
    HANDLER_LABEL(handler) = handler_label;
  }
}

INT
Current_Handler_Count()
{
#ifndef ADD_HANDLER_INFO
  return 0;
#else
  if (temp_cleanup_i != -1) {
    for (int i = temp_cleanup_i; i != -1; --i) {
      if (temp_cleanup_stack [i].label_idx != 0)
	return 1;
    }
  }

  for (int i = scope_cleanup_i; i != -1; --i) {
    tree t = scope_cleanup_stack [i].stmt;
    if (TREE_CODE(t) == CLEANUP_STMT)
      return 1;
    INT result = 0;
    if (TREE_CODE(t) == TRY_BLOCK) {
      for (tree handler = TRY_HANDLERS(t);
           handler;
	   handler = TREE_CHAIN(handler))
        ++result;
      return result;
    }
  }

  return 0;
#endif
}

#ifdef ADD_HANDLER_INFO
void
Add_Handler_Info (WN * call_wn, INT i, INT num_handlers)
{
  if (temp_cleanup_i != -1) { 
    for (int i = temp_cleanup_i; i != -1; --i)
      if (temp_cleanup_stack [i].label_idx != 0) {
        WN_kid (call_wn, i++) =
          WN_CreateHandlerInfo (0,
                                temp_cleanup_stack[temp_cleanup_i].label_idx);
        return;
 
      }
  }

  int j = scope_cleanup_i;
  while (TREE_CODE(scope_cleanup_stack [j].stmt) == SCOPE_STMT)
    --j;
  tree t = scope_cleanup_stack [j].stmt;
  if (TREE_CODE(t) == TRY_BLOCK && TREE_CODE(TRY_HANDLERS(t)) == HANDLER) {
    for (tree handler = TRY_HANDLERS(t);
         handler;
         handler = TREE_CHAIN(handler))
      WN_kid (call_wn, i++) =
        WN_CreateHandlerInfo (Tid_For_Handler (handler),
			      HANDLER_LABEL (handler));
    return;
  }

  WN_kid (call_wn, i++) =
    WN_CreateHandlerInfo (0, scope_cleanup_stack [j].label_idx);
}  
#endif /* ADD_HANDLER_INFO */


/* Generate and return the region to represent a try block. */
WN *
WFE_Start_Try_Block (void)
{
  WN *body = WN_CreateBlock();
  WN *pragmas = WN_CreateBlock();
  WN *exits = WN_CreateBlock();

  WN *region = WN_CreateRegion(REGION_KIND_TRY,
			       body, 
			       pragmas,
			       exits, 
			       -1,
			       NULL);
  return region;
}


/* Finish try block represented by 'region'. If 'handler' is non-NULL,
   expand it as the handler for the region. */
void
WFE_Finish_Try_Block (WN *region, tree handler)
{
  Set_PU_has_exc_scopes(Get_Current_PU());
  WFE_Stmt_Append(region, Get_Srcpos());

  /* If 'handler' is non-NULL, we must emit it as the sole handler for
     'region'. If 'handler' is NULL, it is assumed the caller will
     take care of emitting the handlers. */
  if (handler)
  {
    /* Generate a label for the handler, and add a GOTO in 'region's
       pragmas to show that the handler is an exception target for the
       region. */
    LABEL_IDX handler_label_idx;
    New_LABEL(CURRENT_SYMTAB, handler_label_idx);
    WN *handler_label = WN_CreateLabel((ST_IDX) 0, handler_label_idx,
				       0, NULL);
    WN_Set_Label_Is_Handler_Begin(handler_label);

    WFE_Stmt_Push(WN_region_pragmas(region), wfe_stmk_region_pragmas, Get_Srcpos());
    WFE_Stmt_Append(WN_CreateGoto((ST_IDX)NULL, handler_label_idx), Get_Srcpos());
    WFE_Stmt_Pop(wfe_stmk_region_pragmas);

    /* Add EH info describing 'handler'. */
    WN_ereg_supp(region) = Create_Try_Region_Info(XT_EH_TYPE_TRY, NULL,
						  handler_label_idx, ST_IDX_ZERO);

    /* Because of the way regions must be nested, we emit the handlers
       immediately after the try region; not at the end of the
       function like gcc. So we generate a label that we use to mark
       the "continue" point after the 'handler'. If there is no
       exception, control-flow after the try-block jumps to that label
       to continue execution, and any handlers jump to that label when
       done. */
    LABEL_IDX cont_label_idx;
    New_LABEL (CURRENT_SYMTAB, cont_label_idx);

    /* If we fall-through the try block, jump around 'handler' to
       continue execution. */
    WFE_Stmt_Append(WN_CreateGoto((ST_IDX)NULL, cont_label_idx), Get_Srcpos());
  
    /* Emit label and 'handler'. */
    WFE_Stmt_Append(handler_label, Get_Srcpos());
    WFE_Expand_Expr(handler, NULL, false);

    /* Emit "continue" label after 'handler'. */
    WFE_Stmt_Append(WN_CreateLabel((ST_IDX) 0, cont_label_idx, 0, NULL), Get_Srcpos());
  }
}


static void
WFE_Expand_Try (tree stmt)
{
  FmtAssert(flag_exceptions, ("try block not allowed when exceptions disabled"));
  
  /*
   * Don't generate anything if there are no statements in the
   * try-block.
   */

  if (TRY_STMTS(stmt) == NULL_TREE)
    return;

  /* Set start labels for each handler. */
  Set_Handler_Labels(stmt);

  Push_Scope_Cleanup (stmt);

  /* Generate code for the try-block. The code is placed in a try region. */
  WN *region = WFE_Start_Try_Block();
  WN *body = WN_region_body(region);
  WN *pragmas = WN_region_pragmas(region);

  /* body ... */
  WFE_Stmt_Push(body, wfe_stmk_try_block, Get_Srcpos());
  for (tree s = TRY_STMTS(stmt); s; s = TREE_CHAIN(s))
  {
    WFE_Expand_Stmt(s);
  }

  /* If the try block spans a constructor (including the initializers
     for base classes, see pr2723), then we could have partials still
     open that we need to close before we end the try block (or else
     the regions won't nest properly and we get an assertion). */
  while (WFE_Stmt_Top_Kind() == wfe_stmk_partials)
    Pop_Partials_And_Do_Cleanups ();
  
  WFE_Stmt_Pop(wfe_stmk_try_block);
  
  /* pragmas ... */
  WFE_Stmt_Push(pragmas, wfe_stmk_region_pragmas, Get_Srcpos());
  for (tree handler = TRY_HANDLERS(stmt); handler; handler = TREE_CHAIN (handler))
  {
    if (HANDLER_LABEL(handler))
      WFE_Stmt_Append(WN_CreateGoto((ST_IDX)NULL, HANDLER_LABEL(handler)), Get_Srcpos());
  }
  WFE_Stmt_Pop(wfe_stmk_region_pragmas);

  /* Add EH info describing each handler and associated rtti, if any. */
  WN_ereg_supp(region) = Create_Try_Region_Info(XT_EH_TYPE_TRY, stmt,
						LABEL_IDX_ZERO, ST_IDX_ZERO);

  WFE_Finish_Try_Block(region, NULL);
  --scope_cleanup_i;

  /* Because of the way regions must be nested, we emit the handlers
     immediately after the try region; not at the end of the function
     like gcc. So we generate a label that we use to mark the
     "continue" point after the handlers. If there is no exception,
     control-flow after the try-block jumps to that label to continue
     execution, and any handlers jump to that label when done. */

  LABEL_IDX cont_label_idx;
  New_LABEL (CURRENT_SYMTAB, cont_label_idx);

  /* If we fall-through the try block, jump around the handlers to
     continue execution. */
  WFE_Stmt_Append(WN_CreateGoto((ST_IDX)NULL, cont_label_idx), Get_Srcpos());
  
  /* Emit the handlers... */

  Do_Handlers (TRY_HANDLERS(stmt), cont_label_idx);

  /* Emit "continue" label after handlers. */

  WFE_Stmt_Append (WN_CreateLabel((ST_IDX) 0, cont_label_idx, 0, NULL), Get_Srcpos());
}

static WN *
Call_Named_Function (ST * st)
{
  WN * call_wn = WN_Create (OPR_CALL, MTYPE_V, MTYPE_V, 0);
  WN_st_idx (call_wn) = ST_st_idx (st);
  WFE_Stmt_Append (call_wn, Get_Srcpos());
  return call_wn;
}

void
Call_Throw (void)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));
  
  static ST * st = NULL;
  if (st == NULL) {
    st = Throw_Runtime_ST();
  }
  WN *call = Call_Named_Function (st);
  WN_Set_Call_Never_Return(call);
}

void
Call_Rethrow (void)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));

  static ST * st = NULL;
  if (st == NULL) {
    st = Rethrow_Runtime_ST();
  }
  WN *call = Call_Named_Function (st);
  WN_Set_Call_Never_Return(call);
}

void Call_Terminate (void)
{
  Is_True(flag_exceptions, ("expecting exceptions to be enabled"));

  static ST * st = NULL;
  if (st == NULL) {
    st = Terminate_Runtime_ST();
  }
  WN *call = Call_Named_Function (st);
  WN_Set_Call_Never_Return(call);
}

void
WFE_Expand_Handlers_And_Cleanups (void)
{
#ifdef TARG_XTENSA
  /* We emit the handlers right after the try block, not at the end of
     the function; so we shouldn't have any pending handlers that need
     emission... */
  FmtAssert(handler_info_i == -1, ("unexpected non-emitted handlers at end of function"));
#else
  /* Expand the handlers... */
  while (handler_info_i != -1) {
    LABEL_IDX start_handlers;
    New_LABEL (CURRENT_SYMTAB, start_handlers);
    Set_LABEL_addr_saved (start_handlers);
    WFE_Stmt_Append (WN_CreateLabel ((ST_IDX) 0, start_handlers, 0, NULL),
		     Get_Srcpos());

    tree handler = handler_info_stack[handler_info_i].handler;
    --handler_info_i;
    Do_Handlers(handler_info_stack [handler_info_i+1].handler,
		handler_info_stack[handler_info_i+1].label_idx);
  }
#endif
  
  /* Expand the cleanups... */
  Do_EH_Cleanups();
}

void
WFE_Expand_Stmt(tree stmt)
{
#ifdef WFE_DEBUG
  fprintf (stderr,
           "{( WFE_Expand_Stmt: %x, %s\n", stmt, WFE_Tree_Node_Name (stmt)); // ")}"
#endif /* WFE_DEBUG */

  if (TREE_CODE(stmt) == LABEL_DECL)
    lineno = DECL_SOURCE_LINE(stmt);
  else if (TREE_CODE(stmt) != CASE_LABEL)
    lineno = STMT_LINENO(stmt);

  LINKED_LIST **lla = &file_pragmas;
  while (*lla) {
    PRAGMA_INFO *pi=(PRAGMA_INFO *)(*lla)->data;
    if (pi->next_stmt==stmt) {
      WN_PRAGMA_ID pid = WN_PRAGMA_UNDEFINED;
      if (strcmp(pi->name, "no_reorder_memory") == 0 ||
	  strcmp(pi->name, "flush_memory") == 0 ||
	  strcmp(pi->name, "no_reorder") == 0 ||
	  strcmp(pi->name, "flush") == 0) {

	BOOL no_reorder_memory =
		(strcmp(pi->name, "no_reorder_memory") == 0);
	BOOL flush_memory =
		(strcmp(pi->name, "flush_memory") == 0);
	BOOL no_reorder= (strcmp(pi->name, "no_reorder") == 0);
	BOOL flush= (strcmp(pi->name, "flush") == 0);

	WN* fb = WN_CreateBarrier(TRUE, 0);   // forward
	if (no_reorder_memory)
	  WN_Set_Barrier_No_Reorder_Memory(fb);
	else if (flush_memory)
	  WN_Set_Barrier_Flush_Memory(fb);
	else if (no_reorder)
	  WN_Set_Barrier_No_Reorder(fb);
	else if (flush)
	  WN_Set_Barrier_Flush(fb);

	WFE_Stmt_Append(fb, Get_Srcpos());

	WN* bb = WN_CreateBarrier(FALSE, 0);  // backward
	if (no_reorder_memory)
	  WN_Set_Barrier_No_Reorder_Memory(bb);
	else if (flush_memory)
	  WN_Set_Barrier_Flush_Memory(bb);
	else if (no_reorder)
	  WN_Set_Barrier_No_Reorder(bb);
	else if (flush)
	  WN_Set_Barrier_Flush(bb);
	WFE_Stmt_Append(bb, Get_Srcpos());

        pragma_list_remove_el(lla);
	continue;

      } else if (strcmp(pi->name, "super_swp") == 0) {

	pid = WN_PRAGMA_SUPER_SWP;

      } else if (strcmp(pi->name, "swp_schedule") == 0) {

	const char* str = (const char*)pi->arg1;
	pid = WN_PRAGMA_SWP_SCHEDULE;
	pi->arg1 = (INT)Save_Str(str);

      } else if (strcmp(pi->name, "frequency_hint") == 0) {
	pid = WN_PRAGMA_MIPS_FREQUENCY_HINT;
        pi->arg1 = (strcmp(pi->id, "FREQUENT") == 0 ? 
                    FREQUENCY_HINT_FREQUENT : FREQUENCY_HINT_NEVER);
      }
      else if (strcmp(pi->name,"aligned")==0)
	pid = WN_PRAGMA_ALIGNED;
      else if (strcmp(pi->name,"ivdep")==0)
	pid = WN_PRAGMA_IVDEP;
      else if (strcmp(pi->name,"concurrent")==0) {
	pid = WN_PRAGMA_KAP_ASSERT_DO;
        pi->arg1 = ASSERT_DO_CONCURRENT;
      }
      else if (strcmp(pi->name,"generate_hw")==0)
	pid = WN_PRAGMA_XPRES_GENERATE_HW;
      else if (strcmp(pi->name,"simd_if_convert")==0)
	pid = WN_PRAGMA_SIMD_IF_CONVERT;

      ST *st=NULL;
      if (pi->decl) {
	st = Get_ST(pi->decl);
	if (!st)
	  DevWarn("Cannot find ST for DECL in PRAGMA %s %s\n",pi->name,pi->id);

        /* Don't forward substitute symbols marked as aligned, so that
           the alignment information is not lost. */
        if ((pid == WN_PRAGMA_ALIGNED) && st)
          Set_ST_dont_prop(st);
      }

      /* SIMD pragma just expands into multiple pragmas... */
      if (strcmp(pi->name,"simd")==0) {
        WN *wn = WN_CreatePragma (WN_PRAGMA_SIMD_IF_CONVERT, st, pi->arg1, pi->arg2);
        WFE_Stmt_Append (wn, Get_Srcpos());
        wn = WN_CreatePragma (WN_PRAGMA_KAP_ASSERT_DO, st, ASSERT_DO_CONCURRENT, pi->arg2);
        WFE_Stmt_Append (wn, Get_Srcpos());
      } else {
        WN *wn = WN_CreatePragma (pid, st, pi->arg1, pi->arg2);
        WFE_Stmt_Append (wn, Get_Srcpos());
      }
      pragma_list_remove_el(lla);
    } else
      lla=&((*lla)->next);
  }

  /* Normally case labels are emitted into a "wfe_stmk_switch"
     statement block. But we may see the top block as a
     "wfe_stmk_scope" if the preceeding case started a cleanup, but
     didn't explicitly have scoping. For example:

     case 3:
      throw(LCLASS(1));

     where LCLASS is some object needing constructing.

     If we are in a scope, then we need to end that scope before
     emitting the label, otherwise the label (and following
     statements), will be incorrectly placed in the scope that should
     only contain the previous case. */
  if (TREE_CODE(stmt) == CASE_LABEL)
  {
    if (WFE_Stmt_Top_Kind() == wfe_stmk_scope)
      Pop_Scope_And_Do_Cleanups(FALSE, TRUE);
    else if (WFE_Stmt_Top_Kind() != wfe_stmk_switch)
    {
      extern char *WFE_Stmt_Kind_Name[];
      DevWarn("Expecting case label to be emitted into switch statement block,"
	      " instead got %s", WFE_Stmt_Kind_Name[WFE_Stmt_Top_Kind()]);
    }
  }

  /* We won't have any temp cleanups for these stmts and we don't want
     to start the region for one since it wouldn't nest correctly with
     the scope/partials region started for it. */
  BOOL do_temp_cleanups = FALSE;
  if (STMT_IS_FULL_EXPR_P(stmt) &&
      (TREE_CODE(stmt) != SCOPE_STMT) &&
      (TREE_CODE(stmt) != CLEANUP_STMT) &&
      (TREE_CODE(stmt) != CTOR_STMT) &&
      (TREE_CODE(stmt) != SUBOBJECT) &&
      (TREE_CODE(stmt) != FOR_STMT) &&
      (TREE_CODE(stmt) != DO_STMT) &&
      (TREE_CODE(stmt) != WHILE_STMT) &&
      (TREE_CODE(stmt) != COMPOUND_STMT))
  {
    do_temp_cleanups = TRUE;
    Push_Temp_Cleanup (stmt, true, false);
  }

  Push_Conditional_Cleanup_Level(WFE_Stmt_Top(), WFE_Stmt_Last());

  switch (TREE_CODE(stmt)) {
    case ASM_STMT:
      Wfe_Expand_Asm_Operands (ASM_STRING    (stmt),
			       ASM_OUTPUTS   (stmt),
			       ASM_INPUTS    (stmt),
			       ASM_CLOBBERS  (stmt),
			       ASM_VOLATILE_P(stmt),
			       NULL,
			       0);
      break;

    case BREAK_STMT:
      WFE_Expand_Break ();
      break;

    case CASE_LABEL:
      WFE_Expand_Case (CASE_LOW(stmt), CASE_HIGH(stmt));
      break;

    case CLEANUP_STMT:
      WFE_Expand_Cleanup (stmt);
      break;

    case COMPOUND_STMT: {
      tree t;
      for (t = COMPOUND_BODY(stmt);
	   t != NULL;
	   t = TREE_CHAIN(t))
	WFE_Expand_Stmt (t);
      break;
    }

    case CONTINUE_STMT:
      WFE_Expand_Continue ();
      break;

    case DECL_STMT:
      WFE_Expand_Decl (DECL_STMT_DECL (stmt));
      break;

    case DO_STMT:
      WFE_Expand_Loop (stmt);
      break;

    case EXPR_STMT:
      WFE_One_Stmt (EXPR_STMT_EXPR(stmt));
      break;

    case FOR_STMT:
      WFE_Expand_Loop (stmt);
      break;

    case GOTO_STMT: {
      tree dest = GOTO_DESTINATION(stmt);
      if (TREE_CODE(dest) == LABEL_DECL)
        WFE_Expand_Goto (dest);
      else
        WFE_Expand_Computed_Goto(dest);
      break;
    }

    case IF_STMT:
      WFE_Expand_If (stmt);
      break;

    case LABEL_STMT:
      WFE_Expand_Label (LABEL_STMT_LABEL(stmt));
      break;

    case RETURN_STMT: {
      tree t = RETURN_EXPR(stmt);
      if (t && TREE_CODE(t) == INIT_EXPR) {
  	Is_True(TREE_CODE(TREE_OPERAND(t, 0)) == RESULT_DECL,
			  ("WFE_Expand_Stmt: expected RESULT_DECL"));
	tree t1 = TREE_OPERAND(t, 1);
	if (TREE_CODE(t1) == TARGET_EXPR)
  	  TREE_OPERAND(t1, 2) = 0;
	WFE_Expand_Return (stmt, t1, do_temp_cleanups);
      }
      else
	WFE_Expand_Return(stmt, t, do_temp_cleanups);

      // We've already called Do_Temp_Cleanups!
      do_temp_cleanups = FALSE;
      break;
    }

    case SCOPE_STMT:
      if (SCOPE_BEGIN_P(stmt)) {
	Push_Scope_Cleanup (stmt);
	scope_depth++;
      }
      else {
	Pop_Scope_And_Do_Cleanups (TRUE, TRUE);
	scope_depth--;
      }
      break;

    case START_CATCH_STMT:
      break;

    case SUBOBJECT:
      Push_Partials_Cleanup (stmt);
      break;

    case SWITCH_STMT:
      WFE_Expand_Switch (stmt);
      break;

    case TRY_BLOCK:
      WFE_Expand_Try (stmt);
      break;

    case WHILE_STMT:
      WFE_Expand_Loop (stmt);
      break;

    case CTOR_STMT:
      if (CTOR_BEGIN_P (stmt))
	Push_Partials_Cleanup (stmt);
      else
	Pop_Partials_And_Do_Cleanups ();
      break;

    default:
      Is_True(FALSE,
              ("WFE_Expand_Stmt: Unexpected statement node %s", WFE_Tree_Node_Name (stmt)));
      break;
  } /* switch */
  
  if (do_temp_cleanups)
    Do_Temp_Cleanups (stmt);

  Pop_Conditional_Cleanup_Level();

#ifdef WFE_DEBUG
  fprintf (stderr, // "{("
           ")} WFE_Expand_Stmt: %x, %s\n", stmt, WFE_Tree_Node_Name (stmt));
#endif /* WFE_DEBUG */

} /* WFE_Expand_Stmt */

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
