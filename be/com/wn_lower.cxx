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


//-*-c++-*-

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <values.h>
#include <alloca.h>
#include <isam.h>
#ifdef _WIN32
#include <signal.h>
#else
#include <sys/signal.h>
#endif
#include <elf.h>

#include "defs.h"
#include "config.h"
#include "config_asm.h"
#include "config_debug.h"
#include "config_opt.h"
#include "config_targ_options.h"
#include "errors.h"
#include "erglob.h"
#include "erbe.h"
#include "tracing.h"
#include "glob.h"
#include "timing.h"
#include "stab.h"
#include "strtab.h"
#include "util.h"
#include "wn.h"
#include "wn_util.h"
#include "stblock.h"
#include "data_layout.h"
#include "ir_reader.h"
#include "targ_sim.h"
#include "targ_const.h"
#include "const.h"
#include "ttype.h"
#include "wio.h"
#include "wn_mp.h"
#include "wn_pragmas.h"
#include "wn_simp.h"
#include "opt_alias_interface.h"
#include "wn_lower.h"
#include "region_util.h"
#include "wutil.h"
#include "wn_map.h"
#include "wn_fio.h"
#include "wn_trap.h"
#include "wn_instrument.h"
#include "pu_info.h"
#include "w2op.h"
#include "be_symtab.h"
#include "betarget.h"
#include "be_util.h"
#include "opt_cvtl_rule.h"
#include "fb_whirl.h"
#include "intrn_info.h"
#include "at_defs.h"

/* My changes are a hack till blessed by Steve. (suneel) */
#define SHORTCIRCUIT_HACK 1

/* this next header should be after the external declarations in the others */
#include "pragma_weak.h"	/* Alias routines defined in wopt.so */

/* ====================================================================
 *			 Exported Functions
 * ====================================================================
 */
extern PREG_NUM AssignExpr(WN *, WN *, TYPE_ID);

extern BOOL lower_is_aliased(WN *, WN *, INT64 size, BOOL defalt);

extern INT32 compute_copy_alignment(TY_IDX, TY_IDX, INT32 offset);

extern TYPE_ID compute_copy_quantum(INT32 );

extern WN *WN_I1const(TYPE_ID, INT64 con);

/* ====================================================================
 *			 Imported Declarations
 * ====================================================================
 */
extern WN *emulate(WN*& block, WN *);

extern WN *intrinsic_runtime(WN *, WN *);

#ifdef TARG_XTENSA
extern WN *coerce_emulation_return( WN *, WN *);
#endif

extern WN *make_pointer_to_node(WN *, WN *);

extern char *INTR_intrinsic_name( WN *);

extern void fdump_dep_tree(FILE *, WN *, struct ALIAS_MANAGER *);

extern void InitParityMaps(void);

extern "C" void LNOPreserveMapPair(WN *, WN *, WN *);

extern "C" void LNOPreserveMap(WN *, WN *);

extern "C" void LNOPruneMapsUsingParity(void);

extern "C" void LNOPrintDepGraph(FILE *);

extern void enable_tree_freq_display(void);

extern TYPE_ID INTR_return_mtype(INTRINSIC id);

extern BE_ST_TAB   Be_st_tab;

/* ====================================================================
 *			 Forward Declarations
 * ====================================================================
 */

static WN *lower_scf(WN *, WN *, LOWER_ACTIONS);
static WN *lower_expr(WN *, WN *, LOWER_ACTIONS, WN *p =NULL);
static WN *lower_store(WN *, WN *, LOWER_ACTIONS);
static WN *lower_call(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic_call(WN *, WN *, LOWER_ACTIONS);
static WN *lower_intrinsic_op(WN *, WN *, LOWER_ACTIONS);
static WN *lower_if(WN *, WN *, LOWER_ACTIONS);
static WN *lower_stmt(WN *, WN *, LOWER_ACTIONS);
static WN *lower_entry(WN *, LOWER_ACTIONS);
static WN *lower_eval(WN *, WN *, LOWER_ACTIONS);
static WN *lower_copy_tree(WN *, LOWER_ACTIONS);
static WN *lower_emulation(WN *, WN *, LOWER_ACTIONS, BOOL &intrinsic_lowered);
static WN *lower_mstore(WN *, WN *, LOWER_ACTIONS);
static WN *lower_nary_madd(WN *, WN *, LOWER_ACTIONS);
static WN *lower_madd(WN *, WN *, LOWER_ACTIONS);
static WN *lower_assert(WN *, WN *, LOWER_ACTIONS);
static WN *lower_trapuv(WN *, WN *, LOWER_ACTIONS);
static WN *lower_base_reference(WN *, WN *, ST *, INT64, LOWER_ACTIONS);
static WN *lower_base_register(WN *, ST *, INT64, LOWER_ACTIONS);
static WN *lower_dereference(WN *, INT64, ST *, PREG_NUM, LOWER_ACTIONS);
static WN *lower_split_sym_addrs(WN *, WN *, INT64, LOWER_ACTIONS);
static WN *improve_Malignment(WN *, WN *, WN *, INT64);
static WN *lower_branch(WN *, WN *, LOWER_ACTIONS);
static WN *lower_branch_condition(BOOL, LABEL_IDX, WN *, WN **, LOWER_ACTIONS);
static WN *lower_conditional(WN *, WN *, LABEL_IDX, LABEL_IDX, BOOL,
			     LOWER_ACTIONS);
static WN *lower_tree_height(WN *, WN *, LOWER_ACTIONS);
static WN *lower_select(WN *, WN *, LOWER_ACTIONS);
static WN *lower_mpy_by_const(WN *, WN *, LOWER_ACTIONS);
static WN *lower_int_int_ll(WN *, WN *, LOWER_ACTIONS);
static WN *lower_inline_mpy64(WN *, WN *, LOWER_ACTIONS);
static WN *lower_div_by_const(WN *, WN *, LOWER_ACTIONS);
static WN *lower_rem_by_const(WN *, WN *, LOWER_ACTIONS);
static WN *lower_land_lior(WN *, WN *, LOWER_ACTIONS);

static TY_IDX coerceTY(TY_IDX, TYPE_ID);
static ST *coerceST(const ST *, TYPE_ID);
static ST *coerceST(const ST &, TYPE_ID);
static WN_OFFSET coerceOFFSET(WN *, TYPE_ID, WN_OFFSET);


static WN *lower_mload(WN *, WN *, LOWER_ACTIONS);
static void lower_mload_formal(WN *, WN *, PLOC, LOWER_ACTIONS);
static void lower_mload_actual (WN *, WN *, PLOC, LOWER_ACTIONS);
static void lower_complex_emulation(WN *, WN *, LOWER_ACTIONS, WN **, WN **);
static void lower_complex_expr(WN *, WN *, LOWER_ACTIONS, WN **, WN **);


static void lower_copy_maps(WN *, WN *, LOWER_ACTIONS);
static void lower_tree_copy_maps(WN *, WN *, LOWER_ACTIONS);

static INT32 compute_alignment(WN *, INT64);
static TYPE_ID compute_next_copy_quantum(TYPE_ID , INT32);
static WN *WN_Generate_Intrinsic_Call(INTRINSIC id, INT32 nparm, WN *parm[]);

static WN *adjust_incoming_return_location(WN *load, TYPE_ID mtype);
static WN *adjust_outgoing_return_location(WN *load, TYPE_ID mtype);

/* ====================================================================
 *			 private variables
 * ====================================================================
 */
static INT   max_region;
static char *current_preg_name;
static UINT16 loop_nest_depth;
static BOOL contains_a_loop;
static struct ALIAS_MANAGER *alias_manager;
#define PARITY_MAP_ARRAY_SIZE 32
static WN_MAP parity_map_array[PARITY_MAP_ARRAY_SIZE];
static WN_MAP lowering_parity_map = 0;
static INT32 parity_map_index = -1;
static LOWER_ACTIONS lowering_actions= 0;
static BOOL save_Div_Split_Allowed ;

static BOOL traceIO              = FALSE;
static BOOL traceSpeculate       = FALSE;
static BOOL traceAlignment       = FALSE;
static BOOL traceTreeHeight      = FALSE;
static BOOL traceSplitSymOff     = FALSE;
static BOOL traceWoptFinishedOpt = FALSE;
static BOOL traceMload           = FALSE;
// static BOOL traceUplevel = FALSE;

typedef struct CURRENT_STATE
{
  SRCPOS	srcpos;
  WN		*stmt;
  WN		*function;
  LOWER_ACTIONS	actions;
} CURRENT_STATE, *CURRENT_STATEp;

CURRENT_STATE	current_state;

#define	current_srcpos		current_state.srcpos
#define	current_stmt		current_state.stmt
#define	current_actions		current_state.actions
#define current_function	current_state.function
static SRCPOS   last_srcpos;

typedef enum MSTORE_ACTIONS
{
  MSTORE_aggregate,
  MSTORE_loop,
  MSTORE_intrinsic_memzero,
  MSTORE_intrinsic_memset,
  MSTORE_intrinsic_memcpy
} MSTORE_ACTIONS;

static const char * MSTORE_ACTIONS_name(MSTORE_ACTIONS);

/* the order of the element in Promoted_Mtype need to correspond to the mtype
   orders in Mtype_initialize() of mtypes.cxx and there are 128 mtypes
   allowed
*/
static TYPE_ID Promoted_Mtype[MTYPE_MAX] = {
  MTYPE_UNKNOWN,  /* MTYPE_UNKNOWN */
  MTYPE_UNKNOWN,  /* MTYPE_B */
  MTYPE_I4,       /* MTYPE_I1 */
  MTYPE_I4,       /* MTYPE_I2 */
  MTYPE_I4,       /* MTYPE_I4 */
  MTYPE_I8,       /* MTYPE_I8 */
  MTYPE_U4,       /* MTYPE_U1 */
  MTYPE_U4,       /* MTYPE_U2 */
  MTYPE_U4,       /* MTYPE_U4 */
  MTYPE_U8,       /* MTYPE_U8 */
  MTYPE_F4,       /* MTYPE_F4 */
  MTYPE_F8,       /* MTYPE_F8 */
  MTYPE_UNKNOWN,  /* MTYPE_F10 */
  MTYPE_UNKNOWN,  /* MTYPE_F16 */
  MTYPE_UNKNOWN,  /* MTYPE_STR */
  MTYPE_FQ,       /* MTYPE_FQ */
  MTYPE_M,        /* MTYPE_M */
  MTYPE_C4,       /* MTYPE_C4 */
  MTYPE_C8,       /* MTYPE_C8 */
  MTYPE_CQ,       /* MTYPE_CQ */
  MTYPE_V,        /* MTYPE_V */
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,

  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_XTBOOL,	  /* MTYPE_XTBOOL */
  MTYPE_XTBOOL2,  /* MTYPE_XTBOOL2 */
  MTYPE_XTBOOL4,  /* MTYPE_XTBOOL4 */
  MTYPE_XTBOOL8,  /* MTYPE_XTBOOL8 */
  MTYPE_XTBOOL16, /* MTYPE_XTBOOL16 */
  MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,

  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,

  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,

  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,
  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN,

  MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN
};

/* ====================================================================
 *			Private macros
 * ====================================================================
 */

#define OPCODE_is_intrinsic(op)                                 	\
		((OPCODE_operator((op)) == OPR_INTRINSIC_CALL) ||       \
		(OPCODE_operator((op)) == OPR_INTRINSIC_OP))

#define	Action(x)			(actions & (x))
#define	NotAction(x)			(Action(x)==0)
#define	RemoveScfAction(x)		(x & ~(LOWER_SCF))
#define	RemoveShortCircuitAction(x)	(x & ~(LOWER_SHORTCIRCUIT))


#define	WN_has_alias_info(x)	(OPCODE_is_load(WN_opcode(x))	||	\
				 OPCODE_is_store(WN_opcode(x))	||	\
				 WN_operator_is(x, OPR_PARM)    ||	\
				 WN_operator_is(x, OPR_INTRINSIC_CALL))
#define	WN_has_offset(x)	(OPCODE_has_offset(WN_opcode(x)))

#define	WN_nary_intrinsic(x)	(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				((WN_intrinsic(x)== INTRN_NARY_ADD) ||	\
      				 (WN_intrinsic(x)== INTRN_NARY_MPY)))
#define	WN_nary_add(x)		(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				 (WN_intrinsic(x)== INTRN_NARY_ADD))
#define	WN_nary_mpy(x)		(WN_operator_is(x, OPR_INTRINSIC_OP) &&	\
      				 (WN_intrinsic(x)== INTRN_NARY_MPY))

#define	WN_is_block(x)		(WN_opcode(x) == OPC_BLOCK)

#define INTRN_is_nary(x)	(((x)==INTRN_NARY_ADD) || ((x)==INTRN_NARY_MPY))

#define	WN_is_commutative(x)	(WN_opcode(x) == OPCODE_commutative_op(WN_opcode(x)))

#define	TY_is_pointer(x)	(TY_kind(x) == KIND_POINTER)

#define	lower_truebr(l,c,b,a)	lower_branch_condition(TRUE,l,c,b,a)
#define	lower_falsebr(l,c,b,a)	lower_branch_condition(FALSE,l,c,b,a)

#define mem_offset_2GB(offset)				\
	(!(INT32_MIN <= offset && offset <= INT32_MAX))

#define	PIC_SHARED		(Gen_PIC_Shared || Gen_PIC_Call_Shared)
#define	PIC_NONSHARED		(!PIC_SHARED)

#define ABS(x)			(((x)<0) ? -(x) : (x))
#define IS_POWER_OF_2(val)      ((val != 0) && ((val & (val-1)) == 0))

static void WN_copy_linenum (WN* src, WN* dest)
{
  if (src && dest &&
      WN_operator(src) != OPERATOR_UNKNOWN &&
      WN_operator(dest) != OPERATOR_UNKNOWN &&
      OPCODE_is_stmt(WN_opcode(src)) && OPCODE_is_stmt(WN_opcode(dest)))
    WN_linenum(dest) = WN_linenum(src);

  return;
}



/* ====================================================================
 * Should we lower MADD into separate MPY and ADD for given mtype
 * ==================================================================== */
static inline BOOL
disallow_madd(TYPE_ID mtype)
{
  return (MTYPE_is_float(mtype) && !(xt_hard_float && mtype == MTYPE_F4));
}


/* ====================================================================
 *
 * UINT32 compute_offset_alignment(INT32 offset, UINT32 align)
 *
 * return gcd of offset,align;
 * Used for alignment reasons;
 * For maximum efficiency, offset should be >= align
 *
 * ==================================================================== */
UINT32 compute_offset_alignment(INT32 offset, UINT32 align)
{
  UINT32 m = ABS(offset);
  UINT32 n = align;
  while (n != 0) {
    INT32 new_n = m % n;
    m = n;
    n = new_n;
  }
  return m;
}


/* ====================================================================
 *
 *  The semantics of memset require replicating the byte constant
 *  so replicate the constant into a I8/U8 depending on WN_rtype(con)
 *
 * ==================================================================== */
extern WN *WN_I1const(TYPE_ID type, INT64 con)
{
  // assume con is a byte constant, so clear the other bits
  // (otherwise replicate will "or" with sign bits).
  INT64	n=	  con & 0xff;
  INT64	maxAlign= MTYPE_alignment(Max_Uint_Mtype);

  if (con)
  {
    INT64 i;
    for(i=1; i<maxAlign; i++)
    {
      n |=	(n << 8);
    }
  }

  return WN_Intconst(Mtype_AlignmentClass(maxAlign,
					  MTYPE_type_class(type)), n);
}


/* ====================================================================
 *
 * void push_current_state(WN *tree, STATE *state)
 *
 * return and set current state
 *
 * ==================================================================== */
static void setCurrentState(WN *tree, LOWER_ACTIONS actions)
{
  if (tree) {
    current_stmt =	tree;
    current_srcpos =	WN_Get_Linenum(tree);
    if (current_srcpos)
      last_srcpos =   current_srcpos;
    current_actions =	actions;

    if (WN_opcode(tree) == OPC_FUNC_ENTRY)
      current_function = tree;
  }
}

static void setCurrentStateBlockFirst(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_BLOCK, ("expected BLOCK node"));

  setCurrentState(WN_first(tree), actions);
}

static void setCurrentStateBlockLast(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_BLOCK, ("expected BLOCK node"));

  setCurrentState(WN_last(tree), actions);
}


static CURRENT_STATE pushCurrentState(WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE saveState = current_state;

  setCurrentState(tree, actions);

  return saveState;
}

static void popCurrentState(CURRENT_STATE state)
{
  current_state = state;
  if (current_srcpos)
    last_srcpos =     current_srcpos;
}
    

/* ====================================================================
 *
 * BOOL foldConstOffset(WN *con, INT64 offset)
 *
 * BOOL foldLdaOffset(WN *lda, INT64 offset)
 *
 * can con and offset be folded to "fit" into a WN_OFFSET (INT32?)
 *
 * ==================================================================== */

static BOOL foldConstOffset(WN *con, INT64 offset)
{
  if (WN_operator_is(con, OPR_INTCONST))
  {
    INT64 sum= offset + WN_const_val(con);

    if (INT32_MIN <= sum && sum <= INT32_MAX)
      return TRUE;
  }
  return FALSE;
}

static BOOL foldLdaOffset(WN *lda, INT64 offset)
{
  if (WN_operator_is(lda, OPR_LDA))
  {
    // Don't fold merge strings.  The folding confuses the assembler/linker
    if (ST_sclass(WN_st(lda)) == SCLASS_MERGE_STRING) return FALSE;

    INT64 sum= offset + WN_lda_offset(lda);

    Is_True((WN_class(lda) != CLASS_PREG), ("lda class is PREG!!!"));

    if (INT32_MIN <= sum && sum <= INT32_MAX)
    {
      ST  *sym= WN_st(lda);

     /*
      *	if the offset is greater than the symbol size, we may generate
      * a relocation that is out of bounds (ex LFTR). See pv 482353 for
      * this rare condition
      */
      if ((ST_class(sym) == CLASS_BLOCK) &&
	  (sum > 0) &&
	  (STB_size(sym) < sum))
	return FALSE;

      return TRUE;
    }
  }
  return FALSE;
}








/* ====================================================================
 *
 * WN_OFFSET coerceOFFSET(WN *tree, TYPE_ID realTY, WN_OFFSET offset)
 *
 * The offset may either be an offset or preg number.
 *
 * There is an amazing kludge for complex return values where we
 * return F0, F2
 *
 * ==================================================================== */
static WN_OFFSET coerceOFFSET(WN *tree, TYPE_ID realTY, WN_OFFSET offset)
{

  switch (WN_operator(tree))
  {
  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_ISTORE:
  case OPR_ISTOREX:
    return offset + MTYPE_RegisterSize(realTY);

  case OPR_LDID:
  case OPR_STID:
    if (WN_class(tree) == CLASS_PREG)
    {
#ifndef TARG_XTENSA
      ISA_REGCLASS float_regclass = TI_ISA_Regclass_Float();
      PREG_NUM first_float_preg = Dedicated_Preg_First(float_regclass);

      /*
       *  amazing kludge
       *  for dedicated return register (F0) the ABI defines [F0,F2]
       *  as the return values
       */
      DevWarn("Is this kludge right for xtensa???");
      
      if (Preg_Is_Dedicated(offset) && (offset == first_float_preg))
      {
	return first_float_preg + 2;
      }
      else
      {
	return offset + Preg_Increment(realTY);
      }
#else
      return offset + Preg_Increment(realTY);
#endif
    }
    return offset + MTYPE_RegisterSize(realTY);
  }
  Fail_FmtAssertion("unexpected complex op (%s)",
		    OPCODE_name(WN_opcode(tree)));
  /*NOTREACHED*/
}

/* ====================================================================
 *
 *
 * ==================================================================== */

static void rename_preg(char *f, char *preg_class)
{
  static char name[41];

  name[0] = '\0';
  
  strncat(name, f, 30);

  if (preg_class)
  {
    strncat(name, preg_class, 10);
  }
  current_preg_name = name;
}

static void rename_reset(void)
{
  current_preg_name = NULL;
}


static void WN_Set_Flags(WN *src, WN *dst)
{
  if (OPCODE_has_flags(WN_opcode(src)))
  {
    Is_True(OPCODE_has_flags(WN_opcode(dst)), ("expected wn with flags"));
    WN_set_flag(dst, WN_flag(src));
  }
}

extern void WN_annotate_intrinsic_flags(INTRINSIC id, ST *sym)
{
  if (INTRN_is_pure(id)) {
    Set_PU_is_pure(Get_Current_PU());
  }
  if (INTRN_has_no_side_effects(id)) {
    Set_PU_no_side_effects(Get_Current_PU());
  }
}

extern void WN_annotate_call_flags(WN *call, ST *sym)
{
  WN_Set_Call_Default_Flags(call);

  if (PU_no_side_effects(Pu_Table[ST_pu(sym)]))
  {   
    WN_Reset_Call_Non_Data_Mod(call);
    WN_Reset_Call_Non_Parm_Mod(call);
    WN_Reset_Call_Parm_Mod(call);

  }
  if (PU_is_pure(Pu_Table[ST_pu(sym)]))
  {
    WN_Reset_Call_Non_Data_Mod(call);
    WN_Reset_Call_Non_Parm_Mod(call);
    WN_Reset_Call_Parm_Mod(call);

    WN_Reset_Call_Non_Data_Ref(call);
    WN_Reset_Call_Non_Parm_Ref(call);
  }
}

/* ====================================================================
 *
 *
 * Create a new label with linenum info
 *
 * ==================================================================== */
static LABEL_IDX NewLabel(void)
{
  LABEL_IDX label;
  LABEL& lab = New_LABEL(CURRENT_SYMTAB, label);
  // create label name
  char *name = (char *) alloca (strlen(".L..") + 8 + 8 + 1);
  sprintf(name, ".L%s%d%s%d", Label_Name_Separator, Current_PU_Count(), 
	Label_Name_Separator, label);
  LABEL_Init (lab, Save_Str(name), LKIND_DEFAULT);
  return label;
}

static WN *WN_Label(LABEL_IDX l)
{
  WN *label = WN_CreateLabel(ST_IDX_ZERO, l, 0, NULL);
  WN_Set_Linenum(label, current_srcpos);
  return label;
}

static WN *WN_NewLabel(void)
{
  LABEL_IDX label;
  label = NewLabel();
  return WN_Label(label);
}



/* ====================================================================
 *
 *
 * Create false/true branch with line info
 *
 * ==================================================================== */
static WN *WN_Falsebr(LABEL_IDX label, WN *cond)
{
  WN *branch = WN_CreateFalsebr(label, cond);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static WN *WN_Truebr(LABEL_IDX label, WN *cond)
{
  WN *branch = WN_CreateTruebr(label, cond);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static WN *WN_Goto(LABEL_IDX label)
{
  WN *branch =	WN_CreateGoto((ST_IDX) 0, label);
  WN_Set_Linenum(branch, current_srcpos);
  return branch;
}

static BOOL expr_is_speculative(WN *tree)
{
  if (OPT_Lower_Speculate)
  {
    BOOL  speculate;

    speculate = WN_Expr_Can_Be_Speculative(tree, alias_manager);
    if (traceSpeculate && speculate)
    {
      DevWarn("WN_lower: found speculative expression: line %d",
	      Srcpos_To_Line(current_srcpos));
    }
    return speculate;
  }
  return FALSE;
}

/* ====================================================================
 *
 *
 * Create a Nary representation of the expression passed  
 *
 * ==================================================================== */

static WN *WN_Nary(WN *tree, WN *x0, WN *x1, WN *x2, WN *x3)
{
  TYPE_ID	type = WN_rtype(tree);
  OPCODE	op;
  INTRINSIC	id;

  op = OPCODE_make_op (OPR_INTRINSIC_OP, type, MTYPE_V);
 /*
  *  figure out id from tree
  */
  switch(WN_operator(tree))
  {
  case OPR_ADD:
  case OPR_SUB:
    id = INTRN_NARY_ADD;
    break;
  case OPR_MPY:
    id = INTRN_NARY_MPY;
    break;
  default:	
    Is_True(FALSE,("unexpected nary op"));
  }

  {
    WN		*args[4], **kids;
    INT16	i, n, argN;

   /*
    *  if the child is a nary op of the same type, integrate it
    */
    n= argN = 0;
    if (x0)
      args[n++] = x0;
    if (x1)
      args[n++] = x1;
    if (x2)
      args[n++] = x2;
    if (x3)
      args[n++] = x3;

    for(i=0; i<n; i++)
    {
      WN *wn = (WN *) args[i];

      if (WN_opcode(wn) == op && WN_intrinsic(wn) == id)
	argN += WN_kid_count(wn);
      else
        argN++;
    }

    kids = (WN **) alloca(argN * sizeof(WN *));

    for(i= argN= 0; i<n; i++)
    {
      WN *wn = args[i];

      if (WN_opcode(wn) == op && WN_intrinsic(wn) == id)
      {
	INT16  j;

	for(j=0; j< WN_kid_count(wn); j++)
	  kids[argN++] = WN_kid(wn, j);

	WN_Delete(wn);
      }
      else
      {
	kids[argN++] = wn;
      }
    }

    WN_Delete(tree);
    tree = WN_Create_Intrinsic(op, id, argN, kids);
  }
  return tree;
}





/* ====================================================================
 *
 * WN *WN_ExprToNary(WN *tree, TYPE_ID type)
 *
 * Create a Nary representation of the expression passed  
 *
 * The nary representation looks like an INTRINSIC_OP 	 
 * with an id = INTRN_NARY_ADD | INTRN_NARY_MPY		
 *
 * ==================================================================== */
extern WN *WN_ExprToNaryType(WN *tree, TYPE_ID type)
{
  WN	*l, *r;

  switch(WN_operator(tree))
  {
  case OPR_ADD:
  case OPR_MPY:
    l = WN_kid0(tree);
    r = WN_kid1(tree);

    if ((WN_opcode(tree) == WN_opcode(l)) &&
        (WN_opcode(tree) == WN_opcode(r)))
    {
      WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type);
      WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type);
      WN_kid0(r) = WN_ExprToNaryType(WN_kid0(r), type);
      WN_kid1(r) = WN_ExprToNaryType(WN_kid1(r), type);

      tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), WN_kid0(r), WN_kid1(r));
      WN_Delete(l);
      WN_Delete(r);
    }
    else if (WN_opcode(tree) == WN_opcode(l))
    {
      WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type);
      WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type);

      tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), r, NULL);
      WN_Delete(l);
    }
    else if (WN_opcode(tree) == WN_opcode(r))
    {
      WN_kid0(r) = WN_ExprToNaryType(WN_kid0(r), type);
      WN_kid1(r) = WN_ExprToNaryType(WN_kid1(r), type);

      tree = WN_Nary(tree, WN_kid0(r), WN_kid1(r), l, NULL);
      WN_Delete(r);
    }
    break;

  case OPR_SUB:
    {
      l = WN_kid0(tree);
      r = WN_kid1(tree);

      if (WN_operator_is(l, OPR_ADD))
      {
	TYPE_ID	rtype = WN_rtype(tree);

	WN  *neg = WN_Neg(rtype, r);

	WN_kid0(l) = WN_ExprToNaryType(WN_kid0(l), type);
	WN_kid1(l) = WN_ExprToNaryType(WN_kid1(l), type);

	tree = WN_Nary(tree, WN_kid0(l), WN_kid1(l), neg, NULL);
	WN_Delete(l);
      }
    }
    break;
  }
  return tree;
}



/* ====================================================================
 *
 * WN *WN_NaryToExpr(WN *tree)
 *
 * The nary representation looks like an INTRINSIC_OP 	 
 * with an id = INTRN_NARY_ADD | INTRN_NARY_MPY		
 *
 * ==================================================================== */
extern WN *WN_NaryToExpr(WN *tree)
{
  if (WN_nary_intrinsic(tree))
  {
    INT16	i;
    WN		*wn = WN_kid0(tree);
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
    TYPE_ID	rtype = WN_rtype(tree);
    INT 	num_parms = WN_kid_count(tree);

    for (i = 1; i < num_parms; i++)
    {
      WN *actual = WN_kid(tree, i);

      actual = WN_NaryToExpr(actual);

      switch(id)
      {
      case INTRN_NARY_ADD:
	if (WN_operator_is(actual, OPR_NEG))
	{
	  wn = WN_Sub(rtype, wn, WN_kid0(actual));
	  WN_Delete(actual);
	}
	else
	{
	  wn = WN_Add(rtype, wn, actual);
	}
        break;
      case INTRN_NARY_MPY:
	wn = WN_Mpy(rtype, wn, actual);
        break;
      }
    }
    return wn;
  }

  return tree;
}



/* ====================================================================
 *
 * WN *WN_NaryDelete(WN *tree, INT32 n)
 *
 * Delete the nth kid of a nary intrinsic op
 * The rest of the children get moved and the num_kids are updated
 *
 * ==================================================================== */

static WN *WN_NaryDelete(WN *tree, INT32 n)
{
  INT32	i;
  INT32	num_kids= WN_kid_count(tree);

  Is_True((n<num_kids),("cannot delete nth kid"));
  Is_True(WN_nary_intrinsic(tree),("expected nary op"));

  for(i=n+1; i<num_kids; i++)
  {
    WN_actual(tree, i-1) = WN_actual(tree, i);
  }
  WN_set_kid_count(tree, WN_kid_count(tree)-1);

  return tree;
}

/* ====================================================================
 *
 * Get offset field (avoid preg offsets)
 * amazing I cannot find anything like this
 *
 * ==================================================================== */

extern INT64 lower_offset(WN *tree, INT64 offset)
{
  if (WN_has_offset(tree))
  {
    switch(WN_operator(tree))
    {
    case OPR_LDA:
      offset +=	WN_lda_offset(tree);
      break;
    case OPR_MSTORE:
    case OPR_ISTORE:
    case OPR_ISTOREX:
      offset +=	WN_store_offset(tree);
      break;
    case OPR_STID:
      if (WN_class(tree) != CLASS_PREG)
        offset +=	WN_store_offset(tree);
      break;
    case OPR_LDID:
      if (WN_class(tree) != CLASS_PREG)
	offset +=	WN_load_offset(tree);
      break;
    case OPR_MLOAD:
    case OPR_ILOAD:
    case OPR_ILOADX:
      offset +=	WN_load_offset(tree);
      break;
    }
  }
  return offset;
}


/* ====================================================================
 *
 * Compute alignment consistent with address and offset
 *
 * The new alignment may be improved (or not. see pv [559228])
 *
 * ==================================================================== */
extern TY_IDX compute_alignment_type(WN *tree, TY_IDX type, INT64 offset)
{
  INT32 newAlign;

  newAlign=	compute_alignment(tree, lower_offset(tree, 0));

  newAlign=	compute_offset_alignment(offset, newAlign);

  if (TY_align(type) != newAlign)
    Set_TY_align(type, newAlign);

  return type;
}

static INT32 compute_alignment(WN *tree, INT64 offset)
{
  WN	  *addr;
  TY_IDX   type;
  INT32	   align, align0, align1;

  switch(WN_operator(tree))
  {
  case OPR_MSTORE:
    type = TY_pointed(Ty_Table[WN_ty(tree)]);
    addr = WN_kid1(tree);

    if (WN_has_sym(addr) && WN_ty(addr))
    {
      return compute_alignment(addr, offset+WN_lda_offset(addr));
    }
    align = TY_align(type);
    break;

  case OPR_MLOAD:
    type = TY_pointed(Ty_Table[WN_ty(tree)]);
    addr = WN_kid0(tree);

    if (WN_has_sym(addr) && WN_ty(addr))
    {
      return compute_alignment(addr, offset+WN_lda_offset(addr));
    }
    align = TY_align(type);
    break;

  case OPR_ILOAD:
  case OPR_ILOADX:
  case OPR_LDA:
  case OPR_LDID:
    type = WN_ty(tree);
    if (TY_is_pointer(Ty_Table[type]))
    {
      type = TY_pointed(Ty_Table[type]);
    }
    else
    {
      return 1;
    }
    align = TY_align(type);
    break;

  case OPR_ARRAY:
    align=	compute_alignment(WN_array_base(tree), offset);
    offset=	WN_element_size(tree);
    break;

  case OPR_ADD:
  case OPR_SUB:
    align0=	compute_alignment(WN_kid0(tree), 0);
    align1=	compute_alignment(WN_kid1(tree), 0);
    align=	MIN(align0, align1);
    break;

  case OPR_INTCONST:
    offset=	WN_const_val(tree);
    align=	MTYPE_alignment(Max_Uint_Mtype);
    break;

  default:
    if (traceAlignment)
    {
      DevWarn("compute_alignment(): unrecognized WN returning alignment of 1");
    }
    return 1;
  }

  align=	compute_offset_alignment(offset, MAX(1, align));

  if (WN_has_sym(tree))
  {
    INT32	newAlign = align;
    ST		*sym = WN_st(tree);

    if (WN_operator_is(tree, OPR_LDA))
    {
      newAlign=	ST_alignment(sym);
    }
    else if (WN_operator_is(tree, OPR_LDID)	&&
	     ST_type(sym)			&&
	     TY_is_pointer(Ty_Table[ST_type(sym)]))
    {
      newAlign = TY_align( TY_pointed(Ty_Table[ST_type(sym)]));
    }

    align = compute_offset_alignment(offset, MAX(newAlign, align));
  }
  return align;
}


/* ====================================================================
 *
 * BOOL lower_is_aliased(WN *wn1, WN *wn2, INT64 size)
 *
 * Are these addresses aliased? (used in bcopy/memcpy/memmove)
 *
 * ==================================================================== */
extern BOOL lower_is_aliased(WN *wn1, WN *wn2, INT64 size)
{
  if (alias_manager &&
      Valid_alias(alias_manager, wn1) &&
      Valid_alias(alias_manager, wn2) &&
      (Aliased(alias_manager, wn1, wn2) == NOT_ALIASED))
  {
    return FALSE;
  }

  if (WN_operator_is(wn1, OPR_LDA) && WN_operator_is(wn2, OPR_LDA)) {
    ST	*sym1 = WN_st(wn1);
    ST	*sym2 = WN_st(wn2);
    ST	*base1, *base2;
    INT64 newoffset1, newoffset2;

    if (sym1 != sym2) return FALSE;

    Base_Symbol_And_Offset_For_Addressing(
		    sym1, WN_lda_offset(wn1), &base1, &newoffset1);
    Base_Symbol_And_Offset_For_Addressing(
		    sym2, WN_lda_offset(wn2), &base2, &newoffset2);

    if (ABS(newoffset1 - newoffset2) >= size) return FALSE;
  }

  return TRUE;
}



/* ====================================================================
 *
 * WN *lower_copy_tree(WN *tree, INT32 n)
 *
 * Copy the tree and duplicate the maps
 *
 * ==================================================================== */

static void lower_tree_copy_maps(WN *tree, WN *dup, LOWER_ACTIONS actions)
{
  if (tree == NULL)
  {
    Is_True((dup == NULL),("inconsistency while copying trees"));
  }
  Is_True((WN_opcode(tree) == WN_opcode(dup)),
	  ("inconsistency while copying trees"));

  if (WN_has_map_id(tree) && WN_has_alias_info(tree))
  {
    lower_copy_maps(tree, dup, actions);
  }
	
  if (WN_opcode(tree) == OPC_BLOCK)
  {
    WN  *treeStmt = WN_first(tree);
    WN  *dupStmt  = WN_first(dup);

    while(treeStmt)
    {
      lower_tree_copy_maps(treeStmt, dupStmt, actions);
      treeStmt = WN_next(treeStmt);
      dupStmt  = WN_next(dupStmt);
    }
  }
  else
  {
    INT	n;
    for(n = 0; n < WN_kid_count(tree); n++)
    {
      if (WN_kid(tree, n))
      {
	lower_tree_copy_maps(WN_kid(tree,n), WN_kid(dup,n), actions);
      }
    }
  }
}

static WN *lower_copy_tree(WN *tree, LOWER_ACTIONS actions)
{
  WN  *dup;

  dup = WN_COPY_Tree(tree);

  lower_tree_copy_maps(tree, dup, actions);

  return dup;
}

/* ====================================================================
 *
 * PREG_NUM AssignPregExprPos(WN *block, WN *tree, TY_IDX ty, SRCPOS srcpos,
 *                            LOWER_ACTIONS actions)
 *
 * PREG_NUM AssignExprPos(WN *block, WN *tree, TYPE_ID type, SRCPOS srcpos,
 *                        LOWER_ACTIONS)
 *
 * PREG_NUM AssignExpr(WN *block, WN *tree, TYPE_ID type)
 *
 * PREG_NUM AssignExprTY(WN *block, WN *tree, TY_IDX ty)
 *
 * Allocate a preg of type ST_type(preg) and assign expression tree to it
 * and attach it to block. 
 *
 * Assign srcpos (if not NULL)
 *
 * ==================================================================== */

static PREG_NUM AssignPregExprPos(WN *block, WN *tree, TY_IDX ty,
				  SRCPOS srcpos, LOWER_ACTIONS actions)
{
  PREG_NUM	pregNo;
  TYPE_ID	type;
  ST		*preg = MTYPE_To_PREG(TY_mtype(Ty_Table[ty]));

  Is_True((WN_operator_is(tree, OPR_PARM)==FALSE),("bad parm"));

  type = TY_mtype(Ty_Table[ty]);
  pregNo = Create_Preg(type, current_preg_name);

  {
    WN	*stBlock, *stid;

    stid = WN_Stid(type, pregNo, preg, ty, tree);

    if (srcpos)
      WN_Set_Linenum (stid, srcpos);

    stBlock = WN_CreateBlock();

   /*
    *	This lowering may leed to infinite regress if the
    * 	children cannot be lowered (and are allocated a temp, for example) 
    */
    if (actions)
      stid = lower_store(stBlock, stid, actions);

    WN_INSERT_BlockLast(stBlock, stid);

    WN_INSERT_BlockLast(block, stBlock);
  }

  return pregNo;
}


extern PREG_NUM AssignExprTY(WN *block, WN *tree, TY_IDX type)
{
  return AssignPregExprPos(block, tree, type, current_srcpos,
			   current_actions);
}

extern PREG_NUM AssignExpr(WN *block, WN *tree, TYPE_ID type)
{
  return AssignPregExprPos(block, tree, MTYPE_To_TY(type), current_srcpos,
			   current_actions);
}

extern void AssignExprToPreg(WN *block, WN *tree, PREG_NUM destNo, 
			     TYPE_ID destType)
{
  TY_IDX        ty = MTYPE_To_TY(destType);
  ST		*preg = MTYPE_To_PREG(destType);

  Is_True((WN_operator_is(tree, OPR_PARM)==FALSE),("bad parm"));

  {
    WN	*stBlock, *stid;

    stid = WN_Stid(destType, destNo, preg, ty, tree);

    if (current_srcpos)
      WN_Set_Linenum (stid, current_srcpos);

    stBlock = WN_CreateBlock();

   /*
    *	This lowering may leed to infinite regress if the
    * 	children cannot be lowered (and are allocated a temp, for example) 
    */
    if (current_actions)
      stid = lower_store(stBlock, stid, current_actions);

    WN_INSERT_BlockLast(stBlock, stid);

    WN_INSERT_BlockLast(block, stBlock);
  }

  return;
}



static BOOL WN_unconditional_goto(WN *tree)
{
  switch(WN_operator(tree))
  {
  case OPR_GOTO:
  case OPR_REGION_EXIT:
  case OPR_RETURN:
  case OPR_RETURN_VAL:
    return TRUE;
  }
  return FALSE;
}




/* ====================================================================
 *
 * PARITY WN_parity(WN *tree)
 *
 * return the PARITY associated with a tree.
 *
 * Parity encapsulates dependence information, like complex real, imag
 * ==================================================================== */

PARITY WN_parity(WN *tree)
{
  if (WN_map_id(tree) != -1)
  {
    INT32	map;

    Is_True((lowering_parity_map != 0), ("parity map not initialized"));
    map = WN_MAP32_Get(lowering_parity_map, tree);
   
    if (map!=0)
      return (PARITY) map;
  }
  return PARITY_UNKNOWN;
}

BOOL WN_parity_independent(WN *wn1, WN *wn2)
{

  if (wn1 == NULL		||
      wn2 == NULL		||
      WN_map_id(wn1) == -1	||
      WN_map_id(wn2) == -1)
    return FALSE;

  {
    PARITY p1 = WN_parity(wn1);
    PARITY p2 = WN_parity(wn2);

   return (p1 & p2) ? FALSE : TRUE;
  }
}




/* ====================================================================
 *
 *	MAP PRESERVATION
 *
 *
 * void lower_copy_maps(WN *orig, WN *tree, LOWER_ACTIONS action)
 *
 * void lower_complex_maps(WN *orig, WN *real, WN *imag, LOWER_ACTIONS action)
 *
 * void lower_quad_maps(WN *orig, WN *hipart, WN *lopart, LOWER_ACTIONS action)
 *
 *
 *  copy alias information to tree
 *
 * ==================================================================== */

static void lower_maps_init(LOWER_ACTIONS actions)
{

}

static void lower_maps_reset(LOWER_ACTIONS actions)
{
  if (Action(LOWER_DEPGRAPH_MAPS))
  {
    LNOPruneMapsUsingParity();
  }
}

static void lower_map(WN *tree, LOWER_ACTIONS actions)
{
  if (Action(LOWER_ALIAS_MAPS))
  {
    if (alias_manager)
    {
      if (Valid_alias(alias_manager, tree) == FALSE)
	Create_alias(alias_manager, tree);
    }
  }
}

static void lower_copy_maps(WN *orig, WN *tree, LOWER_ACTIONS actions)
{
  if (orig == NULL)
    return;

 /*
  *	The tree may no longer be valid at the point of call
  *	(ie. may be deleted) so we must check validity
  */
  if (WN_has_map_id(orig))
  {
    if (WN_has_alias_info(orig)	&&
        WN_has_alias_info(tree))
    {
      if (Action(LOWER_PREFETCH_MAPS))
      {
        WN_CopyMap(tree, WN_MAP_PREFETCH, orig);
      }
      if (Action(LOWER_ALIAS_MAPS))
      {
        if (alias_manager)
	  Copy_alias_info(alias_manager, orig, tree);
      }
    }
  }
  if (Action(LOWER_DEPGRAPH_MAPS))
  {
    LNOPreserveMap(orig, tree);
  }
  WN_Set_Flags(orig, tree);
}


/*
 * If an original node has a TY with an f90_pointer attribute on it, copy it
 * to the TYs of the new nodes. node2 might be NULL (for cases in which we
 * only produce one new node)
 */
static void lower_copy_tys (WN *orig, WN *node1, WN *node2) 
{
  TY_IDX  ty;

  if (WN_operator_is(orig, OPR_ILOAD))
  {
    ty =	WN_load_addr_ty(orig);

    if (TY_is_f90_pointer(Ty_Table[ty]))
    {
      if (node1 && WN_operator_is(node1, OPR_ILOAD))
	WN_set_load_addr_ty(node1, ty);

      if (node2 && WN_operator_is(node2, OPR_ILOAD))
	WN_set_load_addr_ty(node2, ty);
    }
  }
  else if (WN_operator_is(orig, OPR_ISTORE))
  {
    ty =	WN_ty(orig);
    if (TY_is_f90_pointer(Ty_Table[ty]))
    {
      if (node1 && WN_operator_is(node1, OPR_ISTORE))
	WN_set_ty(node1, ty);
      if (node2 && WN_operator_is(node2, OPR_ISTORE))
	WN_set_ty(node2, ty);
    }
  }
}

static void lower_paired_maps( WN *orig, WN *x, WN *y,
			       PARITY x_parity, PARITY y_parity,
			       LOWER_ACTIONS actions )
{
  lower_copy_maps(orig, x, actions);
  lower_copy_maps(orig, y, actions);
  lower_copy_tys(orig,x,y);

  if (Action(LOWER_PARITY_MAPS))
  {
    WN_MAP32_Set(lowering_parity_map, x, x_parity);
    WN_MAP32_Set(lowering_parity_map, y, y_parity);
  }
}


static void lower_quad_maps(WN *orig, WN *hipart, WN *lopart,
			    LOWER_ACTIONS actions)
{
  lower_paired_maps( orig, hipart, lopart, PARITY_QUAD_HI, PARITY_QUAD_LO,
		     actions );
}

static void lower_paired_maps(WN *orig, WN *hipart, WN *lopart,
			    LOWER_ACTIONS actions)
{
  lower_paired_maps( orig, hipart, lopart, PARITY_DOUBLE_HI, PARITY_DOUBLE_LO,
		     actions );
}

static void lower_complex_maps(WN *orig, WN *real, WN *imag,
			       LOWER_ACTIONS actions)
{
  lower_paired_maps( orig, real, imag, PARITY_COMPLEX_REAL, PARITY_COMPLEX_IMAG,
		     actions );
}

static WN *add_to_base(WN **base, WN *tree)
{
  if (*base)
  {
    return WN_Add(Pointer_type, *base, tree);
  }
  return tree;
}

static WN *sub_from_base(WN **base, WN *tree)
{
  if (*base)
  {
    return WN_Sub(Pointer_type, *base, tree);
  }
  return tree;
}

static BOOL baseAddress(WN *tree)
{
  switch(WN_operator(tree))
  {
  case OPR_LDA:
  case OPR_LDID:
  case OPR_ILOAD:
  case OPR_ILOADX:
    return TRUE;
  }
  return FALSE;
}


/* ====================================================================
 *
 * void lower_to_base_index(WN *addr, WN **base, WN **index) 
 *
 * Pattern match an address and create a bad/index for it
 *
 * Before this routine we would store the address in a preg
 * and use it (twice).  This would require the ST being marked
 * addr_taken_stored (very bad)
 *
 * This is getting worse and worse. Now the routine is recursive
 * for OPR_ADD.
 * 
 * 17 Dec 1998, R. Shapiro - Add a default case so that this routine never 
 *   fails. At worst, the trees get a little bigger. 
 *
 * ==================================================================== */

static void lower_to_base_index(WN *addr, WN **base, WN **index) 
{
  WN	*l, *r;

  switch (WN_operator(addr))
  {
  case OPR_ARRAY:
    *base  = add_to_base(base, WN_array_base(addr));
    *index = add_to_base(index, addr);
    WN_array_base(addr) = WN_Zerocon(WN_rtype(addr));
    break;

  case OPR_ADD:
    l = WN_kid0(addr);
    r = WN_kid1(addr);

    if (baseAddress(l))
    {
      *base  = add_to_base(base, l);
      *index = add_to_base(index, r);
    }
    else if (baseAddress(r))
    {
      *base  = add_to_base(base, r);
      *index = add_to_base(index, l);
    }
    else if (WN_operator_is(r, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(r));
      WN_array_base(r) = WN_Zerocon(WN_rtype(r));
      *index = add_to_base(index, addr);
    }
    else if (WN_operator_is(l, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(l));
      WN_array_base(l) = WN_Zerocon(WN_rtype(l));
      *index = add_to_base(index, addr);
    }
    else if ((WN_operator_is(r, OPR_ADD)  ||  WN_operator_is(r, OPR_SUB))
	     && WN_operator_is(l, OPR_INTCONST))
    {
      lower_to_base_index(r, base, index);
      *index = add_to_base(index, l);
    }
    else if ((WN_operator_is(l, OPR_ADD)  ||  WN_operator_is(l, OPR_SUB)) 
	     && WN_operator_is(r, OPR_INTCONST))
    {
      lower_to_base_index(l, base, index);
      *index = add_to_base(index, r);
    }
    else
    {
      // Give up
      *base  = add_to_base(base, addr);
      *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    }
    break;

  case OPR_SUB:
    l = WN_kid0(addr);
    r = WN_kid1(addr);

    if (baseAddress(l))
    {
      *base  = add_to_base(base, l);
      *index = sub_from_base(index, r);
    }
    else if (baseAddress(r))
    {
      *base  = sub_from_base(base, r);
      *index = add_to_base(index, l);
    }
    else if (WN_operator_is(r, OPR_ARRAY))
    {
      *base  =	sub_from_base(base, WN_array_base(r));
      WN_array_base(r) = WN_Zerocon(WN_rtype(r));
      *index =	add_to_base(index, addr);
    }
    else if (WN_operator_is(l, OPR_ARRAY))
    {
      *base  = add_to_base(base, WN_array_base(l));
      WN_array_base(l) = WN_Zerocon(WN_rtype(l));
      *index = add_to_base(index, addr);
    }
    else if ((WN_operator_is(r, OPR_ADD) ||  WN_operator_is(r, OPR_SUB))
	     && WN_operator_is(l, OPR_INTCONST))
    {
      lower_to_base_index(r, base, index);
      *base  = WN_Neg(WN_rtype(*base),*base);
      *index = sub_from_base(&l,*index);
    }
    else if ((WN_operator_is(l, OPR_ADD) || WN_operator_is(l, OPR_SUB)) 
	     && WN_operator_is(r, OPR_INTCONST))
    {
      lower_to_base_index(l, base, index);
      *index = sub_from_base(index, r);
    }
    else
    {
      // Give up
      *base  = add_to_base(base, addr);
      *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    }
    break;
  default:
    // Give up
    *base  = add_to_base(base, addr);
    *index = add_to_base(index, WN_Zerocon(WN_rtype(addr)));
    break;
  }
}


/*
 * Describe a leaf expression (see Make_Leaf/Load_Leaf).
 */
typedef enum {LEAF_IS_CONST, LEAF_IS_INTCONST, LEAF_IS_PREG} LEAF_KIND;
typedef struct {
  LEAF_KIND kind;
  TYPE_ID type;
  union {
    PREG_NUM n;
    INT64 intval;
    TCON tc;
  } u;
} LEAF;

/* ====================================================================
 *
 * LEAF Make_Leaf(WN *block, WN *tree, TYPE_ID type)
 *
 * Make an aribtrary expression tree into a leaf.
 * If the expression is an integer or floating point constant
 * no transformation is made. However, other expressions are stored
 * into a PREG.
 *
 * Make_Leaf is used in place of AssignExpr when performing a sort
 * of "poor man's" CSE, and you want to avoid creating unnecessary
 * PREGs for constants (which can also thwart the simplifier).
 *
 * ==================================================================== */

static LEAF Make_Leaf(WN *block, WN *tree, TYPE_ID type)
{
  LEAF leaf;
  leaf.type = type;
  switch (WN_operator(tree)) {
  case OPR_CONST:
    leaf.kind = LEAF_IS_CONST;
    leaf.u.tc = Const_Val(tree);
    WN_Delete(tree);
    break;
  case OPR_INTCONST:
    leaf.kind = LEAF_IS_INTCONST;
    leaf.u.intval = WN_const_val(tree);
    WN_Delete(tree);
    break;
  default:
    leaf.kind = LEAF_IS_PREG;
    leaf.u.n = AssignExpr(block, tree, type);
    break;
  }
  return leaf;
}

/* ====================================================================
 *
 * WN *Load_Leaf(const LEAF &leaf)
 *
 * Generate whirl to load the value of a leaf expression created by
 * Make_Leaf().
 *
 * ==================================================================== */

static WN *Load_Leaf(const LEAF &leaf)
{
  switch (leaf.kind) {
  case LEAF_IS_CONST:
    return Make_Const(leaf.u.tc);
  case LEAF_IS_INTCONST:
    return WN_CreateIntconst(OPR_INTCONST, leaf.type, MTYPE_V, leaf.u.intval);
  case LEAF_IS_PREG:
    return WN_LdidPreg(leaf.type, leaf.u.n);
  }
  FmtAssert(FALSE, ("unhandled leaf kind in Load_Leaf"));
  /*NOTREACHED*/
}

/* ====================================================================
 *
 * void lower_complex_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				 WN **realpart, WN **imagpart)
 *
 * Split complex-type expression <tree> into its lowered real and
 * imaginary parts.  <actions> must include LOWER_COMPLEX.  Note that
 * like the other lowering functions, this one destroys <tree>.  Upon
 * return, *<realpart> points to the real part, and *<imagpart> points
 * to the imaginary part.
 *
 * ==================================================================== */

static void lower_complex_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
			       WN **realpart, WN **imagpart)
{
  TYPE_ID	type;


  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(Action(LOWER_COMPLEX), ("actions does not contain LOWER_COMPLEX"));
  Is_True((MTYPE_is_complex(WN_rtype(tree))),
	  ("expected complex-type node, not %s",
	   OPCODE_name(WN_opcode(tree))));
  type = Mtype_complex_to_real(WN_rtype(tree));

 /*
  *  Complex Arithmetic Notation
  *
  *  Let R(c) = real part of c
  *      I(c) = imaginary part of c
  *
  *  then, if z is complex it can be represented as follows
  *
  *      z= R(z) + I(z)i
  *
  */
  switch (WN_operator(tree))
  {
  case OPR_LDID:
    {
     /*
      *  we must create a new ST of type real
      *  we implicitly assume the storage of complex is
      *		 z =  {  R(z) , I(z) }
      */
      TY_IDX    beTY   = MTYPE_To_TY(type);
      WN_OFFSET offset = WN_load_offset(tree);

      if (WN_class(tree) == CLASS_CONST && offset == 0)
      {
	TCON	val = WN_val(tree);
	TYPE_ID	valType = WN_val_type(tree);

	if (WN_rtype(tree) == valType)
	{
	  *realpart = Make_Const( Extract_Complex_Real(val));
	  *imagpart = Make_Const( Extract_Complex_Imag(val));
 	  break;
	}
      }

      *realpart = WN_Ldid(type, offset,
			  coerceST(WN_st(tree), type),
			  coerceTY(WN_ty(tree), type));

      *imagpart = WN_Ldid(type,
			  coerceOFFSET(tree, type, offset),
			  coerceST(WN_st(tree), type),
			  coerceTY(WN_ty(tree), type));
    }
    break;

  case OPR_ILOAD:
    {
     /*
      *  we implicitly assume the storage of complex is
      *    z =  {  R(z) , I(z) }
      *
      *  The LOWER_BASE_INDEX will try to split the address into a
      *  base and index. The index is put in a preg (and reused) while
      *  the base is cloned.
      */
      WN_OFFSET offset = WN_load_offset(tree);

      if (Action(LOWER_BASE_INDEX))
      {
	WN	*addr, *base, *index;
	LEAF	indexN;

	base = index=	NULL;
	lower_to_base_index(WN_kid0(tree), &base, &index) ;

	base = lower_expr(block, base, actions);
	index = lower_expr(block, index, actions);

	indexN = Make_Leaf(block, index, Pointer_type);
	
	addr = WN_Add(Pointer_type,
		      Load_Leaf(indexN),
		      lower_copy_tree(base, actions)); 

	*realpart = WN_Iload(type,
			     offset,
			     coerceTY(WN_ty(tree), type),
			     addr);

	addr = WN_Add(Pointer_type, Load_Leaf(indexN), base);

	*imagpart = WN_Iload(type,
			     coerceOFFSET(tree, type, offset),
			     coerceTY(WN_ty(tree), type),
			     addr);
      }
      else
      {
	WN	*addr;
	LEAF	addrN;
	
	addr = lower_expr(block, WN_kid0(tree), actions);
	
	addrN = Make_Leaf(block, addr, Pointer_type);
	
	*realpart = WN_Iload(type,
			     offset,
			     coerceTY(WN_ty(tree), type),
			     Load_Leaf(addrN));
	
	*imagpart = WN_Iload(type,
			     coerceOFFSET(tree, type, offset),
			     coerceTY(WN_ty(tree), type),
			     Load_Leaf(addrN));
      }
      lower_complex_maps(tree, *realpart, *imagpart, actions);
    }
    break;

  case OPR_ILOADX:
    Is_True(FALSE, ("unexpected complex ILOADX"));
    break;

  case OPR_NEG:
    {
     /*
      *    -z  = -R(z) + -I(z)i
      */
      WN	*rz, *iz;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      *realpart = WN_Neg( type, rz);
      *imagpart = WN_Neg( type, iz);

    }
    break;

  case OPR_ADD:
    {
      WN	*rz, *rw, *iz, *iw;
     /*
      *    z + w = (R(z) + R(w)) + (I(z) + I(w))i
      */

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
 
      *realpart = WN_Add( type, rz, rw);
      *imagpart = WN_Add( type, iz, iw);
    }
    break;

  case OPR_SUB:
    {
     /*
      *    z - w = (R(z) - R(w)) + (I(z) - I(w))i
      */
      WN	*rz, *rw, *iz, *iw;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
 
      *realpart = WN_Sub( type, rz, rw);
      *imagpart = WN_Sub( type, iz, iw);
    }
    break;

  case OPR_MPY:
    {
     /*
      *    z * w = (R(z)*R(w) - I(z)*I(w)) + (R(z)*I(w)+ R(w)*I(z))i
      *
      */
      WN	*rz, *rw, *iz, *iw;
      LEAF	rzN, rwN, izN, iwN;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);

      rzN = Make_Leaf(block, rz, type);
      rwN = Make_Leaf(block, rw, type);
      izN = Make_Leaf(block, iz, type);
      iwN = Make_Leaf(block, iw, type);

      *realpart = WN_Sub(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rwN)),
			 WN_Mpy(type, Load_Leaf(izN), Load_Leaf(iwN)));

      *imagpart = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(iwN)),
			 WN_Mpy(type, Load_Leaf(rwN), Load_Leaf(izN)));
    }
    break;

  case OPR_COMPLEX:
    /*
     *	Create a complex number from two real children, ie.
     *    z = CMPLX(x,y)
     *    z = (x) + (y) i
     */
    *realpart = lower_expr(block, WN_kid0(tree), actions);
    *imagpart = lower_expr(block, WN_kid1(tree), actions);
    WN_Delete(tree);
    break;

  case OPR_MADD:
  case OPR_MSUB:
  case OPR_NMADD:
  case OPR_NMSUB:
    Is_True( FALSE, ("unexpected complex madd"));
    break;

  case OPR_CVT:
    {
      WN	*rz, *iz;
      TYPE_ID	desc = WN_desc(tree);

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
 
      *realpart = WN_Cvt( type, Mtype_complex_to_real(desc), rz);
      *imagpart = WN_Cvt( type, Mtype_complex_to_real(desc), iz);
    }
    break;

  case OPR_CONST:
    {
     /*
      *	extract the real and imaginary parts of the complex number
      */
      TCON	val = Const_Val(tree);

      *realpart = Make_Const( Extract_Complex_Real(val));
      *imagpart = Make_Const( Extract_Complex_Imag(val));

      WN_Delete(tree);
    }
    break;

  case OPR_RSQRT:
    {
      TYPE_ID	desc = WN_desc(tree);
      WN	*div;

      div = WN_Div(desc,
		   WN_Floatconst(desc, 1.0),
		   WN_Sqrt(desc, WN_kid0(tree)));

      lower_complex_expr(block, div, actions, realpart, imagpart);

      WN_Delete(tree);
    }
    break;

  case OPR_SQRT:
    lower_complex_emulation(block, tree, actions, realpart, imagpart);
    break;


  case OPR_PAREN:
    {
      lower_complex_expr(block, WN_kid0(tree), actions, realpart, imagpart);

      *realpart = WN_Paren(type, *realpart);
      *imagpart = WN_Paren(type, *imagpart);
      WN_Delete(tree);
    }
    break;

  case OPR_ARRAY:
    break;

  case OPR_RECIP:
    {
      /* TODO_FEEDBACK: Get frequencies right, especially when we
       * create an IF statement. We have to assume half the frequency
       * goes to the then part and half goes to the else part; there's
       * no other information available, is there?
       */

      LEAF	rzN, izN;
      {
       /*
	*  assign pregs to their corresponding expressions
	*  Since the expressions will be reused, this avoids building a DAG
	*/
	WN	*rz, *iz; 
    
	lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      
	rzN = Make_Leaf(block, rz, type);
	izN = Make_Leaf(block, iz, type);
      }
    
      if (Fast_Complex_Allowed)
      {
       /*
	*   1 / z
	*    real =	(   R(z) ) /  ( R(z)**2 + I(z)**2 )
	*    imag =	( - I(z) ) /  ( R(z)**2 + I(z)**2 )
	*
	*/
	LEAF	denomN;
	WN	*rz2, *iz2, *add;
    
	rz2 = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rzN));
	iz2 = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(izN));
	add = WN_Add(type, rz2, iz2);
	denomN = Make_Leaf(block, add, type);
    
	*realpart = WN_Div(type, Load_Leaf(rzN), Load_Leaf(denomN));
	*imagpart = WN_Neg(type,
			   WN_Div(type, Load_Leaf(izN), Load_Leaf(denomN)));
      }
      else
      {
       /*
	*   1 / z
	*    real =	(   R(z) ) /  ( R(z)**2 + I(z)**2 )
	*    imag =	( - I(z) ) /  ( R(z)**2 + I(z)**2 )
	*
	*    After factoring out max( |R(z)| , |I(z)| )
	*
	*	| R(z) | >  | I(z) |
	*
	*	Let x = I(z) / R(z)
	*	real = ( 1 )    /  ( R(z) + I(z)*x)
	*	imag = ( - x )  /  ( R(z) + I(z)*x)
	*
	*	| I(z) | >  | R(z) |
	*
	*	Let x = R(z) / I(z)
	*	real = ( x )   /  ( R(z)*x + I(z) )
	*	imag = (-1 )   /  ( R(z)*x + I(z) )
	*
	*/
	WN		*if_then, *if_else, *IF;
	PREG_NUM	realpartN, imagpartN;
	LEAF		xN, arzN, aizN;
    
	{
	  WN	*numer, *denom, *div;
    
	  arzN = Make_Leaf(block,
			   WN_Abs(type, Load_Leaf(rzN)),
    			   type);
    
	  aizN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(izN)),
    			   type);
    
	 /*
	  *  numer =  | R(w) | >  | I(w) |  ?  I(w) : R(x)
	  *  denom =  | R(w) | >  | I(w) |  ?  R(w) : I(x)
	  *
	  *  Let x = numer / denom
	  */
	  numer = WN_Select(type,
			    WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
    			    Load_Leaf(izN),
    			    Load_Leaf(rzN));
	  denom = WN_Select(type,
			    WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
    			    Load_Leaf(rzN),
    		  	    Load_Leaf(izN));

	  div = WN_Div(type, numer, denom);

	  xN = Make_Leaf(block, div, type);
	}

	if_then = WN_CreateBlock();
	{
	 /*
	  *	scale = ( R(z) + I(z)*x )
	  *	real = ( 1 )    /  scale
	  *	imag = ( - x )  /  scale
	  */
	  WN	*scale, *div;
	  LEAF	scaleN;
    
	  scale = WN_Add(type,
			 Load_Leaf(rzN),
			 WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)));
	  scaleN = Make_Leaf(if_then, scale, type);

	  div = WN_Inverse(type, Load_Leaf(scaleN));

	  realpartN = AssignExpr(if_then, div, type);

	  div =  WN_Div(type,
			WN_Neg(type, Load_Leaf(xN)),
			Load_Leaf(scaleN));
    
	  imagpartN = AssignExpr(if_then, div, type);
	}

	if_else = WN_CreateBlock();
	{
	 /*
	  *	scale =	( R(z)*x + I(z) )
	  *	real = ( x )   /  scale
	  *	imag = ( 1 )   /  scale
	  */
	  WN	*scale, *div, *stid;
	  LEAF	scaleN;
	  ST	*preg =	MTYPE_To_PREG(type);
    
	  scale = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)),
			 Load_Leaf(izN));
	  scaleN = Make_Leaf(if_else, scale, type);

	  div =  WN_Div(type, Load_Leaf(xN), Load_Leaf(scaleN));
	  stid = WN_StidIntoPreg(type, realpartN, preg, div);
	  WN_copy_linenum(tree, stid);
	  WN_INSERT_BlockLast(if_else, stid);
    
	  div = WN_Neg(type,WN_Inverse(type, Load_Leaf(scaleN)));
	  stid = WN_StidIntoPreg(type, imagpartN, preg, div);
	  WN_copy_linenum(tree, stid);
	  WN_INSERT_BlockLast(if_else, stid);
	}
    
	IF =  WN_CreateIf(WN_GT(type, Load_Leaf(arzN), Load_Leaf(aizN)),
			  if_then, if_else);

	if (Cur_PU_Feedback) {
	  FB_Info_Branch info_branch(FB_FREQ(1.0,FB_FREQ_TYPE_GUESS),FB_FREQ_UNKNOWN); 
	  Cur_PU_Feedback->Annot_branch(IF, info_branch);
	}

	WN_INSERT_BlockLast(block, lower_if(block, IF, actions));
    
	*realpart = WN_LdidPreg(type, realpartN);
	*imagpart = WN_LdidPreg(type, imagpartN);
      }
      WN_Delete(tree);
    }
    break;

  case OPR_DIV:
    {
     /*
      *  assign pregs to their corresponding expressions
      *  Since the expressions will be reused, this avoids building a DAG
      */
      WN	*rz, *rw, *iz, *iw; 
      LEAF	rzN, rwN, izN, iwN;

      lower_complex_expr(block, WN_kid0(tree), actions, &rz, &iz);
      lower_complex_expr(block, WN_kid1(tree), actions, &rw, &iw);
  
      rzN = Make_Leaf(block, rz, type);
      izN = Make_Leaf(block, iz, type);
      rwN = Make_Leaf(block, rw, type);
      iwN = Make_Leaf(block, iw, type);

      if (Fast_Complex_Allowed)
      {
       /*
	*   z / w
	*    real =	(R(z)*R(w) + I(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*    imag =	(I(z)*R(w) - R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*
	*/
	LEAF	denomN;
	{
	  WN	*rw2, *iw2, *add;
    
	  rw2 = WN_Mpy(type,Load_Leaf(rwN), Load_Leaf(rwN));
	  iw2 = WN_Mpy(type, Load_Leaf(iwN), Load_Leaf(iwN));
	  add = WN_Add(type, rw2, iw2);
	  denomN = Make_Leaf(block, add, type);
	}
	{
	  WN	*rzrw, *iziw;
    
	  rzrw = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(rwN));
	  iziw = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(iwN));
	  *realpart = WN_Div(type,
    			 WN_Add(type, rzrw, iziw),
    			 Load_Leaf(denomN));
	}
	{
	  WN	*rziw, *izrw;
    
	  izrw = WN_Mpy(type, Load_Leaf(izN), Load_Leaf(rwN));
	  rziw = WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(iwN));
	  *imagpart = WN_Div(type,
    			 WN_Sub(type, izrw, rziw),
    			 Load_Leaf(denomN));
	}
      }
      else
      {
       /*
	*   z / w
	*    real =	(R(z)*R(w) + I(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*    imag =	(I(z)*R(w) - R(z)*I(w) /  ( R(w)**2 + I(w)**2 )
	*
	*    After factoring out max( |R(w)| , |I(w)| )
	*
	*	| R(w) | >  | I(w) |
	*
	*	Let x = I(w) / R(w)
	*	  real = ( R(z) + I(z)*x ) /  ( R(w) + I(w)*x )
	*	  imag = ( I(z) - R(z)*x ) /  ( R(w) + I(w)*x )
	*
	*	| I(w) | >  | R(w) |
	*
	*	Let x = R(w) / I(w)
	*	  real = ( R(z)*x + I(z) ) /  ( R(w)*x + I(w) )
	*	  imag = ( I(z)*x - R(z) ) /  ( R(w)*x + I(w) )
	*
	*/
	WN		*if_then, *if_else, *IF;
	LEAF		xN, arwN, aiwN;
	PREG_NUM	realpartN, imagpartN;
	{
	  WN	*numer, *denom, *div;
    
	  arwN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(rwN)),
    			   type);
	  aiwN = Make_Leaf(block,
    			   WN_Abs(type, Load_Leaf(iwN)),
    			   type);
    
	 /*
	  *  numer =  | R(w) | >  | I(w) |  ?  I(w) : R(x)
	  *  denom =  | R(w) | >  | I(w) |  ?  R(w) : I(x)
	  *
	  *  Let x = numer / denom
	  */
	  numer = WN_Select(type,
			    WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
    			    Load_Leaf(iwN),
			    Load_Leaf(rwN));
	  denom = WN_Select(type,
			    WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
    			    Load_Leaf(rwN),
    		 	    Load_Leaf(iwN));
	  div = WN_Div(type, numer, denom);
	  xN = Make_Leaf(block, div, type);
	}

	if_then = WN_CreateBlock();
	{
	  WN		*scale;
	  LEAF	scaleN;
    
	  scale = WN_Add(type,
			 Load_Leaf(rwN),
			 WN_Mpy(type, Load_Leaf(iwN), Load_Leaf(xN)));
	  scaleN = Make_Leaf(if_then, scale, type);
	  {
	   /*
    	    *  real = ( R(z)   + I(z)*x )  / scale
    	    */
    	    WN	*numer, *div;
    
    	    numer = WN_Add(type,
  			   Load_Leaf(rzN),
  			   WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    realpartN = AssignExpr(if_then, div, type);
	  }
	  {
	   /*
	    *  imag = ( I(z) - R(z)*x ) /  scale
    	    */
    	    WN	*numer, *div;
    
	    numer = WN_Sub(type,
    			   Load_Leaf(izN),
			   WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
    	    imagpartN = AssignExpr(if_then, div, type);
	  }
	}
    
	if_else = WN_CreateBlock();
	{
	  WN	*scale;
	  LEAF	scaleN;
	  ST	*preg =	MTYPE_To_PREG(type);

	  scale = WN_Add(type,
			 WN_Mpy(type, Load_Leaf(rwN), Load_Leaf(xN)),
    			 Load_Leaf(iwN));
	  scaleN = Make_Leaf(if_else, scale, type);
	  {
	   /*
    	    *  real = ( R(z)*x + I(z) ) /  scale
    	    *  store away result in an already defined preg
    	    */
    	    WN	*numer, *div, *stid;
    
    	    numer =  WN_Add(type,
    			    WN_Mpy(type, Load_Leaf(rzN), Load_Leaf(xN)),
    			    Load_Leaf(izN));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    stid = WN_StidIntoPreg(type, realpartN, preg, div);
	    WN_copy_linenum(tree, stid);
	    WN_INSERT_BlockLast(if_else, stid);
	  }
	  {
	   /*
	    *  imag = ( I(z)*x - R(z) ) /  scale
    	    *  store away result in an already defined preg
    	    */
    	    WN	*numer, *div, *stid;
    
    	    numer = WN_Sub(type,
    			   WN_Mpy(type, Load_Leaf(izN), Load_Leaf(xN)),
    			   Load_Leaf(rzN));
	    div = WN_Div(type, numer, Load_Leaf(scaleN));
	    stid = WN_StidIntoPreg(type, imagpartN, preg, div);
	    WN_copy_linenum(tree, stid);
	    WN_INSERT_BlockLast(if_else, stid);
	  }
	}
    
	IF =  WN_CreateIf(WN_GT(type, Load_Leaf(arwN), Load_Leaf(aiwN)),
			  if_then, if_else);

	if (Cur_PU_Feedback) {
	  FB_Info_Branch info_branch(FB_FREQ(1.0,FB_FREQ_TYPE_GUESS),FB_FREQ_UNKNOWN); 
	  Cur_PU_Feedback->Annot_branch(IF, info_branch);
	}
    
	WN_INSERT_BlockLast(block, lower_if(block, IF, actions));
    
	*realpart = WN_LdidPreg(type, realpartN);
	*imagpart = WN_LdidPreg(type, imagpartN);
      }
      WN_Delete(tree);
    }
    break;

  case OPR_RND:
  case OPR_TRUNC:
  case OPR_MOD:
  case OPR_REM:
  case OPR_ABS:
    Fail_FmtAssertion("unexpected complex op (%s)",
		      OPCODE_name(WN_opcode(tree)));
    /*NOTREACHED*/

  case OPR_INTRINSIC_OP:
    {
      INTRINSIC     id = (INTRINSIC) WN_intrinsic(tree);

      switch(id)
      {
      case INTRN_C4CONJG:
      case INTRN_C8CONJG:
      case INTRN_CQCONJG:
	{
	  WN	*iz;

	  lower_complex_expr(block, WN_actual(tree, 0), actions,
			     realpart, &iz);

	  *imagpart = WN_Neg(type, iz);

	  WN_Delete(tree);
	}
        break;

	//*****************************************************************
	//
	// N.B. Any complex intrinsic which does not have an emulation must
	// appear in the list below.
	//

	//       case INTRN_F4CIS:
	//case INTRN_F8CIS:
	//case INTRN_FQCIS:
	//  actions |= LOWER_INTRINSIC;
	  /* Fall Through */

      default:
	if (INTRN_is_actual(WN_intrinsic(tree)))
	{
	  tree = lower_intrinsic(block, tree, actions);
	}
	else
	{
	  lower_complex_emulation(block, tree, actions, realpart, imagpart);
	}
        break;
      }
    }
    break;

  case OPR_SELECT:
    {
       WN *r1, *i1;
       WN *r2, *i2;
       LEAF cond;
       
       cond = Make_Leaf(block, WN_kid0(tree), WN_rtype(WN_kid0(tree)));
       
       lower_complex_expr(block, WN_kid1(tree), actions, &r1, &i1);
       lower_complex_expr(block, WN_kid2(tree), actions, &r2, &i2);
       
       *realpart = WN_Select(type, Load_Leaf(cond), r1, r2);
       *imagpart = WN_Select(type, Load_Leaf(cond), i1, i2);
    }
    break;

  case OPR_PARM:
    lower_complex_expr(block, WN_kid0(tree), actions, realpart, imagpart);
    break;
  }
}




/* ====================================================================
 *
 * void lower_quad_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				 WN **hipart, WN **lopart)
 *
 * ==================================================================== */

static void lower_quad_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
				     WN **hipart, WN **lopart)
{
  TYPE_ID	type =	MTYPE_F8;


  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(actions & LOWER_QUAD, ("actions does not contain LOWER_QUAD"));
  Is_True((MTYPE_is_quad(WN_rtype(tree))),
	  ("expected quad-type node, not %s", OPCODE_name(WN_opcode(tree))));
 /*
  *  first lower the quad as expressions.
  *
  *  This will create and serialize a call chain
  *  that should always return a leaf that can be decomposed
  */
  tree = lower_expr(block, tree, actions);

  switch (WN_operator(tree))
  {
  case OPR_LDID:
    {
     /*
      *  we must create a new ST of type real*8
      */
      TY_IDX    beTY   = MTYPE_To_TY(type);
      WN_OFFSET offset = WN_load_offset(tree);

      *hipart = WN_Ldid(type, offset,
			coerceST(WN_st(tree), type),
			coerceTY(WN_ty(tree), type));

      *lopart = WN_Ldid(type,
			coerceOFFSET(tree, type, offset),
			coerceST(WN_st(tree), type),
			coerceTY(WN_ty(tree), type));

      lower_quad_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_ILOAD:
    {
      WN	*addr;
      WN_OFFSET offset = WN_load_offset(tree);
      LEAF	addrN;

      addr = lower_expr(block, WN_kid0(tree), actions);

      addrN = Make_Leaf(block, addr, Pointer_type);

      *hipart = WN_Iload(type,
			 offset,
			 coerceTY(WN_ty(tree), type),
			 Load_Leaf(addrN));

      *lopart = WN_Iload(type,
			 coerceOFFSET(tree, type, offset),
			 coerceTY(WN_ty(tree), type),
			 Load_Leaf(addrN));

      lower_quad_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_CONST:
    {
      TCON	val = Const_Val(tree);

      *hipart = Make_Const( Extract_Quad_Hi(val));
      *lopart = Make_Const( Extract_Quad_Lo(val));

    }
    break;

  case OPR_PAREN:
    {
     /*
      *	preserve the parens
      */
      lower_quad_expr(block, WN_kid0(tree), actions, hipart, lopart);

      *hipart = WN_Paren(type, *hipart);
      *lopart = WN_Paren(type, *lopart);
    }
    break;

  case OPR_PARM:
    lower_quad_expr(block, WN_kid0(tree), actions, hipart, lopart);
    break;

  case OPR_ILOADX:
    Is_True( FALSE, ("unexpected QUAD ILOADX"));
    break;

  default:
    Is_True((FALSE),
	    ("lower_quad_expr didn't %s", OPCODE_name(WN_opcode(tree))));
  }
}

static WN *fix_bad_paired_int_expr(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  if (MTYPE_is_integral(WN_rtype(tree))) {

    /* If 'tree' is an LDID or ILOAD with result type U8/I8 and desc a
       smaller mtype, then we first want to convert that to a CVT ( LOAD ) 
       so that the emulator will correctly "load" both parts of the value. */
    if (((WN_operator(tree)==OPR_LDID) || (WN_operator(tree)==OPR_ILOAD)) &&
        (WN_desc(tree) != MTYPE_BS) &&
	(MTYPE_byte_size(WN_rtype(tree)) > (MTYPE_byte_size(WN_desc(tree)))) &&
	(MTYPE_byte_size(WN_rtype(tree)) > (MTYPE_byte_size(MTYPE_U4))))
    {
      /* Simplifier will get rid of our CVT... */
      BOOL simp = WN_Simplifier_Enable(FALSE);
      
      WN *ld = lower_copy_tree(tree, actions);
      WN_set_rtype(ld, Mtype_TransferSign(WN_rtype(ld), MTYPE_U4));
      tree = WN_Cvt(WN_rtype(ld), WN_rtype(tree), ld);
      
      WN_Simplifier_Enable(simp);
      return tree;
    }
    if (MTYPE_byte_size(WN_rtype(tree)) < (MTYPE_byte_size(MTYPE_U8))) {
      /* Simplifier will get rid of our CVT... */
      BOOL simp = WN_Simplifier_Enable(FALSE);
      
      tree = WN_Cvt(WN_rtype(tree), 
		    Mtype_TransferSign(WN_rtype(tree), MTYPE_U8), tree);
      
      WN_Simplifier_Enable(simp);
      return tree;
    }
  }
  return tree;
}

/* ====================================================================
 *
 * void lower_paired_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				 WN **hipart, WN **lopart)
 *
 * This functions lowers 8-byte loads and constants into pairs of 4-byte
 * loads and constants.  This split becomes necessary for machines without
 * native support 8-byte data types such as Xtensa.
 *
 * ==================================================================== */

static void lower_paired_expr(WN *block, WN *tree, LOWER_ACTIONS actions,
				          WN **hipart, WN **lopart)
{
  TYPE_ID type = MTYPE_I4;
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);

  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(actions & LOWER_PAIRED, ("actions does not contain LOWER_PAIRED"));
  FmtAssert((MTYPE_is_double(rtype) || (rtype == MTYPE_I8) || (rtype == MTYPE_U8)),
        ("expected double or long-long type node, not %s",
         OPCODE_name(WN_opcode(tree))));
/*
  if (!(MTYPE_is_double(rtype) || (rtype == MTYPE_I8) || (rtype == MTYPE_U8)))
    DevWarn("expected double or long-long type node, not %s",
	    OPCODE_name(WN_opcode(tree)));
*/

  tree = fix_bad_paired_int_expr(block, tree, actions);

  tree = lower_expr(block, tree, actions);

  switch (WN_operator(tree))
  {
  case OPR_LDID:
    {
      WN_OFFSET offset = WN_load_offset(tree);

      *lopart = WN_Ldid(type, offset,
			coerceST(WN_st(tree), type),
			coerceTY(WN_ty(tree), type));

      *hipart = WN_Ldid(type,
			coerceOFFSET(tree, type, offset),
			coerceST(WN_st(tree), type),
			coerceTY(WN_ty(tree), type));

      lower_paired_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_ILOAD:
    {
      WN	*addr;
      WN_OFFSET offset = WN_load_offset(tree);
      PREG_NUM	addrN;

      addr = lower_expr(block, WN_kid0(tree), actions);

      addrN = AssignExpr(block, addr, Pointer_type);

      *lopart = WN_Iload(type,
			 offset,
			 coerceTY(WN_ty(tree), type),
			 WN_LdidPreg(Pointer_type, addrN));

      *hipart = WN_Iload(type,
			 coerceOFFSET(tree, type, offset),
			 coerceTY(WN_ty(tree), type),
			 WN_LdidPreg(Pointer_type, addrN));

      lower_paired_maps(tree, *hipart, *lopart, actions);
    }
    break;

  case OPR_CONST:
    switch(WN_rtype(tree)) {
    case MTYPE_F8:
      {
	TCON	val = Const_Val(tree);
	TCON    lotcon = Extract_Double_Lo(val);
	TCON    hitcon = Extract_Double_Hi(val);
	INT32   loconst = TCON_ival(lotcon);
	INT32   hiconst = TCON_ival(hitcon);

	/* FIXME: May use F4 here, in which case use Make_Const instead
	   of Make_Integer_Const */ 
	*hipart = WN_CreateIntconst(OPC_I4INTCONST, hiconst);
	*lopart = WN_CreateIntconst(OPC_I4INTCONST, loconst);
	  
	break;
      }

    case MTYPE_U8:
    case MTYPE_I8:
      {
	TCON	val = Const_Val(tree);

	*hipart = WN_CreateIntconst((WN_rtype(tree) == MTYPE_U8) ? OPC_U4INTCONST : OPC_I4INTCONST,
				    TCON_ival(Extract_LongLong_Hi(val)));
	*lopart = WN_CreateIntconst((WN_rtype(tree) == MTYPE_U8) ? OPC_U4INTCONST : OPC_I4INTCONST,
				    TCON_ival(Extract_LongLong_Lo(val)));
	break;
      }
    }
    break;

  case OPR_INTCONST:
    switch(WN_rtype(tree)) {
    case MTYPE_U8:
    case MTYPE_I8:
      {
	TCON val = Host_To_Targ(WN_rtype(tree), WN_const_val(tree));

	*hipart = WN_CreateIntconst((WN_rtype(tree) == MTYPE_U8) ? OPC_U4INTCONST : OPC_I4INTCONST,
				    TCON_ival(Extract_LongLong_Hi(val)));
	*lopart = WN_CreateIntconst((WN_rtype(tree) == MTYPE_U8) ? OPC_U4INTCONST : OPC_I4INTCONST,
				    TCON_ival(Extract_LongLong_Lo(val)));
	break;
      }

    default:
      FmtAssert(false, ("Non MTYPE_I8/MTYPE_U8 INTCONST found in lower_paired_expr"));
    }
    break;

  case OPR_SELECT:
    {
      /* Lower the select operator first, before lowering the paired
	 expression.  This will avoid doing unnecessary computation.
      */
      WN *ldid = lower_select( block, tree, actions );
      lower_paired_expr( block, ldid, actions, hipart, lopart );
    }
    break;

  case OPR_PAREN:
    {
     /*
      *	preserve the parens
      */
      lower_paired_expr(block, WN_kid0(tree), actions, hipart, lopart);

      *hipart = WN_Paren(type, *hipart);
      *lopart = WN_Paren(type, *lopart);
    }
    break;

  case OPR_PARM:
    lower_paired_expr(block, WN_kid0(tree), actions, hipart, lopart);
    break;

  case OPR_ILOADX:
    Is_True( FALSE, ("unexpected double ILOADX"));
    break;

  default:
    Is_True((FALSE),
	    ("lower_paired_expr didn't %s", OPCODE_name(WN_opcode(tree))));
  }
}

static WN *lower_paired_expr( WN *block, WN *tree, LOWER_ACTIONS actions )
{
  WN	*hi, *lo, *eval;
  
  lower_paired_expr(block, tree, actions, &hi, &lo);
  
  hi = lower_expr(block, hi, actions);
  eval = WN_CreateEval(hi);
  WN_INSERT_BlockLast(block, eval);
  return lo;
}




static WN *WN_Coerce(TYPE_ID dst, WN *expr)
{
  TYPE_ID src= WN_rtype(expr);

  if (MTYPE_size_min(dst) == MTYPE_size_min(src))
    return expr;

  return WN_Cvt(src, dst, expr);
}




/* ====================================================================
 *
 * WN *lower_linearize_array_addr(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on ARRAY expr <tree>,
 * returning linearized address expression tree.  <actions> must
 * contain LOWER_ARRAY.
 *
 *  We are assuming that the index and array_dim information are unique
 *
 * Note: If the element size is < 0, it means that we have a non-contiguous F90
 *       array. These are linearized slightly differently. Instead of
 *       the extent children being extents, they are stride multiplier factors. 
 *       The sum of the products of the index and stride multiplier is scaled
 *       by -element_size to get the offset from the base.  
 * ==================================================================== */

static WN *lower_linearize_array_addr(WN *block, WN *tree,
				      LOWER_ACTIONS actions)
{
  TYPE_ID rtype = WN_rtype(tree);
  WN		*result, *product;
  INT32		n, i;
  BOOL          is_non_contig=FALSE;
  WN_ESIZE      element_size;

  Is_True((WN_operator_is(tree,OPR_ARRAY)),
	  ("expected ARRAY node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(Action(LOWER_ARRAY), ("actions doesn't contain LOWER_ARRAY"));
  Is_True(WN_num_dim(tree) > 0, ("WN_num_dim of ARRAY node not > 0"));

  ST* stride_st = NULL;
  n = WN_num_dim(tree);
  element_size = WN_element_size(tree);
  if (element_size < 0) {
     is_non_contig = TRUE;
     element_size = -element_size;
  } else if (element_size==0) {

    int index = 0;

    WN* base = WN_array_base(tree);
    while (WN_operator(base)==OPR_ARRAY) {
      index++;
      base = WN_array_base(base);
    }

    OPERATOR opr = WN_operator(base);
    if (opr==OPR_LDID) {
      ST* base_st = WN_st(base);
      TY_IDX ty_idx = ST_type(base_st);
      ty_idx = TY_pointed(ty_idx);
      for (int i=0; i<index; i++)
	ty_idx = TY_etype(ty_idx);
      ARB_HANDLE arb = TY_arb(ty_idx);
      if (ARB_const_stride(arb)) {
	FmtAssert(0,("Expecting non-constant stride"));
      }
      ST_IDX stride_st_idx = ARB_stride_var(arb);
      stride_st = ST_ptr(stride_st_idx);
    }
  }

  FmtAssert((element_size!=0 || stride_st),("Missing element_size"));


  if (is_non_contig)
  {
     WN *stride_mult;
     
     result = WN_Coerce(rtype, WN_array_index(tree, n-1));
     stride_mult = WN_Coerce(rtype, WN_array_dim(tree, n-1));
     result = WN_Mpy(rtype,result,stride_mult);

     for (i = n-2; i >= 0; i--) {
	product = WN_Coerce(rtype, WN_array_index(tree, i));
	stride_mult = WN_Coerce(rtype, WN_array_dim(tree, i));
	product = WN_Mpy(rtype,product,stride_mult);
	result = WN_Add(rtype,result,product);
     }
  }
  else
  {
   /*
    *  result <- index[n-1]
    */
    result = WN_Coerce(rtype, WN_array_index(tree, n-1));
     
   /*
    *  result <- result + index[i] * ( dim[n-1] * dim[n-2] ... dim[i+1] )
    */
    for (i = n-2; i >= 0; i--)
    {
      INT32	m;
      WN	*mpy;
	     
      product = WN_Coerce(rtype, lower_copy_tree(WN_array_dim(tree, n-1),
						 actions));
      for (m=n-2; m>i; m--)
      {
	product = WN_Mpy(rtype,
			 product,
			 WN_Coerce(rtype, lower_copy_tree(WN_array_dim(tree, m),
							  actions)));
      }

      mpy = WN_Mpy(rtype,
		   WN_Coerce(rtype, WN_array_index(tree,i)),
		   product);
      result = WN_Add(rtype, result, mpy);
    }
  }  
  

  /*
   *  result <- base + result * elm_size
   */
  {
    WN  *elm_size;

    if (element_size !=0)
      elm_size = WN_Intconst(rtype, element_size);
    else {
      TY_IDX ty_idx = ST_type(stride_st);
      elm_size = WN_Ldid(TY_mtype(ty_idx), 0, stride_st, ty_idx);
    }
    result = WN_Add(rtype,
		    WN_array_base(tree),
		    WN_Mpy(rtype, result, elm_size));
  }
  
  result = lower_expr(block, result, actions);

  WN_Delete(tree);	    /* ARRAY node not used */

  return result;
}




/* ====================================================================
 *
 * WN *lower_unsigned_to_float(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_unsigned_to_float(WN *block, WN *expr, TYPE_ID src,
				   TYPE_ID dst, LOWER_ACTIONS actions)
{
  LEAF	srcNo, dstNo, dstNo2;


  /* Special case of U4. Don't do this special case if the cvt.d.l 
   * instruction is slow. This is true on the R5000 which emulates
   * this in software due to a chip bug.
   */
  if (src == MTYPE_U4 && !Slow_CVTDL)
  {
     BOOL  simp=	WN_Simplifier_Enable(FALSE);
     WN * r;

     r = WN_Cvt(MTYPE_I8,dst,WN_Cvt(MTYPE_U4, MTYPE_U8, expr));

     WN_Simplifier_Enable(simp);
     return (r);
  }


  /*
   *  store the expr into a preg to avoid building a dag
   *	src = expr;
   */
  srcNo = Make_Leaf(block, expr, src);

  /*
   *  dst = (signed cvt) src ;
   */
  {
    WN	*ldid, *cvt;

    ldid = Load_Leaf(srcNo);

    cvt  = WN_Cvt(MTYPE_complement(src), dst, ldid);

    dstNo = Make_Leaf(block, cvt, dst);
  }

#ifdef TARG_XTENSA
  if (MTYPE_byte_size(dst) <= MTYPE_byte_size(src)) {
    /*
     *  dst = 2*(signed cvt) ((src>>1)|(src&1)) ;
     */
    {
      WN	*ldid, *ldid2, *cvt, *sh_right, *last_bit, *bior, *mult;
      
      ldid = Load_Leaf(srcNo);
      sh_right = WN_Lshr(src, ldid, WN_Intconst(MTYPE_I4, 1));
      ldid2 = Load_Leaf(srcNo);
      last_bit = WN_Band(src, ldid2, WN_Intconst(src, 1));
      bior = WN_Bior(src, sh_right, last_bit);
      cvt  = WN_Cvt(MTYPE_complement(src), dst, bior);
      mult = WN_Mpy(dst, cvt, WN_Floatconst(dst, 2));
      dstNo2 = Make_Leaf(block, mult, dst);
    }

    /*
     *  build the select tree that looks like
     *	(src < 0) ? dst2 : dst
     */
    {
      WN	*rel, *select;
      
      rel = WN_LT(MTYPE_complement(src),
		  Load_Leaf(srcNo),
		  WN_Zerocon(MTYPE_complement(src)));
      
      select = WN_Cselect(dst, rel, Load_Leaf(dstNo2), Load_Leaf(dstNo));

      if (Cur_PU_Feedback) {
	/* Branches created during lowering to CG are not instrumented, 
	 * and we don't have a clue about taken vs. not taken counts. 
	 * We used to annotate the branch with (0.5, 0.5), but that's
	 * bogus, because we need counts and not probabilities. Instead,
	 * we'll use a guessed count on one side.
	 */
	FB_Info_Branch info_branch(FB_FREQ(1.0,FB_FREQ_TYPE_GUESS),FB_FREQ_UNKNOWN); 
	Cur_PU_Feedback->Annot_branch(select, info_branch);
      }

      return select;
    }
  } else
#endif
  /*
   *  build the select tree that looks like
   *	(src < 0) ? dst + 2**N : dst
   *	 where N is the src size
   */
  {
    WN	*rel, *add, *select;
    
    rel = WN_LT(MTYPE_complement(src),
		Load_Leaf(srcNo),
		WN_Zerocon(MTYPE_complement(src)));
    
    add = WN_Add(dst,
		 Load_Leaf(dstNo),
		 WN_ConstPowerOf2( dst, MTYPE_size_reg(src)));
    
    select = WN_Cselect(dst, rel, add, Load_Leaf(dstNo));

    if (Cur_PU_Feedback) {
      /* Branches created during lowering to CG are not instrumented, 
	 * and we don't have a clue about taken vs. not taken counts. 
	 * We used to annotate the branch with (0.5, 0.5), but that's
	 * bogus, because we need counts and not probabilities. Instead,
	 * we'll use a guessed count on one side.
       */
      FB_Info_Branch info_branch(FB_FREQ(1.0,FB_FREQ_TYPE_GUESS), FB_FREQ_UNKNOWN); 
      Cur_PU_Feedback->Annot_branch(select, info_branch);
    }

    return select;
  }
}




/* ====================================================================
 *
 * WN *lower_float_to_unsigned(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_float_to_unsigned(WN *block, WN *expr, TYPE_ID src,
				   TYPE_ID dst, LOWER_ACTIONS actions)
{
  LEAF srcNo;
  WN *trunc1,*r;
  WN *rel, *sub, *trunc2,*add;


  /* Two cases, dest type = U8, and dest type = U4 */
  if (dst == MTYPE_U4) {
     r = WN_Cvt(MTYPE_I8,dst,WN_Trunc(src,MTYPE_I8,expr));
  } else if (src==MTYPE_FQ) {  /* Need to do this this way because there
				* is no quad floor */
     
     /*
      *  store the expr into a preg to avoid building a dag
      *	src = expr;
      */
     srcNo = Make_Leaf(block, expr, src);
     
     /*
      *  build the select tree that looks like
      *
      * (2**(N-1) <= src) ? : 2**(N-1) + TRUNC(src-2**(N-1)) : TRUNC(src)
      *
      *	where N is the unsigned dst size
      */


     trunc1 = WN_Trunc(src, MTYPE_complement(dst), Load_Leaf(srcNo));
     
     sub = WN_Sub(src, Load_Leaf(srcNo),
		  WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1));

     trunc2 =  WN_Trunc(src, MTYPE_complement(dst),sub);

     add = WN_Add(dst,trunc2,WN_ConstPowerOf2(dst, MTYPE_size_reg(dst)-1));
     
     rel = WN_LE(src,
		 WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1),
		 Load_Leaf(srcNo));
     
     r = WN_Cselect(dst,rel,add,trunc1);
  } else {
     
     /*
      *  store the expr into a preg to avoid building a dag
      *	src = expr;
      */
     srcNo = Make_Leaf(block, expr, src);
     
     /*
      *  build the select tree that looks like
      *
      * (2**(N-1) <= src) ? : FLOOR(src-2**(N)) : TRUNC(src)
      *
      *	where N is the unsigned dst size
      */


     trunc1 = WN_Trunc(src, MTYPE_complement(dst), Load_Leaf(srcNo));
     
     sub = WN_Sub(src, Load_Leaf(srcNo),
		  WN_ConstPowerOf2(src, MTYPE_size_reg(dst)));

     trunc2 =  WN_Floor(src, MTYPE_complement(dst),sub);

     rel = WN_LE(src,
		 WN_ConstPowerOf2(src, MTYPE_size_reg(dst)-1),
		 Load_Leaf(srcNo));
     
     r = WN_Cselect(dst,rel,trunc2,trunc1);
  }
  return (r);
}




/* ====================================================================
 *
 * WN *lower_cvt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on conversions <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_cvt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN	*expr, *cvt;
  TYPE_ID dst = WN_rtype(tree);
  TYPE_ID src = WN_desc(tree);

  expr = lower_expr(block, WN_kid0(tree), actions);
  WN_kid0(tree) = expr;

  if (   Targ_Lower_Unsigned_To_Float 
      && MTYPE_is_unsigned(src) && MTYPE_is_float(dst))
  {
    WN_Delete(tree);

    cvt = lower_unsigned_to_float(block, expr, src, dst, actions);

#ifdef TARG_XTENSA
    tree = lower_select(block,cvt,actions);
#endif

    tree = lower_expr(block, tree, actions);

    return tree;
  }
  else if (   Targ_Lower_Float_To_Unsigned 
	   && MTYPE_is_float(src) && MTYPE_is_unsigned(dst))
  {
    WN_Delete(tree);

    cvt = lower_float_to_unsigned(block, expr, src, dst, actions);

    return lower_expr(block, cvt, actions);
  }
  else if (   OPERATOR_is_compare(WN_operator(expr))
	   && WN_rtype(expr) == MTYPE_B
	   && (MTYPE_is_integral(dst) && src == MTYPE_B))
  {

    /* Optimize converts of MTYPE_B compares to integral values.
     */
    WN_Delete(tree);

    WN_set_rtype(expr, dst);

    return lower_expr(block, expr, actions);
  }
  
  return tree;
}



/* ====================================================================
 *
 * WN *lower_nary_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 *  Create nary expression reassociate to produce MADDS
 *
 * ==================================================================== */

static WN *lower_nary_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
{

  if (Enable_NaryExpr && Roundoff_Level >= ROUNDOFF_ASSOC)
  {
    TYPE_ID  type = WN_rtype(tree);

    tree = WN_ExprToNaryType(tree, type);

    if (WN_nary_add(tree))
    {
      INT32	i;
      BOOL	found= TRUE;

     /*
      *  Find mpy then a non mpy under nary tree.
      *  The tree may change (ex. to a binary tree)
      */
      while(found)
      {
	INT32	mpyI, exprI, nmpyI, narympyI;
	WN	*mpy, *nmpy, *expr, *narympy;
        INT32	num_kids = WN_kid_count(tree);

	found=	FALSE;
	mpy  =  nmpy  = expr = narympy  = NULL;
	mpyI =  exprI = nmpyI = narympyI = 0;

	for(i = 0; i < num_kids; i++)
	{
	  WN  *actual= WN_actual(tree, i);

	  if (WN_operator_is(actual, OPR_MPY))
	  {
	    mpyI = i;
	    mpy = actual;
	  }
	  else if (WN_operator_is(actual, OPR_NEG)	&&
		   WN_operator_is(WN_kid0(actual), OPR_MPY))
	  {
	    nmpyI = i;
	    nmpy = WN_kid0(actual);
	  }
	  else if (WN_nary_mpy(actual) && WN_kid_count(actual) >= 2)
	  {
	    narympyI = i;
	    narympy = actual;
	  }
	  else 
	  {
	    exprI = i;
	    expr = actual;
	  }

	  if (mpy && expr)
	  {
	    WN_actual(tree, mpyI)= WN_Madd(type, expr, WN_kid0(mpy),
					   WN_kid1(mpy));
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	  else if (nmpy && expr)
	  {
	    WN_actual(tree, nmpyI)= WN_Nmsub(type, expr, WN_kid0(nmpy),
					     WN_kid1(nmpy));
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	  else if (narympy && expr)
	  {
	    mpy=	WN_kid0(narympy);
	    narympy=	WN_NaryDelete(narympy, 0);

	    WN_actual(tree, narympyI)= WN_Madd(type, expr, mpy, narympy);
	    tree = WN_NaryDelete(tree, exprI);
	    found=	TRUE;
	    break;
	  }
	}
      }
     /*
      *  There may be madd opportunites at the children
      */
      for(i = 0; i < WN_kid_count(tree); i++)
      {
	WN_actual(tree, i)= lower_madd(block, WN_actual(tree,i), actions);
      }

    }
    
    if (WN_nary_intrinsic(tree))
    {
      tree = WN_NaryToExpr(tree);
    }
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on expression <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_madd(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);

 /*
  *  look for madd patterns
  */
  switch (WN_operator(tree))
  {
  case OPR_NEG:
    {
      WN	*child = WN_kid0(tree);

      switch(WN_operator(child))
      {
      case OPR_MADD:
	WN_Delete(tree);
	return WN_Nmadd(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_MSUB:
	WN_Delete(tree);
	return WN_Nmsub(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_NMADD:
	WN_Delete(tree);
	return WN_Madd(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));

      case OPR_NMSUB:
	WN_Delete(tree);
	return WN_Msub(type, WN_kid0(child), WN_kid1(child), WN_kid2(child));
      }
    }
    break;

  case OPR_ADD:
    {
      WN	*l= WN_kid0(tree);
      WN	*r= WN_kid1(tree);

      if (WN_operator_is(l, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Madd(type, r, WN_kid0(l), WN_kid1(l));
      }
      else if (WN_operator_is(r, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Madd(type, l, WN_kid0(r), WN_kid1(r));
      }
    }
    break;

  case OPR_SUB:
    {
      WN	*l= WN_kid0(tree);
      WN	*r= WN_kid1(tree);

      if (WN_operator_is(r, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Nmsub(type, l, WN_kid0(r), WN_kid1(r));
      }
      else if (WN_operator_is(l, OPR_MPY))
      {
	WN_Delete(tree);
	return WN_Msub(type, r, WN_kid0(l), WN_kid1(l));
      }
    }
    break;

  case OPR_MADD:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Msub(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_MSUB:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Madd(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_NMADD:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Nmsub(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;

  case OPR_NMSUB:
    {
      WN	*neg= WN_kid0(tree);

      if (WN_operator_is(neg, OPR_NEG))
      {
	WN	*child= WN_kid0(neg);

	WN_Delete(neg);
	return WN_Nmadd(type, child, WN_kid1(tree), WN_kid2(tree));
      }
    }
    break;
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_tree_height(WN *block, WN *wn, LOWER_ACTIONS actions)
 *
 *  Reassociate binary commutative operator at this level 
 *  (ie no recursion)
 *
 * ==================================================================== */

static WN *lower_tree_height(WN *block, WN *wn, LOWER_ACTIONS actions)
{
  OPCODE  opcode = WN_opcode(wn);
  WN	  *l, *r;

  if (NotAction(LOWER_TREEHEIGHT))
    return wn;

  Is_True(WN_is_commutative(wn),("expected commutative op"));

  l = WN_kid0(wn);
  r = WN_kid1(wn);

 /*
  *  do not transform an already balanced tree
  */
  if (opcode == WN_opcode(l) ^ opcode == WN_opcode(r))

  {
    WN *wn1 =  (opcode == WN_opcode(l)) ? l : r;
    WN *wn1X = (opcode == WN_opcode(l)) ? r : l;

    WN *wn1_l = WN_kid0(wn1);
    WN *wn1_r = WN_kid1(wn1);

    if (opcode == WN_opcode(wn1_l) ^ opcode == WN_opcode(wn1_r))
    {

      WN *wn2 =  (opcode == WN_opcode(wn1_l)) ? wn1_l : wn1_r;
      WN *wn2X = (opcode == WN_opcode(wn1_l)) ? wn1_r : wn1_l;
     /*
      *  rearrange tree
      */
      WN_kid0(wn1) =	wn2X;
      WN_kid1(wn1) =	wn1X;

      WN_kid0(wn) =	wn2;
      WN_kid1(wn) =	wn1;

      if (traceTreeHeight)
      {
	DevWarn("lower_tree_height: trace (%s) has been reassociated (line %d)",
	        OPCODE_name(WN_opcode(wn)), Srcpos_To_Line(current_srcpos));
      }
    }
  }
  return wn;
}




/* ====================================================================
 *
 * WN *lower_recip(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * WN *lower_rsqrt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower recip/rsqrt based on ISA flags
 *
 * Quad is lowered unconditionally, as there is no runtime support
 * (kludge alert: turn off simplifer when creating quad divide !!)
 *
 * ==================================================================== */

static WN *lower_recip(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);
  WN	  *kid0 = WN_kid0(tree);
  WN	  *div =  NULL;

  Is_True(WN_operator_is(tree, OPR_RECIP), ("expected recip"));
  Is_True(MTYPE_float(type), ("expected float type"));
  
  if (!MTYPE_RECIP_ALLOWED(type))
  {
    div = WN_Div(type, WN_Floatconst(type, 1.0), kid0);
  }
  else if (Action(LOWER_QUAD) && MTYPE_is_quad(type))
  {
    BOOL  simp=	WN_Simplifier_Enable(FALSE);

    div = WN_Div(type, WN_Floatconst(type, 1.0), kid0);

    WN_Simplifier_Enable(simp);
  }

  if (div)
  {
    WN_Delete(tree);
    return div;
  }

  return tree;
}

static WN *lower_rsqrt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID  type = WN_rtype(tree);

  Is_True(WN_operator_is(tree, OPR_RSQRT), ("expected rsqrt"));
  Is_True(MTYPE_float(WN_rtype(tree)), ("expected float type"));

  if (!MTYPE_RSQRT_ALLOWED(type) || MTYPE_is_quad(type))
  {
    WN	*div;

    div = WN_Div(type, WN_Floatconst(type, 1.0), WN_Sqrt(type, WN_kid0(tree)));
    WN_Delete(tree);

    return div;
  }
  return tree;
}

/* ====================================================================
 *
 * Find minimum shift-add, shift-sub sequence from a shift-add sequence,
 * a helper function for lower_mpy_by_const. 
 *
 * ==================================================================== */
static INT find_min_sequence(char *code, INT &len)
{
  bool change = false;
  do {
    change  = false;
    for (INT i = 0; i < len; i++) {
      /* combine sequence more than two s+ to +s-       *
      ** s+s+s+     ==> +sss-                           *
      ** s+s+s+s+   ==> +ssss-                          *
      ** s+s+s+s+s+ ==> +sssss-                         */
      INT streak = 0;
      INT j;
      for (j = i; j < len - 1; j += 2) {
	if (code[j] == 's' && code[j+1] == '+') {
	  streak++;
	} else {
	  break;
	}
      }
      if (streak >= 3 || (streak == 2 && i == 0)) {
	/* s+s+ at the beginning of the sequence, it is beneficial to
	   +ss- => sss- */
	change = true;
	
	INT cur = i;
	code[cur++] = '+';
	for (INT k = 0; k < streak; k++) {
	  code[cur++] = 's';
	}
	code[cur++] = '-';
	while (j<len) {
	  code[cur++] = code[j++];
	}
	Is_True(cur == len - streak + 2, ("Just checking"));
		
	/* set the new length */
	len = cur;
	break;
      } 
    }
  } while (change);
		
  Is_True(len >= 0 && len < 64, ("Just checking"));
  if (len > 0 && code[0] == '+') {
    /* change the first add to shift */
    code[0] = 's';
  }
    
  /* count the number of instructions */
  INT cnt = 0;
  INT shift = 0;
  for (INT i = 0; i < len; i++) {
    switch(code[i]) {
    case 's':
      shift++;
      break;
    case '+':
      if (shift > 3) {
	/* more than add8x */
	cnt++;
      }
      shift = 0;
      cnt++;
      break;
    case '-':
      if (shift > 3) {
	/* more than sub8x */
	cnt++;
      }
      shift = 0;
      cnt++;
      break;
    default:
      FmtAssert(0, ("Unknown action"));
    }
  }
  if (shift != 0) {
    cnt++;
  }
  Is_True(cnt>=0, ("Just checking"));
  return cnt;
}

static inline void split_paired_preg_nums(PREG_NUM orig_num, TYPE_ID new_type,
                                            PREG_NUM *lo_num, PREG_NUM *hi_num)
{
  if (Target_Byte_Sex == BIG_ENDIAN) {
    *hi_num = orig_num;
    *lo_num = orig_num + Preg_Increment(new_type);
  } else {
    *lo_num = orig_num;
    *hi_num = orig_num + Preg_Increment(new_type);
  }
}


/* ====================================================================
 *
 * WN *lower_int_int_ll(WN *, WN *, LOWER_ACTIONS);
 *
 * lower 32x32into64 multiplication using mul32h
 *
 * ==================================================================== */
static WN *lower_int_int_ll(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_operator(tree) == OPR_MPY, ("Not a multiply")); 
  TYPE_ID type = WN_rtype(tree);
  BOOL is_unsigned = type == MTYPE_U8;
  TYPE_ID int_type = is_unsigned ? MTYPE_U4 : MTYPE_I4;

  WN *kid0 = WN_kid0(tree);
  if (is_unsigned) {
    if (WN_opcode(kid0) == OPC_U8U4CVT) {
      kid0 = WN_kid0(kid0);
      WN_Delete(WN_kid0(tree));
    } else if (WN_opcode(kid0) == OPC_U8U4ILOAD) {
      WN_set_rtype(kid0, MTYPE_U4);
    }
  } else {
    if (WN_opcode(kid0) == OPC_I8I4CVT) {
      kid0 = WN_kid0(kid0);
      WN_Delete(WN_kid0(tree));
    } else if (WN_opcode(kid0) == OPC_I8I4ILOAD) {
      WN_set_rtype(kid0, MTYPE_I4);
    }
  }

  WN *kid1 = WN_kid1(tree);
  if (is_unsigned) {
    if (WN_opcode(kid1) == OPC_U8U4CVT) {
      kid1 = WN_kid0(kid1);
      WN_Delete(WN_kid1(tree));
    } else if (WN_opcode(kid1) == OPC_U8U4ILOAD) {
      WN_set_rtype(kid1, MTYPE_U4);
    }
  } else {
    if (WN_opcode(kid1) == OPC_I8I4CVT) {
      kid1 = WN_kid0(kid1);
      WN_Delete(WN_kid1(tree));
    } else if (WN_opcode(kid1) == OPC_I8I4ILOAD) {
      WN_set_rtype(kid1, MTYPE_I4);
    }
  }

  // copy off the arguments
  PREG_NUM t0, t1;
  t0 = AssignExpr(block, lower_copy_tree(kid0, actions), int_type);
  t1 = AssignExpr(block, lower_copy_tree(kid1, actions), int_type);

  PREG_NUM result = Create_Preg(type, "mpy_result");
  PREG_NUM lo_result, hi_result;
  split_paired_preg_nums(result, int_type, &lo_result, &hi_result);

  // set the low part of the result using a normal mpy operation
  WN *stid = WN_StidIntoPreg(int_type, lo_result, MTYPE_To_PREG(Integer_type),
		WN_Mpy(MTYPE_U4, WN_LdidPreg(int_type,t0), WN_LdidPreg(int_type, t1)));
  WN_INSERT_BlockLast(block, stid);


  // set the high part of the result using an intrinsic
  WN *kids[2];
  kids[0] = WN_CreateParm (int_type, WN_LdidPreg(int_type, t0),
				 Be_Type_Tbl(type),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
  kids[1] = WN_CreateParm (int_type, WN_LdidPreg(int_type, t1),
				 Be_Type_Tbl(type),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
  TIE_MACRO* tie_macro;
  tie_macro = is_unsigned ? tie_info->tie_macro("_TIE_xt_MUL32_MULUH") :
			    tie_info->tie_macro("_TIE_xt_MUL32_MULSH") ;
  INTRINSIC mulhi = Tie_Macro_Id_To_Intrinsic(tie_macro->id());

  stid = WN_StidIntoPreg(int_type, hi_result, MTYPE_To_PREG(Integer_type),
	                 WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_OP, int_type, MTYPE_V),
				  mulhi, 2, kids));
  WN_INSERT_BlockLast(block, stid);

  WN_Delete(tree);
  return WN_LdidPreg(type,result);
}

/*
   If an integer 64bit multiply involves at least one 64bit operand and
   mul32 and mul32h are available, we will use mull and muluh directly
   instead of calling emulation function.
*/
static WN *lower_inline_mpy64(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_operator(tree) == OPR_MPY, ("Not a multiply"));

  TYPE_ID type = WN_rtype(tree);
  BOOL is_unsigned = (type == MTYPE_U8);

  WN *kid32 = NULL;
  WN *kid64_0 = WN_kid0(tree);
  WN *kid64_1 = WN_kid1(tree);
  if (is_unsigned) {
    if (WN_opcode(kid64_0) == OPC_U8U4CVT) {
      kid32 = WN_kid0(kid64_0);
      kid64_0 = NULL;
      WN_Delete(WN_kid0(tree));
    } else if (WN_opcode(kid64_0) == OPC_U8U4ILOAD) {
      WN_set_rtype(kid64_0, MTYPE_U4);
      kid32 = kid64_0;
      kid64_0 = NULL;
    }
  
    if (kid32 == NULL) {
      if (WN_opcode(kid64_1) == OPC_U8U4CVT) {
        kid32 = WN_kid0(kid64_1);
        kid64_1 = NULL;
        WN_Delete(WN_kid1(tree));
      } else if (WN_opcode(kid64_1) == OPC_U8U4ILOAD) {
        WN_set_rtype(kid64_1, MTYPE_U4);
        kid32 = kid64_1;
        kid64_1 = NULL;
      }
    }
  }

  PREG_NUM result = Create_Preg(type, "mpy_result");
  PREG_NUM lo_result, hi_result;
  split_paired_preg_nums(result, MTYPE_U4, &lo_result, &hi_result);

  PREG_NUM t0_lo, t0_hi, t1_lo, t1_hi;
  if (kid32 != NULL) {
    // 32x64 => 64

    PREG_NUM t0;
    WN *w_lo, *w_hi;
    WN *kid64 = (kid64_0 != NULL) ? kid64_0 : kid64_1;

    if (Target_Byte_Sex == BIG_ENDIAN) {
      lower_paired_expr(block, kid64, actions, &w_lo, &w_hi);
    } else {
      lower_paired_expr(block, kid64, actions, &w_hi, &w_lo);
    }
    t0 = AssignExpr(block, lower_copy_tree(kid32, actions), MTYPE_U4);
    t1_hi = AssignExpr(block, lower_copy_tree(w_hi, actions), MTYPE_U4);
    t1_lo = AssignExpr(block, lower_copy_tree(w_lo, actions), MTYPE_U4);

    // set the low part of the result using a normal mpy operation
    WN *stid = WN_StidIntoPreg(MTYPE_U4, lo_result, MTYPE_To_PREG(Integer_type),
                 WN_Mpy(MTYPE_U4, WN_LdidPreg(MTYPE_U4,t0), WN_LdidPreg(MTYPE_U4, t1_lo)));
    WN_INSERT_BlockLast(block, stid);

    // set the high part of the result using an intrinsic
    WN *kids[2];
    kids[0] = WN_CreateParm (MTYPE_U4, WN_LdidPreg(MTYPE_U4, t0),
                             Be_Type_Tbl(type),
                             WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
    kids[1] = WN_CreateParm (MTYPE_U4, WN_LdidPreg(MTYPE_U4, t1_lo),
                             Be_Type_Tbl(type),
                             WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
    TIE_MACRO* tie_macro;
    tie_macro = tie_info->tie_macro("_TIE_xt_MUL32_MULUH");
    INTRINSIC mulhi = Tie_Macro_Id_To_Intrinsic(tie_macro->id());

    WN *tmp = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_OP, MTYPE_U4, MTYPE_V),
			          mulhi, 2, kids);

    WN *tmp1 = WN_Mpy(MTYPE_U4, WN_LdidPreg(MTYPE_U4,t0), WN_LdidPreg(MTYPE_U4, t1_hi));
    tmp = WN_Add(MTYPE_U4, tmp, tmp1); 
    stid = WN_StidIntoPreg(MTYPE_U4, hi_result, MTYPE_To_PREG(Integer_type), tmp);
    WN_INSERT_BlockLast(block, stid);

    WN_Delete(tree);
    return WN_LdidPreg(type, result);

  } else if ( (kid64_0 != NULL) && (kid64_1 != NULL) ) {
    // 64x64 => 64

    WN *w0_lo, *w0_hi, *w1_lo, *w1_hi;

    if (Target_Byte_Sex == BIG_ENDIAN) {
      lower_paired_expr(block, kid64_0, actions, &w0_lo, &w0_hi);
      lower_paired_expr(block, kid64_1, actions, &w1_lo, &w1_hi);
    } else {
      lower_paired_expr(block, kid64_0, actions, &w0_hi, &w0_lo);
      lower_paired_expr(block, kid64_1, actions, &w1_hi, &w1_lo);
    }
    t0_lo = AssignExpr(block, lower_copy_tree(w0_lo, actions), MTYPE_U4);
    t0_hi = AssignExpr(block, lower_copy_tree(w0_hi, actions), MTYPE_U4);
    t1_hi = AssignExpr(block, lower_copy_tree(w1_hi, actions), MTYPE_U4);
    t1_lo = AssignExpr(block, lower_copy_tree(w1_lo, actions), MTYPE_U4);

    // set the low part of the result using a normal mpy operation
    WN *stid = WN_StidIntoPreg(MTYPE_U4, lo_result, MTYPE_To_PREG(Integer_type),
                 WN_Mpy(MTYPE_U4, WN_LdidPreg(MTYPE_U4,t0_lo), WN_LdidPreg(MTYPE_U4, t1_lo)));
    WN_INSERT_BlockLast(block, stid);

    // set the high part of the result using an intrinsic
    WN *kids[2];
    kids[0] = WN_CreateParm (MTYPE_U4, WN_LdidPreg(MTYPE_U4, t0_lo),
                             Be_Type_Tbl(type),
                             WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
    kids[1] = WN_CreateParm (MTYPE_U4, WN_LdidPreg(MTYPE_U4, t1_lo),
                             Be_Type_Tbl(type),
                             WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
    TIE_MACRO* tie_macro;
    tie_macro = tie_info->tie_macro("_TIE_xt_MUL32_MULUH");
    INTRINSIC mulhi = Tie_Macro_Id_To_Intrinsic(tie_macro->id());

    WN *tmp = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_OP, MTYPE_U4, MTYPE_V),
			          mulhi, 2, kids);

    WN *tmp1 = WN_Mpy(MTYPE_U4, WN_LdidPreg(MTYPE_U4,t0_lo), WN_LdidPreg(MTYPE_U4, t1_hi));
    tmp = WN_Add(MTYPE_U4, tmp, tmp1); 
    tmp1 = WN_Mpy(MTYPE_U4, WN_LdidPreg(MTYPE_U4,t0_hi), WN_LdidPreg(MTYPE_U4, t1_lo));
    tmp = WN_Add(MTYPE_U4, tmp, tmp1); 
    stid = WN_StidIntoPreg(MTYPE_U4, hi_result, MTYPE_To_PREG(Integer_type), tmp);
    WN_INSERT_BlockLast(block, stid);

    WN_Delete(tree);
    return WN_LdidPreg(type, result);

  } else {
    FmtAssert(0, ("Unknown operands for 64bit MPY"));
  }
}

/* ====================================================================
 *
 * WN *lower_mpy_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower multiply by constant 
 *
 * ==================================================================== */
static WN *lower_mpy_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_operator(tree) == OPR_MPY, ("Not a multiply")); 
  TYPE_ID type = WN_rtype(tree);

  switch (type) {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
    WN *cst = NULL;
    WN *var = NULL;

    if (WN_operator(WN_kid0(tree)) == OPR_INTCONST) {
      cst = WN_kid0(tree);
      var = WN_kid1(tree);
    } else if (WN_operator(WN_kid1(tree)) == OPR_INTCONST) {
      cst = WN_kid1(tree);
      var = WN_kid0(tree);
    }

    if (cst == NULL) { 
      return tree;
    } else { /* found a multiply by constant */
      INT64 c   = WN_const_val(cst);
      UINT64 uc = WN_const_val(cst);
      WN   *rep = NULL;
      bool negate = false;

      if ((MTYPE_is_signed(WN_rtype(cst)) &&
	  ((c < INT32_MIN) || (c > INT32_MAX))) ||
	  (MTYPE_is_unsigned(WN_rtype(cst)) &&
	   (uc > INT32_MAX))) {
	return tree;
      }
      
      /* screen zero and negative numbers */
      if (c == 0) {
	rep = WN_Intconst(type, c);
	WN_DELETE_Tree(tree);
	return rep;
      } else if (MTYPE_is_signed(WN_rtype(cst)) && c < 0) {
	c = -c;
	negate = true;
      }

      if (c <= 0)
      {
	fdump_tree(TFile, tree);
	fdump_tree(TFile, cst);
	fprintf(TFile, "wn_const_val = %" LLD_FMT ", c = %" LLD_FMT ", signed = %d\n",
		WN_const_val(cst), c, MTYPE_is_signed(WN_rtype(cst)));
      }
      Is_True(c>0, ("Just checking"));

	/* general multiply by constant */
      char  code[64];
      INT   len     = 0;
      bool  start   = false;
      INT   first_1 = 0;
      for (INT i = 0; i < 32; ++i) {
	if (c & (0x80000000 >> i)) {
	  if (start) {
	    code[len++] = 's';  /* shift */
	    code[len++] = '+';  /* plus  */
	  } else {
	    start = true;
	    first_1 = i;
	  }
	} else if (start) {
	  code[len++] = 's';      /* shift */
	}
      }
      Is_True(len >= 0 && len < 64, ("Just checking"));
      Is_True(len == 0 || code[0] == 's', ("Just checking"));
	
      /* find minimum sequence */
      INT ops = find_min_sequence(code, len);
	
      char *final_code = code;
      INT   final_len  = len;
      bool  complement = false;
      INT   total = ops;
	
      /* find the 2's complement */
      char code1[64];
      INT  len1 = 0;
      if (first_1 >= 1) {
	INT c1 = (0x80000000 >> (first_1 - 1)) - c;
	start = false;
	for (INT i = 0; i < 32; ++i) {
	  if (c1 & (0x80000000 >> i)) {
	    if (start) {
	      code1[len1++] = 's';  /* shift */
	      code1[len1++] = '+';  /* plus  */
	    } else {
	      start = true;
	    }
	  } else if (start) {
	    code1[len1++] = 's';      /* shift */
	  }
	}
	Is_True(len1 >= 0 && len1 < 64, ("Just checking"));
	Is_True(len1 == 0 || code1[0] == 's', ("Just checking"));

	if (len1 >= 0) {
	  INT ops1 = 2 + find_min_sequence(code1, len1);
	  if (ops1 < ops) {
	    final_code = code1;
	    final_len = len1;
	    total = ops1;
	    complement = true;
	    // fprintf(stderr, "Using negate for %d\n", c);
	  }
	}
      }

      total = total + ((negate) ? 1 : 0);

	/* If the sequence is too long, don't replace the multiply. */
      if (total > Multiply_Limit(tree) ) {
	return tree;
      }

      /* transform */
      INT shift = 0;

      /* Add an assignment if the 'var' is not a leaf, 
	 otherwise, the lowered MPY may have redundant ops. P.T. #2728 */
      if (!OPCODE_is_leaf(WN_opcode(var))) {
	PREG_NUM t = AssignExpr(block, lower_copy_tree(var, actions), type);
	var = WN_LdidPreg(type, t);
      }

      rep = lower_copy_tree(var, actions);

      Is_True(rep, ("Just checking"));

      for (INT i = 0; i < final_len; i++) {
	switch(final_code[i]) {
	case 's':
	  shift++;
	  break;
	case '+':
	  if (shift != 0) {
	    rep = WN_Shl(type, rep, WN_Intconst(type, shift));
	    shift = 0;
	  }
	  rep = WN_Add(type, rep, lower_copy_tree(var, actions));
	  break;
	case '-':
	  if (shift != 0) {
	    rep = WN_Shl(type, rep, WN_Intconst(type, shift));
	    shift = 0;
	  }
	  rep = WN_Sub(type, rep, lower_copy_tree(var, actions));
	  break;
	default:
	  FmtAssert(0, ("Unknown action"));
	}
      }
	
      if (shift != 0) {
	rep = WN_Shl(type, rep, WN_Intconst(type, shift));
      }
      if (complement) {
	Is_True(final_code == code1 && first_1 >= 1 && first_1 <= 31,
		("Just checking"));
	WN *tmp = WN_Shl(type, lower_copy_tree(var, actions), 
			 WN_Intconst(type, 32-first_1));
	rep = WN_Sub(type, tmp, rep);
      }
      if (negate) {
	rep = WN_Neg(type, rep);
      }

#if 0
      fprintf(stderr, "Mutiply by constant\n");
      dump_tree(tree);
      dump_tree(rep);
      fprintf(stderr, "\n");
#endif

      WN_DELETE_Tree(tree);
      return rep;
    }
  }

  return tree;
}


/* ====================================================================
 *
 * WN *lower_div_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower divide by constant 
 *
 * ==================================================================== */
static WN *lower_div_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_operator(tree) == OPR_DIV, ("Not a divide")); 
  TYPE_ID type = WN_rtype(tree);

  WN *cst = NULL;
  WN *var = NULL;

  if (WN_operator(WN_kid1(tree)) == OPR_INTCONST)
  {
    cst = WN_kid1(tree);
    var = WN_kid0(tree);
  }

  if (cst != NULL)
  {
    WN *new_tree = NULL;
    BOOL negate = FALSE;
    
    /* We only lower divides by a power of 2. */
    INT64 c   = WN_const_val(cst);
    if (MTYPE_is_signed(type) && c < 0)
    {
      c = -c;
      negate = TRUE;
    }

    if (c == 1)
    {
      new_tree = lower_copy_tree(var, actions);
    }
    else if ((c != 0) && IS_POWER_OF_2(c))
    {
      INT shift = TARG_INT_Least_Sig_One(c);
      if (shift < 31)
      {
	switch (type)
	{
	case MTYPE_I1:
	case MTYPE_I2:
	case MTYPE_I4:
	{
	  if (Divide_Limit() < 4)
	    break;
	  
	  /* For signed we have to be careful that -(c-1) < 'var' < 0 becomes 0. */
	  PREG_NUM var_preg = Create_Preg(type, "div_by_const_tmp");
	  WN *stid = WN_StidIntoPreg(type, var_preg, MTYPE_To_PREG(Integer_type),
				     lower_expr(block, lower_copy_tree(var, actions), actions));
	  lower_map(stid, actions);
	  WN_INSERT_BlockLast(block, stid);
	  
	  WN *varplus = WN_Add(type, WN_LdidPreg(type, var_preg), WN_Intconst(type, c - 1));
	  WN *cmp = WN_GE(type, WN_LdidPreg(type, var_preg), WN_Intconst(type, 0));
	  WN *select = WN_Select(type, cmp, WN_LdidPreg(type, var_preg), varplus);
	  new_tree = WN_Ashr(type, select, WN_Intconst(type, shift));
	  break;
	}
	
	case MTYPE_U1:
	case MTYPE_U2:
	case MTYPE_U4:
	  /* For unsigned we can just shift right. */
	  new_tree = WN_Lshr(type, lower_copy_tree(var, actions), WN_Intconst(type, shift));
	  break;
	}
      }
    }

    /* Replace 'tree' if we were able to lower. */
    if (new_tree != NULL)
    {
      if (negate)
	new_tree = WN_Neg(type, new_tree);
      
      WN_DELETE_Tree(tree);
      return new_tree;
    }
  }

  return tree;
}


/* ====================================================================
 *
 * WN *lower_rem_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower remainder by constant 
 *
 * ==================================================================== */
static WN *lower_rem_by_const(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_operator(tree) == OPR_REM, ("Not a REM")); 
  TYPE_ID type = WN_rtype(tree);

  WN *cst = NULL;
  WN *var = NULL;

  if (WN_operator(WN_kid1(tree)) == OPR_INTCONST)
  {
    cst = WN_kid1(tree);
    var = WN_kid0(tree);
  }

  if (cst != NULL)
  {
    /* We only lower remainders by a power of 2. */
    WN *new_tree = NULL;
    INT64 c   = WN_const_val(cst);
    if (MTYPE_is_signed(type) && c < 0)
      c = -c;

    if (c == 1)
    {
      new_tree = WN_Intconst(type, 0);
    }
    else if ((c != 0) && IS_POWER_OF_2(c))
    {
      INT shift = TARG_INT_Least_Sig_One(c);
      if (shift < 31)
      {
	switch (type)
	{
	case MTYPE_I1:
	case MTYPE_I2:
	case MTYPE_I4:
	{
          if (c == 2) {
            // Special case for c = 2 !
            PREG_NUM var_preg = Create_Preg(type, "rem_by_const_tmp");
            WN *stid = WN_StidIntoPreg(type, var_preg, MTYPE_To_PREG(Integer_type),
                                       lower_expr(block, lower_copy_tree(var, actions), actions));
            lower_map(stid, actions);
            WN_INSERT_BlockLast(block, stid);

            WN *var_bit0 = WN_CreateExp1(OPR_EXTRACT_BITS, MTYPE_U4, MTYPE_V, WN_LdidPreg(type, var_preg));
            WN_set_bit_offset_size(var_bit0, 0, 1);

            PREG_NUM var_bit0_preg = Create_Preg(type, "rem_by_const_tmp1");
            WN *stid1 = WN_StidIntoPreg(type, var_bit0_preg, MTYPE_To_PREG(Integer_type), 
                                        lower_expr(block, var_bit0, actions));
            lower_map(stid1, actions);
            WN_INSERT_BlockLast(block, stid1);

            WN *var_neg_bit0 = WN_Neg(type, WN_LdidPreg(type, var_bit0_preg));
            WN *cmp = WN_GE(type, WN_LdidPreg(type, var_preg), WN_Intconst(type, 0));
            new_tree = WN_Select(type, cmp, WN_LdidPreg(type, var_bit0_preg), var_neg_bit0);
            break;
          }

	  if (Divide_Limit() < 5)
	    break;
	  
	  /* For signed we do 'var' - (('var' / 'c') * 'c') */
	  PREG_NUM var_preg = Create_Preg(type, "div_by_const_tmp");
	  WN *stid = WN_StidIntoPreg(type, var_preg, MTYPE_To_PREG(Integer_type),
				     lower_expr(block, lower_copy_tree(var, actions), actions));
	  lower_map(stid, actions);
	  WN_INSERT_BlockLast(block, stid);
	  
	  new_tree = lower_expr(block,
				WN_Sub(type,
				       WN_LdidPreg(type, var_preg),
				       WN_Mpy(type,
					      WN_Div(type,
						     WN_LdidPreg(type, var_preg),
						     WN_Intconst(type, c)),
					      WN_Intconst(type, c))),
				actions);
	  break;
	}
	
	case MTYPE_U1:
	case MTYPE_U2:
	case MTYPE_U4:
	  /* For unsigned we can just extract 'shift' bits. */
	  new_tree = WN_CreateExp1(OPR_EXTRACT_BITS, MTYPE_U4, MTYPE_V, lower_copy_tree(var, actions));
	  WN_set_bit_offset_size(new_tree, 0, shift);
	  break;
	}
      }
    }
    
    /* Replace 'tree' if we were able to lower. */
    if (new_tree != NULL)
    {
      WN_DELETE_Tree(tree);
      return new_tree;
    }
  }

  return tree;
}


/* ====================================================================
 *
 * WN *lower_land_lior(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * lower LAND and LIOR into explicit short-circuit expressions
 *
 * ==================================================================== */
static WN *lower_land_lior(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN *zero = WN_Intconst(MTYPE_I4, 0);
  WN *one = WN_Intconst(MTYPE_I4, 1);

  PREG_NUM result;
  if (WN_operator(tree) == OPR_LIOR)
    result = AssignExpr(block, one, MTYPE_I4);
  else
    result = AssignExpr(block, zero, MTYPE_I4);
  WN *done_label_wn = WN_NewLabel();
  LABEL_IDX done_new_label = WN_label_number(done_label_wn);
  WN *branch_wn1, *branch_wn2;
  WN *kid0 = lower_expr(block, WN_kid0(tree), actions);
  if (WN_operator(tree) == OPR_LIOR)
    branch_wn1 = WN_CreateTruebr(done_new_label, kid0);
  else
    branch_wn1 = WN_CreateFalsebr(done_new_label, kid0);
  WN_INSERT_BlockLast(block, branch_wn1);
  WN *kid1 = lower_expr(block, WN_kid1(tree), actions);
  if (WN_operator(tree) == OPR_LIOR)
    branch_wn2 = WN_CreateTruebr(done_new_label, kid1);
  else
    branch_wn2 = WN_CreateFalsebr(done_new_label, kid1);
  WN_INSERT_BlockLast(block, branch_wn2);
  if (WN_operator(tree) == OPR_LIOR)
    AssignExprToPreg(block, zero, result, MTYPE_I4);
  else
    AssignExprToPreg(block, one, result, MTYPE_I4);
  WN_INSERT_BlockLast(block, done_label_wn);

  WN_Delete(tree);
  return WN_EQ(Boolean_type, WN_LdidPreg(MTYPE_I4, result), 
	       WN_Intconst(MTYPE_I4, 1));
}


/* IPA can mark variables gp-relative, even if they are not allocated
 * in this source file. To deal with them, we explicitly check if a 
 * variable is gp-relative.
 */
static BOOL Symbol_Is_Base_Register (ST *sym)
{
  return ((ST_class(sym) == CLASS_BLOCK && STB_is_basereg(sym)) ||
          (ST_class(sym) == CLASS_VAR && ST_gprel(sym)));
}



/* ====================================================================
 *
 * WN *lower_split_sym_addrs(WN *parent, WN *tree, INT64 offset,
 *                           LOWER_ACTIONS actions)
 *
 * Split symbols into base/offset depending on type/class etc.
 *
 *
 * ==================================================================== */

static WN *lower_split_sym_addrs(WN *parent, WN *tree, INT64 offset,
				 LOWER_ACTIONS actions)
{
  ST *sym  = WN_st(tree);
  ST *base = ST_base(sym);
  INT64	newoffset = 0;
  BOOL linkonce = FALSE;

  if (traceSplitSymOff)
    return NULL;

  switch(ST_class(sym))
  {
  case CLASS_CONST:
    if (ST_sclass(sym) == SCLASS_MERGE_STRING) {
      Allocate_Object(sym);
      return NULL;
    }
   /*
    *  for non_shared, non-gprelative constants we want to expose the
    *  base for RVI
    */
    if (PIC_NONSHARED && (ST_size(sym) > Max_Sdata_Elt_Size))
    {
      Allocate_Object(sym);
      Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);
      return lower_base_reference(tree, parent, base, newoffset, actions);
    }
    return NULL;

  case CLASS_PREG:
    return NULL;

  case CLASS_FUNC:
    if (PIC_SHARED && ST_is_preemptible(sym))
    {
      return NULL;
    }
    break;

  case CLASS_BLOCK:
  case CLASS_VAR:
    Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);

    if (ST_is_uplevelTemp(sym))
    {
      return NULL;
    }
    if (ST_assigned_to_dedicated_preg(sym)) 
    {
	return NULL;
    }

    /* Is 'sym' allocated to a linkonce section? */
    if (ST_has_named_section(sym))
    {
#define LINKONCE_PREFIX ".gnu.linkonce."
      STR_IDX sym_sec_name_idx = Find_Section_Name_For_ST(sym);
      char *sym_sec_name = Index_To_Str(sym_sec_name_idx);
      if (sym_sec_name && !strncmp(sym_sec_name, LINKONCE_PREFIX, strlen(LINKONCE_PREFIX)))
	linkonce = TRUE;
    }
    
   /*
    *  We cannot keep lowering LDA's or we will forever recurse
    */
    if (WN_operator_is(tree, OPR_LDA) &&
	((base == sym) || linkonce))
      return NULL;

    switch(ST_sclass(sym))
    {
    case SCLASS_REG:
    case SCLASS_TEXT:
      return NULL;

    case SCLASS_FORMAL_REF:	
     /*
      *	 If we expand the base, we will lose the FORMAL_REF'ness of the ST
      *  and will not be able to dereference it later
      */
      if (base != sym)
      {
 	base =	sym;
      }
      break;

    case SCLASS_FORMAL:
     /*
      *  Do not allocate Formal_Arg_StkSeg, as it is not correct yet !!
      *  (it will eventually be .sp + <offset> )
      */
      return NULL;

    case SCLASS_AUTO:
      /* 
       * For now, never split a stack variable.
       * We only see stack variables with bases when we do regions,
       * and for now we want to keep those cases looking like the
       * regular cases for correctness.  But at a later date we should
       * try to split these in the region case, as that may allow a
       * base register to be allocated for things like FP+32000,
       * so we could then get 1-instruction accesses to large offsets.
       * But that requires a cgexp change too, so wait on it.
       */
#if 0
      if (Uses_Small_Offset(sym, offset))
        break;
#endif
      return NULL;

    case SCLASS_EXTERN:
     /*
      *	okay to split these. A dlopen() can never redefine these (unless
      * they are weak)
      */
      if (ST_is_weak_symbol(sym))
	return NULL;
      break;

    case SCLASS_COMMON:
     /*
      *	commons may be preempted by a dlopen() (if the new definition is
      * initialized) see wilson/lillian for details of this rather obscure
      * point suneel says not to worry about this though, so please direct
      * bugs to him
      *
      * In the new symbol table, we need to also split common symbols
      * here. The reason is that now both members and the common itself
      * are SCLASS_COMMON, whereas before they used to be SCLASS_BASED.
      * We no longer have to worry about the ST_Full that was present in
      * the old symbol table
      */
      sym = base;
      break;

    case SCLASS_PSTATIC:
	// is possible to have PSTATIC that is based on $sp
	// for altentry upformal.  In that case we don't want to
	// split, as the upformal offset has not been finalized yet.
	if (base == SP_Sym) {
		return NULL;
	}
	// else okay
	break;

    case SCLASS_UGLOBAL:
    case SCLASS_DGLOBAL:
    case SCLASS_FSTATIC:
     /*
      *  okay to split these. We will never split incorrectly as
      *	 Base_Symbol_And_Offset_For_Addressing() will not split a preemptible
      *  symbol
      */
#ifdef TARG_XTENSA
      /* We don't want to split symbols allocated to linkonce sections
	 since there is a problem with literals referring to
	 base+offset when the base is a linkonce data section (see
	 pr2587). */
      if (linkonce)
      {
	base = sym;
	newoffset = offset;
      }
#endif
      break;

    default:
      return NULL;
    }

    if (ST_gprel(sym))
    {
      return NULL;
    }

    tree = lower_base_reference(tree, parent, base, newoffset, actions);
    return tree;

  default:
    return NULL;
  }

  return NULL;
}




/* ====================================================================
 *
 * WN *lower_uplevel_reference(WN *tree, INT64 offset, LOWER_ACTIONS actions)
 *
 * Split uplevel symbols into indirect of slink
 *
 * ==================================================================== */

static WN *lower_uplevel_reference(WN *tree, INT64 offset,
				   LOWER_ACTIONS actions)
{
  ST	*sym = WN_st(tree);
  ST	*base;
  INT64	newoffset;

  Is_True(ST_is_uplevelTemp(sym), ("expected uplevel %s",ST_name(sym)));

  Base_Symbol_And_Offset_For_Addressing(sym, offset, &base, &newoffset);
  base = Find_Slink_For_ST(sym);

  tree = lower_dereference(tree, newoffset, base, 0, actions);
  return tree;
}




#ifdef TARG_XTENSA
/* ====================================================================
 *
 * WN *lower_nested_function_addr(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Set up trampoline for nested function address
 *
 * keep this in sync with __xtensa_trampoline_template and __xtensa_sync_caches
 * ==================================================================== */
static WN *lower_nested_function_addr(WN *block, WN *tree, 
				      LOWER_ACTIONS actions)
{
  int TRAMPOLINE_SIZE = Target_ABI == ABI_WINDOWED ? 59 : 34;
  int Lfnaddr_offset = Target_ABI == ABI_WINDOWED ? 16 : 12;
  int Lchainval_offset = Target_ABI == ABI_WINDOWED ? 12 : 8;
  WN *stack_space = WN_CreateAlloca(WN_Intconst(MTYPE_I4, TRAMPOLINE_SIZE));
  PREG_NUM stack_preg_num = Create_Preg(Pointer_Mtype, "stack_trampoline");
  WN *stid1 = WN_StidPreg(Pointer_Mtype, stack_preg_num, stack_space);
  WN_INSERT_BlockLast(block, stid1);

  WN *parms[3];
  TY_IDX pointer_ty = MTYPE_To_TY(Pointer_Mtype);
  ST *trampoline_st = New_ST(GLOBAL_SYMTAB);
  ST_Init(trampoline_st, Save_Str("__xtensa_trampoline_template"),
	  CLASS_VAR, SCLASS_EXTERN, EXPORT_PREEMPTIBLE, pointer_ty);
  parms[0] = WN_CreateParm(Pointer_Mtype,
			   WN_LdidPreg(Pointer_Mtype, stack_preg_num),
			   pointer_ty, WN_PARM_BY_VALUE);
  parms[1] = WN_CreateParm(Pointer_Mtype, 
			   WN_Lda(Pointer_Mtype, 0, trampoline_st),
			   pointer_ty, WN_PARM_BY_VALUE);
  parms[2] = WN_CreateParm(MTYPE_I4, WN_Intconst(MTYPE_I4, TRAMPOLINE_SIZE),
			   MTYPE_To_TY(MTYPE_I4), WN_PARM_BY_VALUE);
  WN *memcpy_call = WN_Generate_Intrinsic_Call(INTRN_MEMCPY, 3, parms);
  memcpy_call = lower_call(block, memcpy_call, actions);
  WN_INSERT_BlockLast(block, memcpy_call);
 
  TY_IDX ptrptr_ty = Make_Pointer_Type(pointer_ty);
  WN *istore1 = WN_Istore(Pointer_Mtype, Lfnaddr_offset, ptrptr_ty, 
			  WN_LdidPreg(Pointer_Mtype, stack_preg_num), tree);
  WN_INSERT_BlockLast(block, istore1);

  ST* callee_st = WN_st(tree);
  Is_True(callee_st != NULL, ("Can't find nested function symbol"));
  WN *static_chain;
  if (PU_lexical_level(&St_Table[WN_st_idx(current_function)]) <
      PU_lexical_level(callee_st)) {
    static_chain = WN_LdidPreg(Pointer_Mtype, Frame_Pointer_Preg_Offset);
  } else {
    ST  *slink = Find_Slink_For_Scope(WN_st(current_function), callee_st);
    static_chain = WN_Ldid (Pointer_Mtype, 0, slink, ST_type(slink));
  }
  WN *istore2 = WN_Istore(Pointer_Mtype, Lchainval_offset, ptrptr_ty, 
			  WN_LdidPreg(Pointer_Mtype, stack_preg_num), 
			  static_chain);
  WN_INSERT_BlockLast(block, istore2);

  TY_IDX ty2 = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
  ST *cache_flush_st = Gen_Intrinsic_Function(ty2, "__xtensa_sync_caches");
  WN *cache_flush_call = WN_Call(MTYPE_V, MTYPE_V, 0, cache_flush_st);
  cache_flush_call = lower_call(block, cache_flush_call, actions);
  WN_INSERT_BlockLast(block, cache_flush_call);

  return WN_LdidPreg(Pointer_Mtype, stack_preg_num);
}
#endif



/* ====================================================================
 *
 * WN *lower_formal_ref(WN *tree, INT64 offset, ST *base,
 *                      LOWER_ACTIONS actions)
 *
 * lower an SCLASS_FORMAL_REF into a dereference of the new base.
 * or
 * lower an SCLASS_FORMAL into a preg, already computed
 *
 * ==================================================================== */
static WN *lower_formal_ref(WN *tree, INT64 offset, ST *base,
			    LOWER_ACTIONS actions)
{
  PREG_NUM	preg;

  switch(ST_sclass(base))
  {
  case SCLASS_FORMAL_REF:
    base = Get_ST_formal_ref_base(base);
    if (preg = Get_ST_formal_preg_num(base))
    {
      Is_True((ST_has_nested_ref(WN_st(tree))==FALSE),
	      ("lower_formal_ref: cannot use preg for nested ref %s",
	       ST_name(WN_st(tree))));
      base = MTYPE_To_PREG(TY_mtype(Ty_Table[ST_type(base)]));
    }
    return lower_dereference(tree, offset, base, preg, actions);

  case SCLASS_FORMAL:
    if (preg= Get_ST_formal_preg_num(base))
    {
      base = MTYPE_To_PREG(TY_mtype(Ty_Table[ST_type(base)]));
      tree = lower_base_register(tree, base, preg, actions);
      return tree;
    }
    break;
  }
  return NULL;
}




/* ====================================================================
 *
 * WN *lower_base_register(WN *tree, ST *base, INT64 offset,
 *                         LOWER_ACTIONS actions)
 *
 * common routine for lowering to a base register for LDA, LDID, STID
 * into using a new base/offset
 *
 * ==================================================================== */

static WN *lower_base_register(WN *tree, ST *base, INT64 offset,
			       LOWER_ACTIONS actions)
{
  Is_True(WN_st(tree) != base, ("lower_base_register(): possible recursion"));

  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert
    *     LDA (offset) <sym> into
    *
    *     LDA (offset) <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_lda_offset(tree) = offset;
    return tree;

  case OPR_LDID:
   /*
    * Convert
    *	  LDID (offset> <sym>  into
    * 	  LDID (offset <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_load_offset(tree) = offset;
    break;

  case OPR_STID:
   /*
    * Convert
    *	  STID (offset> <sym>  into
    * 	  STID (offset <base>
    */
    WN_st_idx(tree) = ST_st_idx(base);
    WN_store_offset(tree) = offset;
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }

  return tree;
}


// Check if 'field_id' is enclosed by a union.
static bool
field_is_within_union (TY_IDX  struct_ty_idx, 
                       UINT    field_id, 
                       UINT&   cur_field_id,
                       bool parent_union =false)
{
  if (field_id == cur_field_id)
    return parent_union;
  
  if (TY_is_union(struct_ty_idx))
    parent_union = true;
  
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id)
      return parent_union;
    
    TY_IDX ty_idx = FLD_type(fld);
    if (TY_kind(ty_idx) == KIND_STRUCT && TY_fld(ty_idx) != FLD_HANDLE()) {
      bool within_union =
        field_is_within_union(ty_idx, field_id, cur_field_id, parent_union);
      if (cur_field_id >= field_id)
        return within_union;
    }
  } while (!FLD_last_field(fld_iter++));
  
  return false;
} 

// replace the type of LDID/STID nodes with non-zero field_id to the type
// of the field
static void
lower_field_id (WN* tree)
{
  OPERATOR opr = WN_operator(tree);

  Is_True (opr == OPR_LDID || opr == OPR_STID || opr == OPR_MLOAD ||
	   opr == OPR_MSTORE, ("expecting LDID or STID nodes"));

  if (WN_field_id (tree) == 0)
    return;

  BOOL is_ptr_type = (opr == OPR_MLOAD || opr == OPR_MSTORE);

  TY_IDX ty_idx = is_ptr_type ? TY_pointed (WN_ty (tree)) : WN_ty (tree);

  Is_True (TY_kind (ty_idx) == KIND_STRUCT,
	   ("non-zero field id must have KIND_STRUCT"));

  UINT field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (ty_idx, WN_field_id (tree), field_id); 
  
  Is_True (! fld.Is_Null (), ("invalid bit-field ID for %s",
			      OPERATOR_name(opr)));

  TY_IDX new_ty_idx = FLD_type(fld);
  {
    UINT cur_field_id = 0;
    if (field_is_within_union(ty_idx, WN_field_id(tree), cur_field_id)) {
      /* Disable ANSI type aliasing for lowered fields within unions. */
      new_ty_idx = Copy_TY(new_ty_idx);
      Set_TY_no_ansi_alias(new_ty_idx);
    }
  }

  /* Copy the volatile attribute to the new type. */
  if (TY_is_volatile(ty_idx))
    Set_TY_is_volatile(new_ty_idx);
  
  if (is_ptr_type) {
    WN_set_ty (tree, Make_Pointer_Type (new_ty_idx));
  } else {
    WN_set_ty (tree, new_ty_idx);
  }
  WN_set_field_id (tree, 0);
  return;
} // lower_field_id


/* ====================================================================
 *
 * WN *lower_base_reference(WN *tree, WN *parent, ST *base, INT64 offset,
 *                          LOWER_ACTIONS actions)
 *
 * common routine for lowering to a base reference for LDA, LDID, STID
 *
 * ==================================================================== */

static WN *lower_base_reference(WN *tree, WN *parent, ST *base, INT64 offset,
				LOWER_ACTIONS actions)
{
  WN    *addr, *wn;
  TYPE_ID type;
  
  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert
    *     LDA (offset) <sym>
    * 
    * (lo > 0)
    *       LDA (hi) <base>
    *       CONST (lo)
    *     ADD
    *
    * (offset >= 2GB)
    *       LDA (0) <base>
    *       CONST (offset)
    *     ADD
    */

    WN_st_idx(tree) = ST_st_idx(base);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, tree, WN_Intconst(Pointer_type, offset));
      WN_lda_offset(tree)=  0;
      return addr;
    }
    else if (Action(LOWER_SPLIT_CONST_OFFSETS)	&&
	     Memory_Offset_Lo(MTYPE_V, offset, parent, tree))
    {
     /* turn off simplifier else ADD may be removed */
      BOOL  simp=        WN_Simplifier_Enable(FALSE);

      addr =	WN_Add(Pointer_type,
		       tree,
		       WN_Intconst(Pointer_type,
				   Memory_Offset_Lo(MTYPE_V, offset, parent, tree)));
      WN_lda_offset(tree)= 	Memory_Offset_Hi(MTYPE_V, offset, parent, tree);

      WN_Simplifier_Enable(simp);
      return addr;
    }
    WN_lda_offset(tree)=  offset;
    return tree;

  case OPR_LDID:
  case OPR_LDBITS:
    type = WN_desc(tree);
    goto caddr;
    
  case OPR_STID:
  case OPR_STBITS:
    type = WN_rtype(tree);

    caddr:
   /*
    * Process the common address
    *
    * if (offset > 2GB)
    *      LDA (0) <sym>
    *	   CONST offset
    *    ADD
    *
    * if (offset > 16K)
    *      LDA (hi) <sym>
    *    ILOAD | ISTORE (lo)
    */
    addr =	WN_Lda(Pointer_type, 0, base);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, addr, WN_Intconst(Pointer_type, offset));
      offset =	0;
    }
    if (Action(LOWER_SPLIT_CONST_OFFSETS) &&
	Memory_Offset_Must_Split(type, offset, parent, tree))
    {
      WN_lda_offset(addr) =	Memory_Offset_Hi(type, offset, parent, tree);
      offset =			Memory_Offset_Lo(type, offset, parent, tree);
    }
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }	

  switch (WN_operator(tree))
  {
  case OPR_LDID:
   /*
    * Convert (LDID () <sym>) into
    *      LDA (hi) <base>
    *    ILOAD (low)
    */
    if (WN_field_id (tree) != 0)
      lower_field_id (tree);
    
    wn =	WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree),
			  addr);
    break;
  case OPR_LDBITS:
    wn =	WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree),
			  addr);
    WN_set_operator(wn, OPR_ILDBITS);
    WN_set_bit_offset_size(wn, WN_bit_offset(tree), WN_bit_size(tree));
    break;

  case OPR_STID:
   /*
    * Convert   STID (offset) <sym>
    *  into
    *       LDA (hi) <base>
    *     ISTORE (low)
    */
    if (WN_field_id (tree) != 0)
      lower_field_id (tree);
    
    wn =	WN_Istore(WN_desc(tree),
			  offset,
			  // The resulting TY is not an f90 pointer
			  // type because we're converting an STID.
			  // In the case where this STID
			  // came about because of conversion of
			  // ISTORE(something) in the past, and that
			  // ISTORE was through an f90 pointer, the
			  // destination of the STID will have the
			  // F90_TARGET attribute, and we're OK.
			  Make_Pointer_Type(WN_ty(tree)),
			  addr,
			  WN_kid0(tree));
    WN_Set_Linenum(wn, WN_Get_Linenum(tree));
    break;
  case OPR_STBITS:
    wn =	WN_Istore(WN_desc(tree),
			  offset,
			  Make_Pointer_Type(WN_ty(tree)),
			  addr,
			  WN_kid0(tree));
    WN_set_operator(wn, OPR_ISTBITS);
    WN_set_bit_offset_size(wn, WN_bit_offset(tree), WN_bit_size(tree));
    WN_Set_Linenum(wn, WN_Get_Linenum(tree));
    break;
  }	

  lower_copy_maps(tree, wn, actions);

  // need to update the offset canonicalizer's record to avoid dangling references
  // see PR10557
  Delete_Offset_Wn_Delete_Tree(tree);

  WN_Delete(tree);

  return wn;
}



/* ====================================================================
 *
 * WN *lower_dereference(WN *tree, INT64 offset, ST *base,
 *                       PREG_NUM *preg, LOWER_ACTIONS actions)
 *
 * Perform address dereferencing for SCLASS_FORMAL_REF and uplevel variable
 * Caller is responsible for any further lowering.
 *
 *  For uplevel references the idea is to replace the st with a reference
 *  of the slink_sym at level ST_level(st) + 1
 *  This will only work recursively
 *  (ie. that sym must be referenced via the next level's scope
 *
 * ==================================================================== */
static WN
*lower_dereference(WN *tree, INT64 offset, ST *base, PREG_NUM preg,
		   LOWER_ACTIONS actions)
{
  WN	 *addr, *deref;
  TY_IDX  addr_ty;

  switch (WN_operator(tree))
  {
  case OPR_LDA:
   /*
    * Convert uplevel reference
    *     LDA (offset) <sym>
    *  into 
    *       LDID (0) <base>
    *       CONST (offset)    
    *     ADD
    */
    deref =	WN_Ldid(Pointer_type, preg, base, WN_ty(tree));
    addr =	WN_Add(Pointer_type, deref, WN_Intconst(Pointer_type, offset));
    return addr;

  case OPR_LDID:
  case OPR_STID:
   /*
    * Process the common address
    *
    */
    addr_ty = Make_Pointer_Type(WN_ty(tree));
    addr =	WN_Ldid(Pointer_type, preg, base, addr_ty);

    if (mem_offset_2GB(offset))
    {
      addr =	WN_Add(Pointer_type, addr, WN_Intconst(Pointer_type, offset));
      offset =	0;
    }
    break;

  default:
    Is_True(FALSE,("expected lda/ldid/stid"));
    return NULL;
  }	

  switch (WN_operator(tree))
  {
  case OPR_LDID:
   /*
    * Convert uplevel reference  LDID () <sym>
    * into
    *      LDID (preg) <base>
    *    ILOAD (offset)
    */
    deref = WN_RIload(WN_rtype(tree), WN_desc(tree), offset, WN_ty(tree), addr);
    break;

  case OPR_STID:
   /*
    * Convert  STID () <sym>
    * into
    *       LDID (preg) <base> 
    *     ISTORE (offset)          
    */
    deref = WN_Istore(WN_desc(tree), offset, addr_ty, addr, WN_kid0(tree));
    WN_Set_Linenum(deref, WN_Get_Linenum(tree));
    break;
  }	

  lower_copy_maps(tree, deref, actions);
  WN_Delete(tree);

  return deref;
}

/* ====================================================================
 *
 * WN *lower_return_ldid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on 'LDID Return_Val_Preg'
 * nodes returning a lowered 'LDID normal_PREG' node.
 *
 * ==================================================================== */

static WN *lower_return_ldid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX   ty_idx  = WN_ty(tree);
  TY&      ty      = Ty_Table[ty_idx];
  TYPE_ID  mtype   = TY_mtype (ty);

  Is_True((WN_operator(tree) == OPR_LDID),
	  ("expected LDID node, not %s", OPCODE_name(WN_opcode(tree))));

  if (WN_st(tree) == Tie_Output_Volatile_Preg) {
    /* for TIE intrinsic output arguments, the ldid is not lowered */
    return tree;
  }

  switch (mtype) {

    case MTYPE_I8:
    case MTYPE_U8:
      WN_st_idx(tree) = ST_st_idx(MTYPE_To_PREG(mtype));
      WN_load_offset(tree) = Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Integer());
      return tree;

    case MTYPE_I1:
    case MTYPE_I2:
    case MTYPE_I4:
    case MTYPE_U1:
    case MTYPE_U2:
    case MTYPE_U4:
      WN_st_idx(tree) = ST_st_idx(Int_Preg);
      WN_load_offset(tree) = Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Integer());
      return tree;

    case MTYPE_F4:
    case MTYPE_F8:
    case MTYPE_FQ:
    case MTYPE_C4:
    case MTYPE_C8:
    case MTYPE_CQ:
#ifndef TARG_XTENSA
      WN_st_idx(tree) = ST_st_idx(Float_Preg);
      WN_load_offset(tree) = Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Float());
      return tree;
#endif

    case MTYPE_M:
#ifndef TARG_XTENSA
      Fail_FmtAssertion ("MLDID of Return_Val_Preg not allowed in middle"
			 " of expression");
      /*NOTREACHED*/
#endif

    case MTYPE_XTBOOL:
    case MTYPE_XTBOOL2:
    case MTYPE_XTBOOL4:
    case MTYPE_XTBOOL8:
    case MTYPE_XTBOOL16:
    {
      RETURN_INFO return_info = Get_Return_Info(WN_ty(tree),
				      Use_Simulated, Return_Info_Incoming);
      WN_st_idx(tree) = ST_st_idx(MTYPE_To_PREG(mtype));
      WN_load_offset(tree) = RETURN_INFO_preg(return_info,0);
      return tree;
    }

    default:
      if (MTYPE_is_tie(mtype)) {
  	RETURN_INFO return_info = Get_Return_Info(WN_ty(tree),
				      Use_Simulated, Return_Info_Incoming);
        WN_st_idx(tree) = ST_st_idx(MTYPE_To_PREG(mtype));
        WN_load_offset(tree) = RETURN_INFO_preg(return_info,0);
        return tree;
      }
      Fail_FmtAssertion ("Unexpected type in lower_return_ldid");
      /*NOTREACHED*/
  }
}

static TY_IDX
get_field_type (TY_IDX struct_type, UINT field_id)
{
  Is_True (TY_kind (struct_type) == KIND_STRUCT, ("expecting KIND_STRUCT"));
  UINT cur_field_id = 0;
  FLD_HANDLE fld = FLD_get_to_field (struct_type, field_id, cur_field_id);
  Is_True (! fld.Is_Null(), ("Invalid field id %d for type 0x%x",
			     field_id, struct_type));
  return FLD_type (fld);
}

/* ====================================================================
 *
 * WN *lower_mldid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MLDID nodes returning
 * an equivalent MLOAD node.  Note that MLDID's of Return_Val_Preg are
 * not lowered here.  They are handled by lower_return_ldid.
 *
 * ==================================================================== */

static WN *lower_mldid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX ty_idx  = WN_ty(tree);
  TY_IDX pty_idx;
  UINT64 size    = WN_field_id(tree) == 0 ?
      TY_size(ty_idx) :
      TY_size(get_field_type (ty_idx, WN_field_id (tree)));
  WN*    wn;
  WN*    awn;
  WN*    swn;

  Is_True((WN_opcode(tree) == OPC_MMLDID),
	  ("expected mldid node, not %s", OPCODE_name(WN_opcode(tree))));

  // Within an ASM lower MLDID into an LDID of its single field
  if (Action(LOWER_MLDID_IN_ASM)) {
    UINT field_id = 0;
    FLD_HANDLE fld = (WN_field_id(tree) == 0 ?
                      TY_fld(ty_idx) : 
                      FLD_get_to_field(ty_idx, WN_field_id(tree), field_id));

    wn = WN_CreateLdid(OPR_LDID,
                       TY_mtype(FLD_type(fld)), // rtype
                       TY_mtype(FLD_type(fld)), // desc
                       WN_load_offset(tree),    // offset
                       WN_st(tree),             // ST*
                       FLD_type(fld));          // TY_IDX
  }
  else {
    pty_idx = Make_Pointer_Type (ty_idx, FALSE);

    swn = WN_CreateIntconst(OPC_U4INTCONST, size);
    awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, WN_load_offset(tree),
                       pty_idx, WN_st(tree));
    wn  = WN_CreateMload(0, pty_idx, awn, swn);
    WN_set_field_id(wn, WN_field_id(tree));
  }
  
  lower_copy_maps(tree, wn, actions);
  wn  = lower_expr(block, wn, actions);

  WN_copy_linenum(tree, wn);
  WN_Delete(tree);
  return wn;
}

/* ====================================================================
 *
 * WN *lower_miload(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MILOAD nodes returning
 * an equivalent MLOAD node.
 *
 * ==================================================================== */

static WN *lower_miload(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX ty_idx  = WN_field_id (tree) == 0 ?
    WN_ty(tree) :
    get_field_type (WN_ty (tree), WN_field_id (tree));
  TY_IDX pty_idx = WN_load_addr_ty(tree);
  UINT64 size    = TY_size(Ty_Table[ty_idx]);
  WN*    wn;
  WN*    swn;

  Is_True((WN_opcode(tree) == OPC_MMILOAD),
	  ("expected miload node, not %s", OPCODE_name(WN_opcode(tree))));

  swn = WN_CreateIntconst (OPC_U4INTCONST, size);
  wn  = WN_CreateMload (WN_offset(tree), pty_idx, WN_kid0(tree), swn);
  WN_set_field_id(wn, WN_field_id(tree));
  wn  = lower_expr (block, wn, actions);
  WN_copy_linenum(tree, wn);

  lower_copy_maps(tree, wn, actions);
  WN_Delete (tree);
  return wn;
}

/*
  lower_load_packed_bits() is used to lower a load of a packed
  bit field, which happens to occupy 1 more byte than its declared
  type.  For example:

    struct {
      int c:4;
      int c1:31;
      int c2:5;
    };

    c1 normally occupies 4 bytes (int). But when compiled under 
    -fpack-struct, it will occupy five bytes, sharing the
    first byte with c and the last byte with c2.

  This function handles 64 bit field as well.
*/
static WN *
lower_load_packed_bits (WN* block, WN* wn, LOWER_ACTIONS actions)
{
  TYPE_ID rtype = WN_rtype (wn);
  TYPE_ID desc = WN_desc (wn);

  INT delta = MTYPE_bit_size(rtype) - MTYPE_bit_size(desc);
  if (delta < 0) {
    rtype = Mtype_TransferSize (desc, rtype);
    delta = 0;
  }

  INT bit_size = WN_bit_size (wn);
  INT bit_offset = WN_bit_offset (wn);
  INT bytes_accessed = MTYPE_bit_size(desc)/8;

  // Don't let Simplifier get rid of the copy of whirl node!
  BOOL simp = WN_Simplifier_Enable(FALSE);

  WN* wn_low = wn;
  WN *wn_high = WN_COPY_Tree(wn);
  INT wn_high_bit_size = bit_size + bit_offset - MTYPE_bit_size(desc);
  INT wn_low_bit_size = bit_size - wn_high_bit_size; 
  WN_load_offset(wn_high) = WN_load_offset(wn_high) + bytes_accessed;

  INT left_shift;
  if (Target_Byte_Sex == BIG_ENDIAN) {
    left_shift = bit_offset;
    WN *tmp = wn_low;
    wn_low = wn_high;
    wn_high = tmp;
    INT t = wn_high_bit_size;
    wn_high_bit_size = wn_low_bit_size;
    wn_low_bit_size  = t;
  } else {
    left_shift = MTYPE_bit_size(desc) - wn_high_bit_size;
  }
  BOOL bits_signed = MTYPE_signed(rtype);
  WN *tree_low = wn_low, *tree_high = wn_high;

  FmtAssert(left_shift > 0, ("Left shift amount cannot be negative nor zero"));
  left_shift += delta;
  tree_high = WN_Shl (rtype, tree_high, WN_Intconst (MTYPE_I4, left_shift));

  INT right_shift1 = (MTYPE_bit_size(rtype) - delta) - wn_low_bit_size;
  INT right_shift2 = right_shift1 - wn_high_bit_size;
  if (right_shift1 > 0) {
    tree_low = WN_Lshr (rtype, tree_low, WN_Intconst (MTYPE_I4, right_shift1));
  }
  if (right_shift2 > 0) {
    OPERATOR opr = bits_signed ? OPR_ASHR : OPR_LSHR;
    tree_high = WN_Binary (opr, rtype, tree_high, WN_Intconst (MTYPE_I4, right_shift2));
  }

  WN *tree = WN_Bior(rtype, tree_low, tree_high);

  TYPE_ID orig_rtype = WN_rtype (wn);

  WN_set_rtype (wn_low, rtype);
  WN_set_bit_offset_size (wn_low, 0, 0);
  WN_set_operator (wn_low, WN_operator (wn_low) == OPR_LDBITS ? OPR_LDID : OPR_ILOAD);
  WN_set_rtype (wn_high, rtype);
  WN_set_bit_offset_size (wn_high, 0, 0);
  WN_set_operator (wn_high, WN_operator (wn_high) == OPR_LDBITS ? OPR_LDID : OPR_ILOAD);

  if (rtype != orig_rtype)
    tree = WN_Type_Conversion (tree, orig_rtype);

  WN_Simplifier_Enable(simp);
  return lower_expr (block, tree, actions);
}

/* ====================================================================
 *
 * lower_load_bits
 *
 * lower LDBITS and ILDBITS into shifts
 *
 * ==================================================================== */
static WN*
lower_load_bits (WN* block, WN* wn, LOWER_ACTIONS actions)
{
  Is_True (WN_operator (wn) == OPR_LDBITS || WN_operator (wn) == OPR_ILDBITS,
	   ("expected LDBITS or ILDBITS, not %s",
	    OPERATOR_name(WN_operator(wn))));

  TYPE_ID rtype = WN_rtype (wn);
  TYPE_ID desc = WN_desc (wn);
  INT bit_size = WN_bit_size (wn);
  INT bit_offset = WN_bit_offset (wn);
  INT bytes_occupied = (bit_offset + bit_size - 1)/8 - bit_offset/8 + 1;
  INT bytes_accessed = MTYPE_bit_size(desc)/8;
  if (bytes_occupied > bytes_accessed) {
    return lower_load_packed_bits (block, wn, actions);
  }

  INT delta = MTYPE_bit_size(rtype) - MTYPE_bit_size(desc);
  if (delta < 0) {
    rtype = Mtype_TransferSize (desc, rtype);
    delta = 0;
  }

  WN* tree = wn;

  INT bit_ofst = Target_Byte_Sex == BIG_ENDIAN ?
    WN_bit_offset (wn) :
    MTYPE_bit_size(desc) - bit_size - WN_bit_offset (wn);
  BOOL bits_signed = MTYPE_signed(rtype);
  
  if (bit_ofst == 0)
    bit_size += delta;
  else {
    bit_ofst += delta;
    if (bits_signed)
      tree = WN_Shl (rtype, tree, WN_Intconst (MTYPE_I4, bit_ofst));
    else {
      INT shift_count = 64 - (MTYPE_bit_size(rtype) - bit_ofst);
      mUINT64 mask = (~(mUINT64)0) >> shift_count;
      tree = WN_Band (rtype, tree,
		      WN_Intconst (Mtype_TransferSign (MTYPE_U4, rtype),
				   mask));
      bit_size += bit_ofst;
    }
  }

  INT right_shift = MTYPE_bit_size(rtype) - bit_size;

  if (right_shift > 0) {
    OPERATOR opr = bits_signed ? OPR_ASHR : OPR_LSHR;
    tree = WN_Binary (opr, rtype, tree, WN_Intconst (MTYPE_I4, right_shift));
  }

  TYPE_ID orig_rtype = WN_rtype (wn);
  
  WN_set_rtype (wn, rtype);
  WN_set_bit_offset_size (wn, 0, 0);
  WN_set_operator (wn, WN_operator (wn) == OPR_LDBITS ? OPR_LDID : OPR_ILOAD);

  if (rtype != orig_rtype)
    tree = WN_Type_Conversion (tree, orig_rtype);

  return lower_expr (block, tree, actions);
} // lower_load_bits

/*
  To lower a store of a packed bit field when the packed
  bit field occupies more bytes than that of its declared
  type. See lower_load_packed_bits() for an example.
*/ 
static WN*
lower_store_packed_bits(WN* block, WN* wn, LOWER_ACTIONS actions)
{
  TYPE_ID desc = WN_desc (wn);
  INT bit_size = WN_bit_size (wn);
  INT bit_offset = WN_bit_offset (wn);

  INT bytes_accessed = MTYPE_bit_size(desc)/8;
  INT bit_size_high = bit_size + bit_offset - MTYPE_bit_size(desc);
  INT bit_size_low  = bit_size - bit_size_high;
  WN *orig_value_high, *orig_value_low;
  WN *load_address_low, *load_address_high;

  // Don't let Simplifier get rid of the copy of whirl node!
  BOOL simp = WN_Simplifier_Enable(FALSE);

  WN *wn_low = wn;
  WN *wn_high = WN_COPY_Tree(wn);
  WN_load_offset(wn_high) = WN_load_offset(wn_high) + bytes_accessed;

  if (WN_operator (wn) == OPR_ISTBITS) {
    load_address_low = lower_copy_tree (WN_kid1 (wn), actions);
    load_address_high = WN_COPY_Tree(load_address_low);
    orig_value_low = WN_Iload (
                       Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
                       WN_offset (wn), 
                       TY_pointed(WN_ty (wn)), load_address_low, 0);
    orig_value_high = WN_Iload (
                       Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
                       WN_offset (wn)+bytes_accessed,
                       TY_pointed(WN_ty (wn)), load_address_high, 0);
  } else {
    orig_value_low = WN_Ldid (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
                          WN_offset (wn), WN_st_idx (wn), WN_ty (wn), 0);
    orig_value_high = WN_Ldid (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
                          WN_offset (wn)+bytes_accessed, WN_st_idx (wn), WN_ty (wn), 0);
  }

  TYPE_ID cmp_type = WN_rtype (orig_value_low);

  if (Target_Byte_Sex == BIG_ENDIAN ) {

    WN *wn_t = wn_low;
    wn_low = wn_high;
    wn_high = wn_t;

    wn_t = orig_value_low;
    orig_value_low = orig_value_high;
    orig_value_high = wn_t;

    INT it;
    it = bit_size_high;
    bit_size_high = bit_size_low;
    bit_size_low  = it;
  }

  mUINT64 mask_l = (~((mUINT64)0)) >> (64 - MTYPE_bit_size (desc) + bit_size_low);
  orig_value_low = WN_Band (cmp_type, orig_value_low, WN_Intconst (cmp_type, mask_l));
  mUINT64 mask_h = (~((mUINT64)0)) << bit_size_high;
  orig_value_high = WN_Band (cmp_type, orig_value_high, WN_Intconst (cmp_type, mask_h));

  // Process stored value
  WN* new_value = WN_kid0 (wn);

  // check if we need to sign-extend the value
  if (bit_size > MTYPE_bit_size (WN_rtype (new_value))) {
    new_value =
      WN_CreateCvtl (OPR_CVTL,
                     Mtype_TransferSign (WN_rtype (new_value), cmp_type),
                     WN_rtype (new_value),
                     MTYPE_bit_size (WN_rtype (new_value)),
                     new_value);
  }

  // now, set other bits to zero
  mUINT64 mask = ~((mUINT64)0) >> (64 - bit_size);
  new_value =
    WN_Band (cmp_type, new_value,
             WN_Intconst (Mtype_TransferSize (WN_rtype (new_value), cmp_type),
                          mask));

   // move the bits to the right position
  WN *new_value_low = new_value;
  WN *new_value_high = WN_COPY_Tree(new_value);
  INT shift = Target_Byte_Sex == BIG_ENDIAN ?
              MTYPE_bit_size (desc) - bit_size_low :
              bit_offset;
  
  new_value_low = WN_Shl (cmp_type, new_value_low, WN_Intconst (MTYPE_U4, shift));
  new_value_high = WN_Lshr (cmp_type, new_value_high, WN_Intconst (MTYPE_U4, bit_size_low));

  // set the value with bitwise or
  new_value_low  = WN_Bior (cmp_type, orig_value_low, new_value_low);
  new_value_high = WN_Bior (cmp_type, orig_value_high, new_value_high);

  WN_kid0 (wn_low) = new_value_low;
  WN_set_bit_offset_size (wn_low, 0, 0);
  WN_set_operator (wn_low, WN_operator(wn_low) == OPR_STBITS ? OPR_STID : OPR_ISTORE);
  WN_kid0 (wn_high) = new_value_high;
  WN_set_bit_offset_size (wn_high, 0, 0);
  WN_set_operator (wn_high, WN_operator(wn_high) == OPR_STBITS ? OPR_STID : OPR_ISTORE);

  WN *tree = lower_store(block, wn_low, actions);
  WN_copy_linenum(wn_low, tree);
  WN_INSERT_BlockLast(block, tree);

  WN_Simplifier_Enable(simp);

  return lower_store (block, wn_high, actions);
}

/* ====================================================================
 *
 * lower_store_bits
 *
 * lower STBITS and ISTBITS
 *
 * ==================================================================== */
static WN*
lower_store_bits (WN* block, WN* wn, LOWER_ACTIONS actions)
{
  Is_True (WN_operator (wn) == OPR_STBITS || WN_operator (wn) == OPR_ISTBITS,
	   ("expected STBITS or ISTBITS, not %s",
	    OPERATOR_name (WN_operator (wn))));

  INT bit_size = WN_bit_size (wn);
  INT bit_ofst = WN_bit_offset (wn); 
  INT bytes_occupied = (bit_ofst + bit_size - 1)/8 - bit_ofst/8 + 1;
  INT bytes_accessed = MTYPE_bit_size(WN_desc(wn))/8;
  if (bytes_occupied > bytes_accessed) {
    return lower_store_packed_bits (block, wn, actions);
  }

  WN* orig_value;

  if (WN_operator (wn) == OPR_ISTBITS) {
    WN* load_address = lower_copy_tree (WN_kid1 (wn), actions);
    orig_value = WN_Iload (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
			   WN_offset (wn), TY_pointed(WN_ty (wn)), load_address, 0);
  } else 
    orig_value = WN_Ldid (Mtype_TransferSign (MTYPE_U4, WN_desc (wn)),
			  WN_offset (wn), WN_st_idx (wn), WN_ty (wn), 0);
  
  TYPE_ID cmp_type = WN_rtype (orig_value);
  
  INT shift = Target_Byte_Sex == BIG_ENDIAN ?
      MTYPE_bit_size (WN_desc (wn)) - bit_ofst - bit_size :
      bit_ofst;
  mUINT64 mask = ~((~((mUINT64)0) >> (64 - bit_size)) << shift);
  orig_value = WN_Band (cmp_type, orig_value, WN_Intconst (cmp_type, mask));

  WN* new_value = WN_kid0 (wn);

  // check if we need to sign-extend the value
  if (bit_size > MTYPE_bit_size (WN_rtype (new_value)))
    new_value =
      WN_CreateCvtl (OPR_CVTL,
		     Mtype_TransferSign (WN_rtype (new_value), cmp_type),
		     WN_rtype (new_value),
		     MTYPE_bit_size (WN_rtype (new_value)),
		     new_value);
  
  // now, truncate to the right bit size for store
  mask = ~((mUINT64)0) >> (64 - bit_size);
  new_value =
    WN_Band (cmp_type, new_value,
	     WN_Intconst (Mtype_TransferSize (WN_rtype (new_value), cmp_type),
			  mask));

  // move the bits to the right position
  if (shift > 0) {
    // shift amount should be MTYPE_U4
    new_value = WN_Shl (cmp_type, new_value, WN_Intconst (MTYPE_U4, shift));
  }

  // set the value with bitwise or
  new_value = WN_Bior (cmp_type, orig_value, new_value);

  WN_kid0 (wn) = new_value;
  WN_set_bit_offset_size (wn, 0, 0);
  WN_set_operator (wn, WN_operator(wn) == OPR_STBITS ? OPR_STID : OPR_ISTORE);

  return lower_store (block, wn, actions);
} // lower_store_bits

#if 0
/* ====================================================================
 *
 * check_unaligned
 *
 * required_alignment is the natural alignment; offset is the actual offset
 * used in the current access; ty_align is the alignment in the TY of the
 * current access.  Return whether the access is unaligned.
 *
 * ==================================================================== */
static bool check_unaligned(INT required_alignment, INT offset, INT ty_align)
{
  if (required_alignment <= 1)
    return FALSE;
  INT align = ty_align;
  if (offset) {
    INT offset_align = offset % required_alignment;
    if (offset_align)
      align = MIN(align, offset_align);
  }
  return align < required_alignment;
}
#endif


// --------------------------------------------------------------------
// This function mimics FLD_get_to_field from common/com/symtab.cxx,
// but it also computes the offset of <field_id> within <struct_ty_idx>
// We need this because FLD_ofst only gives the offset within the first
// enclosing struct.
// --------------------------------------------------------------------
FLD_HANDLE
FLD_And_Offset_From_Field_Id (TY_IDX  struct_ty_idx, 
                              UINT    field_id, 
                              UINT&   cur_field_id,
                              UINT64& offset)
{
  FLD_ITER fld_iter = Make_fld_iter(TY_fld(struct_ty_idx));
  do {
    FLD_HANDLE fld(fld_iter);
    cur_field_id++;
    if (cur_field_id == field_id) {
      offset += FLD_ofst(fld);
      return fld;
    }
    if (TY_kind(FLD_type(fld)) == KIND_STRUCT &&
        TY_fld(FLD_type(fld)) != FLD_HANDLE()) {
      UINT64 nested_offset = offset + FLD_ofst(fld);
      fld = FLD_And_Offset_From_Field_Id(FLD_type(fld), field_id, 
                                         cur_field_id, nested_offset);
      if (cur_field_id == field_id) {
        offset = nested_offset;
        return fld;
      }
    }
  } while (!FLD_last_field(fld_iter++));
  
  return FLD_HANDLE();
} 

/* ====================================================================
 *
 * lower_bit_field_id
 *
 * The given LDID/STID/ILOAD/ISTORE node has desc type MTYPE_BS.  Lower
 * the node by changing the field_id to bit_offset/bit_size combination.
 * The desc field is changed to reflect the unit in memory to load, and
 * the offset field may need to be updated.
 *
 * ==================================================================== */
static void lower_bit_field_id(WN *wn)
{
  TY_IDX struct_ty_idx;
  TY_IDX ty_idx;
  TYPE_ID rtype;
  OPERATOR opr = WN_operator(wn);
  OPERATOR new_opr;
  BOOL indirect;
  if (opr == OPR_LDID || opr == OPR_STID) {
    ST_IDX st_idx = WN_st_idx(wn);
    struct_ty_idx = WN_ty(wn);
    new_opr = (opr == OPR_LDID) ? OPR_LDBITS : OPR_STBITS;
    indirect = FALSE;
  }
  else {
    if (WN_operator(wn) == OPR_ILOAD) {
      ty_idx = WN_load_addr_ty(wn);
      new_opr = OPR_ILDBITS;
    }
    else {	// ISTORE
      ty_idx = WN_ty(wn);
      new_opr = OPR_ISTBITS;
    }
    Is_True(TY_kind(ty_idx) == KIND_POINTER,
	    ("addr ty not pointer type for %s", OPERATOR_name(opr)));
    struct_ty_idx = TY_pointed(ty_idx);
    indirect = TRUE;
  }
  Is_True(TY_kind(struct_ty_idx) == KIND_STRUCT,
	  ("struct type not associated with bit-field access for %s", OPERATOR_name(opr)));
  UINT cur_field_id = 0;
  UINT64 field_offset = 0;
  FLD_HANDLE fld = FLD_And_Offset_From_Field_Id(struct_ty_idx, 
                                                WN_field_id(wn),
                                                cur_field_id,
                                                field_offset); 
  Is_True(! fld.Is_Null(),
	  ("invalid bit-field ID for %s", OPERATOR_name(opr)));
  TY_IDX fld_ty_idx = FLD_type(fld);
  
  TY_IDX new_ty_idx = fld_ty_idx;
  {
    UINT cur_field_id = 0;
    if (field_is_within_union(struct_ty_idx, WN_field_id(wn), cur_field_id)) {
      /* Disable ANSI type aliasing for lowered fields within unions. */
      new_ty_idx = Copy_TY(new_ty_idx);
      Set_TY_no_ansi_alias(new_ty_idx);
    }
  }

  /* Copy the volatile attribute to the new type. */
  if (TY_is_volatile(struct_ty_idx))
    Set_TY_is_volatile(new_ty_idx);

  if (opr == OPR_ISTORE) {
    WN_set_ty (wn, Make_Pointer_Type (new_ty_idx, FALSE));
  } else {
    WN_set_ty (wn, new_ty_idx);
  }

  if (new_opr == OPR_ILDBITS) {
    /* Update the load high-level pointer type for consistency and better
       optimizations. */
    WN_set_load_addr_ty (wn, Make_Pointer_Type(new_ty_idx, FALSE));
  }

  Is_True(FLD_is_bit_field(fld),
	  ("non-bit-field associated with bit-field access for  %s", OPERATOR_name(opr)));

  // analyze bit field accessed
  UINT bytes_accessed = TY_size(fld_ty_idx);
  if (OPERATOR_is_store(new_opr))
    rtype = TY_mtype(fld_ty_idx);
  else rtype = WN_rtype(wn);
  INT ofst = field_offset;
#if 0
  BOOL unaligned_field = check_unaligned(bytes_accessed * 8, ofst,
					 TY_align(struct_ty_idx));
#endif
  if (ofst >= 0)
    ofst = ofst / bytes_accessed * bytes_accessed;
  else ofst =  (ofst - bytes_accessed + 1) / bytes_accessed * bytes_accessed;
  UINT bsize = FLD_bsize(fld);
  UINT bofst = FLD_bofst(fld) + (field_offset-ofst) * 8;
  if ((bofst + bsize) > (bytes_accessed * 8)) {
    if ( bytes_accessed == MTYPE_byte_size(Max_Int_Mtype) ||
         bytes_accessed == MTYPE_byte_size(MTYPE_U8) ) { 
      // can't enlarge; reverse the adjustment
      ofst = field_offset;
      bofst = FLD_bofst(fld);
    }
    else bytes_accessed *= 2;
  }
  WN_load_offset(wn) = WN_load_offset(wn) + ofst; 
  WN_set_field_id(wn, 0);

  if ((xt_short_bitfields) &&		   // we allow bitfield accesses of less than 32 bits
      (bsize & 7) == 0 && 		   // field size multiple of bytes
      (bytes_accessed * 8 % bsize) == 0 && // bytes_accessed multiple of bsize
      (bofst % bsize) == 0) {  		   // bofst multiple of bsize
    // bit-field operation not needed; leave operator as previous one
    WN_set_desc(wn, Mtype_AlignmentClass(bsize >> 3, MTYPE_type_class(rtype)));
    WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);

#ifdef TARG_XTENSA
    /*
       Make sure store's value get lowered here.  See GNAT 13772.
       We only lower the value if the size of the store value is
       over 4 bytes and is greater than the node's size.
     */
    if (OPERATOR_is_store(opr)) {
      WN *wn_kid0 = WN_kid0(wn);
      if ( (MTYPE_bit_size (WN_rtype(wn_kid0)) > MTYPE_bit_size(MTYPE_U4)) &&
           (MTYPE_bit_size (WN_rtype(wn_kid0)) > MTYPE_bit_size (WN_desc(wn))) ) {
        WN *wn_trunc = WN_Cvt(WN_rtype(wn_kid0), 
                                Mtype_TransferSign (WN_desc(wn), MTYPE_U4),
                                wn_kid0);
        WN_kid(wn,0) = wn_trunc;
      }
    }
#endif

  }
  else { // generate lowered-to bit-field operator
#if defined(TARG_MIPS) || defined(TARG_IA32)
    if ((indirect || WN_class(wn) != CLASS_PREG) && 
	bofst % 8 == 0 &&		// bit offset on byte boundary
	compute_offset_alignment(bytes_accessed*8, bofst) >= bsize) {
      // adjust bytes accessed to get rid of the left-shift
      WN_load_offset(wn) = WN_load_offset(wn) + (bofst >> 3);
      bytes_accessed = compute_offset_alignment(bytes_accessed, bofst >> 3);
      bofst = 0;
    }
#endif
    WN_set_operator(wn, new_opr);
    WN_set_desc(wn, Mtype_AlignmentClass(bytes_accessed, MTYPE_type_class(rtype)));
    if (OPERATOR_is_load(new_opr) && 
	MTYPE_byte_size(WN_rtype(wn)) < bytes_accessed)
      WN_set_rtype(wn, WN_desc(wn));
    WN_set_bit_offset_size(wn, bofst, bsize);
  }
}

static void lower_trapuv_alloca (WN *block, WN *tree, LOWER_ACTIONS actions)
{
	WN *size = WN_kid0(tree);
	WN *con = WN_UVConst(WN_rtype(size));
	WN *mstore;
	if (WN_operator(size) == OPR_INTCONST && WN_const_val(size) == 0)
		return;	// nothing to do

	mstore = WN_CreateMstore(0,
		Make_Pointer_Type(MTYPE_To_TY(WN_rtype(size)),TRUE),
		con,
		WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		WN_COPY_Tree(size) );
  	mstore = lower_store (block, mstore, actions);
        WN_copy_linenum(tree, mstore);
	WN_INSERT_BlockLast(block, mstore);
}


// are we multiplying two 32 bit values of the same signedness into a 64 bit value
static BOOL Mul_32to64(WN *tree)
{
  if (WN_operator(tree) != OPR_MPY) return false;
  WN *kid0 = WN_kid0(tree);
  WN *kid1 = WN_kid1(tree);
  if (WN_rtype(tree) == MTYPE_I8) {
     BOOL kid0_ok = FALSE;
     BOOL kid1_ok = FALSE;
     if (WN_rtype(kid0) == MTYPE_I4) {
	kid0_ok = TRUE;
     } else if (WN_operator(kid0) == OPR_CVT && WN_desc(kid0) == MTYPE_I4) {    
	kid0_ok = TRUE;
     } else if (WN_opcode(kid0) == OPC_I8I4ILOAD) {    
	kid0_ok = TRUE;
     }
     if (WN_rtype(kid1) == MTYPE_I4) {
	kid1_ok = TRUE;
     } else if (WN_operator(kid1) == OPR_CVT && WN_desc(kid1) == MTYPE_I4) {    
	kid1_ok = TRUE;
     } else if (WN_opcode(kid1) == OPC_I8I4ILOAD) {    
	kid1_ok = TRUE;
     }
     return kid0_ok && kid1_ok;
  } else if (WN_rtype(tree) == MTYPE_U8) {
     BOOL kid0_ok = FALSE;
     BOOL kid1_ok = FALSE;
     if (WN_rtype(kid0) == MTYPE_U4) {
	kid0_ok = TRUE;
     } else if (WN_operator(kid0) == OPR_CVT && WN_desc(kid0) == MTYPE_U4) {    
	kid0_ok = TRUE;
     } else if (WN_opcode(kid0) == OPC_U8U4ILOAD) {    
	kid0_ok = TRUE;
     }
     if (WN_rtype(kid1) == MTYPE_U4) {
	kid1_ok = TRUE;
     } else if (WN_operator(kid1) == OPR_CVT && WN_desc(kid1) == MTYPE_U4) {    
	kid1_ok = TRUE;
     } else if (WN_opcode(kid1) == OPC_U8U4ILOAD) {    
	kid1_ok = TRUE;
     }
     return kid0_ok && kid1_ok;
  }

  return FALSE;
}


inline BOOL Should_Call_Divide(TYPE_ID rtype)
{
#if defined(TARG_IA64)
  if (!OPT_Inline_Divide) {
    if (   rtype == MTYPE_I8 || rtype == MTYPE_U8
	|| rtype == MTYPE_I4 || rtype == MTYPE_U4
	|| rtype == MTYPE_F4 || rtype == MTYPE_F8) return TRUE;
  }
#elif defined(EMULATE_LONGLONG)
  if (rtype == MTYPE_I8 || rtype == MTYPE_U8) return TRUE;
#endif
  return FALSE;
}

static BOOL Is_Fast_Divide(WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  switch (opr) {
  case OPR_DIV:
  case OPR_REM:
  case OPR_MOD:
    {
      if (WN_operator_is(WN_kid1(wn), OPR_INTCONST)) {
	TYPE_ID rtype = OPCODE_rtype(WN_opcode(wn));
	INT64 constval = WN_const_val(WN_kid1(wn));

	return   opr == OPR_DIV
	       ? Can_Do_Fast_Divide(rtype, constval)
	       : Can_Do_Fast_Remainder(rtype, constval);
      }
    }
    break;
  }

  return FALSE;
}

/* ====================================================================
 *
 * WN *lower_expr(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on expression <tree>,
 * returning lowered expression tree.
 *
 * ==================================================================== */

static WN *lower_expr(WN *block, WN *tree, LOWER_ACTIONS actions, WN *cvt_parent)
{
  BOOL kids_lowered = FALSE;	/* becomes TRUE when kids are lowered */
  BOOL intrinsic_lowered;
  WN *cvt_wn = NULL;
  
  TYPE_ID type = WN_rtype(tree);

  Is_True(OPCODE_is_expression(WN_opcode(tree)),
	  ("expected expression node, not %s", OPCODE_name(WN_opcode(tree))));

  /* If 'tree' is an ILOAD or LDID with result type U8/I8 and a 
     I4/U4 desc mtype, we first want to convert that to a CVT ( LOAD ) 
     since WOPT fails sometimes when both I8I4ILOAD and
     I8I4CVT( I4I4ILOAD ) are present (see PR14076)
  */
  if (MTYPE_is_integral(WN_rtype(tree)) && MTYPE_is_integral(WN_desc(tree)) &&
      (WN_operator(tree)==OPR_ILOAD || WN_operator(tree)==OPR_LDID) &&
      (MTYPE_byte_size(WN_rtype(tree)) == MTYPE_byte_size(MTYPE_U8)) &&
      (WN_desc(tree) != MTYPE_BS) &&
      (MTYPE_byte_size(WN_desc(tree)) <= MTYPE_byte_size(MTYPE_U4))) {

      /* Simplifier will get rid of our CVT... */
      BOOL simp = WN_Simplifier_Enable(FALSE);
      TYPE_ID rtype = WN_rtype(tree);
      
      WN_set_rtype(tree, Mtype_TransferSign(rtype, MTYPE_U4));
      tree = WN_Cvt(WN_rtype(tree), rtype, tree);
      
      WN_Simplifier_Enable(simp);
  }

  if (OPCODE_is_load(WN_opcode(tree)))
    lower_map(tree, actions);

 /*
  *  before children are processed reassociate for madd oportunities
  */
  if (Action(LOWER_MADD)	&&
      Madd_Allowed		&&
     (MTYPE_id(type) == MTYPE_F4 || MTYPE_id(type) == MTYPE_F8))
  {
    tree = lower_nary_madd(block, tree, actions);
    tree = lower_madd(block, tree, actions);
  }
  if (Action(LOWER_TREEHEIGHT)	&&
      WN_is_commutative(tree))
  {
    if (MTYPE_is_integral(type) || Roundoff_Level >= ROUNDOFF_ASSOC)
      tree = lower_tree_height(block, tree, actions);
  }

 /* Note: We must split constant offsets after lowering complex exprs
  * and splitting symbol addresses since these may create new offsets
  * that need to be split.
  */
  switch (WN_operator(tree))
  {
  case OPR_INTRINSIC_OP:
    if (INTRN_is_nary(WN_intrinsic(tree)))
      break;

    if (INTRN_cg_intrinsic(WN_intrinsic(tree)))
	    break;

    if (INTRN_is_actual(WN_intrinsic(tree)))
    {
      if (Action(LOWER_INTRINSIC))
      {
	tree = lower_intrinsic(block, tree, actions);
        kids_lowered = TRUE;
      }
      break;
    }
    if (Action(LOWER_INTRINSIC) ||
	Action(LOWER_INLINE_INTRINSIC) ||
	Action(LOWER_INL_STACK_INTRINSIC))
    {
      tree = lower_emulation(block, tree, actions, intrinsic_lowered);
      kids_lowered = TRUE;
    }
    break;

  case OPR_ARRAY:
    if (Action(LOWER_ARRAY))
    {
      tree = lower_linearize_array_addr(block, tree, actions);
      kids_lowered = TRUE;
    }
    break;

  case OPR_ADD:
    if (Action(LOWER_SPLIT_CONST_OFFSETS)
#ifdef TARG_XTENSA
	/* split_sym_addrs will take care of breaking up the LDA, so
           we want to combine the constant if split_sym_addr will be
           executed. */
	&& !Action(LOWER_SPLIT_SYM_ADDRS)
#endif
       )
    {
     /*
      * Split
      *       LDA   (c1) <sym>
      *       CONST (c2)
      *     ADD
      * Into
      *       LDA   (hi) <sym>
      *       CONST (low)
      *     ADD
      */
      WN *lda = WN_kid0(tree);
      WN *con = WN_kid1(tree);

      if (WN_operator_is(con, OPR_INTCONST) &&
	  foldLdaOffset(lda, WN_const_val(con)))
      {
        WN_OFFSET  offset = WN_lda_offset(lda) + WN_const_val(con);

	if (Memory_Offset_Must_Split(MTYPE_V, offset, NULL, tree))
	{
	  WN_lda_offset(lda) = Memory_Offset_Hi(MTYPE_V, offset, NULL, tree);
	  WN_const_val(con) =  Memory_Offset_Lo(MTYPE_V, offset, NULL, tree);
	}
      }
    }
    else
    {
     /*
      * Fold
      *       LDA   (c1) <sym>
      *       CONST (c2)
      *     ADD
      * Into
      *     LDA (c1+c2) <sym>
      */
      WN *lda = WN_kid0(tree);
      WN *con = WN_kid1(tree);

      if (WN_operator_is(con, OPR_INTCONST) &&
	  foldLdaOffset(lda, WN_const_val(con)))
      {
	WN_lda_offset(lda) += WN_const_val(con);
	WN_Delete(tree);
	WN_Delete(con);
	tree = lower_expr(block, lda, actions);;
      }
    }
    break;

  case OPR_MLOAD:
    if (Align_Object)
    {
      WN_kid0(tree)=	lower_expr(block, WN_kid0(tree), actions);
      kids_lowered = 	TRUE;

      tree = improve_Malignment(tree, WN_kid0(tree), WN_kid1(tree),
				WN_load_offset(tree));
      break;
    }  /* fall thru */

  case OPR_ILOAD:
    if (Action(LOWER_MLDID_MSTID) && WN_opcode(tree) == OPC_MMILOAD)
      return lower_miload(block, tree, actions);

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator(tree) == OPR_ILDBITS)
	return lower_load_bits (block, tree, actions);
      else if ((WN_operator(tree) == OPR_ILOAD) &&
               (WN_desc(tree) != MTYPE_BS))
        return lower_expr(block, tree, actions);
    }

    if (Action(LOWER_TIE_FOR_CG) && WN_desc(tree) != WN_rtype(tree) &&
	((MTYPE_is_tie(WN_desc(tree)) || MTYPE_is_tie(WN_rtype(tree))) ||
	 (MTYPE_is_xtbool(WN_desc(tree)) || MTYPE_is_xtbool(WN_rtype(tree))))) {
	WN* cvt;
	cvt = WN_Cvt( WN_desc(tree), WN_rtype(tree), tree);
	WN_set_rtype(tree, Promoted_Mtype[WN_desc(tree)]);
	return lower_expr (block, cvt, actions);
    }

    if (Action(LOWER_SPLIT_CONST_OFFSETS))
    {
     /*
      * Convert
      *      EXPR
      *    ILOAD (offset>16bits)
      * Into
      *        CONST (hi)
      *        EXPR
      *      ADD
      *    ILOAD (lo)
      */
      WN_OFFSET  offset = WN_load_offset(tree);
      if (Memory_Offset_Must_Split(WN_desc(tree), offset, cvt_parent, tree))
      {
	WN_kid0(tree) =  WN_Add(Pointer_type,
				WN_kid0(tree),
				WN_Intconst(Pointer_type,
					    Memory_Offset_Hi(WN_desc(tree), offset,
							     cvt_parent, tree)));
	WN_load_offset(tree) = Memory_Offset_Lo(WN_desc(tree), offset, cvt_parent, tree);
      }
    }
    else
    {
     /*
      * Fold
      *       LDA (c1) <sym>
      *     ILOAD (c2)
      * Into
      *       LDA (0)  <sym>
      *     ILOAD (c1+c2)
      */
      WN *kid = WN_kid0(tree);

#ifdef TARG_XTENSA
      /* We don't want this for xtensa since (c1+c2) could likely be
         out-of-range for the ILOAD, and we have the global
         canonicalization optimization to assign c1 and c2
         appropriately. */
      if (FALSE)
#else
      if (foldLdaOffset(kid, WN_load_offset(tree)))
#endif
      {
	WN_load_offset(tree) += WN_lda_offset(kid);
	WN_lda_offset(kid) = 0;
      }
     /*
      * Fold
      *           EXPR 
      *           CONST (c1)
      *       ADD expr
      *     ILOAD (c1)
      * Into
      *       EXPR
      *     ILOAD (c1+c2)
      */
      else if (WN_operator_is(kid, OPR_ADD) &&
	       foldConstOffset(WN_kid1(kid), WN_load_offset(tree)))
      {
	WN_load_offset(tree) += WN_const_val(WN_kid1(kid));
	WN_kid0(tree) = WN_kid0(kid);
	WN_Delete(WN_kid1(kid));
	WN_Delete(kid);
      }
    }
    break;

  case OPR_LDID:
    if (Action(LOWER_RETURN_VAL) && WN_st(tree) == Return_Val_Preg &&
	WN_opcode(tree) != OPC_MMLDID)
      return lower_return_ldid(block, tree, actions);

    if (Action(LOWER_MLDID_MSTID) && WN_opcode(tree) == OPC_MMLDID)
      return lower_mldid(block, tree, actions);

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator(tree) == OPR_LDBITS)
	return lower_load_bits (block, tree, actions);
      else if ((WN_operator(tree) == OPR_LDID) && 
               (WN_desc(tree) != MTYPE_BS)) 
        return lower_expr(block, tree, actions);
    }

    if (Action(LOWER_TIE_FOR_CG) && WN_desc(tree) != WN_rtype(tree) &&
	((MTYPE_is_tie(WN_desc(tree)) || MTYPE_is_tie(WN_rtype(tree))) ||
	 (MTYPE_is_xtbool(WN_desc(tree)) || MTYPE_is_xtbool(WN_rtype(tree))))) {
	WN* cvt;
	cvt = WN_Cvt( WN_desc(tree), WN_rtype(tree), tree);
	WN_set_rtype(tree, Promoted_Mtype[WN_desc(tree)]);
	return lower_expr (block, cvt, actions);
    }

    if ((WN_class(tree) == CLASS_CONST)	&& (WN_load_offset(tree) == 0))
    {
      TCON	val = WN_val(tree);
      TYPE_ID	valType = WN_val_type(tree);
      WN	*con;

      if (MTYPE_is_integral(type) && MTYPE_is_integral(valType))
      {
	con = WN_Intconst(type, Targ_To_Host( val));
        WN_Delete(tree);
        return con;
      }
     /*
      *  check for real (complex constant) conversion not handled by Targ_Conv
      */
      else if ((MTYPE_is_float(type) && MTYPE_is_float(valType))	&&
	      !(!MTYPE_is_complex(type) && MTYPE_is_complex(valType)))
      {
	if (type != valType)
	{
	  val = Targ_Conv(type, val);
	}
	con =	Make_Const(val);
	WN_Delete(tree);
	return con;
      }
    }
    {
      PREG_NUM last_preg = Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB));
      if ((WN_class(tree) == CLASS_PREG) &&
	  (WN_load_offset(tree) > last_preg))
      {
	  DevWarn("lower_expr() pregno %d > SYMTAB_last_preg(%d)",
		  WN_load_offset(tree), last_preg);
      }
    }
   /*
    * Exposes the LDA for RVI usage
    */
    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
      WN	*iload;
      iload = lower_split_sym_addrs(cvt_parent, tree, WN_load_offset(tree), actions);
      if (iload)
      {
	return lower_expr(block, iload, actions, cvt_parent);
      }
    }
    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN	   *iload;

      iload = lower_formal_ref(tree, WN_load_offset(tree),
			       WN_st(tree), actions);
      if (iload)
      {
        return lower_expr(block, iload, actions, cvt_parent);
      }
    }
    if ( Action(LOWER_UPLEVEL))
    {
      ST *sym = WN_st(tree);

      if (ST_is_uplevelTemp(sym))
      {
        WN	   *iload;

        iload = lower_uplevel_reference(tree, WN_load_offset(tree), actions);
	tree = lower_expr(block, iload, actions, cvt_parent);
	return tree;
      }
    }
    break;

  case OPR_ILDBITS:
  case OPR_LDBITS:
    if (Action(LOWER_BITS_OP))
      return lower_load_bits (block, tree, actions);
    break;
    
  case OPR_LDA:
   /*
    *  use of LDA should mark STFL_ADDR_USED_LOCALLY
    */
    {
      ST *sym = WN_st(tree);

      // if ((ST_class(sym) == CLASS_VAR) ||
      //     (ST_class(sym) == CLASS_FUNC)) {
      //   Do nothing here. ADDR flags should only grow more
      //   optimistic; they should never become more conservative,
      //   because the program's semantics cannot grow worse as we
      //   compile it.
      // }

      if (ST_class(sym) == CLASS_BLOCK && STB_merge(sym))
      {
	DevWarn("LDA (%s) potential bad exposed use of a mergeable symbol",
		ST_name(sym));
      }
    }
    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
      WN  *lda;
      lda =	lower_split_sym_addrs(cvt_parent, tree, WN_lda_offset(tree), actions);
      if (lda)
      {
	return lower_expr(block, lda, actions, cvt_parent);
      }
    }
    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN	   *ldid;

      ldid =	lower_formal_ref(tree, WN_lda_offset(tree), WN_st(tree),
				 actions);
      if (ldid)
      {
	return lower_expr(block, ldid, actions, cvt_parent);
      }
    }
    if ( Action(LOWER_UPLEVEL))
    {
      ST *sym = WN_st(tree);

      if (ST_is_uplevelTemp(sym))
      {
        WN	   *ldid;

        ldid = lower_uplevel_reference(tree, WN_lda_offset(tree), actions);
	tree = lower_expr(block, ldid, actions, cvt_parent);
	return tree;
      }
#ifdef TARG_XTENSA
      if ((ST_class(sym) == CLASS_FUNC) && 
	  PU_is_nested_func(Pu_Table[ST_pu(sym)])) {
	return lower_nested_function_addr(block, tree, actions);
      }
#endif
    }
    break;

  case OPR_CVT:

    // restore the TIE type converting iload
    // this undoes the change 49003 and is a part of a better fix for PR4650
    // when considering PR15290

    // a TIE type converting iload may be split into non-converting iload
    // followed by cvt then the iload cse'd by WOPT into a preg
    if (LOWER_TIE_FOR_CG) {
      WN* cvt = tree;
      TYPE_ID cvt_src_mtype = WN_desc(cvt);
      TYPE_ID cvt_dst_mtype = WN_rtype(cvt);
      WN* kid0 = WN_kid0(cvt);
      if (WN_operator(kid0)==OPR_LDID &&
	  (MTYPE_is_tie(cvt_src_mtype) || MTYPE_is_tie(cvt_dst_mtype)) &&
	  (ST_class(WN_st(kid0)) == CLASS_PREG) &&
	  WN_desc(kid0) == WN_rtype(kid0)) {

	PREG_NUM ldid_preg_num = WN_offset(kid0);

	// do a search backwards on immediately preceeding
	// STID pregs of an ILOAD,
	// if an STID with matching preg is found,
	// search forward again to see if the base is updated (PR16972),
	// if it is not updated, then the ILOAD can be forward substituted
	bool found = false;
	WN* def = NULL;
	PREG_NUM iload_base_preg = (PREG_NUM)-1;
	WN* scan = WN_last(block);
	while (scan) {
	  if (WN_operator(scan)==OPR_STID &&
	      ST_class(WN_st(scan)) == CLASS_PREG) {
	    PREG_NUM stid_preg_num = WN_offset(scan);
	    if (stid_preg_num==ldid_preg_num) {
	      if (WN_operator(WN_kid0(scan))==OPR_ILOAD) {
		def = scan;
		WN* iload_base = WN_kid0(WN_kid0(scan));
		if (WN_operator(iload_base)==OPR_LDID &&
		  ST_class(WN_st(iload_base)) == CLASS_PREG) {
		  iload_base_preg = WN_offset(iload_base);
		  scan = WN_next(scan);
		  bool base_killed = false;
		  while (scan) {
		    if (WN_offset(scan) == iload_base_preg) {
		      base_killed = true;
		      break;
		    }
		    scan = WN_next(scan);
		  }
		  if (base_killed) {
		    found = false;
		    def = NULL;
		    break;
		  } else
		    found = true;
		} else {
		  found = true;
		  break;
		}
	      }
	      break;
	    }
	    scan = WN_prev(scan);
	  } else
	    break;
	}
	if (found) {
	  WN_kid0(tree) = lower_copy_tree(WN_kid0(def), actions);
	}
      }
    }
    cvt_wn = tree;
    // fall-through
    
  case OPR_TRUNC:
  case OPR_RND:
    if (Action(LOWER_CVT))
    {
      tree = lower_cvt(block, tree, actions);
      kids_lowered = TRUE;
    }
    break;

  case OPR_CVTL:
    if (Action(LOWER_TIE_FOR_CG)) {
      WN* cvtl = tree;
      if (WN_operator(WN_kid0(tree))==OPR_CVT) {
        WN* cvt = WN_kid0(cvtl);
        TYPE_ID cvt_src_mtype = WN_desc(cvt);
        TYPE_ID cvt_dst_mtype = WN_rtype(cvt);
	if (MTYPE_is_tie(cvt_src_mtype) || MTYPE_is_tie(cvt_dst_mtype) ||
	    MTYPE_is_xtbool(cvt_src_mtype) || MTYPE_is_xtbool(cvt_dst_mtype)) {
	  TYPE_ID recovered_mtype;
	  if (WN_cvtl_bits(cvtl)==8)
	    recovered_mtype = Mtype_TransferSize(MTYPE_I1,WN_rtype(cvtl));
	  else if (WN_cvtl_bits(cvtl)==16)
	    recovered_mtype = Mtype_TransferSize(MTYPE_I2,WN_rtype(cvtl));
	  else
	    FmtAssert(0, ("Unexpected CVTL size"));

	  BOOL has_conversion =
		  tie_info->mtype_rtor_macro(cvt_src_mtype, recovered_mtype) != NULL ||
		  tie_info->mtype_mtor_macro(cvt_src_mtype, recovered_mtype) != NULL ||
		  tie_info->mtype_rtom_macro(cvt_src_mtype, recovered_mtype) != NULL;

	  if (has_conversion) {
	    WN_set_rtype(cvt, recovered_mtype);
	    WN_Delete(cvtl);
	    WN_kid0(cvt) = lower_expr(block, WN_kid0(cvt), actions);
	    tree=cvt;
            kids_lowered = TRUE;
	  }
	}
      } else if ((WN_opcode(WN_kid0(cvtl))==OPC_I4I4LDID ||
		  WN_opcode(WN_kid0(cvtl))==OPC_U4U4LDID) &&
		  (ST_class(WN_st(WN_kid0(cvtl))) == CLASS_PREG) &&
		 (WN_cvtl_bits(cvtl)==8 || WN_cvtl_bits(cvtl)==16)) {

	WN* ldid_preg = WN_kid0(cvtl);
	TYPE_ID ldid_rtype = WN_rtype(ldid_preg);

	// check if the CVTL is redudant if the definition of the
	// preg LDID is already sign/zero extended

	int search_range = 10;	// range for backward search of definition
	bool found_label = false; // will stop if label is encountered
	WN* stmt = WN_last(block);
	bool done = false;
	bool is_redundant = false;

	while ( !done && stmt && !found_label && search_range>0 ) {
	  if (WN_operator(stmt)==OPR_STID &&
	      ldid_rtype == WN_desc(stmt) &&
	      WN_st(stmt)==WN_st(ldid_preg) &&
	      WN_offset(stmt)==WN_offset(ldid_preg)) {
	    // found a definition
	    WN* kid = WN_kid0(stmt);
	    OPCODE opc=WN_opcode(kid);

	    if (OPCODE_operator(opc)==OPR_INTRINSIC_OP &&
		INTRN_is_tie_intrinsic(WN_intrinsic(kid))) {
	      WN* intr_op = kid;
	      TIE_MACRO_ID tie_macro_id=
			Intrinsic_To_Tie_Macro_Id(WN_intrinsic(intr_op));
	      TIE_MACRO_p tie_macro=tie_info->tie_macro(tie_macro_id);
	      TYPE_ID intr_mtype =
			tie_info->mtype_id(tie_macro->return_type_name());
	      if (MTYPE_bit_size (intr_mtype)== WN_cvtl_bits(cvtl) &&
		  MTYPE_signed(intr_mtype) == MTYPE_signed(ldid_rtype))
		is_redundant = true;
	    } else if (WN_cvtl_bits(cvtl)==8) {
	      if ( (ldid_rtype == MTYPE_I4 && opc == OPC_I4I1LDID) ||
		   (ldid_rtype == MTYPE_U4 && opc == OPC_U4U1LDID)) {
		is_redundant = true;
	      }
	    } else {
	      if ( (ldid_rtype == MTYPE_I4 && opc == OPC_I4I2LDID) ||
		   (ldid_rtype == MTYPE_U4 && opc == OPC_U4U2LDID)) {
		is_redundant = true;
	      }
	    }
	    done = true;
	  } else if (WN_operator(stmt)==OPR_LABEL) {
	    found_label = true;
	  } else {
	    stmt = WN_prev(stmt);
	    search_range--;
	  }
	}

	if (is_redundant) {
	  WN* kid = WN_kid0(cvtl);
	  WN_Delete(cvtl);
	  return lower_expr(block, kid, actions);
	}
      }
    }
    break;

  case OPR_TAS:

    /* If the operand of the TAS is a load from memory, try to
     * replace with a load matching the result type of the TAS.
     * Doing so may avoid a move from one register set to another.
     */
    if (Action(LOWER_TO_CG))
    {
      WN *load = WN_kid0(tree);
      if (   OPERATOR_is_load(WN_operator(load))
	  && (!WN_has_sym(load) || WN_class(load) != CLASS_PREG))
      {
	TYPE_ID tas_rtype = WN_rtype(tree);
	if (MTYPE_byte_size(tas_rtype) == MTYPE_byte_size(WN_desc(load))) {
	  WN_set_rtype(load, tas_rtype);
	  WN_set_desc(load, tas_rtype);
	  WN_Delete(tree);
	  return lower_expr(block, load, actions);
	}
      }
    }
    break;

  case OPR_IMAGPART:
    if (Action(LOWER_COMPLEX))
    {
      WN	*realexp, *imagexp;
      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
      WN_Delete(tree);
      tree = lower_expr(block, imagexp, actions);
    }
    break;

  case OPR_REALPART:
    if (Action(LOWER_COMPLEX))
    {
      WN	*realexp, *imagexp;

      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
      WN_Delete(tree);
      tree = lower_expr(block, realexp, actions);
    }
    break;
    
  case OPR_EQ:
    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      /*
       *  x == y
       *    R(x)==R(y) && I(x)==I(y)
       */
      WN	*rx, *ry, *ix, *iy;
      TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));

      lower_complex_expr(block, WN_kid0(tree), actions, &rx, &ix);
      lower_complex_expr(block, WN_kid1(tree), actions, &ry, &iy);
 
      tree = WN_LAND(WN_EQ(realTY, rx, ry),
		     WN_EQ(realTY, ix, iy));

      return lower_expr(block, tree, actions);
    }
    break;

  case OPR_NE:
    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      /*
       *  x != y
       *    ! ( R(x)==R(y)  &&  I(x)==I(y) )
       */
      WN	*rx, *ry, *ix, *iy;
      TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));

      lower_complex_expr(block, WN_kid0(tree), actions, &rx, &ix);
      lower_complex_expr(block, WN_kid1(tree), actions, &ry, &iy);
 
      tree = WN_LNOT(WN_LAND(WN_EQ(realTY, rx, ry),
			     WN_EQ(realTY, ix, iy)));

      return lower_expr(block, tree, actions);
    }
    break;

  case OPR_MADD:
    if( (Action(LOWER_QUAD|LOWER_FLOAT|LOWER_PAIRED) && disallow_madd(type)))
    {
     /*
      *   kid1 * kid2 + kid0
      */
      WN	*wn;

      wn = WN_Add(type,
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		  WN_kid0(tree));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_MSUB:
    if( (Action(LOWER_QUAD|LOWER_FLOAT|LOWER_PAIRED) && disallow_madd(type)))
    {
     /*
      *   kid1 * kid2 - kid0
      */
      WN	*wn;

      wn = WN_Sub(type,
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		  WN_kid0(tree));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_NMADD:
    if( (Action(LOWER_QUAD|LOWER_FLOAT|LOWER_PAIRED) && disallow_madd(type)))
    {
     /*
      *   - (kid1 * kid2 + kid0)
      */
      WN	*wn, *madd;

      madd = WN_Add(type,
		    WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)),
		    WN_kid0(tree));

      wn = WN_Neg(type, madd);
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_NMSUB:
    if( (Action(LOWER_QUAD|LOWER_FLOAT|LOWER_PAIRED) && disallow_madd(type)))
    {
     /*
      *   - (kid1 * kid2 - kid0)  -->   (kid0 - kid1 * kid2)
      */
      WN	*wn;

      wn = WN_Sub(type,
		  WN_kid0(tree),
		  WN_Mpy(type, WN_kid1(tree), WN_kid2(tree)));
      WN_Delete(tree);

      tree = wn;
    }
    break;

  case OPR_RSQRT:
    tree = lower_rsqrt(block, tree, actions);
    break;

  case OPR_RECIP:
    tree = lower_recip(block, tree, actions);
    break;

  case OPR_SELECT:
    {
      WN * const kid0 = WN_kid0(tree);	// the condition expression
      if (WN_operator_is(kid0, OPR_INTCONST))
      {
	INT64 flag = WN_const_val(kid0);
	WN * const kid = flag ? WN_kid1(tree) : WN_kid2(tree);
	return lower_expr(block, kid, actions);
      } else if (WN_operator(kid0) == OPR_LNOT) {
	// swap the select inputs and replace the condition with
	// operand of the LNOT. Then lower the whole tree again
	// as there may be further lowerings that may occur with the
	// new operands.
	WN * const new_kid1 = WN_kid2(tree);
	WN * const new_kid2 = WN_kid1(tree);
	WN * const new_kid0 = WN_kid0(kid0);
	TYPE_ID new_desc = (WN_rtype(new_kid0) == MTYPE_B) ? MTYPE_B : MTYPE_V;
	WN_kid0(tree) = new_kid0;
	WN_kid1(tree) = new_kid1;
	WN_kid2(tree) = new_kid2;
	WN_set_desc(tree, new_desc);
	WN_Delete(kid0);
	return lower_expr(block, tree, actions);
      }
    }
    break;

  case OPR_PAREN:
    if (Roundoff_Level > ROUNDOFF_ASSOC)
    {
     /*
      *  At suitable roundoff we may remove these parens
      *  This will allow better MADD generation latter (pv 316380)
      */
      WN *kid0 = WN_kid0(tree);

      WN_Delete(tree);
      return lower_expr(block, kid0, actions);
    }
    break;

  case OPR_DIV:
  case OPR_REM:
  case OPR_MOD:

    if ( Action(LOWER_MPY_DIV_CONST) ) {
      if ( WN_operator(tree) == OPR_DIV )
	tree = lower_div_by_const( block, tree, actions );
      else if ( WN_operator(tree) == OPR_REM )
	tree = lower_rem_by_const( block, tree, actions );

      if ((WN_operator(tree) != OPR_DIV) &&
	  (WN_operator(tree) != OPR_REM) &&
	  (WN_operator(tree) != OPR_MOD))
	break;
    }

    // If having instructions such as quo/rem, etc., no emulation
    if (xt_div32 && 
        (type == MTYPE_I1 || type == MTYPE_I2 || type == MTYPE_I4 ||
         type == MTYPE_U1 || type == MTYPE_U2 || type == MTYPE_U4
        )) {
      break;
    }
    
    if( Action(LOWER_INTRINSIC) )
      switch( type )
	{
	case MTYPE_I1:
	case MTYPE_I2:
	case MTYPE_I4:
	case MTYPE_I8:
	case MTYPE_U1:
	case MTYPE_U2:
	case MTYPE_U4:
	case MTYPE_U8:
	  tree = lower_emulation( block, tree, actions, intrinsic_lowered);
	  kids_lowered = TRUE;
	  break;
	}
    else {

      /* If not inlining divides, then generate an INTRINSIC_OP that is
       * later lowered to a call
       */
      TYPE_ID rtype = OPCODE_rtype(WN_opcode(tree));
      if (Should_Call_Divide(rtype) && !Is_Fast_Divide(tree)) {

#ifdef EMULATE_LONGLONG
        if (rtype == MTYPE_I8 || rtype == MTYPE_U8) {
	  FmtAssert (OPCODE_rtype(WN_opcode(WN_kid0(tree))) == rtype,
		     ("DIV/REM/MOD: kid0 should be %d, is %d",
		     rtype, OPCODE_rtype(WN_opcode(WN_kid0(tree)))));
	  FmtAssert (OPCODE_rtype(WN_opcode(WN_kid1(tree))) == rtype,
		     ("DIV/REM/MOD: kid1 should be %d, is %d",
		     rtype, OPCODE_rtype(WN_opcode(WN_kid1(tree)))));
	}
#endif

	WN *kids[2];
	WN *iwn;
	LEAF tmpY;
	INTRINSIC intrinsic;
	BOOL is_unsigned = MTYPE_is_unsigned(rtype);
	BOOL is_float = MTYPE_is_float(rtype);
	BOOL is_double = MTYPE_is_size_double(rtype);
	switch (WN_operator(tree)) {
	case OPR_DIV:
	  if (is_float) {
	    intrinsic = (is_double ? INTRN_DIVDF3 : INTRN_DIVSF3);
	  } else if (is_double) {
	    intrinsic = (is_unsigned ? INTRN_UDIVDI3 : INTRN_DIVDI3);
	  } else {
	    intrinsic = (is_unsigned ? INTRN_UDIVSI3 : INTRN_DIVSI3);
	  }
	  break;
	case OPR_MOD:
	  FmtAssert(!is_float, ("Unexpected MOD operator on float"));
	  // Unsigned MOD is the same as REM.
	  // Signed MOD is a REM followed by an adjustment which
	  // uses the divisor, so save it to a temp and replace the
	  // divisor operand of the tree with a load from the temp.
	  if (!is_unsigned) {
	    tmpY = Make_Leaf(block, WN_kid1(tree), type);
	    WN_kid1(tree) = Load_Leaf(tmpY);
	  }
	  /*FALLTHROUGH*/
	case OPR_REM:
	  FmtAssert(!is_float, ("Unexpected REM operator on float"));
	  if (is_double) {
	    intrinsic = (is_unsigned ? INTRN_UMODDI3 : INTRN_MODDI3);
	  } else {
	    intrinsic = (is_unsigned ? INTRN_UMODSI3 : INTRN_MODSI3);
	  }
	  break;
	default:
	  #pragma mips_frequency_hint NEVER
	  FmtAssert (FALSE, ("Unexpected division operator"));
	  /*NOTREACHED*/
	}
	kids[0] = WN_CreateParm (rtype,
				 lower_expr(block, WN_kid0(tree), actions),
				 Be_Type_Tbl(rtype),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
	kids[1] = WN_CreateParm (rtype, 
				 lower_expr(block, WN_kid1(tree), actions),
				 Be_Type_Tbl(rtype),
				 WN_PARM_BY_VALUE | WN_PARM_READ_ONLY);
	iwn = WN_Create_Intrinsic(OPCODE_make_op(OPR_INTRINSIC_OP,
						 rtype, MTYPE_V),
				  intrinsic, 2, kids);

	if (WN_operator(tree) == OPR_MOD && !is_unsigned) {
	  // For signed MOD, we need to add the divisor to the result
	  // of the REM if both of the operands are negative.
	  WN *t2, *t3, *t4;
	  PREG_NUM t1;
	  t1 = AssignExpr(block, iwn, type);
	  t2 = WN_Bxor(type, WN_LdidPreg(type, t1), Load_Leaf(tmpY));
	  t3 = WN_Ashr(type, t2, WN_Intconst(type, MTYPE_size_reg(type) - 1));
	  t4 = WN_Band(type, Load_Leaf(tmpY), t3);
	  iwn = WN_Add(type, WN_LdidPreg(type, t1), t4);
	  iwn = lower_expr(block, iwn, actions);
	}

	WN_Delete(tree);
	return iwn;
      }
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_rtype(tree))) {
	// complex div creates if-then-else structure,
	// so want to expand this early, then just return C*LDID preg here
	// (e.g. in case is under a PARM node, and is not first parm).
	// Note that fortran fe moves this out from under call, 
	// but C doesn't.  Apparently only get here for C PARM case.
	TYPE_ID	cdiv_mtype = WN_rtype(tree);
	// using a preg causes wopt to get confused and not
	// connect the F* preg numbers with the C* preg number.
	// so use temp symbol instead.
	TY_IDX cdiv_ty = MTYPE_To_TY(cdiv_mtype);
	ST *cdiv_st = Gen_Temp_Symbol (cdiv_ty, ".complex_div");
	WN *stid = WN_Stid(cdiv_mtype, 0, cdiv_st, cdiv_ty, tree);
      	WN_Set_Linenum (stid, current_srcpos);
	stid = lower_store(block, stid, actions);
	WN_copy_linenum(tree, stid);
	WN_INSERT_BlockLast(block, stid);
	WN *ldid = WN_Ldid(cdiv_mtype, 0, cdiv_st, cdiv_ty);
	return ldid;
    }
    break;

  case OPR_COMMA:
    {
      WN *commaBlock;
      commaBlock = lower_block(WN_kid0(tree), actions);

      DevWarn("lower_expr(): comma operator seen, line %d",
	      Srcpos_To_Line(current_srcpos));

      WN_copy_linenum(tree, commaBlock);
      WN_INSERT_BlockLast(block, commaBlock);
    }
    return lower_expr(block, WN_kid1(tree), actions);

  case OPR_CSELECT:
   /*
    *  
    */
    if (Action(LOWER_SHORTCIRCUIT))
    {
      /*******************************************************************

      DevWarn("lower_expr(): cselect operator seen, line %d",
              Srcpos_To_Line(current_srcpos));
      *******************************************************************/

      if (expr_is_speculative(tree))
      {
	WN *select = WN_Select(type, WN_kid0(tree), WN_kid1(tree),
			       WN_kid2(tree));
  
	WN_Delete(tree);
	return select;
      }
      else
	return lower_select( block, tree, actions );
    }
    break;

  case OPR_CAND:
  case OPR_CIOR:
   /*
    *  return boolean 0/1 (build CSELECT)
    */
    if (Action(LOWER_SHORTCIRCUIT))
    {
      if (expr_is_speculative(tree))
      {
	WN *cond;

	if (WN_operator_is(tree, OPR_CAND))
	  cond = WN_LAND( WN_kid0(tree), WN_kid1(tree));
	else
	  cond = WN_LIOR( WN_kid0(tree), WN_kid1(tree));

	WN_Delete(tree);
	return lower_expr(block, cond, actions);
      }
      else
      {
	WN *select = WN_Cselect(type,
				tree,	
				WN_Intconst(Boolean_type, 1),
				WN_Intconst(Boolean_type, 0));

        if (Cur_PU_Feedback) {
	  Cur_PU_Feedback->FB_lower_circuit_to_cselect(tree, select);
	}

	return lower_expr(block, select, actions);
      }
    }
    break;

#ifdef TARG_XTENSA    
    /* lower BNOT(x) to BXOR(-1, x) to expose the -1 constant */
  case OPR_BNOT:
    if ( Action(LOWER_MPY_DIV_CONST) ) {
      /* use the LOWER_MPY_DIV_CONST to avoid using another bit */

      if (type == MTYPE_I4 || type == MTYPE_U4 ||
	  type == MTYPE_I8 || type == MTYPE_U8) {
      
        WN *kid = lower_expr(block, WN_kid0(tree), actions);
        WN *cst = WN_Intconst(type, -1);
        tree    = WN_Bxor(type, kid, cst);
      }
      break;
    }
#endif

  case OPR_MPY:
    if ( Action(LOWER_MPY_DIV_CONST) ) {
      tree = lower_mpy_by_const( block, tree, actions );
      if (WN_operator(tree) != OPR_MPY)
	break;
    }
    
    if ( Action( LOWER_INTRINSIC ) ) {
      /* see if we have an appropriate built-in instruction */
      if ( ( (xt_mac16 || xt_mul16 || Enable_HiFi2_Ops) && WN_Mpy_16Bit(tree) ) ||
	   ( xt_mul32  && type != MTYPE_I8 && type != MTYPE_U8 ) ) {
	  /* do nothing and let the instruction selector handle it */
	} 
      else if (xt_mul32 && xt_mul32h && Mul_32to64(tree)) {
	tree = lower_int_int_ll(block,tree, actions);
      } else if ( (Opt_Level > 1) && xt_mul32 && xt_mul32h &&
                  ((type == MTYPE_U8) || (type == MTYPE_I8)) ) {
        tree = lower_inline_mpy64(block, tree, actions);
      } else {

	  /* we have to use an emulated call */
	  switch( type )
	    {
	    case MTYPE_I1:
	    case MTYPE_I2:
	    case MTYPE_I4:
	    case MTYPE_I8:
	    case MTYPE_U1:
	    case MTYPE_U2:
	    case MTYPE_U4:
	    case MTYPE_U8:
	      tree = lower_emulation( block, tree, actions, intrinsic_lowered);
	      kids_lowered = TRUE;
	    }
	}
    }
    break;
#ifdef TARG_XTENSA    
    /* Lower LAND and LIOR into explicit short circuit version. */
  case OPR_LAND:
  case OPR_LIOR:
    if ( Action(LOWER_SHORTCIRCUIT) ) {
      tree = lower_land_lior( block, tree, actions );
    }
    break;
#endif
  }
  
  if (Action(LOWER_QUAD|LOWER_FLOAT|LOWER_PAIRED))
  {
    TYPE_ID rtype = WN_rtype(tree);
    TYPE_ID desc  = WN_desc(tree);

    if (MTYPE_is_float(desc) || (desc == MTYPE_I8) || (desc == MTYPE_U8))
    {
      tree = lower_emulation(block, tree, actions,intrinsic_lowered);
      kids_lowered = TRUE;
#if 0
      switch (WN_operator(tree))
      {
      case OPR_CONST:
      case OPR_LDID:
      case OPR_ILOAD:
	break;
      case OPR_EQ:
      case OPR_NE:
      case OPR_LE:
      case OPR_LT:
      case OPR_GT:
      case OPR_GE:
      case OPR_CVT:
      case OPR_TRUNC:
      case OPR_RND:
      case OPR_CEIL:
      case OPR_FLOOR:
	tree = lower_emulation(block, tree, actions,intrinsic_lowered);
	break;
      default:
	break;
      }
#endif
    } else if
	(MTYPE_is_float(rtype) || (rtype == MTYPE_I8) || (rtype == MTYPE_U8))
    {
      tree = lower_emulation(block, tree, actions, intrinsic_lowered);
      kids_lowered = TRUE;
#if 0
      switch (WN_operator(tree))
      {
      case OPR_CONST:
      case OPR_LDID:
      case OPR_ILOAD:
	break;

	// case OPR_SELECT:
      case OPR_NEG:
      case OPR_ABS:
      case OPR_SQRT:
      case OPR_ADD:
      case OPR_SUB:
      case OPR_MPY:
      case OPR_DIV:
      case OPR_MOD:
      case OPR_REM:
      case OPR_MAX:
      case OPR_MIN:
      case OPR_CVT:
      case OPR_TRUNC:
      case OPR_RND:
	tree = lower_emulation(block, tree, actions, intrinsic_lowered);
	break;
      default:
	break;
      }
#endif
    }
  }

  if (WN_nary_intrinsic(tree))
  {
    tree = WN_NaryToExpr(tree);
  }

  /* Lower kids if not done already. */
  if (! kids_lowered)
  {
     INT16 i;
     for (i = 0; i < WN_kid_count(tree); i++)
       WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions, cvt_wn);
     tree = WN_Simplify_Rebuild_Expr_Tree(tree,alias_manager);
  }

  return tree;
}


/* ====================================================================
 *
 *  static TY_IDX coerceTY(TY_IDX type, TYPE_ID btype)
 *
 * return TY corresponding to the btype
 * (type might be pointer -> btype)
 *
 * ==================================================================== */

static TY_IDX coerceTY(TY_IDX type, TYPE_ID btype)
{
  TY &ty = Ty_Table[type];

  BOOL is_volatile = TY_is_volatile(type);
  BOOL is_const = TY_is_const(type);
  BOOL is_restrict = TY_is_restrict(type);

  TY_IDX ret_type;

  if (TY_is_pointer(ty))
    ret_type = Make_Pointer_Type(coerceTY(TY_pointed(ty), btype));
  else
    ret_type = MTYPE_To_TY(btype);

  if (is_volatile) Set_TY_is_volatile(ret_type);
  if (is_const) Set_TY_is_const(ret_type);
  if (is_restrict) Set_TY_is_restrict(ret_type);

  return ret_type;

}




/* ====================================================================
 *
 *  static ST * coerceST(const ST *st, TYPE_ID type)
 *
 * return ST corresponding to the type
 *
 * ==================================================================== */

static ST *coerceST(const ST *st, TYPE_ID type)
{
  if (ST_class(st) == CLASS_PREG)
  {
   /*
    *  for now, only pregs must correspond to the type
    */
    return MTYPE_To_PREG(type);
  }

  return (ST *) st;
}

static ST *coerceST(const ST &st, TYPE_ID type)
{
  if (ST_class(&st) == CLASS_PREG)
  {
   /*
    *  for now, only pregs must correspond to the type
    */
    return MTYPE_To_PREG(type);
  }

  return (ST *) &st;
}


/* ====================================================================
 *
 * static BOOL WN_StoreIsUnused(WN *tree)
 *
 * Find if store has been marked by IPA as unused
 * This may require traversal of the address expression to find
 * an LDA or ARRAY
 * ==================================================================== */
static BOOL WN_StoreIsUnused(WN *tree)
{
  ST  *sym;

  switch(WN_operator(tree))
  {
  case OPR_LDA:
  case OPR_STID:
    sym = WN_st(tree);

    if (ST_class(sym) != CLASS_PREG  &&
        ST_class(sym) != CLASS_BLOCK &&
	ST_is_not_used(sym))
      return TRUE;
    break;

  case OPR_ARRAY:
    return WN_StoreIsUnused(WN_array_base(tree));
  }

  return FALSE;
}

/* ====================================================================
 *
 * WN *add_fake_parm(WN *o_call, WN *fake_actual, LOWER_ACTIONS actions)
 *
 * Add the fake actual parameter as the first parameter to the original call.
 * All original parameters are shifted down by 1.
 *
 * ==================================================================== */
static WN *add_fake_parm(WN *o_call, WN *fake_actual, TY_IDX ty_idx, 
			 LOWER_ACTIONS actions)
{
  WN *n_call;
  if (WN_operator(o_call) == OPR_ICALL)
    n_call = WN_Icall(MTYPE_V, MTYPE_V, WN_kid_count(o_call)+1, WN_ty(o_call));
  else
    n_call = WN_generic_call(WN_operator(o_call), MTYPE_V, MTYPE_V, 
			     WN_kid_count(o_call)+1, WN_st_idx(o_call));
  WN_call_flag(n_call) = WN_call_flag(o_call);
  WN_Set_Linenum(n_call, WN_Get_Linenum(o_call));
  if ( Cur_PU_Feedback ) {
    Cur_PU_Feedback->FB_lower_call( o_call, n_call );
  }
  WN_kid0(n_call) = WN_CreateParm(Pointer_Mtype, fake_actual, ty_idx,
			      WN_PARM_BY_REFERENCE | WN_PARM_PASSED_NOT_SAVED);
  for (INT32 i = 0; i < WN_kid_count(o_call); i++)
    WN_kid(n_call, i+1) = lower_copy_tree(WN_kid(o_call, i), actions);
  return n_call;
}

/* ====================================================================
 *
 * WN *lower_return_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * "tree" must be an MSTID whose kid is MLDID of Return_Val_Preg (-1).
 * Perform lowering of MSTID whose rhs is Return_Val_Preg; translate to
 * either MSTORE or a sequence of STIDs.
 *
 * ==================================================================== */
static WN *lower_return_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID mtype;
  ST *preg_st;
  WN *n_rhs;
  WN *wn = NULL;	// init to prevent upward-exposed use
  RETURN_INFO return_info = Get_Return_Info(WN_ty(tree), Complex_Not_Simulated,
					    Return_Info_Incoming);
  if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
    // get the previous MCALL statement
    WN *call = WN_last(block);
    Is_True(WN_operator(call) == OPR_CALL || WN_operator(call) == OPR_ICALL ||
	    WN_operator(call) == OPR_PICCALL,
	    ("statement preceding MMLDID of Return_Val_Preg must be a call"));
    Is_True(WN_rtype(call) == MTYPE_M,
	    ("call preceding MMLDID of Return_Val_Preg not type M"));
    WN *awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 
			   WN_store_offset(tree), 
			   Make_Pointer_Type(WN_ty(tree)), WN_st_idx(tree));
    awn = lower_expr(block, awn, actions);
    WN *n_call = add_fake_parm(call, awn, WN_ty(awn), actions);
    WN_DELETE_FromBlock(block, call);
    WN_copy_linenum(tree, n_call);
    WN_INSERT_BlockLast(block, n_call); 

    WN_DELETE_Tree(tree);
    return NULL; // original MSTID disappears
  }
  else { // return via 1 or more return registers
    for (INT32 i = 0; i < RETURN_INFO_count(return_info); i++) {
      if (i != 0)
        WN_INSERT_BlockLast (block, wn); // insert the last STID created 
      mtype = RETURN_INFO_mtype(return_info, i);
      preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
      n_rhs = WN_CreateLdid(OPR_LDID, mtype, mtype, 
			    RETURN_INFO_preg(return_info, i), preg_st,
			    Be_Type_Tbl(mtype));
      n_rhs = adjust_incoming_return_location(n_rhs, WN_ty(tree));
      wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, 
		         WN_store_offset(tree)+i*MTYPE_byte_size(mtype),
			 WN_st_idx(tree), Be_Type_Tbl(mtype), n_rhs);
      WN_copy_linenum(tree, wn);
      wn  = lower_store (block, wn, actions);
      WN_Set_Linenum (wn, WN_Get_Linenum(tree));
    }
    WN_DELETE_Tree(tree);
    return wn;
  }
}

/* ====================================================================
 *
 * WN *lower_return_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * "tree" must be an MISTORE whose rhs is MLDID of Return_Val_Preg (-1).
 * Perform lowering of MISTORE whose rhs is Return_Val_Preg; translate to
 * either MSTORE or a sequence of ISTOREs.
 *
 * ==================================================================== */
static WN *lower_return_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TYPE_ID mtype;
  ST *preg_st;
  WN *n_rhs;
  WN *wn = NULL;	// init to prevent upward-exposed use
  RETURN_INFO return_info = Get_Return_Info(WN_ty(tree), Complex_Not_Simulated,
					    Return_Info_Incoming);
  if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
    // get the previous MCALL statement
    WN *call = WN_last(block);
    Is_True(WN_operator(call) == OPR_CALL || WN_operator(call) == OPR_ICALL ||
	    WN_operator(call) == OPR_PICCALL,
	    ("statement preceding MMLDID of Return_Val_Preg must be a call"));
    Is_True(WN_rtype(call) == MTYPE_M,
	    ("call preceding MMLDID of Return_Val_Preg not type M"));
    WN *awn = WN_COPY_Tree(WN_kid1(tree));
    if (WN_store_offset(tree) != 0) { // generate an ADD node for the offset
      WN *iwn = WN_CreateIntconst(OPR_INTCONST, Pointer_Mtype, MTYPE_V, 
				  WN_store_offset(tree));
      awn = WN_CreateExp2(OPR_ADD, Pointer_Mtype, Pointer_Mtype, awn, iwn);
    }
    awn = lower_expr(block, awn, actions);
    WN *n_call = add_fake_parm(call, awn, WN_ty(tree), actions);
    WN_DELETE_FromBlock(block, call);
    WN_copy_linenum(tree, n_call);
    WN_INSERT_BlockLast(block, n_call); 

    WN_DELETE_Tree(tree);
    return NULL; // original MSTID disappears
  }
  else { // return via 1 or more return registers
    WN *base_expr;
    for (INT32 i = 0; i < RETURN_INFO_count(return_info); i++) {
      if (i != 0)
        WN_INSERT_BlockLast (block, wn); // insert the last STID created 
      mtype = RETURN_INFO_mtype(return_info, i);
      preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
      n_rhs = WN_CreateLdid(OPR_LDID, mtype, mtype, 
			    RETURN_INFO_preg(return_info, i), preg_st,
			    Be_Type_Tbl(mtype));
      base_expr = WN_COPY_Tree(WN_kid1(tree));
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V, mtype, 
		           WN_store_offset(tree)+i*MTYPE_byte_size(mtype),
			   Be_Type_Tbl(mtype), n_rhs, base_expr);
      WN_copy_linenum(tree, wn);
      wn  = lower_store (block, wn, actions);
      WN_Set_Linenum (wn, WN_Get_Linenum(tree));
    }
    WN_DELETE_Tree(tree);
    return wn;
  }
}

/* ====================================================================
 *
 * WN *lower_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MSTID nodes returning
 * an equivalent MSTORE node.
 *
 * ==================================================================== */

static WN *lower_mstid(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX ty_idx  = WN_ty(tree);
  TY_IDX pty_idx;
  UINT64 size    = WN_field_id(tree) == 0 ?
      TY_size(ty_idx):
      TY_size(get_field_type (ty_idx, WN_field_id(tree)));
  WN*    wn;
  WN*    awn;
  WN*    swn;

  Is_True((WN_operator(tree) == OPR_STID && WN_desc(tree) == MTYPE_M),
	  ("expected mstid node, not %s", OPCODE_name(WN_opcode(tree))));

  pty_idx = Make_Pointer_Type (ty_idx, FALSE);

  WN *rhs = WN_kid0(tree);
  if (WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg) {
    // handle lowering of MLDID of Return_Val_Preg followed by MSTID
//    Is_True(Action(LOWER_RETURN_VAL),
      //    ("LOWER_RETURN_VAL action must be specified"));
    return lower_return_mstid(block, tree, actions);
  }

  awn = WN_CreateLda (OPR_LDA, Pointer_Mtype, MTYPE_V, WN_store_offset(tree),
		      pty_idx, WN_st(tree));
  swn = WN_CreateIntconst (OPC_U4INTCONST, size);
  wn  = WN_CreateMstore (0, pty_idx, WN_kid0(tree), awn, swn);
  WN_copy_linenum(tree, wn);
  WN_set_field_id(wn, WN_field_id(tree));
  wn  = lower_store (block, wn, actions);

  WN_Delete(tree);
  return wn;
}


/* ====================================================================
 *
 * WN *lower_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on MISTORE nodes returning
 * an equivalent MSTORE node.
 *
 * ==================================================================== */

static WN *lower_mistore(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  TY_IDX pty_idx  = WN_ty(tree);
  TY_IDX ty_idx  = TY_pointed(pty_idx);

  if (WN_field_id (tree) != 0)
    ty_idx = get_field_type (ty_idx, WN_field_id (tree));
  
  UINT64 size    = TY_size(Ty_Table[ty_idx]);
  WN*    wn;
  WN*    swn;

  Is_True((WN_operator(tree) == OPR_ISTORE && WN_desc(tree) == MTYPE_M),
	  ("expected mistore node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(TY_kind(pty_idx) == KIND_POINTER,
	  ("type specified in MISTORE not pointer"));
//Is_True(size > 0, ("type in MISTORE cannot be zero size"));
  if (size == 0)
    DevWarn ("type in MISTORE cannot be zero size");
  WN *rhs = WN_kid0(tree);
  if (WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg) {
    // handle lowering of MLDID of Return_Val_Preg followed by MISTORE
    Is_True(Action(LOWER_RETURN_VAL),
	    ("LOWER_RETURN_VAL action must be specified"));
    return lower_return_mistore(block, tree, actions);
  }

  swn = WN_CreateIntconst(OPC_U4INTCONST, size);
  wn  = WN_CreateMstore(WN_offset(tree), pty_idx, 
			lower_copy_tree(WN_kid0(tree), actions),
                        lower_copy_tree(WN_kid1(tree), actions), swn);
  
  WN_copy_linenum(tree, wn);
  WN_set_field_id(wn, WN_field_id(tree));

  lower_copy_maps(tree, wn, actions);
  wn  = lower_store(block, wn, actions);

  WN_DELETE_Tree (tree);
  return wn;
}

/* Utility Functions for lower_store */
static void split_istore( WN *block, WN *tree, TYPE_ID ty,
			  WN *x_val, WN *y_val,
			  LEAF *x_leaf, LEAF *y_leaf,
			  LOWER_ACTIONS actions, WN *&x, WN *&y)
{
  WN_OFFSET	offset = WN_store_offset(tree);
  if (Action(LOWER_BASE_INDEX))
    {
      WN		*base, *index, *addr;
      LEAF	indexN;

      base = index=	NULL;
      lower_to_base_index(WN_kid1(tree), &base, &index) ;

      base = lower_expr(block, base, actions);
      index = lower_expr(block, index, actions);

      indexN = Make_Leaf(block, index, Pointer_type);

      addr = WN_Add(Pointer_type,
		    Load_Leaf(indexN),
		    lower_copy_tree(base, actions));

      x = WN_Istore(ty,
		    coerceOFFSET(tree, ty, offset),
		    coerceTY(WN_ty(tree), ty),
		    addr,
		    (x_val) ? x_val : Load_Leaf(*x_leaf));

      addr = WN_Add(Pointer_type,
		    Load_Leaf(indexN),
		    base);

      y = WN_Istore(ty,
		    offset,
		    coerceTY(WN_ty(tree), ty),
		    addr,
		    (y_val) ? y_val : Load_Leaf(*y_leaf));
    }
  else
    {
      WN	*addr;
      LEAF	addrN;

      addr = lower_expr(block, WN_kid1(tree), actions);
      addrN = Make_Leaf(block, addr, Pointer_type);

      x = WN_Istore(ty,
		    coerceOFFSET(tree, ty, offset),
		    coerceTY(WN_ty(tree), ty),
		    Load_Leaf(addrN),
		    (x_val) ? x_val : Load_Leaf(*x_leaf));

      y = WN_Istore(ty,
		    offset,
		    coerceTY(WN_ty(tree), ty),
		    Load_Leaf(addrN),
		    (y_val) ? y_val : Load_Leaf(*y_leaf));
    }

  x = lower_store(block, x, actions);
  WN_Set_Linenum (x, WN_Get_Linenum(tree));
  WN_INSERT_BlockLast(block, x);

  y = lower_store(block, y, actions);
}

static void split_stid( WN *block, WN *tree, TYPE_ID ty,
			WN *x_val, WN *y_val, LOWER_ACTIONS actions, WN *&x, WN *&y)
{
  WN *wn;
  WN_OFFSET	offset = WN_store_offset(tree);
  WN *home = NULL;

  if (WN_operator(tree)==OPR_STID && ST_class(WN_st(tree)) == CLASS_PREG) {
    home = Preg_Home(offset);

    // reset home because we are splitting the preg
    Set_Preg_Home(WN_offset(tree), NULL);
  }

  wn = WN_Stid(ty,
	       coerceOFFSET(tree, ty, offset),
	       coerceST(WN_st(tree), ty),
	       coerceTY(WN_ty(tree), ty),
	       x_val);

  x = lower_store(block, wn, actions);
  WN_Set_Linenum (x, WN_Get_Linenum(tree));
  WN_INSERT_BlockLast(block, x);

  if (home && WN_operator(x)==OPR_STID &&
      WN_operator(home)==WN_operator(WN_kid0(x)) &&
      ST_class(WN_st(x)) == CLASS_PREG)
    Set_Preg_Home(WN_offset(x), WN_kid0(x));

  wn = WN_Stid(ty,
	       offset, 
	       coerceST(WN_st(tree), ty),
	       coerceTY(WN_ty(tree), ty),
	       y_val);

  y = lower_store(block, wn, actions);

  if (home && WN_operator(y)==OPR_STID &&
      WN_operator(home)==WN_operator(WN_kid0(y)) &&
      ST_class(WN_st(y)) == CLASS_PREG)
    Set_Preg_Home(WN_offset(y), WN_kid0(y));

}

/* ====================================================================
 *
 * WN *lower_store(block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on store statement <tree>,
 * returning lowered tree.
 *
 * ==================================================================== */

static WN *lower_store(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  BOOL kids_lowered = FALSE;	/* becomes TRUE when kids are lowered */

  Is_True(OPCODE_is_store(WN_opcode(tree)),
	  ("expected store node, not %s", OPCODE_name(WN_opcode(tree))));

  /* If the store is to memory and the expression begins with a TAS, try to
   * replace with a store matching the result type of the TAS's expression.
   * Doing so may avoid a move from one register set to another.
   */
  if (Action(LOWER_TO_CG))
  {
    WN *kid0 = WN_kid0(tree);
    if (   WN_operator(kid0) == OPR_TAS
	&& (!WN_has_sym(tree) || WN_class(tree) != CLASS_PREG))
    {
      WN *tas = kid0;
      WN *tas_kid0 = WN_kid0(tas);
      TYPE_ID tas_kid0_rtype = WN_rtype(tas_kid0);
      if (MTYPE_byte_size(WN_rtype(tas)) == MTYPE_byte_size(tas_kid0_rtype)) {
	WN_set_desc(tree, tas_kid0_rtype);
	WN_kid0(tree) = tas_kid0;
	WN_Delete(tas);
      }
    } else if (Action(LOWER_TIE_FOR_CG) &&
	       WN_operator(kid0) == OPR_CVT &&
	       (WN_operator(tree)==OPR_ISTORE ||
		WN_operator(tree)==OPR_STID) &&
	       MTYPE_is_integral(WN_rtype(kid0)) &&
	       MTYPE_is_tie(WN_desc(kid0)) &&
	       MTYPE_bit_size(WN_desc(tree))!=32) {

	/* check if any CVTL has been optimized away with the
	   store truncation and restore them so proper
	   TIE to int conversion is recognized
	*/

	TYPE_ID rtype = WN_desc(tree);

	int cvtl_bits = MTYPE_bit_size(rtype);
	OPCODE opc = OPCODE_make_op(OPR_CVTL, WN_rtype(kid0), MTYPE_V);
	kid0 = WN_CreateCvtl(opc, cvtl_bits, kid0);
	WN_kid0(tree) = kid0;
    }
  }

  /*
   * create any maps that need to be present
   */
  lower_map(tree, actions);


  /* Note: We must split constant offsets after lowering complex stores
   * and splitting symbol addresses since these may create new offsets
   * that need to be split.
   */
  
  switch (WN_operator(tree))
  {
  case OPR_ISTORE:
    if (WN_StoreIsUnused(WN_kid1(tree)))
    {
      WN	*eval;

      eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

      WN_Delete(tree);

      return eval;
    }

    if (Action(LOWER_MLDID_MSTID) && WN_desc(tree) == MTYPE_M)
      return lower_mistore(block, tree, actions);

    if (Action(LOWER_RETURN_VAL) && WN_desc(tree) == MTYPE_M &&
	WN_opcode(WN_kid0(tree)) == OPC_MMLDID && 
	WN_st(WN_kid0(tree)) == Return_Val_Preg) {
	return lower_return_mistore(block, tree, actions);
    }

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator (tree) == OPR_ISTBITS)
	return lower_store_bits (block, tree, actions);
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      WN	*realstore, *imagstore;
      LEAF	realexpN, imagexpN;
      TYPE_ID	realTY = Mtype_complex_to_real( WN_desc(tree));
      {
       /*
	* create the real/imaginary stores
	* load the temporary values into a preg before the store (pv314583)
	* as the store may interfere with the expression.
	*/
	WN	*realexp, *imagexp;

	lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);
	
	realexpN= Make_Leaf(block, realexp, realTY);
	imagexpN= Make_Leaf(block, imagexp, realTY);
      }

      split_istore( block, tree, realTY, 
		    NULL, NULL, &realexpN, &imagexpN,
		    actions, realstore, imagstore );

      lower_complex_maps(tree, realstore, imagstore, actions);
      WN_Delete(tree);

      return imagstore;
    }
    else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_desc(tree)))
    {
      WN	*hi, *lo, *hipart, *lopart;

      lower_quad_expr(block, WN_kid0(tree), actions, &hi, &lo);

      split_istore( block, tree, MTYPE_F8, hi, lo, NULL, NULL, actions,
		    hipart, lopart );

      lower_quad_maps(tree, hipart, lopart, actions);
      WN_Delete(tree);
      return lopart;
    }
    else if (Action(LOWER_PAIRED) &&
	     (MTYPE_is_double(WN_desc(tree)) ||
	      (WN_desc(tree) == MTYPE_I8) || (WN_desc(tree) == MTYPE_U8)))
	      
    {
      WN	*hi, *lo, *hipart, *lopart;

      lower_paired_expr(block, WN_kid0(tree), actions, &hi, &lo);

      split_istore( block, tree,
		    ((WN_desc(tree) == MTYPE_U8) ? MTYPE_U4 : MTYPE_I4),
		    hi, lo, NULL, NULL, actions,
		    hipart, lopart );

      lower_paired_maps(tree, hipart, lopart, actions);
      WN_Delete(tree);
      return lopart;
    }
    else if (Action(LOWER_SPLIT_CONST_OFFSETS))
    {
      /*
       * Split
       *       ADDR
       *     ISTORE (offset>16bits)
       * into
       *         ADDR
       *         CONST (hi)
       *       ADD
       *     ISTORE (lo)
       */
      WN_OFFSET offset = WN_store_offset(tree);

      if (Memory_Offset_Must_Split(WN_desc(tree), offset, NULL, tree))
      {
	WN_kid1(tree) = WN_Add(Pointer_type, WN_kid1(tree),
			       WN_Intconst(Pointer_type,
					   Memory_Offset_Hi(WN_desc(tree), offset,
							    NULL, tree)));
	WN_store_offset(tree) = Memory_Offset_Lo(WN_desc(tree), offset,
						 NULL, tree);
      }
    }
    else
    {
     /*
      * Split
      *       LDA  (c1) <sym> 
      *	    ISTORE (c2)
      * into
      *       LDA  (0) <sym>
      *     ISTORE (c1+c2)
      *
      * provided sym is not a PREG.
      */
      WN  *addr_kid = WN_kid1(tree);

#ifdef TARG_XTENSA
      /* We don't want this for xtensa since (c1+c2) could likely be
         out-of-range for the ISTORE, and we have the global
         canonicalization optimization to assign c1 and c2
         appropriately. */
      if (FALSE)
#else
      if (foldLdaOffset(addr_kid, WN_store_offset(tree)))
#endif
      {
	WN_store_offset(tree) += WN_lda_offset(addr_kid);
	WN_lda_offset(addr_kid) = 0;
      }
     /*
      * Fold
      *         EXPR
      *         CONST (c1)
      *       ADD
      *     ISTORE c2
      * Into
      *	      EXPR
      *     ISTORE c1+c2
      */
      if (WN_operator_is(addr_kid, OPR_ADD) &&
	  foldConstOffset(WN_kid1(addr_kid), WN_store_offset(tree)))
      {
	WN_store_offset(tree) += WN_const_val(WN_kid1(addr_kid));
	WN_kid1(tree) = WN_kid0(addr_kid);
	WN_Delete(WN_kid1(addr_kid));
	WN_Delete(addr_kid);
      }
    }
    break;

  case OPR_STID:
    {
      PREG_NUM last_preg = Get_Preg_Num (PREG_Table_Size(CURRENT_SYMTAB));

      if ((WN_class(tree) == CLASS_PREG) &&
	  (WN_store_offset(tree) > last_preg))
      {
	  DevWarn("lower_store() pregno %d > SYMTAB_last_preg(%d)",
		  WN_load_offset(tree), last_preg);
      }
    }

    if (WN_StoreIsUnused(tree))
    {
      WN	*eval;

      eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

      WN_Delete(tree);

      return eval;
    }

    if (Action(LOWER_MLDID_MSTID) && WN_desc(tree) == MTYPE_M)
      return lower_mstid(block, tree, actions);

    if (Action(LOWER_RETURN_VAL) && WN_desc(tree) == MTYPE_M &&
	WN_opcode(WN_kid0(tree)) == OPC_MMLDID && 
	WN_st(WN_kid0(tree)) == Return_Val_Preg) {
	return lower_return_mstid(block, tree, actions);
    }

    if (Action(LOWER_BIT_FIELD_ID) && WN_desc(tree) == MTYPE_BS) {
      lower_bit_field_id(tree);
      if (Action(LOWER_BITS_OP) && WN_operator (tree) == OPR_STBITS)
	return lower_store_bits (block, tree, actions);
    }

    if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_desc(tree)))
    {
      WN	*realexp, *imagexp;
      TYPE_ID	realTY;

      realTY =	Mtype_complex_to_real( WN_desc(tree));
      lower_complex_expr(block, WN_kid0(tree), actions, &realexp, &imagexp);

     /*
      * create the real/imaginary stores
      * load the temporary values into a preg before the store (pv314583)
      * as the store may interfere with the expression.
      */
      {
        PREG_NUM        realexpN, imagexpN;
	WN *realexp_copy,*imagexp_copy;

	if (WN_operator(realexp) == OPR_CONST) {
	   realexp_copy = realexp;
	} else {
	   realexpN = AssignExpr(block, realexp, realTY);
	   realexp_copy = WN_LdidPreg(realTY, realexpN);
	}

	if (WN_operator(imagexp) == OPR_CONST) {
	   imagexp_copy = imagexp;
	} else {
	   imagexpN = AssignExpr(block, imagexp, realTY);
	   imagexp_copy = WN_LdidPreg(realTY, imagexpN);
	}

	split_stid( block, tree, realTY, imagexp_copy, realexp_copy, actions,
		    imagexp, realexp);

        lower_complex_maps(tree, realexp, imagexp, actions);

        WN_Delete(tree);

        return realexp;
      }
    }

    else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_desc(tree)))
    {
      WN	*hiexp, *loexp, *hipart, *lopart;

      lower_quad_expr(block, WN_kid0(tree), actions, &hiexp, &loexp);

      /*
       * create the hi/lo stores: By hi/lo, hi refers to the half with the
       * higher address, while lo refers to the lower address.
       */
      split_stid( block, tree, MTYPE_F8, hiexp, loexp, actions, hipart, lopart );

      lower_quad_maps(tree, hipart, lopart, actions);

      WN_Delete(tree);

      return lopart;
    }
    else if (Action(LOWER_PAIRED) &&
	     (MTYPE_is_double(WN_desc(tree)) ||
	      (WN_desc(tree) == MTYPE_I8) || (WN_desc(tree) == MTYPE_U8)))
    {
      WN	*hiexp, *loexp, *hipart, *lopart;

      lower_paired_expr(block, WN_kid0(tree), actions, &hiexp, &loexp);

      /*
       * create the hi/lo stores
       */
      split_stid( block, tree,
		  ((WN_desc(tree) == MTYPE_U8) ? MTYPE_U4 : MTYPE_I4),
		  hiexp, loexp, actions, hipart, lopart );

      lower_paired_maps(tree, hipart, lopart, actions);

      WN_Delete(tree);

      return lopart;
    }

    if (Action(LOWER_SPLIT_SYM_ADDRS))
    {
     /*
      * Convert   (STID (offset) <sym>) into
      *       LDA (0) <base> 
      *     ISTORE (offs+ofst)          
      */
      WN  *istore;

      istore =	lower_split_sym_addrs(NULL, tree, WN_store_offset(tree), actions);
      if (istore)
      {
	return lower_store(block, istore, actions);
      }
    }

    if ( Action(LOWER_FORMAL_REF) && WN_class(tree) == CLASS_VAR)
    {
      WN  *istore;

      istore =  lower_formal_ref(tree, WN_store_offset(tree),
				 WN_st(tree), actions);
      if (istore)
      {
	return lower_store(block, istore, actions);
      }
    }

    if ( Action(LOWER_UPLEVEL))
    {
     ST *sym = WN_st(tree);

      if (ST_is_uplevelTemp(sym))
      {
        WN	   *istore;

        istore = lower_uplevel_reference(tree, WN_store_offset(tree), actions);
	return lower_store(block, istore, actions);
      }
    }

    break;

  case OPR_ISTBITS:
  case OPR_STBITS:
    if (Action(LOWER_BITS_OP))
      return lower_store_bits (block, tree, actions);
    break;

  case OPR_MSTORE:
    {
      WN *rhs = WN_kid0(tree);
      Is_True(!(WN_opcode(rhs) == OPC_MMLDID && WN_st(rhs) == Return_Val_Preg),
	      ("MMLDID of Return_Val_Preg cannot be rhs of MSTORE"));
    }

    if (Align_Object)
    {
      INT16 i;
      for (i = 0; i < WN_kid_count(tree); i++)
        WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);

      tree = improve_Malignment(tree, WN_kid1(tree), WN_kid2(tree),
				WN_store_offset(tree));
    }

    if (Action(LOWER_MSTORE))
    {
      if (WN_StoreIsUnused(WN_kid1(tree)))
      {
	WN	*eval;

	eval = lower_eval(block, WN_CreateEval(WN_kid0(tree)), actions);

	WN_Delete(tree);

	return eval;
      }
     /*
      *  rewrite
      *		MSTORE (MCSELECT (expr, MLOAD, MLOAD)
      *  into
      *		MSTORE (MLOAD (CSELECT expr, MLOAD, MLOAD), size)
      */
      if (WN_opcode(WN_kid0(tree)) == OPC_MCSELECT)
      {
	WN	*select, *expr, *mload0, *mload1, *cselect;

	select = WN_kid0(tree);
	expr = WN_kid0(select);
	mload0 = WN_kid1(select);
	mload1 = WN_kid2(select);

	Is_True(WN_operator_is(mload0, OPR_MLOAD),
		("unexpected MSTORE (MCSELECT) pattern"));
	Is_True(WN_operator_is(mload1, OPR_MLOAD),
		("unexpected MSTORE (MCSELECT) pattern"));

	cselect = WN_Cselect(Pointer_type, expr, WN_kid0(mload0),
			     WN_kid0(mload1));
	WN_kid0(mload0) = cselect;
	WN_kid0(tree) = mload0;
      }
      tree = lower_mstore(block, tree, actions);
      kids_lowered = TRUE;
    }

    break;

  }

  /* Lower kids if not done already. */
  if (! kids_lowered)
  {
    INT16 i;
    for (i = 0; i < WN_kid_count(tree); i++)
      WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_eval(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on eval statement <tree>,
 * returning lowered tree.
 *
 * ==================================================================== */

static WN *lower_eval(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN	*child = WN_kid0(tree);

  Is_True(WN_opcode(tree) == OPC_EVAL,
	  ("expected EVAL node, not %s", OPCODE_name(WN_opcode(tree))));

  if (Action(LOWER_MLDID_MSTID) && WN_opcode(child) == OPC_MMLDID) {
      child = lower_mldid(block, child, actions);
  }

  if (Action(LOWER_COMPLEX) && MTYPE_is_complex(WN_rtype(child)))
  {
    WN	*realexp, *imagexp, *eval;

    lower_complex_expr(block, child, actions, &realexp, &imagexp);

    realexp = lower_expr(block, realexp, actions);
    eval = WN_CreateEval(realexp);
    WN_copy_linenum(tree, eval);
    WN_INSERT_BlockLast(block, eval);

    child = imagexp;
  }
  else if (Action(LOWER_QUAD) && MTYPE_is_quad(WN_rtype(child)))
  {
    WN	*hi, *lo, *eval;

    lower_quad_expr(block, child, actions, &hi, &lo);

    hi = lower_expr(block, hi, actions);
    eval = WN_CreateEval(hi);
    WN_copy_linenum(tree, eval);
    WN_INSERT_BlockLast(block, eval);

    child = lo;
  }
  else if (Action(LOWER_PAIRED) &&
	   (MTYPE_is_double(WN_rtype(child)) ||
	    (WN_rtype(child) == MTYPE_I8) || (WN_rtype(child) == MTYPE_U8)))
  {
    child = lower_paired_expr( block, child, actions );
  }
  else if (Action(LOWER_MSTORE) && WN_operator_is(child, OPR_MLOAD))
  {
    TY_IDX mloadTY = TY_pointed(Ty_Table[WN_ty(child)]);

    if (TY_is_volatile(mloadTY))
    {
      DevWarn("eval of volatile (mload) seen. I hoped to never see this");
    }
    else if (Action(LOWER_TO_CG) && traceWoptFinishedOpt)
    {
      DevWarn("eval of (mload) processed (wopt should have removed this)");
    }
    block = lower_mload(block, child, actions);
    return block;
  }

  {
    WN	*eval;

    child = lower_expr(block, child, actions);
    eval = WN_CreateEval(child);
    WN_Delete(tree);

    return eval;
  }
  
}



static INT32
calculateLoadStore(INT64 size, INT64 offset, TYPE_ID quantum, WN *src)
{
  INT32 n = (size-offset) / MTYPE_RegisterSize(quantum);

  return WN_operator_is(src, OPR_INTCONST) ? n : 2*n;
}

static WN *
preg_equivalent_integer (WN *wn)
{
  if ((WN_operator(wn) == OPR_LDID) &&
      (WN_class(wn) == CLASS_PREG))
  {
    PREG_NUM preg = WN_load_offset(wn);
    WN *home = Preg_Home(preg);
    if (home && (WN_operator(home) == OPR_INTCONST))
    {
      return home;
    }
  }

  return wn;
}

/* ====================================================================
 *
 * static MSTORE_ACTIONS GenerateMstoreAction(WN *size, INT32 offset, 
 * TYPE_ID quantum, WN *expr)
 * 
 * Generate loop/inline or memory intrinsic code base on size and expr
 * The number of moves is relative to the quantum.
 * Acount for ld/st based on whether expr is constant
 *
 * for size nonconstant
 *	generate loop 
 *	we could generate a call BTW, but based on what criteria ? 
 *
 * for size constant
 *	nMoves >= MinStructCopyMemIntrSize
 *		generate intrinsic based on expr
 *		special case for memset as the expr must be a char.
 *		this means the expr must be same in all n bytes
 *	nMoves >= MinStructCopyLoopSize
 *		generate loop code
 *	else generate aggregate moves
 * 

 * ==================================================================== */
static MSTORE_ACTIONS
GenerateMstoreAction(WN *size, INT32 offset, TYPE_ID quantum, WN *expr)
{
  MSTORE_ACTIONS action;

#ifdef TARG_XTENSA
  /* If either 'size' or 'expr' is a preg, then see if it is
     equivalent to a constant, and if it is use the constant value. */

  size = preg_equivalent_integer(size);
  expr = preg_equivalent_integer(expr);
#endif
  
  INT32 nMoves;
  BOOL sizeIsConstant =  WN_operator_is(size, OPR_INTCONST);

  if (sizeIsConstant)
  {
    nMoves = calculateLoadStore(WN_const_val(size), offset, quantum, expr);
  }

  if (MinStructCopyMemIntrSize	&&
     ((sizeIsConstant==FALSE)		||
      (sizeIsConstant==TRUE && MinStructCopyMemIntrSize <= nMoves)))
  {
    if (WN_operator_is(expr, OPR_INTCONST))
    {
      INT64	val=	WN_const_val(expr);

      if (val == 0)
	action = MSTORE_intrinsic_memzero;
      else
      {
	WN *i1con= WN_I1const(WN_rtype(expr), val);

	action = (val == WN_const_val(i1con)) ? MSTORE_intrinsic_memset
	  : MSTORE_loop;
	WN_Delete(i1con);
      }
    }
    else if (WN_operator_is(expr, OPR_MLOAD))
    {
      action = MSTORE_intrinsic_memcpy;
    }
    else
    {
      action = MSTORE_loop;
    }
  }
  else if (sizeIsConstant==TRUE		&&
	   MinStructCopyLoopSize	&&
	   MinStructCopyLoopSize <= nMoves)
  {
      action = MSTORE_loop;
  }
  else
  {
    action = (sizeIsConstant) ? MSTORE_aggregate : MSTORE_loop;
  }

  if (traceMload)
  {
    INT32	n;
    char	buff[100];

    n=	sprintf(buff, "GenerateMstoreAction: %s : line %d: quantum %d, ",
		MSTORE_ACTIONS_name(action),
		Srcpos_To_Line(current_srcpos),
		MTYPE_alignment(quantum));

    if (sizeIsConstant)
      sprintf(&buff[n], "size %" LLD_FMT ", nMoves %d", WN_const_val(size), nMoves);
    else
      sprintf(&buff[n], "size unknown");
    DevWarn(buff);
  }

  return action;
}

// If the size of the mtype (used in load/store) is the same as the size of 
// the struct, return the struct's type, otherwise, return a predefined
// type corresponding to mtype.
static inline TY_IDX
struct_memop_type (TYPE_ID mtype, TY_IDX struct_type)
{
    if (TY_size (struct_type) != MTYPE_byte_size (mtype))
	Set_TY_IDX_index (struct_type, TY_IDX_index(MTYPE_To_TY (mtype)));
    return struct_type;
}


/* ====================================================================
 *
 * Auxillary routine to copy aggregrate
 * 
 *  Copy size bytes, starting at offset
 * 
 *   There are so many formals that you probably deserve an explanation
 *	WN *block
 *
 *	TY_IDX srcAlign
 *	TY_IDX dstAlign
 *		alignment restrictions of the generated load/stores as cg
 *		needs to know whether to generate unaligned 
 *	INT32 offset
 *		start copying at (byte) offset
 *	INT32 size (or WN *)
 *		struct (byte) size 
 *		for loop code , this is a size expression
 *	TYPE_ID quantum
 *		unit to load/store
 *	ST *preg
 *	PREG_NUM srcPreg
 *	PREG_NUM dstPreg
 *		if srcAlign is NULL
 *		    srcPreg contains an expression (probably zero)
 *		else
 *		    srcPreg contains the address of the src to copy
 *		dstPreg contains the address of the dst to copy
 *		preg should be Int_Preg.
 *	WN *origLoad
 *	WN *origStore
 *		original load/store nodes (or NULL) needed to preserve alias
 *		maps.
 *
 * ==================================================================== */

static void
copy_aggregate(WN *block, TY_IDX srcAlign, TY_IDX dstAlign, INT32 offset,
	       INT32 size, TYPE_ID quantum, ST *preg, PREG_NUM srcPreg,
	       PREG_NUM dstPreg, WN *origLoad, WN *origStore,
	       INT32 copy_alignment, LOWER_ACTIONS actions)
{
  INT32	stride = MTYPE_RegisterSize(quantum);
  INT32	nMoves = size / stride;
  INT32 chunk_size = 2;
  
  if (size <= 0)
    return;

  if (nMoves>0)
  {
    WN 	**value = (WN **)alloca(sizeof(WN *)*chunk_size);
    FmtAssert(chunk_size >= 1, ("expecting chuck_size >= 1, got %d\n", chunk_size));
    
   /*
    *  generate  unrolled load/store
    */
    while (nMoves)
    {
     /*
      *  semantics are similar to the following:
      *      (quantum) *(dst + offset) =   (quantum) *(src + offset);
      *		or
      *      (quantum) *(dst + offset) =   srcPreg;
      */
      WN 	*addr, *store;
      UINT32    chunk_loads;
      INT32     loffset = offset;
      
      /* Perform up to 'chunk_size' loads into pregs. If we are only
         doing one load in this chunk, then we don't bother to put the
         value in a preg since we will be storing it immediately. */

      chunk_loads = MIN(chunk_size, nMoves);
      for (UINT i = 0; i < chunk_loads; i++)
      {
	if (srcAlign)
	{
	  value[i] = WN_IloadLdid(quantum, loffset,
				  struct_memop_type (quantum, srcAlign),
				  preg, srcPreg);  

	  lower_copy_maps(origLoad, value[i], actions);

	  if (chunk_loads > 1)
	  {
	    PREG_NUM n = Create_Preg(quantum, "aggregate_copy_tmp");
	    WN *stid = WN_StidIntoPreg(quantum, n, preg, value[i]);
	    WN_INSERT_BlockLast(block, stid);
	    value[i] = WN_LdidPreg(quantum, n);
	  }
	}
	else
	{
	  value[i] = WN_LdidPreg(quantum, srcPreg);
	}

	loffset  += stride;
      }

      /* Store each loaded value in the chunk. */
      
      for (UINT i = 0; i < chunk_loads; i++)
      {
	addr = WN_LdidPreg(Pointer_type, dstPreg);

	store = WN_Istore(quantum,
			  offset,
			  Make_Pointer_Type (struct_memop_type (quantum,
								dstAlign)),
			  addr,
			  value[i]);
	lower_copy_maps(origStore, store, actions);

        WN_copy_linenum(origStore, store);

	WN_INSERT_BlockLast(block, store);

	offset  += stride;
	size -= stride;
	nMoves--;
      }
    }
  }

  if (size > 0)
  {
   /*
    *  If there is a residue we must recompute a new quantum
    *  and generate a copy for that.
    */
    quantum = compute_next_copy_quantum(quantum, copy_alignment);

    copy_aggregate(block,
		   srcAlign, dstAlign,
		   offset,
		   size,
		   quantum,
		   preg,
		   srcPreg, dstPreg,
		   origLoad, origStore,
		   copy_alignment,
		   actions);
  }
}




static void
copy_element_and_increment(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
			   PREG_NUM offsetN, TYPE_ID quantum, PREG_NUM srcPreg,
			   PREG_NUM dstPreg, WN *origLoad, WN *origStore,
			   LOWER_ACTIONS actions)
{
 /*
  *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
  *		or
  *   (quantum) *(dst + offset) =   srcPreg;
  *    		
  *  and increment offset
  *
  *		offset		+=  stride;
  */
  INT32  stride = MTYPE_RegisterSize(quantum);
  WN 	*value, *addr, *store, *add, *inc;
  ST	*intPreg = MTYPE_To_PREG(Integer_type);

  if (srcAlign)
  {
    addr = WN_Add(Pointer_type,
		  WN_LdidPreg(Pointer_type, srcPreg),
		  WN_LdidPreg(Integer_type, offsetN));
    value = WN_CreateIload (OPR_ILOAD, Mtype_comparison(quantum), quantum,
			    0, struct_memop_type (quantum, srcAlign),
			    Make_Pointer_Type (srcAlign), addr);

    lower_copy_maps(origLoad, value, actions);
  }
  else
  {
    value = WN_LdidPreg(quantum, srcPreg);
  }

  addr = WN_Add(Pointer_type,
		WN_LdidPreg(Pointer_type, dstPreg),
		WN_LdidPreg(Integer_type, offsetN));
  store = WN_Istore(quantum,
		    0,
		    Make_Pointer_Type(struct_memop_type (quantum, dstAlign)),
		    addr,
		    value);

  lower_copy_maps(origStore, store, actions);

  WN_copy_linenum(origStore, store);
  WN_INSERT_BlockLast(block, store);

  /*
   *  offset += stride
   */

  add  = WN_Add(Integer_type,
 		WN_LdidPreg(Integer_type, offsetN),
  		WN_Intconst(Integer_type, stride));
  inc = WN_StidIntoPreg(Integer_type, offsetN, intPreg, add);
  WN_copy_linenum(origStore, inc);
  WN_INSERT_BlockLast(block, inc);
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate loop 
 *
 *	The size must be an integer constant
 *
 * ==================================================================== */
static void
copy_aggregate_loop_const(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
			  INT32 offset, INT32 size, TYPE_ID quantum,
			  PREG_NUM srcPreg, PREG_NUM dstPreg, WN *origLoad,
			  WN *origStore, INT32 copy_alignment,
			  LOWER_ACTIONS actions)
{
 /*
  *  generate the following
  *    n = nMoves;
  *    index = 0;
  *    do
  *    {
  *	(quantum) *(dst + offset) =   (quantum) *(src + offset);
  *	       n--;
  *    } while(n>0);
  *
  * Try generating a DO loop instead of a WHILE loop
  *
  *	(TBD)	we should really build an array expression 
  *		dst[offset] = src[offset]
  */
  PREG_NUM		offsetN;
  ST		*intPreg = MTYPE_To_PREG(Integer_type);
  INT32		stride   = MTYPE_RegisterSize(quantum);
  INT64		nMoves   = size  / stride;
  INT64		residue  = size - ( nMoves * stride );

  /*
   *  Bail out if there is nothing to move and no residue
   */
  if ((nMoves <= 0) && residue == 0)
    return;

  offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset), Integer_type);

 /*
  *	create loop count variable, traditionally called n
  * 	offset is most likely zero
  */
  if (nMoves > 0)
  {
    PREG_NUM	n;
    WN	 	*body;
    WN          *incr;
    WN          *start;
    WN          *test;
    WN	        *doLoop;

    n = Create_Preg(Integer_type,"mstore_loopcount");
    body= WN_CreateBlock();
    start = WN_StidIntoPreg( Integer_type, n, intPreg,
			     WN_Intconst(Integer_type, nMoves));
    incr  = WN_StidIntoPreg( Integer_type, n, intPreg,
			     WN_Sub(Integer_type,
				    WN_LdidPreg(Integer_type, n),
				    WN_Intconst(Integer_type, 1)));
    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
    
   /*
    *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *   (quantum) *(dst + offset) =   srcPreg;
    *   and increment offset
    *			offset    +=  stride;
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN,
			       quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);
    doLoop = WN_CreateDO(WN_CreateIdname(n, intPreg),
			 start, test, incr, body, NULL);
    WN_copy_linenum(origStore, doLoop);
    WN_INSERT_BlockLast(block, doLoop);
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_mstore_to_loop( origStore, doLoop, nMoves );
  }

 /*
  *  If there is a residue we must recompute a new quantum
  *  and generate a copy for that.
  *
  *	if (residue > 0)
  *	{
  *	    if (residue >= 4)
  *	    {
  *  		(int) *(dst + offset) =   (int) *(src + offset);
  *		offset  += 4;
  *		residue -= 4;
  *	    }
  *	    if (residue >= 2)
  *	    {
  *  		 (short) *(dst + offset) =   (short) *(src + offset);
  *		offset  += 2;
  *		residue -= 2;
  *	    }
  *	    etc.
  *	}
  */
  if (residue)
  {
    WN	  *residue_block=  WN_CreateBlock();

    while(residue>0)
    {
      quantum = compute_next_copy_quantum(quantum, copy_alignment);

      while (residue >= MTYPE_alignment(quantum))
      {
	copy_element_and_increment(residue_block, srcAlign, dstAlign,
				   offsetN, quantum, srcPreg, dstPreg,
				   origLoad, origStore, actions);
	residue -= MTYPE_alignment(quantum);
      }
    }
    WN_copy_linenum(origStore, residue_block);
    WN_INSERT_BlockLast(block, residue_block);
  }
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate loop 
 *
 *	The size must be an expression
 *
 * ==================================================================== */

static void
copy_aggregate_loop_n(WN *block, TY_IDX srcAlign, TY_IDX dstAlign,
		      INT32 offset, WN *size, TYPE_ID quantum,
		      PREG_NUM srcPreg, PREG_NUM dstPreg, WN *origLoad,
		      WN *origStore, INT32 copy_alignment,
		      LOWER_ACTIONS actions)
{
 /*
  *  generate the following
  *    n = nMoves;
  *    index = 0;
  *    while(n>0)
  *    {
  *	(quantum) *(dst + offset) =   (quantum) *(src + offset);
  *	       n--;
  *    }
  *
  *	(TBD)	we should really build an array expression 
  *		dst[offset] = src[offset]
  */
  PREG_NUM	offsetN, nMovesN, residueN;
  ST		*intPreg = MTYPE_To_PREG(Integer_type);
  INT32		stride   = MTYPE_RegisterSize(quantum);

  Is_True((WN_operator(size)!=OPR_INTCONST),("unexpected const"));

 /*
  *  Create the following expressions
  *
  *	nMovesn = ( size ) / stride
  *
  *	residue =   size - ( nMoves * stride )
  */
  {
    WN  *nMoves, *residue;

    nMoves   = WN_Div(Integer_type, WN_COPY_Tree(size),
		      WN_Intconst(Integer_type, stride));

    nMovesN  = AssignExpr(block, nMoves, Integer_type);

    residue  = WN_Sub(Integer_type,
		      WN_COPY_Tree(size),
		      WN_Mpy(Integer_type,
			     WN_LdidPreg(Integer_type, nMovesN),
			     WN_Intconst(Integer_type, stride)));

    residueN = AssignExpr(block, residue, Integer_type);
  }

 /*
  *	create loop count variable, traditionally called n
  * 	offset is most likely zero
  */
#if 0
  n = AssignExpr(block, WN_LdidPreg(Integer_type, nMovesN), Integer_type);

  offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset), Integer_type);

  {
    WN	*body, *sub, *dec, *test, *whileDo;

    body = WN_CreateBlock();
   /*
    *	while(n>0)
    *   {
    *
    *	  (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *	  (quantum) *(dst + offset) =   srcPreg;
    *
    *			offset    +=  stride;
    *
    * 			n --;
    *   }
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN, quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);

    sub  = WN_Sub(Integer_type,
		  WN_LdidPreg(Integer_type, n),
		  WN_Intconst(Integer_type, 1));
    dec  = WN_StidIntoPreg(Integer_type, n, intPreg, sub);
    WN_INSERT_BlockLast(body, dec);


    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
  	
    whileDo = WN_CreateWhileDo(test, body);
    // ADD FEEDBACK INFO !!!
    WN_INSERT_BlockLast(block, whileDo);
  }
#else
  {
    PREG_NUM	n;
    WN	 	*body;
    WN          *incr;
    WN          *start;
    WN          *test;
    WN	        *doLoop;

    n = Create_Preg(Integer_type,"mstore_loopcount");
    offsetN = AssignExpr(block, WN_Intconst(Integer_type, offset),
			 Integer_type);

    body= WN_CreateBlock();
    start = WN_StidIntoPreg(Integer_type, n, intPreg,
			    WN_LdidPreg(Integer_type, nMovesN));
    incr  = WN_StidIntoPreg(Integer_type, n, intPreg,
			    WN_Sub(Integer_type,
				   WN_LdidPreg(Integer_type, n),
				   WN_Intconst(Integer_type, 1)));
    test = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, n),
		 WN_Zerocon(Integer_type));
    
   /*
    *   (quantum) *(dst + offset) =   (quantum) *(src + offset);
    *		or
    *   (quantum) *(dst + offset) =   srcPreg;
    *   and increment offset
    *			offset    +=  stride;
    */
    copy_element_and_increment(body,
			       srcAlign, dstAlign,
			       offsetN,
			       quantum,
			       srcPreg, dstPreg,
			       origLoad, origStore,
			       actions);
    doLoop = WN_CreateDO(WN_CreateIdname(n,intPreg),start,test,incr,body,NULL);
    WN_copy_linenum(origStore, doLoop);
    WN_INSERT_BlockLast(block, doLoop);
  }
  
#endif





 /*
  *  If there is a residue we must recompute a new quantum
  *  and generate a copy for that.
  *
  *	if (residue > 0)
  *	{
  *	    if (residue >= 4)
  *	    {
  *  		(int) *(dst + offset) =   (int) *(src + offset);
  *		offset  += 4;
  *		residue -= 4;
  *	    }
  *	    if (residue >= 2)
  *	    {
  *  		 (short) *(dst + offset) =   (short) *(src + offset);
  *		offset  += 2;
  *		residue -= 2;
  *	    }
  *	    etc.
  *	}
  *   We supply an incorrect alignment to compute_next_copy_quantum()
  *   because we cant skip the unaligned halfword 
  */
  {
    WN	*if_then, *unused, *cond, *IF;

    if_then = WN_CreateBlock();
    unused = WN_CreateBlock();	/* will be empty */

    cond = WN_GT(Integer_type,
		 WN_LdidPreg(Integer_type, residueN),
		 WN_Zerocon(Integer_type));

    IF = WN_CreateIf(cond, if_then, unused);
    // ADD FEEDBACK INFO !!

    quantum = compute_next_copy_quantum(quantum,
					MTYPE_alignment(Max_Uint_Mtype));
  	
    while(MTYPE_alignment(quantum) > 0)
    {
      WN	*if_residue_block, *unused, *test, *IF_residue;


      if_residue_block = WN_CreateBlock();
      unused = WN_CreateBlock();

      test = WN_GE(Integer_type,
		   WN_LdidPreg(Integer_type, residueN),
		   WN_Intconst(Integer_type, MTYPE_alignment(quantum)));

      IF_residue = WN_CreateIf(test, if_residue_block, unused);
      // ADD FEEDBACK INFO !!
      lower_copy_maps(origStore, IF_residue, actions);
	
      copy_element_and_increment(if_residue_block, srcAlign, dstAlign,
				 offsetN, quantum, srcPreg, dstPreg,
				 origLoad, origStore, actions);

     /*
      *  residue -= stride
      */
      {
	WN	*sub, *dec;

	sub  = WN_Sub(Integer_type,
		      WN_LdidPreg(Integer_type, residueN),
		      WN_Intconst(Integer_type, MTYPE_alignment(quantum)));
	dec = WN_StidIntoPreg(Integer_type, residueN, intPreg, sub);
        lower_copy_maps(origStore, dec, actions);
        WN_copy_linenum(origStore, dec);
	WN_INSERT_BlockLast(if_residue_block, dec);
      }

      WN_copy_linenum(origStore, IF_residue);
      WN_INSERT_BlockLast(if_then, IF_residue);

      quantum = compute_next_copy_quantum(quantum,
					  MTYPE_alignment(Max_Uint_Mtype));
    }
    WN_copy_linenum(origStore, IF);
    WN_INSERT_BlockLast(block, IF);
  }
}




/* ====================================================================
 *
 * Auxillary routine to copy aggregrate (loop version)
 *
 *  The size can be an expression or integer constant of any value
 *  so it must be tested
 *
 *  loop_const:	will generate a
 *		n= CONST;	do {} while(n<0);
 *
 *  loop_n:	will generate a
 *		n= size;	while(n<0) {};
 *
 * ==================================================================== */

static void
copy_aggregate_loop(WN *block, TY_IDX srcTY, TY_IDX dstTY, INT32 offset,
		    WN *size, TYPE_ID quantum, ST *preg, PREG_NUM srcPreg,
		    PREG_NUM dstPreg, WN *origLoad, WN *origStore,
		    INT32 copy_alignment, LOWER_ACTIONS actions)
{
  if (WN_operator_is(size, OPR_INTCONST))
  {
    if (WN_const_val(size)>0)
    {
      copy_aggregate_loop_const(block,
				srcTY, dstTY,
				offset,
				WN_const_val(size),
				quantum,
				srcPreg, dstPreg,
				origLoad, origStore,
				copy_alignment,
				actions);
    }
  }
  else
  {
    copy_aggregate_loop_n(block,
			  srcTY, dstTY,
			  offset,
			  size,
			  quantum,
			  srcPreg,
			  dstPreg,
			  origLoad, origStore,
			  copy_alignment,
			  actions);
  }
}

/*
 *  mark LDA as addr taken passed for intrinsic memory routines.
 *  Do not recurse on ILOAD (it would mark the pointer as taken_passed
 *  and not the object)
 */
static void markAddrTakenPassed(WN *expr)
{
  switch(WN_operator(expr))
  {
  case OPR_LDA:
    {
      ST *sym = WN_st(expr);
      // if (ST_class(sym) == CLASS_VAR || ST_class(sym) == CLASS_FUNC) {
      //   Do nothing here. ADDR flags should only grow more
      //   optimistic; they should never become more conservative,
      //   because the program's semantics cannot grow worse as we
      //   compile it.
      // }
    }
    return;
  case OPR_ILOAD:
  case OPR_ILOADX:
    return;
  }

  {
    INT32	i;
    for (i = 0; i < WN_kid_count(expr); i++)
      markAddrTakenPassed(WN_actual(expr,i));
  }
}


static WN *WN_Generate_Intrinsic_Call(INTRINSIC id, INT32 nparm, WN *parm[])
{
  ST		*sym;
  TYPE_ID	rtype = INTR_return_mtype(id);

  if (rtype == MTYPE_UNKNOWN) rtype = MTYPE_V;

  {
    TY_IDX ty = Make_Function_Type(MTYPE_To_TY(rtype));

    sym = Gen_Intrinsic_Function(ty, INTRN_rt_name(id));

    WN_annotate_intrinsic_flags(id, sym);
  }

  {
    WN	*call = WN_Call(rtype, MTYPE_V, nparm, sym);
    WN_Set_Linenum(call, current_srcpos);

    while(--nparm >= 0)
    {
      WN_actual(call, nparm)=	parm[nparm];
    }

    WN_annotate_call_flags(call, sym);

    return call;
  }
}


static void copy_aggregate_memset(WN *block, TY_IDX dstTY, WN *src, WN *dst,
				  WN *size)
{
  WN  *call, *parms[3];

  markAddrTakenPassed(dst);
  parms[0]=  WN_CreateParm(Pointer_type,
			   dst,
			   dstTY,
			   WN_PARM_BY_VALUE);

  parms[1]=  WN_CreateParm(WN_rtype(src),
			   src,
			   MTYPE_To_TY(WN_rtype(src)),
			   WN_PARM_BY_VALUE);

  parms[2]=  WN_CreateParm(WN_rtype(size),
			   WN_COPY_Tree(size),
			   MTYPE_To_TY(WN_rtype(size)),
			   WN_PARM_BY_VALUE);

  call=	WN_Generate_Intrinsic_Call(INTRN_MEMSET, 3, parms);

  WN_copy_linenum(dst, call);
  WN_INSERT_BlockLast(block, call);
}

static void copy_aggregate_memcpy(WN *block, INT32 offset, TY_IDX srcTY,
				  TY_IDX dstTY, WN *src, WN *dst, WN *size)
{
  WN  *call, *parms[3];

  if (offset)
  {
    src = WN_Add(Pointer_type, src, WN_Intconst(Pointer_type, offset));
    dst = WN_Add(Pointer_type, dst, WN_Intconst(Pointer_type, offset));
  }
  markAddrTakenPassed(src);
  markAddrTakenPassed(dst);

  parms[0]=  WN_CreateParm(Pointer_type,
			   dst,
			   dstTY ? dstTY : MTYPE_To_TY(WN_rtype(dst)),
			   WN_PARM_BY_VALUE);

  parms[1]=  WN_CreateParm(WN_rtype(src),
			   src,
			   srcTY ? srcTY : MTYPE_To_TY(WN_rtype(src)),
			   WN_PARM_BY_VALUE);

  parms[2]=  WN_CreateParm(WN_rtype(size),
			   WN_COPY_Tree(size),
			   MTYPE_To_TY(WN_rtype(size)),
			   WN_PARM_BY_VALUE);

  call=	WN_Generate_Intrinsic_Call(INTRN_MEMCPY, 3, parms);

  WN_copy_linenum(dst, call);
  WN_INSERT_BlockLast(block, call);
}



/* ====================================================================
 *
 * These routine compute alignment used by the MSTORE/MLOAD routines
 *
 *	aligned memory access
 *		return minimum required alignment
 *
 *	otherwise
 *		form a ratio based on align vrs unaligned reference.
 *		Each unaligned ld/st requires 2 instructions.
 *		In addition, some processors may support 2 memory operations
 *		per clock, but only one unaligned reference (r8k and alien)
 *
 * When we can use unaligned quantums, do not return halfword
 * quantums as there is no hardware support (pv544367)
 *
 * ==================================================================== */

extern INT32 compute_copy_alignment(TY_IDX src, TY_IDX dst, INT32 offset)
{
  INT32	srcAlign, dstAlign, align;
  INT32 max=	MTYPE_alignment(Max_Uint_Mtype);

  srcAlign= (src) ? TY_align(src) : max;
  dstAlign= (dst) ? TY_align(dst) : max;

  align= MIN(srcAlign, dstAlign);
  align= MIN(align, max);

  if (compute_offset_alignment(offset, align) != align) {
    DevWarn("Potentially misaligned memory access: align = %d offset = %d\n", align, offset);
  }

  return MIN(align, max);
}


extern TYPE_ID compute_copy_quantum(INT32 alignment)
{
  if (UseAlignedCopyForStructs==FALSE)
  {
    INT32  maxAlign= MTYPE_alignment(Max_Uint_Mtype);
    INT32  ratio = Copy_Quantum_Ratio();
    
    if (alignment*ratio < maxAlign) 
      return Max_Uint_Mtype;
  }

  return Mtype_AlignmentClass(alignment, MTYPE_CLASS_UNSIGNED_INTEGER);
}

static TYPE_ID compute_next_copy_quantum(TYPE_ID quantum, INT32 alignment)
{
  TYPE_ID next = Mtype_prev_alignment(quantum);

  if (UseAlignedCopyForStructs==FALSE)
  {
    if ((MTYPE_alignment(next) > alignment)	&&
        (next == MTYPE_U2 || next == MTYPE_I2))
      next = Mtype_prev_alignment(next);
  }
  return next;
}




/* ====================================================================
 *
 * TY_IDX computeNewAlignmentTY(addr, type, WN_const_val(size), offset);
 *
 * Compute a new quantum (alignment), subject to size restrictions.
 * For the new quantum to apply we must have an ST of the appropriate
 * type/offset to align/pad
 *
 * ==================================================================== */

static TY_IDX computeNewAlignmentTY(WN *addr, TY_IDX type, INT32 size,
				    INT64 offset)
{
  TY_IDX newType;
  INT32	align =	TY_align(type);
  INT32	maxAlign = MTYPE_alignment(Max_Uint_Mtype);

  if ((size <= align) || (align >= maxAlign))
    return TY_IDX_ZERO;

 /*
  *  create offset consistent with the offset of addr and offset
  */
  newType = compute_alignment_type(addr, type, offset);
 
  if (WN_operator_is(addr, OPR_LDA) && ST_class(WN_st(addr)) != CLASS_BLOCK)
  {
    ST	   *sym     = WN_st(addr);
    TY_IDX  symType = compute_alignment_type(addr, ST_type(sym), offset);
   /*
    *  If the objected is defined in the current module but not allocated yet
    *  we are free to redefine the alignment so compute largest possible
    *  consistent alignment. The alignment should be consistent with the
    *  offset of course
    */
    if (ST_pu_defined(sym) && !Is_Allocated(sym))
    {
      INT32  stAlign;

      stAlign = compute_offset_alignment(WN_lda_offset(addr), maxAlign);
      stAlign = compute_offset_alignment(offset, stAlign);

      if (TY_align(symType) < stAlign)
      {
        if (traceAlignment)
        {
          DevWarn("realign ST %s (from %d to %d) line %d", ST_name(sym),
		  TY_align(symType), stAlign, Srcpos_To_Line(current_srcpos));
        }
	// Convert Make_Align_Type once real iterators are implemented
	// to iterate over the TY table.
	symType = Make_Align_Type(symType, stAlign);

        if ((ST_class(sym) == CLASS_CONST)              &&
            (TCON_ty(STC_val(sym)) == MTYPE_STR)        &&
             TY_is_pointer(Ty_Table[ST_type(sym)]))
        {
          symType = Make_Pointer_Type(newType);
        }
        Set_ST_type(sym, symType);

        if (stAlign > align)
        {
          return Make_Align_Type(newType, stAlign);
        }
        return TY_IDX_ZERO;
      }
    }
  }

  if (TY_align(newType) > align)
    return newType;

  return TY_IDX_ZERO;
}




static WN *compute_new_size(WN *tree, WN *size, INT32 align)
{
  if (Align_Padding)
  {
    INT64  sizeN   = WN_const_val(size);
    INT64  residue = sizeN % align;
    INT32  popcnt  = TARG_INT_Pop_Count(residue);

    if (popcnt > 1)
    {
      INT64  newsize = sizeN + (align-residue);

      return WN_Intconst(WN_rtype(size), newsize);
    }
  }
  return size;
}




/* ====================================================================
 *
 * WN *improve_Malignment(WN *tree, WN *addr, WN *size, INT64 offset)
 *
 * Given an mstore or mload try to improve the alignment (if possible)
 *
 *
 * ==================================================================== */
static WN *improve_Malignment(WN *tree, WN *addr, WN *size, INT64 offset)
{
  TY_IDX type, newType;

  if (Align_Object == FALSE)
    return tree;

  if (!WN_operator_is(size, OPR_INTCONST))
  {
    return tree;
  }

  TY &tree_ty = Ty_Table[WN_ty(tree)];

  if (TY_is_f90_pointer(tree_ty)) {
     /* It's not safe to realign F90 pointers */
     return tree;
  }

  type = TY_pointed(tree_ty);

  newType = computeNewAlignmentTY(addr, type, WN_const_val(size), offset);

  if (newType)
  {
    if (traceAlignment)
    {
      DevWarn("realign OPCODE %s (from %d to %d) line %d",
	      OPCODE_name(WN_opcode(tree)), TY_align(type), TY_align(newType),
	      Srcpos_To_Line(current_srcpos));
    }

    WN_set_ty(tree, Make_Pointer_Type(newType));

    switch(WN_operator(tree))
    {
    case OPR_MSTORE:
      WN_kid2(tree)= compute_new_size(tree, size, TY_align(newType));
      break;

    case OPR_MLOAD:
      WN_kid1(tree)= compute_new_size(tree, size, TY_align(newType));
      break;
    }
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_mstore(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on mstore statement <tree>,
 * returning lowered tree.
 *
 * The following control the expansion
 *    
 *   GenerateMstoreAction
 *	  See notes on this function for more information
 *
 *    BOOL UseAlignedCopyForStructs
 *	  TRUE
 *		expand using minimum common alignment 
 *	  FALSE 
 *		expand using maximum integer type
 *
 * ==================================================================== */

static WN *lower_mstore(WN *block, WN *mstore, LOWER_ACTIONS actions)
{
  WN		*load, *addr, *size;
  SRCPOS	srcpos = WN_Get_Linenum(mstore);
  CURRENT_STATE	mstate = pushCurrentState(mstore, actions);

  load =	WN_kid0(mstore);
  addr =	WN_kid1(mstore);
  size =	WN_kid2(mstore);

  Is_True((WN_opcode(mstore) == OPC_MSTORE),
	  ("expected MSTORE node, not %s", OPCODE_name(WN_opcode(mstore))));

  if (WN_field_id (mstore) != 0)
      lower_field_id (mstore);

  block = WN_CreateBlock();
  {
    TY_IDX  dstTY = TY_pointed(Ty_Table[WN_ty(mstore)]);
    TY_IDX  srcTY = TY_IDX_ZERO;
    WN     *expr = load;

   /*
    *  The "load" may be any expression (ex. constant 0)
    *  If so, there should be no alignment restrictions
    */
    if (WN_opcode(load) == OPC_MLOAD)
    {
      if (WN_field_id(load) != 0)
	lower_field_id (load);

      srcTY = TY_pointed(Ty_Table[WN_ty(load)]);
      expr =	WN_kid0(load);
     /*
      *  the mload may have an offset
      */
      if (WN_load_offset(load))
      {
        expr = WN_Add(Pointer_type,
		      expr,
		      WN_Intconst(Pointer_type, WN_load_offset(load)));
	WN_kid0(load)=	expr;
      }
    }
    else
    {
      load =		NULL;
    }

    {
      INT32	copy_alignment;
      PREG_NUM	dstPreg, srcPreg;
      TYPE_ID	quantum;
      ST	*preg=    MTYPE_To_PREG(Pointer_type);

      if (WN_store_offset(mstore))
      {
        addr = WN_Add(Pointer_type,
		      addr,
		      WN_Intconst(Pointer_type, WN_store_offset(mstore)));
      }

      copy_alignment = compute_copy_alignment(srcTY, dstTY,
					      WN_store_offset(mstore));
      quantum =	compute_copy_quantum(copy_alignment);

      switch(GenerateMstoreAction(size, 0, quantum, WN_kid0(mstore)))
      {
      case MSTORE_intrinsic_memzero:
	copy_aggregate_memset(block, WN_ty(mstore), WN_Intconst(MTYPE_U4, 0), addr, size);
        if (Cur_PU_Feedback)
          Cur_PU_Feedback->FB_lower_mstore_to_call(mstore, WN_last(block));
	break;

      case MSTORE_intrinsic_memset:
	copy_aggregate_memset(block, WN_ty(mstore), expr, addr, size);
        if (Cur_PU_Feedback)
          Cur_PU_Feedback->FB_lower_mstore_to_call(mstore, WN_last(block));
	break;

      case MSTORE_intrinsic_memcpy: 
	copy_aggregate_memcpy(block, 0, TY_pointer(srcTY), TY_pointer(dstTY),
			      expr, addr, size);
        if (Cur_PU_Feedback)
          Cur_PU_Feedback->FB_lower_mstore_to_call(mstore, WN_last(block));
	break;

      case MSTORE_loop:
        srcPreg = AssignExpr(block, expr, WN_rtype(expr));
        dstPreg = AssignExpr(block, addr, Pointer_type);
	copy_aggregate_loop(block,
			    srcTY,
			    dstTY,
			    0,
			    size,
			    quantum,
			    preg,
			    srcPreg,
			    dstPreg,
			    load,
			    mstore,
			    copy_alignment,
			    actions);
	break;

      case MSTORE_aggregate:
        srcPreg = AssignExpr(block, expr, WN_rtype(expr));
        dstPreg = AssignExpr(block, addr, Pointer_type);
	copy_aggregate(block,
		       srcTY,
		       dstTY,
		       0,
      		       WN_const_val(size),
		       quantum,
		       preg,
		       srcPreg,
		       dstPreg,
		       load,
		       mstore,
		       copy_alignment,
		       actions);
	break;
      }
    }
  }

  WN_Delete( WN_kid2(mstore));
  WN_Delete(mstore);

  block = lower_block(block, actions);

  popCurrentState(mstate);
  return block;
}



/* ====================================================================
 *
 * WN *lower_mload(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * This expansion is generated only for eval (mload), where the TY is volatile
 * I hope this never happens in practice
 * With gccfe this can happen when TY is not volatile and we don't optimize.
 *
 * ==================================================================== */

static WN *lower_mload(WN *block, WN *mload, LOWER_ACTIONS actions)
{
  PREG_NUM  srcPreg;
  TYPE_ID   quantum;
  INT32     size, copy_alignment;

  Is_True((WN_opcode(mload) == OPC_MLOAD),
	  ("expected MLOAD node, not %s", OPCODE_name(WN_opcode(mload))));
  Is_True((WN_operator_is(WN_kid1(mload), OPR_INTCONST)),
	  ("expected MLOAD size constant  "));

  if (WN_field_id (mload) != 0)
      lower_field_id (mload);

  WN       *addr   = WN_kid0(mload);
  INT32     offset = WN_load_offset(mload);
  TY_IDX    srcTY  = TY_pointed(Ty_Table[WN_ty(mload)]);

  block = WN_CreateBlock();

  size = WN_const_val( WN_kid1(mload) );
  copy_alignment = compute_copy_alignment( srcTY, TY_IDX_ZERO, offset );
  quantum = compute_copy_quantum( copy_alignment );

  srcPreg = AssignExpr(block, addr, WN_rtype(addr));

  while (size > 0)
  {
    INT32 stride = MTYPE_RegisterSize(quantum);
    INT32 nMoves = size / stride;

   /*
    *  generate  unrolled eval (load)
    */
    while (nMoves > 0)
    {
      WN	*value, *eval;

      // use quantum TY rather than srcTY since want quantum-sized loads.
      value = WN_IloadLdid(quantum, offset, MTYPE_To_TY(quantum),
			   MTYPE_To_PREG(Pointer_type), srcPreg);

      lower_copy_maps(mload, value, actions);

      eval = WN_CreateEval(value);

      WN_INSERT_BlockLast(block, eval);

      offset += stride;
      size   -= stride;
      nMoves--;
    }
   /*
    *  If there is a residue we must recompute a new quantum
    *  and generate a copy for that.
    */
    if (size > 0)
    {
      quantum = compute_next_copy_quantum(quantum, copy_alignment);
    }
  }

  WN_Delete( WN_kid1(mload));
  WN_Delete(mload);

  block = lower_block(block, actions);

  return block;
}




/* ====================================================================
 *
 * void lower_complex_actual
 *
 * Perform lowering (see WN_Lower description) for complex actuals
 * This is used in fortran only , and only by value (%val)
 *
 *
 * ==================================================================== */

static void lower_complex_actual (WN *block, WN *val, PLOC ploc,
				  LOWER_ACTIONS actions)
{
  WN     *size, *mload, *addr;
  TY_IDX  complexType = TY_Of_Expr(val);

  size = WN_Intconst(Integer_type, TY_size(Ty_Table[complexType]));

  addr = make_pointer_to_node(block, val);

  mload = WN_CreateMload(0, Make_Pointer_Type(complexType), addr, size);

  lower_mload_actual (block, mload, ploc, actions);
}




/* ====================================================================
 *
 * void lower_mload_actual
 *
 * Perform lowering (see WN_Lower description) for structure actuals
 * the structure will be copied to [ regs ] and [ stack ]
 *
 *
 * ==================================================================== */

static void lower_mload_actual (WN *block, WN *mload, PLOC ploc,
				LOWER_ACTIONS actions)
{
  ST       *preg;
  INT32     size, mloadOffset = 0; 
  PREG_NUM  addrN;

  if (WN_field_id(mload) != 0)
      lower_field_id (mload);

  TY_IDX    mloadTY = TY_pointed(Ty_Table[WN_ty(mload)]);
  WN       *mloadSize = WN_kid1(mload);

  if (WN_operator(mloadSize) != OPR_INTCONST) {
	DevWarn("mload_actual size is not INTCONST");
	if (WN_operator(mloadSize) == OPR_CVTL) {
		mloadSize = WN_kid0(mloadSize);
	}
  }
  size = WN_const_val(mloadSize);
  if (size<=0 || WN_operator(mloadSize) != OPR_INTCONST)
  {
    DevWarn("size of mload actual should be > 0");
    size = TY_size(Ty_Table[mloadTY]);
    DevWarn("adjusting size of (%s) to TY_size= %d",
	    TY_name(Ty_Table[mloadTY]), size);
    mloadSize = WN_Intconst(Integer_type, size);
  }

  Is_True((WN_opcode(mload) == OPC_MLOAD),
	  ("expected MLOAD node, not %s", OPCODE_name(WN_opcode(mload))));

  Setup_Struct_Output_Parameter_Locations(mloadTY);
  ploc = Get_Struct_Output_Parameter_Location(ploc);

 /*
  *  create addrN assignment to hold the address of the mload
  *  Adjust the address if there is an offset in the mload
  */
  preg = MTYPE_To_PREG(Pointer_type);
  {
    WN	*addr = WN_COPY_Tree(WN_kid0(mload));

    if (WN_load_offset(mload))
    {
      addr = WN_Add(Pointer_type,
                    addr,
                    WN_Intconst(Pointer_type, WN_load_offset(mload)));
    }
    addrN = AssignExpr(block, addr, Pointer_type);
  }

  /* xtensa has special cases for structs smaller than a word on big endian machines. 
     They are passed in the least significant bits, even though the trailing portions 
     of a struct larger than a word are passed in the most significant bits.
     
     This code duplicates much of what goes on below, but getting that to work with 
     our special cases proved too painful.
  */
  if (Target_Byte_Sex == BIG_ENDIAN && size < MTYPE_byte_size(MTYPE_U4)) 
  {
    /* load a small struct into a register, then leave it in the register as a 
       parameter or store it out to the proper location on the stack. */
    
    /* must use 8 bit loads here because sometimes two and three byte structures
       start at odd addresses, and the offset from the sym won't tell us. */
    
    /* load the first byte */
    WN * load = WN_IloadLdid(MTYPE_U1, mloadOffset, MTYPE_To_TY(MTYPE_U1), preg, addrN);
    lower_copy_maps(mload, load, actions);
    if (size > 1)
    {
      /* shift the first byte over, then load the second byte and or it in */
      ST * preg1 = MTYPE_To_PREG(Pointer_type);
      WN * shiftn0 = WN_Shl(MTYPE_U4, load, WN_Intconst(MTYPE_U4, 8));
      
      WN * load1 = WN_IloadLdid(MTYPE_U1, mloadOffset + 1, 
				MTYPE_To_TY(MTYPE_U1), preg1, addrN);
      load = WN_Bior(MTYPE_U4, shiftn0, load1);
      if (size > 2)
      {
	/* shift the first two bytes over, load the third and or it in */
	ST * preg2 = MTYPE_To_PREG(Pointer_type);
	WN * shiftn1 = WN_Shl(MTYPE_U4, load, WN_Intconst(MTYPE_U4, 8));
	
	WN * load2 = WN_IloadLdid(MTYPE_U1, mloadOffset + 2, 
				  MTYPE_To_TY(MTYPE_U1), preg2, addrN);
	load = WN_Bior(MTYPE_U4, shiftn1, load2);
      }
    }
    /* now save the copied structure */
    if (PLOC_on_stack(ploc)) 
    {
      INT32 offset = PLOC_offset(ploc) - Formal_Save_Area_Size
	+ Stack_Offset_Adjustment_For_PU();
      offset -= (offset % MTYPE_byte_size(MTYPE_U4));
      WN * add = WN_Add(Pointer_type,
			WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
			WN_Intconst(Pointer_type, offset));
      WN * stid = WN_Istore(MTYPE_U4, 0, Make_Pointer_Type(MTYPE_To_TY(MTYPE_U4)),
			    add, load);
      WN_INSERT_BlockLast(block, stid);
    }
    else {
      PREG_NUM regNo = PLOC_reg(ploc);
      ST	*reg = Int_Preg;
      TYPE_ID   type = TY_mtype(Ty_Table[ST_type(reg)]);
      WN * stid = WN_Stid(type, regNo, reg, ST_type(reg), load);
      WN_INSERT_BlockLast(block, stid);
    }
    
    return;
  }
  
  while (PLOC_is_nonempty(ploc))
  {
    if (PLOC_on_stack(ploc))
    {
      PREG_NUM	dstPreg;
     /*
      *  create WN to copy mload to sp+offset
      *
      *  dstPreg = SP + offset - mloadoffset
      *  copy_aggregate() will add mloadoffset later
      */
      {
        WN 	*add;
	INT64	offset;

	offset = PLOC_offset(ploc) - Formal_Save_Area_Size
	  + Stack_Offset_Adjustment_For_PU() - mloadOffset; 

	add = WN_Add(Pointer_type,
		     WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset),
		     WN_Intconst(Pointer_type, offset));
	dstPreg = AssignExpr(block, add, Pointer_type);
      }
      {
	TY_IDX srcTY =	mloadTY;
	TY_IDX dstTY =	MTYPE_To_TY(Max_Uint_Mtype);
	TYPE_ID		quantum;
	WN		*con;
	INT32		copy_alignment, nMoves;
	INT32		todo= size - mloadOffset;

       /*
	* the dest is guaranteed to be Max_Uint aligned
	*/
	copy_alignment= compute_copy_alignment(srcTY, dstTY, 0);
	quantum =	compute_copy_quantum(copy_alignment);

	/*
	 *  we cannot (now) generate an intrinsics as we are in the middle
	 *  of setting call registers!!
         */
	nMoves= calculateLoadStore(todo, mloadOffset, quantum, WN_kid0(mload));

	if (MinStructCopyLoopSize && MinStructCopyLoopSize <= nMoves)
	{
	  con = WN_Intconst(Integer_type, todo);

	  copy_aggregate_loop(block,
			      srcTY,
			      dstTY,
			      mloadOffset,
			      con,
			      quantum,
			      preg,
			      addrN,
			      dstPreg,
			      mload,
			      NULL,
			      copy_alignment,
			      actions);
	  WN_Delete(con);
	}
	else
	{
	  copy_aggregate(block,
			 srcTY,
			 dstTY,
			 mloadOffset,
			 todo,
			 quantum,
			 preg,
			 addrN,
			 dstPreg,
			 mload,
			 NULL,
			 copy_alignment,
			 actions);
	}
      }
      return;
    }
    else
    {
     /*
      *  copy structure element to register
      */
      PREG_NUM regNo = PLOC_reg(ploc);
      ST	*reg = Preg_Is_Dedicated_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID   type = TY_mtype(Ty_Table[ST_type(reg)]);

      {
	WN      *load, *stid;
	INT32	todo = size - mloadOffset;

	if (PLOC_size(ploc) < MTYPE_size_reg(type)
		&& type == MTYPE_F8 && PLOC_size(ploc) == 4)
	{
		// need to copy a smaller size than default reg-size
		DevWarn("actual_mload: switch from mtype_f8 to mtype_f4");
		type = MTYPE_F4;
		reg = MTYPE_To_PREG(type);
	}

       /*
	*  special case remainder of structs 
	*  we will try not to run off the end of the structure (as bad)
	*/
	if (todo < MTYPE_alignment(type))
	{
	  TYPE_ID	quantum;
	  INT32		newAlign, shiftn;

	  Is_True(Preg_Is_Dedicated_Integer(regNo),
		  ("mload actual->reg(size/alignment problem"));
	  newAlign = nearest_power_of_two(todo);

	  quantum = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));

	  load = WN_IloadLdid(quantum, mloadOffset,
			      struct_memop_type (quantum, mloadTY), preg,
			      addrN); 

	  lower_copy_maps(mload, load, actions);

	  if (Target_Byte_Sex == BIG_ENDIAN) {
	    shiftn = MTYPE_size_reg(type) - MTYPE_size_reg(quantum);
  
	    load = WN_Shl(type, load, WN_Intconst(type, shiftn));
	  }
	}
	else
	{
	  load = WN_IloadLdid(type, mloadOffset,
			      struct_memop_type (type, mloadTY), preg,
			      addrN); 

	  lower_copy_maps(mload, load, actions);
	}

	stid= WN_Stid(type, regNo, reg, ST_type(reg), load);
	WN_copy_linenum(mload, stid);

	WN_INSERT_BlockLast(block, stid);
      }
    }
    mloadOffset += PLOC_size(ploc);
    ploc = Get_Struct_Output_Parameter_Location(ploc);
  }
}


/* ====================================================================
 *
 * static void lower_mload_formal
 *
 * Perform lowering (see WN_Lower description) for structure formals
 * registers will be copied to [ stack ]
 *
 * ==================================================================== */

static void lower_mload_formal(WN *block, WN *mload, PLOC ploc,
			       LOWER_ACTIONS actions)
{
  INT32   size, offset = 0; 
  ST     *sym = WN_st(mload);
  TY_IDX  symTY = ST_type(sym);

  Setup_Struct_Input_Parameter_Locations(symTY);
  ploc = Get_Struct_Input_Parameter_Location(ploc);
  size = TY_size(Ty_Table[symTY]);

  /* more special casing for small structs */
  if (size < MTYPE_byte_size(MTYPE_U4) &&
      Target_Byte_Sex == BIG_ENDIAN)
  {
    WN * store;
    if (!PLOC_on_stack(ploc)) {
      PREG_NUM  regNo = PLOC_reg(ploc);
      ST	*reg = Int_Preg;
      TYPE_ID type = TY_mtype(ST_type(reg));
      INT32 shift = (MTYPE_byte_size(MTYPE_U4) - size) * 8;
      WN * load = WN_LdidPreg(type, regNo);
      WN * shiftn = WN_Shl(type, load, WN_Intconst(type, shift));
      store = WN_Stid (type, 0, sym, struct_memop_type (type, symTY),
		       shiftn);   
      WN_INSERT_BlockLast(block, store);
    }
    lower_copy_maps(mload, store, actions);

    return;
  }


  while (PLOC_is_nonempty(ploc))
  {
    if (PLOC_on_stack(ploc))
    {
     /*
      *  structure is already on the stack, nothing to do
      */
      return;
    }
    else
    {
     /*
      *	copy register to stack
      */
      PREG_NUM 	regNo =	PLOC_reg(ploc);
      ST	*reg = Preg_Is_Dedicated_Float(regNo) ? Float_Preg : Int_Preg;
      TYPE_ID   type = TY_mtype(ST_type(reg));
      INT32	todo = size - offset;
      {
	WN        *ldid, *store;

	if (PLOC_size(ploc) < MTYPE_size_reg(type)
		&& type == MTYPE_F8 && PLOC_size(ploc) == 4)
	{
		// need to copy a smaller size than default reg-size
		DevWarn("formal_mload: switch from mtype_f8 to mtype_f4");
		type = MTYPE_F4;
		reg = MTYPE_To_PREG(type);
	}

	ldid = WN_LdidPreg(type, regNo);
       /*
	*  special case remainder of struct
	*  to use type closer to size of remaining todo
	*/
	if (todo < MTYPE_alignment(type))
	{
	  TYPE_ID	quantum;
          INT32         newAlign, shiftn;

          Is_True(Preg_Is_Dedicated_Integer(regNo),
		  ("mload actual->reg(size/alignment problem"));
          newAlign = nearest_power_of_two(todo);

          quantum = Mtype_AlignmentClass(newAlign, MTYPE_type_class(type));

          shiftn = MTYPE_size_reg(type) - MTYPE_size_reg(quantum);

	  if (Target_Byte_Sex == BIG_ENDIAN) {
	   /*
	    *  fix (kludge) for kernel bug pv 369702
	    *  Since U4 and I4 are both sign extended we could use
	    *  an arithmetic shift
	    */
	    if (MTYPE_alignment(quantum) == 4)
	    {
	      ldid= WN_Ashr(type, ldid, WN_Intconst(type, shiftn));
	    }
	    else
	    {
	      ldid= WN_Lshr(type, ldid, WN_Intconst(type, shiftn));
	    }
	  }

	  store = WN_Stid (quantum, offset, sym,
			   struct_memop_type (quantum, symTY) , ldid);
        }
	else
	{
	  store = WN_Stid (type, offset, sym, struct_memop_type (type, symTY),
			   ldid);   
	}
	lower_copy_maps(mload, store, actions);
        WN_copy_linenum(mload, store);

        WN_INSERT_BlockLast(block, store);
      }
      offset += PLOC_size(ploc);
    }
    ploc = Get_Struct_Input_Parameter_Location(ploc);
  }
}




/* ====================================================================
 *
 *  void lower_complex_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
 *				WN **realpart, WN **imagpart)
 *
 * Perform lowering (see WN_Lower description) on an expression
 * or intrinsic op, returning lowered tree.  
 *
 * ==================================================================== */
static void lower_complex_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
				   WN **realpart, WN **imagpart)
{
  WN		*wn;
  BOOL          intrinsic_lowered;

  Is_True(MTYPE_is_complex(WN_rtype(tree)),
	 ("expected complex type in lower_complex_emulation"));

 /*
  *  there is no way to turn off intrinsics consistently
  *  ie. the best we could do is 
  *		z = c4intrinsic(...)
  *  and then we would try to lower that !!
  */

  wn = lower_emulation(block, tree, actions, intrinsic_lowered);

  // Don't do this. It causes the optimizer to miss a lot of CSE's. 
  // R. Shapiro 10/5/98
  if (!intrinsic_lowered) {
       actions |= LOWER_INTRINSIC;
   }
  lower_complex_expr(block, wn, actions, realpart, imagpart);
}


/* ====================================================================
 *
 * WN *lower_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
 *                     BOOL & intrinsic_lowered)
 *
 * Perform lowering (see WN_Lower description) on an expression
 * or intrinsic op, returning lowered tree.  
 * 
 * intrinsic_lowered is set to TRUE if lowereing occurred on an intrinsic
 *
 * Should be called only if actions contains LOWER_INTRINSIC.
 *
 * ==================================================================== */
static WN *lower_emulation(WN *block, WN *tree, LOWER_ACTIONS actions,
			   BOOL & intrinsic_lowered)
{
  WN	*wn, *emBlock;
  OPCODE op = WN_opcode(tree);
  TYPE_ID rtype = WN_rtype(tree);
  TYPE_ID desc = WN_desc(tree);
  BOOL is_tie_longlong_cvt = FALSE;

  if (OPCODE_operator(op)==OPR_CVT &&
      (MTYPE_is_tie(rtype) && (desc==MTYPE_I8 || desc==MTYPE_U8)) ||
      (MTYPE_is_tie(desc) && (rtype==MTYPE_I8 || rtype==MTYPE_U8)))
    is_tie_longlong_cvt = TRUE;

  intrinsic_lowered = FALSE;

  if (OPCODE_is_intrinsic(WN_opcode(tree)) &&
      INTRN_is_tie_intrinsic(WN_intrinsic(tree)))
    return tree;

 /*
  *  swap the kids if the second kid results in emulation call
  *  therefore the results can be directly fed to the current call
  *  see PR15475
  */

  OPERATOR opr = WN_operator(tree);
  if ((opr==OPR_ADD || opr==OPR_MPY) &&
      INTR_intrinsic_name(tree)!=NULL) {
    if (INTR_intrinsic_name(WN_kid0(tree))==NULL &&
	INTR_intrinsic_name(WN_kid1(tree))!=NULL) {
      WN* tmp = WN_actual(tree,0);
      WN_actual(tree,0) = WN_actual(tree,1);
      WN_actual(tree,1) = tmp;
    } else if (WN_operator(WN_kid1(tree))==OPR_LDID &&
	       ST_class(WN_st(WN_kid1(tree)))==CLASS_PREG) {

      /* if the second kid is a return value from a call
	 immediately before the current tree then swap the kid
	 to increse the chance that the return value be fed
	 directly into the call to the emulation routine
      */
      WN* kid1 = WN_kid1(tree);
      ST* st = WN_st(kid1);
      WN* prev = WN_last(block);
      if (prev && WN_operator(prev)==OPR_STID &&
	  ST_class(WN_st(prev))==CLASS_PREG &&
	  WN_offset(prev)==WN_offset(kid1) &&
	  WN_operator(WN_kid0(prev)) == OPR_LDID &&
	  ST_class(WN_st(WN_kid0(prev))) == CLASS_PREG &&
	  (WN_offset(WN_kid0(prev))==
	   Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Integer()) ||
	   WN_offset(WN_kid0(prev))+1==
	   Dedicated_Preg_First_Incoming_Ret(TI_ISA_Regclass_Integer()))) {

	WN* tmp = WN_actual(tree,0);
	WN_actual(tree,0) = WN_actual(tree,1);
	WN_actual(tree,1) = tmp;
      }
    }
  }


 /*
  *  try experiment with lowering children first
  *  (remember these are most likely complex expressions)
  */
  {
    INT32	i;

    for (i = 0; i < WN_kid_count(tree); i++)
      WN_actual(tree,i) = lower_expr(block, WN_actual(tree,i), actions);
  }

 /*
  *  Create a callblock and try to emulate call
  */
  wn = NULL;
  emBlock = NULL;

  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    Is_True((INTRN_is_nary(WN_intrinsic(tree))==FALSE),("nary slipped by"));
    if (Action(LOWER_INLINE_INTRINSIC) || Action(LOWER_INL_STACK_INTRINSIC))
    {
      wn = emulate(emBlock , tree);
    }
    if (wn == NULL && NotAction(LOWER_INTRINSIC))
    {
      if (emBlock)
        WN_Delete(emBlock);
       return tree;
    }
    intrinsic_lowered = TRUE;
  }
 /*
  *  most likely quad emulation
  */
  else
  {
    wn = emulate(emBlock , tree);
  }


  if (wn)
  {
    if (is_tie_longlong_cvt==FALSE)
      emBlock = lower_block(emBlock, actions);
    WN_copy_linenum(tree, emBlock);
    WN_INSERT_BlockLast(block, emBlock);

    /*
     *  the emulation may contain nodes that may need to be lowered
     */
    if (OPCODE_is_stmt(WN_opcode(wn)))
    {
      wn = lower_stmt(block, wn, actions);
    }
    else if (OPCODE_is_expression(WN_opcode(wn)))
    {
      wn = lower_expr(block, wn, actions);
    }
  }
  else
  {
    if (emBlock)
      WN_Delete(emBlock);

    if (OPCODE_is_call(op))
    {
      wn = lower_intrinsic(block, tree, actions);
    }
    else
    {
      wn = lower_intrinsic_op(block, tree, actions);
      if (!wn) wn = tree;
    }
  }
  return wn;
}




/* ====================================================================
 *
 * WN *lower_intrinsic_op(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on intrinsic statement <tree>,
 * returning lowered tree.  Should be called only if actions contains
 * LOWER_INTRINSIC.
 *
 * Since we are calling a runtime routine from a non-statement level
 * we need to
 *    [1]	insert call (lower_intrinsic)
 *    [2]	assign return value to preg temp
 *    [3]	return preg temp
 *
 * The return preg value may require special processing after
 * (see below for examples)
 * ==================================================================== */
static WN *lower_intrinsic_op(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  OPCODE	op = WN_opcode(tree);
  TYPE_ID	type = WN_rtype(tree);
  WN		*wn;

  Is_True(Action(LOWER_INTRINSIC),
	  ("actions does not contain LOWER_INTRINSIC"));

 /*
  *  To evaluate the intrinsic call we need to save the return type 
  *  and create a temporary to hold the return value
  */
  wn = lower_intrinsic(block, tree, actions);

  if (!wn) return NULL;

  if (OPCODE_is_call(WN_opcode(wn))) {
    WN_copy_linenum(tree, wn);
    WN_INSERT_BlockLast(block, wn);
  }

  {
    TYPE_ID	ty1, ty2;
    PREG_NUM	reg1, reg2;

    rename_preg(INTR_intrinsic_name(tree), ".return");

    if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (MTYPE_To_TY(type),
                                                 Complex_Not_Simulated,
						 Return_Info_Incoming);

      if (RETURN_INFO_count(return_info) <= 2) {

        ty1  = RETURN_INFO_mtype (return_info, 0);
        ty2  = RETURN_INFO_mtype (return_info, 1);
        reg1 = RETURN_INFO_preg (return_info, 0);
        reg2 = RETURN_INFO_preg (return_info, 1);
      }

      else
        Fail_FmtAssertion("lower_intrinsic_op: more than 2 return registers");
	/*NOTREACHED*/
    }

    else
      Get_Return_Mtypes(MTYPE_To_TY(type), Complex_Not_Simulated, &ty1, &ty2);

    switch(type)
    {
    case MTYPE_F8:
      {
	Is_True(reg1, ("expected F8 return value from intrinsic_op"));

	//
	// This code needs lowering in the context of the use.
	// So we return a MTYPE_F8.
	//
	PREG_NUM loreg = AssignExpr(block, WN_LdidPreg(type, reg1), type);
	wn = WN_LdidPreg(type, loreg);
      }
      break;

    case MTYPE_C4:
    case MTYPE_C8:
      {
	PREG_NUM	rzN, izN;
	TYPE_ID		real = Mtype_complex_to_real(type);

	if (!WHIRL_Return_Info_On) 
	  Get_Return_Pregs ( ty1, ty2, &reg1, &reg2 );
	Is_True((reg1 && reg2),
		("expected complex return value from intrinsic_op"));

	rzN = AssignExpr(block, WN_LdidPreg(real, reg1), real);
	izN = AssignExpr(block, WN_LdidPreg(real, reg2), real);

	wn = WN_Complex(type,
			WN_LdidPreg(real, rzN),
			WN_LdidPreg(real, izN));
      }
      break;

    case MTYPE_CQ:
     /*
      *  call is RSTYLE_VIA_FIRST_ARG
      *
      *  hopefully our intrinsic lowerer has inserted the address of the
      *  temporary area.
      */
      {
	TYPE_ID   real = Mtype_complex_to_real(type);
	TY_IDX    realTY = MTYPE_To_TY(real);
	WN       *temp,*temp1, *actual = WN_kid0(wn);

	if (WN_operator_is(actual, OPR_PARM))
	  actual = WN_kid0(actual);

	temp = lower_copy_tree( actual, actions);
	temp1 = lower_copy_tree( actual, actions);

	Is_True((OPCODE_is_call(WN_opcode(wn))), ("expected call opcode"));

	wn = WN_Complex(type,
			WN_Iload(real, 0, realTY, temp),
			WN_Iload(real, MTYPE_RegisterSize(real),
				 realTY, temp1));
      }
      break;

    default:
      {
	PREG_NUM	retReg;

	if (!WHIRL_Return_Info_On) 
	  Get_Return_Pregs ( ty1, ty2, &reg1, &reg2 );

	Is_True((reg1 != 0), ("expected return value from intrinsic_op"));
	Is_True((reg2 == 0), ("cannot evaluate 2 regs into an expression"));

	retReg = AssignExpr(block, 
			    WN_LdidPreg(ty1, reg1),
			    type);

	wn = WN_LdidPreg(type, retReg);
      }
      break;
    }
    rename_reset();
  }

  if (OPCODE_is_intrinsic(op))
  {
   /*
    *  These require special processing as the function return
    *  value must be interpreted (i think)
    *
    *	    s_cmp(a, b, len_a, len_b)
    *		a <  b		negative
    *		a == 0		0
    *		a >  0		positive
    */
    switch(WN_intrinsic(tree))
    {
    case INTRN_CEQEXPR:
      wn =	WN_EQ(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    case INTRN_CNEEXPR:
      wn =	WN_NE(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    case INTRN_CGEEXPR:
      wn =	WN_GE(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    case INTRN_CGTEXPR:
      wn =	WN_GT(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    case INTRN_CLEEXPR:
      wn =	WN_LE(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    case INTRN_CLTEXPR:
      wn =	WN_LT(Boolean_type, wn, WN_Zerocon(Boolean_type));
      break;
    }
  }
#ifdef TARG_XTENSA
  else
    {
      /* With the gnu math library we have to muck with the return value
	 for some of the functions. This will be unnecessary if/when we
	 switch to the sgi math library. */
      wn = coerce_emulation_return(tree, wn);
    }
#endif

 /*
  *  The preg may need map processing
  */
  return lower_expr(block, wn, actions);
}




/* ====================================================================
 *
 * WN *lower_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on an expression or,
 * intrinsic returning lowered tree.  
 *
 * Should be called only if actions contains LOWER_INTRINSIC.
 *
 * ==================================================================== */
static WN *lower_intrinsic(WN *block, WN *tree, LOWER_ACTIONS actions)
{

  Is_True(Action(LOWER_INTRINSIC),
	  ("actions does not contain LOWER_INTRINSIC"));

  if (OPCODE_is_intrinsic(WN_opcode(tree)))
  {
    INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);

    if (INTRN_is_actual(id))
    {
     /*
      *  weird fortran anachronism, we need to create an ST entry
      * 	and pass an LDA of the address
      */
      TYPE_ID	rtype = WN_rtype(tree);
      WN	*lda;
      {
	TY_IDX  ty = Make_Function_Type(MTYPE_To_TY(rtype));
	ST     *st = Gen_Intrinsic_Function(ty, INTRN_rt_name(id));
	lda = WN_Lda(Pointer_type, 0, st);
      }

      WN_Delete(tree);
      return lower_expr(block, lda, actions);
    }
  }
  {

   char  *function = INTR_intrinsic_name(tree);
   if (function == NULL) return NULL;

   /*
    *  Create a callblock and generate call.
    *  The arguments may be expanded to match the runtime routine.
    */
    WN	*call, *callblock;

    callblock = WN_CreateBlock();

    call = intrinsic_runtime(callblock, tree);

    if (!call) {
      WN_Delete(callblock);
      return NULL;
    }

    callblock = lower_block(callblock, actions);
    WN_copy_linenum(tree, callblock);
    WN_INSERT_BlockLast(block, callblock);

    if (Action(LOWER_TO_CG))
	actions |= LOWER_CALL;
    call = lower_call(block, call, actions);

    return call;
  }
}


/* ====================================================================
 *
 * WN *lower_actual(WN *block, WN *actual, TYPE_ID parmType, INT32 reg)
 *
 * Auxilary function to lower_call to put and actual in a register
 * This was motivated by -DEBUG:Varargs_Prototypes=off, where for
 * unprototyped calls, all floating point registers need to go in the
 * integer registers as well
 *
 * ==================================================================== */
static WN *lower_actual(WN *block, WN *actual, TYPE_ID parmType, INT32 reg)
{
#ifndef TARG_XTENSA
  /* I think the following doesn't work on machines in which the ABI
     specifically passes floating pointer values in int registers.
  */
 /*
  * float parm goes in int register, so convert
  */
  if (MTYPE_float(parmType) && Preg_Is_Dedicated_Integer(reg))
  {
    TYPE_ID type = TY_mtype(ST_type(Int_Preg));

    actual = WN_Tas(type, ST_type(Int_Preg), actual);
  }
#endif  
  {
    WN	*stid;
    ST	*regST;

//     if (Preg_Is_Dedicated_Integer(reg)) 
// 	regST = Int_Preg;
//     else
//     	// keep float size in preg
// 	regST=	MTYPE_To_PREG(parmType);

    // Preserve original type so instruction selector can
    // generate code that spans several return registers.
    regST = MTYPE_To_PREG(parmType);

    TYPE_ID type = TY_mtype(ST_type(regST));

    stid = WN_StidIntoPreg (type, reg, regST, actual);
    WN_Set_Linenum(stid, current_srcpos);
    WN_INSERT_BlockLast(block, stid);
  }

  return actual;
}

/* ====================================================================
 *  For each function generate a call to a profiler function
 *	__prof( &func, &call, funcname, callname, state);
 *  where
 *	state==0	before call
 *     state==1	after call
 *
 * ==================================================================== */

static WN *lower_profile_call(WN *block, WN *tree, INT32 state,
			      LOWER_ACTIONS actions)
{
  if (Gen_Profile && WN_has_sym(current_function))
  {
    WN		*profcall;
    char	*name;

    {
      TY_IDX  ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
      ST     *st = Gen_Intrinsic_Function(ty, Gen_Profile_Name);

      Set_PU_no_side_effects(Pu_Table[ST_pu(st)]);
      Set_PU_is_pure(Pu_Table[ST_pu(st)]);

      profcall = WN_Call(MTYPE_V, MTYPE_V, 5, st);
    }

    ST *current_fcn_st = WN_st(current_function);

    WN_actual(profcall, 0) = WN_Lda(Pointer_type, 0, current_fcn_st);
    name = ST_name(current_fcn_st);
    WN_actual(profcall, 2) = WN_LdaString(name, 0, strlen(name)+1);
    WN_actual(profcall, 4) = WN_Intconst(MTYPE_I4, state);

    if (WN_has_sym(tree))
    {
      ST *st = WN_st(tree);

      WN_actual(profcall, 1) = WN_Lda(Pointer_type, 0, st);
      name = ST_name(st);
      WN_actual(profcall, 3) = WN_LdaString(name, 0, strlen(name)+1);
    }
    else
    {
      Is_True((WN_operator_is(tree, OPR_ICALL)),
	      ("expected icall node, not %s", OPCODE_name(WN_opcode(tree))));

      WN_actual(profcall, 1)
	= lower_copy_tree(WN_actual(tree, WN_num_actuals(tree)), actions);
      WN_actual(profcall, 3) = WN_Zerocon(Pointer_type);
    }

    Gen_Profile = FALSE;
    profcall = lower_call(block, profcall, actions);
    Gen_Profile = TRUE;

    return profcall;
  }
  return NULL;
}

//=============================================================================
// On machines without relational operators (i.e. Xtensa), we need to evaluate
// relational operators before loading dedicated parameter registers.  If we
// don't then expansion of the relational operators may introduce a branch
// which splits the live range of the paramter register, a split forbidden
// by the register allocator.  So, the following routine lifts the relational
// operators out of the parameter lists and evaluates them into pregs.
//=============================================================================
static WN *lift_relational_expr( WN *block, WN *tree, LOWER_ACTIONS actions)
{
#ifdef TARG_XTENSA

  if( !Action(LOWER_CALL) )
    return tree;

  OPCODE opcode;
  OPERATOR opr;
  int num_opnds;

  opcode = WN_opcode(tree);
  opr = WN_operator(tree);
  switch( opr )
    {
    case OPR_CVT:
      // implicit float <-> double conversion turns into a call
      if (opcode != OPC_F4F8CVT && opcode != OPC_F8F4CVT)
        break;
    case OPR_LT:
    case OPR_LE:
    case OPR_EQ:
    case OPR_NE:
    case OPR_GE:
    case OPR_GT:
    case OPR_SELECT:
    case OPR_CSELECT:
    case OPR_MIN:
    case OPR_MAX:
    case OPR_ALLOCA:
      {
	PREG_NUM expr_reg;
	TYPE_ID ty = WN_rtype(tree);

	expr_reg = AssignExpr( block, tree, ty );
	tree = WN_LdidPreg( ty, expr_reg );
      }
      break;

    default:
      num_opnds = OPCODE_nkids( opcode );
      for( int i = 0; i < num_opnds; i++ ) {
	WN_kid(tree,i) = lift_relational_expr( block, WN_kid(tree,i), actions );
      }
    }

#endif
  return tree;
}

/* ====================================================================
 *
 * WN *lower_call(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on call statement <tree>,
 * returning lowered tree.  Should be called only if actions contains
 * LOWER_CALL.
 *
 * ==================================================================== */

static WN *lower_call(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT16	       i;
  PLOC	       ploc;
  TY_IDX       call_ty;
  WN	      *callblock;
  SRCPOS       srcpos = WN_Get_Linenum(tree);
  INT	       num_actuals = WN_num_actuals(tree);
  ST          *callee_st = NULL;

  Is_True(OPERATOR_is_call(WN_operator(tree)),
	  ("expected call node, not %s", OPERATOR_name(WN_operator(tree))));

  for (i = 0; i < WN_kid_count(tree); i++) {
    WN *actual = lift_relational_expr(block, WN_actual(tree, i), actions);
    WN_actual(tree, i) = lower_expr(block, actual, actions);
  }

  /* Get the ST of the callee if available */
  if (WN_has_sym(tree)) {
    callee_st = WN_st(tree);
  }

  if (WHIRL_Return_Val_On && Action(LOWER_RETURN_VAL) && 
      WN_rtype(tree) == MTYPE_V &&
      (WN_operator(tree) == OPR_CALL || WN_operator(tree) == OPR_ICALL ||
       WN_operator(tree) == OPR_PICCALL)) {
    // if it is a call returning struct coerced to void, may need to introduce
    // a fake first parameter
    TY_IDX prototype;
    if (WN_operator(tree) == OPR_ICALL) 
      prototype = WN_ty(tree);
    else {
      ST_IDX func_stidx = WN_st_idx(tree);
      PU_IDX puidx = ST_pu(St_Table[func_stidx]);
      prototype = PU_prototype(Pu_Table[puidx]);
    }
    RETURN_INFO return_info = Get_Return_Info(TY_ret_type(prototype),
                                              Complex_Not_Simulated,
					      Return_Info_Incoming);
    if (RETURN_INFO_return_via_first_arg(return_info)) {
      ST *return_st = Gen_Temp_Symbol(TY_ret_type(prototype), ".vcall");
      WN *awn = WN_CreateLda(OPR_LDA, Pointer_Mtype, MTYPE_V, 0,
                             Make_Pointer_Type(TY_ret_type(prototype)), 
			     return_st);
      awn = lower_expr(block, awn, actions);
      WN *n_call = add_fake_parm(tree, awn, WN_ty(awn), actions);
      WN_Delete(tree);
      tree = n_call;
    }
  }

  if (Action(LOWER_PICCALL))
  {
   /* convert a CALL to PICCALL for the following cases:
    *
    *  1. we are generating PIC code.
    *  2. -TARG:force_indirect_call
    *  3. We need to setup $t9.
    *  4. We are generating CPIC code and the callee is visible outside the
    *     dso.
    */
    if (WN_operator_is(tree, OPR_CALL)  &&
	(Force_Indirect_Call                     ||
	 PU_needs_t9(Pu_Table[ST_pu(callee_st)]) ||
	 (Gen_PIC_Calls &&
	  (Gen_PIC_Shared                       ||
           (Gen_PIC_Call_Shared &&
            ST_visible_outside_dso(*callee_st)) )))) 
    {
      INT16       n = WN_kid_count(tree);
      WN          *itree;
      /*
       *  convert (CALL sym args ...) into (PICCALL args ... (LDA sym)). 
       *  The LDA does not have an OP_PARM
       */
      itree = WN_Piccall( WN_rtype(tree), WN_desc(tree), n+1, callee_st);
  
      Is_True(callee_st == WN_st(tree),
	      ("lower_call: something changed that Robert didn't expect!"));
      WN_actual(itree, n) = WN_Lda(Pointer_type, 0, callee_st);
      WN_Set_Linenum(itree, srcpos);
      WN_Set_Flags(tree, itree);
  
      while (--n >= 0)
        WN_actual(itree, n) = WN_actual(tree, n);

      if (Cur_PU_Feedback) {
	Cur_PU_Feedback->FB_lower_call( tree, itree );
      }
      WN_Delete(tree);
      tree = itree;
    }
  }
  
  if (NotAction(LOWER_CALL))
    return tree;

 /*
  *  For each function generate a call to a profiler function
  *	__prof( &func, &call, funcname, callname);
  */
  if (Gen_Profile)
  {
    WN  *profcall = lower_profile_call(block, tree, 0, actions);
    WN_copy_linenum(tree, profcall);
    WN_INSERT_BlockLast (block, profcall);
  }

  callblock = WN_CreateBlock();

  /*
   * TODO:
   * (a) Just before each call passing a struct by value, if (part of) the
   *     struct can be passed in register(s), convert MLOAD into multiple
   *     ILOADs.  MEDIUM PRIORITY (at least detect condition)
   * (b) Just before each call, pass static link to nested calls.
   * (d) For -o32 -shared, just after returning from a call, restore $gp
   *     (but see bug 159929).
   */

  /*
   * Create block containing stores of actual loads into pregs.
   * The call can continue to have the original actuals, because 
   * we don't use that except to see the number of parameters. 
   *
   * New:  Go through the list twice to give localize an easier job
   *	   First handle structures (there may be loop code to move the
   *       structure)
   */
  call_ty = (WN_operator_is(tree,OPR_ICALL) ? WN_ty(tree) :
	     ST_pu_type(callee_st));
  Is_True(WN_operator_is(tree, OPR_ICALL) ||
	  callee_st == WN_st(tree),
	  ("lower_call: something changed that Robert didn't expect!"));
  ploc = Setup_Output_Parameter_Locations(call_ty);

  for (i = 0; i < num_actuals; i++)
  {
    WN		*parm = WN_actual(tree, i);
    TYPE_ID	parmType = WN_rtype(parm);
    WN		*actual = WN_operator_is(parm, OPR_PARM) ? WN_kid0(parm)
                                                         : parm;

    if (MTYPE_is_m(parmType))
    {
     /*
      * structure parameter
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      lower_mload_actual (callblock, actual, ploc, actions);
    }
    else if (MTYPE_is_complex(parmType) && PU_ftn_lang(Get_Current_PU()))
    {
     /*
      * Fortran by value can generate this construct
      * we must make it look like a structure parameter
      * Note that GCC has complex type, which should go to
      * normal processing (like quads).
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      lower_complex_actual (callblock, actual, ploc, actions);
    }
    else
    {
      ploc = Get_Output_Parameter_Location( MTYPE_To_TY(parmType));
    }
  }

  ploc = Setup_Output_Parameter_Locations(call_ty);
  for (i = 0; i < num_actuals; i++)
  {
   /*
    * We need to get the TY of the parameter: 
    * if no prototype, then we don't have a list of formals;
    * the actual may need to be converted if there is a prototype;
    * the rtype of the opcode is the converted type, so use it. 
    */
    WN         *parm = WN_actual(tree, i);
    TYPE_ID     parmType = WN_rtype(parm);
    WN         *actual = WN_operator_is(parm, OPR_PARM) ? WN_kid0(parm) : parm;
    TY_IDX      ty;

    if (MTYPE_is_m(parmType) 
	|| (MTYPE_is_complex(parmType) && PU_ftn_lang(Get_Current_PU())) )
    {
     /*
      * already processed
      */
      ploc = Get_Output_Parameter_Location( TY_Of_Parameter(parm));
      continue;
    }

    ty =  TY_Of_Parameter(parm);
    ploc = Get_Output_Parameter_Location( MTYPE_To_TY(parmType));
    {
     /*
      *  canonicalize [I,U][1,2] types (to [I,U]4
      */
      TYPE_ID  type = Mtype_comparison( Fix_TY_mtype(ty));

      if (parmType != type)
      {
	DevWarn("lower_call(): line %d, parm #%d type mismatch (WN_rtype(parm)"
		" = %s) (cannonical TY_mtype(parm))) %s)",
		Srcpos_To_Line(WN_Get_Linenum(tree)), i,
		Mtype_Name(parmType), Mtype_Name(type));
      }
    }

    if (PLOC_on_stack(ploc))
    {
     /*
      * stack offset 
      * We can use the preg for $sp here, but a simpler way
      * is to use the SP symbol. 
      */
      WN *wn;

      wn = WN_Stid(parmType, PLOC_offset(ploc) - Formal_Save_Area_Size
		             + Stack_Offset_Adjustment_For_PU(),
		   SP_Sym, ty, actual);
      WN_Set_Linenum(wn, srcpos);
      WN_INSERT_BlockLast(callblock, wn);
    }
   /*
    * special case (as quad -> integer doesn't make much sense).
    * Store argument into temp preg float parm goes in int register, so convert
    */
    else if (MTYPE_is_quad(parmType) && Preg_Is_Dedicated_Integer(PLOC_reg(ploc)))
    {
      PREG_NUM qN;

      qN = AssignExpr(callblock, actual, parmType);

      lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN),   MTYPE_F8,
		   PLOC_reg(ploc));

      lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN+1), MTYPE_F8,
		   PLOC_reg(ploc)+1);
    }
    else
    {
      actual = lower_actual(callblock, actual, parmType, PLOC_reg(ploc));

      if (WN_operator_is(parm, OPR_PARM))
	WN_kid0(parm) = actual;
      else
	WN_actual(tree, i) = actual;

      if (! TY_has_prototype(call_ty)        &&
	  DEBUG_Varargs_Prototypes == FALSE  &&
	  Preg_Is_Dedicated_Float(PLOC_reg(ploc)))
      {
	actual = lower_copy_tree(actual, actions);

	if (MTYPE_is_quad(parmType))
	{
	  PREG_NUM  qN;

	  qN = AssignExpr(callblock, actual, parmType);

	  lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN),   MTYPE_F8,
		       PLOC_vararg_reg(ploc));
	  lower_actual(callblock, WN_LdidPreg(MTYPE_F8, qN+1), MTYPE_F8,
		       PLOC_vararg_reg(ploc)+1);
	}
	else
	{
	  lower_actual(callblock, actual, parmType, PLOC_vararg_reg(ploc));
	}
      }
    }
  }

 /*
  * store param size in tree for later re-use 
  * unless has no prototype (then could have varying # args). 
  */
  if (num_actuals > 0 &&
      TY_has_prototype(call_ty) &&
      Get_PU_arg_area_size(call_ty) == 0 &&
      ! TY_is_varargs(call_ty))
  {
      Set_PU_arg_area_size(call_ty, PLOC_total_size(ploc));
  }


 /*
  *  copy the arguments to avoid building a dag
  *  (region code might delete these nodes)
  *  CG will use these arguments to build register usage information
  */
  {
    INT16       n= num_actuals;

    while (--n >= 0)
      WN_actual(tree, n) = lower_copy_tree( WN_actual(tree, n), actions);
  }

  if (MTYPE_is_complex(WN_rtype(tree)))
  {
    INT16       n = WN_kid_count(tree);
    WN          *itree;
    TYPE_ID	rtype, desc;
    
    rtype = Mtype_complex_to_real(WN_rtype(tree));
    desc = Mtype_complex_to_real(WN_desc(tree));

   /*
    *  For complex the call type will be changed by WN_Call()
    *  into (PICCALL args ... (LDA sym)).
    */
    if (WN_operator_is(tree, OPR_CALL))
    {
      itree = WN_Call( rtype, desc, n, WN_st(tree));
    }
    else if (WN_operator_is(tree, OPR_ICALL))
    {
      itree = WN_Icall( rtype, desc, n, WN_ty(tree));
    }
    WN_Set_Linenum(itree, srcpos);
    WN_Set_Flags(tree, itree);

    while (--n >= 0)
      WN_actual(itree, n) = WN_actual(tree, n);

    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_call( tree, itree );
    }
    WN_Delete(tree);
    tree = itree;
  }

  /*
   *  F90 needs this 
   *  The callee will require the current FP or Slink in $2
   */
  if (Action(LOWER_UPLEVEL))
  {
    if (callee_st && PU_is_nested_func(Pu_Table[ST_pu(callee_st)]))
    {
      WN  *wn, *link;
     
     /*
      *  These are "downlevel" calls for F90 (ex mp procedures),
      *  or uplevel (child calls parent)
      *  This should only happen for F90.
      */
      if (PU_lexical_level(&St_Table[WN_st_idx(current_function)]) <
	  PU_lexical_level(callee_st))
      {
	link = WN_LdidPreg(Pointer_type, Frame_Pointer_Preg_Offset);
      }

     /*
      *  Need to set up the register for "sideways" (internal calling internal)
      *  and uplevel calls. This should only happen for F90.
      */
      else 
      {
	ST  *slink = Find_Slink_For_Scope(WN_st(current_function),
					  callee_st);

	link = WN_Ldid (Pointer_type, 0, slink, ST_type(slink));
      }
#ifdef TARG_XTENSA
      if (Pass_Static_Link_On_Stack) {
	WN *sp = WN_LdidPreg(Pointer_type, Stack_Pointer_Preg_Offset);
	TY_IDX addr_ty = Make_Pointer_Type(MTYPE_To_TY(Pointer_type),TRUE);
	wn = WN_Istore(Pointer_type, Static_Link_Stack_Offset, addr_ty, sp, link);
      } else
#endif
	wn = WN_StidIntoPreg(Pointer_type,
			     Static_Link_Preg_Offset,
			     MTYPE_To_PREG(Pointer_type), link);
	  
      WN_Set_Linenum(wn, srcpos);
      WN_INSERT_BlockLast(callblock, wn);
     }
  }

  /* 
   * Check that stack space needed for actuals doesn't overflow 
   * what we initially reserved. 
   */
  Check_Actual_Stack_Size (tree);

  callblock = lower_block(callblock, actions);
  WN_copy_linenum(tree, callblock);
  WN_INSERT_BlockLast (block, callblock);

  /******************************************************
	STUPID: cannot generate call here as it will affect the return value
	for this function
  if (Gen_Profile)
  {
    WN_INSERT_BlockLast (block, tree);

    return lower_profile_call(block, tree, 1, actions);
  }
  ******************************************************/

  return tree;
}




/* ====================================================================
 *
 * WN *lower_compgoto(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on switch val in COMPGOTO
 * node <tree>, returning XGOTO.
 *
 * If there is a default label, code is generated to test the range
 * of switch values
 * ==================================================================== */

static WN *lower_compgoto(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT32		pointerSize = MTYPE_RegisterSize(Pointer_type);
  INT32		num_entries = WN_num_entries(tree);
  WN		*index, *add;
  ST		*table_st;
  LEAF		indexN;

  Is_True(WN_opcode(tree) == OPC_COMPGOTO,
	  ("expected COMPGOTO node, not %s", OPCODE_name(WN_opcode(tree))));
  Is_True(WN_num_entries(tree) > 0,
	  ("expected COMPGOTO nu,_entries > 0 not %d", WN_num_entries(tree)));

  WN_kid0(tree) = lower_expr(block,
			     WN_Coerce(Pointer_type, WN_kid0(tree)),
			     actions);

  if (NotAction(LOWER_COMPGOTO))
    return tree;

  index = WN_kid0(tree);

  indexN = Make_Leaf(block, WN_Coerce(Pointer_type, index), Pointer_type);

 /*
  *  for default cases we must verify the index fall in range
  */
  WN *wn_truebr = NULL;
  if (WN_kid_count(tree) == 3)
  {
   /*
    *  branch to default if (index >= n)
    *  because the indexN is unsigned, we do not have to compare 
    *  greater then zero
    */
    WN  *ge, *truebr;
    WN	*def  = WN_kid2(tree);

    ge =  WN_GE(Pointer_type,
		Load_Leaf(indexN),
		WN_Intconst(Pointer_type, num_entries));

    truebr = lower_truebr(WN_label_number(def), ge, &wn_truebr, actions);
    WN_copy_linenum(tree, truebr);
    WN_INSERT_BlockLast(block, truebr);
  }

 /*
  * create fake, incomplete array type so table_st has correct size
  *
  *  generate jump for computed goto
  *    agoto ( & table + (i * sizeof(Pointer_type)));
  *
  *  create
  *    table + (i * sizeof(Pointer_type));
  */
  {
    TY_IDX table;
    WN	*mpy;

    table = Make_Array_Type(Pointer_type, 1, num_entries);

    if (xt_switchjump) {
	// Cisco jump table, the st is just a placeholder to remember
	// the labels in the switch table
	// The XGOTO cotains the address of the jump location rather than a 
	// load of the jump address
	table_st = New_ST(CURRENT_SYMTAB);
        ST_Init (table_st, Save_Str(".Ljump_table"), CLASS_VAR,
           SCLASS_EXTERN, EXPORT_LOCAL, table);
        Set_ST_is_switchjump(table_st);
    } else {
      table_st = Gen_Read_Only_Symbol (table, ".Ljump_table");
      Set_ST_is_initialized(table_st);	/* so goes in rdata section */
   
      if (ST_has_named_ro_section(Get_Current_PU_ST())) {
        STR_IDX ro_sec_name_idx = Find_RO_Section_Name_For_ST(Get_Current_PU_ST());
        char * ro_sec_name = Index_To_Str(ro_sec_name_idx);
  
        ST_ATTR_IDX idx;
        ST_ATTR & attr = New_ST_ATTR(CURRENT_SYMTAB, idx);
        ST_ATTR_Init(attr, ST_st_idx(table_st), ST_ATTR_SECTION_NAME, 
				     Save_Str(ro_sec_name));
        Set_ST_has_named_section(table_st);
      }
    }
    
    if (xt_switchjump && OPT_Space) {
      mpy = WN_Mpy(Pointer_type, 
		 Load_Leaf(indexN),
		 WN_Intconst(Pointer_type, 3));
    } else {
      mpy = WN_Mpy(Pointer_type, 
		 Load_Leaf(indexN),
		 WN_Intconst(Pointer_type, pointerSize));
    }

    add = WN_Add(Pointer_type,
		 WN_Lda (Pointer_type, 0, table_st),
		 mpy);
  }
  
 /*
  * create fake, incomplete array type so table_st has correct size
  *
  *  generate jump for computed goto
  *    agoto ( & table + (i * sizeof(Pointer_type)));
  */
  {
    WN  *xgoto;
    WN	*gotoTable = WN_kid1(tree);

    if (xt_switchjump) {
      index = add;
    } else {
      index = WN_Iload(Pointer_type,
		     0,
		     MTYPE_To_TY(Pointer_type),
		     add);
    }

#ifdef TARG_XTENSA
    if (!xt_switchjump) {
      index = WN_Add(Pointer_type, index, 
		     WN_Lda(Pointer_type, 0, Get_Current_PU_ST()));
    }
#endif

    index = lower_expr(block, index, actions);

    xgoto = WN_CreateXgoto(num_entries, index, gotoTable, table_st);
    WN_Set_Linenum (xgoto, WN_Get_Linenum(tree));

    // Lower feedback info
    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_compgoto( tree, xgoto, wn_truebr );

    return xgoto;
  }
}




/* ====================================================================
 *
 * WN *lower_assert(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on OPC_ASSERT
 * node <tree>, returning structured if
 *
 * ==================================================================== */

static WN *lower_assert(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN  *if_else, *if_then, *IF, *trap, *cond;

  Is_True(WN_opcode(tree) == OPC_ASSERT,
	  ("expected ASSERT node, not %s", OPCODE_name(WN_opcode(tree))));

  WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);

  if (NotAction(LOWER_ASSERT))
    return tree;

  if_then = WN_CreateBlock();
  if_else = WN_CreateBlock();
  cond = lower_expr(block, WN_kid0(tree), actions);

 /*
  * generate intrinsic call 
  */
  switch ( WN_offset(tree) ) {
    case WN_TRAP_F77_BOUNDS_ERROR:
      {
	char *name;
	WN **kids = (WN **) alloca(4 * sizeof(WN *));

	/* add blank at end of string cause s_rnge wants one */
	kids[0] = WN_LdaString ( "? ", 0, 2 );
	kids[1] = WN_Intconst ( MTYPE_I4, -1 );	/* cause of s_rnge weirdness */

	/* s_rnge expects name with _ at end */
	name = ST_name(&St_Table[PU_Info_proc_sym(Current_PU_Info)]);
	kids[2] = WN_LdaString ( name, 0, strlen(name)+1 );

	kids[3] = WN_Intconst(MTYPE_I4, Srcpos_To_Line(WN_Get_Linenum(tree)));
	trap = WN_Create_Intrinsic ( OPC_VINTRINSIC_CALL,
				     INTRN_F77_BOUNDS_ERR, 4, kids );
      } 
      break;

    case WN_TRAP_C_BOUNDS_ERROR:
      /* Check DEBUG_Verbose_Runtime.  If set, call __C_runtime_error;
       * otherwise emit a BRK_RANGE trap.
       */
      /* TODO PV 443095:  This doesn't have descriptive parameters due
       * to limitations on the ASSERT node.  Fix that eventually.
       */
      if ( DEBUG_Verbose_Runtime ) {
	char *name;
	WN **kids = (WN **) alloca(4 * sizeof(WN *));

	/* __C_runtime_error ( BRK_RANGE, PU_name, line_no, fmt, ...);
	 */
#if defined(__linux__) || defined(__sun__) || defined(_WIN32)
	/* to do: is there a way to do a break range on solaris? */
	fprintf(stderr, "Don't know how to do BRK_RANGE\n");
	exit(-1);
#else
	trap = WN_CreateTrap( BRK_RANGE );
#endif

	/* Source routine where error occurs: */
	name = ST_name(&St_Table[PU_Info_proc_sym(Current_PU_Info)]);
	kids[1] = WN_LdaString ( name, 0, strlen(name)+1 );

	/* Source line where error occurs: */
	kids[2] = WN_Intconst ( MTYPE_I4,
				Srcpos_To_Line(WN_Get_Linenum(tree)) );

	/* Eventually, this should be a printf format and arguments,
	 * describing the case that fails (i.e. array and index).
	 * For now, the ASSERT node doesn't have the right kids.
	 */
	kids[3] = WN_LdaString ( "unknown array", 0, 14 );

	/* Make the call: */
	trap = WN_Create_Intrinsic ( OPC_VINTRINSIC_CALL,
				     INTRN_RT_ERR, 4, kids );
      } else {
#if defined(__linux__) || defined(__sun__) || defined(_WIN32)
	/* to do: can we do a break range on sun? */
	fprintf(stderr, "Don't know how to do BRK_RANGE\n");
	exit(-1);
#else   
	trap = WN_CreateTrap ( BRK_RANGE );
#endif  

	// ADD FEEDBACK INFO ??
      }
      break;

    default:
      trap = WN_CreateTrap ( WN_offset(tree) );
      // ADD FEEDBACK INFO ??
      break;
  }

  WN_Set_Linenum(trap, WN_Get_Linenum(tree));
  WN_INSERT_BlockLast(if_else, trap);

  IF = WN_CreateIf(cond, if_then, if_else);
  // ADD FEEDBACK INFO !!
  WN_Set_Linenum(IF, WN_Get_Linenum(tree));
  WN_Delete(tree);

  IF = lower_scf(block, IF, actions);
  return IF;
}


static WN *lower_branch(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True((WN_opcode(tree) == OPC_TRUEBR || WN_opcode(tree) == OPC_FALSEBR),
	  ("expected true/false"));

  WN * const kid = lower_expr(block, WN_kid0(tree), actions);
  WN_kid0(tree) = kid;

  if (WN_operator(kid) == OPR_LNOT)
  {
    switch(WN_opcode(tree))
    {
    case OPC_TRUEBR:
      WN_set_opcode(tree, OPC_FALSEBR);
      break;

    case OPC_FALSEBR:
      WN_set_opcode(tree, OPC_TRUEBR);
      break;

    default:
      break;
    }

    WN_kid0(tree) = WN_kid0(kid);
    WN_Delete(kid);
  }

  return tree;
}
 
 
/* ====================================================================
 *
 * WN *lower_return_val(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on RETURN_VAL nodes
 * turning kid0 expression into an appropriate store and the RETURN_VAL
 * into a RETURN.
 *
 * ==================================================================== */

static WN *lower_return_val(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  PREG_NUM    preg;
  ST         *preg_st;
  TYPE_ID     mtype;
  SRCPOS      srcpos = WN_Get_Linenum(tree);
  RETURN_INFO return_info;
  WN         *wn;

  Is_True((WN_operator(tree) == OPR_RETURN_VAL),
	  ("expected RETURN_VAL node, not %s", OPCODE_name(WN_opcode(tree))));

  TYPE_ID return_mtype = WN_rtype(tree);

  if (return_mtype != MTYPE_M) {
    TY_IDX return_ty_idx = Be_Type_Tbl(return_mtype);
    return_info = Get_Return_Info (return_ty_idx, Use_Simulated);

    WN_kid0(tree) = lower_expr (block, WN_kid0(tree), actions);
    if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
      // This happens, for example, if a complex result is returned for ia32.
      // Create an istore; an mstore with a single complex value seemed
      // to confuse things later on.
      WN *first_formal = WN_formal(current_function, 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      OPCODE const_op = OPCODE_make_op(OPR_INTCONST, MTYPE_I8, MTYPE_V);
      // WN *swn = WN_CreateIntconst (const_op, TY_size(return_ty_idx));
      WN *awn = WN_CreateLdid(OPR_LDID, 
			      TY_mtype(Ty_Table[tidx]), 
			      TY_mtype(Ty_Table[tidx]),
			      WN_idname_offset(first_formal), 
			      WN_st(first_formal), 
			      tidx);
      // wn  = WN_CreateMstore (0, tidx, WN_kid0(tree), awn, swn);
      wn = WN_CreateIstore(OPR_ISTORE, MTYPE_V,
			   return_mtype, 0, tidx, WN_kid0(tree), awn);
    } else {  // Returned in preg
      Is_True(RETURN_INFO_count(return_info) == 1,
	      ("expected return_info count to be 1"));

      mtype   = RETURN_INFO_mtype (return_info, 0);
      preg    = RETURN_INFO_preg (return_info, 0);
#ifdef TARG_XTENSA
      // Don't store the result of alloca into a dedicated register
      if (WN_operator(WN_kid0(tree)) == OPR_ALLOCA ||
          (WN_operator(WN_kid0(tree)) == OPR_CVT &&
           WN_operator(WN_kid0(WN_kid0(tree))) == OPR_ALLOCA)) {
        PREG_NUM tmp_preg = AssignExpr (block, WN_kid0(tree), mtype);
        WN_kid0(tree) = WN_LdidPreg(mtype, tmp_preg);
      }
#endif      
      if (MTYPE_is_tie(mtype) || MTYPE_is_xtbool(mtype)) {
        preg_st = MTYPE_To_PREG(mtype); 
        wn  = WN_CreateStid (OPR_STID, MTYPE_V, mtype, 
	  		   preg, preg_st, Be_Type_Tbl(mtype), WN_kid0(tree));
      } else {
        preg_st = (MTYPE_is_float(mtype) ? Float_Preg :
                  ((mtype==MTYPE_I8 || mtype==MTYPE_U8) ? MTYPE_To_PREG(mtype): 
                  Int_Preg));
        wn  = WN_CreateStid (OPR_STID, MTYPE_V, Promoted_Mtype[mtype], 
	  		   preg, preg_st, Be_Type_Tbl(mtype), WN_kid0(tree));
      }

    }
    WN_Set_Linenum(wn, srcpos);
    WN_INSERT_BlockLast (block, wn);
  }
  else { // MTYPE_M; rhs is one of MLDID or MILOAD or MLOAD
    WN *o_rhs = WN_kid0(tree); 
    OPERATOR rhs_opr = WN_operator(o_rhs);
    TY_IDX ty_idx = WN_ty(o_rhs);
    if (OPERATOR_is_load(rhs_opr) && WN_field_id(o_rhs) != 0) {
      if (rhs_opr == OPR_ILOAD || rhs_opr == OPR_LDID) 
        ty_idx = get_field_type(ty_idx, WN_field_id(o_rhs));
      else { // OPR_MLOAD
        ty_idx = TY_pointed(Ty_Table[ty_idx]);
        ty_idx = get_field_type(ty_idx, WN_field_id(o_rhs));
      }
    }
    return_info = Get_Return_Info(ty_idx, Complex_Not_Simulated);

    if (RETURN_INFO_return_via_first_arg(return_info)) { // fake first parm
      Is_True(RETURN_INFO_count(return_info) == 0, 
	      ("expected RETURN_INFO to have 0 count"));
      WN *n_rhs;

      // fix rhs
      if (WN_operator(o_rhs) == OPR_LDID)
        n_rhs = lower_mldid(block, o_rhs, LOWER_MLDID_MSTID);
      else if (WN_operator(o_rhs) == OPR_ILOAD) 
        n_rhs = lower_miload(block, o_rhs, LOWER_MLDID_MSTID);
      else n_rhs = o_rhs; 		// MLOAD

      // create an mstore
      WN *first_formal = WN_formal(current_function, 0);
      TY_IDX tidx = ST_type(WN_st(first_formal));
      WN *awn = WN_CreateLdid(OPR_LDID, 
			      TY_mtype(Ty_Table[tidx]), 
			      TY_mtype(Ty_Table[tidx]),
			      WN_idname_offset(first_formal), 
			      WN_st(first_formal), 
			      tidx);
      WN *swn = WN_CopyNode(WN_kid1(n_rhs));
      wn  = WN_CreateMstore (0, tidx, n_rhs, awn, swn);

      WN_Set_Linenum(wn, srcpos);
      WN_INSERT_BlockLast (block, wn);
    }
    else { // return in return registers
      INT32 i;
      WN *n_rhs;
      UINT algn;
      TY_IDX ty_idx_used;

      if (WN_operator(o_rhs) == OPR_LDID) {
	Is_True(WN_rtype(o_rhs) == MTYPE_M,
		("expected RETURN_VAL kid not type M"));

	algn = TY_align(ty_idx);
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
	  mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  n_rhs = WN_CreateLdid (OPR_LDID, mtype, mtype, 
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 WN_st_idx(o_rhs), ty_idx_used);
	  n_rhs = adjust_outgoing_return_location(n_rhs, ty_idx);
	  preg    = RETURN_INFO_preg (return_info, i);
	  preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_Set_Linenum(wn, srcpos);
	  WN_INSERT_BlockLast (block, wn);
	}
      }
      else if (WN_operator(o_rhs) == OPR_ILOAD) {
	Is_True(WN_rtype(o_rhs) == MTYPE_M,
		("expected RETURN_VAL kid not type M"));

	algn = TY_align(ty_idx);
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
          mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  if (i == 0)
	    n_rhs = WN_kid0(o_rhs);
	  else n_rhs = WN_COPY_Tree(WN_kid0(o_rhs));
	  n_rhs = WN_CreateIload(OPR_ILOAD, mtype, mtype,
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 ty_idx_used,
				 Make_Pointer_Type(ty_idx_used), n_rhs);
	  n_rhs = lower_expr(block, n_rhs, actions);
	  n_rhs = adjust_outgoing_return_location(n_rhs, ty_idx);
	  preg    = RETURN_INFO_preg (return_info, i);
          preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_Set_Linenum(wn, srcpos);
	  WN_INSERT_BlockLast (block, wn);
	}
      }
      else { // MLOAD
	Is_True(WN_operator(WN_kid1(o_rhs)) == OPR_INTCONST,
		("expected RETURN_VAL's MLOAD kid to be of constant size"));
	algn = TY_align(TY_pointed(WN_load_addr_ty(o_rhs)));
	for (i = 0; i < RETURN_INFO_count(return_info); i++) {
          mtype = RETURN_INFO_mtype(return_info, i);
	  ty_idx_used = Be_Type_Tbl(mtype);
	  Set_TY_align(ty_idx_used, algn);
	  if (i == 0)
	    n_rhs = WN_kid0(o_rhs);
	  else n_rhs = WN_COPY_Tree(WN_kid0(o_rhs));
	  n_rhs = WN_CreateIload(OPR_ILOAD, mtype, mtype,
				 WN_load_offset(o_rhs)
				   + i * MTYPE_byte_size(mtype),
				 ty_idx_used,
				 Make_Pointer_Type(ty_idx_used), n_rhs);
	  n_rhs = lower_expr(block, n_rhs, actions);
	  preg    = RETURN_INFO_preg (return_info, i);
          preg_st = MTYPE_is_float(mtype) ? Float_Preg : Int_Preg;
	  wn = WN_CreateStid(OPR_STID, MTYPE_V, mtype, preg, preg_st, 
			     Be_Type_Tbl(mtype), n_rhs);
	  WN_Set_Linenum(wn, srcpos);
	  WN_INSERT_BlockLast (block, wn);
	}
      }

      WN_Delete (o_rhs);
    }
  }

  WN *wn_return = WN_CreateReturn ();
  WN_Set_Linenum(wn_return, srcpos);
  if ( Cur_PU_Feedback )
    Cur_PU_Feedback->FB_lower_return_val( tree, wn_return );
  tree = wn_return;
  return tree;
}


/* ====================================================================
 *
 * WN *adjust_outgoing_return_location(WN *block, TY_IDX ty)
 * WN *adjust_incoming_return_location(WN *block, TY_IDX ty)
 *
 * for Xtensa big endian targets. Small structs are returned in the
 * least significant bits of the register.
 *
 * ==================================================================== */

static WN *adjust_outgoing_return_location(WN *load, TY_IDX ty)
{
  if (Target_Byte_Sex != BIG_ENDIAN)
    return load;
  else {
    const UINT64 size = TY_size(Ty_Table[ty]);
    if (size < MTYPE_byte_size(MTYPE_U4)) {
      INT shift = MTYPE_bit_size(MTYPE_U4) - (size * 8);
      return WN_Lshr(MTYPE_U4, load, WN_Intconst(MTYPE_U4, shift));
    }
    else 
      return load;
  }
}


static WN *adjust_incoming_return_location(WN *load, TY_IDX ty)
{
  if (Target_Byte_Sex != BIG_ENDIAN)
    return load;
  else {
    const UINT64 size = TY_size(Ty_Table[ty]);
    if (size < MTYPE_byte_size(MTYPE_U4)) {
      INT shift = MTYPE_bit_size(MTYPE_U4) - (size * 8);
      return WN_Shl(MTYPE_U4, load, WN_Intconst(MTYPE_U4, shift));
    }
    else 
      return load;
  }
}


/* ====================================================================
 *
 * WN *lower_stmt(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statement <tree>,
 * returning lowered tree.  Usually a single statement is returned, but
 * in some cases, lowering (e.g. of complex stores) produces a BLOCK of
 * statements.
 *
 * ==================================================================== */

static WN *lower_stmt(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE stmtState = pushCurrentState(tree, actions);

  Is_True(OPCODE_is_stmt(WN_opcode(tree)),
	  ("expected statement node, not %s", OPCODE_name(WN_opcode(tree))));

  switch(WN_operator(tree))
  {
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_PICCALL:
    tree = lower_call(block, tree, actions);
    break;

  case OPR_INTRINSIC_CALL:
    tree = lower_intrinsic_call(block, tree, actions);
    break;
    
  case OPR_ISTORE:
  case OPR_STID:
  case OPR_MSTORE:
    tree = lower_store(block, tree, actions);
    break;

  case OPR_COMPGOTO:
    tree = lower_compgoto(block, tree, actions);
    break;

  case OPR_IO:
    if (Action(LOWER_IO_STATEMENT))
    {
      if (traceIO)
      {
          fputs(DBar, TFile);
          fprintf(TFile, "Io Lower: Before Lowering\n");
          fdump_tree(TFile, tree);
          fputs(DBar, TFile);
      }

      tree = lower_io_statement(tree, actions);

      if (traceIO)
      {
          fputs(DBar, TFile);
          fprintf(TFile, "Io Lower: After Lowering\n");
          fdump_tree(TFile, tree);
          fputs(DBar, TFile);
      }
    }
    break;
  case OPR_ALTENTRY:
    tree = lower_entry(tree, actions);
    break;

  case OPR_EVAL:
    tree = lower_eval(block, tree, actions);
    break;

  case OPR_LABEL:
   /*
    *  lower loop info for CG (it will process these opt>0
    */
    if (WN_label_loop_info(tree))
    {
      WN  *infoblock= WN_CreateBlock();
      WN  *info=      WN_label_loop_info(tree);

      info = lower_expr(infoblock, info, actions);
      WN_set_label_loop_info(tree, info);
    }
    break;

  case OPR_EXC_SCOPE_BEGIN:
   /*
    *  We don't want to lower the kids, which are not used to generate code,
    * but only to provide information to the optimizer. 
    */
    break;

  case OPR_REGION_EXIT:
   /*
    *  convert REGION_EXIT to GOTO
    */
    if (Action(LOWER_REGION_EXITS)) 
      WN_set_opcode(tree, OPC_GOTO);
    break;

  case OPR_ASSERT:
    tree = lower_assert(block, tree, actions);
    break;

#ifdef TARG_XTENSA
  case OPR_TRUEBR: 
    if (Action(LOWER_TOP_LEVEL_ONLY))
      tree = lower_branch(block, tree, actions);
    else {
      WN *old_br = tree;
      WN *new_br = NULL;
      tree = lower_truebr(WN_label_number(tree), WN_kid0(tree), &new_br, actions);
      if (Cur_PU_Feedback && new_br) {
        Cur_PU_Feedback->FB_duplicate_node(old_br, new_br);
	Cur_PU_Feedback->Delete(old_br);
      }
    }
    break;
  case OPR_FALSEBR: 
    Is_True(WN_operator(tree)==OPR_TRUEBR || WN_operator(tree)==OPR_FALSEBR,
	    ("expected OPR_TRUEBR or OPR_FALSEBR"));
    
    if (Action(LOWER_TOP_LEVEL_ONLY))
      tree = lower_branch(block, tree, actions);
    else {
      WN *old_br = tree;
      WN *new_br = NULL;
      tree = lower_falsebr(WN_label_number(tree), WN_kid0(tree), &new_br, actions);
      if (Cur_PU_Feedback && new_br) {
        Cur_PU_Feedback->FB_duplicate_node(old_br, new_br);
	Cur_PU_Feedback->Delete(old_br);
      }
    }
    break;
#else
  case OPR_TRUEBR:
  case OPR_FALSEBR:
    tree = lower_branch(block, tree, actions);
    break;
#endif

  case OPR_RETURN_VAL:
    if (Action(LOWER_RETURN_VAL))
      tree = lower_return_val(block, tree, actions);
    else
      WN_kid0(tree) = lower_expr(block, WN_kid0(tree), actions);
    break;

  case OPR_ASM_STMT:
    {
      // Lower only the input expressions
      INT16 i;
      for (i = 2; i < WN_kid_count(tree); i++) {
	WN_kid0(WN_kid(tree,i)) = lower_expr(block, WN_kid0(WN_kid(tree,i)), 
                                             actions | LOWER_MLDID_IN_ASM);
      }
      break;
    }

  default: 
    {
      INT16 i;
      for (i = 0; i < WN_kid_count(tree); i++)
  	WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
    }
  }

  popCurrentState(stmtState);
  return tree;
}

/* ====================================================================
 *
 * WN *replace_ldidPreg(WN *expr, PREG_NUM reg, WN *replacement)
 * 
 *  walk expr looking for an ldid of preg reg.
 *  if found (expect only one of them), replace the ldid with replacement
 *
 *  if replacement is NULL we are doing a dryrun to test if there is a value
 *
 * ==================================================================== */

static WN *replace_ldidPreg(WN *expr, PREG_NUM reg, WN *replacement)
{
  INT16 i;

  if (expr==NULL)
    return NULL;

  for (i = 0; i < WN_kid_count(expr); i++)
  {
    WN	*child= WN_kid(expr, i);

    if ((WN_operator_is(child, OPR_LDID))	&&
	(WN_class(child) == CLASS_PREG)		&&
	(WN_load_offset(child) == reg))
    {
      if (replacement)
      {
	WN_kid(expr, i) = replacement;
      }
      return child;
    }

    {
      WN  *tree;
      if (tree = replace_ldidPreg(child, reg, replacement))
	return tree;
    }
  }
  return NULL;
}

/* ====================================================================
 *
 * 
 * WN *lower_intrinsic_call(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * intrinsic calls have the special problem of defining a return
 * register that is used in the next stmt
 *
 * It is required that the emulation routine either
 *  [1]		passes the corresponding call node back
 *  	or
 *  [2]		returns a value (typically ldid preg)
 *		this value must replace the return register which
 *		should be in the next whirl statement (fchow says so)
 *
 * ==================================================================== */
static WN *lower_intrinsic_call(WN *block, WN *tree, LOWER_ACTIONS actions)
{ 
  WN		*next, *em, *newBlock;
  INTRINSIC	id = (INTRINSIC) WN_intrinsic(tree);
  SRCPOS	srcpos = WN_Get_Linenum(tree);
  TYPE_ID	type;
  PREG_NUM	reg1;
  UINT		reg_cnt = 0;
  BOOL		intrinsic_lowered;

  Is_True(OPCODE_is_stmt(WN_opcode(tree)),
	  ("expected statement node, not %s", OPCODE_name(WN_opcode(tree))));
  {
   /*
    *  first lower the children
    */
    INT16 i;
    for (i = 0; i < WN_kid_count(tree); i++)
      WN_kid(tree,i) = lower_expr(block, WN_kid(tree,i), actions);
  }

  if (INTRN_is_tie_intrinsic(id)) {
    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(id);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
    if (tie_macro->is_simple_addr_load_store())
      lower_map(tree, actions);
  }

  if (INTRN_is_tie_intrinsic(id) && Action(LOWER_TIE_FOR_CG)) {

    TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(id);
    TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
    int out_count = 0;

    if (tie_macro->no_output())
      return tree;

    WN* store = WN_next(tree);
    if (tie_macro->is_conditional_branch()) // skip the return value
      store = WN_next(store);

    for (int i=0; i<tie_macro->num_protos(); i++) {

      if (tie_macro->proto_is_in(i)==FALSE) {

	/* an out or inout argument needs to replace the input expression
	   with a preg
	*/

	out_count++;
	BOOL found=FALSE;
	BOOL has_cvtl=FALSE;
	TYPE_ID proto_mtype = tie_macro->proto_mtype_id(tie_info,i);
	WN* return_ldid = NULL;

	do {

	  has_cvtl=FALSE;
	  if (store==NULL)
	    ErrMsgSrcpos(EC_TIE_missing_store,srcpos,out_count,tie_macro->name());

	  if ((WN_operator(store)==OPR_STID ||
	       WN_operator(store)==OPR_ISTORE)) {

	    return_ldid = WN_kid0(store);
	    if (WN_operator(return_ldid)==OPR_CVTL) {
	      /* go through CVTLs */
	      has_cvtl=TRUE;
	      do {
		return_ldid = WN_kid0(return_ldid);
	      } while (return_ldid && WN_operator(return_ldid)==OPR_CVTL);
	    }
	    if (return_ldid && WN_operator(return_ldid)==OPR_LDID &&
		WN_st(return_ldid)== Tie_Output_Volatile_Preg &&
		Preg_Is_Dedicated(WN_offset(return_ldid)) &&
		WN_offset(return_ldid) == -1 * (out_count)) {
	      found=TRUE;
	    }
	  }
	  if (found==FALSE)
	    store = WN_next(store);
	} while (found==FALSE);

	TYPE_ID store_desc = WN_desc(store);
	if (MTYPE_is_tie(store_desc) || MTYPE_is_tie(proto_mtype))
	  FmtAssert(WN_desc(store)==proto_mtype,
		  ("Internal error: implicit type conversion for store after TIE intrinsic call"));

	WN* param_ldid = WN_kid0(WN_kid(tree,i));
	PREG_NUM arg_preg;
	if (has_cvtl==FALSE				&&
	    tie_macro->proto_is_inout(i)		&&
	    WN_has_sym(param_ldid) && WN_has_sym(store) &&
	    WN_st(param_ldid)==WN_st(store)		&&
	     ((WN_class(store) == CLASS_PREG) ||
	      (WN_operator(store)==OPR_STID		&&
	       WN_operator(param_ldid)==OPR_LDID	&&
	       ST_assigned_to_dedicated_preg(WN_st(store)))) &&
	     WN_has_offset(param_ldid) && WN_has_offset(store) &&
	     WN_offset(param_ldid)==WN_offset(store))
	    /* for inout parameter, if the copy-in symbol is the same
	       as the copy-out, we use that symbol and avoid
	       creating a temp preg as well as the copying
	    */
	  if (ST_assigned_to_dedicated_preg(WN_st(store))) {
	    arg_preg = Find_PREG_For_Symbol(WN_st(store));
	  } else {
	    arg_preg = WN_offset(store);

	} else if (has_cvtl==FALSE			&&
	    tie_macro->proto_is_out(i)			&&
	    WN_operator(store)==OPR_STID		&&
	    WN_has_sym(store)				&&
	    ((WN_st(store)==MTYPE_To_PREG(proto_mtype)	&&
	      WN_class(store) == CLASS_PREG) ||
	     (ST_assigned_to_dedicated_preg(WN_st(store)))))
	    {
	    /* for out parameter, if the copy-out is an stid to
	       preg, we use that symbol and avoid
	       creating a temp preg as well as the copying
	    */
	  if (ST_assigned_to_dedicated_preg(WN_st(store))) {
	    arg_preg = Find_PREG_For_Symbol(WN_st(store));
	  } else {
	    arg_preg = WN_offset(store);
	  }
	  WN_kid0(WN_kid(tree,i))=WN_LdidPreg(proto_mtype, arg_preg);

	} else {
	  arg_preg = Create_Preg(proto_mtype, "TIE_intr_arg");
	  if (tie_macro->proto_is_inout(i)) {
	    WN* param_stid =
	      WN_StidIntoPreg(proto_mtype, arg_preg, MTYPE_To_PREG(proto_mtype),
			    param_ldid);

	    param_stid = lower_stmt(block, param_stid, actions);
	    WN_INSERT_BlockLast(block, param_stid);

	  }
	  WN_kid0(WN_kid(tree,i))=WN_LdidPreg(proto_mtype, arg_preg);
	}

	if (has_cvtl) {
	  WN* cvtl = WN_kid0(store);
	  while (WN_kid0(cvtl)!=return_ldid)
	    cvtl = WN_kid0(cvtl);
	  WN_kid0(cvtl)=WN_LdidPreg(proto_mtype, arg_preg);
	} else 
	  WN_kid0(store)=WN_LdidPreg(proto_mtype, arg_preg);
	WN_DELETE_Tree(return_ldid);
	store = WN_next(store);
      }

    }

    return tree;
  }

  if (INTRN_cg_intrinsic(id))
    return tree;

  if (NotAction(LOWER_INTRINSIC |
		LOWER_INLINE_INTRINSIC |
		LOWER_INL_STACK_INTRINSIC)) {
    return tree;
  }
  else if (NotAction(LOWER_INL_STACK_INTRINSIC)) {
    INTRINSIC intr = (INTRINSIC) WN_intrinsic(tree);
    if (intr == INTRN_U4I4SETSTACKPOINTER ||
	intr == INTRN_U8I8SETSTACKPOINTER ||
	intr == INTRN_U4READSTACKPOINTER ||
	intr == INTRN_U8READSTACKPOINTER ||
	intr == INTRN_U4I4ALLOCA ||
	intr == INTRN_U8I8ALLOCA) {
      return tree;
    }
  }

  next = WN_next(tree);
  type = WN_rtype(tree);
  newBlock = WN_CreateBlock();

 /* see if any of the statement return values exists in the next
    statement but don't replace it */
  {
    BOOL returnValueUnused = MTYPE_is_void(type);

    if (!returnValueUnused)
      {
	RETURN_INFO return_info = Get_Return_Info (MTYPE_To_TY(type),
						   Complex_Not_Simulated,
						   Return_Info_Incoming);

	returnValueUnused = TRUE;
	reg_cnt = RETURN_INFO_count(return_info);
	for (UINT i = 0; i < reg_cnt; i++)
	  {
	    if (i == 0)
	      reg1 = RETURN_INFO_preg(return_info, i);
	    
	    if (replace_ldidPreg(next, RETURN_INFO_preg(return_info, i), NULL) != NULL)
	      {
		returnValueUnused = FALSE;
		break;
	      }
	  }
      }

    if (returnValueUnused && INTRN_has_no_side_effects(id))
      {
	DevWarn("lower_intrinsic_call(): function %s is void or unused and has"
		" no_side_effects. It will be removed", get_intrinsic_name(id));

	return newBlock;
      }
  }

  em = lower_emulation(newBlock, tree, actions, intrinsic_lowered);

 /*
  *  Nothing happened (ie. the intrinsic was not an INLINE
  *  and we should go no further
  */
  if ((Action(LOWER_INLINE_INTRINSIC) ||
       Action(LOWER_INL_STACK_INTRINSIC)) && 
      NotAction(LOWER_INTRINSIC)	  &&
      tree == em)
  {
    WN_Delete(newBlock);
    return tree;
  }

  WN_Delete(tree);

  if (OPCODE_is_call(WN_opcode(em)))
  {
    WN_Set_Linenum (em, srcpos);
    WN_INSERT_BlockLast(block, newBlock);
    return em;
  }
  else if (WN_is_block(em))
  {
    WN_INSERT_BlockLast(block, newBlock);
    return em;
  }
  else if (MTYPE_is_void(type))
  {
    if (OPCODE_is_stmt(WN_opcode(em)))
    {
      WN_INSERT_BlockLast(newBlock, em);
    }
    return newBlock;
  }
  else
  {
    /*
     *	so far we only expand __builtin_alloca() and the memset routines.
     *  This code will need to change based on different return sequences
     */
    Is_True(reg_cnt == 1, ("expected 1 return value from intrinsic_call"));
    Is_True((OPCODE_is_expression(WN_opcode(em))), ("expected expression"));
    {
      WN	*replaced = NULL;

      /*
       *  In the next statement replace an ldid of preg reg1 with em
       *  The use of the function may have been deleted, but the function
       *  still may have side effects!
       *
       *  We are guaranteed the result is in the "next" statement BTW
       */
      replaced = replace_ldidPreg(next, reg1, em);

      if (replaced)
      { /* CVTL-RELATED */
        Is_True(Types_are_compatible(WN_rtype(replaced),type),
		("return reg mismatch type"));
      }
      return newBlock;
    }
  }
}




/* ====================================================================
 *
 * WN *lower_block(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in BLOCK
 * node <tree>, returning (possibly) modified BLOCK.
 *
 * ==================================================================== */

WN *lower_block(WN *tree, LOWER_ACTIONS actions)
{
  WN *out, *node, *next_node;
  CURRENT_STATE blockState;

  Is_True(WN_opcode(tree) == OPC_BLOCK,
	  ("expected BLOCK node, not %s", OPCODE_name(WN_opcode(tree))));

  blockState = pushCurrentState(tree, actions);

  out = WN_CreateBlock();
  WN_Set_Linenum (out, current_srcpos);

  for (node = WN_first(tree); node; node = next_node)
  {

    /*	MP pragmas are handled in a very special manner.  There are three forms
	of parallel constructs - standalone parallel directive, parallel loop
	(doacross) and parallel region.
	A standalone parallel directive starts with an MP pragma node and is
	followed by (depending on the directive) a series of statements ended
	with another, specific MP pragma node.
	Parallel loops start with an MP pragma node followed by a series of MP
	pragmas and assignment statements ended with a DO_LOOP node.
	A parallel region is an OPC_REGION created by the FE which encapsulates
	all statements within the region.  It will always have a func_pragma
	list containing MP pragmas.  To all but the MP lowerer, this region is
	transparent.
	If MP lowering is in effect, then this entire set of nodes needs to be
	removed from the statement chain, translated and replaced with one or
	more statement/scf nodes.  Because a set of nodes must be processed
	as a unit, the usual lowering scheme of passing in a single node (tree)
	and returning a single node (tree) won't work.  */

    while (Action(LOWER_MP) && node &&
      ((((WN_opcode(node) == OPC_PRAGMA) || (WN_opcode(node) == OPC_XPRAGMA))
	&& (WN_pragmas[WN_pragma(node)].users & PUSER_MP)) ||
       ((WN_opcode(node) == OPC_REGION) && WN_first(WN_region_pragmas(node)) &&
        ((WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_PRAGMA) ||
	 (WN_opcode(WN_first(WN_region_pragmas(node))) == OPC_XPRAGMA)) &&
        (WN_pragmas[WN_pragma(WN_first(WN_region_pragmas(node)))].users &
	 PUSER_MP))))
	node = lower_mp(out, node, actions);
    if (node == NULL) break;

    /*
     *  Must read WN_next(node) now since it may be overwritten
     */

    next_node = WN_next(node);

    setCurrentState(node, actions);

    if (OPCODE_is_stmt(WN_opcode(node)))
    {
	node = lower_stmt(out, node, actions);
    }
    else if (OPCODE_is_scf(WN_opcode(node)))
    {
	node = lower_scf(out, node, actions);
    }
    else
    {
	Fail_FmtAssertion("expected statement or SCF node, not %s",
			  OPCODE_name(WN_opcode(tree)));
	/*NOTREACHED*/
    }

    if (node == NULL) // lowering an MSTID of Return_Val_Preg can cause NULL
      continue;

    WN_INSERT_BlockLast(out, node);
  
    // if STID(ALLOCA) and trapuv, insert a mstore
    if ( Action(LOWER_INL_STACK_INTRINSIC) && DEBUG_Trap_Uv
        && WN_operator(node) == OPR_STID
        && WN_operator(WN_kid0(node)) == OPR_ALLOCA)
    {
        lower_trapuv_alloca (out, WN_kid0(node), actions);
    }
  }

 /*
  *  Once we have the integrated block, remove any stmt after
  *  a return or goto, until we hit a label or ALTENTRY
  *  only if we are not called by lower_scf_non_recursive()
  *
  *  Curiously enough, this is needed to prevent dead blocks (-O0)
  *  which causes exception handling to fail ...
  *  
  */
  if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    for (node = WN_first(out); node; node = next_node)
    {
      next_node = WN_next(node);
  
      if (WN_unconditional_goto(node))
      {
        for(node = next_node; node; node = next_node)
        {
	  next_node = WN_next(node);
  
	  if (WN_operator_is(node, OPR_LABEL))
	    break;
	  else if (WN_operator_is(node, OPR_ALTENTRY))
	    break;
	  else if (WN_operator_is(node, OPR_EXC_SCOPE_BEGIN))
	    continue;
	  else if (WN_operator_is(node, OPR_EXC_SCOPE_END))
	    continue;
	  else if (WN_operator_is(node, OPR_PRAGMA))
	    continue;
	  else if (WN_operator_is(node, OPR_XPRAGMA))
	    continue;
	  else if (WN_operator_is(node, OPR_REGION_EXIT))
	    continue;
	  if (OPCODE_is_stmt(WN_opcode(node)))
	  {
	    WN_DELETE_FromBlock(out, node);
	  }
	  else
	    break;
        }
      }
    }
  }

  WN_Delete(tree);

  popCurrentState(blockState);
  return out;
}


/* ====================================================================
 *
 * WN *lower_speculate(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * try to speculate CAND/CIOR to LAND/LIOR
 * Edge frequency data is discarded
 *
 * ==================================================================== */

static WN *lower_speculate(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  INT32	n;
  BOOL  speculate = TRUE;

  if (OPT_Lower_Speculate == FALSE)
    return tree;

  switch(WN_operator(tree))
  {
  case OPR_CAND:
  case OPR_CIOR:
  case OPR_CSELECT:
    for (n = 0; n < WN_kid_count(tree); n++)
    {
      WN_kid(tree, n) =	lower_speculate(block, WN_kid(tree, n), actions);
      speculate &=	expr_is_speculative(WN_kid(tree, n));
    }

    if (speculate)
    {
      if (WN_operator_is(tree, OPR_CAND))
      {
	WN *land = WN_LAND( WN_kid0(tree), WN_kid1(tree) );
	WN_Delete(tree);
	return land;
      }
      else if (WN_operator_is(tree, OPR_CIOR))
      {
	WN *lior = WN_LIOR( WN_kid0(tree), WN_kid1(tree) );
	WN_Delete(tree);
	return lior;
      }
      else if (WN_operator_is(tree, OPR_CSELECT))
      {
	WN *select = WN_Select(WN_rtype(tree), WN_kid0(tree), WN_kid1(tree),
			       WN_kid2(tree));
  
	WN_Delete(tree);
	return select;
      }
    }
    break;
  }

  return tree;
}

/* ====================================================================
 *
 * WN *lower_conditional(WN *block, WN *tree, BOOL branchType,
 *                       LOWER_ACTIONS actions)
 *
 * lower CAND/CIOR to scf
 *
 * ==================================================================== */

static WN *
lower_conditional(WN *block, WN *tree, LABEL_IDX trueLabel,
		  LABEL_IDX falseLabel, BOOL branchType,
		  LOWER_ACTIONS actions)
{
  WN *shortcircuit, *left_branch = NULL, *right_branch = NULL;

  switch(WN_operator(tree))
  {
  case OPR_CAND:
   /*
    *	Process the left child.
    *   We need a label for the children to branch to
    */
    if (WN_operator_is(WN_kid0(tree), OPR_CAND)	||
	WN_operator_is(WN_kid0(tree), OPR_CIOR))
    {
      shortcircuit = WN_NewLabel();

      lower_conditional(block, WN_kid0(tree), WN_label_number(shortcircuit),
			falseLabel, FALSE, actions);
      WN_copy_linenum(tree, shortcircuit);
      WN_INSERT_BlockLast(block, shortcircuit);
    }
    else
    {
      left_branch = WN_Falsebr(falseLabel, WN_kid0(tree));
      WN_INSERT_BlockLast(block, left_branch);
    }

   /*
    *	Process the right child.
    */
    if (WN_operator_is(WN_kid1(tree), OPR_CAND) ||
	WN_operator_is(WN_kid1(tree), OPR_CIOR))
    {
      lower_conditional(block, WN_kid1(tree), trueLabel, falseLabel,
			branchType, actions);
    }
    else {
      if ( branchType /*==TRUE*/ ) {
	right_branch = WN_Truebr(trueLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
      else {
	/* branchType == FALSE */
	right_branch = WN_Falsebr(falseLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
    }
    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_circuit( tree, left_branch, right_branch );
    }
    return NULL;

  case OPR_CIOR:
   /*
    *	Process the left child.
    *   We need a label for the children to branch to
    */
    if (WN_operator_is(WN_kid0(tree), OPR_CAND)	||
	WN_operator_is(WN_kid0(tree), OPR_CIOR))
    {
      shortcircuit = WN_NewLabel();

      lower_conditional(block, WN_kid0(tree), trueLabel,
			WN_label_number(shortcircuit), TRUE, actions);
      WN_copy_linenum(tree, shortcircuit);
      WN_INSERT_BlockLast(block, shortcircuit);
    }
    else
    {
      left_branch = WN_Truebr(trueLabel, WN_kid0(tree));
      WN_INSERT_BlockLast(block, left_branch);
    }

   /*
    *	Process the right child.
    */
    if (WN_operator_is(WN_kid1(tree), OPR_CAND) ||
	WN_operator_is(WN_kid1(tree), OPR_CIOR))
    {
      lower_conditional(block, WN_kid1(tree), trueLabel, falseLabel,
			branchType, actions);
    }
    else {
      if ( branchType /*==TRUE*/ ) {
	right_branch = WN_Truebr(trueLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
      else {
	/* branchType == FALSE */
	right_branch = WN_Falsebr(falseLabel, WN_kid1(tree));
	WN_INSERT_BlockLast(block, right_branch);
      }
    }
    if (Cur_PU_Feedback) {
      Cur_PU_Feedback->FB_lower_circuit( tree, left_branch, right_branch );
    }
    return NULL;

  default:
    tree = lower_expr(block, tree, actions);
    break;
  }

  return tree;
}

static WN *lower_branch_condition(BOOL branchType, LABEL_IDX label, WN *cond,
				  WN **branch, LOWER_ACTIONS actions)
{
  WN *condBlock = WN_CreateBlock();

  cond = lower_speculate(condBlock, cond, actions);

  *branch = NULL;

  switch(WN_operator(cond))
  {
  case OPR_CAND:
  case OPR_CIOR:
    if (Action(LOWER_SHORTCIRCUIT)) {
      LABEL_IDX shortcircuit_lbl;
      shortcircuit_lbl = NewLabel();

      if (branchType == TRUE)
      {
	cond = lower_conditional(condBlock, cond, label, shortcircuit_lbl,
				 branchType, actions);
      }
      else
      {
	cond = lower_conditional(condBlock, cond, shortcircuit_lbl, label,
				 branchType, actions);
      }

      condBlock = lower_block(condBlock, actions);
      WN_INSERT_BlockLast(condBlock, WN_Label(shortcircuit_lbl));
      break;
    }
    /* Fall through */
  default:
    {
      WN * old_cond = cond;
      cond = lower_expr(condBlock, cond, actions);

      /* split the long long and do a bit-or before testing it
	 for branching (see pr14089)
      */
      if (Action(LOWER_PAIRED) &&
	  (WN_rtype(cond) == MTYPE_I8) || (WN_rtype(cond) == MTYPE_U8)) {
	WN *hi, *lo;

	lower_paired_expr(condBlock, cond, actions, &hi, &lo);
	cond = WN_Bior(WN_rtype(lo), lo, hi);

      }

      if (branchType)
	*branch = WN_Truebr(label, cond);
      else
	*branch = WN_Falsebr(label, cond);

      WN_INSERT_BlockLast(condBlock, *branch);
    }
  }
  return condBlock;
}

#ifdef SHORTCIRCUIT_HACK
/* return TRUE if the expression <tree> has a CAND/CIOR that cannot
 * be converted into LAND/LIOR respectively.
 */
static BOOL tree_has_cand_cior (WN *tree)
{
  WN_ITER *wni;
  WN *wn;

  for (wni = WN_WALK_TreeIter (tree); 
       wni != NULL;
       wni = WN_WALK_TreeNext (wni))
  {
    wn = WN_ITER_wn (wni);
    /* TODO: check if the CAND/CIOR can be converted to LAND/LIOR */
    if (WN_operator_is(wn, OPR_CAND)	||
	WN_operator_is(wn, OPR_CIOR)	||
	WN_operator_is(wn, OPR_CSELECT))
      return TRUE;
  }
  return FALSE;
}
#endif

//=====================================================================
// WN *lower_cselect(WN *block, WN *tree, LOWER_ACTIONS actions )
//
// Lower a cselect into an if then else.
// 
// This is only called when both sides of a CSELECT are INTCONSTs
// and the condition operator is CAND or CIOR
//=====================================================================
static WN*
lower_cselect(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  // This implements a very simple heuristic:
  // If the condition operator is CAND favor the false side
  // If the condition operator is CIOR favor the true side

  TYPE_ID type = WN_rtype(tree);

  WN *if_then = WN_CreateBlock();
  WN *if_else = WN_CreateBlock();

  WN *cond_block, *cond_value, *uncond_value;

  if (WN_operator(WN_kid0(tree)) == OPR_CAND) {
    cond_block = if_then;
    cond_value = WN_kid1(tree);
    uncond_value = WN_kid2(tree);
  }
  else {
    Is_True(WN_operator(WN_kid0(tree)) == OPR_CIOR,
            ("Unexpected operator %s in lower_cselect", 
             OPERATOR_name(WN_operator(WN_kid0(tree)))));
    cond_block = if_else;
    cond_value = WN_kid2(tree);
    uncond_value = WN_kid1(tree);
  }
  
  // Put the non-favored side under if branch
  PREG_NUM tmpN = AssignPregExprPos(cond_block, cond_value, 
                                    MTYPE_To_TY(type), current_srcpos, 0);

  // First assign favored side unconditionally
  WN *stid = WN_StidIntoPreg(type, tmpN, MTYPE_To_PREG(type), uncond_value);
  WN_INSERT_BlockLast(block, stid);
  
  // Now put it all together and lower it.
  WN *if_tree = WN_CreateIf(WN_kid0(tree), if_then, if_else );

  if (Cur_PU_Feedback)
    Cur_PU_Feedback->FB_lower_branch(tree, if_tree);

  if_tree = lower_if(block, if_tree, actions);

  WN_INSERT_BlockLast(block, if_tree);
  
  return WN_LdidPreg(type, tmpN);
}

//=====================================================================
//
// WN *lower_select(WN *block, WN *tree, LOWER_ACTIONS actions )
//
// Lower a select into an if then else.
//
//=====================================================================

static WN *lower_select(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  if (WN_operator(tree) == OPR_CSELECT &&
      WN_operator(WN_kid1(tree)) == OPR_INTCONST &&
      WN_operator(WN_kid2(tree)) == OPR_INTCONST &&
      (WN_operator(WN_kid0(tree)) == OPR_CAND ||
       WN_operator(WN_kid0(tree)) == OPR_CIOR)) {
    return lower_cselect(block, tree, actions);
  }
  
  TYPE_ID type = WN_rtype(tree);
  PREG_NUM  tmpN;
  WN *if_tree, *if_then, *if_else, *stid;
  
  /* Assign the true side of the select into a temporary.
     But avoid doing any lowering at this time to avoid duplicating
     the work in call to lower_if later.
  */
  if_then = WN_CreateBlock();
  tmpN = AssignPregExprPos(if_then, WN_kid1(tree), MTYPE_To_TY(type),
			   current_srcpos, 0 );
  
  /* For the else clause we just store directly into the already
     allocated temporary instead of using AssignPregExprPos to
     create a temporary.
  */
  if_else = WN_CreateBlock();
  stid = WN_StidIntoPreg(type, tmpN, MTYPE_To_PREG(type), WN_kid2(tree));
  WN_INSERT_BlockLast(if_else, stid);
  
  /* Now put it all together
     and lower it.
  */
  if_tree = WN_CreateIf( WN_kid0(tree), if_then, if_else );

  if (Cur_PU_Feedback) {
    FB_Info_Branch info_branch(FB_FREQ(1.0,FB_FREQ_TYPE_GUESS), FB_FREQ_UNKNOWN); 
    const FB_Info_Branch& info_if = Cur_PU_Feedback->Query_branch(tree);
    if (info_if.freq_taken!=FB_FREQ_UNINIT || info_if.freq_not_taken!=FB_FREQ_UNINIT)
      info_branch = info_if;
    Cur_PU_Feedback->Annot_branch(if_tree, info_branch);
  }

  if_tree = lower_if( block, if_tree, actions );

  WN_INSERT_BlockLast( block, if_tree );
  
  return WN_LdidPreg(type, tmpN);
}

/* ====================================================================
 *
 * WN *lower_if(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in IF
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_if(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_IF,
	  ("expected IF node, not %s", OPCODE_name(WN_opcode(tree))));

  if (WN_Is_If_MpVersion(tree))
    return lower_block(lower_mp(block, tree, actions), actions);

#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_IF))
#else
  if (Action(LOWER_IF) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior(WN_if_test(tree))))
#endif
  {
   /*
    *  Lower IF.  This is done differently depending on
    *  whether the "then" or "else" clauses are empty.
    *
    *  Pay close attention to the block scope and state while creating nodes
    */
    WN *body = WN_CreateBlock();
    WN *wn_branch = NULL;
    LABEL_IDX cont_lbl;
    cont_lbl = NewLabel();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    if (WN_block_nonempty(WN_then(tree)))
    {
      if (WN_block_nonempty(WN_else(tree)))
      {
       /*
        * Both "then" and "else" clauses exist.  Generate:
        * 	(FALSEBR <cond> <else_lbl>)
        *	<then_clause>
        *	(GOTO <cont_lbl>)
        *	(LABEL <else_lbl>)
        *	<else_clause>
        *	(LABEL <cont_lbl>)
        */
	LABEL_IDX else_lbl;
	else_lbl = NewLabel();

	WN *falsebr_block = lower_falsebr(else_lbl, WN_if_test(tree),
					  &wn_branch, actions);
        WN_copy_linenum(tree, falsebr_block);
	WN_INSERT_BlockFirst(body, falsebr_block);

	setCurrentStateBlockLast(WN_then(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_then(tree), actions));
	WN *wn_goto = WN_Goto(cont_lbl);
        WN_copy_linenum(tree, wn_goto);
	WN_INSERT_BlockLast(body, wn_goto);

        if (Cur_PU_Feedback) {
          const FB_Info_Branch& info_if = Cur_PU_Feedback->Query_branch(tree);
          FB_Info_Invoke info_goto(info_if.freq_taken);
          Cur_PU_Feedback->Annot_invoke(wn_goto, info_goto);
        }

	WN *wn_else_lbl = WN_Label(else_lbl);
        WN_copy_linenum(tree, wn_else_lbl);
	WN_INSERT_BlockLast(body, wn_else_lbl);
	setCurrentStateBlockLast(WN_else(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_else(tree), actions));
	WN *wn_cont_lbl = WN_Label(cont_lbl);
        WN_copy_linenum(tree, wn_cont_lbl);
	WN_INSERT_BlockLast(body, wn_cont_lbl);
      } 
      else
      {
       /*
        * Only "then" clause exists.  Generate:
        *	(FALSEBR <cond> <cont_lbl>)
        *	<then_clause>
        *	(LABEL <cont_lbl>)
        */
	WN *falsebr_block = lower_falsebr(cont_lbl, WN_if_test(tree),
					  &wn_branch, actions);
        WN_copy_linenum(tree, falsebr_block);
	WN_INSERT_BlockFirst(body, falsebr_block);

	setCurrentStateBlockLast(WN_then(tree), actions);
	WN_INSERT_BlockLast(body, lower_block(WN_then(tree), actions));
	WN *wn_cont_lbl = WN_Label(cont_lbl);
        WN_copy_linenum(tree, wn_cont_lbl);
	WN_INSERT_BlockLast(body, wn_cont_lbl);
      }

      if (Cur_PU_Feedback)
	Cur_PU_Feedback->FB_lower_branch( tree, wn_branch );

      WN_Delete(tree);
      return body;
    } 
    else if (WN_block_nonempty(WN_else(tree)))
    {
     /*
      * Only "else" clause exists.  Generate:
      *		(TRUEBR <cond> <cont_lbl>)
      *		<else_clause>
      *		(LABEL <cont_lbl>)
      */
      WN *truebr_block = lower_truebr(cont_lbl, WN_if_test(tree),
				      &wn_branch, actions);
      WN_copy_linenum(tree, truebr_block);
      WN_INSERT_BlockFirst(body, truebr_block);

      setCurrentStateBlockLast(WN_else(tree), actions);
      WN_INSERT_BlockLast(body, lower_block(WN_else(tree), actions));
      WN *wn_cont_lbl = WN_Label(cont_lbl);
      WN_copy_linenum(tree, wn_cont_lbl);
      WN_INSERT_BlockLast(body, wn_cont_lbl);

      if (Cur_PU_Feedback)
	Cur_PU_Feedback->FB_lower_branch( tree, wn_branch );

      WN_Delete(tree);
      return body;
    }
    else
    {
     /*
      * Neither "then" or "else" clause exists.  Generate:
      *		(EVAL <cond>)
      */
      WN *eval;

      eval = WN_CreateExp1(OPC_EVAL, lower_expr(block, WN_if_test(tree),
						actions));
      WN_Set_Linenum (eval, current_srcpos);

      WN_DELETE_Tree(WN_then(tree));
      WN_DELETE_Tree(WN_else(tree));
      WN_Delete(tree);
      return eval;
    }
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN_if_test(tree) = lower_expr(block, WN_if_test(tree),
				  RemoveShortCircuitAction(actions));
    WN_then(tree) = lower_block(WN_then(tree), actions);
    WN_else(tree) = lower_block(WN_else(tree), actions);
  }

  return tree;
}




/* ====================================================================
 *
 * WN *lower_do_loop(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in DO_LOOP
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_do_loop(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  WN *loop_info;

  Is_True(WN_opcode(tree) == OPC_DO_LOOP,
	  ("expected DO_LOOP node, not %s", OPCODE_name(WN_opcode(tree))));

  loop_info = WN_do_loop_info(tree);  
  loop_nest_depth = loop_info ? WN_loop_depth(loop_info) : loop_nest_depth+1;

  if (Action(LOWER_DO_LOOP))
  {
   /*
    *  Lower DO_LOOP.  Generate:
    *	<start>
    *	(FALSEBR <end> <cont_lbl>)		; unless nz_trip flag set
    *	(LABEL <top_lbl> <loop_info>)
    *	<body>
    *	<step>
    *	(TRUEBR <end> <top_lbl>)
    *  (LABEL <cont_lbl>)			; unless nz_trip flag set
    */
    BOOL nz_trip = loop_info && WN_Loop_Nz_Trip(loop_info);
    WN *wn_top_branch = NULL, *wn_back_branch = NULL;
    WN *body = WN_CreateBlock();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    contains_a_loop = FALSE;
   /*
    *  create loop info for CG
    *  it must be lowered as CG processes this
    */
    if (loop_info == NULL)
    {
      WN *infoblock = WN_CreateBlock();
      WN *trip_count = WN_LOOP_TripCount(tree);

      /* Set the nz_trip if we can */
      if (trip_count && WN_operator_is(trip_count, OPR_INTCONST) &&
	  WN_const_val(trip_count) > 0) {
	 nz_trip = TRUE;
      }
      loop_info = WN_CreateLoopInfo(WN_index(tree),
				    trip_count,
				    0,
				    loop_nest_depth,
				    contains_a_loop ? 0 : WN_LOOP_INNERMOST,
                                    AT_REGION_ID_UNKNOWN);
      loop_info = lower_expr(infoblock, loop_info, actions);
      WN_Set_Loop_Nz_Trip(loop_info);
      WN_DELETE_Tree(infoblock);
    }
    else if (WN_loop_induction(loop_info) != WN_index(tree))
    {
      WN_DELETE_Tree(WN_index(tree));
    }

    WN_INSERT_BlockLast(body, lower_stmt(block, WN_start(tree), actions));

    WN *cont_lbl, *branch_lbl;

    if (nz_trip == FALSE)
    {
      if (!OPT_Space) {
	WN *end = lower_copy_tree(WN_end(tree), actions);

	if (Cur_PU_Feedback)
	  Cur_PU_Feedback->FB_clone_loop_test( WN_end(tree), end, tree );
	
	cont_lbl = WN_NewLabel();
	WN *top_branch_block = lower_falsebr(WN_label_number(cont_lbl), end,
					     &wn_top_branch, actions);
	WN_INSERT_BlockLast(body, top_branch_block);
      } else {
	LABEL_IDX branch_lbl_idx = NewLabel();
	branch_lbl = WN_Label(branch_lbl_idx);
	wn_top_branch = WN_Goto(branch_lbl_idx);
        WN_copy_linenum(tree, wn_top_branch);
	WN_INSERT_BlockLast(body, wn_top_branch);
      }
    }

    setCurrentState(WN_do_body(tree), actions);

    LABEL_IDX top_lbl_idx;
    top_lbl_idx = NewLabel();

    WN *top_lbl = WN_CreateLabel(ST_IDX_ZERO, top_lbl_idx, 0, loop_info);
    WN_copy_linenum(tree, top_lbl);
    WN_INSERT_BlockLast(body, top_lbl);

    WN_INSERT_BlockLast(body, lower_block(WN_do_body(tree), actions));

    setCurrentState(WN_step(tree), actions);
    WN_INSERT_BlockLast(body, lower_stmt(block, WN_step(tree), actions));
    if (OPT_Space && (nz_trip == FALSE)) {
      WN_INSERT_BlockLast(body, branch_lbl);
    }
    WN *back_branch_block = lower_truebr(WN_label_number(top_lbl),
					 WN_end(tree), &wn_back_branch,
					 actions);
    WN_INSERT_BlockLast(body, back_branch_block);

    if (!OPT_Space && (nz_trip == FALSE)) {
      WN_copy_linenum(tree, cont_lbl);
      WN_INSERT_BlockLast(body, cont_lbl);
    }

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, wn_top_branch, wn_back_branch );

    WN_Delete(tree);

    tree = body;
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN_start(tree) = lower_stmt(block, WN_start(tree), actions);
    WN_end(tree) = lower_expr(block, WN_end(tree), actions);
    WN_step(tree) = lower_stmt(block, WN_step(tree), actions);
    WN_do_body(tree) = lower_block(WN_do_body(tree), actions);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}



/* ====================================================================
 *
 * WN *lower_do_while(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on statements in DO_WHILE
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_do_while(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_DO_WHILE,
	  ("expected DO_WHILE node, not %s", OPCODE_name(WN_opcode(tree))));

  ++loop_nest_depth;
#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_DO_WHILE))
#else
  if (Action(LOWER_DO_WHILE) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior (WN_while_test(tree))))
#endif
  {
   /*
    * Lower DO_WHILE.  Generate:
    *	(LABEL <top_lbl>)
    *	<body>
    *	(TRUEBR <test> <top_lbl>)
    */
    WN *body = WN_CreateBlock();
    WN *top_lbl = WN_NewLabel();

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }
    WN_copy_linenum(tree, top_lbl);
    WN_INSERT_BlockFirst(body, top_lbl);
    WN_INSERT_BlockLast(body, lower_block(WN_while_body(tree), actions));

    WN *wn_back_branch = NULL;
    WN *back_branch_block
      = lower_truebr(WN_label_number(top_lbl), WN_while_test(tree),
		     &wn_back_branch, actions);
    if (wn_back_branch)
      WN_Set_Linenum(wn_back_branch, WN_linenum(tree));
    WN_INSERT_BlockLast(body, back_branch_block);

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, NULL, wn_back_branch );

    WN_Delete(tree);
    tree = body;
  }
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN *testBlock = WN_CreateBlock();

    WN_while_body(tree) = lower_block(WN_while_body(tree), actions);

    WN_while_test(tree) = lower_expr(testBlock, WN_while_test(tree), actions);

    WN_copy_linenum(tree, testBlock);
    WN_INSERT_BlockLast(WN_while_body(tree), testBlock);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}


/* ====================================================================
 *
 * WN *lower_while_do(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Lower WHILE_DO.  Generate:
 *	(FALSEBR (<test>) <cont_lbl>)
 *	(LABEL <top_lbl>)
 *	<body>
 *	(TRUEBR <test> <top_lbl>)
 *  (LABEL <cont_lbl>)
 *
 *
 * Perform lowering (see WN_Lower description) on statements in WHILE_DO
 * node <tree>, returning lowered statements.  Returned tree will always
 * have a structured control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_while_do(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(tree) == OPC_WHILE_DO,
	  ("expected WHILE_DO node, not %s", OPCODE_name(WN_opcode(tree))));

  ++loop_nest_depth;
#ifndef SHORTCIRCUIT_HACK
  if (Action(LOWER_WHILE_DO))
#else
  if (Action(LOWER_WHILE_DO) || 
      (Action(LOWER_SHORTCIRCUIT) && tree_has_cand_cior (WN_while_test(tree))))
#endif
  {
   /*
    *	(FALSEBR <test> <cont_lbl>) into block
    *	(LABEL <top_lbl>)
    *	<body>
    *	(TRUEBR <test> <top_lbl>)
    *  (LABEL <cont_lbl>)
    */
    LABEL_IDX top_lbl, cont_lbl;
    top_lbl  = NewLabel();
    cont_lbl = NewLabel();

    WN *body = WN_CreateBlock();
    WN *test = NULL;
    if (!OPT_Space) {
      test = lower_copy_tree( WN_while_test(tree), actions);
      if (Cur_PU_Feedback)
	Cur_PU_Feedback->FB_clone_loop_test( WN_while_test(tree), test, tree );
    }

    if (Action(LOWER_TOP_LEVEL_ONLY))
    {
      actions = RemoveScfAction(actions);
    }

    WN *wn_top_branch = NULL;
    WN *wn_branch_lbl;
    if (!OPT_Space) {
      WN *top_branch_block = lower_falsebr(cont_lbl, test, &wn_top_branch,
					   actions);
      if (wn_top_branch)
	WN_Set_Linenum(wn_top_branch, WN_linenum(tree));
      WN_INSERT_BlockLast(block, top_branch_block);
    } else {
      LABEL_IDX branch_lbl = NewLabel();
      wn_branch_lbl = WN_Label(branch_lbl);
      wn_top_branch = WN_Goto(branch_lbl);
      WN_INSERT_BlockLast(block, wn_top_branch);
    }

    setCurrentState(WN_while_body(tree), actions);
    WN *wn_top_lbl = WN_Label(top_lbl);
    WN_INSERT_BlockLast(body, wn_top_lbl);

    setCurrentStateBlockLast(WN_while_body(tree), actions);
    WN_INSERT_BlockLast(body, lower_block(WN_while_body(tree), actions));

    WN *wn_back_branch = NULL;
    if (OPT_Space) {
      WN_INSERT_BlockLast(body, wn_branch_lbl);
    }
    WN *back_branch_block = lower_truebr(top_lbl, WN_while_test(tree),
					 &wn_back_branch, actions);
    if (wn_back_branch)
      WN_Set_Linenum(wn_back_branch, WN_linenum(tree));
    WN_INSERT_BlockLast(body, back_branch_block);
    WN *wn_cont_lbl = WN_Label(cont_lbl);
    WN_INSERT_BlockLast(body, wn_cont_lbl);

    if ( Cur_PU_Feedback )
      Cur_PU_Feedback->FB_lower_loop( tree, wn_top_branch, wn_back_branch );

    WN_Delete(tree);

    tree = body;
  }
 /*
  * We're not lowering WHILE_DOs, so just lower children.
  * The semantics of the WHILE_DO require any statement level whirl
  * created during the lowering of the while test be copied to both the
  * block (before the WHILE_DO) and in the end of while_body
  */
  else if (NotAction(LOWER_TOP_LEVEL_ONLY))
  {
    WN *copytestBlock;
    WN *testBlock = WN_CreateBlock();

    WN_while_body(tree) = lower_block(WN_while_body(tree), actions);

    WN_while_test(tree) = lower_expr(testBlock, WN_while_test(tree), actions);
    copytestBlock = lower_copy_tree(testBlock, actions);

    if (Cur_PU_Feedback && WN_first(testBlock))
      Cur_PU_Feedback->FB_clone_loop_test( testBlock, copytestBlock, tree );

    WN_copy_linenum(tree, copytestBlock);
    WN_INSERT_BlockLast(block, copytestBlock);
    WN_copy_linenum(tree, testBlock);
    WN_INSERT_BlockLast(WN_while_body(tree), testBlock);
  }

  --loop_nest_depth;
  contains_a_loop = TRUE;
  return tree;
}



/* ====================================================================
 *
 *  The parameter types may not be correct, and as usual, we have to
 *  make up for it. Integral types are canonicalized [I,U][1,2] types
 *  (to [I,U]4) and do not require explicit conversion
 *  (the conversion happens when the value is loaded)
 *
 *  Mostly floats are passed as doubles and must be converted 
 *
 * ==================================================================== */
static WN *lower_promoted_formal(PLOC ploc, ST *formalST)
{
  WN		*ldid, *cvt;
  TYPE_ID	regType, formalType;
  WN_OFFSET	offset = 0;
  
  formalType = TY_mtype(Ty_Table[ST_type(formalST)]);

  regType = Mtype_comparison(TY_mtype(Promoted_Parm_Type(formalST)));

  if (!PLOC_on_stack(ploc))
  {
    formalST = MTYPE_To_PREG(regType);
    offset = PLOC_reg(ploc);
  }

  ldid = WN_Ldid(regType, offset, formalST, ST_type(formalST));

  cvt =	WN_Type_Conversion(ldid, formalType);

  return cvt;
}

/* ====================================================================
 *
 * PLOC lower_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 *  lower formal tree returning a ploc
 *
 * ==================================================================== */

static void lower_formals(WN *block, WN *formal, PLOC ploc,
			  LOWER_ACTIONS actions)
{
  BOOL    altentry = PU_has_altentry(Get_Current_PU());
  ST     *sym      = WN_st(formal);
  TY_IDX  formalTY = WN_type(formal);

  if (PLOC_on_stack(ploc))
  {
    /* more small struct special casing */
    if (MTYPE_is_m(TY_mtype(formalTY)) &&
	Target_Byte_Sex == BIG_ENDIAN && 
	PLOC_size(ploc) < MTYPE_byte_size(MTYPE_U4)) {
      INT32 size = PLOC_size(ploc);
      INT32 oldoffset = ST_ofst(sym);
      if (size == 1)
	Set_ST_ofst(sym, oldoffset + 3);
      else if (size == 2)
	Set_ST_ofst(sym, oldoffset + 2);
      else {
	/* if we just set the offset here, then other functions will try to 
	   access it with unaligned loads. If, say, you have

	   struct foo {
	      short a;
	      char b;
           };
	   
	   Then you have a three-byte structure with the first field unaligned
	   if we just leave it in place.

	   Only thing to do is move it. We can use four byte loads and stores here 
	   because this is on the stack, and we know that we won't walk off the end 
	   of it. */
	WN *ldid = WN_Ldid(MTYPE_U4, 0, sym, MTYPE_To_TY(MTYPE_U4));
	WN *shift = WN_Shl(MTYPE_U4, ldid, WN_Intconst(MTYPE_U4, 8));
	WN *stid = WN_Stid(MTYPE_U4, 0, sym, MTYPE_To_TY(MTYPE_U4), shift);
	stid = lower_store(block, stid, actions);
	WN_Set_Linenum (stid, current_srcpos);
	WN_INSERT_BlockLast(block, stid);
      }
    }
    
    /* on stack already */
    if (altentry)
    {
      ST *upformal = Get_Altentry_UpFormal_Symbol (sym, ploc);
      WN *ldid = WN_Ldid(TY_mtype(formalTY), 0, upformal, formalTY);
      WN *stid = WN_Stid(TY_mtype(formalTY), 0, sym, formalTY, ldid);

      stid = lower_store(block, stid, actions);
      WN_Set_Linenum (stid, current_srcpos);
      WN_INSERT_BlockLast(block, stid);
    }
    else if (ST_promote_parm(sym))
    {
      if (MTYPE_is_float(TY_mtype(formalTY)))
      {
	WN *cvt  = lower_promoted_formal(ploc, sym);
	WN *stid = WN_Stid(TY_mtype(formalTY), 0, sym, formalTY, cvt);

	WN_Set_Linenum (stid, current_srcpos);
	WN_INSERT_BlockLast(block, stid);
      }
    }
  }
  else if (MTYPE_is_m(TY_mtype(formalTY)))
  {
    /* structure parameter */
    lower_mload_formal (block, formal, ploc, actions);
  }
  else
  {
    WN *cvt  = lower_promoted_formal(ploc, sym);

#ifdef TARG_XTENSA
    // F8 -> F4 conversion required for non-prototyped float parameters
    // introduces a call to __truncdfsf2 in the entry block, which may
    // lead to incorrect register allocation for parameters that follow
    // it. To avoid this, we order incoming parameter registers so that
    // those that don't require conversion are always before those that
    // do. If there are multiple parameters that require conversion,
    // for all but the first one of those we copy registers in which F8
    // is passed into an F8 PREG, and then use that PREG when calling 
    // the conversion function.

    WN *insertion_point = NULL;
    BOOL new_f4f8cvt = FALSE;
    BOOL seen_f4f8cvt = (WN_last(block) &&
                         WN_opcode(WN_last(block)) == OPC_F4STID &&
                         WN_opcode(WN_kid0(WN_last(block))) == OPC_F4F8CVT);
    
    if (seen_f4f8cvt) {
      // If we had already seen one F8 -> F4 conversion and we have
      // another one, we copy corresponding registers into an F8 PREG
      if (ST_promote_parm(sym) && !PLOC_on_stack(ploc)) {
        Is_True(WN_opcode(cvt) == OPC_F4F8CVT, 
                ("ST_promote_parm set for non-float formal parameter"));
        // Copy LDID of the dedicated parameter register
        WN *ldid = WN_kid0(cvt);
        WN *early_ldid = WN_COPY_Tree(ldid);
        // Create an F8 preg to save incoming parameter registers
        PREG_NUM f8_preg = Create_Preg(MTYPE_F8, ST_name(sym));
        WN *early_stid = WN_Stid(MTYPE_F8, 
                                 f8_preg, 
                                 MTYPE_To_PREG(MTYPE_F8), 
                                 Be_Type_Tbl(MTYPE_F8), 
                                 early_ldid);
        // Insert the copy at the beginning of the block
        WN_INSERT_BlockFirst(block, early_stid);
        // Change the original LDID to use the PREG
        WN_load_offset(ldid) = f8_preg;
        new_f4f8cvt = TRUE;
      }
      else {
        // If this parameter does not need conversion, we insert it before
        // the first F8 -> F4 conversion (which will become a call)
        insertion_point = WN_last(block);
        while (WN_prev(insertion_point) &&
               WN_opcode(WN_prev(insertion_point)) == OPC_F4STID &&
               WN_opcode(WN_kid0(WN_prev(insertion_point))) == OPC_F4F8CVT) {
          insertion_point = WN_prev(insertion_point);
        }
      }
    }
#endif    
      
    WN *stid = WN_Stid(TY_mtype(formalTY), 0, sym, formalTY, cvt);

    WN_Set_Linenum (stid, current_srcpos);

#ifdef TARG_XTENSA
    if (seen_f4f8cvt && !new_f4f8cvt)
      WN_INSERT_BlockBefore(block, insertion_point, stid);
    else
#endif      
      WN_INSERT_BlockLast(block, stid);
  }
}



/* ====================================================================
 *
 * PLOC lower_entry_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on ENTRY and ALTENTRY nodes
 *
 * for LOWER_ENTRY_EXIT
 *		lower all but sclass formal refs
 * for LOWER_ENTRY_FORMAL_REF
 *		lower just sclass formal refs
 *
 * ==================================================================== */

static PLOC lower_entry_formals(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  PLOC	ploc;
  INT32	i, n;

  ploc = Setup_Input_Parameter_Locations(ST_pu_type(WN_st(tree)));

  if (WN_opcode(tree) == OPC_ALTENTRY)
  {
    n =	WN_kid_count(tree);
    Reset_UPFORMAL_Segment();
  }
  else
  {
    n =	WN_num_formals(tree);
  }

  for (i = 0; i < n; i++)
  {
    WN *formal = WN_formal(tree, i);
	
    if (WN_sclass(formal) == SCLASS_FORMAL_REF)
    {
      ST *base;

      base = Get_ST_formal_ref_base(WN_st(formal));
      ploc = Get_Input_Parameter_Location(ST_type(base));

      if (NotAction(LOWER_ENTRY_FORMAL_REF))
      {
	continue;
      }
      else
      {
	formal = WN_CreateIdname(WN_idname_offset(formal), base);
      }
    }
    else
    {
      ploc = Get_Input_Parameter_Location(Promoted_Parm_Type(WN_st(formal)));

      if (NotAction(LOWER_ENTRY_EXIT))
      {
	continue;
      }
    }
    lower_formals(block, formal, ploc, actions);
  }

  return ploc;
}

#ifndef TARG_XTENSA
static WN *
Create_Mcount_Call (void)
{
    TY_IDX ty = Make_Function_Type(MTYPE_To_TY(MTYPE_V));
    ST *call_st = Gen_Intrinsic_Function(ty, "mcount");
    WN *profcall = WN_Call(MTYPE_V, MTYPE_V, 1, call_st);

    ST *data_st = New_ST (GLOBAL_SYMTAB);
    ST_Init (data_st, Save_Str2i("mcount_","data_", Current_PU_Count()),
                CLASS_VAR, SCLASS_FSTATIC, EXPORT_LOCAL,
                MTYPE_To_TY(Pointer_Mtype));
    // Set_ST_not_gprel(data_st);
    Set_ST_is_initialized(data_st);
    INITO_IDX ino = New_INITO(data_st);
    INITV_IDX inv = New_INITV();
    INITV_Init_Integer (inv, Pointer_Mtype, 0);
    Set_INITO_val(ino, inv);

    WN_actual(profcall, 0) = WN_Lda(Pointer_type, 0, data_st);

    return profcall;
}
#endif

/* ====================================================================
 *
 * WN *lower_entry(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on ENTRY and ALTENTRY nodes
 *
 * for ENTRY
 *		return entry node
 * for ALTENTRY
 *		return block with lowered formals 
 *
 * for LOWER_ENTRY_EXIT
 *		create trapuv code (uninitialized variable)
 *		create varargs register assignment
 *		create slink sym initialization
 *
 * for LOWER_RETURN_VAL
 *		create fake first parameter for struct return if needed
 *
 * ==================================================================== */

static WN *lower_entry(WN *tree, LOWER_ACTIONS actions)
{
  PLOC	ploc;
  WN	*block;

  Is_True(((WN_opcode(tree) == OPC_FUNC_ENTRY)
	   || (WN_opcode(tree) == OPC_ALTENTRY)),
          ("expected ENTRY/ALTENTRY node, not %s",
	   OPCODE_name(WN_opcode(tree))));

  if (WHIRL_Return_Val_On && Action(LOWER_RETURN_VAL) && 
      WN_operator(tree) == OPR_FUNC_ENTRY)
  { // create fake first parameter for struct return if needed 
    ST_IDX func_stidx = WN_st_idx(tree);
    PU_IDX puidx = ST_pu(St_Table[func_stidx]);
    TY_IDX prototype = PU_prototype(Pu_Table[puidx]);
    RETURN_INFO return_info = Get_Return_Info(TY_ret_type(prototype), 
					      Complex_Not_Simulated);
    if (RETURN_INFO_return_via_first_arg(return_info)) {
      ST *return_st = Gen_Temp_Symbol(
			      Make_Pointer_Type(TY_ret_type(prototype), FALSE),
			      Index_To_Str(Save_Str2(".return.",
						       ST_name(func_stidx))));
      Set_ST_sclass(return_st, SCLASS_FORMAL);
      Set_ST_is_value_parm(return_st);
      WN *idname = WN_CreateIdname(0, return_st);
      // create the new func_entry node
      WN *n_tree = WN_CreateEntry(WN_num_formals(tree)+1, func_stidx,
				  WN_func_body(tree), WN_func_pragmas(tree),
				  WN_func_varrefs(tree));
      WN_Set_Linenum(n_tree, WN_Get_Linenum(tree));
      WN_map_id(n_tree) = WN_map_id(tree);
      WN_kid0(n_tree) = idname;
      for (INT32 i = 0; i < WN_kid_count(tree); i++)
	WN_kid(n_tree, i+1) = WN_kid(tree, i);
      // fix pu pointer from RID
      if (RID_map != WN_MAP_UNDEFINED) {
        RID *rid = (RID *)WN_MAP_Get(RID_map, tree);
        if (RID_rwn(rid) == tree)
	  RID_rwn(rid) = n_tree;
      }
#ifndef TARG_XTENSA
      /* We can't delete 'tree' since that would free the map id's
         associated with tree even though 'n_tree' is now using
         them. */
      WN_Delete(tree);
#endif
      Set_PU_Info_tree_ptr(Current_PU_Info, n_tree);
      tree = n_tree;
    }
  }

  setCurrentState(tree, actions);
  block =		WN_CreateBlock();
  
  if (Action(LOWER_ENTRY_EXIT))
  {
    ploc = lower_entry_formals(block, tree, actions);

    if (TY_is_varargs(Ty_Table[PU_prototype(Pu_Table[ST_pu(WN_st(tree))])]))
    {
     /*
      *  For varargs, the func-entry just has the list of fixed
      *  parameters, so also have to store the vararg registers. 
      */
      TYPE_ID	type = Def_Int_Mtype;

      if (PLOC_is_nonempty(ploc) && !PLOC_on_stack(ploc)) {
	/* don't do if already reached stack params */
	ploc = Get_Input_Parameter_Location (MTYPE_To_TY(Pointer_Mtype));

        while (!PLOC_on_stack(ploc))
        {
         /*
          *  vararg registers must be integer registers
          */
	  WN	*wn;
	  ST	*st;
  
	  wn = WN_Ldid (type, PLOC_reg(ploc), Int_Preg, ST_type(Int_Preg));
         /*
	  *  get the symbol for the vararg formal
	  */
	  st = Get_Vararg_Symbol (ploc);
  
	  wn = WN_Stid (type, 0, st, ST_type(st), wn);
	  WN_Set_Linenum (wn, current_srcpos);
	  WN_INSERT_BlockLast(block, wn);
  
	  ploc = Get_Vararg_Input_Parameter_Location (ploc);
        }
       }
      }

   /*
    * add initialization code for trapuv
    */
    {
      WN *trapuvBlock = WN_CreateBlock();

      trapuvBlock = lower_trapuv(trapuvBlock, tree, actions);

      trapuvBlock= lower_block(trapuvBlock, actions);

      WN_copy_linenum(tree, trapuvBlock);


      WN_INSERT_BlockLast(block, trapuvBlock);
    }

    /* Initialize the return address if necessary */
    ST* return_addr = Find_Return_Addr_Symbol(CURRENT_SYMTAB);
    if (return_addr) {
      WN *ret_addr_reg = WN_LdidPreg(Pointer_Mtype, 0);
      WN *wn = WN_Stid (Pointer_type, 0, return_addr, ST_type(return_addr), 
			ret_addr_reg);
      WN_INSERT_BlockFirst(block, wn);
    }
  }
  else if (Action(LOWER_ENTRY_FORMAL_REF))
  {
    ploc = lower_entry_formals(block, tree, actions);
  }

  if (Action(LOWER_SPLIT_SYM_ADDRS))
  {
    /*
     *  Initialize the static link if required ($2 should have callers $fp)
     */
    ST *slink = Find_Slink_Symbol(CURRENT_SYMTAB);
    if (slink)
    {
      WN	*ld, *wn;
#ifdef TARG_XTENSA
      if (Pass_Static_Link_On_Stack) {
	WN *stack = WN_Lda(Pointer_type, 0, first_upformal_sym);
	TY_IDX addr_ty = Make_Pointer_Type(MTYPE_To_TY(Pointer_type),TRUE);
	ld = WN_Iload(Pointer_type, Static_Link_Stack_Offset, addr_ty, stack);
      } else
#endif
	ld = WN_LdidPreg(Pointer_type, Static_Link_Preg_Offset);

      wn = WN_Stid (Pointer_type, 0, slink, ST_type(slink), ld);

      WN_Set_Linenum (wn, current_srcpos);
      WN_INSERT_BlockFirst(block, wn);
    }
  }

  if (WN_opcode(tree) == OPC_FUNC_ENTRY)
  {
    /* Process any PU-scope MP pragmas */
    if (WN_func_pragmas(tree) && Action(LOWER_MP)) {
      WN *wn;
      for (wn = WN_first(WN_func_pragmas(tree)); wn; wn = WN_next(wn)) {
	if (((WN_opcode(wn) == OPC_PRAGMA) || (WN_opcode(wn) == OPC_XPRAGMA))
	    && (WN_pragmas[WN_pragma(wn)].users & PUSER_MP)) {
	  (void) lower_mp(NULL, wn, actions);
	}
      }
    }

#ifndef TARG_XTENSA
    if (Call_Mcount && Action(LOWER_ENTRY_EXIT)) {
	WN *profcall = Create_Mcount_Call ();
	WN_Set_Linenum (profcall, current_srcpos);
	WN_INSERT_BlockLast(block, profcall);
    }
#endif

    WN_INSERT_BlockFirst(WN_func_body(tree), block);

    /* Lower statements in function body. */
    WN_func_body(tree) = lower_block(WN_func_body(tree), actions);
    return tree;
  }
  else
  {
    block  = lower_block(block, actions);
    WN_INSERT_BlockFirst(block, tree);
  }
  return block;
}




/* ====================================================================
 *
 * WN *lower_region(WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering on REGION node
 * <tree> returning lowered REGION tree.
 *
 * ==================================================================== */

static WN *lower_region(WN *tree, LOWER_ACTIONS actions)
{
  Is_True(WN_opcode(WN_region_body(tree)) == OPC_BLOCK,
	  ("kid of REGION should be OPC_BLOCK, not %s",
	   OPCODE_name(WN_opcode(WN_region_body(tree)))));

  setCurrentState(tree, actions);
  if (current_function == NULL) {
    // haven't seen FUNC_ENTRY yet
    current_function = PU_Info_tree_ptr(Current_PU_Info);
  }

  if (Action(LOWER_REGION))
  {
    RID *rid = REGION_get_rid( tree );

    Is_True(rid, ("expected valid region id"));

    max_region++;

   /*
    *  first time thru. Set region lowered flags
    *  otherwise remove flags that have already been processed.
    */
    if (RID_lowered(rid) == (LOWER_ACTIONS) NULL)
    {
      RID_lowered(rid) = actions;
    }
    else
    {
      actions ^= (RID_lowered(rid) & (LOWER_CALL | LOWER_ENTRY_EXIT));
    }

    if (actions)
    {
      WN_region_body(tree) = lower_block(WN_region_body(tree), 
					 actions | LOWER_REGION);
    }
  }
  else
  {
    WN_region_body(tree) = lower_block(WN_region_body(tree), actions);
  }

  return tree;
}

WN *lower_scf_non_recursive(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  return lower_scf(block,tree,actions | LOWER_TOP_LEVEL_ONLY);
}

/* ====================================================================
 *
 * WN *lower_scf(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Perform lowering (see WN_Lower description) on structured control
 * flow node <tree>.  Returned tree will always have a structured
 * control flow node (at least a BLOCK) at the top.
 *
 * ==================================================================== */

static WN *lower_scf(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  CURRENT_STATE scfState = pushCurrentState(tree, actions);

  switch (WN_opcode(tree))
  {
  case OPC_DO_WHILE:
    block = lower_do_while(block, tree, actions);
    break;

  case OPC_WHILE_DO:
    block = lower_while_do(block, tree, actions);
    break;

  case OPC_DO_LOOP:
    block = lower_do_loop(block, tree, actions);
    break;
      
  case OPC_IF:
    block = lower_if(block, tree, actions);
    break;

  case OPC_BLOCK:
    block = lower_block(tree, actions);
    break;
    
  case OPC_REGION:
    block = lower_region(tree, actions);
    break;
  }

  popCurrentState(scfState);
  return block;
}


/* ====================================================================
 *
 * WN *lower_trapuv(WN *block, WN *tree, LOWER_ACTIONS actions)
 *
 * Initialize stack variables as per trapuv
 *
 * ==================================================================== */

static WN *lower_trapuv(WN *block, WN *tree, LOWER_ACTIONS actions)
{
  if ( DEBUG_Trap_Uv == FALSE)
    return block;

  {
    ST	*st;
    ST  *slink = Find_Slink_Symbol(CURRENT_SYMTAB);

    INT32 i;
    FOREACH_SYMBOL(CURRENT_SYMTAB, st, i)
    {
      TY_IDX  type;
      TYPE_ID btype;
      INT32   size;

      if (ST_class(st) != CLASS_VAR)
	continue;

      if (ST_sclass(st) != SCLASS_AUTO)
	continue;

      if (Has_Base_Block(st))
	continue;

      if (ST_is_uplevelTemp(st) || st == slink)
	continue;

      if (ST_is_not_used(st)) 
	continue;
      if (Is_Allocated(st))
	continue;

      type  = ST_type(st);
      btype = TY_mtype(Ty_Table[type]);
      size  = TY_size(type);

      Is_True(ST_pu_defined(st), ("trapuv auto or temp not ST_pu_defined"));

      switch(TY_kind(type))
      {
      case KIND_SCALAR:
      case KIND_POINTER:
      case KIND_FUNCTION:
        {
	  WN  *con, *stid;

	  Is_True((MTYPE_RegisterSize(btype) == size),
		  ("bad size for scalar/pointer"));;

	  con = WN_UVConst(btype);

	  stid = WN_Stid(btype, 0, st, type, con);
	  WN_Set_Linenum(stid, WN_Get_Linenum(tree));
	  WN_INSERT_BlockLast(block, stid);
        }
	break;

      case KIND_ARRAY:
      case KIND_STRUCT:
	{
	 /*
	  *  Assign bit pattern just on basis of size.
	  *  We cannot truncate the pattern, so we set ST_use_reg_align();
	  *  We always start at offset 0 (we know it is aligned)
	  *  and replicate the following pattern  (0xFFFA5A5A)
	  *
	  *	size	right-justify		left-justify
	  *	----	-------------		------------
	  *	1	5A			FF
	  *	2	5A5A			FFFA
	  *	3	FA5A5A			FFFA5A
	  *	4	FFFA5A5A		FFFA5A5A
	  *	5	5AFFFA5A 5A		FFFA5A5A FF
	  *	6	5A5AFFFA 5A5A		FFFA5A5A FFFA
	  *	7	FA5A5AFF FA5A5A		FFFA5A5A FFFA5A
	  *
	  *  We do the assignment in chunks, special casing the
	  *  last assigment.
	  *  These weird patterns are only relevant for character
	  *  and structure assignment
	  */
	  TY_IDX	ptype = Make_Pointer_Type(type);
	  WN           *conUV4 = NULL;
	  INT32		todo = size;
	  INT32		offset = 0;
	  UINT32        ncon;
	  TYPE_ID       q;

	  for (q= Max_Uint_Mtype; q!=MTYPE_UNKNOWN; q= Mtype_prev_alignment(q))
	  {
	    WN	   *con, *lda, *num, *store;
	    INT32  qSize   = MTYPE_RegisterSize(q);
	    INT32  qBits   = MTYPE_size_reg(q);
	    INT32  nMoves  = todo / qSize;
	    INT32  residue = todo % qSize;

	    if (q >= MTYPE_U4)
	    { 
	      if (nMoves)
	      {
		con = WN_UVConst(q);

  		if ( DEBUG_Trap_Uv_Rjustify )
  		{
		  con = WN_RotateIntconst(con, residue*8);
  		}

		lda = WN_Lda(Pointer_type, 0, st);
		num = WN_Intconst(MTYPE_I4, nMoves * qSize);

		store = WN_CreateMstore(offset, ptype, con, lda, num);

		WN_Set_Linenum(store, WN_Get_Linenum(tree));
		WN_INSERT_BlockLast(block, store);

		todo   -= (nMoves * qSize);
		offset += (nMoves * qSize);
	      }
	    }
	    else
	    {
	     /*
	      *  very tricky residue code (size 1,2,3).
	      */
	      if (todo > 0)
	      {
		if (conUV4==NULL)
		{
 		  conUV4 = WN_UVConst(MTYPE_U4);
		  ncon   = WN_const_val(conUV4);
		}
		if (nMoves)
		{
		  if ( DEBUG_Trap_Uv_Rjustify )
		  {
		    con = WN_Intconst(MTYPE_U4, ncon>>(todo*8 - qBits));
		  }
		  else
		  {
		    con = WN_Intconst(MTYPE_U4,
				      ncon>>(MTYPE_size_reg(MTYPE_U4)-qBits));
		    ncon <<= qBits;
		  }

		  store = WN_Stid(q, offset, st, type, con);
		  WN_Set_Linenum(store, WN_Get_Linenum(tree));
		  WN_INSERT_BlockLast(block, store);
		
		  todo   -= (nMoves * qSize);
		  offset += (nMoves * qSize);
		}
	      }
	    }
	  }
	  if (conUV4)
	    WN_Delete(conUV4);

	}
	break;
      }
    }
  }
  return block;
}


static void lower_actions_fprintf(FILE *f, LOWER_ACTIONS actions)
{
  LOWER_ACTIONS i = 1;

  while (actions)
  {
    if (Action(i))
    {
      fprintf(f, "%s ", LOWER_ACTIONS_name(i));
      actions = actions ^ i;
    }
    i <<= 1;
  }
  fprintf(f, "\n");
}


/* ====================================================================
 *
 * const char * LOWER_ACTIONS_name(LOWER_ACTIONS actions)
 *
 * Exported.  See interface description in "wn_lower.h".
 *
 * ==================================================================== */
const char * LOWER_ACTIONS_name(LOWER_ACTIONS actions)
{
  if ((actions-1) & actions)
  {
    DevWarn("LOWER_ACTION_name(0x%x): expected only one flag at a time",
	    actions);
  }

  switch(actions)
  {
  case LOWER_NULL:			return "LOWER_NULL";
  case LOWER_DO_LOOP:			return "LOWER_DO_LOOP";
  case LOWER_DO_WHILE:			return "LOWER_DO_WHILE";
  case LOWER_WHILE_DO:			return "LOWER_WHILE_DO";
  case LOWER_IF:			return "LOWER_IF";
  case LOWER_COMPLEX:			return "LOWER_COMPLEX";
  case LOWER_ARRAY:			return "LOWER_ARRAY";
  case LOWER_SPLIT_CONST_OFFSETS:	return "LOWER_SPLIT_CONST_OFFSETS";
  case LOWER_ENTRY_EXIT:		return "LOWER_ENTRY_EXIT";
  case LOWER_CALL:			return "LOWER_CALL";
  case LOWER_SPLIT_SYM_ADDRS:		return "LOWER_SPLIT_SYM_ADDRS";
  case LOWER_IO_STATEMENT:		return "LOWER_IO_STATEMENT";
  case LOWER_MSTORE:			return "LOWER_MSTORE";
  case LOWER_CVT:			return "LOWER_CVT";
  case LOWER_MP:			return "LOWER_MP";
  case LOWER_8X_ARRAY:			return "LOWER_8X_ARRAY";
  case LOWER_INTRINSIC:			return "LOWER_INTRINSIC";
  case LOWER_INLINE_INTRINSIC:		return "LOWER_INLINE_INTRINSIC";
  case LOWER_INL_STACK_INTRINSIC:	return "LOWER_INL_STACK_INTRINSIC";
  case LOWER_REGION:			return "LOWER_REGION";
  case LOWER_QUAD:			return "LOWER_QUAD";
  case LOWER_COMPGOTO:			return "LOWER_COMPGOTO";
  case LOWER_MADD:			return "LOWER_MADD";
  case LOWER_TOP_LEVEL_ONLY:		return "LOWER_TOP_LEVEL_ONLY";
  case LOWER_PREFETCH_MAPS:		return "LOWER_PREFETCH_MAPS";
  case LOWER_ALIAS_MAPS:		return "LOWER_ALIAS_MAPS";
  case LOWER_DEPGRAPH_MAPS:		return "LOWER_DEPGRAPH_MAPS";
  case LOWER_PARITY_MAPS:		return "LOWER_PARITY_MAPS";
    // NOTE: Delete LOWER_FREQUENCY_MAPS
  case LOWER_FREQUENCY_MAPS:		return "LOWER_FREQUENCY_MAPS";
  case LOWER_PICCALL:			return "LOWER_PICCALL";
  case LOWER_BASE_INDEX:		return "LOWER_BASE_INDEX";
  case LOWER_TO_CG:			return "LOWER_TO_CG";
  case LOWER_ASSERT:			return "LOWER_ASSERT";
  case LOWER_FORMAL_REF:		return "LOWER_FORMAL_REF";
  case LOWER_UPLEVEL:			return "LOWER_UPLEVEL";
  case LOWER_ENTRY_FORMAL_REF:		return "LOWER_ENTRY_FORMAL_REF";
  case LOWER_SHORTCIRCUIT:		return "LOWER_SHORTCIRCUIT";
  case LOWER_TREEHEIGHT:		return "LOWER_TREEHEIGHT";
  case LOWER_RETURN_VAL:		return "LOWER_RETURN_VAL";
  case LOWER_MLDID_MSTID:		return "LOWER_MLDID_MSTID";
  case LOWER_BIT_FIELD_ID:		return "LOWER_BIT_FIELD_ID";
  case LOWER_BITS_OP:			return "LOWER_BITS_OP";
  case LOWER_TIE_FOR_CG:		return "LOWER_TIE_FOR_CG";
  case LOWER_FLOAT:			return "LOWER_FLOAT";
  case LOWER_PAIRED:			return "LOWER_PAIRED";
  case LOWER_LONGLONG:			return "LOWER_LONGLONG";
  case LOWER_MPY_DIV_CONST:		return "LOWER_MPY_DIV_CONST";
  default:				return "<unrecognized>";
  }
}

static const char * MSTORE_ACTIONS_name(MSTORE_ACTIONS actions)
{
  switch(actions)
  {
  case MSTORE_aggregate:		return "scalar moves    ";
  case MSTORE_loop:			return "generate loop   ";
  case MSTORE_intrinsic_memzero:	return "intrinsic memset to 0 ";
  case MSTORE_intrinsic_memset:		return "intrinsic memset";
  case MSTORE_intrinsic_memcpy:		return "intrinsic memcpy";
  default:				return "<unrecognized>";
  }
}


void WN_Lower_Checkdump(char *msg, WN *tree, LOWER_ACTIONS actions)
{
  traceAlignment   = Get_Trace(TP_LOWER, 0x004);
  traceSplitSymOff = Get_Trace(TP_LOWER, 0x010);
  traceIO          = Get_Trace(TP_LOWER, 0x020);
  traceSpeculate   = Get_Trace(TP_LOWER, 0x040);
  traceTreeHeight  = Get_Trace(TP_LOWER, 0x080);
  traceMload       = Get_Trace(TP_LOWER, 0x100);
  // traceUplevel    = Get_Trace(TP_LOWER, 0x200);

  if (Get_Trace(TP_LOWER, 0x008))
  {
    enable_tree_freq_display();
  }

  if (Get_Trace(TKIND_IR, TP_LOWER))
  {
    fputs(DBar, TFile);
    fprintf(TFile, "WN_Lower: \"%s\"\n", msg);
    if (actions)
    {
      fprintf(TFile, "flags are:\n");
      lower_actions_fprintf(TFile, actions);
    }
    fdump_tree(TFile, tree);
    fputs(DBar, TFile);
  }

  if (Get_Trace(TP_LOWER, 0x001))
  {
    IR_dump_map_info = TRUE;

    fprintf(TFile, "WN_Lower: LNO DEP GRAPH\n");
    LNOPrintDepGraph(TFile);
  }
  if (Get_Trace(TP_LOWER, 0x002))
  {
    IR_dump_map_info = TRUE;

    fprintf(TFile, "WN_Lower: WOPT ALIAS INFO\n");
    fdump_dep_tree(TFile, tree, alias_manager);
  }

  if (Get_Trace(TKIND_SYMTAB,TP_LOWER)) {
    fprintf(TFile,"\n\n========== Symbol tables after Lowering ==========\n");
    Print_symtab (TFile, GLOBAL_SYMTAB);
    Print_symtab (TFile, CURRENT_SYMTAB);
  }

  /*
   * these options can lead to infinite regress
   */
  if (Action(LOWER_SPLIT_SYM_ADDRS))
  {
    if (Enable_WN_Simp && WN_Simp_Fold_ILOAD)
    {
      DevWarn("disabling option WN_Simp_Fold_ILOAD"
	      " while lowering action LOWER_SPLIT_SYM_ADDRS");
      WN_Simp_Fold_ILOAD = FALSE;
    }
  }
  /*
   * get any relevant pragmas
   * I need to know if wopt was run to generate some eval warnings
   */
  {
    WN	*pragma=	NULL;
	  
    traceWoptFinishedOpt=	FALSE;

    switch(WN_operator(tree))
    {
    case OPR_FUNC_ENTRY:
      pragma=	WN_func_pragmas(tree);
      break;
    case OPR_REGION:
      pragma=	WN_region_pragmas(tree);
      break;
    }
    if (pragma)
    {
      WN	*wn;
      for(wn= WN_first(pragma); wn; wn= WN_next(wn))
      {
	if (WN_pragma(wn) == WN_PRAGMA_WOPT_FINISHED_OPT)
	  traceWoptFinishedOpt=	TRUE;
      }
    }
  }
}


/* ====================================================================
 *
 * LOWER_ACTIONS lower_to_cg(LOWER_ACTIONS, LOWER_ACTIONS)
 *
 * The last lowering is special in that it may require lowering
 * already specified and lowering that has been avoided
 * 
 * Some lowering should not be done more than once (ENTRY_EXIT, CALL)
 *
 * Keep track and add/subtract any lowering already processed
 *
 * ==================================================================== */

static LOWER_ACTIONS lower_actions(WN *pu, LOWER_ACTIONS actions)
{
  if (OPT_Lower_Treeheight && Action(LOWER_TO_CG))
    actions |= LOWER_TREEHEIGHT;

  if (Action(LOWER_TO_CG))
  {
   /*
    *  remove/add one-time-only and must-apply flags
    */
    actions |=  lowering_actions ^ (LOWER_ENTRY_EXIT		|
				    LOWER_ENTRY_FORMAL_REF	|
				    LOWER_FORMAL_REF		|
				    LOWER_UPLEVEL		|
				    LOWER_SPLIT_SYM_ADDRS	|
				    LOWER_CALL			|
				    LOWER_MLDID_MSTID		|
				    LOWER_RETURN_VAL);

   /*
    * remove these always
    *
    * disabling SPLIT_CONST will allow the lowerer to put together addresses
    * that have been pulled apart
    */
    actions &=  ~(LOWER_BASE_INDEX | LOWER_SPLIT_CONST_OFFSETS);

   /*
    * these form the major LOWER_TO_CG actions
    */
    actions |=	LOWER_SCF		  |
		LOWER_ARRAY		  |
		LOWER_MP		  |
		LOWER_IO_STATEMENT	  |
		LOWER_MSTORE		  |
		LOWER_CVT		  |
		LOWER_COMPGOTO		  |
		LOWER_COMPLEX		  |
		LOWER_QUAD		  |
		LOWER_MADD		  |
		LOWER_INTRINSIC		  |
		LOWER_ASSERT		  |
		LOWER_PICCALL		  |
		LOWER_ALL_MAPS		  |
		LOWER_SHORTCIRCUIT	  |
		LOWER_INL_STACK_INTRINSIC |
		LOWER_INLINE_INTRINSIC	  |
#ifdef TARG_XTENSA
		LOWER_BITS_OP             |
		LOWER_TIE_FOR_CG          |
		LOWER_FLOAT               |
                /* Make sure any mul/div/rem created since we last
		   lowered mul/div/rem by constants (e.g. from
		   COMPGOTO lowering), have a chance to be lowered. */
                LOWER_MPY_DIV_CONST       |
#endif
		LOWER_BIT_FIELD_ID;

   /*
    *  do not split divides into mul/recip at the CG lowering
    */
    save_Div_Split_Allowed = Div_Split_Allowed;
    Div_Split_Allowed = FALSE;;
  }

  if (Action(LOWER_FLOAT)) {
    /* If we lower floats then we have to lower doubles too. */
    actions |= LOWER_PAIRED;
  }

  if (WN_opcode(pu) == OPC_FUNC_ENTRY)
  {
    lowering_actions |= actions;
  }

  if (Action(LOWER_BITS_OP))
    actions |= LOWER_BIT_FIELD_ID;

  current_actions = actions;

  lower_maps_init(actions);

  setCurrentState(pu, actions);

  return actions;
}




/* ====================================================================
 *
 * Lower_Init(void)
 *
 * lowering specific initialization
 * ==================================================================== */

void Lower_Init(void)
{
   static CURRENT_STATE current_state_NULL;
   /*
    *  create map for marking parity
    */
   lowering_parity_map = WN_MAP32_Create(MEM_pu_pool_ptr);

   lowering_actions = 0;
   current_state = current_state_NULL;

   // save parity_map in an array becuse it may change in nested PUs
   parity_map_index++;
   FmtAssert(0 <= parity_map_index && parity_map_index < PARITY_MAP_ARRAY_SIZE,
             ("Lower_Init: Index into parity map array is out of range"));
   parity_map_array[parity_map_index] = lowering_parity_map;
}

void Lowering_Finalize(void)
{
  /* free lowering_parity_map */
  WN_MAP_Delete(lowering_parity_map);
  
  parity_map_index--;
  if (parity_map_index >= 0) {
    // if there is a saved parity map, then restore it
    lowering_parity_map = parity_map_array[parity_map_index];
  }
  else {
    // otherwise, set it to undefined
    lowering_parity_map = WN_MAP_UNDEFINED;
  }
}




static void lower_end(WN *tree, LOWER_ACTIONS actions)
{
  lower_maps_reset(actions);

  if (Action(LOWER_TO_CG))
  {
    Div_Split_Allowed = save_Div_Split_Allowed; /* reset */
  }

  popCurrentState(current_state);
}

/* ====================================================================
 *
 * WN *WN_Lower(WN *tree, LOWER_ACTIONS actions)
 *
 * Exported.  See interface description in "wn_lower.h".
 *
 * ==================================================================== */

WN *WN_Lower(WN *tree, LOWER_ACTIONS actions, struct ALIAS_MANAGER *alias,
	     char *msg)
{

  static bool promoted_mtype_initialized = FALSE;
  TYPE_ID mtype_index;

#ifdef BACK_END
  Start_Timer(T_Lower_CU);
#endif
  alias_manager =	alias;
  loop_nest_depth =	0;

  if (promoted_mtype_initialized==FALSE) {

    for (mtype_index=MTYPE_XTBOOL;
	 mtype_index<=MTYPE_XTBOOL16;
	 mtype_index++) {
      Promoted_Mtype[mtype_index] = mtype_index;
    }

    for (mtype_index=MTYPE_CORE_LAST+1;
	 mtype_index<=Mtype_Last;
	 mtype_index++) {
      Promoted_Mtype[mtype_index] = mtype_index;
    }

    promoted_mtype_initialized = TRUE;
  }

  // Don't do any lowering on trees that merely wrap up file-scope
  // assembly language code.
  if (WN_operator(tree) == OPR_FUNC_ENTRY &&
      ST_asm_function_st(*WN_st(tree))) {
    return tree;
  }

  actions = lower_actions(tree, actions);

  if (Action(LOWER_MP)) {
    /* Initialize the MP lowerer.
     * Note: We're assuming that for MP-lowering,
     * this routine (WN_Lower) is called once per PU.
     */
    LowerMP_PU_Init ();
  }

  WN_Lower_Checkdump(msg, tree, actions);

  if (WN_opcode(tree) == OPC_FUNC_ENTRY)
  {
    tree = lower_entry(tree, actions);
  }
  else if (OPCODE_is_scf(WN_opcode(tree)))
    {
    tree = lower_scf(NULL, tree, actions);
  }
  else if (OPCODE_is_stmt(WN_opcode(tree)))
    {
    tree = lower_stmt(NULL, tree, actions);
  }
  else if (OPCODE_is_expression(WN_opcode(tree)))
  {
    tree = lower_expr(NULL, tree, actions);
  }

  lower_end(tree, actions);

  WN_Lower_Checkdump("After lowering", tree, 0);

#ifdef BACK_END
  Stop_Timer(T_Lower_CU);
#endif
 
  WN_verifier(tree, TRUE);

  return tree;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
