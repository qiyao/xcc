
/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: PUinfo.c
 * $Revision: 1.62 $
 * $Date: 2000/04/06 02:20:17 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/whirl2c/RCS/PUinfo.cxx,v $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    Maintains information pertaining to the translation of compilation
 *    units, and provides routines for collecting such information:
 *
 *       * Symbol-name and Symbol-id information
 *
 *       * Preg usage information
 *
 *       * Function call and return site information
 *
 *    The details of the information collected here may vary between 
 *    whirl2f and whirl2c, but every collection of information 
 *    pertains to both.  A collection of information pertaining only
 *    to one or the other should be maintained in a module specific
 *    to one or the other, not here.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/be/whirl2c/RCS/PUinfo.cxx,v $ $Revision: 1.62 $";
#endif /* _KEEP_RCS_ID */

#include <string.h>

#ifdef BUILD_WHIRL2C
# include "whirl2c_common.h"
#else /*BUILD_WHIRL2F*/
# include "whirl2f_common.h"
#endif
#include "w2cf_parentize.h" /* For W2CF_Parent_Map */
#include "mempool.h"
#include "const.h"
#include "wn_util.h"
#include "PUinfo.h"
#include "targ_sim.h"
#include "intrn_info.h"

#define IS_RETURN_PREG(wn) \
        (ST_class(WN_st(wn)) == CLASS_PREG \
        && (Preg_Is_Dedicated_Incoming_Ret(WN_load_offset(wn)) \
	   || WN_st(wn) == Return_Val_Preg))

/*------------------ Some PU state variables --------------------------*
 *---------------------------------------------------------------------*/

const WN          *PUinfo_current_func = NULL; /* OPR_FUNC_ENTRY node */
const RETURN_PREG *PUinfo_return_preg = NULL;  /* Pregs for return type */
TOKEN_BUFFER       PUinfo_local_decls = NULL;  /* Tokens for local decls */
TOKEN_BUFFER       PUinfo_pragmas = NULL;      /* Tokens for PU level pragmas */
UINT               PUinfo_local_decls_indent = 0;/*Indentation for local decls*/


/*---------------- Information about the usage if pregs ---------------
 * 
 * We accumulate information about usage of CLASS_PREG symbols, such
 * that we can declare them as local variables of suitable type and
 * minimize the need to cast them to other types.  We use a very
 * simple hash-table of 73 elements, indexed by the preg offset, to
 * reduce search times.  Note that all uses of pregs must be determined
 * before we start translating a PU, while declarations of pregs occur
 * upon actual use.  We cannot refer to or declare pregs until all
 * information has been accumulated, since we otherwise do not know 
 * what types of pregs we need (in particular, we only declare one type
 * of integral pregs). Each entry in this preg-table may represent
 * multiple preg declarations.
 *
 * We enter the pregs for a PU into the name-disambiguation symbol table
 * before attempting any translation of the PU.  This ensures that the
 * preg names remain invariant regardless whether or not we only
 * translate a small portion of the PU.
 *
 *---------------------------------------------------------------------*/

typedef enum Preg_Usage_Kind
{
   PREG_AS_UNKNOWN,
   PREG_AS_INT8,   /* FIRST_PREG_USAGE_KIND, SMALLEST_iPREG_USAGE_KIND */
   PREG_AS_UINT8,
   PREG_AS_INT16,
   PREG_AS_UINT16,
   PREG_AS_INT32,
   PREG_AS_UINT32,
   PREG_AS_INT64,
   PREG_AS_UINT64,  /* LARGEST_iPREG_USAGE_KIND */
   PREG_AS_IEEE32,
   PREG_AS_IEEE64,
   PREG_AS_QUAD,
   PREG_AS_C4,
   PREG_AS_C8,
   PREG_AS_CQ,
   PREG_AS_SPECIAL  /* LAST_PREG_USAGE_KIND */
} PREG_USAGE_KIND;

#define FIRST_PREG_USAGE_KIND     PREG_AS_INT8
#define SMALLEST_iPREG_USAGE_KIND FIRST_PREG_USAGE_KIND
#define LARGEST_iPREG_USAGE_KIND  PREG_AS_UINT64
#define LAST_PREG_USAGE_KIND      PREG_AS_SPECIAL

static const MTYPE Ukind_to_Mtype[LAST_PREG_USAGE_KIND+1] =
{
   MTYPE_UNKNOWN, /* PREG_AS_UNKNOWN */
   MTYPE_I1, /* PREG_AS_INT8 */
   MTYPE_U1, /* PREG_AS_UINT8 */
   MTYPE_I2, /* PREG_AS_INT16 */
   MTYPE_U2, /* PREG_AS_UINT16 */
   MTYPE_I4, /* PREG_AS_INT32 */
   MTYPE_U4, /* PREG_AS_UINT32 */
   MTYPE_I8, /* PREG_AS_INT64 */
   MTYPE_U8, /* PREG_AS_UINT64 */
   MTYPE_F4, /* PREG_AS_IEEE32 */
   MTYPE_F8, /* PREG_AS_IEEE64 */
   MTYPE_FQ, /* PREG_AS_QUAD */
   MTYPE_C4, /* PREG_AS_C4 */
   MTYPE_C8, /* PREG_AS_C8 */
   MTYPE_CQ, /* PREG_AS_CQ */
   MTYPE_UNKNOWN /* PREG_AS_SPECIAL */
};

#define MAX_OUTPART_LDIDS 32

typedef struct Preg_Info PREG_INFO;
struct Preg_Info
{
  BOOL       use[LAST_PREG_USAGE_KIND+1];  /* prepass analysis use-profile */
  BOOL       decl[LAST_PREG_USAGE_KIND+1]; /* preg declaration status */
  WN *       outpart_ldid[MAX_OUTPART_LDIDS]; /* outpart LDIDs associated with this
						 TIE packed preg */
  INT16      preg_num;                     /* preg number */
  PREG_INFO *next;                         /* next in table entry */
};
#define PREG_INFO_use(info, ukind) (info)->use[ukind]
#define PREG_INFO_decl(info, ukind) (info)->decl[ukind]
#define PREG_INFO_outpart_ldid(info, idx) (info)->outpart_ldid[idx]
#define PREG_INFO_preg_num(info) (info)->preg_num
#define PREG_INFO_next(info) (info)->next

#define PREG_INFO_HASH_TABLE_SIZE 73
#define PREG_INFO_HASH_IDX(offset) \
   (UINT32)(offset % PREG_INFO_HASH_TABLE_SIZE)

static PREG_INFO *Preg_Info_Hash_Tbl[PREG_INFO_HASH_TABLE_SIZE];
static PREG_INFO *Free_Preg_Info = NULL;


static BOOL
WN_in_ioitem(const WN *wn)
{
   /* This assumes we cannot have a region with an IO_ITEM.
    */
   BOOL found_io = FALSE, found_region = FALSE;
   
   wn = W2CF_Get_Parent(wn);
   while (wn != NULL && !found_io && !found_region)
   {
      if (WN_opc_operator(wn) == OPR_IO_ITEM)
	 found_io = TRUE;
      else if (WN_opc_operator(wn) == OPR_REGION)
	 found_region = TRUE;
      wn = W2CF_Get_Parent(wn);
   }
   return (found_io);
} /* WN_in_ioitem */


static PREG_USAGE_KIND 
Mtype_to_Ukind(MTYPE mtype)
{
   /* While PREG_USAGE_KIND --> MTYPE is a complete one-to-one 
    * mapping, and therefore can be represented as table, the 
    * reverse mapping is a partial mapping (e.g. we do not expect
    * to see MTYPE_V uses of pregs), and the safest way to handle
    * this is by means of a switch statment.
    */
   PREG_USAGE_KIND ukind;
   
   switch (mtype)
   {
   case MTYPE_I1: 
      ukind = PREG_AS_INT8;
      break;
   case MTYPE_U1: 
      ukind = PREG_AS_UINT8;
      break;
   case MTYPE_I2: 
      ukind = PREG_AS_INT16;
      break;
   case MTYPE_U2: 
      ukind = PREG_AS_UINT16;
      break;
   case MTYPE_I4: 
      ukind = PREG_AS_INT32;
      break;
   case MTYPE_U4: 
      ukind = PREG_AS_UINT32;
      break;
   case MTYPE_I8: 
      ukind = PREG_AS_INT64;
      break;
   case MTYPE_U8: 
      ukind = PREG_AS_UINT64;
      break;
   case MTYPE_F4: 
      ukind = PREG_AS_IEEE32;
      break;
   case MTYPE_F8: 
      ukind = PREG_AS_IEEE64;
      break;
   case MTYPE_FQ: 
      ukind = PREG_AS_QUAD;
      break;
   case MTYPE_C4: 
      ukind = PREG_AS_C4;
      break;
   case MTYPE_C8: 
      ukind = PREG_AS_C8;
      break;
   case MTYPE_CQ: 
      ukind = PREG_AS_CQ;
      break;
   default:
     ukind = PREG_AS_SPECIAL;
     // Is_True(FALSE, ("Illegal MTYPE for Mtype_to_Ukind mapping"));
     break;
   }
   return ukind;
} /* Mtype_to_Ukind */


static PREG_INFO *
Get_Preg_Info(INT16 preg_num)
{
   PREG_INFO *preg_info = NULL;

   /* Linear search for a matching entry in the hash table list */
   if (preg_num >= 0)
     for (preg_info = Preg_Info_Hash_Tbl[PREG_INFO_HASH_IDX(preg_num)];
          preg_info != NULL && PREG_INFO_preg_num(preg_info) != preg_num;
          preg_info = PREG_INFO_next(preg_info));
   return preg_info;
} /* Get_Preg_Info */


static PREG_INFO *
New_Preg_Info (INT16 preg_num)
{
  PREG_INFO *preg_info = NULL;
  
  /* Add a new entry to the hash-table */
  if (Free_Preg_Info == NULL)
    preg_info = TYPE_ALLOC_N(PREG_INFO, 1);
  else
    {
      preg_info = Free_Preg_Info;
      Free_Preg_Info = PREG_INFO_next(Free_Preg_Info);
    }
  
  /* Reset the usage and also set the other fields */
  for (INT usage_kind = (INT)FIRST_PREG_USAGE_KIND; 
       usage_kind <= (INT)LAST_PREG_USAGE_KIND; 
       usage_kind++)
    {
      PREG_INFO_decl(preg_info, usage_kind) = FALSE;
      PREG_INFO_use(preg_info, usage_kind) = FALSE;
    }
  for (INT idx = 0; idx < MAX_OUTPART_LDIDS; idx++)
    {
      PREG_INFO_outpart_ldid(preg_info, idx) = NULL;
    }
  PREG_INFO_preg_num(preg_info) = preg_num;
  PREG_INFO_next(preg_info) = 
    Preg_Info_Hash_Tbl[PREG_INFO_HASH_IDX(preg_num)];
  Preg_Info_Hash_Tbl[PREG_INFO_HASH_IDX(preg_num)] = preg_info;
  
  return preg_info;
}

static void
Accumulate_Preg_Info(TY_IDX preg_ty, INT16 preg_num)
{
   /* Given a preg with a certain number, update the information we
    * have about it.
    */
   PREG_INFO      *preg_info;
   INT             usage_kind;

   if (preg_num < 0)
     return;
   
   Is_True(TY_Is_Scalar(preg_ty), 
	   ("Expected KIND_SCALAR symbol in Accumulate_Preg_Info()"));
   
   /* Get the preg info record corresponding to this usage.  Create one
    * if none exists.
    */
   preg_info = Get_Preg_Info(preg_num);
   if (preg_info == NULL)
   {
     preg_info = New_Preg_Info(preg_num);
   }
   
   /* Record this usage */
   usage_kind = (INT)Mtype_to_Ukind(TY_mtype(preg_ty));
   PREG_INFO_use(preg_info, usage_kind) = TRUE;
} /* Accumulate_Preg_Info */


static void
Enter_Pregs_Into_Symtab(void)
{
   /* Assuming all preg_info has been accumulated and the PU symtab
    * has been pushed, this will enter the pregs accumulated into
    * the current symtab.  Note that this should be done prior to
    * translation of the PU, but typically after entering symbols for
    * other variables local to the PU.  The pregs entered into the 
    * symtab will be a superset of the pregs we actually need to declare.
    */
   TY_IDX          preg_ty;
   PREG_INFO      *preg_info;
   INT             hash_idx;
   INT             usage_kind;
   
   for (hash_idx = 0; hash_idx < PREG_INFO_HASH_TABLE_SIZE; hash_idx++)
      for (preg_info = Preg_Info_Hash_Tbl[hash_idx];
	   preg_info != NULL;
	   preg_info = PREG_INFO_next(preg_info))
      {
	/* Only define one (largest) preg-type for integral pregs
	 * at the same preg-number.
	 */
	for (usage_kind = (INT)LARGEST_iPREG_USAGE_KIND; 
	     (usage_kind >= (INT)SMALLEST_iPREG_USAGE_KIND && 
	      !PREG_INFO_use(preg_info, usage_kind));
	     usage_kind--);
	
	if (usage_kind >= (INT)SMALLEST_iPREG_USAGE_KIND)
	  {
	    preg_ty = Stab_Mtype_To_Ty(Ukind_to_Mtype[usage_kind]);
	    W2CF_Symtab_Nameof_Preg(preg_ty, PREG_INFO_preg_num(preg_info));
	  }
	
	/* Define all the non-integral pregs as separate variables */
	for (usage_kind = (INT)LARGEST_iPREG_USAGE_KIND + 1; 
	     usage_kind <= (INT)LAST_PREG_USAGE_KIND; 
	     usage_kind++)
	  {
	    if (PREG_INFO_use(preg_info, usage_kind))
	      {
		preg_ty = Stab_Mtype_To_Ty(Ukind_to_Mtype[usage_kind]);
		W2CF_Symtab_Nameof_Preg(preg_ty, PREG_INFO_preg_num(preg_info));
	      }
	  }
      } /* for each preg in this hash-table bucket */
} /* Enter_Pregs_Into_Symtab */


/*--------------- Information about the local PU symbols --------------
 *
 * We enter the local declarations for a PU into the name-disambiguation 
 * symbol table before attempting any translation of the PU.  This 
 * ensures that the symbol names remain invariant regardless whether or 
 * not we translate only a small portion of the PU.
 *---------------------------------------------------------------------*/

static void
Enter_Local_Syms_Into_Symtab(const ST *func_st)
{
   ST_IDX    st_idx;
   const ST *st;

   (void)W2CF_Symtab_Nameof_St(func_st);
   FOREACH_SYMBOL(CURRENT_SYMTAB, st, st_idx)
   {
      if ((ST_sym_class(st) == CLASS_VAR || ST_sym_class(st) == CLASS_FUNC) &&
	  !Stab_Is_Based_At_Common_Or_Equivalence(st))
      {
#ifdef BUILD_WHIRL2F
	 if (TY_Is_Pointer(ST_type(st)))
	    (void)W2CF_Symtab_Nameof_St_Pointee(st);
#endif /*BUILD_WHIRL2F*/
	 (void)W2CF_Symtab_Nameof_St(st);
      }
   }
} /* Enter_Local_Syms_Into_Symtab */


/*--------- Information about function call and return sites ---------
 * 
 * We accumulate information about call sites and the associated
 * stores of return-registers by means of an inorder traversal 
 * (Accumulate_Stmt_PUinfo) of the statement tree for a PU.  We try
 * to match an OPR_CALL (or OPR_ICALL or OPR_PICCALL or 
 * OPR_INTRINSIC_CALL) node, immediately followed by one or two 
 * OPR_ISTORE of return registers, with a pattern we can simplify.
 * E.g.:
 *
 *     (OPR_CALL f)
 *     (OPR_ISTORE (LDID return_preg1) (LDA "x"))
 *     (OPR_ISTORE (LDID return_preg2) (LDA "x"))
 *
 * can be translated into the C code:
 *
 *     x = f();
 *           
 * We also need to identify references to return registers outside
 * of the patterns we recognize, since such occurrences of return 
 * registers mean we must assign the appropriate values to the
 * corresponding "register" variables.  To handle this, we set the
 * call-site attribute "in_regs" during Accumulate_Expr_PUinfo()
 * to indicate that we have seen such an irregular reference to a
 * return register.
 *
 * Similarly, we accumulate information about the return sites and 
 * the associated store of a value into return-registers.
 *
 * For a function returning a struct, we expect the return-registers
 * to be stored into a (temporary) variable at the call sites.  At the
 * return sites we expect the values to be loaded from a (temporary) 
 * variable into the return-registers.  When these expectations are
 * met, we can eliminate the intermediate step of the loading from and
 * storing to return-registers, and instead return directly from/to the
 * (temporary) variable.
 *
 * For a function returning character strings, we always assume these
 * are returned through a first argument.
 *
 * A few utility routines, not to be called other than through
 * Append_CallSite() and Append_ReturnSite() are defined:
 *
 *   Var_Loaded_From:
 *      Get the ST and offset when a variable is loaded (LDID or ILOAD),
 *      returning a NULL ST when no variable is found to be loaded.
 *   Var_Stored_In:
 *      Get the ST and offset when a variable is stored (STID or ISTORE),
 *      returning a NULL ST when no variable is found to be stored into.
 *   Does_Stmt_Store_Into_Preg: 
 *      Return the given wn when it stores into a preg with the given
 *      preg_num.
 *   Does_Stmt_Store_From_Preg: 
 *      Return the given wn when it stores from a preg with the given
 *      preg_num.
 *   New_CallSite:
 *      Creates a new CALLSITE entry, and appends it to the end of
 *      the CALLSITE list.
 *   New_ReturnSite:
 *      Creates a new RETURNSITE entry, and appends it to the end of
 *      the RETURNSITE list.
 *
 * The interface to record CALLSITE and RETURNSITE is as follows:
 *
 *    Append_CallSite:
 *       Invariably appends a new CALLSITE to our list of CALLSITEs.
 *       If a pattern match for storing the function-call value to an
 *       lvalue is found, then we note this in the new CALLSITE.
 *
 *    Append_ReturnSite:
 *       Appends a new RETURNSITE to our list of RETURNSITEs,
 *       provided a (possibly empty) sequence of stores from an 
 *       lvalue to the return-registers preceeds an OPR_RETURN
 *       statement.  We should end up with one RETURNSITE per
 *       OPR_RETURN statement.
 *
 *---------------------------------------------------------------------*/

/* Safer tree walking routines than those provided through
 * common/com/wn_util.h
 */
#define PUINFO_WN_ITER_wn(iter) \
   (iter != NULL? WN_ITER_wn(iter) : NULL)
#define PUINFO_WN_WALK_TreeNext(iter) \
   (iter != NULL? WN_WALK_TreeNext(iter) : NULL)
#define PUINFO_WN_WALK_StmtNext(iter) \
   (iter != NULL? WN_WALK_StmtNext(iter) : NULL)

static CALLSITE *CallSite_First = NULL; /* First entry in list */
static CALLSITE *CallSite_Last = NULL;  /* Last entry in list */
static CALLSITE *CallSite_Free = NULL;  /* Free list */

static RETURNSITE *ReturnSite_First = NULL; /* First entry in list */
static RETURNSITE *ReturnSite_Last = NULL;  /* Last entry in list */
static RETURNSITE *ReturnSite_Free = NULL;  /* Free list */

static TIEOPSITE *TieOpSite_First = NULL; /* First entry in list */
static TIEOPSITE *TieOpSite_Last = NULL;  /* Last entry in list */
static TIEOPSITE *TieOpSite_Free = NULL;  /* Free list */

static WN_LIST *tie_ops = NULL;

void wn_list_push (WN_LIST **wnla, const WN *wn) {
  WN_LIST *new_node = TYPE_ALLOC_N(WN_LIST,1);
  WN_LIST_wn(new_node)=wn;
  WN_LIST_next(new_node)=*wnla;
  *wnla=new_node;
}

WN *wn_list_pop (WN_LIST **wnla) {
  WN *wn = NULL;
  WN_LIST *first = *wnla;
  if (first) {
    wn=(WN *)WN_LIST_wn(first);
    *wnla=WN_LIST_next(first);
    FREE(first);
  }
  return wn;
}

BOOL wn_list_find (WN_LIST *wnl, const WN *wn) {
  while (wnl) {
    if (WN_LIST_wn(wnl)==wn)
      return TRUE;
    wnl=WN_LIST_next(wnl);
  }
  return FALSE;
}

static void
Var_Loaded_From(const WN    *wn,     /* in: arbitrary expression */
		const ST   **st,     /* out: var loaded from (NULL if none) */
		STAB_OFFSET *offset) /* out: offset of var loaded from */
{
   Is_True(wn != NULL, ("wn==NULL in Var_Loaded_From()"));
   if (WN_opc_operator(wn) == OPR_LDID)
   {
      *st = WN_st(wn);
      *offset = WN_load_offset(wn);
   }
   else if (WN_opc_operator(wn) == OPR_ILOAD &&
	    WN_opc_operator(WN_kid0(wn)) == OPR_LDA)
   {
      *st = WN_st(WN_kid0(wn));
      *offset = WN_load_offset(wn) + WN_lda_offset(WN_kid0(wn));
   }
   else
   {
      *st = NULL;
      *offset = -1;
   }
} /* Var_Loaded_From */


static void
Var_Stored_In(const WN    *wn,     /* in: arbitrary expression */
	      const ST   **st,     /* out: var stored into (NULL if none) */
	      STAB_OFFSET *offset) /* out: offset of var stored into */
{
   Is_True(wn != NULL, ("wn==NULL in Var_Stored_In()"));
   if (WN_opc_operator(wn) == OPR_STID)
   {
      *st = WN_st(wn);
      *offset = WN_store_offset(wn);
   }
   else if (WN_opc_operator(wn) == OPR_ISTORE &&
	    WN_opc_operator(WN_kid1(wn)) == OPR_LDA)
   {
      *st = WN_st(WN_kid1(wn));
      *offset = WN_store_offset(wn) + WN_lda_offset(WN_kid1(wn));
   }
   else
   {
      *st = NULL;
      *offset = -1;
   }
} /* Var_Stored_In */


static const WN *
Does_Stmt_Store_From_Preg(const WN *wn, STAB_OFFSET preg_num)
{
   if ((WN_opc_operator(wn) == OPR_STID || 
	WN_opc_operator(wn) == OPR_ISTORE)            &&
       WN_opc_operator(WN_kid0(wn)) == OPR_LDID       &&
       ST_sym_class(WN_st(WN_kid0(wn))) == CLASS_PREG &&
       WN_load_offset(WN_kid0(wn)) == preg_num)
      return wn;
   else
      return NULL;
} /* Does_Stmt_Store_From_Preg */


static const WN *
Does_Stmt_Store_Into_Preg(const WN *wn, STAB_OFFSET preg_num)
{
   if (WN_opc_operator(wn) == OPR_STID       &&
       ST_sym_class(WN_st(wn)) == CLASS_PREG &&
       WN_store_offset(wn) == preg_num)
      return wn;
   else
      return NULL;
} /* Does_Stmt_Store_Into_Preg */


static CALLSITE *
New_CallSite(const WN   *call_wn,
	     TY_IDX      return_ty,  /* Return type of the called function */
	     const WN   *store_wn[], /* Stores from return registers */
	     const ST   *return_var, /* Returned value is stored in this ST */
	     STAB_OFFSET var_offset) /* ... at this offset */
{
   CALLSITE *callsite;

   if (CallSite_Free != NULL)
   {
      callsite = CallSite_Free;
      CallSite_Free = CALLSITE_next(CallSite_Free);
   }
   else
   {
      callsite = TYPE_ALLOC_N(CALLSITE, 1);
   }
   if (CallSite_Last == NULL)
   {
      CallSite_Last = callsite;
      CallSite_First = callsite;
   }
   else
   {
      CALLSITE_next(CallSite_Last) = callsite;
      CallSite_Last = callsite;
   }
   CALLSITE_call(callsite) = call_wn;
   CALLSITE_return_ty(callsite) = return_ty;
   for (INT i = 0; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)
     CALLSITE_store(callsite, i) = store_wn[i];
   CALLSITE_return_var(callsite) = return_var;
   CALLSITE_var_offset(callsite) = var_offset;
   CALLSITE_in_regs(callsite) = FALSE;
   CALLSITE_next(callsite) = NULL;

   return callsite;
} /* New_CallSite */


// scan_for_tie_ops scans in post-order recursively and parallely the given wn and
// new_wn trees (they should be copies of each other) for special TIE intrinsic
// operations, and collects setup information for them in setup_block.
// it also accumulates any processed intrinsic op whirl nodes in the tie_ops list.
//
// if wn/new_wn is a special TIE op, the function returns LDID whirl node from
// the register that the setup block stores the result of the op into, otherwise
// returns NULL.
//
// any kids of new_wn that depend on new registers in the setup block will also
// be updated.

static WN *scan_for_tie_ops (WN *wn, WN *new_wn, TIEOPSITE *tie_op_site) {
  if (!wn) 
    return NULL;

  Is_True((WN_opcode(wn) != OPC_BLOCK),
	  ("Bad intrinsic op expression!"));
  
  for (int i = 0; i< WN_kid_count(wn); i++) {
    WN *return_wn = scan_for_tie_ops(WN_kid(wn,i),WN_kid(new_wn,i),tie_op_site);
    if (return_wn)
      WN_kid(new_wn,i)=return_wn;
  }
  
  if (WN_operator(wn)!=OPR_INTRINSIC_OP)
    return NULL;

  INTRINSIC id = (INTRINSIC) WN_intrinsic(wn);
  if (!INTRN_is_tie_intrinsic(id))
    return NULL;

  TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(id);
  TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
  if (tie_macro->is_c_function())
    return NULL;
  
  INT num_operands = tie_macro->num_protos();
  Is_True(num_operands > 0,
	  ("expected TIE intrinsic op with at least one argument"));

  WN *setup_block = TIEOPSITE_setup_block(tie_op_site);
  wn_list_push(&tie_ops, wn);

  TYPE_ID ret_type = tie_macro->output_mtype(tie_info);
  if (MTYPE_is_tie_packed(ret_type))
    {
      const WN *parent = W2CF_Get_Parent(wn);
      FmtAssert(parent != NULL &&
		WN_operator(parent) == OPR_STID &&
		ST_sym_class(WN_st(parent)) == CLASS_PREG,
		("Unhandled TIE op in whirl2c."));

      PREG_NUM packed_preg = WN_store_offset(parent);
      PREG_INFO *preg_info = Get_Preg_Info(packed_preg);
      if (preg_info == NULL)
	preg_info = New_Preg_Info(packed_preg);
      
      WN **parms = TYPE_ALLOC_N(WN *, num_operands);
      INT in_wn_idx = 0;
      INT outpart_idx = 0;
      for (INT oper_idx = 0; oper_idx < num_operands; oper_idx++)
	{
	  if (tie_macro->proto_is_in(oper_idx))
	    {
	      parms[oper_idx] = WN_kid(new_wn, in_wn_idx);
	      in_wn_idx++;
	      continue;
	    }

	  TYPE_ID oper_mtype = tie_macro->proto_mtype_id(tie_info, oper_idx);
	  PREG_NUM arg_preg = Create_Preg(oper_mtype, WHIRL2C_TIE_TEMP_ID);
	  if (tie_macro->proto_is_inout(oper_idx))
	    {
	      WN* param_ldid = WN_kid0(WN_kid(new_wn, in_wn_idx));
	      in_wn_idx++;
	      
	      WN* param_stid =
		WN_StidIntoPreg(oper_mtype, arg_preg, MTYPE_To_PREG(oper_mtype),
				param_ldid);
	      WN_INSERT_BlockLast(setup_block, param_stid);
	    }
	  
	  WN *ldid_preg = WN_LdidPreg(oper_mtype, arg_preg);
	  WN *parm = WN_CreateParm(oper_mtype, ldid_preg,
				   MTYPE_To_TY(oper_mtype), WN_PARM_BY_VALUE);
	  parms[oper_idx] = parm;
	  
	  PREG_INFO_outpart_ldid(preg_info, outpart_idx) = ldid_preg;
	  outpart_idx++;
	}
      
      OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
      WN *call_node = WN_Create_Intrinsic(intrin_op, id, num_operands, parms);
      WN_INSERT_BlockLast(setup_block, call_node);
      
      return NULL;
    }
  else
    {
      OPCODE intrin_op = OPCODE_make_op(OPR_INTRINSIC_CALL, MTYPE_V, MTYPE_V);
      WN *call_node = WN_Create_Intrinsic(intrin_op, id,
					  WN_kid_count(new_wn),
					  &WN_kid(new_wn, 0));
      
      TYPE_ID proto_mtype = tie_macro->proto_mtype_id(tie_info, 0);
      PREG_NUM arg_preg = Create_Preg(proto_mtype, WHIRL2C_TIE_TEMP_ID);
      WN* param_ldid = WN_kid0(WN_kid(call_node, 0));
      WN* param_stid =
	WN_StidIntoPreg(proto_mtype, arg_preg, MTYPE_To_PREG(proto_mtype),
			param_ldid);
      WN_INSERT_BlockLast(setup_block, param_stid);
      WN_INSERT_BlockLast(setup_block, call_node);
      
      WN *ldid_preg = WN_LdidPreg(proto_mtype, arg_preg);
      WN_kid0(WN_kid(call_node, 0)) =ldid_preg;
      
      WN_Delete(new_wn);
      
      return WN_COPY_Tree(ldid_preg);
    }
} // scan_for_tie_ops


static TIEOPSITE *
New_TieOpSite (const WN   *iop_wn,
	       const WN   *stmt) {
  TIEOPSITE *tiesite;
  
  if (TieOpSite_Free != NULL) {
    tiesite = TieOpSite_Free;
    TieOpSite_Free = TIEOPSITE_next(TieOpSite_Free);
  } else {
    tiesite = TYPE_ALLOC_N(TIEOPSITE, 1);
  }
  if (TieOpSite_Last == NULL) {
    TieOpSite_Last = tiesite;
    TieOpSite_First = tiesite;
  } else {
    TIEOPSITE_next(TieOpSite_Last) = tiesite;
    TieOpSite_Last = tiesite;
  }
  
  WN *copy_wn = WN_COPY_Tree((WN *)iop_wn);
  WN *setup_block = WN_CreateBlock();
  
  TIEOPSITE_iop_wn(tiesite) = iop_wn;
  TIEOPSITE_stmt(tiesite) = stmt;
  TIEOPSITE_setup_block(tiesite) = setup_block;
  TIEOPSITE_ldid_new_preg(tiesite) = scan_for_tie_ops((WN *)iop_wn, copy_wn, tiesite);
  TIEOPSITE_next(tiesite) = NULL;

  return tiesite;
} /* New_TieOpSite */

static void
Free_TieOpSite_Data (TIEOPSITE *tiesite) {
  WN_DELETE_Tree(TIEOPSITE_setup_block(tiesite));
  TIEOPSITE_setup_block(tiesite)=NULL;
  WN_DELETE_Tree(TIEOPSITE_ldid_new_preg(tiesite));
  TIEOPSITE_ldid_new_preg(tiesite)=NULL;
  return;
}

static RETURNSITE *
New_ReturnSite(const WN   *return_wn,
	       const WN   *store_wn[],  /* Stores into return registers */
	       const ST   *return_var,  /* Return regs loaded from this ST */
	       STAB_OFFSET var_offset)  /* ... at this offset */
{
   RETURNSITE *return_info;

   if (ReturnSite_Free != NULL)
   {
      return_info = ReturnSite_Free;
      ReturnSite_Free = RETURNSITE_next(ReturnSite_Free);
   }
   else
   {
      return_info = TYPE_ALLOC_N(RETURNSITE, 1);
   }
   if (ReturnSite_Last == NULL)
   {
      ReturnSite_Last = return_info;
      ReturnSite_First = return_info;
   }
   else
   {
      RETURNSITE_next(ReturnSite_Last) = return_info;
      ReturnSite_Last = return_info;
   }
   RETURNSITE_return(return_info) = return_wn;
   for (INT i = 0; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)
     RETURNSITE_store(return_info, i) = store_wn[i];
   RETURNSITE_return_var(return_info) = return_var;
   RETURNSITE_var_offset(return_info) = var_offset;
   RETURNSITE_next(return_info) = NULL;

   return return_info;
} /* New_ReturnSite */


static WN_ITER *
Append_CallSite(WN_ITER *stmt_iter, const WN *next_stmt)
{
   /* The "next_stmt" is initially an OPR_CALL, OPR_ICALL, OPR_PICCALL
    * or OPR_INTRINSIC_CALL node, while "wn_iter" is set to point 
    * to the next statement node in sequence.
    */
   TY_IDX                    return_ty;    /* Return type of called function */

   RETURN_PREG               return_preg;  /* Info about return registers */
   const RETURN_PREG * const return_preg_ptr = &return_preg;

   /* Var to hold return value of call */
   const ST    *save_var[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0}; 
   
   /* Offset of value in save_var */
   STAB_OFFSET  save_offset[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0}; 
   
   /* Storing return registers */
   const WN    *store[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0};

   const WN    *const call_wn = next_stmt; /* The call node */

   BOOL         is_tie_intrinsic = FALSE;

   BOOL         all_stores_ok = TRUE;

   /* Get the function return type,
      or all information if it is a tie intrinsic call */
   if (WN_operator(call_wn) == OPR_CALL || 
       WN_operator(call_wn) == OPR_PICCALL) {
     Is_True(WN_entry_name(call_wn) != 0, 
	     ("Missing WN_entry_name() for %s", 
	      OPCODE_name(WN_opcode(call_wn))));
     return_ty =
       Func_Return_Type(ST_pu_type(&St_Table[WN_entry_name(call_wn)]));
   }
   else if (WN_operator(call_wn) == OPR_ICALL) {
     /* Used to be:
      *
      *    TY_pointed(
      *       WN_Tree_Type(WN_kid(call_wn, WN_kid_count(call_wn) - 1))));
      */
     return_ty = Func_Return_Type(WN_ty(call_wn));
   }
   else {
     Is_True(WN_operator(call_wn) == OPR_INTRINSIC_CALL, 
	     ("Expected OPR_INTRINSIC_CALL node in Append_CallSite()"));
     INTRINSIC id = (INTRINSIC)WN_intrinsic(call_wn);
     
     if (INTRN_is_tie_intrinsic(id)) {
       TYPE_ID type;
       TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(id);
       TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
       if (tie_macro->no_output() ||
	   (!tie_macro->is_c_function() &&
	    !tie_macro->is_conditional_branch())) {
	 // no C semantics return value
	 type = MTYPE_V;
       } else {
	 // get the return type from the subsequent store from the
	 // tie output register, and fill in all necessary information
	 store[0] = PUINFO_WN_ITER_wn(stmt_iter);
	 stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter); /* next statement */
	 FmtAssert(store[0] &&
		   (WN_operator(store[0])==OPR_STID ||
		    WN_operator(store[0])==OPR_ISTORE),
		   ("Internal error: missing STID after TIE intrinsic call"));
	 WN *return_ldid = WN_kid0(store[0]);
	 FmtAssert(return_ldid && WN_operator(return_ldid)==OPR_LDID,
		   ("Internal error: missing return value LDID after TIE intrinsic call"));
	 FmtAssert(WN_st(return_ldid)== Tie_Output_Volatile_Preg &&
		   Preg_Is_Dedicated(WN_offset(return_ldid)),
		   ("Internal error: missing return value LDID after TIE intrinsic call"));
	 type = WN_desc(store[0]);
	 Var_Stored_In(store[0], &save_var[0], &save_offset[0]);
       }
       return_ty = Stab_Mtype_To_Ty(type);
       Is_True(return_ty,
	       ("Cannot find the return type of the TIE intrinsic call"));
       is_tie_intrinsic = TRUE;
     } else {
       return_ty = WN_intrinsic_return_ty(WN_opcode(call_wn), 
					  id,
					  call_wn);
     }
   }
   
   if (!is_tie_intrinsic) {

     /* Determine how the return value is distributed between return
      * registers to hold the value of the function call.
      */
     return_preg = PUinfo_Get_ReturnPreg(return_ty, Return_Info_Incoming);
     
     /* Update next_stmt to denote the statement following the call_wn */
     next_stmt = PUINFO_WN_ITER_wn(stmt_iter);
     
     /* We want to check that there are exactly RETURN_PREG_num_pregs
      * stores from return registers, all storing to the same base
      * with increasing offsets.  */
     for (UINT i = 0; i < RETURN_PREG_num_pregs(return_preg_ptr); i++)
     {
       if (next_stmt == NULL)
       {
         all_stores_ok == FALSE;
         break;
       }
       
       /* Check if this is a store from a return register */
       store[i] = 
	 Does_Stmt_Store_From_Preg(next_stmt,
				   RETURN_PREG_offset(return_preg_ptr, i));

       if (store[i] == NULL)
       {
         all_stores_ok = FALSE;
         break;
       }
       
       /* Check if the return register is stored off into a variable */
       Var_Stored_In(next_stmt, &save_var[i], &save_offset[i]);
	 
       if (i > 0 &&
           (save_var[i] == NULL ||
            ST_sym_class(save_var[i]) == CLASS_PREG ||
            save_var[i] != save_var[i-1] ||
            save_offset[i] <= save_offset[i-1]))
       {
         all_stores_ok = FALSE;
         break;
       }
       
       stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter); /* next statement */
       next_stmt = PUINFO_WN_ITER_wn(stmt_iter);
     }

     if (!all_stores_ok)
     {
       // clear any partially matching stores
       save_var[0] = NULL;
       save_offset[0] = 0;
       for (INT i = 0; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)       
         store[i] = NULL;
     }
     
   } /* !is_tie_intrinsic */

   (void)New_CallSite(call_wn, return_ty, store, save_var[0], save_offset[0]);
   
   return stmt_iter;
} /* Append_CallSite */


CALLSITE *PUinfo_find_callsite (const CALLSITE *callsite, const WN *call_wn) {
  while (callsite && CALLSITE_call(callsite)!=call_wn)
    callsite=CALLSITE_next(callsite);
  return (CALLSITE *)callsite;
}

static WN_ITER *
Append_ReturnSite(WN_ITER *stmt_iter, const WN *first_stmt)
{
   /* The "next_stmt" is an OPR_RETURN, or an OPR_STID node; 
    * "stmt_iter" points to the next statement node in sequence.  */

   const WN    *stmt = first_stmt; /* Current statement */

   /* Variable holding return value */
   const ST    *load_var[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0};

   /* Offset of value in load_var */
   STAB_OFFSET  load_offset[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0};

   /* Storing return registers */
   const WN    *store[MAX_NUMBER_OF_REGISTERS_FOR_RETURN] = {0};

   BOOL         all_stores_ok = TRUE;

   if (WN_operator(stmt) == OPR_STID)
   {
     /* We want to check that there are exactly RETURN_PREG_num_pregs
      * stores into return registers, all loading from the same base
      * with increasing offsets.  */
     for (UINT i = 0; i < RETURN_PREG_num_pregs(PUinfo_return_preg); i++)
     {
       /* Check if this is a store into a return register */
       store[i] = 
         Does_Stmt_Store_Into_Preg(stmt, 
                                   RETURN_PREG_offset(PUinfo_return_preg, i));
       if (store[i] == NULL)
       {
         all_stores_ok = FALSE;
         break;
       }
       
       /* Check if the return register is set from a variable */
       Var_Loaded_From(WN_kid0(stmt), &load_var[i], &load_offset[i]);

       /* Update next_stmt to denote the stmt following the first store */
       stmt = PUINFO_WN_ITER_wn(stmt_iter);

       if (stmt == NULL ||
           (i > 0 && 
            (load_var[i] == NULL ||
             ST_sym_class(load_var[i]) == CLASS_PREG ||
             load_var[i] != load_var[i-1] ||
             load_offset[i] <= load_offset[i-1])))
       {
         all_stores_ok = FALSE;
         break;
       }

       if (RETURN_PREG_num_pregs(PUinfo_return_preg) > 1)
       {
         stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter); /* go to next stmt */
       }
     }
   }

   if (!all_stores_ok)
   {
     // reset the statement and clear any partially matching stores
     stmt = first_stmt;
     load_var[0] = NULL;
     load_offset[0] = 0;
     for (INT i = 0; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)       
       store[i] = NULL;
   }
     
   /* See if we have a return as the next statement */
   if (stmt != NULL && WN_operator(stmt) == OPR_RETURN)
   {
      (void)New_ReturnSite(stmt, store, load_var[0], load_offset[0]);
       
      if (stmt != first_stmt)
	 stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter);
   }

   return stmt_iter;
} /* Append_ReturnSite */

static BOOL
Is_Stmt_CallSite_Store(const WN *stmt, const CALLSITE *callsite)
{
  for (INT i = 0; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)
    if (stmt == CALLSITE_store(callsite, i))
      return TRUE;

  return FALSE;
}

/*------- Prepass phases over WN tree to record information -----------*/
/*---------------------------------------------------------------------*/

static void
Accumulate_Stmt_PUinfo(WN *wn)
{
   /* This is a preorder prepass over all the statements rooted at wn, 
    * before we actually begin the translation to C.  We accumulate
    * information about function calls and return statements.
    */
   WN_ITER  *stmt_iter = WN_WALK_StmtIter(wn);
   const WN *parent;
   const WN *gparent;

   /* Iterate over all statements */
   while (stmt_iter != NULL)
   {
      const WN *stmt = PUINFO_WN_ITER_wn(stmt_iter);

      if (stmt != NULL)
      {
	 switch(WN_opc_operator(stmt))
	 {
	 case OPR_CALL:
	 case OPR_ICALL:
	 case OPR_PICCALL:
	 case OPR_INTRINSIC_CALL:
	    /* Ignore C++ calls in WN_region_pragmas().  This iterator will
	     * currently ignore calls in IO_ITEMs, but we check explicitly
	     * anyway to safeguard against future changes in the iterator 
	     * definition.
	     */
	    parent = W2CF_Get_Parent(stmt);
	    gparent = W2CF_Get_Parent(parent);
	    if (!WN_in_ioitem(stmt) &&
		(WN_opc_operator(gparent) != OPR_REGION ||
		 WN_region_pragmas(gparent) != parent))
	    {
	       /* Note that here we will skip the call node and any
		* sequence of stores that fits the form expected for a 
		* call statement.
		*/
	       stmt_iter = 
		  Append_CallSite(PUINFO_WN_WALK_StmtNext(stmt_iter), stmt);
	    }
	    else
	       stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter);
	    break;

	 case OPR_STID:
	    /* Note that here we will skip any sequence of stores that
	     * fits the form expected for a return statement, including
	     * the OPR_RETURN we expect to immediately follow the stores.
	     */
	    stmt_iter = 
	       Append_ReturnSite(PUINFO_WN_WALK_StmtNext(stmt_iter), stmt);
	    break;

	 case OPR_RETURN:
	    /* A return statement without the expected sequence of
	     * preceeding store operations.
	     */
	    stmt_iter = 
	       Append_ReturnSite(PUINFO_WN_WALK_StmtNext(stmt_iter), stmt);
	    break;

	 default:
	    stmt_iter = PUINFO_WN_WALK_StmtNext(stmt_iter);
	    break;
	 }
      }
   } /* while */
} /* Accumulate_Stmt_PUinfo */


static void
Accumulate_Expr_PUinfo(WN *root)
{
   /* This is a preorder prepass over all the nodes rooted at the wn,
    * including statements and expressions.  We accumulate information
    * about return-register and other preg uses.  NOTE: we expect this
    * to be done after having done Accumulate_Stmt_Info(wn)!
    */
   WN_ITER  *wn_iter;
   const WN *wn;
   const WN *parent;
   const WN *gparent;
   const WN *next_return_ldid = NULL;
   CALLSITE *last_callsite = NULL;

   WN *last_stmt = NULL;
   
   /* Walk the entire tree to get expression-level information */
   for (wn_iter = WN_WALK_TreeIter(root); 
	wn_iter != NULL;
	wn_iter = PUINFO_WN_WALK_TreeNext(wn_iter))
   {
      /* Record preg usage; even record uses that will be eliminated by
       * simplification of call-/return-sites.  Also, note any unexpected
       * uses of return-registers (only LDID uses, not STID uses) for 
       * every call-site.
       */
     wn = PUINFO_WN_ITER_wn(wn_iter);
     if (OPCODE_is_stmt(WN_opcode(wn)))
       last_stmt = (WN *)wn;
     
	 switch(WN_opc_operator(wn))
	 {
	 case OPR_STID:
	   if (ST_sym_class(WN_st(wn)) == CLASS_PREG) {
	     Accumulate_Preg_Info(ST_type(WN_st(wn)), WN_store_offset(wn));

	     // Handle copying of TIE packed type pregs: propagate
	     // the necessary information from the source to the destination
	     // register. Note that this may not work with control flow when
	     // we haven't processed the source preg yet.
	     TYPE_ID type = WN_desc(wn);
	     if (MTYPE_is_tie_packed(type)) {
	       WN *kid = WN_kid0(wn);
	       if (WN_operator(kid) == OPR_LDID &&
		   ST_sym_class(WN_st(kid)) == CLASS_PREG &&
		   WN_rtype(kid) == type && WN_desc(kid) == type) {
		 INT16 store_preg = WN_store_offset(wn);
		 INT16 load_preg = WN_load_offset(kid);
		 
		 PREG_INFO *load_preg_info = Get_Preg_Info(load_preg);
		 Is_True(load_preg_info != NULL, ("Can't find load preg info"));

		 PREG_INFO *store_preg_info = Get_Preg_Info(store_preg);
		 Is_True(store_preg_info != NULL, ("Can't find store preg info"));
		 
		 for (INT idx = 0; idx < MAX_OUTPART_LDIDS; idx++)
		   {
		     PREG_INFO_outpart_ldid(store_preg_info, idx) =
		       PREG_INFO_outpart_ldid(load_preg_info, idx);
		   }
	       }
	     }
	   }

	   /* Fall through to the OPR_ISTORE case, where we indicate
	    * which LDID node is next expected to load a return-register,
	    * based on the information we have retrieved about the last
	    * OPR_CALL statement ...
	    */


	 case OPR_ISTORE:
	    if (last_callsite != NULL && 
                Is_Stmt_CallSite_Store(wn, last_callsite))
	    {
	       /* Next expected ldid of a return register is WN_kid0(tree) */
	       Is_True(WN_operator(WN_kid0(wn)) == OPR_LDID &&
		       (IS_RETURN_PREG(WN_kid0(wn)) ||
			WN_st(WN_kid0(wn)) == Tie_Output_Volatile_Preg),
		       ("Unexpected STID/ISTORE in Accumulate_Expr_PUinfo()"));

	       next_return_ldid = WN_kid0(wn);
	    }
	    break;
	    
	 case OPR_LDID:
	    if (ST_sym_class(WN_st(wn)) == CLASS_PREG)
	    {
	       Accumulate_Preg_Info(ST_type(WN_st(wn)), WN_load_offset(wn));

	       /* If we encounter an unexpected load of a return
		* register, then update the CALLSITE to indicate that
		* the return-registers must be written for the call.
		*/
                
	       if (next_return_ldid == wn)
		 next_return_ldid = NULL;
	       else if (last_callsite != NULL && IS_RETURN_PREG(wn)) 
		   CALLSITE_in_regs(last_callsite) = TRUE;
	    }
	    break;

	 case OPR_LDA:
	    /* Make certain this never occurs for pregs! */
	    Is_True(ST_sym_class(WN_st(wn)) != CLASS_PREG,
		     ("Attempt to LDA a PREG in Accumulate_Expr_PUinfo()"));
	    break;

	 case OPR_CALL:
	 case OPR_ICALL:
	 case OPR_PICCALL:
	 case OPR_INTRINSIC_CALL:
	    /* Ignore C++ calls in WN_region_pragmas() and calls in IO items.
	     */
	    parent = W2CF_Get_Parent(wn);
	    gparent = W2CF_Get_Parent(parent);
	    if (!WN_in_ioitem(wn) &&
		(WN_opc_operator(gparent) != OPR_REGION ||
		 WN_region_pragmas(gparent) != parent))
	    {
	       /* Note that we need not accumulate information about return
		* pregs here, since such information will be appropriately
		* accumulated when we encounter such references in LDID or 
		* STID nodes.
		*/
	       if (last_callsite == NULL)
		  last_callsite = PUinfo_Get_CallSites();
	       else
		  last_callsite = CALLSITE_next(last_callsite);

	       Is_True(CALLSITE_call(last_callsite) == wn,
		  ("Unexpected callsite order in Accumulate_Expr_PUinfo()"));
	    }
	    break;

	 case OPR_RETURN:
	    /* Make certain the appropriate return-registers are defined,
	     * since it may be that a function returns an undefined value
	     * and no explicit references to the return register occurs in
	     * the WHIRL representation.
	     */
	    if (RETURN_PREG_num_pregs(PUinfo_return_preg) > 0)
	    {
	       TY_IDX      preg_ty;
	       STAB_OFFSET preg_offset;

	       preg_offset = RETURN_PREG_offset(PUinfo_return_preg, 0);
	       preg_ty = 
		  Stab_Mtype_To_Ty(RETURN_PREG_mtype(PUinfo_return_preg, 0));
	       Accumulate_Preg_Info(preg_ty, preg_offset);
	       if (RETURN_PREG_num_pregs(PUinfo_return_preg) > 1)
	       {
		  preg_offset = RETURN_PREG_offset(PUinfo_return_preg, 1);
		  preg_ty = 
		    Stab_Mtype_To_Ty(RETURN_PREG_mtype(PUinfo_return_preg, 1));
		  Accumulate_Preg_Info(preg_ty, preg_offset);
	       }
	    }
	    break;
	    
	 case OPR_INTRINSIC_OP:
	   {
	     // For special TIE intrinsic ops that need to be represented
	     // in the source as void C functions, create setup blocks
	     // and associate them with the current statement
	     INTRINSIC id = (INTRINSIC)WN_intrinsic(wn);
	     if (INTRN_is_tie_intrinsic(id)) {
	       TIE_MACRO_ID tie_macro_id = Intrinsic_To_Tie_Macro_Id(id);
	       TIE_MACRO_p tie_macro = tie_info->tie_macro(tie_macro_id);
	       if (!tie_macro->is_c_function() &&
		   !wn_list_find(tie_ops,wn)) {
		 while(tie_ops)
		   wn_list_pop(&tie_ops);
		 (void)New_TieOpSite(wn,last_stmt);
	       }
	     }
	     
	   }
	   break;
	   
	 default:
	   break;
	 } /* switch on node OPR */
   } /* for each node */
} /* Accumulate_Expr_PUinfo */


/*------------------------ exported routines --------------------------*/
/*---------------------------------------------------------------------*/

void 
PUinfo_initialize(void)
{
   Is_True(PUinfo_local_decls == NULL && 
	   PUinfo_current_func == NULL &&
	   PUinfo_return_preg == NULL,
	   ("Unexpected state in PUinfo_init()"));
   
} /* PUinfo_initialize */


void 
PUinfo_finalize(void)
{
   /* Free up all memory */
   CALLSITE   *callsite;
   RETURNSITE *returnsite;
   TIEOPSITE *tiesite;

   /* Free up the entries in the CallSite_Free list */
   while (CallSite_Free != NULL)
   {
      callsite = CallSite_Free;
      CallSite_Free = CALLSITE_next(callsite);
      FREE(callsite);
   }

   /* Free up the entries in the ReturnSite_Free list */
   while (ReturnSite_Free != NULL)
   {
      returnsite = ReturnSite_Free;
      ReturnSite_Free = CALLSITE_next(returnsite);
      FREE(returnsite);
   }

   while (TieOpSite_Free != NULL) {
     tiesite = TieOpSite_Free;
     TieOpSite_Free = TIEOPSITE_next(tiesite);
     FREE(tiesite);
   }
} /* PUinfo_finalize */


void 
PUinfo_init_pu(const WN *pu, WN *body_part_of_interest)
{
   /* TODO: Handle nested procedures and call/return list stacks.
    * NOTE: This should never cause side-effects to the incoming
    *       PU (e.g. creating a pointer type), without making
    *       sure this is handled by W2C_Pop_Pu() in w2c_driver.c
    *       and W2F_Pop_Pu() in w2f_driver.c.xc
    */
   static RETURN_PREG preg_info;

   Is_True(WN_operator(pu) == OPR_FUNC_ENTRY,
	   ("Expected an OPR_FUNC_ENTRY node in PUinfo_init()"));

   /* Set the PU state: MUST BE DONE BEFORE ANYTHING ELSE! */
   Is_True(PUinfo_local_decls  == NULL &&
           PUinfo_pragmas      == NULL && 
	   PUinfo_current_func == NULL &&
	   PUinfo_return_preg  == NULL,
	   ("Unexpected state in PUinfo_init_pu()"));

   PUinfo_current_func = pu; /* PUINFO_RETURN_TY uses this! */
   preg_info = PUinfo_Get_ReturnPreg(PUINFO_RETURN_TY,Return_Info_Outgoing);
   PUinfo_return_preg  = &preg_info;
   PUinfo_local_decls  = New_Token_Buffer();
   PUinfo_pragmas  = New_Token_Buffer();

   /* Traverse the statements in the current PU, accumulating 
    * information about every function call-site and every function
    * return-site.
    */
   if (!OPCODE_is_expression(WN_opcode(body_part_of_interest)))
      Accumulate_Stmt_PUinfo(body_part_of_interest);
   
   /* Traverse the statements and expressions in the current PU, 
    * accumulating information about pseudo register usage and 
    * refining the information about call-sites to account for 
    * unexpected uses of return-registers.
    */
   Accumulate_Expr_PUinfo(body_part_of_interest);

   /* Push a new symbol-table for name-disambiguation, and enter 
    * every pseudo-register, variable, function, constant, and type
    * of this PU scope into the symbol-table.  The former 
    * accumulative prepass over the expression trees should have 
    * determined all preg uses.
    */
   W2CF_Symtab_Push();
   Enter_Local_Syms_Into_Symtab(&St_Table[WN_entry_name(pu)]);
   Enter_Pregs_Into_Symtab();
   
} /* PUinfo_init_pu */


void 
PUinfo_exit_pu(void)
{
   /* Free up memory and reset the state to prepare for processing the
    * next PU.
    */
   UINT32      hash_idx;
   CALLSITE   *callsite;
   RETURNSITE *returnsite;
   TIEOPSITE *tiesite;

   /* Pop the current symbol-table for name-disambiguation */
   W2CF_Symtab_Pop();
   
   /* Free up the entries in the Preg_Info_Tbl and reset the table */
   for (hash_idx = 0; hash_idx < PREG_INFO_HASH_TABLE_SIZE; hash_idx++)
   {
      PREG_INFO *preg_info = Preg_Info_Hash_Tbl[hash_idx];
      if (preg_info != NULL)
      {
	 while (PREG_INFO_next(preg_info) != NULL)
	    preg_info = PREG_INFO_next(preg_info);
	 PREG_INFO_next(preg_info) = Free_Preg_Info;
	 Free_Preg_Info = Preg_Info_Hash_Tbl[hash_idx];
	 Preg_Info_Hash_Tbl[hash_idx] = NULL;
      }
   } /* for */

   /* Free up the entries in the CallSite list */
   for (callsite = CallSite_First; 
	callsite != NULL; 
	callsite = CALLSITE_next(callsite))
   {
      CALLSITE_next(callsite) = CallSite_Free;
      CallSite_Free = callsite;
   }
   CallSite_First = NULL;
   CallSite_Last = NULL;
   
   /* Free up the entries in the ReturnSite list */
   for (returnsite = ReturnSite_First; 
	returnsite != NULL; 
	returnsite = RETURNSITE_next(returnsite))
   {
      RETURNSITE_next(returnsite) = ReturnSite_Free;
      ReturnSite_Free = returnsite;
   }
   ReturnSite_First = NULL;
   ReturnSite_Last = NULL;

   for (tiesite = TieOpSite_First; 
	tiesite != NULL; 
	tiesite = TIEOPSITE_next(tiesite)) {
     Free_TieOpSite_Data(tiesite);
     TIEOPSITE_next(tiesite) = TieOpSite_Free;
     TieOpSite_Free = tiesite;
   }
   TieOpSite_First = NULL;
   TieOpSite_Last = NULL;
   
   /* Reset the PU state */
   PUinfo_current_func = NULL;
   PUinfo_return_preg = NULL;
   if (PUinfo_local_decls != NULL)
      Reclaim_Token_Buffer(&PUinfo_local_decls);
   if (PUinfo_pragmas != NULL)
      Reclaim_Token_Buffer(&PUinfo_pragmas);

} /* PUinfo_exit_pu */


TY_IDX
PUinfo_Preg_Type(TY_IDX preg_ty, INT16 preg_num)
{
   /* Return the TY_IDX which should be used to refer to and to declare
    * a variable for the given preg number.  The ty argument is the
    * type with which the preg is referenced in WHIRL, which is not
    * necessarily the same type we wish to refer to the preg in the
    * high-level language to which we translate (C or Fortran).
    */
   TY_IDX          ty;
   PREG_INFO      *preg_info;
   INT             usage_kind, this_ukind;

   /* Preg types other than integral types can be referred to and used
    * as they are.  Integral preg types may be "larger" than in WHIRL.
    */
   if (!TY_Is_Integral(preg_ty))
     ty = preg_ty;
   else
     {
       preg_info = Get_Preg_Info(preg_num);
       if (preg_info == NULL)
	 {
	   Accumulate_Preg_Info(preg_ty, preg_num); /* Just to make sure */
	   preg_info = Get_Preg_Info(preg_num);
	 }
       
       if (!preg_info)
	 ty = preg_ty;
       else {
	 this_ukind = (INT)Mtype_to_Ukind(TY_mtype(preg_ty));
	 if (this_ukind == PREG_AS_SPECIAL)
	   ty = preg_ty;
	 else {
	   for (usage_kind = (INT)LARGEST_iPREG_USAGE_KIND; 
		(usage_kind >= this_ukind && 
		 !PREG_INFO_use(preg_info, usage_kind));
		usage_kind--);
	   ty = Stab_Mtype_To_Ty(Ukind_to_Mtype[usage_kind]);
	 }
       }
     }
   return ty;
} /* PUinfo_Preg_Type */


BOOL 
PUinfo_Is_Preg_Declared(TY_IDX preg_ty, INT16 preg_num)
{
   if (preg_ty == 0 || preg_num < 0)
     return TRUE; 

   PREG_INFO *preg_info = Get_Preg_Info(preg_num);

   if (preg_info == NULL)
   {
      Accumulate_Preg_Info(preg_ty, preg_num); /* Just to make sure */
      preg_info = Get_Preg_Info(preg_num);
   }

   return PREG_INFO_decl(preg_info, Mtype_to_Ukind(TY_mtype(preg_ty)));
} /* PUinfo_Is_Preg_Declared */


void 
PUinfo_Set_Preg_Declared(TY_IDX preg_ty, INT16 preg_num)
{
   PREG_INFO *preg_info = Get_Preg_Info(preg_num);

   if (preg_info == NULL)
   {
      Accumulate_Preg_Info(preg_ty, preg_num); /* Just to make sure */
      preg_info = Get_Preg_Info(preg_num);
   }

   PREG_INFO_decl(preg_info, Mtype_to_Ukind(TY_mtype(preg_ty))) = TRUE;
} /* PUinfo_Set_Preg_Declared */


CALLSITE *
PUinfo_Get_CallSites(void)
{
   return CallSite_First;
} /* PUinfo_Get_CallSites */


RETURNSITE *
PUinfo_Get_ReturnSites(void)
{
   return ReturnSite_First;
} /* PUinfo_Get_ReturnSites */

TIEOPSITE *
PUinfo_Get_TieOpSites(void)
{
   return TieOpSite_First;
} /* PUinfo_Get_TieOpSites */

RETURN_PREG
PUinfo_Get_ReturnPreg(TY_IDX return_ty, Return_Info_Dir dir)
{
   /* Use the generic routines from "targ_sim.h" to get information
    * about how a return-type is split up between return registers.
    */
   RETURN_PREG         return_preg;
   RETURN_PREG * const return_preg_ptr = &return_preg;

   if (WHIRL_Return_Info_On) {

      RETURN_INFO return_info = Get_Return_Info (return_ty,
						 Use_Simulated,
						 dir);

      RETURN_PREG_num_pregs(return_preg_ptr) = RETURN_INFO_count(return_info);

      for (INT32 i = 0; i < RETURN_INFO_count(return_info); i++) {
         RETURN_PREG_mtype(return_preg_ptr, i) = 
           RETURN_INFO_mtype(return_info, i);
         RETURN_PREG_offset(return_preg_ptr, i) = 
           RETURN_INFO_preg(return_info, i);
      }
   }
#ifdef __OBSOLETE__
   else {

     PREG_NUM preg_num1, preg_num2;

     /* Get the mtypes of the return registers */
     Get_Return_Mtypes(return_ty,                               /* in */
		       Use_Simulated,                  	        /* in */
		       &RETURN_PREG_mtype(return_preg_ptr, 0),  /* out */
		       &RETURN_PREG_mtype(return_preg_ptr, 1)); /* out */

     /* Get the PREG_NUM of the return registers */
     Get_Return_Pregs(RETURN_PREG_mtype(return_preg_ptr, 0),    /* in */
		      RETURN_PREG_mtype(return_preg_ptr, 1),    /* in */
		      &preg_num1,                               /* out */
		      &preg_num2);                              /* out */

     RETURN_PREG_offset(return_preg_ptr, 0) = preg_num1;
     RETURN_PREG_offset(return_preg_ptr, 1) = preg_num2;
   
     if (RETURN_PREG_mtype(return_preg_ptr, 0) == MTYPE_V)
        RETURN_PREG_num_pregs(return_preg_ptr) = 0;
     else if  (RETURN_PREG_mtype(return_preg_ptr, 1) == MTYPE_V)
        RETURN_PREG_num_pregs(return_preg_ptr) = 1;
     else
        RETURN_PREG_num_pregs(return_preg_ptr) = 2;
   }
#endif
   return return_preg;
} /* PUinfo_Get_ReturnPreg */


WN *
PUinfo_Packed_Tie_Preg_Outpart (INT16 preg_num, INT outpart_idx)
{
  if (outpart_idx >= MAX_OUTPART_LDIDS)
    return NULL;

  PREG_INFO *preg_info = Get_Preg_Info(preg_num);
  if (preg_info == NULL)
    return NULL;
  
  outpart_idx--;
  return PREG_INFO_outpart_ldid(preg_info, outpart_idx);
}
