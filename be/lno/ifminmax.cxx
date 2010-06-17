
/* 
   Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.
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


#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <sys/types.h>

#include "lnopt_main.h"
#include "config.h"
#include "config_lno.h"
#include "strtab.h"
#include "stab.h"
#include "targ_const.h"

#include "lnoutils.h"
#include "wn_simp.h"
#include "stdlib.h"
#include "lwn_util.h"
#include "optimizer.h"
#include "opt_du.h"
#include "name.h"
#include "forward.h"

static DU_MANAGER* du = NULL; 

//-----------------------------------------------------------------------
// NAME: Matching_Refs
// FUNCTION: Returns TRUE if 'wn_ref_one' and 'wn_ref_two' refer 
//   to the same location. 
//-----------------------------------------------------------------------

static BOOL Matching_Refs(WN* wn_ref_one, 
			    WN* wn_ref_two)
{
  OPCODE op_one = WN_opcode(wn_ref_one); 
  OPCODE op_two = WN_opcode(wn_ref_two); 
  OPERATOR oper1, oper2;
  if (WN_desc(wn_ref_one) != WN_desc(wn_ref_two)) return FALSE;

  oper1 = OPCODE_operator(op_one);
  oper2 = OPCODE_operator(op_two);
  if (op_one != op_two) {
    if (oper1 == OPR_STID) {
      if (oper2 != OPR_LDID) return FALSE;
    } else  if (oper1 == OPR_ISTORE) {
      if (oper2 != OPR_ILOAD) return FALSE;
    } else if (oper1 == OPR_LDID) {
      if (oper2 != OPR_STID) return FALSE;
    } else  if (oper1 == OPR_ILOAD) {
      if (oper2 != OPR_ISTORE) return FALSE;
    }
  }

  if (oper1 == OPR_STID || oper1 == OPR_LDID) {
      return SYMBOL(wn_ref_one) == SYMBOL(wn_ref_two); 
  } else if (oper1 == OPR_ISTORE || oper1 == OPR_ILOAD) {
    if (WN_offset(wn_ref_one) != WN_offset(wn_ref_two)) {
      return FALSE;
    }
    WN* wn_array_one;
    WN* wn_array_two;
    if (oper1 == OPR_ILOAD) {
      wn_array_one  = WN_kid0(wn_ref_one); 
    } else { 
      wn_array_one  = WN_kid1(wn_ref_one); 
    }
    if (oper2 == OPR_ILOAD) {
      wn_array_two  = WN_kid0(wn_ref_two); 
    } else { 
      wn_array_two  = WN_kid1(wn_ref_two); 
    }
    if (WN_operator(wn_array_one) != OPR_ARRAY)
      return FALSE; 
    if (WN_operator(wn_array_two) != OPR_ARRAY)
      return FALSE; 
    WN* wn_base_one = WN_array_base(wn_array_one); 
    WN* wn_base_two = WN_array_base(wn_array_two); 
    OPERATOR opr_base_one = WN_operator(wn_base_one);
    if (opr_base_one != OPR_LDA && opr_base_one != OPR_LDID)
	return FALSE;
    OPERATOR opr_base_two = WN_operator(wn_base_two);
    if (opr_base_two != OPR_LDA && opr_base_two != OPR_LDID)
	return FALSE;

    if (SYMBOL(wn_base_one) != SYMBOL(wn_base_two)) {
      return FALSE;
    }
    ACCESS_ARRAY* aa_one = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, 
      wn_array_one);
    ACCESS_ARRAY* aa_two = (ACCESS_ARRAY*) WN_MAP_Get(LNO_Info_Map, 
      wn_array_two);
    if (!(*aa_one == *aa_two))
      return FALSE; 
    return TRUE; 
  } else {
    return FALSE; 
  }
  return FALSE;
} 



//-----------------------------------------------------------------------
// NAME: Matching_Exprs
// FUNCTION: Returns TRUE if 'wn_one' and 'wn_two' are equivalent ex- 
//   pressions. 
//-----------------------------------------------------------------------

static BOOL Matching_Exprs(WN* wn_one, 
			   WN* wn_two)
{
  return WN_Simp_Compare_Trees(wn_one, wn_two) == 0; 
}

// 
// Look for the special pattern
// if (a < b) then
//   r = b
// else 
//   if (a > c) then
//     r = c
//   else r = a
//
// where b < c and convert it into
//   r =  MIN(MAX(a,b),c)
//
// return TRUE if the conversion happens, and do the conversion
static BOOL IF_then_else_convert(WN *if_tree)
{
  // Make sure that it's a nested if then else with a single statement 
  // in each clause
  WN *wn_else_st = WN_first(WN_else(if_tree));
  if (!wn_else_st || WN_next(wn_else_st)) return FALSE;
  if (WN_opcode(wn_else_st) != OPC_IF) return FALSE;


  WN *wn_else_then_st = WN_first(WN_then(wn_else_st));
  if (!wn_else_then_st || WN_next(wn_else_then_st)) return FALSE;
  WN *wn_else_else_st = WN_first(WN_else(wn_else_st));
  if (wn_else_else_st && WN_next(wn_else_else_st)) return FALSE;

  WN *wn_then_st = WN_first(WN_then(if_tree));
  if (!wn_then_st || WN_next(wn_then_st)) return FALSE;

  // Make sure the comparison constraints match
  WN* test1 = WN_if_test(if_tree);
  OPERATOR opr1 = WN_operator(test1);
  WN* test2 = WN_if_test(wn_else_st);
  OPERATOR opr2 = WN_operator(test2);
  if (opr1 == OPR_LT || opr1 == OPR_LE) {
    if (opr2 != OPR_GT && opr2 != OPR_GE) return FALSE;
  } else if (opr1 == OPR_GT || opr1 == OPR_GE) {
    if (opr2 != OPR_LT && opr2 != OPR_LE) return FALSE;
  } else
    return FALSE;

  // Check the first then
  if (WN_operator(WN_kid1(test1)) != OPR_INTCONST) return FALSE;
  if (!OPCODE_is_load(WN_opcode(WN_kid0(test1)))) return FALSE;
  if (!OPCODE_is_store(WN_opcode(wn_then_st))) return FALSE;
  if (WN_operator(WN_kid0(wn_then_st)) != OPR_INTCONST) return FALSE;
  if (WN_const_val(WN_kid0(wn_then_st)) != WN_const_val(WN_kid1(test1))) {
    return FALSE;
  }

  // Check the second then
  if (WN_operator(WN_kid1(test2)) != OPR_INTCONST) return FALSE;
  if (!OPCODE_is_load(WN_opcode(WN_kid0(test2)))) return FALSE;
  if (!Matching_Refs(WN_kid0(test1), WN_kid0(test2))) return FALSE;
  if (!Matching_Refs(wn_then_st, wn_else_then_st)) return FALSE;
  if (WN_operator(WN_kid0(wn_else_then_st)) != OPR_INTCONST) return FALSE;
  if (WN_const_val(WN_kid0(wn_else_then_st)) != 
		  WN_const_val(WN_kid1(test2))) {
    return FALSE;
  }

  // Check the else else
  if (wn_else_else_st) {
    if (!Matching_Refs(wn_else_else_st,wn_then_st)) return FALSE;
    if (!Matching_Refs(WN_kid0(wn_else_else_st),WN_kid0(test1))) {
      return FALSE;
    }
  } else { // No last else, so 'r' must already equal 'a'
    if (!Matching_Refs(wn_then_st, WN_kid0(test1))) return FALSE;
  }

  if (opr1 == OPR_LT || opr1 == OPR_LE) {
    if (WN_const_val(WN_kid1(test1)) >= WN_const_val(WN_kid1(test2))) {
      return FALSE;
    }
  } else {
    if (WN_const_val(WN_kid1(test1)) <= WN_const_val(WN_kid1(test2))) {
      return FALSE;
    }
  }

  // Do the substition
  TYPE_ID type_cmp = Max_Wtype(WN_rtype(WN_kid0(test1)), 
		  	       WN_rtype(WN_kid0(test2)));
  OPCODE op_max, op_min;
  if (opr1 == OPR_LT || opr1 == OPR_LE) {
    op_max = OPCODE_make_op(OPR_MAX , type_cmp, MTYPE_V); 
    op_min = OPCODE_make_op(OPR_MIN , type_cmp, MTYPE_V); 
  } else {
    op_max = OPCODE_make_op(OPR_MIN , type_cmp, MTYPE_V); 
    op_min = OPCODE_make_op(OPR_MAX , type_cmp, MTYPE_V); 
  }

  WN *new_store = LWN_Extract_From_Block(wn_then_st);  // r = b

  LWN_Delete_Tree(WN_kid0(new_store));
  WN *t0 = WN_kid0(test1);
  WN *t1 = WN_kid1(test1);
  WN_kid0(test1) = NULL; WN_kid1(test1) = NULL;
  WN_kid0(new_store) = LWN_CreateExp2(op_max, t0, t1);
  LWN_Parentize(new_store);

  t0 = WN_kid0(wn_else_then_st);
  WN_kid0(wn_else_then_st) = NULL;
  WN_kid0(new_store) = LWN_CreateExp2(op_min,WN_kid0(new_store), t0);
  LWN_Parentize(new_store);
  LWN_Insert_Block_Before(LWN_Get_Parent(if_tree), if_tree, new_store); 
  LWN_Delete_Tree_From_Block(if_tree);
  return TRUE;
}


static INT
find_matching_kid (WN *wn, WN *kid)
{
  for (INT idx = 0; idx < WN_kid_count(wn); idx++)
    {
      if (Matching_Exprs(kid, WN_kid(wn, idx)))
        return idx;
    }
  
  return -1;
}


/* Look for implicit min/max. The general case is:

   if (a .relop. b)
     x = f(a);
   else
     x = f(b);
   
   becomes

   x = f(min(a, b)).

   Currently, we handle only the case where f is a simple operator and
   a and b are top-level children of f.
*/

static BOOL
IFMM_Direct_Use (WN* wn_tree) 
{
  WN* wn_test = WN_if_test(wn_tree);
  OPERATOR opr = WN_operator(wn_test);
  if (!(opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE))
    return FALSE;
  
  WN *wn_st_then = WN_first(WN_then(wn_tree));
  if (!wn_st_then || WN_next(wn_st_then))
    return FALSE;

  WN *wn_st_else = WN_first(WN_else(wn_tree));
  if (!wn_st_else || WN_next(wn_st_else))
    return FALSE;

  WN* wn_expr_left = WN_kid0(wn_test);
  WN* wn_expr_right = WN_kid1(wn_test);

#ifdef TARG_XTENSA
  // Don't generate F4/F8 MIN/MAX
  if (!MTYPE_is_integral(WN_rtype(wn_expr_left)) ||
      !MTYPE_is_integral(WN_rtype(wn_expr_right)))
    return FALSE;
#endif

  if (WN_operator(wn_st_then) != WN_operator(wn_st_else))
    return FALSE;
  
  if (WN_operator(wn_st_then) != OPR_STID &&
      WN_operator(wn_st_then) != OPR_ISTORE)
    return FALSE;
  
  if (!Matching_Refs(wn_st_then, wn_st_else))
    return FALSE;
  
  WN *then_val = WN_kid0(wn_st_then);
  WN *else_val = WN_kid0(wn_st_else);

  if (WN_opcode(then_val) != WN_opcode(else_val))
    return FALSE;

  switch (WN_operator(then_val))
    {
    case OPR_ABS:
    case OPR_ADD:
    case OPR_BAND:
    case OPR_BNOT:
    case OPR_BIOR:
    case OPR_BXOR:
    case OPR_CVT:
    case OPR_EQ:
    case OPR_MAX:
    case OPR_MIN:
    case OPR_MPY:
    case OPR_NEG:
      break;
      
    default:
      return FALSE;
    }
  
  /* Now we know that the code looks like:
     if (a .relop. b)
       x = op(...);
     else
       x = op(...);
     Try to match a and b in the if sides. */

  if (Contains_Dedicated_Preg(wn_tree))
    return FALSE;
  
  if (Contains_Volatile_Refs(wn_tree))
    return FALSE;

  INT op_kid_count = WN_kid_count(then_val);
  if (op_kid_count > 2)
    return FALSE;

  OPERATOR cmp_oper = (opr == OPR_LT || opr == OPR_LE) ? OPR_MIN : OPR_MAX;
  
  INT then_idx = find_matching_kid(then_val, wn_expr_left);
  INT else_idx = find_matching_kid(else_val, wn_expr_right);
  if (then_idx < 0 || else_idx < 0)
    {
      then_idx = find_matching_kid(then_val, wn_expr_right);
      else_idx = find_matching_kid(else_val, wn_expr_left);
      if (then_idx < 0 || else_idx < 0)
        return FALSE;
      
      /* In case the pattern is:
         if (a .relop. b)
           x = op(b);
         else
           x = op(a);
         all we need to do is invert the min/max operator. */
      
      cmp_oper = (cmp_oper == OPR_MIN) ? OPR_MAX : OPR_MIN;
    }
  
  
  /* Found a matching kid in each op. Match the rest of the op if necessary. */
  if (op_kid_count == 2 &&
      !Matching_Exprs(WN_kid(then_val, 1 - then_idx),
                      WN_kid(else_val, 1 - else_idx)))
    return FALSE;

  /* Pattern matched. Apply the transformation. */
  TYPE_ID cmp_type = Max_Wtype(WN_rtype(wn_expr_left), WN_rtype(wn_expr_right));
  OPCODE cmp_opc = OPCODE_make_op(cmp_oper, cmp_type, MTYPE_V); 
  
  WN_kid0(wn_test) = NULL;
  WN_kid1(wn_test) = NULL;
  
  WN *cmp_op = LWN_CreateExp2(cmp_opc, wn_expr_left, wn_expr_right);
  
  /* Use the then-statement as a base for the new statement. */
  WN *res = LWN_Extract_From_Block(wn_st_then);
  WN *old_kid = WN_kid(then_val, then_idx);
  LWN_Set_Parent(old_kid, NULL);
  LWN_Delete_Tree(old_kid);
  WN_kid(then_val, then_idx) = cmp_op;
  LWN_Parentize_One_Level(then_val);
  
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_tree), wn_tree, res); 
  LWN_Delete_Tree_From_Block(wn_tree);
  return TRUE;
}  


//-----------------------------------------------------------------------
// NAME: IFMM_Convertible
// FUNCTION: Returns TRUE if 'wn_tree' is an OPR_IF which can be converted
//   into an OPR_MIN or OPR_MAX.  If so, '*if_mm_max' is set to TRUE if it
//   can be converted to an OPR_MAX , to FALSE if it can be converted
//   to an OPR_MIN. Also, set result_store to the store statement used
//   during the actual conversion. If no conversion is possible, returns
//   FALSE.  
//-----------------------------------------------------------------------

static BOOL
IFMM_Convertible (WN* wn_tree, BOOL* if_mm_max, WN **result_store) 
{

  LWN_Simplify_Tree(WN_if_test(wn_tree));
  WN* wn_test = WN_if_test(wn_tree);
  OPERATOR opr = WN_operator(wn_test);
  if (!(opr == OPR_LT || opr == OPR_LE || opr == OPR_GT || opr == OPR_GE))
    return FALSE;

  /* The then and the else blocks should be empty or single-statement. */
  WN *wn_st_then = WN_first(WN_then(wn_tree));
  if (wn_st_then) {
    if (WN_next(wn_st_then))
      return FALSE;

  }
  
  WN *wn_st_else = WN_first(WN_else(wn_tree));
  if (wn_st_else) {
    if (WN_next(wn_st_else))
      return FALSE;
    
  }
  
  if (!wn_st_then && !wn_st_else)
    return FALSE;
  
  WN* wn_expr_left = WN_kid0(wn_test);
  WN* wn_expr_right = WN_kid1(wn_test);
  
#ifdef TARG_XTENSA
  // Don't generate F4/F8 MIN/MAX
  if (!MTYPE_is_integral(WN_rtype(wn_expr_left)) ||
      !MTYPE_is_integral(WN_rtype(wn_expr_right)))
    return FALSE;
#endif
  
  *result_store = wn_st_then ?: wn_st_else;
  WN *temp_load = NULL;

  if (wn_st_then && wn_st_else) {
    if (!Matching_Refs(wn_st_then, wn_st_else))
      return FALSE;
  } else {
    if (WN_operator(*result_store) != OPR_STID &&
	    !Matching_Refs(*result_store,wn_expr_left) &&
	    !Matching_Refs(*result_store,wn_expr_right)) {
      return FALSE; // Don't speculate
    }
    
    if (Contains_Dedicated_Preg(*result_store))
      return FALSE;
    
    if (Contains_Volatile_Refs(*result_store))
      return FALSE;
    
    /* Create a temporary load to use for comparison. */
    if (WN_operator(*result_store) == OPR_STID) {
      temp_load = WN_CreateLdid(OPR_LDID,
			      Promote_Type(WN_desc(*result_store)),
			      WN_desc(*result_store),
			      WN_offset(*result_store),
			      WN_st(*result_store),
			      WN_ty(*result_store),
			      WN_field_id(*result_store));
    } else if (WN_operator(*result_store) == OPR_ISTORE) {
      /* Include a copy of the address in the temporary ILOAD because
         it's simplified during load creation and later. */
      WN *addr = LWN_Copy_Tree(WN_kid1(*result_store));
      temp_load = WN_CreateIload(OPR_ILOAD,
                                 Promote_Type(WN_desc(*result_store)),
                                 WN_desc(*result_store),
                                 WN_offset(*result_store),
                                 WN_ty(*result_store),
                                 WN_ty(*result_store),
                                 addr,
                                 WN_field_id(*result_store));
      LWN_Parentize_One_Level(temp_load);
    } else {
      return FALSE;
    }
  }
  
  WN* wn_expr_then = wn_st_then ? WN_kid0(wn_st_then) : temp_load;
  WN* wn_expr_else = wn_st_else ? WN_kid0(wn_st_else) : temp_load;
//   if (expr1 .relop. expr2) then 
//     result = expr1 
//   else 
//     result = expr2 
//   end if  
// to expressions of the form: 
//   result = min(expr1, expr2) 
// or
  // Ignore no-op cvtls
  WN *cvtl = NULL;
  INT64 constant;
  if (WN_operator(wn_expr_then) == OPR_CVTL) {
    if (WN_operator(wn_expr_else) == OPR_INTCONST) {
       cvtl = wn_expr_then;
       constant = WN_const_val(wn_expr_else);
       // opportunisitically ignore cvtl
       wn_expr_then = WN_kid0(wn_expr_then);
    }
  } else if (WN_operator(wn_expr_else) == OPR_CVTL) {
    if (WN_operator(wn_expr_then) == OPR_INTCONST) {
       cvtl = wn_expr_else;
       constant = WN_const_val(wn_expr_then);
       wn_expr_else = WN_kid0(wn_expr_else);
    }
  }
  wn_expr_then = LWN_Simplify_Tree(wn_expr_then);
  wn_expr_else = LWN_Simplify_Tree(wn_expr_else);
  
  BOOL mm_max = FALSE; 
  BOOL return_value = FALSE; 
  if (Matching_Exprs(wn_expr_left, wn_expr_else)) { 
    if (Matching_Exprs(wn_expr_right, wn_expr_then)) {
      mm_max = opr == OPR_LT || opr == OPR_LE; 
      return_value = TRUE;
    } 
  } else if (Matching_Exprs(wn_expr_right, wn_expr_else)) { 
    if (Matching_Exprs(wn_expr_left, wn_expr_then)) {
      mm_max = opr == OPR_GT || opr == OPR_GE; 
      return_value = TRUE;
    } 
  } 

  if (return_value && cvtl) {
    INT cvtl_limit;
    if (MTYPE_is_unsigned(WN_rtype(cvtl))) {
      cvtl_limit = 1 << (WN_cvtl_bits(cvtl));
    } else {
      cvtl_limit = 1 << (WN_cvtl_bits(cvtl) -1);
    }
    if (constant >= cvtl_limit || constant < -cvtl_limit) {
      mm_max = FALSE;
      return_value = FALSE;
    }
  }
  

  if (temp_load)
    LWN_Delete_Tree(temp_load);

  *if_mm_max = mm_max;
  return return_value;
}  

//-----------------------------------------------------------------------
// NAME: IFMM_Convert
// FUNCTION: Converts the tree rooted at 'wn_if' to expression beginning 
//   with OPR_MIN if 'ifmm_max' is FALSE or an expression beginning with
//   with OPR_MAX if 'ifmm_max' is TRUE. Use 'result_store' to form
//   the new min or max statement.
//-----------------------------------------------------------------------

static WN *
IFMM_Convert (WN *wn_if, BOOL ifmm_max, WN *result_store)
{
  Is_True(result_store, ("Null result store."));

  WN* wn_left = WN_kid0(WN_if_test(wn_if));
  WN* wn_right = WN_kid1(WN_if_test(wn_if));
  WN* wn_result = result_store;
  WN* wn_result_expr = WN_kid0(wn_result); 
  INT i;
  for (i = 0; i < WN_kid_count(wn_result_expr); i++) 
    if (WN_kid(LWN_Get_Parent(wn_result_expr), i) == wn_result_expr)
      break;
  INT kid_count = i; 
  TYPE_ID type_cmp = Max_Wtype(WN_rtype(wn_left), WN_rtype(wn_right));
  OPCODE op = OPCODE_make_op(ifmm_max ? OPR_MAX : OPR_MIN, type_cmp, MTYPE_V); 
  WN* wn_cmp = LWN_CreateExp2(op, wn_left, wn_right); 
  WN_kid0(WN_if_test(wn_if)) = NULL; 
  WN_kid1(WN_if_test(wn_if)) = NULL; 
  LWN_Set_Parent(wn_cmp, wn_result);
  WN_kid(wn_result, kid_count) = wn_cmp; 
  LWN_Extract_From_Block(wn_result); 
  LWN_Insert_Block_Before(LWN_Get_Parent(wn_if), wn_if, wn_result); 
  LWN_Extract_From_Block(wn_if);
  LWN_Delete_Tree(wn_if); 
  LWN_Delete_Tree(wn_result_expr);
  return wn_result; 
} 

//-----------------------------------------------------------------------
// NAME: Is_Loop_Lower_Bound
// FUNCTION: Returns TRUE if 'wn_use' is a lower bound of a OPC_DO_LOOP, 
//   (i.e. the WN_kid0(WN_start(wn_loop)) of some loop wn_loop).  Returns
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Loop_Lower_Bound(WN* wn_use)
{ 
  if (WN_operator(wn_use) != OPR_LDID)
    return FALSE; 
  WN* wn_start = LWN_Get_Parent(wn_use); 
  if (wn_start == NULL) 
    return FALSE; 
  WN* wn_loop = LWN_Get_Parent(wn_start);
  if (wn_loop == NULL) 
    return FALSE; 
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return FALSE; 
  if (wn_start != WN_start(wn_loop))
    return FALSE; 
  if (wn_use != WN_kid0(wn_start))
    return FALSE; 
  return TRUE;
} 

//-----------------------------------------------------------------------
// NAME: Is_Loop_Upper_Bound
// FUNCTION: Returns TRUE if 'wn_use' is the upper bound of an OPC_DO_LOOP,
//   (i.e. the UBexp(WN_end(wn_loop)) of some loop wn_loop).  Returns 
//   FALSE otherwise. 
//-----------------------------------------------------------------------

static BOOL Is_Loop_Upper_Bound(WN* wn_use)
{
  if (WN_operator(wn_use) != OPR_LDID)
    return FALSE;
  WN* wn_end = LWN_Get_Parent(wn_use);
  if (wn_end == NULL) 
    return FALSE; 
  WN* wn_loop = LWN_Get_Parent(wn_end); 
  if (wn_loop == NULL) 
    return FALSE; 
  if (WN_opcode(wn_loop) != OPC_DO_LOOP)
    return FALSE; 
  if (wn_end != WN_end(wn_loop))
    return FALSE; 
  if (wn_use != UBexp(WN_end(wn_loop)))
    return FALSE; 
  return TRUE; 
} 

static BOOL Closest_Ancestor_If(WN *wn)
{
  while (wn && !OPCODE_is_scf(WN_opcode(wn))) wn = LWN_Get_Parent(wn);
  if (wn) {
    if (WN_operator(wn) == OPR_IF) {
      return TRUE;
    } else if (WN_opcode(wn) == OPC_BLOCK &&
	       WN_operator(LWN_Get_Parent(wn)) == OPR_IF) {
      return TRUE;
    }
  }
  return FALSE;
}

//-----------------------------------------------------------------------
// NAME: IFMM_Sink
// FUNCTION: Attempt to sink the expression rooted at 'wn_max_store' 
//-----------------------------------------------------------------------

static void IFMM_Sink(WN* wn_max_store)
{ 
  if (WN_operator(wn_max_store) != OPR_STID)
    return; 
  USE_LIST *use_list = du->Du_Get_Use(wn_max_store);
  if (use_list == NULL) 
    return;
  USE_LIST_ITER iter(use_list);
  const DU_NODE* node = NULL;
  const DU_NODE* nnode = NULL;
  for (node = iter.First(); !iter.Is_Empty(); node = nnode) {
    WN* wn_use = node->Wn();
    WN *use_statement = LWN_Get_Statement(wn_use);
    nnode = iter.Next();
    if (Is_Loop_Lower_Bound(wn_use)) {
      WN* wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_use)); 
      Forward_Substitute_Ldids(wn_use, du);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
      if (Bound_Is_Too_Messy(dli->LB))
	Hoist_Bounds_One_Level(wn_loop); 
    } else if (Is_Loop_Upper_Bound(wn_use)) { 
      WN* wn_loop = LWN_Get_Parent(LWN_Get_Parent(wn_use)); 
      Forward_Substitute_Ldids(wn_use, du);
      DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_loop); 
      if (Bound_Is_Too_Messy(dli->UB))
	Hoist_Bounds_One_Level(wn_loop); 
    } else if ( WN_operator(wn_use) == OPR_LDID) {
      WN *parent = LWN_Get_Parent(wn_use);
      if (Forward_Substitute_Ldids(wn_use, du)) {
        if (WN_operator(parent) == OPR_STID) {
          IFMM_Sink(parent);
        }
      }
    }
  }
} 

//-----------------------------------------------------------------------
// NAME: If_MinMax_Traverse
// FUNCTION: Traverse the 'wn_tree', attempting to convert IF statements
//  into MIN and MAX statements and sinking them into loop bounds or further ifs. 
//-----------------------------------------------------------------------
//

static void If_MinMax_Traverse(WN* wn_tree)
{
  BOOL ifmm_max = FALSE; 
  WN *result_store = NULL;
  if (WN_opcode(wn_tree) == OPC_IF) {
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      If_MinMax_Traverse(WN_kid(wn_tree, i)); 
    if (IFMM_Convertible(wn_tree, &ifmm_max, &result_store)) {
      WN* wn_max_store = IFMM_Convert(wn_tree, ifmm_max, result_store);
      if (Array_Dependence_Graph) IFMM_Sink(wn_max_store);
      return; 
    } else {
	// Look for if-then-else min/max pattern
	if (IF_then_else_convert(wn_tree)) {
	  return;
	}
        
        if (IFMM_Direct_Use(wn_tree))
          return;
    } 
  } else if(WN_opcode(wn_tree) == OPC_BLOCK) { 
    WN* wnn = NULL; 
    for (WN* wn = WN_first(wn_tree); wn != NULL; wn = wnn) {
      wnn = WN_next(wn); 
      while (wnn && !OPCODE_is_scf(WN_opcode(wnn))) {
        // Find next wn to process, normal statements might get deleted by
	// Traverse but other SCF nodes can't
	wnn = WN_next(wnn);
      }
      If_MinMax_Traverse(wn); 
    } 
  } else { 
    for (INT i = 0; i < WN_kid_count(wn_tree); i++) 
      If_MinMax_Traverse(WN_kid(wn_tree, i)); 
  } 
} 

//-----------------------------------------------------------------------
// NAME: If_MinMax
// FUNCTION: Convert expressions of the form: 
//   if (expr1 .relop. expr2) then 
//     result = expr1 
//   else 
//     result = expr2 
//   end if  
// to expressions of the form: 
//   result = min(expr1, expr2) 
// or
//   result = max(expr1, expr2) 
// where .relop. is one of .LT., .GT., .LE., and .GE.  
// Attempt sinking converted expressions into loop bounds or ifs where 
//   appropriate. 
//-----------------------------------------------------------------------

extern void If_MinMax(WN* func_nd)
{
  if (!LNO_IfMinMax)
    return; 
  if (LNO_Verbose) { 
    fprintf(stdout, "Attempting to convert IFs to MAXs and MINs\n"); 
    fprintf(TFile, "Attempting to convert IFs to MAXs and MINs\n"); 
  } 
  du = Du_Mgr;
  If_MinMax_Traverse(func_nd); 
  if (LNO_Verbose) { 
    fprintf(stdout, "Finished converting IFs to MAXs and MINs\n"); 
    fprintf(TFile, "Finished converting IFs to MAXs and MINs\n"); 
  } 
}


