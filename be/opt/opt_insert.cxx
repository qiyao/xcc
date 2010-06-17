
/*

  Copyright (C) 2004-2005 Tensilica, Inc.  All Rights Reserved.

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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "opt_defs.h"
#include "opt_insert.h"
#include "defs.h"
#include "wn.h"
#include "errors.h"
#include "erglob.h"
#include "cxx_memory.h"
#include "ir_reader.h"
#include "tracing.h"
#include "tie.h"
#include "tietypes.h"
#include "intrn_info.h"
#include "wintrinsic.h"
#include "config_targ_options.h"

#define INS_TRACING Get_Trace(TP_WOPT2, INSERT_OPT_FLAG)
#define INJUI_OPCODE_NAME "INJUI"


static TIE_MACRO_ID
inj_macro_id (void)
{
  static bool inj_id_init = false;
  static TIE_MACRO_ID inj_id = TIE_INVALID_ID;

  if (inj_id_init)
    return inj_id;
  
  inj_id = tie_info->tie_macro_id_demangled(INJUI_OPCODE_NAME);
  if (inj_id == TIE_INVALID_ID) {
    if (INS_TRACING) {
      fprintf(TFile, "### TIE macro %s not found\n", INJUI_OPCODE_NAME);
    }
  } else {
    /* Perform some sanity checks. */
    TIE_MACRO *macro = tie_info->tie_macro(inj_id);
    if (!macro ||
        !macro->is_whirl_intrinsic_op() ||
        (macro->num_protos() != 4) ||
        (macro->num_inout_protos() != 1) ||
        (macro->num_instructions() != 1) ||
        strcasecmp(macro->inst_opcode_name(0), INJUI_OPCODE_NAME)) {
      /* Disqualify the proto. */
      if (INS_TRACING) {
        fprintf(TFile, "### TIE macro %s found (%s) but prototype doesn't match\n",
                INJUI_OPCODE_NAME, macro ? macro->name() : "");
      }
      inj_id = TIE_INVALID_ID;
    } else {
      if (INS_TRACING) {
        fprintf(TFile, "### TIE macro %s found (%s)\n",
                INJUI_OPCODE_NAME, macro->name());
      }
    }
  }

  inj_id_init = true;
  return inj_id;
}


static TIE_MACRO *
inj_macro (void)
{
  TIE_MACRO_ID inj_mid = inj_macro_id();
  if (inj_mid == TIE_INVALID_ID)
    return NULL;
  return tie_info->tie_macro(inj_mid);
}


static INTRINSIC
inj_intrinsic (void)
{
  TIE_MACRO_ID inj_mid = inj_macro_id();
  if (inj_mid == TIE_INVALID_ID)
    return INTRINSIC_INVALID;
  return Tie_Macro_Id_To_Intrinsic(inj_mid);
}


/* Replace 'wn' with an inject intrinsic. */
static bool
replace_inject (WN *parent_wn, WN *wn,
                WN *parent_wn_targ, INT targ_idx,
                WN *parent_wn_val, INT val_idx,
                INT shift, INT bits)
{
  TIE_MACRO *inj = inj_macro();
  if (!inj)
    return false;
  
  if (!inj->immediate_ok(2, shift))
    return false;
  if (!inj->immediate_ok(3, bits))
    return false;
  
  INTRINSIC inj_intrn = inj_intrinsic();
  if (inj_intrn == INTRINSIC_INVALID)
    return false;
  
  if (INS_TRACING) {
    fprintf(TFile, "\n### injui tree before:\n");
    fdump_tree(TFile, parent_wn);
  }
  
  OPCODE inj_op = OPCODE_make_op(OPR_INTRINSIC_OP, WN_rtype(wn), MTYPE_V);
  
  WN *apr[4];
  WN *wn_targ = WN_kid(parent_wn_targ, targ_idx);
  WN_kid(parent_wn_targ, targ_idx) = NULL;
  apr[0] = WN_CreateParm(WN_rtype(wn_targ), wn_targ, MTYPE_To_TY(WN_rtype(wn_targ)),
                         WN_PARM_BY_VALUE);
  
  WN *wn_val = WN_kid(parent_wn_val, val_idx);
  WN_kid(parent_wn_val, val_idx) = NULL;
  apr[1] = WN_CreateParm(WN_rtype(wn_val), wn_val, MTYPE_To_TY(WN_rtype(wn_val)), 
                         WN_PARM_BY_VALUE);
  
  WN *wn_shift = WN_CreateIntconst(OPC_I4INTCONST, shift);
  apr[2] = WN_CreateParm(MTYPE_I4, wn_shift, MTYPE_To_TY(MTYPE_I4),
                         WN_PARM_BY_VALUE);
  
  WN *wn_bits = WN_CreateIntconst(OPC_I4INTCONST, bits);
  apr[3] = WN_CreateParm(MTYPE_I4, wn_bits, MTYPE_To_TY(MTYPE_I4),
                         WN_PARM_BY_VALUE);
  
  WN *wn_inj = WN_Create_Intrinsic(inj_op, inj_intrn, 4, apr);
  for (INT i = 0; i < WN_kid_count(parent_wn); i++) {
    if (WN_kid(parent_wn, i) == wn) {
      WN_kid(parent_wn, i) = wn_inj;
      break;
    }
  }

  IPA_WN_DELETE_Tree(Current_Map_Tab, wn);
  
  if (INS_TRACING) {
    fprintf(TFile, "\n### injui tree after:\n");
    fdump_tree(TFile, parent_wn);
  }
  
  return true;
}


/* If mask is of the form ((1 << bits) - 1), return bits. Return -1 otherwise. */
static INT
mask_bits (INT mask)
{
  for (INT i = 1; i < 32; i++) {
    if (((1 << i) - 1) == mask)
      return i;
  }
  return -1;
}


/* Check if 'wn' is of the form (a & mask). Try to workaround CVTLs. */
static bool
is_masked_val (WN *wn, INT &mask, WN *&val_parent, INT &val_idx)
{

  Is_True(val_parent && (val_idx >= 0) &&
          (WN_kid(val_parent, val_idx) == wn),
          ("Expected val_parent[val_idx] to point to wn."));
  
  INT cvtl_bits = -1;

  if (WN_operator(wn) == OPR_CVTL) {
    cvtl_bits = WN_cvtl_bits(wn);
    val_parent = wn;
    val_idx = 0;
    wn = WN_kid(val_parent, val_idx);
  }
  
  if (WN_operator(wn) == OPR_ILOAD ||
      WN_operator(wn) == OPR_LDID)
    {
      if (WN_rtype(wn) != MTYPE_U4)
        return false;
      
      if (WN_desc(wn) != MTYPE_U1 &&
          WN_desc(wn) != MTYPE_U2)
        return false;
      
      mask = (1 << MTYPE_bit_size(WN_desc(wn))) - 1;
    }
  else
    {
      if (WN_operator(wn) != OPR_BAND)
        return false;

      bool found_mask = false;
      for (INT i = 0; i < 2; i++) {
        WN *wn_mask = WN_kid(wn, i);
        if (WN_operator(wn_mask) == OPR_INTCONST)
          {
            val_parent = wn;
            val_idx = 1 - i;
            mask = WN_const_val(wn_mask);
            found_mask = true;
            break;
          }
      }

      if (!found_mask)
        return false;
    }
  
  if (cvtl_bits > 1 && cvtl_bits < 32)
    {
      cvtl_bits--;
      if (((~((1U << cvtl_bits) - 1)) & mask) != 0)
        return false;
    }
    
  return true;
}


/* Look for:
   1. ((val & mask) << shift)
   2. (val & mask) --> (val & mask) << 0
   3. (val << shift) --> (val & mask(32 - shift)) << shift */
static bool
is_mask_shift (WN *wn, INT &bits, INT &shift, WN *&val_parent, INT &val_idx)
{
  Is_True(val_parent && (val_idx >= 0) &&
          (WN_kid(val_parent, val_idx) == wn),
          ("Expected val_parent[val_idx] to point to wn."));
  
  WN *cwn_parent = val_parent;
  INT cwn_idx = val_idx;
  WN *cwn = wn;

  shift = 0;
  if (WN_operator(cwn) == OPR_SHL)
    {
      WN *wn_sa = WN_kid1(cwn);
      if (WN_operator(wn_sa) != OPR_INTCONST)
        return false;
      
      shift = WN_const_val(wn_sa);
      if (shift < 0 || shift >= 32)
        return false;
      
      cwn_parent = cwn;
      cwn_idx = 0;
      cwn = WN_kid(cwn_parent, cwn_idx);
    }

  INT mask = 0;
  if (is_masked_val(cwn, mask, cwn_parent, cwn_idx))
    {
      bits = mask_bits(mask);
      val_parent = cwn_parent;
      val_idx = cwn_idx;
      return (bits > 0);
    }
  
  if (shift <= 0)
    return false;
  
  val_parent = wn;
  val_idx = 0;
  bits = 32 - shift;
  return true;
}


/* Look for and replace the pattern below with injui.
   (a & ~(mask << shift)) | (b & (mask << shift))
   
   Check if bits are being inserted from wn_val into wn_targ. */
static bool
direct_inject (WN *parent_wn, WN *wn, INT in_targ_idx)
{
  INT targ_mask = 0;
  INT targ_idx = in_targ_idx;
  WN *parent_wn_targ = wn;
  if (!is_masked_val(WN_kid(parent_wn_targ, targ_idx), targ_mask, parent_wn_targ, targ_idx))
    return false;
  
  INT shift = 0;
  INT bits = 0;
  INT val_idx = 1 - in_targ_idx;
  WN *parent_wn_val = wn;
  if (!is_mask_shift(WN_kid(parent_wn_val, val_idx), bits, shift, parent_wn_val, val_idx))
    return false;
  
  INT mask = ((1U << bits) - 1);
  if ((mask << shift) != ~targ_mask)
    return false;

  if (INS_TRACING) {
    fprintf(TFile, "\n### direct injui found, replacing\n");
  }

  if (!replace_inject(parent_wn, wn,
                      parent_wn_targ, targ_idx,
                      parent_wn_val, val_idx,
                      shift, bits)) {
    if (INS_TRACING) {
      fprintf(TFile, "\n### direct injui - replace failed.\n");
    }

    return false;
  }

  return true;
}


/* Return a mask of bits that have been initialized or
   are known to be zero. A bit set to 1 has unknown value. A bit
   set to 0 is known to be zero. */
static INT
value_bit_mask (WN *wn)
{
  /* All bits are valid, none are known to be 0. */
  INT noinfo = -1;
  
  TYPE_ID rtype = WN_rtype(wn);
  if (rtype != MTYPE_I4 && rtype != MTYPE_U4)
    return noinfo;
  
  OPERATOR oper = WN_operator(wn);
  switch (oper) {
  case OPR_INTCONST:
    {
      INT inj_bits = WN_const_val(wn);
      return inj_bits;
    }

  case OPR_BAND:
    {
      INT inj_bits0 = value_bit_mask(WN_kid0(wn));
      INT inj_bits1 = value_bit_mask(WN_kid1(wn));
      INT inj_bits = inj_bits0 & inj_bits1;
      return inj_bits;
    }
    
  case OPR_BIOR:
  case OPR_BXOR:
    {
      INT inj_bits0 = value_bit_mask(WN_kid0(wn));
      INT inj_bits1 = value_bit_mask(WN_kid1(wn));
      INT inj_bits = inj_bits0 | inj_bits1;
      return inj_bits;
    }

  case OPR_CVT:
    {
      TYPE_ID desc = WN_desc(wn);
      if (desc != MTYPE_I4 && desc != MTYPE_U4)
        return noinfo;
      
      INT inj_bits = value_bit_mask(WN_kid0(wn));
      return inj_bits;
    }
    
  case OPR_CVTL:
    {
      INT cvtl_bits = WN_cvtl_bits(wn);
      if (cvtl_bits <= 0 || cvtl_bits > 32)
        return noinfo;
      
      INT sign_bit = 1 << (cvtl_bits - 1);
      INT mask_zext = ((sign_bit << 1) - 1);
      INT mask_sext = ~(sign_bit - 1);
      INT inj_bits0 = value_bit_mask(WN_kid0(wn));
      
      /* If we need to zero extend, or sign-extend but the sign bit is 0,
         zero all bits left of the sign-bit. */
      if (rtype == MTYPE_U4 || ((inj_bits0 & sign_bit) == 0)) {
        INT inj_bits = inj_bits0 & mask_zext;
        return inj_bits;
      }
      
      INT inj_bits = inj_bits0 | mask_sext;
      return inj_bits;
    }

  case OPR_SHL:
  case OPR_ASHR:
  case OPR_LSHR:
    {
      WN *wn_sa = WN_kid1(wn);
      if ((WN_operator(wn_sa) != OPR_INTCONST) ||
          (WN_const_val(wn_sa) < 0) ||
          (WN_const_val(wn_sa) >= 32))
        return noinfo;
      
      INT inj_bits0 = value_bit_mask(WN_kid0(wn));
      
      INT shift = WN_const_val(wn_sa);
      INT inj_bits =
        ((oper == OPR_SHL) ? (inj_bits0 << shift) :
         (oper == OPR_ASHR) ? (((INT)inj_bits0) >> shift) : (((UINT)inj_bits0) >> shift));
      
      return inj_bits;
    }
    
  case OPR_INTRINSIC_OP:
    {
      if (WN_intrinsic(wn) != inj_intrinsic())
        return noinfo;
      
      INT inj_bits0 = value_bit_mask(WN_kid0(WN_kid(wn, 0)));
      INT shift = WN_const_val(WN_kid0(WN_kid(wn, 2)));
      INT bits = WN_const_val(WN_kid0(WN_kid(wn, 3)));
      INT mask = (1 << bits) - 1;
      INT inj_bits = inj_bits0 | (mask << shift);
      return inj_bits;
    }
    
  case OPR_ILOAD:
  case OPR_LDID:
    {
      if (WN_rtype(wn) != MTYPE_U4)
        return noinfo;

      if (WN_desc(wn) != MTYPE_U1 &&
          WN_desc(wn) != MTYPE_U2)
        return noinfo;
      
      INT inj_bits = (1 << MTYPE_bit_size(WN_desc(wn))) - 1;
      return inj_bits;
    }
    
  default:
    break;
  }

  return noinfo;
}


/* Look for and replace the pattern below with injui.
   x | ((a & mask) << shift)
   
   Check if bits are being inserted from WN_kid(1 - targ_idx) into
   WN_kid(targ_idx). */
static bool
inject_sequence (WN *parent_wn, WN *wn, INT targ_idx)
{
  INT shift = 0;
  INT bits = 0;
  INT val_idx = 1 - targ_idx;
  WN *parent_wn_val = wn;
  if (!is_mask_shift(WN_kid(parent_wn_val, val_idx),
                     bits, shift, parent_wn_val, val_idx))
    return false;
  
  INT inj_bits = value_bit_mask(WN_kid(wn, targ_idx));
  INT mask = (1U << bits) - 1;
  if ((inj_bits & (mask << shift)) != 0)
    return false;
  
  if (INS_TRACING) {
    fprintf(TFile, "\n### injui sequence found, replacing\n");
  }
  
  if (!replace_inject(parent_wn, wn,
                      wn, targ_idx,
                      parent_wn_val, val_idx,
                      shift, bits)) {
    if (INS_TRACING) {
      fprintf(TFile, "\n### injui sequence - replace failed.\n");
    }

    return false;
  }

  return true;
}


/* Reassociate an or-tree into a leftist tree, i.e.
   (a | (b | c)) becomes ((a | b) | c). */
static void
leftist_or_tree (WN *wn)
{
  if (WN_operator(wn) != OPR_BIOR)
    return;
  
  while (WN_operator(WN_kid1(wn)) == OPR_BIOR) {
    WN *kid0 = WN_kid0(wn);
    WN *kid1 = WN_kid1(wn);
    
    /* wn -> (a | (b | c)) */
    WN_kid1(wn) = WN_kid1(kid1);
    /* wn -> (a | c) */
    
    /* kid1 -> (b | c) */
    WN_kid1(kid1) = WN_kid0(kid1);
    /* kid1 -> (b | b) */
    WN_kid0(kid1) = kid0;
    /* kid1 -> (a | b) */

    WN_kid0(wn) = kid1;
    /* wn -> ((a | b) | c) */

    if (INS_TRACING) {
      fprintf(TFile, "\n### Leftist or-tree:\n");
      fdump_tree(TFile, wn);
    }
  }
}
  

/* Reassociate an or-tree so that 8- and 16-bit unsigned loads
   feeding directly an "or" are pushed towards the bottom of the
   tree. Return 'true' if the tree has been changed, 'false' otherwise. */
static bool
push_down_direct_loads (WN *wn)
{
  if (WN_operator(wn) != OPR_BIOR)
    return false;

  /* Check that kid1 is a direct load (trying to workaround noop
     converts). */
  WN *k1 = WN_kid1(wn);
  WN *ld = k1;
  if (WN_operator(ld) == OPR_CVT &&
      ((WN_desc(ld) == MTYPE_I4) || (WN_desc(ld) == MTYPE_U4)) &&
      ((WN_rtype(ld) == MTYPE_I4) || (WN_desc(ld) == MTYPE_U4)))
    ld = WN_kid0(ld);

  if (WN_operator(ld) != OPR_ILOAD &&
      WN_operator(ld) != OPR_LDID)
    return false;
  
  if (WN_rtype(ld) != MTYPE_U4)
    return false;
  
  if (WN_desc(ld) != MTYPE_U1 &&
      WN_desc(ld) != MTYPE_U2)
    return false;
  
  /* Check for overlap. If the loaded bits overlap in any way
     with the non-zero bits coming from kid 0, then pushing
     the load down may make "injui" inapplicable. */
  INT ld_bits = (1 << MTYPE_bit_size(WN_desc(ld))) - 1;
  INT obits = value_bit_mask(WN_kid0(wn));
  if ((obits & ld_bits) != 0)
    return false;
  
  /* Swap the load either with kid 0, or with kid 1 of kid 0,
     if kid 0 is an "or". Note that the check for the mask
     helps us avoid ping-ponging (infinite kid swap) in the case
     when we swap kid 0 and 1 of 'wn'. */
  WN *swap_parent = wn;
  INT swap_idx = 0;
  WN *swap = WN_kid(swap_parent, swap_idx);
  if (WN_operator(swap) == OPR_BIOR)
    {
      swap_parent = swap;
      swap_idx = 1;
      swap = WN_kid(swap_parent, swap_idx);
    }
  
  WN_kid1(wn) = swap;
  WN_kid(swap_parent, swap_idx) = k1;
  
  if (INS_TRACING) {
    fprintf(TFile, "\n### Push-down direct loads:\n");
    fdump_tree(TFile, wn);
  }

  return true;
}


static void
canonicalize_or_tree (WN *wn)
{
  leftist_or_tree(wn);
  
  /* If a load is pushed down the tree, we may need to canonicalize the
     or-tree into a leftist tree again. */
  if (push_down_direct_loads(wn))
    leftist_or_tree(wn);
}


static void
find_inject (WN *parent_wn, WN *wn)
{
  if (WN_operator(wn) == OPR_BLOCK) {
    for (WN *stmt = WN_first(wn); stmt; stmt = WN_next(stmt))
      find_inject(NULL, stmt);
    return;
  }
  
  canonicalize_or_tree(wn);
  
  /* Visit and transform the kids first, then the parent node. */
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    find_inject(wn, WN_kid(wn, i));
  }
  
  if (!parent_wn)
    return;

  if (WN_operator(wn) != OPR_BIOR)
    return;
  
  if (MTYPE_bit_size(WN_rtype(wn)) != 32)
    return;

  for (INT i = 0; i < 2; i++) {
    if (direct_inject(parent_wn, wn, i))
      return;
  }
  
  for (INT i = 0; i < 2; i++) {
    if (inject_sequence(parent_wn, wn, i))
      return;
  }
}


void
Find_Bit_Insert (WN *wn)
{
  
  if (!xt_injui || (inj_intrinsic() == INTRINSIC_INVALID)) {
    return;
  }
  
  if (INS_TRACING) {
    fprintf(TFile, "\n### Looking for injui ###\n");
    fdump_tree(TFile, wn);
  }
  
  find_inject(NULL, wn);
}
