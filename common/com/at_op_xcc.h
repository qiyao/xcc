// Copyright (c) 2003 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// at_op_xcc.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  Auto TIE types and operators                                             *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.

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

// $Id: at_op_xcc.h $

#ifndef __AT_OP_XCC__
#define __AT_OP_XCC__

#include "at_defs.h"
#include "defs.h"
#include "mtypes.h"
#include "opcode.h"
#include "tf_defs.h"
#include "topcode.h"
#include "wn.h"
#include "tie.h"
#include "cxx_memory.h"
#include "xt_mempool.h"


// AT_WN_DECODE and AT_WN_DECODE_FUNC allow dynamic decoding of a WN node.
// This is necessary because routines (such as SIMD) may want to override the original
// field in a WN based on external information.
typedef struct {
  TYPE_ID rtype;
  TYPE_ID desc;
} AT_WN_DECODE;

typedef AT_WN_DECODE (*AT_WN_DECODE_FUNC)(const WN *);

extern AT_WN_DECODE at_wn_decode_regular (const WN *wn);

//
// AT_FACTORY
//

class AT_FACTORY
{
private:
  XT_MEMPOOL *_pool;
  
  AT_TY_TAB *_ty_tab;
  AT_OP_TAB *_op_tab;
  
  AT_WN_DECODE_FUNC _wn_decode_func;
  int _default_vl;
    
public:
  AT_FACTORY (XT_MEMPOOL *pool, AT_OP_TAB *op_tab);
  ~AT_FACTORY ();
  
  XT_MEMPOOL *pool (void) const { return _pool; }
  
  void set_op_tab (AT_OP_TAB *op_tab);
  AT_OP_TAB *op_tab (void) const { return _op_tab; }
  AT_TY_TAB *ty_tab (void) const { return _ty_tab; }
  
  AT_WN_DECODE_FUNC wn_decode_func (void) const { return _wn_decode_func; }
  void set_wn_decode_func (AT_WN_DECODE_FUNC func) { _wn_decode_func = func; }
  void default_wn_decode_func (void) { _wn_decode_func = &at_wn_decode_regular; }
  
  int default_vl (void) const { return _default_vl; }
  void set_default_vl (int vl) { _default_vl = vl; }
  
  AT_TY_ID get_at_ty_id (AT_TY *temp_ty);
  
  // Create an Auto TIE type corresponding to 'type', 'vl' and 'flags' and
  // return its id.
  AT_TY_ID get_at_ty_id (TYPE_ID type, INT vl, UINT flags);
  AT_TY_ID get_at_ty_id (AT_TIE_TYPE *type, INT vl, UINT flags);
  AT_TY_ID get_at_ty_id (AT_TIE_STATE *state, INT vl, UINT flags);
  
  AT_OP_ID get_at_op_id (AT_OP *temp_op);
  
  // Create an Auto TIE operator corresponding to 'wn' and 
  // return its id.
  AT_OP_ID get_at_op_id (const WN *wn);
  
  AT_OP_ID get_at_op_id_general (const WN *wn);
  AT_OP_ID get_at_op_id_compare (const WN *wn);
  AT_OP_ID get_at_op_id_shift (const WN *wn);
  AT_OP_ID get_at_op_id_cvtl (const WN *wn);
  AT_OP_ID get_at_op_id_load (const WN *wn,
			      bool scalar_mem =false,
			      bool indexed =false,
			      bool updating =false,
			      INT offset =0);
  AT_OP_ID get_at_op_id_store (const WN *wn,
			       bool indexed =false,
			       bool updating =false,
			       INT offset =0);
  AT_OP_ID get_at_op_id_intrinsic (const WN *wn);
  
  // loads
  AT_OP_ID get_at_op_id_load (TYPE_ID rtype, TYPE_ID desc,
			      bool scalar_mem =false,
			      bool indexed =false,
			      bool updating =false,
			      INT offset = 0);
  
  // MADD/MSUB
  AT_OP_ID get_at_op_id_mac (const WN *wn);
  
  // CMOV
  AT_OP_ID get_at_op_id_cmov(TYPE_ID desc);
  
  // Alignment load/store support
  AT_OP_ID get_at_op_id_prime (const WN *wn, bool store_flush =false);
  AT_OP_ID get_at_op_id_alignment (const WN *wn, bool updating =true);
  
  // Vector select
  AT_OP_ID get_at_op_id_vselect (bool dual, TYPE_ID scalar_type,
				 UINT64 sel_val, INT sel_bits);
  
  // Type conversion
  AT_OP_ID get_at_op_id_convert (AT_TY_ID from_type, AT_TY_ID to_type);
  
  // Sum reduction
  AT_OP_ID get_at_op_id_radd (TYPE_ID scalar_type);
  
  /* Min/max reduction. */
  AT_OP_ID get_at_op_id_rminmax (TYPE_ID scalar_type, bool is_min);
  
  AT_OP_ID get_at_op_id_bxor (TYPE_ID scalar_type);
  AT_OP_ID get_at_op_id_zero (TYPE_ID scalar_type);
  
  // Convert from a machine type TYPE_ID to Auto TIE ATYPE.
  // Return ATYPE_UNKNOWN if no matching ATYPE can be found.
  static ATYPE mtype_to_atype (TYPE_ID type);
  
  // Convert from Auto TIE ATYPE to a machine type TYPE_ID.
  // Return MTYPE_UNKNOWN if no matching MTYPE can be found.
  static TYPE_ID atype_to_mtype (TYPE_ID type);
  
  // Convert from a Whirl OPERATOR to Auto TIE ATOP.
  // Return ATOP_UNKNOWN if no matching ATOP can be found.
  static ATOP opr_to_atop (OPERATOR oper);
  
  // Convert from a Whirl OPERATOR to TIE Fusion DFG node kind.
  // Return TFN_UNKNOWN if no matching kind can be found.
  static tf_node_kind_t opr_to_node_kind (OPERATOR oper);
  
  // Find the canonical type of kid 'kid_idx' of 'wn'.
  static TYPE_ID canonical_kid_type (const WN *wn, INT kid_idx,
				     AT_WN_DECODE_FUNC wn_decode_func =&at_wn_decode_regular);
}; // AT_FACTORY

#endif /* __AT_OP_XCC__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
