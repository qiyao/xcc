// Copyright (c) 2003-2005 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_auto.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_AT: SIMD Auto Tie transformation                                    *
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

// $Id: simd_at.h $

#ifndef __SIMD_AT__
#define __SIMD_AT__

#include "at.h"
#include "at_op_xcc.h"
#include "at_xcc.h"
#include "simd.h"
#include "simd_imem.h"
#include "tf_defs.h"

class SIMD_AT;

//
// SIMD_AT
//

class SIMD_AT
{
private:
  MEM_POOL *_pool;
  XT_MEMPOOL *_at_pool;
  AT_PU *_at_pu;
  INT _max_width; // max width of SIMD
  AT_FACTORY *_factory; // autotie operation/type factory
  AT_STAT *_stat; // current AT_STAT
  SIMD_SELECT *_simd_select; // global SIMD select object
  WN *_inner_loop; // innermost loop
  tf_dfg_t _dfg; // data-flow graph
  SIMD_LOOP *_simd_loop; // current SIMD_LOOP

  /* Starting, ending and current analysis vector length. */
  INT _start_vl, _end_vl, _cur_vl;
  
  bool _trace;
  bool _analyzed; // true if the current loop has been analyzed
  bool _bad_oper; // true if analysis encountered a bad operator
  bool _pre_model;

  INT _outer_unroll;
  ARRAY_REF *_orig_arl;
  ARRAY_REF *_unroll_arl;
  INT _cur_unroll_scale; // current unroll scale
  
  typedef DYN_ARRAY<AT_MESSAGE *> MESSAGES;
  MESSAGES   *_messages;
  
  typedef HASH_TABLE<SIMD_EINFO *, tf_node_t> EINFO_NODE_MAP;
  EINFO_NODE_MAP *_einfo_node_map;
  
  typedef HASH_TABLE<SIMD_SCALAR *, tf_node_t> SINFO_NODE_MAP;
  SINFO_NODE_MAP *_sinfo_node_map;
  
  typedef HASH_TABLE<IMEM_INFO *, tf_node_t> IINFO_NODE_MAP;
  IINFO_NODE_MAP *_iinfo_node_map;
  
  typedef HASH_TABLE<WN *, WN *> WN_WN_MAP;
  WN_WN_MAP *_if_map;
  
  typedef HASH_TABLE<AT_TY *, int> AT_TY_INT_MAP;
  
  void        Add_Orig_Loop_Op(WN *, INVAR_TABLE *, INT);
  void        Add_Address_Overhead(WN *);
  void        Add_Alignment_Overhead(WN *);

  SIMD_LOOP  *Scan_Simd_Loop (SIMD_LOOP *loop);
  
  void        Add_Simd_Loop_Op (WN *wn, WN *enclosing_loop);
  
  void        Add_Simd_Field_Select_By_Table(bool setup,
					     WN *wn, TYPE_ID scalar_type,
					     INT num_fields, bool is_read);
  void        Add_Simd_Field_Select (bool setup,
				     WN *wn, TYPE_ID scalar_type,
				     INT num_fields, bool pairwise,
				     bool is_read);
  void        Add_Simd_Write_Select (bool setup, WN *wn, IMEM_ELEM *ie);
  void        Add_Simd_Read_Select (bool setup, WN *wn, IMEM_ELEM *ie);
  
  void        Add_Simd_Load_Shift_In (bool setup, IMEM_GROUP *ig);
  bool        Index_Address_Updated(WN *wn, WN *ls_wn, IMEM_GROUP *ig);
  void        Add_Simd_Indexed_Load_Store(bool setup, IMEM_GROUP *ig, 
					  WN *ls, bool is_load);
  bool        Add_Simd_Load_Store (WN *wn, SIMD_EINFO *e_info, bool setup);
  void        Add_Simd_Reduction (WN *wn);
  
  void        Request_Kid_Conversion (bool setup, WN *wn);
  
  void        Set_Bad_Oper_Msg (const AT_MSG_ID msg_id, const WN *wn, ...);
  
  void        Annotate_Vector_Symbol_Tree(SYMBOL_TREE_NODE *,
					  LNO_REGCLASS_INFO *);
  
  tf_node_t   Build_DFG_Const (WN *wn);
  tf_node_t   Build_DFG_Default_Rec (WN *wn);
  tf_node_t   Build_DFG_Iload (WN *wn);
  tf_node_t   Build_DFG_Istore_Rec (WN *wn);
  tf_node_t   Build_DFG_If_Rec (WN *wn);
  tf_node_t   Build_DFG_Ldid (WN *wn);
  tf_node_t   Build_DFG_Stid_Rec (WN *wn);
  tf_node_t   Build_DFG_Parm_Rec (WN *wn);
  tf_node_t   Build_DFG_Select_Rec (WN *wn);
  tf_node_t   Build_DFG_Rec (WN *wn);
  void        Build_DFG (void);
  
  void        Add_Simd_Load_Reuse_Shift(bool setup, IMEM_ELEM *ie);
  void        Split_Group(IMEM_GROUP *ig, IMEM_GROUP_Array *ig_array, 
			  int vl);
  void        Remerge_Group(IMEM_GROUP *ig_old, IMEM_GROUP_Array *ig_array);
  
  void        Set_Reg_Usage (AT_REGION *region, AT_TY *ty, int count, bool vector_type);
  
  void Setup_Outer_Unroll (void);
  INT Get_Imem_Group_Unroll_Scale (IMEM_GROUP *ig);

public:
  SIMD_AT (MEM_POOL *pool, INT max_width =16);
  
  MEM_POOL *        Pool (void) { return _pool; }
  XT_MEMPOOL *      AT_pool (void) { return _at_pool; }
  AT_PU*            AT_Pu (void) const { return _at_pu; }
  INT               Max_Width (void) const { return _max_width; }
  AT_FACTORY *      Factory (void) const { return _factory; }
  AT_TY_TAB *       Ty_Tab (void ) const { return Factory()->ty_tab(); }
  AT_OP_TAB *       Op_Tab (void ) const { return Factory()->op_tab(); }
  AT_REGION *       Last_Region (void);
  bool              Trace (void) const { return _trace; }
  
  AT_STAT *         Stat (void) const { return _stat; }
  void              Set_Stat (AT_STAT *stat =NULL) { _stat = stat; }
  
  SIMD_SELECT *     Simd_Select (void) const { return _simd_select; }
  void              Set_Simd_Select (SIMD_SELECT *simd_select) { _simd_select = simd_select; }
  
  WN *              Inner_Loop (void) const { return _inner_loop; }
  void              Set_Inner_Loop (WN *inner_loop) { _inner_loop = inner_loop; }
  
  tf_dfg_t          DFG (void) const { return _dfg; }
  void              Set_DFG (tf_dfg_t dfg) { _dfg = dfg; }
  
  SIMD_LOOP *       Simd_Loop (void) const { return _simd_loop; }
  void              Set_Simd_Loop (SIMD_LOOP *simd_loop) { _simd_loop = simd_loop; }
  
  bool              Analyzed() const { return _analyzed; }
  void              Set_Analyzed()   { _analyzed=true; }
  void              Reset_Analyzed() { _analyzed=false; }
  
  bool Bad_Operator (void) const { return _bad_oper; }
  void Set_Bad_Operator (void) { _bad_oper = true; }
  void Reset_Bad_Operator (void) { _bad_oper = false; }

  bool Pre_Model (void) const { return _pre_model; }
  void Set_Pre_Model (void) { _pre_model = true; }
  void Reset_Pre_Model (void) { _pre_model = false; }

  void Init_Vector_Length_Range (void);
  
  void Clear_Messages (void);
  void Add_Message (AT_MESSAGE *message);
  INT Message_Count (void) const { return _messages->Elements(); }
  AT_MESSAGE *Get_Message (INT i) { return _messages->Get(i); }
  
  EINFO_NODE_MAP   *EInfo_Node_Map (void) const { return _einfo_node_map; }
  void              Set_EInfo_Node_Map (EINFO_NODE_MAP *map) { _einfo_node_map = map; }
  
  SINFO_NODE_MAP   *SInfo_Node_Map (void) const { return _sinfo_node_map; }
  void              Set_SInfo_Node_Map (SINFO_NODE_MAP *map) { _sinfo_node_map = map; }
  
  IINFO_NODE_MAP   *IInfo_Node_Map (void) const { return _iinfo_node_map; }
  void              Set_IInfo_Node_Map (IINFO_NODE_MAP *map) { _iinfo_node_map = map; }
  
  WN_WN_MAP         *If_Map (void) const { return _if_map; }
  void              Set_If_Map (WN_WN_MAP *map) { _if_map = map; }
  
  // add original loop statistics to the current AT_PU for loop 'wn'
  void              Add_Orig_Loop (WN *, INVAR_TABLE *, INT);
  
  // request an operator from Auto TIE and trace it
  // if setup is true, request a setup operation,
  // if setup is false, request an inner loop operation
  void        Request_Op (WN *wn, bool setup, AT_OP_ID at_op_id, INT cnt =1);
  
  SIMD_LOOP *Pre_Model_Simd_Loop (SIMD_LOOP *simd_loop);
  void Add_Simd_Loop (SIMD_LOOP *simd_loop);
  
  
  // annotate vector and alignment information on the array reference and symbol
  // nodes
  void              Annotate_Vector_Info(ARRAY_REF *, SYMBOL_TREE *,
					 SIMD_LOOP *);
  
  // write out the register usage as estimated by the loop model
  void              Output_Register_Usage(LNO_REGS *);
  void              Finalize_Region (void);
};

extern AT_WN_DECODE AT_WN_Decode_SIMD_EINFO (const WN *);

extern void Set_Cur_At_Simd_Loop(SIMD_LOOP *simd_loop =NULL);


#endif /* __SIMD_AT__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
