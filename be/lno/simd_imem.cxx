// simd_imem.cpp
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Automatic SIMD                                                         *
 *                                                                           *
 *    Functions for IMEM data structures                                     *
 *                                                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.

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

// $Id: simd_imem.cpp $

#include <stdio.h>
#include "lnoutils.h"
#include "opt_du.h"
#include "simd.h"
#include "simd_imem.h"
#include "simd_select.h"
#include "simd_ti.h"
#include "simd_at.h"
#include "config_opt.h"
#include "lego_util.h"

/* in in out    r/w f vl    */
static VSEL_OP vsel_list_r_3_2[] = {
 { 0, 1, 3, 
   {  0,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 2, 4, 
   {  1,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 1, 2, 5, 
   {  0,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_r_3_4[] = {
 { 0, 1, 3, 
   {  0,  3,  6,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 1, 4, 
   {  1,  4,  7,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 1, 2, 5,
   {  5,  1,  4,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 4, 2, 4,
   {  0,  1,  2,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 3, 5, 5,
   {  3,  5,  6,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 3, 2, 3,
   {  0,  1,  2,  5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_r_3_8[] = {
 { 0, 1, 3, 
   {  0,  3,  6,  9, 12, 15,  2,  5, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 1, 4, 
   {  1,  4,  7, 10, 13,  8, 11, 14, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 1, 2, 5,
   {  1,  4,  0,  3,  6,  9, 12, 15, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 4, 2, 4,
   {  0,  1,  2,  3,  4,  8, 11, 14, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 3, 5, 5,
   {  6,  7, 10, 11, 12, 13, 14, 15, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 3, 2, 3,
   {  0,  1,  2,  3,  4,  5, 10, 13, -1, -1, -1, -1, -1, -1, -1, -1}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_r_3_16[] = {
 { 0, 1, 3, 
   {  0,  3,  6,  9, 12, 15, 18, 21, 24, 27, 30,  2,  5,  8, 11, 14}},
 { 0, 1, 4, 
   {  1,  4,  7, 10, 13, 16, 19, 22, 25, 28, 31,  2,  5,  8, 11, 14}},
 { 1, 2, 5,
   { 17, 20, 23, 26, 29,  1,  4,  7, 10, 13, 16, 19, 22, 25, 28, 31}},
 { 4, 2, 4,
   {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 18, 21, 24, 27, 30}},
 { 3, 5, 5,
   { 11, 12, 13, 14, 15, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31}},
 { 3, 2, 3,
   {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 17, 20, 23, 26, 29}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

/* in in out    r/w f vl    */
static VSEL_OP vsel_list_w_3_2[] = {
 { 0, 1, 3, 
   {  0,  2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 2, 4, 
   {  2,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 1, 2, 5, 
   {  1,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_w_3_4[] = {
  { 0, 1, 3,
    {  0,  4,  4,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  { 3, 2, 3,
    {  0,  1,  4,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  { 0, 1, 4,
    {  5,  5,  2,  6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  { 4, 2, 4,
    {  0,  5,  2,  3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  { 0, 1, 5,
    {  3,  3,  7,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  { 5, 2, 5,
    {  6,  1,  2,  7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}},
  {-1,-1,-1,
   { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_w_3_8[] = {
 { 0, 1, 3,
   {  0,  8,  8,  1,  9,  9,  2, 10, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 3, 2, 3,
   {  0,  1,  8,  3,  4,  9,  6,  7, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 1, 4, 
   {  3,  3, 11,  4,  4, 12,  5,  5, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 4, 2, 4,
   { 10,  1,  2, 11,  4,  5, 12,  7, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 0, 1, 5,
   { 13, 13,  6, 14, 14,  7, 15, 15, -1, -1, -1, -1, -1, -1, -1, -1}},
 { 5, 2, 5,
   {  0, 13,  2,  3, 14,  5,  6, 15, -1, -1, -1, -1, -1, -1, -1, -1}},
 {-1,-1,-1, 
   { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

static VSEL_OP vsel_list_w_3_16[] = {
 { 0, 1, 3, 
   {  0, 16, 11,  1, 17, 12,  2, 18, 13,  3, 19, 14,  4, 20, 15,  5}},
 { 0, 1, 4, 
   { 21, 21,  6, 22, 22,  7, 23, 23,  8, 24, 24,  9, 25, 25, 10, 26}},
 { 1, 2, 5,
   { 26, 26, 11, 27, 27, 12, 28, 28, 13, 29, 29, 14, 30, 30, 15, 31}},
 { 4, 2, 4,
   {  0, 21,  2,  3, 22,  5,  6, 23,  8,  9, 24, 11, 12, 25, 14, 15}},
 { 3, 5, 5,
   { 16,  2, 18, 19,  5, 21, 22,  8, 24, 25, 11, 27, 28, 14, 30, 31}},
 { 3, 2, 3,
   {  0,  1, 16,  3,  4, 17,  6,  7, 18,  9, 10, 19, 12, 13, 20, 15}},
 {-1,-1,-1, 
  { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1}}
};

void fprint_indent (FILE *out_file, int indent)
{
    if (indent > 0) {
	fprintf(out_file, "%*s", indent, "");
    }
}

static INT
divceil (INT a, INT b)
{
    return (a + b - 1)/b;
}

static WN *
Linearize_Array_Addr (WN *array)
{
    Is_True(WN_operator(array) == OPR_ARRAY, 
	    ("Linearize_Array_Addr: input is not an array"));
    INT      n            = WN_num_dim(array);
    WN_ESIZE element_size = WN_element_size(array);
    Is_True(WN_operator(array)>0, ("Linearize_Array_Addr: element size <= 0"));
    TYPE_ID  rtype        = WN_rtype(array);
    
    WN *result = WN_array_index(array, n-1);
    for (INT i = n-2; i >= 0; i--) {
	WN *product = 
	    LWN_Copy_Tree(WN_array_dim(array, n-1), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(WN_array_dim(array, n-1), product, Du_Mgr);
	for (INT m=n-2; m>i; m--) {
	    WN *copy_dim = 
		LWN_Copy_Tree(WN_array_dim(array, m), TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(WN_array_dim(array, m), copy_dim, Du_Mgr);
	    product = AWN_Mpy(rtype, product, copy_dim);
	}
	WN *mpy = AWN_Mpy(rtype, WN_array_index(array, i), product);
	result = AWN_Add(rtype, result, mpy);
    }
    WN *elm_size = WN_Intconst(rtype, element_size);
    result = AWN_Add(rtype, WN_array_base(array), 
		     AWN_Mpy(rtype, result, elm_size));
    WN_Delete(array);
    return result;
}

static bool
Update_Const_Fit_Immediate(OPERATOR op, WN* cst, 
                           TYPE_ID res_type, 
                           TYPE_ID desc_type, bool is_vector)
{
  // check if constant falls in the immediate range of updating load/store
  TIE_MACRO_ID macro_id = 
    Simd_Info->Search_Vector_Macro(op, desc_type, res_type, 
                                   is_vector ? 0 : 1, true);
  if (macro_id != TIE_INVALID_ID) {
    TIE_MACRO_p tie_macro = tie_info->tie_macro(macro_id);
    INT const_val = WN_const_val(cst);
    if (tie_macro->immediate_ok(tie_macro->num_protos()-1, const_val)) {
      return true;
    }
  }
  return false;
}

static bool
multi_op_memory_proto (OPERATOR op, TYPE_ID res_type, TYPE_ID desc_type)
{
  TIE_MACRO_ID macro_id = 
    Simd_Info->Search_Vector_Macro(op, desc_type, res_type, 0, true);

  if (macro_id == TIE_INVALID_ID)
    return false;
  
  TIE_MACRO_p tie_macro = tie_info->tie_macro(macro_id);
  return (tie_macro->num_instructions() > 1);
}

//
// IMEM_Map
//

IMEM_Map::IMEM_Map(SIMD_LOOP *simd_loop, MEM_POOL *pool) :
    _array(pool), _groups(pool), _pool(pool), _simd_loop(simd_loop),
    _vsel_r_table(pool), _vsel_w_table(pool)
{
    _all_lod_sto = CXX_NEW(WN_ARRAY(pool), pool);
    Initialize_Vsel_Imems(pool);
}

IMEM_Map::~IMEM_Map () { }

void
IMEM_Map::Initialize_Vsel_Imems(MEM_POOL *pool)
{
    VSEL_IMEM *	vsel_t = CXX_NEW(VSEL_IMEM(3, 2, vsel_list_r_3_2), pool);
    _vsel_r_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 4, vsel_list_r_3_4), pool);
    _vsel_r_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 8, vsel_list_r_3_8), pool);
    _vsel_r_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 16, vsel_list_r_3_16), pool);
    _vsel_r_table.AddElement(vsel_t);

    vsel_t = CXX_NEW(VSEL_IMEM(3, 2, vsel_list_w_3_2), pool);
    _vsel_w_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 4, vsel_list_w_3_4), pool);
    _vsel_w_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 8, vsel_list_w_3_8), pool);
    _vsel_w_table.AddElement(vsel_t);
    vsel_t = CXX_NEW(VSEL_IMEM(3, 16, vsel_list_w_3_16), pool);
    _vsel_w_table.AddElement(vsel_t);
}

/*---------------------------------------------------------------------------*
 * Mapping from array reference to shared IMEM_INFO                          *
 *---------------------------------------------------------------------------*/
IMEM_INFO *
IMEM_Map::Find(WN *expr)
{
    for (INT i = 0; i < _array.Elements(); i++) {
	IMEM_INFO *cur = _array[i];
	if (cur->Is_Same_Array_Access(expr)) {
	    return cur;
	}
    }
    return NULL;
}

/*---------------------------------------------------------------------------*
 * Add dependence for pending load/store                                     *
 *---------------------------------------------------------------------------*/
void
IMEM_Map::Add_Store_Dependence(MEM_POOL *pool)
{
    for (INT i = 0; i < _groups.Elements(); i++) {
	IMEM_GROUP *cur = _groups[i];
	if (cur->Pending_Wn().Elements()) {
	    cur->Add_Store_Dependence(pool);
	}
    }
}

/*---------------------------------------------------------------------------*
 * Clear SIMD_EINFO field                                                    *
 *---------------------------------------------------------------------------*/
void 
IMEM_Map::Clear_Simd_EInfo(void)
{
    for (INT i = 0; i < _array.Elements(); ++i) {
	IMEM_INFO *cur = _array[i];
	cur->Set_E_Info(NULL);
    }
}

bool
IMEM_Map::Setup_Access_Properties(bool before_transform) {
  SIMD_LOOP *simd_loop = Simd_Loop();
  Is_True(simd_loop->Simd_Loop_Level()>=0, ("Unexpected SIMD loop level"));
  
  MEM_POOL_Popper popper(Pool());
  
  for (INT i = 0; i < _array.Elements(); ++i) {
    IMEM_INFO *cur = _array[i];

    /* Info messages printed inside. */
    if (!cur->Check_Stride()) {
	return false;
    }

    cur->Setup_Vector_Property(simd_loop);
    if (cur->Is_Vector()) {
	cur->Set_Alignment(simd_loop);
    }
  }
  return true;
}

void
IMEM_Map::Order_Specific_Setup(LOOP_MODEL *lm, INT num_loops,
			       SIMD_LOOP *simd_loop, bool before_transform,
			       INT inner, bool msg) {
    Is_True(simd_loop->Simd_Loop_Level()>=0, ("Unexpected SIMD loop level"));
    
    simd_loop->Reset_Bad_Stride();

    for (INT i = 0; i < _array.Elements(); i++) {
	IMEM_INFO *cur = _array[i];
	cur->Setup_Load_Property(lm, num_loops, simd_loop, before_transform, inner, msg);
	if (simd_loop->Bad_Stride())
          break;
    }
}

void
IMEM_Map::Setup_Index_Value(void) {
    for (INT i=0; i<_groups.Elements(); i++)
	_groups[i]->Setup_Index_Value();
}

void
IMEM_Map::Delete_Index_Value(void) {
  for (INT i=0; i<_groups.Elements(); i++) {
    _groups[i]->Delete_Index_Value();
  }
}

void
IMEM_Map::Allocate_Regs(void) {
    for (INT i=0; i<_groups.Elements(); i++)
	_groups[i]->Allocate_Regs();
}

void
IMEM_Map::Print_Groups(FILE *out_file, INT indent) {
    for (INT i=0; i<_groups.Elements(); i++)
	_groups[i]->Print(out_file,indent);
}

INT
IMEM_Map::Write_Group_Count(void)
{
    INT cnt = 0;
    for (INT i = 0; i < _groups.Elements(); i++) {
	IMEM_GROUP *ig = _groups[i];
	if (ig->Is_Vector() && ig->Is_Def()) {
	    cnt++;
	}
    }
    return cnt;
}

INT
IMEM_Map::Unaligned_Write_Group_Count(void)
{
    INT cnt = 0;
    for (INT i = 0; i < _groups.Elements(); i++) {
	if (_groups[i]->Is_Vector() && _groups[i]->Is_Def() && 
	    !_groups[i]->Is_Aligned()) {
	    cnt++;
	}
    }
    return cnt;
}

INT
IMEM_Map::Read_Group_Count(void)
{
    INT cnt = 0;
    for (INT i = 0; i < _groups.Elements(); i++) {
	if (_groups[i]->Is_Vector() && _groups[i]->Is_Use()) {
	    cnt++;
	}
    }
    return cnt;
}

INT
IMEM_Map::Unaligned_Read_Group_Count(void)
{
    INT cnt = 0;
    for (INT i = 0; i < _groups.Elements(); i++) {
	if (_groups[i]->Is_Vector() && _groups[i]->Is_Use() && 
	    !_groups[i]->Is_Aligned()) {
	    cnt++;
	}
    }
    return cnt;
}


void
IMEM_Map::Reset_AT_Vl (void)
{
  for (INT i = 0; i < _groups.Elements(); i++)
    _groups[i]->Set_AT_Vl(0);
}


IMEM_GROUP::IMEM_GROUP (MEM_POOL *pool) :
    _mem_pool(pool), _flags(0), _non_const_level(-1),
    _load_align_reg(NULL), _store_align_reg(NULL),
    _store_addr_reg(NULL), _load_addr_reg(NULL), _index_addr_reg(NULL),
    _store_copy(NULL), _store_sel_reg(NULL), _store_index_reg(NULL),
    _invar_point(NULL), _pending_wn(pool),  _index_value(NULL),
    _scalar_type(MTYPE_UNKNOWN), _at_vl(0), _reuse_length(0)
{
    _elems    = CXX_NEW(IMEM_ELEMS(Mem_Pool()),Mem_Pool());
    _res_regs = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
    _loads    = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _stores   = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_loads  = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_stores = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_load_vertices  = CXX_NEW(VINDEX16_ARRAY(Mem_Pool()),Mem_Pool());
    _v_store_vertices = CXX_NEW(VINDEX16_ARRAY(Mem_Pool()),Mem_Pool());
}

IMEM_GROUP::IMEM_GROUP(IMEM_GROUP *ig, MEM_POOL *pool) :
    _mem_pool(pool), _flags(0), _non_const_level(-1),
    _load_align_reg(NULL), _store_align_reg(NULL),
    _store_addr_reg(NULL), _load_addr_reg(NULL), _index_addr_reg(NULL),
    _store_copy(NULL), _store_sel_reg(NULL), _store_index_reg(NULL),
    _invar_point(NULL), _pending_wn(pool), _index_value(NULL),
    _scalar_type(MTYPE_UNKNOWN), _at_vl(0), _reuse_length(0)
{
    _elems    = CXX_NEW(IMEM_ELEMS(Mem_Pool()),Mem_Pool());
    _res_regs = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
    _loads    = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _stores   = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_loads  = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_stores = CXX_NEW(WN_ARRAY(Mem_Pool()),Mem_Pool());
    _v_load_vertices  = CXX_NEW(VINDEX16_ARRAY(Mem_Pool()),Mem_Pool());
    _v_store_vertices = CXX_NEW(VINDEX16_ARRAY(Mem_Pool()),Mem_Pool());
    
    Set_Flags(ig->Flags());
    Set_Scalar_Type(ig->Scalar_Type());
    Set_Non_Const_Level(ig->Non_Const_Level());
    Set_Base_Addr(ig->Base_Addr());
    Set_AT_Vl(ig->AT_Vl());
}
    
IMEM_GROUP::~IMEM_GROUP () {
    CXX_DELETE(_elems,Mem_Pool());
    CXX_DELETE(_res_regs,Mem_Pool());
    CXX_DELETE(_loads,Mem_Pool());
    CXX_DELETE(_stores,Mem_Pool());
}

void
IMEM_GROUP::Set_Elem (INT i, IMEM_ELEM *ie) {
    Elems()->Set(i,ie);
    ie->Set_Parent_Group(this);
}

void
IMEM_GROUP::Add_Elem (IMEM_ELEM *ie) {
    Set_Elem(Elems()->Newidx(),ie);
}

WN *
IMEM_GROUP::Compute_Index_Value(WN *t_loop)
{
    /* generate index value */
  WN *old_addr_c[2];
  for (INT i = 0; i < 2; i++) {
    old_addr_c[i] = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(Base_Addr(), old_addr_c[i], Du_Mgr);
  }

  /* replace WN_index(t_loop) with 0 */
  WN *cst_0 = WN_CreateIntconst(OPC_I4INTCONST, 0);
  Replace_Ldid_With_Exp_Copy(WN_index(t_loop), old_addr_c[0],
                             cst_0,
                             Du_Mgr, Array_Dependence_Graph);
  WN_Delete(cst_0);
    
  /* replace WN_index(t_loop) with 'step' */
  Replace_Ldid_With_Exp_Copy(WN_index(t_loop), old_addr_c[1],
                             Loop_Step(t_loop),
                             Du_Mgr, Array_Dependence_Graph);
  
  WN *k0[2];
  WN *k1[2];
  for (INT i = 0; i < 2; i++) {
    if (WN_operator(old_addr_c[i]) == OPR_ARRAY) {
      old_addr_c[i] = Linearize_Array_Addr(old_addr_c[i]);
    }

    /* Workaround some optimizer problems so that the base LDA's can be
       simplified. */
    if (WN_operator(old_addr_c[i]) == OPR_ADD) {
      k0[i] = WN_kid0(old_addr_c[i]);
      k1[i] = WN_kid1(old_addr_c[i]);
    } else {
      k0[i] = old_addr_c[i];
      k1[i] = WN_CreateIntconst(OPC_I4INTCONST, 0);
    }
  }
  
  OPCODE  opc    = OPCODE_make_op(OPR_SUB, MTYPE_U4, MTYPE_V);
  WN *p0 = LWN_CreateExp2(opc, k0[1], k0[0]);
  WN *p1 = LWN_CreateExp2(opc, k1[1], k1[0]);
  opc = OPCODE_make_op(OPR_ADD, MTYPE_U4, MTYPE_V);
  WN *index_val = LWN_CreateExp2(opc, p0, p1);
  index_val = LWN_Simplify_Tree(index_val);
  return index_val;
}


void
IMEM_GROUP::Allocate_Regs (void)
{
  for (INT i = 0; i < Elem_Count(); i++)
    Elem(i)->Allocate_Regs();
  
  Allocate_Res_Regs();
  
  if (Is_Vector() && !Is_Aligned()) {
    if (Is_Use()) {
      Set_Load_Align_Reg(Simd_Info->Gen_Align_Symbol(Scalar_Type(), Mem_Pool()));
      Set_Load_Addr_Reg(Generate_Align_Addr_Reg(Simd_Info,Mem_Pool()));
      Set_Updating_Load();
    } 
    if (Is_Def()) {
      Set_Store_Align_Reg(Simd_Info->Gen_Align_Symbol(Scalar_Type(), Mem_Pool()));
      Set_Store_Addr_Reg(Generate_Align_Addr_Reg(Simd_Info,Mem_Pool()));
      Set_Store_Copy(Gen_Symbol(Simd_Info,Scalar_Type(),Mem_Pool()));
      Set_Store_Sel_Reg(Simd_Info->Gen_Sel_Symbol(Mem_Pool()));
    }

    /* Can't handle indexed unaligned loads and stores. */
    Delete_Index_Value();
  }
  
  if (Index_Value()) {
    if (Simd_Info->Has_Update_Indexed_Load_Store(this, Is_Use(), Is_Def()) ||
        Index_Const_Fit_Immediate() ||
        /* shared index and update */
        (Is_Use() && Is_Def() && (Res_Reg_Count() == 1) && 
         (Def_Before_Use() &&
          Simd_Info->Has_Update_Indexed_Load_Store(this, false, true) ||
          Use_Before_Def() &&
          Simd_Info->Has_Update_Indexed_Load_Store(this, true, false)))) {
      Set_Index_Addr_Reg(Gen_Symbol(MTYPE_U4, Mem_Pool(), "xld"));
      if (Res_Reg_Count() == 1) {// share the same address register
        Set_Addr_Shared();
        SIMD_PREG *sym = Gen_Symbol(MTYPE_U4, Mem_Pool(), "addr");
        if (Is_Def()) {
          Set_Store_Addr_Reg(sym);
        }
        if (Is_Use()) {
          Set_Load_Addr_Reg(sym);
        }
      } else {
        if (Is_Def()) {
          Set_Store_Addr_Reg(Gen_Symbol(MTYPE_U4, Mem_Pool(), "addr"));
        }
        if (Is_Use()) {
          Set_Load_Addr_Reg(Gen_Symbol(MTYPE_U4, Mem_Pool(), "addr"));
        }
      }
    } else {
      Delete_Index_Value();
    }
  }
  
  if (Is_Def()) {
    WN *last_store = Stores()->Get(Stores()->Elements()-1);
    SIMD_EINFO *e_info = Cur_Simd_Loop->Get_E_Info(last_store);
    e_info->Set_Store_Back();
  }
}


bool
IMEM_GROUP::Use_Before_Def(void)
{
    for (INT i=0; i<Elem_Count(); i++) {
	IMEM_ELEM *ie = Elem(i);
	for (INT j=0; j<ie->Offset_Count(); j++) {
	    if (!ie->Offset(j)->Has_Exp_Use()) {
		return false;
	    }
	}
    } 
    return true;
}

bool
IMEM_GROUP::Def_Before_Use(void)
{
    for (INT i=0; i<Elem_Count(); i++) {
	IMEM_ELEM *ie = Elem(i);
	for (INT j=0; j<ie->Offset_Count(); j++) {
	    if (ie->Offset(j)->Has_Exp_Use()) {
		return false;
	    }
	}
    } 
    return true;
}

INT
IMEM_GROUP::Distance(void)
{
    if (Elem_Count() == 1) {
	return 0;
    }

    IMEM_ELEM *first_elem = Elem(0);
    IMEM_ELEM *last_elem  = Elem(Elem_Count()-1);
    IMEM_INFO *first_imem = first_elem->Offset(0)->Imem_Info();
    IMEM_INFO *last_imem  = last_elem->Offset(0)->Imem_Info();
    
    INT idiff;
    last_imem->Compare_Imem_Index(first_imem, &idiff);
    Is_True(idiff>0, ("IMEM_GROUP::Distance: Unexpected zero distance"));
    return idiff;
}

void
IMEM_GROUP::Allocate_Res_Regs (void) 
{
    if (Elem_Count()==1) {
	SIMD_PREGS_Copy(Elem(0)->Res_Regs(),Res_Regs());
    } else {
	Is_True(!Is_Reduction(), ("Unexpected multi-elements for reduction"));
	TYPE_ID scalar_type   = Scalar_Type();
	INT     simd_width    = Simd_Info->Get_SIMD_Width_Scalar(scalar_type);
	INT     group_width   = Distance();
	INT     elem_length   = Elem(0)->Res_Reg_Count();
	INT     elem_to_load  = elem_length * simd_width;
	Is_True(group_width <= elem_to_load,
		("IMEM_GROUP::Allocate_Res_Regs: group width larger than loaded elements"));
	INT     reuse_length  = divceil(group_width * Elem(0)->Offset_Count(),
				  simd_width);
	Set_Reuse_Length(reuse_length);
	INT     regs = elem_length + reuse_length; 

	for (INT i = 0; i < regs; i++) {
	    Add_Res_Reg(Gen_Symbol(Simd_Info, Scalar_Type(), Mem_Pool()));
	}
    }
}

void 
IMEM_GROUP::Load_Variable_Stride_Reg(
    WN *load, WN *stmt, INT idx, SIMD_PREG *var_reg, WN *orig_ls,
    SIMD_PREG *addr_preg, SIMD_PREG *idx_preg, bool update)
{
    WN      *res      = NULL;
    TYPE_ID  res_type = var_reg->Type();
	
    if (idx > 0) {
	/* generate an updating index load */
	res = Simd_Info->Generate_LoadS_X(OPR_ILOAD,
					  res_type, 
					  WN_desc(orig_ls),
					  stmt,
					  addr_preg,
					  var_reg,
					  idx_preg, update);
    } else {
	res = Simd_Info->Generate_Load(OPR_ILOAD,
				       res_type, stmt,
				       addr_preg->Simd_Preg_Ldid(),
				       0,
				       var_reg,
				       WN_desc(orig_ls));
    }
    
    /* append the 'res' to the V_Loads array for dependence update later */
    V_Loads()->AddElement(res);
    
    /* link from EINFO to Simd_Expr */
    Is_True(idx==Cur_Simd_Loop->Get_E_Info(orig_ls)->Simd_Exprs()->Elements(),
	    ("Expressions initialized out of order in SIMD_GROUP"));
    Cur_Simd_Loop->Get_E_Info(orig_ls)->Add_Simd_Expr(res);
}

void
IMEM_GROUP::Get_Vector_Load_Types(TYPE_ID &res_type, TYPE_ID &desc_type)
{
  res_type  = Simd_Info->Get_SIMD_Reg_Type_Scalar(Scalar_Type());
  desc_type =
    Is_Vector() ? Simd_Info->Get_SIMD_Mem_Type_Scalar(Scalar_Type()) : Scalar_Type();
}

bool
IMEM_GROUP::Index_Const_Fit_Immediate() 
{
    if (!Index_Value() || WN_operator(Index_Value()) != OPR_INTCONST) {
	return false;
    }

    TYPE_ID res_type;
    TYPE_ID desc_type;

    Get_Vector_Load_Types(res_type, desc_type);
    if (Is_Use() && 
	!Update_Const_Fit_Immediate(OPR_ILOAD, Index_Value(),
				    res_type, desc_type, Is_Vector())) {
	return false;
    }
    if (Is_Def() && 
	!Update_Const_Fit_Immediate(OPR_ISTORE, Index_Value(),
				    res_type, desc_type, Is_Vector())) {
	return false;
    }
    return true;
}

void IMEM_GROUP::Load_Res_Reg(WN *load, WN *stmt, INT idx, WN *t_loop) {
    WN *res = NULL;

    TYPE_ID scalar_type = Scalar_Type();
    
    Is_True(Res_Reg(idx)!=NULL,("Result register not allocated."));
    
    if (Load_Align_Reg() != NULL) {
	if (Updating_Load()) {
	    Is_True(Load_Addr_Reg() != NULL, 
		    ("No address regster for updating load"));
	    INT inc = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(scalar_type);
	    res = 
	      Simd_Info->Generate_LoadA_Update(Scalar_Type(), stmt,
					       Load_Addr_Reg(), 
					       Res_Reg(idx),
					       Load_Align_Reg(), inc);
	} else {
	    // non-updating with alignment
	    WN *addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(Base_Addr(), addr, Du_Mgr);
	    if (idx > 0) {
	      /* generate an increment */
	      OPCODE opc = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
	      INT inc    = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(scalar_type)*idx;
	      WN* inc_wn = WN_CreateIntconst(OPC_I4INTCONST, inc);
	      addr       = LWN_CreateExp2(opc, addr, inc_wn);
	    }
	    
	    res = Simd_Info->Generate_LoadA(Scalar_Type(), stmt, addr,
					    Res_Reg(idx), Load_Align_Reg());
	}
    } else if (Index_Addr_Reg() && (idx == 0) && 
	       (!Addr_Shared() || !Addr_Updated())) {
	if (Addr_Shared()) {
	    Set_Addr_Updated();
	}
	// aligned indexed load, only the first load is indexed
	WN *index_val = Index_Value();
	TYPE_ID res_type, desc_type;
	Get_Vector_Load_Types(res_type, desc_type);
	if (WN_operator(index_val) == OPR_INTCONST &&
	    Update_Const_Fit_Immediate(OPR_ILOAD, index_val, res_type, 
				       desc_type, Is_Vector()))
	{
	    res = Simd_Info->Generate_Update_Load(OPR_ILOAD,
						  res_type,
						  desc_type,
						  stmt,
						  Load_Addr_Reg(),
						  Res_Reg(idx),
						  WN_const_val(index_val),
						  Is_Vector() ? 0 : 1);
	} else if (Is_Vector()) {
	    res = Simd_Info->Generate_LoadV_X(OPR_ILOAD,
					      res_type,
					      desc_type,
					      stmt,
					      Load_Addr_Reg(),
					      Res_Reg(idx),
					      Index_Addr_Reg(),
					      true);
	} else {
	    res = Simd_Info->Generate_LoadS_X(OPR_ILOAD,
					      res_type,
					      desc_type,
					      stmt,
					      Load_Addr_Reg(),
					      Res_Reg(idx),
					      Index_Addr_Reg(),
					      true);
	}
    } else { // aligned load
	if (Updating_Load()) {
	    // we don't generate normal updating load. It is done in CG
	    FmtAssert(0, ("Unexpected updating load"));
	}
	
	/* generate normal iload */
	WN *addr = NULL;
	/* again we don't expect to have an address register, unless
	   we are required to start to generate normal updating loads.  */
	if (Index_Addr_Reg()) {
	    Is_True(idx>0 || Addr_Updated(), ("Unexpected indexed load"));
	    addr = Load_Addr_Reg()->Simd_Preg_Ldid();
	} else {
	    addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
	    LWN_Copy_Def_Use(Base_Addr(), addr, Du_Mgr);
	}
	if (idx > 0) {
	    /* generate an increment */
	    OPCODE opc = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
	    INT inc    = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(scalar_type)*idx;
	    WN* inc_wn = WN_CreateIntconst(OPC_I4INTCONST, inc);
	    addr       = LWN_CreateExp2(opc, addr, inc_wn);
	}
	
	if (idx < Reuse_Length()) {
	    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), addr,
				       WN_kid0(WN_start(t_loop)),
				       Du_Mgr, Array_Dependence_Graph);
	}

	TYPE_ID desc_type = 
	    (Is_Vector()) ? 
	    Simd_Info->Get_SIMD_Mem_Type_Scalar(scalar_type) : 
	    ((Simd_Info->Is_Vectra1()) ?
	     Mtype_TransferSign(MTYPE_I4, WN_desc(load)) :
	     WN_desc(load));
	
	TYPE_ID res_type = 
	    (Is_Vector()) ? desc_type : Res_Reg(idx)->Type();
	
	res = Simd_Info->Generate_Load(OPR_ILOAD,
				       res_type, stmt,
				       addr,
				       Is_Vector() ? 0 : WN_offset(load),
				       Res_Reg(idx),
				       desc_type);

    }

    /* append the 'res' to the V_Loads array for dependence update later */
    if (idx >= Reuse_Length() && !First_Imem_Info()->Has_Reuse()) {
	V_Loads()->AddElement(res);
    } else {
	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(res);
	}
    }

    /* link from EINFO to Simd_Expr */
    Is_True(idx==Cur_Simd_Loop->Get_E_Info(load)->Simd_Exprs()->Elements(),
	    ("Expressions initialized out of order in SIMD_GROUP"));
    Cur_Simd_Loop->Get_E_Info(load)->Add_Simd_Expr(res);
}

/*---------------------------------------------------------------------------*
 * Generate sel_reg = _vectra_str_align[store_index_reg + mul * length]      *
 * Where    mul     = 0, 1, 2 for pre-shift, merge and post shift            *
 *          length  = simd->Get_Narrow_Reg_Length()                          *
 *---------------------------------------------------------------------------*/
WN* 
IMEM_GROUP::Generate_Store_Select(WN *stmt, INT mul, SIMD_PREG *sto_index_reg, 
				  SIMD_PREG *sel) {
    Is_True(sto_index_reg, ("NULL store index register"));
    
    SIMD_INFO *simd = Simd_Info;
    
    WN *offset_amount = sto_index_reg->Simd_Preg_Ldid();
    INT     length    = simd->Get_Narrow_Reg_Length();
    if (mul != 0) {
	OPCODE opc    = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
	WN *wn_inc    = WN_CreateIntconst(OPC_I4INTCONST, mul * length);
	offset_amount = LWN_CreateExp2(opc, offset_amount, wn_inc);
    }
	
    ST*     st_array  = simd->Store_Align();
    INT     element_count = 3*length;
    TYPE_ID mtype     = MTYPE_U4;
    TY_IDX  ty        = Be_Type_Tbl(mtype);
    TY_IDX  ty_ptr    = Make_Pointer_Type(ty);
    TY_IDX  arr_ty_ptr= Make_Pointer_Type(ST_type(st_array));
    OPCODE  op_lda    = OPCODE_make_op(OPR_LDA, Pointer_type, MTYPE_V);
    WN*     wn_lda    = WN_CreateLda(op_lda, 0, arr_ty_ptr, st_array);
    WN*     wn_size   = LWN_Make_Icon(mtype, element_count);
    OPCODE  op_array  = OPCODE_make_op(OPR_ARRAY, Pointer_type, MTYPE_V);
    WN*     wn_array  = WN_Create(op_array, 3);
    
    WN_element_size(wn_array)   = MTYPE_byte_size(mtype);
    WN_array_base(wn_array)     = wn_lda;
    WN_array_index(wn_array, 0) = offset_amount;
    WN_array_dim(wn_array, 0)   = wn_size;
    LWN_Parentize(wn_array); 

    OPCODE op_iload = OPCODE_make_op(OPR_ILOAD, mtype, mtype);
    WN* wn_iload = LWN_CreateIload(op_iload, 0, ty, ty_ptr, wn_array);
    Create_lda_array_alias(Alias_Mgr, wn_lda, wn_iload);
    
    if (Enclosing_Do_Loop(LWN_Get_Parent(stmt)) != NULL) {
	Array_Dependence_Graph->Add_Vertex(wn_iload);
    }
    
    OPCODE cvt_opc   = OPCODE_make_op(OPR_CVT, sel->Type(), mtype);
    WN        *cvt   = LWN_CreateExp1(cvt_opc, wn_iload);
    WN        *stid  = sel->Simd_Preg_Stid(cvt);
    WN_linenum(stid) = LWN_Get_Linenum(stmt);
    return stid;
}

void
IMEM_GROUP::Transform_Variable_Stride_Load(WN *load, WN *stmt)
{
    FmtAssert(Elem_Count() == 1, 
	    ("IMEM_GROUP::Transform_Variable_Stride_Load: Element count > 1"));
    
    IMEM_ELEM *cur_elem = Elem(0);

    /* set address taken */
    WN *array_wn = Base_Addr();
    WN *base = WN_kid0(array_wn);
    if (WN_operator(base) == OPR_LDA) {
	ST *array_st = WN_st(base);
#ifdef _NEW_SYMTAB
	Clear_ST_addr_not_passed(array_st);
	Clear_ST_addr_not_saved(array_st);
#else
	Set_ST_addr_taken_passed(array_st);
#endif
    }

    for (INT i = 0; i < cur_elem->Offset_Count(); i++) {
	IMEM_OFFSET *cur     = cur_elem->Offset(i);
	TYPE_ID      mtype   = cur->Res_Reg(0)->Type();
	INT          length  = Simd_Info->Get_SIMD_Width_SIMD(mtype);
	SIMD_PREG   *var_reg = cur->Var_Reg();
	WN          *orig_ls = cur->Orig_Wn();
	WN *base_addr = 
	    (OPERATOR_is_store(WN_operator(orig_ls))) ? 
	    WN_kid1(orig_ls) : WN_kid0(orig_ls);
	WN *addr = LWN_Copy_Tree(base_addr, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(base_addr, addr, Du_Mgr);

	if (WN_offset(orig_ls) != 0) {
	    OPCODE opc  = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
	    WN  *wn_inc = WN_CreateIntconst(OPC_I4INTCONST,WN_offset(orig_ls));
	    addr        = LWN_CreateExp2(opc, addr, wn_inc);
	}
	SIMD_PREG *addr_preg = Gen_Symbol(MTYPE_U4, Cur_Simd_Loop->Pool(), 
					  "xld");

	/* initial address */
	WN *stid = addr_preg->Simd_Preg_Stid(addr);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);

	/* increment amount */
	WN *cst_0 = WN_CreateIntconst(OPC_I4INTCONST, 0);
	
	/* step */
	WN *loop = Cur_Simd_Loop->Simd_Loop();
	INT step = Step_Size(loop);
	FmtAssert(step != 0, 
		  ("IMEM_GROUP::Transform_Variable_Stride_Load non-const step"));
	WN *cst_1  = WN_CreateIntconst(OPC_I4INTCONST, 1);

	WN *copy_0 = LWN_Copy_Tree(base_addr, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(base_addr, copy_0, Du_Mgr);

	WN *copy_1 = LWN_Copy_Tree(base_addr, TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(base_addr, copy_1, Du_Mgr);
	
	Replace_Ldid_With_Exp_Copy(WN_index(loop), copy_0, cst_0, 
				   Du_Mgr, Array_Dependence_Graph);
	WN_Delete(cst_0);

	Replace_Ldid_With_Exp_Copy(WN_index(loop), copy_1, cst_1, 
				   Du_Mgr, Array_Dependence_Graph);
	WN_Delete(cst_1);

	/* copy_1 - copy_0 */
	OPCODE opc  = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	copy_1 = LWN_CreateExp2(opc, copy_1, copy_0);

	SIMD_PREG *idx_preg = Gen_Symbol(MTYPE_I4, Cur_Simd_Loop->Pool(), 
					 "idx");

	/* initial idx */
	stid = idx_preg->Simd_Preg_Stid(copy_1);
	copy_1 = LWN_Simplify_Tree(copy_1);
	LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);	
	
	for (INT j = 0; j < cur->Res_Reg_Count(); j++) {
	    for (INT k = 0; k < length; k++) {
		Load_Variable_Stride_Reg(load, stmt, j*length+k, var_reg, 
					 orig_ls, addr_preg, idx_preg,
					 (k != length-1 || j != cur->Res_Reg_Count()-1));
		if (k == 0) {
		    /* generate a copy Res_Reg(j) = var_reg */
		    WN *stid = cur->Res_Reg(j)->Simd_Preg_Stid(
			var_reg->Simd_Preg_Ldid());
		    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
		    WN_linenum(stid) = WN_Whirl_Linenum(stmt);
		} else {
		    /* generate a shift:
		       Res_Reg(j) =  (var_reg || Res_Reg(j)) >> 1 */
		    Simd_Info->Shift_Into_Reg(var_reg, cur->Res_Reg(j),
					      LWN_Get_Parent(stmt), stmt,
					      WN_Whirl_Linenum(stmt));
		}
	    }
	}
    }
}

void
IMEM_GROUP::Transform_Reuse_Load(WN *load, WN *stmt)
{
    SIMD_EINFO  *e_info = Cur_Simd_Loop->Get_E_Info(load);
    Is_True(e_info, ("Cannot find EINFO"));
    IMEM_INFO   *imem   = e_info->IMem();
    Is_True(imem, ("Cannot find IMEM"));

    Is_True(!Is_Def(), ("IMEM_GROUP::Transform_Reuse_Load: unexpected store"));
    
    bool      is_aligned      = 
	Is_Aligned() ||	First_Imem_Info()->Is_2D_Aligned();
    bool      is_use          = Is_Use();
    bool      can_prime_out   = true;   /* can prime outside of the loop */

    INT simd_mem_size = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(Scalar_Type());
    
    /* insertion point for priming */
    WN       *t_loop    = Enclosing_Do_Loop(stmt);
    WN       *prime_pt  = t_loop;
    WN       *prime_blk = LWN_Get_Parent(t_loop);
    
    // this is NULL if the invariant point is the current statement
    Set_Invar_Point(t_loop);

    // set address taken
    WN *array_wn = Base_Addr();
    WN *base = WN_kid0(array_wn);
    if (WN_operator(base) == OPR_LDA) {
	ST *array_st = WN_st(base);
#ifdef _NEW_SYMTAB
	Clear_ST_addr_not_passed(array_st);
	Clear_ST_addr_not_saved(array_st);
#else
	Set_ST_addr_taken_passed(array_st);
#endif
    }

    SIMD_PREG *load_addr_reg = Load_Addr_Reg();
    
    /* setup the address outside of the loop */
    WN *a_addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(Base_Addr(), a_addr, Du_Mgr);
	
    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), a_addr,
			       WN_kid0(WN_start(t_loop)),
			       Du_Mgr, Array_Dependence_Graph);

    WN *stid = load_addr_reg->Simd_Preg_Stid(a_addr);
    LWN_Insert_Block_Before(prime_blk, t_loop, stid);
    
    /* create a new symbol for LS address */
    SIMD_PREG *ls_addr_preg = Gen_Symbol(load_addr_reg->Type(), Mem_Pool(), "addr");
    INT inc = Simd_Ti->Get_SIMD_Mem_Bytes_Scalar(e_info->Res_Type())*
	Res_Reg_Count() - MTYPE_byte_size(e_info->Res_Type());
    WN *cst = WN_CreateIntconst(OPC_I4INTCONST, inc);
    
    OPCODE opc = OPCODE_make_op(OPR_ADD, ls_addr_preg->Type(), MTYPE_V);
    WN *ls_addr = LWN_CreateExp2(opc, load_addr_reg->Simd_Preg_Ldid(), cst);
    
    stid = ls_addr_preg->Simd_Preg_Stid(ls_addr);
    LWN_Insert_Block_Before(prime_blk, t_loop, stid);

    if (!is_aligned) {
	// generate alignment loads
	SIMD_PREG *align    = Load_Align_Reg();
	WN        *callNode = 
	    Simd_Info->Generate_LoadA_Prime(Scalar_Type(), prime_pt, 
					    load_addr_reg->Simd_Preg_Ldid(),
					    load_addr_reg, Res_Reg(0), align);
	
	if ( Cur_PU_Feedback )
	  Cur_PU_Feedback->Annot_call( callNode, FB_FREQ_UNKNOWN );

	/* memorize the preload for later use */
	e_info->Set_Pre_Load(callNode);
	
	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(prime_blk) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(callNode);
	}

    } else {
	// generate aligned loads
	Set_Load_Align_Reg(NULL);
	Set_Load_Addr_Reg(NULL);
        Reset_Updating_Load();
    }
    
    FmtAssert(Elem_Count() == 1, ("Unexpected reuse"));
    for (INT idx = 0; idx < Res_Reg_Count(); idx++) {
	Load_Res_Reg(load, t_loop, idx, t_loop);
    }

    /* generate LS.IU and shift at the end of the loop */
    IMEM_ELEM  *ie        = Elem(0);
    TYPE_ID     desc      = e_info->Res_Type();
    
    for (INT i = 0; i < ie->Offset(0)->Res_Reg_Count(); i++) {
	for (INT j = 0; j < ie->Offset_Count(); j++) {
	    SIMD_PREG *cur_res = ie->Offset(j)->Res_Reg(i);
	    SIMD_PREG *ls_reg = Gen_Symbol(cur_res->Type(), Mem_Pool(), "ls");
	    /* generate a LS.IU into ls_preg */
	    WN *load_wn = 
		Simd_Ti->Generate_Update_Load(OPR_ILOAD, 
					      ls_reg->Type(),
					      desc,
					      stmt, 
					      ls_addr_preg,
					      ls_reg,
					      MTYPE_byte_size(desc),
					      1 /* scalar */);

	    V_Loads()->AddElement(load_wn);

	    /* generate a shift left (cur_res||ls_reg)) << 1 to 
	       cur_res and insert it at the end of loop body */
	    Simd_Ti->Shift_Into_Reg(ls_reg, cur_res,
				    LWN_Get_Parent(stmt), NULL,
				    WN_Whirl_Linenum(stmt));
	}
    }
}

void
IMEM_GROUP::Transform_Indexed_Load(WN *load, WN *stmt, WN *t_loop)
{
    Is_True(Is_Aligned(), ("IMEM_GROUP::Transform_Indexed_Load: unaligned"));
    
    SIMD_EINFO  *e_info = Cur_Simd_Loop->Get_E_Info(load);
    Is_True(e_info, ("Cannot find EINFO"));
    IMEM_INFO   *imem   = e_info->IMem();
    Is_True(imem, ("Cannot find IMEM"));
    
    WN *array_wn = Base_Addr();
    WN *base = WN_kid0(array_wn);
    if (WN_operator(base) == OPR_LDA) {
	ST *array_st = WN_st(base);
#ifdef _NEW_SYMTAB
	Clear_ST_addr_not_passed(array_st);
	Clear_ST_addr_not_saved(array_st);
#else
	Set_ST_addr_taken_passed(array_st);
#endif
    }
    
    WN *index_val = Index_Value();
    
    /* generate the index value outside of the t_loop */
    WN *stid = Index_Addr_Reg()->Simd_Preg_Stid(index_val);
    LWN_Insert_Block_Before(LWN_Get_Parent(t_loop), t_loop, stid);
    
    /* generate the address outside of the t_loop */
    WN *old_addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
    LWN_Copy_Def_Use(Base_Addr(), old_addr, Du_Mgr);
    
    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), old_addr,
			       WN_kid0(WN_start(t_loop)),
			       Du_Mgr, Array_Dependence_Graph);
    
    OPCODE  opc      = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
    WN     *new_addr = LWN_CreateExp2(opc, old_addr, 
				      Index_Addr_Reg()->Simd_Preg_Ldid());
    if (Is_Use()) {
	stid = Load_Addr_Reg()->Simd_Preg_Stid(new_addr);
	LWN_Insert_Block_Before(LWN_Get_Parent(t_loop), t_loop, stid);
    }

    if (Is_Def() && (!Addr_Shared() || !Is_Use())) {
	WN *store_addr_val = new_addr;
	if (Is_Use()) {
	    store_addr_val = Load_Addr_Reg()->Simd_Preg_Ldid();
	}
	stid = Store_Addr_Reg()->Simd_Preg_Stid(store_addr_val);
	LWN_Insert_Block_Before(LWN_Get_Parent(t_loop), t_loop, stid);
    }
    
    if (WN_operator(load) == OPR_ISTORE) {
	/* store initialization stops here */
	return;
    }

    for (INT idx = 0; idx < Res_Reg_Count(); idx++) {
	Load_Res_Reg(load, (idx >= Reuse_Length()) ? stmt : t_loop, idx, t_loop);
    }

    if (Elem_Count()>1) {
	// load reuse, extract into each IMEM_ELEM
	for (INT i = 0; i < Elem_Count(); i++) {
	    Elem(i)->Read_Reuse_Into_Regs(stmt);
	}
	
	for (INT i = 0; i < Reuse_Length(); i++) {
	    // copy the reuse register to the front
	    WN *stid = Res_Reg(i)->Simd_Preg_Stid(
		Res_Reg(Res_Reg_Count() - Reuse_Length() + i)->Simd_Preg_Ldid());
	    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
	    WN_linenum(stid) = WN_Whirl_Linenum(stmt);
	}
    }
}

void
IMEM_GROUP::Transform_Load (WN *load, WN *stmt) {
    if (Loaded())
	return;
    Set_Loaded();
    
    SIMD_EINFO  *e_info = Cur_Simd_Loop->Get_E_Info(load);
    Is_True(e_info, ("Cannot find EINFO"));
    IMEM_INFO   *imem   = e_info->IMem();
    Is_True(imem, ("Cannot find IMEM"));
    
    /* generate a load into SIMD register if 'ls' is a LOD */
    bool      is_vector       = Is_Vector();
    bool      is_aligned      = Is_Aligned();
    bool      is_def          = Is_Def();
    bool      is_use          = Is_Use();
    WN       *t_loop          = NULL;
    WN       *invar_loop      = NULL;
    bool      can_prime_out   = false; /* can prime outside of the loop */

    INT simd_mem_size = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(Scalar_Type());
    
    MEM_POOL_Push(Mem_Pool());
    {
	DOLOOP_STACK stack(Mem_Pool());
	Is_True(Non_Const_Level()>=0, ("Unexpected Non_Const_Level"));
	Build_Doloop_Stack(load, &stack);
	t_loop    = stack.Bottom_nth(Non_Const_Level());
	
	/* can prime only if the Non_Const_Level is the SIMD loop */
	can_prime_out = (t_loop == Cur_Simd_Loop->Simd_Loop());

	INT out_most_invariant = Non_Const_Level() + 1;
	if (out_most_invariant < stack.Elements()) {
	    invar_loop = stack.Bottom_nth(out_most_invariant);
	    e_info->Set_Depth_Diff(stack.Elements() - out_most_invariant);
	}
    }
    MEM_POOL_Pop(Mem_Pool());
    
    if (Is_Reduction()) {
      if (!e_info->Interleaved() || e_info->Need_Deinterleave())
	SIMD_PREGS_Copy(Res_Regs(), e_info->Simd_Regs());
      if (e_info->Interleaved() || e_info->Need_Interleave())
	SIMD_PREGS_Copy(Res_Regs(), e_info->Simd_IRegs());
      e_info->Init_Sum_Reduction(Cur_Simd_Loop->Simd_Loop());
      return;
    }

    /* insertion point for moving out to invariant level */
    WN       *invar_pt = (invar_loop) ? invar_loop : stmt;
    
    /* insertion point for priming */
    WN       *prime_pt  = (can_prime_out) ? t_loop : invar_pt;
    WN       *prime_blk = LWN_Get_Parent(prime_pt);
    
    // this is NULL if the invariant point is the current statement
    Set_Invar_Point(invar_loop);

    if (Index_Addr_Reg()) {
	Transform_Indexed_Load(load, invar_pt, t_loop);
	return;
    }

    // set address taken if we generate SV, LVA etc
    if (Store_Addr_Reg() || Load_Align_Reg()) {
	WN *array_wn = Base_Addr();
	WN *base = WN_kid0(array_wn);
	if (WN_operator(base) == OPR_LDA) {
	    ST *array_st = WN_st(base);
#ifdef _NEW_SYMTAB
	    Clear_ST_addr_not_passed(array_st);
	    Clear_ST_addr_not_saved(array_st);
#else
	    Set_ST_addr_taken_passed(array_st);
#endif
//	    array_st->Print(stdout,1);
	}
    }

    SIMD_PREG *store_addr_reg = Store_Addr_Reg();

    if (is_vector && store_addr_reg) {
	/* generate the address outside of the loop */
	WN *old_addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(Base_Addr(), old_addr, Du_Mgr);
	INT bias_offset =
	  MTYPE_alignment(Simd_Info->Get_SIMD_Mem_Type_Scalar(Scalar_Type()));
	
	/* prime outside of the loop */
	if (can_prime_out) {
	    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), old_addr,
				       WN_kid0(WN_start(t_loop)),
				       Du_Mgr, Array_Dependence_Graph);
	}
	    
	WN     *new_addr = old_addr;
	OPCODE  opc    = OPCODE_make_op(OPR_SUB, MTYPE_I4, MTYPE_V);
	WN     *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, bias_offset);
	new_addr       = LWN_CreateExp2(opc, old_addr, inc_wn);
	    
	WN *stid = store_addr_reg->Simd_Preg_Stid(new_addr);
	LWN_Insert_Block_Before(prime_blk, prime_pt, stid);
	    
	if (!is_aligned) {
	    /* set no prime if the store cannot be move after the loop */
	    if (!can_prime_out || Cur_Simd_Loop->Trapezoidal()) {
		Set_Prime_In();
		prime_pt  = invar_pt;
		prime_blk = LWN_Get_Parent(prime_pt);
	    }
	    
	    Simd_Info->Generate_StoreA_Prime(this, prime_pt);
	}
    }
    
    if (WN_operator(load) == OPR_ISTORE) {
	/* store initialization stops here */
	return;
    }

    SIMD_PREG *align = Load_Align_Reg();
    
    if (align) {
	INT        inc            = simd_mem_size;
	SIMD_PREG *load_addr_reg = Load_Addr_Reg();
	
	/* insertion point for priming */
	WN       *prime_pt  = (can_prime_out) ? t_loop : invar_pt;
	WN       *prime_blk = LWN_Get_Parent(prime_pt);
	
	/* create a pre-load outside of the loop */
	WN *a_addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
	LWN_Copy_Def_Use(Base_Addr(), a_addr, Du_Mgr);

	if (can_prime_out) {
	    Replace_Ldid_With_Exp_Copy(WN_index(t_loop), a_addr,
				       WN_kid0(WN_start(t_loop)),
				       Du_Mgr, Array_Dependence_Graph);
	}
	
        LWN_Clear_Loop_Stmt(a_addr, Du_Mgr);
        
	INT offset = WN_offset(load);
	if (offset != 0 && !Is_Vector()) {
	    WN    *inc_wn = WN_CreateIntconst(OPC_I4INTCONST, offset);
	    OPCODE opc    = OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
	    a_addr        = LWN_CreateExp2(opc, a_addr, inc_wn);
	}
	
	WN *callNode = Simd_Info->Generate_LoadA_Prime(Scalar_Type(),
						       prime_pt,
						       a_addr, load_addr_reg,
						       Res_Reg(0), align);
	if ( Cur_PU_Feedback )
	  Cur_PU_Feedback->Annot_call( callNode, FB_FREQ_UNKNOWN );
	/* memorize the preload for later use */
	e_info->Set_Pre_Load(callNode);
	
	/* add a dependence graph vertex */
	if (Enclosing_Do_Loop(prime_blk) != NULL) {
	    Array_Dependence_Graph->Add_Vertex(callNode);
	}
    }

    for (INT idx = 0; idx < Res_Reg_Count(); idx++) {
	Load_Res_Reg(load, (idx >= Reuse_Length()) ? invar_pt : prime_pt, idx, t_loop);
    }

    if (Elem_Count()>1) {
	// load reuse, extract into each IMEM_ELEM
	for (INT i = 0; i < Elem_Count(); i++) {
	    Elem(i)->Read_Reuse_Into_Regs(stmt);
	}
	
	for (INT i = 0; i < Reuse_Length(); i++) {
	    // copy the reuse register to the front
	    WN *stid = Res_Reg(i)->Simd_Preg_Stid(
		Res_Reg(Res_Reg_Count() - Reuse_Length() + i)->Simd_Preg_Ldid());
	    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
	    WN_linenum(stid) = WN_Whirl_Linenum(stmt);
	}
    }
}

// recompute the base address after group split
void
IMEM_GROUP::Recompute_Base_Addr(void)
{
    WN *wn = First_Imem_Info()->Lod_Sto().Bottom_nth(0);
    Set_Base_Addr((WN_operator(wn) == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn));
}

// split a SIMD reuse group if the loop is not the inner most variant loop
// or there is no VSEL type for the vector
void
IMEM_GROUP::Split_Group(IMEM_Map *im)
{
    Is_True(Elem_Count()>1, ("IMEM_GROUP::Split_Group: group too small"));
    WN *wn = Loads()->Elements() ? Loads()->Get(0) : Stores()->Get(0);

    Is_True(wn, ("IMEM_GROUP::Split_Group: cannot find load/store WN"));
    WN *inner_most_variant_loop = NULL;
    MEM_POOL_Push(Mem_Pool());
    {
	DOLOOP_STACK stack(Mem_Pool());
	Is_True(Non_Const_Level()>=0, ("Unexpected Non_Const_Level"));
	Build_Doloop_Stack(wn, &stack);
	inner_most_variant_loop = stack.Bottom_nth(Non_Const_Level());
    }
    MEM_POOL_Pop(Mem_Pool());

    TYPE_ID      reg_type   = Simd_Ti->Get_SIMD_Reg_Type_Scalar(Scalar_Type());
    SIMD_SELECT *simd_sel   = Simd_Ti->Get_SIMD_Select_SIMD(reg_type);

    if ((inner_most_variant_loop != Cur_Simd_Loop->Simd_Loop()) || 
	simd_sel == NULL) {
	for (INT i = Elem_Count() - 1; i >= 1; i--) {
	    // split the group
	    IMEM_GROUP *ig = CXX_NEW(IMEM_GROUP(this, Mem_Pool()), Mem_Pool());

	    // other
	    ig->Add_Elem(Elem(i));
	    ig->Collect_Load_Store();
	    ig->Recompute_Base_Addr();
	    im->Enter_Group(ig);
	    
	    // remove from the current group
	    Elems()->Decidx();
	}
	
	// reset reuse_length
	Set_Reuse_Length(0);
	Loads()->Resetidx();
	Stores()->Resetidx();
	Collect_Load_Store();
    }
}

// split a SIMD reuse group if there is no VSEL for the
// given offset
void
IMEM_GROUP::Split_Group_No_Vsel(IMEM_Map *im)
{
    Is_True(Elem_Count()>1, ("IMEM_GROUP::Split_Group: group too small"));

    IMEM_INFO   *first_imem = First_Imem_Info();
    INT          simd_width = Simd_Info->Get_SIMD_Width_Scalar(Scalar_Type());
    TYPE_ID      reg_type   = Simd_Ti->Get_SIMD_Reg_Type_Scalar(Scalar_Type());
    SIMD_SELECT *simd_sel   = Simd_Ti->Get_SIMD_Select_SIMD(reg_type);
    
    int i;
    for (i = 0; i < Elem_Count(); i++) {
	IMEM_ELEM *ie       = Elem(i);
	IMEM_INFO *cur_imem = ie->Offset(0)->Imem_Info();
	
	INT idiff;
	cur_imem->Compare_Imem_Index(first_imem, &idiff);
	INT offset = idiff * ie->Offset_Count();
	if (offset > 0) {
	    INT shift_amount = offset % simd_width;
	    if (!Simd_Ti->Tie_Sel_Possible(reg_type, 
					   simd_sel->Sel_4321(shift_amount))) {
		// split the group from i
		IMEM_GROUP *ig = CXX_NEW(IMEM_GROUP(this, Mem_Pool()), 
					 Mem_Pool());
		for (INT j = i; j < Elem_Count(); j++) {
		    // other
		    ig->Add_Elem(Elem(j));
		}
		
		ig->Collect_Load_Store();
		ig->Recompute_Base_Addr();
		im->Enter_Group(ig);

		// split the new group
		if (ig->Elem_Count() > 1) {
		    ig->Split_Group_No_Vsel(im);
		}

		break;
	    }
	}
    }

    if (i != Elem_Count()) {
	while (Elem_Count() > i) {
	    Elems()->Decidx();
	}
	Loads()->Resetidx();
	Stores()->Resetidx();
	Collect_Load_Store();
    }
}

// do we need to split a SIMD reuse group if there is no VSEL for the
// given offset
BOOL
IMEM_GROUP::Need_Split_Group_No_Vsel(IMEM_Map *im)
{
    Is_True(Elem_Count()>1, ("IMEM_GROUP::Split_Group: group too small"));

    IMEM_INFO   *first_imem = First_Imem_Info();
    INT          simd_width = Simd_Info->Get_SIMD_Width_Scalar(Scalar_Type());
    TYPE_ID      reg_type   = Simd_Ti->Get_SIMD_Reg_Type_Scalar(Scalar_Type());
    SIMD_SELECT *simd_sel   = Simd_Ti->Get_SIMD_Select_SIMD(reg_type);
    
    int i;
    for (i = 0; i < Elem_Count(); i++) {
	IMEM_ELEM *ie       = Elem(i);
	IMEM_INFO *cur_imem = ie->Offset(0)->Imem_Info();
	
	INT idiff;
	cur_imem->Compare_Imem_Index(first_imem, &idiff);
	INT offset = idiff * ie->Offset_Count();
	if (offset > 0) {
	    INT shift_amount = offset % simd_width;
	    if (!Simd_Ti->Tie_Sel_Possible(reg_type, 
					   simd_sel->Sel_4321(shift_amount))) {
		return TRUE;
	    }
	}
    }
    return FALSE;
}


INT
IMEM_GROUP::Get_Max_Offset_Count (void)
{
  INT max = 0;
  for (INT j = 0; j < Elem_Count(); j++) {
    INT oc = Elem(j)->Offset_Count();
    if (oc > max) {
      max = oc;
    }
  }

  return max;
}


/*---------------------------------------------------------------------------*
 * Generate extract from GROUP to ELEM;                                      *
 *---------------------------------------------------------------------------*/
void
IMEM_ELEM::Read_Reuse_Into_Regs(WN *stmt)
{
    IMEM_ELEM *first_elem = Parent_Group()->Elem(0);
    IMEM_INFO *first_imem = first_elem->Offset(0)->Imem_Info();
    IMEM_INFO *cur_imem   = Offset(0)->Imem_Info();

    INT idiff;
    cur_imem->Compare_Imem_Index(first_imem, &idiff);

    Is_True(idiff>=0, ("IMEM_ELEM::Read_Reuse_Into_Regs: negative distance"));
    
    WN        *block    = LWN_Get_Parent(stmt);
    INT64      line_num = WN_Whirl_Linenum(stmt);

    SIMD_PREGS *group_regs = Parent_Group()->Res_Regs();
    if (idiff == 0) {
	for (INT i = 0; i < Res_Reg_Count(); i++) {
	    /* copy from group to elem */
	    WN *stid = Res_Reg(i)->Simd_Preg_Stid(
		group_regs->Get(i)->Simd_Preg_Ldid());
	    LWN_Insert_Block_Before(block, stmt, stid);
	    WN_linenum(stid) = line_num;
	}
    } else {
	INT  simd_width   = 
	    Simd_Info->Get_SIMD_Width_Scalar(Parent_Group()->Scalar_Type());
	INT  offset       = idiff * Offset_Count();
	INT  start_idx    = offset/simd_width;
	INT  shift_amount = offset % simd_width;
	for (INT i = 0; i < Res_Reg_Count(); i++) {
	    // generate a shift
	    // Res_Reg(i) = (group_regs(i+1) || group_regs(i)) >> shift_amount
	    Simd_Info->Shift_Into_Reg(group_regs->Get(i+1+start_idx), 
				      group_regs->Get(i+start_idx), 
				      block, stmt, line_num, shift_amount,
				      Res_Reg(i));
	}
    }
}

/*---------------------------------------------------------------------------*
 * Generate st_idx = (addr & mask);                                          *
 *---------------------------------------------------------------------------*/
void
IMEM_GROUP::Generate_Store_Offset_Index(SIMD_PREG *store_addr_reg,
					WN *stmt, SIMD_LOOP *do_info)
{
    /* generate st_idx =(addr & mask) */
    SIMD_INFO *simd = Simd_Info;
    INT        mask = (simd->Get_SIMD_Mem_Bytes_Scalar(Scalar_Type()) - 1);
    WN     *wn_mask = WN_CreateIntconst(OPC_I4INTCONST, mask);
    OPCODE     opc  = OPCODE_make_op(OPR_BAND, MTYPE_I4, MTYPE_V);
    WN *offset_amount = 
	LWN_CreateExp2(opc, store_addr_reg->Simd_Preg_Ldid(), wn_mask);
    
    INT shift = 0;
    INT bytes = simd->Type1_Mem_Bytes();
    while (bytes != 1) {
	bytes = bytes >> 1;
	shift++;
    }
    if (shift != 0) {
	opc           = OPCODE_make_op(OPR_ASHR, MTYPE_I4, MTYPE_V);
	WN  *wn_sft   = WN_CreateIntconst(OPC_I4INTCONST, shift);
	offset_amount = LWN_CreateExp2(opc, offset_amount, wn_sft);
    }

    TYPE_ID mtype = MTYPE_I4;
    SIMD_PREG *idx_sym = Gen_Symbol(mtype, do_info->Pool(), "sel_idx");
    Set_Store_Index_Reg(idx_sym);

    WN *stid = idx_sym->Simd_Preg_Stid(offset_amount);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
    WN_linenum(stid) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Store back to memory from vector register                                 *
 *---------------------------------------------------------------------------*/
WN*
IMEM_GROUP::Generate_Unaligned_Store_Merge(WN *lod_val, SIMD_PREG *store_copy)
{
    Is_True(!Elem(0)->Offset(0)->Imem_Info()->Is_Aligned(), 
	    ("Expecting unaligned store"));

    /* generate SEL(lod_val, store_copy->Ldid(), sel_reg) */
    TYPE_ID   mtype        = WN_rtype(lod_val);
    OPERATOR  op           = OPR_SELECT;
    INTRINSIC intrinsic_id = Simd_Info->Find_Function(op, mtype);
    OPCODE    intrin_op    = OPCODE_make_op(OPR_INTRINSIC_OP, mtype, MTYPE_V);
    
    WN* apr[3];
    apr[0] = LWN_CreateParm(mtype, lod_val,
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[1] = LWN_CreateParm(mtype, store_copy->Simd_Preg_Ldid(),
			    MTYPE_To_TY(mtype), WN_PARM_BY_VALUE);
    apr[2] = LWN_CreateParm(Store_Sel_Reg()->Type(), Store_Sel_Reg()->Simd_Preg_Ldid(),
			    MTYPE_To_TY(Store_Sel_Reg()->Type()), WN_PARM_BY_VALUE);

    WN  *perm = LWN_Create_Intrinsic(intrin_op, intrinsic_id, 3, apr);
    return perm;
}

/*---------------------------------------------------------------------------*
 *  update store copy for unaligned store                                    *
 *---------------------------------------------------------------------------*/
void
IMEM_GROUP::Generate_Unaligned_Store_Copy(WN *new_val, WN* stmt)
{
    WN* stid = Store_Copy()->Simd_Preg_Stid(new_val);
    LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, stid);
    WN_linenum(stid) = LWN_Get_Linenum(stmt);
}

/*---------------------------------------------------------------------------*
 * Store back to memory                                                      *
 *---------------------------------------------------------------------------*/
void
IMEM_GROUP::Transform_Store(WN *sto, WN *stmt) 
{
    SIMD_PREG *addr_reg   = Store_Addr_Reg();
    OPERATOR   oper       = WN_operator(sto);
    Is_True(oper == OPR_ISTORE, ("Expecting an OPR_ISTORE"));
    bool       is_aligned = Elem(0)->Offset(0)->Imem_Info()->Is_Aligned();
    int        inc_u = Simd_Info->Get_SIMD_Mem_Bytes_Scalar(Scalar_Type());

    /* go through the elements to merge offset into elements */
    for (INT i = 0; i < Elem_Count(); i++) {
	IMEM_ELEM *elm = Elem(i);
	elm->Write_Out_Regs(stmt);
    }

    /* merge elements into group */
    Is_True(Elem_Count() == 1, 
	    ("IMEM_GROUP with more than 1 element not implemented yet"));

    SIMD_PREG *store_copy = Store_Copy();
    for (INT i = 0; i < Res_Reg_Count(); i++) {
	/* for indirect store, stores to memory from the vector reg */
	WN        *res     = NULL;
	SIMD_PREG *cur     = Res_Reg(i);
	TYPE_ID    vtype   = Simd_Info->Get_SIMD_Mem_Type_Scalar(Scalar_Type());
	
	if (!is_aligned) {
	    Simd_Info->Generate_StoreA_Update(this, stmt, cur, inc_u);
	} else {
	    WN  *lod_val = cur->Simd_Preg_Ldid();
	    
	    if (Index_Addr_Reg() && (i == 0) && 
		(!Addr_Shared() || !Addr_Updated())) {
		if (Addr_Shared()) {
		    Set_Addr_Updated();
		}
		// indexed updating store
		WN *index_val = Index_Value();
		if (WN_operator(index_val) == OPR_INTCONST &&
		    Update_Const_Fit_Immediate(
			OPR_ISTORE, index_val, cur->Type(), vtype, Is_Vector()))
		{
		    // normal update if index fit the immediate
		    res = Simd_Info->Generate_Update_Store(
			cur->Type(), vtype, stmt, lod_val, addr_reg, 
			WN_const_val(index_val));
		    
		} else { // update indexed store
		    res = Simd_Info->Generate_Update_Indexed_Store(
			cur->Type(), vtype, stmt, lod_val, addr_reg, 
			Index_Addr_Reg());
		}
	    } else if ((addr_reg && !Index_Addr_Reg())) {
		Is_True(!Addr_Shared(), ("Shared address register with multiple update"));
		/* Generating updating store */
		res = Simd_Info->Generate_Update_Store(cur->Type(), vtype, 
						       stmt, lod_val, 
						       addr_reg, inc_u);
	    } else {
		WN *sto_addr = NULL;
		if (Index_Addr_Reg()) {
		    Is_True(addr_reg != NULL && (i > 0 || Addr_Updated()), 
			    ("Unexpected indexed store"));
		    sto_addr = addr_reg->Simd_Preg_Ldid();
		} else {
		    /* copy the address */
		    sto_addr = LWN_Copy_Tree(Base_Addr(), TRUE, LNO_Info_Map);
		    
		    /* copy the DU information */
		    LWN_Copy_Def_Use(Base_Addr(), sto_addr, Du_Mgr);
		}
		if (i>0) {
		    WN    *inc_cst= WN_CreateIntconst(OPC_I4INTCONST, i*inc_u);
		    OPCODE add_opc= OPCODE_make_op(OPR_ADD, MTYPE_I4, MTYPE_V);
		    sto_addr      = LWN_CreateExp2(add_opc, sto_addr, inc_cst);
		}
		
		/* generate a CVT if the RHS and vtype doesn't match */
		if (vtype != WN_rtype(lod_val)) {
		    OPCODE cvt = OPCODE_make_op(OPR_CVT, vtype, WN_rtype(lod_val));
		    lod_val = LWN_CreateExp1(cvt, lod_val);
		}
		
		OPCODE istore_opc   = OPCODE_make_op(oper, MTYPE_V, vtype);
		res = LWN_CreateIstore(istore_opc, 0, 
				       Make_Pointer_Type(MTYPE_To_TY(vtype)), 
				       lod_val, sto_addr);
		LWN_Insert_Block_Before(LWN_Get_Parent(stmt), stmt, res);
		WN_linenum(res) = LWN_Get_Linenum(stmt);
	    }
	    
	    /* add 'res' to the V_Stores() for updating dependence later */
	    V_Stores()->AddElement(res);
	}
    }
    
    if (!is_aligned && Prime_In()) {
	/* Finalize the store right here */
	Simd_Info->Generate_StoreA_Flush(this, stmt, /* ins_after */ false);
    }
}

/*---------------------------------------------------------------------------*
 * Add dependence vertices                                                   *
 *---------------------------------------------------------------------------*/
void
IMEM_Map::Add_Dependence_Vertices(void) {
    for (INT i=0; i<_groups.Elements(); i++)
	_groups[i]->Add_Dependence_Vertices();
}

void
IMEM_GROUP::Add_Dependence_Vertices() 
{
    ARRAY_DIRECTED_GRAPH16* adg   = Array_Dependence_Graph; 

    /* add dependence vertex for each new V_Stores */
    for (INT i = 0; i < V_Stores()->Elements(); i++) {
	WN       *cur  = V_Stores()->Get(i);
	VINDEX16  newv = adg->Add_Vertex(cur);
	V_Store_Vertices()->AddElement(newv);
    }

    /* add dependence vertex for each new V_Loads */
    for (INT i = 0; i < V_Loads()->Elements(); i++) {
	WN       *cur  = V_Loads()->Get(i);
	VINDEX16  newv = adg->Add_Vertex(cur);
	V_Load_Vertices()->AddElement(newv);
    }
}

/*---------------------------------------------------------------------------*
 * Add dependence for pending load/stores                                    *
 *---------------------------------------------------------------------------*/
void
IMEM_GROUP::Add_Store_Dependence(MEM_POOL *pool)
{
    INT els = Pending_Wn().Elements();
    Is_True(els == 2 || els == 3, ("Expecting 2 or 3 elements"));
    WN *store = Pending_Wn().Bottom_nth(els-1);
    
    MEM_POOL_Push(pool);
    {
	DOLOOP_STACK udstack(pool);
	Build_Doloop_Stack(store, &udstack);
	for (INT i = 0; i < els - 1; i++) {
	    WN *load = Pending_Wn().Bottom_nth(i);
	    if (!Array_Dependence_Graph->Add_Edge_Equals(load, &udstack, 
							 store, &udstack)) {
		DevWarn("Can't add a dependence edge for pending load/stores");
		LNO_Erase_Dg_From_Here_In(WN_kid0(load), 
					  Array_Dependence_Graph);
		LNO_Erase_Dg_From_Here_In(WN_kid1(store), 
					  Array_Dependence_Graph);
	    }
	}
    }
    MEM_POOL_Pop(pool);
}

/*---------------------------------------------------------------------------*
 * Get the new vertices array corresponding to 'wn'                          *
 *---------------------------------------------------------------------------*/
static
VINDEX16_ARRAY* Get_V_Vertices(WN *wn)
{
    VINDEX16_ARRAY *res = NULL;
    SIMD_EINFO *e_info = Cur_Simd_Loop->Get_E_Info(wn);
    if (e_info && e_info->IMem()) {
	IMEM_GROUP *ig = e_info->IMem()->Imem_Offset()->
	    Parent_Elem()->Parent_Group();
	if (WN_operator(wn) == OPR_ILOAD &&
	    ig->V_Load_Vertices()->Elements() > 0) {
	    res = ig->V_Load_Vertices();
	} else if (WN_operator(wn) == OPR_ISTORE &&
	    ig->V_Store_Vertices()->Elements() > 0) {
	    res = ig->V_Store_Vertices();
	}
    }
    return res;
}

/*---------------------------------------------------------------------------*
 * Update dependence graph                                                   *
 *                                                                           *
 * Each new vector load in V_Loads() relates to all the loads in Loads().    *
 * The dependence on the v_load is the union of all the dependences on loads.*
 *                                                                           *
 * Similarily for the V_Stores() and Stores()                                *
 *---------------------------------------------------------------------------*/
INT
IMEM_Map::Update_Dependence(void) 
{
    /* add new vertices for all v_loads/v_stores */
    Add_Dependence_Vertices();

    /* update dependence */
    for (INT i=0; i<_groups.Elements(); i++) {
	INT good =  _groups[i]->Update_Dependence();
	if (!good) {
	    return 0;
	}
    }
    return 1;
}

INT IMEM_GROUP::Update_Dependence()
{
    INT good;
    if (V_Load_Vertices()->Elements() > 0) {
	good = Update_Dependence(Loads(), V_Load_Vertices());
	if (!good) {
	    return 0;
	}
    }
    if (V_Store_Vertices()->Elements() > 0) {
	good = Update_Dependence(Stores(), V_Store_Vertices());
    }
    return good;
}

INT IMEM_GROUP::Update_Dependence(WN_ARRAY *ls_array, VINDEX16_ARRAY *newvs)
{
    ARRAY_DIRECTED_GRAPH16* adg   = Array_Dependence_Graph; 
    MEM_POOL *pool = adg->Pool();

    for (INT i = 0; i < ls_array->Elements(); ++i) {
	WN      *origv_wn = ls_array->Get(i);
	/* original vertex */
	VINDEX16 origv  = adg->Get_Vertex(origv_wn);
	Is_True(origv_wn && origv, ("Cannot find WN for DGraph vertex"));
	WN      *loop    = Enclosing_Do_Loop(origv_wn);
	INT      dgood   = Good_Do_Depth(loop);

	SIMD_EINFO *e_info = Cur_Simd_Loop->Get_E_Info(origv_wn);
	Is_True(e_info,("Cannot find EINFO for Dgraph vertex"));
	INT      depth_dif = e_info->Depth_Diff();
	INT      dim       = dgood - depth_dif + 1; /* new dim */

	/* get out the edges, copy out of region dependences */
	EINDEX16 edge = adg->Get_Out_Edge(origv);
	while (edge) {
	    VINDEX16 orig_sinkv  = adg->Get_Sink(edge);
	    WN *orig_sinkv_wn    = adg->Get_Wn(orig_sinkv);
	    Is_True(orig_sinkv_wn, ("Cannot find WN for a Dep vertex"));

	    VINDEX16_ARRAY *new_sinkvs = Get_V_Vertices(orig_sinkv_wn);
	    if (new_sinkvs == NULL && dim) {
		/* the sink goes out of the region, copy the edge */
		DEPV_ARRAY *orig_depv = adg->Depv_Array(edge);
		for (INT i=0; i<newvs->Elements(); i++) {
		    VINDEX16 newv = newvs->Get(i);
		    DEPV_ARRAY *depv = orig_depv->Shorten(dim, pool);
		    if (Add_Edge_Combine(newv, orig_sinkv, 
					 depv, Mem_Pool()) == 0)
		    {
			return 0;
		    }
		}
	    } else {
		/* both ends of the edge are in the region */
		/* Add new edge between the two new vertices and adjust the
		   dependence */
		SIMD_EINFO *e1_info = Cur_Simd_Loop->Get_E_Info(orig_sinkv_wn);
		Is_True(e1_info,("Cannot find EINFO for Dgraph vertex"));
		INT      depth_dif1 = e1_info->Depth_Diff();
		WN      *loop_1     = Enclosing_Do_Loop(orig_sinkv_wn);
		if (loop_1) {
		    // if loop_1 == NULL,
		    // the orig_sinkv_wn is removed by if-conversion
		    INT      dgood_1    = Good_Do_Depth(loop_1);
		    INT      dim1       = dgood_1 - depth_dif1 + 1;
		    if (dim < dim1) {
			dim1 = dim;
		    }
		    if (dim1 &&
                       Cur_Simd_Loop->Add_Dependence_Edges(newvs, new_sinkvs, 
							    edge, dim1) == 0) {
			return 0;
		    }
		}
	    }
	    edge = adg->Get_Next_Out_Edge(edge);
	}

	/* get in the edges, copy out of region dependences */
	edge = adg->Get_In_Edge(origv);
	while (edge) {
	    VINDEX16 orig_srcv   = adg->Get_Source(edge);
	    WN      *orig_src_wn = adg->Get_Wn(orig_srcv);
	    VINDEX16_ARRAY *new_srcvs   = Get_V_Vertices(orig_src_wn);

	    if (new_srcvs == NULL) {
		// the source comes from out of the region
		DEPV_ARRAY *orig_depv = adg->Depv_Array(edge);
		for (INT i=0; i<newvs->Elements(); i++) {
		    VINDEX16 newv = newvs->Get(i);
		    DEPV_ARRAY *depv = orig_depv->Shorten(dim, pool);
		    if (Add_Edge_Combine(orig_srcv, newv, depv, 
					 Mem_Pool()) == 0) {
			return 0;
		    }
		}
	    }
	    edge = adg->Get_Next_In_Edge(edge);
	}
    }
    return 1;
}

void
IMEM_GROUP::Setup_Index_Value ()
{
  if (!First_Imem_Info()->Has_Reuse()) {
    bool require_index_reg = false;
    WN *t_loop = NULL;
    MEM_POOL_Push(Mem_Pool());
    {
      WN *load = First_Imem_Info()->Lod_Sto().Top_nth(0);
      DOLOOP_STACK stack(Mem_Pool());
      Build_Doloop_Stack(load, &stack);
      t_loop    = stack.Bottom_nth(Non_Const_Level());
      
      /* We can use indexed load/store if the non-const-level is one level
         below the simd loop and is the innermost loop. */
      require_index_reg = (t_loop != Cur_Simd_Loop->Simd_Loop()) &&
        Non_Const_Level()>0 && 
        (stack.Bottom_nth(Non_Const_Level() - 1) == 
         Cur_Simd_Loop->Simd_Loop()) && 
        (Non_Const_Level() == stack.Elements() - 1);
      
      /* Use updating load/store for multi-opcode memory protos. */
      if (!Simd_Info->AT_Analysis_Phase() &&
          !require_index_reg &&
          Is_Vector() &&
          t_loop == Cur_Simd_Loop->Simd_Loop()) {
        TYPE_ID res_type;
        TYPE_ID desc_type;
        Get_Vector_Load_Types(res_type, desc_type);
        if ((Is_Use() && multi_op_memory_proto(OPR_ILOAD, res_type, desc_type)) ||
            (Is_Def() && multi_op_memory_proto(OPR_ISTORE, res_type, desc_type))) {
          require_index_reg = true;
        }
      }
    }
    MEM_POOL_Pop(Mem_Pool());
    if (require_index_reg) {
      WN *index_value = Compute_Index_Value(t_loop);
      Set_Index_Value(index_value);
    }
  }
}

void
IMEM_GROUP::Delete_Index_Value (void)
{
  if (Index_Value()) {
    LWN_Delete_Tree(Index_Value());
    Set_Index_Value(NULL);
  }
}

void
IMEM_GROUP::Setup_Property()
{
    for (INT i = 0; i < Elem_Count(); i++) {
	IMEM_ELEM *cur = Elem(i);
	cur->Setup_Property();
	if (cur->Variable_Stride()) {
	    Set_Variable_Stride();
	}

	if (cur->Has_Part_Def() || cur->Has_Exp_Part_Use()) {
          Set_Has_Gaps();
        }

        if (cur->Has_Dif_Size() || cur->Bad_Field_Count()) {
          Set_Has_Bad_Elem();
	}
    }
}

void
IMEM_GROUP::Print(FILE *out_file, int indent) {
    INT i;
    fprint_indent(out_file,indent);
    fprintf(out_file,"IMEM_GROUP {\n");

    indent+=4;
    {
	fprint_indent(out_file,indent);
	fprintf(out_file,"Flags: %08x\n",_flags);

	fprint_indent(out_file,indent);
	fprintf(out_file,"Reuse Length:  %d\n", _reuse_length);

	for (INT i=0; i<Elem_Count(); i++) {
	    Elem(i)->Print(out_file,indent);
	}
	
	if (Res_Reg_Count()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Result registers {\n");
	    for (INT i = 0; i < Res_Regs()->Elements(); i++) {
		Res_Reg(i)->Print(out_file, indent+4);
	    }
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "}\n");
	}
	
	if (Load_Align_Reg()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Load alignment register:    ");
	    Load_Align_Reg()->Print(out_file, 0);
	}
	
	if (Load_Addr_Reg()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Load address register:      ");
	    Load_Addr_Reg()->Print(out_file, 0);
	}
	
	if (Store_Addr_Reg()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Store address register:     ");
	    Store_Addr_Reg()->Print(out_file, 0);
	}

	if (Store_Copy()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Store copy register:        ");
	    Store_Copy()->Print(out_file, 0);
	}
	
	if (Store_Sel_Reg()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Store select register:      ");
	    Store_Sel_Reg()->Print(out_file, 0);
	}
	
	if (Store_Index_Reg()) {
	    fprintf(out_file, "Store index register:       ");
	    Store_Index_Reg()->Print(out_file, 0);
	}

	if (Store_Align_Reg()) {
	    fprintf(out_file, "Store alignment register:   ");
	    Store_Align_Reg()->Print(out_file, 0);
	}
    } // imem group
    indent-=4;
    fprint_indent(out_file,indent);
    fprintf(out_file,"}\n");
}

IMEM_ELEM::IMEM_ELEM (MEM_POOL *pool) :
    _mem_pool(pool), _flags(0) {
    _offsets    = CXX_NEW(IMEM_OFFSETS(Mem_Pool()),Mem_Pool());
    _res_regs   = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
}

void
IMEM_ELEM::Set_Offset (INT i, IMEM_OFFSET *io) {
    Offsets()->Set(i,io);
    io->Set_Parent_Elem(this);
}

void
IMEM_ELEM::Add_Offset (IMEM_OFFSET *io) {
    Set_Offset(Offsets()->Newidx(),io);
}

void
IMEM_ELEM::Allocate_Regs (void) {
    for (INT i=0; i<Offset_Count(); i++)
	Offset(i)->Allocate_Regs();
    Allocate_Res_Regs();
}

void
IMEM_ELEM::Allocate_Res_Regs (void) {
    if (Offset_Count()==1) {
	// No structures, simply use the IMEM_OFFSET's registers
	SIMD_PREGS_Copy(Offset(0)->Res_Regs(),Res_Regs());
    } else {
	TYPE_ID scalar_type = Parent_Group()->Scalar_Type();
	INT simd_width = Simd_Info->Get_SIMD_Width_Scalar(scalar_type);
	INT unroll_factor = Cur_Simd_Loop->V_Unroll_Factor(Simd_Info);
	INT elems_to_load = Offset_Count() * unroll_factor;
	Is_True(simd_width!=0 && elems_to_load%simd_width==0,
		("Can't unroll by %d, SIMD width %d, structure elements %d.",
		 unroll_factor,simd_width,Offset_Count()));
	INT regs = elems_to_load/simd_width;
	if (Parent_Group()->Is_Reduction()) {
	    SIMD_PREG *shared_sym = Gen_Symbol(Simd_Info,scalar_type,Mem_Pool());
	    for (INT i=0; i<regs; i++) {
		Add_Res_Reg(shared_sym);
	    }
	} else {
	    for (INT i=0; i<regs; i++) {
		Add_Res_Reg(Gen_Symbol(Simd_Info,scalar_type,Mem_Pool()));
	    }
	}
    }
}

void
IMEM_ELEM::Transform_Load (WN *load, WN *stmt) {
    if (Loaded())
	return;

    if (Variable_Stride()) {
	Parent_Group()->Transform_Variable_Stride_Load(load, stmt);
	Set_Loaded();

	/* merge into register */
	return;
    }

    if (Parent_Group()->First_Imem_Info()->Has_Reuse() &&
	Cur_Simd_Loop->Test_Imem_Load_Reuse_Supported(Parent_Group())) {
	Parent_Group()->Transform_Reuse_Load(load, stmt);
	Set_Loaded();
	if (WN_operator(load) == OPR_ILOAD && Offset_Count()>1) {
	    // Structure -- deinterleave
	    Read_Into_Regs(Enclosing_Do_Loop(stmt));
	    return;
	}
    } else {
	Parent_Group()->Transform_Load(load,stmt);    
    }
    Set_Loaded();
    
    Is_True(Offset_Count()>0,("IMEM_ELEM with no IMEM_OFFSETS."));
    if (WN_operator(load) == OPR_ILOAD && Offset_Count()>1) {
	// Structure -- deinterleave
	Read_Into_Regs(stmt);
    } else {
	// nothing
	// assume that the registers allocated at the IMEM_ELEM level
	// are the same as the registers at the IMEM_OFFSET level
    }
}

void
IMEM_ELEM::Transform_Store (WN *store, WN *stmt) {
    Parent_Group()->Transform_Store(store,stmt);
}

bool
IMEM_ELEM::Has_Table_For_Fields(INT vl)
{
    VSEL_IMEMS &vsel_table = Cur_Simd_Loop->IMem_Map().Vsel_W_Table();
    return (Find_Vsel_Op(vsel_table, Offset_Count(), vl) != NULL);
}

void
IMEM_ELEM::Setup_Property (void)
{
    Is_True(Offset_Count() > 0,
	    ("Expected at least one IMEM_OFFSET in IMEM_ELEM"));

    if (!LNO_IS_POWER_OF_TWO(Offset_Count()) &&
	!Has_Table_For_Fields()) {
	Set_Bad_Field_Count();
	return;
    }
    
    IMEM_OFFSET *cur = Offset(0);
    IMEM_INFO   *imem= cur->Imem_Info();
    Is_True(imem, ("Expecting IMEM_INFO"));
  
    INT offset_size = TY_size(MTYPE_To_TY(imem->Scalar_Type()));
    INT sum_def     = 0;
    INT sum_exp_use = 0;
  
    bool variable_stride = false;
    for (INT i = 0; i < Offset_Count(); i++) {
	imem = Offset(i)->Imem_Info();
	Is_True(imem, ("Expecting IMEM_INFO"));
	if (Offset(i)->Variable_Stride()) {
	    variable_stride = true;
	}
    
	INT cur_size = TY_size(MTYPE_To_TY(imem->Scalar_Type()));
	if (cur_size != offset_size) {
	    Set_Has_Dif_Size();
	}
    
	if (cur->Has_Def()) {
	    sum_def += cur_size;
	}
    
	if (cur->Has_Exp_Use()) {
	    sum_exp_use += cur_size;
	}
    }
  
    if (variable_stride) {
	Set_Variable_Stride();
	Set_Elem_Byte_Size(Elem_Byte_Size()*Offset_Count());
    }

    if (sum_def != 0 && sum_def != Elem_Byte_Size()) {
	Is_True(sum_def < Elem_Byte_Size(), 
		("Unexpected partial def IMEM_ELEM/IMEM_OFFSET size"));
	Set_Has_Part_Def();
    }
  
    if (sum_exp_use != 0 && sum_exp_use != Elem_Byte_Size()) {
	Is_True(sum_exp_use < Elem_Byte_Size(),
		("Unexpected partial use IMEM_ELEM/IMEM_OFFSET size"));
	Set_Has_Exp_Part_Use();
    }
}


void
IMEM_ELEM::Print(FILE *out_file, int indent) {
    fprint_indent(out_file,indent);
    fprintf(out_file,"IMEM_ELEM {\n");
    indent+=4;
    {
	fprint_indent(out_file,indent);
	fprintf(out_file,"Flags: %08x\n",_flags);

	for (INT i=0; i<Offset_Count(); i++) {
	    Offset(i)->Print(out_file,indent);
	}
	
	if (Res_Reg_Count()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Result registers {\n");
	    for (INT i = 0; i < Res_Regs()->Elements(); i++) {
		Res_Reg(i)->Print(out_file, indent+4);
	    }
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "}\n");
	}
    } // imem elem
    indent-=4;
    fprint_indent(out_file,indent);
    fprintf(out_file,"}\n");
}

IMEM_OFFSET::IMEM_OFFSET (IMEM_INFO *imem_info, MEM_POOL *pool) :
    _imem_info(imem_info), _mem_pool(pool), _flags(0), _var_reg(NULL) {
    _res_regs   = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
    if (imem_info) {
	if (imem_info->Is_Def()) {
	    Set_Has_Def();
	}
	if (imem_info->Is_Use() && 
	    WN_operator(imem_info->Lod_Sto().Bottom_nth(0)) == OPR_ILOAD) {
	    Set_Has_Exp_Use();
	}
	if (imem_info->Variable_Stride()) {
	    Set_Variable_Stride();
	}
    }
}

IMEM_OFFSET::~IMEM_OFFSET () {
    CXX_DELETE(_res_regs,Mem_Pool());
}

void
IMEM_OFFSET::Allocate_Regs (void) {
    Allocate_Res_Regs();
}

void
IMEM_OFFSET::Allocate_Res_Regs (void) {
    SIMD_INFO *simd = Simd_Info;
    TYPE_ID scalar_type = Parent_Group()->Scalar_Type();
    INT unroll_factor = Cur_Simd_Loop->V_Unroll_Factor(simd);
    INT width = simd->Get_SIMD_Width_Scalar(scalar_type);
    Is_True(unroll_factor%width==0,
	    ("Unroll factor (%d) not a multiple of SIMD width (%d).",
	     unroll_factor,width));
    INT regs=unroll_factor/width;
    if (Parent_Group()->Is_Reduction()) {
	SIMD_PREG *shared_sym = Gen_Symbol(Simd_Info,scalar_type,Mem_Pool());
	for (INT i=0; i<regs; i++) {
	    Add_Res_Reg(shared_sym);
	}
    } else {
	for (INT i=0; i<regs; i++) {
	    Add_Res_Reg(Gen_Symbol(Simd_Info,scalar_type,Mem_Pool()));
	}
    }
    if (Variable_Stride()) {
	Set_Var_Reg(Gen_Symbol(Simd_Info,scalar_type,Mem_Pool()));
    }
}


void
IMEM_ELEM::Transform_Odd_Even_Shuffle(WN *stmt,
				      SIMD_PREGS *inputs, 
				      SIMD_PREGS *output_0, 
				      SIMD_PREGS *output_1)
{
    INT input_size = inputs->Elements();
    Is_True((input_size & 0x1) == 0, 
	    ("Expecting input size to be multiple of 2"));
    
    TYPE_ID elem_type = Parent_Group()->Scalar_Type();
    TYPE_ID vec_type = Simd_Info->Get_SIMD_Reg_Type_Scalar(elem_type);
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(vec_type);
    
    INT64 sel_0 = simd_sel->Sel_6420();
    INT64 sel_1 = simd_sel->Sel_7531();
    INT64 dsel  = simd_sel->Dsel_75316420();
    
    for (INT i = 0; i < input_size; i += 2) {
	Cur_Simd_Loop->Generate_Sel_Imm(stmt, 
					(*output_1)[i >> 1], (*output_0)[i >> 1],
					(*inputs)[i + 1], (*inputs)[i],
					sel_1, sel_0, dsel);
    }
}

void
IMEM_ELEM::Transform_Interleave_Shuffle(WN *stmt,
					SIMD_PREGS *inputs, 
					SIMD_PREGS *outputs)
{
    INT input_size = inputs->Elements();
    Is_True((input_size & 0x1) == 0, 
	    ("Expecting input size to be multiple of 2"));

    TYPE_ID elem_type = Parent_Group()->Scalar_Type();
    TYPE_ID vec_type = Simd_Info->Get_SIMD_Reg_Type_Scalar(elem_type);
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(vec_type);
    
    INT64 sel_0 = simd_sel->Sel_5140();
    INT64 sel_1 = simd_sel->Sel_7362();
    INT64 dsel  = simd_sel->Dsel_73625140();
	    
    INT half = input_size >> 1;
    for (INT i = 0; i < half; i++) {
	Cur_Simd_Loop->Generate_Sel_Imm(stmt, 
					(*outputs)[2 * i + 1],
					(*outputs)[2 * i],
					(*inputs)[i + half], 
					(*inputs)[i],
					sel_1, sel_0, dsel);
    }
}

/*---------------------------------------------------------------------------*
 * Deinterleave inputs into outputs                                          *
 * vec_width is the number of elements in a vector                           *
 * good_width is the number of elements in correct order                     *
 *                                                                           *
 * Note that this algorithm is not optimal when the vec_width > num_fields.  *
 *---------------------------------------------------------------------------*/
void 
IMEM_ELEM::Read_Into_Regs(WN *stmt, SIMD_PREGS *inputs, SIMD_PREGS *outputs)
{
    INT num_fields = outputs->Elements();
    FmtAssert(num_fields > 1, ("Number of fields is less than 2"));

    INT     input_size = inputs->Elements();
    Is_True(input_size > 0 && (input_size & 0x1) == 0, 
	    ("unexpected input size"));
    TYPE_ID vec_type   = (*inputs)[0]->Type();
    int vec_width = Simd_Info->Get_SIMD_Width_SIMD(vec_type);

    FmtAssert(LNO_IS_POWER_OF_TWO(num_fields), ("Number of fields is not power of 2"));

    Is_True(input_size == num_fields, ("Input/Output size mismatch"));

    SIMD_PREGS *temps = inputs;
    // intermediate steps
    for (INT good_width=2; good_width<vec_width; good_width=good_width<<1){
	inputs = temps;
	temps = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	for (INT i = 0; i < input_size; i++) {
	    temps->AddElement(Gen_Symbol(vec_type, Mem_Pool()));
	}
	Transform_Interleave_Shuffle(stmt, inputs, temps);
    }

    // final step
    Transform_Interleave_Shuffle(stmt, temps, outputs);
}

VSEL_OP * Find_Vsel_Op(VSEL_IMEMS &table, INT fc, INT vl)
{
    VSEL_OP *vsel_ops = NULL;
    for (INT i = 0; i < table.Elements(); i++) {
	VSEL_IMEM *cur = table[i];
	if (cur->field_count == fc &&
	    cur->vec_width == vl) {
	    vsel_ops = cur->vsel_ops;
	    break;
	}
	Is_True(cur->field_count <= fc || cur->vec_width <= vl,
		("Find_Vsel_Op: ordering error in table"));
    }
    return vsel_ops;
}

void
IMEM_ELEM::Transform_By_Table(WN *stmt, SIMD_PREGS *inputs, 
			      SIMD_PREGS *outputs, bool is_read)
{
    VSEL_IMEMS &vsel_table = (is_read) ? 
	Cur_Simd_Loop->IMem_Map().Vsel_R_Table() :
	Cur_Simd_Loop->IMem_Map().Vsel_W_Table();
    INT num_fields = outputs->Elements();

    for (INT i = 0; i < outputs->Elements(); i++) {
	inputs->AddElement((*outputs)[i]);
    }

    TYPE_ID vec_type   = (*inputs)[0]->Type();
    INT vec_width = Simd_Info->Get_SIMD_Width_SIMD(vec_type);

    VSEL_OP *vsel_ops = Find_Vsel_Op(vsel_table, num_fields, vec_width);
    Is_True(vsel_ops, ("IMEM_ELEM::Transform_By_Table: not in the table"));    
    
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(vec_type);
    while (vsel_ops->input_0 != -1) {
	SIMD_PREG *input_0 = (*inputs)[vsel_ops->input_0];
	SIMD_PREG *input_1 = (*inputs)[vsel_ops->input_1];
	SIMD_PREG *output = (*inputs)[vsel_ops->output_0];
	INT64 sel = simd_sel->Sel_Array_To_Imm(vsel_ops->sel_0);
	Cur_Simd_Loop->Generate_Sel_Imm(stmt, output, input_1, input_0, sel);
	vsel_ops++;
    }
}

/*---------------------------------------------------------------------------*
 * Deinterleave inputs into outputs                                          *
 *---------------------------------------------------------------------------*/
void 
IMEM_ELEM::Read_Into_Regs(WN *stmt, SIMD_PREGS *inputs, SIMD_PREGS *outputs,
			  bool pairwise)
{
    INT num_fields = outputs->Elements();
    FmtAssert(num_fields > 1, ("Number of fields less than 2"));

    if (pairwise) {
	Read_Into_Regs(stmt, inputs, outputs);
    } else if (num_fields == 2) {
	INT     input_size = inputs->Elements();
	Is_True(input_size > 0 && (input_size & 0x1) == 0, 
		("unexpected input size"));
	TYPE_ID vec_type   = (*inputs)[0]->Type();
	
	Is_True(input_size == num_fields, ("Input/Output size mismatch"));
	SIMD_PREGS *output_0 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	SIMD_PREGS *output_1 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	
	/* split the output into 2 */
	for (INT i = 0; i < (input_size >> 1); i++) {
	    output_0->AddElement((*outputs)[i]);
	    output_1->AddElement((*outputs)[(input_size >> 1) + i]);
	}
	
	Transform_Odd_Even_Shuffle(stmt, inputs, output_0, output_1);
    } else { /* not power of 2 */
	Transform_By_Table(stmt, inputs, outputs, true);
    }
}
			    
/*---------------------------------------------------------------------------*
 * Read from IMEM_ELEM's result registers to IMEM_OFFSET's result registers  *
 *---------------------------------------------------------------------------*/
void 
IMEM_ELEM::Read_Into_Regs(WN *stmt)
{
    /* TODO: Need to push/pop Mem_Pool if the pool is not the same
       as the SIMD_INFO's Mem_Pool */
    
    /* output registers */
    SIMD_PREGS *output_0 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
    SIMD_PREGS *output_1 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());

    INT num_fields = Offset_Count();
    for (INT i = 0; i < num_fields; i++) {
	IMEM_OFFSET *offset   = Offset(i);
	SIMD_PREGS  *res_regs = offset->Res_Regs();

	Is_True(res_regs->Elements() > 0, ("Offest result register is NULL"));
	output_0->AddElement((*res_regs)[0]);
	
	if (res_regs->Elements() > 1) {
	    Is_True(res_regs->Elements() == 2, 
		    ("Offset result register has too many parts"));
	    output_1->AddElement((*res_regs)[1]);
	}
    }
    
    TYPE_ID mtype      = Res_Reg(0)->Type();
    INT     reg_length = Simd_Info->Get_SIMD_Width_SIMD(mtype);
    bool    pairwise   = (num_fields > 2) && LNO_IS_POWER_OF_TWO(num_fields);

    if (output_1->Elements() != 0) {
	SIMD_PREGS *input_0  = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	SIMD_PREGS *input_1  = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	SIMD_PREGS *res_regs = Res_Regs();
	INT length           = res_regs->Elements();
	INT half_way         = length >> 1;
	for (INT i = 0; i < length; i++) {
	    if (i < half_way) {
		input_0->AddElement((*res_regs)[i]);
	    } else {
		input_1->AddElement((*res_regs)[i]);
	    }
	}
	Read_Into_Regs(stmt, input_0, output_0, pairwise);
	Read_Into_Regs(stmt, input_1, output_1, pairwise);
    } else {
	Read_Into_Regs(stmt, Res_Regs(), output_0, pairwise);
    }
}

void 
IMEM_ELEM::Write_Out_Regs_P2(WN *stmt, SIMD_PREGS *inputs, SIMD_PREGS *outputs)
{
    INT num_fields = outputs->Elements();
    FmtAssert(num_fields > 1, ("Number of fields is less than 2"));

    INT     input_size = inputs->Elements();
    Is_True(input_size > 0 && (input_size & 0x1) == 0, 
	    ("unexpected input size"));
    TYPE_ID vec_type   = (*inputs)[0]->Type();
    int vec_width = Simd_Info->Get_SIMD_Width_SIMD(vec_type);

    FmtAssert(LNO_IS_POWER_OF_TWO(num_fields), ("Number of fields is not power of 2"));

    Is_True(input_size == num_fields, ("Input/Output size mismatch"));
    Is_True(vec_width >= num_fields, ("Number of fields > vec_width"));

    // intermediate steps
    SIMD_PREGS *temps = inputs;

    // note here we use the num_fields instead of vec_width as in Read
    for (INT good_width=2; good_width<num_fields; good_width=good_width<<1){
	inputs = temps;
	temps = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	for (INT i = 0; i < input_size; i++) {
	    temps->AddElement(Gen_Symbol(vec_type, Mem_Pool()));
	}
	Transform_Interleave_Shuffle(stmt, inputs, temps);
    }

    // final step
    Transform_Interleave_Shuffle(stmt, temps, outputs);
}

/*---------------------------------------------------------------------------*
 * Write from IMEM_OFFSET's result registers to IMEM_ELEM's result registers *
 *---------------------------------------------------------------------------*/
void
IMEM_ELEM::Write_Out_Regs(WN *stmt, SIMD_PREGS *inputs, SIMD_PREGS *outputs)
{
    INT num_fields = inputs->Elements();
    FmtAssert(num_fields > 1, ("Number of fields is less than 2"));

    INT     output_size = outputs->Elements();
    TYPE_ID vec_type   = (*inputs)[0]->Type();
    int     vec_width  = Simd_Info->Get_SIMD_Width_SIMD(vec_type);

    if ( LNO_IS_POWER_OF_TWO(num_fields) ) {
	if (num_fields > vec_width) {
	    // divide the input into groups with vec_width fields
	    // in each group
	    INT groups = num_fields/vec_width;
	    for (INT i = 0; i < groups; i++) {
		SIMD_PREGS *t_in  = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
		SIMD_PREGS *t_out = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
		
		// split the input sequentially into groups
		// split the output in stride vec_width
		for (INT j = 0; j < vec_width; j++) {
		    t_in->AddElement((*inputs)[i*vec_width+j]);
		    t_out->AddElement((*outputs)[i+groups*j]);
		}
		Write_Out_Regs_P2(stmt, t_in, t_out);
	    }
	} else {
	    Write_Out_Regs_P2(stmt, inputs, outputs);
	}
    } else {
	Transform_By_Table(stmt, inputs, outputs, false);
    }
}

/*---------------------------------------------------------------------------*
 * Write from IMEM_OFFSET's result registers to IMEM_ELEM's result registers *
 *---------------------------------------------------------------------------*/
void
IMEM_ELEM::Write_Out_Regs (WN *stmt)
{
    if (Offset_Count()<2) {
	// nothing. assume that all registers allocated IMEM_ELEM and IMEM_OFFSET
	// levels are the same
	return;
    }

    /* input registers */
    SIMD_PREGS *input_0 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
    SIMD_PREGS *input_1 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());

    for (INT i = 0; i < Offset_Count(); i++) {
	IMEM_OFFSET *offset   = Offset(i);
	SIMD_PREGS  *res_regs = offset->Res_Regs();

	Is_True(res_regs->Elements() > 0, ("Offest result register is NULL"));
	input_0->AddElement((*res_regs)[0]);
	
	if (res_regs->Elements() > 1) {
	    Is_True(res_regs->Elements() == 2, 
		    ("Offset result register has too many parts"));
	    input_1->AddElement((*res_regs)[1]);
	}
    }
    
    if (input_1->Elements() != 0) {
	SIMD_PREGS *output_0 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	SIMD_PREGS *output_1 = CXX_NEW(SIMD_PREGS(Mem_Pool()),Mem_Pool());
	SIMD_PREGS *res_regs = Res_Regs();
	INT length           = res_regs->Elements();
	INT half_way         = length >> 1;
	for (INT i = 0; i < length; i++) {
	    if (i < half_way) {
		output_0->AddElement((*res_regs)[i]);
	    } else {
		output_1->AddElement((*res_regs)[i]);
	    }
	}
	Write_Out_Regs(stmt, input_0, output_0);
	Write_Out_Regs(stmt, input_1, output_1);
    } else {
	Write_Out_Regs(stmt, input_0, Res_Regs());
    }
}

bool
IMEM_ELEM::Test_Sel_Fields_By_Table(TYPE_ID vec_type, INT num_fields,
				    bool is_read)
{
    /* not a power of 2, by table */
    VSEL_IMEMS &vsel_table = (is_read) ? 
	Cur_Simd_Loop->IMem_Map().Vsel_R_Table() :
	Cur_Simd_Loop->IMem_Map().Vsel_W_Table();
    
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(vec_type);
    Is_True(simd_sel != NULL, ("NULL SIMD select for %s", MTYPE_name(vec_type)));
    INT vec_width = Simd_Info->Get_SIMD_Width_SIMD(vec_type);
    VSEL_OP *vsel_ops = Find_Vsel_Op(vsel_table, num_fields, vec_width);
    if (vsel_ops == NULL) {
	return false;
    }
    while (vsel_ops->input_0 != -1) {
	INT64 sel = simd_sel->Sel_Array_To_Imm(vsel_ops->sel_0);
	if (!Simd_Ti->Tie_Sel_Possible(vec_type, sel)) {
	    return false;
	}
	vsel_ops++;
    }
    return true;
}

bool
IMEM_ELEM::Test_Sel_Fields (bool pairwise, bool is_read)
{
    TYPE_ID elem_type = Parent_Group()->Scalar_Type();
    TYPE_ID vec_type = Simd_Info->Get_SIMD_Reg_Type_Scalar(elem_type);
    SIMD_SELECT *simd_sel = Simd_Info->Get_SIMD_Select_SIMD(vec_type);
    Is_True(simd_sel != NULL, ("NULL SIMD select for %s", MTYPE_name(vec_type)));
    INT num_fields = Offset_Count();
    
    if (pairwise) {
	if (LNO_IS_POWER_OF_TWO(num_fields)) {
	    INT64 dsel = simd_sel->Dsel_73625140();
	    return Simd_Ti->Tie_Dsel_Possible(vec_type, dsel);
	} 
	return Test_Sel_Fields_By_Table(vec_type, num_fields, is_read);
    } else {
	/* direct read using deinterleave */
	if (num_fields == 2) {
	    INT64 dsel = simd_sel->Dsel_75316420();
	    return Simd_Ti->Tie_Dsel_Possible(vec_type, dsel);
	} else {
	    return Test_Sel_Fields_By_Table(vec_type, num_fields, is_read);
	}
    }
}

bool
IMEM_ELEM::Test_Sel_Read_Into_Regs (void)
{
    INT num_fields = Offset_Count();
    INT reg_length = Simd_Info->Get_SIMD_Width_Scalar(Parent_Group()->Scalar_Type());
    bool pairwise = (num_fields > 2) && LNO_IS_POWER_OF_TWO(num_fields);
    return Test_Sel_Fields(pairwise, true);
}


bool
IMEM_ELEM::Test_Sel_Write_Out_Regs (void)
{
    INT num_fields = Offset_Count();
    INT reg_length = Simd_Info->Get_SIMD_Width_Scalar(Parent_Group()->Scalar_Type());
    
    bool pairwise = true;
    
    /* write use the same select as read, only the input are organized
       differently */
    return Test_Sel_Fields(pairwise, false);
}


void
IMEM_OFFSET::Transform_Load (WN *load, WN *stmt) {
    if (Loaded())
	return;
    Parent_Elem()->Transform_Load(load, stmt);
    Set_Loaded();
}


void
IMEM_OFFSET::Transform_Store (WN *store, WN *stmt) {
    Parent_Elem()->Transform_Store(store, stmt);
}


void
IMEM_OFFSET::Print(FILE *out_file, int indent) {
    fprint_indent(out_file,indent);
    fprintf(out_file,"IMEM_OFFSET {\n");
    indent+=4;
    {
	IMEM_INFO *ii = Imem_Info();
	fprint_indent(out_file,indent);
	ii->Array_Base()->Print(out_file);
	fprintf(out_file," (.%d) ",ii->WN_Offset());
	ii->Access_Array()->Print(out_file,/* is_bound */ FALSE);

	fprint_indent(out_file,indent);
	fprintf(out_file,"Flags: %08x\n",_flags);
	
	fprint_indent(out_file,indent);
	fprintf(out_file,ii->Is_Aligned()?"ALIGNED":"UNALIGNED");
	fprintf(out_file,"\n");

	if (Res_Reg_Count()) {
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "Result registers {\n");
	    for (INT i = 0; i < Res_Regs()->Elements(); i++) {
		Res_Reg(i)->Print(out_file, indent+4);
	    }
	    fprint_indent(out_file, indent);
	    fprintf(out_file, "}\n");
	}
    } // imem offset
    indent-=4;
    fprint_indent(out_file,indent);
    fprintf(out_file,"}\n");
}

void
IMEM_INFO::Print(FILE *outFile, int indent) {
    fprint_indent(outFile,indent);
    fprintf(outFile,"IMEM_INFO {\n");
    fprint_indent(outFile,indent+4);
    Array_Base()->Print(outFile);
    fprintf(outFile," (.%d) ",WN_Offset());
    Access_Array()->Print(outFile,/* is_bound */ FALSE);
    fprint_indent(outFile,indent);
    fprintf(outFile,"}\n");
}


bool
IMEM_INFO::Is_First_Imem_In_Group (void)
{
  IMEM_OFFSET *io = Imem_Offset();
  return (!io || io->Parent_Group()->First_Imem_Info() == this);
}


// Compare array access base (TRUE if equal)
bool
IMEM_INFO::Same_Imem_Base (IMEM_INFO *ii) {
    return *Array_Base() == *ii->Array_Base();
}

// Compare array indexes assuming equal bases (TRUE if differ only in the
// constant offset, which is returned in diff if it is not NULL)
bool
IMEM_INFO::Compare_Imem_Index (IMEM_INFO *ii, INT *diff) {
    Is_True(Same_Imem_Base(ii),
	    ("Different bases -- invalid array access comparison."));
    
    ACCESS_ARRAY *aa1 = Access_Array();
    ACCESS_ARRAY *aa2 = ii->Access_Array();
    if (aa1->Too_Messy || aa2->Too_Messy)
	return false;
    INT num_vec = aa1->Num_Vec();
    if (num_vec != aa2->Num_Vec())
	return false;
    for (INT i=0; i<num_vec-1; i++)
	if (!(*aa1->Dim(i)==*aa2->Dim(i)))
	    return false;
    
    bool result = false;
    MEM_POOL_Push(_pool);
    {
	ACCESS_VECTOR *av_diff = Subtract(aa1->Dim(num_vec-1),
					  aa2->Dim(num_vec-1),
					  _pool,/* use_max_depth */ TRUE);
	
	if (av_diff && av_diff->Is_Const()) {
	    result = true;
	    if (diff)
		*diff = av_diff->Const_Offset;
	}
    }
    MEM_POOL_Pop(_pool);
    
    return result;
}

// Compare the offsets of the memory accesses (returns the difference)
INT
IMEM_INFO::Diff_Imem_Offset (IMEM_INFO *ii) {
    Is_True(Same_Imem_Base(ii),
	    ("Different bases -- invalid array access comparison."));
    Is_True(Compare_Imem_Index(ii),
	    ("Different array acesses -- invalid IMEM access offset comparison."));
    return (INT)(WN_Offset()-ii->WN_Offset());
}

void
IMEM_Map::Group() {
  
  // sort first -- O(n^2)
  for (INT i=1; i<_array.Elements(); i++) { // for each element (i)
    IMEM_INFO *iii = _array[i];
    IMEM_INFO *iij = NULL;
    for (INT j=0; j<i; j++) { // check if (j) is a better position
      bool ins = false;
      IMEM_INFO *iijp = iij;
      iij = _array[j];
      
      INT idiff;
      if (iij->Same_Imem_Base(iii) &&
	  iij->Compare_Imem_Index(iii,&idiff)) {
	if (idiff>0) { // positive constant diff with (j) -- insert
	  ins = true;
	} else if (idiff==0) {
	  INT odiff=iij->Diff_Imem_Offset(iii);
	  if (odiff>0) { // positive offset diff with (j) -- insert
	    ins = true;
	  } else {
	    Is_True(odiff<0,("Same access, different IMEM_INFOs."));
	  }
	}
      } else if (iijp &&
		 iijp->Same_Imem_Base(iii) &&
		 iijp->Compare_Imem_Index(iii,&idiff)) {
	// negative diff with (j-1) -- insert
	// NOTE: if the diff was positive, (i) would have been inserted
	// during the previous iteration
	Is_True((idiff<0 || (idiff==0 && iijp->Diff_Imem_Offset(iii)<0)),
		("Bad IMEM_MAP sorting algorithm."));
	ins = true;
      }
      
      if (ins) { // insert/shift
	for (INT k=i; k>j; k--)
	  _array[k]=_array[k-1];
	_array[j]=iii;
	break; // j
      }
    } // for j
  } // for i
  
  IMEM_GROUP *ig = NULL;
  IMEM_ELEM *ie = NULL;
  IMEM_INFO *iii = NULL;
  INT simd_level = Simd_Loop()->Simd_Loop_Level();
  
  // iiip points to the first IMEM_INFO stored in the current group
  // iiie points to the fist IMEM_INFO of last IMEM_ELEM in the current group
  IMEM_INFO *iiip = NULL;
  IMEM_INFO *iiie = NULL;
  for (INT i=0; i<_array.Elements(); i++) {
    bool new_group = true;
    bool new_elem = true;
    iii = _array[i];
    
    // get the coefficient in front of the SIMD loop index variable
    ACCESS_ARRAY *access = iii->Access_Array();
    ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1);
    INT coeff = av->Loop_Coeff(simd_level);
    
    if (iiip &&
	iiip->Scalar_Type()==iii->Scalar_Type() &&
	iii->Is_Vector() &&
	iiip->Same_Imem_Base(iii)) {
	INT idiff;
	if (iiip->Compare_Imem_Index(iii,&idiff)) {
	    if (idiff<0) {
		if (coeff<=-idiff) {
		    if (!iiip->Is_Def() && !iii->Is_Def() &&
                        !iiip->Variable_Stride() && !iii->Variable_Stride()) {
			if (Simd_Info->AT_Analysis_Phase()) {
			    if (Simd_Info->SIMD_At()->Max_Width() > -idiff) {
				new_group = false;
			    }
			} else {
			    if (Simd_Loop()->V_Unroll_Factor(Simd_Info) > 
				(-idiff)) {
				// put a[i], a[i+1], ...,  a[i+vl-1] into the same group
				new_group = false;
			    }
			}
			/* compare with the current IE */
			if (!new_group && iiie && iiie->Compare_Imem_Index(iii, &idiff)) {
			    if (idiff == 0 || (idiff<0 && coeff>-idiff)) {
				new_elem = false;
			    }
			}
		    }
		} else {
		    // group under the same element a[2*i] and a[2*i+1]
		    // but not a[2*i] and a[2*i+2] or a[i]
		    new_group = false;
		    new_elem = false;
		}
	    } else {
		Is_True(idiff==0,("Wrong IMEM_MAP sort."));
		new_group=false;
		new_elem=false;
	    }
	}
    }
    
    if (new_group) {
      iiip = iii;
      new_elem = true;
      ig=CXX_NEW(IMEM_GROUP(_pool),_pool);
      Enter_Group(ig);
      ig->Set_Non_Const_Level(iii->Non_Const_Level());
      ig->Set_Scalar_Type(iii->Scalar_Type());
      if (iii->Is_Vector()) {
	  ig->Set_Is_Vector();
      } else if (iii->Is_Reduction()) {
	  ig->Set_Is_Reduction();
      }
      WN *wn=iii->Lod_Sto().Bottom_nth(0);
      if (WN_operator(wn)==OPR_ILOAD) {
	ig->Set_Base_Addr(WN_kid0(wn));
      } else {
	Is_True(WN_operator(wn)==OPR_ISTORE,("ISTORE expected."));
	ig->Set_Base_Addr(WN_kid1(wn));
      }
    } else {
      Is_True(ig->Non_Const_Level()==iii->Non_Const_Level(),
	      ("IMEM_GROUP: Bad non-const loop level."));
      Is_True(ig->Scalar_Type()==iii->Scalar_Type(),
	      ("IMEM_GROUP: Different scalar types."));
      Is_True(ig->Is_Vector()==iii->Is_Vector(),
	      ("IMEM_GROUP: Inconsistent vector property."));
    }
    
    if (new_elem) {
	iiie = iii;
	ie = CXX_NEW(IMEM_ELEM(_pool), _pool);
	ig->Add_Elem(ie);
	if (iii->Is_Vector() && !iii->Variable_Stride()) {
	    ie->Set_Elem_Byte_Size(coeff*iii->Element_Byte_Size());
	} else {
	    ie->Set_Elem_Byte_Size(iii->Data_Byte_Size());
	}
    } else {
	Is_True(ie->Elem_Byte_Size()==coeff*iii->Element_Byte_Size() ||
		iii->Variable_Stride(),
		("IMEM_ELEM: Different element sizes."));
    }
    
    IMEM_OFFSET *io = CXX_NEW(IMEM_OFFSET(iii,_pool),_pool);
    ie->Add_Offset(io);
    iii->Set_Imem_Offset(io);
  }
  
  /* setup properties per each group */
  for (INT i = 0; i < _groups.Elements(); i++) {
      IMEM_GROUP *ig = _groups[i];
      ig->Setup_Property();
  }
}

void
IMEM_Map::Split_Group()
{
    for (INT i = 0; i < _groups.Elements(); i++) {
	IMEM_GROUP *ig = _groups[i];
	if (ig->Elem_Count() > 1) {
	    ig->Split_Group(this);
	}
    }
}

void
IMEM_Map::Split_Group_No_Vsel()
{
    for (INT i = 0; i < _groups.Elements(); i++) {
	IMEM_GROUP *ig = _groups[i];
	if (ig->Elem_Count() > 1) {
	    ig->Split_Group_No_Vsel(this);
	}
    }
}

BOOL
IMEM_Map::Need_Split_Group_No_Vsel()
{
    for (INT i = 0; i < _groups.Elements(); i++) {
	IMEM_GROUP *ig = _groups[i];
	if (ig->Elem_Count() > 1) {
	    if (ig->Need_Split_Group_No_Vsel(this)) return true;
	}
    }
    return FALSE;
}

void
IMEM_Map::Collect_Load_Store()
{
  /* collect load/stores per each group */
  for (INT i = 0; i < _groups.Elements(); i++) {
      IMEM_GROUP *ig = _groups[i];
      ig->Collect_Load_Store();
  }
}

INT
IMEM_Map::Get_Max_Offset_Count (void) {
  INT max = 0;
  for (INT i = 0; i < _groups.Elements(); i++) {
    IMEM_GROUP *ig = _groups[i];
    INT oc = ig->Get_Max_Offset_Count();
    if (oc > max) {
      max = oc;
    }
  }

  return max;
}

IMEM_INFO*
IMEM_Map::Only_Write_Imem_Info() 
{
    INT        cnt       = 0;
    IMEM_INFO *imem_info = NULL;
    for (INT i=0; i<_groups.Elements(); i++) {
	IMEM_GROUP *ig = _groups[i];
	if (ig->Is_Def()) {
	    FmtAssert(cnt==0, ("IMEM_Map::Only_Write_Imem_Info: more than 1 write"));
	    cnt++;
	    imem_info = ig->First_Imem_Info();
	}
    }
    return imem_info;
}

void 
IMEM_GROUP::Collect_Load_Store()
{
    for (INT i = 0; i < Elem_Count(); i++) {
	IMEM_ELEM *ie = Elem(i);
	for (INT j = 0; j < ie->Offset_Count(); j++) {
	    IMEM_OFFSET *io = ie->Offset(j);
	    IMEM_INFO *im = io->Imem_Info();
	    EXPR_Stack &lod_sto = im->Lod_Sto();
	    for (INT k = 0; k < lod_sto.Elements(); k++) {
		WN *wn = lod_sto.Bottom_nth(k);
		OPERATOR oper = WN_operator(wn);
		if (oper == OPR_ISTORE) {
		    Stores()->AddElement(wn);
		} else {
		    Is_True(oper==OPR_ILOAD,("Expecting ILOAD"));
		    Loads()->AddElement(wn);
		}
	    }
	}
    }
}

bool
IMEM_GROUP::Is_Aligned()
{
    return First_Imem_Info()->Is_Aligned();
}

IMEM_INFO*
IMEM_GROUP::First_Imem_Info()
{
    return Elem(0)->Offset(0)->Imem_Info();
}

// in dep.cxx
extern WN *Find_First_Ldid_For_Symbol(WN *wn, const SYMBOL *symb);

static
bool UB_Symbol_Is_Power_Of_2(WN *wn_ub, WN* wn_delin, SYMBOL *delin_sym)
{
    if (!wn_ub) {
	return false;
    }
    WN *def_ub = NULL;
    if (Ldid_Is_Power_Of_2(wn_ub, def_ub)) {
	return true;
    }
	
    /* try to see if it is delin>>const */
    DEF_LIST *def_delin = Du_Mgr->Ud_Get_Def(wn_delin);
    if (!def_delin || def_delin->Incomplete()) {
	return false;
    }
    DEF_LIST_ITER iter(def_delin);
    if (!iter.First()){
	return false;
    }
    return (Is_Fraction_Of_Symbol(wn_ub, def_delin, delin_sym) != NULL);
}


static bool
Check_Base_Alignment (WN *base, INT req_align)
{
  switch (WN_operator(base)) {
  case OPR_LDA:
  {
    /* LDA: check if the base is an array variable */
    ST_IDX st_idx = WN_st_idx(base);
    TY_IDX ty_idx = ST_type(st_idx);
    if (ST_class(st_idx) != CLASS_VAR ||
        TY_kind(ty_idx) != KIND_ARRAY) {
      Lmt_DevWarn(1, ("Array access base (LDA) is not an array variable!"));
      return false;
    }
    
    if (TY_align(ty_idx) < req_align && !Align_Arrays)
      return false;
    
    if (WN_offset(base) % req_align != 0)
      return false;
  }
  break;
  
  case OPR_LDID:
  {
    /* LDID: check if a struct or a pointer and the appropriate flags. */
    SYMBOL sym(base);
    ST_IDX st_idx = WN_st_idx(base);
    ST *st = &(St_Table[st_idx]);
    ST_CLASS st_class = ST_class(st);
    TY_IDX ty_idx = ST_type(st);
    TY_KIND ty_kind = TY_kind(ty_idx);
    INT sym_align = Simd_Info->Alignment_Table()->Find(sym);
    INT def_align = Ldid_Def_Alignment(base);
    if (simd_debug) {
      fprintf(TFile,"Alignment for symbol %s -- alignment table %d, DEF %d\n",
              sym.Name(), sym_align, def_align);
    }

    sym_align = MAX(sym_align, def_align);
    
    switch (ty_kind) {
    case KIND_POINTER:
    {
      if (st_class != CLASS_VAR) {
        DevWarn("Unsupported array access base class for LDID KIND_POINTER.");
        return false;
      }
      
      /* Base is LDID var (pointer). Offset should be zero. */
      if (WN_offset(base)) {
        DevWarn("Non-zero offset in array access base pointer to SIMD (%d)!",
                WN_offset(base));
        return false;
      }
      
      /* Check the alignment of the pointed type. */
      TY_IDX pointed_ty_idx = TY_pointed(ty_idx);
      INT pointed_align = TY_align(pointed_ty_idx);
      sym_align = MAX(sym_align, pointed_align);
      
      if (!LNO_Aligned_Pointers && /* all pointers -> aligned arrays */
          !(ST_sclass(st) == SCLASS_FORMAL &&
            LNO_Aligned_Formal_Pointers) && /* formal pointers -> aligned */
          (sym_align == 0 || sym_align % req_align != 0))
        return false;
    }
    break;
      
    case KIND_STRUCT:
      /* base is LDID struct field */
      if (st_class != CLASS_VAR)
        return false;
      
      if (!LNO_Aligned_Pointers && /* all pointers -> aligned arrays */
          (sym_align == 0 || sym_align % req_align != 0))
        return false;

      break;
      
    case KIND_SCALAR:
      if (st_class != CLASS_PREG) {
        DevWarn("Unsupported array access base class for LDID KIND_SCALAR.");
        return false;
      }

      /* Base is LDID pseudo-register (pointer). Offset does not matter. */
      if (!LNO_Aligned_Pointers) /* all pointers -> aligned arrays */
        return false;
      
      break;
      
    default:
      DevWarn("Unsupported array access base kind for LDID.");
      return false;
    }
  }

  break; /* OPR_LDID */
  
  default:
    DevWarn("Unsupported array access base operator %s.",
            OPERATOR_name(WN_operator(base)));
    return false;
  }
  
  return true;
}


void
IMEM_INFO::Set_Alignment(SIMD_LOOP *simd, WN *loop, INT loop_level,
                         WN *loop_inner, INT loop_inner_level) {
    Is_True(simd,("No SIMD_LOOP!"));

    Reset_Is_Aligned();

    if (LNO_Aligned_Accesses || // assume all memory accesses are vector aligned,
	(simd->Peeled() && Is_Def())) { // or this is the store that loop peeling aligns
      Set_Is_Aligned();
      return;
    }

    if (LNO_Aligned_Loads && Is_Use()) {
	Set_Is_Aligned();
	return;
    }

    if (LNO_Aligned_Stores && Is_Def()) {
	Set_Is_Aligned();
	return;
    }

    if (Variable_Stride()) {
	Set_Is_Aligned();
	return;
    }

    WN *simd_loop = simd->Simd_Loop();

    Is_True(simd_loop!=NULL,("No loop to SIMD in SIMD_LOOP"));
    
    INT simd_index = simd->Simd_Loop_Level();

    if (loop) {
	simd_loop  = loop;
	simd_index = loop_level;
    }
    
    // get the minimum required alignment for vector accesses
    INT req_align = 0;
    if (Simd_Info->AT_Analysis_Phase()) {
      req_align = simd->V_Unroll_Factor() * MTYPE_alignment(Scalar_Type());
      req_align = MIN(req_align, 16);
      while (!LNO_IS_POWER_OF_TWO(req_align)) {
	req_align++;
      }
      Is_True(req_align > 0 && req_align <= 16 && LNO_IS_POWER_OF_TWO(req_align),
	      ("Bad required alignment %d", req_align));
    } else {
      req_align = MTYPE_alignment(Simd_Info->Get_SIMD_Mem_Type_Scalar(Scalar_Type()));
    }
    if (simd_debug) {
      fprintf(TFile,"Required Alignment: %d\n", req_align);
    }
      
    ACCESS_ARRAY *access = Access_Array();
    Is_True(access, ("Cannot find access array"));
    
    ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1); // last dimension
    Is_True(av, ("No access vector for Dim(access->Num_Vec()-1)"));
    if (av->Too_Messy)
	return;
    
    // get one access instance
    WN *ref = Lod_Sto().Bottom_nth(0);
    
    WN *array_wn = (OPERATOR_is_load(WN_operator(ref)))?WN_kid0(ref):WN_kid1(ref);
    Is_True(WN_operator(array_wn)==OPR_ARRAY, ("Not an array access!"));

    // check the WN offset
    if (WN_Offset() % req_align != 0)
      return;
    
    /* Check the array base alignment. */
    WN *base = WN_array_base(array_wn);
    if (!Check_Base_Alignment(base, req_align))
      return;
    
    WN_ESIZE el_size = WN_element_size(array_wn);
    // check last dimension only if there are multiple dimensions
    // note that delinearization may cause some extra dimensions in the
    // access array
    INT num_dim = WN_num_dim(array_wn);
    INT acc_dim = access->Num_Vec();
    INT64 last_dim = 0;
    bool check_last = false;
    bool check_power_of_2 = false;
    WN   *wn_delin = NULL;
    SYMBOL *s=NULL;
    if (acc_dim>1 &&
	(s = access->Dim(acc_dim-2)->Delinearized_Symbol)!=NULL) {
      last_dim = Simd_Info->Alignment_Table()->Find(*s);
      check_last = true;
      check_power_of_2 = true;
    } else if (num_dim>1) {
      WN *last_dim_wn = WN_array_dim(array_wn,num_dim-1);
      // not a constant dimension
      if (WN_operator(last_dim_wn) != OPR_INTCONST)
	return;
      last_dim = WN_const_val(last_dim_wn);
      check_last = true;
    }
    if (check_last) {
	if (last_dim<=0) {
	    if (!check_power_of_2) {
		return;
	    } else {// check if the delinearized symbol is power of 2
		WN *array_index = WN_array_index(array_wn, acc_dim-2);
		wn_delin = Find_First_Ldid_For_Symbol(array_index, s);
		if (!wn_delin) {
		    return;
		}
		WN *def_wn = NULL;
		if (!Ldid_Is_Power_Of_2(wn_delin, def_wn)) {
		    return;
		}
		// If the size is power of 2 and it is greater than the span
		// of the index (for delinarization to happen), it will be
		// aligned because the loop in unrolled by vector amount.
		if (simd_debug) {
		    fprintf(TFile,"Last dimension is delinearized power of 2\n");
		}
	    }

	} else {
	    // check el_size * LAST_DIM
	    if ((el_size*last_dim)%req_align)
		return;
	    if (simd_debug) {
		fprintf(TFile,"Last dimension multiple of %d, element size %d\n",
			(INT)last_dim,(INT)el_size);
	    }
	}
    }
    
    // check the lower bound (Bottom_nth should return simd_loop for now)
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(simd_loop);
    
    ACCESS_ARRAY *lb_array = dli->LB;
    if (lb_array->Num_Vec() != 1) // should not be a MIN/MAX of multiple expressions
	return;
    
    // Get the lower bound as an expression (a copy of the do loop info bound)
    ACCESS_VECTOR *lb_vector = lb_array->Dim(0)->Convert_Bound_To_Exp(simd->Pool());
    if (lb_vector->Too_Messy)
	return;
    Is_True(lb_vector->Nest_Depth()<=av->Nest_Depth(),
	    ("Bad relation between loop lower bound and array access vector depth!"));
    Is_True(lb_vector->Nest_Depth()==simd_index+1,
	    ("Bad relation between simd loop depth and simd loop lower bound depth!"));
    Is_True(lb_vector->Loop_Coeff(simd_index)==0,
	    ("Index variable has non-zero coefficient in loop lower bound expression!"));
    // The simd loop might not be the one that encloses the access so we need
    // to increase the lower bound vector depth so we can add/subtract.
    lb_vector->Set_Nest_Depth(av->Nest_Depth());
    
    // Substitute the lower bound for the index variable in the array access
    INT simd_coeff = av->Loop_Coeff(simd_index);
    ACCESS_VECTOR *subst = Add(av,
			       Mul(simd_coeff, lb_vector, simd->Pool()),
			       simd->Pool());
    if (!subst) { // should not happen
	DevWarn("Cannot substitute lower bound into array access!");
	return;
    }
    Is_True(subst->Loop_Coeff(simd_index)==simd_coeff,
	    ("Wrong substitution during vector alignment check!"));
    subst->Set_Loop_Coeff(simd_index,0);

    // Replace index_loop_inner with the LB of loop_inner
    if (loop_inner != NULL) {
	// check the lower bound (Bottom_nth should return simd_loop for now)
	DO_LOOP_INFO *dli_inner = Get_Do_Loop_Info(loop_inner);
        ACCESS_ARRAY *lb_array_inner = dli_inner->LB;
	if (lb_array_inner->Num_Vec() == 1) {
	    // should not be a MIN/MAX of multiple expressions
	    // Get the lower bound as an expression 
	    // (a copy of the do loop info bound)
	    ACCESS_VECTOR *lb_vector_inner = 
		lb_array_inner->Dim(0)->Convert_Bound_To_Exp(simd->Pool());
	    if (!lb_vector_inner->Too_Messy) {
		Is_True(lb_vector_inner->Nest_Depth()<=av->Nest_Depth(),
			("Bad relation between loop lower bound and array access vector depth!"));
		Is_True(lb_vector_inner->Nest_Depth() == loop_inner_level+1,
			("Bad relation between loop1 depth and loop1 lower bound depth!"));
		Is_True(lb_vector_inner->Loop_Coeff(loop_inner_level)==0,
			("Index variable has non-zero coefficient in loop lower bound expression!"));
		// The loop_inner might not be the one that encloses the 
		// access so we need to increase the lower bound vector depth
		// so we can add/subtract.
		lb_vector_inner->Set_Nest_Depth(av->Nest_Depth());
    
		// Substitute the lower bound for the index variable in the array access
		INT loop_inner_coeff = av->Loop_Coeff(loop_inner_level);
		subst = 
		    Add(subst,Mul(loop_inner_coeff, lb_vector_inner, 
				  simd->Pool()), simd->Pool());
		if (!subst) { // should not happen
		    DevWarn("Cannot substitute lower bound into array access!");
		    return;
		}
		Is_True(subst->Loop_Coeff(loop_inner_level)==loop_inner_coeff,
			("Wrong substitution during vector alignment check!"));
		subst->Set_Loop_Coeff(loop_inner_level,0);
		
	    }
	}
    }
    
    // Remove power_of_2 symbols that is the upper bound of the simd loop
    // with 0 lower bound from the subst->Lin_Symb because the loop will
    // execute at least simd_vector length amount
    if (check_power_of_2 && subst->Lin_Symb->Head() &&
	WN_operator(WN_kid0(WN_start(simd_loop))) == OPR_INTCONST &&
	WN_const_val(WN_kid0(WN_start(simd_loop))) == 0) {
        ACCESS_ARRAY *ub_array = dli->UB;
	if (ub_array->Num_Vec() == 1) {
	    // should not be a MIN/MAX of multiple expressions
	    // Get the upper bound as an expression 
	    ACCESS_VECTOR *ub_vector = 
		ub_array->Dim(0)->Convert_Bound_To_Exp(simd->Pool());
	    if (!ub_vector->Too_Messy) {
		// check for "n-1" as the upper bound
		bool is_simple_bound = 
		    (ub_vector->Const_Offset == -1) && 
		    ub_vector->Lin_Symb != NULL &&
		    (ub_vector->Lin_Symb->Head() ==
		     ub_vector->Lin_Symb->Tail()) &&
		    (ub_vector->Non_Lin_Symb == NULL);
		if (is_simple_bound) {
		    for (INT i = 0; i < ub_vector->Nest_Depth(); i++) {
			if (ub_vector->Loop_Coeff(i) != 0) {
			    is_simple_bound = false;
			    break;
			}
		    }
		}

		SYMBOL *ub_sym = &ub_vector->Lin_Symb->Head()->Symbol;
		WN     *wn_ub  = Find_First_Ldid_For_Symbol(WN_end(simd_loop),
							    ub_sym);
		bool ub_is_power_of_2 = 
		    UB_Symbol_Is_Power_Of_2(wn_ub, wn_delin, s);

		if (is_simple_bound && ub_is_power_of_2) {
		    // remove the 'n' from the 'subst' if it exists
		    INTSYMB_ITER iter(subst->Lin_Symb);
		    INTSYMB_NODE *prevnode = NULL;
		    for (INTSYMB_NODE *n = iter.First(); !iter.Is_Empty();
			 n = iter.Next()) {
			if (n->Symbol == *ub_sym) {
			    if (prevnode) {
				CXX_DELETE(subst->Lin_Symb->Remove(prevnode, n),simd->Pool());
				if (simd_debug) {
				    fprintf(TFile,"Remove power of 2 upper bound symbol from the alignment check\n");
				}
			    } else {
				CXX_DELETE(subst->Lin_Symb->Remove_Headnode(),simd->Pool());
				if (simd_debug) {
				    fprintf(TFile,"Remove power of 2 upper bound symbol from the alignment check\n");
				}
			    }
			    break;
			}
		    }
		}
	    }
	}
    }
    
    // Find the gcd of the substituted vector and the required alignment.
    // Note that 'align' will be a power-of-two because req_align is a power-of-two.
    INT align = gcd(subst,Simd_Info->Alignment_Table(),req_align);
    Is_True(LNO_IS_POWER_OF_TWO(align),("Bad GCD result."));
    if (simd_debug) {
      fprintf(TFile,"Index multiple of %d, element size %d\n",
	      align,el_size);
      fprintf(TFile,"Index (before LB): ");
      av->Print(TFile);
      fprintf(TFile,"\n");
      fprintf(TFile,"Index (LB plugged in): ");
      subst->Print(TFile);
      fprintf(TFile,"\n");
    }
    if ((el_size*align)%req_align!=0)
      return;
    
    if (loop_inner) {
	Set_2D_Aligned();
    } else {
	Set_Is_Aligned(); // passes all tests
    }
    if (simd_debug) {
      fprintf(TFile,"Aligned.\n");
    }
}

/*---------------------------------------------------------------------------*
 * Check for unhandable stride access                                        *
 *---------------------------------------------------------------------------*/
bool
IMEM_INFO::Check_Stride (void) {
  Is_True(!Too_Messy(), ("Expected non-messy array access."));

  ACCESS_ARRAY *access = Access_Array();
  Is_True(access, ("Cannot find access array"));
  
  SIMD_LOOP *simd_loop = Simd_Loop();
  INT simd_level = simd_loop->Simd_Loop_Level();
  
  if (simd_debug) {
    fprintf(TFile,"Testing SIMD (level %d) stride: ", simd_level);
    access->Print(TFile);
    fprintf(TFile,"\n");
  }
  
  Is_True(!access->Too_Messy, ("Expected non-messy array access."));

  for (INT index = 0; index < access->Num_Vec()-1; index++) {
    ACCESS_VECTOR *av = access->Dim(index);
    Is_True(av, ("No access vector for Dim(%d)",index));
    Is_True(!av->Too_Messy, ("Expected non-messy array access vector."));
    
    if (av->Loop_Coeff(simd_level) != 0) {
      SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS_STRIDE,
               Orig_Wn(), SIMD_Base_Name_Msg(Orig_Wn()));
      return false;
    }
  }
  
  ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1); // stride 1 dimension
  Is_True(av, ("No access vector for Dim(access->Num_Vec()-1)"));
  Is_True(!av->Too_Messy, ("Expected non-messy array access vector."));
  
  if (av->Non_Const_Loops() > simd_level + 1) {
    SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS_STRIDE,
             Orig_Wn(), SIMD_Base_Name_Msg(Orig_Wn()));
    return false;
  }
  
  if (av->Loop_Coeff(simd_level) < 0) {
    SIMD_Msg(AT_MSG_SIMD_NEGATIVE_ACCESS_STRIDE,
             Orig_Wn(), SIMD_Base_Name_Msg(Orig_Wn()));
    return false;
  }
  
  // allow variable stride load 
  if (av->Non_Const_Loops() == simd_level + 1) {
    if (Is_Def()) {
      // Don't allow stores for now
      SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS_STRIDE,
               Orig_Wn(), SIMD_Base_Name_Msg(Orig_Wn()));
      return false;            
    } else {
      Set_Variable_Stride();
    }
  }
  
  return true;
}


void
IMEM_INFO::Init_Access_Array (WN *array_wn)
{
  _access = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, array_wn);
  Is_True(_access != NULL, ("Null access array"));
  
  if (_access->Too_Messy) {
    Set_Too_Messy();
    return;
  }
  
  for (INT index = 0; index < _access->Num_Vec(); index++) {
    ACCESS_VECTOR *av = _access->Dim(index);
    Is_True(av, ("No access vector for Dim(%d)", index));
    if (av->Too_Messy) {
      Set_Too_Messy();
      return;
    }
  }
}
  

/*---------------------------------------------------------------------------*
 * Constructor                                                               *
 *---------------------------------------------------------------------------*/
IMEM_INFO::IMEM_INFO(WN *wn, IMEM_Map *imem_map, MEM_POOL *pool) :
  _lodsto(pool), _offset(::WN_offset(wn)), _flag(0), _eInfo(NULL),
  _to_loop(NULL), _prev_lod(NULL), _non_const_level(-1), 
  _imem_offset(NULL), _pool(pool), _imem_map(imem_map), _if_conv(pool)
{
  OPERATOR op = WN_operator(wn);
  Is_True(op == OPR_ILOAD || op == OPR_ISTORE,
          ("Not an indirect load/store"));
  
  Add_Lod_Sto(wn);
  
  /* set reduction */
  if (red_manager && red_manager->Which_Reduction(wn) != RED_NONE) {
    Set_Reduction();
  }
  
  WN *array_wn  = (op == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn);
  Is_True(WN_operator(array_wn) == OPR_ARRAY, ("Not an array address"));
  
  /* Set structure access gap. */
  _element_byte_size = WN_element_size(array_wn);
  _scalar_type = WN_desc(wn);
  _data_byte_size = MTYPE_bit_size(_scalar_type) >> 3;
  
  WN *base= WN_array_base(array_wn);
  Is_True(WN_operator(base) == OPR_LDID || WN_operator(base) == OPR_LDA,
          ("Array base is not a scalar"));
  
  _array  = CXX_NEW(SYMBOL(base), pool);
  
  Init_Access_Array(array_wn);
}


void
IMEM_INFO::Add_Lod_Sto (WN *ls)
{
  _lodsto.Push(ls);   
  if (OPERATOR_is_load(WN_operator(ls))) {
    Set_Is_Use();
  } else if (OPERATOR_is_store(WN_operator(ls))) {
    Set_Is_Def();
  }
}


INT
IMEM_INFO::Stride_At_Loop(INT loop_level)
{
    ACCESS_ARRAY *access = Access_Array();
    Is_True(access, ("Cannot find access array"));
    
    ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1); // stride 1 dimension
    Is_True(av, ("No access vector for Dim(access->Num_Vec()-1)"));
    
    INT coeff = av->Loop_Coeff(loop_level);
    return coeff;
}

void
IMEM_INFO::Setup_Vector_Property(SIMD_LOOP *simd_loop) {
    ACCESS_ARRAY *access = Access_Array();
    Is_True(access, ("Cannot find access array"));
    
    ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1); // stride 1 dimension
    Is_True(av, ("No access vector for Dim(access->Num_Vec()-1)"));
    
    INT coeff = av->Loop_Coeff(simd_loop->Simd_Loop_Level());
    if (coeff > 0) {
	Set_Is_Vector();
    } else {
	Is_True(coeff==0,("Non-unit index coefficient"));
    }
    if (Variable_Stride()) {
	Set_Is_Vector();
    }
}


void
IMEM_INFO::Setup_Load_Property(LOOP_MODEL *lm, INT num_loops, 
			       SIMD_LOOP *doinfo, bool before_transform,
			       INT inner, bool msg) {
    ACCESS_ARRAY *access = Access_Array();
    Is_True(access, ("Cannot find access array"));
    
    ACCESS_VECTOR *av = access->Dim(access->Num_Vec()-1); // stride 1 dimension
    Is_True(av, ("No access vector for Dim(access->Num_Vec()-1)"));
    
    if (simd_debug) {
	fprintf(TFile, "setting up load property: ");
	access->Print(TFile, /* is_bound */ FALSE);
	fprintf(TFile, "\n");
    }

    INT simd_new_order = doinfo->Simd_Loop_Level();
    if (before_transform) {
	if (inner>=0) {
	    simd_new_order = lm->Old_To_New_Order(simd_new_order,inner);
	} else {
	    simd_new_order = lm->Old_To_New_Order(simd_new_order);
	}
    }

    /* Check if we can still handle the non-linear accesses after
       the interchange. See PR10438. */
    if (av->Non_Const_Loops() > simd_new_order + 1 ||
        (inner >= 0 &&
         inner < doinfo->Simd_Loop_Level() &&
         (av->Non_Const_Loops() > inner))) {
      doinfo->Set_Bad_Stride();
      if (msg) {
        SIMD_Msg(AT_MSG_SIMD_BAD_ACCESS_STRIDE,
                 Orig_Wn(), SIMD_Base_Name_Msg(Orig_Wn()));
      }
      return;
    }
    
    /* set up the inner most level where 'av' is not invariant */
    INT cur_min = -1;
    INT cur_coeff = 0;
    for (INT i = 0; i < av->Nest_Depth(); i++) {
	if (av->Non_Const_Loops() == i + 1 || av->Loop_Coeff(i) != 0) {
	    INT cur_depth  = i;
	    if (before_transform) {
		if (inner>=0) {
		    cur_depth = lm->Old_To_New_Order(cur_depth,inner);
		} else {
		    cur_depth = lm->Old_To_New_Order(cur_depth);
		}
	    }
	    if (cur_depth > cur_min) {
		cur_min = cur_depth;
		if (av->Loop_Coeff(i)) {
		    cur_coeff = av->Loop_Coeff(i);
		}
	    }
	}
    }
    
//    if (!before_transform) {
    if (1) {

	Reset_Has_Reuse();
	
	/* set up the memory reuse level if it exists */
	if (!Is_Def() && Is_Vector() && (cur_coeff == 1) 
	    && (cur_min == (simd_new_order + 1))) {
	    Set_Has_Reuse();
	}
	
	/* set the Non_Const_Level to the deepest of all access vectors */
	if (access->Non_Const_Loops()-1 > cur_min) {
	    cur_min = access->Non_Const_Loops() - 1;
	}
	
	for (INT i = 0; i < access->Num_Vec()-1; i++) {
	    av = access->Dim(i);
	    for (INT j = 0; j < av->Nest_Depth(); j++) {
		if (av->Loop_Coeff(j) != 0 && j > cur_min) {
		    cur_min = j;
		}
	    }
	}
	Set_Non_Const_Level((cur_min>0) ? cur_min : 0);
    }
}

/*---------------------------------------------------------------------------*
 * Same array and same access                                                *
 *---------------------------------------------------------------------------*/
bool 
IMEM_INFO::Is_Same_Array_Access(WN *wn)
{
    OPERATOR op = WN_operator(wn);
    Is_True(op == OPR_ILOAD ||op == OPR_ISTORE, ("No an indirect load/store"));

    WN *array_wn = (op == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn);
    Is_True(WN_operator(array_wn) == OPR_ARRAY, ("Not an array address"));

    WN *base = WN_array_base(array_wn);
    Is_True(WN_operator(base) == OPR_LDID || WN_operator(base) == OPR_LDA,
	    ("Array base is not a scalar"));

    SYMBOL sym(base);
    WN_OFFSET wn_offset = ::WN_offset(wn);
    
    if (!(*Array_Base() == sym) || wn_offset != WN_Offset())
    {
	return false;
    }
    /* same base and offset, check the access array */
    ACCESS_ARRAY *access_array = 
	(ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map, array_wn);
    Is_True(access_array != NULL, ("Null access array"));
    return (*access_array == *Access_Array());
}

WN *
IMEM_INFO::Get_IStore()
{
    Is_True(Is_Def(), ("Get_IStore: no store in the IMEM_INFO"));
    for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	WN *cur = Lod_Sto().Bottom_nth(i);
	if (WN_operator(cur) == OPR_ISTORE) {
	    return cur;
	}
    }
    FmtAssert(0, ("Get_IStore: no store in the IMEM_INFO"));
    return NULL;
}

WN *
IMEM_INFO::Get_Load_Copy()
{
    WN *copy = NULL;
    if (Is_Use()) {
	for (INT i = 0; i < Lod_Sto().Elements(); i++) {
	    WN *cur = Lod_Sto().Bottom_nth(i);
	    if (WN_operator(cur) == OPR_ILOAD) {
		copy = LWN_Copy_Tree(cur, TRUE, LNO_Info_Map);
		LWN_Copy_Def_Use(cur, copy, Du_Mgr);
		Simd_Copy_Dependence(cur, copy);
		break;
	    }
	}
	Is_True(copy != NULL, ("Cannot find the load"));
    } else {
	WN *cur = Lod_Sto().Bottom_nth(0);
	copy = Create_ILoad_From_IStore(cur, Du_Mgr, Array_Dependence_Graph);
	WN *array_wn = WN_kid1(cur);
	if (WN_operator(array_wn) == OPR_ARRAY) {
	    ACCESS_ARRAY *array = (ACCESS_ARRAY *) WN_MAP_Get(LNO_Info_Map,
							      array_wn);
	    Is_True(array, ("Missing array access"));
	    MEM_POOL *pool = array->Pool();
	    WN_MAP_Set(LNO_Info_Map, WN_kid0(copy), 
		       (void *) CXX_NEW(ACCESS_ARRAY(array, pool), pool));
	}
	Array_Dependence_Graph->Add_Vertex(copy);
    }
    WN *t = Lod_Sto().Bottom_nth(0);
    SIMD_EINFO *old_info = Cur_Simd_Loop->E_Info().Find(t);
    Is_True(old_info, ("IMEM_INFO::Get_Load_Copy: old EINFO is NULL"));
    SIMD_EINFO *e_info = Cur_Simd_Loop->Create_Simd_EInfo(copy, old_info);
    Cur_Simd_Loop->E_Info().Enter(copy, e_info);

    IMEM_INFO *imem = old_info->IMem();
    Is_True(imem, ("IMME_INFO::Get_Load_Copy: old IMEM_INFO is NULL"));
    e_info->Set_IMem(imem);
    imem->Add_Lod_Sto(copy);
    return copy;
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
