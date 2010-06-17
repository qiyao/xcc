// Copyright (c) 2003-2005 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_loop_at.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_LOOP_AT: Auto TIE SIMD transformation routines                      *
 *                                                                           *
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

// $Id: simd_loop_at.h $

#ifndef __SIMD_LOOP_AT__
#define __SIMD_LOOP_AT__

#include "simd.h"
#include "lno_bv.h"


class SIMD_LOOP_AT;
class GUARD_RANGE;
class GUARD_INFO;


class GUARD_RANGE
{
public:
  enum FLAGS {
    F_NONE     = 0x00,
    F_SIGNED   = 0x01,
    F_UNSIGNED = 0x02,
  };

private:
  UINT _flags;
  INT64 _minv;
  INT64 _maxv;

public:
  /* Null range. */
  GUARD_RANGE (void) :
    _flags(F_NONE), _minv(0), _maxv(0) { }
  
  /* Copy range. */
  GUARD_RANGE (const GUARD_RANGE &range) :
    _flags(range._flags), _minv(range._minv), _maxv(range._maxv) { }
  
  GUARD_RANGE (UINT flags, INT64 minv, INT64 maxv) :
    _flags(flags), _minv(minv), _maxv(maxv) { }

  /* Span bits. */
  GUARD_RANGE (bool is_signed, INT bit_size);
  
  INT64 minv (void) const { return _minv; }
  INT64 maxv (void) const { return _maxv; }
  
  bool is_signed (void) const { return _flags & F_SIGNED; }
  void set_signed (void) { _flags |= F_SIGNED; }
  void reset_signed (void) { _flags &= ~F_SIGNED; }
  
  bool is_unsigned (void) const { return _flags & F_UNSIGNED; }
  void set_unsigned (void) { _flags |= F_UNSIGNED; }
  void reset_unsigned (void) { _flags &= ~F_UNSIGNED; }
  
  void union_range (const GUARD_RANGE &range);
  void max_range (const GUARD_RANGE &range);
  void min_range (const GUARD_RANGE &range);
  void ashr_range (INT amount);
  void lshr_range (INT amount);
  void shl_range (INT amount);
  void abs_range (void);
  void negate_range (void);
  void add_range (const GUARD_RANGE &range);
  void sub_range (const GUARD_RANGE &range);  
  
  bool contains (const GUARD_RANGE &range) const;
  
  INT guards (INT type_bits) const;
};


class GUARD_INFO
{
private:
  INT _int8_guards;
  INT _int16_guards;

  /* Current processing info. */
  INT _type_bits;
  GUARD_RANGE _range;
  
public:
  GUARD_INFO (void) :
    _int8_guards(0), _int16_guards(0), _type_bits(0),
    _range() { }

  GUARD_RANGE &range (void) { return _range; }
  
  INT int8_guards (void) const { return _int8_guards; }
  INT int16_guards (void) const { return _int16_guards; }
  
  INT type_bits (void) const { return _type_bits; }
  void set_type_bits (INT type_bits) { _type_bits = type_bits; }
  
  /* Given the current type_bits and range update the
     required guard bit count. */
  void update_guards (void);
  
  void reset_range (void) { _range = GUARD_RANGE(); }
};


//
// SIMD_LOOP_AT
//

class SIMD_LOOP_AT : public SIMD_LOOP {
  
protected:
  BIT_VECTOR *_vector_lengths;
  
  INT _v_unroll_factor;
  
  bool _at_analysis;
  bool _at_transform;
  
  /* Guard bit analysis during transformation. */
  INT _req_int8_guards;
  INT _req_int16_guards;
  TYPE_ID _guard_types[4];
  
  INT Screen_Operator_Compute_Size_Rec(WN *, SIMD_IF_ANA *, INT trunc_bit_size =0);
  SIMD_EINFO *Setup_Type_Conversion_Rec(WN *);
  
  // returns the possibly narrowed return type of "wn"
  // based on 'bit_size' and wn's current rtype and desc
  TYPE_ID Get_Res_Type(const WN *wn, INT bit_size);
  TYPE_ID Parm_Type (const WN *parm_wn);
  
  virtual void Screen_Scalar_To_Vector_Conversion (TYPE_ID scalar_type);
  virtual void Screen_Operator (WN *wn, SIMD_EINFO *e_info);
  virtual void Screen_Type (TYPE_ID scalar_type, WN *wn);
  virtual bool Screen_Guards (void);
  virtual void Init_Vector_Lengths (BIT_VECTOR *vls);
  virtual INT Find_Unroll_Factor (void);

  SIMD_EINFO *Simd_Transform_Intrinsic (WN *, WN*, SIMD_INFO *);
  SIMD_EINFO *Simd_Transform_Parm (WN *, WN *, SIMD_INFO *);
  SIMD_EINFO *Simd_Transform_Outpart (WN *, WN *, SIMD_INFO *);
  
  bool Generate_Tie_Op(WN *expr, WN *stmt);
  
  GUARD_RANGE Find_Guards_Rec (WN *wn, GUARD_INFO *ginfo);
  void Collect_Guard_Type (TYPE_ID type);
  bool Check_Guards (INT vl);
    
public:
  SIMD_LOOP_AT(MEM_POOL *pool, LOOP_MODEL *lm,
	       WN *simd_loop, INT simd_loop_level);
  
  BIT_VECTOR *Vector_Lengths(void) { return _vector_lengths; }
  void Set_Vector_Lengths(BIT_VECTOR *vls) { _vector_lengths = vls; }
  
  bool AT_Analysis (void) const { return _at_analysis; }
  bool AT_Transform (void) const { return _at_transform; }
  
  virtual void Set_Default_Vl();
  virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p);
  virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info);
  
  virtual void Screen_Mulr(WN *wn);
  virtual INT Screen_Operator_Compute_Size(WN *, SIMD_IF_ANA *);
  
  virtual INT V_Unroll_Factor(SIMD_INFO *simd =NULL) { return _v_unroll_factor; }
  virtual INT Set_V_Unroll_Factor(INT factor) { _v_unroll_factor = factor; }
  
  virtual void Setup_Peeling (void) { /* no peeling */ }
  
  virtual SIMD_EINFO *Simd_Preprocess_Transform(WN* wn);
  
  virtual SIMD_EINFO *Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd);
  virtual SIMD_EINFO *Simd_Transform(WN *, WN*, SIMD_INFO *);
  
  virtual SIMD_EINFO *Setup_Type_Conversion(WN *wn);
  
  void Generate_Sel_Imm(WN *stmt, SIMD_PREG *out,
			SIMD_PREG *in1, SIMD_PREG *in2, INT64 imm,
			WN *block =NULL); 
  void Generate_Sel_Imm (WN *stmt, SIMD_PREG *out0, SIMD_PREG *out1,
			 SIMD_PREG *in0, SIMD_PREG *in1, INT64 sel0,
			 INT64 sel1, INT64 sel);
  void Generate_Dsel_Imm(WN *stmt, SIMD_PREG *out1, SIMD_PREG *out2,
			 SIMD_PREG *in1, SIMD_PREG *in2, INT64 imm,
			 WN *block =NULL);
  
  virtual WN* Generate_Sum_Reduction (TYPE_ID scalar_type, SIMD_PREG *vec_reg,
				      WN *block, INT64 line_number);
  virtual WN* Generate_Reduction (TYPE_ID scalar_type, SIMD_PREG *reg, OPERATOR op,
				  WN *stmt, INT64 line_number);
  
  virtual bool Test_If_Conversion (void);
  virtual bool Test_Imem_Alignment (void);
  virtual bool Test_Imem_Field_Selection (void);
  virtual bool Test_Imem_Load_Variable_Stride(IMEM_GROUP *ig);
  virtual bool  Test_Imem_Load_Reuse_Supported(IMEM_GROUP *ig);
  
  virtual void Further_Unroll_Loop(WN *loop, INT level, SIMD_INFO *simd,
				   EST_REGISTER_USAGE est_register_usage,
				   SX_INFO* pinfo,
				   INT pinfo_depth,
				   BOOL no_further_unroll,
				   SX_INFO** wdpinfo_ptr);
  
  WN*          Load_Shift(SIMD_EINFO *e_info, WN *wn, SIMD_INFO *simd);
  virtual void Generate_Load_Shift(WN *wn, SIMD_INFO *simd, 
				   E_Candidate &cand, WN *& wn_next);
  
  /* Find the minimum required guard bits in the tree rooted at 'wn'
     for 8 and 16 bit integer types. */
  void Find_Guard_Bits (WN *wn, INT &int8_guards, INT &int16_guards);
}; // SIMD_LOOP_AT


//
// SIMD_EINFO_AT
//

class SIMD_EINFO_AT : public SIMD_EINFO
{
public:
  SIMD_EINFO_AT(WN *expr, INT s, MEM_POOL *pool) :
    SIMD_EINFO(expr, s, pool) { }
  SIMD_EINFO_AT(WN *expr, SIMD_EINFO *e_info) :
    SIMD_EINFO(expr, e_info) { }
  
  /* Generate expr result into vector register */
  virtual void Generate_Simd_Expr(bool unary, WN *expr, WN *stmt, 
				  SIMD_EINFO *c1_info, SIMD_INFO *simd,
				  SIMD_EINFO *c2_info =NULL, INT idx =0);    
  virtual void Generate_Mula(WN *expr,
			     WN *child2, WN *stmt, 
			     SIMD_EINFO *c1_info, SIMD_LOOP *doinfo,
			     SIMD_INFO *simd);
  virtual void Generate_Mul_Expr(WN *expr, WN *stmt,
				 SIMD_EINFO *c1_info, SIMD_EINFO *s2_info,
				 SIMD_INFO *simd);
  virtual WN*  Generate_Mula_Expr(WN *expr, WN *stmt, 
				  SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
				  SIMD_EINFO *s2_info, WN *mul, 
				  SIMD_INFO *simd, INT idx = 0);
  
  virtual void Init_Sum_Reduction (WN *stmt);
};


#endif /* __SIMD_LOOP_AT__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

