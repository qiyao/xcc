// Copyright (c) 2003 by Tensilica Inc.  ALL RIGHTS RESERVED.
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

// $Id: simd_loop_v2.h $

#ifndef __SIMD_LOOP_V2__
#define __SIMD_LOOP_V2__

#include "simd_loop_at.h"

//
// SIMD_LOOP_V2
//

class SIMD_LOOP_V2 : public SIMD_LOOP_AT {

protected:
  virtual void Screen_Scalar_To_Vector_Conversion(TYPE_ID scalar_type);
  virtual void Screen_Operator(WN *wn, SIMD_EINFO *e_info);
  virtual void Screen_Type(TYPE_ID scalar_type, WN *wn);
  virtual bool Screen_Guards (void);
  virtual void Screen_Mulr(WN *wn);
  virtual void Init_Vector_Lengths(BIT_VECTOR *vls);
  virtual INT Find_Unroll_Factor (void);
  virtual SIMD_EINFO *Simd_Preprocess_Transform(WN* wn);
  
public:
    SIMD_LOOP_V2(MEM_POOL *pool, LOOP_MODEL *lm,
		 WN *simd_loop, INT simd_loop_level);
    
    virtual void Set_Default_Vl();
    virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, INT cur_size, MEM_POOL *p);  
    virtual SIMD_EINFO* Create_Simd_EInfo(WN *wn, SIMD_EINFO *e_info);  
    virtual SIMD_EINFO* Simd_Transform_CVT(WN *expr, WN *stmt, SIMD_INFO *simd);

}; // SIMD_LOOP_V2

//
// SIMD_EINFO_V2
//

class SIMD_EINFO_V2 : public SIMD_EINFO_AT {

private:
    WN  *Generate_Mulr_Call(WN *operand0, WN *operand1, 
			    WN *operand2, WN *operand3);
    WN  *Generate_Mulr_Call(WN *operand0, WN *operand1, 
			    WN *operand2, bool even);
    WN  *Generate_Mac_Call(OPERATOR op, WN *operand0, WN *operand1, 
			   WN *operand2, WN *operand3);
    WN  *Generate_Mac_Call(OPERATOR op, WN *operand0, WN *operand1, 
			   WN *operand2, bool even);
    WN  *Generate_Mul_Call(OPERATOR op, TYPE_ID out_type, 
			   WN *operand2, WN *operand3);
    WN  *Generate_Mul_Call(OPERATOR op, TYPE_ID out_type, 
			   WN *operand2, WN *operand3, bool even);
    
public:
    SIMD_EINFO_V2(WN *expr, INT s, MEM_POOL *pool) :
	SIMD_EINFO_AT(expr, s, pool) {}
    SIMD_EINFO_V2(WN *expr, SIMD_EINFO *e_info) :
	SIMD_EINFO_AT(expr, e_info) {}
   
    virtual void Generate_Simd_Expr(bool unary, WN *expr, WN *stmt, 
				    SIMD_EINFO *c1_info, SIMD_INFO *simd,
				    SIMD_EINFO *c2_info =NULL, INT idx =0);
    
    /* Generate mula/muls result into vector register */
    virtual WN*  Generate_Mula_Expr(WN *expr, WN *stmt, 
				    SIMD_EINFO *c1_info, SIMD_EINFO *s1_info,
				    SIMD_EINFO *s2_info, WN *mul, 
				    SIMD_INFO *simd, INT idx = 0);

    virtual void Generate_Mul_Expr(WN *expr, WN *stmt,
				   SIMD_EINFO *c1_info, SIMD_EINFO *s2_info,
				   SIMD_INFO *simd);
    virtual void Simd_Interleave_One(WN *stmt, SIMD_INFO *simd, 
				     SIMD_LOOP *doinfo);
    virtual void Simd_Interleave(WN* stmt, SIMD_INFO *simd, SIMD_LOOP *doinfo);
    virtual void Simd_Deinterleave_One(WN *stmt, SIMD_INFO *simd, 
				       SIMD_LOOP *doinfo, WN *block = NULL);
    virtual void Simd_Deinterleave(WN *stmt, SIMD_INFO *simd, 
				   SIMD_LOOP *doinfo, WN *block = NULL);

    void    Generate_Conv_Mul(WN *stmt, SIMD_EINFO *c_info);
    void    Generate_Conv_Pack(WN *stmt, SIMD_EINFO *c_info);

};
    
#endif /* __SIMD_LOOP_V2__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

