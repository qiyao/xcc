// Copyright (c) 2003 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_ti_v2.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_TI_V2: Vectra 2 SIMD target info interface routines                 *
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

// $Id: simd_ti_v2.h $

#ifndef __SIMD_TI__V2__
#define __SIMD_TI__V2__

#include "mempool.h"

//
// SIMD_TI_V2
//

class SIMD_TI_V2 : public SIMD_TI {
    
public:
    static const INT NUM_MPY_OPERANDS = 4;
    
    SIMD_TI_V2(MEM_POOL *pool) : SIMD_TI(pool) {}
    ~SIMD_TI_V2() {}
    
    virtual TARG_SIMD_INFO *Targ_Simd_Info(TYPE_ID base, INT vl = 0);
    virtual TIE_MACRO_ID Search_Vector_Macro(OPERATOR op,
					     TYPE_ID dtype,
					     TYPE_ID rtype=MTYPE_UNKNOWN,
					     INT lohi=0, bool update=false);
    INTRINSIC Tie_Pack_Intrinsic(TYPE_ID out_type, TYPE_ID in_type);
    virtual INTRINSIC Tie_Shift_Intrinsic (OPERATOR oper, TYPE_ID vec_type);
    
    virtual bool Reduction_Possible (REDUCTION_TYPE red_type, TYPE_ID scalar_type);

    bool    Has_Normal_Cvt(TYPE_ID out_type, TYPE_ID in_type);
    bool    Has_Pack_Cvt(TYPE_ID out_type, TYPE_ID in_type);
    bool    Has_Mul_Cvt(TYPE_ID out_type, TYPE_ID in_type);
    bool    Can_Cvt(TYPE_ID out_type, TYPE_ID in_type);
}; // SIMD_TI_V2

#endif /* __SIMD_TI__V2__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

