// Copyright (c) 2003 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_model.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_MODEL: SIMD modeling                                                *
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

// $Id: simd_model.h $

#ifndef __SIMD_MODEL__
#define __SIMD_MODEL__

#include "simd.h"
#include "simd_imem.h"

//
// SIMD_MODEL
//

class SIMD_MODEL {

private:
    MEM_POOL     *_pool;
    TI_RES_COUNT *_loop_res_count;
    TI_RES_COUNT *_setup_res_count;
    INT           _setup_scalar_count;
    INT           _loop_misc_count;
    INT           _loop_mem_count;
    INT           _align_reg;
    WN           *_inner_loop;
    float         _orig_count;
    INT           _res_ii;
    INT           _model_vl;

    void        Add_Address_Overhead(WN *);
    void        Add_Alignment_Overhead(WN *);
    
    void        Add_Simd_Loop_Op (WN *wn, WN *enclosing_loop);
    
    void        Add_Simd_Select_By_Table(bool setup,
					 WN *wn, TYPE_ID scalar_type,
					 INT num_fields, bool is_read);
    void        Add_Simd_Field_Select (bool setup,
				       WN *wn, TYPE_ID scalar_type,
				       INT num_fields, bool pairwise,
				       bool is_read);
    void        Add_Simd_Write_Select (bool setup, WN *wn, IMEM_ELEM *ie);
    void        Add_Simd_Read_Select (bool setup, WN *wn, IMEM_ELEM *ie);
    
    void        Add_Simd_Load_Shift_In (bool setup, IMEM_GROUP *ig);
    
    void        Add_Simd_Load_Reuse_Shift(bool setup, IMEM_ELEM *ie);
    void        Request_Load_Store(WN *wn, bool setup, INT cnt);
    void        Request_Mpy_Op(WN *wn, OPERATOR oper, TYPE_ID out_type, 
			       TYPE_ID in_type, bool setup, INT cnt);
    void        Request_Loop_Op(WN *wn, INT cnt, bool is_mem = false);

    INTRINSIC   Get_Select_Intrinsic(INT64 sel, TYPE_ID mtype, bool &has_dsel);
    void        Inc_Align_Reg_Count(INT c) { _align_reg += c; }
    WN         *Inner_Loop(void) { return _inner_loop; }
    void        Set_Inner_Loop(WN *wn) { _inner_loop = wn; }
    void        Inc_Orig_Count(float f)   { _orig_count += f; }
    void        Add_Simd_Orig_Op(WN *wn);

public:
    SIMD_MODEL(MEM_POOL *pool);
    ~SIMD_MODEL ();
    
    MEM_POOL *        Pool (void)      { return _pool; }
    INT               Min_II(void)     { return _res_ii; }
    float             Orig_Count(void) { return _orig_count; }
    bool              Beneficial(void);
    void              Request_Scalar_Op(WN *wn, bool setup, INT cnt);
    void              Request_Op(WN *wn, bool setup, INTRINSIC intrinsic, 
				 INT cnt);    
    // Add vector loop statistics to the current AT_PU for loop 'simd_loop'
    void              Model_Simd_Loop (SIMD_LOOP *simd_loop);
    void              Print(FILE *fp = stderr);
    
    
}; // SIMD_MODEL

#endif /* __SIMD_MODEL__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
