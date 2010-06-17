// Copyright (c) 2003-2004 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// simd_select.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  SIMD_SELECT: SIMD vector selection                                       *
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

// $Id: simd_select.h $

#ifndef __SIMD_SELECT__
#define __SIMD_SELECT__

#include <stdio.h>
#include <stdlib.h>

#include "simd.h"

//
// SIMD_SELECT
//
// Note: Some methods are named using the implied immediate that gets generated,
// assuming vector length of 4 (or 8), one element per way.
//

class SIMD_SELECT
{
private:
  // Overflow flag, clear before and set after each immediate request.
  bool _overflow;
  
  // Vector length of each input register.
  INT _vector_length;
  
  // Number of bits used to encode a selection index.
  INT _bits_per_index;
  
  // Number of adjacent elements that are selected together.
  INT _elems_per_way;

public:
  SIMD_SELECT (INT vector_length =0,
               INT bits_per_index =0,
               INT elems_per_way =0);

  bool Overflow (void) const { return _overflow; }
  void Set_Overflow (bool of) { _overflow = of; }
  
  INT Vector_Length (void) const { return _vector_length; }
  void Set_Vector_Length (INT vl) { _vector_length = vl; }
  
  INT Bits_Per_Index (void) const { return _bits_per_index; }
  void Set_Bits_Per_Index (INT bpi) { _bits_per_index = bpi; }
  
  INT Elems_Per_Way (void) const { return _elems_per_way; }
  void Set_Elems_Per_Way (INT epw) { _elems_per_way = epw; }

  INT Sel_Imm_Size (void) const { return (Vector_Length() * 
                                          Bits_Per_Index() *
                                          Elems_Per_Way()); }
  INT Dsel_Imm_Size (void) const { return 2 * Sel_Imm_Size(); }
  
  INT64 Sel_Array_To_Imm (const INT *sel);
  INT64 Sel_To_Dsel (INT64 sel_1, INT64 sel_0);
  void Dsel_To_Sel (INT64 dsel, INT64 &sel_1, INT64 &sel_0);
  
  INT64 Sel_73625140 (void);
  INT64 Sel_75316420 (void);
  
  INT64 Sel_5140 (void);
  INT64 Sel_7362 (void);
  
  INT64 Sel_6420 (void);
  INT64 Sel_7531 (void);
  
  INT64 Sel_4321 (INT offset =1);
  
  INT64 Sel_1032 (INT dist);
  
  INT64 Dsel_73625140 (void);
  INT64 Dsel_75316420 (void);
  
  void Print (FILE *f, INT tab =0);
  void Print_Debug (INT64 imm) const;
};

#endif /* __SIMD_SELECT__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:

