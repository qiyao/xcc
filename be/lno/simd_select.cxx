// simd_select.cxx
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 * SIMD vectorization module:                                                *
 *                                                                           *
 *    Automatic SIMD                                                         *
 *                                                                           *
 *    Functions for vector selection                                         *
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

// $Id: simd_select.cxx $

#include <stdio.h>

#include "simd_select.h"


#define SIMD_SELECT_BUFFER_SIZE 256
static INT sel_buf[SIMD_SELECT_BUFFER_SIZE];


SIMD_SELECT::SIMD_SELECT (INT vector_length,
			  INT bits_per_index,
			  INT elems_per_way) :
  _overflow(false),
  _vector_length(vector_length),
  _bits_per_index(bits_per_index),
  _elems_per_way(elems_per_way)
{ }


INT64
SIMD_SELECT::Sel_Array_To_Imm (const INT *sel)
{
  _overflow = false;
  
  if (Sel_Imm_Size() > 64) {
    _overflow = true;
    return 0;
  }
  
  INT64 sel_imm = 0;
  
  INT vl = Vector_Length();
  INT sel_bits = Bits_Per_Index();
  INT elems = Elems_Per_Way();

  if (Target_Byte_Sex == LITTLE_ENDIAN) {
    for (INT i = 0; i < vl; i++) {
      for (INT j = 0; j < elems; j++) {
        sel_imm |=
          ( ((UINT64)(elems * sel[i] + j)) << 
            (sel_bits * (elems * i + j)) );
      }
    }
  } else {
    for (INT i = 0; i < vl; i++) {
      for (INT j = 0; j < elems; j++) {
        sel_imm |=
          ( ((UINT64)(elems * sel[i] + j)) << 
            (sel_bits * (elems * vl - 1 - (elems * i + j))) );
      }
    }
  }
  
  Print_Debug(sel_imm);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_To_Dsel (INT64 sel_1, INT64 sel_0)
{
  _overflow = false;

  if (Dsel_Imm_Size() > 64) {
    _overflow = true;
    return 0;
  }
    
  INT sel_imm_size = Sel_Imm_Size();
  INT64 dsel_imm = 0;
  if (Target_Byte_Sex == LITTLE_ENDIAN) {
    dsel_imm = sel_0 | (sel_1 << sel_imm_size);
  } else {
    dsel_imm = sel_1 | (sel_0 << sel_imm_size);
  }
    
  Print_Debug(dsel_imm);
  return dsel_imm;
}


void
SIMD_SELECT::Dsel_To_Sel (INT64 dsel, INT64 &sel_1, INT64 &sel_0)
{
  _overflow = false;

  if (Dsel_Imm_Size() > 64) {
    _overflow = true;
    sel_0 = 0;
    sel_1 = 0;
    return;
  }
    
  INT sel_imm_size = Sel_Imm_Size();
  INT64 sel_lo = (INT64)(((UINT64)dsel) & ((1ULL << sel_imm_size) - 1));
  INT64 sel_hi = (INT64)((UINT64)dsel >> sel_imm_size);
    
  if (Target_Byte_Sex == LITTLE_ENDIAN) {
    sel_0 = sel_lo;
    sel_1 = sel_hi;
  } else {
    sel_0 = sel_hi;
    sel_1 = sel_lo;
  }
}


INT64
SIMD_SELECT::Sel_75316420 (void)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
  Is_True(vl % 2 == 0, ("Odd vector length (%d)", vl));
    
  for (INT i = 0; i < vl / 2; i++)
  {
    sel_buf[i] = 2 * i;
    sel_buf[vl / 2 + i] = 2 * i + 1;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_73625140 (void)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
  Is_True(vl % 2 == 0, ("Odd vector length (%d)", vl));
    
  for (INT i = 0; i < vl / 2; i++)
  {
    sel_buf[2 * i] = i;
    sel_buf[2 * i + 1] = vl / 2 + i;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_5140 (void)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
  Is_True(vl % 2 == 0, ("Odd vector length (%d)", vl));
    
  for (INT i = 0; i < vl / 2; i++)
  {
    sel_buf[2 * i] = i;
    sel_buf[2 * i + 1] = vl + i;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_7362 (void)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
  Is_True(vl % 2 == 0, ("Odd vector length (%d)", vl));
    
  for (INT i = 0; i < vl / 2; i++)
  {
    sel_buf[2 * i] = vl / 2 + i;
    sel_buf[2 * i + 1] = vl + vl / 2 + i;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_6420 (void)
{ 
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
    
  for (INT i = 0; i < vl; i++)
  {
    sel_buf[i] = 2 * i;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_7531 (void)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
    
  for (INT i = 0; i < vl; i++)
  {
    sel_buf[i] = 2 * i + 1;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_4321 (INT offset)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));
  Is_True(offset >= 0 && offset <= vl,
          ("Bad offset %d, vector length %d", offset, vl));
    
  for (INT i = 0; i < vl; i++)
  {
    sel_buf[i] = i + offset;
  }
    
  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Sel_1032 (INT dist)
{
  INT vl = Vector_Length();
  Is_True(vl <= SIMD_SELECT_BUFFER_SIZE,
          ("SIMD_SELECT buffer overflow (needed size %d > %d)",
           vl, SIMD_SELECT_BUFFER_SIZE));

  Is_True(dist >= 1 && (dist <= vl / 2) && (vl % (2 * dist) == 0),
          ("Bad distance %d, vector length %d", dist, vl));
    
  for (INT i = 0; i < vl; i += 2 * dist)
  {
    for (INT j = 0; j < dist; j++)
    {
      sel_buf[i + j] = i + dist + j;
      sel_buf[i + dist + j] = i + j;
    }
  }

  INT64 sel_imm = Sel_Array_To_Imm(sel_buf);
  return sel_imm;
}


INT64
SIMD_SELECT::Dsel_73625140 (void)
{
  INT64 sel_imm_0 = Sel_5140();
  INT64 sel_imm_1 = Sel_7362();
    
  INT64 sel_imm = Sel_To_Dsel(sel_imm_1, sel_imm_0);

  return sel_imm;
}


INT64
SIMD_SELECT::Dsel_75316420 (void)
{
  INT64 sel_imm_0 = Sel_6420();
  INT64 sel_imm_1 = Sel_7531();

  INT64 sel_imm = Sel_To_Dsel(sel_imm_1, sel_imm_0);

  return sel_imm;
}


void
SIMD_SELECT::Print_Debug (INT64 imm) const
{
  if (simd_debug)
  {
    INT vl = Vector_Length();
    INT sel_bits = Bits_Per_Index();
    INT mask = (1 << sel_bits) - 1;
    INT elems = Elems_Per_Way();
    UINT64 uimm = (UINT64)imm;
	
    fprintf(TFile, "Select immediate: 0x%" LLX_FMT " (lo -> hi:", imm);
    while (uimm != 0)
    {
      for (INT i = 0; i < vl; i++)
      {
        for (INT j = 0; j < elems; j++)
        {
          INT idx = (INT)uimm & mask;
          fprintf(TFile, " %d", idx);
          uimm >>= sel_bits;
        }
      }
    }
    fprintf(TFile, ")\n");
  }
}


void
SIMD_SELECT::Print (FILE *f, INT tab)
{
  fprintf(f, "%*sSIMD_SELECT { vl: %d, idx_bits: %d, elems_per_way: %d }\n",
          tab, "", Vector_Length(), Bits_Per_Index(), Elems_Per_Way());
}



// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
