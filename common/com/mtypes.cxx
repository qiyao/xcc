
/* 
   Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: mtypes.c
 * $Revision: 1.26 $
 * $Date: 2000/04/24 14:24:29 $
 * $Author: lesniak $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/mtypes.cxx,v $
 *
 * Revision history:
 *
 * Description:
 *
 * Define IDs for the types supported by the target machine.  Not all
 * of the predefined types will be supported on a given machine; the
 * type attributes of those supported are defined in tdt.awk.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "errors.h"
#include "mtypes.h"
#include "trace.h"
#include "cxx_memory.h"
#include "libti.h"

static bool mtype_initialized = false;
TYPE_ID Mtype_Last=0;

UINT8 Num_Built_In=0;

TYPE_DESC Machine_Types[MTYPE_MAX];

static TYPE_ID Machine_Next_Alignment[MTYPE_MAX];

static TYPE_ID Machine_Prev_Alignment[MTYPE_MAX];

MTYPE_MASK Machine_Types_Available = 0x1fdffe;


/* ====================================================================
 *
 * Mtype_Name
 *
 * Return a string containing a printable name for an MTYPE.
 *
 * ====================================================================
 */

const char *
Mtype_Name (TYPE_ID b)
{
  static char buf[32];

  if ( b>0 && b<=MTYPE_CORE_LAST ) {
    return MTYPE_name(b);
  } else {
    sprintf (buf, "BETYPE_%1d", b);
    return buf;
  }
}

/* ====================================================================
 *
 * Mtype_AlignmentClass
 *
 * Return MTYPE corresponding to alignment(in bytes) and class
 *
 *	TODO -- this really belongs a matrix (align X class)
 * ====================================================================
 */
TYPE_ID Mtype_AlignmentClass(INT32 align, mUINT8 type_class)
{
  INT32	i;

  for(i=0; i<MTYPE_CORE_LAST; i++)
  {
    if ((MTYPE_type_class(i) == type_class) &&
	(MTYPE_align_min(i) == align))
      return MTYPE_id(i);
  }

  return MTYPE_UNKNOWN; 
}


/* ====================================================================
 *
 * Mtype_Promote_to_A4A8
 *
 * Convert I4 or U4 to A4, and I8 or U8 to A8; otherwise, do nothing.
 *
 * ====================================================================
 */
TYPE_ID Mtype_Promote_to_A4A8(TYPE_ID x)
{
  FmtAssert(!MTYPE_is_tie(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  FmtAssert(!MTYPE_is_xtbool(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  if (! MTYPE_is_integral(x))
    return x;
  if (MTYPE_byte_size(x) < 4)
    return x;
  if (MTYPE_byte_size(x) == 4)
    return MTYPE_A4;
  return MTYPE_A8;
}

/* ====================================================================
 *
 * Mtype_TransferSign
 *
 * Return signed/unsigned version of y depending on sign of x.
 * If either type is A4 or A8, return the A[48] of y.
 *
 * ====================================================================
 */
TYPE_ID Mtype_TransferSign(TYPE_ID x, TYPE_ID y)
{
  FmtAssert(!MTYPE_is_tie(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  FmtAssert(!MTYPE_is_tie(y), ("TIE type %s encountered\n", MTYPE_name(y)));
  FmtAssert(!MTYPE_is_xtbool(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  FmtAssert(!MTYPE_is_xtbool(y), ("TIE type %s encountered\n", MTYPE_name(y)));
  if (y == MTYPE_A4 || y == MTYPE_A8)
    return y;
  if (x == MTYPE_A4 || x == MTYPE_A8)
    return Mtype_Promote_to_A4A8(y);
  if (MTYPE_signed(x) ^ MTYPE_signed(y))
  {
    return MTYPE_complement(y);
  }
  return y;
}

/* ====================================================================
 *
 * Mtype_TransferSize
 *
 * Return the mtype version of y taking on the size of x.
 * If y is A4 or A8 and x's size is smaller than 4 bytes, return U1 or U2.
 *
 * ====================================================================
 */
TYPE_ID Mtype_TransferSize(TYPE_ID x, TYPE_ID y)
{
  FmtAssert(!MTYPE_is_tie(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  FmtAssert(!MTYPE_is_tie(y), ("TIE type %s encountered\n", MTYPE_name(y)));
  FmtAssert(!MTYPE_is_xtbool(x), ("TIE type %s encountered\n", MTYPE_name(x)));
  FmtAssert(!MTYPE_is_xtbool(y), ("TIE type %s encountered\n", MTYPE_name(y)));
  if (y == MTYPE_A4 || y == MTYPE_A8) {
    switch (MTYPE_byte_size(x)) {
    case 1: return MTYPE_U1;
    case 2: return MTYPE_U2;
    case 4: return MTYPE_A4;
    case 8: return MTYPE_A8;
    }
  }
  switch (MTYPE_byte_size(x)) {
  case 1: return MTYPE_signed(y) ? MTYPE_I1 : MTYPE_U1;
  case 2: return MTYPE_signed(y) ? MTYPE_I2 : MTYPE_U2;
  case 4: return MTYPE_signed(y) ? MTYPE_I4 : MTYPE_U4;
  case 8: return MTYPE_signed(y) ? MTYPE_I8 : MTYPE_U8;
  }
  return MTYPE_UNKNOWN;
}

/* ====================================================================
 *
 * Mtype_complex_to_real
 *
 * Return real type corresponding to complex
 *
 * ====================================================================
 */
TYPE_ID Mtype_complex_to_real(TYPE_ID type)
{
  FmtAssert(!MTYPE_is_tie(type), ("TIE type %s encountered\n", MTYPE_name(type)));
  FmtAssert(!MTYPE_is_xtbool(type), ("TIE type %s encountered\n", MTYPE_name(type)));
  if (MTYPE_is_complex(type))
  {
    switch(type) {
    case MTYPE_C4:
	return MTYPE_F4;
    case MTYPE_C8:
	return MTYPE_F8;
    case MTYPE_CQ:
	return MTYPE_FQ;
    }
  }
  return type;
}




/* ====================================================================
 *
 * TYPE_ID  MTYPE_comparison(TYPE_ID)
 *
 * Return a canonicalized type for a comparison
 *
 * ====================================================================
 */
TYPE_ID  Mtype_comparison(TYPE_ID type)
{
  switch(type)
  {
  case MTYPE_I1:
  case MTYPE_I2:
    return MTYPE_I4;
  case MTYPE_U1:
  case MTYPE_U2:
    return MTYPE_U4;
  default:
    return type;
  }
}




/* ====================================================================
 *
 * TYPE_ID Mtype_next_alignment(TYPE_ID)
 *
 * Return the next best alignment type (or MTYPE_UNKNOWN)
 * This is used to iterate thru types to improve alignment
 *
 * ====================================================================
 */
TYPE_ID Mtype_next_alignment(TYPE_ID type)
{
  return Machine_Next_Alignment[type];
}




/* ====================================================================
 *
 * TYPE_ID Mtype_prev_alignment(TYPE_ID)
 *
 * Return the prevevious alignment (or MTYPE_UNKNOWN)
 *
 * ====================================================================
 */
TYPE_ID Mtype_prev_alignment(TYPE_ID type)
{
  return Machine_Prev_Alignment[type];
}

/* ====================================================================
 *
 * void Mtype_add()
 *
 * add an entry in Machine_Types[]
 *
 * ====================================================================
 */
void Mtype_add(
  mCLASS_INDEX  id,
  mUINT16       bit_size,
  mUINT16       byte_size,
  mUINT16       num_pregs,
  mUINT8        alignment,
  mUINT8        dummy2,
  mUINT8        dummy3,
  mBOOL         signed_type,
  mBOOL         float_type,
  mCLASS_INDEX  dummy4,
  const char    *name,
  mUINT8        type_class_bits,
  mUINT8        type_order,
  mCLASS_INDEX  complement,
  mCLASS_INDEX	next_alignment,
  mCLASS_INDEX	prev_alignment
)
{
  Machine_Types[id].id = id;
  Machine_Types[id].bit_size = bit_size;
  Machine_Types[id].byte_size = byte_size;
  Machine_Types[id].num_pregs = num_pregs;
  Machine_Types[id].alignment = alignment;
  Machine_Types[id].dummy2 = dummy2;
  Machine_Types[id].dummy3 = dummy3;
  Machine_Types[id].signed_type = signed_type;
  Machine_Types[id].float_type = float_type;
  Machine_Types[id].dummy4 = dummy4;
  Machine_Types[id].name = name;
  Machine_Types[id].type_class_bits = type_class_bits;
  Machine_Types[id].type_order = type_order;
  Machine_Types[id].complement = complement;
  Machine_Next_Alignment[id] = next_alignment;
  Machine_Prev_Alignment[id] = prev_alignment;
}

/* ====================================================================
 *
 * void Mtype_initialize()
 *
 * Initialize Machine_Types[]
 *
 * ====================================================================
 */
void Mtype_initialize() {

  ENTER(1, "Mtype_init");

  if (mtype_initialized)
    RETURNV;

  mtype_initialized=true;

  memset(Machine_Types, 0, MTYPE_MAX * sizeof(TYPE_DESC));
  memset(Machine_Next_Alignment, 0, MTYPE_MAX * sizeof(TYPE_ID));
  memset(Machine_Prev_Alignment, 0, MTYPE_MAX * sizeof(TYPE_ID));

  Mtype_add(MTYPE_UNKNOWN,0,0,0,0,0,0,0,0,0,"",
			0,0,
			MTYPE_UNKNOWN,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_B,1,0,1,0,0,0,0,0,0,"B",
			MTYPE_CLASS_INTEGER,0,
			MTYPE_B,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_I1,8,8,1,1,1,1,1,0,0,"I1",
			MTYPE_CLASS_INTEGER,1,
			MTYPE_U1,MTYPE_I2,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_I2,16,16,1,2,2,2,1,0,0,"I2",
			MTYPE_CLASS_INTEGER,3,
			MTYPE_U2,MTYPE_I4,MTYPE_I1);
  Mtype_add(MTYPE_I4,32,32,1,4,4,4,1,0,0,"I4",
			MTYPE_CLASS_INTEGER,5,
			MTYPE_U4,MTYPE_I8,MTYPE_I2);
  Mtype_add(MTYPE_I8,64,64,1,8,8,8,1,0,0,"I8",
			MTYPE_CLASS_INTEGER,7,
			MTYPE_U8,MTYPE_UNKNOWN,MTYPE_I4);
  Mtype_add(MTYPE_U1,8,8,1,1,1,1,0,0,0,"U1",
			MTYPE_CLASS_UNSIGNED_INTEGER,2,
			MTYPE_I1,MTYPE_U2,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_U2,16,16,1,2,2,2,0,0,0,"U2",
			MTYPE_CLASS_UNSIGNED_INTEGER,4,
			MTYPE_I2,MTYPE_U4,MTYPE_U1);
  Mtype_add(MTYPE_U4,32,32,1,4,4,4,0,0,0,"U4",
			MTYPE_CLASS_UNSIGNED_INTEGER,6,
			MTYPE_I4,MTYPE_U8,MTYPE_U2);
  Mtype_add(MTYPE_U8,64,64,1,8,8,8,0,0,0,"U8",
			MTYPE_CLASS_UNSIGNED_INTEGER,8,
			MTYPE_I8,MTYPE_UNKNOWN,MTYPE_U4);
  Mtype_add(MTYPE_F4,32,32,1,4,4,4,1,1,0,"F4",
			MTYPE_CLASS_FLOAT,9,
			MTYPE_F4,MTYPE_F8,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_F8,64,64,1,8,8,8,1,1,0,"F8",
			MTYPE_CLASS_FLOAT,11,
			MTYPE_F8,MTYPE_UNKNOWN,MTYPE_F4);
  Mtype_add(MTYPE_F10,128,128,1,16,16,16,1,1,0,"F10",
			MTYPE_CLASS_FLOAT,13,
			MTYPE_F10,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_F16,128,128,1,16,16,16,1,1,0,"F16",
			MTYPE_CLASS_FLOAT,15,
			MTYPE_F16,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_STR,0,0,1,1,1,4,0,0,0,"STR",
			MTYPE_CLASS_STR,0,
			MTYPE_STR,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_FQ,128,128,1,16,16,16,1,1,0,"FQ",
			MTYPE_CLASS_FLOAT,14,
			MTYPE_FQ,MTYPE_UNKNOWN,MTYPE_F8);
  Mtype_add(MTYPE_M,0,0,1,0,0,0,0,0,0,"M",
			0,0,
			MTYPE_M,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_C4,64,64,1,4,4,4,0,1,0,"C4",
			MTYPE_CLASS_COMPLEX_FLOAT,10,
			MTYPE_C4,MTYPE_C8,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_C8,128,128,1,8,8,8,0,1,0,"C8",
			MTYPE_CLASS_COMPLEX_FLOAT,12,
			MTYPE_C8,MTYPE_CQ,MTYPE_C4);
  Mtype_add(MTYPE_CQ,256,256,1,16,16,16,0,1,0,"CQ",
			MTYPE_CLASS_COMPLEX_FLOAT,16,
			MTYPE_CQ,MTYPE_UNKNOWN,MTYPE_C8);
  Mtype_add(MTYPE_V,0,0,1,0,0,0,0,0,0,"V",
			0,0,
			MTYPE_V,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_BS,1,0,1,0,0,0,0,0,0,"BS",
			MTYPE_CLASS_INTEGER,0,
			MTYPE_BS,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_A4,32,32,1,4,4,4,0,0,0,"A4",
			MTYPE_CLASS_UNSIGNED_INTEGER,6,
			MTYPE_A4,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_A8,64,64,1,8,8,8,0,0,0,"A8",
			MTYPE_CLASS_UNSIGNED_INTEGER,8,
			MTYPE_A8,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_C10,256,256,1,16,16,16,0,1,0,"C10",
			MTYPE_CLASS_COMPLEX_FLOAT,16,
			MTYPE_C10,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_C16,256,256,1,16,16,16,0,1,0,"C16",
			MTYPE_CLASS_COMPLEX_FLOAT,16,
			MTYPE_C16,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_I16,256,256,1,16,16,16,0,0,0,"I16",
			MTYPE_CLASS_INTEGER,16,
			MTYPE_I16,MTYPE_UNKNOWN,MTYPE_UNKNOWN);
  Mtype_add(MTYPE_U16,256,256,1,16,16,16,0,0,0,"U16",
			MTYPE_CLASS_UNSIGNED_INTEGER,16,
			MTYPE_U16,MTYPE_UNKNOWN,MTYPE_UNKNOWN);

  Mtype_add(MTYPE_XTBOOL, 1, 8, 1, 1, 1, 1, 0, 0, 0, "_TIE_xtbool",
			MTYPE_CLASS_XTBOOL, 0,
			MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
  Mtype_add(MTYPE_XTBOOL2, 2, 8, 1, 1, 1, 1, 0, 0, 0, "_TIE_xtbool2",
			MTYPE_CLASS_XTBOOL, 0,
			MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
  Mtype_add(MTYPE_XTBOOL4, 4, 8, 1, 1, 1, 1, 0, 0, 0, "_TIE_xtbool4",
			MTYPE_CLASS_XTBOOL, 0,
			MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
  Mtype_add(MTYPE_XTBOOL8, 8, 8, 1, 1, 1, 1, 0, 0, 0, "_TIE_xtbool8",
			MTYPE_CLASS_XTBOOL, 0,
			MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);
  Mtype_add(MTYPE_XTBOOL16, 16, 16, 1, 2, 2, 2, 0, 0, 0, "_TIE_xtbool16",
			MTYPE_CLASS_XTBOOL, 0,
			MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);

  Is_True(MTYPE_XTBOOL16 == MTYPE_CORE_LAST,("Core mtypes out of sync"));
  Mtype_Last = MTYPE_CORE_LAST;

  RETURNV;
}

