
/* 
   Copyright (C) 2001-2006 Tensilica, Inc.  All Rights Reserved.
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
 * Module: be_util.c
 * $Revision: 1.8 $
 * $Date: 2000/04/06 01:54:31 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/com/RCS/be_util.cxx,v $
 *
 * Revision history:
 *  06-Dec -95 - Original Version
 *
 * Description:
 *    Utilities for all backend components.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdio.h>

#include "defs.h"
#include "wn.h"
#include "be_util.h"

static INT32 current_pu_count;

void
Reset_Current_PU_Count(void)
{
  current_pu_count = 0;
}

void
Advance_Current_PU_Count(void)
{
  current_pu_count++;
}

INT32
Current_PU_Count(void)
{
  return current_pu_count;
}


// return preg for corresponding ST via dreg table
struct find_dreg_preg {
	ST_IDX st;
	find_dreg_preg (const ST *s) : st (ST_st_idx (s)) {}

	BOOL operator () (UINT, const ST_ATTR *st_attr) const {
	    return (ST_ATTR_kind (*st_attr) == ST_ATTR_DEDICATED_REGISTER &&
		    ST_ATTR_st_idx (*st_attr) == st);
    	}
};
PREG_NUM
Find_PREG_For_Symbol (const ST *st)
{
    ST_IDX idx = ST_st_idx (st);
    ST_ATTR_IDX d;

    d = For_all_until (St_Attr_Table, ST_IDX_level (idx),
                          find_dreg_preg(st));
    return ST_ATTR_reg_id(St_Attr_Table(ST_IDX_level (idx), d));
} 


//-----------------------------------------------------------------------
// NAME: St_Idx_Is_Intconst
// FUNCTION: Returns TRUE if 'st_idx' is an integer constant and, when this
//   is the case, sets the value in 'val'.  Otherwise, returns FALSE.
//-----------------------------------------------------------------------

extern BOOL St_Idx_Is_Intconst(ST_IDX st_idx, INT64 *val)
{
  ST* st = &St_Table[st_idx]; 
  if (ST_class(st)==CLASS_CONST) {
    TCON t = STC_val(st);
    switch(TCON_ty(t)) {
      case MTYPE_I1: case MTYPE_I2 : case MTYPE_I4: case MTYPE_I8 :
        *val = t.vals.i0;
        return TRUE;
      case MTYPE_U1: case MTYPE_U2 : case MTYPE_U4: case MTYPE_U8 :
        *val = t.vals.k0;
        return TRUE;
      default:
        return FALSE;
    }
  } else {
    return FALSE;
  }
}

//-----------------------------------------------------------------------
// NAME: Wn_Is_Intconst
// FUNCTION: Returns TRUE if 'ldid' is an integer constant and, when this
//   is the case, sets the value in 'val'.  Otherwise, returns FALSE.
//-----------------------------------------------------------------------

extern BOOL Wn_Is_Intconst(WN *ldid, INT64 *val)
{
  return St_Idx_Is_Intconst(ST_st_idx(WN_st(ldid)), val);
} 

/*
  Advance_CU_Count()/Current_CU_Count is for debugging purpose.

  They are used for skipping optimization of specific Compilation
  Units (CU). It is often used in the situation where an application
  has many source files (and often in many directories). To narrow
  down which source file(s) are generated incorrectly, the compiler
  can turn off optimizations in a binary search way until the guilt
  file(s) are found. For example, suppose an application has 128
  files. Let's number them from 0 to 127. We know the code generated
  by compiler fails when all those 128 files are optimized.

     First, turn off optimization for half of all the files, say
     file 64 to 127. If the application still fail, the guilty files
     are in 0 - 63; otherwise, the guilty files are in files 64 - 127.
     We keep doing the same thing on the remaining guilty files until
     we cannot narrow down further.

  Since one process of BE is created for each CU. To number those CUs,
  a global counter is needed and it has to be outside of the compiler.
  Here, we use a file as the counter. Reseting the counter must
  be done before debugging.

  Typical usage:

    rm -f <counter_file>
    make CC_FLAGS="-O3 ... -Wb,-fc:<counter_file> -CG:cu_skip_after=<n> "
 
  where 'rm -f <counter_file>' will reset the counter; -fc:<counter_file>
  tells compiler to enable CU skipping and use <counter_file> as a global
  counter.  -CG:cu_skip_after=<n> will skip CG optimization for files 
  whose number is greater than <n>.    
  
  Besides cu_skip_after, there are also cu_skip_before and cu_skip_equal.
  Those CU skip supoptions are similar to PU skip_after/skip_before/skip_equal.
 */

static char *cu_skip_file_name=NULL;
void Set_CU_Skip_File_Name(char *p)
{
  cu_skip_file_name = p;
}

BOOL DO_CU_Skip()
{
  return (cu_skip_file_name != NULL);
}

static INT be_cu_number=-1;
INT Current_CU_Count ()
{ 
  return be_cu_number;
}

void Advance_CU_Count()
{
  static BOOL CU_advanced = FALSE;

  if ( (cu_skip_file_name == NULL) || CU_advanced ) return;

  // for each invocation of BE, this will be invoked only once
  CU_advanced = TRUE;

  FILE *fp=fopen(cu_skip_file_name, "r");
  if (fp != NULL) {
    fscanf(fp, "%d", &be_cu_number);
    fclose(fp);
  }
  be_cu_number++;  // zero if cu_skip_file_name does not exist
  
  /* Now, create or truncate the file */
  fp=fopen(cu_skip_file_name, "w");
  if (fp != NULL) {
     fprintf(fp, "%d", be_cu_number);
     fclose(fp);
     return;   // successful return
  }

  /* Any unusual thing happens, be_cu_number remains -1 */
  be_cu_number = -1;
}

