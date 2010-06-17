
/* 
   Copyright (C) 2004-2005 Tensilica, Inc.  All Rights Reserved.
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


#ifndef ercg_INCLUDED
#define ercg_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: ercg.h
 * $Revision: 3.14 $
 * $Date: 2000/04/06 01:47:32 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/ercg.h,v $
 *
 * Revision history:
 *  02-Nov-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *
 * Description:
 *
 * Define the Muse code generator error codes for use with the error
 * message handler errors.c.  The associated error descriptors may be
 * found in the file ercg.desc.
 *
 * ====================================================================
 * ====================================================================
 */


#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_CG	EP_CG*1000


/* Scheduling Preparation: */
#define EC_Ill_Cycle	EC_BASE_CG+1	/* int, str */

/* Register Allocation: */
#define EC_Ill_Reg_Spill1 EC_BASE_CG+2	/* string(register-name) */
#define EC_Ill_Reg_Spill2b EC_BASE_CG+3 /* int (suggested -O level) */
#define EC_Not_Enough_Allocatable EC_BASE_CG+4 /* string(reg_class_name) */
#define EC_Unallocatable_Register_File EC_BASE_CG+5 /* string(reg_class_name) */
#define EC_Reference_Changed	EC_BASE_CG+6		/* str, str */

/* Start all target-specific codes here: */
#define EC_TARGET	EC_BASE_CG+200
#define EC_CG_no_valid_format	EC_BASE_CG+201

#endif /* ercg_INCLUDED */
