
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
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
 * Module: lnodriver.h
 * $Revision: 1.10 $
 * $Date: 2000/04/06 02:09:21 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/lno/RCS/lnodriver.h,v $
 *
 * Description:
 *  exported functions from lno.so
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef lnodriver_INCLUDED
#define lnodriver_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

class IPA_LNO_READ_FILE;
extern IPA_LNO_READ_FILE* IPA_LNO_File;

extern void lno_main (INT argc, char **argv, INT, char **);

extern void Lno_Init (void);

extern void Lno_Fini (void);

extern WN *Perform_Loop_Nest_Optimization (PU_Info*, WN *, WN *, BOOL);

#ifdef __cplusplus
}
#endif
#endif /* lnodriver_INCLUDED */
