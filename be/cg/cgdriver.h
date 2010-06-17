
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
 * Module: cgdriver.h
 * $Revision: 1.9 $
 * $Date: 2000/04/06 01:46:04 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cgdriver.h,v $
 *
 * Revision history:
 *  20-Feb-95 - Original Version
 *
 * Description:
 *  
 *  Declaration for exported cg interfaces.
 *
 *  void CG_Configure_Opt_Level( INT opt_level )
 *
 *      Call this to set/change the CG optimization level.  This is the only
 *      valid way to do this.  Directly setting CG_opt_level probably won't do
 *      what you want.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cgdriver_INCLUDED
#define cgdriver_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Process command line once for cg-specific options */
extern void CG_Process_Command_Line (INT, char **, INT, char **);

extern void CG_Configure_Opt_Level( INT opt_level );

extern void CG_Init (void);	/* init once per compilation */

extern void CG_Fini (void);	/* finalize once per compilation */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* cgdriver_INCLUDED */
