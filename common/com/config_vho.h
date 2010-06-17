
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
 * Module: config_vho.h
 * $Revision: 1.6 $
 * $Date: 2000/04/06 02:24:10 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config_vho.h,v $
 *
 * Revision history:
 *  05-May-96 - Extracted from be/opt/opt_config.h.
 *
 * Description:
 *
 * Declare global flag variables for -VHO group options.
 * This file is included in common/com/config.c, but should not be
 * otherwise used outside of VHO.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_vho_INCLUDED
#define config_vho_INCLUDED

extern DLL_SHARED BOOL  VHO_Force_Lowering;
extern DLL_SHARED BOOL  VHO_Struct_Opt;
extern DLL_SHARED BOOL  VHO_Recycle_Pregs;
extern DLL_SHARED BOOL  VHO_Combine_Loads;
extern DLL_SHARED INT32 VHO_Switch_Density;
extern DLL_SHARED INT32 VHO_Switch_If_Else_Limit;
extern DLL_SHARED INT32 VHO_Switch_Compgoto_Limit;
extern DLL_SHARED BOOL  VHO_Switch_Opt;
extern DLL_SHARED BOOL  VHO_Cselect_Opt;
extern DLL_SHARED BOOL  VHO_Iload_Opt;
extern DLL_SHARED BOOL  VHO_Istore_Opt;
extern DLL_SHARED BOOL  VHO_Call_Opt;
extern DLL_SHARED BOOL  VHO_Check_Tree;
extern DLL_SHARED BOOL  VHO_Single_Loop_Test;
extern DLL_SHARED BOOL  VHO_Use_Do_While;
#endif /* config_vho_INCLUDED */

