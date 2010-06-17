
/* 
   Copyright (C) 2001-2007 Tensilica, Inc.  All Rights Reserved.
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


/* =======================================================================
 * =======================================================================
 *
 *  Module: ebo.h
 *  $Revision: 1.9 $
 *  $Date: 2000/04/14 16:58:42 $
 *  $Author: dew $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/ebo.h,v $
 *
 *  Revision comments:
 *
 *  29-May-1998 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Extended Block Optimizer module.
 *
 *  Quick summary of what this module  provides:
 *	- recognize an extended block sequence.
 *	- perform simple peephole types of optimizations on the
 *	  instructions in the sequence.
 *  general optimizations:
 *	- forward propagation of constants
 *	- redundant expression elimination
 *	- dead expression elimination
 *
 *  Interface:
 *
 *	void EBO_Init()
 *	  Initialization routine that should be called at the start
 *	  of each invocation of CG.
 *
 *	void EBO_Pre_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform prior to scheduling the region specified
 *	  by <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_before_unrolling(BB_SET *bbs)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Requires: all OPs in the region have omega information available.
 *	  Analyze and transform before unrolling and piplining a region
 *	  that is specified by the block lists that are provided.  A single
 *	  entry block is assumed; the exit block list is intended to be a
 *	  list of the exit TARGET blocks from the region.  The exit list
 *	  of blocks will NOT be processed.
 *
 *	void EBO_after_unrolling(BB_SET *bbs)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform during unrolling and piplining a region
 *	  that is specified by the block lists that are provided.  A single
 *	  entry block is assumed; the exit block list is intended to be a
 *	  list of the exit TARGET blocks from the region.  The exit list
 *	  of blocks will NOT be processed.
 *
 *	void EBO_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform for scheduling the region specified by
 *	  <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_CGPREP_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Analyze and transform for scheduling the region specified by
 *	  <rid>, or the whole PU if <rid> is NULL. Apply some optimizations
 *        after CGPREP but before the first scheduling pass.
 *
 *	void EBO_Post_Process_Region(RID *rid)
 *	  Requires: GRA liveness info for the region/PU is up-to-date.
 *	  Apply peephole optimizations after all register allocation
 *	  and before the last scheduling pass on the region specified
 *	  by <rid>, or the whole PU if <rid> is NULL.
 *
 *	void EBO_Finalize()
 *	  Termination routine that should be called at the end
 *	  of each invocation of CG.
 *
 *      INT32 EBO_Opt_Level
 *        This flag is used to control use of the EBO routines until
 *        final debugging can be completed.
 *
 * =======================================================================
 * =======================================================================
 */

#ifndef EBO_INCLUDED
#define EBO_INCLUDED


void EBO_Init(void);

void EBO_Pre_Process_Region(RID *rid);

void EBO_before_unrolling(BB_SET *bbs);

void EBO_after_unrolling(BB_SET *bbs);

void EBO_Process_Region(RID *rid);

void EBO_ACGPREP_Process_Region(RID *rid);

void EBO_Post_Process_Region(RID *rid);

bool EBO_Post_Process_BB(BB *bb);

void EBO_Finalize(void);

extern INT32 EBO_Opt_Level_Default;
extern INT32 EBO_Opt_Level;
extern INT32 EBO_Copy_Prop;
extern INT32 EBO_Prop_Backward;
extern INT32 EBO_Prop_BB_Loop;
extern INT32 EBO_Copy_Prop_Intra;
extern INT32 EBO_Reg_Copy_Prop;
extern INT32 EBO_Reg_Prop_SwpRegion;
extern INT32 EBO_Reg_Prop_Backward;
extern INT32 EBO_Select_Br;
extern INT32 EBO_Remove_Store;

#endif /* EBO_INCLUDED */
