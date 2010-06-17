
/*

  Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.

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

/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_fusion
 *
 *  Description:
 *  ============
 *
 *  This module provides routines for code optimization using
 *  opcode reference information.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef CG_FUSION_INCLUDED
#define CG_FUSION_INCLUDED


#include "bb.h"
#include "findloops.h"
#include "tf_defs.h"
#include "xt_mempool.h"


extern void CG_FUSION_Initialize (void);
extern void CG_FUSION_Finalize (void);


/* Build a data-flow graph for 'bb'. Return NULL if unsuccessful. Attach
   an (OP *) to each TF_NODE user data and a (TN *) to each TF_OPND user data. */
extern tf_dfg_t CG_FUSION_Build_DFG (XT_MEMPOOL *pool, BB *bb);

/* Optimize 'bb' or 'loop' using any available information from
   the opcode TIE reference. */
extern void CG_FUSION_Optimize_BB (BB *bb, bool aggressive=true);
extern void CG_FUSION_Optimize_Loop (LOOP_DESCR *loop);
extern void CG_FUSION_Optimize_Region (void);


#endif /* CG_FUSION_INCLUDED */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
