
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

/* =======================================================================
 * =======================================================================
 *
 *  Module: cg_autotie
 *
 *  Description:
 *  ============
 *
 *  Auto TIE analysis routines.
 *
 * =======================================================================
 * ======================================================================= */

#ifndef CG_AUTOTIE_INCLUDED
#define CG_AUTOTIE_INCLUDED


#include "bb.h"
#include "findloops.h"

/* Autotie analysis is initialized and finalized once per PU. */
extern void AUTOTIE_Initialize (void);
extern void AUTOTIE_Finalize (void);

/* If 'bb_from' has an associated DFG, associate 'bb_to' with the same DFG. */
extern void AUTOTIE_Copy_BB_DFG (BB *bb_from, BB *bb_to);
extern void AUTOTIE_Move_BB_DFG (BB *bb_from, BB *bb_to);

/* Collect the BBs from the current PU along with their corresponding
   autotie DFGs. */
extern void AUTOTIE_Build_DFGs (void);

/* Convert fusion DFGs of all BBs to autotie DFGs */
extern void AUTOTIE_Convert_DFGs (void);

/* Attach the execution counts to the existing autotie regions. */
extern void AUTOTIE_Set_Region_Exec_Counts (void);

/* Analyze blocks inside and outside of 'loops' by creating new or
   updating the existing autotie regions.  Initialize the AT_STATs'
   DFGs with the ones collected by AUTOTIE_Build_DFGs. */
extern void AUTOTIE_Analyze (LOOP_DESCR *loops);

/* Update the AT_STATs to account for any spill code added by the compiler. */
extern void AUTOTIE_Analyze_Spills (void);

/* Update the AT_STATs with the taken branch probabilities for BBs
   that end in a conditional branch. */
extern void AUTOTIE_Analyze_Taken_Probs (void);

/* Return true if autotie doesn't want loop unrolled completely. */
extern BOOL AUTOTIE_Suppress_Unroll_Fully (LOOPINFO *info);


#endif /* CG_AUTOTIE_INCLUDED */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
