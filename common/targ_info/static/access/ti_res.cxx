
/*
   Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.
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

#include "defs.h"
#include "libti.h"
#include "ti_res.h"


/* ====================================================================
 *
 *  TI_RES_Can_Dual_Issue
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_Can_Dual_Issue(
  TOP    opcode1,
  TOP    opcode2
)
{
  INT min_length, i;
  TI_SI_RR rr1, rr2;

  /* Quick check to see if we could ever dual issue something.
   */
  if (!TI_PROC_Property_Set(PROP_has_same_cycle_branch_shadow))
    return FALSE;

  /* Resource requirements for each.
   */
  rr1 = TSI_Resource_Requirement(opcode1);
  rr2 = TSI_Resource_Requirement(opcode2);

  /* We only have to check up to the end of the shorter of the two resource
   * requests.
   */
  if ( SI_RR_Length(rr1) < SI_RR_Length(rr2) )
    min_length = SI_RR_Length(rr1);
  else
    min_length = SI_RR_Length(rr2);

  /* Check each cycle.
   */
  for ( i = 0; i < min_length; ++i ) {
    TI_SI_RRW rrw = SI_RRW_Initial();

    rrw = SI_RRW_Reserve(rrw,SI_RR_Cycle_RRW(rr1,i));
    rrw = SI_RRW_Reserve(rrw,SI_RR_Cycle_RRW(rr2,i));

    if ( SI_RRW_Has_Overuse(rrw) ) return FALSE;
  }

  return TRUE;
}
