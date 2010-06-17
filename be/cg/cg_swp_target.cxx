
/* 
   Copyright (C) 2002-2006 Tensilica, Inc.  All Rights Reserved.
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
 *  Module: cg_swp_target.cxx
 *  $Revision: 1.56 $
 *  $Date: 2000/08/03 15:44:34 $
 *  $Author: dew $
 *  $Source: /osprey.src/osprey1.0/be/cg/ia64/RCS/cg_swp_target.cxx,v $
 *
 * =======================================================================
 * ======================================================================= */

#define USE_STANDARD_TYPES
#include "libti.h"
#include "tn.h"
#include "op.h"

#ifdef TARG_XTENSA
/* ====================================================================
 *
 *  Base_update_tn
 *
 * ====================================================================
 */

extern
TN* Base_update_tn(OP *op) {

  TOP top = OP_code(op);
  TN* base_tn = NULL;

  if (OP_memory(op) && TI_ISA_Property_Set(PROP_base_update, top)) {
    INT base_idx = TI_TOP_Find_Operand_Use(top, OU_base);
    base_tn = (base_idx >= 0) ? OP_opnd(op,base_idx) : NULL;
  }
  return base_tn;
}

#endif


