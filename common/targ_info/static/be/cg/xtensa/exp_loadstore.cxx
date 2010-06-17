
/* 
   Copyright (C) 2001-2005 Tensilica, Inc.  All Rights Reserved.
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


/* CGEXP routines for loads and stores */
#include <elf.h>
#include "defs.h"
#include "em_elf.h"
#include "erglob.h"
#include "ercg.h"
#include "tracing.h"
#include "config.h"
#include "config_targ_options.h"
#include "config_debug.h"
#include "xstats.h"
#include "tn.h"
#include "cg_flags.h"
#include "op.h"
#include "stblock.h"
#include "data_layout.h"
#include "strtab.h"
#include "stab.h"
#include "cg.h"
#include "cgexp.h"
#include "cgexp_internals.h"

#include "iselector.h"

void
Exp_Tree (TN *tgt_tn, WN *tree, OPS *ops)
{
  BOOL success = ISEL_gen(tree, tgt_tn, ops);
  FmtAssert( success, ("ISEL_gen failed in Exp_Tree"));
}

void
Exp_Lda (TYPE_ID mtype, TN *tgt_tn, ST *sym, INT64 ofst, OPERATOR call_opr, OPS *ops)
{
  WN *tree = WN_Lda(mtype, ofst, sym, 0);
  BOOL success = ISEL_gen(tree, tgt_tn, ops);
  FmtAssert( success, ("ISEL_gen failed in Exp_Lda"));
}

void
Exp_Load (TYPE_ID rtype, TYPE_ID desc, TN *tgt_tn, ST *sym,
	  INT64 ofst, OPS *ops, UINT16 variant)
{
  WN *tree = WN_CreateLdid(OPR_LDID, rtype, desc, ofst, sym, ST_type(sym), 0);
  BOOL success = ISEL_gen( tree, tgt_tn, ops );
  FmtAssert( success, ("ISEL_gen failed in Exp_Load (0)"));
}

void
Exp_Load (TN *tgt_tn, WN *tree, OPS *ops, UINT16 variant)
{
  BOOL success = ISEL_gen(tree, tgt_tn, ops);
  FmtAssert( success, ("ISEL_gen failed in Exp_Load (1)"));
}

void
Exp_Store (TYPE_ID mtype, TN *src_tn, ST *sym, INT64 ofst, OPS *ops, UINT16 variant)
{
  WN *tree = WN_Stid( mtype, ofst, sym, ST_type(sym), WN_CreateTn( src_tn ), 0 );
  BOOL success = ISEL_gen( tree, 0, ops );
  FmtAssert( success, ("ISEL_gen failed in Exp_Store"));
}


void 
Expand_Lda_Label (TN *dest, TN *lab, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Expand_Lda (TN *dest, TN *src, OPS *ops)
{ FmtAssert(FALSE, ("NYI: Expand_Lda")); }

void
Expand_Load (OPCODE opcode, TN *result, TN *base, TN *ofst, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Expand_Store (TYPE_ID mtype, TN *src, TN *base, TN *ofst, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Expand_Misaligned_Load ( OPCODE op, TN *result, TN *base, TN *disp, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Expand_Misaligned_Store (TYPE_ID mtype, TN *obj_tn, TN *base_tn, TN *disp_tn, INT16 variant, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void Exp_Prefetch (TOP opc, UINT32 pf_flags, TN* src1, TN* src2, OPS* ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Exp_Extract_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src_tn, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

void
Exp_Deposit_Bits (TYPE_ID rtype, TYPE_ID desc, UINT bit_offset, UINT bit_size,
		  TN *tgt_tn, TN *src1_tn, TN *src2_tn, OPS *ops)
{ FmtAssert(FALSE,("NOT YET IMPLEMENTED")); }

