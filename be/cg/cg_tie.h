
//
// CG TIE intrinsic support
//


/*

  Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.

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

// $Id: $

//

#ifndef _CG_TIE_H_
#define _CG_TIE_H_

#include <stdio.h>
#include "tietypes.h"
#include "op.h"
#include "tn.h"
#include "tie.h"


extern int expand_tie_macro_to_ops(
                     OPS* const ops,
                     const char* macro_name,
                     TN* const in_operands[]);
extern int expand_tie_macro_to_ops(
                     OPS* const ops,
                     const TIE_MACRO_p tie_macro,
                     TN* const in_operands[]);
extern int expand_tie_macro_to_ops(
		     OPS* const ops,
                     const TIE_MACRO_ID macro_id,
                     TN* const in_operands[]);

extern TN* Tie_Move_To_Result( TN *result, TN *val, OPS *ops );
extern TIE_LITCLASS_LIST *tie_offset_litclass (const TIE_MACRO_p macro, const UINT opnd_idx);

void convert_tie_branches(WN* pu);

#endif // _CG_TIE_H_

