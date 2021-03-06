
/*

  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.

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

//  
//  Generate ISA hazard information
//
//  Note:  A "hazard" is a conflict between instructions which may
//  cause unexpected behavior if the instructions appear at a forbidden
//  distance in the instruction stream.  That is, the hardware does not
//  enforce the required separation, and the software is responsible
//  for avoiding such situations.  Most are avoided via the resource
//  mechanism.  This file handles special cases.
//
///////////////////////////////////////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "targ_isa_subset.h"
#include "isa_hazards_gen.h"


main()
{
  ISA_HAZARD
    result,
    operand,
    errata;

  ISA_Hazards_Begin("xtensa");

  result = Hazard_Create("result");
  operand = Hazard_Create("operand");
  errata = Hazard_Create("errata");

  ISA_Hazards_End();
  return 0;
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
