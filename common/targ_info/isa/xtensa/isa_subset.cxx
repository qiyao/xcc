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
//  Generate ISA subset desdriptions
/////////////////////////////////////
// The instructions are listed by category. The different categories of
// instructions are:
//
//   1. xtensa instructions
//
// This is a temporary hack.  We hope for a more useful implementation soon.
//
/////////////////////////////////////



#include <stddef.h>
#include "topcode.h"
#include "isa_subset_gen.h"

main()
{
    ISA_Subset_Begin("xtensa");
    ISA_SUBSET xtensa = ISA_Subset_Create(0,"xtensa");

    for (int i = 0; i < TOP_count; ++i)
        Instruction(xtensa,static_cast<TOP>(i));
    ISA_Subset_End();
    return 0;
}


        
