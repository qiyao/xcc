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

//  
//  Generate ISA registers information
///////////////////////////////////////

#include <stddef.h>
#include "isa_registers_gen.h"
#include "targ_isa_subset.h"


static int ISA_Mask(ISA_SUBSET isa)
{
  return 1 << (int)isa;
}


static int All_ISA_Mask(void)
{
  int i;
  int mask = 0;
  for (i = ISA_SUBSET_MIN; i <= ISA_SUBSET_MAX; ++i) {
    mask |= 1 << i;
  }
  return mask;
}


static int Range_ISA_Mask(ISA_SUBSET min_isa, ISA_SUBSET max_isa)
{
  int i;
  int mask = 0;
  for (i = (int)min_isa; i <= (int)max_isa; ++i) {
    mask |= 1 << i;
  }
  return mask;
}


#define NELEMS(a) (sizeof(a) / sizeof(*(a)))

static const int mx_regs[] = { 0, 1 };
static const int my_regs[] = { 2, 3 };
static const int lbeg_regs[] = { 0 };
static const int lend_regs[] = { 1 };
static const int lcount_regs[] = { 2 };
static const int sar_regs[] = { 3 };
static const int breg_regs[] = { 4 };
static const int scompare1_regs[] = { 12 };
static const int acclo_regs[] = { 16 };
static const int acchi_regs[] = { 17 };
static const int b1_regs[] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 };
static const int b2_regs[] = { 0, 2, 4, 6, 8, 10, 12, 14 };
static const int b4_regs[] = { 0, 4, 8, 12 };
static const int b8_regs[] = { 0, 8 };
static const int b16_regs[] = { 0 };

main (int argc, char** argv)
{
  ISA_Registers_Begin("xtensa");

  ISA_REGCLASS ar = ISA_Register_Class_Create("integer", 32, true, false);
  ISA_REGCLASS mr = ISA_Register_Class_Create("macc", 32, true, false);
  ISA_REGCLASS br = ISA_Register_Class_Create("branch", 1, true, false);
  ISA_REGCLASS sr = ISA_Register_Class_Create("special", 32, true, false);

  ISA_Register_Set(ar, 0, 15, "a%u", NULL, All_ISA_Mask());
  ISA_Register_Set(mr, 0, 3, "m%u", NULL, All_ISA_Mask());
  ISA_Register_Set(br, 0, 15, "b%u", NULL, All_ISA_Mask());
  ISA_Register_Set(sr, 0, 31, "%u", NULL, All_ISA_Mask());

  ISA_Register_Subclass_Create("macc_x", mr, NELEMS(mx_regs), mx_regs, NULL);
  ISA_Register_Subclass_Create("macc_y", mr, NELEMS(my_regs), my_regs, NULL);

  ISA_Register_Subclass_Create("branch_1", br, NELEMS(b1_regs), b1_regs, NULL);
  ISA_Register_Subclass_Create("branch_2", br, NELEMS(b2_regs), b2_regs, NULL);
  ISA_Register_Subclass_Create("branch_4", br, NELEMS(b4_regs), b4_regs, NULL);
  ISA_Register_Subclass_Create("branch_8", br, NELEMS(b8_regs), b8_regs, NULL);
  ISA_Register_Subclass_Create("branch_16", br, NELEMS(b16_regs), b16_regs, NULL);

  ISA_Register_Subclass_Create("LBEG", sr, NELEMS(lbeg_regs), lbeg_regs, NULL);
  ISA_Register_Subclass_Create("LEND", sr, NELEMS(lend_regs), lend_regs, NULL);
  ISA_Register_Subclass_Create("lcount", sr, NELEMS(lcount_regs), lcount_regs, NULL);
  ISA_Register_Subclass_Create("SAR", sr, NELEMS(sar_regs), sar_regs, NULL);
  ISA_Register_Subclass_Create("breg", sr, NELEMS(breg_regs), breg_regs, NULL);
  ISA_Register_Subclass_Create("SCOMPARE1", sr, NELEMS(scompare1_regs), scompare1_regs, NULL);
  ISA_Register_Subclass_Create("acclo", sr, NELEMS(acclo_regs), acclo_regs, NULL);
  ISA_Register_Subclass_Create("acchi", sr, NELEMS(acchi_regs), acchi_regs, NULL);

  ISA_Registers_End();
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
