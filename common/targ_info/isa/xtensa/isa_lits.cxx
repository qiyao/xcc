
/*

  Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.

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
// Generate literal and operand information for the target ISA.
/////////////////////////////////////////////////////////

#include "isa_lits_gen.h"

main (int argc, char** argv)
{
  ISA_Lits_Begin();

  ISA_Create_Lit_Class_Func("simm4",  "xtensa_simm4", -8, 7);
  ISA_Create_Lit_Class_Func("simm7",  "xtensa_simm7", -32, 95);
  ISA_Create_Lit_Class_Func("simm8",  "xtensa_simm8", -128, 127);
  ISA_Create_Lit_Class_Func("simm12",  "xtensa_simm12", -2048, 2047);
  ISA_Create_Lit_Class_Func("simm16",  "xtensa_simm16", -32768, 32767);
  ISA_Create_Lit_Class_Func("simm32",  "xtensa_simm32", -2147483647-1, 2147483647);

  ISA_Create_Lit_Class_Func("uimm4", "xtensa_uimm4", 0, 15);
  ISA_Create_Lit_Class_Func("uimm5", "xtensa_uimm5", 0, 31);
  ISA_Create_Lit_Class_Func("uimm6", "xtensa_uimm6", 0, 63);
  ISA_Create_Lit_Class_Func("uimm4x16", "xtensa_uimm4x16", 0, 240);
  ISA_Create_Lit_Class_Func("uimm8", "xtensa_uimm8", 0, 255);
  ISA_Create_Lit_Class_Func("const16", "xtensa_const16", 0, 42949671);

  ISA_Create_Lit_Class_Func("uimm8x2", "xtensa_uimm8x2", 0, 510);
  ISA_Create_Lit_Class_Func("uimm8x4", "xtensa_uimm8x4", 0, 1020);
  ISA_Create_Lit_Class_Func("uimm12x8", "xtensa_uimm12x8", 0, 32760);
  ISA_Create_Lit_Class_Func("uimm16x4", "xtensa_uimm16x4", -262144, -4);
  ISA_Create_Lit_Class_Func("simm8x256",  "xtensa_simm8x256", -32768, 32512);

  ISA_Create_Lit_Class_Func("neg_uimm8x4", "xtensa_neg_uimm8x4", -1024, -4);

  ISA_Create_Lit_Class_Func("ai4const",  "xtensa_ai4const", -1, 15);
  ISA_Create_Lit_Class_Func("b4constu",  "xtensa_b4constu", 2, 65536);
  ISA_Create_Lit_Class_Func("b4const",  "xtensa_b4const", -1, 256);
  ISA_Create_Lit_Class_Func("soffset", "xtensa_soffset", -131072, 131071);
  ISA_Create_Lit_Class_Func("soffsetx4", "xtensa_soffsetx4", -524288, 524284);
  ISA_Create_Lit_Class_Func("lsi4x4", "xtensa_lsi4x4", 0, 60);
  ISA_Create_Lit_Class_Func("op2p1", "xtensa_op2p1", 1, 16);
  ISA_Create_Lit_Class_Func("tp7", "xtensa_tp7", 7, 22);
  ISA_Create_Lit_Class_Func("msalp32",  "xtensa_msalp32", 1, 32);

  ISA_Create_Lit_Class_Func("not_simm12",  "xtensa_not_simm12", -2147483647-1, 2147483647);

  ISA_Lits_End();
}


// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
