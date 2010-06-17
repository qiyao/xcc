
/*

  Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.

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
//  Generate ABI information
///////////////////////////////////////

#include <stddef.h>
#include "abi_properties_gen.h"
#include "targ_isa_registers.h"

/* Some of these properties are not used by xtensa, so we don't register them
   so we can catch potential errors. The commented out properties are used in
   be/cg/register.cxx to initialize the dedicated tns that have the
   property. We also ifdef out that code so that no tns are ever shown as
   having these properties. */

static ABI_PROPERTY
  allocatable,
  callee,
  caller,
  func_arg,
  func_val,
  stack_ptr,
  frame_ptr,
  static_link,
  shift_amount,
  br_regfile,
  zcl_loop_begin,
  zcl_loop_end,
  zcl_loop_count,
  acc_lo,
  ret_addr,
  acc_hi;
#if 0 // non xtensa
  global_ptr,
  entry_ptr,
  zero,
  prev_funcstate,
  loop_count,
  epilog_count,
  true_predicate,
  stacked,
  fzero,
  fone;
#endif

static void windowed_abi(void)
{
  /* ar registers */
  Reg_Property(allocatable, ISA_REGCLASS_integer,
		 2,   3,   4,   5,   6,   7,  
		 8,   9,  10,  11,  12,  13,   14,  15,
	       -1);
  Reg_Property(callee, ISA_REGCLASS_integer,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_integer,
		 8,   9,  10,  11,  12,  13,  14,  15, 
	       -1);
  Reg_Property(func_arg, ISA_REGCLASS_integer,
	       2,   3,   4,   5,   6,    7,
	       -1);
  Reg_Property(func_val, ISA_REGCLASS_integer,
			   2,   3,   4,   5,
	       -1);
  
  /* Xtensa doesn't use a global pointer, However, some 
  // code in be/cg/calls.cxx
  // and registers.cxx rely on it being present.  I think this is
  // okay since we should never actually reference the GP and the
  // gra will allocate it for other purposes.
  Reg_Property(global_ptr, ISA_REGCLASS_integer, 
		6,
	       -1);
  */

  Reg_Property(frame_ptr, ISA_REGCLASS_integer, 
		7,
	       -1);
  Reg_Property(stack_ptr, ISA_REGCLASS_integer, 
		1,
	       -1);

  /* windowed abi passes static link at SP-20, but the compiler expects 
     a register, so just make it the same as fp. */
  Reg_Property(static_link, ISA_REGCLASS_integer, 
		7,
	       -1);

  /* For Windowed, we should not set this property because xcalibur
  // will try to save this register upon entry.  In Xtensa's windowed
  // abi, window rotation will in effect save the register.
  // So don't set this to avoid extraneous saves.
  
  Reg_Property(ret_addr, ISA_REGCLASS_integer, 
		0,
	       -1);
  */

  /* mr registers */
  Reg_Property(allocatable, ISA_REGCLASS_macc,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_macc,
	       0, 1, 2, 3,
	       -1);

  /* br registers */
  Reg_Property(allocatable, ISA_REGCLASS_branch,
	       0,  1,  2,  3,  4,  5,  6,  7,
	       8,  9, 10, 11, 12, 13, 14, 15,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_branch,
	       0,  1,  2,  3,  4,  5,  6,  7,
	       8,  9, 10, 11, 12, 13, 14, 15,
	       -1);
  Reg_Property(func_val, ISA_REGCLASS_branch,
	       0,
	       -1);

  /* special registers */
  Reg_Property(zcl_loop_begin, ISA_REGCLASS_special,
	       0, -1);

  Reg_Property(zcl_loop_end, ISA_REGCLASS_special,
	       1, -1);

  Reg_Property(zcl_loop_count, ISA_REGCLASS_special,
	       2, -1);

  Reg_Property(shift_amount, ISA_REGCLASS_special,
	       3, -1);

  Reg_Property(br_regfile, ISA_REGCLASS_special,
	       4, -1);

  Reg_Property(acc_lo, ISA_REGCLASS_special,
	       16, -1);

  Reg_Property(acc_hi, ISA_REGCLASS_special,
	       17, -1);
}

static void call0_abi(void)
{
  /* ar registers */
  Reg_Property(allocatable, ISA_REGCLASS_integer,
	       0, 2,   3,   4,   5,   6,   7,  
	       8,   9,  10,  11,  12,  13,   14,  15,
	       -1);
  Reg_Property(callee, ISA_REGCLASS_integer, 
	       12, 13, 14, 15,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_integer,
	       0, 2,   3,  4,  5,  6,  7,  8,  9, 10, 11,
	       -1);
  Reg_Property(func_arg, ISA_REGCLASS_integer,
	       2,   3,   4,   5,   6,   7,  
	       -1);
  Reg_Property(func_val, ISA_REGCLASS_integer,
	       2,   3,   4,   5,
	       -1);
  
  Reg_Property(frame_ptr, ISA_REGCLASS_integer, 
	       15,
	       -1);
  Reg_Property(stack_ptr, ISA_REGCLASS_integer, 
	       1,
	       -1);

  Reg_Property(static_link, ISA_REGCLASS_integer, 
		8,
	       -1);

  Reg_Property(ret_addr, ISA_REGCLASS_integer, 
		0,
	       -1);

  /* mr registers */
  Reg_Property(allocatable, ISA_REGCLASS_macc,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_macc,
	       0, 1, 2, 3,
	       -1);

  /* br registers */
  Reg_Property(allocatable, ISA_REGCLASS_branch,
	       0,  1,  2,  3,  4,  5,  6,  7,
	       8,  9, 10, 11, 12, 13, 14, 15,
	       -1);
  Reg_Property(caller, ISA_REGCLASS_branch,
	       0,  1,  2,  3,  4,  5,  6,  7,
	       8,  9, 10, 11, 12, 13, 14, 15,
	       -1);
  Reg_Property(func_val, ISA_REGCLASS_branch,
	       0,
	       -1);

  /* special registers */
  Reg_Property(zcl_loop_begin, ISA_REGCLASS_special,
	       0, -1);

  Reg_Property(zcl_loop_end, ISA_REGCLASS_special,
	       1, -1);

  Reg_Property(zcl_loop_count, ISA_REGCLASS_special,
	       2, -1);

  Reg_Property(shift_amount, ISA_REGCLASS_special,
	       3, -1);

  Reg_Property(br_regfile, ISA_REGCLASS_special,
	       4, -1);

  Reg_Property(acc_lo, ISA_REGCLASS_special,
	       16, -1);

  Reg_Property(acc_hi, ISA_REGCLASS_special,
	       17, -1);
}

main()
{
  ABI_Properties_Begin("xtensa");

  allocatable = Create_Reg_Property("allocatable");
  callee = Create_Reg_Property("callee");
  caller = Create_Reg_Property("caller");
  func_arg = Create_Reg_Property("func_arg");
  func_val = Create_Reg_Property("func_val");
  stack_ptr = Create_Reg_Property("stack_ptr");
  frame_ptr = Create_Reg_Property("frame_ptr");
  static_link = Create_Reg_Property("static_link");
  ret_addr = Create_Reg_Property("ret_addr");
  zcl_loop_begin = Create_Reg_Property("zcl_loop_begin");
  zcl_loop_end = Create_Reg_Property("zcl_loop_end");
  zcl_loop_count = Create_Reg_Property("zcl_loop_count");
  shift_amount = Create_Reg_Property("shift_amount");
  br_regfile = Create_Reg_Property("br_regfile");
  acc_lo = Create_Reg_Property("acc_lo");
  acc_hi = Create_Reg_Property("acc_hi");

#if 0
  stacked = Create_Reg_Property("stacked");
  global_ptr = Create_Reg_Property("global_ptr");
  entry_ptr = Create_Reg_Property("entry_ptr");
  zero = Create_Reg_Property("zero");
  prev_funcstate = Create_Reg_Property("prev_funcstate");
  loop_count = Create_Reg_Property("loop_count");
  epilog_count = Create_Reg_Property("epilog_count");
  true_predicate = Create_Reg_Property("true_predicate");
  fzero = Create_Reg_Property("fzero");
  fone = Create_Reg_Property("fone");
#endif
  
  Begin_ABI("windowed");
  windowed_abi();
  
  Begin_ABI("call0");
  call0_abi();

  ABI_Properties_End(); /* xtensa_call0 */


}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
