
/*

  Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.

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
// Print assembly for TOPS
/////////////////////////////////////////////////////////

#include <stddef.h>
#include <string.h>
#include "topcode.h"
#include "isa_print_gen.h"

// Multiple topcodes map to the same assembly name. To disambiguate the 
// topcodes, we append a suffix to the basename. By convention, the 
// suffix starts with an underscore. To get the assembly name we strip off
// the suffix.

main()
{
  ISA_Print_Begin("xtensa");

  Define_Macro("END_GROUP", "");		// end-of-group marker
  Define_Macro("PREDICATE", "(%s)");		// predicate operand format
  Define_Macro("BEGIN_BUNDLE", "{\t# format %s");	// bundle introducer
  Define_Macro("END_BUNDLE", "}");		// bundle terminator

  Set_AsmName_Func(NULL);

  /* No results / no operands */
  ISA_PRINT_TYPE no_rop = ISA_Print_Type_Create("no_rop", "%s");
  Name();
  Instruction_Print_Group(no_rop,
			  TOP_simcall, TOP_syscall, 
			  TOP_nop_n, TOP_nop, 
			  TOP_dsync, TOP_esync, TOP_extw, TOP_hwwitlba, TOP_hwwdtlba,
			  TOP_isync, TOP_ldpte,
			  TOP_memw, TOP_rsync, 
			  TOP_ret, TOP_retw, TOP_ret_n, TOP_retw_n, 
			  TOP_excw, 
			  TOP_rfdd, TOP_rfde, TOP_rfe, TOP_rfme, TOP_rfue,
			  TOP_rfwo, TOP_rfwu,
			  TOP_UNDEFINED);

  /* No results/ one operand */
  ISA_PRINT_TYPE op = ISA_Print_Type_Create("op", "%s\t%s");
  Name();
  Operand(0);
  Instruction_Print_Group(op,
			  TOP_break_n, 
			  TOP_ssa8b, TOP_ssa8l, TOP_ssl, TOP_ssr, TOP_ssai, 
			  TOP_callx0, TOP_callx4, TOP_callx8, TOP_callx12, 
			  TOP_call0, TOP_call4, TOP_call8, TOP_call12,
			  TOP_chk,
			  TOP_idtlb, TOP_iitlb,
			  TOP_jx, TOP_j, 
			  TOP_rfdo,
			  TOP_rfi, TOP_waiti, TOP_rotw, 
			  TOP_UNDEFINED);

  /* One result / one operand */
  ISA_PRINT_TYPE rop = ISA_Print_Type_Create("rop", "%s\t%s,%s");
  Name();
  Result(0);
  Operand(0);
  Instruction_Print_Group(rop,
			  TOP_movi_n, TOP_movi, TOP_mov_n,
			  TOP_abs, TOP_neg,
			  TOP_nsa, TOP_nsau, 
			  TOP_movsp, 
			  TOP_l32r, 
			  TOP_sll, TOP_sra, TOP_srl, 
			  TOP_all4, TOP_any4, TOP_all8, TOP_any8, 
			  TOP_inval, TOP_ldct, TOP_lict, TOP_licw,
			  TOP_pdtlb, TOP_pitlb,
			  TOP_rdtlb0, TOP_rdtlb1,
			  TOP_ritlb0, TOP_ritlb1,
			  TOP_sdct, TOP_sict, TOP_sicw, TOP_rsil,
			  TOP_UNDEFINED);

  ISA_PRINT_TYPE const16 = ISA_Print_Type_Create("const16", "%s\t%s, %s");
  Name();
  Result(0);
  Operand(1);
  Instruction_Print_Group(const16, TOP_const16, TOP_UNDEFINED);

  /* No results / two operands */
  ISA_PRINT_TYPE opop = ISA_Print_Type_Create("opop", "%s\t%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Instruction_Print_Group(opop,
			  TOP_break, 
			  TOP_dhi, TOP_dhu, TOP_dhwb, TOP_dhwbi,
			  TOP_diwb, TOP_diwbi, TOP_dii, TOP_diu,
			  TOP_dpfl,
			  TOP_dpfr, TOP_dpfro, TOP_dpfw, TOP_dpfwo, 
			  TOP_ihi, TOP_ihu, TOP_iii, TOP_iiu, TOP_ipf, TOP_ipfl, 
			  TOP_ill, TOP_ill_n,
			  TOP_bf, TOP_bt, 
			  TOP_beqz_n, TOP_bnez_n, 
			  TOP_beqz, TOP_beqzt, TOP_bgez, TOP_bltz, 
			  TOP_bnez, TOP_bnezt, 
			  TOP_loop, TOP_loopgtz, TOP_loopnez, 
			  TOP_entry, 
			  TOP_wdtlb, TOP_witlb, 
			  TOP_UNDEFINED);

  /* One result / two operands */
  ISA_PRINT_TYPE ropop = ISA_Print_Type_Create("ropop", "%s\t%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Instruction_Print_Group(ropop,
			  TOP_l8ui, TOP_l16si, TOP_l16ui,
			  TOP_l32e, TOP_l32i, TOP_l32i_n, 
			  TOP_l32ai, TOP_l32si,
			  TOP_addmi, TOP_addi_n, 
			  TOP_sext, 
			  TOP_slli, TOP_srli, TOP_srai, 
			  TOP_add_n, TOP_max, TOP_maxu, TOP_min, TOP_minu, TOP_mul16s,
			  TOP_mul16u, TOP_and, TOP_or, TOP_xor, TOP_src,
			  TOP_add, TOP_addi, TOP_addx2, TOP_addx4, TOP_addx8, TOP_sub,
			  TOP_subx2, TOP_subx4, TOP_subx8, 
			  TOP_andb, TOP_andbc, TOP_orb, TOP_orbc, TOP_xorb, 
			  TOP_clamps, 
			  TOP_quou, TOP_quos, TOP_remu, TOP_rems,
			  TOP_UNDEFINED);

  /* One inout / two operands */
  ISA_PRINT_TYPE ioopop = ISA_Print_Type_Create("ioopop", "%s\t%s,%s,%s");
  Name();
  Result(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(ioopop,
			  TOP_moveqz, TOP_movgez, TOP_movltz, TOP_movnez, 
			  TOP_movf, TOP_movt, 
                          TOP_s32c1i,
			  TOP_UNDEFINED);

  /* No results / three operands */
  ISA_PRINT_TYPE opopop = ISA_Print_Type_Create("opopop", "%s\t%s,%s,%s");
  Name();
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,
			  TOP_ball, TOP_bany, TOP_bbc, TOP_bbs, 
			  TOP_beq, TOP_beqt, 
			  TOP_bge, TOP_bgeu, TOP_blt, TOP_bltu, TOP_bnall,
			  TOP_bne, TOP_bnet, TOP_bnone, 
			  TOP_beqi, TOP_bgei, TOP_blti, TOP_bnei, 
			  TOP_bbci, TOP_bbsi, TOP_bgeui, TOP_bltui, 
			  TOP_s8i, TOP_s16i,
			  TOP_s32e, TOP_s32i, TOP_s32i_n, 
			  TOP_s32ri,
			  TOP_UNDEFINED);

  /* One result / three operands */
  ISA_PRINT_TYPE ropopop = ISA_Print_Type_Create("opopop", "%s\t%s,%s,%s,%s");
  Name();
  Result(0);
  Operand(0);
  Operand(1);
  Operand(2);
  Instruction_Print_Group(opopop,
			  TOP_extui, 
			  TOP_UNDEFINED);

  ISA_Print_End();
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
