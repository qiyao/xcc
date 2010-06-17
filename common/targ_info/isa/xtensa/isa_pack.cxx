
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
// Group TOPS with similar packing format together. 
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_pack_gen.h"
 
main()
{
  ISA_Pack_Begin("xtensa", 24);

  ISA_PACK_TYPE p1 = ISA_Pack_Type_Create("p1");
  Result(0, 12, 4);
  Operand(0, 0, 16, 4);
  Operand(1, 0, 20, 4);
  Instruction_Pack_Group(p1,
			 TOP_abs, 0x000000ff,
			 TOP_add, 0x000000ff,
			 TOP_add_n, 0x000000ff,
			 TOP_addi, 0x000000ff,
			 TOP_addi_n, 0x000000ff,
			 TOP_addmi, 0x000000ff,
			 TOP_addx2, 0x000000ff,
			 TOP_addx4, 0x000000ff,
			 TOP_addx8, 0x000000ff,
			 TOP_all4, 0x000000ff,
			 TOP_all8, 0x000000ff,
			 TOP_and, 0x000000ff,
			 TOP_andb, 0x000000ff,
			 TOP_andbc, 0x000000ff,
			 TOP_any4, 0x000000ff,
			 TOP_any8, 0x000000ff,
			 TOP_ball, 0x000000ff,
			 TOP_bany, 0x000000ff,
			 TOP_bbc, 0x000000ff,
			 TOP_bbci, 0x000000ff,
			 TOP_bbs, 0x000000ff,
			 TOP_bbsi, 0x000000ff,
			 TOP_beq, 0x000000ff,
			 TOP_beqi, 0x000000ff,
			 TOP_beqt, 0x000000ff,
			 TOP_beqz, 0x000000ff,
			 TOP_beqzt, 0x000000ff,
			 TOP_beqz_n, 0x000000ff,
			 TOP_bf, 0x000000ff,
			 TOP_bge, 0x000000ff,
			 TOP_bgei, 0x000000ff,
			 TOP_bgeu, 0x000000ff,
			 TOP_bgeui, 0x000000ff,
			 TOP_bgez, 0x000000ff,
			 TOP_blt, 0x000000ff,
			 TOP_blti, 0x000000ff,
			 TOP_bltu, 0x000000ff,
			 TOP_bltui, 0x000000ff,
			 TOP_bltz, 0x000000ff,
			 TOP_bnall, 0x000000ff,
			 TOP_bne, 0x000000ff,
			 TOP_bnei, 0x000000ff,
			 TOP_bnet, 0x000000ff,
			 TOP_bnez, 0x000000ff,
			 TOP_bnezt, 0x000000ff,
			 TOP_bnez_n, 0x000000ff,
			 TOP_bnone, 0x000000ff,
			 TOP_break, 0x000000ff,
			 TOP_break_n, 0x000000ff,
			 TOP_bt, 0x000000ff,
			 TOP_call0, 0x000000ff,
			 TOP_call12, 0x000000ff,
			 TOP_call4, 0x000000ff,
			 TOP_call8, 0x000000ff,
			 TOP_callx0, 0x000000ff,
			 TOP_callx12, 0x000000ff,
			 TOP_callx4, 0x000000ff,
			 TOP_callx8, 0x000000ff,
			 TOP_chk, 0x000000ff,
			 TOP_clamps, 0x000000ff,
			 TOP_const16, 0x000000ff,
			 TOP_dhi, 0x000000ff,
			 TOP_dhu, 0x000000ff,
			 TOP_dhwb, 0x000000ff,
			 TOP_dhwbi, 0x000000ff,
			 TOP_diwb, 0x000000ff,
			 TOP_diwbi, 0x000000ff,
			 TOP_dii, 0x000000ff,
			 TOP_diu, 0x000000ff,
			 TOP_dpfl, 0x000000ff,
			 TOP_dpfr, 0x000000ff,
			 TOP_dpfro, 0x000000ff,
			 TOP_dpfw, 0x000000ff,
			 TOP_dpfwo, 0x000000ff,
			 TOP_dsync, 0x000000ff,
			 TOP_entry, 0x000000ff,
			 TOP_esync, 0x000000ff,
			 TOP_excw, 0x000000ff,
			 TOP_extui, 0x000000ff,
			 TOP_extw, 0x000000ff,
			 TOP_hwwitlba, 0x000000ff,
			 TOP_hwwdtlba, 0x000000ff,
			 TOP_ihi, 0x000000ff,
			 TOP_ihu, 0x000000ff,
			 TOP_idtlb, 0x000000ff,
			 TOP_iitlb, 0x000000ff,
			 TOP_iii, 0x000000ff,
			 TOP_iiu, 0x000000ff,
			 TOP_ill, 0x000000ff,
			 TOP_ill_n, 0x000000ff,
			 TOP_inval, 0x000000ff,
			 TOP_ipf, 0x000000ff,
			 TOP_ipfl, 0x000000ff,
			 TOP_isync, 0x000000ff,
			 TOP_j, 0x000000ff,
			 TOP_jx, 0x000000ff,
			 TOP_l16si, 0x000000ff,
			 TOP_l16ui, 0x000000ff,
			 TOP_l32ai, 0x000000ff,
			 TOP_l32e, 0x000000ff,
			 TOP_l32i, 0x000000ff,
			 TOP_l32i_n, 0x000000ff,
			 TOP_l32r, 0x000000ff,
			 TOP_l32si, 0x000000ff,
			 TOP_l8ui, 0x000000ff,
			 TOP_ldct, 0x000000ff,
			 TOP_ldpte, 0x000000ff,
			 TOP_lict, 0x000000ff,
			 TOP_licw, 0x000000ff,
			 TOP_loop, 0x000000ff,
			 TOP_loopgtz, 0x000000ff,
			 TOP_loopnez, 0x000000ff,
			 TOP_max, 0x000000ff,
			 TOP_maxu, 0x000000ff,
			 TOP_memw, 0x000000ff,
			 TOP_min, 0x000000ff,
			 TOP_minu, 0x000000ff,
			 TOP_mov_n, 0x000000ff,
			 TOP_moveqz, 0x000000ff,
			 TOP_movf, 0x000000ff,
			 TOP_movgez, 0x000000ff,
			 TOP_movi, 0x000000ff,
			 TOP_movi_n, 0x000000ff,
			 TOP_movltz, 0x000000ff,
			 TOP_movnez, 0x000000ff,
			 TOP_movsp, 0x000000ff,
			 TOP_movt, 0x000000ff,
			 TOP_mul16s, 0x000000ff,
			 TOP_mul16u, 0x000000ff,
			 TOP_neg, 0x000000ff,
			 TOP_nop, 0x000000ff,
			 TOP_nop_n, 0x000000ff,
			 TOP_nsa, 0x000000ff,
			 TOP_nsau, 0x000000ff,
			 TOP_or, 0x000000ff,
			 TOP_orb, 0x000000ff,
			 TOP_orbc, 0x000000ff,
			 TOP_pdtlb, 0x000000ff,
			 TOP_pitlb, 0x000000ff,
			 TOP_quou, 0x000000ff,
			 TOP_quos, 0x000000ff,
			 TOP_rdtlb0, 0x000000ff,
			 TOP_rdtlb1, 0x000000ff,
			 TOP_ritlb0, 0x000000ff,
			 TOP_ritlb1, 0x000000ff,
			 TOP_remu, 0x000000ff,
			 TOP_rems, 0x000000ff,
			 TOP_ret, 0x000000ff,
			 TOP_ret_n, 0x000000ff,
			 TOP_retw, 0x000000ff,
			 TOP_retw_n, 0x000000ff,
			 TOP_rfdd, 0x000000ff,
			 TOP_rfde, 0x000000ff,
			 TOP_rfdo, 0x000000ff,
			 TOP_rfe, 0x000000ff,
			 TOP_rfi, 0x000000ff,
			 TOP_rfme, 0x000000ff,
			 TOP_rfue, 0x000000ff,
			 TOP_rfwo, 0x000000ff,
			 TOP_rfwu, 0x000000ff,
			 TOP_rotw, 0x000000ff,
			 TOP_rsil, 0x000000ff,
			 TOP_rsync, 0x000000ff,
			 TOP_s16i, 0x000000ff,
			 TOP_s32c1i, 0x000000ff,
			 TOP_s32e, 0x000000ff,
			 TOP_s32i, 0x000000ff,
			 TOP_s32i_n, 0x000000ff,
			 TOP_s32ri, 0x000000ff,
			 TOP_s8i, 0x000000ff,
			 TOP_sdct, 0x000000ff,
			 TOP_sext, 0x000000ff,
			 TOP_sict, 0x000000ff,
			 TOP_sicw, 0x000000ff,
			 TOP_simcall, 0x000000ff,
			 TOP_sll, 0x000000ff,
			 TOP_slli, 0x000000ff,
			 TOP_sra, 0x000000ff,
			 TOP_srai, 0x000000ff,
			 TOP_src, 0x000000ff,
			 TOP_srl, 0x000000ff,
			 TOP_srli, 0x000000ff,
			 TOP_ssa8b, 0x000000ff,
			 TOP_ssa8l, 0x000000ff,
			 TOP_ssai, 0x000000ff,
			 TOP_ssl, 0x000000ff,
			 TOP_ssr, 0x000000ff,
			 TOP_sub, 0x000000ff,
			 TOP_subx2, 0x000000ff,
			 TOP_subx4, 0x000000ff,
			 TOP_subx8, 0x000000ff,
			 TOP_syscall, 0x000000ff,
			 TOP_xor, 0x000000ff,
			 TOP_xorb, 0x000000ff,
			 TOP_waiti, 0x000000ff,
			 TOP_wdtlb, 0x000000ff,
			 TOP_witlb, 0x000000ff,
			 TOP_UNDEFINED);

  ISA_Pack_End();
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
