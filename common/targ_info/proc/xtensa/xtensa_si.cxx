
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

//  Tensilica xtensa scheduling information. For now just doing
//  something simple for all xtensa targets. This should be
//  implementation dependent...
///////////////////////////////////////



#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue;

int
main (int argc, char *argv[])
{
  Machine("athens", ISA_SUBSET_xtensa, argc, argv);

  res_issue = RESOURCE_Create("issue", 1);

  Instruction_Group("default",
		    TOP_abs,
		    TOP_add,
		    TOP_add_n,
		    TOP_addi,
		    TOP_addi_n,
		    TOP_addmi,
		    TOP_addx2,
		    TOP_addx4,
		    TOP_addx8,
		    TOP_all4,
		    TOP_all8,
		    TOP_and,
		    TOP_andb,
		    TOP_andbc,
		    TOP_any4,
		    TOP_any8,
		    TOP_ball,
		    TOP_bany,
		    TOP_bbc,
		    TOP_bbci,
		    TOP_bbs,
		    TOP_bbsi,
		    TOP_beq,
		    TOP_beqi,
		    TOP_beqt,
		    TOP_beqz,
		    TOP_beqzt,
		    TOP_beqz_n,
		    TOP_bf,
		    TOP_bge,
		    TOP_bgei,
		    TOP_bgeu,
		    TOP_bgeui,
		    TOP_bgez,
		    TOP_blt,
		    TOP_blti,
		    TOP_bltu,
		    TOP_bltui,
		    TOP_bltz,
		    TOP_bnall,
		    TOP_bne,
		    TOP_bnei,
		    TOP_bnet,
		    TOP_bnez,
		    TOP_bnezt,
		    TOP_bnez_n,
		    TOP_bnone,
		    TOP_break,
		    TOP_break_n,
		    TOP_bt,
		    TOP_call0,
		    TOP_call12,
		    TOP_call4,
		    TOP_call8,
		    TOP_callx0,
		    TOP_callx12,
		    TOP_callx4,
		    TOP_callx8,
		    TOP_chk,
		    TOP_clamps,
		    TOP_const16,
		    TOP_dhi,
		    TOP_dhu,
		    TOP_dhwb,
		    TOP_dhwbi,
		    TOP_diwb,
		    TOP_diwbi,
		    TOP_dii,
		    TOP_diu,
		    TOP_dpfl,
		    TOP_dpfr,
		    TOP_dpfro,
		    TOP_dpfw,
		    TOP_dpfwo,
		    TOP_dsync,
		    TOP_entry,
		    TOP_esync,
		    TOP_excw,
		    TOP_extui,
		    TOP_extw,
		    TOP_hwwitlba,
		    TOP_hwwdtlba,
		    TOP_idtlb,
		    TOP_iitlb,
		    TOP_ihi,
		    TOP_ihu,
		    TOP_iii,
		    TOP_iiu,
		    TOP_inval,
		    TOP_ipfl,
		    TOP_ipf,
		    TOP_isync,
		    TOP_j,
		    TOP_jx,
		    TOP_ldct,
		    TOP_ldpte,
		    TOP_lict,
		    TOP_licw,
		    TOP_loop,
		    TOP_loopgtz,
		    TOP_loopnez,
		    TOP_max,
		    TOP_maxu,
		    TOP_memw,
		    TOP_min,
		    TOP_minu,
		    TOP_mov_n,
		    TOP_moveqz,
		    TOP_movf,
		    TOP_movgez,
		    TOP_movi,
		    TOP_movi_n,
		    TOP_movltz,
		    TOP_movnez,
		    TOP_movsp,
		    TOP_movt,
		    TOP_neg,
		    TOP_nop,
		    TOP_nop_n,
		    TOP_nsa,
		    TOP_nsau,
		    TOP_or,
		    TOP_orb,
		    TOP_orbc,
		    TOP_pdtlb,
		    TOP_pitlb,
		    TOP_quou,
		    TOP_quos,
		    TOP_rdtlb0,
		    TOP_rdtlb1,
		    TOP_ritlb0,
		    TOP_ritlb1,
		    TOP_remu,
		    TOP_rems,
		    TOP_ret,
		    TOP_ret_n,
		    TOP_retw,
		    TOP_retw_n,
		    TOP_rfdd,
		    TOP_rfde,
		    TOP_rfdo,
		    TOP_rfe,
		    TOP_rfi,
		    TOP_rfme,
		    TOP_rfue,
		    TOP_rfwo,
		    TOP_rfwu,
		    TOP_rotw,
		    TOP_rsil,
		    TOP_rsr,
		    TOP_rsync,
		    TOP_s16i,
		    TOP_s32c1i,
		    TOP_s32i,
		    TOP_s32e,
		    TOP_s32i_n,
		    TOP_s32ri,
		    TOP_s8i,
		    TOP_sdct,
		    TOP_sext,
		    TOP_sict,
		    TOP_sicw,
		    TOP_simcall,
		    TOP_sll,
		    TOP_slli,
		    TOP_sra,
		    TOP_srai,
		    TOP_src,
		    TOP_srl,
		    TOP_srli,
		    TOP_ssa8b,
		    TOP_ssa8l,
		    TOP_ssai,
		    TOP_ssl,
		    TOP_ssr,
		    TOP_sub,
		    TOP_subx2,
		    TOP_subx4,
		    TOP_subx8,
		    TOP_syscall,
		    TOP_wsr,
		    TOP_xor,
		    TOP_xorb,
		    TOP_xsr,
		    TOP_waiti,
		    TOP_wdtlb,
		    TOP_witlb,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("load",
		    TOP_l16si,
		    TOP_l16ui,
		    TOP_l32ai,
		    TOP_l32e,
		    TOP_l32i,
		    TOP_l32i_n,
		    TOP_l32r,
		    TOP_l32si,
		    TOP_l8ui,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("mul16",
		    TOP_mul16s,
		    TOP_mul16u,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("pseudo1",
		    TOP_asm,
		    TOP_const16hi,
		    TOP_const16lo,
		    TOP_extw_pseudo,
		    TOP_memw_pseudo,
		    TOP_ill,
		    TOP_ill_n,
		    TOP_spadjust,
		    TOP_intrncall,
		    TOP_loop_end,
		    TOP_br_to_ar,
		    TOP_br2_to_ar,
		    TOP_br4_to_ar,
		    TOP_br8_to_ar,
		    TOP_br16_to_ar,
		    TOP_ar_to_br,
		    TOP_ar_to_br2,
		    TOP_ar_to_br4,
		    TOP_ar_to_br8,
		    TOP_ar_to_br16,
		    TOP_get_tmp_ar,
		    TOP_movbr,
		    TOP_movbr2,
		    TOP_movbr4,
		    TOP_movbr8,
		    TOP_movbr16,
		    TOP_wsr_br,
		    TOP_rsr_br,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  Instruction_Group("pseudo2",
		    TOP_load_const,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);


  Machine_Done("xtensa.cxx");
}


// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
