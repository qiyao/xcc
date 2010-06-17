
/*

  Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.

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
//  Generate bundle information
///////////////////////////////////////

#include <stddef.h>
#include "topcode.h"
#include "isa_bundle_gen.h"

main()
{
  ISA_EXEC_UNIT_TYPE
    Fetch_Unit;  // Instruction fetch type

  ISA_Bundle_Begin("xtensa", 32);

  ISA_Bundle_Pack_Create(ISA_Bundle_Pack_Little_Endian);
  Pack_Slot(0, 0, 0, 24);

  /* ===== Specification for Fetch_Unit Type ===== */
  Fetch_Unit = ISA_Exec_Unit_Type_Create("Fetch_Unit", NULL);
  Instruction_Exec_Unit_Group(Fetch_Unit,
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
			      TOP_beqz_n,
			      TOP_beqzt,
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
			      TOP_bnez_n,
			      TOP_bnezt,
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
			      TOP_clamps,
			      TOP_const16,
			      TOP_const16hi,
			      TOP_const16lo,
			      TOP_dhwb,
			      TOP_dhwbi,
			      TOP_dpfr,
			      TOP_dpfro,
			      TOP_dpfw,
			      TOP_dpfwo,
			      TOP_dsync,
			      TOP_entry,
			      TOP_esync,
			      TOP_excw,
			      TOP_extui,
			      TOP_ipf,
			      TOP_isync,
			      TOP_j,
			      TOP_jx,
			      TOP_l16si,
			      TOP_l16ui,
			      TOP_l32i,
			      TOP_l32i_n,
			      TOP_l32r,
			      TOP_l8ui,
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
			      TOP_mul16s,
			      TOP_mul16u,
			      TOP_neg,
			      TOP_nop,
			      TOP_nop_n,
			      TOP_nsa,
			      TOP_nsau,
			      TOP_or,
			      TOP_orb,
			      TOP_orbc,
			      TOP_quou,
			      TOP_quos,
			      TOP_remu,
			      TOP_rems,
			      TOP_ret,
			      TOP_ret_n,
			      TOP_retw,
			      TOP_retw_n,
			      TOP_rsr,
			      TOP_rsync,
			      TOP_s16i,
			      TOP_s32i,
			      TOP_s32i_n,
			      TOP_s8i,
			      TOP_sext,
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
			      TOP_UNDEFINED);

  ISA_Bundle_Type_Create("i", ".i", 1);
  Slot (0, Fetch_Unit);

  ISA_Bundle_End();
  return 0;
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
