
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
// Generate ISA information for operands and results. Group instructions
// with similar formats.
/////////////////////////////////////////////////////////

#include <assert.h>
#include <stdio.h>
#include <trace.h>
#include "isa_operands_gen.h"
#include "topcode.h"


main(int argc, char** argv)
{
  ISA_Operands_Begin("xtensa");

  /* Literals... */

  OPERAND_VALUE_TYPE msalp32, simm8x256, simm4, simm7, simm8, simm12pc,
    simm12, not_simm12, simm16, simm32, simm8pc, simm32pc, ai4const, b4constu, b4const, soffset,
    soffsetx4, lsi4x4, uimm12x8, uimm16x4, uimm4, uimm4x16, uimm5, uimm6pc,
    uimm8, uimm8pc, uimm8x2, uimm8x4, const16, neg_uimm8x4, op2p1, tp7;

  simm8x256 = ISA_Lit_Opnd_Type_Create("simm8x256", 8, SIGNED, LC_simm8x256);
  simm4 = ISA_Lit_Opnd_Type_Create("simm4", 4, SIGNED, LC_simm4);
  simm7 = ISA_Lit_Opnd_Type_Create("simm7", 7, SIGNED, LC_simm7);
  simm8 = ISA_Lit_Opnd_Type_Create("simm8", 8, SIGNED, LC_simm8);
  simm12 = ISA_Lit_Opnd_Type_Create("simm12", 12, SIGNED, LC_simm12);
  simm16 = ISA_Lit_Opnd_Type_Create("simm16", 16, SIGNED, LC_simm16);
  simm32 = ISA_Lit_Opnd_Type_Create("simm32", 32, SIGNED, LC_simm32);
  uimm12x8 = ISA_Lit_Opnd_Type_Create("uimm12x8", 12, UNSIGNED, LC_uimm12x8);
  uimm16x4 = ISA_Lit_Opnd_Type_Create("uimm16x4", 16, UNSIGNED, LC_uimm16x4);
  uimm4x16 = ISA_Lit_Opnd_Type_Create("uimm4x16", 4, UNSIGNED, LC_uimm4x16);
  uimm4 = ISA_Lit_Opnd_Type_Create("uimm4", 4, UNSIGNED, LC_uimm4);
  uimm5 = ISA_Lit_Opnd_Type_Create("uimm5", 5, UNSIGNED, LC_uimm5);
  uimm8 = ISA_Lit_Opnd_Type_Create("uimm8", 8, UNSIGNED, LC_uimm8);
  uimm8x2 = ISA_Lit_Opnd_Type_Create("uimm8x2", 8, UNSIGNED, LC_uimm8x2);
  uimm8x4 = ISA_Lit_Opnd_Type_Create("uimm8x4", 8, UNSIGNED, LC_uimm8x4);
  const16 = ISA_Lit_Opnd_Type_Create("const16", 16, UNSIGNED, LC_const16);
  neg_uimm8x4 = ISA_Lit_Opnd_Type_Create("neg_uimm8x4", 8, UNSIGNED, LC_neg_uimm8x4);

  ai4const = ISA_Lit_Opnd_Type_Create("ai4const", 4, SIGNED, LC_ai4const);
  b4constu = ISA_Lit_Opnd_Type_Create("b4constu", 4, UNSIGNED, LC_b4constu);
  b4const = ISA_Lit_Opnd_Type_Create("b4const", 4, SIGNED, LC_b4const);
  lsi4x4 = ISA_Lit_Opnd_Type_Create("lsi4x4", 4, UNSIGNED, LC_lsi4x4);
  op2p1 = ISA_Lit_Opnd_Type_Create("op2p1", 4, UNSIGNED, LC_op2p1);
  tp7 = ISA_Lit_Opnd_Type_Create("tp7", 4, UNSIGNED, LC_tp7);
  msalp32 = ISA_Lit_Opnd_Type_Create("msalp32", 5, UNSIGNED, LC_msalp32);

  simm8pc = ISA_Lit_Opnd_Type_Create("simm8pc", 8, PCREL, LC_simm8);
  simm12pc = ISA_Lit_Opnd_Type_Create("simm12pc", 12, PCREL, LC_simm12);
  simm32pc = ISA_Lit_Opnd_Type_Create("simm32pc", 32, PCREL, LC_simm32);
  uimm6pc = ISA_Lit_Opnd_Type_Create("uimm6pc", 6, PCREL, LC_uimm6);
  uimm8pc = ISA_Lit_Opnd_Type_Create("uimm8pc", 8, PCREL, LC_uimm8);
  soffset = ISA_Lit_Opnd_Type_Create("soffset", 18, PCREL, LC_soffset);
  soffsetx4 = ISA_Lit_Opnd_Type_Create("soffsetx4", 18, PCREL, LC_soffsetx4);

  not_simm12 = ISA_Lit_Opnd_Type_Create("not_simm12", 32, SIGNED, LC_not_simm12);

  /* Registers... */

  OPERAND_VALUE_TYPE ar, mr, br, sr;
  OPERAND_VALUE_TYPE mr_x, mr_y;
  OPERAND_VALUE_TYPE br_1, br_2, br_4, br_8, br_16;
  OPERAND_VALUE_TYPE sr_lbeg, sr_lend, sr_lcount;
  OPERAND_VALUE_TYPE sr_sar;
  OPERAND_VALUE_TYPE scompare1;

  ar = ISA_Reg_Opnd_Type_Create("ar", ISA_REGCLASS_integer,
				ISA_REGSUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  mr = ISA_Reg_Opnd_Type_Create("mr", ISA_REGCLASS_macc,
				ISA_REGSUBCLASS_UNDEFINED,
				32, SIGNED, INVALID);
  br = ISA_Reg_Opnd_Type_Create("br", ISA_REGCLASS_branch,
				ISA_REGSUBCLASS_UNDEFINED,
				1, UNSIGNED, INVALID);
  sr = ISA_Reg_Opnd_Type_Create("sr", ISA_REGCLASS_special,
				ISA_REGSUBCLASS_UNDEFINED,
				32, UNSIGNED, INVALID);

  mr_x  = ISA_Reg_Opnd_Type_Create("mr_x", ISA_REGCLASS_macc,
				   ISA_REGSUBCLASS_macc_x,
				   32, SIGNED, INVALID);
  mr_y  = ISA_Reg_Opnd_Type_Create("mr_y", ISA_REGCLASS_macc,
				   ISA_REGSUBCLASS_macc_y,
				   32, SIGNED, INVALID);

  br_1  = ISA_Reg_Opnd_Type_Create("br_1", ISA_REGCLASS_branch,
				   ISA_REGSUBCLASS_branch_1,
				   1, UNSIGNED, INVALID);
  br_2  = ISA_Reg_Opnd_Type_Create("br_2", ISA_REGCLASS_branch,
				   ISA_REGSUBCLASS_branch_2,
				   2, UNSIGNED, INVALID);
  br_4  = ISA_Reg_Opnd_Type_Create("br_4", ISA_REGCLASS_branch,
				   ISA_REGSUBCLASS_branch_4,
				   4, UNSIGNED, INVALID);
  br_8  = ISA_Reg_Opnd_Type_Create("br_8", ISA_REGCLASS_branch,
				   ISA_REGSUBCLASS_branch_8,
				   8, UNSIGNED, INVALID);
  br_16  = ISA_Reg_Opnd_Type_Create("br_16", ISA_REGCLASS_branch,
				   ISA_REGSUBCLASS_branch_16,
				   16, UNSIGNED, INVALID);

  sr_lbeg  = ISA_Reg_Opnd_Type_Create("sr_lbeg", ISA_REGCLASS_special,
				     ISA_REGSUBCLASS_LBEG,
				     32, UNSIGNED, INVALID);
  sr_lend  = ISA_Reg_Opnd_Type_Create("sr_lend", ISA_REGCLASS_special,
				     ISA_REGSUBCLASS_LEND,
				     32, UNSIGNED, INVALID);
  sr_lcount  = ISA_Reg_Opnd_Type_Create("sr_lcount", ISA_REGCLASS_special,
				      ISA_REGSUBCLASS_lcount,
				      32, UNSIGNED, INVALID);
  sr_sar  = ISA_Reg_Opnd_Type_Create("sr_sar", ISA_REGCLASS_special,
				     ISA_REGSUBCLASS_SAR,
				     32, UNSIGNED, INVALID);
  scompare1 = ISA_Reg_Opnd_Type_Create("scompare1", ISA_REGCLASS_special,
                                     ISA_REGSUBCLASS_SCOMPARE1,
                                     32, UNSIGNED, INVALID);

  /* Operand uses... */

  OPERAND_USE_TYPE
	  predicate,	// a qualifying predicate
	  base,		// a base address (for memory insts)
	  offset,	// an offset added to a base (implies immed val)
	  postincr,	// a post increment applied to a base address
	  trip_count,	// the trip_count of a loop
	  target,	// the target of a branch
	  loadval,	// value loaded
	  storeval,	// value to be stored
	  opnd1,	// first/left operand of an alu operator
	  opnd2,	// second/right operand of an alu operator
	  maddend;	// addend/subtrahend operand of a madd

  predicate  = Create_Operand_Use("predicate");
  offset     = Create_Operand_Use("offset");
  base       = Create_Operand_Use("base");
  postincr   = Create_Operand_Use("postincr");
  trip_count = Create_Operand_Use("trip_count");
  target     = Create_Operand_Use("target");
  loadval    = Create_Operand_Use("loadval");
  storeval   = Create_Operand_Use("storeval");
  opnd1      = Create_Operand_Use("opnd1");
  opnd2      = Create_Operand_Use("opnd2");
  maddend    = Create_Operand_Use("maddend");

  Instruction_Group("bst8",
		    TOP_ball, TOP_bany, TOP_bbc, TOP_bbs, TOP_beq, TOP_beqt,
		    TOP_bge, TOP_bgeu, TOP_blt, TOP_bltu, TOP_bnall,
		    TOP_bne, TOP_bnet, TOP_bnone,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, ar, opnd2);
  Operand(2, simm8pc, target);
  Relocatable(2);

  Instruction_Group("movi_n",
		    TOP_movi_n,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, simm7, opnd1);

  Instruction_Group("l8i",
		    TOP_l8ui,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, base);
  Operand(1, uimm8, offset);

  Instruction_Group("break_n",
		    TOP_break_n,
		    TOP_rfi,
		    TOP_rfdo,
		    TOP_waiti,
		    TOP_UNDEFINED);
  Operand(0, uimm4);

  Instruction_Group("bsi8",
		    TOP_beqi, TOP_bgei, TOP_blti, TOP_bnei,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, b4const, opnd2);
  Operand(2, simm8pc, target);
  Relocatable(2);

  Instruction_Group("exti",
		    TOP_extui,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);
  Operand(1, uimm5);
  Operand(2, op2p1);

  Instruction_Group("movi",
		    TOP_movi,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, simm12, opnd1);

  Instruction_Group("mov_n",
		    TOP_mov_n,
		    TOP_abs, TOP_neg,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);

  Instruction_Group("addmi",
		    TOP_addmi,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, simm8x256, opnd2);

  Instruction_Group("addi_n",
		    TOP_addi_n,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, ai4const, opnd2);

  Instruction_Group("nsa",
		    TOP_nsa, TOP_nsau,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);

  Instruction_Group("break",
		    TOP_break,
		    TOP_UNDEFINED);
  Operand(0, uimm4);
  Operand(1, uimm4);

  Instruction_Group("sar",
		    TOP_ssa8b, TOP_ssa8l, TOP_ssl, TOP_ssr,
		    TOP_UNDEFINED);
  Result(0, sr_sar);
  Operand(0, ar);

  Instruction_Group("callx",
		    TOP_callx0, TOP_callx4, TOP_callx8, TOP_callx12,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, target);

  Instruction_Group("storei4",
		    TOP_s32i_n,
		    TOP_UNDEFINED);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, lsi4x4, offset);

  Instruction_Group("movz",
		    TOP_moveqz, TOP_movgez, TOP_movltz, TOP_movnez,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);
  Operand(1, ar, opnd1);
  Operand(2, ar, opnd2);

  Instruction_Group("call4",
		    TOP_call0, TOP_call4, TOP_call8, TOP_call12,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, soffsetx4, target);
  Relocatable(0);

  Instruction_Group("entry",
		    TOP_entry,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);
  Operand(1, uimm12x8);

  Instruction_Group("movsp",
		    TOP_movsp,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);

  Instruction_Group("rsr",
		    TOP_rsr,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, sr);

  Instruction_Group("bsi8b",
		    TOP_bbci, TOP_bbsi,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, uimm5, opnd2);
  Operand(2, simm8pc, target);
  Relocatable(2);

  Instruction_Group("l32e",
		    TOP_l32e,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, base);
  Operand(1, neg_uimm8x4, offset);

  Instruction_Group("l32i",
		    TOP_l32i,
		    TOP_l32ai,
		    TOP_l32si,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, base);
  Operand(1, uimm8x4, offset);

  Instruction_Group("syscall",
		    TOP_simcall, TOP_syscall,
		    TOP_UNDEFINED);

  Instruction_Group("nop",
		    TOP_nop, TOP_nop_n,
		    TOP_UNDEFINED);

  Instruction_Group("l16i",
		    TOP_l16si, TOP_l16ui,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, base);
  Operand(1, uimm8x2, offset);

  Instruction_Group("loadi4",
		    TOP_l32i_n,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, base);
  Operand(1, lsi4x4, offset);

  Instruction_Group("l32r",
		    TOP_l32r,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, uimm16x4, base);
  Relocatable(0);

  Instruction_Group("sx",
		    TOP_sext,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, tp7, opnd2);

  Instruction_Group("bsi8u",
		    TOP_bgeui, TOP_bltui,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, b4constu, opnd2);
  Operand(2, simm8pc, target);
  Relocatable(2);

  Instruction_Group("sari",
		    TOP_ssai,
		    TOP_UNDEFINED);
  Result(0, sr_sar);
  Operand(0, uimm5);

  Instruction_Group("bbranch",
		    TOP_bf, TOP_bt,
		    TOP_UNDEFINED);
  Operand(0, br_1, opnd1);
  Operand(1, simm8pc, target);
  Relocatable(1);

  Instruction_Group("shifts",
		    TOP_sll,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, sr_sar, opnd2);

  Instruction_Group("shiftt",
		    TOP_sra, TOP_srl,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, sr_sar, opnd2);

  Instruction_Group("bz6",
		    TOP_beqz_n, TOP_bnez_n,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, uimm6pc, target);
  Relocatable(1);

  Instruction_Group("slli",
		    TOP_slli,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, msalp32, opnd2);

  Instruction_Group("s8i",
		    TOP_s8i,
		    TOP_UNDEFINED);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, uimm8, offset);

  Instruction_Group("jumpx",
		    TOP_jx,
		    TOP_UNDEFINED);
  Operand(0, ar, target);

  Instruction_Group("add_n",
		    TOP_add_n, TOP_max, TOP_maxu, TOP_min, TOP_minu, TOP_mul16s,
		    TOP_mul16u, TOP_and, TOP_or, TOP_xor, 
		    TOP_add, TOP_addx2, TOP_addx4, TOP_addx8, TOP_sub,
		    TOP_subx2, TOP_subx4, TOP_subx8,
		    TOP_quou, TOP_quos, TOP_remu, TOP_rems,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, ar, opnd2);

  Instruction_Group("src",
                  TOP_src,
                  TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, ar, opnd2);
  Operand(2, sr_sar);

  Instruction_Group("srli",
		    TOP_srli,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, uimm5, opnd2);  // actually uimm4 but assembler will convert to extui if nec.

  Instruction_Group("loop",
		    TOP_loop, TOP_loopgtz, TOP_loopnez,
		    TOP_UNDEFINED);
  Result(0, sr_lbeg);
  Result(1, sr_lend);
  Result(2, sr_lcount);
  Operand(0, ar, trip_count);
  Operand(1, uimm8pc, target);

  Instruction_Group("jump",
		    TOP_j,
		    TOP_UNDEFINED);
  Operand(0, soffset, target);
  Relocatable(0);

  Instruction_Group("sync",
		    TOP_dsync, TOP_esync, TOP_extw, TOP_extw_pseudo,
		    TOP_hwwitlba, TOP_hwwdtlba,
		    TOP_isync, TOP_memw, TOP_memw_pseudo, TOP_ldpte, TOP_rsync,
		    TOP_rfdd, TOP_rfde, TOP_rfe, TOP_rfme, TOP_rfue,
		    TOP_rfwo, TOP_rfwu,
		    TOP_UNDEFINED);

  Instruction_Group("illegal",
		    TOP_ill, TOP_ill_n,
		    TOP_UNDEFINED);

  Instruction_Group("bbool1",
		    TOP_andb, TOP_andbc, TOP_orb, TOP_orbc, TOP_xorb,
		    TOP_UNDEFINED);
  Result(0, br_1);
  Operand(0, br_1, opnd1);
  Operand(1, br_1, opnd2);

  Instruction_Group("return",
		    TOP_ret, TOP_retw, TOP_ret_n, TOP_retw_n,
		    TOP_UNDEFINED);
  Operand(0, ar);

  Instruction_Group("s32e",
		    TOP_s32e,
		    TOP_UNDEFINED);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, neg_uimm8x4, offset);

  Instruction_Group("s32i",
		    TOP_s32i,
		    TOP_s32ri,
		    TOP_UNDEFINED);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, uimm8x4, offset);

  Instruction_Group("s32c1i",
		    TOP_s32c1i,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, uimm8x4, offset);
  Operand(3, scompare1);

  Instruction_Group("bbool4",
		    TOP_all4, TOP_any4,
		    TOP_UNDEFINED);
  Result(0, br_1);
  Operand(0, br_4, opnd1);

  Instruction_Group("s16i",
		    TOP_s16i,
		    TOP_UNDEFINED);
  Operand(0, ar, storeval);
  Operand(1, ar, base);
  Operand(2, uimm8x2, offset);

  Instruction_Group("bbool8",
		    TOP_all8, TOP_any8,
		    TOP_UNDEFINED);
  Result(0, br_1);
  Operand(0, br_8, opnd1);

  Instruction_Group("clamp",
		    TOP_clamps,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, tp7, opnd2);

  Instruction_Group("const16",
		    TOP_const16,
		    TOP_const16lo,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, const16, opnd2);

  Instruction_Group("const16hi",
		    TOP_const16hi,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, const16, opnd1);

  Instruction_Group("srai",
		    TOP_srai,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, uimm5, opnd2);

  Instruction_Group("addi",
		    TOP_addi,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, simm8, opnd2);

  Instruction_Group("bsz12",
		    TOP_beqz, TOP_beqzt, TOP_bgez, TOP_bltz, 
		    TOP_bnez, TOP_bnezt,
		    TOP_UNDEFINED);
  Operand(0, ar, opnd1);
  Operand(1, simm12pc, target);
  Relocatable(1);

  Instruction_Group("excw",
		    TOP_excw,
		    TOP_UNDEFINED);

  Instruction_Group("bmove",
		    TOP_movf, TOP_movt,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);
  Operand(1, ar, opnd1);
  Operand(2, br_1, opnd2);

  Instruction_Group("wsr",
		    TOP_wsr,
		    TOP_UNDEFINED);
  Result(0, sr);
  Operand(0, ar);

  Instruction_Group("xsr",
		    TOP_xsr,
		    TOP_UNDEFINED);
  Result(0, ar);
  Result(1, sr);
  Operand(0, ar);
  Operand(1, sr);

  /* Pseudo instructions... */

  Instruction_Group("spadjust",
		    TOP_spadjust,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar, opnd1);
  Operand(1, simm32, opnd2);

  Instruction_Group("pregtn",
		    TOP_begin_pregtn, TOP_end_pregtn,
		    TOP_UNDEFINED);
  Operand(0, ar);
  Operand(1, simm16);

  Instruction_Group("intrncall",
		    TOP_intrncall,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("noop", 
		    TOP_fwd_bar, TOP_bwd_bar, TOP_noop,
		    TOP_UNDEFINED);

  Instruction_Group("asm",
		    TOP_asm,
		    TOP_UNDEFINED);

  Instruction_Group("label",
		    TOP_label,
		    TOP_UNDEFINED);
  Operand(0, simm32pc);
  Relocatable(0);

  Instruction_Group("loopend",
		    TOP_loop_end,
		    TOP_UNDEFINED);
  Result(0, sr_lcount);
  Operand(0, sr_lcount);
  Operand(1, uimm8pc, target);

  Instruction_Group("load_const",
		    TOP_load_const,
		    TOP_UNDEFINED);
  Result(0, ar, loadval);
  Operand(0, not_simm12, opnd1);

  Instruction_Group("br_to_ar",
		    TOP_br_to_ar,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, br_1, opnd1);

  Instruction_Group("br2_to_ar",
		    TOP_br2_to_ar,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, br_2, opnd1);

  Instruction_Group("br4_to_ar",
		    TOP_br4_to_ar,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, br_4, opnd1);

  Instruction_Group("br8_to_ar",
		    TOP_br8_to_ar,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, br_8, opnd1);

  Instruction_Group("br16_to_ar",
		    TOP_br16_to_ar,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, br_16, opnd1);

  Instruction_Group("ar_to_br",
		    TOP_ar_to_br,
		    TOP_UNDEFINED);
  Result(0, br_1);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("ar_to_br2",
		    TOP_ar_to_br2,
		    TOP_UNDEFINED);
  Result(0, br_2);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("ar_to_br4",
		    TOP_ar_to_br4,
		    TOP_UNDEFINED);
  Result(0, br_4);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("ar_to_br8",
		    TOP_ar_to_br8,
		    TOP_UNDEFINED);
  Result(0, br_8);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("ar_to_br16",
		    TOP_ar_to_br16,
		    TOP_UNDEFINED);
  Result(0, br_16);
  Operand(0, ar);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("get_tmp_ar",
		    TOP_get_tmp_ar,
		    TOP_UNDEFINED);
  Result(0, ar);

  Instruction_Group("movbr",
		    TOP_movbr,
		    TOP_UNDEFINED);
  Result(0, br_1);
  Operand(0, br_1);

  Instruction_Group("movbr2",
		    TOP_movbr2,
		    TOP_UNDEFINED);
  Result(0, br_2);
  Operand(0, br_2);

  Instruction_Group("movbr4",
		    TOP_movbr4,
		    TOP_UNDEFINED);
  Result(0, br_4);
  Operand(0, br_4);

  Instruction_Group("movbr8",
		    TOP_movbr8,
		    TOP_UNDEFINED);
  Result(0, br_8);
  Operand(0, br_8);
  Operand(1, ar);
  Operand(2, ar);

  Instruction_Group("movbr16",
		    TOP_movbr16,
		    TOP_UNDEFINED);
  Result(0, br_16);
  Operand(0, br_16);

  Instruction_Group("wsr_br",
		    TOP_wsr_br,
		    TOP_UNDEFINED);
  Result(0, sr);
  Result(1, br_1);
  Result(2, br_1);
  Result(3, br_1);
  Result(4, br_1);
  Result(5, br_1);
  Result(6, br_1);
  Result(7, br_1);
  Result(8, br_1);
  Result(9, br_1);
  Result(10, br_1);
  Result(11, br_1);
  Result(12, br_1);
  Result(13, br_1);
  Result(14, br_1);
  Result(15, br_1);
  Result(16, br_1);
  Operand(0, ar);

  Instruction_Group("rsr_br",
		    TOP_rsr_br,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, sr);
  Operand(1, br_1);
  Operand(2, br_1);
  Operand(3, br_1);
  Operand(4, br_1);
  Operand(5, br_1);
  Operand(6, br_1);
  Operand(7, br_1);
  Operand(8, br_1);
  Operand(9, br_1);
  Operand(10, br_1);
  Operand(11, br_1);
  Operand(12, br_1);
  Operand(13, br_1);
  Operand(14, br_1);
  Operand(15, br_1);
  Operand(16, br_1);

  Instruction_Group("chk",
		    TOP_chk,
		    TOP_idtlb, TOP_iitlb,
		    TOP_UNDEFINED);
  Operand(0, ar);

  Instruction_Group("cache240",
		    TOP_dhu,
                    TOP_diu,
                    TOP_diwb,
                    TOP_diwbi,
                    TOP_dpfl,
                    TOP_ihu,
                    TOP_iiu,
                    TOP_ipfl,
		    TOP_UNDEFINED);
  Operand(0, ar);
  Operand(1, uimm4x16, offset);

  Instruction_Group("cache1020",
		    TOP_dhi,
                    TOP_dhwb,
                    TOP_dhwbi,
                    TOP_dii,
                    TOP_dpfr,
                    TOP_dpfro,
                    TOP_dpfw,
                    TOP_dpfwo,
                    TOP_ihi,
                    TOP_iii,
                    TOP_ipf,
		    TOP_UNDEFINED);
  Operand(0, ar);
  Operand(1, uimm8x4, offset);

  Instruction_Group("inval",
		    TOP_inval,
		    TOP_ldct,
		    TOP_lict,
		    TOP_licw,
		    TOP_pdtlb,
		    TOP_pitlb,
		    TOP_rdtlb0,
		    TOP_rdtlb1,
		    TOP_ritlb0,
		    TOP_ritlb1,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, ar);

  Instruction_Group("sdct",
		    TOP_sdct,
		    TOP_sict,
		    TOP_sicw,
		    TOP_wdtlb,
		    TOP_witlb,
		    TOP_UNDEFINED);
  Operand(0, ar);
  Operand(1, ar);

  Instruction_Group("rotw",
		    TOP_rotw,
		    TOP_UNDEFINED);
  Operand(0, simm4);

  Instruction_Group("rsil",
		    TOP_rsil,
		    TOP_UNDEFINED);
  Result(0, ar);
  Operand(0, uimm4);

  ISA_Operands_End();
  return 0;
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
