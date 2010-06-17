/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `genflags'
from the machine description file `md'.  */

#define HAVE_adddf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_addsf3 (TARGET_HARD_FLOAT)
#define HAVE_addsi3 1
#define HAVE_addsi3_internal (! TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_adddi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_adddi3_internal_1 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_adddi3_internal_2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_adddi3_internal_3 (TARGET_64BIT \
   && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_addsi3_internal_2 (TARGET_64BIT \
   && !TARGET_MIPS16 \
   && (TARGET_GAS \
       || GET_CODE (operands[2]) != CONST_INT \
       || INTVAL (operands[2]) != -32768))
#define HAVE_subdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_subsf3 (TARGET_HARD_FLOAT)
#define HAVE_subsi3 1
#define HAVE_subsi3_internal (!TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_subdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_subdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_subdi3_internal_2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && INTVAL (operands[2]) != -32768)
#define HAVE_subdi3_internal_3 (TARGET_64BIT && !TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_subsi3_internal_2 (TARGET_64BIT && !TARGET_MIPS16 \
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
#define HAVE_muldf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_muldf3_internal (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu != PROCESSOR_R4300)
#define HAVE_muldf3_r4300 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu == PROCESSOR_R4300)
#define HAVE_mulsf3 (TARGET_HARD_FLOAT)
#define HAVE_mulsf3_internal (TARGET_HARD_FLOAT && mips_cpu != PROCESSOR_R4300)
#define HAVE_mulsf3_r4300 (TARGET_HARD_FLOAT && mips_cpu == PROCESSOR_R4300)
#define HAVE_mulsi3 1
#define HAVE_mulsi3_mult3 ((GENERATE_MULT3 \
    || TARGET_MIPS5400 /* CYGNUS LOCAL vr5400/raeburn */ \
    || TARGET_MAD) \
   && !0)
#define HAVE_mulsi3_internal (mips_cpu != PROCESSOR_R4000 || TARGET_MIPS16)
#define HAVE_mulsi3_r4000 (mips_cpu == PROCESSOR_R4000 && !TARGET_MIPS16)
#define HAVE_muldi3 (TARGET_64BIT && !0)
#define HAVE_muldi3_internal (TARGET_64BIT && mips_cpu != PROCESSOR_R4000 && !TARGET_MIPS16 && !0)
#define HAVE_muldi3_internal2 (TARGET_64BIT && (GENERATE_MULT3 || mips_cpu == PROCESSOR_R4000 || TARGET_MIPS16) && !0)
#define HAVE_mulsidi3 1
#define HAVE_umulsidi3 1
#define HAVE_mulsidi3_internal (!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_mulsidi3_64bit (TARGET_64BIT && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_smulsi3_highpart 1
#define HAVE_umulsi3_highpart 1
#define HAVE_xmulsi3_highpart_internal (! TARGET_MIPS5400 && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_xmulsi3_highpart_5400 (TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
#define HAVE_smuldi3_highpart (TARGET_64BIT && !0)
#define HAVE_umuldi3_highpart (TARGET_64BIT && !0)
#define HAVE_madsi (TARGET_MAD)
#define HAVE_divdf3 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_divsf3 (TARGET_HARD_FLOAT)
#define HAVE_divmodsi4 (optimize)
#define HAVE_divmodsi4_internal (optimize && !0)
#define HAVE_divmoddi4 (TARGET_64BIT && optimize && !0)
#define HAVE_divmoddi4_internal (TARGET_64BIT && optimize && !0)
#define HAVE_udivmodsi4 (optimize)
#define HAVE_udivmodsi4_internal (optimize && !0)
#define HAVE_udivmoddi4 (TARGET_64BIT && optimize && !0)
#define HAVE_udivmoddi4_internal (TARGET_64BIT && optimize && !0)
#define HAVE_div_trap 1
#define HAVE_div_trap_normal (!TARGET_MIPS16)
#define HAVE_div_trap_mips16 (TARGET_MIPS16)
#define HAVE_divsi3 (!optimize)
#define HAVE_divsi3_internal (!optimize)
#define HAVE_divdi3 (TARGET_64BIT && !optimize && !0)
#define HAVE_divdi3_internal (TARGET_64BIT && !optimize && !0)
#define HAVE_modsi3 (!optimize)
#define HAVE_modsi3_internal (!optimize)
#define HAVE_moddi3 (TARGET_64BIT && !optimize && !0)
#define HAVE_moddi3_internal (TARGET_64BIT && !optimize && !0)
#define HAVE_udivsi3 (!optimize)
#define HAVE_udivsi3_internal (!optimize)
#define HAVE_udivdi3 (TARGET_64BIT && !optimize && !0)
#define HAVE_udivdi3_internal (TARGET_64BIT && !optimize && !0)
#define HAVE_umodsi3 (!optimize)
#define HAVE_umodsi3_internal (!optimize)
#define HAVE_umoddi3 (TARGET_64BIT && !optimize && !0)
#define HAVE_umoddi3_internal (TARGET_64BIT && !optimize && !0)
#define HAVE_sqrtdf2 (TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT)
#define HAVE_sqrtsf2 (TARGET_HARD_FLOAT && HAVE_SQRT_P())
#define HAVE_abssi2 (!TARGET_MIPS16)
#define HAVE_absdi2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_absdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_abssf2 (TARGET_HARD_FLOAT)
#define HAVE_ffssi2 (!TARGET_MIPS16)
#define HAVE_ffsdi2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_negsi2 1
#define HAVE_negdi2 ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16)
#define HAVE_negdi2_internal (! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_negdi2_internal_2 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_negdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_negsf2 (TARGET_HARD_FLOAT)
#define HAVE_one_cmplsi2 1
#define HAVE_one_cmpldi2 1
#define HAVE_andsi3 1
#define HAVE_anddi3 (TARGET_64BIT || !TARGET_DEBUG_G_MODE)
#define HAVE_anddi3_internal1 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_iorsi3 1
#define HAVE_iordi3 (TARGET_64BIT || !TARGET_DEBUG_G_MODE)
#define HAVE_xorsi3 1
#define HAVE_xordi3 (TARGET_64BIT || !TARGET_DEBUG_G_MODE)
#define HAVE_xordi3_immed (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_truncdfsf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_truncdisi2 (TARGET_64BIT)
#define HAVE_truncdihi2 (TARGET_64BIT)
#define HAVE_truncdiqi2 (TARGET_64BIT)
#define HAVE_zero_extendsidi2 (TARGET_64BIT)
#define HAVE_zero_extendsidi2_internal (TARGET_64BIT)
#define HAVE_zero_extendhisi2 1
#define HAVE_zero_extendhidi2 (TARGET_64BIT)
#define HAVE_zero_extendqihi2 1
#define HAVE_zero_extendqisi2 1
#define HAVE_zero_extendqidi2 (TARGET_64BIT)
#define HAVE_extendsidi2 (TARGET_64BIT)
#define HAVE_extendhidi2 (TARGET_64BIT)
#define HAVE_extendhidi2_internal (TARGET_64BIT)
#define HAVE_extendhisi2 1
#define HAVE_extendhisi2_internal 1
#define HAVE_extendqihi2 1
#define HAVE_extendqihi2_internal 1
#define HAVE_extendqisi2 1
#define HAVE_extendqisi2_insn 1
#define HAVE_extendqidi2 (TARGET_64BIT)
#define HAVE_extendqidi2_insn (TARGET_64BIT)
#define HAVE_extendsfdf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfsi2 (TARGET_HARD_FLOAT && !0)
#define HAVE_fix_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_fix_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsidf2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_floatdidf2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_floatsisf2 (TARGET_HARD_FLOAT)
#define HAVE_floatdisf2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncdfsi2 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncdfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_fixuns_truncsfsi2 (TARGET_HARD_FLOAT && !0)
#define HAVE_fixuns_truncsfdi2 (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
#define HAVE_extv (!TARGET_MIPS16)
#define HAVE_extzv (!TARGET_MIPS16)
#define HAVE_insv (!TARGET_MIPS16)
#define HAVE_movsi_ulw (!TARGET_MIPS16)
#define HAVE_movsi_usw (!TARGET_MIPS16)
#define HAVE_movdi_uld 1
#define HAVE_movdi_usd 1
#define HAVE_high (mips_split_addresses && !TARGET_MIPS16)
#define HAVE_low (mips_split_addresses && !TARGET_MIPS16)
#define HAVE_movdi 1
#define HAVE_movdi_internal (!TARGET_64BIT && !TARGET_MIPS16 \
   && (register_operand (operands[0], DImode) \
       || register_operand (operands[1], DImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DImode)))
#define HAVE_movdi_internal2 (TARGET_64BIT && !TARGET_MIPS16 \
   && (register_operand (operands[0], DImode) \
       || se_register_operand (operands[1], DImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DImode)))
#define HAVE_reload_indi (TARGET_64BIT)
#define HAVE_reload_outdi (TARGET_64BIT)
#define HAVE_movsi 1
#define HAVE_movsi_internal1 (TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], SImode) \
       || register_operand (operands[1], SImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movsi_internal2 (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], SImode) \
       || register_operand (operands[1], SImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_reload_outsi (TARGET_64BIT || TARGET_MIPS16)
#define HAVE_reload_insi (TARGET_MIPS16)
#define HAVE_movcc (mips_isa >= 4 && TARGET_HARD_FLOAT)
#define HAVE_reload_incc (mips_isa >= 4 && TARGET_HARD_FLOAT)
#define HAVE_reload_outcc (mips_isa >= 4 && TARGET_HARD_FLOAT)
#define HAVE_movhi 1
#define HAVE_movhi_internal1 (TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], HImode) \
       || register_operand (operands[1], HImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movhi_internal2 (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], HImode) \
       || register_operand (operands[1], HImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movqi 1
#define HAVE_movqi_internal1 (TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], QImode) \
       || register_operand (operands[1], QImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movqi_internal2 (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16 \
   && (register_operand (operands[0], QImode) \
       || register_operand (operands[1], QImode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
#define HAVE_movsf 1
#define HAVE_movsf_internal1 (TARGET_HARD_FLOAT \
   && (register_operand (operands[0], SFmode) \
       || register_operand (operands[1], SFmode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (SFmode)))
#define HAVE_movsf_internal2 (TARGET_SOFT_FLOAT && !TARGET_MIPS16 \
   && (register_operand (operands[0], SFmode) \
       || register_operand (operands[1], SFmode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (SFmode)))
#define HAVE_movdf 1
#define HAVE_movdf_internal1 (TARGET_HARD_FLOAT && !(TARGET_FLOAT64 && !TARGET_64BIT) \
   && TARGET_DOUBLE_FLOAT \
   && (register_operand (operands[0], DFmode) \
       || register_operand (operands[1], DFmode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DFmode)))
#define HAVE_movdf_internal1a (TARGET_HARD_FLOAT && (TARGET_FLOAT64 && !TARGET_64BIT) \
   && TARGET_DOUBLE_FLOAT \
   && (register_operand (operands[0], DFmode) \
       || register_operand (operands[1], DFmode) \
       || (GET_CODE (operands [0]) == MEM \
	   && ((GET_CODE (operands[1]) == CONST_INT \
		&& INTVAL (operands[1]) == 0) \
	       || operands[1] == CONST0_RTX (DFmode)))))
#define HAVE_movdf_internal2 ((TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16 \
   && (register_operand (operands[0], DFmode) \
       || register_operand (operands[1], DFmode) \
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0) \
       || operands[1] == CONST0_RTX (DFmode)))
#define HAVE_loadgp 1
#define HAVE_movstrsi (!TARGET_MIPS16)
#define HAVE_movstrsi_internal 1
#define HAVE_movstrsi_internal2 1
#define HAVE_movstrsi_internal3 1
#define HAVE_ashlsi3 1
#define HAVE_ashlsi3_internal1 (!TARGET_MIPS16)
#define HAVE_ashlsi3_internal2 (TARGET_MIPS16)
#define HAVE_ashldi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_ashldi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_ashldi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_ashldi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_ashldi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_ashrsi3 1
#define HAVE_ashrsi3_internal1 (!TARGET_MIPS16)
#define HAVE_ashrsi3_internal2 (TARGET_MIPS16)
#define HAVE_ashrdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_ashrdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_ashrdi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_ashrdi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_ashrdi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_lshrsi3 1
#define HAVE_lshrsi3_internal1 (!TARGET_MIPS16)
#define HAVE_lshrsi3_internal2 (TARGET_MIPS16)
#define HAVE_lshrdi3 (TARGET_64BIT || (!TARGET_DEBUG_G_MODE && !TARGET_MIPS16))
#define HAVE_lshrdi3_internal (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
#define HAVE_lshrdi3_internal2 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 32) != 0)
#define HAVE_lshrdi3_internal3 (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16 \
   && (INTVAL (operands[2]) & 63) < 32 \
   && (INTVAL (operands[2]) & 63) != 0)
#define HAVE_lshrdi3_internal4 (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_rotrsi3 (TARGET_MIPS5400)
#define HAVE_rotrdi3 (TARGET_MIPS5400 && TARGET_64BIT)
#define HAVE_cmpsi 1
#define HAVE_tstsi 1
#define HAVE_cmpdi (TARGET_64BIT)
#define HAVE_tstdi (TARGET_64BIT)
#define HAVE_cmpdf (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_cmpsf (TARGET_HARD_FLOAT)
#define HAVE_branch_fp_ne (TARGET_HARD_FLOAT)
#define HAVE_branch_fp_eq (TARGET_HARD_FLOAT)
#define HAVE_branch_zero (!TARGET_MIPS16)
#define HAVE_branch_zero_di (!TARGET_MIPS16)
#define HAVE_branch_equality (!TARGET_MIPS16)
#define HAVE_branch_equality_di (!TARGET_MIPS16)
#define HAVE_beq 1
#define HAVE_bne 1
#define HAVE_bgt 1
#define HAVE_bge 1
#define HAVE_blt 1
#define HAVE_ble 1
#define HAVE_bgtu 1
#define HAVE_bgeu 1
#define HAVE_bltu 1
#define HAVE_bleu 1
#define HAVE_seq 1
#define HAVE_seq_si_zero (!TARGET_MIPS16)
#define HAVE_seq_di_zero (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_seq_si (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_seq_di (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sne (!TARGET_MIPS16)
#define HAVE_sne_si_zero (!TARGET_MIPS16)
#define HAVE_sne_di_zero (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sne_si (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sne_di (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sgt 1
#define HAVE_sgt_si (!TARGET_MIPS16)
#define HAVE_sgt_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sge 1
#define HAVE_sge_si (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sge_di (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_slt 1
#define HAVE_slt_si (!TARGET_MIPS16)
#define HAVE_slt_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sle 1
#define HAVE_sle_si_const (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sle_di_const (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sle_si_reg (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sle_di_reg (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sgtu 1
#define HAVE_sgtu_si 1
#define HAVE_sgtu_di (TARGET_64BIT)
#define HAVE_sgeu 1
#define HAVE_sgeu_si (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sgeu_di (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sltu 1
#define HAVE_sltu_si (!TARGET_MIPS16)
#define HAVE_sltu_di (TARGET_64BIT && !TARGET_MIPS16)
#define HAVE_sleu 1
#define HAVE_sleu_si_const (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sleu_di_const (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
#define HAVE_sleu_si_reg (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_sleu_di_reg (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
#define HAVE_seq_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_slt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sle_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sgt_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_sge_df (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_seq_sf (TARGET_HARD_FLOAT)
#define HAVE_slt_sf (TARGET_HARD_FLOAT)
#define HAVE_sle_sf (TARGET_HARD_FLOAT)
#define HAVE_sgt_sf (TARGET_HARD_FLOAT)
#define HAVE_sge_sf (TARGET_HARD_FLOAT)
#define HAVE_jump (!TARGET_MIPS16)
#define HAVE_indirect_jump 1
#define HAVE_indirect_jump_internal1 (!(Pmode == DImode))
#define HAVE_indirect_jump_internal2 (Pmode == DImode)
#define HAVE_tablejump 1
#define HAVE_tablejump_internal1 (!(Pmode == DImode))
#define HAVE_tablejump_internal2 (Pmode == DImode)
#define HAVE_tablejump_internal3 1
#define HAVE_tablejump_mips161 (TARGET_MIPS16 && !(Pmode == DImode))
#define HAVE_tablejump_mips162 (TARGET_MIPS16 && Pmode == DImode)
#define HAVE_tablejump_internal4 1
#define HAVE_casesi (TARGET_EMBEDDED_PIC)
#define HAVE_casesi_internal (TARGET_EMBEDDED_PIC)
#define HAVE_builtin_setjmp_setup (TARGET_ABICALLS)
#define HAVE_builtin_setjmp_setup_32 (TARGET_ABICALLS && ! (Pmode == DImode))
#define HAVE_builtin_setjmp_setup_64 (TARGET_ABICALLS && Pmode == DImode)
#define HAVE_builtin_longjmp (TARGET_ABICALLS)
#define HAVE_prologue 1
#define HAVE_blockage 1
#define HAVE_epilogue 1
#define HAVE_sibcall_epilogue (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_return (mips_can_use_return_insn ())
#define HAVE_return_internal 1
#define HAVE_get_fnaddr (TARGET_EMBEDDED_PIC \
   && GET_CODE (operands[1]) == SYMBOL_REF)
#define HAVE_call 1
#define HAVE_call_internal0 1
#define HAVE_call_internal1 (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_internal3a (!TARGET_MIPS16 \
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal3b (!TARGET_MIPS16 \
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal3c (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS \
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31)
#define HAVE_call_internal4a (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_internal4b (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_sibcall 1
#define HAVE_sibcall_internal0 (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_sibcall_internal1 (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value 1
#define HAVE_call_value_internal0 1
#define HAVE_call_value_internal1 (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_internal3a (!TARGET_MIPS16  \
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal3b (!TARGET_MIPS16  \
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal3c (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS \
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31)
#define HAVE_call_value_internal4a (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_call_value_internal4b (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
#define HAVE_sibcall_value 1
#define HAVE_sibcall_value_internal0 1
#define HAVE_sibcall_value_internal1 (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_call_value_multiple_internal0 1
#define HAVE_call_value_multiple_internal2 (TARGET_ABICALLS && !TARGET_LONG_CALLS)
#define HAVE_untyped_call 1
#define HAVE_nop 1
#define HAVE_movsicc (mips_isa >= 4 || 0)
#define HAVE_movdicc (mips_isa >= 4 || 0)
#define HAVE_movsfcc (mips_isa >= 4 && TARGET_HARD_FLOAT)
#define HAVE_movdfcc (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
#define HAVE_consttable_qi (TARGET_MIPS16)
#define HAVE_consttable_hi (TARGET_MIPS16)
#define HAVE_consttable_si (TARGET_MIPS16)
#define HAVE_consttable_di (TARGET_MIPS16)
#define HAVE_consttable_sf (TARGET_MIPS16)
#define HAVE_consttable_df (TARGET_MIPS16)
#define HAVE_align_2 (TARGET_MIPS16)
#define HAVE_align_4 (TARGET_MIPS16)
#define HAVE_align_8 (TARGET_MIPS16)
#define HAVE_leasi (Pmode == SImode)
#define HAVE_leadi (Pmode == DImode)

#ifndef NO_MD_PROTOTYPES
extern rtx gen_adddf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_addsf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_addsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_addsi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_adddi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_adddi3_internal_1             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_adddi3_internal_2             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_adddi3_internal_3             PROTO((rtx, rtx, rtx));
extern rtx gen_addsi3_internal_2             PROTO((rtx, rtx, rtx));
extern rtx gen_subdf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_subsf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_subsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_subsi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_subdi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_subdi3_internal               PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_subdi3_internal_2             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_subdi3_internal_3             PROTO((rtx, rtx, rtx));
extern rtx gen_subsi3_internal_2             PROTO((rtx, rtx, rtx));
extern rtx gen_muldf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_muldf3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_muldf3_r4300                  PROTO((rtx, rtx, rtx));
extern rtx gen_mulsf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_mulsf3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_mulsf3_r4300                  PROTO((rtx, rtx, rtx));
extern rtx gen_mulsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_mulsi3_mult3                  PROTO((rtx, rtx, rtx));
extern rtx gen_mulsi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_mulsi3_r4000                  PROTO((rtx, rtx, rtx));
extern rtx gen_muldi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_muldi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_muldi3_internal2              PROTO((rtx, rtx, rtx));
extern rtx gen_mulsidi3                      PROTO((rtx, rtx, rtx));
extern rtx gen_umulsidi3                     PROTO((rtx, rtx, rtx));
extern rtx gen_mulsidi3_internal             PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_mulsidi3_64bit                PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_smulsi3_highpart              PROTO((rtx, rtx, rtx));
extern rtx gen_umulsi3_highpart              PROTO((rtx, rtx, rtx));
extern rtx gen_xmulsi3_highpart_internal     PROTO((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx gen_xmulsi3_highpart_5400         PROTO((rtx, rtx, rtx, rtx, rtx, rtx));
extern rtx gen_smuldi3_highpart              PROTO((rtx, rtx, rtx));
extern rtx gen_umuldi3_highpart              PROTO((rtx, rtx, rtx));
extern rtx gen_madsi                         PROTO((rtx, rtx, rtx));
extern rtx gen_divdf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_divsf3                        PROTO((rtx, rtx, rtx));
extern rtx gen_divmodsi4                     PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_divmodsi4_internal            PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_divmoddi4                     PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_divmoddi4_internal            PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_udivmodsi4                    PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_udivmodsi4_internal           PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_udivmoddi4                    PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_udivmoddi4_internal           PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_div_trap                      PROTO((rtx, rtx, rtx));
extern rtx gen_div_trap_normal               PROTO((rtx, rtx, rtx));
extern rtx gen_div_trap_mips16               PROTO((rtx, rtx, rtx));
extern rtx gen_divsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_divsi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_divdi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_divdi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_modsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_modsi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_moddi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_moddi3_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_udivsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_udivsi3_internal              PROTO((rtx, rtx, rtx));
extern rtx gen_udivdi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_udivdi3_internal              PROTO((rtx, rtx, rtx));
extern rtx gen_umodsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_umodsi3_internal              PROTO((rtx, rtx, rtx));
extern rtx gen_umoddi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_umoddi3_internal              PROTO((rtx, rtx, rtx));
extern rtx gen_sqrtdf2                       PROTO((rtx, rtx));
extern rtx gen_sqrtsf2                       PROTO((rtx, rtx));
extern rtx gen_abssi2                        PROTO((rtx, rtx));
extern rtx gen_absdi2                        PROTO((rtx, rtx));
extern rtx gen_absdf2                        PROTO((rtx, rtx));
extern rtx gen_abssf2                        PROTO((rtx, rtx));
extern rtx gen_ffssi2                        PROTO((rtx, rtx));
extern rtx gen_ffsdi2                        PROTO((rtx, rtx));
extern rtx gen_negsi2                        PROTO((rtx, rtx));
extern rtx gen_negdi2                        PROTO((rtx, rtx));
extern rtx gen_negdi2_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_negdi2_internal_2             PROTO((rtx, rtx));
extern rtx gen_negdf2                        PROTO((rtx, rtx));
extern rtx gen_negsf2                        PROTO((rtx, rtx));
extern rtx gen_one_cmplsi2                   PROTO((rtx, rtx));
extern rtx gen_one_cmpldi2                   PROTO((rtx, rtx));
extern rtx gen_andsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_anddi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_anddi3_internal1              PROTO((rtx, rtx, rtx));
extern rtx gen_iorsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_iordi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_xorsi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_xordi3                        PROTO((rtx, rtx, rtx));
extern rtx gen_xordi3_immed                  PROTO((rtx, rtx, rtx));
extern rtx gen_truncdfsf2                    PROTO((rtx, rtx));
extern rtx gen_truncdisi2                    PROTO((rtx, rtx));
extern rtx gen_truncdihi2                    PROTO((rtx, rtx));
extern rtx gen_truncdiqi2                    PROTO((rtx, rtx));
extern rtx gen_zero_extendsidi2              PROTO((rtx, rtx));
extern rtx gen_zero_extendsidi2_internal     PROTO((rtx, rtx));
extern rtx gen_zero_extendhisi2              PROTO((rtx, rtx));
extern rtx gen_zero_extendhidi2              PROTO((rtx, rtx));
extern rtx gen_zero_extendqihi2              PROTO((rtx, rtx));
extern rtx gen_zero_extendqisi2              PROTO((rtx, rtx));
extern rtx gen_zero_extendqidi2              PROTO((rtx, rtx));
extern rtx gen_extendsidi2                   PROTO((rtx, rtx));
extern rtx gen_extendhidi2                   PROTO((rtx, rtx));
extern rtx gen_extendhidi2_internal          PROTO((rtx, rtx));
extern rtx gen_extendhisi2                   PROTO((rtx, rtx));
extern rtx gen_extendhisi2_internal          PROTO((rtx, rtx));
extern rtx gen_extendqihi2                   PROTO((rtx, rtx));
extern rtx gen_extendqihi2_internal          PROTO((rtx, rtx));
extern rtx gen_extendqisi2                   PROTO((rtx, rtx));
extern rtx gen_extendqisi2_insn              PROTO((rtx, rtx));
extern rtx gen_extendqidi2                   PROTO((rtx, rtx));
extern rtx gen_extendqidi2_insn              PROTO((rtx, rtx));
extern rtx gen_extendsfdf2                   PROTO((rtx, rtx));
extern rtx gen_fix_truncdfsi2                PROTO((rtx, rtx));
extern rtx gen_fix_truncsfsi2                PROTO((rtx, rtx));
extern rtx gen_fix_truncdfdi2                PROTO((rtx, rtx));
extern rtx gen_fix_truncsfdi2                PROTO((rtx, rtx));
extern rtx gen_floatsidf2                    PROTO((rtx, rtx));
extern rtx gen_floatdidf2                    PROTO((rtx, rtx));
extern rtx gen_floatsisf2                    PROTO((rtx, rtx));
extern rtx gen_floatdisf2                    PROTO((rtx, rtx));
extern rtx gen_fixuns_truncdfsi2             PROTO((rtx, rtx));
extern rtx gen_fixuns_truncdfdi2             PROTO((rtx, rtx));
extern rtx gen_fixuns_truncsfsi2             PROTO((rtx, rtx));
extern rtx gen_fixuns_truncsfdi2             PROTO((rtx, rtx));
extern rtx gen_extv                          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_extzv                         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_insv                          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movsi_ulw                     PROTO((rtx, rtx));
extern rtx gen_movsi_usw                     PROTO((rtx, rtx));
extern rtx gen_movdi_uld                     PROTO((rtx, rtx));
extern rtx gen_movdi_usd                     PROTO((rtx, rtx));
extern rtx gen_high                          PROTO((rtx, rtx));
extern rtx gen_low                           PROTO((rtx, rtx, rtx));
extern rtx gen_movdi                         PROTO((rtx, rtx));
extern rtx gen_movdi_internal                PROTO((rtx, rtx));
extern rtx gen_movdi_internal2               PROTO((rtx, rtx));
extern rtx gen_reload_indi                   PROTO((rtx, rtx, rtx));
extern rtx gen_reload_outdi                  PROTO((rtx, rtx, rtx));
extern rtx gen_movsi                         PROTO((rtx, rtx));
extern rtx gen_movsi_internal1               PROTO((rtx, rtx));
extern rtx gen_movsi_internal2               PROTO((rtx, rtx));
extern rtx gen_reload_outsi                  PROTO((rtx, rtx, rtx));
extern rtx gen_reload_insi                   PROTO((rtx, rtx, rtx));
extern rtx gen_movcc                         PROTO((rtx, rtx));
extern rtx gen_reload_incc                   PROTO((rtx, rtx, rtx));
extern rtx gen_reload_outcc                  PROTO((rtx, rtx, rtx));
extern rtx gen_movhi                         PROTO((rtx, rtx));
extern rtx gen_movhi_internal1               PROTO((rtx, rtx));
extern rtx gen_movhi_internal2               PROTO((rtx, rtx));
extern rtx gen_movqi                         PROTO((rtx, rtx));
extern rtx gen_movqi_internal1               PROTO((rtx, rtx));
extern rtx gen_movqi_internal2               PROTO((rtx, rtx));
extern rtx gen_movsf                         PROTO((rtx, rtx));
extern rtx gen_movsf_internal1               PROTO((rtx, rtx));
extern rtx gen_movsf_internal2               PROTO((rtx, rtx));
extern rtx gen_movdf                         PROTO((rtx, rtx));
extern rtx gen_movdf_internal1               PROTO((rtx, rtx));
extern rtx gen_movdf_internal1a              PROTO((rtx, rtx));
extern rtx gen_movdf_internal2               PROTO((rtx, rtx));
extern rtx gen_loadgp                        PROTO((rtx, rtx));
extern rtx gen_movstrsi                      PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movstrsi_internal             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movstrsi_internal2            PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movstrsi_internal3            PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashlsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_ashlsi3_internal1             PROTO((rtx, rtx, rtx));
extern rtx gen_ashlsi3_internal2             PROTO((rtx, rtx, rtx));
extern rtx gen_ashldi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_ashldi3_internal              PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashldi3_internal2             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashldi3_internal3             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashldi3_internal4             PROTO((rtx, rtx, rtx));
extern rtx gen_ashrsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_ashrsi3_internal1             PROTO((rtx, rtx, rtx));
extern rtx gen_ashrsi3_internal2             PROTO((rtx, rtx, rtx));
extern rtx gen_ashrdi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_ashrdi3_internal              PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashrdi3_internal2             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashrdi3_internal3             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_ashrdi3_internal4             PROTO((rtx, rtx, rtx));
extern rtx gen_lshrsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_lshrsi3_internal1             PROTO((rtx, rtx, rtx));
extern rtx gen_lshrsi3_internal2             PROTO((rtx, rtx, rtx));
extern rtx gen_lshrdi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_lshrdi3_internal              PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_lshrdi3_internal2             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_lshrdi3_internal3             PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_lshrdi3_internal4             PROTO((rtx, rtx, rtx));
extern rtx gen_rotrsi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_rotrdi3                       PROTO((rtx, rtx, rtx));
extern rtx gen_cmpsi                         PROTO((rtx, rtx));
extern rtx gen_tstsi                         PROTO((rtx));
extern rtx gen_cmpdi                         PROTO((rtx, rtx));
extern rtx gen_tstdi                         PROTO((rtx));
extern rtx gen_cmpdf                         PROTO((rtx, rtx));
extern rtx gen_cmpsf                         PROTO((rtx, rtx));
extern rtx gen_branch_fp_ne                  PROTO((rtx, rtx, rtx));
extern rtx gen_branch_fp_eq                  PROTO((rtx, rtx, rtx));
extern rtx gen_branch_zero                   PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_branch_zero_di                PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_branch_equality               PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_branch_equality_di            PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_beq                           PROTO((rtx));
extern rtx gen_bne                           PROTO((rtx));
extern rtx gen_bgt                           PROTO((rtx));
extern rtx gen_bge                           PROTO((rtx));
extern rtx gen_blt                           PROTO((rtx));
extern rtx gen_ble                           PROTO((rtx));
extern rtx gen_bgtu                          PROTO((rtx));
extern rtx gen_bgeu                          PROTO((rtx));
extern rtx gen_bltu                          PROTO((rtx));
extern rtx gen_bleu                          PROTO((rtx));
extern rtx gen_seq                           PROTO((rtx));
extern rtx gen_seq_si_zero                   PROTO((rtx, rtx));
extern rtx gen_seq_di_zero                   PROTO((rtx, rtx));
extern rtx gen_seq_si                        PROTO((rtx, rtx, rtx));
extern rtx gen_seq_di                        PROTO((rtx, rtx, rtx));
extern rtx gen_sne                           PROTO((rtx));
extern rtx gen_sne_si_zero                   PROTO((rtx, rtx));
extern rtx gen_sne_di_zero                   PROTO((rtx, rtx));
extern rtx gen_sne_si                        PROTO((rtx, rtx, rtx));
extern rtx gen_sne_di                        PROTO((rtx, rtx, rtx));
extern rtx gen_sgt                           PROTO((rtx));
extern rtx gen_sgt_si                        PROTO((rtx, rtx, rtx));
extern rtx gen_sgt_di                        PROTO((rtx, rtx, rtx));
extern rtx gen_sge                           PROTO((rtx));
extern rtx gen_sge_si                        PROTO((rtx, rtx, rtx));
extern rtx gen_sge_di                        PROTO((rtx, rtx, rtx));
extern rtx gen_slt                           PROTO((rtx));
extern rtx gen_slt_si                        PROTO((rtx, rtx, rtx));
extern rtx gen_slt_di                        PROTO((rtx, rtx, rtx));
extern rtx gen_sle                           PROTO((rtx));
extern rtx gen_sle_si_const                  PROTO((rtx, rtx, rtx));
extern rtx gen_sle_di_const                  PROTO((rtx, rtx, rtx));
extern rtx gen_sle_si_reg                    PROTO((rtx, rtx, rtx));
extern rtx gen_sle_di_reg                    PROTO((rtx, rtx, rtx));
extern rtx gen_sgtu                          PROTO((rtx));
extern rtx gen_sgtu_si                       PROTO((rtx, rtx, rtx));
extern rtx gen_sgtu_di                       PROTO((rtx, rtx, rtx));
extern rtx gen_sgeu                          PROTO((rtx));
extern rtx gen_sgeu_si                       PROTO((rtx, rtx, rtx));
extern rtx gen_sgeu_di                       PROTO((rtx, rtx, rtx));
extern rtx gen_sltu                          PROTO((rtx));
extern rtx gen_sltu_si                       PROTO((rtx, rtx, rtx));
extern rtx gen_sltu_di                       PROTO((rtx, rtx, rtx));
extern rtx gen_sleu                          PROTO((rtx));
extern rtx gen_sleu_si_const                 PROTO((rtx, rtx, rtx));
extern rtx gen_sleu_di_const                 PROTO((rtx, rtx, rtx));
extern rtx gen_sleu_si_reg                   PROTO((rtx, rtx, rtx));
extern rtx gen_sleu_di_reg                   PROTO((rtx, rtx, rtx));
extern rtx gen_seq_df                        PROTO((rtx, rtx, rtx));
extern rtx gen_slt_df                        PROTO((rtx, rtx, rtx));
extern rtx gen_sle_df                        PROTO((rtx, rtx, rtx));
extern rtx gen_sgt_df                        PROTO((rtx, rtx, rtx));
extern rtx gen_sge_df                        PROTO((rtx, rtx, rtx));
extern rtx gen_seq_sf                        PROTO((rtx, rtx, rtx));
extern rtx gen_slt_sf                        PROTO((rtx, rtx, rtx));
extern rtx gen_sle_sf                        PROTO((rtx, rtx, rtx));
extern rtx gen_sgt_sf                        PROTO((rtx, rtx, rtx));
extern rtx gen_sge_sf                        PROTO((rtx, rtx, rtx));
extern rtx gen_jump                          PROTO((rtx));
extern rtx gen_indirect_jump                 PROTO((rtx));
extern rtx gen_indirect_jump_internal1       PROTO((rtx));
extern rtx gen_indirect_jump_internal2       PROTO((rtx));
extern rtx gen_tablejump                     PROTO((rtx, rtx));
extern rtx gen_tablejump_internal1           PROTO((rtx, rtx));
extern rtx gen_tablejump_internal2           PROTO((rtx, rtx));
extern rtx gen_tablejump_internal3           PROTO((rtx, rtx));
extern rtx gen_tablejump_mips161             PROTO((rtx, rtx));
extern rtx gen_tablejump_mips162             PROTO((rtx, rtx));
extern rtx gen_tablejump_internal4           PROTO((rtx, rtx));
extern rtx gen_casesi                        PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_casesi_internal               PROTO((rtx, rtx, rtx));
extern rtx gen_builtin_setjmp_setup          PROTO((rtx));
extern rtx gen_builtin_setjmp_setup_32       PROTO((rtx));
extern rtx gen_builtin_setjmp_setup_64       PROTO((rtx));
extern rtx gen_builtin_longjmp               PROTO((rtx));
extern rtx gen_prologue                      PROTO((void));
extern rtx gen_blockage                      PROTO((void));
extern rtx gen_epilogue                      PROTO((void));
extern rtx gen_sibcall_epilogue              PROTO((void));
extern rtx gen_return                        PROTO((void));
extern rtx gen_return_internal               PROTO((rtx));
extern rtx gen_get_fnaddr                    PROTO((rtx, rtx));
extern rtx gen_call_internal0                PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal1                PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal2                PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal3a               PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal3b               PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal3c               PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal4a               PROTO((rtx, rtx, rtx));
extern rtx gen_call_internal4b               PROTO((rtx, rtx, rtx));
extern rtx gen_sibcall_internal0             PROTO((rtx, rtx));
extern rtx gen_sibcall_internal1             PROTO((rtx, rtx));
extern rtx gen_call_value_internal0          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal1          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal2          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal3a         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal3b         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal3c         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal4a         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value_internal4b         PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_sibcall_value_internal0       PROTO((rtx, rtx, rtx));
extern rtx gen_sibcall_value_internal1       PROTO((rtx, rtx, rtx));
extern rtx gen_call_value_multiple_internal0 PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_call_value_multiple_internal2 PROTO((rtx, rtx, rtx, rtx, rtx));
extern rtx gen_untyped_call                  PROTO((rtx, rtx, rtx));
extern rtx gen_nop                           PROTO((void));
extern rtx gen_movsicc                       PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movdicc                       PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movsfcc                       PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_movdfcc                       PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_consttable_qi                 PROTO((rtx));
extern rtx gen_consttable_hi                 PROTO((rtx));
extern rtx gen_consttable_si                 PROTO((rtx));
extern rtx gen_consttable_di                 PROTO((rtx));
extern rtx gen_consttable_sf                 PROTO((rtx));
extern rtx gen_consttable_df                 PROTO((rtx));
extern rtx gen_align_2                       PROTO((void));
extern rtx gen_align_4                       PROTO((void));
extern rtx gen_align_8                       PROTO((void));
extern rtx gen_leasi                         PROTO((rtx, rtx));
extern rtx gen_leadi                         PROTO((rtx, rtx));

#ifdef MD_CALL_PROTOTYPES
extern rtx gen_call                          PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_sibcall                       PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_call_value                    PROTO((rtx, rtx, rtx, rtx));
extern rtx gen_sibcall_value                 PROTO((rtx, rtx, rtx, rtx));

#else /* !MD_CALL_PROTOTYPES */
extern rtx gen_call ();
extern rtx gen_sibcall ();
extern rtx gen_call_value ();
extern rtx gen_sibcall_value ();
#endif /* !MD_CALL_PROTOTYPES */

#else  /* NO_MD_PROTOTYPES */
extern rtx gen_adddf3 ();
extern rtx gen_addsf3 ();
extern rtx gen_addsi3 ();
extern rtx gen_addsi3_internal ();
extern rtx gen_adddi3 ();
extern rtx gen_adddi3_internal_1 ();
extern rtx gen_adddi3_internal_2 ();
extern rtx gen_adddi3_internal_3 ();
extern rtx gen_addsi3_internal_2 ();
extern rtx gen_subdf3 ();
extern rtx gen_subsf3 ();
extern rtx gen_subsi3 ();
extern rtx gen_subsi3_internal ();
extern rtx gen_subdi3 ();
extern rtx gen_subdi3_internal ();
extern rtx gen_subdi3_internal_2 ();
extern rtx gen_subdi3_internal_3 ();
extern rtx gen_subsi3_internal_2 ();
extern rtx gen_muldf3 ();
extern rtx gen_muldf3_internal ();
extern rtx gen_muldf3_r4300 ();
extern rtx gen_mulsf3 ();
extern rtx gen_mulsf3_internal ();
extern rtx gen_mulsf3_r4300 ();
extern rtx gen_mulsi3 ();
extern rtx gen_mulsi3_mult3 ();
extern rtx gen_mulsi3_internal ();
extern rtx gen_mulsi3_r4000 ();
extern rtx gen_muldi3 ();
extern rtx gen_muldi3_internal ();
extern rtx gen_muldi3_internal2 ();
extern rtx gen_mulsidi3 ();
extern rtx gen_umulsidi3 ();
extern rtx gen_mulsidi3_internal ();
extern rtx gen_mulsidi3_64bit ();
extern rtx gen_smulsi3_highpart ();
extern rtx gen_umulsi3_highpart ();
extern rtx gen_xmulsi3_highpart_internal ();
extern rtx gen_xmulsi3_highpart_5400 ();
extern rtx gen_smuldi3_highpart ();
extern rtx gen_umuldi3_highpart ();
extern rtx gen_madsi ();
extern rtx gen_divdf3 ();
extern rtx gen_divsf3 ();
extern rtx gen_divmodsi4 ();
extern rtx gen_divmodsi4_internal ();
extern rtx gen_divmoddi4 ();
extern rtx gen_divmoddi4_internal ();
extern rtx gen_udivmodsi4 ();
extern rtx gen_udivmodsi4_internal ();
extern rtx gen_udivmoddi4 ();
extern rtx gen_udivmoddi4_internal ();
extern rtx gen_div_trap ();
extern rtx gen_div_trap_normal ();
extern rtx gen_div_trap_mips16 ();
extern rtx gen_divsi3 ();
extern rtx gen_divsi3_internal ();
extern rtx gen_divdi3 ();
extern rtx gen_divdi3_internal ();
extern rtx gen_modsi3 ();
extern rtx gen_modsi3_internal ();
extern rtx gen_moddi3 ();
extern rtx gen_moddi3_internal ();
extern rtx gen_udivsi3 ();
extern rtx gen_udivsi3_internal ();
extern rtx gen_udivdi3 ();
extern rtx gen_udivdi3_internal ();
extern rtx gen_umodsi3 ();
extern rtx gen_umodsi3_internal ();
extern rtx gen_umoddi3 ();
extern rtx gen_umoddi3_internal ();
extern rtx gen_sqrtdf2 ();
extern rtx gen_sqrtsf2 ();
extern rtx gen_abssi2 ();
extern rtx gen_absdi2 ();
extern rtx gen_absdf2 ();
extern rtx gen_abssf2 ();
extern rtx gen_ffssi2 ();
extern rtx gen_ffsdi2 ();
extern rtx gen_negsi2 ();
extern rtx gen_negdi2 ();
extern rtx gen_negdi2_internal ();
extern rtx gen_negdi2_internal_2 ();
extern rtx gen_negdf2 ();
extern rtx gen_negsf2 ();
extern rtx gen_one_cmplsi2 ();
extern rtx gen_one_cmpldi2 ();
extern rtx gen_andsi3 ();
extern rtx gen_anddi3 ();
extern rtx gen_anddi3_internal1 ();
extern rtx gen_iorsi3 ();
extern rtx gen_iordi3 ();
extern rtx gen_xorsi3 ();
extern rtx gen_xordi3 ();
extern rtx gen_xordi3_immed ();
extern rtx gen_truncdfsf2 ();
extern rtx gen_truncdisi2 ();
extern rtx gen_truncdihi2 ();
extern rtx gen_truncdiqi2 ();
extern rtx gen_zero_extendsidi2 ();
extern rtx gen_zero_extendsidi2_internal ();
extern rtx gen_zero_extendhisi2 ();
extern rtx gen_zero_extendhidi2 ();
extern rtx gen_zero_extendqihi2 ();
extern rtx gen_zero_extendqisi2 ();
extern rtx gen_zero_extendqidi2 ();
extern rtx gen_extendsidi2 ();
extern rtx gen_extendhidi2 ();
extern rtx gen_extendhidi2_internal ();
extern rtx gen_extendhisi2 ();
extern rtx gen_extendhisi2_internal ();
extern rtx gen_extendqihi2 ();
extern rtx gen_extendqihi2_internal ();
extern rtx gen_extendqisi2 ();
extern rtx gen_extendqisi2_insn ();
extern rtx gen_extendqidi2 ();
extern rtx gen_extendqidi2_insn ();
extern rtx gen_extendsfdf2 ();
extern rtx gen_fix_truncdfsi2 ();
extern rtx gen_fix_truncsfsi2 ();
extern rtx gen_fix_truncdfdi2 ();
extern rtx gen_fix_truncsfdi2 ();
extern rtx gen_floatsidf2 ();
extern rtx gen_floatdidf2 ();
extern rtx gen_floatsisf2 ();
extern rtx gen_floatdisf2 ();
extern rtx gen_fixuns_truncdfsi2 ();
extern rtx gen_fixuns_truncdfdi2 ();
extern rtx gen_fixuns_truncsfsi2 ();
extern rtx gen_fixuns_truncsfdi2 ();
extern rtx gen_extv ();
extern rtx gen_extzv ();
extern rtx gen_insv ();
extern rtx gen_movsi_ulw ();
extern rtx gen_movsi_usw ();
extern rtx gen_movdi_uld ();
extern rtx gen_movdi_usd ();
extern rtx gen_high ();
extern rtx gen_low ();
extern rtx gen_movdi ();
extern rtx gen_movdi_internal ();
extern rtx gen_movdi_internal2 ();
extern rtx gen_reload_indi ();
extern rtx gen_reload_outdi ();
extern rtx gen_movsi ();
extern rtx gen_movsi_internal1 ();
extern rtx gen_movsi_internal2 ();
extern rtx gen_reload_outsi ();
extern rtx gen_reload_insi ();
extern rtx gen_movcc ();
extern rtx gen_reload_incc ();
extern rtx gen_reload_outcc ();
extern rtx gen_movhi ();
extern rtx gen_movhi_internal1 ();
extern rtx gen_movhi_internal2 ();
extern rtx gen_movqi ();
extern rtx gen_movqi_internal1 ();
extern rtx gen_movqi_internal2 ();
extern rtx gen_movsf ();
extern rtx gen_movsf_internal1 ();
extern rtx gen_movsf_internal2 ();
extern rtx gen_movdf ();
extern rtx gen_movdf_internal1 ();
extern rtx gen_movdf_internal1a ();
extern rtx gen_movdf_internal2 ();
extern rtx gen_loadgp ();
extern rtx gen_movstrsi ();
extern rtx gen_movstrsi_internal ();
extern rtx gen_movstrsi_internal2 ();
extern rtx gen_movstrsi_internal3 ();
extern rtx gen_ashlsi3 ();
extern rtx gen_ashlsi3_internal1 ();
extern rtx gen_ashlsi3_internal2 ();
extern rtx gen_ashldi3 ();
extern rtx gen_ashldi3_internal ();
extern rtx gen_ashldi3_internal2 ();
extern rtx gen_ashldi3_internal3 ();
extern rtx gen_ashldi3_internal4 ();
extern rtx gen_ashrsi3 ();
extern rtx gen_ashrsi3_internal1 ();
extern rtx gen_ashrsi3_internal2 ();
extern rtx gen_ashrdi3 ();
extern rtx gen_ashrdi3_internal ();
extern rtx gen_ashrdi3_internal2 ();
extern rtx gen_ashrdi3_internal3 ();
extern rtx gen_ashrdi3_internal4 ();
extern rtx gen_lshrsi3 ();
extern rtx gen_lshrsi3_internal1 ();
extern rtx gen_lshrsi3_internal2 ();
extern rtx gen_lshrdi3 ();
extern rtx gen_lshrdi3_internal ();
extern rtx gen_lshrdi3_internal2 ();
extern rtx gen_lshrdi3_internal3 ();
extern rtx gen_lshrdi3_internal4 ();
extern rtx gen_rotrsi3 ();
extern rtx gen_rotrdi3 ();
extern rtx gen_cmpsi ();
extern rtx gen_tstsi ();
extern rtx gen_cmpdi ();
extern rtx gen_tstdi ();
extern rtx gen_cmpdf ();
extern rtx gen_cmpsf ();
extern rtx gen_branch_fp_ne ();
extern rtx gen_branch_fp_eq ();
extern rtx gen_branch_zero ();
extern rtx gen_branch_zero_di ();
extern rtx gen_branch_equality ();
extern rtx gen_branch_equality_di ();
extern rtx gen_beq ();
extern rtx gen_bne ();
extern rtx gen_bgt ();
extern rtx gen_bge ();
extern rtx gen_blt ();
extern rtx gen_ble ();
extern rtx gen_bgtu ();
extern rtx gen_bgeu ();
extern rtx gen_bltu ();
extern rtx gen_bleu ();
extern rtx gen_seq ();
extern rtx gen_seq_si_zero ();
extern rtx gen_seq_di_zero ();
extern rtx gen_seq_si ();
extern rtx gen_seq_di ();
extern rtx gen_sne ();
extern rtx gen_sne_si_zero ();
extern rtx gen_sne_di_zero ();
extern rtx gen_sne_si ();
extern rtx gen_sne_di ();
extern rtx gen_sgt ();
extern rtx gen_sgt_si ();
extern rtx gen_sgt_di ();
extern rtx gen_sge ();
extern rtx gen_sge_si ();
extern rtx gen_sge_di ();
extern rtx gen_slt ();
extern rtx gen_slt_si ();
extern rtx gen_slt_di ();
extern rtx gen_sle ();
extern rtx gen_sle_si_const ();
extern rtx gen_sle_di_const ();
extern rtx gen_sle_si_reg ();
extern rtx gen_sle_di_reg ();
extern rtx gen_sgtu ();
extern rtx gen_sgtu_si ();
extern rtx gen_sgtu_di ();
extern rtx gen_sgeu ();
extern rtx gen_sgeu_si ();
extern rtx gen_sgeu_di ();
extern rtx gen_sltu ();
extern rtx gen_sltu_si ();
extern rtx gen_sltu_di ();
extern rtx gen_sleu ();
extern rtx gen_sleu_si_const ();
extern rtx gen_sleu_di_const ();
extern rtx gen_sleu_si_reg ();
extern rtx gen_sleu_di_reg ();
extern rtx gen_seq_df ();
extern rtx gen_slt_df ();
extern rtx gen_sle_df ();
extern rtx gen_sgt_df ();
extern rtx gen_sge_df ();
extern rtx gen_seq_sf ();
extern rtx gen_slt_sf ();
extern rtx gen_sle_sf ();
extern rtx gen_sgt_sf ();
extern rtx gen_sge_sf ();
extern rtx gen_jump ();
extern rtx gen_indirect_jump ();
extern rtx gen_indirect_jump_internal1 ();
extern rtx gen_indirect_jump_internal2 ();
extern rtx gen_tablejump ();
extern rtx gen_tablejump_internal1 ();
extern rtx gen_tablejump_internal2 ();
extern rtx gen_tablejump_internal3 ();
extern rtx gen_tablejump_mips161 ();
extern rtx gen_tablejump_mips162 ();
extern rtx gen_tablejump_internal4 ();
extern rtx gen_casesi ();
extern rtx gen_casesi_internal ();
extern rtx gen_builtin_setjmp_setup ();
extern rtx gen_builtin_setjmp_setup_32 ();
extern rtx gen_builtin_setjmp_setup_64 ();
extern rtx gen_builtin_longjmp ();
extern rtx gen_prologue ();
extern rtx gen_blockage ();
extern rtx gen_epilogue ();
extern rtx gen_sibcall_epilogue ();
extern rtx gen_return ();
extern rtx gen_return_internal ();
extern rtx gen_get_fnaddr ();
extern rtx gen_call_internal0 ();
extern rtx gen_call_internal1 ();
extern rtx gen_call_internal2 ();
extern rtx gen_call_internal3a ();
extern rtx gen_call_internal3b ();
extern rtx gen_call_internal3c ();
extern rtx gen_call_internal4a ();
extern rtx gen_call_internal4b ();
extern rtx gen_sibcall_internal0 ();
extern rtx gen_sibcall_internal1 ();
extern rtx gen_call_value_internal0 ();
extern rtx gen_call_value_internal1 ();
extern rtx gen_call_value_internal2 ();
extern rtx gen_call_value_internal3a ();
extern rtx gen_call_value_internal3b ();
extern rtx gen_call_value_internal3c ();
extern rtx gen_call_value_internal4a ();
extern rtx gen_call_value_internal4b ();
extern rtx gen_sibcall_value_internal0 ();
extern rtx gen_sibcall_value_internal1 ();
extern rtx gen_call_value_multiple_internal0 ();
extern rtx gen_call_value_multiple_internal2 ();
extern rtx gen_untyped_call ();
extern rtx gen_nop ();
extern rtx gen_movsicc ();
extern rtx gen_movdicc ();
extern rtx gen_movsfcc ();
extern rtx gen_movdfcc ();
extern rtx gen_consttable_qi ();
extern rtx gen_consttable_hi ();
extern rtx gen_consttable_si ();
extern rtx gen_consttable_di ();
extern rtx gen_consttable_sf ();
extern rtx gen_consttable_df ();
extern rtx gen_align_2 ();
extern rtx gen_align_4 ();
extern rtx gen_align_8 ();
extern rtx gen_leasi ();
extern rtx gen_leadi ();
extern rtx gen_call ();
extern rtx gen_sibcall ();
extern rtx gen_call_value ();
extern rtx gen_sibcall_value ();
#endif  /* NO_MD_PROTOTYPES */
