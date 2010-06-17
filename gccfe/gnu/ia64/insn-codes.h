
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `gencodes'
from the machine description file `md'.  */

#ifndef MAX_INSN_CODE

enum insn_code {
  CODE_FOR_load_gprel = 5,
  CODE_FOR_gprel64_offset = 6,
  CODE_FOR_extendqidi2 = 11,
  CODE_FOR_extendhidi2 = 12,
  CODE_FOR_extendsidi2 = 13,
  CODE_FOR_zero_extendqidi2 = 14,
  CODE_FOR_zero_extendhidi2 = 15,
  CODE_FOR_zero_extendsidi2 = 16,
  CODE_FOR_extendsfdf2 = 17,
  CODE_FOR_truncdfsf2 = 18,
  CODE_FOR_truncxfsf2 = 19,
  CODE_FOR_truncxfdf2 = 20,
  CODE_FOR_floatdixf2 = 21,
  CODE_FOR_fix_truncsfdi2 = 22,
  CODE_FOR_fix_truncdfdi2 = 23,
  CODE_FOR_floatunsdisf2 = 24,
  CODE_FOR_floatunsdidf2 = 25,
  CODE_FOR_fixuns_truncsfdi2 = 26,
  CODE_FOR_fixuns_truncdfdi2 = 27,
  CODE_FOR_extv = 28,
  CODE_FOR_extzv = 29,
  CODE_FOR_shift_mix4left = 31,
  CODE_FOR_mix4right = 33,
  CODE_FOR_adddi3 = 41,
  CODE_FOR_subdi3 = 43,
  CODE_FOR_muldi3 = 45,
  CODE_FOR_smuldi3_highpart = 48,
  CODE_FOR_umuldi3_highpart = 49,
  CODE_FOR_negdi2 = 50,
  CODE_FOR_addsf3 = 52,
  CODE_FOR_subsf3 = 53,
  CODE_FOR_mulsf3 = 54,
  CODE_FOR_abssf2 = 55,
  CODE_FOR_negsf2 = 56,
  CODE_FOR_minsf3 = 58,
  CODE_FOR_maxsf3 = 59,
  CODE_FOR_adddf3 = 64,
  CODE_FOR_subdf3 = 65,
  CODE_FOR_muldf3 = 66,
  CODE_FOR_absdf2 = 67,
  CODE_FOR_negdf2 = 68,
  CODE_FOR_mindf3 = 70,
  CODE_FOR_maxdf3 = 71,
  CODE_FOR_ashldi3 = 79,
  CODE_FOR_ashrdi3 = 82,
  CODE_FOR_lshrdi3 = 83,
  CODE_FOR_anddi3 = 86,
  CODE_FOR_iordi3 = 88,
  CODE_FOR_xordi3 = 89,
  CODE_FOR_one_cmpldi2 = 90,
  CODE_FOR_call_internal = 108,
  CODE_FOR_call_value_internal = 110,
  CODE_FOR_return_internal = 113,
  CODE_FOR_return = 114,
  CODE_FOR_jump = 117,
  CODE_FOR_indirect_jump = 118,
  CODE_FOR_tablejump_internal = 119,
  CODE_FOR_prologue_allocate_stack = 120,
  CODE_FOR_epilogue_deallocate_stack = 121,
  CODE_FOR_alloc = 122,
  CODE_FOR_gr_spill = 123,
  CODE_FOR_gr_restore = 124,
  CODE_FOR_fr_spill = 125,
  CODE_FOR_fr_restore = 126,
  CODE_FOR_pr_spill = 127,
  CODE_FOR_pr_restore = 128,
  CODE_FOR_pfs_restore = 129,
  CODE_FOR_unat_spill = 130,
  CODE_FOR_unat_restore = 131,
  CODE_FOR_bsp_value = 132,
  CODE_FOR_set_bsp = 133,
  CODE_FOR_flushrs = 134,
  CODE_FOR_nop = 135,
  CODE_FOR_blockage = 136,
  CODE_FOR_insn_group_barrier = 137,
  CODE_FOR_flush_cache = 138,
  CODE_FOR_ccv_restore_si = 139,
  CODE_FOR_ccv_restore_di = 140,
  CODE_FOR_mf = 141,
  CODE_FOR_fetchadd_acq_si = 142,
  CODE_FOR_fetchadd_acq_di = 143,
  CODE_FOR_cmpxchg_acq_si = 144,
  CODE_FOR_cmpxchg_acq_di = 145,
  CODE_FOR_xchgsi = 146,
  CODE_FOR_xchgdi = 147,
  CODE_FOR_movqi = 148,
  CODE_FOR_movhi = 149,
  CODE_FOR_movsi = 150,
  CODE_FOR_movdi = 151,
  CODE_FOR_load_fptr = 152,
  CODE_FOR_load_gprel64 = 153,
  CODE_FOR_load_symptr = 154,
  CODE_FOR_movsf = 155,
  CODE_FOR_movdf = 156,
  CODE_FOR_movxf = 157,
  CODE_FOR_insv = 159,
  CODE_FOR_addsi3 = 162,
  CODE_FOR_subsi3 = 163,
  CODE_FOR_mulsi3 = 164,
  CODE_FOR_negsi2 = 165,
  CODE_FOR_abssi2 = 166,
  CODE_FOR_sminsi3 = 167,
  CODE_FOR_smaxsi3 = 168,
  CODE_FOR_uminsi3 = 169,
  CODE_FOR_umaxsi3 = 170,
  CODE_FOR_absdi2 = 172,
  CODE_FOR_smindi3 = 173,
  CODE_FOR_smaxdi3 = 174,
  CODE_FOR_umindi3 = 175,
  CODE_FOR_umaxdi3 = 176,
  CODE_FOR_ffsdi2 = 177,
  CODE_FOR_ashlsi3 = 178,
  CODE_FOR_ashrsi3 = 179,
  CODE_FOR_lshrsi3 = 180,
  CODE_FOR_rotrsi3 = 181,
  CODE_FOR_rotrdi3 = 183,
  CODE_FOR_one_cmplsi2 = 184,
  CODE_FOR_cmpsi = 185,
  CODE_FOR_cmpdi = 186,
  CODE_FOR_cmpsf = 187,
  CODE_FOR_cmpdf = 188,
  CODE_FOR_cmpxf = 189,
  CODE_FOR_movcc = 190,
  CODE_FOR_beq = 192,
  CODE_FOR_bne = 193,
  CODE_FOR_blt = 194,
  CODE_FOR_ble = 195,
  CODE_FOR_bgt = 196,
  CODE_FOR_bge = 197,
  CODE_FOR_bltu = 198,
  CODE_FOR_bleu = 199,
  CODE_FOR_bgtu = 200,
  CODE_FOR_bgeu = 201,
  CODE_FOR_seq = 202,
  CODE_FOR_sne = 203,
  CODE_FOR_slt = 204,
  CODE_FOR_sle = 205,
  CODE_FOR_sgt = 206,
  CODE_FOR_sge = 207,
  CODE_FOR_sltu = 208,
  CODE_FOR_sleu = 209,
  CODE_FOR_sgtu = 210,
  CODE_FOR_sgeu = 211,
  CODE_FOR_call = 222,
  CODE_FOR_indirect_call_pic = 223,
  CODE_FOR_setjmp_call_pic = 224,
  CODE_FOR_call_pic = 225,
  CODE_FOR_call_value = 226,
  CODE_FOR_indirect_call_value_pic = 227,
  CODE_FOR_indirect_call_multiple_values_pic = 228,
  CODE_FOR_setjmp_call_value_pic = 229,
  CODE_FOR_call_value_pic = 230,
  CODE_FOR_call_multiple_values_pic = 231,
  CODE_FOR_untyped_call = 232,
  CODE_FOR_tablejump = 233,
  CODE_FOR_prologue = 234,
  CODE_FOR_epilogue = 235,
  CODE_FOR_save_stack_nonlocal = 236,
  CODE_FOR_nonlocal_goto = 237,
  CODE_FOR_nonlocal_goto_receiver = 238,
  CODE_FOR_eh_epilogue = 239,
  CODE_FOR_restore_stack_nonlocal = 240,
  CODE_FOR_val_compare_and_swap_si = 241,
  CODE_FOR_val_compare_and_swap_di = 242,
  CODE_FOR_lock_test_and_set_si = 243,
  CODE_FOR_lock_test_and_set_di = 244,
  CODE_FOR_fetch_and_add_si = 245,
  CODE_FOR_fetch_and_sub_si = 246,
  CODE_FOR_fetch_and_or_si = 247,
  CODE_FOR_fetch_and_and_si = 248,
  CODE_FOR_fetch_and_xor_si = 249,
  CODE_FOR_fetch_and_nand_si = 250,
  CODE_FOR_fetch_and_add_di = 251,
  CODE_FOR_fetch_and_sub_di = 252,
  CODE_FOR_fetch_and_or_di = 253,
  CODE_FOR_fetch_and_and_di = 254,
  CODE_FOR_fetch_and_xor_di = 255,
  CODE_FOR_fetch_and_nand_di = 256,
  CODE_FOR_add_and_fetch_di = 257,
  CODE_FOR_sub_and_fetch_di = 258,
  CODE_FOR_or_and_fetch_di = 259,
  CODE_FOR_and_and_fetch_di = 260,
  CODE_FOR_xor_and_fetch_di = 261,
  CODE_FOR_nand_and_fetch_di = 262,
  CODE_FOR_add_and_fetch_si = 263,
  CODE_FOR_sub_and_fetch_si = 264,
  CODE_FOR_or_and_fetch_si = 265,
  CODE_FOR_and_and_fetch_si = 266,
  CODE_FOR_xor_and_fetch_si = 267,
  CODE_FOR_nand_and_fetch_si = 268,
  CODE_FOR_nothing };

#define MAX_INSN_CODE ((int) CODE_FOR_nothing)
#endif /* MAX_INSN_CODE */
