/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `gencodes'
from the machine description file `md'.  */

#ifndef MAX_INSN_CODE

enum insn_code {
  CODE_FOR_adddf3 = 0,
  CODE_FOR_addsf3 = 1,
  CODE_FOR_addsi3 = 2,
  CODE_FOR_addsi3_internal = 3,
  CODE_FOR_adddi3 = 9,
  CODE_FOR_adddi3_internal_1 = 10,
  CODE_FOR_adddi3_internal_2 = 13,
  CODE_FOR_adddi3_internal_3 = 16,
  CODE_FOR_addsi3_internal_2 = 22,
  CODE_FOR_subdf3 = 24,
  CODE_FOR_subsf3 = 25,
  CODE_FOR_subsi3 = 26,
  CODE_FOR_subsi3_internal = 27,
  CODE_FOR_subdi3 = 33,
  CODE_FOR_subdi3_internal = 34,
  CODE_FOR_subdi3_internal_2 = 37,
  CODE_FOR_subdi3_internal_3 = 40,
  CODE_FOR_subsi3_internal_2 = 46,
  CODE_FOR_muldf3 = 48,
  CODE_FOR_muldf3_internal = 49,
  CODE_FOR_muldf3_r4300 = 50,
  CODE_FOR_mulsf3 = 51,
  CODE_FOR_mulsf3_internal = 52,
  CODE_FOR_mulsf3_r4300 = 53,
  CODE_FOR_mulsi3 = 54,
  CODE_FOR_mulsi3_mult3 = 55,
  CODE_FOR_mulsi3_internal = 56,
  CODE_FOR_mulsi3_r4000 = 57,
  CODE_FOR_muldi3 = 63,
  CODE_FOR_muldi3_internal = 64,
  CODE_FOR_muldi3_internal2 = 65,
  CODE_FOR_mulsidi3 = 66,
  CODE_FOR_umulsidi3 = 67,
  CODE_FOR_mulsidi3_internal = 68,
  CODE_FOR_mulsidi3_64bit = 69,
  CODE_FOR_smulsi3_highpart = 72,
  CODE_FOR_umulsi3_highpart = 73,
  CODE_FOR_xmulsi3_highpart_internal = 74,
  CODE_FOR_xmulsi3_highpart_5400 = 75,
  CODE_FOR_smuldi3_highpart = 77,
  CODE_FOR_umuldi3_highpart = 78,
  CODE_FOR_madsi = 79,
  CODE_FOR_divdf3 = 90,
  CODE_FOR_divsf3 = 91,
  CODE_FOR_divmodsi4 = 94,
  CODE_FOR_divmodsi4_internal = 95,
  CODE_FOR_divmoddi4 = 96,
  CODE_FOR_divmoddi4_internal = 97,
  CODE_FOR_udivmodsi4 = 98,
  CODE_FOR_udivmodsi4_internal = 99,
  CODE_FOR_udivmoddi4 = 100,
  CODE_FOR_udivmoddi4_internal = 101,
  CODE_FOR_div_trap = 102,
  CODE_FOR_div_trap_normal = 103,
  CODE_FOR_div_trap_mips16 = 104,
  CODE_FOR_divsi3 = 105,
  CODE_FOR_divsi3_internal = 106,
  CODE_FOR_divdi3 = 107,
  CODE_FOR_divdi3_internal = 108,
  CODE_FOR_modsi3 = 109,
  CODE_FOR_modsi3_internal = 110,
  CODE_FOR_moddi3 = 111,
  CODE_FOR_moddi3_internal = 112,
  CODE_FOR_udivsi3 = 113,
  CODE_FOR_udivsi3_internal = 114,
  CODE_FOR_udivdi3 = 115,
  CODE_FOR_udivdi3_internal = 116,
  CODE_FOR_umodsi3 = 117,
  CODE_FOR_umodsi3_internal = 118,
  CODE_FOR_umoddi3 = 119,
  CODE_FOR_umoddi3_internal = 120,
  CODE_FOR_sqrtdf2 = 121,
  CODE_FOR_sqrtsf2 = 122,
  CODE_FOR_abssi2 = 125,
  CODE_FOR_absdi2 = 126,
  CODE_FOR_absdf2 = 127,
  CODE_FOR_abssf2 = 128,
  CODE_FOR_ffssi2 = 129,
  CODE_FOR_ffsdi2 = 130,
  CODE_FOR_negsi2 = 131,
  CODE_FOR_negdi2 = 132,
  CODE_FOR_negdi2_internal = 133,
  CODE_FOR_negdi2_internal_2 = 134,
  CODE_FOR_negdf2 = 135,
  CODE_FOR_negsf2 = 136,
  CODE_FOR_one_cmplsi2 = 137,
  CODE_FOR_one_cmpldi2 = 138,
  CODE_FOR_andsi3 = 140,
  CODE_FOR_anddi3 = 143,
  CODE_FOR_anddi3_internal1 = 147,
  CODE_FOR_iorsi3 = 148,
  CODE_FOR_iordi3 = 151,
  CODE_FOR_xorsi3 = 155,
  CODE_FOR_xordi3 = 158,
  CODE_FOR_xordi3_immed = 163,
  CODE_FOR_truncdfsf2 = 167,
  CODE_FOR_truncdisi2 = 168,
  CODE_FOR_truncdihi2 = 169,
  CODE_FOR_truncdiqi2 = 170,
  CODE_FOR_zero_extendsidi2 = 177,
  CODE_FOR_zero_extendsidi2_internal = 178,
  CODE_FOR_zero_extendhisi2 = 179,
  CODE_FOR_zero_extendhidi2 = 182,
  CODE_FOR_zero_extendqihi2 = 185,
  CODE_FOR_zero_extendqisi2 = 188,
  CODE_FOR_zero_extendqidi2 = 191,
  CODE_FOR_extendsidi2 = 195,
  CODE_FOR_extendhidi2 = 196,
  CODE_FOR_extendhidi2_internal = 197,
  CODE_FOR_extendhisi2 = 198,
  CODE_FOR_extendhisi2_internal = 199,
  CODE_FOR_extendqihi2 = 200,
  CODE_FOR_extendqihi2_internal = 201,
  CODE_FOR_extendqisi2 = 202,
  CODE_FOR_extendqisi2_insn = 203,
  CODE_FOR_extendqidi2 = 204,
  CODE_FOR_extendqidi2_insn = 205,
  CODE_FOR_extendsfdf2 = 206,
  CODE_FOR_fix_truncdfsi2 = 207,
  CODE_FOR_fix_truncsfsi2 = 208,
  CODE_FOR_fix_truncdfdi2 = 209,
  CODE_FOR_fix_truncsfdi2 = 210,
  CODE_FOR_floatsidf2 = 211,
  CODE_FOR_floatdidf2 = 212,
  CODE_FOR_floatsisf2 = 213,
  CODE_FOR_floatdisf2 = 214,
  CODE_FOR_fixuns_truncdfsi2 = 215,
  CODE_FOR_fixuns_truncdfdi2 = 216,
  CODE_FOR_fixuns_truncsfsi2 = 217,
  CODE_FOR_fixuns_truncsfdi2 = 218,
  CODE_FOR_extv = 219,
  CODE_FOR_extzv = 220,
  CODE_FOR_insv = 221,
  CODE_FOR_movsi_ulw = 222,
  CODE_FOR_movsi_usw = 223,
  CODE_FOR_movdi_uld = 224,
  CODE_FOR_movdi_usd = 225,
  CODE_FOR_high = 226,
  CODE_FOR_low = 227,
  CODE_FOR_movdi = 228,
  CODE_FOR_movdi_internal = 230,
  CODE_FOR_movdi_internal2 = 233,
  CODE_FOR_reload_indi = 236,
  CODE_FOR_reload_outdi = 237,
  CODE_FOR_movsi = 239,
  CODE_FOR_movsi_internal1 = 241,
  CODE_FOR_movsi_internal2 = 242,
  CODE_FOR_reload_outsi = 247,
  CODE_FOR_reload_insi = 248,
  CODE_FOR_movcc = 249,
  CODE_FOR_reload_incc = 250,
  CODE_FOR_reload_outcc = 251,
  CODE_FOR_movhi = 260,
  CODE_FOR_movhi_internal1 = 261,
  CODE_FOR_movhi_internal2 = 262,
  CODE_FOR_movqi = 265,
  CODE_FOR_movqi_internal1 = 266,
  CODE_FOR_movqi_internal2 = 267,
  CODE_FOR_movsf = 270,
  CODE_FOR_movsf_internal1 = 271,
  CODE_FOR_movsf_internal2 = 272,
  CODE_FOR_movdf = 274,
  CODE_FOR_movdf_internal1 = 275,
  CODE_FOR_movdf_internal1a = 276,
  CODE_FOR_movdf_internal2 = 277,
  CODE_FOR_loadgp = 280,
  CODE_FOR_movstrsi = 281,
  CODE_FOR_movstrsi_internal = 282,
  CODE_FOR_movstrsi_internal2 = 287,
  CODE_FOR_movstrsi_internal3 = 289,
  CODE_FOR_ashlsi3 = 291,
  CODE_FOR_ashlsi3_internal1 = 292,
  CODE_FOR_ashlsi3_internal2 = 293,
  CODE_FOR_ashldi3 = 295,
  CODE_FOR_ashldi3_internal = 296,
  CODE_FOR_ashldi3_internal2 = 297,
  CODE_FOR_ashldi3_internal3 = 300,
  CODE_FOR_ashldi3_internal4 = 303,
  CODE_FOR_ashrsi3 = 306,
  CODE_FOR_ashrsi3_internal1 = 307,
  CODE_FOR_ashrsi3_internal2 = 308,
  CODE_FOR_ashrdi3 = 310,
  CODE_FOR_ashrdi3_internal = 311,
  CODE_FOR_ashrdi3_internal2 = 312,
  CODE_FOR_ashrdi3_internal3 = 315,
  CODE_FOR_ashrdi3_internal4 = 318,
  CODE_FOR_lshrsi3 = 321,
  CODE_FOR_lshrsi3_internal1 = 322,
  CODE_FOR_lshrsi3_internal2 = 323,
  CODE_FOR_lshrdi3 = 327,
  CODE_FOR_lshrdi3_internal = 328,
  CODE_FOR_lshrdi3_internal2 = 329,
  CODE_FOR_lshrdi3_internal3 = 332,
  CODE_FOR_lshrdi3_internal4 = 335,
  CODE_FOR_rotrsi3 = 336,
  CODE_FOR_rotrdi3 = 337,
  CODE_FOR_cmpsi = 340,
  CODE_FOR_tstsi = 341,
  CODE_FOR_cmpdi = 342,
  CODE_FOR_tstdi = 343,
  CODE_FOR_cmpdf = 344,
  CODE_FOR_cmpsf = 345,
  CODE_FOR_branch_fp_ne = 346,
  CODE_FOR_branch_fp_eq = 347,
  CODE_FOR_branch_zero = 348,
  CODE_FOR_branch_zero_di = 350,
  CODE_FOR_branch_equality = 352,
  CODE_FOR_branch_equality_di = 353,
  CODE_FOR_beq = 354,
  CODE_FOR_bne = 355,
  CODE_FOR_bgt = 356,
  CODE_FOR_bge = 357,
  CODE_FOR_blt = 358,
  CODE_FOR_ble = 359,
  CODE_FOR_bgtu = 360,
  CODE_FOR_bgeu = 361,
  CODE_FOR_bltu = 362,
  CODE_FOR_bleu = 363,
  CODE_FOR_seq = 364,
  CODE_FOR_seq_si_zero = 365,
  CODE_FOR_seq_di_zero = 367,
  CODE_FOR_seq_si = 369,
  CODE_FOR_seq_di = 371,
  CODE_FOR_sne = 373,
  CODE_FOR_sne_si_zero = 374,
  CODE_FOR_sne_di_zero = 375,
  CODE_FOR_sne_si = 376,
  CODE_FOR_sne_di = 378,
  CODE_FOR_sgt = 380,
  CODE_FOR_sgt_si = 381,
  CODE_FOR_sgt_di = 383,
  CODE_FOR_sge = 385,
  CODE_FOR_sge_si = 386,
  CODE_FOR_sge_di = 388,
  CODE_FOR_slt = 390,
  CODE_FOR_slt_si = 391,
  CODE_FOR_slt_di = 393,
  CODE_FOR_sle = 395,
  CODE_FOR_sle_si_const = 396,
  CODE_FOR_sle_di_const = 398,
  CODE_FOR_sle_si_reg = 400,
  CODE_FOR_sle_di_reg = 402,
  CODE_FOR_sgtu = 404,
  CODE_FOR_sgtu_si = 405,
  CODE_FOR_sgtu_di = 407,
  CODE_FOR_sgeu = 409,
  CODE_FOR_sgeu_si = 410,
  CODE_FOR_sgeu_di = 412,
  CODE_FOR_sltu = 414,
  CODE_FOR_sltu_si = 415,
  CODE_FOR_sltu_di = 417,
  CODE_FOR_sleu = 419,
  CODE_FOR_sleu_si_const = 420,
  CODE_FOR_sleu_di_const = 422,
  CODE_FOR_sleu_si_reg = 424,
  CODE_FOR_sleu_di_reg = 426,
  CODE_FOR_seq_df = 428,
  CODE_FOR_slt_df = 429,
  CODE_FOR_sle_df = 430,
  CODE_FOR_sgt_df = 431,
  CODE_FOR_sge_df = 432,
  CODE_FOR_seq_sf = 433,
  CODE_FOR_slt_sf = 434,
  CODE_FOR_sle_sf = 435,
  CODE_FOR_sgt_sf = 436,
  CODE_FOR_sge_sf = 437,
  CODE_FOR_jump = 438,
  CODE_FOR_indirect_jump = 440,
  CODE_FOR_indirect_jump_internal1 = 441,
  CODE_FOR_indirect_jump_internal2 = 442,
  CODE_FOR_tablejump = 443,
  CODE_FOR_tablejump_internal1 = 444,
  CODE_FOR_tablejump_internal2 = 445,
  CODE_FOR_tablejump_internal3 = 446,
  CODE_FOR_tablejump_mips161 = 447,
  CODE_FOR_tablejump_mips162 = 448,
  CODE_FOR_tablejump_internal4 = 450,
  CODE_FOR_casesi = 452,
  CODE_FOR_casesi_internal = 453,
  CODE_FOR_builtin_setjmp_setup = 454,
  CODE_FOR_builtin_setjmp_setup_32 = 455,
  CODE_FOR_builtin_setjmp_setup_64 = 456,
  CODE_FOR_builtin_longjmp = 457,
  CODE_FOR_prologue = 458,
  CODE_FOR_blockage = 459,
  CODE_FOR_epilogue = 460,
  CODE_FOR_sibcall_epilogue = 461,
  CODE_FOR_return = 462,
  CODE_FOR_return_internal = 463,
  CODE_FOR_get_fnaddr = 464,
  CODE_FOR_call = 465,
  CODE_FOR_call_internal0 = 466,
  CODE_FOR_call_internal1 = 468,
  CODE_FOR_call_internal2 = 469,
  CODE_FOR_call_internal3a = 470,
  CODE_FOR_call_internal3b = 471,
  CODE_FOR_call_internal3c = 472,
  CODE_FOR_call_internal4a = 473,
  CODE_FOR_call_internal4b = 474,
  CODE_FOR_sibcall = 475,
  CODE_FOR_sibcall_internal0 = 476,
  CODE_FOR_sibcall_internal1 = 477,
  CODE_FOR_call_value = 478,
  CODE_FOR_call_value_internal0 = 479,
  CODE_FOR_call_value_internal1 = 481,
  CODE_FOR_call_value_internal2 = 482,
  CODE_FOR_call_value_internal3a = 483,
  CODE_FOR_call_value_internal3b = 484,
  CODE_FOR_call_value_internal3c = 485,
  CODE_FOR_call_value_internal4a = 486,
  CODE_FOR_call_value_internal4b = 487,
  CODE_FOR_sibcall_value = 488,
  CODE_FOR_sibcall_value_internal0 = 489,
  CODE_FOR_sibcall_value_internal1 = 490,
  CODE_FOR_call_value_multiple_internal0 = 491,
  CODE_FOR_call_value_multiple_internal2 = 492,
  CODE_FOR_untyped_call = 493,
  CODE_FOR_nop = 494,
  CODE_FOR_movsicc = 505,
  CODE_FOR_movdicc = 506,
  CODE_FOR_movsfcc = 507,
  CODE_FOR_movdfcc = 508,
  CODE_FOR_consttable_qi = 509,
  CODE_FOR_consttable_hi = 510,
  CODE_FOR_consttable_si = 511,
  CODE_FOR_consttable_di = 512,
  CODE_FOR_consttable_sf = 513,
  CODE_FOR_consttable_df = 514,
  CODE_FOR_align_2 = 515,
  CODE_FOR_align_4 = 516,
  CODE_FOR_align_8 = 517,
  CODE_FOR_leasi = 522,
  CODE_FOR_leadi = 523,
  CODE_FOR_nothing };

#define MAX_INSN_CODE ((int) CODE_FOR_nothing)
#endif /* MAX_INSN_CODE */
