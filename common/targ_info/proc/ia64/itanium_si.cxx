
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//  Intel Itanium (tm) processor scheduling information
/////////////////////////////////////
//  
//  Description:
//
//  Generate a scheduling description of a Intel Itanium (tm) processor
//  via the si_gen interface.
//
/////////////////////////////////////


//  $Revision: 1.30 $
//  $Date: 2000/05/03 19:59:31 $
//  $Author: srinivas $
//  $Source: /osprey.src/osprey1.0/common/targ_info/proc/ia64/RCS/itanium_si.cxx,v $


#include "si_gen.h"
#include "targ_isa_subset.h"
#include "topcode.h"

static RESOURCE res_issue,     
                res_memory,	// Memory unit 
                res_M0,		// M0 Memory unit
                res_float,	// Float unit
                res_F0,		// F0 Float unit
                res_integer,	// Integer unit
                res_I0,		// I0 unit
                res_I0_I1_M0_M1,// to simulate type-A OPs which can go in
				// either int- or memory unit.
                res_branch;

int
main (int argc, char *argv[])
{
  Machine("itanium", ISA_SUBSET_intel1, argc, argv);

  res_issue = RESOURCE_Create("issue", 6);
  res_I0_I1_M0_M1 = RESOURCE_Create("I0-I1-MO-M1", 4);
  res_memory = RESOURCE_Create("memory", 2);
  res_M0 = RESOURCE_Create("memory0", 1);
  res_float = RESOURCE_Create("floating-point", 2);
  res_F0 = RESOURCE_Create("floating-point0", 1);
  res_integer = RESOURCE_Create("integer", 2);
  res_I0 = RESOURCE_Create("integer0", 1);
  res_branch = RESOURCE_Create("branch", 3);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "LD" Functional Unit Class.
  // Contains: cmpxch, fetchadd, ld, lfetch, xchg
  // 1) Can go to either memory0 or memory1.
  ////////////////////////////////////////////////////
  Instruction_Group("LD",
		    TOP_ld1,
		    TOP_ld2,
		    TOP_ld4,
		    TOP_ld8,
		    TOP_ld8_fill,
		    TOP_ld1_r,
		    TOP_ld2_r,
		    TOP_ld4_r,
		    TOP_ld8_r,
		    TOP_ld8_r_fill,
		    TOP_ld1_i,
		    TOP_ld2_i,
		    TOP_ld4_i,
		    TOP_ld8_i,
		    TOP_ld8_i_fill,
		    TOP_lfetch,
		    TOP_lfetch_excl,
		    TOP_lfetch_fault,
		    TOP_lfetch_fault_excl,
		    TOP_lfetch_r,
		    TOP_lfetch_r_excl,
		    TOP_lfetch_r_fault,
		    TOP_lfetch_r_fault_excl,
		    TOP_lfetch_i,
		    TOP_lfetch_i_excl,
		    TOP_lfetch_i_fault,
		    TOP_lfetch_i_fault_excl,
		    TOP_cmpxchg1,
		    TOP_cmpxchg2,
		    TOP_cmpxchg4,
		    TOP_cmpxchg8,
		    TOP_xchg1,
		    TOP_xchg2,
		    TOP_xchg4,
		    TOP_xchg8,
		    TOP_fetchadd4,
		    TOP_fetchadd8,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(2);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "ST" (INT) Functional Unit Class.
  // Contains: st, st8.
  ////////////////////////////////////////////////////
  Instruction_Group("ST",
		    TOP_st1,
		    TOP_st2,
		    TOP_st4,
		    TOP_st8,
		    TOP_st8_spill,
		    TOP_st1_i,
		    TOP_st2_i,
		    TOP_st4_i,
		    TOP_st8_i,
		    TOP_st8_i_spill,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);
  Store_Available_Time(2);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FLD" Functional Unit Class.
  // Contains: ldf.fill, ldf8, ldfd, ldfe, ldfs
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FLD",
		    TOP_ldfs,
		    TOP_ldfd,
		    TOP_ldf8,
		    TOP_ldfe,
		    TOP_ldf_fill,
		    TOP_ldfs_r,
		    TOP_ldfd_r,
		    TOP_ldf8_r,
		    TOP_ldfe_r,
		    TOP_ldf_r_fill,
		    TOP_ldfs_i,
		    TOP_ldfd_i,
		    TOP_ldf8_i,
		    TOP_ldfe_i,
		    TOP_ldf_i_fill,
		    TOP_ldfps,
		    TOP_ldfpd,
		    TOP_ldfp8,
		    TOP_ldfps_i,
		    TOP_ldfpd_i,
		    TOP_ldfp8_i,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(9);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);
  Load_Access_Time(9);  

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "ST2" (Float) Functional Unit Class.
  // Contains: stf, stf8, stfe, stfs
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ST2",
		    TOP_stfs,
		    TOP_stfd,
		    TOP_stf8,
		    TOP_stfe,
		    TOP_stf_spill,
		    TOP_stfs_i,
		    TOP_stfd_i,
		    TOP_stf8_i,
		    TOP_stfe_i,
		    TOP_stf_i_spill,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);
  Store_Available_Time(9);  

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "TOFR" Functional Unit Class.
  // Contains: setf
  //
  ////////////////////////////////////////////////////
  Instruction_Group("TOFR",
		    TOP_setf_sig,
		    TOP_setf_exp,
		    TOP_setf_s,
		    TOP_setf_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(8);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FRFR" Functional Unit Class.
  // Contains: getf
  //
  ////////////////////////////////////////////////////
  Instruction_Group("FRFR", 
		    TOP_getf_sig,
		    TOP_getf_exp,
		    TOP_getf_s,
		    TOP_getf_d,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "CLD" Functional Unit Class.
  // Contains: chk.a, chk.s.m,ld.c
  //
  ////////////////////////////////////////////////////
  Instruction_Group("CLD",   
		    TOP_chk_s_m,
		    TOP_chk_f_s,
		    TOP_chk_a,
		    TOP_chk_f_a,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "SYST" Functional Unit Class.
  // Contains: epc, alloc, bsw, chk.s.m, fc, fwb, invala,
  // itc, itr, mf, probe, ptc, ptr, srlz, sync, tak, thash,
  // tpa, ttag, bws, clrrrb, rfi
  //
  ////////////////////////////////////////////////////
  Instruction_Group("SYST-1",   
		    TOP_invala,
		    TOP_fwb,
		    TOP_invala_e,
		    TOP_invala_f_e,
		    TOP_mf,
		    TOP_mf_a,
		    TOP_srlz_d,
		    TOP_srlz_i,
		    TOP_sync_i,
		    TOP_flushrs,
		    TOP_break_m,
		    TOP_nop_m,
		    TOP_UNDEFINED);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);

  Instruction_Group("SYST-2",   
		    TOP_loadrs,
		    TOP_fc,
		    TOP_ptc_e,
		    TOP_mov_t_ar_r_m,
		    TOP_mov_t_ar_i_m,
		    TOP_mov_f_ar_m,
		    TOP_mov_t_cr,
		    TOP_mov_f_cr,
		    TOP_alloc,
		    TOP_alloc_3,
		    TOP_mov_t_psr,
		    TOP_mov_t_psrum,
		    TOP_mov_f_psr,
		    TOP_mov_f_psrum,
		    TOP_probe_r,
		    TOP_probe_w,
		    TOP_probe_i_r,
		    TOP_probe_i_w,
		    TOP_probe_r_fault,
		    TOP_probe_w_fault,
		    TOP_probe_rw_fault,
		    TOP_itc_d,
		    TOP_itc_i,
		    TOP_mov_t_rr,
		    TOP_mov_t_dbr,
		    TOP_mov_t_ibr,
		    TOP_mov_t_pkr,
		    TOP_mov_t_pmc,
		    TOP_mov_t_pmd,
		    TOP_itr_d,
		    TOP_itr_i,
		    TOP_mov_f_rr,
		    TOP_mov_f_dbr,
		    TOP_mov_f_ibr,
		    TOP_mov_f_pkr,
		    TOP_mov_f_pmc,
		    TOP_mov_f_pmd,
		    TOP_mov_f_cpuid,
		    TOP_sum,
		    TOP_rum,
		    TOP_ssm,
		    TOP_rsm,
		    TOP_ptc_l,
		    TOP_ptc_g,
		    TOP_ptc_ga,
		    TOP_ptr_d,
		    TOP_ptr_i,
		    TOP_thash,
		    TOP_ttag,
		    TOP_tpa,
		    TOP_tak,
		    TOP_UNDEFINED);
  // Dummy resource requirement. TODO: need to update the operands/results 
  // available time(s). These times are not available in any current
  // manuals.
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_memory, 0);
  Resource_Requirement(res_M0, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "MMMUL" Functional Unit Class.
  // Contains: pmpy2, pmpyshr2
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMMUL",
		    TOP_pmpyshr2,
		    TOP_pmpyshr2_u,
		    TOP_pmpy2_r,
		    TOP_pmpy2_l,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);
  
  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "MMSHF" Functional Unit Class.
  // Contains: mix,mux,pack,pshl,pshr,shl,shr,unpack
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMSHF",
		    TOP_mix1_r,
		    TOP_mix2_r,
		    TOP_mix4_r,
		    TOP_mix1_l,
		    TOP_mix2_l,
		    TOP_mix4_l,
		    TOP_pack2_uss,
		    TOP_pack2_sss,
		    TOP_pack4_sss,
		    TOP_unpack1_h,
		    TOP_unpack2_h,
		    TOP_unpack4_h,
		    TOP_unpack1_l,
		    TOP_unpack2_l,
		    TOP_unpack4_l,
		    TOP_pshr2,
		    TOP_pshr4,
		    TOP_pshr2_u,
		    TOP_pshr4_u,
		    TOP_shr_u,
		    TOP_pshr2_i,
		    TOP_pshr4_i,
		    TOP_pshr2_i_u,
		    TOP_pshr4_i_u,
		    TOP_pshl2,
		    TOP_pshl4,
		    TOP_shl,
		    TOP_shr,
		    TOP_pshl2_i,
		    TOP_pshl4_i,
		    TOP_mux1,
		    TOP_mux2,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); 
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0); // can go in I0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "MMALU" (A-type) Functional Unit Class.
  // Contains: padd, paddp1, pavg, pavgsub, pcmp, pmax, pmin,
  // pshladd2, psub, ppopc2, popcnt, psad
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMALU",
		    TOP_padd1,
		    TOP_padd2,
		    TOP_padd4,
		    TOP_padd1_sss,
		    TOP_padd2_sss,
		    TOP_padd1_uuu,
		    TOP_padd2_uuu,
		    TOP_padd1_uus,
		    TOP_padd2_uus,
		    TOP_psub1,
		    TOP_psub2,
		    TOP_psub4,
		    TOP_psub1_sss,
		    TOP_psub2_sss,
		    TOP_psub1_uuu,
		    TOP_psub2_uuu,
		    TOP_psub1_uus,
		    TOP_psub2_uus,
		    TOP_pavg1,
		    TOP_pavg2,
		    TOP_pavg1_raz,
		    TOP_pavg2_raz,
		    TOP_pavgsub1,
		    TOP_pavgsub2,
		    TOP_pcmp1_eq,
		    TOP_pcmp2_eq,
		    TOP_pcmp4_eq,
		    TOP_pcmp1_gt,
		    TOP_pcmp2_gt,
		    TOP_pcmp4_gt,
		    TOP_pshladd2,
		    TOP_pshradd2,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); // Can go in any I- or M-unit.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "MMALU" (I-type) Functional Unit Class.
  // Contains: pmin, pmax, psad, popcnt
  //
  ////////////////////////////////////////////////////
  Instruction_Group("MMALU1",
		    TOP_pmin1_u,
		    TOP_pmax1_u,
		    TOP_pmin2,
		    TOP_pmax2,
		    TOP_psad1,
		    TOP_popcnt,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); 
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "ISHF" Functional Unit Class.
  // Contains: dep, extr, shrp
  //
  ////////////////////////////////////////////////////
  Instruction_Group("ISHF",
		    TOP_shrp,
		    TOP_extr_u,
		    TOP_extr,
		    TOP_dep_z,
		    TOP_dep_i_z,
		    TOP_dep_i,
		    TOP_dep,
		    TOP_shl_i,		// pseudo of dep_z
		    TOP_shr_i_u,	// pseudo of extr_u
		    TOP_shr_i,		// pseudo of extr
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); 
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0); // can go in I0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "TBIT" Functional Unit Class.
  // Contains: tbit
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("TBIT",
		    TOP_tbit_z,
		    TOP_tbit_z_unc,
		    TOP_tbit_z_and,
		    TOP_tbit_nz_and,
		    TOP_tbit_z_or,
		    TOP_tbit_nz_or,
		    TOP_tbit_z_or_andcm,
		    TOP_tbit_nz_or_andcm,
		    TOP_tbit_nz,
		    TOP_tbit_nz_unc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1); // 0 to a dependent branch
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0); // can go in I0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "ICMP" Functional Unit Class.
  // Contains: cmp, cmp4, tnat
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("ICMP",
		    TOP_tnat_z,
		    TOP_tnat_z_unc,
		    TOP_tnat_z_and,
		    TOP_tnat_nz_and,
		    TOP_tnat_z_or,
		    TOP_tnat_nz_or,
		    TOP_tnat_z_or_andcm,
		    TOP_tnat_nz_or_andcm,
		    TOP_tnat_nz,
		    TOP_tnat_nz_unc,
		    TOP_cmp_lt,
		    TOP_cmp_ltu,
		    TOP_cmp_eq,
		    TOP_cmp_lt_unc,
		    TOP_cmp_ltu_unc,
		    TOP_cmp_eq_unc,
		    TOP_cmp_eq_and,
		    TOP_cmp_eq_or,
		    TOP_cmp_eq_or_andcm,
		    TOP_cmp_ne_and,
		    TOP_cmp_ne_or,
		    TOP_cmp_ne_or_andcm,
		    TOP_cmp4_lt,
		    TOP_cmp4_ltu,
		    TOP_cmp4_eq,
		    TOP_cmp4_lt_unc,
		    TOP_cmp4_ltu_unc,
		    TOP_cmp4_eq_unc,
		    TOP_cmp4_eq_and,
		    TOP_cmp4_eq_or,
		    TOP_cmp4_eq_or_andcm,
		    TOP_cmp4_ne_and,
		    TOP_cmp4_ne_or,
		    TOP_cmp4_ne_or_andcm,
		    TOP_cmp_z1_gt_and,
		    TOP_cmp_z1_gt_or,
		    TOP_cmp_z1_gt_or_andcm,
		    TOP_cmp_z1_le_and,
		    TOP_cmp_z1_le_or,
		    TOP_cmp_z1_le_or_andcm,
		    TOP_cmp_z1_ge_and,
		    TOP_cmp_z1_ge_or,
		    TOP_cmp_z1_ge_or_andcm,
		    TOP_cmp_z1_lt_and,
		    TOP_cmp_z1_lt_or,
		    TOP_cmp_z1_lt_or_andcm,
		    TOP_cmp4_z1_gt_and,
		    TOP_cmp4_z1_gt_or,
		    TOP_cmp4_z1_gt_or_andcm,
		    TOP_cmp4_z1_le_and,
		    TOP_cmp4_z1_le_or,
		    TOP_cmp4_z1_le_or_andcm,
		    TOP_cmp4_z1_ge_and,
		    TOP_cmp4_z1_ge_or,
		    TOP_cmp4_z1_ge_or_andcm,
		    TOP_cmp4_z1_lt_and,
		    TOP_cmp4_z1_lt_or,
		    TOP_cmp4_z1_lt_or_andcm,
		    TOP_cmp_i_lt,
		    TOP_cmp_i_ltu,
		    TOP_cmp_i_eq,
		    TOP_cmp_i_lt_unc,
		    TOP_cmp_i_ltu_unc,
		    TOP_cmp_i_eq_unc,
		    TOP_cmp_i_eq_and,
		    TOP_cmp_i_eq_or,
		    TOP_cmp_i_eq_or_andcm,
		    TOP_cmp_i_ne_and,
		    TOP_cmp_i_ne_or,
		    TOP_cmp_i_ne_or_andcm,
		    TOP_cmp4_i_lt,
		    TOP_cmp4_i_ltu,
		    TOP_cmp4_i_eq,
		    TOP_cmp4_i_lt_unc,
		    TOP_cmp4_i_ltu_unc,
		    TOP_cmp4_i_eq_unc,
		    TOP_cmp4_i_eq_and,
		    TOP_cmp4_i_eq_or,
		    TOP_cmp4_i_eq_or_andcm,
		    TOP_cmp4_i_ne_and,
		    TOP_cmp4_i_ne_or,
		    TOP_cmp4_i_ne_or_andcm,
		    TOP_cmp_eq_and_orcm,
		    TOP_cmp_eq_andcm,
		    TOP_cmp_eq_orcm,
		    TOP_cmp_ge,
		    TOP_cmp_ge_unc,
		    TOP_cmp_geu,
		    TOP_cmp_geu_unc,
		    TOP_cmp_ne,
		    TOP_cmp_ne_and_orcm,
		    TOP_cmp_ne_andcm,
		    TOP_cmp_ne_orcm,
		    TOP_cmp_ne_unc,
		    TOP_cmp4_eq_and_orcm,
		    TOP_cmp4_eq_andcm,
		    TOP_cmp4_eq_orcm,
		    TOP_cmp4_ge,
		    TOP_cmp4_ge_unc,
		    TOP_cmp4_geu,
		    TOP_cmp4_geu_unc,
		    TOP_cmp4_ne,
		    TOP_cmp4_ne_and_orcm,
		    TOP_cmp4_ne_orcm,
		    TOP_cmp4_ne_andcm,
		    TOP_cmp4_ne_unc,
		    TOP_cmp_gt,
		    TOP_cmp_gt_unc,
		    TOP_cmp_gtu,
		    TOP_cmp_gtu_unc,
		    TOP_cmp4_gt,
		    TOP_cmp4_gt_unc,
		    TOP_cmp4_gtu,
		    TOP_cmp4_gtu_unc,
		    TOP_cmp_le,
		    TOP_cmp_le_unc,
		    TOP_cmp_leu,
		    TOP_cmp_leu_unc,
		    TOP_cmp4_le,
		    TOP_cmp4_le_unc,
		    TOP_cmp4_leu,
		    TOP_cmp4_leu_unc,
		    TOP_cmp_i_eq_and_orcm,
		    TOP_cmp_i_eq_andcm,
		    TOP_cmp_i_eq_orcm,
		    TOP_cmp_i_ge,
		    TOP_cmp_i_ge_unc,
		    TOP_cmp_i_geu,
		    TOP_cmp_i_geu_unc,
		    TOP_cmp_i_ne,
		    TOP_cmp_i_ne_and_orcm,
		    TOP_cmp_i_ne_andcm,
		    TOP_cmp_i_ne_orcm,
		    TOP_cmp_i_ne_unc,
		    TOP_cmp4_i_eq_and_orcm,
		    TOP_cmp4_i_eq_andcm,
		    TOP_cmp4_i_eq_orcm,
		    TOP_cmp4_i_ge,
		    TOP_cmp4_i_ge_unc,
		    TOP_cmp4_i_geu,
		    TOP_cmp4_i_geu_unc,
		    TOP_cmp4_i_ne,
		    TOP_cmp4_i_ne_and_orcm,
		    TOP_cmp4_i_ne_andcm,
		    TOP_cmp4_i_ne_orcm,
		    TOP_cmp4_i_ne_unc,
		    TOP_cmp_i_le,
		    TOP_cmp_i_le_unc,
		    TOP_cmp_i_leu,
		    TOP_cmp_i_leu_unc,
		    TOP_cmp4_i_le,
		    TOP_cmp4_i_le_unc,
		    TOP_cmp4_i_leu,
		    TOP_cmp4_i_leu_unc,
		    TOP_cmp_i_gt,
		    TOP_cmp_i_gt_unc,
		    TOP_cmp_i_gtu,
		    TOP_cmp_i_gtu_unc,
		    TOP_cmp4_i_gt,
		    TOP_cmp4_i_gt_unc,
		    TOP_cmp4_i_gtu,
		    TOP_cmp4_i_gtu_unc,
		    TOP_cmp_z1_gt_andcm,
		    TOP_cmp_z1_gt_orcm,
		    TOP_cmp_z1_gt_and_orcm,
		    TOP_cmp_z1_le_andcm,
		    TOP_cmp_z1_le_orcm,
		    TOP_cmp_z1_le_and_orcm,
		    TOP_cmp_z1_ge_andcm,
		    TOP_cmp_z1_ge_orcm,
		    TOP_cmp_z1_ge_and_orcm,
		    TOP_cmp_z1_lt_andcm,
		    TOP_cmp_z1_lt_orcm,
		    TOP_cmp_z1_lt_and_orcm,
		    TOP_cmp4_z1_gt_andcm,
		    TOP_cmp4_z1_gt_orcm,
		    TOP_cmp4_z1_gt_and_orcm,
		    TOP_cmp4_z1_le_andcm,
		    TOP_cmp4_z1_le_orcm,
		    TOP_cmp4_z1_le_and_orcm,
		    TOP_cmp4_z1_ge_andcm,
		    TOP_cmp4_z1_ge_orcm,
		    TOP_cmp4_z1_ge_and_orcm,
		    TOP_cmp4_z1_lt_andcm,
		    TOP_cmp4_z1_lt_orcm,
		    TOP_cmp4_z1_lt_and_orcm,
		    TOP_cmp_z2_gt_andcm,
		    TOP_cmp_z2_gt_orcm,
		    TOP_cmp_z2_gt_and_orcm,
		    TOP_cmp_z2_le_andcm,
		    TOP_cmp_z2_le_orcm,
		    TOP_cmp_z2_le_and_orcm,
		    TOP_cmp_z2_ge_andcm,
		    TOP_cmp_z2_ge_orcm,
		    TOP_cmp_z2_ge_and_orcm,
		    TOP_cmp_z2_lt_andcm,
		    TOP_cmp_z2_lt_orcm,
		    TOP_cmp_z2_lt_and_orcm,
		    TOP_cmp4_z2_gt_andcm,
		    TOP_cmp4_z2_gt_orcm,
		    TOP_cmp4_z2_gt_and_orcm,
		    TOP_cmp4_z2_le_andcm,
		    TOP_cmp4_z2_le_orcm,
		    TOP_cmp4_z2_le_and_orcm,
		    TOP_cmp4_z2_ge_andcm,
		    TOP_cmp4_z2_ge_orcm,
		    TOP_cmp4_z2_ge_and_orcm,
		    TOP_cmp4_z2_lt_andcm,
		    TOP_cmp4_z2_lt_orcm,
		    TOP_cmp4_z2_lt_and_orcm,
		    TOP_cmp_z2_gt_and,
		    TOP_cmp_z2_gt_or,
		    TOP_cmp_z2_gt_or_andcm,
		    TOP_cmp_z2_le_and,
		    TOP_cmp_z2_le_or,
		    TOP_cmp_z2_le_or_andcm,
		    TOP_cmp_z2_ge_and,
		    TOP_cmp_z2_ge_or,
		    TOP_cmp_z2_ge_or_andcm,
		    TOP_cmp_z2_lt_and,
		    TOP_cmp_z2_lt_or,
		    TOP_cmp_z2_lt_or_andcm,
		    TOP_cmp4_z2_gt_and,
		    TOP_cmp4_z2_gt_or,
		    TOP_cmp4_z2_gt_or_andcm,
		    TOP_cmp4_z2_le_and,
		    TOP_cmp4_z2_le_or,
		    TOP_cmp4_z2_le_or_andcm,
		    TOP_cmp4_z2_ge_and,
		    TOP_cmp4_z2_ge_or,
		    TOP_cmp4_z2_ge_or_andcm,
		    TOP_cmp4_z2_lt_and,
		    TOP_cmp4_z2_lt_or,
		    TOP_cmp4_z2_lt_or_andcm,
		    TOP_cmp_lt_and,
		    TOP_cmp_lt_or,
		    TOP_cmp_lt_or_andcm,
		    TOP_cmp_le_and,
		    TOP_cmp_le_or,
		    TOP_cmp_le_or_andcm,
		    TOP_cmp_gt_and,
		    TOP_cmp_gt_or,
		    TOP_cmp_gt_or_andcm,
		    TOP_cmp_ge_and,
		    TOP_cmp_ge_or,
		    TOP_cmp_ge_or_andcm,
		    TOP_cmp_lt_orcm,
		    TOP_cmp_lt_andcm,
		    TOP_cmp_lt_and_orcm,
		    TOP_cmp_le_orcm,
		    TOP_cmp_le_andcm,
		    TOP_cmp_le_and_orcm,
		    TOP_cmp_gt_orcm,
		    TOP_cmp_gt_andcm,
		    TOP_cmp_gt_and_orcm,
		    TOP_cmp_ge_orcm,
		    TOP_cmp_ge_andcm,
		    TOP_cmp_ge_and_orcm,
		    TOP_cmp4_lt_and,
		    TOP_cmp4_lt_or,
		    TOP_cmp4_lt_or_andcm,
		    TOP_cmp4_le_and,
		    TOP_cmp4_le_or,
		    TOP_cmp4_le_or_andcm,
		    TOP_cmp4_gt_and,
		    TOP_cmp4_gt_or,
		    TOP_cmp4_gt_or_andcm,
		    TOP_cmp4_ge_and,
		    TOP_cmp4_ge_or,
		    TOP_cmp4_ge_or_andcm,
		    TOP_cmp4_lt_orcm,
		    TOP_cmp4_lt_andcm,
		    TOP_cmp4_lt_and_orcm,
		    TOP_cmp4_le_orcm,
		    TOP_cmp4_le_andcm,
		    TOP_cmp4_le_and_orcm,
		    TOP_cmp4_gt_orcm,
		    TOP_cmp4_gt_andcm,
		    TOP_cmp4_gt_and_orcm,
		    TOP_cmp4_ge_orcm,
		    TOP_cmp4_ge_andcm,
		    TOP_cmp4_ge_and_orcm,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1); // 0 to a dependent branch
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); // Can go in any I or M-unit.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "NOP" Functional Unit Class.
  // Contains: break, nop
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("NOP",
		    TOP_break_i,
		    TOP_nop_i,
		    TOP_UNDEFINED);
  // Resources not specified. guess work here.
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_integer, 0);  

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "IALU" Functional Unit Class.
  // Contains: add, addl, adds, shladd, sub, chk.s.i, movl
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("IALU",
		    TOP_add,
		    TOP_add_1,
		    TOP_sub,
		    TOP_sub_1,
		    TOP_addp4,
		    TOP_and,
		    TOP_andcm,
		    TOP_or,
		    TOP_xor,
		    TOP_shladd,
		    TOP_shladdp4,
		    TOP_sub_i,
		    TOP_and_i,
		    TOP_andcm_i,
		    TOP_or_i,
		    TOP_xor_i,
		    TOP_adds,
		    TOP_addp4_i,
		    TOP_addl,
		    TOP_chk_s_i,
		    TOP_mov,		// pseudo of adds
		    TOP_mov_i,		// pseudo of addl
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); // can go in any I- or M-unit.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "IALU" Functional Unit Class, but
  // belong in I- category, i.e. can only execute in I- unit (no M-unit).
  // Contains: movt,
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("IALU1",
		    TOP_mov_t_br_i,
		    TOP_mov_t_br_ret,
		    TOP_mov_f_br,
		    TOP_mov_t_pr,
		    TOP_mov_t_pr_i,
		    TOP_mov_f_ip,
		    TOP_mov_f_pr,
		    TOP_mov_t_ar_r_i,
		    TOP_mov_t_ar_i_i,
		    TOP_mov_f_ar_i,
		    TOP_mov_t_br,
		    TOP_movl,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0);
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0); // can go in I0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "ILOG" Functional Unit Class.
  // Contains: addp4, and, andcm, or, shladdp4, xor, czx, sxt, zxt
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("ILOG",
		    TOP_zxt1,
		    TOP_zxt2,
		    TOP_zxt4,
		    TOP_sxt1,
		    TOP_sxt2,
		    TOP_sxt4,
		    TOP_czx1_l,
		    TOP_czx2_l,
		    TOP_czx1_r,
		    TOP_czx2_r,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_I0_I1_M0_M1, 0); 
  Resource_Requirement(res_integer, 0);
  Resource_Requirement(res_I0, 0); // can execute in I0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FMAC" Functional Unit Class.
  // Contains: fma, fms, fnma
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FMAC",
		    TOP_fma,
		    TOP_fma_s,
		    TOP_fma_d,
		    TOP_fpma,
		    TOP_fms,
		    TOP_fms_s,
		    TOP_fms_d,
		    TOP_fpms,
		    TOP_fnma,
		    TOP_fnma_s,
		    TOP_fnma_d,
		    TOP_fcvt_xuf,	// pseudo of fma
		    TOP_fcvt_xuf_d,	// pseudo of fma_d
		    TOP_fcvt_xuf_s,	// pseudo of fma_s
		    TOP_fadd,		// pseudo of fma
		    TOP_fadd_d,		// pseudo of fma_d
		    TOP_fadd_s,		// pseudo of fma_s
		    TOP_fmpy,		// pseudo of fma
		    TOP_fmpy_d,		// pseudo of fma_d
		    TOP_fmpy_s,		// pseudo of fma_s
		    TOP_fnmpy,		// pseudo of fnma
		    TOP_fnmpy_d,	// pseudo of fnma_d
		    TOP_fnmpy_s,	// pseudo of fnma_s
		    TOP_fnorm,		// pseudo of fma
		    TOP_fnorm_d,	// pseudo of fma_d
		    TOP_fnorm_s,	// pseudo of fma_s
		    TOP_fpmpy,		// pseudo of fpma
		    TOP_fpnmpy,		// pseudo of fpnma
		    TOP_fsub,		// pseudo of fms
		    TOP_fsub_d,		// pseudo of fms_d
		    TOP_fsub_s,		// pseudo of fms_s
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4); // other scenarios need to be adjusted.
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0); 

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FCMP" Functional Unit Class.
  // Contains: fclass.m, fcmp
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FCMP",
		    TOP_fcmp_eq,
		    TOP_fcmp_lt,
		    TOP_fcmp_le,
		    TOP_fcmp_unord,
		    TOP_fcmp_eq_unc,
		    TOP_fcmp_lt_unc,
		    TOP_fcmp_le_unc,
		    TOP_fcmp_unord_unc,
		    TOP_fcmp_gt,
		    TOP_fcmp_gt_unc,
		    TOP_fcmp_ge,
		    TOP_fcmp_ge_unc,
		    TOP_fcmp_neq,
		    TOP_fcmp_neq_unc,
		    TOP_fcmp_nlt,
		    TOP_fcmp_nlt_unc,
		    TOP_fcmp_nle,
		    TOP_fcmp_nle_unc,
		    TOP_fcmp_ngt,
		    TOP_fcmp_ngt_unc,
		    TOP_fcmp_nge,
		    TOP_fcmp_nge_unc,
		    TOP_fcmp_ord,
		    TOP_fcmp_ord_unc,
		    TOP_fclass_m,
		    TOP_fclass_m_unc,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(2);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0); 

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FMISC" Functional Unit Class.
  // Contains: famax, famin, fand, fandcm, fchkf, fmax, fmerge, fmin,
  // fmix, for, fpack, frcpa, frsqrta, fselect, fsetc, fswap, fsxt,
  // fxor.
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FMISC",
		    TOP_fselect,
		    TOP_frcpa,
		    TOP_fprcpa,
		    TOP_frsqrta,
		    TOP_fprsqrta,
		    TOP_fmin,
		    TOP_fmax,
		    TOP_famin,
		    TOP_famax,
		    TOP_fmerge_s,
		    TOP_fmerge_ns,
		    TOP_fmerge_se,
		    TOP_fmix_lr,
		    TOP_fmix_r,
		    TOP_fmix_l,
		    TOP_fsxt_r,
		    TOP_fsxt_l,
		    TOP_fpack,
		    TOP_fswap,
		    TOP_fswap_nl,
		    TOP_fswap_nr,
		    TOP_fand,
		    TOP_fandcm,
		    TOP_for,
		    TOP_fxor,
		    TOP_fabs,		// pseudo of fmerge_s
		    TOP_fnegabs,	// pseudo of fmerge_ns
		    TOP_fneg,		// pseudo of fmerge_ns
		    TOP_fsetc,
		    TOP_mov_f,		// pseudo of fmerge_s
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0); // can execute in F0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "SFMISC" Functional Unit Class.
  // Contains: fpamax, fpamin, fpcmp, fpmax, fpmerge, fpmin, fprcpa, 
  // fprsqrta
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("SFMISC",
		    TOP_fpmin,
		    TOP_fpmax,
		    TOP_fpamin,
		    TOP_fpamax,
		    TOP_fpcmp_eq,
		    TOP_fpcmp_lt,
		    TOP_fpcmp_le,
		    TOP_fpcmp_gt,
		    TOP_fpcmp_ge,
		    TOP_fpcmp_unord,
		    TOP_fpcmp_neq,
		    TOP_fpcmp_nlt,
		    TOP_fpcmp_nle,
		    TOP_fpcmp_ngt,
		    TOP_fpcmp_nge,
		    TOP_fpcmp_ord,
		    TOP_fpmerge_s,
		    TOP_fpmerge_ns,
		    TOP_fpmerge_se,
		    TOP_fpabs,		// pseudo of fpmerge_s
		    TOP_fpnegabs,	// pseudo of fpmrege_ns
		    TOP_fpneg,		// pseudo of fpmerge_ns
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);
  Resource_Requirement(res_F0, 0);  // can execute in F0 unit only.

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FCVTINT" Functional Unit Class.
  // Contains: fcvt.fx, fcvt.fxu
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FCVTINT",
		    TOP_fcvt_fx,
		    TOP_fcvt_fxu,
		    TOP_fcvt_fx_trunc,
		    TOP_fcvt_fxu_trunc,
		    TOP_UNDEFINED);
  // Resource Description not specified, guess work.
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FCVTFP" Functional Unit Class.
  // Contains: fcvt.xf
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FCVTFP",
		    TOP_fcvt_xf,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "FMISC1" Functional Unit Class.
  // Contains: 
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("FMISC1",
		    TOP_xma_lu,		// pseudo of xma_l
		    TOP_fclass_nm,
		    TOP_fclass_nm_unc,
		    TOP_fpnma,
		    TOP_xma_l,
		    TOP_xma_h,
		    TOP_xma_hu,
		    TOP_fpcvt_fx,
		    TOP_fpcvt_fxu,
		    TOP_fpcvt_fx_trunc,
		    TOP_fpcvt_fxu_trunc,
		    TOP_fclrf,
		    TOP_fchkf,
		    TOP_xmpy_l,		// pseudo of xma_l
		    TOP_xmpy_lu,	// pseudo of xma_l
		    TOP_xmpy_h,		// pseudo of xma_h
		    TOP_xmpy_hu,	// pseudo of xma_hu
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(4);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "NOP" (FP) Functional Unit Class.
  // Contains: break_f, nop_f
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("NOP2",
		    TOP_break_f,
		    TOP_nop_f,
		    TOP_UNDEFINED);
  // Resources not specified. guess work here.
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(0);
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_float, 0);  

  ////////////////////////////////////////////////////
  //
  // Instructions belonging to "BR" Functional Unit Class.
  // Contains: br.call, br.cond, br.ia, br.ret, br.cexit,
  // br.cloop, br.ctop, br.wexit, br.wtop.
  //
  ////////////////////////////////////////////////////  
  Instruction_Group("BR",
		    TOP_br_cond,
		    TOP_br_call,
		    TOP_br_r_cond,
		    TOP_br_ia,
		    TOP_br_ret,
		    TOP_br_r_call,
		    TOP_br_wexit,
		    TOP_br_wtop,
		    TOP_br_cloop,
		    TOP_br_cexit,
		    TOP_br_ctop,
		    TOP_brp,
		    TOP_brp_r,
		    TOP_brp_ret,
		    TOP_cover,
		    TOP_clrrrb,
		    TOP_clrrrb_pr,
		    TOP_rfi,
		    TOP_bsw_0,
		    TOP_bsw_1,
		    TOP_epc,
		    TOP_break_b,
		    TOP_nop_b,
		    TOP_br,
		    TOP_br_r,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1); // Not sure ????
  Resource_Requirement(res_issue, 0);
  Resource_Requirement(res_branch, 0);

  ////////////////////////////////////////////////////
  //
  Instruction_Group("dummy",
		    TOP_break,
		    TOP_nop,
		    TOP_mov_f_ar,
		    TOP_mov_t_ar_r,
		    TOP_mov_t_ar_i,
		    TOP_chk_s,
		    TOP_asm,
		    TOP_intrncall,
		    TOP_spadjust,
		    TOP_copy_br,
		    TOP_UNDEFINED);
  Any_Operand_Access_Time(0);
  Any_Result_Available_Time(1);
  Resource_Requirement(res_issue, 0);

  Machine_Done("itanium.c");
}

