
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Description:
 *
 * External definitions for the -TARG group.
 *
 * Some of these variables are also defined in config.h or
 * MIPS/config_targ.h, for historical reasons.  In order to separate
 * the headers and minimize dependencies on changes to this one, new
 * group members should be defined only here, and their users should
 * explicitly include this file instead of having it indirectly
 * included (e.g. via config.h or MIPS/config_targ.h).  We should also
 * work towards removing most of the definitions from those headers.
 *
 * Exported variables should have names prefixed by "TARG_" to
 * facilitate moving them to a pushable struct later if desired.
 * See config_debug.[hc] for an example.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_TARG_INCLUDED
#define config_TARG_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ====================================================================
 * List of global variables that are set by the -TARG option group
 * These appear only here, requiring explicit inclusion of this file.
 * ====================================================================
 */

/* General target control */
extern DLL_SHARED char *ABI_Name;		/* -TARG:abi=xxx */
extern DLL_SHARED INT Target_FPRs; 	/* Number of target FP registers */

/* ==================================================================== */

/***** The following are flags that we will likely either get rid of, or
       provide access to through targ_info. *****/
  
/* Miscellaneous target instruction features: */
extern DLL_SHARED BOOL Madd_Allowed;	/* Generate madd instructions? */
extern DLL_SHARED BOOL SYNC_Allowed;
extern DLL_SHARED BOOL Slow_CVTDL;

  
extern DLL_SHARED char *Processor_Name;	/* -TARG:processor=xxx */
extern DLL_SHARED char *ISA_Name;		/* -TARG:isa=xxx */
extern DLL_SHARED BOOL Pure_ABI;		/* Avoid non-ABI constructs? */
/* Fault handling: TENSILICA: These are used when emitting object files
 directly and are used to set flags to control how the image should execute
 (put into .option section). We don't have anything like this (yet?). Anyway
 they are currently not being output, even for ia64 - see
 target_em_elf.cxx:Em_Add_New_Option. */
extern DLL_SHARED BOOL Force_FP_Precise_Mode;	/* Force precise FP traps? */
extern DLL_SHARED BOOL Force_Memory_Dismiss;	/* Force mem fault dismissal? */
extern DLL_SHARED BOOL Force_Page_Zero;		/* Force mapping page zero? */
extern DLL_SHARED BOOL Force_SMM;			/* Force sequential memory? */
extern DLL_SHARED char *FP_Excp_Max;		/* Max FP trap enables */
extern DLL_SHARED char *FP_Excp_Min;		/* Min FP trap enables */

extern DLL_SHARED BOOL xt_density;
extern DLL_SHARED BOOL xt_endian;
extern DLL_SHARED BOOL xt_mac16;
extern DLL_SHARED BOOL xt_short_bitfields;
extern DLL_SHARED BOOL xt_mul16;
extern DLL_SHARED BOOL xt_mul32;
extern DLL_SHARED BOOL xt_mul32h;
extern DLL_SHARED BOOL xt_div32;
extern DLL_SHARED BOOL xt_nsa;
extern DLL_SHARED BOOL xt_minmax;
extern DLL_SHARED BOOL xt_sext;
extern DLL_SHARED BOOL xt_booleans;
extern DLL_SHARED BOOL xt_hifi2;
extern DLL_SHARED BOOL xt_vectorfpu2005;
extern DLL_SHARED BOOL xt_hard_float;
extern DLL_SHARED BOOL xt_hard_float_div;
extern DLL_SHARED BOOL xt_hard_float_recip;
extern DLL_SHARED BOOL xt_hard_float_sqrt;
extern DLL_SHARED BOOL xt_hard_float_rsqrt;
extern DLL_SHARED BOOL xt_clamps;
extern DLL_SHARED BOOL xt_injui;
extern DLL_SHARED BOOL xt_flix;
extern DLL_SHARED BOOL xt_only_full_flix;
extern DLL_SHARED BOOL xt_unaligned_stores;
extern DLL_SHARED BOOL xt_unaligned_loads;
extern DLL_SHARED BOOL xt_zero_cost_loop;
extern DLL_SHARED BOOL xt_fused_madd;
extern DLL_SHARED BOOL xt_fused_madd_set;
extern DLL_SHARED BOOL xt_serialize_volatile;
extern DLL_SHARED BOOL xt_xtensa5_memw;
extern DLL_SHARED BOOL xt_switchjump;
extern DLL_SHARED BOOL xt_noswitchjump_seen;
extern DLL_SHARED BOOL xt_reorder_tieport;
extern DLL_SHARED BOOL xt_flush_tieport;
extern DLL_SHARED BOOL xt_text_section_literals;
extern DLL_SHARED BOOL xt_target_align;
extern DLL_SHARED BOOL xt_long_calls;
extern DLL_SHARED BOOL xt_const16;
extern DLL_SHARED BOOL xt_l32r;
extern DLL_SHARED BOOL xt_abs;
extern DLL_SHARED BOOL xt_addx;
extern DLL_SHARED BOOL xt_brt;
extern DLL_SHARED UINT32 xt_i_latency;
extern DLL_SHARED UINT32 xt_e_stage;
extern DLL_SHARED UINT32 xt_m_stage;
extern DLL_SHARED UINT32 xt_b_stage;
extern DLL_SHARED UINT32 xt_w_stage;
extern DLL_SHARED BOOL xt_prefer_const16;
extern DLL_SHARED char * xt_lsp;
extern DLL_SHARED BOOL xt_rw_str;
extern DLL_SHARED UINT32 xt_earliest_arch;
extern DLL_SHARED UINT32 xt_latest_arch;
extern DLL_SHARED char * xt_rename_section;

extern DLL_SHARED float xt_brt_threshold;
extern DLL_SHARED float xt_immed_brt_threshold;
extern DLL_SHARED UINT32 xt_icache_line_bytes;
extern DLL_SHARED UINT32 xt_dcache_line_bytes;
extern DLL_SHARED UINT32 xt_dcache_bytes;
extern DLL_SHARED char * xt_isa_dlls_string;
extern DLL_SHARED char ** xt_isa_dlls;
extern DLL_SHARED char * xt_xtie_dlls_string;
extern DLL_SHARED char ** xt_xtie_dlls;
extern DLL_SHARED UINT32 xt_stack_alignment;

extern void Calculate_Predicted_Threshold(void);

/* ==================================================================== */

#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_TARG_INCLUDED */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
