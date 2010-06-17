
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

//TENSILICA: this file was formerly config_TARG.cxx
//but renamed to prevent case conflicts on NT

/* ====================================================================
 * ====================================================================
 *
 * $Source: /osprey.src/osprey1.0/common/com/ia64/RCS/config_targ_options.cxx,v $
 *
 * Revision history:
 *  26-Feb-97 - Original Version, extracted from config.c.
 *
 * Description:
 *
 * Configure the -TARG group (included in config.c).
 * See config_targ_options.h for usage conventions.
 * See config_targ.* for more general target configuration support.
 *
 * NOTE:  There is an approximate distinction between -TARG option
 * group flags and their configuration (in this file), and more generic
 * target configuration (in config_targ.c).  Note that the related
 * header file config_targ.h is included in config.h, and hence in most
 * source files, whereas config_targ_options.h is only included directly, so
 * putting new -TARG option-related variables in here is to be
 * preferred to putting them in config_targ.[hc].
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_targ_options.h"

/* ====================================================================
 * List of global variables that are set by the -TARG option group
 * ====================================================================
 */

/* General target control: */
DLL_SHARED char *ABI_Name = NULL;		/* -TARG:abi=xxx */
DLL_SHARED INT Target_FPRs = 0;


/* ==================================================================== */

/***** The following are flags that we will likely either get rid of, or
       provide access to through targ_info. *****/
  
DLL_SHARED char *ISA_Name = NULL;		/* -TARG:isa=xxx */
DLL_SHARED char *Processor_Name = NULL;	/* -TARG:processor=xxx */
static char * Platform_Name = NULL;
DLL_SHARED BOOL Pure_ABI = FALSE;		/* Avoid non-ABI constructs? */
/* Fault handling: */
DLL_SHARED BOOL Force_FP_Precise_Mode = FALSE;	/* Force precise FP traps? */
DLL_SHARED BOOL Force_Memory_Dismiss = FALSE;	/* Force mem fault dismissal? */
DLL_SHARED BOOL Force_Page_Zero = FALSE;		/* Force mapping page zero? */
DLL_SHARED BOOL Force_SMM = FALSE;			/* Force sequential memory? */
DLL_SHARED char *FP_Excp_Max = NULL;		/* Max FP trap enables */
DLL_SHARED char *FP_Excp_Min = NULL;		/* Min FP trap enables */
/* Miscellaneous target instruction features: */
DLL_SHARED BOOL Madd_Allowed = TRUE;		/* Generate madd instructions? */
static BOOL Slow_CVTDL_Set = FALSE;

DLL_SHARED BOOL SYNC_Allowed = TRUE;

/* On Xtensa, set this to true, since we don't want to introduce
   extraneous 4 byte to 8 byte conversion.  This variable apparently
   controls that.
*/
DLL_SHARED BOOL Slow_CVTDL = TRUE;


/* theoretically, none of these defaults should ever be used, 
   because the driver will pass them in */
DLL_SHARED BOOL xt_density = FALSE;
DLL_SHARED BOOL xt_endian = FALSE;
DLL_SHARED BOOL xt_mac16 = FALSE;
DLL_SHARED BOOL xt_short_bitfields = TRUE;
DLL_SHARED BOOL xt_mul16 = FALSE;
DLL_SHARED BOOL xt_mul32 = FALSE;
DLL_SHARED BOOL xt_mul32h = FALSE;
DLL_SHARED BOOL xt_div32 = FALSE;
DLL_SHARED BOOL xt_nsa = FALSE;
DLL_SHARED BOOL xt_minmax = FALSE;
DLL_SHARED BOOL xt_sext = FALSE;
DLL_SHARED BOOL xt_booleans = FALSE;
DLL_SHARED BOOL xt_hifi2 = FALSE;
DLL_SHARED BOOL xt_vectorfpu2005 = FALSE;
DLL_SHARED BOOL xt_hard_float = FALSE;
DLL_SHARED BOOL xt_hard_float_div = FALSE;
DLL_SHARED BOOL xt_hard_float_recip = FALSE;
DLL_SHARED BOOL xt_hard_float_sqrt = FALSE;
DLL_SHARED BOOL xt_hard_float_rsqrt = FALSE;
DLL_SHARED BOOL xt_clamps = FALSE;
DLL_SHARED BOOL xt_injui = FALSE;
DLL_SHARED BOOL xt_flix = TRUE;
DLL_SHARED BOOL xt_only_full_flix = FALSE;
DLL_SHARED BOOL xt_unaligned_stores = FALSE;
DLL_SHARED BOOL xt_unaligned_loads = FALSE;
DLL_SHARED BOOL xt_zero_cost_loop = FALSE;
DLL_SHARED BOOL xt_zero_init_data = FALSE;
DLL_SHARED BOOL xt_fused_madd = FALSE;
DLL_SHARED BOOL xt_fused_madd_set = FALSE;
DLL_SHARED BOOL xt_serialize_volatile = TRUE;
DLL_SHARED BOOL xt_xtensa5_memw = FALSE;
DLL_SHARED BOOL xt_switchjump = FALSE;
DLL_SHARED BOOL xt_noswitchjump_seen = FALSE;
DLL_SHARED BOOL xt_reorder_tieport = TRUE;
DLL_SHARED BOOL xt_flush_tieport = FALSE;
DLL_SHARED BOOL xt_text_section_literals = FALSE;
DLL_SHARED BOOL xt_target_align = FALSE;
DLL_SHARED BOOL xt_long_calls = FALSE;
DLL_SHARED BOOL xt_rw_str = FALSE;	/* writable string default false */
DLL_SHARED BOOL xt_const16 = FALSE;
DLL_SHARED BOOL xt_l32r = TRUE;
DLL_SHARED BOOL xt_abs = TRUE;
DLL_SHARED BOOL xt_addx = TRUE;
DLL_SHARED BOOL xt_brt = FALSE;
DLL_SHARED BOOL xt_prefer_const16 = FALSE;
DLL_SHARED BOOL xt_pseudo_loop = TRUE;
DLL_SHARED BOOL xt_lazy_bias = TRUE;

DLL_SHARED UINT32 xt_i_latency = 1;
DLL_SHARED UINT32 xt_e_stage = 3;
DLL_SHARED UINT32 xt_m_stage = 4;
DLL_SHARED UINT32 xt_b_stage = 3;
DLL_SHARED UINT32 xt_w_stage = 3;
DLL_SHARED BOOL xt_print_pipe = FALSE;

DLL_SHARED UINT32 xt_brt_threshold_int = 0;
DLL_SHARED float xt_brt_threshold;
DLL_SHARED float xt_immed_brt_threshold;
DLL_SHARED char * xt_lsp = NULL;
DLL_SHARED UINT32 xt_earliest_arch = 0;
DLL_SHARED UINT32 xt_latest_arch = 0;
DLL_SHARED char * xt_rename_section = NULL;

DLL_SHARED UINT32 xt_icache_line_bytes = 0;
DLL_SHARED UINT32 xt_dcache_line_bytes = 0;
DLL_SHARED UINT32 xt_dcache_bytes = 0;

DLL_SHARED UINT32 xt_goc = 1;

DLL_SHARED UINT32 xt_stack_alignment = 16;

/* note! these *_line_bytes_string vars are only set in the 
   front end. Don't use them in the back. Use the nicer
   integer versions above. */
DLL_SHARED char * xt_icache_line_bytes_string = NULL;
DLL_SHARED char * xt_dcache_line_bytes_string = NULL;

DLL_SHARED char * xt_isa_dlls_string = NULL;
DLL_SHARED char ** xt_isa_dlls = NULL;
DLL_SHARED char * xt_xtie_dlls_string = NULL;
DLL_SHARED char ** xt_xtie_dlls = NULL;
DLL_SHARED char * xt_abi_string = NULL;
DLL_SHARED char * xt_stack_alignment_string = NULL;

/* ==================================================================== */

#define XTENSA_BOOL_OPTION(name, variable) \
  { OVK_BOOL,         \
    OV_VISIBLE,       \
    FALSE,            \
    name,             \
    name,             \
    0, 0, 1,          \
    &variable,        \
    NULL,             \
    " " }



/* Target machine specification options. */
static OPTION_DESC Options_TARG[] = {
  { OVK_NAME,    OV_VISIBLE,    FALSE,  "abi",   "ab",
    0, 0, 0,     &ABI_Name,	NULL,   "Specify the ABI to follow" },

  XTENSA_BOOL_OPTION("density", xt_density),
  XTENSA_BOOL_OPTION("endian", xt_endian),
  XTENSA_BOOL_OPTION("short-bitfields", xt_short_bitfields),
  XTENSA_BOOL_OPTION("mac16", xt_mac16),
  XTENSA_BOOL_OPTION("mul16", xt_mul16),
  XTENSA_BOOL_OPTION("mul32", xt_mul32),
  XTENSA_BOOL_OPTION("mul32h", xt_mul32h),
  XTENSA_BOOL_OPTION("div32", xt_div32),
  XTENSA_BOOL_OPTION("nsa", xt_nsa),
  XTENSA_BOOL_OPTION("minmax", xt_minmax),
  XTENSA_BOOL_OPTION("sext", xt_sext),
  XTENSA_BOOL_OPTION("booleans", xt_booleans),
  XTENSA_BOOL_OPTION("hifi2", xt_hifi2),
  XTENSA_BOOL_OPTION("vectorfpu2005", xt_vectorfpu2005),
  XTENSA_BOOL_OPTION("hard-float", xt_hard_float),
  XTENSA_BOOL_OPTION("hard-float-div", xt_hard_float_div),
  XTENSA_BOOL_OPTION("hard-float-recip", xt_hard_float_recip),
  XTENSA_BOOL_OPTION("hard-float-sqrt", xt_hard_float_sqrt),
  XTENSA_BOOL_OPTION("hard-float-rsqrt", xt_hard_float_rsqrt),
  XTENSA_BOOL_OPTION("clamps", xt_clamps),
  XTENSA_BOOL_OPTION("injui", xt_injui),
  XTENSA_BOOL_OPTION("flix", xt_flix),
  XTENSA_BOOL_OPTION("only-full-flix", xt_only_full_flix),
  XTENSA_BOOL_OPTION("unaligned_stores", xt_unaligned_stores),
  XTENSA_BOOL_OPTION("unaligned_loads", xt_unaligned_loads),
  XTENSA_BOOL_OPTION("zero-cost-loop", xt_zero_cost_loop),
  XTENSA_BOOL_OPTION("serialize-volatile", xt_serialize_volatile),
  XTENSA_BOOL_OPTION("xtensa5-memw", xt_xtensa5_memw),
  XTENSA_BOOL_OPTION("switchjump", xt_switchjump),
  XTENSA_BOOL_OPTION("noswitchjump_seen", xt_noswitchjump_seen),
  XTENSA_BOOL_OPTION("reorder-tieport", xt_reorder_tieport),
  XTENSA_BOOL_OPTION("flush-tieport", xt_flush_tieport),
  XTENSA_BOOL_OPTION("text-section-literals", xt_text_section_literals),
  XTENSA_BOOL_OPTION("target-align", xt_target_align),
  XTENSA_BOOL_OPTION("long-calls", xt_long_calls),
  XTENSA_BOOL_OPTION("writable-strings", xt_rw_str),
  XTENSA_BOOL_OPTION("const16", xt_const16),
  XTENSA_BOOL_OPTION("l32r", xt_l32r),
  XTENSA_BOOL_OPTION("abs", xt_abs),
  XTENSA_BOOL_OPTION("addx", xt_addx),
  XTENSA_BOOL_OPTION("brt", xt_brt),
  XTENSA_BOOL_OPTION("prefer-const16", xt_prefer_const16),

  XTENSA_BOOL_OPTION("pseudo_loop", xt_pseudo_loop),
  XTENSA_BOOL_OPTION("lazy_bias", xt_lazy_bias),
  
  { OVK_BOOL,   OV_VISIBLE, FALSE, "fused-madd", "fused-madd",
    0, 0, 1, &xt_fused_madd, &xt_fused_madd_set, " " },
  { OVK_NAME,   OV_VISIBLE, FALSE, "rename-section-", "rename-section-",
    0, 0, 0, &xt_rename_section, NULL, "rename section x as section y" },
  { OVK_NAME,   OV_VISIBLE, FALSE, "isa-dlls",    "isa-dlls",
    0, 0, 0, &xt_isa_dlls_string, NULL, "set the list of isa dlls" },
  { OVK_NAME,   OV_VISIBLE, FALSE, "xtie-dlls",    "xtie-dlls",
    0, 0, 0, &xt_xtie_dlls_string, NULL, "set the list of xml tie dlls" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "earliest_arch",    "earliest_arch",
    0, 0, 0xFFFFFFFF, &xt_earliest_arch, NULL, "set the earliest micro-architecture" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "latest_arch",    "latest_arch",
    0, 0, 0xFFFFFFFF, &xt_latest_arch, NULL, "set the latest micro-architecture" },
  { OVK_NAME,   OV_VISIBLE, FALSE, "lsp",    "lsp",
    0, 0, 0, &xt_lsp, NULL, "set the lsp package" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "icache-line-bytes", "icache-line-bytes", 
    0, 0, 0xFFFFFFFF, &xt_icache_line_bytes, NULL, "set the line size of the instruction cache" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "dcache-line-bytes", "dcache-line-bytes", 
    0, 0, 0xFFFFFFFF, &xt_dcache_line_bytes, NULL, "set the line size of the data cache" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "dcache-bytes", "dcache-bytes", 
    0, 0, 0xFFFFFFFF, &xt_dcache_bytes, NULL, "set the size of the data cache" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "goc", "goc", 
    1, 0, 2, &xt_goc, NULL, "set the size of the data cache" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "brt-threshold", "brt-threshold", 
    0, 0, 100, &xt_brt_threshold_int, NULL, "frequency of taken branch needed to switch to predicted branch instructions" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "stack-alignment", "stack-alignment", 
    0, 0, 0xFFFFFFFF, &xt_stack_alignment, NULL, "set the stack alignment (4, 8, or 16 bytes)" },
  /* Unimplemented options: */
  /* Obsolete options: */

  { OVK_COUNT }		/* List terminator -- must be last */
};


static OPTION_DESC Options_PIPE[] = {
  { OVK_UINT32, OV_VISIBLE, FALSE, "ilatency", "i", 
    1, 1, 30, &xt_i_latency, NULL, "cycles of fetch latency" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "estage", "e", 
    1, 1, 29, &xt_e_stage, NULL, "cycle of the E stage in the pipeline" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "mstage", "m", 
    2, 2, 30, &xt_m_stage, NULL, "cycle of the M stage in the pipeline" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "bstage", "b", 
    0, 0, 29, &xt_b_stage, NULL, "cycle of the B stage in the pipeline" },
  { OVK_UINT32, OV_VISIBLE, FALSE, "wstage", "w", 
    0, 0, 29, &xt_w_stage, NULL, "cycle of the W stage in the pipeline" },
  { OVK_BOOL, OV_VISIBLE, FALSE, "print", "p", 
    0, 1, 0, &xt_print_pipe, NULL, "print pipeline related info" },

  { OVK_COUNT }
};


void Calculate_Predicted_Threshold(void)
{
  Is_True(xt_b_stage <= xt_e_stage, ("B stage is greater than the E stage"));
  xt_brt_threshold = xt_brt_threshold_int / 100.0;
  if (xt_brt_threshold == 0) 
    xt_brt_threshold = (float)(xt_i_latency + xt_e_stage) /
                       (float)(xt_i_latency + 2 * xt_e_stage - xt_b_stage);
  xt_immed_brt_threshold = 
    xt_brt_threshold + (1 / ((float)(xt_i_latency + 2 * xt_e_stage - xt_b_stage)));
  if (xt_print_pipe)
    printf("i = %d; e = %d; b = %d; threshold = %2f imm threshold = %2f int = %d\n", 
	   xt_i_latency, xt_e_stage, xt_b_stage, 
	   xt_brt_threshold, xt_immed_brt_threshold, xt_brt_threshold_int);
  Is_True((xt_brt_threshold >= 0) && (xt_brt_threshold <= 1.0), ("Illegal predicted branch threshold"));
}



// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
