
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
 * Module: config.c
 * $Revision: 2.378 $
 * $Date: 2000/10/23 21:46:07 $
 * $Author: mpm $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config.cxx,v $
 *
 * Revision history:
 *  06-Jun-90 -	Original Version (moved	from cdriver.c)
 *  01-Feb-91 -	Copied for TP/Muse
 *  15-Jun-91 -	Restructured and integrated Josie
 *  05-May-96 -	Added -WOPT group.
 *
 * Description:
 *
 * Configuration data and routines to set up configuration.
 *
 * Refer to the	discussion in config.h for the distribution of such
 * data	and processing among the various configuration files.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config.cxx,v $ $Revision: 2.378 $";
#endif /* _KEEP_RCS_ID */

#ifdef FRONT_END	/* For setting fullwarn, woff in front end */
#ifndef FRONT_F90
#ifdef EDGSRC
# include "basics.h"
# include "cmd_line.h"
# include "error.h"
#endif /* EDGSRC */
#endif /* ~FRONT_F90 */
#endif /*  FRONT_END */
#include <ctype.h>	/* For isdigit */
#include <elf.h>

#define USE_STANDARD_TYPES 1
#include "defs.h"
#include "em_elf.h"
#include "config.h"
#include "config_platform.h"
#include "config_targ.h"
#include "erglob.h"
#include "flags.h"
#include "tracing.h"
#include "glob.h"
#include "symtab.h"
#include "wn.h"
#include "libti.h"

#ifndef BACK_END
static INT32 Ignore_Int;
#endif

/* The following contains the phase-specific option groups and their
 * associated variable definitions:
 */
#include "config_targ_options.cxx"
#include "config_debug.cxx"
#include "config_ipa.cxx"
#include "config_list.cxx"
#include "config_opt.cxx"
#include "config_wopt.cxx"
#include "config_vho.cxx"
#include "config_flist.cxx"
#include "config_clist.cxx"
#include "config_purple.cxx"
#include "config_promp.cxx"

#ifdef BACK_END
# include "config_lno.cxx"
# include "instr_reader.h"
#endif

/* IR builder sometimes	needs to know whether we're in front end: */
#ifdef SINGLE_PROCESS
INT16 In_Front_End = TRUE;	/* Start out there */
#endif


/* ====================================================================
 *
 * Global option flags
 *
 * ====================================================================
 */

/***** General optimization control *****/
DLL_SHARED BOOL Enable_LOH = FALSE;		/* Do loop overhead processing? */
DLL_SHARED BOOL Enable_LOH_overridden = FALSE;	/* ... option seen? */
DLL_SHARED BOOL CSE_Elim_Enabled = FALSE;		/* Is CSE-elim on? -- this does
					 * not control it, it just
					 * shadows the opt. level
					 */

#ifdef BACK_END
# define DEF_DEBUG_LEVEL        0
DLL_SHARED INT8 Debug_Level = DEF_DEBUG_LEVEL;     /* -gn: debug level */
#endif


/***** Alignment (misaligned memory reference) control *****/
DLL_SHARED BOOL	UseAlignedCopyForStructs = FALSE;	/* control aggregrate copy */
DLL_SHARED INT32	MinStructCopyLoopSize =    16;		/* 0 = always expand */
DLL_SHARED INT32	MinStructCopyMemIntrSize=  0;		/* generate bcopy */
DLL_SHARED INT32	Aggregate_Alignment = -1;		/* This alignment for aggregate layout */

DLL_SHARED INT32 iolist_reuse_limit = 100;

/***** Pointer optimizations, such as treating pointers as arrays *****/
DLL_SHARED BOOL Ptr_Opt_Allowed = FALSE;

/***** Put all-zero initialized file-level data in the BSS section? *****/
DLL_SHARED BOOL Zeroinit_in_bss = TRUE;

/* don't make strings gp-relative (to save gp space) */
DLL_SHARED BOOL Strings_Not_Gprelative = FALSE;

/***** IEEE 754 options *****/
DLL_SHARED IEEE_LEVEL IEEE_Arithmetic = IEEE_ACCURATE; /* IEEE arithmetic? */
DLL_SHARED BOOL IEEE_Arith_Set = FALSE;	/* ... option seen? */
/* BOOL Force_IEEE_Comparisons = FALSE;	*/ /* IEEE NaN comparisons? */
					   /* Moved to ISA/config_targ.cxx */

/***** Speculation eagerness options *****/
DLL_SHARED EAGER_LEVEL Eager_Level = EAGER_SAFE;	/* Eagerness to use: -Xn */
static BOOL Eager_Level_Set = FALSE;	/* ... option seen? */

/***** Constant folding and WHIRL simplifier options *****/
DLL_SHARED ROUNDOFF Roundoff_Level = ROUNDOFF_NONE;/* -OPT_roundoff=n value */
DLL_SHARED BOOL Roundoff_Set = FALSE;		/* ... option seen? */
DLL_SHARED BOOL Fast_Complex_Allowed = FALSE;	/* Fast c_div and c_abs? */
DLL_SHARED BOOL Fast_Complex_Set = FALSE;		/* ... option seen? */
DLL_SHARED BOOL Fast_Bit_Allowed = FALSE;		/* Fast inlined bit intrinsics? */
DLL_SHARED BOOL Fast_Bit_Set = FALSE;		/* ... option seen? */
DLL_SHARED BOOL Fast_NINT_Allowed = FALSE;		/* Fast NINT and ANINT? */
DLL_SHARED BOOL Fast_NINT_Set = FALSE;		/* ... option seen? */
DLL_SHARED BOOL Fast_trunc_Allowed = FALSE;	/* Fast truncs for NINT/ANINT/AINT/AMOD? */
DLL_SHARED BOOL Fast_trunc_Set = FALSE;		/* ... option seen? */
DLL_SHARED BOOL Inline_Intrinsics_Allowed = TRUE;	/* Inline intrinsics? Or lib calls? */
DLL_SHARED BOOL Inline_Intrinsics_Set = FALSE;	/* ... option seen? */
DLL_SHARED BOOL Regions_Around_Inner_Loops = FALSE;/* Put REGIONs around inner loops? */
DLL_SHARED BOOL Region_Boundary_Info = FALSE;	/* calc boundary info for regions */
DLL_SHARED BOOL Simp_Multiply_To_Shift=FALSE;      /* Convert multiplies to shifts */
DLL_SHARED BOOL Enable_NaryExpr= FALSE;		/* allow nary expression in the lowerer */
DLL_SHARED BOOL Enable_NaryExpr_Set = FALSE;	/* ... option seen? */

/***** LANGuage group options *****/
static char *Language_Name = NULL;	/* Source language name */
DLL_SHARED LANGUAGE Language = LANG_UNKNOWN;	/* See language.h */
DLL_SHARED BOOL CXX_Bool_On = TRUE;
DLL_SHARED BOOL CXX_Bool_Set = FALSE;
#ifdef TARG_XTENSA
/* Make sure changes to g++fe/toplev.c::'flag_exceptions' default
   matches the default for config.cxx::CXX_Exceptions_On. */
#endif
DLL_SHARED BOOL CXX_Exceptions_On = FALSE;
DLL_SHARED BOOL CXX_Exceptions_Set = FALSE;
DLL_SHARED BOOL CXX_Alias_Const=FALSE;
DLL_SHARED BOOL CXX_Alias_Const_Set=FALSE;
DLL_SHARED BOOL LANG_Recursive = FALSE;
DLL_SHARED BOOL LANG_Recursive_Set = FALSE;
DLL_SHARED BOOL CXX_Wchar_On = TRUE;
DLL_SHARED BOOL CXX_Wchar_Set = FALSE;
DLL_SHARED BOOL CXX_Namespaces_On = TRUE; 
DLL_SHARED BOOL CXX_Namespaces_Set = FALSE;
DLL_SHARED BOOL CXX_Ansi_For_Init_Scope_On = FALSE;
DLL_SHARED BOOL CXX_Ansi_For_Init_Scope_Set = FALSE;
DLL_SHARED BOOL CXX_Standard_C_Plus_Plus_On = FALSE;
DLL_SHARED BOOL CXX_Standard_C_Plus_Plus_Set = FALSE;
DLL_SHARED BOOL C_Restrict_On = FALSE;
DLL_SHARED BOOL C_Restrict_Set = FALSE;
DLL_SHARED char *C_Auto_Restrict = NULL;
DLL_SHARED BOOL C_Auto_Restrict_Set = FALSE;
DLL_SHARED BOOL FTN_Short_Circuit_On = FALSE;
DLL_SHARED BOOL FTN_Short_Circuit_Set = FALSE;
DLL_SHARED BOOL Macro_Expand_Pragmas_On = FALSE;
DLL_SHARED BOOL Macro_Expand_Pragmas_Set = FALSE;
DLL_SHARED BOOL C_VLA_On = FALSE;
DLL_SHARED BOOL C_VLA_Set = FALSE;
DLL_SHARED BOOL CXX_Typename_On = TRUE; 
DLL_SHARED BOOL CXX_Typename_Set = FALSE;
DLL_SHARED BOOL CXX_Explicit_On = TRUE; 
DLL_SHARED BOOL CXX_Explicit_Set = FALSE;
DLL_SHARED BOOL CXX_Mutable_On = TRUE; 
DLL_SHARED BOOL CXX_Mutable_Set = FALSE;
DLL_SHARED BOOL CXX_Packed_On = FALSE; 
DLL_SHARED BOOL CXX_Packed_Set = FALSE;
DLL_SHARED BOOL LANG_Symtab_Verify_On = TRUE;
DLL_SHARED BOOL LANG_Symtab_Verify_Set = TRUE;
DLL_SHARED BOOL LANG_Ansi_Setjmp_On = TRUE; 
DLL_SHARED BOOL LANG_Ansi_Setjmp_Set = FALSE;
DLL_SHARED BOOL LANG_Ignore_Carriage_Return_On = TRUE; 
DLL_SHARED BOOL LANG_Ignore_Carriage_Return_Set = FALSE;

DLL_SHARED BOOL LANG_Pch;
DLL_SHARED BOOL LANG_Pch_Set;
DLL_SHARED char *LANG_Create_Pch;
DLL_SHARED BOOL LANG_Create_Pch_Set;
DLL_SHARED char *LANG_Use_Pch;
DLL_SHARED BOOL LANG_Use_Pch_Set;
DLL_SHARED char *LANG_Pchdir;
DLL_SHARED char *LANG_cxx_dialect;
DLL_SHARED BOOL LANG_Pchdir_Set;
DLL_SHARED BOOL LANG_cxx_dialect_Set;
DLL_SHARED BOOL LANG_Microsoft_Mode = FALSE;
DLL_SHARED BOOL LANG_Microsoft_Mode_Set = FALSE;

/***** INTERNAL group options *****/

DLL_SHARED BOOL WHIRL_Merge_Types_On = FALSE; 
DLL_SHARED BOOL WHIRL_Merge_Types_Set = FALSE;
DLL_SHARED BOOL WHIRL_Comma_Rcomma_On = TRUE;
DLL_SHARED BOOL WHIRL_Comma_Rcomma_Set = FALSE;
DLL_SHARED BOOL WHIRL_Mtype_A_On = FALSE;
#ifdef TARG_IA64
DLL_SHARED BOOL WHIRL_Mtype_B_On = TRUE;
#else
DLL_SHARED BOOL WHIRL_Mtype_B_On = FALSE;
#endif
DLL_SHARED BOOL WHIRL_Mtype_BS_On = FALSE;
DLL_SHARED BOOL WHIRL_Flatten_Field_On = FALSE;
DLL_SHARED BOOL WHIRL_Vfcall_On = FALSE;
DLL_SHARED BOOL WHIRL_Addr_Passed_On = FALSE;
DLL_SHARED BOOL WHIRL_Addr_Saved_For_Passed_On = FALSE;
DLL_SHARED BOOL WHIRL_Addr_Saved_On = TRUE;
DLL_SHARED BOOL WHIRL_Keep_Cvt_On = DEFAULT_KEEP_CVT;

DLL_SHARED BOOL Global_Pragmas_In_Dummy_PU_On = TRUE;
DLL_SHARED BOOL Malloc_Free_On     = TRUE;
DLL_SHARED BOOL Alloca_Dealloca_On = TRUE;
DLL_SHARED BOOL Barrier_Lvalues_On = TRUE;

/***** F90 Heap/stack allocation threshold */
DLL_SHARED INT32 Heap_Allocation_Threshold=-1;      /* Allocate objects > this on the heap 
					 * (-1 means always use stack), 0 always use heap
					 * default is -1
					 */

/***** Miscellaneous code generation options *****/
DLL_SHARED INT32 Short_Data = DEF_SDATA_ELT_SIZE;	/* Objects of this size in .sdata */
static BOOL Short_Data_Set = FALSE;	/* ... option seen? */
DLL_SHARED INT32 Short_Lits = DEF_SDATA_ELT_SIZE;	/* Literals of this size in .litX */
static BOOL Short_Lits_Set = FALSE;	/* ... option seen? */
DLL_SHARED INT32 Max_Sdata_Elt_Size = DEF_SDATA_ELT_SIZE;	/* -Gn: sdata size */
DLL_SHARED BOOL Constant_GP = FALSE;		/* gp never changes? */



/* ====================================================================
 *
 * Option groups (see flags.h)
 *
 * When defining a new option group, remember to not only add an
 * option descriptor list (e.g. Options_TENV), but also to add an
 * entry describing the group in Common_Option_Groups, below.
 *
 * ====================================================================
 */

/* Temporary variables used for holding GOT size options during option
 * processing until Guaranteed_Small_GOT can be set properly:
 */
static BOOL Use_Small_GOT = FALSE;
static BOOL Use_Large_GOT = FALSE;
INT32 Gspace_Available = DEFAULT_GSPACE;

/* Always force EH Region offsets to be long */
DLL_SHARED BOOL Force_Long_EH_Range_Offsets = FALSE;
/* Force stack frame to use large model */
DLL_SHARED BOOL Force_Large_Stack_Model = FALSE;
DLL_SHARED BOOL Force_GP_Prolog;	/* force usage of gp prolog */
DLL_SHARED BOOL Force_Indirect_Call;/* force usage of indirect calls */
DLL_SHARED BOOL Gen_Indirect_Call = FALSE;  /* force usage of indirect calls in CG */

DLL_SHARED OPTION_LIST *Registers_Not_Allocatable = NULL;

/* Unique ident from IPA */
DLL_SHARED INT32 Ipa_Ident_Number = 0;

DLL_SHARED BOOL Indexed_Loads_Allowed = FALSE;

/* Target environment options: */
static OPTION_DESC Options_TENV[] = {
  { OVK_INT32,	OV_VISIBLE,	FALSE, "align_aggregates",	"align_ag",
    -1, 0, 16,	&Aggregate_Alignment, NULL,
    "Minimum alignment to use for aggregates (structs/arrays)" },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "aligned_copy",		NULL,
    0, 0, 0,	&UseAlignedCopyForStructs, NULL },
  { OVK_BOOL,   OV_SHY,		FALSE, "call_mcount",		NULL,
    0, 0, 0,    &Call_Mcount, NULL },
  { OVK_BOOL,   OV_SHY,		FALSE, "constant_gp",		NULL,
    0, 0, 0,    &Constant_GP, NULL },
  { OVK_BOOL,	OV_SHY,		FALSE, "cpic",			"cp",
    0, 0, 0, &Gen_PIC_Call_Shared, NULL,
    "Generate code for executable programs which may call DSOs" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "fixed_addresses",	"fi",
    0, 0, 0,	&PIC_Fixed_Addresses, NULL },
  { OVK_INT32,	OV_SHY,		FALSE, "Gspace",		NULL,
    DEFAULT_GSPACE,0,INT32_MAX,	&Gspace_Available, NULL,
    "Maximum GP-relative space available" },
  { OVK_UINT32,	OV_INTERNAL,	FALSE, "ipa_ident",		NULL, 
    0, 0, UINT32_MAX, &Ipa_Ident_Number, NULL,
    "Specify IPA timestamp number" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "kernel",		NULL,
    0, 0, 0,	&Kernel_Code,	NULL,
    "Generate code for kernel use" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "large_GOT",		"",
    0, 0, 0,	&Use_Large_GOT, NULL,
    "Assume GOT is larger than 64K bytes" },
  { OVK_NAME,	OV_SHY,		FALSE, "io_library",		NULL,
    0, 0, 0,	&Library_Name, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "large_stack",		NULL,
    0, 0, 0,	&Force_Large_Stack_Model, NULL,
    "Generate code assuming >32KB stack frame" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "local_names",		"",
    0, 0, 0,	&PIC_Local_Names, NULL },
  { OVK_BOOL,	OV_SHY,		FALSE, "long_eh_offsets",	"long_eh", 
    0, 0, 0,	&Force_Long_EH_Range_Offsets, NULL },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "non_volatile_GOT",	"non_v",
    0, 0, 0,	&Non_Volatile_GOT, NULL,
    "Assume GOT is non-volatile" },
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "no_page_offset",	"no_p",
    0, 0, 0,	&PIC_No_Page_Offset, NULL,
    "Don't use GOT page/offset addressing" },
  { OVK_BOOL,	OV_SHY,		FALSE, "pic2",			"pi",
    0, 0, 0, &Gen_PIC_Shared, NULL,
    "Generate position-independent code suitable for DSOs" },
  { OVK_BOOL,	OV_SHY,		FALSE, "pic1",			NULL,
    0, 0, 0, &Gen_PIC_Call_Shared, NULL,
    "Generate code for executable programs which may call DSOs" },
  { OVK_BOOL,   OV_SHY,		FALSE, "profile_call",		"prof",
    0, 0, 0,    &Gen_Profile, NULL },
  { OVK_NAME,	OV_SHY,		FALSE, "profile_name",		"",
    0, 0, 0, &Gen_Profile_Name, NULL },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "protected_names",	"",
    0, 0, 0,	&PIC_Protected_Names, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "short_data",		"short_d",
    0, 0, 4096,	&Short_Data,	&Short_Data_Set,
    "Maximum size of data to allocate GP-relative" },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "short_literals",	"short_l",
    0, 0, 1024,	&Short_Lits,	&Short_Lits_Set,
    "Maximum size of literals to allocate GP-relative" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "small_GOT",		"sm",
    0, 0, 0,	&Use_Small_GOT, NULL,
    "Assume GOT is smaller than 64K bytes" },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "struct_copy_loop_size", "struct_copy_loop",
    -1, 0, 4096,	&MinStructCopyLoopSize, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "struct_copy_mem_intr_size", "struct_copy_mem",
    -1, 0, 4096,	&MinStructCopyMemIntrSize, NULL },
  { OVK_INT32,	OV_VISIBLE,	FALSE, "X",			NULL,
    1, 0, 4,	&Eager_Level,	&Eager_Level_Set,
    "Exception-enable level" },
  { OVK_BOOL,   OV_VISIBLE,	FALSE, "zeroinit_in_bss",	NULL,
    0, 0, 0,    &Zeroinit_in_bss, NULL,
    "Place zero-initialized data in .bss section" },
  { OVK_BOOL,   OV_SHY,		FALSE, "strings_not_gprelative",	"strings_not_gprel",
    0, 0, 0,    &Strings_Not_Gprelative, NULL,
    "Do not put any strings in gp-relative sections" },
  { OVK_NAME,	OV_SHY,		FALSE, "emit_global_data",	"emit_global",
    0, 0, 0, &Emit_Global_Data, NULL,
    "only process the global data" },
  { OVK_NAME,	OV_SHY,		FALSE, "read_global_data",	"read_global",
    0, 0, 0, &Read_Global_Data, NULL,
    "only read the already-processed global data" },
  { OVK_BOOL,	OV_SHY,		FALSE, "force_gp_prolog",	"force_gp",
    0, 0, 0, &Force_GP_Prolog, NULL,
    "force gp_prolog to always be setup" },
  { OVK_LIST,	OV_VISIBLE,	FALSE, "registers_not_allocatable",	NULL,
    0, 0, 0, &Registers_Not_Allocatable, NULL,
    "list of registers that are reserved and not available for allocation" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "force_indirect_call",	"",
    0, 0, 0,	&Force_Indirect_Call,	NULL,
    "Force use of indirect call instructions for all subprogram calls" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "indirect_call",	"",
    0, 0, 0,	&Gen_Indirect_Call,	NULL,
    "Force use of indirect call instructions for all subprogram calls" },

  /***** Options moved elsewhere -- retained for compatibility: *****/
  /* See -DEBUG:div_check */
  { OVK_INT32,   OV_INTERNAL,	FALSE, "check_div",	"check_div",
    1, 0, 3, &Initial_DEBUG.div_check, &Initial_DEBUG.div_check_set },
  /* See -DEBUG:trap_uninitialized */
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "trapuv",	"",
    0, 0, 0, &Initial_DEBUG.trap_uv, &Initial_DEBUG.trap_uv_set },
  /* See -DEBUG:trapuv_right_justify */
  { OVK_BOOL,	OV_INTERNAL,	FALSE, "trapuv_right_justify",	"trapuv_right",
    0, 0, 0, &Initial_DEBUG.trap_uv_rjustify,
	     &Initial_DEBUG.trap_uv_rjustify_set },

  /***** Options moved elsewhere -- replaced: *****/
  /* See -DEBUG:varargs_prototypes */
  { OVK_REPLACED, OV_INTERNAL,	FALSE, "varargs_prototypes",	"varargs_p",
    0, 0, 0,
    const_cast<char*>("-DEBUG:varargs_prototypes"), NULL },

  /***** Obsolete options: *****/
  { /* OVK_INT32, */
    OVK_OBSOLETE, OV_INTERNAL,	FALSE, "align_extern",		NULL,
    0, 0, 16,	NULL, NULL,
    "Assume this alignment for unknown objects" },
  { /* OVK_BOOL, */
    OVK_OBSOLETE, OV_SHY,	FALSE, "aligned",		NULL,
    0, 0, 0,	NULL, NULL,
    "Assume unknown objects are properly aligned" },
  { /* OVK_INT32, */
    OVK_OBSOLETE, OV_INTERNAL,	FALSE, "misalignment",		NULL,
    3, 0, 3,	NULL, NULL },
  { OVK_INT32,	OV_INTERNAL,	FALSE, "iolist_reuse",	"iolist_reuse",
    100, 1, INT32_MAX,	&iolist_reuse_limit, 			NULL,
    "Maximum number of iolists which will share stack space" },

  { OVK_COUNT }		/* List terminator -- must be last */
};

#ifdef BACK_END

/* Phase selection options: */
static OPTION_DESC Options_PHASE[] = {
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "lno",	  "l",	 0, 0, 0,
      &Run_lno,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "wopt",	  "w",	 0, 0, 0,
      &Run_wopt,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "preopt", "p",	 0, 0, 0,
      &Run_preopt,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "cg",	  "c",	 0, 0, 0,
      &Run_cg,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "clist",  NULL,	 0, 0, 0,
      &Run_w2c,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "flist",  NULL,	 0, 0, 0,
      &Run_w2f,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mplist", NULL,	 0, 0, 0,
      &Run_w2fc_early,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "purple", "", 0, 0, 0,
      &Run_purple,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "ipl",    "i",	 0, 0, 0,
      &Run_ipl,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "prompf", NULL,	 0, 0, 0,
      &Run_prompf,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "lpath",  "",	 0, 0, 0,
      &LNO_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "wpath",  "",	 0, 0, 0,
      &WOPT_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "cpath",  "",	 0, 0, 0,
      &CG_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "w2cpath", "", 0, 0, 0,
      &W2C_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "w2fpath", "", 0, 0, 0,
      &W2F_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "purpath", "", 0, 0, 0,
      &Purple_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "ipath",   "", 0, 0, 0,
      &Ipl_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "tpath",   "", 0, 0, 0,
      &Targ_Path,	NULL},
    { OVK_NAME,	OV_INTERNAL,	FALSE, "prompf_anl_path", "", 0, 0, 0,
      &Prompf_Anl_Path, NULL},
    { OVK_COUNT}
};
#elif defined(QIKKI_BE)
static OPTION_DESC Options_PHASE[] = {
    { OVK_NAME, OV_INTERNAL,	FALSE, "tpath",   "t", 0, 0, 0,
      &Targ_Path,	NULL},
    { OVK_COUNT}
};
#endif /* BACK_END */

static OPTION_DESC Options_LANG[] = {
    { OVK_NAME, OV_INTERNAL,	FALSE, "",			NULL,
      0, 0, 0,	&Language_Name,		NULL,
      "Language being compiled, from front end" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "bool",			"",
      0, 0, 0,	&CXX_Bool_On,		&CXX_Bool_Set,
      "C++: enable builtin type 'bool'" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "exceptions",		"",
      0, 0, 0,	&CXX_Exceptions_On,	&CXX_Exceptions_Set,
      "C++: enable exceptions" },
#if 0 // remove it till we have a robust design 
    { OVK_BOOL, OV_SHY,		FALSE, "alias_const",		"",
      0, 0, 0,  &CXX_Alias_Const,       &CXX_Alias_Const_Set },
#endif
    { OVK_BOOL, OV_VISIBLE,	FALSE, "recursive",		"",
      0, 0, 0,	&LANG_Recursive,	&LANG_Recursive_Set,
      "FORTRAN: program contains recursion" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "wchar_t",		"",
      0, 0, 0,	&CXX_Wchar_On,		&CXX_Wchar_Set,
      "C++: enable builtin type 'wchar_t'" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "namespaces",		"namespace",
      0, 0, 0,  &CXX_Namespaces_On,	&CXX_Namespaces_Set,
      "C++: enable namespaces" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "ansi-for-init-scope",	"",
      0, 0, 0,  &CXX_Ansi_For_Init_Scope_On,	&CXX_Ansi_For_Init_Scope_Set},
    { OVK_BOOL, OV_VISIBLE,	FALSE, "std",	"",
      0, 0, 0,  &CXX_Standard_C_Plus_Plus_On,	&CXX_Standard_C_Plus_Plus_Set},
    { OVK_BOOL, OV_SHY,		FALSE, "restrict",		"",
      0, 0, 0,	&C_Restrict_On,		&C_Restrict_Set },
    { OVK_NAME, OV_VISIBLE,	FALSE, "autorestrict",		NULL,
      0, 0, 0,	&C_Auto_Restrict,		&C_Auto_Restrict_Set,
      "Automatically set the \"restrict\" qualifier on some or all pointers" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "scalar_formal_ref", "",
      0, 0, 0,  &Scalar_Formal_Ref,     NULL },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "non_scalar_formal_ref", "",
      0, 0, 0,  &Non_Scalar_Formal_Ref, NULL },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "short_circuit_conditionals", "",
      0, 0, 0,  &FTN_Short_Circuit_On, &FTN_Short_Circuit_Set },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "vla",			"",
      0, 0, 0,	&C_VLA_On,		&C_VLA_Set,
      "C/C++: enable variable length arrays" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "explicit",		"",
      0, 0, 0,  &CXX_Explicit_On,	&CXX_Explicit_Set,
      "C++: enable explicit keyword" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "typename",		"",
      0, 0, 0,  &CXX_Typename_On,	&CXX_Typename_Set,
      "C++: enable typename keyword" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "mutable",		"",
      0, 0, 0,  &CXX_Mutable_On,	&CXX_Mutable_Set,
      "C++: enable mutable keyword" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "macro_expand_pragmas",		"",
      0, 0, 0,	&Macro_Expand_Pragmas_On, &Macro_Expand_Pragmas_Set,
      "C/C++: enable macro expansion in pragmas" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "packed",		"",
      0, 0, 0,  &CXX_Packed_On,	&CXX_Packed_Set,
      "C++: enable pragma pack" },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "symtab_verify",		"",
      0, 0, 0,	&LANG_Symtab_Verify_On,	&LANG_Symtab_Verify_Set },
    { OVK_BOOL, OV_VISIBLE,     FALSE, "pch",          NULL,
      0, 0, 0,  &LANG_Pch, &LANG_Pch_Set,
      "Create a precompiled header for this compilation unit" },
    { OVK_NAME, OV_VISIBLE,     FALSE, "create_pch",          NULL,
      0, 0, 0,  &LANG_Create_Pch, &LANG_Create_Pch_Set,
      "Create a precompiled header file named by this option" },
    { OVK_NAME, OV_VISIBLE,      FALSE, "use_pch",            NULL,
      0, 0, 0,  &LANG_Use_Pch, &LANG_Use_Pch_Set,
      "Use the precompiled header file named by this option" },
    { OVK_NAME, OV_VISIBLE,      FALSE, "pch_dir",            NULL,
      0, 0, 0,  &LANG_Pchdir, &LANG_Pchdir_Set,
      "Create/Use from the directory named by this option" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "ansi_setjmp",		"",
      0, 0, 0,  &LANG_Ansi_Setjmp_On,	&LANG_Ansi_Setjmp_Set,
      "C/C++: enable optimization of functions with calls to setjmp" },
    { OVK_BOOL, OV_VISIBLE,	FALSE, "microsoft_extensions",	"microsoft_extension",
      0, 0, 0,  &LANG_Microsoft_Mode,	&LANG_Microsoft_Mode_Set},

    { OVK_INT32,OV_VISIBLE,	TRUE, "heap_allocation_threshold", "heap_a",
      -1, -1, INT32_MAX, &Heap_Allocation_Threshold,	NULL,
      "Size threshold for switching from stack to heap allocation" },
    { OVK_NAME, OV_VISIBLE, 	FALSE, "cxx_dialect", NULL,
      0, 0, 0,  &LANG_cxx_dialect,   &LANG_cxx_dialect_Set},
    { OVK_BOOL, OV_VISIBLE,	FALSE, "ignore_carriage_return",	"",
      0, 0, 0,  &LANG_Ignore_Carriage_Return_On, &LANG_Ignore_Carriage_Return_Set,
      "C/C++: ignore carriage returns in source" },


    { OVK_COUNT }		    /* List terminator -- must be last */
};

static OPTION_DESC Options_INTERNAL[] = {

    { OVK_BOOL, OV_INTERNAL,	FALSE, "comma_rcomma",		"",
      0, 0, 0,	&WHIRL_Comma_Rcomma_On,	&WHIRL_Comma_Rcomma_Set },
    { OVK_BOOL, OV_INTERNAL,	FALSE, "merge_types",		"",
      0, 0, 0,	&WHIRL_Merge_Types_On,	&WHIRL_Merge_Types_Set },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mtype_a",		NULL,
      0, 0, 0,	&WHIRL_Mtype_A_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mtype_b",		NULL,
      0, 0, 0,	&WHIRL_Mtype_B_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mtype_bs",		NULL,
      0, 0, 0,	&WHIRL_Mtype_BS_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "return_val",		NULL,
      0, 0, 0,	&WHIRL_Return_Val_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "flatten_field",		NULL,
      0, 0, 0,	&WHIRL_Flatten_Field_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mldid_mstid",		NULL,
      0, 0, 0,	&WHIRL_Mldid_Mstid_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "vfcall",		NULL,
      0, 0, 0,	&WHIRL_Vfcall_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "addr_passed",		NULL,
      0, 0, 0,	&WHIRL_Addr_Passed_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "addr_saved_for_passed",	NULL,
      0, 0, 0,	&WHIRL_Addr_Saved_For_Passed_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "addr_saved",		NULL,
      0, 0, 0,	&WHIRL_Addr_Saved_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "keep_cvt",	NULL,
      0, 0, 0,	&WHIRL_Keep_Cvt_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "return_info",	NULL,
      0, 0, 0,	&WHIRL_Return_Info_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "global_pragmas",	NULL,
      0, 0, 0,	&Global_Pragmas_In_Dummy_PU_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "malloc_free",	NULL,
      0, 0, 0,	&Malloc_Free_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "alloca_dealloca",	NULL,
      0, 0, 0,	&Alloca_Dealloca_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "barrier_lvalues",	NULL,
      0, 0, 0,	&Barrier_Lvalues_On, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "mask_shift_counts",	NULL,
      0, 0, 0,	&ARCH_mask_shift_counts, NULL },
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "generate_nor",	NULL,
      0, 0, 0,	&ARCH_generate_nor, NULL },

    { OVK_COUNT }		    /* List terminator -- must be last */
};

DLL_SHARED OPTION_GROUP Common_Option_Groups[] = {
  { "DEBUG",	':', '=', Options_DEBUG, NULL,
    "Options to assist debugging" },
  { "INLINE",	':', '=', Options_INLINE, NULL,
    "Options to control subprogram inlining" },
  { "INTERNAL",	':', '=', Options_INTERNAL, NULL,
    "Options to control internal flags for testing" },
  { "IPA",	':', '=', Options_IPA, NULL,
    "Options to control interprocedural analysis and optimization" },
  { "LANG",	':', '=', Options_LANG, NULL,
    "Options to control source language interpretation" },
  { "LIST",	':', '=', Options_LIST, NULL,
    "Options to control the listing file" },
  { "OPT",	':', '=', Options_OPT, NULL,
    "Options to control general optimization" },
#ifdef BACK_END
  { "LNO",	':', '=', Options_LNO, NULL,
    "Options to control loop nest optimization" },
#endif /* BACK_END */
#if defined(BACK_END) || defined(QIKKI_BE)
  { "PHASE",	':', '=', Options_PHASE, NULL,
    "Options to control phase invocation and locations" },
#endif /* defined(BACK_END) || defined(QIKKI_BE) */
  { "TARG",	':', '=', Options_TARG, NULL,
    "Options to specify the target machine characteristics" },
  { "PIPE",	':', '=', Options_PIPE, NULL,
    "Options to specify the target machine's pipeline" },
  { "TENV",	':', '=', Options_TENV, NULL,
    "Options to set or assert target environment characteristics" },
  { "WOPT",	':', '=', Options_WOPT, NULL,
    "Options to control internal WHIRL optimization" },
  { "VHO",	':', '=', Options_VHO, NULL,
    "Options to control internal VH WHIRL optimization" },
  { "FLIST", ':', '=', Options_FLIST, NULL,
       "Options to control listing of transformed f77 source" },
  { "CLIST", ':', '=', Options_CLIST, NULL,
       "Options to control listing of transformed C source" },
  { "PURPLE", ':', '=', Options_PURPLE, NULL,
       "Options to control program region extraction process" },
  { "PROMP", ':', '=', Options_PROMP, NULL,
       "Options to control listing mp transformations" },
  { NULL }		/* List terminator -- must be last */
};

/* ====================================================================
 *
 * Miscellaneous data declarations and initialization
 *
 * ====================================================================
 */

/* What is the model to be used for logical values in Fortran? */
DLL_SHARED BOOL Use_C_Like_Logicals = TRUE;

/* Is exception-handling enabled in C++? */
DLL_SHARED BOOL Allow_Exceptions = TRUE;

/***** Compiler	debug/trace options *****/
DLL_SHARED BOOL Tracing_Enabled = FALSE;		/* Any trace options set? */
DLL_SHARED INT16 Hard_Failure_Info	= FALSE;	/* Trace on hard failures? */
DLL_SHARED INT16 Soft_Failure_Info	= FALSE;	/* Trace on soft failures? */

/***** Miscellaneous optimization options *****/
/* Should idict commute operands in seeking match? */
DLL_SHARED BOOL Idict_Commutable_Match = FALSE;
DLL_SHARED BOOL Scalar_Formal_Ref = TRUE;		/* for fortran scalar formal refs */
DLL_SHARED BOOL Non_Scalar_Formal_Ref = FALSE;	/* for fortran non_scalar formal refs */

DLL_SHARED BOOL CG_mem_intrinsics = TRUE;		/* for memory intrinsic expansion */
DLL_SHARED INT32 CG_memmove_inst_count = 16;	/* for intrinsic expansion of bzero etc */
DLL_SHARED BOOL CG_memmove_inst_count_overridden = FALSE;
DLL_SHARED BOOL CG_bcopy_cannot_overlap = FALSE;	/* for intrinsic expansion of bcopy */
DLL_SHARED BOOL CG_memcpy_cannot_overlap = FALSE;	/* for intrinsic expansion of memcpy */
DLL_SHARED BOOL CG_memmove_cannot_overlap = FALSE;	/* for intrinsic expansion of memmove */
DLL_SHARED BOOL CG_memmove_nonconst = FALSE;	/* expand mem intrinsics unknown size */

/***** Miscellaneous GOPT options *****/
DLL_SHARED INT32 Opt_Level = DEF_OPT_LEVEL;
DLL_SHARED INT32 OPT_unroll_times = 4;		/* but see Configure_Target() */
DLL_SHARED BOOL OPT_unroll_times_overridden = FALSE;
DLL_SHARED INT32 OPT_unroll_size = 40;		/* but see Configure_CG_Options() */
DLL_SHARED BOOL OPT_unroll_size_overridden = FALSE;
DLL_SHARED INT32 Const_Copy_TN_CNT = DEF_CONST_COPY_TN_CNT;
DLL_SHARED BOOL  Enable_BB_Splitting = TRUE;
DLL_SHARED INT32 Split_BB_Length = DEF_BBLENGTH;
DLL_SHARED BOOL Enable_SWP = FALSE;		/* but see cgdriver.c */
DLL_SHARED BOOL Enable_SWP_overridden = FALSE;
DLL_SHARED BOOL Enable_ZCL = FALSE;		/* but see cgdriver.c */
DLL_SHARED BOOL Enable_ZCL_overridden = FALSE;

/***** Automatic TIE generation options *****/
DLL_SHARED BOOL Run_Autotie = FALSE;
DLL_SHARED BOOL Run_Autotie2 = FALSE;
DLL_SHARED BOOL CG_fusion = TRUE;
DLL_SHARED BOOL CG_find_updating = FALSE;
DLL_SHARED INT32 CG_fusion_before = -1;
DLL_SHARED INT32 CG_fusion_after = -1;
  
/***** What is the byte	sex of the host	and target? *****/
DLL_SHARED UINT8 Host_Byte_Sex = BIG_ENDIAN;	/* Set in config_host.c	*/
DLL_SHARED UINT8 Target_Byte_Sex =	BIG_ENDIAN;	/* Set in config_targ.c	*/
DLL_SHARED BOOL  Same_Byte_Sex = TRUE;		/* Set in config_targ.c	*/

/***** Miscellaneous code generation options *****/
DLL_SHARED BOOL Use_Base_Ptrs = TRUE;	/* Explicit ptrs to .DATA./.RDATA? */
DLL_SHARED BOOL Gen_PIC_Call_Shared = FALSE; /* CPIC */
DLL_SHARED BOOL Gen_PIC_Shared = FALSE;	/* PIC */
DLL_SHARED BOOL Gen_PIC_Calls = FALSE;	/* PIC calls */
DLL_SHARED BOOL Guaranteed_Small_GOT = TRUE; /* GOT < 64kB? */
DLL_SHARED BOOL Non_Volatile_GOT = FALSE;	/* GOT entries volatile? */
DLL_SHARED BOOL PIC_Local_Names = FALSE;	/* Names local by default? */
DLL_SHARED BOOL PIC_Protected_Names = FALSE; /* Names protected by default? */
DLL_SHARED BOOL PIC_Fixed_Addresses = FALSE; /* Names fixed by default? */
DLL_SHARED BOOL PIC_No_Page_Offset = FALSE;  /* Don't use page/offset addressing? */
DLL_SHARED BOOL Force_Mem_Formals = FALSE;	/* Always force formals to memory? */
DLL_SHARED BOOL Kernel_Code = FALSE;	/* Compiling OS kernel? */
DLL_SHARED BOOL Varargs_Prototypes = TRUE;	/* Varargs have prototypes for FP? */
DLL_SHARED BOOL Gen_Profile = FALSE;	/* Generate a profile call for each user call */
DLL_SHARED char *Gen_Profile_Name = "__profile_call"; 
DLL_SHARED BOOL Call_Mcount = FALSE;	/* generate a call to mcount in pu entry */
DLL_SHARED BOOL GP_Is_Preserved = FALSE;	/* GP is neither caller or callee-save */

DLL_SHARED char *Emit_Global_Data = NULL;	/* only emit global data */
DLL_SHARED char *Read_Global_Data = NULL;	/* only read global data */

DLL_SHARED char *Library_Name = NULL;      /* -TENV:io_library=xxx */
DLL_SHARED INT  target_io_library;

DLL_SHARED BOOL Meld_Schedule = FALSE;	/* Attempt to meld basic blocks	*/
DLL_SHARED BOOL Gap_Schedule = FALSE;	/* Attempt to perform gap scheduling */
DLL_SHARED BOOL Attempt_Bypass = FALSE;	/* Attempt to use bypass registers */
DLL_SHARED BOOL Isolate_Lines = FALSE;	/* Don't overlap source	lines */
DLL_SHARED BOOL Fill_Delay_Slots = FALSE;  /* Attempt to fill branch delay slots */
DLL_SHARED BOOL Enable_GDSE = FALSE;       /* Allow global dead store elimination */
DLL_SHARED BOOL Enable_CG_Peephole =FALSE;	/* Enable peephole optimization in cgprep */

#ifdef BACK_END
/* back end phases options */
DLL_SHARED BOOL Run_lno = FALSE;		    /* run loop-nest optimizer */
DLL_SHARED BOOL Run_wopt = FALSE;		    /* run WHIRL global optimizer */
DLL_SHARED BOOL Run_preopt = FALSE;	    /* run WHIRL preopt optimizer */
DLL_SHARED BOOL Run_ipl = FALSE;		    /* run procedure summary phase  */
DLL_SHARED BOOL Run_cg = FALSE;		    /* run code generator */
DLL_SHARED BOOL Run_w2c = FALSE;		    /* run whirl2c */
DLL_SHARED BOOL Run_w2f = FALSE;		    /* run whirl2f */
DLL_SHARED BOOL Run_w2fc_early = FALSE;	    /* run whirl2fc after LNO auto par*/
DLL_SHARED BOOL Run_purple = FALSE;	    /* run purple code instrumenter */
DLL_SHARED BOOL Run_prompf = FALSE;	    /* run to generate prompf analysis file */
DLL_SHARED char *LNO_Path = 0;		    /* path to lno.so */
DLL_SHARED char *WOPT_Path = 0;		    /* path to wopt.so */
DLL_SHARED char *CG_Path = 0;		    /* path to cg.so */
DLL_SHARED char *Ipl_Path = 0;		    /* path to ipl.so */
DLL_SHARED char *W2C_Path = 0;		    /* path to whirl2c.so */
DLL_SHARED char *W2F_Path = 0;		    /* path to whirl2f.so */
DLL_SHARED char *Purple_Path = 0;		    /* path to purple.so */
DLL_SHARED char *Prompf_Anl_Path = 0;	    /* path to prompf_anl.so */
DLL_SHARED WN_MAP Prompf_Id_Map = WN_MAP_UNDEFINED; 
			/* Maps WN constructs to unique identifiers */
#endif /* BACK_END */
DLL_SHARED char *Inline_Path = 0;                    /* path to inline.so */
#if defined(BACK_END) || defined(QIKKI_BE)
DLL_SHARED char *Targ_Path = 0;		    /* path to targ_info .so */
#endif /* defined(BACK_END) || defined(QIKKI_BE) */

/* ====================================================================
 *
 * Preconfigure
 *
 * Configure compiler options prior to flag processing.
 *
 * ====================================================================
 */

void
Preconfigure (void)
{
  OPTION_GROUP *og;

  /* Perform host-specific and target-specific configuration: */
  Preconfigure_Host ();
  Preconfigure_Target ();

  /* Initialize the option groups: */
  Initialize_Option_Groups ( Common_Option_Groups );

  og = Get_Command_Line_Group ( Common_Option_Groups, "TARG" );
  Set_Option_Internal ( og, "fp_regs" /* don't admit to this one */ );
  Set_Option_Internal ( og, "mips1" /* duplicates isa=mips1 */ );
  Set_Option_Internal ( og, "mips2" /* duplicates isa=mips2 */ );
  Set_Option_Internal ( og, "mips3" /* duplicates isa=mips3 */ );
  Set_Option_Internal ( og, "mips4" /* duplicates isa=mips4 */ );
  Set_Option_Internal ( og, "mips5" /* duplicates isa=mips5 */ );
  Set_Option_Internal ( og, "mips6" /* duplicates isa=mips6 */ );

  og = Get_Command_Line_Group ( Common_Option_Groups, "TENV" );
  Set_Option_Internal ( og, "pic1" /* same as -TENV:cpic */ );
  Set_Option_Internal ( og, "pic2" /* same as -TENV:pic */ );

#ifdef BACK_END
  /* -PHASE is just for driver and internal use -- don't expose it */
  og = Get_Command_Line_Group ( Common_Option_Groups, "PHASE" );
  Set_Option_Internal ( og, NULL );

  /* -INLINE and -IPA aren't passed properly to back end, so don't
   * confuse the poor users by printing the defaults (PV 645705)
   */
  og = Get_Command_Line_Group ( Common_Option_Groups, "INLINE" );
  Set_Option_Internal ( og, NULL );
  og = Get_Command_Line_Group ( Common_Option_Groups, "IPA" );
  Set_Option_Internal ( og, NULL );

#endif /* BACK_END */
}

/* ====================================================================
 *
 * Configure_Platform
 *
 * Process a platform name, setting Platform and Processor_Name, the
 * latter to be processed by Configure_Target.  The platform name may
 * come from either -OPT:Ofast=name or -TARG:platform=name.
 *
 * ====================================================================
 */

static void
Configure_Platform ( char *platform_name )
{
  PLATFORM_OPTIONS *popts;

  /* If we've already set the platform, and the new name is empty,
   * don't re-default it:
   */
  if ( Platform != IP0
    && ( platform_name == NULL || *platform_name == 0 ) )
  {
    return;
  }

  /* Get platform and its associated options: */
  popts = Get_Platform_Options ( platform_name );
  Platform = POPTS_id(popts);
  
  /* Set the per-IP options: */
  if ( Processor_Name == NULL ) {
    Processor_Name = POPTS_pname(popts);
  }
}

/* ====================================================================
 *
 * Configure_Ofast
 *
 * Configure option defaults which depend on -Ofast (the baseline SPEC
 * optimizaiton option).  These currently include:
 *
 *   -OPT:Olimit=0 -- no limit on optimization region size.
 *   -OPT:roundoff=3 -- do any mathematically valid rearrangement.
 *   -OPT:div_split=ON -- allow splitting a/b => a*recip(b).
 *   -OPT:speculative_null_ptr_deref=ON -- allow speculation past the null
 *				   	   ptr test. assumes page zero as 
 *					   readable.
 *   -OPT:alias=typed -- pointers to different types don't alias.
 *   -WOPT:copy_ops=OFF -- don't copy-propagate operations that the IVR
 *		can't handle (OFF by default now, but just in case...).
 *   -WOPT:estr_fb_injury=ON -- SSAPRE strength reduction uses
 *                              feedback frequency rather than loop
 *                              nesting to decide whether each IV
 *                              update should be viewed as injury or
 *                              kill.
 *
 * This must be done before abi/isa/processor configuration.
 *
 * ====================================================================
 */

static void
Configure_Ofast ( void )
{
  /* We assume that the driver has defaulted Opt_Level properly. */
  /* First set the options that are common to all targets: */
  if ( ! Olimit_Set ) {
    Olimit = 0;
    Olimit_Set = TRUE;
  }
  if ( ! Roundoff_Set ) {
    Roundoff_Level = ROUNDOFF_ANY;
    Roundoff_Set = TRUE;
  }
  if ( ! Div_Split_Set ) {
    Div_Split_Allowed = TRUE;
    Div_Split_Set = TRUE;
  }
/* #645549: There exists an OS bug which gets triggered by NULL ptr
   speculation. Disable NULL ptr speculation for Ofast (base flags).
   They will however continue to be turned ON for SPEC peak flags.

  if ( ! GCM_Eager_Null_Ptr_Deref_Set ) {
    GCM_Eager_Null_Ptr_Deref = TRUE;
    GCM_Eager_Null_Ptr_Deref_Set = TRUE;
  }
*/
  if ( ! Alias_Pointer_Types_Set ) {
    Alias_Pointer_Types = TRUE;
    Alias_Pointer_Types_Set = TRUE;
  }
  if ( ! WOPT_Enable_Copy_Prop_Bad_Ops_Set ) {
    WOPT_Enable_Copy_Prop_Bad_Ops = FALSE;
    WOPT_Enable_Copy_Prop_Bad_Ops_Set = TRUE;
  }
  if ( ! WOPT_Enable_Estr_FB_Injury_Set ) {
    WOPT_Enable_Estr_FB_Injury = TRUE;
    WOPT_Enable_Estr_FB_Injury_Set = TRUE;
  }

  /* Get platform and its associated options: */
  Configure_Platform ( Ofast );
}

/* ====================================================================
 *
 * Configure
 *
 * Configure compiler options based on flag values.
 *
 * ====================================================================
 */

#ifdef KEEP_WHIRLSTATS
/* defined in wn.c */
extern void whirlstats();
#endif


extern BOOL IR_set_dump_order (BOOL prefix); /* in ir_reader.c */

void
Configure (void)
{
  static BOOL dev_warn_toggled = FALSE;

  /* Hard compiler failure tracing is -ti4, soft is -ti8: */
  Hard_Failure_Info = Get_Trace	( TKIND_INFO, 4 );
  Soft_Failure_Info = Get_Trace	( TKIND_INFO, 8 );
  
#if !defined(SGI_FRONT_END_CPP)
  /* See if trees should be dumped in prefix order */
  if (Get_Trace(TKIND_INFO, TINFO_PREFIXDUMP)) {
     IR_set_dump_order(TRUE);
  }  
#endif

  if ( ! dev_warn_toggled ) {
    if ( Get_Trace( TP_MISC, 0x40 ) || getenv("XTENSA_DISABLE_DEVWARN")) {
      dev_warn_toggled = TRUE;
      DevWarn_Toggle();
    }
  }

#ifdef KEEP_WHIRLSTATS
  atexit(whirlstats);
#endif

  /* Configure the alias options first so the list is processed and
   * we can tell for -OPT:Ofast below what overrides have occurred:
   */
  Configure_Alias_Options ( Alias_Option );

  /* Check the -TARG:platform option (subordinate to Ofast): */
  if ( Platform_Name != NULL && *Platform_Name != 0 ) {
    Configure_Platform ( Platform_Name );
  }

  /* First, if -OPT:Ofast (a.k.a. SPEC) is set, configure defaults: */
  if ( Ofast != NULL ) {
    Configure_Ofast ();
  }


  /* Perform host-specific and target-specific configuration: */
  Configure_Host ();
  Configure_Target ();

  /* What size GOT to use?  Configure_Target sets it to small for
   * 32-bit pointers, large for 64-bit pointers.  Override it here
   * if TENV was used to specify it:
   */
  if ( Use_Large_GOT && Use_Small_GOT ) {
    /* Make up your mind! */
    ErrMsg ( EC_GOT_Size, Guaranteed_Small_GOT ? "small" : "large" );
  } else if ( Use_Large_GOT ) {
    Guaranteed_Small_GOT = FALSE;
  } else if ( Use_Small_GOT ) {
    Guaranteed_Small_GOT = TRUE;
  }

  if (Emit_Global_Data && Read_Global_Data) {
    /* Make up your mind! */
    FmtAssert (FALSE, ("can't specify options to both emit and read global data"));
  }
  else if (Emit_Global_Data) Global_File_Name = Emit_Global_Data;
  else if (Read_Global_Data) Global_File_Name = Read_Global_Data;

  /* Configure the treatment of short literals and data.  We give
   * precedence to the -TENV:short_lits=nnn:short_data=mmm options,
   * but -Gn can override either of them if not specified:
   */
  if ( ! Short_Lits_Set ) {
    Short_Lits = Max_Sdata_Elt_Size;
  }
  if ( ! Short_Data_Set ) {
    Short_Data = Max_Sdata_Elt_Size;
  }

  /* Turn on -OPT:Reorg_Common by default at -O3: */
  if ( ! OPT_Reorg_Common_Set && Opt_Level > 2 ) {
    OPT_Reorg_Common = TRUE;
  }

  /* Turn on -OPT:no_16bit_iv_wrap_around by default at -O3 */
  if ( ! OPT_IV_Short_Wrap_Around_Safe_Set && Opt_Level > 2 ) {
    OPT_IV_Short_Wrap_Around_Safe = TRUE;
  }
  
  if (Force_GP_Prolog)
    Force_Indirect_Call = TRUE;
}

/* ====================================================================
 *
 * Configure_Source
 *
 * Configure compiler options for each source file.
 *
 * Note	that we	set the	various	optimization and code generation
 * options at this level to ultimately allow per-source	respecification
 * with	pragmas.
 *
 * ====================================================================
 */

void
Configure_Source ( char	*filename )
  /**  NOTE: filename CAN BE NULL */
{
  /* Identify the source language: */
  if ( Language_Name != NULL ) {
    if ( strcasecmp ( Language_Name, "KR_C" ) == 0 ) {
      Language = LANG_KR_C;
    } else if ( strcasecmp ( Language_Name, "ANSI_C" ) == 0 ) {
      Language = LANG_ANSI_C;
    } else if ( strcasecmp ( Language_Name, "CPLUS" ) == 0 ) {
      Language = LANG_CPLUS;
    } else if ( strcasecmp ( Language_Name, "DELTA" ) == 0 ) {
      Language = LANG_DELTA;
    } else if ( strcasecmp ( Language_Name, "F77" ) == 0 ) {
      Language = LANG_F77;
    } else if ( strcasecmp ( Language_Name, "F90" ) == 0 ) {
      Language = LANG_F90;
    }
  }

  /* Configure the -DEBUG and -LIST groups: */
  DEBUG_Configure ();
  LIST_Configure ();

  /* Determine whether to use the CRAY or MIPS IO library */
  if (Library_Name != NULL) {
    if (strcasecmp ( Library_Name,"cray") == 0 ) {
       target_io_library = IOLIB_CRAY;
    } else if (strcasecmp ( Library_Name,"mips") == 0 ) {
       target_io_library = IOLIB_MIPS;
    }
  } else {
     /* For F90, use the CRAY libraries by default */
     if (Language == LANG_F90) {
	target_io_library = IOLIB_CRAY;
     /* For F77, use the MIPS libraries by default */
     } else {
	target_io_library = IOLIB_MIPS;
     }
  }

#ifdef BACK_END

  /* If we're invoking CITE, turn on whirl2c/f: */
  if ( List_Cite ) {
    if ( Language == LANG_F77 || Language == LANG_F90 ) {
      Run_w2f = TRUE;
    } else if ( Language == LANG_KR_C || Language == LANG_ANSI_C
	     || Language == LANG_CPLUS || Language == LANG_DELTA )
    {
      Run_w2c = TRUE;
    }
  }

  /* If we're invoking CITE, turn on whirl2c/f: */
  if (Run_w2fc_early) {
    if ( Language == LANG_F77 || Language == LANG_F90 ) {
      Run_w2f = TRUE;
    } else if ( Language == LANG_KR_C || Language == LANG_ANSI_C
	     || Language == LANG_CPLUS || Language == LANG_DELTA )
    {
      Run_w2c = TRUE;
    }
  }
#endif /* BACK_END */

  if ( Use_Large_GOT )	Guaranteed_Small_GOT = FALSE;

  /* if we get both TENV:CPIC and TENV:PIC, use only TENV:CPIC */
  if (Gen_PIC_Call_Shared && Gen_PIC_Shared) Gen_PIC_Shared = FALSE;

  /* Select optimization options: */

  /* Are we skipping any PUs for optimization? */
  Optimization_Skip_List = Build_Skiplist ( Opt_Skip );
  /* Are we skipping any regions for optimization? */
  Region_Skip_List = Build_Skiplist ( Region_Skip );

  /* F90 is a recursive language, so this needs to be set */
  if (!LANG_Recursive_Set && Language == LANG_F90)
     LANG_Recursive = TRUE;

  /* Since there seems to be little compile time reason not to be aggressive, 
   * make the folder aggressive by default
   */
  if ( ! Cfold_Aggr_Set )
    Enable_Cfold_Aggressive = TRUE;

  if (!Enable_CVT_Opt_Set)
    Enable_CVT_Opt = ( Opt_Level > 0);

  CSE_Elim_Enabled = Opt_Level > 0;

  /* Perform the %call16 -> %got_disp relocation change? */
  if ( ! Enable_GOT_Call_overridden )
    Enable_GOT_Call_Conversion = Opt_Level > 2;

  /* Force formal parameters to memory? */
  Force_Mem_Formals = ( Opt_Level < 1 );

  /* Optimize for space */
  if ( OPT_Space ) {

    if (!VHO_Switch_Density_Set) {
	VHO_Switch_Density = 3;
    }
    if (!VHO_Switch_If_Else_Set) {
        VHO_Switch_If_Else_Limit = 10;
    }
    /* TODO:  Other space optimizations to force? */
    if (!CG_memmove_inst_count_overridden)
      CG_memmove_inst_count = 8;
    if (! OPT_unroll_size_overridden)
      OPT_unroll_size = 20;
#if 0 /* not ready for this yet. */
    /* don't inline divide expansions */
    if (!OPT_Inline_Divide_Set) OPT_Inline_Divide = FALSE;
#endif

#ifdef BACK_END
    /* LNO options to be turned off for SPACE */
    LNO_Outer_Unroll_Max = 1;
    LNO_Split_Tiles = FALSE;
    VHO_Single_Loop_Test = TRUE;
#endif /* BACK_END */
  }

  /* symbolic debug stuff */
  Symbolic_Debug_Mode = SDM_NONE;
  if (Debug_Level > 0)
    Symbolic_Debug_Mode |= SDM_SEQLINE;
  if (Debug_Level > 0) {
    Symbolic_Debug_Mode |= SDM_SYMBOL;
    Symbolic_Debug_Mode |= SDM_LINE;
  }

#if 0
  /* Yuck!!! This FLOWOPT trace setting overlaps with an actual trace
     (TRACE_CLONE in cflow.cxx), and we shouldn't use the trace
     mechanism to disable things anyway! */
  
  /* Disabling splitting of long basic blocks: */
  Enable_BB_Splitting = ! Get_Trace ( TP_FLOWOPT, 0x080 );
#endif
  
  if (Opt_Level > 2 && ! Olimit_Set)
	Olimit = DEFAULT_O3_OLIMIT;
  if (Olimit == 0) {
	/* 0 Olimit means no limit or infinite limit */
	Olimit = MAX_OLIMIT;
  }
  else if (Olimit < 10) {
	/* olimit too small to work properly */
	DevWarn("Olimit < 10 is too small; resetting to Olimit=10");
	Olimit = 10;
  }
  if (Opt_Level == 0 && ! Olimit_opt_Set)
	Olimit_opt = FALSE;

#if !defined(SGI_FRONT_END_CPP) && !defined(QIKKI_BE)
  /* -OPT:rail or -OPT:rbi implies -OPT:compile_by_region
   * unless -OPT:compile_by_region=no is specified.
   */
  if (Regions_Around_Inner_Loops || Region_Boundary_Info)
    Set_PU_has_region (Get_Current_PU ());
#endif /* !defined(SGI_FRONT_END_CPP) && !defined(QIKKI_BE) */

  /* Enable IEEE_arithmetic options */
  if (Opt_Level > 2 && !IEEE_Arith_Set) {
     IEEE_Arithmetic = IEEE_INEXACT;
  }

  if ( ! Recip_Set )
    Recip_Allowed = ARCH_recip_is_exact;
  
  /* IEEE arithmetic options: */
  if ( IEEE_Arithmetic > IEEE_ACCURATE ) {
    /* Minor roundoff differences for inexact results: */
    if ( ! Recip_Set )
      Recip_Allowed = IEEE_Arithmetic >= IEEE_INEXACT;
    if ( ! Rsqrt_Set )
      Rsqrt_Allowed = IEEE_Arithmetic >= IEEE_INEXACT;
    /* Potential non-IEEE results for exact operations: */
    if ( ! Div_Split_Set )
      Div_Split_Allowed = IEEE_Arithmetic >= IEEE_ANY;
  }

  /* Constant folding options: */
  if ( ! Roundoff_Set && Opt_Level > 2 ) {
    Roundoff_Level = ROUNDOFF_ASSOC;
  }
  if ( Roundoff_Level > ROUNDOFF_NONE ) {

    /* The following allow minor roundoff differences: */
    if ( ! Fast_Exp_Set )
      Fast_Exp_Allowed = Roundoff_Level >= ROUNDOFF_SIMPLE;

    /* The following allows folding of intrinsics with constant arguments: */
    if ( ! Cfold_Intrinsics_Set )
      Enable_Cfold_Intrinsics = Roundoff_Level >= ROUNDOFF_SIMPLE;

    /* The following allows arbitrary reassociation: */
    if ( ! Cfold_Reassoc_Set )
      Enable_Cfold_Reassociate = Roundoff_Level >= ROUNDOFF_ASSOC;

    /* reassociate in the lowerer to find MADD oportunities */
    if ( ! Enable_NaryExpr_Set )
      Enable_NaryExpr = Roundoff_Level >= ROUNDOFF_ASSOC;

    if (!Fast_Complex_Set)
      Fast_Complex_Allowed = Roundoff_Level >= ROUNDOFF_ANY;
    if (!Fast_NINT_Set)
      Fast_NINT_Allowed = Roundoff_Level >= ROUNDOFF_ANY;
    if (!Fast_trunc_Set)
      Fast_trunc_Allowed = Roundoff_Level >= ROUNDOFF_SIMPLE;

    if ( ! CIS_Set )
      CIS_Allowed |= Roundoff_Level >= ROUNDOFF_SIMPLE;
  }

#if 0
  /* Set the relational operator folding in simplifier based on the optimizer
     setting of Allow_wrap_around_opt */
  if (!Simp_Unsafe_Relops_Set && Allow_wrap_around_opt_Set) {
     Simp_Unsafe_Relops = Allow_wrap_around_opt;
  }
  if (!Allow_wrap_around_opt_Set && Simp_Unsafe_Relops_Set ) {
     Allow_wrap_around_opt = Simp_Unsafe_Relops;
  }
#endif
  if (!Simp_Unsafe_Relops_Set && Opt_Level > 2) {
     Simp_Unsafe_Relops = TRUE;
  }
  
  Enable_GDSE	 = ((Opt_Level > 1) &&
		    (!Get_Trace(TP_GLOBOPT, 4096))
		    );
  /* The lowerer will do a simple treeheight reduction for
   * binary commutative ops
   */
  if (!OPT_Lower_Treeheight_Set)
     OPT_Lower_Treeheight = (Opt_Level > 1);

  /* Perform host-specific and target-specific configuration: */

  /**  NOTE: filename CAN BE NULL */
  
  Configure_Source_Host ( filename );
  Configure_Source_Target ( filename );

  /* Set eagerness-level-based information.  This must come after the
   * call to Configure_Source_Target, since that routine sets the
   * FP exception enable masks.
   */
  if ( ! Eager_Level_Set && Opt_Level > 2 ) {
    Eager_Level = EAGER_ARITH;
  }
  if ( Eager_Level >= EAGER_ARITH ) {
    FP_Exception_Enable_Max &= ~(FPX_I|FPX_U|FPX_O|FPX_V);
  }
  if ( Eager_Level >= EAGER_DIVIDE ) {
    FP_Exception_Enable_Max &= ~FPX_Z;
  }
  if ( Eager_Level >= EAGER_MEMORY ) {
    Force_Memory_Dismiss = TRUE;
  }

#ifdef BACK_END
  /* Configure the -LNO group: */
  LNO_Configure ();
#endif /* BACK_END */

  /* Trace options: */
  if ( Get_Trace ( TP_MISC, 128 ) ) {
    Trace_Option_Groups ( TFile, Common_Option_Groups, TRUE );
  } else if ( Get_Trace ( TP_MISC, 32 ) ) {
    Trace_Option_Groups ( TFile, Common_Option_Groups, FALSE );
  }
}

/* ====================================================================
 *
 * Configure_Alias_Options
 *
 * Configure the options related to alias analysis.
 *
 * ====================================================================
 */

void
Configure_Alias_Options( OPTION_LIST *olist )
{
  OPTION_LIST *ol;
  for (ol = olist; ol != NULL; ol = OLIST_next(ol)) {
    char *val = OLIST_val(ol);
    INT len = strlen (val);
    if (strncasecmp( val, "any", len) == 0) {
      Alias_Pointer_Parms = FALSE;	/* observed by Fortran programs */
      Alias_Pointer_Cray = FALSE;	/* observed by Fortran programs */
      Alias_Pointer_Types = FALSE;	/* observed by C and C++ programs */
      Alias_Not_In_Union  = FALSE;	/* observed by C++ programs only */
      Alias_Pointer_Strongly_Typed = FALSE;	/* observed by C and C++ programs */
      Alias_Pointer_Types_Set = TRUE;
      Alias_Not_In_Union_Set  = TRUE;	/* observed by C++ programs only */
      Alias_Pointer_Named_Data = FALSE;	/* observed by C and C++ programs */
      Alias_Pointer_Restricted = FALSE;	/* observed by C and C++ programs */
      Alias_Pointer_Disjoint   = FALSE;
    } else if (strncasecmp( val, "parm", len) == 0) {
      Alias_Pointer_Parms = TRUE;
    } else if (strncasecmp( val, "typed", len) == 0) {
      Alias_Pointer_Types = TRUE;
      Alias_Pointer_Types_Set = TRUE;
    } else if (strncasecmp( val, "unnamed", len) == 0) {
      Alias_Pointer_Named_Data = TRUE;
    } else if (strncasecmp( val, "nounion",len) == 0) {
      Alias_Not_In_Union  = TRUE;	/* observed by C++ programs only */
      Alias_Not_In_Union_Set  = TRUE;	/* observed by C++ programs only */
    } else if (strncasecmp( val, "restricted", len) == 0) {
      Alias_Pointer_Restricted = TRUE;
      Alias_Pointer_Named_Data = TRUE;
    } else if (strncasecmp( val, "disjoint", len) == 0) {
      Alias_Pointer_Disjoint = TRUE;
      Alias_Pointer_Restricted = TRUE;
      Alias_Pointer_Named_Data = TRUE;
    } else if (strncasecmp( val, "cray_pointer", len) == 0) {
      Alias_Pointer_Cray = TRUE;
    } else if (strncasecmp( val, "strongly_typed", len) == 0) {
      Alias_Pointer_Strongly_Typed = TRUE;
    } else if (strncasecmp( val, "no_parm", len) == 0) {
      Alias_Pointer_Parms = FALSE;
    } else if (strncasecmp( val, "no_typed", len) == 0) {
      Alias_Pointer_Types = FALSE;
      Alias_Pointer_Types_Set = TRUE;
    } else if (strncasecmp( val, "no_unnamed", len) == 0) {
      Alias_Pointer_Named_Data = FALSE;
    } else if (strncasecmp( val, "no_restricted", len) == 0) {
      Alias_Pointer_Restricted = FALSE;
      Alias_Pointer_Named_Data = FALSE;
    } else if (strncasecmp( val, "no_disjoint", len) == 0) {
      Alias_Pointer_Disjoint = FALSE;
      Alias_Pointer_Named_Data = FALSE;
    } else if (strncasecmp( val, "no_cray_pointer", len) == 0) {
      Alias_Pointer_Cray = FALSE;
    } else if (strncasecmp( val, "no_strongly_typed", len) == 0) {
      Alias_Pointer_Strongly_Typed = FALSE;
    } else if (strncasecmp( val, "cckr_default", len) == 0) {
      Alias_Pointer_Cckr = TRUE;
    } else if (strncasecmp( val, "common_scalar", len) == 0) {
      Alias_Common_Scalar = TRUE;
    } else if (strncasecmp( val, "no_common_scalar", len) == 0) {
      Alias_Common_Scalar = FALSE;
    } else if (strncasecmp( val, "no_f90_pointer_alias", len) == 0) {
      Alias_F90_Pointer_Unaliased = TRUE;
    } else if (strncasecmp( val, "f90_pointer_alias", len) == 0) {
      Alias_F90_Pointer_Unaliased = FALSE;
    } else {
      ErrMsg ( EC_Inv_OPT, "alias", val );
    }
  }

  /* If we didn't explicitly set alias=typed, and this is a -cckr
   * compilation, turn off Alias_Pointer_Types:
   */
  if ( ! Alias_Pointer_Types_Set && Alias_Pointer_Cckr ) {
    Alias_Pointer_Types = FALSE;
  }
}

/* ====================================================================
 *
 * SKIPLIST
 *
 * Support for lists of PU numbers to skip (e.g. for optimization)
 * based on options in a command line group.  A typedef for SKIPLIST
 * is defined in config.h, but the type is opaque outside; for now,
 * Build_Skiplist and Query_Skiplist are the only visible interface.
 *
 * This interface is sufficiently generic that it could go into flags.*
 * in common/util.  It isn't there because that could end up breaking
 * the principle that flags.h should not be widely included (i.e. other
 * than by command line processing).
 *
 * ====================================================================
 */

typedef enum {
  SK_NONE,	/* End of list */
  SK_AFTER,	/* Values after this one */
  SK_BEFORE,	/* Values before this one */
  SK_EQUAL	/* Just this one */
} SKIPKIND;

struct skiplist {
  mINT32 size;	/* Number of elements */
  mINT8 *kind;	/* Array of kinds */
  mINT32 *val;	/* Array of values */
};

#define SKIPLIST_size(sl)		((sl)->size)
#define SKIPLIST_kind_vec(sl)		((sl)->kind)
#define SKIPLIST_kind(sl,i)		((SKIPKIND)((sl)->kind[i]))
#define Set_SKIPLIST_kind(sl,i,v)	(((sl)->kind[i]) = (mINT8)(v))
#define SKIPLIST_val_vec(sl)		((sl)->val)
#define SKIPLIST_val(sl,i)		((sl)->val[i])

/* ====================================================================
 *
 * Print_Skiplist
 *
 * Print a skiplist.  
 *
 * ====================================================================
 */

static void
Print_Skiplist ( FILE *tf, SKIPLIST *skip, char *lab )
{
  INT32 i;

  if ( skip == NULL ) {
    fprintf ( tf, "SKIPLIST %s empty\n", lab );
    return;
  }
  fprintf ( tf, "SKIPLIST %s:\n", lab );

  for ( i = 0; SKIPLIST_kind(skip,i) != SK_NONE; i++ ) {
    switch ( SKIPLIST_kind(skip,i) ) {
      case SK_EQUAL:
	fprintf ( tf, "  equal %d\n", SKIPLIST_val(skip,i) );
	break;
      case SK_AFTER:
	fprintf ( tf, "  after %d\n", SKIPLIST_val(skip,i) );
	break;
      case SK_BEFORE:
	fprintf ( tf, "  before %d\n", SKIPLIST_val(skip,i) );
	break;
    }
  }
  fprintf ( tf, "SKIPLIST %s end\n\n", lab );
}

/* ====================================================================
 *
 * Build_Skiplist
 *
 * Build a skiplist from a group option list.  For now, we assume that
 * the only choices are skip_a* (after) skip_b* (before), and skip_e*
 * (equal).  See Query_Skiplist below for the list semantics.
 *
 * Note that we interpret skip_equal=1,3-5,7-10,12,35-39 as you might
 * hope.
 *
 * WARNING:  This routine does no error checking.  This option is for
 * internal use, and if the syntax is wrong, strange (non-fatal) things
 * may happen (typically ignoring the rest of the option).
 *
 * ====================================================================
 */

SKIPLIST *
Build_Skiplist ( OPTION_LIST *olist )
{
  INT32 count = 0;
  OPTION_LIST *ol;
  SKIPLIST *sl;
  BOOL list_found = FALSE;
  char *p;

  /* Count the elements: */
  if ( olist == NULL ) return NULL;
  for ( ol = olist; ol != NULL; ol = OLIST_next(ol) ) {

    /* At least one entry: */
    ++count;
    
    /* Check for commas and ranges: */
    p = OLIST_val(ol);
    while ( *p != ':' && *p != 0 ) {
      if ( *p == ',' || *p == '-' ) {
	++count;
	list_found = TRUE;
      }
      ++p;
    }
  }

  /* Allocate the skiplist: */
  sl = (SKIPLIST *) malloc ( sizeof(SKIPLIST) );
  SKIPLIST_size(sl) = count+1;
  SKIPLIST_kind_vec(sl) = (mINT8 *) calloc ( sizeof(mINT8), count+1 );
  SKIPLIST_val_vec(sl) = (mINT32 *) calloc ( sizeof(mINT32), count+1 );

  /* Fill the skiplist: */
  for ( count = 0, ol = olist;
	ol != NULL;
	++count, ol = OLIST_next(ol) )
  {
    if ( !strncmp ( "skip_a", OLIST_opt(ol), 6 ) ||
	 !strncmp ( "region_skip_a", OLIST_opt(ol), 13 ) ) {
      Set_SKIPLIST_kind ( sl, count, SK_AFTER );
    } else if ( !strncmp ( "skip_b", OLIST_opt(ol), 6 ) ||
	        !strncmp ( "region_skip_b", OLIST_opt(ol), 13 ) ) {
      Set_SKIPLIST_kind ( sl, count, SK_BEFORE );
    } else {
      Set_SKIPLIST_kind ( sl, count, SK_EQUAL );
    }
    SKIPLIST_val(sl,count) = atoi ( OLIST_val(ol) );

    /* If this is skip_equal, look for a list... */
    if ( list_found && SKIPLIST_kind(sl,count) == SK_EQUAL ) {
      p = OLIST_val(ol);
      while ( *p >= '0' && *p <= '9' ) ++p;
      if ( *p == '-' ) {
	Set_SKIPLIST_kind ( sl, count, SK_AFTER );
	--SKIPLIST_val(sl,count);
	++p;
	++count;
	Set_SKIPLIST_kind ( sl, count, SK_BEFORE );
	SKIPLIST_val(sl,count) = 1 + atoi ( p );
	while ( *p >= '0' && *p <= '9' ) ++p;
      }
      while ( *p++ == ',' ) {
	++count;
	Set_SKIPLIST_kind ( sl, count, SK_EQUAL );
	SKIPLIST_val(sl,count) = atoi ( p );
	while ( *p >= '0' && *p <= '9' ) ++p;
	if ( *p == '-' ) {
	  Set_SKIPLIST_kind ( sl, count, SK_AFTER );
	  --SKIPLIST_val(sl,count);
	  ++p;
	  ++count;
	  Set_SKIPLIST_kind ( sl, count, SK_BEFORE );
	  SKIPLIST_val(sl,count) = 1 + atoi ( p );
	  while ( *p >= '0' && *p <= '9' ) ++p;
	}
      }
    }
  }
  Set_SKIPLIST_kind ( sl, count, SK_NONE );

  if ( Get_Trace ( TP_MISC, 0x80 ) ) {
    Print_Skiplist ( TFile, sl, "Build_Skiplist" );
  }

  return sl;
}

/* ====================================================================
 *
 * Query_Skiplist
 *
 * Query a skiplist.  A TRUE result means that the element passed is in
 * the skiplist.  The semantics of the list is as follows:  Return TRUE
 * if elmt is equal to an SK_EQUAL element of the list.  Return TRUE if
 * elmt is greater than an SK_AFTER element AND it is less than an
 * immediately following SK_BEFORE element; otherwise skip over the
 * following SK_BEFORE element.  Return TRUE if elmt is smaller than an
 * SK_BEFORE which does not immediately follow an SK_AFTER.  If nothing
 * on the list produces a TRUE result, return FALSE.  That is, a list
 * consists of SK_EQUAL elements, SK_AFTER/SK_BEFORE pairs in that
 * order, or SK_AFTER and SK_BEFORE elements that aren't in such pairs.
 * Any match of one of these tests causes a skip.
 *
 * ====================================================================
 */

BOOL
Query_Skiplist ( SKIPLIST *skip, INT32 elmt )
{
  INT32 i;
  BOOL ok;

  if ( skip == NULL ) return FALSE;

  for ( i = 0; SKIPLIST_kind(skip,i) != SK_NONE; i++ ) {
    switch ( SKIPLIST_kind(skip,i) ) {
      case SK_EQUAL:
	/* printf ( "<skip> equal %d ? %d\n", SKIPLIST_val(skip,i), elmt ); */
	if ( SKIPLIST_val(skip,i) == elmt ) return TRUE;
	break;
      case SK_AFTER:
	ok = ( SKIPLIST_val(skip,i) < elmt );
	/* printf ( "<skip> after %d ? %d\n", SKIPLIST_val(skip,i), elmt ); */
	if ( SKIPLIST_kind(skip,i+1) == SK_BEFORE 
	  && SKIPLIST_val(skip,i+1) > SKIPLIST_val(skip,i)) 
	{
	  if ( SKIPLIST_val(skip,++i) <= elmt ) ok = FALSE;
	}
	if ( ok ) return TRUE;
	break;
      case SK_BEFORE:
	/* printf ( "<skip> before %d ? %d\n", SKIPLIST_val(skip,i), elmt ); */
	if ( SKIPLIST_val(skip,i) > elmt ) return TRUE;
	break;
    }
  }
  return FALSE;
}

/* ====================================================================
 *
 * Process_Trace_Option
 *
 * Given a trace option -t..., process it.  The caller should already
 * have determined that it cannot be anything else.  The option should
 * be the full option string, with leading -t... (for error messages).
 * such options may be catenated, except for those which take name
 * operands which can't be delimited, which must be last.
 *
 * ====================================================================
 */

BOOL
Process_Trace_Option ( char *option )
{
  char *cp = option+1;
  INT32 phase;

  Tracing_Enabled = TRUE;

  while ( cp && *cp == 't' ) {
    cp += 2;

    switch ( *(cp-1) ) {
    case 'a':
	Set_Trace (TKIND_ALLOC,
		   Get_Trace_Phase_Number ( &cp, option ) );
	break;

    case 'b':
	Set_Trace (TKIND_BB,
		   Get_Numeric_Flag (&cp, 0, 32767, 0, option));
	break;

    case 'c':
	Set_Trace (TKIND_CTRL,
		   Get_Numeric_Flag (&cp, 0, 32767, 0, option));
	break;

    case 'd':
	Set_Trace (TKIND_DEBUG,
		   Get_Numeric_Flag (&cp, 1, 32767, 0, option));
	break;

    /* ex: -Wb,-tf0 for function 0 in the file */
    case 'f':
	if ( isdigit (*cp) ) {
	  Set_Trace_Pu_Number (
	      Get_Numeric_Flag (&cp, 0, 0x7fffffff, 0, option) );
	} else {
	  Set_Trace_Pu ( cp );
	  cp = 0;		/* Done with this flag */
	}
	break;

    /* ex: -Wb,-tg1 for region 1 in each function (best when used with -tf) */
    case 'g':
	if ( isdigit (*cp) ) {
	  Set_Trace_Region_Number (
	      Get_Numeric_Flag (&cp, 0, 0x7fffffff, 0, option) );
	} else
	  Is_True(FALSE,("Process_Trace_Option: regions don't have names"));
	break;

    case 'i':
	Set_Trace (TKIND_INFO,
		   Get_Numeric_Flag (&cp, 1, 32767, 0, option));
	break;

    case 'n':
	Set_Trace (TKIND_TN,
		   Get_Trace_Phase_Number ( &cp, option ) );
	break;
    
    case 'p':
	phase = Get_Trace_Phase_Number ( &cp, option );
	if ( phase != 0 && (*cp == ',' || *cp == '\0'))
	  Set_Trace (TKIND_XPHASE, phase);
	break;
    
    case 'r':
	Set_Trace (TKIND_IR,
		   Get_Trace_Phase_Number ( &cp, option ) );
	break;

    case 's':
	Set_Trace (TKIND_SYMTAB,
		   Get_Trace_Phase_Number ( &cp, option ) );
	Symbol_Table_Out = TRUE;
	break;

    case 't':
	phase = Get_Trace_Phase_Number ( &cp, option );
	if ( *cp != ',' && *cp != ':' ) {
	    ErrMsg ( EC_Trace_Flag, *cp, option );
	    break;
	}
	cp++;	    /* skip separator */
	Set_Trace (phase,
		   Get_Numeric_Flag (&cp, 1, 0xffffffff, 0,
				     option));
	break;

#ifdef FRONT_END
    case 'v':
	{
	  /* Used by the EDG front ends to control tracing verbosity: */
	  extern BOOL trace_verbose;	/* Type must match a_boolean */
	  trace_verbose = TRUE;
	}
	break;
#endif

    case 0:   
	ErrMsg ( EC_Trace_Flag, '?', option );
	return FALSE;

    default:
	--cp;
	ErrMsg ( EC_Trace_Flag, *cp, option );
	return FALSE;
    }

  }

  if ( cp && *cp != 0 ) {
    ErrMsg ( EC_Trace_Flag, '?', option );
    return FALSE;
  }
  return TRUE;

}

/* ====================================================================
 *
 * List_Compile_Options
 *
 * List options to the given file.  This is primarily an interface to
 * the flags.c routine Print_Option_Groups, but also prints a number of
 * non-group options.  The "internal" flag indicates whether to list
 * internal-only options.  The "full" flag indicates whether to list
 * all options if "internal" is set; otherwise option group listing
 * is controlled by List_Options and List_All_Options.
 *
 * ====================================================================
 */

void
List_Compile_Options (
  FILE *f,
  char *pfx,
  BOOL internal,
  BOOL full,
  BOOL update )
{
  char *bar = SBar+12;	/* Shorten it a bit */

  fprintf ( f, "\n%s%s%s Compiling %s (%s)\n%s%s",
	    pfx, bar, pfx, Src_File_Name, Irb_File_Name, pfx, bar ); 
  fprintf ( f, "\n%s%s%s Options:\n%s%s", pfx, bar, pfx, pfx, bar );

  fprintf ( f, "%s  Target:%s, ISA:%s, Pointer Size:%d\n",
	    pfx, Targ_Name (Target), Isa_Name (Target_ISA),
	    (Use_32_Bit_Pointers ? 32 : 64) );
  fprintf ( f, "%s  -O%d\t(Optimization level)\n", pfx, Opt_Level );
  fprintf ( f, "%s  -g%d\t(Debug level)\n", pfx, Debug_Level );
  
  if ( Min_Error_Severity == ES_ADVISORY )
      fprintf ( f, "%s  -m2\t(Report advisories)\n", pfx );
  else if ( Min_Error_Severity == ES_WARNING )
      fprintf ( f, "%s  -m1\t(Report warnings)\n", pfx );
  else
      fprintf ( f, "%s  -m0\t(Report errors only)\n", pfx );
  
  fprintf ( f, "%s%s\n", pfx, bar );

  if ( internal || List_Options ) {
    fprintf ( f, "%s Group options are marked with '#' if changed,\n"
		 "%s or with '=' if explicitly set to default value.\n",
	      pfx, pfx );
    Print_Option_Groups ( f, Common_Option_Groups, pfx, internal,
			  internal ? full : List_All_Options , update );
  }
}

