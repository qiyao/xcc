
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


#ifndef config_INCLUDED
#define config_INCLUDED

/* ====================================================================
 * ====================================================================
 *
 * Module: config.h
 * $Revision: 2.211 $
 * $Date: 2000/10/23 21:46:09 $
 * $Author: mpm $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config.h,v $
 *
 * Revision history:
 *  09-Apr-90 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  15-Jun-91 - Restructuring, Josie integration
 *
 * Description:
 *
 * Declare parameters describing the current host/target configuration,
 * and configuration options selected by the user in the command line.
 *
 * ====================================================================
 * NOTE:  We want to split out the declarations for various option
 * groups, so that adding to them doesn't cause virtually everything
 * to compile.  These are declared in files named
 * common/com/config_groupname.h, and should be used instead of adding
 * to this file, although many of the -OPT group options are still
 * here (feel free to move them).
 * ====================================================================
 *
 * Most of the compiler is designed to be independent of the host
 * (the machine and system where the compiler runs) and of the target
 * (the machine and system where the compiled code will run).  This
 * file (with those it includes) is the central repository for
 * parameters and data which help isolate those parts of the compiler
 * which are not host- and/or target-independent.  Note that we assume
 * in general that the "host" for building the compiler and the "host"
 * for running the compiler are the same.  Although this need not be
 * true, it generally will be except for bootstrap phases.  Given a
 * cross-compiler on the build host which is reasonably compatible with
 * the eventual native compiler on the compilation host, the
 * differences in the compiler code should be limited to type
 * definitions in defs.h and conceivably to the tdgen programs, which
 * will be built and run on the build host.
 *
 * CONFIGURATION PARAMETER CLASSES
 *
 * We recognize several classes of such parameters -- the following
 * attempts to provide guidelines on where to declare them and where
 * to process them.
 *
 * In the following description, "TARGET" refers to the directory
 * containing the host- and target-specific sources and build
 * directories; in the Twin Peaks case, it is "tp".
 *
 *  Ubiquitous:	Some configuration parameters are used essentially
 *		everywhere in the compiler, such as the basic data
 *		type declarations INT32, TN_NUM, and the like.
 *		Notwithstanding the following classifications, these
 *		are declared in TARGET/com/defs.h, which is included
 *		in (virtually) all source files.
 *
 *  Host Machine: Some parameters reflect the capabilities of the host
 *		hardware.  The most important other than the basic
 *		types defined in defs.h involves the representation
 *		and manipulation of target constants on the host
 *		machine, declared in file TARGET/com/targ_const.h.
 *		The remainder will be defined in the file
 *		TARGET/com/config_host.h, which is included in this
 *		file.
 *
 *  Host System: Some parameters reflect the configuration of the host
 *		software, e.g. the pathnames of library files, the
 *		syntax of the assembler, etc.  Those required broadly
 *		will be declared in TARGET/com/config_host.h; those
 *		with a more restricted clientele (e.g. the assembler
 *		interface) will be maintained in more specific files.
 *
 *		Note that we include in this class parameters which
 *		depend on which compiler process is being built, i.e.
 *		driver, front end, or back end.  Definitions specific
 *		to a single process should be #ifdef'ed based on
 *		DRIVER, FRONT_END, or BACK_END.
 *
 *  Target Machine: Most parameters dependent on the target machine
 *		are defined by the Target Information facility, and
 *		are used in general for driving the code generation process
 *		throughout the compiler.  The interface to the target
 *		information is in the form of generated header files
 *		in TARGET/targ_info. The generator programs and their
 *		input are all under common/targ_info.
 *
 *		The second significant component of this class is the
 *		handling of target constants; as described above, the
 *		interface to this functionality is in the file
 *		TARGET/com/targ_const.h.
 *
 *		Other parameters in this class should be declared in
 *		TARGET/com/config_targ.h.
 *
 *  Target System: Parameters in this class, e.g. runtime library
 *		interface information, will generally have a limited
 *		clientele, and will be in files reflecting this usage.
 *		Some such information is in the TDT, either due to its
 *		effect on code generation or due to the need to
 *		preprocess it in tdgen.  Anything else should be in
 *		TARGET/com/config_targ.h.
 *
 *  Compiler:	Parameters in this class control processing choices
 *		made by the compiler, independent of host or target,
 *		e.g. which optimizations to perform, how to schedule,
 *		and the like.  They will generally be declared in the
 *		driver modules for the relevant phases (e.g.
 *		be/cg/cg.h, be/gopt/opt.h), and initialized by the
 *		configuration routines (see below).
 *
 *  Options:	Many configuration parameters are set or affected by
 *		options specified by the command line or environment.
 *		Such parameters which need to be widely available will
 *		be in this file; those required only by command line
 *		processing will be in flags.h.  NOTE: choices made
 *		based on user interface flags like optimization level
 *		should be implemented by declaring a control variable
 *		and setting it in the configuration routines.  We want
 *		to be able to modify the effect of specific flag
 *		settings in a central place where the interactions are
 *		clear, rather than by searching the compiler for who
 *		depends on something like optimization level.
 *
 *		NOTE ALSO:  There is a general interface to compiler
 *		controls, settable via either the command line or
 *		pragmas, in controls.h.  It will still be preferable
 *		to interface to a control variable declared here, but
 *		the general, direct interface is available there.
 *
 *  Global Data: Such data, of a non-configuration nature (e.g. the
 *		current file names), used to be declared in flags.h.
 *		It is now declared in the process' miscellaneous
 *		globals header (i.e. glob.h) and defined in the
 *		process driver (e.g. bedriver.c for the back end).
 *
 * CONFIGURATION PROCESSING
 *
 * Each of the configuration files described above has an associated
 * source file with three external routines (at least):
 *
 *  Preconfigure_Xxx:  Does configuration required before flag
 *		processing (Xxx = Host, Target, etc.).
 *
 *  Configure_Xxx:  Does configuration required after flag processing.
 *
 *  Configure_Source_Xxx:  Does configuration required for each source
 *		file compiled.
 *
 * Processing will involve calling, at compiler initialization time,
 * the Preconfigure routines, flag processing, and the Configure
 * routines.  Then, as each source file is processed, the driver will
 * call the Configure_Source routine.
 *
 * CONTENTS
 *
 * - Memory allocation configuration.
 *
 *
 * Exported variables:
 *
 *	TODO: Cleanup organization of the *_Level and Max_Sdata_Elt_Size
 *	controls.  For example, the Debug and Profile maximums and defaults
 *	are determined by identical code in fedriver.c and bedriver.c.
 *	Eagerness levels need their own enumerated type.
 *
 *	INT8 Opt_Level
 *	    Optimization level, as set by -0 switch.  Bounded by
 *	    MAX_OPT_LEVEL.  Default is DEF_OPT_LEVEL if -O not given,
 *	    or DEF_O_LEVEL if -O given with no argument.
 *
 *	INT8 Debug_Level
 *	    Debug level, as set by -g switch.  Bounded by MAX_DEBUG_LEVEL.
 *	    Defaults to DEF_DEBUG_LEVEL if -g not given or 2 if -g given
 *	    with no argument (see fedriver.c, bedriver.c).
 *
 *	INT32 Max_Sdata_Elt_Size
 *	    Maximum size of data elements allowed in .sdata/.sbss 
 *	    (short data/bss) sections.  Bounded by MAX_SDATA_ELT_SIZE.
 *	    Defaults to DEF_SDATA_ELT_SIZE.
 *
 *	INT16 Eager_Level
 *	    Eagerness level for speculative operations, as set by -X
 *	    switch.  Valid levels are:
 *	     EAGER_NONE		No speculative operations
 *	     EAGER_SAFE		Only exception-safe speculative ops
 *	     EAGER_ARITH	Arithmetic exceptions off
 *	     EAGER_DIVIDE	Divide by zero exceptions off
 *	     EAGER_MEMORY	Memory exceptions off
 *	     EAGER_OTHER	All speculative ops allowed
 *	    Each level includes the levels above (in this text) it.
 *	    Default is EAGER_SAFE.
 *
 *
 * SEE ALSO
 *
 *   common/com/MIPS/config_platform.h -- target platform configuration
 *   common/com/MIPS/config_targ.h -- target processor configuration
 *   common/com/MIPS/config_host.h -- compiler host configuration
 *   common/com/defs.h -- ubiquitous host types and configuration
 *			  options.
 *   common/util/flags.h -- command-line processing utilities.
 *
 *   common/com/config_ipa.h -- -IPA/-INLINE group options.
 *   common/com/config_opt.h -- -OPT group options.
 *   common/com/config_wopt.h -- -WOPT group options.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *config_rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config.h,v $ $Revision: 2.211 $";
#endif /* _KEEP_RCS_ID */

#include "config_host.h"	/* in TARGET/com */
#include "config_targ.h"	/* in TARGET/com */


/* some systems define these, some don't */
#ifndef BIG_ENDIAN
#define BIG_ENDIAN      4321
#endif
#ifndef LITTLE_ENDIAN
#define LITTLE_ENDIAN   1234
#endif

#include "mempool.h"	/* Include the "fundamental" routines */
#include "flags.h"

/* IR builder sometimes needs to know whether we're in front end,
 * either to do something differently, or to complain about attempts
 * to handle something that shouldn't be present.  Note that the
 * arguments to the *Assert_Front_End macros are parenthesized
 * parameter lists to ErrMsg or printf.
 */
#ifdef SINGLE_PROCESS
  extern DLL_SHARED INT16 In_Front_End;
# ifdef Is_True_On
#   define Assert_Front_End(x)		Assert ( In_Front_End, x )
#   define FmtAssert_Front_End(x)	FmtAssert ( In_Front_End, x )
# endif
#else
# ifdef FRONT_END
#   define In_Front_End	TRUE
# else
#   define In_Front_End	FALSE
#   ifdef Is_True_On
#     define Assert_Front_End(x)	ErrMsg x
#     define FmtAssert_Front_End(x)	FmtAssert ( FALSE, x )
#   endif
# endif
#endif

#ifndef Assert_Front_End
# define Assert_Front_End(x)	(void)0
# define FmtAssert_Front_End(x)	(void)0
#endif



/* ====================================================================
 *
 * Miscellaneous configuration options
 *
 * ====================================================================
 */

/***** Language being compiled -- initialized in flags.c *****/
#include "language.h"

#ifdef __cplusplus
extern "C" {
#endif

extern DLL_SHARED LANGUAGE Language;

/* What is the model to be used for logical values in Fortran?
 *   TRUE:	.TRUE. is 1		(i.e. standard C/Unix model).
 *		truth test is zero/non-zero.
 *   FALSE:	.TRUE. is -1		(i.e. VMS Fortran model).
 *		truth test is LSB test.
 * This variable is defaulted to TRUE; the Fortran FE must set it to
 * FALSE prior to any IRB conversions if the VMS model is desired.
 */
extern DLL_SHARED	BOOL Use_C_Like_Logicals;

/***** LANGuage group options *****/
extern DLL_SHARED BOOL CXX_Bool_On;
extern DLL_SHARED BOOL CXX_Bool_Set;
extern DLL_SHARED BOOL CXX_Exceptions_On;
extern DLL_SHARED BOOL CXX_Exceptions_Set;
extern DLL_SHARED BOOL CXX_Alias_Const;
extern DLL_SHARED BOOL CXX_Alias_Const_Set;
extern DLL_SHARED BOOL LANG_Recursive;	/* Fortran program contains recursion */
extern DLL_SHARED BOOL LANG_Recursive_Set;
extern DLL_SHARED BOOL CXX_Wchar_On;
extern DLL_SHARED BOOL CXX_Wchar_Set;

extern DLL_SHARED BOOL CXX_Namespaces_On;
extern DLL_SHARED BOOL CXX_Namespaces_Set;
extern DLL_SHARED BOOL CXX_Ansi_For_Init_Scope_On;
extern DLL_SHARED BOOL CXX_Ansi_For_Init_Scope_Set;
extern DLL_SHARED BOOL CXX_Explicit_On;
extern DLL_SHARED BOOL CXX_Explicit_Set;
extern DLL_SHARED BOOL CXX_Typename_On;
extern DLL_SHARED BOOL CXX_Typename_Set;
extern DLL_SHARED BOOL CXX_Mutable_On;
extern DLL_SHARED BOOL CXX_Mutable_Set;
extern DLL_SHARED BOOL CXX_Packed_On;
extern DLL_SHARED BOOL CXX_Packed_Set;

extern DLL_SHARED BOOL CXX_Standard_C_Plus_Plus_On;
extern DLL_SHARED BOOL CXX_Standard_C_Plus_Plus_Set;

extern DLL_SHARED BOOL LANG_Pch;
extern DLL_SHARED BOOL LANG_Pch_Set;
extern DLL_SHARED char *LANG_Create_Pch;
extern DLL_SHARED BOOL LANG_Create_Pch_Set;
extern DLL_SHARED char *LANG_Use_Pch;
extern DLL_SHARED BOOL LANG_Use_Pch_Set;
extern DLL_SHARED char *LANG_Pchdir;
extern DLL_SHARED BOOL LANG_Pchdir_Set;

extern DLL_SHARED char *LANG_cxx_dialect;
extern DLL_SHARED BOOL LANG_cxx_dialect_Set;

extern DLL_SHARED BOOL LANG_Microsoft_Mode;
extern DLL_SHARED BOOL LANG_Microsoft_Mode_Set;

extern DLL_SHARED BOOL LANG_Ansi_Setjmp_On;
extern DLL_SHARED BOOL LANG_Ansi_Setjmp_Set;

extern DLL_SHARED BOOL LANG_Ignore_Carriage_Return_On;
extern DLL_SHARED BOOL LANG_Ignore_Carriage_Return_Set;

extern DLL_SHARED BOOL C_Restrict_On;
extern DLL_SHARED BOOL C_Restrict_Set;

extern DLL_SHARED char *C_Auto_Restrict;
extern DLL_SHARED BOOL C_Auto_Restrict_Set;

extern DLL_SHARED BOOL FTN_Short_Circuit_On;
extern DLL_SHARED BOOL FTN_Short_Circuit_Set;

extern DLL_SHARED BOOL WHIRL_Comma_Rcomma_On;
extern DLL_SHARED BOOL WHIRL_Comma_Rcomma_Set;

extern DLL_SHARED BOOL Macro_Expand_Pragmas_On;
extern DLL_SHARED BOOL Macro_Expand_Pragmas_Set;

extern DLL_SHARED BOOL C_VLA_On;
extern DLL_SHARED BOOL C_VLA_Set;

extern DLL_SHARED BOOL WHIRL_Merge_Types_On;
extern DLL_SHARED BOOL WHIRL_Merge_Types_Set;

extern DLL_SHARED BOOL LANG_Symtab_Verify_On;
extern DLL_SHARED BOOL LANG_Symtab_Verify_Set;

extern DLL_SHARED BOOL WHIRL_Mtype_A_On;
extern DLL_SHARED BOOL WHIRL_Mtype_B_On;
extern DLL_SHARED BOOL WHIRL_Mtype_BS_On;
extern DLL_SHARED BOOL WHIRL_Return_Val_On;
extern DLL_SHARED BOOL WHIRL_Flatten_Field_On;
extern DLL_SHARED BOOL WHIRL_Mldid_Mstid_On;
extern DLL_SHARED BOOL WHIRL_Vfcall_On;
extern DLL_SHARED BOOL WHIRL_Addr_Passed_On;
extern DLL_SHARED BOOL WHIRL_Addr_Saved_For_Passed_On;
extern DLL_SHARED BOOL WHIRL_Addr_Saved_On;
extern DLL_SHARED BOOL WHIRL_Keep_Cvt_On;
extern DLL_SHARED BOOL WHIRL_Return_Info_On;

extern DLL_SHARED BOOL Global_Pragmas_In_Dummy_PU_On;
extern DLL_SHARED BOOL Malloc_Free_On;
extern DLL_SHARED BOOL Alloca_Dealloca_On;
extern DLL_SHARED BOOL Barrier_Lvalues_On;

/***** The following is TRUE for C++  unless -no_exceptions is specified *****/
extern DLL_SHARED BOOL Allow_Exceptions;

/***** Compiler debug/trace options *****/
extern DLL_SHARED BOOL Tracing_Enabled;	/* Any trace options set? */
extern DLL_SHARED INT16 Hard_Failure_Info;	/* Print trace info on failure? */
extern DLL_SHARED INT16 Soft_Failure_Info;	/* Print trace info on failure? */

/* Control usage of the .I and .J files: */
extern DLL_SHARED BOOL Open_IJ_Files;

/* For communication between driver and config routines ONLY: */
extern DLL_SHARED INT8 Debug_Level;
# define MAX_DEBUG_LEVEL	3

typedef enum {
  EAGER_NONE,
  EAGER_SAFE,
  EAGER_ARITH,
  EAGER_DIVIDE,
  EAGER_MEMORY,
  EAGER_OTHER,
  EAGER_EXCESS
} EAGER_LEVEL;
extern DLL_SHARED EAGER_LEVEL Eager_Level;

/***** Miscellaneous optimization options *****/
/* Should idict commute operands in seeking match? */
extern DLL_SHARED BOOL Idict_Commutable_Match;
extern DLL_SHARED BOOL Enable_FE_Optimization;	/* Enable FE (KAP) scalar opt? */
extern DLL_SHARED BOOL Alias_Pointer_Parms;	/* Reference parms indep? */
extern DLL_SHARED BOOL Alias_Pointer_Types;	/* Ptrs to distinct basic types indep? */
extern DLL_SHARED BOOL Alias_Not_In_Union;          /* C++ ONLY rule: assume ptrs to objects with user-constructors are NOT in unions */
extern DLL_SHARED BOOL Alias_Pointer_Strongly_Typed; /* Ptrs to distinct types indep? */
extern DLL_SHARED BOOL Alias_Pointer_Named_Data;	/* No pointers to named data? */
extern DLL_SHARED BOOL Alias_Pointer_Restricted;	/* *p and *q not aliased */
extern DLL_SHARED BOOL Alias_Pointer_Disjoint;     /* **p and **q not aliased */
extern DLL_SHARED BOOL Alias_Pointer_Cray;         /* Cray pointer semantics? */
extern DLL_SHARED BOOL Alias_Common_Scalar;        /* Distinguish scalar from other array
                                           in a common block */

extern DLL_SHARED BOOL CSE_Elim_Enabled;		/* Is CSE-elim on? -- this does
					 * not control it, it just
					 * shadows the opt. level
					 */
extern DLL_SHARED BOOL Enable_GOT_Call_Conversion; /* %call16 -> %got_disp? */
extern DLL_SHARED BOOL OPT_Unroll_Analysis;	/* Enable unroll limitations? */
extern DLL_SHARED BOOL OPT_Unroll_Analysis_Set;	/* ... option seen? */

/***** Various Scalar Optimizer options *****/
extern DLL_SHARED BOOL Enable_Copy_Propagate;

/***** Put all-zero initialized file-level data in the BSS section? *****/
extern DLL_SHARED BOOL Zeroinit_in_bss;

/***** IEEE 754 options *****/
typedef enum {
  IEEE_STRICT = 0,	/* Conform strictly */
	/* IEEE_STRICT is not supported.  It might be useful to
	 * avoid madds, do gradual underflow, etc...
	 */
  IEEE_ACCURATE = 1,	/* Do not degrade IEEE accuracy */
  IEEE_INEXACT = 2,	/* Inexact results may not be IEEE */
  IEEE_ANY = 3		/* Anything goes */
} IEEE_LEVEL;
extern DLL_SHARED IEEE_LEVEL IEEE_Arithmetic;  /* IEEE arithmetic? */
extern DLL_SHARED  BOOL IEEE_Arith_Set;	/* ... option seen? */

/***** Constant folding options *****/
typedef enum {
  ROUNDOFF_NONE,	/* No roundoff-inducing transformations */
  ROUNDOFF_SIMPLE,	/* Simple roundoff transformations */
  ROUNDOFF_ASSOC,	/* Reassociation transformations */
  ROUNDOFF_ANY		/* Anything goes */
} ROUNDOFF;
extern DLL_SHARED ROUNDOFF Roundoff_Level;		/* -OPT_roundoff=n value */
extern DLL_SHARED BOOL Roundoff_Set;		/* ... option seen? */
extern DLL_SHARED BOOL Enable_WN_Simp;             /* Use the WHIRL simplifier? */
extern DLL_SHARED BOOL Regions_Around_Inner_Loops; /* Put REGIONs around inner loops */
extern DLL_SHARED BOOL Region_Boundary_Info;	/* calc boundary info for regions */
extern DLL_SHARED BOOL Cray_Ivdep;   		/* Use Cray meaning for ivdep */
extern DLL_SHARED BOOL Liberal_Ivdep;   		/* Use liberal meaning for ivdep */

extern DLL_SHARED BOOL Sqrt_Allowed;		/* Generate SQRT instruction? */
extern DLL_SHARED BOOL Sqrt_Set;		/* ... option seen */
extern DLL_SHARED BOOL Rsqrt_Allowed;		/* Generate RSQRT instruction? */
extern DLL_SHARED BOOL Rsqrt_Set;		/* ... option seen */
extern DLL_SHARED BOOL Recip_Allowed;		/* Generate RECIP instruction? */
extern DLL_SHARED BOOL Recip_Set;		/* ... option seen */

#ifdef TARG_XTENSA
#define MTYPE_SQRT_ALLOWED(mtype) (Sqrt_Allowed && (mtype == MTYPE_F4))
#define MTYPE_RECIP_ALLOWED(mtype) (Recip_Allowed && (mtype == MTYPE_F4))
#define MTYPE_RSQRT_ALLOWED(mtype) (Rsqrt_Allowed && (mtype == MTYPE_F4))
#else
#define MTYPE_SQRT_ALLOWED(mtype) (Sqrt_Allowed)
#define MTYPE_RECIP_ALLOWED(mtype) (Recip_Allowed)
#define MTYPE_RSQRT_ALLOWED(mtype) (Rsqrt_Allowed)
#endif

extern DLL_SHARED BOOL Enable_Cfold_Aggressive;	/* Complex constant folding? */
extern DLL_SHARED BOOL Ptr_Opt_Allowed;	        /* Treat pointers as arrays */
extern DLL_SHARED BOOL Fast_Complex_Allowed;	/* Enable fast c_div and c_abs? */
extern DLL_SHARED BOOL Fast_Complex_Set;		/* ... option seen? */
extern DLL_SHARED BOOL Fast_Bit_Allowed;		/* Fast inlined bit intrinsics? */
extern DLL_SHARED BOOL Fast_Bit_Set;		/* ... option seen? */
extern DLL_SHARED BOOL Fast_NINT_Allowed;		/* Fast NINT and ANINT? */
extern DLL_SHARED BOOL Fast_NINT_Set;		/* ... option seen? */
extern DLL_SHARED BOOL Fast_trunc_Allowed;		/* Fast trunc of NINT/ANINT/AINT/AMOD */
extern DLL_SHARED BOOL Fast_trunc_Set;		/* ... option seen? */
extern DLL_SHARED BOOL Fast_IO_Allowed;		/* Fast printf/scanf/printw */
extern DLL_SHARED BOOL Inline_Intrinsics_Allowed;	/* Inline intrinsics? Or lib calls? */
extern DLL_SHARED BOOL Inline_Intrinsics_Set;	/* ... option seen? */
extern DLL_SHARED BOOL Simp_Multiply_To_Shift;	/* Change multiplies to shifts? */
extern DLL_SHARED BOOL Simp_Canonicalize;          /* Simple canon/reassoc */
extern DLL_SHARED BOOL Simp_Fold_Unsigned_Relops;  /* Simplify unsigned relops */
extern DLL_SHARED BOOL Simp_Unsafe_Relops;         /* Allow foldings which might cause error if overflow occurs */
extern DLL_SHARED BOOL Enable_NaryExpr;		/* Allow nary expr in the lowerer */
extern DLL_SHARED BOOL Enable_NaryExpr_Set;	/* ... option seen? */

/***** Global Code Motion (GCM) options *****/
extern DLL_SHARED BOOL GCM_Eager_Null_Ptr_Deref;   /* allow speculation past the NULL
					   ptr test. assumes page zero as
					   readable */
extern DLL_SHARED BOOL GCM_Eager_Null_Ptr_Deref_Set; /* ... option seen? */

/***** Miscellaneous GOPT options *****/
#define MAX_OPT_LEVEL	3
#define DEF_O_LEVEL	2	/* Level implied by -O */
#define DEF_OPT_LEVEL	1
extern DLL_SHARED INT32 Opt_Level;		/* -On level */
extern DLL_SHARED INT32 OPT_unroll_times;
extern DLL_SHARED BOOL OPT_unroll_times_overridden;
extern DLL_SHARED INT32 OPT_unroll_size;
extern DLL_SHARED BOOL OPT_unroll_size_overridden;
extern DLL_SHARED BOOL OPT_Lower_Speculate;
extern DLL_SHARED BOOL OPT_Lower_Treeheight;
extern DLL_SHARED BOOL OPT_Inline_Divide;
extern DLL_SHARED BOOL OPT_Space;
extern DLL_SHARED BOOL OPT_Flix_Space;

/* Functions that account for less than OPT_Space_Threshold / 1000 
 * of the total cycle count will be optimized for space.  */
extern DLL_SHARED UINT32 OPT_Space_Threshold;

/* Estimate PU cycle counts instead of using rsr.ccount.  */
extern DLL_SHARED BOOL OPT_Estimate_Ccount;

extern DLL_SHARED INT32 Olimit;	/* stop optimization or use regions at this limit */
extern DLL_SHARED BOOL Olimit_opt;	/* FALSE => stop optimization if Olimit reached;
			 * TRUE  => use regions to optimize if Olimit reached */
extern DLL_SHARED BOOL CG_mem_intrinsics;
extern DLL_SHARED INT32 CG_memmove_inst_count;
extern DLL_SHARED BOOL CG_memmove_inst_count_overridden;
extern DLL_SHARED BOOL CG_bcopy_cannot_overlap;
extern DLL_SHARED BOOL CG_memcpy_cannot_overlap;
extern DLL_SHARED BOOL CG_memmove_cannot_overlap;
extern DLL_SHARED BOOL CG_memmove_nonconst;
extern DLL_SHARED BOOL Allow_wrap_around_opt;
extern DLL_SHARED BOOL OPT_IV_Short_Wrap_Around_Safe;
#define DEF_FOLD_ARITH_MAX_INS_CNT 1000
extern DLL_SHARED INT32 Fold_Arith_Max_INS_CNT;
#define DEF_CONST_COPY_TN_CNT 10000
extern DLL_SHARED INT32 Const_Copy_TN_CNT;
#define DEF_GOPT_TN_CNT 15000
extern DLL_SHARED INT32 Gopt_TN_CNT;
extern DLL_SHARED BOOL Enable_BB_Splitting; /* Split long basic blocks? */
extern DLL_SHARED INT32 Split_BB_Length;	/* split BBs that are > than this */
#define DEF_BBLENGTH	1000	/* default value for Split_BB_Length */
#define MIN_BBLENGTH	 100	/* don't let the value get too small */
#define MAX_BBLENGTH	5000	/* don't let the value get too big */

/***** Automatic TIE generation options *****/
extern DLL_SHARED BOOL Run_Autotie;
extern DLL_SHARED BOOL Run_Autotie2;
extern DLL_SHARED BOOL CG_fusion;
extern DLL_SHARED BOOL CG_find_updating;
extern DLL_SHARED INT32 CG_fusion_before;
extern DLL_SHARED INT32 CG_fusion_after;
  
/***** What is the byte sex of the host and target? *****/
extern DLL_SHARED UINT8 Host_Byte_Sex;	/* Set in config_host.c */
extern DLL_SHARED UINT8 Target_Byte_Sex;	/* Set in config_targ.c */
extern DLL_SHARED BOOL  Same_Byte_Sex;	/* Set in config_targ.c */

extern DLL_SHARED INT32 iolist_reuse_limit;

/***** Misaligned memory reference control *****/
extern DLL_SHARED INT32 Aggregate_Alignment; /* This alignment for aggregate layout */

extern DLL_SHARED BOOL Align_Object;	/* Try to improve the alignment of objects */
extern DLL_SHARED BOOL Align_Padding;	/* Pad objects to their natural alignment */
extern DLL_SHARED BOOL UseAlignedCopyForStructs;	/* always use aligned copy */

/***** Mem copy/set inlining control *****/
extern DLL_SHARED INT32 MinStructCopyMemIntrSize; /* Use call for copy/set requiring more than this
					  many loads and stores. */
extern DLL_SHARED INT32 MinStructCopyLoopSize;    /* Use inlined loop for copy/set requiring more than
					  this many loads and stores. */


/***** Miscellaneous code generation options *****/
extern DLL_SHARED BOOL Gen_PIC_Call_Shared; /* CPIC */
extern DLL_SHARED BOOL Gen_PIC_Shared;	/* PIC */
extern DLL_SHARED BOOL Gen_PIC_Calls;	/* do calls as PIC code */
extern DLL_SHARED BOOL Guaranteed_Small_GOT; /* GOT < 64kB? */
extern DLL_SHARED BOOL Non_Volatile_GOT;	/* GOT entries volatile? */
extern DLL_SHARED BOOL PIC_Local_Names;	/* Names local by default? */
extern DLL_SHARED BOOL PIC_Protected_Names; /* Names protected by default? */
extern DLL_SHARED BOOL PIC_Fixed_Addresses; /* Names fixed by default? */
extern DLL_SHARED BOOL PIC_No_Page_Offset;	/* Don't use page/offset addressing? */
extern DLL_SHARED BOOL Force_Mem_Formals;	/* Always force formals to memory? */
extern DLL_SHARED BOOL Kernel_Code;	/* Compiling OS kernel? */
extern DLL_SHARED INT32 Short_Data;	/* Objects of this size in .sdata */
extern DLL_SHARED INT32 Short_Lits;	/* Literals of this size in .litX */
extern DLL_SHARED INT32 Max_Sdata_Elt_Size;/* -Gn: sdata size */
extern DLL_SHARED INT32 Gspace_Available;	/* -Gspace: available space for gprel objects */
extern DLL_SHARED BOOL Force_GP_Prolog;	/* force usage of gp prolog */
extern DLL_SHARED BOOL Force_Indirect_Call;/* force usage of indirect calls */
extern DLL_SHARED BOOL Gen_Indirect_Call; /* force usage of indirect calls */
extern DLL_SHARED INT32 Heap_Allocation_Threshold; /* Allocate objects > this on the heap 
					 * (-1 means always use stack), 
					 * 0 always use heap
					 * default is 0
					 */
extern DLL_SHARED BOOL Strings_Not_Gprelative;	/* don't make strings gp-relative */
#define MAX_SDATA_ELT_SIZE	32760
#define DEF_SDATA_ELT_SIZE	8
extern DLL_SHARED BOOL Varargs_Prototypes;	/* Varargs have prototypes for FP? */
extern DLL_SHARED BOOL Gen_Profile;
extern DLL_SHARED char *Gen_Profile_Name;
extern DLL_SHARED BOOL Call_Mcount;	/* generate a call to mcount in pu entry */
extern DLL_SHARED BOOL GP_Is_Preserved;	/* GP is neither caller or callee-save */
extern DLL_SHARED BOOL Constant_GP;	/* GP never changes */

extern DLL_SHARED char *Emit_Global_Data;	/* only emit global data */
extern DLL_SHARED char *Read_Global_Data;	/* only read global data */

extern DLL_SHARED char *Library_Name;              /* -TENV:io_library=xxx */
extern DLL_SHARED INT  target_io_library;

extern DLL_SHARED BOOL Meld_Schedule;	/* Attempt to meld basic blocks	*/
extern DLL_SHARED BOOL Gap_Schedule;	/* Attempt to perform gap scheduling */
extern DLL_SHARED BOOL Attempt_Bypass;	/* Attempt to use bypass registers */
extern DLL_SHARED BOOL Enable_SWP;		/* Do software pipelining */
extern DLL_SHARED BOOL Enable_SWP_overridden; /* override on command line */
extern DLL_SHARED BOOL Enable_ZCL;		/* Use zero-overhead loops. */
extern DLL_SHARED BOOL Enable_ZCL_overridden; /* override on command line */
extern DLL_SHARED BOOL Enable_LOH;		/* Do loop overhead */
extern DLL_SHARED BOOL Enable_LOH_overridden; /* Enable_LOH overridden on cmd line */
extern DLL_SHARED BOOL Enable_Spec_Loads;	/* Allow speculation of loads */
extern DLL_SHARED BOOL Isolate_Lines;	/* Don't overlap source	lines */
extern DLL_SHARED BOOL Fill_Delay_Slots;	/* Attempt to fill branch delay slots */
extern DLL_SHARED BOOL Enable_GDSE;	/* Do global dead store elimination */
extern DLL_SHARED BOOL Enable_CG_Peephole;	/* Enable peephole optimization in cgprep */
extern DLL_SHARED BOOL Optimize_CVTL_Exp;	/* Optimize expansion of CVTL operators */
extern DLL_SHARED BOOL Enable_CVT_Opt;	/* Optimize expansion of CVT operators */
extern DLL_SHARED BOOL Indexed_Loads_Allowed; /* enable generation of indexed loads/stores */
extern DLL_SHARED BOOL Early_MP_Processing; /* Do MP lowering before LNO/PREOPT */
extern DLL_SHARED BOOL Implied_Do_Io_Opt;   /* Do implied-do loop optimization for I/O */

/* back end phases options */
#ifdef BACK_END
extern DLL_SHARED BOOL Run_lno;		    /* run loop-nest optimizer */
extern DLL_SHARED BOOL Run_lego;               /* run lego-lowering */
extern DLL_SHARED BOOL Run_lego_given;         /* was run lego-lowering given/not */
extern DLL_SHARED BOOL Run_wopt;		    /* run WHIRL global optimizer */
extern DLL_SHARED BOOL Run_preopt;		    /* run WHIRL preopt optimizer */
extern DLL_SHARED BOOL Run_cg;		    /* run code generator */
extern DLL_SHARED BOOL Run_w2c;		    /* run whirl2c */
extern DLL_SHARED BOOL Run_w2f;		    /* run whirl2f */
extern DLL_SHARED BOOL Run_w2fc_early;	    /* run whirl2f after LNO parallelization */
extern DLL_SHARED BOOL Run_prompf;		    /* create prompf analysis file */
extern DLL_SHARED BOOL Run_purple;		    /* run purple instrumenter */
extern DLL_SHARED BOOL Run_ipl;		    /* run summary phase of IPA */
extern DLL_SHARED char *LNO_Path;		    /* path to lno.so */
extern DLL_SHARED char *WOPT_Path;		    /* path to wopt.so */
extern DLL_SHARED char *CG_Path;		    /* path to cg.so */
extern DLL_SHARED char *W2C_Path;		    /* path to whirl2c.so */
extern DLL_SHARED char *W2F_Path;		    /* path to whirl2f.so */
extern DLL_SHARED char *Prompf_Anl_Path;	    /* path to prompf_anl.so */
extern DLL_SHARED char *Purple_Path;	    /* path to purple.so */
extern DLL_SHARED char *Ipl_Path;		    /* path to ipl.so */
#endif /* BACK_END */
extern DLL_SHARED char *Inline_Path;           /* path to inline.so */
#if defined(BACK_END) || defined(QIKKI_BE)
extern DLL_SHARED char *Targ_Path;		    /* path to targinfo .so */
#endif /* defined(BACK_END) || defined(QIKKI_BE) */

extern DLL_SHARED char *Schedlist_Option;

/* Force EH Region offsets to be long */
extern DLL_SHARED BOOL Force_Long_EH_Range_Offsets;
/* Force stack frame to use large model */
extern DLL_SHARED BOOL Force_Large_Stack_Model;
/* put each function in its own text section */
extern DLL_SHARED BOOL Section_For_Each_Function;

/* list of registers that are not allocatable */
extern DLL_SHARED OPTION_LIST *Registers_Not_Allocatable;

/* Unique ident from IPA */
extern DLL_SHARED INT32 Ipa_Ident_Number;

extern DLL_SHARED BOOL Scalar_Formal_Ref;		/* for fortran formal scalar refs */
extern DLL_SHARED BOOL Non_Scalar_Formal_Ref;	/* for fortran formal non_scalar refs */

/***** Maximum sizes of recursive structures we will handle *****/
#define MAXDONEST	300
#define MAXIFNEST	300

/* The following define specifies the maximum evaluation depth from
 * leaf to root in an expression containing boolean operators (AND,
 * OR, NOT).  The amount of space specified will be allocated to hold
 * the inherited attributes of boolean expressions and flow of control
 * statements.
 */
#define MAXBOOLDEPTH    (100 + MAXIFNEST)

/* ====================================================================
 *
 * Skip option interface
 *
 * For debugging purposes, we support the suppression (skipping) of
 * optimization for specific PUs in a compilation (e.g. in WOPT and in
 * IPA).  This is controlled by specifying a range (before/after) or
 * item (ident) of numbers to skip, which produces a group option list
 * from the group processing in flags.c.  The support here provides for
 * (1) converting the group option list to a form easier to query, and
 * (2) querying the resulting skip list.
 *
 * ====================================================================
 */

typedef struct skiplist SKIPLIST;
struct option_list;

/* Convert an option list to a SKIPLIST: */
SKIPLIST *Build_Skiplist ( struct option_list *olist );

/* Query a SKIPLIST -- result TRUE means element is in list: */
BOOL Query_Skiplist ( SKIPLIST *slist, INT32 elmt );

/* SKIPLIST for All Optimizations */
extern DLL_SHARED SKIPLIST *Optimization_Skip_List;     /* Processed list */
extern DLL_SHARED SKIPLIST *Region_Skip_List;	     /* regions to skip, processed */

/* ====================================================================
 *
 * Initialization interface
 *
 * ====================================================================
 */

/***** Perform configuration functions prior to flag processing *****/
extern DLL_SHARED void Preconfigure (void);

/***** Perform configuration functions after flag processing *****/
extern DLL_SHARED void Configure (void);

/***** Perform configuration functions for each source file *****/
extern DLL_SHARED void Configure_Source ( char *filename );

/***** Perform configuration functions for the alias analysis options *****/
extern DLL_SHARED void Configure_Alias_Options (struct option_list *);

extern DLL_SHARED void Configure_Feedback_Options (struct option_list *);


/***** Process a trace option string *****/
extern DLL_SHARED BOOL Process_Trace_Option ( char *option );

/***** List options to the given file *****/
extern DLL_SHARED void List_Compile_Options (
  FILE *file,		/* File to which to write */
  char *pfx,		/* Prefix output with this string */
  BOOL internal,	/* Internal or user listing? */
  BOOL full_list,	/* Groups (user)?  All options (internal)? */
  BOOL update );	/* Reset option changed/modified flags? */


#ifndef Is_Target_R4K
#define Is_Target_R4K()		(0)
#endif
#ifndef Is_Target_R5K
#define Is_Target_R5K()		(0)
#endif
#ifndef Is_Target_R8K
#define Is_Target_R8K()		(0)
#endif
#ifndef Is_Target_R10K
#define Is_Target_R10K()	(0)
#endif
#ifndef Is_Target_TP
#define Is_Target_TP()		Is_Target_R8K()
#endif
#ifndef Is_Target_T5
#define Is_Target_T5()		Is_Target_R10K()
#endif
#ifndef Is_Target_Pentium
#define Is_Target_Pentium()	(0)
#endif
#ifndef Is_Target_Itanium
#define Is_Target_Itanium()	(0)
#endif

#ifndef Is_Target_ISA_M1
#define Is_Target_ISA_M1()	(0)
#endif
#ifndef Is_Target_ISA_M2
#define Is_Target_ISA_M2()	(0)
#endif
#ifndef Is_Target_ISA_M3
#define Is_Target_ISA_M3()	(0)
#endif
#ifndef Is_Target_ISA_M4
#define Is_Target_ISA_M4()	(0)
#endif
#ifndef Is_Target_ISA_M2Plus
#define Is_Target_ISA_M2Plus()	(0)
#endif
#ifndef Is_Target_ISA_M3Plus
#define Is_Target_ISA_M3Plus()	(0)
#endif
#ifndef Is_Target_ISA_M4Plus
#define Is_Target_ISA_M4Plus()	(0)
#endif
#ifndef Is_Target_ISA_I1
#define Is_Target_ISA_I1()	(0)
#endif
#ifndef Is_Target_ISA_I1Plus
#define Is_Target_ISA_I1Plus()	(0)
#endif

#ifdef __cplusplus
}
#endif
#endif /* config_INCLUDED */
