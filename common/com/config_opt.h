
/* 
   Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.
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
 * Module: config_opt.h
 * $Revision: 1.21 $
 * $Date: 2000/08/29 22:39:57 $
 * $Author: dlstephe $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config_opt.h,v $
 *
 * Revision history:
 *  05-May-96 - Extracted from be/opt/opt_config.h.
 *
 * Description:
 *
 * Declare global flag variables for -OPT group options.
 * This file is included in common/com/config.c.
 *
 * Declarations of -OPT flags should be put here, instead of in
 * config.h.  The intent is to allow updates of the -OPT group
 * without forcing recompilation of everything that includes config.h.
 * (However, the transfer of the flags' definitions here from config.h
 * is not yet complete, so most of the old ones still require
 * config.h.)
 *
 * ====================================================================
 * WARNING: WHENEVER A NEW FLAG IS ADDED:
 * ###	- Add the flag variable declaration to config_opt.h (here) .
 * ###	- Add the flag variable definition to config_opt.cxx .
 * ###	- Add the option to the group description in config_opt.cxx .
 * ====================================================================
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_opt_INCLUDED
#define config_opt_INCLUDED

#ifndef flags_INCLUDED
#include "flags.h"
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Incomplete types to prevent unnecessary inclusion: */
struct skiplist;

/*********************************************************************
 ***
 *** Flag variable declarations:
 ***
 *********************************************************************
 */

/***** Optimization Warning Messages *****/
extern DLL_SHARED BOOL Show_OPT_Warnings;		/* Display OPT warning messages */

/***** Aliasing control *****/
extern DLL_SHARED BOOL Alias_Pointer_Parms;	/* Reference parms indep? */
extern DLL_SHARED BOOL Alias_Pointer_Types;	/* Ptrs to distinct basic types indep? */
extern DLL_SHARED BOOL Alias_Not_In_Union;	/* Ptrs point to non-union types */
extern DLL_SHARED BOOL Alias_Pointer_Strongly_Typed; /* Ptrs to distinct types indep? */
extern DLL_SHARED BOOL Alias_Pointer_Named_Data;	/* No pointers to named data? */
extern DLL_SHARED BOOL Alias_Pointer_Restricted;	/* *p and *q not aliased */
extern DLL_SHARED BOOL Alias_Pointer_Disjoint;     /* **p and **q not aliased */
extern DLL_SHARED BOOL Alias_Pointer_Cray;         /* Cray pointer semantics? */
extern DLL_SHARED BOOL Alias_Common_Scalar;        /* Distinguish scalar from other array
                                           in a common block */
extern DLL_SHARED BOOL  Alias_F90_Pointer_Unaliased;  /* Are F90 pointers unaliased? */

/***** Expression folding options *****/
extern DLL_SHARED BOOL Enable_Cfold_Float;		/* FP constant folding? */
extern DLL_SHARED BOOL Enable_Cfold_Reassociate;	/* Re-association allowed? */
extern DLL_SHARED BOOL Enable_Cfold_Intrinsics;	/* Intrinsic constant folding? */
extern DLL_SHARED BOOL Cfold_Intrinsics_Set;	/* ... option seen? */
extern DLL_SHARED BOOL CIS_Allowed;	/* sin(x) and cos(x) => cis(x) ? */
extern DLL_SHARED BOOL Div_Split_Allowed;	/* Change a/b --> a*1/b ? */
extern DLL_SHARED BOOL Sqrt_Split_Allowed;	/* Change sqrt(a) --> recip(rsqrt(b)) ? */
extern DLL_SHARED BOOL Sqrt_Split_Set;	        /* ... option seen? */
extern DLL_SHARED BOOL Fast_Exp_Allowed;	/* Avoid exp() calls? */
extern DLL_SHARED BOOL Fast_IO_Allowed;	/* Fast printf/scanf/printw */
extern DLL_SHARED BOOL Fast_Sqrt_Allowed;	/* Change sqrt(x) --> x * rsqrt(x) ? */
extern DLL_SHARED BOOL Optimize_CVTL_Exp;	/* Optimize expansion of CVTL operators */
extern DLL_SHARED BOOL Enable_CVT_Opt;	/* Optimize expansion of CVT operators */
extern DLL_SHARED BOOL Force_IEEE_Comparisons;	/* IEEE NaN comparisons? */
extern DLL_SHARED BOOL Inline_Intrinsics_Early;    /* Inline intrinsics just after VHO */
extern DLL_SHARED BOOL Enable_extract_compose;     /* Enable use of the extract/compose whirl ops */

/***** Miscellaneous optimization options *****/
extern DLL_SHARED BOOL OPT_Pad_Common;	/* Do internal common block padding? */
extern DLL_SHARED BOOL OPT_Reorg_Common;	/* Do common block reorganization (split)? */
extern DLL_SHARED BOOL OPT_Reorg_Common_Set;	/* ... option seen? */
extern DLL_SHARED BOOL OPT_Unroll_Analysis;	/* Enable unroll limitations? */
extern DLL_SHARED BOOL OPT_Unroll_Analysis_Set;	/* ... option seen? */
extern DLL_SHARED BOOL GCM_Speculative_Ptr_Deref;   /* allow load speculation of a memory
                                          reference that differs by a small
                                          offset from some reference location*/
extern DLL_SHARED BOOL GCM_Speculative_Ptr_Deref_Set;   /* ... option seen? */
extern DLL_SHARED BOOL Early_MP_Processing; /* Do mp lowerering before lno/preopt */
extern DLL_SHARED BOOL Implied_Do_Io_Opt;	/* Do implied-do loop opt for I/O */
extern DLL_SHARED BOOL Cray_Ivdep;		/* Use Cray meaning for Ivdep */
extern DLL_SHARED BOOL Liberal_Ivdep;	/* Use liberal meaning for ivdep */
extern DLL_SHARED BOOL Inhibit_EH_opt;     /* Don't remove calless EH regions */
extern DLL_SHARED BOOL OPT_recompute_addr_flags; /* recompute addr saved */
extern DLL_SHARED BOOL OPT_IPA_addr_analysis; /* enable the use of IPA addr analysis result */ 
extern DLL_SHARED BOOL Delay_U64_Lowering;/* Delay unsigned 64-bit lowering to after wopt*/
extern DLL_SHARED BOOL Enable_HiFi2_Ops; /* Enable use of HiFi2 instructions. */
extern DLL_SHARED BOOL    Keep_Inline_Funcs; /* Don't delete inline functions */
extern DLL_SHARED BOOL    Keep_Static_Funcs; /* Don't delete static functions */

/***** Instrumentation related options *****/

extern DLL_SHARED INT32 Instrumentation_Phase_Num;
extern DLL_SHARED INT32 Instrumentation_Bits;
extern DLL_SHARED BOOL Instrumentation_Enabled;
extern DLL_SHARED UINT32 Instrumentation_Actions;
extern DLL_SHARED BOOL Instrumentation_Unique_Output;
extern DLL_SHARED BOOL Instrumentation_Simcalls;
extern DLL_SHARED BOOL Feedback_Strict_Verify;
extern DLL_SHARED INT32 Feedback_Phase_Num;
extern DLL_SHARED OPTION_LIST* Feedback_Option;


extern DLL_SHARED BOOL Align_Arrays; /* Align arrays to allow vectorization */

#ifdef __cplusplus
}
#endif
#endif /* config_opt_INCLUDED */
