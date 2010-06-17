
/* 
   Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.
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
 * Module: glob.c
 *
 * Revision history:
 *  15-Sep-93 - Original Version
 *
 * Description:
 *
 * This file contains miscellaneous global data and utility functions
 * for the compiler which used to be part of each pass driver.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "errors.h"
#include "glob.h"
#include "tracing.h"

#include "mempool.h"
#if !(defined(SGI_FRONT_END_CPP) && !defined(FFE))
#include "wn.h"
#endif /* if !(defined(SGI_FRONT_END_CPP) && !defined(FFE)) */
#include "stab.h"
#include "const.h"
#include "irbdata.h"
#include "cxx_memory.h"


/* The current program unit name -- */
DLL_SHARED char *Cur_PU_Name = NULL;
DLL_SHARED char *Orig_PU_Name = NULL;

/* Type	of the current program unit -- set similarly to	Cur_PU_Name */
DLL_SHARED PU_KIND	Cur_PU_Kind = PU_UNKNOWN;

DLL_SHARED BOOL Symbol_Table_Out = FALSE;	/* Symbol table output (list or trace) */
DLL_SHARED BOOL Show_Progress = FALSE;     /* Report progress to stdout */

/* ====================================================================
 *
 * Front End process info: for consistency checking 
 *
 * The first two are set in the	front end, and examined	in the back end.
 * They	are also put out to the	ipa file.  The third is	only used in the
 * back	end to store the Version number	of the front end.  (In the front
 * end,	this information is stored in "Version".  In the back end,
 * "Version" contains the version number of the	*back* end.  Those
 * variables are initialized in	<machine>/<process>/version.c
 *
 * ====================================================================
 */

DLL_SHARED INT32 Fe_Process_Id = -1;
DLL_SHARED INT32 Fe_Process_Time =	-1;
DLL_SHARED char *Fe_Version = NULL;

/* ====================================================================
 *
 * File names and handles.
 *
 * ====================================================================
 */

/* Current file	names: */
DLL_SHARED char *Src_File_Name = NULL;	/* Source file */
DLL_SHARED char *Orig_Src_File_Name = NULL; /* Original source file */
DLL_SHARED char *Cpp_File_Name = NULL;	/* cpp-preprocessed file */
DLL_SHARED char *Err_File_Name = NULL;	/* Error file */
DLL_SHARED char *Lst_File_Name = NULL;	/* Listing file	*/
DLL_SHARED char *Trc_File_Name = NULL;	/* Trace file */
DLL_SHARED char *Tlog_File_Name = NULL;	/* Transformation log file */
DLL_SHARED char *IR_File_Name  = NULL;	/* SGIR	file */
DLL_SHARED char *Irb_File_Name = NULL;	/* ACIR	intermediate file */
DLL_SHARED char *Asm_File_Name = NULL;	/* Assembly file */
DLL_SHARED char *Obj_File_Name = NULL;	/* Relocatable object file */
DLL_SHARED char *Feedback_File_Name = NULL; /* Feedback file */
DLL_SHARED char *Profile_File_Name = NULL; /* CG Profile Result file */
#ifndef MONGOOSE_BE
DLL_SHARED char *Lib_File_Name = NULL;	/* Program library file	*/
#endif 
DLL_SHARED char *Lib_Lock_Name = NULL;	/* Program library lock	file */
DLL_SHARED char *DSTdump_File_Name = NULL; /* Dwarf (i.e. DST) dump file */
DLL_SHARED char *Global_File_Name = NULL;	/* Global symbol table file */

/* Current file	handles	if open, NULL otherwise: */
DLL_SHARED FILE *Src_File = NULL;		/* Source file */
DLL_SHARED FILE *Cpp_File = NULL;		/* cpp-preprocessed file */
DLL_SHARED FILE *Err_File = NULL;		/* Error file */
DLL_SHARED FILE *Lst_File = NULL;		/* Listing file	*/
DLL_SHARED FILE *Trc_File = NULL;		/* Trace file */
DLL_SHARED FILE *Tlog_File = NULL;		/* Transformation log file */
DLL_SHARED FILE *IR_File  = NULL;		/* SGIR	file */
DLL_SHARED FILE *Irb_File = NULL;		/* ACIR	intermediate file */
DLL_SHARED FILE *Asm_File = NULL;		/* Assembly file */
DLL_SHARED FILE *Obj_File = NULL;		/* Relocatable object file */
DLL_SHARED FILE *Feedback_File = NULL;	/* Feedback file */
DLL_SHARED FILE *Lib_File = NULL;		/* Program library file	*/
DLL_SHARED FILE *Tim_File = NULL;		/* Timer report	file, usually TFile */

/* Tensilica: Moved from cgdriver.cxx because these are shared across
   DLLs. If they are declared in glob.h (and they were, then they should
   be defined there as well, at least from a stylistic point of view */
/* Output requested: */
DLL_SHARED BOOL Assembly = FALSE;          /* Assembly code */
DLL_SHARED BOOL Object_Code = FALSE;       /* Object code */
DLL_SHARED BOOL Regcopies_Translated = FALSE;
/* moved from IPL */
DLL_SHARED BOOL Trace_Sections = FALSE;
/* moved from cxx memory to work around operator new issues on NT*/
DLL_SHARED MEM_POOL* _dummy_new_mempool = (MEM_POOL *) -1;
DLL_SHARED MEM_POOL* _dummy_delete_mempool = (MEM_POOL *) -1;
DLL_SHARED size_t _dummy_pad = 0;

