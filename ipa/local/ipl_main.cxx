/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
/* ====================================================================
 * ====================================================================
 *
 * Module: ipl_main.c
 *
 * Revision history:
 *  20-Sep-94 - Original Version 
 *
 * Description:
 *
 * The local phase driver
 *
 * ====================================================================
 * ====================================================================
 */

/*----------------------------------------------------------
 *          Summary Phase
 * This phase reads in the IR files and collects summary 
 * information
 *----------------------------------------------------------*/
#include <elf.h>
#include <sys/elf_whirl.h>	    // for WHIRL revision number
#include <sys/types.h>		    // for ir_bwrite.h
#include "defs.h"

#define BACK_END		    // needed by config.h
#include "glob.h"		    // for Show_Progress
#include "flags.h"		    // for Process_Command_Line_Group ()
#include "wn.h"			    // for WHIRL related operations
#include "tracing.h"		    // for Get_Trace()
#include "ir_reader.h"		    // for IR_reader_init()
#include "const.h"		    // for CONST_TAB_SIZE
#include "pu_info.h"		    // for ir_bwrite.h
#include "ir_bwrite.h"		    // for IPA_write_summary ()
#include "cxx_memory.h"		    // for CXX_NEW

#include "ipl_summary.h"	    // for summary info data structures
#include "ipl_summarize.h"	    // for summarization info
#include "ipl_bread_write.h"	    // for IPA_irb_write_summary()
#ifdef __AUTOPAR__
#include "ipl_array_bread_write.h"  // for Init_write_asections
#endif
#include "optimizer.h"		    // for struct DU_MANAGER ...
#include "ipl_driver.h"		    // for extern "C" declaration
#include "config.h"		    // for Run_preopt
#include "config_debug.h"
#include "config_opt.h"
#include "config_ipa.h"
#include "ipl_summarize_template.h" // put these two template files
#include "ipl_analyze_template.h"   // last in the include list
#ifdef __AUTOPAR__
#include "ipl_cost_template.h" 	    // execution cost analysis 
#include "ipa_section_main.h" 	    // utilities
#include "wb_ipl.h" 
#endif
#ifndef TARG_XTENSA
#include "ipl_outline.h"	    // outline analysis
#endif
#include "ipl_elfsym.h"		    // for IPL_Write_Elf_Symtab

/* General progress trace: */
BOOL Trace_IPA = FALSE;
BOOL Trace_Perf = FALSE;

BOOL Debug_On = FALSE;
BOOL DoPreopt = FALSE;
BOOL Do_Const = FALSE;
BOOL Do_Par   = FALSE;
BOOL Do_Split_Commons = TRUE;
BOOL Do_Split_Commons_Set = FALSE;
BOOL Do_Common_Const = FALSE;
BOOL IPL_Enable_Outline = FALSE;
BOOL IPL_Enable_Unknown_Frequency = FALSE;
BOOL IPL_Generate_Elf_Symtab = TRUE; // when using GNU linker 

mUINT8 Optlevel = 0;

static INT driver_argc = 0;
static char **driver_argv;

static OPTION_DESC Options_IPL[] = {
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "debug",	"",
      0, 0, 0,	&Debug_On,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "const",	"",
      0, 0, 0,	&Do_Const,	NULL},
    { OVK_BOOL,	OV_INTERNAL,	FALSE, "par",	"",
      0, 0, 0,	&Do_Par,	NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "outline", "",
      0, 0, 0,  &IPL_Enable_Outline, NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "unknown", "",
      0, 0, 0,  &IPL_Enable_Unknown_Frequency, NULL},
    { OVK_BOOL, OV_INTERNAL,	FALSE, "elf_symtab", "",
      0, 0, 0,	&IPL_Generate_Elf_Symtab, NULL},
    { OVK_COUNT }		    // List terminator -- must be last
};


OPTION_GROUP IPL_Option_Groups[] = {
    { "IPL", ':', '=', Options_IPL },
    { NULL }			    // List terminator -- must be last
};


SUMMARY *Summary;			// class for all the summary work
WN_MAP Parent_Map;
WN_MAP Summary_Map;
WN_MAP Stmt_Map;

FILE* STDOUT = stdout;
DYN_ARRAY<char*>* Ipl_Symbol_Names = NULL; 
DYN_ARRAY<char*>* Ipl_Function_Names = NULL; 

/* ====================================================================
 *
 * Process_Command_Line
 *
 * Process the command line arguments.  Evaluate all flags except per-
 * source file control flags and set up global options.
 *
 * ====================================================================
 */
static void
Process_Command_Line (INT argc, char **argv)
{
    INT i;

    for (i = 0; i < argc; i++) {
	if (argv[i] != NULL && *(argv[i]) == '-') {

	    char *arg_str = argv[i];
	    if (Process_Command_Line_Group (arg_str+1,
					    IPL_Option_Groups))
	      {
		continue;
	      }
	    if (strcmp (arg_str, "-cmds") == 0) {
		driver_argc = argc - i - 1;
		if (driver_argc > 0)
		    driver_argv = argv + i + 1;
		i = argc;	    /* stop processing */
	    }
	}		   
    }

    if (OPT_Reorg_Common_Set)
      Do_Split_Commons = OPT_Reorg_Common;

    Do_Split_Commons_Set = OPT_Reorg_Common_Set && OPT_Reorg_Common;

    Do_Par = IPA_Enable_Array_Summary && Run_preopt;

    if (Do_Par)
      WOPT_Enable_Generate_DU = TRUE;

    Do_Common_Const = IPA_Enable_Common_Const && Run_preopt;

} // Process_Command_Line

void
ipl_main (INT ipl_argc, char **ipl_argv)
{
    extern DLL_SHARED char *Whirl_Revision;

    if (strcmp (Whirl_Revision, WHIRL_REVISION) != 0)
	FmtAssert (!DEBUG_Ir_Version_Check,
		   ("WHIRL revision mismatch between be.so (%s) and ipl.so (%s)", 
		    Whirl_Revision, WHIRL_REVISION));
    
    /* Perform preliminary command line processing: */
    Process_Command_Line (ipl_argc, ipl_argv);
    Optlevel = Opt_Level;
} // ipl_main


/* Initialization that needs to be done after the global symtab is read */
void
Ipl_Init (void)
{
    Set_Error_Phase ( "Ipl Initialization" );

    Summary = CXX_NEW (SUMMARY(Malloc_Mem_Pool), Malloc_Mem_Pool);
#ifdef __AUTOPAR__
    // information for writing out array sections
    Init_write_asections (Malloc_Mem_Pool);
#endif
} /* Ipl_Init */


/* Initialization of IPL when it's called from IPA */
void
Ipl_Init_From_Ipa (MEM_POOL* pool)
{
  Summary = CXX_NEW (SUMMARY(pool), pool);
#ifdef __AUTOPAR__
  Init_write_asections (pool);
#endif
} /* Ipl_Init_From_Ipa */


/*-----------------------------------------------------------------*/
/* entry point into the local phase                                */
/*-----------------------------------------------------------------*/
void
Perform_Procedure_Summary_Phase (WN* w, struct DU_MANAGER *du_mgr,
				 struct ALIAS_MANAGER *alias_mgr,
				 void *emitter)
{
    Trace_IPA = Get_Trace (TP_IPL, TT_IPL_IPA);

    if ( Debug_On )
	IR_reader_init();
#ifndef TARG_XTENSA
    if (IPL_Enable_Outline) {
	const WN* wn = Outline_Split_Point (w, IPA_PU_Minimum_Size,
					    IPA_Small_Callee_Limit / 2);
	if (wn) {
	    fprintf (TFile, "Splitting %s:\n", ST_name (WN_st (w)));
	    fdump_tree (TFile, const_cast<WN*> (wn));
	}
    }
#endif
    if (Trace_IPA) {
	fprintf ( TFile, "Summarizing procedure %s \n", ST_name(WN_st(w)) );
    }

    DoPreopt = Run_preopt;
    if (Run_preopt && Cur_PU_Feedback) {
	BOOL pass = Cur_PU_Feedback->Verify ("IPL");
	if (! pass)
	    DevWarn ("Feedback verify fails after preopt");
    }
    
#ifdef __AUTOPAR__
    WB_IPL_Set_Scalar_Summary(Summary);
    WB_IPL_Set_Array_Summary(NULL);
#endif
    Summary->Set_du_mgr (du_mgr);
    Summary->Set_alias_mgr (alias_mgr);
    Summary->Set_emitter ((EMITTER *) emitter);
    Summary->Summarize (w);
#ifdef __AUTOPAR__
    WB_IPL_Set_Array_Summary(NULL);
    WB_IPL_Set_Scalar_Summary(NULL);
#endif
 
} // Perform_Procedure_Summary_Phase

void
Ipl_Fini (void)
{
    Summary->Set_global_addr_taken_attrib ();
} // Ipl_Fini


void
Ipl_Extra_Output (Output_File *ir_output)
{
    IPA_write_summary(IPA_irb_write_summary, ir_output);

    if ( Get_Trace ( TKIND_IR, TP_IPL ) )
	IPA_Trace_Summary_File ( TFile, ir_output, TRUE, 
	  Ipl_Symbol_Names, Ipl_Function_Names );	

    if (driver_argc > 0)
	WN_write_flags (driver_argc, driver_argv, ir_output);

#if defined(__linux__) || defined(TARG_XTENSA)
// This really should be: #if __USING_GNU_LINKER__
    // write out the Elf version of global symtab
    IPL_Write_Elf_Symtab (ir_output);
#endif
    
} // Ipl_Extra_Output
