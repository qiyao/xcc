
/* 
   Copyright (C) 2004-2005 Tensilica, Inc.  All Rights Reserved.
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

#ifdef _WIN32
#include <windows.h>
#endif

#include "linker.h"			/* linker headers */
#include "ipa_process.h"                /* For create_tmpdir. */

#include "errors.h"			/* for Set_Error_Phase() */
#include "glob.h"			/* for Cleanup_Files() */
#include "config.h"			/* for Preconfigure() */
#include "config_list.h" 
#include "config_targ.h"		/* for Target_ABI */
#include "wn.h"				/* for Dont_Use_WN_Free_List() */

#include "ipc_defs.h"			/* IP_32_bit_ABI */
#include "ipa_option.h"			/* Process_IPA_Options () */
#include "ipo_main.h"		/* Perform_Interprocedural_Optimization */
#include "ipc_symtab_merge.h"		// Initialize_Auxiliary_Tables ()
#include "ipc_type_merge.h"		// for merging types
#include "ipc_main.h"
#include "ipc_pic.h"			// for Global_symbol_optimization ()
#include "ld_ipa_interface.h"		// for ld_for_all_ST ()

#include "ipc_weak.h"


LD_IPA_OPTION *ld_ipa_opt;
char **environ_vars;
char *WB_flags;
char *Y_flags;
char *outfilename;
char *tmpdir;

extern "C"
void ipa_init_env(LD_IPA_OPTION *_ld_ipa_opt,
                  char **_environ_vars,
                  char *_WB_flags,
                  char *_Y_flags,
                  char *_outfilename,
                  char *_tmpdir)
{
  ld_ipa_opt = _ld_ipa_opt;
  environ_vars = _environ_vars;
  WB_flags = _WB_flags;
  Y_flags = _Y_flags;
  outfilename = _outfilename;
  tmpdir = _tmpdir;
}


/***************************************************************************/
/* gets the ABI type from the linker                */
/***************************************************************************/
static void 
IP_set_target(void)
{
#ifdef _TARG_MIPS
    switch (ld_ipa_opt[LD_IPA_TARGOS].flag) {
    case TOS_MIPS_O32:
	Target_ABI = ABI_32;
	break;
	
    case TOS_MIPS_N32:
	Target_ABI = ABI_N32;
	break;

    case TOS_MIPS_64:
	Target_ABI = ABI_64;
	break;
    default:
	Target_ABI = ABI_N32;
	break;
    }

    switch (ld_ipa_opt[LD_IPA_ISA].flag) {
    case 1:
	Target_ISA = TARGET_ISA_M1;
	break;
    case 2:
	Target_ISA = TARGET_ISA_M2;
	break;
    case 3:
	Target_ISA = TARGET_ISA_M3;
	break;
    case 4:
	Target_ISA = TARGET_ISA_M4;
	break;
    default:
	break;				// use default
    }

    Use_32_Bit_Pointers = (Target_ABI < ABI_64);
#endif

#ifdef TARG_IA64
    Target_ABI = ABI_I64;
    Target_ISA = TARGET_ISA_I1;
    Use_32_Bit_Pointers = FALSE;
#endif

#ifdef _TARG_IA32
    Target_ABI = ABI_I32;
    Target_ISA = TRUE;
#endif

    IPA_Configure_Target ();
}


static MEM_POOL Type_Merge_Pool;

void
ipa_dot_so_init (INT argc, char **argv)
{
    static BOOL ipa_dot_so_initialized = FALSE;
    if (ipa_dot_so_initialized) return;
    ipa_dot_so_initialized = TRUE;

    Temporary_Error_Phase ephase ("IPA initialization");

    MEM_Initialize();
    Preconfigure ();
    Mtype_initialize ();
#ifndef TARG_XTENSA
    IP_set_target();
#else
    Process_IPA_Options (argc, argv);
    Configure_Target ();
#endif
    Dont_Use_WN_Free_List ();

    Init_Operator_To_Opcode_Table ();
    Initialize_Symbol_Tables (TRUE);
    Initialize_Auxiliary_Tables ();
    
    MEM_POOL_Initialize (&Type_Merge_Pool, "TY Merge Pool", 0);
    Initialize_Type_Merging_Hash_Tables (&Type_Merge_Pool);
	
    Set_FILE_INFO_ipa (File_info);	// mark the symtab IPA-generated
    
    if (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_CALL_SHARED_RELOC)
        IPA_Enable_Relocatable_Opt = TRUE;

} /* ipa_dot_so_init */

void
ipa_driver (INT argc, char **argv)
{
    ipa_dot_so_init (argc, argv);

    Verify_Common_Block_Layout ();
	
    Clear_Extra_Auxiliary_Tables ();
    
    MEM_POOL_Delete (&Type_Merge_Pool);

    // turn off these features until they are ported.
    IPA_Enable_Cloning = FALSE;
    IPA_Enable_AutoGnum = TRUE;
    IPA_Enable_DST = FALSE;
#ifndef TARG_XTENSA
    Process_IPA_Options (argc, argv);
    create_tmpdir ( Tracing_Enabled || List_Cite );
#endif

    if (ld_ipa_opt[LD_IPA_SHARABLE].flag & F_STATIC) {
	IPA_Enable_Picopt = FALSE;
	IPA_Enable_AutoGnum = FALSE;
    }

    if (IPA_Enable_Picopt || IPA_Enable_Relocatable_Opt) {
	Pic_optimization ();
    } else {
	Fix_up_static_functions ();
    }

    Perform_Interprocedural_Optimization ();
   
} /* ipa_driver */



/* preempt the definition in be.so, so that we can call ld's cleanup
   routines */
/*ARGSUSED*/
void
Signal_Cleanup (INT sig)
{
    Cleanup_Files (FALSE, TRUE);

    /* now do the ld part */
    /* we fake a fatal error instead of copying all the cleanup code here */
#if defined(TARG_IA64) || defined(TARG_XTENSA)
    fprintf(stderr,"IPA processing aborted");
    exit(1);
#else
    msg (ER_FATAL, ERN_MESSAGE, "IPA processing aborted");
#endif
} /* Signal_Cleanup */
