
/* 
   Copyright (C) 2004 Tensilica, Inc.  All Rights Reserved.
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


/* main driver for front end */
#include <stdio.h>
#include <stdlib.h>
#include <cmplrs/rcodes.h>
#include "wfe_misc.h"
#include "glob.h"

#define INT int
#define RC_OKAY 0
#define BOOL int

// gnu_init returns file to compile, like t.i (not t.c!)
extern char * gnu_init (INT argc, char **argv, char **envp);
extern void compile_file (char * file_name);
extern void check_gnu_errors (int * err_count, int * sorry_count);

int
main ( 
  INT argc,	/* Number of command line arguments */
  char **argv,	/* Array of command line arguments */
  char **envp)	/* Array of environment pointers */
{
	INT error_count, sorry_count;
	BOOL need_inliner;
	Orig_Src_File_Name = gnu_init (argc, argv, envp);
	WFE_Init (argc, argv, envp);	/* sgi initialization */
#ifdef TENSILICA_CHANGES
	WFE_Target_Init ();             /* target specific initialization. */
	WFE_XC_Decl_Init ();		/* Xcalibur pre-defined ctypes/intr
					   initialization */
#endif /* TENSILICA_CHANGES */
	WFE_File_Init (argc, argv);	/* inits per source file */

	compile_file (Orig_Src_File_Name);

	check_gnu_errors (&error_count, &sorry_count);


	if (error_count || sorry_count) {
		Close_Output_Info(); 
		exit (RC_USER_ERROR);
	}

	WFE_File_Finish ();
        WFE_Finish ();
	WFE_Check_Errors (&error_count, &sorry_count, &need_inliner);
	if (error_count)
    		Terminate (RC_INTERNAL_ERROR) ;
  	if (need_inliner)
		exit ( RC_NEED_INLINER );
	exit (RC_OKAY);
}