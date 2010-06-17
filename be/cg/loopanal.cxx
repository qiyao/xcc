
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



/* ====================================================================
 *
 *
 *
 * This module is the driver for the new loop analysis code.
 *
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/loopanal.cxx,v $ $Revision: 1.25 $";
#endif /* _KEEP_RCS_ID */

#define LOOPANAL

#include "defs.h"
#include "resource.h"
#include "errors.h"

#include "util.h"
#include "tracing.h"
#include "cgir.h"
#include "strtab.h"
#include "const.h"
#include "targ_const.h"
#include "config_asm.h"
#include "bitset.h"
#include "bb.h"

#include "register.h"
#include "tn_set.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gtn_tn_set.h"
#include "bbregs.h"

#include "cg_cflow.h"
#include "op_list.h"
#include "cg_loop.h"
#include "cio_rwtran.h"
#include "note.h"
#include "glob.h"
#include "loopanal.h"


/* ====================================================================
 * ====================================================================
 *
 * General loop analysis utilities.
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 * LC (LA/LS) failure code handling.
 *
 * ====================================================================
 */

static BS *lf_bv;		/* LC failure code BV */
static BOOL lc_failed;		/* a code has been set */

/* ====================================================================
 *
 * InitLF_Reason
 * SetLF_Reason
 *
 * Init/Set Loop Analysis fail codes.
 *
 * ====================================================================
 */

static BOOL lf_reason_init = FALSE;

void
InitLF_Reason ( void )
{

  if ( lf_reason_init == FALSE ) {
     lf_reason_init = TRUE;
     lf_bv = BS_Create_Empty ( LF_Count+1, &MEM_phase_pool );
  }
  else
     lf_bv = BS_ClearD ( lf_bv );
  lc_failed = FALSE;
}


void
SetLF_Reason( INT16 code )
{
  lf_bv = BS_Union1D ( lf_bv, code, &MEM_local_pool );
  lc_failed = TRUE;
}

BOOL
Is_LF_Reason( INT16 code )
{
  return BS_MemberP( lf_bv, code );
}

void 
FinishLF_Reason( void )
{
  lf_reason_init = FALSE;
}

/* ====================================================================
 *
 * LC_Failed
 *
 * Did LA/LS fail?

 * ====================================================================
 */

BOOL
LC_Failed( void )
{
  return lc_failed;
}

typedef struct {
  SRCPOS    srcpos;
  mUINT8    failure_code;
} NOTE_INFO_E;

typedef struct {
  SRCPOS    srcpos;
  BS       *failure_codes;
} NOTE_INFO_F;

#define NOTE_srcpos(n)		((n)->srcpos)
#define NOTE_failure_code(n)	((n)->failure_code)
#define NOTE_failure_codes(n)	((n)->failure_codes)

static const char swpf_asm_prefix[] = ASM_CMNT_LINE"<swpf>";
static const char swpf_list_prefix[] = "<swpf>";

static const char cglf_asm_prefix[] = ASM_CMNT_LINE"<cglf>";
static const char cglf_list_prefix[] = "<cglf>";


/* =======================================================================
 *
 *  If_Conversion_Single_Failure
 *
 *  Handler for the if conversion failure NOTE when there is only a single
 *  failure code (I think this is all the time.)
 *
 * =======================================================================
 */
static void
If_Conversion_Single_Failure(
  NOTE_ACTION action,
  NOTE_INFO  *info,
  FILE       *file
)
{
  const char *prefix;
  SRCPOS srcpos;
  INT32 lineno;
  const char *msg;
  NOTE_INFO_E *infoe = (NOTE_INFO_E*) info;

  switch (action) {
  case NOTE_PRINT_TO_ANL_FILE:
  case NOTE_PRINT_TO_FILE:
    if (action == NOTE_PRINT_TO_ANL_FILE) {
      if ( !Enable_SWP ) break;
    } else if (file == Asm_File) {
      prefix = Enable_SWP ? swpf_asm_prefix : cglf_asm_prefix;
    } else if (file == TFile) {
      prefix = Enable_SWP ? swpf_list_prefix : cglf_list_prefix;
    } else {
      break;
    }

    srcpos = NOTE_srcpos(infoe);
    lineno = SRCPOS_linenum(srcpos);
    msg = LF_Name[NOTE_failure_code(infoe)];

    if ( Enable_SWP ) {
      if ( action == NOTE_PRINT_TO_ANL_FILE ) {
	INT32 fileno = SRCPOS_filenum(srcpos);
	INT32 colno = SRCPOS_column(srcpos);
        fprintf(file, "\nmsg swpf lines [%d %d %d] \"not pipelined -- %s\"\n",
		      fileno,
		      lineno,
		      colno,
		      msg);
      } else {
        fprintf(file,"%s Loop line %d wasn't pipelined because:\n"
	        "%s %5s %s\n"
	        "%s\n",
	        prefix,
	        lineno,
	        prefix,
	        "",
	        msg,
	        prefix);
      }
    } else
      fprintf(file,"%s Loop line %d wasn't if converted because:\n"
	      "%s %5s %s\n"
	      "%s\n",
	      prefix,
	      lineno,
	      prefix,
	      "",
	      msg,
	      prefix);

    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file,"IF_CONVERSION_SINGLE_FAILURE");
    break;
  default:
    FmtAssert(FALSE,("Didn't recognize action"));
  }
}

/* =======================================================================
 *
 *  If_Conversion_Many_Failures
 *
 *  Handler for the if conversion failure note when there are more than
 *  one failure codes.  (I don't think this can happen.)
 *
 * =======================================================================
 */
static void
If_Conversion_Many_Failures(
  NOTE_ACTION action,
  NOTE_INFO  *info,
  FILE       *file
)
{
  INT32 i;
  const char *prefix;
  SRCPOS srcpos;
  INT32 lineno;
  NOTE_INFO_F *infof = (NOTE_INFO_F*) info;

  switch (action) {
  case NOTE_PRINT_TO_FILE:
    if (action == NOTE_PRINT_TO_ANL_FILE) {
      if ( !Enable_SWP ) break;
    } else if (file == Asm_File) {
      prefix = Enable_SWP ? swpf_asm_prefix : cglf_asm_prefix;
    } else if (file == TFile) {
      prefix = Enable_SWP ? swpf_list_prefix : cglf_list_prefix;
    } else {
      break;
    }

    srcpos = NOTE_srcpos(infof);
    lineno = SRCPOS_linenum(srcpos);

    if ( Enable_SWP ) {
      if ( action == NOTE_PRINT_TO_ANL_FILE ) {
	INT32 fileno = SRCPOS_filenum(srcpos);
	INT32 colno = SRCPOS_column(srcpos);
        fprintf(file, "\nmsg swpf lines [%d %d %d] \"not pipelined -- ",
		      fileno,
		      lineno,
		      colno);
	prefix = "";
      } else {
	fprintf(file,"%s Loop line %d wasn't pipelined because:\n",
	        prefix,
	        lineno);
      }
    } else 
      fprintf(file,"%s Loop line %d wasn't if converted because:\n",
	      prefix,
	      lineno);
    
    for ( i = 0; i < LF_Count; ++i ) {
      if ( BS_MemberP(NOTE_failure_codes(infof),i) ) {
	const char *msg = LF_Name[i];
	if ( action == NOTE_PRINT_TO_ANL_FILE ) {
          fprintf(file,"%s%s\"\n", prefix, msg);
	  prefix = "\"";
	} else {
          fprintf(file,"%s %5s %s\n", prefix, "", msg);
	}
      }
    }

    if ( action != NOTE_PRINT_TO_ANL_FILE ) fprintf(file,"%s\n",prefix);
    break;
  case NOTE_PRINT_HANDLER_NAME_TO_FILE:
    fprintf(file,"IF_CONVERSION_MANY_FAILURES");
    break;
  default:
    FmtAssert(FALSE,("Didn't recognize action"));
  }
}


/* =======================================================================
 *
 *  CG_LOOP_NOTE_If_Conversion_Failure
 *
 *  See interface description.
 *
 * =======================================================================
 */
static void
CG_LOOP_NOTE_If_Conversion_Failure(
  BB *bb,
  BS *bs
)
{
  INT32  failure_code;
  INT32  i;
  INT32  failure_count = 0;


  for ( i = 0; i < LF_Count; ++i ) {
    if ( BS_MemberP(bs,i) ) {
      failure_code = i;
      ++failure_count;
    }
  }

  FmtAssert(failure_count > 0,("Unexpected 0 failure count."));

  if ( failure_count == 1 ) {
    NOTE_INFO_E *info = TYPE_MEM_POOL_ALLOC(NOTE_INFO_E,&MEM_pu_pool);

    NOTE_srcpos(info) = BB_Loop_Srcpos(bb);
    NOTE_failure_code(info) = failure_code;

    NOTE_Add_To_BB(bb,If_Conversion_Single_Failure,(NOTE_INFO*) info);
  }
  else {
    NOTE_INFO_F *info = TYPE_MEM_POOL_ALLOC(NOTE_INFO_F,&MEM_pu_pool);

    NOTE_srcpos(info) = BB_Loop_Srcpos(bb);
    NOTE_failure_codes(info) = bs;

    NOTE_Add_To_BB(bb,
		   If_Conversion_Many_Failures,
		   (NOTE_INFO*) info);
  }
}

/* ====================================================================
 *
 * LC_Report_Failure
 *
 * We have failed to overlap a loop.  Report the reason (given the
 * appropriate flags) and clean up.
 *
 * ====================================================================
 */

void
LC_Report_Failure ( BB *body )
{
  char *FailureName;
  INT16 i;

  CG_LOOP_NOTE_If_Conversion_Failure(body,lf_bv);

  if ( Get_Trace(TP_CGPREP, 1) ) {
    fprintf ( TFile, "\n");
    fprintf ( TFile, "<cgprep> Loop if conversion failed for BB:%d\n",
	     BB_id(body) );

    /* Report the reasons:
     */
    for ( i = 0; i < LF_Count; i++ ) {
      if ( BS_MemberP( lf_bv, i ) ) {
        FailureName = LF_Name [i];
	fprintf ( TFile,  "<cgprep> \t%s\n", FailureName );
      }
    }
    fprintf ( TFile, "\n");
  }
}
