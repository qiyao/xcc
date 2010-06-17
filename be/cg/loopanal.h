
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



#ifndef loopanal_INCLUDED
#define loopanal_INCLUDED

#ifdef _KEEP_RCS_ID
static const char loopanal_rcs_id[] = "$Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/loopanal.h,v $ $Revision: 1.13 $";
#endif /* _KEEP_RCS_ID */

/* Reasons for overlapped loop analysis/scheduling failure: */
typedef INT LF_Type;	/* a mask of the following bits (from right) */
#define LF_Success	0	/* Analysis and scheduling successful */
#define LF_Empty	1	/* Empty loop body */
#define LF_Recur_Sch	2	/* Recurrence scheduling failure */
#define LF_Total_Sch	3	/* Post-recurrence sched failure */
#define LF_Iter_Sch	3	/* Post-recurrence sched failure */
#define LF_GPR_Alloc	4	/* GPR allocation failure */
#define LF_MC_Alloc	5	/* Multiconnect allocation failure */
#define LF_Small_Trip	6	/* Small trip count */
#define LF_GOTO		7	/* Computed or Assigned GOTOs */
#define LF_Cycle	8	/* Cycles in the DoBody */
#define LF_Intrinsic	9	/* Intrinsic call in the DoBody */
#define LF_IO_Call	10	/* I/O subprogram call in the DoBody */
#define LF_User_Call	11	/* User subroutine call in the DoBody */
#define LF_Function	12	/* User function call in the DoBody */
#define LF_Return	13	/* Return from the DoBody */
#define LF_Exit		14	/* Branch out of the DoBody */
#define LF_ldlit	15	/* ldlit (i.e. global spill) in DoBody */
#define LF_MRF		16	/* MRF operation in the DoBody */
#define LF_svc		17	/* svc operation in the DoBody */
#define LF_Multiple_IF	18	/* Multiple IFs in the DoBody */
#define LF_Unrec_IF	19	/* Unrecognized IF in the DoBody -- NP*/
#define LF_Memory	19	/* Out of memory -- NPR */
#define LF_BigBody	20	/* DoBody too big to schedule */
#define LF_Entry	21	/* Branch into the DoBody */
#define LF_Equiv_I	22	/* Equivalenced loop control variable */
#define LF_Float_I	23	/* Non-integer loop control variable */
#define LF_Float_Bound	24	/* Non-integer loop bound */
#define LF_Float_Skip	25	/* Non-integer loop skip increment */
#define LF_Missing_Tree	26	/* I/O item without tree node */
#define LF_Equiv_Out	27	/* Equivalenced output item */
#define LF_Dup_Result	28	/* Duplicate result items */
#define LF_Diff_InOut	29	/* I/O items for different variables */
#define LF_Normalize	30	/* Unable to normalize a subscript */
#define LF_Branch_ICR	31	/* Error while analyzing branch conds. */
#define LF_Store_Conv	32	/* Failure during store conversion */
#define LF_IF		33	/* IF in DO body */
#define LF_No_IF	34	/* Opt. level disallows IF conv. */
#define LF_Sel_2_Cmv    35	/* Failed to convert a select to a cmove */
#define LF_Ifixup	36	/* TN use without definition detected */
#define LF_Other	37	/* Unimplemented construct seen */
#define LF_Not_Peelable	38	/* Trip test not peelable */
#define LF_Cond_Def     39      /* Horrible non-cmove cond_def */
#define LF_Volatile     40      /* Volatile op seen */
#define LF_Needs_X1	41	/* Requires TENV:X=1 to if-convert */
#define LF_Needs_X2	42	/* Requires TENV:X=2 to if-convert */
#define LF_Needs_X3	43	/* Requires TENV:X=3 to if-convert */
#define LF_Spec_Imul    44      /* spec integer mul flag turned off for if-conv */
#define LF_Spec_Fdiv    45      /* spec fdiv turned off for if_conversion */
#define LF_Spec_Fsqrt   46      /* spec fsqrt turned off for if_conversion */
#define LF_Guard_Ops    47      /* too many guard ops */
#define LF_Body_Freq    48      /* fb freq of conditional BB too small */
#define LF_Spec_Idiv    49      /* spec integer div flag turned off for if-conv */
#define LF_Not_Innermost 50     /* not an innermost loop */
#define LF_IFC_RATIO     51     /* ifc ins has added too many inst */

#define LF_Count	52	/* Number of reasons */

/* Early loop exit definitions.
 */
#define LC_MAX_LOOP_EXITS      8


/* ====================================================================
 * External declarations of global data:
 * ====================================================================
 */

#ifndef LOOPANAL

/* Reasons for overlap failure: */
extern char *LF_Name[LF_Count];	/* Error message descriptions */
extern LF_Type LF_Reason;	/* Mask of the possible reasons */

/* Remember how many TNs there were when we started: */
extern INT Original_TN_count;

/* Define the block structure: */
extern INT ifcount;

/* Timing information: */
extern BOOL Trace_Time;			/* Generate timing information */
extern TIME_INFO time_prep, tmp_prep;	/* Total preparation time */
extern TIME_INFO time_iness, tmp_iness;/* Inessential instr removal */


/* ====================================================================
 * Actual declarations of global data, for loopanal.c only:
 * ====================================================================
 */
#else

/* Remember how many TNs there were when we started: */
INT Original_TN_count;

/* Define the block structure: */
INT ifcount;

/* Timing information: */
BOOL Trace_Time;			/* Generate timing information */
TIME_INFO time_prep, tmp_prep;		/* Total preparation time */
TIME_INFO time_iness, tmp_iness;	/* Inessential instr removal */

LF_Type LF_Reason;
char *LF_Name[LF_Count] = {
	"Loop analysis and scheduling successful",	/* must be first */
	"Empty loop body",
	"Recurrence scheduling failure",
	"Post-recurrence scheduling failure",
	"GPR allocation failure",
	"CRM allocation failure",
	"Small trip count",
	"Computed or Assigned GOTOs in the loop body",
	"Cycles in the loop body",
	"Intrinsic call in the loop body",
	"I/O subprogram call in the loop body",
	"CALL statement in the loop body",
	"Function call in the loop body",
	"Return from the loop body",
	"Branch out of the loop body",
	"Out of GPRs (ldlit) in the loop body",
	"MRF operation in the loop body",
	"svc operation in the loop body",
	"Too many IFs in the loop body",
	"Out of memory",
	"loop body too large to schedule",
	"Branch into loop body",
	"Equivalenced loop control variable",
	"Non-integer loop control variable",
	"Non-integer loop bound",
	"Non-integer loop skip increment",
	"I/O item without associated tree node",
	"Equivalenced LHS in the loop body",
	"Duplicate result items in the loop body",
	"Global spill code (mismatched items) in the loop body",
	"Unable to normalize subscript expression",
	"Error in the analysis of branch conditions",
	"Unable to transform conditional store",
	"IF in loop body",
	"Optimization level disallows IF conversion",
	"Failed to convert a select to a cmove",
	"TN use without definition detected",
	"Unimplemented construct seen",
	"Loop exit test too complicated (optimizer bug?)",
	"Optimizer determined that loop requires misaligned references",
	"Loop contains volatile operation(s)",
	"Need to speculate operation(s) that may trap: try -TENV:X=1",
	"Need to speculate operation(s) that may trap: try -TENV:X=2",
	"Need to speculate operation(s) that may trap: try -TENV:X=3",
	"Speculate imul (-CG:spec_imul) turned off",
	"Speculate fdiv (-CG:spec_fdiv) turned off",
	"Speculate fsqrt (-CG:spec_fsqrt) turned off",
	"Number of guard ops exceeds maximum",
	"feedback frequency of conditional BB too small",
	"Speculate idiv (-CG:spec_idiv) turned off",
	"Not an innermost loop",
	"instructions added exceeds -CG:body_ifc_ratio"
};
#endif

/* LA/LS failure code routines.
 */
extern void InitLF_Reason ( void ) ;
extern void SetLF_Reason( INT16 code );
extern BOOL Is_LF_Reason( INT16 code );
extern void FinishLF_Reason( void );
extern void LC_Report_Failure ( BB *body );
extern BOOL LC_Failed ( void );

extern void Check_Suitability (void );


/* The following routines allocate memory using lalloc or palloc,
 * check for success, and either fail or set LF_Reason:
 */

extern void Remove_Inessential_Instructions ( void );

#endif /* loopanal_INCLUDED */

