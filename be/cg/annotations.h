
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



/* =======================================================================
 * =======================================================================
 *
 *  Module: annotations.h
 *  $Revision: 1.39 $
 *  $Date: 2000/10/09 19:43:17 $
 *  $Author: dehnert $
 *  $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/annotations.h,v $
 *
 *  Description:
 *  ============
 *
 *  General Annotation facility for attaching information to any data
 *  structure. The basic type is ANNOTATION. An annotation list can
 *  be added to a data structure with a field of type (ANNOTATION *).
 *
 *	typedef struct annotation {
 *	  ANNOTATION_KIND kind;
 *        struct annotation *next;
 *	  void *info;
 *      } ANNOTATION;
 *
 *  Thus the information is a pointer sized object 'info' that contains 
 *  a pointer an object of the appropriate type.
 *
 *
 *  When to use Annotations:
 *  ========================
 *
 *  Annotations should be used to attach information that is present only
 *  in a few instances of the data structure. An example is ENTRYINFO
 *  which needs to be present only for entry BBs. Using a BB_MAP for 
 *  this purpose is too wasteful and so is adding a field the BB data
 *  structure for this purpose.
 *
 *  Adding an Annotation:
 *  =====================
 *
 *    ANNOTATION *ANNOT_Add (ANNOTATION *annot_list,
 *			     ANNOTATION_KIND kind,
 *	  		     void *info,
 *	  		     MEM_POOL *pool)
 *
 *	Add an annotation with the given 'kind' and containing 'info'
 *      to the end of 'annot_list'. Memory is allocated from 'pool'
 *      for this new annotation. The return value is the updated list.
 *
 *  Unlinking an Annotation:
 *  ========================
 *
 *    ANNOTATION *ANNOT_Unlink (ANNOTATION *annot_list,
 *			        ANNOTATION *this)
 *
 *  Return a list which does not include the first instance of this.
 *  this is not deleted, and may still point into the original list.
 *
 *  Retreiving Annotations:
 *  =======================
 *
 *    ANNOTATION *ANNOT_Get (ANNOTATION *annot_list, ANNOTATION_KIND kind)
 *
 * 	Get the first annotation of the given 'kind' from the 'annot_list'.
 * 	The return value is NULL, if there is no annotation of this 'kind'.
 *
 *    If there can be more than one annotation of a certain kind, the 
 *    macros ANNOT_First and ANNOT_Next can be used to get the whole
 *    list. For example:
 *
 *	ANNOTATION *ant;
 *
 *	for (ant = ANNOT_First(annot_list, kind); 
 *	     ant != NULL; 
 *	     ant = ANNOT_Next (ant, kind))
 *	{
 *	  use ANNOT_info(ant) ....
 *	}
 *
 * =======================================================================
 */

#ifndef ANNOTATIONS_INCLUDED
#define ANNOTATIONS_INCLUDED

#include "mempool.h"
#include "srcpos.h"
#include "symtab.h"
#include "register.h"
#include "tn_list.h"

typedef enum {
  ANNOT_LABEL 	  = 0,
  ANNOT_PRAGMA    = 1,
  ANNOT_ENTRYINFO = 2,
  ANNOT_EXITINFO  = 3,
  ANNOT_CALLINFO  = 4,
  ANNOT_NOTE	  = 5,
  ANNOT_LOOPINFO  = 6,
  ANNOT_SWITCH 	  = 7,
  ANNOT_ROTATING_KERNEL = 8,
  ANNOT_ASMINFO   = 9
} ANNOTATION_KIND;

class WN;
struct tn;
struct ti_res_count;  // forward declaration

typedef struct annotation {
  ANNOTATION_KIND kind;
  struct annotation *next;
  void *info;
} ANNOTATION;

#define ANNOT_next(a)	((a)->next)
#define ANNOT_info(a)	((a)->info)
#define ANNOT_kind(a)   ((a)->kind)

/* type of information to be returned for the different annotation kinds */
#define ANNOT_label(a)		((LABEL_IDX)(INTPTR)ANNOT_info(a))
#define ANNOT_pragma(a)		((WN *)ANNOT_info(a))
#define ANNOT_entryinfo(a)  	((ENTRYINFO *)ANNOT_info(a))
#define ANNOT_exitinfo(a)  	((EXITINFO *)ANNOT_info(a))
#define ANNOT_callinfo(a)  	((CALLINFO *)ANNOT_info(a))
#define ANNOT_note(a)  		((NOTE *)ANNOT_info(a))
#define ANNOT_loopinfo(a)	((LOOPINFO *)ANNOT_info(a))
#define ANNOT_switch(a)		((ST *)ANNOT_info(a))
#define ANNOT_rotating_kernel(a)   ((ROTATING_KERNEL_INFO*)ANNOT_info(a))
#define ANNOT_asminfo(a)	((ASMINFO *)ANNOT_info(a))


typedef struct loopinfo {
 WN *wn;			/* LOOP_INFO WHIRL node */
 INT swp_failure_code;		/* only used when SWP failed and non-SWP loop
				   optimization is performed */
 ISA_REGCLASS swp_unallococatable_rc;
 				/* used when SWP failed and show which
				   register class is not allocatable */
 struct tn *trip_count_tn;	/* TN holding trip count (if any) */
 struct tn *max_trip_count_tn;	/* TN holding trip count (if any),
				   even for loops that have early
				   exits ('trip_count_tn' will be NULL
				   for loops with early exits). */
 SRCPOS srcpos;			/* source position of start of body */
} LOOPINFO;

#define LOOPINFO_wn(x)			((x)->wn)
#define LOOPINFO_swp_failure_code(x)	((x)->swp_failure_code)
#define LOOPINFO_swp_unallococatable_rc(x)	((x)->swp_unallococatable_rc)
#define LOOPINFO_srcpos(x)		((x)->srcpos)
#define LOOPINFO_line(x)		(Srcpos_To_Line(LOOPINFO_srcpos(x)))
#define LOOPINFO_filenum(x)		(SRCPOS_filenum(LOOPINFO_srcpos(x)))
#define LOOPINFO_trip_count_tn(x)	((x)->trip_count_tn)
#define LOOPINFO_max_trip_count_tn(x)	((x)->max_trip_count_tn)


typedef	struct entryinfo {
  ST *name;		/* entry point name.		     */
  struct op *sp_adj;	/* Entry SP adjustment operation     */
  WN *entry_wn;         /* pointer to entry WN.              */
  TN_LIST *in_params;   /* incoming parameters (dedicated    */
                        /* TNs) to this entry                */
  SRCPOS    srcpos;	/* first source position of func.    */
} ENTRYINFO;

#define ENTRYINFO_name(x)	((x)->name)
#define ENTRYINFO_sp_adj(x)	((x)->sp_adj)
#define ENTRYINFO_entry_wn(x)   ((x)->entry_wn)
#define ENTRYINFO_in_params(x)  ((x)->in_params)
#define ENTRYINFO_srcpos(x)	((x)->srcpos)


typedef struct exitinfo {
  struct op *sp_adj;	/* Exit SP adjustment operation */
  TN_LIST *ret_value;   /* Dedicated TNs for return values */
  SRCPOS    srcpos;	/* source position of function exit */
} EXITINFO;

#define EXITINFO_sp_adj(x)    ((x)->sp_adj)
#define EXITINFO_ret_value(x) ((x)->ret_value)
#define EXITINFO_srcpos(x)    ((x)->srcpos)

typedef struct callinfo {
  ST *call_st;
  WN *call_wn;
  TN_LIST *actual_params;  /* actuals (dedicated TNs) to the call */
  TN_LIST *ret_value;      /* dedicated TNs for return value */
} CALLINFO;

#define CALLINFO_call_st(x)	((x)->call_st)
#define CALLINFO_call_wn(x)	((x)->call_wn)
#define CALLINFO_actual_params(x)	((x)->actual_params)
#define CALLINFO_ret_value(x)	((x)->ret_value)


struct ROTATING_KERNEL_INFO {
  BOOL succeeded;
  INT  failure_code;
  INT ii;
  INT stage_count;
  INT min_ii;
  INT res_min_ii;
  INT rec_min_ii;
  INT sched_len;
  INT min_sched_len;
  INT real_op_count;
  ISA_REGCLASS unallocatable_rc;
  struct ti_res_count *res_counts;
  REGISTER_SET live_in[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET kill[TI_ISA_REGCLASS_MAX+1];
  vector<struct tn *> copyin;
  vector<struct tn *> copyout;
};

#define ROTATING_KERNEL_INFO_succeeded(x)     ((x)->succeeded)
#define ROTATING_KERNEL_INFO_failure_code(x)  ((x)->failure_code)
#define ROTATING_KERNEL_INFO_live_in(x)       ((x)->live_in)
#define ROTATING_KERNEL_INFO_kill(x)          ((x)->kill)
#define ROTATING_KERNEL_INFO_ii(x)            ((x)->ii)
#define ROTATING_KERNEL_INFO_stage_count(x)   ((x)->stage_count)
#define ROTATING_KERNEL_INFO_min_ii(x)        ((x)->min_ii)
#define ROTATING_KERNEL_INFO_res_min_ii(x)    ((x)->res_min_ii)
#define ROTATING_KERNEL_INFO_rec_min_ii(x)    ((x)->rec_min_ii)
#define ROTATING_KERNEL_INFO_sched_len(x)     ((x)->sched_len)
#define ROTATING_KERNEL_INFO_min_sched_len(x) ((x)->min_sched_len)
#define ROTATING_KERNEL_INFO_res_counts(x)    ((x)->res_counts)
#define ROTATING_KERNEL_INFO_real_op_count(x) ((x)->real_op_count)
#define ROTATING_KERNEL_INFO_copyin(x)        ((x)->copyin)
#define ROTATING_KERNEL_INFO_copyout(x)       ((x)->copyout)
#define ROTATING_KERNEL_INFO_unallocatable_rc(x)       ((x)->unallocatable_rc)


typedef struct asminfo {
  REGISTER_SET livein[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET liveout[TI_ISA_REGCLASS_MAX+1];
  REGISTER_SET kill[TI_ISA_REGCLASS_MAX+1];
} ASMINFO;

#define ASMINFO_livein(x)	((x)->livein)
#define ASMINFO_liveout(x)	((x)->liveout)
#define ASMINFO_kill(x)		((x)->kill)


extern ANNOTATION *ANNOT_Add (
  ANNOTATION *annot_list, 
  ANNOTATION_KIND kind, 
  void *info,
  MEM_POOL *pool);

extern ANNOTATION *ANNOT_Unlink (
  ANNOTATION *annot_list, 
  ANNOTATION *this1);

extern ANNOTATION *ANNOT_Get (ANNOTATION *annot_list, ANNOTATION_KIND kind);

#define ANNOT_First(list,kind)	(ANNOT_Get (list, kind))
#define ANNOT_Next(list,kind)	(ANNOT_Get (ANNOT_next(list), kind))


struct ASM_OP_ANNOT 
{
  const WN* wn;

  REGISTER_SET clobber_set[TI_ISA_REGCLASS_MAX+1];

  const char* result_constraint[10];
  ISA_REGSUBCLASS result_subclass[10];
  mUINT32 result_position[10];
  bool result_clobber[10];
  bool result_memory[10];

  const char* opnd_constraint[TI_ISA_OPERANDS_MAX];
  ISA_REGSUBCLASS opnd_subclass[TI_ISA_OPERANDS_MAX];
  mUINT32 opnd_position[TI_ISA_OPERANDS_MAX];
  bool opnd_memory[TI_ISA_OPERANDS_MAX];
};

#define ASM_OP_wn(x)			((x)->wn)
#define ASM_OP_clobber_set(x)		((x)->clobber_set)
#define ASM_OP_result_constraint(x)	((x)->result_constraint)
#define ASM_OP_result_subclass(x)	((x)->result_subclass)
#define ASM_OP_result_position(x)	((x)->result_position)
#define ASM_OP_result_clobber(x)	((x)->result_clobber)
#define ASM_OP_result_memory(x)		((x)->result_memory)
#define ASM_OP_opnd_constraint(x)	((x)->opnd_constraint)
#define ASM_OP_opnd_subclass(x)		((x)->opnd_subclass)
#define ASM_OP_opnd_position(x)		((x)->opnd_position)
#define ASM_OP_opnd_memory(x)		((x)->opnd_memory)


#endif /* ANNOTATIONS_INCLUDED */

