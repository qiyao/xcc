
/* 
   Copyright (C) 2001-2005 Tensilica, Inc.  All Rights Reserved.
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
 * Module: config_wopt.h
 * $Revision: 1.94 $
 * $Date: 2000/04/06 02:24:15 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/config_wopt.h,v $
 *
 * Revision history:
 *  05-May-96 - Extracted from be/opt/opt_config.h.
 *
 * Description:
 *
 * Declare global flag variables for -WOPT group options.
 * This file is included in common/com/config.c, but should not be
 * otherwise used outside of WOPT.
 *
 * ====================================================================
 * WARNING: WHENEVER A NEW FLAG IS ADDED:
 * ###	- Add its definition to config_wopt.c .
 * ###	- Add it to the group description config_wopt.c .
 * ###	- UPDATE 'class WOPT_SWITCH' DEFINED IN be/opt/opt_main.cxx.
 * ====================================================================
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_wopt_INCLUDED
#define config_wopt_INCLUDED

/* Incomplete types to prevent unnecessary inclusion: */
struct skiplist;

/*********************************************************************
 ***
 *** Anything that may be seen outside of opt_config.c should
 *** be declared in this section.
 ***
 *********************************************************************
 */

extern DLL_SHARED UINT32 WOPT_Alias_Class_Limit;
extern DLL_SHARED UINT32 WOPT_Ip_Alias_Class_Limit;
extern DLL_SHARED BOOL WOPT_Ldx_Ratio_RegIns;
extern DLL_SHARED BOOL WOPT_Enable_Add_Do_Loop_Info;
extern DLL_SHARED BOOL WOPT_Enable_Add_Label_Loop_Info;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_Code_Motion;
extern DLL_SHARED INT32 WOPT_Enable_Aggressive_CM_Limit;	
extern DLL_SHARED INT32 WOPT_Enable_Aggressive_CM_Threshold;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_dce;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_Doloop_Promotion;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_IVR;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_Lftr;
extern DLL_SHARED BOOL WOPT_Enable_Aggressive_Phi_Simp;
extern DLL_SHARED BOOL WOPT_Enable_Aggstr_Reduction;
extern DLL_SHARED BOOL WOPT_Enable_Alias_Classification;
extern DLL_SHARED BOOL WOPT_Enable_Alias_Class_Fortran_Rule;
extern DLL_SHARED BOOL WOPT_Enable_Avoid_Rehash;	/* SSAPRE to try to minimize rehashing*/
extern DLL_SHARED BOOL WOPT_Enable_Backedge_Placement; /* BB on critical backedge */
extern DLL_SHARED BOOL WOPT_Enable_Bitwise_DCE;
extern DLL_SHARED BOOL WOPT_Enable_CSE_FP_comparison;
extern DLL_SHARED BOOL WOPT_Enable_Call_Flag;
extern DLL_SHARED BOOL WOPT_Enable_Calls_Break_BB;
extern DLL_SHARED BOOL WOPT_Enable_Calls_Break_BB_Set;
extern DLL_SHARED BOOL WOPT_Enable_Call_Zero_Version; /* allow zero versions at calls */
extern DLL_SHARED BOOL WOPT_Enable_Canon_Expr;
extern DLL_SHARED BOOL WOPT_Enable_Canon_Uplevel;  /* canonicalize the up level ref */
/* allow operations in expression to be combined (or possibly just
 * rearranged to allow other optimizations).
 */
extern DLL_SHARED BOOL WOPT_Enable_Combine_Operations;
extern DLL_SHARED BOOL WOPT_Enable_Compare_Simp;
extern DLL_SHARED BOOL WOPT_Enable_Const_PRE;
extern DLL_SHARED INT32 WOPT_Enable_Const_PRE_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Copy_Propagate;
extern DLL_SHARED BOOL WOPT_Enable_Copy_Prop_Bad_Ops;
extern DLL_SHARED BOOL WOPT_Enable_Copy_Prop_LNO_Ops;
/* copy prop certain ops into ARRAY subscripts */
extern DLL_SHARED BOOL WOPT_Enable_Copy_Prop_Ops_Into_Array;
extern DLL_SHARED BOOL WOPT_Enable_Copy_Prop_Ops_Into_Array_Set;
extern DLL_SHARED BOOL WOPT_Enable_Cvt_Folding;    /* enable folding of CVT/CVTL in emitter */    
extern DLL_SHARED BOOL WOPT_Enable_DIVREM;		/* allow DIVREM opcode */
extern DLL_SHARED BOOL WOPT_Enable_CG_Alias;
extern DLL_SHARED BOOL WOPT_Enable_CRSIMP;         /* simplify coderep */
extern DLL_SHARED BOOL WOPT_Enable_DCE;
extern DLL_SHARED BOOL WOPT_Enable_DCE_Alias;	/* extra alias analysis w/DCE */
extern DLL_SHARED BOOL WOPT_Enable_DCE_Branch;	/* delete redundant condition */
extern DLL_SHARED INT32 WOPT_Enable_DCE_Branch_Pred_Limit;	/* local search limit */
extern DLL_SHARED BOOL WOPT_Enable_DCE_Global;	/* delete global stores */
extern DLL_SHARED BOOL WOPT_Enable_DCE_Label;	/* delete unref'd labels */
extern DLL_SHARED BOOL WOPT_Enable_Dse_Aggressive;
extern DLL_SHARED BOOL WOPT_Enable_DU_Full;	/* full DU-info for indirects */
extern DLL_SHARED BOOL WOPT_Enable_DU_Union;	/* fix DU w/unions */
extern DLL_SHARED BOOL WOPT_Enable_Estr_FB_Injury; /* use feedback frequency to */
					/* decide when IV updates are */
					/* reparable injuries */
extern DLL_SHARED BOOL WOPT_Enable_Exp_PRE;
extern DLL_SHARED INT32 WOPT_Enable_Exp_PRE_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Dead_CR_Fix;    /* process dead CR in main_emit time */

// Tighten up assertions about interprocedural alias class
extern DLL_SHARED BOOL WOPT_Enable_Debug_Inconsistent_Ip_Class;

extern DLL_SHARED BOOL WOPT_Enable_Edge_Placement; /* insert BB on critical edge */
extern DLL_SHARED BOOL WOPT_Enable_Fast_Simp;	/* temporary 377066 */
extern DLL_SHARED BOOL WOPT_Enable_Fold2const;
extern DLL_SHARED BOOL WOPT_Enable_Fold_Lda_Iload_Istore; // for folding in coderep
extern DLL_SHARED BOOL WOPT_Enable_FSA;
extern DLL_SHARED BOOL WOPT_Enable_Generate_DU;
extern DLL_SHARED INT32 WOPT_Enable_Generate_Trip_Count;
extern DLL_SHARED BOOL WOPT_Enable_Goto;
extern DLL_SHARED BOOL WOPT_Enable_Hoisting;
extern DLL_SHARED BOOL WOPT_Enable_Ivar_Hoisting;
extern DLL_SHARED BOOL WOPT_Enable_I8_Primary_IV;  /* allow primary IV to be I8 */
extern DLL_SHARED BOOL WOPT_Enable_Iload_Prop;	/* propagate expression containing ivars */
extern DLL_SHARED BOOL WOPT_Enable_Improved_Addr_Taken;
extern DLL_SHARED BOOL WOPT_Enable_Input_Prop;     /* copy prop at value number time */
extern DLL_SHARED BOOL WOPT_Enable_Itself_Prop;     /* copy prop of t=func(t) where func
					    contains only t plus other constants */
extern DLL_SHARED BOOL WOPT_Enable_IPAA;           /* enable the use of IPA alias analysis result */
extern DLL_SHARED BOOL WOPT_Enable_IVE;		/* induction-var elimination */
extern DLL_SHARED BOOL WOPT_Enable_IVE_Old;        /* use old IVE with bug in it */
extern DLL_SHARED BOOL WOPT_Enable_Ivar_Common;
extern DLL_SHARED BOOL WOPT_Enable_Ivar_PRE;       /* enable *p as PRE candidate */
extern DLL_SHARED BOOL WOPT_Enable_Ivincr_Cand;
extern DLL_SHARED BOOL WOPT_Enable_IVR;		/* induction-var recognition */
extern DLL_SHARED INT32 WOPT_Enable_IVR_Expand_Limit;  /* limit of expr expansion to search for incr */
					
/* do ivr for outermost in ||-region */
extern DLL_SHARED BOOL WOPT_Enable_IVR_Outermost_Loop_Parallel_Region; 
extern DLL_SHARED BOOL WOPT_Enable_Ldx;            /* index load optimization */
extern DLL_SHARED BOOL WOPT_Enable_Lego_Opt;       /* max optimization for lego */
extern DLL_SHARED BOOL WOPT_Enable_LFTR;           /* linear function test replacement */
extern DLL_SHARED BOOL WOPT_Enable_LFTR_Ivar;      /* handle expr containing ivars */
extern DLL_SHARED BOOL WOPT_Enable_LFTR2;          /* linear function test replacement */
extern DLL_SHARED BOOL WOPT_Enable_LFTR2_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Load_PRE;
extern DLL_SHARED INT32 WOPT_Enable_Load_PRE_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Local_Rvi;      /* enable fast rvi of locals */    
extern DLL_SHARED BOOL WOPT_Enable_Rvi_Uninitialized; /* enable local rvi of undefined */
extern DLL_SHARED INT32 WOPT_Enable_Local_Rvi_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Loopinvarexp_Str_Reduction;
extern DLL_SHARED BOOL WOPT_Enable_Lower_Short_Circuit;
extern DLL_SHARED BOOL WOPT_Enable_Lower_Short_Circuit_Set;
extern DLL_SHARED BOOL WOPT_Enable_LNO_Copy_Propagate;
extern DLL_SHARED BOOL WOPT_Enable_MINMAX;		/* allow MINMAX opcode */
extern DLL_SHARED BOOL WOPT_Enable_Min_Type;       /* use minimum size type in PRE PREG */
extern DLL_SHARED BOOL WOPT_Enable_Move_Intrinsicop;
extern DLL_SHARED BOOL WOPT_Enable_MP_varref;      /* trust the var list in the nested procedure */
extern DLL_SHARED const BOOL WOPT_Enable_MP_Const_Prop;  /* perform const prop into MP region */
extern DLL_SHARED BOOL WOPT_Enable_New_SR;		/* new strength-reduction */
extern DLL_SHARED BOOL WOPT_Enable_New_SR_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Output_Copy;    /* output copy propagation */
extern DLL_SHARED INT32 WOPT_Enable_Ocopy_Lookupstmt;
extern DLL_SHARED BOOL WOPT_Enable_Parm;		/* insert OPTPARM over parms */
extern DLL_SHARED char *WOPT_Enable_Process;
extern DLL_SHARED BOOL WOPT_Enable_Phi_Simp;
extern DLL_SHARED BOOL WOPT_Enable_Prop_Aggressive;/* use inverse function to avoid 
					   overlapping live ranges */
extern DLL_SHARED BOOL WOPT_Enable_Prop_Ivar;	/* copy propagation thru iload's */
extern DLL_SHARED BOOL WOPT_Enable_Prop_CSE;       /* copy propagation of CSE expressions */
extern DLL_SHARED INT32 WOPT_Enable_Prop_Limit;	/* tree height limit in copy prop */
extern DLL_SHARED BOOL WOPT_Enable_Prune;		/* temporary, pv 370066 */
extern DLL_SHARED BOOL WOPT_Enable_Replace_Second_IV; /* Force replacement of secondary IV */
extern DLL_SHARED BOOL WOPT_Enable_Replace_While_Loop_Second_IV; /* Force replacement of secondary IV */
extern DLL_SHARED BOOL  WOPT_Enable_Restricted_Map;
extern DLL_SHARED INT32 WOPT_Enable_Rsv_Bits;	/* reserve bit count in itable */
extern DLL_SHARED BOOL WOPT_Enable_RVI;		/* reg-var identification */
extern DLL_SHARED BOOL WOPT_Enable_RVI1;		/* rvi phase 1 */
extern DLL_SHARED BOOL WOPT_Enable_RVI2;		/* rvi phase 2 */
extern DLL_SHARED BOOL WOPT_Enable_Rviistore;	/* agg. chi-handling on istore*/
extern DLL_SHARED char *WOPT_Enable_Rviskip;	/* skip variable during rvi */
extern DLL_SHARED BOOL WOPT_Enable_Rvisplit;	/* split bbs at ever stmt */
extern DLL_SHARED BOOL WOPT_Enable_Rvivsym;	/* ignore vsym in chi lists */
extern DLL_SHARED BOOL WOPT_Enable_Second_Alias_Class; /* repeat alias class for LNO */
extern DLL_SHARED BOOL WOPT_Enable_Second_Order;
extern DLL_SHARED BOOL WOPT_Enable_Simp_Iload;	/* simplifier folding iload */
extern DLL_SHARED BOOL WOPT_Enable_Simple_If_Conv; /* enable simple if-conversion at CFG build time */
extern DLL_SHARED char *WOPT_Enable_Skip;
extern DLL_SHARED struct option_list *WOPT_Skip;	/* Skip option list */
extern DLL_SHARED struct skiplist *WOPT_Skip_List;	/* Preprocessed skip list */
extern DLL_SHARED BOOL WOPT_Enable_SLT;
extern DLL_SHARED BOOL WOPT_Enable_Small_Br_Target; /* Disable propagation into br BBs */
extern DLL_SHARED BOOL WOPT_Enable_Source_Order;   /* trace BB's in source order */
extern DLL_SHARED BOOL WOPT_Enable_Speculation_Defeats_LFTR;
extern DLL_SHARED BOOL WOPT_Enable_SSA_Minimization; /* SSA minimization in SSAPRE */
extern DLL_SHARED BOOL WOPT_Enable_SSA_PRE;
extern DLL_SHARED BOOL WOPT_Enable_Store_PRE;
extern DLL_SHARED INT32 WOPT_Enable_Store_PRE_Limit;
extern DLL_SHARED BOOL WOPT_Enable_Strength_Reduction;
extern DLL_SHARED BOOL WOPT_Enable_Tail_Recur;	/* tail recursion opt */
extern DLL_SHARED char *WOPT_Set_Unique_Pt;
extern DLL_SHARED BOOL WOPT_Enable_Undef_Prop;
extern DLL_SHARED BOOL WOPT_Enable_Undef_Aggressive_Prop;
extern DLL_SHARED BOOL WOPT_Enable_Unique_Pt_Vsym;
extern DLL_SHARED BOOL WOPT_Enable_Update_Vsym;
extern DLL_SHARED INT32 WOPT_Enable_Value_Numbering; /*0=OFF, 1=after_pre, 2=befr_n_aftr*/
extern DLL_SHARED INT32 WOPT_Enable_Vn_Ivc;        /* Induction variable coalescing; 0=OFF
					 * See be/opt/opt_vn_ivc.h */
extern DLL_SHARED UINT32 WOPT_Enable_Vnfre_After;  /* Disable vnfre after given valnum */
extern DLL_SHARED UINT32 WOPT_Enable_Vnfre_Before; /* Disable vnfre before given valnum */
extern DLL_SHARED BOOL WOPT_Enable_Verbose;
extern DLL_SHARED INT32 WOPT_Enable_Verify;	/* verify data structures */
extern DLL_SHARED BOOL WOPT_Enable_Vsym_Unique;
extern DLL_SHARED BOOL WOPT_Enable_VN_Full;	/* full value number for ivars */
extern DLL_SHARED BOOL WOPT_Enable_While_Loop;	/* cvt while-do to do-loop */
extern DLL_SHARED BOOL WOPT_Enable_Worklist_Pruning;
extern DLL_SHARED BOOL WOPT_Enable_Zero_Version;
extern DLL_SHARED BOOL WOPT_Enable_Strong_Barrier; /* disallow any memop motion across a barrier */
extern DLL_SHARED BOOL WOPT_Enable_Aggr_Invariant; /* aggressive invariant detection */
extern DLL_SHARED BOOL WOPT_Enable_Shrink;         /* enable live range shrinking */
extern DLL_SHARED INT32 WOPT_Enable_Extra_Rename_Pass;
extern DLL_SHARED BOOL  WOPT_Enable_Extra_Rename_Pass_Set;
extern DLL_SHARED UINT32 WOPT_Enable_Extra_Preopt_Pass; // additional iterations of preopt
extern DLL_SHARED BOOL  WOPT_Enable_Bool_Simp; 
extern DLL_SHARED BOOL  WOPT_Enable_Feedback_LPRE;
extern DLL_SHARED BOOL  WOPT_Enable_Feedback_EPRE;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Display;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Merge_Multi_Zone;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Merge_Multi_Zone_Set;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt1;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt2;
extern DLL_SHARED INT32 WOPT_Enable_CFG_Opt2_Limit;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt3;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt4;
extern DLL_SHARED BOOL  WOPT_Enable_CFG_Opt_Limit;
extern DLL_SHARED BOOL  WOPT_Enable_Bits_Load_Store;
extern DLL_SHARED BOOL  WOPT_Enable_Epre_Before_Ivr; // For running epre early
extern DLL_SHARED BOOL  WOPT_Enable_Lpre_Before_Ivr; // For running lpre early
extern DLL_SHARED BOOL  WOPT_Enable_Spre_Before_Ivr; // For running spre early
extern DLL_SHARED BOOL  WOPT_Enable_Bdce_Before_Ivr; // For running bdce early
extern DLL_SHARED BOOL  WOPT_Enable_New_Phase_Ordering; // Enables some phases before ivr
extern DLL_SHARED BOOL  WOPT_Enable_Expand_MPY_Const; // Enable expand mpy by constant
#endif /* config_wopt_INCLUDED */

