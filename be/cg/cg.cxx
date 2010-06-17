
/* 
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
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
 * Module: cg.cxx
 * $Revision: 1.249 $
 * $Date: 2000/06/29 23:51:47 $
 * $Author: mpm $
 * $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/cg.cxx,v $
 *
 * Description:
 *
 * This	file contains the main driver and initialization,termination
 * routines for	the code generator.
 *
 * ====================================================================
 * ====================================================================
 */

#include "defs.h"
#include "wn.h"
#include "be_util.h"             // for DO_CU_Skip()
#include "cg.h"
#include "cg_internal.h"
#include "cg_flags.h"
#include "config.h"
#include "config_list.h"
#include "tracing.h"
#include "timing.h"
#include "strtab.h"
#include "cgir.h"
#include "erglob.h"
#include "ercg.h"
#include "data_layout.h"
#include "whirl2ops.h"
#include "calls.h"
#include "bitset.h"
#include "tn_set.h"
#include "gtn_universe.h"
#include "bb_set.h"
#include "register.h"
#include "gra.h"
#include "freq.h"
#include "fb_whirl.h"
#include "lra.h"
#include "cgemit.h"
#include "cgprep.h"
#include "glob.h"
#include "cgexp.h"
#include "igls.h"
#include "tn_map.h"
#include "cg_region.h"
#include "wn_util.h"
#include "cg_spill.h"
#include "localize.h"
#include "gra_live.h"
#include "opt_alias_interface.h"
#include "ir_reader.h"
#include "cflow.h"
#include "dwarf_DST_mem.h"
#include "region_util.h"
#include "eh_region.h"
#include "reg_live.h"
#include "findloops.h"
#include "cgdriver.h"
#include "label_util.h"
#include "cgtarget.h"
#include "ebo.h"
#include "hb.h"
#include "pqs_cg.h"
#include "tag.h"
#include "cg_tie.h"
#include "cmotion.h"
#include "cg_special_op.h"
#include "cg_loop.h"	/* for Remove_Guard_Branch() */
#include "cg_autotie.h"
#include "entry_exit_targ.h"
#include "cg_prof.h"
#include "cg_swp.h"
#include "cg_swp_options.h"

#ifdef TARG_XTENSA
#include "iselector.h"
#endif

MEM_POOL MEM_local_region_pool;	/* allocations local to processing a region */
MEM_POOL MEM_local_region_nz_pool;

BOOL Trace_REGION_Interface = FALSE;

BOOL PU_Has_Calls;
BOOL PU_References_GP;

RID *Current_Rid;

TN_MAP TN_To_PREG_Map;

/* WOPT alias manager */
struct ALIAS_MANAGER *Alias_Manager;

static BOOL Orig_Enable_SWP;


/* Stuff that needs to be done at the start of each PU in cg. */
void
CG_PU_Initialize (WN *wn_pu)
{

  MEM_POOL_Push ( &MEM_phase_pool );
  MEM_POOL_Push ( &MEM_local_pool );
  MEM_POOL_Push ( &MEM_phase_nz_pool );
  MEM_POOL_Push ( &MEM_local_nz_pool );

  PU_Has_Calls = FALSE;
  PU_References_GP = FALSE;

  Regcopies_Translated = FALSE;

  if (DO_CU_Skip()) {
    /* Enable debugging using CU skip */
    static BOOL is_done = FALSE;
    INT cu_num = Current_CU_Count();
    if ( (cu_num >= 0) &&
         (   cu_num < CG_CU_skip_before
          || cu_num > CG_CU_skip_after
          || cu_num == CG_CU_skip_equal) ) {
      Opt_Level = 0;  // don't bother with CG_opt_level
      if (!is_done) {
        printf("  <<CU_Skip>> CG: skip optimizing %s (%d) ......\n",
               Src_File_Name, cu_num);
        is_done = TRUE;
      }
    }
  }

  INT pu_num = Current_PU_Count();
  BOOL do_pu_skip =    pu_num < CG_skip_before
  		    || pu_num > CG_skip_after
		    || pu_num == CG_skip_equal;
  CG_Configure_Opt_Level(do_pu_skip ? 0 : Opt_Level);
  if (do_pu_skip) {
    printf("  <<PU_Skip>> CG: skip optimizing %s(%d) ......\n",
               Cur_PU_Name, pu_num);
  }

  if (PU_has_syscall_linkage(Get_Current_PU())) {
	// turn off swp so stacked registers are preserved
	Orig_Enable_SWP = Enable_SWP;
	Enable_SWP = FALSE;
  }

  Reuse_Temp_TNs = (CG_opt_level == 0);
  if (Get_Trace (TP_CGEXP, 1024)) Reuse_Temp_TNs = FALSE;

  CGTARG_Initialize();
  BB_PU_Initialize ();
  Init_TNs_For_PU ();
  LOOP_DESCR_Init_For_PU();
  TN_MAP_Init();
  BB_MAP_Init();
  OP_MAP_Init();

  if (SWP_Options.Enable_Op_Info) {
    swp_op_map = OP_MAP_Create();
  } else
    swp_op_map = NULL;

  CGSPILL_Initialize_For_PU ();
  CFLOW_Initialize();
  CGPREP_Init();
  HB_Init();
  if (Enable_CG_Peephole) EBO_Init();
  Init_Label_Info();
  BBlist_Initialize();
  
#ifdef EMULATE_LONGLONG
  extern void Init_TN_Pair();
  Init_TN_Pair ();
#endif

  /* initialize register package for current pu */
  REGISTER_Pu_Begin();

  Init_Entry_Exit_Code (wn_pu);
  REGISTER_Reset_FP();	// in case $fp is used, must be after entry_exit init

  /* Initialize global tn universe */
  GTN_UNIVERSE_Pu_Begin();

  Trace_REGION_Interface = Get_Trace( TP_REGION, TT_REGION_CG_DEBUG ) ||
    Get_Trace(TP_REGION, TT_REGION_BOUND_DEBUG );

  Init_gen_quad_preg(); // init statics to hold quad preg STs
  
  // Autotie analysis structures and routines.
  AUTOTIE_Initialize();

  /* data layout and lowering now happens earlier, in bedriver. */
  /* don't finalize stack frame until just before emit, after all spills. */
}

void
CG_PU_Finalize(void)
{
  AUTOTIE_Finalize();

  TAG_Finish();
  GTN_UNIVERSE_Pu_End ();

  if (swp_op_map) {
    OP_MAP_Delete(swp_op_map);
    swp_op_map = NULL;
  }

  OP_MAP_Finish();
  CGSPILL_Finalize_For_PU();
  if (Enable_CG_Peephole) EBO_Finalize();

  if (PU_has_syscall_linkage(Get_Current_PU())) {
	Enable_SWP = Orig_Enable_SWP;
  }

  /* TN_To_PREG_Map is allocated from MEM_pu_pool and so can't be popped
     but it can be put on the free list and cleared at the end of the PU */
  TN_MAP_Delete(TN_To_PREG_Map);
  TN_To_PREG_Map = NULL;

  /*  Added a memory pool for BBLIST, so it is no need to invoke Free_BB_Memory */
  //Free_BB_Memory();		    /* Free non-BB_Alloc space. */
  BBlist_Finalize();                // make sure all memory used for BBLIST are freed
  MEM_POOL_Pop ( &MEM_local_pool );
  MEM_POOL_Pop ( &MEM_local_nz_pool );
  MEM_POOL_Pop ( &MEM_phase_pool );
  MEM_POOL_Pop ( &MEM_phase_nz_pool );
}

/* Stuff that needs to be done at the start of each REGION in cg. */
static void
CG_Region_Initialize (WN *rwn, struct ALIAS_MANAGER *alias_mgr)
{
  MEM_POOL_Push (&MEM_local_region_pool);
  MEM_POOL_Push (&MEM_local_region_nz_pool);
  Init_CG_Expand ();
  FREQ_Region_Initialize ();
  BB_REGION_Initialize ();
  LRA_Init();
  GRA_Initialize();
  Init_TNs_For_REGION ();
  /*
   * Create Array to map PREGs into TNs
   * Must be done after Init_Entry_Exit_Code, since
   * Init_Entry_Exit_Code creates special PREGs to represent
   * save locations in WHIRL
   */
  PREG_NUM last_preg_num;
  last_preg_num = Get_Preg_Num (PREG_Table_Size(CURRENT_SYMTAB))+1;
  PREG_To_TN_Array = (TN **) Pu_Alloc (sizeof (TN *) * last_preg_num);
  PREG_To_TN_Mtype = (TYPE_ID *) Pu_Alloc (sizeof (TYPE_ID) * last_preg_num);

  PREG_To_TN_Clear();	/* this enforces different preg maps between regions */
  if (TN_To_PREG_Map == NULL)
    TN_To_PREG_Map = TN_MAP_Create();

  TN_CORRESPOND_Free(); /* remove correspondence between tns (ex. divrem) */

  GTN_UNIVERSE_REGION_Begin();

  Whirl2ops_Initialize(alias_mgr);

  Current_Rid = REGION_get_rid( rwn );
}

/*
 * Stuff that needs to be done at the end of each REGION in cg.
 * This includes making glue code to map TNs in CG'd code
 * from/to PREGs in WHIRL
 */
static void
CG_Region_Finalize (WN *result_before, WN *result_after,
		    WN *rwn, struct ALIAS_MANAGER *am, BOOL generate_glue_code)
{
  RID *rid;
  CGRIN *cgrin;
  WN *entry_fixup, *exit_fixup;
  INT32 i, num_exits;

  Is_True(REGION_consistency_check(rwn),("CG_Region_Finalize"));
  rid = REGION_get_rid( rwn );
  cgrin = RID_cginfo( rid );
  FmtAssert(rid != NULL && cgrin != NULL,
	    ("CG_Region_Finalize, inconsistent region"));

  REGION_set_level(rid, RL_CGSCHED);

  if (generate_glue_code) {
    /* region entry glue code */
    entry_fixup = CGRIN_entry_glue( cgrin );
    REGION_Entry_PREG_Whirl( rid, entry_fixup, CGRIN_tns_in( cgrin ), am );
    if ( Trace_REGION_Interface ) {
      fprintf( TFile, "<region> Entry glue code for RGN %d\n", RID_id(rid) );
      fdump_tree( TFile, entry_fixup );
    }
    WN_INSERT_BlockFirst( result_before, entry_fixup );

    num_exits = RID_num_exits( rid );
    for (i=0; i<num_exits; i++) {
      exit_fixup = CGRIN_exit_glue_i( cgrin, i );
      REGION_Exit_PREG_Whirl( rid, i, exit_fixup,
			     CGRIN_tns_out_i( cgrin, i ), am );
      if ( Trace_REGION_Interface ) {
	fprintf( TFile, "<region> Exit glue code for exit %d RGN %d\n",
		i, RID_id(rid) );
	fdump_tree( TFile, exit_fixup );
      }
      WN_INSERT_BlockLast( result_after, exit_fixup );
    }
  }

  Whirl2ops_Finalize();

  MEM_POOL_Pop (&MEM_local_region_pool);
  MEM_POOL_Pop (&MEM_local_region_nz_pool);
}


/* Can be called two ways:
   1) on a region (pu_dst is NULL, returns code)
   2) on a PU (pu_dst is no NULL, returns NULL)
*/
WN *
CG_Generate_Code( 
    WN *rwn, 
    struct ALIAS_MANAGER *alias_mgr, 
    DST_IDX pu_dst, 
    BOOL region )
{
/*later:  BOOL region = DST_IS_NULL(pu_dst); */
  BOOL orig_reuse_temp_tns = Reuse_Temp_TNs;

  Alias_Manager = alias_mgr;

  Set_Error_Phase( "Code Generation" );
  Start_Timer( T_CodeGen_CU );

  CG_Region_Initialize ( rwn, alias_mgr );

  if (tie_info->has_tie_branch_macro()) {
    Set_Error_Phase ( "TIE branch conversion" );
    /* identify and convert TIE branches */
    convert_tie_branches(rwn);
  }

  Set_Error_Phase ( "Code_Expansion" );
  Start_Timer ( T_Expand_CU );

  // If this PU is simply a wrapper for assembly code to be placed
  // into the .s file, take care of that job and move on.
  if (WN_operator(rwn) == OPR_FUNC_ENTRY &&
      ST_asm_function_st(*WN_st(rwn))) {
    if (!Run_Autotie) {
      FmtAssert(Assembly && !Object_Code,
	      ("Cannot produce non-assembly output with file-scope asm"));
      fprintf(Asm_File, "\n%s\n", ST_name(WN_st(rwn)));
    }
    return rwn;
  }

  /* Call per-pu ISEL routines */
#ifdef TARG_XTENSA
  ISEL_initialize();
#endif
  Convert_WHIRL_To_OPs ( rwn );
  Check_for_Dump ( TP_CGEXP, NULL );

  Split_BBs();

  // If using feedback, incorporate into the CFG as early as possible.
  // This phase also fills in any missing feedback using heuristics.
  if (Cur_PU_Feedback) {
    Set_Error_Phase ("FREQ feedback");
    Start_Timer (T_Freq_CU);
    FREQ_Incorporate_Feedback ( rwn );
    Stop_Timer (T_Freq_CU);
    Set_Error_Phase ( "Code_Expansion" );
  }

  EH_Prune_Range_List();

  Optimize_Tail_Calls( Get_Current_PU_ST() );

  Init_Callee_Saved_Regs_for_REGION( Get_Current_PU_ST(), region );
  Generate_Entry_Exit_Code ( Get_Current_PU_ST(), region );

#define Profile_PU_New_Checksum64(sum, bits)	\
	(((sum) << 3 | (sum) >> 61)  ^ (unsigned long long)(bits))

  unsigned long long challenge;
  Cur_PU_Profile     = Get_Cur_PU_Profile_Data(Src_File_Name, Cur_PU_Name, &challenge);
  if (Cur_PU_Profile) {
    unsigned long long checksum = 0;
    BB *first_bb = REGION_First_BB;
    BB *bb;
    for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
      checksum = Profile_PU_New_Checksum64(checksum, BB_length(bb));
    }
    if (checksum!=challenge)
      Cur_PU_Profile = NULL;
  }
  if (!Cur_PU_Feedback && Cur_PU_Profile) {

    unsigned long long checksum = 0;

    Set_Error_Phase ("FREQ profile");
    Start_Timer (T_Freq_CU);

    FREQ_Incorporate_Profile ();
    Stop_Timer (T_Freq_CU);
    Set_Error_Phase ( "Code_Expansion" );
  }

  // generate bb range info for profiling if -g is specified
  //if (Debug_Level>0)
    //CG_generate_bb_range = true;

  // initialize profile_bb_id for each op if -prof_create is turned on
  // we also turn off unrolling since it messes up the block execution count
  if (CG_generate_bb_range) {
    INT i;
    BB *first_bb = REGION_First_BB;

    unsigned long long checksum = 0;

    BB *bb;
    for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
      OP* op;
      int bb_id = BB_id(bb);
      FOR_ALL_BB_OPs_FWD(bb,op) {
        Set_OP_profile_bb_id(op, bb_id);
      }
      checksum = Profile_PU_New_Checksum64(checksum, BB_length(bb));
    }
    profile_info_record_checksum(checksum);

  }

  Stop_Timer ( T_Expand_CU );
  Check_for_Dump ( TP_CGEXP, NULL );

  if (CG_localize_tns) {
    /* turn all global TNs into local TNs */
    Set_Error_Phase ( "Localize" );
    Start_Timer ( T_Localize_CU );
    Localize_Any_Global_TNs(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_Localize_CU );
    Check_for_Dump ( TP_LOCALIZE, NULL );
  } else {
    /* Initialize liveness info for new parts of the REGION */
    /* also compute global liveness for the REGION */
    Set_Error_Phase( "Global Live Range Analysis");
    Start_Timer( T_GLRA_CU );
#ifdef TARG_XTENSA
    /* Show that trip count tns should be considered live. */
    GRA_LIVE_include_trip_count = TRUE;
#endif
    GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
    Stop_Timer ( T_GLRA_CU );
    Check_for_Dump ( TP_FIND_GLOB, NULL );
  }

  if (Enable_CG_Peephole) {
    Set_Error_Phase("Extended Block Optimizer (Pre_Process)");
    Start_Timer(T_EBO_CU);
    EBO_Pre_Process_Region (region ? REGION_get_rid(rwn) : NULL);
    Stop_Timer ( T_EBO_CU );
    Check_for_Dump ( TP_EBO, NULL );

    Set_Error_Phase ( "Convert to predicted branches" );
    if (CG_opt_level > 0 && xt_brt) {
      for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
	CG_Convert_To_Predicted_Branch(bb);
      }
    }

  }

  if (CM_Enable&& (CG_opt_level >= 2)) {
    Set_Error_Phase("Code Motion");
    CM_Optimize_Region();

    /* Code motion may have changed local TNs to global TNs and vice versa */
    Start_Timer( T_GLRA_CU);
    GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
    Stop_Timer ( T_GLRA_CU );
    Check_for_Dump (TP_FIND_GLOB, NULL);
  }

  // Optimize control flow (first pass)
  if (CG_opt_level > 0 && CFLOW_opt_before_cgprep) {
    // Perform all the optimizations that make things more simple.
    // Reordering doesn't have that property.
    CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_IN_CGPREP)
		   & ~(CFLOW_FREQ_ORDER | CFLOW_REORDER),
		   "CFLOW (first pass)");
  }

  if (!xt_flix) {

    // if flix is turned off explicitly, scan for any op that cannot be
    // emitted

    for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
      OP* op;
      FOR_ALL_BB_OPs(bb,op)
      {
        if (TI_TOP_No_Valid_Format(OP_code(op))) {
	  ErrMsgSrcpos(
		EC_CG_no_valid_format, OP_srcpos(op), TI_TOP_Name(OP_code(op)));
	}
      }
    }
  }

  // Invoke global optimizations before register allocation at -O2 and above.
  if (CG_opt_level > 1) {

    // Compute frequencies using heuristics when not using feedback.
    // It is important to do this after the code has been given a
    // cleanup by cflow so that it more closely resembles what it will
    // to the later phases of cg.
    if (!Cur_PU_Feedback && !Cur_PU_Profile) {
      Set_Error_Phase("FREQ heuristics");
      Start_Timer (T_Freq_CU);
      FREQ_Compute_BB_Frequencies();
      Stop_Timer (T_Freq_CU);
    }

    // Perform hyperblock formation (if-conversion).  Only works for
    // IA-64 at the moment. 
    //
    if (CGTARG_Can_Predicate()) {
      // Initialize the predicate query system in the hyperblock formation phase
      HB_Form_Hyperblocks(region ? REGION_get_rid(rwn) : NULL, NULL);
      if (!PQSCG_pqs_valid()) {
	PQSCG_reinit(REGION_First_BB);
      }
    }
    
    // Run a pass of EBO AFTER the frequency info is valid and BEFORE
    // loop optimization to maximize single BB loops
    if (Enable_CG_Peephole) {
      Set_Error_Phase( "Extended Block Optimizer (first)");
      Start_Timer( T_EBO_CU );
      EBO_Process_Region (region ? REGION_get_rid(rwn) : NULL);
      Stop_Timer ( T_EBO_CU );
      Check_for_Dump ( TP_EBO, NULL );
    }

    // Optimize control flow after EBO and if-conversion (in EBO)
    // to allow merging blocks to maximize single BB loops
    if (CFLOW_opt_before_cgprep) {
      // Perform all the optimizations that make things more simple.
      // Reordering doesn't have that property.
      CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_IN_CGPREP)
		   & ~(CFLOW_FREQ_ORDER | CFLOW_REORDER),
		   "CFLOW (first pass after EBO)");
    }

    if (CG_enable_loop_optimizations) {
      Set_Error_Phase("CGPREP");
      Start_Timer(T_Prep_CU);
      CGPREP_Process_Region (region ? REGION_get_rid(rwn) : NULL);
      Stop_Timer(T_Prep_CU);
      Check_for_Dump(TP_CGPREP, NULL);
    }

    /* Optimize control flow (second pass) */
    if (CFLOW_opt_after_cgprep) {
      CFLOW_Optimize(CFLOW_ALL_OPTS, "CFLOW (second pass)");
    }

    if (Enable_CG_Peephole) {
      Set_Error_Phase( "Extended Block Optimizer (second)");
      Start_Timer( T_EBO_CU );
      EBO_ACGPREP_Process_Region (region ? REGION_get_rid(rwn) : NULL);
      PQSCG_reinit(REGION_First_BB);
      Stop_Timer ( T_EBO_CU );
      Check_for_Dump ( TP_EBO, NULL );
    }
  } else {
  }

  if (!Get_Trace (TP_CGEXP, 1024))
	Reuse_Temp_TNs = TRUE;	/* for spills */

  if (CGSPILL_Enable_Force_Rematerialization)
    CGSPILL_Force_Rematerialization();

  if (!region) {
    /* in case cgprep introduced a gp reference */
    Adjust_GP_Setup_Code( Get_Current_PU_ST(), FALSE /* allocate registers */ );
    /* in case cgprep introduced a lc reference */
    Adjust_LC_Setup_Code();

    // TODO:  when generate control speculation (ld.s) and st8.spill
    // of NaT bits, then need to save and restore ar.unat. 
  }

  /* Global register allocation, Scheduling:
   *
   * The overall algorithm is as follows:
   *   - Global code motion before register allocation
   *   - Local scheduling before register allocation
   *   - Global register allocation
   *   - Local register allocation
   *   - Global code motion phase (GCM) 
   *   - Local scheduling after register allocation
   */

  IGLS_Schedule_Region (TRUE /* before register allocation */);

  if (!CG_localize_tns)
  {
    // Earlier phases (esp. GCM) might have introduced local definitions
    // and uses for global TNs. Rename them to local TNs so that GRA 
    // does not have to deal with them.

    if (GRA_recalc_liveness) {
      Start_Timer( T_GLRA_CU);
      GRA_LIVE_Recalc_Liveness(region ? REGION_get_rid( rwn) : NULL);
      Stop_Timer ( T_GLRA_CU );
    } else {
      GRA_LIVE_Rename_TNs ();
    }

    if (GRA_redo_liveness) {
      Start_Timer( T_GLRA_CU );
      GRA_LIVE_Init(region ? REGION_get_rid( rwn ) : NULL);
      Stop_Timer ( T_GLRA_CU );
    }

    Check_for_Dump ( TP_FIND_GLOB, NULL );
    GRA_Allocate_Global_Registers( region );
    Check_for_Dump ( TP_FIND_GLOB, NULL );
  }

  Set_Error_Phase ( "Convert to predicted branches" );
  if (CG_opt_level > 0 && xt_brt) {
    for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb)) {
      CG_Convert_To_Predicted_Branch(bb);
    }
  }

  LRA_Allocate_Registers (!region);

#ifdef TARG_XTENSA
  if (Run_Autotie) {
    /* Register allocation may have added spill code to the autotie regions
       and we try to account for it by adding the spill ops to the correct
       AT_STATs if we can find them.
       This has to be done before the grant memory is popped in
       GRA_Finalize_Grants().
    */
    AUTOTIE_Analyze_Spills();

    /* After all basic block layout, record the taken branch
       probabilities. */
    AUTOTIE_Analyze_Taken_Probs();
  }
#endif

  if (!CG_localize_tns ) {
    Set_Error_Phase ( "GRA_Finish" );
    /* Done with all grant information */
    GRA_Finalize_Grants();
  }

#ifdef TARG_XTENSA
  if (Run_Autotie) {
    AUTOTIE_Convert_DFGs();
    
    /* If running autotie analysis, we don't need to go further. We do
       need to finalize a few things though... */
    Finalize_Stack_Frame();
    ISEL_finalize();
    GRA_LIVE_Finish_PU();
    return rwn;
  }
#endif
    
  // remove zcl guard branch which can only be removed now after
  // GRA preferencing

  Set_Error_Phase ( "Remove Guard Branch After GRA" );
  for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
  {
    OP* loop_op = BB_branch_op(bb);
    if (loop_op && OP_loop_start(loop_op)) {
      Remove_Guard_Branch(bb);
    }
  }

  // Optimize control flow (third pass)
  if (CG_opt_level > 0 && CFLOW_opt_after_lra) {
    // Perform all the optimizations that make things more simple.
    // Reordering doesn't have that property.
    CFLOW_Optimize(  (CFLOW_ALL_OPTS|CFLOW_AFTER_LRA)
		   & ~(CFLOW_FREQ_ORDER | CFLOW_REORDER),
		   "CFLOW (third pass)");
  }

  /* At this point, all branch register number has been determined so
     we can expand the simulated operations that move/load/store
     branch registers here to take advantage of the optimizations
     performed later
  */
  Set_Error_Phase ( "Expand Branch Register Simulated OPs" );
  for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
  {
    for (OP *next, *op = BB_first_op(bb); op != NULL; op = next)
    {
      next = OP_next(op);

      if (OP_simulated(op))
      {
	BOOL expanded;
	OPS* ops = OPS_Create();
	expanded = Exp_Br_Reg_Simulated_Op(op,ops);
	if (expanded) {
	  BB_Insert_Ops_After(bb, op, ops);
	  BB_Remove_Op(bb,op);
	}
      }
    }
  }

  if (!region) {
    /* Check that we didn't introduce a new gp reference */
    Adjust_GP_Setup_Code( Get_Current_PU_ST(), TRUE /* allocate registers */ );

    /* The stack frame is final at this point, no more spilling after this.
     * We can set the Frame_Len now.
     * Then we can go through all the entry/exit blocks and fix the SP 
     * adjustment OP or delete it if the frame length is zero.
     */
    Set_Frame_Len (Finalize_Stack_Frame());
    Set_Error_Phase ( "Final SP adjustment" );
    Adjust_Entry_Exit_Code ( Get_Current_PU_ST() );
  }

  if (Enable_CG_Peephole) {
    Set_Error_Phase("Extended Block Optimizer (Post_Process)");
    Start_Timer(T_EBO_CU);
    EBO_Post_Process_Region (region ? REGION_get_rid(rwn) : NULL);
    Stop_Timer ( T_EBO_CU );
    Check_for_Dump ( TP_EBO, NULL );
  }

  if (CG_opt_level > 0 && (CFLOW_Enable_Layout > 0)) {
    /* Layout does not handle generic branches well, so let's do layout
       before relaxing special OPs.
     */
    CFLOW_Layout();
  }

#ifdef TARG_XTENSA
  ISEL_finalize();
#endif

  if (CG_opt_level > 1 && CG_specialization) {
    Set_Error_Phase("Relax Special Ops");
    for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
      if (BB_rotating_kernel(bb))
        continue;

      OP* op;
      FOR_ALL_BB_OPs(bb,op) {
        if (!OP_is_no_generic(op)) {
          CG_Relax_Special_Op(op);
        }
      }
    }
  }

  /* At this point, expand TOP_load_const, so the IGLS can bundle them
     together.
  */
  Set_Error_Phase ( "Expand load_const" );
  for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
  {
    for (OP *next, *op = BB_first_op(bb); op != NULL; op = next)
    {
      next = OP_next(op);
      if (OP_simulated(op))
      {
	BOOL expanded;
	OPS* ops = OPS_Create();

	expanded = Exp_load_const(op, ops);
	if (expanded) {
	  BB_Insert_Ops_After(bb, op, ops);
	  BB_Remove_Op(bb,op);
	}
      }
    }
  }

  IGLS_Schedule_Region (FALSE /* after register allocation */);

  if (CG_opt_level > 1 && CG_specialization) {
    Set_Error_Phase("Specialize Generic Ops");
    for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
    {
      if (BB_rotating_kernel(bb))
        continue;

      OP* op;
      FOR_ALL_BB_OPs(bb,op)
      {
        CG_Specialize_Op(op);
      }
    }
  }

  Reuse_Temp_TNs = orig_reuse_temp_tns;		/* restore */

  if (region) {
    /*--------------------------------------------------------------------*/
    /* old region: rwn, rid_orig					  */
    /* new region: rwn_new, rid_new (encloses old region)		  */
    /*--------------------------------------------------------------------*/
    WN	*inner_body, *outer_body, *exitBlock, *comment;
    WN  *rwn_new, *result_block_before, *result_block_after;
    RID *rid_orig;
    char str[100];

    Is_True(REGION_consistency_check(rwn),("CG_Generate_Code"));
    rid_orig = REGION_get_rid(rwn);

    /* don't delete rwn, it contains the stub that helps find the MOPS
       that the region has been lowered to */

    outer_body = WN_CreateBlock();
    /* put inner region inside outer containment block */
    WN_INSERT_BlockFirst(outer_body, rwn);
    /* we assembled the new exit block earlier in Build_CFG()		*/
    exitBlock = CGRIN_nested_exit(RID_cginfo(rid_orig));
    WN_region_exits(rwn) = exitBlock; /* PPP ??? */

    rwn_new = outer_body;

    /* put a note in the inner body that the code isn't there anymore */
    inner_body = WN_CreateBlock();
    WN_region_body(rwn) = inner_body; /* overwrite old body, now in MOPs */
    sprintf(str,"RGN %d has been lowered to MOPs, level=%s",
	    RID_id(rid_orig), RID_level_str(rid_orig));
    comment = WN_CreateComment(str);
    WN_INSERT_BlockFirst(inner_body, comment);

    /* Need to split result block for glue code into two parts: before and
       after the region body. The reason we can't just insert the glue code
       directly before or after the region directly is that we need to keep
       it separate for updating the alias info.
       If CG_LOOP has made some WHIRL glue, it is inserted in result_block. */
    result_block_before = WN_CreateBlock();
    result_block_after = WN_CreateBlock();

    /* fill-in blocks with glue code */
    Set_Error_Phase("Region Finalize");
    Start_Timer(T_Region_Finalize_CU);
    CG_Region_Finalize( result_block_before, result_block_after,
		       rwn, alias_mgr, TRUE /* generate_glue_code */ );
    Stop_Timer(T_Region_Finalize_CU);

    /* generate alias information for glue code */
    REGION_update_alias_info(result_block_before, alias_mgr);
    REGION_update_alias_info(result_block_after, alias_mgr);

    /* insert glue code before and after */
    WN_INSERT_BlockFirst( rwn_new, result_block_before );
    WN_INSERT_BlockLast( rwn_new, result_block_after );

    GRA_LIVE_Finish_REGION();
    PQSCG_term();

    Stop_Timer ( T_CodeGen_CU );
    Set_Error_Phase ( "Codegen Driver" );

    return rwn_new;
  } /* if (region */

  else { /* PU */
    Set_Error_Phase ( "EH_Finalization" );
    if (Target_ABI == ABI_CALL0 && CXX_Exceptions_On) {
      /* Add EH to restore callee saved registers. It is vitally important
	 that this happens after all other code motion, register allocation
	 optimization and everything. */
      EH_Build_Restore_Callee_EH_Region(rwn);
    }
    /* Write the EH range table. */
    if (EH_Need_EH_Table()) {
      EH_Write_Range_Table(rwn);
    }

    Set_Error_Phase ( "Assembly" );
    Start_Timer (	T_Emit_CU );
    EMT_Emit_PU (Get_Current_PU_ST(), pu_dst, rwn);
    Check_for_Dump (TP_EMIT, NULL);
    Stop_Timer ( T_Emit_CU );

    Set_Error_Phase("Region Finalize");
    Start_Timer(T_Region_Finalize_CU);
    CG_Region_Finalize( NULL, NULL, rwn, alias_mgr,
		       FALSE /* generate_glue_code */ );
    Stop_Timer(T_Region_Finalize_CU);

    GRA_LIVE_Finish_PU();
    PQSCG_term();

    /* List local symbols if desired: */
    if ( List_Symbols )
	Print_symtab (Lst_File, CURRENT_SYMTAB);

    Stop_Timer ( T_CodeGen_CU );
    Set_Error_Phase ( "Codegen Driver" );

    return rwn;
  }
}



/* ================================================================= */
/* routines for dumping/tracing the program */

void
Trace_IR(
  INT phase,		/* Phase after which we're printing */
  const char *pname,	/* Print name for phase	*/
  BB *cur_bb)		/* BB to limit traces to */
{
  INT cur_bb_id = cur_bb ? BB_id(cur_bb) : 0;
  if ( ( Get_Trace(TKIND_IR, phase) || Get_Trace(TKIND_IR, TP_CGALL) )
      && (cur_bb_id == 0 || Get_BB_Trace(cur_bb_id)))
  {
    fprintf(TFile, "\n%s%s\tIR after %s\n%s%s\n",
	    DBar, DBar, pname, DBar, DBar);
    if (cur_bb != NULL) {
      Print_BB(cur_bb);
    } else {
      BB *bb;
      for (bb = REGION_First_BB; bb; bb = BB_next(bb))	{
	if (Get_BB_Trace(BB_id(bb)) &&
	    ( Get_Trace(TKIND_IR, phase) || Get_Trace(TKIND_IR, TP_CGALL) )) {
	  Print_BB(bb);
	}
      }
    }
    fprintf(TFile, "%s%s\n", DBar, DBar);
  }
}

static void
Trace_TN (
  INT phase,		/* Phase after which we're printing */
  const char *pname )	/* Print name for phase	*/
{
  if ( Get_Trace ( TKIND_TN, phase ) ) {
    fprintf ( TFile, "\n%s%s\tTNs after %s\n%s%s\n",
	      DBar, DBar, pname, DBar, DBar );
    Print_TNs ();
  }
}

static void
Trace_ST (
  INT phase,		/* Phase after which we're printing */
  const char *pname )	/* Print name for phase	*/
{
  if ( Get_Trace ( TKIND_SYMTAB, phase ) || Get_Trace(TKIND_SYMTAB, TP_CGALL) ) {
  	fprintf ( TFile, "\n%s%s\tSymbol table after %s\n%s%s\n",
              DBar, DBar, pname, DBar, DBar );
  	SYMTAB_IDX level = CURRENT_SYMTAB;
	while (level >= GLOBAL_SYMTAB) {
	  	Print_symtab (TFile, level);
		--level;
	}
  }
}

/* ====================================================================
 *
 * Check_for_Dump
 *
 * Check whether symbol table, TN, or IR dumps have been requested for
 * the given pass; if so, generate them to the trace file.  If a BB is
 * given, limit the dumps to that BB.
 *
 * ====================================================================
 */
void
Check_for_Dump ( INT32 pass, BB *bb )
{
  if (bb == NULL || Get_BB_Trace(BB_id(bb))) {
    const char *s = Get_Error_Phase();

    /* Check to see if we should dump the STAB.
     */
    Trace_ST ( pass, s );

    /* Check to see if we should dump the TNs.
     */
    Trace_TN ( pass, s );

    /* Check to see if we should dump the IR.  If yes, check each BB.
     */
    Trace_IR ( pass, s, bb );

#ifdef MEM_STATS
    /* Check to see if we should give a memory allocation trace.
     */
    Trace_Memory_Allocation ( pass, s, Get_Current_PU_ST() );
#endif
  }
}

void
CG_Dump_Region(FILE *fd, WN *wn)
{
  RID	*rid = REGION_get_rid(wn);
  Is_True(rid != NULL, ("CG_Dump_Region, NULL RID"));
  if (rid && RID_level(rid) >= RL_CGSCHED) {
    CGRIN  *cgrin = RID_cginfo(rid);
    if (cgrin && CGRIN_entry(cgrin)) {
      BB *bb;
      for (bb=CGRIN_entry(cgrin); bb; bb=BB_next(bb))
	Print_BB( bb );
    }
  }
}


/* just an externally-visible wrapper to cgemit function */
extern void
CG_Change_Elf_Symbol_To_Undefined (ST *st)
{
	EMT_Change_Symbol_To_Undefined(st);
}
