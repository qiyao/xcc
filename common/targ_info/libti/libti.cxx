
/*

  Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.

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

*/

/*
 * Core functions for the mongoose target-info library.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <limits.h>
#include "mempool.h"
#include "cxx_memory.h"
#include "xtmap.h"
#include "errors.h"
#include "libti.h"
#include "tie.h"
#include "topcode.h"
#include "targ_abi_properties.h"
#include "targ_isa_registers.h"
#include "targ_isa_bundle.h"
#include "targ_isa_properties.h"
#include "targ_isa_subset.h"
#include "targ_isa_operands.h"
#include "targ_isa_hazards.h"
#include "targ_isa_pack.h"
#include "targ_isa_print.h"
#include "targ_proc.h"
#include "targ_proc_properties.h"
#include "xtarch.h"
#include "xtmicroarch.h"
#include "xtarch_interface.h"
#include "ti_res_res.h"
#include "ti_res.h"
#include "config_targ.h"

static void TI_TOP_Init (void);
static void TI_LC_Init_For_Derived (void);
static xtensa_isa libisa_info = NULL;
static xtensa_tie xtie_info = NULL;
static xtie_phase xtie_post_rewrite = NULL;
static xtie_phase xtie_compiler = NULL;

/* Interface to scheduling information. */
extern DLL_SHARED INT TSI_resource_count;
extern DLL_SHARED TI_SI_RESOURCE *TSI_resources[];
extern DLL_SHARED TI_SI_RRW TSI_RRW_initializer;
extern DLL_SHARED TI_SI_RRW TSI_RRW_overuse_mask;
extern DLL_SHARED TI_SI_RRW TSI_RRW_format_resource_overuse_mask[];
extern DLL_SHARED INT TSI_issue_slot_count;
extern DLL_SHARED TI_SI_ISSUE_SLOT *TSI_issue_slots[];
extern DLL_SHARED TI_SI *TSI_top_si[];
extern DLL_SHARED INT TSI_ID_count;
extern DLL_SHARED TI_SI *TSI_ID_si[];

extern DLL_SHARED BOOL xt_mac16;
extern DLL_SHARED BOOL xt_mul16;
extern DLL_SHARED BOOL xt_mul32;
extern DLL_SHARED BOOL xt_mul32h;
extern DLL_SHARED BOOL xt_div32;
extern DLL_SHARED BOOL xt_hifi2;

INT TI_SI_resource_count;
TI_SI_RESOURCE **TI_SI_resources;
TI_SI_RRW* TI_SI_RRW_initializer;
TI_SI_RRW* TI_SI_RRW_overuse_mask;
TI_SI_RRW* TI_SI_RRW_format_resource_overuse_mask;
TI_SI_RRW* TI_SI_RRW_all_format_resource_overuse_mask;
TI_SI_RESOURCE_ID_SET* TI_SI_resource_id_set_universe;
TI_SI_RESOURCE_ID_SET* TI_SI_resource_id_set_empty;
INT TI_SI_issue_slot_count;
TI_SI_ISSUE_SLOT **TI_SI_issue_slots;
TI_SI **TI_SI_top_si;
INT TI_SI_ID_count;
TI_SI **TI_SI_ID_si;

/* Memory pool for libti use. */
static MEM_POOL libti_pool;

/* Object representing tie architecture (regfiles and instructions). */
static XT_Architecture_p xtarch = NULL;

/* Object representing tie scheduling information. */
static XTM_MicroArchitecture_p xtmicroarch = NULL;

/* For user-defined types, a map from mtype to the regclass that holds
   that type. */
ISA_REGCLASS *mtype_to_regclass = NULL;
INT *mtype_to_regsize = NULL;

/* For user-defined types, a map from the regclass to mtype that bases
   on that type. */

TYPE_ID * regclass_to_mtype = NULL;

/* =============================================================

   Initialization

   =========================================================== */

extern DLL_SHARED TARGET_ABI Target_ABI;

extern "C" BOOL
Target_Information_Init (const char * dll,
			 char **libisa_dlls,
			 char **libtie_dlls)
{
  static BOOL init = FALSE;

  /* Initialize the target information if necessary. */
  if (!init)
  {
    MEM_POOL_Initialize(&libti_pool, "LIBTI", TRUE);
    MEM_POOL_Push(&libti_pool); // first push needed

    ISA_SUBSET_Value = ISA_SUBSET_xtensa;
    PROCESSOR_Value = PROCESSOR_xtensa;
    ABI_PROPERTIES_ABI_Value = Target_ABI == ABI_WINDOWED ? 
        ABI_PROPERTIES_ABI_windowed : ABI_PROPERTIES_ABI_call0;

    TI_TOP_Init();
    ABI_PROPERTIES_Initialize();
    ISA_HAZARD_Initialize();
    ISA_REGISTER_Initialize();

    /* Grab build-in scheduling structures. */
    TI_SI_resource_count = TSI_resource_count;
    TI_SI_resources = TSI_resources;
    TI_SI_issue_slot_count = TSI_issue_slot_count;
    TI_SI_issue_slots = TSI_issue_slots;
    TI_SI_top_si = TSI_top_si;
    TI_SI_ID_count = TSI_ID_count;
    TI_SI_ID_si = TSI_ID_si;

    /* Create an XT_Architecture and an XT_MicroArchitecture object to
       hold information about the tie regfiles and instructions. */
    xtarch = TI_Architecture_Init(&libti_pool, dll, libisa_dlls, libtie_dlls);
    
    if (xtarch && xtarch->initialized()) {
      xtmicroarch = CXX_NEW(XTM_MicroArchitecture(&libti_pool, xtarch), &libti_pool);
      libisa_info = xtarch->isa();
      xtie_info = xtarch->xtie();
      xtie_post_rewrite = xtarch->xtie_post_rewrite();
      xtie_compiler = xtarch->xtie_compiler();
    }
    
    if (!xtarch || !xtmicroarch ||
        !xtarch->initialized() || !xtmicroarch->initialized())
    {
      /* We don't have architectural extensions, or something went
         wrong with loading them. Continue without them, using the
         built-in architecture. */
      xtarch = NULL;
      xtmicroarch = NULL;
    }
    else
    {
      /* Override the built-in scheduling data structures. */
      TI_SI_resource_count = xtmicroarch->resource_count();
      TI_SI_resources = xtmicroarch->resources();
      TI_SI_RRW_initializer=xtmicroarch->RRW_initializer();
      TI_SI_RRW_overuse_mask=xtmicroarch->RRW_overuse_mask();
      TI_SI_RRW_format_resource_overuse_mask =
        xtmicroarch->RRW_format_resource_overuse_mask();
      
      TI_SI_RRW tmp_rrw;
      tmp_rrw.reset();
      for (int i=0; i< xtarch->num_bundles(); i++)
        tmp_rrw |= TI_SI_RRW_format_resource_overuse_mask[i];
      
      TI_SI_RRW_all_format_resource_overuse_mask =
        CXX_NEW(TI_SI_RRW, &libti_pool);
      TI_SI_RRW_all_format_resource_overuse_mask->copy(tmp_rrw);
      
      TI_SI_issue_slot_count = xtmicroarch->issue_slot_count();
      TI_SI_issue_slots = xtmicroarch->issue_slots();
      TI_SI_top_si = xtmicroarch->top_si();
      TI_SI_ID_count = xtmicroarch->ID_count();
      TI_SI_ID_si = xtmicroarch->ID_si();
    }

    /* turn off optional TIE since they depends on xtarch */
    if (xtarch==NULL) {
      xt_mac16 = FALSE;
      xt_mul16 = FALSE;
      xt_mul32 = FALSE;
      xt_mul32h = FALSE;
      xt_div32 = FALSE;
      xt_hifi2 = FALSE;
      TI_SI_RRW::set_length(1);
      TI_SI_RESOURCE_ID_SET::set_length(1);
      TI_SI_RRW_initializer = &TSI_RRW_initializer;
      TI_SI_RRW_overuse_mask = &TSI_RRW_overuse_mask;
      TI_SI_RRW_format_resource_overuse_mask = CXX_NEW_ARRAY(TI_SI_RRW, 1, &libti_pool);
      TI_SI_RRW_format_resource_overuse_mask[0].init(
			TSI_RRW_format_resource_overuse_mask[0], &libti_pool);
      TI_SI_RRW_all_format_resource_overuse_mask =
			CXX_NEW(TI_SI_RRW, &libti_pool);
      TI_SI_RRW_all_format_resource_overuse_mask->reset();
    }

    TI_SI_resource_id_set_universe = CXX_NEW(TI_SI_RESOURCE_ID_SET, &libti_pool);
    for (int i=0; i<TI_SI_resource_count; i++)
      TI_SI_resource_id_set_universe->add_bit_mask(1, i);

    TI_SI_resource_id_set_empty = CXX_NEW(TI_SI_RESOURCE_ID_SET, &libti_pool);
    TI_SI_resource_id_set_empty->reset();

    /* All "original" litclasses have been created; initialize to
       allow derived litclasses to be created. */
    TI_LC_Init_For_Derived();

    /* It's time to intialize wide branch which is dependent upon 
       derived litclasses.
     */
    if (xtarch != NULL) {
      xtarch->init_wbranches();
    }

    /* Initialize the tie_info object to maintain tie related
       information. */
    tie_info = CXX_NEW(TIE_INFO, &libti_pool);

    /* Make sure the target has not exceeded any hardwired maximums. */

    if (TI_ISA_Regclass_Last() > TI_ISA_REGCLASS_MAX)
    {
      DevWarn("Maximum number of register classes exceeded.\n"
	       "Maximum = %d, this target = %d",
	       TI_ISA_REGCLASS_MAX, TI_ISA_Regclass_Last());
      return FALSE;
    }
    else if (TI_ISA_Num_Regsubclasses() > TI_ISA_REGSUBCLASS_MAX)
    {
      DevWarn("Maximum number of register subclasses exceeded.\n"
	       "Maximum = %d, this target = %d",
	       TI_ISA_REGSUBCLASS_MAX, TI_ISA_Num_Regsubclasses());
      return FALSE;
    }
    else if (TI_ISA_Register_Max() > TI_ISA_REGISTER_MAX)
    {
      DevWarn("Maximum number of register in any class exceeded.\n"
	       "Maximum = %d, this target = %d",
	       TI_ISA_REGISTER_MAX, TI_ISA_Register_Max());
      return FALSE;
    }
    else if ((8*sizeof(ISA_EXEC_UNIT_PROPERTY)) < TI_ISA_Exec_Num_Units())
    {
      DevWarn("Maximum number of execution units exceeded.\n"
	       "Maximum = %d, this target = %d",
	       8*sizeof(ISA_EXEC_UNIT_PROPERTY), TI_ISA_Exec_Num_Units());
      return FALSE;
    }

    /* make sure enum types are large enough to hold possible TIE values */
    if (sizeof(mISA_REGCLASS) > sizeof(ISA_REGCLASS))
    {
      DevWarn("enum ISA_REGCLASS size too small");
      return FALSE;
    } else if (sizeof(mISA_REGSUBCLASS) > sizeof(ISA_REGSUBCLASS))
    {
      DevWarn("enum ISA_REGSUBCLASS size too small");
      return FALSE;
    }

    /* Expensive checks... */
#ifdef Is_True_On
    for (UINT i = 0; i < TI_TOP_Count(); i++)
    {
      TOP top = (TOP)i;

      /* Make sure base_updating tops have a base operand, and have
         PROP_same_res. */
      if (TI_ISA_Property_Set(PROP_base_update, top))
      {
	INT base_idx = TI_TOP_Find_Operand_Use(top, OU_base);
	Is_True(TI_ISA_Property_Set(PROP_same_res, top) &&
		(base_idx != -1),
		("base_update top %s incorrect\n", TI_TOP_Name(top)));
      }
    }

    if (xtmicroarch) xtmicroarch->debugGenerated();
#endif

    init = TRUE;
  }

  return TRUE;
}


/* =============================================================

   Topcodes

   =========================================================== */

typedef UTL_Map<const char *, TOP, UTL_Map_StringNoCaseHash> TOP_NAME_MAP;
static TOP_NAME_MAP *top_name_map;
static TOP *branch_invert_table;
static TOP *same_branch_flipped_operands_table;
static TOP *immed_to_reg_table;
static TOP *reg_to_immed_table;


static void
TI_TOP_Init (void)
{
  top_name_map = CXX_NEW(TOP_NAME_MAP(&libti_pool), &libti_pool);

  /* Initialize the map from name to topcode with the core opcodes. */

  for (UINT i = 0; i < TOP_count; i++)
  {
    top_name_map->insert(TOP_Name((TOP)i), (TOP)i);
  }

  /* Initialize tables of TOP relationships/mappings. */

  const UINT tcnt = TOP_count;

  branch_invert_table = TYPE_MEM_POOL_ALLOC_N(TOP, &libti_pool, tcnt+1);
  same_branch_flipped_operands_table = TYPE_MEM_POOL_ALLOC_N(TOP, &libti_pool, tcnt+1);
  immed_to_reg_table = TYPE_MEM_POOL_ALLOC_N(TOP, &libti_pool, tcnt+1);
  reg_to_immed_table = TYPE_MEM_POOL_ALLOC_N(TOP, &libti_pool, tcnt+1);

  for (UINT i = 0; i <= tcnt; ++i)
    {
      branch_invert_table[i] = TOP_UNDEFINED;
      same_branch_flipped_operands_table[i] = TOP_UNDEFINED;
      immed_to_reg_table[i] = TOP_UNDEFINED;
      reg_to_immed_table[i] = TOP_UNDEFINED;
    }

  /* Init table for invert_branch */

  branch_invert_table[TOP_beqz]     = TOP_bnez;
  branch_invert_table[TOP_beqz_n]   = TOP_bnez_n;
  branch_invert_table[TOP_beqzt]    = TOP_bnez;
  branch_invert_table[TOP_bnez]     = TOP_beqz;
  branch_invert_table[TOP_bnez_n]   = TOP_beqz_n;
  branch_invert_table[TOP_bnezt]    = TOP_beqz;
  branch_invert_table[TOP_bgez]     = TOP_bltz;
  branch_invert_table[TOP_bltz]     = TOP_bgez;
  branch_invert_table[TOP_beqi]     = TOP_bnei;
  branch_invert_table[TOP_bnei]     = TOP_beqi;
  branch_invert_table[TOP_bgei]     = TOP_blti;
  branch_invert_table[TOP_blti]     = TOP_bgei;
  branch_invert_table[TOP_bgeui]    = TOP_bltui;
  branch_invert_table[TOP_bltui]    = TOP_bgeui;
  branch_invert_table[TOP_bbci]     = TOP_bbsi;
  branch_invert_table[TOP_bbsi]     = TOP_bbci;
  branch_invert_table[TOP_beq]      = TOP_bne;
  branch_invert_table[TOP_beqt]     = TOP_bne;
  branch_invert_table[TOP_bne]      = TOP_beq;
  branch_invert_table[TOP_bnet]     = TOP_beq;
  branch_invert_table[TOP_bge]      = TOP_blt;
  branch_invert_table[TOP_blt]      = TOP_bge;
  branch_invert_table[TOP_bgeu]     = TOP_bltu;
  branch_invert_table[TOP_bltu]     = TOP_bgeu;
  branch_invert_table[TOP_bany]     = TOP_bnone;
  branch_invert_table[TOP_bnone]    = TOP_bany;
  branch_invert_table[TOP_ball]     = TOP_bnall;
  branch_invert_table[TOP_bnall]    = TOP_ball;
  branch_invert_table[TOP_bbc]      = TOP_bbs;
  branch_invert_table[TOP_bbs]      = TOP_bbc;
  branch_invert_table[TOP_bt]       = TOP_bf;
  branch_invert_table[TOP_bf]       = TOP_bt;

  /* Init table for same_branch_flipped_operands */

  same_branch_flipped_operands_table[TOP_beq]      = TOP_beq;
  same_branch_flipped_operands_table[TOP_bne]      = TOP_bne;

  /* Init table for immed_to_reg */

  immed_to_reg_table[TOP_beqi]   = TOP_beq;
  immed_to_reg_table[TOP_bnei]   = TOP_bne;
  immed_to_reg_table[TOP_bgei]   = TOP_bge;
  immed_to_reg_table[TOP_blti]   = TOP_blt;
  immed_to_reg_table[TOP_bgeui]  = TOP_bgeu;
  immed_to_reg_table[TOP_bltui]  = TOP_bltu;
  immed_to_reg_table[TOP_bbci]   = TOP_bbc;
  immed_to_reg_table[TOP_bbsi]   = TOP_bbs;

  immed_to_reg_table[TOP_addi]   = TOP_add;

  /* Init table for reg_to_immed */

  reg_to_immed_table[TOP_beq]   = TOP_beqi;
  reg_to_immed_table[TOP_bne]   = TOP_bnei;
  reg_to_immed_table[TOP_bge]   = TOP_bgei;
  reg_to_immed_table[TOP_blt]   = TOP_blti;
  reg_to_immed_table[TOP_bgeu]  = TOP_bgeui;
  reg_to_immed_table[TOP_bltu]  = TOP_bltui;
  reg_to_immed_table[TOP_bbc]   = TOP_bbci;
  reg_to_immed_table[TOP_bbs]   = TOP_bbsi;

  reg_to_immed_table[TOP_add]   = TOP_addi;
}


INT
TI_TOP_Count (void)
{
  return TOP_count + (xtarch ? xtarch->num_instructions() : 0);
}


void
TI_TOP_Topcode_Remove_From_Core (const char *opcode)
{
  /* First look in the core opcodes, and then try tie. */

  TOP top;
  if (top_name_map->find(opcode, &top)){
    top_name_map->remove(opcode);
  }
}

TOP
TI_TOP_Topcode (const char *opcode)
{
  /* First look in the core opcodes, and then try tie. */

  TOP top;
  if (top_name_map->find(opcode, &top))
    return top;

  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(opcode);
    if (inst)
      return inst->topcode();
  }

  return TOP_UNDEFINED;
}


const char *
TI_TOP_Name (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return TOP_Name(topcode);

  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    if (inst)
      return inst->name();
  }

  FmtAssert(FALSE, ("Topcode %d has no name\n", topcode));
  return NULL;
}

UINT
TI_TOP_Min_Issue_Slot_Count (TOP topcode)
{
  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    if (inst)
      return inst->min_slot_count();
  }

  return TI_ISA_Max_Num_Slots();
}

extern DLL_SHARED UINT32 xt_w_stage; // in config_targ_options.h

UINT
TI_TOP_Latest_Def_Stage (TOP topcode)
{
  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    if (inst)
      return inst->latest_def_stage();
  }

  return xt_w_stage+2;
}

TOP
TI_TOP_Invert_Branch (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));
  return((topcode < TOP_count) ?
	 branch_invert_table[(UINT)topcode] : TOP_UNDEFINED);
}


TOP
TI_TOP_Same_Branch_Flipped_Operands (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));
  return((topcode < TOP_count) ?
	 same_branch_flipped_operands_table[(UINT)topcode] : TOP_UNDEFINED);
}


TOP
TI_TOP_Immed_To_Reg (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));
  if ((topcode < TOP_count)) {
    TOP ret_val =  immed_to_reg_table[(UINT)topcode];
    if (ret_val != TOP_UNDEFINED)
      return ret_val;
  }

  if (tie_info) {
    const char* top_name = TI_TOP_Name(topcode);
    const char* mangled_top_name = tie_info->mangle_macro_name(top_name);
    TIE_MACRO_p m = tie_info->tie_macro(mangled_top_name);
    if (m) {
      TIE_MACRO_p m1 = tie_info->immediate_to_register_form(m);
      if (m1) {
	TOP ret_val = TI_TOP_Topcode(m1->demangled_name());
	return ret_val;
      }
    }
  }

  return TOP_UNDEFINED;
}


TOP
TI_TOP_Reg_To_Immed (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));
  if ((topcode < TOP_count)) {
    TOP ret_val =  reg_to_immed_table[(UINT)topcode];
    if (ret_val != TOP_UNDEFINED)
      return ret_val;
  }

  if (tie_info) {
    const char* top_name = TI_TOP_Name(topcode);
    const char* mangled_top_name = tie_info->mangle_macro_name(top_name);
    TIE_MACRO_p m = tie_info->tie_macro(mangled_top_name);
    if (m) {
      TIE_MACRO_p m1 = tie_info->register_to_immediate_form(m);
      if (m1) {
	TOP ret_val = TI_TOP_Topcode(m1->demangled_name());
	return ret_val;
      }
    }
  }

  return TOP_UNDEFINED;
}


TOP
TI_TOP_Nonupdate_To_Update (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return TOP_UNDEFINED;

  if (xtarch)
    return xtarch->updating_version(topcode);

  return TOP_UNDEFINED;
}

TOP
TI_TOP_Update_To_Nonupdate (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return TOP_UNDEFINED;

  if (xtarch)
    return xtarch->non_updating_version(topcode);

  return TOP_UNDEFINED;
}


void
TI_TOP_Set_Nonupdate_To_Update (TOP non_update, TOP update)
{
  Is_True(non_update < TI_TOP_Count(), ("unexpected topcode %d\n", non_update));
  Is_True(update < TI_TOP_Count(), ("unexpected topcode %d\n", update));

  if (xtarch)
    xtarch->set_updating_version(non_update, update);
}


TOP
TI_TOP_Nonindex_To_Index (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return TOP_UNDEFINED;

  if (xtarch)
    return xtarch->indexed_version(topcode);

  return TOP_UNDEFINED;
}

INT
TI_TOP_Issue_Alignment(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return 1;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->issue_alignment();
  }

  return 1;
}

bool
TI_TOP_Stream_Get(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return false;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->is_stream_get();
  }

  return false;
}

bool
TI_TOP_Stream_Put(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return false;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->is_stream_put();
  }

  return false;
}

bool
TI_TOP_Use_Shared_Functional_Unit(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return false;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->use_shared_functional_unit();
  }

  return false;
}

bool
TI_TOP_Has_Export_State(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return false;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->has_export_state();
  }

  return false;
}

bool
TI_TOP_Has_Tie_Queue(TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return false;

  if (xtarch) {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    return inst->has_tie_queue();
  }

  return false;
}

TOP
TI_ISA_Regclass_Def_Topcode (ISA_REGCLASS rc)
{
  XT_RegFile_p rf = xtarch->find_regfile(rc);
  if (rf)
    return rf->def_topcode();
  else
    return TOP_UNDEFINED;
}

BOOL
TI_ISA_Regclass_Has_Split_Pipe (ISA_REGCLASS rc)
{
  if (rc == ISA_REGCLASS_UNDEFINED)
    return false;

  XT_RegFile_p rf = xtarch->find_regfile(rc);
  if (rf)
    return rf->has_split_pipe();
  else
    return false;
}

extern UINT
TI_TOP_Get_Special_Tops (TOP generic_top, TOP return_tops[], UINT size)
{
  if (xtarch)
    return xtarch->get_special_tops(generic_top, return_tops, size);

  return 0;
}

extern UINT
TI_TOP_Get_Compatible_Tops (TOP top, TOP return_tops[], UINT size)
{
  if (xtarch)
    return xtarch->get_compatible_tops(top, return_tops, size);

  return 0;
}

extern TOP
TI_TOP_Get_Generic_Top (const TOP tops_array[], int num_tops)
{
  if (xtarch)
    return xtarch->get_generic_top(tops_array, num_tops);

  return TOP_UNDEFINED;
}

bool
TI_TOP_Is_Legal_In_Format_Slot (int format, int slot, TOP topcode)
{
  if (xtarch) {
    return xtarch->top_is_legal_in_format_slot(format, slot, topcode);
  }
  return true;
}
  

extern bool
TI_TOP_Is_CoreBranch(TOP top)
{
  switch (top) {
  case TOP_ball:
  case TOP_bany:
  case TOP_bbc:
  case TOP_bbs:
  case TOP_bbci:
  case TOP_bbsi:
  case TOP_beq:
  case TOP_beqi:
  case TOP_beqz:
  case TOP_bge:
  case TOP_bgei:
  case TOP_bgeu:
  case TOP_bgeui:
  case TOP_bgez:
  case TOP_blt:
  case TOP_blti:
  case TOP_bltu:
  case TOP_bltui:
  case TOP_bltz:
  case TOP_bnall:
  case TOP_bnone:
  case TOP_bne:
  case TOP_bnei:
  case TOP_bnez:
    return true;
  default:
    return false;
  }
}

extern TOP
TI_TOP_Get_WBranch_Top_Litclass(TOP top, ISA_LITCLASS *lc)
{
  *lc = LC_UNDEFINED;
  if (xtarch) {
    return xtarch->get_wbranch_top_litclass(top, lc);
  }
  return TOP_UNDEFINED;
}

extern INT
TI_Wide_Branch_MinRange()
{
  if (xtarch) {
    return xtarch->get_wbranch_range();
  }
  return -1;
}


/* =============================================================

   Subsets

   =========================================================== */

ISA_SUBSET
TI_ISA_Subset_Value (void)
{
  return ISA_SUBSET_Value;
}


const char*
TI_ISA_Subset_Name (ISA_SUBSET subset)
{
  return ISA_SUBSET_Name(subset);
}


BOOL
TI_ISA_Subset_Member (ISA_SUBSET subset, TOP topcode)
{
  return ISA_SUBSET_Member(subset, topcode);
}


/* =============================================================

   ISA Properties

   =========================================================== */

BOOL
TI_ISA_Property_Set (ISA_PROPERTY prop, TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return TOP_has_property(topcode, prop);

  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    if (inst != NULL)
    {
      /* We currently support these properties for tie
         instructions. The Is_True checks for each property make sure
         that the ISA_PROPERTY value is defined even when no core
         instructions have that property (see changes to
         isa_properties that allow forcing a property to be
         defined). */

      switch (prop)
      {
      case PROP_move:
	return inst->is_always_copy();

      case PROP_memtrap:
	return inst->is_load() || inst->is_store();

      case PROP_load:
	return inst->is_load();

      case PROP_store:
	return inst->is_store();

      case PROP_unknown_memdata:
	return inst->has_unknown_mem_data();

      case PROP_unknown_addr:
	return inst->has_unknown_addr();

      case PROP_unalign_store:
	return inst->unalign_store();

      case PROP_same_res:
	return inst->same_res();

      case PROP_base_update:
	return inst->base_update();

      case PROP_cond:
	return inst->is_cond();

      case PROP_xfer:
	return inst->is_xfer();

      case PROP_simulated:
	return inst->is_simulated();

      case PROP_use_shared_resource:
	return inst->use_shared_resource();

      case PROP_has_multireg_operand:
	return inst->has_multireg_operand();

      case PROP_noop:
	return inst->is_noop();

      case PROP_generic:
	return inst->is_generic();

      case PROP_side_effects:
	return inst->has_side_effects();

      case PROP_extern_effects:
	return inst->has_tie_port() || inst->has_export_state();
      }
    }
  }

  return FALSE;
}


BOOL
TI_ISA_Has_Updating_Ops (void)
{
  if (xtarch && (xtarch->num_updating_ops() != 0))
    return TRUE;

  return FALSE;
}


BOOL
TI_ISA_Has_Indexed_Ops (void)
{
  if (xtarch && (xtarch->num_indexed_ops() != 0))
    return TRUE;

  return FALSE;
}


UINT32
TI_ISA_Mem_Ref_Bytes (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
  {
    switch (topcode)
    {
    case TOP_l8ui:
    case TOP_s8i:
      return 1;

    case TOP_l16si:
    case TOP_l16ui:
    case TOP_s16i:
      return 2;

    case TOP_l32i:
    case TOP_l32i_n:
    case TOP_l32r:
    case TOP_s32i:
    case TOP_s32i_n:
      return 4;
    }
  }
  else if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    if (inst != NULL)
      return inst->memory_size();
  }

  return 0;
}


INT
TI_ISA_Inst_Bytes (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
  {
    return TOP_size(topcode);
  }
  else if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    if (inst != NULL)
      return inst->size();
  }

  FmtAssert(FALSE, ("no size of top %d", topcode));
  return 0;
}


INT
TI_ISA_Copy_Operand (TOP topcode, BOOL required)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
  {
    if (topcode == TOP_mov_n)
      return 0;

    /* simulated ops for moving br register */
    if (topcode == TOP_movbr || topcode == TOP_movbr2 ||
        topcode == TOP_movbr4 || topcode == TOP_movbr8 ||
	topcode == TOP_movbr16)
      return 0;

    /* for ops that are sometimes copy but cannot be turned into mov */
    if (topcode == TOP_orb)
      return 0;
  }
  else if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    Is_True(inst != NULL, ("no XT_Instruction for topcode %d", topcode));
    if ((inst != NULL) && (inst->is_sometimes_copy() || inst->is_always_copy()))
      return inst->copy_src();
  }

  FmtAssert(!required, ("topcode %s is not a copy", TI_TOP_Name(topcode)));
  return -1;
}

bool TI_ISA_Has_Wide_Branches(void)
{
  if (xtarch)
  {
    return xtarch->has_wide_branch();
  } else {
    return false;
  }
}

bool TI_ISA_Prefer_MUL16(void)
{
  if (xtmicroarch)
    return xtmicroarch->prefer_mul16();

  return false;
}


/* =============================================================

   Processor Properties

   =========================================================== */

BOOL
TI_PROC_Property_Set (PROC_PROPERTY prop)
{
  return PROC_has_property(prop);
}


/* =============================================================

   Registers

   =========================================================== */

INT
TI_ISA_Num_Regclasses (void)
{
  return ISA_REGCLASS_COUNT + (xtarch ? xtarch->num_regclasses() : 0);
}


ISA_REGCLASS
TI_ISA_Regclass_First (void)
{
  return ISA_MIN_REGCLASS;
}


ISA_REGCLASS
TI_ISA_Regclass_Last (void)
{
  return (ISA_REGCLASS)((INT)ISA_MAX_REGCLASS
			+ (xtarch ? xtarch->num_regclasses() : 0));
}


ISA_REGCLASS
TI_ISA_Regclass_Integer (void)
{
  return ISA_REGCLASS_integer;
}


ISA_REGCLASS
TI_ISA_Regclass_Float (void)
{
  if (xtarch)
  {
    ISA_REGCLASS frc = xtarch->float_regclass();
    if (frc != ISA_REGCLASS_UNDEFINED)
      return frc;
  }

  return ISA_REGCLASS_integer;
}


ISA_REGCLASS
TI_ISA_Regclass_Branch (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGCLASS_branch;
}


ISA_REGCLASS
TI_ISA_Regclass_Macc (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGCLASS_macc;
}


ISA_REGCLASS
TI_ISA_Regclass_Special (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGCLASS_special;
}


ISA_REGCLASS
TI_ISA_Regclass_HiFi2_PR (void)
{
  ISA_REGCLASS rc =
    xtarch ? xtarch->hifi2_pr_regclass() : ISA_REGCLASS_UNDEFINED;
  return rc;
}


ISA_REGCLASS
TI_ISA_Regclass_HiFi2_QR (void)
{
  ISA_REGCLASS rc =
    xtarch ? xtarch->hifi2_qr_regclass() : ISA_REGCLASS_UNDEFINED;
  return rc;
}


ISA_REGSUBCLASS
TI_ISA_Regsubclass(const char *state_name)
{
  ISA_REGSUBCLASS sc;
  FOR_ALL_ISA_REGSUBCLASS(sc) {
    const ISA_REGSUBCLASS_INFO * info = TI_ISA_Regsubclass_Info (sc);
    if (!strcmp(TI_ISA_Regsubclass_Name(info),state_name))
      return sc;
  }

  return ISA_REGSUBCLASS_UNDEFINED;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Special_Reg_Num (INT reg_num)
{

  switch (reg_num) {
    case 0: return TI_ISA_Regsubclass("LBEG");
    case 1: return TI_ISA_Regsubclass("LEND");
    case 2: return TI_ISA_Regsubclass("lcount");
    case 3: return TI_ISA_Regsubclass("SAR");
    case 4: return TI_ISA_Regsubclass("breg");
    case 12: return TI_ISA_Regsubclass("SCOMPARE1");
    case 16: return TI_ISA_Regsubclass("ACC");
    case 17: return TI_ISA_Regsubclass("ACC");
  }
  DevWarn(false,("Unhandled special register number %d", reg_num));
  return ISA_REGSUBCLASS_UNDEFINED;
}

const char*
TI_ISA_Regsubclass_Special_Name (INT reg_num)
{
  
  switch (reg_num) {
    case 0: return "lbeg";
    case 1: return "lend";
    case 2: return "lcount";
    case 3: return "sar";
    case 4: return "breg";
    case 12: return "scompare1";
    case 16: return "acclo";
    case 17: return "acchi";
  }
  DevWarn(false,("Unhandled special register number %d", reg_num));
  return NULL;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Branch1 (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGSUBCLASS_branch_1;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Branch2 (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGSUBCLASS_branch_2;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Branch4 (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGSUBCLASS_branch_4;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Branch8 (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGSUBCLASS_branch_8;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_Branch16 (void)
{
  // fixme, check here for coprocessor tie...
  return ISA_REGSUBCLASS_branch_16;
}


BOOL
TI_ISA_Regclass_Is_State (ISA_REGCLASS rc)
{
  if (xtarch)
    return xtarch->is_state_regclass(rc);

  return FALSE;
}

ISA_REGCLASS
TI_ISA_Regclass_State (const char *state_name)
{
  if (xtarch)
    return xtarch->state_regclass(state_name);
  
  return ISA_REGCLASS_UNDEFINED;
}

INT
TI_ISA_State_Bit_Size (const char *state_name)
{
  if (xtarch)
    return xtarch->find_state(state_name)->bit_size();
  
  return -1;
}

ISA_REGSUBCLASS
TI_ISA_Regsubclass_State (const char *state_name)
{
  if (xtarch)
    return xtarch->state_regsubclass(state_name);

  return ISA_REGSUBCLASS_UNDEFINED;
}

INT
TI_ISA_Register_Max (void)
{
  return xtarch ? (xtarch->max_registers() - 1) : ISA_MAX_REGISTER;
}


const ISA_REGCLASS_INFO *
TI_ISA_Regclass_Info (ISA_REGCLASS rc)
{
  Is_True(rc <= TI_ISA_Regclass_Last(), ("unexpected regclass %d\n", rc));

  if (xtarch)
  {
    const ISA_REGCLASS_INFO *info = xtarch->regclass_info(rc);
    if (info)
      return info;
  }

  if (rc <= ISA_MAX_REGCLASS)
    return ISA_REGCLASS_Info(rc);

  FmtAssert(FALSE, ("No regclass info for regclass %d\n", rc));
  return NULL;
}


INT
TI_ISA_Regclass_First_Reg (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_First_Reg(info);
}


INT
TI_ISA_Regclass_Last_Reg (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_Last_Reg(info);
}


INT
TI_ISA_Regclass_Bit_Size (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_Bit_Size(info);
}


INT
TI_ISA_Regclass_Can_Store (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_Can_Store(info);
}


INT
TI_ISA_Regclass_Multiple_Save (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_Multiple_Save(info);
}


const char *
TI_ISA_Regclass_Name (const ISA_REGCLASS_INFO *info)
{
  return ISA_REGCLASS_INFO_Name(info);
}

const char *
TI_ISA_Regclass_Reg_Name (const ISA_REGCLASS_INFO *info,
			  INT reg_index)
{
  return ISA_REGCLASS_INFO_Reg_Name(info, reg_index);
}

/*
   This function returns 
     true : if <regclass, reg> is callee saved;
     false: if <regclass, reg> is caller saved.
*/
static bool TI_ISA_Regclass_Reg_Callee_Save (ISA_REGCLASS regclass, INT reg)
{
  FmtAssert((regclass > ISA_MAX_REGCLASS), ("Not a TIE regfile"));
  if (xtarch) {
    XT_RegFile_p rf = xtarch->find_regfile(regclass);
    if (rf) {
      return rf->is_callee_saved(reg);
    }
  }
  return false;
}

INT
TI_ISA_Regclass_Reg_Debug_Number (ISA_REGCLASS regclass, INT reg_index)
{
  /* Debugging register numbers are assigned as follows.

     0  - 15   : a0 - a15
     16 - 31   : b0 - b15
     32 - 35   : m0 - m3
     256- ...  : tie registers in libcc order

  */
#define AR_DBG_START   0
#define BR_DBG_START   16
#define MR_DBG_START   32
#define TIE_DBG_START  256

  if (regclass == ISA_REGCLASS_integer)
    return AR_DBG_START + reg_index;
  else if (regclass == ISA_REGCLASS_branch)
    return BR_DBG_START + reg_index;
  else if (regclass == ISA_REGCLASS_macc)
    return MR_DBG_START + reg_index;
  else if (regclass == ISA_REGCLASS_special)
    return -1;

  if (xtarch)
  {
    XT_RegFile_p rf = xtarch->find_regfile(regclass);
    if (!rf)
    {
      Lmt_DevWarn(1, ("Can't find register file for regclass %d", regclass));
      return -1;
    }

    return TIE_DBG_START + rf->debug_number_start() + reg_index;
  }

  return -1;
}


ISA_REGCLASS
TI_ISA_Regclass_For_Mtype (TYPE_ID mtype)
{
  switch (mtype)
  {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_I8:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
  case MTYPE_U8:
  case MTYPE_F8:
    return TI_ISA_Regclass_Integer();

  case MTYPE_F4:
    return TI_ISA_Regclass_Float();

  case MTYPE_XTBOOL:
  case MTYPE_XTBOOL2:
  case MTYPE_XTBOOL4:
  case MTYPE_XTBOOL8:
  case MTYPE_XTBOOL16:
    return TI_ISA_Regclass_Branch();
  }

  /* If we have user-define ctypes, and the mtypes representing them
     have been initialized, then we can find the regclass for the
     user-defined mtype. */
  if ((MTYPE_is_tie(mtype) || MTYPE_is_xtbool(mtype)) &&
      xtarch && (mtype_to_regclass != NULL))
  {
    return mtype_to_regclass[mtype];
  }

  return ISA_REGCLASS_UNDEFINED;
}

INT
TI_ISA_Regsize_For_Mtype (TYPE_ID mtype)
{
  switch (mtype)
  {
  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
  case MTYPE_F4:
    return 1;
  case MTYPE_I8:
  case MTYPE_U8:
  case MTYPE_F8:
    return 2;

  case MTYPE_XTBOOL:
    return 1;
  case MTYPE_XTBOOL2:
    return 2;
  case MTYPE_XTBOOL4:
    return 4;
  case MTYPE_XTBOOL8:
    return 8;
  case MTYPE_XTBOOL16:
    return 16;
  }

  /* If we have user-define ctypes, and the mtypes representing them
     have been initialized, then we can find the regclass for the
     user-defined mtype. */
  if ((MTYPE_is_tie(mtype) || MTYPE_is_xtbool(mtype)) &&
      xtarch && (mtype_to_regsize != NULL))
  {
    return mtype_to_regsize[mtype];
  }

  return 1;
}

TYPE_ID
TI_ISA_Mtype_For_Regclass (ISA_REGCLASS rc)
{
  if (rc==TI_ISA_Regclass_Integer())
    return MTYPE_U4;
  else if (rc==TI_ISA_Regclass_Branch())
    return MTYPE_XTBOOL;
  else
    return regclass_to_mtype[(int)rc];
}

ISA_REGCLASS
TI_ISA_Find_Regclass (const char* short_name)
{
  /* We initialize some core register files with short names that are different
     from the TIE ones. Make sure we handle the TIE ones here. */

  extern DLL_SHARED BOOL xt_booleans;
  extern DLL_SHARED BOOL xt_mac16;

  if (!strcmp(short_name, "a"))
    return TI_ISA_Regclass_Integer();

  if (xt_booleans && !strcmp(short_name, "b"))
    return TI_ISA_Regclass_Branch();

  if (xt_mac16 && !strcmp(short_name, "m"))
    return TI_ISA_Regclass_Macc();

  ISA_REGCLASS cl;
  FOR_ALL_ISA_REGCLASS(cl) {
    if (!strcmp(short_name,TI_ISA_Regclass_Name(TI_ISA_Regclass_Info(cl)))) {
      return cl;
    }
  }
  return ISA_REGCLASS_UNDEFINED;
}

INT
TI_ISA_Num_Regsubclasses (void)
{
  return ISA_REGSUBCLASS_COUNT + (xtarch ? xtarch->num_regsubclasses() : 0);
}


ISA_REGSUBCLASS
TI_ISA_Regsubclass_First (void)
{
  return ISA_MIN_REGSUBCLASS;
}


ISA_REGSUBCLASS
TI_ISA_Regsubclass_Last (void)
{
  return (ISA_REGSUBCLASS)((INT)ISA_MAX_REGSUBCLASS
			   + (xtarch ? xtarch->num_regsubclasses() : 0));
}


const ISA_REGSUBCLASS_INFO *
TI_ISA_Regsubclass_Info (ISA_REGSUBCLASS rc)
{
  Is_True(rc <= TI_ISA_Regsubclass_Last(), ("unexpected regsubclass %d\n", rc));

  if (rc <= ISA_MAX_REGSUBCLASS)
    return ISA_REGSUBCLASS_Info(rc);

  if (xtarch)
  {
    const ISA_REGSUBCLASS_INFO *info = xtarch->regsubclass_info(rc);
    if (info)
      return info;
  }

  FmtAssert(FALSE, ("No regsubclass info for regsubclass %d\n", rc));
  return NULL;
}


ISA_REGCLASS
TI_ISA_Regsubclass_Class (const ISA_REGSUBCLASS_INFO *info)
{
  return ISA_REGSUBCLASS_INFO_Class(info);
}


INT
TI_ISA_Regsubclass_Count (const ISA_REGSUBCLASS_INFO *info)
{
  return ISA_REGSUBCLASS_INFO_Count(info);
}


UINT
TI_ISA_Regsubclass_Member (const ISA_REGSUBCLASS_INFO *info, INT n)
{
  return ISA_REGSUBCLASS_INFO_Member(info, n);
}


const char *
TI_ISA_Regsubclass_Name (const ISA_REGSUBCLASS_INFO *info)
{
  return ISA_REGSUBCLASS_INFO_Name(info);
}


const char *
TI_ISA_Regsubclass_Reg_Name (const ISA_REGSUBCLASS_INFO *info,
			     INT reg_index)
{
  return ISA_REGSUBCLASS_INFO_Reg_Name(info, reg_index);
}


/* =============================================================

   ABI

   =========================================================== */

ABI_PROPERTIES_ABI
TI_ABI_Value (void)
{
  return ABI_PROPERTIES_ABI_Value;
}


const char *
TI_ABI_Reg_Name (ISA_REGCLASS rc, INT reg)
{
  Is_True(rc <= TI_ISA_Regclass_Last(), ("unexpected regclass %d\n", rc));

  if (rc <= ISA_MAX_REGCLASS)
    return ABI_PROPERTY_Reg_Name(rc, reg);

  if (xtarch)
  {
    const ISA_REGCLASS_INFO *info = TI_ISA_Regclass_Info(rc);
    return TI_ISA_Regclass_Reg_Name(info, reg);
  }

  FmtAssert(FALSE, ("No abi name for regclass %d, reg %d\n", rc, reg));
  return NULL;
}


BOOL
TI_ABI_Property_Set (ABI_PROPERTY prop, ISA_REGCLASS rc, INT reg)
{
  Is_True(rc <= TI_ISA_Regclass_Last(), ("unexpected regclass %d\n", rc));

  if (rc <= ISA_MAX_REGCLASS && reg != -1)
    return ABI_PROPERTY_Set(rc, reg, prop);

  switch (prop) {

    case ABI_PROPERTY_allocatable:
    {
      if (rc <= ISA_MAX_REGCLASS) {
	if (rc==ISA_REGCLASS_macc || rc==ISA_REGCLASS_special)
	  return FALSE;
	else
	  return TRUE;
      } else if (xtarch) {
	/* Unless the user-defined register file is marked as
	   non-allocatable, all registers are allocated. */
        XT_RegFile_p rf = xtarch->find_regfile(rc);
        FmtAssert(rf != NULL,
		  ("can't find user regfile for regclass %d\n", rc));
        return (!TI_ISA_Regclass_Is_State(rc) && rf->allocatable());
      }
      break;
    }

    /* All user-defined register files have all caller-saved
       registers. */
    case ABI_PROPERTY_caller:
      if (TI_ISA_Regclass_Reg_Callee_Save(rc, reg)) {
        return FALSE;
      } else {
        return TRUE;
      }
      break;

    case ABI_PROPERTY_callee:
      if (TI_ISA_Regclass_Reg_Callee_Save(rc, reg)) {
        return TRUE;
      } else {
        return FALSE;
      }
      break;

    /* All user-defined register files have register 0 as the function
       return value register. */
    case ABI_PROPERTY_func_val:
      if (rc>ISA_MAX_REGCLASS)
	return (!TI_ISA_Regclass_Is_State(rc)) && (reg == 0);
      break;

    case ABI_PROPERTY_func_arg:
    case ABI_PROPERTY_stack_ptr:
    case ABI_PROPERTY_frame_ptr:
    case ABI_PROPERTY_static_link:
    case ABI_PROPERTY_shift_amount:
    case ABI_PROPERTY_br_regfile:
    case ABI_PROPERTY_zcl_loop_begin:
    case ABI_PROPERTY_zcl_loop_end:
    case ABI_PROPERTY_zcl_loop_count:
    case ABI_PROPERTY_acc_lo:
    case ABI_PROPERTY_acc_hi:
      if (rc>ISA_MAX_REGCLASS)
	return FALSE;
      break;
    case ABI_PROPERTY_ret_addr:
      if (rc == ISA_REGCLASS_integer && reg == 0)
	return TRUE;
      else
	return FALSE;
      break;
  }

  Is_True(false,("Unrecognized property %d queried on reg %d of class %s",
		prop, reg, TI_ISA_Regclass_Name(TI_ISA_Regclass_Info(rc))));

  return FALSE;
}


/* =============================================================

   Literals.

   =========================================================== */

static BOOL init_create_derived = FALSE;
static UINT next_derived_lc, first_derived_lc;

struct LC_OFFSET
{
  const ISA_LITCLASS _lc;
  const ISA_LITCLASS _orig_lc;
  const INT32 _offset;
  char *_name;

  LC_OFFSET (ISA_LITCLASS lc, ISA_LITCLASS orig_lc, INT32 offset) :
    _lc(lc), _orig_lc(orig_lc), _offset(offset)
  {
    char buf[128];
    sprintf(buf, "derivedlc%d", lc - first_derived_lc);
    _name = CXX_NEW_ARRAY(char, strlen(buf) + 2, &libti_pool);
    strcpy(_name, buf);
  }
};

typedef UTL_Map<ISA_LITCLASS, LC_OFFSET *, UTL_Map_IntHash> LC_OFFSET_MAP;
static LC_OFFSET_MAP *lc_offset_map = NULL;


static void
TI_LC_Init_For_Derived (void)
{
  lc_offset_map = CXX_NEW(LC_OFFSET_MAP(&libti_pool, 31), &libti_pool);

  next_derived_lc = LC_max;
  if (xtarch)
    next_derived_lc = xtarch->litclass_max();

  first_derived_lc = next_derived_lc;
  init_create_derived = TRUE;
}


static LC_OFFSET *
find_derived_lc (ISA_LITCLASS lc)
{
  if ((lc >= first_derived_lc) && (lc < next_derived_lc))
  {
    LC_OFFSET *lco;
    if (!lc_offset_map->find(lc, &lco))
      FmtAssert(TRUE, ("inconsistent derived litclass map"));

    return lco;
  }

  Is_True(!lc_offset_map->find(lc), ("unexpected derived litclass"));
  return FALSE;
}


ISA_LITCLASS
TI_ISA_LC_Find (xtensa_opcode isa_opc, int isa_opnd)
{
  if (xtarch) {
    XT_Litclass_p lcp = xtarch->find_litclass(isa_opc, isa_opnd);
    if (lcp)
      return lcp->litclass();
    else
      return LC_UNDEFINED;
  } else
    return LC_UNDEFINED;
}

ISA_LITCLASS
TI_ISA_LC_Create (ISA_LITCLASS orig_lc, INT32 offset)
{
  Is_True(init_create_derived, ("can't create derived litclasses before init"));

  if (offset == 0)
    return orig_lc;

  /* Search for an existing litclass representing 'lc' plus
     'offset'. If we find one, simply return that litclass, otherwise
     we must create a new one. */

  LC_OFFSET_MAP::keyValuePair_p scan;
  LC_OFFSET_MAP::iter iter(lc_offset_map);
  while ((scan = iter.next()) != NULL)
  {
    LC_OFFSET *lc_offset = scan->value();
    if ((lc_offset->_orig_lc == orig_lc) && (lc_offset->_offset == offset))
      return lc_offset->_lc;
  }

  LC_OFFSET *new_lco = CXX_NEW(LC_OFFSET((ISA_LITCLASS)next_derived_lc, orig_lc, offset),
			       &libti_pool);
  lc_offset_map->insert((ISA_LITCLASS)next_derived_lc, new_lco);
  next_derived_lc++;

  return new_lco->_lc;
}


const char *
TI_ISA_LC_Name (ISA_LITCLASS lc)
{
  Is_True(init_create_derived, ("expecting init"));

  if (lc < LC_max)
    return ISA_LC_Name(lc);

  LC_OFFSET *lco = find_derived_lc(lc);
  if (lco)
    return lco->_name;

  if (xtarch)
  {
    const char *lcn = xtarch->litclass_name(lc);
    if (lcn)
      return lcn;
  }

  FmtAssert(FALSE, ("no name for literal class %d", lc));
  return NULL;
}


BOOL
TI_ISA_LC_Value_In_Class (INT64 val, ISA_LITCLASS lc)
{
  Is_True(init_create_derived, ("expecting init"));

  if (lc < LC_max)
    return ISA_LC_Value_In_Class(val, lc);

  LC_OFFSET *lco = find_derived_lc(lc);
  if (lco)
    return TI_ISA_LC_Value_In_Class(val - lco->_offset, lco->_orig_lc);

  if (xtarch)
    return xtarch->litclass_value_ok(lc, val);

  return FALSE;
}


BOOL
TI_ISA_LC_Range (ISA_LITCLASS lc, INT32 *low, INT32 *high)
{
  Is_True(init_create_derived, ("expecting init"));

  if (lc < LC_max)
  {
    *low = ISA_LC_Value_Min(lc);
    *high = ISA_LC_Value_Max(lc);
    return TRUE;
  }

  LC_OFFSET *lco = find_derived_lc(lc);
  if (lco)
  {
    if (TI_ISA_LC_Range(lco->_orig_lc, low, high))
    {
      *low += lco->_offset;
      *high += lco->_offset;

      return(*low <= *high);
    }

    return FALSE;
  }

  if (xtarch)
    return xtarch->litclass_range(lc, low, high);

  return FALSE;
}


/* =============================================================

   Enums.

   =========================================================== */

const char *
TI_ISA_EC_Name (ISA_ENUMCLASS ec)
{
  return ISA_EC_Name(ec);
}


ISA_ENUMCLASS_VALUE
TI_ISA_EC_First_Value (ISA_ENUMCLASS ec)
{
  return ISA_EC_First_Value(ec);
}


ISA_ENUMCLASS_VALUE
TI_ISA_EC_Last_Value (ISA_ENUMCLASS ec)
{
  return ISA_EC_Last_Value(ec);
}


const char *
TI_ISA_ECV_Name (ISA_ENUMCLASS_VALUE ecv)
{
  return ISA_ECV_Name(ecv);
}


INT
TI_ISA_ECV_Intval (ISA_ENUMCLASS_VALUE ecv)
{
  return ISA_ECV_Intval(ecv);
}


/* =============================================================

   Operands.

   =========================================================== */

INT
TI_ISA_Operand_Max (void)
{
  UINT tie_max_operands = (xtarch ? xtarch->max_inst_operands() : 0);

  return Max(TI_ISA_ASM_OPERANDS_MAX, Max(ISA_OPERAND_max_operands, tie_max_operands));
}


INT
TI_ISA_Result_Max (void)
{
  UINT tie_max_results = (xtarch ? xtarch->max_inst_results() : 0);

  return Max(TI_ISA_ASM_RESULTS_MAX, Max(ISA_OPERAND_max_results, tie_max_results));
}


const ISA_OPERAND_INFO *
TI_ISA_Operand_Info (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return ISA_OPERAND_Info(topcode);

  if (xtarch)
    return xtarch->find_instruction(topcode)->operand_info();

  return NULL;
}


INT
TI_ISA_Op_Operands (const ISA_OPERAND_INFO *info)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Operands(info);

  return ti->num_operands();
}


INT
TI_ISA_Op_Results (const ISA_OPERAND_INFO *info)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Results(info);

  return ti->num_results();
}


const ISA_OPERAND_VALTYP *
TI_ISA_Op_Operand (const ISA_OPERAND_INFO *info, INT opnd)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Operand(info, opnd);

  return ti->operand(opnd)->info();
}



const ISA_OPERAND_VALTYP *
TI_ISA_Op_Result (const ISA_OPERAND_INFO *info, INT result)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Result(info, result);

  return ti->result(result)->info();
}


ISA_OPERAND_USE
TI_ISA_Op_Operand_Use (const ISA_OPERAND_INFO *info, INT opnd)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Opnd_Use(info, opnd);

  return ti->operand(opnd)->use();
}

ISA_OPERAND_USE
TI_ISA_Op_Result_Use (const ISA_OPERAND_INFO *info, INT result)
{
  XT_Instruction_p ti = (XT_Instruction_p)ISA_OPERAND_INFO_Tie_Info(info);
  if (ti == NULL)
    return ISA_OPERAND_INFO_Res_Use(info, result);

  return ti->result(result)->use();
}

INT32 TI_ISA_Op_Sameres_Operand (TOP topcode, INT32 result_index)
{
  if (!TI_ISA_Property_Set(PROP_same_res, topcode))
    return -1;

  switch (topcode) {
    case TOP_const16hi:
    case TOP_const16lo:
    case TOP_const16:
    case TOP_moveqz:
    case TOP_movgez:
    case TOP_movltz:
    case TOP_movnez:
    case TOP_movf:
    case TOP_movt:   if (result_index==0) return 0;
		break;
    case TOP_asm:
	DevWarn("TOP_asm same res relation unknown");
  }

  return (xtarch? xtarch->same_res_get_opnd(topcode, result_index) : -1);
}

INT32 TI_ISA_Op_Sameres_Result (TOP topcode, INT32 operand_index)
{
  if (!TI_ISA_Property_Set(PROP_same_res, topcode))
    return -1;

  switch (topcode) {
    case TOP_const16hi:
    case TOP_const16lo:
    case TOP_const16:
    case TOP_moveqz:
    case TOP_movgez:
    case TOP_movltz:
    case TOP_movnez:
    case TOP_movf:
    case TOP_movt:   if (operand_index==0) return 0;
		break;
    case TOP_asm:
	DevWarn("TOP_asm same res relation unknown");
  }

  return (xtarch? xtarch->same_res_get_res(topcode, operand_index) : -1);
}

ISA_REGCLASS
TI_ISA_Valtyp_Regclass (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Register_Class(type);
}


ISA_REGSUBCLASS
TI_ISA_Valtyp_Regsubclass (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Register_Subclass(type);
}


ISA_LITCLASS
TI_ISA_Valtyp_Litclass (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Literal_Class(type);
}


ISA_ENUMCLASS
TI_ISA_Valtyp_Enumclass (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Enum_Class(type);
}


INT
TI_ISA_Valtyp_Size (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Size(type);
}

INT
TI_ISA_Valtyp_Num_Regs (const ISA_OPERAND_VALTYP *type)
{
  FmtAssert(type && ISA_OPERAND_VALTYP_Is_Register(type),
	    ("Need register value type for TI_ISA_Valtyp_Num_Regs()"));
  int bit_size = ISA_OPERAND_VALTYP_Size(type);
  int reg_size = TI_ISA_Regclass_Bit_Size(TI_ISA_Regclass_Info(
		 TI_ISA_Valtyp_Regclass(type)));
  FmtAssert((bit_size % reg_size)==0, ("Unexpected bit size %d", bit_size));
  return (bit_size/reg_size);
}


BOOL
TI_ISA_Valtyp_Is_Register (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_Register(type);
}


BOOL
TI_ISA_Valtyp_Is_Signed (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_Signed(type);
}


BOOL
TI_ISA_Valtyp_Is_FPU_Int (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_FPU_Int(type);
}


BOOL
TI_ISA_Valtyp_Is_PCRel (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_PCRel(type);
}

BOOL
TI_ISA_Valtyp_Is_Continuation (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_Continuation(type);
}


BOOL
TI_ISA_Valtyp_Is_Literal (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_Literal(type);
}


BOOL
TI_ISA_Valtyp_Is_Enum (const ISA_OPERAND_VALTYP *type)
{
  return ISA_OPERAND_VALTYP_Is_Enum(type);
}


BOOL
TI_TOP_Can_Have_Immediate (INT64 value, TOP topcode)
{
  /* Instead of calling TOP_Can_Have_Immediate in targ_info (which
     assumes an instruction has at most one immediate), we check all
     immediates. */

  TOP tops[256];

  if (xtarch) {
    XT_Instruction* inst = xtarch->find_instruction(topcode);
    if (inst->is_generic()) {
      int num_tops,i;
      num_tops = TI_TOP_Get_Special_Tops (topcode, tops, 256);
      for (i=0; i<num_tops; i++) {
	if (TI_TOP_Can_Have_Immediate(value, tops[i])==false)
	  return false;
      }
      return true;
    }
  }

  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);
  BOOL have_lit = FALSE;

  for (UINT i = 0; i < opnds; i++)
  {
    const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, i);
    ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
    if (lit_class != LC_UNDEFINED)
    {
      have_lit = TRUE;
      if (!TI_ISA_LC_Value_In_Class(value, lit_class))
	return FALSE;
    }
  }

  return have_lit;
}


INT
TI_TOP_Immediate_Operand (TOP topcode, ISA_LITCLASS *lclass)
{
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);

  for (UINT i = 0; i < opnds; i++)
  {
    const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, i);
    ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
    if (lit_class != LC_UNDEFINED)
    {
      if (lclass)
	*lclass = lit_class;

      return i;
    }
  }

  return -1;
}

INT
TI_TOP_Num_Immediate_Operand (TOP topcode)
{
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);
  int count = 0;

  for (UINT i = 0; i < opnds; i++)
  {
    const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, i);
    ISA_LITCLASS lit_class = TI_ISA_Valtyp_Litclass(vtype);
    if (lit_class != LC_UNDEFINED)
    {
      count++;
    }
  }

  return count;
}


INT
TI_TOP_Find_Operand_Use (TOP topcode, ISA_OPERAND_USE use)
{
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);

  for (UINT i = 0; i < opnds; i++)
  {
    ISA_OPERAND_USE this_use = TI_ISA_Op_Operand_Use(opinfo, i);
    if (this_use == use)
      return i;
  }

  return -1;
}

INT
TI_TOP_Find_Result_Use (TOP topcode, ISA_OPERAND_USE use)
{
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT results = TI_ISA_Op_Results(opinfo);

  for (UINT i = 0; i < results; i++)
  {
    ISA_OPERAND_USE this_use = TI_ISA_Op_Result_Use(opinfo, i);
    if (this_use == use)
      return i;
  }

  return -1;
}

ISA_LITCLASS
TI_TOP_Branch_Target_Litclass (TOP topcode)
{
  const ISA_OPERAND_INFO *opinfo = TI_ISA_Operand_Info(topcode);
  UINT opnds = TI_ISA_Op_Operands(opinfo);

  UINT target_ix;
  for (target_ix = 0; target_ix < opnds; target_ix++)
  {
    ISA_OPERAND_USE this_use = TI_ISA_Op_Operand_Use(opinfo, target_ix);
    if (this_use == OU_target) {
      break;
    }
  }
  if (target_ix > opnds) return LC_UNDEFINED;

  const ISA_OPERAND_VALTYP *vtype = TI_ISA_Op_Operand(opinfo, target_ix);
  return TI_ISA_Valtyp_Litclass(vtype);
}


/* =============================================================

   Bundle.

   =========================================================== */

INT
TI_ISA_Num_Bundles (void)
{
  return (xtarch ? xtarch->num_bundles() : ISA_MAX_BUNDLES);
}


INT
TI_ISA_Bundle_Bytes (INT bundle)
{
  return (xtarch ? xtarch->bundle_bytes(bundle) : 3);
}


INT
TI_ISA_Num_Slots (INT bundle)
{
  return (xtarch ? xtarch->num_slots(bundle) :
	  ISA_EXEC_Bundle_Info(bundle).slot_count);
}

INT
TI_ISA_Max_Num_Slots ()
{
  return (xtarch ? xtarch->max_num_slots() : 1);
}

INT
TI_ISA_Exec_Num_Units (void)
{
  return (xtarch ? xtarch->num_exec_units() : ISA_EXEC_MAX + 1);
}


INT
TI_ISA_Tag_Shift (void)
{
  FmtAssert(FALSE, ("Not supported\n"));
  return ISA_TAG_SHIFT;
}


ISA_EXEC_UNIT_PROPERTY
TI_ISA_Exec_Unit_Prop (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (TI_ISA_Property_Set(PROP_simulated, topcode))
    return (ISA_EXEC_UNIT_PROPERTY)-1;

  if (xtarch)
    return (ISA_EXEC_UNIT_PROPERTY)(xtarch->exec_unit_prop(topcode));

  if (topcode < TOP_count)
    return ISA_EXEC_Unit_Prop(topcode);

  return 0;
}


ISA_EXEC_UNIT_PROPERTY
TI_ISA_Exec_Slot_Prop (INT bundle, INT slot_index)
{
  if (xtarch)
    return (ISA_EXEC_UNIT_PROPERTY)(xtarch->exec_slot_prop(bundle, slot_index));

  if (bundle < ISA_MAX_BUNDLES)
    return ISA_EXEC_Slot_Prop(bundle, slot_index);

  return 0;
}


UINT64
TI_ISA_Exec_Slot_Mask (INT bundle)
{
  FmtAssert(FALSE, ("Not supported\n"));
  if (bundle < ISA_MAX_BUNDLES)
    return ISA_EXEC_Slot_Mask(bundle);

  return 0;
}


BOOL
TI_ISA_Exec_Stop (INT bundle, INT slot_index)
{
/*  if (xtarch)
    return (slot_index == xtarch->num_slots(bundle)-1);*/

  if (bundle < ISA_MAX_BUNDLES)
    return ISA_EXEC_Stop(bundle, slot_index);

  return FALSE;
}


ISA_EXEC_UNIT
TI_ISA_Exec_Unit (INT bundle, INT slot_index)
{
  FmtAssert(FALSE, ("Not supported\n"));
/*
  if (xtarch)
    return xtarch->find_instruction(topcode)->operand_info();
*/
  if (bundle < ISA_MAX_BUNDLES)
    return ISA_EXEC_Unit(bundle, slot_index);

  return ISA_EXEC_MAX;
}


UINT32
TI_ISA_Exec_Stop_Mask (INT bundle)
{
  FmtAssert(FALSE, ("Not supported\n"));
  if (bundle < ISA_MAX_BUNDLES)
    return ISA_EXEC_Stop_Mask(bundle);

  return 0;
}

const char *
TI_ISA_Exec_Asmname (INT bundle)
{
  return (xtarch ? xtarch->bundle_name(bundle) :
	  ISA_EXEC_AsmName(bundle));
}


TOP TI_ISA_Noop_Top(ISA_EXEC_UNIT_PROPERTY unit)
{
  if (xtarch) {
    int count = 0;
    for (int format = 0; format < TI_ISA_Num_Bundles(); format++) {
      int slots = TI_ISA_Num_Slots(format);
      int mask = (1<<(count+slots)) - (1<<count);
      if ((mask & unit) != 0) {
	for (int i = 0; i < slots; i++) {
	  mask = 1<<(count+i);
	  if ((mask & unit) != 0)
	    return xtarch->noop_opcode(format, i);
	}
      }
      count += slots;
    }
  }

  return TOP_nop_n;
}

/* =============================================================

   Hazards.

   =========================================================== */

BOOL
TI_ISA_Top_Has_Hazard (TOP topcode)
{
  if (topcode < TOP_count)
    return ISA_HAZARD_TOP_Has_Hazard(topcode);
  else
    return FALSE;
}


ISA_HAZARD_INFO *
TI_ISA_Hazard_First (TOP topcode)
{
  if (topcode < TOP_count)
    return ISA_HAZARD_First(topcode);
  else
    return (ISA_HAZARD_INFO*)NULL;
}


ISA_HAZARD_INFO *
TI_ISA_Hazard_Next (ISA_HAZARD_INFO *info)
{
  return ISA_HAZARD_Next(info);
}


ISA_HAZARD
TI_ISA_Hazard_Type (ISA_HAZARD_INFO *info)
{
  return ISA_HAZARD_Type(info);
}


INT
TI_ISA_Hazard_Data (ISA_HAZARD_INFO *info)
{
  return ISA_HAZARD_Data(info);
}


INT
TI_ISA_Hazard_Pre_Ops (ISA_HAZARD_INFO *info)
{
  return ISA_HAZARD_Pre_Ops(info);
}


INT
TI_ISA_Hazard_Post_Ops (ISA_HAZARD_INFO *info)
{
  return ISA_HAZARD_Post_Ops(info);
}


/* =============================================================

   Print.

   =========================================================== */

const ISA_PRINT_INFO *
TI_ISA_Print_Info (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return ISA_PRINT_Info(topcode);

  if (xtarch)
  {
    XT_Instruction_p inst = xtarch->find_instruction(topcode);
    if (inst)
      return inst->print_info();
  }

  FmtAssert(FALSE, ("Topcode %d has no print information\n", topcode));
  return NULL;
}


INT
TI_ISA_Print_Info_Comp (const ISA_PRINT_INFO *info, INT index)
{
  return ISA_PRINT_INFO_Comp(info, index);
}


const char *
TI_ISA_Print_Info_Format (const ISA_PRINT_INFO *info)
{
  return ISA_PRINT_INFO_Format(info);
}


const char *
TI_ISA_Print_Asmname (TOP topcode)
{
  Is_True(topcode < TI_TOP_Count(), ("unexpected topcode %d\n", topcode));

  if (topcode < TOP_count)
    return ISA_PRINT_AsmName(topcode);

  return TI_TOP_Name(topcode);
}


BOOL
TI_ISA_Print_Operand_Is_Part_Of_Name (TOP topcode, INT opindex)
{
  Is_True(topcode < TOP_count, ("only supported for core topcodes, not %d", topcode));
  return ISA_PRINT_Operand_Is_Part_Of_Name(topcode, opindex);
}


/* =============================================================

   TIE.

   =========================================================== */

xtensa_isa
TI_TIE_Libisa_Info (void)
{
  return libisa_info;
}


xtensa_tie
TI_TIE_Xtie_Info (void)
{
  return xtie_info;
}


xtie_phase
TI_TIE_Xtie_Post_Rewrite (void)
{
  Lmt_DevWarn(1, ("Needed Post Rewrite TIE"));
  return xtie_post_rewrite;
}


xtie_phase
TI_TIE_Xtie_Compiler (void)
{
  return xtie_compiler;
}


BOOL 
TI_TIE_Need_Post_Rewrite_TIE (void)
{
  static bool checked = false;
  static bool need = false;
  if (checked)
      return need;

  /* Search for the item that serves as a marker 
     we need to use the post-rewrite tie to examine fusions.
     It's contents are never actually used.
  */
  xtensa_tie tie = TI_TIE_Xtie_Info();
  xtie_phase parse = xtie_get_compiler_phase(tie);
  xtie_function * function;
  xtie_foreach_function(parse, function) {
    /* any at all here imply that we need it */
    need = true;
  } end_xtie_foreach_function;
  
  checked = true;
  return need;
}


UINT
TI_TIE_Num_Macros (void)
{
  return xtarch ? xtarch->num_macros() : 0;
}


UINT
TI_TIE_Num_Ctypes (void)
{
  return xtarch ? xtarch->num_ctypes() : 0;
}


void
TI_TIE_Mtypes_Init (void)
{
  mtype_to_regclass = CXX_NEW_ARRAY(ISA_REGCLASS,
				    Mtype_Last + TI_TIE_Num_Ctypes() + 1,
				    &libti_pool);
  mtype_to_regsize = CXX_NEW_ARRAY(INT,
				    Mtype_Last + TI_TIE_Num_Ctypes() + 1,
				    &libti_pool);
  regclass_to_mtype = CXX_NEW_ARRAY(TYPE_ID,
				    TI_ISA_Num_Regclasses(),
				    &libti_pool);
  for (TYPE_ID i = 0;
       i < (Mtype_Last + TI_TIE_Num_Ctypes() + 1);
       i++)
  {
    mtype_to_regclass[i] = ISA_REGCLASS_UNDEFINED;
    mtype_to_regsize[i] = 1;
  }

  for (int rc_index = 0; rc_index < TI_ISA_Num_Regclasses(); rc_index++)
  {
    regclass_to_mtype[rc_index] = MTYPE_UNKNOWN;
  }

  regclass_to_mtype[TI_ISA_Regclass_Integer()] = MTYPE_U4;
  regclass_to_mtype[TI_ISA_Regclass_Branch()] = MTYPE_XTBOOL;

  /* create mtype for each ctype... */

  xtensa_isa isa = TI_TIE_Libisa_Info();
  for (TIE_TYPE_ID i = 0; i < TI_TIE_Num_Ctypes(); i++)
  {
    xtensa_ctype ct = i;
    const char* ct_name = xtensa_ctype_name(isa, ct);
    if (xtensa_ctype_is_builtin(isa, ct)==0 &&
	strncmp(ct_name, "_TIE_xtbool", 11) != 0)
    {

      UINT32 byte_alignment = (xtensa_ctype_alignment(isa, ct)+7)/8;
      UINT32 bit_alignment = byte_alignment*8;
      UINT32 bit_size = bit_alignment*((xtensa_ctype_num_bits(isa, ct)+
			      bit_alignment-1)/bit_alignment);

      Mtype_add(++Mtype_Last,
		bit_size, bit_size, 1,
		byte_alignment, byte_alignment, byte_alignment,
		0, 0, 0, ct_name,
		MTYPE_CLASS_TIE, 0,
		MTYPE_UNKNOWN, MTYPE_UNKNOWN, MTYPE_UNKNOWN);

      xtensa_regfile xrf = xtensa_ctype_regfile(isa, ct);
      const char* xrf_name = xtensa_regfile_shortname(isa,xrf);
      XT_RegFile_p rf = xtarch->find_regfile(xrf_name);
      FmtAssert(rf != NULL, ("can't find regfile %s for type %s",
			     xrf_name, ct_name));
      ISA_REGCLASS rc = rf->regclass();
      mtype_to_regclass[Mtype_Last] = rc;
      mtype_to_regsize[Mtype_Last] = xtensa_ctype_num_regs(isa, ct);

      if (xtensa_regfile_default_ctype(isa,xrf)==ct)
	regclass_to_mtype[(int)rc]=Mtype_Last;
      else if (regclass_to_mtype[(int)rc]==MTYPE_UNKNOWN &&
	       xtensa_ctype_num_bits(isa,ct)==rf->bit_size())
	regclass_to_mtype[(int)rc]=Mtype_Last;
    } else {
      Num_Built_In++;	// used by common/com/tietypes.h
    }

    FmtAssert(Mtype_Last <= 100, ("Too many (>100) TIE types"));
  }

  ISA_REGCLASS br_rc = TI_ISA_Regclass_Branch();

  mtype_to_regclass[MTYPE_XTBOOL] = br_rc;
  mtype_to_regclass[MTYPE_XTBOOL2] = br_rc;
  mtype_to_regclass[MTYPE_XTBOOL4] = br_rc;
  mtype_to_regclass[MTYPE_XTBOOL8] = br_rc;
  mtype_to_regclass[MTYPE_XTBOOL16] = br_rc;

  mtype_to_regsize[MTYPE_XTBOOL] = 1;
  mtype_to_regsize[MTYPE_XTBOOL2] = 2;
  mtype_to_regsize[MTYPE_XTBOOL4] = 4;
  mtype_to_regsize[MTYPE_XTBOOL8] = 8;
  mtype_to_regsize[MTYPE_XTBOOL16] = 16;

  if (tie_info)
    tie_info->init_macro_output_mtypes();

}


ISA_SIMD_TYPE_INFO
TI_TIE_First_Simd_Type_Info (void)
{
  xtie_phase xp = TI_TIE_Xtie_Compiler();
  if (!xp)
    return NULL;

  for (xtie_property_iter it = xtie_get_property_iter(xp);
       it; it = xtie_property_iter_step(it)) {
    xtie_property p = xtie_property_iter_get(it);
    if (xtie_property_get_type(p) == XTIE_PROPERTY_SIMD_CTYPE)
      return it;
  }

  return NULL;
}


ISA_SIMD_TYPE_INFO
TI_TIE_Next_Simd_Type_Info (ISA_SIMD_TYPE_INFO current)
{
  if (!current)
    return NULL;

  for (xtie_property_iter it = xtie_property_iter_step(current);
       it; it = xtie_property_iter_step(it)) {
    xtie_property p = xtie_property_iter_get(it);
    if (xtie_property_get_type(p) == XTIE_PROPERTY_SIMD_CTYPE) {

      bool repeat = false;
      xtie_phase xp = TI_TIE_Xtie_Compiler();
      for (xtie_property_iter jt = xtie_get_property_iter(xp);
	   jt; jt = xtie_property_iter_step(jt)) {

	if (jt==it)
	  break;

	xtie_property q = xtie_property_iter_get(jt);
	if (xtie_property_get_type(q) == XTIE_PROPERTY_SIMD_CTYPE) {
	  if (!strcmp(xtie_property_get_arg_id(q,0), xtie_property_get_arg_id(p,0)) &&
	      !strcmp(xtie_property_get_arg_id(q,1), xtie_property_get_arg_id(p,1)) &&
	      xtie_property_get_arg_int(q,2) == xtie_property_get_arg_int(p,2)) {
	    repeat = true;
	    break;
	  }
	}
      }

      if (!repeat)
	return it;
    }

  }
  
  return NULL;
}


unsigned int
TI_ISA_Simd_Type_Vl (ISA_SIMD_TYPE_INFO isi)
{
  xtie_property p = xtie_property_iter_get(isi);
  return xtie_property_get_arg_int(p, 2);
}


TYPE_ID
TI_ISA_Simd_Base_Mtype (ISA_SIMD_TYPE_INFO isi)
{
  xtie_property p = xtie_property_iter_get(isi);
  const char *base_name = xtie_property_get_arg_id(p, 1);
  const char *mangled_name = tie_info->mangle_ctype_name(base_name);
  TYPE_ID mtype = tie_info->mtype_id(mangled_name);
  Is_True(mtype != MTYPE_UNKNOWN, ("Can't find mtype for %s", base_name));
  return mtype;
}


TYPE_ID
TI_ISA_Simd_Vector_Register_Mtype (ISA_SIMD_TYPE_INFO isi)
{
  xtie_phase xp = TI_TIE_Xtie_Compiler();
  if (!xp)
    return MTYPE_UNKNOWN;
  
  xtie_property p = xtie_property_iter_get(isi);
  const char *mname = xtie_property_get_arg_id(p, 0);
  
  /* Check for type promotion */
  for (xtie_property_iter it = xtie_get_property_iter(xp);
       it; it = xtie_property_iter_step(it)) {
    xtie_property p = xtie_property_iter_get(it);
    if (xtie_property_get_type(p) != XTIE_PROPERTY_SIMD_PROMO)
      continue;
    
    const char *from_name = xtie_property_get_arg_id(p, 0);
    if (strcmp(from_name, mname))
      continue;
    
    const char *to_name = xtie_property_get_arg_id(p, 1);
    TYPE_ID mtype = tie_info->mtype_id(tie_info->mangle_ctype_name(to_name));
    Is_True(mtype != MTYPE_UNKNOWN, ("Can't find mtype for %s", to_name));
    return mtype;
  }
  
  TYPE_ID mtype = tie_info->mtype_id(tie_info->mangle_ctype_name(mname));
  Is_True(mtype != MTYPE_UNKNOWN, ("Can't find mtype for %s", mname));
  return mtype;
}


TYPE_ID
TI_ISA_Simd_Vector_Memory_Mtype (ISA_SIMD_TYPE_INFO isi)
{
  xtie_property p = xtie_property_iter_get(isi);
  const char *mname = xtie_property_get_arg_id(p, 0);
  TYPE_ID mtype = tie_info->mtype_id(tie_info->mangle_ctype_name(mname));
  Is_True(mtype != MTYPE_UNKNOWN, ("Can't find mtype for %s", mname));
  return mtype;
}


struct TIE_MACRO_LIST
{
  TIE_MACRO_p _macro;
  TIE_MACRO_LIST *_next;

  TIE_MACRO_LIST (TIE_MACRO_p macro, TIE_MACRO_LIST *next) :
    _macro(macro), _next(next)
  { }
};

typedef UTL_Map<const char *, TIE_MACRO_LIST *, UTL_Map_StringNoCaseHash> SIMD_PROTO_MAP;
static SIMD_PROTO_MAP *simd_proto_map = NULL;

typedef UTL_Map<TYPE_ID, TYPE_ID, UTL_Map_IntHash> ALIGN_TYPE_MAP;
static ALIGN_TYPE_MAP *align_type_map = NULL;

typedef UTL_Map<TYPE_ID, TYPE_ID, UTL_Map_IntHash> VSEL_TYPE_MAP;
static VSEL_TYPE_MAP  *vsel_type_map = NULL;

static INT             vsel_max_way  = 0;

static void
init_simd_intrinsics (void)
{
  if (!simd_proto_map || !align_type_map)
  {
    simd_proto_map = CXX_NEW(SIMD_PROTO_MAP(&libti_pool), &libti_pool);
    align_type_map = CXX_NEW(ALIGN_TYPE_MAP(&libti_pool, 63), &libti_pool);
    vsel_type_map = CXX_NEW(VSEL_TYPE_MAP(&libti_pool, 63), &libti_pool);

    /* For each simd op property, initialize a map entry from
       the base operation to the list of TIE_MACROs representing the
       vector operation. We use a string for the base operation since
       it may be either a builtin or a proto. */
    xtie_phase xp = TI_TIE_Xtie_Compiler();
    if (!xp)
      return;

    for (xtie_property_iter it = xtie_get_property_iter(xp);
         it; it = xtie_property_iter_step(it))
    {
      xtie_property prop = xtie_property_iter_get(it);
      if (xtie_property_get_type(prop) != XTIE_PROPERTY_SIMD_PROTO)
        continue;
      
      const char *vname_nonmangled = xtie_property_get_arg_id(prop, 0);
      const char *vname = tie_info->mangle_macro_name(vname_nonmangled);
      TIE_MACRO_p macro = tie_info->tie_macro(vname);
      
      /* Sometimes TIE info fails to initialize so there's no guarantee that
         we can find the macro. */
      if (!macro)
        continue;
      
      const char *bname_nonmangled = xtie_property_get_arg_id(prop, 1);
      const char *bname = tie_info->mangle_macro_name(bname_nonmangled);
      
      TIE_MACRO_LIST *macro_list;
      if (!simd_proto_map->find(bname, &macro_list))
      {
	simd_proto_map->insert(bname, NULL);
	macro_list = NULL;
      }
      
      macro_list = CXX_NEW(TIE_MACRO_LIST(macro, macro_list), &libti_pool);
      simd_proto_map->setValue(bname, macro_list);

      /* If 'base_proto' is an alignment load prime or store flush builtin,
         then we use it to record the relationship of an alignment
         type with the non-alignment type. */
      if (!strcmp(bname, "xt_loadap") ||
	  !strcmp(bname, "xt_storeaf"))
      {
	FmtAssert(macro->num_protos() == 2,
		  ("expecting 2 operands to alignment load prime/store flush"));
	FmtAssert(macro->proto_is_pointer(1),
		  ("expecting pointer operand 1 for alignment load prime/store flush"));

	TYPE_ID align_mtype = macro->proto_mtype_id(tie_info, 0);
	/* proto_mtype_id(tie_info, 1) will strip off the '*' */
	TYPE_ID non_align_mtype = macro->proto_pointed_mtype_id(tie_info, 1);
	if (!align_type_map->find(non_align_mtype))
	  align_type_map->insert(non_align_mtype, align_mtype);
      }

      /* If 'base_proto' is a VSEL builtin,
         then we use it to record the relationship of a select
         type with the register type */
      if (!strcmp(bname, "xt_vsel"))
      {
        int vl = xtie_property_get_arg_int(prop, 2);

        /* the max number of fields the select can handle is
           used to compute the size of each field id:
           
           size_of(field_id) = ceillog2(vsel_max_way)
           size_of(select_register) = vsel_max_way * size_of(field_id)
        */
        int cur_way = (macro->proto_is_out(1)) ? vl : (2 * vl);
        
        if (cur_way > vsel_max_way) {
          vsel_max_way = cur_way;
        }
        
        INT last_idx = macro->num_protos() - 1;
        FmtAssert(macro->proto_is_out(0),
                  ("expecting first operand to be out for VSEL"));
        FmtAssert(macro->proto_is_in(last_idx),
                  ("expecting last operand to be in for VSEL"));
        
        if (!macro->proto_is_immed(last_idx)) {
          /* grep the type from here */
          TYPE_ID vsel_type = macro->proto_mtype_id(tie_info, last_idx);
          TYPE_ID vec_type = macro->proto_mtype_id(tie_info, 0);
          if (!vsel_type_map->find(vec_type)) {
            vsel_type_map->insert(vec_type, vsel_type);
          }
        }
      }
    }
  }
}


TYPE_ID
TI_ISA_Alignment_Mtype (TYPE_ID simd_mem_type)
{
  init_simd_intrinsics();

  TYPE_ID align;
  if (align_type_map->find(simd_mem_type, &align))
    return align;

  return MTYPE_UNKNOWN;
}

TYPE_ID
TI_ISA_Vsel_Mtype (TYPE_ID vec_type)
{
  init_simd_intrinsics();

  TYPE_ID vsel;
  if (vsel_type_map->find(vec_type, &vsel))
    return vsel;

  return MTYPE_UNKNOWN;
}

INT
TI_ISA_Vsel_Max_Way()
{
  init_simd_intrinsics();
  return vsel_max_way;
}

static const char *
operator_to_builtin (OPERATOR oper)
{
  switch (oper)
  {
  case OPR_ABS: return "xt_abs";
  case OPR_ADD: return "xt_add";
  case OPR_ASHR: return "xt_ashr";
  case OPR_BAND: return "xt_band";
  case OPR_BIOR: return "xt_bor";
  case OPR_BNOR: return "xt_bnor";
  case OPR_BNOT: return "xt_bnot";
  case OPR_BXOR: return "xt_bxor";
  case OPR_EQ: return "xt_eq";
  case OPR_GE: return "xt_ge";
  case OPR_GT: return "xt_gt";
  case OPR_LAND: return "xt_land";
  case OPR_LE: return "xt_le";
  case OPR_LIOR: return "xt_lor";
  case OPR_LNOT: return "xt_lnot";
  case OPR_LSHR: return "xt_lshr";
  case OPR_LT: return "xt_lt";
  case OPR_MADD: return "xt_madd";
  case OPR_MAX: return "xt_max";
  case OPR_MIN: return "xt_min";
  case OPR_MPY: return "xt_mpy";
  case OPR_MSUB: return "xt_msub";
  case OPR_NE: return "xt_ne";
  case OPR_NEG: return "xt_neg";
  case OPR_RECIP: return "xt_recip";
  case OPR_RSQRT: return "xt_rsqrt";
  case OPR_SHL: return "xt_shl";
  case OPR_SUB: return "xt_sub";
  }

  return NULL;
}


static const char *
operator_to_builtin (TI_OPERATOR oper)
{
  switch (oper)
  {
  case TI_OPR_VSEL:      return "xt_vsel";
  case TI_OPR_LOADA:     return "xt_loada";
  case TI_OPR_LOADAU:    return "xt_loadau";
  case TI_OPR_LOADAP:    return "xt_loadap";
  case TI_OPR_STOREA:    return "xt_storea";
  case TI_OPR_STOREAU:   return "xt_storeau";
  case TI_OPR_STOREAP:   return "xt_storeap";
  case TI_OPR_STOREAF:   return "xt_storeaf";
  case TI_OPR_LS_U:      return "xt_lsiu";
  case TI_OPR_LSX:       return "xt_lsx";
  case TI_OPR_LSX_U:     return "xt_lsxu";
  case TI_OPR_LV_U:      return "xt_lviu";
  case TI_OPR_LVX:       return "xt_lvx";
  case TI_OPR_LVX_U:     return "xt_lvxu";
  case TI_OPR_MPY_EVEN:  return "xt_mpy_even";
  case TI_OPR_MPY_ODD:   return "xt_mpy_odd";
  case TI_OPR_MADD_EVEN: return "xt_madd_even";
  case TI_OPR_MADD_ODD:  return "xt_madd_odd";
  case TI_OPR_MSUB_EVEN: return "xt_msub_even";
  case TI_OPR_MSUB_ODD:  return "xt_msub_odd";
  case TI_OPR_SS_U:      return "xt_ssiu";
  case TI_OPR_SV_U:      return "xt_sviu";
  case TI_OPR_SVX:       return "xt_svx";
  case TI_OPR_SVX_U:     return "xt_svxu";
  case TI_OPR_WVSAR:     return "xt_wvsar";
  case TI_OPR_WROUND:    return "xt_wround";
  case TI_OPR_MULR:      return "xt_mulr";
  case TI_OPR_MULR_EVEN: return "xt_mulr_even";
  case TI_OPR_MULR_ODD:  return "xt_mulr_odd";
  case TI_OPR_RADD:      return "xt_radd";
  case TI_OPR_RMIN:      return "xt_rmin";
  case TI_OPR_RMAX:      return "xt_rmax";
  case TI_OPR_ZERO:      return "xt_zero";
  case TI_OPR_VPACK:     return "xt_vpack";
  case TI_OPR_MOVT:      return "xt_movt";
  case TI_OPR_MOVF:      return "xt_movf";
  }

  return NULL;
}


static bool
compare_operator (TIE_MACRO_p macro, unsigned int num_operands,
		  TI_DIR *dirs, TYPE_ID *operand_types, INT64 *immediates =NULL,
		  BOOL *pointers =NULL, BOOL *labels =NULL)
{
  if (macro->num_protos() != num_operands)
    return false;

  for (unsigned int i = 0; i < num_operands; i++)
   {
     if (operand_types[i] == MTYPE_UNKNOWN)
     {
       if (!immediates || !macro->proto_is_immed(i) ||
	   !macro->immediate_ok(i, immediates[i]))
	 return false;
     } else {
       if (macro->proto_is_immed(i))
	 return false;

       bool pointer = pointers != NULL && pointers[i];
       if (pointer != macro->proto_is_pointer(i))
	 return false;

       TYPE_ID proto_type;
       if (pointer)
         proto_type = macro->proto_pointed_mtype_id(tie_info, i);
       else
         proto_type = macro->proto_mtype_id(tie_info, i);
       if (operand_types[i] != proto_type)
	 return false;
     }

     if ((!labels && macro->proto_is_label(i)) ||
	 (labels && (labels[i] != macro->proto_is_label(i))))
       return false;

     if ((macro->proto_is_inout(i) && (dirs[i] != TI_DIR_INOUT)) ||
	 (macro->proto_is_in(i) && (dirs[i] != TI_DIR_IN)) ||
	 (macro->proto_is_out(i) && (dirs[i] != TI_DIR_OUT)))
       return false;
   }

  return true;
}


static bool
compare_operator (TIE_MACRO_p macro, OPERATOR oper, TYPE_ID rtype, TYPE_ID desc)
{
#define MAX_COMPARATOR_OPERANDS 16

  TI_DIR dir[MAX_COMPARATOR_OPERANDS];
  TYPE_ID operands[MAX_COMPARATOR_OPERANDS];
  unsigned int num_operands = 0;

  switch (oper)
  {
  case OPR_ASHR:
  case OPR_LSHR:
  case OPR_SHL:
    operands[0] = operands[1] = rtype;
    operands[2] = MTYPE_I4;
    dir[0] = TI_DIR_OUT;
    dir[1] = dir[2] = TI_DIR_IN;
    num_operands = 3;
    break;

  case OPR_ABS:
  case OPR_BNOT:
  case OPR_LNOT:
  case OPR_NEG:
  case OPR_RECIP:
  case OPR_RSQRT:
    operands[0] = operands[1] = rtype;
    dir[0] = TI_DIR_OUT;
    dir[1] = TI_DIR_IN;
    num_operands = 2;
    break;

  case OPR_ADD:
  case OPR_BAND:
  case OPR_BIOR:
  case OPR_BNOR:
  case OPR_BXOR:
  case OPR_LAND:
  case OPR_LIOR:
  case OPR_MAX:
  case OPR_MIN:
  case OPR_SUB:
    operands[0] = operands[1] = operands[2] = rtype;
    dir[0] = TI_DIR_OUT;
    dir[1] = dir[2] = TI_DIR_IN;
    num_operands = 3;
    break;

  case OPR_MPY:
    operands[0] = rtype;
    operands[1] = operands[2] = desc;
    dir[0] = TI_DIR_OUT;
    dir[1] = dir[2] = TI_DIR_IN;
    num_operands = 3;
    break;

  case OPR_MADD:
  case OPR_MSUB:
    operands[0] = rtype;
    operands[1] = operands[2] = desc;
    dir[0] = TI_DIR_INOUT;
    dir[1] = dir[2] = TI_DIR_IN;
    num_operands = 3;
    break;

  case OPR_EQ:
  case OPR_GE:
  case OPR_GT:
  case OPR_LE:
  case OPR_LT:
  case OPR_NE:
    operands[0] = rtype;
    operands[1] = operands[2] = desc;
    dir[0] = TI_DIR_OUT;
    dir[1] = dir[2] = TI_DIR_IN;
    num_operands = 3;
    break;

  default:
    return false;
  }

  return compare_operator(macro, num_operands, dir, operands);
}


static INT64
get_scalar_stride (TYPE_ID type)
{
  /* The stride for a scalar type is the number of bytes it occupies
     extended to the next possible alignment. */
  INT req_align = MTYPE_alignment(type);
  INT align = req_align;
  while (align < MTYPE_byte_size(type))
    align <<= 1;
  return align;
}


static INT64
get_vector_stride (TYPE_ID reg_type, TYPE_ID mem_type)
{
  /* Determine the vector stride by multiplying the vector length
     by the stride of the scalar type. */
  ISA_SIMD_TYPE_INFO isi;
  FOR_ALL_ISA_SIMD_TYPE_INFO(isi)
    {
      if (TI_ISA_Simd_Vector_Register_Mtype(isi) != reg_type ||
          TI_ISA_Simd_Vector_Memory_Mtype(isi) != mem_type)
        continue;
      
      TYPE_ID scalar = TI_ISA_Simd_Base_Mtype(isi);
      unsigned int vl = TI_ISA_Simd_Type_Vl(isi);
      
      INT64 scalar_stride = get_scalar_stride(scalar);
      INT64 stride = scalar_stride * vl;
      return stride;
    }
  
  return 0;
}


static bool
compare_operator (TIE_MACRO_p macro, TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc)
{
#define MAX_COMPARATOR_OPERANDS 16

  TI_DIR dir[MAX_COMPARATOR_OPERANDS];
  TYPE_ID operands[MAX_COMPARATOR_OPERANDS];
  INT64 immeds_buf[MAX_COMPARATOR_OPERANDS];
  INT64 *immeds = NULL;
  BOOL pointers_buf[MAX_COMPARATOR_OPERANDS];
  BOOL *pointers = NULL;
  unsigned int num_operands = 0;

  switch (oper)
  {
  case TI_OPR_VSEL:
    operands[0] = operands[1] = operands[2] = rtype;
    /* FIXME we should find the type of the select register for
       'rtype' and use that as 'operands[3]'. That way we will search
       for a select that uses the appropriate select register. */
    // operands[3] = select_register_mtype(rtype);
    return false;    // for now...
    dir[0] = TI_DIR_OUT;
    dir[1] = dir[2] = dir[3] = TI_DIR_IN;
    num_operands = 4;
    break;

  case TI_OPR_LOADA:
  case TI_OPR_LOADAU:
  {
    TYPE_ID align_mtype = TI_ISA_Alignment_Mtype(desc);
    if (align_mtype == MTYPE_UNKNOWN)
      return false;

    operands[0] = rtype;
    operands[1] = align_mtype;
    operands[2] = desc;
    operands[3] = MTYPE_UNKNOWN;
    immeds = immeds_buf;
    immeds[0] = immeds[1] = immeds[2] = 0;
    immeds[3] = ((oper == TI_OPR_LOADA) ? 0 : get_vector_stride(rtype, desc));
    pointers = pointers_buf;
    pointers[0] = pointers[1] = pointers[3] = false;
    pointers[2] = true;
    dir[0] = TI_DIR_OUT;
    dir[1] = TI_DIR_INOUT;
    dir[2] = ((oper == TI_OPR_LOADA) ? TI_DIR_IN : TI_DIR_INOUT);
    dir[3] = TI_DIR_IN;
    num_operands = 4;
  }
  break;

  case TI_OPR_LOADAP:
  {
    TYPE_ID align_mtype = TI_ISA_Alignment_Mtype(desc);
    if (align_mtype == MTYPE_UNKNOWN)
      return false;

    operands[0] = align_mtype;
    operands[1] = desc;
    pointers = pointers_buf;
    pointers[0] = false;
    pointers[1] = true;
    dir[0] = TI_DIR_OUT;
    dir[1] = TI_DIR_INOUT;
    num_operands = 2;
  }
  break;

  case TI_OPR_STOREA:
  case TI_OPR_STOREAU:
  {
    TYPE_ID align_mtype = TI_ISA_Alignment_Mtype(desc);
    if (align_mtype == MTYPE_UNKNOWN)
      return false;

    operands[0] = rtype;
    operands[1] = align_mtype;
    operands[2] = desc;
    operands[3] = MTYPE_UNKNOWN;
    immeds = immeds_buf;
    immeds[0] = immeds[1] = immeds[2] = 0;
    immeds[3] = ((oper == TI_OPR_STOREA) ? 0 : get_vector_stride(rtype, desc));
    pointers = pointers_buf;
    pointers[0] = pointers[1] = pointers[3] = false;
    pointers[2] = true;
    dir[0] = dir[3] = TI_DIR_IN;
    dir[1] = TI_DIR_INOUT;
    dir[2] = ((oper == TI_OPR_STOREA) ? TI_DIR_IN : TI_DIR_INOUT);
    num_operands = 4;
  }
  break;

  case TI_OPR_STOREAP:
  {
    TYPE_ID align_mtype = TI_ISA_Alignment_Mtype(desc);
    if (align_mtype == MTYPE_UNKNOWN)
	return false;

    operands[0] = align_mtype;
    dir[0] = TI_DIR_OUT;
    num_operands = 1;
  }
  break;

  case TI_OPR_STOREAF:
  {
    TYPE_ID align_mtype = TI_ISA_Alignment_Mtype(desc);
    if (align_mtype == MTYPE_UNKNOWN)
      return false;

    operands[0] = align_mtype;
    operands[1] = desc;
    pointers = pointers_buf;
    pointers[0] = false;
    pointers[1] = true;
    dir[0] = TI_DIR_IN;
    dir[1] = TI_DIR_IN;
    num_operands = 2;
  }
  break;

  case TI_OPR_LS_U:
  case TI_OPR_SS_U:
  case TI_OPR_LV_U:
  case TI_OPR_SV_U:
  {
      operands[0] = rtype;
      operands[1] = desc;
      operands[2] = MTYPE_UNKNOWN;
      immeds = immeds_buf;
      immeds[0] = immeds[1] = 0;
      immeds[2] = ((oper == TI_OPR_LS_U || oper == TI_OPR_SS_U) ?
                   get_scalar_stride(desc) : get_vector_stride(rtype, desc));
      pointers = pointers_buf;
      pointers[0] = false;
      pointers[1] = true;
      dir[0] = (oper == TI_OPR_LS_U || oper == TI_OPR_LV_U) ? TI_DIR_OUT : TI_DIR_IN;
      dir[1] = TI_DIR_INOUT;
      dir[2] = TI_DIR_IN;
      num_operands = 3;
      break;
  }

  case TI_OPR_SVX:
  case TI_OPR_SVX_U:
  {
      operands[0] = rtype;
      operands[1] = desc;
      operands[2] = MTYPE_I4;
      pointers = pointers_buf;
      pointers[0] = false;
      pointers[1] = true;
      pointers[2] = false;
      dir[0] = TI_DIR_IN;
      dir[1] = (oper == TI_OPR_SVX_U) ? TI_DIR_INOUT : TI_DIR_IN;
      dir[2] = TI_DIR_IN;
      num_operands = 3;
      break;
  }

  case TI_OPR_LSX:
  case TI_OPR_LSX_U:
  case TI_OPR_LVX:
  case TI_OPR_LVX_U:
  {
      operands[0] = rtype;
      operands[1] = desc;
      operands[2] = MTYPE_I4;
      pointers = pointers_buf;
      pointers[0] = false;
      pointers[1] = true;
      pointers[2] = false;
      dir[0] = TI_DIR_OUT;
      dir[1] = ((oper == TI_OPR_LSX_U) || (oper == TI_OPR_LVX_U))
	  ? TI_DIR_INOUT : TI_DIR_IN;
      dir[2] = TI_DIR_IN;
      num_operands = 3;
      break;
  }

  case TI_OPR_WVSAR:
  case TI_OPR_WROUND:
  {
      operands[0] = MTYPE_U4;
      dir[0] = TI_DIR_IN;
      num_operands = 1;
  }
  break;

  case TI_OPR_RADD:
  case TI_OPR_RMIN:
  case TI_OPR_RMAX:
  {
      operands[0] = rtype;
      operands[1] = desc;
      dir[0] = TI_DIR_OUT;
      dir[1] = TI_DIR_IN;
      num_operands = 2;
  }
  break;

  case TI_OPR_ZERO:
  {
    operands[0] = rtype;
    dir[0] = TI_DIR_OUT;
    num_operands = 1;
  }
  break;

  default:
    return false;
  }

  return compare_operator(macro, num_operands, dir, operands,
			  immeds, pointers);
}


const char *
TI_ISA_Operator_Intrinsic (OPERATOR oper, TYPE_ID rtype, TYPE_ID desc)
{
  /* We initialize the information needed for this interface lazily,
     since most compilations won't need it. */
  init_simd_intrinsics();

  /* Find the builtin operator for 'oper'. If there isn't one then we
     can't find an instrinsic. */
  const char *builtin = operator_to_builtin(oper);
  if (!builtin)
    return NULL;

  /* Search the list of candidate protos for one that matches the
     required 'rtype' and 'desc'. Return the first one we find... */
  TIE_MACRO_LIST *mlist;
  if (!simd_proto_map->find(builtin, &mlist))
    return NULL;

  for (; mlist; mlist = mlist->_next)
  {
    if (compare_operator(mlist->_macro, oper, rtype, desc))
      return mlist->_macro->name();
  }

  return NULL;
}


const char *
TI_ISA_TiOperator_Intrinsic (TI_OPERATOR oper, TYPE_ID rtype, TYPE_ID desc)
{
  /* We initialize the information needed for this interface lazily,
     since most compilations won't need it. */
  init_simd_intrinsics();

  /* Find the builtin operator for 'oper'. If there isn't one then we
     can't find an instrinsic. */
  const char *builtin = operator_to_builtin(oper);
  if (!builtin)
    return NULL;

  /* Search the list of candidate protos for one that matches the
     required 'rtype' and 'desc'. Return the first one we find... */
  TIE_MACRO_LIST *mlist;
  if (!simd_proto_map->find(builtin, &mlist))
    return NULL;

  for (; mlist; mlist = mlist->_next)
  {
    if (compare_operator(mlist->_macro, oper, rtype, desc))
      return mlist->_macro->name();
  }

  return NULL;
}


const char *
TI_ISA_Operator_Intrinsic_Exp (OPERATOR oper, unsigned int num_operands,
			       TI_DIR *operand_dirs, TYPE_ID *operand_types,
			       INT64 *immediates)
{
  /* Find the builtin operator for 'oper'. If there isn't one then we
     can't find an instrinsic. */
  const char *builtin = operator_to_builtin(oper);
  if (!builtin)
    return NULL;

  return TI_ISA_Named_Intrinsic_Exp(builtin, num_operands, operand_dirs,
				    operand_types, immediates);
}


const char *
TI_ISA_TiOperator_Intrinsic_Exp (TI_OPERATOR oper, unsigned int num_operands,
				 TI_DIR *operand_dirs, TYPE_ID *operand_types,
				 INT64 *immediates)
{
  /* Find the builtin operator for 'oper'. If there isn't one then we
     can't find an instrinsic. */
  const char *builtin = operator_to_builtin(oper);
  if (!builtin)
    return NULL;

  return TI_ISA_Named_Intrinsic_Exp(builtin, num_operands, operand_dirs,
				    operand_types, immediates);
}


const char *
TI_ISA_Named_Intrinsic_Exp (const char *oper, unsigned int num_operands,
			    TI_DIR *operand_dirs, TYPE_ID *operand_types,
			    INT64 *immediates)
{
  /* We initialize the information needed for this interface lazily,
     since most compilations won't need it. */
  init_simd_intrinsics();

  /* Search the list of candidate protos... */
  TIE_MACRO_LIST *mlist;
  if (!simd_proto_map->find(oper, &mlist))
    return NULL;

  for (; mlist; mlist = mlist->_next)
  {
    if (compare_operator(mlist->_macro, num_operands,
			 operand_dirs, operand_types, immediates))
      return mlist->_macro->name();
  }

  return NULL;
}


/* =============================================================

   Misc.

   =========================================================== */


BOOL
TI_Equivalent_Immed_Branch (TOP *intop, INT64 *inimm_val, BOOL flip)
{
  TOP top = *intop;
  INT64 imm_val = *inimm_val;

  /* Special cases for xtensa's bnez/beqz branches. We can check these
     before considering if we need to flip the branch, because equal
     and not-equal comparisons are commutative. */

  if (imm_val == 0)
  {
    if ((top == TOP_beq) || (top == TOP_beqi))
    {
      *intop = TOP_beqz;
      return TRUE;
    }
    else if ((top == TOP_bne) || (top == TOP_bnei))
    {
      *intop = TOP_bnez;
      return TRUE;
    }
  }

  /* If we need to flip the branch operands, and an identical branch
     doesn't exist, then try to adjust the immediate to find an
     identical branch. */

  if (flip)
  {
    const TOP oldtop = top;
    top = TI_TOP_Same_Branch_Flipped_Operands(oldtop);
    if (top == TOP_UNDEFINED)
    {
      /* By adjusting the literal by 1, we can use blt, for bge (where
	 we actually need ble), and vice-versa. */

      if (((oldtop == TOP_bge) || (oldtop == TOP_bgei)) && (imm_val < INT_MAX))
      {
	top = TOP_blt;
	imm_val++;
      }
      else if (((oldtop == TOP_bgeu) || (oldtop == TOP_bgeui)) && (imm_val < UINT_MAX))
      {
	top = TOP_bltu;
	imm_val++;
      }
      else if (((oldtop == TOP_blt) || (oldtop == TOP_blti)) && (imm_val < INT_MAX))
      {
	top = TOP_bge;
	imm_val++;
      }
      else if (((oldtop == TOP_bltu) || (oldtop == TOP_bltui)) && (imm_val < UINT_MAX))
      {
	top = TOP_bgeu;
	imm_val++;
      }
      else
      {
	return FALSE;
      }
    }
  }

  /* Special cases for xtensa's bgez/bltz branches. */

  if (imm_val == 0)
  {
    if ((top == TOP_bge) || (top == TOP_bgei))
    {
      *intop = TOP_bgez;
      return TRUE;
    }
    else if ((top == TOP_blt) || (top == TOP_blti))
    {
      *intop = TOP_bltz;
      return TRUE;
    }
  }
  else if (imm_val == 1)
  {
    if ((top == TOP_bltu) || (top == TOP_bltui))
    {
      *intop = TOP_beqz;
      return TRUE;
    }
    else if ((top == TOP_bgeu) || (top == TOP_bgeui))
    {
      *intop = TOP_bnez;
      return TRUE;
    }
  }

  /* Get the immediate version of 'top', and make sure the immediate
     fits within it's allowed range. */

  top = TI_TOP_Reg_To_Immed(top);
  if (top == TOP_UNDEFINED)
    return FALSE;

  const ISA_OPERAND_INFO *info = TI_ISA_Operand_Info(top);
  ISA_LITCLASS lc = TI_ISA_Valtyp_Litclass(TI_ISA_Op_Operand(info, 1));
  if (!TI_ISA_LC_Value_In_Class(imm_val, lc))
    return FALSE;

  *intop = top;
  *inimm_val = imm_val;
  return TRUE;
}

/* =============================================================

   Scheduling.

   =========================================================== */

extern BOOL
TI_SI_Check_Resource_Ok (TOP topcodes[], INT num_tops) {

  if (num_tops<2) return TRUE;

  if (xtarch==NULL) return FALSE;

  MEM_POOL_Push(&libti_pool);

  TI_RES_RES* res = TI_RES_RES_Alloc(0, &libti_pool);

  int max_cycles = 0;
  for (int i=0; i<num_tops; i++) {
    TOP top = topcodes[i];
    int length = TI_RES_Cycle_Count(top);
    if (length>max_cycles)
      max_cycles = length;
  }
  TI_RES_RES_Set_BB_Cycle_Count(res, max_cycles);

  bool ok = TRUE;
  for (int i=0; i<num_tops && ok; i++) {
    if (TI_RES_RES_Resources_Available(res, topcodes[i], 0, TOP_UNDEFINED))
      TI_RES_RES_Reserve_Resources(res, topcodes[i], 0, TOP_UNDEFINED);
    else
      ok = FALSE;
  }

  if (ok==TRUE) {
    for (int i=0; i<num_tops && ok; i++) {
      XT_Instruction_p inst_i = xtarch->find_instruction(topcodes[i]);
      if (inst_i==NULL) {
	ok = false;
	break;
      }
      int num_results_i = inst_i->num_results();
      for (int j=i+1; j<num_tops && ok; j++) {
	XT_Instruction_p inst_j = xtarch->find_instruction(topcodes[j]);
	if (inst_j==NULL) {
	  ok = false;
	  break;
	}
	int num_results_j = inst_j->num_results();

	for (int ii=0; ii<num_results_i && ok; ii++) {
	  XT_Operand_cp result_i = inst_i->result(ii);
	  XT_State_p state_i = result_i->state();
	  if (state_i==NULL)
	    continue;
	  for (int jj=0; jj<num_results_j && ok; jj++) {
	    XT_Operand_cp result_j = inst_j->result(jj);
	    XT_State_p state_j = result_j->state();
	    if (state_j==NULL)
	      continue;
	    /* not ok if two writes to the same state or two volatile writes */
	    if (state_i == state_j &&
		(!state_i->is_artificial() || state_i->is_volatile())) {
	      const char* state_name = state_i->name();
	      xtensa_interface xtif = xtensa_interface_lookup(libisa_info, state_name);
	      if (xtif==XTENSA_UNDEFINED || xtensa_interface_has_side_effect(libisa_info, xtif))
	        ok = false;
	      break;
	    }
	  }
	}
      }
    }
  }

  MEM_POOL_Pop(&libti_pool);

  return ok;

}

extern TOP TI_Convert_OP(TOP opcode, ISA_EXEC_UNIT_PROPERTY prop,
			     INT *opnds)
{
  TOP new_opcode = TOP_UNDEFINED;
  extern BOOL xt_density;

  if (xt_density) {

    switch (opcode)
    {
    case TOP_add:
      new_opcode = TOP_add_n;
      break;
    case TOP_add_n:
      new_opcode = TOP_add;
      break;

    case TOP_addi:
      if (TI_ISA_LC_Value_In_Class(opnds[1], LC_ai4const))
	new_opcode = TOP_addi_n;
      break;
    case TOP_addi_n:
      new_opcode = TOP_addi;
      break;

    case TOP_l32i:
      if (TI_ISA_LC_Value_In_Class(opnds[1], LC_lsi4x4))
	new_opcode = TOP_l32i_n;
      break;
    case TOP_l32i_n:
      new_opcode = TOP_l32i;
      break;

    case TOP_s32i:
      if (TI_ISA_LC_Value_In_Class(opnds[2], LC_lsi4x4))
	new_opcode = TOP_s32i_n;
      break;
    case TOP_s32i_n:
      new_opcode = TOP_s32i;
      break;

    case TOP_or:
      /* Is this TOP_or acting as a move... */
      if (opnds[0] != opnds[1])
	break;

      /* TOP_mov_n can't have source and destination registers the
	 same. Of course we shouldn't see that, and there is a
	 DevWarn elsewhere to flag it, so we don't need one
	 here. */
      if (opnds[2] != opnds[0])
	new_opcode = TOP_mov_n;
      break;
    case TOP_movi:
      if (TI_ISA_LC_Value_In_Class(opnds[0], LC_simm7))
	new_opcode = TOP_movi_n;
      break;
    case TOP_movi_n:
      new_opcode = TOP_movi;
      break;

    case TOP_nop:
      new_opcode = TOP_nop_n;
      break;
    case TOP_nop_n:
      new_opcode = TOP_nop;
      break;

    case TOP_ret:
      new_opcode = TOP_ret_n;
      break;
    case TOP_ret_n:
      new_opcode = TOP_ret;
      break;

    case TOP_retw:
      new_opcode = TOP_retw_n;
      break;
    case TOP_retw_n:
      new_opcode = TOP_retw;
      break;
    }
  }
  if (new_opcode != TOP_UNDEFINED) {
    ISA_EXEC_UNIT_PROPERTY new_prop = TI_ISA_Exec_Unit_Prop(new_opcode);
    if (new_prop & prop == 0)
      new_opcode = TOP_UNDEFINED;
  }
  return new_opcode;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
