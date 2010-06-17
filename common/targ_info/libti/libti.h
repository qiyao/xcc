/*

  Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.

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
 * Interface definition for the mongoose target-info library.
 */

#ifndef LIBTI_H
#define LIBTI_H

#include "defs.h"
#include "mtypes.h"
#include "cxx_memory.h"
#include "opcode_gen_core.h"
#include "xtensa-isa.h"
#include "xtensa-tie.h"
#include "errors.h"
#include "targ_isa_operands.h"	/* built in the targ_info directory */

#ifdef __cplusplus
#endif

  
/* The highest register classes allowed. To get the actual number of
   register classes for a target use TI_ISA_Num_Regclasses. */
#define TI_ISA_REGCLASS_MAX  63
  
/* The highest register in any class allowed. To get the
   actual maximum for a target use TI_ISA_Register_Max. */
#define TI_ISA_REGISTER_MAX  128
  
/* The highest register subclasses allowed. To get the actual number
   of register classes use TI_ISA_Num_Regsubclasses. */
#define TI_ISA_REGSUBCLASS_MAX  4095
  
/* The maximum number of explicit and implicit operands/results of an instruction */
#define TI_ISA_OPERANDS_MAX	ISA_OPERAND_max_operands_limit	/* from targ_isa_operands.h */
#define TI_ISA_RESULTS_MAX	ISA_OPERAND_max_results_limit	/* from targ_isa_operands.h */

/* The maximum number of operands of a TIE macro */
#define TI_TIE_OPERANDS_MAX	128

/* The maximum number of formats in a config */
#define TI_TIE_FORMATS_MAX	10

/* The maximum number of slots in any format */
#define TI_TIE_SLOTS_MAX	31
#define TI_TIE_SLOTS_MAX_BITS	5

/* The maximum number of operands in any inlined asm statement */
#define TI_ISA_ASM_OPERANDS_MAX	30

/* The maximum number of results in any inlined asm statement */
#define TI_ISA_ASM_RESULTS_MAX	10

/* =============================================================

   Initialization

   =========================================================== */

/* Initialize the target information. Return FALSE if unable to initialize. */
extern "C" BOOL Target_Information_Init (const char *dll,
                                         char **libisa_dlls,
                                         char **libtie_dlls);


/* =============================================================

   Topcodes

   =========================================================== */

#include "topcode.h"
  
/* Return the number of topcodes. */
extern INT TI_TOP_Count (void);

/* Return the topcode representing the given named 'opcode'. */
extern TOP TI_TOP_Topcode (const char *opcode);

/* remove 'opcode' from the list of pre-defined core instructions */
/* this is used when user TIE instruction over-writes core
 * instruction by name
 */
extern void TI_TOP_Topcode_Remove_From_Core (const char *opcode);

/* Returns an assembler style name for the given TOP. */
extern const char *TI_TOP_Name (TOP topcode);

/* Returns the minimum issue slot count for all valid formats of the
   'topcode'. */
extern UINT TI_TOP_Min_Issue_Slot_Count (TOP topcode);

/* Returns the stage number for the latest definition of the
   'topcode'.
   This is useful in establishing an upper bound of the length
   of resource reservation table during scheduling */
extern UINT TI_TOP_Latest_Def_Stage (TOP topcode);

/* Return the topcode that is equivalent to branch topcode 'top' but
   that has the taken and not-taken targets switched. Return
   TOP_UNDEFINED if no such topcode exists. */
extern TOP TI_TOP_Invert_Branch (TOP top);

/* Return the topcode that is equivalent to 'top', but that has the
   operands switched. Return TOP_UNDEFINED if an equivalent topcode
   does not exist. */
extern TOP TI_TOP_Same_Branch_Flipped_Operands (TOP top);

/* Return the immediate version of 'top', or TOP_UNDEFINED if an
   immediate version does not exist. */
extern TOP TI_TOP_Immed_To_Reg (TOP top);

/* Return the register version of 'top', or TOP_UNDEFINED if an
   immediate version does not exist. */
extern TOP TI_TOP_Reg_To_Immed (TOP top);

/* Return the topcode for the updating version of 'topcode'. Return
   TOP_UNDEFINED if there is no updating version. */
extern TOP TI_TOP_Nonupdate_To_Update (TOP topcode);
extern void TI_TOP_Set_Nonupdate_To_Update (TOP non_update, TOP update);

/* Return the topcode for the non-updating version of 'topcode'. Return
   TOP_UNDEFINED if there is no non-updating version. */
extern TOP TI_TOP_Update_To_Nonupdate (TOP topcode);

/* Return the topcode for the indexed version of 'topcode'. Return
   TOP_UNDEFINED if there is no indexed version. */
extern TOP TI_TOP_Nonindex_To_Index (TOP topcode);

/* Return the alignment boundary for issueing a 'topcode' OP */
extern INT TI_TOP_Issue_Alignment(TOP topcode);

/* Return true if this is a stream get/put opcode */
extern bool TI_TOP_Stream_Get(TOP topcode);
extern bool TI_TOP_Stream_Put(TOP topcode);

/* Return true if this topcode uses a (global or slot) shared functional unit */
extern bool TI_TOP_Use_Shared_Functional_Unit(TOP topcode);

/* Return true if this topcode has Export State results */
extern bool TI_TOP_Has_Export_State(TOP topcode);

/* Return true if this topcode has Import Wire operands */
extern bool TI_TOP_Has_Tie_Queue(TOP topcode);

/* Return the number of topcodes which specialize the given <generic_top>.
 * The specialized topcodes are returned in the return_tops[].
 * It is allocated by the caller and its size is specified.
 */
extern UINT TI_TOP_Get_Special_Tops (TOP generic_top, TOP return_tops[],
				     UINT size);

/* Return the number of compatible topcodes for the given <top>.
 * In general, two topcodes are compatible if they differ
 * only in the ranges of their immediate operands.
 * The compatible topcoes are returned in the return_tops[].
 * It is allocated by the caller and its size is specified.
 */
extern UINT TI_TOP_Get_Compatible_Tops (TOP top, TOP return_tops[], UINT size);

/* Return the generic topcode which represent the set of topcodes in
 * <tops_array>. The generic topcode can be bundlded in any format/slot
 * that is specified by one of the opcode in tops_array[].
 * It is allocated by the caller and its size is specified by <num_tops>.
 */
extern TOP TI_TOP_Get_Generic_Top (const TOP tops_array[], int num_tops);

/* Return true if TOP is a legal OP in <format, slot>. Used to check
 * what is the final OP for a given <format, slot> in cg emit phase. 
 */
extern bool TI_TOP_Is_Legal_In_Format_Slot (int, int, TOP);

/* Return true if top is a normal (not narrow) branch TPO of the xtensa core */
extern bool TI_TOP_Is_CoreBranch(TOP top);

/* Return the TOP of theh corresponding wide branch for a given branch TOP,
 * and LITCLASS of the target operand is returned in the 2nd param. 
 */
extern TOP TI_TOP_Get_WBranch_Top_Litclass(TOP, ISA_LITCLASS *);

/* Return mininum of all wide branches' target ranges; 
 * return -1 if no wide branches
 */
extern INT TI_Wide_Branch_MinRange();

/* =============================================================

   Subsets

   =========================================================== */

#include "targ_isa_subset.h"

/* Return the current isa subset. */  
extern ISA_SUBSET TI_ISA_Subset_Value (void);

/* Return the name of the 'subset'. */  
extern const char* TI_ISA_Subset_Name (ISA_SUBSET subset);

/* Return true if 'topcode' is a member of 'subset'. */  
extern BOOL TI_ISA_Subset_Member (ISA_SUBSET subset, TOP topcode);


/* =============================================================

   ISA Properties

   =========================================================== */

#include "targ_isa_properties.h"

/* Return true/false if 'topcode' has/does-not-have property
   'prop'. */
extern BOOL TI_ISA_Property_Set (ISA_PROPERTY prop, TOP topcode);

/* Return true/false if 'topcode' is/is-not a TIE opcode */
#define TI_ISA_Property_Is_Tie(topcode) ((topcode) >= TOP_count)

/* Return true/false if target has updating load or store
   instructions. */
extern BOOL TI_ISA_Has_Updating_Ops (void);

/* Return true/false if target has indexed load or store
   instructions. */
extern BOOL TI_ISA_Has_Indexed_Ops (void);

/* Return the number of bytes accessed by opcode 'memop'. 'memop'
   must be a load or store opcode. */
extern UINT32 TI_ISA_Mem_Ref_Bytes (TOP topcode);
  
/* Returns the size in bytes of the specified topcode. */
extern INT TI_ISA_Inst_Bytes (TOP topcode);

/* If 'topcode' is a copy, returns the index of the source operand. If
   'required' is true, aborts if 'topcode' is not a copy. Otherwise
   returns -1. */
extern INT TI_ISA_Copy_Operand (TOP topcode, BOOL required);

/* If ISA has wide branches, returns true; otherwise, return false. */
extern bool TI_ISA_Has_Wide_Branches(void);

/* When both mul16 and mac16 are available, use this to see if mul16
   preferred */
extern bool TI_ISA_Prefer_MUL16(void);

/* =============================================================

   Processor Properties

   =========================================================== */

#include "targ_proc_properties.h"

/* Return true/false if the current processor target has/does-not-have
   property 'prop'. */
extern BOOL TI_PROC_Property_Set (PROC_PROPERTY prop);

  
/* =============================================================

   Registers

   =========================================================== */

#include "targ_isa_registers.h"
  
/* Return the number of register classes. This is TI_ISA_Regclass_Last
   - TI_ISA_Regclass_First + 1. */
extern INT TI_ISA_Num_Regclasses (void);
  
/* Return the identifier of the first register class. */
extern ISA_REGCLASS TI_ISA_Regclass_First (void);
  
/* Return the identifier of the last register class. */
extern ISA_REGCLASS TI_ISA_Regclass_Last (void);

/* Return the register class representing the integer registers. */
extern ISA_REGCLASS TI_ISA_Regclass_Integer (void);

/* Return the register class representing the fp registers. */
extern ISA_REGCLASS TI_ISA_Regclass_Float (void);

/* Return the register class representing the br registers. */
extern ISA_REGCLASS TI_ISA_Regclass_Branch (void);

/* Return the register class representing the mr registers. */
extern ISA_REGCLASS TI_ISA_Regclass_Macc (void);

/* Return the register class representing the special registers (for rsr). */
extern ISA_REGCLASS TI_ISA_Regclass_Special (void);

/* Return the register class representing HiFi2 AE_PR registers. */
extern ISA_REGCLASS TI_ISA_Regclass_HiFi2_PR (void);

/* Return the register class representing HiFi2 AE_QR registers. */
extern ISA_REGCLASS TI_ISA_Regclass_HiFi2_QR (void);

/* Return the register subclass representing either
   1. special system state, or
   2. tie user state register
   with 'state_name'.
   Return ISA_REGSUBCLASS_UNDEFINED if the given state doesn't
   exist. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass(const char *state_name);

/* Return the register subclass representing special system state
   which corresponds to (reg_num)th entry in the special register file
   Return ISA_REGSUBCLASS_UNDEFINED if the given state doesn't
   exist. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Special_Reg_Num (INT reg_num);

/* Return the name of special system state
   which corresponds to (reg_num)th entry in the special register file
   Return NULL if the given state doesn't
   exist. */
extern const char* TI_ISA_Regsubclass_Special_Name (INT reg_num);

/* Return the register subclass representing the br1 registers. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Branch1 (void);

/* Return the register subclass representing the br2 registers. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Branch2 (void);

/* Return the register subclass representing the br4 registers. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Branch4 (void);

/* Return the register subclass representing the br8 registers. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Branch8 (void);

/* Return the register subclass representing the br16 registers. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Branch16 (void);

/* Return TRUE if the register class representing tie states. */
extern BOOL TI_ISA_Regclass_Is_State (ISA_REGCLASS rc);

/* Return the register class representing tie state 'state_name'.
   Return ISA_REGSUBCLASS_UNDEFINED if the given state doesn't
   exist. */
extern ISA_REGCLASS TI_ISA_Regclass_State (const char *state_name);

/* Return the register class representing tie state 'state_name'.
   Return ISA_REGSUBCLASS_UNDEFINED if the given state doesn't
   exist. */
extern INT TI_ISA_State_Bit_Size (const char *state_name);

/* Return the register subclass representing tie state 'state_name'.
   Return ISA_REGSUBCLASS_UNDEFINED if the given state doesn't
   exist. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_State (const char *state_name);

/* Return the maximum register number of all classes. The lowest
   register number is implicitly zero. */
extern INT TI_ISA_Register_Max (void);

/* Return register class info for class 'rc'. */
extern const ISA_REGCLASS_INFO *TI_ISA_Regclass_Info (ISA_REGCLASS rc);

/* Return the first (lowest numbered) register for the class described
   by 'info'. */
extern INT TI_ISA_Regclass_First_Reg (const ISA_REGCLASS_INFO *info);

/* Return the last (highest numbered) register for the class described
   by 'info'. */
extern INT TI_ISA_Regclass_Last_Reg (const ISA_REGCLASS_INFO *info);

/* Return the size, in bits, of the registers in the class described
   by 'info'. */
extern INT TI_ISA_Regclass_Bit_Size (const ISA_REGCLASS_INFO *info);

/* Return a flag that indicates if the registers in the class
   described by 'info' can be stored to memory, i.e. there is a store
   instruction for the registers in the class. */
extern INT TI_ISA_Regclass_Can_Store (const ISA_REGCLASS_INFO *info);

/* Return a flag that indicates if the registers in the class
   described by 'info' can be saved and restore to memory in
   multiples, i.e. as a group. */
extern INT TI_ISA_Regclass_Multiple_Save (const ISA_REGCLASS_INFO *info);

/* Return the name of the class described by 'info'. */
extern const char *TI_ISA_Regclass_Name (const ISA_REGCLASS_INFO *info);

/* Return the name of the 'reg_index'th register in the class
   described by 'info'. NOTE: reg_index==0 corresponds to the first
   register of the class. */
extern const char *TI_ISA_Regclass_Reg_Name (const ISA_REGCLASS_INFO *info,
					     INT reg_index);
  
/* Return the register class to which 'mtype' should be
   assigned. Return ISA_REGCLASS_UNDEFINED if the mapping is
   unknown. */
extern ISA_REGCLASS TI_ISA_Regclass_For_Mtype (TYPE_ID mtype);
extern INT TI_ISA_Regsize_For_Mtype (TYPE_ID mtype);

/* Return one of the mtypes which is based on the register class 'rc'.
   Return MTYPE_UNKNOWN if the mapping is unknown. */
extern TYPE_ID TI_ISA_Mtype_For_Regclass (ISA_REGCLASS rc);

/* Return the register class with the short name 'short_name' */
extern ISA_REGCLASS TI_ISA_Find_Regclass (const char* short_name);

/* Return the debugging register number of the 'reg_index'th register
   in 'regclass'. Return -1 if we don't know the debugging number for
   the register. NOTE: reg_index==0 corresponds to the first register
   of the class. */
extern INT TI_ISA_Regclass_Reg_Debug_Number (ISA_REGCLASS regclass, INT reg_index);
  
/* Return the topcode of the simulated define instruction which
   has one result in 'rc'
*/
extern TOP TI_ISA_Regclass_Def_Topcode (ISA_REGCLASS rc);

/* Return TRUE if the register file is accessed with split pipeline */
extern BOOL TI_ISA_Regclass_Has_Split_Pipe (ISA_REGCLASS rc);

/* Iterate in forward or reverse order over all the register class
   values using the ISA_REGCLASS variable 'cl'. */
#define FOR_ALL_ISA_REGCLASS(cl)       \
  for (cl = TI_ISA_Regclass_First();   \
       cl <= TI_ISA_Regclass_Last();   \
       cl = (ISA_REGCLASS)(cl + 1))

#define FOR_ALL_ISA_REGCLASS_REVERSE(cl) \
  for (cl = TI_ISA_Regclass_Last();      \
       cl >= TI_ISA_Regclass_First();    \
       cl = (ISA_REGCLASS)(cl - 1))


/* Return the number of register subclasses. This is
   TI_ISA_Regsubclass_Last - TI_ISA_Regsubclass_First + 1. */
extern INT TI_ISA_Num_Regsubclasses (void);
  
/* Return the identifier of the first register subclass. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_First (void);
  
/* Return the identifier of the last register subclass. */
extern ISA_REGSUBCLASS TI_ISA_Regsubclass_Last (void);

/* Return register subclass info for subclass 'rc'. */
extern const ISA_REGSUBCLASS_INFO *TI_ISA_Regsubclass_Info (ISA_REGSUBCLASS rc);

/* Return the base register class for the subclass described by
   'info'. */
extern ISA_REGCLASS TI_ISA_Regsubclass_Class (const ISA_REGSUBCLASS_INFO *info);

/* Return the number of registers in the subclass described by
   'info'. */
extern INT TI_ISA_Regsubclass_Count (const ISA_REGSUBCLASS_INFO *info);

/* Return the 'n'th member (register) of the subclass described by
   'info'. The order of the registers returned is arbitrary. */
extern UINT TI_ISA_Regsubclass_Member (const ISA_REGSUBCLASS_INFO *info, INT n);

/* Return the name of the subclass described by 'info'. */
extern const char *TI_ISA_Regsubclass_Name (const ISA_REGSUBCLASS_INFO *info);

/* Return the name of the 'reg_index'th register in the subclass
   described by 'info'.  If the member does not have a subclass
   specific name, NULL is returned. */
extern const char *TI_ISA_Regsubclass_Reg_Name (const ISA_REGSUBCLASS_INFO *info,
						INT reg_index);
  
/* Iterate over all the register subclass values using the
   ISA_REGSUBCLASS variable 'cl'. */
#define FOR_ALL_ISA_REGSUBCLASS(cl)       \
  for (cl = TI_ISA_Regsubclass_First();   \
       cl <= TI_ISA_Regsubclass_Last();   \
       cl = (ISA_REGSUBCLASS)(cl + 1))

  
/* =============================================================

   ABI

   =========================================================== */

#include "targ_abi_properties.h"
  
/* Return the current abi. */  
extern ABI_PROPERTIES_ABI TI_ABI_Value (void);

/* Return the assembly name of register 'reg' in regclass 'rc'. */
extern const char *TI_ABI_Reg_Name (ISA_REGCLASS rc, INT reg);

/* Return true if 'prop'erty set for 'reg'ister in regclass 'rc'. */
extern BOOL TI_ABI_Property_Set(ABI_PROPERTY prop, ISA_REGCLASS rc, INT reg);

  
/* =============================================================

   Literals. 

   =========================================================== */

#include "targ_isa_lits.h"

/* Return a litclass representing the same literals as 'lc' plus
   'offset'. I.e. if 'lc' represents 0, 1, 2; and 'offset' is -2, then
   return a litclass representing -2, -1, 0. If a litclass
   representing 'lc' plus 'offset' already exists, that litclass is
   returned, otherwise a new litclass is created and returned. */
extern ISA_LITCLASS TI_ISA_LC_Create (ISA_LITCLASS lc, INT32 offset);

/* Return a litclass created from a libisa operand
   or LC_UNDEFINED if there is some problem */
extern ISA_LITCLASS TI_ISA_LC_Find (xtensa_opcode isa_opc, int isa_opnd);

/* Return the name of the literal class 'lc'. */  
extern const char *TI_ISA_LC_Name (ISA_LITCLASS lc);
  
/* Returns whether 'val' is a value that belongs to 'lc'." */
extern BOOL TI_ISA_LC_Value_In_Class (INT64 val, ISA_LITCLASS lc);

/* Return a range for 'lc' such that 'low' == the minimum value
   represented by 'lc', and 'high' is == the maximum value. Return
   FALSE if 'low' and 'high' cannot be determined. Note that for TIE
   litclasses, the low and high values may not be exact. Note that for
   any litclass there can be "holes" in the values representable by
   'lc' between 'low' and 'high'. */
extern BOOL TI_ISA_LC_Range (ISA_LITCLASS lc, INT32 *low, INT32 *high);
  
/* =============================================================

   Enums. 

   =========================================================== */

#include "targ_isa_enums.h"

/* Return the name of 'ec'. */  
extern const char *TI_ISA_EC_Name (ISA_ENUMCLASS ec);

/* Returns the first ECV for enum class 'ec'. */  
extern ISA_ENUMCLASS_VALUE TI_ISA_EC_First_Value (ISA_ENUMCLASS ec);

/* Returns the last ECV for enum class 'ec'. Note that it assumes all
   ECV for an EC are in the first/last range. */
extern ISA_ENUMCLASS_VALUE TI_ISA_EC_Last_Value (ISA_ENUMCLASS ec);

/* Return the name of 'ecv'. */  
extern const char *TI_ISA_ECV_Name (ISA_ENUMCLASS_VALUE ecv);

/* Return the int-value of 'ecv'. */  
extern INT TI_ISA_ECV_Intval (ISA_ENUMCLASS_VALUE ecv);


/* =============================================================

   Operands. 

   =========================================================== */

#include "targ_isa_operands.h"

/* Return the maximum number of operands used by any instruction in
   the target. */
extern INT TI_ISA_Operand_Max (void);

/* Return the maximum number of results used by any instruction in the
   target. */
extern INT TI_ISA_Result_Max (void);

/* Return topcode info for 'topcode'. */
extern const ISA_OPERAND_INFO *TI_ISA_Operand_Info (TOP topcode);

/* Return the number of operands for opcode described by 'info'. */
extern INT TI_ISA_Op_Operands (const ISA_OPERAND_INFO *info);

/* Return the number of results for the opcode described by 'info'. */
extern INT TI_ISA_Op_Results (const ISA_OPERAND_INFO *info);

/* Get the operand type of 'opnd' for opcode described by 'info'. */
extern const ISA_OPERAND_VALTYP *TI_ISA_Op_Operand (const ISA_OPERAND_INFO *info, INT opnd);

/* Get the operand type of 'result' for opcode described by 'info'. */
extern const ISA_OPERAND_VALTYP *TI_ISA_Op_Result (const ISA_OPERAND_INFO *info, INT result);

/* Get the operand use type of 'opnd' for opcode described by 'info'. */
extern ISA_OPERAND_USE TI_ISA_Op_Operand_Use (const ISA_OPERAND_INFO *info, INT opnd);

/* Get the operand use type of 'result' for opcode described by 'info'. */
extern ISA_OPERAND_USE TI_ISA_Op_Result_Use (const ISA_OPERAND_INFO *info, INT result);

/* For a topcode with same result property (PROP_same_res), the following
   return corresponding result or operand index given a operand or result
   index
*/
extern INT32 TI_ISA_Op_Sameres_Operand (TOP topcode, INT32 result_index);
extern INT32 TI_ISA_Op_Sameres_Result (TOP topcode, INT32 operand_index);

/* Return the register class for the operand valtype 'type'. */  
extern ISA_REGCLASS TI_ISA_Valtyp_Regclass (const ISA_OPERAND_VALTYP *type);

/* Return the register subclass for the operand valtype 'type'. */  
extern ISA_REGSUBCLASS TI_ISA_Valtyp_Regsubclass (const ISA_OPERAND_VALTYP *type);

/* Return the literal class for the operand valtype 'type'. */  
extern ISA_LITCLASS TI_ISA_Valtyp_Litclass (const ISA_OPERAND_VALTYP *type);

/* Return the enum class for the operand valtype 'type'. */  
extern ISA_ENUMCLASS TI_ISA_Valtyp_Enumclass (const ISA_OPERAND_VALTYP *type);

/* Get the bit size for the operand valtype specified by 'type'. */
extern INT TI_ISA_Valtyp_Size (const ISA_OPERAND_VALTYP *type);

/* Get the number of register for multi-reg valtype specified by 'type'. */
extern INT TI_ISA_Valtyp_Num_Regs (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if the operand valtyp is a register. */
extern BOOL TI_ISA_Valtyp_Is_Register (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if the operand valtyp is signed. */
extern BOOL TI_ISA_Valtyp_Is_Signed (const ISA_OPERAND_VALTYP *type);

/* Return a bool to specify if the operand valtyp 'type' is an FPU
   integer. */
extern BOOL TI_ISA_Valtyp_Is_FPU_Int (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if operand valtyp 'type' is
   pc-relative. */
extern BOOL TI_ISA_Valtyp_Is_PCRel (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if operand valtyp 'type' is a continuation
   in a aggregate register operand such as the second operand of a
   register pair */
extern BOOL TI_ISA_Valtyp_Is_Continuation (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if operand valtyp 'type' is a literal. */
extern BOOL TI_ISA_Valtyp_Is_Literal (const ISA_OPERAND_VALTYP *type);

/* Return a bool specifying if operand valtyp 'type' is an enum. */
extern BOOL TI_ISA_Valtyp_Is_Enum (const ISA_OPERAND_VALTYP *type);
  
/* Return a boolean to specify if the 64-bit integer value can fit in
   the literal field of an instruction with the given topcode. Return
   FALSE if 'topcode' has no literals. If 'topcode' has more than one
   literal field, make sure that it can fit in them all (conservative
   since we have no imformation about which literal is actually being
   tested). */
extern BOOL TI_TOP_Can_Have_Immediate (INT64 value, TOP topcode);

/* If 'topcode' has an immediate operand, return its operand number by
   value and literal class by reference through 'lclass' (a null
   pointer can be passed for 'lclass' if the literal class is not
   needed). If 'topcode' has more than 1 literal, return the first
   one. If there is no immediate operand, return -1. */
extern INT TI_TOP_Immediate_Operand (TOP topcode, ISA_LITCLASS *lclass);

/* Return number of immediate operands ofa 'topcode' */
extern INT TI_TOP_Num_Immediate_Operand (TOP topcode);

/* For the instruction specified by 'topcode', give the operand number
   with the use 'use'. If there is no such operand, return -1. */
extern INT TI_TOP_Find_Operand_Use (TOP topcode, ISA_OPERAND_USE use);

/* For the instruction specified by 'topcode', give the result number
   with the use 'use'. If there is no such operand, return -1. */
extern INT TI_TOP_Find_Result_Use (TOP topcode, ISA_OPERAND_USE use);

/* If 'topcode' has a target operand, return the litclass of this
   operand. If no such an operand, return LC_UNDEFINED. */
extern ISA_LITCLASS TI_TOP_Branch_Target_Litclass (TOP topcode);


/* =============================================================

   Bundle. 

   =========================================================== */

#include "targ_isa_bundle.h"
  
/* Return the number of different bundles. */  
extern INT TI_ISA_Num_Bundles (void);

/* Return the number of different bundles. */  
extern INT TI_ISA_Bundle_Bytes (INT bundle);

/* Return the number of slots per bundle. */  
extern INT TI_ISA_Num_Slots (INT bundle);

/* Return the maxinum number of slots for all bundles. */  
extern INT TI_ISA_Max_Num_Slots ();

/* Return the number of execution units. */  
extern INT TI_ISA_Exec_Num_Units (void);
  
/* Maximum number of bits required to encode all the execution
   property types. */
extern INT TI_ISA_Tag_Shift (void);

/* Returns exec_unit_property for the instruction specified by
   'topcode'. */
extern ISA_EXEC_UNIT_PROPERTY TI_ISA_Exec_Unit_Prop (TOP topcode);

/* Return exec_unit_property for the slot position 'slot_index' in
   'bundle'. */
extern ISA_EXEC_UNIT_PROPERTY TI_ISA_Exec_Slot_Prop (INT bundle, INT slot_index);

/* Return slot_mask for 'bundle'. */
extern UINT64 TI_ISA_Exec_Slot_Mask (INT bundle);

/* Return stop bit for the slot position 'slot_index' in 'bundle'. */
extern BOOL TI_ISA_Exec_Stop (INT bundle, INT slot_index);

/* Return the execution unit slot position 'slot_index' in
   'bundle'. */
extern ISA_EXEC_UNIT TI_ISA_Exec_Unit (INT bundle, INT slot_index);

/* Return stop_mask for 'bundle'. */
extern UINT32 TI_ISA_Exec_Stop_Mask (INT bundle);

/* Return assembly name for 'bundle'. */
extern const char *TI_ISA_Exec_Asmname (INT bundle);

/* Return opcode to use as a NOP for a given slot */
extern TOP TI_ISA_Noop_Top(ISA_EXEC_UNIT_PROPERTY unit);
  
/* =============================================================

   Hazards. 

   =========================================================== */

#include "targ_isa_hazards.h"
  
/* Return true if 'topcode' has a hazard. */  
extern BOOL TI_ISA_Top_Has_Hazard (TOP topcode);

/* Get the first hazard description for 'topcode'. */
extern ISA_HAZARD_INFO *TI_ISA_Hazard_First (TOP topcode);

/* Get the next hazard description for 'topcode'. */
extern ISA_HAZARD_INFO *TI_ISA_Hazard_Next (ISA_HAZARD_INFO *info);

/* Return the type of the hazard. */  
extern ISA_HAZARD TI_ISA_Hazard_Type (ISA_HAZARD_INFO *info);

/* Return the hazard specific data. */  
extern INT TI_ISA_Hazard_Data (ISA_HAZARD_INFO *info);

/* Returns the number of OPs that must precede the instruction with
   the hazard. */
extern INT TI_ISA_Hazard_Pre_Ops (ISA_HAZARD_INFO *info);

/* Returns the number of OPs that must follow the instruction with the
   hazard. */
extern INT TI_ISA_Hazard_Post_Ops (ISA_HAZARD_INFO *info);

  
/* =============================================================

   Print. 

   =========================================================== */

#include "targ_isa_print.h"
  
/* Return print information about 'topcode'. */  
extern const ISA_PRINT_INFO *TI_ISA_Print_Info (TOP topcode);

/* Identifies an instruction component to be printed.  'index'
   specifies the component. The first component has index 0; the end
   of the components is signalled by the return of
   ISA_PRINT_COMP_end. */
extern INT TI_ISA_Print_Info_Comp(const ISA_PRINT_INFO *info, INT index);

/* The printf format string for printing the instruction described by
   'info'. */
extern const char *TI_ISA_Print_Info_Format (const ISA_PRINT_INFO *info);

/* Returns the assembly language name for 'topcode'. */
extern const char *TI_ISA_Print_Asmname (TOP topcode);

/* Returns whether the operand is part of the full asm name. */
extern BOOL TI_ISA_Print_Operand_Is_Part_Of_Name (TOP topcode, INT opindex);


/* =============================================================

   Scheduling. 

   =========================================================== */

typedef UINT TI_SI_RESOURCE_ID;

// this works with TI_TIE_FORMAT_MAX==10
typedef mUINT32 TI_SI_FORMAT_ID_SET;
typedef UINT TI_SI_ID;

// this is a variable length bit vector class in ti_si.h
template <int type_id> class TI_SI_RRV;

typedef TI_SI_RRV<1> TI_SI_RRW;

typedef TI_SI_RRV<2> TI_SI_RESOURCE_ID_SET;

typedef TI_SI_RRW* TI_SI_RR;

enum { TI_SI_INVALID_FORMAT_ID= -1, TI_SI_BAD_II_SET_MAX=127 };

typedef struct {
  const char* name;
  TI_SI_RESOURCE_ID id;
  mUINT8 avail_per_cycle;
  mUINT8 word_index;
  mUINT8 bit_index;
  mINT8 format_id;
} TI_SI_RESOURCE;

typedef struct {
  TI_SI_RESOURCE* resource;
  mINT32 total_used;
} TI_SI_RESOURCE_TOTAL;

typedef struct {
  const char* name;
  mINT32 skew;
  mINT32 avail_per_cycle;
} TI_SI_ISSUE_SLOT;

typedef struct {
  mUINT64 dw[2];
} TI_SI_BAD_II_SET;

typedef struct {
  const char* name;
  TI_SI_ID id;
  mUINT8 *operand_access_times;
  mUINT8 *result_available_times;
  mINT32 load_access_time;
  mINT32 last_issue_cycle;
  mINT32 store_available_time;
  TI_SI_RR rr;
  TI_SI_RESOURCE_ID_SET *resources_used;
  mUINT32 ii_info_size;
  TI_SI_RR *ii_rr;
  TI_SI_RESOURCE_ID_SET **ii_resources_used;
  TI_SI_BAD_II_SET bad_iis;
  mINT32 valid_issue_slot_count;
  TI_SI_ISSUE_SLOT **valid_issue_slots;
  TI_SI_FORMAT_ID_SET valid_issue_formats;
  mINT32 resource_total_vector_size;
  TI_SI_RESOURCE_TOTAL *resource_total_vector;
  mUINT8 write_write_interlock;
} TI_SI;

/* Number of target resources and pointer to the array of those
   resources. */
extern DLL_SHARED INT TI_SI_resource_count;
extern DLL_SHARED TI_SI_RESOURCE **TI_SI_resources;

/* The value of an initialized (no reserved resources) resource
   reservation entry. */
extern DLL_SHARED TI_SI_RRW* TI_SI_RRW_initializer;

/* An mask for quick resource overuse detection */  
extern DLL_SHARED TI_SI_RRW* TI_SI_RRW_overuse_mask;

/* An array of masks for masking out format overuse */  
/* Entry i of this array has overuse bit set for each resource created
   for format i */
extern DLL_SHARED TI_SI_RRW* TI_SI_RRW_format_resource_overuse_mask;

extern DLL_SHARED TI_SI_RRW* TI_SI_RRW_all_format_resource_overuse_mask;

/* Number of target issue slots and pointer to the array of those
   issue slots. */
extern DLL_SHARED INT TI_SI_issue_slot_count;
extern DLL_SHARED TI_SI_ISSUE_SLOT **TI_SI_issue_slots;

/* Array holding scheduling information for each topcode. */  
extern DLL_SHARED TI_SI **TI_SI_top_si;

/* Number of scheduling information groups, and pointer to array
   holding scheduling information for each group. */
extern DLL_SHARED INT TI_SI_ID_count;
extern DLL_SHARED TI_SI **TI_SI_ID_si;

/* commonly used set */
extern DLL_SHARED TI_SI_RESOURCE_ID_SET* TI_SI_resource_id_set_universe;
extern DLL_SHARED TI_SI_RESOURCE_ID_SET* TI_SI_resource_id_set_empty;

/* check if there is any resource over-subscription if the topcodes in
   the input array are scheduled at the same cycle.
   return TRUE if no over-subscription or FALSE if there is any
*/
extern BOOL TI_SI_Check_Resource_Ok (TOP topcodes[], INT num_tops);

/* return TRUE if the top has no valid formats
   this can be caused by no flix option which invalidates flix formats
   it is not strictly an error if the corresponding opcode is not really
   used by the user
*/
inline BOOL TI_TOP_No_Valid_Format(const TOP top) {
  return TI_SI_top_si[top]->valid_issue_formats == (TI_SI_FORMAT_ID_SET)0;
}

/* =============================================================

   TIE. 

   =========================================================== */

typedef xtie_property_iter ISA_SIMD_TYPE_INFO;

/* Return a pointer to the xtensa_isa structure. */
extern xtensa_isa TI_TIE_Libisa_Info (void);

/* Return pointers to xtensa_tie structures. */
extern xtensa_tie TI_TIE_Xtie_Info (void);
extern xtie_phase TI_TIE_Xtie_Post_Rewrite (void);
extern xtie_phase TI_TIE_Xtie_Compiler (void);

/* TIE files  generated by RA (BearValley) Xpress are complicated and 
   are not fully represented in the new compiler xml. This function
   tells whether we need to process and understand that older xml.
   Used entirely for fusions. */
extern BOOL TI_TIE_Need_Post_Rewrite_TIE (void);

/* Return the number of tie macros, or 0 if there is no tie. */  
extern UINT TI_TIE_Num_Macros (void);

/* Return the number of tie ctypes, or 0 if there is no tie. */  
extern UINT TI_TIE_Num_Ctypes (void);

/* Initialize the mtypes to represent the user-defined types. */
extern void TI_TIE_Mtypes_Init (void);


/* Autotie operators with no OPERATOR eqivalent. */
typedef enum ti_operator
{
  TI_OPR_VSEL,                /* select */
  TI_OPR_LOADA,               /* alignment load */
  TI_OPR_LOADAU,              /* alignment updating load */
  TI_OPR_LOADAP,              /* alignment priming load */
  TI_OPR_STOREA,              /* alignment store */
  TI_OPR_STOREAU,             /* alignment updating store */
  TI_OPR_STOREAP,             /* alignment priming store */
  TI_OPR_STOREAF,             /* alignment store flush */
  TI_OPR_LS_U,                /* updating load scalar */
  TI_OPR_LSX,                 /* indexed load scalar */
  TI_OPR_LSX_U,               /* updating indexed load scalar */
  TI_OPR_LV_U,                /* updating load vector */
  TI_OPR_LVX,                 /* indexed load vector */
  TI_OPR_LVX_U,               /* updating indexed load vector */
  TI_OPR_MPY_EVEN,            /* multiply even */
  TI_OPR_MPY_ODD,             /* multiply odd */
  TI_OPR_MADD_EVEN,           /* madd even */
  TI_OPR_MADD_ODD,            /* madd odd */
  TI_OPR_MSUB_EVEN,           /* msub even */
  TI_OPR_MSUB_ODD,            /* msub odd */
  TI_OPR_SS_U,                /* updating store scalar */
  TI_OPR_SV_U,                /* updating store vector */
  TI_OPR_SVX,                 /* indexed vector store */
  TI_OPR_SVX_U,               /* updating indexed vector store */
  TI_OPR_MULR,                /* multiply add with ROUND reg */
  TI_OPR_MULR_EVEN,           /* multiply add with ROUND reg even*/
  TI_OPR_MULR_ODD,            /* multiply add with ROUND reg odd*/
  TI_OPR_WVSAR,               /* write VSAR reg */
  TI_OPR_WROUND,              /* write ROUND reg */
  TI_OPR_RADD,                /* sum reduction */
  TI_OPR_RMIN,                /* min reduction */
  TI_OPR_RMAX,                /* max reduction */
  TI_OPR_ZERO,                /* constant zero result */
  TI_OPR_VPACK,               /* vector pack */
  TI_OPR_MOVT,                /* MOVT */
  TI_OPR_MOVF,                /* MOVF */
} TI_OPERATOR;

/* Direction of usage for an operand. */  
typedef enum ti_dir
{
  TI_DIR_NONE,
  TI_DIR_IN,
  TI_DIR_OUT,
  TI_DIR_INOUT
} TI_DIR;

  
/* Get the first or next set of simd type information. Return NULL if
   no (more) simd information. */
extern ISA_SIMD_TYPE_INFO TI_TIE_First_Simd_Type_Info (void);
extern ISA_SIMD_TYPE_INFO TI_TIE_Next_Simd_Type_Info (ISA_SIMD_TYPE_INFO current);

/* Iterate over all the simd type information, using
   ISA_SIMD_TYPE_INFO variable 'isi'. */
#define FOR_ALL_ISA_SIMD_TYPE_INFO(isi)             \
  for (isi = TI_TIE_First_Simd_Type_Info(); isi;    \
       isi = TI_TIE_Next_Simd_Type_Info(isi))

/* Return the vector length of 'isi'. */
extern unsigned int TI_ISA_Simd_Type_Vl (ISA_SIMD_TYPE_INFO isi);

/* Return the base mtype for vector 'isi'. */
extern TYPE_ID TI_ISA_Simd_Base_Mtype (ISA_SIMD_TYPE_INFO isi);

/* Return the vector register mtype for vector 'isi'. */
extern TYPE_ID TI_ISA_Simd_Vector_Register_Mtype (ISA_SIMD_TYPE_INFO isi);

/* Return the vector memory mtype for vector 'isi'. */
extern TYPE_ID TI_ISA_Simd_Vector_Memory_Mtype (ISA_SIMD_TYPE_INFO isi);

/* Return the alignment type (if any) used for alignment loads and stores to
   'simd_mem_type'. */
extern TYPE_ID TI_ISA_Alignment_Mtype (TYPE_ID simd_mem_type);

/* Return the vsel type (if any) used for VSEL of 'vec_type'. */
extern TYPE_ID TI_ISA_Vsel_Mtype(TYPE_ID vec_type);

/* Return the maximum number of fields VSEL can handle. */
extern INT TI_ISA_Vsel_Max_Way();

/* Return the name of the instrinsic that implements an operator on an
   rtype and desc. Return NULL if there is no such intrinsic. */
extern const char *TI_ISA_Operator_Intrinsic (OPERATOR oper, TYPE_ID rtype,
					      TYPE_ID desc);
extern const char *TI_ISA_TiOperator_Intrinsic (TI_OPERATOR oper, TYPE_ID rtype,
						TYPE_ID desc);

/* Return the name of the instrinsic that implements an operator on a
   list of explicit types and immediate values. For each operand the
   direction is specified in 'operand_dirs', and the type in
   'operand_types'. If 'operand_types' is MTYPE_UNKNOWN, that operand
   must be able to encode the immediate specified for the operand in
   'immediates'. */
extern const char *TI_ISA_Operator_Intrinsic_Exp (OPERATOR oper,
						  unsigned int num_operands,
						  TI_DIR *operand_dirs,
						  TYPE_ID *operand_types,
						  INT64 *immediates);
extern const char *TI_ISA_TiOperator_Intrinsic_Exp (TI_OPERATOR oper,
						    unsigned int num_operands,
						    TI_DIR *operand_dirs,
						    TYPE_ID *operand_types,
						    INT64 *immediates);
extern const char *TI_ISA_Named_Intrinsic_Exp (const char *oper,
					       unsigned int num_operands,
					       TI_DIR *operand_dirs,
					       TYPE_ID *operand_types,
					       INT64 *immediates);

  
/* =============================================================

   Misc. 

   =========================================================== */

/* Try to create an immediate version of branch 'intop' and the
   immediate 'inimm_val'. Return true if we can, with the new 'intop'
   and 'inimm_val' that is equivalent to the old. If 'flip' is true,
   then 'inimm_val' is the first operand to the branch, otherwise it
   is the second. */
extern BOOL TI_Equivalent_Immed_Branch (TOP *intop, INT64 *inimm_val, BOOL flip);

extern TOP TI_Convert_OP(TOP opcode, ISA_EXEC_UNIT_PROPERTY prop, 
			     INT *opnds);

/* Export the DLL load/lookup wrappers implemented in
   xtarch_interface.cxx. */  
extern void *TI_DLL_Load (const char *path, int print_err);
extern void *TI_DLL_Get_Symbol (void *handle, char *sym, const char *libname, int print_err);


#ifdef __cplusplus
#endif
#endif /* LIBTI_H */

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
