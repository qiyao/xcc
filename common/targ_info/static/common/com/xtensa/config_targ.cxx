
/*
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
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
 * Module: config_targ.c
 * $Revision: 1.24 $
 * $Date: 2000/04/06 02:29:07 $
 * $Author: mtibuild $
 * $Source: /osprey.src/osprey1.0/common/com/ia64/RCS/config_targ.cxx,v $
 *
 *
 * Description:
 *
 * Configuration specific to the target machine/system.
 *
 * NOTE:  There is an approximate distinction between -TARG option
 * group flags and their configuration (in config_targ_options.c), and more
 * generic target configuration (in this file).  Note that the related
 * header file config_targ.h is included in config.h, and hence in most
 * source files, whereas config_targ_options.h is only included directly, so
 * putting new -TARG option-related variables in config_targ_options.c is to
 * be preferred to putting them here.
 *
 * ====================================================================
 * ====================================================================
 */

#include "libti.h"
#include "defs.h"
#include "config.h"
#include "config_asm.h"
#include "config_debug.h"
#include "config_targ_options.h"
#include "config_opt.h"
#include "config_wopt.h"
#include "erglob.h"
#include "tracing.h"
#include "mtypes.h"
#include "stab.h"
#include "targ_sim.h"

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
typedef unsigned char an_integer_kind;
#include "c_int_model.h"
#endif


/* Architecture specific definitions */
DLL_SHARED BOOL ARCH_generate_nor = FALSE; // Generate the NOR opcode
DLL_SHARED BOOL ARCH_mask_shift_counts = FALSE; // shift counts are masked by the hardware (vs. truncated)

/* Target selection */
DLL_SHARED TARGET_ABI Target_ABI = ABI_UNDEF;
DLL_SHARED TARGET_PROCESSOR Target = TARGET_UNDEF;		/* -Tc */
DLL_SHARED TARGET_ISA  Target_ISA  = TARGET_ISA_UNDEF;	/* -Tc.Ic */

/* We need to know the machine type of the integer and floating point
 * registers for purposes of subprogram save/restore:
 */
DLL_SHARED CLASS_INDEX Spill_Int_Mtype = 0;
DLL_SHARED CLASS_INDEX Spill_Float_Mtype = 0;

/* The maximum integer machine type corresponding directly to the
 * machine registers, and the default integer machine type:
 */
DLL_SHARED CLASS_INDEX Max_Int_Mtype = 0;
DLL_SHARED CLASS_INDEX Max_Uint_Mtype = 0;
DLL_SHARED CLASS_INDEX Def_Int_Mtype = 0;
DLL_SHARED CLASS_INDEX Def_Uint_Mtype = 0;

/* On MIPS III targets, should we use 32-bit pointers? */
DLL_SHARED BOOL Use_32_Bit_Pointers = TRUE;

/* For various targets, what are the characteristics of pointers */
DLL_SHARED INT		Pointer_Size;
DLL_SHARED CLASS_INDEX	Pointer_Mtype;
DLL_SHARED CLASS_INDEX	Pointer_Mtype2;

/* What are pointers and booleans aliased to in WHIRL */
DLL_SHARED TYPE_ID Pointer_type;
DLL_SHARED TYPE_ID Pointer_type2;
DLL_SHARED TYPE_ID Boolean_type;
DLL_SHARED TYPE_ID Boolean_type2;
DLL_SHARED TYPE_ID Integer_type;

/* For various targets, what is the comparison result type? */
DLL_SHARED INT		Comparison_Result_Size;		/* in bytes */
DLL_SHARED CLASS_INDEX	Comparison_Result_Mtype;

/* The assembler directive for emitting an address depends on the target
 * pointer size.  The following is declared in config_asm.h:
 */
DLL_SHARED char *AS_ADDRESS;
DLL_SHARED char *AS_ADDRESS_UNALIGNED;

/* Is the "char" type signed? */
DLL_SHARED BOOL Char_Type_Is_Signed = FALSE;

/* Various categories which are static, for now: */
static BOOL Target_int64;	/* 64-bit integer registers? */

/* Miscellaneous exception control */
#define FPX_DEF EXC_ALL	/* Default enables */
DLL_SHARED INT16 FP_Exception_Enable_Max = FPX_DEF;/* Max FP trap enables */
DLL_SHARED INT16 FP_Exception_Enable_Min = 0;	/* Min FP trap enables */

DLL_SHARED INT32 Align_Instructions = 0;	/* 0 means hasn't been set */
DLL_SHARED BOOL Avoid_TFP_blikely_bug = FALSE;
DLL_SHARED BOOL Avoid_TFP_blikely_bug_overridden = FALSE;

/***** IEEE 754 options *****/
DLL_SHARED BOOL Force_IEEE_Comparisons = TRUE;    /* IEEE NaN comparisons? */

/***** INTERNAL group options *****/

DLL_SHARED BOOL WHIRL_Return_Val_On  = TRUE;
DLL_SHARED BOOL WHIRL_Mldid_Mstid_On = TRUE;
DLL_SHARED BOOL WHIRL_Return_Info_On = TRUE;


/* ====================================================================
 *
 * Target debugging options
 *
 * ====================================================================
 */

/* Symbolic Debug mode as specified on command line.  (The mode can
 * change from PU to PU because, for example, we encounter a call to
 * the routine 'alloca()' -- we must restore the mode to the value as
 * it was specified on the command line, so we remember it.)
 */
DLL_SHARED INT16 Symbolic_Debug_Mode;
DLL_SHARED INT16 Max_Symbolic_Debug_Mode;	/* Maximum for any PU */


/* ====================================================================
 *
 * Miscellaneous options
 *
 * ====================================================================
 */

/* Can 64-bit values be 4-byte aligned in memory? */
DLL_SHARED BOOL Allow_Word_Aligned_Doubles = FALSE;

/* Should we generate position-independent code by default? */
DLL_SHARED BOOL Generate_Position_Independent_Code = FALSE;

/* Split 64-bit integer ops into 32-bit ops, and simulate them? */
DLL_SHARED BOOL Split_64_Bit_Int_Ops = TRUE;

/* Split quad-precision ops into double-precision, and simulate them? */
DLL_SHARED BOOL Split_Quad_Ops = TRUE;

/* Work around a TFP branch cache problem. */
DLL_SHARED BOOL No_Quad_Aligned_Branch = FALSE;

/* Does target provides only unsigned 64-bit instructions? */
DLL_SHARED BOOL Only_Unsigned_64_Bit_Ops = FALSE;

DLL_SHARED BOOL Has_GP_Groups = FALSE;

/* Does target have offsets in load and store instructions?
 * Note: CG should instead test:
 * ( TI_TOP_Find_Operand_Use( OP_code(op), OU_offset ) >= 0 ) */
DLL_SHARED BOOL Use_Load_Store_Offset = TRUE;

DLL_SHARED BOOL Pass_Static_Link_On_Stack = TRUE;
DLL_SHARED INT Static_Link_Stack_Offset = -20;


/* ====================================================================
 *
 * Abi_Name / Isa_Name / Targ_Name
 *
 * Produce printable names for the target choices.
 *
 * ====================================================================
 */

char *
Abi_Name ( TARGET_ABI b )
{
  if (b == ABI_WINDOWED)
    return "windowed";
  else
    return "call0";
}

char *
Isa_Name ( TARGET_ISA b)
{
  return "xtensa";
}

char *
Targ_Name ( TARGET_PROCESSOR b)
{
  return "xtensa";
}

/* ====================================================================
 *
 * Preconfigure_Target
 *
 * Configuration of target-specific parameters, before flag processing.
 *
 * ====================================================================
 */

void
Preconfigure_Target ( void )
{
  return;
}

/* =======================================================================
 * Check if FP flags should be turned off if ISA extensions were not found
 * =======================================================================
 */
static void
Check_For_Missing_FP()
{
  if (TI_TOP_Topcode("add.s") == TOP_UNDEFINED ||
      TI_TOP_Topcode("sub.s") == TOP_UNDEFINED ||
      TI_TOP_Topcode("mul.s") == TOP_UNDEFINED) {
    xt_vectorfpu2005 =    FALSE;
    xt_hard_float =       FALSE;
    xt_hard_float_div =   FALSE;
    xt_hard_float_recip = FALSE;
    xt_hard_float_sqrt =  FALSE;
    xt_hard_float_rsqrt = FALSE;
  }
}

/* ====================================================================
 *
 * Prepare_Target
 *
 * Given target specification choices, fill in defaulted pieces and
 * check for conflicting specifications.  When this routine is done,
 * ABI, Target_ISA, Target, and Target_FPRs are all valid.  We also
 * use the target information to set the descriptive variables
 * Target_int64, Target_Int_Model, and Use_32_Bit_Pointers.
 *
 * TODO:  Pending final conversion of the driver, we may have incoming
 * information from either or both of -T... and -TARG:... options.  We
 * effectively give precedence to the -TARG: specifications, and after
 * final conversion should remove the -T support.  Note that we ignore
 * the pointer size and integer model specifications from -Tx,Pnn,Mm.
 *
 * ====================================================================
 */


static void
init_dll_paths (char **&dlls, char *&dlls_string)
{
  if (!dlls && dlls_string && *dlls_string != '\0')
  {
    int n, num_dlls = 1;
    char *p, *string;
    
    /* work with a copy of the string, so it can be modified */
    string = (char *) malloc (strlen (dlls_string) + 1);
    strcpy (string, dlls_string);
    p = string;
    
    /* Look for commas in the list to find the number of DLLs.  */
    while (*p)
    {
      if (*p++ == ',')
        num_dlls += 1;
    }
    
    dlls = (char **) malloc ((num_dlls + 2) * sizeof (char *));
    for (n = 0; n < num_dlls; n++)
    {
      p = strchr (string, ',');
      if (p)
        *p = '\0';
      dlls[n] = string;
      string = p + 1;
    }
    dlls[n] = 0;
  }
}


static void
Prepare_Target ( void )
{
  Target = TARGET_XTENSA;
  Target_ISA = TARGET_ISA_XTENSA;

  /* Check ABI... */
  if ( ABI_Name != NULL ) {

    if ( strcmp ( ABI_Name, "windowed" ) == 0 ) {
      Target_ABI = ABI_WINDOWED;
    } else {
      if ( strcmp ( ABI_Name, "call0" ) == 0 ) {
	Target_ABI = ABI_CALL0;
      } else {
	ErrMsg ( EC_Inv_TARG, "abi", ABI_Name );
      }
    }
  }

#ifdef _WIN32
  static const char dll[] = "extend.dll";
#else
  static const char dll[] = "extend.so";
#endif

  /* This code may be invoked from a tool where the dlls have been set
     directly from the parameter file (e.g., ir_a2b) or it may be used in
     a compiler pass where the list of dlls has been passed on the
     command-line as the comma-separated list stored in xt_isa_dlls_string
     and xt_xtie_dlls_string. In the latter case, we need to split up the
     list and create the xt_isa_dlls/xt_xtie_dlls array.  */

  init_dll_paths(xt_isa_dlls, xt_isa_dlls_string);
  init_dll_paths(xt_xtie_dlls, xt_xtie_dlls_string);

  if (!Target_Information_Init(dll, xt_isa_dlls, xt_xtie_dlls))
    FmtAssert(FALSE, ("Unable to initialize target information"));

  Is_True(Mtype_Last == MTYPE_CORE_LAST,("Core mtypes out of sync"));

  /* Initialize the user-defined types. */
  TI_TIE_Mtypes_Init();

  /* This is needed by lno, it would be nice if it was initialized
     there somewhere instead of being global across the entire
     compiler. */

  const ISA_REGCLASS_INFO *fprc = TI_ISA_Regclass_Info(TI_ISA_Regclass_Float());
  Target_FPRs = TI_ISA_Regclass_Last_Reg(fprc) - TI_ISA_Regclass_First_Reg(fprc) + 1;

  /* Set descriptive variables: */
  Target_int64 = FALSE;
  Use_32_Bit_Pointers = TRUE;
#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
  Target_Int_Model = TARGET_INT_ILP32;
  Make_Int_Model_Consistent ();
#endif

  Check_For_Missing_FP();

  Calculate_Predicted_Threshold();
}

/* ====================================================================
 *
 * Configure_Target
 *
 * Configuration of target-specific parameters, after flag processing.
 *
 * ====================================================================
 */

void
Configure_Target ( void )
{

  Target_Byte_Sex = ((xt_endian) ? BIG_ENDIAN : LITTLE_ENDIAN);
  Same_Byte_Sex = ( Target_Byte_Sex == Host_Byte_Sex );

  Gen_PIC_Calls = FALSE;
  GP_Is_Preserved = FALSE;

  /* Set up the target processor and ISA: */
  Prepare_Target ();

  /* Set up the target register set: */

  Spill_Int_Mtype = MTYPE_I4;
  Spill_Float_Mtype = MTYPE_F4;
  Max_Int_Mtype = Def_Int_Mtype = MTYPE_I4;
  Max_Uint_Mtype = Def_Uint_Mtype = MTYPE_U4;
  Boolean_type  = MTYPE_I4;
  Boolean_type2 = MTYPE_I4;
  Integer_type = MTYPE_I4;

  Split_Quad_Ops = TRUE;
  Split_64_Bit_Int_Ops = TRUE;

#if defined(FRONT_END_C) || defined(FRONT_END_CPLUSPLUS)
#ifndef EDG_FORTRAN
  Make_Int_Model_Consistent();
#endif
#endif

  /* Initialize pointer information */

  Pointer_Size = 4;
  Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A4 : MTYPE_U4;
  Pointer_type   = Pointer_Mtype;
  Pointer_Mtype2 = MTYPE_U4;
  Pointer_type2  = MTYPE_U4;

  AS_ADDRESS = AS_WORD;
  AS_ADDRESS_UNALIGNED = AS_WORD;

  /* If the user has requested aggregate alignment without specifying
   * a threshhold, set it to the register size.  Otherwise, make sure
   * it's a power of two.  WARNING:  The option group processing sets
   * it to -1 if no threshhold is given, and otherwise restricts it to
   * a reasonable range, so we don't worry about overflow or bad values.
   * Also, if the user has "turned down" alignment, don't try to
   * realign objects (pv 525474)
   */
  if ( Aggregate_Alignment > 0 ) {
    INT32 i = 1;
    while ( i < Aggregate_Alignment ) i <<= 1;
    Aggregate_Alignment = i;

    if (Aggregate_Alignment < (Target_int64 ? 8 : 4))
    {
      Align_Object = FALSE;
    }
  }

#if defined(BACK_END) || defined(FRONT_END) || defined (IR_TOOLS)
  Init_Targ_Sim();	/* must be done before initialize_stack_frame */
#endif

#define IS_POW2(n)              (((n) & ((n)-1))==0)
  FmtAssert (IS_POW2(Align_Instructions),
	     ("-OPT:align_instructions=<n> must equal power of two"));

  // Align instructions comes in bits, we need it in bytes
  Align_Instructions /= 8;
  return;
}

/* ====================================================================
 *
 * IPA_Configure_Target
 *
 * IPA-specific configuration.  Similar to Configure_Target but only set up
 * those variables that IPA cares.
 *
 * ====================================================================
 */
void
IPA_Configure_Target (void)
{
  Pointer_Size = 4;
  Pointer_Mtype  = WHIRL_Mtype_A_On ? MTYPE_A4 : MTYPE_U4;
  Pointer_type   = Pointer_Mtype;
  Pointer_Mtype2 = MTYPE_U4;
  Pointer_type2  = MTYPE_U4;

  Integer_type = MTYPE_I4;
  Boolean_type  = MTYPE_I4;
  Boolean_type2 = MTYPE_I4;

} /* IPA_Configure_Target */

/* ====================================================================
 *
 * Configure_Source_Target
 *
 * Reconfiguration of target-specific parameters for each source file.
 *
 * ====================================================================
 */

void
Configure_Source_Target ( char * /* filename */ )
{
  char *option;

  /* Enable zero-overhead loops at higher optimization levels. */
  if (!Enable_ZCL_overridden)
    Enable_ZCL = (Opt_Level >= 1);

  /* Unrolling defaults. OPT_unroll_size is the maximum unrolled size
     in ops. OPT_unroll_times is the maximum number of unrolled
     iterations. */
  if (!OPT_unroll_times_overridden)
    OPT_unroll_times = 2;
  if (!OPT_unroll_size_overridden)
    OPT_unroll_size = (OPT_Space) ? 4 : 32;

  /* Structure copy parameters. 'MinStructCopyLoopSize' and
     'MinStructCopyMemIntrSize' control how we lower MLOAD/MSTORE,
     into calls to memcpy, inlined loops, on inlined
     copies. 'CG_memmove_inst_count' controls when we convert memcpy,
     memset, etc. into inlined loops, or inlined copies. */

  if (OPT_Space || (Opt_Level == 0))
  {
    if (!CG_memmove_inst_count_overridden)
      CG_memmove_inst_count = 2;

    MinStructCopyLoopSize = 0;
    MinStructCopyMemIntrSize = CG_memmove_inst_count;
  }
  else if (Opt_Level <= 2)
  {
    if (!CG_memmove_inst_count_overridden)
      CG_memmove_inst_count = 8;

    MinStructCopyLoopSize = 0;
    MinStructCopyMemIntrSize = CG_memmove_inst_count;
  }
  else /* Opt_Level >= 3 */
  {
    if (!CG_memmove_inst_count_overridden)
      CG_memmove_inst_count = 32;

    MinStructCopyLoopSize = 0;
    MinStructCopyMemIntrSize = CG_memmove_inst_count;
  }

  if (OPT_Space && !xt_noswitchjump_seen && !Gen_PIC_Shared) {
    xt_switchjump = 1;
  }

  if (xt_switchjump && Gen_PIC_Shared) {
    ErrMsg(EC_Opt_Conflict, "-mswitchjump", "-fpic", "-mno-switchjump");
    xt_switchjump = 0;
  }

  /* Allow recip, rsqrt and sqrt only if the current Xtensa configuration
     supports them. Also, check the IEEE flag because recip and rsqrt are
     not IEEE-exact (as specified for xt_vectorfpu2005). */
  if (!Recip_Set)
    Recip_Allowed = (IEEE_Arithmetic >= IEEE_INEXACT) && xt_hard_float_recip;
  
  if (!Rsqrt_Set)
    Rsqrt_Allowed = (IEEE_Arithmetic >= IEEE_INEXACT) && xt_hard_float_rsqrt;
  
  if (!Sqrt_Set)
    Sqrt_Allowed = xt_hard_float_sqrt;
  
  if (!Sqrt_Split_Set) {
    Sqrt_Split_Allowed = 
      ((IEEE_Arithmetic >= IEEE_ANY) && !Sqrt_Allowed && Recip_Allowed && Rsqrt_Allowed);
  }

  /* Enable fused floating-point multiply-add if explicitly requested, or
     if IEEE exact arithmetic is not required. */
  Madd_Allowed = (xt_fused_madd_set ?
		  xt_fused_madd :
		  ((IEEE_Arithmetic >= IEEE_INEXACT) && xt_hard_float));

  /* Can't use HiFi2 if it's not available. */
  if (Enable_HiFi2_Ops)
    Enable_HiFi2_Ops = xt_hifi2;
  
  /* Xtensa doesn't support these (yet)... */
  Indexed_Loads_Allowed = FALSE;
  CIS_Allowed = FALSE;
  WOPT_Enable_DIVREM = FALSE;
  WOPT_Enable_Bits_Load_Store = FALSE;

  // Xtensa doesn't benefit from EXTRACT_BITS/COMPOSE_BITS operators
  Enable_extract_compose = FALSE;

  if (!xt_l32r && xt_const16) {
    xt_prefer_const16 = TRUE;
  }

  if (xt_prefer_const16 && Gen_PIC_Shared) {
    ErrMsg(EC_Opt_Conflict, "-prefer-const16", "-fpic", "-prefer-const16");
    Gen_PIC_Shared = 0;
  }
}

/* return FALSE if abi mismatch */
extern BOOL
Set_Target_ABI (BOOL is_64bit, INT isa)
{
  if (is_64bit
      || (isa != TARGET_ISA_XTENSA))
    return FALSE;

  switch (Target_ABI) {

  case ABI_WINDOWED:
    Pass_Static_Link_On_Stack = TRUE;
    break;
  case ABI_CALL0:
    Pass_Static_Link_On_Stack = FALSE;
    break;
  case ABI_UNDEF:
    FmtAssert(FALSE, ("No ABI specified"));
    break;
  default:
    return FALSE;
  }

  return TRUE;
}
// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
