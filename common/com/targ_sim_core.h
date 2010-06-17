
/* 
   Copyright (C) 2002-2004 Tensilica, Inc.  All Rights Reserved.
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


#ifndef targ_sim_core_INCLUDED
#define targ_sim_core_INCLUDED

#include "targ_isa_registers.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This defines the ABI subprogram interface,
 * and is used to determine how parameters and results are passed.
 * The register values are the PREG offsets, so these values can be
 * used in WHIRL.
 */

/* The PREG_NUM of the last dedicated preg. */
extern DLL_SHARED PREG_NUM Last_Dedicated_Preg_Offset;

/* The PREG_NUM of the dedicated preg representing the stack pointer. */
extern DLL_SHARED PREG_NUM Stack_Pointer_Preg_Offset;

/* The PREG_NUM of the dedicated preg representing the frame pointer. */
extern DLL_SHARED PREG_NUM Frame_Pointer_Preg_Offset;

/* The PREG_NUM of the dedicated preg representing the static link
   register. */
extern DLL_SHARED PREG_NUM Static_Link_Preg_Offset;

/* Return the base preg for input/output arguments and return values. */
extern PREG_NUM Preg_Input_Base (ISA_REGCLASS rc);
extern PREG_NUM Preg_Output_Base (ISA_REGCLASS rc);

/* Does 'preg' represent a target processor register? */
extern BOOL Preg_Is_Dedicated (PREG_NUM preg);

/* Does 'preg' represent a target processor integer register? */
extern BOOL Preg_Is_Dedicated_Integer (PREG_NUM preg);
 
/* Does 'preg' represent a target processor float register? */
extern BOOL Preg_Is_Dedicated_Float (PREG_NUM preg);

/* Does 'preg' represent a register that can be used to hold incoming
   function results? */
extern BOOL Preg_Is_Dedicated_Incoming_Ret (PREG_NUM preg);

/* Does 'preg' represent a register that can be used to hold outgoing
   function results? */
extern BOOL Preg_Is_Dedicated_Outgoing_Ret (PREG_NUM preg);

/* Does 'preg' represent a register that can be used to hold an
   incoming function parameter? */
extern BOOL Preg_Is_Dedicated_Incoming_Arg (PREG_NUM preg);

/* Does 'preg' represent a register that can be used to hold an
   outgoing function parameter? */
extern BOOL Preg_Is_Dedicated_Outgoing_Arg (PREG_NUM preg);

/* Return the regclass represented by dedicated 'preg'. Error if
   'preg' is not a dedicated preg. */
extern ISA_REGCLASS Dedicated_Preg_Regclass (PREG_NUM preg);

/* Return the number of the isa register represented by dedicated
   'preg'. Error if 'preg' is not a dedicated preg. */
extern INT Dedicated_Preg_Isa_Reg (PREG_NUM preg);

/* Return the first dedicated preg representing regclass 'rc'. */
extern PREG_NUM Dedicated_Preg_First (ISA_REGCLASS rc);

/* Return the last dedicated preg representing regclass 'rc'. */
extern PREG_NUM Dedicated_Preg_Last (ISA_REGCLASS rc);

/* Return true if regclass 'rc' has dedicated argument registers. */  
extern BOOL Regclass_Has_Arg_Registers (ISA_REGCLASS rc);

/* Return true if regclass 'rc' has dedicated return registers. */  
extern BOOL Regclass_Has_Ret_Registers (ISA_REGCLASS rc);

/* Return the dedicated preg representing the first incoming argument
   register in regclass 'rc'. Error if 'rc' has no argument
   registers. */
extern PREG_NUM Dedicated_Preg_First_Incoming_Arg (ISA_REGCLASS rc);

/* Return the dedicated preg representing the last incoming argument
   register in regclass 'rc'. Error if 'rc' has no argument
   registers. */
extern PREG_NUM Dedicated_Preg_Last_Incoming_Arg (ISA_REGCLASS rc);

/* Return the dedicated preg representing the first incoming return
   register in regclass 'rc'. Error if 'rc' has no return
   registers. */
extern PREG_NUM Dedicated_Preg_First_Incoming_Ret (ISA_REGCLASS rc);

/* Return the dedicated preg representing the last incoming return
   register in regclass 'rc'. Error if 'rc' has no return
   registers. */
extern PREG_NUM Dedicated_Preg_Last_Incoming_Ret (ISA_REGCLASS rc);

/* Return the dedicated preg representing the first outgoing argument
   register in regclass 'rc'. Error if 'rc' has no argument
   registers. */
extern PREG_NUM Dedicated_Preg_First_Outgoing_Arg (ISA_REGCLASS rc);

/* Return the dedicated preg representing the last outgoing argument
   register in regclass 'rc'. Error if 'rc' has no argument
   registers. */
extern PREG_NUM Dedicated_Preg_Last_Outgoing_Arg (ISA_REGCLASS rc);

/* Return the dedicated preg representing the first outgoing return
   register in regclass 'rc'. Error if 'rc' has no return
   registers. */
extern PREG_NUM Dedicated_Preg_First_Outgoing_Ret (ISA_REGCLASS rc);

/* Return the dedicated preg representing the last outgoing return
   register in regclass 'rc'. Error if 'rc' has no return
   registers. */
extern PREG_NUM Dedicated_Preg_Last_Outgoing_Ret (ISA_REGCLASS rc);


/* Convert between preg indices and preg numbers. */  
/* PREG_IDX == PREG_NUM - Last_Dedicated_Preg_Offset */

inline PREG_IDX
Get_Preg_Idx (PREG_NUM n)
{
  return n - Last_Dedicated_Preg_Offset;
}
  
inline PREG_NUM
Get_Preg_Num (PREG_IDX i)
{
  return i + Last_Dedicated_Preg_Offset;
}


/* define an enumeration of the different levels of mtypes/whirl
 * that we can use for returns */
typedef enum {
  Use_Simulated,		/* use simulated types */
  Complex_Not_Simulated, 	/* lower complex but not quad */
  No_Simulated		/* all lowered to machine-level */
} Mtype_Return_Level;

/* enumeration used to Get_Return_Info to indicate if we want to
   incoming or outgoing return information. (from the point-of-view of
   the PU being compiled). */
typedef enum {
  Return_Info_Incoming,
  Return_Info_Outgoing
} Return_Info_Dir;

  
class RETURN_INFO {
private:
  mINT8    count;
  BOOL     return_via_first_arg;
  TYPE_ID  mtype [MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
  PREG_NUM preg  [MAX_NUMBER_OF_REGISTERS_FOR_RETURN];
public:
  friend inline mINT8 RETURN_INFO_count (const RETURN_INFO&);
  friend inline BOOL  RETURN_INFO_return_via_first_arg (const RETURN_INFO&);
  friend inline TYPE_ID  RETURN_INFO_mtype (const RETURN_INFO&, INT32);
  friend inline PREG_NUM RETURN_INFO_preg (const RETURN_INFO&, INT32);
  friend RETURN_INFO Get_Return_Info (TY_IDX rtype,
				      Mtype_Return_Level level,
				      Return_Info_Dir dir =Return_Info_Outgoing);
};

inline mINT8
RETURN_INFO_count (const RETURN_INFO& info) { return info.count; }

inline BOOL 
RETURN_INFO_return_via_first_arg (const RETURN_INFO& info)
{
  return info.return_via_first_arg;
}

inline TYPE_ID
RETURN_INFO_mtype (const RETURN_INFO& info, INT32 i) { return info.mtype [i]; }

inline PREG_NUM
RETURN_INFO_preg (const RETURN_INFO& info, INT32 i) { return info.preg [i]; }

/* This routine figures out the mtypes of the return registers that are
 * used for returning an object of the given type.
 * This returns the mtypes to use for the CALL opcode in high-level whirl.
 * This means that returns of simulated objects, like FQ, are just shown
 * as returning FQ, which will later be split into F8F8.
 * However, structures that return in registers are specified explicitly.
 * If a register is unused, MTYPE_V is returned.
 */
extern void Get_Return_Mtypes (
  TY_IDX rtype,		/* The result type */
  Mtype_Return_Level level,	/* whether to lower the mtypes */
  TYPE_ID *mreg1,	/* out: mtype for result register 1 */
  TYPE_ID *mreg2);	/* out: mtype for result register 2 */

/* This routine figures out which return registers are to be used
 * for returning an object with the given mtypes.
 * Preg 0 is returned for unused registers.
 * It is assumed that the mtypes will be determined by calling
 * Get_Return_Mtypes.
 */
extern void Get_Return_Pregs (
  TYPE_ID mreg1,	/* in:  mtype for result register 1 */
  TYPE_ID mreg2,	/* in:  mtype for result register 2 */
  PREG_NUM *rreg1,	/* out: result register 1 */
  PREG_NUM *rreg2);	/* out: result register 2 */


/* PLOC contains information about the location of a parameter.
 * If reg == -1, then the parameter is stored on the stack. 
 * For -DEBUG:varargs_prototypes=off a floating point parameter is
 * passed in both the floating point register and integer register.
 */
typedef struct {
	PREG_NUM _reg;
	INT32 _start_offset;
	INT32 _size;
	PREG_NUM _vararg_reg;
} PLOC;

#define PLOC_reg(p)		(p._reg)
#define PLOC_vararg_reg(p)	(p._vararg_reg)
#define PLOC_offset(p)		(p._start_offset)
#define PLOC_on_stack(p)	(p._reg == -1)
#define PLOC_total_size(p)	(p._start_offset+p._size)
#define PLOC_size(p)		(p._size)
#define PLOC_is_empty(p)	(PLOC_size(p) == 0)
#define PLOC_is_nonempty(p)	(PLOC_size(p) != 0)

/* 
 * When processing a parameter list,
 * we first call Setup_Parameter_Locations with the TY of the PU.
 * Setup_* returns a ploc initialized to zero's.
 * Then we iterate over each parameter with Get_Parameter_Location.
 * Get_Parameter_Location uses the parameter TY and the previous
 * location to determine the current location.  
 * Structures and register-pairs return the beginning location,
 * then First/Next_PLOC_Reg must be used to get info about each preg.
 * e.g.
    ploc = Setup_Parameter_Locations (call_ty);
    foreach parm
	ploc = Get_Parameter_Location (parm_ty);
    	ploc = First_PLOC_Reg (ploc, parm_ty);
    	while (PLOC_is_nonempty(ploc)) {
		do_something;
        	ploc = Next_PLOC_Reg (ploc);
 *
 */
extern PLOC Setup_Input_Parameter_Locations (TY_IDX pu_type);
extern PLOC Get_Input_Parameter_Location (TY_IDX ptype);

extern PLOC First_Input_PLOC_Reg (PLOC ploc, TY_IDX parm_ty);
extern PLOC Next_Input_PLOC_Reg (PLOC prev);

extern PLOC Setup_Output_Parameter_Locations (TY_IDX pu_type);
extern PLOC Get_Output_Parameter_Location (TY_IDX ptype);

extern PLOC First_Output_PLOC_Reg (PLOC ploc, TY_IDX parm_ty);
extern PLOC Next_Output_PLOC_Reg (PLOC prev);

/* Iterate over implicit vararg non-fixed register parameters */
extern PLOC Get_Vararg_Input_Parameter_Location (PLOC prev);

/*
 * When we have a structure or register-pair,
 * we need to break it into register-sized chunks.
 * So we first call Setup_Struct_Parameter_Locations with the structure TY,
 * then we iterate with Get_Struct_Parameter_Location until it
 * returns PLOC_is_empty.
 */
extern void Setup_Struct_Input_Parameter_Locations (TY_IDX struct_ty);
extern PLOC Get_Struct_Input_Parameter_Location ( PLOC prev );
extern void Setup_Struct_Output_Parameter_Locations (TY_IDX struct_ty);
extern PLOC Get_Struct_Output_Parameter_Location ( PLOC prev );

/*
 * TY_mtype() is not correct for structures/arrays.
 * This function will return a corrected TYPE_ID
 */
extern TYPE_ID Fix_TY_mtype(TY_IDX);

/*
 * The following variables give info about the calling convention,
 * and are set by Init_Targ_Sim()
 */
extern DLL_SHARED BOOL Is_Caller_Save_GP;	/* whether GP is caller-save */

/* Amount of space available in stack frame to save register formals. */
extern DLL_SHARED INT Formal_Save_Area_Size;

extern UINT Stack_Size_Adjustment (BOOL frame_has_calls);
extern void Init_Targ_Sim (void);	/* initialize the info */

#ifndef PUSH_RETURN_ADDRESS_ON_STACK
#define PUSH_RETURN_ADDRESS_ON_STACK FALSE
#endif

#ifndef PUSH_FRAME_POINTER_ON_STACK
#define PUSH_FRAME_POINTER_ON_STACK  FALSE
#endif

#ifdef __cplusplus
}
#endif
#endif /* targ_sim_core_INCLUDED */

