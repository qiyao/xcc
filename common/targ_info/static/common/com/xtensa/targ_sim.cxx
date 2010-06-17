
/* 
   Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.
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


/*
 * This defines the ABI subprogram interface,
 * and is used to determine how parameters and results are passed.
 * The register values are the PREG offsets, so these values can be
 * used in WHIRL.
 */

#include <limits.h>
#include "defs.h"
#include "mtypes.h"
#include "errors.h"
#include "erglob.h"
#include "stab.h"
#include "config_targ.h"
#include "targ_sim.h"
#include "libti.h"


/* For each regclass, this struct holds information about the pregs
   representing that regclass. */
typedef struct rc_preg {
  ISA_REGCLASS regclass;
  
  /* Pregs representing registers in 'regclass'. A preg represents
     this regclass if 'preg_first <= preg <= preg_last. */
  PREG_NUM preg_first;
  PREG_NUM preg_last;

  /* Pregs representing registers used to pass arguments. If no
     registers can be used to pass arguments, then both 'arg_first'
     and 'arg_last' = -1. */
  PREG_NUM arg_first;
  PREG_NUM arg_last;

  /* Pregs representing registers used to return values. If no
     registers can be used to return values, then both 'ret_first'
     and 'ret_last' = -1. */
  PREG_NUM ret_first;
  PREG_NUM ret_last;

} RC_PREG;


/* Preg information for each register class. */
static RC_PREG preg_info[TI_ISA_REGCLASS_MAX + 1];

/* Register classes representing integer and float register
   files. Cached for fast access. */
static ISA_REGCLASS pregrc_int = ISA_REGCLASS_UNDEFINED;
static ISA_REGCLASS pregrc_float = ISA_REGCLASS_UNDEFINED;

/* Map from preg number to register class represented by the preg. */
static ISA_REGCLASS rc_map[(TI_ISA_REGCLASS_MAX + 1)*(TI_ISA_REGISTER_MAX + 1)];

/* Is the preg information initialized? */
static BOOL initialized = FALSE;

/* Max size, in bits, of a structure result that can be returned in
   registers. */
static INT max_struct_result;

/* Largest integer and float types that can be held in integer and
   float registers. */
static TYPE_ID int_type = MTYPE_I4;
static TYPE_ID flt_type = MTYPE_F4;

/* Parameters to computer windowing of argument registers. These
   values assume use of call8, will need to revisit if we want to
   allow call[0,4,8,12]. */
static PREG_NUM Input_Base_Preg[(TI_ISA_REGCLASS_MAX + 1)];
static PREG_NUM Output_Base_Preg[(TI_ISA_REGCLASS_MAX + 1)];


PREG_NUM Last_Dedicated_Preg_Offset;
PREG_NUM Stack_Pointer_Preg_Offset;
PREG_NUM Frame_Pointer_Preg_Offset;
PREG_NUM Static_Link_Preg_Offset;

BOOL Is_Caller_Save_GP = TRUE;
INT Formal_Save_Area_Size = 24;


extern void 
Init_Targ_Sim (void)
{
  /* Preg offsets for each regclass are assigned in the order the
     regclasses occur in targ_info. */

  Stack_Pointer_Preg_Offset = (PREG_NUM)-1;
  Frame_Pointer_Preg_Offset = (PREG_NUM)-1;
  Static_Link_Preg_Offset = (PREG_NUM)-1;

  /* There is only windowing on the ar registers. If we ever allow
     windowing of tie registers, we will need to move some logic into
     targ_info. */

  memset(Input_Base_Preg, 0, sizeof(Input_Base_Preg));
  memset(Output_Base_Preg, 0, sizeof(Output_Base_Preg));
  //this variable is slightly misnamed, it is really the size
  //of the window rotation
  Output_Base_Preg[TI_ISA_Regclass_Integer()] = 
    Target_ABI == ABI_WINDOWED ? 8 : 0;

  PREG_NUM curPreg = 0;
  ISA_REGCLASS rclass;

  FOR_ALL_ISA_REGCLASS(rclass)
    {
      RC_PREG *rcp = preg_info + rclass;
      const ISA_REGCLASS_INFO *rcinfo = TI_ISA_Regclass_Info(rclass);
      INT first_isa_reg  = TI_ISA_Regclass_First_Reg(rcinfo);
      INT last_isa_reg   = TI_ISA_Regclass_Last_Reg(rcinfo);
      INT rcnt = last_isa_reg - first_isa_reg + 1;

      rcp->regclass = rclass;
      rcp->preg_first = curPreg;
      rcp->preg_last = curPreg + rcnt - 1;

      rcp->arg_first = -1;
      rcp->arg_last = -1;
      rcp->ret_first = -1;
      rcp->ret_last = -1;

      /* Examine each register's abi properties to see if it is an
         argument or return register. We also check here to make sure
         argument (return) registers are contiguous (since that is
         what we can currently represent in RC_PREG). */
      
      for ( UINT i = 0; i < rcnt; ++i )
      {
	INT isa_reg = i + first_isa_reg;

	/* Function args... */
	
	if (TI_ABI_Property_Set(ABI_PROPERTY_func_arg, rclass, isa_reg))
	{
	  if (rcp->arg_first == -1)
	  {
	    rcp->arg_first = rcp->arg_last = i + curPreg;
	  }
	  else
	  {
	    FmtAssert(rcp->arg_last == ( i + curPreg - 1),
		      ("Expecting contiguous argument registers for "
		       "register class %s.", TI_ISA_Regclass_Name(rcinfo)));
	    rcp->arg_last = i + curPreg;
	  }
	}
	
	/* Function return values... */

	if (TI_ABI_Property_Set(ABI_PROPERTY_func_val, rclass, isa_reg))
	{
	  if (rcp->ret_first == -1)
	  {
	    rcp->ret_first = rcp->ret_last = i + curPreg;
	  }
	  else
	  {
	    FmtAssert(rcp->ret_last == (i + curPreg - 1),
		      ("Expecting contiguous return registers for "
		       "register class %s.", TI_ISA_Regclass_Name(rcinfo)));
	    rcp->ret_last = i + curPreg;
	  }
	}

	/* Find stack point, frame pointer, static link. */

	if (TI_ABI_Property_Set(ABI_PROPERTY_stack_ptr, rclass, isa_reg))
	{
	  Is_True(Stack_Pointer_Preg_Offset == (PREG_NUM)-1,
		  ("Expecting one stack pointer!"));
	  Stack_Pointer_Preg_Offset = i + curPreg;
	}

	if (TI_ABI_Property_Set(ABI_PROPERTY_frame_ptr, rclass, isa_reg))
	{
	  Is_True(Frame_Pointer_Preg_Offset == (PREG_NUM)-1,
		  ("Expecting one frame pointer!"));
	  Frame_Pointer_Preg_Offset = i + curPreg;
	}

	if (TI_ABI_Property_Set(ABI_PROPERTY_static_link, rclass, isa_reg))
	{
	  Is_True(Static_Link_Preg_Offset == (PREG_NUM)-1,
		  ("Expecting one static link!"));
	  Static_Link_Preg_Offset = i + curPreg;
	}

	/* Initialize the map from dedicated preg to regclass. */

	rc_map[curPreg + i] = rclass;
      }

      /* For efficiency, remember the integer and float regclasses,
         since these are frequently queried. If the float regclass is
         the same as the integer, then we don't want to show us having
         any float pregs. (that's why we have the second if as an
         else). */

      if (rclass == TI_ISA_Regclass_Integer())
	pregrc_int = rclass;
      else if (rclass == TI_ISA_Regclass_Float())
	pregrc_float = rclass;
      
      curPreg += rcnt;
    }

  Is_True(Stack_Pointer_Preg_Offset != (PREG_NUM)-1, ("Stack pointer preg required"));
  Is_True(Frame_Pointer_Preg_Offset != (PREG_NUM)-1, ("Frame pointer preg required"));
  Is_True(Static_Link_Preg_Offset != (PREG_NUM)-1, ("Static link preg required"));

  Is_True(curPreg <= (sizeof(rc_map) / sizeof(ISA_REGCLASS)),
	  ("Too many dedicated pregs, max allowed = %d, got %d.", 
	   sizeof(rc_map) / sizeof(ISA_REGCLASS), curPreg));

  /* We used to set 'Last_Dedicated_Preg_Offset' to 'curPreg' - 1, to
     exactly match the number of dedicated register we actually
     have. However, to support autotie with ipa, the first dedicated
     preg must be the same no matter what tie we have. So we now set
     'Last_Dedicated_Preg_Offset' to the maximum possible. */
  Last_Dedicated_Preg_Offset = (sizeof(rc_map) / sizeof(ISA_REGCLASS)) - 1;

  const ISA_REGCLASS_INFO *rcinfo = TI_ISA_Regclass_Info(pregrc_int);
  max_struct_result = TI_ISA_Regclass_Bit_Size(rcinfo)
    * (preg_info[pregrc_int].ret_last - preg_info[pregrc_int].ret_first + 1);

  initialized = TRUE;
}


PREG_NUM
Preg_Input_Base (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  return Input_Base_Preg[rc];
}

PREG_NUM
Preg_Output_Base (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  return Output_Base_Preg[rc];
}

BOOL
Preg_Is_Dedicated (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  return preg <= Last_Dedicated_Preg_Offset;
}

BOOL
Preg_Is_Dedicated_Integer (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  return((pregrc_int != ISA_REGCLASS_UNDEFINED)
	 && (preg >= Dedicated_Preg_First(pregrc_int))
	 && (preg <= Dedicated_Preg_Last(pregrc_int)));
}

BOOL
Preg_Is_Dedicated_Float (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  return((pregrc_float != ISA_REGCLASS_UNDEFINED)
	 && (preg >= Dedicated_Preg_First(pregrc_float))
	 && (preg <= Dedicated_Preg_Last(pregrc_float)));
}

BOOL
Preg_Is_Dedicated_Incoming_Ret (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  if (Preg_Is_Dedicated(preg) && (preg >= 0))
  {
    ISA_REGCLASS rc = Dedicated_Preg_Regclass(preg);
    if ((rc != ISA_REGCLASS_UNDEFINED) &&
	Regclass_Has_Ret_Registers(rc))
    {
      PREG_NUM first = Dedicated_Preg_First_Incoming_Ret(rc);
      PREG_NUM last = Dedicated_Preg_Last_Incoming_Ret(rc);
      return (preg >= first) && (preg <= last);
    }
  }

  return FALSE;
}

BOOL
Preg_Is_Dedicated_Outgoing_Ret (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  if (Preg_Is_Dedicated(preg) && (preg >= 0))
  {
    ISA_REGCLASS rc = Dedicated_Preg_Regclass(preg);
    if ((rc != ISA_REGCLASS_UNDEFINED) &&
	Regclass_Has_Ret_Registers(rc))
    {
      PREG_NUM first = Dedicated_Preg_First_Outgoing_Ret(rc);
      PREG_NUM last = Dedicated_Preg_Last_Outgoing_Ret(rc);
      return (preg >= first) && (preg <= last);
    }
  }

  return FALSE;
}

BOOL
Preg_Is_Dedicated_Incoming_Arg (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  if (Preg_Is_Dedicated(preg) && (preg >= 0))
  {
    ISA_REGCLASS rc = Dedicated_Preg_Regclass(preg);
    if ((rc != ISA_REGCLASS_UNDEFINED) &&
	Regclass_Has_Arg_Registers(rc))
    {
      PREG_NUM first = Dedicated_Preg_First_Incoming_Arg(rc);
      PREG_NUM last = Dedicated_Preg_Last_Incoming_Arg(rc);
      return (preg >= first) && (preg <= last);
    }
  }

  return FALSE;
}

BOOL
Preg_Is_Dedicated_Outgoing_Arg (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  if (Preg_Is_Dedicated(preg) && (preg >= 0))
  {
    ISA_REGCLASS rc = Dedicated_Preg_Regclass(preg);
    if ((rc != ISA_REGCLASS_UNDEFINED) &&
	Regclass_Has_Arg_Registers(rc))
    {
      PREG_NUM first = Dedicated_Preg_First_Outgoing_Arg(rc);
      PREG_NUM last = Dedicated_Preg_Last_Outgoing_Arg(rc);
      return (preg >= first) && (preg <= last);
    }
  }

  return FALSE;
}

ISA_REGCLASS
Dedicated_Preg_Regclass (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  Is_True(Preg_Is_Dedicated(preg), ("Expecting dedicated preg, got %d.", preg));
  return ((preg >= 0) ? rc_map[preg] : ISA_REGCLASS_UNDEFINED);
}

INT
Dedicated_Preg_Isa_Reg (PREG_NUM preg)
{
  Is_True(initialized, ("Target abi not initialized"));
  Is_True(Preg_Is_Dedicated(preg), ("Expecting dedicated preg, got %d.", preg));

  ISA_REGCLASS rclass = Dedicated_Preg_Regclass(preg);
  if (rclass == ISA_REGCLASS_UNDEFINED)
    return -1;
  
  return preg - Dedicated_Preg_First(rclass);
}

PREG_NUM
Dedicated_Preg_First (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  return preg_info[rc].preg_first;
}

PREG_NUM
Dedicated_Preg_Last (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  return preg_info[rc].preg_last;
}

BOOL
Regclass_Has_Arg_Registers (ISA_REGCLASS rc)
{
  return((preg_info[rc].arg_first != -1) && (preg_info[rc].arg_last != -1));
}

BOOL
Regclass_Has_Ret_Registers (ISA_REGCLASS rc)
{
  return((preg_info[rc].ret_first != -1) && (preg_info[rc].ret_last != -1));
}

PREG_NUM
Dedicated_Preg_First_Incoming_Arg (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].arg_first != -1,
	    ("no incoming args for regclass %d", rc));
  return preg_info[rc].arg_first + Input_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_Last_Incoming_Arg (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].arg_last != -1,
	    ("no incoming args for regclass %d", rc));
  return preg_info[rc].arg_last + Input_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_First_Incoming_Ret (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].ret_first != -1,
	    ("no incoming ret for regclass %d", rc));
  return preg_info[rc].ret_first + Output_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_Last_Incoming_Ret (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].ret_last != -1,
	    ("no incoming ret for regclass %d", rc));
  return preg_info[rc].ret_last + Output_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_First_Outgoing_Arg (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].arg_first != -1,
	    ("no outgoing args for regclass %d", rc));
  return preg_info[rc].arg_first + Output_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_Last_Outgoing_Arg (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].arg_last != -1,
	    ("no outgoing args for regclass %d", rc));
  return preg_info[rc].arg_last + Output_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_First_Outgoing_Ret (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].ret_first != -1,
	    ("no outgoing ret for regclass %d", rc));
  return preg_info[rc].ret_first + Input_Base_Preg[rc];
}

PREG_NUM
Dedicated_Preg_Last_Outgoing_Ret (ISA_REGCLASS rc)
{
  Is_True(initialized, ("Target abi not initialized"));
  FmtAssert(preg_info[rc].ret_last != -1,
	    ("no outgoing ret for regclass %d", rc));
  return preg_info[rc].ret_last + Input_Base_Preg[rc];
}


UINT
Stack_Size_Adjustment (BOOL frame_has_calls)
{
  
#if 0
  /* We always need an extra 16 bytes to hold the caller's a0-a3. If
     we have calls, then we need an extra 16 bytes to hold our a4-a7
     (since we are only using call8's right now). */
  return frame_has_calls ? 32 : 16;
#else
  /* For now, we always want to reserve 32 bytes even for a leaf
     function, so that we can do a call from a leaf routine in the
     debugger without clobbering the stack on a subsequent
     overflow. */
  Is_True((Target_ABI == ABI_WINDOWED) || (Target_ABI == ABI_CALL0), ("Illegal ABI"));

  if (Target_ABI == ABI_WINDOWED)
    return 32;
  else 
    return 0;
#endif
}


/*
 *  check for array case where fe doesn't fill in right btype
 */
TYPE_ID
Fix_TY_mtype (TY_IDX ty)
{
  TYPE_ID type = TY_mtype (ty);
  if (type == MTYPE_UNKNOWN && TY_kind (ty) == KIND_ARRAY)
    type = Pointer_Mtype;
  else if (MTYPE_is_complex(type) && TY_kind (ty) == KIND_STRUCT)
    type = MTYPE_M;

  return type;
}


RETURN_INFO
Get_Return_Info (TY_IDX rtype,
		 Mtype_Return_Level level,
		 Return_Info_Dir dir)
{
  TYPE_ID mtype = TY_mtype (rtype);
  RETURN_INFO info;
  info.return_via_first_arg = FALSE;

  PREG_NUM first_int_ret = ((dir == Return_Info_Outgoing) ?
			    Dedicated_Preg_First_Outgoing_Ret(pregrc_int) :
			    Dedicated_Preg_First_Incoming_Ret(pregrc_int));

  switch (mtype)
  {
  case MTYPE_UNKNOWN:
    // FORTRAN character array
    info.count = 0;
    // f90 already has made visible the arg for arrays
    // info.return_via_first_arg = TRUE;
    break;

  case MTYPE_V:
    info.count = 0;
    break;

  case MTYPE_I1:
  case MTYPE_I2:
  case MTYPE_I4:
  case MTYPE_U1:
  case MTYPE_U2:
  case MTYPE_U4:
  case MTYPE_A4:
    info.count = 1;
    info.mtype [0] = mtype;
    info.preg  [0] = first_int_ret;
    break;

  case MTYPE_I8:
    if( level != No_Simulated ) {
      info.count = 1;
      info.mtype [0] = MTYPE_I8;
      info.preg  [0] = first_int_ret;
    } else {
      info.count = 2;
      info.mtype [0] = MTYPE_I4;
      info.mtype [1] = MTYPE_I4;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 1;
    }
    break;

  case MTYPE_U8:
    if( level != No_Simulated ) {
      info.count = 1;
      info.mtype [0] = MTYPE_U8;
      info.preg  [0] = first_int_ret;
    } else {
      info.count = 2;
      info.mtype [0] = MTYPE_U4;
      info.mtype [1] = MTYPE_U4;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 1;
    }
    break;

  case MTYPE_A8:
    FmtAssert(FALSE, ("64-bit addressing not supported"));
    break;

  case MTYPE_F4:
    info.count = 1;
    info.mtype [0] = mtype;
    info.preg  [0] = first_int_ret;
    break;

  case MTYPE_F8:
    if( level != No_Simulated ) {
      info.count = 1;
      info.mtype [0] = MTYPE_F8;
      info.preg  [0] = first_int_ret;
    } else {
      info.count = 2;
      info.mtype [0] = MTYPE_F4;
      info.mtype [1] = MTYPE_F4;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 1;
    }
    break;

  case MTYPE_FQ:
    FmtAssert(FALSE, ("128-bit floating-point not supported"));
    break;

  case MTYPE_C4:
    if( level == Use_Simulated ) {
      info.count = 1;
      info.mtype [0] = MTYPE_C4;
      info.preg  [0] = first_int_ret;
    } else {
      info.count     = 2;
      info.mtype [0] = MTYPE_F4;
      info.mtype [1] = MTYPE_F4;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 1;
    }
    break;

  case MTYPE_C8:
    if( level == Use_Simulated ) {
      info.count = 1;
      info.mtype [0] = MTYPE_C8;
      info.preg  [0] = first_int_ret;
    }
    else if( level == Complex_Not_Simulated ) {
      info.count     = 2;
      info.mtype [0] = MTYPE_F8;
      info.mtype [1] = MTYPE_F8;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 2;
    } else {
      info.count     = 4;
      info.mtype [0] = MTYPE_F4;
      info.mtype [1] = MTYPE_F4;
      info.mtype [2] = MTYPE_F4;
      info.mtype [3] = MTYPE_F4;
      info.preg  [0] = first_int_ret;
      info.preg  [1] = info.preg[0] + 1;
      info.preg  [2] = info.preg[0] + 2;
      info.preg  [3] = info.preg[0] + 3;
    }
    break;

  case MTYPE_CQ:
    FmtAssert(FALSE, ("128-bit complex not supported"));
    break;

  case MTYPE_M:
  {
    info.count = 0;
    info.return_via_first_arg = TRUE;

    /* Try to return the entire struct in registers. */

    const UINT64 size = TY_size(Ty_Table[rtype]);
    const UINT64 bit_size = size * 8;
      
    if (bit_size <= max_struct_result)
    {
      UINT n = ((size + MTYPE_RegisterSize(int_type) - 1)
		/ MTYPE_RegisterSize(int_type));
      PREG_NUM reg = first_int_ret;

      info.return_via_first_arg = FALSE;
      info.count = n;

      for (int i = 0; i < n; i++)
      {
	info.mtype [i] = int_type;
	info.preg  [i] = reg++;
      }
    }

    break;
  }    

  case MTYPE_XTBOOL:
  case MTYPE_XTBOOL2:
  case MTYPE_XTBOOL4:
  case MTYPE_XTBOOL8:
  case MTYPE_XTBOOL16:
  {
    ISA_REGCLASS pregrc_tie = TI_ISA_Regclass_For_Mtype(mtype);
    PREG_NUM first_tie_ret = ((dir == Return_Info_Outgoing) ?
			    Dedicated_Preg_First_Outgoing_Ret(pregrc_tie) :
			    Dedicated_Preg_First_Incoming_Ret(pregrc_tie));
    info.count = 1;
    info.mtype [0] = mtype;
    info.preg  [0] = first_tie_ret;
    break;
  }

  default:
    if (MTYPE_is_tie(mtype)) {
      ISA_REGCLASS pregrc_tie = TI_ISA_Regclass_For_Mtype(mtype);
      PREG_NUM first_tie_ret = ((dir == Return_Info_Outgoing) ?
			    Dedicated_Preg_First_Outgoing_Ret(pregrc_tie) :
			    Dedicated_Preg_First_Incoming_Ret(pregrc_tie));
      info.count = 1;
      info.mtype [0] = mtype;
      info.preg  [0] = first_tie_ret;
      break;
    }
    info.count = 0;
    Fail_FmtAssertion ("Invalid return mtype %s encountered",
		       (MTYPE_name(mtype)));
    break;
  }

  for (UINT i = info.count; i < MAX_NUMBER_OF_REGISTERS_FOR_RETURN; i++)
  {
    info.mtype [i] = MTYPE_V;
    info.preg  [i] = 0;
  }

  return info;
}


/* Location of the next parameter register. The first 'Num_Param_Regs'
   are in registers and the rest are in memory. */
static UINT Next_Param_Loc = 0;

/* Number of integer parameter registers. */
static UINT Num_Param_Regs;

/* 2^Register_Exp == size of an integer register. */
static UINT Register_Exp;


static PLOC
Setup_Parameter_Locations (TY_IDX pu_type)
{
  static PLOC plocNULL;

  Next_Param_Loc = 0;

  UINT sz = MTYPE_RegisterSize(int_type);
  for (Register_Exp = 0; (sz & 1) == 0; Register_Exp++)
    sz = sz >> 1;

  Num_Param_Regs = (Dedicated_Preg_Last_Outgoing_Arg(pregrc_int) -
		    Dedicated_Preg_First_Outgoing_Arg(pregrc_int) + 1);

  return plocNULL;
}


PLOC
Setup_Input_Parameter_Locations (TY_IDX pu_type)
{
  return Setup_Parameter_Locations (pu_type);
}


PLOC
Setup_Output_Parameter_Locations (TY_IDX pu_type)
{
  return Setup_Parameter_Locations (pu_type);
}


static inline UINT
Aligned_Param_Loc (UINT curr_loc, UINT align_exp)
{
  /* Find a parameter location (word index, starting at word
     'curr_loc') that has alignment of at least 'align_exp'. */

  /* The first register location meets all alignment requirements. */

  if (curr_loc == 0)
    return curr_loc;
  
  /* The first stack location meets all alignment requirements. */

  if (curr_loc == Num_Param_Regs)
    return curr_loc;

  while (TRUE)
  {
    /* Find the alignment exponent of curr_loc. */

    UINT curr_align_exp = 0;
    UINT loc = curr_loc;
    /* Parameter locations become aligned again when they switch
       from the registers to the stack, so adjust for that condition */
    if (loc >= Num_Param_Regs)
      loc += 2;
    while ((loc & 0x01) == 0)
    {
      loc = loc >> 1;
      curr_align_exp++;
    }

    if ((curr_align_exp + Register_Exp) >= align_exp)
      break;

    curr_loc++;
  }

  return curr_loc;
}


static PLOC
Get_Parameter_Location (TY_IDX ty)
{
  PLOC ploc;

  PLOC_reg(ploc) = -1;
  PLOC_offset(ploc) = 0;
  PLOC_size(ploc) = 0;
  PLOC_vararg_reg(ploc) = 0;

  if (TY_kind (ty) == KIND_VOID)
    return ploc;

  /* Find the actual size of the parameter, and the number of words
     required to hold it. */

  TYPE_ID pmtype = Fix_TY_mtype (ty);
  PLOC_size(ploc) = ((pmtype == MTYPE_M) ? TY_size (ty) : MTYPE_RegisterSize(pmtype));
  UINT words = (PLOC_size(ploc) + MTYPE_RegisterSize(int_type) - 1) / MTYPE_RegisterSize(int_type);

  /* Find a parameter location that has alignment at least as large as
     'ty' (parameter locations start in registers and then continue in
     memory). */

  UINT first_param_word = Aligned_Param_Loc(Next_Param_Loc, TY_align_exp(ty));
  
  /* If this parameter fits completely in registers, then return the
     preg of the first argument register. */

  if ((first_param_word + words) <= Num_Param_Regs)
  {
    PLOC_reg(ploc) = Dedicated_Preg_First_Incoming_Arg(pregrc_int) + first_param_word;
    PLOC_vararg_reg(ploc) = PLOC_reg(ploc);
  }
  else
  {
    /* If the parameter is paritally in registers and memory, then we
       must make it completely in memory. */
    if (first_param_word <= Num_Param_Regs)
      first_param_word = Num_Param_Regs;
  }

  /* Set the stack offset for this parameter. Adjust offset for big
     endian targets when the parameter is smaller than the
     register. */

  PLOC_offset(ploc) = first_param_word * MTYPE_RegisterSize(int_type);
  
  if ((Target_Byte_Sex == BIG_ENDIAN)
      /*&& (pmtype != MTYPE_M)*/
      && (PLOC_size(ploc) < MTYPE_RegisterSize(int_type)))
  {
    PLOC_offset(ploc) += (MTYPE_RegisterSize(int_type) - PLOC_size(ploc));
  }

  Next_Param_Loc = first_param_word + words;

  return ploc;
}


PLOC
Get_Input_Parameter_Location (TY_IDX ty)
{
  PLOC ploc = Get_Parameter_Location (ty);
  if (Preg_Is_Dedicated_Integer(PLOC_reg(ploc)))
    PLOC_reg(ploc) = Input_Base_Preg[pregrc_int] + PLOC_reg(ploc);
  return ploc;
}


PLOC
Get_Output_Parameter_Location (TY_IDX ty)
{
  PLOC ploc = Get_Parameter_Location (ty);
  if (Preg_Is_Dedicated_Integer(PLOC_reg(ploc)))
    PLOC_reg(ploc) = Output_Base_Preg[pregrc_int] + PLOC_reg(ploc);
  return ploc;
}


static UINT Ploc_Remaining_Size;

static PLOC
First_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
  PLOC first = ploc;

  if (PLOC_size(first) > MTYPE_RegisterSize(int_type))
    PLOC_size(first) = MTYPE_RegisterSize(int_type);

  Ploc_Remaining_Size = PLOC_size(ploc) - PLOC_size(first);

  return first;
}


PLOC
First_Input_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
  return First_PLOC_Reg (ploc, parm_ty);
}


PLOC
First_Output_PLOC_Reg (PLOC ploc, TY_IDX parm_ty)
{
  return First_PLOC_Reg (ploc, parm_ty);
}


static PLOC
Next_PLOC_Reg (PLOC prev, PREG_NUM last_arg_reg)
{
  PLOC next = prev;

  /* If there is nothing remaining of this parameter, then return a
     0-sized ploc to indicate that we are done. */

  if (Ploc_Remaining_Size == 0)
  {
    PLOC_size(next) = 0;
    return next;
  }

  /* Reduce the size of parameter remaining... */
  
  if (Ploc_Remaining_Size > MTYPE_RegisterSize(int_type))
    PLOC_size(next) = MTYPE_RegisterSize(int_type);
  else
    PLOC_size(next) = Ploc_Remaining_Size;
    
  Ploc_Remaining_Size -= PLOC_size(next);

  /* If the parameter is being passed in a register, increment the
     register number. If being passed in memory, increment the
     offset. */

  if (!PLOC_on_stack(next))
  {
    PLOC_reg(next)++;
    FmtAssert(PLOC_reg(next) <= last_arg_reg,
	      ("Parameter spans registers and memory"));
  }
  else
  {
    PLOC_offset(next) += PLOC_size(prev);
  }

  return next;
}


PLOC
Next_Input_PLOC_Reg (PLOC prev)
{
  return Next_PLOC_Reg (prev, Dedicated_Preg_Last_Incoming_Arg(pregrc_int));
}


PLOC
Next_Output_PLOC_Reg (PLOC prev)
{
  return Next_PLOC_Reg (prev, Dedicated_Preg_Last_Outgoing_Arg(pregrc_int));
}


PLOC
Get_Vararg_Input_Parameter_Location (PLOC prev)
{
  PLOC next = prev;

  PLOC_size(next) = MTYPE_RegisterSize(int_type);

  if (!PLOC_on_stack(next))
  {
    PLOC_reg(next)++;
    if (PLOC_reg(next) > Dedicated_Preg_Last_Incoming_Arg(pregrc_int))
    {
      PLOC_reg(next) = -1;
      PLOC_size(next) = 0;
    }
  }

  PLOC_vararg_reg(next) = PLOC_reg(next);
  PLOC_offset(next) += MTYPE_RegisterSize(int_type);

  return next;
}


static TY_IDX Struct_Ty;
static BOOL Struct_First;

static void
Setup_Struct_Parameter_Locations (TY_IDX struct_ty)
{
  Struct_Ty = struct_ty;
  Struct_First = TRUE;
}


void
Setup_Struct_Input_Parameter_Locations (TY_IDX struct_ty)
{
  Setup_Struct_Parameter_Locations (struct_ty);
}


void
Setup_Struct_Output_Parameter_Locations (TY_IDX struct_ty)
{
  Setup_Struct_Parameter_Locations (struct_ty);
}


static PLOC 
Get_Struct_Parameter_Location (PLOC prev, PREG_NUM last_arg_reg)
{
  if (Struct_First)
  {
    Struct_First = FALSE;
    return First_PLOC_Reg(prev, Struct_Ty);
  }

  return Next_PLOC_Reg(prev, last_arg_reg);
}


PLOC 
Get_Struct_Input_Parameter_Location (PLOC prev)
{
  return Get_Struct_Parameter_Location (prev, Dedicated_Preg_Last_Incoming_Arg(pregrc_int));
}


PLOC 
Get_Struct_Output_Parameter_Location (PLOC prev)
{
  return Get_Struct_Parameter_Location (prev, Dedicated_Preg_Last_Outgoing_Arg(pregrc_int));
}


/* This routine figures out the mtypes of the return registers that are 
 * used for returning an object of the given type.
 * This returns the mtypes to use for the CALL opcode in high-level whirl.
 * This means that returns of simulated objects, like FQ, are just shown
 * as returning FQ, which will later be split into F8F8.
 * However, structures that return in registers are specified explicitly.
 */
/*ARGSUSED*/
extern void
Get_Return_Mtypes (
  TY_IDX rtype,		/* The result type */
  Mtype_Return_Level level,	/* whether to lower the mtypes */
  TYPE_ID *mreg1,	/* out: mtype for result register 1 */
  TYPE_ID *mreg2)	/* out: mtype for result register 2 */
{
  Fail_FmtAssertion (
    ("Get_Return_Mtypes should not be invoked; invoke Get_Return_Info instead"));
}

/* This routine figures out which return registers are to be used
 * for returning an object with the given mtypes.
 * It is assumed that the mtypes will be determined by calling
 * Get_Return_Mtypes.
 */
/*ARGSUSED*/
extern void
Get_Return_Pregs (
  TYPE_ID mreg1,	/* in:  mtype for result register 1 */
  TYPE_ID mreg2,	/* in:  mtype for result register 2 */
  PREG_NUM *rreg1,	/* out: result register 1 */
  PREG_NUM *rreg2)	/* out: result register 2 */
{
  Fail_FmtAssertion (
    ("Get_Return_Pregs should not be invoked; invoke Get_Return_Info instead"));
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
