
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
 * Module: register.c
 * $Revision: 3.77 $
 * $Date: 2000/04/06 01:53:20 $
 * $Author: mtibuild $
 * $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/register.cxx,v $
 *
 * Revision history:
 *  17-May-93 - Original Version
 *
 * Description:
 *
 *      register implementation.
 *
 * ====================================================================
 * ====================================================================
 */

#define INCLUDING_IN_REGISTER

#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "config.h"
#include "glob.h"
#include "util.h"
#include "calls.h"
#include "data_layout.h"
#include "tn.h"
#include "targ_sim.h"
#include "op.h"

#include "register.h"
#include "cgtarget.h"

#include <vector>
#include <utility>

using std::vector;
using std::pair;

/* ====================================================================
 * Define a few things that differ among code generators
 * ====================================================================
 */

/* ====================================================================
 * Is the frame-pointer register required for the current function
 * ====================================================================
 */

#define FRAME_POINTER_REQUIRED_FOR_PU	\
  	(Current_PU_Stack_Model != SMODEL_SMALL || PUSH_FRAME_POINTER_ON_STACK)

/* ====================================================================
 * Shared data structures
 * ====================================================================
 */

/* Exported data:
 */
REGISTER_SUBCLASS_INFO* REGISTER_SUBCLASS_info=NULL;
ISA_REGCLASS  REGISTER_CLASS_vec[TI_ISA_REGCLASS_MAX + 1];
REGISTER_CLASS_INFO REGISTER_CLASS_info[TI_ISA_REGCLASS_MAX + 1];
CLASS_REG_PAIR      CLASS_REG_PAIR_zero;
CLASS_REG_PAIR      CLASS_REG_PAIR_ep;
CLASS_REG_PAIR      CLASS_REG_PAIR_gp;
CLASS_REG_PAIR      CLASS_REG_PAIR_sp;
CLASS_REG_PAIR      CLASS_REG_PAIR_fp;
CLASS_REG_PAIR      CLASS_REG_PAIR_ra;
CLASS_REG_PAIR      CLASS_REG_PAIR_v0;
CLASS_REG_PAIR      CLASS_REG_PAIR_static_link;
CLASS_REG_PAIR      CLASS_REG_PAIR_pfs;
CLASS_REG_PAIR      CLASS_REG_PAIR_lc;
CLASS_REG_PAIR      CLASS_REG_PAIR_sar;
CLASS_REG_PAIR      CLASS_REG_PAIR_br;
CLASS_REG_PAIR      CLASS_REG_PAIR_lbeg;
CLASS_REG_PAIR      CLASS_REG_PAIR_lend;
CLASS_REG_PAIR      CLASS_REG_PAIR_lcount;
CLASS_REG_PAIR      CLASS_REG_PAIR_ec;
CLASS_REG_PAIR      CLASS_REG_PAIR_true;
CLASS_REG_PAIR      CLASS_REG_PAIR_fzero;
CLASS_REG_PAIR      CLASS_REG_PAIR_fone;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc_lo;
CLASS_REG_PAIR      CLASS_REG_PAIR_acc_hi;

const CLASS_REG_PAIR CLASS_REG_PAIR_undef =
  {CREATE_CLASS_N_REG(ISA_REGCLASS_UNDEFINED,REGISTER_UNDEFINED)};

#if TI_ISA_REGISTER_MAX >= 64
const REGISTER_SET REGISTER_SET_EMPTY_SET = { 0 };
#endif /* TI_ISA_REGISTER_MAX >= 64 */

/* Track the "allocatable" state of each register.
 */
enum {
  AS_default = 0,	/* the default is what targ_info says */
  AS_allocatable = 1,
  AS_not_allocatable = 2
};

static mUINT8 reg_alloc_status[TI_ISA_REGCLASS_MAX + 1][REGISTER_MAX + 1];

static int _max_dedicated_boolean_register_width[16]={
 /* b0 */  16,
 /* b1 */   1,
 /* b2 */   2,
 /* b3 */   1,
 /* b4 */   4,
 /* b5 */   1,
 /* b6 */   2,
 /* b7 */   1,
 /* b8 */   8,
 /* b9 */   1,
 /* b0 */   2,
 /* b11 */  1,
 /* b12 */  4,
 /* b13 */  1,
 /* b14 */  2,
 /* b15 */  1
};

/* ======================================================================
 * Max_Dedicated_Boolean_Register_Width
 * return the max register width for dedicated boolean register
 * ======================================================================*/

INT Max_Dedicated_Boolean_Register_Width(REGISTER reg) {
  FmtAssert(reg>=1 && reg<=16,("Bad boolean register index"));
  return _max_dedicated_boolean_register_width[reg-1];
}

/* ======================================================================
 * Mtype_Register_Width
 * return the number of registers needed in the register file for a mtype
 * ======================================================================*/
INT Mtype_Register_Width(TYPE_ID mtype) {

  // need to modify this switch once TIE allows ctypes that requires
  // more than one registers (such as register pair)
  switch (mtype) {
    case MTYPE_XTBOOL2: return 2;
    case MTYPE_XTBOOL4: return 4;
    case MTYPE_XTBOOL8: return 8;
    case MTYPE_XTBOOL16: return 16;
    default: return 1;
  }
}

// list of registers that should not be allocated, both globally and locally.
static vector< pair< ISA_REGCLASS, REGISTER> > dont_allocate_these_registers;
static vector< pair< ISA_REGCLASS, REGISTER> > dont_allocate_these_registers_in_pu;


/* ====================================================================
 *
 *  Set_CLASS_REG_PAIR
 *
 *  Set the rclass and register.x
 *
 * ====================================================================
 */
void Set_CLASS_REG_PAIR(CLASS_REG_PAIR& rp, ISA_REGCLASS rclass, REGISTER reg)
{
  rp.class_reg.rclass = rclass;
  rp.class_reg.reg = reg;
}


/* ====================================================================
 *
 *  REGISTER_SET_Range
 *
 *  Return the a set of the registers: low .. high
 *
 * ====================================================================
 */
REGISTER_SET
REGISTER_SET_Range(UINT low, UINT high)
{
#if TI_ISA_REGISTER_MAX < 64
  Is_True(low >= REGISTER_MIN && low <= high && high <= TI_ISA_Register_Max(),
	  ("REGISTER_SET_Range: bad range specification"));

  UINT leading_zeros = (sizeof(REGISTER_SET_WORD) * 8) - high;
  UINT trailing_zeros = low - REGISTER_MIN;
  return   ((REGISTER_SET_WORD)-1 << (leading_zeros + trailing_zeros)) 
	>> leading_zeros;
#else /* TI_ISA_REGISTER_MAX < 64 */
  INT i;
  REGISTER_SET set;
  for (i = 0; i <= MAX_REGISTER_SET_IDX; ++i) {
    UINT this_low = (i * 64) + REGISTER_MIN;
    UINT this_high = this_low + 63;
    if (low > this_high || high < this_low) {
      set.v[i] = 0;
    } else {
      UINT leading_zeros = high > this_high ? 0 : this_high - high;
      UINT trailing_zeros = low < this_low ? 0 : low - this_low;
      set.v[i] =   ((REGISTER_SET_WORD)-1 << (leading_zeros + trailing_zeros)) 
		>> leading_zeros;
    }
  }
  return set;
#endif /* TI_ISA_REGISTER_MAX < 64 */
}

/* ====================================================================
 * ====================================================================
 *
 * Initialization and termination
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  Initialize_Register_Class
 *
 *  Initialize the register class 'rclass'. A register class may be
 *  intialized multiple times.
 *
 * ====================================================================
 */
static void
Initialize_Register_Class(
  ISA_REGCLASS rclass
)
{
  INT32              i;
  const ISA_REGCLASS_INFO *icinfo = TI_ISA_Regclass_Info(rclass);
  const char        *rcname         = TI_ISA_Regclass_Name(icinfo);
  INT		     bit_size       = TI_ISA_Regclass_Bit_Size(icinfo);
  INT                first_isa_reg  = TI_ISA_Regclass_First_Reg(icinfo);
  INT                last_isa_reg   = TI_ISA_Regclass_Last_Reg(icinfo);
  INT                register_count = last_isa_reg - first_isa_reg + 1;
  REGISTER_SET       allocatable    = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       caller         = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       callee         = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       shrink_wrap    = REGISTER_SET_EMPTY_SET;
  REGISTER_SET	     stacked        = REGISTER_SET_EMPTY_SET;
  REGISTER_SET	     rotating       = REGISTER_SET_EMPTY_SET;
  REGISTER_SET       out_return_val = REGISTER_SET_EMPTY_SET; // callee
  REGISTER_SET       in_return_val  = REGISTER_SET_EMPTY_SET; // caller
  REGISTER_SET       out_argument   = REGISTER_SET_EMPTY_SET; // caller
  REGISTER_SET       in_argument    = REGISTER_SET_EMPTY_SET; // callee

  // only ar registers are windowed, and we assume call8
  INT window_size = (Target_ABI == ABI_WINDOWED && 
                     rclass == TI_ISA_Regclass_Integer() ? 8 : 0); 

  /* Verify we have a valid rclass and that the type used to implement 
   * a register set is large enough.
   */
  FmtAssert(rclass >= TI_ISA_Regclass_First() && rclass <= TI_ISA_Regclass_Last(),
	    ("invalide register class %d", (INT)rclass));
  FmtAssert((sizeof(REGISTER_SET) * 8) >= register_count,
	    ("REGISTER_SET type cannot represent all registers in "
	     "the class %s", rcname));

  REGISTER_CLASS_name(rclass) = rcname;

  /* Now make sets of various register properties:
   */
  for ( i = 0; i < register_count; ++i ) {
    INT      isa_reg        = i + first_isa_reg;
    REGISTER reg            = i + REGISTER_MIN;
    BOOL     is_allocatable = TI_ABI_Property_Set(ABI_PROPERTY_allocatable, rclass, isa_reg);
    INT      alloc_status   = reg_alloc_status[rclass][reg];

    /* CG likes to pretend that a class with only one register can't
     * be allocated, so perpetuate that illusion.
     */
    if ( register_count <= 1 ) is_allocatable = FALSE;

    switch ( alloc_status ) {
      case AS_allocatable:
	is_allocatable = TRUE;
	break;
      case AS_not_allocatable:
	is_allocatable = FALSE;
	break;
      case AS_default:
	break;
      default:
	Is_True(FALSE, ("unhandled allocations status: %d", alloc_status));
    }

    if ( is_allocatable ) {
      allocatable = REGISTER_SET_Union1(allocatable,reg);

#ifdef TARG_XTENSA
      if (FALSE) {
#else
      if ( TI_ABI_Property_Set(ABI_PROPERTY_global_ptr, rclass, isa_reg) ) {
#endif
        if ( GP_Is_Preserved ) {
          /* neither caller nor callee saved (always preserved). */
        } else if ( Is_Caller_Save_GP ) {
          /* caller-saved. */
          caller = REGISTER_SET_Union1(caller, reg);
        } else {
          /* callee-saved. */
          callee = REGISTER_SET_Union1(callee, reg);
        }
      }
      else {
        if ( TI_ABI_Property_Set(ABI_PROPERTY_callee, rclass, isa_reg) ) {
          callee = REGISTER_SET_Union1(callee, reg);
          shrink_wrap = REGISTER_SET_Union1(shrink_wrap, reg);
        }
        if ( TI_ABI_Property_Set(ABI_PROPERTY_caller, rclass, isa_reg) )
          caller = REGISTER_SET_Union1(caller, reg);
        if ( TI_ABI_Property_Set(ABI_PROPERTY_func_arg, rclass, isa_reg) ) {
          in_argument = REGISTER_SET_Union1(in_argument, reg);
          out_argument = REGISTER_SET_Union1(out_argument, reg+window_size);
        }
        if ( TI_ABI_Property_Set(ABI_PROPERTY_func_val, rclass, isa_reg) ) {
          out_return_val = REGISTER_SET_Union1(out_return_val, reg);
          in_return_val = REGISTER_SET_Union1(in_return_val, reg+window_size);
        }
        if ( Target_ABI == ABI_CALL0 &&
	     TI_ABI_Property_Set(ABI_PROPERTY_ret_addr, rclass, isa_reg) )
          shrink_wrap = REGISTER_SET_Union1(shrink_wrap, reg);
#ifndef TARG_XTENSA
        if ( TI_ABI_Property_Set(ABI_PROPERTY_stacked, rclass, isa_reg) )
          stacked = REGISTER_SET_Union1(stacked, reg);
#endif
      }
    }

    REGISTER_bit_size(rclass, reg) = bit_size;
    REGISTER_machine_id(rclass, reg) = isa_reg;
    REGISTER_allocatable(rclass, reg) = is_allocatable;
    REGISTER_name(rclass, reg) = TI_ISA_Regclass_Reg_Name(icinfo, isa_reg);

    if ( TI_ABI_Property_Set(ABI_PROPERTY_frame_ptr, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fp, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_stack_ptr, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_sp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_sp, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_static_link, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_static_link, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_static_link, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_shift_amount, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_sar, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_sar, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_br_regfile, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_br, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_br, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_zcl_loop_begin, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lbeg, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lbeg, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_zcl_loop_end, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lend, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lend, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_zcl_loop_count, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lcount, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lcount, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_acc_lo, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc_lo, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc_lo, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_acc_hi, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_acc_hi, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_acc_hi, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_ret_addr, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ra, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ra, rclass);
    }
#ifndef TARG_XTENSA
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_global_ptr, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_gp, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_gp, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_entry_ptr, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ep, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ep, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_zero, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_zero, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_zero, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_prev_funcstate, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_pfs, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_pfs, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_loop_count, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_lc, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_lc, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_epilog_count, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_ec, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_ec, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_true_predicate, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_true, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_true, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_fzero, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fzero, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fzero, rclass);
    }
    else if ( TI_ABI_Property_Set(ABI_PROPERTY_fone, rclass, isa_reg) ) {
      Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_fone, reg);
      Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_fone, rclass);
    }
#endif
  }

  REGISTER_CLASS_universe(rclass)          =
	REGISTER_SET_Range(REGISTER_MIN, REGISTER_MIN + register_count - 1);
  REGISTER_CLASS_allocatable(rclass)       = allocatable;
  REGISTER_CLASS_callee_saves(rclass)      = callee;
  REGISTER_CLASS_caller_saves(rclass)      = caller;
  REGISTER_CLASS_shrink_wrap(rclass)       = shrink_wrap;
  REGISTER_CLASS_register_count(rclass)    = register_count;
  REGISTER_CLASS_stacked(rclass)           = stacked;
  REGISTER_CLASS_rotating(rclass)          = rotating;
  REGISTER_CLASS_outgoing_return_value(rclass) = out_return_val;
  REGISTER_CLASS_incoming_return_value(rclass) = in_return_val;
  REGISTER_CLASS_outgoing_argument(rclass) = out_argument;
  REGISTER_CLASS_incoming_argument(rclass) = in_argument;
  REGISTER_CLASS_can_store(rclass)
	= TI_ISA_Regclass_Can_Store(icinfo);
  REGISTER_CLASS_multiple_save(rclass)
	= TI_ISA_Regclass_Multiple_Save(icinfo);

  /* There are multiple integer return regs -- v0 is the lowest
   * of the set.
   */
  if ( rclass == TI_ISA_Regclass_Integer() ) {
    Set_CLASS_REG_PAIR_reg(CLASS_REG_PAIR_v0, 
                           REGISTER_SET_Choose(out_return_val));
    Set_CLASS_REG_PAIR_rclass(CLASS_REG_PAIR_v0, rclass);
  }

}


/* ====================================================================
 *
 *  Initialize_Register_Subclasses
 *
 *  Initialize the register subclass information cache.
 *
 * ====================================================================
 */
static void
Initialize_Register_Subclasses(void)
{
  ISA_REGSUBCLASS sc;

  REGISTER_SUBCLASS_info =
	(REGISTER_SUBCLASS_INFO*)malloc(
		sizeof(REGISTER_SUBCLASS_INFO)*(TI_ISA_Regsubclass_Last()+1));

  for (sc = (ISA_REGSUBCLASS)0; // standard iterator skips _UNDEFINED (0)
       sc <= TI_ISA_Regsubclass_Last();
       sc = (ISA_REGSUBCLASS)(sc + 1))
  {
    INT i;
    ISA_REGCLASS rc;
    REGISTER_SET members = REGISTER_SET_EMPTY_SET;
    const ISA_REGSUBCLASS_INFO *scinfo = TI_ISA_Regsubclass_Info(sc);
    INT count = TI_ISA_Regsubclass_Count(scinfo);

    for (i = 0; i < count; ++i) {
      INT isa_reg = TI_ISA_Regsubclass_Member(scinfo, i);
      const char *reg_name = TI_ISA_Regsubclass_Reg_Name(scinfo, i);
      REGISTER reg = (REGISTER)(isa_reg + REGISTER_MIN);
      members = REGISTER_SET_Union1(members, reg);
      REGISTER_SUBCLASS_reg_name(sc, reg) = reg_name;
    }
    rc = TI_ISA_Regsubclass_Class(scinfo);
    members = REGISTER_SET_Intersection(members, REGISTER_CLASS_universe(rc));

    REGISTER_SUBCLASS_members(sc) = members;
    REGISTER_SUBCLASS_name(sc) = TI_ISA_Regsubclass_Name(scinfo);
    REGISTER_SUBCLASS_register_class(sc) = rc;
  }
}


/* ====================================================================
 *
 *  REGISTER_Begin
 *
 *  See interface description
 *
 * ====================================================================
 */
void
REGISTER_Begin(void)
{
  ISA_REGCLASS rclass;

  /*  Create the register classes for all the target registers.
   */
  FOR_ALL_ISA_REGCLASS( rclass ) {
	Initialize_Register_Class(rclass);
#ifdef HAS_STACKED_REGISTERS
    	REGISTER_Init_Stacked(rclass);
#endif
  }
  Initialize_Register_Subclasses();
  Init_Mtype_RegClass_Map();
}

struct Dont_Allocate_Dreg
{
  inline void operator() (UINT32, ST_ATTR *st_attr) const {
        if (ST_ATTR_kind (*st_attr) != ST_ATTR_DEDICATED_REGISTER)
	    return;
	PREG_NUM preg = ST_ATTR_reg_id(*st_attr);
	ST* st = ST_ptr(ST_ATTR_st_idx(*st_attr));
	TYPE_ID mtype = ST_mtype(st);
	ISA_REGCLASS rclass;
	REGISTER reg;
	CGTARG_Preg_Register_And_Class(preg, &rclass, &reg);
	int width = Mtype_Register_Width(mtype);
	for (int i=0; i<width; i++)
	  REGISTER_Set_Allocatable (rclass, reg+i, FALSE /* is_allocatable */);
  }
};
 
/* ====================================================================
 *
 *  REGISTER_Pu_Begin
 *
 *  See interface description
 *
 * ====================================================================
 */
extern void
REGISTER_Pu_Begin(void)
{
  ISA_REGCLASS rclass;

  /* Scan all the registers to find if the initial allocation status
   * will be different from the current state. The initial status
   * is all registers are set to their "default".
   */
  FOR_ALL_ISA_REGCLASS( rclass ) {
    REGISTER reg;
    BOOL re_init = FALSE;

    for ( reg = REGISTER_MIN;
	  reg <= REGISTER_CLASS_last_register(rclass);
	  reg++
    ) {
      if ( reg_alloc_status[rclass][reg] != AS_default) {
	reg_alloc_status[rclass][reg] = AS_default;
	re_init = TRUE;
      }
    }

    if ( re_init ) Initialize_Register_Class(rclass);

    // always reset rotating register set
    REGISTER_CLASS_rotating(rclass) = REGISTER_SET_EMPTY_SET;

#ifdef HAS_STACKED_REGISTERS
    REGISTER_Init_Stacked(rclass);
#endif
  }

  // now check for any registers that user doesn't want allocated
  vector< pair< ISA_REGCLASS, REGISTER > >::iterator r;
  for (r = dont_allocate_these_registers.begin(); 
	r != dont_allocate_these_registers.end(); 
	++r)
  {
	REGISTER_Set_Allocatable ((*r).first, (*r).second, FALSE /* is_allocatable */);
  }
  // also check for user register variables in PU (local dreg list).
  if ( ST_ATTR_Table_Size (CURRENT_SYMTAB)) {
    For_all (St_Attr_Table, CURRENT_SYMTAB, 
	Dont_Allocate_Dreg());
  }

  if ( Get_Trace(TP_MISC, 0x100) ) REGISTER_CLASS_Trace_All();
}

// possibly reset fp to non-allocatable if need a frame pointer
extern void
REGISTER_Reset_FP (void)
{
  ISA_REGCLASS rclass;
  if (FRAME_POINTER_REQUIRED_FOR_PU && FP_TN != NULL) {
	rclass = TN_register_class(FP_TN);
	reg_alloc_status[rclass][TN_register(FP_TN)] = AS_not_allocatable;
	Initialize_Register_Class(rclass);
  }
}


/* ====================================================================
 * ====================================================================
 *
 * REGISTER_SET functions
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  REGISTER_SET_Difference_Range
 *
 *  See interface description
 *
 * ====================================================================
 */
REGISTER_SET
REGISTER_SET_Difference_Range(
  REGISTER_SET   set,
  REGISTER       low,
  REGISTER       high
)
{
  return REGISTER_SET_Difference(set, REGISTER_SET_Range(low, high));
}


/* ====================================================================
 *
 *  REGISTER_SET_CHOOSE_ENGINE
 *
 *  The guts of the REGISTER_SET_Choose... functions. Return the index
 *  (1-based) of the first set bit in 'set'.
 *
 * ====================================================================
 */
inline REGISTER REGISTER_SET_Choose_Engine(
  const REGISTER_SET & set
)
{
  INT i = 0;
  do {
    REGISTER_SET_WORD w = REGISTER_SET_ELEM(set, i);
    INT regbase = REGISTER_MIN + (i * sizeof(REGISTER_SET_WORD) * 8);
    do {
      REGISTER_SET_WORD lowb = w & 0xff;
      if ( lowb ) return regbase + UINT8_least_sig_one[lowb];
    } while (regbase += 8, w >>= 8);
  } while (++i <= MAX_REGISTER_SET_IDX);
  return REGISTER_UNDEFINED;
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose(
  const REGISTER_SET & set
)
{
  return REGISTER_SET_Choose_Engine(set);
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Range
 *
 *  See interface description
 *
 * ====================================================================
 */

extern REGISTER
REGISTER_SET_Choose_Range(
  const REGISTER_SET & set,
  REGISTER     low,
  REGISTER     high
)
{
  if (low > TI_ISA_Register_Max()) {
    return REGISTER_UNDEFINED;
  } else {
    REGISTER_SET temp;
    temp = REGISTER_SET_Intersection(set, REGISTER_SET_Range(low, high));
    return REGISTER_SET_Choose_Engine(temp);
  }
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Next
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose_Next(
  const REGISTER_SET & set,
  REGISTER     reg
)
{
  if ( reg >= TI_ISA_Register_Max() ) {
    return REGISTER_UNDEFINED;
  } else {
    REGISTER_SET temp;
    temp = REGISTER_SET_Difference(set, REGISTER_SET_Range(REGISTER_MIN, reg));
    return REGISTER_SET_Choose_Engine(temp);
  }
}


/* ====================================================================
 *
 *  REGISTER_SET_Choose_Intersection
 *
 *  See interface description
 *
 * ====================================================================
 */
extern REGISTER
REGISTER_SET_Choose_Intersection(
  const REGISTER_SET & set1,
  const REGISTER_SET & set2
)
{
  REGISTER_SET set = REGISTER_SET_Intersection(set1, set2);
  return REGISTER_SET_Choose(set);
}


/* ====================================================================
 *
 *  REGISTER_SET_Size
 *
 *  See interface description
 *
 * ====================================================================
 */
extern INT32
REGISTER_SET_Size(
  const REGISTER_SET & set
)
{
  INT32 size = 0;
  INT i = 0;
  do {
    REGISTER_SET_WORD w = REGISTER_SET_ELEM(set, i);
    do {
      size += UINT8_pop_count[w & 0xff];
    } while (w >>= 8);
  } while (++i <= MAX_REGISTER_SET_IDX);
  return size;
}


/* ====================================================================
 *
 *  REGISTER_SET_Print
 *
 *  Prints out a register set
 *
 * ====================================================================
 */
extern void
REGISTER_SET_Print(
  REGISTER_SET regset,
  FILE *f
)
{
  REGISTER    i;
  const char *sep = "";

  fprintf(f, "[");
  for ( i = REGISTER_SET_Choose(regset);
        i != REGISTER_UNDEFINED;
        i = REGISTER_SET_Choose_Next(regset,i)
  ) {
    fprintf(f, "%s%d", sep, i);
    sep = ",";
  }

  fprintf(f, "]");
}

/* ====================================================================
 * ====================================================================
 *
 * REGISTER_CLASS functions
 *
 * ====================================================================
 * ====================================================================
 */

/* ====================================================================
 *
 *  REGISTER_CLASS_OP_Update_Mapping
 *
 *  See interface description
 *
 * ====================================================================
 */
extern void
REGISTER_CLASS_OP_Update_Mapping(
    OP *op
)
{
  INT32 i;
  const ISA_OPERAND_INFO *oinfo = TI_ISA_Operand_Info(OP_code(op));

  for (i = OP_results(op) - 1; i >= 0; --i) {
    TN *tn = OP_result(op,i);

    if (    TN_is_register(tn)
         && TN_register_class(tn) == ISA_REGCLASS_UNDEFINED
    ) {
      const ISA_OPERAND_VALTYP *otype = TI_ISA_Op_Result(oinfo, i);
      ISA_REGCLASS rclass = TI_ISA_Valtyp_Regclass(otype);
      Set_TN_register_class(tn, rclass);
    }
  }

  for ( i = OP_opnds(op) - 1; i >= 0; --i ) {
    TN *tn = OP_opnd(op,i);

    if (    TN_is_register(tn)
         && TN_register_class(tn) == ISA_REGCLASS_UNDEFINED
    ) {
      const ISA_OPERAND_VALTYP *otype = TI_ISA_Op_Operand(oinfo, i);
      ISA_REGCLASS rclass = TI_ISA_Valtyp_Regclass(otype);
      Set_TN_register_class(tn, rclass);
    }
  }
}

/* ====================================================================
 * ====================================================================
 *
 * REGISTER functions
 *
 * ====================================================================
 * ====================================================================
 */


/* ====================================================================
 *
 *  REGISTER_Print
 *
 *  Prints out a register to a specified file.
 *
 * ====================================================================
 */
extern void
REGISTER_Print(
  ISA_REGCLASS rclass,
  REGISTER reg,
  FILE *f
)
{
  fprintf(f, REGISTER_name(rclass, reg));
}

/* ====================================================================
 *
 *  CLASS_REG_PAIR_Print
 *
 *  Prints out a register to a specified file.
 *
 * ====================================================================
 */
extern void
CLASS_REG_PAIR_Print(
  CLASS_REG_PAIR crp,
  FILE *f
)
{
  REGISTER_Print(CLASS_REG_PAIR_rclass(crp), CLASS_REG_PAIR_reg(crp),f);
}


/* ====================================================================
 *
 *  REGISTER_Set_Allocatable
 *
 *  See interface description
 *
 * ====================================================================
 */
void
REGISTER_Set_Allocatable(
  ISA_REGCLASS rclass,
  REGISTER           reg,
  BOOL               is_allocatable
)
{
  INT prev_status = reg_alloc_status[rclass][reg];
  INT new_status  = is_allocatable ? AS_allocatable : AS_not_allocatable;

  if ( prev_status != new_status ) {
    reg_alloc_status[rclass][reg] = new_status;
    Initialize_Register_Class(rclass);
  }
}

/* ====================================================================
 * ====================================================================
 *
 * Tracing
 *
 * ====================================================================
 * ====================================================================
 */


#define TRUE_FALSE(b) ((b) ? "true" : "false")


/* =======================================================================
 *
 *  REGISTER_SET_Print_Name
 *
 *  
 *
 * =======================================================================
 */
static void
REGISTER_SET_Print_Name(
  ISA_REGCLASS rclass,
  REGISTER_SET regset,
  FILE *f
)
{
  REGISTER i;
  char    *sep = "";

  fprintf(f, "[");
  for ( i = REGISTER_SET_Choose(regset);
        i != REGISTER_UNDEFINED;
        i = REGISTER_SET_Choose_Next(regset,i)
  ) {
    fprintf(f, "%s%s",sep,REGISTER_name(rclass,i));
    sep = ",";
  }

  fprintf(f, "]");
}


/* =======================================================================
 *
 *  REGISTER_Trace
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void REGISTER_Trace(
  ISA_REGCLASS rclass,
  REGISTER reg
)
{
  if (    reg < REGISTER_MIN 
       || reg > REGISTER_CLASS_last_register(rclass) ) return;

  fprintf(TFile, "  reg %2d:"
		 " name=%-5s"
		 " bit-size=%-3d"
		 " mach-id=%-2d"
		 " allocatable=%-5s\n",
		 reg,
		 REGISTER_name(rclass, reg),
		 REGISTER_bit_size(rclass, reg),
		 REGISTER_machine_id(rclass, reg),
		 TRUE_FALSE(REGISTER_allocatable(rclass, reg)));
}


/* =======================================================================
 *
 *  REGISTER_CLASS_Trace
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void REGISTER_CLASS_Trace(
  ISA_REGCLASS rclass
)
{
  REGISTER reg;
  REGISTER_SET set;

  fprintf(TFile, "register class %d (%s) register-count=%d can-store=%s\n",
		 rclass, REGISTER_CLASS_name(rclass),
		 REGISTER_CLASS_register_count(rclass),
		 TRUE_FALSE(REGISTER_CLASS_can_store(rclass)));

  for ( reg = REGISTER_MIN; reg <= TI_ISA_Register_Max(); reg++ ) {
    REGISTER_Trace(rclass, reg);
  }

  fprintf(TFile, "\n  universe: ");
  REGISTER_SET_Print(REGISTER_CLASS_universe(rclass), TFile);
  fprintf(TFile, "\n");

  set = REGISTER_CLASS_allocatable(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  allocatable: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_callee_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  callee_saves: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_caller_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  caller_saves: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_incoming_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument (callee): ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_outgoing_return_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value (callee): ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_outgoing_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument (caller): ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_incoming_return_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value (caller): ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_shrink_wrap(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  shrink_wrap: ");
    REGISTER_SET_Print(set, TFile);
    fprintf(TFile, "\n");
  }

  fprintf(TFile, "\n  universe: ");
  REGISTER_SET_Print_Name(rclass, REGISTER_CLASS_universe(rclass), TFile);
  fprintf(TFile, "\n");

  set = REGISTER_CLASS_allocatable(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  allocatable: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_callee_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  callee_saves: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_caller_saves(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  caller_saves: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_incoming_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument (callee): ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_outgoing_return_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value (callee): ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_outgoing_argument(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_argument (caller): ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_incoming_return_value(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  function_value (caller): ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }

  set = REGISTER_CLASS_shrink_wrap(rclass);
  if ( !REGISTER_SET_EmptyP(set) ) {
    fprintf(TFile, "  shrink_wrap: ");
    REGISTER_SET_Print_Name(rclass, set, TFile);
    fprintf(TFile, "\n");
  }
}


/* =======================================================================
 *
 *  REGISTER_CLASS_Trace_All
 *
 *  See interface description
 *
 * =======================================================================
 */
extern void
REGISTER_CLASS_Trace_All(void)
{
  ISA_REGCLASS rclass;

  fprintf(TFile, "\n%s"
                 " REGISTERs and ISA_REGCLASSes for PU \"%s\"\n"
                 "%s",
                 DBar, Cur_PU_Name, DBar);

  FOR_ALL_ISA_REGCLASS( rclass ) {
    fprintf(TFile, "\n");
    REGISTER_CLASS_Trace(rclass);
  }
}

// user wants given register to not be allocatable in file.
void
Set_Register_Never_Allocatable (char *regname) 
{
	ISA_REGCLASS rclass;
	REGISTER reg;
	switch (regname[0]) {
	case 'r':
		rclass = TI_ISA_Regclass_Integer();
		break;
	case 'f':
		rclass = TI_ISA_Regclass_Float();
		break;
	default:
		FmtAssert(FALSE, ("unexpected reg letter %c", regname[0]));
	}
	reg = REGISTER_MIN + atoi(regname+1);
	FmtAssert(reg <= REGISTER_CLASS_last_register(rclass),
		("%s is not a valid register", regname));
	dont_allocate_these_registers.push_back(std::make_pair( rclass, reg ));
}

// user wants given register to not be allocatable in file.
void
Set_Register_Never_Allocatable (PREG_NUM preg) 
{
	ISA_REGCLASS rclass;
	REGISTER reg;
	CGTARG_Preg_Register_And_Class(preg, &rclass, &reg);
	dont_allocate_these_registers.push_back(std::make_pair( rclass, reg ));
}

