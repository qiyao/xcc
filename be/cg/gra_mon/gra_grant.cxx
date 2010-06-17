
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

//  GRA -> LRA grant implementation
/////////////////////////////////////
//  
//  Description:
//
//      Not much more than a map from BBs to vectors of REGISTER_SETs.
//
/////////////////////////////////////


//  $Revision: 1.15 $
//  $Date: 2000/07/27 21:10:21 $
//  $Author: dew $
//  $Source: /isms/cmplrs.src/osprey1.0/be/cg/gra_mon/RCS/gra_grant.cxx,v $

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /isms/cmplrs.src/osprey1.0/be/cg/gra_mon/RCS/gra_grant.cxx,v $ $Revision: 1.15 $";
#endif

#include "tracing.h"
#include "defs.h"
#include "mempool.h"
#include "bb.h"
#include "bb_map.h"
#include "gra_grant.h"
#include "register.h"
#include "gra_bb.h"
#include "gra_trace.h"
#include "lra.h"

MEM_POOL grant_pool;    // Just for grants
static BOOL grant_pool_initialized = FALSE;
BB_MAP grant_map;

#ifndef Is_True_On
#define GRANT_INLINE inline
#else
#define GRANT_INLINE static
#endif

/////////////////////////////////////
typedef REGISTER_SET GRANT;
/////////////////////////////////////
//
//  Not a very impressive type really.
//
/////////////////////////////////////

/////////////////////////////////////
GRANT_INLINE void
GRANT_REGISTER_SET_Set( GRANT* grant, ISA_REGCLASS rc,  REGISTER_SET  set )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  grant[rc-TI_ISA_Regclass_First()] = set;
}

/////////////////////////////////////
GRANT_INLINE REGISTER_SET
GRANT_REGISTER_SET_Get( GRANT* grant, ISA_REGCLASS rc )
/////////////////////////////////////
//
//  Get the REGISTER_SET for <rc> from <grant>.
//
/////////////////////////////////////
{
  return grant[rc-TI_ISA_Regclass_First()];
}

/////////////////////////////////////
GRANT_INLINE void
GRANT_Union1D( GRANT* grant, ISA_REGCLASS rc, REGISTER reg )
/////////////////////////////////////
//
//  Add a register to <grant>...
//
/////////////////////////////////////
{
  GRANT_REGISTER_SET_Set(
    grant,
    rc,
    REGISTER_SET_Union1(GRANT_REGISTER_SET_Get(grant,rc),reg)
  );
}

/////////////////////////////////////
GRANT_INLINE void
GRANT_UnionD( GRANT* grant, ISA_REGCLASS rc, REGISTER_SET register_set )
/////////////////////////////////////
//
//  Add the registers in <register_set> to <grant>...
//
/////////////////////////////////////
{
  GRANT_REGISTER_SET_Set(
    grant,
    rc,
    REGISTER_SET_Union(GRANT_REGISTER_SET_Get(grant,rc),register_set)
  );
}

/////////////////////////////////////
void
GRA_GRANT_Initialize(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  BB *bb;
  GRANT* grant_mem;

  if ( ! grant_pool_initialized ) {
    grant_pool_initialized = TRUE;
    MEM_POOL_Initialize(&grant_pool,"GRA register to LRA",FALSE);
    MEM_POOL_Push(&grant_pool);
  }

  grant_mem =
    TYPE_MEM_POOL_ALLOC_N(REGISTER_SET,&grant_pool,
			  TI_ISA_Num_Regclasses()*PU_BB_Count + 2);
  memset(grant_mem, 0,sizeof(GRANT)*TI_ISA_Num_Regclasses()*PU_BB_Count + 2);
  grant_map = BB_MAP_Create();

  for ( bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    BB_MAP_Set(grant_map,bb,(void*) grant_mem);
    grant_mem += TI_ISA_Num_Regclasses();
  }
}

/////////////////////////////////////
void
GRA_GRANT_Transfer( BB* from_bb, BB* to_bb )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGCLASS rc;
  GRANT* from_grant = (GRANT*)BB_MAP_Get(grant_map,from_bb);
  GRANT* to_grant   = (GRANT*)BB_MAP_Get(grant_map,to_bb);

  GRA_BB* from_gbb = gbb_mgr.Get(from_bb);
  GRA_BB* to_gbb = gbb_mgr.Get(to_bb);

  FOR_ALL_ISA_REGCLASS( rc ) {
    REGISTER_SET common = REGISTER_SET_Intersection(
		    GRANT_REGISTER_SET_Get(from_grant,rc),
		    GRANT_REGISTER_SET_Get(to_grant,rc));
    if (from_gbb && to_gbb) {
      GRANT_REGISTER_SET_Set(to_grant, rc, 
		REGISTER_SET_Difference(GRANT_REGISTER_SET_Get(to_grant,rc),
					from_gbb->Registers_Used(rc)));
      GRANT_REGISTER_SET_Set(from_grant, rc, 
		REGISTER_SET_Difference(GRANT_REGISTER_SET_Get(from_grant,rc),
					to_gbb->Registers_Used(rc)));
    }
    GRANT_UnionD(to_grant,rc,common);
    GRANT_UnionD(from_grant,rc,common);
  }

}

// used by swp to assign a register to loop var TN
bool GRA_GRANT_Local_Register_Okay (BB *bb, ISA_REGCLASS rc, REGISTER reg )
{
  /* Reconsider the decision to let LRA use a particular register.
 
     Some registers may be needed to carry implicit values between
     blocks.  We must make sure that LRA does not destroy registers
     that may contain valid information, and we will do this by
     keeping those registers out of the grant that is given to LRA.
 
     The only implicitly defined registers that we are concerned about
     are the ones that contain the results of a function call.  Since
     calls terminate a block, the result registers are always used in
     successor blocks.  Initially, this would always be the immediate
     successor, but hyperblock optimization may move code between
     blocks and cause the use of the implicitly defined result to be
     several blocks away from the call.
 
     Note: this may not be the cleanest way of preventing the problem.
     In the long run, it may not even be safe if changes are made to
     how Build_Dedicated_TN works. But it works for now!  */

  if (! REGISTER_SET_MemberP(REGISTER_CLASS_incoming_return_value(rc), reg) ||
      ! GTN_SET_MemberP(BB_live_in(bb),Build_Dedicated_TN(rc, reg, 8)) ||
      ! GTN_SET_MemberP(BB_live_out(bb),Build_Dedicated_TN(rc, reg, 8)) ) {
    return true;
  }

  return false;
}

bool
GRA_GRANT_Local_Register( GRA_BB* gbb, ISA_REGCLASS rc, REGISTER reg )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  GRANT* gr = (GRANT*)BB_MAP_Get(grant_map,gbb->Bb());

  if ( GRA_GRANT_Local_Register_Okay(gbb->Bb(), rc, reg) ) {
    GRA_Trace_Grant(gbb,rc,reg);
    GRANT_Union1D(gr,rc,reg);
    return TRUE;
  }
  GRA_Trace_Reject_Grant(gbb,rc, reg);
  return FALSE;
}

REGISTER_SET
GRA_GRANT_Get_Local_Registers( BB* bb, ISA_REGCLASS rc )
/////////////////////////////////////
//  See interface description.
//  Description
/////////////////////////////////////
{
  return GRANT_REGISTER_SET_Get((GRANT*) BB_MAP_Get(grant_map,bb),rc);
}

/////////////////////////////////////
void
GRA_GRANT_Finalize(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if ( grant_pool_initialized ) {
    MEM_POOL_Pop(&grant_pool);
    MEM_POOL_Delete(&grant_pool);
    grant_pool_initialized = FALSE;
  }
  BB_MAP_Delete(grant_map);
}

/////////////////////////////////////
extern mINT8
LRA_examine_last_op_needs (BB *bb, ISA_REGCLASS cl);


void
GRA_GRANT_Unused_Caller_Saved(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGCLASS rc;

  GRA_Trace_Grant_Unused_Caller_Saved();
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_BB* gbb = gbb_mgr.Get(bb);
    FOR_ALL_ISA_REGCLASS( rc ) {
      REGISTER_SET free_regs;
      REGISTER reg;

      free_regs = REGISTER_SET_Difference(REGISTER_CLASS_caller_saves(rc),
					  gbb->Registers_Used(rc));
      FOR_ALL_REGISTER_SET_members(free_regs, reg) {
	GRA_GRANT_Local_Register(gbb, rc, reg);
      }
    }
  }
}


void
GRA_GRANT_Avail_Callee_Saved(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_REGCLASS rc;

  GRA_Trace_Grant_Unused_Caller_Saved();

  FOR_ALL_ISA_REGCLASS( rc ) {
    REGISTER_SET regs_used = REGISTER_SET_EMPTY_SET;
    REGISTER_SET callee_regs_used = REGISTER_SET_EMPTY_SET;

    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
      GRA_BB* gbb = gbb_mgr.Get(bb);
      regs_used = REGISTER_SET_Union(regs_used, gbb->Registers_Used(rc));
    }

    callee_regs_used = REGISTER_SET_Intersection(REGISTER_CLASS_callee_saves(rc), regs_used);

    for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
      GRA_BB* gbb = gbb_mgr.Get(bb);
      REGISTER_SET callee_regs_avail = REGISTER_SET_Difference(callee_regs_used, gbb->Registers_Used(rc));
      REGISTER reg;
      FOR_ALL_REGISTER_SET_members(callee_regs_avail, reg) {
	GRA_GRANT_Local_Register(gbb, rc, reg);
      }
    }
  }
}



#if 0
static void
Check_LRA_Request(void)
{
  for (BB* bb = REGION_First_BB; bb != NULL; bb = BB_next(bb) ) {
    GRA_BB* gbb = gbb_mgr.Get(bb);
    ISA_REGCLASS cl;
    FOR_ALL_ISA_REGCLASS(cl) {
      UINT8 reg_request = LRA_Register_Request(bb, cl);
      UINT8 reg_grant = GRA_Local_Register_Grant(bb, cl);
      if (reg_request == 2 && reg_grant < 2)
	{
	  int i = reg_request - reg_grant;;
	  REGISTER_SET free_regs = REGISTER_SET_Difference(REGISTER_CLASS_callee_saves(rc),
							   gbb->Registers_Used(rc));
	  REGISTER reg = REGISTER_SET_Choose(free_regs);
	  do
	    {
	      GRA_GRANT_Local_Register(gbb, cl, reg);
	      reg = REGISTER_SET_Choose_Next(free_regs, reg);
	      i--;
	    }
	  while (i);
	  for (i = 0; i < reg_request - reg_grant; i++)
	    
	}
    }
#endif
