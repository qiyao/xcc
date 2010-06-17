        
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

#include "defs.h"
#include <alloca.h>
#include "errors.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "tracing.h"
#include "bb.h"
#include "op.h"
#include "tn.h"
#include "whirl2ops.h"
#include "tn_map.h"
#include "tn_list.h"
#include "tracing.h"
#include "cgexp.h"
#include "cgtarget.h"
#include "gtn_universe.h"
#include "gtn_set.h"
#include "gra_live.h"
#include "reg_live.h"
#include "ebo.h"
#include "ebo_info.h"
#include "ebo_special.h"
#include "ebo_util.h"
#include "targ_sim.h"
#include "cg_loop.h"

#define BB_visited          BB_local_flag1	/* same as in ebo.cxx */
#define Set_BB_visited      Set_BB_local_flag1
#define Reset_BB_visited    Reset_BB_local_flag1

static BOOL TN_Dead (TN *tn, BB *bb, OP *op);

enum EBO_CP_LOC { EBO_CP_BEFORE, EBO_CP_AFTER };
enum EBO_PROP_TYPE { EBO_PROP_NONE, EBO_PROP_DIFF, EBO_PROP_SAME };
enum CP_FLAGS { CP_FLAG_NONE = 0,
		CP_FLAG_NO_BACKWARD_ELIM = 0x01,
		CP_FLAG_PLACEMENT_NEEDED = 0x02 };
class TN_PROP;  
static void Propagate_BB_Forward (BB *bb, TN_PROP *tn_props, BOOL trace);
static void Propagate_BB_Backward (BB *bb, TN_PROP *tn_props, BOOL trace);
static void BB_Replace_TN(BB *bb, TN *dst_tn, TN *repl_tn);
static void BB_Replace_Reg(BB *bb, TN *dst_tn, TN *repl_tn);

/* List of BBs that need to be visited for copy propagation. Only the "head"
   block of an extended-BB is in the list. */
static BB_LIST *ebo_bbs;

/* Set of visited BBs. */
static BB_SET *visited_bbs;

/* BB_LIVENESS. Maintains live-in, and live-out information for local
   and global TNs. Locals can become live across BB boundaries due to
   copy propagation. We don't bother to keep defreach-in or
   deafreach-out information because we don't use that information for
   copy-propagation. */
class BB_LIVENESS
{
private:
  struct LIVENESS
  {
    TN_SET *_live_in;
    TN_SET *_live_out;
  };

  /* Tracing on */
  const BOOL _trace;

  /* MEM_POOL for allocation. */
  MEM_POOL *const _pool;
  
  /* Array mapping from BB id to corresponding liveness information. */
  UINT _liveness_sz;
  LIVENESS *_liveness;

  /* Has liveness information changed for any BB? */
  BOOL _liveness_changed;
  
  
  /* Return a pointer to the LIVENESS struct representing
     'bb'. Initialize it if needed. */
  LIVENESS *Get_Liveness (BB *bb)
  {
    Is_True(BB_id(bb) < _liveness_sz, ("unexpected BB for liveness"));
    LIVENESS *l = &_liveness[BB_id(bb)];
    if (l->_live_in == NULL)
    {
      l->_live_in = TN_SET_Create_Empty(Last_TN + 1, _pool);
      l->_live_out = TN_SET_Create_Empty(Last_TN + 1, _pool);
    }

    return l;
  }

  
public:
  BB_LIVENESS (MEM_POOL *pool, BOOL trace) :
    _pool(pool), _trace(trace)
  {
    _liveness_sz = PU_BB_Count + 1;
    _liveness = CXX_NEW_ARRAY(LIVENESS, _liveness_sz, _pool);
    memset(_liveness, 0, sizeof(LIVENESS) * _liveness_sz);
    _liveness_changed = FALSE;
  }

  /* Has liveness information for any BB changed? */
  BOOL Liveness_Changed (void) const { return _liveness_changed; }
  
  /* Return TRUE if 'tn' is live into 'bb'. */
  BOOL Is_Live_In (BB *bb, TN *tn)
  {
    if (TN_is_global_reg(tn))
      return GRA_LIVE_TN_Live_Into_BB(tn, bb);

    LIVENESS *l = Get_Liveness(bb);
    return TN_SET_MemberP(l->_live_in, tn);
  }

  /* Return TRUE if 'tn' is live outof 'bb'. */
  BOOL Is_Live_Out (BB *bb, TN *tn)
  {
    if (TN_is_global_reg(tn))
      return GRA_LIVE_TN_Live_Outof_BB(tn, bb);

    LIVENESS *l = Get_Liveness(bb);
    return TN_SET_MemberP(l->_live_out, tn);
  }

  /* Show that 'tn' is live-in in 'bb'. */
  void Set_Live_In (BB *bb, TN *tn)
  {
    if (!Is_Live_In(bb, tn))
    {
      _liveness_changed = TRUE;

      if (TN_is_global_reg(tn))
      {
	GRA_LIVE_Add_Live_In_GTN(bb, tn);
	GRA_LIVE_Add_Defreach_In_GTN(bb, tn);
      }
      else
      {
	LIVENESS *l = Get_Liveness(bb);
	l->_live_in = TN_SET_Union1D(l->_live_in, tn, _pool);
      }
    }
  }

  /* Show that 'tn' is not live-in in 'bb'. */
  void Clear_Live_In (BB *bb, TN *tn)
  {
    if (Is_Live_In(bb, tn))
    {
      _liveness_changed = TRUE;

      if (TN_is_global_reg(tn))
      {
	GRA_LIVE_Remove_Live_In_GTN(bb, tn);
      }
      else
      {
	LIVENESS *l = Get_Liveness(bb);
	l->_live_in = TN_SET_Difference1D(l->_live_in, tn);
      }
    }
  }

  /* Show that 'tn' is live-out in 'bb'. */
  void Set_Live_Out (BB *bb, TN *tn)
  {
    if (!Is_Live_Out(bb, tn))
    {
      _liveness_changed = TRUE;

      if (TN_is_global_reg(tn))
      {
	GRA_LIVE_Add_Live_Out_GTN(bb, tn);
        GRA_LIVE_Add_Defreach_Out_GTN(bb, tn);
      }
      else
      {
	LIVENESS *l = Get_Liveness(bb);
	l->_live_out = TN_SET_Union1D(l->_live_out, tn, _pool);
      }
    }
  }

  /* Show that 'tn' is not live-out in 'bb'. */
  void Clear_Live_Out (BB *bb, TN *tn)
  {
    if (Is_Live_Out(bb, tn))
    {
      _liveness_changed = TRUE;

      if (TN_is_global_reg(tn))
      {
	GRA_LIVE_Remove_Live_Out_GTN(bb, tn);
      }
      else
      {
	LIVENESS *l = Get_Liveness(bb);
	l->_live_out = TN_SET_Difference1D(l->_live_out, tn);
      }
    }
  }

};

static BB_LIVENESS *bb_liveness;


/* TN_PROP. Collection of ONE_TN_PROP entries containing
   copy-propagation information for each TN. TN_PROP maintains
   information for TNs that are being copy-propagated. For a TN that
   is the target of a copy, the corresponding ONE_TN_PROP entry for
   that TN holds the source TN of the copy and the copy OP itself. */
class TN_PROP_BASE
{
protected:
  struct ONE_TN_PROP
  {
    TN *_replace_tn;
    OP *_copy_op;
    UINT32 _flags;
    UINT8 _omega;
  };

  /* Tracing on? */
  const BOOL _trace;
  const char *_trace_msg_prefix;

  /* MEM_POOL for allocation. */
  MEM_POOL *const _pool;
  
  /* Array of copy-propation information for each TN. */
  UINT _tn_infos_sz;
  ONE_TN_PROP *_tn_infos;

  /* List of TNs for which copy propagation information is
     valid. Provides a fast way to visit all "active" copy
     propagations (instead of scanning all of '_tn_infos'. */
  TN_LIST *_active_cp_tns;

public:
  TN_PROP_BASE (MEM_POOL *pool, BOOL trace, const char *prefix) : 
    _pool(pool),
    _trace(trace),
    _trace_msg_prefix(prefix)
  {
    _tn_infos_sz = Last_TN + 1;
    _tn_infos = CXX_NEW_ARRAY(ONE_TN_PROP, _tn_infos_sz, _pool);
    memset(_tn_infos, 0, sizeof(ONE_TN_PROP) * _tn_infos_sz);
    _active_cp_tns = NULL;
  }

  TN_PROP_BASE (const TN_PROP_BASE& cp) :
    _trace(cp._trace),
    _pool(cp._pool),
    _trace_msg_prefix(cp._trace_msg_prefix)
  {
    _tn_infos_sz = cp._tn_infos_sz;
    _tn_infos = CXX_NEW_ARRAY(ONE_TN_PROP, _tn_infos_sz, _pool);
    memset(_tn_infos, 0, sizeof(ONE_TN_PROP) * _tn_infos_sz);
    _active_cp_tns = NULL;

    for (TN_LIST *tn_list = cp._active_cp_tns; tn_list; tn_list = TN_LIST_rest(tn_list))
    {
      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = cp.Get(tn);
      Set(tn, tn_prop->_replace_tn, tn_prop->_omega,
	  Dup_OP(tn_prop->_copy_op), tn_prop->_flags);
    }
  }

  ~TN_PROP_BASE () {
    CXX_DELETE_ARRAY(_tn_infos, _pool);
  }

  /* Return the ONE_TN_PROP for 'tn'. */
  ONE_TN_PROP *Get (TN *tn) const
  {
    Is_True(TN_number(tn) < _tn_infos_sz, ("tn out of range"));
    return &_tn_infos[TN_number(tn)];
  }

  /* Return the copy OP for 'tn'. */
  OP *Copy (TN *tn) const
  {
    Is_True(Has_Replacement(tn), ("no copy op"));
    ONE_TN_PROP *tn_prop = Get(tn);
    return tn_prop->_copy_op;
  }
  
  /* Return the copy-propagation flags for 'tn'. */
  UINT32 Flags (TN *tn) const
  {
    Is_True(Has_Replacement(tn), ("no flags"));
    ONE_TN_PROP *tn_prop = Get(tn);
    return tn_prop->_flags;
  }

  /* Merge 'flags' into the copy-propagation flags for 'tn'. */
  UINT32 Merge_Flags (TN *tn, UINT32 flags)
  {
    Is_True(Has_Replacement(tn), ("no flags"));
    ONE_TN_PROP *tn_prop = Get(tn);

    /* This merge should be conservative, for the current flags,
       setting them is conservative. */
    tn_prop->_flags |= flags;
    return tn_prop->_flags;
  }
  
  /* Return true if there is another TN to replace 'tn'. */
  BOOL Has_Replacement (TN *tn) const
  {
    ONE_TN_PROP *tn_prop = Get(tn);
    return (tn_prop->_replace_tn != NULL);
  }
  
  /* Return the replacement TN for 'tn'. */
  TN *Replacement (TN *tn) const
  {
    Is_True(Has_Replacement(tn), ("no replacement tn"));
    ONE_TN_PROP *tn_prop = Get(tn);
    return tn_prop->_replace_tn;
  }

  /* Replace 'op':'opnd' with 'tn's replacement TN. */
  void Replace_TN_Operand (TN *tn, OP *op, INT opnd)
  {

    ONE_TN_PROP *tn_prop = Get(tn);
    TN *new_tn = tn_prop->_replace_tn;
    TN *old_tn = OP_opnd(op, opnd);
    Is_True(new_tn, ("can't replace TN"));

    UINT8 omega = tn_prop->_omega;
    if (omega>0 && _CG_LOOP_info(op)==NULL)
      CG_LOOP_Init_Op(op);
    Set_OP_opnd(op, opnd, new_tn);
    if (omega>0)
      Set_OP_omega(op, opnd, omega);
    else if (Is_CG_LOOP_Op(op))
      Set_OP_omega(op, opnd, 0);

    if (_trace)
    {
      fprintf(TFile, "%s replaced operand ", _trace_msg_prefix);
      Print_TN(old_tn, FALSE);
      fprintf(TFile, " with ");
      Print_TN(new_tn, FALSE);
      if (omega>0)
        fprintf(TFile, "[%d]", omega);
      fprintf(TFile, "\n    new op: ");
      Print_OP_No_SrcLine(op);
    }
  }

  /* Replace 'op':'result' with 'tn's replacement TN. */
  void Replace_TN_Result (TN *tn, OP *op, INT result)
  {

    ONE_TN_PROP *tn_prop = Get(tn);
    TN *new_tn = tn_prop->_replace_tn;
    TN *old_tn = OP_result(op, result);

    Is_True(new_tn, ("can't replace TN"));
    FmtAssert(tn_prop->_omega==0,
            ("Trying to replace result with non-zero omega"));
    Set_OP_result(op, result, new_tn);

    if (_trace)
    {
      fprintf(TFile, "%s replaced result ", _trace_msg_prefix);
      Print_TN(old_tn, FALSE);
      fprintf(TFile, " with ");
      Print_TN(new_tn, FALSE);
      fprintf(TFile, "\n    new op: ");
      Print_OP_No_SrcLine(op);
    }
  }

  /* Show that 'res' can be replaced with 'opnd' due to copy 'op'. */
  void Set (TN *res, TN *opnd, UINT8 omega, OP *op, UINT32 flags)
  {
    Is_True(!Has_Replacement(res), ("can't overwrite existing copy propagation"));
    ONE_TN_PROP *tn_prop = Get(res);
    tn_prop->_replace_tn = opnd;
    tn_prop->_omega = omega;
    tn_prop->_copy_op = op;
    tn_prop->_flags = flags;
    if (omega>0)
      tn_prop->_flags |= CP_FLAG_NO_BACKWARD_ELIM;
    _active_cp_tns = TN_LIST_Push(res, _active_cp_tns, _pool);
  }
  
  /* Clear the ONE_TN_PROP information for 'tn'. */
  void Clear (TN *tn)
  {
    ONE_TN_PROP *tn_prop = Get(tn);
    tn_prop->_replace_tn = NULL;
    tn_prop->_omega = 0;
    tn_prop->_copy_op = NULL;
    tn_prop->_flags = 0;
    _active_cp_tns = TN_LIST_Delete(tn, _active_cp_tns);
  }

  /* Drop the copy targeting 'tn'. */
  void Drop_Copy (TN *tn)
  {
    if (_trace)
    {
      fprintf(TFile, "%s dropping unnecessary copy of TN:%d (0x%x), ",
              _trace_msg_prefix, TN_number(tn), Flags(tn));
      Print_OP_No_SrcLine(Copy(tn));
    }

    Clear(tn);
  }

  /* Stop propagating the copy targeting 'tn'. If 'loc' is
     EBO_CP_BEFORE, do nothing since the copy op was not removed.
     If 'loc' is EBO_CP_AFTER, emit the copy
     after 'op' in 'bb', or if 'op' is NULL, at the beginning of
     'bb'. */
  void Stop_Copy_Prop (TN *tn, BB *bb, OP *op, EBO_CP_LOC loc)
  {
    ONE_TN_PROP *tn_prop = Get(tn);
    Is_True(Has_Replacement(tn), ("expecting replaceable tn"));

    if (_trace)
    {
      fprintf(TFile, "%s stopping propagation of ", _trace_msg_prefix);
      Print_OP_No_SrcLine(tn_prop->_copy_op);
    }

    OP* cp_op = tn_prop->_copy_op;
    BB* cp_bb = OP_bb(cp_op);

    if (tn_prop->_flags & CP_FLAG_PLACEMENT_NEEDED) {

      // initialize the op to be placed in loop if the loop info is not
      // initialized for the op
      if (EBO_in_loop && _CG_LOOP_info(tn_prop->_copy_op)==NULL)
        CG_LOOP_Init_Op(tn_prop->_copy_op);

      if (!op)
      {
        BB_Prepend_Op(bb, cp_op);
      }
      else
      {
        BB_Insert_Op_After(bb, op, cp_op);
      }
    }
    Clear(tn);
  }
  
  /* Stop propagating all copies and emit them before 'op' in 'bb', or
     if 'op' is NULL, at the end of 'bb'. */
  void Stop_All_Copy_Props (BB *bb, OP *op, EBO_CP_LOC loc, bool reverse_order=false)
  {
    TN_LIST *tmp_list=NULL;
    TN_LIST *next;
    if (reverse_order) {
      for (TN_LIST *tn_list = _active_cp_tns; tn_list; tn_list = TN_LIST_rest(tn_list)) {
         tmp_list = TN_LIST_Push (TN_LIST_first(tn_list), tmp_list, _pool);
      }
    } else {
      tmp_list = _active_cp_tns;
    }
      
    for (TN_LIST *tn_list = tmp_list; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));
      Stop_Copy_Prop(tn, bb, op, loc);
    }
  }

  void Print (UINT indent)
  {
    for (TN_LIST *tn_list = _active_cp_tns; tn_list; tn_list = TN_LIST_rest(tn_list))
    {
      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      fprintf(TFile, "%*sTN:%d (0x%x), ", indent, "",
	      TN_number(tn), tn_prop->_flags);
      if (tn_prop->_copy_op)
	Print_OP_No_SrcLine(tn_prop->_copy_op);
      else
	fprintf(TFile, "<corrupt list>\n");
    }
  }

};

class TN_PROP : public TN_PROP_BASE
{
public:
  TN_PROP(MEM_POOL *pool, BOOL trace)
    : TN_PROP_BASE(pool, trace, "<ebo_cp>") { }

  TN_PROP (const TN_PROP& cp)
    : TN_PROP_BASE(cp) { }


#if 0

  BOOL Find_Def (TN *rtn, UINT8 rtn_omega, TN *except_tn) const
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));

      /* If 'except_tn' is the same as 'tn', then we don't stop this
         copy, since the equality indicates that we knowingly replaced
         the TN with the intent of allowing the copy to propagate past
         it. */
      if ((_tn_infos[TN_number(tn)]._replace_tn == rtn) &&
          (_tn_infos[TN_number(tn)]._omega == rtn_omega) &&
          (!except_tn || (except_tn != tn)))
        return TRUE;
    }
    return FALSE;
  }

#endif

  /* Return the omega for the replacement TN for 'tn'. */
  UINT8 Replacement_Omega (TN *tn) const
  {
    Is_True(Has_Replacement(tn), ("no replacement tn"));
    ONE_TN_PROP *tn_prop = Get(tn);
    return tn_prop->_omega;
  }

  /* Stop all copy propagations where 'rtn' is the replacing TN. Emit
     any pending copies before/after 'op' in 'bb'. */
  void Stop_Def_Copy_Props (TN *rtn, TN *orig_tn,
                            BB *bb, OP *op, EBO_CP_LOC loc)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));

      /* If 'orig_tn' is the same as 'tn', then we don't stop this
         copy, since the equality indicates that we knowingly replaced
         the TN with the intent of allowing the copy to propagate past
         it. */
      if ((_tn_infos[TN_number(tn)]._replace_tn == rtn) &&
          (!orig_tn || (orig_tn != tn)))
        Stop_Copy_Prop(tn, bb, op, loc);
    }
  }

  /* Stop all copy propagations where 'rtn' is the replacing TN.
     Emit any pending copies before/after 'op' in 'bb'. */
  void Stop_Duplicate_Copy_Props (TN *rtn, TN *except_tn,
                             BB *bb, OP *op, EBO_CP_LOC loc)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));

      /* If 'except_tn' is the same as 'tn', then we don't stop this
         copy. */
      if ((_tn_infos[TN_number(tn)]._replace_tn == rtn) &&
          (!except_tn || (except_tn != tn)))
        Stop_Copy_Prop(tn, bb, op, loc);
    }
  }

  /* Stop propagating all copies where the source or destination TN is
     assigned a register, and emit them before/after 'op' in 'bb'. */
  void Stop_All_Assigned_Copy_Props (BB *bb, OP *op, EBO_CP_LOC loc)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));
      if ((TN_register(tn) != REGISTER_UNDEFINED) ||
          (TN_register(Replacement(tn)) != REGISTER_UNDEFINED))
        Stop_Copy_Prop(tn, bb, op, loc);
    }
  }

  /* Adjust 'this' copy propagation information as necessary to move
     forward into 'bb'. */
  void Adjust_Forward_Entry (BB *bb, BOOL update_liveness)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      /* If 'tn' is not live into 'bb', then we don't need to
         propagate the corresponding copy. */
      if (!bb_liveness->Is_Live_In(bb, tn))
      {
        Drop_Copy(tn);
      }
      else if (update_liveness)
      {
        /* Otherwise, show that 'tn' is no longer live-in, and the
           replacement TN is now live-in. */
        bb_liveness->Set_Live_In(bb, tn_prop->_replace_tn);
      }
    }
  }

  /* Make sure we don't have any duplicate replacements being
     propagated, like:

     mov b <- a
     mov c <- a

     since during backwards propagation we can't correctly handle
     these. It's much easier to stop all but one of the copies now,
     then to try to take care of it during backward propagation. */
  void Eliminate_Forward_Duplicates (BB *bb, OP *op)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      {
        /* We stop the propagation of this copy if it involves any
           assigned registers. otherwise we stop propagation of all
           other copies that have the same replacement TN, and that
           are also live out-of 'bb'. */
        if ((TN_register(tn) != REGISTER_UNDEFINED) ||
            (TN_register(tn_prop->_replace_tn) != REGISTER_UNDEFINED))
        {
          Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
        }
        else
        {
          Stop_Duplicate_Copy_Props(tn_prop->_replace_tn, tn,
                               bb, op, EBO_CP_BEFORE);
        }
      }
      next = TN_LIST_rest(tn_list);
    }
  }

  /* Remove copies that has non-zero omega.
   * This is called in Propagate_EBO before invert
   * without exiting the current block in forward propagation
   * since copy with non-zero omega cannot be inverted.
   */
  void Stop_Copy_Non_Omega_Copy ()
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      if (tn_prop->_omega!=0)
      {
        Stop_Copy_Prop(tn, /*bb=*/NULL, NULL, EBO_CP_BEFORE);
      }
    }
  }

  /* Adjust 'this' copy propagation information as necessary to move
     forward out-of 'bb'. Do this by removing any copies that target
     TNs not live out-of 'bb'. */
  void Adjust_Forward_Exit (BB *bb)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      /* If 'tn' is not live outof 'bb', then we don't need to
         propagate the corresponding copy. */
      if (!bb_liveness->Is_Live_Out(bb, tn))
      {
        Drop_Copy(tn);
      }
      /* Don't propagate copies involving assigned TNs. */
      else if ((TN_register(tn) != REGISTER_UNDEFINED) ||
               (TN_register(tn_prop->_replace_tn) != REGISTER_UNDEFINED))
      {
        Stop_Copy_Prop(tn, bb, NULL, EBO_CP_BEFORE);
      }
      /* Don't propagate copies involving non-zero omegas since we do not
         know where the loop body ends. This is conservative but works
         for single BB swp loop.
      */
      else if (tn_prop->_omega!=0)
      {
        Stop_Copy_Prop(tn, bb, NULL, EBO_CP_BEFORE);
      }
      /* If the replacement TN is live-out, then show that this copy
         cannot be eliminated during backwards propagation, since that
         would eliminate the definition of the replacement TN. */
      else if (bb_liveness->Is_Live_Out(bb, tn_prop->_replace_tn))
      {
        tn_prop->_flags |= CP_FLAG_NO_BACKWARD_ELIM;
      }
      else
      {
        /* Otherwise, show that 'tn' is no longer live-out, and the
           replacement TN is now live-out. */
        bb_liveness->Set_Live_Out(bb, tn_prop->_replace_tn);
      }
    }
  }

  /* Handle any adjustments needed at the end of 'bb' before
     propagating backwards. */
  void Adjust_Backward_Exit (BB *bb)
  {
    // nothing to do here, we must maintain the liveness information...
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      bb_liveness->Set_Live_Out(bb, tn_prop->_replace_tn);
#if 0
      if (!TN_is_global_reg(tn))
        bb_liveness->Clear_Live_Out(bb, tn);
#endif
    }
  }

  /* Handle any adjustments needed at the entry of 'bb' before
     propagating backwards. */
  void Adjust_Backward_Entry (BB *bb)
  {
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);

      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      /* Don't propagate copies involving assigned TNs. */
      if ((TN_register(tn) != REGISTER_UNDEFINED) ||
          (TN_register(tn_prop->_replace_tn) != REGISTER_UNDEFINED))
      {
        Stop_Copy_Prop(tn, bb, BB_last_op(bb), EBO_CP_AFTER);
      } else {
        bb_liveness->Set_Live_In(bb, tn_prop->_replace_tn);
#if 0
        if (!TN_is_global_reg(tn))
          bb_liveness->Clear_Live_In(bb, tn);
#endif
      }
    }
  }

  /* Invert the TN information so that if "b" is replaced by "a",
     instead show "a" being replaced by "b". */
  void Invert (void)
  {
    if (_trace)
    {
      fprintf(TFile, "%s Pre-invert\n", _trace_msg_prefix);
      Print(4);
    }
    
    Check();

    TN_LIST *tn_list = _active_cp_tns;
    _active_cp_tns = NULL;

    for (; tn_list; tn_list = TN_LIST_rest(tn_list))
    {
      TN *tn = TN_LIST_first(tn_list);
      ONE_TN_PROP *tn_prop = Get(tn);

      /* We should not have both "a" replaces "b" and "b" replaces
         "a", so we can invert '_tn_infos' in place. */
      FmtAssert(!Has_Replacement(tn_prop->_replace_tn),
		("unexpected mutual replacement TNs"));

      FmtAssert (tn_prop->_omega==0,
		 ("_replace_tn with Non-zero omega left in Invert()"));
      Set(tn_prop->_replace_tn, tn, 0, tn_prop->_copy_op, tn_prop->_flags);
      Clear(tn);
    }

    if (_trace)
    {
      fprintf(TFile, "%s Post-invert\n", _trace_msg_prefix);
      Print(4);
    }
    
    Check();
  }

  /* Make sure the '_active_cp_tns' is consistant with '_tn_infos'. */
  BOOL Check (void) const
  {
#ifdef Is_True_On
    BOOL *active = (BOOL *)alloca(sizeof(BOOL) * _tn_infos_sz);
    memset(active, 0, sizeof(BOOL) * _tn_infos_sz);
    for (TN_LIST *tn_list = _active_cp_tns; tn_list; tn_list = TN_LIST_rest(tn_list))
    {
      TN *tn = TN_LIST_first(tn_list);
      Is_True(Has_Replacement(tn), ("_active_cp_tns contains entry not in _tn_infos"));
      Is_True(!active[TN_number(tn)], ("multiple _active_cp_tns entries for tn:%d",
				       TN_number(tn)));
      active[TN_number(tn)] = TRUE;
    }

    for (UINT i = 0; i < _tn_infos_sz; i++)
      Is_True((_tn_infos[i]._replace_tn != NULL) == active[i],
	      ("_tn_infos contains entry not in _active_cp_tns"));
#endif
    return TRUE;
  }

  void Prop_Copy_Forward_SINGLE_BB_LOOP  (BB *prolog, BB *body);
  void Prop_Copy_Backward_SINGLE_BB_LOOP (BB *epilog, BB *body);
};

void
TN_PROP::Prop_Copy_Forward_SINGLE_BB_LOOP (BB *prolog, BB *body)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    ONE_TN_PROP *tn_prop = Get(tn);

    /* If 'tn' is not live outof 'prolog', then we don't need to
       propagate the corresponding copy. */
    if (!bb_liveness->Is_Live_Out(prolog, tn))
    {
       Drop_Copy(tn);
    }
    /* Don't propagate copies involving assigned TNs. */
    else if ((TN_register(tn) != REGISTER_UNDEFINED) ||
             (TN_register(tn_prop->_replace_tn) != REGISTER_UNDEFINED))
    {
      Stop_Copy_Prop(tn, prolog, NULL, EBO_CP_BEFORE);
    }
    /* Don't propagate copies involving non-zero omegas since we do not
       know where the loop body ends. This is conservative but works
       for single BB swp loop.
    */
    else if (tn_prop->_omega!=0)
    {
      Stop_Copy_Prop(tn, prolog, NULL, EBO_CP_BEFORE);
    }
    else
    {
      // Processing the copy 
      //     dst(tn) = src(_replace_tn)
      //
      TN *dst = tn;
      TN *src = tn_prop->_replace_tn;
      bool def_dst = BB_Defs_TN (body, dst);
      bool def_src = BB_Defs_TN (body, src);
      bool ref_dst = BB_Refs_TN (body, dst);
      bool ref_src = BB_Refs_TN (body, src);
      bool is_dst_dead = !bb_liveness->Is_Live_Out(prolog, dst);
      bool is_src_dead = !bb_liveness->Is_Live_Out(prolog, src);

      if ( is_dst_dead ) {
        Stop_Copy_Prop(dst, prolog, NULL, EBO_CP_BEFORE);
      } else if (
         !def_dst && !def_src && !is_src_dead && (ref_dst || ref_src) || 
         !def_src && !ref_src && is_src_dead  && 
           (def_dst && !TN_is_rematerializable(src) || !def_dst && ref_dst) ) {
        // move the copy into epilog
        BB *epilog = BB_Other_Successor (body, body);
        OP *copy_op = Dup_OP(tn_prop->_copy_op);
        BB_Insert_Op(epilog, NULL, copy_op, TRUE);
        
        BB_Replace_TN (body, dst, src);
        Clear (dst);

        if (_trace) {
          fprintf(TFile, "%s Propagate forward over SINGLE_BB Loop (BB:%d)\n", 
                          _trace_msg_prefix, BB_id(body));
          fprintf(TFile, "%s replaced operand ", _trace_msg_prefix);
          Print_TN(dst, FALSE);
          fprintf(TFile, " with ");
          Print_TN(src, FALSE);
          fprintf(TFile, "\n        move copy to epilog (BB:%d)\n", BB_id(epilog));
/*
          if (_omega>0)
            fprintf(TFile, "[%d]", omega);
*/
        }

        bb_liveness->Set_Live_Out(prolog, src);
        bb_liveness->Set_Live_In(body, src);
        bb_liveness->Set_Live_Out(body, src);
        bb_liveness->Set_Live_In(epilog, src);

      } else {
        Stop_Copy_Prop(dst, prolog, NULL, EBO_CP_BEFORE);
      }
    }
  }

  // Now _active_cp_tns must be NULL
  Is_True (_active_cp_tns == NULL, ("No copies are active at this point"));
  Propagate_BB_Forward (body, this, _trace);
  if (!EBO_Copy_Prop_Intra) {
    // Need to stop all propagation for the body BB.
    //   If EBO_Copy_Prop_Intra is true, no need to do so
    //   because it has been done within Propagate_BB_Forward().
    Stop_All_Copy_Props(body, NULL, EBO_CP_BEFORE);
  }
}

void
TN_PROP::Prop_Copy_Backward_SINGLE_BB_LOOP (BB *epilog, BB *body)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    ONE_TN_PROP *tn_prop = Get(tn);

    /* If 'tn' is not live out into 'prolog', then we don't need to
       propagate the corresponding copy. */
    if (!bb_liveness->Is_Live_In(epilog, tn))
    {
       Drop_Copy(tn);
    }
    /* Don't propagate copies involving assigned TNs. */
    else if ((TN_register(tn) != REGISTER_UNDEFINED) ||
             (TN_register(tn_prop->_replace_tn) != REGISTER_UNDEFINED))
    {
      Stop_Copy_Prop(tn, epilog, NULL, EBO_CP_AFTER);
    }
    /* Don't propagate copies involving non-zero omegas since we do not
       know where the loop body ends. This is conservative but works
       for single BB swp loop.
    */
    else if (tn_prop->_omega!=0)
    {
      Stop_Copy_Prop(tn, epilog, NULL, EBO_CP_AFTER);
    }
    else
    {
      // Processing the copy 
      //    dst(_replace_tn) = src(tn)
      //
      TN *src = tn;
      TN *dst = tn_prop->_replace_tn;
      bool def_dst = BB_Defs_TN (body, dst);
      bool ref_dst = BB_Refs_TN (body, dst);
      //bool is_dst_dead = !bb_liveness->Is_Live_Out(epilog, dst);
      bool is_dst_dead = TN_Dead(dst, epilog, BB_first_op(epilog));
      //bool is_src_dead = !bb_liveness->Is_Live_Out(epilog, src);
      bool is_src_dead = TN_Dead(src, epilog, BB_first_op(epilog));
      BB *prolog = BB_Other_Predecessor (body, body);
      OP *branch_op = BB_xfer_op(prolog);

      if ( is_dst_dead ) {
        Stop_Copy_Prop(tn, epilog, NULL, EBO_CP_AFTER);
      } else if ( !def_dst && !ref_dst && is_src_dead && 
                  (branch_op == NULL || !OP_Refs_TN(branch_op, dst)) ) {
        if ( bb_liveness->Is_Live_Out(prolog, src) ) {
          // move the copy into prolog
          OP *copy_op = Dup_OP(tn_prop->_copy_op);
          if (branch_op != NULL) {
            BB_Insert_Op_Before(prolog, branch_op, copy_op);
          } else {
            BB_Insert_Op (prolog, NULL, copy_op, FALSE);
          }

          if (_trace) {
            fprintf(TFile, "%s Propagate backward over SINGLE_BB Loop (BB:%d)\n", 
                            _trace_msg_prefix, BB_id(body));
            fprintf(TFile, "%s replaced operand ", _trace_msg_prefix);
            Print_TN(src, FALSE);
            fprintf(TFile, " with ");
            Print_TN(dst, FALSE);
            fprintf(TFile, "\n        move copy to prolog (BB:%d)\n", BB_id(prolog));
          }
        } else {
          if (_trace) {
            fprintf(TFile, "%s Propagate over SINGLE_BB Loop (BB:%d)\n", 
                            _trace_msg_prefix, BB_id(body));
            fprintf(TFile, "%s replaced operand ", _trace_msg_prefix);
            Print_TN(src, FALSE);
            fprintf(TFile, " with ");
            Print_TN(dst, FALSE);
            fprintf(TFile, "\n        remove the copy\n");
          }
        }
        
        BB_Replace_TN (body, src, dst);
        Clear (src);

        if (bb_liveness->Is_Live_Out(prolog, src)) {
          bb_liveness->Set_Live_In(body, dst);
          bb_liveness->Set_Live_Out(prolog, dst);
        }
        bb_liveness->Set_Live_Out(body, dst);
        bb_liveness->Set_Live_In(epilog, dst);
      } else {
        Stop_Copy_Prop(tn, epilog, NULL, EBO_CP_AFTER);
      }
    }
  }

  // Now _active_cp_tns must be NULL
  Is_True (_active_cp_tns == NULL, ("No copies are active at this point"));
  Propagate_BB_Backward (body, this, _trace);
  if (!EBO_Copy_Prop_Intra) {
    // Need to stop all propagation for the body BB.
    //   If EBO_Copy_Prop_Intra is true, no need to do so
    //   because it has been done within Propagate_BB_Backward().
    Stop_All_Copy_Props(body, NULL, EBO_CP_AFTER);
  }
}


class REG_PROP : public TN_PROP_BASE
{
private:

  BB *first_bb;
  bool *visitedbbs;  // temporary for visiting BBs

  TN *Reg_Get (TN *tn) const
  {
    TN *target_tn = tn;
    for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
    {
      next = TN_LIST_rest(tn_list);
      TN *tmp_tn = TN_LIST_first(tn_list);
      Is_True((_tn_infos[TN_number(tmp_tn)]._replace_tn != NULL),
              ("corrupt active copy-propagations list"));
      if ( tn_registers_identical(tmp_tn, tn) ) {
        target_tn = tmp_tn;
        break;
      }
    }
    return target_tn;
  }

  BOOL Can_Reg_TN_Be_Replaced (BB *bb, OP *op, int index, BOOL is_result=FALSE);
  BOOL BB_Reg_Replaceable (BB *bb, ISA_REGCLASS rc, REGISTER reg);
  BOOL Reg_TN_Next_Ref (BB *bb, OP *op, int opnd_ix);
  BB  *Reg_TN_unique_def_bb(TN *tn, BB *use_bb);

public:

  REG_PROP (MEM_POOL *pool, BB *the_first_bb, BOOL trace) :
    TN_PROP_BASE(pool, trace, "<ebo_reg_cp>"),
    first_bb (the_first_bb)
  {
    FmtAssert (EBO_in_peep, ("Must create REG_PROP's object in EBO_in_peep"));
    visitedbbs = NULL;
  }

  REG_PROP (const REG_PROP& cp) :
    TN_PROP_BASE (cp)
  {
    visitedbbs = NULL;
    first_bb = cp.first_bb;
  }

  void set_reg_live_in(ISA_REGCLASS cl, REGISTER reg, BB *bb, OP *op);

//
// member functions that overide those in the base class
//
  ONE_TN_PROP *Get (TN *tn) const
  {
    TN *target_tn = Reg_Get(tn);
    
    return TN_PROP_BASE::Get(target_tn);
  }

  OP *Copy (TN *tn) const
  {
    TN *target_tn = Reg_Get(tn);

    return TN_PROP_BASE::Copy(target_tn);
  }

  UINT32 Flags (TN *tn) const
  {
    TN *target_tn = Reg_Get(tn);

    return TN_PROP_BASE::Flags(target_tn);
  }

  UINT32 Merge_Flags (TN *tn, UINT32 flags)
  {
    TN *target_tn = Reg_Get(tn);

    return TN_PROP_BASE::Merge_Flags(target_tn, flags);
  }

  BOOL Has_Replacement (TN *tn) const
  {
    TN *target_tn = Reg_Get(tn);

    return TN_PROP_BASE::Has_Replacement(target_tn);
  }

  TN *Replacement (TN *tn) const
  {
    TN *target_tn = Reg_Get(tn);

    return TN_PROP_BASE::Replacement(target_tn);
  }

  void Replace_TN_Operand (TN *tn, OP *op, INT opnd) {
    TN *target_tn = Reg_Get(tn);
    TN_PROP_BASE::Replace_TN_Operand(target_tn, op, opnd);

    // Update REG_LIVE info
    BB *bb=OP_bb(op);
    if (bb) {
      TN *repl_tn = TN_PROP_BASE::Replacement(target_tn);
      Is_True(TN_is_register(repl_tn), ("Copy's source isn't register TN"));
      set_reg_live_in(TN_register_class(repl_tn), TN_register(repl_tn), bb, op);
    }
  }

  void Replace_TN_Result (TN *tn, OP *op, INT result) {
    TN *target_tn = Reg_Get(tn);
    TN_PROP_BASE::Replace_TN_Result(target_tn, op, result);
  }

  void Drop_Copy(TN *tn) 
  {
    TN *target_tn = Reg_Get(tn);
    TN_PROP_BASE::Drop_Copy(target_tn);
  }
    
  void Stop_Copy_Prop(TN *tn, BB *bb, OP *op, EBO_CP_LOC loc) {
    TN *target_tn = Reg_Get(tn);

    TN_PROP_BASE::Stop_Copy_Prop(target_tn, bb, op, loc);
  }


  void Stop_Copy_At_Call (BB *bb, OP *op, EBO_CP_LOC loc);
  void Stop_Copy_At_Exit (BB *bb, OP *op, EBO_CP_LOC loc);
  void Stop_Copy_At_Entry(BB *bb, OP *op, EBO_CP_LOC loc);
  void Stop_Reg_Def_Copy(ISA_REGCLASS, REGISTER, REGISTER, BB *, OP *, EBO_CP_LOC); 
  bool Propagate_Reg_Forward_EBO(BB *);
  bool Propagate_Reg_Backward_EBO(BB *);
  bool Copy_Forward_SwpRegion(BB *, BB *, BB *);
  bool Copy_Backward_SwpRegion(BB *, BB *, BB *);
  bool Copy_BB_Forward(BB *);
  bool Copy_BB_Backward(BB *);
  void Adjust_Reg_Forward_Exit (BB *);
  void Adjust_Reg_Backward_Exit (BB *);
  void Adjust_Reg_Forward_Entry (BB *);
  void Adjust_Reg_Backward_Entry (BB *);
};

void REG_PROP::set_reg_live_in (ISA_REGCLASS cl, REGISTER reg, BB *bb, OP *op)
{
  OP *t_op;
  BOOL is_killed = FALSE;
  for (t_op = OP_prev(op); t_op; t_op = OP_prev(t_op)) {
    if (OP_Defs_Reg(t_op, cl, reg)) {
      is_killed = TRUE;
      break; 
    }
  }
  if (!is_killed) {
    REG_LIVE_Update(cl, reg, bb);
  }
}

void REG_PROP::Stop_Copy_At_Call (BB *bb, OP *op, EBO_CP_LOC loc)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    TN *replace_tn = _tn_infos[TN_number(tn)]._replace_tn;
    Is_True(replace_tn, ("corrupt active copy-propagations list"));
    Is_True(TN_register_class(tn) == TN_register_class(replace_tn),
            ("Copy's target and source must be in the same register class"));
    Is_True( (TN_register(tn) != REGISTER_UNDEFINED) && 
             (TN_register(replace_tn) != REGISTER_UNDEFINED),
             ("TNs should've been assigned registers already") );

    // call_regs are all registers that are modified by call.
    REGISTER_SET call_regs = REGISTER_CLASS_caller_saves(TN_register_class(tn));
    if ( REGISTER_SET_MemberP(call_regs, TN_register(tn)) ||
         REGISTER_SET_MemberP(call_regs, TN_register(replace_tn)) )
    {
      Stop_Copy_Prop(tn, bb, op, loc);
    }
  }
}


void REG_PROP::Stop_Copy_At_Exit (BB *bb, OP *op, EBO_CP_LOC loc)
{
  // exit_regs are all registers that are live out of the PU.
  REGISTER_SET exit_regs;
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_EXITINFO);
  EXITINFO *my_ant = ANNOT_exitinfo(ant);
  Is_True(my_ant != 0, ("No EXITINFO annotation"));

  exit_regs = REGISTER_SET_EMPTY_SET;
  TN_LIST *tnl;
  for (tnl = EXITINFO_ret_value(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
    TN *tmp_tn = TN_LIST_first(tnl);
    exit_regs = REGISTER_SET_Union1 (exit_regs, TN_register(tmp_tn));
  }

  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    TN *replace_tn = _tn_infos[TN_number(tn)]._replace_tn;
    Is_True(replace_tn, ("corrupt active copy-propagations list"));
    Is_True(TN_register_class(tn) == TN_register_class(replace_tn),
            ("Copy's target and source must be in the same register class"));
    Is_True( (TN_register(tn) != REGISTER_UNDEFINED) && 
             (TN_register(replace_tn) != REGISTER_UNDEFINED),
             ("TNs should've been assigned registers already") );

    exit_regs = REGISTER_SET_Union(exit_regs, 
                                   REGISTER_CLASS_callee_saves(TN_register_class(tn)));
    if ( REGISTER_SET_MemberP(exit_regs, TN_register(tn)) ||
         REGISTER_SET_MemberP(exit_regs, TN_register(replace_tn)) )
    {
      Stop_Copy_Prop(tn, bb, op, loc);
    }
  }
}

void REG_PROP::Stop_Copy_At_Entry (BB *bb, OP *op, EBO_CP_LOC loc)
{
  // entry_regs are all registers that are live-in of the PU.
  REGISTER_SET entry_regs;
  ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_ENTRYINFO);
  ENTRYINFO *my_ant = ANNOT_entryinfo(ant);
  Is_True(my_ant != 0, ("No ENTRYINFO annotation"));

  entry_regs = REGISTER_SET_EMPTY_SET;
  TN_LIST *tnl;
  for (tnl = ENTRYINFO_in_params(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
    TN *tmp_tn = TN_LIST_first(tnl);
    entry_regs = REGISTER_SET_Union1 (entry_regs, TN_register(tmp_tn));
  }

  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    TN *replace_tn = _tn_infos[TN_number(tn)]._replace_tn;
    Is_True(replace_tn, ("corrupt active copy-propagations list"));
    Is_True(TN_register_class(tn) == TN_register_class(replace_tn),
            ("Copy's target and source must be in the same register class"));
    Is_True( (TN_register(tn) != REGISTER_UNDEFINED) && 
             (TN_register(replace_tn) != REGISTER_UNDEFINED),
             ("TNs should've been assigned registers already") );

    if ( REGISTER_SET_MemberP(entry_regs, TN_register(tn)) ||
         REGISTER_SET_MemberP(entry_regs, TN_register(replace_tn)) )
    {
      Stop_Copy_Prop(tn, bb, op, loc);
    }
  }
}

/*
  Do the same thing for registers as what TN_PROP::Adjust_Forward_Exit()
  does for TNs. 
*/
void REG_PROP::Adjust_Reg_Forward_Exit(BB *bb)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     reg = TN_register(tn);

    /*
       If '<rc, reg>' is not live out of 'bb', then we don't need
       to propagate the corresponding copy.
     */
    if (!REG_LIVE_Outof_BB(rc, reg, bb)) 
    {
      TN_PROP_BASE::Drop_Copy(tn);
    }
  }
}

void REG_PROP::Adjust_Reg_Backward_Exit(BB *bb)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     reg = TN_register(tn);

    /*
       If '<rc, reg>' is not live into 'bb', then we don't need
       to propagate the corresponding copy.
     */
    if (!REG_LIVE_Into_BB(rc, reg, bb))
    {
      TN_PROP_BASE::Stop_Copy_Prop(tn, bb, NULL, EBO_CP_AFTER);
    }
  }
}


/*
   This func does the similar things for registers as 
   TN_PROP::Adjust_Forward_Entry() does for TNs.
*/
void REG_PROP::Adjust_Reg_Forward_Entry(BB *bb)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     reg = TN_register(tn);

    /*
       If '<rc, reg>' is not live into 'bb', then we don't need to
       propagate the corresponding copy.
     */
    if (!REG_LIVE_Into_BB(rc, reg, bb))
    {
      TN_PROP_BASE::Drop_Copy(tn); 
    }
  }
}

void REG_PROP::Adjust_Reg_Backward_Entry(BB *bb)
{
  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     reg = TN_register(tn);

    /*
       If '<rc, reg>' is not live out of 'bb', then we don't need to
       propagate the corresponding copy.
     */
    if (!REG_LIVE_Outof_BB(rc, reg, bb))
    {
      TN_PROP_BASE::Stop_Copy_Prop(tn, bb, BB_last_op(bb), EBO_CP_AFTER);
    }
  }
}



/* If 'op' is a copy, return the operand index of the source
   TN. Return -1 if 'op' is not a copy. */
static INT
EBO_Prop_Copy_Operand (OP *op)
{
  INT opnd_idx = CGTARG_Copy_Operand(op);
  if (opnd_idx < 0)
    return opnd_idx;
  
  TN *res = OP_result(op, 0);
  TN *opnd = OP_opnd(op, opnd_idx);
    
  if (TN_register_class(res) != TN_register_class(opnd)) {
    return -1;
  }

  return opnd_idx;
}

/*
  Check if copy 'dst = src' can be profitably propagated into
  a same result OP (that is the prev of 'op'). If so, return
  TRUE, otherwise, return FALSE.

  It checks if the copy 'dst =src' will be stopped (so the copy
  will not be deleted) by OPs started from 'op'.  If so, the
  copy will not be deleted and the func returns FALSE; otherwise,
  return TRUE. 

  It skips over all COPY of form 'src = dst', because
      dst = src
      src = dst
   makes the 'src = dst' redundant.
*/
static BOOL Copy_Prop_Profitable (BB *bb, OP *op, TN *dst, TN *src)
{
  ISA_REGCLASS rc  = TN_register_class(dst);
  REGISTER reg_dst = TN_register(dst);
  REGISTER reg_src = TN_register(src);

  OP *o1 = op;
  while (o1) {
    INT copy_opnd_idx = EBO_Prop_Copy_Operand(o1);

    // Skip over self copy here
    if ( (copy_opnd_idx >=0) &&
         (TN_register_class(OP_result(o1, 0)) == rc) &&
         (TN_register(OP_result(o1, 0)) == reg_src) &&        
         (TN_register(OP_opnd(o1, copy_opnd_idx)) == reg_dst) )
    {
      o1 = OP_next(o1);
    } else if (OP_call(o1)) {
      // assume that the copy will be stopped.
      return FALSE;
    } else if (OP_Defs_Reg(o1, rc, reg_dst)) {
      // the copy is redundant and will be deleted
      return TRUE;
    } else if (OP_Defs_Reg(o1, rc, reg_src)) {
      // the copy is not redundant, must be stopped
      return FALSE;
    } else {
      o1 = OP_next(o1);
    }
  }       

  return TRUE;
}

/* Return TRUE if 'tn' is dead after 'op' in 'bb'. Finding this
   information requires a forward walk of the OPs, so use this routine
   sparingly. */
static BOOL
TN_Dead (TN *tn, BB *bb, OP *op)
{
  /* Search forward from 'op' looking for a use or def of 'tn', or the
     end of the block. */
  if (op)
  {
//    for (op = OP_next(op); op; op = OP_next(op))
    for (; op; op = OP_next(op))
    {
      if (OP_Refs_TN(op, tn))
	return FALSE;
      
      /* If 'op' is a call, conservatively return that 'tn' is live if
	 it is assigned a register. */
      if ((OP_call(op) || OP_code(op)==TOP_spadjust) &&
	  (TN_register(tn) != REGISTER_UNDEFINED))
	return FALSE;
      
      if (OP_Defs_TN(op, tn))
	return TRUE;
    }
  }

  /* At the end of the block, use the live-out information. */
  return !bb_liveness->Is_Live_Out(bb, tn);
}

/* Mimic of TN_Dead */
static BOOL
Reg_Dead(ISA_REGCLASS rc, REGISTER reg, BB *bb, OP *op)
{
  // Search forward from 'op', looking for a use or def to <rc, reg>.
  // When reaching the end of the block, use REG_LIVE info.
  if (op)
  {
    for (op = OP_next(op); op; op = OP_next(op))
    {
      if ( OP_Refs_Reg(op, rc, reg) )
        return FALSE;

      if (OP_call(op)) {
        ANNOTATION  *ant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
        CALLINFO *my_ant = ANNOT_callinfo(ant);
        REGISTER_SET call_regs = REGISTER_CLASS_caller_saves(rc);
        if (my_ant == 0)
        { // Since no actual argument info, conservatively assume all caller_save
          // registers are used by the call.
          if ( REGISTER_SET_MemberP(call_regs, reg) )
            return FALSE;
        } else {
          TN_LIST *tnl;
          for (tnl = CALLINFO_actual_params(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
            TN *tmp_tn = TN_LIST_first(tnl);
            if (TN_register_class(tmp_tn) == rc &&
                TN_register(tmp_tn) == reg) {
              return FALSE;
            }
          }
 
          // Now, <rc, reg> isn't an actual argument. If it is a caller-save reg,
          // it should be dead (unless cg does an IPA register allocation, which
          // is not true at this time).
          if ( REGISTER_SET_MemberP(call_regs, reg) ) {
            return TRUE;
          }
        }
      } else if (OP_code(op)==TOP_spadjust) {
        // This is probably too conservative
        return FALSE;
      }

      if ( OP_Defs_Reg(op, rc, reg) )
        return TRUE;
    }
  }

  return !REG_LIVE_Outof_BB(rc, reg, bb);
}

/*
   REF_VALUE:   OP refs a reg definitely
   REF_KILL:    OP kills a reg definitely.
   REF_UNKNOWN: OP does not kill or ref a reg
 */
typedef enum {REF_VALUE, REF_KILL, REF_UNKNOWN} REG_REF_KIND;

static REG_REF_KIND 
OP_Reg_Ref_Info(ISA_REGCLASS rc, REGISTER reg, BB *bb, OP *op)
{
  if ( OP_Refs_Reg(op, rc, reg) ) {
    return REF_VALUE;
  } 

  if (OP_call(op)) {
    ANNOTATION  *ant = ANNOT_Get(BB_annotations(bb), ANNOT_CALLINFO);
    CALLINFO *my_ant = ANNOT_callinfo(ant);
    REGISTER_SET call_regs = REGISTER_CLASS_caller_saves(rc);
    if (my_ant == 0)
    { // Since no actual argument info, conservatively assume all caller_save
      // registers are used by the call.
      if ( REGISTER_SET_MemberP(call_regs, reg) )
        return REF_VALUE;
    } else {
      TN_LIST *tnl;
      for (tnl = CALLINFO_actual_params(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
        TN *tmp_tn = TN_LIST_first(tnl);
        if (TN_register_class(tmp_tn) == rc && TN_register(tmp_tn) == reg) {
          return REF_VALUE;
        }
      }

      // Now, <rc, reg> isn't an actual argument. If it is a caller-save reg,
      // it should be dead (unless cg does an IPA register allocation, which
      // is not true at this time).
      if ( REGISTER_SET_MemberP(call_regs, reg) ) {
         return REF_KILL;
      }
    }
  } else if (BB_exit(bb) && OP_next(op) == NULL) {
    // Return value is ref'ed
    REGISTER_SET exit_regs;
    ANNOTATION *ant = ANNOT_Get(BB_annotations(bb), ANNOT_EXITINFO);
    EXITINFO *my_ant = ANNOT_exitinfo(ant);

    if (my_ant == 0)
    {
      // No info, assume it is ref'ed.
      return REF_VALUE;
    } else {
      exit_regs = REGISTER_SET_EMPTY_SET;
      TN_LIST *tnl;
      for (tnl = EXITINFO_ret_value(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
        TN *tmp_tn = TN_LIST_first(tnl);
        if (TN_register_class(tmp_tn) == rc && TN_register(tmp_tn) == reg) {
          return REF_VALUE;
        }
      }

      // It isn't a return value, so it must be dead
      return REF_KILL;
    }
  } else if (OP_code(op) == TOP_asm) {
    ASM_OP_ANNOT* asm_info = (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op);
    REGISTER_SET asm_regs = ASM_OP_clobber_set(asm_info)[rc];
    if (REGISTER_SET_MemberP(asm_regs, reg)) {
      return REF_KILL;
    }
  }

  if ( OP_Defs_Reg(op, rc, reg) ) {
    return REF_KILL;
  }

  return REF_UNKNOWN;
}

// 
static bool Get_Reg_Value_Next_Ref (
  BB *bb, 
  OP *op,          // If op isn't NULL, start checking from this op
  ISA_REGCLASS rc, 
  REGISTER reg, 
  bool *vbbs,      // If vbbs[i] is true, BB whose id is i has been visited
  BB   *begin_bb,  
  OP   *begin_op
)
{
  OP *cur_op;
  OP *end_op = NULL;

  /* 
     Each BB's vbbs[] will be set to true if the BB is visited
     from entry to BB.  

     The begin_bb is a special BB that may be visited twice. Backward
     copy will replace all <rc,reg> in begin_bb unless there is a def
     in begin_bb that stops replacing.

     When begin_bb is visited from entry to this BB, it will do
         1) don't search at all if there is no def b/w the first_op to
            begin_op;
         2) search to the next OP of a defining OP if there is a defining
            op b/w the first_op and begin_op in begin_bb.
   */
  if (bb != begin_bb) {
    vbbs[BB_id(bb)] = true;
  } else {
    // Special handling for begin_bb
    if (op == BB_first_op(bb)) {
      // visited from entry to bb
      vbbs[BB_id(bb)] = true;
      end_op = BB_first_op(bb); // stop searching, assume no definition.
      for ( cur_op = op; cur_op != begin_op; cur_op = OP_next(cur_op)) {
        if (OP_Defs_Reg(cur_op, rc, reg)){
          end_op = OP_next(cur_op);
          break;
        }
      }
    }
  }

  if (op) {
    for ( cur_op = op;
          cur_op != end_op;
          cur_op = OP_next(cur_op))
    {
      REG_REF_KIND refkind = OP_Reg_Ref_Info (rc, reg, bb, cur_op);
      if (refkind == REF_KILL) 
        return false;
      else if (refkind == REF_VALUE)
        return true;
    }
  }

  // Search stops at begin_op
  if ((bb == begin_bb) && (op == BB_first_op(bb))) {
    return false;
  }

  BBLIST *edge;
  FOR_ALL_BB_SUCCS(bb, edge) {
    BB *succ = BBLIST_item(edge);
    if (vbbs[BB_id(succ)])
      continue;
    if (!REG_LIVE_Into_BB(rc, reg, succ)) {
      continue;
    }
    if (Get_Reg_Value_Next_Ref(succ, BB_first_op(succ), rc, reg, vbbs,
                               begin_bb, begin_op)) {
      return true;
    }
  }

  return false;
}


static BOOL
Bad_Forward_OP (OP *op)
{
  if (OP_code(op) == TOP_spadjust)
    return TRUE;
  return FALSE;
}

static BOOL
Bad_Backward_OP (OP *op)
{
  if (OP_code(op) == TOP_spadjust ||
      OP_code(op) == TOP_entry)
    return TRUE;
  return FALSE;
}


/* Propagate copies forward through BB. 'tn_props' holds
   copy-propagation information for the TNs on entry to 'bb'. */
static void
Propagate_BB_Forward (BB *bb, TN_PROP *tn_props, BOOL trace)
{
  if (trace)
  {
    fprintf(TFile, "%s Forward copy propagation in BB:%d\n",
      "<ebo_cp>",  BB_id(bb));
    tn_props->Print(4);
  }

  /* Used to mark replacement of same_result result TNs. */
  const UINT max_results = TI_ISA_Result_Max();
  TN *same_result_tn[max_results];
  
  for (OP *next, *op = BB_first_op(bb); op; op = next)
  {
    next = OP_next(op);

    if (trace)
    {
      fprintf(TFile, "%s ", "<ebo_cp>");
      Print_OP_No_SrcLine(op);
    }
    
    const BOOL same_res = OP_same_res(op);
    const INT copy_opnd_idx = EBO_Prop_Copy_Operand(op);
    ASM_OP_ANNOT* asm_info = (OP_code(op) == TOP_asm) ?
      (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op) : NULL;

    memset(same_result_tn, 0, sizeof(TN *) * max_results);
    
    /* If 'op' is the last OP in an exit block, then emit all
       copies that target a TN assigned to a register. */
    if (!next && BB_exit(bb))
      tn_props->Stop_All_Assigned_Copy_Props(bb, op, EBO_CP_BEFORE);
      
    /* If 'op' is a call, then emit any copies being propagated that
       target a register potentially used by the call. */
    if (OP_call(op))
      tn_props->Stop_All_Assigned_Copy_Props(bb, op, EBO_CP_BEFORE);

    if (Bad_Forward_OP(op)) {
      for (INT i = 0; i < OP_opnds(op); i++) {
	TN *const tn = OP_opnd(op, i);
	if (TN_is_register(tn) && tn_props->Has_Replacement(tn))
	  tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
      }
      for (INT i = 0; i < OP_results(op); i++) {
	TN *const tn = OP_result(op, i);
	if (TN_is_register(tn)) {
	  if (tn_props->Has_Replacement(tn))
	    tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
	  tn_props->Stop_Def_Copy_Props(tn, NULL, bb, op, EBO_CP_BEFORE);
	}
      }
      continue;
    }

    /* Stop some copy propagations for these cases: There are some
       cases where we don't replace 'tn' even-though it is used and
       has a replacement.

       1. If 'tn' is also defined in 'op', and 'op' requires that
       result TN to be in the same register as the operand TN, and
       the replacement TN is not dead after 'op'. This prevents us
       from propagating this case:

       mov b <- a
       cmov b <- b, x, y
       a, b live

       can't propagate since the cmov would then define "a". But we
       can handle the case where "a" is not live after the cmov. In
       the case where we can propagate the copy, we must make sure to
       stop all other copies being propagated where "a" is the
       replacement TN. Consider a tie instruction that has two
       same-results:

       < case 1.1 >
       mov b <- a
       mov c <- a
       tie b, c <- b, c, x, y

       we can't replace both b and c with a, since then a would be
       defined twice (with unpredictable outcome).

       < case 1.2 >
       Don't do propagation for the following:
        mov b <- a
        tie a, b <-- b, x, y  // b is both input and output (same_res)

       2. If the replacement TN is defined in 'op', and 'tn' is not
       last-used in 'op'. This condition avoids this case:

       mov b <- a
       ...
       op  a <- b op c
       a, b live
       
       becoming this:
       
       mov b <- a
       op  a <- a op c
       a, b live
       
       which has the problem that a and b now interfere where
       originally they didn't. This will interfere with backwards
       copy-propagation ("mov b <- a" won't be eliminated since both
       'a' and 'b' are live) and probably result in worse GRA. */
       
    for (INT i = 0; i < OP_opnds(op); i++)
    {
      TN *const tn = OP_opnd(op, i);

      if (TN_is_register(tn) && tn_props->Has_Replacement(tn))
      {
	TN *rep = tn_props->Replacement(tn);
	BOOL rw_tn = FALSE;
	INT same_res_idx = Op_Sameres_Result(op, i);
        BOOL mv_copy_down = FALSE;

	for (INT res = 0; res < OP_results(op); res++)
	{
	  if ( /* case 1 */
	    (same_res && 
	     (res == same_res_idx) &&
	     (!TN_Dead(rep, bb, OP_next(op))))
	    || /* case 2 */
	    ((rep == OP_result(op, res)) &&
	     !TN_Dead(tn, bb, OP_next(op)))
	    || // case 1.1.  
	    (same_res && res != same_res_idx && same_res_idx != -1 &&
	     (Op_Sameres_Operand(op, res) != -1) &&
	     tn_props->Has_Replacement(OP_result(op, res)) &&
	     (rep == tn_props->Replacement(OP_result(op, res))))
	    || // case 1.2
            ((rep == OP_result(op, res)) && (same_res_idx != -1) &&
             (res != same_res_idx))
	    ||
	    (asm_info && 
	     ASM_OP_result_clobber(asm_info)[res] &&
	     (rep == OP_result(op, res))))
	  {
	    rw_tn = TRUE;
	    break;
	  } else if (same_res && (res == same_res_idx) &&
                     TN_Dead(rep, bb, OP_next(op)) && !TN_Dead(tn, bb, OP_next(op))) {
            /* 
               case 1: when b is alive after cmov

               Need to insert copy 'tn = rep' after the 'op'.  

               Delete the original copy OP if it is in the current 'bb'; otherwise,
               leave it for the core EBO to handle it.
            */
            mv_copy_down = TRUE;
          }
	}

	if (rw_tn)
	{
	  tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
	}
	else if (same_res_idx >= 0 && tn_props->Replacement_Omega(tn)>0)
	{
	  tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
	}
	else if (same_res_idx >= 0)
	{
          if (mv_copy_down) {
            OP *orig_copy_op = tn_props->Copy(tn);
            OP *copy_op = Dup_OP(orig_copy_op);
            if (OP_bb(orig_copy_op) == bb) {
              BB_Remove_Op(bb, orig_copy_op);
            }
            BB_Insert_Op_After(bb, op, copy_op);
          }

	  /* Mark that the 'same_res_idx' result must be
             replaced. Also stop propagation of all other copies that
             have the same replacement, for reasons described in case
             1 above. */
	  same_result_tn[same_res_idx] = tn;
	  tn_props->Stop_Def_Copy_Props(tn, tn, bb, op, EBO_CP_BEFORE);
	}
      }
    }

    /* Replace each operand TN with it's replacement TN. */
    for (INT i = 0; i < OP_opnds(op); i++)
    {
      TN *const tn = OP_opnd(op, i);

      if (TN_is_register(tn) && tn_props->Has_Replacement(tn))
      {
	  tn_props->Replace_TN_Operand(tn, op, i);
	  TN *new_tn = OP_opnd(op, i);
	  if (TN_is_gra_homeable(new_tn) && 
	      (!TN_is_gra_homeable(tn) || 
	       (TN_home(tn) != TN_home(new_tn)))) {
	    Reset_TN_is_gra_homeable(new_tn);
	    Set_TN_home(new_tn, NULL);
	  }
      }
    }

    /* Process result TNs. If 'op' is a self copy, then we don't do
       anything with its results. A self copy can occur when:

       mov b <- a
       ...
       mov a <- b

       becomes

       ...
       mov a <- a    // propagating mov b <- a

       In this case we want to continue propagating the existing move
       (i.e. mov b <- a). 'op' (i.e. mov a <- a) will be deleted
       below.
    */
    if ((copy_opnd_idx == -1) ||
	(OP_result(op, 0) != OP_opnd(op, copy_opnd_idx)))
    {
      for (INT i = 0; i < OP_results(op); i++)
      {
	TN *tn = OP_result(op, i);
	if (TN_is_register(tn))
	{
	  /* If result 'i' has a same_result replacement, then apply
             that replacement and continue propagating the copy. */
	  if (same_result_tn[i] && tn_props->Has_Replacement(same_result_tn[i]))
	  {
	    tn_props->Replace_TN_Result(same_result_tn[i], op, i);

	    /* Now that the TN is defined in a cmov, it is no longer
               rematerializable, if it happened to be before. */
	    TN *new_tn = OP_result(op, i);
	    if (TN_is_rematerializable(new_tn)) {
	      Reset_TN_is_rematerializable(new_tn);
	      Set_TN_home(new_tn, NULL);
	    }
	    if (TN_is_gra_homeable(new_tn) && 
		(!TN_is_gra_homeable(tn) || 
		 (TN_home(tn) != TN_home(new_tn)))) {
	      Reset_TN_is_gra_homeable(new_tn);
	      Set_TN_home(new_tn, NULL);
	    }
	    tn = new_tn;
	  }
	  /* Any pending copy to the TN can be dropped, since we are
	     redefining the TN. */
 	  else if (tn_props->Has_Replacement(tn))
	  {
	    tn_props->Drop_Copy(tn);
	  }

	  /* Any pending copy using the TN as the source must be
	     emitted now, since 'op' is changing the value of the
	     TN. We don't want to stop the copy that caused the result
	     TN to be replaced above because of a same_res
	     result/operand pair. */
	  tn_props->Stop_Def_Copy_Props(tn, same_result_tn[i], bb, op, EBO_CP_BEFORE);
	}
      }
    }

    /* If 'inst' is a copy, show that the destination is represented
       subsequently by the source. */
    if (copy_opnd_idx >= 0)
    {
      TN *res = OP_result(op, 0);
      TN *opnd = OP_opnd(op, copy_opnd_idx);
      UINT8 omega = 0;
      if (Is_CG_LOOP_Op(op))
	omega = OP_omega(op, copy_opnd_idx);

      BB* pred = BB_Unique_Predecessor(bb);

      if (TN_is_asm_reg(res) || TN_is_asm_reg(opnd)) {
	      ; // Don't propogate asm registers
      }
      /* If 'op' is a self copy, simply delete it. */
      /* do not do this if it is a dedicated reg copy before reg allocation
       * since the live info will be changed for hard-wired reg usage such as
       * return and calls
       */
      else if (res == opnd && omega ==0 &&
	       (EBO_in_peep || !TN_is_dedicated(res)))
      {
	if (trace)
	{
	  fprintf(TFile, "%s Removing self copy ", "<ebo_cp>");
	  Print_OP_No_SrcLine(op);
	}

        BB_Remove_Op(bb, op);
      } else if (OP_glue(op)) {
	// do not propagate glue copy since it mess up GRA's assumption of
	// region TN references at region entry/exit blocks
      }
      /* In an entry BB or BB following a call, don't propagate a copy that sources a
         dedicated TN. GRA handles these specially to improve
         preferencing and we don't want to extend the liverange of the
         dedicated TN and mess that up. */
      else if ((!BB_entry(bb) && !(pred && BB_call(pred))) || !TN_is_dedicated(opnd) ||
			      !BB_succs(bb))
      {
	tn_props->Set(res, opnd, omega, op, CP_FLAG_NONE);
	//BB_Remove_Op(bb, op); Leave the op when doing forward prop

	if (trace)
	{
	  fprintf(TFile, "%s Propagating copy ", "<ebo_cp>");
	  Print_OP_No_SrcLine(op);
	}
      }
    }

  }

  /* Make sure we don't have any duplicate replacements being
     propagated out of the block, like:

     mov b <- a
     mov c <- a

     since during backwards propagation we can't correctly handle
     these. It's much easier to stop all but one of the copies now,
     then to try to take care of it during backward
     propagation. Here, we stop duplicate copies before the xfer
     instruction to avoid leaving copies after a xfer that backwards
     propagation will not be able to move back above the xfer. If
     the block doesn't end in an xfer, we call the same function
     below, after processing the last op. */
  tn_props->Eliminate_Forward_Duplicates(bb, NULL);

  if (trace)
  {
    fprintf(TFile, "%s reached end of BB:%d\n", "<ebo_cp>", BB_id(bb));
    tn_props->Print(4);
  }

  /* At the end of the BB, clear propagation information for any TNs
     that are not live out of the BB. */
  tn_props->Check();

  /* If we are not propagating across blocks, then just emit any
     pending copies now. */
  if (EBO_Copy_Prop_Intra)
    tn_props->Stop_All_Copy_Props(bb, NULL, EBO_CP_BEFORE);
}


/* Propagate copies backwards through BB. 'tn_props' holds
   copy-propagation information for the TNs on exit to 'bb'. */
static void
Propagate_BB_Backward (BB *bb, TN_PROP *tn_props, BOOL trace)
{
  if (trace)
  {
    fprintf(TFile, "%s Backward copy propagation in BB:%d\n", 
      "<ebo_cp>", BB_id(bb));
    tn_props->Print(4);
  }

  for (OP *prev, *op = BB_last_op(bb); op; op = prev)
  {
    prev = OP_prev(op);

    if (trace)
    {
      fprintf(TFile, "%s ", "<ebo_cp>");
      Print_OP_No_SrcLine(op);
    }
    
    const BOOL same_res = OP_same_res(op);
    const INT copy_opnd_idx = EBO_Prop_Copy_Operand(op);
    ASM_OP_ANNOT* asm_info = (OP_code(op) == TOP_asm) ?
      (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op) : NULL;


    /* If 'op' is the first OP in an entry block, then emit all
       copies. We don't want copies emitted before the entry
       instruction(s). */
    if (!prev && BB_entry(bb))
      tn_props->Stop_All_Copy_Props(bb, op, EBO_CP_AFTER);
      
    /* If 'op' is a call, then emit any copies being propagated that
       target a register potentially written/clobbered by the
       call. */
    if (OP_call(op))
      tn_props->Stop_All_Assigned_Copy_Props(bb, op, EBO_CP_AFTER);
    
    if (Bad_Backward_OP(op)) {
      for (INT i = 0; i < OP_results(op); i++) {
	TN *const tn = OP_result(op, i);
	if (TN_is_register(tn)) {
	  if (tn_props->Has_Replacement(tn))
	    tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_AFTER);
	  tn_props->Stop_Def_Copy_Props(tn, NULL, bb, op, EBO_CP_AFTER);
	}
      }
      for (INT i = 0; i < OP_opnds(op); i++) {
	TN *const tn = OP_opnd(op, i);

	if (TN_is_register(tn))
	  tn_props->Stop_Def_Copy_Props(tn, NULL, bb, op, EBO_CP_AFTER);
      }
      continue;
    }

    /* Process result TNs. */
    for (INT i = 0; i < OP_results(op); i++)
    {
      TN *tn = OP_result(op, i);
      TN *orig_tn = NULL;
      if (TN_is_register(tn))
      {
	if (tn_props->Has_Replacement(tn))
	{
	  /* If we can't eliminate the copy, then stop it's
             propagation just after 'op'. */
	  if (tn_props->Flags(tn) & CP_FLAG_NO_BACKWARD_ELIM)
	  {
	    tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_AFTER);
	  }
	  else
	  {
	    /* After rewriting, any pending copy from the TN can be
	       dropped, since we are redefining the TN. We don't drop
	       the copy if the define is a same-result, we just let
	       the copy continue so it can rewrite the operands and
	       then continue propagating. */
	    BOOL keep = FALSE;
	    BOOL can_replace = TRUE;
	    if (same_res && (Op_Sameres_Operand(op, i) != -1))
	      keep = TRUE;
	    if ((same_res && (Op_Sameres_Operand(op, i) != -1)) ||
		(asm_info && ASM_OP_result_clobber(asm_info)[i]))
	    {
	      for (INT op_idx = 0; op_idx < OP_opnds(op); op_idx++)
		if (tn_props->Replacement(tn) == OP_opnd(op, op_idx))
		{
		  can_replace = FALSE;
		  break;
		}
	    }

            /*
               Check if the following is true, if so, stop copying
                    a, b <-- ....
                    a <- b
             */
            for (INT res=0; res < OP_results(op); res++) {
              if ( (res != i) && 
                   tn_registers_identical(tn_props->Replacement(tn), OP_result(op, res)) ) {
                can_replace = FALSE;
                break;
              }
            }

	    if (can_replace) {
	      tn_props->Replace_TN_Result(tn, op, i);
	      
	      TN *new_tn = OP_result(op, i);
	      if (TN_is_gra_homeable(new_tn) && 
		  (!TN_is_gra_homeable(tn) || 
		   (TN_home(tn) != TN_home(new_tn)))) {
		Reset_TN_is_gra_homeable(new_tn);
		Set_TN_home(new_tn, NULL);
	      }
	      
	      if (!keep)
		tn_props->Drop_Copy(tn);
	      orig_tn = tn;
	      tn = new_tn;
	    } else {
	      tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_AFTER);
	    }
	  }
	}
	
	/* If there are pending copies defining 'tn', then the define
           of 'tn' in 'op' is dead. But we must emit the copy now
           since 'op' may not be able to be eliminated because of
           other defines (if it is eliminated a later pass of copy
           propagate will continue moving the copy back). */
	tn_props->Stop_Def_Copy_Props(tn, orig_tn, bb, op, EBO_CP_AFTER);
      }
    }

    /* Stop any copies that define a TN used by 'op', since moving the
       copy above 'op' would change the used value. */
    for (INT i = 0; i < OP_opnds(op); i++)
    {
      TN *const tn = OP_opnd(op, i);
      tn_props->Stop_Def_Copy_Props(tn, NULL, bb, op, EBO_CP_AFTER);
    }

    /* Replace each operand TN with it's replacement TN. */
    for (INT i = 0; i < OP_opnds(op); i++)
    {
      TN *const tn = OP_opnd(op, i);

      if (TN_is_register(tn) && tn_props->Has_Replacement(tn))
      {
	/* If 'op' is a copy, then stop propagating the copy that
           sources 'tn'. Otherwise we will get 2 different TNs being
           replaced by that same TN, something we don't allow during
           backwards prop. */
	if ((copy_opnd_idx >= 0) && (copy_opnd_idx == i))
	{
	  tn_props->Stop_Copy_Prop(tn, bb, op, EBO_CP_AFTER);
	}
	else
	{
	  tn_props->Replace_TN_Operand(tn, op, i);
	  TN *new_tn = OP_opnd(op, i);
	  if (TN_is_gra_homeable(new_tn) && 
	      (!TN_is_gra_homeable(tn) || 
	       (TN_home(tn) != TN_home(new_tn)))) {
	    Reset_TN_is_gra_homeable(new_tn);
	    Set_TN_home(new_tn, NULL);
	  }
	}
      }
    }

    /* If 'inst' is a copy, show that the source is represented
       subsequently by the destination. Don't propagate copies where:

       1. the destination is dead. Forward propagation can leave these
       around, and propagating them backwards can result in incorrect
       code.

       2. In an exit BB, don't propagate a copy that targets a
       dedicated TN. GRA handles these specially to improve
       preferencing and we don't want to extend the liverange of the
       dedicated TN and mess that up. */
    if (copy_opnd_idx >= 0)
    {
      TN *res = OP_result(op, 0);
      TN *opnd = OP_opnd(op, copy_opnd_idx);
      UINT8 omega = 0;
      if (Is_CG_LOOP_Op(op))
	omega = OP_omega(op, copy_opnd_idx);

      /* If 'op' is a self copy, simply delete it. */
      if (TN_is_asm_reg(res) || TN_is_asm_reg(opnd)) {
	      ; // Don't propogate asm registers
      }
      /* do not do this if it is a dedicated reg copy before reg allocation
       * since the live info will be changed for hard-wired reg usage such as
       * return and calls
       */
      else if (res == opnd && omega == 0 &&
	       (EBO_in_peep || !TN_is_dedicated(res)))
      {
	if (trace)
	{
	  fprintf(TFile, "%s Removing self copy ", "<ebo_cp>");
	  Print_OP_No_SrcLine(op);
	}

        BB_Remove_Op(bb, op);
      } else if (OP_glue(op)) {
	// do not propagate glue copy since it mess up GRA's assumption of
	// region TN references at region entry/exit blocks
      }
      else if (!TN_Dead(res, bb, OP_next(op)) &&
	       ((!BB_exit(bb) && !BB_call(bb)) || !TN_is_dedicated(res)))
      {
	UINT32 flags = CP_FLAG_NONE;

	/* If the source is dedicated or live, then mark that we can't
	   eliminate the copy during backwards propagation, since that
	   might mistakenly eliminate the copy, and thereby eliminate
	   the definition of the source TN.  */
	if ((TN_register(opnd) != REGISTER_UNDEFINED) ||
	    !TN_Dead(opnd, bb, OP_next(op)))
	  flags |= CP_FLAG_NO_BACKWARD_ELIM;

	// copies in backward prop are moved with propagation
	flags |= CP_FLAG_PLACEMENT_NEEDED;
	
	tn_props->Set(opnd, res, 0, op, flags);
	BB_Remove_Op(bb, op);

	if (trace)
	{
	  fprintf(TFile, "%s Propagating copy ", "<ebo_cp>");
	  Print_OP_No_SrcLine(op);
	}
      }
    }
  }

  if (trace)
  {
    fprintf(TFile, "%s reached start of BB:%d\n",
      "<ebo_cp>",  BB_id(bb));
    tn_props->Print(4);
  }

  if (EBO_Copy_Prop_Intra) {
    tn_props->Stop_All_Copy_Props(bb, NULL, EBO_CP_AFTER);
  } 
}


/* Return EBO_PROP_SAME if 'tn' is replaceable with the same TN in
   every successor when 'tn' is live into the successor. Return
   EBO_PROP_NONE if there is no copy information for any successor
   'tn' is live into. */
static EBO_PROP_TYPE
Same_Copy_Prop (BB *bb, TN *tn, TN_PROP **succ_tn_props, UINT num_succs)
{
  /* Find a successor that has copy information for 'tn'. */
  INT first_succ_cnt = -1, succ_cnt = 0;
  for (UINT i = 0; i < num_succs; i++)
  {
    if (succ_tn_props[i]->Has_Replacement(tn))
    {
      first_succ_cnt = i;
      break;
    }

    succ_cnt++;
  }
    
  /* If 'tn' does not have copy information in any successor, then
     return EBO_PROP_NONE to indicate that no copy information for
     'tn' needs to be propagated. */
  if (first_succ_cnt == -1)
    return EBO_PROP_NONE;

  /* We can propagate 'first_succ_cnt's copy if for each other
     successors:

     1. The target and source of the copy is not live into the
     successor. In this case propagating it into the predecessor does
     not effect the other predecessor (there are some performance
     impliciations here if the copy is not ultimately eliminted).

     2, The successor has the same copy information.
     
     If these tests pass, return EBO_PROP_SAME + index to indicate the
     successor from which to get copy information. */
  TN *rep = succ_tn_props[first_succ_cnt]->Replacement(tn);
  succ_cnt = 0;

  for (BBLIST *edge = BB_succs(bb); edge; edge = BBLIST_next(edge))
  {
    if (succ_cnt != first_succ_cnt)
    {
      BB *succ = BBLIST_item(edge);

      if ((bb_liveness->Is_Live_In(succ, rep) ||
	   bb_liveness->Is_Live_In(succ, tn)) &&
	  (!succ_tn_props[succ_cnt]->Has_Replacement(tn) ||
	   (succ_tn_props[succ_cnt]->Replacement(tn) != rep)))
	  return EBO_PROP_DIFF;

      if ((BB_Unique_Predecessor(succ) != bb) &&
	  succ_tn_props[succ_cnt]->Has_Replacement(tn) &&
	  (succ_tn_props[succ_cnt]->Replacement(tn) != rep))
	return EBO_PROP_DIFF;

      /* Combine flags from the successor. Be conservative... */
      if (succ_tn_props[succ_cnt]->Has_Replacement(tn))
	succ_tn_props[first_succ_cnt]->Merge_Flags(tn, succ_tn_props[succ_cnt]->Flags(tn)); 
    }

    succ_cnt++;
  }

  return (EBO_PROP_TYPE)(EBO_PROP_SAME + first_succ_cnt);
}


/* For each successor block to 'bb' where 'tn' has copy information,
   emit the copy. */
static void
Emit_Non_Prop_Copies (BB *bb, TN *tn, TN *except_tn, TN_PROP **succ_tn_props, UINT num_succs, BOOL trace)
{
  UINT succ_cnt = 0;

  for (BBLIST *edge = BB_succs(bb); edge; edge = BBLIST_next(edge))
  {
    Is_True(succ_cnt < num_succs, ("unexpected successor"));
    
    BB *succ = BBLIST_item(edge);
    if (succ_tn_props[succ_cnt]->Has_Replacement(tn) && 
	((except_tn == NULL) || 
	 (succ_tn_props[succ_cnt]->Replacement(tn) != except_tn)))
    {
      if (trace) {
	fprintf(TFile, "%s Restoring non-prop copy in BB:%d\n",
          "<ebo_cp>", BB_id(succ));
      }

      succ_tn_props[succ_cnt]->Stop_Copy_Prop(tn, succ, NULL, EBO_CP_AFTER);
    }

    succ_cnt++;
  }
}


static BOOL
Copy_Used_in_Xfer_OP(BB *bb, TN *tn, TN *rtn)
{
  OP *xfer_op = BB_branch_op(bb);
  if (xfer_op) {
    for (INT i=0; i<OP_opnds(xfer_op); i++) {
      if (OP_opnd(xfer_op,i) == rtn)
	return TRUE;
    }
  }
  return FALSE;
}

/*
   Return TRUE if bb is a single-bb loop and its successor has
   bb as its unique predecessor and its predecessor has bb as its
   unique successor;  otherwise, return FALSE.
              P
              |<----
              |    |
              bb --^
              |
              S
*/
static BOOL Is_SINGLE_BB_LOOP (BB *bb)
{
  if (BB_preds_len(bb) == 2 && BB_succs_len(bb) == 2) {
    BB *p0 = BB_First_Pred(bb);
    BB *p1 = BB_Other_Predecessor(bb, p0);
    if (bb == p0) {
      if (BB_Unique_Successor(p1) != bb) {
        return FALSE;
      }
    } else if (bb == p1) {
      if (BB_Unique_Successor(p0) != bb) {
        return FALSE;
      }
    } else {
      return FALSE;
    }
     
    BB *s0 = BB_First_Succ(bb);
    BB *s1 = BB_Other_Successor(bb, s0);
    if ( bb == s0 ) {
      if (BB_Unique_Predecessor(s1) != bb) {
        return FALSE;
      }
    } else if ( bb == s1 ) {
      if (BB_Unique_Predecessor(s0) != bb) {
        return FALSE;
      }
    } else {
      return FALSE;
    }

    return TRUE;
  }
  return FALSE;
}


/* Propagate copies forward and backward across the extended BB
   starting at 'bb'. */
static void
Propagate_EBO (BB *cur_bb, TN_PROP *tn_props, MEM_POOL *pool, BOOL trace)
{
  BBLIST *edge;
  BB     *bb = cur_bb;

  // First we forward and backward propagate within a block to avoid
  // imprecise global liveness affecting propagation results

  /* Propagate forward through 'bb'. */
  Propagate_BB_Forward(bb, tn_props, trace);

  tn_props->Stop_Copy_Non_Omega_Copy();

  tn_props->Invert();

  /* Propagate backwards through 'bb'. */
  Propagate_BB_Backward(bb, tn_props, trace);
  if (!EBO_Copy_Prop_Intra) {
    // Need to stop propagation always. 
    //   If EBO_Copy_Prop_Intra is true, no need to do so 
    //   because it has been done within Propagate_BB_Backward().
    tn_props->Stop_All_Copy_Props(bb, NULL, EBO_CP_AFTER);
  }

  tn_props->Invert();

  // then a final forward propagation which will continue on successors

  /* Propagate forward through 'bb'. */
  Propagate_BB_Forward(bb, tn_props, trace);

  /* Copy over a single-BB loop */
  if (EBO_Prop_BB_Loop)
  {
    BB *loop_bb = BB_Unique_Successor(bb);
    if (loop_bb != NULL && !BB_call(loop_bb) &&
        !BB_rotating_kernel(loop_bb) &&
        Is_SINGLE_BB_LOOP(loop_bb)) {
      tn_props->Prop_Copy_Forward_SINGLE_BB_LOOP (bb, loop_bb);
      bb = BB_Other_Successor (loop_bb, loop_bb);
      Propagate_BB_Forward(bb, tn_props, trace);
    }
  }

  /* For each successor:

     1. If the successor has no other predecessors, copy 'tn_props'
     and continue propagating.

     2. If the successor has other predecessors, add it to 'ebo_bbs'
     so that it will be visited as the head of a new extended
     block. */
  const UINT num_succs = BB_succs_len(bb);
  UINT succ_cnt = 0;
  
  if (num_succs>0)
    tn_props->Adjust_Forward_Exit(bb);

  FOR_ALL_BB_SUCCS(bb, edge)
  {
    Is_True(succ_cnt < num_succs, ("unexpected successor"));
    
    BB *succ = BBLIST_item(edge);

    if (BB_Unique_Predecessor(succ) == bb)
    {
      if (BB_visited(succ)) {
	if (trace) {
	  fprintf(TFile, "%s No copy propagation to out of region succ BB:%d\n",
            "<ebo_cp>", BB_id(succ));
        }
      } else {
	if (trace) {
	  fprintf(TFile, "%s Continuing copy propagation to succ BB:%d\n",
            "<ebo_cp>", BB_id(succ));
        }

	TN_PROP *succ_tn_props = CXX_NEW(TN_PROP(*tn_props), pool);
	succ_tn_props->Adjust_Forward_Entry(succ, true);
	Propagate_EBO(succ, succ_tn_props, pool, trace);
      }
    }
    else
    {
      if (trace)
	fprintf(TFile, "%s No copy propagation to non-unique succ BB:%d\n",
          "<ebo_cp>", BB_id(succ));

      if (!BB_visited(succ)) {
        ebo_bbs = BB_LIST_Push(succ, ebo_bbs, pool);
      }
    }

    succ_cnt++;
  }

  Is_True(succ_cnt == num_succs, ("unexpected number of successors"));

  return;
}

static void
Propagate_Backward_EBO (BB *cur_bb, TN_PROP *tn_props, MEM_POOL *pool, BOOL trace)
{
  BBLIST *edge;
  BB     *bb = cur_bb;

  /* Propagate backwards through 'bb'. */
  Propagate_BB_Backward(bb, tn_props, trace);

  /* Copy over a single-BB loop */
  if (EBO_Prop_BB_Loop)
  {
    // For backward copy prop, make sure not to propagate into EH.
    BB *loop_bb = BB_Unique_Predecessor(bb);
    if ( loop_bb != NULL && !BB_visited(loop_bb) &&
         !BB_call(loop_bb) && !BB_rotating_kernel(loop_bb) &&
         Is_SINGLE_BB_LOOP(loop_bb) ) {
      tn_props->Prop_Copy_Backward_SINGLE_BB_LOOP (bb, loop_bb);
      bb = BB_Other_Predecessor (loop_bb, loop_bb);
      Propagate_BB_Backward(bb, tn_props, trace);
    }
  }

#if 1
  tn_props->Stop_All_Copy_Props(bb, NULL, EBO_CP_AFTER, true);

  FOR_ALL_BB_PREDS(bb, edge)
  {
    BB *pred = BBLIST_item(edge);
    if (!BB_visited(pred)) {
      ebo_bbs = BB_LIST_Push(pred, ebo_bbs, pool);
    }
  }

#else
  /* 
     1. If the predecessor has no other successors and not a call BB,
         copy 'tn_props' and continue propagating.

     2. Otherwise, stop propagations and add its successors to 'ebo_bbs'
        so that it will be visited as the head of a new extended block.
  */
  const UINT num_preds = BB_preds_len(bb);
  UINT pred_cnt = 0;

  if (num_preds > 0)
    tn_props->Adjust_Backward_Exit(bb);

  bool can_prop = true;
  FOR_ALL_BB_PREDS(bb, edge)
  {
    // For backward copy prop, make sure not to propagate into EH.
    BB *pred = BBLIST_item(edge);
    if ( (BB_Unique_Successor(pred) != bb) ||
         BB_call(pred) || (BB_xfer_op(pred) == NULL) ) {
      can_prop = false;
      break;
    }
  }

  if (!can_prop) {
    tn_props->Stop_All_Copy_Props(bb, NULL, EBO_CP_AFTER, true);

    if (trace) {
      fprintf(TFile, "%s No copy propagation to non-unique pred of BB:%d\n",
        "<ebo_cp>", BB_id(bb));
    }

    FOR_ALL_BB_PREDS(bb, edge)
    {
      BB *pred = BBLIST_item(edge);
      if (!BB_visited(pred)) {
        ebo_bbs = BB_LIST_Push(pred, ebo_bbs, pool);
      }
    }
    return;
  }

  FOR_ALL_BB_PREDS(bb, edge)
  {
    BB *pred = BBLIST_item(edge);
    if (BB_Unique_Successor(pred) == bb)
    {
      if (!BB_visited(pred))
      {
        if (trace) {
          fprintf(TFile, "%s Continuing copy propagation to pred BB:%d\n",
            "<ebo_cp>", BB_id(pred));
        }

        TN_PROP *pred_tn_props = CXX_NEW(TN_PROP(*tn_props), pool);
        pred_tn_props->Adjust_Backward_Entry(pred);
        Propagate_Backward_EBO(pred, pred_tn_props, pool, trace);
      }
    }
    else
    {
      if (trace)
        fprintf(TFile, "%s No copy propagation to non-unique pred BB:%d\n",
          "<ebo_cp>", BB_id(pred));

      if (!BB_visited(pred)) {
        ebo_bbs = BB_LIST_Push(pred, ebo_bbs, pool);
      }
    }

    pred_cnt++;
  }

  Is_True(pred_cnt == num_preds, ("unexpected number of predecessors"));

#endif 

  return;
}



/* Make sure any xfer OPs are at the end of the block, and that we
   haven't left any moves after them. */
static void
Verify_Xfer_Ops (void)
{
  for (BB *bb = REGION_First_BB; bb; bb = BB_next(bb))
  {
    OP *last_op = BB_last_op(bb);
    if (last_op && (EBO_Prop_Copy_Operand(last_op) >= 0))
    {
      for (OP *op = OP_prev(last_op); op; op = OP_prev(op))
	FmtAssert(!OP_xfer(op), ("xfer OP not at end of BB:%d\n", BB_id(bb)));
    }
  }
}

  
/* Entry-point for copy propagation. Propagate across all blocks
   reachable from 'bb'. Return TRUE if liveness information has
   changed in a way that requires us to redo liveness analysis. */
BOOL
EBO_Copy_Propagation (BB *bb)
{
  Is_True(!EBO_in_peep, ("copy-prop assumes pre-GRA liveness, register-assignments, etc."));

  MEM_POOL *const pool = &MEM_local_nz_pool;
  MEM_POOL_Push(pool);
  BOOL trace = Get_Trace(TP_EBO, 0x020);

  ebo_bbs = BB_LIST_Push(bb, NULL, pool);
  visited_bbs = BB_SET_Create_Empty(PU_BB_Count + 1, pool);
  bb_liveness = CXX_NEW(BB_LIVENESS(pool, trace), pool);
			
  while (ebo_bbs)
  {
    BB *ebb = BB_LIST_first(ebo_bbs);
    ebo_bbs = BB_LIST_rest(ebo_bbs);

    if (!BB_SET_MemberP(visited_bbs, ebb))
    {

      RID *bbrid = BB_rid(ebb);
      // Don't process blocks that have been marked as visited by EBO
      // or blocks which are rotating kernels or blocks in regions
      // that have been swp scheduled
      if (BB_visited(ebb) || BB_rotating_kernel(ebb) ||
	  (bbrid && RID_level( bbrid ) >= RL_CGSCHED) ) {

	BBLIST *edge;
	FOR_ALL_BB_SUCCS(ebb, edge) {
	  BB *succ = BBLIST_item(edge);
	  ebo_bbs = BB_LIST_Push(succ, ebo_bbs, pool);
	}
      } else {
	if (trace) {
	  fprintf(TFile, "%s Copy propagation, start ebo at BB:%d\n",
             "<ebo_cp>", BB_id(ebb));
        }
	
	TN_PROP tn_props_loc(pool, trace);
	TN_PROP *tn_props = &tn_props_loc;
	
	Propagate_EBO(ebb, tn_props, pool, trace);
	tn_props->Stop_All_Copy_Props(ebb, NULL, EBO_CP_AFTER);
      }
    }

    visited_bbs = BB_SET_Union1D(visited_bbs, ebb, pool);
  }

  /* Make sure the xfer OP in a block is at the end, and not followed
     by any moves. */
  Verify_Xfer_Ops();
  
  /* We request a recalc of liveness information if any
     copy-propagation changed the liveness at a BB boundary. This is
     conservative because a liveness change during forward propagation
     could be undone during backwards propagation, and we don't detect
     that. */
  /* This doesn't work because we don't update global liveness during
     backward propagation. Much safter to just always recalc the
     liveness. */
  //  BOOL recalc_liveness = bb_liveness->Liveness_Changed();
  BOOL recalc_liveness = TRUE;

  if (EBO_Prop_Backward) 
  {
    GRA_LIVE_Recalc_Liveness(0);
  
    CXX_DELETE(bb_liveness, pool);
    ebo_bbs = NULL;
  
    /* Doing Backward Propagation accross BBs */
    visited_bbs = BB_SET_Create_Empty(PU_BB_Count + 1, pool);
    bb_liveness = CXX_NEW(BB_LIVENESS(pool, trace), pool);
    for (BB *tbb=bb; tbb != NULL; tbb = BB_next(tbb)) {
      if (BB_succs_len(tbb) == 0) {
        ebo_bbs = BB_LIST_Push(tbb, ebo_bbs, pool);
      }
    }
  
    while (ebo_bbs)
    {
      BB *ebb = BB_LIST_first(ebo_bbs);
      ebo_bbs = BB_LIST_rest(ebo_bbs);
  
      if (!BB_SET_MemberP(visited_bbs, ebb))
      {
        RID *bbrid = BB_rid(ebb);
        // Don't process blocks that have been marked as visited by EBO
        // or blocks which are rotating kernels or blocks in regions
        // that have been swp scheduled
        if (BB_visited(ebb) || BB_rotating_kernel(ebb) ||
            (bbrid && RID_level( bbrid ) >= RL_CGSCHED) )
        {
          BBLIST *edge;
          FOR_ALL_BB_PREDS(ebb, edge) {
            BB *pred = BBLIST_item(edge);
            ebo_bbs = BB_LIST_Push(pred, ebo_bbs, pool);
          }
        } else {
          if (trace) {
            fprintf(TFile, "%s Copy propagation (Backward), start ebo at BB:%d\n",
               "<ebo_cp>", BB_id(ebb));
          }
          TN_PROP tn_props_loc(pool, trace);
          TN_PROP *tn_props = &tn_props_loc;
  

          Propagate_Backward_EBO(ebb, tn_props, pool, trace);
          //tn_props->Stop_All_Copy_Props(ebb, NULL, EBO_CP_AFTER, true);
        }
      }
  
      visited_bbs = BB_SET_Union1D(visited_bbs, ebb, pool);
    }

    Verify_Xfer_Ops();
  }

  MEM_POOL_Pop(pool);
  return recalc_liveness;
}

/*
  Assume the swp'ed region has the following control flow:

               Prolog
                 |
                 |
                 V
         ----------------
         V              V<----
         |              |    |
         |             Body --
         |              |
         ----------------
                 |
                 |
                 V
               Epilog

  Is_SwpRegion_Prolog() checks if 'bb' is prolog of the swp region as
    described above, if so, return TRUE with pepilog and pbody being 
    set accordingly.

  Is_SwpRegion_Epilog() check if 'bb' is epilog of the swp region as
    described above, if so, return TRUE with pprolog and pbody being 
    set accordingly.
*/
static BOOL
Is_SwpRegion_Prolog(BB *bb, BB **pepilog, BB **pbody)
{
  BB *epilog, *body;

  *pepilog = *pbody = 0;

  if (BB_succs_len(bb) != 2) {
    return FALSE;
  } else {
    BBLIST *edge = BB_succs(bb);
    BB *body, *epolog;
    body = BBLIST_item(edge);
    if (BB_rotating_kernel(body)) {
      edge = BBLIST_next(edge);
      epilog = BBLIST_item(edge);
    } else {
      epilog = body;
      edge = BBLIST_next(edge);
      body = BBLIST_item(edge);
    }

    if ( !BB_rotating_kernel(body) ||
         BB_succs_len(body) != 2   ||
         BB_preds_len(epilog) != 2 ) {
      return FALSE;
    }

    // body
    edge = BB_succs(body);
    BB *tmp1 = BBLIST_item(edge);
    edge = BBLIST_next(edge);
    BB *tmp2 = BBLIST_item(edge);
    if (!((tmp1 == body && tmp2 == epilog) ||
          (tmp2 == body && tmp1 == epilog)) ) {
      return FALSE;
    }

    // epilog
    edge = BB_preds(epilog);
    tmp1 = BBLIST_item(edge);
    edge = BBLIST_next(edge);
    tmp2 = BBLIST_item(edge);
    if (!((tmp1 == body && tmp2 == bb) ||
          (tmp2 == body && tmp1 == bb)) ) {
      return FALSE;
    }

    *pepilog = epilog;
    *pbody   = body;

    // Check TOP_loop...
    OP *loop_op = BB_branch_op(bb);
    bool is_loop_op = loop_op && (OP_code(loop_op) == TOP_loop || 
                                  OP_code(loop_op) == TOP_loopgtz ||
                                  OP_code(loop_op) == TOP_loopnez);
    if (is_loop_op) {
      return TRUE;
    } else {
      return FALSE;
    }
  }
}

static BOOL
Is_SwpRegion_Epilog(BB *bb, BB **pprolog, BB **pbody)
{
  *pprolog = *pbody = NULL;
  if (BB_preds_len(bb) != 2) {
    return FALSE;
  } else {
    BBLIST *edge = BB_preds(bb);
    BB *prolog = BBLIST_item(edge);
    BB *epilog;
    if (BB_rotating_kernel(prolog)) {
      edge = BBLIST_next(edge);
      prolog = BBLIST_item(edge);
    }
    if (Is_SwpRegion_Prolog(prolog, &epilog, pbody)) {
      // sanity check
      if ( epilog == bb ) {
        *pprolog = prolog;
      }
      return TRUE;
    }
  }
  return FALSE;
} 

static BOOL BB_Defs_Reg(const BB *bb, ISA_REGCLASS rclass, REGISTER reg)
{
  OP *op;
  FOR_ALL_BB_OPs (bb, op) {
    if (OP_Defs_Reg(op, rclass, reg)) {
      return TRUE;
    }
  }
  return FALSE;
}

static BOOL BB_Refs_Reg(const BB *bb, ISA_REGCLASS rclass, REGISTER reg)
{
  OP *op;
  FOR_ALL_BB_OPs (bb, op) {
    if (OP_Refs_Reg(op, rclass, reg)) {
      return TRUE;
    }
  }
  return FALSE;
}

/*
  Check if bb1 can reach bb2, if it can, return true; otherwise, return false.

  Note: if bb1 == bb2, it does not mean bb1 reaches bb2 ! Only if bb1 can reach
        the entry of bb2 via control flow does it mean bb1 reach bb2.
*/
static bool BB_reaches_BB (BB *bb1, BB *bb2, bool *vbbs)
{
  vbbs[BB_id(bb1)] = true;

  BBLIST *edge;
  FOR_ALL_BB_SUCCS (bb1, edge) {
    BB *succ = BBLIST_item(edge);
    if ( succ == bb2) {
      return true;
    }
    if (vbbs[BB_id(succ)]) {
      continue;
    }
    if (BB_reaches_BB(succ, bb2, vbbs)) {
      return true;
    }
  }

  return false;
}

/* Return true if tn is the same as input parameter */
static bool Is_RegTN_InParam(TN *tn, BB *entry_bb)
{
  if ( (TN_register_class(tn) != TI_ISA_Regclass_Integer()) ||
       (TN_register(tn) == REGISTER_UNDEFINED) ) {
    return false;
  }

  ANNOTATION *ant = ANNOT_Get(BB_annotations(entry_bb), ANNOT_ENTRYINFO);
  FmtAssert(ant != NULL, ("No ENTRYINFO annotation"));
  ENTRYINFO *my_ant = ANNOT_entryinfo(ant);
  FmtAssert(my_ant != 0, ("No ENTRYINFO annotation"));

  TN_LIST *tnl;
  for (tnl = ENTRYINFO_in_params(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
    TN *tmp_tn = TN_LIST_first(tnl);
    if (TN_register(tn) == TN_register(tmp_tn)) {
      return true;
    }
  }
  return false;
}

/* return true if tn is one of return register of the call */
static bool Is_RegTN_ReturnReg(TN *tn, BB *call_bb)
{
  ANNOTATION *ant = ANNOT_Get(BB_annotations(call_bb), ANNOT_CALLINFO);
  FmtAssert(ant != NULL, ("No CALLINFO annotation"));
  CALLINFO *my_ant = ANNOT_callinfo(ant);
  FmtAssert(my_ant != 0, ("No CALLINFO annotation"));

  TN_LIST *tnl;
  for (tnl = CALLINFO_ret_value(my_ant); tnl; tnl = TN_LIST_rest(tnl)) {
    TN *tmp_tn = TN_LIST_first(tnl);
    if ( (TN_register_class(tn) == TN_register_class(tmp_tn)) &&
         (TN_register(tn) == TN_register(tmp_tn)) ) {
      return true;
    }
  }
  return false;
}

/*
   Given a use tn in use_bb, this function checks if this tn
   has a unique definition BB.

   Return value: 
      NULL:      no unique def (or no def at all)
      bb:        bb is the unqiue BB that defines tn.
*/
BB *REG_PROP::Reg_TN_unique_def_bb(TN *tn, BB *use_bb)
{
  if ( visitedbbs == NULL) {
    // Allocate 2 more for safety (1 more is necessary)
    visitedbbs = TYPE_MEM_POOL_ALLOC_N(bool, _pool,  PU_BB_Count + 2);
  }

  BB *bb;
  BB *ret_bb = NULL;
  for (bb = first_bb; bb != NULL; bb = BB_next(bb)) {
    if ( BB_Defs_Reg(bb, TN_register_class(tn), TN_register(tn)) ||
         (BB_entry(bb) && Is_RegTN_InParam(tn, bb)) ||
         (BB_call(bb)  && Is_RegTN_ReturnReg(tn, bb)) ) {
      // Check if bb reaches use_bb, if not, this def should be ignored
      int i;
      for (i=0; i < PU_BB_Count + 2; i++) {
        visitedbbs[i] = false;
      }
      if (BB_reaches_BB(bb, use_bb, visitedbbs)) {
        if (ret_bb == NULL) {
          ret_bb = bb;
        } else {
          return NULL;
        }
      }
    }
  }
  return ret_bb;
}
  

/*
 * Given a new def_tn, from all valid copies, remove those that are
 * invalid due to this new def_tn.  
 *
 * Two TNs are the same if they both are assigned to the same register.
 */
void REG_PROP::Stop_Reg_Def_Copy(ISA_REGCLASS rc, REGISTER r, REGISTER except_r, 
                                 BB *bb, OP *op, EBO_CP_LOC loc)
{
  bool  copy_found=false;

  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);
    Is_True(Has_Replacement(tn), ("corrupt active copy-propagations list"));
    if (rc == TN_register_class(tn) &&
        except_r == TN_register(tn)) continue;

    /*
        Stop the following copies:
           copy   :  r1 = tn <rc, r>
     */
    TN *repl_tn = _tn_infos[TN_number(tn)]._replace_tn;
    if ( (TN_register_class(repl_tn) == rc) && (TN_register(repl_tn) == r) ) {
      TN_PROP_BASE::Stop_Copy_Prop(tn, bb, op, loc);
    }
  }
}

/*
   Decide whether a register TN, which is OP_opnd(op, index)/OP_result(op, index)
   can be replaced.
 */
BOOL REG_PROP::Can_Reg_TN_Be_Replaced (BB *bb, OP *op, int index, BOOL is_result)
{
  TN *tn = (is_result) ? OP_result(op, index) : OP_opnd(op, index);
  Is_True( TN_is_register(tn), ("TN must be register TN"));
  Is_True( TN_register(tn) != REGISTER_UNDEFINED, ("TN must be allocated a register"));

  /* 
    If tn is a asm_reg, then it cannot be replaced with another TN !

    For example, simcall requires a2 be used as request code:
          register int simcode asm ("a2");

          a2 = a10
          asm volatile ("simcall" : "+a" (simcode));
          --------------------------
          a10 cannot be propagated into asm to replace a2 !
   */
  if (TN_is_asm_reg(tn)) {
    return FALSE;
  }

  /*
    If tn is a dedicated tn in a simulated OPs, cannot propagate.

    For example, 'b' is boolean register.

        TN225(b1) :- orb GTN21(b0) GTN21(b0) ; copy
        GTN230(a12) :- rsr_br TN41(breg) GTN21(b0) TN22(b1) TN23(b2)
                              TN24(b3) TN25(b4) TN26(b5) TN27(b6) TN28(b7)
                              TN29(b8) TN30(b9) TN31(b10) TN32(b11) TN33(b12)
                              TN34(b13) TN35(b14) TN36(b15) ;

        Where, TN22(b1) in rsr_br denotes a use of a physical regsiter and thus
        cannot be replaced. (If replaced, the copy may be deleted which is wrong.)

     Note: for real OPs, we have the assumption that any explicitly-defined 
           dedicated TN has no explicit use. So, if there are explicit uses of
           a dedicated TN, the uses must be non-dedicated by nature; and thus
           can be replaced with other TNs.
  
           However, for compiler-generated simulated OPs, the above assumption
           does not hold.
    */
  if (OP_simulated(op) && TN_is_dedicated(tn)) {
    return FALSE;
  }

  return TRUE;
}

BOOL REG_PROP::BB_Reg_Replaceable (BB *bb, ISA_REGCLASS rc, REGISTER reg)
{
  OP *op;
  int i;

  // Don't replace BB with asm for now
  if (BB_asm(bb)) {
    return FALSE;
  }

  FOR_ALL_BB_OPs (bb, op)
  {
    for (i=0; i < OP_results(op); i++) {
      TN *tn = OP_result(op, i);
      if (TN_is_register(tn) && 
          TN_register_class(tn) == rc && TN_register(tn) == reg) {
        if ( !Can_Reg_TN_Be_Replaced (bb, op, i, TRUE) )
          return FALSE;
      }
    }

    for (i=0; i < OP_opnds(op); i++) {
      TN *tn = OP_opnd(op, i);
      if (TN_is_register(tn) && 
          TN_register_class(tn) == rc && TN_register(tn) == reg) {
        if ( !Can_Reg_TN_Be_Replaced (bb, op, i, FALSE) )
          return FALSE;
      }
    }
  }
  return TRUE;
}

/*
   Find out the next reference to the TN,  OP_opnd(op, opnd_ix).
   Return NULL, if none.
*/ 
BOOL REG_PROP::Reg_TN_Next_Ref (BB *bb, OP *op, int opnd_ix)
{
  TN *tn = OP_opnd(op, opnd_ix);
  Is_True(TN_is_register(tn), ("TN must be register TN"));
  ISA_REGCLASS rc = TN_register_class(tn);
  REGISTER    reg = TN_register(tn);

  if ( visitedbbs == NULL) {
    // Allocate 2 more for safety (1 more is necessary)
    visitedbbs = TYPE_MEM_POOL_ALLOC_N(bool, _pool,  PU_BB_Count + 2);
  }
  int i;
  for (i=0; i < PU_BB_Count + 2; i++) {
    visitedbbs[i] = false;
  }

  bool ret = Get_Reg_Value_Next_Ref (bb, OP_next(op), rc, reg, visitedbbs, bb, op);

  return ret ? TRUE : FALSE;
}
    
/*
   If within_bb is true, we only do propagation within a BB
 */
bool REG_PROP::Copy_BB_Forward(BB *bb)
{
  bool is_propagated = false;

  if (_trace) {
    fprintf(TFile, "%s START BB:%d\n", _trace_msg_prefix, BB_id(bb));
  }

  /*
     Cannot do propagation for the following cases 

     case 1:
          r1 = r2
          asm r3 = r1  (asm's clobber set has r2)

     case 2:
          r1 = r2
          asm r2 = r1  (r2 is asm's early clobber)

     case 3:
          r1 = r2
          r1 = r1, r3, r4 // cmov
          ---------------------
          r2 alive

          If r2 is not alive, we can propagate r2 into r1 = r1,....
          And then, the copy 'r1 = r2' will be inserted afterwards, like:
              r2 = r2, r3, r4  // cmov
              r1 = r2

        case 3.1

          But if the code will become the following after propagation, 
          don't propagate:
              r2 = r2, r3, r4  // cmov
              r1 = r2
              r2 = ...
              ---------
              r1, r2 alive

          because copy 'r1 = r2' cannot be deleted. ( This may cause 
          worse flix due to dependence between cmov and the following
          new copy.) 

          In addition, if we have:

        case 3.2

            B1   r1 = r2
                 r1 = r1, r3, r4 // cmov
                          |
                          |
                          V
                  |------------------------------|
                  |                              |
                  V                              V
            B2:  r1 = r1, r5, r6          B3    r2 = 
                                                   = r1

            Apply the above transformation:

            B1   r2 = r2, r3, r4 // cmov
            B1   r1 = r2
                          |
                          |
                          V
                  |------------------------------|
                  |                              |
                  V                              V
            B2:  r2 = r2, r5, r6          B3    r2 = 
                 r1 = r2                           = r1

            where we have two copies, which is bad. To prevent this from 
            happening, we conservatively don't copy-propagate into 
            same-result OP (like cmov) if the copy's original BB isn't
            the current BB.

     case 4:
          b <- a
          a, b <- b, x, y   // tie

          If 'a' is propagated, the TIE instruction will have two results
          sharing the same register, which causes the undefined/unknown
          behavior.

  */
  for (OP *next, *op = BB_first_op(bb); op; op = next)
  {
    next = OP_next(op);

    bool is_op_propagated = false;

    if (_trace) {
     fprintf(TFile, "%s ", _trace_msg_prefix);
      Print_OP_No_SrcLine(op);
    }

    const BOOL same_res = OP_same_res(op);
    const INT copy_opnd_idx = EBO_Prop_Copy_Operand(op);
    ASM_OP_ANNOT* asm_info = (OP_code(op) == TOP_asm) ?
      (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op) : NULL;

    /* 
      Case 1

      For all copies whose registers (either source or target) are
      clobbered by ASM, stop these copies.
    */
    if (asm_info) {
      ISA_REGCLASS rc;
      FOR_ALL_ISA_REGCLASS( rc ) {
        REGISTER_SET asm_regs = ASM_OP_clobber_set(asm_info)[rc];
        for (REGISTER reg = REGISTER_SET_Choose(asm_regs);
             reg != REGISTER_UNDEFINED;
             reg = REGISTER_SET_Choose_Next(asm_regs, reg)) {
          // Stop copies whose sources are reg
          Stop_Reg_Def_Copy(rc, reg, REGISTER_UNDEFINED, bb, op, EBO_CP_BEFORE);

          // Stop the copy whose dest is reg
          TN *dst = Build_Dedicated_TN(rc, reg, REGISTER_bit_size(rc, reg)/8);
          if (Has_Replacement(dst))
            Stop_Copy_Prop(dst, bb, op, EBO_CP_BEFORE);
        }
      }
    }

    if (Bad_Forward_OP(op)) {
      Stop_All_Copy_Props(bb, op, EBO_CP_BEFORE);
      continue;
    } else {
      for (int i=0; i < OP_opnds(op); i++) {
        TN *const tn = OP_opnd(op, i);

        // Need to check if tn can be replaced with another TN
        if ( TN_is_register(tn) && Has_Replacement(tn)  &&
             Can_Reg_TN_Be_Replaced(bb, op, i, FALSE) )
        {
          TN *rep = Replacement(tn);
          bool stop_ppg = false;

          for (int res=0; res < OP_results(op); res++)
          {
            if (
                   // case 2
                    TNs_Are_Equivalent(rep, OP_result(op, res)) &&
                    asm_info && 
                    ASM_OP_result_clobber(asm_info)[res]
                 || 
                   // case 3   
                    same_res && 
                    (res == Op_Sameres_Result(op, i)) && 
                    (!Reg_Dead(TN_register_class(rep), TN_register(rep), bb, op) ||
                     !Copy_Prop_Profitable(bb, OP_next(op), tn, rep))  // case 3.1
                 ||
                   // case 4
                   same_res && 
                   (res == Op_Sameres_Result(op, i)) &&
                   OP_Defs_Reg(op, TN_register_class(rep), TN_register(rep))
               )
            {
              stop_ppg = true;
              break;
            }
          }
          if (stop_ppg) {
            Stop_Copy_Prop (tn, bb, op, EBO_CP_BEFORE);
          } else {
            /*
               case 3.2
             */
            int res_ix = Op_Sameres_Result(op, i);
            if (same_res && (res_ix >= 0)) 
            {
              OP *cop = Copy(tn);
              if (OP_bb(cop) == bb) {
                Replace_TN_Operand(tn, op, i);
                Replace_TN_Result(tn, op, res_ix);

                // Move copy op to be after this 'op'
                OP *new_copy_op = Dup_OP(cop);
                BB_Insert_Op_After(bb, op, new_copy_op);

                is_op_propagated = true;
              } else {
                Stop_Copy_Prop (tn, bb, op, EBO_CP_BEFORE);
              }
            } else {
              Replace_TN_Operand(tn, op, i);
              is_op_propagated = true;
            }
          }
        }
      }
    }

    // Process results. If it is a self copy, ignore it.
    bool is_self_copy = false; 
    if (copy_opnd_idx >= 0) {
      TN *t1 = OP_result(op, 0);
      TN *t2 = OP_opnd(op, copy_opnd_idx);
      is_self_copy = (TN_register(t1) == TN_register(t2));
    }

    if (!is_self_copy)
    {
      for (int i = 0; i < OP_results(op); i++)
      {
        TN *tn = OP_result(op, i);
        if (TN_is_register(tn)) {
          Stop_Reg_Def_Copy(TN_register_class(tn), TN_register(tn),
                            REGISTER_UNDEFINED, bb, op, EBO_CP_BEFORE);
          if (Has_Replacement(tn))
            Stop_Copy_Prop(tn, bb, op, EBO_CP_BEFORE);
        }
      }
    }

    // Process special OPs
    if (OP_call(op)) {
      Stop_Copy_At_Call (bb, op, EBO_CP_BEFORE);
    } else if (!next && BB_exit(bb)) {
      Stop_Copy_At_Exit(bb, op, EBO_CP_BEFORE);
    } else if (copy_opnd_idx >= 0) {
      TN *res = OP_result(op, 0);
      TN *opnd = OP_opnd(op, copy_opnd_idx);

      if (BB_rotating_kernel(bb)) {
        // Since we cannot remove copy in SWP'ed BB, no need to
        // consider any copy in a SWP'ed BB
        if (_trace) {
          fprintf(TFile, "%s Copy in SWP BB(%d) ignored, ",
                         _trace_msg_prefix, BB_id(bb));
          Print_OP_No_SrcLine(op);
        }
      } else if (is_self_copy) { // TN_register(res) == TN_register(opnd)
        if (_trace)
        {
          fprintf(TFile, "%s Removing self copy ", _trace_msg_prefix);
          Print_OP_No_SrcLine(op);
        }
        BB_Remove_Op(bb, op);
        is_op_propagated = true;
      } else {
        FmtAssert( (TN_register(res) != REGISTER_UNDEFINED) &&
                   (TN_register(opnd) != REGISTER_UNDEFINED),
                   ("TN must be allocated a register"));
        Set(res, opnd, 0, op, CP_FLAG_NONE);
      }
    }

    if (_trace && is_op_propagated) {
      fprintf(TFile, "%s After Propagating copy ", _trace_msg_prefix);
      Print_OP_No_SrcLine(op);
    }
    is_propagated |= is_op_propagated;
  }

  if (_trace) {
    fprintf(TFile, "%s End of BB:%d \n", _trace_msg_prefix, BB_id(bb));
  }

  return is_propagated;
}

bool REG_PROP::Copy_BB_Backward(BB *bb)
{
  bool is_propagated = false;

  if (_trace) {
    fprintf(TFile, "%s START BB:%d (Backward)\n", _trace_msg_prefix, BB_id(bb));
  }

  /* 
    We only need to consider copy d = s where d is not dead ANS d != s.
    The copy d=s will be removed from the original BB and is marked as
    replacement_needed. If the copy propagates to an OP that defines s and
    that s is replaced by d, then the copy 'd=s' will be dropped (no longer
    needed); otherwise, it must be kept (the replacement copy must be inserted 
    into the current BB).

    In addition, for a copy d = s, if s is NOT DEAD, this copy cannot be
    removed, we will not propagate this copy (but propagating it may result
    in other copies to be removed).

    For the following cases, we stop propagation:

    Case 1:
      asm r3 = r2   // r1 in asm's clobber set
      r1 = r2  (copy)  
      --------------
      Any clobber register cannot be used as an operand to asm.

    Case 2:
      asm r1 = r2   // r1 is asm's early clobber
      r1 = r2  (copy)
      --------------
      Early clobber register cannot be used as an operand to asm.

    Case 3:
      r1 = r1, r2, r3  // cmov, same-result OP
      r4 = r1  (copy)
      ---------------

      Well, we actually can propagate. But we will stop propagation
      until it is showed to be useful.

    Case 4:
      r1, r2 = r2, r4, r5  // tie instruction
      r1 = r2 (copy)
      ---------------

      If r2 is propagated into this TIE inst, both result registers
      are the same, which has a undefined behavior.
  */

  for (OP *prev, *op = BB_last_op(bb); op; op = prev)
  {
    prev = OP_prev(op);
    bool is_op_propagated = false;

    if (_trace) {
     fprintf(TFile, "%s ", _trace_msg_prefix);
      Print_OP_No_SrcLine(op);
    }

    const BOOL same_res = OP_same_res(op);
    const INT copy_opnd_idx = EBO_Prop_Copy_Operand(op);
    ASM_OP_ANNOT* asm_info = (OP_code(op) == TOP_asm) ?
      (ASM_OP_ANNOT*) OP_MAP_Get(OP_Asm_Map, op) : NULL;

    /*
      Case 1

      For all copies whose registers (either source or target) are
      clobbered by ASM, stop these copies.
    */
    if (asm_info) {
      ISA_REGCLASS rc;
      FOR_ALL_ISA_REGCLASS( rc ) {
        REGISTER_SET asm_regs = ASM_OP_clobber_set(asm_info)[rc];
        for (REGISTER reg = REGISTER_SET_Choose(asm_regs);
             reg != REGISTER_UNDEFINED;
             reg = REGISTER_SET_Choose_Next(asm_regs, reg)) {
          Stop_Reg_Def_Copy(rc, reg, REGISTER_UNDEFINED, bb, op, EBO_CP_AFTER);

          TN *dst = Build_Dedicated_TN(rc, reg, REGISTER_bit_size(rc, reg)/8);
          if (Has_Replacement(dst))
            Stop_Copy_Prop(dst, bb, op, EBO_CP_AFTER);
        }
      }
    }

    if (Bad_Backward_OP(op)) {
      Stop_All_Copy_Props(bb, op, EBO_CP_AFTER);
      continue;
    } else {
      for (int i=0; i < OP_results(op); i++)
      {
        TN *const tn = OP_result(op, i);

        if (TN_is_register(tn) && Has_Replacement(tn)) 
        {
          if (Can_Reg_TN_Be_Replaced(bb, op, i, TRUE)) 
          {
            TN *rep = Replacement(tn);
            int opnd_ix = Op_Sameres_Operand(op, i);

            if (
                   // Case 2
                   asm_info && ASM_OP_result_clobber(asm_info)[i] &&
                   OP_Refs_Reg(op, TN_register_class(rep), TN_register(rep)) 
                || 
                   // Case 3
                   same_res && (opnd_ix >= 0) 
                || 
                   // Case 4
                   OP_Defs_Reg(op, TN_register_class(rep), TN_register(rep))
               )
            {
              Stop_Copy_Prop (tn, bb, op, EBO_CP_AFTER);
            } else
            {
              Replace_TN_Result(tn, op, i);
              is_op_propagated = true;

              /*
                 Since 'rep' is redefined, the original copy 'rep = tn'
                 must be removed !
              */
              Drop_Copy(tn);
            }
          } else { 
            Stop_Copy_Prop (tn, bb, op, EBO_CP_AFTER);
          }
        } 
        /*
           If there is an active copy like 'tn = sth', the def of 'tn' in this OP is
           actually dead. We stop here and let the core EBO to decide if this
           op can be removed.
        */
        Stop_Reg_Def_Copy (TN_register_class(tn), TN_register(tn), REGISTER_UNDEFINED,
                           bb, op, EBO_CP_AFTER);
      }

      /*
         Stop any copies of the following:
           op1:       =   r1  
           copy:  r1  =   r2 
         The copy must be stopped; otherwise, r1 in op1 would use the wrong value.
         Since this is the backward propagation and r1 of 'copy' is treated as source,
         invoking Stop_Reg_Def_Copy() should work just fine.

         We will ignore a self copy!
       */
      bool ignore_copy = false;
      if (    copy_opnd_idx < 0
           || copy_opnd_idx >=0 && 
              !TNs_Are_Equivalent(OP_result(op,0), OP_opnd(op, copy_opnd_idx)) )
      {
        for (INT i = 0; i < OP_opnds(op); i++)
        {
          TN *const tn = OP_opnd(op, i);

          if (TN_is_register(tn)) {
            Stop_Reg_Def_Copy(TN_register_class(tn), TN_register(tn),
                            REGISTER_UNDEFINED, bb, op, EBO_CP_AFTER);
          }
        }

        /* Replace each operand TN with its replacement TN. */
        for (INT i = 0; i < OP_opnds(op); i++)
        {
          TN *const tn = OP_opnd(op, i);
  
          if (TN_is_register(tn) && Has_Replacement(tn)  &&
               Can_Reg_TN_Be_Replaced(bb, op, i, FALSE) )
          {
            /*
               If we have two copies:
                   c1: r1 = r2
                   c2: r3 = r2 
               We will keep the c2 (first in backward propagation) and ignore c1.
               After propagation, the above will be:
                   (pending c2: r3 = r2)
                   c1: r1 = r3
             */
            if (copy_opnd_idx >= 0) {
              ignore_copy = true;
            }
            Replace_TN_Operand(tn, op, i);
  
            is_op_propagated = true;
          } else if (TN_is_register(tn) && Has_Replacement(tn)) {
            // Have replacement TN, but cannot be replaced, so stop the copy
            Stop_Copy_Prop(tn, bb, op, EBO_CP_AFTER);
          }
        }
      }

      if (OP_call(op)) {
        Stop_Copy_At_Call (bb, op, EBO_CP_AFTER);
      } else if (!prev && BB_entry(bb)) {
        Stop_Copy_At_Entry(bb, NULL, EBO_CP_AFTER);
      } else if (copy_opnd_idx >= 0)
      {
        TN *res = OP_result(op, 0);
        TN *opnd = OP_opnd(op, copy_opnd_idx);

        if (BB_rotating_kernel(bb)) {
          // Since we cannot remove copy in SWP'ed BB, no need to
          // consider any copy in a SWP'ed BB
          if (_trace) {
            fprintf(TFile, "%s Copy in SWP BB(%d) ignored, ",
                           _trace_msg_prefix, BB_id(bb));
            Print_OP_No_SrcLine(op);
          }

        } else if (TNs_Are_Equivalent(res, opnd)) {
          if (_trace) {
            fprintf(TFile, "%s Removing self copy ", _trace_msg_prefix);
            Print_OP_No_SrcLine(op);
          }

          BB_Remove_Op(bb, op);
          is_op_propagated = true;

        } else if (!ignore_copy &&
                   !Reg_Dead(TN_register_class(res), TN_register(res), bb, op) &&
                   (Reg_Dead(TN_register_class(opnd), TN_register(opnd), bb, op) ||
                    !Reg_TN_Next_Ref(bb, op, copy_opnd_idx))) {
          /*
             First, do not propagate copies whose's source is 'opnd' to prevent
             two copies with the same source from being active at the same time.

             Second, if res is dead, don't propagate!

             Only propagate the copy that can be removed. If 'opnd' isn't dead,
             the copy must be kept. Therefore, we don't propagate such the copy.
             For the case that 'opnd' isn't dead, but there is no reference to 'opnd'
             following the BB 'bb', then the copy can be removed, and we will
             propagate it. 
          */
          if (Has_Replacement(opnd))
            Stop_Copy_Prop(opnd, bb, op, EBO_CP_AFTER);

          Set(opnd, res, 0, op, CP_FLAG_PLACEMENT_NEEDED);
          BB_Remove_Op(bb, op);
 
          // Since op has been removed, set is_op_propagated to true.
          is_op_propagated = true;

          if (_trace)
          {
            fprintf(TFile, "%s Backward propagating copy ", _trace_msg_prefix);
            Print_OP_No_SrcLine(op);
          }
        }
      }
    }

    if (_trace && is_op_propagated) {
      fprintf(TFile, "%s After Propagating copy ", _trace_msg_prefix);
      Print_OP_No_SrcLine(op);
    }

    is_propagated |= is_op_propagated;
  } 

  if (_trace) {
    fprintf(TFile, "<ebo_reg_cp> Reached Start of BB:%d\n", BB_id(bb));
  }

  return is_propagated;
}

static void
BB_Replace_TN(BB *bb, TN *dst_tn, TN *repl_tn)
{
  OP *op;
  int i;

  FOR_ALL_BB_OPs (bb, op)
  {
    for (i=0; i < OP_results(op); i++) {
      if (OP_result(op, i) == dst_tn) {
        Set_OP_result(op, i, repl_tn);
      }
    }

    for (i=0; i < OP_opnds(op); i++) {
      if (OP_opnd(op, i) == dst_tn) {
        Set_OP_opnd(op, i, repl_tn);
      }
    }
  }
}


static void
BB_Replace_Reg(BB *bb, TN *dst_tn, TN *repl_tn)
{
  OP *op;
  int i;
  ISA_REGCLASS rc = TN_register_class(dst_tn);
  REGISTER    reg = TN_register(dst_tn);

  FOR_ALL_BB_OPs (bb, op)
  {
    for (i=0; i < OP_results(op); i++) {
      TN *tn=OP_result(op, i);
      if ( TN_is_register(tn) && 
           TN_register_class(tn) == rc && TN_register(tn) == reg) {
        Set_OP_result(op, i, repl_tn);
      }
    }

    for (i=0; i < OP_opnds(op); i++) {
      TN *tn=OP_opnd(op, i);
      if ( TN_is_register(tn) && 
           TN_register_class(tn) == rc && TN_register(tn) == reg) {
        Set_OP_opnd(op, i, repl_tn);
      }
    }
  }
}

bool REG_PROP::Copy_Forward_SwpRegion(BB *prolog, BB *epilog, BB *body)
{
  bool ret = false;
  if (_trace) {
    fprintf(TFile, "%s Start propagating to SWP BB:%d\n", _trace_msg_prefix, BB_id(body));
  }

  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     dst_reg = TN_register(tn);
    REGISTER     src_reg = TN_register(tn_prop->_replace_tn);

    /*
                 |
                 V
        copy: dst = src (prolog)
              V    V
              |    |
          |-->|    |c
          --BODY   |
              |    |
              |    |
              V    V
              Epilog
                |
                V

        We propagate 'dst = src' to Epilog if the following are true:
          1) Both dst and src are not modified in BODY, or
          2) src is dead AND body does not reference nor modify src.

        We also need to see if dst in BODY can be replaced. If all of
        these conditions are met, we can replace all dst in BODY with
        src and move the copy into epilog.
    */
    bool def_src = BB_Defs_Reg(body, rc, src_reg); 
    if ( ((!BB_Defs_Reg(body, rc, dst_reg) && !def_src) ||
          (!REG_LIVE_Outof_BB(rc, src_reg, prolog) && !def_src &&
           !BB_Refs_Reg(body, rc, src_reg)) 
         ) && BB_Reg_Replaceable(body, rc, dst_reg) )
    {
      if (_trace) {
        fprintf(TFile, "%s  replaced register ", _trace_msg_prefix);
        Print_TN(tn, FALSE);
        fprintf(TFile, " with ");
        Print_TN(tn_prop->_replace_tn, FALSE);
        fprintf(TFile, "\n");
      }

      OP *copy_op = Dup_OP(tn_prop->_copy_op);
      if (tn_prop->_copy_op->bb == prolog) {
        BB_Remove_Op(prolog, tn_prop->_copy_op);
      }
      BB_Insert_Op(epilog, NULL, copy_op, TRUE);
      // Need to update reg live info
      REG_LIVE_Update(rc, src_reg, epilog);

      BB_Replace_Reg(body, tn, tn_prop->_replace_tn);
      if (REG_LIVE_Into_BB(rc, dst_reg, body)) {
        REG_LIVE_Update(rc, src_reg, body);
      }
      TN_PROP_BASE::Clear(tn);

      ret = true;
    } else {
      TN_PROP_BASE::Stop_Copy_Prop(tn, prolog, NULL, EBO_CP_BEFORE);
    }
  }

  if (ret) {
    if (_trace) {
      OP *op;
      fprintf(TFile, "%s SWP BB after propagation:\n", _trace_msg_prefix);
      FOR_ALL_BB_OPs (body, op)
        Print_OP_No_SrcLine(op);
    }
  }

  if (_trace) {
    fprintf(TFile, "%s End propagating to SWP BB:%d\n", _trace_msg_prefix, BB_id(body));
  }
   
  return ret;
}

bool REG_PROP::Copy_Backward_SwpRegion(BB *prolog, BB *epilog, BB *body)
{
  bool ret = false;
  if (_trace) {
    fprintf(TFile, "%s Start Backward propagating to SWP BB:%d\n", 
                   _trace_msg_prefix, BB_id(body));
  }

  OP *loop_op = BB_branch_op(prolog);
  bool is_loop_op = loop_op && (OP_code(loop_op) == TOP_loop || 
                                OP_code(loop_op) == TOP_loopgtz ||
                                OP_code(loop_op) == TOP_loopnez);
  FmtAssert(is_loop_op, ("SWP region's prolog does not have loop op"));

  for (TN_LIST *next, *tn_list = _active_cp_tns; tn_list; tn_list = next)
  {
    next = TN_LIST_rest(tn_list);

    TN *tn = TN_LIST_first(tn_list);

    /* no need to call Get(tn), TN_PROP_BASE::Get(tn) is faster */
    ONE_TN_PROP *tn_prop = TN_PROP_BASE::Get(tn);

    ISA_REGCLASS rc  = TN_register_class(tn);
    REGISTER     dst_reg = TN_register(tn);
    REGISTER     src_reg = TN_register(tn_prop->_replace_tn);

    /*
            Prolog
              V V
              | |
          |-->| |---
          |   |    |
          |-BODY   |
              V    |
              |    |
       copy: src = dst (epilog)

        We can do propagation for the following cases:
          Only consider the cases in which BODY does not reference 
          nor modify src
            1) if dst is not modified in BODY, then 
                move the copy to Prolog.

            2) if dst is defined in body AND also in prolog, then 
                move the copy to Prolog.
               
            3) if dst is only defined in body.
               The copy will be removed after replacing dst with src

          Note: in the backward propagation, only copies that can
                be removed are propagated. In another word,  dst
                of a copy 'src = dst' will be the last ref to dst
                (the last ref is stronger than dead !).

        We also need to see if dst in BODY can be replaced. If all of
        these conditions are met, we can replace all dst in BODY with
        src.
    */
    BB *unique_bb = Reg_TN_unique_def_bb (tn, epilog);
    bool remove_copy = (unique_bb == body);
    bool prolog_def_dst = BB_Defs_Reg(prolog, rc, dst_reg);
    bool def_src = BB_Defs_Reg(body, rc, src_reg); 
    bool def_dst = BB_Defs_Reg(body, rc, dst_reg);
    bool ref_src = BB_Refs_Reg(body, rc, src_reg);
    if ( ((!def_src && !ref_src && 
           (!def_dst ||         // case 1
            prolog_def_dst ||   // case 2
            remove_copy))       // case 3
         ) && BB_Reg_Replaceable(body, rc, dst_reg) 
           && !OP_Refs_Reg(loop_op, rc, src_reg) && !OP_Refs_Reg(loop_op, rc, dst_reg) )
    {
      if (_trace) {
        fprintf(TFile, "%s  replaced register ", _trace_msg_prefix);
        Print_TN(tn, FALSE);
        fprintf(TFile, " with ");
        Print_TN(tn_prop->_replace_tn, FALSE);
        fprintf(TFile, "\n");
      }

      BB_Replace_Reg(body, tn, tn_prop->_replace_tn);

      if (!def_src && !ref_src && remove_copy) {
        if (_trace) {
          fprintf(TFile, "%s      removed the copy in epilog (BB:%d)\n",
                          _trace_msg_prefix, BB_id(epilog));
        }
      } else {
        OP *copy_op = Dup_OP(tn_prop->_copy_op);
        BB_Insert_Op(prolog, loop_op, copy_op, TRUE);
        if (_trace) {
          fprintf(TFile, "%s      moved the copy to prolog (BB:%d)\n",
                          _trace_msg_prefix, BB_id(prolog));
        }
      }

      TN_PROP_BASE::Clear(tn);

      ret = true;
    } else {
      TN_PROP_BASE::Stop_Copy_Prop(tn, epilog, NULL, EBO_CP_AFTER);
    }
  }

  if (ret) {
    if (_trace) {
      OP *op;
      fprintf(TFile, "%s SWP BB after propagation:\n", _trace_msg_prefix);
      FOR_ALL_BB_OPs (body, op)
        Print_OP_No_SrcLine(op);
    }
  }

  if (_trace) {
    fprintf(TFile, "%s End propagating to SWP BB:%d\n", _trace_msg_prefix, BB_id(body));
  }
   
  return ret;
}

/*
   Do a register copy propagation within an extended BB
 */
bool REG_PROP::Propagate_Reg_Forward_EBO(BB *curr_bb)
{
  BB *bb = curr_bb;
  bool ret=false;

  // Mark bb as visited
  Set_BB_visited(bb);

  // Do forward copy propagation with the BB
  ret |= Copy_BB_Forward(bb);

  Adjust_Reg_Forward_Exit(bb);

  if (EBO_Reg_Prop_SwpRegion) {
    BB *prolog, *epilog, *body;
    while (Is_SwpRegion_Prolog(bb, &epilog, &body)) {
      prolog = bb;

      ret |= Copy_Forward_SwpRegion(prolog, epilog, body);

      Set_BB_visited(epilog);
      ret |= Copy_BB_Forward(epilog);
      Adjust_Reg_Forward_Exit(epilog);
  
      bb = epilog;
    }
  }
  
  BBLIST *edge;
  FOR_ALL_BB_SUCCS(bb, edge)
  {
    BB *succ = BBLIST_item(edge);
    if (BB_Unique_Predecessor(succ) == bb) {
      if (_trace) {
        fprintf(TFile, "%s Continuing copy propagation to succ BB:%d\n",
                _trace_msg_prefix, BB_id(succ));
      }

      REG_PROP *succ_reg_props = CXX_NEW(REG_PROP(*this), _pool);

      succ_reg_props->Adjust_Reg_Forward_Entry(succ);
      ret |= succ_reg_props->Propagate_Reg_Forward_EBO(succ);

      CXX_DELETE(succ_reg_props, _pool);

    } else {
      if (_trace) {
        fprintf(TFile, "%s No copy propagation to non-unique succ BB:%d\n",
                _trace_msg_prefix, BB_id(succ));
      }

      /*  'succ is a new head of EBO, add it into ebo_bbs */
      ebo_bbs = BB_LIST_Push(succ, ebo_bbs, _pool);
    }
  }

  return ret;
}

/*
   Do a register copy propagation within an extended BB
 */
bool REG_PROP::Propagate_Reg_Backward_EBO(BB *curr_bb)
{
  BB *bb = curr_bb;
  bool ret=false;

  // Mark bb as visited
  Set_BB_visited(bb);

  // Do backward copy propagation with the BB
  ret |= Copy_BB_Backward(bb);

  Adjust_Reg_Backward_Exit(bb);

  if (EBO_Reg_Prop_SwpRegion) {
    BB *prolog, *epilog, *body;
    if (Is_SwpRegion_Epilog(bb, &prolog, &body)) {
      epilog = bb;
      Set_BB_visited(body);

      ret |= Copy_Backward_SwpRegion(prolog, epilog, body);

      REG_PROP *tmp_rprop = CXX_NEW(REG_PROP(*this), _pool);
      ret |= tmp_rprop->Propagate_Reg_Backward_EBO(prolog);
      CXX_DELETE(tmp_rprop, _pool);
      bb = prolog;
    } else {
      Stop_All_Copy_Props (bb, NULL, EBO_CP_AFTER);
    }
  } else {
    Stop_All_Copy_Props (bb, NULL, EBO_CP_AFTER);
  }

  BBLIST *edge;
  FOR_ALL_BB_PREDS(bb, edge)
  {
    BB *pred = BBLIST_item(edge);
    if (!BB_visited(pred)) {
      ebo_bbs = BB_LIST_Push(pred, ebo_bbs, _pool);
    }
  }

  
#if 0
  const UINT num_preds = BB_preds_len(bb);
  if (num_preds > 0)
    tn_props->Adjust_Reg_Backward_Exit(bb);

  bool can_prop = true;
  FOR_ALL_BB_PREDS(bb, edge)
  {
    BB *pred = BBLIST_item(edge);
    if ( (BB_Unique_Successor(pred) != bb) ||
         BB_call(pred) || (BB_xfer_op(pred) == NULL) ) {
      can_prop = false;
      break;
    }
  }

  if (!can_prop) {
    Stop_All_Copy_Props(bb, NULL, EBO_CP_AFTER);

    if (trace) {
      fprintf(TFile, "%s No copy propagation to non-candidate pred of BB:%d\n",
        "<ebo_reg_cp>", BB_id(bb));
    }

    FOR_ALL_BB_PREDS(bb, edge)
    {
      BB *pred = BBLIST_item(edge);
      if (!BB_visited(pred)) {
        ebo_bbs = BB_LIST_Push(pred, ebo_bbs, pool);
      }
    }
    return;
  }

  BBLIST *edge;
  FOR_ALL_BB_PREDS(bb, edge)
  {
    BB *pred = BBLIST_item(edge);
    if (BB_Unique_Successor(pred) == bb) {
      if (_trace) {
        fprintf(TFile, "%s Continuing copy propagation to pred BB:%d\n",
                _trace_msg_prefix, BB_id(pred));
      }

      REG_PROP *pred_reg_props = CXX_NEW(REG_PROP(*this), _pool);

      pred_reg_props->Adjust_Reg_Backward_Entry(pred);
      ret |= pred_reg_props->Propagate_Reg_Backward_EBO(pred);

      CXX_DELETE(pred_reg_props, _pool);

    } else {
      if (_trace) {
        fprintf(TFile, "%s No copy propagation to non-unique pred BB:%d\n",
                _trace_msg_prefix, BB_id(pred));
      }

      /*  'pred' is a new head of EBO, add it into ebo_bbs */
      ebo_bbs = BB_LIST_Push(pred, ebo_bbs, _pool);
    }
  }
#endif

  return ret;
}

BOOL EBO_Reg_Copy_Propagation(BB *starting_bb)
{
  Is_True(EBO_in_peep, ("reg-copy-prog should be invoked in peep phase only"));

  MEM_POOL *const pool = &MEM_local_nz_pool;
  MEM_POOL_Push(pool);
  BOOL trace = Get_Trace(TP_EBO, 0x080);

  if (trace) {
     fprintf(TFile, "%s\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s Start of Register Forward Copy propagation\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s\n", "<ebo_reg_cp>");
  }

  BOOL has_propagated = false;
  BB *bb;

 
  if (EBO_Copy_Prop_Intra) {

    REG_PROP cppg (pool, starting_bb, trace);
    // Don't propagate across BBs !
    for (bb=starting_bb; bb != NULL; bb = BB_next(bb)) {
       // skip swp-scheduled BB
       if (BB_rotating_kernel(bb)) continue;

       has_propagated |= cppg.Copy_BB_Forward(bb);

       /*
          Need to stop all copies so no copies will be 
          accidentally used by the following BBs.
        */
       cppg.Stop_All_Copy_Props (bb, NULL, EBO_CP_BEFORE);
    } 
  } else {   

    REG_PROP cppg (pool, starting_bb, trace);

    // Clear visited bit in every BB
    for (bb = starting_bb; bb != NULL; bb = BB_next(bb)) {
      Reset_BB_visited(bb);
    }

    ebo_bbs = BB_LIST_Push(starting_bb, NULL, pool); // a list of heads of every EBO
    while (ebo_bbs)
    { 
      /*
         The head BBs are added into ebo_bbs on the fly in Propagate_Reg_Forward_EBO().

         We can also add all head BBs here by traversing all BBs once (any BBs who
         has more than one predecessor are head BBs) before doing copy propagation.
       */ 
      BB *ebb = BB_LIST_first(ebo_bbs);
      ebo_bbs = BB_LIST_rest(ebo_bbs);

      if (BB_visited(ebb)) {
        continue;
      } else {
        if (trace) {
          fprintf(TFile, "%s Register Copy propagation, start ebo at BB:%d\n",
             "<ebo_reg_cp>", BB_id(ebb));
        }

        has_propagated |= cppg.Propagate_Reg_Forward_EBO(ebb);

        /*
           Need to stop all copies so no copies will be 
           accidentally used by the following EB.
         */
        cppg.Stop_All_Copy_Props (ebb, NULL, EBO_CP_BEFORE);
      }
    }

    // Clear visited bit in every BB
    for (bb = starting_bb; bb != NULL; bb = BB_next(bb)) {
      Reset_BB_visited(bb);
    }
  }

  if (trace) {
     fprintf(TFile, "%s\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s End of Register Forward Copy propagation\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n", "<ebo_reg_cp>");
     fprintf(TFile, "%s\n", "<ebo_reg_cp>");
  }

  if (EBO_Reg_Prop_Backward) {

    if (has_propagated) {
      REG_LIVE_Finish();
      REG_LIVE_Analyze_Region();
    }

    REG_PROP *regppg = CXX_NEW(REG_PROP(pool, starting_bb, trace), pool);

    if (trace) {
       fprintf(TFile, "%s\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s Start of Register Backward Copy propagation\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s\n", "<ebo_reg_cp>");
    }

    if (EBO_Copy_Prop_Intra) {
      // Do propagation within a BB
      for (bb=starting_bb; bb != NULL; bb = BB_next(bb)) {
        // skip swp-scheduled BB
        if (BB_rotating_kernel(bb)) continue;
 
        has_propagated |= regppg->Copy_BB_Backward(bb);

        /*
           Need to stop all copies so no copies will be
           accidentally used by the following BBs.
         */
        regppg->Stop_All_Copy_Props (bb, NULL, EBO_CP_AFTER);
      }
    } else {
      // Clear visited bit in every BB, and set up ebo_bbs.
      for (bb = starting_bb; bb != NULL; bb = BB_next(bb)) {
        Reset_BB_visited(bb);
        if (BB_exit(bb)) {
          ebo_bbs = BB_LIST_Push(bb, NULL, pool); // a list of EBO heads
        }
      }

      while (ebo_bbs)
      {
        /*
           The head BBs are added into ebo_bbs on the fly in Propagate_Reg_Backward_EBO().
  
           We can also add all head BBs here by traversing all BBs once (any BBs who
           has more than one predecessor are head BBs) before doing copy propagation.
         */
        BB *ebb = BB_LIST_first(ebo_bbs);
        ebo_bbs = BB_LIST_rest(ebo_bbs);

        if (BB_visited(ebb)) {
          continue;
        } else {
          if (trace) {
            fprintf(TFile, "%s Register Backward Copy propagation, start ebo at BB:%d\n",
               "<ebo_reg_cp>", BB_id(ebb));
          }

          has_propagated |= regppg->Propagate_Reg_Backward_EBO(ebb);
  
#if 0
          /*
             Need to stop all copies so no copies will be
             accidentally used by the following EB.
           */
          regppg->Stop_All_Copy_Props (ebb, NULL, EBO_CP_AFTER);
#endif
        }
      }
    }

    // Clear visited bit in every BB
    for (bb = starting_bb; bb != NULL; bb = BB_next(bb)) {
      Reset_BB_visited(bb);
    }

    CXX_DELETE (regppg, pool);

    if (trace) {
       fprintf(TFile, "%s\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s End of Register Backward Copy propagation\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n", "<ebo_reg_cp>");
       fprintf(TFile, "%s\n", "<ebo_reg_cp>");
    }
  }

  MEM_POOL_Pop(pool);
  return has_propagated;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
