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


/*
 * This module encapsulates the generation of exception range tables.
 */

#ifdef USE_PCH
#include "cg_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <vector>
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "symtab.h"
#include "wn.h"
#include "irbdata.h"
#include "strtab.h"
#include "stblock.h"
#include "config.h"
#include "config_opt.h"
#include "xstats.h"
#include "eh_region.h"
#include "data_layout.h"
#include "region_util.h"
#include "region_main.h"
#include "bb.h"
#include "whirl2ops.h"
#include "label_util.h"

/* needed for call0 restore regions */
#include "calls.h"
#include "cg_spill.h"
#include "cg.h"

using namespace std;

#define PARAMS(A) A
extern "C" {
#include "../../g++fe/gnu/eh-common.h"
}
#undef PARAMS

/*
 * eh_region.cxx is responsible for building the EH range tables
 * used to implement exception handling.  It does this by building
 * an initialized object for each PU.  The emitter will use these
 * initialized objects to write the tables into the object file.
 * Each range  table consists of a header and an array of ranges,
 * each range specifying the following:
 *   a region supplement pointer
 *   a kind (try-block, cleanup, mask, or exception specification)
 *   a low and high adress (offsets from start of function)
 *   a pointer to the parent range.
 *
 * A table may use either short (16-bit) or long (32-bit) offsets
 * to represent the low and high addresses.  16 bits almost always
 * suffices.  The header specifies which kind of offset is used.
 *
 * Before the initialized object is created, the range table is
 * represented internally by an object of type EH_RANGE_LIST.
 * An EH_RANGE_LIST is implemented using a vector of objects
 * of type EH_RANGE.  An EH_RANGE contains the components which
 * will form the range table, together with a number of fields
 * required during the process of building and modifying the
 * range list.
 *
 * An EH range list is built in a number of stages:
 *
 * An initial list is built before optimization and code generation,
 * by the routine EH_Generate_Range_List.
 * This uses the RID tree for the PU to create a tree of EH ranges,
 * represented as a postorder list.  The tree structure is indicated
 * only by the parent fields.  The tree is the natural subtree of
 * the RID tree formed by the EH regions.  A pointer to each
 * range is put into the corresponding RID, for use during code
 * generation.

 *
 * During code generation, when an EH region is encountered,
 * EH_Set_Start_Label is called at the beginning and EH_Set_EndLabel
 * at the end to set the start_label and end_label fields in the
 * range.  When a call is processed, EH_Set_Has_Call is called
 * for each EH region currently on the region stack, to set the
 * has_call field in the range.  Ranges for which this field is
 * not set will be eliminated later (by EH_Prune_Range_list).
 *
 * Prior to CG optimization, EH_Prune_Range_List is called to
 * eliminate unneeded EH ranges.  This gets rid of ranges which
 * do not have calls or throws.  Such ranges are irrelevant to
 * exception handling.  Eliminating them before CG optimization
 * allows transformations which might otherwise be unsafe.
 *
 * EH_Write_Range_Table does some transformations before
 * generating the INITO for a PU.  First of all, mask ranges
 * need to be eliminated.  This is done by resetting the parent
 * of each mask region to the parent of the nearest ancestor
 * cleanup range.  Then both mask regions and guard regions
 * must be replaced by cleanup regions with a trivial cleanup.
 * (Mask regions cannot be left in the range table because of backwards
 * compatibility requirements).
 *
 * At this point, we need to take account of the fact that CG
 * optimization may have reordered the basic blocks.  The range
 * table needs to be reordered accordingly.
 * 
 * Finally, the INITO is created.
 */

 /* Iterators and function objects related to the RIDs. */

 /* Generation of the range list requires doing a post-order
  * tree walk of the RIDs.  After the tree walk, we need to
  * set the parent fields in the ranges.  These two phases
  * require defining iterators RID_POST_ITER and RID_PARENT_ITER.
  */

 /* We also define a function object IS_EH_RID to identify the
  * EH regions in the RID.  It is important to note that
  * null-cleanup regions are not counted as EH regions.  They
  * are retained during the frontend but eliminated at this
  * point.
  */

class RID_POST_ITER {
private:
  RID * start;
  RID * own;
public:
  typedef forward_iterator_tag iterator_category;
  typedef RID * 	       value_type;
  typedef ptrdiff_t            difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  RID_POST_ITER(RID* p = NULL);
  RID * operator*() const {return own;}
  RID_POST_ITER& operator++();
  RID_POST_ITER  operator++(int);
  friend bool operator==(const RID_POST_ITER&, const RID_POST_ITER&);
  friend bool operator!=(const RID_POST_ITER&, const RID_POST_ITER&);
};

RID_POST_ITER::RID_POST_ITER(RID * p): start(p), own(p) {
  if (own != NULL) {
    while (RID_first_kid(own) != NULL)
      own = RID_first_kid(own);
  }
}

RID_POST_ITER& RID_POST_ITER::operator++()
{
  if (own == start) {
    own = NULL;
  }
  else
  if (RID_next(own) != NULL) {
    own = RID_next(own);
    while (RID_first_kid(own) != NULL)
      own = RID_first_kid(own);
  }
  else
    own = RID_parent(own);

  return *this;
}

RID_POST_ITER RID_POST_ITER::operator++(int)
{
  RID_POST_ITER tmp(*this);
  ++*this;
  return tmp;
}

inline bool operator==(const RID_POST_ITER& x, const RID_POST_ITER & y)
{
  return x.own == y.own;
}

inline bool operator!=(const RID_POST_ITER& x, const RID_POST_ITER & y)
{
  return x.own != y.own;
}

class RID_PARENT_ITER {
private:
  RID * p;
public:
  typedef forward_iterator_tag iterator_category;
  typedef RID * 	       value_type;
  typedef ptrdiff_t	       difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  RID_PARENT_ITER(RID* x = NULL): p(x) {}
  RID * operator*() const {return p;}
  RID_PARENT_ITER& operator++() {p = RID_parent(p); return *this;}
  RID_PARENT_ITER  operator++(int) {
    RID_PARENT_ITER tmp = *this;
    ++*this;
    return tmp;
  }
  friend bool operator==(const RID_PARENT_ITER&, const RID_PARENT_ITER &);
  friend bool operator!=(const RID_PARENT_ITER&, const RID_PARENT_ITER &);

};

bool operator==(const RID_PARENT_ITER& x, const RID_PARENT_ITER& y) {
  return x.p == y.p;
}

bool operator!=(const RID_PARENT_ITER& x, const RID_PARENT_ITER& y) {
  return x.p != y.p;
}

struct IS_EH_RID
{
  bool operator()(const RID* rid) {
    return RID_TYPE_eh(rid);}
};

/* The EH_RANGE and EH_RANGE_LIST classes. */

/* The first three eh_range_kind enumerators correspond to the
 * kinds that appear in the range table.  eh_mask and eh_guard
 * will be replaced by eh_cleanup before the table is created.
 */


enum eh_range_kind {
  ehk_try_block,
  ehk_exc_spec,
  ehk_cleanup,
  ehk_mask,
  ehk_guard,
  ehk_restore_callee,
  ehk_last};

static eh_range_kind Range_Kind(RID * rid)
{
  if (RID_TYPE_try(rid))
    return ehk_try_block;
  if (RID_TYPE_exc_spec(rid))
    return ehk_exc_spec;
  if (RID_TYPE_cleanup(rid))
    return ehk_cleanup;
  if (RID_TYPE_null_cleanup(rid))
    return ehk_cleanup;
  if (RID_TYPE_mask(rid))
    return ehk_mask;
  if (RID_TYPE_guard(rid))
    return ehk_guard;
  if (RID_TYPE_func_entry(rid))
    return ehk_restore_callee;
  return ehk_last;
}

struct EH_RANGE {
// components of range table
  INITO_IDX    	ereg_supp;
  LABEL_IDX     start_label;
  LABEL_IDX     end_label;
  EH_RANGE      *parent;
  eh_range_kind kind;

// bookkeeping items
  RID *		rid;		// for setting parents
  BB		*end_bb;	// for sorting
  EH_RANGE*	id;		// for sorting
  INT32		key;		// for sorting
  INT32		adjustment;	// to adjust parents after compression
  bool		has_call;	// ranges without calls get deleted

// constructor
  EH_RANGE(RID * x):
    ereg_supp(WN_ereg_supp(RID_rwn(x))),
    start_label(LABEL_IDX_ZERO),
    end_label(LABEL_IDX_ZERO),
    parent(NULL),
    kind(Range_Kind(x)),
    rid(x),
    end_bb(NULL),
    id(this),
    key(0),
    adjustment(0),
    has_call(false) {}
};

/* an EH_RANGE_LIST is essentially just a vector of EH_RANGE.  We
 * make it a separate type because it has a tree structure
 * and we want to define a parent iterator on it.
 */

class EH_RANGE_LIST {
private:
  vector<EH_RANGE> v;
public:
  EH_RANGE_LIST(): v() {}
  void add_range(EH_RANGE range) {
    v.push_back(range);
  }
  void clear() {v.clear();}
  size_t size() {return v.size();}
  EH_RANGE& operator[](size_t i) {return v[i];}
  typedef vector<EH_RANGE>::iterator 	     iterator;
  typedef vector<EH_RANGE>::reverse_iterator reverse_iterator;
  iterator begin() {return v.begin();}
  iterator end()   {return v.end();}
  reverse_iterator rbegin() {return v.rbegin();}
  reverse_iterator rend()   {return v.rend();}
  iterator erase(iterator first, iterator last) {
    return v.erase(first, last);}
};

class EH_RANGE_LIST_PARENT_ITER {
private:
  EH_RANGE_LIST::iterator iter;
public:
  typedef forward_iterator_tag iterator_category;
  typedef EH_RANGE             value_type;
  typedef ptrdiff_t            difference_type;
  typedef value_type *         pointer;
  typedef value_type &         reference;
  EH_RANGE_LIST_PARENT_ITER(): iter() {}
  EH_RANGE_LIST_PARENT_ITER(EH_RANGE_LIST::iterator x): iter(x) {}
  EH_RANGE& operator*() {return *iter;}
  EH_RANGE_LIST_PARENT_ITER& operator++() {
    iter = EH_RANGE_LIST::iterator(iter->parent);
    return *this;
  }
  EH_RANGE_LIST_PARENT_ITER operator++(int) {
    EH_RANGE_LIST_PARENT_ITER tmp = *this;
    ++*this;
    return tmp;
  }
  friend bool operator==(const EH_RANGE_LIST_PARENT_ITER&,
			 const EH_RANGE_LIST_PARENT_ITER&);
  friend bool operator!=(const EH_RANGE_LIST_PARENT_ITER&,
			 const EH_RANGE_LIST_PARENT_ITER&);
};

inline bool operator==(const EH_RANGE_LIST_PARENT_ITER & x,
		       const EH_RANGE_LIST_PARENT_ITER &y) {
  return x.iter == y.iter;
}

inline bool operator!=(const EH_RANGE_LIST_PARENT_ITER & x,
		       const EH_RANGE_LIST_PARENT_ITER &y) {
  return x.iter != y.iter;
}

/* There is always just one EH_RANGE_LIST which belongs to
 * eh_region.cxx and which is cleared at the beginning of
 * EH_Generate_Range_List.
 */

static INT64 EH_Size(xtensa_eh_types type);
static enum xtensa_eh_types Pick_EH_Type(INT64 frame_size);

BOOL need_restore_region;
static EH_RANGE_LIST range_list;

/* We define a function object ADD_EH_RANGE to be passed to
 * for_each as we iterate over the RID using RID_POST_ITER.
 * This will add a range for every EH region.
 */

struct ADD_EH_RANGE {
  void operator()(RID * rid) {
    if (RID_TYPE_eh(rid)) {
      FmtAssert(CXX_Exceptions_On, ("exception range unexpected when exceptions disabled."));
      range_list.add_range(rid);
    }
  }
};

/* The function object SET_PARENT finds the nearest ancestor 
 * EH region and sets the parent accordingly.
 */

struct SET_PARENT {
  void operator()(EH_RANGE& r) {
    RID_PARENT_ITER first(r.rid);
    RID_PARENT_ITER last(NULL);
    first = find_if(++first, last, IS_EH_RID());
    if (first == last)
      r.parent = NULL;
    else
      r.parent = RID_eh_range_ptr(*first);
  }
};

/* EH_Generate_Range_List does a post-order RID walk to create
 * the range list, then iterates over the range list to set the
 * parent fields.
 */
 
void
EH_Generate_Range_List(WN * pu)
{
  need_restore_region = false;
  range_list.clear();

  RID * rid = (RID *) WN_MAP_Get(RID_map, pu);
  RID_POST_ITER rid_first(rid);
  RID_POST_ITER rid_last(NULL);

  for_each(rid_first, rid_last, ADD_EH_RANGE());

  EH_RANGE_LIST::iterator list_first(range_list.begin());
  EH_RANGE_LIST::iterator list_last (range_list.end());

  for (EH_RANGE_LIST::iterator p = list_first; p!=list_last; p++)
    RID_eh_range_ptr(p->rid) = &(*p);

  for_each(list_first, list_last, SET_PARENT());
}


/* Normally EH_Set_Start_Label just creates a label and an
 * associated basic block and sets the start_label field of
 * the designated range accordingly.  This simple picture is
 * complicated by the requirements of guard ranges.  A guard
 * range is required before every mask range to fill up the
 * unused space in the enclosing cleanup region:  otherwise
 * binary search can be foiled and incorrectly attribute an
 * address to a mask region.  So we don't create a new label
 * for a guard range:  instead we use the end label of the
 * elder sibling, if any, and otherwise the start label of the
 * enclosing range.  The function object IS_SIB_RANGE is
 * used to search for the elder sibling.
 */

struct IS_SIB_RANGE {
  const EH_RANGE* me;
  IS_SIB_RANGE(const EH_RANGE * x): me(x) {}
  bool operator()(const EH_RANGE& r) const {
    return (r.rid != me->rid)       &&
           (r.parent == me->parent) &&
           (r.end_label != (LABEL_IDX)NULL);
  }
};


static LABEL_IDX Duplicate_LABEL (LABEL_IDX oldi)
{
	LABEL_IDX lbi;
	LABEL& lab = New_LABEL (CURRENT_SYMTAB, lbi);
	LABEL old = Label_Table[oldi];
	Set_LABEL_name_idx(lab, Save_Str2(LABEL_name(old), ".dup"));
	Set_LABEL_kind(lab, LABEL_kind(old));
	return lbi;
}
void
EH_Set_Start_Label(EH_RANGE* p)
{
  LABEL_IDX label;
  if (p->kind == ehk_guard) {
    EH_RANGE_LIST::reverse_iterator rfirst = range_list.rbegin();
    while (&(*rfirst) != p)
      rfirst++;

    EH_RANGE_LIST::reverse_iterator rlast  = range_list.rend();
    EH_RANGE_LIST::reverse_iterator riter =
      find_if(rfirst, rlast, IS_SIB_RANGE(p));
    if (riter == rlast) {
      if (p->parent != NULL) {
	label = Duplicate_LABEL(p->parent->start_label);
     	Set_Label_BB(label, NULL);
      }
    }

    else {
      label = Duplicate_LABEL(riter->end_label);
      Set_Label_BB(label, NULL);
      Set_LABEL_kind(Label_Table[label], LKIND_DEFAULT);
      Set_LABEL_begin_eh_range(label);
    }
    Add_Label(label);
  }

  else {
    label   = Gen_Temp_Label();
    BB * bb = Add_Label(label);
    Set_LABEL_begin_eh_range(label);
  }

  p->start_label = label;
}

/* No complications with EH_Set_End_Label. */

void
EH_Set_End_Label(EH_RANGE* p)
{
  LABEL_IDX label;
  label = Gen_Temp_Label();
  BB * bb    = Add_Label(label);
  p->end_label = label;
  p->end_bb    = bb;
  Set_LABEL_end_eh_range(label);
}

/* Guard regions also complicate EH_Set_Has_Call.  We don't want
 * to eliminate guard regions when they are required by a mask
 * region.  Therefore when we find a call in a mask region, we
 * set has_call in the associated guard region as well.
 */


void EH_Set_Has_Call(EH_RANGE* p)
{
  p->has_call = TRUE;
  if (p->kind == ehk_mask) {
    // set has_call for associated guard region also
    EH_RANGE_LIST::reverse_iterator rfirst = range_list.rbegin();
    while (&(*rfirst) != p)
      rfirst++;

    EH_RANGE_LIST::reverse_iterator rlast  = range_list.rend();
    rfirst = find_if(rfirst, rlast, IS_SIB_RANGE(p));
    Is_True(rfirst != rlast && rfirst->kind == ehk_guard,
		      ("mask region must have guard"));
    rfirst->has_call = TRUE;
  }    
}

/*
 * EH_Prune_Range_list has four phases:
 *  (1) The adjustment field of each range is set to the number
 *      of ranges prior to this one which contain no call and
 *      will therefore be eliminated.
 *
 *  (2) The adjustment field of each range is replace by the
 *	adjustment of its parent.
 *
 *  (3) The ranges with no call are eliminated.
 *
 *  (4) The parents are adjusted.
 *
 */


struct HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL {
  bool operator()(const EH_RANGE& r) {
    if (Inhibit_EH_opt) return false;
    if (r.has_call && r.start_label != (LABEL_IDX)NULL) {
      BB *start_bb = Get_Label_BB(r.start_label);
      if (start_bb && !BB_unreachable(start_bb)) return false;
    }
    return true;
    }
};

struct SET_ADJUSTMENT {
  INT32 amount;
  SET_ADJUSTMENT(): amount(0) {}
  void operator()(EH_RANGE& r) {
    if (HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()(r)) {
      ++amount;
      if (r.start_label != (LABEL_IDX) NULL)
        Set_LABEL_kind(Label_Table[r.start_label], LKIND_DEFAULT);
      if (r.end_label != (LABEL_IDX) NULL) 
        Set_LABEL_kind(Label_Table[r.end_label], LKIND_DEFAULT);
      if (r.ereg_supp)
	Set_ST_is_not_used(INITO_st(r.ereg_supp));
    }
    r.adjustment = amount;
  }
};

struct CLEAR_USED {
  CLEAR_USED() {}
  void operator()(EH_RANGE& r) {
#ifndef TARG_XTENSA
    /* For xtensa, we don't want to emit 'ereg_supp', it is used only
       to pass information internally from the front-end. */
    if (!HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()(r))
      if (r.ereg_supp)
	Clear_ST_is_not_used(INITO_st(r.ereg_supp));
#endif
  }
};

struct SET_ADJUSTMENT_TO_PARENT_ADJUSTMENT {
  void operator()(EH_RANGE & r) {
    if (r.parent == NULL) {
      r.adjustment = 0;
    }
    else {
      r.adjustment = r.parent->adjustment;
    }
  }
};

struct ADJUST_PARENT {
  void operator()(EH_RANGE&r) {r.parent -= r.adjustment;}
};

void
EH_Prune_Range_List(void)
{
  EH_RANGE_LIST::iterator first(range_list.begin());
  EH_RANGE_LIST::iterator last(range_list.end());
  if (first == last) return;
  if (!PU_has_exc_scopes(Get_Current_PU()) && 
      !Inhibit_EH_opt) {
    range_list.erase(first, last);
    return;
  }

  for_each  (first, last, SET_ADJUSTMENT());
  for_each  (first, last, CLEAR_USED());
  for_each  (first, last, SET_ADJUSTMENT_TO_PARENT_ADJUSTMENT());
  range_list.erase(
    remove_if (first, last, 
               HAS_NO_CALL_OR_HAS_NULL_OR_UNREACHABLE_LABEL()), 
    last);
  for_each  (range_list.begin(), range_list.end(), ADJUST_PARENT());
} 

struct COMPARE_RANGES {
  bool operator()(const EH_RANGE& r1, const EH_RANGE& r2) {
    if (r1.key < r2.key) return true;
    if (r1.key > r2.key) return false;
    return r1.id < r2.id;
  }
};

void
reorder_range_list()
{
  BB * bb;
  INT32 bb_count;
  size_t i;

  for (bb = REGION_First_BB, bb_count = 0;
       bb != NULL;
       bb = BB_next(bb), ++bb_count) {
    for (i = 0; i < range_list.size(); ++i) {
      if (range_list[i].end_bb == bb)
	range_list[i].key = bb_count;
      range_list[i].id = &range_list[i];
    }
  }

  EH_RANGE_LIST::iterator first(range_list.begin());
  EH_RANGE_LIST::iterator last (range_list.end());

  stable_sort(first, last, COMPARE_RANGES());

  // reset parent pointers using inverse vector

  vector<int> inv(range_list.size());
  for (i = 0; i < range_list.size(); ++i)
    inv[range_list[i].id - &range_list[0]] = i;

  for (i = 0; i < range_list.size(); ++i) {
    if (range_list[i].parent != NULL) {
      range_list[i].parent = &range_list[0] + 
			     inv[range_list[i].parent - &range_list[0]];
    }
  }
}

struct IS_CLEANUP_RANGE {
  bool operator()(const EH_RANGE& r) const {return r.kind == ehk_cleanup;}
};

struct FIX_MASK_PARENT {
  void operator()(EH_RANGE& r) {
    if (r.kind == ehk_mask) {
      EH_RANGE_LIST_PARENT_ITER first(EH_RANGE_LIST::iterator(r.parent));
      EH_RANGE_LIST_PARENT_ITER last (EH_RANGE_LIST::iterator(NULL));
      first = find_if(first, last, IS_CLEANUP_RANGE());
      Is_True(first != last, ("mask region must have cleanup ancestor"));
      r.parent = (*first).parent;
    }
  }
};

struct CHANGE_MASK_OR_GUARD_TO_CLEANUP {
  void operator()(EH_RANGE& r) {
    if (r.kind == ehk_mask || r.kind == ehk_guard)
      r.kind = ehk_cleanup;
    }
};

static void
fix_mask_ranges(void)
{
  /*
   * For mask regions the parent pointers need to be readjusted.
   * This needs to be done from the outside in, so we traverse the
   * range table in reverse.  For every mask region, we follow the 
   * parent pointers till we encounter a cleanup region, then set
   * the parent to the parent of that cleanup region. Then we make
   * a second pass and replace eh_mask by eh_cleanup.
   */

  EH_RANGE_LIST::reverse_iterator rfirst(range_list.rbegin());
  EH_RANGE_LIST::reverse_iterator rlast (range_list.rend());

  for_each(rfirst, rlast, FIX_MASK_PARENT());
  for_each(range_list.begin(), range_list.end(),
	   CHANGE_MASK_OR_GUARD_TO_CLEANUP());
}

static ST * eh_pu_range_st;
static ST * eh_pu_desc_st;

extern ST* EH_Get_PU_Range_ST(void)
{
  return eh_pu_range_st;
}

extern ST* EH_Get_PU_Desc_ST(void)
{
  return eh_pu_desc_st;
}

inline BOOL Use_Long_EH_Range_Offsets(void)
{
  return Force_Long_EH_Range_Offsets ||
	 PU_WN_BB_Cnt + PU_WN_Stmt_Cnt > 2000;
}


static INT32
Int_For_Initv (INITV_IDX initv)
{
  if (initv)
  {
    switch (INITV_kind(initv))
    {
    case INITVKIND_ZERO:
      return 0;
      
    case INITVKIND_ONE:
      return 1;
      
    case INITVKIND_VAL:
      TCON tcon = INITV_tc_val(Initv_Table[initv]);
      return Targ_To_Host(tcon);
    }
  }
  
  FmtAssert(FALSE, ("Unexpected initv kind in Int_For_Initv()"));
  return 0;
}

static ST*
ST_For_Range_Table(WN * wn)
{
  ST * pu = WN_st(wn);
  ST * st;

  // terminator_size + number_of_ranges * exception_table entry size
  // exception_table: size of
  //   { the structure "exception_table" as defined in eh-common.h. }
  //
  // terminator_size: size of
  //    one integer           (-1 indicates end of table for function)
  UINT32 terminator_size = 4;
  UINT32 size = 0;

  UINT32 number_of_ranges = 0;
  for (INT32 i = 0; i < range_list.size(); i++) {
    FmtAssert(range_list[i].ereg_supp, ("expecting exception range information"));
    INITV_IDX inv = INITO_val(range_list[i].ereg_supp);
    int number_of_ranges = Int_For_Initv(inv); 
    if (range_list[i].kind == ehk_restore_callee) {
      FmtAssert(number_of_ranges == 1, ("only one range allowed for restore callee"));
      FmtAssert(Target_ABI==ABI_CALL0, ("restore regs range for windowed abi?"));
      // EH_Size includes the terminator size, but that will get added on
      // the end
      size += EH_Size(Pick_EH_Type(Frame_Len)) - terminator_size;
    }
    else
      size += number_of_ranges * ((Pointer_Size * 4) + 4);
  }
  
  size += terminator_size;

  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, size, KIND_STRUCT, MTYPE_M,
	  Save_Str2(".eh_table.",ST_name(pu)));
  Set_TY_align(tyi, 4);
  st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, TY_name_idx(ty),
	  CLASS_VAR, SCLASS_EH_REGION, EXPORT_LOCAL, tyi);
  Set_ST_is_initialized(st);
  Set_ST_is_const_var(st);

  /* If this function is being emitted to a linkonce section, then
     emit it's eh table to a corresponding linkonce section. */
  if (ST_has_named_section(pu))
  {
    /* Make sure this matches what is used in cgemit_targ.cxx (with
       "t" changed to "e"). */
#define LINKONCE_PREFIX ".gnu.linkonce.t."
    
    STR_IDX pu_sec_name_idx = Find_Section_Name_For_ST(pu);
    char *pu_sec_name = Index_To_Str(pu_sec_name_idx);
    if (pu_sec_name && !strncmp(pu_sec_name, LINKONCE_PREFIX, strlen(LINKONCE_PREFIX)))
    {
      char eh_sec_name[strlen(pu_sec_name) + 1];
      strcpy(eh_sec_name, pu_sec_name);
      eh_sec_name[strlen(LINKONCE_PREFIX) - 2] = 'e';

      ST_ATTR_IDX st_attr_idx;
      ST_ATTR& st_attr = New_ST_ATTR(CURRENT_SYMTAB, st_attr_idx);
      ST_ATTR_Init(st_attr, ST_st_idx(st), ST_ATTR_SECTION_NAME,
		   Save_Str(eh_sec_name));
      Set_ST_has_named_section (st);
    }
  }

  Allocate_Object(st);
  return st;
}

static ST*
ST_For_Desc_Table(WN * wn)
{
  ST * pu = WN_st(wn);
  ST * st;

  // header_size: size of
  //    one pointer           (regions start addr)
  //    one pointer           (regions end addr)
  //    one pointer           (region table addr)
  UINT32 size = Pointer_Size * 3;

  TY_IDX tyi;
  TY& ty = New_TY(tyi);
  TY_Init(ty, size, KIND_STRUCT, MTYPE_M,
	  Save_Str2(".eh_desc.",ST_name(pu)));
  Set_TY_align(tyi, 4);
  st = New_ST(CURRENT_SYMTAB);
  ST_Init(st, TY_name_idx(ty),
	  CLASS_VAR, SCLASS_EH_DESC, EXPORT_LOCAL, tyi);
  Set_ST_is_initialized(st);
  Set_ST_is_const_var(st);

  /* If this function is being emitted to a linkonce section, then
     emit it's eh desc to a corresponding linkonce section. */
  if (ST_has_named_section(pu))
  {
    /* Make sure this matches what is used in cgemit_targ.cxx (with
       "t" changed to "h"). */
#define LINKONCE_PREFIX ".gnu.linkonce.t."
    
    STR_IDX pu_sec_name_idx = Find_Section_Name_For_ST(pu);
    char *pu_sec_name = Index_To_Str(pu_sec_name_idx);
    if (pu_sec_name && !strncmp(pu_sec_name, LINKONCE_PREFIX, strlen(LINKONCE_PREFIX)))
    {
      char eh_sec_name[strlen(pu_sec_name) + 1];
      strcpy(eh_sec_name, pu_sec_name);
      eh_sec_name[strlen(LINKONCE_PREFIX) - 2] = 'h';

      ST_ATTR_IDX st_attr_idx;
      ST_ATTR& st_attr = New_ST_ATTR(CURRENT_SYMTAB, st_attr_idx);
      ST_ATTR_Init(st_attr, ST_st_idx(st), ST_ATTR_SECTION_NAME,
		   Save_Str(eh_sec_name));
      Set_ST_has_named_section (st);
    }
  }

  Allocate_Object(st);
  return st;
}

#define SHORT_OFFSETS  0
#define LONG_OFFSETS   1
#define HEADER_VERSION 1

inline INT16 parent_offset(INT32 i)
{
  if (range_list[i].parent == NULL)
    return 0;
  else
    return (INT16) (range_list[i].parent - &range_list[i]);
}


static LABEL_IDX
Find_Label_Kind (BB *bb, LABEL_KIND kind)
{
  for (ANNOTATION *ant = ANNOT_First(BB_annotations(bb), ANNOT_LABEL);
       ant != NULL;
       ant = ANNOT_Next(ant, ANNOT_LABEL))
  {
    LABEL_IDX lab = ANNOT_label(ant);
    if (LABEL_kind(Label_Table[lab]) == kind)
      return lab;
  }

  return LABEL_IDX_ZERO;
}


/* Return the start and end labels that bound all exception regions in
   the current PU. */
static void
Find_Outer_Region_Labels (ST *pu, LABEL_IDX *start, LABEL_IDX *end)
{
  BB *start_bb = NULL, *end_bb = NULL;

  for (BB *bb = REGION_First_BB; bb != NULL; bb = BB_next(bb))
  {
    if (BB_Has_Exc_Label(bb))
    {
      if (!start_bb)
	start_bb = bb;
      else
	end_bb =bb;
    }
  }

  FmtAssert(start_bb && end_bb,
	    ("unable to find exception region bounds for %s\n",
	     ST_name(pu)));
  
  /* Find the labels marked as exception region start/end in
     'start_bb'/'end_bb'. */
  *start = Find_Label_Kind(start_bb, LKIND_BEGIN_EH_RANGE);
  *end = Find_Label_Kind(end_bb, LKIND_END_EH_RANGE);

  FmtAssert(*start && *end,
	    ("unable to find exception region bounds labels for %s\n",
	     ST_name(pu)));
}


/* Copy INITV 'block' and insert the copy after 'prev_inv'. */
static void
Copy_INITV_Block(INITV_IDX block, INITV_IDX prev_inv)
{
  FmtAssert(INITV_kind(block) == INITVKIND_BLOCK, ("expecting INITV block"));
  block = INITV_blk(block);
  
  while (block) {
    FmtAssert(INITV_kind(block) != INITVKIND_BLOCK, ("unexpected INITV block"));

    INITV_IDX inv = Copy_INITV(INITV_IDX_ZERO, INITO_IDX_ZERO, block);
    prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
    block = INITV_next(block);
  }
}


static void
Create_INITO_For_Range_Table(ST * st, ST * pu)
{
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv_blk = INITV_IDX_ZERO;
  INITV_IDX inv, prev_inv;

  for (INT32 i = 0; i < range_list.size(); i++) {
    /* For each region we may need to output multiple entries. For try
       regions, each entry has the same start and end label with
       different handler label and rtti information. For catch and
       cleanup regions, there is "this" pointer and destructor. */
    INITV_IDX ehinfo_first = INITO_val(range_list[i].ereg_supp);
    INITV_IDX ehinfo_next, ehinfo_inv = ehinfo_first;
    Int_For_Initv(ehinfo_inv);  // just to check it is valid
    ehinfo_inv = INITV_next(ehinfo_inv);
    FmtAssert(ehinfo_inv, ("expecting at least one eh info block per region"));

    while (ehinfo_inv)
    {
      /* 'ehinfo_inv' should be a block containing the EH type and
         then any associated data. We copy it from where it is and
         insert it into 'inito'. */
      FmtAssert(INITV_kind(ehinfo_inv) == INITVKIND_BLOCK, ("expecting eh info block"));
      ehinfo_next = INITV_next(ehinfo_inv);
      
      /* block for each range */
      inv_blk = Append_INITV (New_INITV (),
			      ((inv_blk == INITV_IDX_ZERO) ? inito : INITO_IDX_ZERO),
			      inv_blk);

      // start(32), end(32), type(32), data(32), data(32) + call0abi ? data(32) : 0
      inv = New_INITV();
      INITV_Init_Label (inv, range_list[i].start_label, 1);
      INITV_Init_Block (inv_blk, inv);
      prev_inv = inv;
      inv = New_INITV();
      INITV_Init_Label (inv, range_list[i].end_label, 1);
      prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);
      Copy_INITV_Block(ehinfo_inv, prev_inv);

      ehinfo_inv = ehinfo_next;
    }
  }

  /* Terminate the table with -1. */
  inv = New_INITV ();
  INITV_Init_Integer(inv, MTYPE_U4, -1); 
  Append_INITV(inv, INITO_IDX_ZERO, inv_blk);
}

static void
Create_INITO_For_Desc_Table(ST *st, ST *pu, ST *region_table)
{
  INITO_IDX inito = New_INITO(st);
  INITV_IDX inv = New_INITV ();
  INITV_IDX prev_inv, inv_blk;
  
  // descriptor: regions_start_addr(32), regions_end_addr(32),
  //             region_table_addr(32)               
  inv_blk = Append_INITV (inv, inito, INITV_IDX_ZERO);
  inv = New_INITV ();
  INITV_Init_Block(inv_blk, inv);

  LABEL_IDX regions_start_label_idx, regions_end_label_idx;
  Find_Outer_Region_Labels(pu, &regions_start_label_idx, &regions_end_label_idx);
  INITV_Init_Label(inv, regions_start_label_idx); 

  prev_inv = inv;
  inv = New_INITV ();
  INITV_Init_Label(inv, regions_end_label_idx);
  prev_inv = Append_INITV (inv, INITO_IDX_ZERO, prev_inv);

  inv = New_INITV ();
  INITV_Init_Symoff(inv, region_table, 0);
  Append_INITV (inv, INITO_IDX_ZERO, prev_inv);
}

void 
EH_Write_Range_Table(WN * wn)
{
  if (range_list.size() == 0) {
    eh_pu_range_st = NULL;
    eh_pu_desc_st = NULL;
    return;
  }
  fix_mask_ranges();
  reorder_range_list();

  ST * range_st = ST_For_Range_Table(wn);
  eh_pu_range_st = range_st;
  Create_INITO_For_Range_Table(range_st, WN_st(wn));  

  ST * desc_st = ST_For_Desc_Table(wn);
  eh_pu_desc_st = desc_st;
  Create_INITO_For_Desc_Table(desc_st, WN_st(wn), range_st);  
}

void print_label(LABEL_IDX label)
{
  Label_Table[label].Print(stderr);
}


/* Below here is all call0. Much of this code is duplicated from or similar to
   xtensa_eh.cc. Keep the two in sync. */

BOOL EH_Need_EH_Table()
{
  if (PU_has_exc_scopes(Get_Current_PU()) || need_restore_region)
    return true;
  else
    return false;
}


#define ET_RESTORE_REG_SMALL_SIZE 20
#define ET_RESTORE_REG_MED_SIZE 28
#define ET_RESTORE_REG_LARGE_SIZE 40
#define ALLOCA_CALLEE_SAVED_REG_COUNT 3
/* can't use MAX_INT and such here because these are target
   dependent values */
#define MAX_UNSIGNED_CHAR_ENCODED (0xFF << 2)
#define MAX_UNSIGNED_SHORT_ENCODED (0xFFFF << 2)


static enum xtensa_eh_types Pick_EH_Type(INT64 frame_size)
{
  bool lp_rel = PU_has_alloca(Get_Current_PU());
  if (frame_size <= MAX_UNSIGNED_CHAR_ENCODED)
    return lp_rel ? XT_EH_TYPE_RESTORE_CALLEE_LPREL : XT_EH_TYPE_RESTORE_CALLEE_SPREL;
  if (frame_size <= MAX_UNSIGNED_SHORT_ENCODED)
    return lp_rel ? XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED : XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE;
  else 
    return lp_rel ? XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE : XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE;  
}

static INT64 EH_Size(xtensa_eh_types type)
{
  switch (type) 
  {
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL:
    return ET_RESTORE_REG_SMALL_SIZE;
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED:
    return ET_RESTORE_REG_MED_SIZE;
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE:
    return ET_RESTORE_REG_LARGE_SIZE;
  }
  FmtAssert(0, ("Unkown eh region type"));
}

static TYPE_ID EH_MType(xtensa_eh_types type)
{
  switch (type) 
  {
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL:
    return MTYPE_U1;
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED:
    return MTYPE_U2;
  case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE:
  case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE:
    return MTYPE_U4;
  }
  FmtAssert(0, ("Unkown eh region type"));
}

static uint32 Shift_And_Mask_Mtype_Aligned_4(uint32 val, TYPE_ID mtype)
{
  FmtAssert((val & 0x3) == 0, ("Storing a register in an unaligned location"));
  val >>= 2;
  if (mtype == MTYPE_U1)
    FmtAssert((val & 0xFFFFFF00) == 0, ("Value too large for unsigned char"));
  else if (mtype == MTYPE_U2)
    FmtAssert((val & 0xFFFF0000) == 0, ("Value too large for unsigned short"));
  /* everything fits in an unsigned long */
  return val;
}


void EH_Build_Restore_Callee_EH_Region(WN * pu)
{
  Is_True((Target_ABI == ABI_CALL0) && (CXX_Exceptions_On), ("No need to build callee restore region"));
  need_restore_region = true;
  /* the register allocator has it at physical reg + 1 */
  Is_True ((Callee_Saved_Regs_Count == 4) || (Callee_Saved_Regs_Count == ALLOCA_CALLEE_SAVED_REG_COUNT), ("Call0 abi restore regs don't match runtime. %d != 4", Callee_Saved_Regs_Count));
  Is_True(TN_register(CALLEE_ded_tn(0)) == 13, ("broken callee saved register list"));
  Is_True(TN_register(CALLEE_ded_tn(1)) == 14, ("broken callee saved register list"));
  Is_True(TN_register(CALLEE_ded_tn(2)) == 15, ("broken callee saved register list"));
  Is_True(Callee_Saved_Regs_Count == ALLOCA_CALLEE_SAVED_REG_COUNT || TN_register(CALLEE_ded_tn(3)) == 16, ("broken callee saved register list"));

  /* we need to restore callee saved regs if an exception
     can be thrown by this PU, or if an exception
     can be thrown _through_ this PU. */
  if (!PU_Has_Calls && !PU_has_exc_scopes(Get_Current_PU()))
    return;

  /* To allow compiling C code not as C++ but still allow -fexceptions,
     we don't mark the pu as having an exc scope even though the restore
     region really is one.
  */

  //Set_PU_has_exc_scopes(Get_Current_PU());

  /* finally. We know we need it. Build it and go home */
  EH_RANGE restore_range(REGION_get_rid(pu));
  
  /* we don't want to optimize this range  away, but the rest
     of the code will if has_call is false. */
  restore_range.has_call = true;
  restore_range.start_label = Gen_Label_For_BB(REGION_First_BB);
  restore_range.parent = NULL;
  Set_LABEL_begin_eh_range(restore_range.start_label);

  /* end_region = end of function. */
  BB *last_bb = REGION_First_BB;
  while (BB_next(last_bb) != NULL)
    last_bb = BB_next(last_bb);
  /* make sure we have the very last lexical block in the function */
  last_bb = Gen_And_Append_BB(last_bb);
  restore_range.end_label = Gen_Label_For_BB(last_bb);
  Set_LABEL_end_eh_range(restore_range.end_label);
  
  /* now build the inito */

  /* Frame size governs the choice of small, medium or large */
  xtensa_eh_types type = Pick_EH_Type(Frame_Len);
  TYPE_ID mtype = EH_MType(type);

  /* build the type */
  TY_IDX table_ty_idx;
  TY& ty = New_TY(table_ty_idx);
  TY_Init(ty, EH_Size(type), KIND_STRUCT, MTYPE_M,
	  Save_Str2(".eh_table.callee_reg_restore.", ST_name(Get_Current_PU_ST())));
  Set_TY_align(table_ty_idx, 4);

  /* Build the variable */
  ST *table_entry = New_ST(CURRENT_SYMTAB);
  ST_Init(table_entry, TY_name_idx(table_ty_idx),
	  CLASS_VAR, SCLASS_EH_REGION_SUPP, EXPORT_LOCAL, table_ty_idx);
  Set_ST_is_initialized(table_entry);
  Set_ST_is_not_used(table_entry);

  /* This region contains one entry. */
  INITO_IDX inito = New_INITO(table_entry);
  INITV_IDX inv = New_INITV();
  INITV_Init_Integer(inv, MTYPE_U4, 1 );
  INITV_IDX prev_inv = Append_INITV(inv, inito, INITV_IDX_ZERO);
  restore_range.ereg_supp = inito;

  /* the entry is a block */
  INITV_IDX block = New_INITV();
  inv = New_INITV();
  INITV_Init_Block(block, inv);
  Append_INITV(block, INITO_IDX_ZERO, prev_inv);

  /* first entry in the block is the type */
  INITV_Init_Integer(inv, MTYPE_U2, type);
  prev_inv = inv;
  /* now pad out as necessary */
  if ((type == XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE) ||
      (type == XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE))
  {
    INITV_IDX reg_field = New_INITV();
    INITV_Init_Integer(reg_field, MTYPE_U2, 0);
    prev_inv = Append_INITV(reg_field, INITO_IDX_ZERO, prev_inv);
  }

  /* now the frame size */
  inv = New_INITV();
  uint32 encoded = Shift_And_Mask_Mtype_Aligned_4(Frame_Len, mtype);
  INITV_Init_Integer(inv, mtype, encoded);
  prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);

  /* now the ra offset */
  inv = New_INITV();
  TN * ra_save_tn = SAVE_tn(Return_Address_Reg);
  ST * ra_loc = CGSPILL_Get_TN_Spill_Location(ra_save_tn, CGSPILL_GRA);
  ST * base;
  INT64 ra_offset;
  Base_Symbol_And_Offset_For_Addressing(ra_loc, 0, &base, &ra_offset);
  encoded = Shift_And_Mask_Mtype_Aligned_4(ra_offset, mtype);
  INITV_Init_Integer(inv, mtype, encoded);
  prev_inv = Append_INITV(inv, INITO_IDX_ZERO, prev_inv);

  bool lp_rel = PU_has_alloca(Get_Current_PU());
  /* now the offsets to the callee saved regs */
  for (int reg = 0; reg < Callee_Saved_Regs_Count; reg++) {
    ST * base;
    INT64 offset = 0;
    if (TN_spill(CALLEE_tn(reg))) {
      ST * spill_loc = CGSPILL_Get_TN_Spill_Location(CALLEE_tn(reg), CGSPILL_GRA);
      Base_Symbol_And_Offset_For_Addressing(spill_loc, 0, &base, &offset);
      FmtAssert(base == SP_Sym|| (lp_rel && base == FP_Sym), ("unknown stack relative reference in exception restore handler"));
    }
    encoded = Shift_And_Mask_Mtype_Aligned_4(offset, mtype);
    INITV_IDX reg_field = New_INITV();
    INITV_Init_Integer(reg_field, mtype, encoded);
    prev_inv = Append_INITV(reg_field, INITO_IDX_ZERO, prev_inv);
  }
  if (Callee_Saved_Regs_Count == ALLOCA_CALLEE_SAVED_REG_COUNT) {
    ST * base;
    INT64 offset = 0;
    ST * spill_loc = CGSPILL_Get_TN_Spill_Location(Caller_FP_TN, CGSPILL_GRA);
    Base_Symbol_And_Offset_For_Addressing(spill_loc, 0, &base, &offset);
    FmtAssert(base == SP_Sym|| (lp_rel && base == FP_Sym), ("unknown stack relative reference in exception restore handler"));
    encoded = Shift_And_Mask_Mtype_Aligned_4(offset, mtype);
    INITV_IDX frame_field = New_INITV();
    INITV_Init_Integer(frame_field, mtype, encoded);
    prev_inv = Append_INITV(frame_field, INITO_IDX_ZERO, prev_inv);

    /* the LP is guaranteed to be saved in the prolog */
  }

  inv = INITO_val(restore_range.ereg_supp);
  range_list.add_range(restore_range);

  /* there is probably a better way than redoing this entire bit
     but I don't know what it is at the moment, and the expected number
     of exeption ranges is less than three or so. */
  {
    EH_RANGE_LIST::iterator list_first(range_list.begin());
    EH_RANGE_LIST::iterator list_last (range_list.end());
    
    for (EH_RANGE_LIST::iterator p = list_first; p!=list_last; p++)
      RID_eh_range_ptr(p->rid) = &(*p);
    
    for_each(list_first, list_last, SET_PARENT());
  }

}



// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
