
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
 * Module: betarget.cxx
 * $Revision: 1.20 $
 * $Date: 2000/04/06 02:02:13 $
 * $Author: mtibuild $
 * $Source: /osprey.src/osprey1.0/be/com/ia64/RCS/betarget.cxx,v $
 *
 * Description:
 *
 * Support routines for target-specific functionality.
 *
 * ====================================================================
 * ====================================================================
 */

#include <alloca.h>
#include "defs.h"
#include "errors.h"
#include "util.h"
#include "tracing.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "wn.h"
#include "opcode.h"
#include "config_targ.h"
#include "config_targ_options.h"
#include "config_opt.h"
#include "betarget.h"
#include "w2op.h"
#include "be_symtab.h"
#include "lwn_util.h"
#include "ir_reader.h"
#include "tie.h"
#include "xtmap.h"
#include "stblock.h"
#include "opt_du.h"
#include "opt_alias_mgr.h"
#include "cxx_hash.h"

BOOL Targ_Lower_Float_To_Unsigned = FALSE;
BOOL Targ_Lower_Unsigned_To_Float = FALSE;

/* largest signed offset possible in small-frame stack model. For
   xtensa we don't have a large-frame stack model, so we want to allow
   all offsets. */
const INT Max_Small_Frame_Offset = INT_MAX;

typedef HASH_TABLE<WN *, INT> WN_HASH;


/* only return machine_ops, TOP_UNDEFINED if not an exact correspondence */
TOP
OPCODE_To_TOP (OPCODE opcode)
{
  OPERATOR opr   = OPCODE_operator (opcode);
  TYPE_ID  rtype = OPCODE_rtype (opcode);
  TYPE_ID  desc  = OPCODE_desc  (opcode);

  switch (opr) {

  case OPR_GOTO:
    return TOP_j;

  case OPR_FORWARD_BARRIER:
    return TOP_fwd_bar;

  case OPR_BACKWARD_BARRIER:
    return TOP_bwd_bar;

  case OPR_INTRINSIC_CALL:
    if (rtype == MTYPE_V) return TOP_intrncall;
    else                  return TOP_UNDEFINED;

  case OPR_NEG:
    return TOP_UNDEFINED;

  case OPR_ABS:
    return TOP_UNDEFINED;

  case OPR_PAREN:
    if (rtype == MTYPE_F4) return TOP_noop;
    else if (rtype == MTYPE_F8) return TOP_noop;
    else                        return TOP_UNDEFINED;

  case OPR_PARM:
    return TOP_noop;

  case OPR_TRAP:
    return TOP_break;

  default:
    return TOP_UNDEFINED;
  }
}

/* pick the opcode corresponding to the TAS, which will either
 * be a float<->int move or a no-op. */
TOP
TAS_To_TOP (WN *tas_wn)
{
  FmtAssert(FALSE, ("Not yet implemented"));
  return TOP_UNDEFINED;
}

/* return TRUE if the val is a power of 2 */
#define IS_POWER_OF_2(val)      ((val != 0) && ((val & (val-1)) == 0))

static BOOL Is_Power_Of_2(INT64 val, TYPE_ID mtype)
{
  if (MTYPE_is_signed(mtype) && val < 0)
    val=        -val;

  if (mtype == MTYPE_U4)
    val &= 0xffffffffull;

  return IS_POWER_OF_2(val);
}

/* return whether MPY will be translated into shifts and adds */
/* NOTE:  this routine must stay in sync with cg's Expand_Multiply */
BOOL
Can_Do_Fast_Multiply (TYPE_ID mtype, INT64 val)
{
  /* This isn't called... */
  FmtAssert(FALSE, ("Not implemented"));
  return FALSE;
}

/* return whether DIV will be translated into shifts */
BOOL
Can_Do_Fast_Divide (TYPE_ID mtype, INT64 dividend)
{
  return Is_Power_Of_2(dividend, mtype);
}

/* return whether REM or MOD will be translated into shifts */
BOOL
Can_Do_Fast_Remainder (TYPE_ID mtype, INT64 dividend)
{
  return Is_Power_Of_2(dividend, mtype);
}


/* ====================================================================
 *
 * Multiply_Limit
 * Divide_Limit
 *
 * When trying to convert a multiply or divide operation into a series
 * of shifts/adds/subtracts, there is some operation limit at which
 * the conversion is not profitable.  Return that limit.
 *
 * ==================================================================== */
INT
Multiply_Limit (WN * tree)
{
  Is_True( WN_operator( tree ) == OPR_MPY, ("Expecting a multiply operator") );

  if (xt_mac16)
    Lmt_DevWarn(1, ("multiply limit for mac16"));

  BOOL mul16 = (xt_mul16 && WN_Mpy_16Bit(tree));
  
  if (OPT_Space)
  {
    if( xt_mul32 || mul16 )
      return 2;
   
    /* Estimate the HiFi2 multiplication code size in terms of
       Inst16/Inst24 operations. Account for the fact that we could
       schedule extra operations in the available slot0 slots. */
    if (Enable_HiFi2_Ops && WN_Mpy_16Bit(tree))
      return (WN_Mpy_16Bit_Signed(tree)) ? 5 : 6;

    return 5;
  }
  else {
    /* If we have mul16 or mul32 instruction, then try to account for
       the need to load the constant and the latency of the
       instruction. */
    if( xt_mul32 || mul16 )
      return 2;

    if (Enable_HiFi2_Ops && WN_Mpy_16Bit(tree))
      return (WN_Mpy_16Bit_Signed(tree)) ? 4 : 5;
    
    /* We don't have a mul32, and we can't use a mul16 for 'tree',
       then we must do an instrinsic call. If we have mul16 or hifi2
       instructions, then the instrinsic takes no more than 20
       cycles. Otherwise it is much slower (but we just use the fairly
       optimistic number 30). */
    
    return ((xt_mul16 || Enable_HiFi2_Ops) ? 20 : 30);
  }
}

INT
Divide_Limit (void)
{
  if (OPT_Space)
  {
    if (xt_div32)
      return 2;

    return 4;
  }
  else
  {
    if (xt_div32)
      return 4;

    return 50;
  }
}


INT
Copy_Quantum_Ratio(void)
{
  INT32  ratio;

  //  Lmt_DevWarn(1, ("Copy_Quantum_Ratio needs work"));
  // I don't think it still needs work
  switch(Target) {
  case TARGET_XTENSA:	ratio=	4; break;
  default:		ratio=	4; break;
  }

  return ratio;    
}


/* ====================================================================
 *
 * Global offset canonicalization and load/store updating
 *
 * ==================================================================== */

#define	ABS(x)		(((x)<0) ? -(x) : (x))

struct OFFSET_SPAN
{
  INT32 offset_min, offset_max;
  OFFSET_SPAN () { }
  OFFSET_SPAN (INT32 l, INT32 h) : offset_min(l), offset_max(h) { }
};

struct OFFSET_ST
{
  BOOL valid;
  ST *st;
  INT64 offset;
  OFFSET_ST () : valid(FALSE) { }
  OFFSET_ST (ST *s, INT64 o) : valid(TRUE), st(s), offset(o) { }
};

struct OFFSET_SCALE
{
  BOOL valid;
  ST *st;
  INT64 scale;
  OFFSET_SCALE () : valid(FALSE) { }
  OFFSET_SCALE (ST *s, INT64 o) : valid(TRUE), st(s), scale(o) { }
};


struct OFFSET_MAP_KEY
{
  const UINT32 _depth;
  const UINT64 _id;
  ST *const _st;
  ST *const _scalar;
  const INT64 _scale;
  
  OFFSET_MAP_KEY (UINT32 depth, UINT64 id, ST *st, ST *scalar, INT64 scale) :
    _depth(depth), _id(id), _st(st), _scalar(scalar), _scale(scale)
  { }
};

class OFFSET_MAP_HASH
{
public:
  unsigned int hash (const OFFSET_MAP_KEY *key) const
  {
    return key->_depth + key->_id + key->_scale +
      (unsigned int)key->_st + (unsigned int)key->_scalar;
  }
  
  bool test (const OFFSET_MAP_KEY *key1, const OFFSET_MAP_KEY *key2) const
  {
    return((key1->_st == key2->_st) &&
	   (key1->_scalar == key2->_scalar) &&
	   (key1->_scale == key2->_scale) &&
	   (key1->_depth == key2->_depth) &&
	   (key1->_id == key2->_id));
  }
};



static OFFSET_ST Find_Offset_St (WN *tree);
static OFFSET_SCALE Find_Offset_Scalar (WN *tree);
static BOOL Memory_Offset_Span (TYPE_ID type, WN *parent, WN *wn, OFFSET_SPAN *span);
static WN_MAP ofc_wn_map = WN_MAP_UNDEFINED;
static BOOL trace_oc = FALSE;
static ST **PREG_To_ST_Map = NULL;
static PREG_NUM PREG_To_ST_Map_Sz = 0;

#ifdef sun
/* the mess with elf and defs makes this necessary on sun */
#undef INT64_MAX
#define INT64_MAX (9223372036854775807ll)
#endif
static const INT64 NO_PREFERENCE = INT64_MAX;

class OFFSET_CANON_LOOP;
class OFFSET_CANON_SYM;
class OFFSET_CANON_WN;
class OFFSET_WN;


/* Maintain updating load/store information for a symbol at a given
   control-flow scope. */
class UPDATE_SYM
{
private:
  /* The ST and scope represented by this object. '_depth' and '_id'
     identify the scope as defined in UPDATE_LOOP. The same ST can be
     represented by multiple UPDATE_SYMs, one for each scope where the
     ST occurs. */
  const UINT32 _depth;
  const UINT64 _id;
  ST *const _st;
  ST *const _scalar;
  const INT64 _scale;
  
  /* The last seen load or store that can potentially be changed to an
     updating version. */
  TOP _last_update_top;
  OFFSET_WN *_last_own;
  
public:
  UPDATE_SYM (UINT32 depth, UINT64 id, ST *st, ST *scalar, INT64 scale) :
    _depth(depth), _id(id), _st(st), _scalar(scalar), _scale(scale),
    _last_update_top(TOP_UNDEFINED), _last_own(NULL)
  {
    Is_True(_st != NULL, ("must have symbol"));
    Is_True((_scalar != NULL) || (_scale == 0), ("bad scale for NULL scalar symbol"));
  }

  /* Accessors ... */
  UINT32 Scope_Depth (void) const { return _depth; }
  UINT32 Scope_Id (void) const { return _id; }
  ST *St (void) const { return _st; }
  ST *Scalar (void) const { return _scalar; }
  INT64 Scale (void) const { return _scale; }

  /* Process an increment by 'con'. */
  BOOL Process_Increment (ST *st, INT64 inc_val);

  /* Process a load or store of 'oc_sym'. */
  BOOL Process_Load_Store (WN *wn, WN *parent, OFFSET_WN *own,
			   INT64 scale, INT64 inc);

  void Print (FILE *file, UINT tab =0) const
  {
    fprintf(file, "%*s<update_sym %d:%" LLD_FMT ">  ",
	    tab, "", _depth, _id);
    _st->Print(file, FALSE);
    if (_scalar)
    {
      fprintf(file, "%*s scale %" LLD_FMT ", scalar ", tab+4, "", _scale);
      _scalar->Print(file, FALSE);
    }
    
    fprintf(file, "%*slast_top %s\n",
	    tab+4, "", TI_TOP_Name(_last_update_top));
  }
};


/* Maintain updating load/store information for a loop. */
class UPDATE_LOOP
{
private:
  typedef UTL_Map<OFFSET_MAP_KEY *, UPDATE_SYM *, OFFSET_MAP_HASH> UL_MAP;
  typedef UTL_Map<OFFSET_MAP_KEY *, INT64, OFFSET_MAP_HASH> INC_MAP;
  enum { MAX_DEPTH = 63 };

  /* Memory pool... */
  MEM_POOL *_pool;
  
  /* Identify the current conditional scope of this object.  '_depth'
     indicates the conditional depth, and '_id' gives a bit-pattern
     indicating the then-else "path" to the current block. '_depth' ==
     0 indicates current block is unconditional. '_depth' > MAX_DEPTH
     indicates conditional scope is unknown. */
  UINT32 _depth;
  UINT64 _id;

  /* Map from conditional scope/symbol to UPDATE_SYM object
     representing that scope/symbol. */
  UL_MAP _map;

  /* Map of last seen scalar symbol increments. */
  INC_MAP _inc_map;

  /* Map indicating which ST's have already been biased in this
     loop. Used to prevent us from attempting to bias a symbol
     multiple times. */
  UL_MAP _bias_map;


  /* Return true if 'symbol'/'scalar'/'scale' have already been biased in this
     loop. */
  BOOL Biased (ST *st, ST *scalar, INT64 scale)
  {
    /* Use '_depth' and '_id' equal to 0 to ignore scope. */
    OFFSET_MAP_KEY key(0, 0, st, scalar, scale);
    return _bias_map.find(&key);
  }
  
  /* Show that 'symbol'/'scalar'/'scale' have been biased in this loop. */
  void Set_Biased (ST *st, ST *scalar, INT64 scale)
  {
    /* Use '_depth' and '_id' equal to 0 to ignore scope. */
    OFFSET_MAP_KEY *key = CXX_NEW(OFFSET_MAP_KEY(0, 0, st, scalar, scale), _pool);
    Is_True(!_bias_map.find(key), (""));
    _bias_map.insert(key, NULL);
  }
  
  /* Return the UPDATE_SYM representing 'st'/'scalar'/'scale' in the
     current scope. Return NULL if such an object does not exist. */
  UPDATE_SYM *Lookup (ST *st, ST *scalar, INT64 scale)
  {
    OFFSET_MAP_KEY key(_depth, _id, st, scalar, scale);
    UPDATE_SYM *value;
    if (!_map.find(&key, &value))
      return NULL;

    return value;
  }

  /* Create an UPDATE_SYM object representing 'st'/'scalar'/'scale'. */
  UPDATE_SYM *Create (ST *st, ST *scalar, INT64 scale)
  {
    OFFSET_MAP_KEY *key = CXX_NEW(OFFSET_MAP_KEY(_depth, _id, st, scalar, scale), _pool);
    UPDATE_SYM *usym = CXX_NEW(UPDATE_SYM(_depth, _id, st, scalar, scale), _pool);
    Is_True(!_map.find(key), ("UPDATE_SYM already exists in map"));
    _map.insert(key, usym);

    if (trace_oc)
    {
      fprintf(TFile, "<update_loop %d:%" LLD_FMT "> create new UPDATE_SYM\n",
	      _depth, _id);
      usym->Print(TFile, 4);
    }

    return usym;
  }

  
public:
  UPDATE_LOOP (MEM_POOL *pool) :
    _depth(0), _id(0), _map(pool, 31), _inc_map(pool, 31), _bias_map(pool, 31), _pool(pool)
  { }

  /* Maintain the conditional depth and id... */
  void start_then (void)
  {
    /* "then" represented by 0 bit in '_id'. We don't need to explicitly
       0 it since bits > '_depth' kept at 0. */
    _depth++;
  }

  void end_then (void)
  {
    Is_True(_depth > 0, (""));
    _depth--;
  }

  void start_else (void)
  {
    /* "else" represented by 1 bit in '_id'. */
    if (_depth <= MAX_DEPTH)
      _id |= (1 << _depth);
    _depth++;
  }

  void end_else (void)
  {
    Is_True(_depth > 0, (""));
    _depth--;
    if (_depth <= MAX_DEPTH)
      _id &= ~(1 << _depth);
  }

  /* If 'wn' is a constant increment of a symbol, then try to
     influence the canonicalization of a previous load/store so that
     the add can be combined with the load/store to form an updating
     version. */
  void Process_Increment (WN *wn);

  void Process_Load_Store (WN *wn, WN *parent, OFFSET_CANON_SYM *oc_sym);

  void Finish_Biasing (void);

  void Print (FILE *file, UINT tab =0)
  {
    fprintf(file, "%*s<update_loop %d:%" LLD_FMT ">\n",
	    tab, "", _depth, _id);

    UL_MAP::keyValuePair_p scan;
    UL_MAP::iter iter(_map);
    while ((scan = iter.next()) != NULL)
    {
      scan->value()->Print(TFile, tab + 4);
    }
  }

};


/* List of WNs and offsets */
class OFFSET_WN
{
private:
  /* The offset canonicalization for this WN. */
  OFFSET_CANON_WN *_canon_wn;
  
  /* The WN and offset. '_offset' is not necessarily the same as the
     offset specified in '_wn'. */
  WN *const _wn;
  INT64 _offset;

  /* Preferred canonicalization for this WN. */
  INT64 _prefer;
  
  /* The inclusive range of canonical offsets that can be used for
     'wn'. */
  OFFSET_SPAN _offset_span;
  
  /* The next OFFSET_WN that refers to the same symbol as this
     one and gets the same canonization. */
  OFFSET_WN *_next;

public:
  OFFSET_WN (TYPE_ID type, WN *const wn, WN *const parent,
	     const INT64 offset, OFFSET_WN *next) :
    _canon_wn(NULL), _wn(wn), _offset(offset), _next(next), _prefer(NO_PREFERENCE)
  {
    /* If we can't find an offset span for 'wn', make the span as
       large as possible so that this WN doesn't influence the
       canonicalization. */
    if (!Memory_Offset_Span(type, parent, wn, &_offset_span))
      _offset_span = OFFSET_SPAN(INT32_MIN, INT32_MAX);
  }

  /* Accessors... */
  WN *Wn (void) const { return _wn; }
  OFFSET_WN *Next (void) const { return _next; }
  INT64 Min_Canon (void) const { return _offset - _offset_span.offset_max; }
  INT64 Max_Canon (void) const { return _offset - _offset_span.offset_min; }
  BOOL Has_Preference (void) const { return _prefer != NO_PREFERENCE; }
  
  OFFSET_WN *&Next (void) { return _next; }
  INT64& Offset (void) { return _offset; }
  INT64& Preference (void) { return _prefer; }
  OFFSET_CANON_WN *&Offset_Canon_Wn (void) { return _canon_wn; }
  
  void Print (FILE *file, INT tab)
  {
    fprintf(file, "%*soffset = %" LLD_FMT ", canon %d - %d",
	    tab, "", _offset, _offset_span.offset_min, _offset_span.offset_max);
    if (Has_Preference())
      fprintf(file, ", prefer = %" LLD_FMT "", _prefer);

    fprintf(file, ", node = ");
    fdump_wn(file, _wn);
  }
};



/* Head of a list of WNs and offsets that share the same
   canonicalization. */
class OFFSET_CANON_WN
{
private:
  /* Symbol that this canonicalization is for. */
  OFFSET_CANON_SYM *_canon_sym;
  
  /* Range of offset canonicalizations that are available for use by
     all WNs in 'this's list. */
  INT64 _canon_min, _canon_max;
  
  /* Canonical offset chosen for this set of WNs. */
  INT64 _canon_offset;

  /* Prefer canonicalization for this set of WNs. */
  INT64 _prefer;
  
  /* List of WN/offset pairs that share this canonicalization. */
  OFFSET_WN *_wns;
  
  /* The next OFFSET_CANON_WN that refers to the same symbol as this
     one, but that gets a different canonization. */
  OFFSET_CANON_WN *_next;

  // Maximum alignment of all WNs in the list
  INT64 _max_align;

  // Number of WNs (and offsets) in this canonicalization
  UINT32 _num_wns;
  
public:
  OFFSET_CANON_WN (OFFSET_WN *own, OFFSET_CANON_WN *next);
  
  /* Accessors... */
  INT64 Canon_Offset (void) const { return _canon_offset; }
  INT64 Preference (void) { return _prefer; }
  OFFSET_CANON_WN *Next (void) const { return _next; }
  BOOL Has_Preference (void) const { return _prefer != NO_PREFERENCE; }
  OFFSET_CANON_SYM *&Offset_Canon_Symbol (void) { return _canon_sym; }
  OFFSET_CANON_WN *&Next (void) { return _next; }
  UINT32 Num_Wns (void) const { return _num_wns; }
  
  /* Try to add 'own' to the list of WNs canonicalized to this
     offset. If possible, return TRUE and adjust '_canon_min' and
     '_canon_max'. If not possible return FALSE. */
  BOOL Try_Add_Wn (OFFSET_WN *own);

  /* Remove 'own' from the list of WNs canonicalized to this
     offset. Assert if 'own' is not canonicalized to this offset. */
  void Remove_Wn (OFFSET_WN *own);

  /* Return the amount of overlap in the canonicalization ranges for
     'this' and 'canon'. */
  INT64 Overlap (OFFSET_CANON_WN *canon);
  
  /* Merge 'canon' with 'this'. Abort if the merge is not possible. */
  void Merge_Canon (OFFSET_CANON_WN *canon);
  
  /* Determine the canonical offset to use. */
  void Finalize_Offset (void);
  
  /* For each WN in this canonicalization, initialize the map from the
     WN to 'this'. */
  void Build_Map (void);
  
  void Print (FILE *file, INT tab, BOOL verbose =FALSE);
};

  OFFSET_CANON_WN::OFFSET_CANON_WN (OFFSET_WN *own, OFFSET_CANON_WN *next) :
    _canon_offset(0), _prefer(NO_PREFERENCE), _wns(own), _next(next),
    _max_align(1), _num_wns(1)
  {
    _canon_min = _wns->Min_Canon();
    _canon_max = _wns->Max_Canon();
    if (_wns->Has_Preference())
      _prefer = _wns->Offset() - _wns->Preference();

    if (MTYPE_alignment(WN_desc(own->Wn())) > _max_align)
      {
	INT64 new_align = MTYPE_alignment(WN_desc(own->Wn()));
	FmtAssert((new_align&(new_align-1)) == 0, 
		  ("Alignment is not a power of two"));
	_max_align = new_align;
      }

    _wns->Offset_Canon_Wn() = this;
    _wns->Next() = NULL;
  }

  /* Try to add 'own' to the list of WNs canonicalized to this
     offset. If possible, return TRUE and adjust '_canon_min' and
     '_canon_max'. If not possible return FALSE. */
  BOOL 
  OFFSET_CANON_WN::Try_Add_Wn (OFFSET_WN *own)
  {
    if ((own->Min_Canon() <= _canon_max) &&
	(own->Max_Canon() >= _canon_min))
    {
      /* If 'own' has a preference, and this canon doesn't, then make
         'own's preference the preference for this canon. */
      if (own->Has_Preference())
      {
	if (Has_Preference())
	  DevWarn("multiple preferences attempted for same global canon.");
	else
	  _prefer = own->Offset() - own->Preference();
      }

      _canon_min = MAX(_canon_min, own->Min_Canon());
      _canon_max = MIN(_canon_max, own->Max_Canon());

      /* If the new canon range does not allow an existing preference,
         then remove the preference. The assumption is that it is more
         important to minimize the number of canons than to obey
         preferences. */
      if (Has_Preference() &&
	  ((_prefer < _canon_min) || (_prefer > _canon_max)))
      {
	DevWarn("abandoning preference %" LLD_FMT ", %" LLD_FMT " -> %" LLD_FMT "",
		_prefer, _canon_min, _canon_max);
	_prefer = NO_PREFERENCE;
      }
      
      own->Next() = _wns;
      own->Offset_Canon_Wn() = this;
      _wns = own;
      _num_wns++;

      if (MTYPE_alignment(WN_desc(own->Wn())) > _max_align)
      {
	INT64 new_align = MTYPE_alignment(WN_desc(own->Wn()));
	FmtAssert((new_align&(new_align-1)) == 0, 
		  ("Alignment is not a power of two"));
	_max_align = new_align;
      }
      
      return TRUE;
    }
    
    return FALSE;
  }

  /* Remove 'own' from the list of WNs canonicalized to this
     offset. Assert if 'own' is not canonicalized to this offset. */
  void
  OFFSET_CANON_WN::Remove_Wn (OFFSET_WN *own)
  {
    if (own == _wns)
      {
        _wns = _wns->Next();
      }
    else
      {
        for (OFFSET_WN *scan = _wns; scan; scan = scan->Next())
          {
            OFFSET_WN *next = scan->Next();
            FmtAssert(next, ("expecting to find OFFSET_WN"));
            if (next == own)
              {
                scan->Next() = next->Next();
                break;
              }
          }
      }
    
    _num_wns--;

    own->Next() = NULL;
    own->Offset_Canon_Wn() = NULL;
  }

  /* Return the amount of overlap in the canonicalization ranges for
     'this' and 'canon'. */
  INT64 
  OFFSET_CANON_WN::Overlap (OFFSET_CANON_WN *canon)
  {
    INT64 max = MIN(_canon_max, canon->_canon_max);
    INT64 max_align = MAX(_max_align, canon->_max_align);
    INT64 min1 = (_canon_min+max_align-1)&(-max_align);
    INT64 min2 = (canon->_canon_min+max_align-1)&(-max_align);
    INT64 min = MAX(min1,min2);
    if (min > max)
      return 0;

    return max - min + 1;
  }

  /* Merge 'canon' with 'this'. Abort if the merge is not possible. */
  void
  OFFSET_CANON_WN::Merge_Canon (OFFSET_CANON_WN *canon)
  {
    FmtAssert(Overlap(canon) > 0, ("unable to merge non-overlapping canons"));
    
    INT64 max_align = MAX(_max_align, canon->_max_align);
    INT64 min1 = (_canon_min+max_align-1)&(-max_align);
    INT64 min2 = (canon->_canon_min+max_align-1)&(-max_align);
    _canon_min = MAX(min1,min2);
    _canon_max = MIN(_canon_max, canon->_canon_max);
    _max_align = max_align;

    if (!_wns)
    {
      _wns = canon->_wns;
      _num_wns = canon->_num_wns;
    }
    else
    {
      OFFSET_WN *end = _wns;
      while (end->Next())
	end = end->Next();
      
      end->Next() = canon->_wns;
      _num_wns += canon->_num_wns;
    }

    /* Make sure all the OFFSET_WNs have 'this' as their canon. */
    for (OFFSET_WN *scan = _wns; scan; scan = scan->Next())
      scan->Offset_Canon_Wn() = this;
    
    /* If one or both of the canon's has a preference, then try to
       preserve one of them. */

    if (Has_Preference() || canon->Has_Preference())
    {
      if (Has_Preference() && (_prefer >= _canon_min) &&
	  (_prefer <= _canon_max))
      {
	// keep existing preference
      }
      else if (canon->Has_Preference() &&
	       (canon->_prefer >= _canon_min) &&
	       (canon->_prefer <= _canon_max))
      {
	_prefer = canon->_prefer;
      }
      else
      {
	DevWarn("abandoning preference during canon merge");
      }
    }
  }

  /* Determine the canonical offset to use. */
  void
  OFFSET_CANON_WN::Finalize_Offset (void)
  {
    /* Use canon preference is there is one. */
    if (Has_Preference())
    {
      _canon_offset = _prefer;
      return;
    }
    
    /* Prefer canonicalization to 0, if that is available. We prefer
       this because then we don't have to adjust the base pointer
       outside the loop. */
    if ((_canon_min <= 0) && (_canon_max >= 0))
    {
      _canon_offset = 0;
      return;
    }

#ifdef TARG_XTENSA
    extern UINT32 xt_goc;

    /* To allow further experimentation for this, I've added
     * -TARG:goc=%u flag
     * 0 - don't try to pick canonical offset using logic below
     * 1 - do it if there are many WNs/offsets in this canonicalization
     * 2 - always use the logic below
     * Currently, the default is set to 1.
     */

    /* Try to take advantage of addmi instruction */
    if ((xt_goc == 2 || (xt_goc == 1 && _num_wns > 20)) && 
        (_canon_min < 128*256) && (_canon_max >= -128*256) && 
	((_canon_max&0xffffff00)>=_canon_min))
    {
      if (_canon_min >= -128*256)
	_canon_offset = (_canon_min+0xff)&0xffffff00;
      else if (_canon_max < 128*256)
	_canon_offset = _canon_max&0xffffff00;
      else
	_canon_offset = 0;
      return;
    }
#endif

    /* Choose a canonicalization between the min and max. When
       re-canonicalizing after offset changes, this increases the
       chance that two re-canonicalized wn's can share a
       canonicalization. */
    _canon_offset = (_canon_min + _canon_max) / 2;
    
    /* Force _canon_offset to be a multiple of _max_align. _max_align is
       assumed to be a power of two. */
    _canon_offset = (_canon_offset+_max_align-1)&(-_max_align);

#if 0
    /* Force _canon_offset to be a multiple of _max_align. _max_align is
       assumed to be a power of two. */
    _canon_min = (_canon_min+_max_align-1)&(-_max_align);
    _canon_max = _canon_max&(-_max_align);

    /* If there is a WN with offset equal to the canon min/max then
       use that. Otherwise just arbitrarity choose one. */
    for (OFFSET_WN *wn = _wns; wn; wn = wn->Next())
    {
      if (wn->Offset() == _canon_min)
      {
	_canon_offset = _canon_min;
	return;
      }
      else if (wn->Offset() == _canon_max)
      {
	_canon_offset = _canon_max;
	return;
      }
    }
    
    _canon_offset = _canon_max;
#endif
  }
  
  /* For each WN in this canonicalization, initialize the map from the
     WN to 'this'. */
  void
  OFFSET_CANON_WN::Build_Map (void)
  {
    OFFSET_WN *wn = _wns;
    while (wn)
    {
      FmtAssert(!WN_MAP_Get(ofc_wn_map, wn->Wn()), ("multiple visits to WN"));
      WN_MAP_Set(ofc_wn_map, wn->Wn(), wn);
      wn = wn->Next();
    }
  }
  
  void
  OFFSET_CANON_WN::Print (FILE *file, INT tab, BOOL verbose)
  {
    fprintf(file, "%*scanon_min = %" LLD_FMT ", canon_max = %" LLD_FMT ", canon = %" LLD_FMT "",
	    tab, "", _canon_min, _canon_max, _canon_offset);
    if (Has_Preference())
      fprintf(file, ", prefer = %" LLD_FMT "", _prefer);

    fprintf(file, "\n");
    
    if (verbose)
    {
      OFFSET_WN *scan = _wns;
      while (scan)
      {
	scan->Print(file, tab+2);
	scan = scan->Next();
      }
    }
  }
  

/* Offset canon information for a symbol. */
class OFFSET_CANON_SYM
{
private:
  /* Pool... */
  MEM_POOL *_pool;
     
  /* ST/scalar ST/scale represented by this class. */
  ST *const _st;
  ST *const _scalar;
  const INT64 _scale;

  /* List of WN_offset pairs that have not yet be canonicalized. */
  OFFSET_WN *_wns;
  
  /* List of lists of WN/offset pairs that have ben canonicalized. */
  OFFSET_CANON_WN *_canons;

public:
  OFFSET_CANON_SYM (MEM_POOL *pool, ST *st, ST *scalar, INT64 scale) :
    _pool(pool), _st(st), _scalar(scalar), _scale(scale), _wns(NULL), _canons(NULL) 
  { }

  /* Accessors... */
  ST *St (void) const { return _st; }
  ST *Scalar (void) const { return _scalar; }
  INT64 Scale (void) const { return _scale; }
  
  /* Add 'wn'/'offset' to the set of WNs/offsets that reference this
     symbol. */
  void Add_Wn (TYPE_ID type, WN *wn, WN *parent, INT64 offset);
  
  /* Return the last 'wn'/'offset' pair added with Add_Wn. */
  OFFSET_WN *Last_Added_Wn (void)
  {
    return _wns;
  }
  
  /* Determine the final canonical offset for 'own'. Return the
     OFFSET_CANON_WN it is canonicalized to. */
  OFFSET_CANON_WN *Finalize_One_Offset (OFFSET_WN *own);
  
  /* Determine the final canonical offset for each WN/offset pair in
     '_wns' by moving it to the appropriate list in '_canons'. */
  void Finalize_Offsets (void);
  
  /* Merge the canonicalize offset lists in 'mergee' into 'this's. */
  void Merge_Offsets (OFFSET_CANON_SYM *mergee);
  
  /* For each conicalization of this symbol, initialize map from WN to
     the canonicalization. */
  void Build_Map (void);
  
  void Print (FILE *file, INT tab);
};

  /* Add 'wn'/'offset' to the set of WNs/offsets that reference this
     symbol. */
  void
  OFFSET_CANON_SYM::Add_Wn (TYPE_ID type, WN *wn, WN *parent, INT64 offset)
  {
    _wns = CXX_NEW(OFFSET_WN(type, wn, parent, offset, _wns), _pool);
  }

  /* Determine the final canonical offset for 'own'. Return the
     OFFSET_CANON_WN it is canonicalized to. */
  OFFSET_CANON_WN *
  OFFSET_CANON_SYM::Finalize_One_Offset (OFFSET_WN *own)
  {
    OFFSET_CANON_WN *cans = _canons;
    while (cans)
      {
	if (cans->Try_Add_Wn(own))
	  break;
	
	cans = cans->Next();
      }

    /* Didn't find an existing canonicalization? Create one. */
    if (!cans)
      {
        _canons = cans = CXX_NEW(OFFSET_CANON_WN(own, _canons), _pool);
        cans->Offset_Canon_Symbol() = this;
      }

    return cans;
  }

  /* Determine the final canonical offset for each WN/offset pair in
     '_wns' by moving it to the appropriate list in '_canons'. */
  void
  OFFSET_CANON_SYM::Finalize_Offsets (void)
  {
    /* We just do the greedy thing. Take each OFFSET_WN from '_wns'
       and try to fit it in each existing entry in '_canons'. If we
       can't find an existing canonicalization for it, create a new
       one. */
    OFFSET_WN *scan = _wns;
    while (scan)
    {
      /* We will relink 'scan' into an OFFSET_CANON_WN list, so we
         need to get the next pointer here. */
      OFFSET_WN *next = scan->Next();
      Finalize_One_Offset(scan);
      scan = next;
    }

    /* Show that all '_wns' have been canonicalized. */
    _wns = NULL;
  }
  
  /* Merge the canonicalize offset lists in 'mergee' into 'this's. */
  void
  OFFSET_CANON_SYM::Merge_Offsets (OFFSET_CANON_SYM *mergee)
  {
    Is_True((_st == mergee->_st) && (_scalar == mergee->_scalar) &&
	    (_scale == mergee->_scale),
	    ("expecting identical symbols"));

    /* Greedy algorithm. For each canon list in 'mergee', try to add
       it to our canon list that has the most offset range in
       common. If we can't find any canon list with any range in
       common, then move the 'mergee' canon list over as a new
       list. */

    OFFSET_CANON_WN *scan = mergee->_canons;
    while (scan)
    {
      /* We will relink 'scan' into '_canons', so we need to get the
         next pointer here. */
      OFFSET_CANON_WN *next = scan->Next();

      /* Find our canon list with maximim offset overlap to 'scan'. */
      INT64 max_overlap = 0;
      OFFSET_CANON_WN *max_overlap_canon = NULL;
      
      OFFSET_CANON_WN *cans = _canons;
      while (cans)
      {
	INT64 overlap = cans->Overlap(scan);
	if (overlap > max_overlap)
	{
	  max_overlap = overlap;
	  max_overlap_canon = cans;
	}
	
	cans = cans->Next();
      }

      /* Merge 'scan' with 'max_overlap_canon', or if we didn't find
	 an existing canonicalization, copy 'scan' to _canons. */
      if (max_overlap_canon)
      {
	max_overlap_canon->Merge_Canon(scan);
      }
      else
      {
        scan->Offset_Canon_Symbol() = this;
	scan->Next() = _canons;
	_canons = scan;
      }
      
      scan = next;
    }
  }

  /* For each conicalization of this symbol, initialize map from WN to
     the canonicalization. */
  void
  OFFSET_CANON_SYM::Build_Map (void)
  {
    OFFSET_CANON_WN *cans = _canons;
    while (cans)
    {
      cans->Finalize_Offset();
      cans->Build_Map();
      cans = cans->Next();
    }
  }
  
  void
  OFFSET_CANON_SYM::Print (FILE *file, INT tab)
  {
    _st->Print(file, FALSE);
    if (_scalar)
    {
      fprintf(file, "%*s<scale %" LLD_FMT ", scalar> ", tab+4, "", _scale);
      _scalar->Print(file, FALSE);
    }
    
    fprintf(file, "%*s<Non-Canon WNs>:\n", tab, "");
    OFFSET_WN *scan = _wns;
    while (scan)
    {
      scan->Print(file, 8);
      scan = scan->Next();
    }
    
    fprintf(file, "%*s<Canon WNs>:\n", tab, "");
    OFFSET_CANON_WN *scan_canon = _canons;
    while (scan_canon)
    {
      scan_canon->Print(file, 8, TRUE);
      scan_canon = scan_canon->Next();
    }
  }
  

/* Offset information for a single loop. */
struct OFFSET_CANON_LOOP_LIST
{
  OFFSET_CANON_LOOP *_ocl;
  OFFSET_CANON_LOOP_LIST *_next;

  OFFSET_CANON_LOOP_LIST (OFFSET_CANON_LOOP *first, OFFSET_CANON_LOOP_LIST *rest) :
    _ocl(first), _next(rest)
  { }
  
};

/* Added -TARG:pseudo_loop flag (default is TRUE)
 * to allow us to try picking offsets for each loop nest
 * separately, without merging them at the function level.
 * This came up in PR 3697.
 */

extern BOOL xt_pseudo_loop;

class OFFSET_CANON_LOOP
{
private:
  typedef UTL_Map<OFFSET_MAP_KEY *, OFFSET_CANON_SYM *, OFFSET_MAP_HASH> ST_MAP;

  /* WN representing this loop, e.g. OPC_DO_LOOP. */
  WN *_loop_wn;

  /* The first and last WN in the loop body. */
  WN *_start, *_end;

  /* A list of loops immediately contained within this one. */
  OFFSET_CANON_LOOP_LIST *_inner;

  /* The loop that immediately contains this one. */
  OFFSET_CANON_LOOP *_outer;

  /* Map from ST to OFFSET_CANON_SYM representing that ST. Only ST's
     referenced in this loop are entered in the map. */
  ST_MAP _st_map;


public:
  OFFSET_CANON_LOOP (WN *loop_wn, WN *start, WN *end,
		     OFFSET_CANON_LOOP *outer, MEM_POOL *pool) :
    _loop_wn(loop_wn), _start(start), _end(end), _inner(NULL),
    _outer(outer), _st_map(pool, 31)
  {
    if (_outer)
      _outer->_inner = CXX_NEW(OFFSET_CANON_LOOP_LIST(this, _outer->_inner), pool);
  }

  /* Accessors... */
  OFFSET_CANON_LOOP_LIST *Inner_Loops (void) const { return _inner; }
  OFFSET_CANON_LOOP *Outer_Loop (void) const { return _outer; }

  /* Record that symbol 'st' at 'offset' is referenced within this
     loop by 'wn'. */
  OFFSET_CANON_SYM *Record_Symbol (ST *st, ST *scalar, INT64 scale,
				   TYPE_ID type, WN *wn,
				   WN *parent, INT64 offset, MEM_POOL *pool);
  
  /* Recursively visit expression 'wn', recording symbol/offset
     pairs, and collecting canon info for ILOAD/ISTORE. */
  void Record_Symbols (WN *wn, WN *parent, UPDATE_LOOP *update_loop,
		       BOOL find_base, MEM_POOL *pool);
  
  /* Determine the canonical offset to be used by each symbol. */
  void Finalize_Canonical_Offsets (MEM_POOL *pool);
  
  /* Merge the canonicalized offsets from 'mergee' into 'this's
     canonical offsets for each symbol. */
  void Merge_Canonical_Offsets (OFFSET_CANON_LOOP *mergee);
  
  /* Build map from WN to canonical offset information. */
  void Build_Canonical_Map (void);
  
  void Print (FILE *file, BOOL verbose =FALSE);
};

  /* Record that symbol 'st' at 'offset' is referenced within this
     loop by 'wn'. */
  OFFSET_CANON_SYM *
  OFFSET_CANON_LOOP::Record_Symbol (ST *st, ST *scalar, INT64 scale,
                                    TYPE_ID type, WN *wn,
                                    WN *parent, INT64 offset, MEM_POOL *pool)
  {
    OFFSET_CANON_SYM *oc_sym;
    OFFSET_MAP_KEY key(0, 0, st, scalar, scale);
    if (!_st_map.find(&key, &oc_sym))
    {
      OFFSET_MAP_KEY *ikey = CXX_NEW(OFFSET_MAP_KEY(0, 0, st, scalar, scale), pool);
      oc_sym = CXX_NEW(OFFSET_CANON_SYM(pool, st, scalar, scale), pool);
      _st_map.insert(ikey, oc_sym);
    }

    oc_sym->Add_Wn(type, wn, parent, offset);

    if (trace_oc)
    {
      fprintf(TFile, "<recsym> ");
      oc_sym->Print(TFile, 4);
    }

    return oc_sym;
  }

  /* Recursively visit expression 'wn', recording symbol/offset
     pairs, and collecting canon info for ILOAD/ISTORE. */
  void 
  OFFSET_CANON_LOOP::Record_Symbols (WN *wn, WN *parent, UPDATE_LOOP *update_loop,
		       BOOL find_base, MEM_POOL *pool)
  {
    OPERATOR opr = WN_operator(wn);
    ST *st = NULL, *scalar = NULL;
    INT64 offset = 0, scale = 0;

    /* We don't expect to hit an OPR_BLOCK or OPR_IF here since that
       could contain loops. So OPR_BLOCK/OPR_IF should be handled in
       Collect_Loop_Offsets or Collect_Loop_Offsets_Flat. */
    FmtAssert((opr != OPR_BLOCK) && (opr != OPR_IF), ("unexpected block/if"));

    /* LDID/STID/LDA of a variable. We only canonicalize these if
       'find_base' is true, and the symbols base symbol is different
       than the symbol itself. We have these conditions because
       otherwise the LDID/STID/LDA doesn't need canonicalization. */
    if (find_base &&
	((opr == OPR_LDID) || (opr == OPR_STID) || (opr == OPR_LDA)) &&
	(WN_class(wn) == CLASS_VAR) &&
	(ST_base(WN_st(wn)) != WN_st(wn)))
    {
      st = WN_st(wn);
      offset = 0;  /* WN_load_offset(wn) added below. */
    }
    /* Indirect loads and stores... */
    else if (!find_base && ((opr == OPR_ILOAD) || (opr == OPR_ISTORE)))
    {
      /* Search the address expression to find the symbol and the
         offset, and to find the scalar and scale, if any. */
      WN *addr = ((opr == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn));

      OFFSET_ST opair = Find_Offset_St(addr);
      if (opair.valid && opair.st)
      {
	st = opair.st;
	offset = opair.offset;
      }

      OFFSET_SCALE oscalar = Find_Offset_Scalar(addr);
      if (oscalar.valid && oscalar.st)
      {
	scalar = oscalar.st;
	scale = oscalar.scale;
      }
    }

    /* If we found a symbol/offset in the address expression, record
       it with 'wn'. */
    OFFSET_CANON_SYM *oc_sym = NULL;

    if (st)
    {
      /* Make sure 'st' is a variable; use it's base if indicated by
	 'find_base'. */

      ST_CLASS st_class = ST_sym_class(st);
      if (st_class == CLASS_VAR)
      {
	if (find_base)
	{
	  ST *base;
	  INT64 base_offset;
	  Base_Symbol_And_Offset(st, &base, &base_offset);
	  st = base;
	  offset += base_offset;
	}
	
	oc_sym = Record_Symbol(st, scalar, scale, WN_desc(wn), wn, parent,
			       WN_load_offset(wn) + offset, pool);
      }
    }

    /* If we are looking for updating load/store opportunities... */
    if (update_loop)
    {
      /* If 'wn' is an increment of a symbol, we may want to influence
         the canonicalization of a previous or later load/store to
         create an updating load/store opportunity. */

      if (opr == OPR_STID)
      {
	update_loop->Process_Increment(wn);
      }
      /* If 'wn' a load/store that we were able to record as a
         canonicalization candidate above, then see if we want to
         influence the canonicalization value of the load/store to
         potentially allow an updating load/store. */
      else if (oc_sym && ((opr == OPR_ILOAD) || (opr == OPR_ISTORE)))
      {
	update_loop->Process_Load_Store(wn, parent, oc_sym);
      }
    }
    
    for (INT kidno = 0; kidno < WN_kid_count(wn); kidno++)
      Record_Symbols(WN_kid(wn, kidno), wn, update_loop, find_base, pool);
  }

  /* Determine the canonical offset to be used by each symbol. */
  void 
  OFFSET_CANON_LOOP::Finalize_Canonical_Offsets (MEM_POOL *pool)
  {
    ST_MAP::keyValuePair_p scan;
    ST_MAP::iter iter(_st_map);
    while ((scan = iter.next()) != NULL)
    {
      scan->value()->Finalize_Offsets();
    }

    if (trace_oc)
    {
      fprintf(TFile, "%sFinalize Offsets\n%s", SBar, SBar);
      Print(TFile, TRUE);
    }
  }

  /* Merge the canonicalized offsets from 'mergee' into 'this's
     canonical offsets for each symbol. */
  void 
  OFFSET_CANON_LOOP::Merge_Canonical_Offsets (OFFSET_CANON_LOOP *mergee)
  {
    /* Iterate through the symbols in 'mergee' and either merge them
       with the same symbol in 'this', or if the symbol does not exist
       in 'this', just add it. */
    
    ST_MAP::keyValuePair_p scan;
    ST_MAP::iter iter(mergee->_st_map);
    while ((scan = iter.next()) != NULL)
    {
      OFFSET_CANON_SYM *this_sym;
      if (_st_map.find(scan->key(), &this_sym))
	this_sym->Merge_Offsets(scan->value());
      else
	_st_map.insert(scan->key(), scan->value());
    }

    if (trace_oc)
    {
      fprintf(TFile, "%sMerge Offsets\n%s", SBar, SBar);
      Print(TFile, TRUE);
    }
  }

  /* Build map from WN to canonical offset information. */
  void 
  OFFSET_CANON_LOOP::Build_Canonical_Map (void)
  {
    // If -TARG:pseudo_loop=off don't build the function level map
    if (!xt_pseudo_loop && !_outer) 
    {
      OFFSET_CANON_LOOP_LIST *inner = _inner;
      while (inner)
      {
        inner->_ocl->Build_Canonical_Map();
        inner = inner->_next;
      }
    }
    else 
    {
      ST_MAP::keyValuePair_p scan;
      ST_MAP::iter iter(_st_map);
      while ((scan = iter.next()) != NULL)
      {
        scan->value()->Build_Map();
      }

      if (trace_oc)
      {
        fprintf(TFile, "%sBuild Canonical Offsets Map\n%s", SBar, SBar);
        Print(TFile, TRUE);
      }
    }
  }
  
  
  void 
  OFFSET_CANON_LOOP::Print (FILE *file, BOOL verbose)
  {
    fprintf(file, "Loop: ");
    fdump_wn(file, _loop_wn);
    fprintf(file, "  Start: ");
    fdump_wn(file, _start);
    fprintf(file, "  End: ");
    fdump_wn(file, _end);
    if (_outer)
    {
      fprintf(file, "  Outer: ");
      fdump_wn(file, _outer->_loop_wn);
    }

    if (verbose)
    {
      ST_MAP::keyValuePair_p scan;
      ST_MAP::iter iter(_st_map);
      while ((scan = iter.next()) != NULL)
      {
	scan->value()->Print(TFile, 4);
      }
    }
  }
  

  extern BOOL xt_lazy_bias;


/* Process an increment by 'con'. */
BOOL
UPDATE_SYM::Process_Increment (ST *st, INT64 inc_val)
{
  if (trace_oc)
  {
    fprintf(TFile, "<procinc %d:%" LLD_FMT "> inc %" LLD_FMT ", scale %" LLD_FMT ", top %s, ",
	    _depth, _id, inc_val, _scale, TI_TOP_Name(_last_update_top));
    st->Print(TFile, FALSE);
  }

  /* If we have a previous updateable load/store, and it can
     potentially combine with 'inc', then bias it's canonicalization
     appropriately. */
  
  if (_last_update_top != TOP_UNDEFINED)
  {
    Is_True(_last_own != NULL, (""));

    const INT64 scale = _scale * inc_val;
    
    if (!TI_TOP_Can_Have_Immediate(scale, _last_update_top))
    {
      if (trace_oc)
	fprintf(TFile, "<procinc> cannot fit offset value %" LLD_FMT " in opcode %s\n",
		scale, TI_TOP_Name(_last_update_top));
    }
    else
    {
      Is_True(!_last_own->Has_Preference(), ("unexpected existing preference"));

      if ((_last_own->Offset() - scale) < _last_own->Min_Canon() ||
	      (_last_own->Offset() - scale) > _last_own->Max_Canon())
	return FALSE;
      
      _last_own->Preference() = scale;

      if (trace_oc)
      {
	fprintf(TFile, "<procinc> biasing canonicalization to %" LLD_FMT "\n", scale);
	_last_own->Print(TFile, 4);
      }
      
      _last_update_top = TOP_UNDEFINED;
      _last_own = NULL;
      return TRUE;
    }
  }

  return FALSE;
}


/* Process a load or store of 'oc_sym'. */
BOOL
UPDATE_SYM::Process_Load_Store (WN *wn, WN *parent, OFFSET_WN *own,
				INT64 scale, INT64 inc)
{
  INT64 inc_val = inc * scale;
  
  if (trace_oc)
  {
    fprintf(TFile, "<procls %d:%" LLD_FMT "> inc %" LLD_FMT ", load/store ",
	    _depth, _id, inc_val);
    fdump_wn(TFile, wn);
  }

  /* Throw away any previous load or store, since an increment
     following 'wn' cannot move past 'wn' to combine with the previous
     load or store. (independent of whether the increment can combine
     with 'wn' or not).  */
//  _last_update_top = TOP_UNDEFINED;
//  _last_own = NULL;
  
  /* Determine if 'wn' will generate a topcode that has an updating
     version. Only tie types can have updating versions, so we only
     bother with them. */

  OPCODE opc = WN_opcode(wn);
  TYPE_ID desc = WN_desc(wn);
  TYPE_ID rtype = WN_rtype(wn);

  if (xt_hard_float)
  {
    if (desc == MTYPE_F4)
      desc = tie_info->xtfloat_mtype_id();
    if (rtype == MTYPE_F4)
      rtype = tie_info->xtfloat_mtype_id();
  }

  if (!MTYPE_is_tie(desc) &&
      (OPCODE_is_store(opc) || !MTYPE_is_tie(rtype)))
  {
    if (trace_oc)
      fprintf(TFile, "<procls> non-tie\n");

    return FALSE;
  }
	
  /* First, find the prototype (macro) that corresponds to 'wn'. */
    
  TIE_MACRO_p macro = NULL;
  
  if (OPCODE_is_load(opc))
  {
    /* If 'rtype' and 'desc' are different, then look for the
       appropriate mtor rule. Otherwise, if the parent of a load is a
       CVT, then use the appropriate mtor rule, if there is one. */
    if (rtype != desc)
    {
      macro = tie_info->mtype_mtor_macro(desc, rtype);
    }
    else
    {
      if (parent && WN_operator(parent) == OPR_CVT)
      {
	macro = tie_info->mtype_mtor_macro(WN_desc(parent), WN_rtype(parent));
      }
      
      if (!macro)
	macro = tie_info->mtype_loadi_macro(desc);
    }
  }
  else if (OPCODE_is_store(opc))
  {
    /* If the stored value is a CVT, then use the appropriate rtom
       rule, if there is one. */
      
    WN *kid = WN_kid0(wn);
    if (WN_operator(kid) == OPR_CVT)
    {
      macro = tie_info->mtype_rtom_macro(WN_desc(kid), WN_rtype(kid));
    }
      
    if (!macro)
      macro = tie_info->mtype_storei_macro(desc);
  }

  /* If no 'macro' exists, or if it is implemented with more than
     one instruction, then we don't have an updating version. */

  if (!macro || (macro->num_instructions() != 1))
  {
    if (trace_oc)
      fprintf(TFile, "<procls> no macro or macro has >1 instruction\n");

    return FALSE;
  }

  TOP top = TI_TOP_Topcode(macro->inst_opcode_name(0));
  if (top == TOP_UNDEFINED)
    return FALSE;

    /* If 'top' does not have an updating version, then we don't want
       to try to influence 'wn' canonicalization. */

  TOP update_top = TI_TOP_Nonupdate_To_Update(top);
  if (update_top == TOP_UNDEFINED)
  {
    if (trace_oc)
      fprintf(TFile, "<procls> no updating\n");

    return FALSE;
  }

  if (trace_oc)
  {
    fprintf(TFile, "<procls %d:%" LLD_FMT "> %s -> %s, ",
	    _depth, _id, TI_TOP_Name(top), TI_TOP_Name(update_top));
    fdump_wn(TFile, wn);
    if (parent)
    {
      fprintf(TFile, "<procls>    parent: ");
      fdump_wn(TFile, parent);
    }
  }

  /* If we have a previous increment, and it can fit in the immediate
     field of 'top', then bias the canonicalization of 'wn' to 0, so
     that the previous increment can be combined with 'top' to form
     'update_top'. */

  if (inc_val != 0)
  {
    if (!TI_TOP_Can_Have_Immediate(inc_val, update_top))
    {
      if (trace_oc)
	fprintf(TFile, "<procls> cannot fit offset value %" LLD_FMT " in opcode %s\n",
		inc_val, TI_TOP_Name(update_top));
    }
    else
    {
      Is_True(!own->Has_Preference(), ("unexpected existing preference"));
      Is_True((own->Offset() >= own->Min_Canon()) &&
	      (own->Offset() <= own->Max_Canon()),
	      ("cannot canonicalize to preferred offset"));
      
      own->Preference() = 0;

      if (trace_oc)
      {
	fprintf(TFile, "<procls> biasing canonicalization to 0\n");
	own->Print(TFile, 4);
      }

      return TRUE;
    }
  }

  /* We don't have a previous increment, or we couldn't combine it
     with 'wn', so just record that 'wn' was the last seen load/store
     of '_st', perhaps we can combine it with a following
     increment... */
  
  _last_update_top = update_top;
  _last_own = own;

  return FALSE;
}


void
UPDATE_LOOP::Process_Increment (WN *wn)
{
  Is_True(WN_operator(wn) == OPR_STID, ("expecting STID"));

  /* We can't do anything if we are unable to represent the
     control-flow scope. */
  if (_depth > MAX_DEPTH)
    return;

  ST *scalar = WN_st(wn);
  WN *add = WN_kid0(wn);
  if (WN_operator(add) != OPR_ADD)
    return;

  WN *ldid = WN_kid0(add);
  WN *con = WN_kid1(add);
  if (WN_operator(ldid) == OPR_INTCONST)
  {
    WN *tmp = ldid;  ldid = con;  con = tmp;
  }

  if ((WN_operator(ldid) != OPR_LDID) ||
      (WN_operator(con) != OPR_INTCONST) ||
      (WN_const_val(con) == 0))
    return;

  if (WN_st(ldid) != scalar &&
      (WN_class(ldid) != CLASS_PREG ||
       PREG_To_ST_Map[WN_load_offset(ldid)] != scalar))
    return;

  INT64 inc_val = WN_const_val(con);
  
  /* Record that 'con' is the last seen increment value for
     'scalar' in the current scope. */
  INC_MAP::keyValuePair_p pair;
  OFFSET_MAP_KEY imkey = OFFSET_MAP_KEY(_depth, _id, NULL, scalar, 0);
  if (_inc_map.findPair(&imkey, pair))
  {
    if (xt_lazy_bias)
      inc_val += pair->value();

    if (trace_oc)
      fprintf(TFile, "<procinc %d:%" LLD_FMT "> changing last inc %" LLD_FMT " -> %" LLD_FMT "\n",
	      _depth, _id, pair->value(), inc_val);

    pair->value() = inc_val;
  }
  else
  {
    if (trace_oc)
      fprintf(TFile, "<procinc %d:%" LLD_FMT "> creating last inc %" LLD_FMT "\n",
	      _depth, _id, inc_val);

    OFFSET_MAP_KEY *nimkey = CXX_NEW(OFFSET_MAP_KEY(_depth, _id, NULL, scalar, 0), _pool);
    _inc_map.insert(nimkey, inc_val);
  }

  if (xt_lazy_bias)
    return;

  /* For each symbol updated with 'scalar'/'inc_val', try to bias the
     canonicalization so that this inc can be combined with it. */
  
  UL_MAP::keyValuePair_p scan;
  UL_MAP::iter iter(_map);
  while ((scan = iter.next()) != NULL)
  {
    UPDATE_SYM *usym = scan->value();
    if ((usym->Scope_Depth() == _depth) &&
	(usym->Scope_Id() == _id) &&
	(usym->Scalar() == scalar))
    {
      if (usym->Process_Increment(usym->St(), inc_val))
	Set_Biased(usym->St(), usym->Scalar(), usym->Scale());
    }
  }
}

void
UPDATE_LOOP::Process_Load_Store (WN *wn, WN *parent, OFFSET_CANON_SYM *oc_sym)
{
  /* We can't do anything if we are unable to represent the
     control-flow scope. */
  if (_depth > MAX_DEPTH)
    return;

  /* Find the offset canonicalization information for 'wn', and the
     updating symbol and scale. */
  OFFSET_WN *own = oc_sym->Last_Added_Wn();
  ST *st = oc_sym->St();
  Is_True((wn == own->Wn()) && (st != NULL), (""));

  WN *addr = ((WN_operator(wn) == OPR_ILOAD) ? WN_kid0(wn) : WN_kid1(wn));
  OFFSET_SCALE uscale = Find_Offset_Scalar(addr);
  
  ST *scalar;
  INT64 scale;

  if (!uscale.valid || !uscale.st || (uscale.scale == 0))
    scalar = st, scale = 1;
  else 
    scalar = uscale.st, scale = uscale.scale;
  
  /* If 'st'/'scalar' has already been biased in this loop, then don't
     bother to continue. */
  if (Biased(st, scalar, scale))
    return;
  
  /* Find, creating if necessary, the UPDATE_SYM object representing
     the incremented ST in the current scope. Register 'wn' with that
     object. */
  UPDATE_SYM *usym = Lookup(st, scalar, scale);
  if (!usym)
    usym = Create(st, scalar, scale);

  /* Find the last increment of 'uscale.st' in this scope. If there is
     one try to combine it with this load/store. */

  INT64 inc;
  OFFSET_MAP_KEY imkey = 
    OFFSET_MAP_KEY(_depth, _id, NULL, scalar, 0);
  if (!_inc_map.find(&imkey, &inc))
    inc = 0;
    
  if (usym->Process_Load_Store(wn, parent, own, scale, inc))
    Set_Biased(st, scalar, scale);
}


void
UPDATE_LOOP::Finish_Biasing (void)
{
  INC_MAP::keyValuePair_p inc_pair;
  INC_MAP::iter inc_iter(_inc_map);
  while ((inc_pair = inc_iter.next()) != NULL)
  {
    OFFSET_MAP_KEY *inc = inc_pair->key();
    UL_MAP::keyValuePair_p usym_pair;
    UL_MAP::iter usym_iter(_map);
    while ((usym_pair = usym_iter.next()) != NULL)
    {
      UPDATE_SYM *usym = usym_pair->value();
      if ((usym->Scope_Depth() == inc->_depth) &&
	  (usym->Scope_Id() == inc->_id) &&
	  (usym->Scalar() == inc->_scalar))
      {
	if (!Biased(usym->St(), usym->Scalar(), usym->Scale()) &&
            usym->Process_Increment(usym->St(), inc_pair->value()))
	  Set_Biased(usym->St(), usym->Scalar(), usym->Scale());
      }
    }
  }
}


/* 'body' is the block of statements in a loop. Collect the
   symbols/offsets referenced in the loop, and recursively find
   contained loops. */
static OFFSET_CANON_LOOP *
Collect_Loop_Offsets (WN *loop_wn, WN *body, OFFSET_CANON_LOOP *outer,
		      UPDATE_LOOP *update_loop, MEM_POOL *pool)
{
  if (!body)
    return NULL;
  
  Is_True(WN_opcode(body) == OPC_BLOCK,
	  ("expected BLOCK node, not %s", OPCODE_name(WN_opcode(body))));

  OFFSET_CANON_LOOP *loop =
    CXX_NEW(OFFSET_CANON_LOOP(loop_wn, WN_first(body), WN_last(body), outer, pool), pool);

  if (trace_oc)
  {    
    fprintf(TFile, "\nFound loop:\n");
    loop->Print(TFile);
    if (update_loop)
      update_loop->Print(TFile);
  }

  BOOL find_base = FALSE;
  
  /* Search for loads, stores, and ldas, collecting the referenced
     symbol and offset. */
  for (WN *node = WN_first(body); node; node = WN_next(node))
  {
    OPCODE opc = WN_opcode(node);
    OPERATOR opr = WN_operator(node);
    
    /* Recurse for inner loops... */
    if ((opc == OPC_DO_LOOP) ||	(opc == OPC_DO_WHILE) ||
	(opc == OPC_WHILE_DO))
    {
      WN *new_body = ((opc == OPC_DO_LOOP) ?
		      WN_do_body(node) : WN_while_body(node));

      /* Make a new update loop object if 'update_loop' is
         non-NULL. ('update_loop' == NULL means we aren't attempting
         to find updating loads/stores). */
      UPDATE_LOOP *new_update_loop = NULL;
      if (update_loop)
	new_update_loop = CXX_NEW(UPDATE_LOOP(pool), pool);

      Collect_Loop_Offsets(node, new_body, loop, new_update_loop, pool);

      /* DO_LOOPs have the increment stored as a kid, not in the body,
         so process it after the body. */
      if (opc == OPC_DO_LOOP)
	loop->Record_Symbols(WN_step(node), NULL, new_update_loop, find_base, pool);
      if (xt_lazy_bias && update_loop)
        new_update_loop->Finish_Biasing();

      continue;
    }
    /* Recurse into blocks, IFs, and regions. */
    else if (opr == OPR_REGION)
    {
      Collect_Loop_Offsets(node, WN_kid2(node), loop, update_loop, pool);
      continue;
    }
    else if (opr == OPR_BLOCK)
    {
      Collect_Loop_Offsets(node, node, loop, update_loop, pool);
      continue;
    }
    else if (opr == OPR_IF)
    {
      loop->Record_Symbols(WN_if_test(node), NULL, update_loop, find_base, pool);

      if (update_loop)
	update_loop->start_then();

      Collect_Loop_Offsets(node, WN_then(node), loop, update_loop, pool);

      if (update_loop)
      {
	update_loop->end_then();
	update_loop->start_else();
      }

      Collect_Loop_Offsets(node, WN_else(node), loop, update_loop, pool);

      if (update_loop)
	update_loop->end_else();

      continue;
    }
    /* Skip ASM_STMTs since they contain nothing we are interested in
       (but do contain a block which will cause Record_Symbols to
       fail. */
    else if (opr == OPR_ASM_STMT)
    {
      continue;
    }
    
    /* For COMPGOTO, we only want to visit kid0, since kid1 and kid2
       are just the jump table. Otherwise visit all the children to
       collect symbol/offset references. */
    if (opr == OPR_COMPGOTO)
      loop->Record_Symbols(WN_kid0(node), node, update_loop, find_base, pool);
    else
      loop->Record_Symbols(node, NULL, update_loop, find_base, pool);
  }

  return loop;
}


/* 'body' is the block of statements in a loop. Collect the
   symbols/offsets referenced in the loop, and recursively find
   contained loops. Same as "Collect_Loop_Offsets" except that we
   can't assume OPC_DO_LOOP, OPC_DO_WHILE, and OPC_WHILE_DO are
   present, and so must find the loops by looking for back edges. */
static OFFSET_CANON_LOOP *
Collect_Loop_Offsets_Flat (WN *loop_wn, WN *first, WN *last,
			   OFFSET_CANON_LOOP *outer, MEM_POOL *pool)
{
  if (!first || !last)
    return NULL;
  
  OFFSET_CANON_LOOP *loop =
    CXX_NEW(OFFSET_CANON_LOOP(loop_wn, first, last, outer, pool), pool);

  if (trace_oc)
  {    
    fprintf(TFile, "Found flat loop:\n");
    loop->Print(TFile);
  }

  /* Search for loads, stores, and ldas, collecting the referenced
     symbol and offset. */
  for (WN *node = first; node != last; node = WN_next(node))
  {
    OPCODE opc = WN_opcode(node);

    /* A label with LOOP_INFO attached indicates the start of a
       loop. Search forward to find the end as a branch back to this
       label. Stop if we reach 'last' or the end of the function. */

    if (WN_operator(node) == OPR_LABEL)
    {
      WN *loop_info = WN_label_loop_info(node);
      if (loop_info)
      {
	INT32 label_num = WN_label_number(node);

	WN *new_first = WN_next(node);
	WN *new_last = new_first;
	while (new_last && (new_last != last))
	{
	  OPERATOR opr = WN_operator(new_last);
	  if (((opr == OPR_GOTO) || (opr == OPR_FALSEBR) || (opr == OPR_TRUEBR)) &&
	      (WN_label_number(new_last) == label_num))
	  {
	    Collect_Loop_Offsets_Flat(loop_info, new_first, new_last, loop, pool);
	    break;
	  }

	  new_last = WN_next(new_last);
	}

	/* If we didn't find the end of the loop, then just continue
           processing statements as if they were in the current loop,
           otherwise continue with the statements after the loop. */

	if (new_last && (new_last != last))
	  node = new_last;
      }
    }
    /* Recurse into regions. */
    else if (WN_operator(node) == OPR_REGION)
    {
      Collect_Loop_Offsets_Flat(node, WN_first(WN_kid2(node)), WN_last(WN_kid2(node)), loop, pool);
      continue;
    }
    /* Skip ASM_STMTs since they contain nothing we are interested in
       (but do contain a block which will cause Record_Symbols to
       fail. */
    else if (WN_operator(node) == OPR_ASM_STMT)
    {
      continue;
    }

    /* For COMPGOTO, we only want to visit kid0, since kid1 and kid2
       are just the jump table. Otherwise visit all the children to
       collect symbol/offset references. */
    if (WN_operator(node) == OPR_COMPGOTO)
      loop->Record_Symbols(WN_kid0(node), node, NULL, TRUE, pool);
    else
      loop->Record_Symbols(node, NULL, NULL, TRUE, pool);
  }

  return loop;
}


/* Recursively find canonical offsets for 'loop' and for all of
   'loop's inner loops. */
static void
Find_Canonical_Offsets (OFFSET_CANON_LOOP *loop, MEM_POOL *pool)
{
  /* The order in which we attempt to combine loop offset
     canonicalizations could use some experimentation, and could also
     probably benefit from profile information. For now I simply
     attempt to combine each inner loop with 'loop' in the order they
     are processed. */

  /* Recursively visit each inner loop. Try to combine the symbols in
     'loop' with the same symbols in the inner loop so that they can
     use the same canonicalized offset. */

  // When -TARG:pseudo_loop=off don't merge and finalize
  // offsets for the psuedo loop at the function level

  OFFSET_CANON_LOOP_LIST *inner_list = loop->Inner_Loops();
  while (inner_list)
  {
    OFFSET_CANON_LOOP *inner = inner_list->_ocl;
    Find_Canonical_Offsets(inner, pool);
    if (xt_pseudo_loop || loop->Outer_Loop())
      loop->Merge_Canonical_Offsets(inner);
    inner_list = inner_list->_next;
  }

  /* Finalize the canonical offsets for 'loop'. */
  if (xt_pseudo_loop || loop->Outer_Loop())
    loop->Finalize_Canonical_Offsets(pool);
}


// Build PREG to ST mapping for those PREGs that have unique ST
// for which there's a statement
//   LDID st
// STID preg
//
static void
Build_PREG_To_ST_Map(WN *pu_tree, BOOL trace)
{
  for (WN_ITER *wni = WN_WALK_StmtIter(pu_tree);
       wni && WN_ITER_wn(wni);
       wni = WN_WALK_StmtNext(wni)) {
    WN *wn = WN_ITER_wn(wni);
    if (WN_operator(wn) == OPR_STID && 
        WN_class(wn) == CLASS_PREG &&
        TY_kind(WN_ty(wn)) == KIND_POINTER) {
      WN *kid = WN_kid0(wn);
      if (WN_operator(kid) == OPR_LDID &&
          WN_class(kid) == CLASS_VAR &&
          TY_kind(WN_ty(kid)) == KIND_POINTER &&
          WN_load_offset(kid) == 0) {
        PREG_NUM preg_num = WN_store_offset(wn);
        if (!PREG_To_ST_Map[preg_num]) {
          PREG_To_ST_Map[preg_num] = WN_st(kid);
          if (trace)
            fprintf(TFile,"PREG %d -> ST %s\n", preg_num, ST_name(WN_st(kid)));
        }
        else if (PREG_To_ST_Map[preg_num] != (ST*)-1 &&
                 PREG_To_ST_Map[preg_num] != WN_st(kid)) {
          PREG_To_ST_Map[preg_num] = (ST*)-1;
          if (trace)
            fprintf(TFile,"PREG %d maps to multiple STs\n", preg_num);
        }
      }
    }
  }
}


/* If 'st' is a PREG, try to find ST represented by the preg and
   return it. Otherwise return NULL. */
static ST *
Find_Preg_St (ST *st, INT64 offset)
{
  if (ST_sym_class(st) == CLASS_PREG)
  {
    Is_True(offset < PREG_To_ST_Map_Sz,
	    ("unexpected preg %" LLD_FMT " >= %d\n", offset, PREG_To_ST_Map_Sz));

    if ((offset < PREG_To_ST_Map_Sz) &&	PREG_To_ST_Map[offset] &&
	(PREG_To_ST_Map[offset] != (ST*)-1))
      return PREG_To_ST_Map[offset];
  }

  return NULL;
}


/* For each load and store whirl node, find the canonical symbol
   offset we want to use for that node. */
void
Init_Offset_Canonicalization (WN *func, MEM_POOL *pool, BOOL have_do_while, BOOL trace)
{
  FmtAssert(WN_operator(func) == OPR_FUNC_ENTRY, ("expecting OPR_FUNC_ENTRY"));

  trace_oc = trace;
  
  if (trace_oc)
    fprintf(TFile, "%sCanonicalize Offsets\n%s", DBar, DBar);

  // Build a map to go from a PREG to an ST
  // in order to recognize cases like *p++
  PREG_To_ST_Map_Sz = Get_Preg_Num(PREG_Table_Size(CURRENT_SYMTAB))+1;
  PREG_To_ST_Map = (ST**) alloca(PREG_To_ST_Map_Sz * sizeof(ST*));
  memset((char*)PREG_To_ST_Map, 0, PREG_To_ST_Map_Sz * sizeof(ST*));
  Build_PREG_To_ST_Map(func, trace);
  
  /* Create and initalize the map that we use to associate offset
     canonicalization information with each WN. */

  ofc_wn_map = WN_MAP_Create(pool);
  
  /* Recursively visit each loop body, collecting the symbols and
     offsets used in the loop. If 'have_do_while' is true, we can
     assume that DO_LOOP, DO_WHILE, and WHILE_DO nodes have not yet
     been lowered. */

  OFFSET_CANON_LOOP *loops;
  
  if (have_do_while)
  {
    UPDATE_LOOP *update_loop = NULL;
    if (TI_ISA_Has_Updating_Ops())
      update_loop = CXX_NEW(UPDATE_LOOP(pool), pool);

    loops = Collect_Loop_Offsets(func, WN_func_body(func), NULL, update_loop, pool);
  }
  else
  {
    WN *body = WN_func_body(func);
    loops = Collect_Loop_Offsets_Flat(func, WN_first(body), WN_last(body), NULL, pool);
  }

  /* Visit each loop, innermost first, and determine a canonical
     offset for each symbol/loop (i.e. a symbol can have different
     canonicalizations in different loops). */

  Find_Canonical_Offsets(loops, pool);

  /* Build map from WN to canonical offset information for that WN. */
  loops->Build_Canonical_Map();
}


void
Finalize_Offset_Canonicalization (void)
{
  Is_True(ofc_wn_map != WN_MAP_UNDEFINED, (""));
  
  if (trace_oc)
    fprintf(TFile, "%sFinalize Canonicalize Offsets\n%s", DBar, DBar);

  WN_MAP_Delete(ofc_wn_map);
  ofc_wn_map = WN_MAP_UNDEFINED;
  trace_oc = FALSE;
}


/* Recursively search 'tree' and attempt to find a ST and offset. */
static OFFSET_ST
Find_Offset_St (WN *tree)
{
  OPCODE opc = WN_opcode(tree);
  OPERATOR opr = WN_operator(tree);

  /* For a U4LDA, or LDID of a pointer type, return the symbol and
     offset. */
  if (opc == OPC_U4LDA)
  {
    return OFFSET_ST(WN_st(tree), WN_lda_offset(tree));
  }
  else if (opr == OPR_LDID)
  {
    TY_IDX ty = WN_ty(tree);
    if (TY_kind(Ty_Table[ty]) == KIND_POINTER)
    {
      ST *preg_st = Find_Preg_St(WN_st(tree), WN_load_offset(tree));
      if (preg_st)
	return OFFSET_ST(preg_st, 0);
      else
	return OFFSET_ST(WN_st(tree), WN_load_offset(tree));
    }

    return OFFSET_ST();
  }

  /* 'tree' is an ADD. If one, and only one, of the children has a
     valid symbol, then add its offset to the other child if it is
     valid. */
  if (opr == OPR_ADD)
  {
    OFFSET_ST p0 = Find_Offset_St(WN_kid0(tree));
    OFFSET_ST p1 = Find_Offset_St(WN_kid1(tree));

    if (p0.valid && (!p1.valid || !p1.st))
      return OFFSET_ST(p0.st, p0.offset + ((p1.valid) ? p1.offset : 0));
    else if (p1.valid && (!p0.valid || !p0.st))
      return OFFSET_ST(p1.st, p1.offset + ((p0.valid) ? p0.offset : 0));
      
    return OFFSET_ST();
  }

  /* 'tree' is a MPY. If both children aren't valid, then we
     assume that this multiply is scaling the array index, and so we
     return invliad. Otherwise one, and only one, child must have a
     symbol. */
  if (opr == OPR_MPY)
  {
    OFFSET_ST p0 = Find_Offset_St(WN_kid0(tree));
    OFFSET_ST p1 = Find_Offset_St(WN_kid1(tree));

    if (p0.valid && p1.valid)
    {
      if (p0.st && !p1.st)
	return OFFSET_ST(p0.st, p0.offset * p1.offset);
      else if (!p0.st)
	return OFFSET_ST(p1.st, p0.offset * p1.offset);
    }
      
    return OFFSET_ST();
  }

  // SUB -- same as ADD but subtract offset instead of adding it
  if (opr == OPR_SUB)
  {
    OFFSET_ST p0 = Find_Offset_St(WN_kid0(tree));
    OFFSET_ST p1 = Find_Offset_St(WN_kid1(tree));

    if (p0.valid && (!p1.valid || !p1.st))
      return OFFSET_ST(p0.st, p0.offset - ((p1.valid) ? p1.offset : 0));
    else if (p1.valid && (!p0.valid || !p0.st))
      return OFFSET_ST(p1.st, -p1.offset + ((p0.valid) ? p0.offset : 0));
      
    return OFFSET_ST();
  }

  // NEG -- if kid is valid, negate its offset
  if (opr == OPR_NEG) 
  {
    OFFSET_ST p0 = Find_Offset_St(WN_kid0(tree));

    if (p0.valid && p0.st)
      return OFFSET_ST(p0.st, -p0.offset);

    return OFFSET_ST();
  }

  // Noop CVTs -- ignore them
  if (opc == OPC_U4I4CVT || opc == OPC_I4U4CVT)
    return Find_Offset_St(WN_kid0(tree));

  /* Return constants... */
  if (opr == OPR_INTCONST)
    return OFFSET_ST(NULL, WN_const_val(tree));

  return OFFSET_ST();
}

  
/* Recursively search 'tree' to find the updateable symbol and scaling
   for that symbol. */
static OFFSET_SCALE
Find_Offset_Scalar (WN *tree)
{
  OPCODE opc = WN_opcode(tree);
  OPERATOR opr = WN_operator(tree);

  /* For an LDID of a non-pointer type, return the symbol. */
  if (opr == OPR_LDID)
  {
    TY_IDX ty = WN_ty(tree);
    if (TY_kind(Ty_Table[ty]) == KIND_SCALAR)
      return OFFSET_SCALE(WN_st(tree), 1);

    return OFFSET_SCALE();
  }

  /* 'tree' is an ADD. If one, and only one, of the children has a
     valid symbol, then propagate it. */
  if (opr == OPR_ADD)
  {
    OFFSET_SCALE p0 = Find_Offset_Scalar(WN_kid0(tree));
    OFFSET_SCALE p1 = Find_Offset_Scalar(WN_kid1(tree));

#if 0
    fprintf(TFile, "add\n");
    fprintf(TFile, "valid %d, st %x, scale %" LLD_FMT "\n", p0.valid, p0.st, p0.scale);
    fdump_wn(TFile, WN_kid0(tree));
    fprintf(TFile, "valid %d, st %x, scale %" LLD_FMT "\n", p1.valid, p1.st, p1.scale);
    fdump_wn(TFile, WN_kid1(tree));
#endif
      
    if (p0.valid && p0.st && (!p1.valid || !p1.st))
      return p0;
    else if (p1.valid && p1.st && (!p0.valid || !p0.st))
      return p1;
    /* If both are valid, pick the one with smaller scale.  */
    else if (p1.valid && p1.st && p0.valid && p0.st)
      return p0.scale < p1.scale ? p0 : p1;
          
    return OFFSET_SCALE();
  }

  /* 'tree' is a MPY. If one, and only one, of the children has a
     valid symbol, and the other has a valid scale, then multiply the
     scales and propagate. */
  if (opr == OPR_MPY)
  {
    OFFSET_SCALE p0 = Find_Offset_Scalar(WN_kid0(tree));
    OFFSET_SCALE p1 = Find_Offset_Scalar(WN_kid1(tree));

#if 0
    fprintf(TFile, "mul\n");
    fprintf(TFile, "valid %d, st %x, scale %" LLD_FMT "\n", p0.valid, p0.st, p0.scale);
    fdump_wn(TFile, WN_kid0(tree));
    fprintf(TFile, "valid %d, st %x, scale %" LLD_FMT "\n", p1.valid, p1.st, p1.scale);
    fdump_wn(TFile, WN_kid1(tree));
#endif

    if (p0.valid && p1.valid)
    {
      if (p0.st && !p1.st)
	return OFFSET_SCALE(p0.st, p0.scale * p1.scale);
      else if (p1.st && !p0.st)
	return OFFSET_SCALE(p1.st, p0.scale * p1.scale);
    }
      
    return OFFSET_SCALE();
  }

  // SUB -- same as ADD, but might need to change sign of the scale
  if (opr == OPR_SUB)
  {
    OFFSET_SCALE p0 = Find_Offset_Scalar(WN_kid0(tree));
    OFFSET_SCALE p1 = Find_Offset_Scalar(WN_kid1(tree));

    if (p0.valid && p0.st && (!p1.valid || !p1.st))
      return p0;
    else if (p1.valid && p1.st && (!p0.valid || !p0.st))
      return OFFSET_SCALE(p1.st, -p1.scale);
      
    return OFFSET_SCALE();
  }

  // NEG -- if kid is valid, negate its scale
  if (opr == OPR_NEG)
  {
    OFFSET_SCALE p0 = Find_Offset_Scalar(WN_kid0(tree));

    if (p0.valid && p0.st)
      return OFFSET_SCALE(p0.st, -p0.scale);
      
    return OFFSET_SCALE();
  }

  // Noop CVTs -- ignore them
  if (opc == OPC_U4I4CVT || opc == OPC_I4U4CVT)
    return Find_Offset_Scalar(WN_kid0(tree));

  /* Return constants... */
  if (opr == OPR_INTCONST)
    return OFFSET_SCALE(NULL, WN_const_val(tree));

  return OFFSET_SCALE();
}    


/* Remove wn from the offset canonicalizer since it is deleted by WN_Lower() */
void Delete_Offset_Wn_Delete_Tree(WN* wn) {

  if (ofc_wn_map != WN_MAP_UNDEFINED)
  {
    OFFSET_WN *own = (OFFSET_WN *)WN_MAP_Get(ofc_wn_map, wn);
    if (own) {
      OFFSET_CANON_WN *ofc = own->Offset_Canon_Wn();
      if (ofc) {
        if (trace_oc)
          {
            fprintf(TFile, "  remove ");
	    fdump_wn(TFile, wn);
            fprintf(TFile, "  from ");
            ofc->Print(TFile, 4);
          }

        /* Remove 'own' from the current canonicalization */
        ofc->Remove_Wn(own);
      }
    }
  }
}

/* Return the range of offsets that can be accessed as 'type' from
   'wn'. 'parent' optionally gives the immediate parent of 'wn'. */
static BOOL
Memory_Offset_Span (TYPE_ID type, WN *parent, WN *wn, OFFSET_SPAN *span)
{
  INT32 min, max;

  //if (parent && WN_operator(parent) == OPR_PARM) {
  if (0) {
    min = 0;
    max = 0;
  } else {
    switch (type)
    {
    case MTYPE_I1:
    case MTYPE_U1:
      min = 0;
      max = 255;
      break;
      
    case MTYPE_I2:
    case MTYPE_U2:
      min = 0;
      max = 510;
      break;
  
    case MTYPE_I4:
    case MTYPE_U4:
    case MTYPE_F4:
      min = 0;
      max = 1020;
      break;
  
    case MTYPE_V:
      /* If 'type' is MTYPE_V, then assume that we will be using the low
         portion of the offset in an addi. */
      min = -128;
      max = 127;
      break;
  
    default:
      if (!MTYPE_is_tie(type))
      {
        min = 0;
        max = 1020;
      }
      else
      {
        /* If we don't have 'wn', then we can't tell what prototype we
           need to check to determine the valid range of offsets. */
  
        if (!wn)
	  return FALSE;
        
        /* Find the prototype (macro) that corresponds to 'wn'. */
  
        OPCODE opc = WN_opcode(wn);
        TIE_MACRO_p macro = NULL;
        
        if (OPCODE_is_load(opc))
        {
	  /* If the parent of a load is a CVT, then use the appropriate
             mtor rule, if there is one. */
  
	  if (parent && WN_operator(parent) == OPR_CVT)
	  {
	    macro = tie_info->mtype_mtor_macro(WN_desc(parent), WN_rtype(parent));
	  }
  
	  if (!macro)
	    macro = tie_info->mtype_loadi_macro(type);
        }
        else if (OPCODE_is_store(opc))
        {
	  /* If the stored value is a CVT, then use the appropriate rtom
             rule, if there is one. */
  
	  WN *kid = WN_kid0(wn);
	  if (WN_operator(kid) == OPR_CVT)
	  {
	    macro = tie_info->mtype_rtom_macro(WN_desc(kid), WN_rtype(kid));
	  }
  
	  if (!macro)
	    macro = tie_info->mtype_storei_macro(type);
        }
  
        if (!macro)
	  return FALSE;
        
        /* Get the range of immediates allowed for 'macro'. If 'macro'
           doesn't have an offset, then only 0 is allowed. */
  
        if (macro->num_protos() < 3)
	  min = max = 0;
        else if (!macro->immediate_range(2, &min, &max))
	  return FALSE;
      }
    }
  }

  *span = OFFSET_SPAN(min, max);
  return TRUE;
}


/* Return the portion of 'offset' that cannot be represented in a
   memory operation accessing an object of type 'type'. */
static INT64
Hi_Memory_Offset (TYPE_ID type, INT64 offset, WN *parent, WN *wn)
{
  if (trace_oc)
  {
    fprintf(TFile, "<hioff> %s, offset %" LLD_FMT ", ",
	    MTYPE_name(type), offset);
    fdump_wn(TFile, wn);
    if (parent)
    {
      fprintf(TFile, "\t");
      fdump_wn(TFile, parent);
    }
  }

  /* If we have a canonical offset already determined for 'wn', use
     that. If the canonicalized offset does not match 'offset', then
     we need to re-canonicalize 'wn'. */
  if (ofc_wn_map != WN_MAP_UNDEFINED)
  {
    OFFSET_WN *own = (OFFSET_WN *)WN_MAP_Get(ofc_wn_map, wn);
    if (own && (own->Offset() != offset))
    {
      OFFSET_CANON_WN *ofc = own->Offset_Canon_Wn();
      if (trace_oc)
        {
          fprintf(TFile, "  needs adjustment (actual offset %" LLD_FMT " != canon offset %" LLD_FMT ")\n",
                  offset, own->Offset());
          ofc->Print(TFile, 4);
        }

      /* Remove 'own' from the current canonicalization,
         'ofc'. *Don't* re-finalize 'ofc' because we don't want it's
         canonicalization to change because some accesses might
         already be using it (we might be tempted to refinalize
         because with 'own' removed, we might get a different
         canonicalization. */
      ofc->Remove_Wn(own);

      /* Modify 'own' to have the new 'offset'... */
      own->Offset() = offset;

      /* Add 'own' back to the appropriate canon symbol. If we had to
         create a new canonicalization, then finalize it so that 'own'
         is canonicalized correctly for the new 'offset'. */
      OFFSET_CANON_SYM *ocs = ofc->Offset_Canon_Symbol();
      OFFSET_CANON_WN *new_ofc = ocs->Finalize_One_Offset(own);
      if (new_ofc->Num_Wns() == 1)
        new_ofc->Finalize_Offset();

      if (trace_oc)
        {
          fprintf(TFile, "  new canonicalization:\n");
          new_ofc->Print(TFile, 4, TRUE);
        }
    }

    if (own)
      {
        if (trace_oc)
          own->Offset_Canon_Wn()->Print(TFile, 4);
          
        return own->Offset_Canon_Wn()->Canon_Offset();
      }
  }
  else
    {
      // don't perform canonicalization in preopt.
      return 0;
    }

  /* We don't have a canonical offset determined globally, so just
     find one from the local information we have. */
  
  OFFSET_SPAN off_span;
  if (!Memory_Offset_Span(type, parent, wn, &off_span))
    return 0;

  INT32 min = off_span.offset_min;
  INT32 max = off_span.offset_max;
    
  /* Get the "span" of offsets accessable. We assume the offset range
     accessable is the smallest power of 2 greater than this span. */

  UINT32 span = (((max - min) == 0) ? 0 : nearest_power_of_two(max - min));

  /* If 'offset' fits within the allowable range, then set the
     canonical offset to 0. This avoids unnecessary base pointer
     adjustments for objects that are accessible with the offset. */

  if ((offset >= min) && (offset <= max))
    return 0;

  INT64 offset_sign = (offset >= 0) ? 1 : -1;
  /* If allowable range spans 0, then set the high portion of the
     offset to be ..., -2*span, -span, 0, span, 2*span, .... */
  
  if ((min < 0) && (max > 0))
    return ((offset + offset_sign * (span / 2)) / span) * span;

  /* If allowable range is 0 - span, then set the high portion of the
     offset to be ..., -3/2*span, -1/2*span, 1/2*span, 3/2*span,
     .... The high portion must be set so that the remainder is
     positive. */

  if ((min >= 0) && (max > 0))
    return (((offset + offset_sign * (span / 2)) / span) * span) - (span / 2);

  /* If allowable range is -span - 0, then set the high portion of the
     offset to be ..., -3/2*span, -1/2*span, 1/2*span, 3/2*span,
     .... The high portion must be set so that the remainder is
     negative. */
 
  if ((min < 0) && (max <= 0))
    return (((offset + offset_sign * (span / 2)) / span) * span) + (span / 2);

  return 0;
}


INT64
Canonicalize_Memory_Offset (WN *wn, WN *parent, INT64 offset)
{
  const OPCODE op = WN_opcode(wn);
  TYPE_ID type = OPCODE_desc(op);
  if (type == MTYPE_V) 
    type = OPCODE_rtype(op);

  return Hi_Memory_Offset(type, offset, parent, wn);
}


INT32
Memory_Offset_Hi (TYPE_ID type, INT64 offset, WN *parent, WN *wn)
{
  return Hi_Memory_Offset(type, offset, parent, wn);
}


INT32
Memory_Offset_Lo (TYPE_ID type, INT64 offset, WN *parent, WN *wn)
{
  return offset - Hi_Memory_Offset(type, offset, parent, wn);
}


BOOL
Memory_Offset_Must_Split (TYPE_ID type, INT64 offset, WN *parent, WN *wn)
{
  return Hi_Memory_Offset(type, offset, parent, wn) != 0;
}



/* Indicate if the specified operation can have an immediate operand.
 */
BOOL Can_Be_Immediate(OPERATOR opr,
		      INT64 val,
		      TYPE_ID dtype,
		      INT whichkid,
		      ST *stid_st)
{
  TOP top;
  
  switch (opr)
  {
  case OPR_AGOTO:	// leave a constant condition here alone
  case OPR_LOOP_INFO:	// leave the constant trip-count alone
    return TRUE;
    
    /* Branches on xtensa are tricky since they have a small
       non-contiguous range of immediates. We select the topcode that
       will be used during code selection and try to fit the immediate
       in that opcode, letting TI_Equivalent_Immed_Branch worry about all
       the possible tweaking. */
  case OPR_EQ:
    top = TOP_beq;
    return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));

  case OPR_NE:
    top = TOP_bne;
    return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));

  case OPR_GE:
    if (MTYPE_is_signed(dtype))
    {
      top = TOP_bge;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));
    }
    else
    {
      top = TOP_bgeu;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));
    }

  case OPR_GT:
    if (MTYPE_is_signed(dtype))
    {
      top = TOP_blt;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 1));
    }
    else
    {
      top = TOP_bltu;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 1));
    }

  case OPR_LE:
    if (MTYPE_is_signed(dtype))
    {
      top = TOP_bge;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 1));
    }
    else
    {
      top = TOP_bgeu;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 1));
    }

  case OPR_LT:
    if (MTYPE_is_signed(dtype))
    {
      top = TOP_blt;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));
    }
    else
    {
      top = TOP_bltu;
      return TI_Equivalent_Immed_Branch(&top, &val, (whichkid == 0));
    }
    
  case OPR_ASHR:
  case OPR_LSHR:
    return (whichkid == 1) && TI_ISA_LC_Value_In_Class(val, LC_uimm5);

  case OPR_SHL:
    return (whichkid == 1) && TI_ISA_LC_Value_In_Class(val, LC_msalp32);

  case OPR_MLOAD:
    return whichkid == 1;

  case OPR_MSTORE:
    return whichkid == 2;

  case OPR_BAND:
    // leave extui decision to CG instead of in WOPT
    return FALSE;
    
  case OPR_SUB:
    /* If the constant is the second operand, then we can add -val. */
    if (whichkid != 1)
      return FALSE;
    val = -val;
    // fall-through

  case OPR_ADD:
    return TI_ISA_LC_Value_In_Class(val, LC_simm8) || TI_ISA_LC_Value_In_Class(val, LC_simm8x256);

  case OPR_REM:
  case OPR_MOD:
  case OPR_DIVREM:
  case OPR_DIV:
    /* Since these are all (currently) done with intrinsic calls, we
       can handle any immediate. */
    return TRUE;

  case OPR_MPY:
    /* We either do shifts and adds or use an intrinsic call, so allow
       all immediates. */
    return TRUE;
    
  case OPR_CALL:
  case OPR_ICALL:
  case OPR_INTRINSIC_CALL:
  case OPR_PARM:
    /* calls end up storing their constant parameters to dedicated
       registers, which can be quicker if they're left in place as
       ldimm's. */
    return TRUE;
    
  case OPR_STID:
    /* is this in a store to a register, which usually means in
       preparation for a call, or return value, so just let us
       generate the stid/load-immediate in place if it fits. */
    return ST_class(stid_st) == CLASS_PREG;
  }

  return FALSE;
}


/* ==================================================================== 
 * 
 * 16-bit multiply identification
 *
 * ==================================================================== */


/* ==================================================================== 
 * 
 *  bool wn_is_sixteen_bits(WN * node) 
 *
 *  helper function for the mul16 detection.
 *
 *  given a whirl node, return if its rdesc or its mtype fits in 
 *  sixteen bits or less 
 * 
 * ==================================================================== */

static bool
wn_is_sixteen_bits(WN * node) 
{
  OPERATOR op = WN_operator( node );
  if ( op == OPR_CVTL ) {
    return (WN_offset( node ) <= 16);
  }

  switch ( WN_rtype( node ) ) {
  case MTYPE_U1:
  case MTYPE_I1:
  case MTYPE_U2:
  case MTYPE_I2:
    return TRUE;
    break;
  }

  switch ( WN_desc( node ) ) {
  case MTYPE_U1:
  case MTYPE_I1:
  case MTYPE_U2:
  case MTYPE_I2:
    return TRUE;
    break;
  }

  return FALSE;
}

/* ==================================================================== 
 * 
 *  bool wn_is_eight_bits(WN * node) 
 *
 *  helper function for the mul16 detection.
 *
 *  given a whirl node, return if its rdesc or its mtype fits in 
 *  eight bits 
 * 
 * ==================================================================== */

static bool
wn_is_eight_bits(WN * node) 
{
  OPERATOR op = WN_operator( node );
  if ( op == OPR_CVTL ) {
    return (WN_offset( node ) <= 8);
  }

  switch ( WN_rtype( node ) ) {
  case MTYPE_U1:
  case MTYPE_I1:
    return TRUE;
    break;
  }

  switch ( WN_desc( node ) ) {
  case MTYPE_U1:
  case MTYPE_I1:
    return TRUE;
    break;
  }

  return FALSE;
}

// return true if wn1 and wn2 are to the same scalar location
static BOOL Same_Scalar_Location(WN *store, WN *load) 
{
    if (WN_operator(store) != OPR_STID) return FALSE;
    if (WN_operator(load) != OPR_LDID) return FALSE;
    if (WN_st(load) != WN_st(store)) return FALSE;
    if (WN_offset(load) != WN_offset(store)) return FALSE;
    if (WN_rtype(load) != WN_desc(store)) return FALSE;
    return TRUE;
}

/* ====================================================================
 *
 *  bool operand_fits_mul16(WN * node, WN *enclosing_statement, DU_MANAGER *du_mgr, 
 *			ALIAS_MANAGER *alias_mgr, WN_HASH *wn_hash)
 *
 * given an operand to a multiply, return whether or not it's operands are 
 * 16 bit variables.
 *
 * ie, it might be an LDID of a short, or it might be an LDID of a preg
 * representing a short, or it might be the result of a cast to a 
 * short.
 *
 * short a, b, c;
 * 
 * a * b               //can use mul16
 * (a + b) * c         //can't
 * (short)(a + b) * c  //can
 *
 *  If du_mgr is set and the operand is a 32 bit load, check all it's
 *  definitions since they might be 16 bit safe
 *  Otherwise if alias_mgr is set, do a hackish scan of the basic block 
 *  to search for definitions
 *
 *  
 *
 * ==================================================================== */



static bool
operand_fits_mul16(WN * node, WN *enclosing_statement, DU_MANAGER *du_mgr, 
			ALIAS_MANAGER *alias_mgr, WN_HASH *wn_hash) 
{
  OPERATOR opr = WN_operator(node);


  /* we may have a cast to or from a 16 bit representable value or
     just loaded a 16 bit representable value.
  */
  
  if ( ( opr == OPR_CVT  || 
	 opr == OPR_CVTL || 
	 opr == OPR_LDID || 
	 opr == OPR_ILOAD || 
	 opr == OPR_ISTORE || 
	 opr == OPR_STID) //the stid is for a store to a preg
       && wn_is_sixteen_bits( node ) ) {
    return TRUE;
  }

  /* if 'node' is OPR_BAND and one of the children is a constant that
     masks to 16 bits, then 'node' produces only 16 bits. */

  if (opr == OPR_BAND)
  {
    bool uns  = MTYPE_is_unsigned(WN_rtype(node));
    WN *mask = NULL;
    if (WN_operator(WN_kid0(node)) == OPR_INTCONST)
      mask = WN_kid0(node);
    else if (WN_operator(WN_kid1(node)) == OPR_INTCONST)
      mask = WN_kid1(node);

    if (uns && mask && ((WN_const_val(mask) & 0x0ffff) == WN_const_val(mask)))
      return TRUE;
    else if (mask && ((WN_const_val(mask) & 0x07fff) == WN_const_val(mask)))
      return TRUE;
  }
  
  /* if 'node' is OPR_LSHR or OPR_ASHR and one of the children is a
     constant that shifts out all but 16 bits of the other child, then
     'node' produces only 16 bits. */

  if ((opr == OPR_ASHR) || (opr == OPR_LSHR))
  {
    WN *shift = WN_kid1(node);
    if (WN_operator(shift) == OPR_INTCONST)
    {
      WN *shiftee = WN_kid0(node);
      UINT32 size = MTYPE_bit_size(WN_rtype(shiftee));
      if ((size - WN_const_val(shift)) <= 16)
	return TRUE;
    }
  }
  
  /* it may be an int constant that fits */
  if (opr == OPR_INTCONST) {
      INT64 c   = WN_const_val(node);
      switch ( WN_rtype( node ) ) {
      case MTYPE_I1:
      case MTYPE_I2:
      case MTYPE_I4:
      case MTYPE_I8:
	//the literals here are thirty-two bit reps of sixteen bit limits
	if ((c >= -32768) && (c <= 32767)) {
	  return TRUE;
	}
	break;
      case MTYPE_U1:
      case MTYPE_U2:
      case MTYPE_U4:
      case MTYPE_U8:
	/* it is a pretty safe bet that an unsigned number is >= 0 */
	if (c <= 65535) {
	  return TRUE;
	}
	break;
      }
  }

  /* we may have an ldid of a preg which represents a 16 bit value.
     to check this, we just see of the preg's home fits any of 
     the other cases.
  */

  if (opr == OPR_LDID && WN_class( node ) == CLASS_PREG ) {
    WN * preg_node = Preg_Home( WN_load_offset( node ) );
    if( preg_node ) {
      if ( operand_fits_mul16( preg_node , NULL, NULL, NULL, NULL) )
	return TRUE;
    }
  }

  // Look at the defs of the operand, if they are all 16 bit safe then the
  // operand is 16 bit safe
  if (wn_hash && opr == OPR_LDID && 
	MTYPE_is_integral(OPCODE_rtype(WN_opcode(node)))) {
    if (du_mgr) {
      BOOL found_problem = FALSE;
      DEF_LIST* def_list=du_mgr->Ud_Get_Def(node);
      if (!def_list->Incomplete()) {
        DEF_LIST_ITER iter(def_list);
        const DU_NODE* node1 = NULL;
        for (node1 = iter.First(); !iter.Is_Empty() && !found_problem; 
						  node1 = iter.Next()) {
          WN* def = node1->Wn();
          if (WN_operator(def) != OPR_STID) {
                 found_problem = TRUE;
          } else {
	    if (!wn_hash->Find(def)) {  // we haven't seen this def before
                wn_hash->Enter(def,TRUE);
                if (!operand_fits_mul16(WN_kid0(def),def, du_mgr,alias_mgr, wn_hash)) {
                    found_problem = TRUE;
                }
            }
          }
        }
        if (!found_problem) return TRUE;
      }
    } else if (alias_mgr) {
	WN *tmp=WN_prev(enclosing_statement);
	while (tmp && OPCODE_is_store(WN_opcode(tmp))) {
	  if (Same_Scalar_Location(tmp,node)) { // check manually and using alias manager because this routine
		  				// is sometimes called before alias analysis has enough 
		  				// info to catch obvious cases
	    return operand_fits_mul16(WN_kid0(tmp),tmp, du_mgr,alias_mgr, wn_hash);
	  }
	  ALIAS_RESULT result = Aliased(alias_mgr, tmp, node);
	  if (result == POSSIBLY_ALIASED) {
	    return FALSE;
	  } else if (result == SAME_LOCATION) {
	    return operand_fits_mul16(WN_kid0(tmp),tmp, du_mgr,alias_mgr, wn_hash);
	  }
	  tmp = WN_prev(tmp);
	} 
    }
  }
  return FALSE;
}


/* ====================================================================
 *
 * BOOL can_use_mul16(WN * node, WN *parent, WN *enclosing_statement,
 *		DU_MANAGER *du_mgr, ALIAS_MANAGER *alias_mgr, bool * signed_mul);
 *
 * given a multiply node, return whether or not we can use a mul16.
 * 
 * two things could make it possible:
 *    1. Both its operands could be 16 bits
 *    2. only 16 bits of the result could be used.
 *
 * ==================================================================== */


static BOOL
can_use_mul16(WN * node, WN * parent, WN *enclosing_statement, DU_MANAGER *du_mgr, 
			ALIAS_MANAGER *alias_mgr, BOOL * signed_mul)
{
  Is_True( WN_operator( node ) == OPR_MPY, ("Expecting a multiply operator") );
  static MEM_POOL pool;
  static BOOL pool_init=FALSE;
  if (!pool_init) {
    pool_init=TRUE;
    MEM_POOL_Initialize(&pool,"can_use_mul16_pool",FALSE);
  }

  MEM_POOL_Push(&pool);

  BOOL mul16_ok = FALSE;
  
  /* the mpy result may be cast or stored to a sixteen bit value */
  if (parent != NULL)
  {
    OPERATOR par_op = WN_operator( parent );
    if (wn_is_sixteen_bits(parent) &&
	(par_op == OPR_CVT || par_op == OPR_CVTL ||
	 par_op == OPR_STID || par_op == OPR_ISTORE))
    {
      mul16_ok = TRUE;
    }
  }
  
  /* maybe its operands fit in sixteen bits */
  if (!mul16_ok) {
      WN_HASH htable(50,&pool);
      BOOL kid0_is16 = operand_fits_mul16(WN_kid0(node), enclosing_statement, du_mgr, alias_mgr, &htable);
      BOOL kid1_is16 = operand_fits_mul16(WN_kid1(node), enclosing_statement, du_mgr, alias_mgr, &htable);
      if ( kid0_is16 && kid1_is16) {
          mul16_ok = TRUE;
      }
  }

  /*
    cases
    ========================================================
    same sign
    ------------------
      U? * U? = mul16u
      I? * I? = mul16s
    
      
    Mixed sign
    -----------------
    U1 * I? = mul16s -- the U1 won't appear negative in 16 bits
    U2 * (I? const >= 0) -- mul16u, the I? is positive
    (U? const <= 0x7fff) * I? -- mul16s
    anything else: can't use a mul16
  */

  if (mul16_ok) {
    if (MTYPE_is_unsigned(WN_rtype(WN_kid0(node))) ==
	MTYPE_is_unsigned(WN_rtype(WN_kid1(node)))) {
      *signed_mul = !MTYPE_is_unsigned(WN_rtype(WN_kid1(node)));
    }
    else {
      WN *signed_wn = (MTYPE_is_unsigned(WN_rtype(WN_kid0(node))) ?
		       WN_kid1(node) : WN_kid0(node));
      WN *unsigned_wn = (MTYPE_is_unsigned(WN_rtype(WN_kid0(node))) ?
			 WN_kid0(node) : WN_kid1(node));
      if (wn_is_eight_bits(unsigned_wn)) {
	*signed_mul = TRUE;
      }
      else if (WN_operator(signed_wn) == OPR_INTCONST &&
	       WN_const_val(signed_wn) >= 0) {
	*signed_mul = FALSE;
      }	
      else if (WN_operator(unsigned_wn) == OPR_INTCONST &&
	       WN_const_val(unsigned_wn) <= 0x7fff) {
	*signed_mul = TRUE;
      }
   else
	mul16_ok = FALSE;
    }
  }

  MEM_POOL_Pop(&pool);

  return mul16_ok;
}


/* Recursively mark 16-bit OPR_MPY nodes. */
static void
Mark_Mul16 (WN *node, WN *parent, WN *enclosing_statement, 
		DU_MANAGER *du_mgr, ALIAS_MANAGER *alias_mgr)
{
  const BOOL trace = Get_Trace(TP_TEMP, 0x1000);

  if (WN_operator(node) == OPR_MPY)
  {
    /* If the 16-bit multiply bit has already been set for this
       multiply, then only change it from non-16-bit to 16-bit. We
       don't want to change a 16-bit multiply to a non-16-bit since we
       assume that only occurs because the optimizer performs some
       transformation that obscures our ability to detect 16-bit
       multiplies. Report changes from non-16-bit to 16-bit since we
       don't expected that to occur, but go ahead and make the
       change. */
    if (WN_Mpy_16Bit_Set(node))
    {
      if (trace)
	fprintf(TFile, "Existing %s16-bit multiply, ",
		WN_Mpy_16Bit(node)? "" : "non-");

      BOOL signed_mul = FALSE;
      if (can_use_mul16(node, parent, enclosing_statement, du_mgr, alias_mgr, &signed_mul))
      {
	if (WN_Mpy_16Bit(node))
	{
	  if (trace)
	    fprintf(TFile, "keeping as 16-bit\n");
	}
	else
	{
	  WN_Set_Mpy_16Bit(node);
	  if (signed_mul)
	    WN_Set_Mpy_16Bit_Signed(node);
	  else
	    WN_Set_Mpy_16Bit_Unsigned(node);

	  if (trace)
	    fprintf(TFile, "changing to 16-bit\n");
	}
      }
      else
      {
	if (WN_Mpy_16Bit(node))
	{
	  if (trace)
	    fprintf(TFile, "ignoring change to non-16-bit\n");
	}
	else
	{
	  if (trace)
	    fprintf(TFile, "keeping as non-16-bit\n");
	}
      }

      if (trace)
	fdump_tree(TFile, (parent) ? parent : node);
    }
    else
    {
      WN_Set_Mpy_16Bit_Set(node);
      BOOL signed_mul;

      if (can_use_mul16(node, parent, enclosing_statement, du_mgr, alias_mgr, &signed_mul)) {
	WN_Set_Mpy_16Bit(node);
	if (signed_mul) 
	  WN_Set_Mpy_16Bit_Signed(node);
	else
	  WN_Set_Mpy_16Bit_Unsigned(node);
      }
      else
	WN_Reset_Mpy_16Bit(node);
      
      if (trace)
      {
	fprintf(TFile, "New %s16-bit multiply\n",
		WN_Mpy_16Bit(node)? "" : "non-");
	fdump_tree(TFile, (parent) ? parent : node);
      }
    }
  }
  
  if (WN_opcode(node) == OPC_BLOCK)
  {
    for (WN *scan = WN_first(node); scan; scan = WN_next(scan))
      Mark_Mul16(scan, NULL, scan, du_mgr, alias_mgr);
  }
  else
  {
    for (UINT kidno = 0; kidno < WN_kid_count(node); kidno++)
      Mark_Mul16(WN_kid(node, kidno), node, enclosing_statement, du_mgr, alias_mgr);
  }
}
 

/* Mark OPR_MPY nodes that can be implemented with a 16-bit
   multiply. */
void
Find_Mul16 (WN *func, DU_MANAGER *du_mgr, ALIAS_MANAGER *alias_mgr)
{
  FmtAssert(WN_operator(func) == OPR_FUNC_ENTRY, ("expecting OPR_FUNC_ENTRY"));

  const BOOL trace = Get_Trace(TP_TEMP, 0x1000);

  if (trace)
    fprintf(TFile, "%sFind_Mul16\n%s", DBar, DBar);
  
  /* Recursively visit each node in 'func', marking 16-bit
     OPR_MPYs. */
  Mark_Mul16(func, NULL, NULL, du_mgr, alias_mgr);
}


static INT32 get_max_align (INT32 align, INT32 offset)
{
  INT32 r;
  INT32 n = align;
  INT32 m = align + offset;

  while(r = m % n)
  {
    m=  n;
    n=  r;
  }
  return n;
}

/* Target specific aliasing analysis. Return TRUE if access to 'st1'
   at 'offset1' of size 'size1' may overlap with access to 'st2' at
   'offset2' of size 'size2'. This routine should only check for
   target specific aliasing that is not detected by the common alias
   routines. */
BOOL
Target_Analyze_Alias (ST *st1, INT32 offset1, INT32 size1,
		      ST *st2, INT32 offset2, INT32 size2)
{
#ifdef TARG_XTENSA
  /* xtensa loads and stores truncate the lower address bits and do
     not give unaligned exceptions. Thus when a load/store potentially
     accesses a symbol at an alignment less than the memory-reference
     width of the load/store, we must be conservative in assuming what
     offset in that symbol it actually accesses. */

  /* 'st1' and 'st2' must be the same symbol. If not, we assume we
     don't need this additional check. Also don't bother to check if
     the offset/size pairs overlap, since then we would be detecting
     something already detected by the normal alias routines. */
  if (st1 && st2 && (st1 == st2) &&
      ((offset1 >= (offset2 + size2)) ||
       ((offset1 + size1) <= offset2)))
  {
    /* Find the maximum alignment of the addresses accessed by 'st1' +
       'offset1' and 'st2' + 'offset2', given the alignment of the
       base symbol and the offset from that symbol. */
    INT32 align1 = get_max_align(ST_alignment(st1), offset1);
    INT32 align2 = get_max_align(ST_alignment(st2), offset2);
    const INT32 offset_high1 = offset1 + size1;
    const INT32 offset_high2 = offset2 + size2;

    /* Find the lowest offset accessed, adjusting if necessary if the
       alignment is less than the access size. */
    if ((align1 < size1) || (align2 < size2)) {
      const INT32 offset_low1 = offset1 -
	((align1 < size1) ? (size1 - align1) : 0);
      const INT32 offset_low2 = offset2 -
	((align2 < size2) ? (size2 - align2) : 0);

      if ((offset_low1 < offset_high2) && (offset_high1 > offset_low2)) {
	Lmt_DevWarn(1, ("assuming overlapping for potentially unaligned memory ops"));
#if 0
	fprintf(stderr, "target alias: offset %d, size %d, st ", offset1, size1);
	Print_ST(stderr, st1, TRUE);
	fprintf(stderr, "              offset %d, size %d, st ", offset2, size2);
	Print_ST(stderr, st2, TRUE);
#endif
	
	return TRUE;
      }
    }
  }
#endif

  return FALSE;
}

// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
