
/* 
   Copyright (C) 2004-2007 Tensilica, Inc.  All Rights Reserved.
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


//-*-c++-*-
/* ====================================================================
* ====================================================================
*
* Module: opt_alias_rule.cxx
* $Revision: 1.56 $
* $Date: 2000/08/17 15:59:28 $
* $Author: dlstephe $
* $Source: /isms/cmplrs.src/osprey1.0/be/com/RCS/opt_alias_rule.cxx,v $
*
* Revision history:
*  04-APR-95 lo - Split from opt_alias_rule.cxx
*
* Description:
*
* ====================================================================
* ====================================================================
*/

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#ifdef _KEEP_RCS_ID
#define opt_alias_rule_CXX	"opt_alias_rule.cxx"
static char *rcs_id = 	opt_alias_rule_CXX"$Revision: 1.56 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "stab.h"
#include "tracing.h"
#include "wn_util.h"
#include "opt_points_to.h"
#include "opt_alias_class.h"
#include "opt_alias_rule.h"
#include "opt_alias_mgr.h"
#include "tie.h"
#include "intrn_info.h"


// ***********************************************************
//
//              Alias Rules
//
// ***********************************************************


// Implement A.1 (See opt_alias_rule.h.)
//  TRUE -- possibly aliased
//  FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Base_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  if (mem1->Different_base(mem2))
    return FALSE;

  return TRUE;
}

// Implement A.2 (See opt_alias_rule.h.)
//  Given the base are the same!
//    TRUE -- aliased
//    FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Ofst_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  if (mem1->Same_base(mem2) && !mem1->Overlap(mem2))
    return FALSE;
  else
    return TRUE;
}


// Implement A.3 (See opt_alias_rule.h.)
//  TRUE -- possibly aliased
//  FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Static_Nest_Rule(const POINTS_TO *call, const POINTS_TO *mem) const
{
  // Warn_todo("Nest Rule not implemented.", call, mem);
  return TRUE;
}

BOOL
ALIAS_RULE::Aliased_Classification_Rule(const POINTS_TO *const mem1,
					const POINTS_TO *const mem2) const
{
  return (mem1->Alias_class() == mem2->Alias_class() ||
	  mem1->Alias_class() == PESSIMISTIC_AC_ID   ||
	  mem2->Alias_class() == PESSIMISTIC_AC_ID   ||
          mem1->Alias_class() == OPTIMISTIC_AC_ID    ||
          mem2->Alias_class() == OPTIMISTIC_AC_ID);
}

BOOL
ALIAS_RULE::Aliased_Ip_Classification_Rule(const POINTS_TO *const mem1,
					   const POINTS_TO *const mem2) const
{
  BOOL aliased = (mem1->Ip_alias_class() == mem2->Ip_alias_class() ||
		  mem1->Ip_alias_class() == PESSIMISTIC_AC_ID      ||
		  mem2->Ip_alias_class() == PESSIMISTIC_AC_ID      ||
                  mem1->Ip_alias_class() == OPTIMISTIC_AC_ID       ||
                  mem2->Ip_alias_class() == OPTIMISTIC_AC_ID);

  return aliased;
}

// Fortran-90 pointers can point only to items lacking the not_f90_target
// attribute.
BOOL
ALIAS_RULE::Aliased_F90_Target_Rule(const POINTS_TO *const mem1,
				    const POINTS_TO *const mem2,
				    TY_IDX object_ty1,
				    TY_IDX object_ty2) const
{
  if (mem1->Known_f90_pointer()) {
#if 0
    fprintf(TFile, "---------------\n");
    mem1->Print(TFile);
    fprintf(TFile, "      and\n");
    mem2->Print(TFile);
    fprintf(TFile, "      do %salias\n",
	    (!mem2->Known_not_f90_pointer() || !mem2->Not_f90_target() ? "" : "not "));
#endif
    if (mem2->Base() != NULL) {
      Is_True(!mem2->Not_f90_target() ||
	      (ST_class(mem2->Base()) != CLASS_VAR) ||
	      !ST_is_f90_target(mem2->Base()),
	      ("Target attribute lost"));
    }
    return (!mem2->Known_not_f90_pointer() || !mem2->Not_f90_target());
  }
  if (mem2->Known_f90_pointer()) {
#if 0
    fprintf(TFile, "---------------\n");
    mem2->Print(TFile);
    fprintf(TFile, "      and\n");
    mem1->Print(TFile);
    fprintf(TFile, "      do %salias\n",
	    (!mem1->Known_not_f90_pointer() || !mem1->Not_f90_target() ? "" : "not "));
#endif
    if (mem1->Base() != NULL) {
      Is_True(!mem1->Not_f90_target() ||
	      (ST_class(mem1->Base()) != CLASS_VAR) ||
	      !ST_is_f90_target(mem1->Base()),
	      ("Target attribute lost"));
    }
    return (!mem1->Known_not_f90_pointer() || !mem1->Not_f90_target());
  }
  return TRUE;
}

// Implement A.4 (See opt_alias_rule.h.)
//    TRUE -- possibly aliased
//    FALSE -- not aliased (if its address is not taken)
//
BOOL ALIAS_RULE::Aliased_Indirect_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  // Change to Aliased_Indirect_Rule 
  //  Using new symtab -- addr_passed is deleted.  5/15/98.

  //  Fix 653016:  with IPA introducing BARRIER, the vsym is unnamed and unique_pt.
  //  The istore starts with unnamed and unique_pt, but FSA eventually determines
  //  the named based.  The named variable is not_addr_saved.  Without fixing
  //  this rule, the vsym of the istore becomes not alias with the istore!

  if ((mem1->Unnamed() && !mem1->Unique_pt())
      && mem2->Not_addr_saved())

    // has to further check if indirect load can access
    // data that is address passed but not saved (PV 14594)
    if ((mem1->Base_kind() == BASE_IS_DYNAMIC &&
	!mem2->Not_addr_passed()) == FALSE)
      return FALSE;
  if ((mem2->Unnamed() && !mem2->Unique_pt())
      && mem1->Not_addr_saved())
    if ((mem2->Base_kind() == BASE_IS_DYNAMIC &&
	!mem1->Not_addr_passed()) == FALSE)
      return FALSE;
  return TRUE;
}

//  Implement A.6.3 and A.6.4. (See opt_alias_rule.h.)
//    TRUE:  may be aliased
//    FALSE: not aliased
//
BOOL ALIAS_RULE::Aliased_Qualifier_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2, TY_IDX ty1, TY_IDX ty2) const
{
  // If mem1 or mem2 is declared const, ...
#if 1
  // If at least one of mem1 and mem2 is declared global const, and
  // they do not overlap, the two operations don't alias.
  if (((mem1->Const() &&
	(mem1->Base_kind() == BASE_IS_FIXED) &&
	mem1->Not_auto()) ||
       (mem2->Const() &&
	(mem2->Base_kind() == BASE_IS_FIXED) &&
	mem2->Not_auto())) &&
      ((mem1->Base() != mem2->Base()) ||
       !mem1->Overlap(mem2))) {
    return FALSE;
  }
#else
  if (Rule_enabled(C_RESTRICT_CONST_RULE) && ty1 != NULL && ty2 != NULL) {
    // disabled analysis of const qualifier because of inlining of
    // C constructors.
    if ((TY_is_const(ty1) && mem2->Not_init_const()) ||
	(TY_is_const(ty2) && mem1->Not_init_const()))
      return FALSE;
  }
#endif

  // mem1 is a unique pointer
  if (mem1->Based_sym() != NULL &&
      mem1->Unique_pt() && 
      mem1->Based_sym() != mem2->Based_sym() &&
      !mem2->Default_vsym())
       return FALSE;

  // mem2 is a unique pointer
  if (mem2->Based_sym() != NULL &&
      mem2->Unique_pt() && 
      mem2->Based_sym() != mem1->Based_sym() &&
      !mem1->Default_vsym())
       return FALSE;

  return TRUE;
}


//  Examine the alias attributes determined during alias analysis.
//    TRUE:  may be aliased
//    FALSE: not aliased
//
BOOL ALIAS_RULE::Aliased_Attribute_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  // If either mem1 or mem2 is allocated in a read-only page, ...
  if (mem1->Const() || mem2->Const())
    return FALSE;

  if ((mem1->Local() && mem2->Global()) ||
      (mem2->Local() && mem1->Global()))
      return FALSE;

  return TRUE;
}


// Implement F.1.  
//    TRUE:  may be aliased
//    FALSE: not aliased
//
BOOL ALIAS_RULE::Aliased_F_Param_Rule(const POINTS_TO *mem1,
				      const POINTS_TO *mem2) const
{
  // if one is an f90 pointer and the other is an f90 target, then this
  // alias rule does not apply (pv 794649)
  if (! mem1->Known_not_f90_pointer() && ! mem2->Not_f90_target())
    return TRUE;
  if (! mem2->Known_not_f90_pointer() && ! mem1->Not_f90_target())
    return TRUE;

  // if one of them is a formal parameter, the other is not,
  // then not aliased.
  if ((mem1->F_param() && ! mem2->F_param()) ||
      (mem2->F_param() && ! mem1->F_param()))
    return FALSE;

  // if both of them are formal parameters, have based on different symbol,
  // then they are not aliased.
  if (mem1->F_param() && mem2->F_param() &&
      mem1->Based_sym() != mem2->Based_sym())
    return FALSE;

  return TRUE;
}

// Implement C.1.  See opt_ailas_rule.h
//
//  Strip off qualifiers, signed-ness.
//  Return the member type for a KIND_ARRAY type.
//  Return the set of basic types for a structure/class.
//  Exclude pointers.
//  Use bit 0 to bit 30 for mtypes.
//  Use bit 31 for pointers.
//

static const int NUM_MTYPE_BIT_SET_BYTES=16;
static const int MTYPE_PTR = 8*NUM_MTYPE_BIT_SET_BYTES-1;

class MTYPE_BIT_SET {

private:
  unsigned char _bytes[NUM_MTYPE_BIT_SET_BYTES];

public:

  MTYPE_BIT_SET() { clear(); }
  ~MTYPE_BIT_SET() {};
  void clear() {
	int i;
	for (i=0; i<NUM_MTYPE_BIT_SET_BYTES; i++) _bytes[i]=0;
  }

  void operator|=(const int& _pos) {
	int i=0;
	int pos = _pos;
	Is_True(pos>=0 && pos<NUM_MTYPE_BIT_SET_BYTES*8,("Bad position"));
	while (pos>7) { i++; pos -= 8; }
	_bytes[i] |= 1<<pos;
  }

  void set_pointer_type() { (*this) |= (MTYPE_PTR); }

  void set_all() {
	int i;
	for (i=0; i<NUM_MTYPE_BIT_SET_BYTES; i++) _bytes[i]= 0xff;
  }

  bool operator&(const MTYPE_BIT_SET& bs2) const {
	int i;
	for (i=0; i<NUM_MTYPE_BIT_SET_BYTES; i++)
	  if (_bytes[i] & bs2._bytes[i])
	    return TRUE;

	return FALSE;
  }

  bool operator&(const int& _pos) const {
	int i=0;
	int pos = _pos;
	Is_True(pos>=0 && pos<NUM_MTYPE_BIT_SET_BYTES*8,("Bad position"));
	while (pos>7) { i++; pos -= 8; }
	if (_bytes[i] & 1<<pos)
	  return TRUE;
	return FALSE;
  }

  int max_mtype() const { return 8*NUM_MTYPE_BIT_SET_BYTES-2; }

};


static void
Get_mtype_alias_class_types (const ALIAS_MANAGER *am,
                             TYPE_ID mtype, MTYPE_BIT_SET &ret_type)
{
  UINT32 alias_class = am->Mtype_alias_class(mtype);
  if (alias_class == 0)
    return;
  
  /* Collect all types from mtype's alias class in ret_type. */
  ALIAS_MANAGER::MTYPE_ALIAS_CLASS_MAP *alias_map = am->Mtype_alias_class_map();
  ALIAS_MANAGER::MTYPE_ALIAS_CLASS_MAP::keyValuePair *scan;
  ALIAS_MANAGER::MTYPE_ALIAS_CLASS_MAP::iter iter(alias_map);
  while ((scan = iter.next()) != NULL)
    {
      if (scan->value() == alias_class)
        ret_type |= scan->key();
    }
}


static void
Get_stripped_mtype (const ALIAS_MANAGER *am, TY_IDX ty_idx, MTYPE_BIT_SET& ret_type)
{
  const TY& ty = Ty_Table[ty_idx];
  switch (TY_kind(ty)) {
  case KIND_SCALAR:
    //  Get the de-qualified and unsigned basic type for KIND_SCALAR.
    //    Note that enum is represented as KIND_SCALAR.
    {
      TYPE_ID mtype = TY_mtype(ty);
      Is_True(mtype <= ret_type.max_mtype(), ("more than 31 mtypes."));
      switch (mtype) {
      case MTYPE_I1:
	ret_type |= (MTYPE_U1); break;
      case MTYPE_I2:
	ret_type |= (MTYPE_U2); break;
      case MTYPE_I4:
	ret_type |= (MTYPE_U4); break;
      case MTYPE_I8:
	ret_type |= (MTYPE_U8); break;
      default:
	ret_type |= (mtype); break;
      }
      
      /* Collect all types from mtype's alias class in ret_type. */
      Get_mtype_alias_class_types(am, mtype, ret_type);
    }
    break;
  case KIND_ARRAY:  //  Get the element type of the array
    {
      TY_IDX etype = TY_AR_etype (ty);
      while (TY_KIND (etype) == KIND_ARRAY)
	etype = TY_AR_etype (etype);
      Get_stripped_mtype(am, etype, ret_type);
    }
    break;
  case KIND_STRUCT:  // Collect the basic types of a structure recursively.
    if (!TY_fld (ty).Is_Null ()) {
      FLD_ITER fld_iter = Make_fld_iter (TY_fld (ty));
      do {
	Get_stripped_mtype (am, FLD_type (fld_iter), ret_type);
      } while (!FLD_last_field (fld_iter++));
    }
    break;
  case KIND_FUNCTION:  
    // KIND_FUNCTION are allowed because LDA func_st might be
    // entered as virtual variable into the opt_stab.
    // FmtAssert(FALSE, ("KIND_FUNCTION should not be used."));
    ret_type.clear();
    break;
  case KIND_POINTER:  // All pointers considered equal.
    ret_type.set_pointer_type();
    break;
  case KIND_VOID:  // Void aliased to all types.
    ret_type.set_all();
    break;
  default:
    FmtAssert(FALSE, ("KIND_INVALID should not be used."));
    break;
  }

  // Expand UNKNOWN type and CHAR type to cover all basic types.
  // Expand MTYPE_V (void) to cover all basic types.
  // If the TY is marked TY_is_varargs(), the memop is a copy
  // from a dedicated register to stack location, assume we don't
  // know its type.

  if (ret_type & (MTYPE_UNKNOWN) || 
      ret_type & (MTYPE_V) ||		// MTYPE_V is another void type
      ret_type & (MTYPE_U1) ||		// unsigned char type 
      TY_no_ansi_alias(ty))		// varargs TY:  See PV 329475.
    ret_type.set_all();

  return;
}


// Are the two pointer types compatible?
// Pointers to void and char are compatible with any pointer type.
// Otherwise, declare them non-compatible if they point to types of
// different kinds, different sizes or different mtypes. 
// This is conservative; for example, pointers to two different 
// structs of the same size would be considered compatible.
static BOOL
Compatible_Ptr_Types (const ALIAS_MANAGER *am, TY_IDX ty1, TY_IDX ty2)
{
  do {
    ty1 = TY_pointed(ty1);
    ty2 = TY_pointed(ty2);
  } while (TY_kind(ty1) == KIND_POINTER && TY_kind(ty2) == KIND_POINTER);
  
  TYPE_ID mtype1 = TY_mtype(ty1);
  TYPE_ID mtype2 = TY_mtype(ty2);
  
  if (TY_kind(ty1) == KIND_VOID ||
      TY_kind(ty2) == KIND_VOID ||
      (TY_kind(ty1) == KIND_SCALAR && MTYPE_byte_size(mtype1) == 1) ||
      (TY_kind(ty2) == KIND_SCALAR && MTYPE_byte_size(mtype2) == 1))
    return TRUE;
  
  if (TY_kind(ty1) != TY_kind(ty2))
    return FALSE;
  
  if (TY_size(ty1) == TY_size(ty2) && mtype1 == mtype2)
    return TRUE;
  
  /* If the 2 types are in the same alias class, assume they are compatible. */
  if (MTYPE_is_tie(mtype1) || MTYPE_is_tie(mtype2)) {
    UINT32 ac1 = am->Mtype_alias_class(mtype1);
    UINT32 ac2 = am->Mtype_alias_class(mtype2);
    if (ac1 != 0 && ac1 == ac2)
      return TRUE;
  }

  return FALSE;
}


// Does struct ty contain a field whose type is struct sub_ty?
static BOOL
Is_Struct_Subtype(TY_IDX ty, TY_IDX sub_ty)
{
  if (TY_fld(ty).Is_Null())
    return FALSE;

  FLD_ITER fld_iter = Make_fld_iter(TY_fld(ty));
  do {
    TY_IDX fld_ty = FLD_type(fld_iter);
    if (TY_kind(fld_ty) == KIND_STRUCT &&
        (fld_ty == sub_ty || Is_Struct_Subtype(fld_ty, sub_ty)))
      return TRUE;
  } while (!FLD_last_field(fld_iter++));
  
  return FALSE;
}


// Implement C.1.  See opt_ailas_rule.h
//  
//  Apply ANSI Rule for alias disambiguation
//    TRUE -- cannot be determined by this test
//    FALSE -- not aliased
// 
BOOL ALIAS_RULE::Aliased_ANSI_Type_Rule(const POINTS_TO *mem1,
					const POINTS_TO *mem2, 
					TY_IDX ty1, TY_IDX ty2) const
{
  if (ty1 == (TY_IDX)NULL || ty2 == (TY_IDX)NULL)  // One of type is unknown. Assume aliased.
    return TRUE;

  if (ty1 == ty2)  // aliased if same type.
    return TRUE;

  // If both are scalar, do not use ANSI rule.
  // The base and ofst rules are sufficient.
  if (mem1->Base_is_fixed() && mem2->Base_is_fixed())
    return TRUE;

  // If both mem have same base, should be handled by offset rule.
  // Do not use ansi rule.
  // Note: same_base may return FALSE for the same object if the object
  // are passed in multiple times via distinct parameters.
  if (mem1->Same_base(mem2))
    return TRUE;

  if (TY_no_ansi_alias(ty1) || TY_no_ansi_alias(ty2))
    return TRUE;

  // Recognize pointers to different (non-compatible) types
  if (TY_kind(ty1) == KIND_POINTER && TY_kind(ty2) == KIND_POINTER &&
      !Compatible_Ptr_Types(_alias_mgr, ty1, ty2)) {
    // printf("\nIncompatible pointer types don't alias\n");
    // Ty_Table[ty1].Print(stdout);
    // Ty_Table[ty2].Print(stdout);
    return FALSE;
  }

  // Recognize non-subsetting structs
  if (TY_kind(ty1) == KIND_STRUCT && TY_kind(ty2) == KIND_STRUCT &&
      !Is_Struct_Subtype(ty1, ty2) && !Is_Struct_Subtype(ty2, ty1)) {
    // printf("\nNon-subsetting struct types don't alias\n");
    // Ty_Table[ty1].Print(stdout);
    // Ty_Table[ty2].Print(stdout);
    return FALSE;
  }

  // Fixed-base scalar cannot be pointed to by a pointer to struct
  if (mem1->Base_is_fixed() &&
      (TY_kind(ty1) == KIND_SCALAR || TY_kind(ty1) == KIND_POINTER) &&
      MTYPE_byte_size(TY_mtype(ty1)) != 1 &&
      ST_class(mem1->Base())==CLASS_VAR && ST_type(mem1->Base())==ty1 &&
      TY_kind(ty2) == KIND_STRUCT) {
    // printf("\nFixed-base scalar can't alias with struct\n");
    // Ty_Table[ty1].Print(stdout);
    // Ty_Table[ty2].Print(stdout);
    return FALSE;
  } 
  if (mem2->Base_is_fixed() &&
      (TY_kind(ty2) == KIND_SCALAR || TY_kind(ty2) == KIND_POINTER) &&
      MTYPE_byte_size(TY_mtype(ty2)) != 1 &&
      ST_class(mem2->Base())==CLASS_VAR && ST_type(mem2->Base())==ty2 &&
      TY_kind(ty1) == KIND_STRUCT) {
    // printf("\nFixed-base scalar can't alias with struct\n");
    // Ty_Table[ty1].Print(stdout);
    // Ty_Table[ty2].Print(stdout);
    return FALSE;
  }

  MTYPE_BIT_SET ret_type1, ret_type2;
  //  Handle SCALAR, POINTER, STRUCT, CLASS, and ARRAY
  Get_stripped_mtype(_alias_mgr, ty1, ret_type1);
  Get_stripped_mtype(_alias_mgr, ty2, ret_type2);
  if (ret_type1 & ret_type2) 
    return TRUE;
  
  // printf("\nTypes with disjoint MTYPE sets don't alias\n");
  // Ty_Table[ty1].Print(stdout);
  // Ty_Table[ty2].Print(stdout);
  return FALSE;
}


// Very aggressive rule!  Turn on by flags for experiment purposes.
// If both are pointers and their pointer kind are different ...
//
BOOL ALIAS_RULE::Aliased_Strongly_Typed_Rule(TY_IDX ty1, TY_IDX ty2) const
{
  if (ty1 != (TY_IDX) NULL &&
      ty2 != (TY_IDX) NULL &&
      ty1 != ty2)
    return FALSE;
  return TRUE;
}


//  Implement C.2.1 (See opt_alias_rule.h.)
//    TRUE:  may be aliased
//    FALSE: not aliased
//
BOOL ALIAS_RULE::Aliased_C_Qualifier_Rule(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
#if 0
  // OLD Restricted rule
  if (mem1->Based_sym() != NULL && mem1->Restricted() &&
      mem2->Based_sym() != NULL && mem2->Restricted() &&
      mem1->Based_sym() != mem2->Based_sym())
    return FALSE;
#else
  // Implement restrict pointer like a unique_pt.
  //
  if (mem1->Based_sym() != NULL &&
      mem1->Restricted() && 
      mem1->Based_sym() != mem2->Based_sym() &&
      !mem2->Default_vsym())
    return FALSE;

  if (mem2->Based_sym() != NULL &&
      mem2->Restricted() && 
      mem2->Based_sym() != mem1->Based_sym() &&
      !mem1->Default_vsym())
    return FALSE;
#endif

  return TRUE;
}

//  Implement O.1 (See opt_alias_rule.h.)
//    TRUE:  may be aliased
//    FALSE: not aliased
//
BOOL ALIAS_RULE::Aliased_Ragnarok_Unnamed(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  if ((mem1->Named() && mem2->Unnamed() && mem2->Based_sym() != NULL) ||
      (mem2->Named() && mem1->Unnamed() && mem1->Based_sym() != NULL))
    return FALSE;
  return TRUE;
}


//  Implement O.2 (See opt_alias_rule.h.)
//    TRUE:  may be aliased
//    FALSE: not aliased 
//
BOOL ALIAS_RULE::Aliased_Ragnarok_Restrict(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  if (mem1->Based_sym() != NULL && 
      mem2->Based_sym() != NULL &&
      mem1->Based_sym_depth() == 0 &&
      mem2->Based_sym_depth() == 0 &&
      mem1->Based_sym() != mem2->Based_sym())
       return FALSE;
  return TRUE;
}

//  Implement -OPT:alias=disjoint
//
BOOL ALIAS_RULE::Aliased_Disjoint(const POINTS_TO *mem1, const POINTS_TO *mem2) const
{
  if (mem1->Based_sym() != NULL && 
      mem2->Based_sym() != NULL) {
    if (mem1->Based_sym() != mem2->Based_sym())
      return FALSE;
    else if (mem1->Based_sym_depth() != mem2->Based_sym_depth())
      return FALSE;
  }
  return TRUE;
}

// ************************************************************************
//
//    External Alias Analysis Interface
//
// ************************************************************************

//  Same location
//
BOOL ALIAS_RULE::Same_location(const WN *wn1, const WN *wn2, const POINTS_TO *mem1, const POINTS_TO *mem2)
{
  if (mem1->Same_base(mem2) &&
      mem1->Ofst_kind() == OFST_IS_FIXED &&
      mem2->Ofst_kind() == OFST_IS_FIXED &&
      mem1->Byte_Ofst() == mem2->Byte_Ofst() &&
      mem1->Byte_Size() == mem2->Byte_Size() &&
      WN_operator(wn1) != OPR_INTRINSIC_CALL &&
      WN_operator(wn2) != OPR_INTRINSIC_CALL &&
      (WN_operator(wn1) == OPR_IDNAME ||
       WN_object_size(wn1) == mem1->Byte_Size()) &&
      (WN_operator(wn2) == OPR_IDNAME ||
       WN_object_size(wn2) == mem2->Byte_Size())) {
    
    if (mem1->Bit_Size() == 0 || mem2->Bit_Size() == 0)
      return TRUE;
    else
      return (mem1->Bit_Ofst() == mem2->Bit_Ofst() &&
	      mem1->Bit_Size() == mem2->Bit_Size());
  }
  return FALSE;
}


//  Determine aliasing information based on analysis of the program.
//
//    TRUE  -- possibly aliased
//    FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Memop_By_Analysis(const POINTS_TO *p1, const POINTS_TO *p2) const
{
  if (p1->Expr_kind() == EXPR_IS_INVALID ||
      p2->Expr_kind() == EXPR_IS_INVALID)
    return TRUE;

  // no analysis for weak symbol
  if ((p1->Weak() && p2->Weak()) ||
      (p1->Weak() && p2->Weak_base()) ||
      (p2->Weak() && p1->Weak_base()))
    return TRUE;

  if (Rule_enabled(BASE_RULE) && !Aliased_Base_Rule(p1, p2))
    return FALSE;
  
  if (Rule_enabled(OFST_RULE) && !Aliased_Ofst_Rule(p1, p2))
    return FALSE;
  
  if (Rule_enabled(INDR_RULE) && !Aliased_Indirect_Rule(p1, p2))
    return FALSE;

  if (Rule_enabled(ATTR_RULE) && !Aliased_Attribute_Rule(p1, p2))
    return FALSE;

  if (Rule_enabled(NEST_RULE) && !Aliased_Static_Nest_Rule(p1, p2))
    return FALSE;

  if (Rule_enabled(CLAS_RULE) && !Aliased_Classification_Rule(p1, p2))
    return FALSE;

  if (Rule_enabled(IP_CLAS_RULE) && !Aliased_Ip_Classification_Rule(p1, p2))
    return FALSE;

  return TRUE;
}
  

//  Determine aliasing information based on declarations.
//
//    TRUE  -- possibly aliased
//    FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Memop_By_Declaration(const POINTS_TO *p1,
					      const POINTS_TO *p2,
					      TY_IDX ty1, TY_IDX ty2) const
{
  if (p1->Expr_kind() == EXPR_IS_INVALID ||
      p2->Expr_kind() == EXPR_IS_INVALID)
    return TRUE;

  if (Rule_enabled(F_PARM_RULE) && !Aliased_F_Param_Rule(p1, p2))
    return FALSE;
  
  if (Rule_enabled(QUAL_RULE) && !Aliased_Qualifier_Rule(p1, p2, ty1, ty2))
    return FALSE;

  if (Rule_enabled(C_ANSI_RULE) && !Aliased_ANSI_Type_Rule(p1, p2, ty1, ty2))
    return FALSE;

  if (Rule_enabled(C_STRONGLY_TYPED_RULE) && !Aliased_Strongly_Typed_Rule(ty1, ty2))
    return FALSE;
  
  if (Rule_enabled(C_QUAL_RULE) && !Aliased_C_Qualifier_Rule(p1, p2))
    return FALSE;
  
  if (Rule_enabled(RAG_UNNAMED_RULE) && !Aliased_Ragnarok_Unnamed(p1, p2))
    return FALSE;
  
  if ((Rule_enabled(RAG_RESTRICTED_RULE) || Rule_enabled(RAG_PARMS_RULE)) && 
      !Aliased_Ragnarok_Restrict(p1, p2))
    return FALSE;

  if (Rule_enabled(IBM_DISJOINT_RULE) && !Aliased_Disjoint(p1, p2))
    return FALSE;
  
  if (Rule_enabled(F90_TARGET_RULE) && !Aliased_F90_Target_Rule(p1, p2, ty1, ty2))
    return FALSE;

  return TRUE;
}


//  Combine all Alias rules
//
//    TRUE  -- possibly aliased
//    FALSE -- not aliased
//
BOOL ALIAS_RULE::Aliased_Memop(const POINTS_TO *p1, const POINTS_TO *p2,
			       TY_IDX ty1, TY_IDX ty2) const
{
  if (!Aliased_Memop_By_Analysis(p1, p2))
    return FALSE;

  if (!Aliased_Memop_By_Declaration(p1, p2, ty1, ty2))
    return FALSE;

  return TRUE;
}

BOOL ALIAS_RULE::Aliased_Memop(const POINTS_TO *p1, const POINTS_TO *p2) const
{
  if (!Aliased_Memop_By_Analysis(p1, p2))
    return FALSE;

  if (!Aliased_Memop_By_Declaration(p1, p2, p1->Ty(), p2->Ty()))
    return FALSE;

  return TRUE;
}


//  Implement A.5, A.6.1, A.6.2, and F.2 (See opt_alias_rule.h.)
//
//  WARNING:  For Fortran, the dependence between parameter and memop is ignored.
//
//  Handle interaction between CALL and memop
//    NO_READ_NO_WRITE:   the procedure do not read or write the variable.
//    READ:               the procedure may call read the variable.
//    WRITE:              the procedure may write the variable.
//    READ_AND_WRITE:     the procedure may read and/or write the variable.
//  
READ_WRITE ALIAS_RULE::Aliased_with_Call(const WN *wn, const POINTS_TO *mem) const
{
  if (mem->Dedicated()) 
    return WRITE;

  if (mem->Const())
    return NO_READ_NO_WRITE;

  ST *st = (WN_operator(wn) == OPR_CALL ? WN_st(wn) : NULL);
  INT32 flags = WN_call_flag(wn);

  BOOL pure = FALSE;
  BOOL no_side_effects = FALSE;
  BOOL no_recursive_invocation = FALSE;

  if (st != NULL) {
    pure = PU_is_pure(Pu_Table[ST_pu(st)]);
    no_side_effects = PU_no_side_effects(Pu_Table[ST_pu(st)]);
  }
  else if (WN_operator(wn) == OPR_INTRINSIC_CALL) {
    INTRINSIC intr = WN_intrinsic(wn);
    if (INTRN_is_tie_intrinsic(intr)) {
      TIE_MACRO_p tie = tie_info->tie_macro(Intrinsic_To_Tie_Macro_Id(intr));
      pure = (!tie->reads_memory() && !tie->writes_memory());
      no_side_effects = !tie->writes_memory();
    }
    else {
      pure = INTRN_is_pure(intr);
      no_side_effects = INTRN_has_no_side_effects(intr);
    }
    if (INTRN_is_leaf(intr)) {
      no_recursive_invocation = TRUE;
    }
  }

  if (Rule_enabled(QUAL_RULE) && pure)
    return NO_READ_NO_WRITE;

  if (Rule_enabled(F_CALL_RULE)) {
    // Fortran parameter is aliased to the call unless passed by parameter.
    if (mem->F_param())
      return NO_READ_NO_WRITE;

    // local variable not in the PARM list are not aliased
    if (mem->Local() && mem->Not_addr_saved())
      return NO_READ_NO_WRITE;
  }
  
  if (Rule_enabled(CALL_RULE)) {
    if (mem->Local() && mem->Not_addr_saved() && mem->Not_addr_passed()) {
      return NO_READ_NO_WRITE;
    }
    if (no_recursive_invocation &&
	(mem->Base_kind() == BASE_IS_FIXED) &&
	(ST_sclass(mem->Base()) == SCLASS_PSTATIC) &&
	mem->Not_addr_saved() &&
	mem->Not_addr_passed()) {
      /* If there is no possible recursive invocation of the current
       * PU, the memory object is guaranteed to be out of scope. So if
       * its address is not passed or saved to any other scope, the
       * call cannot touch it.
       */
      return NO_READ_NO_WRITE;
    }
  }


  BOOL ref = TRUE;
  BOOL mod = TRUE;

  if (Rule_enabled(QUAL_RULE) && no_side_effects)
    mod = FALSE;

  // check the call flags, don't expect them to be correct for C!
  // need to update opt_alias_analysis.cxx as well.
  if (Rule_enabled(F_CALL_RULE) && WOPT_Enable_Call_Flag) {
    if ((flags & WN_CALL_NON_PARM_MOD) == 0) 
      mod = FALSE;
    if ((flags & WN_CALL_NON_PARM_REF) == 0)
      ref = FALSE;
  }

  if (mod && ref)
    return READ_AND_WRITE;
  else if (mod)
    return WRITE;
  else if (ref)
    return READ;

  return NO_READ_NO_WRITE;
}

READ_WRITE ALIAS_RULE::Aliased_with_Asm(const WN *wn, const POINTS_TO *mem) const
{
  if (mem->Dedicated())
    return READ_AND_WRITE;

  OPERATOR opr = WN_operator(wn);
  if ((OPERATOR_is_scalar_load (opr) || OPERATOR_is_scalar_store (opr)) &&
      ST_sclass(WN_st(wn)) == SCLASS_REG)
    return NO_READ_NO_WRITE;

  if (!WN_Asm_Clobbers_Mem(wn)) {
    if (mem->Not_addr_saved())
      return NO_READ_NO_WRITE;
    else
      return READ;
  } else {
    if (mem->Not_addr_saved())
      if (mem->Local() && mem->Not_addr_passed())
	return NO_READ_NO_WRITE;
      else
	return WRITE;
    else
      return READ_AND_WRITE;
  }
}


//  Return TRUE if the POINTS_TO aliased with the REGION global bit
//
BOOL ALIAS_RULE::Aliased_with_Global(const POINTS_TO *mem) const
{
  // return FALSE for those variable known to be local (thus 
  // they cannot be global variables)
  return !mem->Local();  
}

//  Return TRUE if the POINTS_TO aliased with the REGION indirect bit
//
BOOL ALIAS_RULE::Aliased_with_Indirect(const POINTS_TO *mem) const
{
  // return FALSE for those variable that are not addr saved (hence
  //  cannot be accessed thru an indirect with a unknown base)
  return !mem->Not_addr_saved();
}
