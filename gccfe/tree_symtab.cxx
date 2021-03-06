
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


/* translate gnu decl trees to symtab references */
#ifdef _WIN32
#define BITSPERBYTE 8
#else
#include <values.h>
#endif
#include "defs.h"
#include "errors.h"
#include "gnu_config.h"
#include "gnu/flags.h"
extern "C" {
#include "gnu/system.h"
#include "gnu/tree.h"
#include "gnu/toplev.h"
}
#ifdef TARG_IA32
// the definition in gnu/config/i386/i386.h causes problem
// with the enumeration in common/com/ia32/config_targ.h
#undef TARGET_PENTIUM
#endif /* TARG_IA32 */

#include "symtab.h"
#include "strtab.h"
#include "tree_symtab.h"
#include "wn.h"
#include "wfe_expr.h"
#include "wfe_misc.h"
#include "wfe_dst.h"
#include "ir_reader.h"
#include "tie.h"
#include <cmplrs/rcodes.h>

extern FILE *tree_dump_file; // For debugging only

extern INT pstatic_as_global;

extern "C" void Update_TY(tree t) { Get_TY(t); }

PREG_NUM asm_neg_preg = -2;

static char*
Get_Name (tree node)
{
	if (node == NULL)
		return ".anonymous";
	else if (TREE_CODE (node) == IDENTIFIER_NODE)
		return IDENTIFIER_POINTER (node);
	else if (TREE_CODE (node) == TYPE_DECL)
		// If type has a typedef-name, the TYPE_NAME is a TYPE_DECL.
		return IDENTIFIER_POINTER (DECL_NAME (node));
	else
		FmtAssert(FALSE, ("Get_Name unexpected tree"));
		return NULL;
}

// idx is non-zero only for RECORD and UNION, when there is forward declaration
extern TY_IDX
Create_TY_For_Tree (tree type_tree, TY_IDX idx)
{
        static int level = 0;

	if (TREE_CODE(type_tree) == ERROR_MARK)
		exit (RC_USER_ERROR);

	TY_IDX orig_idx = idx;
	if(TREE_CODE_CLASS(TREE_CODE(type_tree)) != 't') {
	    DevWarn("Bad tree class passed to Create_TY_For_Tree %c",
		TREE_CODE_CLASS(TREE_CODE(type_tree)));
	    return idx;
	}
	// for typedefs get the information from the base type
	if (TYPE_NAME(type_tree) &&
	    (TREE_CODE(type_tree) == RECORD_TYPE ||
	     TREE_CODE(type_tree) == UNION_TYPE) &&
	    TYPE_MAIN_VARIANT(type_tree) != type_tree) {
		idx = Get_TY (TYPE_MAIN_VARIANT(type_tree));
		if (TYPE_READONLY(type_tree))
			Set_TY_is_const (idx);
		if (TYPE_VOLATILE(type_tree))
			Set_TY_is_volatile (idx);
		unsigned attr_align = 1 << TYPE_ATTR_ALIGN(type_tree);
		if (attr_align>TY_align(idx))
		  Set_TY_align(idx,attr_align);
		// restrict qualifier not supported by gcc
		TYPE_TY_IDX(type_tree) = idx;
		if(Debug_Level >= 2) {
		  struct mongoose_gcc_DST_IDX dst =
		    Create_DST_type_For_Tree(type_tree,idx,orig_idx);
		  TYPE_DST_IDX(type_tree) = dst;
		}
		return idx;
	}

        level++;

	TYPE_ID mtype;
	INT tsize;
	BOOL variable_size = FALSE;
	tree type_size = TYPE_SIZE(type_tree);

	UINT align = TYPE_ALIGN(type_tree) / BITSPERBYTE;
	if (TREE_CODE(type_tree) == VOID_TYPE)
		tsize = 0;
	else
	if (type_size == NULL) {
		// incomplete structs have 0 size
		FmtAssert(TREE_CODE(type_tree) == ARRAY_TYPE
			|| TREE_CODE(type_tree) == UNION_TYPE
			|| TREE_CODE(type_tree) == RECORD_TYPE
			|| TREE_CODE(type_tree) == ENUMERAL_TYPE,
			  ("Create_TY_For_Tree: type_size NULL for non ARRAY/RECORD"));
		tsize = 0;
	}
	else {
		if (TREE_CODE(type_size) != INTEGER_CST) {
			if (TREE_CODE(type_tree) == ARRAY_TYPE)
				DevWarn ("Encountered VLA at line %d", lineno);
			else {
				error ("Variable-length arrays as members are not supported");
				exit (RC_UNIMPLEMENTED_ERROR);
			}
			variable_size = TRUE;
			tsize = 0;
		}
		else
			tsize = Get_Integer_Value(type_size) / BITSPERBYTE;
	}
	switch (TREE_CODE(type_tree)) {
	case VOID_TYPE:
		idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
#ifdef TENSILICA_CHANGES
	case TIE_TYPE:
		{
		  char* type_name =
			IDENTIFIER_POINTER(DECL_NAME(
			TYPE_NAME(TYPE_MAIN_VARIANT(type_tree))));

		  /* the support for non-ctype TIE types below is not
		     complete
		     need to revisit later
		  */
		  if (!strcmp(type_name,"_TIE_xtbool"))
		    idx = MTYPE_To_TY(MTYPE_XTBOOL);
		  else if (!strcmp(type_name,"_TIE_xtbool2"))
		    idx = MTYPE_To_TY(MTYPE_XTBOOL2);
		  else if (!strcmp(type_name,"_TIE_xtbool4"))
		    idx = MTYPE_To_TY(MTYPE_XTBOOL4);
		  else if (!strcmp(type_name,"_TIE_xtbool8"))
		    idx = MTYPE_To_TY(MTYPE_XTBOOL8);
		  else if (!strcmp(type_name,"_TIE_xtbool16"))
		    idx = MTYPE_To_TY(MTYPE_XTBOOL16);
		  else {
		    TYPE_ID mtype_id = tie_info->mtype_id(type_name);
		    idx = MTYPE_To_TY (mtype_id);
		  }
		}
		break;
#endif /* TENSILICA_CHANGES */
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:
		switch (tsize) {
		case 1:  mtype = MTYPE_I1; break;
		case 2:  mtype = MTYPE_I2; break;
		case 4:  mtype = MTYPE_I4; break;
		case 8:  mtype = MTYPE_I8; break;
#ifdef _LP64
		case 16:  mtype = MTYPE_I8; break;
#endif /* _LP64 */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		if (TREE_UNSIGNED(type_tree)) {
			mtype = MTYPE_complement(mtype);
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		Set_TY_align (idx, align);
		break;
	case CHAR_TYPE:
		mtype = (TREE_UNSIGNED(type_tree) ? MTYPE_U1 : MTYPE_I1);
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case ENUMERAL_TYPE:
		switch (tsize) {
		case 1:  mtype = (TREE_UNSIGNED(type_tree) ?
				 MTYPE_U1 : MTYPE_I1);
			 break;
		case 2:  mtype = (TREE_UNSIGNED(type_tree) ?
				 MTYPE_U2 : MTYPE_I2);
			 break;
		case 4:  mtype = (TREE_UNSIGNED(type_tree) ?
				 MTYPE_U4 : MTYPE_I4);
			 break;
		default: mtype = (TREE_UNSIGNED(type_tree) ?
				 MTYPE_U4 : MTYPE_I4);
			 break;
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case REAL_TYPE:
		switch (tsize) {
		case 4:  mtype = MTYPE_F4; break;
		case 8:  mtype = MTYPE_F8; break;
#ifdef TARG_MIPS
		case 16: mtype = MTYPE_FQ; break;
#endif /* TARG_MIPS */
#ifdef TARG_IA64
		case 12: mtype = MTYPE_F10; break;
#endif /* TARG_IA64 */
#ifdef TARG_IA32
		case 12: mtype = MTYPE_F10; break;
#endif /* TARG_IA32 */
		default: FmtAssert(FALSE, ("Get_TY unexpected REAL_TYPE size %d", tsize));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case COMPLEX_TYPE:
		switch (tsize) {
		case  8: mtype = MTYPE_C4; break;
		case 16: mtype = MTYPE_C8; break;
#ifdef TARG_MIPS
		case 32: mtype = MTYPE_CQ; break;
#endif /* TARG_MIPS */
#ifdef TARG_IA64
		case 24: mtype = MTYPE_C10; break;
#endif /* TARG_IA64 */
#ifdef TARG_IA32
		case 24: mtype = MTYPE_C10; break;
#endif /* TARG_IA32 */
		default:  FmtAssert(FALSE, ("Get_TY unexpected size"));
		}
		idx = MTYPE_To_TY (mtype);	// use predefined type
		break;
	case REFERENCE_TYPE:
	case POINTER_TYPE:
		idx = Make_Pointer_Type (Get_TY (TREE_TYPE(type_tree)));
		Set_TY_align (idx, align);
		break;
	case ARRAY_TYPE:
		{	// new scope for local vars
                char *type_name = TYPE_NAME(type_tree) ?
                  Get_Name(TYPE_NAME(type_tree)) : NULL;
		TY &ty = New_TY (idx);
		TY_Init (ty, tsize, KIND_ARRAY, MTYPE_M, Save_Str(type_name) );
		Set_TY_etype (ty, Get_TY (TREE_TYPE(type_tree)));
		Set_TY_align (idx, TY_align(TY_etype(ty)));
		// assumes 1 dimension
		// nested arrays are treated as arrays of arrays
		ARB_HANDLE arb = New_ARB ();
		ARB_Init (arb, 0, 0, 0);
		Set_TY_arb (ty, arb);
		Set_ARB_first_dimen (arb);
		Set_ARB_last_dimen (arb);
		Set_ARB_dimension (arb, 1);
                if (!TYPE_SIZE(TREE_TYPE(type_tree))) {
		  return TY_IDX_ZERO; // Error
                }
		if (TREE_CODE(TYPE_SIZE(TREE_TYPE(type_tree))) == INTEGER_CST) {
			Set_ARB_const_stride (arb);
			Set_ARB_stride_val (arb,
				Get_Integer_Value (TYPE_SIZE(TREE_TYPE(type_tree)))
				/ BITSPERBYTE);
		}
		else {
			WN *swn;
			swn = WFE_Expand_Expr (TYPE_SIZE_UNIT(TREE_TYPE(type_tree)));
			if (WN_opcode (swn) == OPC_U4I4CVT ||
			    WN_opcode (swn) == OPC_U8I8CVT) {
				swn = WN_kid0 (swn);
			}
			FmtAssert (WN_operator (swn) == OPR_LDID,
				("stride operator for VLA not LDID"));
			ST *st = WN_st (swn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_stride (arb);
			Set_ARB_stride_var (arb, (ST_IDX) ST_st_idx (st));
		}
		Set_ARB_const_lbnd (arb);
		Set_ARB_lbnd_val (arb, 0);
		if (type_size) {
		    if (TREE_CODE(TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree))) ==
			INTEGER_CST) {
			Set_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, Get_Integer_Value (
				TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) ));
		    }
		    else {
			WN *uwn = WFE_Expand_Expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type_tree)) );
			if (WN_opcode (uwn) == OPC_U4I4CVT ||
			    WN_opcode (uwn) == OPC_U8I8CVT) {
				uwn = WN_kid0 (uwn);
			}
			FmtAssert (WN_operator (uwn) == OPR_LDID,
				("bounds operator for VLA not LDID"));
			ST *st = WN_st (uwn);
			TY_IDX ty_idx = ST_type (st);
			WN *wn = WN_CreateXpragma (WN_PRAGMA_COPYIN_BOUND,
						   (ST_IDX) NULL, 1);
			WN_kid0 (wn) = WN_Ldid (TY_mtype (ty_idx), 0, st, ty_idx);
			WFE_Stmt_Append (wn, Get_Srcpos());
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_var (arb, ST_st_idx (st));
		    }
		}
		else {
			Clear_ARB_const_ubnd (arb);
			Set_ARB_ubnd_val (arb, 0);
			Set_ARB_ubnd_var (arb, ST_IDX_ZERO);
		}
		if (variable_size) {
			tree type_size_unit = TYPE_SIZE_UNIT(type_tree);
			(void)WFE_Expand_Expr (type_size_unit);
		}
		} // end array scope
		break;
	case RECORD_TYPE:
	case UNION_TYPE:
		{	// new scope for local vars
                char *type_name = TYPE_NAME(type_tree) ?
                  Get_Name(TYPE_NAME(type_tree)) : NULL;
		TY &ty = (idx == TY_IDX_ZERO) ? New_TY(idx) : Ty_Table[idx];
		TY_Init (ty, tsize, KIND_STRUCT, MTYPE_M, Save_Str(type_name) );
		if (TREE_CODE(type_tree) == UNION_TYPE) {
			Set_TY_is_union(idx);
		}
		if (align == 0) align = 1;	// in case incomplete type
		Set_TY_align (idx, align);
		// set idx now in case recurse thru fields
		TYPE_TY_IDX(type_tree) = idx;

		// to handle nested structs and avoid entering flds
		// into wrong struct, make two passes over the fields.
		// first create the list of flds for the current struct,
		// but don't follow the nested types.  Then go back thru
		// the fields and set the fld_type, recursing down into
		// nested structs.
  		Set_TY_fld (ty, FLD_HANDLE());
		FLD_IDX first_field_idx = Fld_Table.Size ();
		tree field;
		FLD_HANDLE fld;
		for (field = TREE_PURPOSE(type_tree);
			field;
			field = TREE_CHAIN(field) )
		{
			if (TREE_CODE(field) == TYPE_DECL) {
				DevWarn ("got TYPE_DECL in field list");
				continue;
			}
			if (TREE_CODE(field) == CONST_DECL) {
				// DevWarn ("got CONST_DECL in field list");
				continue;
			}
			fld = New_FLD ();
			FLD_Init (fld, Save_Str(Get_Name(DECL_NAME(field))),
				0, // type
				Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
				Get_Integer_Value(DECL_FIELD_BIT_OFFSET(field))
					/ BITSPERBYTE );
#ifdef OLDCODE
			if ( ! DECL_BIT_FIELD(field)
				&& TREE_CODE(TREE_TYPE(field))!=TIE_TYPE
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field)))
					* BITSPERBYTE) )
			{
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because
				// otherwise the field type is wrong.
				DevWarn("field size %d doesn't match type size %d",
					Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field)))
						* BITSPERBYTE );
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld,
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
#endif /* OLDCODE */
		}
  		FLD_IDX last_field_idx = Fld_Table.Size () - 1;
		if (last_field_idx >= first_field_idx) {
			Set_TY_fld (ty, FLD_HANDLE (first_field_idx));
			Set_FLD_last_field (FLD_HANDLE (last_field_idx));
		}
		// now set the fld types.
		fld = TY_fld(ty);
		for (field = TREE_PURPOSE(type_tree);
			field;
			field = TREE_CHAIN(field))
		{
			if (TREE_CODE(field) == TYPE_DECL)
				continue;
			if (TREE_CODE(field) == CONST_DECL)
				continue;
			if ( ! DECL_BIT_FIELD(field)
                                && DECL_SIZE(field)
				&& Get_Integer_Value(DECL_SIZE(field)) > 0
				&& Get_Integer_Value(DECL_SIZE(field))
				  != (TY_size(Get_TY(TREE_TYPE(field)))
					* BITSPERBYTE) )
			{
				// for some reason gnu doesn't set bit field
				// when have bit-field of standard size
				// (e.g. int f: 16;).  But we need it set
				// so we know how to pack it, because
				// otherwise the field type is wrong.
				DevWarn("field size %d doesn't match type size %d",
					Get_Integer_Value(DECL_SIZE(field)),
					TY_size(Get_TY(TREE_TYPE(field)))
						* BITSPERBYTE );
				DECL_BIT_FIELD(field) = 1;
			}
			if (DECL_BIT_FIELD(field)) {
				Set_FLD_is_bit_field (fld);
				// bofst is remaining bits from byte offset
				Set_FLD_bofst (fld,
//					Get_Integer_Value(DECL_FIELD_OFFSET(field)) +
					Get_Integer_Value(
						DECL_FIELD_BIT_OFFSET(field))
					% BITSPERBYTE );
				Set_FLD_bsize (fld, Get_Integer_Value(DECL_SIZE(field)));
			}
			TY_IDX fty_idx = Get_TY(TREE_TYPE(field));
			if ((TY_align (fty_idx) > align) || (TY_is_packed (fty_idx)))
				Set_TY_is_packed (ty);
			if (TREE_THIS_VOLATILE(field))
				Set_TY_is_volatile (fty_idx);
			Set_FLD_type(fld, fty_idx);
			fld = FLD_next(fld);
		}
		} // end record scope
		break;
	case METHOD_TYPE:
		DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
	case FUNCTION_TYPE:
		{	// new scope for local vars
		tree arg;
		INT32 num_args;
		TY &ty = New_TY (idx);
		TY_Init (ty, 0, KIND_FUNCTION, MTYPE_UNKNOWN, STR_IDX_ZERO);
		Set_TY_align (idx, 1);
		TY_IDX ret_ty_idx;
		TY_IDX arg_ty_idx;
		TYLIST tylist_idx;

		// allocate TYs for return as well as parameters
		// this is needed to avoid mixing TYLISTs if one
		// of the parameters is a pointer to a function

		ret_ty_idx = Get_TY(TREE_TYPE(type_tree));
		for (arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     arg = TREE_CHAIN(arg))
			arg_ty_idx = Get_TY(TREE_VALUE(arg));

		// if return type is pointer to a zero length struct
		// convert it to void
		if (!WFE_Keep_Zero_Length_Structs    &&
		    TY_mtype (ret_ty_idx) == MTYPE_M &&
		    TY_size (ret_ty_idx) == 0) {
			// zero length struct being returned
		  	DevWarn ("function returning zero length struct at line %d", lineno);
			ret_ty_idx = Be_Type_Tbl (MTYPE_V);
		}

		Set_TYLIST_type (New_TYLIST (tylist_idx), ret_ty_idx);
		Set_TY_tylist (ty, tylist_idx);
		for (num_args = 0, arg = TYPE_ARG_TYPES(type_tree);
		     arg;
		     num_args++, arg = TREE_CHAIN(arg))
		{
			arg_ty_idx = Get_TY(TREE_VALUE(arg));
			if (!WFE_Keep_Zero_Length_Structs    &&
			    TY_mtype (arg_ty_idx) == MTYPE_M &&
			    TY_size (arg_ty_idx) == 0) {
				// zero length struct passed as parameter
				DevWarn ("zero length struct encountered in function prototype at line %d", lineno);
			}
			else
				Set_TYLIST_type (New_TYLIST (tylist_idx), arg_ty_idx);
		}
		if (num_args)
		{
			Set_TY_has_prototype(idx);
			if (arg_ty_idx != Be_Type_Tbl(MTYPE_V))
			{
				Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
				Set_TY_is_varargs(idx);
			}
			else
				Set_TYLIST_type (Tylist_Table [tylist_idx], 0);
		}
		else
			Set_TYLIST_type (New_TYLIST (tylist_idx), 0);
		} // end FUNCTION_TYPE scope
		break;
	default:
		FmtAssert(FALSE, ("Get_TY unexpected tree_type"));
	}
	if (TYPE_READONLY(type_tree))
		Set_TY_is_const (idx);
	if (TYPE_VOLATILE(type_tree))
		Set_TY_is_volatile (idx);

	unsigned attr_align = 1 << TYPE_ATTR_ALIGN(type_tree);
	if (attr_align>TY_align(idx))
	  Set_TY_align(idx,attr_align);

	TYPE_TY_IDX(type_tree) = idx;

	if (Debug_Level >= 2) {

          tree new_type_tree = type_tree;
          TY_IDX new_idx = idx;

          if (variable_size) {
            // special case for VLAs (variable-length arrays)
            if (level == 1) {
              // at the outermost level, replace the array
              // type with the pointer to the element type
              new_type_tree = build_pointer_type(TREE_TYPE(type_tree));
              new_idx = Make_Pointer_Type(TY_AR_etype(idx));
            }
            else {
              // otherwise, replace the array type with the
              // pointer to the array type
              new_type_tree = build_pointer_type(type_tree);
              new_idx = Make_Pointer_Type(idx);
            }
          }

	  TYPE_DST_IDX(type_tree) =
            Create_DST_type_For_Tree(new_type_tree, new_idx, orig_idx);
	}

        level--;

	return idx;
}

ST*
Create_ST_For_Tree (tree decl_node)
{
  TY_IDX     ty_idx;
  ST*        st;
  char      *name;
  ST_SCLASS  sclass;
  ST_EXPORT  eclass;
  SYMTAB_IDX level;

  if (TREE_CODE(decl_node) == ERROR_MARK)
    exit (RC_USER_ERROR);

  DECL_DST_TYPE(decl_node) = NULL;

  if (DECL_ASSEMBLER_NAME (decl_node))
    name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node));
  else {
    DevWarn ("no name for DECL_NODE");
    name = "__unknown__";
  }

  switch (TREE_CODE(decl_node)) {

    case FUNCTION_DECL:
      {
        TY_IDX func_ty_idx = Get_TY(TREE_TYPE(decl_node));

        if (DECL_WIDEN_RETVAL (decl_node)) {
/*
          extern tree long_long_integer_type_node;
          extern tree long_long_unsigned_type_node;
*/
          tree type_tree = TREE_TYPE(decl_node);
          tree ret_type_tree = TREE_TYPE (type_tree);
          TY_IDX ret_ty_idx = Get_TY(ret_type_tree);
	  if (MTYPE_signed (TY_mtype (ret_ty_idx)))
            TREE_TYPE (type_tree) = long_long_integer_type_node;
          else
            TREE_TYPE (type_tree) = long_long_unsigned_type_node;
          TY_IDX old_func_ty_idx = func_ty_idx;
          func_ty_idx = Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
          TREE_TYPE (type_tree) = ret_type_tree;
          TYPE_TY_IDX(type_tree) = old_func_ty_idx;
        }

        sclass = SCLASS_EXTERN;
        eclass = TREE_PUBLIC(decl_node) ? EXPORT_PREEMPTIBLE : EXPORT_LOCAL;
        level  = GLOBAL_SYMTAB+1;

        PU_IDX pu_idx;
        PU&    pu = New_PU (pu_idx);

        PU_Init (pu, func_ty_idx, level);

	if (strcmp(name, "main") == 0) {
          Set_PU_is_mainpu(pu);
          Set_PU_no_inline(pu);
	}
        if (TREE_READONLY(decl_node)) {
          Set_PU_is_pure(pu);
    	} else if (DECL_IS_PURE(decl_node)) {
          Set_PU_no_side_effects(pu);
    	}


        st = New_ST (GLOBAL_SYMTAB);

        ST_Init (st,
                 Save_Str ( IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl_node))),
                 CLASS_FUNC, sclass, eclass, TY_IDX (pu_idx));
/*
       if (TREE_CODE(TREE_TYPE(decl_node)) == METHOD_TYPE)
         fprintf (stderr, "Create_ST_For_Tree: METHOD_TYPE\n");
*/
      }
      break;

    case PARM_DECL:
    case VAR_DECL:
      {
        if (TREE_CODE(decl_node) == PARM_DECL) {
          sclass = SCLASS_FORMAL;
          eclass = EXPORT_LOCAL;
          level = CURRENT_SYMTAB;
        }
        else {
          if (DECL_CONTEXT (decl_node) == 0) {
            if (TREE_PUBLIC (decl_node)) {
	      if (DECL_INITIAL(decl_node))
		sclass = SCLASS_DGLOBAL;
	      else if (TREE_STATIC(decl_node)) {
		if (!DECL_COMMON(decl_node) || DECL_SECTION_NAME(decl_node))
              	  sclass = SCLASS_UGLOBAL;
		else
              	  sclass = SCLASS_COMMON;
	      }
	      else
              	sclass = SCLASS_EXTERN;
              eclass = EXPORT_PREEMPTIBLE;
            }
            else {
              	sclass = SCLASS_FSTATIC;
		eclass = EXPORT_LOCAL;
            }
            level = GLOBAL_SYMTAB;
          }
          else {
            if (DECL_EXTERNAL(decl_node)) {
	      sclass = SCLASS_EXTERN;
	      level  = GLOBAL_SYMTAB;
              eclass = EXPORT_PREEMPTIBLE;
            }
            else {
	      if (TREE_STATIC (decl_node)) {
		sclass = SCLASS_PSTATIC;
		if (pstatic_as_global)
			level = GLOBAL_SYMTAB;
		else
			level = CURRENT_SYMTAB;
              }
              else {
		sclass = SCLASS_AUTO;
		level = decl_node->decl.symtab_idx ?
                        decl_node->decl.symtab_idx : CURRENT_SYMTAB;
              }
              eclass = EXPORT_LOCAL;
            }
          }
        }
        st = New_ST (level);
	if(Debug_Level >= 2) {
	  UINT qualifiers = TYPE_UNQUALIFIED;
	  if (TREE_THIS_VOLATILE(decl_node))
	    qualifiers |= TYPE_QUAL_VOLATILE;
	  if (TREE_READONLY(decl_node))
	    qualifiers |= TYPE_QUAL_CONST;
	  if (qualifiers != TYPE_UNQUALIFIED) {
	    tree type_tree = TREE_TYPE(decl_node);
	    if (TREE_CODE (type_tree) == ERROR_MARK) {
		error("Bad type");
		exit (RC_USER_ERROR);
	    }
	    DECL_DST_TYPE(decl_node) = build_qualified_type(type_tree, qualifiers);
	    (void)Get_TY (DECL_DST_TYPE(decl_node));
	  }
	}

        ty_idx = Get_TY (TREE_TYPE(decl_node));
        if (TY_kind (ty_idx) == KIND_ARRAY &&
            TREE_STATIC (decl_node) &&
            DECL_INITIAL (decl_node) == FALSE &&
            TY_size (ty_idx) == 0) {
          Set_TY_size (ty_idx, TY_size (Get_TY (TREE_TYPE (TREE_TYPE (decl_node)))));
        }
	if (TY_mtype (ty_idx) == MTYPE_M &&
	    Aggregate_Alignment > 0 &&
	    Aggregate_Alignment > TY_align (ty_idx))
	  Set_TY_align (ty_idx, Aggregate_Alignment);

	// for typedefs check if base type has stricter alignment 
	// pr14202, Without this it's possible that a typedef variable
	// is not sufficiently aligned when the typedef and variable 
	// declaration preceed the actual structure definition
	tree type_tree = TREE_TYPE(decl_node);
	if (TYPE_NAME(type_tree) &&
	    (TREE_CODE(type_tree) == RECORD_TYPE ||
	     TREE_CODE(type_tree) == UNION_TYPE) &&
	       TYPE_MAIN_VARIANT(type_tree) != type_tree) {
	    TY_IDX base_idx = Get_TY(TYPE_MAIN_VARIANT(type_tree));
	    if (TY_align(base_idx) > TY_align(ty_idx)) {
	      Set_TY_align(ty_idx, TY_align(base_idx));
	      TYPE_TY_IDX(type_tree) = base_idx;
	    }
	}
	// In the C++ front end we can use TYPE_READONLY and TYPE_VOLATILE
	// on the TREE_TYPE node.
	// In the C front end these qualifiers may not have been set yet,
	// and we have to check the declaration node.
	// Qualifiers in an array declaration apply only to the element type.
	if (TREE_READONLY(decl_node) && TY_kind(ty_idx) != KIND_ARRAY)
          Set_TY_is_const(ty_idx);
	if (TREE_THIS_VOLATILE(decl_node) && TY_kind(ty_idx) != KIND_ARRAY)
          Set_TY_is_volatile(ty_idx);
	if (DECL_RESTRICT(decl_node) && TY_kind(ty_idx) != KIND_ARRAY)
          Set_TY_is_restrict(ty_idx);

	INT attr_align = 1 << DECL_ATTR_ALIGN(decl_node);
	if (attr_align>TY_align(ty_idx))
	  Set_TY_align(ty_idx,attr_align);

#ifdef STACK_BOUNDARY
	int align_units = STACK_BOUNDARY;
#else
	int align_units = BIGGEST_ALIGNMENT;
#endif
	align_units /= BITS_PER_UNIT;

	if (level!=GLOBAL_SYMTAB && !TREE_STATIC(decl_node) &&
	    TY_align(ty_idx)>align_units) {
	  warning("alignment of non-global variable `%s' too large (%d), reset to %d",
		  name, TY_align(ty_idx), align_units);
	  Set_TY_align(ty_idx,align_units);
	}

        ST_Init (st, Save_Str(name), CLASS_VAR, sclass, eclass, ty_idx);

	INT fill = DECL_FILL(decl_node);
        if (fill) {
	  Set_ST_is_fill_align(*st);
          ST_ATTR_IDX st_attr_idx;
          ST_ATTR& st_attr = New_ST_ATTR (level, st_attr_idx);
          ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_FILL, fill);
        }

        if (TREE_CODE(decl_node) == PARM_DECL)
          Set_ST_is_value_parm(st);
        if (TREE_CODE(decl_node) == VAR_DECL &&
            TREE_STATIC(decl_node) &&
            TREE_READONLY(decl_node))
          Set_ST_is_const_var(st);
        
        /* Disable forward substitution on restrict non-formals in order
           to preserve correct alias information. Functions with
           restrict formals don't get inlined so forward substitution
           should be OK (eg., for *p++ purposes). */
        if (TY_is_restrict(ty_idx) && (sclass != SCLASS_FORMAL))
          Set_ST_dont_prop(st);
      }
      break;

    default:
      {
        Fail_FmtAssertion ("Create_ST_For_Tree: unexpected tree type");
      }
      break;
  }

  DECL_ST(decl_node) = st;

  if ((DECL_WEAK (decl_node)) && (TREE_CODE (decl_node) != PARM_DECL)) {
    Set_ST_is_weak_symbol (st);
/*
    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      Set_ST_sclass (st, SCLASS_TEXT);
*/
  }

  if (DECL_SECTION_NAME (decl_node)) {
    char *section_name = TREE_STRING_POINTER(DECL_SECTION_NAME(decl_node));
    if (strstr(section_name,".literal")) {
      error("Can not use .literal in section attribute %s \n",section_name);
      exit (RC_USER_ERROR);
    } else if (strstr(section_name,".lit4")) {
      error("Can not use .lit4 in section attribute %s \n",section_name);;
      exit (RC_USER_ERROR);
    }

    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      level = GLOBAL_SYMTAB;
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
                  Save_Str (TREE_STRING_POINTER (DECL_SECTION_NAME (decl_node))));
    Set_ST_has_named_section (st);
  }

  if (DECL_ROSECTION_NAME (decl_node)) {
    char *section_name = TREE_STRING_POINTER(DECL_ROSECTION_NAME(decl_node));
    if (strstr(section_name,".literal")) {
      error("Can not use .literal in section attribute %s \n",section_name);
      exit (RC_USER_ERROR);
    } else if (strstr(section_name,".lit4")) {
      error("Can not use .lit4 in section attribute %s \n",section_name);;
      exit (RC_USER_ERROR);
    }
    if (TREE_CODE (decl_node) == FUNCTION_DECL)
      level = GLOBAL_SYMTAB;
    ST_ATTR_IDX st_attr_idx;
    ST_ATTR&    st_attr = New_ST_ATTR (level, st_attr_idx);
    ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_RO_DATA_SECTION_NAME,
                  Save_Str (TREE_STRING_POINTER (DECL_ROSECTION_NAME (decl_node))));
    Set_ST_has_named_ro_section (st);
  }

  if (DECL_SYSCALL_LINKAGE (decl_node)) {
	Set_PU_has_syscall_linkage (Pu_Table [ST_pu(st)]);
  }
  if(Debug_Level >= 2) {
     tree type_save = TREE_TYPE(decl_node);
     if (DECL_DST_TYPE(decl_node))
       TREE_TYPE(decl_node) = DECL_DST_TYPE(decl_node);
     struct mongoose_gcc_DST_IDX dst =
       Create_DST_decl_For_Tree(decl_node,st);
     DECL_DST_IDX(decl_node) = dst;
     DECL_DST_TYPE(decl_node) = TREE_TYPE(decl_node);
     TREE_TYPE(decl_node) = type_save;
  }
  return st;
}
