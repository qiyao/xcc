
/* 
   Copyright (C) 2001-2007 Tensilica, Inc.  All Rights Reserved.
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

#ifndef tree_symtab_INCLUDED
#define tree_symtab_INCLUDED

#include <cmplrs/rcodes.h>

extern PREG_NUM asm_neg_preg;

extern TY_IDX Create_TY_For_Tree (tree, TY_IDX idx = TY_IDX_ZERO);
extern "C" ST* Create_ST_For_Tree (tree);

/* 
 * either return a previously created TY_IDX associated with a type,
 * or create a new one.
 */
inline TY_IDX
Get_TY (tree type_tree)
{
	if (!type_tree || TREE_CODE(type_tree) == ERROR_MARK)
	    exit (RC_USER_ERROR);
	TY_IDX idx = TYPE_TY_IDX(type_tree);
        if (idx != 0) {
            tree pointed_type_tree = (TREE_CODE(type_tree) == POINTER_TYPE ?
                                      TREE_TYPE(type_tree) : type_tree);
	    if (TREE_CODE(pointed_type_tree) == RECORD_TYPE ||
	        TREE_CODE(pointed_type_tree) == UNION_TYPE) {
	      FLD_HANDLE elt_fld = TY_fld(idx);
	      if (elt_fld.Is_Null()) 
		return Create_TY_For_Tree (type_tree, idx); // forward declared
	      else return idx;
	    }
	    else return idx;
        }
	return Create_TY_For_Tree (type_tree, TY_IDX_ZERO);
}

extern STR_IDX Find_Section_Name_For_ST (const ST *st);
extern STR_IDX Find_Rosection_Name_For_ST (const ST *st);
extern "C" void error (const char *, ...);
extern struct mongoose_gcc_DST_IDX Create_DST_decl_For_Tree( tree decl, ST* var_st);


/*
 * either return a previously created ST associated with a
 * var-decl/parm-decl/function_decl, or create a new one.
 */
inline ST *
Get_ST (tree decl_tree)
{
	ST *st = DECL_ST(decl_tree);
        if (st != NULL) {
	    if (TREE_CODE(decl_tree) == VAR_DECL &&
		    ST_sclass(st) == SCLASS_EXTERN   &&
		    !ST_is_weak_symbol(st)           &&
		    !DECL_EXTERNAL(decl_tree)        &&
		    !DECL_INITIAL(decl_tree)) {
		Set_ST_sclass (st, SCLASS_UGLOBAL);
	    }

            if (TREE_CODE(decl_tree) == VAR_DECL && TREE_STATIC(decl_tree) &&
                	TREE_READONLY(decl_tree)) {
                Set_ST_is_const_var(st);
            }


	    // Fix from KEY
	    // the earlier definition may not have the complete type
            if (TREE_CODE(decl_tree) == VAR_DECL) {
              TY_IDX ty_idx = Get_TY(TREE_TYPE(decl_tree));
              if (ty_idx && TY_IDX_index(ty_idx) != TY_IDX_index(st->u2.type))
                    st->u2.type = ty_idx;
	       if (Debug_Level >= 2) {
                 struct mongoose_gcc_DST_IDX dst = Create_DST_decl_For_Tree(decl_tree,st);
  		 DECL_DST_IDX(decl_tree) = dst;
	       }
            }

	    // If a function is declared twice, once with a section attribute
	    // and once without, take the section attribute regardless of order
	    // If it's declared twice with different section names, error out
            if (TREE_CODE(decl_tree) == FUNCTION_DECL) {
	      if (DECL_SECTION_NAME (decl_tree)) {
		char *section_name = TREE_STRING_POINTER(DECL_SECTION_NAME(decl_tree));
		if (strstr(section_name,".literal")) {
		  error("Can not use .literal in section attribute %s \n",section_name);
		  exit (RC_USER_ERROR);
                } else if (strstr(section_name,".lit4")) {
		  error("Can not use .lit4 in section attribute %s \n",section_name);;
		  exit (RC_USER_ERROR);
                }
	        if (!ST_has_named_section(st)) {  
	          ST_ATTR_IDX st_attr_idx;
	          ST_ATTR&  st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
	          ST_ATTR_Init (st_attr, ST_st_idx (st), ST_ATTR_SECTION_NAME,
	                Save_Str (TREE_STRING_POINTER (
					DECL_SECTION_NAME (decl_tree))));
	          Set_ST_has_named_section (st);
	        } else {
		  STR_IDX old_sec_idx = Find_Section_Name_For_ST(st);
		  char *old_sec_name = Index_To_Str(old_sec_idx);
	          if (strcmp(TREE_STRING_POINTER(DECL_SECTION_NAME(decl_tree)),  
					  old_sec_name)) {
		    error("Section attribute %s conflicts with previous %s \n",
			old_sec_name,
			TREE_STRING_POINTER(DECL_SECTION_NAME(decl_tree)));
                    exit (RC_USER_ERROR);
		  }
		}
	      }
	      if (DECL_ROSECTION_NAME (decl_tree)) {
		char *section_name = TREE_STRING_POINTER(DECL_ROSECTION_NAME(decl_tree));
		if (strstr(section_name,".literal")) {
		  error("Can not use .literal in section attribute %s \n",section_name);
		  exit (RC_USER_ERROR);
                } else if (strstr(section_name,".lit4")) {
		  error("Can not use .lit4 in section attribute %s \n",section_name);;
		  exit (RC_USER_ERROR);
		}
	        if (!ST_has_named_ro_section(st)) {  
	          ST_ATTR_IDX st_attr_idx;
	          ST_ATTR& st_attr = New_ST_ATTR (GLOBAL_SYMTAB, st_attr_idx);
	          ST_ATTR_Init (st_attr, ST_st_idx (st), 
				  ST_ATTR_RO_DATA_SECTION_NAME,
	          Save_Str (TREE_STRING_POINTER (
				  DECL_ROSECTION_NAME (decl_tree))));
	          Set_ST_has_named_ro_section (st);
	        } else {
		  STR_IDX old_rosec_idx = Find_Rosection_Name_For_ST(st);
		  char *old_rosec_name = Index_To_Str(old_rosec_idx);
	          if (strcmp(TREE_STRING_POINTER(DECL_ROSECTION_NAME(decl_tree)),  
					  old_rosec_name)) {
		    error("Rosection attribute %s conflicts with previous %s \n",
			old_rosec_name,
			TREE_STRING_POINTER(DECL_ROSECTION_NAME(decl_tree)));
                    exit (RC_USER_ERROR);
		  }
		}
	      }
	    }

	} else {
	    st = Create_ST_For_Tree (decl_tree);
	}

	if ((CURRENT_SYMTAB > GLOBAL_SYMTAB + 1) &&
	    ((TREE_CODE(decl_tree) == VAR_DECL) ||
	     (TREE_CODE(decl_tree) == PARM_DECL)) &&
	    (ST_level(st) < CURRENT_SYMTAB) &&
	    (ST_level(st) > GLOBAL_SYMTAB)) {
		Set_ST_has_nested_ref (st);
		ST *base_st = st;
		while (base_st != ST_base (base_st)) {
			base_st = ST_base (base_st);
			Set_ST_has_nested_ref (base_st);
		}
	}
	return st;
}

#endif
