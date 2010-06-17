
/* 
   Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.
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


#include <stdio.h>

#include "basic.h"
#include "string_utils.h"
#include "objects.h"
#include "option_names.h"
#include "options.h"
#include "option_seen.h"
#include "opt_actions.h"
#include "get_options.h"
#include "errors.h"
#include "lang_defs.h"
#include "file_names.h"
#include "file_utils.h"
#include "run.h"

string_list_t *objects;
string_list_t *lib_objects;
static string_list_t *cxx_prelinker_objects;
static string_list_t *ar_objects; 
string_list_t *library_dirs;
string_list_t *ld_args_objs;

extern void
init_objects (void)
{
 	objects = init_string_list();
 	ld_args_objs = init_string_list();
 	lib_objects = init_string_list();
 	cxx_prelinker_objects = init_string_list();
 	ar_objects = init_string_list();
	library_dirs = init_string_list();
}

/* whether option is an object or not */
extern boolean
is_object_option (int flag)
{
	switch (flag) {
	case O_none:
	case O_object:
	case O_objectlist:
	case O_l:
		return TRUE;
	default:
		return FALSE;
	}
}

/* library list options get put in object list,
 * so order w.r.t. libraries is preserved. */
extern void
add_object (int flag, string arg)
{
    /* cxx_prelinker_object_list contains real objects, -objectlist flags. */
	switch (flag) {
	case O_l:
		/* xpg fort77 has weird rule about putting all libs after objects */
		if (xpg_flag && invoked_lang == L_f77) {
			add_string(lib_objects, concat_strings("-l",arg));
		} else {
			add_string(objects, concat_strings("-l",arg));
			add_string(ld_args_objs, concat_strings("-l",arg));
		}
		if (invoked_lang == L_CC) {
		    add_string(cxx_prelinker_objects,concat_strings("-l",arg));
		}
		break;
 	case O_none: add_string(objects, get_option_name(flag));
 	             add_string(ld_args_objs, get_option_name(flag));
                                                break;
        case O_objectlist:
                add_multi_strings(objects, concat_strings("-objectlist ",arg));
                add_multi_strings(ld_args_objs, concat_strings("-objectlist ",arg));
                if (invoked_lang == L_CC) {
                    add_string(cxx_prelinker_objects,
                                concat_strings("-YO=",arg));
                }
                break;
        case O_object:
                if (dashdash_flag && arg[0] == '-') {
                  add_string(objects,"--");
                  add_string(ld_args_objs,"--");
                  dashdash_flag = 1;
                }
                add_string(objects, arg);
                add_string(ld_args_objs, arg);
                if (invoked_lang == L_CC) {
                    add_string(cxx_prelinker_objects, arg);
                }

                break;

	default:
		internal_error("add_object called with not-an-object");
	}
}

/* append object files to the ar_objects list. */
extern void
add_ar_objects (string arg)
{
    add_string(ar_objects, arg);
}

/* append objects to end of list */
extern void
append_objects_to_list (string_list_t *list)
{
	append_string_lists (list, objects);
	if (xpg_flag && invoked_lang == L_f77) {
		append_string_lists (list, lib_objects);
	}
}

extern void
append_ar_objects_to_list(string_list_t *list)
{
    append_string_lists (list, ar_objects);
}

extern void
append_libraries_to_list (string_list_t *list)
{
        string_item_t *p;
        for (p = library_dirs->head; p != NULL; p = p->next) {
		add_string(list, concat_strings("-L", p->name));
        }
        /*
         * get_phase_dir(P_library) is not in library_dirs because
         * library_dirs is also used as the search path for the crt file
         */
        if (!option_was_seen(O_L)) {
                add_string(list,
                           concat_strings("-L", get_phase_dir(P_library)));
        }
}

extern void
dump_objects (void)
{
	printf("objects:  ");
	print_string_list (stdout, objects);
}

extern void
add_library_dir (string path)
{
	add_string(library_dirs, path);
}

extern void
add_library_options (void)
{
	switch (abi) {
	case ABI_WINDOWED:
	        break;
	case ABI_CALL0:
	        break;
	default:
		internal_error("no abi set? (%d)", abi);
	}
}

