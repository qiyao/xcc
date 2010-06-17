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


#include <alloca.h>
#include <stdio.h>
#include <stdlib.h>
#include <stamp.h>
#include <cmplrs/rcodes.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include "string_utils.h"
#include "options.h"
#include "option_seen.h"
#include "option_names.h"
#include "opt_actions.h"
#include "get_options.h"
#include "lang_defs.h"
#include "errors.h"
#include "phases.h"
#include "file_utils.h"
#include "file_names.h"
#include "run.h"
#include "objects.h"

#include <xtensa-params.h>

extern void expandargv (int *, char ***);

string help_pattern = NULL;
boolean debug = FALSE;
boolean nostdinc = FALSE;
boolean nostdincplusplus = FALSE;
boolean show_version = FALSE;
extern void check_for_combos(void);
extern boolean is_replacement_combo(int);
extern void toggle_implicits(void);
extern void set_defaults(void);
extern void add_special_options (void);

static void check_old_CC_options (string name);
static void mark_used (void);
static void dump_args (string msg);
static void print_help_msg (void);

static string_list_t *files;
static string_list_t *file_suffixes;
string_list_t *feedback_files;

static char compiler_version[] = INCLUDE_STAMP;

static void add_source_file(char * file_name);

#define XTTOOLS_STRING "XTTOOLS=%s"

extern int
main (int argc, char *argv[])
{
	int i;		/* index to argv */
	int flag;
	int base_flag;
	string_item_t *p, *q;
	int num_files = 0;
	char * xttools_env;

	xtensa_params params;
	char * xtensa_dir;

	expandargv(&argc, &argv);

	if ((params = xtensa_getopt(&argc, argv)) == NULL)
		exit(RC_USER_ERROR);
	if ((xtensa_dir = xtensa_path_value
	     (xtensa_find_key(params, "install-prefix"))) == NULL)
		exit(RC_INTERNAL_ERROR);
	save_command_line(argc, argv);		/* for prelinker    */
	program_name = drop_path(argv[0]);	/* don't print path */
	orig_program_name = string_copy(argv[0]);
	files = init_string_list();
	file_suffixes = init_string_list();
	feedback_files = init_string_list();	/* for cord feedback files */
	init_options();
	init_temp_files();
	init_count_files();
	init_option_seen();
	init_objects();

#ifdef linux
	{
	  /* Disable virtual address randomization.  */
	  extern int personality (unsigned long);
	  unsigned long persona = personality (-1);
	  /* 0x40000 = ADDR_NO_RANDOMIZE */
	  if ((persona & 0x40000) == 0)
	    personality (persona | 0x40000);
	}
#endif /* linux */

	invoked_lang = get_named_language(program_name);
	check_for_driver_controls (argc, argv);

	prefix_all_phase_dirs(PHASE_MASK, xtensa_dir);
	xttools_env = alloca(strlen(XTTOOLS_STRING) + strlen(xtensa_dir) + 1);
	sprintf(xttools_env, XTTOOLS_STRING, xtensa_dir);
	putenv(xttools_env);
	init_phase_info();	/* can't add toolroot until other prefixes */

        /* Hack for F90 ftpp; For pre processing F90 calls ftpp;
         * Unlike cpp, ftpp does not like the -Amachine(mips) and -Asystem(unix)
         * that are passed to cpp; we need to remove these if ftpp is usSxt-ed for
         * preprocessing. Also remove all the woff options for cpp and ftpp.
         */

	remove_phase_for_option(O_A,P_f90_cpp);
	remove_phase_for_option(O_E,P_f90_cpp);

	i = 1;

	lsp_name=NULL;
	while (i < argc) {
		option_name = argv[i];
		set_current_arg_pos(i);
		if (argv[i][0] == '-' && !dashdash_flag) {
			flag = get_option(&i, argv);
			if (flag == O_Unrecognized) {
			  parse_error(option_name, "unknown flag");
			}
			else {
			  if (flag == O_) {
                            source_kind = get_source_kind("-");
                            if (last_phase == P_any_cpp && source_kind == S_o)
                              source_kind = S_c;
			    add_source_file("-");
			    num_files ++;
			  }
                          /* reset option name to possibly include
                           * 2nd part, e.g. -G 8 */
			  option_name = get_option_name(flag);
			}
			/* sometimes is simple alias to another flag */
			flag = get_real_option_if_aliased (flag);

			/* sometimes need to look at parent flag */
			if (is_derived_option (flag)) {
				base_flag = get_derived_parent(flag);
				/* sometimes base is simple alias */
				base_flag = get_real_option_if_aliased (base_flag);
			}
			else {
				base_flag = flag;
			}

			if (is_object_option(base_flag)) {
				/* put in separate object list */
				add_object (base_flag, optargs);
				source_kind = S_o;
			} else {
				/* add unique real flag to list */
				add_option_seen (flag);
			}

			opt_action(base_flag);

		} else if (argv[i][0] == '+') {
			check_old_CC_options(argv[i]);
			i++;
		} else {
			source_kind = get_source_kind(argv[i]);
			/* if -E used, then preprocess anything, even .o's */
			if (last_phase == P_any_cpp && source_kind == S_o)
			{
				source_kind = S_c;
			}
			if (source_kind == S_o) {
			  struct stat statbuf;
			  int statval = stat(argv[i], &statbuf);
			  if (statval == 0) {
			    /* object file or library */
			    add_object (O_object, argv[i]);
			    /* Save away objects should it be necessary
			       to invoke the archive phase (-ar option). */
			    add_ar_objects(argv[i]);
			  }
			  else {
			    warning("%s: %s", argv[i], strerror(errno));
			  }
			} else {
			  add_source_file(argv[i]);
			  num_files++;
			}
			cancel_saved_arg(1);
			i++;
		}
	}

	/* Check target specifications for consistency: */
	Check_Target ();

        if (show_version) {
            /* Echo information about the compiler version */
            fprintf(show_flag ? stderr : stdout,
                    "xt-xcc version %s\n", compiler_version);
        }

	if (argc == 1 || option_was_seen(O_help) || option_was_seen(O__help)
		|| help_pattern != NULL)
	{
		print_help_msg();
	}
	if ( ! execute_flag && ! show_flag) {
		exit(RC_OKAY);	/* just exit */
	}
	if (source_kind == S_NOTHING) {
		if (show_version) {	/* just exit */
		  if (!option_was_seen(O_xpres) || (ipa == TRUE))
		    exit(RC_OKAY);
		}
		else if (!option_was_seen(O_xpres) || (ipa == TRUE)) {
		  extern int O_W_was_seen;
		  if (!O_W_was_seen)
		    error("no source or object file given");
		}
	}
	/* if toggle flags have superceded previous flags,
	 * or if option has been repeated,
	 * unmark the previous flags:
	 */
	FOREACH_OPTION_SEEN(i) {
		if (current_option_seen_later(i)) {
			set_current_option_unseen();
		} else if (flag_is_superceded(i)) {
			set_option_unseen(i);
		}
	}

	/* check for certain combinations of options */
	check_for_combos();

        /* with -Os set default optimization level to -O2 */
        if (is_toggled(ospace) && !is_toggled(olevel)) {
          toggle(&olevel, 2);
          flag = add_string_option(O_D, "__OPTIMIZE__");
          prepend_option_seen(flag);
          prepend_option_seen(O_O2);
        }

        /* -pg only makes sense on Linux configurations */
        if (option_was_seen(O_pg)) {
          xtensa_values *xt_val =
            xtensa_find_key(xtensa_default_params, "alwaysPIC");
          if (xt_val && xt_val->num_values == 1 && xtensa_int_value(xt_val)) {
            flag = add_string_option(O_TENV_, "call_mcount");
            prepend_option_seen(flag);
          }
          else {
            warning("configuration does not support -pg: use xt-run --profile");
          }
        }

	if (debug) {
		dump_args("user flags");
	}
	if (ipa == TRUE)
	    save_ipl_commands ();

	last_phase=earliest_phase(P_any_ld, last_phase);

	/* mark the used options */
	mark_used();

	/* call toggle routine for implicitly-used options */
	/*toggle_implicits();*/

	/* add defaults if not already set */
	set_defaults();

	if (num_files > 1) {
		multiple_source_files = TRUE;
	}
	/* handle anything else */
	add_library_options();
	add_special_options();

	if (debug) {
		dump_args("with defaults");
		dump_objects();
	}
	if (has_errors()) return error_status;

	catch_signals();
	remember_last_phase = last_phase;

/* for DRA (Distributed Reshape Array) templitization, we want to
 * run prelinker and ld later
 * ??? why not just have ar and dsm prelink be set in determine_phase_order?
 */
	if ((multiple_source_files 
	     ) &&
	    ((last_phase == P_any_ld) && (shared != RELOCATABLE))) {
	  /* compile all files to object files, do ld later */
	  last_phase = P_any_as;
	  add_minus_c_option();	/* for .ii file */
	}
	cur_source_file_num = 0;
	for (p = files->head, q=file_suffixes->head; p != NULL; p = p->next, q=q->next)
	{
		source_file = p->name;
		if (multiple_source_files && option_was_seen(O_v)) {
			fprintf(stderr, "%s:\n", source_file);
		}
		//this strcmp is to check for the file stdin, which always exists
		if (strcmp(source_file, "-") && execute_flag && !file_exists(source_file)) {
			error("file does not exist:  %s", source_file);
			continue;
		}

                source_kind = get_source_kind_from_suffix(q->name);
		if (source_kind == S_o && !strcmp(p->name, "-")) {
                  error("-E or specified language required "
                        "when input is from standard input");
                  exit(RC_USER_ERROR);
		}
		source_lang = get_source_lang(source_kind);
		if (source_lang != invoked_lang
			&& last_phase == P_any_ld
			&& source_lang != L_as
			&& (fullwarn || (source_lang == L_f90)) )
		{
			warning("compiler not invoked with language of source file; will compile with %s but link with %s", get_lang_name(source_lang), get_lang_name(invoked_lang));
		}
		run_compiler();
		//if (multiple_source_files) cleanup();
		cur_source_file_num++;
	}

	if (has_errors()) {
		cleanup();
		return error_status;
	}

	if (num_files == 0 || remember_last_phase != last_phase) {
		/* run ld */
		last_phase = remember_last_phase;
		source_file = NULL;
		source_kind = S_o;
		source_lang = get_source_lang(source_kind);


		run_ld ();
	}
	cleanup();
	return error_status;
}

static void add_source_file(char * file_name)
{
  /*
   * Reserve place in object list for .o,
   * so multiple .o's have same position
   * relative to other parameters.
   * If only one source file, then will
   * treat this as a temp file.
   */
    char *obj_name = get_object_file(file_name);
    add_object (O_object, obj_name);
    add_ar_objects(obj_name);
    add_string(files, file_name);
    /* Because of -x <lang> option,
     * we need position as well as name to
     * determine a file's source kind.
     * So we want to record the source_kind now,
     * before we lose the position info.
     * Thus have parallel list of file suffixes,
     * which will be the known suffix that we
     * want to treat this file as, e.g.
     * -x assembler t.asm will give suffix "s".
     * Use string_list just cause it is readily
     * available. */
    add_string(file_suffixes,
	       get_suffix_string(source_kind));
}


static void
check_old_CC_options (string name)
{
	if (same_string(name, "+I")) {
		warn_no_longer_supported2(name, "-keep");
	} else if (same_string(name, "+L")) {
		warn_no_longer_supported(name);
	} else if (same_string(name, "+d")) {
		warn_no_longer_supported2(name, "-INLINE:none");
	} else if (same_string(name, "+p")  ||
	           same_string(name, "+pc") ||
	           same_string(name, "+pa")) {
		warn_ignored(name);
		warning("the effect of +p is now the default (see -anach and -cfront)");
	} else if (same_string(name, "+v")) {
		warn_no_longer_supported2(name, "-show");
	} else if (same_string(name, "+w")) {
		warn_no_longer_supported2(name, "-fullwarn");
	} else if (same_string(name, "+a0")) {
		warn_no_longer_supported(name);
	} else if (same_string(name, "+a1")) {
		warn_no_longer_supported(name);
	} else {
		parse_error(name, "bad syntax");
	}
}


/* mark all implied options as implicitly seen */
static void
mark_used (void)
{
  int i;
  int iflag;

  FOREACH_OPTION(i) {
    if (option_was_seen(i) && !option_was_implicitly_seen(i)) {
      FOREACH_IMPLIED_OPTION(iflag, i) {
	if (option_was_seen(iflag)) {
		continue;	/* already on list */
	}
	add_option_implicitly_seen (iflag);
	if (is_object_option(iflag)
	  && option_matches_language(i, invoked_lang) )
	{
	  /* put in object list. */
	  /* can assume it is ld option. */
	  /* name is full name, so cheat
	   * by saying it is an object,
	   * even if is really -lname. */
	  add_object (O_object, get_current_implied_name());
	}
      }
    }
  }
}

static void
print_help_msg (void)
{
	int i;
	string msg;
	string name;
	fprintf(stdout, "usage:  %s <options> <files>\n", program_name);
	if (help_pattern != NULL)
	  fprintf(stdout, "available options that contain %s:\n", help_pattern);
	else
	  fprintf(stdout, "available options:\n");

	FOREACH_OPTION(i) {
		msg = get_option_help(i);
		if (msg != NULL) {
		    if (option_matches_language (i, invoked_lang)) {
			name = get_option_name(i);
			/* if pattern specified, only print if pattern found */
			if (help_pattern != NULL
				&& !contains_substring(name, help_pattern)
				&& !contains_substring(msg, help_pattern) )
			{
				continue;	/* to next option */
			}
			fprintf(stdout, "\t%s:  %s\n", name, msg);
		    }
		}
	}
	exit(RC_OKAY);
}

static void
dump_args (string msg)
{
	int i;
	printf("dump args %s: ", msg);
	FOREACH_OPTION_SEEN(i) {
		if (i == O_Unrecognized) continue;
		/* there are some combos that result in a warning
		 * and replacing one of the args; in that case the
		 * option name looks like "arg1 arg2" but the implied
		 * list is just "arg1". */
		if (is_replacement_combo(i)) {
			int iflag;
			FOREACH_IMPLIED_OPTION(iflag, i) {
				printf(" %s", get_current_implied_name());
			}
		} else {
			printf(" %s", get_option_name(i));
		}
	}
	printf("\n");
}


