
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

#include <ctype.h>
#include <malloc.h>
#include <stdlib.h>
#include "string_utils.h"
#include "get_options.h"
#include "options.h"
#include "option_names.h"
#include "errors.h"
#include "file_utils.h"
#include "lang_defs.h"
#include "phases.h"
#include "opt_actions.h"

string option_name;

/*
 * In following code, we are using "arg" to refer to the
 * argv[argi] string, and "option" to refer to a user option.
 * "optarg" is used to refer to the argument of an option,
 * e.g. for "-Dfoo" foo is the argument to -D.
 */

string optargs = NULL;		/* argument to option, visible externally */
int optargd = 0;		/* argument to option, visible externally */
static int optindex = 1;	/* current index into option */

/* are we at last character of arg string? */
#define is_last_char(argv,argi)	(argv[*argi][optindex+1] == '\0')

/* get new "-" arg */
#define get_next_arg(argi)	(*argi)++; optindex = 1;

/* are we at beginning of user arg? */
#define is_new_arg	(optindex == 1)

/* get next character in arg; 
 * for handling single-letter options that combine into one arg */
static void
get_next_char (char **argv, int *argi)
{
	if (is_last_char(argv,argi)) {
		get_next_arg(argi);
	} else {
		optindex++;
	}
}

/* move to end of option with given length */
static void
end_option (char **argv, int *argi, int length)
/*ARGSUSED*/
{
	optindex = length-1;	/* -1 cause we start at 0 */
}

#define current_string(argv,argi)	&argv[*argi][optindex]

/* get argument that follows option */
static string
get_optarg (char **argv, int *argi)
{
	if (is_last_char(argv,argi)) {
		(*argi)++;
		optindex = 0;
	} else {
		optindex++;
	}
	return current_string(argv,argi);
}

static string null_string = "";

/* get next string in option */
static string
next_string (char **argv, int *argi)
{
	if (is_last_char(argv,argi)) {
		if (argv[*argi+1] == NULL)
			return null_string;
		else
			return argv[*argi+1];
	} else {
		return &argv[*argi][optindex+1];
	}
}

/* get next string in option after prefix */
static string
next_string_after (string prefix, char **argv, int *argi)
{
	/* use new_optindex to temporarily move to end of prefix */
	size_t new_optindex = strlen(prefix);
	if (argv[*argi][new_optindex] == '\0') {
		if (argv[*argi+1] == NULL)
			return null_string;
		else
			return argv[*argi+1];
	} else {
		return &argv[*argi][new_optindex];
	}
}

static boolean
is_decimal (string s)
{
	if (isdigit(*s)) return TRUE;
	else return FALSE;
}

/* add arg to prefix for indirect option */
extern int 
add_string_option (int flag, string arg)
{
	/* check that there is an argument after the option,
	 * which should not begin with a dash 
	 * unless option ends in a comma (e.g. -pfa,%s ) */
	string s = get_option_name(flag);
	if (arg == NULL || (*arg == '-' && s[strlen(s)-1] != ',')) {
		parse_error(get_option_name(flag), "no argument given for option");
		return O_xnone;
	}
	return add_derived_option(flag, arg);
}

/* do initial pass through args to check for options that affect driver */
extern void
check_for_driver_controls (int argc, char *argv[])
{
        int i;
	string s;
	for (i = 1; i < argc; i++) {
                if (strncmp(argv[i], "-woff", 5) == 0) {
			s = next_string_after("-woff",argv,&i);
			if (same_string(s, "options")) {
                                print_warnings = FALSE;
			} else if (same_string(s, "all")) {
                                print_warnings = FALSE;
                        }
		} 
		else if (strcmp(argv[i], "-w") == 0) {
                        print_warnings = FALSE;
                } 
		else if (strcmp(argv[i], "-Wall") == 0) {
			fullwarn = TRUE;
		} 
		else if (strcmp(argv[i], "-fullwarn") == 0) {
			fullwarn = TRUE;
		} 
		else if (strcmp(argv[i], "-v") == 0) {
			fullwarn = TRUE;
                } 
		else if (strcmp(argv[i], "-E") == 0) {
			last_phase = P_any_cpp;
                } 
		else if (strcmp(argv[i], "-M") == 0) {
			last_phase = P_any_cpp;
                } 
		else if (strcmp(argv[i], "-MM") == 0) {
			last_phase = P_any_cpp;
                } 
		else if (strcmp(argv[i], "--user-dependencies") == 0) {
			last_phase = P_any_cpp;
                } 
		else if (strcmp(argv[i], "-ignore_suffix") == 0) {
			ignore_suffix = TRUE;
                } 
		else if (strcmp(argv[i], "-i32") == 0) {
			abi = ABI_I32;
                }
		else if (strcmp(argv[i], "-i64") == 0) {
			abi = ABI_I64;
                }
		else if (strcmp(argv[i], "-ia32") == 0) {
			abi = ABI_IA32;
                }
        }
}

/* for -u option: 
 * UGLY KLUDGE:  f77 has -u option with no args, ld has -u option with one arg.
 * Ucode compiler handled this by having -u when invoked by non-fortran be
 * ld option, and Sun seems to also do this.  So we have to do it too, even
 * though it breaks all our assumptions about options having only one meaning.*/
static int
parse_u_option (char **argv, int *argi)
{
	if (invoked_lang == L_f77 || invoked_lang == L_f90) {
		int flag;
       		get_next_arg(argi);
		flag = add_new_option("-u");
		add_phase_for_option(flag, P_f_fe);
		return flag;
	} else {
                optargs = get_optarg(argv, argi);
                get_next_arg(argi);
                return add_string_option(O_u,optargs);
	}
}

/* for -U option:
 * UGLY KLUDGE:  f77 has -U with no args and -U<arg>.
 * In C we allow -U <arg> but for f77 we will return -U if a space after the U.
 */
static int
parse_U_option (char **argv, int *argi)
{
	if ((invoked_lang == L_f77 || invoked_lang == L_f90)
		&& is_last_char(argv,argi)) 
	{
		/* -U */
		int flag;
       		get_next_arg(argi);
		flag = add_new_option("-U");
		add_phase_for_option(flag, P_f_fe);
		return flag;
	} else {
		/* -U<arg> */
                optargs = get_optarg(argv, argi);
                get_next_arg(argi);
                return add_string_option(O_U,optargs);
	}
}


/* need to hand code -R option since the generic code doeesnt like
   the arg string to begin with - */
static int
parse_R_option (char **argv, int *argi)
{
	if ((invoked_lang == L_f77 || invoked_lang == L_f90)
		&& !is_last_char(argv,argi)) 
	{
		/* -R */
		int flag;
		optargs = get_optarg(argv, argi);
       		get_next_arg(argi);
		flag = add_new_option(optargs);
		add_phase_for_option(flag, P_ratfor);
		return flag;
	} else {
		parse_error(option_name, "no argument given for option");
       		get_next_arg(argi);
		return O_Unrecognized;
	}
}

static boolean middle_of_multi_option = FALSE;

/* common routine to end -W by adjusting to correct place */
static void
end_multi_option (char **argv, int *argi, string p)
{
	if (*p == NIL) {
		middle_of_multi_option = FALSE;
        	(void) get_optarg(argv, argi);
        	get_next_arg(argi);
	} else {
		middle_of_multi_option = TRUE;
		optargs = p+1;
	}
}

static int
parse_multi_option (char **argv, int *argi)
{
	int flag;
	string p, q;
	buffer_t buf;
	if (! middle_of_multi_option) {
        	optargs = next_string(argv,argi);
		for (p = optargs; *p != NIL && *p != ','; p++)
			;
		if (*p == NIL) {
			parse_error(option_name, "bad syntax for option");
			end_multi_option(argv, argi, p);
			return O_Unrecognized;
		}
		p++;	/* skip past comma */
	} else {
		p = optargs;
	}
	if (*p == NIL) {
		/* no args */
		parse_error(option_name, "bad syntax for option");
		end_multi_option(argv, argi, p);
		return O_Unrecognized;
	}
	/* args separated by comma are separate args, unless \, */
	q = buf;
	while (*p != NIL && *p != ',') {
		if (*p == '\\' && *(p+1) == ',') {
			*q = ',';
			p++;
		} else {
			*q = *p;
		}
		p++;
		q++;
	}
	*q = NIL;
	flag = add_new_option(buf);
	end_multi_option(argv, argi, p);
	return flag;
}


int O_W_was_seen = 0;
extern string_list_t *ld_args_objs;

/* for -W option: */
/* add option for particular phase(s), e.g. -Wb,-foo becomes -foo option */
static int
parse_W_option (char **argv, int *argi)
{
	int flag;
	phases_t phase;
	string start = next_string(argv,argi);
	if ( ! contains_substring(start, ",") || *start == '-') {
		get_next_char(argv,argi);
		return O_W;
	}
	flag = parse_multi_option (argv, argi);
	/* set phases for whatever user said */
	for (; *start != NIL && *start != ','; start++) {
		phase = get_phase(*start);
		if (phase == P_NONE) {
			parse_error(option_name, "bad phase for -W option");
		}
		if (phase == P_ld || phase == P_ldplus || phase == P_any_ld){
			add_string(ld_args_objs, get_option_name(flag));
		} else {
		  add_phase_for_option(flag, phase);
		}
	}

	/* warn about -WK being obsolete */
	if (auto_parallelize && phase == P_any_optfe) {
		warning("-WK,<options> is ignored with new -pfa/-pca");
	}

	/* Force treatment as internal so that -Wb, gets prefixed
	 * to IPL -cmds list entry (PV 375260):
	 */
	set_internal_option ( flag );
	O_W_was_seen = 1;
	return flag;
}


#include "get_option.i"


/* this will only works for options that do not use arguments */
int get_option_from_name(char * name)
{
  int i = 0;
  int ret_val;
  char * real_name = alloca(strlen(name) + 2);
  real_name[0] = '-';
  real_name[1] = 0;
  strcat(real_name, name);
  
  ret_val =  get_option(&i, &real_name);
  if (ret_val > LAST_PREDEFINED_OPTION)
    return -1;
  return ret_val;
}
