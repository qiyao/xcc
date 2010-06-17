
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


#include <alloca.h>
#include <stdlib.h>
#include "config_platform.h"
#include "opt_actions.h"
#include "options.h"
#include "option_names.h"
#include "option_seen.h"
#include "lang_defs.h"
#include "errors.h"
#include "file_utils.h"
#include "file_names.h"
#include "string_utils.h"
#include "get_options.h"
#include "objects.h"
#include "phases.h"
#include "run.h"
#include "xtensa-params.h"


/* keep list of previous toggled option names, to give better messages */
typedef struct toggle_name_struct {
	int *address;
	string name;
} toggle_name;
#define MAX_TOGGLES	50
static toggle_name toggled_names[MAX_TOGGLES];
static int last_toggle_index = 0;
static int inline_on_seen = FALSE;
int inline_t = UNDEFINED;
boolean skip_inliner = FALSE;
boolean dashdash_flag = FALSE;
boolean read_stdin = FALSE;
boolean xpg_flag = FALSE;
int default_olevel = UNDEFINED;
static int default_proc = UNDEFINED;
int instrumentation_invoked = UNDEFINED;
boolean reorder_tie = UNDEFINED;
boolean flush_tie = UNDEFINED;

extern void xt_handle_lsp_option(void);
extern void xt_add_linker_options();
int timernumber = 0;


static void
add_toggle_name (int *obj, string name)
{
	int i;
	for (i = 0; i < last_toggle_index; i++) {
		if (obj == toggled_names[i].address) {
			break;
		}
	}
	if (i == last_toggle_index) {
		if (last_toggle_index >= MAX_TOGGLES) {
			internal_error("too many toggle names\n");
		} else {
			last_toggle_index++;
		}
	}
	toggled_names[i].address = obj;
	toggled_names[i].name = string_copy(option_name);
}

static string
get_toggle_name (int *obj)
{
	int i;
	for (i = 0; i < last_toggle_index; i++) {
		if (obj == toggled_names[i].address) {
			return toggled_names[i].name;
		}
	}
	internal_error("no previously toggled name?");
	return "<unknown>";
}

/* return whether has been toggled yet */
extern boolean
is_toggled (int obj)
{
	return (obj != UNDEFINED);
}

/* set obj to value; allow many toggles; last toggle is final value */
extern void
toggle (int *obj, int value)
{
	if (*obj != UNDEFINED && *obj != value) {
		warning ("%s conflicts with %s; using latter value (%s)", 
			get_toggle_name(obj), option_name, option_name);
	}
	*obj = value;
	add_toggle_name(obj, option_name);
}

extern void
process_tie_port(int *obj, int value)
{
	toggle(obj, value);
	if (reorder_tie!=UNDEFINED && flush_tie!=UNDEFINED) {
	  if (flush_tie) {
	    if (reorder_tie) {
	      if (obj== &flush_tie)
	        warning (
		  "-mflush-tieport conflicts with -mreorder-tieport;"
		  " assuming -mflush-tieport"); 
	      else
	        warning (
		  "-mreorder-tieport conflicts with -mflush-tieport;"
		  " assuming -mflush-tieport"); 
	    }
	  }
	}
}


/* ====================================================================
 *
 * Get_Group_Option_Value
 *
 * Given a group option string, search for the option with the given
 * name.  Return NULL if not found, the option value if found ("" if
 * value is empty).
 *
 * ====================================================================
 */

static char *
Get_Group_Option_Value (
  char *arg,	/* Raw option string */
  char *name,	/* Suboption full name */
  char *abbrev)	/* Suboption abbreviation */
{
  char *endc = arg;
  int n;

  while ( TRUE ) {
    n = strcspn ( arg, ":=" );
    if ( strncasecmp ( arg, abbrev, strlen(abbrev) ) == 0
      && strncasecmp ( arg, name, n ) == 0 )
    {
      endc += n;
      if ( *endc == '=' ) {
	/* Duplicate value lazily: */
	char *result = strdup ( endc+1 );

	* ( result + strcspn ( result, ":=" ) ) = 0;
	return result;
      } else {
	/* No value: */
	return "";
      }
    }
    if ( ( endc = strchr ( arg, ':' ) ) == NULL ) return NULL;
    arg = ++endc;
  }

  /* Shouldn't get here, but ... */
  /* return NULL;  compiler gets better */
}

/* ====================================================================
 *
 * Bool_Group_Value
 *
 * Given a group option value string for a Boolean group value,
 * determine whether it is TRUE or FALSE.
 *
 * ====================================================================
 */

static boolean
Bool_Group_Value ( char *val )
{
  if ( *val == 0 ) {
    /* Empty string is TRUE for group options */
    return TRUE;
  }

  if ( strcasecmp ( val, "OFF" ) == 0
    || strcasecmp ( val, "NO" ) == 0
    || strcasecmp ( val, "FALSE" ) == 0
    || strcasecmp ( val, "0" ) == 0 )
  {
    return FALSE;
  } else {
    return TRUE;
  }
}

/* ====================================================================
 *
 * Process_Opt_Group
 *
 * We've found a -OPT option group.  Inspect it for -OPT:reorg_common
 * options, and set -split_common and -ivpad accordingly.
 *
 * NOTE: We ignore anything that doesn't match what's expected --
 * the compiler will produce reasonable error messages for junk.
 *
 * ====================================================================
 */

void
Process_Opt_Group ( string opt_args )
{
  char *reorg = NULL;

  if ( debug ) {
    fprintf ( stderr, "Process_Opt_Group: %s\n", opt_args );
  }

  
  /* Go look for -OPT:instrument */
  reorg = Get_Group_Option_Value ( opt_args, "instrument", "instr");

  if (reorg != NULL) {
     instrumentation_invoked = TRUE;
  }

  /* Go look for -OPT:reorg_common: */
  reorg = Get_Group_Option_Value ( opt_args, "reorg_common", "reorg");

}

void
Process_Default_Group (string default_args)
{
  string s;
  int i;

  if ( debug ) {
    fprintf ( stderr, "Process_Default_Group: %s\n", default_args );
  }

  /* Go look for -DEFAULT:opt=[0-3]: */
  s = Get_Group_Option_Value ( default_args, "opt", "opt");
  if (s != NULL) {
	default_olevel = atoi(s);
  }
  /* Go look for -DEFAULT:arith=[0-3]: */
  s = Get_Group_Option_Value ( default_args, "arith", "arith");
  if (s != NULL) {
	i = add_string_option (O_OPT_, concat_strings("IEEE_arith=", s));
	add_option_seen (i);
  }
}

/* ====================================================================
 *
 * Routines to manage the target selection (ABI, ISA, and processor).
 *
 * Make sure that the driver picks up a consistent view of the target
 * selected, based either on user options or on defaults.
 *
 * ====================================================================
 */

/* ====================================================================
 *
 * Process_Targ_Group
 *
 * We've found a -TARG option group.  Inspect it for ABI, ISA, and/or
 * processor specification, and toggle the state appropriately.
 *
 * NOTE: We ignore anything that doesn't match what's expected --
 * the compiler will produce reasonable error messages for junk.
 *
 * ====================================================================
 */

void
Process_Targ_Group ( string targ_args )
{
  char *cp = targ_args;	/* Skip -TARG: */
  char *cpeq;

  if ( debug ) {
    fprintf ( stderr, "Process_Targ_Group: %s\n", targ_args );
  }

  while ( *cp != 0 ) {
    switch ( *cp ) {
      case 'a':
	if ( strncasecmp ( cp, "abi", 3 ) == 0 && *(cp+3) == '=' ) {
#ifdef TARG_MIPS
	  if ( strncasecmp ( cp+4, "n32", 3 ) == 0 ) {
	    add_option_seen ( O_n32 );
	    toggle ( &abi, ABI_N32 );
	  } else if ( strncasecmp ( cp+4, "64", 2 ) == 0 ) {
	    add_option_seen ( O_64 );
	    toggle ( &abi, ABI_64 );
	  }
#endif
	}
	break;

      case 'i':
	/* Fall through */

      case 'm':
	break;

      case 'p':
	break;
    }

    /* Skip to the next group option: */
    while ( *cp != 0 && *cp != ':' ) ++cp;
    if ( *cp == ':' ) ++cp;
  }
}

/* ====================================================================
 *
 * Check_Target
 *
 * Verify that the target selection is consistent and set defaults.
 *
 * ====================================================================
 */

void
Check_Target ( void )
{
  int opt_id;
  int opt_val;

  if (!is_toggled(abi)) {
    xtensa_values * xt_val;
    if (((xt_val = xtensa_find_key(xtensa_default_params, "SW_ABI")) != NULL)
	&& (xt_val->num_values != 0)) {
      const char * val = xtensa_string_value(xt_val);
      if (strcmp(val, "windowed") == 0) {
	toggle(&abi, ABI_WINDOWED);
	prepend_option_seen(O_mabiQwindowed);
      }
      else if (strcmp(val, "call0") == 0) {
	toggle(&abi, ABI_CALL0);
	prepend_option_seen(O_mabiQcall0);
      }
      else {
	toggle(&abi, ABI_WINDOWED);
	prepend_option_seen(O_mabiQwindowed);
      }
    }
    else {
      toggle(&abi, ABI_WINDOWED);
      prepend_option_seen(O_mabiQwindowed);
    }
  }
}

/* ====================================================================
 *
 * Routines to manage inlining choices (the -INLINE group and friends).
 *
 * ====================================================================
 */

/* toggle inline for a normal option (not "=on" or "=off") */

static void
toggle_inline_normal(void)
{
  if (inline_t == UNDEFINED)
    inline_t = TRUE;
}

/* toggle inline for "=on" */

static void
toggle_inline_on(void)
{
  if (inline_t == FALSE) {
    warning ("-noinline or -INLINE:=off has been seen, %s ignored",
	     option_name);
  }
  else {

    inline_t = TRUE;
    inline_on_seen = TRUE;
  }
}

/* toggle inline for "=off" */

static void
toggle_inline_off(void)
{
  if (inline_on_seen == TRUE) {
    warning ("Earlier request for inline processing has been overridden by %s",
	     option_name);
  }
  inline_t = FALSE;
}

/* process -INLINE option */
void
Process_Inline ( void )
{
  int more_symbols = TRUE;
  char *args = option_name+7;

  if (strncmp (option_name, "-fno-inline", 11) == 0 ||
      strncmp (option_name, "-noinline", 9) == 0)
    toggle_inline_off();
  else if (strncmp (option_name, "-finline-functions", 18) == 0)
    toggle_inline_on();
  else if (*args == '\0')
    /* Treat "-INLINE" like "-INLINE:=on" for error messages */
    toggle_inline_on();
  else do {
    char *endc;
    *args = ':';
    if ((endc = strchr(++args, ':')) == NULL)
      more_symbols = FALSE;
    else
      *endc = '\0';
    if (strcasecmp(args, "=off") == 0) {
      toggle_inline_off();
      skip_inliner = TRUE;
    }
    else if (strcasecmp(args, "=on") == 0)
      toggle_inline_on();
    else if (strcmp(args, "list") != 0)
      toggle_inline_normal();
    args = endc;
  }
  while (more_symbols);
}

/*
 * Processing -F option: ratfor-related stuff for Fortran, but
 * (obsolete) C code generation option in C++ and unknown for C.
 */
void dash_F_option(void)
{
    if (invoked_lang == L_f77) {
	last_phase=earliest_phase(P_ratfor,last_phase);
    } else if (invoked_lang == L_CC) {
	error("-F is not supported: cannot generate intermediate C code");
    } else {
	parse_error("-F", "unknown flag");
    }
}

/* untoggle the object, so it can be re-toggled later */
extern void
untoggle (int *obj, int value)
/*ARGSUSED*/
{
  *obj = UNDEFINED;
}

/* change path for particular phase(s), e.g. -Yb,/usr */
static void
change_phase_path (string arg)
{
	string dir;
	string s;
	for (s = arg; s != NULL && *s != NIL && *s != ','; s++)
		;
	if (s == NULL || *s == NIL) {
		parse_error(option_name, "bad syntax for -Y option");
		return;
	}
	dir = s+1;
	if (dir[0] == '~' && (dir[1] == '/' || dir[1] == '\0')) {
	    char *home = getenv("HOME");
	    if (home)
		dir = concat_strings(home, dir+1);
	}
	if (!is_directory(dir))
		parse_error(option_name, "not a directory");
	for (s = arg; *s != ','; s++) {
		/* do separate check so can give better error message */
		if (get_phase(*s) == P_NONE) {
			parse_error(option_name, "bad phase for -Y option");
		} else {
			set_phase_dir(get_phase_mask(get_phase(*s)), dir);
		}
	}
}

/* halt after a particular phase, e.g. -Hb */
/* but also process -H and warn its ignored */
static void
change_last_phase (string s)
{
	phases_t phase;
	if (s == NULL || *s == NIL) {
		warn_ignored("-H");
	} else if ( *(s+1)!=NIL) {
		parse_error(option_name, "bad syntax for -H option");
	} else if ((phase=get_phase(*s)) == P_NONE) {
			parse_error(option_name, "bad phase for -H option");
	} else {
			last_phase=earliest_phase(phase, last_phase);
	}
}

extern void
save_name (string *obj, string value)
{
	char * pointer_so_macro_works;
	*obj = (char *) malloc(strlen(value) + 1);
	pointer_so_macro_works = *obj;
	FIXPATH(value, pointer_so_macro_works);
}

static void
check_output_name (string name)
{
  string suf = get_suffix(name);
  if (name == NULL) return;
  if (get_source_kind_from_suffix(suf) != S_o &&
      file_exists(name)) {
    if (strcmp(option_name, name)) {
      warning("%s %s will overwrite a file that has a source-file suffix", option_name, name);
    }
    else {
      warning("%s will overwrite a file that has a source-file suffix", option_name);
    }
  }
}

void
check_dashdash (void)
{
	if(xpg_flag)
	   dashdash_flag = 1;
	else
	   error("%s not allowed in non XPG4 environment", option_name);
}

static string
Get_Binary_Name ( string name)
{
  string new;
  int len, i;
  new = string_copy(name);
  len = strlen(new);
  for ( i=0; i<len; i++ ) {
    if (strncmp(&new[i], ".x.Counts", 9) == 0) {
      new[i] = 0;
      break;
    }
  }
  return new;
}
 
void
Process_fb_create ( char *fname, int counter_size )
{
   int flag;
   char *p;
   fb_file = string_copy(fname);
   /* strip trailing slashes */
   p = fb_file + strlen(fb_file);
   while (p != fb_file && is_dir_separator(*--p)) *p = '\0';
   toggle ( &instrumentation_invoked, TRUE );
   instrumentation_invoked = TRUE;
   flag = add_string_option(O_OPT_, "instr=0");
   add_option_seen (flag);
   if (counter_size == 32)
     flag = add_string_option (O_OPT_, "instrument_bits=32");
   else if (counter_size == 64)
     flag = add_string_option (O_OPT_, "instrument_bits=64");
   else if (counter_size == 96) {
     flag = add_string_option (O_OPT_, "instrument_bits=64");
   }
   add_option_seen (flag);
   flag = add_string_option (O_OPT_, "instr_unique_output=on");
   add_option_seen (flag);
}

void
Process_fb_entry ( char *fentry )
{
   fb_entry = string_copy(fentry);
}

void
Process_fb_opt ( char *fname, boolean strict )
{
   char *p;
   if (fb_file) {
     string tmp = concat_strings("-ff,",fname);
     /* strip trailing slashes */
     p = tmp + strlen(tmp);
     while (p != tmp && is_dir_separator(*--p)) *p = '\0';
     tmp = concat_strings(tmp,".instr ");
     fb_file = concat_strings(fb_file , tmp);
   } else {
     fb_file = concat_strings("-ff,",fname);
     /* strip trailing slashes */
     p = fb_file + strlen(fb_file);
     while (p != fb_file && is_dir_separator(*--p)) *p = '\0';
     fb_file = concat_strings(fb_file,".instr ");
   }

   toggle ( &instrumentation_invoked, FALSE);
   if (strict) {
     int flag = add_string_option (O_OPT_, "fb_opt_strict");
     add_option_seen (flag);
   }
}

void
Process_prof_opt ( char *fname )
{
   char *p;
   prof_file = string_copy(fname);
}

void
Process_fbexe ( char *fname )
{
  prof_file = string_copy(fname);
}

void
Process_fb_xdir ( char *fname )
{
  fb_xdir = string_copy(fname);
}

void
Process_fb_cdir ( char *fname )
{
  fb_cdir =  string_copy(fname);
}


/* ====================================================================
 *
 * Process_Mp_Group
 *
 * We've found a -MP option group.  Inspect it for dsm request
 * and toggle the state appropriately.
 *
 * NOTE: We ignore anything that doesn't match what's expected --
 * the compiler will produce reasonable error messages for junk.
 *
 * ====================================================================
 */

void
Process_Mp_Group ( string mp_args )
{
  char *cp = mp_args;	/* Skip -MP: */

  if ( debug ) {
    fprintf ( stderr, "Process_Mp_Group: %s\n", mp_args );
  }

}

void
Process_Mp ( void )
{

}

void Process_Cray_Mp (void) {

  if (invoked_lang == L_f90) {
    /* this part is now empty (we do the processing differently)
     * but left as a placeholder and error-checker.
     */
  }
  else error ("-cray_mp applicable only to f90");
}


extern string_list_t * library_dirs;

boolean print_path_if_exists(string path, string fname)
{
  /* xt- handles cases like --print-prog-name=ld, which should produce
     something like ../swtools/xt-ld
  */
  string xtfname = concat_strings("xt-", fname);
  string dir_with_trailing_slash = 
    concat_strings(path, 
		   is_dir_separator(path[strlen(path) - 1]) ? "" : "/");
  string full_path = concat_strings(dir_with_trailing_slash, fname);
  string xt_full_path = concat_strings(dir_with_trailing_slash, xtfname);
  
  if (file_exists(full_path)) {
    printf("%s\n", full_path);
    return TRUE;
  }
  if (file_exists(xt_full_path)) {
    printf("%s\n", xt_full_path);
    return TRUE;
  }
#if _WIN32
  /* handle .exe extensions automatically on windows */
  xt_full_path = concat_strings(xt_full_path, ".exe");
  if (file_exists(xt_full_path)) {
    printf("%s\n", xt_full_path);
    return TRUE;
  }
#endif  
  return FALSE;
}
  

/* This function exists to support the mplinker */

void
print_lsp (void)
{
  string_item_t *p;
  string path;

  xt_handle_lsp_option();
  xtensa_add_linker_options();

  printf("*start_files\n");
  for (p = start_link_args->head; p != NULL; p = p->next) {
    printf("%s\n", p->name);
  }
  printf("*libraries\n");
  for (p = lib_link_args->head; p != NULL; p = p->next) {
    printf("%s\n", p->name);
  }
  printf("*end_files\n");
  for (p = end_link_args->head; p != NULL; p = p->next) {
    printf("%s\n", p->name);
  }
  printf("*library_dirs\n");
  for (p = library_dirs->head; p != NULL; p = p->next) {
    printf("%s\n", p->name);
  }
}


void
print_file_path (string fname)
{
  /* Search for fname in usual places, and print path when found. */
  /* gcc does separate searches for libraries and programs,
   * but that seems redundant as the paths are nearly identical,
   * so try combining into one search. */

  string_item_t *p;
  string path;

  xt_handle_lsp_option();
  xtensa_add_linker_options();
  for (p = library_dirs->head; p != NULL; p = p->next) {
    if (print_path_if_exists(p->name, fname)) 
      return;
 }

  path = get_phase_dir(P_be);
  if (print_path_if_exists(path, fname)) {
    return;
  }
  path = get_phase_dir(P_library);
  if (print_path_if_exists(path, fname)) {
    return;
  }
  path = get_phase_dir(P_gcpp);
  if (print_path_if_exists(path, fname)) {
    return;
  }
  path = get_phase_dir(P_gas);
  if (print_path_if_exists(path, fname)) {
    return;
  }

  path = get_phase_dir(P_alt_library);
  if (print_path_if_exists(path, fname)) {
    return;
  }
  /* not found, so just print fname */
  printf("%s\n", fname);
}

void
print_multi_lib (void)
{
  /* TODO: if we ever want multiple versions of newlib, this will 
     have to be more intelligent
  */
  printf(".;\n");
}


/* only prints installation dir at this point */
void 
print_search_dirs (void)
{
  char * config_path;
  char * fixed_config_path;
  char * system_path;
  char * fixed_system_path;
  /* find config-independent install directory */
  system_path = xtensa_path_value
    (xtensa_find_key (xtensa_default_params, "install-prefix"));
  if (!system_path) {
    fprintf (stderr, "Error: missing install-prefix in parameter file\n");
    exit (-1);
  }
  fixed_system_path = (char *) alloca(strlen(system_path) + 1);
  FIXPATH(system_path, fixed_system_path);
  free(system_path);

  /* find config-dependent install directory */
  config_path = xtensa_path_value
    (xtensa_find_key (xtensa_default_params, "config-prefix"));
  if (!config_path) {
    fprintf (stderr, "Error: missing config-prefix in parameter file\n");
    exit (-1);
  }
  fixed_config_path = (char *)alloca(strlen(config_path) + 1);
  FIXPATH(config_path, fixed_config_path);
  free(config_path);

  printf("install: %s%s/\n", fixed_system_path, get_phase_dir(P_library));
  printf("programs: %s/\n", fixed_system_path, get_phase_dir(P_be));
}


#include "opt_action.i"

