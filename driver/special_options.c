
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


/*
 * OPTIONS that are not simple enough to handle in the table
 * are done by hand in these routines.
 */
#include <stdlib.h>
#include <stamp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/fcntl.h> //for O_rdonly under solaris

#include "string_utils.h"
#include "objects.h"
#include "options.h"
#include "option_seen.h"
#include "option_names.h"
#include "lang_defs.h"
#include "errors.h"
#include "opt_actions.h"
#include "file_names.h"
#include "file_utils.h"
#include "get_options.h"
#include "phases.h"
extern int timernumber;

static void xt_set_default_switch(int toggleSwitch, char * key, int onOption, int offOption);
static void xt_set_default_option(int toggleSwitch, char * key, int option, int isPath);
static void xt_read_linker_specs(void);
static char * skip_whitespace (char * p);
void xt_handle_lsp_option(void);
static int xt_add_object_file(char * fullName, string_list_t * list);
static void handle_specs(void);


#include "xtensa-params.h"

string xtensa_tools;
string lsp_name;
string input_charset;


/* Concatenate an array of strings separated by the specified character.  */
static char *
join (char sep, char **strings)
{
  int n, len = 0;
  char *result;
  for (n = 0; strings[n]; n++)
    len += strlen (strings[n]) + 1;

  result = (char *) malloc (len + 1);

  len = 0;
  for (n = 0; strings[n]; n++)
    {
      int nlen = strlen (strings[n]);
      strcpy (result + len, strings[n]);
      result[len + nlen] = sep;
      len += (nlen + 1);
    }

  result[len - 1] = '\0';
  return result;
}

extern void
set_defaults (void)
{
  int flag;
  char ** xt_isa_dlls;
  char * xt_isa_dlls_string;
  char ** xt_xtie_dlls;
  char * xt_xtie_dlls_string;
  xtensa_params xp = xtensa_default_params;

  /* by default char is unsigned */
  if (!is_toggled(signed_char))
    toggle(&signed_char, FALSE);

  {
    char buf[16];
    sprintf(buf, "%d", 0);
    flag = add_string_option(O_G__, buf);
    prepend_option_seen(flag);
  }

  xt_set_default_switch (bigendian, "IsaIsBigEndian",  O_mbig_endian, O_mlittle_endian);
  xt_set_default_switch (density, "IsaUseDensityInstruction", O_mdensity, O_mno_density);
  xt_set_default_switch (mac16, "IsaUseMAC16", O_mmac16, O_mno_mac16);
  xt_set_default_switch (mul16, "IsaUseMul16", O_mmul16, O_mno_mul16);
  xt_set_default_switch (mul32, "IsaUse32bitMul", O_mmul32, O_mno_mul32);
  xt_set_default_switch (mul32h, "IsaUse32bitMulh", O_mmul32h, O_mno_mul32h);
  xt_set_default_switch (div32, "IsaUse32bitDiv", O_mdiv32, O_mno_div32);
  xt_set_default_switch (clamps, "IsaUseClamps", O_mclamps, O_mno_clamps);
  xt_set_default_switch (minmax, "IsaUseMinMax", O_mminmax, O_mno_minmax);
  xt_set_default_switch (nsa, "IsaUseNormShiftAmount", O_mnsa, O_mno_nsa);
  xt_set_default_switch (sext, "IsaUseSignExtend", O_msext, O_mno_sext);
  xt_set_default_switch (booleans, "IsaUseBooleans", O_mbooleans, O_mno_booleans);
  xt_set_default_switch (hifi2, "IsaUseHiFi2", O_mhifi2, O_mno_hifi2);
  xt_set_default_switch (vectorfpu2005, "IsaUseVectorFPU2005", O_mvectorfpu2005, O_mno_vectorfpu2005);
  xt_set_default_switch (hard_float, "IsaUseFloatingPoint", O_mhard_float, O_msoft_float);
  xt_set_default_switch (hard_float_div, "IsaUseFloatingPointDiv", O_mhard_float_div, O_mno_hard_float_div);
  xt_set_default_switch (hard_float_recip, "IsaUseFloatingPointRecip", O_mhard_float_recip, O_mno_hard_float_recip);
  xt_set_default_switch (hard_float_sqrt, "IsaUseFloatingPointSqrt", O_mhard_float_sqrt, O_mno_hard_float_sqrt);
  xt_set_default_switch (hard_float_rsqrt, "IsaUseFloatingPointRSqrt", O_mhard_float_rsqrt, O_mno_hard_float_rsqrt);
  xt_set_default_switch (zero_cost_loop, "IsaUseLoops", O_mzero_cost_loop, O_mno_zero_cost_loop);
  xt_set_default_switch (const16, "IsaUseConst16", O_mconst16, O_mno_const16);
  xt_set_default_switch (l32r, "IsaUseL32R", O_ml32r, O_mno_l32r);
  xt_set_default_switch (abs_ins, "IsaUseAbs", O_mabs, O_mno_abs);
  xt_set_default_switch (brt, "IsaUsePredictedBranches", O_mpredicted_branches, O_mno_predicted_branches);
  xt_set_default_switch (addx, "IsaUseAddx", O_maddx, O_mno_addx);
  xt_set_default_switch (unaligned_stores, "IsaSysHandlesMisAlignedStore", O_munaligned_stores, O_mno_unaligned_stores);
  xt_set_default_switch (unaligned_loads, "IsaSysHandlesMisAlignedLoad", O_munaligned_loads, O_mno_unaligned_loads);

  {
    /* If -fpic or -fno-pic was not specified, set the default based on
     * alwaysPIC parameter.  */
    xtensa_values * xt_val;
    if (((xt_val = xtensa_find_key(xp, "alwaysPIC")) != NULL)
	&& (xt_val->num_values != 0)
	&& !is_toggled(pic_code)) {
      prepend_option_seen(xtensa_int_value(xt_val) ? O_fpic : O_none);
    }
  }
  xt_set_default_option (icache_line_bytes, "InstCacheLineBytes", O_micache_line_bytesQ__, 0);
  xt_set_default_option (dcache_line_bytes, "DataCacheLineBytes", O_mdcache_line_bytesQ__, 0);
  xt_set_default_option (dcache_bytes, "DataCacheBytes", O_mdcache_bytesQ__, 0);

  xt_set_default_option (UNDEFINED, "InstFetchWidth", O_mfetchwidthQ__, 0);
  xt_set_default_option (UNDEFINED, "ISSPipeBStage", O_mbstageQ__, 0);
  xt_set_default_option (UNDEFINED, "ISSPipeEStage", O_mestageQ__, 0);
  xt_set_default_option (UNDEFINED, "ISSPipeMStage", O_mmstageQ__, 0);
  xt_set_default_option (UNDEFINED, "ISSPipeWStage", O_mwstageQ__, 0);
  xt_set_default_option (UNDEFINED, "InstCIFCycles", O_milatencyQ__, 0);

  xt_set_default_option (earliest_arch, "HWMicroArchEarliest", O_mearliest_archQ__, 0);

  xt_set_default_option (latest_arch, "HWMicroArchLatest", O_mlatest_archQ__, 0);

  /* Check that we have a matching number of libisa and libtie
     DLLs. We may get a libisa DLL with no corresponding libtie DLL
     when the user supplies a BearValley or older TDK. */
  xtensa_values *isa_tie_dll = xtensa_find_key (xp, "isa-tie-dll");
  xtensa_values *xml_tie_dll = xtensa_find_key (xp, "xml-tie-dll");
  if (((isa_tie_dll == NULL) != (xml_tie_dll == NULL)) ||
      ((isa_tie_dll != NULL) && (xml_tie_dll != NULL) &&
       (isa_tie_dll->num_values != xml_tie_dll->num_values))) {
    error("The version of the TDK does not match the current set of\n"
	  "               "
	  "software tools.  You may need to regenerate the TDK using\n"
	  "               "
	  "the matching version of the TIE compiler.");
  }

  xt_isa_dlls = xtensa_path_array (xtensa_find_key (xp, "isa-base-dlls"), isa_tie_dll, 0);
  if (!xt_isa_dlls) {
    error (xtensa_params_error_msg ());
  } else {
    xt_isa_dlls_string = join (',', xt_isa_dlls);
    xtensa_free_filelist (xt_isa_dlls);
    prepend_option_seen (add_string_option (O_misa_dllsQ, xt_isa_dlls_string));
  }

  xt_xtie_dlls = xtensa_path_array (xtensa_find_key (xp, "xml-base-dll"), xml_tie_dll, 0);
  if (!xt_xtie_dlls) {
    error (xtensa_params_error_msg ());
  } else {
    xt_xtie_dlls_string = join (',', xt_xtie_dlls);
    xtensa_free_filelist (xt_xtie_dlls);
    prepend_option_seen (add_string_option (O_mxtie_dllsQ, xt_xtie_dlls_string));
  }

  xt_handle_lsp_option();
  xtensa_add_linker_options();
  if( xtensa_tools == NULL )
    xtensa_tools = xtensa_path_value (xtensa_find_key (xp, "xtensa-tools"));

  if (xtensa_tools) {
    /* Export XTENSA_TOOLS environment variable, which ISS requires to
       locate the tenlp DLL for FlexLM licensing. */
    char *new_xttools = (char *)malloc (strlen ("XTENSA_TOOLS=")
                                        + strlen (xtensa_tools) + 1);
    strcpy (new_xttools, "XTENSA_TOOLS=");
    strcat (new_xttools, xtensa_tools);
    putenv (new_xttools);
  }

  /* Seiko/Epson special for SJIS support (pr10992): Tensilica's current
     version of CPP has been modified to handle a limited set of Japanese
     character sets in comments only.  This feature is controlled by the
     "CLANG" environment variable (instead of the standard "LANG") and the
     names of the character sets have "C-" prefixes.  The XCC driver
     accepts the "-finput-charset=<name>" option used by recent versions of
     CPP in the hope that this code can be removed when our CPP is
     upgraded. */
  if (input_charset) {
    if (!strcmp (input_charset, "SJIS") ||
	!strcmp (input_charset, "EUCJP") ||
	!strcmp (input_charset, "JIS")) {
      char *clang = (char *) malloc (sizeof ("CLANG=C-EUCJP"));
      strcpy (clang, "CLANG=C-");
      strcat (clang, input_charset);
      putenv (clang);
    } else {
      error("input character set '%s' is not recognized", input_charset);
    }
  }
}

static int
get_olevel_flag (int olevel)
{
	switch (olevel) {
	case 0: return O_O0;
	case 1: return O_O1;
	case 2: return O_O2;
	case 3: return O_O3;
	default: return O_Unrecognized;
	}
}

/* replace -O* with O0 */
static void
turn_down_opt_level (int new_olevel, string msg)
{
	int flag;
	int new_flag;
	if (fullwarn) warning(msg);
	flag = get_olevel_flag(olevel);
	new_flag = get_olevel_flag(new_olevel);
	if (option_was_seen(O_O))
		replace_option_seen (O_O, new_flag);
	else if (option_was_seen(flag))
		replace_option_seen (flag, new_flag);
	else
		internal_error("driver didn't find -O flag");
	olevel = new_olevel;
}

static void
turn_off_ipa (string msg)
{
	int flag;
	warning (msg);
	ipa = FALSE;
	/* remove all ipa flags from option_seen list */
	FOREACH_OPTION_SEEN(flag) {
		if (flag == O_ipa)
			set_option_unseen(flag);
		else if (flag == O_IPA)
			set_option_unseen(flag);
		else if (is_derived_option (flag) && 
			 get_derived_parent(flag) == O_IPA_)
			set_option_unseen(flag);
	}
}

extern void
add_special_options (void)
{
	int flag;
	buffer_t buf;
	string s;

        /* Hack for gfecc.  The OPT group is valid for C++ compilations, and
           it's valid for most front ends, but it's not valid for gfecc. */
        remove_phase_for_option(O_OPT_, P_cplus_gfe);


        if (auto_parallelize && ipa) {
                flag = add_new_option("-IPA:array_summary");
                add_phase_for_option(flag, P_ipl);
                prepend_option_seen (flag);
        }

	if (olevel == UNDEFINED) {
		olevel = default_olevel;
		if (olevel == UNDEFINED) {
			/* if no default, use -O0 */
			olevel = 0;
		}
		flag = get_olevel_flag(olevel);
		prepend_option_seen (flag);
	}

	// Turn inlining on with -O2 or higher
	if (olevel >= 2 && inline_t == UNDEFINED) {
		inline_t = TRUE;
	}
	// Turn inlining off with -ipa -fb_create
	if (ipa == TRUE && instrumentation_invoked == TRUE) {
		inline_t = FALSE;
	}

        if (ipa == TRUE && inline_t == FALSE) {
                flag = add_new_option("-IPA:inline=off");
                add_phase_for_option(flag, P_ipa_link);
                prepend_option_seen(flag);
        }
	  
	// Turn inlining off with -O0
	if (olevel == 0 && inline_t == UNDEFINED) {
		inline_t = FALSE;
	}

	{
		sprintf(buf, "%d", 8);
		flag = add_string_option(O_G__, buf);
		prepend_option_seen(flag);
	}

	/* some checks are easier to do by hand */
	if (olevel >= 2 && glevel == 2) {
		glevel = 3;
		if (option_was_seen (O_g)) replace_option_seen (O_g, O_g3);
		if (option_was_seen (O_g2)) replace_option_seen (O_g2, O_g3);
	}

	if (option_was_seen(O_S) && ipa == TRUE) {
		turn_off_ipa ("-IPA -S combination not allowed, replaced with -S");
	}
	if (option_was_seen(O__assemble) && ipa == TRUE) {
		turn_off_ipa ("-IPA --assemble combination not allowed, replaced with --assemble");
	}
	if (option_was_seen(O__rename_section) && ipa == TRUE) {
		warning ("-mrename-section not supported with -IPA.\n"
                         "                 "
                         "Use __attribute__((section(\"name\"))) instead");
	}
        if (option_was_seen(O_S) && option_was_seen(O_xpres)) {
          error ("-xpres -S combination not allowed");
        }
        if (option_was_seen(O_S) && option_was_seen(O_xpres2)) {
          error ("-xpres2 -S combination not allowed");
        }
	if (option_was_seen(O_xpres2) && ipa == TRUE) {
	  turn_off_ipa ("-IPA -xpres2 combination not allowed, replaced with -xpres2");
        }

	if (ipa == TRUE && shared == RELOCATABLE) {
          error ("-ipa -r combination not allowed");
	}

	if (ipa == TRUE) {
	    if (olevel <= 1)
		flag = add_string_option (O_PHASE_, "i");
	    else {
		flag = add_string_option (O_PHASE_, "p");
		flag = add_string_option (O_PHASE_, "i");
	    }
	} else {
	    /*
	     * Determine which back end phase(s) need to be run.
	     *
	     *			-O0/-O1	-O2		-O3
	     *			===========================
	     *		.B,.I:	cg	wopt/cg		lno/wopt/cg
	     *		.N:	cg	wopt/cg		wopt/cg
	     *		.O:	cg	cg		cg
	     */
	    if (olevel <= 1 || source_kind == S_O)
		flag = add_string_option(O_PHASE_, "c");
	    else if (olevel == 2 || source_kind == S_N) {
		flag = add_string_option(O_PHASE_, "w");
		prepend_option_seen (flag);
		flag = add_string_option(O_PHASE_, "c");
	    }
	    else {
		flag = add_string_option(O_PHASE_, "l");
		prepend_option_seen (flag);
		flag = add_string_option(O_PHASE_, "w");
		prepend_option_seen (flag);
		flag = add_string_option(O_PHASE_, "c");
	    }
	}
	prepend_option_seen (flag);


}


/* xt_set_default_switch assumes that xtensa_getopt has already been called,
   and that all keys passed to it can only take a value 0 or 1.
   In other words, it is for toggle switches only.

   if you keep the OPTIONS file up to date properly, then this will automatically
   handle the #defines and such. Isn't that nice?

   for xtensa options, is_toggled == was this option set on the command line
*/

static void xt_set_default_switch(int toggleObject, char * key, int onOption, int offOption)
{
  xtensa_values * xt_val;
  int 	isUsed;
  int 	flag;

  if ((xt_val = xtensa_find_key(xtensa_default_params, key)) == NULL)
    return;

  if (xt_val->num_values == 0)
    return;

  /* if it wasn't set on the command line, then we can set it without fear */
  if (!is_toggled(toggleObject)) {
    prepend_option_seen(xtensa_int_value(xt_val) ? onOption : offOption);
  }
  else {
    /* An option cannot be turned on if the config doesn't support it.  */
    if ((option_was_seen(onOption) && xtensa_int_value(xt_val) == 0))
      error("configuration does not support %s ", get_option_name(onOption));
    /* Similarly, the endianness cannot be changed.  */
    if (onOption == O_mbig_endian &&
        option_was_seen(offOption) && xtensa_int_value(xt_val) == 1)
      error("configuration does not support %s ", get_option_name(offOption));
    if (option_was_seen(offOption) && xtensa_int_value(xt_val) == 1 && onOption == O_mdensity) {
      error("configuration does not support %s ", get_option_name(offOption));
    }
  }
}

static void xt_set_default_option(int toggleObject, char * key, int option, int isPath)
{
  xtensa_values * xt_val;
  int 	          flag;
  char * 	  buf;

  if ((xt_val = xtensa_find_key(xtensa_default_params, key)) == NULL)
    return;

  if (xt_val->num_values == 0)
    return;

  if (!is_toggled(toggleObject)) {
    if (xt_val->values[0].type == XTPM_INT)
      {
	buf = malloc(16);
	sprintf(buf, "%d", xt_val->values[0].v.i);
      }
    else {
      if (xt_val->values[0].v.s == NULL)
	return;

      if (isPath) {
	buf = string_copy(xtensa_path_value(xt_val));
      }
      else {
	buf = string_copy(xtensa_string_value(xt_val));
      }
    }

    flag = add_string_option(option, buf);
    prepend_option_seen(flag);
    free(buf);
  }
}

void xt_handle_lsp_option(void)
{
  int flag;
  string temp;
  if (lsp_name == NULL) {
    if ((temp = xtensa_string_value
	 (xtensa_find_key(xtensa_default_params, "default-lsp"))) == NULL)
      return;
    lsp_name = (char *) malloc(strlen(temp) + 1);
    strcpy(lsp_name, temp);
    flag = add_string_option(O_mlspQ, lsp_name);
    prepend_option_seen(flag);
  }
}

char fixed_prefix[1024];
char target_xcc_libdir[1024];
char target_libdir[1024];
char arch_libdir[1024];
char lspDir[1024];
static char *B_file_arg=NULL;
string config_prefix = NULL;

void xtensa_add_linker_options(void)
{
  int flag;
  int offset;
  if ((config_prefix = xtensa_path_value
       (xtensa_find_key(xtensa_default_params, "config-prefix"))) == NULL)
    return;
  FIXPATH(config_prefix, fixed_prefix);

  strcpy(target_libdir, fixed_prefix);
  strcat(target_libdir, "/xtensa-elf/lib/");

  FIXPATH(lsp_name, lspDir);
  /* If the LSP name does not include a directory separator, then
     assume it is in the default library directory.  Note: There's no
     need to check for backward slashes as directory separators
     because the FIXPATH macro above will have converted the name to a
     Unix-style path.  */
  if (strchr(lspDir, '/') == 0) {
    strcpy(lspDir, target_libdir);
    strcat(lspDir, lsp_name);
  }
#ifdef _WIN32 
  else if (strchr(lspDir, '/') == 0 && lsp_name[1] != ':') {
    strcpy(lspDir, target_libdir);
    strcat(lspDir, lsp_name);
  }
#endif
  strcat(lspDir, "/");
  handle_specs();
}


/* xt-read_linker_specs must be called after xt_set_default_params for the lsp */

string_list_t * start_link_args;
string_list_t * lib_link_args;
string_list_t * end_link_args;
string_list_t * c_args;
string_list_t * cpp_args;

static void find_and_add_library(string_list_t * list, char * fileName);

/* Pass the -B arg to cpp, handled specially because argument must be postfixed with include */ 
extern void handle_B(char *dir)
{
  if (dir) {
    B_file_arg = malloc(strlen(dir)+1);
      strcpy(B_file_arg,dir);
  }
}

extern void handle_C() 
{
  if (last_phase != P_any_cpp) {
    error("Use of -C requires -E");
  }
}

extern void handle_stack_alignment(char *arg)
{
	if (arg && *arg) {
		char *p;
		int n = strtod(arg, &p);
		if (*p != 0)
			error("illegal characters in argument: %s ", p);
		if ( n != 4 && n != 8 && n != 16 )
			error("bad value %d for stack alignment; expecting 4, 8, or 16", n);
	} else {
		error("missing argument for -mstack-alignment option");
	}
}


string_list_t * search_dirs;

static void find_and_add_library(string_list_t * list, char * fileName)
{
  string_item_t * s;
  FOREACH_STRING(s, search_dirs) {
    string fullyQualifiedFileName = concat_strings(STRING_NAME(s), fileName);
    if (xt_add_object_file(fullyQualifiedFileName, list)) {
      return;
    }
  }
  // just bail and see if ld can find it.
  add_string(list, fileName);
}

typedef struct spec {
  string name;
  string data;
  string expanded;
  int refed;

  struct spec * next;
  struct spec * prev;
} spec;

static spec * spec_list = NULL;

static spec * find_spec(const char * name)
{
  spec * s = spec_list;
  while (s && strcmp(s->name, name) != 0) {
    s = s->next;
  }
  return s;
}


static spec * new_spec(const char * name, const char * data) 
{
  spec * t = find_spec(name);
  if (t == NULL) {
    t = malloc(sizeof(spec));
    if (t == NULL) {
      error ("Out of memory\n");
      exit(1);
    }
    t->name = malloc(strlen(name) + 1);
    strcpy(t->name, name);
    if (data) {
      t->data = malloc(strlen(data) + 1);
      strcpy(t->data, data);
    }
    else {
      t->data = malloc(1);
      t->data[0] = '\0';
    }
    t->expanded = NULL;
    t->refed = 0;
    t->next = spec_list;
    t->prev = NULL;
    spec_list = t;
  }
  else {
    // t's name is already the same
    if (data) {
      t->data = malloc(strlen(data) + 1);
      strcpy(t->data, data);
    }
    else {
      t->data = malloc(1);
      t->data[0] = '\0';
    }
  }
  return t;
}

static void append_data(spec * s, const char * data)
{
  if (data) {
    char * tmp = s->data;

    // one for the terminator, one for the space
    int len = strlen(s->data) + strlen(data) + 2;
    tmp = malloc(len);
    sprintf(tmp, "%s %s", s->data, data);  
    free(s->data);
    s->data = tmp;
  }
}


static char *ensure_trailing_slash(char *prefix)
{
  int len = strlen(prefix);
  if (prefix[len - 1] != '/') {
    char *new_prefix = (char *)malloc(len + 2);
    memcpy(new_prefix, prefix, len);
    new_prefix[len] = '/';
    new_prefix[len + 1] = 0;
    /* This is always called with a path obtained from xtensa_path_value,
       so free it once we are done with it.  */
    free(prefix);
    return new_prefix;
  }
  return prefix;
}


static void init_spec_list(void) 
{
  xtensa_params xp = xtensa_default_params;
  char *tools_dir = xtensa_path_value(xtensa_find_key(xp, "install-prefix"));
  char *config_dir = xtensa_path_value(xtensa_find_key(xp, "config-prefix"));
  char *tie_dir = xtensa_path_value(xtensa_find_key(xp, "tie-includedir"));

  /* Both "tools_dir" and "config_dir" are used as GCC "prefixes" and must
     end with directory separators (i.e., slashes).  */
  tools_dir = ensure_trailing_slash(tools_dir);
  config_dir = ensure_trailing_slash(config_dir);

  new_spec("libgcc", "-lgcc");
  new_spec("xtensa_tools_dir", tools_dir);
  new_spec("xtensa_config_dir", config_dir);
  new_spec("xtensa_lsp_dir", lspDir);
  
  new_spec("xtensa_tie_includedir", tie_dir ? tie_dir : "%(xtensa_config_dir)xtensa-elf/include");
  new_spec("xtensa_compiler_includedir", "%(xtensa_tools_dir)lib/xcc-lib/include");
  new_spec("xtensa-c-includes", 
	   "%(xtensa-lsp-includes) "
	   "-isystem %(xtensa_compiler_includedir) "
	   "-isystem %(xtensa_config_dir)xtensa-elf/arch/include "
	   "-isystem %(xtensa_tie_includedir) "
	   "-isystem %(xtensa_tools_dir)xtensa-elf/include "
	   "-iprefix %(xtensa_config_dir)xtensa-elf/");
  new_spec("xtensa-cpp-includes", "-isystem %(xtensa_config_dir)xtensa-elf/include/xcc/g++ -isystem %(xtensa_config_dir)xtensa-elf/include/xcc/");
  new_spec("xtensa-startfile-dirs", 
	   "%(xtensa-lsp-startfile-dirs) "
	   "%(xtensa_lsp_dir) "
	   "%(xtensa_config_dir)xtensa-elf/lib/xcc/ "
	   "%(xtensa_config_dir)xtensa-elf/arch/lib/ "
	   "%(xtensa_config_dir)xtensa-elf/lib/ "
	   );
}


#define BUF_SIZE 2048

static void parse_specs_file(FILE * file)
{
  char buf[BUF_SIZE];
  spec * s = NULL;
  char * c;
  while(fgets(buf, BUF_SIZE, file) != NULL) {
    switch (buf[0]) 
      {
      case '*':
	c = strrchr(buf, ':');
	if (!c || *c != ':') {
	  error("Invalid specs file. Erroneous line was \"%s\".", buf);
	  exit(1);
	}
	*c = '\0';
	s = new_spec(buf + 1, NULL);
	break;
      case '#':
      case '\r':
      case '\n':
	break;
      default:
        // "%O" in a spec means to use the correct object file suffix.
        // for us that is ".o", which we can do in place early
	c = buf;
        while (c = strchr(c, '%')) {
          if (c[1] == 'O') {
            c[0] = '.';
            c[1] = 'o';
          }
	  c++;
        }
	append_data(s, buf);
	break;
    }
  }
}

char * expand_spec_string(char * spec_string, int * consumed);
char * expand_spec(const char * name)
{
  spec * s = find_spec(name);
  if (s == NULL) {
    return NULL;
  }
  if (s->expanded != NULL) {
    return s->expanded;
  }
  else {
    int dummy;
    s->expanded = expand_spec_string(s->data, &dummy);
    return s->expanded;
  }
}

char * expand_spec_string(char * spec_string, int * consumed)
{
  int pos = 0;
  int start = 0;
  char * expanded = calloc(4096, 1);
  char * e_pos = expanded;
  char * expanded_spec;
  char * expand_name_end;
  int advance;
  int negate;
  int flag;
  
  while (1) {
    switch (spec_string[pos])
      {
      case '%':
	pos++;
	switch(spec_string[pos]) 
	  {
	  case '(':
	    expand_name_end = strchr(&spec_string[pos++], ')');
	    if (expand_name_end == NULL) {
	      error("Invalid specs file. Erroneous string was \"%s\".", spec_string);
	      exit(1);
	    }
	    *expand_name_end = '\0';
	    expanded_spec = expand_spec(&spec_string[pos]);
	    if (expanded_spec != NULL) { 
	      strcat(expanded, expanded_spec);
	      e_pos += strlen(expanded_spec);
	    }
	    pos += expand_name_end - &spec_string[pos] + 1;
	    break;
	  case '{':
	    pos++;
	    if (spec_string[pos] == '!') {
	      negate = 1;
	      pos++;
	    }
	    else {
	      negate = 0;
	    }

	    expand_name_end = strchr(&spec_string[pos], ':');
	    if (expand_name_end == NULL) {
	      error("Invalid specs file. Erroneous string was \"%s\".", spec_string);
	      exit(1);
	    }

	    *expand_name_end = '\0';
	    // do not error out if the flag couldn't be found. But just ignore it
	    // This is a hacky workaround for situations where gcc needs an option
	    // but xcc's other option handling works.

	    // For example, the linux lsp uses {-mlsp=*:--multi-lib-dir=*} to get
	    // --multi-lib-dir passed to the linker. But xcc's OPTIONS handling
	    // solves that case for us
	    flag = get_option_from_name(&spec_string[pos]);
	    pos = expand_name_end - spec_string + 1;
	    expanded_spec = expand_spec_string(&spec_string[pos], &advance);
	    if (flag != -1 && 
		expanded_spec != NULL &&
		((!negate && option_was_seen(flag)) || 
		 (negate && !option_was_seen(flag)))) { 
	      strcat(expanded, expanded_spec);
	      e_pos += strlen(expanded_spec);
	    }
	    pos += advance;
	    free(expanded_spec);

	    // Check for and advance past the closing brace.
	    if (spec_string[pos] != '}') {
	      error("Invalid specs file. Missing closing brace.", spec_string);
	      exit(1);
	    }
	    pos++;
	    break;
	  case 's':
	    *e_pos++ = '%';
	    *e_pos++ = 's';
	    pos++;
	    break;
	  case '*':
	    // see the comment above about unrecognized options:
	    pos++;
	    break;
	  default:
	    error("xt-xcc does not support the %%%c feature of spec files. "
		  "Erroneous string was \"%s\".", spec_string[pos], spec_string);
	    exit(1);
	    break;
	  }
	break;
      case '\r':
         pos++;
         break; 
      case '}':
      case '\0':
      case '\n':
	*consumed = pos;
	return expanded;
      default:
	*e_pos++ = spec_string[pos++];
	break;
      }
  }
}

static void expand_specs(void)
{
  spec * s = spec_list;
  while (s != NULL) {
    expand_spec(s->name);
    s = s->next;
  }
}

void spec_to_string_list(string_list_t * list, const char * spec_name) 
{
  spec * s = find_spec(spec_name);
  if (s) {
    char * pos = s->expanded;
    char saved;
    while (1) {
      char * start = pos;
      while (*pos != '\0' && *pos != ' ' && *pos != '%') {
	pos++;
      }
      switch (*pos) 
	{
	case '\0': 
	case '\r': 
	case '\n': 
	  if (strlen(start) > 0)
	    add_string(list, start); 
	  return;
	case ' ':
	  saved = *pos;
	  *pos = '\0';
	  if (strlen(start) > 0)
	    add_string(list, string_copy(start));
	  *pos = saved;
	  pos++;
	  break;
	case '%':
	  if (*(pos + 1) != 's') {
	    //this error should have been caught a long time ago
	    //'%s' is the only spec directive that should appear here
	    error("invalid spec file");
	    exit(1);
	  }
	  *pos = '\0';
	  find_and_add_library(list, start);
	  pos += 2;
	  break;
      }
    }
  }
}


static void handle_specs(void) 
{
  int desc;
  int readlen;
  struct stat statbuf;
  char *buffer;
  char *p;
  char * fullyQualifiedFileName;
  char * specFileName;
  string_item_t * sli;

  specFileName = string_copy(lspDir);
  specFileName = concat_strings(specFileName, "specs");
  /* Open and stat the file.  */
  FILE * file = fopen (specFileName, "rb");
#ifdef _WIN32
  if (file == NULL) {
    int len = strlen(specFileName);
    int i;
    for (i = 0; i < len; i++)
      if (specFileName[i] == '\\')
          specFileName[i] = '/';
    file = fopen(specFileName, "rb");
  }
#endif   
 
  if (file == NULL) {
    error ("cannot open %s. Continuing without lsp options.", specFileName);
    return;
  }

  init_spec_list();
  parse_specs_file(file);
  expand_specs();

  start_link_args = init_string_list();
  end_link_args = init_string_list();
  lib_link_args = init_string_list();
  c_args = init_string_list();
  cpp_args = init_string_list();

  search_dirs = init_string_list();
  spec_to_string_list(search_dirs, "xtensa-startfile-dirs");

  if (instrumentation_invoked == TRUE) {
    prepend_option_seen(O_mlongcalls);
  }
  spec_to_string_list(start_link_args, "link");
  if (!option_was_seen(O_nostartfiles)) {
    spec_to_string_list(start_link_args, "startfile");
    spec_to_string_list(end_link_args, "endfile");
  }
  if (!option_was_seen(O_nodefaultlibs)) {
    spec_to_string_list(lib_link_args, "libgcc");
    spec_to_string_list(lib_link_args, "lib");

    if (invoked_lang == L_CC || instrumentation_invoked == TRUE) {
      add_string_to_beginning(lib_link_args, "-lm");
      add_string_to_beginning(lib_link_args, "-lstdc++");
    }
    if (instrumentation_invoked == TRUE) {
      if (option_was_seen(O_fb_create_32) || option_was_seen(O_fb_create_32Q)) {
	add_string_to_beginning(lib_link_args, "-linstr32");
      }
      else if (option_was_seen(O_fb_create_64) || option_was_seen(O_fb_create_64Q)) {
	add_string_to_beginning(lib_link_args, "-linstr64");
      }
      else if (option_was_seen(O_fb_create_HW) || option_was_seen(O_fb_create_HWQ)) {
	add_string_to_beginning(lib_link_args, "-lgdbionss");
	add_string_to_beginning(lib_link_args, "-ldbfs");
	add_string_to_beginning(lib_link_args, "-linstrHW");
      }
    }
    if (option_was_seen(O_hwpgQ__)){
      char * hwpglibtempl = "-lhwprofile%d";
      char * hwpglib = (char *) malloc (strlen(hwpglibtempl) + 10);
      sprintf(hwpglib, hwpglibtempl, timernumber);
      add_string_to_beginning(lib_link_args, "-lgdbionss");
      add_string_to_beginning(lib_link_args, "-ldbfs");
      add_string_to_beginning(lib_link_args, "--no-whole-archive");
      add_string_to_beginning(lib_link_args, hwpglib);
      add_string_to_beginning(lib_link_args, "--whole-archive");
    }

    spec_to_string_list(lib_link_args, "libgcc");
  }

  if (B_file_arg) {
    add_string(c_args,"-isystem");
    add_string(c_args,concat_strings(B_file_arg,"/include"));
  }
  spec_to_string_list(c_args, "subtarget_cpp_spec");

  if (!option_was_seen(O_undef)) {
    spec_to_string_list(c_args, "predefines");
  }
  if (!nostdinc) {
    spec_to_string_list(c_args, "xtensa-c-includes");
  }
  if (!nostdinc && !nostdincplusplus) {
    spec_to_string_list(cpp_args, "xtensa-cpp-includes");
  }
  
  FOREACH_STRING(sli, search_dirs) {
    add_library_dir(sli->name);
  }
}


static int xt_add_object_file(char * fullName, string_list_t * list)
{
  struct stat statbuf;
  int flag;

  if (stat (fullName, &statbuf) >= 0) {
    add_string(list, fullName);
    return TRUE;
  }
  else
    return FALSE;
}
