
/*
   Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.
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
#include <stdlib.h>
#ifdef _WIN32
#include <io.h>
#endif
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include "phases.h"
#include "lang_defs.h"
#include "string_utils.h"
#include "file_names.h"
#include "file_utils.h"
#include "errors.h"
#include "opt_actions.h"
#include "option_seen.h"
#include "option_names.h"
#include "run.h"

#ifdef _WIN32
#define tempnam _tempnam
#endif

extern int errno;

boolean keep_flag = FALSE;
int cur_source_file_num = 0;
string_list_t *count_files = NULL;
static string_list_t *temp_files = NULL;
static string_list_t * late_delete_temp_files = NULL;

static string tmpdir;
static string saved_object = NULL;

#ifndef _WIN32
#define DEFAULT_TMPDIR	"/tmp"
#else
#include <windows.h>

#define DEFAULT_TMPDIR default_tmpdir()

static string
conv_dir_separator_to_posix(string path)
{
  int i;
  for (i = 0; path[i]; i++) {
    if (path[i] == '\\')
      path[i] = '/';
  }
  return path;
}

static string
conv_dir_separator_to_win32(string path)
{
  int i;
  for (i = 0; path[i]; i++) {
    if (path[i] == '/')
      path[i] = '\\';
  }
  return path;
}

static string
default_tmpdir(void)
{
  static char dir[MAX_PATH];
  if (GetTempPath(MAX_PATH, (LPSTR)dir) == 0)
    return "";
  conv_dir_separator_to_posix(dir);
  return (string) dir;
}

#endif /* _WIN32 */

/* get object file corresponding to src file */
extern string
get_object_file (string src)
{
	return change_suffix(drop_path(src), "o");
}

/*
 * Need temp file names to be same if use same suffix
 * (because this can be called for both producer and consumer
 * of temp file), but also need names that won't conflict.
 * Put suffix in standard place so have easy way to check
 * if file already created.
 * Use tempnam to generate unique file name;
 * tempnam verifies that file is writable.
 */
string
create_temp_file_name (string suffix)
{
	buffer_t buf;
	buffer_t pathbuf;
	size_t prefix_len;
	string s;
	string_item_t *p;
	char * left;
	char * right;
	string divider;
	/* use same prefix as gcc compilers; */
	sprintf(buf, "cc%d%s#%x.XXXXXX", cur_source_file_num, suffix, rand());
	sprintf(pathbuf, "%s/%s", tmpdir, buf); /* full path of tmp files */
#ifdef _WIN32
	/* Canonicalize paths to use forward slashes so that comparisons
	   with existing temp_files will work.  */
        conv_dir_separator_to_posix(pathbuf);
#endif
	prefix_len = strlen(pathbuf) - strlen(strchr(pathbuf, '#')); /* subtracting the XXXXXX */

	for (p = temp_files->head; p != NULL; p = p->next) {
	  if (strncmp(p->name, pathbuf, prefix_len) == 0) {
	    /* matches the prefix and suffix character */
	    return p->name;
	  }
	}

	/* need new file name */
	s = mktemp (pathbuf);
	if (!s)
	  internal_error("Couldn't create temporary file %s\n", pathbuf);
#ifdef _WIN32
	/* Some phases cannot handle backslashes in file names yet.  */
        s = conv_dir_separator_to_posix(s);
#endif

	s = string_copy(s);
	add_string (temp_files, s);
	return s;
}

extern string
construct_name (string src, string suffix)
{

	if (keep_flag || current_phase == remember_last_phase) {
		string srcname;
		/*
		 * if -c -o <name>, then use name.suffix
		 * (this helps when use same source to create different .o's)
		 * if outfile doesn't have .o suffix, don't do this.
		 */
		if (outfile && option_was_seen(O_c) && get_suffix(outfile))
		  srcname = outfile;
		else
		  srcname = src;
		return change_suffix(drop_path(srcname), suffix);
	} else {
		return create_temp_file_name (suffix);
	}
}

/* use given src name, but check if treated as a temp file or not */
extern string
construct_given_name (string src, string suffix, boolean keep)
{
	string s;
	s = change_suffix(drop_path(src), suffix);
	if (keep || current_phase == remember_last_phase) {
		return s;
	} else {
		s = string_copy(s);
		add_string_if_new (temp_files, s);
		return s;
	}
}

extern void
mark_saved_object_for_cleanup ( void )
{
	if (saved_object != NULL)
	add_string_if_new (temp_files, saved_object);
}

/* Create filename with the given extension; eg. foo.anl from foo.f */
extern string
construct_file_with_extension (string src, string ext)
{
	return change_suffix(drop_path(src),ext);
}

/* amazingly, tempnam on window's NT reverses the order 
   from other platforms.
*/

#ifdef _WIN32
#define FIRST_TMP_ENV_VAR "TMP"
#define SEC_TMP_ENV_VAR "TMPDIR"
#else
#define FIRST_TMP_ENV_VAR "TMPDIR"
#define SEC_TMP_ENV_VAR "TMP"
#endif

extern void
init_temp_files (void)
{
	srand((unsigned)time(NULL));
        tmpdir = getenv(FIRST_TMP_ENV_VAR);
        if (tmpdir == NULL) {
	  tmpdir = getenv(SEC_TMP_ENV_VAR);
	  if (tmpdir == NULL) {
	    tmpdir = DEFAULT_TMPDIR;
	  }
	}
#ifdef _WIN32
	/* avoid space in temp path issues on windows */
	{
	  static char buf[1024];
	  GetShortPathName(tmpdir, buf, 1024);
	  tmpdir = buf;
        }
#endif
	if (!is_directory(tmpdir)) {
		error("Temporary directory does not exist: %s", tmpdir);
	}
	else if (!directory_is_writable(tmpdir)) {
		error("Temporary directory is not writable: %s", tmpdir);
	}
	else if (is_dir_separator (tmpdir[strlen(tmpdir)-1])) {
		/* drop / at end so strcmp matches */
		tmpdir[strlen(tmpdir)-1] = '\0';
	}
	temp_files = init_string_list();
	late_delete_temp_files = init_string_list();
}

extern void
init_count_files (void)
{
        count_files = init_string_list();
}

extern void
cleanup (void)
{
  string_item_t *p;
  int status;

  /* phase == P_NONE before anything and then again when everything is over */
  if (current_phase == P_NONE) {
    append_string_lists(temp_files, late_delete_temp_files);
  }

  /* cleanup temp-files */
  if (temp_files == NULL) return;
  for (p = temp_files->head; p != NULL; p = p->next) {
    if (debug) printf("unlink %s\n", p->name);
    if (execute_flag) {
#ifndef _WIN32
      status = unlink(p->name);
#else
      /* WIN32 unlink does not accept '/' as a directory separator. 
       * So gotta convert that before calling unlink. */
      conv_dir_separator_to_win32(p->name);
      status = unlink(p->name);
      conv_dir_separator_to_posix(p->name);
#endif
      if (status != 0 && errno != ENOENT) {
		internal_error("cannot unlink temp file %s", p->name);
		perror(program_name);
      }
    }
  }
  temp_files->head = temp_files->tail = NULL;
}

extern void delete_after_ld (string file_name)
{
  add_string(late_delete_temp_files, file_name);
}
