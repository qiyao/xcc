
/* 
   Copyright (C) 2002-2005 Tensilica, Inc.  All Rights Reserved.
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


#include "basic.h"

/* drops path prefix in string; result points inside old string */
extern string drop_path (string s);

/* check whether file exists */
extern boolean file_exists (string path);

/* check whether is a directory */
extern boolean is_directory (string path);

/* check whether directory is writable */
extern boolean directory_is_writable (string path);

/* get current working directory */
extern string get_cwd (void);

/* Platform-specific macros for checking filenames */
#ifdef _WIN32
#define is_dir_separator(x)      ({ char ds = (x); ds == '/' || ds == '\\'; })
#define is_absolute_file_name(x) ({ const char *fn = (x); \
				    is_dir_separator(fn[0]) || fn[1] == ':'; })
#else /* !_WIN32 */
#define is_dir_separator(x)      ((x) == '/')
#define is_absolute_file_name(x) (is_dir_separator((x)[0]))
#endif /* !_WIN32 */

