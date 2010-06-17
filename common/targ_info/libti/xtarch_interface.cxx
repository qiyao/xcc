
/*

  Copyright (C) 2004-2005 Tensilica, Inc.  All Rights Reserved.

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

*/

#include "errors.h"
#include "defs.h"
#include "cxx_memory.h"
#include "xtmap.h"
#include "xtensa-isa.h"
#include "xtensa-tie.h"
#include "topcode.h"
#include "xtarch_interface.h"


#ifdef _WIN32

#include <windows.h>

static void
print_last_error (void)
{
  char *buf = NULL;
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		FORMAT_MESSAGE_FROM_SYSTEM | 
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		GetLastError(),
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR)&buf,
		0,
		NULL);

  if (buf) {
    fputs (buf, stderr);
    fputc ('\n', stderr);
  }
}

void *
TI_DLL_Load (const char *path, int print_err)
{
    HINSTANCE handle;
    handle = LoadLibrary (path);
    if (!handle && print_err) {
      fprintf (stderr, "Unable to load library: %s\n", path);
      print_last_error();
    }
    return (void *)handle;
}

void *
TI_DLL_Get_Symbol (void *handle, char *sym, const char *libname, int print_err)
{
    void *addr = (void *) GetProcAddress ((HINSTANCE) handle, sym);
    if (!addr && print_err) {
      fprintf (stderr, "Cannot find \"%s\" symbol in library: %s\n",
	       sym, libname);
      print_last_error();
    }
    return addr;
}

#else /* !_WIN32 */

#include <dlfcn.h>

void *
TI_DLL_Load (const char *path, int print_err)
{
    void *handle = dlopen (path, RTLD_LAZY );
    if (!handle && print_err)
      {
	fputs (dlerror (), stderr);
	fputc ('\n', stderr);
      }
    return handle;
}

void *
TI_DLL_Get_Symbol (void *handle, char *sym, const char *libname, int print_err)
{
    char *error;
    void *addr = dlsym (handle, sym);
    if (((error = dlerror ()) != NULL) && print_err)
      {
	fputs (error, stderr);
	fputc ('\n', stderr);
      }
    return addr;
}

#endif /* !_WIN32 */


/* Try to load and initialize the xtensa architecture dll. */
XT_Architecture_p
TI_Architecture_Init (MEM_POOL *pool,
		      const char *dll,
		      char **libisa_dlls,
		      char **libtie_dlls)
{
#ifdef ENABLE_ARCHITECTURE_EXTENSIONS

  /* Try to load the architecture dll. Return NULL if it doesn't exist or there
     are other problems loading it. */
  void *handle = TI_DLL_Load(dll, 1);
  if (!handle)
    return NULL;

  int (*interface_version_fn) ();
  interface_version_fn = (int (*)())
    TI_DLL_Get_Symbol(handle, "interface_version", dll, 1);

  void (*xt_arch_fn) (xtensa_isa *, char **);
  xt_arch_fn = (void (*)(xtensa_isa *, char **))
    TI_DLL_Get_Symbol(handle, "architecture_extensions", dll, 1);

  if (!interface_version_fn ||
      !xt_arch_fn)
  {
    fprintf(stderr, "Warning: Corrupt architecture extensions library.\n");
    fprintf(stderr, "Continuing without architecture extensions.\n");
    return NULL;
  }
  
  /* check that the version number matches */
  if ((*interface_version_fn) () != XTARCH_INTERFACE_VERSION)
  {
    fprintf(stderr, "Warning: The version number of the architecture extensions\n"
	    "library, %s, does not match the current compiler.\n", dll);
    fprintf(stderr, "Continuing without architecture extensions.\n");
    return NULL;
  }

  /* get the architecture extensions information. */
  xtensa_isa isa;
  (*xt_arch_fn)(&isa, libisa_dlls);

  char *error_msg;
  xtensa_tie xtie = xtie_init(libtie_dlls, 0, &error_msg);
  if (xtie == 0) {
    fprintf (stderr, "Error: could not load TIE: %s\n", error_msg);
    exit(32);
  }
  
  return CXX_NEW(XT_Architecture(pool, isa, xtie), pool);
#else

  return NULL;

#endif
}

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
