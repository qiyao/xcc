/*

 Copyright (C) 2004 Tensilica, Inc.  All Rights Reserved.

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

#ifndef _COMMON_UTIL_WINUTIL_H_
#define _COMMON_UTIL_WINUTIL_H_
#ifdef _WIN32
#include<windows.h>

#ifdef __cplusplus
extern "C" {
#endif
HINSTANCE dlopen(
  char* lpLibFileName,  // points to name of executable module
  int dwFlags           // entry-point execution flag
);
#define RTLD_LAZY DONT_RESOLVE_DLL_REFERENCES
#define RTLD_NOW 0
#define RTLD_GLOBAL 0

/* definitions for open flags */
#define O_RDONLY _O_RDONLY
#define O_CREAT _O_CREAT
#define O_RDWR _O_RDWR
#define O_WRONLY _O_WRONLY
#define O_BINARY _O_BINARY


void
PrintFormatMessage(void);

extern void *win_sbrk (long size);

#define sbrk(n) (n)

extern long getregionsize (void);
extern long getpagesize (void);
extern int slwait (int *sl);
extern int slrelease (int *sl);


extern const char *dlerror(void);
extern HINSTANCE dlopen(
  char *lpLibFileName,  // points to name of executable module
  int dwFlags           // entry-point execution flag
);

#ifdef __cplusplus
}
#endif

#endif
#endif /* winutil.h */
