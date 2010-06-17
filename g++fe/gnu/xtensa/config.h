
/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


#ifdef IN_GCC
#include "auto-host.h"
#endif
#ifdef IN_GCC
#include "gansidecl.h"
#endif
#ifdef IN_GCC
#ifdef SGI_MONGOOSE
/* hack for sun */
#if defined(__i386) || defined(__sun__) || defined(__CYGWIN__) || defined(_WIN32)
#include "i386/xm-i386.h"
#else
This clause should never be used on Xtensa, and this comment
will result in a parse error if it is.
#include "ia64/xm-ia64.h"
#endif
#endif
#endif
#ifdef IN_GCC
#include "hwint.h"
#endif
#ifndef HAVE_ATEXIT
#define HAVE_ATEXIT
#endif
#ifndef POSIX
#define POSIX
#endif
#ifndef BSTRING
#define BSTRING
#endif
