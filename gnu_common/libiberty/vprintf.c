
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>
#include <ansidecl.h>
#undef vprintf
int
vprintf (format, ap)
     const char *format;
     va_list ap;
{
  return vfprintf (stdout, format, ap);
}
