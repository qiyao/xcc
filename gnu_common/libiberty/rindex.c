
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Stub implementation of (obsolete) rindex(). */

extern char *strrchr ();

char *
rindex (s, c)
  char *s;
  int c;
{
  return strrchr (s, c);
}
