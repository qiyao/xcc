
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Stub implementation of (obsolete) index(). */

extern char * strchr();

char *
index (s, c)
  char *s;
  int c;
{
  return strchr (s, c);
}
