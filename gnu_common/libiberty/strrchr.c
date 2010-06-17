
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Portable version of strrchr().
   This function is in the public domain. */

/*
NAME
	strrchr -- return pointer to last occurance of a character

SYNOPSIS
	char *strrchr (const char *s, int c)

DESCRIPTION
	Returns a pointer to the last occurance of character C in
	string S, or a NULL pointer if no occurance is found.
	
BUGS
	Behavior when character is the null character is implementation
	dependent.
*/

#include <ansidecl.h>

char *
strrchr (s, c)
  register const char *s;
  int c;
{
  char *rtnval = 0;

  do {
    if (*s == c)
      rtnval = (char*) s;
  } while (*s++);
  return (rtnval);
}
