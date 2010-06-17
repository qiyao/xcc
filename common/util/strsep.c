
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*
 * Copyright (c) 1995, 1996 Gunther Schadow.  All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdlib.h>
#include <string.h>

/*
 * The BSD strsep(3) function
 */

char *
strsep(char **str, const char *delim)
{
  char *p;
  char *old=*str;

  if (old)
    for(p=old;*p;p++)
      {
	if (strchr(delim,*p))
	  {
	    *p=0;
	    *str= ++p;     /* delim found --> *str = what follows delim */
	    return old;    /* delim found --> return old *str           */
	  }
      }
  else
    return NULL;         /* *str==NULL --> return NULL */
  *str=NULL;             /* no delim found --> *str = NULL */
  return old;            /* return old *str */
}
  
