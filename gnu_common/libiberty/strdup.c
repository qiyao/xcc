
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

char *
strdup(s)
     char *s;
{
    char *result = (char*)malloc(strlen(s) + 1);
    if (result == (char*)0)
	return (char*)0;
    strcpy(result, s);
    return result;
}
