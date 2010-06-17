
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	int	__finite(double);

#ifdef __GNUC__
extern  int  __finitel(long double);

int    finitel() __attribute__ ((weak, alias ("__finitel")));

#endif

int
__finitel(long double x)
{
	return ( __finite((double)x) );
}

