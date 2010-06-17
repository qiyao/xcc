
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__fabs(double);

#ifdef __GNUC__
extern  long double  __fabsl(long double);

long double    fabsl() __attribute__ ((weak, alias ("__fabsl")));

#endif

long double
__fabsl(long double x)
{
	return ( (long double)__fabs((double)x) );
}

