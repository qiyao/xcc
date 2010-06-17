
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__trunc(double);

#ifdef __GNUC__
extern  long double  __truncl(long double);

long double    truncl() __attribute__ ((weak, alias ("__truncl")));

#endif

long double
__truncl(long double x)
{
	return ( (long double)__trunc((double)x) );
}

