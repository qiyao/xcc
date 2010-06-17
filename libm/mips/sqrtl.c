
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__sqrt(double);

#ifdef __GNUC__
extern  long double  __sqrtl(long double);

long double    sqrtl() __attribute__ ((weak, alias ("__sqrtl")));

#endif

long double
__sqrtl(long double x)
{
	return ( (long double)__sqrt((double)x) );
}

