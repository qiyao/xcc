
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__logb(double);

#ifdef __GNUC__
extern  long double  __logbl(long double);

long double    logbl() __attribute__ ((weak, alias ("__logbl")));

#endif

long double
__logbl(long double x)
{
	return ( (long double)__logb((double)x) );
}

