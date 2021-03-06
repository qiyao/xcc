
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__floor(double);

#ifdef __GNUC__
extern  long double  __floorl(long double);

long double    floorl() __attribute__ ((weak, alias ("__floorl")));

#endif

long double
__floorl(long double x)
{
	return ( (long double)__floor((double)x) );
}

