
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__rint(double);

#ifdef __GNUC__
extern  long double  __rintl(long double);

long double    rintl() __attribute__ ((weak, alias ("__rintl")));

#endif

long double
__rintl(long double x)
{
	return ( (long double)__rint((double)x) );
}

