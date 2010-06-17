
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */


extern	double	__ceil(double);

#ifdef __GNUC__
extern  long double  __ceill(long double);

long double    ceill() __attribute__ ((weak, alias ("__ceill")));

#endif

long double
__ceill(long double x)
{
	return ( (long double)__ceil((double)x) );
}

