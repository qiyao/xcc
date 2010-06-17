
/* 
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

#ifndef ISELECTOR_INCLUDED
#define ISELECTOR_INCLUDED

extern void ISEL_initialize(void);
extern void ISEL_finalize(void);
extern bool ISEL_gen(WN *stmt, TN *result, OPS *ops);
extern TN *Generate_Constant (TYPE_ID rtype, WN* wn, TN *result, TN *src, OPS *ops);
extern TN *Generate_Constant (TYPE_ID rtype, TN *result, INT32 val, OPS *ops);

#endif /* ISELECTOR_INCLUDED */
