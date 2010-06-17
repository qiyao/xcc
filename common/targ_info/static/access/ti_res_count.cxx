
/* 
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /isms/cmplrs.src/osprey1.0/common/targ_info/access/RCS/ti_res_count.c,v $ $Revision: 1.17 $";

#include <stdio.h>
#include <math.h>	/* for ceil */
#include <float.h>

#include "defs.h"
#include "erglob.h"
#include "mempool.h"
#include "libti.h"

#include "ti_res_count.h"
#include "ti_si.h"

/* Declare the TI_RES_COUNT opaque type (a context for resource counting):
 */
struct ti_res_count {
  INT32 bad_ii[TI_SI_BAD_II_SET_MAX+1];
  INT32 num_ops;
  double *vec;
};

/* TI_RES_COUNT accessors:
 */
#define TI_RES_COUNT_bad_ii(r,i)	((r)->bad_ii[(i)])
#define TI_RES_COUNT_vec(r,i)		((r)->vec[(i)])
#define TI_RES_COUNT_num_ops_add(r,n)	((r)->num_ops+=(n))
#define TI_RES_COUNT_num_ops_sub(r,n)	((r)->num_ops-=(n))
#define TI_RES_COUNT_num_ops_set(r,n)	((r)->num_ops=(n))
#define TI_RES_COUNT_num_ops(r)		((r)->num_ops+0)

/* ====================================================================
 *
 *  TI_RES_COUNT_Alloc
 *
 *  See interface description
 *
 * ====================================================================
 */
TI_RES_COUNT *
TI_RES_COUNT_Alloc(
  MEM_POOL *pool
)
{
  TI_RES_COUNT *counts = TYPE_MEM_POOL_ALLOC(TI_RES_COUNT, pool);
  counts->vec = TYPE_MEM_POOL_ALLOC_N(double, pool, TI_SI_resource_count);
  if ( !MEM_POOL_Zeroed(pool) ) {
    memset(counts->vec,    0, sizeof(double) * TI_SI_resource_count);
    memset(counts->bad_ii, 0, sizeof(counts->bad_ii));
    counts->num_ops = 0;
  }
  return counts;
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Min_Cycles
 *
 *  See interface description
 *
 * ====================================================================
 */
double
TI_RES_COUNT_Min_Cycles(
  TI_RES_COUNT    *res_counts
)
{
  INT32  i;
  double min_cycles = -1.0;
  double format_min_cycles = DBL_MAX;
  double min_issue_cycles = DBL_MAX;

  for ( i = 0; i < TI_SI_resource_count; ++i ) {
    double this_min =  (TI_RES_COUNT_vec(res_counts,i)
                      + SI_RESOURCE_ID_Avail_Per_Cycle(i) - 1)
                      / SI_RESOURCE_ID_Avail_Per_Cycle(i);
    if (TI_SI_resources[i]->format_id != TI_SI_INVALID_FORMAT_ID) {
      if ( this_min < format_min_cycles ) format_min_cycles = this_min;
    } else {
      if ( this_min > min_cycles ) min_cycles = this_min;
    }
  }
  min_issue_cycles =
	(TI_RES_COUNT_num_ops(res_counts)+TI_ISA_Max_Num_Slots()-1) /
	TI_ISA_Max_Num_Slots();
  if (format_min_cycles > min_issue_cycles)
    min_issue_cycles = format_min_cycles;
  if (min_issue_cycles > min_cycles)
    min_cycles = min_issue_cycles;
  return min_cycles;
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Min_II
 *
 *  See interface description
 *
 * ====================================================================
 */
INT32
TI_RES_COUNT_Min_II(
  TI_RES_COUNT    *res_counts
)
{
  INT32 min_ii;

  for ( min_ii = (INT32) ceil(TI_RES_COUNT_Min_Cycles(res_counts));
        min_ii <= TI_SI_BAD_II_SET_MAX;
        ++min_ii 
  ) {
    INT ops_with_bad_ii = TI_RES_COUNT_bad_ii(res_counts,min_ii);

    Is_True(ops_with_bad_ii >= 0, ("negative count of OPs with bad II"));
    if ( ops_with_bad_ii == 0 ) break;
  }

  return min_ii;
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Add_Op_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Add_Op_Resources(
  TI_RES_COUNT *res_counts,
  TOP           opcode
)
{
  INT                i;
  TOP                topcode = opcode;
  TI_SI_BAD_II_SET      bad_iis = TSI_Bad_IIs(topcode);
  TI_SI_RESOURCE_TOTAL* rt_vec  = TSI_Resource_Total_Vector(topcode);

  for ( i = 0; i < TSI_Resource_Total_Vector_Size(topcode); ++i ) {
    TI_SI_RESOURCE_ID id    = SI_RESOURCE_TOTAL_Resource_Id(rt_vec+i);
    UINT           count = SI_RESOURCE_TOTAL_Total_Used(rt_vec+i);

    TI_RES_COUNT_vec(res_counts,id) += count;
  }

  for ( i = SI_RR_Length(TSI_Resource_Requirement(topcode));
	i > 0;
	--i
  ) {
    if ( SI_BAD_II_SET_MemberP(bad_iis, i) ) {
      ++TI_RES_COUNT_bad_ii(res_counts, i);
    }
  }
  TI_RES_COUNT_num_ops_add(res_counts,1);
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Add_Op_Resources_Scaled
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Add_Op_Resources_Scaled(
  TI_RES_COUNT *res_counts,
  TOP           opcode,
  double	factor
)
{
  INT                i;
  TOP                topcode = opcode;
  TI_SI_BAD_II_SET      bad_iis = TSI_Bad_IIs(topcode);
  TI_SI_RESOURCE_TOTAL *rt_vec  = TSI_Resource_Total_Vector(topcode);

  for ( i = 0; i < TSI_Resource_Total_Vector_Size(topcode); ++i ) {
    TI_SI_RESOURCE_ID id    = SI_RESOURCE_TOTAL_Resource_Id(rt_vec+i);
    UINT           count = SI_RESOURCE_TOTAL_Total_Used(rt_vec+i);

    TI_RES_COUNT_vec(res_counts,id) += count * factor;
  }

  for ( i = SI_RR_Length(TSI_Resource_Requirement(topcode));
	i > 0;
	--i
  ) {
    if ( SI_BAD_II_SET_MemberP(bad_iis, i) ) {
      ++TI_RES_COUNT_bad_ii(res_counts, i);
    }
  }
  TI_RES_COUNT_num_ops_add(res_counts, (INT32) factor);
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Subtract_Op_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Subtract_Op_Resources(
  TI_RES_COUNT *res_counts,
  TOP           opcode
)
{
  INT                i;
  TOP                topcode = opcode;
  TI_SI_BAD_II_SET      bad_iis = TSI_Bad_IIs(topcode);
  TI_SI_RESOURCE_TOTAL *rt_vec = TSI_Resource_Total_Vector(topcode);

  for ( i = 0; i < TSI_Resource_Total_Vector_Size(topcode); ++i ) {
    TI_SI_RESOURCE_ID id    = SI_RESOURCE_TOTAL_Resource_Id(rt_vec+i);
    UINT           count = SI_RESOURCE_TOTAL_Total_Used(rt_vec+i);

    TI_RES_COUNT_vec(res_counts,id) -= count;
  }

  for ( i = SI_RR_Length(TSI_Resource_Requirement(topcode));
	i > 0;
	--i
  ) {
    if ( SI_BAD_II_SET_MemberP(bad_iis, i) ) {
      --TI_RES_COUNT_bad_ii(res_counts, i);
    }
  }
  TI_RES_COUNT_num_ops_sub(res_counts,1);
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Subtract_Op_Resources_Scaled
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Subtract_Op_Resources_Scaled(
  TI_RES_COUNT *res_counts,
  TOP           opcode,
  double	factor
)
{
  INT                i;
  TOP                topcode = opcode;
  TI_SI_BAD_II_SET      bad_iis = TSI_Bad_IIs(topcode);
  TI_SI_RESOURCE_TOTAL *rt_vec = TSI_Resource_Total_Vector(topcode);

  for ( i = 0; i < TSI_Resource_Total_Vector_Size(topcode); ++i ) {
    TI_SI_RESOURCE_ID id    = SI_RESOURCE_TOTAL_Resource_Id(rt_vec+i);
    UINT           count = SI_RESOURCE_TOTAL_Total_Used(rt_vec+i);

    TI_RES_COUNT_vec(res_counts,id) -= count * factor;
  }

  for ( i = SI_RR_Length(TSI_Resource_Requirement(topcode));
	i > 0;
	--i
  ) {
    if ( SI_BAD_II_SET_MemberP(bad_iis, i) ) {
      --TI_RES_COUNT_bad_ii(res_counts, i);
    }
  }
  TI_RES_COUNT_num_ops_sub(res_counts, (INT32) factor);
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Add
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Add(
  TI_RES_COUNT *sum,
  TI_RES_COUNT *addend1,
  TI_RES_COUNT *addend2
)
{
  const INT length = TI_SI_resource_count;
  INT i;

  for (i = 0; i < length; i++) {
    TI_RES_COUNT_vec(sum,i) =   TI_RES_COUNT_vec(addend1,i)
			      + TI_RES_COUNT_vec(addend2,i);
  }

  for (i = 0; i <= TI_SI_BAD_II_SET_MAX; i++) {
    TI_RES_COUNT_bad_ii(sum,i) =   TI_RES_COUNT_bad_ii(addend1,i)
				 + TI_RES_COUNT_bad_ii(addend2,i);
  }
  TI_RES_COUNT_num_ops_set(sum,TI_RES_COUNT_num_ops(addend1));
  TI_RES_COUNT_num_ops_add(sum,TI_RES_COUNT_num_ops(addend2));
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Subtract
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Subtract(
  TI_RES_COUNT *difference,
  TI_RES_COUNT *minuend,
  TI_RES_COUNT *subtrahend
)
{
  const INT length = TI_SI_resource_count;
  INT i;

  for (i = 0; i < length; i++) {
    TI_RES_COUNT_vec(difference,i) =   TI_RES_COUNT_vec(minuend,i)
				     - TI_RES_COUNT_vec(subtrahend,i);
  }

  for (i = 0; i <= TI_SI_BAD_II_SET_MAX; i++) {
    TI_RES_COUNT_bad_ii(difference,i) =   TI_RES_COUNT_bad_ii(minuend,i)
				        - TI_RES_COUNT_bad_ii(subtrahend,i);
  }
  TI_RES_COUNT_num_ops_set(difference,TI_RES_COUNT_num_ops(minuend));
  TI_RES_COUNT_num_ops_sub(difference,TI_RES_COUNT_num_ops(subtrahend));
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Print
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Print(
  FILE *fp,
  TI_RES_COUNT *res
)
{
  TI_SI_RESOURCE_ID i;

  fprintf(fp, "TI_RES_COUNT(");
  fprintf(fp, "max issue %d/%d ", TI_RES_COUNT_num_ops(res),
				  TI_ISA_Max_Num_Slots());
  for (i = 0; i < TI_SI_resource_count; i++) {
    if (TI_RES_COUNT_vec(res, i) > 0.0) {
      fprintf(fp, ", %s %G", SI_RESOURCE_ID_Name(i), TI_RES_COUNT_vec(res, i));
      if (SI_RESOURCE_ID_Avail_Per_Cycle(i) > 1)
	fprintf(fp, "/%d", SI_RESOURCE_ID_Avail_Per_Cycle(i));
    }
  }
  fprintf(fp, ")");
  fflush(fp);
}


/* ====================================================================
 *
 *  TI_RES_COUNT_Emit_Note
 *
 *  See interface description
 *
 * ====================================================================
 */
void
TI_RES_COUNT_Emit_Note(
  const char *prefix,		       
  FILE *fp,
  TI_RES_COUNT *res,
  INT ii
)
{
  TI_SI_RESOURCE_ID i;
  for (i = 0; i < TI_SI_resource_count; i++) {
    if (TI_RES_COUNT_vec(res, i) > 0.0 && SI_RESOURCE_ID_Avail_Per_Cycle(i) > 0) {
      INT usage = (INT) (TI_RES_COUNT_vec(res, i) * 100.0 /
			 (SI_RESOURCE_ID_Avail_Per_Cycle(i) * ii));
      fprintf(fp, "%s%d %s units ( %d%% of peak )\n",
	      prefix, (INT) TI_RES_COUNT_vec(res, i),  SI_RESOURCE_ID_Name(i), usage);
    }
  }
}
