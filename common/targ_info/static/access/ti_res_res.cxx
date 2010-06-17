
/*
   Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.
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



#include <limits.h>
#include "defs.h"
#include "erglob.h"
#include "mempool.h"
#include "bitset.h"
#include "libti.h"

#include "ti_res_res.h"
#include "ti_si.h"
#include "tracing.h"

/* Declare the TI_RES_RES opaque type (a context for resource reservation
 * actitivies):
 */
struct ti_res_res {
  MEM_POOL *pool;		/* For our dynamic memory needs		     */
  TI_SI_RRW   *rrtab;		/* First cycle (word) of table		     */
  TI_SI_FORMAT_ID_SET   *fmttab;/* First cycle (word) of format table	     */
  UINT64   *fmt_vote_tab;	/* First cycle (word) of format vote table   */
				/* we allot TI_TIE_SLOTS_MAX_BITS bits for
				 * each format/bundle
				 * so we allow at most 10 formats and each
				 * can have at most TI_TIE_SLOTS_MAX ops     */
  UINT32   *op_count;		/* First cycle (word) of op count table      */
  INT32     length;		/* Current length of the table		     */
  INT32     alloc_size;		/* Its allocated size (in cycles)	     */
  BOOL      cyclic;		/* Is this a schedule for a loop, e.g.	     */
				/* modulo scheduling for software pipelining */

  /* Fields used only for cyclic scheduling:
   */
  BS       *si_ids;		/* Set of the scheduling information IDs     */
				/* being scheduled                           */
  UINT      min_rr_length;	/* minimum length of any or the RRs being    */
				/* scheduled.				     */
  TI_SI_RESOURCE_ID_SET *uncommon_res_ids;
  				/* ith element is a set of resource IDs used */
				/* in the ith cycle by some, but not all the */
				/* opcodes being scheduled.                  */
  TI_SI_BAD_II_SET bad_iis;	/* Impossible IIs given the opcodes being    */
				/* scheduled				     */
};

/* TI_RES_RES accessors:
 */
#define TI_RES_RES_pool(t)		((t)->pool)
#define TI_RES_RES_rrtab(t)		((t)->rrtab)
#define TI_RES_RES_fmttab(t)		((t)->fmttab)
#define TI_RES_RES_fmt_vote_tab(t)	((t)->fmt_vote_tab)
#define TI_RES_RES_op_count(t)		((t)->op_count)
#define TI_RES_RES_length(t)		((t)->length)
#define TI_RES_RES_alloc_size(t)	((t)->alloc_size)
#define TI_RES_RES_cyclic(t)		((t)->cyclic)
#define TI_RES_RES_si_ids(t)		((t)->si_ids)
#define TI_RES_RES_min_rr_length(t)	((t)->min_rr_length)
#define TI_RES_RES_uncommon_res_ids(t)	((t)->uncommon_res_ids)
#define TI_RES_RES_bad_iis(t)		((t)->bad_iis)

static BOOL Trace_RES = FALSE;
static BOOL Trace_RES_detail = FALSE;


/* ====================================================================
 *
 *  Cycle_Mod_II
 *
 *  Return the cycle number modulo the II. See be/cg/swp_ii_funcs.[ch]
 *  for a discussion of why we just don't use the remainder operator
 *  (performance).
 *
 *  TODO: in general the cycle should be close, so the simple loop is
 *  probably good enough. Verify that this is true.
 *
 * ====================================================================
 */
static INT32
Cycle_Mod_II(
  INT32 cyc,
  INT32 ii
)
{
  if ( cyc < 0 ) {
    do {
      cyc += ii;
    } while (cyc < 0);
  } else if (cyc >= ii) {
    do {
      cyc -= ii;
    } while (cyc >= ii);
  }

  return cyc;
}


/* ====================================================================
 *
 *  TI_RES_RES_Has_TOP
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Has_TOP(
  TI_RES_RES *res,
  TOP         opcode
)
{
  if ( !BS_MemberP(TI_RES_RES_si_ids(res), TSI_Id(opcode) ) ) {
    UINT rr_length;

    TI_RES_RES_si_ids(res) = BS_Union1D(TI_RES_RES_si_ids(res),
					TSI_Id(opcode),
					TI_RES_RES_pool(res));
    TI_RES_RES_bad_iis(res) = SI_BAD_II_SET_Union(TI_RES_RES_bad_iis(res),
						  TSI_Bad_IIs(opcode));

    rr_length = SI_RR_Length(TSI_Resource_Requirement(opcode));
    if ( rr_length < TI_RES_RES_min_rr_length(res) ) {
      TI_RES_RES_min_rr_length(res) = rr_length;
    }
  }
}


/* Set up some constrol variables common to Resource_Available,
 * Reserve_Resources, and Unreserve_Resources.  The idea is to break the
 * work into two loops and obviate the need to do a Mod operation for every
 * cycle checked.  The first should check <length1> cycles of <rr>, starting
 * with cycle 0 of <rr> and cycle <cycle_mod_ii> of the schedule.  The
 * second loop should theck <length1> cycles of <rr>, starting with cycle
 * <length1> of <rr> and cycle 0 of the schedule.
 */
static void Check_Reserve_Loop_Control(
  TI_RES_RES *res,
  TOP         opcode,
  TOP         alt_opcode,
  INT         cycle,
  TI_SI_RR      *rr,
  TI_SI_RR      *alt_rr,
  INT        *length1,
  INT        *length2,
  INT        *cycle_mod_ii
)
{
  INT32 rr_length;
  INT32 length = TI_RES_RES_length(res);

  if ( TI_RES_RES_cyclic(res) ) {
    *rr = TSI_II_Resource_Requirement(opcode,length);
  }
  else {
    *rr = TSI_Resource_Requirement(opcode);
  }
  *cycle_mod_ii = Cycle_Mod_II(cycle,length);

  rr_length = SI_RR_Length(*rr);
  if (alt_opcode != TOP_UNDEFINED) {
    if ( TI_RES_RES_cyclic(res) ) {
      *alt_rr = TSI_II_Resource_Requirement(alt_opcode,length);
    }
    else {
      *alt_rr = TSI_Resource_Requirement(alt_opcode);
    }
  }
  if ( *cycle_mod_ii + rr_length <= length ) {
    *length1 = rr_length;
    *length2 = 0;
  }
  else {
    *length1 = length - *cycle_mod_ii;
    *length2 = rr_length - *length1;
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Set_BB_Cycle_Count
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Set_BB_Cycle_Count(
  TI_RES_RES  *res,
  INT          length
)
{
  INT  i;
  BOOL cyclic = TI_RES_RES_cyclic(res);

  if ( length > TI_RES_RES_alloc_size(res) ) {
    INT new_alloc_size = length * 2;
    TI_RES_RES_alloc_size(res) = new_alloc_size;

    TI_RES_RES_rrtab(res) = TYPE_MEM_POOL_ALLOC_N(TI_SI_RRW,
					TI_RES_RES_pool(res),
					new_alloc_size);
    TI_RES_RES_fmttab(res) = TYPE_MEM_POOL_ALLOC_N(TI_SI_FORMAT_ID_SET,
					TI_RES_RES_pool(res),
					new_alloc_size);
    TI_RES_RES_fmt_vote_tab(res) = TYPE_MEM_POOL_ALLOC_N(UINT64,
					TI_RES_RES_pool(res),
					new_alloc_size);
    TI_RES_RES_op_count(res) = TYPE_MEM_POOL_ALLOC_N(UINT32,
					TI_RES_RES_pool(res),
					new_alloc_size);
    if ( cyclic ) {
      TI_RES_RES_uncommon_res_ids(res)
	= TYPE_MEM_POOL_ALLOC_N(TI_SI_RESOURCE_ID_SET,
				TI_RES_RES_pool(res),
				new_alloc_size);
    }
  }

  TI_RES_RES_length(res) = length;

  /* Initialize the part of the table we will use
   */
  for ( i = 0; i < length; ++i ) {
    TI_RES_RES_rrtab(res)[i].init(SI_RRW_Initial(), TI_RES_RES_pool(res));
    TI_RES_RES_fmttab(res)[i] = ~0;
    TI_RES_RES_fmt_vote_tab(res)[i] = 0;
    TI_RES_RES_op_count(res)[i] = 0;
  }

  if ( cyclic ) {
    INT id;
    BS *si_ids = TI_RES_RES_si_ids(res);
    TI_SI_RESOURCE_ID_SET *uncommon_res_ids = TI_RES_RES_uncommon_res_ids(res);
    INT common_length = MIN(TI_RES_RES_min_rr_length(res), length);

    /* For each cycle, compute the set of resources that not all the OPs in
     * the loop use in that cycle.  We do this by computing its complement --
     * the set of resources that all the OPs use in the cycle -- and then
     * complementing it in place.
     */

    /* Compute common resources into "uncommon_res_ids"
     *
     * NOTE: the following loop also initializes the "res_ids"
     * from common_length to the end of the vector. These
     * are by definition not common to all OPs, and we will leave
     * the setting unchanged in the following loops.
     */
    for ( i = 0; i < length; ++i ) {
      uncommon_res_ids[i].init(
		SI_RESOURCE_ID_SET_Universe(), TI_RES_RES_pool(res));
    }

    for ( id = BS_Choose(si_ids); id != BS_CHOOSE_FAILURE;
                                  id = BS_Choose_Next(si_ids,id)
    ) {
      const TI_SI_RESOURCE_ID_SET* resource_ids_used
        = SI_ID_II_Cycle_Resource_Ids_Used(id,length);

      for ( i = 0; i < common_length; ++i ) {
        uncommon_res_ids[i]
          = SI_RESOURCE_ID_SET_Intersection(uncommon_res_ids[i],
                                            resource_ids_used[i]);
      }
    }

    /* Complement in place
     */
    for ( i = 0; i < common_length; ++i ) {
      uncommon_res_ids[i] =
        SI_RESOURCE_ID_SET_Complement(uncommon_res_ids[i]);
    }
  }
}


void TI_RES_RES_Clear(
  TI_RES_RES  *res
)
{
  INT  i;
  INT length = TI_RES_RES_length(res);

  FmtAssert (!TI_RES_RES_cyclic(res),
  	("TI_RES_RES_Clear undefined for cyclic schedules"));

  /* Initialize the part of the table we will use
   */
  for ( i = 0; i < length; ++i ) {
    TI_RES_RES_rrtab(res)[i] = SI_RRW_Initial();
    TI_RES_RES_fmttab(res)[i] = ~0;
    TI_RES_RES_fmt_vote_tab(res)[i] = 0;
    TI_RES_RES_op_count(res)[i] = 0;
  }
}

/* ====================================================================
 *
 *  TI_RES_RES_Alloc
 *
 *  See interface description
 *
 * ====================================================================
 */
TI_RES_RES *TI_RES_RES_Alloc(
  BOOL      cyclic,
  MEM_POOL *pool
)
{
  TI_RES_RES *res = TYPE_MEM_POOL_ALLOC(TI_RES_RES, pool);

  /* we use UINT64 for the fmt_vote_table which can handle up to
     10 formats each with TI_TIE_SLOTS_MAX ops
  */
  FmtAssert(TI_ISA_Num_Bundles()*TI_TIE_SLOTS_MAX_BITS<=sizeof(UINT64)*8,
	    ("Limit on number of bundle (10) exceeded"));

  TI_RES_RES_pool(res) = pool;
  TI_RES_RES_cyclic(res) = cyclic;
  TI_RES_RES_bad_iis(res) = SI_BAD_II_SET_Empty();
  TI_RES_RES_length(res) = 0;
  TI_RES_RES_alloc_size(res) = 0;
  TI_RES_RES_min_rr_length(res) = UINT_MAX;

  Trace_RES = Get_Trace(TP_SCHED, 0x0002, 0);
  Trace_RES_detail = Get_Trace(TP_SCHED, 0x0004, 0);

  if ( cyclic ) {
    TI_RES_RES_si_ids(res) = BS_Create_Empty(SI_ID_Count(), pool);
  }

  return res;
}

static void Print_Overuse(TI_SI_RRW overuse, TOP opcode, INT cycle) {

  TI_SI_RRW mask = *TI_SI_RRW_overuse_mask;
  int resource_index = 0;
  overuse &= mask;
  int i=0;
  while (overuse.is_zero() == false) {

    if (overuse.has_bit(i))
      fprintf(TFile,
	"RES: opcode \"%s\" has resource (%s) conflict with ops at cycle %d\n",
	TI_TOP_Name(opcode), TI_SI_resources[resource_index]->name, cycle);

    overuse.reset_bit(i);

    if (mask.has_bit(i)) {
      resource_index++;
      do {
	overuse.reset_bit(i);
	mask.reset_bit(i);
	i++;
      } while (mask.has_bit(i));
    } else
      i++;

  }
}


static void Print_Cannot_Bundle(TI_SI_FORMAT_ID_SET formats, TOP opcode, INT cycle) {

  int fmt = 0;
  TI_SI_FORMAT_ID_SET opcode_formats;

  if (formats==0) {
    fprintf(TFile, "RES: No possible bundle format at cycle %d\n", cycle);
    return;
  }

  opcode_formats= TI_SI_top_si[opcode]->valid_issue_formats;
  fprintf(TFile, "RES: opcode \"%s\" has incompatible format(s)",
	TI_TOP_Name(opcode));
  while (opcode_formats != 0) {

    if (opcode_formats & 1)
      fprintf(TFile, " %s", TI_ISA_Exec_Asmname(fmt));

    opcode_formats >>= 1;
    fmt++;

  }

  fmt = 0;
  fprintf(TFile, " at cycle %d with existing format(s)", cycle);
  while (formats != 0) {

    if (formats & 1)
      fprintf(TFile, " %s", TI_ISA_Exec_Asmname(fmt));

    formats >>= 1;
    fmt++;

  }
  fprintf(TFile, "\n");
}

static void Print_Opcode_Bundle(
  char* msg,
  TI_SI_FORMAT_ID_SET opcode_format,
  TI_SI_RRW opcode_format_resource,
  TOP opcode, INT cycle) {

  fprintf(TFile,
	"RES(%s): attempt bundling for opcode \"%s\" at cycle %d :\n",
	msg, TI_TOP_Name(opcode), cycle);
  fprintf(TFile,
	"     opcode_format = 0x%08x, opcode_format_resource = ", opcode_format);
  opcode_format_resource.dump_hex(TFile);
  fputc('\n', TFile);
}

static void Print_Cycle_Bundle(
  char* msg,
  TI_SI_FORMAT_ID_SET compatible_format,
  TI_SI_RRW format_resource, INT cycle) {

  fprintf(TFile,
	"RES(%s): bundle info at cycle %d :\n",
	msg, cycle);
  fprintf(TFile,
	"     compatible_format = 0x%08x, format_resource = init + ", compatible_format);
  TI_SI_RRW tmp = format_resource - SI_RRW_Initial();
  tmp.dump_hex(TFile);
  fprintf(TFile, " = ");
  format_resource.dump_hex(TFile);
  fputc('\n', TFile);
}

/* ====================================================================
 *
 *  TI_RES_RES_Compatible_Format
 *
 *  See interface description
 *
 * ====================================================================
 */
TI_SI_FORMAT_ID_SET TI_RES_RES_Compatible_Format(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle,
  TOP          alt_opcode
)
{
  TI_SI_FORMAT_ID_SET valid_formats = TI_RES_RES_Valid_Formats(res, cycle);
  TI_SI_FORMAT_ID_SET compatible_format = 0;

  compatible_format =
	valid_formats & TI_SI_top_si[opcode]->valid_issue_formats;
  if (alt_opcode != TOP_UNDEFINED)
    compatible_format |=
        valid_formats & TI_SI_top_si[alt_opcode]->valid_issue_formats;

  return compatible_format;
}

/* ====================================================================
 *
 *  TI_RES_RES_Resources_Available
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Available(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle,
  TOP          alt_opcode
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i,j,length;
  TI_SI_RR   rr, alt_rr;
  TI_SI_RRW *rrtab = TI_RES_RES_rrtab(res);
  TI_SI_FORMAT_ID_SET *fmttab = TI_RES_RES_fmttab(res);
  TI_SI_FORMAT_ID_SET compatible_format = 0;

  Check_Reserve_Loop_Control(res,opcode,alt_opcode,cycle,
			     &rr,&alt_rr,&length1,&length2,&cycle_mod_ii);

  if (Trace_RES_detail) {
    Print_Cycle_Bundle("avail",fmttab[cycle_mod_ii], rrtab[cycle_mod_ii], cycle);
    Print_Opcode_Bundle("avail",TI_SI_top_si[opcode]->valid_issue_formats,
		 SI_RR_Cycle_RRW(rr,0), opcode, cycle);
  }

  compatible_format = TI_RES_RES_Compatible_Format(res,opcode,cycle,alt_opcode);

  if (compatible_format == 0) {
    if (Trace_RES)
      Print_Cannot_Bundle(fmttab[cycle_mod_ii],opcode, cycle);
    return FALSE;
  }

  {
    TI_SI_RRW reserved = SI_RRW_Reserve(rrtab[cycle_mod_ii],
					SI_RR_Cycle_RRW(rr,0));
    TI_SI_RRW non_format_overuse;
    TI_SI_RRW format_overuse;
    if (alt_opcode != TOP_UNDEFINED)
      reserved =
	SI_RRW_Reserve(rrtab[cycle_mod_ii],
		       SI_RR_Cycle_RRW(rr,0) | SI_RR_Cycle_RRW(alt_rr,0));
    non_format_overuse =
	    reserved & ~(*TI_SI_RRW_all_format_resource_overuse_mask);
    format_overuse = reserved & *TI_SI_RRW_all_format_resource_overuse_mask;

    if ( SI_RRW_Has_Overuse(non_format_overuse) ) {
      if (Trace_RES) Print_Overuse(non_format_overuse,opcode,cycle);
      return FALSE;
    }

    for (i=0; i<TI_ISA_Num_Bundles(); i++) {
      TI_SI_RRW format_overuse_tmp =
		format_overuse & TI_SI_RRW_format_resource_overuse_mask[i];
      if ( format_overuse_tmp.is_zero() == false )
	compatible_format &= ~((TI_SI_FORMAT_ID_SET)1<<i);
    }
    if (compatible_format == 0) {
      if (Trace_RES) {
	Print_Overuse(format_overuse,opcode,cycle);
      }
      return FALSE;
    }
  }

  for ( i = 1; i < length1; ++i ) {
    TI_SI_RRW non_format_overuse;
    TI_SI_RRW reserved = rrtab[cycle_mod_ii+i];
    reserved = SI_RRW_Reserve(reserved, SI_RR_Cycle_RRW(rr,i));
    non_format_overuse =
	    reserved & ~(*TI_SI_RRW_all_format_resource_overuse_mask);
    if ( SI_RRW_Has_Overuse(non_format_overuse) ) {
      if (Trace_RES) Print_Overuse(non_format_overuse,opcode,cycle+i);
      return FALSE;
    }
  }

  length = TI_RES_RES_length(res);
  j=0;
  while (j<length2) {
    for ( i = 0; i<length && i+j < length2; ++i ) {
      TI_SI_RRW non_format_overuse;
      TI_SI_RRW reserved = rrtab[i];
      reserved = SI_RRW_Reserve(reserved, SI_RR_Cycle_RRW(rr,i+j+length1));
      non_format_overuse =
	      reserved & ~(*TI_SI_RRW_all_format_resource_overuse_mask);
      if ( SI_RRW_Has_Overuse(non_format_overuse) ) {
	if (Trace_RES) Print_Overuse(non_format_overuse,opcode,cycle+i+j+length1);
	return FALSE;
      }
    }
    j+=length;
  }

  return TRUE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Reserve_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Reserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle,
  TOP          alt_opcode
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i,j,length;
  TI_SI_RR   rr,alt_rr;
  TI_SI_RRW *rrtab = TI_RES_RES_rrtab(res);
  TI_SI_FORMAT_ID_SET *fmttab = TI_RES_RES_fmttab(res);
  UINT64 *fmt_vote_tab = TI_RES_RES_fmt_vote_tab(res);
  UINT32 *op_count = TI_RES_RES_op_count(res);
  TI_SI_FORMAT_ID_SET compatible_format = 0;

  Check_Reserve_Loop_Control(res,opcode,alt_opcode,cycle,
			     &rr,&alt_rr,&length1,&length2,&cycle_mod_ii);

  if (Trace_RES_detail) {
    Print_Cycle_Bundle("reserve",fmttab[cycle_mod_ii], rrtab[cycle_mod_ii], cycle);
    Print_Opcode_Bundle("reserve",TI_SI_top_si[opcode]->valid_issue_formats,
		 SI_RR_Cycle_RRW(rr,0), opcode, cycle);
  }
  compatible_format = TI_RES_RES_Compatible_Format(res,opcode,cycle,alt_opcode);

  {
    TI_SI_RRW reserved = SI_RRW_Reserve(rrtab[cycle_mod_ii],
					SI_RR_Cycle_RRW(rr,0));
    TI_SI_RRW format_overuse;

    if (alt_opcode != TOP_UNDEFINED)
      reserved =
	SI_RRW_Reserve(rrtab[cycle_mod_ii],
		       SI_RR_Cycle_RRW(rr,0) | SI_RR_Cycle_RRW(alt_rr,0));
    format_overuse =
		reserved & *TI_SI_RRW_all_format_resource_overuse_mask;

    for (i=0; i<TI_ISA_Num_Bundles(); i++) {
      TI_SI_RRW format_overuse_tmp =
		format_overuse & TI_SI_RRW_format_resource_overuse_mask[i];
      if ( format_overuse_tmp.is_zero() == false ) {
	compatible_format &= ~((TI_SI_FORMAT_ID_SET)1<<i);
      }
    }
    rrtab[cycle_mod_ii] = reserved;
  }

  fmttab[cycle_mod_ii] = compatible_format;
  op_count[cycle_mod_ii]++;
  FmtAssert(op_count[cycle_mod_ii]<=TI_TIE_SLOTS_MAX,
	    ("Too many (>%d) ops scheduled at one cycle",TI_TIE_SLOTS_MAX));

  {

    TI_SI_FORMAT_ID_SET bundle_pos;
    long long scaled_bundle_pos;
    TI_SI_FORMAT_ID_SET valid_formats =
      TI_SI_top_si[opcode]->valid_issue_formats;
    if (alt_opcode != TOP_UNDEFINED)
      valid_formats |= TI_SI_top_si[alt_opcode]->valid_issue_formats;

    bundle_pos = 1LL;
    scaled_bundle_pos = 1LL;
    for (i=0; i<TI_ISA_Num_Bundles();
	 i++,bundle_pos <<= 1,scaled_bundle_pos <<=TI_TIE_SLOTS_MAX_BITS) {
      if (valid_formats & (bundle_pos))
        fmt_vote_tab[cycle_mod_ii] += scaled_bundle_pos;
    }
  }

  for ( i = 1; i < length1; ++i ) {
    rrtab[cycle_mod_ii+i]
      = SI_RRW_Reserve(rrtab[cycle_mod_ii+i],SI_RR_Cycle_RRW(rr,i));
  }

  length = TI_RES_RES_length(res);
  j=0;
  while (j<length2) {
    for ( i = 0; i<length && i+j < length2; ++i ) {
      rrtab[i] = SI_RRW_Reserve(rrtab[i],SI_RR_Cycle_RRW(rr,i+j+length1));
    }
    j+=length;
  }
  if (Trace_RES_detail) {
    Print_Cycle_Bundle("reserved",fmttab[cycle_mod_ii], rrtab[cycle_mod_ii], cycle);
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Unreserve_Resources
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_RES_RES_Unreserve_Resources(
  TI_RES_RES  *res,
  TOP          opcode,
  INT          cycle,
  TOP          alt_opcode
)
{
  INT     cycle_mod_ii;
  INT     length1;
  INT     length2;
  INT     i,j,length;
  UINT32  count;
  TI_SI_RR   rr,alt_rr;
  TI_SI_RRW *rrtab = TI_RES_RES_rrtab(res);
  TI_SI_FORMAT_ID_SET *fmttab = TI_RES_RES_fmttab(res);
  UINT64 *fmt_vote_tab = TI_RES_RES_fmt_vote_tab(res);
  UINT32 *op_count = TI_RES_RES_op_count(res);
  TI_SI_FORMAT_ID_SET compatible_format = 0;

  Check_Reserve_Loop_Control(res,opcode,alt_opcode,cycle,
			     &rr,&alt_rr,&length1,&length2,&cycle_mod_ii);

  if (Trace_RES_detail) {
    Print_Cycle_Bundle("unreserve",fmttab[cycle_mod_ii], rrtab[cycle_mod_ii], cycle);
    Print_Opcode_Bundle("unreserve",TI_SI_top_si[opcode]->valid_issue_formats,
		 SI_RR_Cycle_RRW(rr,0), opcode, cycle);
  }
  compatible_format = fmttab[cycle_mod_ii];
  op_count[cycle_mod_ii]--;
  count = op_count[cycle_mod_ii];

  for ( i = 0; i < length1; ++i ) {
    if ((i==0) && (alt_opcode != TOP_UNDEFINED))
      rrtab[cycle_mod_ii+i]
	= SI_RRW_Unreserve(rrtab[cycle_mod_ii+i],
			   SI_RR_Cycle_RRW(rr,i) | SI_RR_Cycle_RRW(alt_rr,i));
    else
      rrtab[cycle_mod_ii+i]
	= SI_RRW_Unreserve(rrtab[cycle_mod_ii+i],SI_RR_Cycle_RRW(rr,i));
  }

  length = TI_RES_RES_length(res);
  j=0;
  while (j<length2) {
    for ( i = 0; i<length && i+j < length2; ++i ) {
      rrtab[i] = SI_RRW_Unreserve(rrtab[i],SI_RR_Cycle_RRW(rr,i+j+length1));
    }
    j+=length;
  }

  /* we need to re-compute the compatible format after the resource usage
   * is updated above so we get updated format resource usage
   */
  {

    TI_SI_FORMAT_ID_SET bundle_pos;
    long long scaled_bundle_pos;
    long long scaled_bundle_mask;
    TI_SI_FORMAT_ID_SET valid_formats =
      TI_SI_top_si[opcode]->valid_issue_formats;
    if (alt_opcode != TOP_UNDEFINED)
      valid_formats |= TI_SI_top_si[alt_opcode]->valid_issue_formats;

    bundle_pos = 1LL;
    scaled_bundle_pos = 1LL;
    scaled_bundle_mask = (1LL << TI_TIE_SLOTS_MAX_BITS) - 1;
    for (i=0; i<TI_ISA_Num_Bundles();
	 i++,bundle_pos <<= 1,scaled_bundle_pos <<=TI_TIE_SLOTS_MAX_BITS) {
      if (valid_formats & (bundle_pos))
        fmt_vote_tab[cycle_mod_ii] -= scaled_bundle_pos;
      TI_SI_RRW tmp_rrw =
	   (rrtab[cycle_mod_ii] & TI_SI_RRW_format_resource_overuse_mask[i]);
      if ((compatible_format & (bundle_pos))==0			&&
	   ((fmt_vote_tab[cycle_mod_ii])>>(i*TI_TIE_SLOTS_MAX_BITS) &
	    scaled_bundle_mask)==count	&& tmp_rrw.is_zero()) {
	/* restore compatible formats */
	fmttab[cycle_mod_ii] |= bundle_pos;
      }
    }
  }

  if (Trace_RES_detail) {
    Print_Cycle_Bundle("unreserved",fmttab[cycle_mod_ii], rrtab[cycle_mod_ii], cycle);
  }
}


/* ====================================================================
 *
 *  TI_RES_RES_Valid_Formats
 *
 *  See interface description
 *
 * ====================================================================
 */
TI_SI_FORMAT_ID_SET TI_RES_RES_Valid_Formats(
  TI_RES_RES  *res,
  INT          cycle
)
{
  TI_SI_FORMAT_ID_SET *fmttab = TI_RES_RES_fmttab(res);
  INT32 length = TI_RES_RES_length(res);
  cycle = Cycle_Mod_II(cycle,length);
  return fmttab[cycle];
}

/* ====================================================================
 *
 *  TI_RES_RES_Is_Bad_II
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Is_Bad_II(
  TI_RES_RES  *res,
  INT          ii
)
{
  return SI_BAD_II_SET_MemberP(TI_RES_RES_bad_iis(res),ii);
}


/* ====================================================================
 *
 *  TI_RES_RES_Resources_Relevant
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Relevant(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2,
  INT          offset
)
{
  INT length1, length2, i;
  const INT32 length = TI_RES_RES_length(res);
  const TI_SI_RESOURCE_ID_SET *const res_ids1
    = TSI_II_Cycle_Resource_Ids_Used(opcode1,length);
  const TI_SI_RESOURCE_ID_SET *const res_ids2
    = TSI_II_Cycle_Resource_Ids_Used(opcode2,length);
  const INT rr1_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode1,length));
  const INT rr2_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode2,length));
  INT offset_mod_ii = Cycle_Mod_II(offset,length);
  const TI_SI_RESOURCE_ID_SET *const uncommon_res_ids
    = TI_RES_RES_uncommon_res_ids(res);

  FmtAssert (TI_RES_RES_cyclic(res),
  	("TI_RES_RES_Resources_Relevant not applicable to non-cyclic schedules"));

  /* Check from the start of rr2 until either the end of rr2 or the end of rr1 */
  /* for the portion of schedule info where no uncommon resource is available from
   * the resource table (since the table is too short), we assume all resources are
   * uncommon
   */
  length1 = rr1_length - offset_mod_ii;
  if ( rr2_length < length1 ) length1 = rr2_length;

  for ( i = 0; i < length1; ++i ) {

    TI_SI_RESOURCE_ID_SET res_id1 = (i+offset_mod_ii<length)?
	    (res_ids1[i + offset_mod_ii] & uncommon_res_ids[i + offset_mod_ii]) :
	    res_ids1[i + offset_mod_ii];
    TI_SI_RESOURCE_ID_SET res_id2 = (i<length)?
	    (res_ids2[i] & uncommon_res_ids[i]) : res_ids2[i];
    if ( SI_RESOURCE_ID_SET_Intersection_Non_Empty(res_id1, res_id2))
    {
      return TRUE;
    }
  }

  /* since the schedule is cyclic, we can view rr1 as scheduled with 'length-offset_mod_ii'
   * offset relative to the start of rr2 then do the similar check above
   */
  offset_mod_ii = length - offset_mod_ii;

  /* Check from the start of rr1 until either the end of rr1 or the end of rr2 */
  length2 = rr2_length - offset_mod_ii;
  if ( rr1_length < length2 ) length2 = rr1_length;

  for ( i = 0; i < length2; ++i ) {

    TI_SI_RESOURCE_ID_SET res_id2 = (i+offset_mod_ii<length)?
	    (res_ids2[i + offset_mod_ii] & uncommon_res_ids[i + offset_mod_ii]) :
	    res_ids2[i + offset_mod_ii];
    TI_SI_RESOURCE_ID_SET res_id1 = (i<length)?
	    (res_ids1[i] & uncommon_res_ids[i]) : res_ids1[i];
    if ( SI_RESOURCE_ID_SET_Intersection_Non_Empty(res_id2, res_id1))
    {
      return TRUE;
    }
  }

  return FALSE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Resources_Equivalent
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Equivalent(
  TI_RES_RES  *res,
  TOP          opcode1,
  TOP          opcode2
)
{
  INT i;
  const INT32 length = TI_RES_RES_length(res);
  TI_SI_RR rr1 = TSI_II_Resource_Requirement(opcode1,length);
  TI_SI_RR rr2 = TSI_II_Resource_Requirement(opcode2,length);

  if ( rr1 == rr2 ) return TRUE;

  if ( SI_RR_Length(rr1) != SI_RR_Length(rr2) ) return FALSE;

  for (i = 0; i < SI_RR_Length(rr1); ++i) {
    if ( SI_RR_Cycle_RRW(rr1,i) != SI_RR_Cycle_RRW(rr2,i) )
      return FALSE;
  }

  return TRUE;
}


/* ====================================================================
 *
 *  TI_RES_RES_Resource_Grainy
 *
 *  See interface description
 *
 * ====================================================================
 */
BOOL TI_RES_RES_Resources_Grainy(
  TI_RES_RES  *res,
  TOP          opcode
)
{
  INT i;
  const INT32 length = TI_RES_RES_length(res);
  TI_SI_RESOURCE_ID_SET *uncommon_res_ids = TI_RES_RES_uncommon_res_ids(res);
  UINT min_rr_length = TI_RES_RES_min_rr_length(res);
  const TI_SI_RESOURCE_ID_SET* res_used
    = TSI_II_Cycle_Resource_Ids_Used(opcode,length);
  INT res_used_length
    = SI_RR_Length(TSI_II_Resource_Requirement(opcode,length));

  if ( min_rr_length < res_used_length ) return TRUE;

  for ( i = 0; i < min_rr_length; ++i ) {
    if ( SI_RESOURCE_ID_SET_Intersection_Non_Empty(res_used[i],
                                                   uncommon_res_ids[i])
    ) {
      return TRUE;
    }
  }

  return FALSE;
}


INT TI_RES_RES_Resources_Length(
  TI_RES_RES  *res,
  TOP          opcode
)
{
  return SI_RR_Length(TSI_Resource_Requirement(opcode));
}


void TI_RES_RES_Print(FILE *fp, TI_RES_RES *res)
{
  INT i;
  for (i = 0; i < TI_RES_RES_length(res); i++)
    fprintf(fp, "%d --> ", i);
    TI_RES_RES_rrtab(res)[i].dump_hex(fp);
    fputc('\n', fp);
}


BOOL TI_RES_RES_Equal(TI_RES_RES *res1, TI_RES_RES *res2)
{
  INT i;
  if (TI_RES_RES_length(res1) != TI_RES_RES_length(res2)) return FALSE;
  for (i = 0; i < TI_RES_RES_length(res1); i++)
    if (TI_RES_RES_rrtab(res1)[i] != TI_RES_RES_rrtab(res2)[i])
      return FALSE;
  return TRUE;
}

