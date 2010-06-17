
/*
   Copyright (C) 2004-2005 Tensilica, Inc.  All Rights Reserved.
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


// isa_lits_gen.cxx
/////////////////////////////////////
//
//  Generate a list of lit classes and their values.
//
/////////////////////////////////////

#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <list>
#include "gen_util.h"
#include "isa_lits_gen.h"

using std::list;

struct lit_range {
  const char *name;
  long long min;
  long long max;
};

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A list of all the lit classes used in an ISA.",
  " *   It exports the following:",
  " *",
  " *   typedef (enum) ISA_LITCLASS",
  " *       An enumeration of the lit classes.",
  " *",
  " *   typedef (struct) ISA_LITCLASS_INFO",
  " *       Contains info about first and last ECV in the EC.",
  " *       The contents are private.",
  " *",
  " *   typedef (struct) ISA_LITCLASS_VALUE_INFO",
  " *       Contains info about name and min/max of the LC.",
  " *       The contents are private.",
  " *",
  " *   const char * ISA_LC_Name (ISA_LITCLASS lc)",
  " *       Returns name of <lc>.",
  " *",
  " *   BOOL ISA_LC_Value_In_Class (INT64 val, ISA_LITCLASS lc)",
  " *       Returns whether <val> is a value that belongs to <lc>.",
  " *",
  " *   BOOL ISA_LC_Value_Min (ISA_LITCLASS lc)",
  " *       Returns the minimum value belonging to <lc>.",
  " *",
  " *   BOOL ISA_LC_Value_Max (ISA_LITCLASS lc)",
  " *       Returns the maximum value belonging to <lc>.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

static FILE *hfile, *cfile, *efile;
static struct lit_range signed_range[65];
static struct lit_range unsigned_range[65];
static int max_ranges = 0;

static list<char *> nameList;

/////////////////////////////////////
void ISA_Lits_Begin (void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
#define FNAME "targ_isa_lits"
  char buf[1000];
  sprintf (buf, "%s.h", FNAME);
  hfile = fopen(buf, "wb");
  sprintf (buf, "%s.c", FNAME);
  cfile = fopen(buf, "wb");
  sprintf (buf, "%s.Exported", FNAME);
  efile = fopen(buf, "wb");

  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  sprintf (buf, "%s", FNAME);
  Emit_Header (hfile, buf, interface);

  fprintf(hfile, "\ntypedef enum {\n");
  // start with undefined value
  fprintf(hfile, "\tLC_UNDEFINED,\n");

  fprintf(cfile, "const ISA_LITCLASS_INFO ISA_LITCLASS_info[] = {\n");
  fprintf(cfile, "  { { { 0x0000000000000000LL, 0x0000000000000000LL } }, 0, 0, 0,\"LC_UNDEFINED\" },\n");

  for (int i = 1; i <= 64; ++i) {
    unsigned_range[i].min = 0;
    unsigned_range[i].max = (i == 64) ? -1ULL : (1ULL << i) - 1;

    signed_range[i].min = -1LL << (i - 1);
    signed_range[i].max = (1LL << (i - 1)) - 1;
  }

  nameList.clear();
}


/////////////////////////////////////
LIT_RANGE SignedBitRange(unsigned int bit_size)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (bit_size == 0 || bit_size > 64) {
    fprintf(stderr, "### Error: invalid signed bit range: %d\n", bit_size);
    exit(EXIT_FAILURE);
  }
  return &signed_range[bit_size];
}


/////////////////////////////////////
LIT_RANGE UnsignedBitRange(unsigned int bit_size)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  if (bit_size == 0 || bit_size > 64) {
    fprintf(stderr, "### Error: invalid unsigned bit range: %d\n", bit_size);
    exit(EXIT_FAILURE);
  }
  return &unsigned_range[bit_size];
}


/////////////////////////////////////
LIT_RANGE ISA_Create_Lit_Range(const char *name, long long min, long long max)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  LIT_RANGE range = new lit_range;
  range->name = name;
  range->min = min;
  range->max = max;
  return range;
}


/////////////////////////////////////
void ISA_Create_Lit_Class_Range(const char* name, LITCLASS_TYPE type, ...)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  LIT_RANGE range;
  bool is_signed = type == SIGNED;
  long long min = is_signed ? LONG_LONG_MAX : ULONG_LONG_MAX;
  long long max = is_signed ? LONG_LONG_MIN : 0;
  int num_ranges = 0;

  // Find the smallest min and largest max for all ranges, and
  // count the number of ranges.
  va_start(ap,type);
  while ((range = va_arg(ap,LIT_RANGE)) != LIT_RANGE_END) {
    ++num_ranges;
    if (is_signed) {
      if (range->min < min) min = range->min;
      if (range->max > max) max = range->max;
    } else {
      if ((unsigned long long)range->min < (unsigned long long)min) {
	min = range->min;
      }
      if ((unsigned long long)range->max > (unsigned long long)max) {
	max = range->max;
      }
    }
  }
  va_end(ap);
  if (num_ranges > max_ranges) max_ranges = num_ranges;

  // Initialize ISA_LITCLASS_info for this class. Note that .range[0]
  // holds the smallest min/largest max; .range[1] is the first sub-range.
  fprintf(hfile, "\tLC_%s,\n", name);
  fprintf(cfile, "  { { { 0x%016" LL_FORMAT "xLL, 0x%016" LL_FORMAT "xLL }", min, max);
  va_start(ap,type);
  while ((range = va_arg(ap,LIT_RANGE)) != LIT_RANGE_END) {
    fprintf(cfile, ",\n      { 0x%016" LL_FORMAT "xLL, 0x%016" LL_FORMAT "xLL }",
		   range->min, range->max);
  }
  va_end(ap);
  fprintf(cfile, " }, %d, %d, 0, \"LC_%s\" },\n",
		 num_ranges, is_signed, name);
}


/////////////////////////////////////
void ISA_Create_Lit_Class_Func(const char* name, const char* func,
			       const long long min, const long long max)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  nameList.push_back(strdup(func));

  fprintf(hfile, "\tLC_%s,\n", name);
  fprintf(cfile, "  { { { 0x%016" LL_FORMAT "xLL, 0x%016" LL_FORMAT "xLL } }, 0, 0, &%s, \"LC_%s\" },\n",
  	  min, max, func, name);
}


/////////////////////////////////////
void ISA_Lits_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  fprintf(hfile, "\tLC_max\n");
  fprintf(hfile, "} ISA_LITCLASS;\n");

  fprintf(cfile, "};\n");

  fprintf(hfile, "\ntypedef struct {\n"
		"  struct { INT64 min; INT64 max; } range[%d];\n"
		"  mUINT8 num_ranges;\n"
		"  mBOOL is_signed;\n"
		"  BOOL (*func)(INT32 lit);\n"
		"  const char *name;\n"
		"} ISA_LITCLASS_INFO;\n",
		max_ranges + 1);
  fprintf(efile, "ISA_LITCLASS_info\n");

  fprintf(hfile, "\ninline const char * ISA_LC_Name (ISA_LITCLASS lc)\n"
		 "{\n"
		 "  extern DLL_SHARED const ISA_LITCLASS_INFO ISA_LITCLASS_info[];\n"
		 "  return ISA_LITCLASS_info[lc].name;\n"
		 "}\n");

  fprintf(hfile, "\ninline BOOL ISA_LC_Value_In_Class (INT64 val, ISA_LITCLASS lc)\n"
		 "{\n"
		 "  extern DLL_SHARED const ISA_LITCLASS_INFO ISA_LITCLASS_info[];\n"
		 "  const ISA_LITCLASS_INFO *plc = ISA_LITCLASS_info + lc;\n"
		 "  INT i;\n"
		 "  if (plc->func != 0) {\n"
	         "    return plc->func((INT32)val);\n"
	         "  }\n"
		 "  for (i = 1; i <= plc->num_ranges; ++i) {\n"
		 "    INT64 min = plc->range[i].min;\n"
		 "    INT64 max = plc->range[i].max;\n"
		 "    if ( plc->is_signed ) {\n"
		 "      if (val >= min && val <= max) return TRUE;\n"
		 "    } else {\n"
		 "      if ((UINT64)val >= (UINT64)min && (UINT64)val <= (UINT64)max) return TRUE;\n"
		 "    }\n"
		 "  }\n"
		 "  return FALSE;\n"
		 "}\n");

  fprintf(hfile, "\ninline int ISA_LC_Value_Min (ISA_LITCLASS lc)\n"
		 "{\n"
		 "  extern DLL_SHARED const ISA_LITCLASS_INFO ISA_LITCLASS_info[];\n"
		 "  return ISA_LITCLASS_info[lc].range[0].min;\n"
		 "}\n");

  fprintf(hfile, "\ninline int ISA_LC_Value_Max (ISA_LITCLASS lc)\n"
		 "{\n"
		 "  extern DLL_SHARED const ISA_LITCLASS_INFO ISA_LITCLASS_info[];\n"
		 "  return ISA_LITCLASS_info[lc].range[0].max;\n"
		 "}\n\n");

  list<char *>::iterator str;
  for (str = nameList.begin(); str != nameList.end(); str++)
    fprintf(hfile, "extern BOOL %s (INT32 lit);\n", *str);

  Emit_Footer (hfile);
}
