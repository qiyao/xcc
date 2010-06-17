
/*
   Copyright (C) 2002-2005 Tensilica, Inc.  All Rights Reserved.
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


//  isa_properties_gen.cxx
/////////////////////////////////////
//
//  Generate an interface for specifying properties (attributes) for
//  various instructions in the ISA.
//
/////////////////////////////////////
//
//  $Revision: 1.21 $
//  $Date: 2000/04/06 02:32:58 $
//  $Author: mtibuild $
//  $Source: /isms/cmplrs.src/osprey1.0/common/targ_info/generate/RCS/isa_properties_gen.cxx,v $


#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <assert.h>
#include <list>
#include <vector>
#include "topcode.h"
#include "gen_util.h"
#include "isa_properties_gen.h"

using std::list;
using std::vector;


struct isa_property {
  const char* name;         // Name given for documentation and debugging
  int bit_position;         // bit postion in flag word
  bool force_bit_position;  // create a bit position for property even if no members?
  vector <bool> members;    // set of opcodes that have this property
};

// special values for bit_position above:
enum {
  BIT_POS_ALL = -1,         // all members have this property
  BIT_POS_NONE = -2         // no members have this property
};

static list<ISA_PROPERTY> properties; // All the properties
static vector<int> sizes;             // size of each opcode in bytes

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the properties (attributes) for the instructions",
  " *   in the ISA. The description exports the following:",
  " *",
  " *   typedef (enum) ISA_PROPERTY",
  " *      Contains all the isa properties.  Their names have the form",
  " *      PROP_<name>.",
  " *",
  " *   BOOL TOP_has_property(TOP topcode, ISA_PROPERTY prop)",
  " *       Return true/false if 'topcode' has/does-not-have the property",
  " *       'prop'.",
  " *",
  " *   BOOL TOP_size(TOP topcode)",
  " *       Return the size, in bytes, of 'topcode'",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


/////////////////////////////////////
void ISA_Properties_Begin( const char* /* name */ )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  sizes = vector <int> (TOP_count, -1);
  sizes[(int)TOP_UNDEFINED] = 0;
}

/////////////////////////////////////
ISA_PROPERTY ISA_Property_Create( const char* name, bool force )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  ISA_PROPERTY result = new isa_property;

  result->name = name;
  result->force_bit_position = force;
  result->members = vector <bool> (TOP_count, false);

  properties.push_back(result);

  return result;
}

/////////////////////////////////////
void Instruction_Group( ISA_PROPERTY property, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  va_start(ap,property);
  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED ) {
    property->members[(int)opcode] = true;
  }
  va_end(ap);
}

/////////////////////////////////////
void Instruction_Size( unsigned int size, ... )
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  va_list ap;
  TOP opcode;

  va_start(ap,size);
  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED ) {
    if (sizes[(int)opcode] != -1) {
      fprintf (stderr, "###Error: multiple sizes specified for opcode: %s\n",
	       TOP_Name(opcode));
      exit(EXIT_FAILURE);
    }

    sizes[(int)opcode] = size;
  }
  va_end(ap);
}

/////////////////////////////////////
void ISA_Properties_End(void)
/////////////////////////////////////
//  See interface description.
/////////////////////////////////////
{
  list<ISA_PROPERTY>::iterator isi;
  int isa_property_count;	// How many non-constant properties?
  int code;

#define FNAME "targ_isa_properties"
  char filename[1000];
  sprintf (filename, "%s.h", FNAME);
  FILE* hfile = fopen(filename, "wb");
  sprintf (filename, "%s.c", FNAME);
  FILE* cfile = fopen(filename, "wb");
  sprintf (filename, "%s.Exported", FNAME);
  FILE* efile = fopen(filename, "wb");

  fprintf(cfile,"#include \"%s.h\"\n\n", FNAME);

  Emit_Header (hfile, "targ_isa_properties", interface);

  isa_property_count = 0;
  for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
    ISA_PROPERTY property = *isi;
    bool member;
    bool prev_member = property->members[0];
    for (code = 1; code < TOP_count; code++) {
      member = property->members[code];
      if (member != prev_member) break;
    }
    if (property->force_bit_position || (member != prev_member)) {
      property->bit_position = isa_property_count++;
    } else {
      property->bit_position = member ? BIT_POS_ALL : BIT_POS_NONE;
    }
  }

  char *int_type;
  char *int_suffix;
  int int_size;
  if (isa_property_count <= 8) {
    int_type = "mUINT8";
    int_suffix = "";
    int_size = 8;
  } else if (isa_property_count <= 16) {
    int_type = "mUINT16";
    int_suffix = "";
    int_size = 16;
  } else if (isa_property_count <= 32) {
    int_type = "mUINT32";
    int_suffix = "U";
    int_size = 32;
  } else {
#ifdef TARG_XTENSA
    /* enum members at most 32-bits... */
    assert (0);
#else
    assert (isa_property_count <= 64);
    int_type = "mUINT64";
    int_suffix = "ULL";
    int_size = 64;
#endif
  }
  fprintf (hfile, "extern DLL_SHARED const %s ISA_PROPERTIES_flags[];\n\n", int_type);
  fprintf (efile, "ISA_PROPERTIES_flags\n");
  fprintf (cfile,"const %s ISA_PROPERTIES_flags[] = {\n", int_type);

  for (code = 0; code < TOP_count; code++) {
    unsigned long long flag_value = 0;

    for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
      ISA_PROPERTY property = *isi;
      if (property->bit_position >= 0 && property->members[code]) {
	flag_value |= (1ULL << property->bit_position);
      }
    }
    fprintf (cfile, "  0x%0*" LL_FORMAT "x%s, /* %s:", int_size / 4,
					    flag_value,
					    int_suffix,
					    TOP_Name((TOP)code));
    for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
      ISA_PROPERTY property = *isi;
      if (property->members[code]) fprintf (cfile, " %s", property->name);
    }
    fprintf (cfile, " */\n");
  }
  fprintf (cfile, "};\n");
  fprintf (hfile, "typedef enum isa_property {\n");
  for ( isi = properties.begin(); isi != properties.end(); ++isi ) {
    ISA_PROPERTY property = *isi;
    int bit_position = property->bit_position;
    fprintf (hfile, "\tPROP_%-16s = 0x%0" LL_FORMAT "x%s,\n",
	     property->name,
	     (bit_position < 0) ? 0 : (1ULL << bit_position),
	     int_suffix);
  }
  fprintf (hfile, "} ISA_PROPERTY;\n");

  fprintf (hfile, "\n");
  fprintf (hfile, "#define TOP_has_property(t, p)\t (ISA_PROPERTIES_flags[(INT)t] & (p))\n");

  fprintf (hfile, "\n\nextern DLL_SHARED const mUINT8 ISA_PROPERTIES_sizes[];\n");
  fprintf (efile, "ISA_PROPERTIES_sizes\n");
  fprintf (cfile,"\nconst mUINT8 ISA_PROPERTIES_sizes[] = {\n");

  for (code = 0; code < TOP_count; code++) {
    if (sizes[code] == -1) {
      fprintf (stderr, "###Error: no size specified opcode: %s\n",
	       TOP_Name((TOP)code));
      exit(EXIT_FAILURE);
    }

    fprintf (cfile, "  %u, /* %s */\n", sizes[code], TOP_Name((TOP)code));
  }
  fprintf (cfile, "};\n");

  fprintf (hfile, "\n");
  fprintf (hfile, "#define TOP_size(t)\t (ISA_PROPERTIES_sizes[(INT)t])\n");

  Emit_Footer (hfile);
}
