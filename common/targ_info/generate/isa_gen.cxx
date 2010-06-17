
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
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


// isa_gen.cxx
/////////////////////////////////////
//
//  Generate an interface to create a new ISA (actually just an enum of
//  all the opcodes).
//    
/////////////////////////////////////
//
//  $Revision: 1.11 $
//  $Date: 2000/04/06 02:32:23 $
//  $Author: mtibuild $
//  $Source: /isms/cmplrs.src/osprey1.0/common/targ_info/generate/RCS/isa_gen.cxx,v $

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "gen_util.h"
#include "isa_gen.h"

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   A description of the ISA (actually just an enum of all the opcodes).",
  " *   The description exports the following:",
  " *",
  " *   TOP stands for Target OPCODE; prefix is TOP.",
  " *",
  " *   typedef (enum) TOP",
  " *      Contains all the target opcodes.  Their names have the form",
  " *      TOP_<name>.",
  " *",
  " *   typedef mTOP",
  " *      The smallest integer type that can contain all values of a TOP,",
  " *      including TOP_UNDEFINED -- useful for conserving space in tables.",
  " *",
  " *   const TOP TOP_UNDEFINED",
  " *      Useful value guaranteed not to be a valid TOP.",
  " *",
  " *   const int TOP_count",
  " *      Gives the number of topcodes.",
  " *",
  " *   const char* TOP_Name(TOP topcode)",
  " *      Returns an assembler style name for the given TOP.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};


FILE* hfile = fopen("topcode.h","wb");
FILE* cfile = fopen("topcode.c","wb");
FILE* efile = fopen("topcode.Exported","wb");
bool is_first = true;
int instruction_count = 1;  // start at 1 to count TOP_UNDEFINED

/////////////////////////////////////
static char* Dot_To_Line(const char* str)
/////////////////////////////////////
//  Copy <str> to newly allocated memory, replacing "." with "_" and return
//  the result.
/////////////////////////////////////
{
  char *result = (char*) malloc(strlen(str)+1);
  const char *s;
  char *r;

  for (s = str, r = result; *s != 0; ++s, ++r) {
    if (*s == '.')
      *r = '_';
    else
      *r = *s;
  }

  *r = 0;

  return result;
}


/////////////////////////////////////
void ISA_Create_Begin(const char* isa)
/////////////////////////////////////
//  Prepare to emit the opcode for the named <isa>
/////////////////////////////////////
{
  hfile = fopen("topcode.h","wb");
  cfile = fopen("topcode.c","wb");
  efile = fopen("topcode.Exported","wb");
  
  fprintf(cfile,"#include \"topcode.h\"\n");

  Emit_Header (hfile, "TOP", interface);

  fprintf(hfile,"typedef enum topcode {");
  fprintf(cfile,"static const char* const top_names[] = {");

  fprintf(hfile,"\n  TOP_UNDEFINED,");
  fprintf(cfile,"\n  \"UNDEFINED\",");
}


/////////////////////////////////////
void ISA_Opcode(const char* opcode)
/////////////////////////////////////
//  Enter a new opcode into the ISA
/////////////////////////////////////
{
    fprintf(hfile,"%s\n  TOP_%s",is_first ? "" : ",",Dot_To_Line(opcode));
    fprintf(cfile,"%s\n  \"%s\"",is_first ? "" : ",",opcode);
    if ( is_first )
      is_first = false;
  
    instruction_count++;
}


/////////////////////////////////////
void ISA_Create_End()
/////////////////////////////////////
//  Complete the ISA opcode list
/////////////////////////////////////
{
  fprintf(hfile,"\n} TOP;\n");
  fprintf(cfile,"\n};\n");

  // cannot use instruction_count to determine the smallest bit size
  // for mTOP because the TIE opcodes may overflow
  fprintf(hfile,"\ntypedef mUINT16 mTOP;\n"); 

  fprintf(hfile,"\n#define TOP_count %d\n", instruction_count);

  fprintf(hfile,"\nextern DLL_SHARED const char* TOP_Name(TOP topcode);\n");
  fprintf(efile,"TOP_Name\n");
  fprintf(cfile,"\nconst char* TOP_Name(TOP topcode)\n{\n"
                "  return top_names[(int)topcode];\n"
		"}\n");

  Emit_Footer (hfile);
}


/////////////////////////////////////
void ISA_Create (const char *isa_name, ...)
/////////////////////////////////////
//  Emit the topcode header and c files.
/////////////////////////////////////
{
  ISA_Create_Begin(isa_name);

  va_list ap;
  va_start(ap,isa_name);
  const char* opcode;
  while ((opcode = va_arg (ap, char *)) != NULL)
    ISA_Opcode(opcode);
  va_end(ap);

  ISA_Create_End();
}


/*
 * Local Variables:
 * mode: c
 * fill-column: 79
 * comment-column: 0
 * c-file-style: "mongoose"
 * End:
 */
