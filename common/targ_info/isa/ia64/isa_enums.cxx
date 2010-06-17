
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



//
// Generate a list of enumeration classes and values for the ISA.
/////////////////////////////////////////////////////////

#include <stddef.h>
#include "isa_enums_gen.h"

main ()
{
  ISA_Enums_Begin();

  ISA_Create_Enum_Class ("mwh",
      ".sptk",		0,
      ".dptk",		2,
      NULL,		1);	// default value

  ISA_Create_Enum_Class ("ldhint",
      ".nt1",		1,
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("sthint",
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("lfhint",
      ".nt1",		1,
      ".nt2",		2,
      ".nta",		3,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("ph", 
      ".few",		0,
      ".many",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("bwh", 
      ".sptk",		0,
      ".spnt",		1,
      ".dptk",		2,
      ".dpnt",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("dh", 
      ".clr",		1,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("ipwh", 
      ".sptk",		0,
      ".loop",		1,
      ".dptk",		2,
      ".exit",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("indwh", 
      ".sptk",		0,
      ".dptk",		2,
      NULL,		UNDEFINED);	// default value

  // combine ph, pvec, and ih into one enum so max # operands = 5
  // the value is encoded such that bit 0 is ih, bits 1-3 are pvec,
  // and bit 4 is ph.
  ISA_Create_Enum_Class ("ph.pvec.ih", 
      ".few.dc.dc",	0x00,
      ".few.dc.dc.imp",	0x01,
      ".few.dc.nt",	0x02,
      ".few.dc.nt.imp",	0x03,
      ".few.tk.dc",	0x04,
      ".few.tk.dc.imp",	0x05,
      ".few.tk.tk",	0x06,
      ".few.tk.tk.imp",	0x07,
      ".few.tk.nt",	0x08,
      ".few.tk.nt.imp",	0x09,
      ".few.nt.dc",	0x0a,
      ".few.nt.dc.imp",	0x0b,
      ".few.nt.tk",	0x0c,
      ".few.nt.tk.imp",	0x0d,
      ".few.nt.nt",	0x0e,
      ".few.nt.nt.imp",	0x0f,
      ".many.dc.dc",	0x10,
      ".many.dc.dc.imp",0x11,
      ".many.dc.nt",	0x12,
      ".many.dc.nt.imp",0x13,
      ".many.tk.dc",	0x14,
      ".many.tk.dc.imp",0x15,
      ".many.tk.tk",	0x16,
      ".many.tk.tk.imp",0x17,
      ".many.tk.nt",	0x18,
      ".many.tk.nt.imp",0x19,
      ".many.nt.dc",	0x1a,
      ".many.nt.dc.imp",0x1b,
      ".many.nt.tk",	0x1c,
      ".many.nt.tk.imp",0x1d,
      ".many.nt.nt",	0x1e,
      ".many.nt.nt.imp",0x1f,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("aclr",
      ".nc",		0,
      ".clr",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("sem", 
      ".acq",		0,
      ".rel",		1,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("ldtype",
      ".s",		1,
      ".a",		2,
      ".sa",		3,
      ".bias",		4,
      ".acq",		5,
      ".c.clr",		8,
      ".c.nc",		9,
      ".c.clr.acq",	10,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("fldtype",
      ".s",		1,
      ".a",		2,
      ".sa",		3,
      ".c.clr",		8,
      ".c.nc",		9,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("sttype",
      ".rel",		1,
      NULL,		0);	// default value

  ISA_Create_Enum_Class ("mbtype4",
      "@brdcst",	0,
      "@mix",		8,
      "@shuf",		9,
      "@alt",		0xa,
      "@rev",		0xb,
      NULL,		UNDEFINED);	// default value

  ISA_Create_Enum_Class ("sf",
      ".s0",		0,
      ".s1",		1,
      ".s2",		2,
      ".s3",		3,
      NULL,		UNDEFINED);	// default value

  ISA_Enums_End();
}
