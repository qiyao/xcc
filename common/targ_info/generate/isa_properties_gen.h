
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


// isa_properties_gen.h
/////////////////////////////////////
//
//  Interface for specifying properties (attributes) for various
//  instructions in the ISA.
//
//  void ISA_Properties_Begin( const char* archname )
//      Initialize to generate properties information for the architecture 
//      with the given <archname>.  The information will be written to the 
//      files targ_isa_properties.[ch].  
//
//  TYPE ISA_PROPERTY
//      An abstract type that represents a property of an instruction.
//      No client visible fields.
//
//  ISA_PROPERTY ISA_Property_Create( const char* name, bool force )
//      Used to create a new ISA_PROPERTY.  <name> is the property name.
//      It will be used to define a TOPCODE_<name> access function.
//      <force> is true if we should create a bit to represent this property
//      even if no opcodes have this property.
//
//  void Instruction_Group( ISA_PROPERTY property, ... )
//      Lists the instructions with the given <property>. Subsequent arguments 
//      are TOPs, terminating in TOP_UNDEFINED.  
//
//  void Instruction_Size( unsigned int size, ... )
//      Lists the instructions with the given <size> in bytes. Subsequent arguments 
//      are TOPs, terminating in TOP_UNDEFINED.  
//
//  void ISA_Properties_End(void)
//      Complete processing of properties.
//
//
/////////////////////////////////////


//  $Revision: 1.5 $
//  $Date: 2000/04/06 02:33:01 $
//  $Author: mtibuild $
//  $Source: /isms/cmplrs.src/osprey1.0/common/targ_info/generate/RCS/isa_properties_gen.h,v $

typedef struct isa_property *ISA_PROPERTY;

extern void ISA_Properties_Begin( const char* archname );
extern ISA_PROPERTY ISA_Property_Create( const char* name, bool force );
extern void Instruction_Group( ISA_PROPERTY property, ... );
extern void Instruction_Size( unsigned int size, ... );
extern void ISA_Properties_End(void);
