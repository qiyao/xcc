
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


/**
***		Opcodes, Opcode Types, and Opcode Flags
***		---------------------------------------
***
*** Description:
***
***	This interface describes all the opcode names, operators,
***	types associated with opcodes, and properties associated with opcodes.
***	The files opcode_gen_core.h and opcode_gen_core.c are generated by
***	running './opcodegen', which uses perl prodecure calls to encode
***	all legal opcodes and types in a tabular form.
***
***	This code require absolutely no #includes to use, and is thus useful
***	for programs other than the compiler proper.
***
*** Reserved Prefixes:
***
***	OPC		for OPCODE members only.
***	OPR		for OPERATOR members only.
***	OPCODE		for other opcode information, e.g. external
***			function names.
***	OPERATOR	for the operator, i.e. untyped operation
***
*** Exported types:
***
***	OPCODE
***
***	    An enumerated type.  The members are generated automatically.
***	    All OPCODEs are prefixed with OPC.
***
***	    OPCODE has the following fields:
***
***	    const OPERATOR operator
***
***		The operator for this opcode.
***		E.g. OPCODE_operator(OPC_F8ADD) is OPR_ADD.
***
***	    const TYPE_ID rtype
***
***		The 'return type' of this opcode.
***		E.g. OPCODE_rtype(OPC_F8ADD) is MTYPE_F8.
***
***	    const TYPE_ID desc
***
***		The 'descriptor type' of this opcode.
***		E.g. OPCODE_desc(OPC_F8ADD) is MTYPE_V.
***
***	    An opcode is fully and uniquely defined by these three components.
***
***	OPERATOR
***
***	    An enumerated type representing the operator.
***	    These are all prefixed with OPR.
***
***	OPCODE_MAPCAT
***
***	    The annotation category for opcodes, one of
***
***		  OPCODE_MAPCAT_HDR
***		  OPCODE_MAPCAT_SCF
***		  OPCODE_MAPCAT_LDST
***		  OPCODE_MAPCAT_PRAGMA
***		  OPCODE_MAPCAT_OSTMT
***		  OPCODE_MAPCAT_OEXP
***		  OPCODE_MAPCAT_ARRAY
***		  OPCODE_MAPCAT_CALL
***
***	    These are an enumerated type, the first value of which is 0.
***	    There are WN_MAP_CATEGORIES values, 0 through WN_MAP_CATEGORIES-1.
***
***
*** Exported data:
***
***	Commonly, these data structures would be used by opcode.h and opcode.c
***	to look up interesting information.  However, they are provided
***	separately because there is demand for them.
***
***	struct OPCODE_gop_info_struct {
***	  char name[16];
***	  OPCODE first;
***	  OPCODE last;
***	} OPCODE_gop_info[];
***
***	    Used to look up information about a OPERATOR: what the first
***	    opcode and last opcode are for that operator, and the ascii
***	    representation of the operator.
***
***	extern struct OPCODE_info_struct {
***	  char          _name[20];
***	  mUINT16       _operator;
***	  mUINT8        _rtype;
***	  mUINT8        _desc;
***	  mINT8         nkids;
***	  mUINT16	wnfields;
***	  mUINT16       _flags;
***	} OPCODE_info[];
***
***	    Used to look up information about an OPCODE: the ascii name,
***	    the "operator", the "return type" and "descriptor type" of
***	    the opcode, the number of kids (-1 if unknown) and the flags
***	    that define the boolean properties, currently
***
***	    OPCODE_PROPERTY_scf			structured control flow
***	    OPCODE_PROPERTY_stmt		statement
***	    OPCODE_PROPERTY_expression		expression
***	    OPCODE_PROPERTY_leaf		nkids is zero
***	    OPCODE_PROPERTY_store		is a store
***	    OPCODE_PROPERTY_load		is a load
***	    OPCODE_PROPERTY_call		is a call
***	    OPCODE_PROPERTY_compare		is a comparison
***	    OPCODE_PROPERTY_non_scf		control flow, but not scf
***	    OPCODE_PROPERTY_boolean		boolean returns, e.g. GE
**/

/** $Revision: 1.13 $
*** $Date: 2000/04/06 02:26:42 $
*** $Author: mtibuild $
*** $Source: /isms/cmplrs.src/osprey1.0/common/com/RCS/opcode_core.h,v $
**/

#ifndef opcode_core_INCLUDED
#define opcode_core_INCLUDED "opcode_core.h"

#ifdef _KEEP_RCS_ID
static char *opcode_core_rcs_id = opcode_core_INCLUDED "$Revision: 1.13 $";
#endif /* _KEEP_RCS_ID */

#include "opcode_gen_core.h"

#endif

