/*

  Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.

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

*/

//
// TIE exported types
//

// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/com/tietypes.h#1 $

//
// Exported Types
// --------------
//
//	TIE_TYPE_ID
//
//		an id for a tie type
//
//	TIE_MACRO_ID
//
//		an id for a tie macro
//
// Exported Macro
// --------------
//
//	TIE_INVALID_ID
//
//		an invalid id value for either TIE_TYPE_ID or TIE_MACRO_ID
//
// class TIE_INFO
// --------------
//
//	TIE_INFO contains all TIE related information and provides
//	methods to access those information. It obtains the TIE
//	data from libisa.so. To access the members, you need
//	to include tie.h
//
// class TIE_MACRO
// --------------
//
//	TIE_MACRO contains all information of a TIE macro and provides
//	methods to access those information. To access the members, you need
//	to include tie.h
//

#ifndef _TIE_TYPES_H_
#define _TIE_TYPES_H_

#include "mtypes.h"

typedef int TIE_TYPE_ID;
typedef int TIE_MACRO_ID;
const int TIE_INVALID_ID = -1;

class TIE_MACRO;

typedef TIE_MACRO* TIE_MACRO_p;

class TIE_INFO;

extern DLL_SHARED TIE_INFO* tie_info;

#define Mtype_To_Tie_Type_Id(i)		((i)-MTYPE_CORE_LAST-1+Num_Built_In)
#define Tie_Type_Id_To_Mtype(i)		((TYPE_ID)((i)+MTYPE_CORE_LAST+1-Num_Built_In))

#endif // _TIE_TYPES_H_

