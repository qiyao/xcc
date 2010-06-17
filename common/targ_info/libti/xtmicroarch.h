
/*

  Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.

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

// Xtensa micro-architecture
//////////////////////////////////////////////////
//
// Encapsulates implementation dependent information.
//
// Reserved prefix: XTM
//
//
// $Id: //depot/rel/BadgerPass/Xtensa/Software/xcalibur/common/targ_info/libti/xtmicroarch.h#1 $

#ifndef _XTMICROARCH_H_
#define _XTMICROARCH_H_

#include "errors.h"
#include "defs.h"
#include "cxx_memory.h"
#include "xtmap.h"
#include "xtarch.h"
#include "libti.h"
#include "si_gen.h"

//
// XTM_MicroArchitecture
//
typedef class XTM_MicroArchitecture *XTM_MicroArchitecture_p;
typedef const class XTM_MicroArchitecture *XTM_MicroArchitecture_cp;

// the following has to be a scalar type
typedef TARG_INT SLOT_BIT_VECTOR_t;

class XTM_MicroArchitecture {
private:
  typedef UTL_Map<const char *, RESOURCE, UTL_Map_StringNoCaseHash> resourceNameMap;
    
  MEM_POOL *_pool;
  XT_Architecture_p _arch;
  xtensa_isa _isa;
  BOOL _initialized;

  INT _resource_count;
  TI_SI_RESOURCE **_resources;
  TI_SI_RRW* _RRW_initializer;
  TI_SI_RRW* _RRW_overuse_mask;
  TI_SI_RRW* _RRW_format_resource_overuse_mask;
  INT _issue_slot_count;
  TI_SI_ISSUE_SLOT **_issue_slots;
  TI_SI **_top_si;
  INT _ID_count;
  TI_SI **_ID_si;

  resourceNameMap _resourceMap;
  RESOURCE _core_slot_resource;
  xtensa_format _core_format;
  BOOL _mov_n_initialized;
  BOOL _prefer_mul16;
  UINT _normalized_op_read;
  UINT _normalized_op_write;
  
  void find_power_set(xtensa_format bv_fmt,
		    SLOT_BIT_VECTOR_t bv, int begin, int end, int more);

  char* constructFormatResourceName (SLOT_BIT_VECTOR_t bv, xtensa_format fmt);
  void constructFormatResource ( SLOT_BIT_VECTOR_t bv, xtensa_format fmt);

  bool readIssueFormats ();
  void readFunctionUnits ();
  bool parseSchedule (void);
  void genericSchedule (void);
  void simulatedSchedule (void);
  RESOURCE find_resource (const char *res);

public:
  XTM_MicroArchitecture (MEM_POOL *pool, XT_Architecture_p arch);
  BOOL initialized (void) const { return _initialized; }

  INT resource_count (void) const { return _resource_count; }
  TI_SI_RESOURCE **resources (void) const { return _resources; }
  TI_SI_RRW* RRW_initializer (void) const { return _RRW_initializer; }
  TI_SI_RRW* RRW_overuse_mask (void) const { return _RRW_overuse_mask; }
  TI_SI_RRW* RRW_format_resource_overuse_mask (void) const { return _RRW_format_resource_overuse_mask; }
  INT issue_slot_count (void) const { return _issue_slot_count; }
  TI_SI_ISSUE_SLOT **issue_slots (void) const { return _issue_slots; }
  TI_SI **top_si (void) const { return _top_si; }
  INT ID_count (void) const { return _ID_count; }
  TI_SI **ID_si (void) const { return _ID_si; }

  // when both mul16 and mac16 are available, use this to determine
  // if mul16 is preferred
  bool prefer_mul16(void) const { return _prefer_mul16; }

#ifdef Is_True_On
  void debugGenerated (void);
#endif

};

#endif /* _XTMICROARCH_H_ */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
