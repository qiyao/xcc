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
// Parse literal information from isa. We need this as a standalone class so
// that multiple targets can be identical literal initialization.
/////////////////////////////////////////////////////////

#ifndef literals_INCLUDED
#define literals_INCLUDED

#include <assert.h>
#include <xtmap.h>
#include <xtlist.h>
#include <MEM_Pool.h>
#include "libisa_interface.h"

typedef struct operand_value_type *OPERAND_VALUE_TYPE;


class LITERALS;

class LITERALS {
public:
  struct literal {
    const char *name;
    const char *func;
    OPERAND_VALUE_TYPE valType;
    
    literal (const char *n, const char *f) : name(n), func(f), valType(NULL) { };
  };
  
private:
  typedef UTL_Map<const char *, literal *, UTL_Map_PtrHash> literalsMap;
  typedef LST_List<literal *> literalsList;

  literalsMap _map;
  literalsList _list;

public:
  LITERALS (LIBISA_INTERFACE& lii, MEM_Pool_p pool, bool includeBaseIsa =false) :
    _map(pool)
  {
    static char nameBuf[128], funcBuf[128];
    unsigned int cnt = 0;

    const unsigned int startOp = (includeBaseIsa) ? 0 : lii.numBaseOpcodes();
    
    for (int i = startOp; i < xtensa_isa_num_opcodes(lii.isa); ++i)
    {
      xtensa_opcode xtop = (xtensa_opcode) i;
      
      for (int oper = 0;
	   oper < xtensa_opcode_num_operands(lii.isa, xtop);
	   oper++)
      {
	const char *oper_name = xtensa_operand_name(lii.isa, xtop, oper);
	if (xtensa_operand_is_register (lii.isa, xtop, oper) == 0 &&
	    !_map.find(oper_name))
	{
	  sprintf(nameBuf, "user%d", cnt);
	  sprintf(funcBuf, "isa_lc_lit_user%d_in_range", cnt);

	  literal *newLit = MEM_New(pool) literal(strdup(nameBuf), strdup(funcBuf));
	  _map.insert(oper_name, newLit);
	  _list.append(newLit, pool);
	  cnt++;
	}
      }
    }
  }

  friend class iterator : public literalsList::iter {
  public:
    iterator (LITERALS *l) : literalsList::iter(l->_list) { }
    iterator (LITERALS& l) : literalsList::iter(l._list) { }
  };
};

#endif /* literals_INCLUDED */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
