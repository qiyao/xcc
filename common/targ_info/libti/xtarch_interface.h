/*

  Copyright (C) 2002-2005 Tensilica, Inc.  All Rights Reserved.

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
// Interface to xtensa architecture extensions


#ifndef _XTARCH_INTERFACE_H_
#define _XTARCH_INTERFACE_H_

#include "errors.h"
#include "defs.h"
#include "libti.h"
#include "xtarch.h"
#include "xtmap.h"

//
// Interface version
//
#define XTARCH_INTERFACE_VERSION 1


/* Try to load and initialize the xtensa architecture dll. */
extern XT_Architecture_p TI_Architecture_Init (MEM_POOL *pool,
					       const char *dll,
					       char **libisa_dlls,
					       char **libtie_dlls);


#endif /* _XTARCH_INTERFACE_H_ */

// Local Variables:
// mode: c++
// fill-column: 79
// comment-column: 0
// c-file-style: "mongoose"
// End:
