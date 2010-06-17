// Copyright (c) 2004-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// at_xcc.h
//////////////////////////////////////////////////
/*---------------------------------------------------------------------------*
 *  Auto TIE interface to Xcalibur                                           *
 *---------------------------------------------------------------------------*/


/*

  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.

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

// $Id: at_xcc.h $

#ifndef __AT_XCC__
#define __AT_XCC__

#include "at.h"
#include "xt_mempool.h"
#include "pu_info.h"

/* libfusion handle */
extern DLL_SHARED tf_exports_t *tfex;

typedef tf_sem_tree_t (*xml2sem_fn_t) (tf_sems_t, xtie_xml_item);
extern DLL_SHARED xml2sem_fn_t xml2sem_fn;


/* Initialize and finalize autotie PU processsing. */
extern bool AT_Init_XCC (void);
extern void AT_Fini_XCC (void);
extern void AT_Create_PU (PU_Info * pu, char * src_file_name, char * sym_name, UINT32 linenum);

#endif /* __AT_XCC__ */


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
