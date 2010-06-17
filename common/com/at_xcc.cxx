// Copyright (c) 2004-2006 by Tensilica Inc.  ALL RIGHTS RESERVED.
// These coded instructions, statements, and computer programs are the
// copyrighted works and confidential proprietary information of Tensilica Inc.
// They may not be modified, copied, reproduced, distributed, or disclosed to
// third parties in any manner, medium, or form, in whole or in part, without
// the prior written consent of Tensilica Inc.

// at_xcc.cxx
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

// $Id: at_xcc.cxx $

#include <stdlib.h>
#include "at_trace.h"
#include "at_xcc.h"
#include "libti.h"
#include "tf_defs.h"
#include "tracing.h"
#include "cxx_memory.h"
#include "xt_mempool.h"
#include "pu_info.h"

DLL_SHARED tf_exports_t *tfex;
DLL_SHARED xml2sem_fn_t xml2sem_fn;

static XT_MEMPOOL *at_mempool = NULL;

static tf_exports_t *
libfusion_init (const char *libfusion_dll,
                xml2sem_fn_t &xml2sem_fn,
                bool trace)
{
  // Load the libfusion DLL.
  void *tf_handle = TI_DLL_Load(libfusion_dll, trace);
  if (!tf_handle)
    return NULL;
  
  // Lookup the libfusion interface handle initialization routine.
  tf_exports_init_fn_t tf_exports_init_fn = (tf_exports_init_fn_t)
    TI_DLL_Get_Symbol(tf_handle, "tf_exports_init", libfusion_dll, 1);
  
  if (!tf_exports_init_fn)
    return NULL;
  
  xml2sem_fn = (xml2sem_fn_t)
    TI_DLL_Get_Symbol(tf_handle, "tfc_xml2sem", libfusion_dll, 1);
  if (!xml2sem_fn)
    return NULL;
  
  if (at_mempool == NULL)
    at_mempool = CXX_NEW(XT_MEMPOOL("AUTOTIE"), Malloc_Mem_Pool);

  // Initialize the libfusion interface handle.
  tf_exports_t *tfex = (tf_exports_t *)at_mempool->allocate(sizeof(tf_exports_t));
  tf_exports_init_fn(tfex);
  
  // Initialize libfusion.
  tfex->init();

  return tfex;
}


bool
AT_Init_XCC (void)
{
  /* TODO: Make sure an autotie license is available. This is not strictly
     needed to collect analysis information, but it gives an earlier
     error then waiting until link time when the auto module will
     complain if there is no license. */

  BOOL trace = Get_Trace(TP_TEMP, 0x4000);
  
#ifdef _WIN32
  static const char *libfusion_dll = "fusion.dll";
#else
  static const char *libfusion_dll = "libfusion.so";
#endif
  
  tfex = libfusion_init(libfusion_dll, xml2sem_fn, trace);
  if (trace)
  {
    fprintf(TFile, "\n### Libfusion DLL: %s.\n\n",
	    tfex ? "SUCCESS" : "FAIL");
  }
  
  /* If doing autotie analysis, initialize libauto. */
  if (Run_Autotie)
  {
    if (at_mempool == NULL)
      at_mempool = CXX_NEW(XT_MEMPOOL("AUTOTIE"), Malloc_Mem_Pool);
    
    AT_Init(at_mempool, TI_TIE_Libisa_Info(), TI_TIE_Xtie_Info(), tfex);
    
    /* Enable libauto tracing if necessary. */
    if (trace)
    {
      xt_set_trace_file(TFile);
      xt_enable_trace(AT_Libauto_Trace());
    }
  }
  
  return true;
}


void
AT_Fini_XCC (void)
{
  if (Run_Autotie)
  {
    xt_disable_trace(AT_Libauto_Trace());
    AT_Fini();
  }
  
  if (tfex)
    tfex->free();

  if (at_mempool)
  {
    CXX_DELETE(at_mempool, Malloc_Mem_Pool);
    at_mempool = NULL;
  }
}

void 
AT_Create_PU(PU_Info * current_pu, char * src_file_name, char * sym_name, UINT32 linenum)
{
  AT_TY_TAB *ty_tab = XT_New(at_mempool) AT_TY_TAB(at_mempool, AT_Tie_Info());
  AT_OP_TAB *op_tab = XT_New(at_mempool) AT_OP_TAB(at_mempool, ty_tab);
  AT_PU *at_pu = XT_New(at_mempool) AT_PU(src_file_name, sym_name, linenum,
					  ty_tab, op_tab);

  Set_PU_Info_autotie_ptr(current_pu, at_pu);
  Set_PU_Info_state(current_pu, WT_AUTOTIE, Subsect_InMem);
}


// Local Variables:
// mode: c++
// c-style-variables-are-local-p: t
// c-file-style: "mongoose"
// End:
