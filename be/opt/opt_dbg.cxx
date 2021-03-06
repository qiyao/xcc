//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_dbg.cxx
// $Revision: 1.20 $
// $Date: 2000/04/06 15:22:09 $
// $Author: dlstephe $
// $Source: /isms/cmplrs.src/osprey1.0/be/opt/RCS/opt_dbg.cxx,v $
//
// Revision history:
//  28-NOV-94 fchow - Original Version
//
// ====================================================================
//
/* 
   Copyright (C) 2002 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */
//
// Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// This file provides things that are helpful inside dbx when
// debugging WOPT.
// 
// ====================================================================
// ====================================================================


#include "defs.h"
#include "opt_main.h"
#include "errors.h"
#include "erglob.h"
#include "bb_node_set.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_cfg_trans.h"
#include "opt_htable.h"

#ifdef Is_True_On

COMP_UNIT *g_comp_unit;
OPT_STAB *g_opt_stab;

extern "C" void Dump_cfg(void);

void
Dump_cfg(void)
{
  g_comp_unit->Cfg()->Print();
}

void Dump_bb(BB_NODE *bb)
{
  bb->Print();
}

void Dump_cr(CODEREP *cr)
{
   cr->Print(2);
}

void Dump_sr(STMTREP *sr)
{
   sr->Print();
}

#endif


