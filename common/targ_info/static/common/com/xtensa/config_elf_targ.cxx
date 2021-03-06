
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


/* ====================================================================
 * ====================================================================
 *
 * Module: config_elf_targ.cxx
 * $Revision: 1.10 $
 * $Date: 2000/04/06 02:28:35 $
 * $Author: mtibuild $
 * $Source: /osprey.src/osprey1.0/common/com/ia64/RCS/config_elf_targ.cxx,v $
 *
 * Description:
 *
 * ELF configuration specific to the target machine/system.
 *
 * ====================================================================
 * ====================================================================
 */

#include <elf.h>
#include <sys/elf_whirl.h>
#include "defs.h"
#include "erglob.h"
#include "tracing.h"
#include "config_elf_targ.h"

/* ====================================================================
 *
 * Config_Target_From_ELF
 *
 * Based on the flags in the ELF header <ehdr>,
 * return whether is 64bit and the isa level.
 *
 * ====================================================================
 */
void Config_Target_From_ELF (Elf64_Word e_flags, BOOL *is_64bit, INT *isa)
{
  *is_64bit = (e_flags & EF_IRIX_ABI64);

  *isa = 1;
}


/* ====================================================================
 *
 * Config_ELF_From_Target
 *
 * Return the ELF specific flags for the current target ABI and ISA subset.
 *
 * ====================================================================
 */
Elf32_Word Config_ELF_From_Target(BOOL is_64bit, BOOL old_abi, INT isa)
{
  Elf32_Word e_flags = 0;

  if (is_64bit) e_flags |= EF_IRIX_ABI64;

  return e_flags;
}

Elf32_Half Get_Elf_Target_Machine (void)
{
// From Software/gnu/include/elf/common.h
#define EM_XTENSA      94       /* Tensilica's Xtensa architecture */
#define EM_XTENSA_OLD  0xabc7   /* old, unofficial value for Xtensa */

  return EM_XTENSA;
}

