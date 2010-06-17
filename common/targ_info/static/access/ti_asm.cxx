
/*
   Copyright (C) 2003-2005 Tensilica, Inc.  All Rights Reserved.
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


#include <stdio.h>
#include <assert.h>
#include "errors.h"
#include "libti.h"
#include "ti_errors.h"
#include "ti_asm.h"


/* ====================================================================
 *
 *  TI_ASM_Pack_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_Pack_Inst(
  /* TOP topcode,
  const INT64 *result,
  const INT64 *opnd,
  ISA_PACK_INST *pinst */)
{
  FmtAssert(FALSE, ("TI_ASM_Pack_Inst not supported for xtensa."));
  return 0;
}
/* ====================================================================
 *
 *  TI_ASM_Print_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_Print_Inst(
  TOP topcode,
  const char **result,
  const char **opnd,
  FILE *f)
{
  INT i;
  INT st;
  INT comp;
  const char *arg[ISA_PRINT_COMP_MAX];
  const ISA_PRINT_INFO *pinfo = TI_ISA_Print_Info(topcode);

  if (!pinfo) {
    #pragma mips_frequency_hint NEVER
    sprintf(TI_errmsg, "no ISA_PRINT_INFO for %s", TI_TOP_Name(topcode));
    return TI_RC_ERROR;
  }

  i = 0;
  do {
    comp = TI_ISA_Print_Info_Comp(pinfo, i);

    if (comp == ISA_PRINT_COMP_name)
      {
	arg[i] = TI_ISA_Print_Asmname(topcode);
      }
    else if ((comp >= ISA_PRINT_COMP_opnd)
	     && (comp < ISA_PRINT_COMP_result))
      {
	arg[i] = opnd[comp - ISA_PRINT_COMP_opnd];
      }
    else if ((comp >= ISA_PRINT_COMP_result)
	     && (comp <= ISA_PRINT_COMP_MAX))
      {
	arg[i] = result[comp - ISA_PRINT_COMP_result];
      }
    else if (comp != ISA_PRINT_COMP_end)
      {
	sprintf(TI_errmsg, "Unhandled listing component %d for %s",
		comp, TI_TOP_Name(topcode));
	return TI_RC_ERROR;
      }
  } while (++i, comp != ISA_PRINT_COMP_end);

  if (i<=9) {
    st = fprintf (f, TI_ISA_Print_Info_Format(pinfo),
		     arg[0], arg[1], arg[2], arg[3],
		     arg[4], arg[5], arg[6], arg[7],
		     arg[8]);
  } else if (i<=20) {
    st = fprintf (f, TI_ISA_Print_Info_Format(pinfo),
		     arg[0], arg[1], arg[2], arg[3],
		     arg[4], arg[5], arg[6], arg[7],
		     arg[8], arg[9],
		     arg[10], arg[11], arg[12], arg[13],
		     arg[14], arg[15], arg[16], arg[17],
		     arg[18], arg[19]);
  } else if (i<=30) {
    st = fprintf (f, TI_ISA_Print_Info_Format(pinfo),
		     arg[0], arg[1], arg[2], arg[3],
		     arg[4], arg[5], arg[6], arg[7],
		     arg[8], arg[9],
		     arg[10], arg[11], arg[12], arg[13],
		     arg[14], arg[15], arg[16], arg[17],
		     arg[18], arg[19],
		     arg[20], arg[21], arg[22], arg[23],
		     arg[24], arg[25], arg[26], arg[27],
		     arg[28], arg[29]);
  } else {
    sprintf(TI_errmsg, "Too many (>29) arguments");
    return TI_RC_ERROR;
  }

  if (st == -1) {
	sprintf(TI_errmsg, "fprintf failed:  not enough disk space");
	return TI_RC_ERROR;
  }
  else return st;
}
/* ====================================================================
 *
 *  TI_ASM_DisAsm_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
INT TI_ASM_DisAsm_Inst(
  /*TOP topcode,
  INT64 *result,
  INT64 *opnd,
  INT64 pc,
  INT flags,
  char *bufptr */)
{
  FmtAssert(FALSE, ("TI_ASM_DisAsm_Inst not supported for xtensa."));
  return 0;
}
/* ====================================================================
 *
 *  TI_ASM_Set_Bundle_Comp
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_ASM_Set_Bundle_Comp(
  /*ISA_BUNDLE           *bundle,
  ISA_BUNDLE_PACK_COMP  comp,
  UINT64                val*/
)
{
  FmtAssert(FALSE, ("TI_ASM_Set_Bundle_Comp not supported for xtensa."));
}


/* ====================================================================
 *
 *  TI_ASM_Get_Bundle_Comp
 *
 *  See interface description
 *
 * ====================================================================
 */
UINT64 TI_ASM_Get_Bundle_Comp(
  /*  const ISA_BUNDLE     *bundle,
      ISA_BUNDLE_PACK_COMP  comp */
)
{
  FmtAssert(FALSE, ("TI_ASM_Get_Bundle_Comp not supported for xtensa."));
  return 0;
}
/* ====================================================================
 *
 *  TI_ASM_Set_Bundle_Reloc_Value
 *
 *  See interface description
 *
 * ====================================================================
 */
void TI_ASM_Set_Bundle_Reloc_Value(
  /*  ISA_BUNDLE *bundle,
  INT         slot,
  UINT64      val */
)
{
  FmtAssert(FALSE, ("TI_ASM_Get_Bundle_Comp not supported for xtensa."));
}


/* ====================================================================
 *
 *  TI_ASM_Get_Bundle_Reloc_Value
 *
 *  See interface description
 *
 * ====================================================================
 */
UINT64 TI_ASM_Get_Bundle_Reloc_Value(
  /*  const ISA_BUNDLE *bundle,
      INT               slot */
)
{
  FmtAssert(FALSE, ("TI_ASM_Get_Bundle_Comp not supported for xtensa."));
  return 0;
}
/* ====================================================================
 *
 *  TI_ASM_Unpack_Inst
 *
 *  See interface description
 *
 * ====================================================================
 */
TOP TI_ASM_Unpack_Inst(
  /*  const ISA_PACK_INST *pinst,
  ISA_EXEC_UNIT ex_unit,
  INT64 *result,
  INT64 *opnd,
  BOOL xlate_pseudo */)
{
  FmtAssert(FALSE, ("TI_ASM_Get_Bundle_Comp not supported for xtensa."));
  return (TOP)0;
}
