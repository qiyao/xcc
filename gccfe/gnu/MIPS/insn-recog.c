/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `genrecog'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "function.h"
#include "insn-config.h"
#include "recog.h"
#include "real.h"
#include "output.h"
#include "flags.h"

extern rtx gen_split_7 ();
extern rtx gen_split_8 ();
extern rtx gen_split_11 ();
extern rtx gen_split_12 ();
extern rtx gen_split_14 ();
extern rtx gen_split_15 ();
extern rtx gen_split_20 ();
extern rtx gen_split_21 ();
extern rtx gen_split_31 ();
extern rtx gen_split_32 ();
extern rtx gen_split_35 ();
extern rtx gen_split_36 ();
extern rtx gen_split_38 ();
extern rtx gen_split_39 ();
extern rtx gen_split_44 ();
extern rtx gen_split_45 ();
extern rtx gen_split_59 ();
extern rtx gen_split_62 ();
extern rtx gen_split_139 ();
extern rtx gen_split_146 ();
extern rtx gen_split_154 ();
extern rtx gen_split_162 ();
extern rtx gen_split_166 ();
extern rtx gen_split_232 ();
extern rtx gen_split_235 ();
extern rtx gen_split_238 ();
extern rtx gen_split_244 ();
extern rtx gen_split_245 ();
extern rtx gen_split_246 ();
extern rtx gen_split_264 ();
extern rtx gen_split_269 ();
extern rtx gen_split_279 ();
extern rtx gen_split_286 ();
extern rtx gen_split_294 ();
extern rtx gen_split_298 ();
extern rtx gen_split_299 ();
extern rtx gen_split_301 ();
extern rtx gen_split_302 ();
extern rtx gen_split_305 ();
extern rtx gen_split_309 ();
extern rtx gen_split_313 ();
extern rtx gen_split_314 ();
extern rtx gen_split_316 ();
extern rtx gen_split_317 ();
extern rtx gen_split_320 ();
extern rtx gen_split_324 ();
extern rtx gen_split_326 ();
extern rtx gen_split_330 ();
extern rtx gen_split_331 ();
extern rtx gen_split_333 ();
extern rtx gen_split_334 ();
extern rtx gen_split_339 ();
extern rtx gen_split_370 ();
extern rtx gen_split_372 ();
extern rtx gen_split_377 ();
extern rtx gen_split_379 ();
extern rtx gen_split_387 ();
extern rtx gen_split_389 ();
extern rtx gen_split_401 ();
extern rtx gen_split_403 ();
extern rtx gen_split_411 ();
extern rtx gen_split_413 ();
extern rtx gen_split_425 ();
extern rtx gen_split_427 ();

/* `recog' contains a decision tree
   that recognizes whether the rtx X0 is a valid instruction.

   recog returns -1 if the rtx is not valid.
   If the rtx is valid, recog returns a nonnegative number
   which is the insn code number for the pattern that matched.
   This is the same as the order in the machine description of
   the entry that matched.  This number can be used as an index into various
   insn_* tables, such as insn_templates, insn_outfun, and insn_n_operands
   (found in insn-output.c).

   The third argument to recog is an optional pointer to an int.
   If present, recog will accept a pattern if it matches except for
   missing CLOBBER expressions at the end.  In that case, the value
   pointed to by the optional pointer will be set to the number of
   CLOBBERs that need to be added (it should be initialized to zero by
   the caller).  If it is set nonzero, the caller should allocate a
   PARALLEL of the appropriate size, copy the initial entries, and call
   add_clobbers (found in insn-emit.c) to fill in the CLOBBERs.

   The function split_insns returns 0 if the rtl could not
   be split or the split rtl in a SEQUENCE if it can be.*/

#define operands recog_operand

int
recog_1 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != DFmode)
    goto ret0;
  switch (GET_CODE (x1))
    {
    default:
      break;
    case PLUS:
      goto L759;
    case MINUS:
      goto L773;
    case MULT:
      goto L271;
    case NEG:
      goto L787;
    case DIV:
      goto L827;
    case SQRT:
      goto L1073;
    case ABS:
      goto L1101;
    case FLOAT_EXTEND:
      goto L1413;
    case FLOAT:
      goto L1465;
    case MEM:
      goto L1580;
    case IF_THEN_ELSE:
      goto L2899;
    }
  goto ret0;

  L759: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L760;
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L4;
    }
  goto ret0;

  L760: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      ro[1] = x3;
      goto L761;
    }
  goto ret0;

  L761: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      ro[2] = x3;
      goto L762;
    }
  goto ret0;

  L762: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 82;
      }
  goto ret0;

  L4: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 0;
      }
  goto ret0;

  L773: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L774;
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L804;
    }
  goto ret0;

  L774: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      ro[1] = x3;
      goto L775;
    }
  goto ret0;

  L775: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      ro[2] = x3;
      goto L776;
    }
  goto ret0;

  L776: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 84;
      }
  goto ret0;

  L804: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L805;
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 24;
      }
  goto ret0;

  L805: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      ro[2] = x3;
      goto L806;
    }
  goto ret0;

  L806: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      ro[3] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 88;
      }
  goto ret0;

  L271: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L272;
    }
  goto ret0;

  L272: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    goto L278;
  goto ret0;

  L278: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu != PROCESSOR_R4300)
    return 49;
  L279: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && mips_cpu == PROCESSOR_R4300)
    return 50;
  goto ret0;

  L787: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == PLUS && 1)
    goto L788;
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 135;
      }
  goto ret0;

  L788: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DFmode && GET_CODE (x3) == MULT && 1)
    goto L789;
  goto ret0;

  L789: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, DFmode))
    {
      ro[1] = x4;
      goto L790;
    }
  goto ret0;

  L790: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, DFmode))
    {
      ro[2] = x4;
      goto L791;
    }
  goto ret0;

  L791: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DFmode))
    {
      ro[3] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 86;
      }
  goto ret0;

  L827: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == CONST_DOUBLE && const_float_1_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L1082;
    }
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L818;
    }
  goto ret0;

  L1082: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != DFmode)
    goto ret0;
  if (GET_CODE (x2) == SQRT && 1)
    goto L1083;
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_fast_math)
	return 92;
      }
  goto ret0;

  L1083: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DFmode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT && flag_fast_math)
	return 123;
      }
  goto ret0;

  L818: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 90;
      }
  goto ret0;

  L1073: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && HAVE_SQRT_P() && TARGET_DOUBLE_FLOAT)
	return 121;
      }
  goto ret0;

  L1101: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 127;
      }
  goto ret0;

  L1413: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 206;
      }
  goto ret0;

  L1465: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (nonimmediate_operand (x2, SImode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	    return 211;
	  }
      break;
    case DImode:
      if (se_nonimmediate_operand (x2, DImode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	    return 212;
	  }
    }
  goto ret0;

  L1580: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != PLUS)
    goto ret0;
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      goto L1581;
    case DImode:
      goto L1587;
    }
  goto ret0;

  L1581: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1582;
    }
  goto ret0;

  L1582: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 254;
      }
  goto ret0;

  L1587: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1588;
    }
  goto ret0;

  L1588: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 255;
      }
  goto ret0;

  L2899: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      ro[4] = x2;
      goto L2900;
    }
  L2907: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2908;
    }
  goto ret0;

  L2900: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L2901;
    }
  goto L2907;

  L2901: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2902;
  goto L2907;

  L2902: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      goto L2903;
    }
  x2 = XEXP (x1, 0);
  goto L2907;

  L2903: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, DFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 503;
      }
  x2 = XEXP (x1, 0);
  goto L2907;

  L2908: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[4] = x3;
      goto L2909;
    }
  goto ret0;

  L2909: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2910;
  goto ret0;

  L2910: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      goto L2911;
    }
  goto ret0;

  L2911: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 504;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_2 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != SFmode)
    goto ret0;
  switch (GET_CODE (x1))
    {
    default:
      break;
    case PLUS:
      goto L766;
    case MINUS:
      goto L780;
    case MULT:
      goto L283;
    case NEG:
      goto L795;
    case DIV:
      goto L832;
    case SQRT:
      goto L1077;
    case ABS:
      goto L1105;
    case FLOAT_TRUNCATE:
      goto L1291;
    case FLOAT:
      goto L1473;
    case MEM:
      goto L1568;
    case IF_THEN_ELSE:
      goto L2883;
    }
  goto ret0;

  L766: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L767;
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L9;
    }
  goto ret0;

  L767: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      ro[1] = x3;
      goto L768;
    }
  goto ret0;

  L768: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      ro[2] = x3;
      goto L769;
    }
  goto ret0;

  L769: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 83;
      }
  goto ret0;

  L9: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 1;
      }
  goto ret0;

  L780: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L781;
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L811;
    }
  goto ret0;

  L781: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      ro[1] = x3;
      goto L782;
    }
  goto ret0;

  L782: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      ro[2] = x3;
      goto L783;
    }
  goto ret0;

  L783: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 85;
      }
  goto ret0;

  L811: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L812;
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 25;
      }
  goto ret0;

  L812: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      ro[2] = x3;
      goto L813;
    }
  goto ret0;

  L813: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      ro[3] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 89;
      }
  goto ret0;

  L283: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L284;
    }
  goto ret0;

  L284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    goto L290;
  goto ret0;

  L290: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_HARD_FLOAT && mips_cpu != PROCESSOR_R4300)
    return 52;
  L291: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_HARD_FLOAT && mips_cpu == PROCESSOR_R4300)
    return 53;
  goto ret0;

  L795: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == PLUS && 1)
    goto L796;
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT)
	return 136;
      }
  goto ret0;

  L796: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SFmode && GET_CODE (x3) == MULT && 1)
    goto L797;
  goto ret0;

  L797: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SFmode))
    {
      ro[1] = x4;
      goto L798;
    }
  goto ret0;

  L798: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SFmode))
    {
      ro[2] = x4;
      goto L799;
    }
  goto ret0;

  L799: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SFmode))
    {
      ro[3] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 87;
      }
  goto ret0;

  L832: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == CONST_DOUBLE && const_float_1_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L1088;
    }
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L823;
    }
  goto ret0;

  L1088: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SFmode)
    goto ret0;
  if (GET_CODE (x2) == SQRT && 1)
    goto L1089;
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && flag_fast_math)
	return 93;
      }
  goto ret0;

  L1089: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SFmode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && flag_fast_math)
	return 124;
      }
  goto ret0;

  L823: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 91;
      }
  goto ret0;

  L1077: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && HAVE_SQRT_P())
	return 122;
      }
  goto ret0;

  L1105: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT)
	return 128;
      }
  goto ret0;

  L1291: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DFmode))
    {
      ro[1] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 167;
      }
  goto ret0;

  L1473: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (nonimmediate_operand (x2, SImode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT)
	    return 213;
	  }
      break;
    case DImode:
      if (se_nonimmediate_operand (x2, DImode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	    return 214;
	  }
    }
  goto ret0;

  L1568: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != PLUS)
    goto ret0;
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      goto L1569;
    case DImode:
      goto L1575;
    }
  goto ret0;

  L1569: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1570;
    }
  goto ret0;

  L1570: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 252;
      }
  goto ret0;

  L1575: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1576;
    }
  goto ret0;

  L1576: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 253;
      }
  goto ret0;

  L2883: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      ro[4] = x2;
      goto L2884;
    }
  L2891: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2892;
    }
  goto ret0;

  L2884: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L2885;
    }
  goto L2891;

  L2885: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2886;
  goto L2891;

  L2886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      goto L2887;
    }
  x2 = XEXP (x1, 0);
  goto L2891;

  L2887: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, SFmode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 501;
      }
  x2 = XEXP (x1, 0);
  goto L2891;

  L2892: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[4] = x3;
      goto L2893;
    }
  goto ret0;

  L2893: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2894;
  goto ret0;

  L2894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[1] = x2;
      goto L2895;
    }
  goto ret0;

  L2895: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 502;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_3 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != SImode)
    goto ret0;
  switch (GET_CODE (x1))
    {
    default:
      break;
    case MINUS:
      goto L157;
    case MULT:
      goto L307;
    case PLUS:
      goto L364;
    case NEG:
      goto L400;
    case TRUNCATE:
      goto L1307;
    case DIV:
      goto L963;
    case MOD:
      goto L993;
    case UDIV:
      goto L1023;
    case UMOD:
      goto L1053;
    case ABS:
      goto L1093;
    case FFS:
      goto L1118;
    case NOT:
      goto L1158;
    case AND:
      goto L1170;
    case IOR:
      goto L1202;
    case XOR:
      goto L1229;
    case ZERO_EXTEND:
      goto L1325;
    case SIGN_EXTEND:
      goto L1397;
    case UNSPEC:
      if (XINT (x1, 1) == 0 && XVECLEN (x1, 0) == 1 && 1)
	goto L1481;
      break;
    case HIGH:
      goto L1497;
    case LO_SUM:
      goto L1501;
    case ASHIFT:
      goto L1865;
    case ASHIFTRT:
      goto L1961;
    case LSHIFTRT:
      goto L2057;
    case ROTATERT:
      goto L2151;
    case EQ:
      goto L2237;
    case NE:
      goto L2281;
    case GT:
      goto L2311;
    case GE:
      goto L2331;
    case LT:
      goto L2351;
    case LE:
      goto L2375;
    case GTU:
      goto L2419;
    case GEU:
      goto L2439;
    case LTU:
      goto L2459;
    case LEU:
      goto L2483;
    case IF_THEN_ELSE:
      goto L2835;
    }
  goto ret0;

  L157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L158;
  L162: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L423;
    }
  goto ret0;

  L158: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[1] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 29;
      }
  x2 = XEXP (x1, 0);
  goto L162;

  L423: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == MULT && 1)
    goto L424;
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000)))
	return 30;
      }
  goto ret0;

  L424: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L425;
    }
  goto ret0;

  L425: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (pnum_clobbers != 0 && register_operand (x3, SImode))
    {
      ro[3] = x3;
      if (TARGET_MIPS5400)
	{
	  *pnum_clobbers = 4;
	  return 61;
	}
      }
  goto ret0;

  L307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L308;
    }
  goto ret0;

  L308: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L324;
  L342: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (mips_cpu == PROCESSOR_R4000 && !TARGET_MIPS16)
	{
	  *pnum_clobbers = 3;
	  return 57;
	}
      }
  goto ret0;

  L324: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x2;
      if ((GENERATE_MULT3
    || TARGET_MIPS5400 /* CYGNUS LOCAL vr5400/raeburn */
    || TARGET_MAD)
   && !0)
	{
	  *pnum_clobbers = 3;
	  return 55;
	}
      }
  L325: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x2;
      if (mips_cpu != PROCESSOR_R4000 || TARGET_MIPS16)
	{
	  *pnum_clobbers = 2;
	  return 56;
	}
      }
  goto L342;

  L364: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == MULT && 1)
    goto L365;
  goto ret0;

  L365: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L366;
    }
  goto ret0;

  L366: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L367;
    }
  goto ret0;

  L367: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && register_operand (x2, SImode))
    {
      ro[3] = x2;
      if ((TARGET_MIPS3900
    || TARGET_MIPS5400)			/* CYGNUS LOCAL vr5400/raeburn */
   && !TARGET_MIPS16)
	{
	  *pnum_clobbers = 4;
	  return 58;
	}
      }
  L709: ATTRIBUTE_UNUSED_LABEL
  if (rtx_equal_p (x2, ro[0]) && pnum_clobbers != 0 && 1)
    if (TARGET_MAD)
      {
	*pnum_clobbers = 2;
	return 79;
      }
  goto ret0;

  L400: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L401;
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      return 131;
    }
  goto ret0;

  L401: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L402;
    }
  goto ret0;

  L402: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (pnum_clobbers != 0 && register_operand (x3, SImode))
    {
      ro[2] = x3;
      if (TARGET_MIPS5400)
	{
	  *pnum_clobbers = 3;
	  return 60;
	}
      }
  goto ret0;

  L1307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case ASHIFTRT:
      goto L1308;
    case LSHIFTRT:
      goto L1314;
    case ASHIFT:
      goto L1320;
    }
  L576: ATTRIBUTE_UNUSED_LABEL
  if (highpart_shift_operator (x2, DImode))
    {
      ro[5] = x2;
      goto L577;
    }
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT)
	return 168;
      }
  goto ret0;

  L1308: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1309;
    }
  goto L576;

  L1309: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 171;
      }
  goto L576;

  L1314: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1315;
    }
  goto L576;

  L1315: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 172;
      }
  goto L576;

  L1320: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1321;
    }
  goto ret0;

  L1321: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT)
	return 173;
      }
  goto ret0;

  L577: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) != DImode)
    goto ret0;
  switch (GET_CODE (x3))
    {
    default:
      break;
    case MULT:
      goto L578;
    case NEG:
      goto L635;
    }
  goto ret0;

  L578: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      ro[3] = x4;
      goto L579;
    }
  goto ret0;

  L579: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[1] = x5;
      goto L580;
    }
  goto ret0;

  L580: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      ro[4] = x4;
      goto L581;
    }
  goto ret0;

  L581: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[2] = x5;
      goto L582;
    }
  goto ret0;

  L582: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 32 && 1)
    goto L610;
  goto ret0;

  L610: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    if (! TARGET_MIPS5400 && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      {
	*pnum_clobbers = 2;
	return 74;
      }
  L611: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    if (TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      {
	*pnum_clobbers = 3;
	return 75;
      }
  goto ret0;

  L635: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode && GET_CODE (x4) == MULT && 1)
    goto L636;
  goto ret0;

  L636: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      ro[3] = x5;
      goto L637;
    }
  goto ret0;

  L637: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[1] = x6;
      goto L638;
    }
  goto ret0;

  L638: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      ro[4] = x5;
      goto L639;
    }
  goto ret0;

  L639: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[2] = x6;
      goto L640;
    }
  goto ret0;

  L640: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 32 && pnum_clobbers != 0 && 1)
    if (TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      {
	*pnum_clobbers = 3;
	return 76;
      }
  goto ret0;

  L963: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L964;
    }
  goto ret0;

  L964: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && nonmemory_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!optimize)
	{
	  *pnum_clobbers = 2;
	  return 106;
	}
      }
  goto ret0;

  L993: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L994;
    }
  goto ret0;

  L994: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && nonmemory_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!optimize)
	{
	  *pnum_clobbers = 2;
	  return 110;
	}
      }
  goto ret0;

  L1023: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1024;
    }
  goto ret0;

  L1024: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && nonmemory_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!optimize)
	{
	  *pnum_clobbers = 2;
	  return 114;
	}
      }
  goto ret0;

  L1053: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1054;
    }
  goto ret0;

  L1054: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && nonmemory_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!optimize)
	{
	  *pnum_clobbers = 2;
	  return 118;
	}
      }
  goto ret0;

  L1093: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      if (!TARGET_MIPS16)
	return 125;
      }
  goto ret0;

  L1118: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (pnum_clobbers != 0 && register_operand (x2, SImode))
    {
      ro[1] = x2;
      if (!TARGET_MIPS16)
	{
	  *pnum_clobbers = 2;
	  return 129;
	}
      }
  goto ret0;

  L1158: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      return 137;
    }
  goto ret0;

  L1170: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (uns_arith_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1171;
    }
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (GET_CODE (x2) == NOT && 1)
    goto L1271;
  L1175: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1176;
    }
  goto ret0;

  L1171: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!TARGET_MIPS16)
	return 141;
      }
  x2 = XEXP (x1, 0);
  goto L1175;

  L1271: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1272;
    }
  goto ret0;

  L1272: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == NOT && 1)
    goto L1273;
  goto ret0;

  L1273: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      if (!TARGET_MIPS16)
	return 164;
      }
  goto ret0;

  L1176: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16)
	return 142;
      }
  goto ret0;

  L1202: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (uns_arith_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1203;
    }
  L1207: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1208;
    }
  goto ret0;

  L1203: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!TARGET_MIPS16)
	return 149;
      }
  x2 = XEXP (x1, 0);
  goto L1207;

  L1208: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16)
	return 150;
      }
  goto ret0;

  L1229: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (uns_arith_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1230;
    }
  goto ret0;

  L1230: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    goto L1236;
  goto ret0;

  L1236: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 156;
  L1237: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 157;
  goto ret0;

  L1325: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case HImode:
      if (GET_CODE (x2) == TRUNCATE && 1)
	goto L1326;
      if (nonimmediate_operand (x2, HImode))
	{
	  ro[1] = x2;
	  if (!TARGET_MIPS16)
	    return 180;
	  }
    L1348: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x2, HImode))
	{
	  ro[1] = x2;
	  if (TARGET_MIPS16)
	    return 181;
	  }
      break;
    case QImode:
      if (GET_CODE (x2) == TRUNCATE && 1)
	goto L1331;
      if (nonimmediate_operand (x2, QImode))
	{
	  ro[1] = x2;
	  if (!TARGET_MIPS16)
	    return 189;
	  }
    L1372: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x2, QImode))
	{
	  ro[1] = x2;
	  if (TARGET_MIPS16)
	    return 190;
	  }
    }
  goto ret0;

  L1326: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 174;
      }
  goto ret0;

  L1331: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 175;
      }
  goto ret0;

  L1397: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case HImode:
      if (memory_operand (x2, HImode))
	{
	  ro[1] = x2;
	  return 199;
	}
      break;
    case QImode:
      if (memory_operand (x2, QImode))
	{
	  ro[1] = x2;
	  return 203;
	}
    }
  goto ret0;

  L1481: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (general_operand (x2, BLKmode))
    {
      ro[1] = x2;
      if (!TARGET_MIPS16)
	return 222;
      }
  goto ret0;

  L1497: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (immediate_operand (x2, SImode))
    {
      ro[1] = x2;
      if (mips_split_addresses && !TARGET_MIPS16)
	return 226;
      }
  goto ret0;

  L1501: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1502;
    }
  goto ret0;

  L1502: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      ro[2] = x2;
      if (mips_split_addresses && !TARGET_MIPS16)
	return 227;
      }
  goto ret0;

  L1865: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1866;
    }
  goto ret0;

  L1866: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L1872;
  goto ret0;

  L1872: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 292;
  L1873: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 293;
  goto ret0;

  L1961: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1962;
    }
  goto ret0;

  L1962: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L1968;
  goto ret0;

  L1968: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 307;
  L1969: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 308;
  goto ret0;

  L2057: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2058;
    }
  L2074: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2075;
    }
  goto ret0;

  L2058: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L2064;
  x2 = XEXP (x1, 0);
  goto L2074;

  L2064: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 322;
  L2065: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 323;
  x2 = XEXP (x1, 0);
  goto L2074;

  L2075: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16)
	return 325;
      }
  goto ret0;

  L2151: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2152;
    }
  goto ret0;

  L2152: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS5400)
	return 336;
      }
  goto ret0;

  L2237: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2238;
    }
  goto ret0;

  L2238: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && XWINT (x2, 0) == 0 && 1)
    goto L2244;
  L2262: ATTRIBUTE_UNUSED_LABEL
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 369;
      }
  goto ret0;

  L2244: ATTRIBUTE_UNUSED_LABEL
  if (!TARGET_MIPS16)
    return 365;
  L2245: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 366;
  goto L2262;

  L2281: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2282;
    }
  goto ret0;

  L2282: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && XWINT (x2, 0) == 0 && 1)
    if (!TARGET_MIPS16)
      return 374;
  L2292: ATTRIBUTE_UNUSED_LABEL
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 376;
      }
  goto ret0;

  L2311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2312;
    }
  goto ret0;

  L2312: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!TARGET_MIPS16)
	return 381;
      }
  L2317: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16)
	return 382;
      }
  goto ret0;

  L2331: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2332;
    }
  goto ret0;

  L2332: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 386;
      }
  goto ret0;

  L2351: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2352;
    }
  goto ret0;

  L2352: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L2358;
  goto ret0;

  L2358: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 391;
  L2359: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 392;
  goto ret0;

  L2375: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2400;
    }
  goto ret0;

  L2400: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 400;
      }
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    goto L2382;
  goto ret0;

  L2382: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 396;
  L2383: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 397;
  goto ret0;

  L2419: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2420;
    }
  goto ret0;

  L2420: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[2] = x2;
      return 405;
    }
  L2425: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      return 406;
    }
  goto ret0;

  L2439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2440;
    }
  goto ret0;

  L2440: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 410;
      }
  goto ret0;

  L2459: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2460;
    }
  goto ret0;

  L2460: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L2466;
  goto ret0;

  L2466: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16)
    return 415;
  L2467: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16)
    return 416;
  goto ret0;

  L2483: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2508;
    }
  goto ret0;

  L2508: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 424;
      }
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    goto L2490;
  goto ret0;

  L2490: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 420;
  L2491: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 421;
  goto ret0;

  L2835: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      ro[4] = x2;
      goto L2836;
    }
  L2851: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2852;
    }
  goto ret0;

  L2836: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    default:
      break;
    case SImode:
      if (register_operand (x3, SImode))
	{
	  ro[1] = x3;
	  goto L2837;
	}
      break;
    case DImode:
      if (se_register_operand (x3, DImode))
	{
	  ro[1] = x3;
	  goto L2845;
	}
    }
  goto L2851;

  L2837: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2838;
  goto L2851;

  L2838: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L2839;
    }
  x2 = XEXP (x1, 0);
  goto L2851;

  L2839: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 || 0)
	return 495;
      }
  x2 = XEXP (x1, 0);
  goto L2851;

  L2845: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2846;
  goto L2851;

  L2846: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L2847;
    }
  x2 = XEXP (x1, 0);
  goto L2851;

  L2847: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 || 0)
	return 496;
      }
  x2 = XEXP (x1, 0);
  goto L2851;

  L2852: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[4] = x3;
      goto L2853;
    }
  goto ret0;

  L2853: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2854;
  goto ret0;

  L2854: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2855;
    }
  goto ret0;

  L2855: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 497;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_4 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != DImode)
    goto ret0;
  switch (GET_CODE (x1))
    {
    default:
      break;
    case MINUS:
      goto L239;
    case SIGN_EXTEND:
      goto L259;
    case MULT:
      goto L455;
    case NEG:
      goto L530;
    case TRUNCATE:
      goto L659;
    case PLUS:
      goto L725;
    case DIV:
      goto L978;
    case MOD:
      goto L1008;
    case UDIV:
      goto L1038;
    case UMOD:
      goto L1068;
    case ABS:
      goto L1097;
    case FFS:
      goto L1131;
    case NOT:
      goto L1162;
    case AND:
      goto L1277;
    case IOR:
      goto L1212;
    case XOR:
      goto L1241;
    case ZERO_EXTEND:
      goto L1340;
    case UNSPEC:
      if (XINT (x1, 1) == 0 && XVECLEN (x1, 0) == 1 && 1)
	goto L1489;
      break;
    case ASHIFT:
      goto L1944;
    case ASHIFTRT:
      goto L2040;
    case LSHIFTRT:
      goto L2146;
    case ROTATERT:
      goto L2156;
    case EQ:
      goto L2249;
    case NE:
      goto L2286;
    case GT:
      goto L2321;
    case GE:
      goto L2341;
    case LT:
      goto L2363;
    case LE:
      goto L2387;
    case GTU:
      goto L2429;
    case GEU:
      goto L2449;
    case LTU:
      goto L2471;
    case LEU:
      goto L2495;
    case IF_THEN_ELSE:
      goto L2859;
    }
  goto ret0;

  L239: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  if (GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L240;
  L244: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L245;
    }
  L552: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L553;
    }
  goto ret0;

  L240: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 42;
      }
  x2 = XEXP (x1, 0);
  goto L244;

  L245: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000)))
	return 43;
      }
  x2 = XEXP (x1, 0);
  goto L552;

  L553: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == MULT && 1)
    goto L554;
  goto ret0;

  L554: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      ro[4] = x3;
      goto L555;
    }
  goto ret0;

  L555: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L556;
    }
  goto ret0;

  L556: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      ro[5] = x3;
      goto L557;
    }
  goto ret0;

  L557: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (pnum_clobbers != 0 && register_operand (x4, SImode))
    {
      ro[2] = x4;
      if (TARGET_64BIT && TARGET_MIPS5400 && GET_CODE (operands[4]) == GET_CODE (operands[5]))
	{
	  *pnum_clobbers = 2;
	  return 71;
	}
      }
  goto ret0;

  L259: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      switch (GET_CODE (x2))
	{
	default:
	  break;
	case MINUS:
	  goto L260;
	case SUBREG:
	  if (XINT (x2, 1) == 0 && 1)
	    goto L1381;
	}
    L1389: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x2, SImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT)
	    return 195;
	  }
      break;
    case HImode:
      if (memory_operand (x2, HImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT)
	    return 197;
	  }
      break;
    case QImode:
      if (memory_operand (x2, QImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT)
	    return 205;
	  }
    }
  goto ret0;

  L260: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (reg_or_0_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L261;
    }
  L266: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L267;
    }
  goto ret0;

  L261: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 46;
      }
  x3 = XEXP (x2, 0);
  goto L266;

  L267: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT && TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT
       || (INTVAL (operands[2]) != -32768 && INTVAL (operands[2]) != -0x4000)))
	return 47;
      }
  goto ret0;

  L1381: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (memory_operand (x3, HImode))
    {
      ro[1] = x3;
      if (TARGET_64BIT)
	return 193;
      }
  goto L1389;

  L455: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L456;
    }
  L489: ATTRIBUTE_UNUSED_LABEL
  if (extend_operator (x2, DImode))
    {
      ro[3] = x2;
      goto L490;
    }
  goto ret0;

  L456: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    goto L474;
  x2 = XEXP (x1, 0);
  goto L489;

  L474: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x2;
      if (TARGET_64BIT && mips_cpu != PROCESSOR_R4000 && !TARGET_MIPS16 && !0)
	{
	  *pnum_clobbers = 2;
	  return 64;
	}
      }
  L475: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x2;
      if (TARGET_64BIT && (GENERATE_MULT3 || mips_cpu == PROCESSOR_R4000 || TARGET_MIPS16) && !0)
	{
	  *pnum_clobbers = 3;
	  return 65;
	}
      }
  x2 = XEXP (x1, 0);
  goto L489;

  L490: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L491;
    }
  goto ret0;

  L491: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (extend_operator (x2, DImode))
    {
      ro[4] = x2;
      goto L492;
    }
  goto ret0;

  L492: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    goto L512;
  goto ret0;

  L512: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x3;
      if (!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	{
	  *pnum_clobbers = 1;
	  return 68;
	}
      }
  L513: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x3;
      if (TARGET_64BIT && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	{
	  *pnum_clobbers = 2;
	  return 69;
	}
      }
  goto ret0;

  L530: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  if (GET_CODE (x2) == MULT && 1)
    goto L531;
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 134;
      }
  goto ret0;

  L531: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      ro[3] = x3;
      goto L532;
    }
  goto ret0;

  L532: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L533;
    }
  goto ret0;

  L533: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      ro[4] = x3;
      goto L534;
    }
  goto ret0;

  L534: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (pnum_clobbers != 0 && register_operand (x4, SImode))
    {
      ro[2] = x4;
      if (TARGET_64BIT && TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	{
	  *pnum_clobbers = 2;
	  return 70;
	}
      }
  goto ret0;

  L659: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == TImode && GET_CODE (x2) == LSHIFTRT && 1)
    goto L660;
  goto ret0;

  L660: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode && GET_CODE (x3) == MULT && 1)
    goto L661;
  goto ret0;

  L661: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) != TImode)
    goto ret0;
  switch (GET_CODE (x4))
    {
    default:
      break;
    case SIGN_EXTEND:
      goto L662;
    case ZERO_EXTEND:
      goto L687;
    }
  goto ret0;

  L662: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      ro[1] = x5;
      goto L663;
    }
  goto ret0;

  L663: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode && GET_CODE (x4) == SIGN_EXTEND && 1)
    goto L664;
  goto ret0;

  L664: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      ro[2] = x5;
      goto L665;
    }
  goto ret0;

  L665: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 64 && pnum_clobbers != 0 && 1)
    if (TARGET_64BIT && !0)
      {
	*pnum_clobbers = 2;
	return 77;
      }
  goto ret0;

  L687: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      ro[1] = x5;
      goto L688;
    }
  goto ret0;

  L688: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_MODE (x4) == TImode && GET_CODE (x4) == ZERO_EXTEND && 1)
    goto L689;
  goto ret0;

  L689: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (se_register_operand (x5, DImode))
    {
      ro[2] = x5;
      goto L690;
    }
  goto ret0;

  L690: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 64 && pnum_clobbers != 0 && 1)
    if (TARGET_64BIT && !0)
      {
	*pnum_clobbers = 2;
	return 78;
      }
  goto ret0;

  L725: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == MULT && 1)
    goto L726;
  goto ret0;

  L726: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      ro[3] = x3;
      goto L727;
    }
  goto ret0;

  L727: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L728;
    }
  goto ret0;

  L728: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      ro[4] = x3;
      goto L729;
    }
  goto ret0;

  L729: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L730;
    }
  goto ret0;

  L730: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (rtx_equal_p (x2, ro[0]) && 1)
    goto L754;
  goto ret0;

  L754: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    if (TARGET_MAD
   && ! TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      {
	*pnum_clobbers = 1;
	return 80;
      }
  L755: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    if ((TARGET_MAD || TARGET_MIPS5400) /* CYGNUS LOCAL vr5400/raeburn */
   && TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
      {
	*pnum_clobbers = 2;
	return 81;
      }
  goto ret0;

  L978: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L979;
    }
  goto ret0;

  L979: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && se_nonmemory_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !optimize && !0)
	{
	  *pnum_clobbers = 2;
	  return 108;
	}
      }
  goto ret0;

  L1008: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1009;
    }
  goto ret0;

  L1009: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && se_nonmemory_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !optimize && !0)
	{
	  *pnum_clobbers = 2;
	  return 112;
	}
      }
  goto ret0;

  L1038: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1039;
    }
  goto ret0;

  L1039: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && se_nonmemory_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !optimize && !0)
	{
	  *pnum_clobbers = 2;
	  return 116;
	}
      }
  goto ret0;

  L1068: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1069;
    }
  goto ret0;

  L1069: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pnum_clobbers != 0 && se_nonmemory_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !optimize && !0)
	{
	  *pnum_clobbers = 2;
	  return 120;
	}
      }
  goto ret0;

  L1097: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 126;
      }
  goto ret0;

  L1131: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (pnum_clobbers != 0 && se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	{
	  *pnum_clobbers = 2;
	  return 130;
	}
      }
  goto ret0;

  L1162: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      return 138;
    }
  goto ret0;

  L1277: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  if (GET_CODE (x2) == NOT && 1)
    goto L1278;
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1181;
    }
  goto ret0;

  L1278: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1279;
    }
  goto ret0;

  L1279: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == NOT && 1)
    goto L1280;
  goto ret0;

  L1280: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      if (!TARGET_MIPS16)
	return 165;
      }
  goto ret0;

  L1181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    goto L1187;
  L1198: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 147;
      }
  goto ret0;

  L1187: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16)
    return 144;
  L1188: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16)
    return 145;
  goto L1198;

  L1212: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1213;
    }
  goto ret0;

  L1213: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    goto L1219;
  goto ret0;

  L1219: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16)
    return 152;
  L1220: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && TARGET_MIPS16)
    return 153;
  goto ret0;

  L1241: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1242;
    }
  goto ret0;

  L1242: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    goto L1248;
  L1254: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    goto L1265;
  goto ret0;

  L1248: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if ((TARGET_64BIT || !TARGET_DEBUG_G_MODE) && !TARGET_MIPS16)
    return 159;
  L1249: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_64BIT && TARGET_MIPS16)
    return 160;
  goto L1254;

  L1265: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 161;
  L1266: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 163;
  goto ret0;

  L1340: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (memory_operand (x2, SImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT)
	    return 178;
	  }
      break;
    case HImode:
      if (nonimmediate_operand (x2, HImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT && !TARGET_MIPS16)
	    return 183;
	  }
    L1356: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x2, HImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT && TARGET_MIPS16)
	    return 184;
	  }
      break;
    case QImode:
      if (nonimmediate_operand (x2, QImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT && !TARGET_MIPS16)
	    return 192;
	  }
    L1385: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x2, QImode))
	{
	  ro[1] = x2;
	  if (TARGET_64BIT && TARGET_MIPS16)
	    return 194;
	  }
    }
  goto ret0;

  L1489: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (general_operand (x2, BLKmode))
    {
      ro[1] = x2;
      return 224;
    }
  goto ret0;

  L1944: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1945;
    }
  goto ret0;

  L1945: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L1951;
  goto ret0;

  L1951: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 303;
  L1952: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 304;
  goto ret0;

  L2040: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2041;
    }
  goto ret0;

  L2041: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L2047;
  goto ret0;

  L2047: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 318;
  L2048: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 319;
  goto ret0;

  L2146: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2147;
    }
  goto ret0;

  L2147: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    goto L2163;
  goto ret0;

  L2163: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 335;
  L2164: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 338;
  goto ret0;

  L2156: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2157;
    }
  goto ret0;

  L2157: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS5400 && TARGET_64BIT)
	return 337;
      }
  goto ret0;

  L2249: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2250;
    }
  goto ret0;

  L2250: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && XWINT (x2, 0) == 0 && 1)
    goto L2256;
  L2272: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 371;
      }
  goto ret0;

  L2256: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 367;
  L2257: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_64BIT && TARGET_MIPS16)
    return 368;
  goto L2272;

  L2286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2287;
    }
  goto ret0;

  L2287: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && XWINT (x2, 0) == 0 && 1)
    if (TARGET_64BIT && !TARGET_MIPS16)
      return 375;
  L2302: ATTRIBUTE_UNUSED_LABEL
  if (se_uns_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 378;
      }
  goto ret0;

  L2321: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2322;
    }
  goto ret0;

  L2322: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 383;
      }
  L2327: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_MIPS16)
	return 384;
      }
  goto ret0;

  L2341: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2342;
    }
  goto ret0;

  L2342: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 388;
      }
  goto ret0;

  L2363: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2364;
    }
  goto ret0;

  L2364: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    goto L2370;
  goto ret0;

  L2370: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 393;
  L2371: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 394;
  goto ret0;

  L2387: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2410;
    }
  goto ret0;

  L2410: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 402;
      }
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    goto L2394;
  goto ret0;

  L2394: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 398;
  L2395: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 399;
  goto ret0;

  L2429: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2430;
    }
  goto ret0;

  L2430: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT)
	return 407;
      }
  L2435: ATTRIBUTE_UNUSED_LABEL
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT)
	return 408;
      }
  goto ret0;

  L2449: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2450;
    }
  goto ret0;

  L2450: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 412;
      }
  goto ret0;

  L2471: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2472;
    }
  goto ret0;

  L2472: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    goto L2478;
  goto ret0;

  L2478: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16)
    return 417;
  L2479: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16)
    return 418;
  goto ret0;

  L2495: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2518;
    }
  goto ret0;

  L2518: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_MIPS16)
	return 426;
      }
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    goto L2502;
  goto ret0;

  L2502: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && !TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 422;
  L2503: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_64BIT && TARGET_MIPS16 && INTVAL (operands[2]) < 32767)
    return 423;
  goto ret0;

  L2859: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (equality_op (x2, VOIDmode))
    {
      ro[4] = x2;
      goto L2860;
    }
  L2875: ATTRIBUTE_UNUSED_LABEL
  if (equality_op (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2876;
    }
  goto ret0;

  L2860: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    default:
      break;
    case SImode:
      if (register_operand (x3, SImode))
	{
	  ro[1] = x3;
	  goto L2861;
	}
      break;
    case DImode:
      if (se_register_operand (x3, DImode))
	{
	  ro[1] = x3;
	  goto L2869;
	}
    }
  goto L2875;

  L2861: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2862;
  goto L2875;

  L2862: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[2] = x2;
      goto L2863;
    }
  x2 = XEXP (x1, 0);
  goto L2875;

  L2863: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 || 0)
	return 498;
      }
  x2 = XEXP (x1, 0);
  goto L2875;

  L2869: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2870;
  goto L2875;

  L2870: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[2] = x2;
      goto L2871;
    }
  x2 = XEXP (x1, 0);
  goto L2875;

  L2871: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[3] = x2;
      if (mips_isa >= 4 || 0)
	return 499;
      }
  x2 = XEXP (x1, 0);
  goto L2875;

  L2876: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[4] = x3;
      goto L2877;
    }
  goto ret0;

  L2877: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2878;
  goto ret0;

  L2878: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2879;
    }
  goto ret0;

  L2879: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[2] = x2;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 500;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_5 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x1, DFmode))
	{
	  ro[0] = x1;
	  goto L2;
	}
    L1659: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, DFmode))
	{
	  ro[0] = x1;
	  goto L1660;
	}
      break;
    case SFmode:
      if (GET_CODE (x1) == MEM && 1)
	goto L1591;
      if (register_operand (x1, SFmode))
	{
	  ro[0] = x1;
	  goto L7;
	}
    L1648: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, SFmode))
	{
	  ro[0] = x1;
	  goto L1649;
	}
      break;
    case SImode:
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L12;
	}
    L16: ATTRIBUTE_UNUSED_LABEL
      if (GET_CODE (x1) == REG && XINT (x1, 0) == 29 && 1)
	goto L17;
    L21: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L22;
	}
      break;
    case DImode:
      if (register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L94;
	}
    L98: ATTRIBUTE_UNUSED_LABEL
      if (GET_CODE (x1) == REG && XINT (x1, 0) == 29 && 1)
	goto L99;
    L103: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L104;
	}
    }
  L150: ATTRIBUTE_UNUSED_LABEL
  switch (GET_MODE (x1))
    {
    default:
      break;
    case SImode:
      if (GET_CODE (x1) == REG && XINT (x1, 0) == 29 && 1)
	goto L151;
    L155: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L156;
	}
    L1424: ATTRIBUTE_UNUSED_LABEL
      if (general_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L1425;
	}
    L1535: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L1536;
	}
    L1538: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L1539;
	}
    L2931: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L2932;
	}
      break;
    case DImode:
    L232: ATTRIBUTE_UNUSED_LABEL
      switch (GET_CODE (x1))
	{
	default:
	  break;
	case REG:
	  if (XINT (x1, 0) == 29 && 1)
	    goto L233;
	  if (XINT (x1, 0) == 28 && 1)
	    goto L1687;
	}
    L237: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L238;
	}
    L1448: ATTRIBUTE_UNUSED_LABEL
      if (general_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L1449;
	}
    L1504: ATTRIBUTE_UNUSED_LABEL
      if (memory_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L1505;
	}
    L1507: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L1508;
	}
    L2934: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L2935;
	}
      break;
    case HImode:
      if (register_operand (x1, HImode))
	{
	  ro[0] = x1;
	  goto L1298;
	}
    L1614: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, HImode))
	{
	  ro[0] = x1;
	  goto L1615;
	}
      break;
    case QImode:
      if (register_operand (x1, QImode))
	{
	  ro[0] = x1;
	  goto L1302;
	}
    L1631: ATTRIBUTE_UNUSED_LABEL
      if (nonimmediate_operand (x1, QImode))
	{
	  ro[0] = x1;
	  goto L1632;
	}
      break;
    case BLKmode:
      if (memory_operand (x1, BLKmode))
	{
	  ro[0] = x1;
	  goto L1484;
	}
      break;
    case CCmode:
      if (nonimmediate_operand (x1, CCmode))
	{
	  ro[0] = x1;
	  goto L1564;
	}
    L2525: ATTRIBUTE_UNUSED_LABEL
      if (register_operand (x1, CCmode))
	{
	  ro[0] = x1;
	  goto L2526;
	}
      break;
    case DFmode:
    L1602: ATTRIBUTE_UNUSED_LABEL
      switch (GET_CODE (x1))
	{
	default:
	  break;
	case MEM:
	  goto L1603;
	}
    }
  if (GET_CODE (x1) == PC && 1)
    goto L2584;
  L2663: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x1, VOIDmode))
    {
      ro[0] = x1;
      goto L2664;
    }
  goto ret0;
 L2: ATTRIBUTE_UNUSED_LABEL
  tem = recog_1 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x1 = XEXP (x0, 0);
  goto L1659;

  L1660: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, DFmode))
    goto L1664;
  L1668: ATTRIBUTE_UNUSED_LABEL
  if (general_operand (x1, DFmode))
    goto L1672;
  x1 = XEXP (x0, 0);
  goto L1602;

  L1664: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_HARD_FLOAT && !(TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode)))
    return 275;
  L1665: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_HARD_FLOAT && (TARGET_FLOAT64 && !TARGET_64BIT)
   && TARGET_DOUBLE_FLOAT
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands [0]) == MEM
	   && ((GET_CODE (operands[1]) == CONST_INT
		&& INTVAL (operands[1]) == 0)
	       || operands[1] == CONST0_RTX (DFmode)))))
    return 276;
  goto L1668;

  L1672: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if ((TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT) && !TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DFmode)))
    return 277;
  L1673: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_MIPS16
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode)))
    return 278;
  x1 = XEXP (x0, 0);
  goto L1602;

  L1591: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != PLUS)
    {
    goto L1648;
    }
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      goto L1592;
    case DImode:
      goto L1598;
    }
  goto L1648;

  L1592: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1593;
    }
  goto L1648;

  L1593: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1594;
    }
  goto L1648;

  L1594: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, SFmode))
    {
      ro[0] = x1;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 256;
      }
  x1 = XEXP (x0, 0);
  goto L1648;

  L1598: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1599;
    }
  goto L1648;

  L1599: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L1600;
    }
  goto L1648;

  L1600: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, SFmode))
    {
      ro[0] = x1;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 257;
      }
  x1 = XEXP (x0, 0);
  goto L1648;
 L7: ATTRIBUTE_UNUSED_LABEL
  tem = recog_2 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x1 = XEXP (x0, 0);
  goto L1648;

  L1649: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, SFmode))
    goto L1653;
  L1657: ATTRIBUTE_UNUSED_LABEL
  if (general_operand (x1, SFmode))
    {
      ro[1] = x1;
      if (TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)))
	return 273;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1653: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_HARD_FLOAT
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode)))
    return 271;
  L1654: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_SOFT_FLOAT && !TARGET_MIPS16
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (SFmode)))
    return 272;
  goto L1657;

  L12: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode && GET_CODE (x1) == PLUS && 1)
    goto L13;
  x1 = XEXP (x0, 0);
  goto L16;

  L13: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L14;
    }
  x1 = XEXP (x0, 0);
  goto L16;

  L14: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (! TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768))
	return 3;
      }
  x1 = XEXP (x0, 0);
  goto L16;

  L17: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode && GET_CODE (x1) == PLUS && 1)
    goto L18;
  x1 = XEXP (x0, 0);
  goto L21;

  L18: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L19;
  x1 = XEXP (x0, 0);
  goto L21;

  L19: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[0] = x2;
      if (TARGET_MIPS16)
	return 4;
      }
  x1 = XEXP (x0, 0);
  goto L21;

  L22: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != SImode)
    {
      x1 = XEXP (x0, 0);
      goto L150;
    }
  switch (GET_CODE (x1))
    {
    default:
      break;
    case PLUS:
      goto L23;
    case MINUS:
      goto L147;
    }
  x1 = XEXP (x0, 0);
  goto L150;

  L23: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    {
      x1 = XEXP (x0, 0);
      goto L150;
    }
  if (GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L24;
  L28: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L29;
    }
  x1 = XEXP (x0, 0);
  goto L150;

  L24: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[1] = x2;
      if (TARGET_MIPS16)
	return 5;
      }
  x2 = XEXP (x1, 0);
  goto L28;

  L29: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM))
	return 6;
      }
  x1 = XEXP (x0, 0);
  goto L150;

  L147: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L148;
    }
  x1 = XEXP (x0, 0);
  goto L150;

  L148: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 27;
      }
  x1 = XEXP (x0, 0);
  goto L150;

  L94: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == PLUS && 1)
    goto L95;
  x1 = XEXP (x0, 0);
  goto L98;

  L95: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L96;
    }
  x1 = XEXP (x0, 0);
  goto L98;

  L96: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768))
	return 16;
      }
  x1 = XEXP (x0, 0);
  goto L98;

  L99: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == PLUS && 1)
    goto L100;
  x1 = XEXP (x0, 0);
  goto L103;

  L100: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L101;
  x1 = XEXP (x0, 0);
  goto L103;

  L101: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    {
      ro[0] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT)
	return 17;
      }
  x1 = XEXP (x0, 0);
  goto L103;

  L104: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != DImode)
    {
      x1 = XEXP (x0, 0);
      goto L232;
    }
  switch (GET_CODE (x1))
    {
    default:
      break;
    case PLUS:
      goto L105;
    case SIGN_EXTEND:
      goto L125;
    case MINUS:
      goto L229;
    }
  x1 = XEXP (x0, 0);
  goto L232;

  L105: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    {
      x1 = XEXP (x0, 0);
      goto L232;
    }
  if (GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L106;
  L110: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L111;
    }
  x1 = XEXP (x0, 0);
  goto L232;

  L106: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT)
	return 18;
      }
  x2 = XEXP (x1, 0);
  goto L110;

  L111: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT
   && (GET_CODE (operands[1]) != REG
       || REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[1]))
       || REGNO (operands[1]) == ARG_POINTER_REGNUM
       || REGNO (operands[1]) == FRAME_POINTER_REGNUM
       || REGNO (operands[1]) == STACK_POINTER_REGNUM)
   && (GET_CODE (operands[2]) != REG
       || REGNO (operands[2]) >= FIRST_PSEUDO_REGISTER
       || M16_REG_P (REGNO (operands[2]))
       || REGNO (operands[2]) == ARG_POINTER_REGNUM
       || REGNO (operands[2]) == FRAME_POINTER_REGNUM
       || REGNO (operands[2]) == STACK_POINTER_REGNUM))
	return 19;
      }
  x1 = XEXP (x0, 0);
  goto L232;

  L125: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == PLUS && 1)
    goto L126;
  x1 = XEXP (x0, 0);
  goto L232;

  L126: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (reg_or_0_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L127;
    }
  L132: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L133;
    }
  x1 = XEXP (x0, 0);
  goto L232;

  L127: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      ro[2] = x3;
      if (TARGET_64BIT
   && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768))
	return 22;
      }
  x3 = XEXP (x2, 0);
  goto L132;

  L133: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (arith_operand (x3, SImode))
    {
      ro[2] = x3;
      if (TARGET_MIPS16 && TARGET_64BIT)
	return 23;
      }
  x1 = XEXP (x0, 0);
  goto L232;

  L229: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_reg_or_0_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L230;
    }
  x1 = XEXP (x0, 0);
  goto L232;

  L230: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 40;
      }
  x1 = XEXP (x0, 0);
  goto L232;

  L151: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode && GET_CODE (x1) == MINUS && 1)
    goto L152;
  x1 = XEXP (x0, 0);
  goto L155;

  L152: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L153;
  x1 = XEXP (x0, 0);
  goto L155;

  L153: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[0] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 28;
      }
  x1 = XEXP (x0, 0);
  goto L155;
 L156: ATTRIBUTE_UNUSED_LABEL
  tem = recog_3 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x1 = XEXP (x0, 0);
  goto L1424;

  L1425: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode && GET_CODE (x1) == FIX && 1)
    goto L1426;
  x1 = XEXP (x0, 0);
  goto L1535;

  L1426: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (pnum_clobbers != 0 && register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	    {
	      *pnum_clobbers = 2;
	      return 207;
	    }
	  }
      break;
    case SFmode:
      if (pnum_clobbers != 0 && register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && !0)
	    {
	      *pnum_clobbers = 2;
	      return 208;
	    }
	  }
    }
  x1 = XEXP (x0, 0);
  goto L1535;

  L1536: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == SImode && GET_CODE (x1) == REG && XINT (x1, 0) == 31 && 1)
    if (TARGET_MIPS16)
      return 240;
  x1 = XEXP (x0, 0);
  goto L1538;

  L1539: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (move_operand (x1, SImode))
    goto L1543;
  L1547: ATTRIBUTE_UNUSED_LABEL
  if (move_operand (x1, SImode))
    {
      ro[1] = x1;
      if (TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[0]) == MEM
	   && GET_CODE (XEXP (operands[0], 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST
	   && mips16_gp_offset_p (XEXP (XEXP (operands[0], 0), 1))
	   && GET_CODE (operands[1]) == CONST_INT
	   && (SMALL_INT (operands[1])
	       || SMALL_INT_UNSIGNED (operands[1])))))
	return 243;
      }
  x1 = XEXP (x0, 0);
  goto L2931;

  L1543: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 241;
  L1544: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 242;
  goto L1547;

  L2932: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (address_operand (x1, SImode))
    {
      ro[1] = x1;
      if (Pmode == SImode)
	return 522;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L233: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == MINUS && 1)
    goto L234;
  x1 = XEXP (x0, 0);
  goto L237;

  L234: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == REG && XINT (x2, 0) == 29 && 1)
    goto L235;
  x1 = XEXP (x0, 0);
  goto L237;

  L235: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, DImode))
    {
      ro[0] = x2;
      if (TARGET_MIPS16
   && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != -32768))
	return 41;
      }
  x1 = XEXP (x0, 0);
  goto L237;

  L1687: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == UNSPEC_VOLATILE && XINT (x1, 1) == 2 && XVECLEN (x1, 0) == 2 && 1)
    goto L1688;
  x1 = XEXP (x0, 0);
  goto L237;

  L1688: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (address_operand (x2, DImode))
    {
      ro[0] = x2;
      goto L1689;
    }
  x1 = XEXP (x0, 0);
  goto L237;

  L1689: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 1);
  if (pnum_clobbers != 0 && register_operand (x2, DImode))
    {
      ro[1] = x2;
      *pnum_clobbers = 1;
      return 280;
    }
  x1 = XEXP (x0, 0);
  goto L237;
 L238: ATTRIBUTE_UNUSED_LABEL
  tem = recog_4 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x1 = XEXP (x0, 0);
  goto L1448;

  L1449: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == FIX && 1)
    goto L1450;
  x1 = XEXP (x0, 0);
  goto L1504;

  L1450: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (pnum_clobbers != 0 && register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	    {
	      *pnum_clobbers = 1;
	      return 209;
	    }
	  }
      break;
    case SFmode:
      if (pnum_clobbers != 0 && register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	    {
	      *pnum_clobbers = 1;
	      return 210;
	    }
	  }
    }
  x1 = XEXP (x0, 0);
  goto L1504;

  L1505: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == DImode && GET_CODE (x1) == REG && XINT (x1, 0) == 31 && 1)
    if (TARGET_MIPS16 && TARGET_64BIT)
      return 229;
  x1 = XEXP (x0, 0);
  goto L1507;

  L1508: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, DImode))
    goto L1512;
  L1519: ATTRIBUTE_UNUSED_LABEL
  if (movdi_operand (x1, DImode))
    goto L1523;
  x1 = XEXP (x0, 0);
  goto L2934;

  L1512: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (!TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode)))
    return 230;
  L1513: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (!TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)))
    return 231;
  goto L1519;

  L1523: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_64BIT && !TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || se_register_operand (operands[1], DImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
       || operands[1] == CONST0_RTX (DImode)))
    return 233;
  L1524: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_64BIT && TARGET_MIPS16
   && (register_operand (operands[0], DImode)
       || se_register_operand (operands[1], DImode)))
    return 234;
  x1 = XEXP (x0, 0);
  goto L2934;

  L2935: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (address_operand (x1, DImode))
    {
      ro[1] = x1;
      if (Pmode == DImode)
	return 523;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1298: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != HImode)
    {
      x1 = XEXP (x0, 0);
      goto L1614;
    }
  switch (GET_CODE (x1))
    {
    default:
      break;
    case TRUNCATE:
      goto L1299;
    case ZERO_EXTEND:
      goto L1335;
    case SIGN_EXTEND:
      goto L1401;
    }
  x1 = XEXP (x0, 0);
  goto L1614;

  L1299: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT)
	return 169;
      }
  x1 = XEXP (x0, 0);
  goto L1614;

  L1335: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != QImode)
    {
      x1 = XEXP (x0, 0);
      goto L1614;
    }
  if (GET_CODE (x2) == TRUNCATE && 1)
    goto L1336;
  if (nonimmediate_operand (x2, QImode))
    {
      ro[1] = x2;
      if (!TARGET_MIPS16)
	return 186;
      }
  L1364: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, QImode))
    {
      ro[1] = x2;
      if (TARGET_MIPS16)
	return 187;
      }
  x1 = XEXP (x0, 0);
  goto L1614;

  L1336: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 176;
      }
  x1 = XEXP (x0, 0);
  goto L1614;

  L1401: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (memory_operand (x2, QImode))
    {
      ro[1] = x2;
      return 201;
    }
  x1 = XEXP (x0, 0);
  goto L1614;

  L1615: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, HImode))
    goto L1619;
  L1623: ATTRIBUTE_UNUSED_LABEL
  if (general_operand (x1, HImode))
    {
      ro[1] = x1;
      if (TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)))
	return 263;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1619: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 261;
  L1620: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 262;
  goto L1623;

  L1302: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == QImode && GET_CODE (x1) == TRUNCATE && 1)
    goto L1303;
  x1 = XEXP (x0, 0);
  goto L1631;

  L1303: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (TARGET_64BIT)
	return 170;
      }
  x1 = XEXP (x0, 0);
  goto L1631;

  L1632: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, QImode))
    goto L1636;
  L1640: ATTRIBUTE_UNUSED_LABEL
  if (general_operand (x1, QImode))
    {
      ro[1] = x1;
      if (TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)))
	return 268;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1636: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 266;
  L1637: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (!TARGET_DEBUG_H_MODE && !TARGET_MIPS16
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)))
    return 267;
  goto L1640;

  L1484: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == BLKmode && GET_CODE (x1) == UNSPEC && XINT (x1, 1) == 1 && XVECLEN (x1, 0) == 1 && 1)
    goto L1485;
  x1 = XEXP (x0, 0);
  goto L2663;

  L1485: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (reg_or_0_operand (x2, SImode))
    {
      ro[1] = x2;
      if (!TARGET_MIPS16)
	return 223;
      }
  L1493: ATTRIBUTE_UNUSED_LABEL
  if (reg_or_0_operand (x2, DImode))
    {
      ro[1] = x2;
      return 225;
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1564: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (general_operand (x1, CCmode))
    {
      ro[1] = x1;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT)
	return 249;
      }
  x1 = XEXP (x0, 0);
  goto L2525;

  L2526: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != CCmode)
    {
      x1 = XEXP (x0, 0);
      goto L2663;
    }
  switch (GET_CODE (x1))
    {
    default:
      break;
    case EQ:
      goto L2527;
    case LT:
      goto L2532;
    case LE:
      goto L2537;
    case GT:
      goto L2542;
    case GE:
      goto L2547;
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2527: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  goto L2528;
	}
      break;
    case SFmode:
      if (register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  goto L2553;
	}
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2528: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 428;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2553: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 433;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2532: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  goto L2533;
	}
      break;
    case SFmode:
      if (register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  goto L2558;
	}
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2533: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 429;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2558: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 434;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2537: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  goto L2538;
	}
      break;
    case SFmode:
      if (register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  goto L2563;
	}
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2538: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 430;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2563: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 435;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2542: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  goto L2543;
	}
      break;
    case SFmode:
      if (register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  goto L2568;
	}
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2543: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 431;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2568: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 436;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2547: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x2, DFmode))
	{
	  ro[1] = x2;
	  goto L2548;
	}
      break;
    case SFmode:
      if (register_operand (x2, SFmode))
	{
	  ro[1] = x2;
	  goto L2573;
	}
    }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2548: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 432;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L2573: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 437;
      }
  x1 = XEXP (x0, 0);
  goto L2663;

  L1603: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != PLUS)
    goto ret0;
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      goto L1604;
    case DImode:
      goto L1610;
    }
  goto ret0;

  L1604: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1605;
    }
  goto ret0;

  L1605: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1606;
    }
  goto ret0;

  L1606: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      ro[0] = x1;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 258;
      }
  goto ret0;

  L1610: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1611;
    }
  goto ret0;

  L1611: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L1612;
    }
  goto ret0;

  L1612: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      ro[0] = x1;
      if (mips_isa >= 4 && TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 259;
      }
  goto ret0;

  L2584: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_MODE (x1))
    {
    default:
      break;
    case SImode:
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  if (!(Pmode == DImode))
	    return 441;
	  }
      break;
    case DImode:
      if (se_register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  if (Pmode == DImode)
	    return 442;
	  }
    }
  switch (GET_CODE (x1))
    {
    default:
      break;
    case IF_THEN_ELSE:
      goto L2173;
    case LABEL_REF:
      goto L2577;
    }
  goto ret0;

  L2173: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case CCmode:
      switch (GET_CODE (x2))
	{
	default:
	  break;
	case NE:
	  goto L2174;
	case EQ:
	  goto L2182;
	}
      break;
    case SImode:
      if (cmp_op (x2, SImode))
	{
	  ro[0] = x2;
	  goto L2190;
	}
    L2197: ATTRIBUTE_UNUSED_LABEL
      if (equality_op (x2, SImode))
	{
	  ro[0] = x2;
	  goto L2198;
	}
      break;
    case DImode:
      if (cmp_op (x2, DImode))
	{
	  ro[0] = x2;
	  goto L2206;
	}
    L2213: ATTRIBUTE_UNUSED_LABEL
      if (equality_op (x2, DImode))
	{
	  ro[0] = x2;
	  goto L2214;
	}
    }
  goto ret0;

  L2174: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[0] = x3;
      goto L2175;
    }
  goto ret0;

  L2175: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2176;
  goto ret0;

  L2176: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[1] = x2;
      goto L2177;
    }
  goto ret0;

  L2177: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 346;
      }
  goto ret0;

  L2182: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, CCmode))
    {
      ro[0] = x3;
      goto L2183;
    }
  goto ret0;

  L2183: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2184;
  goto ret0;

  L2184: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[1] = x2;
      goto L2185;
    }
  goto ret0;

  L2185: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT)
	return 347;
      }
  goto ret0;

  L2190: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L2191;
    }
  goto L2197;

  L2191: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2192;
  goto L2197;

  L2192: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      goto L2193;
    }
  x2 = XEXP (x1, 0);
  goto L2197;

  L2193: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      if (!TARGET_MIPS16)
	return 348;
      }
  x2 = XEXP (x1, 0);
  goto L2197;

  L2198: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L2223;
    }
  goto ret0;

  L2223: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L2224;
    }
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2200;
  goto ret0;

  L2224: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2225;
    }
  goto ret0;

  L2225: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[4] = x2;
      if (!TARGET_MIPS16)
	return 352;
      }
  goto ret0;

  L2200: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      goto L2201;
    }
  goto ret0;

  L2201: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      if (TARGET_MIPS16)
	return 349;
      }
  goto ret0;

  L2206: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L2207;
    }
  goto L2213;

  L2207: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2208;
  goto L2213;

  L2208: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      goto L2209;
    }
  x2 = XEXP (x1, 0);
  goto L2213;

  L2209: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      if (!TARGET_MIPS16)
	return 350;
      }
  x2 = XEXP (x1, 0);
  goto L2213;

  L2214: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L2231;
    }
  goto ret0;

  L2231: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L2232;
    }
  if (GET_CODE (x3) == CONST_INT && XWINT (x3, 0) == 0 && 1)
    goto L2216;
  goto ret0;

  L2232: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2233;
    }
  goto ret0;

  L2233: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[4] = x2;
      if (!TARGET_MIPS16)
	return 353;
      }
  goto ret0;

  L2216: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      goto L2217;
    }
  goto ret0;

  L2217: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 2);
  if (pc_or_label_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      if (TARGET_MIPS16)
	return 351;
      }
  goto ret0;

  L2577: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  ro[0] = x2;
  if (!TARGET_MIPS16)
    return 438;
  L2581: ATTRIBUTE_UNUSED_LABEL
  ro[0] = x2;
  if (TARGET_MIPS16 && GET_CODE (operands[0]) != REG)
    return 439;
  goto ret0;

  L2664: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_CODE (x1))
    {
    default:
      break;
    case UNSPEC:
      if (XINT (x1, 1) == 1 && XVECLEN (x1, 0) == 1 && 1)
	goto L2665;
      break;
    case CALL:
      goto L2813;
    }
  goto ret0;

  L2665: ATTRIBUTE_UNUSED_LABEL
  x2 = XVECEXP (x1, 0, 0);
  if (pnum_clobbers != 0 && 1)
    {
      ro[1] = x2;
      if (TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF)
	{
	  *pnum_clobbers = 1;
	  return 464;
	}
      }
  goto ret0;

  L2813: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == MEM && 1)
    goto L2814;
  goto ret0;

  L2814: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_insn_operand (x3, VOIDmode))
    {
      ro[1] = x3;
      goto L2815;
    }
  goto ret0;

  L2815: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  ro[2] = x2;
  if (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
    return 490;
  goto ret0;
 ret0:
  return -1;
}

int
recog_6 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case PLUS:
      goto L714;
    case MINUS:
      goto L178;
    case MULT:
      goto L480;
    case DIV:
      goto L875;
    case UDIV:
      goto L923;
    case NEG:
      goto L1140;
    case ASHIFT:
      goto L1883;
    case ASHIFTRT:
      goto L1979;
    case LSHIFTRT:
      goto L2085;
    }
  goto ret0;

  L714: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) != DImode)
    goto ret0;
  if (GET_CODE (x3) == MULT && 1)
    goto L715;
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L45;
    }
  goto ret0;

  L715: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      ro[3] = x4;
      goto L716;
    }
  goto ret0;

  L716: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[1] = x5;
      goto L717;
    }
  goto ret0;

  L717: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      ro[4] = x4;
      goto L718;
    }
  goto ret0;

  L718: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[2] = x5;
      goto L719;
    }
  goto ret0;

  L719: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L720;
  goto ret0;

  L720: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L721;
  goto ret0;

  L721: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      if (TARGET_MAD
   && ! TARGET_64BIT
   && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 80;
      }
  goto ret0;

  L45: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L46;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      goto L72;
    }
  goto ret0;

  L46: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L47;
  goto ret0;

  L47: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 10;
      }
  goto ret0;

  L72: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L73;
  goto ret0;

  L73: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (TARGET_GAS
       || GET_CODE (operands[2]) != CONST_INT
       || INTVAL (operands[2]) != -32768))
	return 13;
      }
  goto ret0;

  L178: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L179;
    }
  goto ret0;

  L179: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L180;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      goto L206;
    }
  goto ret0;

  L180: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L181;
  goto ret0;

  L181: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 34;
      }
  goto ret0;

  L206: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L207;
  goto ret0;

  L207: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && INTVAL (operands[2]) != -32768)
	return 37;
      }
  goto ret0;

  L480: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (extend_operator (x3, DImode))
    {
      ro[3] = x3;
      goto L481;
    }
  goto ret0;

  L481: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L482;
    }
  goto ret0;

  L482: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      ro[4] = x3;
      goto L483;
    }
  goto ret0;

  L483: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L484;
    }
  goto ret0;

  L484: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L485;
  goto ret0;

  L485: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      if (!TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 68;
      }
  goto ret0;

  L875: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L876;
    }
  goto ret0;

  L876: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L877;
    }
  goto ret0;

  L877: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L878;
  goto ret0;

  L878: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L879;
    }
  goto ret0;

  L879: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == MOD && 1)
    goto L880;
  goto ret0;

  L880: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L881;
  goto ret0;

  L881: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && pnum_clobbers != 0 && 1)
    if (TARGET_64BIT && optimize && !0)
      {
	*pnum_clobbers = 1;
	return 97;
      }
  goto ret0;

  L923: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L924;
    }
  goto ret0;

  L924: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L925;
    }
  goto ret0;

  L925: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L926;
  goto ret0;

  L926: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L927;
    }
  goto ret0;

  L927: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == UMOD && 1)
    goto L928;
  goto ret0;

  L928: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L929;
  goto ret0;

  L929: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && pnum_clobbers != 0 && 1)
    if (TARGET_64BIT && optimize && !0)
      {
	*pnum_clobbers = 1;
	return 101;
      }
  goto ret0;

  L1140: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1141;
    }
  goto ret0;

  L1141: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1142;
  goto ret0;

  L1142: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (! TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 133;
      }
  goto ret0;

  L1883: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1884;
    }
  goto ret0;

  L1884: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1885;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L1893;
    }
  goto ret0;

  L1885: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1886;
  goto ret0;

  L1886: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 296;
      }
  goto ret0;

  L1893: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1894;
  goto ret0;

  L1894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L1921;
  goto ret0;

  L1921: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0)
    return 297;
  L1922: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return 300;
  goto ret0;

  L1979: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1980;
    }
  goto ret0;

  L1980: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1981;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L1989;
    }
  goto ret0;

  L1981: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1982;
  goto ret0;

  L1982: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 311;
      }
  goto ret0;

  L1989: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1990;
  goto ret0;

  L1990: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L2017;
  goto ret0;

  L2017: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && (INTVAL (operands[2]) & 32) != 0)
    return 312;
  L2018: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return 315;
  goto ret0;

  L2085: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L2086;
    }
  goto ret0;

  L2086: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L2087;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L2095;
    }
  goto ret0;

  L2087: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2088;
  goto ret0;

  L2088: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16)
	return 328;
      }
  goto ret0;

  L2095: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2096;
  goto ret0;

  L2096: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L2123;
  goto ret0;

  L2123: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 32) != 0)
    return 329;
  L2124: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_64BIT && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return 332;
  goto ret0;
 ret0:
  return -1;
}

int
recog_7 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case DImode:
      if (register_operand (x2, DImode))
	{
	  ro[0] = x2;
	  goto L43;
	}
    L1442: ATTRIBUTE_UNUSED_LABEL
      if (general_operand (x2, DImode))
	{
	  ro[0] = x2;
	  goto L1443;
	}
      break;
    case SImode:
      if (register_operand (x2, SImode))
	{
	  ro[0] = x2;
	  goto L850;
	}
    }
  L1679: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x2))
    {
    default:
      break;
    case REG:
      if (GET_MODE (x2) == DImode && XINT (x2, 0) == 28 && 1)
	goto L1680;
      break;
    case PC:
      goto L2591;
    }
  L2657: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, VOIDmode))
    {
      ro[0] = x2;
      goto L2658;
    }
  goto ret0;
 L43: ATTRIBUTE_UNUSED_LABEL
  tem = recog_6 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x2 = XEXP (x1, 0);
  goto L1442;

  L1443: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == FIX && 1)
    goto L1444;
  x2 = XEXP (x1, 0);
  goto L1679;

  L1444: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x3, DFmode))
	{
	  ro[1] = x3;
	  goto L1445;
	}
      break;
    case SFmode:
      if (register_operand (x3, SFmode))
	{
	  ro[1] = x3;
	  goto L1456;
	}
    }
  x2 = XEXP (x1, 0);
  goto L1679;

  L1445: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1446;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1679;

  L1446: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	return 209;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1679;

  L1456: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1457;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1679;

  L1457: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      ro[2] = x2;
      if (TARGET_HARD_FLOAT && TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	return 210;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L1679;

  L850: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    {
      x2 = XEXP (x1, 0);
      goto L2657;
    }
  switch (GET_CODE (x2))
    {
    default:
      break;
    case DIV:
      goto L851;
    case UDIV:
      goto L899;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L851: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L852;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L852: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L853;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L853: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L854;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L854: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L855;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L855: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == MOD && 1)
    goto L856;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L856: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L857;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L857: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && pnum_clobbers != 0 && 1)
    if (optimize && !0)
      {
	*pnum_clobbers = 1;
	return 95;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L899: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L900;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L900: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L901;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L901: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L902;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L902: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L903;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L903: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == UMOD && 1)
    goto L904;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L904: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L905;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L905: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && pnum_clobbers != 0 && 1)
    if (optimize && !0)
      {
	*pnum_clobbers = 1;
	return 99;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L1680: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == UNSPEC_VOLATILE && XINT (x2, 1) == 2 && XVECLEN (x2, 0) == 2 && 1)
    goto L1681;
  x2 = XEXP (x1, 0);
  goto L2657;

  L1681: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  if (address_operand (x3, DImode))
    {
      ro[0] = x3;
      goto L1682;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L1682: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 1);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1683;
    }
  x2 = XEXP (x1, 0);
  goto L2657;

  L1683: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1684;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L1684: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == REG && XINT (x2, 0) == 1 && 1)
    return 280;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2657;

  L2591: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (register_operand (x2, SImode))
	{
	  ro[0] = x2;
	  goto L2592;
	}
      break;
    case DImode:
      if (GET_CODE (x2) == PLUS && 1)
	goto L2616;
      if (se_register_operand (x2, DImode))
	{
	  ro[0] = x2;
	  goto L2599;
	}
    }
  if (GET_MODE (x2) != SImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case PLUS:
      goto L2606;
    case MEM:
      goto L2640;
    }
  goto ret0;

  L2592: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE && 1)
    goto L2593;
  goto ret0;

  L2593: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF && 1)
    goto L2594;
  goto ret0;

  L2594: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  ro[1] = x3;
  if (!(Pmode == DImode))
    return 444;
  goto ret0;

  L2616: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[0] = x3;
      goto L2617;
    }
  goto ret0;

  L2617: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode && GET_CODE (x3) == LABEL_REF && 1)
    goto L2618;
  goto ret0;

  L2618: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  ro[1] = x4;
  goto L2619;

  L2619: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE && 1)
    goto L2620;
  goto ret0;

  L2620: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == LABEL_REF && 1)
    goto L2621;
  goto ret0;

  L2621: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    if (Pmode == DImode && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1])
      return 451;
  goto ret0;

  L2599: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE && 1)
    goto L2600;
  goto ret0;

  L2600: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == LABEL_REF && 1)
    goto L2601;
  goto ret0;

  L2601: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  ro[1] = x3;
  if (Pmode == DImode)
    return 445;
  goto ret0;

  L2606: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[0] = x3;
      goto L2607;
    }
  goto ret0;

  L2607: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == LABEL_REF && 1)
    goto L2608;
  goto ret0;

  L2608: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  ro[1] = x4;
  goto L2609;

  L2609: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == USE && 1)
    goto L2610;
  goto ret0;

  L2610: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == LABEL_REF && 1)
    goto L2611;
  goto ret0;

  L2611: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    if (!(Pmode == DImode) && next_active_insn (insn) != 0
   && GET_CODE (PATTERN (next_active_insn (insn))) == ADDR_DIFF_VEC
   && PREV_INSN (next_active_insn (insn)) == operands[1])
      return 449;
  goto ret0;

  L2640: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == PLUS && 1)
    goto L2641;
  goto ret0;

  L2641: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode && GET_CODE (x4) == MULT && 1)
    goto L2642;
  goto ret0;

  L2642: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[0] = x5;
      goto L2643;
    }
  goto ret0;

  L2643: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT && XWINT (x5, 0) == 4 && 1)
    goto L2644;
  goto ret0;

  L2644: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF && 1)
    goto L2645;
  goto ret0;

  L2645: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  ro[1] = x5;
  goto L2646;

  L2646: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2647;
  goto ret0;

  L2647: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (pnum_clobbers != 0 && register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_EMBEDDED_PIC)
	{
	  *pnum_clobbers = 1;
	  return 453;
	}
      }
  goto ret0;

  L2658: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  switch (GET_CODE (x2))
    {
    default:
      break;
    case UNSPEC:
      if (XINT (x2, 1) == 1 && XVECLEN (x2, 0) == 1 && 1)
	goto L2659;
      break;
    case CALL:
      goto L2736;
    }
  goto ret0;

  L2659: ATTRIBUTE_UNUSED_LABEL
  x3 = XVECEXP (x2, 0, 0);
  ro[1] = x3;
  goto L2660;

  L2660: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2661;
  goto ret0;

  L2661: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == REG && XINT (x2, 0) == 31 && 1)
    if (TARGET_EMBEDDED_PIC
   && GET_CODE (operands[1]) == SYMBOL_REF)
      return 464;
  goto ret0;

  L2736: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) != MEM)
    goto ret0;
  goto L2737;
  L2765: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x3) != MEM)
    goto ret0;
  switch (GET_MODE (x3))
    {
    default:
      break;
    case SImode:
      goto L2766;
    case DImode:
      goto L2775;
    }
  goto ret0;

  L2737: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_insn_operand (x4, VOIDmode))
    {
      ro[1] = x4;
      goto L2738;
    }
  goto L2765;

  L2738: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  ro[2] = x3;
  goto L2739;

  L2739: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2740;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L2765;

  L2740: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    {
      x1 = XVECEXP (x0, 0, 0);
      x2 = XEXP (x1, 1);
      x3 = XEXP (x2, 0);
      goto L2765;
    }
  if (register_operand (x2, SImode))
    goto L2750;
  L2760: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (TARGET_ABICALLS && !TARGET_LONG_CALLS)
	return 482;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L2765;

  L2750: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31)
    return 480;
  L2751: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
    return 481;
  goto L2760;

  L2766: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L2767;
    }
  goto ret0;

  L2767: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  ro[2] = x3;
  goto L2768;

  L2768: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2769;
  goto ret0;

  L2769: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L2788;
  L2798: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
	return 486;
      }
  goto ret0;

  L2788: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_MIPS16 
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 483;
  L2789: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[3]) == REG && REGNO (operands[3]) == 31)
    return 485;
  goto L2798;

  L2775: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (se_register_operand (x4, DImode))
    {
      ro[1] = x4;
      goto L2776;
    }
  goto ret0;

  L2776: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  ro[2] = x3;
  goto L2777;

  L2777: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2778;
  goto ret0;

  L2778: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L2808;
  goto ret0;

  L2808: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (!TARGET_MIPS16 
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 484;
  L2809: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 487;
  goto ret0;
 ret0:
  return -1;
}

int
recog_8 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (register_operand (x2, SImode))
	{
	  ro[0] = x2;
	  goto L295;
	}
      break;
    case DImode:
      if (register_operand (x2, DImode))
	{
	  ro[0] = x2;
	  goto L460;
	}
    }
  goto ret0;

  L295: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case MULT:
      goto L296;
    case NEG:
      goto L388;
    case TRUNCATE:
      goto L587;
    }
  goto ret0;

  L296: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L297;
    }
  goto ret0;

  L297: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L298;
    }
  goto ret0;

  L298: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L299;
  goto ret0;

  L299: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L300;
    }
  goto ret0;

  L300: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L301;
  goto ret0;

  L301: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L302;
    }
  goto ret0;

  L302: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L303;
  goto ret0;

  L303: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    goto L343;
  goto ret0;

  L343: ATTRIBUTE_UNUSED_LABEL
  ro[5] = x2;
  if ((GENERATE_MULT3
    || TARGET_MIPS5400 /* CYGNUS LOCAL vr5400/raeburn */
    || TARGET_MAD)
   && !0)
    return 55;
  L344: ATTRIBUTE_UNUSED_LABEL
  ro[5] = x2;
  if (mips_cpu == PROCESSOR_R4000 && !TARGET_MIPS16)
    return 57;
  goto ret0;

  L388: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L389;
  goto ret0;

  L389: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L390;
    }
  goto ret0;

  L390: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L391;
    }
  goto ret0;

  L391: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L392;
  goto ret0;

  L392: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L393;
    }
  goto ret0;

  L393: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L394;
  goto ret0;

  L394: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L395;
    }
  goto ret0;

  L395: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L396;
  goto ret0;

  L396: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      if (TARGET_MIPS5400)
	return 60;
      }
  goto ret0;

  L587: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (highpart_shift_operator (x3, DImode))
    {
      ro[5] = x3;
      goto L588;
    }
  goto ret0;

  L588: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) != DImode)
    goto ret0;
  switch (GET_CODE (x4))
    {
    default:
      break;
    case MULT:
      goto L589;
    case NEG:
      goto L618;
    }
  goto ret0;

  L589: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      ro[3] = x5;
      goto L590;
    }
  goto ret0;

  L590: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[1] = x6;
      goto L591;
    }
  goto ret0;

  L591: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      ro[4] = x5;
      goto L592;
    }
  goto ret0;

  L592: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[2] = x6;
      goto L593;
    }
  goto ret0;

  L593: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT && XWINT (x4, 0) == 32 && 1)
    goto L594;
  goto ret0;

  L594: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L595;
  goto ret0;

  L595: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L596;
    }
  goto ret0;

  L596: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L597;
  goto ret0;

  L597: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      goto L598;
    }
  goto ret0;

  L598: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L599;
  goto ret0;

  L599: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[8] = x2;
      if (TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 75;
      }
  goto ret0;

  L618: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) == DImode && GET_CODE (x5) == MULT && 1)
    goto L619;
  goto ret0;

  L619: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (extend_operator (x6, DImode))
    {
      ro[3] = x6;
      goto L620;
    }
  goto ret0;

  L620: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (register_operand (x7, SImode))
    {
      ro[1] = x7;
      goto L621;
    }
  goto ret0;

  L621: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 1);
  if (extend_operator (x6, DImode))
    {
      ro[4] = x6;
      goto L622;
    }
  goto ret0;

  L622: ATTRIBUTE_UNUSED_LABEL
  x7 = XEXP (x6, 0);
  if (register_operand (x7, SImode))
    {
      ro[2] = x7;
      goto L623;
    }
  goto ret0;

  L623: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT && XWINT (x4, 0) == 32 && 1)
    goto L624;
  goto ret0;

  L624: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L625;
  goto ret0;

  L625: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L626;
    }
  goto ret0;

  L626: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L627;
  goto ret0;

  L627: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      goto L628;
    }
  goto ret0;

  L628: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L629;
  goto ret0;

  L629: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[8] = x2;
      if (TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 76;
      }
  goto ret0;

  L460: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == MULT && 1)
    goto L461;
  goto ret0;

  L461: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L462;
    }
  goto ret0;

  L462: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L463;
    }
  goto ret0;

  L463: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L464;
  goto ret0;

  L464: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L465;
    }
  goto ret0;

  L465: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L466;
  goto ret0;

  L466: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[4] = x2;
      goto L467;
    }
  goto ret0;

  L467: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L468;
  goto ret0;

  L468: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[5] = x2;
      if (TARGET_64BIT && (GENERATE_MULT3 || mips_cpu == PROCESSOR_R4000 || TARGET_MIPS16) && !0)
	return 65;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_9 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case MULT:
      goto L313;
    case TRUNCATE:
      goto L562;
    case PLUS:
      goto L695;
    case DIV:
      goto L838;
    case UDIV:
      goto L886;
    case MOD:
      goto L984;
    case UMOD:
      goto L1044;
    case FFS:
      goto L1110;
    }
  goto ret0;

  L313: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L314;
    }
  goto ret0;

  L314: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L315;
    }
  goto ret0;

  L315: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L316;
  goto ret0;

  L316: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L317;
    }
  goto ret0;

  L317: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L318;
  goto ret0;

  L318: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (mips_cpu != PROCESSOR_R4000 || TARGET_MIPS16)
	return 56;
      }
  goto ret0;

  L562: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (highpart_shift_operator (x3, DImode))
    {
      ro[5] = x3;
      goto L563;
    }
  goto ret0;

  L563: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == DImode && GET_CODE (x4) == MULT && 1)
    goto L564;
  goto ret0;

  L564: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (extend_operator (x5, DImode))
    {
      ro[3] = x5;
      goto L565;
    }
  goto ret0;

  L565: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[1] = x6;
      goto L566;
    }
  goto ret0;

  L566: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (extend_operator (x5, DImode))
    {
      ro[4] = x5;
      goto L567;
    }
  goto ret0;

  L567: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (register_operand (x6, SImode))
    {
      ro[2] = x6;
      goto L568;
    }
  goto ret0;

  L568: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT && XWINT (x4, 0) == 32 && 1)
    goto L569;
  goto ret0;

  L569: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L570;
  goto ret0;

  L570: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L571;
    }
  goto ret0;

  L571: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L572;
  goto ret0;

  L572: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if (! TARGET_MIPS5400 && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 74;
      }
  goto ret0;

  L695: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L696;
  goto ret0;

  L696: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L697;
    }
  goto ret0;

  L697: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L698;
    }
  goto ret0;

  L698: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L699;
  goto ret0;

  L699: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L700;
  goto ret0;

  L700: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L701;
    }
  goto ret0;

  L701: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L702;
  goto ret0;

  L702: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_MAD)
	return 79;
      }
  goto ret0;

  L838: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L839;
    }
  goto ret0;

  L839: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L840;
    }
  L955: ATTRIBUTE_UNUSED_LABEL
  if (nonmemory_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L956;
    }
  goto ret0;

  L840: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L841;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L841: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L842;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L842: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == MOD && 1)
    goto L843;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L843: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L844;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L844: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && 1)
    goto L845;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L845: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L846;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L846: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      if (optimize && !0)
	return 95;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L955;

  L956: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L957;
  goto ret0;

  L957: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L958;
    }
  goto ret0;

  L958: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L959;
  goto ret0;

  L959: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (!optimize)
	return 106;
      }
  goto ret0;

  L886: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L887;
    }
  goto ret0;

  L887: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L888;
    }
  L1015: ATTRIBUTE_UNUSED_LABEL
  if (nonmemory_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1016;
    }
  goto ret0;

  L888: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L889;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L889: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L890;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L890: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == UMOD && 1)
    goto L891;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L891: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L892;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L892: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && 1)
    goto L893;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L893: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L894;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L894: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      if (optimize && !0)
	return 99;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1015;

  L1016: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1017;
  goto ret0;

  L1017: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L1018;
    }
  goto ret0;

  L1018: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1019;
  goto ret0;

  L1019: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (!optimize)
	return 114;
      }
  goto ret0;

  L984: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L985;
    }
  goto ret0;

  L985: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L986;
    }
  goto ret0;

  L986: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L987;
  goto ret0;

  L987: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L988;
    }
  goto ret0;

  L988: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L989;
  goto ret0;

  L989: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (!optimize)
	return 110;
      }
  goto ret0;

  L1044: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1045;
    }
  goto ret0;

  L1045: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (nonmemory_operand (x3, SImode))
    {
      ro[2] = x3;
      goto L1046;
    }
  goto ret0;

  L1046: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1047;
  goto ret0;

  L1047: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L1048;
    }
  goto ret0;

  L1048: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1049;
  goto ret0;

  L1049: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (!optimize)
	return 118;
      }
  goto ret0;

  L1110: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1111;
    }
  goto ret0;

  L1111: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1112;
  goto ret0;

  L1112: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L1113;
    }
  goto ret0;

  L1113: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1114;
  goto ret0;

  L1114: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      if (!TARGET_MIPS16)
	return 129;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog_10 (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case MULT:
      goto L446;
    case NEG:
      goto L518;
    case MINUS:
      goto L539;
    case TRUNCATE:
      goto L645;
    case PLUS:
      goto L735;
    case DIV:
      goto L862;
    case UDIV:
      goto L910;
    case MOD:
      goto L999;
    case UMOD:
      goto L1059;
    case FFS:
      goto L1123;
    }
  goto ret0;

  L446: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) != DImode)
    goto ret0;
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L447;
    }
  L497: ATTRIBUTE_UNUSED_LABEL
  if (extend_operator (x3, DImode))
    {
      ro[3] = x3;
      goto L498;
    }
  goto ret0;

  L447: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L448;
    }
  x3 = XEXP (x2, 0);
  goto L497;

  L448: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L449;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L497;

  L449: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L450;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L497;

  L450: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L451;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L497;

  L451: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && mips_cpu != PROCESSOR_R4000 && !TARGET_MIPS16 && !0)
	return 64;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 0);
  goto L497;

  L498: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L499;
    }
  goto ret0;

  L499: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (extend_operator (x3, DImode))
    {
      ro[4] = x3;
      goto L500;
    }
  goto ret0;

  L500: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L501;
    }
  goto ret0;

  L501: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L502;
  goto ret0;

  L502: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[5] = x2;
      goto L503;
    }
  goto ret0;

  L503: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L504;
  goto ret0;

  L504: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[6] = x2;
      if (TARGET_64BIT && !0 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 69;
      }
  goto ret0;

  L518: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode && GET_CODE (x3) == MULT && 1)
    goto L519;
  goto ret0;

  L519: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      ro[3] = x4;
      goto L520;
    }
  goto ret0;

  L520: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[1] = x5;
      goto L521;
    }
  goto ret0;

  L521: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      ro[4] = x4;
      goto L522;
    }
  goto ret0;

  L522: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[2] = x5;
      goto L523;
    }
  goto ret0;

  L523: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L524;
  goto ret0;

  L524: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L525;
    }
  goto ret0;

  L525: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L526;
  goto ret0;

  L526: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      if (TARGET_64BIT && TARGET_MIPS5400 && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 70;
      }
  goto ret0;

  L539: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[3] = x3;
      goto L540;
    }
  goto ret0;

  L540: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == DImode && GET_CODE (x3) == MULT && 1)
    goto L541;
  goto ret0;

  L541: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      ro[4] = x4;
      goto L542;
    }
  goto ret0;

  L542: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[1] = x5;
      goto L543;
    }
  goto ret0;

  L543: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      ro[5] = x4;
      goto L544;
    }
  goto ret0;

  L544: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[2] = x5;
      goto L545;
    }
  goto ret0;

  L545: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L546;
  goto ret0;

  L546: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L547;
    }
  goto ret0;

  L547: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L548;
  goto ret0;

  L548: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if (TARGET_64BIT && TARGET_MIPS5400 && GET_CODE (operands[4]) == GET_CODE (operands[5]))
	return 71;
      }
  goto ret0;

  L645: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == TImode && GET_CODE (x3) == LSHIFTRT && 1)
    goto L646;
  goto ret0;

  L646: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == TImode && GET_CODE (x4) == MULT && 1)
    goto L647;
  goto ret0;

  L647: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (GET_MODE (x5) != TImode)
    goto ret0;
  switch (GET_CODE (x5))
    {
    default:
      break;
    case SIGN_EXTEND:
      goto L648;
    case ZERO_EXTEND:
      goto L673;
    }
  goto ret0;

  L648: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      ro[1] = x6;
      goto L649;
    }
  goto ret0;

  L649: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode && GET_CODE (x5) == SIGN_EXTEND && 1)
    goto L650;
  goto ret0;

  L650: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      ro[2] = x6;
      goto L651;
    }
  goto ret0;

  L651: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT && XWINT (x4, 0) == 64 && 1)
    goto L652;
  goto ret0;

  L652: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L653;
  goto ret0;

  L653: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L654;
    }
  goto ret0;

  L654: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L655;
  goto ret0;

  L655: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !0)
	return 77;
      }
  goto ret0;

  L673: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      ro[1] = x6;
      goto L674;
    }
  goto ret0;

  L674: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_MODE (x5) == TImode && GET_CODE (x5) == ZERO_EXTEND && 1)
    goto L675;
  goto ret0;

  L675: ATTRIBUTE_UNUSED_LABEL
  x6 = XEXP (x5, 0);
  if (se_register_operand (x6, DImode))
    {
      ro[2] = x6;
      goto L676;
    }
  goto ret0;

  L676: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == CONST_INT && XWINT (x4, 0) == 64 && 1)
    goto L677;
  goto ret0;

  L677: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L678;
  goto ret0;

  L678: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L679;
    }
  goto ret0;

  L679: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L680;
  goto ret0;

  L680: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !0)
	return 78;
      }
  goto ret0;

  L735: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == DImode && GET_CODE (x3) == MULT && 1)
    goto L736;
  goto ret0;

  L736: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (extend_operator (x4, DImode))
    {
      ro[3] = x4;
      goto L737;
    }
  goto ret0;

  L737: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[1] = x5;
      goto L738;
    }
  goto ret0;

  L738: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (extend_operator (x4, DImode))
    {
      ro[4] = x4;
      goto L739;
    }
  goto ret0;

  L739: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[2] = x5;
      goto L740;
    }
  goto ret0;

  L740: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L741;
  goto ret0;

  L741: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L742;
  goto ret0;

  L742: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L743;
    }
  goto ret0;

  L743: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L744;
  goto ret0;

  L744: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      if ((TARGET_MAD || TARGET_MIPS5400) /* CYGNUS LOCAL vr5400/raeburn */
   && TARGET_64BIT && GET_CODE (operands[3]) == GET_CODE (operands[4]))
	return 81;
      }
  goto ret0;

  L862: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L863;
    }
  goto ret0;

  L863: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L864;
    }
  L970: ATTRIBUTE_UNUSED_LABEL
  if (se_nonmemory_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L971;
    }
  goto ret0;

  L864: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L865;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L865: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L866;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L866: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == MOD && 1)
    goto L867;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L867: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L868;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L868: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && 1)
    goto L869;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L869: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L870;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L870: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[6] = x2;
      if (TARGET_64BIT && optimize && !0)
	return 97;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L970;

  L971: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L972;
  goto ret0;

  L972: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L973;
    }
  goto ret0;

  L973: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L974;
  goto ret0;

  L974: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !optimize && !0)
	return 108;
      }
  goto ret0;

  L910: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L911;
    }
  goto ret0;

  L911: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L912;
    }
  L1030: ATTRIBUTE_UNUSED_LABEL
  if (se_nonmemory_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L1031;
    }
  goto ret0;

  L912: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L913;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L913: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[3] = x2;
      goto L914;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L914: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == UMOD && 1)
    goto L915;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L915: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[1]) && 1)
    goto L916;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L916: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && 1)
    goto L917;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L917: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L918;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L918: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[6] = x2;
      if (TARGET_64BIT && optimize && !0)
	return 101;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  x3 = XEXP (x2, 1);
  goto L1030;

  L1031: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1032;
  goto ret0;

  L1032: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L1033;
    }
  goto ret0;

  L1033: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1034;
  goto ret0;

  L1034: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !optimize && !0)
	return 116;
      }
  goto ret0;

  L999: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1000;
    }
  goto ret0;

  L1000: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_nonmemory_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L1001;
    }
  goto ret0;

  L1001: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1002;
  goto ret0;

  L1002: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L1003;
    }
  goto ret0;

  L1003: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1004;
  goto ret0;

  L1004: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !optimize && !0)
	return 112;
      }
  goto ret0;

  L1059: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1060;
    }
  goto ret0;

  L1060: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (se_nonmemory_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L1061;
    }
  goto ret0;

  L1061: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1062;
  goto ret0;

  L1062: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[3] = x2;
      goto L1063;
    }
  goto ret0;

  L1063: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1064;
  goto ret0;

  L1064: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_64BIT && !optimize && !0)
	return 120;
      }
  goto ret0;

  L1123: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1124;
    }
  goto ret0;

  L1124: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1125;
  goto ret0;

  L1125: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[2] = x2;
      goto L1126;
    }
  goto ret0;

  L1126: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1127;
  goto ret0;

  L1127: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DImode))
    {
      ro[3] = x2;
      if (TARGET_64BIT && !TARGET_MIPS16)
	return 130;
      }
  goto ret0;
 ret0:
  return -1;
}

int
recog (x0, insn, pnum_clobbers)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     int *pnum_clobbers ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  int tem ATTRIBUTE_UNUSED;

  L0: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x0))
    {
    default:
      break;
    case SET:
      goto L1;
    case PARALLEL:
      if (XVECLEN (x0, 0) == 2 && 1)
	goto L41;
      if (XVECLEN (x0, 0) == 4 && 1)
	goto L293;
      if (XVECLEN (x0, 0) == 3 && 1)
	goto L310;
      if (XVECLEN (x0, 0) == 5 && 1)
	goto L346;
      if (XVECLEN (x0, 0) == 8 && 1)
	goto L1691;
      break;
    case TRAP_IF:
      goto L931;
    case UNSPEC_VOLATILE:
      if (XINT (x0, 1) == 0 && XVECLEN (x0, 0) == 1 && 1)
	goto L2649;
      if (XINT (x0, 1) == 10 && XVECLEN (x0, 0) == 1 && 1)
	goto L2913;
      if (XINT (x0, 1) == 11 && XVECLEN (x0, 0) == 1 && 1)
	goto L2915;
      if (XINT (x0, 1) == 12 && XVECLEN (x0, 0) == 1 && 1)
	goto L2917;
      if (XINT (x0, 1) == 13 && XVECLEN (x0, 0) == 1 && 1)
	goto L2919;
      if (XINT (x0, 1) == 14 && XVECLEN (x0, 0) == 1 && 1)
	goto L2921;
      if (XINT (x0, 1) == 15 && XVECLEN (x0, 0) == 1 && 1)
	goto L2923;
      if (XINT (x0, 1) == 16 && XVECLEN (x0, 0) == 1 && 1)
	goto L2925;
      if (XINT (x0, 1) == 17 && XVECLEN (x0, 0) == 1 && 1)
	goto L2927;
      if (XINT (x0, 1) == 18 && XVECLEN (x0, 0) == 1 && 1)
	goto L2929;
      break;
    case RETURN:
      if (mips_can_use_return_insn ())
	return 462;
      break;
    case CALL:
      goto L2729;
    case CONST_INT:
      if (XWINT (x0, 0) == 0 && 1)
	return 494;
    }
  goto ret0;
 L1: ATTRIBUTE_UNUSED_LABEL
  return recog_5 (x0, insn, pnum_clobbers);

  L41: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  switch (GET_CODE (x1))
    {
    default:
      break;
    case SET:
      goto L42;
    case TRAP_IF:
      goto L937;
    case USE:
      goto L2653;
    case CALL:
      goto L2668;
    }
  goto ret0;
 L42: ATTRIBUTE_UNUSED_LABEL
  return recog_7 (x0, insn, pnum_clobbers);

  L937: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == EQ && 1)
    goto L938;
  goto ret0;

  L938: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, VOIDmode))
    {
      ro[0] = x3;
      goto L939;
    }
  goto ret0;

  L939: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (true_reg_or_0_operand (x3, VOIDmode))
    {
      ro[1] = x3;
      goto L940;
    }
  goto ret0;

  L940: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, VOIDmode))
    {
      ro[2] = x2;
      goto L941;
    }
  goto ret0;

  L941: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L942;
  goto ret0;

  L942: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == REG && XINT (x2, 0) == 24 && 1)
    if (TARGET_MIPS16)
      return 104;
  goto ret0;

  L2653: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, VOIDmode))
    {
      ro[0] = x2;
      goto L2654;
    }
  goto ret0;

  L2654: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == RETURN && 1)
    return 463;
  goto ret0;

  L2668: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != MEM)
    goto ret0;
  goto L2669;
  L2691: ATTRIBUTE_UNUSED_LABEL
  if (GET_CODE (x2) != MEM)
    goto ret0;
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      goto L2692;
    case DImode:
      goto L2699;
    }
  goto ret0;

  L2669: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (call_insn_operand (x3, VOIDmode))
    {
      ro[0] = x3;
      goto L2670;
    }
  goto L2691;

  L2670: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  ro[1] = x2;
  goto L2671;

  L2671: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2672;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2691;

  L2672: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    {
      x1 = XVECEXP (x0, 0, 0);
      x2 = XEXP (x1, 0);
      goto L2691;
    }
  if (register_operand (x2, SImode))
    goto L2680;
  L2688: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_ABICALLS && !TARGET_LONG_CALLS)
	return 469;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2691;

  L2680: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31)
    return 467;
  L2681: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_ABICALLS && !TARGET_LONG_CALLS)
    return 468;
  goto L2688;

  L2692: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[0] = x3;
      goto L2693;
    }
  goto ret0;

  L2693: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  ro[1] = x2;
  goto L2694;

  L2694: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2695;
  goto ret0;

  L2695: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L2710;
  L2718: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (!(Pmode == DImode) && TARGET_ABICALLS && TARGET_LONG_CALLS)
	return 473;
      }
  goto ret0;

  L2710: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16
   && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 470;
  L2711: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (TARGET_MIPS16 && !(Pmode == DImode) && !TARGET_ABICALLS && TARGET_LONG_CALLS
   && GET_CODE (operands[2]) == REG && REGNO (operands[2]) == 31)
    return 472;
  goto L2718;

  L2699: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (se_register_operand (x3, DImode))
    {
      ro[0] = x3;
      goto L2700;
    }
  goto ret0;

  L2700: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  ro[1] = x2;
  goto L2701;

  L2701: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2702;
  goto ret0;

  L2702: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L2726;
  goto ret0;

  L2726: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (!TARGET_MIPS16
   && Pmode == DImode && !TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 471;
  L2727: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x2;
  if (Pmode == DImode && TARGET_ABICALLS && TARGET_LONG_CALLS)
    return 474;
  goto ret0;

  L293: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L294;
  goto ret0;
 L294: ATTRIBUTE_UNUSED_LABEL
  return recog_8 (x0, insn, pnum_clobbers);

  L310: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L311;
  goto ret0;

  L311: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  switch (GET_MODE (x2))
    {
    default:
      break;
    case SImode:
      if (register_operand (x2, SImode))
	{
	  ro[0] = x2;
	  goto L312;
	}
    L1416: ATTRIBUTE_UNUSED_LABEL
      if (general_operand (x2, SImode))
	{
	  ro[0] = x2;
	  goto L1417;
	}
      break;
    case DImode:
      if (register_operand (x2, DImode))
	{
	  ro[0] = x2;
	  goto L445;
	}
    }
  if (GET_CODE (x2) == PC && 1)
    goto L2625;
  L2818: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, VOIDmode))
    {
      ro[0] = x2;
      goto L2819;
    }
  goto ret0;
 L312: ATTRIBUTE_UNUSED_LABEL
  tem = recog_9 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x2 = XEXP (x1, 0);
  goto L1416;

  L1417: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == FIX && 1)
    goto L1418;
  x2 = XEXP (x1, 0);
  goto L2818;

  L1418: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  switch (GET_MODE (x3))
    {
    default:
      break;
    case DFmode:
      if (register_operand (x3, DFmode))
	{
	  ro[1] = x3;
	  goto L1419;
	}
      break;
    case SFmode:
      if (register_operand (x3, SFmode))
	{
	  ro[1] = x3;
	  goto L1432;
	}
    }
  x2 = XEXP (x1, 0);
  goto L2818;

  L1419: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1420;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1420: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L1421;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1421: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1422;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1422: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, DFmode))
    {
      ro[3] = x2;
      if (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
	return 207;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1432: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1433;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1433: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L1434;
    }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1434: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1435;
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;

  L1435: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SFmode))
    {
      ro[3] = x2;
      if (TARGET_HARD_FLOAT && !0)
	return 208;
      }
  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 0);
  goto L2818;
 L445: ATTRIBUTE_UNUSED_LABEL
  tem = recog_10 (x0, insn, pnum_clobbers);
  if (tem >= 0) return tem;
  x2 = XEXP (x1, 0);
  goto L2818;

  L2625: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == MEM && 1)
    goto L2626;
  goto ret0;

  L2626: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == PLUS && 1)
    goto L2627;
  goto ret0;

  L2627: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (GET_MODE (x4) == SImode && GET_CODE (x4) == MULT && 1)
    goto L2628;
  goto ret0;

  L2628: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  if (register_operand (x5, SImode))
    {
      ro[0] = x5;
      goto L2629;
    }
  goto ret0;

  L2629: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 1);
  if (GET_CODE (x5) == CONST_INT && XWINT (x5, 0) == 4 && 1)
    goto L2630;
  goto ret0;

  L2630: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (GET_CODE (x4) == LABEL_REF && 1)
    goto L2631;
  goto ret0;

  L2631: ATTRIBUTE_UNUSED_LABEL
  x5 = XEXP (x4, 0);
  ro[1] = x5;
  goto L2632;

  L2632: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2633;
  goto ret0;

  L2633: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      goto L2634;
    }
  goto ret0;

  L2634: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2635;
  goto ret0;

  L2635: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == REG && XINT (x2, 0) == 31 && 1)
    if (TARGET_EMBEDDED_PIC)
      return 453;
  goto ret0;

  L2819: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL && 1)
    goto L2820;
  goto ret0;

  L2820: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM && 1)
    goto L2821;
  goto ret0;

  L2821: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (call_insn_operand (x4, VOIDmode))
    {
      ro[1] = x4;
      goto L2822;
    }
  goto ret0;

  L2822: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  ro[2] = x3;
  goto L2823;

  L2823: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == SET && 1)
    goto L2824;
  goto ret0;

  L2824: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, VOIDmode))
    {
      ro[3] = x2;
      goto L2825;
    }
  goto ret0;

  L2825: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CALL && 1)
    goto L2826;
  goto ret0;

  L2826: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_CODE (x3) == MEM && 1)
    goto L2827;
  goto ret0;

  L2827: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (rtx_equal_p (x4, ro[1]) && 1)
    goto L2828;
  goto ret0;

  L2828: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (rtx_equal_p (x3, ro[2]) && 1)
    goto L2829;
  goto ret0;

  L2829: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2830;
  goto ret0;

  L2830: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[4] = x2;
      if (TARGET_ABICALLS && !TARGET_LONG_CALLS)
	return 492;
      }
  goto ret0;

  L346: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L347;
  goto ret0;

  L347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[0] = x2;
      goto L348;
    }
  goto ret0;

  L348: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case PLUS:
      goto L349;
    case MINUS:
      goto L407;
    }
  goto ret0;

  L349: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L350;
  goto ret0;

  L350: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L351;
    }
  goto ret0;

  L351: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L352;
    }
  goto ret0;

  L352: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[3] = x3;
      goto L353;
    }
  goto ret0;

  L353: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L354;
  goto ret0;

  L354: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L355;
    }
  goto ret0;

  L355: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L356;
  goto ret0;

  L356: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L357;
    }
  goto ret0;

  L357: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L358;
  goto ret0;

  L358: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L359;
    }
  goto ret0;

  L359: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L360;
  goto ret0;

  L360: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if ((TARGET_MIPS3900
    || TARGET_MIPS5400)			/* CYGNUS LOCAL vr5400/raeburn */
   && !TARGET_MIPS16)
	return 58;
      }
  goto ret0;

  L407: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L408;
    }
  goto ret0;

  L408: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L409;
  goto ret0;

  L409: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L410;
    }
  goto ret0;

  L410: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[3] = x4;
      goto L411;
    }
  goto ret0;

  L411: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L412;
  goto ret0;

  L412: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L413;
    }
  goto ret0;

  L413: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L414;
  goto ret0;

  L414: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L415;
    }
  goto ret0;

  L415: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L416;
  goto ret0;

  L416: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L417;
    }
  goto ret0;

  L417: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L418;
  goto ret0;

  L418: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if (TARGET_MIPS5400)
	return 61;
      }
  goto ret0;

  L1691: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L1692;
  goto ret0;

  L1692: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (memory_operand (x2, BLKmode))
    {
      ro[0] = x2;
      goto L1693;
    }
  goto ret0;

  L1693: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (memory_operand (x2, BLKmode))
    {
      ro[1] = x2;
      goto L1694;
    }
  goto ret0;

  L1694: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1695;
  goto ret0;

  L1695: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L1696;
    }
  goto ret0;

  L1696: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1697;
  goto ret0;

  L1697: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L1698;
    }
  goto ret0;

  L1698: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1699;
  goto ret0;

  L1699: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L1700;
    }
  goto ret0;

  L1700: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1701;
  goto ret0;

  L1701: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      goto L1702;
    }
  goto ret0;

  L1702: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == USE && 1)
    goto L1703;
  goto ret0;

  L1703: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[2] = x2;
      goto L1704;
    }
  goto ret0;

  L1704: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 6);
  if (GET_CODE (x1) == USE && 1)
    goto L1705;
  goto ret0;

  L1705: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[3] = x2;
      goto L1706;
    }
  goto ret0;

  L1706: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 7);
  if (GET_CODE (x1) == USE && 1)
    goto L1707;
  goto ret0;

  L1707: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) != CONST_INT)
    goto ret0;
  if (XWINT (x2, 0) == 0 && 1)
    goto L1726;
  if (GET_CODE (x2) != CONST_INT)
    goto ret0;
  if (XWINT (x2, 0) == 1 && 1)
    goto L1822;
  if (XWINT (x2, 0) == 2 && 1)
    goto L1860;
  goto ret0;

  L1726: ATTRIBUTE_UNUSED_LABEL
  return 282;
  L1727: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 283;
  L1746: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 284;
  L1765: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 285;
  goto ret0;

  L1822: ATTRIBUTE_UNUSED_LABEL
  return 287;
  L1823: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 288;
  goto ret0;

  L1860: ATTRIBUTE_UNUSED_LABEL
  return 289;
  L1861: ATTRIBUTE_UNUSED_LABEL
  if (TARGET_MIPS16)
    return 290;
  goto ret0;

  L931: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == EQ && 1)
    goto L932;
  goto ret0;

  L932: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, VOIDmode))
    {
      ro[0] = x2;
      goto L933;
    }
  goto ret0;

  L933: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (true_reg_or_0_operand (x2, VOIDmode))
    {
      ro[1] = x2;
      goto L934;
    }
  goto ret0;

  L934: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (immediate_operand (x1, VOIDmode))
    goto L948;
  goto ret0;

  L948: ATTRIBUTE_UNUSED_LABEL
  ro[2] = x1;
  if (!TARGET_MIPS16)
    return 103;
  L949: ATTRIBUTE_UNUSED_LABEL
  if (pnum_clobbers != 0 && 1)
    {
      ro[2] = x1;
      if (TARGET_MIPS16)
	{
	  *pnum_clobbers = 1;
	  return 104;
	}
      }
  goto ret0;

  L2649: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT && XWINT (x1, 0) == 0 && 1)
    return 459;
  goto ret0;

  L2913: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, QImode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 509;
      }
  goto ret0;

  L2915: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, HImode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 510;
      }
  goto ret0;

  L2917: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, SImode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 511;
      }
  goto ret0;

  L2919: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, DImode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 512;
      }
  goto ret0;

  L2921: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, SFmode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 513;
      }
  goto ret0;

  L2923: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (consttable_operand (x1, DFmode))
    {
      ro[0] = x1;
      if (TARGET_MIPS16)
	return 514;
      }
  goto ret0;

  L2925: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT && XWINT (x1, 0) == 0 && 1)
    if (TARGET_MIPS16)
      return 515;
  goto ret0;

  L2927: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT && XWINT (x1, 0) == 0 && 1)
    if (TARGET_MIPS16)
      return 516;
  goto ret0;

  L2929: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == CONST_INT && XWINT (x1, 0) == 0 && 1)
    if (TARGET_MIPS16)
      return 517;
  goto ret0;

  L2729: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 0);
  if (GET_CODE (x1) == MEM && 1)
    goto L2730;
  goto ret0;

  L2730: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (call_insn_operand (x2, VOIDmode))
    {
      ro[0] = x2;
      goto L2731;
    }
  goto ret0;

  L2731: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  ro[1] = x1;
  if (!TARGET_MIPS16 && !TARGET_ABICALLS && !TARGET_LONG_CALLS)
    return 477;
  goto ret0;
 ret0:
  return -1;
}

rtx
split_1 (x0, insn)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) != DImode)
    goto ret0;
  switch (GET_CODE (x1))
    {
    default:
      break;
    case PLUS:
      goto L115;
    case MINUS:
      goto L249;
    case NOT:
      goto L1166;
    case AND:
      goto L1284;
    case IOR:
      goto L1224;
    case XOR:
      goto L1258;
    case MEM:
      goto L1528;
    case ASHIFT:
      goto L1956;
    case ASHIFTRT:
      goto L2052;
    case LSHIFTRT:
      goto L2168;
    case EQ:
      goto L2276;
    case NE:
      goto L2306;
    case GE:
      goto L2346;
    case LE:
      goto L2414;
    case GEU:
      goto L2454;
    case LEU:
      goto L2522;
    case SUBREG:
    case REG:
      if (register_operand (x1, DImode))
	{
	  ro[1] = x1;
	  if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1])))
	    return gen_split_232 (operands);
	  }
    }
  goto ret0;

  L115: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, ro[0]) && 1)
    goto L116;
  L120: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L121;
    }
  goto ret0;

  L116: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[1] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0xf
	&& INTVAL (operands[1]) <= 0xf + 0xf)
       || (INTVAL (operands[1]) < - 0x10
	   && INTVAL (operands[1]) >= - 0x10 - 0x10)))
	return gen_split_20 (operands);
      }
  x2 = XEXP (x1, 0);
  goto L120;

  L121: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0xf)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x10)))
	return gen_split_21 (operands);
      }
  goto ret0;

  L249: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, ro[0]) && 1)
    goto L250;
  L254: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L255;
    }
  goto ret0;

  L250: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[1] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x10
	&& INTVAL (operands[1]) <= 0x10 + 0x10)
       || (INTVAL (operands[1]) < - 0xf
	   && INTVAL (operands[1]) >= - 0xf - 0xf)))
	return gen_split_44 (operands);
      }
  x2 = XEXP (x1, 0);
  goto L254;

  L255: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x10)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0xf)))
	return gen_split_45 (operands);
      }
  goto ret0;

  L1166: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1])))
	return gen_split_139 (operands);
      }
  goto ret0;

  L1284: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  if (GET_CODE (x2) == NOT && 1)
    goto L1285;
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1193;
    }
  goto ret0;

  L1285: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1286;
    }
  goto ret0;

  L1286: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == NOT && 1)
    goto L1287;
  goto ret0;

  L1287: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      if (reload_completed && !TARGET_MIPS16 && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
	return gen_split_166 (operands);
      }
  goto ret0;

  L1193: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
	return gen_split_146 (operands);
      }
  goto ret0;

  L1224: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1225;
    }
  goto ret0;

  L1225: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
	return gen_split_154 (operands);
      }
  goto ret0;

  L1258: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1259;
    }
  goto ret0;

  L1259: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
	return gen_split_162 (operands);
      }
  goto ret0;

  L1528: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == DImode && GET_CODE (x2) == PLUS && 1)
    goto L1529;
  goto ret0;

  L1529: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L1530;
  goto ret0;

  L1530: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && 1)
    {
      ro[1] = x3;
      if (TARGET_64BIT && TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x10)
       || (INTVAL (operands[1]) >= 32 * 8
	   && INTVAL (operands[1]) <= 31 * 8 + 0x8)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 8
	   && (INTVAL (operands[1]) & 7) != 0)))
	return gen_split_235 (operands);
      }
  goto ret0;

  L1956: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L1957;
    }
  goto ret0;

  L1957: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_305 (operands);
      }
  goto ret0;

  L2052: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2053;
    }
  goto ret0;

  L2053: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && TARGET_64BIT
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_320 (operands);
      }
  goto ret0;

  L2168: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2169;
    }
  goto ret0;

  L2169: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_339 (operands);
      }
  goto ret0;

  L2276: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2277;
    }
  goto ret0;

  L2277: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_uns_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0))
	return gen_split_372 (operands);
      }
  goto ret0;

  L2306: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2307;
    }
  goto ret0;

  L2307: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_uns_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
    && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0))
	return gen_split_379 (operands);
      }
  goto ret0;

  L2346: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2347;
    }
  goto ret0;

  L2347: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16)
	return gen_split_389 (operands);
      }
  goto ret0;

  L2414: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2415;
    }
  goto ret0;

  L2415: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16)
	return gen_split_403 (operands);
      }
  goto ret0;

  L2454: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2455;
    }
  goto ret0;

  L2455: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_arith_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16)
	return gen_split_413 (operands);
      }
  goto ret0;

  L2522: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (se_register_operand (x2, DImode))
    {
      ro[1] = x2;
      goto L2523;
    }
  goto ret0;

  L2523: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (se_register_operand (x2, DImode))
    {
      ro[2] = x2;
      if (TARGET_64BIT && TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE
   && !TARGET_MIPS16)
	return gen_split_427 (operands);
      }
  goto ret0;
 ret0:
  return 0;
}

rtx
split_2 (x0, insn)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  x1 = XEXP (x0, 0);
  switch (GET_MODE (x1))
    {
    default:
      break;
    case SImode:
      if (register_operand (x1, SImode))
	{
	  ro[0] = x1;
	  goto L32;
	}
      break;
    case DImode:
      if (register_operand (x1, DImode))
	{
	  ro[0] = x1;
	  goto L114;
	}
      break;
    case HImode:
      if (register_operand (x1, HImode))
	{
	  ro[0] = x1;
	  goto L1626;
	}
      break;
    case QImode:
      if (register_operand (x1, QImode))
	{
	  ro[0] = x1;
	  goto L1643;
	}
      break;
    case DFmode:
      if (register_operand (x1, DFmode))
	{
	  ro[0] = x1;
	  goto L1676;
	}
    }
  goto ret0;

  L32: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  switch (GET_MODE (x1))
    {
    default:
      break;
    case SImode:
      switch (GET_CODE (x1))
	{
	default:
	  break;
	case PLUS:
	  goto L33;
	case MINUS:
	  goto L167;
	case MEM:
	  goto L1551;
	case ASHIFT:
	  goto L1877;
	case ASHIFTRT:
	  goto L1973;
	case LSHIFTRT:
	  goto L2069;
	case EQ:
	  goto L2266;
	case NE:
	  goto L2296;
	case GE:
	  goto L2336;
	case LE:
	  goto L2404;
	case GEU:
	  goto L2444;
	case LEU:
	  goto L2512;
	}
    }
  if (GET_CODE (x1) != CONST_INT)
    goto ret0;
  if (large_int (x1, SImode))
    {
      ro[1] = x1;
      if (!TARGET_DEBUG_D_MODE && !TARGET_MIPS16)
	return gen_split_238 (operands);
      }
  L1556: ATTRIBUTE_UNUSED_LABEL
  goto L1560;

  L33: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, ro[0]) && 1)
    goto L34;
  L38: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L39;
    }
  goto ret0;

  L34: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[1] = x2;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x7f
	&& INTVAL (operands[1]) <= 0x7f + 0x7f)
       || (INTVAL (operands[1]) < - 0x80
	   && INTVAL (operands[1]) >= - 0x80 - 0x80)))
	return gen_split_7 (operands);
      }
  x2 = XEXP (x1, 0);
  goto L38;

  L39: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x7
	&& INTVAL (operands[2]) <= 0x7 + 0x7f)
       || (INTVAL (operands[2]) < - 0x8
	   && INTVAL (operands[2]) >= - 0x8 - 0x80)))
	return gen_split_8 (operands);
      }
  goto ret0;

  L167: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (rtx_equal_p (x2, ro[0]) && 1)
    goto L168;
  L172: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L173;
    }
  goto ret0;

  L168: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[1] = x2;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) > 0x80
	&& INTVAL (operands[1]) <= 0x80 + 0x80)
       || (INTVAL (operands[1]) < - 0x7f
	   && INTVAL (operands[1]) >= - 0x7f - 0x7f)))
	return gen_split_31 (operands);
      }
  x2 = XEXP (x1, 0);
  goto L172;

  L173: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1]))
   && REGNO (operands[0]) != REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && ((INTVAL (operands[2]) > 0x8
	&& INTVAL (operands[2]) <= 0x8 + 0x80)
       || (INTVAL (operands[2]) < - 0x7
	   && INTVAL (operands[2]) >= - 0x7 - 0x7f)))
	return gen_split_32 (operands);
      }
  goto ret0;

  L1551: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == PLUS && 1)
    goto L1552;
  goto ret0;

  L1552: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L1553;
  goto ret0;

  L1553: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && 1)
    {
      ro[1] = x3;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 4
	   && INTVAL (operands[1]) <= 31 * 4 + 0x7c)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 4
	   && (INTVAL (operands[1]) & 3) != 0)))
	return gen_split_244 (operands);
      }
  goto ret0;

  L1877: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1878;
    }
  goto ret0;

  L1878: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_294 (operands);
      }
  goto ret0;

  L1973: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L1974;
    }
  goto ret0;

  L1974: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_309 (operands);
      }
  goto ret0;

  L2069: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2070;
    }
  L2079: ATTRIBUTE_UNUSED_LABEL
  if (memory_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2080;
    }
  goto ret0;

  L2070: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_CODE (x2) == CONST_INT && 1)
    {
      ro[2] = x2;
      if (TARGET_MIPS16
   && reload_completed
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) > 8
   && INTVAL (operands[2]) <= 16)
	return gen_split_324 (operands);
      }
  x2 = XEXP (x1, 0);
  goto L2079;

  L2080: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (immediate_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_MIPS16)
	return gen_split_326 (operands);
      }
  goto ret0;

  L2266: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2267;
    }
  goto ret0;

  L2267: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0))
	return gen_split_370 (operands);
      }
  goto ret0;

  L2296: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2297;
    }
  goto ret0;

  L2297: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (uns_arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16
    && (GET_CODE (operands[2]) != CONST_INT || INTVAL (operands[2]) != 0))
	return gen_split_377 (operands);
      }
  goto ret0;

  L2336: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2337;
    }
  goto ret0;

  L2337: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16)
	return gen_split_387 (operands);
      }
  goto ret0;

  L2404: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2405;
    }
  goto ret0;

  L2405: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16)
	return gen_split_401 (operands);
      }
  goto ret0;

  L2444: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2445;
    }
  goto ret0;

  L2445: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (arith_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16)
	return gen_split_411 (operands);
      }
  goto ret0;

  L2512: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[1] = x2;
      goto L2513;
    }
  goto ret0;

  L2513: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (register_operand (x2, SImode))
    {
      ro[2] = x2;
      if (TARGET_DEBUG_C_MODE && !TARGET_DEBUG_D_MODE && !TARGET_MIPS16)
	return gen_split_425 (operands);
      }
  goto ret0;

  L1560: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) >= 0x100
   && INTVAL (operands[1]) <= 0xff + 0x7f)
    return gen_split_245 (operands);
  L1561: ATTRIBUTE_UNUSED_LABEL
  ro[1] = x1;
  if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && INTVAL (operands[1]) < 0
   && INTVAL (operands[1]) > - 0x8000)
    return gen_split_246 (operands);
  goto ret0;
 L114: ATTRIBUTE_UNUSED_LABEL
  return split_1 (x0, insn);

  L1626: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == HImode && GET_CODE (x1) == MEM && 1)
    goto L1627;
  goto ret0;

  L1627: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == PLUS && 1)
    goto L1628;
  goto ret0;

  L1628: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L1629;
  goto ret0;

  L1629: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && 1)
    {
      ro[1] = x3;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32 * 2
	   && INTVAL (operands[1]) <= 31 * 2 + 0x7e)
       || (INTVAL (operands[1]) >= 0
	   && INTVAL (operands[1]) < 32 * 2
	   && (INTVAL (operands[1]) & 1) != 0)))
	return gen_split_264 (operands);
      }
  goto ret0;

  L1643: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (GET_MODE (x1) == QImode && GET_CODE (x1) == MEM && 1)
    goto L1644;
  goto ret0;

  L1644: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == SImode && GET_CODE (x2) == PLUS && 1)
    goto L1645;
  goto ret0;

  L1645: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (rtx_equal_p (x3, ro[0]) && 1)
    goto L1646;
  goto ret0;

  L1646: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && 1)
    {
      ro[1] = x3;
      if (TARGET_MIPS16 && reload_completed
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == CONST_INT
   && ((INTVAL (operands[1]) < 0
	&& INTVAL (operands[1]) >= -0x80)
       || (INTVAL (operands[1]) >= 32
	   && INTVAL (operands[1]) <= 31 + 0x7f)))
	return gen_split_269 (operands);
      }
  goto ret0;

  L1676: ATTRIBUTE_UNUSED_LABEL
  x1 = XEXP (x0, 1);
  if (register_operand (x1, DFmode))
    {
      ro[1] = x1;
      if (reload_completed && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1])))
	return gen_split_279 (operands);
      }
  goto ret0;
 ret0:
  return 0;
}

rtx
split_3 (x0, insn)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  x1 = XVECEXP (x0, 0, 0);
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != DImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case PLUS:
      goto L52;
    case MINUS:
      goto L186;
    case ASHIFT:
      goto L1899;
    case ASHIFTRT:
      goto L1995;
    case LSHIFTRT:
      goto L2101;
    }
  goto ret0;

  L52: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L53;
    }
  goto ret0;

  L53: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L54;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      goto L80;
    }
  goto ret0;

  L54: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L55;
  goto ret0;

  L55: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L64;
  goto ret0;

  L64: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2])))
    return gen_split_11 (operands);
  L65: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2]))
   && (REGNO (operands[0]) != REGNO (operands[1])
       || REGNO (operands[0]) != REGNO (operands[2])))
    return gen_split_12 (operands);
  goto ret0;

  L80: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L81;
  goto ret0;

  L81: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L90;
  goto ret0;

  L90: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0)
    return gen_split_14 (operands);
  L91: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0)
    return gen_split_15 (operands);
  goto ret0;

  L186: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L187;
    }
  goto ret0;

  L187: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, DImode))
    {
      ro[2] = x3;
      goto L188;
    }
  if (GET_CODE (x3) == CONST_INT && small_int (x3, DImode))
    {
      ro[2] = x3;
      goto L214;
    }
  goto ret0;

  L188: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L189;
  goto ret0;

  L189: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L198;
  goto ret0;

  L198: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
    return gen_split_35 (operands);
  L199: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && GET_CODE (operands[2]) == REG && GP_REG_P (REGNO (operands[2])))
    return gen_split_36 (operands);
  goto ret0;

  L214: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L215;
  goto ret0;

  L215: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    goto L224;
  goto ret0;

  L224: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0)
    return gen_split_38 (operands);
  L225: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && GP_REG_P (REGNO (operands[0]))
   && GET_CODE (operands[1]) == REG && GP_REG_P (REGNO (operands[1]))
   && INTVAL (operands[2]) > 0)
    return gen_split_39 (operands);
  goto ret0;

  L1899: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1900;
    }
  goto ret0;

  L1900: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L1901;
    }
  goto ret0;

  L1901: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1902;
  goto ret0;

  L1902: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L1911;
  L1930: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    goto L1939;
  goto ret0;

  L1911: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_298 (operands);
  L1912: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_299 (operands);
  goto L1930;

  L1939: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_301 (operands);
  L1940: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_302 (operands);
  goto ret0;

  L1995: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L1996;
    }
  goto ret0;

  L1996: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L1997;
    }
  goto ret0;

  L1997: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1998;
  goto ret0;

  L1998: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L2007;
  L2026: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    goto L2035;
  goto ret0;

  L2007: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_313 (operands);
  L2008: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_314 (operands);
  goto L2026;

  L2035: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_316 (operands);
  L2036: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_317 (operands);
  goto ret0;

  L2101: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, DImode))
    {
      ro[1] = x3;
      goto L2102;
    }
  goto ret0;

  L2102: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_CODE (x3) == CONST_INT && small_int (x3, SImode))
    {
      ro[2] = x3;
      goto L2103;
    }
  goto ret0;

  L2103: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L2104;
  goto ret0;

  L2104: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  if (register_operand (x2, SImode))
    goto L2113;
  L2132: ATTRIBUTE_UNUSED_LABEL
  if (register_operand (x2, SImode))
    goto L2141;
  goto ret0;

  L2113: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_330 (operands);
  L2114: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 32) != 0)
    return gen_split_331 (operands);
  goto L2132;

  L2141: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && !WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_333 (operands);
  L2142: ATTRIBUTE_UNUSED_LABEL
  ro[3] = x2;
  if (reload_completed && WORDS_BIG_ENDIAN && !TARGET_64BIT
   && !TARGET_DEBUG_D_MODE && !TARGET_DEBUG_G_MODE && !TARGET_MIPS16
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < FIRST_PSEUDO_REGISTER
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < FIRST_PSEUDO_REGISTER
   && (INTVAL (operands[2]) & 63) < 32
   && (INTVAL (operands[2]) & 63) != 0)
    return gen_split_334 (operands);
  goto ret0;
 ret0:
  return 0;
}

rtx
split_insns (x0, insn)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  rtx tem ATTRIBUTE_UNUSED;

  L30: ATTRIBUTE_UNUSED_LABEL
  switch (GET_CODE (x0))
    {
    default:
      break;
    case SET:
      goto L31;
    case PARALLEL:
      if (XVECLEN (x0, 0) == 2 && 1)
	goto L49;
      if (XVECLEN (x0, 0) == 5 && 1)
	goto L369;
      if (XVECLEN (x0, 0) == 8 && 1)
	goto L1767;
    }
  goto ret0;
 L31: ATTRIBUTE_UNUSED_LABEL
  return split_2 (x0, insn);

  L49: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L50;
  goto ret0;

  L50: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, DImode))
    {
      ro[0] = x2;
      goto L51;
    }
  goto ret0;
 L51: ATTRIBUTE_UNUSED_LABEL
  return split_3 (x0, insn);

  L369: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L370;
  goto ret0;

  L370: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[0] = x2;
      goto L371;
    }
  goto ret0;

  L371: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) != SImode)
    goto ret0;
  switch (GET_CODE (x2))
    {
    default:
      break;
    case PLUS:
      goto L372;
    case MINUS:
      goto L430;
    }
  goto ret0;

  L372: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L373;
  goto ret0;

  L373: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[1] = x4;
      goto L374;
    }
  goto ret0;

  L374: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L375;
    }
  goto ret0;

  L375: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (register_operand (x3, SImode))
    {
      ro[3] = x3;
      goto L376;
    }
  goto ret0;

  L376: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L377;
  goto ret0;

  L377: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L378;
    }
  goto ret0;

  L378: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L379;
  goto ret0;

  L379: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L380;
    }
  goto ret0;

  L380: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L381;
  goto ret0;

  L381: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L382;
    }
  goto ret0;

  L382: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L383;
  goto ret0;

  L383: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if (reload_completed && GP_REG_P (true_regnum (operands[0])) && GP_REG_P (true_regnum (operands[3])))
	return gen_split_59 (operands);
      }
  goto ret0;

  L430: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L431;
    }
  goto ret0;

  L431: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 1);
  if (GET_MODE (x3) == SImode && GET_CODE (x3) == MULT && 1)
    goto L432;
  goto ret0;

  L432: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 0);
  if (register_operand (x4, SImode))
    {
      ro[2] = x4;
      goto L433;
    }
  goto ret0;

  L433: ATTRIBUTE_UNUSED_LABEL
  x4 = XEXP (x3, 1);
  if (register_operand (x4, SImode))
    {
      ro[3] = x4;
      goto L434;
    }
  goto ret0;

  L434: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L435;
  goto ret0;

  L435: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L436;
    }
  goto ret0;

  L436: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L437;
  goto ret0;

  L437: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L438;
    }
  goto ret0;

  L438: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L439;
  goto ret0;

  L439: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L440;
    }
  goto ret0;

  L440: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L441;
  goto ret0;

  L441: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (scratch_operand (x2, SImode))
    {
      ro[7] = x2;
      if (reload_completed && GP_REG_P (true_regnum (operands[0])) && GP_REG_P (true_regnum (operands[1])))
	return gen_split_62 (operands);
      }
  goto ret0;

  L1767: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 0);
  if (GET_CODE (x1) == SET && 1)
    goto L1768;
  goto ret0;

  L1768: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_MODE (x2) == BLKmode && GET_CODE (x2) == MEM && 1)
    goto L1769;
  goto ret0;

  L1769: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[0] = x3;
      goto L1770;
    }
  goto ret0;

  L1770: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 1);
  if (GET_MODE (x2) == BLKmode && GET_CODE (x2) == MEM && 1)
    goto L1771;
  goto ret0;

  L1771: ATTRIBUTE_UNUSED_LABEL
  x3 = XEXP (x2, 0);
  if (register_operand (x3, SImode))
    {
      ro[1] = x3;
      goto L1772;
    }
  goto ret0;

  L1772: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 1);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1773;
  goto ret0;

  L1773: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[4] = x2;
      goto L1774;
    }
  goto ret0;

  L1774: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 2);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1775;
  goto ret0;

  L1775: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[5] = x2;
      goto L1776;
    }
  goto ret0;

  L1776: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 3);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1777;
  goto ret0;

  L1777: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[6] = x2;
      goto L1778;
    }
  goto ret0;

  L1778: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 4);
  if (GET_CODE (x1) == CLOBBER && 1)
    goto L1779;
  goto ret0;

  L1779: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (register_operand (x2, SImode))
    {
      ro[7] = x2;
      goto L1780;
    }
  goto ret0;

  L1780: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 5);
  if (GET_CODE (x1) == USE && 1)
    goto L1781;
  goto ret0;

  L1781: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[2] = x2;
      goto L1782;
    }
  goto ret0;

  L1782: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 6);
  if (GET_CODE (x1) == USE && 1)
    goto L1783;
  goto ret0;

  L1783: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT && small_int (x2, SImode))
    {
      ro[3] = x2;
      goto L1784;
    }
  goto ret0;

  L1784: ATTRIBUTE_UNUSED_LABEL
  x1 = XVECEXP (x0, 0, 7);
  if (GET_CODE (x1) == USE && 1)
    goto L1785;
  goto ret0;

  L1785: ATTRIBUTE_UNUSED_LABEL
  x2 = XEXP (x1, 0);
  if (GET_CODE (x2) == CONST_INT && XWINT (x2, 0) == 0 && 1)
    if (reload_completed && !TARGET_DEBUG_D_MODE && INTVAL (operands[2]) > 0)
      return gen_split_286 (operands);
  goto ret0;
 ret0:
  return 0;
}

rtx
peephole2_insns (x0, insn, _plast_insn)
     register rtx x0;
     rtx insn ATTRIBUTE_UNUSED;
     rtx *_plast_insn ATTRIBUTE_UNUSED;
{
  register rtx *ro = &recog_operand[0];
  register rtx x1 ATTRIBUTE_UNUSED, x2 ATTRIBUTE_UNUSED, x3 ATTRIBUTE_UNUSED, x4 ATTRIBUTE_UNUSED, x5 ATTRIBUTE_UNUSED, x6 ATTRIBUTE_UNUSED, x7 ATTRIBUTE_UNUSED;
  register rtx _last_insn = insn;
  rtx tem ATTRIBUTE_UNUSED;

  goto ret0;
 ret1:
  *_plast_insn = _last_insn;
  return tem;
 ret0:
  return 0;
}

