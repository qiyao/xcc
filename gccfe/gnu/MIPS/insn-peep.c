/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `genpeep'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "insn-config.h"
#include "rtl.h"
#include "regs.h"
#include "output.h"
#include "real.h"
#include "recog.h"
#include "except.h"

#include "function.h"

#ifdef HAVE_peephole
extern rtx peep_operand[];

#define operands peep_operand

rtx
peephole (ins1)
     rtx ins1;
{
  rtx insn ATTRIBUTE_UNUSED, x ATTRIBUTE_UNUSED, pat ATTRIBUTE_UNUSED;

  if (NEXT_INSN (ins1)
      && GET_CODE (NEXT_INSN (ins1)) == BARRIER)
    return 0;

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L518;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L518;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L518;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L518; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L518;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L518;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L518;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L518;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L518;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L518;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L518;
  if (XWINT (x, 0) != 0) goto L518;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L518;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L518;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L518;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 518;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L518:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L519;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L519;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L519;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L519; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L519;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L519;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L519;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L519;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L519;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L519;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L519;
  if (XWINT (x, 0) != 0) goto L519;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L519;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L519;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[0]) == REG
   && REGNO (operands[0]) == 24
   && dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == REG
   && M16_REG_P (REGNO (operands[1])))) goto L519;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 519;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L519:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L520;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, SImode)) goto L520;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, SImode)) goto L520;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L520; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L520;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L520;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L520;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L520;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, SImode)) goto L520;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L520;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L520;
  if (XWINT (x, 0) != 0) goto L520;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L520;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L520;
  if (! (TARGET_MIPS16
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L520;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 520;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L520:

  insn = ins1;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L521;
  x = XEXP (pat, 0);
  operands[0] = x;
  if (! register_operand (x, DImode)) goto L521;
  x = XEXP (pat, 1);
  operands[1] = x;
  if (! register_operand (x, DImode)) goto L521;
  do { insn = NEXT_INSN (insn);
       if (insn == 0) goto L521; }
  while (GET_CODE (insn) == NOTE
	 || (GET_CODE (insn) == INSN
	     && (GET_CODE (PATTERN (insn)) == USE
		 || GET_CODE (PATTERN (insn)) == CLOBBER)));
  if (GET_CODE (insn) == CODE_LABEL
      || GET_CODE (insn) == BARRIER)
    goto L521;
  pat = PATTERN (insn);
  x = pat;
  if (GET_CODE (x) != SET) goto L521;
  x = XEXP (pat, 0);
  if (GET_CODE (x) != PC) goto L521;
  x = XEXP (pat, 1);
  if (GET_CODE (x) != IF_THEN_ELSE) goto L521;
  x = XEXP (XEXP (pat, 1), 0);
  operands[2] = x;
  if (! equality_op (x, DImode)) goto L521;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 0);
  if (!rtx_equal_p (operands[0], x)) goto L521;
  x = XEXP (XEXP (XEXP (pat, 1), 0), 1);
  if (GET_CODE (x) != CONST_INT) goto L521;
  if (XWINT (x, 0) != 0) goto L521;
  x = XEXP (XEXP (pat, 1), 1);
  operands[3] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L521;
  x = XEXP (XEXP (pat, 1), 2);
  operands[4] = x;
  if (! pc_or_label_operand (x, VOIDmode)) goto L521;
  if (! (TARGET_MIPS16 && TARGET_64BIT
   && GET_CODE (operands[1]) == REG
   && REGNO (operands[1]) == 24
   && GET_CODE (operands[0]) == REG
   && M16_REG_P (REGNO (operands[0]))
   && dead_or_set_p (insn, operands[0]))) goto L521;
  PATTERN (ins1) = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (5, operands));
  INSN_CODE (ins1) = 521;
  delete_for_peephole (NEXT_INSN (ins1), insn);
  return NEXT_INSN (insn);
 L521:

  return 0;
}

rtx peep_operand[5];
#endif
