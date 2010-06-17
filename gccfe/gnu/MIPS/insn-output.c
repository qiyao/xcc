/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `genoutput'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "flags.h"
#include "rtl.h"
#include "function.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"

#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"

#include "insn-codes.h"

#include "recog.h"

#include "output.h"

static const char *
output_6 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  return "addu\t%0,%1,%2";
}
}

static const char *
output_10 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (REGNO (operands[0]) == REGNO (operands[1])
	  && REGNO (operands[0]) == REGNO (operands[2]))
    ? "srl\t%3,%L0,31\n\tsll\t%M0,%M0,1\n\tsll\t%L0,%L1,1\n\taddu\t%M0,%M0,%3"
    : "addu\t%L0,%L1,%L2\n\tsltu\t%3,%L0,%L2\n\taddu\t%M0,%M1,%M2\n\taddu\t%M0,%M0,%3";
}
}

static const char *
output_13 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_13[] = {
    "addu\t%L0,%L1,%2\n\tsltu\t%3,%L0,%2\n\taddu\t%M0,%M1,%3",
    "move\t%L0,%L1\n\tmove\t%M0,%M1",
    "subu\t%L0,%L1,%n2\n\tsltu\t%3,%L0,%2\n\tsubu\t%M0,%M1,1\n\taddu\t%M0,%M0,%3",
  };
  return strings_13[which_alternative];
}

static const char *
output_16 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "dsubu\t%0,%z1,%n2"
    : "daddu\t%0,%z1,%2";
}
}

static const char *
output_19 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "daddu\t%0,%2";
  return "daddu\t%0,%1,%2";
}
}

static const char *
output_22 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "subu\t%0,%z1,%n2"
    : "addu\t%0,%z1,%2";
}
}

static const char *
output_23 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "addu\t%0,%2";
  return "addu\t%0,%1,%2";
}
}

static const char *
output_30 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "subu\t%0,%2";
  return "subu\t%0,%1,%2";
}
}

static const char *
output_37 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_37[] = {
    "sltu\t%3,%L1,%2\n\tsubu\t%L0,%L1,%2\n\tsubu\t%M0,%M1,%3",
    "move\t%L0,%L1\n\tmove\t%M0,%M1",
    "sltu\t%3,%L1,%2\n\tsubu\t%L0,%L1,%2\n\tsubu\t%M0,%M1,1\n\tsubu\t%M0,%M0,%3",
  };
  return strings_37[which_alternative];
}

static const char *
output_40 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "daddu\t%0,%z1,%n2"
    : "dsubu\t%0,%z1,%2";
}
}

static const char *
output_43 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "dsubu\t%0,%2";
  return "dsubu\t%0,%1,%2";
}
}

static const char *
output_46 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? "addu\t%0,%z1,%n2"
    : "subu\t%0,%z1,%2";
}
}

static const char *
output_47 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "subu\t%0,%2";
  return "subu\t%0,%1,%2";
}
}

static const char *
output_50 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  output_asm_insn ("mul.d\t%0,%1,%2", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn ("nop", operands);
  return "";
}
}

static const char *
output_53 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  output_asm_insn ("mul.s\t%0,%1,%2", operands);
  if (TARGET_4300_MUL_FIX)
    output_asm_insn ("nop", operands);
  return "";
}
}

static const char *
output_55 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 1)
    return "mult\t%1,%2";
  if (TARGET_MIPS5400			/* CYGNUS LOCAL vr5400/raeburn */
      || TARGET_MAD)
    return "mul\t%0,%1,%2";
  return "mult\t%0,%1,%2";
}
}

static const char *
output_57 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  xoperands[0] = operands[0];
  xoperands[1] = gen_rtx (REG, SImode, LO_REGNUM);

  output_asm_insn ("mult\t%1,%2", operands);
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *
output_58 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  static const char *const madd[] = { "madd\t%1,%2", "madd\t%0,%1,%2" };
  static const char *const macc[] = { "macc\t$0,%1,%2", "macc\t%0,%1,%2" }; /* CYGNUS LOCAL vr5400/raeburn */
  if (which_alternative == 2)
    return "#";
  /* CYGNUS LOCAL vr5400/raeburn */
  if (TARGET_MIPS5400)
    return macc[which_alternative];
  /* END CYGNUS LOCAL */
  return madd[which_alternative];
}
}

static const char *
output_60 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_60[] = {
    "muls\t$0,%1,%2",
    "muls\t%0,%1,%2",
  };
  return strings_60[which_alternative];
}

static const char *
output_61 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_61[] = {
    "msac\t$0,%2,%3",
    "msac\t%0,%2,%3",
    "#",
  };
  return strings_61[which_alternative];
}

static const char *
output_65 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GENERATE_MULT3)
    output_asm_insn ("dmult\t%0,%1,%2", operands);
  else 
    {
    rtx xoperands[10];

    xoperands[0] = operands[0];
    xoperands[1] = gen_rtx (REG, DImode, LO_REGNUM);

    output_asm_insn ("dmult\t%1,%2", operands);
    output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
    }
  return "";
}
}

static const char *
output_68 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  return "multu\t%1,%2";
}
}

static const char *
output_69 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  return "multu\t%1,%2";
}
}

static const char *
output_70 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "muls\t$0,%1,%2";
  else
    return "mulsu\t$0,%1,%2";
}
}

static const char *
output_71 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[4]) == SIGN_EXTEND)
    return "msac\t$0,%1,%2";
  else
    return "msacu\t$0,%1,%2";
}
}

static const char *
output_74 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mult\t%1,%2";
  else
    return "multu\t%1,%2";
}
}

static const char *
output_75 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  char *const sign[] = { "mult\t%1,%2",  "mulhi\t%0,%1,%2"  };
  char *const zero[] = { "multu\t%1,%2", "mulhiu\t%0,%1,%2" };
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return sign[which_alternative];
  else
    return zero[which_alternative];
}
}

static const char *
output_76 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  char *const sign[] = { "mulshi\t$0,%1,%2",  "mulshi\t%0,%1,%2"  };
  char *const zero[] = { "mulshiu\t$0,%1,%2", "mulshiu\t%0,%1,%2" };
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return sign[which_alternative];
  else
    return zero[which_alternative];
}
}

static const char *
output_80 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[3]) == SIGN_EXTEND)
    return "mad\t%1,%2";
  else
    return "madu\t%1,%2";
}
}

static const char *
output_81 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MAD)
    {
      if (GET_CODE (operands[3]) == SIGN_EXTEND)
	return "mad\t%1,%2";
      else
	return "madu\t%1,%2";
    }
  /* CYGNUS LOCAL vr5400/raeburn */
  else if (TARGET_MIPS5400)
    {
      if (GET_CODE (operands[3]) == SIGN_EXTEND)
	return "macc\t$0,%1,%2";
      else
	return "maccu\t$0,%1,%2";
    }
  /* END CYGNUS LOCAL */
  else
    abort ();
}
}

static const char *
output_103 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& REGNO (operands[1]) == 0)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      if (GENERATE_BRANCHLIKELY)
	{
          if (GET_CODE (operands[1]) == CONST_INT)
	    return "%(beql\t%0,$0,1f\n\tbreak\t%2\n%~1:%)";
	  else
	    return "%(beql\t%0,%1,1f\n\tbreak\t%2\n%~1:%)";
	}
      else
	{
          if (GET_CODE (operands[1]) == CONST_INT)
	    return "%(bne\t%0,$0,1f\n\tnop\n\tbreak\t%2\n%~1:%)";
	  else
	    return "%(bne\t%0,%1,1f\n\tnop\n\tbreak\t%2\n%~1:%)";
	}
    }
  return "";
}
}

static const char *
output_104 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx link;
  int have_dep_anti = 0;

  /* For divmod if one division is not needed then we don't need an extra
     divide by zero trap, which is anti dependent on previous trap */
  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))

    if ((int) REG_DEP_ANTI == (int) REG_NOTE_KIND (link)
        && GET_CODE (XEXP (link, 0)) == INSN
        && GET_CODE (PATTERN (XEXP (link, 0))) == TRAP_IF
	&& REGNO (operands[1]) == 0)
      have_dep_anti = 1;
  if (! have_dep_anti)
    {
      /* No branch delay slots on mips16. */ 
      if (GET_CODE (operands[1]) == CONST_INT)
        return "%(bnez\t%0,1f\n\tbreak\t%2\n%~1:%)";
      else
        return "%(bne\t%0,%1,1f\n\tbreak\t%2\n%~1:%)";
    }
  return "";
}
}

static const char *
output_125 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    {
      if (GENERATE_BRANCHLIKELY)
	return "%(bltzl\t%1,1f\n\tsubu\t%0,%z2,%0\n%~1:%)";
      else
	return "bgez\t%1,1f%#\n\tsubu\t%0,%z2,%0\n%~1:";
    }	  
  else
    return "%(bgez\t%1,1f\n\tmove\t%0,%1\n\tsubu\t%0,%z2,%0\n%~1:%)";
}
}

static const char *
output_126 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total++;
  dslots_jump_filled++;
  operands[2] = const0_rtx;

  if (REGNO (operands[0]) == REGNO (operands[1]))
    return "%(bltzl\t%1,1f\n\tdsubu\t%0,%z2,%0\n%~1:%)";
  else
    return "%(bgez\t%1,1f\n\tmove\t%0,%1\n\tdsubu\t%0,%z2,%0\n%~1:%)";
}
}

static const char *
output_129 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%z4\n\
\tbeq\t%1,%z4,2f\n\
%~1:\tand\t%2,%1,0x0001\n\
\taddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tsrl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%z4\n\
\tmove\t%3,%1\n\
\tbeq\t%3,%z4,2f\n\
%~1:\tand\t%2,%3,0x0001\n\
\taddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tsrl\t%3,%3,1\n\
%~2:%)";
}
}

static const char *
output_130 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_jump_total += 2;
  dslots_jump_filled += 2;
  operands[4] = const0_rtx;

  if (optimize && find_reg_note (insn, REG_DEAD, operands[1]))
    return "%(\
move\t%0,%z4\n\
\tbeq\t%1,%z4,2f\n\
%~1:\tand\t%2,%1,0x0001\n\
\tdaddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tdsrl\t%1,%1,1\n\
%~2:%)";

  return "%(\
move\t%0,%z4\n\
\tmove\t%3,%1\n\
\tbeq\t%3,%z4,2f\n\
%~1:\tand\t%2,%3,0x0001\n\
\tdaddu\t%0,%0,1\n\
\tbeq\t%2,%z4,1b\n\
\tdsrl\t%3,%3,1\n\
%~2:%)";
}
}

static const char *
output_131 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "neg\t%0,%1";
  operands[2] = const0_rtx;
  return "subu\t%0,%z2,%1";
}
}

static const char *
output_133 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[3] = const0_rtx;
  return "subu\t%L0,%z3,%L1\n\tsubu\t%M0,%z3,%M1\n\tsltu\t%2,%z3,%L0\n\tsubu\t%M0,%M0,%2";
}
}

static const char *
output_134 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = const0_rtx;
  return "dsubu\t%0,%z2,%1";
}
}

static const char *
output_137 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "not\t%0,%1";
  operands[2] = const0_rtx;
  return "nor\t%0,%z2,%1";
}
}

static const char *
output_138 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    {
      if (TARGET_64BIT)
	return "not\t%0,%1";
      return "not\t%M0,%M1\n\tnot\t%L0,%L1";
    }
  operands[2] = const0_rtx;
  if (TARGET_64BIT)
    return "nor\t%0,%z2,%1";
  return "nor\t%M0,%z2,%M1\n\tnor\t%L0,%z2,%L1";
}
}

static const char *
output_141 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_141[] = {
    "and\t%0,%1,%2",
    "andi\t%0,%1,%x2",
  };
  return strings_141[which_alternative];
}

static const char *
output_144 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "and\t%0,%1,%2";
  return "and\t%M0,%M1,%M2\n\tand\t%L0,%L1,%L2";
}
}

static const char *
output_145 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "and\t%0,%2";
  return "and\t%M0,%M2\n\tand\t%L0,%L2";
}
}

static const char *
output_147 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_147[] = {
    "and\t%0,%1,%2",
    "andi\t%0,%1,%x2",
  };
  return strings_147[which_alternative];
}

static const char *
output_149 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_149[] = {
    "or\t%0,%1,%2",
    "ori\t%0,%1,%x2",
  };
  return strings_149[which_alternative];
}

static const char *
output_152 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "or\t%0,%1,%2";
  return "or\t%M0,%M1,%M2\n\tor\t%L0,%L1,%L2";
}
}

static const char *
output_153 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "or\t%0,%2";
  return "or\t%M0,%M2\n\tor\t%L0,%L2";
}
}

static const char *
output_156 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_156[] = {
    "xor\t%0,%1,%2",
    "xori\t%0,%1,%x2",
  };
  return strings_156[which_alternative];
}

static const char *
output_157 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_157[] = {
    "xor\t%0,%2",
    "cmpi\t%1,%2",
    "cmp\t%1,%2",
  };
  return strings_157[which_alternative];
}

static const char *
output_159 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "xor\t%0,%1,%2";
  return "xor\t%M0,%M1,%M2\n\txor\t%L0,%L1,%L2";
}
}

static const char *
output_161 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_161[] = {
    "xor\t%0,%2",
    "cmpi\t%1,%2",
    "cmp\t%1,%2",
  };
  return strings_161[which_alternative];
}

static const char *
output_165 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_64BIT)
    return "nor\t%0,%z1,%z2";
  return "nor\t%M0,%M1,%M2\n\tnor\t%L0,%L1,%L2";
}
}

static const char *
output_168 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,32\n\tdsra\t%0,32";
  return "dsll\t%0,%1,32\n\tdsra\t%0,%0,32";
}
}

static const char *
output_169 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,48\n\tdsra\t%0,48";
  return "andi\t%0,%1,0xffff";
}
}

static const char *
output_170 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (TARGET_MIPS16)
    return "dsll\t%0,%1,56\n\tdsra\t%0,56";
  return "andi\t%0,%1,0x00ff"; 
}
}

static const char *
output_171 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return "dsra\t%0,%1,%2";
    }
}
}

static const char *
output_172 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 - shift_amt);
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else if (shift_amt == 32)
    return "dsra\t%0,%1,32";
  else
    {
      operands[2] = GEN_INT (shift_amt);
      return "dsrl\t%0,%1,%2";
    }
}
}

static const char *
output_173 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int shift_amt = INTVAL (operands[2]) & 0x3f;

  if (shift_amt < 32)
    {
      operands[2] = GEN_INT (32 + shift_amt);
      if (TARGET_MIPS16)
	return "dsll\t%0,%1,%2\n\tdsra\t%0,32";
      return "dsll\t%0,%1,%2\n\tdsra\t%0,%0,32";
    }
  else
    return "move\t%0,%.";
}
}

static const char *
output_178 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_180 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0xffff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_181 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_183 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0xffff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_184 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_186 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_187 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_189 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_190 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_192 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "andi\t%0,%1,0x00ff";
  else
    return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_193 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_move_1word (operands, insn, TRUE);
}
}

static const char *
output_194 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_195 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_197 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_199 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_201 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_203 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_205 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_207 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  if (which_alternative == 1)
    return "trunc.w.d %0,%1,%2";

  output_asm_insn ("trunc.w.d %3,%1,%2", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[3];
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *
output_208 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  if (which_alternative == 1)
    return "trunc.w.s %0,%1,%2";

  output_asm_insn ("trunc.w.s %3,%1,%2", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[3];
  output_asm_insn (mips_move_1word (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *
output_209 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  if (which_alternative == 1)
    return "trunc.l.d %0,%1";

  output_asm_insn ("trunc.l.d %2,%1", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[2];
  output_asm_insn (mips_move_2words (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *
output_210 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx xoperands[10];

  if (which_alternative == 1)
    return "trunc.l.s %0,%1";

  output_asm_insn ("trunc.l.s %2,%1", operands);

  xoperands[0] = operands[0];
  xoperands[1] = operands[2];
  output_asm_insn (mips_move_2words (xoperands, insn, FALSE), xoperands);
  return "";
}
}

static const char *
output_211 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return "l.s\t%0,%1%#\n\tcvt.d.w\t%0,%0";

  return "mtc1\t%1,%0%#\n\tcvt.d.w\t%0,%0";
}
}

static const char *
output_212 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return "l.d\t%0,%1%#\n\tcvt.d.l\t%0,%0";

  return "dmtc1\t%1,%0%#\n\tcvt.d.l\t%0,%0";
}
}

static const char *
output_213 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return "l.s\t%0,%1%#\n\tcvt.s.w\t%0,%0";

  return "mtc1\t%1,%0%#\n\tcvt.s.w\t%0,%0";
}
}

static const char *
output_214 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  dslots_load_total++;
  if (GET_CODE (operands[1]) == MEM)
    return "l.d\t%0,%1%#\n\tcvt.s.l\t%0,%0";

  return "dmtc1\t%1,%0%#\n\tcvt.s.l\t%0,%0";
}
}

static const char *
output_222 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = "lw\t%0,%1";
  else
    ret = "ulw\t%0,%1";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}
}

static const char *
output_223 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 3) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return "sw\t%1,%0";

  return "usw\t%z1,%0";
}
}

static const char *
output_224 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[1], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);
  const char *ret;

  if (TARGET_STATS)
    mips_count_memory_refs (operands[1], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster lw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    ret = "ld\t%0,%1";
  else
    ret = "uld\t%0,%1";

  return mips_fill_delay_slot (ret, DELAY_LOAD, operands, insn);
}
}

static const char *
output_225 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  rtx offset = const0_rtx;
  rtx addr = XEXP (operands[0], 0);
  rtx mem_addr = eliminate_constant_term (addr, &offset);

  if (TARGET_STATS)
    mips_count_memory_refs (operands[0], 2);

  /* The stack/frame pointers are always aligned, so we can convert
     to the faster sw if we are referencing an aligned stack location.  */

  if ((INTVAL (offset) & 7) == 0
      && (mem_addr == stack_pointer_rtx || mem_addr == frame_pointer_rtx))
    return "sd\t%1,%0";

  return "usd\t%z1,%0";
}
}

static const char *
output_229 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[1] = gen_rtx (REG, DImode, 31);
  return mips_move_2words (operands, insn);
}
}

static const char *
output_230 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *
output_231 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *
output_233 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *
output_234 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *
output_240 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[1] = gen_rtx (REG, SImode, 31);
  return mips_move_1word (operands, insn, FALSE);
}
}

static const char *
output_241 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_242 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_243 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_249 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_261 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_262 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_263 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_266 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_267 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_268 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, TRUE);
}

static const char *
output_271 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_272 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_273 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_1word (operands, insn, FALSE);
}

static const char *
output_275 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *
output_276 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *
output_277 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn); 
}

static const char *
output_278 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return mips_move_2words (operands, insn);
}

static const char *
output_282 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *
output_283 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *
output_284 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *
output_285 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NORMAL);
}

static const char *
output_287 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);
}

static const char *
output_288 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_NOT_LAST);
}

static const char *
output_289 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_LAST);
}

static const char *
output_290 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 return output_block_move (insn, operands, 4, BLOCK_MOVE_LAST);
}

static const char *
output_292 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
}

static const char *
output_293 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "sll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sll\t%0,%1,%2";
}
}

static const char *
output_296 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsll\t%M0,%L1,%2\n\
\t%(b\t3f\n\
\tmove\t%L0,%z4%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsll\t%M0,%M1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsrl\t%3,%L1,%3\n\
\tor\t%M0,%M0,%3\n\
%~2:\n\
\tsll\t%L0,%L1,%2\n\
%~3:";
}
}

static const char *
output_297 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return "sll\t%M0,%L1,%2\n\tmove\t%L0,%z4";
}
}

static const char *
output_300 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT ((amount & 31));
  operands[4] = const0_rtx;
  operands[5] = GEN_INT (((-amount) & 31));

  return "sll\t%M0,%M1,%2\n\tsrl\t%3,%L1,%5\n\tor\t%M0,%M0,%3\n\tsll\t%L0,%L1,%2";
}
}

static const char *
output_303 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
}

static const char *
output_304 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "dsll\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsll\t%0,%1,%2";
}
}

static const char *
output_307 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
}

static const char *
output_308 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "sra\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "sra\t%0,%1,%2";
}
}

static const char *
output_311 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsra\t%L0,%M1,%2\n\
\t%(b\t3f\n\
\tsra\t%M0,%M1,31%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsrl\t%L0,%L1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsll\t%3,%M1,%3\n\
\tor\t%L0,%L0,%3\n\
%~2:\n\
\tsra\t%M0,%M1,%2\n\
%~3:";
}
}

static const char *
output_312 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  return "sra\t%L0,%M1,%2\n\tsra\t%M0,%M1,31";
}
}

static const char *
output_315 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT ((amount & 31));
  operands[4] = GEN_INT (((-amount) & 31));

  return "srl\t%L0,%L1,%2\n\tsll\t%3,%M1,%4\n\tor\t%L0,%L0,%3\n\tsra\t%M0,%M1,%2";
}
}

static const char *
output_318 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%1,%2";
}
}

static const char *
output_319 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsra\t%0,%2";
}
}

static const char *
output_322 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
}

static const char *
output_323 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (which_alternative == 0)
    return "srl\t%0,%2";

  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "srl\t%0,%1,%2";
}
}

static const char *
output_328 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
 
{
  operands[4] = const0_rtx;
  dslots_jump_total += 3;
  dslots_jump_filled += 2;

  return "sll\t%3,%2,26\n\
\tbgez\t%3,1f\n\
\tsrl\t%L0,%M1,%2\n\
\t%(b\t3f\n\
\tmove\t%M0,%z4%)\n\
\n\
%~1:\n\
\t%(beq\t%3,%z4,2f\n\
\tsrl\t%L0,%L1,%2%)\n\
\n\
\tsubu\t%3,%z4,%2\n\
\tsll\t%3,%M1,%3\n\
\tor\t%L0,%L0,%3\n\
%~2:\n\
\tsrl\t%M0,%M1,%2\n\
%~3:";
}
}

static const char *
output_329 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);
  operands[4] = const0_rtx;
  return "srl\t%L0,%M1,%2\n\tmove\t%M0,%z4";
}
}

static const char *
output_332 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  int amount = INTVAL (operands[2]);

  operands[2] = GEN_INT ((amount & 31));
  operands[4] = GEN_INT (((-amount) & 31));

  return "srl\t%L0,%L1,%2\n\tsll\t%3,%M1,%4\n\tor\t%L0,%L0,%3\n\tsrl\t%M0,%M1,%2";
}
}

static const char *
output_335 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%1,%2";
}
}

static const char *
output_336 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "ror\t%0,%1,%2";
}
}

static const char *
output_337 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return "dror\t%0,%1,%2";
}
}

static const char *
output_338 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return "dsrl\t%0,%2";
}
}

static const char *
output_346 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[1] != pc_rtx) ? "%*bc1t%?\t%Z0%1" : "%*bc1f%?\t%Z0%2";
}
}

static const char *
output_347 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[1] != pc_rtx) ? "%*bc1f%?\t%Z0%1" : "%*bc1t%?\t%Z0%2";
}
}

static const char *
output_348 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  if (operands[2] != pc_rtx)
    {				/* normal jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return "%*beq%?\t%z1,%.,%2";
	case NE:  return "%*bne%?\t%z1,%.,%2";
	case GTU: return "%*bne%?\t%z1,%.,%2";
	case LEU: return "%*beq%?\t%z1,%.,%2";
	case GEU: return "%*j\t%2";
	case LTU: return "%*bne%?\t%.,%.,%2";
	default:
	  break;
	}

      return "%*b%C0z%?\t%z1,%2";
    }
  else
    {				/* inverted jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return "%*bne%?\t%z1,%.,%3";
	case NE:  return "%*beq%?\t%z1,%.,%3";
	case GTU: return "%*beq%?\t%z1,%.,%3";
	case LEU: return "%*bne%?\t%z1,%.,%3";
	case GEU: return "%*beq%?\t%.,%.,%3";
	case LTU: return "%*j\t%3";
	default:
	  break;
	}

      return "%*b%N0z%?\t%z1,%3";
    }
}
}

static const char *
output_349 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "%*b%C0z\t%1,%2";
      else
	return "%*bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "%*b%N0z\t%1,%3";
      else
	return "%*bt%N0z\t%3";
    }
}
}

static const char *
output_350 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  if (operands[2] != pc_rtx)
    {				/* normal jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return "%*beq%?\t%z1,%.,%2";
	case NE:  return "%*bne%?\t%z1,%.,%2";
	case GTU: return "%*bne%?\t%z1,%.,%2";
	case LEU: return "%*beq%?\t%z1,%.,%2";
	case GEU: return "%*j\t%2";
	case LTU: return "%*bne%?\t%.,%.,%2";
	default:
	  break;
	}

      return "%*b%C0z%?\t%z1,%2";
    }
  else
    {				/* inverted jump */
      switch (GET_CODE (operands[0]))
	{
	case EQ:  return "%*bne%?\t%z1,%.,%3";
	case NE:  return "%*beq%?\t%z1,%.,%3";
	case GTU: return "%*beq%?\t%z1,%.,%3";
	case LEU: return "%*bne%?\t%z1,%.,%3";
	case GEU: return "%*beq%?\t%.,%.,%3";
	case LTU: return "%*j\t%3";
	default:
	  break;
	}

      return "%*b%N0z%?\t%z1,%3";
    }
}
}

static const char *
output_351 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[2] != pc_rtx)
    {
      if (which_alternative == 0)
	return "%*b%C0z\t%1,%2";
      else
	return "%*bt%C0z\t%2";
    }
  else
    {
      if (which_alternative == 0)
	return "%*b%N0z\t%1,%3";
      else
	return "%*bt%N0z\t%3";
    }
}
}

static const char *
output_352 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[3] != pc_rtx)
	? "%*b%C0%?\t%z1,%z2,%3"
	: "%*b%N0%?\t%z1,%z2,%4";
}
}

static const char *
output_353 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));
  return (operands[3] != pc_rtx)
	? "%*b%C0%?\t%z1,%z2,%3"
	: "%*b%N0%?\t%z1,%z2,%4";
}
}

static const char *
output_369 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_369[] = {
    "xor\t%0,%1,%2\n\tsltu\t%0,%0,1",
    "xori\t%0,%1,%2\n\tsltu\t%0,%0,1",
  };
  return strings_369[which_alternative];
}

static const char *
output_371 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_371[] = {
    "xor\t%0,%1,%2\n\tsltu\t%0,%0,1",
    "xori\t%0,%1,%2\n\tsltu\t%0,%0,1",
  };
  return strings_371[which_alternative];
}

static const char *
output_376 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_376[] = {
    "xor\t%0,%1,%2\n\tsltu\t%0,%.,%0",
    "xori\t%0,%1,%x2\n\tsltu\t%0,%.,%0",
  };
  return strings_376[which_alternative];
}

static const char *
output_378 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_378[] = {
    "xor\t%0,%1,%2\n\tsltu\t%0,%.,%0",
    "xori\t%0,%1,%x2\n\tsltu\t%0,%.,%0",
  };
  return strings_378[which_alternative];
}

static const char *
output_396 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
}

static const char *
output_397 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
}

static const char *
output_398 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%0,%1,%2";
}
}

static const char *
output_399 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "slt\t%1,%2";
}
}

static const char *
output_420 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%0,%1,%2";
}
}

static const char *
output_421 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
}

static const char *
output_422 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%0,%1,%2";
}
}

static const char *
output_423 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  operands[2] = GEN_INT (INTVAL (operands[2])+1);
  return "sltu\t%1,%2";
}
}

static const char *
output_428 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.eq.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_429 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_430 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.d\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_431 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.d\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *
output_432 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.d\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *
output_433 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.eq.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_434 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_435 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.s\t%Z0%1,%2", DELAY_FCMP, operands, insn);
}
}

static const char *
output_436 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.lt.s\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *
output_437 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return mips_fill_delay_slot ("c.le.s\t%Z0%2,%1", DELAY_FCMP, operands, insn);
}
}

static const char *
output_438 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (GET_CODE (operands[0]) == REG)
    return "%*j\t%0";
  /* ??? I don't know why this is necessary.  This works around an
     assembler problem that appears when a label is defined, then referenced
     in a switch table, then used in a `j' instruction.  */
  else if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    return "%*b\t%l0";
  else	
    return "%*j\t%l0";
}
}

static const char *
output_449 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  /* .cpadd expands to add REG,REG,$gp when pic, and nothing when not pic.  */
  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    output_asm_insn (".cpadd\t%0", operands);
  return "%*j\t%0";
}
}

static const char *
output_453 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  output_asm_insn ("%(bal\t%S1\n\tsll\t%0,2\n%~%S1:", operands);
  output_asm_insn ("addu\t%0,%0,$31%)", operands);
  output_asm_insn ("lw\t%0,%1-%S1(%0)\n\taddu\t%0,%0,$31", operands);
  return "j\t%0";
}
}

static const char *
output_463 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  return "%*j\t%0";
}
}

static const char *
output_468 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[0];

  if (GET_CODE (target) == SYMBOL_REF)
    return "%*jal\t%0";
  else if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%0\n\t%*jal\t%2,%@%]";
  else
    return "%*jal\t%2,%0";
}
}

static const char *
output_469 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[0];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%0\n\tjal\t%2,%^";
      else
	return "dla\t%^,%0\n\tjal\t%2,%^";
    }
  else if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%0\n\tjal\t%2,%^";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *
output_473 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *
output_474 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[0]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%0\n\tjal\t%2,%^";
  else
    return "jal\t%2,%0";
}
}

static const char *
output_477 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[0];

  if (GET_CODE (target) == SYMBOL_REF)
    return "%*j\t%0";
  else if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%0\n\t%*j\t%@%]";
  else
    return "%*j\t%0";
}
}

static const char *
output_481 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    return "%*jal\t%1";
  else if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%1\n\t%*jal\t%3,%@%]";
  else
    return "%*jal\t%3,%1";
}
}

static const char *
output_482 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%1\n\tjal\t%3,%^";
      else
	return "dla\t%^,%1\n\tjal\t%3,%^";
    }
  else if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%1\n\tjal\t%3,%^";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *
output_486 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *
output_487 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (REGNO (operands[1]) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%3,%^";
  else
    return "jal\t%3,%1";
}
}

static const char *
output_490 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    return "%*j\t%1";
  else if (GET_CODE (target) == CONST_INT)
    return "%[li\t%@,%1\n\t%*j\t%@%]";
  else
    return "%*j\t%1";
}
}

static const char *
output_492 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  register rtx target = operands[1];

  if (GET_CODE (target) == SYMBOL_REF)
    {
      if (GET_MODE (target) == SImode)
	return "la\t%^,%1\n\tjal\t%4,%^";
      else
	return "la\t%^,%1\n\tjal\t%4,%^";
    }
  else if (GET_CODE (target) == CONST_INT)
    return "li\t%^,%1\n\tjal\t%4,%^";
  else if (REGNO (target) != PIC_FUNCTION_ADDR_REGNUM)
    return "move\t%^,%1\n\tjal\t%4,%^";
  else
    return "jal\t%4,%1";
}
}

static const char *
output_495 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_495[] = {
    "mov%B4\t%0,%z2,%1",
    "mov%b4\t%0,%z3,%1",
  };
  return strings_495[which_alternative];
}

static const char *
output_496 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_496[] = {
    "mov%B4\t%0,%z2,%1",
    "mov%b4\t%0,%z3,%1",
  };
  return strings_496[which_alternative];
}

static const char *
output_497 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_497[] = {
    "mov%T3\t%0,%z1,%4",
    "mov%t3\t%0,%z2,%4",
  };
  return strings_497[which_alternative];
}

static const char *
output_498 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_498[] = {
    "mov%B4\t%0,%z2,%1",
    "mov%b4\t%0,%z3,%1",
  };
  return strings_498[which_alternative];
}

static const char *
output_499 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_499[] = {
    "mov%B4\t%0,%z2,%1",
    "mov%b4\t%0,%z3,%1",
  };
  return strings_499[which_alternative];
}

static const char *
output_500 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_500[] = {
    "mov%T3\t%0,%z1,%4",
    "mov%t3\t%0,%z2,%4",
  };
  return strings_500[which_alternative];
}

static const char *
output_501 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_501[] = {
    "mov%B4.s\t%0,%2,%1",
    "mov%b4.s\t%0,%3,%1",
  };
  return strings_501[which_alternative];
}

static const char *
output_502 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_502[] = {
    "mov%T3.s\t%0,%1,%4",
    "mov%t3.s\t%0,%2,%4",
  };
  return strings_502[which_alternative];
}

static const char *
output_503 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_503[] = {
    "mov%B4.d\t%0,%2,%1",
    "mov%b4.d\t%0,%3,%1",
  };
  return strings_503[which_alternative];
}

static const char *
output_504 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{
  static const char *const strings_504[] = {
    "mov%T3.d\t%0,%1,%4",
    "mov%t3.d\t%0,%2,%4",
  };
  return strings_504[which_alternative];
}

static const char *
output_509 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 1, 1);
  return "";
}
}

static const char *
output_510 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 2, 1);
  return "";
}
}

static const char *
output_511 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 4, 1);
  return "";
}
}

static const char *
output_512 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  assemble_integer (operands[0], 8, 1);
  return "";
}
}

static const char *
output_513 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  union real_extract u;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
  assemble_real (u.d, SFmode);
  return "";
}
}

static const char *
output_514 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  union real_extract u;

  if (GET_CODE (operands[0]) != CONST_DOUBLE)
    abort ();
  bcopy ((char *) &CONST_DOUBLE_LOW (operands[0]), (char *) &u, sizeof u);
  assemble_real (u.d, DFmode);
  return "";
}
}

static const char *
output_518 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*b%C2z\t%1,%3";
  else
    return "%*b%N2z\t%1,%4";
}
}

static const char *
output_519 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*b%C2z\t%1,%3";
  else
    return "%*b%N2z\t%1,%4";
}
}

static const char *
output_520 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*bt%C2z\t%3";
  else
    return "%*bt%N2z\t%4";
}
}

static const char *
output_521 (operands, insn)
     rtx *operands ATTRIBUTE_UNUSED;
     rtx insn ATTRIBUTE_UNUSED;
{

{
  if (operands[3] != pc_rtx)
    return "%*bt%C2z\t%3";
  else
    return "%*bt%N2z\t%4";
}
}

const char * const insn_template[] =
  {
    "add.d\t%0,%1,%2",
    "add.s\t%0,%1,%2",
    0,
    "addu\t%0,%z1,%2",
    "addu\t%$,%$,%0",
    "addu\t%0,%$,%1",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "daddu\t%$,%$,%0",
    "daddu\t%0,%$,%1",
    0,
    0,
    0,
    0,
    0,
    "sub.d\t%0,%1,%2",
    "sub.s\t%0,%1,%2",
    0,
    "subu\t%0,%z1,%2",
    "addu\t%$,%$,%n0",
    "addu\t%0,%$,%n1",
    0,
    0,
    0,
    0,
    "sltu\t%3,%L1,%L2\n\tsubu\t%L0,%L1,%L2\n\tsubu\t%M0,%M1,%M2\n\tsubu\t%M0,%M0,%3",
    0,
    0,
    0,
    0,
    0,
    0,
    "daddu\t%$,%$,%n0",
    "daddu\t%0,%$,%n1",
    0,
    0,
    0,
    0,
    0,
    0,
    "mul.d\t%0,%1,%2",
    0,
    0,
    "mul.s\t%0,%1,%2",
    0,
    0,
    0,
    "mult\t%1,%2",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "dmult\t%1,%2",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "dmult\t%1,%2",
    "dmultu\t%1,%2",
    "mad\t%1,%2",
    0,
    0,
    "madd.d\t%0,%3,%1,%2",
    "madd.s\t%0,%3,%1,%2",
    "msub.d\t%0,%3,%1,%2",
    "msub.s\t%0,%3,%1,%2",
    "nmadd.d\t%0,%3,%1,%2",
    "nmadd.s\t%0,%3,%1,%2",
    "nmsub.d\t%0,%1,%2,%3",
    "nmsub.s\t%0,%1,%2,%3",
    "div.d\t%0,%1,%2",
    "div.s\t%0,%1,%2",
    "recip.d\t%0,%2",
    "recip.s\t%0,%2",
    0,
    "div\t$0,%1,%2",
    0,
    "ddiv\t$0,%1,%2",
    0,
    "divu\t$0,%1,%2",
    0,
    "ddivu\t$0,%1,%2",
    0,
    0,
    0,
    0,
    "div\t$0,%1,%2",
    0,
    "ddiv\t$0,%1,%2",
    0,
    "div\t$0,%1,%2",
    0,
    "ddiv\t$0,%1,%2",
    0,
    "divu\t$0,%1,%2",
    0,
    "ddivu\t$0,%1,%2",
    0,
    "divu\t$0,%1,%2",
    0,
    "ddivu\t$0,%1,%2",
    "sqrt.d\t%0,%1",
    "sqrt.s\t%0,%1",
    "rsqrt.d\t%0,%2",
    "rsqrt.s\t%0,%2",
    0,
    0,
    "abs.d\t%0,%1",
    "abs.s\t%0,%1",
    0,
    0,
    0,
    0,
    0,
    0,
    "neg.d\t%0,%1",
    "neg.s\t%0,%1",
    0,
    0,
    0,
    0,
    0,
    "and\t%0,%2",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "or\t%0,%2",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "xor\t%M0,%M2\n\txor\t%L0,%L2",
    0,
    0,
    "xori\t%0,%1,%x2",
    "nor\t%0,%z1,%z2",
    0,
    0,
    "cvt.s.d\t%0,%1",
    0,
    0,
    0,
    0,
    0,
    0,
    "andi\t%0,%1,0xffff",
    "andi\t%0,%1,0xff",
    "andi\t%0,%1,0xff",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "cvt.d.s\t%0,%1",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "lui\t%0,%%hi(%1) # high",
    "addiu\t%0,%1,%%lo(%2) # low",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "lwxc1\t%0,%1(%2)",
    "lwxc1\t%0,%1(%2)",
    "ldxc1\t%0,%1(%2)",
    "ldxc1\t%0,%1(%2)",
    "swxc1\t%0,%1(%2)",
    "swxc1\t%0,%1(%2)",
    "sdxc1\t%0,%1(%2)",
    "sdxc1\t%0,%1(%2)",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "%[lui\t$1,%%hi(%%neg(%%gp_rel(%a0)))\n\taddiu\t$1,$1,%%lo(%%neg(%%gp_rel(%a0)))\n\tdaddu\t$gp,$1,%1%]",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "lw\t%0,%1\n\tsrl\t%0,%2",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "sltu\t%0,%1,1",
    "sltu\t%1,1",
    "sltu\t%0,%1,1",
    "sltu\t%1,1",
    0,
    0,
    0,
    0,
    0,
    "sltu\t%0,%.,%1",
    "sltu\t%0,%.,%1",
    0,
    0,
    0,
    0,
    0,
    "slt\t%0,%z2,%1",
    "slt\t%2,%1",
    "slt\t%0,%z2,%1",
    "slt\t%2,%1",
    0,
    "slt\t%0,%1,%2\n\txori\t%0,%0,0x0001",
    0,
    "slt\t%0,%1,%2\n\txori\t%0,%0,0x0001",
    0,
    0,
    "slt\t%0,%1,%2",
    "slt\t%1,%2",
    "slt\t%0,%1,%2",
    "slt\t%1,%2",
    0,
    0,
    0,
    0,
    0,
    "slt\t%0,%z2,%1\n\txori\t%0,%0,0x0001",
    0,
    "slt\t%0,%z2,%1\n\txori\t%0,%0,0x0001",
    0,
    0,
    "sltu\t%0,%z2,%1",
    "sltu\t%2,%1",
    "sltu\t%0,%z2,%1",
    "sltu\t%2,%1",
    0,
    "sltu\t%0,%1,%2\n\txori\t%0,%0,0x0001",
    0,
    "sltu\t%0,%1,%2\n\txori\t%0,%0,0x0001",
    0,
    0,
    "sltu\t%0,%1,%2",
    "sltu\t%1,%2",
    "sltu\t%0,%1,%2",
    "sltu\t%1,%2",
    0,
    0,
    0,
    0,
    0,
    "sltu\t%0,%z2,%1\n\txori\t%0,%0,0x0001",
    0,
    "sltu\t%0,%z2,%1\n\txori\t%0,%0,0x0001",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "b\t%l0",
    0,
    "%*j\t%0",
    "%*j\t%0",
    0,
    "%*j\t%0",
    "%*j\t%0",
    0,
    0,
    0,
    0,
    0,
    "%*j\t%0",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "",
    0,
    0,
    "%*j\t$31",
    0,
    "%($LF%= = . + 8\n\tbal\t$LF%=\n\tla\t%0,%1-$LF%=%)\n\taddu\t%0,%0,$31",
    0,
    0,
    "%*jal\t%0",
    0,
    0,
    "%*jal\t%2,%0",
    "%*jal\t%2,%0",
    "%*jal\t%2,%0",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "%*jal\t%1",
    0,
    0,
    "%*jal\t%3,%1",
    "%*jal\t%3,%1",
    "%*jal\t%3,%1",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    "%(nop%)",
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    ".align 1",
    ".align 2",
    ".align 3",
    0,
    0,
    0,
    0,
    "la %0,%a1",
    "la %0,%a1",
  };

const char *(*const insn_outfun[])() =
  {
    0,
    0,
    0,
    0,
    0,
    0,
    output_6,
    0,
    0,
    0,
    output_10,
    0,
    0,
    output_13,
    0,
    0,
    output_16,
    0,
    0,
    output_19,
    0,
    0,
    output_22,
    output_23,
    0,
    0,
    0,
    0,
    0,
    0,
    output_30,
    0,
    0,
    0,
    0,
    0,
    0,
    output_37,
    0,
    0,
    output_40,
    0,
    0,
    output_43,
    0,
    0,
    output_46,
    output_47,
    0,
    0,
    output_50,
    0,
    0,
    output_53,
    0,
    output_55,
    0,
    output_57,
    output_58,
    0,
    output_60,
    output_61,
    0,
    0,
    0,
    output_65,
    0,
    0,
    output_68,
    output_69,
    output_70,
    output_71,
    0,
    0,
    output_74,
    output_75,
    output_76,
    0,
    0,
    0,
    output_80,
    output_81,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_103,
    output_104,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_125,
    output_126,
    0,
    0,
    output_129,
    output_130,
    output_131,
    0,
    output_133,
    output_134,
    0,
    0,
    output_137,
    output_138,
    0,
    0,
    output_141,
    0,
    0,
    output_144,
    output_145,
    0,
    output_147,
    0,
    output_149,
    0,
    0,
    output_152,
    output_153,
    0,
    0,
    output_156,
    output_157,
    0,
    output_159,
    0,
    output_161,
    0,
    0,
    0,
    output_165,
    0,
    0,
    output_168,
    output_169,
    output_170,
    output_171,
    output_172,
    output_173,
    0,
    0,
    0,
    0,
    output_178,
    0,
    output_180,
    output_181,
    0,
    output_183,
    output_184,
    0,
    output_186,
    output_187,
    0,
    output_189,
    output_190,
    0,
    output_192,
    output_193,
    output_194,
    output_195,
    0,
    output_197,
    0,
    output_199,
    0,
    output_201,
    0,
    output_203,
    0,
    output_205,
    0,
    output_207,
    output_208,
    output_209,
    output_210,
    output_211,
    output_212,
    output_213,
    output_214,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_222,
    output_223,
    output_224,
    output_225,
    0,
    0,
    0,
    output_229,
    output_230,
    output_231,
    0,
    output_233,
    output_234,
    0,
    0,
    0,
    0,
    0,
    output_240,
    output_241,
    output_242,
    output_243,
    0,
    0,
    0,
    0,
    0,
    output_249,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_261,
    output_262,
    output_263,
    0,
    0,
    output_266,
    output_267,
    output_268,
    0,
    0,
    output_271,
    output_272,
    output_273,
    0,
    output_275,
    output_276,
    output_277,
    output_278,
    0,
    0,
    0,
    output_282,
    output_283,
    output_284,
    output_285,
    0,
    output_287,
    output_288,
    output_289,
    output_290,
    0,
    output_292,
    output_293,
    0,
    0,
    output_296,
    output_297,
    0,
    0,
    output_300,
    0,
    0,
    output_303,
    output_304,
    0,
    0,
    output_307,
    output_308,
    0,
    0,
    output_311,
    output_312,
    0,
    0,
    output_315,
    0,
    0,
    output_318,
    output_319,
    0,
    0,
    output_322,
    output_323,
    0,
    0,
    0,
    0,
    output_328,
    output_329,
    0,
    0,
    output_332,
    0,
    0,
    output_335,
    output_336,
    output_337,
    output_338,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_346,
    output_347,
    output_348,
    output_349,
    output_350,
    output_351,
    output_352,
    output_353,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_369,
    0,
    output_371,
    0,
    0,
    0,
    0,
    output_376,
    0,
    output_378,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_396,
    output_397,
    output_398,
    output_399,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_420,
    output_421,
    output_422,
    output_423,
    0,
    0,
    0,
    0,
    output_428,
    output_429,
    output_430,
    output_431,
    output_432,
    output_433,
    output_434,
    output_435,
    output_436,
    output_437,
    output_438,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_449,
    0,
    0,
    0,
    output_453,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    output_463,
    0,
    0,
    0,
    0,
    output_468,
    output_469,
    0,
    0,
    0,
    output_473,
    output_474,
    0,
    0,
    output_477,
    0,
    0,
    0,
    output_481,
    output_482,
    0,
    0,
    0,
    output_486,
    output_487,
    0,
    0,
    output_490,
    0,
    output_492,
    0,
    0,
    output_495,
    output_496,
    output_497,
    output_498,
    output_499,
    output_500,
    output_501,
    output_502,
    output_503,
    output_504,
    0,
    0,
    0,
    0,
    output_509,
    output_510,
    output_511,
    output_512,
    output_513,
    output_514,
    0,
    0,
    0,
    output_518,
    output_519,
    output_520,
    output_521,
    0,
    0,
  };

rtx (*const insn_gen_function[]) () =
  {
    gen_adddf3,
    gen_addsf3,
    gen_addsi3,
    gen_addsi3_internal,
    0,
    0,
    0,
    0,
    0,
    gen_adddi3,
    gen_adddi3_internal_1,
    0,
    0,
    gen_adddi3_internal_2,
    0,
    0,
    gen_adddi3_internal_3,
    0,
    0,
    0,
    0,
    0,
    gen_addsi3_internal_2,
    0,
    gen_subdf3,
    gen_subsf3,
    gen_subsi3,
    gen_subsi3_internal,
    0,
    0,
    0,
    0,
    0,
    gen_subdi3,
    gen_subdi3_internal,
    0,
    0,
    gen_subdi3_internal_2,
    0,
    0,
    gen_subdi3_internal_3,
    0,
    0,
    0,
    0,
    0,
    gen_subsi3_internal_2,
    0,
    gen_muldf3,
    gen_muldf3_internal,
    gen_muldf3_r4300,
    gen_mulsf3,
    gen_mulsf3_internal,
    gen_mulsf3_r4300,
    gen_mulsi3,
    gen_mulsi3_mult3,
    gen_mulsi3_internal,
    gen_mulsi3_r4000,
    0,
    0,
    0,
    0,
    0,
    gen_muldi3,
    gen_muldi3_internal,
    gen_muldi3_internal2,
    gen_mulsidi3,
    gen_umulsidi3,
    gen_mulsidi3_internal,
    gen_mulsidi3_64bit,
    0,
    0,
    gen_smulsi3_highpart,
    gen_umulsi3_highpart,
    gen_xmulsi3_highpart_internal,
    gen_xmulsi3_highpart_5400,
    0,
    gen_smuldi3_highpart,
    gen_umuldi3_highpart,
    gen_madsi,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    gen_divdf3,
    gen_divsf3,
    0,
    0,
    gen_divmodsi4,
    gen_divmodsi4_internal,
    gen_divmoddi4,
    gen_divmoddi4_internal,
    gen_udivmodsi4,
    gen_udivmodsi4_internal,
    gen_udivmoddi4,
    gen_udivmoddi4_internal,
    gen_div_trap,
    gen_div_trap_normal,
    gen_div_trap_mips16,
    gen_divsi3,
    gen_divsi3_internal,
    gen_divdi3,
    gen_divdi3_internal,
    gen_modsi3,
    gen_modsi3_internal,
    gen_moddi3,
    gen_moddi3_internal,
    gen_udivsi3,
    gen_udivsi3_internal,
    gen_udivdi3,
    gen_udivdi3_internal,
    gen_umodsi3,
    gen_umodsi3_internal,
    gen_umoddi3,
    gen_umoddi3_internal,
    gen_sqrtdf2,
    gen_sqrtsf2,
    0,
    0,
    gen_abssi2,
    gen_absdi2,
    gen_absdf2,
    gen_abssf2,
    gen_ffssi2,
    gen_ffsdi2,
    gen_negsi2,
    gen_negdi2,
    gen_negdi2_internal,
    gen_negdi2_internal_2,
    gen_negdf2,
    gen_negsf2,
    gen_one_cmplsi2,
    gen_one_cmpldi2,
    0,
    gen_andsi3,
    0,
    0,
    gen_anddi3,
    0,
    0,
    0,
    gen_anddi3_internal1,
    gen_iorsi3,
    0,
    0,
    gen_iordi3,
    0,
    0,
    0,
    gen_xorsi3,
    0,
    0,
    gen_xordi3,
    0,
    0,
    0,
    0,
    gen_xordi3_immed,
    0,
    0,
    0,
    gen_truncdfsf2,
    gen_truncdisi2,
    gen_truncdihi2,
    gen_truncdiqi2,
    0,
    0,
    0,
    0,
    0,
    0,
    gen_zero_extendsidi2,
    gen_zero_extendsidi2_internal,
    gen_zero_extendhisi2,
    0,
    0,
    gen_zero_extendhidi2,
    0,
    0,
    gen_zero_extendqihi2,
    0,
    0,
    gen_zero_extendqisi2,
    0,
    0,
    gen_zero_extendqidi2,
    0,
    0,
    0,
    gen_extendsidi2,
    gen_extendhidi2,
    gen_extendhidi2_internal,
    gen_extendhisi2,
    gen_extendhisi2_internal,
    gen_extendqihi2,
    gen_extendqihi2_internal,
    gen_extendqisi2,
    gen_extendqisi2_insn,
    gen_extendqidi2,
    gen_extendqidi2_insn,
    gen_extendsfdf2,
    gen_fix_truncdfsi2,
    gen_fix_truncsfsi2,
    gen_fix_truncdfdi2,
    gen_fix_truncsfdi2,
    gen_floatsidf2,
    gen_floatdidf2,
    gen_floatsisf2,
    gen_floatdisf2,
    gen_fixuns_truncdfsi2,
    gen_fixuns_truncdfdi2,
    gen_fixuns_truncsfsi2,
    gen_fixuns_truncsfdi2,
    gen_extv,
    gen_extzv,
    gen_insv,
    gen_movsi_ulw,
    gen_movsi_usw,
    gen_movdi_uld,
    gen_movdi_usd,
    gen_high,
    gen_low,
    gen_movdi,
    0,
    gen_movdi_internal,
    0,
    0,
    gen_movdi_internal2,
    0,
    0,
    gen_reload_indi,
    gen_reload_outdi,
    0,
    gen_movsi,
    0,
    gen_movsi_internal1,
    gen_movsi_internal2,
    0,
    0,
    0,
    0,
    gen_reload_outsi,
    gen_reload_insi,
    gen_movcc,
    gen_reload_incc,
    gen_reload_outcc,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    gen_movhi,
    gen_movhi_internal1,
    gen_movhi_internal2,
    0,
    0,
    gen_movqi,
    gen_movqi_internal1,
    gen_movqi_internal2,
    0,
    0,
    gen_movsf,
    gen_movsf_internal1,
    gen_movsf_internal2,
    0,
    gen_movdf,
    gen_movdf_internal1,
    gen_movdf_internal1a,
    gen_movdf_internal2,
    0,
    0,
    gen_loadgp,
    gen_movstrsi,
    gen_movstrsi_internal,
    0,
    0,
    0,
    0,
    gen_movstrsi_internal2,
    0,
    gen_movstrsi_internal3,
    0,
    gen_ashlsi3,
    gen_ashlsi3_internal1,
    gen_ashlsi3_internal2,
    0,
    gen_ashldi3,
    gen_ashldi3_internal,
    gen_ashldi3_internal2,
    0,
    0,
    gen_ashldi3_internal3,
    0,
    0,
    gen_ashldi3_internal4,
    0,
    0,
    gen_ashrsi3,
    gen_ashrsi3_internal1,
    gen_ashrsi3_internal2,
    0,
    gen_ashrdi3,
    gen_ashrdi3_internal,
    gen_ashrdi3_internal2,
    0,
    0,
    gen_ashrdi3_internal3,
    0,
    0,
    gen_ashrdi3_internal4,
    0,
    0,
    gen_lshrsi3,
    gen_lshrsi3_internal1,
    gen_lshrsi3_internal2,
    0,
    0,
    0,
    gen_lshrdi3,
    gen_lshrdi3_internal,
    gen_lshrdi3_internal2,
    0,
    0,
    gen_lshrdi3_internal3,
    0,
    0,
    gen_lshrdi3_internal4,
    gen_rotrsi3,
    gen_rotrdi3,
    0,
    0,
    gen_cmpsi,
    gen_tstsi,
    gen_cmpdi,
    gen_tstdi,
    gen_cmpdf,
    gen_cmpsf,
    gen_branch_fp_ne,
    gen_branch_fp_eq,
    gen_branch_zero,
    0,
    gen_branch_zero_di,
    0,
    gen_branch_equality,
    gen_branch_equality_di,
    gen_beq,
    gen_bne,
    gen_bgt,
    gen_bge,
    gen_blt,
    gen_ble,
    gen_bgtu,
    gen_bgeu,
    gen_bltu,
    gen_bleu,
    gen_seq,
    gen_seq_si_zero,
    0,
    gen_seq_di_zero,
    0,
    gen_seq_si,
    0,
    gen_seq_di,
    0,
    gen_sne,
    gen_sne_si_zero,
    gen_sne_di_zero,
    gen_sne_si,
    0,
    gen_sne_di,
    0,
    gen_sgt,
    gen_sgt_si,
    0,
    gen_sgt_di,
    0,
    gen_sge,
    gen_sge_si,
    0,
    gen_sge_di,
    0,
    gen_slt,
    gen_slt_si,
    0,
    gen_slt_di,
    0,
    gen_sle,
    gen_sle_si_const,
    0,
    gen_sle_di_const,
    0,
    gen_sle_si_reg,
    0,
    gen_sle_di_reg,
    0,
    gen_sgtu,
    gen_sgtu_si,
    0,
    gen_sgtu_di,
    0,
    gen_sgeu,
    gen_sgeu_si,
    0,
    gen_sgeu_di,
    0,
    gen_sltu,
    gen_sltu_si,
    0,
    gen_sltu_di,
    0,
    gen_sleu,
    gen_sleu_si_const,
    0,
    gen_sleu_di_const,
    0,
    gen_sleu_si_reg,
    0,
    gen_sleu_di_reg,
    0,
    gen_seq_df,
    gen_slt_df,
    gen_sle_df,
    gen_sgt_df,
    gen_sge_df,
    gen_seq_sf,
    gen_slt_sf,
    gen_sle_sf,
    gen_sgt_sf,
    gen_sge_sf,
    gen_jump,
    0,
    gen_indirect_jump,
    gen_indirect_jump_internal1,
    gen_indirect_jump_internal2,
    gen_tablejump,
    gen_tablejump_internal1,
    gen_tablejump_internal2,
    gen_tablejump_internal3,
    gen_tablejump_mips161,
    gen_tablejump_mips162,
    0,
    gen_tablejump_internal4,
    0,
    gen_casesi,
    gen_casesi_internal,
    gen_builtin_setjmp_setup,
    gen_builtin_setjmp_setup_32,
    gen_builtin_setjmp_setup_64,
    gen_builtin_longjmp,
    gen_prologue,
    gen_blockage,
    gen_epilogue,
    gen_sibcall_epilogue,
    gen_return,
    gen_return_internal,
    gen_get_fnaddr,
    gen_call,
    gen_call_internal0,
    0,
    gen_call_internal1,
    gen_call_internal2,
    gen_call_internal3a,
    gen_call_internal3b,
    gen_call_internal3c,
    gen_call_internal4a,
    gen_call_internal4b,
    gen_sibcall,
    gen_sibcall_internal0,
    gen_sibcall_internal1,
    gen_call_value,
    gen_call_value_internal0,
    0,
    gen_call_value_internal1,
    gen_call_value_internal2,
    gen_call_value_internal3a,
    gen_call_value_internal3b,
    gen_call_value_internal3c,
    gen_call_value_internal4a,
    gen_call_value_internal4b,
    gen_sibcall_value,
    gen_sibcall_value_internal0,
    gen_sibcall_value_internal1,
    gen_call_value_multiple_internal0,
    gen_call_value_multiple_internal2,
    gen_untyped_call,
    gen_nop,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    gen_movsicc,
    gen_movdicc,
    gen_movsfcc,
    gen_movdfcc,
    gen_consttable_qi,
    gen_consttable_hi,
    gen_consttable_si,
    gen_consttable_di,
    gen_consttable_sf,
    gen_consttable_df,
    gen_align_2,
    gen_align_4,
    gen_align_8,
    0,
    0,
    0,
    0,
    gen_leasi,
    gen_leadi,
  };

const char *insn_name[] =
  {
    "adddf3",
    "addsf3",
    "addsi3",
    "addsi3_internal",
    "addsi3_internal+1",
    "addsi3_internal+2",
    "addsi3_internal+3",
    "adddi3-2",
    "adddi3-1",
    "adddi3",
    "adddi3_internal_1",
    "adddi3_internal_1+1",
    "adddi3_internal_2-1",
    "adddi3_internal_2",
    "adddi3_internal_2+1",
    "adddi3_internal_3-1",
    "adddi3_internal_3",
    "adddi3_internal_3+1",
    "adddi3_internal_3+2",
    "adddi3_internal_3+3",
    "addsi3_internal_2-2",
    "addsi3_internal_2-1",
    "addsi3_internal_2",
    "addsi3_internal_2+1",
    "subdf3",
    "subsf3",
    "subsi3",
    "subsi3_internal",
    "subsi3_internal+1",
    "subsi3_internal+2",
    "subsi3_internal+3",
    "subdi3-2",
    "subdi3-1",
    "subdi3",
    "subdi3_internal",
    "subdi3_internal+1",
    "subdi3_internal_2-1",
    "subdi3_internal_2",
    "subdi3_internal_2+1",
    "subdi3_internal_3-1",
    "subdi3_internal_3",
    "subdi3_internal_3+1",
    "subdi3_internal_3+2",
    "subdi3_internal_3+3",
    "subsi3_internal_2-2",
    "subsi3_internal_2-1",
    "subsi3_internal_2",
    "subsi3_internal_2+1",
    "muldf3",
    "muldf3_internal",
    "muldf3_r4300",
    "mulsf3",
    "mulsf3_internal",
    "mulsf3_r4300",
    "mulsi3",
    "mulsi3_mult3",
    "mulsi3_internal",
    "mulsi3_r4000",
    "*mul_acc_si",
    "*mul_acc_si+1",
    "*muls_r5400",
    "*msac_r5400",
    "*msac_r5400+1",
    "muldi3",
    "muldi3_internal",
    "muldi3_internal2",
    "mulsidi3",
    "umulsidi3",
    "mulsidi3_internal",
    "mulsidi3_64bit",
    "*muls_r5400_di",
    "*msac_r5400_di",
    "smulsi3_highpart",
    "umulsi3_highpart",
    "xmulsi3_highpart_internal",
    "xmulsi3_highpart_5400",
    "*xmulsi3_neg_highpart_5400",
    "smuldi3_highpart",
    "umuldi3_highpart",
    "madsi",
    "*mul_acc_di",
    "*mul_acc_64bit_di",
    "*mul_acc_64bit_di+1",
    "*mul_acc_64bit_di+2",
    "*mul_acc_64bit_di+3",
    "*mul_acc_64bit_di+4",
    "divdf3-4",
    "divdf3-3",
    "divdf3-2",
    "divdf3-1",
    "divdf3",
    "divsf3",
    "divsf3+1",
    "divmodsi4-1",
    "divmodsi4",
    "divmodsi4_internal",
    "divmoddi4",
    "divmoddi4_internal",
    "udivmodsi4",
    "udivmodsi4_internal",
    "udivmoddi4",
    "udivmoddi4_internal",
    "div_trap",
    "div_trap_normal",
    "div_trap_mips16",
    "divsi3",
    "divsi3_internal",
    "divdi3",
    "divdi3_internal",
    "modsi3",
    "modsi3_internal",
    "moddi3",
    "moddi3_internal",
    "udivsi3",
    "udivsi3_internal",
    "udivdi3",
    "udivdi3_internal",
    "umodsi3",
    "umodsi3_internal",
    "umoddi3",
    "umoddi3_internal",
    "sqrtdf2",
    "sqrtsf2",
    "sqrtsf2+1",
    "abssi2-1",
    "abssi2",
    "absdi2",
    "absdf2",
    "abssf2",
    "ffssi2",
    "ffsdi2",
    "negsi2",
    "negdi2",
    "negdi2_internal",
    "negdi2_internal_2",
    "negdf2",
    "negsf2",
    "one_cmplsi2",
    "one_cmpldi2",
    "one_cmpldi2+1",
    "andsi3",
    "andsi3+1",
    "anddi3-1",
    "anddi3",
    "anddi3+1",
    "anddi3+2",
    "anddi3_internal1-1",
    "anddi3_internal1",
    "iorsi3",
    "iorsi3+1",
    "iordi3-1",
    "iordi3",
    "iordi3+1",
    "iordi3+2",
    "xorsi3-1",
    "xorsi3",
    "xorsi3+1",
    "xordi3-1",
    "xordi3",
    "xordi3+1",
    "xordi3+2",
    "xordi3_immed-2",
    "xordi3_immed-1",
    "xordi3_immed",
    "*norsi3",
    "*nordi3",
    "*nordi3+1",
    "truncdfsf2",
    "truncdisi2",
    "truncdihi2",
    "truncdiqi2",
    "truncdiqi2+1",
    "truncdiqi2+2",
    "truncdiqi2+3",
    "zero_extendsidi2-3",
    "zero_extendsidi2-2",
    "zero_extendsidi2-1",
    "zero_extendsidi2",
    "zero_extendsidi2_internal",
    "zero_extendhisi2",
    "zero_extendhisi2+1",
    "zero_extendhidi2-1",
    "zero_extendhidi2",
    "zero_extendhidi2+1",
    "zero_extendqihi2-1",
    "zero_extendqihi2",
    "zero_extendqihi2+1",
    "zero_extendqisi2-1",
    "zero_extendqisi2",
    "zero_extendqisi2+1",
    "zero_extendqidi2-1",
    "zero_extendqidi2",
    "zero_extendqidi2+1",
    "*paradoxical_extendhidi2",
    "*paradoxical_extendhidi2+1",
    "extendsidi2",
    "extendhidi2",
    "extendhidi2_internal",
    "extendhisi2",
    "extendhisi2_internal",
    "extendqihi2",
    "extendqihi2_internal",
    "extendqisi2",
    "extendqisi2_insn",
    "extendqidi2",
    "extendqidi2_insn",
    "extendsfdf2",
    "fix_truncdfsi2",
    "fix_truncsfsi2",
    "fix_truncdfdi2",
    "fix_truncsfdi2",
    "floatsidf2",
    "floatdidf2",
    "floatsisf2",
    "floatdisf2",
    "fixuns_truncdfsi2",
    "fixuns_truncdfdi2",
    "fixuns_truncsfsi2",
    "fixuns_truncsfdi2",
    "extv",
    "extzv",
    "insv",
    "movsi_ulw",
    "movsi_usw",
    "movdi_uld",
    "movdi_usd",
    "high",
    "low",
    "movdi",
    "movdi+1",
    "movdi_internal",
    "movdi_internal+1",
    "movdi_internal2-1",
    "movdi_internal2",
    "movdi_internal2+1",
    "reload_indi-1",
    "reload_indi",
    "reload_outdi",
    "reload_outdi+1",
    "movsi",
    "movsi+1",
    "movsi_internal1",
    "movsi_internal2",
    "movsi_internal2+1",
    "movsi_internal2+2",
    "reload_outsi-2",
    "reload_outsi-1",
    "reload_outsi",
    "reload_insi",
    "movcc",
    "reload_incc",
    "reload_outcc",
    "reload_outcc+1",
    "reload_outcc+2",
    "reload_outcc+3",
    "reload_outcc+4",
    "movhi-4",
    "movhi-3",
    "movhi-2",
    "movhi-1",
    "movhi",
    "movhi_internal1",
    "movhi_internal2",
    "movhi_internal2+1",
    "movqi-1",
    "movqi",
    "movqi_internal1",
    "movqi_internal2",
    "movqi_internal2+1",
    "movsf-1",
    "movsf",
    "movsf_internal1",
    "movsf_internal2",
    "movsf_internal2+1",
    "movdf",
    "movdf_internal1",
    "movdf_internal1a",
    "movdf_internal2",
    "movdf_internal2+1",
    "loadgp-1",
    "loadgp",
    "movstrsi",
    "movstrsi_internal",
    "movstrsi_internal+1",
    "movstrsi_internal+2",
    "movstrsi_internal2-2",
    "movstrsi_internal2-1",
    "movstrsi_internal2",
    "movstrsi_internal2+1",
    "movstrsi_internal3",
    "movstrsi_internal3+1",
    "ashlsi3",
    "ashlsi3_internal1",
    "ashlsi3_internal2",
    "ashlsi3_internal2+1",
    "ashldi3",
    "ashldi3_internal",
    "ashldi3_internal2",
    "ashldi3_internal2+1",
    "ashldi3_internal3-1",
    "ashldi3_internal3",
    "ashldi3_internal3+1",
    "ashldi3_internal4-1",
    "ashldi3_internal4",
    "ashldi3_internal4+1",
    "ashrsi3-1",
    "ashrsi3",
    "ashrsi3_internal1",
    "ashrsi3_internal2",
    "ashrsi3_internal2+1",
    "ashrdi3",
    "ashrdi3_internal",
    "ashrdi3_internal2",
    "ashrdi3_internal2+1",
    "ashrdi3_internal3-1",
    "ashrdi3_internal3",
    "ashrdi3_internal3+1",
    "ashrdi3_internal4-1",
    "ashrdi3_internal4",
    "ashrdi3_internal4+1",
    "lshrsi3-1",
    "lshrsi3",
    "lshrsi3_internal1",
    "lshrsi3_internal2",
    "lshrsi3_internal2+1",
    "lshrsi3_internal2+2",
    "lshrdi3-1",
    "lshrdi3",
    "lshrdi3_internal",
    "lshrdi3_internal2",
    "lshrdi3_internal2+1",
    "lshrdi3_internal3-1",
    "lshrdi3_internal3",
    "lshrdi3_internal3+1",
    "lshrdi3_internal4-1",
    "lshrdi3_internal4",
    "rotrsi3",
    "rotrdi3",
    "rotrdi3+1",
    "cmpsi-1",
    "cmpsi",
    "tstsi",
    "cmpdi",
    "tstdi",
    "cmpdf",
    "cmpsf",
    "branch_fp_ne",
    "branch_fp_eq",
    "branch_zero",
    "branch_zero+1",
    "branch_zero_di",
    "branch_zero_di+1",
    "branch_equality",
    "branch_equality_di",
    "beq",
    "bne",
    "bgt",
    "bge",
    "blt",
    "ble",
    "bgtu",
    "bgeu",
    "bltu",
    "bleu",
    "seq",
    "seq_si_zero",
    "seq_si_zero+1",
    "seq_di_zero",
    "seq_di_zero+1",
    "seq_si",
    "seq_si+1",
    "seq_di",
    "seq_di+1",
    "sne",
    "sne_si_zero",
    "sne_di_zero",
    "sne_si",
    "sne_si+1",
    "sne_di",
    "sne_di+1",
    "sgt",
    "sgt_si",
    "sgt_si+1",
    "sgt_di",
    "sgt_di+1",
    "sge",
    "sge_si",
    "sge_si+1",
    "sge_di",
    "sge_di+1",
    "slt",
    "slt_si",
    "slt_si+1",
    "slt_di",
    "slt_di+1",
    "sle",
    "sle_si_const",
    "sle_si_const+1",
    "sle_di_const",
    "sle_di_const+1",
    "sle_si_reg",
    "sle_si_reg+1",
    "sle_di_reg",
    "sle_di_reg+1",
    "sgtu",
    "sgtu_si",
    "sgtu_si+1",
    "sgtu_di",
    "sgtu_di+1",
    "sgeu",
    "sgeu_si",
    "sgeu_si+1",
    "sgeu_di",
    "sgeu_di+1",
    "sltu",
    "sltu_si",
    "sltu_si+1",
    "sltu_di",
    "sltu_di+1",
    "sleu",
    "sleu_si_const",
    "sleu_si_const+1",
    "sleu_di_const",
    "sleu_di_const+1",
    "sleu_si_reg",
    "sleu_si_reg+1",
    "sleu_di_reg",
    "sleu_di_reg+1",
    "seq_df",
    "slt_df",
    "sle_df",
    "sgt_df",
    "sge_df",
    "seq_sf",
    "slt_sf",
    "sle_sf",
    "sgt_sf",
    "sge_sf",
    "jump",
    "jump+1",
    "indirect_jump",
    "indirect_jump_internal1",
    "indirect_jump_internal2",
    "tablejump",
    "tablejump_internal1",
    "tablejump_internal2",
    "tablejump_internal3",
    "tablejump_mips161",
    "tablejump_mips162",
    "tablejump_mips162+1",
    "tablejump_internal4",
    "tablejump_internal4+1",
    "casesi",
    "casesi_internal",
    "builtin_setjmp_setup",
    "builtin_setjmp_setup_32",
    "builtin_setjmp_setup_64",
    "builtin_longjmp",
    "prologue",
    "blockage",
    "epilogue",
    "sibcall_epilogue",
    "return",
    "return_internal",
    "get_fnaddr",
    "call",
    "call_internal0",
    "call_internal0+1",
    "call_internal1",
    "call_internal2",
    "call_internal3a",
    "call_internal3b",
    "call_internal3c",
    "call_internal4a",
    "call_internal4b",
    "sibcall",
    "sibcall_internal0",
    "sibcall_internal1",
    "call_value",
    "call_value_internal0",
    "call_value_internal0+1",
    "call_value_internal1",
    "call_value_internal2",
    "call_value_internal3a",
    "call_value_internal3b",
    "call_value_internal3c",
    "call_value_internal4a",
    "call_value_internal4b",
    "sibcall_value",
    "sibcall_value_internal0",
    "sibcall_value_internal1",
    "call_value_multiple_internal0",
    "call_value_multiple_internal2",
    "untyped_call",
    "nop",
    "nop+1",
    "nop+2",
    "nop+3",
    "nop+4",
    "nop+5",
    "movsicc-5",
    "movsicc-4",
    "movsicc-3",
    "movsicc-2",
    "movsicc-1",
    "movsicc",
    "movdicc",
    "movsfcc",
    "movdfcc",
    "consttable_qi",
    "consttable_hi",
    "consttable_si",
    "consttable_di",
    "consttable_sf",
    "consttable_df",
    "align_2",
    "align_4",
    "align_8",
    "align_8+1",
    "align_8+2",
    "leasi-2",
    "leasi-1",
    "leasi",
    "leadi",
  };
const char **insn_name_ptr = insn_name;

const int insn_n_operands[] =
  {
    3,
    3,
    3,
    3,
    1,
    2,
    3,
    2,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    1,
    2,
    3,
    2,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    1,
    2,
    3,
    2,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    1,
    2,
    3,
    2,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    5,
    6,
    5,
    6,
    8,
    8,
    6,
    8,
    8,
    5,
    5,
    6,
    3,
    3,
    6,
    7,
    7,
    8,
    3,
    3,
    8,
    9,
    9,
    5,
    5,
    5,
    6,
    7,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    7,
    7,
    7,
    7,
    7,
    7,
    7,
    7,
    3,
    3,
    3,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    2,
    2,
    3,
    3,
    2,
    2,
    2,
    2,
    4,
    4,
    2,
    2,
    3,
    2,
    2,
    2,
    2,
    2,
    2,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    2,
    2,
    2,
    2,
    3,
    3,
    3,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    4,
    4,
    3,
    3,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    4,
    4,
    4,
    2,
    2,
    2,
    2,
    2,
    3,
    2,
    1,
    2,
    2,
    2,
    2,
    2,
    2,
    3,
    3,
    2,
    2,
    1,
    2,
    2,
    2,
    2,
    2,
    2,
    3,
    3,
    2,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    4,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    8,
    3,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    3,
    2,
    1,
    2,
    1,
    2,
    2,
    3,
    3,
    4,
    4,
    4,
    4,
    5,
    5,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    2,
    2,
    2,
    3,
    3,
    3,
    3,
    1,
    2,
    2,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    1,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    1,
    1,
    1,
    1,
    1,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    7,
    3,
    1,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    1,
    2,
    4,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    3,
    4,
    2,
    2,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    4,
    3,
    3,
    5,
    5,
    3,
    0,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    5,
    4,
    4,
    4,
    4,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    0,
    0,
    5,
    5,
    5,
    5,
    2,
    2,
  };

const int insn_n_dups[] =
  {
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    0,
    0,
    1,
    1,
    1,
    3,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    2,
    2,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
  };

const char *const insn_operand_constraint[][MAX_RECOG_OPERANDS] =
  {
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=d", "dJ", "dI", },
    { "=d", "dJ", "dI", },
    { "I", },
    { "=d", "I", },
    { "=d,d,d", "0,d,d", "IQ,O,d", },
    { "", "", },
    { "", "", "", },
    { "", "", "", },
    { "=d,&d", "0,d", "d,d", "=d,d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d,d,d", "%d,%d,%d", "P,J,N", "=d,d,d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "dJ", "dI", },
    { "I", },
    { "=d", "I", },
    { "=d,d,d", "0,d,d", "IQ,O,d", },
    { "", "", },
    { "", "", "", },
    { "=d", "dJ", "dI", },
    { "=d,d,d", "0,d,d", "I,O,d", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=d", "dJ", "dI", },
    { "=d", "dJ", "dI", },
    { "I", },
    { "=d", "I", },
    { "=d,d,d", "0,d,d", "I,O,d", },
    { "", "", },
    { "", "", "", },
    { "=d", "d", "d", },
    { "=d", "d", "d", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d,d,d", "d,d,d", "P,J,N", "=d,d,d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "dJ", "dI", },
    { "I", },
    { "=d", "I", },
    { "=d,d,d", "0,d,d", "I,O,d", },
    { "", "", },
    { "", "", "", },
    { "=d", "dJ", "dI", },
    { "=d,d,d", "0,d,d", "I,O,d", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=l", "d", "d", "=h", "=a", },
    { "=d,l", "d,d", "d,d", "=h,h", "=l,X", "=a,a", },
    { "=l", "d", "d", "=h", "=a", },
    { "=d", "d", "d", "=h", "=l", "=a", },
    { "=l,*d,*d", "d,d,d", "d,d,d", "0,l,*d", "=h,h,h", "=X,3,l", "=a,a,a", "=X,X,d", },
    { "", "", "", "", "", "", "", "", },
    { "=l,d", "d,d", "d,d", "=h,h", "=a,a", "=X,l", },
    { "=l,*d,*d", "0,l,*d", "d,d,d", "d,d,d", "=h,h,h", "=X,1,l", "=a,a,a", "=X,X,d", },
    { "", "", "", "", "", "", "", "", },
    { "=l", "d", "d", "=h", "=a", },
    { "=l", "d", "d", "=h", "=a", },
    { "=d", "d", "d", "=h", "=l", "=a", },
    { "=x", "d", "d", },
    { "=x", "d", "d", },
    { "=x", "d", "d", "", "", "=a", },
    { "=a", "d", "d", "", "", "=l", "=h", },
    { "=a", "d", "d", "", "", "=h", "=l", },
    { "=a", "d", "d", "0", "", "", "=h", "=l", },
    { "=h", "d", "d", },
    { "=h", "d", "d", },
    { "=h", "d", "d", "", "", "", "=l", "=a", },
    { "=h,d", "d,d", "d,d", "", "", "", "=l,l", "=a,a", "=X,h", },
    { "=h,d", "d,d", "d,d", "", "", "", "=l,l", "=a,a", "=X,h", },
    { "=h", "d", "d", "=l", "=a", },
    { "=h", "d", "d", "=l", "=a", },
    { "+l", "d", "d", "=h", "=a", },
    { "+x", "d", "d", "", "", "=a", },
    { "+a", "d", "d", "", "", "=h", "=l", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "f", "f", },
    { "=f", "", "f", },
    { "=f", "", "f", },
    { "=d", "d", "d", "=d", "=l", "=h", "=a", },
    { "=l", "d", "d", "=h", "", "", "=a", },
    { "=d", "d", "d", "=d", "=l", "=h", "=a", },
    { "=l", "d", "d", "=h", "", "", "=a", },
    { "=d", "d", "d", "=d", "=l", "=h", "=a", },
    { "=l", "d", "d", "=h", "", "", "=a", },
    { "=d", "d", "d", "=d", "=l", "=h", "=a", },
    { "=l", "d", "d", "=h", "", "", "=a", },
    { "d", "dJ", "", },
    { "d", "dJ", "", },
    { "d", "dJ", "", },
    { "=l", "d", "d", "=h", "=a", },
    { "=l", "d", "di", "=h", "=a", },
    { "=l", "d", "d", "=h", "=a", },
    { "=l", "d", "di", "=h", "=a", },
    { "=h", "d", "d", "=l", "=a", },
    { "=h", "d", "di", "=l", "=a", },
    { "=h", "d", "d", "=l", "=a", },
    { "=h", "d", "di", "=l", "=a", },
    { "=l", "d", "d", "=h", "=a", },
    { "=l", "d", "di", "=h", "=a", },
    { "=l", "d", "di", "=h", "=a", },
    { "=l", "d", "di", "=h", "=a", },
    { "=h", "d", "d", "=l", "=a", },
    { "=h", "d", "di", "=l", "=a", },
    { "=h", "d", "di", "=l", "=a", },
    { "=h", "d", "di", "=l", "=a", },
    { "=f", "f", },
    { "=f", "f", },
    { "=f", "", "f", },
    { "=f", "", "f", },
    { "=d", "d", },
    { "=d", "d", },
    { "=f", "f", },
    { "=f", "f", },
    { "=&d", "d", "=&d", "=&d", },
    { "=&d", "d", "=&d", "=&d", },
    { "=d", "d", },
    { "=d", "d", },
    { "=d", "d", "=d", },
    { "=d", "d", },
    { "=f", "f", },
    { "=f", "f", },
    { "=d", "d", },
    { "=d", "d", },
    { "", "", },
    { "=d,d", "%d,d", "d,K", },
    { "=d,d", "%d,d", "d,K", },
    { "=d", "%0", "d", },
    { "=d", "d", "d", },
    { "=d", "d", "d", },
    { "=d", "0", "d", },
    { "", "", "", },
    { "=d,d", "%d,d", "d,K", },
    { "=d,d", "%d,d", "d,K", },
    { "=d,d", "%d,d", "d,K", },
    { "=d", "%0", "d", },
    { "=d", "d", "d", },
    { "=d", "d", "d", },
    { "=d", "0", "d", },
    { "", "", "", },
    { "=d,d", "%d,d", "d,K", },
    { "=d,d", "%d,d", "d,K", },
    { "=d,t,t", "%0,d,d", "d,K,d", },
    { "=d", "d", "d", },
    { "=d", "d", "d", },
    { "=d", "0", "d", },
    { "=d,t,t", "%0,d,d", "d,K,d", },
    { "", "", "", },
    { "=d", "d", "K", },
    { "=d", "d", "d", },
    { "=d", "d", "d", },
    { "", "", "", },
    { "=f", "f", },
    { "=d", "d", },
    { "=d", "d", },
    { "=d", "d", },
    { "=d", "d", "I", },
    { "=d", "d", "I", },
    { "=d", "d", "I", },
    { "=d", "d", },
    { "=d", "d", },
    { "=d", "d", },
    { "", "", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d,d", "d,R,m", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d,d", "d,R,m", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d,d", "d,R,m", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d,d", "d,R,m", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d,d", "d,R,m", },
    { "=d,d", "R,m", },
    { "=d,d", "R,m", },
    { "=d,y,d,*d,d,d", "d,d,y,*x*w,R,m", },
    { "", "", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d", "R,m", },
    { "", "", },
    { "=d,d", "R,m", },
    { "=f", "f", },
    { "=d,*f,R,To", "f,*f,f,f", "=d,*d,&d,&d", "=f,?*X,f,f", },
    { "=d,*f,R,To", "f,*f,f,f", "=d,*d,&d,&d", "=f,?*X,f,f", },
    { "=d,*f,R,To", "f,*f,f,f", "=f,?*X,f,f", },
    { "=d,*f,R,To", "f,*f,f,f", "=f,?*X,f,f", },
    { "=f,f,f", "d,R,m", },
    { "=f,f,f", "d,R,m", },
    { "=f,f,f", "d,R,m", },
    { "=f,f,f", "d,R,m", },
    { "", "", },
    { "", "", },
    { "", "", },
    { "", "", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=&d,&d", "R,o", },
    { "=R,o", "dJ,dJ", },
    { "=&d,&d", "R,o", },
    { "=R,o", "dJ,dJ", },
    { "=r", "", },
    { "=r", "r", "", },
    { "", "", },
    { "R,m", },
    { "=d,d,d,d,R,o,*x*w,*d,*x*w", "d,iF,R,o,d,d,J,*x,*d", },
    { "=d,y,d,d,d,d,d,R,To,*d", "d,d,y,K,N,R,To,d,d,*x*w", },
    { "", "", },
    { "=d,d,d,d,d,d,R,m,*x*w,*d,*x*w,*a*q", "d,S,IKL,Mnis,R,m,dJ,dJ,J,*x*w,*d,*J", },
    { "=d,y,d,d,d,d,d,d,R,m,*d", "d,d,y,K,N,s,R,m,d,d,*x*w", },
    { "", "", },
    { "=b", "b", "=&d", },
    { "=b", "b", "=&d", },
    { "", "", },
    { "", "", },
    { "R,m", },
    { "=d,d,d,d,d,d,R,m,*d,*f*z,*f,*f,*f,*R,*m,*x*w,*x*w,*d,*d", "d,S,IKL,Mnis,R,m,dJ,dJ,*f*z,*d,*f,*R,*m,*f,*f,J,*d,*x*w,*a*q", },
    { "=d,d,d,d,d,d,R,m,*d,*z,*x*w,*d,*x*w,*d", "d,S,IKL,Mnis,R,m,dJ,dJ,*z,*d,J,*x*w,*d,*a*q", },
    { "=d,y,d,d,d,d,d,d,d,R,m,*d,*d", "d,d,y,S,K,N,s,R,m,d,d,*x,*a", },
    { "", "", },
    { "", "", },
    { "", "", },
    { "=b", "b", "=&d", },
    { "=b", "b", "=&d", },
    { "=d,*d,*d,*d,*R,*m,*d,*f,*f,*f,*f,*R,*m", "z,*d,*R,*m,*d,*d,*f,*d,*f,*R,*m,*f,*f", },
    { "=z", "z", "=&f", },
    { "=z", "z", "=&d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "=f", "d", "d", },
    { "", "", },
    { "=d,d,d,d,R,m,*d,*f,*f*z,*x,*d", "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x", },
    { "=d,d,d,d,R,m,*d,*z,*x,*d", "d,IK,R,m,dJ,dJ,*z,*d,*d,*x", },
    { "=d,y,d,d,d,d,d,R,m,*d", "d,d,y,K,N,R,m,d,d,*x", },
    { "", "", },
    { "", "", },
    { "=d,d,d,d,R,m,*d,*f*z,*f,*x,*d", "d,IK,R,m,dJ,dJ,*f*z,*d,*f,*d,*x", },
    { "=d,d,d,d,R,m,*d,*z,*x,*d", "d,IK,R,m,dJ,dJ,*z,*d,*d,*x", },
    { "=d,y,d,d,d,d,d,R,m,*d", "d,d,y,K,N,R,m,d,d,*x", },
    { "", "", },
    { "", "", },
    { "=f,f,f,f,R,m,*f,*d,*d,*d,*d,*R,*m", "f,G,R,Fm,fG,fG,*d,*f,*G*d,*R,*F*m,*d,*d", },
    { "=d,d,d,R,m", "      Gd,R,Fm,d,d", },
    { "=d,y,d,d,d,R,m", "d,d,y,R,Fm,d,d", },
    { "", "", },
    { "=f,f,f,R,To,f,*f,*d,*d,*d,*d,*R,*T", "f,R,To,fG,fG,F,*d,*f,*d*G,*R,*T*F,*d,*d", },
    { "=f,f,R,R,To,To,f,*d,*d,*d,*To,*R", " f,To,f,G,f,G,F,*F,*To,*R,*d,*d", },
    { "=d,d,d,R,To", "dG,R,ToF,d,d", },
    { "=d,y,d,d,d,R,To", "d,d,y,R,ToF,d,d", },
    { "", "", },
    { "", "", },
    { "", "", "", "", },
    { "=o", "o", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=d", "d", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=d", "o", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=o", "d", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "", "", "", "", "", "", "", "", },
    { "=o", "o", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=d", "d", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=Ro", "Ro", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=d", "d", "I", "I", "=&d", "=&d", "=&d", "=&d", },
    { "=d", "d", "dI", },
    { "=d", "d", "dI", },
    { "=d,d", "0,d", "d,I", },
    { "", "", "", },
    { "", "", "", },
    { "=&d", "d", "d", "=d", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "dI", },
    { "=d,d", "0,d", "d,I", },
    { "", "", "", },
    { "=d", "d", "dI", },
    { "=d", "d", "dI", },
    { "=d,d", "0,d", "d,I", },
    { "", "", "", },
    { "", "", "", },
    { "=&d", "d", "d", "=d", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "dI", },
    { "=d,d", "0,0", "d,I", },
    { "", "", "", },
    { "=d", "d", "dI", },
    { "=d", "d", "dI", },
    { "=d,d", "0,d", "d,I", },
    { "", "", "", },
    { "=d,d", "R,m", "I,I", },
    { "", "", "", },
    { "", "", "", },
    { "=&d", "d", "d", "=d", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "IJK", "=d", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=d", "d", "dI", },
    { "=d", "d", "dn", },
    { "=d", "d", "dn", },
    { "=d,d", "0,0", "d,I", },
    { "", "", "", },
    { "", "", },
    { "", },
    { "", "", },
    { "", },
    { "", "", },
    { "", "", },
    { "z", "", "", },
    { "z", "", "", },
    { "", "d", "", "", },
    { "", "d,t", "", "", },
    { "", "d", "", "", },
    { "", "d,t", "", "", },
    { "", "d", "d", "", "", },
    { "", "d", "d", "", "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "", },
    { "=d", },
    { "=d", "d", },
    { "=t", "d", },
    { "=d", "d", },
    { "=t", "d", },
    { "=d,d", "%d,d", "d,K", },
    { "", "", "", },
    { "=d,d", "%d,d", "d,K", },
    { "", "", "", },
    { "=d", },
    { "=d", "d", },
    { "=d", "d", },
    { "=d,d", "%d,d", "d,K", },
    { "", "", "", },
    { "=d,d", "%d,d", "d,K", },
    { "", "", "", },
    { "=d", },
    { "=d", "d", "dJ", },
    { "=t", "d", "d", },
    { "=d", "d", "dJ", },
    { "=d", "d", "d", },
    { "=d", },
    { "=d", "d", "dI", },
    { "", "", "", },
    { "=d", "d", "dI", },
    { "", "", "", },
    { "=d", },
    { "=d", "d", "dI", },
    { "=t,t", "d,d", "d,I", },
    { "=d", "d", "dI", },
    { "=t,t", "d,d", "d,I", },
    { "=d", },
    { "=d", "d", "I", },
    { "=t", "d", "I", },
    { "=d", "d", "I", },
    { "=t", "d", "I", },
    { "=d", "d", "d", },
    { "", "", "", },
    { "=d", "d", "d", },
    { "", "", "", },
    { "=d", },
    { "=d", "d", "dJ", },
    { "=t", "d", "d", },
    { "=d", "d", "dJ", },
    { "=t", "d", "d", },
    { "=d", },
    { "=d", "d", "dI", },
    { "", "", "", },
    { "=d", "d", "dI", },
    { "", "", "", },
    { "=d", },
    { "=d", "d", "dI", },
    { "=t,t", "d,d", "d,I", },
    { "=d", "d", "dI", },
    { "=t,t", "d,d", "d,I", },
    { "=d", },
    { "=d", "d", "I", },
    { "=t", "d", "I", },
    { "=d", "d", "I", },
    { "=t", "d", "I", },
    { "=d", "d", "d", },
    { "", "", "", },
    { "=d", "d", "d", },
    { "", "", "", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "=z", "f", "f", },
    { "", },
    { "", },
    { "d", },
    { "d", },
    { "d", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "", },
    { "d", "dI", "", "", "", "", "", },
    { "d", "", "d", },
    { "r", },
    { "r", },
    { "r", },
    { "r", },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { "", },
    { "=d", "", },
    { "m", "i", "", "", },
    { "", "", "", },
    { "ei", "i", "=y", },
    { "ri", "i", "=d", },
    { "ri", "i", "=d", },
    { "r", "i", "=d", },
    { "r", "i", "=d", },
    { "e", "i", "=y", },
    { "r", "i", "=d", },
    { "r", "i", "=d", },
    { "m", "i", "", "", },
    { "", "", },
    { "ri", "i", },
    { "=df", "m", "i", "", },
    { "", "", "", "", },
    { "=d", "ei", "i", "=y", },
    { "=df", "ri", "i", "=d", },
    { "=df", "ri", "i", "=d", },
    { "=df", "r", "i", "=d", },
    { "=df", "r", "i", "=d", },
    { "=df", "e", "i", "=y", },
    { "=df", "r", "i", "=d", },
    { "=df", "r", "i", "=d", },
    { "=df", "m", "i", "", },
    { "", "", "", },
    { "=df", "ri", "i", },
    { "", "", "", "", "", },
    { "=df", "ri", "i", "=df", "=d", },
    { "", "", "", },
    { 0 },
    { "=d,d", "d,d", "dJ,0", "0,dJ", "", },
    { "=d,d", "d,d", "dJ,0", "0,dJ", "", },
    { "=d,d", "dJ,0", "0,dJ", "", "z,z", },
    { "=d,d", "d,d", "dJ,0", "0,dJ", "", },
    { "=d,d", "d,d", "dJ,0", "0,dJ", "", },
    { "=d,d", "dJ,0", "0,dJ", "", "z,z", },
    { "=f,f", "d,d", "f,0", "0,f", "", },
    { "=f,f", "f,0", "0,f", "", "z,z", },
    { "=f,f", "d,d", "f,0", "0,f", "", },
    { "=f,f", "f,0", "0,f", "", "z,z", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "", "", "", "", },
    { "=g", },
    { "=g", },
    { "=g", },
    { "=g", },
    { "=g", },
    { "=g", },
    { 0 },
    { 0 },
    { 0 },
    { "=t", "d", "", "", "", },
    { "=t", "d", "", "", "", },
    { "=d", "t", "", "", "", },
    { "=d", "t", "", "", "", },
    { "=d", "p", },
    { "=d", "p", },
  };

const enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS] =
  {
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, },
    { SImode, SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { DImode, },
    { DImode, DImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, SImode, SImode, },
    { DImode, SImode, SImode, },
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, },
    { SImode, SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { DImode, },
    { DImode, DImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, SImode, SImode, },
    { DImode, SImode, SImode, },
    { DFmode, DFmode, DFmode, },
    { DFmode, DFmode, DFmode, },
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { SFmode, SFmode, SFmode, },
    { SFmode, SFmode, SFmode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, DImode, DImode, DImode, },
    { DImode, SImode, SImode, },
    { DImode, SImode, SImode, },
    { DImode, SImode, SImode, DImode, DImode, SImode, },
    { DImode, SImode, SImode, DImode, DImode, DImode, DImode, },
    { DImode, SImode, SImode, DImode, DImode, SImode, SImode, },
    { DImode, SImode, SImode, DImode, DImode, DImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, DImode, DImode, DImode, SImode, SImode, },
    { SImode, SImode, SImode, DImode, DImode, DImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, DImode, DImode, DImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { DImode, SImode, SImode, DImode, DImode, SImode, },
    { DImode, SImode, SImode, DImode, DImode, SImode, SImode, },
    { DFmode, DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, SFmode, },
    { DFmode, DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, SFmode, },
    { DFmode, DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, SFmode, },
    { DFmode, DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, SFmode, },
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, VOIDmode, VOIDmode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, DImode, VOIDmode, VOIDmode, DImode, },
    { SImode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, VOIDmode, VOIDmode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, DImode, VOIDmode, VOIDmode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, DImode, },
    { DImode, DImode, DImode, SImode, SImode, },
    { DFmode, DFmode, },
    { SFmode, SFmode, },
    { DFmode, DFmode, DFmode, },
    { SFmode, SFmode, SFmode, },
    { SImode, SImode, },
    { DImode, DImode, },
    { DFmode, DFmode, },
    { SFmode, SFmode, },
    { SImode, SImode, SImode, SImode, },
    { DImode, DImode, DImode, DImode, },
    { SImode, SImode, },
    { DImode, DImode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, },
    { DFmode, DFmode, },
    { SFmode, SFmode, },
    { SImode, SImode, },
    { DImode, DImode, },
    { VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SFmode, DFmode, },
    { SImode, DImode, },
    { HImode, DImode, },
    { QImode, DImode, },
    { SImode, DImode, DImode, },
    { SImode, DImode, DImode, },
    { SImode, DImode, DImode, },
    { SImode, DImode, },
    { SImode, DImode, },
    { HImode, DImode, },
    { DImode, SImode, },
    { DImode, SImode, },
    { SImode, HImode, },
    { SImode, HImode, },
    { SImode, HImode, },
    { DImode, HImode, },
    { DImode, HImode, },
    { DImode, HImode, },
    { HImode, QImode, },
    { HImode, QImode, },
    { HImode, QImode, },
    { SImode, QImode, },
    { SImode, QImode, },
    { SImode, QImode, },
    { DImode, QImode, },
    { DImode, QImode, },
    { DImode, HImode, },
    { DImode, QImode, },
    { DImode, SImode, },
    { DImode, HImode, },
    { DImode, HImode, },
    { SImode, HImode, },
    { SImode, HImode, },
    { HImode, QImode, },
    { HImode, QImode, },
    { SImode, QImode, },
    { SImode, QImode, },
    { DImode, QImode, },
    { DImode, QImode, },
    { DFmode, SFmode, },
    { SImode, DFmode, SImode, DFmode, },
    { SImode, SFmode, SImode, SFmode, },
    { DImode, DFmode, DFmode, },
    { DImode, SFmode, DFmode, },
    { DFmode, SImode, },
    { DFmode, DImode, },
    { SFmode, SImode, },
    { SFmode, DImode, },
    { SImode, DFmode, },
    { DImode, DFmode, },
    { SImode, SFmode, },
    { DImode, SFmode, },
    { VOIDmode, QImode, VOIDmode, VOIDmode, },
    { VOIDmode, QImode, VOIDmode, VOIDmode, },
    { QImode, VOIDmode, VOIDmode, VOIDmode, },
    { SImode, BLKmode, },
    { BLKmode, SImode, },
    { DImode, BLKmode, },
    { BLKmode, DImode, },
    { SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, },
    { DImode, },
    { DImode, DImode, },
    { DImode, DImode, },
    { VOIDmode, VOIDmode, },
    { DImode, DImode, },
    { DImode, DImode, },
    { VOIDmode, VOIDmode, },
    { DImode, DImode, TImode, },
    { DImode, DImode, TImode, },
    { VOIDmode, VOIDmode, },
    { SImode, SImode, },
    { SImode, },
    { SImode, SImode, },
    { SImode, SImode, },
    { SImode, SImode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { CCmode, CCmode, },
    { CCmode, CCmode, TFmode, },
    { CCmode, CCmode, CCmode, },
    { SFmode, SImode, SImode, },
    { SFmode, DImode, DImode, },
    { DFmode, SImode, SImode, },
    { DFmode, DImode, DImode, },
    { SFmode, SImode, SImode, },
    { SFmode, DImode, DImode, },
    { DFmode, SImode, SImode, },
    { DFmode, DImode, DImode, },
    { HImode, HImode, },
    { HImode, HImode, },
    { HImode, HImode, },
    { HImode, HImode, },
    { VOIDmode, VOIDmode, },
    { QImode, QImode, },
    { QImode, QImode, },
    { QImode, QImode, },
    { QImode, QImode, },
    { VOIDmode, VOIDmode, },
    { SFmode, SFmode, },
    { SFmode, SFmode, },
    { SFmode, SFmode, },
    { SFmode, SFmode, },
    { DFmode, DFmode, },
    { DFmode, DFmode, },
    { DFmode, DFmode, },
    { DFmode, DFmode, },
    { DFmode, DFmode, },
    { VOIDmode, VOIDmode, },
    { DImode, DImode, },
    { BLKmode, BLKmode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { BLKmode, BLKmode, SImode, SImode, SImode, SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, },
    { SImode, },
    { DImode, DImode, },
    { DImode, },
    { DFmode, DFmode, },
    { SFmode, SFmode, },
    { CCmode, VOIDmode, VOIDmode, },
    { CCmode, VOIDmode, VOIDmode, },
    { SImode, SImode, VOIDmode, VOIDmode, },
    { SImode, SImode, VOIDmode, VOIDmode, },
    { DImode, DImode, VOIDmode, VOIDmode, },
    { DImode, DImode, VOIDmode, VOIDmode, },
    { SImode, SImode, SImode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, VOIDmode, VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { SImode, },
    { SImode, SImode, },
    { SImode, SImode, },
    { DImode, DImode, },
    { DImode, DImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, },
    { SImode, SImode, },
    { DImode, DImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, },
    { SImode, SImode, SImode, },
    { SImode, SImode, SImode, },
    { DImode, DImode, DImode, },
    { DImode, DImode, DImode, },
    { SImode, SImode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { DImode, DImode, DImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { CCmode, DFmode, DFmode, },
    { CCmode, DFmode, DFmode, },
    { CCmode, DFmode, DFmode, },
    { CCmode, DFmode, DFmode, },
    { CCmode, DFmode, DFmode, },
    { CCmode, SFmode, SFmode, },
    { CCmode, SFmode, SFmode, },
    { CCmode, SFmode, SFmode, },
    { CCmode, SFmode, SFmode, },
    { CCmode, SFmode, SFmode, },
    { VOIDmode, },
    { VOIDmode, },
    { VOIDmode, },
    { SImode, },
    { DImode, },
    { VOIDmode, VOIDmode, },
    { SImode, VOIDmode, },
    { DImode, VOIDmode, },
    { SImode, SImode, },
    { HImode, SImode, },
    { HImode, SImode, },
    { SImode, SImode, },
    { DImode, SImode, },
    { DImode, SImode, },
    { SImode, SImode, SImode, VOIDmode, VOIDmode, VOIDmode, SImode, },
    { SImode, VOIDmode, SImode, },
    { VOIDmode, },
    { SImode, },
    { DImode, },
    { VOIDmode, },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, SImode, },
    { SImode, VOIDmode, SImode, },
    { DImode, VOIDmode, SImode, },
    { SImode, VOIDmode, SImode, },
    { SImode, VOIDmode, SImode, },
    { DImode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, SImode, VOIDmode, SImode, },
    { VOIDmode, DImode, VOIDmode, SImode, },
    { VOIDmode, SImode, VOIDmode, SImode, },
    { VOIDmode, SImode, VOIDmode, SImode, },
    { VOIDmode, DImode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, SImode, },
    { VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode },
    { SImode, SImode, SImode, SImode, VOIDmode, },
    { SImode, DImode, SImode, SImode, VOIDmode, },
    { SImode, SImode, SImode, VOIDmode, CCmode, },
    { DImode, SImode, DImode, DImode, VOIDmode, },
    { DImode, DImode, DImode, DImode, VOIDmode, },
    { DImode, DImode, DImode, VOIDmode, CCmode, },
    { SFmode, SImode, SFmode, SFmode, VOIDmode, },
    { SFmode, SFmode, SFmode, VOIDmode, CCmode, },
    { DFmode, SImode, DFmode, DFmode, VOIDmode, },
    { DFmode, DFmode, DFmode, VOIDmode, CCmode, },
    { SImode, VOIDmode, SImode, SImode, },
    { DImode, VOIDmode, DImode, DImode, },
    { SFmode, VOIDmode, SFmode, SFmode, },
    { DFmode, VOIDmode, DFmode, DFmode, },
    { QImode, },
    { HImode, },
    { SImode, },
    { DImode, },
    { SFmode, },
    { DFmode, },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { VOIDmode, VOIDmode, VOIDmode, VOIDmode, VOIDmode, },
    { SImode, SImode, },
    { DImode, DImode, },
  };

const char insn_operand_strict_low[][MAX_RECOG_OPERANDS] =
  {
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, },
    { 0 },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0 },
    { 0 },
    { 0 },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, },
    { 0, 0, },
  };

extern int register_operand ();
extern int reg_or_0_operand ();
extern int arith_operand ();
extern int small_int ();
extern int se_register_operand ();
extern int se_arith_operand ();
extern int se_reg_or_0_operand ();
extern int scratch_operand ();
extern int extend_operator ();
extern int highpart_shift_operator ();
extern int const_float_1_operand ();
extern int true_reg_or_0_operand ();
extern int immediate_operand ();
extern int nonmemory_operand ();
extern int se_nonmemory_operand ();
extern int uns_arith_operand ();
extern int se_uns_arith_operand ();
extern int nonimmediate_operand ();
extern int memory_operand ();
extern int general_operand ();
extern int se_nonimmediate_operand ();
extern int movdi_operand ();
extern int move_operand ();
extern int address_operand ();
extern int arith32_operand ();
extern int pc_or_label_operand ();
extern int cmp_op ();
extern int equality_op ();
extern int call_insn_operand ();
extern int comparison_operator ();
extern int consttable_operand ();

int (*const insn_operand_predicate[][MAX_RECOG_OPERANDS])() =
  {
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { small_int, },
    { register_operand, small_int, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_arith_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, se_reg_or_0_operand, se_arith_operand, },
    { small_int, },
    { register_operand, small_int, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, },
    { 0, 0, 0, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { small_int, },
    { register_operand, small_int, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, se_reg_or_0_operand, se_arith_operand, },
    { small_int, },
    { register_operand, small_int, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, },
    { 0, 0, 0, },
    { register_operand, reg_or_0_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { register_operand, se_register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, highpart_shift_operator, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, highpart_shift_operator, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, highpart_shift_operator, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, },
    { register_operand, register_operand, register_operand, extend_operator, extend_operator, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, const_float_1_operand, register_operand, },
    { register_operand, const_float_1_operand, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, 0, 0, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, register_operand, 0, 0, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, register_operand, 0, 0, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, register_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, register_operand, 0, 0, scratch_operand, },
    { register_operand, true_reg_or_0_operand, immediate_operand, },
    { register_operand, true_reg_or_0_operand, immediate_operand, },
    { register_operand, true_reg_or_0_operand, immediate_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, se_nonmemory_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, const_float_1_operand, register_operand, },
    { register_operand, const_float_1_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, scratch_operand, scratch_operand, },
    { register_operand, se_register_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { 0, 0, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_uns_arith_operand, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { 0, 0, 0, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, uns_arith_operand, uns_arith_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, se_register_operand, se_uns_arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_uns_arith_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { 0, 0, 0, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, memory_operand, },
    { register_operand, register_operand, },
    { general_operand, register_operand, scratch_operand, scratch_operand, },
    { general_operand, register_operand, scratch_operand, scratch_operand, },
    { general_operand, register_operand, scratch_operand, },
    { general_operand, register_operand, scratch_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, se_nonimmediate_operand, },
    { register_operand, nonimmediate_operand, },
    { register_operand, se_nonimmediate_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, memory_operand, immediate_operand, immediate_operand, },
    { register_operand, memory_operand, immediate_operand, immediate_operand, },
    { memory_operand, immediate_operand, immediate_operand, register_operand, },
    { register_operand, general_operand, },
    { memory_operand, reg_or_0_operand, },
    { register_operand, general_operand, },
    { memory_operand, reg_or_0_operand, },
    { register_operand, immediate_operand, },
    { register_operand, register_operand, immediate_operand, },
    { nonimmediate_operand, general_operand, },
    { memory_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { 0, 0, },
    { nonimmediate_operand, movdi_operand, },
    { nonimmediate_operand, movdi_operand, },
    { 0, 0, },
    { register_operand, 0, register_operand, },
    { 0, se_register_operand, register_operand, },
    { 0, 0, },
    { nonimmediate_operand, general_operand, },
    { memory_operand, },
    { nonimmediate_operand, move_operand, },
    { nonimmediate_operand, move_operand, },
    { nonimmediate_operand, move_operand, },
    { 0, 0, },
    { 0, 0, },
    { 0, 0, },
    { general_operand, register_operand, register_operand, },
    { register_operand, 0, register_operand, },
    { nonimmediate_operand, general_operand, },
    { register_operand, general_operand, register_operand, },
    { general_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { 0, 0, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { 0, 0, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { nonimmediate_operand, general_operand, },
    { 0, 0, },
    { address_operand, register_operand, },
    { general_operand, general_operand, arith32_operand, immediate_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { 0, 0, 0, 0, 0, 0, 0, 0, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { memory_operand, memory_operand, small_int, small_int, scratch_operand, scratch_operand, scratch_operand, scratch_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, se_register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, se_register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, memory_operand, immediate_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, register_operand, register_operand, register_operand, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, register_operand, small_int, register_operand, },
    { 0, 0, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, se_register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, se_register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, arith_operand, },
    { register_operand, },
    { se_register_operand, se_arith_operand, },
    { se_register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, pc_or_label_operand, pc_or_label_operand, },
    { register_operand, pc_or_label_operand, pc_or_label_operand, },
    { cmp_op, register_operand, pc_or_label_operand, pc_or_label_operand, },
    { equality_op, register_operand, pc_or_label_operand, pc_or_label_operand, },
    { cmp_op, se_register_operand, pc_or_label_operand, pc_or_label_operand, },
    { equality_op, se_register_operand, pc_or_label_operand, pc_or_label_operand, },
    { equality_op, register_operand, register_operand, pc_or_label_operand, pc_or_label_operand, },
    { equality_op, se_register_operand, se_register_operand, pc_or_label_operand, pc_or_label_operand, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { 0, },
    { register_operand, },
    { register_operand, register_operand, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, register_operand, uns_arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_uns_arith_operand, },
    { 0, 0, 0, },
    { register_operand, },
    { register_operand, register_operand, },
    { register_operand, se_register_operand, },
    { register_operand, register_operand, uns_arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_uns_arith_operand, },
    { 0, 0, 0, },
    { register_operand, },
    { register_operand, register_operand, reg_or_0_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_reg_or_0_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_arith_operand, },
    { 0, 0, 0, },
    { register_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, se_register_operand, se_arith_operand, },
    { register_operand, se_register_operand, se_arith_operand, },
    { register_operand, },
    { register_operand, register_operand, small_int, },
    { register_operand, register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, register_operand, register_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_register_operand, },
    { 0, 0, 0, },
    { register_operand, },
    { register_operand, register_operand, reg_or_0_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, se_register_operand, se_reg_or_0_operand, },
    { register_operand, se_register_operand, se_register_operand, },
    { register_operand, },
    { register_operand, register_operand, arith_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_arith_operand, },
    { 0, 0, 0, },
    { register_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, register_operand, arith_operand, },
    { register_operand, se_register_operand, se_arith_operand, },
    { register_operand, se_register_operand, se_arith_operand, },
    { register_operand, },
    { register_operand, register_operand, small_int, },
    { register_operand, register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, se_register_operand, small_int, },
    { register_operand, register_operand, register_operand, },
    { 0, 0, 0, },
    { register_operand, se_register_operand, se_register_operand, },
    { 0, 0, 0, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { register_operand, register_operand, register_operand, },
    { 0, },
    { 0, },
    { register_operand, },
    { register_operand, },
    { se_register_operand, },
    { register_operand, 0, },
    { register_operand, 0, },
    { se_register_operand, 0, },
    { register_operand, 0, },
    { register_operand, 0, },
    { register_operand, 0, },
    { register_operand, 0, },
    { se_register_operand, 0, },
    { se_register_operand, 0, },
    { register_operand, arith_operand, arith_operand, 0, 0, 0, scratch_operand, },
    { register_operand, 0, register_operand, },
    { register_operand, },
    { register_operand, },
    { register_operand, },
    { register_operand, },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { 0 },
    { register_operand, },
    { register_operand, 0, },
    { memory_operand, 0, 0, 0, },
    { 0, 0, 0, },
    { call_insn_operand, 0, register_operand, },
    { call_insn_operand, 0, register_operand, },
    { call_insn_operand, 0, register_operand, },
    { register_operand, 0, register_operand, },
    { se_register_operand, 0, register_operand, },
    { register_operand, 0, register_operand, },
    { register_operand, 0, register_operand, },
    { se_register_operand, 0, register_operand, },
    { memory_operand, 0, 0, 0, },
    { 0, 0, },
    { call_insn_operand, 0, },
    { register_operand, memory_operand, 0, 0, },
    { 0, 0, 0, 0, },
    { register_operand, call_insn_operand, 0, register_operand, },
    { register_operand, call_insn_operand, 0, register_operand, },
    { register_operand, call_insn_operand, 0, register_operand, },
    { register_operand, register_operand, 0, register_operand, },
    { register_operand, se_register_operand, 0, register_operand, },
    { register_operand, register_operand, 0, register_operand, },
    { register_operand, register_operand, 0, register_operand, },
    { register_operand, se_register_operand, 0, register_operand, },
    { register_operand, memory_operand, 0, 0, },
    { 0, 0, 0, },
    { register_operand, call_insn_operand, 0, },
    { 0, 0, 0, 0, 0, },
    { register_operand, call_insn_operand, 0, register_operand, register_operand, },
    { 0, 0, 0, },
    { 0 },
    { register_operand, register_operand, reg_or_0_operand, reg_or_0_operand, equality_op, },
    { register_operand, se_register_operand, reg_or_0_operand, reg_or_0_operand, equality_op, },
    { register_operand, reg_or_0_operand, reg_or_0_operand, equality_op, register_operand, },
    { register_operand, register_operand, se_reg_or_0_operand, se_reg_or_0_operand, equality_op, },
    { register_operand, se_register_operand, se_reg_or_0_operand, se_reg_or_0_operand, equality_op, },
    { register_operand, se_reg_or_0_operand, se_reg_or_0_operand, equality_op, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, equality_op, },
    { register_operand, register_operand, register_operand, equality_op, register_operand, },
    { register_operand, register_operand, register_operand, register_operand, equality_op, },
    { register_operand, register_operand, register_operand, equality_op, register_operand, },
    { register_operand, comparison_operator, reg_or_0_operand, reg_or_0_operand, },
    { register_operand, comparison_operator, se_reg_or_0_operand, se_reg_or_0_operand, },
    { register_operand, comparison_operator, register_operand, register_operand, },
    { register_operand, comparison_operator, register_operand, register_operand, },
    { consttable_operand, },
    { consttable_operand, },
    { consttable_operand, },
    { consttable_operand, },
    { consttable_operand, },
    { consttable_operand, },
    { 0 },
    { 0 },
    { 0 },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { 0, 0, 0, 0, 0, },
    { register_operand, address_operand, },
    { register_operand, address_operand, },
  };

const int insn_n_alternatives[] =
  {
    1,
    1,
    1,
    1,
    1,
    1,
    3,
    0,
    0,
    0,
    2,
    0,
    0,
    3,
    0,
    0,
    1,
    1,
    1,
    3,
    0,
    0,
    1,
    3,
    1,
    1,
    1,
    1,
    1,
    1,
    3,
    0,
    0,
    1,
    1,
    0,
    0,
    3,
    0,
    0,
    1,
    1,
    1,
    3,
    0,
    0,
    1,
    3,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    1,
    1,
    3,
    0,
    2,
    3,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    2,
    2,
    1,
    1,
    1,
    1,
    0,
    2,
    2,
    2,
    1,
    1,
    1,
    1,
    0,
    2,
    2,
    3,
    1,
    1,
    1,
    3,
    0,
    1,
    1,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    2,
    0,
    3,
    2,
    0,
    3,
    2,
    0,
    3,
    2,
    0,
    3,
    2,
    0,
    3,
    2,
    2,
    6,
    0,
    2,
    0,
    2,
    0,
    2,
    0,
    2,
    0,
    2,
    1,
    4,
    4,
    4,
    4,
    3,
    3,
    3,
    3,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    2,
    2,
    2,
    1,
    1,
    0,
    2,
    9,
    10,
    0,
    12,
    11,
    0,
    1,
    1,
    0,
    0,
    2,
    19,
    14,
    13,
    0,
    0,
    0,
    1,
    1,
    13,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    11,
    10,
    10,
    0,
    0,
    11,
    10,
    10,
    0,
    0,
    13,
    5,
    7,
    0,
    13,
    12,
    5,
    7,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    0,
    0,
    1,
    1,
    0,
    0,
    1,
    0,
    0,
    1,
    2,
    0,
    1,
    1,
    2,
    0,
    0,
    1,
    1,
    0,
    0,
    1,
    0,
    0,
    1,
    2,
    0,
    1,
    1,
    2,
    0,
    2,
    0,
    0,
    1,
    1,
    0,
    0,
    1,
    0,
    0,
    1,
    1,
    1,
    2,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    2,
    1,
    2,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    1,
    2,
    0,
    2,
    0,
    1,
    1,
    1,
    2,
    0,
    2,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    0,
    1,
    1,
    2,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    0,
    1,
    1,
    2,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    1,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    1,
    0,
    1,
    0,
    0,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    2,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    1,
    1,
  };
