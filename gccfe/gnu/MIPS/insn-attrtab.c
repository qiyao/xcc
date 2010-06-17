/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Generated automatically by the program `genattrtab'
from the machine description file `md'.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "regs.h"
#include "real.h"
#include "output.h"
#include "insn-attr.h"
#include "toplev.h"

#define operands recog_operand

int
insn_current_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

int
insn_variable_length_p (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

int
insn_default_length (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 278:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 4;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 277:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 276:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return 1;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))
        {
	  return 2;
        }
      else if (which_alternative == 9)
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 275:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 4;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return 4;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 2;
        }
      else if (which_alternative == 10)
        {
	  return 4;
        }
      else if (which_alternative == 11)
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 273:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 1;
        }
      else if (which_alternative == 4)
        {
	  return 2;
        }
      else if (which_alternative == 5)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 272:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return 1;
        }
      else if (which_alternative == 2)
        {
	  return 2;
        }
      else if (which_alternative == 3)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else if (which_alternative == 11)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 268:
    case 263:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 4)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 2;
	    }
	  else
	    {
	      return 3;
	    }
        }
      else if (which_alternative == 5)
        {
	  return 1;
        }
      else if (which_alternative == 6)
        {
	  return 2;
        }
      else if (which_alternative == 7)
        {
	  return 1;
        }
      else if (which_alternative == 8)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 267:
    case 262:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else if (which_alternative == 11)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 5)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 2;
	    }
	  else
	    {
	      return 3;
	    }
        }
      else if (which_alternative == 6)
        {
	  if (m16_usym8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 7)
        {
	  return 1;
        }
      else if (which_alternative == 8)
        {
	  return 2;
        }
      else if (which_alternative == 9)
        {
	  return 1;
        }
      else if (which_alternative == 10)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else if ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || (which_alternative == 12)))))
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else if ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || (which_alternative == 11))))
        {
	  return 1;
        }
      else if (which_alternative == 12)
        {
	  return 2;
        }
      else if (which_alternative == 13)
        {
	  return 1;
        }
      else if (which_alternative == 14)
        {
	  return 2;
        }
      else if ((which_alternative == 15) || ((which_alternative == 16) || (which_alternative == 17)))
        {
	  return 1;
        }
      else
        {
	  return 1;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  if (m16_uimm8_1 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 4)
        {
	  if (m16_nuimm8_1 (operands[1], VOIDmode))
	    {
	      return 2;
	    }
	  else
	    {
	      return 3;
	    }
        }
      else if (which_alternative == 5)
        {
	  if (m16_usym5_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else if (which_alternative == 8)
        {
	  return 1;
        }
      else if (which_alternative == 9)
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else if (which_alternative == 2)
        {
	  return 1;
        }
      else if (which_alternative == 3)
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 1;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 1;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else if ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 2;
        }
      else if (which_alternative == 4)
        {
	  return 3;
        }
      else if (which_alternative == 5)
        {
	  return 2;
        }
      else if (which_alternative == 6)
        {
	  return 4;
        }
      else if (which_alternative == 7)
        {
	  return 2;
        }
      else if (which_alternative == 8)
        {
	  return 4;
        }
      else
        {
	  return 2;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else if (which_alternative == 1)
        {
	  return 4;
        }
      else if (which_alternative == 2)
        {
	  return 2;
        }
      else if (which_alternative == 3)
        {
	  return 4;
        }
      else if (which_alternative == 4)
        {
	  return 2;
        }
      else if (which_alternative == 5)
        {
	  return 4;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return 2;
        }
      else
        {
	  return 2;
        }

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative != 5)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative != 2)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 523:
    case 522:
      return 10 /* 0xa */;

    case 521:
    case 520:
    case 519:
    case 518:
    case 516:
    case 515:
    case 513:
    case 511:
    case 510:
    case 509:
    case 492:
    case 487:
    case 486:
    case 482:
    case 480:
    case 474:
    case 473:
    case 469:
    case 467:
    case 449:
    case 439:
    case 426:
    case 424:
    case 412:
    case 410:
    case 402:
    case 400:
    case 388:
    case 386:
    case 378:
    case 376:
    case 371:
    case 369:
    case 351:
    case 349:
    case 329:
    case 312:
    case 297:
    case 173:
    case 172:
    case 171:
    case 160:
    case 53:
    case 50:
      return 2;

    case 517:
    case 280:
    case 126:
    case 125:
    case 104:
    case 103:
    case 57:
      return 3;

    case 514:
    case 512:
    case 464:
    case 332:
    case 315:
    case 300:
    case 133:
    case 34:
    case 10:
      return 4;

    case 459:
      return 0;

    case 453:
    case 130:
    case 129:
      return 6;

    case 423:
    case 421:
    case 399:
    case 397:
      extract_insn (insn);
      if (m16_uimm8_m1_1 (operands[2], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 418:
    case 416:
    case 394:
    case 392:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  if (m16_uimm8_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }

    case 338:
    case 323:
    case 319:
    case 308:
    case 304:
    case 293:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }

    case 328:
    case 311:
    case 296:
      return 12 /* 0xc */;

    case 325:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 2;
	    }
	  else
	    {
	      return 3;
	    }
        }
      else
        {
	  if (m16_uimm3_b (operands[2], VOIDmode))
	    {
	      return 3;
	    }
	  else
	    {
	      return 4;
	    }
        }

    case 288:
    case 287:
    case 285:
    case 284:
    case 283:
    case 282:
      return 20 /* 0x14 */;

    case 240:
    case 229:
    case 205:
    case 203:
    case 201:
    case 199:
    case 197:
    case 194:
    case 193:
    case 190:
    case 187:
    case 184:
    case 181:
    case 178:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 225:
    case 224:
    case 223:
    case 222:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 214:
    case 213:
    case 212:
    case 211:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 3;
        }
      else if (which_alternative == 1)
        {
	  return 4;
        }
      else
        {
	  return 3;
        }

    case 210:
    case 209:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 2;
        }
      else if (which_alternative == 1)
        {
	  return 1;
        }
      else if (which_alternative == 2)
        {
	  return 2;
        }
      else
        {
	  return 3;
        }

    case 208:
    case 207:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 11 /* 0xb */;
        }
      else if (which_alternative == 1)
        {
	  return 9;
        }
      else if (which_alternative == 2)
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 11 /* 0xb */;
        }

    case 170:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case 169:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  return 1;
        }
      else
        {
	  return 4;
        }

    case 168:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 165:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 161:
    case 157:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 1;
        }
      else if (which_alternative == 1)
        {
	  if (m16_uimm8_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else
        {
	  return 1;
        }

    case 159:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 153:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips_isa) >= (3))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 152:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 145:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips_isa) >= (3))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 144:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((TARGET_64BIT) != (0))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 138:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips_isa) >= (3))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 65:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((GENERATE_MULT3) != (0))
        {
	  return 1;
        }
      else
        {
	  return 3;
        }

    case 47:
    case 30:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  if (m16_nsimm8_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_nsimm4_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else
        {
	  return 1;
        }

    case 43:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  if (m16_nsimm5_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_nsimm4_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else
        {
	  return 1;
        }

    case 42:
      extract_insn (insn);
      if (m16_nuimm5_4 (operands[0], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 41:
    case 28:
      extract_insn (insn);
      if (m16_nsimm8_8 (operands[0], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 37:
    case 13:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return 3;
        }
      else if (which_alternative == 1)
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 29:
      extract_insn (insn);
      if (m16_nuimm8_4 (operands[1], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 23:
    case 6:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  if (m16_simm8_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_simm4_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else
        {
	  return 1;
        }

    case 19:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  if (m16_simm5_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else if (which_alternative == 1)
        {
	  if (m16_simm4_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 2;
	    }
        }
      else
        {
	  return 1;
        }

    case 18:
      extract_insn (insn);
      if (m16_uimm5_4 (operands[0], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 17:
    case 4:
      extract_insn (insn);
      if (m16_simm8_8 (operands[0], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case 5:
      extract_insn (insn);
      if (m16_uimm8_4 (operands[1], VOIDmode))
        {
	  return 1;
        }
      else
        {
	  return 2;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

int
result_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R6000))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 276:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 275:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 9) || (which_alternative == 10))))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 9) || (which_alternative == 10))))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 278:
    case 273:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 277:
    case 272:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || (which_alternative == 2)) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || (which_alternative == 2)) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 267:
    case 266:
    case 262:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 2) || (which_alternative == 3)) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 2) || (which_alternative == 3)) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 3) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8)))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 3) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8)))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((which_alternative == 8) || (which_alternative == 9)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 11) || (which_alternative == 12))))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative == 1) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 11) || (which_alternative == 12))))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((which_alternative == 8) || (which_alternative == 9)))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 5) || (which_alternative == 6))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 5) || (which_alternative == 6))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 2) || (which_alternative == 3)) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if (((which_alternative == 2) || (which_alternative == 3)) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && (which_alternative != 3))) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && (which_alternative != 3))) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 3;
        }
      else if ((which_alternative != 0) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 325:
    case 255:
    case 254:
    case 253:
    case 252:
    case 224:
    case 222:
    case 205:
    case 203:
    case 201:
    case 199:
    case 197:
    case 194:
    case 193:
    case 190:
    case 187:
    case 184:
    case 181:
    case 178:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 209:
    case 208:
    case 207:
    case 206:
    case 167:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 6;
        }
      else
        {
	  return 1;
        }

    case 136:
    case 135:
    case 128:
    case 127:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case 124:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 61 /* 0x3d */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))
        {
	  return 54 /* 0x36 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 31 /* 0x1f */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else
        {
	  return 1;
        }

    case 123:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 121 /* 0x79 */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000)))))))
        {
	  return 112 /* 0x70 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 60 /* 0x3c */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 1;
        }

    case 122:
      if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))
        {
	  return 54 /* 0x36 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))))
        {
	  return 31 /* 0x1f */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else
        {
	  return 1;
        }

    case 121:
      if ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))
        {
	  return 112 /* 0x70 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 60 /* 0x3c */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 59 /* 0x3b */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 1;
        }

    case 120:
    case 116:
    case 112:
    case 108:
      if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 69 /* 0x45 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 68 /* 0x44 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  return 67 /* 0x43 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 66 /* 0x42 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R4650))))))
        {
	  return 38 /* 0x26 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 35 /* 0x23 */;
        }

    case 118:
    case 114:
    case 110:
    case 106:
    case 101:
    case 99:
    case 97:
    case 95:
      if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  return 69 /* 0x45 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  return 42 /* 0x2a */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))
        {
	  return 38 /* 0x26 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 37 /* 0x25 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 36 /* 0x24 */;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || (((mips_cpu_attr) == (CPU_R4100))))
        {
	  return 35 /* 0x23 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 34 /* 0x22 */;
        }
      else
        {
	  return 1;
        }

    case 93:
    case 91:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 32 /* 0x20 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 31 /* 0x1f */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && (! (((mips_cpu_attr) == (CPU_R5000))))))))
        {
	  return 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else
        {
	  return 1;
        }

    case 92:
    case 90:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 61 /* 0x3d */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 59 /* 0x3b */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 16 /* 0x10 */;
        }
      else
        {
	  return 1;
        }

    case 89:
    case 87:
    case 85:
    case 83:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 9;
        }
      else
        {
	  return 1;
        }

    case 88:
    case 86:
    case 84:
    case 82:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 1;
        }

    case 78:
    case 77:
    case 65:
    case 64:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 9;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 8;
        }
      else if (((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4100)))) || (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 2) && ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))
        {
	  return 12 /* 0xc */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600)))))
        {
	  return 10 /* 0xa */;
        }
      else if ((which_alternative != 2) && ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 5;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 4;
        }
      else if ((which_alternative != 2) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 81:
    case 80:
    case 79:
    case 76:
    case 75:
    case 74:
    case 71:
    case 70:
    case 69:
    case 68:
    case 60:
    case 57:
    case 56:
    case 55:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  return 17 /* 0x11 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 12 /* 0xc */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 10 /* 0xa */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 3;
        }
      else
        {
	  return 1;
        }

    case 53:
    case 52:
      if ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))
        {
	  return 8;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))
        {
	  return 7;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) || ((((mips_cpu_attr) == (CPU_R6000))) || (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 4;
        }
      else
        {
	  return 1;
        }

    case 50:
    case 49:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))
        {
	  return 8;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) || (((mips_cpu_attr) == (CPU_R6000))))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000)))))
        {
	  return 5;
        }
      else
        {
	  return 1;
        }

    case 25:
    case 24:
    case 1:
    case 0:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R6000)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))))
        {
	  return 4;
        }
      else if ((((mips_cpu_attr) == (CPU_R6000))) || (((mips_cpu_attr) == (CPU_R4300))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  return 2;
        }
      else
        {
	  return 1;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 1;

    }
}

int
alu_5400_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 275:
    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || (which_alternative == 8)) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) && (((mips_cpu_attr) == (CPU_R5400)))) || (((which_alternative == 0) || (which_alternative == 8)) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 8))) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))) && (((mips_cpu_attr) == (CPU_R5400)))) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 2) || (which_alternative == 3)) && (((mips_cpu_attr) == (CPU_R5400)))) || (((which_alternative == 0) || (which_alternative == 10)) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))) && (((mips_cpu_attr) == (CPU_R5400)))) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 242:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 2) || (which_alternative == 3)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 3) || (which_alternative == 4))) && (((mips_cpu_attr) == (CPU_R5400)))) || (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 267:
    case 262:
    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) && (((mips_cpu_attr) == (CPU_R5400)))) || ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 278:
    case 273:
    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 277:
    case 276:
    case 272:
    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) && (((mips_cpu_attr) == (CPU_R5400))))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 2;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 124:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 61 /* 0x3d */;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 122:
    case 93:
    case 91:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 31 /* 0x1f */;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 121:
    case 92:
    case 90:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 59 /* 0x3b */;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 89:
    case 87:
    case 85:
    case 83:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 9;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 88:
    case 86:
    case 84:
    case 82:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 10 /* 0xa */;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 53:
    case 52:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 5;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 214:
    case 213:
    case 212:
    case 211:
    case 210:
    case 209:
    case 208:
    case 207:
    case 206:
    case 167:
    case 50:
    case 49:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 6;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case 25:
    case 24:
    case 1:
    case 0:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 4;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 521:
    case 520:
    case 519:
    case 518:
    case 517:
    case 516:
    case 515:
    case 514:
    case 513:
    case 512:
    case 511:
    case 510:
    case 509:
    case 492:
    case 490:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 477:
    case 474:
    case 473:
    case 472:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 464:
    case 463:
    case 462:
    case 459:
    case 453:
    case 451:
    case 449:
    case 445:
    case 444:
    case 442:
    case 441:
    case 439:
    case 438:
    case 353:
    case 352:
    case 351:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
    case 325:
    case 290:
    case 289:
    case 288:
    case 287:
    case 285:
    case 284:
    case 283:
    case 282:
    case 259:
    case 258:
    case 257:
    case 256:
    case 255:
    case 254:
    case 253:
    case 252:
    case 240:
    case 229:
    case 225:
    case 224:
    case 223:
    case 222:
    case 205:
    case 203:
    case 201:
    case 199:
    case 197:
    case 194:
    case 193:
    case 190:
    case 187:
    case 184:
    case 181:
    case 178:
    case 130:
    case 129:
    case 126:
    case 125:
    case 123:
    case 120:
    case 118:
    case 116:
    case 114:
    case 112:
    case 110:
    case 108:
    case 106:
    case 104:
    case 103:
    case 101:
    case 99:
    case 97:
    case 95:
    case 81:
    case 80:
    case 79:
    case 78:
    case 77:
    case 76:
    case 75:
    case 74:
    case 71:
    case 70:
    case 69:
    case 68:
    case 65:
    case 64:
    case 61:
    case 60:
    case 58:
    case 57:
    case 56:
    case 55:
      return 121 /* 0x79 */;

    default:
      if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  return 1;
        }
      else
        {
	  return 121 /* 0x79 */;
        }

    }
}

unsigned int
alu_5400_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65656 /* min 1, max 120 */;

    }
}

int
divide_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 124:
    case 122:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 31 /* 0x1f */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 54 /* 0x36 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 123:
    case 121:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 60 /* 0x3c */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 93:
    case 91:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 21 /* 0x15 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 32 /* 0x20 */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 15 /* 0xf */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 12 /* 0xc */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 23 /* 0x17 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case 92:
    case 90:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 36 /* 0x24 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  return 61 /* 0x3d */;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 16 /* 0x10 */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 19 /* 0x13 */;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 36 /* 0x24 */;
        }
      else
        {
	  return 112 /* 0x70 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 112 /* 0x70 */;

    }
}

int
mult_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 53:
    case 52:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 4;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 5;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 4;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && (! (((mips_cpu_attr) == (CPU_R4600)))))))
        {
	  return 7;
        }
      else
        {
	  return 8;
        }

    case 50:
    case 49:
      if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 5;
        }
      else if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 5;
        }
      else
        {
	  return 8;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 8;

    }
}

int
adder_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
      if ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000)))))
        {
	  return 2;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  return 1;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 3;
        }
      else
        {
	  return 4;
        }

    case 136:
    case 135:
    case 128:
    case 127:
      if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 2;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R5000))) || ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4600)))))))
        {
	  return 1;
        }
      else if ((! (((mips_cpu_attr) == (CPU_R5400)))) && (! (((mips_cpu_attr) == (CPU_R4300)))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case 25:
    case 24:
    case 1:
    case 0:
      if (((mips_cpu_attr) == (CPU_R6000)))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R3000))))
        {
	  return 2;
        }
      else
        {
	  return 4;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 4;

    }
}

int
imuldiv_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 268:
    case 263:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((which_alternative == 9) && ((mips16) != (0))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && (which_alternative != 14)))))))))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 10)
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 267:
    case 262:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 7))))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 9)
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5))))))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 3)
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 1;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 120:
    case 116:
    case 112:
    case 108:
      if (((! (((mips_cpu_attr) == (CPU_R3000)))) && (! (((mips_cpu_attr) == (CPU_R3900))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))
        {
	  if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4300))))
	    {
	      return 69 /* 0x45 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5000)))
	    {
	      return 68 /* 0x44 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4100)))
	    {
	      return 67 /* 0x43 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5400)))
	    {
	      return 66 /* 0x42 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4600)))
	    {
	      return 42 /* 0x2a */;
	    }
	  else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R4650))))))
	    {
	      return 38 /* 0x26 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4650)))
	    {
	      return 36 /* 0x24 */;
	    }
	  else
	    {
	      return 35 /* 0x23 */;
	    }
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 118:
    case 114:
    case 110:
    case 106:
    case 101:
    case 99:
    case 97:
    case 95:
      if (((! (((mips_cpu_attr) == (CPU_R3000)))) && (! (((mips_cpu_attr) == (CPU_R3900))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))
        {
	  if (((mips_cpu_attr) == (CPU_R4000)))
	    {
	      return 69 /* 0x45 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4600)))
	    {
	      return 42 /* 0x2a */;
	    }
	  else if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))
	    {
	      return 38 /* 0x26 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4300)))
	    {
	      return 37 /* 0x25 */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R5000))))
	    {
	      return 36 /* 0x24 */;
	    }
	  else if (((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || (((mips_cpu_attr) == (CPU_R4100))))
	    {
	      return 35 /* 0x23 */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5400)))
	    {
	      return 34 /* 0x22 */;
	    }
	  else
	    {
	      return 1;
	    }
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 78:
    case 77:
    case 65:
    case 64:
      if (((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))))) || ((((((((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))) || (((mips_cpu_attr) == (CPU_R4650)))) || (((mips_cpu_attr) == (CPU_R4100)))) || (((mips_cpu_attr) == (CPU_R4300)))) || (((mips_cpu_attr) == (CPU_R5000)))) || (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
	    {
	      return 17 /* 0x11 */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
	    {
	      return 12 /* 0xc */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
	    {
	      return 10 /* 0xa */;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5000)))
	    {
	      return 9;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4300)))
	    {
	      return 8;
	    }
	  else if (((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R4100)))) || (((mips_cpu_attr) == (CPU_R5400))))
	    {
	      return 4;
	    }
	  else
	    {
	      return 1;
	    }
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 2) && (((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))))) || (((((((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))) || (((mips_cpu_attr) == (CPU_R4650)))) || (((mips_cpu_attr) == (CPU_R4100)))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))) || (((mips_cpu_attr) == (CPU_R5400))))))
        {
	  if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
	    {
	      return 17 /* 0x11 */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
	    {
	      return 12 /* 0xc */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
	    {
	      return 10 /* 0xa */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
	    {
	      return 5;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4650)))
	    {
	      return 4;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5400)))
	    {
	      return 3;
	    }
	  else
	    {
	      return 1;
	    }
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 81:
    case 80:
    case 79:
    case 76:
    case 75:
    case 74:
    case 71:
    case 70:
    case 69:
    case 68:
    case 60:
    case 57:
    case 56:
    case 55:
      if (((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400)))))))))))) || (((((((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))) || (((mips_cpu_attr) == (CPU_R4650)))) || (((mips_cpu_attr) == (CPU_R4100)))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))) || (((mips_cpu_attr) == (CPU_R5400)))))
        {
	  if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
	    {
	      return 17 /* 0x11 */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
	    {
	      return 12 /* 0xc */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
	    {
	      return 10 /* 0xa */;
	    }
	  else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
	    {
	      return 5;
	    }
	  else if (((mips_cpu_attr) == (CPU_R4650)))
	    {
	      return 4;
	    }
	  else if (((mips_cpu_attr) == (CPU_R5400)))
	    {
	      return 3;
	    }
	  else
	    {
	      return 1;
	    }
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 53:
    case 52:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 5;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 50:
    case 49:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 8;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case 25:
    case 24:
    case 1:
    case 0:
      if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  return 3;
        }
      else
        {
	  return 69 /* 0x45 */;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 69 /* 0x45 */;

    }
}

unsigned int
imuldiv_unit_blockage_range (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 65605 /* min 1, max 69 */;

    }
}

int
memory_unit_ready_cost (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 276:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9)))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9)))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 275:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 9) || (which_alternative == 10)))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 5) && ((which_alternative != 9) && (which_alternative != 10))))) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 278:
    case 273:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative != 3) && (which_alternative != 4))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 4))) && ((which_alternative != 3) && (which_alternative != 4)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 277:
    case 272:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || (which_alternative == 2))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && (which_alternative != 2)) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10))))))))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) && (((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 267:
    case 266:
    case 262:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && (which_alternative != 3)) && (((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative != 4) && (which_alternative != 5)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12)))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || ((which_alternative == 11) || (which_alternative == 12))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10)))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 11) && (which_alternative != 12))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 3) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 9) || (which_alternative == 10))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 9) && (which_alternative != 10)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14))))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 11) || (which_alternative == 12)))))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 13) || (which_alternative == 14)))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) && (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 13) && (which_alternative != 14)))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative != 6) && (which_alternative != 7)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7)))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 6) || (which_alternative == 7)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) && ((which_alternative != 6) && (which_alternative != 7))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6))))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 5) || (which_alternative == 6)))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 0) && ((which_alternative != 1) && (which_alternative != 2))) && ((which_alternative == 7) || (which_alternative == 8))))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative == 0) || (which_alternative == 1)) || (((which_alternative != 7) && (which_alternative != 8)) && ((which_alternative != 5) && (which_alternative != 6)))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((mips_cpu_attr) == (CPU_R5400))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5)))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 2) || (which_alternative == 3))))
        {
	  return 3;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return 1;
        }
      else if (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && (((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 4) && (which_alternative != 5))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 290:
    case 289:
    case 287:
    case 282:
    case 259:
    case 258:
    case 257:
    case 256:
    case 240:
    case 229:
    case 225:
    case 223:
      return 1;

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R5000))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R4300))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R4100))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R4650))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R4600))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R3900))) && (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (which_alternative == 3))) || (((((mips_cpu_attr) == (CPU_R3000))) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || (which_alternative == 3)))) || (((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000))))))))))) && ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && (which_alternative != 3))) || (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || (which_alternative == 3)))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((((mips_cpu_attr) == (CPU_R5400))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R5000))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4300))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4100))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4650))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R4600))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R3900))) && (which_alternative == 0)) || (((((mips_cpu_attr) == (CPU_R3000))) && (which_alternative == 0)) || ((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case 325:
    case 255:
    case 254:
    case 253:
    case 252:
    case 224:
    case 222:
    case 205:
    case 203:
    case 201:
    case 199:
    case 197:
    case 194:
    case 193:
    case 190:
    case 187:
    case 184:
    case 181:
    case 178:
      if ((! (((mips_cpu_attr) == (CPU_R5400)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && (! (((mips_cpu_attr) == (CPU_R3000)))))))))))
        {
	  return 3;
        }
      else
        {
	  return 2;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 3;

    }
}

int
function_units_used (insn)
     rtx insn;
{
  register enum attr_mode attr_mode = get_attr_mode (insn);
  register enum attr_type attr_type = get_attr_type (insn);
  register unsigned long accum = 0;

  accum |= ((((attr_type == TYPE_LOAD) && ((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))) || (((attr_type == TYPE_STORE) && (1)) || ((attr_type == TYPE_XFER) && (1)))) ? (1) : (0));
  accum |= ((((attr_type == TYPE_HILO) && (1)) || (((attr_type == TYPE_IMUL) && (! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))))) || (((attr_type == TYPE_HILO) && ((mips16) != (0))) || (((attr_type == TYPE_IMUL) && (((((((((((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))) || ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))) || (((mips_cpu_attr) == (CPU_R4650)))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R4100))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4100))))) || ((attr_mode == MODE_SI) && ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000)))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R5400))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5400)))))) || (((attr_type == TYPE_IDIV) && (((((((((((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4100))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R4600)))) || (((mips_cpu_attr) == (CPU_R4650)))) || (((mips_cpu_attr) == (CPU_R4000)))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R4100))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4100))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R4300))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_SI) && (((mips_cpu_attr) == (CPU_R5400))))) || ((attr_mode == MODE_DI) && (((mips_cpu_attr) == (CPU_R5400)))))) || (((attr_type == TYPE_FADD) && (((mips_cpu_attr) == (CPU_R4300)))) || ((((attr_type == TYPE_FCMP) || ((attr_type == TYPE_FABS) || (attr_type == TYPE_FNEG))) && (((mips_cpu_attr) == (CPU_R4300)))) || (((attr_type == TYPE_FMUL) && (((attr_mode == MODE_SF) && (((mips_cpu_attr) == (CPU_R4300)))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R4300)))))) || (((attr_type == TYPE_FDIV) && ((attr_type == TYPE_FSQRT) || (attr_type == TYPE_FRSQRT))) && (((attr_mode == MODE_SF) && (((mips_cpu_attr) == (CPU_R4300)))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R4300)))))))))))))) ? (2) : (0));
  accum |= ((((attr_type == TYPE_FCMP) && (((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R6000)))))) || (((mips_cpu_attr) == (CPU_R5000))))) || (((attr_type == TYPE_FADD) && (((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5400)))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R6000))))) || (((attr_type == TYPE_FABS) || (attr_type == TYPE_FNEG)) && ((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || (((mips_cpu_attr) == (CPU_R5000))))))))))) ? (4) : (0));
  accum |= (((attr_type == TYPE_FMUL) && (((((attr_mode == MODE_SF) && ((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000)))))) || (((mips_cpu_attr) == (CPU_R6000)))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || (((mips_cpu_attr) == (CPU_R5000))))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R6000)))))) ? (8) : (0));
  accum |= (((((attr_type == TYPE_MOVE) || ((attr_type == TYPE_ARITH) || ((attr_type == TYPE_DARITH) || ((attr_type == TYPE_ICMP) || (attr_type == TYPE_NOP))))) && (((mips_cpu_attr) == (CPU_R5400)))) || (((attr_type == TYPE_FADD) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FCMP) || ((attr_type == TYPE_FABS) || (attr_type == TYPE_FNEG))) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FMUL) && (attr_mode == MODE_SF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FMUL) && (attr_mode == MODE_DF)) && (((mips_cpu_attr) == (CPU_R5400)))) || (((((attr_type == TYPE_FDIV) || (attr_type == TYPE_FSQRT)) && (attr_mode == MODE_SF)) && (((mips_cpu_attr) == (CPU_R5400)))) || (((((attr_type == TYPE_FDIV) || (attr_type == TYPE_FSQRT)) && (attr_mode == MODE_DF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FRSQRT) && (attr_mode == MODE_SF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FRSQRT) && (attr_mode == MODE_DF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FMADD) && (attr_mode == MODE_SF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((((attr_type == TYPE_FMADD) && (attr_mode == MODE_DF)) && (((mips_cpu_attr) == (CPU_R5400)))) || ((attr_type == TYPE_FCVT) && (((mips_cpu_attr) == (CPU_R5400))))))))))))))) ? (32) : (0));
  accum |= ((((attr_type == TYPE_FDIV) && ((((((attr_mode == MODE_SF) && (((((! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400))))))))))) || ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))) || (((mips_cpu_attr) == (CPU_R6000)))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))) || (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R3000))) || ((((mips_cpu_attr) == (CPU_R3900))) || ((((mips_cpu_attr) == (CPU_R6000))) || ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5400)))))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900)))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R6000))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))))) || (((attr_type == TYPE_FSQRT) || (attr_type == TYPE_FRSQRT)) && (((((attr_mode == MODE_SF) && (((! ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))) || ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650))))) || (((mips_cpu_attr) == (CPU_R5000))))) || ((attr_mode == MODE_DF) && (! ((((mips_cpu_attr) == (CPU_R4600))) || ((((mips_cpu_attr) == (CPU_R4650))) || ((((mips_cpu_attr) == (CPU_R4300))) || ((((mips_cpu_attr) == (CPU_R5000))) || (((mips_cpu_attr) == (CPU_R5400)))))))))) || ((attr_mode == MODE_DF) && ((((mips_cpu_attr) == (CPU_R4600))) || (((mips_cpu_attr) == (CPU_R4650)))))) || ((attr_mode == MODE_DF) && (((mips_cpu_attr) == (CPU_R5000))))))) ? (16) : (0));

  if (accum && accum == (accum & -accum))
    {
      int i;
      for (i = 0; accum >>= 1; ++i) continue;
      accum = i;
    }
  else
    accum = ~accum;
  return accum;
}

int
num_delay_slots (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 492:
    case 490:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 477:
    case 474:
    case 473:
    case 472:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 464:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case 463:
    case 462:
    case 453:
    case 451:
    case 449:
    case 445:
    case 444:
    case 442:
    case 441:
    case 438:
      return 1;

    case 521:
    case 520:
    case 519:
    case 518:
    case 439:
    case 353:
    case 352:
    case 351:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  return 1;
        }
      else
        {
	  return 0;
        }

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return 0;

    }
}

enum attr_dslot
get_attr_dslot (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 276:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 275:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 6) || (which_alternative == 7)) || (((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 5) || ((which_alternative == 9) || (which_alternative == 10))))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 278:
    case 273:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 277:
    case 272:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || (which_alternative == 2)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 1) || ((which_alternative == 6) || (which_alternative == 7))) || (((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 9) || (which_alternative == 10)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 268:
    case 263:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 5) || (which_alternative == 6))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 267:
    case 262:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 7))))))))) || (((which_alternative == 2) || (which_alternative == 3)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 6) || (which_alternative == 7)) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && (which_alternative != 8)))))))))) || (((which_alternative == 2) || (which_alternative == 3)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))) || ((((which_alternative != 0) && (which_alternative != 1)) && (((which_alternative == 2) || (which_alternative == 3)) || ((which_alternative == 9) || (which_alternative == 10)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && (which_alternative != 10)))))))))) || (((which_alternative == 3) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 7) || (which_alternative == 8)))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 8) || (which_alternative == 9)) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))))))))) || (((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((((which_alternative == 8) || (which_alternative == 9)) || ((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && ((which_alternative != 9) && ((which_alternative != 10) && ((which_alternative != 11) && ((which_alternative != 12) && ((which_alternative != 13) && (which_alternative != 14)))))))))))))))) || (((which_alternative == 1) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 11) || (which_alternative == 12))))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 10) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 6) || (which_alternative == 7))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && (which_alternative != 7)))))))) || (((which_alternative == 1) || ((which_alternative == 4) || (which_alternative == 5))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 9) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 5) || (which_alternative == 6))) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative != 0) && ((which_alternative != 1) && ((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 4) && (which_alternative != 5)))))) || (((which_alternative == 2) || (which_alternative == 3)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 3) || ((((which_alternative != 0) && (which_alternative != 1)) && (which_alternative != 2)) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative != 0) && (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900)))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 325:
    case 255:
    case 254:
    case 253:
    case 252:
    case 224:
    case 222:
    case 205:
    case 203:
    case 201:
    case 199:
    case 197:
    case 194:
    case 193:
    case 190:
    case 187:
    case 184:
    case 181:
    case 178:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((mips_isa) == (1)) && (((mips16) == (0)) && (! (((mips_cpu_attr) == (CPU_R3900))))))
        {
	  return DSLOT_YES;
        }
      else
        {
	  return DSLOT_NO;
        }

    case 521:
    case 520:
    case 519:
    case 518:
    case 492:
    case 490:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 477:
    case 474:
    case 473:
    case 472:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 464:
    case 463:
    case 462:
    case 453:
    case 451:
    case 449:
    case 445:
    case 444:
    case 442:
    case 441:
    case 439:
    case 438:
    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 353:
    case 352:
    case 351:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
      return DSLOT_YES;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      return DSLOT_NO;

    }
}

enum attr_mode
get_attr_mode (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 428:
    case 429:
    case 430:
    case 431:
    case 432:
    case 433:
    case 434:
    case 435:
    case 436:
    case 437:
      return MODE_FPSW;

    case 0:
    case 24:
    case 49:
    case 50:
    case 82:
    case 84:
    case 86:
    case 88:
    case 90:
    case 92:
    case 121:
    case 123:
    case 127:
    case 135:
    case 206:
    case 207:
    case 209:
    case 211:
    case 212:
    case 254:
    case 255:
    case 258:
    case 259:
    case 275:
    case 276:
    case 277:
    case 278:
    case 503:
    case 504:
    case 514:
      return MODE_DF;

    case 1:
    case 25:
    case 52:
    case 53:
    case 83:
    case 85:
    case 87:
    case 89:
    case 91:
    case 93:
    case 122:
    case 124:
    case 128:
    case 136:
    case 167:
    case 208:
    case 210:
    case 213:
    case 214:
    case 252:
    case 253:
    case 256:
    case 257:
    case 271:
    case 272:
    case 273:
    case 501:
    case 502:
    case 513:
      return MODE_SF;

    case 10:
    case 13:
    case 16:
    case 17:
    case 18:
    case 19:
    case 34:
    case 37:
    case 40:
    case 41:
    case 42:
    case 43:
    case 46:
    case 64:
    case 65:
    case 77:
    case 78:
    case 108:
    case 112:
    case 116:
    case 120:
    case 126:
    case 130:
    case 133:
    case 134:
    case 138:
    case 144:
    case 145:
    case 147:
    case 152:
    case 153:
    case 159:
    case 160:
    case 161:
    case 163:
    case 165:
    case 178:
    case 183:
    case 184:
    case 192:
    case 193:
    case 194:
    case 195:
    case 197:
    case 205:
    case 229:
    case 230:
    case 231:
    case 233:
    case 234:
    case 280:
    case 297:
    case 300:
    case 303:
    case 304:
    case 311:
    case 312:
    case 315:
    case 318:
    case 319:
    case 328:
    case 329:
    case 332:
    case 335:
    case 337:
    case 338:
    case 367:
    case 368:
    case 371:
    case 375:
    case 378:
    case 383:
    case 384:
    case 388:
    case 393:
    case 394:
    case 398:
    case 399:
    case 402:
    case 407:
    case 408:
    case 412:
    case 417:
    case 418:
    case 422:
    case 423:
    case 426:
    case 498:
    case 499:
    case 500:
    case 512:
    case 517:
    case 523:
      return MODE_DI;

    case 169:
    case 176:
    case 186:
    case 187:
    case 261:
    case 262:
    case 263:
    case 510:
    case 515:
      return MODE_HI;

    case 170:
    case 266:
    case 267:
    case 268:
    case 509:
      return MODE_QI;

    case 282:
    case 283:
    case 284:
    case 285:
    case 287:
    case 288:
    case 289:
    case 290:
    case 346:
    case 347:
    case 348:
    case 349:
    case 350:
    case 351:
    case 352:
    case 353:
    case 438:
    case 439:
    case 441:
    case 442:
    case 444:
    case 445:
    case 449:
    case 451:
    case 453:
    case 459:
    case 462:
    case 463:
    case 464:
    case 467:
    case 468:
    case 469:
    case 470:
    case 471:
    case 472:
    case 473:
    case 474:
    case 477:
    case 480:
    case 481:
    case 482:
    case 483:
    case 484:
    case 485:
    case 486:
    case 487:
    case 490:
    case 492:
    case 494:
    case 518:
    case 519:
    case 520:
    case 521:
      return MODE_NONE;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 103:
    case 104:
    case 226:
      return MODE_UNKNOWN;

    default:
      return MODE_SI;

    }
}

enum attr_type
get_attr_type (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    case 276:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5))))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 10)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 275:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_STORE;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 278:
    case 273:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 5)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 277:
    case 272:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 1) || (which_alternative == 2))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_XFER;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 267:
    case 262:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || (which_alternative == 1))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 8)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_LOAD;
        }
      else if (which_alternative == 11)
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_STORE;
        }

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || ((which_alternative == 5) || (which_alternative == 6)))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 7) || (which_alternative == 8))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 9) || (which_alternative == 10))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 8) || (which_alternative == 9))
        {
	  return TYPE_XFER;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_STORE;
        }
      else if ((which_alternative == 8) || (which_alternative == 9))
        {
	  return TYPE_XFER;
        }
      else if (which_alternative == 10)
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 11) || (which_alternative == 12))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 13) || (which_alternative == 14))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || ((which_alternative == 4) || (which_alternative == 5)))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 8) || (which_alternative == 9))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 6) || (which_alternative == 7))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if ((which_alternative == 3) || (which_alternative == 4))
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 5) || (which_alternative == 6))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 7) || (which_alternative == 8))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 1)
        {
	  return TYPE_ARITH;
        }
      else if ((which_alternative == 2) || (which_alternative == 3))
        {
	  return TYPE_LOAD;
        }
      else if ((which_alternative == 4) || (which_alternative == 5))
        {
	  return TYPE_STORE;
        }
      else
        {
	  return TYPE_HILO;
        }

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2)))
        {
	  return TYPE_MOVE;
        }
      else if (which_alternative == 3)
        {
	  return TYPE_HILO;
        }
      else
        {
	  return TYPE_LOAD;
        }

    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative == 0)
        {
	  return TYPE_ARITH;
        }
      else
        {
	  return TYPE_LOAD;
        }

    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (which_alternative != 2)
        {
	  return TYPE_IMUL;
        }
      else
        {
	  return TYPE_MULTI;
        }

    case 494:
      return TYPE_NOP;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    case 125:
    case 126:
    case 129:
    case 130:
    case 283:
    case 284:
    case 285:
    case 288:
      return TYPE_MULTI;

    case 123:
    case 124:
      return TYPE_FRSQRT;

    case 121:
    case 122:
      return TYPE_FSQRT;

    case 167:
    case 206:
    case 207:
    case 208:
    case 209:
    case 210:
    case 211:
    case 212:
    case 213:
    case 214:
      return TYPE_FCVT;

    case 428:
    case 429:
    case 430:
    case 431:
    case 432:
    case 433:
    case 434:
    case 435:
    case 436:
    case 437:
      return TYPE_FCMP;

    case 135:
    case 136:
      return TYPE_FNEG;

    case 127:
    case 128:
      return TYPE_FABS;

    case 90:
    case 91:
    case 92:
    case 93:
      return TYPE_FDIV;

    case 82:
    case 83:
    case 84:
    case 85:
    case 86:
    case 87:
    case 88:
    case 89:
      return TYPE_FMADD;

    case 49:
    case 50:
    case 52:
    case 53:
      return TYPE_FMUL;

    case 0:
    case 1:
    case 24:
    case 25:
      return TYPE_FADD;

    case 95:
    case 97:
    case 99:
    case 101:
    case 106:
    case 108:
    case 110:
    case 112:
    case 114:
    case 116:
    case 118:
    case 120:
      return TYPE_IDIV;

    case 55:
    case 56:
    case 57:
    case 60:
    case 64:
    case 65:
    case 68:
    case 69:
    case 70:
    case 71:
    case 74:
    case 75:
    case 76:
    case 77:
    case 78:
    case 79:
    case 80:
    case 81:
      return TYPE_IMUL;

    case 10:
    case 13:
    case 16:
    case 34:
    case 37:
    case 40:
    case 133:
    case 138:
    case 144:
    case 145:
    case 152:
    case 153:
    case 159:
    case 160:
    case 165:
    case 168:
    case 169:
    case 170:
    case 171:
    case 172:
    case 173:
    case 174:
    case 175:
    case 176:
    case 296:
    case 297:
    case 300:
    case 311:
    case 312:
    case 315:
    case 328:
    case 329:
    case 332:
      return TYPE_DARITH;

    case 226:
    case 280:
    case 495:
    case 496:
    case 497:
    case 498:
    case 499:
    case 500:
    case 501:
    case 502:
    case 503:
    case 504:
      return TYPE_MOVE;

    case 223:
    case 225:
    case 229:
    case 240:
    case 256:
    case 257:
    case 258:
    case 259:
    case 282:
    case 287:
    case 289:
    case 290:
      return TYPE_STORE;

    case 178:
    case 181:
    case 184:
    case 187:
    case 190:
    case 193:
    case 194:
    case 197:
    case 199:
    case 201:
    case 203:
    case 205:
    case 222:
    case 224:
    case 252:
    case 253:
    case 254:
    case 255:
    case 325:
      return TYPE_LOAD;

    case 464:
    case 467:
    case 468:
    case 469:
    case 470:
    case 471:
    case 472:
    case 473:
    case 474:
    case 477:
    case 480:
    case 481:
    case 482:
    case 483:
    case 484:
    case 485:
    case 486:
    case 487:
    case 490:
    case 492:
      return TYPE_CALL;

    case 438:
    case 441:
    case 442:
    case 444:
    case 445:
    case 449:
    case 451:
    case 453:
    case 462:
    case 463:
      return TYPE_JUMP;

    case 346:
    case 347:
    case 348:
    case 349:
    case 350:
    case 351:
    case 352:
    case 353:
    case 439:
    case 518:
    case 519:
    case 520:
    case 521:
      return TYPE_BRANCH;

    case 103:
    case 104:
    case 459:
    case 509:
    case 510:
    case 511:
    case 512:
    case 513:
    case 514:
    case 515:
    case 516:
    case 517:
      return TYPE_UNKNOWN;

    default:
      return TYPE_ARITH;

    }
}

int
eligible_for_delay (delay_insn, slot, candidate_insn, flags)
     rtx delay_insn;
     int slot;
     rtx candidate_insn;
     int flags;
{
  rtx insn;

  if (slot >= 1)
    abort ();

  insn = delay_insn;
  switch (recog_memoized (insn))
    {
    case 492:
    case 490:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 477:
    case 474:
    case 473:
    case 472:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 464:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  slot += 3 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case 463:
    case 462:
    case 453:
    case 451:
    case 449:
    case 445:
    case 444:
    case 442:
    case 441:
    case 438:
      slot += 2 * 1;
      break;
      break;

    case 521:
    case 520:
    case 519:
    case 518:
    case 439:
    case 353:
    case 352:
    case 351:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  slot += 1 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      slot += 0 * 1;
      break;
      break;

    }

  if (slot < 1)
    abort ();

  insn = candidate_insn;
  switch (slot)
    {
    case 3:
      switch (recog_memoized (insn))
	{
        case 423:
        case 421:
        case 399:
        case 397:
	  extract_insn (insn);
	  if (m16_uimm8_m1_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 338:
        case 323:
        case 319:
        case 308:
        case 304:
        case 293:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 276:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 273:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 272:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 271:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 268:
        case 263:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 267:
        case 262:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 266:
        case 261:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 255:
        case 254:
        case 253:
        case 252:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 249:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))) && (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 243:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 4) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 6) && (m16_usym8_4 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 8) && (which_alternative != 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 242:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 1) && ((which_alternative != 3) && ((which_alternative != 5) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 241:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || (which_alternative == 14)))))))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 13) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || (which_alternative == 18))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 234:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 5) && (m16_usym5_4 (operands[1], VOIDmode))) || ((which_alternative == 6) || ((which_alternative == 8) || (which_alternative == 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 233:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 275:
        case 240:
        case 229:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 0)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 210:
        case 209:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 1)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 195:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 3) && ((((which_alternative == 0) || (which_alternative == 1)) || (which_alternative == 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (which_alternative != 5))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 192:
        case 189:
        case 186:
        case 183:
        case 180:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 205:
        case 203:
        case 201:
        case 199:
        case 197:
        case 194:
        case 193:
        case 190:
        case 187:
        case 184:
        case 181:
        case 178:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 170:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 418:
        case 416:
        case 394:
        case 392:
        case 161:
        case 157:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 153:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 152:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 145:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 144:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 138:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 65:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((GENERATE_MULT3) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 61:
        case 58:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative != 2)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 43:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_insn (insn);
	  if (m16_nuimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 47:
        case 30:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 29:
	  extract_insn (insn);
	  if (m16_nuimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 41:
        case 28:
	  extract_insn (insn);
	  if (m16_nsimm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 19:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 18:
	  extract_insn (insn);
	  if (m16_uimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 23:
        case 6:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 5:
	  extract_insn (insn);
	  if (m16_uimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 17:
        case 4:
	  extract_insn (insn);
	  if (m16_simm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        case 504:
        case 503:
        case 502:
        case 501:
        case 500:
        case 499:
        case 498:
        case 497:
        case 496:
        case 495:
        case 494:
        case 422:
        case 420:
        case 417:
        case 415:
        case 408:
        case 407:
        case 406:
        case 405:
        case 398:
        case 396:
        case 393:
        case 391:
        case 384:
        case 383:
        case 382:
        case 381:
        case 375:
        case 374:
        case 368:
        case 367:
        case 366:
        case 365:
        case 337:
        case 336:
        case 335:
        case 322:
        case 318:
        case 307:
        case 303:
        case 292:
        case 290:
        case 289:
        case 259:
        case 258:
        case 257:
        case 256:
        case 227:
        case 226:
        case 206:
        case 176:
        case 175:
        case 174:
        case 167:
        case 164:
        case 163:
        case 156:
        case 150:
        case 149:
        case 147:
        case 142:
        case 141:
        case 137:
        case 136:
        case 135:
        case 134:
        case 131:
        case 128:
        case 127:
        case 124:
        case 123:
        case 122:
        case 121:
        case 120:
        case 118:
        case 116:
        case 114:
        case 112:
        case 110:
        case 108:
        case 106:
        case 101:
        case 99:
        case 97:
        case 95:
        case 93:
        case 92:
        case 91:
        case 90:
        case 89:
        case 88:
        case 87:
        case 86:
        case 85:
        case 84:
        case 83:
        case 82:
        case 81:
        case 80:
        case 79:
        case 78:
        case 77:
        case 76:
        case 75:
        case 74:
        case 71:
        case 70:
        case 69:
        case 68:
        case 64:
        case 60:
        case 56:
        case 55:
        case 52:
        case 49:
        case 46:
        case 40:
        case 27:
        case 25:
        case 24:
        case 22:
        case 16:
        case 3:
        case 1:
        case 0:
	  return 1;

        default:
	  return 0;

      }
    case 2:
      switch (recog_memoized (insn))
	{
        case 423:
        case 421:
        case 399:
        case 397:
	  extract_insn (insn);
	  if (m16_uimm8_m1_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 338:
        case 323:
        case 319:
        case 308:
        case 304:
        case 293:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 276:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 273:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 272:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 271:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 268:
        case 263:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 267:
        case 262:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 266:
        case 261:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 255:
        case 254:
        case 253:
        case 252:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 249:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))) && (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 243:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 4) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 6) && (m16_usym8_4 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 8) && (which_alternative != 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 242:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 1) && ((which_alternative != 3) && ((which_alternative != 5) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 241:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || (which_alternative == 14)))))))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 13) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || (which_alternative == 18))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 234:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 5) && (m16_usym5_4 (operands[1], VOIDmode))) || ((which_alternative == 6) || ((which_alternative == 8) || (which_alternative == 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 233:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 275:
        case 240:
        case 229:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 0)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 210:
        case 209:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 1)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 195:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 3) && ((((which_alternative == 0) || (which_alternative == 1)) || (which_alternative == 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (which_alternative != 5))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 192:
        case 189:
        case 186:
        case 183:
        case 180:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 205:
        case 203:
        case 201:
        case 199:
        case 197:
        case 194:
        case 193:
        case 190:
        case 187:
        case 184:
        case 181:
        case 178:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 170:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 418:
        case 416:
        case 394:
        case 392:
        case 161:
        case 157:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 153:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 152:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 145:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 144:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 138:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 65:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((GENERATE_MULT3) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 61:
        case 58:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative != 2)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 43:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_insn (insn);
	  if (m16_nuimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 47:
        case 30:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 29:
	  extract_insn (insn);
	  if (m16_nuimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 41:
        case 28:
	  extract_insn (insn);
	  if (m16_nsimm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 19:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 18:
	  extract_insn (insn);
	  if (m16_uimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 23:
        case 6:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 5:
	  extract_insn (insn);
	  if (m16_uimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 17:
        case 4:
	  extract_insn (insn);
	  if (m16_simm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        case 504:
        case 503:
        case 502:
        case 501:
        case 500:
        case 499:
        case 498:
        case 497:
        case 496:
        case 495:
        case 494:
        case 422:
        case 420:
        case 417:
        case 415:
        case 408:
        case 407:
        case 406:
        case 405:
        case 398:
        case 396:
        case 393:
        case 391:
        case 384:
        case 383:
        case 382:
        case 381:
        case 375:
        case 374:
        case 368:
        case 367:
        case 366:
        case 365:
        case 337:
        case 336:
        case 335:
        case 322:
        case 318:
        case 307:
        case 303:
        case 292:
        case 290:
        case 289:
        case 259:
        case 258:
        case 257:
        case 256:
        case 227:
        case 226:
        case 206:
        case 176:
        case 175:
        case 174:
        case 167:
        case 164:
        case 163:
        case 156:
        case 150:
        case 149:
        case 147:
        case 142:
        case 141:
        case 137:
        case 136:
        case 135:
        case 134:
        case 131:
        case 128:
        case 127:
        case 124:
        case 123:
        case 122:
        case 121:
        case 120:
        case 118:
        case 116:
        case 114:
        case 112:
        case 110:
        case 108:
        case 106:
        case 101:
        case 99:
        case 97:
        case 95:
        case 93:
        case 92:
        case 91:
        case 90:
        case 89:
        case 88:
        case 87:
        case 86:
        case 85:
        case 84:
        case 83:
        case 82:
        case 81:
        case 80:
        case 79:
        case 78:
        case 77:
        case 76:
        case 75:
        case 74:
        case 71:
        case 70:
        case 69:
        case 68:
        case 64:
        case 60:
        case 56:
        case 55:
        case 52:
        case 49:
        case 46:
        case 40:
        case 27:
        case 25:
        case 24:
        case 22:
        case 16:
        case 3:
        case 1:
        case 0:
	  return 1;

        default:
	  return 0;

      }
    case 1:
      switch (recog_memoized (insn))
	{
        case 423:
        case 421:
        case 399:
        case 397:
	  extract_insn (insn);
	  if (m16_uimm8_m1_1 (operands[2], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 338:
        case 323:
        case 319:
        case 308:
        case 304:
        case 293:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 276:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 273:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 272:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 271:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 268:
        case 263:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 9) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 267:
        case 262:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 266:
        case 261:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 255:
        case 254:
        case 253:
        case 252:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 249:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))) && (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 243:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 4) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 6) && (m16_usym8_4 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 8) && (which_alternative != 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 242:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 1) && ((which_alternative != 3) && ((which_alternative != 5) && (which_alternative != 7)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 241:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || (which_alternative == 14)))))))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 13) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || (which_alternative == 18))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 234:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 10) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 5) && (m16_usym5_4 (operands[1], VOIDmode))) || ((which_alternative == 6) || ((which_alternative == 8) || (which_alternative == 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 233:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 275:
        case 240:
        case 229:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 0)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 210:
        case 209:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative == 1)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 195:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative != 3) && ((((which_alternative == 0) || (which_alternative == 1)) || (which_alternative == 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (which_alternative != 5))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 192:
        case 189:
        case 186:
        case 183:
        case 180:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 205:
        case 203:
        case 201:
        case 199:
        case 197:
        case 194:
        case 193:
        case 190:
        case 187:
        case 184:
        case 181:
        case 178:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 170:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips16) == (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 418:
        case 416:
        case 394:
        case 392:
        case 161:
        case 157:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 153:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 152:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 145:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 144:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((TARGET_64BIT) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 138:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((mips_isa) >= (3))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 65:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((GENERATE_MULT3) != (0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 61:
        case 58:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (which_alternative != 2)
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 43:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_insn (insn);
	  if (m16_nuimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 47:
        case 30:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_nsimm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 29:
	  extract_insn (insn);
	  if (m16_nuimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 41:
        case 28:
	  extract_insn (insn);
	  if (m16_nsimm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 19:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 18:
	  extract_insn (insn);
	  if (m16_uimm5_4 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 23:
        case 6:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if (((which_alternative == 0) && (m16_simm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 5:
	  extract_insn (insn);
	  if (m16_uimm8_4 (operands[1], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 17:
        case 4:
	  extract_insn (insn);
	  if (m16_simm8_8 (operands[0], VOIDmode))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        case 504:
        case 503:
        case 502:
        case 501:
        case 500:
        case 499:
        case 498:
        case 497:
        case 496:
        case 495:
        case 494:
        case 422:
        case 420:
        case 417:
        case 415:
        case 408:
        case 407:
        case 406:
        case 405:
        case 398:
        case 396:
        case 393:
        case 391:
        case 384:
        case 383:
        case 382:
        case 381:
        case 375:
        case 374:
        case 368:
        case 367:
        case 366:
        case 365:
        case 337:
        case 336:
        case 335:
        case 322:
        case 318:
        case 307:
        case 303:
        case 292:
        case 290:
        case 289:
        case 259:
        case 258:
        case 257:
        case 256:
        case 227:
        case 226:
        case 206:
        case 176:
        case 175:
        case 174:
        case 167:
        case 164:
        case 163:
        case 156:
        case 150:
        case 149:
        case 147:
        case 142:
        case 141:
        case 137:
        case 136:
        case 135:
        case 134:
        case 131:
        case 128:
        case 127:
        case 124:
        case 123:
        case 122:
        case 121:
        case 120:
        case 118:
        case 116:
        case 114:
        case 112:
        case 110:
        case 108:
        case 106:
        case 101:
        case 99:
        case 97:
        case 95:
        case 93:
        case 92:
        case 91:
        case 90:
        case 89:
        case 88:
        case 87:
        case 86:
        case 85:
        case 84:
        case 83:
        case 82:
        case 81:
        case 80:
        case 79:
        case 78:
        case 77:
        case 76:
        case 75:
        case 74:
        case 71:
        case 70:
        case 69:
        case 68:
        case 64:
        case 60:
        case 56:
        case 55:
        case 52:
        case 49:
        case 46:
        case 40:
        case 27:
        case 25:
        case 24:
        case 22:
        case 16:
        case 3:
        case 1:
        case 0:
	  return 1;

        default:
	  return 0;

      }
    default:
      abort ();
    }
}

int
eligible_for_annul_false (delay_insn, slot, candidate_insn, flags)
     rtx delay_insn;
     int slot;
     rtx candidate_insn;
     int flags;
{
  rtx insn;

  if (slot >= 1)
    abort ();

  insn = delay_insn;
  switch (recog_memoized (insn))
    {
    case 492:
    case 490:
    case 487:
    case 486:
    case 485:
    case 484:
    case 483:
    case 482:
    case 481:
    case 480:
    case 477:
    case 474:
    case 473:
    case 472:
    case 471:
    case 470:
    case 469:
    case 468:
    case 467:
    case 464:
      if (((mips_abicalls_attr) == (ABICALLS_NO)))
        {
	  slot += 3 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case 463:
    case 462:
    case 453:
    case 451:
    case 449:
    case 445:
    case 444:
    case 442:
    case 441:
    case 438:
      slot += 2 * 1;
      break;
      break;

    case 521:
    case 520:
    case 519:
    case 518:
    case 439:
    case 353:
    case 352:
    case 351:
    case 350:
    case 349:
    case 348:
    case 347:
    case 346:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((mips16) == (0))
        {
	  slot += 1 * 1;
      break;
        }
      else
        {
	  slot += 0 * 1;
      break;
        }
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      slot += 0 * 1;
      break;
      break;

    }

  if (slot < 1)
    abort ();

  insn = candidate_insn;
  switch (slot)
    {
    case 3:
      switch (recog_memoized (insn))
	{
        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 0;

      }
    case 2:
      switch (recog_memoized (insn))
	{
        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        default:
	  return 0;

      }
    case 1:
      switch (recog_memoized (insn))
	{
        case 423:
        case 421:
        case 399:
        case 397:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm8_m1_1 (operands[2], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 338:
        case 323:
        case 319:
        case 308:
        case 304:
        case 293:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative != 1) || (m16_uimm3_b (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 276:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 9))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 7) && ((which_alternative != 8) && (which_alternative != 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 273:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) || ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))) && ((which_alternative != 3) && (which_alternative != 4))))) || (((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || (which_alternative == 5))))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 272:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && (which_alternative != 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 3)))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 271:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative != 1) && ((which_alternative != 6) && (which_alternative != 7))) && (((which_alternative != 2) && ((which_alternative != 3) && ((which_alternative != 9) && (which_alternative != 10)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11)))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 268:
        case 263:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative != 9) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 5) && (which_alternative != 6))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 6) && (which_alternative != 8)))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 267:
        case 262:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 266:
        case 261:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 6) && (which_alternative != 7)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || (which_alternative == 8)))))))))) && (((which_alternative != 2) && (which_alternative != 3)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 3) && (which_alternative != 5))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 255:
        case 254:
        case 253:
        case 252:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 249:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative == 1) || (((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 11))))))) && (((((which_alternative != 2) && (which_alternative != 3)) && ((which_alternative != 9) && (which_alternative != 10))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && ((which_alternative != 6) && (which_alternative != 7))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 243:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))) && (((which_alternative != 3) && (((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 7) && (which_alternative != 8)))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (((which_alternative == 4) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 6) && (m16_usym8_4 (operands[1], VOIDmode))) || ((which_alternative != 3) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 6) && ((which_alternative != 8) && (which_alternative != 10))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 242:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || (which_alternative == 9))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative != 1) && ((which_alternative != 3) && ((which_alternative != 5) && (which_alternative != 7))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 241:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((((which_alternative != 8) && (which_alternative != 9)) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || ((which_alternative == 7) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 12) || ((which_alternative == 13) || (which_alternative == 14)))))))))))))))) && (((which_alternative != 1) && ((which_alternative != 4) && ((which_alternative != 5) && ((which_alternative != 11) && (which_alternative != 12))))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || ((which_alternative == 10) || ((which_alternative == 11) || ((which_alternative == 13) || ((which_alternative == 15) || ((which_alternative == 16) || ((which_alternative == 17) || (which_alternative == 18)))))))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 234:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative != 10) && ((((which_alternative == 0) || (which_alternative == 1)) || ((which_alternative != 6) && (which_alternative != 7))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || (((which_alternative == 3) && (m16_uimm8_1 (operands[1], VOIDmode))) || (((which_alternative == 5) && (m16_usym5_4 (operands[1], VOIDmode))) || ((which_alternative == 6) || ((which_alternative == 8) || (which_alternative == 10))))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 233:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((((which_alternative == 0) || ((which_alternative == 1) || ((which_alternative == 2) || ((which_alternative == 3) || ((which_alternative == 4) || ((which_alternative == 5) || ((which_alternative == 6) || (which_alternative == 7)))))))) && (((which_alternative != 1) && ((which_alternative != 4) && (which_alternative != 5))) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && ((which_alternative == 0) || ((which_alternative == 2) || ((which_alternative == 4) || ((which_alternative == 6) || ((which_alternative == 8) || ((which_alternative == 9) || (which_alternative == 10)))))))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 275:
        case 240:
        case 229:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative == 0))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 210:
        case 209:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative == 1))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 195:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative != 3) && ((((which_alternative == 0) || (which_alternative == 1)) || (which_alternative == 2)) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))))) && (which_alternative != 5)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 192:
        case 189:
        case 186:
        case 183:
        case 180:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) || ((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900)))))) && (which_alternative != 2)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 205:
        case 203:
        case 201:
        case 199:
        case 197:
        case 194:
        case 193:
        case 190:
        case 187:
        case 184:
        case 181:
        case 178:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((! ((mips_isa) == (1))) || ((! ((mips16) == (0))) || (((mips_cpu_attr) == (CPU_R3900))))) && (which_alternative == 0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 170:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 169:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips16) == (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 165:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 159:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 418:
        case 416:
        case 394:
        case 392:
        case 161:
        case 157:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((which_alternative != 1) || (m16_uimm8_1 (operands[2], VOIDmode))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 153:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 152:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 145:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 144:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((TARGET_64BIT) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 138:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((mips_isa) >= (3)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 65:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && ((GENERATE_MULT3) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 61:
        case 58:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (which_alternative != 2))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 43:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) && (m16_nsimm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 42:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nuimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 47:
        case 30:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) && (m16_nsimm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_nsimm4_1 (operands[2], VOIDmode))) || (which_alternative == 2))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 29:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nuimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 41:
        case 28:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_nsimm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 19:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) && (m16_simm5_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 18:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm5_4 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 23:
        case 6:
	  extract_insn (insn);
	  if (! constrain_operands (reload_completed))
	    fatal_insn_not_found (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (((which_alternative == 0) && (m16_simm8_1 (operands[2], VOIDmode))) || (((which_alternative == 1) && (m16_simm4_1 (operands[2], VOIDmode))) || (which_alternative == 2))))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 5:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_uimm8_4 (operands[1], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case 17:
        case 4:
	  extract_insn (insn);
	  if ((((GENERATE_BRANCHLIKELY) != (0))) && (m16_simm8_8 (operands[0], VOIDmode)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        case -1:
	  if (GET_CODE (PATTERN (insn)) != ASM_INPUT
	      && asm_noperands (PATTERN (insn)) < 0)
	    fatal_insn_not_found (insn);
        case 504:
        case 503:
        case 502:
        case 501:
        case 500:
        case 499:
        case 498:
        case 497:
        case 496:
        case 495:
        case 494:
        case 422:
        case 420:
        case 417:
        case 415:
        case 408:
        case 407:
        case 406:
        case 405:
        case 398:
        case 396:
        case 393:
        case 391:
        case 384:
        case 383:
        case 382:
        case 381:
        case 375:
        case 374:
        case 368:
        case 367:
        case 366:
        case 365:
        case 337:
        case 336:
        case 335:
        case 322:
        case 318:
        case 307:
        case 303:
        case 292:
        case 290:
        case 289:
        case 259:
        case 258:
        case 257:
        case 256:
        case 227:
        case 226:
        case 206:
        case 176:
        case 175:
        case 174:
        case 167:
        case 164:
        case 163:
        case 156:
        case 150:
        case 149:
        case 147:
        case 142:
        case 141:
        case 137:
        case 136:
        case 135:
        case 134:
        case 131:
        case 128:
        case 127:
        case 124:
        case 123:
        case 122:
        case 121:
        case 120:
        case 118:
        case 116:
        case 114:
        case 112:
        case 110:
        case 108:
        case 106:
        case 101:
        case 99:
        case 97:
        case 95:
        case 93:
        case 92:
        case 91:
        case 90:
        case 89:
        case 88:
        case 87:
        case 86:
        case 85:
        case 84:
        case 83:
        case 82:
        case 81:
        case 80:
        case 79:
        case 78:
        case 77:
        case 76:
        case 75:
        case 74:
        case 71:
        case 70:
        case 69:
        case 68:
        case 64:
        case 60:
        case 56:
        case 55:
        case 52:
        case 49:
        case 46:
        case 40:
        case 27:
        case 25:
        case 24:
        case 22:
        case 16:
        case 3:
        case 1:
        case 0:
	  if (((GENERATE_BRANCHLIKELY) != (0)))
	    {
	      return 1;
	    }
	  else
	    {
	      return 0;
	    }

        default:
	  return 0;

      }
    default:
      abort ();
    }
}

static int
alu_5400_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 275:
    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || (which_alternative == 6)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))) || (which_alternative == 5)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 242:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 267:
    case 262:
    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 278:
    case 273:
    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 277:
    case 276:
    case 272:
    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      casenum = 2;
      break;

    case 124:
      casenum = 7;
      break;

    case 123:
      casenum = 8;
      break;

    case 122:
    case 93:
    case 91:
      casenum = 5;
      break;

    case 121:
    case 92:
    case 90:
      casenum = 6;
      break;

    case 89:
    case 87:
    case 85:
    case 83:
      casenum = 9;
      break;

    case 88:
    case 86:
    case 84:
    case 82:
      casenum = 10 /* 0xa */;
      break;

    case 53:
    case 52:
      casenum = 3;
      break;

    case 50:
    case 49:
      casenum = 4;
      break;

    case 523:
    case 522:
    case 504:
    case 503:
    case 502:
    case 501:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 426:
    case 424:
    case 423:
    case 422:
    case 421:
    case 420:
    case 418:
    case 417:
    case 416:
    case 415:
    case 412:
    case 410:
    case 408:
    case 407:
    case 406:
    case 405:
    case 402:
    case 400:
    case 399:
    case 398:
    case 397:
    case 396:
    case 394:
    case 393:
    case 392:
    case 391:
    case 388:
    case 386:
    case 384:
    case 383:
    case 382:
    case 381:
    case 378:
    case 376:
    case 375:
    case 374:
    case 371:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 338:
    case 337:
    case 336:
    case 335:
    case 332:
    case 329:
    case 328:
    case 323:
    case 322:
    case 319:
    case 318:
    case 315:
    case 312:
    case 311:
    case 308:
    case 307:
    case 304:
    case 303:
    case 300:
    case 297:
    case 296:
    case 293:
    case 292:
    case 280:
    case 227:
    case 226:
    case 176:
    case 175:
    case 174:
    case 173:
    case 172:
    case 171:
    case 170:
    case 169:
    case 168:
    case 165:
    case 164:
    case 163:
    case 161:
    case 160:
    case 159:
    case 157:
    case 156:
    case 153:
    case 152:
    case 150:
    case 149:
    case 147:
    case 145:
    case 144:
    case 142:
    case 141:
    case 138:
    case 137:
    case 134:
    case 133:
    case 131:
    case 47:
    case 46:
    case 43:
    case 42:
    case 41:
    case 40:
    case 37:
    case 34:
    case 30:
    case 29:
    case 28:
    case 27:
    case 23:
    case 22:
    case 19:
    case 18:
    case 17:
    case 16:
    case 13:
    case 10:
    case 6:
    case 5:
    case 4:
    case 3:
      casenum = 0;
      break;

    case 25:
    case 24:
    case 1:
    case 0:
      casenum = 1;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 11 /* 0xb */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 3;

    case 2:
      return 1;

    case 3:
      return 4;

    case 4:
      return 5;

    case 5:
      return 30 /* 0x1e */;

    case 6:
      return 58 /* 0x3a */;

    case 7:
      return 60 /* 0x3c */;

    case 8:
      return 120 /* 0x78 */;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 5;

    default:
      abort ();
    }
}

static int
alu_5400_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 275:
    case 271:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 249:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 4) || (which_alternative == 5))) || (which_alternative == 6)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || ((((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))) || (which_alternative == 5)))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 242:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 268:
    case 263:
    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if (((which_alternative == 0) || ((which_alternative == 1) || (which_alternative == 2))) || (((which_alternative != 0) && (which_alternative != 1)) && ((which_alternative == 3) || (which_alternative == 4))))
        {
	  casenum = 0;
        }
      else
        {
	  casenum = 11 /* 0xb */;
        }
      break;

    case 267:
    case 262:
    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 278:
    case 273:
    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 277:
    case 276:
    case 272:
    case 192:
    case 189:
    case 186:
    case 183:
    case 180:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      casenum = 2;
      break;

    case 124:
      casenum = 7;
      break;

    case 123:
      casenum = 8;
      break;

    case 122:
    case 93:
    case 91:
      casenum = 5;
      break;

    case 121:
    case 92:
    case 90:
      casenum = 6;
      break;

    case 89:
    case 87:
    case 85:
    case 83:
      casenum = 9;
      break;

    case 88:
    case 86:
    case 84:
    case 82:
      casenum = 10 /* 0xa */;
      break;

    case 53:
    case 52:
      casenum = 3;
      break;

    case 50:
    case 49:
      casenum = 4;
      break;

    case 523:
    case 522:
    case 504:
    case 503:
    case 502:
    case 501:
    case 500:
    case 499:
    case 498:
    case 497:
    case 496:
    case 495:
    case 494:
    case 426:
    case 424:
    case 423:
    case 422:
    case 421:
    case 420:
    case 418:
    case 417:
    case 416:
    case 415:
    case 412:
    case 410:
    case 408:
    case 407:
    case 406:
    case 405:
    case 402:
    case 400:
    case 399:
    case 398:
    case 397:
    case 396:
    case 394:
    case 393:
    case 392:
    case 391:
    case 388:
    case 386:
    case 384:
    case 383:
    case 382:
    case 381:
    case 378:
    case 376:
    case 375:
    case 374:
    case 371:
    case 369:
    case 368:
    case 367:
    case 366:
    case 365:
    case 338:
    case 337:
    case 336:
    case 335:
    case 332:
    case 329:
    case 328:
    case 323:
    case 322:
    case 319:
    case 318:
    case 315:
    case 312:
    case 311:
    case 308:
    case 307:
    case 304:
    case 303:
    case 300:
    case 297:
    case 296:
    case 293:
    case 292:
    case 280:
    case 227:
    case 226:
    case 176:
    case 175:
    case 174:
    case 173:
    case 172:
    case 171:
    case 170:
    case 169:
    case 168:
    case 165:
    case 164:
    case 163:
    case 161:
    case 160:
    case 159:
    case 157:
    case 156:
    case 153:
    case 152:
    case 150:
    case 149:
    case 147:
    case 145:
    case 144:
    case 142:
    case 141:
    case 138:
    case 137:
    case 134:
    case 133:
    case 131:
    case 47:
    case 46:
    case 43:
    case 42:
    case 41:
    case 40:
    case 37:
    case 34:
    case 30:
    case 29:
    case 28:
    case 27:
    case 23:
    case 22:
    case 19:
    case 18:
    case 17:
    case 16:
    case 13:
    case 10:
    case 6:
    case 5:
    case 4:
    case 3:
      casenum = 0;
      break;

    case 25:
    case 24:
    case 1:
    case 0:
      casenum = 1;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 11 /* 0xb */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 1;

    case 1:
      return 3;

    case 2:
      return 1;

    case 3:
      return 4;

    case 4:
      return 5;

    case 5:
      return 30 /* 0x1e */;

    case 6:
      return 58 /* 0x3a */;

    case 7:
      return 60 /* 0x3c */;

    case 8:
      return 120 /* 0x78 */;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 5;

    default:
      abort ();
    }
}

static int
imuldiv_unit_blockage (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 268:
    case 263:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 267:
    case 262:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      casenum = 27 /* 0x1b */;
      break;

    case 120:
    case 116:
    case 112:
    case 108:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 25 /* 0x19 */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 118:
    case 114:
    case 110:
    case 106:
    case 101:
    case 99:
    case 97:
    case 95:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 22 /* 0x16 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 78:
    case 77:
    case 65:
    case 64:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 7;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 9;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 12 /* 0xc */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 11 /* 0xb */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 81:
    case 80:
    case 79:
    case 76:
    case 75:
    case 74:
    case 71:
    case 70:
    case 69:
    case 68:
    case 60:
    case 57:
    case 56:
    case 55:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 11 /* 0xb */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 53:
    case 52:
      casenum = 28 /* 0x1c */;
      break;

    case 50:
    case 49:
      casenum = 29 /* 0x1d */;
      break;

    case 25:
    case 24:
    case 1:
    case 0:
      casenum = 26 /* 0x1a */;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 31 /* 0x1f */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 3;

    case 1:
      return 17 /* 0x11 */;

    case 2:
      return 5;

    case 3:
      return 12 /* 0xc */;

    case 4:
      return 10 /* 0xa */;

    case 5:
      return 4;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 5;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 1;

    case 12:
      return 2;

    case 13:
      return 38 /* 0x26 */;

    case 14:
      return 35 /* 0x23 */;

    case 15:
      return 42 /* 0x2a */;

    case 16:
      return 36 /* 0x24 */;

    case 17:
      return 69 /* 0x45 */;

    case 18:
      return 35 /* 0x23 */;

    case 19:
      return 67 /* 0x43 */;

    case 20:
      return 37 /* 0x25 */;

    case 21:
      return 69 /* 0x45 */;

    case 22:
      return 36 /* 0x24 */;

    case 23:
      return 68 /* 0x44 */;

    case 24:
      return 34 /* 0x22 */;

    case 25:
      return 66 /* 0x42 */;

    case 26:
      return 3;

    case 27:
      return 1;

    case 28:
      return 5;

    case 29:
      return 8;

    case 31:
      return 58 /* 0x3a */;

    default:
      abort ();
    }
}

static int
imuldiv_unit_conflict_cost (executing_insn, candidate_insn)
     rtx executing_insn;
     rtx candidate_insn;
{
  rtx insn;
  int casenum;

  insn = executing_insn;
  switch (recog_memoized (insn))
    {
    case 268:
    case 263:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 266:
    case 261:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 243:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 242:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 241:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 234:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 267:
    case 262:
    case 233:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 231:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 230:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 195:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      casenum = 0;
      break;

    case 437:
    case 436:
    case 435:
    case 434:
    case 433:
    case 432:
    case 431:
    case 430:
    case 429:
    case 428:
    case 136:
    case 135:
    case 128:
    case 127:
      casenum = 27 /* 0x1b */;
      break;

    case 120:
    case 116:
    case 112:
    case 108:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 19 /* 0x13 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 21 /* 0x15 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 23 /* 0x17 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 25 /* 0x19 */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 118:
    case 114:
    case 110:
    case 106:
    case 101:
    case 99:
    case 97:
    case 95:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 13 /* 0xd */;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 14 /* 0xe */;
        }
      else if (((mips_cpu_attr) == (CPU_R4600)))
        {
	  casenum = 15 /* 0xf */;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 16 /* 0x10 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4000)))
        {
	  casenum = 17 /* 0x11 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 18 /* 0x12 */;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 20 /* 0x14 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 22 /* 0x16 */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 24 /* 0x18 */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 78:
    case 77:
    case 65:
    case 64:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 7;
        }
      else if (((mips_cpu_attr) == (CPU_R4300)))
        {
	  casenum = 9;
        }
      else if (((mips_cpu_attr) == (CPU_R5000)))
        {
	  casenum = 10 /* 0xa */;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 12 /* 0xc */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 61:
    case 58:
      extract_insn (insn);
      if (! constrain_operands (reload_completed))
        fatal_insn_not_found (insn);
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 11 /* 0xb */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 81:
    case 80:
    case 79:
    case 76:
    case 75:
    case 74:
    case 71:
    case 70:
    case 69:
    case 68:
    case 60:
    case 57:
    case 56:
    case 55:
      if ((! (((mips_cpu_attr) == (CPU_R3000)))) && ((! (((mips_cpu_attr) == (CPU_R3900)))) && ((! (((mips_cpu_attr) == (CPU_R4000)))) && ((! (((mips_cpu_attr) == (CPU_R4600)))) && ((! (((mips_cpu_attr) == (CPU_R4650)))) && ((! (((mips_cpu_attr) == (CPU_R4100)))) && ((! (((mips_cpu_attr) == (CPU_R4300)))) && ((! (((mips_cpu_attr) == (CPU_R5000)))) && (! (((mips_cpu_attr) == (CPU_R5400))))))))))))
        {
	  casenum = 1;
        }
      else if ((((mips_cpu_attr) == (CPU_R3000))) || (((mips_cpu_attr) == (CPU_R3900))))
        {
	  casenum = 3;
        }
      else if ((((mips_cpu_attr) == (CPU_R4000))) || (((mips_cpu_attr) == (CPU_R4600))))
        {
	  casenum = 4;
        }
      else if (((mips_cpu_attr) == (CPU_R4650)))
        {
	  casenum = 5;
        }
      else if (((mips_cpu_attr) == (CPU_R4100)))
        {
	  casenum = 6;
        }
      else if ((((mips_cpu_attr) == (CPU_R4300))) || (((mips_cpu_attr) == (CPU_R5000))))
        {
	  casenum = 8;
        }
      else if (((mips_cpu_attr) == (CPU_R5400)))
        {
	  casenum = 11 /* 0xb */;
        }
      else
        {
	  casenum = 31 /* 0x1f */;
        }
      break;

    case 53:
    case 52:
      casenum = 28 /* 0x1c */;
      break;

    case 50:
    case 49:
      casenum = 29 /* 0x1d */;
      break;

    case 25:
    case 24:
    case 1:
    case 0:
      casenum = 26 /* 0x1a */;
      break;

    case -1:
      if (GET_CODE (PATTERN (insn)) != ASM_INPUT
          && asm_noperands (PATTERN (insn)) < 0)
        fatal_insn_not_found (insn);
    default:
      casenum = 31 /* 0x1f */;
      break;

    }

  insn = candidate_insn;
  switch (casenum)
    {
    case 0:
      return 3;

    case 1:
      return 17 /* 0x11 */;

    case 2:
      return 5;

    case 3:
      return 12 /* 0xc */;

    case 4:
      return 10 /* 0xa */;

    case 5:
      return 4;

    case 6:
      return 1;

    case 7:
      return 4;

    case 8:
      return 5;

    case 9:
      return 8;

    case 10:
      return 9;

    case 11:
      return 1;

    case 12:
      return 2;

    case 13:
      return 38 /* 0x26 */;

    case 14:
      return 35 /* 0x23 */;

    case 15:
      return 42 /* 0x2a */;

    case 16:
      return 36 /* 0x24 */;

    case 17:
      return 69 /* 0x45 */;

    case 18:
      return 35 /* 0x23 */;

    case 19:
      return 67 /* 0x43 */;

    case 20:
      return 37 /* 0x25 */;

    case 21:
      return 69 /* 0x45 */;

    case 22:
      return 36 /* 0x24 */;

    case 23:
      return 68 /* 0x44 */;

    case 24:
      return 34 /* 0x22 */;

    case 25:
      return 66 /* 0x42 */;

    case 26:
      return 3;

    case 27:
      return 1;

    case 28:
      return 5;

    case 29:
      return 8;

    case 31:
      return 58 /* 0x3a */;

    default:
      abort ();
    }
}

struct function_unit_desc function_units[] = {
  {"memory", 1, 1, 0, 1, 1, memory_unit_ready_cost, 0, 1, 0, 0}, 
  {"imuldiv", 2, 1, 0, 0, 69, imuldiv_unit_ready_cost, imuldiv_unit_conflict_cost, 69, imuldiv_unit_blockage_range, imuldiv_unit_blockage}, 
  {"adder", 4, 1, 1, 1, 1, adder_unit_ready_cost, 0, 1, 0, 0}, 
  {"mult", 8, 1, 1, 1, 1, mult_unit_ready_cost, 0, 1, 0, 0}, 
  {"divide", 16, 1, 1, 1, 1, divide_unit_ready_cost, 0, 1, 0, 0}, 
  {"alu_5400", 32, 2, 0, 0, 120, alu_5400_unit_ready_cost, alu_5400_unit_conflict_cost, 120, alu_5400_unit_blockage_range, alu_5400_unit_blockage}, 
};

int
const_num_delay_slots (insn)
     rtx insn;
{
  switch (recog_memoized (insn))
    {
    default:
      return 1;
    }
}

int length_unit_log = 0;
