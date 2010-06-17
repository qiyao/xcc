/* 
   Copyright (C) 2003-2006 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "basic-block.h"
#include "toplev.h"

/* Advance the argument to the next argument position.  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  int words, max;
  int *arg_words;

  arg_words = &cum->arg_words;
  max = MAX_ARGS_IN_REGISTERS;

  words = (((mode != BLKmode)
	    ? GET_MODE_SIZE (mode)
	    : int_size_in_bytes (type)) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if ((*arg_words + words > max) && (*arg_words < max))
    *arg_words = max;

  *arg_words += words;
}


/* Return an RTL expression containing the register for the given mode,
   or 0 if the argument is to be passed on the stack.  */

struct rtx_def *
function_arg (cum, mode, type, named, incoming_p)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
     int incoming_p;		/* computing the incoming registers? */
{
  int regbase, words, max;
  int *arg_words;
  int regno;

  arg_words = &cum->arg_words;
  regbase = (incoming_p ? GP_ARG_FIRST : GP_OUTGOING_ARG_FIRST);
  max = MAX_ARGS_IN_REGISTERS;

  words = (((mode != BLKmode)
	    ? GET_MODE_SIZE (mode)
	    : int_size_in_bytes (type)) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (type && (TYPE_ALIGN (type) > BITS_PER_WORD))
    *arg_words += (*arg_words & 1);

  if (*arg_words + words > max)
    return (rtx)0;

  regno = regbase + *arg_words;

  return gen_rtx (REG, ((mode == BLKmode) ? TYPE_MODE(type) : mode), regno);
}


/* We need this to exists so __builtin_saveregs is expanded, but we
   don't have to do anything here. */

struct rtx_def *
xtensa_builtin_saveregs ()
{
  return const0_rtx;
}

void
override_options ()
{
  xt_endian = TARGET_BIG_ENDIAN;
  xt_density = TARGET_DENSITY;
  xt_mac16 = TARGET_MAC16;    
  xt_mul16 = TARGET_MUL16;    
  xt_mul32 = TARGET_MUL32;    
  xt_mul32h = TARGET_MUL32H;
  xt_div32 = TARGET_DIV32;
  xt_nsa = TARGET_NSA;
  xt_minmax = TARGET_MINMAX;    
  xt_sext = TARGET_SEXT; 
  xt_booleans = TARGET_BOOLEANS;
  xt_hard_float = TARGET_HARD_FLOAT;
  xt_hard_float_div = TARGET_HARD_FLOAT_DIV;
  xt_hard_float_recip = TARGET_HARD_FLOAT_RECIP;
  xt_hard_float_sqrt = TARGET_HARD_FLOAT_SQRT;
  xt_hard_float_rsqrt = TARGET_HARD_FLOAT_RSQRT;
  xt_clamps = TARGET_CLAMPS;
  xt_zero_cost_loop = TARGET_ZERO_COST_LOOP;
  xt_zero_init_data = TARGET_ZERO_INIT_DATA;
  xt_fused_madd = !TARGET_NO_FUSED_MADD;
  xt_serialize_volatile = TARGET_SERIALIZE_VOLATILE;
  xt_const16 = TARGET_CONST16;
  xt_l32r = TARGET_L32R;
  xt_addx = TARGET_ADDX;
  xt_brt = TARGET_BRT;


  if (xt_icache_line_bytes_string != NULL)
    xt_icache_line_bytes = atoi (xt_icache_line_bytes_string);
  else
    xt_icache_line_bytes = 0;

  if (xt_dcache_line_bytes_string != NULL)
    xt_dcache_line_bytes = atoi (xt_dcache_line_bytes_string);
  else
    xt_dcache_line_bytes = 0;
}


void
xtensa_init_builtins (void)
{
  tree ftype;

  ftype = build_function_type (unsigned_intDI_type_node,
			       tree_cons (NULL_TREE, unsigned_intSI_type_node,
			       tree_cons (NULL_TREE, unsigned_intSI_type_node,
			       tree_cons (NULL_TREE, void_type_node,
					  NULL_TREE))));


  builtin_function ("__builtin_umulsidi3", ftype,
		    XTENSA_BUILTIN_UMULSIDI3, BUILT_IN_MD, "__umulsidi3");
}


rtx
xtensa_expand_builtin (tree exp, rtx target,
		       rtx subtarget ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       int ignore)
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  /* The umulsidi3 builtin is just a mechanism to avoid calling the real
     __umulsidi3 function when the Xtensa configuration can directly
     implement it.  It doesn't matter what RTL is generated for this.  */
  if (fcode == XTENSA_BUILTIN_UMULSIDI3)
    return expand_call (exp, target, ignore);

  error ("bad builtin code");
  return NULL_RTX;
}



