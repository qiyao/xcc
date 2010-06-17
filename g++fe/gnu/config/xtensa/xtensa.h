/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Definitions of target machine GNU compiler.  Xtensa version.

This file is part of GNU CC.

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

extern struct rtx_def *	function_arg ();
extern void function_arg_advance ();
extern struct rtx_def *	xtensa_builtin_saveregs ();

/* Run-time target specifications */

/* This declaration should be present.  */
extern int target_flags;

/* Run-time compilation parameters selecting different hardware subsets.  */

#define MASK_BIG_ENDIAN		0x00000001	/* big or little endian */
#define MASK_DENSITY            0x00000002      /* code density option */
#define MASK_MAC16              0x00000004      /* MAC16 option */
#define MASK_MUL16              0x00000008      /* 16-bit integer multiply */
#define MASK_MUL32              0x00000010      /* integer multiply/divide */
#define MASK_DIV32              0x00000020      /* integer multiply/divide */
#define MASK_NSA                0x00000040      /* nsa instruction option */
#define MASK_MINMAX             0x00000080      /* min/max instructions */
#define MASK_SEXT               0x00000100      /* sign extend insn option */
#define MASK_BOOLEANS           0x00000200      /* boolean register option */
#define MASK_HARD_FLOAT         0x00000400      /* floating-point option */
#define MASK_HARD_FLOAT_DIV     0x00000800      /* floating-point divide */
#define MASK_HARD_FLOAT_RECIP   0x00001000      /* floating-point reciprocal */
#define MASK_HARD_FLOAT_SQRT    0x00002000      /* floating-point sqrt */
#define MASK_HARD_FLOAT_RSQRT   0x00004000      /* floating-point recip sqrt */
#define MASK_NO_FUSED_MADD      0x00008000      /* avoid f-p mul/add */
#define MASK_SERIALIZE_VOLATILE 0x00020000      /* serialize volatile refs */
#define MASK_CONST16		0x00040000      /* use const16 */
#define MASK_CLAMPS             0x00080000      /* clamps */
#define MASK_ZERO_COST_LOOP     0x00100000      /* loop instructions */
#define MASK_ZERO_INIT_DATA     0x00200000      /* zero initialized data */
#define MASK_L32R               0x00400000      /* use l32r */
#define MASK_ABS                0x00800000      /* use abs */
#define MASK_BRT                0x01000000      /* use predicted branches */
#define MASK_ADDX               0x02000000      /* use ADDX */
#define MASK_MUL32H             0x04000000      /* 32-bit multiply high */
 
#define MASK_LSP                0x00000001
#define MASK_ICACHE_LINE        0x00000002
#define MASK_DCACHE_LINE        0x00000004
#define MASK_ISA_BASE_DLL       0x00000008
#define MASK_ISA_TIE_DLL        0x00000010
#define MASK_ABI                0x00000080


/* Macros used in the machine description to test the flags.  */

#define TARGET_BIG_ENDIAN	(target_flags & MASK_BIG_ENDIAN)
#define TARGET_DENSITY          (target_flags & MASK_DENSITY)
#define TARGET_MAC16            (target_flags & MASK_MAC16)
#define TARGET_MUL16            (target_flags & MASK_MUL16)
#define TARGET_MUL32            (target_flags & MASK_MUL32)
#define TARGET_MUL32H           (target_flags & MASK_MUL32H)
#define TARGET_DIV32            (target_flags & MASK_DIV32)
#define TARGET_NSA              (target_flags & MASK_NSA)
#define TARGET_MINMAX           (target_flags & MASK_MINMAX)
#define TARGET_SEXT             (target_flags & MASK_SEXT)
#define TARGET_BOOLEANS         (target_flags & MASK_BOOLEANS)
#define TARGET_HARD_FLOAT       (target_flags & MASK_HARD_FLOAT)
#define TARGET_HARD_FLOAT_DIV   (target_flags & MASK_HARD_FLOAT_DIV)
#define TARGET_HARD_FLOAT_RECIP (target_flags & MASK_HARD_FLOAT_RECIP)
#define TARGET_HARD_FLOAT_SQRT  (target_flags & MASK_HARD_FLOAT_SQRT)
#define TARGET_HARD_FLOAT_RSQRT (target_flags & MASK_HARD_FLOAT_RSQRT)
#define TARGET_NO_FUSED_MADD    (target_flags & MASK_NO_FUSED_MADD)
#define TARGET_SERIALIZE_VOLATILE (target_flags & MASK_SERIALIZE_VOLATILE)
#define TARGET_CLAMPS           (target_flags & MASK_CLAMPS)
#define TARGET_ZERO_COST_LOOP   (target_flags & MASK_ZERO_COST_LOOP)
#define TARGET_ZERO_INIT_DATA   (target_flags & MASK_ZERO_INIT_DATA)
#define TARGET_CONST16          (target_flags & MASK_CONST16)
#define TARGET_L32R             (target_flags & MASK_L32R)
#define TARGET_ABS              (target_flags & MASK_ABS)
#define TARGET_BRT              (target_flags & MASK_BRT)
#define TARGET_ADDX             (target_flags & MASK_ADDX)
 

/* Default target_flags if no switches are specified  */

#define TARGET_DEFAULT 0

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES                                                 \
{                                                                       \
  {"big-endian",                MASK_BIG_ENDIAN},                       \
  {"little-endian",             -MASK_BIG_ENDIAN},                      \
  {"density",                   MASK_DENSITY},                          \
  {"no-density",                -MASK_DENSITY},                         \
  {"mac16",                     MASK_MAC16},                            \
  {"no-mac16",                  -MASK_MAC16},                           \
  {"mul16",                     MASK_MUL16},                            \
  {"no-mul16",                  -MASK_MUL16},                           \
  {"mul32",                     MASK_MUL32},                            \
  {"no-mul32",                  -MASK_MUL32},                           \
  {"mul32h",                    MASK_MUL32H},                           \
  {"no-mul32h",                 -MASK_MUL32H},                          \
  {"div32",                     MASK_DIV32},                            \
  {"no-div32",                  -MASK_DIV32},                           \
  {"nsa",                       MASK_NSA},                              \
  {"no-nsa",                    -MASK_NSA},                             \
  {"minmax",                    MASK_MINMAX},                           \
  {"no-minmax",                 -MASK_MINMAX},                          \
  {"sext",                      MASK_SEXT},                             \
  {"no-sext",                   -MASK_SEXT},                            \
  {"const16",                   MASK_CONST16},                          \
  {"no-const16",                -MASK_CONST16},                         \
  {"l32r",                      MASK_L32R},                             \
  {"no-l32r",                   -MASK_L32R},                            \
  {"abs",                       MASK_ABS},                              \
  {"no-abs",                    -MASK_ABS},                             \
  {"predicted-branches",        MASK_BRT},                              \
  {"no-predicted-branches",     -MASK_BRT},                             \
  {"addx",                      MASK_ADDX},                             \
  {"no-addx",                   -MASK_ADDX},                            \
  {"booleans",                  MASK_BOOLEANS},                         \
  {"no-booleans",               -MASK_BOOLEANS},                        \
  {"hard-float",                MASK_HARD_FLOAT},                       \
  {"soft-float",                -MASK_HARD_FLOAT},                      \
  {"hard-float-div",            MASK_HARD_FLOAT_DIV},                   \
  {"no-hard-float-div",         -MASK_HARD_FLOAT_DIV},                  \
  {"hard-float-recip",          MASK_HARD_FLOAT_RECIP},                 \
  {"no-hard-float-recip",       -MASK_HARD_FLOAT_RECIP},                \
  {"hard-float-sqrt",           MASK_HARD_FLOAT_SQRT},                  \
  {"no-hard-float-sqrt",        -MASK_HARD_FLOAT_SQRT},                 \
  {"hard-float-rsqrt",          MASK_HARD_FLOAT_RSQRT},                 \
  {"no-hard-float-rsqrt",       -MASK_HARD_FLOAT_RSQRT},                \
  {"clamps",                    MASK_CLAMPS},                           \
  {"no-clamps",                 -MASK_CLAMPS},                          \
  {"zero-cost-loop",            MASK_ZERO_COST_LOOP},                   \
  {"no-zero-cost-loop",         -MASK_ZERO_COST_LOOP},                  \
  {"zero-init-data",            MASK_ZERO_INIT_DATA},                   \
  {"no-zero-init-data",         -MASK_ZERO_INIT_DATA},                  \
  {"no-fused-madd",             MASK_NO_FUSED_MADD},                    \
  {"fused-madd",                -MASK_NO_FUSED_MADD},                   \
  {"serialize-volatile",        MASK_SERIALIZE_VOLATILE},               \
  {"no-serialize-volatile",     -MASK_SERIALIZE_VOLATILE},              \
  {"text-section-literals",     0},                                     \
  {"no-text-section-literals",  0},                                     \
  {"target-align",              0},                                     \
  {"no-target-align",           0},                                     \
  {"longcalls",                 0},                                     \
  {"no-longcalls",              0},                                     \
  {"",                          TARGET_DEFAULT}                         \
}

#define TARGET_OPTIONS							\
{                                                                       \
  {"icache-line-bytes=",	&xt_icache_line_bytes_string, NULL},   	\
  {"dcache-line-bytes=",	&xt_dcache_line_bytes_string, NULL},	\
  {"isa-dlls=",			&xt_isa_dlls_string, NULL},		\
  {"xtie-dlls=",		&xt_xtie_dlls_string, NULL},		\
  {"abi=",		        &xt_abi_string, NULL},		        \
  {"stack-alignment=",	        &xt_stack_alignment_string, NULL}       \
}

#define OVERRIDE_OPTIONS override_options()           


/* This macro is a C statement to print on `stderr' a string describing the
   particular machine description choice.  */

#define TARGET_VERSION fprintf (stderr, " (xtensa)");

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP


/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
*/
#define BITS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant byte of a word is the lowest numbered. */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant word of a multiword number is the lowest. */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __XTENSA_EB__
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type 'int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32
#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4
#define MIN_UNITS_PER_WORD 4

/* Width of a floating point register.  */
#define UNITS_PER_FPREG 4

/* A C expression for the size in bits of the type 'int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE 32
#define MAX_INT_TYPE_SIZE 32

/* Tell the preprocessor the maximum size of wchar_t.  */
#ifndef MAX_WCHAR_TYPE_SIZE
#ifndef WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE MAX_INT_TYPE_SIZE
#endif
#endif

/* A C expression for the size in bits of the type 'short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type 'long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE 32
#define MAX_LONG_TYPE_SIZE 32

/* A C expression for the size in bits of the type 'long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type 'char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */
#define CHAR_TYPE_SIZE BITS_PER_UNIT

/* A C expression for the size in bits of the type 'float' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type 'double' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type 'long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width in bits of a pointer.
   See also the macro 'Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after 'int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Align stack frames on 128 bits. */
#define STACK_BOUNDARY 128

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 128

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers
   handle alignment of bitfields and the structures that contain
   them.

   The behavior is that the type written for a bitfield ('int',
   'short', or other integer type) imposes an alignment for the
   entire structure, as if the structure really did contain an
   ordinary field of that type.  In addition, the bitfield is placed
   within the structure so that it would fit within such a field,
   not crossing a boundary for it.

   Thus, on most machines, a bitfield whose type is written as 'int'
   would not cross a four-byte boundary, and would force four-byte
   alignment for the whole structure.  (The alignment used may not
   be four bytes; it is controlled by the other alignment
   parameters.)

   If the macro is defined, its definition should be a C expression;
   a nonzero value for the expression enables this behavior.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that 'strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD						\
	? BITS_PER_WORD							\
	: (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that 'strcpy' calls
   that copy constants to character arrays can be done inline.  */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))


/* Define this macro if an argument declared as 'char' or 'short' in a
   prototype should actually be passed as an 'int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines.  XTENSA - This must be zero since
   be seems to expect that (see pr2539). */
#define PROMOTE_PROTOTYPES 0

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND


/* An integer expression for the size in bits of the largest integer machine
   mode that should actually be used.  */

/* Allow pairs of registers to be used, which is the intent of the default.  */
#define MAX_FIXED_MODE_SIZE GET_MODE_BITSIZE (TImode)

/* A code distinguishing the floating point format of the target machine.  */
#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* GNU CC supports two ways of implementing C++ vtables: traditional or with
   so-called "thunks".  The flag `-fvtable-thunk' chooses between them.  Define
   this macro to be a C expression for the default value of that flag.  If
   `DEFAULT_VTABLE_THUNKS' is 0, GNU CC uses the traditional implementation by
   default.  The "thunk" implementation is more efficient (especially if you
   have provided an implementation of `ASM_OUTPUT_MI_THUNK', but is not binary
   compatible with code compiled using the traditional implementation.  If you
   are writing a new ports, define `DEFAULT_VTABLE_THUNKS' to 1.

   If you do not define this macro, the default for `-fvtable-thunk' is 0.  */
#define DEFAULT_VTABLE_THUNKS 1

/* Define this as 1 if 'char' should by default be signed; else as 0.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 0
#endif

/* Define results of standard character escape sequences.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015


/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   0 - 15	ar[0] - ar[15]
   16 - 31	fr[0] - fr[15]

   Our first pseudo register value is much higher, because we want to
   leave room for user defined registers.
*/

/* 
 * this is the max number of dedicated registers allowed. It must be
 * greater than the number of hardware registers. This is checked in 
 * wfe_misc.cxx. It used to be based on TI_ISA_REGISTER_MAX and 
 * TI_ISA_MAX_REGISTER, however, doing so adds some complexity that is
 * unnecessary. It just needs to be greater than those two multiplied 
 * together.
 */
#define FIRST_PSEUDO_REGISTER (1024*32)

/* the real max number of all dedicated registers */
extern int first_pseudo_register;

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator. */

#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS						\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
}

#define CONDITIONAL_REGISTER_USAGE                                      \
  if (strcmp(xt_abi_string, "call0") == 0)				\
    {									\
      call_used_regs[0] = 1;						\
      call_used_regs[1] = 1;						\
      call_used_regs[2] = 1;						\
      call_used_regs[3] = 1;						\
      call_used_regs[4] = 1;						\
      call_used_regs[5] = 1;						\
      call_used_regs[6] = 1;						\
      call_used_regs[7] = 1;						\
      call_used_regs[8] = 1;						\
      call_used_regs[9] = 1;						\
      call_used_regs[10] = 1;						\
      call_used_regs[11] = 1;						\
      call_used_regs[12] = 0;						\
      call_used_regs[13] = 0;						\
      call_used_regs[14] = 0;						\
      call_used_regs[15] = 0;						\
    }									

/* 16 ar registers */
#define GP_REG_FIRST 0
#define GP_REG_LAST  15
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

/* 16 fp registers */
#define FR_REG_FIRST 16
#define FR_REG_LAST  31
#define FR_REG_NUM   (FR_REG_LAST - FR_REG_FIRST + 1)

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define FR_REG_P(REGNO) ((unsigned) ((REGNO) - FR_REG_FIRST) < FR_REG_NUM)

/* if this is changed, the hardwired values for CALL insns in xtensa.md
   also need to be changed */
#define WINDOW_SIZE 8

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT)						\
    ((GP_REG_P(OUT) &&							\
      ((unsigned) ((OUT) - GP_REG_FIRST) >= WINDOW_SIZE)) ?		\
     (OUT) - WINDOW_SIZE : (OUT))

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN)						\
    ((GP_REG_P(IN) &&							\
      ((unsigned) ((IN) - GP_REG_FIRST) < WINDOW_SIZE)) ?		\
     (IN) + WINDOW_SIZE : (IN))


/* How Values Fit in Registers */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers. */

#define HARD_REGNO_NREGS(REGNO, MODE) 1

#define HARD_REGNO_MODE_OK(REGNO, MODE)	1

#define MODES_TIEABLE_P(MODE1, MODE2) 1


/* Register Classes */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.  */

enum reg_class
{
  NO_REGS,
  FR_REGS,
  GP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GP_REGS

/* The number of distinct register classes.  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES \
{ "NO_REGS", "FR_REGS", "GP_REGS", "ALL_REGS" }

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.  */
#define REG_CLASS_CONTENTS \
{ 							\
  { 0x00000000 } ,                                      \
  /* FR_REGS.  */					\
  { 0xffff0000 } ,                                      \
  /* GP_REGS.  */					\
  { 0x0000ffff } ,                                      \
  /* ALL_REGS.  */					\
  { 0xffff0000 } ,                                      \
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */
/* The NO_REGS case is primarily for the benefit of rws_access_reg, which
   may call here with private (invalid) register numbers, such as
   REG_VOLATILE.  */
#define REGNO_REG_CLASS(REGNO) \
(FR_REG_P (REGNO) ? FR_REGS	\
 : GP_REG_P (REGNO) ? GP_REGS	\
 : NO_REGS)

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS NO_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.  */

#define REG_CLASS_FROM_LETTER(CHAR) \
((CHAR) == 'f' ? FR_REGS		\
 : (CHAR) == 'a' ? GP_REGS		\
 : NO_REGS)


#define CONST_OK_FOR_LETTER_P(VALUE, C) 1
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) (0)

#define GP_REG_OR_PSEUDO_STRICT_P(regno) \
  GP_REG_P((regno < FIRST_PSEUDO_REGISTER) ? regno : reg_renumber[regno])

#define GP_REG_OR_PSEUDO_NONSTRICT_P(regno) \
  (((regno) >= FIRST_PSEUDO_REGISTER) || (GP_REG_P (regno)))

#define REGISTER_NAMES							\
{									\
  "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",                       \
  "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15",                 \
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",                       \
  "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",                 \
}

#define ADDITIONAL_REGISTER_NAMES \
{                                 \
  { "sp", 1 }                     \
}

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not
   known until the compilation is completed.  In such a case, a
   separate hard register must be used for the argument pointer. 
   This register can be eliminated by replacing it with either the
   frame pointer or the argument pointer, depending on whether or not
   the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{{ FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM}}

/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if 'ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define CAN_ELIMINATE(FROM, TO) 0

/* This macro is similar to 'INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if 'ELIMINABLE_REGS' is
   defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) 0

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard reg.  */
#define REGNO_OK_FOR_BASE_P(regno)	GP_REG_OR_PSEUDO_STRICT_P (regno)

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard reg.  */
#define REGNO_OK_FOR_INDEX_P(NUM) 0

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.
   This is closely related to the macro `HARD_REGNO_NREGS'.  */

#define CLASS_MAX_NREGS(CLASS, MODE) 1


/* Basic Stack Layout */

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated. */
#define STARTING_FRAME_OFFSET current_function_outgoing_args_size

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current
   frame, after the prologue.  FRAMEADDR is the frame pointer of the
   COUNT frame, or the frame pointer of the COUNT - 1 frame if
   'RETURN_ADDR_IN_PREVIOUS_FRAME' is defined.

   The 2 most-significant bits of the return address on Xtensa hold
   the register window size.  To get the real return address, these bits
   must be masked off and replaced with the high bits from the current
   PC.  Since it is unclear how the __builtin_return_address function
   is used, the current code does not do this masking and simply returns
   the raw return address from the a0 register. */

#define RETURN_ADDR_RTX(count, frame)					\
  ((count == -1) ? gen_rtx (REG, Pmode, 0)				\
   : gen_rtx (MEM, Pmode, memory_address				\
	      (Pmode, plus_constant (frame, -4 * UNITS_PER_WORD))))

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.  This enables DWARF2
   unwind info for C++ EH.  */
/*#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (VOIDmode, GP_REG_FIRST)*/


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */

#define STACK_POINTER_REGNUM (GP_REG_FIRST + 1)

/* Offset from the stack pointer to the first available location.  */
#define STACK_POINTER_OFFSET FIRST_PARM_OFFSET(0)


/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */

#define FRAME_POINTER_REGNUM (GP_REG_FIRST + 1)

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  */
/* r0 won't otherwise be used, so put the always eliminated argument pointer
   in it.  */
#define ARG_POINTER_REGNUM STACK_POINTER_REGNUM

/* The register number for the return address pointer register, which holds
   the address of where the return address was stored on the stack.  */

#define RETURN_ADDRESS_POINTER_REGNUM (GP_REG_FIRST)

/* If the static chain is passed in memory, these macros provide rtx
   giving 'mem' expressions that denote where they are stored.
   'STATIC_CHAIN' and 'STATIC_CHAIN_INCOMING' give the locations as
   seen by the calling and called functions, respectively.  Often the
   former will be at an offset from the stack pointer and the latter
   at an offset from the frame pointer. */

#define STATIC_CHAIN							\
  gen_rtx (MEM, Pmode, plus_constant (stack_pointer_rtx, -5 * UNITS_PER_WORD))

#define STATIC_CHAIN_INCOMING						\
  gen_rtx (MEM, Pmode, plus_constant (arg_pointer_rtx, -5 * UNITS_PER_WORD))


/* Eliminating the Frame Pointer and the Arg Pointer */

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.  */

#define FRAME_POINTER_REQUIRED 0


/* Passing Function Arguments on the Stack */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  */

#define ACCUMULATE_OUTGOING_ARGS 1

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.  */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

/* Function Arguments in Registers */

#define MAX_ARGS_IN_REGISTERS 6
#define MAX_INT_RETURN_SLOTS 4
#define GP_ARG_FIRST (GP_REG_FIRST + 2)
#define GP_ARG_LAST  (GP_REG_FIRST + 7)
#define GP_OUTGOING_ARG_FIRST (GP_REG_FIRST + 2 + WINDOW_SIZE)
#define GP_OUTGOING_ARG_LAST  (GP_REG_FIRST + 7 + WINDOW_SIZE)
#define GP_RETURN (GP_REG_FIRST + 2 + WINDOW_SIZE)
#define GP_OUTGOING_RETURN (GP_REG_FIRST + 2)

/* A C expression that controls whether a function argument is passed in a
   register, and which register.  */

#define FUNCTION_ARG_REGNO_P(N)						\
    ((N) >= GP_OUTGOING_ARG_FIRST && (N) <= GP_OUTGOING_ARG_LAST)

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg( &CUM, MODE, TYPE, NAMED, FALSE)

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg( &CUM, MODE, TYPE, NAMED, TRUE)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero. */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) (0)

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.  */

typedef struct xtensa_args {
    int arg_words;		/* # total words the arguments take */
} CUMULATIVE_ARGS;

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
do {									\
  (CUM).arg_words = 0;							\
} while (0)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.  */

/* We set prototype to true so that we never try to return a PARALLEL from
   function_arg.  */
#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
do {									\
  (CUM).arg_words = 0;							\
} while (0)

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG'.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
 function_arg_advance (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  (((((MODE) == BLKmode ? int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE)) \
     + UNITS_PER_WORD - 1) / UNITS_PER_WORD) > 1 ? 128 : PARM_BOUNDARY)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */

#define FUNCTION_ARG_REGNO_P(N)						\
    ((N) >= GP_OUTGOING_ARG_FIRST && (N) <= GP_OUTGOING_ARG_LAST)


/* How Scalar Function Values are Returned */

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  All coprocessor and user modes
   are returned in the register 0 of the register file to which they
   must be allocated, all other modes in GP_RETURN. */

#define LIBCALL_VALUE(MODE) (							\
    gen_rtx (REG, MODE, GP_RETURN))

#define LIBCALL_OUTGOING_VALUE(MODE) (				 		\
    gen_rtx (REG, MODE, GP_OUTGOING_RETURN))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
    LIBCALL_VALUE (TYPE_MODE (VALTYPE))
#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) \
    LIBCALL_OUTGOING_VALUE (TYPE_MODE (VALTYPE))

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.  */

#define FUNCTION_VALUE_REGNO_P(N) (					\
    ((N) == GP_OUTGOING_RETURN))


/* How Large Values are Returned */

/* A nonzero value says to return the function value in memory, just as large
   structures are always returned.  */

#define RETURN_IN_MEMORY(TYPE) (						\
  (int_size_in_bytes (TYPE) > 4 * UNITS_PER_WORD))

/* If you define this macro to be 0, then the conventions used for structure
   and union return values are decided by the `RETURN_IN_MEMORY' macro.  */

#define DEFAULT_PCC_STRUCT_RETURN 0

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */

#define STRUCT_VALUE_REGNUM (GP_REG_FIRST + 8)


/* Function Entry and Exit */

/* A C compound statement that outputs the assembler code for entry and exit to a
   function.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)
#define FUNCTION_EPILOGUE(FILE, SIZE)

/* If defined, is a C expression that produces the machine-specific
   code for a call to '__builtin_saveregs'.  This code will be moved
   to the very beginning of the function, before any parameter access
   are made.  The return value of this function should be an RTX that
   contains the value to use as the return of '__builtin_saveregs'. */

#define EXPAND_BUILTIN_SAVEREGS() xtensa_builtin_saveregs ()


/* Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack pointer;
   in other words, if it is safe to delete an instruction to adjust the stack
   pointer before a return from the function.  */

#define EXIT_IGNORE_STACK 1


/* Define this macro if GNU CC should generate calls to the System V (and ANSI
   C) library functions `memcpy' and `memset' rather than the BSD functions
   `bcopy' and `bzero'.  */

#define TARGET_MEM_FUNCTIONS

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  */

#define CONSTANT_ADDRESS_P(X) 0

/* The max number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.  */

/* ??? IA64 post increment addressing mode is much more powerful than this.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 1

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  */

#ifndef REG_OK_STRICT

#define REG_OK_STRICT_P 0
#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X)  GP_REG_OR_PSEUDO_NONSTRICT_P (REGNO (X))

#else

#define REG_OK_STRICT_P 1
#define REG_OK_FOR_INDEX_P(X) 0
#define REG_OK_FOR_BASE_P(X)  REGNO_OK_FOR_BASE_P  (REGNO (X))

#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.  */

#define REG_OK_FOR_INDEX_P(X) 0

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.

   This must be present, but there is nothing useful to be done here.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.  */

/* ??? Strictly speaking this isn't true, because we can use any increment with
   any mode.  Unfortunately, the RTL implies that the increment depends on the
   mode, so we need this for now.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) {}

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  */

#define LEGITIMATE_CONSTANT_P(X) 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.

   Indirect function calls are more expensive that direct function calls, so
   don't cse function addresses.  */

#define NO_FUNCTION_CSE 1


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  */

#define DATA_SECTION_ASM_OP ".data"

/* If defined, a C expression whose value is a string containing the assembler
   operation to identify the following data as uninitialized global data.  */

#define BSS_SECTION_ASM_OP ".bss"

/* The Overall Framework of an Assembler File.  */
/* When using gcc as a front-end, we don't care about these... */

#define ASM_COMMENT_START "#"
#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"
#define ASM_OUTPUT_LONG_DOUBLE(FILE, VALUE)
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)
#define ASM_OUTPUT_FLOAT(FILE,VALUE)
#define ASM_OUTPUT_CHAR(FILE, VALUE)
#define ASM_OUTPUT_SHORT(FILE, VALUE)
#define ASM_OUTPUT_INT(FILE, VALUE)
#define ASM_OUTPUT_DOUBLE_INT(FILE, VALUE)
#define ASM_OUTPUT_BYTE(STREAM, VALUE)
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"
#define ASM_OUTPUT_LABEL(STREAM, NAME)
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)
#define ASM_OUTPUT_DEF(STREAM, NAME, VALUE)
#define ASM_OUTPUT_ALIGN(STREAM, POWER)

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM) \
      do { strcpy(LABEL, "IL"; } while(0)
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER) \
do {									\
  (OUTVAR) = (char *) alloca (strlen (NAME) + 12);			\
  sprintf (OUTVAR, "%s.%ld", (NAME), (long)(NUMBER));			\
} while (0)

#define PRINT_OPERAND(STREAM, X, CODE)
#define PRINT_OPERAND_ADDRESS(STREAM, X)
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX ""

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)
#define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE)
#define ADDR_VEC_ALIGN(ADDR_VEC) 3

/* Define this macro if GNU CC should produce dwarf version 2 format debugging
   output in response to the `-g' option.  */

#define DWARF2_DEBUGGING_INFO

/* Section names for DWARF2 debug info.  */

#define DEBUG_INFO_SECTION	".debug_info, \"\", \"progbits\""
#define ABBREV_SECTION		".debug_abbrev, \"\", \"progbits\""
#define ARANGES_SECTION		".debug_aranges, \"\", \"progbits\""
#define DEBUG_LINE_SECTION	".debug_line, \"\", \"progbits\""
#define PUBNAMES_SECTION	".debug_pubnames, \"\", \"progbits\""

/* C string constants giving the pseudo-op to use for a sequence of
   2, 4, and 8 byte unaligned constants.  dwarf2out.c needs these.  */

#define UNALIGNED_SHORT_ASM_OP		"data2.ua"
#define UNALIGNED_INT_ASM_OP		"data4.ua"
#define UNALIGNED_DOUBLE_INT_ASM_OP	"data8.ua"

/* Define to enable software floating point emulation. */
#define REAL_ARITHMETIC

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */

#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.  */

#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 4

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a 'char' or a
   'short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.  */
#define SLOW_BYTE_ACCESS 1

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* A C expression describing the value returned by a comparison operator with
   an integral mode and stored by a store-flag instruction (`sCOND') when the
   condition is true.  */

#define STORE_FLAG_VALUE 1

/* An alias for the machine mode for pointers.  */

#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  */

#define FUNCTION_MODE Pmode

/* Define this macro to handle System V style pragmas: #pragma pack and
   #pragma weak.  Note, #pragma weak will only be supported if SUPPORT_WEAK is
   defined.  */

#define HANDLE_SYSV_PRAGMA

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry. */

#define FUNCTION_PROFILER(FILE, LABELNO) {}

/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline. 
   This code should not include a label--the label is taken care of
   automatically.

   For Xtensa, the trampoline must perform an entry instruction with a
   minimal stack frame in order to get some free registers.  Once the
   actual call target is known, the proper stack frame size is extracted
   from the entry instruction at the target and the current frame is
   adjusted to match.  The trampoline then transfers control to the
   instruction following the entry at the target.  Note: this assumes
   that the target begins with an entry instruction. */

/* minimum frame = reg save area (4 words) plus static chain (1 word)
   and the total number of words must be even */
#define MIN_FRAME_SIZE (6 * UNITS_PER_WORD)

#define TRAMPOLINE_TEMPLATE(STREAM)

/* A C expression for the size in bytes of the trampoline, as an
   integer.  */

#define TRAMPOLINE_SIZE 49

/* Alignment required for trampolines, in bits.  */

#define TRAMPOLINE_ALIGNMENT (32)

/* A C statement to initialize the variable parts of a trampoline. 
   ADDR is an RTX for the address of the trampoline; FNADDR is an
   RTX for the address of the nested function; CHAIN is an
   RTX for the static chain value that should be passed to the
   function when it is called. */

#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CHAIN) { }

/* Builtins.  */

enum xtensa_builtin
{
  XTENSA_BUILTIN_UMULSIDI3,
  XTENSA_BUILTIN_max
};

#define MD_INIT_BUILTINS \
  do { \
    xtensa_init_builtins (); \
  } while (0)

#define MD_EXPAND_BUILTIN(EXP, TARGET, SUBTARGET, MODE, IGNORE) \
  xtensa_expand_builtin ((EXP), (TARGET), (SUBTARGET), (MODE), (IGNORE))

void xtensa_init_builtins ();
struct rtx_def *xtensa_expand_builtin ();

#ifndef BOOL
#define BOOL int
#endif



/* these are also declared in config_targ_options.h. In an ideal world,
   there would only be one declaration, but I cannot sort out the dependencies
   right now.
 */

extern BOOL xt_density;
extern BOOL xt_endian;
extern BOOL xt_mac16;
extern BOOL xt_mul16;
extern BOOL xt_mul32;
extern BOOL xt_mul32h;
extern BOOL xt_div32;
extern BOOL xt_nsa;
extern BOOL xt_minmax;
extern BOOL xt_sext;
extern BOOL xt_const16;
extern BOOL xt_l32r;
extern BOOL xt_abs;
extern BOOL xt_brt;
extern BOOL xt_addx;
extern BOOL xt_booleans;
extern BOOL xt_hard_float;
extern BOOL xt_hard_float_div;
extern BOOL xt_hard_float_recip;
extern BOOL xt_hard_float_sqrt;
extern BOOL xt_hard_float_rsqrt;
extern BOOL xt_clamps;
extern BOOL xt_zero_cost_loop;
extern BOOL xt_zero_init_data;
extern BOOL xt_fused_madd;
extern BOOL xt_serialize_volatile;
extern BOOL xt_text_section_literals;
extern BOOL xt_target_align;
extern BOOL xt_long_calls;
extern char * xt_lsp;
extern char * xt_rename_section;

extern unsigned int xt_icache_line_bytes;
extern unsigned int xt_dcache_line_bytes;
extern char * xt_isa_dlls_string;
extern char ** xt_isa_dlls;
extern char * xt_xtie_dlls_string;
extern char ** xt_xtie_dlls;
extern char * xt_icache_line_bytes_string;
extern char * xt_dcache_line_bytes_string;
extern char * xt_abi_string;
extern char * xt_stack_alignment_string;

