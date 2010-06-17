
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Pragma related interfaces.
   Copyright (C) 1995, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#ifndef _C_PRAGMA_H
#define _C_PRAGMA_H

#ifdef HANDLE_SYSV_PRAGMA
/* Support #pragma weak iff ASM_WEAKEN_LABEL and ASM_OUTPUT_DEF are
   defined.  */
#if defined (ASM_WEAKEN_LABEL) && defined (ASM_OUTPUT_DEF)
#define HANDLE_PRAGMA_WEAK SUPPORTS_WEAK
#endif

/* We always support #pragma pack for SYSV pragmas.  */
#ifndef HANDLE_PRAGMA_PACK
#define HANDLE_PRAGMA_PACK 1
#endif
#endif /* HANDLE_SYSV_PRAGMA */


#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
/* If we are supporting #pragma pack(push... then we automatically
   support #pragma pack(<n>)  */
#define HANDLE_PRAGMA_PACK 1
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */


#ifdef HANDLE_PRAGMA_WEAK
/* This structure contains any weak symbol declarations waiting to be emitted.  */
struct weak_syms
{
  struct weak_syms * next;
  char * name;
  char * value;
};

/* Declared in varasm.c */
extern struct weak_syms * weak_decls;

extern int add_weak PARAMS ((char *, char *));
#endif /* HANDLE_PRAGMA_WEAK */

/* Define HANDLE_GENERIC_PRAGMAS if any kind of front-end pragma
   parsing is to be done.  The code in GCC's generic C source files
   will only look for the definition of this constant.  They will
   ignore definitions of HANDLE_PRAGMA_PACK and so on. 
   With #pragma poison, this is always set.  */
#define HANDLE_GENERIC_PRAGMAS 1


#ifdef HANDLE_GENERIC_PRAGMAS
enum pragma_state
{
  ps_start,
  ps_done,
#ifdef HANDLE_PRAGMA_WEAK
  ps_weak,
  ps_name,
  ps_equals,
  ps_value,
#endif
#ifdef HANDLE_PRAGMA_PACK
  ps_pack,
  ps_left,
  ps_align,
  ps_right,
#endif
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
  ps_push, ps_pushcomma, ps_pushid, ps_pushcomma2,
  ps_pop, ps_popcomma,
#endif
  /* id,arg1,arg2 states begin */
  ps_idaa,
  ps_idaa_left,
  ps_idaa_right,
  ps_idaa_id,
  ps_idaa_right_id,
  ps_idaa_comma1,
  ps_idaa_arg1,
  ps_idaa_right_arg1,
  ps_idaa_comma2,
  ps_idaa_arg2,
  ps_idaa_right_arg2,
  /* id,arg1,arg2 states end */
  ps_poison,
#ifdef TARG_XTENSA
  ps_frequency_hint, ps_frequent, ps_never,
  ps_no_reorder_memory,
  ps_flush_memory,
  ps_no_reorder,
  ps_flush,
  ps_ivdep,
  ps_concurrent,
  ps_generate_hw,
  ps_simd_if_convert,
  ps_simd,
  ps_super_swp,
  ps_super_swp_ii,
  ps_super_swp_unroll,
  ps_swp_schedule,
#endif
  ps_bad
};

/* Handle a C style pragma */
extern int handle_pragma_token PARAMS ((const char *, tree));

#endif /* HANDLE_GENERIC_PRAGMAS */

extern void init_pragma PARAMS ((void));

#endif /* _C_PRAGMA_H */
