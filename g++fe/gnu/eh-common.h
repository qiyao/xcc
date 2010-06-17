
/* 
   Copyright (C) 2002-2004 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* EH stuff
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

This file is part of GNU CC.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


/* This file contains the structures required for the language
   independant exception handling model. Both the static compiler and
   the runtime library share this file. */

/* The runtime flag flag_new_exceptions is used to determine whether the 
   compiler supports the new runtime typechecking mechanism or not. Under
   the new model, runtime info is contained in the exception table, and
   the __throw() library routine determines which handler to call based
   on the results of a call to a matching function provided by the expcetion
   thrower.  Otherwise the old scheme of calling any handler which matches
   an exception range is used, and the handler is responsible for all
   checking of runtime conditions. If the handler wasn't suppose to
   get the exception, it performs a re-throw. */


/* The handler_label field MUST be the first field in this structure. The 
   __throw()  library routine expects uses __eh_stub() from except.c, which
   simply dereferences the context pointer to get the handler.
   The routine get_dynamic_handler_chain() also has a dependancy on
   the location of 'dynamic_handler_chain'. If its location is changed, 
   that routine must be modified as well. */

#ifndef EH_ALLOC_SIZE
/* 192 bytes means the entire eh_context plus malloc overhead fits in 256
   bytes (assuming 8 byte pointers). 192 bytes gives an eh_info and object
   size limit of 96 bytes. This should be sufficient for throwing bad_alloc. */
#define EH_ALLOC_SIZE 192
#endif
#ifndef EH_ALLOC_ALIGN
/* We can't use BIGGEST_ALIGNMENT, because on some systems, that expands to
   a check on a compile time switch like
   'target_flags & MASK_ALIGN_DOUBLE ? 64 : 32'. There's no macro for
   'largest alignment for any code this compiler can build for', which is
   really what is needed. */
#define EH_ALLOC_ALIGN 16
#endif

/* if you modify this structure, please also update throw and 
   rethrow for call0 abi in lib2funcs.S */

struct eh_context
{
  void *handler_label;
  void **dynamic_handler_chain;
  /* This is language dependent part of the eh context. */
  void *info;
  /* This is used to remember where we threw for re-throws */
  void *table_index;  /* address of exception table entry to rethrow from */
  /* emergency fallback space, if malloc fails during handling */
  char alloc_buffer[EH_ALLOC_SIZE]
      __attribute__((__aligned__(EH_ALLOC_ALIGN)));
  unsigned alloc_mask;
};


/* Version number for xtensa exception handling. */
#ifndef __XTENSA_CALL0_ABI__
#define XTENSA_EH_VERSION  1
#else
#define XTENSA_EH_VERSION 2
#endif

/* exception_descriptor identifiers. These must be chosen to always be
   distinquishable from a regions start address, since they share that
   field in the struct. */
#define EH_DESC_END_MARKER     -1

/* Types for exception table entries. */
typedef enum xtensa_eh_types
  {
    XT_EH_TYPE_TRY = 0,
    XT_EH_TYPE_CATCH = 1,
    XT_EH_TYPE_CLEANUP_CALL = 2,
    XT_EH_TYPE_CLEANUP_GENERAL = 3,
    XT_EH_TYPE_SKIP = 4,
    XT_EH_TYPE_SKIP_GENERAL = 5,
    /* all below are only used for the call0 abi */
    XT_EH_TYPE_RESTORE_CALLEE_SPREL = 6,
    XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED = 7,
    XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE = 8,
    XT_EH_TYPE_RESTORE_CALLEE_LPREL = 9,
    XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED = 10,
    XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE = 11
  } xtensa_eh_types;


#if !__XTENSA_CALL0_ABI__

typedef struct exception_table 
{
  void *start_region;
  void *end_region;
  int type;
  union {
    struct {
      void *exception_handler;
      void *match_info;
    } try_region;
    struct {
      void *this_ptr;
      void *destructor;
    } cleanup_region;
    struct {
      void *exception_info;
      void *dummy;
    } catch_region;
  } u1;
} exception_table;

typedef exception_table * exception_table_ptr;
#define NEXT_EXCEPTION_TABLE_ENTRY(et_entry) ((et_entry) + 1)
#define ET_TRY_REG_EXCEPTION_HANDLER(et) ((et)->u1.try_region.exception_handler)
#define ET_TRY_REG_MATCH_INFO(et) ((et)->u1.try_region.match_info)
#define ET_CLEANUP_REG_THIS_PTR(et) ((et)->u1.cleanup_region.this_ptr)
#define ET_CLEANUP_REG_DESTRUCTOR(et) ((et)->u1.cleanup_region.destructor)
#define ET_CATCH_REG_EXCEPTION_INFO(et) ((et)->u1.catch_region.exception_info)

#else

#define FIRST_RESTORE_ET_TYPE XT_EH_TYPE_RESTORE_CALLEE_SPREL
#define LAST_RESTORE_ET_TYPE XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE
#define FIRST_RESTORE_ET_SP_REL XT_EH_TYPE_RESTORE_CALLEE_SPREL
#define LAST_RESTORE_ET_SP_REL XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE

/* because call0 abi needs much more data to restore callee saved
   registers and unwind the stack. call0 abi uses a variable
   length exception table entry.

   keep these in sync with the restore regions, which cannot
   use the header structure because of padding issues.

   keep this also in sync with lib2funcs.S:__throw, which
   has an et_restore_region_small.

*/

typedef struct et_header
{
  void *start_region;
  void *end_region;
  short type; /* making this a short means that only the large structure
		 will need any padding between the header and the rest
		 of the structure. */
} et_header;

typedef et_header * exception_table_ptr;

typedef struct et_try_region
{
  et_header header;
  void *exception_handler;
  void *match_info;
} et_try_region;

typedef struct et_cleanup_region
{
  et_header header;
  void *this_ptr;
  void *destructor;
} et_cleanup_region;

typedef struct et_catch_region 
{
  et_header header;
  void *exception_info;
} et_catch_region;

/* the offsets to registers are scaled by 4
   the frame size is scaled by 4 also. 
   
   This is because they are word aligned, and 
   scaling by 4 allows us to have displacements
   much greater than the sizeof(offset_type) in
   each of the restore types. So an unsigned char
   gives us 10 bits of displacment instead of just
   eight.
*/

#define REG_ALIGNMENT 4

typedef struct et_restore_region_small
{
  void *start_region; 
  void *end_region;
  short type;
  unsigned char frame_size;
  unsigned char ra;
  unsigned char a12;
  unsigned char a13;
  unsigned char a14;
  unsigned char a15;
} et_restore_region_small;

typedef struct et_restore_region_med
{
  void *start_region;
  void *end_region;
  short type;
  unsigned short frame_size;
  unsigned short ra;
  unsigned short a12;
  unsigned short a13;
  unsigned short a14;
  unsigned short a15;
} et_restore_region_med;

typedef struct et_restore_region_large
{
  void *start_region;
  void *end_region;
  short type;
  unsigned int frame_size;
  unsigned int ra;
  unsigned int a12;
  unsigned int a13;
  unsigned int a14;
  unsigned int a15;
} et_restore_region_large;

#define NEXT_EXCEPTION_TABLE_ENTRY(et_entry) advance_et_ptr(et_entry)
#define ET_TRY_REG_EXCEPTION_HANDLER(et) (((et_try_region *)et)->exception_handler)
#define ET_TRY_REG_MATCH_INFO(et) (((et_try_region *)et)->match_info)
#define ET_CLEANUP_REG_THIS_PTR(et) (((et_cleanup_region *)et)->this_ptr)
#define ET_CLEANUP_REG_DESTRUCTOR(et) (((et_cleanup_region *)et)->destructor)
#define ET_CATCH_REG_EXCEPTION_INFO(et) (((et_catch_region *)et)->exception_info)

#endif


typedef struct exception_descriptor 
{
  void *start_regions;
  void *end_regions;
  exception_table_ptr eh_table;
} exception_descriptor;

/* Flag values for the "flags" field of exception_desc_list */
#define EH_DESC_SORTED	0x1

typedef struct exception_desc_list {
  exception_descriptor *eh_desc;
  void *eh_desc_end;
  unsigned int flags;
  struct exception_desc_list *prev, *next;
} exception_desc_list;

/* This value is to be checked as a 'match all' case in the runtime field. */
#define CATCH_ALL_TYPE   ((void *) -1)

struct __eh_info; /* forward declaration */

/* A matching function to determine if a handler catches a given
   exception. The function takes 2 parameters

   1 - runtime exception that has been thrown info. (__eh_info *)
   2 - Match info pointer from the region being considered (void *) */
extern void *__cplus_type_matcher (struct __eh_info *info_, void *match_info);

/* This is the runtime exception information. This forms the minimum
   required information for an exception info pointer in an eh_context
   structure.  **** When changing, be sure to change builtin
   declaration of the type in cp/except.c. **** */
typedef struct __eh_info 
{
  int version;
} __eh_info;



/* Convienient language codes for ID the originating language. Similar
   to the codes in dwarf2.h. */
 
enum exception_source_language
  {
    EH_LANG_C89 = 0x0001,
    EH_LANG_C = 0x0002,
    EH_LANG_Ada83 = 0x0003,
    EH_LANG_C_plus_plus = 0x0004,
    EH_LANG_Cobol74 = 0x0005,
    EH_LANG_Cobol85 = 0x0006,
    EH_LANG_Fortran77 = 0x0007,
    EH_LANG_Fortran90 = 0x0008,
    EH_LANG_Pascal83 = 0x0009,
    EH_LANG_Modula2 = 0x000a,
    EH_LANG_Java = 0x000b,
    EH_LANG_Mips_Assembler = 0x8001
  };

#define NEW_EH_RUNTIME  ((void *) -2)

