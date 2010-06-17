/* Custom XCC exception handling for Xtensa processors.
   Copyright (C) 2003-2004 Tensilica, Inc.  All Rights Reserved.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License,
Tensilica gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#ifdef __XTENSA__

extern "C" {

#include "tconfig.h"

#include <stddef.h>
#ifndef malloc
extern void *malloc (size_t);
#endif
#ifndef free
extern void free (void *);
#endif

#define NUM_CALLEE_SAVED 4
  /*  Note that __xtensa_nonlocal_goto in lib2funcs.S 
      manipulates one of these, so be sure to update that code if you
      change it's structure. */

typedef struct reg_restore_struct
{
  char * ra;
  char * sp;
  unsigned int callee_saves[NUM_CALLEE_SAVED];
} reg_restore_struct;


/* Struct to return information about the stack frame and handler for
   an exception.
*/
typedef struct _handler_frame_info
{
  void *handler;
  void *sp;
} handler_frame_info;


#define XT_FLUSH_RW __xtensa_libgcc_window_spill ()


//#define DEBUG_XTENSA_EH
#ifdef DEBUG_XTENSA_EH
#include <stdio.h>
#endif

/* The gthr-vxworks.h header checks if REG_SAVED_REG is defined to
   determine whether it is being included in libgcc2.c or frame.c.
   There is some code in gthr-vxworks.h that only works in libgcc2.c,
   so define REG_SAVED_REG so we don't get that code.  */
#define REG_SAVED_REG 2

#include "machmode.h"
#include "defaults.h" 
#include "gthr.h"
#include "eh-common.h"

/* imports */
extern struct eh_context *__get_eh_context (void);
extern void __xtensa_nonlocal_goto (void *sp, void *handler, void *label)
  __attribute__ ((__noreturn__));
extern void __terminate (void);
extern void __cp_pop_exception (void *p);
extern void __xtensa_libgcc_window_spill (void);

/* exports */
#if __XTENSA_CALL0_ABI__
handler_frame_info __find_exception_handler (char *pc, char *sp, char *ra,
					     __eh_info *eh_info, int rethrow,
					     reg_restore_struct *rr);
static exception_table_ptr advance_et_ptr(exception_table_ptr et);
#else
handler_frame_info __find_exception_handler (char *pc, char *sp, char *ra,
					     __eh_info *eh_info, int rethrow);
#endif
extern exception_descriptor *find_exception_descriptor (void *pc);
extern void __register_exception_descriptor_table
  (exception_descriptor *desc_table, void *desc_end);
extern void __deregister_exception_descriptor_table
  (exception_descriptor *desc_table);
extern void __load_eh_table (char **desc_table);
extern void __unload_eh_table (char **desc_table);
extern void __throw (void);
extern void __rethrow (void *index);
} /* extern "C" */

static void
find_caller_frame(exception_table_ptr et, char * sp, reg_restore_struct * rr);


/* __find_exception_handler finds the correct handler, if there is one,
   to handle an exception.  Returns information about the stack frame
   and handler, or NULL information if no handler is found.

   Parameters:
   PC - pc where the exception originates.  If this is a rethrow, 
        then this starts out as a pointer to the exception table
	entry we wish to rethrow out of.
   SP - sp where the exception originates.
   RA - return address register ($a0) where the exception originates.
   EH_INFO - eh info pointer for this exception.
   RETHROW - 1 if this is a rethrow. (see incoming value of PC). */

#if __XTENSA_CALL0_ABI__
handler_frame_info
__find_exception_handler (char *pc, char *sp, char *ra,
                          __eh_info *eh_info, int rethrow,
                           reg_restore_struct *rr)
#else
handler_frame_info
__find_exception_handler (char *pc, char *sp, char *ra,
                          __eh_info *eh_info, int rethrow)
#endif
{
#if __XTENSA_CALL0_ABI__
  reg_restore_struct restore_regs = *rr;
#endif
  /* Keep searching up through the stack until we find a handler... */
  while (1)
    {
      exception_table_ptr eh_table;

      if (rethrow) 
        {
          rethrow = 0;

          /* pc is actually the region table entry to rethrow out of */
          eh_table = (exception_table_ptr) pc;
          pc = (char *) eh_table->end_region - 1;

          /* The label is always on the LAST handler entry for a region, 
             so we know the next entry is a different region, even if the
             addresses are the same.  Make sure its not end of table tho. */
          if (eh_table->start_region != (void *) -1)
            eh_table = NEXT_EXCEPTION_TABLE_ENTRY(eh_table);
        }
      else
	{
	  /* Find the exception_descriptor that contains 'pc'.  */
	  exception_descriptor *eh_desc = find_exception_descriptor (pc);
	  if (!eh_desc)
	    eh_table = NULL;
	  else
	    eh_table = eh_desc->eh_table;
	}
      
      /* Using 'sp', find the caller's SP and RA. We assume
	 that the register windows have be flushed.  */

      char *caller_sp, *caller_ra;
#if !__XTENSA_CALL0_ABI__
      char *save_area;
      __asm__ ("addi  %0, %1, -16" : "=a" (save_area) : "a" (sp));
      __asm__ ("l32i  %0, %1, 0" : "=a" (caller_ra) : "a" (save_area));
      __asm__ ("l32i  %0, %1, 4" : "=a" (caller_sp) : "a" (save_area));
#endif

      /* We can't do a binary search because the table is in inner-most
         to outermost address ranges within functions.  */
#ifdef DEBUG_XTENSA_EH
      fprintf (stderr, "searching for pc %x, sp %x, ra %x eh_table %x\n", pc, sp, ra, eh_table);
#endif
#if __XTENSA_CALL0_ABI__
      /* in the call0 abi, every function should have an exception descriptor.
	 If it doesn't then there are a couple of possibilities:
	 A. We are attempting to throw through C code, which call0 does 
            not support 
         B. We are at the top of the stack, which means there was no catch
            for this throw.

	 In either case, we just __terminate, which is standard compliant */
      if (eh_table == NULL) {
#ifdef DEBUG_XTENSA_EH
	fprintf (stderr, "No eh_desc found\n");
#endif
	__terminate();
      }
#endif

      if (eh_table)
	{
	  void *skip_this_ptr = NULL;
	  void *skip_dest = NULL;

#if __XTENSA_CALL0_ABI__
	  find_caller_frame(eh_table, sp, &restore_regs);
	  caller_sp = restore_regs.sp;
	  caller_ra = restore_regs.ra;
#ifdef DEBUG_XTENSA_EH
	  fprintf (stderr, "caller sp = 0x%x, caller_ra = 0x%x\n", caller_sp, caller_ra);
#endif
#endif
	  for ( ; eh_table->start_region != (void *) -1; 
		eh_table = NEXT_EXCEPTION_TABLE_ENTRY(eh_table))
	    { 
#ifdef DEBUG_XTENSA_EH
	      fprintf (stderr, "table = %x; region %x - %x, type %d\n", eh_table,
		       eh_table->start_region, eh_table->end_region,
		       eh_table->type);
#endif

	      if (eh_table->start_region <= pc && eh_table->end_region > pc)
		{
		  /* We've found an exception region that bounds 'pc'.
		     Use the region type to determine what action we
		     need to take.  */
		  switch (eh_table->type)
		    {
		    case XT_EH_TYPE_TRY:
		      {
			void *ret;
			/* For a try block, we must have rtti, check
			   for a match using the rtti.  If we match
			   then we have found the handler.  */
			if (!ET_TRY_REG_MATCH_INFO(eh_table))
			  __terminate ();
			
			ret = __cplus_type_matcher
			  ((__eh_info *) eh_info,
			   ET_TRY_REG_MATCH_INFO(eh_table));
			if (ret)
			  {
			    handler_frame_info handler_info;
			    handler_info.handler =
			      ET_TRY_REG_EXCEPTION_HANDLER(eh_table);
			    handler_info.sp = sp;
#if __XTENSA_CALL0_ABI__
			    rr->ra = restore_regs.ra;
			    rr->sp = sp;
#endif
			    return handler_info;
			  }

		      }
		      break;
		      
		    case XT_EH_TYPE_CATCH:
 		      /* If we reach a cleanup region, then pop the
 			 current exception and continue looking for a
 			 handler.  */
 		      {
 			void **ei = (void **)
			  ((char *) caller_sp
			   - (int) ET_CATCH_REG_EXCEPTION_INFO(eh_table));
#ifdef DEBUG_XTENSA_EH
 			fprintf (stderr, "popping exception at %x, "
				 "offset %d from caller sp %x\n",
				 ei,
				 (int) ET_CATCH_REG_EXCEPTION_INFO(eh_table),
				 caller_sp);
#endif
 			__cp_pop_exception (*ei);
 		      }
		      break;
		      
		    case XT_EH_TYPE_CLEANUP_CALL:
		      /* Call the cleanup destructor, terminating if
                         it throws an exception.  */
		      try
			{
			  typedef void (*dest_fn) (void *obj, int in_charge);
			  
			  /* 'this_ptr' is the offset from 'caller_sp' to the
                             object being cleaned up.  */
			  void *obj = (void *)
			    ((char *) caller_sp
			     - (int) ET_CLEANUP_REG_THIS_PTR(eh_table));
			  dest_fn dest =
			    (dest_fn) ET_CLEANUP_REG_DESTRUCTOR(eh_table);

#ifdef DEBUG_XTENSA_EH
			  fprintf (stderr,
				   "cleanup for %x, caller_sp %x, dest %x\n",
				   obj, caller_sp, dest);
#endif

			  /* If we are skipping to a cleanup and this
                             cleanup matches, then stop skipping (but
                             don't do this cleanup).  */
			  if (skip_this_ptr || skip_dest)
			    {
#ifdef DEBUG_XTENSA_EH
			      fprintf (stderr, "skipping...\n");
#endif
			      if (skip_this_ptr == obj
				  && skip_dest == (void *) dest)
				{
				  skip_this_ptr = skip_dest = NULL;
				}
			      else if (((skip_this_ptr == NULL)
					!= (skip_dest == NULL)) ||
				       ((skip_this_ptr == obj)
					!= (skip_dest == (void *) dest)))
				{
#ifdef DEBUG_XTENSA_EH
				  fprintf (stderr,
					   "corrupt skip data %d %d\n",
					   skip_this_ptr, skip_dest);
#endif
				  __terminate ();
				}
			    }
			  else
			    {
			      (*dest) (obj, 2);
			    }
			}
		      catch (...)
			{
			  __terminate ();
			}
		      break;
		      
		    case XT_EH_TYPE_CLEANUP_GENERAL:
		      {
			/* For general cleanup code, return the
			   cleanup code as the handler.  If we are
			   skipping to a cleanup and this cleanup
			   matches, then stop skipping (but don't do
			   this cleanup).  */
			void *handler =
			  ET_TRY_REG_EXCEPTION_HANDLER(eh_table);
			
			if (skip_this_ptr == handler && !skip_dest)
			  {
#ifdef DEBUG_XTENSA_EH
			    fprintf (stderr, "skipping...\n");
#endif
			    skip_this_ptr = NULL;
			  }
			else
			  {
			    handler_frame_info handler_info;
			    handler_info.handler = handler;
			    handler_info.sp = sp;
#if __XTENSA_CALL0_ABI__
			    rr->ra = restore_regs.ra;
			    rr->sp = sp;
#endif
			    return handler_info;
			  }
		      }
		      break;
		      
		    case XT_EH_TYPE_SKIP:
		      /* If we are already skipping, then we shouldn't
                         try to start another skip... */
		      if (skip_this_ptr != NULL || skip_dest != NULL)
			{
#ifdef DEBUG_XTENSA_EH
			  fprintf (stderr, "skip already active %x, %x\n",
				   skip_this_ptr, skip_dest);
#endif
			  __terminate ();
			}
			
		      /* We want to skip all cleanups in this function
			 until we reach the one matching
			 'skip_this_ptr' and 'skip_dest'.  */
		      skip_this_ptr = (void *)
			((char *) caller_sp
			 - (int) ET_CLEANUP_REG_THIS_PTR(eh_table));
		      skip_dest = (void *)
			ET_CLEANUP_REG_DESTRUCTOR(eh_table);
		      break;
		      
		    case XT_EH_TYPE_SKIP_GENERAL:
		      /* If we are already skipping, then we shouldn't
                         try to start another skip... */
		      if (skip_this_ptr != NULL || skip_dest != NULL)
			{
#ifdef DEBUG_XTENSA_EH
			  fprintf (stderr, "skip already active %x, %x\n",
				   skip_this_ptr, skip_dest);
#endif
			  __terminate ();
			}
			
		      /* We want to skip all cleanups in this function
			 until we reach a general one with handler
			 label matching 'skip_this_ptr'.  */
		      skip_this_ptr =
			ET_TRY_REG_EXCEPTION_HANDLER(eh_table);
		      skip_dest = NULL;
		      break;

#if __XTENSA_CALL0_ABI__
		    case XT_EH_TYPE_RESTORE_CALLEE_SPREL:
		    case XT_EH_TYPE_RESTORE_CALLEE_LPREL:
		    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED:
		    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED:
		    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE:
		    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE:
		      /* already done */
		      break;
#endif
		    default:
#ifdef DEBUG_XTENSA_EH
		      fprintf (stderr,
			       "unknown exception type %x\n", eh_table->type);
#endif
		      __terminate ();
		      break;
		    }
		}
	    }
	  /* We should always find the cleanup we are skipping to
             before we finish all the exception regions in a function. */
	  if (skip_this_ptr || skip_dest)
	    {
#ifdef DEBUG_XTENSA_EH
	      fprintf (stderr, "finished function exception regions "
		       "without finding skip cleanup\n");
#endif
	      __terminate ();
	    }
	}
      

      /* If 'ra' is 0, then we have reached the top of the stack, so
	 return a NULL handler to indicate that we didn't find a
	 handler.  */
      if (ra == 0)
	{
	  handler_frame_info handler_info;
	  handler_info.handler = NULL;
	  handler_info.sp = NULL;
	  return handler_info;
	}
      /* "Unwind" the stack one level and continue looking for the
         handler. We don't actually do the unwind here, we just adjust
         'pc', 'sp', and 'ra' as if we had.  */


#if __XTENSA_CALL0_ABI__      
      pc = caller_ra - 3;
      /* we know if we get to this point that we will be unwinding through
	 the function. */
      *rr = restore_regs;
      rr->sp = sp;
#else
      /* Use 'ra' to determine the new value for 'pc' within the
         caller.  We assume the upper bits of the caller's PC are the
         same as of 'pc' (a valid assumption since we must have
         reached 'pc' by a call from the caller).  */
      __asm__ ("extui %0, %1, 30, 2" : "=a" (pc) : "a" (pc));
      __asm__ ("slli  %0, %1, 2" : "=a" (ra) : "a" (ra));
      __asm__ ("ssai  2");
      __asm__ ("src   %0, %1, %2" : "=a" (pc) : "a" (pc), "a" (ra));
      pc -= 3;
#endif
      
      ra = caller_ra;
      sp = caller_sp;

#ifdef DEBUG_XTENSA_EH
      fprintf (stderr, "unwind to pc %x, sp %x, ra %x\n", pc, sp, ra);
#endif
    }

  __terminate ();
}


/* Return the PC, SP, and RA of the caller of the current procedure.
 */

/* We assume that the register windows have be flushed.  We assume the
   upper bits of the caller's PC are the same as the current pc (a
   valid assumption since we must have reached the current PC by a
   call from the caller).  LABEL must be unique label name in the
   current procedure (from which we get the upper bits of the current
   PC).  */

#define XT_CALLER_PC(LABEL, PC) {					\
  void *cur_pc;						\
  __asm__ ("mov   %0, a0" : "=a" (PC));					\
  __asm__ (LABEL": movi %0, "LABEL : "=a" (cur_pc));			\
  __asm__ ("extui %0, %1, 30, 2" : "=a" (cur_pc) : "a" (cur_pc));	\
  __asm__ ("slli  %0, %1, 2" : "=a" (PC) : "a" (PC));			\
  __asm__ ("ssai  2");							\
  __asm__ ("src   %0, %1, %2" : "=a" (PC) : "a" (cur_pc), "a" (PC));	\
}

#define XT_CALLER_SP_RA(SP, RA) {					\
  void *save_area;							\
  __asm__ ("addi  %0, sp, -16" : "=a" (save_area));			\
  __asm__ ("l32i  %0, %1, 0" : "=a" (RA) : "a" (save_area));		\
  __asm__ ("l32i  %0, %1, 4" : "=a" (SP) : "a" (save_area));		\
}

extern char __XT_EXCEPTION_DESCS__[];
extern char __XT_EXCEPTION_DESCS_END__[];
static exception_desc_list __XT_EXCEPTION_DESC_LIST__ = {
  (exception_descriptor *) __XT_EXCEPTION_DESCS__, 
  __XT_EXCEPTION_DESCS_END__, 0, NULL, NULL};

#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t desc_list_mutex = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t desc_list_mutex;
#endif

/* This is undefined below if we need it to be an actual function.  */
#define init_desc_list_mutex_once()

#if __GTHREADS
#ifdef __GTHREAD_MUTEX_INIT_FUNCTION

/* Helper for init_desc_list_mutex_once.  */

static void
init_desc_list_mutex (void)
{
  __GTHREAD_MUTEX_INIT_FUNCTION (&desc_list_mutex);
}

/* Call this to arrange to initialize the object mutex.  */

#undef init_desc_list_mutex_once
static void
init_desc_list_mutex_once (void)
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  __gthread_once (&once, init_desc_list_mutex);
}

#endif /* __GTHREAD_MUTEX_INIT_FUNCTION */
#endif /* __GTHREADS */

 
/* Standard quick sort routine to sort the exception desciptor based on
   region for faster searches.  */

static void 
swap_eh_desc (exception_descriptor *e1, exception_descriptor *e2) 
{
  exception_descriptor t;
  t = *e1;
  *e1 = *e2;
  *e2 = t;
}
 
static exception_descriptor *
qsort_partition_eh_desc (exception_descriptor *left,
			 exception_descriptor *right)
{
   exception_descriptor *e, *pivot, *cmp_pivot;
   swap_eh_desc (left, left + (right - left)/ 2);
   cmp_pivot = left;
   for (cmp_pivot = left, pivot = cmp_pivot, e = left + 1; e <= right; e++)
     {
       if (e->start_regions < cmp_pivot->start_regions)
	 {
	   swap_eh_desc (++pivot, e);
	 }
     }
   swap_eh_desc (left, pivot);
   return pivot;
}

static void 
qsort_eh_desc (exception_descriptor *left, exception_descriptor *right)
{
  if (left < right)
    {
      exception_descriptor *pivot = qsort_partition_eh_desc (left, right);
      qsort_eh_desc (left, pivot);
      qsort_eh_desc (pivot + 1, right);
    }
}


/* find_exception_descriptor finds the correct exception descriptor
   (which contains the exception table) for 'pc'.  Returns NULL if no
   exception_descriptor exists.  */

exception_descriptor *
find_exception_descriptor (void *pc)
{
  exception_desc_list *eh_desc_list = &__XT_EXCEPTION_DESC_LIST__;
   
#ifdef DEBUG_XTENSA_EH 
  fprintf (stderr, "searching for exception desc for pc %x\n", pc);
#endif
 
  init_desc_list_mutex_once (); 
  __gthread_mutex_lock (&desc_list_mutex);
 
  while (eh_desc_list != NULL)
    {
      exception_descriptor *eh_desc_left, *eh_desc_right, *eh_desc_mid;

      /* Initialize the left and right pointers (the last exception
	 descriptor is like the null pointer).  */
      eh_desc_left  = eh_desc_list->eh_desc;
      eh_desc_right = (exception_descriptor *) eh_desc_list->eh_desc_end - 1;

      /* sort the exception descriptors the first time */
      if (!(eh_desc_list->flags & EH_DESC_SORTED))
	{
	  /* sort them in place */
	  qsort_eh_desc (eh_desc_left, eh_desc_right);
	  eh_desc_list->flags |= EH_DESC_SORTED;
	}

      /* binary search */
      while (eh_desc_left <= eh_desc_right)
	{
	  eh_desc_mid = eh_desc_left + (eh_desc_right - eh_desc_left) / 2;

#ifdef DEBUG_XTENSA_EH
	  fprintf (stderr, "trying desc %x, %x - %x\n",
		   eh_desc_mid, eh_desc_mid->start_regions,
		   eh_desc_mid->end_regions);
#endif

	  if (pc >= eh_desc_mid->start_regions
	      && pc < eh_desc_mid->end_regions)
	    {
#ifdef DEBUG_XTENSA_EH
	      fprintf (stderr, "found bounding desc %x - %x\n",
		       eh_desc_mid->start_regions, eh_desc_mid->end_regions);
	      fprintf (stderr, "   et contents = ");
	      char * advancer = (char *)eh_desc_mid->eh_table;
	      for (int i = 0; i < 20; i++)
		fprintf(stderr, "%x ", advancer[i]);
	      fprintf (stderr, "\n");
#endif
	      __gthread_mutex_unlock (&desc_list_mutex);
	      return eh_desc_mid;
	    }
	  else if (pc >= eh_desc_mid->end_regions)
	    {
	      eh_desc_left = eh_desc_mid + 1;
	    }
	  else
	    {
	      eh_desc_right = eh_desc_mid - 1;
	    }
	}

      eh_desc_list = eh_desc_list->next;
    }

#ifdef DEBUG_XTENSA_EH
  fprintf (stderr, "failed to find descriptor for pc %x\n", pc);
#endif

  __gthread_mutex_unlock (&desc_list_mutex);
  return NULL;
}


void
__register_exception_descriptor_table (exception_descriptor *desc_table,
				       void *desc_end)
{
  exception_desc_list *eh_desc_list = &__XT_EXCEPTION_DESC_LIST__;
  exception_desc_list *new_eh_desc_list;

  new_eh_desc_list = (exception_desc_list *)
    malloc (sizeof (exception_desc_list));
  new_eh_desc_list->eh_desc = desc_table;
  new_eh_desc_list->eh_desc_end = desc_end;
  new_eh_desc_list->flags = 0;
  new_eh_desc_list->prev = NULL;
  new_eh_desc_list->next = NULL;

  init_desc_list_mutex_once ();
  __gthread_mutex_lock (&desc_list_mutex);
  while (eh_desc_list->next != NULL)
    eh_desc_list = eh_desc_list->next;
  new_eh_desc_list->prev = eh_desc_list;
  eh_desc_list->next = new_eh_desc_list;
  __gthread_mutex_unlock (&desc_list_mutex);
}


void
__deregister_exception_descriptor_table (exception_descriptor *desc_table)
{
  exception_desc_list *eh_desc_list = &__XT_EXCEPTION_DESC_LIST__;

  init_desc_list_mutex_once ();
  __gthread_mutex_lock (&desc_list_mutex);
  while ((eh_desc_list != NULL) && (eh_desc_list->eh_desc != desc_table))
    eh_desc_list = eh_desc_list->next;
  if (eh_desc_list != NULL)
    {
      if (eh_desc_list->prev != NULL)
	eh_desc_list->prev->next = eh_desc_list->next;
      if (eh_desc_list->next != NULL)
	eh_desc_list->next->prev = eh_desc_list->prev;
    }
  __gthread_mutex_unlock (&desc_list_mutex);
  if (eh_desc_list != NULL) 
    free (eh_desc_list);
}


static void
sort_eh_table (char **desc_table)
{
  int changed = 1;
  while (changed)
    {
      char *temp_ptr = desc_table[0];
      int i;

      changed = 0;
      for (i = 1; desc_table[i] != NULL; i++)
	{
	  if (temp_ptr > desc_table[i])
	    {
	      desc_table[i-1] = desc_table[i];
	      changed = 1;
	    }
	  else
	    {
	      desc_table[i-1] = temp_ptr;
	      temp_ptr = desc_table[i];
	    }
	}
      desc_table[i-1] = temp_ptr;
    }
}


/* The __load_eh_table and __unload_eh_table functions are provided for
   systems that can load and run an unlinked object file (e.g., VxWorks).  */

void
__load_eh_table (char **desc_table)
{
  char *first_desc;
  int i;

  if (desc_table[0] == NULL) return;

  sort_eh_table (desc_table);
  first_desc = desc_table[0];
  for (i = 1; desc_table[i] != NULL; i++)
    {
      if (desc_table[i] != desc_table[i-1] + sizeof (exception_descriptor))
	{
	  __register_exception_descriptor_table
	    ((exception_descriptor *) first_desc, 
	     desc_table[i-1] + sizeof (exception_descriptor));
	  first_desc = desc_table[i];
	}
    }
  __register_exception_descriptor_table
    ((exception_descriptor *) first_desc, 
     desc_table[i-1] + sizeof (exception_descriptor));
}


void
__unload_eh_table (char **desc_table)
{
  char *first_desc = desc_table[0];
  int i;

  if (first_desc == NULL) return;

  for (i = 1; desc_table[i] != NULL; i++)
    {
      if (desc_table[i] != desc_table[i-1] + sizeof (exception_descriptor))
	{
	  __deregister_exception_descriptor_table
	    ((exception_descriptor *) first_desc);
	  first_desc = desc_table[i];
	}
    }
  __deregister_exception_descriptor_table
    ((exception_descriptor *) first_desc);
}


/* Find the correct handler for the currently active exception, and
   return control to that handler.  If we can't find a handler, or if
   there are other problems we call __terminate.  We never return
   directly to our caller (we may return to a handler in the same
   procedure context as our caller).  */

#if !__XTENSA_CALL0_ABI__
/* these are found in lib2funcs.S for call0 */

void
__throw (void)
{
  struct eh_context *eh = __get_eh_context ();
  char *pc, *sp, *ra;

  /* This is required for C++ semantics.  We must call terminate if we
     try to throw an exception, when there is no exception currently
     active.  */
  if (! eh->info)
    __terminate ();

  /* Find the pc, sp, and ra of the caller (i.e., the throw()).  We
     subtract 3 to get the pc of the call.  */
  XT_FLUSH_RW;
  XT_CALLER_PC (".Lt_throw", pc);
  XT_CALLER_SP_RA (sp, ra);
  pc -= 3;

  /* Find the handler for 'pc' and transfer control directly to the
     handler, or call __terminate if no handler can be found. */

  __eh_info *eh_info = (__eh_info *) eh->info;
  handler_frame_info handler_info =
    __find_exception_handler (pc, sp, ra, eh_info, 0);

  if (!handler_info.handler) 
    __terminate ();

  eh->handler_label = handler_info.handler;

  /* Perform a non-local jump to the handler.  This takes care of
     unwinding the stack, restoring registers, etc.  */

  __xtensa_nonlocal_goto (handler_info.sp, handler_info.handler, NULL);
}


void
__rethrow (void *index)
{
  struct eh_context *eh = __get_eh_context ();
  char *sp, *ra;
  /* This is required for C++ semantics.  We must call terminate if we
     try and rethrow an exception, when there is no exception
     currently active.  */
  if (! eh->info)
    __terminate ();

  /* This is the table index we want to rethrow from. The value of
     the END_REGION label is used for the PC of the throw, and the
     search begins with the next table entry. */
  eh->table_index = index;
    
  /* Find sp and ra of the caller (i.e., the rethrow()).  */
  XT_FLUSH_RW;
  XT_CALLER_SP_RA (sp, ra);

  /* Find the handler for 'pc' and transfer control directly to the
     handler, or call __terminate if no handler can be found.  */

  __eh_info *eh_info = (__eh_info *) eh->info;
  handler_frame_info handler_info =
    __find_exception_handler ((char *) eh->table_index, sp, ra, eh_info, 1);
  eh->table_index = NULL;

  if (!handler_info.handler) 
    __terminate ();

  eh->handler_label = handler_info.handler;

  /* Perform a non-local jump to the handler.  This takes care of
     unwinding the stack, restoring registers, etc.  */

  __xtensa_nonlocal_goto (handler_info.sp, handler_info.handler, NULL);
}

#else

static inline bool et_is_sp_rel (exception_table_ptr et)
{
  if (et->type >= FIRST_RESTORE_ET_SP_REL &&
      et->type <= LAST_RESTORE_ET_SP_REL)
    return true;
  else
    return false;
}

/* OBSERVE: base is an unsigned long, so pointer arithmetic comes
   in multiples of four. So we don't have to unscale our offset fields. */

static inline unsigned int et_get_ra (exception_table_ptr et)
{
  switch (et->type) 
    {
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL: 
      return ((et_restore_region_small *)et)->ra; 
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED: 
      return ((et_restore_region_med *)et)->ra; 
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE: 
      return ((et_restore_region_large *)et)->ra; 
    default:
      return 0;
    }      
}

static inline unsigned int et_get_frame_size (exception_table_ptr et)
{
  switch (et->type) 
    {
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL: 
      return ((et_restore_region_small *)et)->frame_size; 
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED: 
      return ((et_restore_region_med *)et)->frame_size; 
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE: 
      return ((et_restore_region_large *)et)->frame_size; 
    default:
      return 0;
    }      
}

static inline void
et_restore_registers (exception_table_ptr et, reg_restore_struct * rr, unsigned long * base)
{
  switch (et->type) 
    {
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL: 
      {
	et_restore_region_small * typed_et = (et_restore_region_small *)et;
	if (typed_et->a12)
	  rr->callee_saves[0] = *(base + typed_et->a12);
	if (typed_et->a13)
	  rr->callee_saves[1] = *(base + typed_et->a13);
	if (typed_et->a14)
	  rr->callee_saves[2] = *(base + typed_et->a14);
	if (typed_et->a15)
	  rr->callee_saves[3] = *(base + typed_et->a15);
	break;
      }
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED: 
      {
	et_restore_region_med * typed_et = (et_restore_region_med *)et;
	if (typed_et->a12)
	  rr->callee_saves[0] = *(base + typed_et->a12);
	if (typed_et->a13)
	  rr->callee_saves[1] = *(base + typed_et->a13);
	if (typed_et->a14)
	  rr->callee_saves[2] = *(base + typed_et->a14);
	if (typed_et->a15)
	  rr->callee_saves[3] = *(base + typed_et->a15);
	break;
      }
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE: 
      {
	et_restore_region_large * typed_et = (et_restore_region_large *)et;
	if (typed_et->a12)
	  rr->callee_saves[0] = *(base + typed_et->a12);
	if (typed_et->a13)
	  rr->callee_saves[1] = *(base + typed_et->a13);
	if (typed_et->a14)
	  rr->callee_saves[2] = *(base + typed_et->a14);
	if (typed_et->a15)
	  rr->callee_saves[3] = *(base + typed_et->a15);
	break;
      }
    default:
      break;
    }      
}

static void find_caller_frame(exception_table_ptr et, char * sp, reg_restore_struct * rr)
{
  while ((et->type < FIRST_RESTORE_ET_TYPE ||
	  et->type > LAST_RESTORE_ET_TYPE) &&
	 et->start_region != (void *) -1) {
    et = NEXT_EXCEPTION_TABLE_ENTRY(et);
  }
  if (et->start_region == (void *) -1)
    return;

  unsigned long * base;
  if (et_is_sp_rel(et))
    base = (unsigned long *) sp;
  else
    base = (unsigned long *) rr->callee_saves[3];

#ifdef DEBUG_XTENSA_EH
  fprintf (stderr, "frame type = %d\nframe_size = %d, base = 0x%x\n", 
	   et->type, et_get_frame_size(et), base);
#endif
  rr->ra = (char *)*(base + et_get_ra(et));
  rr->sp = (char *)(base + et_get_frame_size(et));
  et_restore_registers(et, rr, base);
}


static exception_table_ptr advance_et_ptr(exception_table_ptr et)
{
  unsigned int size;
  char * advancer = (char *) et;
  switch (et->type)
    {
    case XT_EH_TYPE_TRY: 
      size = sizeof(et_try_region); 
      break;
    case XT_EH_TYPE_CATCH: 
      size = sizeof(et_catch_region); 
      break;
    case XT_EH_TYPE_CLEANUP_CALL:
    case XT_EH_TYPE_CLEANUP_GENERAL: 
    case XT_EH_TYPE_SKIP: 
    case XT_EH_TYPE_SKIP_GENERAL: 
      size = sizeof(et_cleanup_region); 
      break;
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL: 
      size = sizeof(et_restore_region_small); 
      break;
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_MED: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_MED: 
      size = sizeof(et_restore_region_med);
      break;
    case XT_EH_TYPE_RESTORE_CALLEE_SPREL_LARGE: 
    case XT_EH_TYPE_RESTORE_CALLEE_LPREL_LARGE: 
      size = sizeof(et_restore_region_large); 
      break;
    default:
#ifdef DEBUG_XTENSA_EH
      fprintf (stderr, "unknown type of exception_handler\n");
#endif
      __terminate();
      break;
    }
  advancer += size;
#ifdef DEBUG_XTENSA_EH
      fprintf (stderr, "old et = 0x%x, new_et = 0x%x\n new contents = ", et, advancer);
      for (int i = 0; i < 20; i++)
	fprintf(stderr, "%x ", advancer[i]);
      fprintf(stderr, "\n");
#endif
  return (exception_table_ptr) advancer;
}

#endif

#endif /* __XTENSA__ */
