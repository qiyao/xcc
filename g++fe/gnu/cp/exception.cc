
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

// Functions for Exception Support for -*- C++ -*-
// Copyright (C) 1994, 1995, 1996, 1997, 1998, 1999, 2000 Free Software Foundation

// This file is part of GNU CC.

// GNU CC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// GNU CC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU CC; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA. 

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#pragma implementation "exception"

#include "typeinfo"
#include "exception"
#include <stddef.h>
#include "gansidecl.h" /* Needed to support macros used in eh-common.h. */
extern "C" {
#include "eh-common.h"
}

/* Define terminate, unexpected, set_terminate, set_unexpected as
   well as the default terminate func and default unexpected func.  */

extern std::terminate_handler __terminate_func __attribute__((__noreturn__));
using std::terminate;

void
std::terminate ()
{
  __terminate_func ();
}

void
__default_unexpected ()
{
  terminate ();
}

static std::unexpected_handler __unexpected_func __attribute__((__noreturn__))
  = __default_unexpected;

std::terminate_handler
std::set_terminate (std::terminate_handler func)
{
  std::terminate_handler old = __terminate_func;

  __terminate_func = func;
  return old;
}

std::unexpected_handler
std::set_unexpected (std::unexpected_handler func)
{
  std::unexpected_handler old = __unexpected_func;

  __unexpected_func = func;
  return old;
}

void
std::unexpected ()
{
  __unexpected_func ();
}

/* The type of a function called to clean up an exception object.
   (These will be destructors.)  Under the old ABI, these take a
   second argument (the `in-charge' argument), that indicates whether
   or not do delete the object, and whether or not to destroy virtual
   bases.  Under the new ABI, there is no second argument.  */
#if !defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
typedef void (*cleanup_fn)(void *, int);
/* The `2' is the value for the in-charge parameter that indicates
   that virtual bases should be destroyed.  */
#define CALL_CLEANUP(FN, THIS) FN (THIS, 2)
#else
typedef void (*cleanup_fn)(void *);
#define CALL_CLEANUP(FN, THIS) FN (THIS)
#endif

/* C++-specific state about the current exception.
   This must match init_exception_processing().

   Note that handlers and caught are not redundant; when rethrown, an
   exception can have multiple active handlers and still be considered
   uncaught.  */

struct cp_eh_info
{
  __eh_info eh_info;
  void *value;
  void *type;
  cleanup_fn cleanup;
  bool caught;
  cp_eh_info *next;
  long handlers;
  void *original_value;
};

/* Language-specific EH info pointer, defined in libgcc2. */

extern "C" cp_eh_info **__get_eh_info (); 	// actually void **

/* Exception allocate and free, defined in libgcc2. */
extern "C" void *__eh_alloc(size_t);
extern "C" void __eh_free(void *);
 
/* Is P the type_info node for a pointer of some kind?  */

extern bool __is_pointer (void *);


/* OLD Compiler hook to return a pointer to the info for the current exception.
   Used by get_eh_info ().  This fudges the actualy returned value to
   point to the beginning of what USE to be the cp_eh_info structure.
   THis is so that old code that dereferences this pointer will find
   things where it expects it to be.*/
extern "C" void *
__cp_exception_info (void)
{
  return &((*__get_eh_info ())->value);
}

#define CP_EH_INFO ((cp_eh_info *) *__get_eh_info ())

/* Old Compiler hook to return a pointer to the info for the current exception.
   Used by get_eh_info ().  */

extern "C" cp_eh_info *
__cp_eh_info (void)
{
  cp_eh_info *p = CP_EH_INFO;
  return p;
}

/* Compiler hook to return a pointer to the info for the current exception,
   Set the caught bit, and increment the number of handlers that are
   looking at this exception. This makes handlers smaller. */

extern "C" cp_eh_info *
__start_cp_handler (void)
{
  cp_eh_info *p = CP_EH_INFO;
  p->caught = 1;
  p->handlers++;
  return p;
}

extern "C" int __throw_type_match_rtti_2 (const void *, const void *,
					 void *, void **);

extern "C" void *
__cplus_type_matcher (__eh_info *info_, void *match_info)
{
  cp_eh_info *info = (cp_eh_info *)info_;

  if (match_info == CATCH_ALL_TYPE)
    return (void *)1;

  void *match_type = match_info;
  
#if !defined (__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
  match_type  = ((void *(*)())match_type) ();
#endif

  if (__throw_type_match_rtti_2 (match_type, info->type,
				 info->original_value, &info->value))
    // Arbitrary non-null pointer.
    return (void *)1;
  else
    return NULL;
}

/* Compiler hook to push a new exception onto the stack.
   Used by expand_throw().  */

extern "C" void
__cp_push_exception (void *value, void *type, cleanup_fn cleanup)
{
  cp_eh_info *p = (cp_eh_info *) __eh_alloc (sizeof (cp_eh_info));

  p->value = value;
  p->type = type;
  p->cleanup = cleanup;
  p->handlers = 0;
  p->caught = false;
  p->original_value = value;

  p->eh_info.version = XTENSA_EH_VERSION;

  cp_eh_info **q = __get_eh_info ();

  p->next = *q;
  *q = p;
}

/* Compiler hook to pop an exception that has been finalized.  Used by
   push_eh_cleanup().  P is the info for the exception caught by the
   current catch block.  */

extern "C" void
__cp_pop_exception (cp_eh_info *p)
{
  cp_eh_info **stack = __get_eh_info ();
  cp_eh_info **q = stack;

  --p->handlers;

  /* Make sure the version is correct. This also acts as a magic
     number to make sure 'p' is really pointing to a cp_eh_info
     object. */
  if (p->eh_info.version != XTENSA_EH_VERSION)
    terminate ();

  /* Do nothing if our exception is being rethrown (i.e. if the active
     exception is our exception and it is uncaught).  */
  if (p == *q && !p->caught)
    return;

  /* Don't really pop if there are still active handlers for our exception;
     rather, push it down past any uncaught exceptions.  */
  if (p->handlers != 0)
    {
      if (p == *q && p->next && !p->next->caught)
	{
	  q = &(p->next);
	  while (1)
	    {
	      if (*q == 0 || (*q)->caught)
		break;

	      q = &((*q)->next);
	    }
	  *stack = p->next;
	  p->next = *q;
	  *q = p;
	}
      return;
    }

  for (; *q; q = &((*q)->next))
    if (*q == p)
      break;

  if (! *q)
    terminate ();

  *q = p->next;

  if (p->cleanup)
    // value may have been adjusted.
    CALL_CLEANUP (p->cleanup, p->original_value);

  if (! __is_pointer (p->type))
    __eh_free (p->original_value);  // value may have been adjusted.

  __eh_free (p);
}

/* We're doing a rethrow.  Find the currently handled exception, mark it
   uncaught, and move it to the top of the EH stack.  */

extern "C" void
__uncatch_exception (void)
{
  cp_eh_info **stack = __get_eh_info ();
  cp_eh_info **q = stack;
  cp_eh_info *p;

  while (1)
    {
      p = *q;

      if (p == 0)
	terminate ();
      if (p->caught)
	break;

      q = &(p->next);
    }

  if (q != stack)
    {
      *q = p->next;
      p->next = *stack;
      *stack = p;
    }

  p->caught = false;
}

/* As per [except.unexpected]:
   If an exception is thrown, we check it against the spec.  If it doesn't
   match, we call unexpected ().  If unexpected () throws, we check that
   exception against the spec.  If it doesn't match, if the spec allows
   bad_exception we throw that; otherwise we call terminate ().

   The compiler treats an exception spec as a try block with a generic
   handler that just calls this function with a list of the allowed
   exception types, so we have an active exception that can be rethrown.

   This function does not return.  */   

extern "C" void
__check_eh_spec (int n, const void **spec)
{
  cp_eh_info *p = CP_EH_INFO;
  void *d;

  for (int i = 0; i < n; ++i)
    {
      if (__throw_type_match_rtti_2 (spec[i], p->type, p->value, &d))
	throw;
    }

  try
    {
      std::unexpected ();
    }
  catch (...)
    {
      // __exception_info is an artificial var pushed into each catch block.
      if (p != __exception_info)
	{
	  p = __exception_info;
	  for (int i = 0; i < n; ++i)
	    {
	      if (__throw_type_match_rtti_2 (spec[i], p->type, p->value, &d))
		throw;
	    }
	}

      const std::type_info &bad_exc = typeid (std::bad_exception);
      for (int i = 0; i < n; ++i)
	{
	  if (__throw_type_match_rtti_2 (spec[i], &bad_exc, p->value, &d))
	    throw std::bad_exception ();
	}

      terminate ();
    }
}

/* Special case of the above for throw() specs.  */

extern "C" void
__check_null_eh_spec (void)
{
  __check_eh_spec (0, 0);
}

// Helpers for rtti. Although these don't return, we give them return types so
// that the type system is not broken.

extern "C" void *
__throw_bad_cast ()
{
  throw std::bad_cast ();
  return 0;
}

extern "C" std::type_info const &
__throw_bad_typeid ()
{
  throw std::bad_typeid ();
  return typeid (void);
}

/* Has the current exception been caught?  */

bool
std::uncaught_exception ()
{
  cp_eh_info *p = CP_EH_INFO;
  return p && ! p->caught;
}

const char * std::exception::
what () const
{
  return typeid (*this).name ();
}
