
/* 
   Copyright (C) 2003-2007 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Handle #pragma, system V.4 style.  Supports #pragma weak and #pragma pack.
   Copyright (C) 1992, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "function.h"
#include "defaults.h"
#include "c-pragma.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"

#ifdef HANDLE_GENERIC_PRAGMAS

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
typedef struct align_stack
{
  int                  alignment;
  unsigned int         num_pushes;
  tree                 id;
  struct align_stack * prev;
} align_stack;

static struct align_stack * alignment_stack = NULL;

/* If we have a "global" #pragma pack(<n>) if effect when the first
   #pragma push(pack,<n>) is encountered, this stores the the value of 
   maximum_field_alignment in effect.  When the final pop_alignment() 
   happens, we restore the value to this, not to a value of 0 for
   maximum_field_alignment.  Value is in bits. */
static int  default_alignment;

static int super_swp_ii;
static int super_swp_unroll;
static char* swp_schedule_str = NULL;
static int swp_schedule_str_len = 0;

static int  push_alignment PARAMS ((int, tree));
static int  pop_alignment  PARAMS ((tree));
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
static void mark_align_stack PARAMS ((void *));
#endif

/* Push an alignment value onto the stack.  */
static int
push_alignment (alignment, id)
     int alignment;
     tree id;
{
  switch (alignment)
    {
    case 0:
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
      break;
    default:
      warning ("\
Alignment must be a small power of two, not %d, in #pragma pack",
	       alignment);
      return 0;
    }
  
  if (alignment_stack == NULL
      || alignment_stack->alignment != alignment
      || id != NULL_TREE)
    {
      align_stack * entry;

      entry = (align_stack *) xmalloc (sizeof (* entry));

      entry->alignment  = alignment;
      entry->num_pushes = 1;
      entry->id         = id;
      entry->prev       = alignment_stack;
      
      /* The current value of maximum_field_alignment is not necessarily 
	 0 since there may be a #pragma pack(<n>) in effect; remember it 
	 so that we can restore it after the final #pragma pop(). */
      if (alignment_stack == NULL)
	default_alignment = maximum_field_alignment;
      
      alignment_stack = entry;

      maximum_field_alignment = alignment * BITS_PER_UNIT;
    }
  else
    alignment_stack->num_pushes ++;

  return 1;
}

/* Undo a push of an alignment onto the stack.  */
static int
pop_alignment (id)
     tree id;
{
  align_stack * entry;
      
  if (alignment_stack == NULL)
    {
      warning ("\
#pragma pack (pop) encountered without matching #pragma pack (push, <n>)"
	       );
      return 0;
    }

  /* If we got an identifier, strip away everything above the target
     entry so that the next step will restore the state just below it.  */
  if (id)
    {
      for (entry = alignment_stack; entry; entry = entry->prev)
	if (entry->id == id)
	  {
	    entry->num_pushes = 1;
	    alignment_stack = entry;
	    break;
	  }
      if (entry == NULL)
	warning ("\
#pragma pack(pop, %s) encountered without matching #pragma pack(push, %s, <n>)"
		 , IDENTIFIER_POINTER (id), IDENTIFIER_POINTER (id));
    }

  if (-- alignment_stack->num_pushes == 0)
    {
      entry = alignment_stack->prev;

      if (entry == NULL)
	maximum_field_alignment = default_alignment;
      else
	maximum_field_alignment = entry->alignment * BITS_PER_UNIT;

      free (alignment_stack);

      alignment_stack = entry;
    }

  return 1;
}
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */

/* Handle one token of a pragma directive.  TOKEN is the current token, and
   STRING is its printable form.  Some front ends do not support generating
   tokens, and will only pass in a STRING.  Also some front ends will reuse
   the buffer containing STRING, so it must be copied to a local buffer if
   it needs to be preserved.

   If STRING is non-NULL, then the return value will be ignored, and there
   will be futher calls to handle_pragma_token in order to handle the rest of
   the line containing the #pragma directive.  If STRING is NULL, the entire
   line has now been presented to handle_pragma_token and the return value
   should be zero if the pragma flawed in some way, or if the pragma was not
   recognised, and non-zero if it was successfully handled.  */

int
handle_pragma_token (string, token)
     const char * string;
     tree token;
{
  static enum pragma_state state = ps_start;
  static enum pragma_state type;
#ifdef HANDLE_PRAGMA_WEAK
  static char * value;
#endif
  static tree id;
  static unsigned int align;
  static char * name;

  static char *idaa_name;
  static char *idaa_id;
  static tree idaa_id_tree;
  static int idaa_arg1;
  static int idaa_arg2;
  tree idaa_decl;
  
  PRAGMA_INFO *pi;

  int lookup_decl;


  /* If we have reached the end of the #pragma directive then
     determine what value we should return.  */

  if (string == NULL)
    {
      int ret_val = 0;

      switch (type)
	{
	default:
	  abort ();
	  break;

	case ps_done:
	  /* The pragma was not recognised.  */
	  break;
	  
#ifdef HANDLE_PRAGMA_PACK	  
	case ps_pack:
	  if (state == ps_right)
	    {
	      maximum_field_alignment = align * BITS_PER_UNIT;
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
	      default_alignment = maximum_field_alignment;
#endif
	      ret_val = 1;
	    }
	  else
	    warning ("malformed `#pragma pack'");
	  break;
#endif /* HANDLE_PRAGMA_PACK */
	  
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
	case ps_push:
	  if (state == ps_right)
	    ret_val = push_alignment (align, id);
	  else
	    warning ("malformed '#pragma pack(push[,id],<n>)'");
	  break;
	  
	case ps_pop:
	  if (state == ps_right)
	    ret_val = pop_alignment (id);
	  else
	    warning ("malformed '#pragma pack(pop[,id])'");
	  break;
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */
	  
#ifdef HANDLE_PRAGMA_WEAK
	case ps_weak:
	  if (HANDLE_PRAGMA_WEAK)
	    {
	      if (state == ps_name)
		ret_val = add_weak (name, NULL);
	      else if (state == ps_value)
		ret_val = add_weak (name, value);
	      else
		warning ("malformed `#pragma weak'");
	    }
	  else
	    ret_val = 1; /* Ignore the pragma.  */
	  break;
#endif /* HANDLE_PRAGMA_WEAK */

	case ps_idaa:
	  idaa_decl=NULL_TREE;
	  lookup_decl = 0;

	  if (strcmp(idaa_name,"aligned")==0 && state == ps_idaa_right_arg1) {
	    if (idaa_arg1 == 0 || (idaa_arg1&(idaa_arg1-1))!=0)
	      warning("invalid alignment (%d) in pragma 'aligned'",idaa_arg1);
	    else {
	      lookup_decl = 1;
	      ret_val = 1;
	    }
	  } else
	    warning("malformed pragma '%s'",idaa_name);
	  
	  if (ret_val && lookup_decl) {
	    idaa_decl=lookup_name(idaa_id_tree,0);
	    if (!idaa_decl) {
	      ret_val=0;
	      warning("cannot find declaration of '%s' in pragma '%s'",idaa_id,idaa_name);
	    }
	  }

	  if (ret_val) {
	    pi=new_pragma_info();
	    pi->name=idaa_name;
	    pi->id=idaa_id;
	    pi->decl=idaa_decl;
	    pi->arg1=idaa_arg1;
	    pi->arg2=idaa_arg2;
	    pragma_list_push(&new_pragmas,pi);
	  }
	  break;
	  
	case ps_poison:
	  ret_val = 1;
	  break;

#ifdef TARG_XTENSA
	case ps_frequency_hint:
	  if (state == ps_frequent || state == ps_never) {
	    pi = new_pragma_info();
	    pi->name = xstrdup("frequency_hint");
	    pi->id = xstrdup(state == ps_frequent ? "FREQUENT" : "NEVER");
	    pi->decl = NULL;
	    pi->arg1 = 0;
	    pi->arg2 = 0;
	    pragma_list_push(&new_pragmas, pi);
	    ret_val = 1;
	  } else {
	      warning("malformed frequency_hint pragam ");
	  }
	break;
	case ps_no_reorder_memory:
	case ps_flush_memory:
	case ps_no_reorder:
	case ps_flush:
	  pi = new_pragma_info();
	  pi->name = xstrdup(type==ps_no_reorder_memory?"no_reorder_memory":
			     (type==ps_flush_memory?"flush_memory":
			     (type==ps_no_reorder?"no_reorder":"flush")));
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_ivdep:
	  pi = new_pragma_info();
	  pi->name = xstrdup("ivdep");
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_concurrent:
	  pi = new_pragma_info();
	  pi->name = xstrdup("concurrent");
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_generate_hw:
	  pi = new_pragma_info();
	  pi->name = xstrdup("generate_hw");
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_simd:
	  pi = new_pragma_info();
	  pi->name = xstrdup("simd");
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_simd_if_convert:
	  pi = new_pragma_info();
	  pi->name = xstrdup("simd_if_convert");
	  pi->id = NULL;
	  pi->decl = NULL;
	  pi->arg1 = 0;
	  pi->arg2 = 0;
	  pragma_list_push(&new_pragmas, pi);
	  ret_val = 1;
	  break;
	case ps_super_swp_ii:
	case ps_super_swp_unroll:
	  warning("malformed or invalid '#pragma super_swp' ignored");
	  break;
	case ps_super_swp:
	  if (state == ps_bad) {
	    warning("malformed or invalid '#pragma super_swp' ignored");
	  } else {
	    pi = new_pragma_info();
	    pi->name = xstrdup("super_swp");
	    pi->id = NULL;
	    pi->decl = NULL;
	    pi->arg1 = super_swp_ii;
	    pi->arg2 = super_swp_unroll;
	    pragma_list_push(&new_pragmas, pi);
	  }
	  ret_val = 1;
	  break;
	case ps_swp_schedule:
	  if (state == ps_bad) {
	    warning("malformed (len>1024) or invalid '#pragma swp_schdule' ignored");
	  } else {
	    pi = new_pragma_info();
	    pi->name = xstrdup("swp_schedule");
	    pi->id = NULL;
	    pi->decl = NULL;
	    pi->arg1 = (int)xstrdup(swp_schedule_str);
	    pi->arg2 = 0;
	    pragma_list_push(&new_pragmas, pi);
	  }
	  ret_val = 1;
	  break;
#endif
	}

      type = state = ps_start;
      id = NULL_TREE;
      
      return ret_val;
    }

  /* If we have been given a token, but it is not an identifier,
     or a small constant, then something has gone wrong.  */
  if (token)
    {
      switch (TREE_CODE (token))
	{
	case IDENTIFIER_NODE:
	  break;
	  
	case INTEGER_CST:
	  if (TREE_INT_CST_HIGH (token) != 0)
	    return 0;
	  break;
	  
	default:
	  return 0;
	}
    }
      
  switch (state)
    {
    case ps_start:
      type = state = ps_done;
#ifdef HANDLE_PRAGMA_PACK
      if (strcmp (string, "pack") == 0)
	type = state = ps_pack;
#endif
#ifdef HANDLE_PRAGMA_WEAK
      if (strcmp (string, "weak") == 0)
	type = state = ps_weak;
#endif
      if (strcmp (string, "poison") == 0)
	type = state = ps_poison;
#ifdef TARG_XTENSA
      if (strcmp (string, "frequency_hint") == 0)
	type = state = ps_frequency_hint;
      else if (strcmp (string, "no_reorder_memory") == 0)
	type = state = ps_no_reorder_memory;
      else if (strcmp (string, "flush_memory") == 0)
	type = state = ps_flush_memory;
      else if (strcmp (string, "no_reorder") == 0)
	type = state = ps_no_reorder;
      else if (strcmp (string, "flush") == 0)
	type = state = ps_flush;
      else if (strcmp (string, "ivdep") == 0)
	type = state = ps_ivdep;
      else if (strcmp (string, "concurrent") == 0)
	type = state = ps_concurrent;
      else if (strcmp (string, "generate_hw") == 0)
	type = state = ps_generate_hw;
      else if (strcmp (string, "simd") == 0)
	type = state = ps_simd;
      else if (strcmp (string, "simd_if_convert") == 0)
	type = state = ps_simd_if_convert;
      else if (strcmp (string, "super_swp") == 0) {
	/* format:
	   #pragma super_swp ii=10, unroll=2
	*/
	type = state = ps_super_swp;
	super_swp_ii = 0;
	super_swp_unroll = 0;
      } else if (strcmp (string, "swp_schedule") == 0) {
	/* format:
	   #pragma swp_schedule ii=10, unroll=2, sched[18]= 0 1 1 2 3 5 6 6 7 3 4 4 7 8 9 15 10 12
	*/
	if (swp_schedule_str==NULL) {
	  swp_schedule_str=xmalloc(1024);
	}
	swp_schedule_str_len = 0;
	swp_schedule_str[swp_schedule_str_len]='\0';
	type = state = ps_swp_schedule;
      }

#endif
      /* any other supported idaa pragmas here and in wfe_stmt.cxx */
      if (strcmp (string, "aligned") == 0) {
	idaa_name = xstrdup(string);
	idaa_id = NULL;
	idaa_id_tree = NULL_TREE;
	idaa_arg1 = idaa_arg2 = 0;
	type = state = ps_idaa;
      }
      break;

#ifdef HANDLE_PRAGMA_WEAK
    case ps_weak:
      name = xstrdup (string);
	  state = ps_name;
      break;
      
    case ps_name:
      state = (strcmp (string, "=") ? ps_bad : ps_equals);
      break;

    case ps_equals:
      value = xstrdup (string);
      state = ps_value;
      break;

    case ps_value:
      state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_WEAK */
      
#ifdef HANDLE_PRAGMA_PACK
    case ps_pack:
      state = (strcmp (string, "(") ? ps_bad : ps_left);
      break;

    case ps_left:

      if (token == NULL_TREE)
	{
	  /* #pragma pack () resets packing rules to their
	     defaults.  */
	  if (strcmp (string, ")") == 0)
	    {
	      align = 0;
	      state = ps_right;
	    }
	  else
	    state = ps_bad;
	}
      else if (TREE_CODE (token) == INTEGER_CST)
	goto handle_align;

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
      else if (TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  if (strcmp (string, "push") == 0)
	    type = state = ps_push;
	  else if (strcmp (string, "pop") == 0)
	    type = state = ps_pop;
	  else
	    state = ps_bad;
	}
#endif
      else
	state = ps_bad;
      break;

    handle_align:
      switch (tree_log2 (token))
	{
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	  state = ps_align;
	  align = 1 << tree_log2 (token);
	  break;

	default:
	  state = ps_bad;
	  break;
	}
      break;

    case ps_align:
      state = (strcmp (string, ")") ? ps_bad : ps_right);
      break;

    case ps_right:
      state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_PACK */

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
    case ps_push:
      state = (strcmp (string, ",") ? ps_bad : ps_pushcomma);
      break;

    case ps_pushid:
      state = (strcmp (string, ",") ? ps_bad : ps_pushcomma2);
      break;

    case ps_pushcomma:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  id = token;
	  state = ps_pushid;
	  break;
	}

      /* else fall through */
    case ps_pushcomma2:
      if (token && TREE_CODE (token) == INTEGER_CST)
	goto handle_align;
      else
	state = ps_bad;
      break;

    case ps_pop:
      if (strcmp (string, ",") == 0)
	state = ps_popcomma;
      else
	state = (strcmp (string, ")") ? ps_bad : ps_right);
      break;

    case ps_popcomma:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  id = token;
	  state = ps_align;
	}
      else
	state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */

    case ps_poison:
      if (token && TREE_CODE (token) != IDENTIFIER_NODE)
	state = ps_bad;
      break;

#ifdef TARG_XTENSA
    case ps_frequency_hint:
      if (token && TREE_CODE (token) != IDENTIFIER_NODE)
	state = ps_bad;
      else if (strcasecmp (string, "FREQUENT") == 0)
	state = ps_frequent;
      else if (strcasecmp (string, "NEVER") == 0)
	state = ps_never;
      else
        state = ps_bad;
      break;

    case ps_frequent:
    case ps_never:
    case ps_no_reorder_memory:
    case ps_flush_memory:
    case ps_no_reorder:
    case ps_flush:
    case ps_ivdep:
    case ps_concurrent:
    case ps_generate_hw:
    case ps_simd:
    case ps_simd_if_convert:
      state = ps_bad;
      break;
    case ps_super_swp:
      if (strncasecmp (string, "ii", 2) == 0) {
	state = ps_super_swp_ii;
      } else if (strncasecmp (string, ",", 1) == 0) {
      } else if (strncasecmp (string, "unroll", 2) == 0) {
	state = ps_super_swp_unroll;
      } else
	state = ps_bad;
      break;
    case ps_super_swp_ii:
      if (strncasecmp (string, "=", 1) == 0) {
	break;
      } else {
	char* p;
	super_swp_ii = strtol(string, &p, 10);
	if (*p != '\0' || !(super_swp_ii>=0 && super_swp_ii<300))
	  state = ps_bad;
        else
	  state = ps_super_swp;
      }
      break;
    case ps_super_swp_unroll:
      if (strncasecmp (string, "=", 1) == 0) {
	break;
      } else {
	char* p;
	super_swp_unroll = strtol(string, &p, 10);
	if (*p != '\0' ||
	    (super_swp_unroll != 1 &&
	     super_swp_unroll != 2 &&
	     super_swp_unroll != 4 &&
	     super_swp_unroll != 8 &&
	     super_swp_unroll != 16 &&
	     super_swp_unroll != 32))
	  state = ps_bad;
	else
	  state = ps_super_swp;
      }
      break;
    case ps_swp_schedule:
      if (swp_schedule_str_len+strlen(string)+1<1024) {
	strcpy(&swp_schedule_str[swp_schedule_str_len], string);
	swp_schedule_str_len+=strlen(string);
	swp_schedule_str[swp_schedule_str_len++]=' ';
	swp_schedule_str[swp_schedule_str_len]='\0';
      } else
	state = ps_bad;
      break;
#endif
	 
      /*** process #pragma name([id[,arg1[,arg2]]]) ***/
      
    case ps_idaa:
      state = (strcmp (string, "(") ? ps_bad : ps_idaa_left);
      break;
      
    case ps_idaa_left:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE) {
	idaa_id = xstrdup (string);
	idaa_id_tree = token;
	state = ps_idaa_id;
      } else
	state = (strcmp (string, ")") ? ps_bad : ps_idaa_right);
      break;
      
    case ps_idaa_id:
      state = (strcmp (string, ",") ?
	       (strcmp (string, ")") ? ps_bad : ps_idaa_right_id) : ps_idaa_comma1);
      break;
      
    case ps_idaa_comma1:
      if (token && TREE_CODE (token) == INTEGER_CST) {
	idaa_arg1 = TREE_INT_CST_LOW(token);
	state = ps_idaa_arg1;
      } else
	state = ps_bad;
      break;

    case ps_idaa_arg1:
      state = (strcmp (string, ",") ?
	       (strcmp (string, ")") ? ps_bad : ps_idaa_right_arg1) : ps_idaa_comma2);
      break;
      
    case ps_idaa_comma2:
      if (token && TREE_CODE (token) == INTEGER_CST) {
	idaa_arg2 = TREE_INT_CST_LOW(token);
	state = ps_idaa_arg2;
      } else
	state = ps_bad;
      break;

    case ps_idaa_arg2:
      state = strcmp (string, ")") ? ps_bad : ps_idaa_right_arg2;
      break;
      
    case ps_idaa_right:
    case ps_idaa_right_id:
    case ps_idaa_right_arg1:
    case ps_idaa_right_arg2:
      state = ps_bad;
      break;

      /*** end #pragma name([id[,arg1[,arg2]]]) ***/
      
    case ps_bad:
    case ps_done:
      break;

    default:
      abort ();
    }

  return 1;
}
#endif /* HANDLE_GENERIC_PRAGMAS */

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
static void
mark_align_stack (p)
    void *p;
{
  align_stack *a = *(align_stack **) p;

  while (a)
    {
      ggc_mark_tree (a->id);
      a = a->prev;
    }
}
#endif

void
init_pragma ()
{
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
  ggc_add_root (&alignment_stack, 1, sizeof(alignment_stack),
		mark_align_stack);
#endif
}
