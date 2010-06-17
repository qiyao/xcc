/* GNU C varargs support for Tensilica's Xtensa architecture.
   Copyright (C) 2002, 2004 Tensilica, Inc.  All Rights Reserved.  */

/* Define __gnuc_va_list.  */

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

/* The __gnuc_va_list struct is set up by __builtin_saveregs.  The
   __va_reg field points to a stack-allocated region holding the
   contents of the incoming argument registers.  The __va_ndx field is
   an index initialized to the position of the first unnamed
   (variable) argument.  This same index is also used to address the
   arguments passed in memory.  Thus, the __va_stk field is
   initialized to point to the position of the first argument in
   memory offset to account for the arguments passed in registers and
   to account for the size of the argument registers not being 16-byte
   aligned.  E.G., there are 6 argument registers of 4 bytes each, but
   we want the __va_ndx for the first stack argument to have the
   maximal alignment of 16 bytes, so we offset the __va_stk address by
   32 bytes so that __va_stk[32] references the first argument on the
   stack.  */
   
typedef struct
{
    char *__va_stk;		/* args passed on stack - 32 */
    char *__va_reg;		/* args passed in regs */
    int  __va_ndx;
} __gnuc_va_list;
#endif /* not __GNUC_VA_LIST */

/* If this is for internal libc use, don't define anything but
   __gnuc_va_list.  */
#if defined (_STDARG_H) || defined (_VARARGS_H)

#ifdef _STDARG_H /* stdarg.h support */

/* Call __builtin_next_arg even though we aren't using its value, so that
   we can verify that LASTARG is correct.  */
#define va_start(AP,LASTARG)				\
 (__builtin_next_arg (LASTARG),				\
  (AP) = *(__gnuc_va_list *)__builtin_saveregs())

#else /* varargs.h support */

#define va_start(AP) ((AP) = *(__gnuc_va_list *)__builtin_saveregs())
#define va_alist __va_1st_arg
#define va_dcl register int va_alist;...

#endif /* _STDARG_H */

#define __MAX_ARGS_IN_REGISTERS 6


/* get the type size rounded up to the nearest word */

#define	__va_size(TYPE) ((sizeof(TYPE) + 3) & -4)


/* if necessary, align the index */

#define __va_align(AP, TYPE)						\
    if (__alignof__ (TYPE) > 4)						\
      (AP).__va_ndx = (((AP).__va_ndx + __alignof__ (TYPE) - 1)		\
		       & -__alignof__ (TYPE));


/* given the base array pointer and index to the subsequent argument,
   return the address of the argument; the results are endian-dependent
   because values smaller than one word are aligned differently for big
   and little endian systems */

#ifdef __XTENSA_EB__
#define __va_addr(AP, ARRAY, TYPE)					\
    ((ARRAY) + (AP).__va_ndx						\
     - (sizeof (TYPE) < 4 ? sizeof (TYPE) : __va_size (TYPE)))
#else /* !__XTENSA_EB__ */
#define __va_addr(AP, ARRAY, TYPE)					\
    ((ARRAY) + (AP).__va_ndx - __va_size (TYPE))
#endif /* __XTENSA_EB__ */


/* get a variable argument: increment __va_ndx to point past the argument;
   determine whether the argument is in registers or on the stack (never
   split between the two); get the address; and dereference it */

#define va_arg(AP, TYPE)						\
__extension__								\
(*({char * __array;							\
    int orig_ndx;							\
    __va_align (AP, TYPE);						\
    orig_ndx = (AP).__va_ndx;						\
    (AP).__va_ndx += __va_size (TYPE);					\
    if ((AP).__va_ndx <= __MAX_ARGS_IN_REGISTERS * 4) {			\
        __array = (AP).__va_reg;					\
    } else {								\
	if (orig_ndx <= __MAX_ARGS_IN_REGISTERS * 4)			\
	    (AP).__va_ndx = 32 + __va_size (TYPE);			\
	__array = (AP).__va_stk;					\
    }									\
    (TYPE *) __va_addr (AP, __array, TYPE); }))


#define va_end(AP)	((void)0)


/* Copy __gnuc_va_list into another variable of this type.  */
#define __va_copy(dest, src) (dest) = (src)

#endif /* defined (_STDARG_H) || defined (_VARARGS_H) */
