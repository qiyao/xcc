/* Assembly functions for libgcc2.
   Copyright (C) 2001, 2006 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.
   Copyright (C) 2003-2006 Tensilica, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include <xtensa/coreasm.h>

#if !XCHAL_HAVE_WINDOWED || __XTENSA_CALL0_ABI__
	.text
	.align	4
	.global	__xtensa_libgcc_window_spill
	.type	__xtensa_libgcc_window_spill, @function
__xtensa_libgcc_window_spill:
	ret
	.size	__xtensa_libgcc_window_spill, .-__xtensa_libgcc_window_spill

/* __throw and __rethrow:  Save all callee-saved registers
   so the structure starts with known good values.  Technically,
   we should only have to do this with the frame pointer, but it
   simplifies the code quite a bit to do it with all of them now.

   Also pass in the ra and sp of the caller, which
   simplifies the world a lot, too.

   typedef struct reg_restore_struct
   {
     void *ra;
     void *sp;
     void *callee_saves[4];
   } reg_restore_struct;

   extern void __throw (void);
   extern void __rethrow (void *index);
 */

	.section .xt_except_table, "a"
	.type	.eh_table.__throw, @object
	.size	.eh_table.__throw, 24
	.align 4
.eh_table.__throw:
	.long	__throw
	.long	.L__throw_end
	.short	6		/* XT_EH_TYPE_RESTORE_CALLEE_SPREL */
	.byte	8		/* frame size (32 / 4) */
	.byte	0		/* offset to ra (0 / 4) */
	.byte	0		/* no callee saved registers used */
	.byte	0
	.byte	0
	.byte	0
	.long	-1

	.section .xt_except_desc, "wa"
	.align	0
	.type	.eh_desc.__throw, @object
	.size	.eh_desc.__throw, 12
.eh_desc.__throw:
	.long	__throw
	.long	.L__throw_end
	.long	.eh_table.__throw

	.text
	.align 4
	.global __throw
	.type   __throw, @function
__throw:
	addi	a1, a1, -32	/* allocate reg_restore struct */
	/* now fill it out */
	s32i	a0, a1, 0	/* save ra */
	s32i	a1, a1, 4	/* save sp */
	s32i	a12, a1, 8	/* save callee saved regs */
	s32i	a13, a1, 12
	s32i	a14, a1, 16
	s32i	a15, a1, 20

	/* if (__get_eh_context()->info == NULL) __terminate() */
	call0	__get_eh_context
	mov	a12, a2		/* we'll need eh after the call */
	l32i	a5, a12, 8
	bnez	a5, .L_found_context
	call0	__terminate

.L_found_context:
	/* It is easier if we just use the unwind mechanism
	   to unwind the stack through __throw, rather than
	   passing the thrower's sp, ra and pc immediately.  */

	/* handler_frame_info handler_info =
	     find_exception_handler (pc, sp, ra, eh_info, 0, rr); */

	l32i	a4, a1, 0	/* load ra */
	movi	a2, .L_found_context /* load a suitable pc */
	mov	a3, a1	        /* mov sp into param position */
	/* a5 already has eh_context->info */
	movi	a6, 0		/* not a rethrow */
	mov	a7, a1		/* pointer to the reg_restore_struct */
	call0	__find_exception_handler

        /* if (!handler_info.handler) __terminate (); */
	bnez	a2, .L_found_handler
	call0	__terminate

.L_found_handler:

	s32i	a2, a12, 0	/* eh->handler_label = handler_info.handler; */

	mov	a4, a2		/* twizzle the params into the right place */
	mov	a3, a2
	mov	a2, a4
	mov	a4, a1
	call0	__xtensa_nonlocal_goto
.L__throw_end:
	.size	__throw, .-__throw


	.section .xt_except_table, "a"
	.type	.eh_table.__throw, @object
	.size	.eh_table.__throw, 24
	.align	4
.eh_table.__rethrow:
	.long	__rethrow
	.long	.L__rethrow_end
	.long	6		/* XT_EH_TYPE_RESTORE_CALLEE_SPREL */
	.short	8		/* frame size (32 / 4) */
	.short	0		/* offset to ra (0 / 4) */
	.short	0		/* no callee saved registers used */
	.short	0
	.short	0
	.short	0
	.long	-1

	.section .xt_except_desc, "wa"
	.align	4
	.type	.eh_desc.__rethrow, @object
	.size	.eh_desc.__rethrow, 12
.eh_desc.__rethrow:
	.long	__rethrow
	.long	.L__rethrow_end
	.long	.eh_table.__rethrow

	.text
	.align	4
	.global	__rethrow
	.type	__rethrow, @function
__rethrow:
	addi	a1, a1, -32	/* allocate reg_restore struct */
	/* now fill it out */
	s32i	a0, a1, 0	/* save ra */
	s32i	a11, a1, 4	/* save sp */
	s32i	a12, a1, 8	/* save callee saved regs */
	s32i	a13, a1, 12
	s32i	a14, a1, 16
	s32i	a15, a1, 20

	mov	a13, a2		/* save index */
	/* if (__get_eh_context()->info == NULL) __terminate() */
	call0	__get_eh_context
	mov	a12, a2		/* we'll need eh after the call */
	l32i	a5, a12, 8
	bnez	 a5, .L_found_context_r
	call0	__terminate

.L_found_context_r:
	/* eh->table_info = index */
	s32i a13, a12, 12
	/* It is easier if we just use the unwind mechanism
	   to unwind the stack through __throw, rather than
	   passing the thrower's sp, ra and pc immediately.  */

	/* handler_frame_info handler_info =
	     find_exception_handler (pc, sp, ra, eh_info, 0); */

	l32i	a4, a1, 0	/* load ra */
	movi	a2, .L_found_context_r /* load a suitable pc */
	mov	a3, a1		/* mov sp into position */
	/* a5 already has eh_context->info */
	movi	a6, 1		/* rethrow */
	mov	a7, a1		/* pointer to the reg_restore_struct */
	call0	__find_exception_handler

        /*  if (!handler_info.handler) __terminate (); */
	bnez	a2, .L_found_handler_r
	call0	__terminate

.L_found_handler_r:

	s32i	a2, a12, 0	/* eh->handler_label = handler_info.handler; */
	movi	a14, 0
	s32i	a14, a13, 12	/* eh->table_info = NULL; */
	mov	a4, a2		/* twizzle the params into the right place */
	mov	a3, a2
	mov	a2, a4
	mov	a4, a1
	call0	__xtensa_nonlocal_goto
.L__rethrow_end:
	.size	__rethrow, .-__rethrow


/* __xtensa_nonlocal_goto: This code does all the hard work of a
   nonlocal goto on Xtensa.  It is here in the library to avoid the
   code size bloat of generating it in-line.

   This function never returns to its caller but instead goes directly
   to the address of the specified goto handler.

   This code also restores the callee saved registers to their correct
   values.

	a2 = stack pointer for the procedure containing the label
	a3 = goto handler address
	a4 = pointer to a reg_restore_struct from xtensa_eh.cc
 */

	.align	4
	.global	__xtensa_nonlocal_goto
	.type	__xtensa_nonlocal_goto,@function
__xtensa_nonlocal_goto:
	l32i	a0, a4, 0
	l32i	a2, a4, 4
	l32i	a12, a4, 8
	l32i	a13, a4, 12
	l32i	a14, a4, 16
	l32i	a15, a4, 20
	/* Have to do the sp last because a4 points into the stack
	   somwehere.  */
	mov	a1, a2

	/* We are going on to the goto handler.  */
	jx	a3
	.size	__xtensa_nonlocal_goto, .-__xtensa_nonlocal_goto


/* __xtensa_trampoline_template: This code is used by XCC when taking
   the address of a nested function.  The trampoline is copied onto the
   stack and the function address and static link pointer are pasted in.

   Avoid all references to optional instructions.  */

/* Keep this in sync with wn_lower.cxx:lower_nested_function_addr.  */

	.section .rodata
	.begin	no-transform
	.align	4
	.global	__xtensa_trampoline_template
__xtensa_trampoline_template:
	/* Save the return address.  */
	or	a10, a0, a0	/* 0-2 */
	call0	.L_skip_data	/* 3-5 */

.L_data_start:
	.align	4		/* 2 bytes of padding (6-7) */
	.word	0		/* .Lchainval */
	.word	0		/* .Lfnaddr */

.L_skip_data:
	/* a0 now has the address of .L_data_start.
	   Patch it up to point to .LchainVal.  */
	addi	a0, a0, 2

	/* Get the static chain into the static chain register.  */
	l32i	a8, a0, 0	/* .Lchainval */

	/* Jump to the target function.  */
	l32i	a9, a0, 4	/* .Lfnaddr */
	or      a0, a10, a10	/* restore ra */
	jx      a9
	.end	no-transform


/* __xtensa_sync_caches: This function is called after writing a trampoline
   on the stack to force all the data writes to memory and invalidate the
   instruction cache.

   a2 is the address of the new trampoline.

   After the trampoline data is written out, it must be flushed out of
   the data cache into memory.  We use DHWB in case we have a writeback
   cache.  At least one DHWB instruction is needed for each data cache
   line which may be touched by the trampoline.  An ISYNC instruction
   must follow the DHWBs.

   We have to flush the i-cache to make sure that the new values get used.
   At least one IHI instruction is needed for each i-cache line which may
   be touched by the trampoline.  An ISYNC instruction is also needed to
   make sure that the modified instructions are loaded into the instruction
   fetch buffer.  */

/* Keep this in sync with wn_lower.cxx:lower_nested_function_addr.  */

#define TRAMPOLINE_SIZE 34

	.text
	.align	4
	.global	__xtensa_sync_caches
	.type	__xtensa_sync_caches,@function
__xtensa_sync_caches:
#if XCHAL_DCACHE_SIZE > 0
	/* Flush the trampoline from the data cache.  */
	extui	a4, a2, 0, XCHAL_DCACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_DCACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_DCACHE_LINEWIDTH
	mov	a3, a2
.Ldcache_loop:
	dhwb	a3, 0
	addi	a3, a3, (1 << XCHAL_DCACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Ldcache_loop
	isync
#endif
#if XCHAL_ICACHE_SIZE > 0
	/* Invalidate the corresponding lines in the instruction cache.  */
	extui	a4, a2, 0, XCHAL_ICACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_ICACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_ICACHE_LINEWIDTH
.Licache_loop:
	ihi	a2, 0
	addi	a2, a2, (1 << XCHAL_ICACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Licache_loop
#endif
	isync
	nop
	ret
	.size	__xtensa_sync_caches, .-__xtensa_sync_caches

#else /* XCHAL_HAVE_WINDOWED && ! __XTENSA_CALL0_ABI__ */

/* __xtensa_libgcc_window_spill: This function flushes out all but the
   current register window.  This is used to set up the stack so that
   arbitrary frames can be accessed.  */

	.align	4
	.global	__xtensa_libgcc_window_spill
	.type	__xtensa_libgcc_window_spill,@function
__xtensa_libgcc_window_spill:
	entry	sp, 32
	movi	a2, 0
	syscall
	retw
	.size	__xtensa_libgcc_window_spill, .-__xtensa_libgcc_window_spill


/* __xtensa_nonlocal_goto: This code does all the hard work of a
   nonlocal goto on Xtensa.  It is here in the library to avoid the
   code size bloat of generating it in-line.  There are three
   arguments:

	a2 = frame pointer for the procedure containing the label
	a3 = goto handler address
	a4 = nonlocal label address

  This function never returns to its caller but instead goes directly
  to the address of the specified goto handler.  */

	.align	4
	.global	__xtensa_nonlocal_goto
	.type	__xtensa_nonlocal_goto,@function
__xtensa_nonlocal_goto:
	entry	sp, 32

	/* Flush registers.  */
	mov	a5, a2
	movi	a2, 0
	syscall
	mov	a2, a5

	/* Because the save area for a0-a3 is stored one frame below
	   the one identified by a2, the only way to restore those
	   registers is to unwind the stack.  If alloca() were never
	   called, we could just unwind until finding the sp value
	   matching a2.  However, a2 is a frame pointer, not a stack
	   pointer, and may not be encountered during the unwinding.
	   The solution is to unwind until going _past_ the value
	   given by a2.  This involves keeping three stack pointer
	   values during the unwinding:

		next = sp of frame N-1
		cur = sp of frame N
		prev = sp of frame N+1

	   When next > a2, the desired save area is stored relative
	   to prev.  At this point, cur will be the same as a2
	   except in the alloca() case.

	   Besides finding the values to be restored to a0-a3, we also
	   need to find the current window size for the target
	   function.  This can be extracted from the high bits of the
	   return address, initially in a0.  As the unwinding
	   proceeds, the window size is taken from the value of a0
	   saved _two_ frames below the current frame.  */

	addi	a5, sp, -16	/* a5 = prev - save area */
	l32i	a6, a5, 4
	addi	a6, a6, -16	/* a6 = cur - save area */
	mov	a8, a0		/* a8 = return address (for window size) */
	j	.Lfirstframe

.Lnextframe:
	l32i	a8, a5, 0	/* next return address (for window size) */
	mov	a5, a6		/* advance prev */
	addi	a6, a7, -16	/* advance cur */
.Lfirstframe:
	l32i	a7, a6, 4	/* a7 = next */
	bge	a2, a7, .Lnextframe

	/* Store the nonlocal label address in the static chain slot
	   (relative to cur) where the goto handler can grab it. */
	addi	a7, a6, -4
	s32i	a4, a7, 0

	/* At this point, prev (a5) points to the save area with the saved
	   values of a0-a3.  Copy those values into the save area at the
	   current sp so they will be reloaded when the return from this
	   function underflows.  We don't have to worry about exceptions
	   while updating the current save area, because the windows have
	   already been flushed.  */

	addi	a4, sp, -16	/* a4 = save area of this function */
	l32i	a6, a5, 0
	l32i	a7, a5, 4
	s32i	a6, a4, 0
	s32i	a7, a4, 4
	l32i	a6, a5, 8
	l32i	a7, a5, 12
	s32i	a6, a4, 8
	s32i	a7, a4, 12

	/* Set return address to goto handler.  Use the window size bits
	   from the return address two frames below the target.  */
	extui	a8, a8, 30, 2	/* get window size from return addr. */
	slli	a3, a3, 2	/* get goto handler addr. << 2 */
	ssai	2
	src	a0, a8, a3	/* combine them with a funnel shift */

	retw
	.size	__xtensa_nonlocal_goto, .-__xtensa_nonlocal_goto


/* __xtensa_trampoline_template: This code is used by XCC when taking
   the address of a nested function.  The trampoline is copied onto the
   stack and the function address and static link pointer are pasted in.
   This template is copied from gcc/config/xtensa.h.

   For windowed abi, the trampoline must perform an ENTRY instruction with a
   minimal stack frame in order to get some free registers.  Once the
   actual call target is known, the proper stack frame size is extracted
   from the ENTRY instruction at the target and the current frame is
   adjusted to match.  The trampoline then transfers control to the
   instruction following the entry at the target.  Note: this assumes
   that the target begins with an ENTRY instruction.

   This code also needs to avoid any optional instructions or
   instructions that may have differing semantics from config to config
   such as L32R and const16.  */

/* minimum frame = reg save area (4 words) plus static chain (1 word)
   and the total number of words must be a multiple of 128 bits */
#define MIN_FRAME_SIZE 32

#if XCHAL_HAVE_BE
#define XTENSA_ENTRY_OFFSET 8
#else /* ! XCHAL_HAVE_BE */
#define XTENSA_ENTRY_OFFSET 12
#endif /* ! XCHAL_HAVE_BE */

	.section .rodata
	.begin	no-transform
	.align	4
	.global	__xtensa_trampoline_template
__xtensa_trampoline_template:
	entry	sp, MIN_FRAME_SIZE
	/* save the return address */
	or	a10, a0, a0
	call0	.L_skip_data

.L_data_start:
	.align	4		/* three bytes of padding */
	.word	0		/* .Lchainval */
	.word	0		/* .Lfnaddr */

.L_skip_data:
	/* a0 now has the address of .L_data_start */

	/* patch it up to point to .LchainVal */
	addi	a0, a0, 3

	/* store the static chain */
	l32i	a8, a0, 0	/* .Lchainval */
	s32i	a8, sp, (MIN_FRAME_SIZE-20)

	/* set the proper stack pointer value */
	l32i	a8, a0, 4	/* .Lfnaddr */
	l32i	a9, a8, 0
	extui	a9, a9, XTENSA_ENTRY_OFFSET, 12
	slli	a9, a9, 3
	addi	a9, a9, -(MIN_FRAME_SIZE)
	sub	a9, sp, a9
	movsp	sp, a9

	/* restore the return address */
	or	a0, a10, a10

	/* jump to the instruction following the entry */
	addi	a8, a8, 3
	jx	a8
	.end	no-transform


/* __xtensa_sync_caches: This function is called after writing a trampoline
   on the stack to force all the data writes to memory and invalidate the
   instruction cache. a2 is the address of the new trampoline.

   After the trampoline data is written out, it must be flushed out of
   the data cache into memory.  We use DHWB in case we have a writeback
   cache.  At least one DHWB instruction is needed for each data cache
   line which may be touched by the trampoline.  An ISYNC instruction
   must follow the DHWBs.

   We have to flush the i-cache to make sure that the new values get used.
   At least one IHI instruction is needed for each i-cache line which may
   be touched by the trampoline.  An ISYNC instruction is also needed to
   make sure that the modified instructions are loaded into the instruction
   fetch buffer.  */

/* keep this in sync with wn_lower.cxx:lower_nested_function_addr */
#define TRAMPOLINE_SIZE 59

	.text
	.align	4
	.global	__xtensa_sync_caches
	.type	__xtensa_sync_caches,@function
__xtensa_sync_caches:
	entry 	sp, 32
#if XCHAL_DCACHE_SIZE > 0
	/* Flush the trampoline from the data cache.  */
	extui	a4, a2, 0, XCHAL_DCACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_DCACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_DCACHE_LINEWIDTH
	mov	a3, a2
.Ldcache_loop:
	dhwb	a3, 0
	addi	a3, a3, (1 << XCHAL_DCACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Ldcache_loop
	isync
#endif
#if XCHAL_ICACHE_SIZE > 0
	/* Invalidate the corresponding lines in the instruction cache.  */
	extui	a4, a2, 0, XCHAL_ICACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_ICACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_ICACHE_LINEWIDTH
.Licache_loop:
	ihi	a2, 0
	addi	a2, a2, (1 << XCHAL_ICACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Licache_loop
#endif
	isync
	isync_retw_nop
	retw
	.size	__xtensa_sync_caches, .-__xtensa_sync_caches

#endif /* XCHAL_HAVE_WINDOWED && ! __XTENSA_CALL0_ABI__ */
