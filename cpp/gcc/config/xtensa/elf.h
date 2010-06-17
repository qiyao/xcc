/* Xtensa/Elf configuration.
   Derived from the configuration for GCC for Intel i386 running Linux.
   Copyright (C) 2001,2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#define TARGET_SECTION_TYPE_FLAGS xtensa_multibss_section_type_flags

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

#undef TARGET_VERSION
#define TARGET_VERSION fputs (" (Xtensa/ELF)", stderr);

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#define ASM_NAME_SPEC "xt-as"

#undef ASM_SPEC
#define ASM_SPEC "%{v} %{mno-density:--no-density} \
                  %{mtext-section-literals:--text-section-literals} \
                  %{mno-text-section-literals:--no-text-section-literals} \
		  %{mtarget-align:--target-align} \
		  %{mno-target-align:--no-target-align} \
		  %{mlongcalls:--longcalls} \
		  %{mno-longcalls:--no-longcalls} \
                  %{mrename-section-*:--rename-section %*} \
                  %{mno-autobundle:--no-autobundle} \
                  %{!mno-autobundle:%{-O2:--autobundle}%{-O3:--autobundle}}"

#undef ASM_FINAL_SPEC

#undef LIB_SPEC
#define LIB_SPEC "-lc -lxt"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s crtbegin%O%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"  

#undef LINK_SPEC
#define LINK_SPEC \
 "%{mlsp=*:--multilib-dir %*} \
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
    %{static:-static}}}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__XTENSA__ -D__ELF__ -Acpu=xtensa -Amachine=xtensa"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"."

/* Avoid dots for compatibility with VxWorks.  */
#undef NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

/* Don't switch sections in the middle of a literal pool! */
#undef SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE,RTX,ALIGN)
  
/* Do not force "-fpic" for this target.  */
#define XTENSA_ALWAYS_PIC 0

/* Redefine the standard ELF version of ASM_DECLARE_FUNCTION_SIZE to
   allow adding the ".end literal_prefix" directive at the end of the
   function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	{							\
	  char label[256];					\
	  static int labelno;					\
	  							\
	  labelno++;						\
	  							\
	  ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);	\
	  ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);	\
	  							\
	  fprintf (FILE, "%s", SIZE_ASM_OP);			\
	  assemble_name (FILE, (FNAME));			\
	  fprintf (FILE, ",");					\
	  assemble_name (FILE, label);				\
	  fprintf (FILE, "-");					\
	  assemble_name (FILE, (FNAME));			\
	  putc ('\n', FILE);					\
	}							\
      XTENSA_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL);		\
    }								\
  while (0)

/* FIXME: temporary hack until we can upgrade binutils */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
