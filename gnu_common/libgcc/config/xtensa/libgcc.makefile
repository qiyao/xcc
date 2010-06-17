# Makefile for libgcc.
# 
# Copyright (c) 2003, 2004 by Tensilica Inc.
# 
# This file was derived from GCC and is free software; you can
# redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation; either
# version 2, or (at your option) any later version.

# libgcc is normally built at the same time as gcc itself, but Tensilica
# separates these two steps, first building a configuration-independent
# gcc and then later building config-specific versions of libgcc.
# This makefile contains the rules and variables from the full GCC makefile
# that are needed to build libgcc and various crt files.

# Directory where sources are, from where we are.
srcdir = @srcdir@
top_srcdir = $(srcdir)
VPATH = $(srcdir)

.PHONY: all-target install-target install-libgcc install-common installdirs

default: all-target

# Selection of languages to be made.
CONFIG_LANGUAGES =  c++
LANGUAGES = c $(CONFIG_LANGUAGES)

# Various ways of specifying flags for compilations:
# CFLAGS is for the user to override to, e.g., do a cross build with -O2.
# XCFLAGS is used for most compilations but not when using the GCC just built.
# TCFLAGS is used for compilations with the GCC just built.
XCFLAGS =
TCFLAGS =
CFLAGS = -g
LOOSE_CFLAGS = `echo $(CFLAGS)|sed -e 's/-pedantic//g' -e 's/-Wtraditional//g'`
# These exists to be overridden by the x-* and t-* files, respectively.
X_CFLAGS =
T_CFLAGS =

SHELL = /bin/sh
INSTALL = @INSTALL@
INSTALL_DATA = @INSTALL_DATA@
# For GNUmake: let us decide what gets passed to recursive makes.
MAKEOVERRIDES =

# The GCC to use for compiling libgcc.a, enquire, and libgcc1-test.
GCC_FOR_TARGET = xt-xcc

# This is used instead of ALL_CFLAGS when compiling with GCC_FOR_TARGET.
GCC_CFLAGS = $(INTERNAL_CFLAGS) $(X_CFLAGS) $(T_CFLAGS) $(LOOSE_CFLAGS) -isystem ./include $(TCFLAGS)

# Specify the abi to use when building the c++ runtime
GXX_ABI_FLAG=

AR_FOR_TARGET = xt-ar
AR_FLAGS_FOR_TARGET =
AR_CREATE_FOR_TARGET = $(AR_FOR_TARGET) $(AR_FLAGS_FOR_TARGET) rc
RANLIB_FOR_TARGET = xt-ranlib
RANLIB_TEST_FOR_TARGET = true

GTHREAD_FLAGS=@gthread_flags@

# Common prefix for installation directories.
prefix = @prefix@
# Directory in which to put host dependent programs and libraries
exec_prefix = @exec_prefix@
# Where to install target libraries
tooldir = $(exec_prefix)/xtensa-elf/lib/xcc

objext = .o

# Top build directory, relative to here.
top_builddir = .

# Options to use when compiling libgcc2.a.
#
LIBGCC2_DEBUG_CFLAGS = -g
LIBGCC2_CFLAGS = -O2 $(LIBGCC2_INCLUDES) $(GCC_CFLAGS) $(TARGET_LIBGCC2_CFLAGS) $(LIBGCC2_DEBUG_CFLAGS) $(GTHREAD_FLAGS) -DIN_LIBGCC2 -D__GCC_FLOAT_NOT_NEEDED @inhibit_libc@

# Additional options to use when compiling libgcc2.a.
# Some targets override this to -isystem include
LIBGCC2_INCLUDES =

# Additional target-dependent options for compiling libgcc2.a.
TARGET_LIBGCC2_CFLAGS =

# Additional sources to handle exceptions; overridden by some targets.
# FIXME: the following should probably be frame-dwarf2.c (?)
LIB2ADDEH = $(srcdir)/frame.c

# List of extra object files that should be compiled for this target machine.
EXTRA_PARTS = crtbegin.o crtend.o crtbeginS.o crtendS.o

# List of extra C and assembler files to add to static and shared libgcc2.
# Assembler files should have names ending in `.asm'.
LIB2FUNCS_EXTRA =

# Prefix to apply to names of object files when using them
# to run on the machine we are compiling on.
HOST_PREFIX=
# Prefix to apply to names of object files when compiling them
# to run on the machine we are compiling on.
HOST_PREFIX_1=loser-

# Extra flags to use when compiling crt{begin,end}.o.
CRTSTUFF_T_CFLAGS =

# "t" or nothing, for building multilibbed versions of, say, crtbegin.o.
T =

# End of variables for you to override.

####target overrides

# Build CRT files and libgcc with the "longcalls" option
CRTSTUFF_T_CFLAGS = -mlongcalls -INLINE:=off
CRTSTUFF_T_CFLAGS_S = -mlongcalls -INLINE:=off -fpic
TARGET_LIBGCC2_CFLAGS = -mlongcalls -fexceptions -DDONT_USE_BUILTIN_SETJMP \
	-INLINE:aggressive -Wa,--schedule

LIBGCC1 = libgcc1-asm.a
LIB1ASMSRC = xtensa/lib1funcs.asm
LIB1ASMFUNCS =  _mulsi3 _mulsi3hifi2 _divsi3 _modsi3 _udivsi3 _umodsi3 \
	_umulsidi3 _clz _clzsi2 _ctzsi2 _ffssi2 \
	_ashldi3 _ashrdi3 _lshrdi3 \
	_negsf2 _addsubsf3 _mulsf3 _divsf3 _cmpsf2 _fixsfsi _fixsfdi \
	_fixunssfsi _fixunssfdi _floatsisf _floatunsisf \
	_floatdisf _floatundisf \
	_negdf2 _addsubdf3 _muldf3 _divdf3 _cmpdf2 _fixdfsi _fixdfdi \
	_fixunsdfsi _fixunsdfdi _floatsidf _floatunsidf \
	_floatdidf _floatundidf \
	_truncdfsf2 _extendsfdf2

LIB2FUNCS_EXTRA = \
    $(srcdir)/config/xtensa/lib2funcs.S

$(T)crti.o: $(srcdir)/config/xtensa/crti.asm $(GCC_PASSES)
	$(GCC_FOR_TARGET) $(MULTILIB_CFLAGS) -c -o $(T)crti.o \
	  -x assembler-with-cpp $(srcdir)/config/xtensa/crti.asm
$(T)crtn.o: $(srcdir)/config/xtensa/crtn.asm $(GCC_PASSES)
	$(GCC_FOR_TARGET) $(MULTILIB_CFLAGS) -c -o $(T)crtn.o \
	  -x assembler-with-cpp $(srcdir)/config/xtensa/crtn.asm

EXTRA_PARTS = crti.o crtn.o crtbegin.o crtend.o crtbeginS.o crtendS.o

####cross overrides

CROSS=-DCROSS_COMPILE

#

# IN_GCC tells various files that system.h, toplev.c, etc are available.
INTERNAL_CFLAGS = $(CROSS) -DIN_GCC

# Specify the directories to be searched for header files.
# Both . and srcdir are used, in that order,
# so that *config.h will be found in the compilation
# subdirectory rather than in the source directory.
INCLUDES = -I. -I$(srcdir) -I$(srcdir)/config

#
# Support for additional languages (other than c and objc).

LANG_LIB2FUNCS = cplib2.txt

#
# Lists of files for various purposes.

# Library members defined in libgcc2.c.
# TENSILICA: Removed floating-point functions since we have our own.
LIB2FUNCS = _muldi3 _divdi3 _moddi3 _udivdi3 _umoddi3 _negdi2 \
    _ffsdi2 \
    _udiv_w_sdiv _udivmoddi4 _cmpdi2 _ucmpdi2 \
    __gcc_bcmp _varargs __dummy _eprintf \
    _bb _shtab _clear_cache _trampoline __main _exit \
    _ctors _pure

LIB2FUNCS_EH = _eh

CONFIG_H =
MACHMODE_H = machmode.h machmode.def

#
# Language makefile fragments.

####language fragments

# Extra headers to install.
# FIXME: These files are currently in $(srcdir)/cp and not all are present.
#CXX_EXTRA_HEADERS = $(srcdir)/cp/inc/typeinfo $(srcdir)/cp/inc/exception \
#	$(srcdir)/cp/inc/new $(srcdir)/cp/inc/new.h $(srcdir)/cp/inc/cxxabi.h

# FIXME: CXX_LIB2FUNCS is missing: vec.o cp-demangle.o dyn-string.o
# (and CXX_LIB2SRCS is missing the corresponding source files).
# It isn't clear if these were deliberately omitted or just forgotten.

# Extra code to include in libgcc2.
CXX_LIB2FUNCS = tinfo.o tinfo2.o new.o opnew.o opnewnt.o opvnew.o opvnewnt.o \
	opdel.o opdelnt.o opvdel.o opvdelnt.o exception.o xtensa_eh.o
CXX_LIB2SRCS = $(srcdir)/cp/new.cc $(srcdir)/cp/new1.cc $(srcdir)/cp/new2.cc \
	$(srcdir)/cp/exception.cc $(srcdir)/cp/tinfo.cc \
	$(srcdir)/cp/tinfo2.cc $(srcdir)/cp/tinfo.h $(srcdir)/cp/xtensa_eh.cc

# C++ language-support library pieces for libgcc.
tinfo.o: $(srcdir)/cp/tinfo.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/tinfo.cc
tinfo2.o: $(srcdir)/cp/tinfo2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/tinfo2.cc
exception.o: $(srcdir)/cp/exception.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c -fexceptions $(srcdir)/cp/exception.cc
new.o: $(srcdir)/cp/new.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new.cc
opnew.o: $(srcdir)/cp/new1.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new1.cc -DL_op_new -o opnew.o
opnewnt.o: $(srcdir)/cp/new1.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new1.cc -DL_op_newnt -o opnewnt.o
opvnew.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_vnew -o opvnew.o
opvnewnt.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_vnewnt -o opvnewnt.o
opdel.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_delete -o opdel.o
opdelnt.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_delnt -o opdelnt.o
opvdel.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_vdel -o opvdel.o
opvdelnt.o: $(srcdir)/cp/new2.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/new2.cc -DL_op_vdelnt -o opvdelnt.o
xtensa_eh.o: $(srcdir)/cp/xtensa_eh.cc
	$(GCC_FOR_TARGET) $(LIBGCC2_CFLAGS) $(GXX_ABI_FLAG) $(CXXFLAGS) $(INCLUDES) \
	  -c $(srcdir)/cp/xtensa_eh.cc -o xtensa_eh.o

# We want to update cplib2.txt if any of the source files change...
cplib2.txt: $(CXX_LIB2SRCS) $(CXX_EXTRA_HEADERS)
	case " $(LANGUAGES) " in \
	*" "[cC]"++ "*) \
	  echo $(CXX_LIB2FUNCS) > cplib2.new;; \
	*) \
	  echo "" > cplib2.new;; \
	esac
	mv -f cplib2.new cplib2.txt 

#
# Dummy target to satisfy dependencies
stmp-int-hdrs:
	touch $@

#
# Build libgcc.a.

all-target: libgcc.a $(EXTRA_PARTS)

LIB2ADD = $(LIB2ADDEH) $(LIB2FUNCS_EXTRA) $(LANG_LIB2FUNCS)

libgcc.mk: config.status Makefile mklibgcc $(LIB2ADD)
	objext='$(objext)' \
	OLDCC='$(OLDCC)' \
	LIBGCC1='$(LIBGCC1)' \
	LIB1FUNCS='$(LIB1FUNCS)' \
	LIB1ASMFUNCS='$(LIB1ASMFUNCS)' \
	LIB1FUNCS_EXTRA='$(LIB1FUNCS_EXTRA)' \
	LIB2FUNCS='$(LIB2FUNCS)' \
	LIB2FUNCS_EH='$(LIB2FUNCS_EH)' \
	LIB2ADD='$(LIB2ADD)' \
	FPBIT='$(FPBIT)' \
	FPBIT_FUNCS='$(FPBIT_FUNCS)' \
	DPBIT='$(DPBIT)' \
	DPBIT_FUNCS='$(DPBIT_FUNCS)' \
	MULTILIBS='.;' \
	EXTRA_MULTILIB_PARTS='$(EXTRA_MULTILIB_PARTS)' \
	  $(SHELL) mklibgcc > tmp-libgcc.mk
	mv tmp-libgcc.mk libgcc.mk

# All the things that might cause us to want to recompile bits of libgcc.
LIBGCC_DEPS = libgcc.mk $(srcdir)/libgcc2.c $(CONFIG_H) \
	$(MACHMODE_H) longlong.h frame.h gbl-ctors.h config.status \
	stmp-int-hdrs tsystem.h $(FPBIT) $(DPBIT) $(LIB2ADD)

libgcc.a: $(LIBGCC_DEPS)
	$(MAKE) GCC_FOR_TARGET="$(GCC_FOR_TARGET)" \
	  HOST_PREFIX="$(HOST_PREFIX)" HOST_PREFIX_1="$(HOST_PREFIX_1)" \
	  AR_FOR_TARGET="$(AR_FOR_TARGET)" \
	  AR_CREATE_FOR_TARGET="$(AR_CREATE_FOR_TARGET)" \
	  AR_FLAGS_FOR_TARGET="$(AR_FLAGS_FOR_TARGET)" \
	  CFLAGS="$(CFLAGS)" \
	  RANLIB_FOR_TARGET="$(RANLIB_FOR_TARGET)" \
	  RANLIB_TEST_FOR_TARGET="$(RANLIB_TEST_FOR_TARGET)" \
	  LIBGCC2_CFLAGS="$(LIBGCC2_CFLAGS)" \
	  INCLUDES="$(INCLUDES)" \
	  CONFIG_H="$(CONFIG_H)" MACHMODE_H="$(MACHMODE_H)" \
	  LIB1ASMSRC='$(LIB1ASMSRC)' \
	  MAKEOVERRIDES= \
	  -f libgcc.mk all

# Compile two additional files that are linked with every program
# linked using GCC on systems using COFF or ELF, for the sake of C++
# constructors.
$(T)crtbegin.o: crtstuff.c $(GCC_PASSES) $(CONFIG_H) \
  defaults.h frame.h gbl-ctors.h stmp-int-hdrs tsystem.h
	$(GCC_FOR_TARGET) $(GCC_CFLAGS) $(INCLUDES) $(MULTILIB_CFLAGS) -g0 \
	  -finhibit-size-directive -fno-inline-functions \
	  -fno-exceptions $(CRTSTUFF_T_CFLAGS) @inhibit_libc@ \
	  -c $(srcdir)/crtstuff.c -DCRT_BEGIN -o $(T)crtbegin$(objext)

$(T)crtend.o: crtstuff.c $(GCC_PASSES) $(CONFIG_H) \
  defaults.h frame.h gbl-ctors.h stmp-int-hdrs tsystem.h
	$(GCC_FOR_TARGET) $(GCC_CFLAGS) $(INCLUDES) $(MULTILIB_CFLAGS) -g0 \
	  -finhibit-size-directive -fno-inline-functions \
	  -fno-exceptions $(CRTSTUFF_T_CFLAGS) @inhibit_libc@ \
	  -c $(srcdir)/crtstuff.c -DCRT_END -o $(T)crtend$(objext)

# These are versions of crtbegin and crtend for shared libraries.
$(T)crtbeginS.o: crtstuff.c $(GCC_PASSES) $(CONFIG_H) \
  defaults.h frame.h gbl-ctors.h stmp-int-hdrs tsystem.h
	$(GCC_FOR_TARGET) $(GCC_CFLAGS) $(INCLUDES) $(MULTILIB_CFLAGS) -g0 \
	  -finhibit-size-directive -fno-inline-functions \
	  -fno-exceptions $(CRTSTUFF_T_CFLAGS_S) @inhibit_libc@ \
	  -c $(srcdir)/crtstuff.c -DCRT_BEGIN -DCRTSTUFFS_O \
	  -o $(T)crtbeginS$(objext)

$(T)crtendS.o: crtstuff.c $(GCC_PASSES) $(CONFIG_H) \
  defaults.h frame.h gbl-ctors.h stmp-int-hdrs tsystem.h
	$(GCC_FOR_TARGET) $(GCC_CFLAGS) $(INCLUDES) $(MULTILIB_CFLAGS) -g0 \
	  -finhibit-size-directive -fno-inline-functions \
	  -fno-exceptions $(CRTSTUFF_T_CFLAGS_S) @inhibit_libc@ \
	  -c $(srcdir)/crtstuff.c -DCRT_END -DCRTSTUFFS_O \
	  -o $(T)crtendS$(objext)

#
# Entry points `install' and `uninstall'.

install-target: install-libgcc install-common

# Create the installation directories.
installdirs:
	-if [ -d $(tooldir) ] ; then true ; else mkdir -p $(tooldir) ; chmod a+rx $(tooldir) ; fi

# Install the library.
install-libgcc: libgcc.a installdirs
	-if [ -f libgcc.a ] ; then \
	  rm -f $(tooldir)/libgcc.a; \
	  $(INSTALL_DATA) libgcc.a $(tooldir)/libgcc.a; \
	  if $(RANLIB_TEST_FOR_TARGET) ; then \
	    (cd $(tooldir); $(RANLIB_FOR_TARGET) libgcc.a); else true;fi; \
	  chmod a-x $(tooldir)/libgcc.a; \
	else true; fi

install-common: installdirs $(EXTRA_PARTS)
	for file in $(EXTRA_PARTS) ..; do \
	  if [ x"$$file" != x.. ]; then \
	    rm -f $(tooldir)/$$file; \
	    $(INSTALL_DATA) $$file $(tooldir)/$$file; \
	    chmod a-x $(tooldir)/$$file; \
	  else true; fi; \
	done

###########################################################################

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status

$(top_builddir)/config.status: $(top_srcdir)/configure
	cd $(top_builddir) && $(SHELL) ./config.status --recheck

$(top_srcdir)/configure: $(top_srcdir)/configure.in $(CONFIGURE_DEPENDENCIES)
	cd $(top_srcdir) && autoconf
