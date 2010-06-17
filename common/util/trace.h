
/*

  Copyright (C) 2001,2004,2005 Tensilica, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/


//                                                                     -*-c++-*-
//
// Debug Macros
// ============
//
// This file contains macros for generating and formatting trace
// output.
//
// The macros are activated by defining the TRACING macro before
// including this file, or with -DTRACING on the CC command line.
// When TRACING is not defined these macros expand to nothing at all.
// Otherwise, they arrange for output to be generated base on a simple
// runtime check.
//
// When tracing is enabled the environment variable TRACE must be set
// to a non-zero value for output to be generated.  The actual value
// controls the amount of output generated.  The value must be a two
// digit number.  The first digit is used by the ENTER macro to
// determine which functions (or blocks) will have tracing enabled.
// If the value of the first digit is less than the value given in the
// ENTER macro, no trace output will be generated for any TRACE macros
// in the  scope containing the ENTER macro.  If the  value of the
// second digit is less than the value given in the TRACE macro, no
// output will be given.
//
// This header file is self contained.  No additional support is
// required.
//
//
// Example
// =======
//
//  #define TRACING
//  #include "trace.h"
//
//  void main()
//  {
//    ENTER(1, "main");
//    int i = 0;
//    int sum = 0;
//    TRACE(0, "entering loop");
//    for (i = 0; i < 10; i++)
//      {
//        sum = sum + i;
//        TRACE(2, TRACEX(i) << TRACEX(sum));
//      }
//    LEAVE;
//  }
//
//
// The following output is generated:
// % setenv TRACE 11
// % a.out
//     trace.cxx:   5:(main
//     trace.cxx:  13:)main
// % setenv TRACE 12
// % a.out
//     trace.cxx:   5:(main
//     trace.cxx:   8: entering loop
//     trace.cxx:  12: i=0 sum=0
//     trace.cxx:  12: i=1 sum=1
//     trace.cxx:  12: i=2 sum=3
//     trace.cxx:  12: i=3 sum=6
//     trace.cxx:  12: i=4 sum=10
//     trace.cxx:  12: i=5 sum=15
//     trace.cxx:  12: i=6 sum=21
//     trace.cxx:  12: i=7 sum=28
//     trace.cxx:  12: i=8 sum=36
//     trace.cxx:  12: i=9 sum=45
//     trace.cxx:  14:)main
// %
//
//
// Trace Macros
// ============
//
// #define ENTER(level, func_name)
//
// The ENTER macro\'s second argument is intended to be the name of
// the function being entered.  If an ENTER macro appears in a
// function a LEAVE, RETURN, or RETURNV macro should be used when
// leaving the function.  Doing so is not required, however.  If one
// is not used, the return line in the trace output will not be
// labeled correctly.  No output will be generated for any TRACE
// macros in the function unless the value of the TRACE environment
// variable is less than level.
//
//
// #define M_ENTER(level, module, func_name)
//
// The M_ENTER macro is just like the ENTER macro except that it
// uses 'module' to determine the TRACE level for the function or block
// being entered.  The TRACE macros used within the scope of the M_ENTER
// will be looking for an environment variable named TRACE_xxx (where
// xxx is the value of 'module') This version of the ENTER macro is
// intended for those cases where it is necessary to have a single
// compliation unit contain functionality from more than one
// functional unit.
//
//
// #define TRACE(level, msg)
//
// TRACE is used to print a message when control passes over a point
// in the source.  Its argument is "shifted" onto cerr with a newline
// shifted on after that.  Output only will be generated for the TRACE
// macro if the value of the TRACE environment variable is greater
// than or equal to 'level'.  If the TRACE macro is contained in a scope
// that also contains an ENTER macro, the output is also controled by
// the value given there.
//
// The TRACE macro can be used in functions that do not use the ENTER
// macro.  The output from these traces will nested to the same level
// as the output from the calling function.
//
//
// #define M_TRACE(level, module, msg)
//
// M_TRACE is just like the TRACE macro, but like the M_ENTER
// macro it allows a trace module name to be specified as well.
//
//
// #define TRACE_IS_ON(level)
//
// A predicate that returns true iff TRACE(level,msg) would output msg
// at the same point in the code.  This is useful for controlling actions
// not easily expressible in terms of stream output.  For example:
//
//      if (TRACE_IS_ON(5))
//        display_message_in_popup_window("Start processing...");
//
//
// #define TRACEX(msg)
//
// TRACEX is used to format an expression for tracing.  It's expands
// to its argument turned into a string followed by an equals sign
// followed by the argument.
//
//
// #define LEAVE
//
// The LEAVE macro generates no output.  It is used to record the line
// number and filename where it appears.  If another LEAVE  macro is
// not encounted before the block containing it goes out of scope,
// this line number and filename will be used when reporting the point
// of return from the block.
//
//
// #define RETURN
//
// The RETURN macro is nearly a direct replacement for the 'return'
// keyword.  It does not take any arguments and can only be used when
// the function returns a value to its caller.  It causes the line
// number of the return from the function to be reported correctly.
//
//
// #define RETURNV
//
// The RETURNV macro is used like the RETURN macro, but must be used when
// the function does not return a value to its caller.
//
//
// Fine Tuning
// ===========
//
// The trace module uses values from user definable macros to help
// control the output from the trace macros.  These macros must be
// defined prior to the inclusion of trace.h.
//
//
// TRACING
//
// The TRACING macro is the main on/off switch for the tracing
// package.  It controls whether the trace macros generate code or
// simply expand to nothing.  If tracing is desired, it must be set
// prior to the inclusion of the trace.h.
//
//
// TRACE_STREAM
//
// The TRACE_STREAM macro defaults to cerr but can be set to send all
// trace output elsewhere.  It must be an instance of the ostream
// class.
//
//
// TRACE_MODULE
//
// The TRACE_MODULE macro may be defined by the application on the
// compile command line (or prior to including trace.h).  It can be
// used to specify the enviroment varible that controls tracing for
// the module.  It is intended to allow multiple modules within a
// single application to have their trace output controlled
// separately.  It's value must be a quoted string.
//
//
// TRACE_LEVEL
//
// The process of converting from a trace module name to a trace
// level can also be controlled by the application.  The TRACE_LEVEL
// macro is used for this purpose.  Applications that wish to use
// their own conversion function, rather than the supplied environment
// variable based look up mechanism, may define the TRACE_LEVEL macro.
// It should take a single 'const char *' argument and return an
// integer. The string passed to it is either empty or the string
// value  assigned to the TRACE_MODULE macro. The value it returns
// must be an integer in the range [0..99].
//
//
// Fine Tuning via Environment Variables
// =====================================
//
// The style of output may be changed by using the folloing
// environment variables.  Boolean variables are considered true if
// set to "true" and false otherwise.  Variables that are unset assume
// their default values.
//
//
// TRACE_SOURCE_POSITION
//
// Write out the filename and line number at the start of each trace
// line.  Default is true
//
//
// TRACE_TIME
//
// Write out the value of the high precision clock in
// seconds.microseconds at the start of each trace line (before the
// source position, if that is also written.)  Default is false.
//
//
// TRACE_ABSOLUTE_TIME
//
// When TRACE_TIME is true, controls the representation of the time
// written.  if TRACE_ABSOLUTE_TIME is true, time is measured in
// seconds since the epoch.  Otherwise time is measured since a time
// during static initialization (before entry to main.)  Default is
// false, but absolute time is useful when looking at a program
// consisting of more than one cooperating executable.
//
//
// Class Based Control
// ===================
//
// When it is inconvenient or undesirable to separate your classes
// into their own files, it might be desirable to add class based
// tracing control to your application.  Class based control is
// handled through two macros: TRACE_DECL and TRACE_DEFN.  Class
// methods can still be traced even if these macros are not used.
// They are provided only as a way to increase the ease with which you
// can control the trace output for a given class.  If your class is
// implemented entirely in its own source file these macros are not
// needed.  The TRACE_MODULE macro is a better choice.
//
//
// #define TRACE_DECL
//
// TRACE_DECL takes no arguments and must be placed
// inside the class declaration.  Using this macro, of course,
// requires that trace.h be included.  Since classes are usually
// declared in public header files, this might not be desirable.
// Instead, you can include:
//
//    class TRACE_module;
//
// outside of your class and
//
//    static TRACE_module _TRACE_module;
//
// inside your class.  When tracing is not enabled at compile time
// the static _TRACE_module member will never be referenced and
// therefore will consume no resources.
//
//
// #define TRACE_DEFN
//
// The TRACE_DEFN macro is used to define the _TRACE_module member.
// It must be placed in a source file along with the other definations
// for the class. This macro takes a single argument which specifies
// the name of the class.  TRACE_M_DEFN can be used in place of
// TRACE_DEFN and allow for the specification of trace module to which
// the class belong. TRACE_DEFN assumes that the class belongs to a
// trace module of the same name as the class itself.
//
//
// BUGS
// ====
//
// When tracing is enabled for a module, it needs access to a global
// variable that is used for tracking the nesting depth of the trace
// output.  To keep things simple, this variable is defined in
// trace.h.  This means that each compilation unit that includes
// trace.h will define the variable.  This results loader warnings at
// link time.
//


#ifndef _TRACE_H_
#define _TRACE_H_

#ifndef Is_True_On
//
// tracing is disabled
//
#define ENTER(l, x)
#define M_ENTER(l, m, x)
#define TRACE(l, x)
#define M_TRACE(l, m, x)
#define LEAVE
#define RETURN return
#define RETURNV return
#define TRACEX(x)
#define TRACE_DECL
#define TRACE_DEFN(cl)
#define TRACE_M_DEFN(cl, name)

#define TRACE_IS_ON(l) false

#else
//
// tracing is enabled
//
#include <iostream>
#include <iomanip>

#define ENTER(l, x)                                                     \
    TRACE_block _TRACE_block(l, __FILE__, __LINE__, x, _TRACE_module.level());\
    _TRACE_block.doNothing()


#define M_ENTER(l, m, x)			\
  static TRACE_module _TRACE_module(m);		\
  ENTER(l, x)


#define TRACE_IS_ON(l)                                                  \
  (_TRACE_module.level() >= _TRACE_block.level()                        \
   && _TRACE_module.level() % 10 >= l)


#define TRACE(l, x)                                                     \
{                                                                       \
  if (TRACE_IS_ON(l))                                                   \
    {                                                                   \
      TRACE_OUT(__FILE__, __LINE__, TRACE_depth, " ");                  \
      TRACE_STREAM << x << std::endl;                                        \
    }                                                                   \
}


#define M_TRACE(l, m, x)                                                \
{                                                                       \
  M_ENTER(_TRACE_block.level()/10, m, 0);                               \
  TRACE(l%10, x);                                                       \
}


#define TRACEX(x) #x"=" << x << " "
#define LEAVE _TRACE_block.line(__LINE__)
#define RETURN return LEAVE,
#define RETURNV do {LEAVE; return;} while(0)
#define TRACE_DECL  static TRACE_module _TRACE_module
#define TRACE_DEFN(cl)  TRACE_module cl::_TRACE_module(#cl)

#define TRACE_M_DEFN(cl, name)  TRACE_module cl::_TRACE_module(name)

#ifndef TRACE_MODULE
#define TRACE_MODULE ""
#endif

#ifndef TRACE_STREAM
#define TRACE_STREAM std::cerr
#endif

#ifndef TRACE_LEVEL
#define TRACE_LEVEL TRACE_level
#endif

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

// Some simple utilities for printing out high precision clock
struct TRACE_TimeOfDay : public timeval
{
  TRACE_TimeOfDay()
  {
#ifndef _WIN32
    gettimeofday(this, NULL);
#endif
  }

  TRACE_TimeOfDay operator-(TRACE_TimeOfDay& tod) const
  {
    if (tv_usec >= tod.tv_usec)
      return TRACE_TimeOfDay(tv_sec - tod.tv_sec,tv_usec - tod.tv_usec);

    return TRACE_TimeOfDay((tv_sec - 1) - tod.tv_sec,
                           (tv_usec + 1000000) - tod.tv_usec);
  }

 private:
  TRACE_TimeOfDay( time_t sec, long usec )
  {
    this->tv_sec = sec;
    this->tv_usec = usec;
  }
};

inline static std::ostream& operator<<(std::ostream& s, const timeval& tv)
{
  char buff[7];
  sprintf(buff,"%06ld",tv.tv_usec);

  s << tv.tv_sec << "." << buff;
  return s;
}


// Use a template class here so we don't need a .cxx file -- the prelinker
// cleans up in this case.
template <class T>
class TRACE
{
 public:
  static int
  level(const char *module)
  {
    char buffer[150] = "TRACE";
    if (module && module[0] != '\0')
      {
        strcat(buffer, "_");
        strncat(buffer, module, 100);
      }
    char *l = getenv(buffer);
    return l ? atoi(l) : 0;
  }

  static void
  out(const char *file, int line, int depth, const char *prefix)
  {
    std::cout.flush();
    const char *f = strrchr(file, '/');
    if (f) file = f+1;
    if ( _timing.value() )
      {
        if ( _absolute_times.value() )
          TRACE_STREAM << TRACE_TimeOfDay() << " ";
        else
          TRACE_STREAM << TRACE_TimeOfDay() - _initTime << " ";
      }

    if ( _sourcePosition.value() )
      {
        TRACE_STREAM << std::setw(30) << file << ':'
                     << std::setw(4) << line << ':'
                     << std::setw(depth) << prefix;
      }
  }

  static int depth;

 private:

  struct _flag
  {
   public:
    _flag( const char* envstr, bool default_value )
    {
      char* b = getenv(envstr);
      if ( ! b )
        _value = default_value;
      else
        _value = strcmp(b, "true") == 0 || strcmp(b, "1") == 0;
    }

    bool value()
    {
      return _value;
    }

   private:
    bool _value;
  };

  static _flag _timing;
  static _flag _absolute_times;
  static _flag _sourcePosition;
  static TRACE_TimeOfDay _initTime;
};


template <class T>
int TRACE<T>::depth = 0;

template <class T>
struct TRACE<T>::_flag TRACE<T>::_timing("TRACE_TIME", false);

template <class T>
struct TRACE<T>::_flag TRACE<T>::_absolute_times("TRACE_ABSOLUTE_TIME",false);

template <class T>
struct TRACE<T>::_flag TRACE<T>::_sourcePosition("TRACE_SOURCE_POSITION", true);

template <class T>
TRACE_TimeOfDay TRACE<T>::_initTime;

#define TRACE_depth TRACE<int>::depth
#define TRACE_level TRACE<int>::level
#define TRACE_OUT TRACE<int>::out

class TRACE_module
{
 public:
  TRACE_module(const char *module)
    : _level(TRACE_LEVEL(module))
  {
    // empty
  }

  int level()
  {
    return _level;
  }

 private:
  int _level;
};


class TRACE_block
{
 public:
  TRACE_block()
  {
    // empty
  }

  TRACE_block(int level, const char *file, int line,
	      const char *func_name, int module_level)
    : _func_name(func_name), _file(0), _line(line), _level(level*10)
  {
    if (! _func_name || module_level < _level)
      return;

    _file = file;

    TRACE_OUT(_file, _line, ++TRACE_depth, "(");
    TRACE_STREAM << _func_name << std::endl;
  }

  ~TRACE_block()
  {
    if (! _file)
      return;

    TRACE_OUT(_file, _line, TRACE_depth--, ")");
    TRACE_STREAM << _func_name << std::endl;
  }

  void doNothing() // Used to prevent warnings
  {
  }

  void line(int line)
  {
    _line = line;
  }

  inline int level()
  {
    return _level;
  }

 private:
  const char *_func_name;
  const char *_file;
  int _line;
  int _level;
};


static TRACE_module _TRACE_module(TRACE_MODULE);
static TRACE_block _TRACE_block;

#endif
#endif

// Local Variables:
// mode:c++
// indent-tabs-mode:()
// End:
