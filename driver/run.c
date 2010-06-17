
/*
   Copyright (C) 2004-2006 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#include <stdio.h>
#include <stdlib.h>
#ifndef _WIN32
 #include <sys/wait.h>
 #include <sys/times.h>
#else
 #include <windows.h>
 #include <process.h> /* spawn */
 #include <time.h>    /* time, difftime */
#endif /* _WIN32 */
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h>
#include <malloc.h>
#include <limits.h>
#include <cmplrs/rcodes.h>
#include <alloca.h>
#include "run.h"
#include "string_utils.h"
#include "errors.h"
#include "file_names.h"
#include "phases.h"
#include "opt_actions.h"
#include "file_utils.h"


boolean show_flag = FALSE;
boolean execute_flag = TRUE;
boolean time_flag = FALSE;
boolean prelink_flag = TRUE;
boolean quiet_flag = TRUE;
boolean run_m4 = FALSE;
static boolean ran_twice = FALSE;

static void init_time (void);
static void print_time (string phase);

/* solaris doesn't define this automatically */
#ifndef WCOREFLAG
#define WCOREFLAG 0200
#endif

#ifdef linux
#define LOGFILE "/var/log/messages"
#else
#define LOGFILE "/usr/adm/SYSLOG"
#endif

extern void
run_phase (phases_t phase, string name, string_list_t *args)
{
  char **argv;
  int argc;
  string_item_t *p;
  string output = NULL;
  string input = NULL;
  boolean save_stderr = FALSE;
  int fdin, fdout;
  int spawnRet;
  int forkpid, termsig, waitpid;
  int waitstatus;
  int	num_maps;
  string rld_path;
  struct stat stat_buf;

  if (show_flag) {
    /* echo the command */
    fprintf(stderr, "%s ", name);
    print_string_list(stderr, args);
  }
  if (!execute_flag) return;

  if (time_flag) init_time();

  /* copy arg_list to argv format that exec wants */
  argc = 1;
  for (p = args->head; p != NULL; p=p->next) {
    argc++;
  }
  argv = (char **) alloca((argc+1)*sizeof(char*));
  argv[0] = name;
  for (argc = 1, p = args->head; p != NULL; argc++, p=p->next) {
    /* don't put redirection in arg list */
    if (same_string(p->name, "<")) {
      /* has input file */
      input = p->next->name;
      break;
    } else if (same_string(p->name, ">")) {
      /* has output file */
      output = p->next->name;
      break;
    } else if (same_string(p->name, ">&")) {
      /* has error output file */
      output = p->next->name;
      save_stderr = TRUE;
      break;
    }
    argv[argc] = alloca(strlen(p->name) + 1);
    strcpy(argv[argc], p->name);
  }
  argv[argc] = NULL;

#ifndef _WIN32

  /* fork a process */
  forkpid = fork();
  if (forkpid == -1) {
    error("no more processes");
    cleanup ();
    exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
  }

  if (forkpid == 0) {
    /* child */

    if (input != NULL) {
      if ((fdin = open (input, O_RDONLY)) == -1) {
        error ("cannot open input file %s", input);
        cleanup ();
        exit (RC_SYSTEM_ERROR);
        /* NOTREACHED */
      }
      dup2 (fdin, fileno(stdin));
    }
    if (output != NULL) {
      if ((fdout = creat (output, 0666)) == -1) {
        error ("cannot create output file %s", output);
        cleanup ();
        exit (RC_SYSTEM_ERROR);
	/* NOTREACHED */
      }
      if (save_stderr)
        dup2 (fdout, fileno(stderr));
      else
        dup2 (fdout, fileno(stdout));
    }

    rld_path = get_phase_ld_library_path (phase);

    if (rld_path != 0) {
      string new_env;
      string env_name = "LD_LIBRARY_PATH";
      int len = strlen (env_name) + strlen(rld_path) + 2;

      if (ld_library_path) {
        len += strlen (ld_library_path) + 1;
        new_env = alloca (len);
        sprintf (new_env, "%s=%s%c%s", env_name, rld_path,
		 PATH_SEPARATOR, ld_library_path);
      } else {
        new_env = alloca (len);
        sprintf (new_env, "%s=%s", env_name, rld_path);
      }
      putenv (new_env);
    }

    execv(name, argv);
    error("cannot exec %s", name);
    cleanup ();
    exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
  } else {
    /* parent */
    int procid;	/* id of the /proc file */

    while ((waitpid = wait (&waitstatus)) != forkpid) {
      if (waitpid == -1) {
        error("bad return from wait");
        cleanup();
        exit(RC_SYSTEM_ERROR);
	/* NOTREACHED */
      }
    }
    if (time_flag) print_time(name);

    if (WIFSTOPPED(waitstatus)) {
      termsig = WSTOPSIG(waitstatus);
      error("STOPPED signal received from %s", name);
      cleanup();
      exit(RC_SYSTEM_ERROR);
      /* NOTREACHED */
    } 
    else if (WIFEXITED(waitstatus)) {
      int status = WEXITSTATUS(waitstatus);
      extern int inline_t;
      boolean internal_err = FALSE;
      boolean user_err = FALSE;

      switch (status) {
      
      case RC_OKAY:
        if (inline_t == UNDEFINED && 
            is_matching_phase(get_phase_mask(phase), P_any_fe))
          inline_t = FALSE;
        break;
      
      case RC_NEED_INLINER:
        if (inline_t == UNDEFINED && 
            is_matching_phase(get_phase_mask(phase), P_any_fe))
          inline_t = TRUE;
	/* completed successfully */
        break;

      case RC_USER_ERROR:
      case RC_NORECOVER_USER_ERROR:
      case RC_SYSTEM_ERROR:
      case RC_GCC_ERROR:
        user_err = TRUE;
        break;

      case RC_OVERFLOW_ERROR:
        if (!ran_twice && phase == P_be) {
          /* try recompiling with larger limits */
          ran_twice = TRUE;
          add_string (args, "-TENV:long_eh_offsets");
          add_string (args, "-TENV:large_stack");
          run_phase (phase, name, args);
          return;
        }
        internal_err = TRUE;
        break;

      case RC_INTERNAL_ERROR:
        if (phase == P_ld || phase == P_ldplus)
          /* gcc/ld returns 1 for undefined */
          user_err = TRUE;
        else
          internal_err = TRUE;
        break;

      default:
        if (phase == P_c_gfe || phase == P_cplus_gfe)
          user_err = TRUE;
        else
          internal_err = TRUE;
        break;
      }
      
      if (internal_err) {
        error("%s returned non-zero status %d", name, status);
      }
      else if (user_err) {
	/* assume phase will print diagnostics */
        if (!show_flag || save_stderr)
          nomsg_error();
        else
          error("%s returned non-zero status %d", name, status);
      }
      ran_twice = FALSE;
      return;
    } 
    else if(WIFSIGNALED(waitstatus)){
      termsig = WTERMSIG(waitstatus);
      error("%s died due to signal %d", name, termsig);
      if (waitstatus & WCOREFLAG) {
        error("core dumped");
      }
      if (termsig == SIGKILL) {
        error("Probably caused by running out of swap space -- check %s", 
              LOGFILE);
      }
      cleanup();
      exit(RC_SYSTEM_ERROR);
    } 
    else {
      /* cannot happen, I think! */
      internal_error("driver exec'ing is confused");
      return;
    }
  }

#else

  if (input != NULL) {
    if ((fdin = open (input, O_RDONLY)) == -1) {
      error ("cannot open input file %s", input);
      cleanup ();
      exit (RC_SYSTEM_ERROR);
      /* NOTREACHED */
    }
    dup2 (fdin, fileno(stdin));
  }
  if (output != NULL) {
    if ((fdout = creat (output, 0666)) == -1) {
      error ("cannot create output file %s", output);
      cleanup ();
      exit (RC_SYSTEM_ERROR);
				/* NOTREACHED */
    }
    if (save_stderr) {
      dup2 (fdout, fileno(stderr));
    } else {
      dup2 (fdout, fileno(stdout));
    }
  }

  rld_path = get_phase_ld_library_path (phase);

  if (rld_path != 0) {
    string new_env;
    string env_name = "PATH";
    int len = strlen (env_name) + strlen(rld_path) + 2;
    if (ld_library_path) {
      len += strlen (ld_library_path) + 1;
      new_env = alloca (len);
      sprintf (new_env, "%s=%s%c%s", env_name, rld_path,
               PATH_SEPARATOR, ld_library_path);
    } else {
      new_env = alloca (len);
      sprintf (new_env, "%s=%s", env_name, rld_path);
    }
    putenv (new_env);
  }
 
{
  unsigned int total_bytes = 0;
  int i;
  char *argv_str, *new_a;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  DWORD exit_code;

  ZeroMemory( &si, sizeof(si) );
  si.cb = sizeof(si);
  ZeroMemory( &pi, sizeof(pi) );

  /* Calculate a bound on the length of the overall command line.
     Backslashes and quotes may need to be escaped, doubling the length
     of each argument in the worst case.  Each argument may need to be
     surrounded by quotes and will be followed by either a space or nil
     character, so we add 3 bytes overhead for each argument.  */
  for (i = 0; i < argc; i++)
    total_bytes += (2 * strlen (argv[i])) + 3;
  argv_str = malloc (total_bytes);


  /* The following argument quoting code was derived from Cygwin.
     Copyright 1996, 1997, 1998, 1999, 2000 Cygnus Solutions.

     This program is free software; you can redistribute it and/or modify it
     under the terms of the GNU General Public License (GPL) as published by
     the Free Software Foundation; either version 2 of the License, or (at
     your option) any later version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License
     along with this program; if not, write to the Free Software
     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
     USA.  */

  new_a = argv_str;
  for (i = 0; i < argc; i++) {
    int nback;
    char *p = NULL;
    const char *a = argv[i];
    int len = strlen (a);
    if (len != 0 && !strpbrk (a, " \t\n\r\"")) {
      memcpy (new_a, a, len);
      new_a += len;
    } else {
      *new_a++ = '"';
      /* Handle embedded special characters " and \.
	 A " is always preceded by a \.
	 A \ is not special unless it precedes a ".  If it does,
	 then all preceding \'s must be doubled to avoid having
	 the Windows command line parser interpret the \ as quoting
	 the ".  This rule applies to a string of \'s before the end
	 of the string, since cygwin/windows uses a " to delimit the
	 argument. */
      for (; (p = strpbrk (a, "\"\\")); a = ++p) {
	memcpy (new_a, a, p - a);
	new_a += (p - a);
	/* Find length of string of backslashes */
	nback = strspn (p, "\\");
	if (nback == 0) {
	  /* No backslashes, so it must be a ".
	     The " has to be protected with a backslash. */
	  *new_a++ = '\\';
	  *new_a++ = '"';
	} else {
	  /* Add the run of backslashes */
	  memcpy (new_a, p, nback);
	  new_a += nback;
	  /* Double up all of the preceding backslashes if they precede
	     a quote or EOS. */
	  if (!p[nback] || p[nback] == '"') {
	    memcpy (new_a, p, nback);
	    new_a += nback;
	  }
	  /* Point to last backslash */
	  p += nback - 1;
	}
      }
      if (*a) {
	len = strlen (a);
	memcpy (new_a, a, len);
	new_a += len;
      }
      *new_a++ = '"';
    }
    if (i == argc - 1)
      *new_a++ = '\0';
    else
      *new_a++ = ' ';
  }

  spawnRet = CreateProcess(NULL, argv_str, NULL, NULL, 
			   TRUE, 0, NULL, NULL, &si, &pi);
  if (time_flag) print_time(name);
  if (spawnRet == 0) {
    // could check errno here, but haven't done it yet.
    switch(GetLastError())
      {
      case E2BIG:
        error("Spawn:Too many arguments\n");
        break;
      case EINVAL:
        error("Spawn:Invalid mode\n");
        break;
      case ENOENT:
        error("Spawn: File/Path not found\n");
        break;
      case ENOEXEC:
        error("Spawn: Bad executable file\n");
        break;
      case ENOMEM:
        error("Spawn: Not enough memory\n");
        break;
      default:
        error("Could not create process %s. Windows error code %d\n", argv_str, GetLastError());
      }
    //error("no more processes");
    cleanup ();
    exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
  }
  WaitForSingleObject(pi.hProcess, INFINITE);
  GetExitCodeProcess(pi.hProcess, &exit_code);
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);

  if (exit_code == 0 || exit_code < -1) {
    // normal termination. exit_code should never be < -1
    int status = exit_code;
    extern int inline_t;
    boolean internal_err = FALSE;
    boolean user_err = FALSE;

    switch (status) {

    case RC_OKAY:
      if (inline_t == UNDEFINED && 
          is_matching_phase(get_phase_mask(phase), P_any_fe))
        inline_t = FALSE;
      break;

    case RC_NEED_INLINER:
      if (inline_t == UNDEFINED && 
          is_matching_phase(get_phase_mask(phase), P_any_fe))
        inline_t = TRUE;
	/* completed successfully */
      break;

    case RC_USER_ERROR:
    case RC_NORECOVER_USER_ERROR:
    case RC_SYSTEM_ERROR:
    case RC_GCC_ERROR:
      user_err = TRUE;
      break;

    case RC_OVERFLOW_ERROR:
      if (!ran_twice && phase == P_be) {
        /* try recompiling with larger limits */
        ran_twice = TRUE;
        add_string (args, "-TENV:long_eh_offsets");
        add_string (args, "-TENV:large_stack");
        run_phase (phase, name, args);
        return;
      }
      internal_err = TRUE;
      break;

    case RC_INTERNAL_ERROR:
      if (phase == P_ld || phase == P_ldplus)
        /* gcc/ld returns 1 for undefined */
        user_err = TRUE;
      else
        internal_err = TRUE;
      break;

    default:
      if (phase == P_c_gfe || phase == P_cplus_gfe)
        user_err = TRUE;
      else
        internal_err = TRUE;
      break;
    }

    if (internal_err) {
      error("%s returned non-zero status %d", name, status);
    }
    else if (user_err) {
      /* assume phase will print diagnostics */
      if (!show_flag || save_stderr)
        nomsg_error();
      else
        error("%s returned non-zero status %d", name, status);
    }
    ran_twice = FALSE;
    return;
  }
  else {
    /* Abnormal exit. It's assumed that the spawned process
     * hasn't called exit with exit code > 0 
     */
    error("error from spawn");
    cleanup ();
    exit (RC_SYSTEM_ERROR);
    /* NOTREACHED */
  }
}
#endif /* _WIN32 */

}


#ifndef _WIN32

/* this code is copied from csh, for printing times */
clock_t time0;
struct tms tm0;

static void
init_time (void)
{
    time0 = times (&tm0);
}


static void
print_time (string phase)
{
    clock_t time1, wtime;
    double utime, stime;
    struct tms tm1;

    time1 = times (&tm1);
    utime = (double)(tm1.tms_utime + tm1.tms_cutime -
		     tm0.tms_utime - tm0.tms_cutime) / (double)HZ;
    stime = (double)(tm1.tms_stime + tm1.tms_cstime -
		     tm0.tms_stime - tm0.tms_cstime) / (double)HZ;
    wtime = time1 - time0;

    fprintf (stderr, "%s phase time:  %.2fu %.2fs %u:%04.1f %.0f%%\n",
		phase, utime, stime, wtime / (60*HZ),
		(double)(wtime % (60*HZ)) / (double)HZ,
		(utime + stime) / ((double)wtime / (double)HZ) * 100.0);
}

#else /* _WIN32 */

#ifndef HZ
 #define HZ 100
#endif
time_t time0;

static void
init_time (void)
{
    time0 = time (NULL);
}

static void
print_time (string phase)
{
    time_t time1, wtime;
    //double utime, stime;
    //struct tms tm1;

    time1 = time(NULL);
    wtime = difftime(time1,time0);

    fprintf (stderr, "%s phase time: %u:%04.1f\n",
		phase, wtime / (60*HZ),
		(double)(wtime % (60*HZ)) / (double)HZ);
}

#endif /* _WIN32 */

/*
 * Handler () is used for catching signals.
 */
extern void
handler (int sig)
{
#ifdef _WIN32
	error("signal %d caught, stop processing", sig);
#else
	error("signal %s caught, stop processing", _sys_siglist[sig]);
#endif
	cleanup ();
	exit (RC_SYSTEM_ERROR);
}

/* set signal handler*/
extern void
catch_signals (void)
{
	/* modelled after Handle_Signals in common/util/errors.c */
#ifndef _WIN32
    if (signal (SIGHUP, SIG_IGN) != SIG_IGN) //
        signal (SIGHUP,  handler);
    if (signal (SIGBUS, SIG_IGN) != SIG_IGN)//
        signal (SIGBUS,  handler);
    if (signal (SIGQUIT, SIG_IGN) != SIG_IGN)//
        signal (SIGQUIT,  handler);
    if (signal (SIGTRAP, SIG_IGN) != SIG_IGN)//
        signal (SIGTRAP,  handler);
    if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
        signal (SIGPIPE,  handler);
#endif
    if (signal (SIGINT, SIG_IGN) != SIG_IGN)
        signal (SIGINT,  handler);
    if (signal (SIGILL, SIG_IGN) != SIG_IGN)
        signal (SIGILL,  handler);
#ifndef _WIN32
    if (signal (SIGIOT, SIG_IGN) != SIG_IGN)
        signal (SIGIOT,  handler);
#endif
    if (signal (SIGFPE, SIG_IGN) != SIG_IGN)
        signal (SIGFPE,  handler);
    if (signal (SIGSEGV, SIG_IGN) != SIG_IGN)
        signal (SIGSEGV,  handler);
    if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
        signal (SIGTERM,  handler);
}


