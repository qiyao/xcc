
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

int
waitpid (pid, stat_loc, options)
	int pid, *stat_loc, options;
{
  for (;;)
    {
      int wpid = wait(stat_loc);
      if (wpid == pid || wpid == -1)
	return wpid;
    }
}
