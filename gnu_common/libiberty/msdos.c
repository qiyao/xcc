
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

char msg[] = "No vfork available - aborting\n";
vfork()
{
  write(1, msg, sizeof(msg));
}

sigsetmask()
{
  /* no signals support in go32 (yet) */
}

waitpid()
{
  return -1;
}
