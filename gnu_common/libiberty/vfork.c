
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/* Emulate vfork using just plain fork, for systems without a real vfork.
   This function is in the public domain. */

int
vfork ()
{
  return (fork ());
}
