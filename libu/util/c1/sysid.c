
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

/*

  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libu/util/c1/sysid.c	92.1	07/07/99 13:22:26"

#include <sys/utsname.h>
#include <sys/param.h>
#ifndef NBPW
#define NBPW 8
#endif
 
long
SYSID(sys, node, rel, ver, mach)
long *sys, *node, *rel, *ver, *mach;
{
	struct utsname utn;
	int r;
 
	*sys = *node = *rel = *ver = *mach = (long) 0;
 
	if ((r = uname(&utn)) >= 0) {
		strncpy((char *)sys,  utn.sysname,  NBPW);
		strncpy((char *)node, utn.nodename, NBPW);
		strncpy((char *)rel,  utn.release,  NBPW);
		strncpy((char *)ver,  utn.version,  NBPW);
		strncpy((char *)mach, utn.machine,  NBPW);
	}
	
	return((long)r);
}
