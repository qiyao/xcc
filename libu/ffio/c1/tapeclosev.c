
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


static char USMID[] = "@(#) libu/ffio/c1/tapeclosev.c	92.0	10/08/98 14:57:41";


#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include <fcntl.h>
#include <tapereq.h>
#include <sys/stat.h>
#include <sys/bmxctl.h>
#include <sys/dpacket.h>
#include <sys/types.h>
#include <sys/iosw.h>


/* 
 *	close volume - go to next tape
 *	works for either a bmx or er90 device
 */
int
_tape_closev(int fd)
{

	struct	dmn_comm	tapepos;

	tapepos.POS_REQ = TR_CLV;


	if(ioctl(fd, BXC_DMN_REQ, &tapepos) <0) {
		return(-1);
	}

	if( ioctl(fd, BXC_DMN_REP, &tapepos) <0) {
		return(-1);
	}
	if (tapepos.EOV_REP != 0) {
		errno = tapepos.EOV_REP;
		return(-1);
	}
	return(0);
}
