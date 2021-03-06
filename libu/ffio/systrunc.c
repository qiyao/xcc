
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


#pragma ident "@(#) libu/ffio/systrunc.c	92.1	06/29/99 13:16:47"

#include <errno.h>
#include <ffio.h>
#ifndef	_CRAY
#include <stdio.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include "sysio.h"

/*
 * _sys_trunc() calls the system call trunc(2) and returns status
 */

int
_sys_trunc(fio, stat)
struct fdinfo *fio;
struct ffsw *stat;
{
	int ret;
	struct stat statbuf;

#ifdef  _CRAY
	LOOP_SYSCALL(ret, trunc(fio->realfd));
#else
	off_t offset;

#ifdef __mips
	if (((struct sys_f *)fio->lyr_info)->needpos) {
		if (lseek( fio->realfd, ((struct sys_f *)fio->lyr_info)->curpos,
				0)  < 0)
		ERETURN(stat, errno, 0);
		((struct sys_f *)fio->lyr_info)->needpos = 0;
		offset = ((struct sys_f *)fio->lyr_info)->curpos;
	}
	else
#endif
	{
		offset = lseek(fio->realfd, 0, SEEK_CUR);
	}
	LOOP_SYSCALL(ret, ftruncate(fio->realfd, offset));
#endif
	if (ret < 0) {
		if (fstat(fio->realfd, &statbuf) < 0)
			ERETURN(stat, errno, 0);
		if (S_ISREG(statbuf.st_mode))
			ERETURN(stat, errno, 0);
#ifdef __mips
		ret = 0;
#else
		ret = statbuf.st_size;
#endif
	}
#ifdef __mips
	((struct sys_f *)fio->lyr_info)->endpos = offset;
	((struct sys_f *)fio->lyr_info)->curpos = offset;
#endif
	SETSTAT(stat, FFEOD, 0);
	fio->ateod = YES;
	fio->ateof = NO;
	return (ret);
}
