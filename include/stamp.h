
/* 
   Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
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


/* $Header: /isms/cmplrs.src/osprey1.0/include/RCS/stamp.h,v 7.23 2000/04/06 02:51:01 mtibuild Exp $ */

#ifndef __STAMP_H__
#define __STAMP_H__

#ifdef __cplusplus
extern "C" {
#endif

#ifdef TARG_XTENSA

#ifdef XTENSA_VERSION
#define INCLUDE_STAMP XTENSA_VERSION
#else
#undef INCLUDE_STAMP
#endif /* XTENSA_VERSION */

#else

#if defined(__linux) || defined(sun)
#define	MS_STAMP 0
#define	LS_STAMP 9
#define INCLUDE_STAMP "0.01.0-12"
#else
#define	MS_STAMP 7
#define	LS_STAMP 40
#define INCLUDE_STAMP "7.40"
#endif

#endif /* XTENSA */

#ifdef __cplusplus
}
#endif

#endif  /* __STAMP_H__ */
