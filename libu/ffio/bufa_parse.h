
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


/* USMID @(#) libu/ffio/bufa_parse.h	92.0	10/08/98 14:57:41 */

/*  BUFA LAYER ASSIGN PARSING DEFINITION */

#define NUM_BUFA_OPTS     0
#define NUM_BUFA_ALIAS    0

struct LAYER_DATA _bufa_data =
    {
         CLASS_BUFA ,
         FLD_EXT_TYPE,
         "bufa",
         "" ,
         0,
         0,
         NUM_BUFA_OPTS,
         1 ,
         NUM_NBUF_NUMERICS,
         NUM_BUFA_ALIAS ,
         NULL ,
         _nbuf_numerics ,
         NULL,
         NULL
    } ;
