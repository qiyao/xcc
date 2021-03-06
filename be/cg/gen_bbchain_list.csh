#!/bin/csh -f
#
# Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
# Revised to support Tensilica processors and to improve overall performance
#
#  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
#
#  This program is free software; you can redistribute it and/or modify it
#  under the terms of version 2 of the GNU General Public License as
#  published by the Free Software Foundation.
#
#  This program is distributed in the hope that it would be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
#  Further, this software is distributed without any warranty that it is
#  free of the rightful claim of any third person regarding infringement 
#  or the like.  Any license provided herein, whether implied or 
#  otherwise, applies only to this software file.  Patent licenses, if 
#  any, provided herein do not apply to combinations of this program with 
#  other software, or any other product whatsoever.  
#
#  You should have received a copy of the GNU General Public License along
#  with this program; if not, write the Free Software Foundation, Inc., 59
#  Temple Place - Suite 330, Boston MA 02111-1307, USA.
#
#  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
#  Mountain View, CA 94043, or:
#
#  http://www.sgi.com
#
#  For further information regarding this notice, see:
#
#  http://oss.sgi.com/projects/GenInfo/NoticeExplan
#
#

### ======================================================================
### ======================================================================
###
### Module: gen_bb_list.csh
### $Revision: 1.4 $
### $Date: 2000/04/19 20:59:52 $
### $Author: mtibuild $
### $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/gen_bb_list.csh,v $
### Revision history:
###   27-Feb-92 - Original version
###
### Usage:      gen_bb_list MTP_BIN
###
###     Generate the bb_list.[ch] module.  The argument is the MTP_BIN
###     directory.  We do this in a file so the make rule can depend on
###     and it can be rebuilt when the procedure changes
###
### ======================================================================
### ======================================================================



csh -f $1/gen_x_list.csh    'BBCHAIN*'                                      \
                            'BBCHAIN'                                       \
                            'defs.h'                                   \
			    'errors.h'				       \
                            'mempool.h'                                 \
                            'cflow.h'                                   \
                            'bbchain_list.h'
