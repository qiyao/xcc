#!/bin/csh -f
#
# Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
# Revised to support Tensilica processors and to improve overall performance
#
### ======================================================================
### ======================================================================
###
### Module: gen_st_list.csh
### $Revision: 1.2 $
### $Date: 1997/08/22 02:26:24 $
### $Author: ho $
### $Source: /isms/cmplrs.src/osprey1.0/be/cg/RCS/gen_st_list.csh,v $
### Revision history:
###   27-Feb-92 - Original version
###
### Usage:      gen_st_list MTP_BIN
###
###     Generate the st_list.[ch] module.  The argument is the MTP_BIN
###     directory.  We do this in a file so the make rule can depend on
###     and it can be rebuilt when the procedure changes
###
### ======================================================================
### ======================================================================



csh -f $1/gen_x_list.csh    'ST*'                                      \
                            'ST'                                       \
                            'defs.h'                                   \
			    'errors.h'				       \
                            'mempool.h'                                 \
                            'cgir.h'                                   \
                            'st_list.h'
