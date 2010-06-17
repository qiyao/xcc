C  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
C  Revised to support Tensilica processors and to improve overall performance
C
C
C  Copyright (C) 2000 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2.1 of the GNU Lesser General Public License 
C  as published by the Free Software Foundation.
C
C  This program is distributed in the hope that it would be useful, but
C  WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C
C  Further, this software is distributed without any warranty that it is
C  free of the rightful claim of any third person regarding infringement 
C  or the like.  Any license provided herein, whether implied or 
C  otherwise, applies only to this software file.  Patent licenses, if
C  any, provided herein do not apply to combinations of this program with 
C  other software, or any other product whatsoever.  
C
C  You should have received a copy of the GNU Lesser General Public 
C  License along with this program; if not, write the Free Software 
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
C  USA.
C
C  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
C  Mountain View, CA 94043, or:
C
C  http://www.sgi.com
C
C  For further information regarding this notice, see:
C
C  http://oss.sgi.com/projects/GenInfo/NoticeExplan
C
C


      SUBROUTINE VXSCTC(FPN,ISB,DEST,NUM,INC)
C
C     The VXSCTC subroutine converts VAX 32-bit floating-point
C     numbers into Cray 64-bit single-precision numbers.
C
      INTEGER FPN(1)              ! Variable or array of any type
C                                 ! containing VAX 32-bit floating
C                                 ! point numbers to convert.
C
      INTEGER ISB                 ! Byte number to begin the conversion.
C                                 ! Type integer variable, expression or
C                                 ! constant.  Bytes are numbered from 1
C                                 ! beginning at the leftmost byte
C                                 ! position of FPN.
C
      REAL DEST(1)                ! Variable or array of type real to
C                                 ! receive the converted values.
C
      INTEGER NUM                 ! Number of VAX floating-point numbers
C                                 ! to convert.  Type integer variable,
C                                 ! expression, or constant.
C
      INTEGER INC                 ! Memory increment for storing the
C                                 ! conversion results in DEST.  Optional
C                                 ! parameter of type integer variable,
C                                 ! expression, or constant.  The default
C                                 ! value is 1.

CDIR$ VFUNCTION VF32I
      REAL      VF32I

      IF (NUMARG() .EQ. 4) THEN
        JNC = 1
      ELSE IF (NUMARG() .EQ. 5) THEN
        JNC = INC
      ELSE
        CALL ABORT('VAX conversion routine called with incorrect number 
     +of arguments')
      ENDIF

      IF (NUM .LT. 0) THEN
        CALL ABORT('Invalid input to VAX conversion routine')
      ENDIF

      IF (NUM .EQ. 0) RETURN

      CALL VXUNPACK(FPN,ISB,DEST,NUM,32,JNC)

      IOUT = 1

      DO 100 I = 1, NUM
        DEST(IOUT) = VF32I(DEST(IOUT))
        IOUT       = IOUT + JNC
  100 CONTINUE

      RETURN
CDIR$ ID "@(#) libu/vms/vxsctc.f	92.0	10/08/98 14:57:41"
      END
