C
C  Copyright (C) 2001 Tensilica, Inc.  All Rights Reserved.
C  Revised to support Tensilica processors and to improve overall performance
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



       SUBROUTINE WGETB(IFLAG)
CDIR$ ID "@(#) libu/waio/wgetb.f	92.0	10/08/98 14:57:41"

CC     WGETB  -  GET BLOCK OR BLOCKS TO GET/PUT DATA TO.
C
C      ENTRY  -  IFLAG = 1. WGETB WAS CALLED FROM *SEEK* ENTRY, BLOCKS
C                WILL BE ASSIGNED FROM THE FRONT OF THE PAGED AREA TO
C                MAXIMIZE THE ASYNCRONOUS EFFECT.
C
C                IFLAG = 0. WGETB WILL GO THRU THE NORMAL LOGIC.
C
C      EXIT   -  WORD ADDRESSES OF REQUIRED NUMBER OF BLOCKS(OR MAX)
C                ARE IN THE *WABP* ARRAY AND IF READING THE INFORMATION
C                IS READ IN TO THE FILE BUFFERS.  IF WRITING THE PARTIAL
C                BLOCKS(FIRST AND LAST) ARE READ IN IF NECESSARY.
C
C      CALLS  -  *WFLUSH* TO FLUSH THE *WABP* ARRAY IF NECESSARY.
C
C                *READWA* TO READ BLOCKS FROM DISK.
C
C      METHOD:
C
C        WGETB FIRST SETS THE MAXIMUM NUMBER OF BLOCKS (OR ALL) TO DO.
C      IF THIS IS A READ CALL THE READ SEQUENTIAL COUNTS ARE CHECKED
C      AND BLOCK COUNT ADJUSTED UPWARD IF WE ARE IN SEQUENTIAL MODE.
C      NEXT THE UNUSED BIT IN THE *FET* TABLE IS CHECKED.  IF NON ZERO
C      IT MEANS THAT THERE STILL ARE SOME 'VIRGIN' BUFFERS LEFT AROUND,
C      AND THESE ARE SEARCHED FOR FIRST.  IF NOT ENOUGH FOUND OR NONE
C      TO BEGIN WITH WGETB THEN SEARCHS FOR BUFFERS IN READ MODE.  THE
C      GLOBAL WRITE FLAG IN THE *FET* TABLE IS CHECKED FIRST.  IF THIS
C      BIT IS ZERO THERE ARE NO BUFFERS IN WRITE MODE SO PAGES ARE
C      ASSIGNED IN SEQUENCE TO THE LAST PAGE NUMBER FIELD FROM THE FET
C      TABLE.  IF THERE ARE PAGES IN WRITE MODE THEN A SEARCH IS DONE
C      TO SEE IF THERE ARE ENOUGH PAGES IN READ MODE IN SEQUENCE TO
C      SATISFY THIS REQUEST.  IF THE REQUIRED NUMBER OF PAGES ARE NOT
C      FOUND THEY MUST BE IN WRITE MODE SO *WFLUSH* IS CALLED TO FLUSH
C      THE BUFFERS TO DISK.  IF WE ARE READING DATA FROM THE DISK, ONE
C      CALL IS MADE TO XFER THE NEEDED BLOCKS.  IF WE ARE WRITING TO
C      DISK WE HAVE TO WORRY ABOUT THE FIRST AND LAST BLOCKS INVOLVED
C      BEING PARTIAL ONES.  IF THE FIRST WORD ADDRESS DOES NOT START ON
C      A DISK BLOCK BOUNDARY AND/OR THE LAST WORD ADDRESS DOES NOT END
C      ON A DISK BLOCK BOUNDARY, THEN THE OLD BLOCK OR BLOCKS (IF THERE
C      IS ONE) MUST BE READ FROM THE DISK SO THE NEW DATA CAN BE MERGED
C      IN WITH THE OLD. FINALLY THE *WABP* ARRAY IS UPDATED TO REFLECT
C      THE NEW WORD ADDRESSES AND CONTROL RETURNED TO CALLER.
C
C      FLOW:
C
C      1. SET MAX BLOCK COUNT AND IF READING POSSIBLY ADJUST THE BLOCK
C         COUNT UP IF READING IS SEQUENTIAL.
C
C      2. IF NO UNUSED BUFFERS SKIP TO  6.
C
C      3. SEARCH *WABP* ARRAY BACK TO FRONT AND SAVE FIRST ZERO ENTRY.
C
C      4. IF FIRST ZERO BLOCK PLUS COUNT REQUIRED IS LESS THAN
C         OR EQUAL TO TOTAL BLOCKS THAN THERE ARE ENOUGH EMPTY
C         BLOCKS AVAILABLE.  SAVE THE START OF THESE BLOCKS AND
C         SKIP TO 23.
C
C      5. NOT ENOUGH OR NO EMPTY BLOCKS.  IF NONE SET UNUSED FLAG IN
C         THE *FET* TABLE TO ZERO SINCE THE BUFFERS WILL NEVER BE
C         EMPTY AGAIN.
C
C      6. NOW SEARCH FOR BUFFERS IN READ MODE.
C
C      7. SET INCREMENT INTO BUFFERS TO LAST ENTRY SEARCH NUMBER.
C         THIS INSURES SORT OF A ROUND ROBBIN USE OF BUFFERS.
C
C      8. IF THE STARTING BLOCK NUMBER PLUS THE NUMBER OF BLOCKS TO
C         LOOK FOR IS GREATER THAN THE MAXIMUM, OR IF CALLER HAS
C         REQUESTED ASSIGNMENT FROM THE START OF THE BUFFER POOL,
C         (THIS IS DONE ON A SEEK CALL TO ALLOW USERS TO MAKE MAXIMUM
C         USE OF BUFFER POOL) RESET THE STARTING BLOCK NUMBER TO ONE.
C
C      9. CHECK THE GLOBAL WRITE FLAG IN THE *FET* TABLE AND IF THIS
C         IS ZERO THERE ARE NO BUFFERS IN WRITE MODE SO WE CAN SIMPLY
C         ASSIGN THE BUFFERS REQUIRED ANYWHERE.  IF ZERO SKIP TO 20.
C
C     10. IF THERE ARE MORE THEN 20 BUFFERS TO SEARCH START SEARCH
C         FROM THE ROUND ROBBIN COUNTER OTHERWISE START AT FRONT.
C
C     11. ADVANCE BUFFER SEARCH NUMBER.
C
C     12. IF THE *WABP* ENTRY POINTED TO BY THE SEARCH BLOCK NUMBER IS
C         IN WRITE MODE SKIP TO 16.
C
C     13. ADVANCE THE READ BUFFERS IN A ROW COUNTER (INAROW) BY ONE.
C
C     14. IF THIS IS THE FIRST READ BUFFER FOUND OR FIRST ONE AFTER
C         SOME WRITE BUFFERS, SAVE THE STARTING INDEX OF THIS BUFFER
C         IN *IST*.
C
C     15. SKIP TO 18.
C
C     16. WE HAVE FOUND A BUFFER IN WRITE MODE.  IF THE NUMBER OF READ
C         BUFFERS IN A ROW JUST FOUND IS GREATER THAN ANY NUMBER FOUND
C         SO FAR SAVE THIS AS THE MAX VALUE(INMAX) AND ALSO SAVE THE
C         STARTING INDEX OF THIS MAX STRING OF BUFFERS.
C
C     17. RESET *INAROW* TO ZERO AND SET FLAG FOR TO INDICATE WRITE
C         BUFFER WAS FOUND.
C
C     18. LOOP TO 11 UNTIL ALL BUFFERS SEARCHED.
C
C     19. ALL BLOCKS HAVE BEEN SEARCHED, IF THE LAST *INAROW* VALUE IS
C         THE MAX SET NEW *INMAX* AND SAVE THE STARTING INDEX OF THIS
C         NEW MAX.
C
C     20. IF THE MAXIMUM NUMBER OF BUFFERS FOUND IS LESS THAN REQUIRED
C         NUMBER SKIP TO 22 TO FLUSH THE BUFFERS.
C
C     21. SET THE BUFFER OFFSET (PRU) BASED ON THE BUFFER SIZE AND THE
C         BUFFER NUMBER.  SET THE STARTING BLOCK NUMBER AND SAVE THE
C         BLOCK NUMBER USED PLUS BLKCNT IN THE *FET* ARRAY. SKIP TO 23.
C
C     22. CALL *WFLUSH* TO FLUSH BUFFERS. SET *PRU* TO 1 AND BLOCK TO 1.
C
C     23. IF WRITING TO DISK SKIP TO 25.
C
C     24. READING FROM DISK.  SET WORD COUNT TO READ EQUAL TO BLOCK
C         NUMBER REQUIRED TIMES BLOCK SIZE AND READ INTO THE BUFFER
C         AREA POINTED TO BY THE RELATIVE POINTER (PRU). SKIP TO 31.
C
C     25. WRITING - WE MUST NOW CHECK IF THE FIRST AND LAST BLOCKS
C         ARE PARTIALS.
C
C     26. IF FIRST BLOCK IS NOT A PARTIAL OR WE ARE NOT DOING THE FIRST
C         BLOCK SKIP TO 30.
C
C     27. SET PARTIAL FIRST BLOCK FLAG TO FALSE, AND CHECK PARTIAL
C         LAST FLAG.  IF PARTIAL LAST FLAG NOT SET OR IF THE LAST
C         BLOCK OF THE TRANSFER WILL BE DONE LATER SKIP TO 29.
C
C     28. SET PARTIAL LAST FLAG FALSE. READ IN THE PARTIAL FIRST
C         BLOCK, THE INTERVEINING FULL BLOCKS(MAY BE 0), AND THE
C         PARTIAL LAST BLOCK WITH ONE READ CALL.  SKIP TO 31.
C
C     29. HERE IF NO PARTIAL LAST OR THE PARTIAL LAST BLOCK IS DOWN
C         STREAM AWAYS YET.  READ IN THE PARTIAL FIRST BLOCK AND
C         SKIP TO 31.
C
C     30. HERE IF NO PARTIAL FIRST BLOCK.  IF WE HAVE A PARTIAL LAST
C         BLOCK CONDITION AND THE ENDING BLOCK IS BEING DONE THIS
C         PASS READ IN THIS PARTIAL LAST BLOCK.
C
C     31. UPDATE THE *WABP* ARRAY TO REFLECT THE NEW WORD ADDRESSES
C         JUST ASSIGNED, MAKING SURE THERE ARE NO DUPLICATE ENTRYS.
C
C     32. RETURN. (WHEW)
C
C
CDIR$  EJECT
       IMPLICIT INTEGER (A-Z)

       INCLUDE "wavars.fh"
C      FIRST COMPUTE BLOCK COUNTS AND READ AHEAD SEQUENCES.

       MAXBLKS = EOIB - FIRST + 1
       BLKS    = BLKCNT
       LSTSEQ  = FET(12,INDEX).AND.MASK(128-32)
       IF(READ)
     +   THEN
           LSTSEQ        = (LSTSEQ + SHIFTR(FET(11,INDEX),32)) / 2
           FET(12,INDEX) = FET(12,INDEX).AND.MASK(32) + LSTSEQ
           BLKS          = MIN0(MAX0(BLKCNT,LSTSEQ),MAXBLKS)
         ENDIF
       BLOCKS = MIN0(BLKS,BUFFERS)

C      NOW SEARCH FOR AN UNUSED BUFFER.

       IF(SHIFTR(FET(12,INDEX),32).NE.0)
     +   THEN
           K       = BLOCKS
           IBLOCK  = BUFFERS + 1
           DO 10 I = BUFFERS,1,-1
             IF(WABP(I).EQ.0)IBLOCK = I
10         CONTINUE
           IF((IBLOCK + K - 1).LE.BUFFERS)
     +       THEN

C              WE HAVE FOUND ENOUGH UNUSED BUFFERS.

               I      = IBLOCK
               PRU    = ((I-1)*BSIZE)+1
               GO TO 40
             ENDIF
          FET(12,INDEX) = LSTSEQ
         ENDIF

C      NOW SEARCH FOR BUFFERS IN READ MODE.

       J      =  FET(17,INDEX)
       K      =  BLOCKS
       IF(((J + K).GT.BUFFERS).OR.(IFLAG.EQ.1)) J = 1
       INMAX  =  BUFFERS - J + 1
       ISTM   =  J
       INAROW =  0


C      LOOK AT THE GLOBAL WRITE FLAG.  IF THERE ARE NO BUFFERS
C      IN WRITE MODE SKIP THE PAINFULL SEARCH.

       IF(SHIFTR(FET(9,INDEX),62).NE.0)
     +   THEN

           FLIP    =  .TRUE.
           INAROW  =  0
           INMAX   =  0
           IST     =  0
           ISTM    =  0
           IL      =  1
           IF(BUFFERS.GT.20)IL = J

           DO 20 I = IL,BUFFERS
           IF(WABP(I).GE.0)
     +       THEN
               INAROW  = INAROW + 1
               IF(FLIP)
     +           THEN
                   IST  =  I
                   FLIP =  .FALSE.
                 ENDIF
             ELSE
               IF(INAROW.GT.INMAX)
     +           THEN
                   INMAX = INAROW
                   ISTM  = IST
                 ENDIF
               INAROW = 0
               FLIP   = .TRUE.
             ENDIF
20         CONTINUE

           IF(INAROW.GT.INMAX)
     +       THEN
               INMAX = INAROW
               ISTM  = IST
             ENDIF
         ENDIF

C      NOW SEE WHAT WE FOUND FOR MAX BUFFERS.

       IF(INMAX.GE.K)
     +   THEN
           IF((ISTM.GT.J).OR.((J+K).GT.(ISTM+INMAX)))J = ISTM
           PRU          = ((J - 1) * BSIZE) + 1
           IBLOCK       = J
           J            = J + K
           IF(J.GT.BUFFERS) J = 1
         ELSE
           CALL WFLUSH(1,BUFFERS)
           PRU          = 1
           IBLOCK       = 1
           J            = 1
         ENDIF
       FET(17,INDEX) = J

C      AT THIS POINT WE EITHER FOUND FREE BUFFERS OR CREATED FREE
C      SPACE BY A CALL TO *WFLUSH*. THE VARIABLE *PRU* WILL HAVE
C      THE OFFSET INTO THE WORD ADDRESS BUFFER(*WAA*) WHERE THIS
C      READ OR WRITE WILL GO.  THE VARIABLE *IBLOCK* CONTAINS THE
C      INDEX INTO THE WORD ADDRESS BUFFER POINTER ARRAY(*WABP*) WHERE
C      THE NEW WORD ADDRESSES WILL GO.  THE DATA WILL BE READ IN IF
C      NEEDED AND THE WORD ADDRESSES PUT IN THE WORD ADDRESS TABLE.

40     CONTINUE
       WADD = BSIZE * (FIRST - 1) + 1
       L    = IBLOCK + BLOCKS - 1
       IND = PRU
       WADD1 = WADD
       LWA  = WADD + (BLOCKS - 1) * BSIZE
       IF(READ)
     +   THEN

C          IF LAST BLOCK TO BE READ IS IN BUFFER AND MARKED FOR
C          WRITE THEN FLUSH BUFFER TO AVOID POSSIBLE PROBLEM AT
C          EOI.

           IX   = IXMM@(WABP(1),BUFFERS,MASK(64),LWA.OR.SIGNBIT)
           IF(IX.NE.0) CALL WFLUSH(1,BUFFERS)
           ICDK = BSIZE * BLOCKS
           IRCL = FIND
         ELSE
           ICDK = BSIZE
           IRCL = 0
           IF(PARTF.AND.FIRST.EQ.START)         ! part 1st sect now
     +       THEN
               PARTF = .FALSE.
               IF((BLKCNT.LE.BUFFERS).AND.PARTEND)
     +           THEN
                   IX = IXMM@(WABP(1),BUFFERS,MASK(64),LWA .OR. SIGNBIT)
                   IF(IX.NE.0) CALL WFLUSH(1,BUFFERS)
                   PARTEND = .FALSE.
                   ICDK    = BSIZE * BLKCNT
                 ENDIF
               IF(FIRST.GT.EOIBW) GOTO 50
               IDSKLIM = (EOIBW - FIRST + 1) * BSIZE
               IF(ICDK.GT.IDSKLIM) ICDK = IDSKLIM ! only read what's been writ
             ELSE
               BLOCKS = LAST - FIRST + 1
               IF(PARTEND.AND.BLOCKS.LE.BUFFERS) ! last part sector in buf
     +           THEN
                   IF (LAST .GT. EOIBW) THEN
                     CALL WFLUSH(1,BUFFERS)
                     GOTO 50
                   ENDIF
                   WADD1 = BSIZE * (LAST - 1) + 1
                   IND   = BSIZE * (BLOCKS - 1) + PRU ! read only last sector
                 ELSE
                   GOTO 50                       ! full sectors
                 ENDIF
             ENDIF
         ENDIF

C      FLUSH THE BUFFER, IF NECESSARY, TO EXTEND FILE OUT TO FULL SIZE BEFORE
C      READING.

       IF (FIRST+BLOCKS-1 .GT. EOIBW) CALL WFLUSH(1,BUFFERS)

       FET(6,INDEX)  = FET(6,INDEX) + 1
       FET(13,INDEX) = FET(13,INDEX) + ICDK
       IT1   = IRTC()
       CALL READWA(INDEX,WAA(IND),WADD1,ICDK,IRCL)
       IT2  = IRTC()
       FET(10,INDEX) = FET(10,INDEX) + (IT2-IT1)
       IF(IRCL.NE.0) FET(2,INDEX) = FET(2,INDEX).OR.SIGNBIT

C      NOW PUT THE NEW WORD ADDRESSES IN THE WORD ADDRESS TABLE, BUT
C      MAKE SURE THERE ARE NO DUPLICATE ENTRIES.

   50  CONTINUE
       DO 60 I = IBLOCK,L
       FLIP    = .TRUE.
       J       = IXMM@(WABP,BUFFERS,MSK,WADD)
       IF (J.EQ.I) GOTO 55
       IF(J.NE.0)
     +   THEN
           IF(WABP(J).LT.0 .OR. (PARTEND.AND.I.EQ.L)) 
     +       THEN
               FLIP = .FALSE.
             ELSE
               WABP(J) = 2
             ENDIF
         ENDIF
       WABP(I) = 2
       IF(FLIP) WABP(I) = WADD
55     CONTINUE
       WADD    = WADD + BSIZE
60     CONTINUE

       RETURN
       END