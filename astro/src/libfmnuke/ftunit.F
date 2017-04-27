      INTEGER FUNCTION FTUNIT(IRC)
C***********************************************************************
C                                                                      *
C WRITTEN/MODIFIED BY:                                                 *
C--------------------------------------------------------------------- *
C|    NAME      |   DATE   |                 REASON                  | *
C--------------------------------------------------------------------- *
C| L. BEHRENDT  | 04/03/91 | NEW.                                    | *
C|              |          |                                         | *
C--------------------------------------------------------------------- *
C***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER IRC
C
      CHARACTER*16 MYNAME
      INTEGER UN,maxun
      data maxun /91/
      LOGICAL FOUND,T,F
C
      DATA F       /.FALSE./
      DATA T       /.TRUE./
C
      DATA MYNAME /'FTUNIT'/
C
C
C------- Find an unused unit number between 30 and 99.
C
C      un=maxun
C      maxun=maxun-1
C      ftunit=un
C      write(*,*) myname,'Hardcoding unit number to:',UN
C      return
C
      UN=MAXUN
      FOUND=T
      DO WHILE (FOUND.AND.UN.GE.30)
         UN=UN-1
         INQUIRE(UNIT=UN,OPENED=FOUND)
C     WRITE(*,*) MYNAME,UN,FOUND
      ENDDO
      IF (FOUND) THEN
         WRITE(*,*) MYNAME,': ----------- WARNING ------------'
         WRITE(*,*) MYNAME,': NO UNUSED UNITS FOUND (30 TO 99)'
         WRITE(*,*) MYNAME,': ----------- WARNING ------------'
         IRC=107
      ELSE
         FTUNIT=UN
      ENDIF
C
      RETURN
      END
