      subroutine QBDY(MAXUNIT,FILES,LUNIT,CUNIT,
     &     MAXHDR,HDR250,NRLEN,PPDAT,INTOUT,
     &     CNT,CODHDR,LINE,DAT250,
     &     MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,
     &     BUFF1000,LENB,PROCESS,BDONE,IRC)
C     
      implicit none
C     
      INTEGER MAXUNIT
      CHARACTER*250 FILES(MAXUNIT)
      INTEGER UNITA(MAXUNIT),LUNIT(MAXUNIT),CUNIT
      INTEGER MAXHDR,NROPT
      CHARACTER*250 HDR250(MAXHDR)
      PARAMETER (NROPT=5)
      LOGICAL PPDAT(NROPT,MAXHDR)
      INTEGER INTOUT, CNT,CODHDR(MAXHDR),LINE
      CHARACTER*250 DAT250
      INTEGER NRLEN(3,MAXHDR),MAXVAR,NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
      INTEGER VARLEN(MAXVAR)
      CHARACTER*1000 BUFF1000
      INTEGER LENB
      LOGICAL PROCESS, BDONE
      INTEGER IRC
C     
      CHARACTER*16 MYNAME
      DATA MYNAME /'QBDY'/
      INTEGER LENGTH,LENF,LEND
      EXTERNAL LENGTH
      CHARACTER*1000 DUMP
      LOGICAL ERROR
C     
C     PRE-PROCESS LINE IF THIS IS REQUIRED
C     
      ERROR=.FALSE.
      IF (LINE.NE.-1) THEN
         PROCESS=.FALSE.
         CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,VALVAR,VARLEN,
     &        PPDAT(1,LINE),IRC)
         IF (IRC.NE.0) THEN
            IRC=0
            ERROR=.TRUE.
            LENB=LENGTH(BUFF1000,250,10)
            BUFF1000='Error return from EVAL '//
     &           BUFF1000(1:LENB)//').'
         ELSE
            LENB=LENGTH(BUFF1000,1000,LENB)
            IF (LENB.EQ.0) THEN
C     DATA LINE IS EMPTY
               WRITE(*,*) MYNAME,
     &              'Warning: empty data line found under: '//
     &              HDR250(LINE)(1:NRLEN(1,LINE))
            ELSE
C     
C     DO NOT ALLOW RESIDUAL VARIABLES
C     
               CALL QVAR(BUFF1000,1000,
     &              NRVAR,NAMVAR,VALVAR,VARLEN,IRC)
               IF (IRC.NE.0) THEN
                  ERROR=.TRUE.
                  IF (.NOT. PPDAT(1,LINE)) THEN ! variable substitution not active in header
                     BUFF1000='Header does not use '
     &                    //'variable substitution?'
                  ELSE
                     BUFF1000='Undefined variable.'
                  END IF
               ELSE
C     
C     STORE DATA LINE
C     
                  BDONE=.TRUE.
C     
                  INTOUT=LINE
                  DAT250=BUFF1000(1:250)
C     
                  CNT=CNT+1
C     
C     CHECK IF WE ARE ACCUMULATING TOO MANY LINES
C     
                  IF (CODHDR(LINE).NE.-1 .AND. 
     &                 CNT.GT.CODHDR(LINE)) THEN
                     ERROR=.TRUE.
                     write(BUFF1000,'(A,I5,A,I5,4(A))')
     &                    'Too many data lines, found:',
     &                    cnt,', expected:',CODHDR(LINE),
     &                    ', Header:',HDR250(LINE)
     &                    (1:NRLEN(1,LINE)),' Body:',
     &                    dat250
                  END IF
               END IF
C     
            ENDIF
         ENDIF
      ENDIF
C     
      IF (ERROR) THEN
         CALL CHOP0(FILES(CUNIT),250)
         LENF=LENGTH(FILES(CUNIT),250,10)
         CALL CHOP0(BUFF1000,250)
         LENB=LENGTH(BUFF1000,250,10)
         IF (LENF.GT.80)LENF=80
         WRITE(DUMP,*) 
     &        '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &        LUNIT(CUNIT),': Error: '//BUFF1000(1:LENB)
         CALL CHOP0(DUMP,200)
         LEND=LENGTH(DUMP,200,20)
         WRITE(*,*) DUMP(1:LEND)
         IRC=291
         RETURN
      ENDIF
C     
      RETURN
      END
