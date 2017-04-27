      SUBROUTINE GETFLAGS(BUFF250,
     &           PPDAT,
     &           IRC)
C
      IMPLICIT NONE
C
      INTEGER NROPT
      PARAMETER(NROPT=5)
C
      character*250 buff250
      LOGICAL PPDAT(NROPT)
      integer irc
C
      CHARACTER*1 OPT,OPTL(NROPT)
      DATA OPTL /'V','F','M','L','R'/
C     
      logical bdone,bbdone,found
      integer lenb, length,kk,jj
      external length
      CHARACTER*16 MYNAME
      DATA MYNAME /'GETFLAGS'/
C     
C     CHECK FOR PRE-PROCESSING
C     
      DO KK=1,NROPT
         PPDAT(KK)=.FALSE.
      ENDDO
C     
C     CHECK FOR PRE-PROCESSOR FLAGS
C     
      LENB=LENGTH(BUFF250,250,10)
      JJ=1
      BDONE=(JJ.GT.LENB)
      DO WHILE (.NOT.BDONE)
         OPT=BUFF250(JJ:JJ)
         FOUND=.FALSE.
         KK=1
         BBDONE=(KK.GT.NROPT)
         DO WHILE (.NOT.BBDONE)
            IF (OPT.EQ.OPTL(KK)) THEN
               PPDAT(KK)=.TRUE.
               FOUND=.TRUE.
               BBDONE=.TRUE.
            ELSE
               KK=KK+1
               BBDONE= (KK.GT.NROPT) 
            ENDIF
         END DO
         IF (.NOT.FOUND) THEN
            BDONE=.TRUE.
         ELSE
            JJ=JJ+1
            BDONE=(JJ.GT.LENB)
         END IF
      END DO
C
      BUFF250=BUFF250(MAX(1,JJ):LENB)
      call chop0(buff250,250)
C
      RETURN
      END 
