      BLOCK DATA BDEVAL
      IMPLICIT NONE
      LOGICAL ACTIVE,BDEB
      COMMON /CCEVAL/BDEB,ACTIVE
      DATA ACTIVE/.false./
      END BLOCK DATA


      SUBROUTINE EVAL(STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,
     &     LFL,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EVALUATES AN EXPRESSION                                               *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
      INTEGER NRVAR
      CHARACTER*250 NAMVAR(NRVAR),VALVAR(NRVAR)
      INTEGER VARLEN(NRVAR),IRC
C
      INTEGER II,CNT,LENS,LENGTH
      EXTERNAL LENGTH
      LOGICAL BDEB,ACTIVE
      LOGICAL CHANGED,CHNG(5),LFL(5)
      CHARACTER*16 MYNAME
      DATA MYNAME /'EVAL'/
      COMMON /CCEVAL/BDEB,ACTIVE
C
C     Debug System.
C
      BDEB=.false.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
C-----REMOVE UNNECCESARY BLANKS
C
      CALL CHOP0(STRING,N)
C
      IF (BDEB) THEN
         LENS=LENGTH(STRING,N,0)
         WRITE(*,*) MYNAME,'Debug:',(STRING(II:II),II=1,LENS)
      ENDIF
C
C-----INITIALISE
C
      CNT=0
      IRC=0
      CHANGED=.TRUE.
      DO WHILE(CHANGED)
C
C------VARIABLE SUBSTITUTION
C
         IF (LFL(1)) CALL EVALV(STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,
     &        CHNG(1),IRC)
         IF (IRC.NE.0) RETURN
C
C------FUNCTION EVALUATION
C
         IF (LFL(2)) CALL EVALF(STRING,N,CHNG(2),IRC)
         IF (IRC.NE.0) RETURN
C
C------MATHEMATICAL PROCESSING
C
         IF (LFL(3)) CALL EVALM(STRING,N,CHNG(3),IRC)
         IF (IRC.NE.0) RETURN
C
C------LOGICAL OPERATIONS
C
         IF (LFL(4)) CALL EVALL(STRING,N,CHNG(4),IRC)
         IF (IRC.NE.0) RETURN

C
C------RECURSIVE PROCESSING
C
         CHANGED=.FALSE.
         DO II=1,4
            IF(CHNG(II).AND.LFL(II)) CHANGED=LFL(5)
         ENDDO
C
C------UPDATE COUNTER AND MAKE SURE WE DON'T LOOP FOREVER
C
         IF (CNT.LT.250) THEN
            CNT=CNT+1
         ELSE
            IRC=999
            CHANGED=.FALSE.
            WRITE(*,*) MYNAME,'Error: recursive variable call.'
         ENDIF
      ENDDO
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine ends.',IRC
C
      RETURN
      END
