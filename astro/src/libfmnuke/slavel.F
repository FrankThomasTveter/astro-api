      BLOCK DATA BDSLAVEL
      IMPLICIT NONE
      INTEGER LEN1,LEN2
      COMMON /CCSLAVEL/LEN1,LEN2
      DATA LEN1 /1/,LEN2 /1/
      END BLOCK DATA


      SUBROUTINE SLAVEL(STRING1,STRING2,STRING3,OP,CLEAN,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EXECUTES MATHEMATICAL OPERATION 'OP' ON STRING1 AND STRING2.          *
C     THE RESULT IS RETURNED IN STRING3                                     *
C     +                                                                     *
CI    STRING1 (C*250) = FIRST ARGUMENT                                      *
CI    STRING2 (C*250) = SECOND ARGUMENT                                     *
CO    STRING3 (C*250) = RESULT                                              *
CI    OP (I*4) = OPERATION,                                                 *
C     +      =1  ==                                                         *
C     +      =2  >=                                                         *
C     +      =3  <=                                                         *
C     +      =4  <>                                                         *
C     +      =5  >                                                          *
C     +      =6  <                                                          *
C     +      =7  !                                                          *
C     +      =8  &&                                                         *
C     +      =9  ||                                                         *
CIO   CLEAN (L*4) = .TRUE. IF NO BRACKETS SHOULD BE ADDED (IF NOT EVAL)     *
CO    IRC=ERROR RETURN CODE (IRC=0 OK, IRC=1 IF NO OPERATION)               *
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
      CHARACTER*250 STRING1,STRING2,STRING3
      CHARACTER*250 STRB1,STRB2
      LOGICAL CLEAN,CHANGED
      INTEGER OP,IRC
C
      INTEGER LEN1,LEN2,LEN3,LENGTH,ISI(2)
      REAL ISR(2)
C
      LOGICAL ISINT,ISREAL,QINT(2),QREA(2),QLOG(2),
     &     CAMEWS,PSGN(2),ATOM,ISL(2),RRL,DONE,ISLOG
      EXTERNAL LENGTH,ISINT,ISREAL,ISLOG,ATOM
C
      common /ccslavel/LEN1,LEN2
C
C     INITIALIZE
C
      IRC=0
      CALL CHOP0(STRING1,250)
      CALL CHOP0(STRING2,250)
      CALL RESET(STRING3,250)
C
      STRB1=STRING1
      STRB2=STRING2
C
      LEN1=LENGTH(STRB1,250,LEN1)
      LEN2=LENGTH(STRB2,250,LEN2)
C
      CAMEWS=(STRB1(1:1).EQ.'-'.OR.STRB1(1:1).EQ.'+')
C
      IF (STRB1(1:1).EQ.'-') THEN
         PSGN(1)=.FALSE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSEIF (STRB1(1:1).EQ.'+') THEN
         PSGN(1)=.TRUE.
         STRB1=STRB1(2:LEN1)
         LEN1=LEN1-1
      ELSE
         PSGN(1)=.TRUE.
      ENDIF
C
      IF (STRB2(1:1).EQ.'-') THEN
         PSGN(2)=.FALSE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSEIF (STRB2(1:1).EQ.'+') THEN
         PSGN(2)=.TRUE.
         STRB2=STRB2(2:LEN2)
         LEN2=LEN2-1
      ELSE
         PSGN(2)=.TRUE.
      ENDIF
C
      IF (STRB1(1:1).EQ.'['.AND.STRB1(LEN1:LEN1).EQ.']') THEN
         IF (ATOM(STRB1(2:LEN1-1),LEN1-2)) THEN
            STRB1=STRB1(2:LEN1-1)
         ENDIF
      ENDIF
C
      IF (STRB2(1:1).EQ.'['.AND.STRB2(LEN2:LEN2).EQ.']') THEN
         IF (ATOM(STRB2(2:LEN2-1),LEN2-2)) THEN
            STRB2=STRB2(2:LEN2-1)
         ENDIF
      ENDIF
C
C     FIND OUT IF NUMBERS ARE INTEGERS OR REAL
C
      QINT(1)=ISINT(STRB1,ISI(1))
      QINT(2)=ISINT(STRB2,ISI(2))
      QREA(1)=ISREAL(STRB1,ISR(1))
      QREA(2)=ISREAL(STRB2,ISR(2))
      QLOG(1)=ISLOG(STRB1,ISL(1))
      QLOG(2)=ISLOG(STRB2,ISL(2))
C
      DONE=.FALSE.
C
      IF (QINT(1).AND.QINT(2)) THEN
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).EQ.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).GE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).LE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).NE.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.5) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).GT.ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.6) THEN
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRL=ISI(1).LT.ISI(2)
            DONE=.TRUE.
         ENDIF
      ELSEIF ((QINT(1).AND.QREA(2)).OR.
     &        (QINT(2).AND.QREA(1)).OR.
     &        (QREA(1).AND.QREA(2))) THEN
         IF (QINT(1)) ISR(1)=REAL(ISI(1))
         IF (QINT(2)) ISR(2)=REAL(ISI(2))
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).EQ.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).GE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).LE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).NE.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.5) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).GT.ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.6) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRL=ISR(1).LT.ISR(2)
            DONE=.TRUE.
         ENDIF
      ELSE
         IF (OP.EQ.1.AND.STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSEIF (OP.EQ.2 .AND. STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSEIF (OP.EQ.3 .AND. STRING1.EQ.STRING2) THEN
            RRL=.TRUE.
            DONE=.TRUE.
         ELSE
            IF  (QLOG(2) .AND.
     &           (OP.EQ.7.OR.(QLOG(1).AND.OP.GE.8))) THEN
               IF (OP.EQ.7) THEN
                  RRL=(.NOT.ISL(2))
                  DONE=.TRUE.
               ELSEIF (OP.EQ.8) THEN
                  RRL=(ISL(1).AND.ISL(2))
                  DONE=.TRUE.
               ELSEIF (OP.EQ.9) THEN
                  RRL=(ISL(1).OR.ISL(2))
                  DONE=.TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF (DONE) THEN
         CHANGED=.TRUE.
         IF (RRL) THEN
            STRING3='T'
         ELSE
            STRING3='F'
         ENDIF
         IF (CAMEWS) THEN
            IF (PSGN(1)) THEN
               STRING3='+'//STRING3(1:249)
            ELSE
               STRING3='-'//STRING3(1:249)
            ENDIF
         ENDIF
         IF (OP.EQ.7) THEN
            LEN1=LENGTH(STRING1,250,LEN1)
            STRING3=STRING1(1:LEN1)//STRING3(1:250-LEN1)
            CALL CHOP0(STRING3,250)
         ENDIF
      ELSE
C
C     ADD PROTECTIVE BRACKETS...
C
         LEN1=LENGTH(STRING1,250,LEN1)
         LEN2=LENGTH(STRING2,250,LEN1)
         IF (OP.EQ.1) THEN
            STRING3=STRING1(1:LEN1)//'=='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.2) THEN
            STRING3=STRING1(1:LEN1)//'>='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.3) THEN
            STRING3=STRING1(1:LEN1)//'<='//STRING2(1:LEN2)
         ELSEIF (OP.EQ.4) THEN
            STRING3=STRING1(1:LEN1)//'<>'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.5) THEN
            STRING3=STRING1(1:LEN1)//'>'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.6) THEN
            STRING3=STRING1(1:LEN1)//'<'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.7) THEN
            STRING3=STRING1(1:LEN1)//'!'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.8) THEN
            STRING3=STRING1(1:LEN1)//'&&'//STRING2(1:LEN2)
         ELSEIF (OP.EQ.9) THEN
            STRING3=STRING1(1:LEN1)//'||'//STRING2(1:LEN2)
         ENDIF
C
C     NO OPERATION WAS PERFORMED
C
         IRC=105
C
         IF (.NOT.CLEAN) THEN
            CALL CHOP0(STRING3,250)
            LEN3=LENGTH(STRING3,250,LEN1+LEN2+1)
            STRING3='{'//STRING3(1:LEN3)//'}'
         ENDIF
      ENDIF
      RETURN
      END
