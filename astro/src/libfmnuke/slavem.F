      BLOCK DATA BDSLAVEM
      IMPLICIT NONE
      INTEGER LEN1,LEN2
      LOGICAL ACTIVE,BDEB
      COMMON /CCSLAVEM/LEN1,LEN2,ACTIVE,BDEB
      DATA LEN1 /1/,LEN2 /1/,ACTIVE /.FALSE./
      END BLOCK DATA


      SUBROUTINE SLAVEM(STRING1,STRING2,STRING3,OP,CLEAN,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EXECUTES MATHEMATICAL OPERATION 'OP' ON STRING1 AND STRING2.          *
C     THE RESULT IS RETURNED IN STRING3                                     *
C     +                                                                     *
CI    STRING1 (C*250) = FIRST ARGUMENT                                      *
CI    STRING2 (C*250) = SECOND ARGUMENT                                     *
CO    STRING3 (C*250) = RESULT                                              *
CI    OP (I*4) = OPERATION,                                                 *
C     +      =1  ^                                                          *
C     +      =2  *                                                          *
C     +      =3  /                                                          *
C     +      =4  +                                                          *
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
      INTEGER LEN1,LEN2,LEN3,LENGTH,ISI(2),RRI
      REAL ISR(2),RRR
C
      LOGICAL ISINT,ISREAL,QINT(2),QREA(2),
     &     CAMEWS,PSGN(2),PST,ATOM,DONE, GOINT
      EXTERNAL LENGTH,ISINT,ISREAL,ATOM
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'SLAVEM'/
C
      LOGICAL ACTIVE,BDEB
C
      common /ccslavem/LEN1,LEN2,ACTIVE,BDEB
C
C
      BDEB=.false.
C      BDEB=.true.
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
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
C
      DONE=.FALSE.
C
C     MAKE SURE WE DON'T RETURN AN INTEGER (0) FOR '2^(-2)'
      GOINT=(QINT(1).AND.QINT(2)) ! both numbers must be integers
      IF (GOINT.AND.OP.EQ.1) GOINT=(ISI(2).GE.0) ! negative exponents are always treated as real
      IF (GOINT.AND.OP.EQ.3) GOINT=(ISI(2).NE.0) ! we can not divide by 0
      IF (GOINT.AND.OP.EQ.3) GOINT=(MOD(ISI(1),ISI(2)).NE.0) ! check that division product is integer
      IF (GOINT) THEN
C     BOTH NUMBERS ARE INTEGERS
         IF (OP.EQ.1) THEN      ! ^
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)**ISI(2)
            IF (.NOT.PSGN(1)) RRI=-RRI
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN  ! *
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)*ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN ! /
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)/ISI(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN ! +
            IF (.NOT.PSGN(1)) ISI(1)=-ISI(1)
            IF (.NOT.PSGN(2)) ISI(2)=-ISI(2)
            RRI=ISI(1)+ISI(2)
            DONE=.TRUE.
         ENDIF
         WRITE(STRING3,*)RRI
         PST=(.NOT.(RRI.LT.0))
      ELSEIF ((QREA(1).OR.QINT(1)).AND.
     &        (QREA(2).OR.QINT(2))) THEN
C     CONVERT TO REAL
         IF (QINT(1)) ISR(1)=REAL(ISI(1))
         IF (QINT(2)) ISR(2)=REAL(ISI(2))
         IF (OP.EQ.1) THEN
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)**ISR(2)
            IF (.NOT.PSGN(1)) RRR=-RRR
            DONE=.TRUE.
         ELSEIF (OP.EQ.2) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)*ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.3) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)/ISR(2)
            DONE=.TRUE.
         ELSEIF (OP.EQ.4) THEN
            IF (.NOT.PSGN(1)) ISR(1)=-ISR(1)
            IF (.NOT.PSGN(2)) ISR(2)=-ISR(2)
            RRR=ISR(1)+ISR(2)
            DONE=.TRUE.
         ENDIF
         WRITE(STRING3,*)RRR
         PST=(.NOT.(RRR.LT.0))
      ENDIF
C
      IF (DONE) THEN
C
         CHANGED=.TRUE.
         CALL CHOP0(STRING3,250)
         IF (PST.AND.CAMEWS) THEN
            STRING3='+'//STRING3(1:249)
         ENDIF
C
      ELSE
C
C     ADD PROTECTIVE BRACKETS (OR ELSE '1+(2+3)^b'='6^b'
C
         IF (OP.EQ.1) THEN
            STRING3=STRB1(1:LEN1)//'^'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.2) THEN
            STRING3=STRB1(1:LEN1)//'*'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.3) THEN
            STRING3=STRB1(1:LEN1)//'/'//STRB2(1:LEN2)
         ELSEIF (OP.EQ.4) THEN
            LEN2=LENGTH(STRING2,250,LEN2)
            STRING3=STRB1(1:LEN1)//STRING2(1:LEN2)
         ENDIF
C
C     NO OPERATION WAS PERFORMED
C
         IRC=106
C
         IF (.NOT.CLEAN) THEN
            CALL CHOP0(STRING3,250)
            LEN3=LENGTH(STRING3,250,LEN1+LEN2+1)
            STRING3='{'//STRING3(1:LEN3)//'}'
         ENDIF
C
         IF (CAMEWS) THEN
            IF (PSGN(1)) THEN
               STRING3='+'//STRING3(1:249)
            ELSE
               STRING3='-'//STRING3(1:249)
            ENDIF
         ENDIF
      ENDIF
C
      IF (BDEB) THEN
         LEN2=LENGTH(STRING2,250,LEN2)
         LEN3=LENGTH(STRING3,250,LEN3)
         WRITE(*,*) MYNAME,'Debug: A1:',
     &     STRING1(1:LEN1),' A2:',STRING2(1:LEN2),
     &     ' A3:',STRING3(1:LEN3),' OP:',OP,goint
      ENDIF
C
      IF (BDEB) WRITE(*,*) MYNAME,'Debug: Routine starts.',IRC
C
      RETURN
      END
