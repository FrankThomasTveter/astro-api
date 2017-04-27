      BLOCK DATA BDOBJECT
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCOBJECT/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE OBJECT(STRING,N,START,POS,CODE)
C     ***********************************************************************
C     +                                                                     *
C     IDENTIFIES AN OBJECT (VARIABLE, OPERAND, NUMBER), AND RETURNS LOCATION*
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    START (I*4) = POSITION AT WHICH TO START SEARCH                       *
CO    POS(2) (I*4) = START AND END POSITION OF OBJECT                       *
CO    CODE (I*4) = OBJECT TYPE IDENTIFIER                                   *
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
      CHARACTER*1 XCAR
      INTEGER START,POS(2),CODE,ICODE
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'OBJECT'/
C
      INTEGER LENS,LENGTH,LEVEL,OLEVEL,JJ,CODE2,PAR,CPOS
      LOGICAL FOBJ,OFOBJ,QREAL,OQREAL,DIGIT,DEFNTNR,NEED
      EXTERNAL LENGTH,DIGIT,DEFNTNR
      COMMON /CCOBJECT/ LENS
C
      CPOS=0
      IF (CODE.EQ.1) THEN
C
C------SEARCH FORWARDS
C
         CODE2=1
         POS(1)=START
         PAR=0
C
C------IF WE PREVIOUS CHAR WAS A SIGN, SHIFT POS
C
         IF (START.GT.1) THEN
            CALL WHICH(STRING(START-1:START-1),ICODE)
            IF (ICODE.EQ.5.OR.ICODE.EQ.6) THEN
               POS(1)=START-1
            ENDIF
         ENDIF
C
C------IF WE ARE CURRENTLY AT A SIGN, MOVE FORWARDS
C
         IF (START.LT.N .AND. POS(1).EQ.START) THEN
            CALL WHICH(STRING(START:START),ICODE)
            IF(ICODE.EQ.5.OR.ICODE.EQ.6) START=START+1
         ENDIF
C
         LEVEL=0
         OLEVEL=0
         POS(2)=START
         JJ=START
         OQREAL=.TRUE.
         OFOBJ=.FALSE.
         FOBJ=.FALSE.
C
         LENS=LENGTH(STRING,N,LENS)
         QREAL=(LENS.GT.0)
         IF (JJ.LE.N.AND.JJ.GE.1) THEN
            XCAR=STRING(JJ:JJ)
            CALL WHICH(XCAR,ICODE)
         ELSE
            ICODE=1
         ENDIF
         IF (ICODE.EQ.0) THEN
            DO WHILE (JJ.LE.LENS .AND.
     &           .NOT.FOBJ .AND. LEVEL.GE.0)
C
               IF (STRING(JJ:JJ).EQ.'(') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.')') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'[') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.']') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'{') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.'}') LEVEL=LEVEL-1
C
               IF (LEVEL.EQ.1 .AND. JJ.EQ.START) THEN
                  PAR=LEVEL
                  POS(1)=POS(1)+1
               ENDIF
C
               IF (LEVEL.EQ.0) THEN
                  IF (JJ.LE.N.AND.JJ.GE.1) THEN
                     XCAR=STRING(JJ:JJ)
                     CALL WHICH(XCAR,ICODE)
                     FOBJ=(ICODE.NE.0)
                  ELSE
                     FOBJ=.FALSE.
                  ENDIF
                  IF (OQREAL) QREAL=DEFNTNR(STRING(JJ:JJ),CODE2,NEED)
                  IF (FOBJ.AND.OQREAL.AND.QREAL) FOBJ=.FALSE.
                  IF (.NOT.FOBJ) CPOS=JJ
                  IF (OLEVEL.NE.0) FOBJ=.TRUE.
                  OQREAL=QREAL
               ELSEIF(LEVEL.GT.0.AND.OLEVEL.EQ.0.AND.PAR.EQ.0) THEN
                  FOBJ=.TRUE.
               ELSEIF(LEVEL.GT.0) THEN
                  CODE2=+1
                  QREAL=.FALSE.
                  NEED=.FALSE.
                  FOBJ=.FALSE.
               ELSE
                  FOBJ=.TRUE.
               ENDIF
C
               IF (.NOT. FOBJ) THEN
                  IF (LEVEL.EQ.PAR) POS(2)=JJ
                  CPOS=JJ
                  OLEVEL=LEVEL
                  IF (OQREAL) OQREAL=QREAL
                  JJ=JJ+1
               ENDIF
C
            ENDDO
         ELSE
            CODE=ICODE
         ENDIF
      ELSEIF(CODE.EQ.-1) THEN
C
C-----SEARCH BACKWARDS
C
         CODE2=-1
         LEVEL=0
         OLEVEL=0
         POS(1)=START+1
         POS(2)=START
         JJ=START
         OQREAL=.TRUE.
         OFOBJ=.FALSE.
         FOBJ=.FALSE.
         PAR=0
C
         LENS=LENGTH(STRING,N,LENS)
         QREAL=(LENS.GT.0)
         IF (JJ.LE.N.AND.JJ.GE.1) THEN
            XCAR=STRING(JJ:JJ)
            CALL WHICH(XCAR,ICODE)
         ELSE
            ICODE=1
         ENDIF
         IF (ICODE.EQ.0) THEN
            DO WHILE (JJ.GE.1 .AND.
     &           .NOT.FOBJ .AND. LEVEL.LE.0)
C
               IF (STRING(JJ:JJ).EQ.'(') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.')') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'[') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.']') LEVEL=LEVEL-1
               IF (STRING(JJ:JJ).EQ.'{') LEVEL=LEVEL+1
               IF (STRING(JJ:JJ).EQ.'}') LEVEL=LEVEL-1
C
               IF (LEVEL.EQ.-1 .AND. JJ.EQ.START) THEN
                  PAR=LEVEL
                  POS(2)=POS(2)-1
               ENDIF
C
               IF (LEVEL.EQ.0) THEN
                  IF (JJ.LE.N.AND.JJ.GE.1) THEN
                     XCAR=STRING(JJ:JJ)
                     CALL WHICH(XCAR,ICODE)
                     FOBJ=(ICODE.NE.0)
                  ELSE
                     FOBJ=.FALSE.
                  ENDIF
                  IF (OQREAL) QREAL=DEFNTNR(STRING(JJ:JJ),CODE2,NEED)
                  IF (FOBJ.AND.OQREAL.AND.QREAL)FOBJ=.FALSE.
                  IF (.NOT.FOBJ) CPOS=JJ
                  IF (OLEVEL.NE.0) FOBJ=.TRUE.
                  OQREAL=QREAL
               ELSEIF(LEVEL.LT.0.AND.OLEVEL.EQ.0.AND.PAR.EQ.0) THEN
                  FOBJ=.TRUE.
               ELSEIF(LEVEL.LT.0) THEN
                  CODE2=-1
                  QREAL=.FALSE.
                  NEED=.FALSE.
                  FOBJ=.FALSE.
               ELSE
                  FOBJ=.TRUE.
               ENDIF
C
               IF (.NOT. FOBJ) THEN
                  IF (LEVEL.EQ.PAR) POS(1)=JJ
                  CPOS=JJ
                  OLEVEL=LEVEL
                  IF (OQREAL) OQREAL=QREAL
                  JJ=JJ-1
               ENDIF
C
            ENDDO
C
C------IF WE PREVIOUS CHAR WAS A SIGN, SHIFT POS
C
            IF (CPOS.GT.1.AND.
     &           STRING(CPOS:CPOS).NE.'-'.AND.
     &           STRING(CPOS:CPOS).NE.'+') THEN
               CALL WHICH(STRING(CPOS-1:CPOS-1),ICODE)
               IF (ICODE.EQ.5.OR.ICODE.EQ.6) THEN
                  CPOS=CPOS-1
               ENDIF
            ENDIF
         ELSE
            CODE=ICODE
         ENDIF
      ELSE
         WRITE(*,*) MYNAME,'WRONG CODE',CODE
      ENDIF
C
      START=CPOS
      IF (CPOS.GE.1 .AND. CPOS.LE.N) THEN
         CODE=0
      ELSE
         CODE=ICODE
      ENDIF
C
      IF (CODE.EQ.0) THEN
C     CHECK IF OBJECT IS SURROUNDED BY PARANTHESIS
         IF (STRING(POS(1):POS(1)).EQ.'(') POS(1)=POS(1)+1
         IF (STRING(POS(2):POS(2)).EQ.')') POS(2)=POS(2)-1
      ENDIF
C
      RETURN
      END
