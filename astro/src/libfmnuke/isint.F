      BLOCK DATA BDISINT
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISINT/ LENS
      DATA LENS /1/
      END BLOCK DATA


      LOGICAL FUNCTION ISINT(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS AN INTEGER                                        *
C     +                                                                     *
CI    STRING (C*250) = STRING TO BE EXAMINED                                  *
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
      CHARACTER*250 STRING
      INTEGER OUT
C
      INTEGER LENS,II,LENGTH,NRSG
      LOGICAL BUFF, DIGIT
      EXTERNAL LENGTH,DIGIT
      COMMON /CCISINT/ LENS
C
      CALL CHOP0(STRING,250)
      LENS=LENGTH(STRING,250,LENS)
C
      BUFF=(LENS.GT.0)
      NRSG=0
      II=0
      DO WHILE (II.LT.LENS.AND.BUFF)
         II=II+1
         IF (BUFF) THEN
            IF (STRING(II:II).EQ.'-'.OR.STRING(II:II).EQ.'+') THEN
               NRSG=NRSG+1
               BUFF=(II.EQ.1.AND.NRSG.LE.1)
            ELSE
               BUFF=(DIGIT(STRING(II:II)))
            ENDIF
         ENDIF
      ENDDO
C
      IF (BUFF) THEN
         READ(STRING(1:LENS),*,ERR=99,END=99) OUT
         ISINT=BUFF
         RETURN
 99      BUFF=.FALSE.
         ISINT=BUFF
         RETURN
      ENDIF
C
      ISINT=BUFF
      RETURN
      END
