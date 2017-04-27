      SUBROUTINE CLEANUP(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES {}, AND REPLACES [] WITH () IN STRING                         *
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
      CHARACTER*1 BUFF
C
      INTEGER II,LENGTH,LENS,DELL,IIZERO
      LOGICAL DIGIT,ACTIVE
      EXTERNAL LENGTH,DIGIT
C
      LENS=LENGTH(STRING,N,1)
      DELL=1
      II=1
      DO WHILE(II.LE.LENS)
C     REPLACE [] WITH ()
         IF (STRING(II:II).EQ.'[') THEN
            BUFF='('
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ELSEIF (STRING(II:II).EQ.']') THEN
            BUFF=')'
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ENDIF
C     REMOVE 'PROTECTIVE BRACKETS' {}, OR GOTO NEXT CHARACTER
         IF (STRING(II:II).EQ.'{'.OR.STRING(II:II).EQ.'}') THEN
            CALL SUBSTR(STRING,N,BUFF,0,II,DELL)
            LENS=LENS-1
         ELSE
            II=II+1
         ENDIF
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
