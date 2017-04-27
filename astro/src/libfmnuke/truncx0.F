      SUBROUTINE TRUNCX0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES TRAILING 0 FROM EXPRESSION                                    *
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
C
C     REMOVE TRAILING '0'ES
C
      IIZERO=LENS+1
      ACTIVE=.FALSE.
      II=0
      DO WHILE(II.LE.LENS)
         II=II+1
         IF ((II.LE.LENS).AND.
     &        (DIGIT(STRING(II:II)).OR.
     &        (STRING(II:II).EQ.'.'))) THEN
            IF (STRING(II:II).EQ.'0') THEN
               IF (ACTIVE.AND.IIZERO.GT.II) IIZERO=II
            ELSEIF(STRING(II:II).EQ.'.') THEN
               ACTIVE=.TRUE.
C     WE DO NOT WANT THE PROGRAM TO CONVERT REAL TO INTEGERS, I.E. 11.0 => 11
C     IF (IIZERO.GT.II) IIZERO=II
            ELSE
               IIZERO=LENS+1
            ENDIF
         ELSE
            IF (ACTIVE.AND.IIZERO.LT.II) THEN
C     REMOVE EVERYTHING FROM IIZERO TO II-1
               DELL=II-IIZERO
               CALL SUBSTR(STRING,N,BUFF,0,IIZERO,DELL)
               II=IIZERO+1
               LENS=LENGTH(STRING,N,LENS-DELL)
               IIZERO=LENS+1
            ENDIF
            ACTIVE=.FALSE.
         ENDIF
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
