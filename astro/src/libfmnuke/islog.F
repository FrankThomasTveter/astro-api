      BLOCK DATA BDISLOG
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISLOG/ LENS
      DATA LENS /1/
      END BLOCK DATA



      LOGICAL FUNCTION ISLOG(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IF STRING IS LOGICAL                                           *
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
      LOGICAL OUT
C
      CHARACTER*7 TEXT
      INTEGER LENS,LENGTH
      LOGICAL BUFF
      EXTERNAL LENGTH
C
      COMMON /CCISLOG/ LENS
C
      CALL CHOP0(STRING,250)
      LENS=MIN(MAX(LENS,1),250)
      LENS=LENGTH(STRING,250,LENS)
C
      BUFF=.FALSE.
      IF (LENS.LE.7) THEN
C
         CALL RESET(TEXT,7)
         TEXT(1:LENS)=STRING(1:LENS)
C
         IF (TEXT(1:LENS).EQ.'T') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'TRUE') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'.TRUE.') THEN
            OUT=.TRUE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'F') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'FALSE') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ELSEIF(TEXT(1:LENS).EQ.'.FALSE.') THEN
            OUT=.FALSE.
            BUFF=.TRUE.
         ENDIF
      ENDIF
C
      ISLOG=BUFF
C
      RETURN
      END
