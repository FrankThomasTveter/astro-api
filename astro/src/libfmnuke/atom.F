      LOGICAL FUNCTION ATOM(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     CHECK IF STRING CONSISTS OF A SINGLE OBJECT                           *
C     ....(THAT DOES NOT NEED BRACKETS)                                     *
C     +                                                                     *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
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
C
      INTEGER LENS,LENGTH,POS(2),CODE,START
      EXTERNAL LENGTH
      LOGICAL BUFF
C
      CALL CHOP0(STRING,N)
      LENS=LENGTH(STRING,N,1)
C
      START=1
      CODE=1
      CALL OBJECT(STRING,N,START,POS,CODE)
C
      BUFF=(POS(1).EQ.1 .AND. POS(2).EQ.LENS)
C
      ATOM=BUFF
C
      RETURN
      END
