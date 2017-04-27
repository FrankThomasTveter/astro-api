      SUBROUTINE NUKECP(STRING,ST1,ST2,N)
C     ***********************************************************************
C     +                                                                     *
C     REPLACES ST1 BY ST2 IN STRING (WITH LENGTH N)
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    ST1 (C*1) = CHARACTER(S) WHICH SHOULD BE REMOVED FROM STRING          *
CI    ST2 (C*1) = CHARACTER WHICH SHOULD REPLACE ST1                        *
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
      CHARACTER*1 ST1,ST2
      INTEGER II,LENS,LENGTH
      EXTERNAL LENGTH
C
      LENS=LENGTH(STRING,N,1)
C
      DO II=1,LENS
         IF (STRING(II:II).EQ.ST1) STRING(II:II)=ST2
      ENDDO
C
      CALL CHOP0(STRING,LENS)
C
      RETURN
      END
