      SUBROUTINE CEQUAL(STRING1,N,STRING2,POS1,POS2)
C     ***********************************************************************
C     +                                                                     *
C     SETS STRING1 EQUAL TO STRING2 (FROM POS1 TO POS2)                     *
C     +                                                                     *
CO    STRING1 (C*N1) = STRING TO BE OVERWRITTEN                             *
CI    N1 (I*4) = LENGTH OF STRING                                           *
CI    STRING2 (C*N2) = STRING FROM WHICH TO COPY DATA, WHERE N2y.GT.POS2     *
CI    POS1 (I*4) = START POSITION IN STRING2 FROM WHICH TO START COPYING    *
CI    POS2 (I*4) = END POSITION IN STRING2 FROM WHICH TO END COPYING        *
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
      INTEGER N,POS1,POS2
      CHARACTER*(*) STRING1, STRING2
      CHARACTER*1 C1
C
      INTEGER II,MAXX
C
      C1=CHAR(0)
      MAXX=MIN(N,POS2-POS1+1)
      DO II=1,MAXX
         STRING1(II:II)=STRING2(POS1+II-1:POS1+II-1)
      ENDDO
      MAXX=MAXX+1
      IF (MAXX.LT.N) THEN
         DO II=MAXX,N
            STRING1(II:II)=C1
         ENDDO
      ENDIF
C
      RETURN
      END
