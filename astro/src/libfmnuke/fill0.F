C Library: strgen4 $Id: fill0.F,v 1.1 2012-12-10 12:03:54 franktt Exp $
C
      SUBROUTINE FILL0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     INSERTS SURPLUS BLANKS IN A STRING                                    *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME        |   DATE   |                 REASON                | *
C     --------------------------------------------------------------------- *
C     | F. TVETER      | 18/07/95 | NEW                                   | *
C     | T. WILHELMSSON | 24/11/06 | Remove SAVE statements                | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      INTEGER II,JJ
C
      C1=CHAR(0)
C
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (STRING(II:II).EQ.C1) STRING(II:II)=' '
      ENDDO
      RETURN
      END
