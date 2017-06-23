      SUBROUTINE CLEAN0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES ILLEGAL CHARACTERS BLANKS FROM A STRING                       *
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
      INTEGER II
      CHARACTER*1 C1
      C1=CHAR(0)
      DO II=1,N
      if (string(II:II).eq.'<'.or. 
     &     string(II:II).eq.'>'.or. 
     &     string(II:II).eq.'"'.or. 
     &     string(II:II).eq."'") THEN
         STRING(II:II)=" "
      ELSE IF (string(ii:ii).EQ.c1) THEN
            EXIT
         end if
      end do
      RETURN
      END
