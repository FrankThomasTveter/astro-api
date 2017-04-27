      BLOCK DATA BDWHICH
      IMPLICIT NONE
      LOGICAL FIRST
      COMMON /CCWHICH/FIRST
      DATA FIRST/.TRUE./
      END BLOCK DATA


      SUBROUTINE WHICH(CCHR,CODE)
C     ***********************************************************************
C     +                                                                     *
C     FINDS OUT IF STRING CONTAINS A SPECIAL CHARACTER AT POS               *
C     +                                                                     *
C     CCHR (C*1)                                                            *
CO    CODE (I*4) = OBJECT TYPE IDENTIFIER                                   *
C     +     =0 UNIDENTIFIED                                                 *
C     +     =1 ' '                                                          *
C     +     =2 '^'                                                          *
C     +     =3 '*'                                                          *
C     +     =4 '/'                                                          *
C     +     =5 '+'                                                          *
C     +     =6 '-'                                                          *
C     +     =7 '='                                                          *
C     +     =8 '&'                                                          *
C     +     =9 '|'                                                          *
C     +     =10'>'                                                          *
C     +     =11'<'                                                          *
C     +     =12'!'                                                          *
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
C
      IMPLICIT NONE
C
      CHARACTER*1 CCHR,C1
      INTEGER CODE
C
      INTEGER MAXOBJ,II
      PARAMETER (MAXOBJ=13)
      CHARACTER*1 OBJECTS(MAXOBJ)
      LOGICAL FIRST,FOUND
      COMMON /CCWHICH/ FIRST
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'WHICH'/
C
      IF (FIRST) THEN
         C1=CHAR(0)
         OBJECTS(1)=' '
         OBJECTS(2)='^'
         OBJECTS(3)='*'
         OBJECTS(4)='/'
         OBJECTS(5)='+'
         OBJECTS(6)='-'
         OBJECTS(7)='='
         OBJECTS(8)='&'
         OBJECTS(9)='|'
         OBJECTS(10)='>'
         OBJECTS(11)='<'
         OBJECTS(12)='!'
         OBJECTS(13)=C1
         FIRST=.FALSE.
      ENDIF
C
C     SEARCH IN STRING
C
      CODE=0
      II=1
      FOUND=.FALSE.
      DO WHILE (II.LE.MAXOBJ .AND..NOT.FOUND)
         IF (OBJECTS(II).EQ.CCHR) THEN
            IF (.NOT.FOUND) FOUND=.TRUE.
            IF (FOUND) CODE=II
         ELSE
            II=II+1
         ENDIF
      ENDDO
C
      RETURN
      END
