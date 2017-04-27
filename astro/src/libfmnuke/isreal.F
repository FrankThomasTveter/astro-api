      BLOCK DATA BDISREAL
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCISREAL/ LENS
      DATA LENS /1/
      END BLOCK DATA

      LOGICAL FUNCTION ISREAL(STRING,OUT)
C     ***********************************************************************
C     +                                                                     *
C     CHECKS IS STRING IS A REAL NUMBER                                     *
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
      REAL OUT
C
      INTEGER LENS,II,LENGTH,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL BUFF, DIGIT
      EXTERNAL DIGIT,LENGTH
      COMMON /CCISREAL/ LENS
C
      character*8 myname
      data myname /'ISREAL'/
C
      logical bdeb
      bdeb=.false.
C
      CALL CHOP0(STRING,250)
      LENS=LENGTH(STRING,250,LENS)
C
C
      POSSG=-1
      POSNR=-1
      POSDE=-1
      POSDD=-1
C
      NRSGN=0
      NRNRR=0
      NRDES=0
      NRDDE=0
C
      BUFF=(LENS.GT.0)
      II=0
      DO WHILE (II.LT.LENS.AND.BUFF)
         II=II+1
         IF (BUFF) THEN
            IF (STRING(II:II).EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=II
               BUFF=(NRDES.LE.1.AND.NRDDE.LT.1)
            ELSEIF (STRING(II:II).EQ.'D' .OR. STRING(II:II).EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=II
               BUFF=(NRDDE.LE.1 .AND. II.LT.LENS)
            ELSEIF (STRING(II:II).EQ.'-'.OR.STRING(II:II).EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=II
               BUFF=((NRSGN.LE.1.AND.II.EQ.1) .OR.
     &              (NRSGN.LE.2.AND.NRDDE.EQ.1
     &              .AND.POSDD.EQ.(POSSG-1)
     &              .AND.II.LT.LENS))
            ELSE
               BUFF=(DIGIT(STRING(II:II)))
               IF (BUFF) POSNR=II
            ENDIF
         ENDIF
      ENDDO
C
      IF (BUFF) BUFF=NRDES.GT.0
C
      IF (BUFF) THEN
         READ(STRING(1:LENS),*,ERR=99,END=99) OUT
         ISREAL=BUFF
         if (bdeb) write(*,*)myname,'Val:',
     &        string(1:lens),out
         RETURN
 99      BUFF=.FALSE.
         ISREAL=BUFF
         RETURN
      ENDIF
C
      ISREAL=BUFF
      RETURN
      END
