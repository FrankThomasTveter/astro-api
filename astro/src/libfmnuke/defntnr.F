      BLOCK DATA BDDEFNTNR
      IMPLICIT NONE
      INTEGER ICODE
      INTEGER JJ,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL QREAL,OQREAL,OUT
      COMMON /CCDEFNTNR/ICODE,JJ,POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE,QREAL,OQREAL,OUT
      DATA ICODE /0/
      END BLOCK DATA


      LOGICAL FUNCTION DEFNTNR(CHAR,CODE,NEED)
C
C     RETURNS FALSE IF THE STRING WE ARE READING DEFINITELY IS NOT A NUMBER
C
C     CHAR (C*1) THE NEXT CHARACTER
C     CODE =+1 INITIALISE TO GO FORWARDS
C     CODE =-1 INITIALISE TO GO BACKWARDS
C     CODE =+1 CHECK CURRENT CHARACTER
C     NEED = .TRUE. IF WE NEED AT LEAST ONE MORE CHARACTER
C
      IMPLICIT NONE
C
      CHARACTER*1 CHAR
      INTEGER CODE
      LOGICAL NEED
C
      INTEGER ICODE
      CHARACTER*16 MYNAME
      DATA MYNAME /'DEFNTNR'/
C
      INTEGER JJ,
     &     POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE
      LOGICAL QREAL,OQREAL,DIGIT
      EXTERNAL DIGIT
C
      LOGICAL OUT
C
      COMMON /CCDEFNTNR/ICODE,JJ,POSSG,POSNR,POSDE,POSDD,
     &     NRSGN,NRNRR,NRDES,NRDDE,QREAL,OQREAL,OUT
C
      IF (CODE.EQ.-1.OR.CODE.EQ.1)THEN
         ICODE=CODE
         QREAL=.TRUE.
         OQREAL=.FALSE.
         NEED=.FALSE.
C
         POSSG=0
         POSNR=0
         POSDE=0
         POSDD=0
C
         NRSGN=0
         NRNRR=0
         NRDES=0
         NRDDE=0
C
         JJ=1000
C
         CODE=0
C
      ENDIF
C
      IF (ICODE.EQ.1)THEN
C
C     GOING FORWARDS
C
         JJ=JJ+1
         IF (QREAL) THEN
            IF (CHAR.EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=JJ
               QREAL=(NRDES.LE.1.AND.NRDDE.LT.1)
               NEED=(POSDE.LE.(1+NRSGN))
            ELSEIF (CHAR.EQ.'D' .OR. CHAR.EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=JJ
               QREAL=(NRDDE.LE.1 .AND. POSDD.GT.(NRDES+NRSGN+1))
               NEED=.TRUE.
            ELSEIF (CHAR.EQ.'-'.OR.CHAR.EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=JJ
               QREAL=((NRSGN.LE.1.AND.POSSG.EQ.1) .OR.
     &              (NRSGN.LE.2.AND.NRDDE.EQ.1
     &              .AND.POSDD.EQ.(POSSG-1)))
               NEED=.TRUE.
            ELSE
               QREAL=(DIGIT(CHAR))
               NEED=.FALSE.
               IF (QREAL) POSNR=JJ
            ENDIF
         ENDIF
C
         OUT=QREAL
C
      ELSEIF (ICODE.EQ.-1)THEN
C
C     GOING BACKWARDS
C
         JJ=JJ-1
         IF (QREAL) THEN
            IF (CHAR.EQ.'.') THEN
               NRDES=NRDES+1
               POSDE=JJ
               QREAL=(NRDES.LE.1)
               IF (QREAL) THEN
                  QREAL=(POSDE+1.NE.POSSG)
               ENDIF
               IF (QREAL.AND.NRSGN.GT.0)
     &              QREAL=(.NOT.(POSSG.EQ.JJ+1))
            ELSEIF (CHAR.EQ.'D' .OR.
     &              CHAR.EQ.'E') THEN
               NRDDE=NRDDE+1
               POSDD=JJ
               QREAL=(NRDDE.LE.1.AND.NRDES.LT.1.AND.
     &              ((NRSGN.EQ.1.AND.POSDD+1.EQ.POSSG).OR.
     &              (NRSGN.EQ.0)))
               NEED=.TRUE.
            ELSEIF (CHAR.EQ.'-'.OR.
     &              CHAR.EQ.'+') THEN
               NRSGN=NRSGN+1
               POSSG=JJ
               QREAL=((NRSGN.EQ.1.OR.(NRSGN.EQ.2.AND.NRDDE.EQ.1))
     &              .AND.(POSSG+1.EQ.POSDE.OR.POSSG+1.EQ.POSNR))
               NEED=.FALSE.
            ELSE
               QREAL=((DIGIT(CHAR)).AND.NRSGN.LE.1)
               IF (QREAL) THEN
                  POSNR=JJ
                  QREAL=(POSNR+1.NE.POSSG)
               ENDIF
               NEED=.FALSE.
            ENDIF
         ENDIF
C
         OUT=QREAL
C
      ELSE
         WRITE(*,*) MYNAME,'CODE UNKNOWN',CODE,'.'
      ENDIF
C
      DEFNTNR=OUT
      RETURN
      END
