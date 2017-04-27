      SUBROUTINE WOBJECT(STRING,N,START,POS,CODE)
C
C     RETURNS THE 'WHOLE' OBJECT (INCLUDED BRACKETS)
C
      IMPLICIT NONE
C
      INTEGER N
      CHARACTER*(*) STRING
      INTEGER START,POS(2),CODE
C
      INTEGER ICODE
      CHARACTER*16 MYNAME
      DATA MYNAME /'OBJECT'/
C
      ICODE=CODE
      CALL OBJECT(STRING,N,START,POS,ICODE)
C
      IF (CODE.EQ.1) THEN
         IF (POS(1).GT.1) THEN
            IF (STRING(POS(1)-1:POS(1)-1).EQ.'('
     &           .OR.STRING(POS(1)-1:POS(1)-1)
     &           .EQ.'[') THEN
               POS(1)=POS(1)-1
               IF (POS(1).GT.1) THEN
                  IF(STRING(POS(1)-1:POS(1)-1).EQ.'-'
     &                 .OR.STRING(POS(1)-1:POS(1)-1)
     &                 .EQ.'+') THEN
                     POS(1)=POS(1)-1
                  ENDIF
               ENDIF
            ELSEIF((STRING(POS(1)-1:POS(1)-1).EQ.'-'
     &              .OR.STRING(POS(1)-1:POS(1)-1)
     &              .EQ.'+').AND..NOT.
     &              (STRING(POS(1):POS(1)).EQ.'-'
     &              .OR.STRING(POS(1):POS(1))
     &              .EQ.'+')) THEN
               POS(1)=POS(1)-1
            ENDIF
         ENDIF
         POS(2)=START
      ELSEIF (CODE.EQ.-1) THEN
         POS(1)=START
         IF (POS(2).LT.N) THEN
            IF (STRING(POS(2)+1:POS(2)+1).EQ.')'
     &           .OR.STRING(POS(2)+1:POS(2)+1)
     &           .EQ.']') POS(2)=POS(2)+1
         ENDIF
      ENDIF
C
      CODE=ICODE
C
      RETURN
      END
