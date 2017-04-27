      SUBROUTINE CHOP0(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     REMOVES SURPLUS BLANKS FROM A STRING                                  *
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
      CHARACTER*1 C1
      INTEGER II,JJ
      LOGICAL LASTBL
C
      C1=CHAR(0)
      LASTBL=.TRUE.
      JJ=0
      II=0
      DO WHILE(II.LT.N)
         II=II+1
         IF (LASTBL .AND. STRING(II:II).EQ.C1) THEN
            II=N
         ELSE
            IF (STRING(II:II).NE.' ' .AND.
     &           STRING(II:II).NE.C1) THEN
C     ADDING CHARACTER TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=STRING(II:II)
               LASTBL=.FALSE.
            ELSEIF (.NOT.LASTBL) THEN
C     ADDING (*ONE*) BLANK TO BUFF
               JJ=JJ+1
               STRING(JJ:JJ)=' '
               LASTBL=.TRUE.
            ENDIF
         ENDIF
      ENDDO
C
      IF (.NOT.LASTBL.OR.JJ.EQ.0) JJ=JJ+1
C
      DO II=JJ,N
         STRING(II:II)=C1
      ENDDO
      RETURN
      END
