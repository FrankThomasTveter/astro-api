      SUBROUTINE ITEM(STRING,N,DEL,LEV,POS,IRC)
C     ***********************************************************************
C     +                                                                     *
C     RETURNS THE POSITION OF THE SUBSTRING (AT LEVEL LEV), WITH THE        *
C     DELIMITERS DEL(2). IF THE SPECIFIED LEVEL IS NOT FOUND, THE ROUTINE   *
C     RETURNS THE LOWEST LEVEL IN THE STRING, E.G. STRING="1+((2+(3)))"     *
C     CALLED WITH DEL(1)='(',DEL(2)=')' AND LEV=-1 GIVES POS(1)=7,POS(2)=9  *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    DEL(2) (C*1) = THE START AND END DELIMITER TO SEARCH FOR              *
CI    LEV = LEVEL TO SEARCH FOR, E.G. '(a)' AND '((a))' ARE RESP. LEV=1,2   *
CIO   POS(2) (I*4) = POSITION TO SEARCH WITHIN/LOCATION OF SUBSTRING        *
CO    IRC (I*4) = ERROR RETURN CODE (IRC=0 ALL OK)                          *
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
C     STRING WHOLE STRING IN WHICH TO SEARCH
C     N= LENGTH OF STRING
C     DEL=DELIMITERS IN WHICH TO SEARCH FOR
C     POS=LIMIT OF STRING TO SEARCH FOR, ALSO LOC OF DEL
C
C     IDENTIFIES AN ITEM STARTING WITH DEL(1) AND ENDING WITH DEL(2)
C
      IMPLICIT NONE
C
      INTEGER N,LEV
      CHARACTER*1 DEL(2)
      CHARACTER*(*) STRING
      CHARACTER(len=n) STRING2
      INTEGER II,IRC,POS(2),MPOS(2),LEVEL,MAXLEV,START,STOP
      LOGICAL FOUND(2),MFOUND(2)
C
C     FIND START
C
      STRING2=STRING
      START=POS(1)
      STOP=POS(2)
C
      LEVEL=0
      MAXLEV=-1
      MPOS(1)=START
      MPOS(2)=STOP
      POS(1)=0
      POS(2)=0
      FOUND(1)=.FALSE.
      FOUND(2)=.FALSE.
C
      II=START
      DO WHILE (II.LE.STOP)
         IF (STRING2(II:II).EQ.DEL(1)) THEN
            IF (.NOT. FOUND(1) .AND. LEVEL.EQ.LEV) THEN
               POS(1)=II
               FOUND(1)=.TRUE.
            ENDIF
            LEVEL=LEVEL+1
            IF (LEVEL.GT.MAXLEV) THEN
               MAXLEV=LEVEL
               MPOS(1)=II
               MFOUND(1)=.TRUE.
               MFOUND(2)=.FALSE.
            ENDIF
         ELSEIF(STRING2(II:II).EQ.DEL(2)) THEN
            IF (.NOT.MFOUND(2).AND.LEVEL.EQ.MAXLEV) THEN
               MPOS(2)=II
               MFOUND(2)=.TRUE.
            ENDIF
            LEVEL=LEVEL-1
            IF (.NOT. FOUND(2) .AND. LEVEL.EQ.LEV) THEN
               POS(2)=II
               FOUND(2)=.TRUE.
            ENDIF
         ENDIF
         IF (LEVEL.LT.0) THEN
            IRC=98
         ENDIF
         II=II+1
      ENDDO
C
      IF (LEVEL.NE.0) IRC=99
C
      IF (.NOT. FOUND(1) .OR. .NOT. FOUND(2)) THEN
         LEV=MAXLEV
         POS(1)=MPOS(1)
         POS(2)=MPOS(2)
      ENDIF
C
      string=string2
      RETURN
      END
