      CHARACTER*250 FUNCTION NUKETAIL(STRING,N)
C     ***********************************************************************
C     +                                                                     *
C     FUNCTION RETURNS THE LAST ITEM IN STRING,                             *
C     AND REMOVES IT FROM STRING                                            *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CO    NUKETAIL (C*250) = TAIL OF STRING                                     *
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
C
      CHARACTER*250 OUT
      INTEGER CRR,II,JJ
      LOGICAL BLNK
      CHARACTER*16 MYNAME
      DATA MYNAME /'NUKETAIL'/
C
      C1=CHAR(0)
C
      BLNK=.FALSE.
C
      CRR=N+1
C
      DO WHILE (.NOT.BLNK .AND. CRR.GT.1)
         CRR = CRR-1
         BLNK= (STRING(CRR:CRR) .EQ. ' ')
      ENDDO
C
      CALL RESET(OUT,250)
      IF (BLNK) THEN
C     SEVERAL ITEMS IN STRING
         JJ=MIN(250,N-CRR)
         DO II=1,JJ
            OUT(II:II)=STRING(II+CRR:II+CRR)
         ENDDO
         DO II=CRR,N
            STRING(II:II)=C1
         ENDDO
      ELSE
C     JUST ONE ITEM IN STRING
         JJ=MIN(250,N)
         DO II=1,JJ
            OUT(II:II)=STRING(II:II)
         ENDDO
         DO II=1,N
            STRING(II:II)=C1
         ENDDO
      ENDIF
C
      CALL CHOP0(OUT,250)
      CALL CHOP0(STRING,N)
C
      NUKETAIL=OUT
      RETURN
      END
