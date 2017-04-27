      INTEGER FUNCTION LENGTH(STRING,N,GUESS)
C     ***********************************************************************
C     +                                                                     *
C     GIVES THE LENGTH OF THE STRING                                        *
C     +                                                                     *
CI    STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    GUESS (I*4) = FIRST GUESS FOR LENGTH                                  *
C     +                                                                     *
C     VERSION                      : 18/07/95                               *
C     +                                                                     *
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     | FTT, OV      | 22/06/00 | removed save + 0:0-bug
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
      IMPLICIT NONE
C
      INTEGER N,LL,LENS,GUESS
      CHARACTER*(*) STRING
      CHARACTER*1 C1
      LOGICAL BL,FORW,BDONE
C
      C1=CHAR(0)
      BDONE=.FALSE.
      LENS=0
C
      LL=MIN(MAX(1,GUESS),N)
      FORW=(STRING(LL:LL).NE.C1)
      DO WHILE (.NOT.BDONE)
C
         BL=(STRING(LL:LL).EQ.C1)
         IF (.NOT.BL) LENS=LL
C
         BDONE=((BL.AND.FORW).OR.(.NOT.BL.AND..NOT.FORW))
C
         IF (.NOT. BDONE) THEN
            IF (FORW) THEN
               LL=LL+1
            ELSE
               LL=LL-1
            ENDIF
            BDONE=(LL.LE.0.OR.LL.GT.N)
         END IF
      ENDDO
C
      LENGTH=LENS
C
      RETURN
      END
