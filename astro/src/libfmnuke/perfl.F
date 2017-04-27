      BLOCK DATA BDPERFL
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCPERFL/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE PERFL(STRING,N,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     PERFORMS LOGICAL OPERATIONS ON STRING (AND, OR, NOT ETC.)
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
      INTEGER N,IRC
      CHARACTER*(*) STRING
      CHARACTER*1 DEL(2)
      LOGICAL CHANGED
C
      INTEGER LEV,POS(2),LENS,LENGTH
      EXTERNAL LENGTH
      LOGICAL DONE
      COMMON /CCPERFL/ LENS
C
      DONE=.FALSE.
      DO WHILE (.NOT. DONE)
C
         LEV=-1
         DEL(1)='('
         DEL(2)=')'
         LENS=LENGTH(STRING,N,LENS)
         POS(1)=1
         POS(2)=LENS
         CALL ITEM(STRING,LENS,DEL,LEV,POS,IRC)
         IF (IRC.EQ.0) THEN
            IF (LEV.EQ.-1) THEN
C     NO PARANTHESIS
               DONE=.TRUE.
               POS(1)=0
               POS(2)=N+1
               CALL EXECL(STRING,N,CHANGED,POS)
               if (changed) call cleansome(string,n,pos)
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECL(STRING,N,CHANGED,POS)
               if (changed) call cleansome(string,n,pos)
            ENDIF
         ELSE
            DONE=.TRUE.
         ENDIF
      ENDDO
C
      IRC=0
C
      RETURN
      END
