      BLOCK DATA BDEVALF
      IMPLICIT NONE
      INTEGER LENS
      COMMON /CCEVALF/ LENS
      DATA LENS /1/
      END BLOCK DATA


      SUBROUTINE EVALF(STRING,N,CHANGED,IRC)
C     ***********************************************************************
C     +                                                                     *
C     EVALUATES AN EXPRESSION                                               *
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
      LOGICAL CHANGED
C
      CHARACTER*1 DEL(2)
      INTEGER LEV,POS(2),LENS,LENGTH,II,DELL
      EXTERNAL LENGTH
      LOGICAL DONE
      CHARACTER*1 BUFF
C
      COMMON /CCEVALF/ LENS
C
C-----PERFORM ALL MATHEMATICAL OPERATIONS
C
      CHANGED=.FALSE.
      DONE=.FALSE.
C
      DO WHILE (.NOT. DONE)
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
               CALL EXECF(STRING,N,CHANGED,POS)
            ELSE
C     EXECUTE CALC, AND REPLACE PARANTHESIS WITH []
               STRING(POS(1):POS(1))='['
               STRING(POS(2):POS(2))=']'
               CALL EXECF(STRING,N,CHANGED,POS)
            ENDIF
         ELSE
            DONE=.TRUE.
         ENDIF
      ENDDO
C
C-----CLEAN UP
C
      CALL CHOP0(STRING,N)
C
      LENS=LENGTH(STRING,N,1)
      DELL=1
      II=1
      DO WHILE(II.LE.LENS)
C     REPLACE ALL [] WITH ()
         IF (STRING(II:II).EQ.'[') THEN
            BUFF='('
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ELSEIF (STRING(II:II).EQ.']') THEN
            BUFF=')'
            CALL SUBSTR(STRING,N,BUFF,1,II,DELL)
         ENDIF
         II=II+1
      ENDDO
C
      LENS=LENGTH(STRING,N,LENS)
      CALL CHOP0(STRING,LENS)
C
      IRC=0
C
      RETURN
      END
