      BLOCK DATA BDQVAR
      IMPLICIT NONE
      INTEGER LENS,LENB
      COMMON /CCQVAR/ LENS,LENB
      DATA LENS /1/,LENB/1/
      END BLOCK DATA


      SUBROUTINE QVAR (STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,IRC)
C     ***********************************************************************
C     +
C     + checks if string contains an unevaluated variable
C     +
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
      INTEGER NRVAR
      CHARACTER*250 NAMVAR(NRVAR),VALVAR(NRVAR)
      INTEGER VARLEN(NRVAR),IRC
C
      INTEGER II,JJ,KK,CNT,VALLEN,
     &     LENS,LENB,LENGTH
      EXTERNAL LENGTH
      LOGICAL CHANGED,EQUAL,DONE,ATOM,QQ(2)
      EXTERNAL ATOM
      REAL RR(2)
      CHARACTER*250 BUFF,BUFF1
      INTEGER START,END,CPOS
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'QVAR'/
C
      COMMON /CCQVAR/ LENS,LENB
C
      CNT=0
      IRC=0
      CHANGED=.FALSE.
C
C     SEARCH FOR A '$'-SIGN IN STRING
C
      II=0
      LENS=LENGTH(STRING,N,LENS)
      DO WHILE(II.LT.LENS)
         II=II+1
         IF (STRING(II:II).EQ.'$') THEN
C     FIND FOLLOWING OBJECT
            QQ(1)=.FALSE.
            QQ(2)=.FALSE.
            START=II
            CPOS=II+1
            CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RR)
            END=CPOS
            CALL CHOP0(BUFF1,250)
            LENB=LENGTH(BUFF1,250,LENB)
            IF (LENB.NE.0) THEN
               WRITE(*,*) MYNAME,
     &              'Attempt to use undefined variable:'//BUFF1(1:LENB)
               IRC=273
               RETURN
            END IF
         ENDIF
      ENDDO
C
C
      RETURN
      END
