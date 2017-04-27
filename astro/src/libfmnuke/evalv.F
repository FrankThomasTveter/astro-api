      SUBROUTINE EVALV(STRING,N,NRVAR,NAMVAR,VALVAR,VARLEN,CHANGED,IRC)
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
      INTEGER START,END,CPOS,POS2(2),CODE
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'EVALV'/
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
            START=II
            CPOS=II+1
            CODE=+1             ! search forwards
            CALL OBJECT(STRING,N,CPOS,POS2,CODE)
            CALL CEQUAL(BUFF1,250,STRING,POS2(1),POS2(2))
            END=CPOS
            CALL CHOP0(BUFF1,250)
            LENB=LENGTH(BUFF1,250,LENB)
C     CHECK FOR HELP
            IF (LENB.EQ.1.AND.BUFF1(1:1).EQ.'?') THEN
               IF (NRVAR.GT.0) THEN
                  WRITE(*,*) MYNAME,
     &                 '--------LIST OF CURRENT VARIABLES-------'
                  DO KK=1,NRVAR
                     VALLEN=LENGTH(VALVAR(KK),250,VALLEN)
                     WRITE(*,*) '$(',NAMVAR(KK)(1:VARLEN(KK))//') = '//
     &                    VALVAR(KK)(1:VALLEN)
                  ENDDO
                  WRITE(*,*) MYNAME,
     &                 '----------------------------------------'
               ELSE
                  WRITE(*,*) MYNAME,
     &                 '----------------------------------------'
                  WRITE(*,*) 'No variables have been defined.'
                  WRITE(*,*) MYNAME,
     &                 '----------------------------------------'
               ENDIF
               CALL RESET(BUFF,250)
               WRITE(BUFF,*) NRVAR
               CALL CHOP0(BUFF,250)
               LENB=LENGTH(BUFF,250,LENB)
               CALL SUBSTR(STRING,N,BUFF,LENB,START,END-START+1)
               CHANGED=.TRUE.
               JJ=NRVAR
C
            ENDIF
C     CHECK IF VARIABLE IS DEFINED
            JJ=0
            DO WHILE (JJ.LT.NRVAR.AND..NOT.CHANGED)
               JJ=JJ+1
               KK=1
               DONE=(VARLEN(JJ).EQ.0.OR.VARLEN(JJ).NE.LENB)
               EQUAL=.NOT.DONE
               DO WHILE (.NOT.DONE)
                  IF (EQUAL.AND.KK.LE.VARLEN(JJ)) THEN
                     EQUAL=(BUFF1(KK:KK).EQ.
     &                    NAMVAR(JJ)(KK:KK))
                     KK=KK+1
                  ELSE
                     DONE=.TRUE.
                  ENDIF
               ENDDO
               IF (EQUAL) THEN
C     REPLACE VARIABLE WITH EXPRESSION
                  VALLEN=LENGTH(VALVAR(JJ),250,VALLEN)
                  CALL RESET(BUFF,250)
                  BUFF=VALVAR(JJ)(1:VALLEN)
                  CALL CHOP0(BUFF,250)
                  LENB=LENGTH(BUFF,250,LENB)
                  if (start.gt.1.and.end.lt.n) then
                     if (string(start-1:start-1).eq.'{'
     &                    .and.string(end+1:end+1).eq.'}') then
                        start=start-1
                        end=end+1
                     end if
                  end if
                  CALL SUBSTR(STRING,N,BUFF,LENB,START,END-START+1)
C     IN CASE THE EXPRESSION STARTS WITH A NEW VARIABLE
                  II=II-1
C
                  CHANGED=.TRUE.

                  JJ=NRVAR
C
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C
C
      RETURN
      END
