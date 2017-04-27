      BLOCK DATA BDEXECF
      IMPLICIT NONE
      INTEGER NRFUNK
      PARAMETER (NRFUNK=21)
      CHARACTER*20 FUNK20(NRFUNK),FUNH20(NRFUNK)
      INTEGER FUNKLEN(NRFUNK)
      REAL PI,E,XMU,RE,RAD,DEG
      LOGICAL FIRST
      COMMON /CCEXECF/FUNK20,FUNH20,FUNKLEN,PI,E,XMU,RE,RAD,DEG,FIRST
      DATA FIRST /.TRUE./
      END BLOCK DATA


      SUBROUTINE EXECF(STRING,N,CHANGED,POS)
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
      INTEGER N,IRC,POS(2)
      CHARACTER*(*) STRING
      LOGICAL CHANGED
C
      CHARACTER*250 BUFF,BUFF1
C      CHARACTER*24 fdate
C      EXTERNAL fdate
C
      INTEGER II,JJ,KK,CNT,FTUNIT,
     &     LENB,LENH,LENGTH
      EXTERNAL LENGTH,FTUNIT
      LOGICAL EQUAL,DONE,QQ(2),ISINT,ISREAL,LCHNG
      EXTERNAL ISINT,ISREAL
      INTEGER START,END,CPOS,UNITT,
     &     IY,IM,ID,IH,IMM
C
      INTEGER NRFUNK
      PARAMETER (NRFUNK=21)
      CHARACTER*20 FUNK20(NRFUNK),FUNH20(NRFUNK)
      INTEGER FUNKLEN(NRFUNK)
CINM  REAL PI,E,XMU,RE,RAD,DEG,RRR(2),RR,SEC,TIME
      REAL PI,E,XMU,RE,RAD,DEG,RR,SEC,TIME
      REAL*8 RRR(2)
      LOGICAL FIRST
      COMMON /CCEXECF/FUNK20,FUNH20,FUNKLEN,PI,E,XMU,RE,RAD,DEG,FIRST
C
      CHARACTER*16 MYNAME
      DATA MYNAME /'EXECF'/
C
      IF (FIRST) THEN
C
C     DEFINE AUXILIARY FUNCTIONS
C
         FUNK20(1)='%sin'
         FUNH20(1)='(<number>)'
         FUNK20(2)='%cos'
         FUNH20(2)='(<number>)'
         FUNK20(3)='%tan'
         FUNH20(3)='(<number>)'
         FUNK20(4)='%arcsin'
         FUNH20(4)='(<number>)'
         FUNK20(5)='%arccos'
         FUNH20(5)='(<number>)'
         FUNK20(6)='%arctan'
         FUNH20(6)='(<number>)'
         FUNK20(7)='%log'
         FUNH20(7)='(<number>)'
         FUNK20(8)='%exp'
         FUNH20(8)='(<number>)'
         FUNK20(9)='%inquire'
         FUNH20(9)='(<file>)'
         FUNK20(10)='%pi'
         CALL RESET(FUNH20(10),20)
         FUNK20(11)='%e'
         CALL RESET(FUNH20(11),20)
         FUNK20(12)='%mu'
         CALL RESET(FUNH20(12),20)
         FUNK20(13)='%re'
         CALL RESET(FUNH20(13),20)
         FUNK20(14)='%rad'
         CALL RESET(FUNH20(14),20)
         FUNK20(15)='%deg'
         CALL RESET(FUNH20(15),20)
         FUNK20(16)='%dj2000'
         FUNH20(16)='(<date>)'
         FUNK20(17)='%jd2000'
         FUNH20(17)='(<MJD2000>)'
         FUNK20(18)='%?'
         CALL RESET(FUNH20(18),20)
         FUNK20(19)='%date'
         CALL RESET(FUNH20(19),20)
         FUNK20(20)='%int'
         FUNH20(20)='(<number>)'
         FUNK20(21)='%sqrt'
         FUNH20(21)='(<number>)'
C
         PI=2.0*ATAN2(1.0,0.0)
         E=2.718281828459045
         XMU=398600.440
         RE=6378.136
         RAD=PI/180.0
         DEG=180.0/PI
C
         DO II=1,NRFUNK
            CALL CHOP0(FUNK20(II),20)
            FUNKLEN(II)=LENGTH(FUNK20(II),20,4)
         ENDDO
C
         FIRST=.FALSE.
      ENDIF
C
      CNT=0
      IRC=0
      CHANGED=.FALSE.
C
C
C     SEARCH FOR A '%'-SIGN IN STRING
C
      II=POS(1)+1
      DO WHILE(II.LT.POS(2))
         IF (STRING(II:II).EQ.'%') THEN
C     FIND FOLLOWING OBJECT
            QQ(1)=.FALSE.
            QQ(2)=.FALSE.
            START=II
            CPOS=II+1
            CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RR)
            END=CPOS
            CALL CEQUAL(BUFF1,250,STRING,START,END)
            CALL CHOP0(BUFF1,250)
            LENB=LENGTH(BUFF1,250,LENB)
C     CHECK IF FUNCTION IS DEFINED
            JJ=0
            DO WHILE (JJ.LT.NRFUNK.AND..NOT.CHANGED)
               JJ=JJ+1
               KK=1
               DONE=(FUNKLEN(JJ).EQ.0.OR.FUNKLEN(JJ).NE.LENB)
               EQUAL=.NOT.DONE
               DO WHILE (.NOT.DONE)
                  IF (EQUAL.AND.KK.LE.FUNKLEN(JJ)) THEN
                     EQUAL=(BUFF1(KK:KK).EQ.
     &                    FUNK20(JJ)(KK:KK))
                     KK=KK+1
                  ELSE
                     DONE=.TRUE.
                  ENDIF
               ENDDO
               IF (EQUAL) THEN
C
                  CALL RESET(BUFF,250)
                  LCHNG=.FALSE.
                  IF (JJ.EQ.1) THEN
C     %sin
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=SIN(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.2) THEN
C     %cos
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=COS(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.3) THEN
C     %tan
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=TAN(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.4) THEN
C     %arcsin
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=ASIN(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.5) THEN
C     %arccos
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=ACOS(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.6) THEN
C     %arctan
                     QQ(1)=.TRUE.
                     QQ(2)=.TRUE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1).AND.QQ(2)) THEN
                        RR=ATAN2(RRR(1),RRR(2))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ELSEIF (QQ(1)) THEN
                        RR=ATAN(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.7) THEN
C     %log
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=LOG(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.8) THEN
C     %exp
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=EXP(RRR(1))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.9) THEN
C     %inquire
                     QQ(1)=.FALSE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     LENB=LENGTH(BUFF1,250,LENB)
                     UNITT=FTUNIT(IRC)
                     IF (IRC.NE.0) THEN
                        WRITE(*,*) MYNAME,'Error return from DEBUG.',
     &                       IRC
                        RETURN
                     ENDIF
C     DON'T DO ANYTHING IF THE FILE DOES NOT EXIST...
C     ...(IT MAY NEED YET SOME VARIABLE REPLACEMENTS)
                     OPEN(UNIT=UNITT,FILE=BUFF1,
     &                    STATUS='OLD',ERR=999)
                     CLOSE (UNIT=UNITT)
                     CALL RESET(BUFF,250)
                     BUFF='T'
                     LCHNG=.TRUE.
                     JJ=NRFUNK
 999                 CONTINUE
                  ELSEIF (JJ.EQ.10) THEN
C     %pi
                     WRITE(BUFF,*) PI
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.11) THEN
C     %e
                     WRITE(BUFF,*) E
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.12) THEN
C     %mu
                     WRITE(BUFF,*) XMU
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.13) THEN
C     %re
                     WRITE(BUFF,*) RE
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.14) THEN
C     %rad
                     WRITE(BUFF,*) RAD
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.15) THEN
C     %deg
                     WRITE(BUFF,*) DEG
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.16) THEN
C     %dj2000
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     LENB=LENGTH(BUFF1,250,LENB)
                     IF (QQ(1)) THEN
                        CALL DJ2000(RRR(1),IY,IM,ID,IH,IMM,SEC)
                        WRITE(BUFF,'(I4,1X,4(I2,1X),F6.3)')
     &                       IY,IM,ID,IH,IMM,SEC
                        LCHNG=.TRUE.
                     ENDIF
                  ELSEIF (JJ.EQ.17) THEN
C     %jd2000
                     QQ(1)=.FALSE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     LENB=LENGTH(BUFF1,250,LENB)
                     CALL NUKECP(BUFF1,'/',' ',LENB)
                     CALL NUKECP(BUFF1,':',' ',LENB)
                     READ(BUFF1(1:LENB),*,END=201,ERR=201)
     &                    IY,IM,ID,IH,IMM,SEC
                     CALL JD2000(TIME,IY,IM,ID,IH,IMM,SEC)
                     WRITE(BUFF,*) TIME
                     LCHNG=.TRUE.
 201                 CONTINUE
                  ELSEIF (JJ.EQ.18) THEN
C     %?
                     WRITE(*,*) MYNAME,
     &                    '--------LIST OF POSSIBLE FUNCTIONS------'
                     DO II=1,NRFUNK
                        LENB=LENGTH(FUNK20(II),20,3)
                        LENH=LENGTH(FUNH20(II),20,3)
                        WRITE(*,*) FUNK20(II)(1:LENB)//' '//
     &                       FUNH20(II)(1:LENH)
                     ENDDO
                     WRITE(*,*) MYNAME,
     &                    '----------------------------------------'
                     CALL RESET(BUFF,250)
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.19) THEN
C     %date
                     CALL RESET(BUFF,250)
C                     BUFF(1:24)=fdate()
                     BUFF(1:24)='not availible'
                     LCHNG=.TRUE.
                  ELSEIF (JJ.EQ.20) THEN
C     %int
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        WRITE(BUFF,*) NINT(RRR(1))
                        LCHNG=.TRUE.
                     END IF
                  ELSEIF (JJ.EQ.21) THEN
C     %sqrt
                     QQ(1)=.TRUE.
                     QQ(2)=.FALSE.
                     CPOS=END+1
                     CALL ARGUMENT(STRING,N,CPOS,BUFF1,QQ,RRR)
                     END=CPOS
                     IF (QQ(1)) THEN
                        RR=SQRT(max(0.0D0,RRR(1)))
                        WRITE(BUFF,*) RR
                        LCHNG=.TRUE.
                     END IF
                  ENDIF
                  IF (LCHNG) THEN
                     CALL CHOP0(BUFF,250)
                     LENB=LENGTH(BUFF,250,LENB)
                     CALL SUBSTR(STRING,N,BUFF,LENB,
     &                    START,END-START+1)
                     CHANGED=.TRUE.
                     JJ=NRFUNK
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
         II=II+1
      ENDDO
C
      RETURN
      END
