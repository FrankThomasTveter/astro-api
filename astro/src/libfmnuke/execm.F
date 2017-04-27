      BLOCK DATA BDEXECM
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER NROP,LENS,LENB
      PARAMETER(NROP=4)
      CHARACTER*2 COP(NROP)
      INTEGER IOP(NROP)
      COMMON /CCEXECM/COP,IOP,FIRST,LENS,LENB
      DATA FIRST /.TRUE./,LENS/1/,LENB /1/
      END BLOCK DATA


      SUBROUTINE EXECM(STRING,N,CHANGED,POS)
C     ***********************************************************************
C     +                                                                     *
C     EXECUTES (EVALUATES) A SUBSTRING OF STRING                            *
C     +                                                                     *
CIO   STRING (C*N) = STRING TO BE EXAMINED                                  *
CI    N (I*4) = LENGTH OF STRING                                            *
CI    POS(2) (I*4) = START AND END POSITION OF SUBSTRING TO EVALUATE        *
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
      INTEGER N,POS(2)
      LOGICAL CHANGED
      CHARACTER*(*) STRING
      CHARACTER*1000 BUFF1000
      CHARACTER*250 BUFF1,BUFF2,BUFF3
C
      INTEGER MAXO,OPOS(2,2)
      LOGICAL LCHNG
      PARAMETER(MAXO=250)
      INTEGER LENGTH,LENB1,LENB2,LENB3,
     &     LENX,LENB,LENS,
     &     DELL,IRC,CPOS,
     &     BPOS(MAXO),MAXB,
     &     CODE,START,END,II,JJ,KK,LEVEL
      EXTERNAL LENGTH
      LOGICAL FIRST,CLEAN,LAST,DONE
      INTEGER NROP
      PARAMETER(NROP=4)
      CHARACTER*2 COP(NROP)
      INTEGER IOP(NROP)
      COMMON /CCEXECM/COP,IOP,FIRST,LENS,LENB
C
      IF (FIRST)THEN
C
C     DEFINITION OF OPERATION STRING
C
         COP(1)='^'
         COP(2)='*'
         COP(3)='/'
         COP(4)='+-'
C
C     LENGTH OF OPERATION STRING
C
         DO II=1,NROP
            CALL CHOP0(COP(II),2)
            IOP(II)=LENGTH(COP(II),2,1)
         ENDDO
C
         FIRST=.FALSE.
      ENDIF
C
      LENS=LENGTH(STRING,N,LENS)
      LENB=LENGTH(BUFF1000,1000,LENB)
C
      LENX=MAX(LENS,LENB)
      LENX=MIN(LENX,1000)
C
      CALL CEQUAL(BUFF1000,LENX,STRING,POS(1)+1,POS(2)-1)
C
C     FIND ALL  SPECIAL FUNCTIONS
C
      DO KK=1,NROP
C
         CLEAN=.TRUE.
         LAST=.FALSE.
         DO WHILE (.NOT.LAST)
C
C     FIND ALL '*' '/' '^' '-' '+'
C
            LENS=LENGTH(BUFF1000,N,LENS)
            CALL CHOP0(BUFF1000,LENS)
            LENS=LENGTH(BUFF1000,N,LENS)
C
            LEVEL=0
            MAXB=0
            DO II=1,LENS
               IF (BUFF1000(II:II).EQ.'(') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.')') LEVEL=LEVEL-1
               IF (BUFF1000(II:II).EQ.'[') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.']') LEVEL=LEVEL-1
               IF (BUFF1000(II:II).EQ.'{') LEVEL=LEVEL+1
               IF (BUFF1000(II:II).EQ.'}') LEVEL=LEVEL-1
               IF (LEVEL.EQ.0) THEN
                  DO JJ=1,IOP(KK)
                     IF (BUFF1000(II:II).EQ.COP(KK)(JJ:JJ)) THEN
                        IF (MAXB.LT.MAXO) MAXB=MAXB+1
                        BPOS(MAXB)=II
                     ENDIF
                  ENDDO
               ENDIF
            ENDDO
C
C     EXECUTE OPERATIONS
C
            DO II=1,MAXB
               CPOS=BPOS(II)+1
               CODE=1
               CALL WOBJECT(BUFF1000,N,CPOS,OPOS(1,2),CODE)
               END=CPOS
C
               IF (CODE.EQ.0) THEN
                  CPOS=BPOS(II)-1
                  CODE=-1
                  CALL WOBJECT(BUFF1000,N,CPOS,OPOS(1,1),CODE)
                  START=CPOS
                  IF (CODE.EQ.0) THEN
                     CALL CEQUAL (BUFF1,250,BUFF1000,
     &                    OPOS(1,1),OPOS(2,1))
                     CALL CEQUAL (BUFF2,250,BUFF1000,
     &                    OPOS(1,2),OPOS(2,2))
C
                     LENB1=LENGTH(BUFF1,250,LENB1)
                     LENB2=LENGTH(BUFF2,250,LENB2)
                     CALL CHOP0(BUFF1,LENB1)
                     CALL CHOP0(BUFF2,LENB1)
                     LENB1=LENGTH(BUFF1,250,LENB1)
                     LENB2=LENGTH(BUFF2,250,LENB2)
C
                     IRC=0
                     LCHNG=.FALSE.
                     CALL SLAVEM(BUFF1,BUFF2,BUFF3,KK,CLEAN,LCHNG,IRC)
                     IF (LCHNG) CHANGED=.TRUE.
                     DONE=(IRC.EQ.0)
                     IRC=0
C
                     IF (LCHNG) THEN
                        CALL CHOP0(BUFF3,250)
                        LENB3=LENGTH(BUFF3,250,LENB3)
                        DELL=END-START+1
                        CALL SUBSTR(BUFF1000,N,BUFF3,250,START,DELL)
C
C     UPDATE POSITION OF REMAINING OPERATORS
                        LENB=LENB3-DELL
                        JJ=II+1
                        DO WHILE(JJ.LE.MAXB)
C     REMOVE JOB IF IT WAS INSIDE OTHER JOB
                           IF (BPOS(JJ).LE.START .OR.
     &                          BPOS(JJ).GT.END) THEN
                              BPOS(JJ)=BPOS(JJ)+LENB
                           ELSE
                              BPOS(JJ)=BPOS(MAXB)
                              MAXB=MAXB-1
                              JJ=JJ-1
                           ENDIF
C     REMOVE JOB IF IT FALLS OUTSIDE BUFFER
                           IF (BPOS(JJ).GT.N) THEN
                              BPOS(JJ)=BPOS(MAXB)
                              MAXB=MAXB-1
                              JJ=JJ-1
                           ENDIF
                           JJ=JJ+1
                        ENDDO
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            LAST=.NOT.CLEAN
C     ONLY RUN THROUGH '+-' ONCE (SO THAT '+2' DOES NOT TURN INTO '{+2}')
            IF (KK.EQ.4)LAST=.TRUE.
            CLEAN=.FALSE.
         ENDDO
      ENDDO
C
      CALL SUBSTR(STRING,N,BUFF1000,N,POS(1)+1,POS(2)-POS(1)-1)
      LENB=LENGTH(BUFF1000,N,LENB)
      POS(2)=POS(1)+LENB+1
C
      RETURN
      END
