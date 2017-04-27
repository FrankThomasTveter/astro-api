      SUBROUTINE DEBUG(NAME,BDEB,ACTIVE)
C
C     ***********************************************************************
C     WRITTEN/MODIFIED BY:                                                  *
C     --------------------------------------------------------------------- *
C     |    NAME      |   DATE   |                 REASON                  | *
C     --------------------------------------------------------------------- *
C     | F. TVETER    | 18/07/95 | NEW                                     | *
C     |              |          |                                         | *
C     --------------------------------------------------------------------- *
C     ***********************************************************************
C
C     THE ROUTINE RETURNS BDEB=.TRUE. IF NAME
C     OCCURS IN DEBUG-FILE....
C     ...This routine was inspired by Lars Behrend's RUNPAR debug system...
C
      IMPLICIT NONE
C
      CHARACTER*8 NAME
      LOGICAL BDEB
C
      LOGICAL FIRST,FOPEN,OK,ENTER,DONE,ACTIVE
      DATA FIRST /.TRUE./
      DATA FOPEN /.FALSE./
      DATA ENTER /.FALSE./
      DATA DONE /.FALSE./
      CHARACTER*8 MYNAME
      DATA MYNAME /'DEBUG'/
C
      INTEGER NRNAME,MXNAME,LINE,II,IRC
      PARAMETER (MXNAME=250)
      CHARACTER*8 NAMES(MXNAME)
      DATA NRNAME /0/
      CHARACTER*80 PATHF,IPATHF
      DATA PATHF /'debug.txt'/
C
      INTEGER FTUNIT,UNITI
      EXTERNAL FTUNIT
C
      INTEGER NRHDR,NRDAT
      PARAMETER (NRHDR=250,NRDAT=1)
      CHARACTER*250 HDR250(NRHDR),DAT250
      INTEGER INTOUT
      LOGICAL ENDOFF
C
      INTEGER LENGTH,LENH,LEND,LENS,LENP,KODE
      EXTERNAL LENGTH
C
      LOGICAL ALL(2),SPEC
      DATA ALL /.FALSE.,.FALSE./
      DATA SPEC /.FALSE./
C
C     ONLY EXECUTE THIS ROUTINE IF IT HAS NOT BEEN ENTERED...
C     ...OR IF IT HAS BEEN RUN THROUGH COMPLETELY...
C     ...(AVVOID LOOPS SINCE THE ROUTINE CALLS OTHER ROUTINES ...
C     ...THAT CALL THIS ROUTINE)...
C
      BDEB=.FALSE.
      return
C
      ACTIVE=DONE
      IF (ENTER.AND..NOT.DONE) RETURN
C
      ENTER=.TRUE.
C
      IF (FIRST) THEN
C
C     OPEN FILE AND READ NAMES
C
         UNITI=FTUNIT(IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from FTUNIT.',IRC
            RETURN
         ENDIF
         OPEN(UNIT=UNITI,FILE=PATHF,
     &        ACCESS='SEQUENTIAL',FORM='FORMATTED',
     &        STATUS='OLD',ERR=999)
C
         FOPEN=.TRUE.
C
         HDR250(1)='DEBUG V1.0[0]VFMLR'
         HDR250(2)='PRINT DEBUG DATA FOR THE ROUTINES : [*]VFR'
         HDR250(3)='PRINT DEBUG DATA FOR ALL ROUTINES, '//
     &        'EXCEPT : [*]VFR'
         IRC=0
C     
C     READ DATA FROM INPUT FILE..............................
C     
         KODE=-1
         CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from NUKEM.'
            RETURN
         END IF
         LINE=INTOUT
C     
         KODE=0
         DO WHILE (.NOT.ENDOFF)
C     
C     READ NEXT DATA LINE
C     
            CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
            IF (IRC.NE.0) THEN
               WRITE(*,*) MYNAME,'Error return from NUKEM.'
               RETURN
            END IF
            LINE=INTOUT
C
            IF (LINE.EQ.1) THEN
C     WE JUST READ 'DEBUG V1.0'
            ELSEIF (LINE.EQ.2.AND..NOT.ALL(2)) THEN
C     WE JUST READ 'PRINT DEBUG DATA FOR THE ROUTINES :'
               SPEC=.TRUE.
               ALL(1)=.TRUE.
               IF (NRNAME.LT.MXNAME) NRNAME=NRNAME+1
               LEND=LENGTH(DAT250,250,LEND)
               NAMES(NRNAME)=DAT250(1:LEND)
            ELSEIF (LINE.EQ.3.AND..NOT.ALL(1)) THEN
C     WE JUST READ 'PRINT DEBUG DATA FOR ALL ROUTINES, EXCEPT :'
               SPEC=.TRUE.
               ALL(2)=.TRUE.
               IF (NRNAME.LT.MXNAME) NRNAME=NRNAME+1
               LEND=LENGTH(DAT250,250,LEND)
               NAMES(NRNAME)=DAT250(1:LEND)
            ELSE IF (LINE.NE.0) THEN ! LINE.EQ.0 IMPLIES SOMETIMES EOF
               WRITE(*,*) MYNAME,
     &              'System error, line not implemented:',LINE
               IRC=999
               RETURN
            ENDIF
         ENDDO
C
         KODE=1
         CALL NUKEM(KODE,UNITI,HDR250,INTOUT,DAT250,ENDOFF,IRC)
         IF (IRC.NE.0) THEN
            WRITE(*,*) MYNAME,'Error return from NUKEM.'
            RETURN
         END IF
C     
         CLOSE(UNITI)
C
         FOPEN=.TRUE.
         GOTO 1000
 999     CONTINUE
         CALL CHOP0(PATHF,80)
         LENP=LENGTH(PATHF,80,LENP)
         WRITE(*,*) MYNAME,'Note, no debug file: ',
     &        PATHF(1:LENP)
         FOPEN=.FALSE.
 1000    CONTINUE
C
         FIRST=.FALSE.
      ENDIF
C
      IF (FOPEN) THEN
C
C     CHECK IF NAME IS IN NAMES
C
         DO II=1,NRNAME
            IF (.NOT.BDEB) BDEB=(NAMES(II).EQ.NAME)
C     WRITE(*,*) MYNAME,NAME,NAMES(II),BDEB
         ENDDO
C
         IF (ALL(2)) BDEB=(.NOT.BDEB)
C
      ENDIF
C
      DONE=.TRUE.
      ACTIVE=DONE
C
      RETURN
C
      ENTRY DEBINT(IPATHF)
C
C     SET FILE NAME
C
      PATHF=IPATHF
C
      RETURN
      END
