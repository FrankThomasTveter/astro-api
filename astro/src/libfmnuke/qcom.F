c Library:strgen4 $Id: qcom.F,v 1.1 2012-12-10 12:03:54 franktt Exp $
      BLOCK DATA BDQCOM
      IMPLICIT NONE
      INTEGER LEN2
      LOGICAL IFACT(10),IFDON(10)
      COMMON /CCQCOM/ LEN2,IFACT,IFDON
      DATA LEN2/1/
      END BLOCK DATA


      subroutine QCOM(MAXCOM,NRCOM,COM20,HLP20,CMLEN,PPDAT,
     &     MAXUNIT,UNITA,FILES,LUNIT,CUNIT,
     &     MAXHDR,NRHDR,HDR250,ACTHDR,NRLEN,
     &     MAXVAR,NRVAR,NAMVAR,VALVAR,VARLEN,IFLVL,
     &     MXGRP,NRGRP,HDRGRP,LFLGRP,
     &     BUFF1000,LENB,PROCESS,BDONE,ENDOFF,IRC)
C     
      implicit none
C     
      INTEGER MAXCOM,NRCOM
      CHARACTER*20 COM20(MAXCOM),HLP20(MAXCOM)
      INTEGER CMLEN(MAXCOM),MAXUNIT
      INTEGER NROPT
      PARAMETER (NROPT=5)
      INTEGER MAXHDR,NRHDR
      LOGICAL PPDAT(NROPT,NRHDR)
      INTEGER UNITA(MAXUNIT),CUNIT,LUNIT(MAXUNIT)
      CHARACTER*250 FILES(MAXUNIT)
      CHARACTER*250 HDR250(MAXHDR)
      LOGICAL ACTHDR(MAXHDR)
      INTEGER NRLEN(3,MAXHDR),MAXVAR,NRVAR
      CHARACTER*250 NAMVAR(MAXVAR),VALVAR(MAXVAR)
      INTEGER VARLEN(MAXVAR)
      INTEGER MXGRP           ! MAX GROUPS
      INTEGER NRGRP           ! NUMBER OF GROUPS
      INTEGER HDRGRP(6,MXGRP) ! 1=parent, 2=next sibling, 3=first child,  4=type, 5=id/chdr, 6=lvl
      LOGICAL LFLGRP(MXGRP) ! is group set?
      CHARACTER*1000 BUFF1000
      LOGICAL PROCESS, BDONE, ENDOFF
      INTEGER IRC
C     
      INTEGER EGRP
      CHARACTER*50 CERR50
      INTEGER LENGTH,LEN2,LEN3,LENF,LENS,LENH,LEND,
     &     COMMAND,CVAR,LENB,system
C ,system,
      integer IRC2
      CHARACTER*1000 STR2, STR3, DUMP, PATHF
      LOGICAL BBDONE,ERROR,GO,IFACT(10),IFDON(10),IFVAL,
     &     FOUND, COMP, ACTIVE
      CHARACTER*250 NUKEHEAD, JNK250
      EXTERNAL NUKEHEAD, LENGTH,system
C, system
      CHARACTER*1 DEL(2),C1
      INTEGER POS(2),II,JJ,KK,LEV,IFLVL,CLVL
      CHARACTER*16 MYNAME
      DATA MYNAME /'QCOM'/
C
C     EQUAL SIGN IDENTIFIERS
C
      INTEGER MAXEQN
      PARAMETER (MAXEQN=250)
      INTEGER POSEQN(MAXEQN),CEQN
C
      COMMON /CCQCOM/ LEN2,IFACT,IFDON
C
      ACTIVE=.TRUE.
      ERROR=.FALSE.
C     
C--------CHECK IF THE LINE STARTS WITH A COMMAND
C     
      C1=CHAR(0)
      COMMAND=0
      JJ=1
      FOUND=.FALSE.
      DO WHILE(.NOT.FOUND .AND. JJ.LE.MAXCOM)
         FOUND=(CMLEN(JJ).LE.LENB)
         KK=1
         DO WHILE(KK.LE.CMLEN(JJ).AND.FOUND)
            IF(FOUND) FOUND=(BUFF1000(KK:KK).EQ.COM20(JJ)(KK:KK))
            KK=KK+1
         ENDDO
         IF (FOUND)THEN
            COMMAND=JJ
         ELSE
            JJ=JJ+1
         ENDIF
      ENDDO
C
C--------PERFORM COMMAND (IF LINE STARTS WITH IT)
C     
      IF (COMMAND.NE.0) THEN
C
         IF (COMMAND.EQ.1) THEN ! if command
            PROCESS=.FALSE.
            DEL(1)='('
            DEL(2)=')'
            LEV=0
            POS(1)=1
            POS(2)=1000
            CALL ITEM(BUFF1000,1000,DEL,LEV,POS,IRC)
            CALL RESET(STR2,1000)
            STR2=BUFF1000(1:POS(1)-1)
            CALL CHOP0(STR2,1000)
            LEN2=LENGTH(STR2,1000,LENB)
            IF (IRC.NE.0.OR.LEV.EQ.-1) THEN
C     SYNTAX ERROR
               ERROR=.TRUE.
               BUFF1000='missing () in if statement.'
            ELSEIF (COM20(COMMAND)(1:CMLEN(COMMAND))
     7              .EQ.STR2(1:LEN2).AND.
     &              LENB.EQ.POS(2)) THEN
C     ANALYZE TEST VALUE
               IF ((POS(2)-POS(1)).LE.1) THEN
                  IFVAL=.TRUE.
               ELSE
                  CALL RESET(STR3,1000)
                  STR3=BUFF1000(POS(1)+1:POS(2)-1)
                  CALL EVAL(STR3,1000,NRVAR,NAMVAR,
     &                 VALVAR,VARLEN,PPDAT(1,1),IRC)
                  IF (IRC.NE.0) RETURN
                  CALL CHOP0(STR3,1000)
                  LEN3=LENGTH(STR3,1000,1)
                  IFVAL=(STR3(1:LEN3).EQ.'T')
               ENDIF
C     
               IFLVL=IFLVL+1
C     
               IF (IFVAL) THEN
                  IFACT(IFLVL)=.TRUE.
                  IFDON(IFLVL)=.TRUE.
               ELSE
                  IFACT(IFLVL)=.FALSE.
                  IFDON(IFLVL)=.FALSE.
               ENDIF
C     
            ELSE
C     SYNTAX ERROR
               ERROR=.TRUE.
               BUFF1000='strange if statement.'
            ENDIF
         ELSEIF (COMMAND.EQ.2 .AND. IFLVL.GT.0) THEN ! elseif command
            PROCESS=.FALSE.
            IF (.NOT.IFDON(IFLVL)) THEN
               DEL(1)='('
               DEL(2)=')'
               LEV=0
               POS(1)=1
               POS(2)=1000
               CALL ITEM(BUFF1000,1000,DEL,LEV,POS,IRC)
               CALL RESET(STR2,1000)
               STR2=BUFF1000(1:POS(1)-1)
               CALL CHOP0(STR2,1000)
               LEN2=LENGTH(STR2,1000,LEN2)
               IF (IRC.NE.0.OR.LEV.EQ.-1) THEN
C     SYNTAX ERROR
                  ERROR=.TRUE.
                  BUFF1000='missing () in elseif statement.'
               ELSEIF (COM20(COMMAND)(1:CMLEN(COMMAND))
     &                 .EQ.STR2(1:LEN2).AND.
     &                 LENB.EQ.POS(2)) THEN
C     ANALYZE TEST VALUE
                  IF ((POS(2)-POS(1)).LE.1) THEN
                     IFVAL=.TRUE.
                  ELSE
                     CALL RESET(STR3,1000)
                     STR3=BUFF1000(POS(1)+1:POS(2)-1)
                     CALL EVAL(STR3,1000,NRVAR,NAMVAR,
     &                    VALVAR,VARLEN,PPDAT(1,1),IRC)
                     IF (IRC.NE.0) RETURN
                     CALL CHOP0(STR3,1000)
                     LEN3=LENGTH(STR3,1000,1)
                     IFVAL=(STR3(1:LEN3).EQ.'T')
                  ENDIF
C     
                  IF (IFVAL) THEN
                     IFACT(IFLVL)=.TRUE.
                     IFDON(IFLVL)=.TRUE.
                  ELSE
                     IFACT(IFLVL)=.FALSE.
                     IFDON(IFLVL)=.FALSE.
                  ENDIF
C     
               ELSE
                  ERROR=.TRUE.
                  BUFF1000='strange elseif statement.'
               ENDIF
            ELSE
               IFACT(IFLVL)=.FALSE.
            ENDIF
         ELSEIF (COMMAND.EQ.3 .AND. IFLVL.GT.0) THEN ! else command
            PROCESS=.FALSE.
            IF (.NOT.IFDON(IFLVL)) THEN
               IFACT(IFLVL)=.TRUE.
               IFDON(IFLVL)=.TRUE.
            ELSE
               IFACT(IFLVL)=.FALSE.
            ENDIF
         ELSEIF (COMMAND.EQ.4 .AND. IFLVL.GT.0) THEN  ! endif command
            PROCESS=.FALSE.
            IFACT(IFLVL)=.FALSE.
            IFDON(IFLVL)=.FALSE.
            IFLVL=IFLVL-1
         ENDIF
C     
C     MAKE SURE WE REIGSTER NESTING ERRORS
C     
         IF (PROCESS .AND. COMMAND.LE.4) THEN
            ERROR=.TRUE.
            BUFF1000='elseif, else or endif out of place.'
         ENDIF
      END IF
C     
C     COMMANDS ONLY CHECKED IF WE ARE IN AN ACTIVE REGION
C     
      IF (IFLVL.NE.0) THEN
            active=.true.
            do clvl=1,iflvl
               if (.not.IFACT(CLVL)) ACTIVE=.false.
            end do
      ELSE
         ACTIVE=.TRUE.
      END IF
C
      IF (COMMAND.NE.0.AND.PROCESS) THEN
         IF (PROCESS .AND. .NOT.ACTIVE) PROCESS = .FALSE.
C     
C     EVALUATE EXPRESSIONS (IN CASE OF ECHO OR INCLUDE)...
C     
         IF (PROCESS) THEN
         ENDIF
C
         IF (COMMAND.EQ.5 .AND. PROCESS) THEN ! execute unix command
            CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,
     &           VALVAR,VARLEN,PPDAT(1,1),IRC)
            IF (IRC.NE.0) THEN
               IRC=0
               ERROR=.TRUE.
               BUFF1000='Error return from EVAL.'
               PROCESS=.FALSE.
            ENDIF
            LENB=LENGTH(BUFF1000,1000,LENB)
            PROCESS=.FALSE.
            ii=3
            bbdone=ii.gt.lenb
            do while (.not.bbdone)
               if (buff1000(ii-1:ii).eq.'\\['.or.
     &              buff1000(ii-1:ii).eq.'\\]'.or.
     &              buff1000(ii-1:ii).eq.'\\{'.or.
     &              buff1000(ii-1:ii).eq.'\\}'.or.
     &              buff1000(ii-1:ii).eq.'\\('.or.
     &              buff1000(ii-1:ii).eq.'\\)') then
                  buff1000=buff1000(1:ii-2)//buff1000(ii:lenb)
                  lenb=lenb-1
               end if
               ii=ii+1
               bbdone=ii.gt.lenb
            end do
            IRC=system(BUFF1000(2:LENB))
            IF (IRC.NE.0) THEN
               BUFF1000='Error in UNIX command.'
               ERROR = .TRUE.
            ENDIF
         ELSEIF (COMMAND.EQ.6.AND.PROCESS) THEN ! include file
            PROCESS=.FALSE.
            DEL(1)='('
            DEL(2)=')'
            LEV=0
            POS(1)=1
            POS(2)=LENB
            CALL ITEM(BUFF1000,LENB,DEL,LEV,POS,IRC)
            IF (IRC.NE.0.OR.LEV.EQ.-1) THEN  ! syntax error
               ERROR=.TRUE.
               BUFF1000='missing () in include statement.'
            ELSE
               PATHF=BUFF1000(POS(1)+1:POS(2)-1)
               CALL CHOP0(PATHF,LENB)
               LENF=LENGTH(PATHF,250,10)
               CALL EVAL(PATHF,LENF,NRVAR,NAMVAR,
     &              VALVAR,VARLEN,PPDAT(1,1),IRC)
               IF (CUNIT.LT.MAXUNIT) THEN
                  CUNIT=CUNIT+1
                  FILES(CUNIT)=PATHF(1:LENF)
                  CALL CHOP0(FILES(CUNIT),250)
               ELSE
                  ERROR=.TRUE.
                  BUFF1000='too deep file structure: '//
     &                 PATHF(1:LENF)
               ENDIF
            ENDIF
         ELSEIF (COMMAND.EQ.7.AND.PROCESS) THEN ! stop command
            PROCESS=.FALSE.
            BDONE=.TRUE.
            ENDOFF=.TRUE.
         ELSEIF (COMMAND.EQ.8.AND.PROCESS) THEN ! exit command
            CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,
     &           VALVAR,VARLEN,PPDAT(1,1),IRC)
            IF (IRC.NE.0) THEN
               IRC=0
               ERROR=.TRUE.
               BUFF1000='Error return from EVAL.'
               PROCESS=.FALSE.
            ENDIF
            LENB=LENGTH(BUFF1000,1000,LENB)
            PROCESS=.FALSE.
            JNK250=NUKEHEAD(BUFF1000,250)
            LENB=LENGTH(BUFF1000,1000,2)
            IF (LENB.NE.0) THEN
               READ(BUFF1000(1:LENB),*,IOSTAT=IRC2) IRC
               IF (IRC2.NE.0) THEN
                  BUFF1000='Unable to read return code.'
                  ERROR=.TRUE.
               ELSE
                  CALL exit(IRC)
               END IF
            ELSE
               IRC=0
               CALL exit(IRC)
            END IF
         ELSEIF (COMMAND.EQ.9.AND.PROCESS) THEN ! write command
            CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,
     &           VALVAR,VARLEN,PPDAT(1,1),IRC)
            IF (IRC.NE.0) THEN
               IRC=0
               ERROR=.TRUE.
               BUFF1000='Error return from EVAL.'
               PROCESS=.FALSE.
            ENDIF
            LENB=LENGTH(BUFF1000,1000,LENB)
            PROCESS=.FALSE.
            JNK250=NUKEHEAD(BUFF1000,250)
            CALL CHOP0(BUFF1000,LENB)
            LENB=LENGTH(BUFF1000,LENB,LENB)
            WRITE(*,*) BUFF1000(1:LENB)
         ELSEIF (COMMAND.EQ.10 .AND. PROCESS) THEN ! set command
            PROCESS=.FALSE.
            CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,
     &           VALVAR,VARLEN,PPDAT(1,1),IRC)
            IF (IRC.NE.0) THEN
               IRC=0
               ERROR=.TRUE.
               BUFF1000='Error return from EVAL.'
               PROCESS=.FALSE.
            ENDIF
            LENB=LENGTH(BUFF1000,1000,LENB)
C
C     IDENTIFY ALL EQUAL SIGNS
C
            COMP=.FALSE.
            CEQN=0
            LENB=LENGTH(BUFF1000,1000,LENB)
            DO II=1,LENB
               IF (BUFF1000(II:II).EQ.'=') THEN
                  CEQN=CEQN+1
                  POSEQN(CEQN)=II
               ENDIF
            ENDDO
C     
C     LOOP THROUGH ALL EQUATIONS
C     
            IF (CEQN.GT.0) THEN
C     FOUND AT LEAST ONE '='
               DO KK=CEQN,1,-1
C     
                  IF (KK.GT.1) THEN
                     POS(1)=POSEQN(KK-1)+1
                  ELSE
                     POS(1)=4
                  ENDIF
C     
                  IF (KK.LT.CEQN) THEN
                     POS(2)=POSEQN(KK+1)-1
                  ELSE
                     POS(2)=1000
                  ENDIF
C     
C     GET PART OF STRING BEFORE AND AFTER '='
C     
                  CALL RESET(STR2,1000)
                  STR2=BUFF1000(POS(1):POSEQN(KK)-1)
                  CALL EVAL(STR2,1000,NRVAR,NAMVAR,
     &                 VALVAR,VARLEN,PPDAT(1,1),IRC)
                  IF (IRC.NE.0) RETURN
                  CALL CHOP0(STR2,1000)
                  LEN2=LENGTH(STR2,250,LEN2)
                  CALL RESET(STR3,1000)
                  STR3=BUFF1000(POSEQN(KK)+1:POS(2))
                  CALL EVAL(STR3,1000,NRVAR,NAMVAR,
     &                 VALVAR,VARLEN,PPDAT(1,1),IRC)
                  IF (IRC.NE.0) RETURN
                  CALL CHOP0(STR3,1000)
                  LEN3=LENGTH(STR3,250,LEN3)
C     
C     FIND OUT IF VARIABLE EXISTS
C     
                  II=0
                  FOUND=.FALSE.
                  DO WHILE (II.LT.NRVAR .AND. .NOT.FOUND)
                     II=II+1
                     JJ=0
                     FOUND=(VARLEN(II).EQ.LEN2.AND.LEN2.NE.0)
                     DO WHILE (JJ.LT.LEN2 .AND. FOUND)
                        JJ=JJ+1
                        IF (FOUND) FOUND=(STR2(JJ:JJ).EQ.
     &                       NAMVAR(II)(JJ:JJ))
                     ENDDO
                  ENDDO
                  CVAR=II
C     
                  IF (LEN3.EQ.1.and.STR3(1:LEN3).EQ.'.') THEN
                     IF (FOUND) THEN
C     REMOVE VARIABLE
                        NAMVAR(CVAR)=NAMVAR(NRVAR)
                        VALVAR(CVAR)=VALVAR(NRVAR)
                        VARLEN(CVAR)=VARLEN(NRVAR)
                        NRVAR=NRVAR-1
                     ELSE
                        ERROR=.TRUE.
                        BUFF1000='attempt to remove non-existent'//
     &                       ' variable.'
                     ENDIF
                  ELSE
C     ADD/UPDATE VARIABLE
                     IF (FOUND) THEN
                        VALVAR(CVAR)=STR3(1:250)
                     ELSE
                        IF (NRVAR.LT.MAXVAR) THEN
                           NRVAR=NRVAR+1
                           NAMVAR(NRVAR)=STR2(1:250)
                           VALVAR(NRVAR)=STR3(1:250)
                           VARLEN(NRVAR)=
     &                          LENGTH(NAMVAR(NRVAR),250,5)
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ELSEIF (COMMAND.EQ.11 .AND. PROCESS) THEN
            CALL EVAL(BUFF1000,1000,NRVAR,NAMVAR,
     &           VALVAR,VARLEN,PPDAT(1,1),IRC)
            IF (IRC.NE.0) THEN
               IRC=0
               ERROR=.TRUE.
               BUFF1000='Error return from EVAL.'
               PROCESS=.FALSE.
            ENDIF
            LENB=LENGTH(BUFF1000,1000,LENB)
            PROCESS=.FALSE.
C     IMPORT
C     FIRST REMOVE THE ACTUAL COMMAND...
            STR2=NUKEHEAD(BUFF1000,250)
C     READ THE FIRST ARGUMENT...
            STR2=NUKEHEAD(BUFF1000,250)
            CALL CHOP0(STR2,1000)
            LEN2=LENGTH(STR2,250,LEN2)
            BBDONE = (LEN2.LE.0)
            DO WHILE (.NOT. BBDONE)
               CALL EVAL(STR2,1000,NRVAR,NAMVAR,
     &              VALVAR,VARLEN,PPDAT(1,1),IRC)
               IF (IRC.NE.0) RETURN
               CALL CHOP0(STR2,1000)
               LEN2=LENGTH(STR2,250,LEN2)
               CALL RESET(STR3,1000)
C     IMPORT ENVIRONMENT VARIABLE FROM SHELL...
#if defined POSIX
               call pxfgetenv(str2(1:len2),len2,str3,len3,irc)
               if (irc.ne.0) then
                  str3 = ''
                  irc = 0
               endif
#else
               call getenv(str2(1:len2),str3)
#endif
C     CALL EVAL(STR3,1000,NRVAR,NAMVAR,
C     &                    VALVAR,VARLEN,PPDAT(1,1),IRC)
               CALL CHOP0(STR3,1000)
               LEN3=LENGTH(STR3,250,LEN3)
C     
C     FIND OUT IF VARIABLE EXISTS
C     
               II=0
               FOUND=.FALSE.
               DO WHILE (II.LT.NRVAR .AND. .NOT.FOUND)
                  II=II+1
                  JJ=0
                  FOUND=(VARLEN(II).EQ.LEN2.AND.LEN2.NE.0)
                  DO WHILE (JJ.LT.LEN2 .AND. FOUND)
                     JJ=JJ+1
                     IF (FOUND) FOUND=(STR2(JJ:JJ).EQ.
     &                    NAMVAR(II)(JJ:JJ))
                  ENDDO
               ENDDO
               CVAR=II
C     
C     ADD/UPDATE VARIABLE
               IF (FOUND) THEN
                  IF (LEN3.NE.0) THEN ! only update if nonzero
                     VALVAR(CVAR)=STR3(1:250)
                  END IF
               ELSE
                  IF (LEN3.NE.0) THEN ! only update if nonzero
                     IF (NRVAR.LT.MAXVAR) THEN
                        NRVAR=NRVAR+1
                        NAMVAR(NRVAR)=STR2(1:250)
                        VALVAR(NRVAR)=STR3(1:250)
                        VARLEN(NRVAR)=
     &                       LENGTH(NAMVAR(NRVAR),250,5)
                     ENDIF
                  ELSE
                     WRITE(*,*) MYNAME,
     &                    'Nonexistent environment variable:'//
     &                    STR2(1:LEN2)//
     &                    ' must be initialised in input file.'
                     IRC=264
                     RETURN
                  END IF
               ENDIF
C     
C     READ THE NEXT ARGUMENT...
C     
               IF (ERROR) THEN
                  BBDONE=.TRUE.
               ELSE
                  STR2=NUKEHEAD(BUFF1000,250)
                  CALL CHOP0(STR2,1000)
                  LEN2=LENGTH(STR2,250,LEN2)
                  BBDONE = (LEN2.LE.0)
               END IF
            END DO
         ELSEIF (COMMAND.EQ.12 .AND. PROCESS) THEN ! help
            PROCESS=.FALSE.
            WRITE(*,*) MYNAME,
     &           '--------LOGICAL ORDER OF HEADERS-------'
            EGRP=0
            CERR50='system error'
            call REPERR (EGRP,CERR50,MAXHDR,HDR250,
     &           MXGRP,NRGRP,HDRGRP,LFLGRP,
     &           IRC)
            WRITE(*,*) MYNAME,
     &           '----------------------------------------'
         ELSEIF (COMMAND.EQ.13 .AND. PROCESS) THEN ! help
            PROCESS=.FALSE.
C     LIST ALL HEADERS
            WRITE(*,*) MYNAME,
     &           '--------LIST OF POSSIBLE COMMANDS-------'
            DO II=1,MAXCOM
               LENS=LENGTH(COM20(II),20,3)
               LENH=LENGTH(HLP20(II),20,3)
               WRITE(*,*) COM20(II)(1:LENS)//' '//
     &              HLP20(II)(1:LENH)
            ENDDO
            WRITE(*,*) MYNAME,
     &           '--------LIST OF POSSIBLE HEADERS--------'
            DO II=1,NRHDR
               LENS=LENGTH(HDR250(II),250,NRLEN(1,II)+3)
               IF(ACTHDR(II))
     &              WRITE(*,*) HDR250(II)(1:LENS)
            ENDDO
            WRITE(*,*) MYNAME,
     &           '----------------------------------------'
         ENDIF
C
         IF (BDONE) PROCESS=.FALSE.
C
         IF (ERROR) THEN
            LENF=LENGTH(FILES(CUNIT),250,10)
            CALL CHOP0(BUFF1000,250)
            LENB=LENGTH(BUFF1000,250,10)
            IF (LENF.GT.80)LENF=80
            WRITE(DUMP,*) 
     &           '"'//FILES(CUNIT)(1:LENF)//'", line ',
     &           LUNIT(CUNIT),': Error: '//BUFF1000(1:LENB)
            CALL CHOP0(DUMP,200)
            LEND=LENGTH(DUMP,200,20)
            WRITE(*,*) DUMP(1:LEND)
            IRC=291
            RETURN
         ENDIF
C
      ELSE IF (PROCESS) THEN
         PROCESS=ACTIVE         ! ONLY PROCESS HEADERS IN ACTIVE REGIONS
      ENDIF
C
      RETURN
      END


